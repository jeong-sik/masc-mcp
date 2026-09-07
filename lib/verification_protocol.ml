(** Verification_protocol -- out-of-band completion verification orchestration.

    Bridges task FSM transitions (AwaitingVerification state) with:
    - Board system (visibility posts for completion authorities)
    - SSE events (masc:verification:requested, :verdict, :rejected)
    - Verification storage (.masc/verifications/)

    @since Phase B+C *)

(* Contract source rules (must stay aligned with [task_contract] in
   types_core.ml):
   - [criteria]: the operator-facing "must be true" statements →
     [task.contract.completion_contract] as exact criterion strings.
   - [evidence_refs]: the artefact list the completion authority expects to see →
     [task.contract.verify_gate_evidence] plus required evidence refs,
     passed in by the caller at task-state lifecycle so this function does
     not reach into task.contract twice for different fields. *)
type submit_request_spec =
  { criteria : Verification.criterion list
  ; output : Yojson.Safe.t
  ; board_type : string
  ; board_title : string
  ; board_content : string
  ; evidence_fields : (string * Yojson.Safe.t) list
      (* Request persistence replaces [submitted_evidence] with its typed
         submit-time snapshot SSOT. *)
  ; submitted_evidence : string list
  }

let submit_request_spec ~(config : Workspace.config) ~(task : Masc_domain.task)
    ~assignee ~(claim : Masc_domain.verification_claim) =
  let board_type = "verification_request" in
  (* The Board post names what was asked. A stop carries the producer's
     reason where a completion carries its evidence references: the reason is
     the whole claim, so the post states it for the operator who closes the
     stop (RFC-0417 §4.1). The record keeps no copy of that sentence. Which
     question was asked is the Task's status to answer, the authority routes
     a stop to the operator before it opens the record's output, and the
     operator reads the post. The task contract describes work the producer
     says should not be finished, and is not what a stop is judged on. *)
  let board_title, board_content, evidence_refs =
    match claim with
    | Masc_domain.Completion_evidence { evidence_refs } ->
      ( Printf.sprintf "Verify: %s" task.title
      , Printf.sprintf "Verification requested for task %s (%s) by %s"
          task.id task.title assignee
      , evidence_refs )
    | Masc_domain.Cancellation_reason { reason } ->
      ( Printf.sprintf "Cancel: %s" task.title
      , Printf.sprintf "Cancellation requested for task %s (%s) by %s: %s"
          task.id task.title assignee reason
      , [] )
  in
  let criteria =
    match task.contract with
    | Some c -> c.completion_contract
    | None -> []
  in
  (* Derive the required/submitted role split from the task SSOT. The strings
     remain transient here; [create_submit_request] persists the typed
     snapshot. *)
  let verification_evidence =
    Masc_task_handlers.Tool_task_completion_review.concrete_verification_evidence
      ~submitted_evidence_refs:evidence_refs
      task
  in
  let evidence_fields =
    Masc_task_handlers.Tool_task_completion_review.verification_evidence_fields
      verification_evidence
  in
  let output =
    `Assoc
      (* [request_kind], [request_summary] and [next_action] were written here
         as the literals "normal", "" and "". Nothing computed them and nothing
         could set them to anything else, so every request carried the same
         three values and three rows of the verification detail pane said the
         same thing on every request ever drawn. The reader even knew a second
         kind, "conflict_triage", that had no producer anywhere in the repo.

         [task_title] is what those rows were standing in front of: filled on
         every request, and now what the queue reads. *)
      ([ ("evidence_refs", `List (List.map (fun s -> `String s) evidence_refs));
         ("task_title", `String task.title);
       ]
       @ evidence_fields)
  in
  { criteria
  ; output
  ; board_type
  ; board_title
  ; board_content
  ; evidence_fields
  ; submitted_evidence = verification_evidence.submitted_evidence
  }

let warn_contract_gap (task : Masc_domain.task) =
  (* Observability for #8272: tasks submitted without a contract land in
     storage with empty completion_contract + empty evidence, which the
     dashboard renders as "—". Surface this as a warn so operators can
     trace the gap back to the task creation site instead of only
     noticing it in the UI. No behavior change. *)
  (match task.contract with
   | None ->
     Log.Task.warn
       ~keeper_name:task.id
       "[verification-submit] task=%s has no contract — completion_contract \
        and evidence will be empty in the verification record"
       task.id
   | Some c
     when c.completion_contract = []
          && c.required_evidence = []
          && c.verify_gate_evidence = [] ->
     Log.Task.warn
       ~keeper_name:task.id
       "[verification-submit] task=%s has a contract but both \
       completion_contract and verification evidence are empty"
       task.id
   | Some _ -> ())

(* A truncated artifact cannot be judged from the snapshot — the review
   instructions order the judge to treat it as unavailable and its prefix is
   not transmitted (#29615). The producer can still act at submit time: split
   the artifact, summarize it, or expect the verdict to rest on the readable
   evidence alone. Saying so here is the earliest honest place. *)
let warn_oversized_evidence ~(task : Masc_domain.task) ~(snapshot : Yojson.Safe.t) =
  List.iter
    (fun (reference, bytes) ->
       Log.Task.warn
         ~keeper_name:task.id
         "[verification-submit] task=%s evidence %s is %d bytes — over the \
          %d-byte snapshot cap; the judge treats it as unavailable. Split or \
          summarize the artifact, or the verdict rests on the readable \
          evidence alone"
         task.id
         reference
         bytes
         Workspace_verification_store.verification_evidence_max_bytes)
    (Workspace_verification_store.truncated_snapshot_items snapshot)

let create_submit_request ~(config : Workspace.config)
    ~(task : Masc_domain.task) ~assignee ~verification_id
    ~(claim : Masc_domain.verification_claim) =
  let base_path = config.Workspace.base_path in
  (match claim with
   | Masc_domain.Completion_evidence _ -> warn_contract_gap task
   | Masc_domain.Cancellation_reason _ -> ());
  let spec = submit_request_spec ~config ~task ~assignee ~claim in
  let artifact_read =
    (* The capture reads the artifact where the producer's sandbox keeps it;
       see [Keeper_tool_task_runtime.evidence_artifact_reader]. *)
    match Keeper_meta_store.read_effective_meta_resolved config assignee with
    | Ok (Some (_file, meta)) ->
        Keeper_tool_task_runtime.evidence_artifact_reader ~config ~meta ()
    | Ok None | Error _ -> None
  in
  let evidence_snapshot =
    Workspace_verification_store.snapshot_submitted_evidence_json
      ?artifact_read
      ~request_id:verification_id
      ~base_path
      ~worker:assignee
      spec.submitted_evidence
  in
  warn_oversized_evidence ~task ~snapshot:evidence_snapshot;
  let output =
    match spec.output with
    | `Assoc fields ->
      `Assoc
        (List.map
           (fun (key, value) ->
             if String.equal key "submitted_evidence"
             then key, evidence_snapshot
             else key, value)
           fields)
    | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _)
      as impossible ->
      impossible
  in
  match
    Verification.create_request ~base_path ~task_id:task.id ~request_id:verification_id
      ~output ~criteria:spec.criteria ~worker:assignee ()
  with
  | Ok _ -> Ok ()
  | Error e ->
    Log.Task.error
      ~keeper_name:task.id
      "verification create_request failed (task=%s vrf=%s): %s"
      task.id verification_id e;
    Error e

(* RFC-0221 §3.1: compensation for atomic submit. Remove the verification record
   for [verification_id] when the task_status commit it was written for did not
   land, so the record store and [task_status] are never left disagreeing.
   Mirrors {!create_submit_request}'s base_path derivation. A missing record is
   success (idempotent), so compensation is safe to run unconditionally. *)
let delete_verification_request ~(config : Workspace.config) ~verification_id =
  let base_path = config.Workspace.base_path in
  match Verification.delete_request base_path verification_id with
  | Ok () -> Ok ()
  | Error e ->
    Log.Task.error
      ~keeper_name:verification_id
      "verification delete_request failed (vrf=%s): %s"
      verification_id e;
    Error e

let notify_submit_for_verification ~(config : Workspace.config)
    ~(task : Masc_domain.task) ~assignee ~verification_id
    ~(claim : Masc_domain.verification_claim) =
  let spec = submit_request_spec ~config ~task ~assignee ~claim in
  let evidence_refs =
    match claim with
    | Masc_domain.Completion_evidence { evidence_refs } -> evidence_refs
    | Masc_domain.Cancellation_reason _ -> []
  in
  let meta_json = `Assoc ([
    ("type", `String spec.board_type);
    ("task_id", `String task.id);
    ("verification_id", `String verification_id);
    ("worker", `String assignee);
    ("evidence_refs", `List (List.map (fun s -> `String s) evidence_refs));
    ("criteria", `List (List.map Verification.criterion_to_yojson spec.criteria));
  ] @ spec.evidence_fields) in
  let () =
    match Board_dispatch.create_post
      ~author:"system"
      ~content:spec.board_content
      ~title:spec.board_title
      ~post_kind:Board.System_post
      ~meta_json
        (* Unlisted: the completion authority reads the verification store,
           not the Board, and the operator reads the post by id. No keeper
           needs to discover a request receipt, so none judges it. *)
      ~visibility:Board.Unlisted
      ~hearth:"verification"
      ()
    with
    | Ok _ -> ()
    | Error e ->
      Log.Task.error
        ~keeper_name:task.id
        "board post failed (task=%s vrf=%s): %s"
        task.id verification_id (Board_types.show_board_error e)
  in
  Subscriptions.push_event_to_sessions (`Assoc ([
    ("type", `String "masc/verification/requested");
    ("task_id", `String task.id);
    ("verification_id", `String verification_id);
    ("worker", `String assignee);
    ("evidence_refs", `List (List.map (fun s -> `String s) evidence_refs));
    ("timestamp", `Float (Time_compat.now ()));
  ] @ spec.evidence_fields));
  ()

let completion_authority_fields (authority : Masc_domain.completion_authority) =
  [ ( "authority_kind"
    , `String (Masc_domain.completion_authority_kind authority) )
  ; ( "authority_actor"
    , `String (Masc_domain.completion_authority_actor authority) )
  ]

let verification_verdict_fields
      ~event_type
      ~(authority : Masc_domain.completion_authority)
      ~task_id
      ~verification_id
      ~(verdict : Masc_domain.completion_verdict)
  =
  let verdict_name =
    match verdict with
    | Masc_domain.Verdict_approved -> "approved"
    | Masc_domain.Verdict_rejected _ -> "rejected"
  in
  [ ("type", `String event_type)
  ; ("task_id", `String task_id)
  ; ("verification_id", `String verification_id)
  ]
  @ completion_authority_fields authority
  @ [ "verdict", `String verdict_name ]

let verification_verdict_metadata
      ~(authority : Masc_domain.completion_authority)
      ~task_id
      ~verification_id
      ~(verdict : Masc_domain.completion_verdict)
  =
  `Assoc
    (verification_verdict_fields
       ~event_type:"verification_verdict"
       ~authority
       ~task_id
       ~verification_id
       ~verdict)

let verdict_event_json
      ~(authority : Masc_domain.completion_authority)
      ~task_id
      ~verification_id
      ~(verdict : Masc_domain.completion_verdict)
      ~notes
      ~timestamp
  =
  let event_type =
    match verdict with
    | Masc_domain.Verdict_approved -> "masc/verification/verdict"
    | Masc_domain.Verdict_rejected _ -> "masc/verification/rejected"
  in
  let detail_fields =
    match verdict with
    | Masc_domain.Verdict_approved -> [ "notes", `String notes ]
    | Masc_domain.Verdict_rejected { reason } -> [ "reason", `String reason ]
  in
  `Assoc
    (verification_verdict_fields
       ~event_type
       ~authority
       ~task_id
       ~verification_id
       ~verdict
     @ detail_fields
     @ [ "timestamp", `Float timestamp ])

let post_verdict_board
      ~(authority : Masc_domain.completion_authority)
      ~task_id
      ~verification_id
      ~(verdict : Masc_domain.completion_verdict)
      ~content
  =
  match
    Board_dispatch.create_post
      ~author:(Masc_domain.completion_authority_actor authority)
      ~content
      ~post_kind:Board.System_post
      ~meta_json:(verification_verdict_metadata ~authority ~task_id ~verification_id ~verdict)
        (* Unlisted: a rejection reaches the producer as a typed stimulus
           ([Completion_authority_wakeup]); an approval wakes nobody — the
           task leaves the backlog as Done, and the producer's current task
           was already cleared at submission
           ([Keeper_current_task_reconcile] counts only Claimed and
           InProgress as owned). So no keeper needs to discover the verdict
           receipt. The stalled receipt below stays Internal — the Board is
           its only path to the producer. *)
      ~visibility:Board.Unlisted
      ~hearth:"verification"
      ()
  with
  | Ok _ -> ()
  | Error e ->
    Log.Task.error
      ~keeper_name:task_id
      "board post failed (task=%s vrf=%s): %s"
      task_id verification_id (Board_types.show_board_error e)

let notify_approve_verification
      ~task_id
      ~(authority : Masc_domain.completion_authority)
      ~verification_id
      ~notes
  =
  post_verdict_board
    ~authority
    ~task_id
    ~verification_id
    ~verdict:Masc_domain.Verdict_approved
    ~content:(Printf.sprintf "Approved task %s (vrf:%s)%s"
                task_id
                verification_id
                (if notes = "" then "" else " — " ^ notes));
  Subscriptions.push_event_to_sessions
    (verdict_event_json
       ~authority
       ~task_id
       ~verification_id
       ~verdict:Masc_domain.Verdict_approved
       ~notes
       ~timestamp:(Time_compat.now ()))

let notify_reject_verification
      ~task_id
      ~(authority : Masc_domain.completion_authority)
      ~verification_id
      ~reason
  =
  let verdict = Masc_domain.Verdict_rejected { reason } in
  post_verdict_board
    ~authority
    ~task_id
    ~verification_id
    ~verdict
    ~content:(Printf.sprintf "Rejected task %s (vrf:%s): %s"
                task_id verification_id reason);
  Subscriptions.push_event_to_sessions
    (verdict_event_json
       ~authority
       ~task_id
       ~verification_id
       ~verdict
       ~notes:""
       ~timestamp:(Time_compat.now ()))

(* The destructive 24h verification deadline rescue is removed. With the
   verification sub-state folded into [task_status], the illegal Todo+Pending
   drift is unrepresentable. An
   AwaitingVerification obligation remains in the live backlog until an
   authenticated operator or typed system-LLM judge commits a verdict. Long-waiting
   obligations are surfaced from the activity-event stream, not a poll-timer. *)

let stalled_board_content ~task_id ~verification_id ~gate ~detail =
  Printf.sprintf
    "Stalled task %s (vrf:%s) — review will not retry. gate=%s: %s. Forward \
     path: the assignee resubmits with submit_for_verification (supersedes \
     this verification), or an operator commits a HITL verdict."
    task_id
    verification_id
    gate
    detail

let stalled_metadata
      ~(authority : Masc_domain.completion_authority)
      ~task_id
      ~verification_id
      ~gate
      ~detail
  =
  `Assoc
    ([ ("type", `String "verification_stalled")
     ; ("task_id", `String task_id)
     ; ("verification_id", `String verification_id)
     ]
     @ completion_authority_fields authority
     @ [ ("gate", `String gate)
       ; ("detail", `String detail)
       ; ("timestamp", `Float (Time_compat.now ()))
       ])

(* How far back a stall looks for its own earlier post.

   The window exists because the Board is a stream, not an index: there is an
   exact-key index for Fusion run ids and nothing equivalent for a stall, and
   a stall does not carry enough durable truth to justify building one. Set
   against the storm this closes — one verification produced 40+ posts over
   1h44m — the hearth's own recent history is far more than the repeats ever
   spanned. A stall that outlives the window posts again, which is the right
   answer that far out. *)
let stall_lookback_posts = 200

(* Whether this exact stall is already on the Board.

   The Board is what the repeat is about, so the Board is what decides. The
   post carries its identity in typed metadata — [type], [verification_id],
   [gate], [detail] — so this reads those fields rather than matching the
   rendered sentence, and it asks the same question the metadata answers.

   A post that failed to land leaves nothing to find, so a stall whose notice
   never reached anyone is reported again on the next sweep. *)
let stall_already_on_the_board ~verification_id ~gate ~detail =
  let same_stall (post : Board.post) =
    match post.meta_json with
    | None -> false
    | Some (`Assoc fields) ->
      let field name =
        match List.assoc_opt name fields with
        | Some (`String value) -> Some value
        | Some _ | None -> None
      in
      field "type" = Some "verification_stalled"
      && field "verification_id" = Some verification_id
      && field "gate" = Some gate
      && field "detail" = Some detail
    | Some _ -> false
  in
  Board_dispatch.list_posts
    ~hearth:"verification"
    ~post_kind_filter:Board.System_post
    ~limit:stall_lookback_posts
    ()
  |> List.exists same_stall

let notify_stalled_verification
      ~(authority : Masc_domain.completion_authority)
      ~task_id
      ~verification_id
      ~gate
      ~detail
  =
  if stall_already_on_the_board ~verification_id ~gate ~detail
  then ()
  else
  match
    Board_dispatch.create_post
      ~author:(Masc_domain.completion_authority_actor authority)
      ~content:(stalled_board_content ~task_id ~verification_id ~gate ~detail)
      ~post_kind:Board.System_post
      ~meta_json:
        (stalled_metadata ~authority ~task_id ~verification_id ~gate ~detail)
      ~visibility:Board.Internal
      ~hearth:"verification"
      ()
  with
  | Ok _ -> ()
  | Error e ->
    Log.Task.error
      ~keeper_name:task_id
      "stalled-review board post failed (task=%s vrf=%s): %s"
      task_id verification_id (Board_types.show_board_error e)

module For_testing = struct
  let verdict_event_json = verdict_event_json
  let stalled_board_content = stalled_board_content

  let stalled_metadata ~authority ~task_id ~verification_id ~gate ~detail =
    stalled_metadata ~authority ~task_id ~verification_id ~gate ~detail
end
