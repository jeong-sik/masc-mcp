(** System LLM completion-authority lane.

    This is deliberately an application-owned LLM agent rather than a Keeper:
    it has no Keeper identity, no Keeper task action, and no Keeper lifecycle.
    The only durable mutation it can perform is the typed completion-verdict
    boundary after the verification request and evidence identities match. *)

open Result.Syntax

(* RFC-0361 D7(b): fixed authority identity — every judgement carries the
   same actor string (the [verifier_exact] lane id) so verdicts aggregate by
   actor; run identity stays with the per-review [verification_id]. *)
let authority_actor = Runtime.verifier_exact_lane_id

(* How many operator-disagreement examples ride along in the judge's prompt.
   Few-shot selection puts false positives first, so the examples that cost
   the most (an approve a human overturned) are the ones that survive the
   cut. *)
let judge_few_shot_examples = 3

type runtime =
  { config : Workspace_utils_backend_setup.config
  ; sw : Eio.Switch.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; wake : Eio.Condition.t
  ; sweep_pending : bool Atomic.t
      (** A whole-backlog read is due. Boot recovery and a failed backlog read
          are the only things that need one: they have no key to aim at. *)
  ; targets : review_key list Atomic.t
      (** Verifications a submission or a retryable deferral asked for by name.
          The submission hook already receives [task], [assignee] and
          [verification_id]; carrying them here is what keeps one submission
          from re-reviewing every other awaiting Task. *)
  ; retry_scheduled : bool Atomic.t
  ; retry_interval_sec : float
  ; in_flight : review_key list Atomic.t
  ; review_slots : Eio.Semaphore.t
  }

and review_key =
  { task_id : string
  ; verification_id : string
  }

let active_runtime : runtime option Atomic.t = Atomic.make None

let review_key_equal left right =
  String.equal left.task_id right.task_id
  && String.equal left.verification_id right.verification_id
;;

let claim_review (runtime : runtime) key =
  let rec loop () =
    let current = Atomic.get runtime.in_flight in
    if List.exists (review_key_equal key) current
    then false
    else if Atomic.compare_and_set runtime.in_flight current (key :: current)
    then true
    else loop ()
  in
  loop ()
;;

let release_review (runtime : runtime) key =
  let rec loop () =
    let current = Atomic.get runtime.in_flight in
    let next = List.filter (fun candidate -> not (review_key_equal candidate key)) current in
    if List.length next = List.length current
    then ()
    else if Atomic.compare_and_set runtime.in_flight current next
    then ()
    else loop ()
  in
  loop ()
;;

(** What one wake of the daemon is allowed to look at. A submission and a
    retryable deferral both name the verification they mean, so they get
    [Targets]. Boot and a failed backlog read have nothing to name, so they get
    [Whole_backlog]. Keeping the two apart is what stops one submission from
    re-reviewing every other awaiting Task. *)
type scan_scope =
  | Whole_backlog
  | Targets of review_key list

(* Pure: the awaiting entries one scope admits. [Whole_backlog] admits all of
   them; [Targets] admits only the named keys, and names a key at most once even
   if it was requested repeatedly before the daemon woke. *)
let entries_in_scope ~scope entries =
  match scope with
  | Whole_backlog -> entries
  | Targets keys ->
    List.filter
      (fun (key, _) -> List.exists (review_key_equal key) keys)
      entries
;;

let take_targets (runtime : runtime) =
  let rec loop () =
    let current = Atomic.get runtime.targets in
    if current = []
    then []
    else if Atomic.compare_and_set runtime.targets current []
    then current
    else loop ()
  in
  loop ()
;;

let evidence_refs_of_output = function
  | `Assoc fields ->
    (match List.assoc_opt "evidence_refs" fields with
     | Some (`List values) ->
       let rec collect index acc = function
         | [] -> Ok (List.rev acc)
         | (`String value) :: rest -> collect (index + 1) (value :: acc) rest
         | value :: _ ->
           Error
             (Printf.sprintf
                "verification request output evidence_refs[%d] must be a string, got %s"
                index
                (Json_util.excerpt value))
       in
       collect 0 [] values
     | Some value ->
       Error
         (Printf.sprintf
            "verification request output evidence_refs must be a JSON array, got %s"
            (Json_util.excerpt value))
     | None -> Error "verification request output has no evidence_refs")
  | other ->
    Error
      (Printf.sprintf
         "verification request output must be a JSON object, got %s"
         (Json_util.excerpt other))
;;

let completion_verdict_of_review = function
  | Task.Anti_rationalization.Approve _ -> Masc_domain.Verdict_approved
  | Task.Anti_rationalization.Reject reason ->
    Masc_domain.Verdict_rejected { reason }
;;

let review_notes
    ~(request : Verification.verification_request)
    ~evidence_access
    ~result
    ~authority =
  let verdict =
    match result.Task.Anti_rationalization.verdict with
    (* Stays the bare "approve" token: [Dashboard_harness_health] and the
       label decoders read this notes field as that exact one-piece string. The
       stated reason travels to the run registry instead, where the outcome has
       a field for it. *)
    | Some (Task.Anti_rationalization.Approve _) -> `String "approve"
    | Some (Task.Anti_rationalization.Reject reason) ->
      `Assoc [ "kind", `String "reject"; "reason", `String reason ]
    | None -> `Null
  in
  let review =
    `Assoc
      [ "evaluator_runtime", `String result.evaluator_runtime
      ; "generator_runtime",
        (match result.generator_runtime with
         | Some runtime -> `String runtime
         | None -> `Null)
      ; "gate", `String (Task.Anti_rationalization.gate_to_string result.gate)
      ; "verdict", verdict
      ; "authority_kind", `String (Masc_domain.completion_authority_kind authority)
      ; "authority_actor", `String (Masc_domain.completion_authority_actor authority)
      ]
  in
  Yojson.Safe.pretty_to_string
    (`Assoc
       [ ( "verification_request"
         , `Assoc
             [ "id", `String request.id
             ; "task_id", `String request.task_id
             ; "worker", `String request.worker
             ; "created_at", `Float request.created_at
             ] )
       ; ( "submitted_evidence_metadata"
         , Workspace_verification_store.submitted_evidence_access_metadata_to_yojson
             evidence_access )
       ; "review", review
       ])
;;

let required_string_list_field ~context name fields =
  match List.assoc_opt name fields with
  | Some (`List values) ->
    let rec collect index acc = function
      | [] -> Ok (List.rev acc)
      | (`String value) :: rest -> collect (index + 1) (value :: acc) rest
      | value :: _ ->
        Error
          (Printf.sprintf
             "%s %s[%d] must be a string, got %s"
             context
             name
             index
             (Json_util.excerpt value))
    in
    collect 0 [] values
  | Some value ->
    Error
      (Printf.sprintf
         "%s %s must be a JSON array, got %s"
         context
         name
         (Json_util.excerpt value))
  | None ->
    Error (Printf.sprintf "%s has no %s" context name)
;;

(* The question to put, and the material it needs: a completion weighed
   against its contract and artifacts. The request supplies both. *)
let verdict_question_of_request (request : Verification.verification_request) =
  match request.output with
  | `Assoc fields ->
    let* required_evidence =
      required_string_list_field
        ~context:"verification request output"
        "required_artifacts"
        fields
    in
    Ok
      { Task.Anti_rationalization.completion_contract =
          (match request.criteria with
           | [] -> None
           | descriptions -> Some descriptions)
      ; required_evidence
        (* Both filled at the review site, where the snapshot and the
           calibration ledger are read. This function maps a request to a
           question and reads no store. *)
      ; evidence_posture = Task.Anti_rationalization.Note_only
      ; few_shot_block = ""
      }
  | other ->
    Error
      (Printf.sprintf
         "verification request output must be a JSON object, got %s"
         (Json_util.excerpt other))
;;

(* The evidence posture, computed from the fixed snapshot this request was
   submitted with. The arithmetic is the judge prompt's rules 3 and 4 made
   typed: an artifact the judge cannot open whole counts as nothing. An
   unavailable snapshot is the same posture as an empty one — the judge
   already sees the typed reason beside the question. *)
let evidence_posture_of_snapshot
      (snapshot : Workspace_verification_store.submitted_evidence_access) =
  let usable =
    match snapshot with
    | Workspace_verification_store.Evidence_unavailable _ -> 0
    | Workspace_verification_store.Evidence_available { request = _; items } ->
      items
      |> List.filter (function
           | Workspace_verification_store.Evidence_artifact
               { reference = _; content = _; bytes = _; truncated = false } ->
             true
           | Workspace_verification_store.Evidence_artifact_binary _ ->
             (* A binary item is judgeable on its own terms: the hash, the
                size, and the filed body are the facts the verdict can rest
                on (RFC-0436 §4.1). *)
             true
           | _ -> false)
      |> List.length
  in
  if usable = 0 then Task.Anti_rationalization.Note_only
  else Task.Anti_rationalization.Usable_artifacts usable
;;

type prepared_review =
  { request : Verification.verification_request
  ; evidence_access : Workspace_verification_store.submitted_evidence_access
  ; review_request : Task.Anti_rationalization.review_request
  ; question : Task.Anti_rationalization.verdict_question
  }

(* Artifact references the store looked for and did not find. The snapshot layer
   already resolves every reference against the producer's root and answers
   [Evidence_artifact_unreadable] for the ones that lead nowhere, so this only
   collects that answer; it does not decide what counts as readable. Only
   [Evidence_missing] is collected: a file that exists but cannot be read is a
   different situation, and the reviewer is the right place to weigh it.

   A reference that names no file is not weak evidence for the authority to
   weigh, it is a submission with nothing behind it, and no reviewer can read
   what was never written. Left to the reviewer it becomes a judgement call that
   can land either way on the same evidence: task-808 was refused at 10:55 for
   an artifact the store reported missing, and approved at 10:59 with the file
   still absent. *)

let prepare_review
      ~(config : Workspace_utils_backend_setup.config)
      ~(task : Masc_domain.task)
      ~assignee
      ~verification_id
      ~(authority : Masc_domain.completion_authority)
  : (prepared_review, string) result
  =
  let* request = Verification.load_request config.base_path verification_id in
  if not (String.equal request.id verification_id)
  then
    Error
      (Printf.sprintf
         "verification request id mismatch (path=%s payload=%s)"
         verification_id
         request.id)
  else if not (String.equal request.task_id task.id)
  then
    Error
      (Printf.sprintf
         "verification request task mismatch (request=%s awaiting=%s)"
         request.task_id
         task.id)
  else if not (String.equal request.worker assignee)
  then
    Error
      (Printf.sprintf
         "verification request worker mismatch (request=%s awaiting=%s)"
         request.worker
         assignee)
  else
    let evidence_access =
      Workspace_verification_store.inspect_submitted_evidence_for_authority
        ~base_path:config.base_path
        ~request_id:verification_id
        ~task_id:task.id
        ~task_worker:assignee
        ~authority
    in
    match evidence_access with
    | Workspace_verification_store.Evidence_unavailable { request_id; reason } ->
      Error
        (Printf.sprintf
           "submitted evidence unavailable: %s"
           (Workspace_verification_store.evidence_access_failure_to_string
              ~request_id
              reason))
    | Workspace_verification_store.Evidence_available { request = header; items } ->
      if not (String.equal header.id verification_id)
      then
        Error
          (Printf.sprintf
             "submitted evidence header id mismatch (expected=%s actual=%s)"
             verification_id
             header.id)
      else if not (String.equal header.task_id task.id)
      then
        Error
          (Printf.sprintf
             "submitted evidence header task mismatch (expected=%s actual=%s)"
             task.id
             header.task_id)
      else if not (String.equal header.worker assignee)
      then
        Error
          (Printf.sprintf
             "submitted evidence header worker mismatch (expected=%s actual=%s)"
             assignee
             header.worker)
      else
        let* evidence_refs = evidence_refs_of_output request.output in
        let* question = verdict_question_of_request request in
        (* An artifact reference the store could not read is carried here,
           not refused here.

           The three header checks above are identity: this payload is not the
           one this review is for, and no reviewer can repair that. An
           unreadable artifact is a different kind of fact. The store resolves
           a reference against the producer's sandbox root only, so a path the
           submitter wrote relative to a checkout misses even when the file is
           right there — [Evidence_missing] answers the path that was asked
           for, not whether the work exists.

           Refusing here made that miss terminal. [prepare_review] runs before
           the lookup surface is built, so no evaluator ran, no verdict was
           committed, and [observe_rejection_wakeup] fires only on a committed
           [Verdict_rejected] — the producer was never told. The outcome
           recorded as [Infrastructure_unavailable], which schedules no retry.
           The Task then sat in [AwaitingVerification] until an operator
           noticed (task-568, task-785, task-845, 2026-08-29). RFC-0337
           (Withdrawn) already removed the hierarchy where evidence shape
           rejects work ahead of the judge; this was that hierarchy again,
           minus the notification.

           The judge is equipped for it: [submitted_evidence_access] carries
           each unreadable item as a typed [artifact_unreadable] with its
           reference and reason, and the judge holds Read/Grep on the same
           root plus a [root_layout] naming every checkout under it.
           [verification.lookup.producer_tree] tells it to prefix a
           checkout-relative path and to establish the tree's shape before
           calling a path absent. A REJECT from there reaches the producer. *)
        let completion_notes =
          Yojson.Safe.pretty_to_string
            (`Assoc
               [ "verification_request", Verification.request_to_yojson request
               ; ( "submitted_evidence_access"
                 , Workspace_verification_store
                    .submitted_evidence_access_transport_to_yojson
                     evidence_access )
               ])
        in
        Ok
          { request
          ; evidence_access
          ; review_request =
              { task_title = task.title
              ; task_description = task.description
              ; completion_notes
              ; agent_name = assignee
              ; task_id = task.id
              ; evidence_refs
              }
          ; question
          }
;;

type process_outcome =
  | Committed
  | Deferred
  | Retryable_deferred

let process_outcome_of_evaluator_retryable = function
  | Some true -> Retryable_deferred
  | Some false | None -> Deferred
;;

let defer ?(evaluator_retryable = None) ~task_id ~verification_id ~authority ~reason () =
  Log.Misc.warn
    "system LLM completion authority deferred task_id=%s verification_id=%s authority=%s reason=%s"
    task_id
    verification_id
    (Masc_domain.completion_authority_actor authority)
    reason;
  process_outcome_of_evaluator_retryable evaluator_retryable
;;

let observe_rejection_wakeup
      (runtime : runtime)
      (task : Masc_domain.task)
      ~assignee
      ~verification_id
      ~reason
      ~authority
  =
  match
    Completion_authority_wakeup.wake_rejected_producer
      ~config:runtime.config
      ~producer:assignee
      ~task_id:task.id
      ~verification_id
      ~reason
      ~authority
  with
  | Completion_authority_wakeup.Signaled { keeper_name } ->
    Log.Misc.info
      "completion authority rejection durably queued and signaled producer Keeper task_id=%s verification_id=%s keeper=%s"
      task.id
      verification_id
      keeper_name
  | Completion_authority_wakeup.Durable_deferred { keeper_name; wakeup } ->
    (match wakeup with
     | Keeper_registry.Deferred_unregistered ->
       Log.Misc.warn
         "completion authority rejection durably queued; producer Keeper is unregistered task_id=%s verification_id=%s keeper=%s"
         task.id
         verification_id
         keeper_name
     | Keeper_registry.Deferred_not_running phase ->
       Log.Misc.warn
         "completion authority rejection durably queued; producer Keeper is not running task_id=%s verification_id=%s keeper=%s phase=%s"
         task.id
         verification_id
         keeper_name
         (Keeper_state_machine.phase_to_string phase)
     | Keeper_registry.Deferred_lifecycle denial ->
       Log.Misc.warn
         "completion authority rejection durably queued; producer Keeper wake denied task_id=%s verification_id=%s keeper=%s reason=%s"
         task.id
         verification_id
         keeper_name
         (Keeper_lifecycle_admission.autonomous_denial_to_wire denial)
     | Keeper_registry.Signaled ->
       Log.Misc.error
         "completion authority rejection returned deferred Signaled outcome task_id=%s verification_id=%s keeper=%s"
         task.id
         verification_id
         keeper_name)
  | Completion_authority_wakeup.Durable_wake_failed { keeper_name; detail } ->
    Log.Misc.error
      "completion authority rejection durably queued but live wake failed task_id=%s verification_id=%s keeper=%s detail=%s"
      task.id
      verification_id
      keeper_name
      detail
  | Completion_authority_wakeup.Unroutable_producer { producer; task_id } ->
    Log.Misc.error
      "completion authority rejection has no registered or persisted Keeper producer binding task_id=%s producer=%s verification_id=%s"
      task_id
      producer
      verification_id
  | Completion_authority_wakeup.Producer_identity_lookup_failed
      { producer; task_id; detail } ->
    Log.Misc.error
      "completion authority rejection producer identity lookup failed task_id=%s producer=%s verification_id=%s detail=%s"
      task_id
      producer
      verification_id
      detail
  | Completion_authority_wakeup.Durable_queue_failed { keeper_name; detail } ->
    Log.Misc.error
      "completion authority rejection durable queue failed task_id=%s verification_id=%s keeper=%s detail=%s"
      task.id
      verification_id
      keeper_name
      detail
;;

(* Returns the control-flow outcome the scan loop acts on, paired with the
   observation outcome recorded for this review. [on_commit] is the exact
   semantic verdict produced by the evaluator. Infrastructure failures never
   call this function. A failed commit is its own outcome because the verdict
   was decided but never reached the Task. *)
let commit_verdict
      (runtime : runtime)
      (task : Masc_domain.task)
      ~assignee
      ~verification_id
      ~authority
      ~verdict
      ~notes
      ~verdict_label
      ~on_commit
      ~(evaluator_runtime : string option)
  =
  match
    Workspace.commit_verdict_r
      runtime.config
      ~authority
      ~verdict
      ~task_id:task.id
      ~verification_id
      ~notes
      ?evaluator_runtime
      ()
  with
  | Ok _ ->
    (match verdict with
     | Masc_domain.Verdict_approved -> ()
     | Masc_domain.Verdict_rejected { reason } ->
       observe_rejection_wakeup
         runtime
         task
         ~assignee
         ~verification_id
         ~reason
         ~authority);
    Log.Misc.info
      "system LLM completion authority committed task_id=%s verification_id=%s authority=%s verdict=%s"
      task.id
      verification_id
      (Masc_domain.completion_authority_actor authority)
      verdict_label;
    Committed, on_commit
  | Error error ->
    let detail = Masc_domain.masc_error_to_string error in
    ( defer ~task_id:task.id ~verification_id ~authority ~reason:detail ()
    , Verification_run_registry.Commit_failed { detail } )
;;

(** What the system lane does with one Task it was woken for, read off the
    status alone. Which question was asked lives on the Task, put there by
    the transition that created the obligation; it is not copied into the
    request record, so one field has one owner and the record cannot disagree
    with the status the verdict is applied to.

    RFC-0417 §4.1: completion review is the system LLM's job; a cancellation
    is permission for work to stop existing, and that authority belongs to
    the operator's one click. No review prompt exists for a cancel claim —
    the lane records it as [Verification_run_registry.Operator_routed] and
    the Task stays [AwaitingVerification] (§5 stay_pending) until the
    operator clicks, so a keeper's refusal of its own cancel request cannot
    be laundered into a system-LLM verdict either. Pure, so the routing is
    testable without a runtime. *)
type admission =
  | Review_completion
  | Operator_routed
  | Not_awaiting

let admission_of_status = function
  | Masc_domain.AwaitingVerification { intent = Masc_domain.Complete_task; _ } ->
    Review_completion
  | Masc_domain.AwaitingVerification { intent = Masc_domain.Cancel_task; _ } ->
    Operator_routed
  | Masc_domain.Todo
  | Masc_domain.Claimed _
  | Masc_domain.InProgress _
  | Masc_domain.Done _
  | Masc_domain.Cancelled _ -> Not_awaiting
;;

let process_task_once
      (runtime : runtime)
      (task : Masc_domain.task)
      ~assignee
      ~verification_id
  =
  (* RFC-0361 D7(b): fixed identity, not a per-judgement random mint — see
     [authority_actor] above. *)
  let authority = Masc_domain.System_llm_agent { agent_run_id = authority_actor } in
  (* Register before any work. Every exit below records through [complete],
     including paths that produce no semantic verdict. *)
  let registry = Verification_run_registry.global () in
  let started_at = Eio.Time.now runtime.clock in
  let tools = ref [] in
  let on_tool_result ~input result =
    tools :=
      Verification_run_registry.observe_tool_result
        ~input
        ~finished_at:(Eio.Time.now runtime.clock)
        result
      :: !tools
  in
  Verification_run_registry.register_running
    registry
    ~verification_id
    ~task_id:task.id
    ~producer:assignee
    ~authority_kind:(Masc_domain.completion_authority_kind authority)
    ~authority_actor:(Masc_domain.completion_authority_actor authority)
    ~started_at;
  let complete ?evaluator_runtime (process_outcome, outcome) =
    Verification_run_registry.mark_completed
      registry
      ~verification_id
      ~outcome
      ~tools:(List.rev !tools)
      ?evaluator_runtime
      ~elapsed_s:(Eio.Time.now runtime.clock -. started_at)
      ();
    process_outcome
  in
  let defer_unavailable ~stage ~detail =
    complete
      ( defer ~task_id:task.id ~verification_id ~authority ~reason:detail ()
      , Verification_run_registry.Infrastructure_unavailable { stage; detail } )
  in
  try
    match admission_of_status task.task_status with
    (* Not deferred for retry and not auto-finalized: the operator's click is
       the next event, and the row says so. *)
    | Operator_routed -> complete (Deferred, Verification_run_registry.Operator_routed)
    | Not_awaiting ->
      defer_unavailable
        ~stage:Verification_run_registry.Review_preparation
        ~detail:
          (Printf.sprintf
             "task %s is not awaiting a verdict (status=%s)"
             task.id
             (Masc_domain.task_status_to_string task.task_status))
    | Review_completion ->
    match
      prepare_review
        ~config:runtime.config
        ~task
        ~assignee
        ~verification_id
        ~authority
    with
    | Error reason ->
      defer_unavailable
        ~stage:Verification_run_registry.Review_preparation
        ~detail:reason
    | Ok prepared ->
      (match
         Verification_authority_tools.create
           ~config:runtime.config
           ~producer:assignee
       with
       | Error reason ->
         defer_unavailable
           ~stage:Verification_run_registry.Lookup_surface
           ~detail:reason
       | Ok lookup_tools ->
         (match Verification_authority_tools.root_layout lookup_tools with
          | Error reason ->
            defer_unavailable
              ~stage:Verification_run_registry.Lookup_surface
              ~detail:reason
          | Ok root_layout ->
            let lookup =
              Task.Anti_rationalization.Lookup_tools
                { schemas = Verification_authority_tools.schemas lookup_tools
                ; dispatch = Verification_authority_tools.dispatch lookup_tools
                ; root_layout
                }
            in
            (* Human labels close the judge's own loop: where an operator
               disagreed with a past verdict, the divergence returns to the
               judge as a few-shot example, false positives first. Filled
               here, where the ledger and the snapshot are read, so the
               question picker stays a pure mapping. *)
            let question =
              { prepared.question with
                Task.Anti_rationalization.few_shot_block =
                  Eval_calibration.format_few_shot_block
                    (Eval_calibration.select_examples
                       ~max_examples:judge_few_shot_examples)
              ; evidence_posture =
                  evidence_posture_of_snapshot prepared.evidence_access
              }
            in
            let result =
              Task.Anti_rationalization.review
                ~base_path:runtime.config.base_path
                ~sw:(Some runtime.sw)
                ~lookup
                ~question
                ~on_tool_result
                prepared.review_request
            in
            (* Every verdict lands in the calibration ledger the Harness
               surface reads — which evaluator judged, and whether a fallback
               answered for the intended one. The judge kept running after the
               July evaluation flow was removed, but this call went with it,
               and the ledger starved for a month while verdicts flowed
               (last record 2026-07-27; the surface read as a museum). A
               non-verdict is a no-op inside; the stalled path below already
               reaches the Board. *)
            Eval_calibration.record_verdict ~task_id:task.id
              ~req:prepared.review_request ~result ();
            let evaluator_runtime = result.evaluator_runtime in
            match result.verdict with
       | None ->
         let gate = Task.Anti_rationalization.gate_to_string result.gate in
         (* Both arms are real descriptions, not a default standing in for an
            unknown: the reviewer's own fallback text when it produced one, the
            gate name when it did not. Written as a match so neither arm hides
            inside an [Option.value ~default]. *)
         let detail =
           match result.fallback_reason with
           | Some reason -> reason
           | None -> gate
         in
         (* No verdict means nobody judged this submission. The registry row
            alone reaches no one, so the outcome is always promoted to the
            Board, where the producer Keeper and the operator both read it and
            decide whether to resubmit. The authority does not decide that on
            their behalf. *)
         Verification_protocol.notify_stalled_verification
           ~authority
           ~task_id:task.id
           ~verification_id
           ~gate
           ~detail;
         complete
           ~evaluator_runtime
           ( defer
               ~evaluator_retryable:result.evaluator_error_retryable
               ~task_id:task.id
               ~verification_id
               ~authority
               ~reason:detail
               ()
           , Verification_run_registry.Not_reviewed { gate; detail } )
       | Some review_verdict ->
         let verdict = completion_verdict_of_review review_verdict in
         let notes =
           review_notes
             ~request:prepared.request
             ~evidence_access:prepared.evidence_access
             ~result
             ~authority
         in
         (* Built from [review_verdict], not from [verdict]: the domain verdict
            drops the reason on approval, and the registry row is where an
            operator later asks what the reviewer checked. *)
         let on_commit =
           match review_verdict with
           | Task.Anti_rationalization.Approve reason ->
             Verification_run_registry.Approved { reason }
           | Task.Anti_rationalization.Reject reason ->
             Verification_run_registry.Rejected { reason }
         in
         complete
           ~evaluator_runtime
           (commit_verdict
              runtime
              task
              ~assignee
              ~verification_id
              ~authority
              ~verdict
              ~notes
              ~verdict_label:
                (Task.Anti_rationalization.verdict_constructor_name review_verdict)
              ~on_commit
              ~evaluator_runtime:(Some evaluator_runtime))))
  with
  | Eio.Cancel.Cancelled _ as exn ->
    (* Mirror the four exact lanes: a cancelled review completes its
       observation row before the cancellation continues, so it neither
       stays Running in memory nor vanishes on the next replay (lane audit
       W6 — a cancelled verifier run used to leave no trace at all). The
       task itself stays awaiting_verification; the boot sweep re-reviews
       it. *)
    Eio.Cancel.protect (fun () ->
      complete
        ( ()
        , Verification_run_registry.Review_cancelled
            { detail = "review fiber cancelled: " ^ Printexc.to_string exn }
        ));
    raise exn
  | exn ->
    let detail = Printexc.to_string exn in
    complete
      ( defer ~task_id:task.id ~verification_id ~authority ~reason:detail ()
      , Verification_run_registry.Raised { detail } )
;;

(* Boot and a failed backlog read have no key to aim at. Everything else does,
   and must use [request_review] instead. *)
let request_sweep (runtime : runtime) =
  Atomic.set runtime.sweep_pending true;
  Eio.Condition.broadcast runtime.wake
;;

let request_review (runtime : runtime) key =
  let rec loop () =
    let current = Atomic.get runtime.targets in
    if List.exists (review_key_equal key) current
    then ()
    else if Atomic.compare_and_set runtime.targets current (key :: current)
    then ()
    else loop ()
  in
  loop ();
  Eio.Condition.broadcast runtime.wake
;;

let schedule_retry (runtime : runtime) key =
  if Atomic.compare_and_set runtime.retry_scheduled false true
  then
    Eio.Fiber.fork ~sw:runtime.sw (fun () ->
      Eio.Time.sleep runtime.clock runtime.retry_interval_sec;
      Atomic.set runtime.retry_scheduled false;
      request_review runtime key)
;;

let schedule_sweep_retry (runtime : runtime) =
  if Atomic.compare_and_set runtime.retry_scheduled false true
  then
    Eio.Fiber.fork ~sw:runtime.sw (fun () ->
      Eio.Time.sleep runtime.clock runtime.retry_interval_sec;
      Atomic.set runtime.retry_scheduled false;
      request_sweep runtime)
;;

let process_task (runtime : runtime) (task : Masc_domain.task) ~assignee ~verification_id =
  let key = { task_id = task.id; verification_id } in
  if claim_review runtime key
  then (
    (* A retryable evaluator failure has no producer action that can legally
       advance the Task: it is still [AwaitingVerification]. Re-arm the
       application-owned scan after the durable run/Board observation is
       recorded. Non-retryable deferrals keep the existing producer/operator
       contract, and restart recovery still comes from [start]'s initial
       pending scan. *)
    let outcome =
      Fun.protect
        ~finally:(fun () -> release_review runtime key)
        (fun () -> process_task_once runtime task ~assignee ~verification_id)
    in
    match outcome with
    | Retryable_deferred ->
      Log.Misc.info
        "system LLM completion authority scheduled retry task_id=%s verification_id=%s interval_sec=%.1f"
        task.id
        verification_id
        runtime.retry_interval_sec;
      schedule_retry runtime key
    | Committed -> ()
    | Deferred ->
      (* Nothing schedules another look at this key: the scope rule admits it
         again only through a fresh submission, or through the sweep, which is
         armed at boot and after a failed backlog read rather than on a timer.
         So the Task sits in [AwaitingVerification] until a producer or operator
         acts, and this is the one line that says so — at the level its
         retryable sibling above already uses. *)
      Log.Misc.info
        "system LLM completion authority settled without retry; producer or operator owns the next move task_id=%s verification_id=%s"
        task.id
        verification_id)
  else
    Log.Misc.debug
      "system LLM completion authority skipped duplicate in-flight review task_id=%s verification_id=%s"
      task.id
      verification_id
;;

let process_scope (runtime : runtime) ~scope =
  match Workspace_backlog.read_backlog_r runtime.config with
  | Error detail ->
    Log.Misc.error
      "system LLM completion authority backlog read failed; pending tasks remain unresolved: %s"
      detail;
    (* The read failed, so which keys are awaiting is unknown. Re-read the whole
       backlog rather than guess. *)
    schedule_sweep_retry runtime
  | Ok backlog ->
    let entries =
      List.filter_map
        (fun (task : Masc_domain.task) ->
           match task.task_status with
           | Masc_domain.AwaitingVerification { assignee; verification_id; _ } ->
             Some ({ task_id = task.id; verification_id }, (task, assignee))
           | Masc_domain.Todo
           | Masc_domain.Claimed _
           | Masc_domain.InProgress _
           | Masc_domain.Done _
           | Masc_domain.Cancelled _ -> None)
        backlog.tasks
    in
    List.iter
      (fun (key, (task, assignee)) ->
         Eio.Fiber.fork ~sw:runtime.sw (fun () ->
           Eio.Semaphore.acquire runtime.review_slots;
           (* fun-protect-finally-ok: [Eio.Semaphore.release] is
              non-suspending and must return the bounded review slot on
              normal completion, exception, or cancellation. *)
           Fun.protect
             ~finally:(fun () -> Eio.Semaphore.release runtime.review_slots)
             (fun () ->
                process_task runtime task ~assignee ~verification_id:key.verification_id)))
      (entries_in_scope ~scope entries)
;;

let run (runtime : runtime) : [ `Stop_daemon ] =
  Eio.Condition.loop_no_mutex runtime.wake (fun () ->
    (* Targets first: a named verification is the common case and costs one
       backlog read for the batch that accumulated since the last wake. The
       sweep stays for boot and for a failed read, which have no key to aim at. *)
    (match take_targets runtime with
     | [] -> ()
     | (_ :: _) as keys -> process_scope runtime ~scope:(Targets keys));
    if Atomic.exchange runtime.sweep_pending false
    then process_scope runtime ~scope:Whole_backlog;
    None)
;;

let install_callback (runtime : runtime) =
  Atomic.set Workspace_hooks.verification_submitted_fn
    (fun config ~task ~assignee ~verification_id ->
       if not (String.equal config.base_path runtime.config.base_path)
       then
         Log.Misc.error
           "system LLM completion authority rejected submit from another base path task_id=%s verification_id=%s"
           task.id
           verification_id
       else if String.equal (String.trim verification_id) ""
       then
         Log.Misc.error
           "system LLM completion authority rejected empty verification id task_id=%s"
           task.id
       else (
         request_review runtime { task_id = task.id; verification_id };
         Log.Misc.info
           "system LLM completion authority scheduled task_id=%s verification_id=%s producer=%s"
           task.id
           verification_id
           assignee))
;;

let start ~sw ~clock ~(config : Workspace_utils_backend_setup.config) =
  Eio.Switch.check sw;
  let runtime =
    { config
    ; sw
    ; clock
    ; wake = Eio.Condition.create ()
    ; sweep_pending = Atomic.make true
    ; targets = Atomic.make []
    ; retry_scheduled = Atomic.make false
    ; retry_interval_sec = Env_config.Timeouts.maintenance_pulse_interval_sec
    ; in_flight = Atomic.make []
    ; review_slots = Eio.Semaphore.make 4
    }
  in
  let owner = Some runtime in
  match Atomic.compare_and_set active_runtime None owner with
  | false ->
    (match Atomic.get active_runtime with
     | Some active when String.equal active.config.base_path config.base_path ->
       Log.Misc.warn
         "system LLM completion authority already started for base path %s"
         config.base_path
     | Some active ->
       Log.Misc.error
         "system LLM completion authority already owns base path %s; refusing second base path %s"
         active.config.base_path
         config.base_path
     | None ->
       (* A concurrent starter won the CAS between the failed read and this
          branch. The next bootstrap owns the diagnostic; no duplicate lane is
          created here. *)
       Log.Misc.error
         "system LLM completion authority start race left no visible owner")
  | true ->
    let previous_submitted_hook =
      Atomic.get Workspace_hooks.verification_submitted_fn
    in
    install_callback runtime;
    Eio.Switch.on_release sw (fun () ->
      if Atomic.compare_and_set active_runtime owner None
      then Atomic.set Workspace_hooks.verification_submitted_fn previous_submitted_hook);
    Eio.Fiber.fork_daemon ~sw (fun () -> run runtime)
;;

module For_testing = struct
  let authority_actor = authority_actor
  let evidence_refs_of_output = evidence_refs_of_output
  let verdict_question_of_request = verdict_question_of_request
  let completion_verdict_of_review = completion_verdict_of_review
  let review_notes = review_notes

  type nonrec process_outcome = process_outcome =
    | Committed
    | Deferred
    | Retryable_deferred

  let process_outcome_of_evaluator_retryable =
    process_outcome_of_evaluator_retryable

  type nonrec review_key = review_key =
    { task_id : string
    ; verification_id : string
    }

  type nonrec scan_scope = scan_scope =
    | Whole_backlog
    | Targets of review_key list

  let entries_in_scope = entries_in_scope

  type nonrec admission = admission =
    | Review_completion
    | Operator_routed
    | Not_awaiting

  let admission_of_status = admission_of_status
end
