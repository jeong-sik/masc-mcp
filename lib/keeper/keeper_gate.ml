type causal_context =
  { turn_id : int option
  ; snapshot : Yojson.Safe.t
  }

type request =
  { keeper_name : string
  ; operation : string
  ; input : Yojson.Safe.t
  ; call_summary : string option
  ; base_path : string
  ; causal_context : causal_context option
  ; task_id : string option
  ; continuation_channel : Keeper_continuation_channel.t option
  ; sandbox_profile : Keeper_types_profile_sandbox.sandbox_profile option
  }

(* Gate operation vocabulary — the strings the approval store keys on and
   the host replay engine dispatches on. The Gate owns them so that
   [replayable_operation], which decides what the deferred payload may
   promise the model, cannot drift from the literal a producer submits:
   a deferred "the host replays this exact call" spoken over an operation
   the replay engine does not recognize starves the approved effect
   silently (2026-09-02 keeper_voice_speak incident, #32668). *)
let filesystem_write_gate_operation = "filesystem_write"

let tool_execute_gate_operation = "tool_execute"

let network_read_gate_operation = "network_read"

let connector_post_gate_operation = "connector_post"

let identity_call_gate_operation = "identity_call"

(* Not read from Keeper_tool_voice_runtime.command_to_string: that module
   sits above the Gate in the dependency graph (its chat/broadcast leaves
   reach back here through the turn driver), so borrowing the literal
   would close a cycle. The pairing with command_to_string Speak is pinned
   by test_speak_replays_while_listen_never_reaches_the_gate. *)
let voice_speak_gate_operation = "keeper_voice_speak"

type replayable =
  | Replay_write
  | Replay_execute
  | Replay_network_read
  | Replay_connector_post
  | Replay_identity
  | Replay_voice_speak

let replayable_operation operation =
  if String.equal operation filesystem_write_gate_operation
  then Some Replay_write
  else if String.equal operation tool_execute_gate_operation
  then Some Replay_execute
  else if String.equal operation network_read_gate_operation
  then Some Replay_network_read
  else if String.equal operation connector_post_gate_operation
  then Some Replay_connector_post
  else if String.equal operation identity_call_gate_operation
  then Some Replay_identity
  else if String.equal operation voice_speak_gate_operation
  then Some Replay_voice_speak
  else None
;;

type boxed_execution =
  { run : Keeper_types_profile_sandbox.observation_run
  ; result : Masc_exec.Exec_dispatch.dispatch_result
  }

type authorization_source =
  | One_shot_resolution of string
  | Exact_always_rule of string
  | Keeper_always_allow
  | Workspace_always_allow
  | Readonly_sandbox
  | Observed_in_box of boxed_execution

type observation =
  | Observed_result of boxed_execution
  | Observed_refused of
      { status : Unix.process_status
      ; stderr : string
      }
  | Observation_unavailable of string

type authorization =
  { source : authorization_source
  ; audit_receipts : Keeper_approval.Audit.receipt list
  }

type deferred_reason =
  | Human_requested
  | Judge_requested
  | Auto_judge_unavailable of string
  | Mode_state_invalid of string

type unavailable_reason =
  | Queue_storage_unavailable of Keeper_approval_queue.storage_error
  | Approval_grant_unavailable of Keeper_approval_queue.grant_error
  | Approval_grant_consumption_in_progress of string

type decision =
  | Allow of authorization
  | Deferred of
      { operation : string
      ; approval_id : string
      ; reason : deferred_reason
      ; audit_receipts : Keeper_approval.Audit.receipt list
      }
  | Unavailable of unavailable_reason

type auto_judge_completion_rejection =
  | Completion_not_found
  | Completion_key_mismatch
  | Completion_invalid_identity
  | Completion_summary_not_pending
  | Completion_unbound_state
  | Completion_disposition_conflict
  | Completion_identity_conflict
  | Completion_status_conflict
  | Completion_provenance_mismatch
  | Completion_content_conflict

type auto_judge_resume_failure_code =
  | Resume_worker_start_failed
  | Resume_identity_unbound
  | Resume_completion_persistence_uncertain
  | Resume_completion_rejected of auto_judge_completion_rejection
  | Resume_judgment_resolution_failed
  | Resume_exact_state_not_completed

let auto_judge_completion_rejection_to_string = function
  | Completion_not_found -> "not_found"
  | Completion_key_mismatch -> "key_mismatch"
  | Completion_invalid_identity -> "invalid_identity"
  | Completion_summary_not_pending -> "summary_not_pending"
  | Completion_unbound_state -> "unbound_state"
  | Completion_disposition_conflict -> "disposition_conflict"
  | Completion_identity_conflict -> "identity_conflict"
  | Completion_status_conflict -> "status_conflict"
  | Completion_provenance_mismatch -> "provenance_mismatch"
  | Completion_content_conflict -> "content_conflict"
;;

let auto_judge_resume_failure_code_to_string = function
  | Resume_worker_start_failed -> "worker_start_failed"
  | Resume_identity_unbound -> "identity_unbound"
  | Resume_completion_persistence_uncertain -> "completion_persistence_uncertain"
  | Resume_completion_rejected rejection ->
    "completion_rejected:"
    ^ auto_judge_completion_rejection_to_string rejection
  | Resume_judgment_resolution_failed -> "judgment_resolution_failed"
  | Resume_exact_state_not_completed -> "exact_state_not_completed"
;;

let completion_rejection_of_exact_attempt = function
  | Keeper_approval_queue.Exact_attempt_not_found _ ->
    Completion_not_found
  | Keeper_approval_queue.Exact_attempt_key_mismatch _ ->
    Completion_key_mismatch
  | Keeper_approval_queue.Exact_attempt_invalid_identity _ ->
    Completion_invalid_identity
  | Keeper_approval_queue.Exact_attempt_summary_not_pending _ ->
    Completion_summary_not_pending
  | Keeper_approval_queue.Exact_attempt_unbound_state _ ->
    Completion_unbound_state
  | Keeper_approval_queue.Exact_attempt_disposition_conflict _ ->
    Completion_disposition_conflict
  | Keeper_approval_queue.Exact_attempt_identity_conflict _ ->
    Completion_identity_conflict
  | Keeper_approval_queue.Exact_attempt_status_conflict _ ->
    Completion_status_conflict
  | Keeper_approval_queue.Exact_attempt_provenance_mismatch _ ->
    Completion_provenance_mismatch
  | Keeper_approval_queue.Exact_attempt_content_conflict _ ->
    Completion_content_conflict
;;

let completion_rejection_operator_detail = function
  | Completion_not_found ->
    "Exact completion was rejected because the approval no longer exists."
  | Completion_key_mismatch ->
    "Exact completion was rejected because the durable row identity changed."
  | Completion_invalid_identity ->
    "Exact completion was rejected because its identity is invalid."
  | Completion_summary_not_pending ->
    "Exact completion was rejected because the summary is not pending."
  | Completion_unbound_state ->
    "Exact completion was rejected because no attempt identity is bound."
  | Completion_disposition_conflict ->
    "Exact completion was rejected because the durable disposition changed."
  | Completion_identity_conflict ->
    "Exact completion was rejected because a different attempt is bound."
  | Completion_status_conflict ->
    "Exact completion was rejected by the durable attempt status."
  | Completion_provenance_mismatch ->
    "Exact completion was rejected because its provenance does not match."
  | Completion_content_conflict ->
    "Exact completion was rejected because different summary content is already durable."
;;

type auto_judge_resume_failure =
  { approval_id : string
  ; code : auto_judge_resume_failure_code
  ; operator_detail : string
  }

type auto_judge_resume_report =
  { requested : int
  ; started_ids : string list
  ; finalized_ids : string list
  ; skipped_ids : string list
  ; failures : auto_judge_resume_failure list
  ; queue_error : Keeper_approval_queue.storage_error option
  }

type cycle_grant_entry =
  { approval_id : string }

type cycle_grant_state =
  | Cycle_grant_available of cycle_grant_entry
  | Cycle_grant_consuming of cycle_grant_entry
  | Cycle_grant_consumed

type cycle_grant = cycle_grant_state Atomic.t

type cycle_grant_take_result =
  | Cycle_grant_authorized of string * Keeper_approval.Audit.receipt
  | Cycle_grant_not_applicable
  | Cycle_grant_temporarily_unavailable of string * unavailable_reason

let cycle_grant_of_resolution (resolution : Keeper_event_queue.hitl_resolution) =
  match resolution.decision with
  | Keeper_event_queue.Hitl_approved ->
    Some
      (Atomic.make
         (Cycle_grant_available { approval_id = resolution.approval_id }))
  | Keeper_event_queue.Hitl_rejected _ -> None
;;

let rec take_matching_cycle_grant grant request =
  match Atomic.get grant with
  | Cycle_grant_consumed -> Cycle_grant_not_applicable
  | Cycle_grant_consuming entry ->
    Cycle_grant_temporarily_unavailable
      ( entry.approval_id
      , Approval_grant_consumption_in_progress entry.approval_id )
  | Cycle_grant_available entry as current ->
    let reserved = Cycle_grant_consuming entry in
    if Atomic.compare_and_set grant current reserved
    then (
      match
        Keeper_approval_queue.consume_approved_resolution
        ~base_path:request.base_path
        ~id:entry.approval_id
        ~keeper_name:request.keeper_name
        ~tool_name:request.operation
        ~input:request.input
      with
      | Error error ->
        Atomic.set grant current;
        Cycle_grant_temporarily_unavailable
          (entry.approval_id, Approval_grant_unavailable error)
      | Ok Keeper_approval_queue.Consumption_not_matching ->
        Atomic.set grant current;
        Cycle_grant_not_applicable
      | Ok Keeper_approval_queue.Consumption_already_committed ->
        Atomic.set grant Cycle_grant_consumed;
        Cycle_grant_not_applicable
      | Ok (Keeper_approval_queue.Consumption_committed audit_receipt) ->
        Atomic.set grant Cycle_grant_consumed;
        Cycle_grant_authorized (entry.approval_id, audit_receipt))
    else take_matching_cycle_grant grant request
;;

let authorization_source_to_string = function
  | One_shot_resolution _ -> "one_shot_resolution"
  | Exact_always_rule _ -> "exact_always_rule"
  | Keeper_always_allow -> "keeper_always_allow"
  | Workspace_always_allow -> "workspace_always_allow"
  | Readonly_sandbox -> "readonly_sandbox"
  | Observed_in_box _ -> "observed_in_box"
;;

let deferred_reason_to_string = function
  | Human_requested -> "human_requested"
  | Judge_requested -> "judge_requested"
  | Auto_judge_unavailable _ ->
    Keeper_approval_queue_rules_types.summary_attempt_pre_worker_unavailable_code_to_string
      Keeper_approval_queue_rules_types.Summary_pre_worker_auto_judge_unavailable
  | Mode_state_invalid _ ->
    Keeper_approval_queue_rules_types.summary_attempt_pre_worker_unavailable_code_to_string
      Keeper_approval_queue_rules_types.Summary_pre_worker_mode_state_invalid
;;

let unavailable_reason_to_string = function
  | Queue_storage_unavailable error ->
    Keeper_approval_queue.storage_error_to_string error
  | Approval_grant_unavailable error ->
    Keeper_approval_queue.grant_error_to_string error
  | Approval_grant_consumption_in_progress approval_id ->
    Printf.sprintf "approval %s is being consumed" approval_id
;;

let source_fields = function
  | One_shot_resolution approval_id ->
    [ "authorization_source", `String "one_shot_resolution"
    ; "approval_id", `String approval_id
    ]
  | Exact_always_rule rule_id ->
    [ "authorization_source", `String "exact_always_rule"
    ; "rule_id", `String rule_id
    ]
  | Keeper_always_allow ->
    [ "authorization_source", `String "keeper_always_allow" ]
  | Workspace_always_allow ->
    [ "authorization_source", `String "workspace_always_allow" ]
  | Readonly_sandbox ->
    [ "authorization_source", `String "readonly_sandbox" ]
  | Observed_in_box { run; result = _ } ->
    [ "authorization_source", `String "observed_in_box"
    ; ( "observation_run"
      , `String (Keeper_types_profile_sandbox.observation_run_to_string run) )
    ]
;;

let request_turn_id request =
  Option.bind request.causal_context (fun context -> context.turn_id)
;;

let audit_receipts_to_yojson receipts =
  `List (List.map Keeper_approval.Audit.receipt_to_yojson receipts)
;;

let approval_sse_audit_event = "approval:audit"

let authorization_subject_id = function
  | One_shot_resolution approval_id -> Some approval_id
  | Exact_always_rule rule_id -> Some rule_id
  | Keeper_always_allow | Workspace_always_allow | Readonly_sandbox | Observed_in_box _ ->
    None
;;

(* Authorization is already committed when these receipts exist.  A failed
   audit append therefore cannot turn [Allow] into an error or invite the tool
   caller to retry the external effect.  Publish only the failed receipts as
   observation; the append boundary has already recorded the same failure in
   logs and metrics if no Dashboard SSE client is connected. *)
let broadcast_failed_authorization_audits ~keeper_name ~source receipts =
  let id = authorization_subject_id source in
  List.iter
    (fun (receipt : Keeper_approval.Audit.receipt) ->
       match receipt.write_result with
       | Ok () -> ()
       | Error _ ->
         (try
            Sse.broadcast
              (`Assoc
                  [ "type", `String approval_sse_audit_event
                  ; ( "payload"
                    , `Assoc
                        [ "id", Json_util.string_opt_to_json id
                        ; "audit", Keeper_approval.Audit.receipt_to_yojson receipt
                        ] )
                  ])
          with
          | Eio.Cancel.Cancelled _ as exn ->
            (* Authorization is already committed.  This observation lane must
               not turn cancellation into a lost [Allow] or consume a one-shot
               grant without giving the caller its authorization. *)
            Log.Keeper.warn
              ~keeper_name
              "approval audit failure SSE publish cancelled event=%s err=%s"
              (Keeper_approval.Audit.event_to_string receipt.event_type)
              (Printexc.to_string exn)
          | exn ->
            Log.Keeper.warn
              ~keeper_name
              "approval audit failure SSE publish failed event=%s err=%s"
              (Keeper_approval.Audit.event_to_string receipt.event_type)
              (Printexc.to_string exn)))
    receipts
;;

let allow request source audit_receipts =
  broadcast_failed_authorization_audits
    ~keeper_name:request.keeper_name
    ~source
    audit_receipts;
  Allow { source; audit_receipts }
;;

let decision_to_yojson = function
  | Allow authorization ->
    `Assoc
      ([ "decision", `String "allow"
       ; "audit_receipts", audit_receipts_to_yojson authorization.audit_receipts
       ]
       @ source_fields authorization.source)
  | Deferred { operation; approval_id; reason; audit_receipts } ->
    let detail =
      match reason with
      | Mode_state_invalid detail -> [ "mode_read_error", `String detail ]
      | Auto_judge_unavailable detail ->
        [ "auto_judge_error", `String detail ]
      | Human_requested | Judge_requested -> []
    in
    (* RFC-0356 host replay, stated where the model reads it: without
       this line the payload reads as a plain block and the model
       resubmits the same call while the approval is in flight —
       measured as three duplicate approvals in #28866. The promise is
       made only for operations [replayable_operation] recognizes: over
       an unrecognized one it starves the approved effect silently
       (#32668), so those are told the truth — the one-shot
       authorization arrives on the next turn and the exact call spends
       it. *)
    let on_approve =
      match replayable_operation operation with
      | Some _ ->
        "The host replays this exact call and delivers its output to you \
         automatically. Do not resubmit it; a resubmission folds onto this \
         same approval."
      | None ->
        "The resolution reaches your next turn. If approved, a one-shot \
         authorization for this exact operation and input is delivered \
         there; re-issue the exact call to spend it. A different call while \
         this one is pending opens a new request."
    in
    `Assoc
      ([ "decision", `String "deferred"
       ; "approval_id", `String approval_id
       ; "reason", `String (deferred_reason_to_string reason)
       ; "on_approve", `String on_approve
       ; "audit_receipts", audit_receipts_to_yojson audit_receipts
       ]
       @ detail)
  | Unavailable reason ->
    `Assoc
      [ "decision", `String "unavailable"
      ; "reason", `String (unavailable_reason_to_string reason)
      ]
;;

let authorization_metadata ?producer_metadata authorization =
  let fields =
    [ "gate", decision_to_yojson (Allow authorization) ]
  in
  `Assoc
    (fields
     @ Option.fold
         ~none:[]
         ~some:(fun metadata -> [ "producer", metadata ])
         producer_metadata)
;;

(* The Gate's own [authorization_source] carries the identifier it resolved
   with; the audit event already records that identifier in its own fields, so
   what it is missing is the tag. Map to the payload-free contract variant
   rather than a string, so a new Gate authority cannot reach the audit log
   without this match being updated. *)
let audit_authorization_source
  : authorization_source -> Keeper_approval_queue_rules_types.authorization_source
  = function
  | One_shot_resolution _ -> Keeper_approval_queue_rules_types.One_shot_resolution
  | Exact_always_rule _ -> Keeper_approval_queue_rules_types.Exact_always_rule
  | Keeper_always_allow -> Keeper_approval_queue_rules_types.Keeper_always_allow
  | Workspace_always_allow ->
    Keeper_approval_queue_rules_types.Workspace_always_allow
  | Readonly_sandbox -> Keeper_approval_queue_rules_types.Readonly_sandbox
  | Observed_in_box _ -> Keeper_approval_queue_rules_types.Observed_in_box
;;

let audit_allow request ?rule_match ?source_approval_id ?decision_source source =
  Keeper_approval.Audit.record
    ~base_path:request.base_path
    ~event_type:Keeper_approval.Audit.Gate_allowed
    ~authorization_source:(audit_authorization_source source)
    ~id:
      (match source with
       | One_shot_resolution approval_id -> approval_id
       | Exact_always_rule rule_id -> rule_id
       | Keeper_always_allow | Workspace_always_allow | Readonly_sandbox | Observed_in_box _ ->
         Keeper_approval_queue.generate_id ())
    ~keeper_name:request.keeper_name
    ~tool_name:request.operation
    ?turn_id:(request_turn_id request)
    ?task_id:request.task_id
    ?rule_match
    ?source_approval_id
    ?decision_source
    ()
;;

let submit ?observation request =
  Keeper_approval_queue.submit_pending
    ~keeper_name:request.keeper_name
    ~tool_name:request.operation
    ~input:request.input
    ~call_summary:request.call_summary
    ~base_path:request.base_path
    ?turn_id:(request_turn_id request)
    ?request_context:(Option.map (fun context -> context.snapshot) request.causal_context)
    ?observation
    ?task_id:request.task_id
    ?continuation_channel:request.continuation_channel
    ()
;;

(* What the box refused, in the shape the row stores and the judge reads
   (RFC-0422). The stderr tail is bounded here, once, by the operator's knob,
   so the durable row and the keeper's own deferred receipt carry the same
   bytes. *)
let observed_refusal ~status ~stderr =
  Keeper_approval_queue_rules_types.observed_refusal
    ~max_stderr_bytes:(Keeper_config.keeper_hitl_observation_stderr_bytes ())
    ~status:
      (match status with
       | Unix.WEXITED code -> Keeper_approval_queue_rules_types.Observed_exit code
       | Unix.WSIGNALED signal -> Keeper_approval_queue_rules_types.Observed_signal signal
       | Unix.WSTOPPED signal -> Keeper_approval_queue_rules_types.Observed_stopped signal)
    ~stderr
;;

let log_auto_resolution_error ~keeper_name ~approval_id reason =
  Log.Keeper.warn
    ~keeper_name
    "auto judge resolution failed approval=%s: %s"
    approval_id
    reason
;;

type judgment_finalize_outcome =
  | Judgment_finalized
  | Judgment_skipped

let resolve_judgment (entry : Keeper_approval_queue_rules_types.pending_approval) ~approval_id
      (summary : Keeper_approval_queue_rules_types.hitl_context_summary) =
  let decision =
    match summary.Keeper_approval_queue_rules_types.judgment with
    | Keeper_approval_queue_rules_types.Approve -> Some Keeper_approval_queue_rules_types.Decision.Approve
    | Keeper_approval_queue_rules_types.Deny ->
      Some (Keeper_approval_queue_rules_types.Decision.Reject summary.rationale)
    | Keeper_approval_queue_rules_types.Require_human -> None
  in
  match decision with
  | None -> Ok Judgment_skipped
  | Some decision ->
    (match
       Keeper_approval_queue.resolve_with_policy
         ~base_path:entry.audit_base_path
         ~id:approval_id
         ~decision
         ~source:Keeper_approval_queue_rules_types.Auto_judge
         ~created_by:("auto_judge:" ^ summary.model_run_id)
         ()
     with
     | Ok _ -> Ok Judgment_finalized
     | Error (Keeper_approval_queue.Not_found _ | Keeper_approval_queue.Already_resolved _) ->
       Ok Judgment_skipped
     | Error error -> Error (Keeper_approval_queue.resolve_error_to_string error))
;;

type auto_judge_start_outcome =
  | Started
  | Skipped

type auto_judge_retry_outcome =
  | Retry_started
  | Retry_skipped

module Auto_judge_owner = struct
  type t = string * string

  let compare (left_base, left_keeper) (right_base, right_keeper) =
    let by_base = String.compare left_base right_base in
    if by_base <> 0 then by_base else String.compare left_keeper right_keeper
  ;;
end

module Auto_judge_owners = Map.Make (Auto_judge_owner)
module Auto_judge_owner_set = Set.Make (Auto_judge_owner)
module Auto_judge_ids = Set.Make (String)

let auto_judge_owner (entry : Keeper_approval_queue_rules_types.pending_approval) =
  entry.audit_base_path, entry.keeper_name
;;

(** Immutable process projection of the bounded active approvals for each exact
    workspace/Keeper owner. The durable approval queue remains the work SSOT;
    this map is only atomic admission state. *)
let active_auto_judges : Auto_judge_ids.t Auto_judge_owners.t Atomic.t =
  Atomic.make Auto_judge_owners.empty
;;

let rec claim_auto_judge (entry : Keeper_approval_queue_rules_types.pending_approval) =
  let active = Atomic.get active_auto_judges in
  let owner = auto_judge_owner entry in
  let active_ids =
    Auto_judge_owners.find_opt owner active
    |> Option.value ~default:Auto_judge_ids.empty
  in
  if Auto_judge_ids.mem entry.id active_ids
     || Auto_judge_ids.cardinal active_ids
        >= Keeper_config.keeper_hitl_max_concurrent_per_keeper ()
  then false
  else
    let claimed =
      Auto_judge_owners.add
        owner
        (Auto_judge_ids.add entry.id active_ids)
        active
    in
    if Atomic.compare_and_set active_auto_judges active claimed
    then true
    else claim_auto_judge entry
;;

let rec release_auto_judge (entry : Keeper_approval_queue_rules_types.pending_approval) =
  let active = Atomic.get active_auto_judges in
  let owner = auto_judge_owner entry in
  match Auto_judge_owners.find_opt owner active with
  | Some active_ids when Auto_judge_ids.mem entry.id active_ids ->
    let remaining = Auto_judge_ids.remove entry.id active_ids in
    let released =
      if Auto_judge_ids.is_empty remaining
      then Auto_judge_owners.remove owner active
      else Auto_judge_owners.add owner remaining active
    in
    if not (Atomic.compare_and_set active_auto_judges active released)
    then release_auto_judge entry
  | Some _ | None -> ()
;;

(* Whether this process holds a live in-memory admission for [entry]. Because
   [claim_auto_judge] precedes every [reserve_pre_worker_start] and the set is
   reset empty on process start, a durable start reservation is live iff its id
   is admitted here. A stranded reservation (its claiming process gone) is
   absent, which is what boot recovery relies on to reclaim only orphans. *)
let auto_judge_entry_claimed
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  let active = Atomic.get active_auto_judges in
  match Auto_judge_owners.find_opt (auto_judge_owner entry) active with
  | Some active_ids -> Auto_judge_ids.mem entry.id active_ids
  | None -> false
;;

let active_auto_judges_for_owner ~base_path ~keeper_name =
  Auto_judge_owners.find_opt
    (base_path, keeper_name)
    (Atomic.get active_auto_judges)
  |> Option.value ~default:Auto_judge_ids.empty
  |> Auto_judge_ids.elements
;;

type auto_judge_entry_class =
  | Auto_judge_not_requested
  | Auto_judge_pending_unbound
  | Auto_judge_finalizable of Keeper_approval_queue_rules_types.hitl_context_summary
  | Auto_judge_ineligible

let classify_auto_judge_entry
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  match
    entry.summary_attempt_disposition,
    entry.exact_attempt,
    entry.summary_status
  with
  | Keeper_approval_queue_rules_types.Summary_attempt_ready,
    Keeper_approval_queue_rules_types.Exact_unbound,
    Keeper_approval_queue_rules_types.Summary_not_requested ->
    Auto_judge_not_requested
  | Keeper_approval_queue_rules_types.Summary_attempt_ready,
    Keeper_approval_queue_rules_types.Exact_unbound,
    Keeper_approval_queue_rules_types.Summary_pending ->
    Auto_judge_pending_unbound
  | ( Keeper_approval_queue_rules_types.Summary_attempt_settled
    | Keeper_approval_queue_rules_types.Summary_attempt_persistence_uncertain ),
    Keeper_approval_queue_rules_types.Exact_bound
      { status = Keeper_approval_queue_rules_types.Exact_completed; _ },
    Keeper_approval_queue_rules_types.Summary_available summary ->
    Auto_judge_finalizable summary
  | _ ->
    Auto_judge_ineligible
;;

let auto_judge_entry_ready entry =
  match classify_auto_judge_entry entry with
  | Auto_judge_not_requested
  | Auto_judge_pending_unbound ->
    true
  | Auto_judge_finalizable _
  | Auto_judge_ineligible ->
    false
;;

let compare_auto_judge_entries
      (left : Keeper_approval_queue_rules_types.pending_approval)
      (right : Keeper_approval_queue_rules_types.pending_approval)
  =
  Int.compare left.sequence right.sequence
;;

(* Every pending approval for the owner, in durable sequence order. Selection
   ranges over this list; [ready_auto_judges_for_owner] decides which member
   Auto Judge may start. *)
let owner_auto_judges_in_fifo_order ~base_path ~keeper_name entries =
  entries
  |> List.filter (fun (entry : Keeper_approval_queue_rules_types.pending_approval) ->
    String.equal entry.audit_base_path base_path
    && String.equal entry.keeper_name keeper_name)
  |> List.sort compare_auto_judge_entries
;;

let auto_judge_entry_has_start_reservation
      reserved_id
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  String.equal entry.id reserved_id
  &&
  match
    entry.summary_status,
    entry.exact_attempt,
    entry.summary_attempt_disposition
  with
  | Keeper_approval_queue_rules_types.Summary_pending,
    Keeper_approval_queue_rules_types.Exact_unbound,
    Keeper_approval_queue_rules_types.Summary_attempt_pre_worker_unavailable
      { reason_code =
          Keeper_approval_queue_rules_types.Summary_pre_worker_start_reserved
      ; _
      } ->
    true
  | _ -> false
;;

(* Sequence order is kept, but selection ranges over the entries Auto Judge can
   still start instead of testing only the FIFO head. A head that is not
   startable is not always transient. An entry whose judgment concluded in
   [Require_human] keeps [Summary_attempt_settled] with [Summary_available]
   for as long as it stays pending, because [resolve_judgment] persists no
   transition for that verdict; a [Summary_attempt_pre_worker_unavailable]
   entry waits for an explicit operator retry. Both classify as not ready, so
   testing only the head stops the whole owner queue at the first such entry
   and no drain trigger can restart it. Observed on 2026-07-28: 18 approvals
   across two Keepers sat behind two heads of exactly these two kinds, the
   oldest for 2416s, while new submissions kept firing the drain. Concurrency
   remains bounded by [claim_auto_judge], not by this ordering. Selection fills
   the owner's currently available slots in durable sequence order. *)
let ready_auto_judges_for_owner
      ?reserved_id
      ~base_path
      ~keeper_name
      entries
  =
  let startable (entry : Keeper_approval_queue_rules_types.pending_approval) =
    auto_judge_entry_ready entry
    ||
    (match reserved_id with
     | Some reserved_id ->
       auto_judge_entry_has_start_reservation reserved_id entry
     | None -> false)
  in
  let available =
    Keeper_config.keeper_hitl_max_concurrent_per_keeper ()
    - List.length (active_auto_judges_for_owner ~base_path ~keeper_name)
    |> max 0
  in
  let candidates =
    owner_auto_judges_in_fifo_order ~base_path ~keeper_name entries
    |> List.filter startable
  in
  let rec take n acc = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | entry :: rest -> take (n - 1) (entry :: acc) rest
  in
  match reserved_id with
  | Some reserved_id when available > 0 ->
    (match
       List.find_opt
         (fun (entry : Keeper_approval_queue_rules_types.pending_approval) ->
            String.equal entry.id reserved_id)
         candidates
     with
     | Some entry -> [ entry ]
     | None -> [])
  | Some _ -> []
  | None -> take available [] candidates
;;

type auto_judge_drain_blocker =
  | Drain_owner_at_capacity of string list
  | Drain_entry_changed of string
  | Drain_entry_missing of string
  | Drain_start_failed of string * string
  | Drain_mode_manual
  | Drain_mode_always_allow

let auto_judge_drain_blocker_to_string = function
  | Drain_owner_at_capacity approval_ids ->
    Printf.sprintf
      "Auto Judge retry could not start because the owner has %d active worker(s): %s"
      (List.length approval_ids)
      (String.concat "," approval_ids)
  | Drain_entry_changed approval_id ->
    Printf.sprintf
      "Auto Judge retry could not start because approval %s changed before worker start"
      approval_id
  | Drain_entry_missing approval_id ->
    Printf.sprintf
      "Auto Judge retry could not start because approval %s is no longer pending"
      approval_id
  | Drain_start_failed (approval_id, reason) ->
    Printf.sprintf
      "Auto Judge retry could not start because approval %s failed before worker start: %s"
      approval_id
      reason
  | Drain_mode_manual ->
    "Auto Judge retry could not start because Gate mode changed to manual"
  | Drain_mode_always_allow ->
    "Auto Judge retry could not start because Gate mode changed to always_allow"
;;

type auto_judge_drain_outcome =
  { started_ids : string list
  ; failures : (string * string) list
  ; blocker : auto_judge_drain_blocker option
  }

type auto_judge_owner_failure =
  { keeper_name : string
  ; approval_id : string option
  ; operator_detail : string
  }

type auto_judge_workspace_drain_report =
  { started_ids : string list
  ; failures : auto_judge_owner_failure list
  }

let drain_auto_judge_owners_with ~drain_owner owners =
  let started_ids, failures =
    List.fold_left
      (fun (started_ids, failures) (base_path, keeper_name) ->
         match drain_owner ~base_path ~keeper_name with
         | Error operator_detail ->
           ( started_ids
           , { keeper_name; approval_id = None; operator_detail } :: failures )
         | Ok (owner_started_ids, owner_failures) ->
           let started_ids = List.rev_append owner_started_ids started_ids in
           let failures =
             List.fold_left
               (fun failures (approval_id, operator_detail) ->
                  { keeper_name
                  ; approval_id = Some approval_id
                  ; operator_detail
                  }
                  :: failures)
               failures
               owner_failures
           in
           started_ids, failures)
      ([], [])
      owners
  in
  { started_ids = List.rev started_ids
  ; failures = List.rev failures
  }
;;

type hitl_worker_spawner =
  sw:Eio.Switch.t ->
  entry:Keeper_approval_queue_rules_types.pending_approval ->
  on_summary:(Keeper_approval_queue_rules_types.hitl_context_summary -> unit) ->
  on_finish:(Hitl_summary_worker.finish_outcome -> unit) ->
  unit ->
  (Hitl_summary_worker.spawn_outcome, string) result

let mark_pre_worker_unavailable
      (entry : Keeper_approval_queue_rules_types.pending_approval)
      ~reason_code
      ~operator_detail
  =
  Keeper_approval_queue.mark_summary_attempt_pre_worker_unavailable
    ~base_path:entry.audit_base_path
    ~id:entry.id
    ~input_hash:entry.input_hash
    ~sequence:entry.sequence
    ~reason_code
    ~operator_detail
;;

let durable_pre_worker_unavailable_error reason = function
  | Ok true -> reason
  | Ok false ->
    reason ^ "; durable pre-worker blocked observation was not applied"
  | Error error ->
    reason
    ^ "; durable pre-worker blocked observation failed: "
    ^ Keeper_approval_queue.exact_attempt_error_to_string error
;;

let reserve_pre_worker_start
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  match
    mark_pre_worker_unavailable
      entry
      ~reason_code:Keeper_approval_queue_rules_types.Summary_pre_worker_start_reserved
      ~operator_detail:
        Keeper_approval_queue.summary_attempt_start_reserved_operator_detail
  with
  | Ok true -> Ok ()
  | Ok false ->
    Error "Auto Judge worker start reservation was not applied"
  | Error error ->
    Error
      ("Auto Judge worker start reservation failed: "
       ^ Keeper_approval_queue.exact_attempt_error_to_string error)
;;

let rec run_reserved_auto_judge_entry_with
      ~(spawn_worker : hitl_worker_spawner)
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  let approval_id = entry.id in
  let on_summary summary =
    match resolve_judgment entry ~approval_id summary with
    | Ok (Judgment_finalized | Judgment_skipped) -> ()
    | Error reason ->
      log_auto_resolution_error
        ~keeper_name:entry.keeper_name
        ~approval_id
        reason
  in
  let fail_before_worker ~reason =
    Fun.protect
      ~finally:(fun () -> release_auto_judge entry)
      (fun () ->
         mark_pre_worker_unavailable
           entry
           ~reason_code:
             Keeper_approval_queue_rules_types.Summary_pre_worker_auto_judge_unavailable
           ~operator_detail:reason
         |> durable_pre_worker_unavailable_error reason
         |> Result.error)
  in
  let start_after_reservation () =
  match Eio_context.get_root_switch_opt () with
  | Some sw ->
    (try
       match
         spawn_worker
           ~sw
           ~entry
           ~on_summary
           ~on_finish:(fun finish_outcome ->
             release_auto_judge entry;
             match finish_outcome with
             | Hitl_summary_worker.Conclusive_terminalization ->
               ignore
                 (drain_auto_judge_owner
                    ~base_path:entry.audit_base_path
                    ~keeper_name:entry.keeper_name
                    ())
             | Hitl_summary_worker.Terminalization_identity_unbound ->
               Log.Keeper.warn
                 ~keeper_name:entry.keeper_name
                 "Auto Judge terminalization blocked before exact attempt identity was bound; durable pending approval retained approval=%s"
                 entry.id
             | Hitl_summary_worker.Terminalization_persistence_uncertain ->
               Log.Keeper.error
                 ~keeper_name:entry.keeper_name
                 "Auto Judge owner drain withheld after persistence uncertainty \
                  approval=%s"
                 entry.id
             | Hitl_summary_worker.Terminalization_rejected ->
               Log.Keeper.warn
                 ~keeper_name:entry.keeper_name
                 "Auto Judge owner drain withheld after deterministic exact-attempt rejection approval=%s"
                 entry.id
             )
           ()
       with
       | Ok Hitl_summary_worker.Worker_forked -> Ok Started
       | Error reason -> fail_before_worker ~reason
     with
     | Eio.Cancel.Cancelled _ as exn ->
       let backtrace = Printexc.get_raw_backtrace () in
       let reason =
         "Auto Judge worker start was cancelled: " ^ Printexc.to_string exn
       in
       Eio.Cancel.protect (fun () ->
         match fail_before_worker ~reason with
         | Ok _ | Error _ -> ());
       Printexc.raise_with_backtrace exn backtrace
     | exn ->
       let reason =
         "Auto Judge worker start failed: " ^ Printexc.to_string exn
       in
       fail_before_worker ~reason)
  | None ->
    fail_before_worker
      ~reason:"Auto Judge unavailable: server root switch is not installed"
  in
  start_after_reservation ()

and spawn_claimed_auto_judge_entry_with
      ~(spawn_worker : hitl_worker_spawner)
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  match reserve_pre_worker_start entry with
  | Ok () -> run_reserved_auto_judge_entry_with ~spawn_worker entry
  | Error reason ->
    release_auto_judge entry;
    Error reason

and spawn_claimed_auto_judge_entry_detached_with
      ~(spawn_worker : hitl_worker_spawner)
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  match reserve_pre_worker_start entry with
  | Error reason ->
    release_auto_judge entry;
    Error reason
  | Ok () ->
    (match Eio_context.get_root_switch_opt () with
     | None ->
       let reason = "Auto Judge unavailable: server root switch is not installed" in
       let reason =
         mark_pre_worker_unavailable
           entry
           ~reason_code:
             Keeper_approval_queue_rules_types
             .Summary_pre_worker_auto_judge_unavailable
           ~operator_detail:reason
         |> durable_pre_worker_unavailable_error reason
       in
       release_auto_judge entry;
       Error reason
     | Some sw ->
       (try
          Eio.Fiber.fork ~sw (fun () ->
            match run_reserved_auto_judge_entry_with ~spawn_worker entry with
            | Ok _ -> ()
            | Error reason ->
              Log.Keeper.error
                ~keeper_name:entry.keeper_name
                "Auto Judge detached worker start failed approval=%s: %s"
                entry.id
                reason);
          Ok Started
        with
        | Eio.Cancel.Cancelled _ as exn ->
          let backtrace = Printexc.get_raw_backtrace () in
          let reason =
            "Auto Judge detached worker launch was cancelled: "
            ^ Printexc.to_string exn
          in
          Eio.Cancel.protect (fun () ->
            ignore
              (mark_pre_worker_unavailable
                 entry
                 ~reason_code:
                   Keeper_approval_queue_rules_types
                   .Summary_pre_worker_auto_judge_unavailable
                 ~operator_detail:reason);
            release_auto_judge entry);
          Printexc.raise_with_backtrace exn backtrace
        | exn ->
          let reason =
            "Auto Judge detached worker launch failed: " ^ Printexc.to_string exn
          in
          let reason =
            mark_pre_worker_unavailable
              entry
              ~reason_code:
                Keeper_approval_queue_rules_types
                .Summary_pre_worker_auto_judge_unavailable
              ~operator_detail:reason
            |> durable_pre_worker_unavailable_error reason
          in
          release_auto_judge entry;
          Error reason))

and spawn_claimed_auto_judge_entry entry =
  spawn_claimed_auto_judge_entry_detached_with
    ~spawn_worker:Hitl_summary_worker.spawn
    entry

and spawn_auto_judge_entry_with
      ~(spawn_worker : hitl_worker_spawner)
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  if claim_auto_judge entry
  then spawn_claimed_auto_judge_entry_with ~spawn_worker entry
  else Ok Skipped

and spawn_auto_judge_entry entry =
  if claim_auto_judge entry
  then
    spawn_claimed_auto_judge_entry_detached_with
      ~spawn_worker:Hitl_summary_worker.spawn
      entry
  else Ok Skipped

and retry_auto_judge_entry
      ~requested_by
      ~expected_input_hash
      ~expected_sequence
      ~expected_exact_attempt
      ~expected_disposition
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  match
    Keeper_approval_queue.reserve_summary_attempt_retry
      ~base_path:entry.audit_base_path
      ~id:entry.id
      ~input_hash:expected_input_hash
      ~sequence:expected_sequence
      ~expected_exact_attempt
      ~expected_disposition
      ~requested_by
  with
  | Error error ->
    Error (Keeper_approval_queue.exact_attempt_error_to_string error)
  | Ok false -> Ok Retry_skipped
  | Ok true ->
    let reblock reason =
      mark_pre_worker_unavailable
        entry
        ~reason_code:
          Keeper_approval_queue_rules_types.Summary_pre_worker_auto_judge_unavailable
        ~operator_detail:reason
      |> durable_pre_worker_unavailable_error reason
    in
    (try
       match
         drain_auto_judge_owner
           ~reserved_id:entry.id
           ~base_path:entry.audit_base_path
           ~keeper_name:entry.keeper_name
           ()
       with
       | Error reason -> Error (reblock reason)
       | Ok (outcome : auto_judge_drain_outcome) ->
         (match
            List.assoc_opt entry.id outcome.failures,
            List.mem entry.id outcome.started_ids,
            outcome.started_ids,
            outcome.blocker
          with
          | Some reason, _, _, _ -> Error (reblock reason)
          | None, true, _, _ -> Ok Retry_started
          | None, false, started_id :: _, _ ->
            Error
              (reblock
                 (Printf.sprintf
                    "Auto Judge retry could not start because earlier approval %s acquired the owner"
                    started_id))
          | None, false, [], Some blocker ->
            Error (reblock (auto_judge_drain_blocker_to_string blocker))
          | None, false, [], None ->
            Error
              (reblock
                 "Auto Judge retry drain completed without a start or blocker"))
     with
     | Eio.Cancel.Cancelled _ as exn ->
       let backtrace = Printexc.get_raw_backtrace () in
       let reason =
         "Auto Judge retry was cancelled before exact attempt binding: "
         ^ Printexc.to_string exn
       in
       let _reblocked_reason =
         Eio.Cancel.protect (fun () -> reblock reason)
       in
       Printexc.raise_with_backtrace exn backtrace
     | exn ->
       let reason =
         "Auto Judge retry failed before exact attempt binding: "
         ^ Printexc.to_string exn
       in
       Error (reblock reason))

and start_auto_judge (entry : Keeper_approval_queue_rules_types.pending_approval) =
  if not (claim_auto_judge entry)
  then Ok Skipped
  else
    match Keeper_approval_queue.mark_summary_pending ~id:entry.id with
    | Error error ->
      release_auto_judge entry;
      Error (Keeper_approval_queue.summary_transition_error_to_string error)
    | Ok false ->
      release_auto_judge entry;
      Ok Skipped
    | Ok true -> spawn_claimed_auto_judge_entry entry

and start_auto_judge_entry (entry : Keeper_approval_queue_rules_types.pending_approval) =
  match
    Keeper_approval_queue.get_pending_entry_for_workspace
      ~base_path:entry.audit_base_path
      ~id:entry.id
  with
  | Error error ->
    Error (Keeper_approval_queue.storage_error_to_string error)
  | Ok None -> Ok Skipped
  | Ok (Some current) ->
    (match classify_auto_judge_entry current with
     | Auto_judge_not_requested -> start_auto_judge current
     | Auto_judge_pending_unbound -> spawn_auto_judge_entry current
     | Auto_judge_finalizable _
     | Auto_judge_ineligible ->
       Ok Skipped)

and drain_auto_judge_owner_queue
      ?reserved_id
      ~base_path
      ~keeper_name
      ()
  =
  let rec loop started_ids failures blocker = function
    | [] ->
      { started_ids = List.rev started_ids
      ; failures = List.rev failures
      ; blocker
      }
    | entry :: rest ->
      let start_result =
        match reserved_id with
        | Some reserved_id
          when auto_judge_entry_has_start_reservation reserved_id entry ->
          spawn_auto_judge_entry entry
        | Some _ | None -> start_auto_judge_entry entry
      in
      (match start_result with
       | Ok Started ->
         loop (entry.id :: started_ids) failures None rest
       | Ok Skipped ->
         let active_ids =
           active_auto_judges_for_owner ~base_path ~keeper_name
         in
         if
           List.length active_ids
           >= Keeper_config.keeper_hitl_max_concurrent_per_keeper ()
         then
            { started_ids = List.rev started_ids
            ; failures = List.rev failures
            ; blocker = Some (Drain_owner_at_capacity active_ids)
            }
         else
           loop
             started_ids
             failures
             (Some (Drain_entry_changed entry.id))
             rest
       | Error reason ->
         Log.Keeper.error
           ~keeper_name
           "Auto Judge owner drain failed approval=%s: %s"
           entry.id
           reason;
         loop
           started_ids
           ((entry.id, reason) :: failures)
           (Some (Drain_start_failed (entry.id, reason)))
           rest)
  in
  Keeper_approval_queue.list_pending_entries_for_workspace ~base_path
  |> Result.map (fun entries ->
    let selected =
      ready_auto_judges_for_owner
        ?reserved_id
        ~base_path
        ~keeper_name
        entries
    in
    (* Selection ranges over every startable entry, so an empty selection under
       a reservation means the reserved entry itself is no longer startable --
       never that an earlier approval holds the FIFO head. *)
    let active_ids = active_auto_judges_for_owner ~base_path ~keeper_name in
    let at_capacity =
      List.length active_ids
      >= Keeper_config.keeper_hitl_max_concurrent_per_keeper ()
    in
    let blocker =
      match reserved_id, selected with
      | _, [] when at_capacity -> Some (Drain_owner_at_capacity active_ids)
      | Some reserved_id, [] ->
        (match
           List.exists
             (fun (entry : Keeper_approval_queue_rules_types.pending_approval) ->
                String.equal entry.id reserved_id)
             entries
         with
         | false -> Some (Drain_entry_missing reserved_id)
         | true -> Some (Drain_entry_changed reserved_id))
      | _ -> None
    in
    loop [] [] blocker selected)

and drain_auto_judge_owner
      ?reserved_id
      ~base_path
      ~keeper_name
      ()
  =
  match Keeper_gate_mode.resolve ~base_path ~keeper_name with
  | Ok Keeper_gate_mode.Auto_judge ->
    drain_auto_judge_owner_queue
      ?reserved_id
      ~base_path
      ~keeper_name
      ()
    |> Result.map_error Keeper_approval_queue.storage_error_to_string
  | Ok Keeper_gate_mode.Manual ->
    Ok
      { started_ids = []
      ; failures = []
      ; blocker = Some Drain_mode_manual
      }
  | Ok Keeper_gate_mode.Always_allow ->
    Ok
      { started_ids = []
      ; failures = []
      ; blocker = Some Drain_mode_always_allow
      }
  | Error detail ->
    Log.Keeper.error
      ~keeper_name
      "Auto Judge owner drain unavailable workspace=%s: %s"
      base_path
      detail;
    Error detail

and drain_auto_judges ~base_path =
  (* The sweep admits owners by their EFFECTIVE mode, not by the workspace
     mode alone. A stricter keeper override can hold one keeper in
     auto_judge while the workspace sits in manual or always_allow — the
     old workspace-mode short-circuit left that keeper's approvals queued
     with nothing sweeping them (the hazard keeper_gate_mode.ml's
     strictness comment predicted for the loosening direction, reached
     from the stricter one). The same filter also stops sweeping an owner
     a manual override holds ABOVE an auto_judge workspace, which the old
     branch judged strictly but drained anyway. The workspace read stays
     first so an unreadable mode store remains a loud error, not an empty
     sweep. *)
  match Keeper_gate_mode.read ~base_path with
  | Error detail ->
    Log.Keeper.error
      "Auto Judge workspace drain unavailable workspace=%s: %s"
      base_path
      detail;
    Error detail
  | Ok (_ : Keeper_gate_mode.t) ->
    (match Keeper_approval_queue.list_pending_entries_for_workspace ~base_path with
     | Error error ->
       Error (Keeper_approval_queue.storage_error_to_string error)
     | Ok entries ->
       let owners =
         List.fold_left
           (fun owners (entry : Keeper_approval_queue_rules_types.pending_approval) ->
              if auto_judge_entry_ready entry
              then Auto_judge_owner_set.add (auto_judge_owner entry) owners
              else owners)
           Auto_judge_owner_set.empty
           entries
       in
       let auto_judge_owners =
         List.filter
           (fun (owner_base_path, keeper_name) ->
              match
                Keeper_gate_mode.resolve ~base_path:owner_base_path ~keeper_name
              with
              | Ok Keeper_gate_mode.Auto_judge -> true
              | Ok (Keeper_gate_mode.Manual | Keeper_gate_mode.Always_allow) ->
                false
              | Error detail ->
                Log.Keeper.error
                  ~keeper_name
                  "Auto Judge drain owner mode unresolved workspace=%s: %s"
                  owner_base_path
                  detail;
                false)
           (Auto_judge_owner_set.elements owners)
       in
       Ok
         (drain_auto_judge_owners_with
            ~drain_owner:(fun ~base_path ~keeper_name ->
              drain_auto_judge_owner_queue ~base_path ~keeper_name ()
              |> Result.map
                   (fun (outcome : auto_judge_drain_outcome) ->
                      outcome.started_ids, outcome.failures)
              |> Result.map_error
                   Keeper_approval_queue.storage_error_to_string)
            auto_judge_owners))
;;

type recovered_work =
  | Activate_worker of Keeper_approval_queue_rules_types.pending_approval
  | Finalize_judgment of
      Keeper_approval_queue_rules_types.pending_approval
      * Keeper_approval_queue_rules_types.hitl_context_summary

(* Reclaim a start reservation orphaned by a hard restart: the graceful settle
   to identity-unbound runs only in memory, so a process death in the
   reserve->bind window strands the durable start-reserved row. Only a row this
   process does not hold a live admission for is reclaimed, so a reservation
   still in flight is never disturbed. On success the in-memory entry is
   advanced to ready so the caller's classification re-activates a worker. *)
let reclaim_orphaned_start_reservation
      (entry : Keeper_approval_queue_rules_types.pending_approval)
  =
  match entry.summary_attempt_disposition with
  | Keeper_approval_queue_rules_types.Summary_attempt_pre_worker_unavailable
      { reason_code =
          Keeper_approval_queue_rules_types.Summary_pre_worker_start_reserved
      ; _
      }
    when not (auto_judge_entry_claimed entry) ->
    (match
       Keeper_approval_queue.release_orphaned_start_reservation
         ~base_path:entry.audit_base_path
         ~id:entry.id
         ~input_hash:entry.input_hash
         ~sequence:entry.sequence
     with
     | Ok true ->
       Log.Keeper.warn
         ~keeper_name:entry.keeper_name
         "reclaimed orphaned Auto Judge start reservation approval=%s at boot \
          recovery"
         entry.id;
       { entry with
         summary_attempt_disposition =
           Keeper_approval_queue_rules_types.Summary_attempt_ready
       }
     | Ok false -> entry
     | Error error ->
       Log.Keeper.error
         ~keeper_name:entry.keeper_name
         "orphaned Auto Judge start reservation reclaim failed approval=%s: %s"
         entry.id
         (Keeper_approval_queue.exact_attempt_error_to_string error);
       entry)
  | _ -> entry
;;

let recovered_work_for_base_path ~base_path =
  (* Boot recovery admits owners by their EFFECTIVE mode, mirroring
     [drain_auto_judges]: a keeper override held in auto_judge above a
     manual/always_allow workspace must be recovered too, or restart —
     the only escape from a stalled owner — recovers nothing for it. *)
  let workspace_readable =
    match Keeper_gate_mode.read ~base_path with
    | Ok (_ : Keeper_gate_mode.t) -> true
    | Error detail ->
      Log.Keeper.error
        "Auto Judge recovery unavailable workspace=%s: %s"
        base_path
        detail;
      false
  in
  if not workspace_readable
  then Ok []
  else
    Keeper_approval_queue.list_pending_entries_for_workspace ~base_path
    |> Result.map (fun entries ->
    let entries = List.map reclaim_orphaned_start_reservation entries in
    let owners =
      List.fold_left
        (fun owners (entry : Keeper_approval_queue_rules_types.pending_approval) ->
           Auto_judge_owner_set.add (auto_judge_owner entry) owners)
        Auto_judge_owner_set.empty
        entries
    in
    let owners =
      Auto_judge_owner_set.filter
        (fun (owner_base_path, keeper_name) ->
           match
             Keeper_gate_mode.resolve ~base_path:owner_base_path ~keeper_name
           with
           | Ok Keeper_gate_mode.Auto_judge -> true
           | Ok (Keeper_gate_mode.Manual | Keeper_gate_mode.Always_allow) ->
             false
           | Error detail ->
             Log.Keeper.error
               ~keeper_name
               "Auto Judge recovery owner mode unresolved workspace=%s: %s"
               owner_base_path
               detail;
             false)
        owners
    in
    let recovered_work_for_entry
          (entry : Keeper_approval_queue_rules_types.pending_approval)
      =
      match classify_auto_judge_entry entry with
      | Auto_judge_not_requested
      | Auto_judge_pending_unbound ->
        Some (Activate_worker entry)
      | Auto_judge_finalizable
          ({ judgment = (Keeper_approval_queue_rules_types.Approve | Keeper_approval_queue_rules_types.Deny); _ }
           as summary) ->
        Some (Finalize_judgment (entry, summary))
      | Auto_judge_finalizable
          { judgment = Keeper_approval_queue_rules_types.Require_human; _ }
      | Auto_judge_ineligible ->
        None
    in
    let entry_of_recovered_work = function
      | Activate_worker entry -> entry
      | Finalize_judgment (entry, _) -> entry
    in
    (* Per owner, admit worker-bound entries up to the available slots, but do
       not cross a finalizable exact-output row. That row is a durability
       barrier: its already-produced judgment must be fsync-confirmed before
       later provider work starts. Require_human and other ineligible rows are
       still passed over rather than becoming FIFO barriers. *)
    owners
    |> Auto_judge_owner_set.elements
    |> List.concat_map (fun (_, keeper_name) ->
      let available =
        Keeper_config.keeper_hitl_max_concurrent_per_keeper ()
        - List.length (active_auto_judges_for_owner ~base_path ~keeper_name)
        |> max 0
      in
      let rec select worker_slots selected = function
        | [] -> List.rev selected
        | entry :: rest ->
          (match recovered_work_for_entry entry with
           | Some (Finalize_judgment _ as work) ->
             if List.is_empty selected then [ work ] else List.rev selected
           | Some (Activate_worker _ as work) when worker_slots > 0 ->
             select (worker_slots - 1) (work :: selected) rest
           | Some (Activate_worker _) -> List.rev selected
           | None -> select worker_slots selected rest)
      in
      owner_auto_judges_in_fifo_order ~base_path ~keeper_name entries
      |> select available [])
    |> List.sort (fun left right ->
      compare_auto_judge_entries
        (entry_of_recovered_work left)
        (entry_of_recovered_work right)))
;;

let observe_recovered_work kind (entry : Keeper_approval_queue_rules_types.pending_approval) =
  let event_type, outcome =
    match kind with
    | `Activate_worker ->
      ( Keeper_approval.Audit.Auto_judge_restart_worker_recovered
      , "restart_worker_recovered" )
    | `Finalize_judgment ->
      ( Keeper_approval.Audit.Auto_judge_restart_judgment_recovered
      , "restart_judgment_recovered" )
  in
  Log.Keeper.warn
    ~keeper_name:entry.keeper_name
    "auto judge durable work recovered kind=%s approval=%s operation=%s"
    outcome
    entry.id
    entry.tool_name;
  Otel_metric_store.inc_counter
    Keeper_metrics.(to_string HitlSummaryOutcomes)
    ~labels:[ "outcome", outcome ]
    ();
  ignore
    (Keeper_approval.Audit.record
       ~base_path:entry.audit_base_path
       ~event_type
       ~id:entry.id
       ~keeper_name:entry.keeper_name
       ~tool_name:entry.tool_name
       ?turn_id:entry.turn_id
       ?task_id:entry.task_id
       ?goal_id:entry.goal_id
       ())
;;

let retry_blocked_auto_judge
      ~base_path
      ~requested_by
      ~expected_input_hash
      ~expected_sequence
      ~expected_exact_attempt
      ~expected_disposition
      approval_id
  =
  (* Same admission rule as [drain_auto_judges] (#31321): the entry's owner
     is judged by its EFFECTIVE mode, not the workspace mode alone. The old
     workspace-mode short-circuit refused this operator escape hatch for a
     keeper pinned to auto_judge under an always_allow or manual workspace —
     the exact stalled-owner configuration the retry exists for. The
     workspace read stays first so an unreadable mode store remains a loud
     error. *)
  match Keeper_gate_mode.read ~base_path with
  | Error detail -> Error detail
  | Ok _workspace_mode ->
    (match
       Keeper_approval_queue.get_pending_entry_for_workspace
         ~base_path
         ~id:approval_id
     with
     | Error error ->
       Error (Keeper_approval_queue.storage_error_to_string error)
     | Ok None -> Error ("pending approval not found: " ^ approval_id)
     | Ok (Some entry) ->
       (match
          let owner_base_path, keeper_name = auto_judge_owner entry in
          Keeper_gate_mode.resolve ~base_path:owner_base_path ~keeper_name
        with
        | Error detail -> Error detail
        | Ok (Keeper_gate_mode.Manual | Keeper_gate_mode.Always_allow) ->
          Error
            (Printf.sprintf
               "Auto Judge retry requires auto_judge mode for keeper %s"
               entry.keeper_name)
        | Ok Keeper_gate_mode.Auto_judge ->
       (match
          retry_auto_judge_entry
            ~requested_by
            ~expected_input_hash
            ~expected_sequence
            ~expected_exact_attempt
            ~expected_disposition
            entry
        with
        | Error reason -> Error reason
        | Ok Retry_skipped ->
          Error
            ("approval summary is not blocked or is already active: "
             ^ approval_id)
        | Ok Retry_started ->
          Log.Keeper.info
            ~keeper_name:entry.keeper_name
            "auto judge operator retry started approval=%s operation=%s actor=%s"
            entry.id
            entry.tool_name
            requested_by;
          Otel_metric_store.inc_counter
            Keeper_metrics.(to_string HitlSummaryOutcomes)
            ~labels:[ "outcome", "operator_retry_started" ]
            ();
       ignore
         (Keeper_approval.Audit.record
            ~base_path:entry.audit_base_path
            ~event_type:Keeper_approval.Audit.Auto_judge_operator_retry_started
            ~id:entry.id
            ~keeper_name:entry.keeper_name
            ~tool_name:entry.tool_name
            ?turn_id:entry.turn_id
            ?task_id:entry.task_id
            ?goal_id:entry.goal_id
            ~actor:requested_by
            ());
       Ok ())))
;;

let finalize_recovered_judgment
      ~complete_summary_exact_attempt
      (entry : Keeper_approval_queue_rules_types.pending_approval)
      summary
  =
  let persistence_uncertain () =
    ignore
      (Keeper_approval_queue.mark_summary_attempt_persistence_uncertain
         ~base_path:entry.audit_base_path
         ~id:entry.id
         ~input_hash:entry.input_hash
         ~sequence:entry.sequence
       : (bool, Keeper_approval_queue.exact_attempt_error) result)
  in
  match entry.exact_attempt with
  | Keeper_approval_queue_rules_types.Exact_unbound ->
    Error
      ( Resume_identity_unbound
      , "Recovered Auto Judge output has no exact attempt identity; finalization is withheld." )
  | Keeper_approval_queue_rules_types.Exact_bound
      ({ status = Keeper_approval_queue_rules_types.Exact_completed; _ } as binding) ->
    (match
       complete_summary_exact_attempt
         ~id:entry.id
         ~input_hash:entry.input_hash
         ~sequence:entry.sequence
         ~slot_id:binding.slot_id
         ~call_id:binding.call_id
         ~plan_fingerprint:binding.plan_fingerprint
         ~request_body_sha256:binding.request_body_sha256
         ~summary
     with
     | Ok
         { Keeper_approval_queue.write_outcome =
             Keeper_approval_queue.Fsync_completed
         ; _
          } ->
       (match resolve_judgment entry ~approval_id:entry.id summary with
        | Ok outcome -> Ok outcome
        | Error operator_detail ->
          Error (Resume_judgment_resolution_failed, operator_detail))
      | Ok
          { write_outcome =
              Keeper_approval_queue.Visible_sync_unconfirmed _detail
          ; _
          } ->
       persistence_uncertain ();
       Error
         ( Resume_completion_persistence_uncertain
         , "Exact completion is visible but durability is not confirmed; finalization is withheld." )
     | Error (Keeper_approval_queue.Exact_attempt_storage_error _error) ->
       persistence_uncertain ();
       Error
         ( Resume_completion_persistence_uncertain
         , "Exact completion durability is not confirmed; finalization is withheld." )
     | Error (Keeper_approval_queue.Exact_attempt_rejected rejection) ->
       let rejection = completion_rejection_of_exact_attempt rejection in
       Error
         ( Resume_completion_rejected rejection
         , completion_rejection_operator_detail rejection ))
  | Keeper_approval_queue_rules_types.Exact_bound _ ->
    Error
      ( Resume_exact_state_not_completed
      , "Recovered Auto Judge entry is not a completed exact judgment." )
;;

let resume_persisted_auto_judges_with
      ~complete_summary_exact_attempt
      ~base_path
  =
  match recovered_work_for_base_path ~base_path with
  | Error queue_error ->
    { requested = 0
    ; started_ids = []
    ; finalized_ids = []
    ; skipped_ids = []
    ; failures = []
    ; queue_error = Some queue_error
    }
  | Ok recovered ->
    let requested = List.length recovered in
    let started_ids, finalized_ids, skipped_ids, failures =
      List.fold_left
        (fun (started_ids, finalized_ids, skipped_ids, failures) work ->
         let entry, result =
           match work with
           | Activate_worker entry ->
             observe_recovered_work `Activate_worker entry;
             entry, `Start (start_auto_judge_entry entry)
             | Finalize_judgment (entry, summary) ->
               observe_recovered_work `Finalize_judgment entry;
               ( entry
               , `Finalize
                   (finalize_recovered_judgment
                      ~complete_summary_exact_attempt
                      entry
                      summary) )
         in
         match result with
         | `Start (Ok Started) ->
           entry.id :: started_ids, finalized_ids, skipped_ids, failures
         | `Finalize (Ok Judgment_finalized) ->
           started_ids, entry.id :: finalized_ids, skipped_ids, failures
         | `Start (Ok Skipped) | `Finalize (Ok Judgment_skipped) ->
           started_ids, finalized_ids, entry.id :: skipped_ids, failures
         | `Start (Error reason) ->
           ( started_ids
           , finalized_ids
           , skipped_ids
           , { approval_id = entry.id
             ; code = Resume_worker_start_failed
             ; operator_detail = reason
             }
             :: failures )
         | `Finalize (Error (code, operator_detail)) ->
           ( started_ids
           , finalized_ids
           , skipped_ids
           , { approval_id = entry.id; code; operator_detail } :: failures ))
        ([], [], [], [])
        recovered
    in
    { requested
    ; started_ids = List.rev started_ids
    ; finalized_ids = List.rev finalized_ids
    ; skipped_ids = List.rev skipped_ids
    ; failures = List.rev failures
    ; queue_error = None
    }
;;

let resume_persisted_auto_judges =
  resume_persisted_auto_judges_with
    ~complete_summary_exact_attempt:
      Keeper_approval_queue.complete_summary_exact_attempt
;;

type operator_recovery_report =
  { started_ids : string list
  ; queued : int
  ; failures : auto_judge_owner_failure list
  }

let request_operator_auto_judge_recovery ~base_path =
  (* [drain_auto_judges] already admits owners by their EFFECTIVE mode
     (#31321), so refusing here on the workspace mode alone re-closed the
     escape hatch for a keeper pinned to auto_judge under an always_allow
     or manual workspace. The workspace read stays as the loud
     unreadable-store check; a workspace where no owner resolves to
     auto_judge simply drains zero entries, and the report says so. *)
  match Keeper_gate_mode.read ~base_path with
  | Error detail -> Error detail
  | Ok _workspace_mode ->
    (match Hitl_summary_worker.snapshot_topology_readiness () with
     | Error detail -> Error detail
     | Ok () ->
       (match drain_auto_judges ~base_path with
       | Error detail -> Error detail
       | Ok drain_report ->
          (match
             Keeper_approval_queue.list_pending_entries_for_workspace ~base_path
           with
           | Error error ->
             Error (Keeper_approval_queue.storage_error_to_string error)
           | Ok entries ->
             let queued =
               List.fold_left
                 (fun count (entry : Keeper_approval_queue_rules_types.pending_approval) ->
                    if auto_judge_entry_ready entry then count + 1 else count)
                 0
                 entries
             in
             Ok
               { started_ids = drain_report.started_ids
               ; queued
               ; failures = drain_report.failures
               })))
;;

let defer ?observation request reason =
  match submit ?observation request with
  | Error error -> Unavailable (Queue_storage_unavailable error)
  | Ok submission ->
    let approval_id = submission.approval_id in
    let audit_receipts =
      match submission.disposition with
      | Keeper_approval_queue.Pending_created receipt -> [ receipt ]
      | Keeper_approval_queue.Pending_deduplicated
      | Keeper_approval_queue.Folded_onto_unconsumed_grant -> []
    in
    let reason =
      match reason with
      | Judge_requested ->
        let drained =
          try
            drain_auto_judge_owner
              ~base_path:request.base_path
              ~keeper_name:request.keeper_name
              ()
          with
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | exn ->
            Error
              ("Auto Judge start failed before worker launch: "
               ^ Printexc.to_string exn)
        in
        (match drained with
         | Error detail -> Auto_judge_unavailable detail
         | Ok (outcome : auto_judge_drain_outcome) ->
           (match List.assoc_opt approval_id outcome.failures with
            | Some detail -> Auto_judge_unavailable detail
            | None -> Judge_requested))
      | Human_requested | Auto_judge_unavailable _ | Mode_state_invalid _ -> reason
    in
    let persist_pre_worker_block ~reason_code detail =
      match
        Keeper_approval_queue.get_pending_entry_for_workspace
          ~base_path:request.base_path
          ~id:approval_id
      with
      | Error error -> Error error
      | Ok None -> Ok ()
      | Ok (Some entry) ->
        (match
           mark_pre_worker_unavailable entry ~reason_code ~operator_detail:detail
         with
         | Ok _ -> Ok ()
         | Error (Keeper_approval_queue.Exact_attempt_storage_error error) ->
           Error error
         | Error (Keeper_approval_queue.Exact_attempt_rejected rejection) ->
           ignore
             (Keeper_approval.Audit.record
                ~base_path:request.base_path
                ~event_type:
                  Keeper_approval.Audit.Auto_judge_block_observation_superseded
                ~id:approval_id
                ~keeper_name:request.keeper_name
                ~tool_name:request.operation
                ());
           Log.Keeper.warn
             ~keeper_name:request.keeper_name
             "Auto Judge pre-worker block observation superseded approval=%s \
              reason_code=%s reason=%s"
             approval_id
             (Keeper_approval_queue_rules_types
              .summary_attempt_pre_worker_unavailable_code_to_string
                reason_code)
             (Keeper_approval_queue.exact_attempt_error_to_string
                (Keeper_approval_queue.Exact_attempt_rejected rejection));
           Ok ())
    in
    (match reason with
     | Mode_state_invalid detail ->
       (match
          persist_pre_worker_block
            ~reason_code:
              Keeper_approval_queue_rules_types.Summary_pre_worker_mode_state_invalid
            detail
        with
        | Ok () -> Deferred { operation = request.operation; approval_id; reason; audit_receipts }
        | Error error -> Unavailable (Queue_storage_unavailable error))
     | Auto_judge_unavailable detail ->
       (match
          persist_pre_worker_block
            ~reason_code:
              Keeper_approval_queue_rules_types.Summary_pre_worker_auto_judge_unavailable
            detail
        with
        | Ok () -> Deferred { operation = request.operation; approval_id; reason; audit_receipts }
        | Error error -> Unavailable (Queue_storage_unavailable error))
     | Human_requested | Judge_requested ->
       Deferred { operation = request.operation; approval_id; reason; audit_receipts })
;;

let observe_exact_rule_store_degraded (request : request) error =
  let detail = Keeper_approval_queue_rules_types.rule_store_error_to_string error in
  Log.Keeper.error
    ~keeper_name:request.keeper_name
    "exact Always Allowed rule lookup unavailable operation=%s: %s; continuing configured Gate mode"
    request.operation
    detail;
  Otel_metric_store.inc_counter
    Keeper_metrics.(to_string ApprovalQueueFailures)
    ~labels:[ "keeper", request.keeper_name; "site", "exact_rule_lookup" ]
    ();
  ignore
    (Keeper_approval.Audit.record
       ~base_path:request.base_path
       ~event_type:Keeper_approval.Audit.Gate_exact_rule_store_degraded
       ~id:(Keeper_approval_queue.generate_id ())
       ~keeper_name:request.keeper_name
       ~tool_name:request.operation
       ?turn_id:(request_turn_id request)
       ?task_id:request.task_id
       ())
;;

let observe_exact_rule_expired
      (request : request)
      (rule_match : Keeper_approval_queue_rules_types.rule_match)
  =
  Log.Keeper.warn
    ~keeper_name:request.keeper_name
    "exact Always Allowed rule %s expired operation=%s; continuing configured Gate mode"
    rule_match.rule_id
    request.operation;
  ignore
    (Keeper_approval.Audit.record
       ~base_path:request.base_path
       ~event_type:Keeper_approval.Audit.Gate_exact_rule_expired
       ~id:(Keeper_approval_queue.generate_id ())
       ~keeper_name:request.keeper_name
       ~tool_name:request.operation
       ?turn_id:(request_turn_id request)
       ?task_id:request.task_id
       ~rule_match
       ())
;;

let status_label = function
  | Unix.WEXITED code -> Printf.sprintf "exit=%d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal=%d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped=%d" signal
;;

(* The box is asked after every cheaper authority has declined. Observe
   failures retain the judge. Guest_local results have already executed on
   the keeper's tree, even when they failed: returning their exact result
   avoids replaying an arbitrary script's completed prefix. *)
let decide_after_observation request ~observe =
  match observe with
  | None -> defer request Judge_requested
  | Some run ->
    (match (run () : observation) with
     | Observed_result execution ->
       let source = Observed_in_box execution in
       let audit_receipt =
         audit_allow
           request
           ~decision_source:Keeper_approval_queue_rules_types.Always_allowed
           source
       in
       allow request source [ audit_receipt ]
     | Observed_refused { status; stderr } ->
       Log.Keeper.info
         ~keeper_name:request.keeper_name
         "observe run refused operation=%s %s stderr_bytes=%d; the judge decides"
         request.operation
         (status_label status)
         (String.length stderr);
       (* The judge is shown what the box refused rather than left to guess
          what the request would have done (RFC-0422 §3.3). *)
       defer ~observation:(observed_refusal ~status ~stderr) request Judge_requested
     | Observation_unavailable reason ->
       Log.Keeper.info
         ~keeper_name:request.keeper_name
         "observe run unavailable operation=%s reason=%s; the judge decides"
         request.operation
         reason;
       defer request Judge_requested)
;;

let decide_from_selected_mode ?observe request = function
  | Error detail -> defer request (Mode_state_invalid detail)
  | Ok Keeper_gate_mode.Manual -> defer request Human_requested
  | Ok Keeper_gate_mode.Auto_judge ->
    (* Observation-only requests have a deterministic safety answer —
       read-only argv dispatched into a disposable guest (the typed
       [sandbox_profile] on the request decides that), server-side
       [web_search] reads, and
       [web_fetch] GETs whose literal destination the fetch checks itself
       (it does not resolve DNS names);
       paying the judge (or the human queue) for them is how a bare `ls`
       became an approval prompt and a keeper stopped searching. Manual
       mode is untouched: an operator who asked to see everything still
       sees everything. *)
    if
      Keeper_gate_readonly.observation_only_request
        ~operation:request.operation
        ~sandbox_profile:request.sandbox_profile
        ~input:request.input
    then (
      let source = Readonly_sandbox in
      let audit_receipt =
        audit_allow
          request
          ~decision_source:Keeper_approval_queue_rules_types.Always_allowed
          source
      in
      allow request source [ audit_receipt ])
    else decide_after_observation request ~observe
  | Ok Keeper_gate_mode.Always_allow ->
    let source = Workspace_always_allow in
    let audit_receipt =
      audit_allow
        request
        ~decision_source:Keeper_approval_queue_rules_types.Always_allowed
        source
    in
    allow request source [ audit_receipt ]
;;

let decide_without_cycle_grant ~read_mode ?observe ~keeper_always_allow request =
  if keeper_always_allow
  then (
    let source = Keeper_always_allow in
    let audit_receipt =
      audit_allow
        request
        ~decision_source:Keeper_approval_queue_rules_types.Always_allowed
        source
    in
    allow request source [ audit_receipt ])
  else
    (* Both lanes resolve through the Keeper override (#31128): an operator
       who singled a Keeper out for a higher bar meant it for everything
       that Keeper does, outside services included. *)
    let mode =
      read_mode ~base_path:request.base_path
        ~keeper_name:request.keeper_name
    in
    (match mode with
     | Ok Keeper_gate_mode.Always_allow ->
       let source = Workspace_always_allow in
       let audit_receipt =
         audit_allow
           request
           ~decision_source:Keeper_approval_queue_rules_types.Always_allowed
           source
       in
       allow request source [ audit_receipt ]
     | Error _ | Ok (Keeper_gate_mode.Manual | Keeper_gate_mode.Auto_judge) ->
       (match
          Keeper_approval_queue_rules.find_matching_rule
            ~base_path:request.base_path
            ~keeper_name:request.keeper_name
            ~tool_name:request.operation
            ~input:request.input
            ()
        with
        | Error error ->
          observe_exact_rule_store_degraded request error;
          decide_from_selected_mode ?observe request mode
        | Ok (Keeper_approval_queue_rules_types.Rule_match_active rule_match) ->
          let source = Exact_always_rule rule_match.rule_id in
          let audit_receipt =
            audit_allow
              request
              ~rule_match
              ~decision_source:Keeper_approval_queue_rules_types.Always_allowed
              source
          in
          allow request source [ audit_receipt ]
        | Ok (Keeper_approval_queue_rules_types.Rule_match_expired rule_match) ->
          observe_exact_rule_expired request rule_match;
          decide_from_selected_mode ?observe request mode
        | Ok Keeper_approval_queue_rules_types.Rule_match_absent ->
          decide_from_selected_mode ?observe request mode))
;;

let decide_with_mode ~read_mode ?cycle_grant ?observe ~keeper_always_allow request =
  let grant_result =
    match cycle_grant with
    | None -> Cycle_grant_not_applicable
    | Some grant -> take_matching_cycle_grant grant request
  in
  match grant_result with
  | Cycle_grant_authorized (approval_id, grant_audit_receipt) ->
    let source = One_shot_resolution approval_id in
    let gate_audit_receipt =
      audit_allow request ~source_approval_id:approval_id source
    in
    allow request source [ grant_audit_receipt; gate_audit_receipt ]
  | Cycle_grant_not_applicable ->
    decide_without_cycle_grant ~read_mode ?observe ~keeper_always_allow request
  | Cycle_grant_temporarily_unavailable (approval_id, reason) ->
    Log.Keeper.warn
      ~keeper_name:request.keeper_name
      "one-shot Gate grant unavailable; preserving the unconsumed grant operation=%s reason=%s"
      request.operation
      (unavailable_reason_to_string reason);
    ignore
      (Keeper_approval.Audit.record
         ~base_path:request.base_path
         ~event_type:Keeper_approval.Audit.Gate_grant_unavailable
         ~id:approval_id
         ~keeper_name:request.keeper_name
         ~tool_name:request.operation
         ?turn_id:(request_turn_id request)
         ?task_id:request.task_id
         ~source_approval_id:approval_id
         ());
    Otel_metric_store.inc_counter
      Keeper_metrics.(to_string ApprovalQueueFailures)
      ~labels:[ "keeper", request.keeper_name; "site", "cycle_grant_lookup" ]
      ();
    Unavailable reason
;;

let decide ?cycle_grant ?observe ~keeper_always_allow request =
  decide_with_mode
    ~read_mode:Keeper_gate_mode.resolve
    ?cycle_grant
    ?observe
    ~keeper_always_allow
    request
;;

let decide_external_service ?cycle_grant ~keeper_always_allow request =
  decide_with_mode
    ~read_mode:Keeper_gate_mode.resolve_external
    ?cycle_grant
    ~keeper_always_allow
    request
;;

module For_testing = struct
  type exact_completion =
    id:string ->
    input_hash:string ->
    sequence:int ->
    slot_id:string ->
    call_id:string ->
    plan_fingerprint:string ->
    request_body_sha256:string ->
    summary:Keeper_approval_queue_rules_types.hitl_context_summary ->
    ( Keeper_approval_queue.exact_attempt_transition
    , Keeper_approval_queue.exact_attempt_error )
      result

  let auto_judge_entry_ready = auto_judge_entry_ready

  let ready_auto_judges_for_owner ~base_path ~keeper_name entries =
    ready_auto_judges_for_owner ~base_path ~keeper_name entries
  ;;

  let claim_auto_judge = claim_auto_judge
  let release_auto_judge = release_auto_judge
  let active_auto_judges_for_owner = active_auto_judges_for_owner

  type owner_drain_outcome =
    { started_ids : string list
    ; failures : (string * string) list
    }

  let drain_auto_judge_owners_with ~drain_owner owners =
    drain_auto_judge_owners_with
      ~drain_owner:(fun ~base_path ~keeper_name ->
        drain_owner ~base_path ~keeper_name
        |> Result.map (fun (outcome : owner_drain_outcome) ->
          outcome.started_ids, outcome.failures))
      owners
  ;;

  type nonrec hitl_worker_spawner = hitl_worker_spawner

  let spawn_auto_judge_entry_with_worker ~spawn_worker entry =
    match spawn_auto_judge_entry_with ~spawn_worker entry with
    | Ok Started -> Ok true
    | Ok Skipped -> Ok false
    | Error reason -> Error reason
  ;;

  let spawn_auto_judge_entry_with_detached_worker ~spawn_worker entry =
    if claim_auto_judge entry
    then
      match
        spawn_claimed_auto_judge_entry_detached_with ~spawn_worker entry
      with
      | Ok Started -> Ok true
      | Ok Skipped -> Ok false
      | Error reason -> Error reason
    else Ok false
  ;;

  let resume_persisted_auto_judges_with_exact_completion =
    resume_persisted_auto_judges_with
  ;;
end
