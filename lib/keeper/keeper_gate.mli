(** Non-hierarchical authorization boundary for Keeper external effects.

    The Gate receives an already-normalized, opaque operation identity and its
    complete concrete input. It never parses a command, tool name, provider,
    connector, or product-specific payload. *)

type causal_context =
  { turn_id : int option
  ; snapshot : Yojson.Safe.t
  }
(** Exact outer-turn evidence captured before the tool call. The Gate stores
    and forwards [snapshot] without interpreting its fields. *)

type request =
  { keeper_name : string
  ; operation : string
  ; input : Yojson.Safe.t
  ; call_summary : string option
      (** The one line this call is about, stated by the producer from its
          typed input through that tool's declared summary function (a shell
          tool its command line, a file tool its path, a network tool its
          URL). The Gate does not read it: it travels to the chat row the
          queue writes when the call is deferred, so the operator sees what
          was parked and not only which tool asked. [None] when the tool
          declares no summary or its statement is blank; a summary is never
          derived here from the [input] fields. *)
  ; base_path : string
  ; causal_context : causal_context option
  ; task_id : string option
  ; continuation_channel : Keeper_continuation_channel.t option
  ; sandbox_profile : Keeper_types_profile_sandbox.sandbox_profile option
      (** The typed sandbox a [tool_execute] will dispatch into, taken from
          the dispatch bundle at the call site — the route authority for the
          observation-only classification ({!Keeper_gate_readonly}). [None]
          for every non-execute operation. The sandbox labels inside [input]
          are display/audit data; no decision reads them. *)
  }

(** Gate operation vocabulary — the strings the approval store keys on and
    the host replay engine dispatches on. The Gate owns them so the
    deferred payload's promise cannot drift from what replay actually
    spends; producers borrow the literal from here instead of declaring
    their own. *)
val filesystem_write_gate_operation : string

val tool_execute_gate_operation : string

val network_read_gate_operation : string

val connector_post_gate_operation : string

val identity_call_gate_operation : string

val voice_speak_gate_operation : string

type replayable =
  | Replay_write
  | Replay_execute
  | Replay_network_read
  | Replay_connector_post
  | Replay_identity
  | Replay_voice_speak

val replayable_operation : string -> replayable option
(** Whether an approved operation is executed by the host replay engine
    ({!Keeper_gate_replay}). The deferred payload's [on_approve] wording
    follows this: a "host replays this exact call" promise made over an
    unrecognized operation starves the approved effect silently. *)

type boxed_execution =
  { run : Keeper_types_profile_sandbox.observation_run
  ; result : Masc_exec.Exec_dispatch.dispatch_result
  }
(** A request that already ran, including the exact result the caller must
    return. [Observe] reaches this state only on exit 0. [Guest_local] reaches
    it for every process status: guest writes may already have landed. *)

type authorization_source =
  | One_shot_resolution of string
  | Exact_always_rule of string
  | Keeper_always_allow
  | Workspace_always_allow
  | Readonly_sandbox
      (** The request is a closed-set observation-only argv inside a
          per-keeper disposable guest — docker container or microvm
          ({!Keeper_gate_readonly}); allowed without judgment. *)
  | Observed_in_box of boxed_execution
      (** The request ran once inside the executor's box. Under
          [Observe] the guest kernel refused every file write outside a
          scratch and every socket (Landlock and seccomp, RFC-0422), so
          nothing it did could have left an effect; under [Guest_local] the
          writes it made landed inside the guest, on the keeper's own tree,
          and the operator chose to let those pass without judgment
          (RFC-0422 §3.4). Either way its output is returned and no judge is
          asked; the audit row names which box it was. A [Guest_local] failure
          is returned as a failed process result too, without whole-call
          replay: an arbitrary script may already have changed the tree. *)

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

(** Every reason above says the approval machinery could not act, not that
    the call was wrong or that the effect half-happened. Callers report
    {!Unavailable} as [Tool_result.Dependency_unavailable] with
    [Proven_pre_effect]. Four of the five call sites reported
    [Runtime_failure] until 2026-09-06, which reaches a model as
    [Agent_core.Types.Unknown] -- an uncertain outcome, next to a message
    saying the effect was not executed. *)

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

val auto_judge_resume_failure_code_to_string :
  auto_judge_resume_failure_code -> string

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

(** Mutable only to serialize consumption inside one Keeper cycle. The durable
    Gate journal, not the wake event, owns the exact authorization. A match
    requires the same workspace, Keeper, opaque operation identity, and
    canonical complete input; provenance fields never become constraints. *)
type cycle_grant

val cycle_grant_of_resolution :
  Keeper_event_queue.hitl_resolution -> cycle_grant option

(** What one run of the request inside the executor's box came back as
    (RFC-0422). The caller that owns the sandbox runs it; the Gate only
    decides when to ask, and what each answer means. *)
type observation =
  | Observed_result of boxed_execution
      (** [Observe] exit 0, or any [Guest_local] process result. The result
          travels with the decision so returning it never needs a second
          dispatch, including when guest-local writes preceded a failure. *)
  | Observed_refused of
      { status : Unix.process_status
      ; stderr : string
      }
      (** An [Observe] run ended otherwise: a refused write or socket, a
          program that exited non-zero, a signal. Not an effect either, but
          not an answer — the request keeps the judge, and this is what the
          judge will be shown (RFC-0422 step 3b). *)
  | Observation_unavailable of string
      (** No box could be built for this request — a profile with no shim, a
          shim that advertises no box, a dispatch the typed gate refused — so
          the request keeps the judge exactly as before this stage existed.
          Never read as clean. *)

val observed_refusal :
  status:Unix.process_status ->
  stderr:string ->
  Keeper_approval_queue_rules_types.observed_refusal
(** What a refused observe run becomes on the approval row and in the
    keeper's deferred receipt: the status, and the stderr tail bounded by
    [keeper.hitl.observation_stderr_bytes]. The Gate applies it when it
    defers on {!Observed_refused}; the tool_execute runtime applies the same
    function so the keeper reads the same bytes the judge does. *)

(** Evaluate one exact external-effect request. [keeper_always_allow] is the
    explicit Keeper profile switch; it carries no inferred semantics. Manual,
    Auto Judge, and invalid-mode outcomes enqueue durably and return without
    suspending the caller. Explicit Keeper/workspace Always Allow modes do not
    depend on the optional exact-rule store being readable. A supplied one-shot
    grant that cannot be consumed returns [Unavailable] without evaluating a
    second authorization path, so the durable grant remains single-use.

    [observe] is the executor's box (RFC-0422): a [tool_execute] caller that
    can run the request boxed passes it, every other caller omits it. The
    Gate calls it at exactly one point — Auto Judge mode, after the one-shot
    grant, both Always Allow switches, the exact rules and the observation
    tables have all declined — and only there, so an always-allowed keeper
    never pays a box run and a Manual workspace still sees every request.
    [Observed_result] is returned through source {!Observed_in_box}; the other two
    answers defer to the judge as the request would have without the box. *)
val decide :
  ?cycle_grant:cycle_grant ->
  ?observe:(unit -> observation) ->
  keeper_always_allow:bool ->
  request ->
  decision

(** {!decide} for an effect that leaves the workspace for an attached outside
    service. Identical evaluation — one-shot grants, the Keeper profile
    switch, and exact Always Allow rules all apply — except the mode comes
    from {!Keeper_gate_mode.read_external} instead of the workspace lane, so
    opening the workspace ([Always_allow]) does not open outside writes. A
    separate entry point rather than an optional lane argument: the last hole
    in this area was an optional gate parameter that compiled when omitted. *)
val decide_external_service :
  ?cycle_grant:cycle_grant ->
  keeper_always_allow:bool ->
  request ->
  decision

(** Recover durable Auto Judge work for exactly one workspace. Each exact
    [(base_path, keeper_name)] owner activates up to its configured bounded
    concurrency, in durable sequence order. An oldest entry that is failed,
    quarantined, released, uncertain, judged [Require_human], or otherwise
    ineligible carries no such work and is passed over instead of held as a
    FIFO barrier. None of those states leaves itself, so treating them as a
    barrier starved every later same-owner entry for as long as the owner
    stayed in one: observed on 2026-07-28 holding 25 approvals across two
    Keepers, the oldest for 2416s, while every drive path kept firing. Entries
    that do carry work keep durable sequence order among themselves.
    Completion refills only that owner's available slots. Decisive output without an exact
    attempt identity is retained pending and recorded as a recovery failure.
    Completed exact output is first idempotently strict-rewritten with the same
    identity and summary; only [Keeper_approval_queue.Fsync_completed] permits
    Gate finalization. Visible unconfirmed or failed rewrites leave the approval
    pending and record a recovery failure. Dispatch-uncertain, released,
    released-recovery-required, restart-quarantined, and quarantined entries
    never enter automatic restart recovery. Failed judgments are never retried
    merely because a process restarted. Every recovery candidate id has an
    explicit started, finalized, skipped, or failed outcome. *)
val resume_persisted_auto_judges :
  base_path:string -> auto_judge_resume_report

val retry_blocked_auto_judge :
  base_path:string ->
  requested_by:string ->
  expected_input_hash:string ->
  expected_sequence:int ->
  expected_exact_attempt:Keeper_approval_queue_rules_types.exact_attempt_state ->
  expected_disposition:Keeper_approval_queue_rules_types.summary_attempt_disposition ->
  string ->
  (unit, string) result
(** Explicitly rearm one typed blocked Auto Judge summary. The configured Gate
    mode, authenticated workspace, non-blank operator identity, and exact
    approval row identity must all match. No cadence or restart hook calls it. *)

type auto_judge_owner_failure =
  { keeper_name : string
  ; approval_id : string option
  ; operator_detail : string
  }

type auto_judge_workspace_drain_report =
  { started_ids : string list
  ; failures : auto_judge_owner_failure list
  }

type operator_recovery_report =
  { started_ids : string list
  ; queued : int
  ; failures : auto_judge_owner_failure list
  }

(** After an explicit operator selection of Auto Judge, fill the bounded worker
    slots for each Keeper owner with eligible current-schema work in the workspace.
    Exact-bound entries remain operator-visible but are never reconstructed or
    reopened. *)
val request_operator_auto_judge_recovery :
  base_path:string -> (operator_recovery_report, string) result

val authorization_source_to_string : authorization_source -> string
val unavailable_reason_to_string : unavailable_reason -> string
val decision_to_yojson : decision -> Yojson.Safe.t

(** One-way tool-result projection for an already committed authorization.
    Existing producer metadata is preserved under [producer]; consumers must
    keep using the typed decision as the authorization authority. *)
val authorization_metadata
  :  ?producer_metadata:Yojson.Safe.t
  -> authorization
  -> Yojson.Safe.t

module For_testing : sig
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

  val auto_judge_entry_ready :
    Keeper_approval_queue_rules_types.pending_approval -> bool

  val ready_auto_judges_for_owner :
    base_path:string ->
    keeper_name:string ->
    Keeper_approval_queue_rules_types.pending_approval list ->
    Keeper_approval_queue_rules_types.pending_approval list

  val claim_auto_judge : Keeper_approval_queue_rules_types.pending_approval -> bool
  val release_auto_judge : Keeper_approval_queue_rules_types.pending_approval -> unit

  val active_auto_judges_for_owner
    :  base_path:string
    -> keeper_name:string
    -> string list

  type owner_drain_outcome =
    { started_ids : string list
    ; failures : (string * string) list
    }

  val drain_auto_judge_owners_with
    :  drain_owner:
         (base_path:string ->
          keeper_name:string ->
          (owner_drain_outcome, string) result)
    -> (string * string) list
    -> auto_judge_workspace_drain_report
  (** Drain every supplied workspace/Keeper owner even when an earlier owner
      fails, preserving both successful starts and typed owner-local failures. *)

  type hitl_worker_spawner =
    sw:Eio.Switch.t ->
    entry:Keeper_approval_queue_rules_types.pending_approval ->
    on_summary:(Keeper_approval_queue_rules_types.hitl_context_summary -> unit) ->
    on_finish:(Hitl_summary_worker.finish_outcome -> unit) ->
    unit ->
    (Hitl_summary_worker.spawn_outcome, string) result

  val spawn_auto_judge_entry_with_worker
    :  spawn_worker:hitl_worker_spawner
    -> Keeper_approval_queue_rules_types.pending_approval
    -> (bool, string) result
  (** Run the production atomic claim, active-owner lifecycle, cleanup, and
      conclusive-only drain with only the worker spawner injected. *)

  val spawn_auto_judge_entry_with_detached_worker
    :  spawn_worker:hitl_worker_spawner
    -> Keeper_approval_queue_rules_types.pending_approval
    -> (bool, string) result
  (** Run the production server-root launcher with only the worker spawner
      injected. The worker-start path survives the submitting Keeper turn. *)

  val resume_persisted_auto_judges_with_exact_completion :
    complete_summary_exact_attempt:exact_completion ->
    base_path:string ->
    auto_judge_resume_report
end
