(** Keeper single-turn orchestration via Agent_core.Agent.run().

    This module is intentionally a compatibility facade: public types and
    entrypoints stay here while prompt metrics, result/error helpers, and
    tool-surface policy live in focused implementation modules. *)

include module type of Keeper_agent_prompt_metrics
include module type of Keeper_agent_tool_surface
include module type of Keeper_agent_result
include module type of Keeper_agent_error

module Contract_helpers = Keeper_agent_run_contract_helpers
module Turn_helpers = Keeper_agent_run_turn_helpers

(** Outcome of building the per-turn AGENT_CORE raw-trace sink
    ([.masc/keepers/<name>/raw-traces/turn-*.jsonl]). [Sink_degraded] is
    the typed health record for trace-store failures: the turn still
    dispatches (untraced, so [run_result.trace_ref]/[run_validation] stay
    [None] for that turn) — trace-store state never fails a turn
    pre-dispatch. *)
type raw_trace_sink_outcome =
  | Sink_ready of Agent_core.Raw_trace.t
  | Sink_degraded of Agent_core.Error.t

(** Typed reason for an autonomous Keeper run to release its lane after AGENT_CORE
    completes a tool boundary. *)
type durable_stimulus_summary = {
  pending_count : int;
  head : Keeper_event_queue.stimulus option;
  head_age_sec : float;
  kinds : Keeper_event_queue.stimulus_payload list;
}
(** What the durable queue held at the instant the turn yielded. The yield
    condition is [pending <> empty], so a yield record without this says only
    that a yield happened, not what it yielded to — which cannot distinguish
    healthy cooperation from a keeper that never finishes a turn. *)

type autonomous_yield_reason =
  | Operation_queued
  | Durable_stimulus_waiting of durable_stimulus_summary

type autonomous_yield_request = {
  reason : autonomous_yield_reason;
}

val durable_stimulus_summary
  :  now:float
  -> Keeper_event_queue.t
  -> durable_stimulus_summary
(** Project the queue snapshot the yield decision already read. [now] is
    supplied by the caller so the age is measured against the same clock the
    decision used. *)

val durable_stimulus_summary_to_string : durable_stimulus_summary -> string

val terminal_effect_boundary_decision
  :  Keeper_tools_agent_core.terminal_effect_state
  -> (Runtime_agent.cooperative_yield_decision, Agent_core.Error.t) result
(** Production boundary projection for Keeper tool results. Generic deferred
    tool transitions retain the normal durable-stimulus checkpoint; deferred
    external effects yield the current provider loop so their durable
    resolution can wake a later turn. Failed terminal effects retain their typed
    [Tool_result.tool_failure_class] in the exact structured Keeper error
    envelope. *)

module For_testing : sig
  val registry_progress_on_event
    :  record_turn_progress:(string -> unit)
    -> (Agent_core.Types.sse_event -> unit) option
    -> Agent_core.Types.sse_event
    -> unit
  val progress_keeper_tool_names_for_contract
    :  actual_keeper_tool_names:string list
    -> tool_calls:tool_call_detail list
    -> string list

  val normalize_response_text_for_finalization
    :  runtime_id:string
    -> initial_messages:Agent_core.Types.message list
    -> run_result:Runtime_agent.run_result
    -> text:string
    -> tool_names:string list
    -> unit
    -> (string, Agent_core.Error.t) result

  (** AGENT_CORE raw-trace sink for keeper turns: a fresh per-turn file under
      [Keeper_types_support.keeper_raw_trace_dir]. The dispatch section passes
      it into [Keeper_turn_driver.run_named] so
      [run_result.trace_ref]/[run_validation] are populated. *)
  val keeper_raw_trace_sink
    :  config:Workspace.config
    -> meta:Keeper_meta_contract.keeper_meta
    -> raw_trace_sink_outcome

  (** Dispatch adapter over {!keeper_raw_trace_sink}: [Sink_degraded]
      becomes [None] (turn runs untraced) after emitting the typed
      degrade record (warn log + [Keeper_metrics.RawTraceSinkDegraded]
      counter). Never raises; never fails the turn. *)
  val raw_trace_for_dispatch
    :  config:Workspace.config
    -> meta:Keeper_meta_contract.keeper_meta
    -> Agent_core.Raw_trace.t option

  (** Run reference-aware cleanup only after the current TurnRecord commit
      attempt. A missing/degraded sink is a no-op; cleanup failure is logged
      and never changes the turn result. *)
  val prune_raw_traces_after_turn_record
    :  config:Workspace.config
    -> meta:Keeper_meta_contract.keeper_meta
    -> Agent_core.Raw_trace.t option
    -> unit

  val runtime_yield_reason
    :  autonomous_yield_request
    -> Runtime_agent.cooperative_yield_reason

  val repeated_exact_tool_call
    :  threshold:int
    -> tool_call_detail list
    -> (string * int) option

  (** Newest-first tool calls; [Some (tool, streak)] when the last [threshold]+
      calls share a tool name and input fingerprint, whatever their outputs did.

      The axis {!repeated_exact_tool_call} cannot answer for a tool whose
      result is a clock: its output moves on every call while nothing advances,
      so the output fingerprint it uses as no-progress proof never matches.
      This one drops the output and requires the repeats to be adjacent
      instead. *)
  val repeated_tool_call_input
    :  threshold:int
    -> tool_call_detail list
    -> (string * int) option

  (** Newest-first per-turn assistant texts; [Some streak] when the last
      [threshold]+ consecutive turns carry the same non-blank text. *)
  val repeated_assistant_text
    :  threshold:int
    -> string list
    -> int option

  val dispatch_after_provider_transcript_admission
    :  messages:Agent_core.Types.message list
    -> checkpoint:Agent_core.Checkpoint.t option
    -> dispatch:
         (checkpoint:Agent_core.Checkpoint.t option -> Agent_core.Types.message list -> ('a, Agent_core.Error.t) result)
    -> ('a, Agent_core.Error.t) result

  (** Exact-run reference recorded on the turn record. Accepts a reference
      whose session identity matches the keeper trace; the AGENT_CORE runtime
      identity carried by the reference is recorded, not compared, because it
      names a different identity space than the keeper agent name. *)
  val turn_record_raw_trace_run_ref
    :  expected_session_id:string
    -> Agent_core.Raw_trace.run_ref
    -> (Turn_record.raw_trace_run_ref, string) result

  val raw_trace_reference_for_turn
    :  turn_trace_ref:Agent_core.Raw_trace.run_ref option
    -> sink:Agent_core.Raw_trace.t option
    -> Agent_core.Raw_trace.run_ref option
  (** Which run this turn's TurnRecord names for retention. [turn_trace_ref] is
      what the turn result reported and is absent on every failed turn; the
      sink's last finished run covers that case, since a keeper sink holds one
      turn. *)

end

(** {1 Turn execution} *)

(** Run a single keeper turn.

    @param config Workspace configuration
    @param meta Keeper metadata
    @param base_dir Session base directory for checkpoints
    @param max_context Maximum context window tokens
     @param build_turn_prompt Callback: receives the base keeper system prompt
            and checkpoint message history, returns the final turn system prompt
    @param user_message The user's message to the keeper
    @param turn_kind Producer-owned lane identity for the durable turn record
    @param user_blocks Optional structured user-authored AGENT_CORE content blocks for
           the current turn. [user_message] remains the display/history
           fallback and must not contain raw media payloads.
    @param runtime_id Typed runtime profile name for model selection
     @param world_observation Structured keeper world snapshot used by
            advisory execution-progress checks. When omitted, the progress check
            does not infer world state from prompt text.
    @param history_user_source Source label for user messages in history
    @param history_assistant_source Source label for assistant messages in history
    @param temperature Subsystem temperature fallback; a selected runtime model
           declaration takes precedence
    @param on_event Optional event callback
    @param trajectory_acc Optional trajectory accumulator for recording
    @param is_retry When [true], replays current user message without persisting
    @param shared_context Optional shared AGENT_CORE context for cross-turn state
    @param event_bus Optional MASC event bus *)
val run_turn
  :  config:Workspace.config
  -> meta:Keeper_meta_contract.keeper_meta
  -> publication_recovery:
       Keeper_publication_recovery_availability.turn_context
  -> profile_defaults:Keeper_types_profile.keeper_profile_defaults
  -> turn_ctx_cell:Keeper_tool_call_log.turn_ctx_cell
  -> base_dir:string
  -> max_context:int
  -> build_turn_prompt:
       (base_system_prompt:string -> messages:Agent_core.Types.message list -> turn_prompt)
  -> user_message:string
  -> turn_kind:Turn_record.turn_kind
  -> skill_snapshot:Skill_catalog_snapshot.t
  -> task_skill_selection:
       (Keeper_task_skill_turn.t, Keeper_task_skill_turn.error) result
  -> ?user_blocks:Agent_core.Types.content_block list
  -> runtime_id:string
  -> ?world_observation:Keeper_world_observation.world_observation
  -> ?history_user_source:string
  -> ?user_turn_record:Keeper_run_prompt.user_turn_record
  -> ?history_assistant_source:string
  -> ?temperature:float
  -> ?on_event:(Agent_core.Types.sse_event -> unit)
  -> ?on_tool_stream_observation:
       (Keeper_hooks_agent_core.tool_stream_observation -> unit)
  -> ?on_tool_result_ready:(tool_call_id:string -> turn:int -> planned_index:int -> execution_id:Ids.Execution_id.t -> unit)
  -> ?approval_gate:Keeper_tool_approval_gate.t
  -> ?trajectory_acc:Trajectory.accumulator
  -> ?degraded_retry_applied:bool
  -> ?degraded_retry_runtime:string
  -> ?fallback_reason:Keeper_error_classify.degraded_retry_reason
  -> ?runtime_rotation_attempts:Keeper_execution_receipt.runtime_rotation_attempt list
  -> ?deferred_runtime_lane:Keeper_turn_driver.deferred_runtime_lane
  -> ?on_runtime_retry_deferred:
       (Keeper_turn_driver.deferred_runtime_lane -> unit)
  -> ?on_deferred_runtime_consumed:(unit -> unit)
  -> ?is_retry:bool
  -> ?shared_context:Agent_core.Context.t
  -> ?event_bus:Agent_core.Event_bus.t
  -> ?trace_link:string * string
  -> ?continuation_channel:Keeper_continuation_channel.t
  -> ?hitl_resolution:Keeper_event_queue.hitl_resolution
  -> ?autonomous_yield_requested:
       (unit -> (autonomous_yield_request option, string) result)
       (* Evaluated only after a typed AGENT_CORE tool boundary. Snapshot failures
          remain explicit errors. The chat lane never receives this hook. *)
  -> ?on_checkpoint_stage:(Agent_core.Agent.checkpoint_stage -> unit)
  -> unit
  -> (run_result, Agent_core.Error.t) result

val capture_skill_snapshot : base_path:string -> Skill_catalog_snapshot.t
(** Capture one immutable Skill revision outside provider retry/failover loops.
    Every attempt of the same logical turn must receive the same value. *)
