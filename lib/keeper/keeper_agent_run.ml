(** Keeper_agent_run — Run a single keeper turn via Agent_core.Agent.run().

    This module is intentionally a compatibility facade: public types and
    entrypoints stay here while prompt metrics, result/error helpers, and
    tool-surface policy live in focused implementation modules. *)

include Keeper_agent_prompt_metrics
include Keeper_agent_tool_surface
include Keeper_agent_result
include Keeper_agent_error
module Contract_helpers = Keeper_agent_run_contract_helpers
module Turn_helpers = Keeper_agent_run_turn_helpers

let progress_keeper_tool_names_for_contract =
  Contract_helpers.progress_keeper_tool_names_for_contract
;;


let normalize_response_text_for_finalization
      ~runtime_id
      ~initial_messages:_
      ~(run_result : Runtime_agent.run_result)
      ~text
      ~tool_names
      ()
  =
  match run_result.stop_reason with
  | Runtime_agent.Yielded_to_operation_queued _
  | Runtime_agent.Yielded_to_durable_stimulus _
  | Runtime_agent.Yielded_after_repeated_tool_call _
  | Runtime_agent.Yielded_after_repeated_assistant_text _
  | Runtime_agent.Completed
  | Runtime_agent.InputRequired _ ->
  if
    Keeper_agent_run_response_text.stop_reason_suppresses_visible_response
      run_result.stop_reason
  then Ok ""
  else
    match Keeper_tooling.Response.normalize_response_text ~text ~tool_names () with
  | Ok response_text -> Ok response_text
  | Error _ ->
    (* Finalization exposes the typed accept-rejected response itself. Tool
       execution history stays in the AGENT_CORE checkpoint; it is not projected into
       a read/mutating behavioral classification. *)
    Error
      (Keeper_turn_driver_try_provider.accept_rejected_error
         ~runtime_id
         ~response:run_result.response)
;;

(* AGENT_CORE raw-trace sink for keeper turns: parsed Run_started / Assistant_block /
   Tool_execution / Run_finished records written to a fresh per-turn JSONL
   under [Keeper_types_support.keeper_raw_trace_dir]. Passing the sink into
   [Keeper_turn_driver.run_named] is what populates
   [run_result.trace_ref]/[run_validation] for unified observation consumers.

   Failure isolation: the trace store is observability state and must never
   gate keeper liveness. A fresh file per turn keeps [Raw_trace.create]
   (AGENT_CORE [create -> scan_next_seq -> read_all]) from parsing any previous
   turn's data, so a corrupt or oversized historical trace cannot wedge
   dispatch — and if sink creation still fails, the turn dispatches
   untraced with the typed [Sink_degraded] record emitted as a warn log
   plus the [Keeper_metrics.RawTraceSinkDegraded] counter. *)
type raw_trace_sink_outcome =
  | Sink_ready of Agent_core.Raw_trace.t
  | Sink_degraded of Agent_core.Error.t

(* The attribution inputs masc owns, kept apart from the wire measurement it
   does not always own. [body_bytes] can only be read on the Agent Core lane,
   where masc's own serializer produced the bytes; the three official-client
   lanes assemble the wire inside the client. Welding the two together is what
   made every antigravity, claude_code and codex turn report an input
   attribution of zero: one missing measurement erased four outputs that had
   nothing to do with it. The file already made this split once for
   [model_input_window_ref], for the same reason. *)
type request_attribution =
  { runtime_profile : string
    (** The lane attempt that assembled this request. A turn that failed over
        settles on a different runtime, and the record keeps both so a reader
        compares instead of assuming. *)
  ; prompt_blocks : Turn_record.prompt_block list
  ; provider_content :
      ( Agent_core.Types.message list
      , Keeper_agent_prompt_metrics.attribution_gap )
      result
    (** The transmitted messages with the typed prompt-context carrier
        removed, or the typed reason there are none to report. A [result]
        rather than an [option]: "the projection never ran", "it ran and its
        output could not be classified", and "this lane resumed a session it
        does not hold" are different facts and the record says which. *)
  ; tools : Agent_core.Tool.t list
    (** The surface this request carried. On the Agent Core lane this comes
        from {!Keeper_agent_tool_surface.on_the_wire} rather than the list the
        turn was built with, because that lane sends a listing in place of the
        attached-service schemas and widens its own set mid-turn. On an
        official-client lane it is the list handed to the client, which the
        Claude lane narrows to [[]] when the target declares no tool
        support. *)
  }

type request_wire_evidence =
  { wire_observation : Turn_record.request_wire_observation
  ; serialized_observation :
      Llm_provider.Request_wire_observer.observation option
  ; wire_projected_messages : Agent_core.Types.message list option
  ; wire_tools : Agent_core.Tool.t list
  }

(* What the queue held when the turn decided to yield. The decision itself is
   [pending <> empty], so without this the record says a yield happened and
   nothing about what it yielded to. *)
type durable_stimulus_summary =
  { pending_count : int
  ; head : Keeper_event_queue.stimulus option
  ; head_age_sec : float
  ; kinds : Keeper_event_queue.stimulus_payload list
  }

type autonomous_yield_reason =
  | Operation_queued
  | Durable_stimulus_waiting of durable_stimulus_summary

type autonomous_yield_request =
  { reason : autonomous_yield_reason }

let durable_stimulus_summary ~now (pending : Keeper_event_queue.t) =
  let stimuli = Keeper_event_queue.to_list pending in
  let kinds =
    List.map (fun (s : Keeper_event_queue.stimulus) -> s.payload) stimuli
  in
  match stimuli with
  | [] -> { pending_count = 0; head = None; head_age_sec = 0.; kinds = [] }
  | head :: _ ->
    { pending_count = List.length stimuli
    ; head = Some head
    ; (* A stimulus stamped by a different clock can sit ahead of [now]. A
         negative age reads as a stimulus from the future rather than as the
         clock skew it is. *)
      head_age_sec = Float.max 0. (now -. head.arrived_at)
    ; kinds
    }
;;

(* Rendering happens here, at the boundary where the value leaves the type
   system for a log line. The summary itself holds the typed payloads. *)
let durable_stimulus_summary_to_string summary =
  let label (payload : Keeper_event_queue.stimulus_payload) =
    Keeper_event_queue.payload_kind_label payload
  in
  Printf.sprintf
    "pending=%d head=%s head_age_sec=%.1f kinds=[%s]"
    summary.pending_count
    (match summary.head with
     | None -> "none"
     | Some head -> label head.payload)
    summary.head_age_sec
    (summary.kinds
     |> List.map label
     |> List.sort_uniq String.compare
     |> String.concat ",")
;;

let runtime_yield_reason request =
  match request.reason with
  | Operation_queued -> Runtime_agent.Operation_queued
  | Durable_stimulus_waiting _ ->
    Runtime_agent.Durable_stimulus_waiting
;;

(* Constitution exception (named bound + rationale): loop detection is
   inherently a repetition count, so no closed variant can replace the
   number — what counts as "the same call" is already typed (tool name +
   input/output fingerprints in [same_exact_tool_call]). The yield is a
   cooperative turn boundary, not a keeper lifecycle gate: the keeper loop
   stays active and decides the next cycle from the yielded outcome. 3 =
   the first call plus two identical repeats: a single repeat (count 2) can
   still be legitimate (an idempotent poll or a deliberate re-read while the
   model waits for state to change); a second repeat with an unchanged input
   AND output fingerprint means the world did not change and the model made
   no progress — a deterministic loop. The repeats need not be adjacent: a
   provider alternating between two stalled calls is the same loop. *)
let repeated_tool_call_yield_threshold = 3

(* Constitution exception (named bound + rationale): same shape as
   [repeated_tool_call_yield_threshold] — loop detection is inherently a
   repetition count, and what counts as "the same text" is already typed
   (byte equality on the turn's accumulated [Text] blocks, no normalization).
   3 = the first emission plus two identical repeats: a single repeat can be a
   legitimate restatement after new tool evidence, but a second byte-identical
   repeat means the model is narrating the same plan without acting on it.
   Unlike the tool axis, the repeats must be the trailing consecutive turns:
   the text axis has no output fingerprint proving "the world did not change",
   so a non-adjacent recurrence (a deliberate re-summary several turns later)
   is not loop evidence. Blank text never counts; it only breaks a streak. *)
let repeated_assistant_text_yield_threshold = 3

let same_present_fingerprint left right =
  match left, right with
  | Some left, Some right -> String.equal left right
  | None, None
  | Some _, None
  | None, Some _ ->
    false
;;

let same_exact_tool_call
      (left : Keeper_agent_result.tool_call_detail)
      (right : Keeper_agent_result.tool_call_detail)
  =
  String.equal left.tool_name right.tool_name
  && same_present_fingerprint left.input_fingerprint right.input_fingerprint
  && same_present_fingerprint left.output_fingerprint right.output_fingerprint
;;

let repeated_exact_tool_call ~threshold tool_calls =
  match tool_calls with
  | [] -> None
  | latest :: previous ->
    (* Every earlier identical call counts, not only the ones immediately
       before [latest]. The old fold stopped at the first different call, so it
       measured a run rather than a total, and a provider alternating between
       two stalled calls never reached the threshold: one keeper ran
       [git status --short --branch] 48 times with identical input in one
       dispatch, always with a Read or a git diff in between, and the count
       never left 1. That dispatch made 279 tool calls over 31 minutes and
       produced no answer.

       The identity test is unchanged -- input and output must both match, so
       this still fires only on a call whose result did not move. The contract
       in the .mli says "repeated exact tool input and output"; adjacency was
       never part of it. Measured over 815 recorded dispatches: catches that
       loop on call 73 instead of never, and reaches 93 of the 116 dispatches
       that ended without an answer. The 47 answered dispatches it also stops
       lose nothing -- a repeat yield persists a checkpoint and resumes. *)
    let repeated_count =
      List.fold_left
        (fun count call -> if same_exact_tool_call latest call then count + 1 else count)
        1
        previous
    in
    if threshold > 1 && repeated_count >= threshold
    then Some (latest.tool_name, repeated_count)
    else None
;;

(* Constitution exception (named bound + rationale): the same repetition count
   as the two axes above, on the one key they cannot use.

   [same_exact_tool_call] requires the output fingerprint to match, and that is
   deliberate -- an unchanged output is the proof that the world did not move.
   A tool whose result is a clock has no such proof available: every call
   returns a different value while nothing advances, so no threshold on that
   axis can ever fire. Live, one keeper called [keeper_time_now] 280 times in a
   single turn out of 281 tool calls, writing the same sentence between them
   with only the freshly read timestamp changed, and neither the tool axis nor
   the text axis saw it (masc #33021).

   So this axis drops the output and keeps adjacency instead. Adjacency is what
   the output fingerprint was buying: consecutive identical calls with nothing
   in between is the same "nothing happened" evidence, without asking the
   result to stand still.

   5 = the first call plus four consecutive repeats. Measured over three days
   of live tool calls (2026-09-01..03), adjacent runs of an identical
   (tool, input) pair separate cleanly at that point: runs of 3-4 are ordinary
   work (keeper_tasks_list 101, keeper_spawn_read 40, masc_board_post_get 60,
   Execute 33), while 35 of the 36 runs reaching 5 were one tool -- the clock --
   under the two keepers that were looping. A yield here persists a checkpoint
   and resumes, so the one non-clock run of that length cost a resume. *)
let repeated_tool_call_input_yield_threshold = 5

let same_tool_call_input
      (left : Keeper_agent_result.tool_call_detail)
      (right : Keeper_agent_result.tool_call_detail)
  =
  String.equal left.tool_name right.tool_name
  && same_present_fingerprint left.input_fingerprint right.input_fingerprint
;;

(* Newest-first, so the head is the latest call and the streak runs back from
   it. Unlike [repeated_exact_tool_call] the repeats must be adjacent: without
   the output fingerprint, two identical inputs far apart in a dispatch are
   ordinary re-reads, not a loop. *)
let repeated_tool_call_input ~threshold tool_calls =
  match tool_calls with
  | [] -> None
  | latest :: previous ->
    let rec streak count = function
      | call :: rest when same_tool_call_input latest call -> streak (count + 1) rest
      | _ -> count
    in
    let repeated_count = streak 1 previous in
    if threshold > 1 && repeated_count >= threshold
    then Some (latest.tool_name, repeated_count)
    else None
;;

let assistant_text_is_blank text =
  String.for_all
    (fun ch -> ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r')
    text
;;

(* [assistant_turn_texts] is newest-first with one entry per completed
   provider turn ("" for a turn without a [Text] block), so a streak from the
   head is exactly "the last N consecutive turns". Comparison is
   [String.equal] on the accumulated text — byte equality, no trimming or
   other normalization; blankness only gates entry, it never rewrites the
   compared text. *)
let repeated_assistant_text ~threshold assistant_turn_texts =
  match assistant_turn_texts with
  | [] -> None
  | latest :: previous ->
    if assistant_text_is_blank latest
    then None
    else (
      let rec streak count = function
        | text :: rest when String.equal text latest -> streak (count + 1) rest
        | _ :: _ | [] -> count
      in
      let repeated_count = streak 1 previous in
      if threshold > 1 && repeated_count >= threshold
      then Some repeated_count
      else None)
;;

let keeper_raw_trace_sink
      ~(config : Workspace.config)
      ~(meta : Keeper_meta_contract.keeper_meta)
  : raw_trace_sink_outcome
  =
  (* Path derivation ensures [.masc/keepers/<name>/raw-traces/]; any
     filesystem refusal (unwritable parent, blocked path) must land in
     [Sink_degraded], not escape into the turn. *)
  let path_result =
    try Ok (Keeper_types_support.keeper_raw_trace_turn_path config meta.name)
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> Error (Agent_core.Error.Internal (Printexc.to_string exn))
  in
  match path_result with
  | Error err -> Sink_degraded err
  | Ok path ->
    (match
       Agent_core.Raw_trace.create
         ~session_id:(Keeper_id.Trace_id.to_string meta.runtime.trace_id)
         ~path
         ()
     with
     | Ok sink -> Sink_ready sink
     | Error err -> Sink_degraded err)
;;

(* Dispatch adapter: a degraded sink means the turn runs untraced
   ([trace_ref]/[run_validation] stay [None] for that turn), never that
   the turn fails pre-dispatch. The degrade is typed and observable:
   warn log + [RawTraceSinkDegraded] counter labelled by keeper. *)
let raw_trace_for_dispatch
      ~(config : Workspace.config)
      ~(meta : Keeper_meta_contract.keeper_meta)
  : Agent_core.Raw_trace.t option
  =
  match keeper_raw_trace_sink ~config ~meta with
  | Sink_ready sink -> Some sink
  | Sink_degraded err ->
    Otel_metric_store.inc_counter
      Keeper_metrics.(to_string RawTraceSinkDegraded)
      ~labels:[ "keeper", meta.name ]
      ();
    Log.Keeper.warn ~keeper_name:meta.name
      "raw-trace sink degraded; dispatching turn untraced: %s"
      (Agent_core.Error.to_string err);
    None
;;

let prune_raw_traces_after_turn_record
      ~(config : Workspace.config)
      ~(meta : Keeper_meta_contract.keeper_meta)
      (raw_trace : Agent_core.Raw_trace.t option)
  =
  match raw_trace with
  | None -> ()
  | Some _ ->
    (match Keeper_raw_trace_retention.prune ~config ~keeper_name:meta.name () with
     | Error error ->
       Otel_metric_store.inc_counter
         Keeper_metrics.(to_string RawTraceRetentionSkipped)
         ~labels:[ "keeper", meta.name ]
         ();
       Log.Keeper.warn ~keeper_name:meta.name
         "raw-trace retention skipped after TurnRecord commit without gating the turn: %s"
         (Keeper_raw_trace_retention.error_to_string error)
     | Ok { removed; deletion_failures; _ } ->
       if removed > 0
       then
         Otel_metric_store.inc_counter
           Keeper_metrics.(to_string RawTraceRetentionDeleted)
           ~labels:[ "keeper", meta.name ]
           ~delta:(float_of_int removed)
           ();
       (match deletion_failures with
        | [] -> ()
        | first :: rest ->
          let failure_count = 1 + List.length rest in
          Otel_metric_store.inc_counter
            Keeper_metrics.(to_string RawTraceRetentionUnlinkFailed)
            ~labels:[ "keeper", meta.name ]
            ~delta:(float_of_int failure_count)
            ();
          Log.Keeper.warn ~keeper_name:meta.name
            "raw-trace retention completed with %d unlink failure(s); first=%s: %s"
            failure_count
            first.path
            first.detail))
;;

(* An open tool cycle at dispatch is not corruption. Checkpoint persistence
   stores one on purpose between [After_assistant_collected] and
   [After_tool_results_appended] so recovery knows which calls were in flight,
   and any turn that dies in that window leaves one behind — a provider timeout
   does it without the process ever dying, which boot recovery's "process
   death" premise did not cover. Measured 2026-08-22..25: one timeout latched
   one lane for 33 consecutive turns and two other lanes likewise, 555 turns
   on 2026-08-23 alone, each lane failing every turn until a restart.

   So the routine case is closed here with the same synthesized results boot
   recovery appends, and the turn proceeds on the closed history. A structural
   break still latches: a history that does not parse is genuine corruption and
   no result can repair it. *)
let provider_transcript_admission messages =
  let reject transcript_error =
    let reason, tool_use_ids =
      match transcript_error with
      | Keeper_transcript_unit.Invalid_transcript_structure _ ->
        Keeper_internal_error.Structurally_invalid, []
      | Keeper_transcript_unit.Unresolved_tool_results { tool_use_ids } ->
        Keeper_internal_error.Unresolved_tool_results, tool_use_ids
    in
    Error
      (Keeper_internal_error.core_error_of_masc_internal_error
         (Keeper_internal_error.Incomplete_tool_transcript
            { reason
            ; detail =
                Keeper_transcript_unit.show_provider_transcript_error transcript_error
            ; tool_use_ids
            }))
  in
  let close_with ~close ~transcript_error =
    match close messages with
    | Ok { Keeper_transcript_unit.messages; closed_tool_use_ids } ->
      Log.Keeper.info
        "closed %d in-flight tool call(s) an interrupted turn left open: %s"
        (List.length closed_tool_use_ids)
        (String.concat "," closed_tool_use_ids);
      Ok messages
    | Error _ ->
      (* Both closers pass a break they cannot answer through unchanged, and a
         history that does not parse is corruption no synthesized result can
         repair. *)
      reject transcript_error
  in
  match Keeper_transcript_unit.validate_provider_transcript messages with
  | Ok () -> Ok messages
  | Error
      (Keeper_transcript_unit.Invalid_transcript_structure
         (Keeper_transcript_unit.Overlapping_tool_cycle _) as transcript_error) ->
    (* The one structural error a result can answer.
       [Overlapping_tool_cycle] does not mean the history is unreadable; it
       means a request never got its results and a later request exposed it.
       That is the same missing result the tail case synthesizes, sitting
       mid-history because turns kept appending after the interruption.
       Rejecting it latched the keeper at a fixed [message_index] on every
       turn forever (#31595): one keeper went an hour without completing a
       turn on 2026-09-01, another logged 585 of these in a day. *)
    close_with ~close:Keeper_transcript_unit.close_open_cycles ~transcript_error
  | Error (Keeper_transcript_unit.Invalid_transcript_structure _ as transcript_error) ->
    (* Every other member stays fatal: an orphaned result has no request to
       attach to, and a request in a non-assistant role is not a missing
       result. *)
    reject transcript_error
  | Error (Keeper_transcript_unit.Unresolved_tool_results _ as transcript_error) ->
    close_with ~close:Keeper_transcript_unit.close_open_tail ~transcript_error
;;

(* [dispatch] receives the admitted history, which is the input list unless an
   interrupted turn's open cycle had to be closed first. *)
let dispatch_after_provider_transcript_admission ~messages ~checkpoint ~dispatch =
  match provider_transcript_admission messages with
  | Error _ as error -> error
  | Ok admitted ->
    (* Runtime_agent resumes from checkpoint.messages. Repairing only the
       initial history leaves that resume path replaying the unanswered calls
       that admission just closed. Preserve every other checkpoint field. *)
    let checkpoint =
      Option.map
        (fun (checkpoint : Agent_core.Checkpoint.t) ->
          { checkpoint with messages = admitted })
        checkpoint
    in
    dispatch ~checkpoint admitted
;;

(* [run_ref.agent_name] is the AGENT_CORE runtime identity, not the Keeper identity.
   The writer binds the reference to this turn's session; the autonomous-turn
   reader validates the recorded AGENT_CORE identity and session against every raw
   trace row before projecting the run. *)
let turn_record_raw_trace_run_ref
      ~expected_session_id
      (run_ref : Agent_core.Raw_trace.run_ref)
  : (Turn_record.raw_trace_run_ref, string) result
  =
  match run_ref.session_id with
  | None -> Error "missing session identity"
  | Some session_id when not (String.equal session_id expected_session_id) ->
    Error "session identity does not match the keeper trace"
  | Some session_id ->
    Ok
      { worker_run_id = run_ref.worker_run_id
      ; path = run_ref.path
      ; start_seq = run_ref.start_seq
      ; end_seq = run_ref.end_seq
      ; agent_name = run_ref.agent_name
      ; session_id
      }
;;

(* Retention (RFC-0358) deletes every trace no TurnRecord names, so this
   reference is what keeps a turn's trace on disk. [run_result] carries one
   only when the turn succeeded: a failed turn still writes and closes its run
   — [finish_raw_error] calls [Raw_trace.finish_run] — but the failure travels
   up as [Error] with no result to read the reference from, so the record
   named nothing and the next prune removed the one trace a failure
   investigation needs.

   The sink answers the same question for either outcome. A keeper sink is one
   file per turn, so [Raw_trace.last_run] can only be the run this turn just
   finished. It stays [None] when the turn failed before [start_run]; that
   file holds no run and deleting it is correct. The turn's own reference
   still wins when present, which leaves the succeeding path unchanged. *)
let raw_trace_reference_for_turn ~turn_trace_ref ~sink =
  match turn_trace_ref with
  | Some _ as reference -> reference
  | None -> Option.bind sink Agent_core.Raw_trace.last_run
;;

let terminal_effect_boundary_decision = Keeper_tool_terminal_boundary.decision

module For_testing = struct
  let registry_progress_on_event = Turn_helpers.registry_progress_on_event
  let progress_keeper_tool_names_for_contract =
    Contract_helpers.progress_keeper_tool_names_for_contract
  let normalize_response_text_for_finalization =
    normalize_response_text_for_finalization
  let keeper_raw_trace_sink = keeper_raw_trace_sink
  let raw_trace_for_dispatch = raw_trace_for_dispatch
  let prune_raw_traces_after_turn_record = prune_raw_traces_after_turn_record
  let runtime_yield_reason = runtime_yield_reason
  let repeated_exact_tool_call = repeated_exact_tool_call
  let repeated_tool_call_input = repeated_tool_call_input
  let repeated_assistant_text = repeated_assistant_text
  let dispatch_after_provider_transcript_admission =
    dispatch_after_provider_transcript_admission
  let turn_record_raw_trace_run_ref = turn_record_raw_trace_run_ref
  let raw_trace_reference_for_turn = raw_trace_reference_for_turn
end

let capture_skill_snapshot ~base_path =
  match Skill_catalog_snapshot_service.find_workspace_of_base_path ~base_path with
  | Error error ->
    Skill_catalog_snapshot.config_unreadable
      ~detail:
        (Config_dir_resolver.canonical_base_path_error_to_string error)
  | Ok None ->
    Skill_catalog_snapshot.config_unreadable
      ~detail:"Skill snapshot workspace is not initialized"
  | Ok (Some workspace) ->
    (match Skill_catalog_snapshot_service.current ~workspace with
     | Some snapshot -> snapshot
     | None ->
       Skill_catalog_snapshot.config_unreadable
         ~detail:"Skill snapshot workspace has no published revision")
;;

(** Run a single keeper turn via Agent_core.Agent.run().

    Loads checkpoint, creates working context with the base keeper system
    prompt, then calls [build_turn_prompt] with the base prompt and message
    history so the caller can layer skill routing, continuity context,
    policy guards, and turn-specific instructions on top.

    After the callback returns the final system prompt, appends the user
    message, builds AGENT_CORE tools + hooks, and delegates to
    [Keeper_turn_driver.run_named] which internally calls Agent.run().

    @param config Workspace configuration
    @param meta Keeper metadata
    @param base_dir Session base directory for checkpoints
    @param max_context Maximum context window tokens
     @param build_turn_prompt Callback: receives the base keeper system prompt
            and checkpoint message history, returns the final turn system prompt
     @param user_message The user's message to the keeper
    @param runtime_id Runtime profile name for model selection
    @param temperature Subsystem temperature fallback; a selected runtime model
           declaration takes precedence. When omitted,
           [Keeper_config.keeper_unified_temperature] is the fallback.
    @param is_retry When [true], replays the current user message into the
           working context without persisting it again, so transient retry
           attempts do not duplicate the user entry in session history *)
let run_turn
      ~(config : Workspace.config)
      ~(meta : Keeper_meta_contract.keeper_meta)
      ~(publication_recovery :
          Keeper_publication_recovery_availability.turn_context)
      ~(profile_defaults : Keeper_types_profile.keeper_profile_defaults)
      ~(turn_ctx_cell : Keeper_tool_call_log.turn_ctx_cell)
      ~(base_dir : string)
      ~(max_context : int)
      ~(build_turn_prompt :
         base_system_prompt:string -> messages:Agent_core.Types.message list -> turn_prompt)
      ~(user_message : string)
      ~(turn_kind : Turn_record.turn_kind)
      ~(skill_snapshot : Skill_catalog_snapshot.t)
      ~(task_skill_selection :
          (Keeper_task_skill_turn.t, Keeper_task_skill_turn.error) result)
      ?user_blocks
      ~(runtime_id : string)
      ?world_observation
      ?(history_user_source = "direct_user")
      ?(user_turn_record = Keeper_run_prompt.Record_user_turn)
      ?(history_assistant_source = "direct_assistant")
      ?temperature
      ?on_event
      ?on_tool_stream_observation
      ?on_tool_result_ready
      ?approval_gate
      ?(trajectory_acc : Trajectory.accumulator option)
      ?(degraded_retry_applied = false)
      ?degraded_retry_runtime
      ?fallback_reason
      ?(runtime_rotation_attempts = [])
      ?deferred_runtime_lane
      ?on_runtime_retry_deferred
      ?on_deferred_runtime_consumed
      ?(is_retry = false)
      ?shared_context
      ?event_bus
      ?trace_link
      ?continuation_channel
      ?hitl_resolution
      ?autonomous_yield_requested
      ?on_checkpoint_stage
      ()
  : (run_result, Agent_core.Error.t) result
  =
  (* Section 1: Setup — sanitize input, build context, compose prompt. *)
  let deferred_runtime_lane_ref = ref None in
  let record_runtime_retry_deferred hint =
    deferred_runtime_lane_ref := Some hint;
    Option.iter (fun callback -> callback hint) on_runtime_retry_deferred
  in
  let user_message = Keeper_run_prompt.sanitize_user_message user_message in
  Masc_runtime_events.emit_turn_start ();
  (* Cancel-safe cleanup (#9747): [Eio_guard.protect] already uses
     [Eio.Switch.on_release], so cleanup runs under cooperative cancellation
     and does not mask the outer [Eio.Cancel.Cancelled]. The turn's durable
     record is the keeper turn-records store (RFC-0378 §5.2); the
     observation bus carries no turn events. *)
  let safe_emit_turn_end () = Turn_helpers.emit_turn_end_safely ~keeper_name:meta.name () in
  Eio_guard.protect ~finally:safe_emit_turn_end
  @@ fun () ->
  try
  (* RFC-0107 §3.3 Phase C.1 wiring — turn-scoped Eio.Switch.
     Resources opened during a turn (HTTP connections, sandbox exec
     handles, retry sub-tasks via [Keeper_turn_driver_try_provider])
     that read [Eio_context.get_switch_opt ()] now attach to [turn_sw],
     not the server root_sw. When this [Eio.Switch.run] closes (turn
     end, success or cancellation), those resources are released —
     bounding per-turn FD growth.

     Server/dashboard fibers that read [get_switch_opt] from *outside*
     this binding are unaffected (audit §10.2): they have no
     [Eio.Fiber] binding for [sw_key], so [get_switch_opt] falls
     through to the global atomic = server root_sw.

     The [with_turn_switch] binding propagates with [Eio.Fiber.fork]
     children, so runtime attempts and tool invocations spawned inside
     the turn body all see [turn_sw] automatically. *)
  Eio.Switch.run @@ fun turn_sw ->
  Keeper_registry.set_turn_switch ~base_path:config.base_path meta.name (Some turn_sw);
  Eio.Switch.on_release turn_sw (fun () ->
    Keeper_registry.clear_turn_switch ~base_path:config.base_path meta.name);
  Eio_context.with_turn_switch turn_sw
  @@ fun () ->
  (* The spawn registry is bound for the same span as the turn switch, and on
     the same fiber, so a handle means something exactly as long as the process
     it names can still be running. A registry that outlived the turn would
     keep answering for processes the switch already ended, and would need a
     retention bound to stop its table growing -- a cap with nothing to say.
     [runtime_id] is the turn's own identity, so a handle issued here cannot
     collide with one from any other turn. *)
  let spawn_registry =
    Spawn_registry.create
      ~run:runtime_id
      ~output_limit_bytes:Env_config_keeper.KeeperSpawn.spawn_output_buffer_bytes
  in
  Spawn_turn_registry.with_turn_registry spawn_registry
  @@ fun () ->
  (* The language servers a code query starts belong to this turn, for the
     reason lsp_turn_pool.mli measures: starting one costs 11-60 ms, keeping
     one costs 12-155 MB, and every keeper has its own sandbox root. Binding
     here also shuts them down when the turn ends. *)
  Lsp_turn_pool.with_turn_pool ~servers:(Runtime.lsp_servers ())
  @@ fun () ->
  let runtime_id_string = runtime_id in
  (* Steps 0–4: inference params, session dir, checkpoint, base prompt,
     working context, checkpoint hygiene — all in Keeper_run_context. *)
  let ctx =
    Keeper_run_context.prepare_run_context
      ~config
      ~meta
      ~profile_defaults
      ~base_dir
      ~runtime_id
      ?temperature
      ?shared_context
      ()
  in
  let meta = ctx.meta in
  let temperature = ctx.temperature in
  let context_injector = ctx.context_injector in
  let shared_context = ctx.shared_context in
  let session = ctx.session in
  let base_system_prompt = ctx.base_system_prompt in
  let resume_agent_core_checkpoint = ctx.resume_agent_core_checkpoint in
  let start_turn_count = ctx.start_turn_count in
  let receipt_started_at = ctx.receipt_started_at in
  let config_root = ctx.config_root in
  let runtime_config_path = ctx.runtime_config_path in
  let trace_id = Keeper_id.Trace_id.to_string meta.runtime.trace_id in
  let manifest_keeper_turn_id = meta.runtime.usage.total_turns + 1 in
  let turn_start = Mtime_clock.now () in
  let seq_ref = Atomic.make 0 in
  let runtime_manifest_context =
    Turn_helpers.runtime_manifest_context
      ~keeper_name:meta.name
      ~trace_id
      ~keeper_turn_id:manifest_keeper_turn_id
  in
  let checkpoint_path =
    Keeper_checkpoint_store.agent_core_checkpoint_path ~session_dir:session.session_dir
      ~session_id:trace_id
  in
  let append_manifest =
    Turn_helpers.make_append_manifest
      ~config
      ~keeper_name:meta.name
      ~trace_id
      ~runtime_id:runtime_id_string
      ~turn_start
      ~seq_ref
  in
  let digest_text = Keeper_context_digest.text in
  let digest_message_texts_as_joined =
    Keeper_context_digest.message_texts_as_joined
  in
  append_manifest ~site:"checkpoint_loaded"
    ~keeper_turn_id:manifest_keeper_turn_id
    ~checkpoint_path
    ~decision:
      (Keeper_runtime_manifest.with_payload_role ~payload_role:Checkpoint
        (`Assoc
          [ "loaded_checkpoint_present", `Bool ctx.loaded_checkpoint_present ]))
    Keeper_runtime_manifest.Checkpoint_loaded;
  (* Steps 5-6: turn prompt, memory/temporal context, prompt metrics,
     and user message append — Keeper_run_prompt. *)
  let prompt_user_turn_record =
    match hitl_resolution with
    | Some _ -> Keeper_run_prompt.Skip_already_checkpointed_user_turn
    | None -> user_turn_record
  in
  let prompt_ctx =
    Keeper_run_prompt.build_turn_context
      ~ctx
      ~build_turn_prompt
      ~user_message
      ~config
      ~meta
      ~history_user_source
      ~user_turn_record:prompt_user_turn_record
      ~is_retry
      ~start_turn_count
  in
  let turn_system_prompt = prompt_ctx.Keeper_run_prompt.turn_system_prompt in
  let dynamic_context = prompt_ctx.Keeper_run_prompt.dynamic_context in
  let temporal_context = prompt_ctx.Keeper_run_prompt.temporal_context in
  let history_messages = prompt_ctx.Keeper_run_prompt.history_messages in
  let resume_agent_core_checkpoint =
    Option.map
      (fun (checkpoint : Agent_core.Checkpoint.t) ->
        { checkpoint with messages = history_messages })
      resume_agent_core_checkpoint
  in
  let ctx_work = prompt_ctx.Keeper_run_prompt.ctx_work in
  (* 7. Set up agent — delegated to Keeper_run_tools *)
  let composition_plan_index =
    Option.map
      (fun (gate : Keeper_tool_approval_gate.t) -> gate.composition_plan_index)
      approval_gate
  in
  let setup =
    Keeper_run_tools.prepare_agent_setup
      ~config
      ~meta
      ~profile_defaults
      ~publication_recovery
      ?continuation_channel
      ?on_tool_stream_observation
      ?on_tool_result_ready
      ?hitl_resolution
      ?composition_plan_index
      ~turn_ctx_cell
      ~ctx_work
      ~session
      ~base_system_prompt
      ~turn_system_prompt
      ~user_message
      ~dynamic_context
      ~history_messages
      ~shared_context
      ~context_injector
      ~start_turn_count
      ~keeper_turn_id:manifest_keeper_turn_id
      ~turn_kind
      ~runtime_id
      ~is_retry
      ~config_root
      ~runtime_config_path
      ~skill_snapshot
      ~skill_names:profile_defaults.skill_names
      ~task_skill_selection
      ~trajectory_acc
      ~runtime_manifest_context
      ~runtime_manifest_append:
        (fun manifest ->
           Keeper_runtime_manifest.append_best_effort
             ~site:"context_injection_hook"
             config
             manifest)
      ()
  in
  (* Section 2: prepare runtime tools and hooks. *)
  match setup with
  | Error e -> Error e
  | Ok s ->
    let user_message = s.Keeper_run_tools.user_message in
    let user_blocks =
      match user_blocks, s.Keeper_run_tools.gate_replay_evidence with
      | Some blocks, Some evidence ->
        Some (Keeper_gate_replay.append_model_evidence_block evidence blocks)
      | (Some _ as blocks), None -> blocks
      | None, _ -> None
    in
    let ctx_work =
      match hitl_resolution with
      | None -> ctx_work
      | Some _ ->
        let user_message = Agent_core.Types.user_msg user_message in
        Keeper_context_runtime.append ctx_work user_message
    in
    let prompt_metrics =
      Keeper_agent_prompt_metrics.build_prompt_metrics
        ~system_prompt:turn_system_prompt
        ~dynamic_context
        ~user_message
    in
    let history_messages_digest =
      digest_message_texts_as_joined history_messages
    in
    let context_digest =
      digest_text
        (base_system_prompt ^ turn_system_prompt ^ dynamic_context
         ^ temporal_context ^ user_message
         ^ history_messages_digest)
    in
    append_manifest
      ~site:"context_injected"
      ~keeper_turn_id:manifest_keeper_turn_id
      ?checkpoint_path:
        (if ctx.loaded_checkpoint_present then Some checkpoint_path else None)
      ~decision:
        (Keeper_runtime_manifest.with_payload_role
           ~payload_role:Model_input
           (`Assoc
              [ ( "base_system_prompt_digest"
                , `String (digest_text base_system_prompt) )
              ; ( "turn_system_prompt_digest"
                , `String (digest_text turn_system_prompt) )
              ; ( "dynamic_context_digest"
                , `String (digest_text dynamic_context) )
              ; ( "temporal_context_digest"
                , `String (digest_text temporal_context) )
              ; "history_message_count", `Int (List.length history_messages)
              ; "history_messages_digest", `String history_messages_digest
              ; "context_window", `Int max_context
              ; "context_digest", `String context_digest
              ; ( "skill_snapshot_revision"
                , `String
                    (Skill_catalog_snapshot.snapshot_revision skill_snapshot
                     |> Skill_catalog_snapshot.snapshot_revision_to_string) )
              ; ( "skill_catalog_revision"
                , `String
                    (Skill_catalog_snapshot.catalog_revision skill_snapshot
                     |> Skill_catalog_snapshot.catalog_revision_to_string) )
              ; ( "skill_config_revision"
                , match Skill_catalog_snapshot.config_revision skill_snapshot with
                  | Some revision ->
                    `String
                      (Skill_catalog_snapshot.config_revision_to_string revision)
                  | None -> `Null )
              ; ( "skill_rejection_count"
                , `Int
                    (List.length
                       (Skill_catalog_snapshot.rejections skill_snapshot)) )
              ; ( "skill_projection_diagnostic_count"
                , `Int (List.length s.skill_projection_diagnostics) )
              ; ( "skill_projection_diagnostics"
                , `List
                    (List.map
                       (fun
                         (diagnostic :
                           Keeper_skill_catalog.projection_diagnostic) ->
                          `Assoc
                            [ ( "identity"
                              , Skill_catalog_snapshot.identity_to_yojson
                                  diagnostic.identity )
                            ; ( "code"
                              , `String
                                  (Keeper_skill_catalog.error_code
                                     diagnostic.error) )
                            ])
                       s.skill_projection_diagnostics) )
              ]))
      Keeper_runtime_manifest.Context_injected;
    let cleanup_agent_setup () =
      Turn_helpers.cleanup_agent_setup ~keeper_name:meta.name s
    in
    Turn_helpers.run_with_setup_cleanup ~cleanup:cleanup_agent_setup
    @@ fun () ->
    (* Named for what it is: the surface handed to the lanes that cannot defer.
       The Agent Core lane sends [agent_core_tools] and then widens it, so an
       observation point that reaches for a bare [tools] is reaching for the
       wrong list -- this name makes that a compile error rather than a silent
       one. *)
    let built_tools = s.Keeper_run_tools.tools in
    let agent_core_tools = s.Keeper_run_tools.agent_core_tools in
    let hooks = s.Keeper_run_tools.hooks in
    let acc = s.Keeper_run_tools.acc in
    (* The same cell the turn's tools captured when they were built: an
       attached tool loaded mid-turn has to reach the agent that is running,
       and a second cell here would leave that one empty forever. *)
    let agent_ref = s.Keeper_run_tools.agent_cell in
    let final_agent_core_turn_ordinal_ref =
      s.Keeper_run_tools.final_agent_core_turn_ordinal_ref
    in
    let receipt_turn_count_ref = s.Keeper_run_tools.receipt_turn_count_ref in
    let receipt_model_used_ref = s.Keeper_run_tools.receipt_model_used_ref in
    let receipt_stop_reason_ref = s.Keeper_run_tools.receipt_stop_reason_ref in
    let receipt_runtime_observation_ref =
      s.Keeper_run_tools.receipt_runtime_observation_ref
    in
    let receipt_lane_attempt_index_ref =
      s.Keeper_run_tools.receipt_lane_attempt_index_ref
    in
    let receipt_response_text_present_ref =
      s.Keeper_run_tools.receipt_response_text_present_ref
    in
    let request_attribution_ref : request_attribution option ref = ref None in
    let request_wire_evidence_ref : request_wire_evidence option ref =
      ref None
    in
    (* Kept apart from the evidence cells rather than folded into them: the
       window cut is observed before serialization, so a turn whose request was
       refused at the wire has a real cut and no wire observation. Sharing one
       cell would let the missing half erase the half that was measured. *)
    let model_input_window_ref = ref None in
    let current_request_provider_content_ref :
      ( Agent_core.Types.message list
      , Keeper_agent_prompt_metrics.provenance_failure )
      result
      option
      ref
      =
      ref None
    in
    let current_request_projected_messages_ref = ref None in
    let source_model_input_projection =
      s.Keeper_run_tools.model_input_projection
    in
    let model_input_projection messages =
      (* [messages] already carries the bounded transmission view: the provider
         attempt applies it, because its budget is the target's declared
         request-body cap and that is only resolved per runtime
         ([Keeper_turn_driver_try_provider.budgeted_model_input_projection]).
         The source projection appends only a bounded typed Gate replay
         reference; exact replay bytes remain in the artifact store. The
         provenance check below compares against the list as received, so its
         projected-prefix precondition keeps holding. Durable state and
         checkpoints receive the unwindowed history. *)
      match source_model_input_projection messages with
      | Error _ as error ->
        current_request_provider_content_ref := None;
        current_request_projected_messages_ref := None;
        error
      | Ok projected_messages as result ->
        current_request_projected_messages_ref := Some projected_messages;
        let prompt_context_present =
          Option.is_some acc.Keeper_run_tools.extra_system_context_size
        in
        let provider_content =
          Keeper_agent_prompt_metrics.provider_content_messages
            ~prompt_context_present
            ~projection_input:messages
            ~projected_messages
        in
        (* The failure is kept, not only logged. A warn line tells an operator
           reading logs; the record has to tell a reader of the metrics why
           the turn carries no bytes. *)
        current_request_provider_content_ref := Some provider_content;
        (match provider_content with
         | Ok _ -> ()
         | Error failure ->
           Log.Keeper.warn
             "turn input composition unavailable: keeper=%s trace=%s reason=%s"
             meta.name trace_id
             (Keeper_agent_prompt_metrics.provenance_failure_summary failure));
        result
    in
    (* Attribution recorded by an official-client lane, which windows the
       provider-bound history inside the runtime and hands the result to a
       client that assembles the wire itself.

       Only a lane that started the conversation reports a list: on those
       turns the whole window is rendered into the request, so its bytes are
       the ones the model read. A resumed lane reports no list at all, because
       the client re-sends only the new turn and the accumulated history never
       leaves this process -- attributing the local window there would have
       counted bytes that were not sent, and on Antigravity would additionally
       have dropped the carrier that was. The gap is recorded as
       [Client_session_holds_input] rather than as a zero or an absent
       attribution, so a reader can tell it from a turn that never
       dispatched. *)
    let record_transmitted_model_input ~runtime_id ~tools ~transmitted =
      let prompt_context_present =
        Option.is_some acc.Keeper_run_tools.extra_system_context_size
      in
      let provider_content =
        match transmitted with
        | Keeper_official_client_host.Held_by_client_session ->
          Error Keeper_agent_prompt_metrics.Client_session_holds_input
        | Keeper_official_client_host.Whole_input_transmitted messages ->
          Result.map_error
            (fun failure ->
               Keeper_agent_prompt_metrics.Input_provenance_unresolved failure)
            (Keeper_agent_prompt_metrics.provider_content_of_transmitted
               ~prompt_context_present
               ~messages)
      in
      let attribution : request_attribution =
        { runtime_profile = runtime_id
        ; prompt_blocks = acc.prompt_blocks
        ; provider_content
        ; tools
        }
      in
      request_attribution_ref := Some attribution
    in
    (* 8. Run Agent *)
    let record_turn_progress, yield_on_tool, on_yield, on_resume, on_event =
      Turn_helpers.turn_progress_callbacks
        ~config
        ~keeper_name:meta.name
        ~downstream:on_event
        ~turn_id:manifest_keeper_turn_id
    in
    ignore (Keeper_alerting_path.ensure_sandbox_bundle ~config ~meta);
    let _keeper_sandbox_root = Keeper_sandbox.host_root_abs_of_meta ~config meta in
    let keeper_visible_sandbox_root =
      Keeper_sandbox.keeper_visible_root_abs_of_meta ~config meta
    in
    (* Tool/path confinement stays owned by MASC dispatch. Each filesystem and
       shell operation resolves its concrete target through
       [Keeper_alerting_path] and [Keeper_sandbox_containment]; AGENT_CORE receives no
       ambient path capability. *)
    (
       (* AGENT_CORE [stream_idle_timeout_s] bounds inter-line idle on HTTP streams
          only when the operator explicitly configures it. The deadline resets
          after each successful line, so this is gap detection, not a total run
          cap. [None] is carried unchanged: neither MASC nor AGENT_CORE may infer a
          provider/model default. *)
       let stream_idle_timeout_s =
         Keeper_runtime_resolved.stream_idle_timeout_sec ()
       in
       Keeper_agent_run_phase0_telemetry.record
         ~meta
         ~turn_system_prompt
           (* Pre-dispatch: run_named's failover has not chosen a lane yet, so
              nothing here knows which list will be sent. This records what the
              turn was built with, which is the only answer available. *)
         ~tools:built_tools
         ~history_messages
         ?user_blocks
         ~user_message
         ~start_turn_count
         ~max_context
         ();
       (* Section 3: Dispatch — call Keeper_turn_driver.run_named / Agent.run. *)
       let raw_trace = raw_trace_for_dispatch ~config ~meta in
       let turn_result =
         let cooperative_yield_probe =
           Some
             (fun (_ : Agent_core.Agent.Advanced.tool_boundary) ->
                try
                  (* AGENT_CORE invokes this probe after tool results and the
                     checkpoint have persisted. A descriptor-typed terminal
                     effect therefore either completes the turn or fails it;
                     neither state can re-enter the provider loop. *)
                  (match
                     terminal_effect_boundary_decision (s.terminal_effect_state ())
                   with
                   | Error _ as error -> error
                   | Ok (Runtime_agent.Yield _ as decision) -> Ok decision
                   | Ok Runtime_agent.Continue ->
                     (* Tool axis first: its input+output fingerprints are the
                        stronger no-progress proof and carry the tool name.
                        The text axis runs only when tool fingerprints still
                        move — the observed loop shape, where every turn's tool
                        batch differed but the plan sentence never did. *)
                     let repeated_loop_decision () =
                           (match
                              repeated_exact_tool_call
                                ~threshold:repeated_tool_call_yield_threshold
                                s.acc.tool_calls
                            with
                            | Some (tool_name, repeated_count) ->
                              Log.Keeper.warn
                                ~keeper_name:meta.name
                                "yielding repeated exact tool loop tool=%s \
                                 count=%d"
                                tool_name
                                repeated_count;
                              Ok
                                (Runtime_agent.Yield
                                   (Runtime_agent.Repeated_tool_call
                                      { tool_name; repeated_count }))
                            | None ->
                              (match
                                 repeated_tool_call_input
                                   ~threshold:
                                     repeated_tool_call_input_yield_threshold
                                   s.acc.tool_calls
                               with
                               | Some (tool_name, repeated_count) ->
                                 Log.Keeper.warn
                                   ~keeper_name:meta.name
                                   "yielding repeated tool input loop tool=%s \
                                    count=%d"
                                   tool_name
                                   repeated_count;
                                 Ok
                                   (Runtime_agent.Yield
                                      (Runtime_agent.Repeated_tool_call
                                         { tool_name; repeated_count }))
                               | None ->
                              (match
                                 repeated_assistant_text
                                   ~threshold:
                                     repeated_assistant_text_yield_threshold
                                   s.acc.assistant_turn_texts
                               with
                               | None -> Ok Runtime_agent.Continue
                               | Some repeated_count ->
                                 Log.Keeper.warn
                                   ~keeper_name:meta.name
                                   "yielding repeated assistant text count=%d"
                                   repeated_count;
                                 Ok
                                   (Runtime_agent.Yield
                                      (Runtime_agent.Repeated_assistant_text
                                         { repeated_count })))))
                     in
                     (match autonomous_yield_requested with
                      | None -> repeated_loop_decision ()
                      | Some requested ->
                        (match requested () with
                         | Ok (Some request) ->
                           Ok (Runtime_agent.Yield (runtime_yield_reason request))
                         | Ok None -> repeated_loop_decision ()
                         | Error detail ->
                           Error
                             (Agent_core.Error.Internal
                                ("keeper cooperative-yield snapshot failed: "
                                 ^ detail)))))
                with
                | Eio.Cancel.Cancelled _ as exn -> raise exn
                | exn ->
                  Error
                    (Agent_core.Error.Internal
                       (Printf.sprintf
                          "keeper cooperative-yield probe failed: %s"
                          (Printexc.to_string exn))))
         in
         let checkpoint_sidecar =
                ctx_work.checkpoint.Agent_core.Checkpoint.working_context
         in
         let last_persisted_checkpoint_ref = ref None in
         (* masc#28885: typed pre_tool_use rejects recorded by the
            official-client host during this turn. Flushed into the
            replay checkpoint only when the turn dies, so the model can
            repair the call next turn; a surviving turn already carries
            the round-trip through its own history. *)
         let pre_tool_rejects = ref [] in
         let checkpoint_sink (snapshot : Agent_core.Agent.checkpoint_snapshot) =
                Option.iter (fun observe -> observe snapshot.stage) on_checkpoint_stage;
                (* AGENT_CORE's per-turn pipeline builds checkpoints with an empty
                   session_id (the AGENT_CORE agent carries no session field), so the
                   sink must stamp the keeper's own session identity before
                   persisting. [meta.runtime.trace_id] is a validated,
                   non-empty [Trace_id.t]; without this restamp the checkpoint
                   transaction rejects an invalid persistence identity. *)
                let checkpoint =
                  { snapshot.checkpoint with
                    session_id =
                      Keeper_id.Trace_id.to_string meta.runtime.trace_id
                  ; working_context =
                      (match checkpoint_sidecar with
                       | Some _ as sidecar -> sidecar
                       | None -> snapshot.checkpoint.working_context)
                  }
                in
                match
                  Keeper_checkpoint_store.save_agent_core_classified
                    ~session_dir:session.session_dir
                    checkpoint
                with
                | Ok (Keeper_checkpoint_store.Saved _) ->
                  last_persisted_checkpoint_ref := Some checkpoint;
                  Ok ()
                | Ok (Keeper_checkpoint_store.Stale_noop _) -> Ok ()
                | Error _ as error -> error
         in
         let call_run_named ?raw_trace ~initial_messages () =
                (* Keeper does not impose a cumulative turn, time, token, or cost
                   budget. Explicit cancellation and provider/tool progress
                   boundaries settle the lane, while usage remains observational. *)
                dispatch_after_provider_transcript_admission
                  ~messages:initial_messages
                  ~checkpoint:resume_agent_core_checkpoint
                  ~dispatch:(fun ~checkpoint initial_messages ->
                    Keeper_turn_driver.run_named
                      ~runtime_id:runtime_id_string
                      ~base_path:config.base_path
                      ~keeper_name:meta.name
                      ~pre_tool_rejects
                      ~goal:user_message
                      ?goal_blocks:user_blocks
                      ~session_id:
                        (Keeper_id.Trace_id.to_string meta.runtime.trace_id)
                      ?raw_trace
                      ~system_prompt:turn_system_prompt
                      ~tools:built_tools
                      ~agent_core_tools
                      ~checkpoint_sink
                      ~initial_messages
                      ~model_input_projection
                      ~hooks
                      ?approval_gate
                      ~runtime_manifest_context
                      ~runtime_manifest_append:
                        (fun manifest ->
                           Keeper_runtime_manifest.append_best_effort
                             ~site:"runtime_runtime"
                             config
                             manifest)
                      ?deferred_runtime_lane
                      ~on_runtime_retry_deferred:record_runtime_retry_deferred
                      ?on_deferred_runtime_consumed
                      ?stream_idle_timeout_s
                      ?body_timeout_s:
                        (Keeper_runtime_resolved.body_timeout_override_sec ())
                      ~temperature
                      ~accept:
                        Keeper_tooling.Response.response_has_text_or_tool_progress
                      ?on_event
                      ?on_yield
                      ?on_resume
                      ~agent_ref
                      ?checkpoint_sidecar
                      ~cache_system_prompt:true
                      ~yield_on_tool
                      ~context_injector
                      ~context:shared_context
                      ~terminal_effect_state:s.terminal_effect_state
                      ~enable_thinking:(Keeper_config.keeper_enable_thinking ())
                      ?cooperative_yield_probe
                      ?agent_core_checkpoint:checkpoint
                      ?event_bus
                      ?trace_link
                      ~on_runtime_attempt:
                        (fun attempt ->
                           (* Each lane attempt assembles its own request.
                              Without this clear, a failed attempt's evidence
                              survives into the record of the runtime that
                              finally answered, and the metrics row credits
                              one lane's bytes to another.

                              All four cells, not just the two the record is
                              built from: the Agent Core wire handler reads
                              the provider-content and projected-message cells
                              to assemble its attribution, so leaving them set
                              makes the clear true of the record and false of
                              the inputs it is assembled from. That the Agent
                              Core lane happens to overwrite both on every
                              request is a property of that lane, not of this
                              invariant. *)
                           request_attribution_ref := None;
                           request_wire_evidence_ref := None;
                           current_request_provider_content_ref := None;
                           current_request_projected_messages_ref := None;
                           s.Keeper_run_tools.on_runtime_attempt attempt)
                      ~on_runtime_attempt_error:
                        (fun ~runtime_id:_ ~attempt _error ->
                           (* [on_runtime_attempt] observes only materialized
                              runtimes immediately before provider dispatch.
                              A candidate that disappeared from the runtime
                              catalog is still a routed lane attempt and emits
                              a typed attempt error without reaching that
                              callback. Preserve its index too, so total
                              failure cannot collapse the manifest's routed
                              candidates back to one. [max] makes the duplicate
                              observation of an ordinary failed provider
                              idempotent. *)
                           Keeper_run_tools_setup.record_lane_attempt_index
                             receipt_lane_attempt_index_ref
                             attempt)
                      ~on_runtime_observation:
                        (fun observation ->
                           receipt_runtime_observation_ref := Some observation)
                      ~on_model_input_window_observation:
                        (fun ~measurement observation ->
                           model_input_window_ref :=
                             Some (measurement, observation))
                      ~on_request_attribution:
                        (fun ~runtime_id ~tools ~transmitted ->
                           record_transmitted_model_input
                             ~runtime_id
                             ~tools
                             ~transmitted)
                      ~on_request_wire_observation:
                        (fun
                          ~runtime_id
                          ~max_request_body_bytes
                          ~body_bytes
                          ~serialized
                        ->
                           (match !current_request_provider_content_ref with
                            | Some (Ok provider_content) ->
                              s.Keeper_run_tools.stage_skill_delivery_on_wire
                                ~runtime_id
                                ~agent_core_turn:acc.current_turn
                                provider_content
                            | Some (Error _) | None -> ());
                           Keeper_request_wire_observation.record
                             ~keeper_name:meta.name
                             ~runtime_id
                             ~max_request_body_bytes
                             ~body_bytes;
                           let request_tools =
                             Keeper_agent_tool_surface.on_the_wire
                               ~agent_cell:agent_ref
                               ~built:built_tools
                           in
                           (match !current_request_provider_content_ref with
                            | None -> ()
                            | Some provider_content ->
                              let provider_content =
                                Result.map_error
                                  (fun failure ->
                                     Keeper_agent_prompt_metrics
                                     .Input_provenance_unresolved failure)
                                  provider_content
                              in
                              let attribution : request_attribution =
                                { runtime_profile = runtime_id
                                ; prompt_blocks = acc.prompt_blocks
                                ; provider_content
                                ; tools = request_tools
                                }
                              in
                              request_attribution_ref := Some attribution);
                           let wire_evidence : request_wire_evidence =
                             { wire_observation =
                                 { Turn_record.runtime_profile = runtime_id
                                 ; body_bytes
                                 }
                             ; serialized_observation = serialized
                             ; wire_projected_messages =
                                 !current_request_projected_messages_ref
                             ; wire_tools = request_tools
                             }
                           in
                           request_wire_evidence_ref := Some wire_evidence)
                      ~on_official_client_result_handoff:
                        s.Keeper_run_tools.observe_official_client_result_handoff
                      ~on_official_client_native_action:
                        s.Keeper_run_tools.observe_official_client_native_action
                      ())
         in
         (* Trace-store failure isolation: [raw_trace_for_dispatch]
                 degrades to [None] (turn runs untraced, typed record
                 emitted) — sink trouble never fails the turn pre-dispatch. *)
         (match
                 call_run_named ?raw_trace ~initial_messages:history_messages ()
               with
               | Error e ->
                 (match
                    Keeper_official_client_host.persist_pre_tool_rejects
                      ~session_dir:session.session_dir
                      ~session_id:
                        (Keeper_id.Trace_id.to_string meta.runtime.trace_id)
                      !pre_tool_rejects
                  with
                  | Ok 0 -> ()
                  | Ok persisted ->
                    Log.Keeper.info
                      "%s: persisted %d rejected tool round-trip(s) from the \
                       failed turn into the replay checkpoint (masc#28885)"
                      meta.name
                      persisted
                  | Error detail ->
                    Log.Keeper.warn
                      "%s: failed-turn reject round-trips could not be \
                       persisted: %s"
                      meta.name
                      detail);
                 Error e
               | Ok selected_run ->
                 let result = selected_run.Keeper_turn_driver.run_result in
                 let selected_runtime_id = selected_run.selected_runtime_id in
                 let selected_max_context = selected_run.selected_max_context in
                 let checkpoint_owner = selected_run.checkpoint_owner in
                 let lane_attempt_index = selected_run.lane_attempt_index in
                 let post_turn_t0 = Time_compat.now () in
                 (* Section 4: Result processing — parse response, handle tool calls, validate contracts. *)
                (* RFC-MASC-004: AfterTurn hooks flush incrementally during
          Agent.run. Post-run episode creation requires an explicit
          flush_incremental call since AfterTurn already fired. *)
                 let text = Agent_core.Types.text_of_content result.response.content in
                 let manifest_model_label =
                   Boundary_redaction.to_string
                     Boundary_redaction.runtime_model_label
                 in
                 receipt_turn_count_ref := Some result.turns;
                 receipt_model_used_ref :=
                   Option.bind
                     result.runtime_observation
                     (fun observation -> observation.selected_model);
                 receipt_stop_reason_ref := Some result.stop_reason;
                 receipt_runtime_observation_ref := result.runtime_observation;
                 receipt_lane_attempt_index_ref := lane_attempt_index;
                 (* Thinking is now persisted per-turn inside the after_turn
                    hook (Keeper_hooks_agent_core), untruncated, for EVERY turn. The
                    old post-run single-shot capture here saved only the final
                    turn's reasoning and would double-write the terminal turn
                    now, so it was removed. *)
                 let actual_keeper_tool_names =
                   Keeper_agent_result.tool_names_of_calls (List.rev acc.tool_calls)
                 in
                 let progress_keeper_tool_names =
                   progress_keeper_tool_names_for_contract
                     ~actual_keeper_tool_names
                     ~tool_calls:acc.tool_calls
                 in
                 let usage = Inference_utils.usage_of_response result.response in
                 let ctx_composition
                   : Keeper_agent_prompt_metrics.ctx_composition_metrics
                   =
                   let actual_input_tokens =
                     if usage.input_tokens > 0
                     then Some usage.input_tokens
                     else None
                   in
                   (* Absent evidence and unresolved provenance are recorded
                      as what they are. Writing a zero here is what let a
                      reader conclude the largest tool surfaces in the fleet
                      cost nothing (masc#32995). *)
                   let attribution =
                     match !request_attribution_ref with
                     | None ->
                       Keeper_agent_prompt_metrics.Not_measured
                         Keeper_agent_prompt_metrics.Dispatch_not_reached
                     | Some (attribution : request_attribution) ->
                       (match attribution.provider_content with
                        | Error gap ->
                          Keeper_agent_prompt_metrics.Not_measured gap
                        | Ok input_messages ->
                          Keeper_agent_prompt_metrics.Attributed
                            { runtime_profile = attribution.runtime_profile
                            ; segments =
                                Keeper_agent_prompt_metrics.build_ctx_segments
                                  ~prompt_blocks:attribution.prompt_blocks
                                  ~tools:attribution.tools
                                  ~input_messages
                            })
                   in
                   { Keeper_agent_prompt_metrics.actual_input_tokens
                   ; attribution
                   }
                 in
                 let completion_observation ()
                     : Keeper_execution_receipt.completion_contract_result =
                   Contract_helpers.observed_completion_evidence
                     ~actual_keeper_tool_names:progress_keeper_tool_names
                     ~stop_reason:result.stop_reason
                     ~response_text_present:(String.trim text <> "")
                 in
                 let completion_observation = completion_observation () in
                 acc.receipt_completion_contract_result <- completion_observation;
                 (* Root B (#22710): capture the world-observation actionable
                    signal alongside the contract status so the receipt carries
                    the real "is there anything to do" signal. [operator_disposition]
                    uses it to replace the [goal_ids = []] proxy. [None] when no
                    observation was threaded (disposition stays broadcast-required;
                    conservative). *)
                 acc.receipt_actionable_signal <-
                   Option.map
                     (fun obs ->
                       Keeper_contract_classifier.classify_actionable_signal
                         (Keeper_contract_classifier.of_keeper_world_observation obs))
                     world_observation;
                     (match
                        normalize_response_text_for_finalization
                          ~runtime_id:selected_runtime_id
                          ~initial_messages:history_messages
                          ~run_result:result
                          ~text
                          ~tool_names:actual_keeper_tool_names
                          ()
                      with
                      | Error e -> Error e
                      | Ok response_text ->
                        let terminal_effect_state = s.terminal_effect_state () in
                        let terminal_effect_receipt =
                          match terminal_effect_state with
                          | Keeper_tools_agent_core.Terminal_effect_completed receipt ->
                            Some receipt
                          | Keeper_tools_agent_core.Terminal_effect_open
                          | Keeper_tools_agent_core.Deferred_tool_result
                          | Keeper_tools_agent_core.External_effect_deferred
                          | Keeper_tools_agent_core.Terminal_effect_failed _ ->
                            None
                        in
                        let turn_outcome =
                          match terminal_effect_state with
                          | Keeper_tools_agent_core.Terminal_effect_completed _ ->
                            Ok Keeper_turn_outcome.Terminal_effect_settled
                          | Keeper_tools_agent_core.Terminal_effect_failed failure ->
                            Error
                              (Agent_core.Error.Internal
                                 ("successful Keeper run retained a failed terminal effect: "
                                  ^ failure.diagnostic))
                          | Keeper_tools_agent_core.Terminal_effect_open
                          | Keeper_tools_agent_core.Deferred_tool_result
                          | Keeper_tools_agent_core.External_effect_deferred ->
                            Ok
                              (Keeper_turn_outcome.of_result_surface
                                 ~response_text
                                 result.stop_reason)
                        in
                        (match turn_outcome, !final_agent_core_turn_ordinal_ref with
                         | Error e, _ -> Error e
                         | Ok _, None ->
                           Error
                             (Agent_core.Error.Internal
                                "successful Agent.run returned without an \
                                 AfterTurn ordinal")
                         | Ok turn_outcome, Some final_agent_core_turn_ordinal ->
                           Keeper_agent_run_finalize_response.finalize
                             ~config ~meta ~publication_recovery
                             ~ctx_snapshot:ctx_work
                             ~profile_defaults
                             ~manifest_keeper_turn_id
                             ~session ~append_manifest
                             ~model:manifest_model_label
                             ~acc
                             ~result
                             ~last_persisted_checkpoint:
                               !last_persisted_checkpoint_ref
                             ~final_agent_core_turn_ordinal
                             ~checkpoint_persistence_error
                             ~post_turn_t0
                             ~runtime_id_string:selected_runtime_id
                             ~max_context:selected_max_context
                             ~checkpoint_owner
                             ~history_messages
                             ~prompt_metrics ~ctx_composition ~usage
                             ~receipt_response_text_present_ref
                             ~history_assistant_source
                             ~raw_response_text:response_text
                             ~turn_outcome
                             ~terminal_effect_receipt
                             ?continuation_channel
                             ~capture_replay_response:
                               (fun ~response_text ->
                                 (* Phase O observability: capture the exact
                                    assistant text persisted for next-turn replay,
                                    after response finalization has applied
                                    suppression and internal-markup stripping. The
                                    capture is best-effort and gated by
                                    MASC_KEEPER_WIRE_CAPTURE. *)
                                 Keeper_wire_capture.capture_response
                                   ~base_path:config.base_path
                                   ~masc_root:(Workspace.masc_root_dir config)
                                   ~keeper_name:meta.name
                                   ~turn_id:manifest_keeper_turn_id
                                   ~agent_core_turn:result.turns
                                   ~trace_id:meta.runtime.trace_id
                                   ~response_text
                                   ())
                             ())))
               in
       let deferred_retry =
         Option.map
           (fun (hint : Keeper_turn_driver.deferred_runtime_lane) ->
              let reason =
                match
                  Keeper_error_classify.recoverable_runtime_failure_reason
                    hint.failure
                with
                | Some reason -> reason
                | None -> Keeper_error_classify.Deferred_runtime_lane
              in
              hint.next_runtime_id, reason)
           !deferred_runtime_lane_ref
       in
       let receipt_degraded_retry_runtime =
         match deferred_retry with
         | Some (runtime_id, _) -> Some runtime_id
         | None -> degraded_retry_runtime
       in
       let receipt_fallback_reason =
         match deferred_retry with
         | Some (_, reason) -> Some reason
         | None -> fallback_reason
       in
       let settled_runtime_id =
         match turn_result with
         | Ok result -> result.runtime_id
         | Error _ -> runtime_id_string
       in
       let settled_context_window =
         Keeper_turn_record_writer.context_window_of_turn
           ~turn_budget:max_context
           (match turn_result with Ok _ -> `Produced_result | Error _ -> `Errored)
       in
       let receipt_result =
         Keeper_agent_run_receipt.finalize
           ~config
           ~meta
           ~manifest_keeper_turn_id
           ~runtime_id:settled_runtime_id
           ~keeper_visible_sandbox_root
           ~receipt_started_at
           ~runtime_manifest_context
           ~acc
           ~degraded_retry_applied
           ~degraded_retry_runtime:receipt_degraded_retry_runtime
           ~fallback_reason:receipt_fallback_reason
           ~runtime_rotation_attempts
           ~turn_result
           ~receipt_turn_count_ref
           ~receipt_stop_reason_ref
           ~receipt_runtime_observation_ref
           ~receipt_lane_attempt_index_ref
           ~receipt_response_text_present_ref
           ()
       in
       (* RFC-0233 PR-3: TurnRecord — same per-keeper-turn cadence as the
          receipt above. execution_ids come from the trajectory
          accumulator (every entry of this run carries the id minted at
          the dispatch boundary); sampling reads the last agent-core turn's
          effective values from the turn context cell. *)
       (let tctx =
          Keeper_tool_call_log_context.get_turn_context_record
            ~cell:turn_ctx_cell ()
        in
        let execution_ids =
          match trajectory_acc with
          | None -> []
          | Some tacc ->
            (* entries are prepended on record; rev restores call order *)
            List.rev
              (List.filter_map
                 (fun (e : Trajectory.tool_call_entry) ->
                    Option.map Ids.Execution_id.of_string e.execution_id)
                 tacc.Trajectory.entries)
        in
        let usage : Turn_record.usage =
          match turn_result with
          | Ok result when result.usage_reported ->
            (* Cache counts travel with the turn rather than being dropped: a large
               input_tokens on a cache-heavy turn and one on a genuinely large prompt
               read identically without them, and the ctx-fill denominator below is the
               conversation ceiling, so the difference is what an operator acts on. *)
            { input_tokens = Some result.usage.input_tokens
            ; output_tokens = Some result.usage.output_tokens
            ; cache_creation_input_tokens =
                Some result.usage.cache_creation_input_tokens
            ; cache_read_input_tokens = Some result.usage.cache_read_input_tokens
            ; scope = result.usage_scope
            }
          | Ok _ | Error _ ->
            { input_tokens = None
            ; output_tokens = None
            ; cache_creation_input_tokens = None
            ; cache_read_input_tokens = None
            ; scope = Runtime_usage_scope.Usage_scope_unavailable
            }
        in
        let request_latency_ms : int option =
          (* RFC-0233 §9 — wall-clock duration of the provider call in
             milliseconds, sourced from AGENT_CORE
             [inference_telemetry.request_latency_ms]. The AGENT_CORE transport
             layer ([complete_common.patch_telemetry] non-streaming,
             [complete_stream] streaming) synthesizes it whenever a response
             is produced. Both [inference_telemetry] and the field itself are
             [option] (the transport layer may not synthesize a latency for
             every code path), so [Option.bind] flattens the two layers
             rather than nesting option-of-option; on the error path the
             dashboard renders absence for the generation phase rather than a
             fabricated duration. *)
          match turn_result with
          | Ok result ->
              Option.bind result.inference_telemetry (fun t ->
                t.request_latency_ms)
          | Error _ -> None
        in
        let ttfrc_ms : float option =
          (* RFC-0233 §10 — time-to-first-response-chunk (wall-clock, ms),
             sourced from AGENT_CORE [inference_telemetry.ttfrc_ms]. The streaming
             transport ([complete_stream]) fills it for every provider on the
             first SSE chunk, so it is populated across the streaming keeper
             fleet; non-streaming turns and the error path leave it [None].
             Both [inference_telemetry] and the field are [option], so
             [Option.bind] flattens (same pattern as [request_latency_ms]
             above). The decode (post-first-chunk) duration is NOT derived
             from request_latency_ms - ttfrc_ms (§9.6 fabrication guard). *)
          match turn_result with
          | Ok result ->
              Option.bind result.inference_telemetry (fun t -> t.ttfrc_ms)
          | Error _ -> None
        in
        (* RFC-0233 §2.3 — views derive, no view-side repair: ground the
           inspector's [selected_model] and [finish_reason] in the same refs
           the execution receipt already records this turn. [selected_model]
           is the successful attempt's observed model; [finish_reason] is
           the keeper stop reason serialized through the receipt SSOT
           ([Keeper_execution_receipt.stop_reason_to_string]). Both are
           [None] on the error path (receipt refs unset), never a
           fabricated value. *)
        (* RFC-0233 §8 — ground ctx-window/cost in real runtime facts so the
           dashboard stops fabricating 200K / Claude $3·$15. [context_window]
           is the keeper-resolved effective budget ([max_context]); pricing
           comes from the runtime binding retained in the Runtime singleton
           ([Runtime.pricing_of_runtime_id] projects binding.price_input/output
           — both option, None when the operator left runtime.toml unset, in
           which case the dashboard renders absence rather than a default). *)
        let (price_input_per_million, price_output_per_million) =
          Runtime.pricing_of_runtime_id settled_runtime_id
        in
        (* The names and schema sizes of the surface this request carried.
           [input_components] records what the schemas cost; without this the
           byte total can never be resolved back into which tools it was.

           Stored as a content-addressed blob and referenced by its canonical
           marker: the surface repeats almost unchanged from turn to turn, so
           inlining it would repeat in every record what one blob holds once.
           The maintenance scan already walks the keepers root these records
           live under and recognises the marker, so the blob stays reachable
           for as long as the record does.

           [None] when the turn recorded no request evidence, or when the put
           failed -- an unwritable blob is an absent reference, never a marker
           pointing at bytes that are not there. *)
        let tool_surface_ref =
          match !request_attribution_ref with
          | None -> None
          | Some (attribution : request_attribution) ->
            let request_tools = attribution.tools in
            let payload =
              Yojson.Safe.to_string
                (Turn_record.tool_surface_to_json
                   (List.map
                      (fun (tool : Agent_core.Tool.t) ->
                         (* The wire encoding, not the storage one.
                            [tool_schema_to_yojson] also emits [parameters],
                            the derived view kept for argument validation; a
                            definition carrying both is rejected on the way
                            out, so those bytes reach no provider. Counting
                            them put Execute at 10,396 bytes against the
                            7,239 it sends, and ranked the surface by a
                            number nobody is charged for. masc never sets
                            [strict], so what this records is exactly what
                            test_keeper_tool_schema_bytes ratchets. *)
                         { Turn_record.name = tool.Agent_core.Tool.schema.name
                         ; schema_bytes =
                             Agent_core.Base.Tool.wire_bytes_of_schema
                               tool.Agent_core.Tool.schema
                         })
                      request_tools))
            in
            let store = Tool_blob_store.create ~base_path:config.base_path in
            (match
               Tool_blob_store.put_durable
                 store
                 ~bytes:payload
                 ~mime:"application/json"
             with
             | reference ->
               Some
                 (Tool_output.encode_for_agent_core
                    (Tool_output.Stored reference)))
        in
        let input_components =
          match !request_attribution_ref with
          | None -> None
          | Some (attribution : request_attribution) ->
            (match attribution.provider_content with
             | Error _ -> None
             | Ok input_messages ->
               let segments =
                 Keeper_agent_prompt_metrics.build_ctx_segments
                   ~prompt_blocks:attribution.prompt_blocks
                   ~tools:attribution.tools
                   ~input_messages
               in
               Some
                 (List.map
                    (fun
                      (component,
                       (segment :
                         Keeper_agent_prompt_metrics.prompt_segment_metrics)) ->
                       { Turn_record.component = component
                       ; bytes = segment.bytes
                       })
                    segments))
        in
        let blocks =
          match !request_attribution_ref with
          | Some (attribution : request_attribution) ->
            attribution.prompt_blocks
          | None -> acc.prompt_blocks
        in
        let raw_trace_run_ref =
          let turn_trace_ref =
            match turn_result with
            | Ok { trace_ref; _ } -> trace_ref
            | Error _ -> None
          in
          match
            raw_trace_reference_for_turn ~turn_trace_ref ~sink:raw_trace
          with
          | None -> None
          | Some run_ref ->
            (match
               turn_record_raw_trace_run_ref ~expected_session_id:trace_id run_ref
             with
             | Ok exact_ref -> Some exact_ref
             | Error detail ->
               Log.Keeper.warn ~keeper_name:meta.name
                 "raw-trace run reference omitted from turn record: worker_run_id=%s: %s"
                 run_ref.worker_run_id detail;
               None)
        in
        (match !request_wire_evidence_ref with
         | Some
             { serialized_observation = Some wire
             ; wire_projected_messages = Some messages
             ; wire_tools = tools
             ; _
             } ->
           Keeper_provider_input_snapshot.write_best_effort
             ~config
             ~keeper:meta.name
             ~trace_id
             ~absolute_turn:manifest_keeper_turn_id
             ~runtime_profile:settled_runtime_id
             ~wire
             ~system_prompt:
               (Inference_utils.sanitize_text_utf8 turn_system_prompt)
             ~messages
             ~tools
         | Some { serialized_observation = None; _ }
         | Some { wire_projected_messages = None; _ }
         | None -> ());
        Keeper_turn_record_writer.write
          ~config
          ~keeper_name:meta.name
          ~agent_name:meta.name
          ~turn_kind
          ~trace_id
          ~absolute_turn:manifest_keeper_turn_id
          ~runtime_profile:settled_runtime_id
          ~selected_model:!receipt_model_used_ref
          ~finish_reason:
            (Option.map
               Keeper_execution_receipt.stop_reason_to_string
               !receipt_stop_reason_ref)
          ~context_window:settled_context_window
          ~price_input_per_million
          ~price_output_per_million
          ~request_latency_ms
          ~ttfrc_ms
          ~request_wire_observation:
            (Option.map
               (fun (evidence : request_wire_evidence) ->
                evidence.wire_observation)
               !request_wire_evidence_ref)
          ~model_input_window:
            (Option.map
               (fun
                 ( (measurement : Turn_record.model_input_measurement)
                 , (observation :
                     Runtime_model_input_tail_window.window_observation) )
               ->
                  { Turn_record.transmitted_atoms =
                      observation
                        .Runtime_model_input_tail_window.transmitted_atoms
                  ; total_atoms =
                      observation.Runtime_model_input_tail_window.total_atoms
                  ; measurement
                  })
               !model_input_window_ref)
          ~raw_trace_run_ref
          ~sampling:
            { temperature = Some temperature
            ; top_p = Runtime.top_p_of_runtime_id settled_runtime_id
            ; max_tokens = None
            ; thinking_budget = tctx.thinking_budget
            ; enable_thinking = tctx.thinking_enabled
            }
          ~usage
          ~execution_ids
          ~blocks
          ~input_components
          ~tool_surface_ref
          ();
        prune_raw_traces_after_turn_record ~config ~meta raw_trace;
        (* RFC-0233 §2.3 PR-4: project the same record onto the ambient
           turn span. Both turn drivers (unified "invoke_agent <keeper>"
           and direct "keeper_turn") keep their span open across this
           tail on the same fiber, so one add_attrs covers both. The
           OTel value type has no array — blocks serialize through the
           Turn_record codec (single encoding SSOT), execution ids join
           with commas. *)
        Otel_spans.add_attrs
          ~attrs:
            [ ( Otel_genai.Attr_key.masc_turn_blocks
              , `String
                  (Yojson.Safe.to_string
                     (`List
                        (List.map Turn_record.prompt_block_to_json blocks))) )
            ; ( Otel_genai.Attr_key.masc_turn_profile
              , `String settled_runtime_id )
            ; ( Otel_genai.Attr_key.masc_turn_execution_ids
              , `String
                  (String.concat ","
                     (List.map Ids.Execution_id.to_string execution_ids)) )
            ]
          ());
       receipt_result)
with
| exn when Keeper_registry_types.is_operator_interrupt exn ->
  (* Every delivery shape — bare at the [Switch.run] boundary,
     [Cancelled]-wrapped inside the switch, [Finally_raised]/[Multiple]
     combinations — records the same failure reason and re-raises
     unchanged (#28810, #28868 review). *)
  Keeper_registry.set_failure_reason
    ~base_path:config.base_path meta.name (Some Keeper_registry.Operator_interrupt);
  raise exn
| Eio.Cancel.Cancelled _ as ce ->
  raise ce
