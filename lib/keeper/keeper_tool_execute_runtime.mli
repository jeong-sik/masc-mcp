(** Keeper_tool_execute_runtime — owner of the typed Shell IR execution
    pipeline and its public Keeper execution boundary. *)

val handle_tool_execute :
  turn_sandbox_factory:Keeper_sandbox_factory.t option ->
  config:Workspace.config ->
  meta:Keeper_meta_contract.keeper_meta ->
  ?continuation_channel:Keeper_continuation_channel.t ->
  ?gate_context:(unit -> Keeper_gate.causal_context) ->
  ?gate_grant:Keeper_gate.cycle_grant ->
  shell_ir_rewrite:(Masc_exec.Shell_ir.t -> (Masc_exec.Shell_ir.t, string) result) ->
    (** The caller's shell surface (RFC tools-as-shell-commands): it turns
        [masc] stages into delegated tool calls.  Required, because routing
        lives in the IR's sandbox field — a line that skips it is not refused,
        it runs as a host program named [masc], which is not the effect the
        caller asked for (#32730).  A lane with no turn to look a tool up in
        passes {!Keeper_shell_tool_command.refuse_reserved_command}, which
        answers rather than omits.  A closure rather than a module reference,
        so this runtime never names the module that supplies it. *)
  args:Yojson.Safe.t ->
  unit ->
  string

val gate_operation : string
(** The Gate operation name this runtime submits under. Shared with the replay
    path so an approved execute is recognised rather than skipped. *)

val replay_args_of_gate_input : Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** Recover the approved tool arguments from the stored Gate input.

    The Gate request wraps the arguments with execution context rather than
    re-encoding them, so the approved arguments are returned verbatim —
    including the [cwd] the submitting turn resolved and upserted into them.
    Replaying that [cwd] is the point: the approval describes one working
    directory, and re-deriving the current turn's default would execute
    somewhere the operator never saw.

    The envelope's sibling [cwd]/[sandbox_profile]/[sandbox_target] fields
    stay behind. The handler re-derives the sandbox from the current turn and
    rebuilds the envelope, so a sandbox that moved between approval and replay
    produces a different canonical input and fails the match rather than
    executing under a profile the approval did not describe. *)

val handle_tool_execute_with_outcome :
  turn_sandbox_factory:Keeper_sandbox_factory.t option ->
  config:Workspace.config ->
  meta:Keeper_meta_contract.keeper_meta ->
  ?continuation_channel:Keeper_continuation_channel.t ->
  ?gate_context:(unit -> Keeper_gate.causal_context) ->
  ?gate_grant:Keeper_gate.cycle_grant ->
  shell_ir_rewrite:(Masc_exec.Shell_ir.t -> (Masc_exec.Shell_ir.t, string) result) ->
    (** The caller's shell surface (RFC tools-as-shell-commands): it turns
        [masc] stages into delegated tool calls.  Required, because routing
        lives in the IR's sandbox field — a line that skips it is not refused,
        it runs as a host program named [masc], which is not the effect the
        caller asked for (#32730).  A lane with no turn to look a tool up in
        passes {!Keeper_shell_tool_command.refuse_reserved_command}, which
        answers rather than omits.  A closure rather than a module reference,
        so this runtime never names the module that supplies it. *)
  args:Yojson.Safe.t ->
  unit ->
  Keeper_tool_execution.t

module For_testing : sig
  val secret_files_for_source :
    source:Keeper_gate.authorization_source ->
    observed:(unit -> string list) ->
    prepare:(unit -> (string list, string) result) ->
    (string list, string) result
  (** Production identity selection at the Execute boundary: an already
      executed boxed result reads its bound identity instead of refreshing it. *)

  (* Test seam: when set, [handle_tool_execute_typed] routes its dispatch
     through this override instead of the real shell dispatch, so tests can
     drive each rejected-dispatch branch through the real production wiring
     (stream start -> dispatch -> stream end) without spawning a process. *)
  val dispatch_override :
    (unit ->
     ( Masc_exec.Exec_dispatch.dispatch_result
     , Keeper_tooling.Execute_shell_ir.dispatch_error )
     result)
    option
    ref

  val model_execute_location_fields :
    config:Workspace.config ->
    meta:Keeper_meta_contract.keeper_meta ->
    args:Yojson.Safe.t ->
    cwd:string ->
    (string * Yojson.Safe.t) list

  val redact_execute_output :
    base_path:string ->
    keeper_name:string ->
    stdout:string ->
    stderr:string ->
    string * string * string

  val redact_execute_output_with_additional_secret_files :
    additional_secret_files:string list ->
    base_path:string ->
    keeper_name:string ->
    stdout:string ->
    stderr:string ->
    string * string * string
end
