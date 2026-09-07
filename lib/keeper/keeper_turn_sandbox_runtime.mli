(** Turn-scoped handle on a keeper-lifetime sandbox container.

    Lazily ensures one hardened container per keeper (adopting what a previous
    turn or server left running) and reuses it across compatible tool calls.
    A Docker container mounts the keeper playground read-write and takes
    commands by [docker exec]; a microvm guest owns its tree on a work volume
    and takes commands through the remote lane, so the exec entrypoints here
    refuse it. The root filesystem stays read-only either way. Turn cleanup
    drops the handle without removing the container; the keeper's shutdown
    finalization removes it. *)

type t

type state =
  | Not_started
  | Running of { container_name : string }

val create :
  config:Workspace.config ->
  meta:Keeper_meta_contract.keeper_meta ->
  ?network_mode:Keeper_types_profile_sandbox.network_mode ->
  unit ->
  t

val host_root : t -> string

val github_identity_secret_files : t -> string list
(** Credential files of the microvm identity snapshots already bound to this
    runtime, including retained snapshots. Reads the in-memory binding only:
    no token refresh, boot or guest replacement. A boxed execution has already
    acquired that binding; inspecting its output must not prepare a different
    identity after the command ran. *)

val prepare_github_identity_secret_files :
  ?timeout_sec:float -> t -> (string list, string) result
(** After authorization, observe and bind the container to the current GitHub
    identity, then return every credential file whose token must remain
    redacted. Docker: the bind is [ensure_started] itself and the redaction
    target is the stable [hosts.yml] the running container has mounted; a
    central login reaches it through the mount. Microvm: the guest's identity
    is its boot-time snapshot, and a drift from the central revision drops
    the handle so the next boot rebuilds it. *)

val cleanup : t -> unit
(** Best-effort teardown. Safe to call multiple times. *)

val microvm_remote_endpoint_of_running :
  t -> container_name:string -> (Keeper_sandbox_remote.t, string) result
(** The running guest as a remote endpoint (RFC-0400): [container exec] into
    [container_name] delivering the framed request to the mounted shim, the
    work volume as the remote root, the mounted identity snapshot as
    [GH_CONFIG_DIR], and the config env naming the mounted config. Refused
    for a non-microvm keeper. Pure. *)

val microvm_remote_endpoint :
  ?timeout_sec:float -> t -> (Keeper_sandbox_remote.t, string) result
(** {!microvm_remote_endpoint_of_running} after ensuring the guest is up. The
    keeper's root on the work volume is made at boot, so an adopted guest is
    already an endpoint. *)

val microvm_attached_endpoint :
  config:Workspace.config ->
  meta:Keeper_meta_contract.keeper_meta ->
  unit ->
  (Keeper_sandbox_remote.t, string) result
(** The keeper's guest as an endpoint, without a turn. The guest name is a
    function of the keeper and the base path and the guest is keeper-lifetime,
    so a caller that owns no part of the keeper's lifecycle can still reach
    one that is up.

    Never starts a guest and never probes for one: a stopped guest fails the
    [container exec] instead, and {!microvm_guest_absence_reason} names that
    failure afterwards. Booting for a reader would spend a VM start and write
    the identity snapshot and work root, which belong to the keeper's turn.
    Refused for a non-microvm keeper. *)

val microvm_guest_absence_reason :
  ?timeout_sec:float ->
  config:Workspace.config ->
  meta:Keeper_meta_contract.keeper_meta ->
  unit ->
  string option
(** [Some reason] when this keeper's guest is not running, for a caller that
    already holds a failure and wants the fact behind it. [None] when the
    guest is up, when the probe could not answer, or when the keeper is not a
    microvm -- in each of those the caller keeps its own error rather than
    replacing it with a guess. *)

val is_microvm_guest_booted :
  config:Workspace.config ->
  meta:Keeper_meta_contract.keeper_meta ->
  unit ->
  bool
(** Pure in-memory check: returns true if this server process has booted the
    microVM guest and prepared its work volume root. Returns false if the keeper
    is not Micro_vm or has not been booted yet in this process lifetime. *)

val forget_microvm_guest_booted :
  config:Workspace.config ->
  meta:Keeper_meta_contract.keeper_meta ->
  unit ->
  unit
(** Evicts the guest from the booted set (e.g. after it was observed stopped or dead). *)

module For_testing : sig
  val create_minimal
    :  config:Workspace.config
    -> meta:Keeper_meta_contract.keeper_meta
    -> state:state
    -> t

  val get_state : t -> state
  val set_state : t -> state -> unit
  val keeper_docker_container_name : t -> string
  (** The stable per-keeper container name, so the naming contract (stable
      across turns, split by network mode, bound to the base path) is
      testable without a docker daemon. *)

  val policy_route_holds
    :  network_mode:Keeper_types_profile_sandbox.network_mode
    -> booted_port:int option
    -> bound_port:int option
    -> bool
  (** Whether a running guest may be adopted rather than replaced, on the
      question of its network route alone. A [Network_policy] guest carries
      the lane's proxy port in its environment; the port is ephemeral and the
      guest is not, so the two can part. Answering this without a guest or a
      registry keeps the rule testable. *)
end

module For_testing_microvm : sig
  val microvm_container_name
    :  config:Workspace.config
    -> keeper_name:string
    -> network_mode:Keeper_types_profile_sandbox.network_mode
    -> string
  (** The stable per-keeper guest name. Apart from {!For_testing} because it
      is defined below it, and exposed for the same reason its Docker sibling
      is: a test that boots a guest has to be able to name the one it booted,
      and the mode belongs in that name so a guest from one policy is never
      adopted under another. *)

  val mark_microvm_guest_booted
    :  config:Workspace.config
    -> meta:Keeper_meta_contract.keeper_meta
    -> unit
    -> unit
end

val container_cwd_of_host :
  t -> host_cwd:string -> string

val run_argv_with_stdin_and_status_split :
  ?timeout_sec:float ->
  ?on_stdout_chunk:(string -> unit) ->
  ?on_stderr_chunk:(string -> unit) ->
  stdin_content:string ->
  string list ->
  Unix.process_status * string * string
(** Run a sandbox-management argv with stdin through the owned Docker execution
    boundary exactly once. The returned status, stdout, and stderr are the
    subprocess result without text-based classification, retry, or output
    suppression. This is intentionally lower-level than the turn-scoped [t]
    operations because one-shot sandbox startup paths need the same execution
    boundary before a reusable container exists. *)

val run_command_with_status :
  ?ok_exit_codes:int list ->
  timeout_sec:float ->
  t ->
  cwd:string ->
  command_argv:string list ->
  max_bytes:int ->
  unit ->
  (Unix.process_status * string, string) result

val exec_argv :
  ?stdin:bool ->
  ?timeout_sec:float ->
  validate_cached_container:bool ->
  t ->
  cwd:string ->
  command_argv:string list ->
  (string list, string) result
(** The argv that runs [command_argv] inside the turn-scoped container.

    Exactly what {!run_exec_with_status_split} blocks on, handed over instead
    of run: a command that must not hold the turn is spawned, and it has to
    land in the same container as the same uid under the same rewritten paths.
    Building that argv twice would be building the boundary twice. *)

val run_exec_with_status_split :
  ?stdin_content:string ->
  ?on_stdout_chunk:(string -> unit) ->
  ?on_stderr_chunk:(string -> unit) ->
  ?timeout_sec:float ->
  t ->
  cwd:string ->
  command_argv:string list ->
  (Unix.process_status * string * string, string) result
(** Execute [command_argv] inside the turn-scoped container and return split
    stdout/stderr without applying success-code policy. This is the argv-level
    entrypoint used by Shell IR dispatch. *)

type exec_pipeline_stage = {
  command_argv : string list;
  cwd : string option;
}

val run_exec_pipeline_with_status :
  ?on_stdout_chunk:(string -> unit) ->
  ?on_stderr_chunk:(string -> unit) ->
  ?timeout_sec:float ->
  t ->
  cwd:string ->
  stages:exec_pipeline_stage list ->
  (Unix.process_status * string * string, string) result
(** Execute [stages] as a streaming argv pipeline inside the turn-scoped
    container. Each stage is a separate [docker exec -i] process and adjacent
    stages are connected by host-side process pipes. *)

val run_bash_with_status :
  timeout_sec:float ->
  t ->
  cwd:string ->
  cmd:string ->
  unit ->
  (Unix.process_status * string, string) result

val teardown_keeper_sandbox_by_name :
  ?timeout_sec:float ->
  config:Workspace.config ->
  keeper_name:string ->
  backend:Keeper_sandbox.backend ->
  ?microvm_backend:Keeper_microvm_backend.t ->
  unit ->
  (unit, string) result
(** {!teardown_keeper_sandbox} for callers that hold the keeper's name and
    typed backend -- shutdown finalization, which runs after the registry
    entry is gone. Local and remote-SSH Keepers own no local container;
    Docker and microVM teardown target only their declared runtime.

    A [Micro_vm] teardown with no [microvm_backend] is
    [microvm_teardown_backend_unresolved] rather than an assumed runtime:
    sending [container delete --force] to a guest another runtime booted
    reports success while the guest keeps running. *)

val teardown_keeper_sandbox :
  ?timeout_sec:float ->
  config:Workspace.config ->
  meta:Keeper_meta_contract.keeper_meta ->
  unit ->
  (unit, string) result
(** Remove the keeper-lifetime containers for [meta]: the microvm guest and
    the persistent Docker containers, if any. Turn cleanup deliberately
    leaves both running (the guest boot and the container start are paid
    once per keeper, not per turn); this is the remove path, run at keeper
    shutdown finalization. A missing container is a successful teardown.
    The MicroVM identity snapshot is released only after its guest has
    stopped and been deleted. *)
