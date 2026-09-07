(** Filesystem Compatibility Layer - Eio-native I/O with fallback

    @since 2026-02 - Keeper Emergent Identity v2.0
*)

open Fs_compat_internal

module Atomic_orphan_size_class = Atomic_orphan_size_class

(** Capability-scoped observation of one immutable regular-file snapshot.

    This module owns no digest, schema, history, migration, scan, retry, repair,
    or deletion policy. It pins the supplied parent capability and dispatches
    exactly one single-component leaf open.

    Before allocation, [read] proves non-negative [expected_length <=
    max_length], representability, regular-file kind, one link, mode [0600]
    without special bits, the safety ceiling, and the exact expected size. It
    then reads exactly N bytes plus EOF and checks the opened descriptor's
    identity, link count, mode, and size again. Successful observations and
    non-fatal typed failures preserve child-close and parent-resource
    settlement warnings. [Out_of_memory], [Stack_overflow], and [Sys.Break]
    remain fatal: they are re-raised with their original backtrace and are
    outside the typed-outcome and settlement-warning preservation contract.

    Cancellation is linearized as follows: pending parent cancellation is
    checked when [read] starts. The child open dispatch, exact observation, and
    close settlement run in a protected cancellation scope. The completed
    typed outcome is never replaced by a later cancellation: cancellation
    observed during parent-resource settlement is retained as a typed
    [Settle_resources] warning rather than raised by a final check. *)
module Capability_exact_read : sig
  type operation =
    | Pin_parent
    | Open_parent_descriptor
    | Open_leaf
    | Inspect_opened
    | Allocate
    | Read_exact
    | Inspect_after_read
    | Close_leaf
    | Settle_parent_resources
    | Observe_parent_cancellation

  type diagnostic =
    { operation : operation
    ; detail : string
    }

  type settlement_warning =
    | Close_failed of diagnostic
    | Settle_resources of diagnostic

  type error =
    | Invalid_leaf of string
    | Invalid_length_bounds of
        { expected_length : int64
        ; max_length : int64
        }
    | Length_not_representable of int64
    | Cancelled of diagnostic
    | Parent_descriptor_unavailable
    | Missing
    | Symbolic_link
    | Not_regular of Unix.file_kind
    | Unsafe_link_count of int
    | Unsafe_mode of int
    | Length_exceeds_max of
        { max_length : int64
        ; observed_length : int64
        }
    | Length_mismatch of
        { expected_length : int64
        ; observed_length : int64
        }
    | Changed_during_read
    | Io_error of diagnostic

  type observation

  val observation_bytes : observation -> string
  val observation_length : observation -> int64
  val observation_settlement_warnings :
    observation -> settlement_warning list

  type failure = private
    { error : error
    ; settlement_warnings : settlement_warning list
    }

  (** [read ~parent ~leaf ~expected_length ~max_length] observes only [leaf]
      relative to the pinned [parent] descriptor. [leaf] must be one valid path
      component. *)
  val read :
    parent:Eio.Fs.dir_ty Eio.Path.t ->
    leaf:string ->
    expected_length:int64 ->
    max_length:int64 ->
    (observation, failure) result
end

(** #9921: raised by mutating [Fs_compat] entry points
    ([append_file], [save_file], [mkdir_p]) when the target path falls
    under [HOME] and the process is a test executable. Defense in depth
    behind [Env_config_core.base_path_prod_guard]. Bypass with
    [MASC_TEST_ALLOW_HOME_BASE_PATH=1]. *)
exception Test_isolation_breach of string

(** Set global Eio filesystem. Call at server startup. *)
val set_fs : Eio.Fs.dir_ty Eio.Path.t -> unit

(** Clear global fs (testing/shutdown). *)
val clear_fs : unit -> unit

(** Get the global Eio filesystem if available. *)
val get_fs_opt : unit -> Eio.Fs.dir_ty Eio.Path.t option

(** Check if Eio fs is available. *)
val has_fs : unit -> bool

type execution_context =
  | Eio_fiber
  | Non_eio

val execution_context : unit -> execution_context
(** Actual execution context of the current caller. Process-global filesystem
    installation does not imply that a raw Domain has an Eio effect handler. *)

type exact_path_kind =
  | Exact_missing
  | Exact_kind of Unix.file_kind
  | Exact_unknown

(** Eio-native exact path classification. Unlike {!path_kind}, this preserves
    regular files, symbolic links, FIFOs, sockets, and devices as distinct
    [Unix.file_kind] values. Classification followed by a separate path-based
    I/O operation is not atomic; owned-file readers must use
    {!load_owned_regular_file}. *)
val exact_path_kind : ?follow:bool -> string -> exact_path_kind

type path_kind =
  | Missing
  | Directory
  | Other

(** Coarse projection of {!exact_path_kind}. [follow=false] classifies a
    symbolic link as [Other] instead of classifying its target. Non-missing
    I/O failures remain explicit. *)
val path_kind : ?follow:bool -> string -> path_kind

type owned_directory_chain_rejection = Owned_directory_chain.rejection =
  | Owned_path_outside_root of
      { ownership_root : string
      ; path : string
      }
  | Owned_path_non_directory of
      { path : string
      ; kind : Unix.file_kind
      }

type owned_directory_chain_observation = Owned_directory_chain.observation =
  | Owned_directory_missing
  | Owned_directory of Unix.stats

val inspect_owned_directory_chain
  :  ownership_root:string
  -> string
  -> (owned_directory_chain_observation, owned_directory_chain_rejection) result
(** Shared no-follow ownership-boundary inspection. *)

val file_kind_to_string : Unix.file_kind -> string
(** Render a [Unix.file_kind] as the lowercase snake_case name of the
    constructor ([S_REG] becomes ["regular_file"], [S_FIFO] becomes
    ["fifo"]). *)

val owned_directory_chain_rejection_to_string
  :  owned_directory_chain_rejection
  -> string

val owned_directory_paths
  :  ownership_root:string
  -> string
  -> (string list, owned_directory_chain_rejection) result
(** Lexical ordered descendant paths for ownership-aware directory creation. *)

type owned_regular_file_read_operation =
  | Inspect_parent
  | Inspect_path
  | Open_path
  | Inspect_descriptor
  | Read_contents
  | Close_descriptor

type owned_regular_file_read_failure =
  | Ownership_boundary_rejected of
      { path : string
      ; rejection : owned_directory_chain_rejection
      }
  | Path_is_not_regular_file of
      { path : string
      ; kind : Unix.file_kind
      }
  | Filesystem_identity_changed of { path : string }
  | Owned_file_operation_failed of
      { path : string
      ; operation : owned_regular_file_read_operation
      ; cause : exn
      }

type owned_regular_file_read_error =
  { failure : owned_regular_file_read_failure
  ; close_failure : exn option
  }

(** Read one process-owned regular file without accepting symbolic links or a
    changed parent chain. [Ok None] means that the owned directory or file is
    absent. OCaml 5.4 does not expose [O_NOFOLLOW], so the implementation
    validates [lstat]/[fstat] identity before reading and revalidates the
    no-follow parent and leaf boundary after the read. Blocking Unix operations
    run in a system thread when the Eio filesystem is active. A simultaneous
    read and descriptor-close failure preserves both causes. *)
val load_owned_regular_file
  :  ownership_root:string
  -> string
  -> (string option, owned_regular_file_read_error) result

type owned_regular_file_snapshot =
  { device : int
  ; inode : int
  ; owner_uid : int
  ; permissions : int
  ; file_size : int
  ; modified_at : float
  ; changed_at : float
  }

val equal_owned_regular_file_snapshot
  :  owned_regular_file_snapshot
  -> owned_regular_file_snapshot
  -> bool

type owned_regular_file_contents =
  { content : string
  ; snapshot : owned_regular_file_snapshot
  }

val load_owned_regular_file_with_snapshot
  :  ownership_root:string
  -> string
  -> (owned_regular_file_contents option, owned_regular_file_read_error) result
(** Whole-file owned read plus the exact descriptor snapshot validated before
    and after I/O. Consumers may cache a content digest against [snapshot] and
    reuse it only while a later owned read reports an equal snapshot. *)

type owned_regular_file_prefix =
  { content : string
  ; file_size : int
  ; modified_at : float
        (** From the same validated descriptor as [file_size], so a caller
            listing a directory can order by modification time without a
            second stat and without reading past [max_bytes]. *)
  ; truncated : bool
  }

val load_owned_regular_file_prefix
  :  ownership_root:string
  -> max_bytes:int
  -> string
  -> (owned_regular_file_prefix option, owned_regular_file_read_error) result
(** Bounded-prefix sibling of {!load_owned_regular_file}. It preserves the
    same no-follow parent-chain, descriptor identity, regular-file, and
    changed-during-read checks, but reads at most [max_bytes]. The existing
    whole-file API and its callers are unchanged. *)

type owned_regular_file_range =
  { content : string
  ; snapshot : owned_regular_file_snapshot
  }

val load_owned_regular_file_range
  :  ownership_root:string
  -> offset:int
  -> max_bytes:int
  -> string
  -> (owned_regular_file_range option, owned_regular_file_read_error) result
(** Bounded random-access sibling of {!load_owned_regular_file}. It preserves
    the same owned path and stable-descriptor checks while reading at most
    [max_bytes] from [offset]. The returned snapshot is the descriptor
    snapshot validated across that exact read. Negative bounds return a typed
    read error; an offset at or beyond EOF returns empty [content]. *)

val owned_regular_file_read_error_to_string
  :  owned_regular_file_read_error
  -> string

(** Eio-native, deterministically sorted directory inventory. *)
val read_dir : string -> string list

(** Load entire file as string. *)
val load_file : string -> string

(** Load entire file as string, or [None] when the file is missing.
    Option-returning sibling of {!load_file} (which raises on a missing
    path). Other I/O failures of an existing file propagate as
    [Sys_error]. *)
val load_file_opt : string -> string option

(** Save string to file (overwrite). *)
val save_file : string -> string -> unit

(** Write content to path via temp file + rename.
    Returns [Error msg] on I/O failure instead of raising. *)
val save_file_atomic : string -> string -> (unit, string) Result.t

type atomic_replace_failure_stage =
  Atomic_write.atomic_replace_failure_stage =
  | Before_rename
  | After_rename

type atomic_replace_failure =
  Atomic_write.atomic_replace_failure =
  { path : string
  ; stage : atomic_replace_failure_stage
  ; exception_ : exn
  ; backtrace : Printexc.raw_backtrace
  }

val atomic_replace_failure_to_string : atomic_replace_failure -> string

val save_file_atomic_strict_staged
  :  string
  -> string
  -> (unit, atomic_replace_failure) Result.t
(** Strict replacement retaining whether a failure preceded or followed the
    target rename. Payload and parent-directory [Unix.fsync] must both return
    successfully. This supports process-restart recovery, not hardware or
    power-loss persistence, and does not use Darwin [F_FULLFSYNC]. Transaction
    owners must converge any dependent in-memory publication before
    propagating an [After_rename] failure. *)

val write_file_atomic_strict_staged
  :  string
  -> write:(out_channel -> unit)
  -> (unit, atomic_replace_failure) Result.t
(** Streaming sibling of {!save_file_atomic_strict_staged}. [write] receives a
    binary channel and runs synchronously inside the blocking replacement job
    (a system thread when called from Eio). It must not perform Eio effects,
    close the channel, or retain it. The channel is closed before payload sync
    and rename. Callback exceptions, including cancellation, preserve the
    original exception and backtrace in a [Before_rename] failure. *)

(** Atomic replacement whose payload and parent-directory fsyncs are mandatory. *)
val save_file_atomic_strict : string -> string -> (unit, string) Result.t

module Atomic_replace_for_testing : sig
  val save_file_atomic_strict_staged
    :  ?sync_file:(string -> unit)
    -> sync_parent:(string -> unit)
    -> string
    -> string
    -> (unit, atomic_replace_failure) Result.t

  val write_file_atomic_strict_staged
    :  ?sync_file:(string -> unit)
    -> sync_parent:(string -> unit)
    -> string
    -> write:(out_channel -> unit)
    -> (unit, atomic_replace_failure) Result.t
end

(** [open_atomic_temp_file ~temp_dir ()] creates and opens a fresh
    temp file in [temp_dir] using the canonical [.atomic_*.tmp]
    filename shape. The caller owns the returned channel and file. *)
val open_atomic_temp_file : temp_dir:string -> unit -> string * out_channel

(** [true] exactly for one non-empty lexical child component. This is the
    shared path-effect and mutation-lease validation boundary. *)
val is_capability_leaf : string -> bool

type capability_append_operation_failure =
  { exception_ : exn
  ; backtrace : Printexc.raw_backtrace
  }

type capability_append_failure =
  | Capability_append_posix_descriptor_unavailable
  | Capability_append_mutation_contended
  | Capability_append_operation_failed of capability_append_operation_failure

type capability_append_target_binding =
  | Capability_append_target_not_checked
  | Capability_append_target_verified
  | Capability_append_target_changed
  | Capability_append_target_check_failed of capability_append_operation_failure

type capability_append_outcome =
  { requested_bytes : int
  ; bytes_written : int
  ; write_failure : capability_append_failure option
  ; sync_failure : capability_append_operation_failure option
  ; target_binding : capability_append_target_binding
  }

val capability_append_failure_to_string : capability_append_failure -> string

type capability_append_open_error =
  | Capability_append_open_invalid_leaf of string
  | Capability_append_open_missing
  | Capability_append_open_failed of capability_append_operation_failure

type capability_append_file

val capability_append_open_error_to_string : capability_append_open_error -> string

(** Open an opaque append capability. The resource is always opened by this
    module with kernel append semantics; callers cannot construct a capability
    from an arbitrary file resource. Its lifetime belongs to [sw]. *)
val open_capability_append_file
  :  sw:Eio.Switch.t
  -> parent:Eio.Fs.dir_ty Eio.Path.t
  -> leaf:string
  -> (capability_append_file, capability_append_open_error) result

val capability_append_file_stat : capability_append_file -> Eio.File.Stat.t

(** Append through an opaque append capability without destructive rollback.
    Cooperative same-process mutations of the same opened target identity use
    one shared non-blocking lease without lexical-name normalization. An active
    absent-target publication under the same parent also excludes the append,
    covering the transition from an absent name to a visible inode. The
    leaf-to-open-file identity is checked before and after the write, so an
    external rename is reported rather than misclassified as a visible append.
    External processes are an observation boundary, not an exclusion
    guarantee. Partial bytes and their fsync result remain explicit. The caller
    owns the next cancellation checkpoint. *)
val append_capability_observed
  :  capability_append_file
  -> string
  -> capability_append_outcome

type capability_append_io_for_testing =
  { write_substring : Unix.file_descr -> string -> int -> int -> int
  ; fsync : Unix.file_descr -> unit
  }

module Capability_append_for_testing : sig
  val append_capability_observed
    :  after_write:(unit -> unit)
    -> capability_append_file
    -> string
    -> capability_append_outcome

  val append_fd_observed
    :  io:capability_append_io_for_testing
    -> fd:Unix.file_descr
    -> string
    -> capability_append_outcome
end

type atomic_replace_recovery_target = Atomic_write.atomic_replace_recovery_target
type atomic_replace_recovery_target_error = Atomic_write.atomic_replace_recovery_target_error

module Publication_recovery : sig
  type registry = Fs_compat_internal.Publication_recovery_access.registry
  type t = Fs_compat_internal.Publication_recovery_access.t
  type owner = Fs_compat_internal.Publication_recovery_access.owner
  type registry_error =
    Fs_compat_internal.Publication_recovery_access.registry_error
  type lane_open_error =
    Fs_compat_internal.Publication_recovery_access.lane_open_error
  type lane_release_failure =
    Fs_compat_internal.Publication_recovery_access.lane_release_failure

  type lane_open_error_category =
    | Invalid_owner_category
    | Reconciliation_blocked_category
    | Store_failed_category

  type owner_discovery_row =
    | Discovered_owner of owner
    | Invalid_owner_name of string

  type discovery_failure

  type discovery_error =
    | Registry_discovery_in_progress
    | Registry_discovery_terminal of discovery_failure

  type discovery_health_phase =
    | Health_discovery_required
    | Health_discovery_running
    | Health_discovery_failed
    | Health_discovery_complete

  type owner_health_counts =
    { inspection_pending : int
    ; inspection_running : int
    ; reconciliation_pending : int
    ; reconciliation_running : int
    ; ready_without_obligation : int
    ; ready : int
    ; blocked : int
    }

  type health_snapshot =
    { discovery_phase : discovery_health_phase
    ; discovery_row_count : int
    ; discovered_owner_count : int
    ; invalid_owner_name_count : int
    ; retryable_lane_failure_count : int
    ; owners : owner_health_counts
    }

  type 'a lane_outcome =
    | Lane_released of 'a
    | Lane_release_failed of
        { value : 'a
        ; release_failure : lane_release_failure
        }

  val open_registry
    :  sw:Eio.Switch.t
    -> fs:Eio.Fs.dir_ty Eio.Path.t
    -> registry_root:Eio.Fs.dir_ty Eio.Path.t
    -> (registry, registry_error) result

  val discover_owners
    :  registry
    -> (owner_discovery_row list, discovery_error) result

  val health_snapshot : registry -> health_snapshot
  val owner_to_string : owner -> string
  val owner_discovery_row_to_string : owner_discovery_row -> string
  val registry_error_to_string : registry_error -> string
  val discovery_failure_to_string : discovery_failure -> string
  val discovery_error_to_string : discovery_error -> string

  val with_lane
    :  registry:registry
    -> owner:string
    -> (t -> 'a)
    -> ('a lane_outcome, lane_open_error) result

  val lane_open_error_to_string : lane_open_error -> string
  val lane_release_failure_to_string : lane_release_failure -> string
  val lane_open_error_category : lane_open_error -> lane_open_error_category
end

(** Build the immutable recovery locator projection used by
    {!replace_capability_file}. *)
val atomic_replace_recovery_target
  :  allowed_root_path:string
  -> allowed_root_device:int64
  -> allowed_root_inode:int64
  -> parent_components:string list
  -> target_leaf:string
  -> permissions:int
  -> (atomic_replace_recovery_target, atomic_replace_recovery_target_error) result

val atomic_replace_recovery_target_error_to_string
  :  atomic_replace_recovery_target_error
  -> string

type capability_write_operation = Atomic_write.capability_write_operation =
  | Atomic_replace_operation
  | Create_exclusive_operation

type capability_write_stage = Atomic_write.capability_write_stage =
  | Validate_leaf
  | Acquire_mutation_lease
  | Acquire_publication_lease
  | Inspect_target_entry
  | Verify_target_binding
  | Prepare_recovery_obligation
  | Create_staging_directory
  | Inspect_staging_directory
  | Acquire_staging_directory
  | Apply_staging_directory_permissions
  | Verify_staging_directory_identity
  | Preserve_unbound_recovery_obligation
  | Bind_recovery_obligation
  | Create_staging_entry
  | Create_target_entry
  | Inspect_open_resource
  | Write_payload
  | Apply_permissions
  | Sync_payload
  | Close_payload
  | Verify_entry_identity
  | Publish_replace
  | Sync_staging_directory
  | Sync_parent
  | Remove_staging_directory
  | Close_staging_directory
  | Discharge_prepared_recovery_obligation
  | Discharge_bound_recovery_obligation
  | Cleanup_close
  | Cleanup_verify_identity
  | Cleanup_unlink
  | Cleanup_sync_staging_directory
  | Cleanup_verify_staging_directory_identity
  | Cleanup_remove_staging_directory
  | Cleanup_close_staging_directory
  | Cleanup_sync_parent

type capability_write_target_effect = Atomic_write.capability_write_target_effect =
  | Target_unchanged
  | Target_created
  | Target_created_incomplete
  | Target_replaced
  | Target_state_unknown

type capability_write_operation_failure =
  Atomic_write.capability_write_operation_failure =
  { exception_ : exn
  ; backtrace : Printexc.raw_backtrace
  }

type capability_write_payload_failure =
  Atomic_write.capability_write_payload_failure =
  { exception_ : exn
  ; backtrace : Printexc.raw_backtrace
  ; bytes_written : int
  }

type capability_write_cause = Atomic_write.capability_write_cause =
  | Invalid_leaf of string
  | Invalid_recovery_target of atomic_replace_recovery_target_error
  | Mutation_contended
  | Posix_descriptor_unavailable
  | Unexpected_resource_kind of Eio.File.Stat.kind
  | Resource_identity_unavailable
  | Resource_identity_changed
  | Payload_write_failed of capability_write_payload_failure
  | Operation_failed of capability_write_operation_failure

type capability_write_failure = Atomic_write.capability_write_failure =
  { stage : capability_write_stage
  ; cause : capability_write_cause
  }

type capability_recovery_phase = Atomic_write.capability_recovery_phase =
  | Recovery_validate_owner
  | Recovery_open_registry
  | Recovery_open_store
  | Recovery_prepare
  | Recovery_preserve_unbound
  | Recovery_bind
  | Recovery_discharge_prepared
  | Recovery_discharge_bound

type capability_recovery_removal_transition =
  Atomic_write.capability_recovery_removal_transition =
  | Recovery_discharge_active
  | Recovery_discharge_owned
  | Recovery_active_to_owned
  | Recovery_active_to_forensic
  | Recovery_owned_to_forensic

type capability_recovery_effect = Atomic_write.capability_recovery_effect =
  | Recovery_no_record_change
  | Recovery_layout_may_be_incomplete
  | Recovery_layout_ready
  | Recovery_active_record_state_unknown
  | Recovery_active_record_durable
  | Recovery_active_record_discharged
  | Recovery_owned_record_state_unknown_with_active
  | Recovery_owned_record_durable_with_active
  | Recovery_owned_record_durable
  | Recovery_owned_record_discharged
  | Recovery_forensic_record_state_unknown_with_source
  | Recovery_forensic_record_durable_with_source
  | Recovery_forensic_record_durable
  | Recovery_source_removal_durability_unknown of
      capability_recovery_removal_transition

type capability_recovery_failure = Atomic_write.capability_recovery_failure

val capability_recovery_phase_to_string : capability_recovery_phase -> string
val capability_recovery_effect_to_string : capability_recovery_effect -> string

val capability_recovery_failure_phase
  :  capability_recovery_failure
  -> capability_recovery_phase

val capability_recovery_failure_effect
  :  capability_recovery_failure
  -> capability_recovery_effect

val capability_recovery_failure_to_string
  :  capability_recovery_failure
  -> string

type capability_recovery_access_failure =
  Atomic_write.capability_recovery_access_failure =
  | Recovery_access_not_available

type capability_write_primary_failure =
  Atomic_write.capability_write_primary_failure =
  | Write_primary_failure of capability_write_failure
  | Recovery_primary_failure of capability_recovery_failure
  | Recovery_access_primary_failure of capability_recovery_access_failure

type capability_write_cleanup_failure =
  Atomic_write.capability_write_cleanup_failure =
  | Write_cleanup_failure of capability_write_failure
  | Recovery_cleanup_failure of capability_recovery_failure

type capability_write_error = Atomic_write.capability_write_error =
  { operation : capability_write_operation
  ; target_effect : capability_write_target_effect
  ; primary_failure : capability_write_primary_failure
  ; cleanup_failures : capability_write_cleanup_failure list
  }

type capability_directory_sync_error = Atomic_write.capability_directory_sync_error =
  { failure : capability_write_failure
  ; cleanup_failures : capability_write_failure list
  }

type capability_write_cancellation = Atomic_write.capability_write_cancellation =
  { operation : capability_write_operation
  ; target_effect : capability_write_target_effect
  ; interrupted_primary_failure : capability_write_primary_failure option
  ; interrupted_recovery : capability_recovery_failure option
  ; cleanup_failures : capability_write_cleanup_failure list
  }

exception Capability_write_cancelled of exn * capability_write_cancellation

(** Durable replacement below an already-open target-parent capability.
    Recovery access and the immutable target projection are mandatory. *)
val replace_capability_file
  :  recovery:Publication_recovery.t
  -> parent:Eio.Fs.dir_ty Eio.Path.t
  -> target:atomic_replace_recovery_target
  -> string
  -> (unit, capability_write_error) result

(** Exclusive creation is physically separate from replacement and has no
    recovery-obligation argument. Once the public leaf is created it is never
    unlinked by failure cleanup. *)
val create_capability_file_exclusive
  :  parent:Eio.Fs.dir_ty Eio.Path.t
  -> leaf:string
  -> permissions:int
  -> string
  -> (unit, capability_write_error) result

val capability_write_error_to_string : capability_write_error -> string
val capability_write_operation_to_string : capability_write_operation -> string
val capability_write_stage_to_string : capability_write_stage -> string

val capability_write_target_effect_to_string
  :  capability_write_target_effect
  -> string

val capability_write_cause_to_string : capability_write_cause -> string
val capability_write_failure_to_string : capability_write_failure -> string

val sync_directory_capability
  :  _ Eio.Path.t
  -> (unit, capability_directory_sync_error) result

val capability_directory_sync_error_to_string
  :  capability_directory_sync_error
  -> string

(** [true] iff [name] matches the canonical [.atomic_*.tmp] pattern produced by
    this module. Exposed for tests and recovery sweeps. *)
val is_atomic_orphan_name : string -> bool

type atomic_orphan_cleanup_scope =
  | Directory_only
  | Directory_and_immediate_subdirectories

type atomic_orphan_cleanup_operation =
  | Inspect_cleanup_root
  | Read_cleanup_directory
  | Inspect_orphan
  | Create_recovery_directory
  | Sync_recovery_parent
  | Link_preserved_orphan
  | Verify_preserved_orphan
  | Sync_preserved_orphan
  | Sync_recovery_directory
  | Delete_empty_orphan
  | Delete_preserved_source
  | Sync_source_directory
  | Close_cleanup_descriptor

type atomic_orphan_cleanup_cause =
  | Unix_failure of Unix.error * string * string
  | Sys_failure of string
  | Unexpected_file_kind of Unix.file_kind
  | Outside_ownership_root of { ownership_root : string }
  | Identity_changed
  | Other_failure of exn

type atomic_orphan_cleanup_failure =
  { operation : atomic_orphan_cleanup_operation
  ; path : string
  ; cause : atomic_orphan_cleanup_cause
  }

type atomic_orphan_cleanup_report =
  { inspected : int
  ; deleted : int
  ; preserved : int
  ; failures : atomic_orphan_cleanup_failure list
  }

val atomic_orphan_cleanup_failure_to_string
  :  atomic_orphan_cleanup_failure
  -> string

(** No-follow orphan cleanup. [Directory_only] is bounded by the named
    staging inventory. The broader scope also scans real immediate child
    directories. Every failed mutation or unexpected orphan-shaped entry is
    returned in the typed report. The caller must own stable directory
    identities and quiesce the matching temp namespace; see
    {!Atomic_write.cleanup_atomic_orphans} for the OCaml 5.4 dirfd
    limitation. *)
val cleanup_atomic_orphans
  :  ownership_root:string
  -> base_path:string
  -> scope:atomic_orphan_cleanup_scope
  -> unit
  -> atomic_orphan_cleanup_report

(** Append string to file. *)
val append_file : string -> string -> unit

(** Check if file exists. *)
val file_exists : string -> bool

(** Return file size or None *)
val file_size : string -> int option

(** Return file mtime or None *)
val file_mtime : string -> float option

(** Rename file. *)
val rename : string -> string -> unit

(** [rename_if_exists ~src ~dst] renames [src] to [dst], returning [true]
    on success and [false] if [src] did not exist. Other I/O errors
    propagate as [Sys_error] (Eio.Io is normalized internally, matching
    {!rename}). Both runtime paths recognize the missing-source case
    via typed catches ([Eio.Fs.Not_found] / verified [Sys.file_exists])
    rather than substring matching on the libc message. *)
val rename_if_exists : src:string -> dst:string -> bool

(** Remove a file, symlink, or directory tree without invoking a shell.
    Missing paths are ignored.  Symlinks are unlinked, not followed. *)
val remove_tree : string -> unit

(** Get realpath. *)
val realpath : string -> string

(** [realpath] with a fallback for paths that do not (fully) exist on
    disk: walks up the directory tree until an ancestor resolves, then
    reconstructs the remaining suffix onto the resolved ancestor.  This
    canonicalizes symlinked spellings (e.g. [/tmp] vs [/private/tmp] on
    macOS) even when the leaf directories have not been created yet, so
    two spellings of the same location compare equal.  Total: a path
    with no resolvable ancestor is returned reconstructed as-is. *)
val realpath_lenient : string -> string

(** Create directory recursively. *)
val mkdir_p : string -> unit

(** [mkdir_p_memoized path] is [mkdir_p] but skips the stat/mkdir
    syscalls on every call after the first for the same [path].
    Use on hot append paths (jsonl writers, ledger appends) where the
    same dir is touched many times per second. RFC-0162 §3.1.

    The cache caches only the *fact* of dir existence; no fd is held.
    External processes that delete the dir after first call will see
    silent skip — acceptable for [.masc/] (self-owned). *)
val mkdir_p_memoized : string -> unit

(** Forget the memoized existence of [path]. Use when application-owned code
    removes a directory that a later memoized writer may recreate. *)
val invalidate_mkdir_memo : string -> unit

(** Reset the [mkdir_p_memoized] cache. Test-only — production code
    relies on process-lifetime persistence. *)
val reset_mkdir_memo_for_testing : unit -> unit

(** Load JSONL file as list of JSON values.
    Malformed lines are logged and dropped. *)
val load_jsonl : string -> Yojson.Safe.t list

(** Load JSONL file, returning parsed values and count of malformed lines.
    Logs each malformed line with the provided source label. Use when the caller needs
    to surface degraded state (e.g. dashboard malformed_lines field). *)
val load_jsonl_diagnostics : string -> Yojson.Safe.t list * int

(** Parse pre-read string lines as JSONL, returning parsed values and
    malformed count.  [source] is used in log messages.
    Use when lines come from tail-readers or non-file sources. *)
val number_jsonl_lines : string list -> (int * string) list
(** The non-blank rows, trimmed, each with the 1-based number the malformed
    warning would print for it. Blank rows are dropped and take no number, so
    the number is the printed JSONL row an operator sees in [cat -n]. *)

val parse_jsonl_line : source:string -> line_no:int -> string -> Yojson.Safe.t option
(** Parse one trimmed row. A malformed row warns on stderr, naming [source]
    and [line_no], and returns [None] - the same warning {!parse_jsonl_lines}
    prints. Use it with {!number_jsonl_lines} to walk rows without parsing the
    ones the walk never reaches. *)

val parse_jsonl_lines : source:string -> string list -> Yojson.Safe.t list * int

(** Stream JSONL line-by-line via [Eio.Buf_read.lines] when the global
    fs is registered, falling back to a raw-line iterator over the
    Stdlib channel otherwise (both branches share the same [line_no]
    counting).  [line_no] is the 1-based index of {b non-blank}
    JSONL rows — blank lines are skipped, malformed lines emit a
    stderr warning and are skipped but {i still consume} the index
    so the counter tracks the printed JSONL row number rather than
    the count of successfully parsed values.  Use when the file may
    be too large to materialize as a list, e.g. audit/metrics JSONL
    on HTTP hot paths.

    Returns [init] when [path] is missing (consistent with
    {!load_jsonl}); raises [Sys_error] on read failures of an
    existing file. *)
val fold_jsonl_lines
  :  init:'acc
  -> f:('acc -> line_no:int -> Yojson.Safe.t -> 'acc)
  -> string
  -> 'acc

(** [fold_appended_lines ~path ~from ~init ~f] folds [f] over the raw
    non-blank, newline-terminated lines whose bytes start at offset
    [from], returning [(acc, boundary)] where [boundary] is the offset
    just past the last ['\n'] consumed.

    Contract for incremental readers over append-only JSONL stores:
    cache [(boundary, acc)] per path and pass the cached [boundary] as
    [from] on the next call — only the appended delta is re-read.
    Bytes after the last ['\n'] (a partially flushed line) are neither
    folded nor included in [boundary], so they are re-read once the
    writer completes the line. A [from] outside [0, file_size] (file
    truncated or rotated) restarts the scan from byte 0. Returns
    [(init, 0)] when [path] does not exist. Lines are raw strings;
    callers parse (and decide how to surface malformed rows). *)
(** [read_slice ~path ~from ~len] returns the byte slice
    [[from, from+len)] of the file, clamped to its current size.
    Missing file or empty clamped range returns [""]. Callers bound
    [len], so one call never scales with file size (RFC-0228 P1). *)
val read_slice : path:string -> from:int -> len:int -> string

val fold_appended_lines
  :  path:string
  -> from:int
  -> init:'acc
  -> f:('acc -> string -> 'acc)
  -> 'acc * int

module Private_jsonl_slice : sig
  type t =
    { bytes : string
    ; end_offset : int
    }

  type error =
    | Negative_offset of int
    | Missing_file_after_offset of int
    | Offset_beyond_end of
        { offset : int
        ; end_offset : int
        }
    | Offset_not_at_row_boundary of int
    | Incomplete_tail of int
    | Io_failed of exn

  val error_to_string : error -> string
end

type durable_append_operation =
  | Write
  | Append_fsync
  | Rollback_truncate
  | Rollback_fsync

type durable_append_failure =
  | Unix_error of
      { operation : durable_append_operation
      ; error : Unix.error
      ; function_name : string
      ; argument : string
      }
  | No_write_progress

type durable_append_error =
  { append_failure : durable_append_failure
  ; rollback_failures : durable_append_failure list
  }

module Private_jsonl_cursor : sig
  (** Durable identity of a private JSONL store. The file identity is part of
      the cursor so an atomic rewrite cannot be mistaken for an append-only
      continuation at the same byte offset. *)
  type t

  val equal : t -> t -> bool
end

type private_jsonl_snapshot =
  { bytes : string
  ; cursor : Private_jsonl_cursor.t
  }

type private_jsonl_transaction_operation =
  | Create_parent_directory
  | Canonicalize_parent_directory
  | Inspect_stable_lock
  | Open_stable_lock
  | Set_stable_lock_permissions
  | Sync_stable_lock_parent
  | Acquire_stable_lock
  | Read_stable_lock_state
  | Write_stable_lock_state
  | Sync_stable_lock
  | Close_stable_lock
  | Open_transaction_data
  | Set_transaction_data_permissions
  | Inspect_transaction_data
  | Inspect_transaction_path
  | Read_transaction_data
  | Close_transaction_data
  | Create_rewrite_stage
  | Set_rewrite_stage_permissions
  | Write_rewrite_stage
  | Sync_rewrite_stage
  | Close_rewrite_stage
  | Rename_rewrite_stage
  | Sync_rewrite_parent
  | Inspect_rewritten_data
  | Remove_rewrite_stage
  | Truncate_transaction_data
  | Sync_transaction_data

type private_jsonl_operation_failure =
  { operation : private_jsonl_transaction_operation
  ; exception_ : exn
  ; backtrace : Printexc.raw_backtrace
  }

type ('value, 'error) private_file_transaction_outcome =
  | Private_file_succeeded of 'value
  | Private_file_succeeded_with_cleanup_failure of
      { value : 'value
      ; cleanup_failure : private_jsonl_operation_failure
      }
  | Private_file_failed of 'error
  | Private_file_failed_with_cleanup_failure of
      { error : 'error
      ; cleanup_failure : private_jsonl_operation_failure
      }
(** Result and descriptor-settlement outcome of a blocking private-file
    transaction. A cleanup failure never replaces the primary value/error, and
    a successful durable effect remains distinguishable from a primary failure
    so callers cannot retry it accidentally. Exceptions documented by the
    underlying operation still propagate. *)

val private_jsonl_operation_failure_to_string :
  private_jsonl_operation_failure -> string

(** Read the exact complete JSONL bytes in [[from, end_offset)] while holding
    the same per-path in-process mutex and a cross-process read lock used by
    durable private JSONL writers. A missing file at offset zero is the empty
    stream; every other cursor mismatch is explicit. Descriptor-close failure
    preserves the successful slice or primary read error. *)
val read_private_jsonl_slice_locked_result :
  string ->
  from:int ->
  ( Private_jsonl_slice.t
  , Private_jsonl_slice.error )
  private_file_transaction_outcome

module Private_jsonl_rows : sig
  type t =
    | Rows_missing  (** No file at the path. *)
    | Rows_present of
        { rows : string
              (** Bytes [[0, rows_end)]: every ['\n']-terminated row, in order. *)
        ; rows_end : int  (** Offset just past the last ['\n']; [0] for an empty store. *)
        ; end_offset : int
              (** File length under the lock. [rows_end < end_offset] means a
                  final fragment with no ['\n'] follows the rows: an append
                  that never completed. *)
        }

  type error = Io_failed of exn

  val error_to_string : error -> string
end

(** Read a private JSONL store through the writer's own framing rule
    ({!append_private_jsonl_durable_locked_result} refuses to append after a
    tail without ['\n']), while holding the same per-path in-process mutex and
    a shared cross-process lock the durable writer takes exclusively — so no
    append is in progress while the bytes are read, and the read runs in a
    systhread when the caller is an Eio fiber. The complete rows come back
    as bytes; a torn tail is reported by offset, never returned. Unlike
    {!read_private_jsonl_slice_locked_result}, a missing file is its own
    value, not the empty stream. *)
val read_private_jsonl_rows_locked_result :
  string ->
  ( Private_jsonl_rows.t
  , Private_jsonl_rows.error )
  private_file_transaction_outcome

type private_jsonl_transaction_success =
  | Snapshot_succeeded of private_jsonl_snapshot
  | Cursor_succeeded of Private_jsonl_cursor.t
  | Cursor_precondition_succeeded of Private_jsonl_cursor.t

type private_jsonl_transaction_primary =
  | Transaction_succeeded of private_jsonl_transaction_success
  | Transaction_failed of private_jsonl_transaction_error

and private_jsonl_transaction_error =
  | Stable_lock_contended of { lock_path : string }
  | Unexpected_stable_lock_permissions of
      { path : string
      ; actual : int
      }
  | Invalid_stable_lock_state of
      { path : string
      ; observed_length : int
      }
  | Cursor_mismatch of
      { expected : Private_jsonl_cursor.t
      ; actual : Private_jsonl_cursor.t
      }
  | Unexpected_transaction_file_kind of Unix.file_kind
  | Ambiguous_transaction_file_identity of
      { path : string
      ; link_count : int
      }
  | Transaction_path_binding_changed of { path : string }
  | Incomplete_transaction_tail of { end_offset : int }
  | Invalid_transaction_suffix
  | Private_jsonl_operation_failed of private_jsonl_operation_failure
  | Rewrite_stage_failed of
      { failure : private_jsonl_operation_failure
      ; cleanup_failures : private_jsonl_operation_failure list
      }
  | Rewrite_published_durability_unknown of
      { cursor : Private_jsonl_cursor.t option
      ; failure : private_jsonl_operation_failure
      }
  | Transaction_settlement_failed of
      { primary : private_jsonl_transaction_primary
      ; cleanup_failures : private_jsonl_operation_failure list
      }
  | Transaction_append_failed of durable_append_error

type 'a private_jsonl_success_receipt =
  { value : 'a
  ; settlement_error : private_jsonl_transaction_error option
  }
(** A completed transaction value together with exact evidence that descriptor
    settlement remained incomplete. Consumers may advance from [value], but
    must observe [settlement_error]. Primary failures and mismatched success
    kinds are never converted to receipts. *)

val private_jsonl_snapshot_success_receipt :
  (private_jsonl_snapshot, private_jsonl_transaction_error) result ->
  ( private_jsonl_snapshot private_jsonl_success_receipt
  , private_jsonl_transaction_error )
  result

val private_jsonl_cursor_success_receipt :
  (Private_jsonl_cursor.t, private_jsonl_transaction_error) result ->
  ( Private_jsonl_cursor.t private_jsonl_success_receipt
  , private_jsonl_transaction_error )
  result

val private_jsonl_transaction_error_to_string :
  private_jsonl_transaction_error -> string

(** Stable sibling lock owned by the private JSONL transaction protocol. It is
    created with mode [0600] and must never be renamed or removed while the
    store may be in use. *)
val private_jsonl_lock_path : string -> string

(** Read a private JSONL store under its stable sibling lock. [after = None]
    returns the full store. [after = Some cursor] returns only bytes appended
    after that exact file identity and offset. A replacement, truncation, or
    disappearance is a typed [Cursor_mismatch], never an implicit full rescan.
    The returned cursor distinguishes a missing store from a present empty
    store. *)
val read_private_jsonl_durable_locked_result :
  string ->
  after:Private_jsonl_cursor.t option ->
  (private_jsonl_snapshot, private_jsonl_transaction_error) result

(** Process-start recovery read of a private JSONL store under its stable
    sibling lock. Behaves like {!read_private_jsonl_durable_locked_result} with
    [after = None], except that a torn tail (an incomplete final row left by a
    mid-append crash) is truncated to the last complete row and fsynced while
    the lock is held, after which reading resumes with the truncated cursor.
    Every other failure propagates unchanged. Only process-start recovery may
    use this entry point; general reads keep hard-failing on
    [Incomplete_transaction_tail]. *)
val recover_private_jsonl_durable_locked_result :
  string ->
  (private_jsonl_snapshot, private_jsonl_transaction_error) result

type private_jsonl_transaction_io_for_testing =
  { before_sync_parent : string -> unit
  ; close_fd : Unix.file_descr -> unit
  }

val read_private_jsonl_slice_locked_with_io_for_testing :
  io:private_jsonl_transaction_io_for_testing ->
  string ->
  from:int ->
  ( Private_jsonl_slice.t
  , Private_jsonl_slice.error )
  private_file_transaction_outcome

val update_private_file_durable_locked_with_io_for_testing :
  io:private_jsonl_transaction_io_for_testing ->
  string ->
  (string -> string option * 'a) ->
  ('a, durable_append_error) private_file_transaction_outcome

(** Production-path seam for deterministic stable-lock creation and descriptor
    settlement tests. *)
val read_private_jsonl_durable_locked_with_io_for_testing :
  io:private_jsonl_transaction_io_for_testing ->
  string ->
  after:Private_jsonl_cursor.t option ->
  (private_jsonl_snapshot, private_jsonl_transaction_error) result

(** Append complete newline-terminated JSONL rows iff [expected] still names
    the exact store identity and end offset observed by the caller. All
    participants must use this stable-lock transaction family for [path]; the
    sibling lock is never renamed, so atomic rewrites cannot strand an appender
    on the old inode. Lock contention and stale cursors fail explicitly without
    timeout, retry, or backoff policy. *)
val append_private_jsonl_durable_locked_at_cursor_result :
  string ->
  expected:Private_jsonl_cursor.t ->
  string ->
  (Private_jsonl_cursor.t, private_jsonl_transaction_error) result

(** Production-path seam for cancellation during descriptor settlement. *)
val append_private_jsonl_durable_locked_at_cursor_with_io_for_testing :
  io:private_jsonl_transaction_io_for_testing ->
  string ->
  expected:Private_jsonl_cursor.t ->
  string ->
  (Private_jsonl_cursor.t, private_jsonl_transaction_error) result

(** Atomically replace a complete private JSONL store iff [expected] still
    names the exact current store. The staged payload and parent directory are
    fsynced; no directory-fsync failure is suppressed. The stable sibling lock
    remains the serialization authority across the inode replacement. *)
val rewrite_private_jsonl_durable_locked_at_cursor_result :
  string ->
  expected:Private_jsonl_cursor.t ->
  string ->
  (Private_jsonl_cursor.t, private_jsonl_transaction_error) result

(** Production-path seam proving that a descriptor settlement failure while
    reading the rewrite precondition is not classified as a committed rewrite. *)
val rewrite_private_jsonl_durable_locked_at_cursor_with_io_for_testing :
  io:private_jsonl_transaction_io_for_testing ->
  string ->
  expected:Private_jsonl_cursor.t ->
  string ->
  (Private_jsonl_cursor.t, private_jsonl_transaction_error) result

(** Render a structured durable-append failure without discarding the original
    [Unix.error] or rollback failures. *)
val durable_append_error_to_string : durable_append_error -> string

(** [update_private_file_durable_locked_result path decide] serializes in-process
    callers with the shared per-path append mutex, takes a cross-process file
    lock, reads the exact existing bytes, and calls [decide]. [Some suffix]
    appends the complete suffix and fsyncs it before returning a successful
    outcome; [None] performs no write. If writing or the append fsync fails,
    the file is truncated to its original length and that rollback is fsynced.
    A failed outcome preserves the append failure and every rollback failure.
    Descriptor-close failure is returned together with the primary value or
    error, so a committed append is never misclassified as retryable. Setup,
    read, and [decide] exceptions still propagate. The file is created with
    mode [0600], and every transaction fsyncs its parent directory before
    touching payload bytes so a failed creation can be retried without silently
    skipping that durability boundary. Filesystems that reject directory fsync
    fail explicitly. The shared path mutex serializes this operation with
    cached JSONL writers without closing their already-flushed descriptors.
    When the Eio filesystem is active, the transaction and [decide] run in a
    system thread so a contended file cannot stop unrelated fibers; [decide]
    therefore must not perform Eio effects. *)
val update_private_file_durable_locked_result :
  string ->
  (string -> string option * 'a) ->
  ('a, durable_append_error) private_file_transaction_outcome

(** [rewrite_private_file_durable_locked_result path decide] is the whole-file
    replacement sibling of {!update_private_file_durable_locked_result}. It takes
    the same shared per-path append mutex, reads the exact existing bytes (empty
    string when the file is absent), and calls [decide]. [Some content] replaces
    the file's entire contents with [content] via a temp-file + atomic rename
    (see {!save_file_atomic}); [None] performs no write. Because it shares the
    append mutex, in-process appends and rewrites of the same path are
    serialized. The atomic rename leaves the original file untouched until it
    succeeds, so a failed rewrite never leaves a partially written ledger. This
    coordinates in-process writers only; unlike the append primitive it does not
    hold a whole-file lock across the read/rewrite, so a second OS process must
    not write the same path concurrently. Returns the rename failure message on
    [Error]; [decide] exceptions still propagate. *)
val rewrite_private_file_durable_locked_result :
  string -> (string -> string option * 'a) -> ('a, string) result

type private_jsonl_append_error =
  | Incomplete_jsonl_tail
  | Invalid_jsonl_suffix
  | Negative_expected_end_offset of int
  | End_offset_mismatch of
      { expected : int
      ; actual : int
      }
  | Durable_jsonl_append_failed of durable_append_error

(** Append one or more complete JSONL rows without reading the existing file.
    The operation holds the same in-process and cross-process path locks as
    {!update_private_file_durable_locked_result}, verifies only that an existing
    file ends at a newline boundary, then appends and fsyncs with rollback on
    failure. Every transaction also fsyncs the parent directory. Runtime cost
    is proportional to [suffix], not to historical file size. From an Eio
    fiber, the entire blocking lock/write/fsync transaction runs in a system
    thread, including directory creation and in-process mutex acquisition, so
    one contended file cannot stop unrelated fibers. Non-Eio callers execute
    the same transaction directly. Descriptor-close failure is returned with
    the primary commit or rejection. *)
val append_private_jsonl_durable_locked_with_end_offset_result :
  string ->
  string ->
  (int, private_jsonl_append_error) private_file_transaction_outcome

(** Append only when the file's locked byte length is exactly
    [expected_end_offset]. A stale writer receives [End_offset_mismatch] and
    writes no bytes. The successful result is the committed newline-end byte
    offset. *)
val append_private_jsonl_durable_locked_at_end_offset_result :
  string ->
  expected_end_offset:int ->
  string ->
  (int, private_jsonl_append_error) private_file_transaction_outcome

(** As {!append_private_jsonl_durable_locked_with_end_offset_result}, discarding
    the committed newline-end byte offset. *)
val append_private_jsonl_durable_locked_result :
  string ->
  string ->
  (unit, private_jsonl_append_error) private_file_transaction_outcome

val append_private_jsonl_durable_locked_at_end_offset_with_io_for_testing :
  io:private_jsonl_transaction_io_for_testing ->
  string ->
  expected_end_offset:int ->
  string ->
  (int, private_jsonl_append_error) private_file_transaction_outcome

val private_jsonl_append_error_to_string : private_jsonl_append_error -> string

type durable_append_io_for_testing =
  { write : Unix.file_descr -> bytes -> int -> int -> int
  ; ftruncate : Unix.file_descr -> int -> unit
  ; fsync : Unix.file_descr -> unit
  }

(** Direct fd-level seam for deterministic partial-write and rollback tests.
    Production code uses the same implementation with [Unix] operations. *)
val append_fd_durable_for_testing :
  io:durable_append_io_for_testing ->
  fd:Unix.file_descr ->
  original_length:int ->
  string ->
  (unit, durable_append_error) result

(** Append JSON value as line to JSONL file.

    Backed by a process-local per-path fd cache (RFC-0162 §3.4).
    Each path keeps one cached [out_channel] reused across appends;
    cross-domain serialization is provided by the same per-path
    mutex registry as [append_file_unix], preserving RFC-0108 §3.2's
    Record-interleave-0 guarantee. The cache is bounded by
    [fd_cache_max=32] with LRU eviction; [close_all_cached_writers]
    is registered at [at_exit]. *)
val append_jsonl : string -> Yojson.Safe.t -> unit

(** [append_jsonl_batch path jsons] writes multiple JSON entries to [path]
    in a single lock+flush cycle. More efficient than calling [append_jsonl]
    repeatedly when batching pending entries. No-op if [jsons] is empty. *)
val append_jsonl_batch : string -> Yojson.Safe.t list -> unit

(** [invalidate_cached_writer path] drops the cached [append_jsonl]
    writer for [path] (a no-op if none is cached). Call it after
    replacing the inode at [path] with [save_file_atomic]: the cached
    [O_APPEND] channel still points at the pre-rename inode, so without
    this a later [append_jsonl] would write to the orphaned file.
    Serialization with concurrent [append_jsonl] calls is handled by the
    same per-path append mutex used by the append path. *)
val invalidate_cached_writer : string -> unit

(** Drop and close every cached writer. Test-only — production
    relies on process-lifetime persistence and [at_exit] drain. *)
val reset_fd_cache_for_testing : unit -> unit

(** Lease the cached writer directly. Test-only — production callers
    should use {!append_jsonl} / {!append_jsonl_batch} so directory
    creation, HOME guards, and per-path write serialization stay composed
    at the public append boundary. *)
val with_cached_writer_for_testing : string -> (out_channel -> 'a) -> 'a
module Capability_head = Capability_head
