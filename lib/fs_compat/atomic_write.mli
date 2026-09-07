(** Atomic replacement with process-restart sync and orphan recovery.

    The strict contract is
    [tmp → Unix.fsync(tmp) → rename → Unix.fsync(parent dir)]. Successful
    return means both Unix sync calls returned successfully and supports
    process-restart recovery. It does not claim hardware/power-loss
    persistence and does not use Darwin [F_FULLFSYNC].

    Crash recovery is provided by [cleanup_atomic_orphans], a boot-time sweep
    for canonical [.atomic_*.tmp] files left behind when the owning process was
    SIGKILL'd after creating its temp file.

    The [save_file] primitive is injected so this module stays free of
    [Fs_compat]'s Eio bridge; it must be a blocking writer, because the
    replacement runs as one blocking job (a system thread inside Eio) from
    temp-file creation to the parent directory fsync. Recovery uses only
    typed [Unix] operations and returns every failure to the caller. *)

(** [save_file_atomic ~save_file path content] writes [content] to
    a temp file in [path]'s directory, fsyncs the tmp, renames it
    over [path], and best-effort fsyncs the parent directory.

    Returns [Ok ()] on success or [Error msg] when temp-file creation,
    writing, fsync, or rename fails (an existing tmp is cleaned up). Re-raises
    [Eio.Cancel.Cancelled] after cleaning up the tmp — cancellation
    must not be swallowed (RFC-0143). *)
val save_file_atomic
  :  save_file:(string -> string -> unit)
  -> string
  -> string
  -> (unit, string) Result.t

type atomic_replace_failure_stage =
  | Before_rename
  | After_rename

type atomic_replace_failure =
  { path : string
  ; stage : atomic_replace_failure_stage
  ; exception_ : exn
  ; backtrace : Printexc.raw_backtrace
  }

val atomic_replace_failure_to_string : atomic_replace_failure -> string

val save_file_atomic_strict_staged
  :  save_file:(string -> string -> unit)
  -> string
  -> string
  -> (unit, atomic_replace_failure) Result.t
(** Strict atomic replacement that preserves whether failure occurred before
    or after the target rename became visible. Payload [Unix.fsync] is
    mandatory before rename and parent-directory [Unix.fsync] is mandatory
    afterward. Unlike the compatibility wrapper below, cancellation is
    returned with its original backtrace so a transaction owner can repair
    publication state before re-raising it. *)

val write_file_atomic_strict_staged
  :  string
  -> write:(out_channel -> unit)
  -> (unit, atomic_replace_failure) Result.t
(** Stream bytes into the replacement using the same strict staged protocol.
    [write] runs synchronously in the blocking replacement job and must not
    perform Eio effects, close the channel, or retain it after returning.
    The writer closes the channel before syncing and publishing the file.
    Callback exceptions, including cancellation, retain their failure stage
    and original backtrace. *)

(** Strict sibling of {!save_file_atomic}. Payload or parent-directory
    descriptor/fsync failure is returned as [Error] instead of being treated
    as best effort. *)
val save_file_atomic_strict
  :  save_file:(string -> string -> unit)
  -> string
  -> string
  -> (unit, string) Result.t

module Atomic_replace_for_testing : sig
  val save_file_atomic_strict_staged
    :  ?sync_file:(string -> unit)
    -> sync_parent:(string -> unit)
    -> save_file:(string -> string -> unit)
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

type atomic_replace_recovery_target
type atomic_replace_recovery_target_error
type publication_recovery_reconciliation_report = Capability_recovery_reconciler.report

type publication_recovery_record_area =
  | Publication_recovery_active
  | Publication_recovery_owned
  | Publication_recovery_forensic

type publication_recovery_source_state =
  | Publication_recovery_prepared_source
  | Publication_recovery_bound_source

type publication_recovery_prepared_outcome_kind =
  | Publication_recovery_prepared_unmaterialized
  | Publication_recovery_prepared_allowed_root_mismatch
  | Publication_recovery_prepared_parent_mismatch
  | Publication_recovery_prepared_unbound_stage_preserved

type publication_recovery_bound_outcome_kind =
  | Publication_recovery_bound_stage_absent
  | Publication_recovery_bound_allowed_root_mismatch
  | Publication_recovery_bound_parent_mismatch
  | Publication_recovery_bound_stage_mismatch
  | Publication_recovery_bound_stage_preserved

type publication_recovery_reconciliation_row_kind =
  | Publication_recovery_unexpected_lane_entry
  | Publication_recovery_missing_lane_entry
  | Publication_recovery_lane_entry_unavailable
  | Publication_recovery_area_inventory_unavailable
  | Publication_recovery_source_transition_capabilities_unavailable
  | Publication_recovery_prepared_reconciled of
      publication_recovery_prepared_outcome_kind
  | Publication_recovery_bound_reconciled of
      publication_recovery_bound_outcome_kind
  | Publication_recovery_existing_forensic_record of
      publication_recovery_source_state
  | Publication_recovery_conflicting_source_records
  | Publication_recovery_invalid_record_name
  | Publication_recovery_unexpected_record_kind
  | Publication_recovery_missing_record_entry
  | Publication_recovery_record_entry_unavailable
  | Publication_recovery_corrupt_record_preserved
  | Publication_recovery_record_observation_failed
  | Publication_recovery_record_transition_failed
  | Publication_recovery_record_scope_release_failed
  | Publication_recovery_owner_store_release_failed
  | Publication_recovery_owner_store_unavailable
  | Publication_recovery_owner_inventory_unavailable

(** Typed, order-preserving projection of report rows for callers that need to
    branch without parsing diagnostics. Corrupt raw payloads remain only in the
    forensic-file SSOT; reports retain their byte count and SHA-256 identity.
    Exact identities, exceptions, and transition errors remain retained by the
    opaque report and its lane-block evidence. *)
val publication_recovery_reconciliation_report_row_kinds
  :  publication_recovery_reconciliation_report
  -> publication_recovery_reconciliation_row_kind list

val publication_recovery_reconciliation_report_to_string
  :  publication_recovery_reconciliation_report
  -> string

(** Build the immutable recovery locator projection used by
    [replace_capability_file]. The allowed-root identity is caller-certified;
    the final parent identity and initial no-follow target observation are
    captured under the publication mutation lease. Every component and the
    target permissions are validated before a target value is returned. *)
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

type capability_write_operation =
  | Atomic_replace_operation
  | Create_exclusive_operation

type capability_write_stage =
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

type capability_write_target_effect =
  | Target_unchanged
  | Target_created
  | Target_created_incomplete
  | Target_replaced
  | Target_state_unknown

type capability_write_operation_failure =
  { exception_ : exn
  ; backtrace : Printexc.raw_backtrace
  }

type capability_write_payload_failure =
  { exception_ : exn
  ; backtrace : Printexc.raw_backtrace
  ; bytes_written : int
  }

type capability_write_cause =
  | Invalid_leaf of string
  | Invalid_recovery_target of atomic_replace_recovery_target_error
  | Mutation_contended
  | Posix_descriptor_unavailable
  | Unexpected_resource_kind of Eio.File.Stat.kind
  | Resource_identity_unavailable
  | Resource_identity_changed
  | Payload_write_failed of capability_write_payload_failure
  | Operation_failed of capability_write_operation_failure

type capability_write_failure =
  { stage : capability_write_stage
  ; cause : capability_write_cause
  }

type capability_recovery_phase =
  | Recovery_validate_owner
  | Recovery_open_registry
  | Recovery_open_store
  | Recovery_prepare
  | Recovery_preserve_unbound
  | Recovery_bind
  | Recovery_discharge_prepared
  | Recovery_discharge_bound

type capability_recovery_removal_transition =
  | Recovery_discharge_active
  | Recovery_discharge_owned
  | Recovery_active_to_owned
  | Recovery_active_to_forensic
  | Recovery_owned_to_forensic

type capability_recovery_effect =
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

type capability_recovery_failure

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

type capability_recovery_access_failure = Recovery_access_not_available

type capability_write_primary_failure =
  | Write_primary_failure of capability_write_failure
  | Recovery_primary_failure of capability_recovery_failure
  | Recovery_access_primary_failure of capability_recovery_access_failure

type capability_write_cleanup_failure =
  | Write_cleanup_failure of capability_write_failure
  | Recovery_cleanup_failure of capability_recovery_failure

type capability_write_error =
  { operation : capability_write_operation
  ; target_effect : capability_write_target_effect
  ; primary_failure : capability_write_primary_failure
  ; cleanup_failures : capability_write_cleanup_failure list
  }

type capability_directory_sync_error =
  { failure : capability_write_failure
  ; cleanup_failures : capability_write_failure list
  }

type publication_recovery_fixture_error

val publication_recovery_fixture_error_to_string
  :  publication_recovery_fixture_error
  -> string

type capability_write_cancellation =
  { operation : capability_write_operation
  ; target_effect : capability_write_target_effect
  ; interrupted_primary_failure : capability_write_primary_failure option
  ; interrupted_recovery : capability_recovery_failure option
  ; cleanup_failures : capability_write_cleanup_failure list
  }

exception Capability_write_cancelled of exn * capability_write_cancellation

(** Durable replacement below an already-open target-parent capability.
    Recovery access and the immutable target projection are mandatory. A
    durable Prepared obligation precedes exact staging-directory creation; a
    durable Bound obligation precedes payload creation. The Bound obligation
    is discharged only after rename, staging-directory sync and removal, and
    target-parent sync all complete. No target tree is scanned during failure
    handling. [Out_of_memory], [Stack_overflow], [Sys.Break], and
    [Eio.Cancel.Cancelled] are re-raised with their original backtrace rather
    than entering the typed write-error channel. *)
val replace_capability_file
  :  recovery:Publication_recovery_access.t
  -> parent:Eio.Fs.dir_ty Eio.Path.t
  -> target:atomic_replace_recovery_target
  -> string
  -> (unit, capability_write_error) result

(** Exclusive creation is physically separate from replacement and has no
    recovery-obligation argument. Once the public leaf is created it is never
    unlinked by failure cleanup. Fatal runtime exceptions and cancellation
    follow the same re-raise contract as {!replace_capability_file}. *)
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
(** Fsync an already-open directory capability. Descriptor acquisition,
    fsync, and explicit close failures are preserved; cancellation propagates
    after the system-thread operation is observed. *)

val capability_directory_sync_error_to_string
  :  capability_directory_sync_error
  -> string

module Capability_write_for_testing : sig
  type replace_dispatch_fault

  val replace_dispatch_fault
    :  stage:capability_write_stage
    -> exception_:exn
    -> replace_dispatch_fault

  val remove_staging_payload_before_publish
    :  unit
    -> replace_dispatch_fault

  val with_replace_dispatch_fault
    :  replace_dispatch_fault
    -> (unit -> 'a)
    -> 'a

  type resource_scope_callback =
    | Return_completed_rows of string list
    | Cancel_callback of exn

  type resource_scope_evidence =
    | Returned_rows of
        { completed_rows : string list
        ; release_failure : exn option
        }
    | Cancelled_callback of
        { reason : exn
        ; release_failure : exn option
        }
    | Raised_callback of
        { exception_ : exn
        ; release_failure : exn option
        }

  type cleanup_body =
    | Return_cleanup_value of string
    | Raise_cleanup_body of exn
    | Cancel_cleanup_body of exn

  type observed_cleanup_failure =
    { exception_ : exn
    ; backtrace : Printexc.raw_backtrace
    }

  type cleanup_evidence =
    | Cleanup_returned of string
    | Cleanup_failed_without_cancellation of
        { body : observed_cleanup_failure option
        ; cleanup : observed_cleanup_failure
        }
    | Cancellation_primary_with_cleanup_failure of
        { body : observed_cleanup_failure option
        ; cancellation : observed_cleanup_failure
        ; cleanup : observed_cleanup_failure
        }
    | Body_failure_during_cancellation of
        { body : observed_cleanup_failure
        ; cancellation : observed_cleanup_failure
        }
    | Cancellation_primary of observed_cleanup_failure
    | Cleanup_boundary_raised of observed_cleanup_failure

  type single_borrow_evidence =
    | Single_borrow_balance of
        { during_borrow : int
        ; after_release : int
        ; close_completed : bool
        }
    | Single_borrow_rejected
    | Single_borrow_invariant of invariant_violation
    | Single_borrow_raised of observed_cleanup_failure

  and invariant_violation =
    | Borrow_count_underflow
    | Borrow_count_overflow
    | Closing_without_active_borrows
    | Closed_with_active_borrows of int
    | Closed_without_drain_signal
    | Drain_signal_already_resolved
    | Discovery_settled_twice
    | Discovery_finished_outside_running
    | Owner_inventory_owner_not_running of string
    | Reconciliation_owner_not_running of string
    | Owner_generation_settled_twice of string
    | Owner_generation_settled_before_terminal of string
    | Health_counter_underflow
    | Health_counter_overflow
    | Cleanup_body_outcome_lost

  val run_publication_recovery_resource_scope
    :  callback:resource_scope_callback
    -> release_failure:exn option
    -> resource_scope_evidence

  val run_publication_recovery_cleanup_boundary
    :  body:cleanup_body
    -> cleanup_failure:exn option
    -> cleanup_evidence

  val single_publication_recovery_borrow_balance
    :  registry:Publication_recovery_access.registry
    -> owner:string
    -> (single_borrow_evidence, Publication_recovery_access.lane_open_error) result

  val publication_recovery_stage_name : Uuidm.t -> string

  val replace_capability_file
    :  before_stage:(capability_write_stage -> unit)
    -> recovery:Publication_recovery_access.t
    -> parent:Eio.Fs.dir_ty Eio.Path.t
    -> target:atomic_replace_recovery_target
    -> string
    -> (unit, capability_write_error) result

  val create_capability_file_exclusive
    :  before_stage:(capability_write_stage -> unit)
    -> parent:Eio.Fs.dir_ty Eio.Path.t
    -> leaf:string
    -> permissions:int
    -> string
    -> (unit, capability_write_error) result

  val seed_prepared_publication_recovery
    :  registry:Publication_recovery_access.registry
    -> owner:string
    -> operation_id:Uuidm.t
    -> allowed_root_path:string
    -> allowed_root_device:int64
    -> allowed_root_inode:int64
    -> parent_components:string list
    -> parent_device:int64
    -> parent_inode:int64
    -> target_leaf:string
    -> permissions:int
    -> (unit, publication_recovery_fixture_error) result

  val seed_bound_publication_recovery
    :  registry:Publication_recovery_access.registry
    -> owner:string
    -> operation_id:Uuidm.t
    -> allowed_root_path:string
    -> allowed_root_device:int64
    -> allowed_root_inode:int64
    -> parent_components:string list
    -> parent_device:int64
    -> parent_inode:int64
    -> target_leaf:string
    -> permissions:int
    -> stage_device:int64
    -> stage_inode:int64
    -> (unit, publication_recovery_fixture_error) result

  val write_raw_publication_recovery_record
    :  registry:Publication_recovery_access.registry
    -> owner:string
    -> area:publication_recovery_record_area
    -> record_name:string
    -> raw:string
    -> (unit, publication_recovery_fixture_error) result

  val sync_directory_capability
    :  before_stage:(capability_write_stage -> unit)
    -> _ Eio.Path.t
    -> (unit, capability_directory_sync_error) result
end

(** [true] iff [name] matches the canonical [.atomic_*.tmp] shape produced by
    this module. Exposed so recovery sweeps do not re-derive the filename
    contract — #10205 finding 2. *)
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

(** #10130: no-follow boot-time cleanup for canonical [.atomic_*.tmp] orphans.

    [Directory_only] scans exactly [base_path].
    [Directory_and_immediate_subdirectories] additionally scans only real
    immediate child directories; symbolic links are never followed.

    [ownership_root] is the canonical process-owned ancestor of [base_path].
    Every existing component from that root through [base_path] is inspected
    with [lstat] and must be a real directory. A symbolic-link ancestor, a
    non-directory component, or a lexical path outside [ownership_root]
    produces a typed failure before the inventory is read.

    The caller must keep every scanned directory identity process-owned and
    stable, and must ensure no writer creates a matching atomic-temp name
    concurrently. Unrelated entries may change. OCaml 5.4's portable [Unix]
    API has no dirfd-relative [openat]/[unlinkat] operations, so the
    implementation validates inode identity immediately before each mutation
    but cannot make concurrent replacement of an intermediate path component
    atomic. Server startup and the dedicated Keeper staging lifecycle provide
    this ownership boundary.

    Zero-byte regular files are unlinked. Non-empty regular files are
    preserved without overwrite under
    [<base_path>/.recovered/{root,children/<child>}/]. The preservation path
    retains source provenance, uses a hard-link-then-unlink protocol, and
    fsyncs both directory sides. Orphan-shaped non-regular entries and every
    filesystem failure remain in [report.failures]; cancellation is re-raised.
    A missing [base_path] is an empty inventory, not a failure. *)
val cleanup_atomic_orphans
  :  ownership_root:string
  -> base_path:string
  -> scope:atomic_orphan_cleanup_scope
  -> unit
  -> atomic_orphan_cleanup_report
