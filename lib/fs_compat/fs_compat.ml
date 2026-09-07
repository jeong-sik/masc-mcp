(** Filesystem Compatibility Layer - Eio-native I/O with fallback

    Provides a unified filesystem API for gradual migration from
    blocking Unix I/O to Eio.Path operations.

    Usage:
    1. At server startup: [Fs_compat.set_fs (Eio.Stdenv.fs env)]
    2. In code: [Fs_compat.load_file path] instead of [open_in ...]

    When fs is not set (non-Eio contexts), falls back to blocking Unix I/O.
    This allows incremental migration without changing all call sites at once.

    @since 2026-02 - Keeper Emergent Identity v2.0
*)

open Fs_compat_internal

module Atomic_orphan_size_class = Atomic_orphan_size_class
module Capability_exact_read = Capability_exact_read

(** Global fs — WORM Atomic (write-once at startup, read from any domain).
    Using Atomic.t is required for OCaml 5 multi-domain safety:
    Executor_pool workers run on a separate domain and read this value. *)
let global_fs : Eio.Fs.dir_ty Eio.Path.t option Atomic.t = Atomic.make None

(** Set the global Eio filesystem. Call once at server startup.
    @param fs The Eio fs from [Eio.Stdenv.fs env] *)
let set_fs fs = Atomic.set global_fs (Some fs)

(** Clear the global fs (testing/shutdown only — not called in production).
    Safe because test runners and shutdown are single-fiber sequential. *)
let clear_fs () = Atomic.set global_fs None

let get_fs_opt () = Atomic.get global_fs

(** Check if Eio fs is available *)
let has_fs () = Option.is_some (Atomic.get global_fs)

type execution_context = Execution_context.t =
  | Eio_fiber
  | Non_eio

let execution_context = Execution_context.current

(** Normalize [Eio.Io] to [Sys_error] so callers only need one catch.
    Eio operations raise [Eio.Io _] on permission errors, missing files, etc.
    Stdlib I/O already raises [Sys_error], so wrapping only the Eio branch
    keeps the exception contract uniform. *)
let with_io ~path f =
  try f () with
  | Eio.Io _ as e ->
    raise (Sys_error (Printf.sprintf "%s: %s" path (Printexc.to_string e)))
;;

(* #9921: defense-in-depth write-boundary guard.

   [Env_config_core.base_path_prod_guard] stops test-time writes when path
   resolution points under HOME.  Any code that caches a stale [base_path ()]
   result or builds a HOME-relative path directly hits this gate before the
   write lands on the production ledger.

   The prod ledger observed 106 test-pattern rows
   ([hot-voter-*], [flipper], [same-voter], [judge]) written pre-#9920.
   This guard prevents regression if any new code path slips past the
   resolution guard.

   Active only for test executables (basename starts with [test_]).
   Escape hatch [MASC_TEST_ALLOW_HOME_BASE_PATH=1] matches
   [base_path_prod_guard] for the rare test that legitimately writes
   under HOME.  Reads remain unguarded — this is about preventing
   silent corruption, not restricting observability. *)
exception Test_isolation_breach of string

let test_exec_home_guard ~op path =
  let basename =
    Stdlib.Sys.executable_name |> Stdlib.Filename.basename |> String.lowercase_ascii
  in
  let is_test_exec =
    String.length basename >= 5 && String.starts_with basename ~prefix:"test_"
  in
  if not is_test_exec
  then ()
  else (
    let allow =
      match Sys.getenv_opt "MASC_TEST_ALLOW_HOME_BASE_PATH" with
      | Some v ->
        let v = String.lowercase_ascii (String.trim v) in
        String.equal v "1" || String.equal v "true" || String.equal v "yes"
      | None -> false
    in
    if allow
    then ()
    else (
      match Sys.getenv_opt "HOME" with
      | None | Some "" -> ()
      | Some home ->
        let home_norm =
          let trimmed = String.trim home in
          let len = String.length trimmed in
          if len > 1 && Char.equal trimmed.[len - 1] '/'
          then String.sub trimmed 0 (len - 1)
          else trimmed
        in
        let home_len = String.length home_norm in
        if
          home_len > 0
          && String.length path >= home_len
          && String.starts_with path ~prefix:home_norm
        then
          raise
            (Test_isolation_breach
               (Printf.sprintf
                  "#9921 %s blocked under HOME=%S (path=%S) in test executable %S. \
                   MASC_BASE_PATH override did not apply — fix the test setup or set \
                   MASC_TEST_ALLOW_HOME_BASE_PATH=1."
                  op
                  home_norm
                  path
                  (Stdlib.Filename.basename Stdlib.Sys.executable_name)))))
;;

let with_fs_or_fallback ~path ~fallback f =
  match Atomic.get global_fs with
  | Some fs ->
    (try with_io ~path (fun () -> f fs) with
     | Stdlib.Effect.Unhandled _ -> fallback ())
  | None -> fallback ()
;;

let load_file_unix (path : string) : string =
  let ic = Stdlib.open_in path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in_noerr ic)
    (fun () ->
       let len = Stdlib.in_channel_length ic in
       Stdlib.really_input_string ic len)
;;

(* The Eio path below creates with 0o644. [Stdlib.open_out] creates with
   0o666 masked by the process umask, so the same [save_file] call produced a
   different mode depending on which branch ran — and which branch runs is
   whether an Eio fs happens to be installed, not anything about the file
   (#29358). The mode is named here so both branches agree. *)
let save_file_mode = 0o644

let save_file_unix (path : string) (content : string) : unit =
  let fd =
    Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] save_file_mode
  in
  let oc = Unix.out_channel_of_descr fd in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out_noerr oc)
    (fun () -> Stdlib.output_string oc content)
;;

(* RFC-0108: per-path Stdlib.Mutex registry + fresh-fd open/close
   on every append.

   Background: prior [Append_fd_cache] LRU cached an [out_channel]
   per path and reused it across calls. Cache lookup was mutex-
   protected, but the OCaml-runtime [out_channel] buffer state is
   not domain-safe — two domains writing through the same cached
   channel corrupted records mid-line (observed 2026-05-17:
   utf-8 multibyte tears across trajectories/, keepers/*/reaction-
   ledger/, plus "}{"-concat in agent-core-events/ — total 243 live
   malformed lines).

   PR #15936 (RFC-0108 root-fix scope #1) addressed [append_jsonl]
   with its own per-path registry but left [append_file_unix] still
   pointing at the cache. This PR extends the fix to
   [append_file_unix] (and removes the now-dead [Append_fd_cache]
   module and [at_exit] hook) so the ~15 [append_file] callers
   (metrics_store_eio, workspace_utils_ops, board_core,
   keeper_chat_store, etc.) get the
   same guarantee.

   The mutex registry is shared between [append_file_unix] and
   [append_jsonl] (single [append_path_mutex_registry]) so a
   caller mixing the two helpers on a single path remains
   race-free. Per-path granularity lets appends to *different*
   files run concurrently.

   Throughput trade-off (RFC-0108 §6 performance follow-up): the
   removed cache folded three syscalls (open/output_string/close)
   into one cached output_string under 64-keeper telemetry. Fresh
   fd per call restores those three syscalls. A future domain-safe
   cache (per-domain fd, or a single-writer workspace fiber) can
   reinstate the optimization without giving up correctness. *)
let append_path_mutex_registry : (string, Stdlib.Mutex.t) Hashtbl.t =
  Hashtbl.create 32
let append_path_mutex_registry_mu = Stdlib.Mutex.create ()

let get_append_path_mutex path =
  Stdlib.Mutex.lock append_path_mutex_registry_mu;
  Fun.protect
    ~finally:(fun () -> Stdlib.Mutex.unlock append_path_mutex_registry_mu)
    (fun () ->
      match Hashtbl.find_opt append_path_mutex_registry path with
      | Some m -> m
      | None ->
        let m = Stdlib.Mutex.create () in
        Hashtbl.add append_path_mutex_registry path m;
        m)

let append_file_unix (path : string) (content : string) : unit =
  let mu = get_append_path_mutex path in
  Stdlib.Mutex.protect mu (fun () ->
    Fd_cache.with_writer path (fun oc ->
      Stdlib.output_string oc content;
      Stdlib.flush oc))
;;

let mkdir_p_unix (path : string) : unit =
  let rec ensure_dir (p : string) : unit =
    if String.equal p "" || String.equal p "." || String.equal p "/"
    then ()
    else if Stdlib.Sys.file_exists p
    then ()
    else (
      ensure_dir (Stdlib.Filename.dirname p);
      try Unix.mkdir p 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  ensure_dir path
;;

(* Whole-file reads, writes and appends of regular files block in the
   kernel for as long as the disk takes, and eio_posix runs them on the
   calling fiber: a regular file never reports "not ready", so [readv] and
   [writev] are issued inline. On the live server a 2 MB [save_file] held
   the main domain 120-130 ms and a large [load_file] 40-50 ms per call
   (2026-09-06 fiber trace, labels [with_open_out] and [with_open_in]; RFC
   main-domain-scheduler-latency §8.8). Inside an Eio fiber the three
   primitives therefore run their Unix implementation on a system thread;
   outside Eio they run it inline, as before. *)
(* The label reaches the runtime-events ring as the fiber's suspend reason
   ([Eio_unix.run_in_systhread] calls [Trace.suspend_fiber label]), and a
   trace reads a long run as "resumed from X, suspended on Y". With the file
   named, X and Y identify the code: a run between
   [fs-compat-append-file chat.jsonl] and [fs-compat-load-file memory.json]
   needs no further instrumentation to place. The basename alone keeps the
   label short and is unique enough among the stores a keeper touches. *)
let labelled operation path = operation ^ " " ^ Filename.basename path

let on_systhread ~label f =
  let result = Eio_unix.run_in_systhread ~label f in
  Eio.Fiber.check ();
  result
;;

(** Load entire file contents as string.
    Read on a system thread inside Eio, inline otherwise.
    @raises Sys_error on all I/O failures. *)
let load_file (path : string) : string =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () -> load_file_unix path)
    (fun _fs ->
       on_systhread
         ~label:(labelled "fs-compat-load-file" path)
         (fun () -> load_file_unix path))
;;

(* The write behind [save_file] and the atomic writers: the path guard on
   the fiber, the open, write and close wherever the caller runs it. The
   atomic writers run it inside their own blocking job. *)
let save_file_blocking (path : string) (content : string) : unit =
  test_exec_home_guard ~op:"save_file" path;
  save_file_unix path content
;;

(** Save string to file (overwrite).
    Written on a system thread inside Eio, inline otherwise.
    @raises Sys_error on all I/O failures. *)
let save_file (path : string) (content : string) : unit =
  test_exec_home_guard ~op:"save_file" path;
  with_fs_or_fallback
    ~path
    ~fallback:(fun () -> save_file_unix path content)
    (fun _fs ->
       on_systhread
         ~label:(labelled "fs-compat-save-file" path)
         (fun () -> save_file_unix path content))
;;

let save_file_atomic path content =
  Atomic_write.save_file_atomic ~save_file:save_file_blocking path content
;;

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

let atomic_replace_failure_to_string =
  Atomic_write.atomic_replace_failure_to_string
;;

let save_file_atomic_strict_staged path content =
  Atomic_write.save_file_atomic_strict_staged ~save_file:save_file_blocking path content
;;

let write_file_atomic_strict_staged path ~write =
  Atomic_write.write_file_atomic_strict_staged path ~write
;;

let save_file_atomic_strict path content =
  Atomic_write.save_file_atomic_strict ~save_file:save_file_blocking path content
;;

module Atomic_replace_for_testing = struct
  let save_file_atomic_strict_staged ?sync_file ~sync_parent path content =
    Atomic_write.Atomic_replace_for_testing.save_file_atomic_strict_staged
      ?sync_file
      ~sync_parent
      ~save_file:save_file_blocking
      path
      content
  ;;

  let write_file_atomic_strict_staged ?sync_file ~sync_parent path ~write =
    Atomic_write.Atomic_replace_for_testing.write_file_atomic_strict_staged
      ?sync_file
      ~sync_parent
      path
      ~write
  ;;
end

let open_atomic_temp_file ~temp_dir () =
  Atomic_write.open_atomic_temp_file ~temp_dir ()
;;

let is_capability_leaf = Capability_leaf.is_valid

type atomic_replace_recovery_target = Atomic_write.atomic_replace_recovery_target

type atomic_replace_recovery_target_error =
  Atomic_write.atomic_replace_recovery_target_error

module Publication_recovery = struct
  include Publication_recovery_access

  type lane_open_error_category =
    | Invalid_owner_category
    | Reconciliation_blocked_category
    | Store_failed_category

  let lane_open_error_category = function
    | Invalid_owner _ -> Invalid_owner_category
    | Reconciliation_blocked _ -> Reconciliation_blocked_category
    | Store_failed _ -> Store_failed_category
  ;;
end

let atomic_replace_recovery_target = Atomic_write.atomic_replace_recovery_target

let atomic_replace_recovery_target_error_to_string =
  Atomic_write.atomic_replace_recovery_target_error_to_string
;;

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

type capability_write_payload_failure = Atomic_write.capability_write_payload_failure =
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

let capability_recovery_phase_to_string =
  Atomic_write.capability_recovery_phase_to_string
;;

let capability_recovery_effect_to_string =
  Atomic_write.capability_recovery_effect_to_string
;;

let capability_recovery_failure_phase =
  Atomic_write.capability_recovery_failure_phase
;;

let capability_recovery_failure_effect =
  Atomic_write.capability_recovery_failure_effect
;;

let capability_recovery_failure_to_string =
  Atomic_write.capability_recovery_failure_to_string
;;

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

exception Capability_write_cancelled = Atomic_write.Capability_write_cancelled

let replace_capability_file = Atomic_write.replace_capability_file

let create_capability_file_exclusive =
  Atomic_write.create_capability_file_exclusive
;;

let capability_write_error_to_string = Atomic_write.capability_write_error_to_string

let capability_write_operation_to_string =
  Atomic_write.capability_write_operation_to_string
;;

let capability_write_stage_to_string = Atomic_write.capability_write_stage_to_string

let capability_write_target_effect_to_string =
  Atomic_write.capability_write_target_effect_to_string
;;

let capability_write_cause_to_string = Atomic_write.capability_write_cause_to_string
let capability_write_failure_to_string = Atomic_write.capability_write_failure_to_string
let sync_directory_capability = Atomic_write.sync_directory_capability

let capability_directory_sync_error_to_string =
  Atomic_write.capability_directory_sync_error_to_string
;;

let is_atomic_orphan_name = Atomic_write.is_atomic_orphan_name
type atomic_orphan_cleanup_scope = Atomic_write.atomic_orphan_cleanup_scope =
  | Directory_only
  | Directory_and_immediate_subdirectories

type atomic_orphan_cleanup_operation = Atomic_write.atomic_orphan_cleanup_operation =
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

type atomic_orphan_cleanup_cause = Atomic_write.atomic_orphan_cleanup_cause =
  | Unix_failure of Unix.error * string * string
  | Sys_failure of string
  | Unexpected_file_kind of Unix.file_kind
  | Outside_ownership_root of { ownership_root : string }
  | Identity_changed
  | Other_failure of exn

type atomic_orphan_cleanup_failure = Atomic_write.atomic_orphan_cleanup_failure =
  { operation : atomic_orphan_cleanup_operation
  ; path : string
  ; cause : atomic_orphan_cleanup_cause
  }

type atomic_orphan_cleanup_report = Atomic_write.atomic_orphan_cleanup_report =
  { inspected : int
  ; deleted : int
  ; preserved : int
  ; failures : atomic_orphan_cleanup_failure list
  }

let atomic_orphan_cleanup_failure_to_string =
  Atomic_write.atomic_orphan_cleanup_failure_to_string
;;

let cleanup_atomic_orphans ~ownership_root ~base_path ~scope () =
  Atomic_write.cleanup_atomic_orphans ~ownership_root ~base_path ~scope ()
;;

(** Append string to file.
    Appended on a system thread inside Eio, inline otherwise; both go
    through the per-path mutex of [append_file_unix], so two appends to one
    path from two threads stay whole.
    @raises Sys_error on all I/O failures. *)
let append_file (path : string) (content : string) : unit =
  test_exec_home_guard ~op:"append_file" path;
  with_fs_or_fallback
    ~path
    ~fallback:(fun () -> append_file_unix path content)
    (fun _fs ->
       on_systhread
         ~label:(labelled "fs-compat-append-file" path)
         (fun () -> append_file_unix path content))
;;

(** Check if file exists.
    Uses Stdlib.Sys.file_exists (works in both Eio and non-Eio contexts). *)
let file_exists (path : string) : bool =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () -> Stdlib.Sys.file_exists path)
    (fun fs ->
       try
         let _ = Eio.Path.stat ~follow:true Eio.Path.(fs / path) in
         true
       with
       | Eio.Io _ -> false)
;;

type path_kind =
  | Missing
  | Directory
  | Other

type exact_path_kind =
  | Exact_missing
  | Exact_kind of Unix.file_kind
  | Exact_unknown

let exact_path_kind ?(follow = true) (path : string) : exact_path_kind =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () ->
      try
        let stats = if follow then Unix.stat path else Unix.lstat path in
        Exact_kind stats.Unix.st_kind
      with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Exact_missing
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> Exact_unknown)
    (fun fs ->
       match Eio.Path.kind ~follow Eio.Path.(fs / path) with
       | `Not_found -> Exact_missing
       | `Directory -> Exact_kind Unix.S_DIR
       | `Fifo -> Exact_kind Unix.S_FIFO
       | `Character_special -> Exact_kind Unix.S_CHR
       | `Block_device -> Exact_kind Unix.S_BLK
       | `Regular_file -> Exact_kind Unix.S_REG
       | `Symbolic_link -> Exact_kind Unix.S_LNK
       | `Socket -> Exact_kind Unix.S_SOCK
       | `Unknown -> Exact_unknown)
;;

let path_kind ?(follow = true) (path : string) : path_kind =
  match exact_path_kind ~follow path with
  | Exact_missing -> Missing
  | Exact_kind Unix.S_DIR -> Directory
  | Exact_kind
      (Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
      | Unix.S_SOCK)
  | Exact_unknown -> Other
;;

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

let inspect_owned_directory_chain = Owned_directory_chain.inspect
let owned_directory_paths = Owned_directory_chain.paths

let owned_directory_chain_rejection_to_string =
  Owned_directory_chain.rejection_to_string
;;

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

let owned_regular_file_read_operation_to_string = function
  | Inspect_parent -> "inspect_parent"
  | Inspect_path -> "inspect_path"
  | Open_path -> "open_path"
  | Inspect_descriptor -> "inspect_descriptor"
  | Read_contents -> "read_contents"
  | Close_descriptor -> "close_descriptor"
;;

let file_kind_to_string = Owned_directory_chain.file_kind_to_string

let owned_regular_file_read_failure_to_string = function
  | Ownership_boundary_rejected { path; rejection } ->
    Printf.sprintf
      "owned file boundary rejected path=%s reason=%s"
      path
      (owned_directory_chain_rejection_to_string rejection)
  | Path_is_not_regular_file { path; kind } ->
    Printf.sprintf
      "owned file path is not a regular file path=%s kind=%s"
      path
      (file_kind_to_string kind)
  | Filesystem_identity_changed { path } ->
    Printf.sprintf "owned file identity changed during read path=%s" path
  | Owned_file_operation_failed { path; operation; cause } ->
    Printf.sprintf
      "owned file operation failed path=%s operation=%s reason=%s"
      path
      (owned_regular_file_read_operation_to_string operation)
      (Printexc.to_string cause)
;;

let owned_regular_file_read_error_to_string { failure; close_failure } =
  let primary = owned_regular_file_read_failure_to_string failure in
  match close_failure with
  | None -> primary
  | Some cause ->
    Printf.sprintf
      "%s; descriptor close also failed: %s"
      primary
      (Printexc.to_string cause)
;;

let same_file_identity (left : Unix.stats) (right : Unix.stats) =
  left.st_dev = right.st_dev && left.st_ino = right.st_ino
;;

let same_file_snapshot (left : Unix.stats) (right : Unix.stats) =
  same_file_identity left right
  && left.st_kind = right.st_kind
  && left.st_size = right.st_size
  && left.st_mtime = right.st_mtime
  && left.st_ctime = right.st_ctime
;;

type owned_regular_file_snapshot =
  { device : int
  ; inode : int
  ; owner_uid : int
  ; permissions : int
  ; file_size : int
  ; modified_at : float
  ; changed_at : float
  }

let owned_regular_file_snapshot_of_stats (stats : Unix.stats) =
  { device = stats.st_dev
  ; inode = stats.st_ino
  ; owner_uid = stats.st_uid
  ; permissions = stats.st_perm land 0o7777
  ; file_size = stats.st_size
  ; modified_at = stats.st_mtime
  ; changed_at = stats.st_ctime
  }
;;

let equal_owned_regular_file_snapshot left right =
  Int.equal left.device right.device
  && Int.equal left.inode right.inode
  && Int.equal left.owner_uid right.owner_uid
  && Int.equal left.permissions right.permissions
  && Int.equal left.file_size right.file_size
  && Float.equal left.modified_at right.modified_at
  && Float.equal left.changed_at right.changed_at
;;

let owned_file_error failure = Error { failure; close_failure = None }

let owned_file_operation_error ~path operation cause =
  owned_file_error (Owned_file_operation_failed { path; operation; cause })
;;

let reraise_current exn =
  Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())
;;

let read_exact_file_descriptor ~path fd length =
  try
    let bytes = Bytes.create length in
    let rec read offset =
      if offset = length
      then Ok (Bytes.unsafe_to_string bytes)
      else
        match Unix.read fd bytes offset (length - offset) with
        | 0 -> owned_file_error (Filesystem_identity_changed { path })
        | count -> read (offset + count)
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> read offset
    in
    read 0
  with
  | Eio.Cancel.Cancelled _ as cancellation -> reraise_current cancellation
  | cause -> owned_file_operation_error ~path Read_contents cause
;;

let load_owned_regular_file_blocking_with
    ~ownership_root ~read_descriptor path =
  let parent = Filename.dirname path in
  let inspect_parent () =
    try
      match inspect_owned_directory_chain ~ownership_root parent with
      | Ok observation -> Ok observation
      | Error rejection ->
        owned_file_error (Ownership_boundary_rejected { path; rejection })
    with
    | Eio.Cancel.Cancelled _ as cancellation -> reraise_current cancellation
    | cause -> owned_file_operation_error ~path Inspect_parent cause
  in
  let inspect_current parent_before descriptor =
    match inspect_parent () with
    | Error _ as error -> error
    | Ok Owned_directory_missing -> Ok false
    | Ok (Owned_directory parent_now) ->
      if not (same_file_identity parent_before parent_now)
      then Ok false
      else
        (try
           let current = Unix.lstat path in
           Ok
             (current.st_kind = Unix.S_REG
              && same_file_identity descriptor current)
         with
         | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
         | Eio.Cancel.Cancelled _ as cancellation -> reraise_current cancellation
         | cause -> owned_file_operation_error ~path Inspect_path cause)
  in
  match inspect_parent () with
  | Error _ as error -> error
  | Ok Owned_directory_missing -> Ok None
  | Ok (Owned_directory parent_before) ->
    let before_open =
      try Ok (Some (Unix.lstat path)) with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
      | Eio.Cancel.Cancelled _ as cancellation -> reraise_current cancellation
      | cause -> owned_file_operation_error ~path Inspect_path cause
    in
    (match before_open with
     | Error _ as error -> error
     | Ok None -> Ok None
     | Ok (Some stat) when stat.st_kind <> Unix.S_REG ->
       owned_file_error (Path_is_not_regular_file { path; kind = stat.st_kind })
     | Ok (Some before_open) ->
       let opened =
         try
           Ok
             (Unix.openfile
                path
                [ Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC ]
                0)
         with
         | Eio.Cancel.Cancelled _ as cancellation -> reraise_current cancellation
         | cause -> owned_file_operation_error ~path Open_path cause
       in
       (match opened with
        | Error _ as error -> error
        | Ok fd ->
          let result =
            match Unix.fstat fd with
            | exception cause ->
              owned_file_operation_error ~path Inspect_descriptor cause
            | descriptor
              when descriptor.st_kind <> Unix.S_REG
                   || not (same_file_identity before_open descriptor) ->
              owned_file_error (Filesystem_identity_changed { path })
            | descriptor ->
              (match inspect_current parent_before descriptor with
               | Error _ as error -> error
               | Ok false ->
                 owned_file_error (Filesystem_identity_changed { path })
               | Ok true ->
                 (match read_descriptor ~path fd descriptor with
                  | Error _ as error -> error
                  | Ok content ->
                    (match Unix.fstat fd with
                     | after_read when same_file_snapshot descriptor after_read ->
                       (match inspect_current parent_before descriptor with
                        | Error _ as error -> error
                        | Ok true -> Ok (Some content)
                        | Ok false ->
                          owned_file_error
                            (Filesystem_identity_changed { path }))
                     | _ ->
                       owned_file_error (Filesystem_identity_changed { path })
                     | exception cause ->
                       owned_file_operation_error
                         ~path
                         Inspect_descriptor
                         cause)))
          in
          let close_result =
            try Unix.close fd; Ok () with
            | Eio.Cancel.Cancelled _ as cancellation ->
              reraise_current cancellation
            | cause -> Error cause
          in
          (match close_result, result with
           | Ok (), result -> result
           | Error cause, Ok _ ->
             owned_file_operation_error ~path Close_descriptor cause
           | Error cause, Error error ->
             Error { error with close_failure = Some cause })))
;;

type owned_regular_file_contents =
  { content : string
  ; snapshot : owned_regular_file_snapshot
  }

let load_owned_regular_file_with_snapshot_blocking ~ownership_root path =
  load_owned_regular_file_blocking_with
    ~ownership_root
    ~read_descriptor:(fun ~path fd descriptor ->
      match read_exact_file_descriptor ~path fd descriptor.Unix.st_size with
      | Error _ as error -> error
      | Ok content ->
        Ok
          { content
          ; snapshot = owned_regular_file_snapshot_of_stats descriptor
          })
    path
;;

let load_owned_regular_file_with_snapshot ~ownership_root path =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () ->
      load_owned_regular_file_with_snapshot_blocking ~ownership_root path)
    (fun _fs ->
       let result =
         Eio_unix.run_in_systhread ~label:(labelled "fs-compat-load-owned-file" path) (fun () ->
           load_owned_regular_file_with_snapshot_blocking
             ~ownership_root
             path)
       in
       Eio.Fiber.check ();
       result)
;;

let load_owned_regular_file ~ownership_root path =
  load_owned_regular_file_with_snapshot ~ownership_root path
  |> Result.map (Option.map (fun contents -> contents.content))
;;

type owned_regular_file_prefix =
  { content : string
  ; file_size : int
  ; modified_at : float
  ; truncated : bool
  }

let load_owned_regular_file_prefix_blocking
    ~ownership_root ~max_bytes path =
  if max_bytes < 0
  then
    owned_file_operation_error
      ~path
      Read_contents
      (Invalid_argument "max_bytes must be non-negative")
  else
    load_owned_regular_file_blocking_with
      ~ownership_root
      ~read_descriptor:(fun ~path fd descriptor ->
        let length = min max_bytes descriptor.Unix.st_size in
        match read_exact_file_descriptor ~path fd length with
        | Error _ as error -> error
        | Ok content ->
          Ok
            { content
            ; file_size = descriptor.Unix.st_size
            ; modified_at = descriptor.Unix.st_mtime
            ; truncated = descriptor.Unix.st_size > length
            })
      path
;;

let load_owned_regular_file_prefix ~ownership_root ~max_bytes path =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () ->
      load_owned_regular_file_prefix_blocking
        ~ownership_root ~max_bytes path)
    (fun _fs ->
       let result =
         Eio_unix.run_in_systhread ~label:(labelled "fs-compat-load-owned-prefix" path) (fun () ->
           load_owned_regular_file_prefix_blocking
             ~ownership_root ~max_bytes path)
       in
       Eio.Fiber.check ();
       result)
;;

type owned_regular_file_range =
  { content : string
  ; snapshot : owned_regular_file_snapshot
  }

let load_owned_regular_file_range_blocking
    ~ownership_root ~offset ~max_bytes path =
  if offset < 0 || max_bytes < 0
  then
    owned_file_operation_error
      ~path
      Read_contents
      (Invalid_argument "offset and max_bytes must be non-negative")
  else
    load_owned_regular_file_blocking_with
      ~ownership_root
      ~read_descriptor:(fun ~path fd descriptor ->
        let available = max 0 (descriptor.Unix.st_size - offset) in
        let length = min max_bytes available in
        try
          let positioned = Unix.lseek fd offset Unix.SEEK_SET in
          if positioned <> offset
          then owned_file_error (Filesystem_identity_changed { path })
          else
            match read_exact_file_descriptor ~path fd length with
            | Error _ as error -> error
            | Ok content ->
              Ok
                { content
                ; snapshot =
                    owned_regular_file_snapshot_of_stats descriptor
                }
        with
        | Eio.Cancel.Cancelled _ as cancellation ->
          reraise_current cancellation
        | cause -> owned_file_operation_error ~path Read_contents cause)
      path
;;

let load_owned_regular_file_range
    ~ownership_root ~offset ~max_bytes path =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () ->
      load_owned_regular_file_range_blocking
        ~ownership_root ~offset ~max_bytes path)
    (fun _fs ->
       let result =
         Eio_unix.run_in_systhread ~label:(labelled "fs-compat-load-owned-range" path) (fun () ->
           load_owned_regular_file_range_blocking
             ~ownership_root ~offset ~max_bytes path)
       in
       Eio.Fiber.check ();
       result)
;;

let read_dir (path : string) : string list =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () ->
      Stdlib.Sys.readdir path
      |> Array.to_list
      |> List.sort String.compare)
    (fun fs ->
       Eio.Path.read_dir Eio.Path.(fs / path) |> List.sort String.compare)
;;

(** Load entire file contents as string, or [None] when the file is
    missing. Option-returning sibling of {!load_file} (which raises on a
    missing path). [Sys_error] from a vanished file (TOCTOU race after the
    [file_exists] check) is also mapped to [None]; other I/O failures of an
    existing file propagate as [Sys_error], matching {!load_file}. *)
let load_file_opt (path : string) : string option =
  if not (file_exists path)
  then None
  else (
    try Some (load_file path) with
    | Sys_error _ when not (file_exists path) -> None)
;;

let file_size (path : string) : int option =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () ->
      try Some (Unix.stat path).st_size with
      | Unix.Unix_error _ -> None)
    (fun _fs ->
       try Some (Eio_unix.run_in_systhread ~label:(labelled "fs-compat-file-size" path) (fun () -> (Unix.stat path).st_size)) with
       | Unix.Unix_error _ -> None)
;;

let file_mtime (path : string) : float option =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () ->
      try Some (Unix.stat path).st_mtime with
      | Unix.Unix_error _ -> None)
    (fun _fs ->
       try Some (Eio_unix.run_in_systhread ~label:(labelled "fs-compat-file-mtime" path) (fun () -> (Unix.stat path).st_mtime)) with
       | Unix.Unix_error _ -> None)
;;

let rename (src : string) (dst : string) : unit =
  with_fs_or_fallback
    ~path:src
    ~fallback:(fun () -> Stdlib.Sys.rename src dst)
    (fun fs -> Eio.Path.rename Eio.Path.(fs / src) Eio.Path.(fs / dst))
;;

(* Both runtime paths catch the missing-source case explicitly rather
   than substring-matching on the libc error text. Stdlib's [Sys.rename]
   raises [Sys_error] with a libc-translated, locale-sensitive message;
   matching "No such file" against it skips the Eio path where
   [Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _))] is propagated instead (the
   [with_io] normalizer wraps it into a [Sys_error] whose body does not
   necessarily contain "No such file"). The result was a silent
   path-dependent classifier failure: callers using the substring guard
   only recognized the missing-source case under the Stdlib fallback. *)
let rename_if_exists ~src ~dst =
  with_fs_or_fallback
    ~path:src
    ~fallback:(fun () ->
      try
        Stdlib.Sys.rename src dst;
        true
      with
      | Sys_error _ when not (Stdlib.Sys.file_exists src) -> false)
    (fun fs ->
      try
        Eio.Path.rename Eio.Path.(fs / src) Eio.Path.(fs / dst);
        true
      with
      | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false)
;;

let remove_tree_unix (path : string) : unit =
  let rec remove path =
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | exception Unix.Unix_error (Unix.ENOTDIR, _, _) -> ()
    | stat when stat.Unix.st_kind = Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove (Filename.concat path name));
      Unix.rmdir path
    | _stat -> Sys.remove path
  in
  remove path
;;

let remove_tree (path : string) : unit =
  let normalized = String.trim path in
  if String.equal normalized "" || String.equal normalized "/" || String.equal normalized "."
  then invalid_arg (Printf.sprintf "Fs_compat.remove_tree refuses unsafe path %S" path);
  test_exec_home_guard ~op:"remove_tree" path;
  with_fs_or_fallback
    ~path
    ~fallback:(fun () -> remove_tree_unix path)
    (fun _fs -> Eio_unix.run_in_systhread ~label:(labelled "fs-compat-remove-tree" path) (fun () -> remove_tree_unix path))
;;

let realpath (path : string) : string =
  with_fs_or_fallback
    ~path
    ~fallback:(fun () -> Unix.realpath path)
    (fun _fs -> Eio_unix.run_in_systhread ~label:(labelled "fs-compat-realpath" path) (fun () -> Unix.realpath path))
;;

let realpath_lenient (path : string) : string =
  try realpath path with
  | Unix.Unix_error _ ->
    (* Walk up the directory tree until we find an ancestor that exists and
       can be resolved via realpath, then reconstruct the suffix.
       This handles symlinks (e.g., /tmp -> /private/tmp on macOS) even when
       intermediate directories do not exist on disk.
       Tail-recursive to avoid stack overflow on deep untrusted paths. *)
    let rec collect_suffix p acc =
      let parent = Filename.dirname p in
      if parent = p
      then
        (* Reached filesystem root without a successful realpath. *)
        p, acc
      else (
        match
          try Some (realpath p) with
          | Unix.Unix_error _ -> None
        with
        | Some resolved -> resolved, acc
        | None -> collect_suffix parent (Filename.basename p :: acc))
    in
    let resolved_base, suffix_parts = collect_suffix path [] in
    List.fold_left Filename.concat resolved_base suffix_parts
;;

(** Create directory recursively if not exists.
    @raises Sys_error on all I/O failures. Eio.Io is normalized internally. *)
let mkdir_p (path : string) : unit =
  test_exec_home_guard ~op:"mkdir_p" path;
  with_fs_or_fallback
    ~path
    ~fallback:(fun () -> mkdir_p_unix path)
    (fun fs ->
       let eio_path = Eio.Path.(fs / path) in
       Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 eio_path)
;;

(* RFC-0162 §3.1: once-per-path mkdir memoize. Hot append paths
   ([append_jsonl] in particular) call [mkdir_p] on every record so
   that the day-rollover dir gets created on first append. After the
   dir exists, every subsequent call only burns a [Sys.file_exists]
   or [Eio.Path.mkdirs ~exists_ok:true] stat syscall — yet on the
   tool_call_io path this adds up to ~22k stats over a few hours and
   contributes to the EMFILE/ENFILE pressure documented in the RFC.

   The cache stores only the *fact* that the dir exists; it does not
   keep an fd open. RFC-0108 §2.5's cached [out_channel] corruption
   does not apply. RFC-0108 §3.3 (cross-domain fd cache) is unrelated.

   Race: two domains may both miss-and-mkdir; the second [mkdir] is a
   harmless EEXIST. The mutex covers the [Hashtbl] op only. *)
let mkdir_p_memoized path = Mkdir_memo.mkdir_p_memoized ~mkdir_p path
let invalidate_mkdir_memo path = Mkdir_memo.invalidate path
let reset_mkdir_memo_for_testing () = Mkdir_memo.reset_for_testing ()

(** Parse pre-read string lines as JSONL.
    Use when lines come from typed tail readers such as
    [Keeper_memory.read_file_tail_lines_result] or other non-file
    sources.  Logs malformed lines with [source] tag.

    Matches [fold_jsonl]'s line-tracking semantics: [line_no] is
    1-based, increments only on non-blank lines so it tracks the
    {b printed} JSONL row number an operator would see in [cat -n].
    Aligns with the file-level diagnostic at line 559 ("line %d") so
    a malformed log from either path uses the same orchestrate system. *)
(* One row, with the row number the warning prints. A caller that walks rows
   itself - to stop before the end of a window - parses through this so the
   warning it prints is the same one, in the same shape, as the whole-list
   parse below. *)
let parse_jsonl_line ~(source : string) ~(line_no : int) (line : string)
  : Yojson.Safe.t option
  =
  match Yojson.Safe.from_string line with
  | json -> Some json
  | exception Yojson.Json_error msg ->
    Stdlib.Printf.eprintf
      "[fs_compat] malformed JSONL (%s) line %d: %s\n%!"
      source
      line_no
      msg;
    None
;;

(* Blank lines do not take a number, matching what [cat -n] shows for the
   printed JSONL rows. A caller that needs those numbers without parsing
   uses this. *)
let number_jsonl_lines (lines : string list) : (int * string) list =
  let line_no = ref 0 in
  List.filter_map
    (fun line ->
       let trimmed = String.trim line in
       if String.equal trimmed ""
       then None
       else begin
         incr line_no;
         Some (!line_no, trimmed)
       end)
    lines
;;

let parse_jsonl_lines ~(source : string) (lines : string list) : Yojson.Safe.t list * int =
  let malformed = ref 0 in
  let parsed =
    List.filter_map
      (fun (line_no, trimmed) ->
         match parse_jsonl_line ~source ~line_no trimmed with
         | Some json -> Some json
         | None ->
           incr malformed;
           None)
      (number_jsonl_lines lines)
  in
  parsed, !malformed
;;

(** Load JSONL file, returning parsed values and count of malformed lines.
    Delegates to [parse_jsonl_lines] for the actual parsing. *)
let load_jsonl_diagnostics (path : string) : Yojson.Safe.t list * int =
  if not (file_exists path)
  then [], 0
  else (
    let content = load_file path in
    let lines = String.split_on_char '\n' content in
    parse_jsonl_lines ~source:path lines)
;;

(** Load JSONL file as list of JSON values.
    Malformed lines are logged and dropped. *)
let load_jsonl (path : string) : Yojson.Safe.t list = fst (load_jsonl_diagnostics path)

(* Bounded byte slice of a file. Clamps to the current size; a missing
   file or an empty clamped range returns "". Stdlib-blocking like the
   other tail-readers — callers bound [len], so the read cost is fixed
   regardless of file size (RFC-0228 P1). *)
let read_slice ~path ~from ~len =
  if not (file_exists path) || len <= 0 then ""
  else begin
    let ic = Stdlib.open_in_bin path in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_in_noerr ic)
      (fun () ->
         let size = Stdlib.in_channel_length ic in
         let from = if from < 0 then 0 else if from > size then size else from in
         let len = Stdlib.min len (size - from) in
         if len <= 0 then ""
         else begin
           Stdlib.seek_in ic from;
           Stdlib.really_input_string ic len
         end)
  end
;;

(* Fold over newline-terminated lines appended after byte offset [from].
   Append-only JSONL stores never rewrite earlier bytes, so a (offset,
   accumulator) pair is a pure function of the file prefix — callers cache
   it and re-scan only the delta instead of the whole file. Bytes after the
   last '\n' (a partially flushed line) are excluded from both the fold and
   the returned boundary, so the next call re-reads them once the writer
   completes the line. A [from] beyond EOF (file truncated/rotated) falls
   back to a full scan from byte 0; callers detect shrinkage the same way
   via the returned boundary. Blank lines advance the boundary but are not
   folded. *)
let fold_appended_lines ~path ~from ~init ~f =
  if not (file_exists path)
  then init, 0
  else begin
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
         let len = in_channel_length ic in
         let from = if from < 0 || from > len then 0 else from in
         seek_in ic from;
         let chunk = Bytes.create 65536 in
         let line_buf = Buffer.create 256 in
         let acc = ref init in
         let boundary = ref from in
         let pos = ref from in
         let rec loop () =
           let n = input ic chunk 0 (Bytes.length chunk) in
           if n > 0
           then begin
             for i = 0 to n - 1 do
               match Bytes.get chunk i with
               | '\n' ->
                 let line = Buffer.contents line_buf in
                 Buffer.clear line_buf;
                 boundary := !pos + i + 1;
                 if not (String.equal (String.trim line) "")
                 then acc := f !acc line
               | c -> Buffer.add_char line_buf c
             done;
             pos := !pos + n;
             loop ()
           end
         in
         loop ();
         !acc, !boundary)
  end
;;

(** Stream JSONL line-by-line, folding [f] over parsed values.

    Uses [Eio.Buf_read.lines] over [Eio.Path.with_open_in] when the
    global fs is registered ([set_fs] called at boot), giving O(1)
    memory regardless of file size and non-blocking IO inside
    the Eio scheduler.  Falls back to {!load_jsonl} + [List.fold_left]
    when no fs is available (tests, pre-boot helpers).

    [line_no] is 1-based and skips blank lines so it tracks the
    {b printed} JSONL row number rather than the raw byte stream
    position — matches {!load_jsonl_diagnostics} semantics.

    Malformed JSON lines are skipped after a stderr warning, like
    {!load_jsonl_diagnostics}.  Returns [init] when [path] does not
    exist (no read attempt).  Raises [Sys_error] on read failures of
    an existing file (e.g. permission denied, mid-stream IO error). *)
let fold_jsonl_lines ~init ~f path =
  if not (file_exists path)
  then init
  else
    (* 16 MiB per-line cap — protects [Eio.Buf_read.of_flow] from a
       corrupted/attacker-controlled JSONL with no newlines (which
       would otherwise force the buf_read to grow unbounded).  Real
       audit/metric rows are <1 KiB; 16 MiB is two orders of
       magnitude over expected and still bounds the OOM blast. *)
    let max_line_bytes = 16 * 1024 * 1024 in
    with_fs_or_fallback
      ~path
      ~fallback:(fun () ->
        (* Iterate raw lines so [line_no] reflects the same "non-blank
           index, malformed counted but skipped" semantics as the Eio
           branch; folding over [fst (load_jsonl_diagnostics ...)] alone
           would skip malformed rows and desync the index. *)
        let line_idx = ref 0 in
        let acc = ref init in
        let chan = Stdlib.open_in path in
        Fun.protect
          ~finally:(fun () -> Stdlib.close_in_noerr chan)
          (fun () ->
            try
              while true do
                let raw = Stdlib.input_line chan in
                let trimmed = String.trim raw in
                if not (String.equal trimmed "")
                then begin
                  incr line_idx;
                  match Yojson.Safe.from_string trimmed with
                  | json -> acc := f !acc ~line_no:!line_idx json
                  | exception Yojson.Json_error msg ->
                    Stdlib.Printf.eprintf
                      "[fs_compat] malformed JSONL (%s) line %d: %s\n%!"
                      path
                      !line_idx
                      msg
                end
              done
            with End_of_file -> ());
        !acc)
      (fun fs ->
         let eio_path = Eio.Path.(fs / path) in
         Eio.Path.with_open_in eio_path (fun flow ->
           let buf = Eio.Buf_read.of_flow ~max_size:max_line_bytes flow in
           let line_idx = ref 0 in
           let acc = ref init in
           Eio.Buf_read.lines buf
           |> Seq.iter (fun raw ->
             let trimmed = String.trim raw in
             if not (String.equal trimmed "")
             then begin
               incr line_idx;
               match Yojson.Safe.from_string trimmed with
               | json -> acc := f !acc ~line_no:!line_idx json
               | exception Yojson.Json_error msg ->
                 Stdlib.Printf.eprintf
                   "[fs_compat] malformed JSONL (%s) line %d: %s\n%!"
                   path
                   !line_idx
                   msg
             end);
           !acc))
;;

(** Append JSON value as line to JSONL file.

    Atomic per record (in-process): the same [append_path_mutex_registry]
    used by {!append_file_unix} serializes callers against each
    other, and a fresh fd is opened and closed around a single
    [output_string] of [record + "\n"]. Cross-domain safe. Records
    of any size are written without interleaving (the mutex spans
    the whole syscall sequence). Crash durability is not guaranteed
    (no fsync).

    PR #15936 introduced this helper with its own per-path
    registry. This commit unifies the registry with
    [append_file_unix] so a caller mixing the two helpers on a
    single path remains race-free. *)
(* RFC-0162 §3.4: per-path fd cache (single fd per path, cross-domain
   serialized by [append_path_mutex_registry]).

   RFC-0108 §3.3 declared cross-domain fd cache an explicit non-goal
   under the assumption that fd count ≈ keeper N (≤64). Production
   evidence (RFC-0162 §1.3) invalidated that on the open/close
   *churn* axis: 22,440 tool calls × fresh open+close per record was
   shifting host kernel filp_cachep slab pressure and contributing
   to the EMFILE/ENFILE trace.

   Design choice — Per-path (not per-domain) cache:
   - A single [out_channel] per path eliminates the cross-domain
     write interleave window that an early RFC draft's per-domain
     design opened up (POSIX O_APPEND+write atomicity is only
     guaranteed up to PIPE_BUF, ~4 KB, and tool-call records
     routinely exceed that).
   - The existing [get_append_path_mutex] is already a cross-domain
     [Stdlib.Mutex] per path. Wrapping [output_string + flush]
     inside it preserves RFC-0108 §3.2's Record-interleave-0
     guarantee verbatim.
   - The cache lookup uses a separate, microsecond-scoped mutex
     ([fd_cache_mu]) so two appends to *different* paths never
     contend on a global fd-cache lock. *)
let invalidate_cached_writer path =
  let path_mu = get_append_path_mutex path in
  Stdlib.Mutex.protect path_mu (fun () -> Fd_cache.invalidate path)
;;

let reset_fd_cache_for_testing () = Fd_cache.reset_for_testing ()

let with_cached_writer_for_testing path f = Fd_cache.with_writer path f

let rec read_fd_chunks fd buffer =
  let chunk = Bytes.create 65536 in
  match Unix.read fd chunk 0 (Bytes.length chunk) with
  | 0 -> Buffer.contents buffer
  | count ->
    Buffer.add_subbytes buffer chunk 0 count;
    read_fd_chunks fd buffer
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_fd_chunks fd buffer

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

let durable_append_operation_to_string = function
  | Write -> "write"
  | Append_fsync -> "append fsync"
  | Rollback_truncate -> "rollback truncate"
  | Rollback_fsync -> "rollback fsync"
;;

let durable_append_failure_to_string = function
  | No_write_progress -> "write made no progress"
  | Unix_error { operation; error; function_name; argument } ->
    Printf.sprintf
      "%s failed: %s (function=%S argument=%S)"
      (durable_append_operation_to_string operation)
      (Unix.error_message error)
      function_name
      argument
;;

let durable_append_error_to_string { append_failure; rollback_failures } =
  let append = durable_append_failure_to_string append_failure in
  match rollback_failures with
  | [] -> Printf.sprintf "durable append failed and rollback succeeded: %s" append
  | failures ->
    Printf.sprintf
      "durable append failed: %s; rollback failed: %s"
      append
      (failures
       |> List.map durable_append_failure_to_string
       |> String.concat "; ")
;;

type durable_append_io_for_testing =
  { write : Unix.file_descr -> bytes -> int -> int -> int
  ; ftruncate : Unix.file_descr -> int -> unit
  ; fsync : Unix.file_descr -> unit
  }

let unix_failure ~operation error function_name argument =
  Unix_error { operation; error; function_name; argument }
;;

let rec run_unix_io ~operation f =
  match f () with
  | () -> Ok ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> run_unix_io ~operation f
  | exception Unix.Unix_error (error, function_name, argument) ->
    Error (unix_failure ~operation error function_name argument)
;;

let rollback_durable_append ~io ~fd ~original_length =
  let truncate_result =
    run_unix_io ~operation:Rollback_truncate (fun () ->
      io.ftruncate fd original_length)
  in
  let fsync_result =
    run_unix_io ~operation:Rollback_fsync (fun () -> io.fsync fd)
  in
  [ truncate_result; fsync_result ]
  |> List.filter_map (function
    | Ok () -> None
    | Error failure -> Some failure)
;;

let append_fd_durable ~io ~fd ~original_length suffix =
  let bytes = Bytes.of_string suffix in
  let append_result =
    match
      Fd_write_all.run
        ~length:(Bytes.length bytes)
        ~write:(fun ~offset ~length -> io.write fd bytes offset length)
    with
    | Error (Fd_write_all.No_progress _) -> Error No_write_progress
    | Error
        (Fd_write_all.Unix_error
          { bytes_written = _; error; function_name; argument }) ->
      Error (unix_failure ~operation:Write error function_name argument)
    | Error
        (Fd_write_all.Operation_failed
          { bytes_written = _; exception_; backtrace }) ->
      Printexc.raise_with_backtrace exception_ backtrace
    | Ok () -> run_unix_io ~operation:Append_fsync (fun () -> io.fsync fd)
  in
  match append_result with
  | Ok () -> Ok ()
  | Error append_failure ->
    let rollback_failures = rollback_durable_append ~io ~fd ~original_length in
    Error { append_failure; rollback_failures }
;;

let append_fd_durable_for_testing = append_fd_durable

let durable_append_unix_io =
  { write = Unix.write; ftruncate = Unix.ftruncate; fsync = Unix.fsync }
;;

let fsync_parent_directory dir =
  let fd = Unix.openfile dir [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.fsync fd)
;;

let rec lock_whole_file fd =
  match Unix.lockf fd Unix.F_LOCK 0 with
  | () -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> lock_whole_file fd
;;

let rec lock_whole_file_shared fd =
  match Unix.lockf fd Unix.F_RLOCK 0 with
  | () -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> lock_whole_file_shared fd
;;

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

type capability_append_open_error =
  | Capability_append_open_invalid_leaf of string
  | Capability_append_open_missing
  | Capability_append_open_failed of capability_append_operation_failure

type capability_append_file =
  { parent : Eio.Fs.dir_ty Eio.Path.t
  ; leaf : Capability_leaf.t
  ; resource : Eio.File.rw_ty Eio.Resource.t
  ; stat : Eio.File.Stat.t
  }

let capability_append_failure_to_string = function
  | Capability_append_posix_descriptor_unavailable ->
    "POSIX descriptor unavailable"
  | Capability_append_mutation_contended ->
    "append target identity is busy in another cooperative writer"
  | Capability_append_operation_failed { exception_; _ } ->
    Printexc.to_string exception_
;;

let capability_append_open_error_to_string = function
  | Capability_append_open_invalid_leaf leaf ->
    Printf.sprintf "invalid capability leaf %S" leaf
  | Capability_append_open_missing -> "capability append target is missing"
  | Capability_append_open_failed { exception_; _ } ->
    Printexc.to_string exception_
;;

let capability_append_operation_failure exception_ backtrace =
  { exception_; backtrace }
;;

let open_capability_append_file ~sw ~parent ~leaf =
  match Capability_leaf.of_string leaf with
  | None -> Error (Capability_append_open_invalid_leaf leaf)
  | Some leaf ->
    let leaf_name = Capability_leaf.to_string leaf in
    (try
       let resource =
         Eio.Path.open_out
           ~sw
           ~append:true
           ~create:`Never
           Eio.Path.(parent / leaf_name)
       in
       let stat = Eio.File.stat resource in
       Ok { parent; leaf; resource; stat }
     with
     | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
     | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
       Error Capability_append_open_missing
     | exception_ ->
       let backtrace = Printexc.get_raw_backtrace () in
       Error
         (Capability_append_open_failed
            (capability_append_operation_failure exception_ backtrace)))
;;

let capability_append_file_stat file = file.stat

let capability_append_outcome
      ~requested_bytes
      ?(bytes_written = 0)
      ?write_failure
      ?sync_failure
      ?(target_binding = Capability_append_target_not_checked)
      ()
  =
  { requested_bytes
  ; bytes_written
  ; write_failure
  ; sync_failure
  ; target_binding
  }
;;

type capability_append_io_for_testing =
  { write_substring : Unix.file_descr -> string -> int -> int -> int
  ; fsync : Unix.file_descr -> unit
  }

let append_fd_observed ~io ~fd content =
  let requested_bytes = String.length content in
  let bytes_written, write_failure =
    match
      Fd_write_all.run
        ~length:requested_bytes
        ~write:(fun ~offset ~length ->
          io.write_substring fd content offset length)
    with
    | Ok () -> requested_bytes, None
    | Error (Fd_write_all.No_progress { bytes_written }) ->
      ( bytes_written
      , Some
          (Capability_append_operation_failed
             (capability_append_operation_failure
                (Unix.Unix_error
                   (Unix.EIO, "write", "regular file accepted zero bytes"))
                (Printexc.get_callstack 0))) )
    | Error
        (Fd_write_all.Unix_error
          { bytes_written; error; function_name; argument }) ->
      ( bytes_written
      , Some
          (Capability_append_operation_failed
             (capability_append_operation_failure
                (Unix.Unix_error (error, function_name, argument))
                (Printexc.get_callstack 0))) )
    | Error
        (Fd_write_all.Operation_failed
          { bytes_written; exception_; backtrace }) ->
      ( bytes_written
      , Some
          (Capability_append_operation_failed
             (capability_append_operation_failure exception_ backtrace)) )
  in
  let sync_failure =
    if bytes_written > 0 || Option.is_none write_failure
    then
      let rec sync () =
        try
          io.fsync fd;
          None
        with
        | Unix.Unix_error (Unix.EINTR, _, _) -> sync ()
        | exception_ ->
          let backtrace = Printexc.get_raw_backtrace () in
          Some (capability_append_operation_failure exception_ backtrace)
      in
      sync ()
    else None
  in
  capability_append_outcome
    ~requested_bytes
    ~bytes_written
    ?write_failure
    ?sync_failure
    ()
;;

let capability_append_unix_io =
  { write_substring = Unix.write_substring; fsync = Unix.fsync }
;;

type append_target_observation =
  | Append_target_verified
  | Append_target_changed
  | Append_target_check_failed of capability_append_operation_failure

let append_target_binding ~parent ~leaf opened =
  try
    let lexical = Eio.Path.stat ~follow:false Eio.Path.(parent / leaf) in
    if
      opened.Eio.File.Stat.kind = `Regular_file
      && lexical.kind = `Regular_file
      && Int64.equal opened.dev lexical.dev
      && Int64.equal opened.ino lexical.ino
    then Append_target_verified
    else Append_target_changed
  with
  | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Append_target_changed
  | exception_ ->
    let backtrace = Printexc.get_raw_backtrace () in
    Append_target_check_failed
      (capability_append_operation_failure exception_ backtrace)
;;

let capability_append_target_binding_of_observation = function
  | Append_target_verified -> Capability_append_target_verified
  | Append_target_changed -> Capability_append_target_changed
  | Append_target_check_failed failure ->
    Capability_append_target_check_failed failure
;;

let append_capability_observed_with ~after_write file content =
  let { parent; leaf; resource; stat = opened_stat } = file in
  let requested_bytes = String.length content in
  let leaf_name = Capability_leaf.to_string leaf in
  if requested_bytes = 0
  then capability_append_outcome ~requested_bytes ()
  else
    let parent_stat =
      try Ok (Eio.Path.stat ~follow:true parent) with
      | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
      | exception_ ->
        let backtrace = Printexc.get_raw_backtrace () in
        Error (capability_append_operation_failure exception_ backtrace)
    in
    match parent_stat with
    | Error failure ->
      capability_append_outcome
        ~requested_bytes
        ~write_failure:(Capability_append_operation_failed failure)
        ~target_binding:(Capability_append_target_check_failed failure)
        ()
    | Ok parent_stat when parent_stat.kind <> `Directory ->
      let exception_ =
        Invalid_argument "append parent capability is not a directory"
      in
      let failure =
        capability_append_operation_failure
          exception_
          (Printexc.get_callstack 0)
      in
      capability_append_outcome
        ~requested_bytes
        ~write_failure:(Capability_append_operation_failed failure)
        ~target_binding:(Capability_append_target_check_failed failure)
        ()
    | Ok parent_stat ->
      (match
         Capability_mutation_lease.try_acquire
           (Capability_mutation_lease.Existing_target
              { target_dev = opened_stat.dev
              ; target_ino = opened_stat.ino
              ; parent_dev = parent_stat.dev
              ; parent_ino = parent_stat.ino
              })
       with
       | None ->
         capability_append_outcome
           ~requested_bytes
           ~write_failure:Capability_append_mutation_contended
           ()
       | Some mutation_lease ->
         Fun.protect
           ~finally:(fun () ->
             Capability_mutation_lease.release mutation_lease)
         @@ fun () ->
         (match append_target_binding ~parent ~leaf:leaf_name opened_stat with
          | (Append_target_changed | Append_target_check_failed _) as observation ->
            let target_binding =
              capability_append_target_binding_of_observation observation
            in
            capability_append_outcome ~requested_bytes ~target_binding ()
          | Append_target_verified ->
            let outcome =
              match Eio_unix.Resource.fd_opt resource with
              | None ->
                capability_append_outcome
                  ~requested_bytes
                  ~write_failure:Capability_append_posix_descriptor_unavailable
                  ~target_binding:Capability_append_target_verified
                  ()
              | Some fd ->
                (try
                   Eio_unix.run_in_systhread
                     ~label:"fs-compat-open-file-append"
                     (fun () ->
                        Eio_unix.Fd.use_exn
                          "fs-compat-open-file-append"
                          fd
                          (fun unix_fd ->
                             append_fd_observed
                               ~io:capability_append_unix_io
                               ~fd:unix_fd
                               content))
                 with
                 | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
                 | exception_ ->
                   let backtrace = Printexc.get_raw_backtrace () in
                   capability_append_outcome
                     ~requested_bytes
                     ~write_failure:
                       (Capability_append_operation_failed
                          (capability_append_operation_failure
                             exception_
                             backtrace))
                     ~target_binding:Capability_append_target_verified
                     ())
            in
            after_write ();
            let target_binding =
              append_target_binding ~parent ~leaf:leaf_name opened_stat
              |> capability_append_target_binding_of_observation
            in
            { outcome with target_binding }))
;;

let append_capability_observed file content =
  append_capability_observed_with ~after_write:(fun () -> ()) file content
;;

module Capability_append_for_testing = struct
  let append_capability_observed = append_capability_observed_with
  let append_fd_observed = append_fd_observed
end

let run_blocking_private_file_transaction ~label ~path:_ f =
  match execution_context () with
  | Non_eio -> f ()
  | Eio_fiber -> Eio_unix.run_in_systhread ~label f
;;

module Private_jsonl_cursor = struct
  type t =
    | Missing
    | Present of
        { device : int
        ; inode : int
        ; end_offset : int
        }

  let equal left right =
    match left, right with
    | Missing, Missing -> true
    | ( Present
          { device = left_device
          ; inode = left_inode
          ; end_offset = left_end_offset
          }
      , Present
          { device = right_device
          ; inode = right_inode
          ; end_offset = right_end_offset
          } ) ->
      Int.equal left_device right_device
      && Int.equal left_inode right_inode
      && Int.equal left_end_offset right_end_offset
    | Missing, Present _ | Present _, Missing -> false
  ;;

  let to_string = function
    | Missing -> "missing"
    | Present { device; inode; end_offset } ->
      Printf.sprintf
        "present(device=%d,inode=%d,end_offset=%d)"
        device
        inode
        end_offset
  ;;

  let prefix_end_offset cursor ~device ~inode ~file_length =
    match cursor with
    | Missing -> None
    | Present
        { device = expected_device
        ; inode = expected_inode
        ; end_offset
        } ->
      if Int.equal expected_device device
         && Int.equal expected_inode inode
         && end_offset <= file_length
      then Some end_offset
      else None
  ;;
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

let private_jsonl_settlement_success = function
  | (Transaction_settlement_failed
      { primary = Transaction_succeeded success; _ } as error) ->
    Some (success, error)
  | Transaction_settlement_failed { primary = Transaction_failed _; _ }
  | Stable_lock_contended _
  | Unexpected_stable_lock_permissions _
  | Invalid_stable_lock_state _
  | Cursor_mismatch _
  | Unexpected_transaction_file_kind _
  | Ambiguous_transaction_file_identity _
  | Transaction_path_binding_changed _
  | Incomplete_transaction_tail _
  | Invalid_transaction_suffix
  | Private_jsonl_operation_failed _
  | Rewrite_stage_failed _
  | Rewrite_published_durability_unknown _
  | Transaction_append_failed _ -> None
;;

let private_jsonl_snapshot_success_receipt = function
  | Ok value -> Ok { value; settlement_error = None }
  | Error error ->
    (match private_jsonl_settlement_success error with
     | Some (Snapshot_succeeded value, settlement_error) ->
       Ok { value; settlement_error = Some settlement_error }
     | Some
         ( (Cursor_succeeded _ | Cursor_precondition_succeeded _)
         , _ )
     | None -> Error error)
;;

let private_jsonl_cursor_success_receipt = function
  | Ok value -> Ok { value; settlement_error = None }
  | Error error ->
    (match private_jsonl_settlement_success error with
     | Some (Cursor_succeeded value, settlement_error) ->
       Ok { value; settlement_error = Some settlement_error }
     | Some
         ( (Snapshot_succeeded _ | Cursor_precondition_succeeded _)
         , _ )
     | None -> Error error)
;;

let private_jsonl_operation_to_string = function
  | Create_parent_directory -> "create parent directory"
  | Canonicalize_parent_directory -> "canonicalize parent directory"
  | Inspect_stable_lock -> "inspect stable lock"
  | Open_stable_lock -> "open stable lock"
  | Set_stable_lock_permissions -> "set stable lock permissions"
  | Sync_stable_lock_parent -> "sync stable lock parent"
  | Acquire_stable_lock -> "acquire stable lock"
  | Read_stable_lock_state -> "read stable lock state"
  | Write_stable_lock_state -> "write stable lock state"
  | Sync_stable_lock -> "sync stable lock"
  | Close_stable_lock -> "close stable lock"
  | Open_transaction_data -> "open transaction data"
  | Set_transaction_data_permissions -> "set transaction data permissions"
  | Inspect_transaction_data -> "inspect transaction data"
  | Inspect_transaction_path -> "inspect transaction path"
  | Read_transaction_data -> "read transaction data"
  | Close_transaction_data -> "close transaction data"
  | Create_rewrite_stage -> "create rewrite stage"
  | Set_rewrite_stage_permissions -> "set rewrite stage permissions"
  | Write_rewrite_stage -> "write rewrite stage"
  | Sync_rewrite_stage -> "sync rewrite stage"
  | Close_rewrite_stage -> "close rewrite stage"
  | Rename_rewrite_stage -> "rename rewrite stage"
  | Sync_rewrite_parent -> "sync rewrite parent"
  | Inspect_rewritten_data -> "inspect rewritten data"
  | Remove_rewrite_stage -> "remove rewrite stage"
  | Truncate_transaction_data -> "truncate transaction data"
  | Sync_transaction_data -> "sync transaction data"
;;

let private_jsonl_operation_failure_to_string failure =
  Printf.sprintf
    "%s failed: %s"
    (private_jsonl_operation_to_string failure.operation)
    (Printexc.to_string failure.exception_)
;;

let private_jsonl_cleanup_failures_to_string cleanup_failures =
  cleanup_failures
  |> List.map private_jsonl_operation_failure_to_string
  |> String.concat "; "
;;

let rec private_jsonl_transaction_error_to_string = function
  | Stable_lock_contended { lock_path } ->
    Printf.sprintf "private JSONL stable lock is contended: %s" lock_path
  | Unexpected_stable_lock_permissions { path; actual } ->
    Printf.sprintf
      "private JSONL stable lock permissions are %04o, expected 0600: %s"
      actual
      path
  | Invalid_stable_lock_state { path; observed_length } ->
    Printf.sprintf
      "private JSONL stable lock has invalid durable state: %s (%d bytes)"
      path
      observed_length
  | Cursor_mismatch { expected; actual } ->
    Printf.sprintf
      "private JSONL cursor mismatch: expected %s, observed %s"
      (Private_jsonl_cursor.to_string expected)
      (Private_jsonl_cursor.to_string actual)
  | Unexpected_transaction_file_kind kind ->
    Printf.sprintf
      "private JSONL transaction target has unexpected file kind: %s"
      (file_kind_to_string kind)
  | Ambiguous_transaction_file_identity { path; link_count } ->
    Printf.sprintf
      "private JSONL transaction path has ambiguous hard-link identity: %s (links=%d)"
      path
      link_count
  | Transaction_path_binding_changed { path } ->
    Printf.sprintf
      "private JSONL transaction path binding changed during operation: %s"
      path
  | Incomplete_transaction_tail { end_offset } ->
    Printf.sprintf
      "private JSONL transaction target has an incomplete tail at end %d"
      end_offset
  | Invalid_transaction_suffix ->
    "private JSONL transaction payload must be empty or newline-terminated"
  | Private_jsonl_operation_failed failure ->
    private_jsonl_operation_failure_to_string failure
  | Rewrite_stage_failed { failure; cleanup_failures } ->
    let cleanup =
      match cleanup_failures with
      | [] -> ""
      | cleanup_failures ->
        Printf.sprintf
          "; cleanup also failed: %s"
          (private_jsonl_cleanup_failures_to_string cleanup_failures)
    in
    private_jsonl_operation_failure_to_string failure ^ cleanup
  | Rewrite_published_durability_unknown { cursor; failure } ->
    Printf.sprintf
      "private JSONL rewrite was published but durability is unknown (cursor=%s): %s"
      (match cursor with
       | None -> "unknown"
       | Some cursor -> Private_jsonl_cursor.to_string cursor)
      (private_jsonl_operation_failure_to_string failure)
  | Transaction_settlement_failed { primary; cleanup_failures } ->
    let primary =
      match primary with
      | Transaction_succeeded (Snapshot_succeeded snapshot) ->
        Printf.sprintf
          "private JSONL snapshot read succeeded (cursor=%s)"
          (Private_jsonl_cursor.to_string snapshot.cursor)
      | Transaction_succeeded (Cursor_succeeded cursor) ->
        Printf.sprintf
          "private JSONL cursor operation succeeded (cursor=%s)"
          (Private_jsonl_cursor.to_string cursor)
      | Transaction_succeeded (Cursor_precondition_succeeded cursor) ->
        Printf.sprintf
          "private JSONL cursor precondition read succeeded (cursor=%s)"
          (Private_jsonl_cursor.to_string cursor)
      | Transaction_failed primary ->
        private_jsonl_transaction_error_to_string primary
    in
    Printf.sprintf
      "%s; descriptor settlement failed: %s"
      primary
      (private_jsonl_cleanup_failures_to_string cleanup_failures)
  | Transaction_append_failed append_error ->
    durable_append_error_to_string append_error
;;

let private_jsonl_failure operation exception_ =
  { operation; exception_; backtrace = Printexc.get_raw_backtrace () }
;;

let private_jsonl_capture operation f =
  match f () with
  | value -> Ok value
  | exception (Eio.Cancel.Cancelled _ as cancellation) -> raise cancellation
  | exception exception_ -> Error (private_jsonl_failure operation exception_)
;;

let private_file_transaction_map f = function
  | Private_file_succeeded value -> Private_file_succeeded (f value)
  | Private_file_succeeded_with_cleanup_failure
      { value; cleanup_failure } ->
    Private_file_succeeded_with_cleanup_failure
      { value = f value; cleanup_failure }
  | Private_file_failed error -> Private_file_failed error
  | Private_file_failed_with_cleanup_failure { error; cleanup_failure } ->
    Private_file_failed_with_cleanup_failure { error; cleanup_failure }
;;

let private_jsonl_with_fd_outcome ~close_operation ~close_fd fd f =
  match f () with
  | Ok value ->
    (match private_jsonl_capture close_operation (fun () -> close_fd fd) with
     | Ok () -> Private_file_succeeded value
     | Error cleanup_failure ->
       Private_file_succeeded_with_cleanup_failure
         { value; cleanup_failure })
  | Error error ->
    (match private_jsonl_capture close_operation (fun () -> close_fd fd) with
     | Ok () -> Private_file_failed error
     | Error cleanup_failure ->
       Private_file_failed_with_cleanup_failure
         { error; cleanup_failure })
  | exception exception_ ->
    let backtrace = Printexc.get_raw_backtrace () in
    (match private_jsonl_capture close_operation (fun () -> close_fd fd) with
     | Ok () -> Printexc.raise_with_backtrace exception_ backtrace
     | Error cleanup_failure ->
       let combined, combined_backtrace =
         Eio.Exn.combine
           (exception_, backtrace)
           (cleanup_failure.exception_, cleanup_failure.backtrace)
       in
       Printexc.raise_with_backtrace combined combined_backtrace)
;;

let private_jsonl_add_cleanup_failure error cleanup_failure =
  match error with
  | Transaction_settlement_failed { primary; cleanup_failures } ->
    Transaction_settlement_failed
      { primary; cleanup_failures = cleanup_failures @ [ cleanup_failure ] }
  | (Stable_lock_contended _
    | Unexpected_stable_lock_permissions _
    | Invalid_stable_lock_state _
    | Cursor_mismatch _
    | Unexpected_transaction_file_kind _
    | Ambiguous_transaction_file_identity _
    | Transaction_path_binding_changed _
    | Incomplete_transaction_tail _
    | Invalid_transaction_suffix
    | Private_jsonl_operation_failed _
    | Rewrite_stage_failed _
    | Rewrite_published_durability_unknown _
    | Transaction_append_failed _) as primary ->
    Transaction_settlement_failed
      { primary = Transaction_failed primary
      ; cleanup_failures = [ cleanup_failure ]
      }
;;

let private_jsonl_settlement_failed ~success result cleanup_failure =
  match result with
  | Ok value ->
    Error
      (Transaction_settlement_failed
         { primary = Transaction_succeeded (success value)
         ; cleanup_failures = [ cleanup_failure ]
         })
  | Error error -> Error (private_jsonl_add_cleanup_failure error cleanup_failure)
;;

let private_jsonl_with_fd ~close_operation ~close_fd ~success fd f =
  match private_jsonl_with_fd_outcome ~close_operation ~close_fd fd f with
  | Private_file_succeeded value -> Ok value
  | Private_file_failed error -> Error error
  | Private_file_succeeded_with_cleanup_failure
      { value; cleanup_failure } ->
    private_jsonl_settlement_failed ~success (Ok value) cleanup_failure
  | Private_file_failed_with_cleanup_failure { error; cleanup_failure } ->
    Error (private_jsonl_add_cleanup_failure error cleanup_failure)
;;

let private_jsonl_lock_path path = path ^ ".lock"

type private_jsonl_transaction_io_for_testing =
  { before_sync_parent : string -> unit
  ; close_fd : Unix.file_descr -> unit
  }

let private_jsonl_transaction_unix_io =
  { before_sync_parent = (fun _dir -> ()); close_fd = Unix.close }
;;

let private_jsonl_stable_lock_ready_marker = "\xA5"

type private_jsonl_stable_lock_state =
  | Stable_lock_initializing
  | Stable_lock_ready

type private_jsonl_stable_lock_creation =
  | Stable_lock_created
  | Stable_lock_existing

type private_jsonl_stable_lock =
  { fd : Unix.file_descr
  ; creation : private_jsonl_stable_lock_creation
  }

let private_jsonl_open_stable_lock lock_path =
  match
    private_jsonl_capture Open_stable_lock (fun () ->
      Unix.openfile
        lock_path
        [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ]
        0o600)
  with
  | Ok fd -> Ok { fd; creation = Stable_lock_created }
  | Error failure ->
    (match failure.exception_ with
     | Unix.Unix_error (error, _, _) when error = Unix.EEXIST ->
       private_jsonl_capture Open_stable_lock (fun () ->
         Unix.openfile lock_path [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0)
       |> Result.map (fun fd -> { fd; creation = Stable_lock_existing })
       |> Result.map_error (fun failure -> Private_jsonl_operation_failed failure)
     (* DET-OK: [exn] is open. Every non-EEXIST open failure remains typed. *)
     | _ -> Error (Private_jsonl_operation_failed failure))
;;

let rec private_jsonl_write_substring_all fd value offset =
  if offset < String.length value
  then
    match Unix.write_substring fd value offset (String.length value - offset) with
    | 0 ->
      raise
        (Unix.Unix_error
           (Unix.EIO, "write", "private JSONL stable lock made no progress"))
    | count -> private_jsonl_write_substring_all fd value (offset + count)
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      private_jsonl_write_substring_all fd value offset
;;

let private_jsonl_read_stable_lock_state path fd =
  match private_jsonl_capture Read_stable_lock_state (fun () -> Unix.fstat fd) with
  | Error failure -> Error (Private_jsonl_operation_failed failure)
  | Ok stats ->
    let observed_length = stats.Unix.st_size in
    if observed_length = 0
    then Ok Stable_lock_initializing
    else if observed_length <> String.length private_jsonl_stable_lock_ready_marker
    then Error (Invalid_stable_lock_state { path; observed_length })
    else
      (match private_jsonl_capture Read_stable_lock_state (fun () ->
         let bytes = Bytes.create observed_length in
         (* fire-and-forget: seek target is fixed at 0; a failed lseek surfaces as a short/garbage read below *)
         ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
         let rec read_all offset =
           if offset = observed_length
           then ()
           else
             match Unix.read fd bytes offset (observed_length - offset) with
             | 0 -> raise End_of_file
             | count -> read_all (offset + count)
             | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_all offset
         in
         read_all 0;
         Bytes.unsafe_to_string bytes)
       with
       | Error failure -> Error (Private_jsonl_operation_failed failure)
       | Ok marker ->
         if String.equal marker private_jsonl_stable_lock_ready_marker
         then Ok Stable_lock_ready
         else Error (Invalid_stable_lock_state { path; observed_length }))
;;

let private_jsonl_sync_stable_lock fd =
  private_jsonl_capture Sync_stable_lock (fun () -> Unix.fsync fd)
  |> Result.map_error (fun failure -> Private_jsonl_operation_failed failure)
;;

let private_jsonl_initialize_stable_lock ~io ~dir fd =
  let ( let* ) = Result.bind in
  let* () = private_jsonl_sync_stable_lock fd in
  let* () =
    private_jsonl_capture Sync_stable_lock_parent (fun () ->
      io.before_sync_parent dir;
      fsync_parent_directory dir)
    |> Result.map_error (fun failure -> Private_jsonl_operation_failed failure)
  in
  let* () =
    private_jsonl_capture Write_stable_lock_state (fun () ->
      (* fire-and-forget: seek target is fixed at 0; a failed lseek surfaces as a short write below *)
      ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
      private_jsonl_write_substring_all
        fd
        private_jsonl_stable_lock_ready_marker
        0)
    |> Result.map_error (fun failure -> Private_jsonl_operation_failed failure)
  in
  private_jsonl_sync_stable_lock fd
;;

let private_jsonl_prepare_stable_lock ~io ~dir ~path stable_lock =
  let ( let* ) = Result.bind in
  let* () =
    match stable_lock.creation with
    | Stable_lock_created ->
      private_jsonl_capture Set_stable_lock_permissions (fun () ->
        Unix.fchmod stable_lock.fd 0o600)
      |> Result.map_error (fun failure -> Private_jsonl_operation_failed failure)
    | Stable_lock_existing -> Ok ()
  in
  let* permissions =
    private_jsonl_capture Inspect_stable_lock (fun () ->
      (Unix.fstat stable_lock.fd).Unix.st_perm land 0o7777)
    |> Result.map_error (fun failure -> Private_jsonl_operation_failed failure)
  in
  if permissions <> 0o600
  then Error (Unexpected_stable_lock_permissions { path; actual = permissions })
  else
    let* state = private_jsonl_read_stable_lock_state path stable_lock.fd in
    match state with
    | Stable_lock_initializing ->
      private_jsonl_initialize_stable_lock ~io ~dir stable_lock.fd
    | Stable_lock_ready -> Ok ()
;;

let private_jsonl_same_identity left right =
  Int.equal left.Unix.st_dev right.Unix.st_dev
  && Int.equal left.Unix.st_ino right.Unix.st_ino
;;

let private_jsonl_validate_unique_regular_binding ~path ~path_stats ~fd_stats =
  if path_stats.Unix.st_kind <> Unix.S_REG
  then Error (Unexpected_transaction_file_kind path_stats.Unix.st_kind)
  else if fd_stats.Unix.st_kind <> Unix.S_REG
  then Error (Unexpected_transaction_file_kind fd_stats.Unix.st_kind)
  else if path_stats.Unix.st_nlink <> 1
  then
    Error
      (Ambiguous_transaction_file_identity
         { path; link_count = path_stats.Unix.st_nlink })
  else if fd_stats.Unix.st_nlink <> 1
  then
    Error
      (Ambiguous_transaction_file_identity
         { path; link_count = fd_stats.Unix.st_nlink })
  else if not (private_jsonl_same_identity path_stats fd_stats)
  then Error (Transaction_path_binding_changed { path })
  else Ok ()
;;

let private_jsonl_validate_open_binding ~operation ~path fd =
  match private_jsonl_capture operation (fun () -> Unix.lstat path, Unix.fstat fd) with
  | Error failure -> Error (Private_jsonl_operation_failed failure)
  | Ok (path_stats, fd_stats) ->
    private_jsonl_validate_unique_regular_binding ~path ~path_stats ~fd_stats
;;

let rec try_lock_whole_file fd =
  match Unix.lockf fd Unix.F_TLOCK 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> try_lock_whole_file fd
  | exception Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) -> false
;;

let with_private_jsonl_stable_lock ~io ~success requested_path f =
  let requested_dir = Filename.dirname requested_path in
  run_blocking_private_file_transaction
    ~label:"fs-compat-private-jsonl-transaction"
    ~path:requested_path
    (fun () ->
       match private_jsonl_capture Create_parent_directory (fun () ->
         test_exec_home_guard ~op:"private_jsonl_transaction" requested_path;
         mkdir_p_unix requested_dir)
       with
       | Error failure -> Error (Private_jsonl_operation_failed failure)
       | Ok () ->
         (match private_jsonl_capture Canonicalize_parent_directory (fun () ->
            Unix.realpath requested_dir)
          with
          | Error failure -> Error (Private_jsonl_operation_failed failure)
          | Ok dir ->
            let path = Filename.concat dir (Filename.basename requested_path) in
            let lock_path = private_jsonl_lock_path path in
            let path_mu = get_append_path_mutex path in
            Stdlib.Mutex.protect path_mu (fun () ->
              match private_jsonl_open_stable_lock lock_path with
              | Error _ as error -> error
              | Ok stable_lock ->
                private_jsonl_with_fd
                  ~close_operation:Close_stable_lock
                  ~close_fd:io.close_fd
                  ~success
                  stable_lock.fd
                  (fun () ->
                     let ( let* ) = Result.bind in
                     let* () =
                       private_jsonl_validate_open_binding
                         ~operation:Inspect_stable_lock
                         ~path:lock_path
                         stable_lock.fd
                     in
                     let* acquired =
                       private_jsonl_capture Acquire_stable_lock (fun () ->
                         try_lock_whole_file stable_lock.fd)
                       |> Result.map_error (fun failure ->
                         Private_jsonl_operation_failed failure)
                     in
                     if not acquired
                     then Error (Stable_lock_contended { lock_path })
                     else
                       let* () =
                         private_jsonl_validate_open_binding
                           ~operation:Inspect_stable_lock
                           ~path:lock_path
                           stable_lock.fd
                       in
                       let* () =
                         private_jsonl_prepare_stable_lock
                           ~io
                           ~dir
                           ~path:lock_path
                           stable_lock
                       in
                       f ~dir ~path))))
;;

let private_jsonl_cursor_of_stats stats =
  if stats.Unix.st_kind <> Unix.S_REG
  then Error (Unexpected_transaction_file_kind stats.Unix.st_kind)
  else
    Ok
      (Private_jsonl_cursor.Present
         { device = stats.Unix.st_dev
         ; inode = stats.Unix.st_ino
         ; end_offset = stats.Unix.st_size
         })
;;

let private_jsonl_open_existing ~close_fd path flags =
  match Unix.openfile path (Unix.O_CLOEXEC :: flags) 0 with
  | fd ->
    (match
       private_jsonl_validate_open_binding
         ~operation:Inspect_transaction_path
         ~path
         fd
     with
     | Ok () -> Ok (Some fd)
     | Error error ->
       (match private_jsonl_capture Close_transaction_data (fun () -> close_fd fd) with
        | Ok () -> Error error
        | Error cleanup_failure ->
          Error (private_jsonl_add_cleanup_failure error cleanup_failure)))
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
  | exception (Eio.Cancel.Cancelled _ as cancellation) -> raise cancellation
  | exception exception_ ->
    Error
      (Private_jsonl_operation_failed
         (private_jsonl_failure Open_transaction_data exception_))
;;

let rec private_jsonl_read_byte fd byte offset =
  (* See POSIX lseek: no exception confirms the requested absolute offset. *)
  ignore (Unix.lseek fd offset Unix.SEEK_SET : int);
  match Unix.read fd byte 0 1 with
  | 1 -> Bytes.get byte 0
  | 0 -> raise End_of_file
  | count ->
    invalid_arg (Printf.sprintf "single-byte JSONL read returned %d bytes" count)
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
    private_jsonl_read_byte fd byte offset
;;

let private_jsonl_validate_complete_tail fd end_offset =
  if end_offset = 0
  then Ok ()
  else
    match private_jsonl_capture Read_transaction_data (fun () ->
      let byte = Bytes.create 1 in
      Char.equal (private_jsonl_read_byte fd byte (end_offset - 1)) '\n')
    with
    | Error failure -> Error (Private_jsonl_operation_failed failure)
    | Ok true -> Ok ()
    | Ok false -> Error (Incomplete_transaction_tail { end_offset })
;;

let private_jsonl_read_exact fd ~from ~end_offset =
  private_jsonl_capture Read_transaction_data (fun () ->
    let length = end_offset - from in
    let bytes = Bytes.create length in
    (* See POSIX lseek: no exception confirms the requested absolute cursor. *)
    ignore (Unix.lseek fd from Unix.SEEK_SET : int);
    let rec read_all offset =
      if offset = length
      then ()
      else
        match Unix.read fd bytes offset (length - offset) with
        | 0 -> raise End_of_file
        | count -> read_all (offset + count)
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_all offset
    in
    read_all 0;
    Bytes.unsafe_to_string bytes)
  |> Result.map_error (fun failure -> Private_jsonl_operation_failed failure)
;;

let read_private_jsonl_durable_locked_with_io ~io path ~after =
  let success snapshot = Snapshot_succeeded snapshot in
  with_private_jsonl_stable_lock ~io ~success path @@ fun ~dir:_ ~path ->
  match private_jsonl_open_existing ~close_fd:io.close_fd path [ Unix.O_RDONLY ] with
  | Error _ as error -> error
  | Ok None ->
    let actual = Private_jsonl_cursor.Missing in
    (match after with
     | None -> Ok { bytes = ""; cursor = actual }
     | Some expected ->
       if Private_jsonl_cursor.equal expected actual
       then Ok { bytes = ""; cursor = actual }
       else Error (Cursor_mismatch { expected; actual }))
  | Ok (Some fd) ->
    private_jsonl_with_fd
      ~close_operation:Close_transaction_data
      ~close_fd:io.close_fd
      ~success
      fd
      (fun () ->
         match private_jsonl_capture Inspect_transaction_data (fun () -> Unix.fstat fd)
         with
         | Error failure -> Error (Private_jsonl_operation_failed failure)
         | Ok stats ->
           (match private_jsonl_cursor_of_stats stats with
            | Error _ as error -> error
            | Ok actual ->
              (match private_jsonl_validate_complete_tail fd stats.Unix.st_size with
               | Error _ as error -> error
               | Ok () ->
                 let from =
                   match after with
                   | None -> Ok 0
                   | Some expected ->
                     (match
                        Private_jsonl_cursor.prefix_end_offset
                          expected
                          ~device:stats.Unix.st_dev
                          ~inode:stats.Unix.st_ino
                          ~file_length:stats.Unix.st_size
                      with
                      | Some end_offset -> Ok end_offset
                      | None -> Error (Cursor_mismatch { expected; actual }))
                 in
                 (match from with
                  | Error _ as error -> error
                  | Ok from ->
                    private_jsonl_read_exact
                      fd
                      ~from
                      ~end_offset:stats.Unix.st_size
                    |> Result.map (fun bytes -> { bytes; cursor = actual })))))
;;

let read_private_jsonl_durable_locked_result path ~after =
  read_private_jsonl_durable_locked_with_io
    ~io:private_jsonl_transaction_unix_io
    path
    ~after
;;

let private_jsonl_last_complete_row_length bytes =
  match String.rindex_opt bytes '\n' with
  | Some index -> index + 1
  | None -> 0
;;

(* Process-start recovery entry point: a torn tail left by a mid-append crash
   is truncated to the last complete row under the stable lock, then reading
   resumes. General reads keep hard-failing on [Incomplete_transaction_tail];
   only recovery callers may opt into truncation. *)
let recover_private_jsonl_durable_locked_with_io ~io path =
  let success snapshot = Snapshot_succeeded snapshot in
  with_private_jsonl_stable_lock ~io ~success path @@ fun ~dir:_ ~path ->
  match private_jsonl_open_existing ~close_fd:io.close_fd path [ Unix.O_RDWR ] with
  | Error _ as error -> error
  | Ok None -> Ok { bytes = ""; cursor = Private_jsonl_cursor.Missing }
  | Ok (Some fd) ->
    private_jsonl_with_fd
      ~close_operation:Close_transaction_data
      ~close_fd:io.close_fd
      ~success
      fd
      (fun () ->
         match private_jsonl_capture Inspect_transaction_data (fun () -> Unix.fstat fd) with
         | Error failure -> Error (Private_jsonl_operation_failed failure)
         | Ok stats ->
           (match private_jsonl_cursor_of_stats stats with
            | Error _ as error -> error
            | Ok actual ->
              (match private_jsonl_validate_complete_tail fd stats.Unix.st_size with
               | Ok () ->
                 private_jsonl_read_exact fd ~from:0 ~end_offset:stats.Unix.st_size
                 |> Result.map (fun bytes -> { bytes; cursor = actual })
               | Error (Incomplete_transaction_tail { end_offset }) ->
                 (match private_jsonl_read_exact fd ~from:0 ~end_offset with
                  | Error _ as error -> error
                  | Ok bytes ->
                    let complete_length = private_jsonl_last_complete_row_length bytes in
                    (match
                       private_jsonl_capture Truncate_transaction_data (fun () ->
                         Unix.ftruncate fd complete_length)
                     with
                     | Error failure -> Error (Private_jsonl_operation_failed failure)
                     | Ok () ->
                       (match
                          private_jsonl_capture Sync_transaction_data (fun () ->
                            Unix.fsync fd)
                        with
                        | Error failure -> Error (Private_jsonl_operation_failed failure)
                        | Ok () ->
                          (match
                             private_jsonl_capture Inspect_transaction_data (fun () ->
                               Unix.fstat fd)
                           with
                           | Error failure ->
                             Error (Private_jsonl_operation_failed failure)
                           | Ok truncated_stats ->
                             (match private_jsonl_cursor_of_stats truncated_stats with
                              | Error _ as error -> error
                              | Ok cursor ->
                                Ok
                                  { bytes = String.sub bytes 0 complete_length
                                  ; cursor
                                  })))))
               (* Recovery is scoped to [Incomplete_transaction_tail]; every
                  other typed failure propagates exactly as the general read
                  path surfaces it. *)
               | Error
                   ( Stable_lock_contended _
                   | Unexpected_stable_lock_permissions _
                   | Invalid_stable_lock_state _
                   | Cursor_mismatch _
                   | Unexpected_transaction_file_kind _
                   | Ambiguous_transaction_file_identity _
                   | Transaction_path_binding_changed _
                   | Invalid_transaction_suffix
                   | Private_jsonl_operation_failed _
                   | Rewrite_stage_failed _
                   | Rewrite_published_durability_unknown _
                   | Transaction_settlement_failed _
                   | Transaction_append_failed _ ) as error -> error)))
;;

let recover_private_jsonl_durable_locked_result path =
  recover_private_jsonl_durable_locked_with_io
    ~io:private_jsonl_transaction_unix_io
    path
;;

let read_private_jsonl_durable_locked_with_io_for_testing ~io path ~after =
  read_private_jsonl_durable_locked_with_io ~io path ~after
;;

let private_jsonl_current_cursor ~io path =
  let success cursor = Cursor_precondition_succeeded cursor in
  match private_jsonl_open_existing ~close_fd:io.close_fd path [ Unix.O_RDONLY ] with
  | Error _ as error -> error
  | Ok None -> Ok Private_jsonl_cursor.Missing
  | Ok (Some fd) ->
    private_jsonl_with_fd
      ~close_operation:Close_transaction_data
      ~close_fd:io.close_fd
      ~success
      fd
      (fun () ->
         match private_jsonl_capture Inspect_transaction_data (fun () -> Unix.fstat fd)
         with
         | Error failure -> Error (Private_jsonl_operation_failed failure)
         | Ok stats ->
           (match private_jsonl_cursor_of_stats stats with
            | Error _ as error -> error
            | Ok cursor ->
              private_jsonl_validate_complete_tail fd stats.Unix.st_size
              |> Result.map (fun () -> cursor)))
;;

let private_jsonl_remove_rewrite_stage temp_path =
  match private_jsonl_capture Remove_rewrite_stage (fun () -> Unix.unlink temp_path) with
  | Ok () -> None
  | Error failure ->
    (match failure.exception_ with
     | Unix.Unix_error (error, _, _) when error = Unix.ENOENT -> None
     (* DET-OK: [exn] is open. Every non-ENOENT cleanup exception is preserved
        as typed failure evidence; this arm never chooses a permissive default. *)
     | _ -> Some failure)
;;

let private_jsonl_replace_locked ~dir path content =
  match private_jsonl_capture Create_rewrite_stage (fun () ->
    (* Open_binary for the same reason as [Atomic_write.open_atomic_temp_file]:
       the stage file holds the replacement bytes verbatim. *)
    Filename.open_temp_file
      ~mode:[ Open_binary ]
      ~temp_dir:dir
      ".private_jsonl_"
      ".stage")
  with
  | Error failure -> Error (Private_jsonl_operation_failed failure)
  | Ok (temp_path, output) ->
    let descriptor = Unix.descr_of_out_channel output in
    let first_failure = ref None in
    let run_until_failure operation f =
      match !first_failure with
      | Some _ -> ()
      | None ->
        (match private_jsonl_capture operation f with
         | Ok () -> ()
         | Error failure -> first_failure := Some failure)
    in
    run_until_failure Set_rewrite_stage_permissions (fun () ->
      Unix.fchmod descriptor 0o600);
    run_until_failure Write_rewrite_stage (fun () ->
      output_string output content;
      flush output);
    run_until_failure Sync_rewrite_stage (fun () -> Unix.fsync descriptor);
    let operation_failure = !first_failure in
    let close_failure =
      match private_jsonl_capture Close_rewrite_stage (fun () -> close_out output) with
      | Ok () -> None
      | Error failure -> Some failure
    in
    (match operation_failure, close_failure with
     | Some failure, close_failure ->
       let cleanup_failures =
         [ close_failure; private_jsonl_remove_rewrite_stage temp_path ]
         |> List.filter_map Fun.id
       in
       Error (Rewrite_stage_failed { failure; cleanup_failures })
     | None, Some failure ->
       let cleanup_failures =
         private_jsonl_remove_rewrite_stage temp_path |> Option.to_list
       in
       Error (Rewrite_stage_failed { failure; cleanup_failures })
     | None, None ->
       (match private_jsonl_capture Rename_rewrite_stage (fun () ->
          Unix.rename temp_path path)
        with
        | Error failure ->
          let cleanup_failures =
            private_jsonl_remove_rewrite_stage temp_path |> Option.to_list
          in
          Error (Rewrite_stage_failed { failure; cleanup_failures })
        | Ok () ->
          (match private_jsonl_capture Inspect_rewritten_data (fun () ->
             Unix.lstat path)
           with
           | Error failure ->
             Error
               (Rewrite_published_durability_unknown
                  { cursor = None; failure })
           | Ok stats ->
             (match private_jsonl_cursor_of_stats stats with
              | Error error ->
                let failure =
                  private_jsonl_failure
                    Inspect_rewritten_data
                    (Failure (private_jsonl_transaction_error_to_string error))
                in
                Error
                  (Rewrite_published_durability_unknown
                     { cursor = None; failure })
              | Ok cursor ->
                (match private_jsonl_capture Sync_rewrite_parent (fun () ->
                   fsync_parent_directory dir)
                 with
                 | Ok () -> Ok cursor
                 | Error failure ->
                   Error
                     (Rewrite_published_durability_unknown
                        { cursor = Some cursor; failure }))))))
;;

let append_private_jsonl_durable_locked_at_cursor_with_io ~io path ~expected suffix =
  if String.equal suffix ""
     || not (Char.equal suffix.[String.length suffix - 1] '\n')
  then Error Invalid_transaction_suffix
  else
    let success cursor = Cursor_succeeded cursor in
    with_private_jsonl_stable_lock ~io ~success path @@ fun ~dir ~path ->
    match
      private_jsonl_open_existing
        ~close_fd:io.close_fd
        path
        [ Unix.O_RDWR; Unix.O_APPEND ]
    with
    | Error _ as error -> error
    | Ok None ->
      let actual = Private_jsonl_cursor.Missing in
      if not (Private_jsonl_cursor.equal expected actual)
      then Error (Cursor_mismatch { expected; actual })
      else private_jsonl_replace_locked ~dir path suffix
    | Ok (Some fd) ->
      private_jsonl_with_fd
        ~close_operation:Close_transaction_data
        ~close_fd:io.close_fd
        ~success
        fd
        (fun () ->
           match private_jsonl_capture Inspect_transaction_data (fun () -> Unix.fstat fd)
           with
           | Error failure -> Error (Private_jsonl_operation_failed failure)
           | Ok stats ->
             (match private_jsonl_cursor_of_stats stats with
              | Error _ as error -> error
              | Ok actual ->
                if not (Private_jsonl_cursor.equal expected actual)
                then Error (Cursor_mismatch { expected; actual })
                else (
                  match private_jsonl_validate_complete_tail fd stats.Unix.st_size with
                  | Error _ as error -> error
                  | Ok () ->
                    (match private_jsonl_capture Set_transaction_data_permissions (fun () ->
                       Unix.fchmod fd 0o600)
                     with
                     | Error failure ->
                       Error (Private_jsonl_operation_failed failure)
                     | Ok () ->
                       (* See POSIX lseek: O_APPEND owns writes; this resets reads. *)
                       ignore (Unix.lseek fd 0 Unix.SEEK_END : int);
                       (match
                          append_fd_durable
                            ~io:durable_append_unix_io
                            ~fd
                            ~original_length:stats.Unix.st_size
                            suffix
                        with
                        | Error append_error ->
                          Error (Transaction_append_failed append_error)
                        | Ok () ->
                          (match private_jsonl_capture Inspect_transaction_data (fun () ->
                             Unix.fstat fd)
                           with
                           | Error failure ->
                             Error (Private_jsonl_operation_failed failure)
                           | Ok committed_stats ->
                             private_jsonl_cursor_of_stats committed_stats))))))
;;

let append_private_jsonl_durable_locked_at_cursor_result path ~expected suffix =
  append_private_jsonl_durable_locked_at_cursor_with_io
    ~io:private_jsonl_transaction_unix_io
    path
    ~expected
    suffix
;;

let append_private_jsonl_durable_locked_at_cursor_with_io_for_testing
      ~io
      path
      ~expected
      suffix
  =
  append_private_jsonl_durable_locked_at_cursor_with_io
    ~io
    path
    ~expected
    suffix
;;

let rewrite_private_jsonl_durable_locked_at_cursor_with_io
      ~io
      path
      ~expected
      content
  =
  if not (String.equal content "")
     && not (Char.equal content.[String.length content - 1] '\n')
  then Error Invalid_transaction_suffix
  else
    with_private_jsonl_stable_lock
      ~io
      ~success:(fun cursor -> Cursor_succeeded cursor)
      path
    @@ fun ~dir ~path ->
    match private_jsonl_current_cursor ~io path with
    | Error _ as error -> error
    | Ok actual ->
      if not (Private_jsonl_cursor.equal expected actual)
      then Error (Cursor_mismatch { expected; actual })
      else private_jsonl_replace_locked ~dir path content
;;

let rewrite_private_jsonl_durable_locked_at_cursor_result
      path
      ~expected
      content
  =
  rewrite_private_jsonl_durable_locked_at_cursor_with_io
    ~io:private_jsonl_transaction_unix_io
    path
    ~expected
    content
;;

let rewrite_private_jsonl_durable_locked_at_cursor_with_io_for_testing
      ~io
      path
      ~expected
      content
  =
  rewrite_private_jsonl_durable_locked_at_cursor_with_io
    ~io
    path
    ~expected
    content
;;

module Private_jsonl_slice = struct
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

  let error_to_string = function
    | Negative_offset offset -> Printf.sprintf "negative JSONL cursor: %d" offset
    | Missing_file_after_offset offset ->
      Printf.sprintf "JSONL store is missing after cursor %d" offset
    | Offset_beyond_end { offset; end_offset } ->
      Printf.sprintf "JSONL cursor %d is beyond end %d" offset end_offset
    | Offset_not_at_row_boundary offset ->
      Printf.sprintf "JSONL cursor %d is not a row boundary" offset
    | Incomplete_tail end_offset ->
      Printf.sprintf "JSONL store has an incomplete tail at end %d" end_offset
    | Io_failed exn -> Printexc.to_string exn
  ;;
end

let read_private_jsonl_slice_locked_with_io ~io path ~from =
  let open Private_jsonl_slice in
  if from < 0
  then Private_file_failed (Negative_offset from)
  else
    try
      test_exec_home_guard ~op:"read_private_jsonl_slice_locked" path;
      let path_mu = get_append_path_mutex path in
      run_blocking_private_file_transaction
        ~label:"fs-compat-private-jsonl-read"
        ~path
        (fun () ->
           Stdlib.Mutex.protect path_mu (fun () ->
             match Unix.openfile path [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 with
             | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
               if from = 0
               then Private_file_succeeded { bytes = ""; end_offset = 0 }
               else Private_file_failed (Missing_file_after_offset from)
             | fd ->
               private_jsonl_with_fd_outcome
                 ~close_operation:Close_transaction_data
                 ~close_fd:io.close_fd
                 fd
                 (fun () ->
                    try
                      lock_whole_file_shared fd;
                      let end_offset = Unix.lseek fd 0 Unix.SEEK_END in
                      if from > end_offset
                      then Error (Offset_beyond_end { offset = from; end_offset })
                      else (
                        let byte_at offset =
                          (* See Unix.lseek: only the file-position side effect is required. *)
                          ignore (Unix.lseek fd offset Unix.SEEK_SET : int);
                          let byte = Bytes.create 1 in
                          let rec read () =
                            match Unix.read fd byte 0 1 with
                            | 1 -> Bytes.get byte 0
                            | 0 -> raise End_of_file
                            | count ->
                              invalid_arg
                                (Printf.sprintf
                                   "single-byte JSONL read returned %d bytes"
                                   count)
                            | exception Unix.Unix_error (Unix.EINTR, _, _) ->
                              read ()
                          in
                          read ()
                        in
                        if from > 0
                           && not (Char.equal (byte_at (from - 1)) '\n')
                        then Error (Offset_not_at_row_boundary from)
                        else if
                          end_offset > 0
                          && not (Char.equal (byte_at (end_offset - 1)) '\n')
                        then Error (Incomplete_tail end_offset)
                        else (
                          let length = end_offset - from in
                          (* See Unix.lseek: only the file-position side effect is required. *)
                          ignore (Unix.lseek fd from Unix.SEEK_SET : int);
                          let bytes = Bytes.create length in
                          let rec read_all offset =
                            if offset = length
                            then ()
                            else
                              match Unix.read fd bytes offset (length - offset) with
                              | 0 -> raise End_of_file
                              | count -> read_all (offset + count)
                              | exception Unix.Unix_error (Unix.EINTR, _, _) ->
                                read_all offset
                          in
                          read_all 0;
                          Ok
                            { bytes = Bytes.unsafe_to_string bytes
                            ; end_offset
                            }))
                    with
                    | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
                    | exn -> Error (Io_failed exn))))
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> Private_file_failed (Io_failed exn)
;;

let read_private_jsonl_slice_locked_result path ~from =
  read_private_jsonl_slice_locked_with_io
    ~io:private_jsonl_transaction_unix_io
    path
    ~from
;;

let read_private_jsonl_slice_locked_with_io_for_testing ~io path ~from =
  read_private_jsonl_slice_locked_with_io ~io path ~from
;;

module Private_jsonl_rows = struct
  type t =
    | Rows_missing
    | Rows_present of
        { rows : string
        ; rows_end : int
        ; end_offset : int
        }

  type error = Io_failed of exn

  let error_to_string = function
    | Io_failed exn -> Printexc.to_string exn
  ;;
end

(* The writer's framing rule applied by a reader: under the same per-path
   mutex and a shared cross-process lock, the file is every row that ends in
   '\n' plus, at most, one fragment a crashed append left behind (the
   exclusive lock means no append is in progress while the shared lock is
   held). The fragment is reported by [rows_end < end_offset], never returned
   as bytes, so the caller sees exactly the rows a later append would build
   on. *)
let read_private_jsonl_rows_locked_with_io ~io path =
  let open Private_jsonl_rows in
  try
    test_exec_home_guard ~op:"read_private_jsonl_rows_locked" path;
    let path_mu = get_append_path_mutex path in
    run_blocking_private_file_transaction
      ~label:"fs-compat-private-jsonl-rows-read"
      ~path
      (fun () ->
         Stdlib.Mutex.protect path_mu (fun () ->
           match Unix.openfile path [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 with
           | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
             Private_file_succeeded Rows_missing
           | fd ->
             private_jsonl_with_fd_outcome
               ~close_operation:Close_transaction_data
               ~close_fd:io.close_fd
               fd
               (fun () ->
                  try
                    lock_whole_file_shared fd;
                    let bytes = read_fd_chunks fd (Buffer.create 65536) in
                    let end_offset = String.length bytes in
                    let rows_end =
                      match String.rindex_opt bytes '\n' with
                      | Some newline -> newline + 1
                      | None -> 0
                    in
                    Ok
                      (Rows_present
                         { rows = String.sub bytes 0 rows_end; rows_end; end_offset })
                  with
                  | Eio.Cancel.Cancelled _ as cancellation -> raise cancellation
                  | exn -> Error (Io_failed exn))))
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Private_file_failed (Io_failed exn)
;;

let read_private_jsonl_rows_locked_result path =
  read_private_jsonl_rows_locked_with_io ~io:private_jsonl_transaction_unix_io path
;;

let update_private_file_durable_locked_with_io ~io path decide =
  test_exec_home_guard ~op:"update_private_file_durable_locked" path;
  let dir = Filename.dirname path in
  mkdir_p_memoized dir;
  let path_mu = get_append_path_mutex path in
  run_blocking_private_file_transaction
    ~label:"fs-compat-durable-update"
    ~path
    (fun () ->
    Stdlib.Mutex.protect path_mu (fun () ->
      let fd =
        Unix.openfile path
          [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_APPEND; Unix.O_CLOEXEC ]
          0o600
      in
      private_jsonl_with_fd_outcome
        ~close_operation:Close_transaction_data
        ~close_fd:io.close_fd
        fd
        (fun () ->
           Unix.fchmod fd 0o600;
           fsync_parent_directory dir;
           (* See Unix.lseek: only the file-position side effect is required. *)
           ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
           lock_whole_file fd;
           (* See Unix.lseek: only the file-position side effect is required. *)
           ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
           let existing = read_fd_chunks fd (Buffer.create 4096) in
           let suffix, result = decide existing in
           match suffix with
            | None -> Ok result
            | Some suffix ->
              (* See Unix.lseek: only the file-position side effect is required. *)
              let original_length = Unix.lseek fd 0 Unix.SEEK_END in
              append_fd_durable
                ~io:durable_append_unix_io
                ~fd
                ~original_length
                suffix
              |> Result.map (fun () -> result))))
;;

let update_private_file_durable_locked_result path decide =
  update_private_file_durable_locked_with_io
    ~io:private_jsonl_transaction_unix_io
    path
    decide
;;

let update_private_file_durable_locked_with_io_for_testing ~io path decide =
  update_private_file_durable_locked_with_io ~io path decide
;;

let rewrite_private_file_durable_locked_result path decide =
  test_exec_home_guard ~op:"rewrite_private_file_durable_locked" path;
  let dir = Filename.dirname path in
  mkdir_p_memoized dir;
  let path_mu = get_append_path_mutex path in
  run_blocking_private_file_transaction
    ~label:"fs-compat-durable-rewrite"
    ~path
    (fun () ->
    Stdlib.Mutex.protect path_mu (fun () ->
      let existing = Option.value (load_file_opt path) ~default:"" in
      let content, result = decide existing in
      match content with
      | None -> Ok result
      | Some content ->
        (match save_file_atomic path content with
         | Ok () -> Ok result
         | Error message -> Error message)))
;;

type private_jsonl_append_error =
  | Incomplete_jsonl_tail
  | Invalid_jsonl_suffix
  | Negative_expected_end_offset of int
  | End_offset_mismatch of
      { expected : int
      ; actual : int
      }
  | Durable_jsonl_append_failed of durable_append_error

let private_jsonl_append_error_to_string = function
  | Incomplete_jsonl_tail ->
    "existing JSONL file ends with an incomplete row"
  | Invalid_jsonl_suffix ->
    "JSONL append suffix must be non-empty and newline-terminated"
  | Negative_expected_end_offset offset ->
    Printf.sprintf "expected JSONL end offset must be non-negative: %d" offset
  | End_offset_mismatch { expected; actual } ->
    Printf.sprintf
      "JSONL end offset mismatch: expected %d, observed %d"
      expected
      actual
  | Durable_jsonl_append_failed error -> durable_append_error_to_string error
;;

let append_private_jsonl_durable_locked_with_expected_end_offset_with_io
      ~io
      path
      ~expected_end_offset
      suffix
  =
  if String.equal suffix ""
     || not (Char.equal suffix.[String.length suffix - 1] '\n')
  then Private_file_failed Invalid_jsonl_suffix
  else
    match expected_end_offset with
    | Some offset when offset < 0 ->
      Private_file_failed (Negative_expected_end_offset offset)
    | _ ->
      run_blocking_private_file_transaction
        ~label:"fs-compat-durable-append"
        ~path
        (fun () ->
           test_exec_home_guard ~op:"append_private_jsonl_durable_locked" path;
           let dir = Filename.dirname path in
           Mkdir_memo.mkdir_p_memoized
             ~mkdir_p:(fun dir ->
               test_exec_home_guard ~op:"mkdir_p" dir;
               mkdir_p_unix dir)
             dir;
           let path_mu = get_append_path_mutex path in
           Stdlib.Mutex.protect path_mu (fun () ->
             let fd =
               Unix.openfile path
                 [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_APPEND; Unix.O_CLOEXEC ]
                 0o600
             in
             private_jsonl_with_fd_outcome
               ~close_operation:Close_transaction_data
               ~close_fd:io.close_fd
               fd
               (fun () ->
                  Unix.fchmod fd 0o600;
                  fsync_parent_directory dir;
                  (* See Unix.lseek: only the file-position side effect is required. *)
                  ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
                  lock_whole_file fd;
                  let original_length = Unix.lseek fd 0 Unix.SEEK_END in
                  match expected_end_offset with
                  | Some expected when expected <> original_length ->
                    Error
                      (End_offset_mismatch
                         { expected; actual = original_length })
                  | _ ->
                    let tail_is_complete =
                      if original_length = 0
                      then true
                      else (
                        (* See Unix.lseek: only the file-position side effect is required. *)
                        ignore (Unix.lseek fd (original_length - 1) Unix.SEEK_SET : int);
                        let byte = Bytes.create 1 in
                        let rec read_tail () =
                          match Unix.read fd byte 0 1 with
                          | 1 -> Char.equal (Bytes.get byte 0) '\n'
                          | 0 -> false
                          | _ -> false
                          | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_tail ()
                        in
                        read_tail ())
                    in
                    if not tail_is_complete
                    then Error Incomplete_jsonl_tail
                    else (
                      (* See Unix.lseek: only the file-position side effect is required. *)
                      ignore (Unix.lseek fd 0 Unix.SEEK_END : int);
                      append_fd_durable
                        ~io:durable_append_unix_io
                        ~fd
                        ~original_length
                        suffix
                      |> Result.map_error (fun error ->
                        Durable_jsonl_append_failed error)
                      |> Result.map (fun () ->
                        original_length + String.length suffix)))))
;;

let append_private_jsonl_durable_locked_with_expected_end_offset_result
      path
      ~expected_end_offset
      suffix
  =
  append_private_jsonl_durable_locked_with_expected_end_offset_with_io
    ~io:private_jsonl_transaction_unix_io
    path
    ~expected_end_offset
    suffix
;;

let append_private_jsonl_durable_locked_with_end_offset_result path suffix =
  append_private_jsonl_durable_locked_with_expected_end_offset_result
    path
    ~expected_end_offset:None
    suffix
;;

let append_private_jsonl_durable_locked_at_end_offset_result
      path
      ~expected_end_offset
      suffix
  =
  append_private_jsonl_durable_locked_with_expected_end_offset_result
    path
    ~expected_end_offset:(Some expected_end_offset)
    suffix
;;

let append_private_jsonl_durable_locked_result path suffix =
  append_private_jsonl_durable_locked_with_end_offset_result path suffix
  |> private_file_transaction_map ignore
;;

let append_private_jsonl_durable_locked_at_end_offset_with_io_for_testing
      ~io
      path
      ~expected_end_offset
      suffix
  =
  append_private_jsonl_durable_locked_with_expected_end_offset_with_io
    ~io
    path
    ~expected_end_offset:(Some expected_end_offset)
    suffix
;;

let append_jsonl (path : string) (json : Yojson.Safe.t) : unit =
  test_exec_home_guard ~op:"append_jsonl" path;
  let dir = Stdlib.Filename.dirname path in
  mkdir_p_memoized dir;
  let line = Yojson.Safe.to_string json ^ "\n" in
  let path_mu = get_append_path_mutex path in
  Stdlib.Mutex.protect path_mu (fun () ->
    Fd_cache.with_writer path (fun oc ->
      Stdlib.output_string oc line;
      Stdlib.flush oc))

let append_jsonl_batch (path : string) (jsons : Yojson.Safe.t list) : unit =
  if jsons = [] then ()
  else begin
    test_exec_home_guard ~op:"append_jsonl_batch" path;
    let dir = Stdlib.Filename.dirname path in
    mkdir_p_memoized dir;
    let buf = Buffer.create 4096 in
    List.iter (fun json ->
      Buffer.add_string buf (Yojson.Safe.to_string json);
      Buffer.add_char buf '\n'
    ) jsons;
    let chunk = Buffer.contents buf in
    let path_mu = get_append_path_mutex path in
    Stdlib.Mutex.protect path_mu (fun () ->
      Fd_cache.with_writer path (fun oc ->
        Stdlib.output_string oc chunk;
        Stdlib.flush oc))
  end
;;
module Capability_head = Capability_head
