(** Room Utilities - Shared helpers for Room module *)

open Types

(** Storage backend type - unified interface *)
type storage_backend =
  | Memory of Backend.MemoryBackend.t
  | FileSystem of Backend.FileSystemBackend.t
  | PostgresNative of Backend.PostgresNative.t

(** Room configuration *)
type config = {
  base_path: string;
  lock_expiry_minutes: int;
  backend_config: Backend.config;
  backend: storage_backend;
}

(* ============================================ *)
(* Git Root Detection (Worktree Support)        *)
(* ============================================ *)

(** Read .git file content (for worktrees) *)
let read_git_file path =
  match Safe_ops.read_file_safe path with
  | Ok content ->
    (match String.split_on_char '\n' content with
     | line :: _ -> Some (String.trim line)
     | [] -> None)
  | Error _ -> None

(** Parse gitdir from .git file
    Format: "gitdir: /path/to/main/.git/worktrees/branch-name"
    Returns: /path/to/main (the main repository root) *)
let parse_gitdir_to_main_root gitdir_line =
  match String.split_on_char ':' gitdir_line with
  | [prefix; path] when String.trim prefix = "gitdir" ->
      let gitdir = String.trim path in
      (* gitdir: /main/.git/worktrees/branch → /main *)
      if String.length gitdir > 0 then begin
        (* Check if it's a worktree path: .git/worktrees/xxx *)
        let parts = String.split_on_char '/' gitdir in
        let rec find_git_parent = function
          | [] -> None
          | ".git" :: "worktrees" :: _ :: _ ->
              (* Found worktree pattern, reconstruct main repo path *)
              let rec take_until_git acc = function
                | [] -> None
                | ".git" :: _ -> Some (String.concat "/" (List.rev acc))
                | h :: t -> take_until_git (h :: acc) t
              in
              take_until_git [] parts
          | _ :: t -> find_git_parent t
        in
        find_git_parent parts
      end else None
  | _ -> None

(** Find git root from a path, handling worktrees
    - If .git is a directory → this is the main repo
    - If .git is a file → this is a worktree, find main repo
    - If no .git found → search parent directories *)
let rec find_git_root path =
  let git_path = Filename.concat path ".git" in
  if Sys.file_exists git_path then begin
    if Sys.is_directory git_path then
      Some path  (* Main repository *)
    else begin
      (* Worktree: .git is a file pointing to main repo *)
      match read_git_file git_path with
      | Some content ->
          (match parse_gitdir_to_main_root content with
           | Some main_root -> Some main_root
           | None -> Some path)  (* Fallback to current if parse fails *)
      | None -> Some path
    end
  end else begin
    let parent = Filename.dirname path in
    if parent = path then None  (* Reached filesystem root *)
    else find_git_root parent
  end

(** Resolve base_path: if in worktree, use main repo's path for .masc/
    This ensures all worktrees share the same MASC coordination space *)
let resolve_masc_base_path path =
  match find_git_root path with
  | Some git_root ->
      Log.Room.info "MASC base resolved: %s → %s (git root)" path git_root;
      git_root
  | None ->
      Log.Room.info "MASC base: %s (no git root found)" path;
      path

(* ============================================ *)
(* Environment helpers                          *)
(* ============================================ *)

let env_opt name =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> Some value
  | _ -> None

(** Storage type from environment variable *)
let storage_type_from_env () =
  match env_opt "MASC_STORAGE_TYPE" with
  | Some value -> String.lowercase_ascii value
  | None -> "filesystem"  (* Default to zero-dependency FileSystem backend *)

(** Auto-detect best backend based on environment variables
    Priority order:
    1. MASC_POSTGRES_URL / DATABASE_URL - if available, use PostgreSQL for distributed coordination
    2. FileSystem - zero-dependency default for personal/small use *)
let auto_detect_backend () =
  if env_opt "MASC_POSTGRES_URL" <> None || env_opt "DATABASE_URL" <> None then begin
    Log.Backend.info "Auto-detect: PostgreSQL URL found → PostgresNative backend";
    "postgres"
  end else begin
    Log.Backend.info "Auto-detect: No distributed DB found → FileSystem backend (default)";
    "filesystem"
  end

(* ============================================ *)
(* Backend creation                             *)
(* ============================================ *)

(* Sanitize namespace/cluster name for filesystem path segments.
   Keep alnum, '-', '_' and replace others with '-'. *)
let sanitize_namespace_segment name =
  let buf = Buffer.create (String.length name) in
  String.iter (fun c ->
    let is_safe =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c = '-' || c = '_'
    in
    Buffer.add_char buf (if is_safe then c else '-')
  ) name;
  let sanitized = String.trim (Buffer.contents buf) in
  if sanitized = "" then "default" else sanitized

let backend_config_for base_path =
  let raw_storage_type = storage_type_from_env () in
  let storage_type =
    if raw_storage_type = "auto" then auto_detect_backend ()
    else raw_storage_type
  in
  let postgres_url =
    match env_opt "MASC_POSTGRES_URL" with
    | Some _ as url -> url
    | None ->
        (* Fallback chain: DATABASE_URL -> RAILWAY_PG_URL *)
        match env_opt "DATABASE_URL" with
        | Some _ as url -> url
        | None -> env_opt "RAILWAY_PG_URL"
  in
  let cluster_name =
    match env_opt "MASC_CLUSTER_NAME" with
    | Some name -> name
    | None -> "default"
  in
  let backend_type =
    match storage_type with
    | "postgres" | "postgresql" -> Backend.PostgresNative
    | "memory" -> Backend.Memory
    | _ -> Backend.FileSystem
  in
  let masc_root = Filename.concat base_path ".masc" in
  let cluster_segment =
    match cluster_name with
    | "" | "default" -> None
    | other -> Some (sanitize_namespace_segment other)
  in
  let backend_base_path =
    match cluster_segment with
    | None -> masc_root
    | Some seg -> Filename.concat (Filename.concat masc_root "clusters") seg
  in
  {
    Backend.backend_type;
    Backend.postgres_url;
    Backend.base_path = backend_base_path;
    Backend.cluster_name;
    Backend.node_id = Backend.generate_node_id ();
    Backend.pubsub_max_messages = Backend.pubsub_max_messages_from_env ();
  }

let create_backend cfg =
  match cfg.Backend.backend_type with
  | Backend.Memory ->
      (match Backend.MemoryBackend.create cfg with
       | Ok backend -> Ok (Memory backend)
       | Error e -> Error e)
  | Backend.FileSystem ->
      (match Backend.FileSystemBackend.create cfg with
       | Ok backend -> Ok (FileSystem backend)
       | Error e -> Error e)
  | Backend.PostgresNative ->
      (* PostgresNative requires Eio context - use create_backend_eio instead *)
      (match Backend.PostgresNative.create cfg with
       | Ok backend -> Ok (PostgresNative backend)
       | Error e -> Error e)

(** Create backend with Eio context - required for PostgresNative *)
let create_backend_eio ~sw ~env cfg =
  match cfg.Backend.backend_type with
  | Backend.PostgresNative ->
      (match Backend.PostgresNative.create_eio ~sw ~env cfg with
       | Ok backend -> Ok (PostgresNative backend)
       | Error e -> Error e)
  | _ ->
      (* Non-Eio backends can use the regular create_backend *)
      create_backend cfg

let default_config base_path =
  (* Resolve to git root for worktree support - all worktrees share same .masc/ *)
  let resolved_path = resolve_masc_base_path base_path in
  let backend_config = backend_config_for resolved_path in
  Log.Backend.info "MASC Backend: type=%s, postgres_url=%s"
    (Backend.show_backend_type backend_config.backend_type)
    (match backend_config.postgres_url with Some _ -> "<configured>" | None -> "none");
  let backend =
    match create_backend backend_config with
    | Ok backend ->
        Log.Backend.info "Backend initialized: %s"
          (match backend with
           | Memory _ -> "Memory"
           | FileSystem _ -> "FileSystem"
           | PostgresNative _ -> "PostgresNative");
        backend
    | Error e ->
        Log.Backend.warn "Backend init failed (%s). Falling back to filesystem."
          (Backend.show_error e);
        let fallback_cfg =
          { backend_config with Backend.backend_type = Backend.FileSystem }
        in
        (match Backend.FileSystemBackend.create fallback_cfg with
         | Ok fs -> FileSystem fs
         | Error _ ->
             (* Final fallback: in-memory to keep server alive *)
             (match Backend.MemoryBackend.create fallback_cfg with
              | Ok mem -> Memory mem
              | Error _ -> failwith "Failed to initialize any MASC backend"))
  in
  {
    base_path = resolved_path;  (* Use resolved path (git root for worktrees) *)
    lock_expiry_minutes = 30;
    backend_config;
    backend;
  }

(** Create config with Eio context - required for PostgresNative backend *)
let default_config_eio ~sw ~env base_path =
  let resolved_path = resolve_masc_base_path base_path in
  let backend_config = backend_config_for resolved_path in
  Log.Backend.info "MASC Backend: type=%s, postgres_url=%s"
    (Backend.show_backend_type backend_config.backend_type)
    (match backend_config.postgres_url with Some _ -> "<configured>" | None -> "none");
  let backend =
    match create_backend_eio ~sw ~env backend_config with
    | Ok backend ->
        Log.Backend.info "Backend initialized: %s"
          (match backend with
           | Memory _ -> "Memory"
           | FileSystem _ -> "FileSystem"
           | PostgresNative _ -> "PostgresNative");
        backend
    | Error e ->
        Log.Backend.warn "Backend init failed (%s). Falling back to filesystem."
          (Backend.show_error e);
        let fallback_cfg =
          { backend_config with Backend.backend_type = Backend.FileSystem }
        in
        (match Backend.FileSystemBackend.create fallback_cfg with
         | Ok fs -> FileSystem fs
         | Error _ ->
             (match Backend.MemoryBackend.create fallback_cfg with
              | Ok mem -> Memory mem
              | Error _ -> failwith "Failed to initialize any MASC backend"))
  in
  {
    base_path = resolved_path;
    lock_expiry_minutes = 30;
    backend_config;
    backend;
  }

(* ============================================ *)
(* Path utilities                               *)
(* ============================================ *)

let masc_root_dir config =
  let masc_root = Filename.concat config.base_path ".masc" in
  let cluster_name = config.backend_config.Backend.cluster_name in
  match cluster_name with
  | "" | "default" -> masc_root
  | other ->
      let seg = sanitize_namespace_segment other in
      Filename.concat (Filename.concat masc_root "clusters") seg

let rooms_root_dir config = Filename.concat (masc_root_dir config) "rooms"
let registry_root_path config = Filename.concat (masc_root_dir config) "rooms.json"
let current_room_root_path config = Filename.concat (masc_root_dir config) "current_room"

(* Legacy paths (pre-room refactor) kept for backward compatibility *)
let legacy_rooms_root_dir config = Filename.concat config.base_path "rooms"
let legacy_registry_root_path config = Filename.concat config.base_path "rooms.json"
let legacy_current_room_path config = Filename.concat config.base_path "current_room"

(** Read current room ID from .masc/current_room with legacy fallback. *)
let read_current_room config =
  let read_from path =
    match Safe_ops.read_file_safe path with
    | Ok content ->
      let trimmed = String.trim content in
      if trimmed = "" then None
      else
        (match String.split_on_char '\n' trimmed with
         | line :: _ -> Some (String.trim line)
         | [] -> None)
    | Error _ -> None
  in
  match read_from (current_room_root_path config) with
  | Some room_id -> Some room_id
  | None ->
      (match read_from (legacy_current_room_path config) with
       | Some legacy_room -> Some legacy_room
       | None -> Some "default")

let room_dir_for config room_id =
  if room_id = "default" then
    masc_root_dir config
  else
    let root_path = Filename.concat (rooms_root_dir config) room_id in
    let legacy_path = Filename.concat (legacy_rooms_root_dir config) room_id in
    if Sys.file_exists root_path then root_path
    else if Sys.file_exists legacy_path then legacy_path
    else root_path

let masc_dir config =
  match read_current_room config with
  | Some room_id -> room_dir_for config room_id
  | None -> masc_root_dir config

let agents_dir config = Filename.concat (masc_dir config) "agents"
let tasks_dir config = Filename.concat (masc_dir config) "tasks"
let messages_dir config = Filename.concat (masc_dir config) "messages"
let state_path config = Filename.concat (masc_dir config) "state.json"
let backlog_path config = Filename.concat (tasks_dir config) "backlog.json"
let archive_path config = Filename.concat (masc_dir config) "tasks-archive.json"

(* ============================================ *)
(* Backend dispatch functions                   *)
(* ============================================ *)

let backend_get config ~key =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.get t ~key
  | FileSystem t -> Backend.FileSystemBackend.get t ~key
  | PostgresNative t -> Backend.PostgresNative.get t ~key

let backend_set config ~key ~value =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.set t ~key ~value
  | FileSystem t -> Backend.FileSystemBackend.set t ~key ~value
  | PostgresNative t -> Backend.PostgresNative.set t ~key ~value

let backend_delete config ~key =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.delete t ~key
  | FileSystem t -> Backend.FileSystemBackend.delete t ~key
  | PostgresNative t -> Backend.PostgresNative.delete t ~key

let backend_exists config ~key =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.exists t ~key
  | FileSystem t -> Backend.FileSystemBackend.exists t ~key
  | PostgresNative t -> Backend.PostgresNative.exists t ~key

let backend_list_keys config ~prefix =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.list_keys t ~prefix
  | FileSystem t -> Backend.FileSystemBackend.list_keys t ~prefix
  | PostgresNative t -> Backend.PostgresNative.list_keys t ~prefix

let backend_get_all config ~prefix =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.get_all t ~prefix
  | FileSystem t -> Backend.FileSystemBackend.get_all t ~prefix
  | PostgresNative t -> Backend.PostgresNative.get_all t ~prefix

let lock_count config =
  let is_active now value =
    match Safe_ops.parse_json_safe ~context:"lock_is_active" value with
    | Error _ -> true  (* If we can't parse, assume active (safe default) *)
    | Ok json ->
      let open Yojson.Safe.Util in
      match json |> member "expires_at" |> to_float_option with
      | Some expires_at -> expires_at > now
      | None -> true
  in
  let now = Unix.gettimeofday () in
  match backend_get_all config ~prefix:"locks:" with
  | Ok pairs ->
      List.fold_left (fun acc (_key, value) ->
        if is_active now value then acc + 1 else acc
      ) 0 pairs
  | Error _ -> 0

let backend_set_if_not_exists config ~key ~value =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.set_if_not_exists t ~key ~value
  | FileSystem t -> Backend.FileSystemBackend.set_if_not_exists t ~key ~value
  | PostgresNative t -> Backend.PostgresNative.set_if_not_exists t ~key ~value

let backend_acquire_lock config ~key ~ttl_seconds ~owner =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.acquire_lock t ~key ~ttl_seconds ~owner
  | FileSystem t -> Backend.FileSystemBackend.acquire_lock t ~key ~ttl_seconds ~owner
  | PostgresNative t -> Backend.PostgresNative.acquire_lock t ~key ~ttl_seconds ~owner

let backend_release_lock config ~key ~owner =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.release_lock t ~key ~owner
  | FileSystem t -> Backend.FileSystemBackend.release_lock t ~key ~owner
  | PostgresNative t -> Backend.PostgresNative.release_lock t ~key ~owner

let backend_extend_lock config ~key ~ttl_seconds ~owner =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.extend_lock t ~key ~ttl_seconds ~owner
  | FileSystem t -> Backend.FileSystemBackend.extend_lock t ~key ~ttl_seconds ~owner
  | PostgresNative t -> Backend.PostgresNative.extend_lock t ~key ~ttl_seconds ~owner

let backend_health_check config =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.health_check t
  | FileSystem t -> Backend.FileSystemBackend.health_check t
  | PostgresNative t -> Backend.PostgresNative.health_check t

let backend_publish config ~channel ~message =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.publish t ~channel ~message
  | FileSystem t -> Backend.FileSystemBackend.publish t ~channel ~message
  | PostgresNative t -> Backend.PostgresNative.publish t ~channel ~message

let backend_subscribe config ~channel ~callback =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.subscribe t ~channel ~callback
  | FileSystem t -> Backend.FileSystemBackend.subscribe t ~channel ~callback
  | PostgresNative t -> Backend.PostgresNative.subscribe t ~channel ~callback

let backend_name config =
  match config.backend with
  | Memory _ -> "memory"
  | FileSystem _ -> "filesystem"
  | PostgresNative _ -> "postgres-native"

(** Cleanup pubsub messages - only effective for PostgreSQL backend.
    Other backends use FS cleanup in gc or are ephemeral.
    Returns the number of deleted messages. *)
let backend_cleanup_pubsub config ~days ~max_messages =
  match config.backend with
  | PostgresNative t -> Backend.PostgresNative.cleanup_pubsub t ~days ~max_messages
  | Memory _ | FileSystem _ ->
      (* No-op for non-PostgreSQL backends:
         - FileSystem: handled separately by gc (file deletion)
         - Memory: ephemeral, no persistence *)
      Ok 0

(* ============================================ *)
(* Key/path conversion                          *)
(* ============================================ *)

(** Generate a short hash prefix for project isolation.
    Uses first 8 chars of MD5 hash of base_path.
    This ensures different test directories get different keys. *)
let project_prefix config =
  let hash = Digest.string config.base_path |> Digest.to_hex in
  String.sub hash 0 8

(** Convert absolute path to backend key.
    For distributed backends: includes project hash prefix for isolation.
    Example: /tmp/test-abc/.masc/state.json -> "a1b2c3d4:state.json"
    For filesystem: returns relative path without prefix. *)
let key_of_path_from_root config ~root path =
  let prefix = root ^ "/" in
  if String.length path >= String.length prefix &&
     String.sub path 0 (String.length prefix) = prefix then
    let rel =
      String.sub path (String.length prefix) (String.length path - String.length prefix)
    in
    let key = String.map (fun c -> if c = '/' then ':' else c) rel in
    match config.backend with
    | Memory _ | PostgresNative _ -> Some (project_prefix config ^ ":" ^ key)
    | FileSystem _ -> Some key
  else
    None

(* Key mapping is always relative to the .masc root so room paths
   are preserved in the backend key (e.g., rooms:my-room:state.json). *)
let key_of_path config path = key_of_path_from_root config ~root:(masc_root_dir config) path
let root_key_of_path config path = key_of_path_from_root config ~root:(masc_root_dir config) path

let strip_prefix prefix s =
  if String.length s >= String.length prefix then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else
    s

let list_dir config path =
  match key_of_path config path with
  | None ->
      if Sys.file_exists path && Sys.is_directory path then
        Sys.readdir path |> Array.to_list
      else
        []
  | Some key_prefix ->
      let prefix = key_prefix ^ ":" in
      (match backend_list_keys config ~prefix with
       | Ok keys ->
           List.map (fun key ->
             let rest = strip_prefix prefix key in
             String.map (fun c -> if c = ':' then '/' else c) rest
           ) keys
       | Error _ -> [])

(* ============================================ *)
(* Initialization check                         *)
(* ============================================ *)

let root_state_path config = Filename.concat (masc_root_dir config) "root-state.json"

(* Legacy root marker before root/room split lived at .masc/state.json. *)
let legacy_root_state_path config = Filename.concat (masc_root_dir config) "state.json"

(** Root initialization check - independent of current room.
    Used by room/cluster management features. *)
let root_is_initialized config =
  match config.backend with
  | Memory _ | PostgresNative _ ->
      let exists_root path ~fallback_key =
        let key =
          match root_key_of_path config path with
          | Some k -> k
          | None -> fallback_key
        in
        backend_exists config ~key
      in
      exists_root (root_state_path config) ~fallback_key:"root-state.json" ||
      exists_root (legacy_root_state_path config) ~fallback_key:"state.json"
  | FileSystem _ ->
      Sys.file_exists (masc_root_dir config) &&
      Sys.is_directory (masc_root_dir config) &&
      (Sys.file_exists (root_state_path config) || Sys.file_exists (legacy_root_state_path config))

(** Check if current room is initialized - backend-agnostic *)
let is_initialized config =
  match config.backend with
  | Memory _ | PostgresNative _ ->
      let state_key =
        match key_of_path config (state_path config) with
        | Some k -> k
        | None -> "state.json"
      in
      backend_exists config ~key:state_key
  | FileSystem _ ->
      Sys.file_exists (masc_dir config) &&
      Sys.is_directory (masc_dir config) &&
      Sys.file_exists (state_path config)

(* ============================================ *)
(* Validation helpers                           *)
(* ============================================ *)

let validate_agent_name name =
  (* Delegate to Validation module for consistent security checks *)
  Validation.Agent_id.validate name

let validate_task_id id =
  (* Delegate to Validation module for consistent security checks *)
  Validation.Task_id.validate id

let contains_substring haystack needle =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len > haystack_len then false
  else
    let rec check i =
      if i > haystack_len - needle_len then false
      else if String.sub haystack i needle_len = needle then true
      else check (i + 1)
    in
    check 0

let validate_file_path path =
  (* Delegate to Validation module for consistent security checks *)
  (* Additional length check for file paths *)
  if String.length path > 500 then Error "File path too long (max 500 chars)"
  else if contains_substring path "<" || contains_substring path ">" then
    Error "Invalid characters in path (security)"
  else Validation.Safe_path.validate_relative path

(* ============================================ *)
(* Sanitization helpers                         *)
(* ============================================ *)

let sanitize_html str =
  let buf = Buffer.create (String.length str) in
  String.iter (fun c ->
    match c with
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&#x27;"
    | _ -> Buffer.add_char buf c
  ) str;
  Buffer.contents buf

let sanitize_agent_name = sanitize_html
let sanitize_message = sanitize_html

let safe_filename name =
  let buf = Buffer.create (String.length name * 3) in
  String.iter (fun c ->
    let valid =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c = '.' || c = '_' || c = '-'
    in
    if valid then
      Buffer.add_char buf c
    else
      Buffer.add_string buf (Printf.sprintf "_%02x" (Char.code c))
  ) name;
  Buffer.contents buf

(* ============================================ *)
(* Result-returning validators                  *)
(* ============================================ *)

let validate_agent_name_r name : (string, masc_error) result =
  (* Delegate to Validation module, convert error type *)
  match Validation.Agent_id.validate name with
  | Ok _ -> Ok name
  | Error msg -> Error (InvalidAgentName msg)

let validate_task_id_r id : (string, masc_error) result =
  (* Delegate to Validation module, convert error type *)
  match Validation.Task_id.validate id with
  | Ok _ -> Ok id
  | Error msg -> Error (InvalidTaskId msg)

let validate_file_path_r path : (string, masc_error) result =
  (* Delegate to Validation module, convert error type *)
  if String.length path > 500 then Error (InvalidFilePath "too long (max 500 chars)")
  else if contains_substring path "<" || contains_substring path ">" then
    Error (InvalidFilePath "invalid characters (security)")
  else match Validation.Safe_path.validate_relative path with
  | Ok _ -> Ok path
  | Error msg -> Error (InvalidFilePath msg)

(* ============================================ *)
(* Ensure initialized                           *)
(* ============================================ *)

let ensure_initialized config =
  if not (is_initialized config) then
    failwith "MASC not initialized. Use masc_init first."

let ensure_initialized_r config : (unit, masc_error) result =
  if is_initialized config then Ok ()
  else Error NotInitialized

(* ============================================ *)
(* File I/O helpers                             *)
(* ============================================ *)

let rec mkdir_p path =
  if not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let read_json_local path =
  match Safe_ops.read_json_file_safe path with
  | Ok json -> json
  | Error _ -> `Assoc []

let write_json_local path json =
  mkdir_p (Filename.dirname path);
  let content = Yojson.Safe.pretty_to_string json in
  let tmp_path = path ^ ".tmp" in
  Out_channel.with_open_text tmp_path (fun oc ->
    Out_channel.output_string oc content;
    Out_channel.flush oc
  );
  Unix.rename tmp_path path

(* Root-scoped JSON helpers for shared room registry/current_room metadata. *)
let read_json_root config path =
  match root_key_of_path config path with
  | Some key -> begin
      match backend_get config ~key with
      | Ok (Some content) ->
          (let trimmed = String.trim content in
           if trimmed = "" then `Assoc []
           else match Safe_ops.parse_json_safe ~context:"read_json_root" trimmed with
           | Ok json -> json
           | Error _ -> `Assoc [])
      | Ok None -> `Assoc []
      | Error _ -> `Assoc []
    end
  | None -> read_json_local path

let write_json_root config path json =
  match root_key_of_path config path with
  | Some key ->
      let content = Yojson.Safe.pretty_to_string json in
      let _ = backend_set config ~key ~value:content in
      ()
  | None -> write_json_local path json

let delete_path_root config path =
  match root_key_of_path config path with
  | Some key -> ignore (backend_delete config ~key)
  | None -> if Sys.file_exists path then Sys.remove path

let path_exists_root config path =
  match root_key_of_path config path with
  | Some key -> backend_exists config ~key
  | None -> Sys.file_exists path

let read_json config path =
  match key_of_path config path with
  | Some key -> begin
      match backend_get config ~key with
      | Ok (Some content) ->
          (let trimmed = String.trim content in
           if trimmed = "" then `Assoc []
           else match Safe_ops.parse_json_safe ~context:"read_json_root" trimmed with
           | Ok json -> json
           | Error _ -> `Assoc [])
      | Ok None -> `Assoc []
      | Error _ -> `Assoc []
    end
  | None -> read_json_local path

let write_json config path json =
  match key_of_path config path with
  | Some key ->
      let content = Yojson.Safe.pretty_to_string json in
      let _ = backend_set config ~key ~value:content in
      ()
  | None -> write_json_local path json

let delete_path config path =
  match key_of_path config path with
  | Some key -> ignore (backend_delete config ~key)
  | None -> if Sys.file_exists path then Sys.remove path

let path_exists config path =
  match key_of_path config path with
  | Some key -> backend_exists config ~key
  | None -> Sys.file_exists path

let read_json_opt config path =
  if path_exists config path then
    Some (read_json config path)
  else
    None

(* ============================================ *)
(* File locking                                 *)
(* ============================================ *)

let with_file_lock config path f =
  match key_of_path config path with
  | None ->
      let lock_path = path ^ ".lock" in
      mkdir_p (Filename.dirname lock_path);
      let fd = Unix.openfile lock_path [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.lockf fd Unix.F_ULOCK 0
           with Unix.Unix_error (err, _, _) ->
             Printf.eprintf "[WARN] Failed to release flock: %s\n%!" (Unix.error_message err));
          Unix.close fd)
        (fun () ->
          Unix.lockf fd Unix.F_LOCK 0;
          f ())
  | Some key ->
      let owner = config.backend_config.node_id in
      let ttl_seconds = config.lock_expiry_minutes * 60 in
      let rec acquire attempts =
        if attempts <= 0 then false
        else
          match backend_acquire_lock config ~key ~ttl_seconds ~owner with
          | Ok true -> true
          | _ ->
              Unix.sleepf 0.05;
              acquire (attempts - 1)
      in
      if acquire 20 then
        Fun.protect
          ~finally:(fun () -> ignore (backend_release_lock config ~key ~owner))
          f
      else
        failwith "Failed to acquire distributed lock"

let with_file_lock_r config path f : ('a, masc_error) result =
  match key_of_path config path with
  | None ->
      let lock_path = path ^ ".lock" in
      mkdir_p (Filename.dirname lock_path);
      let fd = Unix.openfile lock_path [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.lockf fd Unix.F_ULOCK 0
           with Unix.Unix_error (err, _, _) ->
             Printf.eprintf "[WARN] Failed to release flock: %s\n%!" (Unix.error_message err));
          Unix.close fd)
        (fun () ->
          Unix.lockf fd Unix.F_LOCK 0;
          Ok (f ()))
  | Some key ->
      let owner = config.backend_config.node_id in
      let ttl_seconds = config.lock_expiry_minutes * 60 in
      let rec acquire attempts =
        if attempts <= 0 then false
        else
          match backend_acquire_lock config ~key ~ttl_seconds ~owner with
          | Ok true -> true
          | _ -> Unix.sleepf 0.05; acquire (attempts - 1)
      in
      if acquire 20 then
        Fun.protect
          ~finally:(fun () -> ignore (backend_release_lock config ~key ~owner))
          (fun () -> Ok (f ()))
      else
        Error (FileLocked { file = path; by = "distributed lock timeout" })

(* ============================================ *)
(* Event logging                                *)
(* ============================================ *)

let log_event config event_json =
  let events_dir = Filename.concat (masc_dir config) "events" in
  mkdir_p events_dir;

  let today =
    let open Unix in
    let tm = gmtime (gettimeofday ()) in
    Printf.sprintf "%04d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
  in
  let month_dir = Filename.concat events_dir today in
  mkdir_p month_dir;

  let day =
    let open Unix in
    let tm = gmtime (gettimeofday ()) in
    Printf.sprintf "%02d.jsonl" tm.tm_mday
  in
  let log_file = Filename.concat month_dir day in

  let oc = open_out_gen [Open_append; Open_creat] 0o644 log_file in
  output_string oc (event_json ^ "\n");
  close_out oc
