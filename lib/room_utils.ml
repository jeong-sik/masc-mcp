(** Room Utilities - Shared helpers for Room module *)

open Types

(** Storage backend type - unified interface *)
type storage_backend =
  | Memory of Backend.MemoryBackend.t
  | FileSystem of Backend.FileSystemBackend.t
  | RedisRest of Backend.RedisBackend.t
  | RedisNative of Backend.RedisNative.t
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
  try
    let ic = open_in path in
    let content = input_line ic in
    close_in ic;
    Some content
  with _ -> None

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
    1. REDIS_URL / MASC_REDIS_URL - if available, use Redis for high-performance
    2. FileSystem - zero-dependency default for personal/small use *)
let auto_detect_backend () =
  if env_opt "REDIS_URL" <> None || env_opt "MASC_REDIS_URL" <> None then begin
    Log.Backend.info "Auto-detect: REDIS_URL found → Redis backend";
    "redis"
  end else begin
    Log.Backend.info "Auto-detect: No distributed DB found → FileSystem backend (default)";
    "filesystem"
  end

(* ============================================ *)
(* Backend creation                             *)
(* ============================================ *)

let backend_config_for base_path =
  let raw_storage_type = storage_type_from_env () in
  let storage_type =
    if raw_storage_type = "auto" then auto_detect_backend ()
    else raw_storage_type
  in
  let redis_url =
    match env_opt "MASC_REDIS_URL" with
    | Some _ as url -> url
    | None -> env_opt "REDIS_URL"  (* Fallback to standard REDIS_URL *)
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
    | "redis" -> Backend.Redis
    | "postgres" | "postgresql" -> Backend.PostgresNative
    | "memory" -> Backend.Memory
    | _ -> Backend.FileSystem
  in
  {
    Backend.backend_type;
    Backend.redis_url;
    Backend.postgres_url;
    Backend.base_path = Filename.concat base_path ".masc";
    Backend.cluster_name;
    Backend.node_id = Backend.generate_node_id ();
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
  | Backend.Redis ->
      (match cfg.redis_url with
       | None -> Error (Backend.ConnectionFailed "Redis URL not configured")
       | Some url ->
           if Backend.is_native_redis_url url then
             match Backend.RedisNative.create cfg with
             | Ok backend -> Ok (RedisNative backend)
             | Error e -> Error e
           else if Backend.is_rest_redis_url url then
             match Backend.RedisBackend.create cfg with
             | Ok backend -> Ok (RedisRest backend)
             | Error e -> Error e
           else
             Error (Backend.ConnectionFailed (Printf.sprintf "Unsupported Redis URL scheme: %s" url)))
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
  Log.Backend.info "MASC Backend: type=%s, redis_url=%s"
    (Backend.show_backend_type backend_config.backend_type)
    (match backend_config.redis_url with Some u -> u | None -> "none");
  let backend =
    match create_backend backend_config with
    | Ok backend ->
        Log.Backend.info "Backend initialized: %s"
          (match backend with
           | Memory _ -> "Memory"
           | FileSystem _ -> "FileSystem"
           | RedisRest _ -> "RedisRest"
           | RedisNative _ -> "RedisNative"
           | PostgresNative _ -> "PostgresNative");
        backend
    | Error e ->
        Log.Backend.warn "Backend init failed (%s). Falling back to filesystem."
          (Backend.show_error e);
        let fallback_cfg =
          { backend_config with Backend.backend_type = Backend.FileSystem; redis_url = None }
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
  Log.Backend.info "MASC Backend (Eio): type=%s, postgres_url=%s"
    (Backend.show_backend_type backend_config.backend_type)
    (match backend_config.postgres_url with Some _ -> "<configured>" | None -> "none");
  let backend =
    match create_backend_eio ~sw ~env backend_config with
    | Ok backend ->
        Log.Backend.info "Backend initialized (Eio): %s"
          (match backend with
           | Memory _ -> "Memory"
           | FileSystem _ -> "FileSystem"
           | RedisRest _ -> "RedisRest"
           | RedisNative _ -> "RedisNative"
           | PostgresNative _ -> "PostgresNative");
        backend
    | Error e ->
        Log.Backend.warn "Backend init failed (%s). Falling back to filesystem."
          (Backend.show_error e);
        let fallback_cfg =
          { backend_config with Backend.backend_type = Backend.FileSystem; redis_url = None }
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

let masc_dir config = Filename.concat config.base_path ".masc"
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
  | RedisRest t -> Backend.RedisBackend.get t ~key
  | RedisNative t -> Backend.RedisNative.get t ~key
  | PostgresNative t -> Backend.PostgresNative.get t ~key

let backend_set config ~key ~value =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.set t ~key ~value
  | FileSystem t -> Backend.FileSystemBackend.set t ~key ~value
  | RedisRest t -> Backend.RedisBackend.set t ~key ~value
  | RedisNative t -> Backend.RedisNative.set t ~key ~value
  | PostgresNative t -> Backend.PostgresNative.set t ~key ~value

let backend_delete config ~key =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.delete t ~key
  | FileSystem t -> Backend.FileSystemBackend.delete t ~key
  | RedisRest t -> Backend.RedisBackend.delete t ~key
  | RedisNative t -> Backend.RedisNative.delete t ~key
  | PostgresNative t -> Backend.PostgresNative.delete t ~key

let backend_exists config ~key =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.exists t ~key
  | FileSystem t -> Backend.FileSystemBackend.exists t ~key
  | RedisRest t -> Backend.RedisBackend.exists t ~key
  | RedisNative t -> Backend.RedisNative.exists t ~key
  | PostgresNative t -> Backend.PostgresNative.exists t ~key

let backend_list_keys config ~prefix =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.list_keys t ~prefix
  | FileSystem t -> Backend.FileSystemBackend.list_keys t ~prefix
  | RedisRest t -> Backend.RedisBackend.list_keys t ~prefix
  | RedisNative t -> Backend.RedisNative.list_keys t ~prefix
  | PostgresNative t -> Backend.PostgresNative.list_keys t ~prefix

let backend_get_all config ~prefix =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.get_all t ~prefix
  | FileSystem t -> Backend.FileSystemBackend.get_all t ~prefix
  | RedisRest t -> Backend.RedisBackend.get_all t ~prefix
  | RedisNative t -> Backend.RedisNative.get_all t ~prefix
  | PostgresNative t -> Backend.PostgresNative.get_all t ~prefix

let lock_count config =
  let is_active now value =
    try
      let open Yojson.Safe.Util in
      let json = Yojson.Safe.from_string value in
      match json |> member "expires_at" |> to_float_option with
      | Some expires_at -> expires_at > now
      | None -> true
    with _ -> true
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
  | RedisRest t -> Backend.RedisBackend.set_if_not_exists t ~key ~value
  | RedisNative t -> Backend.RedisNative.set_if_not_exists t ~key ~value
  | PostgresNative t -> Backend.PostgresNative.set_if_not_exists t ~key ~value

let backend_acquire_lock config ~key ~ttl_seconds ~owner =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.acquire_lock t ~key ~ttl_seconds ~owner
  | FileSystem t -> Backend.FileSystemBackend.acquire_lock t ~key ~ttl_seconds ~owner
  | RedisRest t -> Backend.RedisBackend.acquire_lock t ~key ~ttl_seconds ~owner
  | RedisNative t -> Backend.RedisNative.acquire_lock t ~key ~ttl_seconds ~owner
  | PostgresNative t -> Backend.PostgresNative.acquire_lock t ~key ~ttl_seconds ~owner

let backend_release_lock config ~key ~owner =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.release_lock t ~key ~owner
  | FileSystem t -> Backend.FileSystemBackend.release_lock t ~key ~owner
  | RedisRest t -> Backend.RedisBackend.release_lock t ~key ~owner
  | RedisNative t -> Backend.RedisNative.release_lock t ~key ~owner
  | PostgresNative t -> Backend.PostgresNative.release_lock t ~key ~owner

let backend_extend_lock config ~key ~ttl_seconds ~owner =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.extend_lock t ~key ~ttl_seconds ~owner
  | FileSystem t -> Backend.FileSystemBackend.extend_lock t ~key ~ttl_seconds ~owner
  | RedisRest t -> Backend.RedisBackend.extend_lock t ~key ~ttl_seconds ~owner
  | RedisNative t -> Backend.RedisNative.extend_lock t ~key ~ttl_seconds ~owner
  | PostgresNative t -> Backend.PostgresNative.extend_lock t ~key ~ttl_seconds ~owner

let backend_health_check config =
  match config.backend with
  | Memory t -> Backend.MemoryBackend.health_check t
  | FileSystem t -> Backend.FileSystemBackend.health_check t
  | RedisRest t -> Backend.RedisBackend.health_check t
  | RedisNative t -> Backend.RedisNative.health_check t
  | PostgresNative t -> Backend.PostgresNative.health_check t

let backend_name config =
  match config.backend with
  | Memory _ -> "memory"
  | FileSystem _ -> "filesystem"
  | RedisRest _ -> "redis-rest"
  | RedisNative _ -> "redis-native"
  | PostgresNative _ -> "postgres-native"

(* ============================================ *)
(* Key/path conversion                          *)
(* ============================================ *)

(** Generate a short hash prefix for project isolation.
    Uses first 8 chars of MD5 hash of base_path.
    This ensures different test directories get different Redis keys. *)
let project_prefix config =
  let hash = Digest.string config.base_path |> Digest.to_hex in
  String.sub hash 0 8

(** Convert absolute path to Redis/Memory key.
    For distributed backends: includes project hash prefix for isolation.
    Example: /tmp/test-abc/.masc/state.json -> "a1b2c3d4:state.json"
    For filesystem: returns relative path without prefix. *)
let key_of_path config path =
  let masc_root = masc_dir config in
  let prefix = masc_root ^ "/" in
  if String.length path >= String.length prefix &&
     String.sub path 0 (String.length prefix) = prefix then
    let rel = String.sub path (String.length prefix) (String.length path - String.length prefix) in
    let key = String.map (fun c -> if c = '/' then ':' else c) rel in
    (* For Redis/Memory/Postgres backends, add project prefix for isolation *)
    match config.backend with
    | RedisRest _ | RedisNative _ | Memory _ | PostgresNative _ ->
        Some (project_prefix config ^ ":" ^ key)
    | FileSystem _ ->
        Some key
  else
    None

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

(** Check if MASC room is initialized - backend-agnostic *)
let is_initialized config =
  match config.backend with
  | RedisRest _ | RedisNative _ | Memory _ | PostgresNative _ ->
      (* For distributed/memory/postgres backends, check state exists in backend *)
      let state_key = match key_of_path config (state_path config) with
        | Some k -> k
        | None -> "state.json"  (* fallback key *)
      in
      backend_exists config ~key:state_key
  | FileSystem _ ->
      (* For filesystem, check directory structure and state file *)
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
  if Sys.file_exists path then
    try
      let content = In_channel.with_open_text path In_channel.input_all in
      if String.trim content = "" then `Assoc []
      else Yojson.Safe.from_string content
    with _ -> `Assoc []
  else
    `Assoc []

let write_json_local path json =
  mkdir_p (Filename.dirname path);
  let content = Yojson.Safe.pretty_to_string json in
  let tmp_path = path ^ ".tmp" in
  Out_channel.with_open_text tmp_path (fun oc ->
    Out_channel.output_string oc content;
    Out_channel.flush oc
  );
  Unix.rename tmp_path path

let read_json config path =
  match key_of_path config path with
  | Some key -> begin
      match backend_get config ~key with
      | Ok (Some content) ->
          (try
             if String.trim content = "" then `Assoc []
             else Yojson.Safe.from_string content
           with _ -> `Assoc [])
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
          (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ());
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
          (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ());
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
