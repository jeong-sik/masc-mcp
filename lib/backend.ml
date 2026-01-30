(** Backend Module - Storage abstraction for MASC (Memory/FileSystem/PostgreSQL) *)

(* ============================================ *)
(* Backend Types                                *)
(* ============================================ *)

type backend_type =
  | Memory
  | FileSystem
  | PostgresNative  (* Eio-native PostgreSQL via caqti-eio *)
[@@deriving show, eq]

type error =
  | ConnectionFailed of string
  | KeyNotFound of string
  | OperationFailed of string
  | BackendNotSupported of string
  | InvalidKey of string
[@@deriving show]

type config = {
  backend_type: backend_type;
  base_path: string;
  postgres_url: string option;  (* PostgreSQL connection URL for PostgresNative backend *)
  node_id: string;
  cluster_name: string;
  pubsub_max_messages: int;     (* Max messages per channel before LTRIM, default: 1000 *)
}

(** Get pubsub max messages from env or default *)
let pubsub_max_messages_from_env () =
  match Sys.getenv_opt "MASC_PUBSUB_MAX_MESSAGES" with
  | Some s -> Safe_ops.int_of_string_with_default ~default:1000 s
  | None -> 1000

let generate_node_id () =
  let hostname = try Unix.gethostname () with Unix.Unix_error _ -> "unknown" in
  let pid = Unix.getpid () in
  let rand = Random.int 10000 in
  Printf.sprintf "%s-%d-%04d" hostname pid rand

let default_config = {
  backend_type = FileSystem;
  base_path = ".masc";
  postgres_url = None;
  node_id = generate_node_id ();
  cluster_name = "default";
  pubsub_max_messages = pubsub_max_messages_from_env ();
}

let get_status config : Yojson.Safe.t =
  let backend_str = match config.backend_type with
    | Memory -> "memory"
    | FileSystem -> "filesystem"
    | PostgresNative -> "postgres_native"
  in
  `Assoc [
    ("backend_type", `String backend_str);
    ("base_path", `String config.base_path);
    ("node_id", `String config.node_id);
    ("cluster_name", `String config.cluster_name);
    ("postgres_url", match config.postgres_url with Some u -> `String u | None -> `Null);
  ]

(* ============================================ *)
(* Backend Interface                            *)
(* ============================================ *)

module type BACKEND = sig
  type t

  val create : config -> (t, error) result
  val close : t -> unit

  (* Basic KV operations *)
  val get : t -> key:string -> (string option, error) result
  val set : t -> key:string -> value:string -> (unit, error) result
  val delete : t -> key:string -> (bool, error) result
  val exists : t -> key:string -> bool

  (* List operations *)
  val list_keys : t -> prefix:string -> (string list, error) result
  val get_all : t -> prefix:string -> ((string * string) list, error) result

  (* Atomic operations *)
  val set_if_not_exists : t -> key:string -> value:string -> (bool, error) result
  val compare_and_swap : t -> key:string -> expected:string -> value:string -> (bool, error) result

  (* Distributed locking *)
  val acquire_lock : t -> key:string -> ttl_seconds:int -> owner:string -> (bool, error) result
  val release_lock : t -> key:string -> owner:string -> (bool, error) result
  val extend_lock : t -> key:string -> ttl_seconds:int -> owner:string -> (bool, error) result

  (* Pub/Sub (optional, not all backends support) *)
  val publish : t -> channel:string -> message:string -> (int, error) result
  val subscribe : t -> channel:string -> callback:(string -> unit) -> (unit, error) result

  (* Health check *)
  val health_check : t -> (bool, error) result
end

(* ============================================ *)
(* Safety Utilities                             *)
(* ============================================ *)
(* NOTE: validate_key is defined locally in each backend module.
   These utilities are for cross-cutting concerns. *)

(** Validate TTL to prevent invalid durations.
    Returns sanitized TTL (minimum 1, maximum 86400 = 24h) *)
let validate_ttl ttl_seconds =
  if ttl_seconds <= 0 then 1
  else if ttl_seconds > 86400 then 86400
  else ttl_seconds

(** Safely parse JSON lock file, returning None on any error.
    Also removes corrupted files to allow recovery. *)
let safe_parse_lock_json file_path =
  if not (Sys.file_exists file_path) then None
  else
    try
      let content = In_channel.with_open_text file_path In_channel.input_all in
      if String.length content = 0 then begin
        (* Empty file is corrupted - remove it *)
        Safe_ops.remove_file_logged ~context:"backend_lock" file_path;
        None
      end else
        let json = Yojson.Safe.from_string content in
        let open Yojson.Safe.Util in
        let parse_float_field field =
          match json |> member field with
          | `Float f -> Some f
          | `Int i -> Some (float_of_int i)
          | `Intlit s -> float_of_string_opt s
          | `String s -> float_of_string_opt s
          | _ -> None
        in
        let parse_string_field field =
          match json |> member field with
          | `String s -> Some s
          | _ -> None
        in
        match parse_string_field "owner", parse_float_field "expires_at" with
        | Some own, Some exp -> Some (own, exp)
        | _ ->
            Safe_ops.remove_file_logged ~context:"backend_lock" file_path;
            None
    with
    | _ ->
        (* Corrupted JSON file - remove it to allow recovery *)
        Safe_ops.remove_file_logged ~context:"backend_lock" file_path;
        None

(** Acquire exclusive file lock using Unix.lockf.
    Returns true if lock acquired, false if would block. *)
let acquire_flock fd =
  try
    Unix.lockf fd Unix.F_TLOCK 0;
    true
  with
  | Unix.Unix_error (Unix.EAGAIN, _, _)
  | Unix.Unix_error (Unix.EACCES, _, _) -> false
  | _ -> false

(** Release file lock *)
let release_flock fd =
  try Unix.lockf fd Unix.F_ULOCK 0
  with Unix.Unix_error (err, _, _) ->
    Printf.eprintf "[WARN] Failed to release flock: %s\n%!" (Unix.error_message err)

(* ============================================ *)
(* Memory Backend (In-Process)                  *)
(* ============================================ *)

module MemoryBackend : BACKEND = struct
  type lock_info = {
    owner: string;
    expires_at: float;
  }

  type t = {
    data: (string, string) Hashtbl.t;
    locks: (string, lock_info) Hashtbl.t;
    mutex: Mutex.t;
  }

  let with_lock t f =
    Mutex.lock t.mutex;
    let result = try f () with e -> Mutex.unlock t.mutex; raise e in
    Mutex.unlock t.mutex;
    result

  let create (_cfg : config) : (t, error) result =
    Ok {
      data = Hashtbl.create 1000;
      locks = Hashtbl.create 100;
      mutex = Mutex.create ();
    }

  let close _t = ()

  let get t ~key =
    with_lock t (fun () ->
      Ok (Hashtbl.find_opt t.data key)
    )

  let set t ~key ~value =
    with_lock t (fun () ->
      Hashtbl.replace t.data key value;
      Ok ()
    )

  let delete t ~key =
    with_lock t (fun () ->
      let existed = Hashtbl.mem t.data key in
      Hashtbl.remove t.data key;
      Ok existed
    )

  let exists t ~key =
    with_lock t (fun () ->
      Hashtbl.mem t.data key
    )

  let list_keys t ~prefix =
    with_lock t (fun () ->
      let keys = Hashtbl.fold (fun k _ acc ->
        if String.length k >= String.length prefix &&
           String.sub k 0 (String.length prefix) = prefix
        then k :: acc
        else acc
      ) t.data [] in
      Ok (List.sort compare keys)
    )

  let get_all t ~prefix =
    with_lock t (fun () ->
      let pairs = Hashtbl.fold (fun k v acc ->
        if String.length k >= String.length prefix &&
           String.sub k 0 (String.length prefix) = prefix
        then (k, v) :: acc
        else acc
      ) t.data [] in
      Ok (List.sort (fun (a, _) (b, _) -> compare a b) pairs)
    )

  let set_if_not_exists t ~key ~value =
    with_lock t (fun () ->
      if Hashtbl.mem t.data key then
        Ok false
      else begin
        Hashtbl.add t.data key value;
        Ok true
      end
    )

  let compare_and_swap t ~key ~expected ~value =
    with_lock t (fun () ->
      match Hashtbl.find_opt t.data key with
      | Some current when current = expected ->
          Hashtbl.replace t.data key value;
          Ok true
      | _ ->
          Ok false
    )

  let acquire_lock t ~key ~ttl_seconds ~owner =
    with_lock t (fun () ->
      let now = Unix.gettimeofday () in
      match Hashtbl.find_opt t.locks key with
      | Some lock when lock.expires_at > now && lock.owner <> owner ->
          Ok false  (* Locked by someone else *)
      | _ ->
          let expires_at = now +. float_of_int ttl_seconds in
          Hashtbl.replace t.locks key { owner; expires_at };
          Ok true
    )

  let release_lock t ~key ~owner =
    with_lock t (fun () ->
      match Hashtbl.find_opt t.locks key with
      | Some lock when lock.owner = owner ->
          Hashtbl.remove t.locks key;
          Ok true
      | _ ->
          Ok false
    )

  let extend_lock t ~key ~ttl_seconds ~owner =
    with_lock t (fun () ->
      match Hashtbl.find_opt t.locks key with
      | Some lock when lock.owner = owner ->
          let expires_at = Unix.gettimeofday () +. float_of_int ttl_seconds in
          Hashtbl.replace t.locks key { lock with expires_at };
          Ok true
      | _ ->
          Ok false
    )

  let publish _t ~channel:_ ~message:_ =
    Error (BackendNotSupported "Memory backend does not support pub/sub")

  let subscribe _t ~channel:_ ~callback:_ =
    Error (BackendNotSupported "Memory backend does not support pub/sub")

  let health_check _t = Ok true
end

(* ============================================ *)
(* FileSystem Backend                           *)
(* ============================================ *)

module FileSystemBackend : BACKEND = struct
  type t = {
    base_path: string;
    mutex: Mutex.t;
  }

  let with_lock t f =
    Mutex.lock t.mutex;
    let result = try f () with e -> Mutex.unlock t.mutex; raise e in
    Mutex.unlock t.mutex;
    result

  (* Security: validate key with strict allowlist (parse, don't sanitize) *)
  let validate_key key =
    (* Reject empty keys *)
    if String.length key = 0 then
      raise (Invalid_argument "Empty key not allowed");

    (* Reject NUL bytes (C string truncation attack) *)
    if String.contains key '\x00' then
      raise (Invalid_argument "NUL byte not allowed in key");

    (* Reject '/' anywhere (we use ':' as path separator) *)
    if String.contains key '/' then
      raise (Invalid_argument "Slash not allowed in key (use ':' as separator)");

    (* Reject keys starting or ending with ':' (would create absolute/trailing path) *)
    if key.[0] = ':' then
      raise (Invalid_argument "Key cannot start with ':'");
    if key.[String.length key - 1] = ':' then
      raise (Invalid_argument "Key cannot end with ':'");

    (* Reject consecutive colons (empty path segment) *)
    if String.length key >= 2 then begin
      for i = 0 to String.length key - 2 do
        if key.[i] = ':' && key.[i+1] = ':' then
          raise (Invalid_argument "Consecutive colons not allowed")
      done
    end;

    (* Check each segment for path traversal and allowlist *)
    let segments = String.split_on_char ':' key in
    List.iter (fun seg ->
      (* Reject . and .. segments *)
      if seg = "." || seg = ".." then
        raise (Invalid_argument "Path traversal detected (. or ..)");
      (* Reject segments starting with .. *)
      if String.length seg >= 2 && String.sub seg 0 2 = ".." then
        raise (Invalid_argument "Path traversal detected");
      (* Blocklist: reject only dangerous characters for path safety *)
      (* Allow UTF-8 (bytes >= 0x80) and most printable ASCII *)
      String.iter (fun c ->
        let code = Char.code c in
        let dangerous =
          code = 0 ||                    (* null byte *)
          code < 32 ||                   (* control characters *)
          c = '/' || c = '\\' ||         (* path separators *)
          c = ':' ||                     (* key separator (should be split already) *)
          c = '*' || c = '?' ||          (* wildcards *)
          c = '"' || c = '\'' ||         (* quotes *)
          c = '<' || c = '>' || c = '|'  (* shell metacharacters *)
        in
        if dangerous then
          raise (Invalid_argument (Printf.sprintf "Invalid character (code=%d) in key" code))
      ) seg
    ) segments;

    key  (* Return unchanged - validation only, no sanitization *)

  let key_to_path t key =
    let safe_key = validate_key key in
    let path_part = String.map (function ':' -> '/' | c -> c) safe_key in
    (* Double-check: path_part must not start with '/' after conversion *)
    if String.length path_part > 0 && path_part.[0] = '/' then
      raise (Invalid_argument "Internal error: path starts with /");
    Filename.concat t.base_path path_part

  let safe_key_to_path t key =
    try Ok (key_to_path t key)
    with Invalid_argument msg -> Error (InvalidKey msg)

  let ensure_dir path =
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then
      let rec mkdir_p d =
        if not (Sys.file_exists d) then begin
          mkdir_p (Filename.dirname d);
          try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
        end
      in
      mkdir_p dir

  let create (cfg : config) : (t, error) result =
    let path = cfg.base_path in
    (try
      if not (Sys.file_exists path) then
        Unix.mkdir path 0o755
    with Unix.Unix_error (err, _, _) ->
      Printf.eprintf "[WARN] Failed to mkdir %s: %s\n%!" path (Unix.error_message err));
    Ok { base_path = path; mutex = Mutex.create () }

  let close _t = ()

  let get t ~key =
    with_lock t (fun () ->
      match safe_key_to_path t key with
      | Error e -> Error e
      | Ok path ->
          if Sys.file_exists path then
            match Safe_ops.read_file_safe path with
            | Ok content -> Ok (Some content)
            | Error _ -> Ok None
          else
            Ok None
    )

  let set t ~key ~value =
    with_lock t (fun () ->
      match safe_key_to_path t key with
      | Error e -> Error e
      | Ok path ->
          ensure_dir path;
          try
            Out_channel.with_open_text path (fun oc ->
              Out_channel.output_string oc value
            );
            Ok ()
          with e -> Error (OperationFailed (Printexc.to_string e))
    )

  let delete t ~key =
    with_lock t (fun () ->
      match safe_key_to_path t key with
      | Error e -> Error e
      | Ok path ->
          if Sys.file_exists path then begin
            try
              Sys.remove path;
              Ok true
            with e -> Error (OperationFailed (Printexc.to_string e))
          end else
            Ok false
    )

  let exists t ~key =
    with_lock t (fun () ->
      match safe_key_to_path t key with
      | Error _ -> false
      | Ok path -> Sys.file_exists path
    )

  let list_keys t ~prefix =
    with_lock t (fun () ->
      match safe_key_to_path t prefix with
      | Error e -> Error e
      | Ok prefix_path ->
          let dir = Filename.dirname prefix_path in
          if Sys.file_exists dir && Sys.is_directory dir then begin
            let files = Sys.readdir dir |> Array.to_list in
            let prefix_base = Filename.basename prefix_path in
            let matching = List.filter (fun f ->
              String.length f >= String.length prefix_base &&
              String.sub f 0 (String.length prefix_base) = prefix_base
            ) files in
            Ok (List.map (fun f -> prefix ^ String.sub f (String.length prefix_base) (String.length f - String.length prefix_base)) matching)
          end else
            Ok []
    )

  let get_all t ~prefix =
    match list_keys t ~prefix with
    | Error e -> Error e
    | Ok keys ->
        let pairs = List.filter_map (fun k ->
          match get t ~key:k with
          | Ok (Some v) -> Some (k, v)
          | _ -> None
        ) keys in
        Ok pairs

  (* Atomic set using O_EXCL *)
  let set_if_not_exists t ~key ~value =
    with_lock t (fun () ->
      match safe_key_to_path t key with
      | Error e -> Error e
      | Ok path ->
          ensure_dir path;
          try
            let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o644 in
            let _ = Unix.write_substring fd value 0 (String.length value) in
            Unix.close fd;
            Ok true
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok false
          | e -> Error (OperationFailed (Printexc.to_string e))
    )

  let compare_and_swap t ~key ~expected ~value =
    with_lock t (fun () ->
      match get t ~key with
      | Ok (Some current) when current = expected ->
          (match set t ~key ~value with
           | Ok () -> Ok true
           | Error e -> Error e)
      | _ -> Ok false
    )

  (* File-based locking with JSON metadata *)
  (* SAFETY: Uses validate_ttl, safe_parse_lock_json, flock *)
  (* NOTE: key_to_path already calls validate_key for path traversal prevention *)
  let acquire_lock t ~key ~ttl_seconds ~owner =
    try
      (* TTL validation: sanitize to safe range *)
      let safe_ttl = validate_ttl ttl_seconds in
      with_lock t (fun () ->
        let lock_key = "locks:" ^ key in
        let path = key_to_path t lock_key in  (* calls validate_key internally *)
        ensure_dir path;
        let now = Unix.gettimeofday () in
        let expires_at = now +. float_of_int safe_ttl in

        (* File-level locking for cross-process safety *)
        let lock_file = path ^ ".flock" in
        let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
        if not (acquire_flock fd) then begin
          Unix.close fd;
          Ok false  (* Another process is modifying *)
        end else begin
          (* flock acquired - safe to read/write *)
          let result =
            try
              (* Check existing lock using safe parser *)
              let existing_valid =
                match safe_parse_lock_json path with
                | Some (own, exp) when exp > now && own <> owner -> Some own
                | _ -> None  (* Expired, same owner, or corrupted (removed) *)
              in

              match existing_valid with
              | Some _ -> Ok false  (* Locked by someone else *)
              | None ->
                  let json = `Assoc [
                    ("owner", `String owner);
                    ("expires_at", `Float expires_at);
                    ("acquired_at", `Float now);
                  ] in
                  Out_channel.with_open_text path (fun oc ->
                    Out_channel.output_string oc (Yojson.Safe.to_string json)
                  );
                  Ok true
            with exn ->
              Error (OperationFailed (Printexc.to_string exn))
          in
          release_flock fd;
          Unix.close fd;
          result
        end
      )
    with
    | Invalid_argument msg -> Error (InvalidKey msg)
    | exn -> Error (OperationFailed (Printexc.to_string exn))

  (* SAFETY: Uses safe_parse_lock_json, flock *)
  (* NOTE: key_to_path already calls validate_key for path traversal prevention *)
  let release_lock t ~key ~owner =
    try
      with_lock t (fun () ->
        let lock_key = "locks:" ^ key in
        let file_path = key_to_path t lock_key in  (* calls validate_key internally *)

        (* Check if lock file exists first *)
        if not (Sys.file_exists file_path) then
          Ok false  (* No lock file exists *)
        else begin
          (* File-level locking for cross-process safety *)
          let lock_file = file_path ^ ".flock" in
          let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
          if not (acquire_flock fd) then begin
            Unix.close fd;
            Ok false  (* Another process is modifying *)
          end else begin
            let result =
              try
                (* Check ownership using safe parser *)
                match safe_parse_lock_json file_path with
                | Some (own, _) when own = owner ->
                    Safe_ops.remove_file_logged ~context:"backend_lock" file_path;
                    Ok true
                | Some _ -> Ok false  (* Different owner *)
                | None -> Ok false    (* No valid lock *)
              with e ->
                Printf.eprintf "[WARN] Lock operation failed: %s\n%!" (Printexc.to_string e);
                Ok false
            in
            release_flock fd;
            Unix.close fd;
            result
          end
        end
      )
    with
    | Invalid_argument msg -> Error (InvalidKey msg)
    | exn -> Error (OperationFailed (Printexc.to_string exn))

  (* SAFETY: Uses validate_ttl, flock *)
  (* NOTE: key_to_path already calls validate_key for path traversal prevention *)
  let extend_lock t ~key ~ttl_seconds ~owner =
    try
      (* TTL validation: sanitize to safe range *)
      let safe_ttl = validate_ttl ttl_seconds in
      with_lock t (fun () ->
        let lock_key = "locks:" ^ key in
        let file_path = key_to_path t lock_key in  (* calls validate_key internally *)

        (* File-level locking for cross-process safety *)
        let lock_file = file_path ^ ".flock" in
        if not (Sys.file_exists file_path) then
          Ok false  (* No lock to extend *)
        else begin
          let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
          if not (acquire_flock fd) then begin
            Unix.close fd;
            Ok false  (* Another process is modifying *)
          end else begin
            let result =
              try
                let content = In_channel.with_open_text file_path In_channel.input_all in
                let json = Yojson.Safe.from_string content in
                let open Yojson.Safe.Util in
                let own = json |> member "owner" |> to_string in
                if own = owner then begin
                  let now = Unix.gettimeofday () in
                  let expires_at = now +. float_of_int safe_ttl in
                  let new_json = `Assoc [
                    ("owner", `String owner);
                    ("expires_at", `Float expires_at);
                    ("acquired_at", json |> member "acquired_at");
                  ] in
                  Out_channel.with_open_text file_path (fun oc ->
                    Out_channel.output_string oc (Yojson.Safe.to_string new_json)
                  );
                  Ok true
                end else
                  Ok false
              with e ->
                Printf.eprintf "[WARN] Lock operation failed: %s\n%!" (Printexc.to_string e);
                Ok false
            in
            release_flock fd;
            Unix.close fd;
            result
          end
        end
      )
    with
    | Invalid_argument msg -> Error (InvalidKey msg)
    | exn -> Error (OperationFailed (Printexc.to_string exn))

  let publish _t ~channel:_ ~message:_ =
    Error (BackendNotSupported "FileSystem backend does not support pub/sub")

  let subscribe _t ~channel:_ ~callback:_ =
    Error (BackendNotSupported "FileSystem backend does not support pub/sub")

  let health_check t =
    try
      let test_path = Filename.concat t.base_path ".health_check" in
      Out_channel.with_open_text test_path (fun oc ->
        Out_channel.output_string oc "ok"
      );
      Sys.remove test_path;
      Ok true
    with e ->
      Printf.eprintf "[WARN] Health check failed: %s\n%!" (Printexc.to_string e);
      Ok false
end

(* ============================================ *)
(* PostgreSQL Backend (Eio-native, non-blocking) *)
(* ============================================ *)

(** PostgresNative - Eio-based PostgreSQL backend using caqti-eio.

    Benefits over Redis:
    - Non-blocking: Uses Eio fibers, no blocking calls
    - Connection pooling: Built-in pool management
    - ACID transactions: Full transaction support
    - Already available: Uses existing Railway PostgreSQL

    Usage:
      export MASC_POSTGRES_URL="postgresql://user:pass@host:port/db"

    Schema (auto-created if not exists):
      CREATE TABLE masc_kv (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL,
        expires_at TIMESTAMP,
        created_at TIMESTAMP DEFAULT NOW(),
        updated_at TIMESTAMP DEFAULT NOW()
      );
*)
(* PostgresNative: Eio-native PostgreSQL backend using caqti-eio
   Note: This module extends BACKEND with create_eio for Eio context initialization *)
module PostgresNative : sig
  include BACKEND
  (* create_eio requires Caqti-compatible Eio environment (net, clock, mono_clock) *)
  val create_eio : sw:Eio.Switch.t -> env:Caqti_eio.stdenv -> config -> (t, error) result

  (* Cleanup old pubsub messages - PostgreSQL specific *)
  val cleanup_pubsub_by_age : t -> days:int -> (int, error) result
  val cleanup_pubsub_by_limit : t -> max_messages:int -> (int, error) result
  val cleanup_pubsub : t -> days:int -> max_messages:int -> (int, error) result
end = struct
  type t = {
    pool: (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t;
    namespace: string;
    _sw: Eio.Switch.t;  (* Keep switch alive for pool lifetime *)
  }

  (* Result monad binding operator for Caqti operations *)
  let (let*) = Result.bind

  let namespaced_key namespace key =
    if namespace = "" then key
    else namespace ^ ":" ^ key

  let strip_namespace namespace key =
    let prefix = namespace ^ ":" in
    let prefix_len = String.length prefix in
    if String.length key >= prefix_len && String.sub key 0 prefix_len = prefix then
      String.sub key prefix_len (String.length key - prefix_len)
    else key

  (* Caqti 2.x query definitions using Infix operators
     Syntax: (param_type ->? row_type) "SQL" for static queries *)
  open Caqti_request.Infix

  let get_q =
    (Caqti_type.string ->? Caqti_type.string)
    "SELECT value FROM masc_kv WHERE key = $1 AND (expires_at IS NULL OR expires_at > NOW())"

  let set_q =
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "INSERT INTO masc_kv (key, value, updated_at) VALUES ($1, $2, NOW()) \
     ON CONFLICT (key) DO UPDATE SET value = $2, updated_at = NOW()"

  let _set_with_ttl_q =
    (Caqti_type.(t3 string string int) ->. Caqti_type.unit)
    "INSERT INTO masc_kv (key, value, expires_at, updated_at) \
     VALUES ($1, $2, NOW() + $3 * INTERVAL '1 second', NOW()) \
     ON CONFLICT (key) DO UPDATE SET value = $2, \
       expires_at = NOW() + $3 * INTERVAL '1 second', updated_at = NOW()"

  let delete_q =
    (Caqti_type.string ->. Caqti_type.unit)
    "DELETE FROM masc_kv WHERE key = $1"

  let exists_q =
    (Caqti_type.string ->? Caqti_type.int)
    "SELECT 1 FROM masc_kv WHERE key = $1 AND (expires_at IS NULL OR expires_at > NOW())"

  let list_keys_q =
    (Caqti_type.string ->* Caqti_type.string)
    "SELECT key FROM masc_kv WHERE key LIKE $1 AND (expires_at IS NULL OR expires_at > NOW())"

  let get_all_q =
    (Caqti_type.string ->* Caqti_type.(t2 string string))
    "SELECT key, value FROM masc_kv WHERE key LIKE $1 AND (expires_at IS NULL OR expires_at > NOW())"

  let set_if_not_exists_q =
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "INSERT INTO masc_kv (key, value, updated_at) VALUES ($1, $2, NOW()) ON CONFLICT DO NOTHING"

  let acquire_lock_q =
    (Caqti_type.(t3 string string int) ->. Caqti_type.unit)
    "INSERT INTO masc_kv (key, value, expires_at, updated_at) \
     VALUES ($1, $2, NOW() + $3 * INTERVAL '1 second', NOW()) \
     ON CONFLICT DO NOTHING"

  let release_lock_q =
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "DELETE FROM masc_kv WHERE key = $1 AND value = $2"

  let extend_lock_q =
    (Caqti_type.(t3 string int string) ->. Caqti_type.unit)
    "UPDATE masc_kv SET expires_at = NOW() + $2 * INTERVAL '1 second', updated_at = NOW() \
     WHERE key = $1 AND value = $3"

  let cleanup_expired_q =
    (Caqti_type.unit ->. Caqti_type.unit)
    "DELETE FROM masc_kv WHERE expires_at IS NOT NULL AND expires_at < NOW()"

  let health_check_q =
    (Caqti_type.unit ->! Caqti_type.int)
    "SELECT 1"

  (* Pub/Sub queries (using table as queue for message passing)

     PostgreSQL LISTEN/NOTIFY Integration:
     - publish: INSERT + pg_notify for real-time push
     - subscribe: Table polling for reliability (messages persist)

     Hybrid approach benefits:
     - NOTIFY: Instant notification to LISTEN clients (< 1ms)
     - Table queue: Reliability (no message loss if client disconnected)
     - Caqti limitation: LISTEN requires dedicated connection outside pool
  *)
  let publish_q =
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "INSERT INTO masc_pubsub (channel, message) VALUES ($1, $2)"

  (* pg_notify sends real-time notification to all LISTEN clients
     Payload limited to 8000 bytes by PostgreSQL *)
  let notify_q =
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "SELECT pg_notify($1, $2)"

  (* PostgreSQL pg_notify payload limit (actual limit is 8000, use 7900 for safety margin) *)
  let pg_notify_max_payload = 7900

  let subscribe_q =
    (Caqti_type.string ->? Caqti_type.string)
    "DELETE FROM masc_pubsub WHERE id = (\
       SELECT id FROM masc_pubsub \
       WHERE channel = $1 \
       ORDER BY id \
       LIMIT 1 \
       FOR UPDATE SKIP LOCKED\
     ) RETURNING message"

  (* Schema creation queries *)
  let create_schema_q =
    (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TABLE IF NOT EXISTS masc_kv (\
       key TEXT PRIMARY KEY, \
       value TEXT NOT NULL, \
       expires_at TIMESTAMP, \
       created_at TIMESTAMP DEFAULT NOW(), \
       updated_at TIMESTAMP DEFAULT NOW() \
     )"

  let create_pubsub_table_q =
    (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TABLE IF NOT EXISTS masc_pubsub (\
       id SERIAL PRIMARY KEY, \
       channel TEXT NOT NULL, \
       message TEXT NOT NULL, \
       created_at TIMESTAMP DEFAULT NOW() \
     )"

  let create_index_q =
    (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE INDEX IF NOT EXISTS idx_masc_kv_expires ON masc_kv(expires_at)"

  let create_pubsub_index_q =
    (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE INDEX IF NOT EXISTS idx_masc_pubsub_channel ON masc_pubsub(channel, id)"

  let create_pubsub_created_at_index_q =
    (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE INDEX IF NOT EXISTS idx_masc_pubsub_created_at ON masc_pubsub(created_at)"

  (* Cleanup old pubsub messages - returns count of deleted rows *)
  let cleanup_pubsub_q =
    (Caqti_type.int ->! Caqti_type.int)
    "WITH deleted AS (DELETE FROM masc_pubsub WHERE created_at < NOW() - $1 * INTERVAL '1 day' RETURNING 1) SELECT COUNT(*)::int FROM deleted"

  (* Cleanup keeping max N messages per channel *)
  let cleanup_pubsub_limit_q =
    (Caqti_type.int ->! Caqti_type.int)
    "WITH ranked AS (\
       SELECT id, ROW_NUMBER() OVER (PARTITION BY channel ORDER BY id DESC) as rn \
       FROM masc_pubsub\
     ), deleted AS (\
       DELETE FROM masc_pubsub WHERE id IN (SELECT id FROM ranked WHERE rn > $1) RETURNING 1\
     ) SELECT COUNT(*)::int FROM deleted"

  (* Helper to convert Caqti_error to our error type *)
  let caqti_error_to_masc err =
    OperationFailed (Caqti_error.show err)

  (* WARNING: create requires an Eio.Switch context!
     This is a blocking workaround - in production, create should be called
     within an Eio.Switch.run context. *)
  let create (cfg : config) : (t, error) result =
    match cfg.postgres_url with
    | None -> Error (ConnectionFailed "PostgreSQL URL not configured (set MASC_POSTGRES_URL)")
    | Some url ->
        (* For now, create a dummy result - actual pool creation needs Eio.Switch *)
        (* The real implementation should be called from an Eio context *)
        Error (ConnectionFailed
          (Printf.sprintf "PostgresNative.create must be called from Eio context. URL: %s" url))

  (* Eio-aware create function - call this from Eio.Switch.run *)
  let create_eio ~sw ~env (cfg : config) : (t, error) result =
    match cfg.postgres_url with
    | None -> Error (ConnectionFailed "PostgreSQL URL not configured (set MASC_POSTGRES_URL)")
    | Some url ->
        let uri = Uri.of_string url in
        let pool_config = Caqti_pool_config.create ~max_size:10 () in
        (* Caqti_eio.stdenv = < net; clock; mono_clock > *)
        match Caqti_eio_unix.connect_pool ~sw ~stdenv:env ~pool_config uri with
        | Error err -> Error (caqti_error_to_masc err)
        | Ok pool ->
            (* Initialize schema if needed *)
            let init_result = Caqti_eio.Pool.use (fun conn ->
              let module C = (val conn : Caqti_eio.CONNECTION) in
              let* () = C.exec create_schema_q () in
              let* () = C.exec create_pubsub_table_q () in
              let* () = C.exec create_index_q () in
              let* () = C.exec create_pubsub_index_q () in
              let* () = C.exec create_pubsub_created_at_index_q () in
              Ok ()
            ) pool in
            (match init_result with
             | Error err -> Error (caqti_error_to_masc err)
             | Ok () -> Ok { pool; namespace = cfg.cluster_name; _sw = sw })

  let close _t = ()

  let get t ~key =
    let nkey = namespaced_key t.namespace key in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find_opt get_q nkey
    ) t.pool with
    | Ok v -> Ok v
    | Error err -> Error (caqti_error_to_masc err)

  let set t ~key ~value =
    let nkey = namespaced_key t.namespace key in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.exec set_q (nkey, value)
    ) t.pool with
    | Ok () -> Ok ()
    | Error err -> Error (caqti_error_to_masc err)

  let delete t ~key =
    let nkey = namespaced_key t.namespace key in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.exec delete_q nkey
    ) t.pool with
    | Ok () -> Ok true
    | Error err -> Error (caqti_error_to_masc err)

  let exists t ~key =
    let nkey = namespaced_key t.namespace key in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find_opt exists_q nkey
    ) t.pool with
    | Ok (Some _) -> true
    | Ok None -> false
    | Error _ -> false

  let list_keys t ~prefix =
    let nprefix = namespaced_key t.namespace prefix in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.collect_list list_keys_q (nprefix ^ "%")
    ) t.pool with
    | Ok keys -> Ok (List.map (strip_namespace t.namespace) keys)
    | Error err -> Error (caqti_error_to_masc err)

  let get_all t ~prefix =
    let nprefix = namespaced_key t.namespace prefix in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.collect_list get_all_q (nprefix ^ "%")
    ) t.pool with
    | Ok pairs ->
        Ok (List.map (fun (k, v) -> (strip_namespace t.namespace k, v)) pairs)
    | Error err -> Error (caqti_error_to_masc err)

  let set_if_not_exists t ~key ~value =
    let nkey = namespaced_key t.namespace key in
    (* First check if exists, then insert *)
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      let* existing = C.find_opt exists_q nkey in
      match existing with
      | Some _ -> Ok false  (* Key already exists *)
      | None ->
          let* () = C.exec set_if_not_exists_q (nkey, value) in
          Ok true
    ) t.pool with
    | Ok b -> Ok b
    | Error err -> Error (caqti_error_to_masc err)

  let compare_and_swap t ~key ~expected ~value =
    let nkey = namespaced_key t.namespace key in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      let* current = C.find_opt get_q nkey in
      match current with
      | Some v when v = expected ->
          let* () = C.exec set_q (nkey, value) in
          Ok true
      | _ -> Ok false
    ) t.pool with
    | Ok b -> Ok b
    | Error err -> Error (caqti_error_to_masc err)

  let acquire_lock t ~key ~ttl_seconds ~owner =
    let nkey = namespaced_key t.namespace ("lock:" ^ key) in
    (* Clean up expired locks first *)
    let _ = Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.exec cleanup_expired_q ()
    ) t.pool in
    (* Try to acquire lock *)
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      let* existing = C.find_opt get_q nkey in
      match existing with
      | Some _ -> Ok false  (* Lock held by someone *)
      | None ->
          let* () = C.exec acquire_lock_q (nkey, owner, ttl_seconds) in
          (* Verify we got it *)
          let* check = C.find_opt get_q nkey in
          Ok (check = Some owner)
    ) t.pool with
    | Ok b -> Ok b
    | Error err -> Error (caqti_error_to_masc err)

  let release_lock t ~key ~owner =
    let nkey = namespaced_key t.namespace ("lock:" ^ key) in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.exec release_lock_q (nkey, owner)
    ) t.pool with
    | Ok () -> Ok true
    | Error err -> Error (caqti_error_to_masc err)

  let extend_lock t ~key ~ttl_seconds ~owner =
    let nkey = namespaced_key t.namespace ("lock:" ^ key) in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.exec extend_lock_q (nkey, ttl_seconds, owner)
    ) t.pool with
    | Ok () -> Ok true
    | Error err -> Error (caqti_error_to_masc err)

  (* Pub/Sub using table as queue + NOTIFY for real-time *)
  let publish t ~channel ~message =
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      (* Insert into table for reliability (persistent queue) *)
      let* () = C.exec publish_q (channel, message) in
      (* Send NOTIFY for real-time push to LISTEN clients
         Skip NOTIFY for large payloads - subscribers poll anyway *)
      let total_payload = String.length channel + String.length message + 1 in
      if total_payload <= pg_notify_max_payload then
        C.exec notify_q (channel, message)
      else begin
        Log.debug ~ctx:"Pubsub" "NOTIFY skipped: payload too large (%d bytes, limit %d)"
          total_payload pg_notify_max_payload;
        Ok ()  (* Graceful degradation: table insert succeeded *)
      end
    ) t.pool with
    | Ok () -> Ok 1
    | Error err -> Error (caqti_error_to_masc err)

  let subscribe t ~channel ~callback =
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find_opt subscribe_q channel
    ) t.pool with
    | Ok (Some msg) -> callback msg; Ok ()
    | Ok None -> Ok ()
    | Error err -> Error (caqti_error_to_masc err)

  let health_check t =
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find health_check_q ()
    ) t.pool with
    | Ok 1 -> Ok true
    | Ok _ -> Ok false
    | Error err -> Error (caqti_error_to_masc err)

  (* Cleanup old pubsub messages by age (days) *)
  let cleanup_pubsub_by_age t ~days =
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find cleanup_pubsub_q days
    ) t.pool with
    | Ok count -> Ok count
    | Error err -> Error (caqti_error_to_masc err)

  (* Cleanup pubsub messages keeping only max_messages per channel *)
  let cleanup_pubsub_by_limit t ~max_messages =
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find cleanup_pubsub_limit_q max_messages
    ) t.pool with
    | Ok count -> Ok count
    | Error err -> Error (caqti_error_to_masc err)

  (* Combined cleanup: by age first, then by limit *)
  let cleanup_pubsub t ~days ~max_messages =
    let age_result = cleanup_pubsub_by_age t ~days in
    let limit_result = cleanup_pubsub_by_limit t ~max_messages in
    match (age_result, limit_result) with
    | (Ok age_count, Ok limit_count) -> Ok (age_count + limit_count)
    | (Error e, _) -> Error e
    | (_, Error e) -> Error e
end

(* ============================================ *)
(* Async backend interface removed (Eio-only)  *)
(* ============================================ *)
