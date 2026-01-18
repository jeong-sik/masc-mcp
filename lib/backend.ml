(** Backend Module - Storage abstraction for MASC (Memory/FileSystem/Redis) *)

(* ============================================ *)
(* Backend Types                                *)
(* ============================================ *)

type backend_type =
  | Memory
  | FileSystem
  | Redis
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
  redis_url: string option;
  postgres_url: string option;  (* PostgreSQL connection URL for PostgresNative backend *)
  node_id: string;
  cluster_name: string;
}

let generate_node_id () =
  let hostname = try Unix.gethostname () with _ -> "unknown" in
  let pid = Unix.getpid () in
  let rand = Random.int 10000 in
  Printf.sprintf "%s-%d-%04d" hostname pid rand

let default_config = {
  backend_type = FileSystem;
  base_path = ".masc";
  redis_url = None;
  postgres_url = None;
  node_id = generate_node_id ();
  cluster_name = "default";
}

let get_status config : Yojson.Safe.t =
  let backend_str = match config.backend_type with
    | Memory -> "memory"
    | FileSystem -> "filesystem"
    | Redis -> "redis"
    | PostgresNative -> "postgres_native"
  in
  `Assoc [
    ("backend_type", `String backend_str);
    ("base_path", `String config.base_path);
    ("node_id", `String config.node_id);
    ("cluster_name", `String config.cluster_name);
    ("redis_url", match config.redis_url with Some u -> `String u | None -> `Null);
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
let safe_parse_lock_json path =
  if not (Sys.file_exists path) then None
  else
    try
      let content = In_channel.with_open_text path In_channel.input_all in
      if String.length content = 0 then begin
        (* Empty file is corrupted - remove it *)
        (try Sys.remove path with _ -> ());
        None
      end else
        let json = Yojson.Safe.from_string content in
        let open Yojson.Safe.Util in
        let exp = json |> member "expires_at" |> to_float in
        let own = json |> member "owner" |> to_string in
        Some (own, exp)
    with
    | _ ->
        (* Corrupted JSON file - remove it to allow recovery *)
        (try Sys.remove path with _ -> ());
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
  try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ()

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
    with _ -> ());
    Ok { base_path = path; mutex = Mutex.create () }

  let close _t = ()

  let get t ~key =
    with_lock t (fun () ->
      match safe_key_to_path t key with
      | Error e -> Error e
      | Ok path ->
          if Sys.file_exists path then
            try
              let content = In_channel.with_open_text path In_channel.input_all in
              Ok (Some content)
            with _ -> Ok None
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
                    (try Sys.remove file_path with _ -> ());
                    Ok true
                | Some _ -> Ok false  (* Different owner *)
                | None -> Ok false    (* No valid lock *)
              with _ -> Ok false
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
              with _ -> Ok false
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
    with _ -> Ok false
end

(* ============================================ *)
(* Redis Backend (HTTP/REST - Upstash style)    *)
(* ============================================ *)

module RedisBackend : BACKEND = struct
  type t = {
    base_url: string;
    token: string;
    namespace: string;
  }

  let create (cfg : config) : (t, error) result =
    match cfg.redis_url with
    | None -> Error (ConnectionFailed "Redis URL not configured")
    | Some url ->
        let token = try Sys.getenv "UPSTASH_REDIS_TOKEN" with _ ->
          try Sys.getenv "REDIS_TOKEN" with _ -> ""
        in
        Ok { base_url = url; token; namespace = cfg.cluster_name }

  let close _t = ()

  let namespaced_key t key = Redis_common.make_namespaced_key ~namespace:t.namespace key

  (* Async HTTP client using cohttp-lwt *)
  let http_request t ~meth endpoint =
    let open Lwt.Syntax in
    let url = Printf.sprintf "%s/%s" t.base_url endpoint in
    let uri = Uri.of_string url in
    let headers = Cohttp.Header.of_list [
      ("Authorization", Printf.sprintf "Bearer %s" t.token);
      ("Content-Type", "application/json");
    ] in
    Lwt.catch
      (fun () ->
        let* (resp, body) = Cohttp_lwt_unix.Client.call ~headers meth uri in
        let status = Cohttp.Response.status resp in
        let* body_str = Cohttp_lwt.Body.to_string body in
        if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
          Lwt.return (Ok body_str)
        else
          Lwt.return (Error (OperationFailed (Printf.sprintf "HTTP %d: %s"
            (Cohttp.Code.code_of_status status) body_str))))
      (fun exn ->
        Lwt.return (Error (OperationFailed (Printexc.to_string exn))))

  let http_get t endpoint = http_request t ~meth:`GET endpoint

  let http_post t endpoint body =
    let open Lwt.Syntax in
    let url = Printf.sprintf "%s/%s" t.base_url endpoint in
    let uri = Uri.of_string url in
    let headers = Cohttp.Header.of_list [
      ("Authorization", Printf.sprintf "Bearer %s" t.token);
      ("Content-Type", "application/json");
    ] in
    let body_content = Cohttp_lwt.Body.of_string body in
    Lwt.catch
      (fun () ->
        let* (resp, resp_body) = Cohttp_lwt_unix.Client.post ~headers ~body:body_content uri in
        let status = Cohttp.Response.status resp in
        let* body_str = Cohttp_lwt.Body.to_string resp_body in
        if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
          Lwt.return (Ok body_str)
        else
          Lwt.return (Error (OperationFailed (Printf.sprintf "HTTP %d: %s"
            (Cohttp.Code.code_of_status status) body_str))))
      (fun exn ->
        Lwt.return (Error (OperationFailed (Printexc.to_string exn))))

  (* Sync wrapper for interface compatibility - runs Lwt in blocking mode

     ⚠️ DEPRECATED: This function is a DEADLOCK TIMEBOMB!

     WARNING: Calling run_sync within an existing Lwt event loop (e.g., MCP server,
     Dream, or any Lwt application) will cause a DEADLOCK because Lwt_main.run
     cannot be nested.

     MIGRATION PATH:
     - For async code: Use Backend.RedisBackendAsync module instead
     - For sync CLI tools: RedisBackend (this module) is still safe

     The safe pattern is:
     ┌─────────────────────────────────────────────────────────────┐
     │ Context               │ Use                                 │
     ├─────────────────────────────────────────────────────────────┤
     │ CLI tools (sync)      │ RedisBackend                        │
     │ MCP server (async)    │ RedisBackendAsync                   │
     │ Dream/Cohttp (async)  │ RedisBackendAsync                   │
     └─────────────────────────────────────────────────────────────┘

     If you see "Lwt_main.run: cannot be called inside Lwt" error,
     you're calling this from an async context - use RedisBackendAsync! *)
  let run_sync (promise : 'a Lwt.t) : 'a =
    (* Note: Lwt_main.run will raise "cannot be called inside Lwt" if nested,
       providing some protection against deadlock. However, the error message
       isn't clear about the fix - hence this documentation. *)
    Lwt_main.run promise

  let http_get_sync t endpoint = run_sync (http_get t endpoint)
  let _http_post_sync t endpoint body = run_sync (http_post t endpoint body)

  let get t ~key =
    let nkey = namespaced_key t key in
    match http_get_sync t (Printf.sprintf "get/%s" nkey) with
    | Error e -> Error e
    | Ok response ->
        try
          let json = Yojson.Safe.from_string response in
          let open Yojson.Safe.Util in
          let result = json |> member "result" in
          match result with
          | `Null -> Ok None
          | `String s -> Ok (Some s)
          | _ -> Ok None
        with _ -> Ok None

  let set t ~key ~value =
    let nkey = namespaced_key t key in
    match http_get_sync t (Printf.sprintf "set/%s/%s" nkey (Uri.pct_encode value)) with
    | Error e -> Error e
    | Ok _ -> Ok ()

  let delete t ~key =
    let nkey = namespaced_key t key in
    match http_get_sync t (Printf.sprintf "del/%s" nkey) with
    | Error e -> Error e
    | Ok response ->
        try
          let json = Yojson.Safe.from_string response in
          let open Yojson.Safe.Util in
          let result = json |> member "result" |> to_int in
          Ok (result > 0)
        with _ -> Ok false

  let exists t ~key =
    let nkey = namespaced_key t key in
    match http_get_sync t (Printf.sprintf "exists/%s" nkey) with
    | Ok response ->
        (try
          let json = Yojson.Safe.from_string response in
          let open Yojson.Safe.Util in
          json |> member "result" |> to_int > 0
        with _ -> false)
    | Error _ -> false

  let list_keys t ~prefix =
    let nprefix = namespaced_key t prefix in
    match http_get_sync t (Printf.sprintf "keys/%s*" nprefix) with
    | Error e -> Error e
    | Ok response ->
        try
          let json = Yojson.Safe.from_string response in
          let open Yojson.Safe.Util in
          let keys = json |> member "result" |> to_list |> List.map to_string in
          (* Remove namespace prefix *)
          let stripped = List.map (Redis_common.strip_namespace_prefix ~namespace:t.namespace) keys in
          Ok stripped
        with _ -> Ok []

  let get_all t ~prefix =
    Redis_common.get_all_generic
      ~list_keys:(fun p -> list_keys t ~prefix:p)
      ~get:(fun k -> get t ~key:k)
      ~prefix

  let set_if_not_exists t ~key ~value =
    let nkey = namespaced_key t key in
    match http_get_sync t (Printf.sprintf "setnx/%s/%s" nkey (Uri.pct_encode value)) with
    | Error e -> Error e
    | Ok response ->
        try
          let json = Yojson.Safe.from_string response in
          let open Yojson.Safe.Util in
          Ok (json |> member "result" |> to_int = 1)
        with _ -> Ok false

  let compare_and_swap _t ~key:_ ~expected:_ ~value:_ =
    (* Redis doesn't have native CAS, would need Lua script *)
    Error (BackendNotSupported "CAS requires Lua scripting")

  (* Distributed lock with atomic SET NX EX *)
  let acquire_lock t ~key ~ttl_seconds ~owner =
    let lock_key = Redis_common.make_lock_key key in
    let nkey = namespaced_key t lock_key in
    let value = Redis_common.make_owner_value owner in
    (* Upstash REST: set/key/value/ex/seconds/nx for atomic SET key value EX seconds NX *)
    match http_get_sync t (Printf.sprintf "set/%s/%s/ex/%d/nx" nkey (Uri.pct_encode value) ttl_seconds) with
    | Error e -> Error e
    | Ok response ->
        try
          let json = Yojson.Safe.from_string response in
          let open Yojson.Safe.Util in
          let result = json |> member "result" in
          (* "OK" means lock acquired, null means already locked *)
          Ok (result <> `Null && result <> `String "")
        with _ -> Ok false

  (* Safe lock release - only owner can release *)
  let release_lock t ~key ~owner =
    Redis_common.release_lock_generic
      ~get:(fun k -> get t ~key:k)
      ~delete:(fun k -> delete t ~key:k)
      ~key ~owner

  (* Extend lock TTL - only owner can extend *)
  let extend_lock t ~key ~ttl_seconds ~owner =
    let lock_key = Redis_common.make_lock_key key in
    match get t ~key:lock_key with
    | Error e -> Error e
    | Ok None -> Ok false  (* Lock doesn't exist *)
    | Ok (Some value) ->
        if Redis_common.verify_owner ~owner ~value then begin
          let nkey = namespaced_key t lock_key in
          match http_get_sync t (Printf.sprintf "expire/%s/%d" nkey ttl_seconds) with
          | Error e -> Error e
          | Ok response ->
              (try
                let json = Yojson.Safe.from_string response in
                let open Yojson.Safe.Util in
                Ok (json |> member "result" |> to_int = 1)
              with _ -> Ok false)
        end else
          Ok false  (* Not the owner *)

  (* Pub/Sub using Redis LIST as message queue (polling-based) *)
  let publish t ~channel ~message =
    let queue_key = Redis_common.make_pubsub_key channel in
    let nkey = namespaced_key t queue_key in
    let msg_json = Redis_common.make_message_json message in
    match http_get_sync t (Printf.sprintf "lpush/%s/%s" nkey (Uri.pct_encode msg_json)) with
    | Error e -> Error e
    | Ok response ->
        (try
          let json = Yojson.Safe.from_string response in
          let open Yojson.Safe.Util in
          let queue_len = json |> member "result" |> to_int in
          (* Trim queue to last 1000 messages to prevent unbounded growth *)
          let _ = http_get_sync t (Printf.sprintf "ltrim/%s/0/999" nkey) in
          (* Return 1 as pseudo-subscriber count (LIST-based doesn't track subscribers) *)
          Ok (min queue_len 1)
        with _ -> Ok 0)

  (* Polling-based subscribe - returns pending messages *)
  let subscribe t ~channel ~callback =
    let queue_key = Redis_common.make_pubsub_key channel in
    let nkey = namespaced_key t queue_key in
    (* RPOP to get oldest message (FIFO) *)
    match http_get_sync t (Printf.sprintf "rpop/%s" nkey) with
    | Error e -> Error e
    | Ok response ->
        (try
          let json = Yojson.Safe.from_string response in
          let open Yojson.Safe.Util in
          let result = json |> member "result" in
          match result with
          | `Null -> Ok ()  (* No messages *)
          | `String msg_json ->
              (match Redis_common.parse_message_json msg_json with
               | Some message -> callback message; Ok ()
               | None -> Ok ())
          | _ -> Ok ()
        with _ -> Ok ())

  let health_check t =
    match http_get_sync t "ping" with
    | Error _ -> Ok false
    | Ok response ->
        Ok (String.length response > 0 && (
            (* Direct PONG response or JSON wrapped *)
            String.sub response 0 1 <> "{" ||
            try
              let json = Yojson.Safe.from_string response in
              let open Yojson.Safe.Util in
              json |> member "result" |> to_string = "PONG"
            with _ -> false))
end

(* ============================================ *)
(* Redis Backend (Native Protocol - redis-sync)  *)
(* ============================================ *)

module RedisNative : BACKEND = struct
  type t = {
    host: string;
    port: int;
    password: string option;
    namespace: string;
  }

  (* Parse redis:// URL into components *)
  let parse_redis_url url =
    (* Format: redis://[:password@]host:port or redis://user:password@host:port *)
    let url =
      if String.length url > 8 && String.sub url 0 8 = "redis://" then
        String.sub url 8 (String.length url - 8)
      else url
    in
    (* Check for auth@ pattern *)
    let (auth_part, host_part) =
      match String.index_opt url '@' with
      | Some i -> (Some (String.sub url 0 i), String.sub url (i+1) (String.length url - i - 1))
      | None -> (None, url)
    in
    (* Parse host:port *)
    let (host, port) =
      match String.index_opt host_part ':' with
      | Some i ->
          let h = String.sub host_part 0 i in
          let p_str = String.sub host_part (i+1) (String.length host_part - i - 1) in
          (h, int_of_string_opt p_str |> Option.value ~default:6379)
      | None -> (host_part, 6379)
    in
    (* Parse password from auth (user:password or just password) *)
    let password = match auth_part with
      | None -> None
      | Some auth ->
          match String.index_opt auth ':' with
          | Some i -> Some (String.sub auth (i+1) (String.length auth - i - 1))  (* user:password *)
          | None -> Some auth  (* just password *)
    in
    (host, port, password)

  let create (cfg : config) : (t, error) result =
    match cfg.redis_url with
    | None -> Error (ConnectionFailed "Redis URL not configured")
    | Some url ->
        let (host, port, password) = parse_redis_url url in
        Ok { host; port; password; namespace = cfg.cluster_name }

  let close _t = ()

  let namespaced_key t key = Redis_common.make_namespaced_key ~namespace:t.namespace key

  (* Create Redis connection spec - uses blocking redis-sync *)
  let make_spec (cfg : t) : Redis_sync.Client.connection_spec =
    { Redis_sync.Client.host = cfg.host; port = cfg.port }

  (* Run command with connection (blocking - safe to call from Lwt context) *)
  let with_connection (cfg : t) f =
    let spec = make_spec cfg in
    try
      let conn = Redis_sync.Client.connect spec in
      (* Auth if password provided *)
      (match cfg.password with
       | Some pwd -> let _ = Redis_sync.Client.auth conn pwd in ()
       | None -> ());
      let result = f conn in
      Redis_sync.Client.disconnect conn;
      Ok result
    with exn ->
      Error (OperationFailed (Printexc.to_string exn))

  let get t ~key =
    let nkey = namespaced_key t key in
    with_connection t (fun conn ->
      Redis_sync.Client.get conn nkey)

  let set t ~key ~value =
    let nkey = namespaced_key t key in
    with_connection t (fun conn ->
      let _ = Redis_sync.Client.set conn nkey value in
      ())

  let delete t ~key =
    let nkey = namespaced_key t key in
    with_connection t (fun conn ->
      let count = Redis_sync.Client.del conn [nkey] in
      count > 0)

  let exists t ~key =
    let nkey = namespaced_key t key in
    match with_connection t (fun conn ->
      Redis_sync.Client.exists conn nkey) with
    | Ok b -> b
    | Error _ -> false

  let list_keys t ~prefix =
    let nprefix = namespaced_key t prefix in
    with_connection t (fun conn ->
      let keys = Redis_sync.Client.keys conn (nprefix ^ "*") in
      (* Remove namespace prefix *)
      List.map (Redis_common.strip_namespace_prefix ~namespace:t.namespace) keys)

  let get_all t ~prefix =
    Redis_common.get_all_generic
      ~list_keys:(fun p -> list_keys t ~prefix:p)
      ~get:(fun k -> get t ~key:k)
      ~prefix

  let set_if_not_exists t ~key ~value =
    let nkey = namespaced_key t key in
    with_connection t (fun conn ->
      Redis_sync.Client.setnx conn nkey value)

  let compare_and_swap _t ~key:_ ~expected:_ ~value:_ =
    (* Redis doesn't have native CAS, would need Lua script *)
    Error (BackendNotSupported "CAS requires Lua scripting")

  (* Distributed lock with atomic SET NX EX *)
  let acquire_lock t ~key ~ttl_seconds ~owner =
    let lock_key = Redis_common.make_lock_key key in
    let nkey = namespaced_key t lock_key in
    let value = Redis_common.make_owner_value owner in
    with_connection t (fun conn ->
      (* Atomic SET with NX (only if not exists) and EX (expiration in seconds) *)
      Redis_sync.Client.set conn ~ex:ttl_seconds ~nx:true nkey value)

  let release_lock t ~key ~owner =
    Redis_common.release_lock_generic
      ~get:(fun k -> get t ~key:k)
      ~delete:(fun k -> delete t ~key:k)
      ~key ~owner

  let extend_lock t ~key ~ttl_seconds ~owner =
    let lock_key = Redis_common.make_lock_key key in
    match get t ~key:lock_key with
    | Error e -> Error e
    | Ok None -> Ok false
    | Ok (Some value) ->
        if Redis_common.verify_owner ~owner ~value then begin
          let nkey = namespaced_key t lock_key in
          with_connection t (fun conn ->
            Redis_sync.Client.expire conn nkey ttl_seconds)
        end else
          Ok false

  (* Pub/Sub using LIST as message queue *)
  let publish t ~channel ~message =
    let queue_key = Redis_common.make_pubsub_key channel in
    let nkey = namespaced_key t queue_key in
    let msg_json = Redis_common.make_message_json message in
    with_connection t (fun conn ->
      let len = Redis_sync.Client.lpush conn nkey [msg_json] in
      (* Trim to 1000 messages *)
      let _ = Redis_sync.Client.ltrim conn nkey 0 999 in
      min len 1)

  let subscribe t ~channel ~callback =
    let queue_key = Redis_common.make_pubsub_key channel in
    let nkey = namespaced_key t queue_key in
    with_connection t (fun conn ->
      let result = Redis_sync.Client.rpop conn nkey in
      match result with
      | None -> ()
      | Some msg_json ->
          match Redis_common.parse_message_json msg_json with
          | Some message -> callback message
          | None -> ())

  let health_check t =
    with_connection t (fun conn ->
      let _ = Redis_sync.Client.ping conn in
      true)
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

  (* Schema creation query *)
  let create_schema_q =
    (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TABLE IF NOT EXISTS masc_kv (\
       key TEXT PRIMARY KEY, \
       value TEXT NOT NULL, \
       expires_at TIMESTAMP, \
       created_at TIMESTAMP DEFAULT NOW(), \
       updated_at TIMESTAMP DEFAULT NOW() \
     )"

  let create_index_q =
    (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE INDEX IF NOT EXISTS idx_masc_kv_expires ON masc_kv(expires_at)"

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
              let* () = C.exec create_index_q () in
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

  (* Pub/Sub not directly supported in PostgreSQL KV model *)
  (* Use LISTEN/NOTIFY for real pub/sub - TODO: implement later *)
  let publish _t ~channel:_ ~message:_ =
    Ok 0  (* No subscribers in simple KV mode *)

  let subscribe _t ~channel:_ ~callback:_ =
    Ok ()  (* No-op for now *)

  let health_check t =
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find health_check_q ()
    ) t.pool with
    | Ok 1 -> Ok true
    | Ok _ -> Ok false
    | Error err -> Error (caqti_error_to_masc err)
end

(* ============================================ *)
(* URL Scheme Auto-Detection for Redis Backend  *)
(* ============================================ *)

(* Detect protocol from URL scheme *)
let is_native_redis_url url =
  let url_lower = String.lowercase_ascii url in
  (String.length url_lower >= 8 && String.sub url_lower 0 8 = "redis://") ||
  (String.length url_lower >= 9 && String.sub url_lower 0 9 = "rediss://")

let is_rest_redis_url url =
  let url_lower = String.lowercase_ascii url in
  (String.length url_lower >= 8 && String.sub url_lower 0 8 = "https://") ||
  (String.length url_lower >= 7 && String.sub url_lower 0 7 = "http://")

(* ============================================ *)
(* Async Backend Interface (Lwt-safe)          *)
(* ============================================ *)

(** BACKEND_ASYNC - Asynchronous backend interface using Lwt.
    Use this interface when running in an Lwt event loop context (e.g., MCP server).
    Unlike BACKEND, all operations return Lwt.t and are safe to call from async contexts.

    This avoids the deadlock issue with run_sync/Lwt_main.run nested calls. *)
module type BACKEND_ASYNC = sig
  type t

  val create : config -> (t, error) result
  val close : t -> unit Lwt.t

  (* Basic KV operations - all return Lwt.t *)
  val get : t -> key:string -> (string option, error) result Lwt.t
  val set : t -> key:string -> value:string -> (unit, error) result Lwt.t
  val delete : t -> key:string -> (bool, error) result Lwt.t
  val exists : t -> key:string -> bool Lwt.t

  (* List operations *)
  val list_keys : t -> prefix:string -> (string list, error) result Lwt.t
  val get_all : t -> prefix:string -> ((string * string) list, error) result Lwt.t

  (* Atomic operations *)
  val set_if_not_exists : t -> key:string -> value:string -> (bool, error) result Lwt.t

  (* Distributed locking *)
  val acquire_lock : t -> key:string -> ttl_seconds:int -> owner:string -> (bool, error) result Lwt.t
  val release_lock : t -> key:string -> owner:string -> (bool, error) result Lwt.t
  val extend_lock : t -> key:string -> ttl_seconds:int -> owner:string -> (bool, error) result Lwt.t

  (* Pub/Sub *)
  val publish : t -> channel:string -> message:string -> (int, error) result Lwt.t

  (* Health check *)
  val health_check : t -> (bool, error) result Lwt.t
end

(* ============================================ *)
(* Redis Backend Async (Upstash REST API)      *)
(* Safe to use within Lwt event loop           *)
(* ============================================ *)

(** RedisBackendAsync - Async version of RedisBackend using Lwt directly.
    Use this module when running inside an Lwt event loop (MCP server, Dream, etc.)
    to avoid deadlocks from nested Lwt_main.run calls. *)
module RedisBackendAsync : BACKEND_ASYNC = struct
  type t = {
    base_url: string;
    token: string;
    namespace: string;
  }

  let create (cfg : config) : (t, error) result =
    match cfg.redis_url with
    | None -> Error (ConnectionFailed "Redis REST URL not configured (set MASC_REDIS_URL)")
    | Some url ->
        let token = try Sys.getenv "UPSTASH_REDIS_TOKEN" with _ ->
          try Sys.getenv "REDIS_TOKEN" with _ -> ""
        in
        Ok { base_url = url; token; namespace = cfg.cluster_name }

  let close _t = Lwt.return_unit

  let namespaced_key t key = Redis_common.make_namespaced_key ~namespace:t.namespace key

  (* Pure async HTTP client - NO run_sync! *)
  let http_get t endpoint =
    let open Lwt.Syntax in
    let url = Printf.sprintf "%s/%s" t.base_url endpoint in
    let uri = Uri.of_string url in
    let headers = Cohttp.Header.of_list [
      ("Authorization", Printf.sprintf "Bearer %s" t.token);
      ("Content-Type", "application/json");
    ] in
    Lwt.catch
      (fun () ->
        let* (resp, body) = Cohttp_lwt_unix.Client.call ~headers `GET uri in
        let status = Cohttp.Response.status resp in
        let* body_str = Cohttp_lwt.Body.to_string body in
        if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
          Lwt.return (Ok body_str)
        else
          Lwt.return (Error (OperationFailed (Printf.sprintf "HTTP %d: %s"
            (Cohttp.Code.code_of_status status) body_str))))
      (fun exn ->
        Lwt.return (Error (OperationFailed (Printexc.to_string exn))))

  let get t ~key =
    let open Lwt.Syntax in
    let nkey = namespaced_key t key in
    let* response = http_get t (Printf.sprintf "get/%s" nkey) in
    Lwt.return (match response with
    | Error e -> Error e
    | Ok body ->
        Redis_common.parse_get_response body)

  let set t ~key ~value =
    let open Lwt.Syntax in
    let nkey = namespaced_key t key in
    let* response = http_get t (Printf.sprintf "set/%s/%s" nkey (Uri.pct_encode value)) in
    Lwt.return (match response with
    | Error e -> Error e
    | Ok _ -> Ok ())

  let delete t ~key =
    let open Lwt.Syntax in
    let nkey = namespaced_key t key in
    let* response = http_get t (Printf.sprintf "del/%s" nkey) in
    Lwt.return (match response with
    | Error e -> Error e
    | Ok body -> Redis_common.parse_del_response body)

  let exists t ~key =
    let open Lwt.Syntax in
    let nkey = namespaced_key t key in
    let* response = http_get t (Printf.sprintf "exists/%s" nkey) in
    Lwt.return (match response with
    | Ok body -> Redis_common.parse_exists_response body
    | Error _ -> false)

  let list_keys t ~prefix =
    let open Lwt.Syntax in
    let nprefix = namespaced_key t prefix in
    let* response = http_get t (Printf.sprintf "keys/%s*" nprefix) in
    Lwt.return (match response with
    | Error e -> Error e
    | Ok body -> Redis_common.parse_keys_response body ~namespace:t.namespace)

  let get_all t ~prefix =
    let open Lwt.Syntax in
    let* keys_result = list_keys t ~prefix in
    match keys_result with
    | Error e -> Lwt.return (Error e)
    | Ok keys ->
        let* pairs = Lwt_list.filter_map_s (fun k ->
          let* v = get t ~key:k in
          Lwt.return (match v with
          | Ok (Some value) -> Some (k, value)
          | _ -> None)
        ) keys in
        Lwt.return (Ok pairs)

  let set_if_not_exists t ~key ~value =
    let open Lwt.Syntax in
    let nkey = namespaced_key t key in
    let* response = http_get t (Printf.sprintf "setnx/%s/%s" nkey (Uri.pct_encode value)) in
    Lwt.return (match response with
    | Error e -> Error e
    | Ok body -> Redis_common.parse_setnx_response body)

  let acquire_lock t ~key ~ttl_seconds ~owner =
    let open Lwt.Syntax in
    let lock_key = Redis_common.make_lock_key key in
    let nkey = namespaced_key t lock_key in
    let value = Redis_common.make_lock_value owner in
    let* response = http_get t (Printf.sprintf "set/%s/%s/ex/%d/nx" nkey (Uri.pct_encode value) ttl_seconds) in
    Lwt.return (match response with
    | Error e -> Error e
    | Ok body -> Redis_common.parse_set_nx_ex_response body)

  let release_lock t ~key ~owner =
    let open Lwt.Syntax in
    let lock_key = Redis_common.make_lock_key key in
    let* value_result = get t ~key:lock_key in
    match value_result with
    | Error e -> Lwt.return (Error e)
    | Ok None -> Lwt.return (Ok false)
    | Ok (Some value) ->
        if Redis_common.verify_lock_owner value owner then
          delete t ~key:lock_key
        else
          Lwt.return (Ok false)

  let extend_lock t ~key ~ttl_seconds ~owner =
    let open Lwt.Syntax in
    let lock_key = Redis_common.make_lock_key key in
    let* value_result = get t ~key:lock_key in
    match value_result with
    | Error e -> Lwt.return (Error e)
    | Ok None -> Lwt.return (Ok false)
    | Ok (Some value) ->
        if Redis_common.verify_lock_owner value owner then begin
          let nkey = namespaced_key t lock_key in
          let* response = http_get t (Printf.sprintf "expire/%s/%d" nkey ttl_seconds) in
          Lwt.return (match response with
          | Error e -> Error e
          | Ok body -> Redis_common.parse_expire_response body)
        end else
          Lwt.return (Ok false)

  let publish t ~channel ~message =
    let open Lwt.Syntax in
    let queue_key = Redis_common.make_pubsub_key channel in
    let nkey = namespaced_key t queue_key in
    let msg_json = Redis_common.make_message_json message in
    let* response = http_get t (Printf.sprintf "lpush/%s/%s" nkey (Uri.pct_encode msg_json)) in
    match response with
    | Error e -> Lwt.return (Error e)
    | Ok body ->
        (* Fire-and-forget trim to keep queue bounded *)
        let _ = http_get t (Printf.sprintf "ltrim/%s/0/999" nkey) in
        Lwt.return (Redis_common.parse_lpush_response body)

  let health_check t =
    let open Lwt.Syntax in
    let* response = http_get t "ping" in
    Lwt.return (match response with
    | Error _ -> Ok false
    | Ok body -> Ok (Redis_common.parse_ping_response body))
end
