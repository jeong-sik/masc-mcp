(** Backend_eio: OCaml 5.x Eio-native storage backend

    Direct-style async I/O using Eio.

    This module provides the same interface as Backend, but uses:
    - Eio.Path for file operations
    - Eio.Mutex for concurrency control
    - Direct style (no let*/>>= needed)

    Migration path: Backend.FileSystemBackend -> Backend_eio.FileSystem

    Compact Protocol v4: Transparent zstd compression with Dictionary
    - Uses trained multi-format dictionary for 32-2048 byte messages
    - Dictionary achieves ~70% compression vs ~6% standard zstd on small data
    - Automatically compresses data >32 bytes on save
    - Automatically decompresses on load (ZSTD/ZSTDD header detection)
*)

(** {1 Compression} *)

module Compression = struct
  (** Minimum size for dictionary compression *)
  let min_size = Compression_dict.min_dict_size  (* 32 bytes *)

  (** Default compression level *)
  let default_level = 3

  (** ZSTD header magic (standard compression) *)
  let magic = "ZSTD"

  (** ZSTDD header magic (dictionary compression) *)
  let magic_dict = "ZSTDD"

  (** Compress with zstd + optional dictionary *)
  let compress ?(level = default_level) (data : string) : (string * bool * bool) =
    Compression_dict.compress ~level data  (* returns (data, used_dict, did_compress) *)

  (** Encode with size header: MAGIC (5) + orig_size (4 BE) + compressed
      MAGIC = "ZSTD\x00" for standard, "ZSTDD" for dictionary *)
  let encode_with_header ~(used_dict : bool) (orig_size : int) (compressed : string) : string =
    let header = Bytes.create 9 in
    if used_dict then
      Bytes.blit_string magic_dict 0 header 0 5  (* "ZSTDD" = 5 chars *)
    else begin
      Bytes.blit_string magic 0 header 0 4;      (* "ZSTD" = 4 chars *)
      Bytes.set header 4 '\x00'                   (* + null = 5 chars *)
    end;
    Bytes.set header 5 (Char.chr ((orig_size lsr 24) land 0xFF));
    Bytes.set header 6 (Char.chr ((orig_size lsr 16) land 0xFF));
    Bytes.set header 7 (Char.chr ((orig_size lsr 8) land 0xFF));
    Bytes.set header 8 (Char.chr (orig_size land 0xFF));
    Bytes.to_string header ^ compressed

  (** Decode header, returns (orig_size, compressed_data, used_dict) if valid
      Supports:
      - Legacy 8-byte: ZSTD + 4-byte size (backwards compat)
      - New 9-byte: ZSTD\x00 + 4-byte size (standard compression)
      - New 9-byte: ZSTDD + 4-byte size (dictionary compression)

      IMPORTANT: Check ZSTDD first because "ZSTDD" starts with "ZSTD" *)
  let decode_header (data : string) : (int * string * bool) option =
    if String.length data < 8 then None
    (* Check dictionary header FIRST (ZSTDD) *)
    else if String.length data >= 9 && String.sub data 0 5 = magic_dict then begin
      (* Dictionary header: ZSTDD *)
      let orig_size =
        (Char.code data.[5] lsl 24) lor
        (Char.code data.[6] lsl 16) lor
        (Char.code data.[7] lsl 8) lor
        Char.code data.[8]
      in
      let compressed = String.sub data 9 (String.length data - 9) in
      Some (orig_size, compressed, true)
    end
    (* Then check standard headers *)
    else
      let header4 = String.sub data 0 4 in
      if header4 = magic then begin
        (* Could be legacy 8-byte or new 9-byte with ZSTD\x00 *)
        if String.length data >= 9 && data.[4] = '\x00' then begin
          (* New 9-byte header: ZSTD\x00 *)
          let orig_size =
            (Char.code data.[5] lsl 24) lor
            (Char.code data.[6] lsl 16) lor
            (Char.code data.[7] lsl 8) lor
            Char.code data.[8]
          in
          let compressed = String.sub data 9 (String.length data - 9) in
          Some (orig_size, compressed, false)
        end else begin
          (* Legacy 8-byte header *)
          let orig_size =
            (Char.code data.[4] lsl 24) lor
            (Char.code data.[5] lsl 16) lor
            (Char.code data.[6] lsl 8) lor
            Char.code data.[7]
          in
          let compressed = String.sub data 8 (String.length data - 8) in
          Some (orig_size, compressed, false)
        end
      end else
        None

  (** Decompress with known original size and dict flag *)
  let decompress ~(orig_size : int) ~(used_dict : bool) (compressed : string) : string option =
    try Some (Compression_dict.decompress ~orig_size ~used_dict compressed)
    with _ -> None

  (** Auto-decompress if ZSTD/ZSTDD header present *)
  let decompress_auto (data : string) : string =
    match decode_header data with
    | Some (orig_size, compressed, used_dict) ->
        (match decompress ~orig_size ~used_dict compressed with
         | Some decompressed -> decompressed
         | None -> data)  (* Return original on failure *)
    | None -> data

  (** Compress and add header if beneficial *)
  let compress_with_header ?(level = default_level) (data : string) : string =
    let (compressed, used_dict, did_compress) = compress ~level data in
    if did_compress then
      encode_with_header ~used_dict (String.length data) compressed
    else
      data
end

(** {1 Types} *)

type error =
  | NotFound of string
  | AlreadyExists of string
  | IOError of string
  | InvalidKey of string

type 'a result = ('a, error) Stdlib.result

(** {1 Configuration} *)

type config = {
  base_path: string;
  node_id: string;
  cluster_name: string;
}

let default_config = {
  base_path = ".masc";
  node_id = Printf.sprintf "node_%d" (Unix.getpid ());
  cluster_name = "default";
}

(** {1 Health Check Result} *)

type health_result = {
  latency_ms: float;
  is_healthy: bool;
}

(** {1 FileSystem Backend (Eio)} *)

module FileSystem = struct
  type t = {
    config: config;
    fs: Eio.Fs.dir_ty Eio.Path.t;
    mutex: Eio.Mutex.t;
  }

  (** Create a new FileSystem backend *)
  let create ~fs config =
    let path = Eio.Path.(fs / config.base_path) in
    (* Ensure base directory exists *)
    (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path
     with _ -> ());
    {
      config;
      fs = path;
      mutex = Eio.Mutex.create ();
    }

  (** {2 Key Validation} *)

  let validate_key key =
    if String.length key = 0 then
      Error (InvalidKey "Empty key not allowed")
    else if String.contains key '\x00' then
      Error (InvalidKey "NUL byte not allowed")
    else if String.contains key '/' then
      Error (InvalidKey "Slash not allowed (use ':' as separator)")
    else if key.[0] = ':' then
      Error (InvalidKey "Key cannot start with ':'")
    else if key.[String.length key - 1] = ':' then
      Error (InvalidKey "Key cannot end with ':'")
    else
      (* Check segments *)
      let segments = String.split_on_char ':' key in
      let rec check_segments = function
        | [] -> Ok key
        | seg :: rest ->
            if seg = "." || seg = ".." then
              Error (InvalidKey "Path traversal detected")
            else if String.length seg >= 2 && String.sub seg 0 2 = ".." then
              Error (InvalidKey "Path traversal detected")
            else
              (* Blocklist: reject only dangerous characters, allow UTF-8 *)
              let has_dangerous = String.exists (fun c ->
                let code = Char.code c in
                code = 0 || code < 32 ||       (* null/control chars *)
                c = '/' || c = '\\' ||         (* path separators *)
                c = ':' ||                     (* key separator *)
                c = '*' || c = '?' ||          (* wildcards *)
                c = '"' || c = '\'' ||         (* quotes *)
                c = '<' || c = '>' || c = '|'  (* shell metacharacters *)
              ) seg in
              if has_dangerous then
                Error (InvalidKey (Printf.sprintf "Invalid character in key segment '%s'" seg))
              else
                check_segments rest
      in
      check_segments segments

  let key_to_path t key =
    match validate_key key with
    | Error e -> Error e
    | Ok safe_key ->
        let path_part = String.map (function ':' -> '/' | c -> c) safe_key in
        Ok Eio.Path.(t.fs / path_part)

  (** {2 Core Operations} *)

  (** Get value by key (auto-decompresses ZSTD if detected) *)
  let get t key =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match key_to_path t key with
      | Error e -> Error e
      | Ok path ->
          try
            let content = Eio.Path.load path in
            (* Compact Protocol v4: Auto-decompress if ZSTD header present *)
            let decompressed = Compression.decompress_auto content in
            Ok decompressed
          with
          | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
              Error (NotFound key)
          | exn ->
              Error (IOError (Printexc.to_string exn))
    )

  (** Set value (auto-compresses with ZSTD if beneficial) *)
  let set t key value =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match key_to_path t key with
      | Error e -> Error e
      | Ok path ->
          try
            (* Ensure parent directory exists *)
            (match Eio.Path.split path with
             | Some (parent, _) ->
                 (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent
                  with _ -> ())
             | None -> ());
            (* Compact Protocol v4: Compress before saving (if beneficial) *)
            let compressed = Compression.compress_with_header value in
            (* Write file *)
            Eio.Path.save ~create:(`Or_truncate 0o644) path compressed;
            Ok ()
          with exn ->
            Error (IOError (Printexc.to_string exn))
    )

  (** Check if key exists *)
  let exists t key =
    match key_to_path t key with
    | Error _ -> false
    | Ok path ->
        try
          match Eio.Path.kind ~follow:true path with
          | `Regular_file -> true
          | _ -> false
        with _ -> false

  (** Delete key *)
  let delete t key =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match key_to_path t key with
      | Error e -> Error e
      | Ok path ->
          try
            Eio.Path.unlink path;
            Ok ()
          with
          | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
              Error (NotFound key)
          | exn ->
              Error (IOError (Printexc.to_string exn))
    )

  (** List keys with prefix *)
  let list_keys t ~prefix =
    match key_to_path t prefix with
    | Error e -> Error e
    | Ok dir_path ->
        try
          let entries = Eio.Path.read_dir dir_path in
          let keys = List.filter_map (fun name ->
            let key = if prefix = "" then name else prefix ^ ":" ^ name in
            match validate_key key with
            | Ok k -> Some k
            | Error _ -> None
          ) entries in
          Ok keys
        with
        | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
            Ok []  (* Directory doesn't exist = no keys *)
        | exn ->
            Error (IOError (Printexc.to_string exn))

  (** Set if not exists (atomic, auto-compresses) *)
  let set_if_not_exists t key value =
    (* Compact Protocol v4: Compress before saving *)
    let compressed = Compression.compress_with_header value in
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match key_to_path t key with
      | Error e -> Error e
      | Ok path ->
          try
            (* Check if exists first *)
            match Eio.Path.kind ~follow:true path with
            | `Regular_file -> Error (AlreadyExists key)
            | _ ->
                (* Ensure parent directory *)
                (match Eio.Path.split path with
                 | Some (parent, _) ->
                     (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent
                      with _ -> ())
                 | None -> ());
                (* Write with exclusive create *)
                Eio.Path.save ~create:(`Exclusive 0o644) path compressed;
                Ok true
          with
          | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
              (* Parent doesn't exist, create it *)
              (match key_to_path t key with
               | Error e -> Error e
               | Ok path ->
                   (match Eio.Path.split path with
                    | Some (parent, _) ->
                        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent
                    | None -> ());
                   Eio.Path.save ~create:(`Exclusive 0o644) path compressed;
                   Ok true)
          | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) ->
              Error (AlreadyExists key)
          | exn ->
              Error (IOError (Printexc.to_string exn))
    )

  (** {2 Lock Operations} *)

  type lock_info = {
    owner: string;
    acquired_at: float;
    expires_at: float;
  }

  let lock_info_to_json info =
    Printf.sprintf {|{"owner":"%s","acquired_at":%f,"expires_at":%f}|}
      info.owner info.acquired_at info.expires_at

  let lock_info_of_json json =
    try
      let open Yojson.Safe.Util in
      let j = Yojson.Safe.from_string json in
      let parse_float = function
        | `Float f -> Some f
        | `Int i -> Some (float_of_int i)
        | `Intlit s -> float_of_string_opt s
        | `String s -> float_of_string_opt s
        | _ -> None
      in
      let parse_string = function
        | `String s -> Some s
        | _ -> None
      in
      match
        (parse_string (member "owner" j),
         parse_float (member "acquired_at" j),
         parse_float (member "expires_at" j))
      with
      | Some owner, Some acquired_at, Some expires_at ->
          Some { owner; acquired_at; expires_at }
      | _ -> None
    with _ -> None

  let acquire_lock t ~key ~owner ~ttl_seconds =
    let lock_key = "locks:" ^ key in
    let now = Unix.gettimeofday () in
    let info = {
      owner;
      acquired_at = now;
      expires_at = now +. float_of_int ttl_seconds;
    } in
    match set_if_not_exists t lock_key (lock_info_to_json info) with
    | Ok true -> Ok true
    | Ok false -> Ok false
    | Error (AlreadyExists _) ->
        (* Check if expired *)
        (match get t lock_key with
         | Ok json ->
             (match lock_info_of_json json with
              | Some existing when existing.expires_at < now ->
                  (* Expired, try to take over *)
                  (match set t lock_key (lock_info_to_json info) with
                   | Ok () -> Ok true
                   | Error e -> Error e)
              | Some _ -> Ok false
              | None ->
                  (* Invalid lock metadata, overwrite to recover *)
                  (match set t lock_key (lock_info_to_json info) with
                   | Ok () -> Ok true
                   | Error e -> Error e))
         | Error _ -> Ok false)
    | Error e -> Error e

  let release_lock t ~key ~owner =
    let lock_key = "locks:" ^ key in
    match get t lock_key with
    | Ok json ->
        (match lock_info_of_json json with
         | Some info when info.owner = owner ->
             (match delete t lock_key with
              | Ok () -> Ok true
              | Error _ -> Ok false)
         | _ -> Ok false)  (* Not owner or invalid *)
    | Error (NotFound _) -> Ok true  (* Already released *)
    | Error e -> Error e

  let extend_lock t ~key ~owner ~ttl_seconds =
    let lock_key = "locks:" ^ key in
    match get t lock_key with
    | Ok json ->
        (match lock_info_of_json json with
         | Some info when info.owner = owner ->
             let now = Unix.gettimeofday () in
             let new_info = { info with expires_at = now +. float_of_int ttl_seconds } in
             (match set t lock_key (lock_info_to_json new_info) with
              | Ok () -> Ok true
              | Error e -> Error e)
         | _ -> Ok false)
    | Error e -> Error e

  (** {2 Atomic Operations (Cross-Process Safe)} *)

  (** Atomically increment a counter stored in a file.
      Uses Unix.lockf with non-blocking retry for cross-process synchronization.
      Returns the NEW value after increment.

      This is safe for multiple processes accessing the same file.
      Uses F_TLOCK (try lock) to avoid blocking Eio's event loop.
  *)
  let atomic_increment t key =
    match key_to_path t key with
    | Error e -> Error e
    | Ok path ->
        try
          (* Ensure parent directory exists *)
          (match Eio.Path.split path with
           | Some (parent, _) ->
               (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent
                with _ -> ())
           | None -> ());

          (* Get the actual filesystem path string *)
          let path_str = Eio.Path.native_exn path in

          (* Open file for read+write, create if needed *)
          let fd = Unix.openfile path_str [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
          Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->

          (* Acquire exclusive lock with non-blocking retry *)
          let max_retries = 100 in
          let rec try_lock retries =
            if retries <= 0 then
              raise (Failure "Failed to acquire lock after max retries")
            else
              try
                Unix.lockf fd Unix.F_TLOCK 0;  (* Non-blocking try *)
                true
              with Unix.Unix_error (Unix.EAGAIN, _, _)
                 | Unix.Unix_error (Unix.EACCES, _, _) ->
                (* Lock held by another process, wait and retry *)
                Unix.sleepf 0.001;  (* 1ms backoff *)
                try_lock (retries - 1)
          in
          let _ = try_lock max_retries in
          Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) @@ fun () ->

          (* Read current value *)
          let _ = Unix.lseek fd 0 Unix.SEEK_SET in
          let buf = Bytes.create 32 in
          let n = Unix.read fd buf 0 32 in
          let current =
            if n = 0 then 0
            else
              try int_of_string (String.trim (Bytes.sub_string buf 0 n))
              with _ -> 0
          in

          (* Increment and write back *)
          let new_value = current + 1 in
          let new_str = string_of_int new_value in
          let _ = Unix.lseek fd 0 Unix.SEEK_SET in
          let _ = Unix.ftruncate fd 0 in
          let _ = Unix.write_substring fd new_str 0 (String.length new_str) in

          Ok new_value
        with exn ->
          Error (IOError (Printf.sprintf "atomic_increment failed: %s" (Printexc.to_string exn)))

  (** Atomically get the current counter value without incrementing *)
  let atomic_get t key =
    match key_to_path t key with
    | Error e -> Error e
    | Ok path ->
        try
          let path_str = Eio.Path.native_exn path in
          if not (Sys.file_exists path_str) then Ok 0
          else
            let fd = Unix.openfile path_str [Unix.O_RDONLY] 0o644 in
            Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
            (* Shared lock for reading *)
            Unix.lockf fd Unix.F_RLOCK 0;
            Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) @@ fun () ->
            let buf = Bytes.create 32 in
            let n = Unix.read fd buf 0 32 in
            if n = 0 then Ok 0
            else Ok (try int_of_string (String.trim (Bytes.sub_string buf 0 n)) with _ -> 0)
        with exn ->
          Error (IOError (Printf.sprintf "atomic_get failed: %s" (Printexc.to_string exn)))

  (** Atomically update a file with a transform function.
      The transform receives [Some content] if file exists, [None] if not.
      Returns [Ok new_content] on success.

      This is safe for multiple processes accessing the same file.
      Uses F_TLOCK (try lock) to avoid blocking Eio's event loop.
  *)
  let atomic_update t key ~f =
    match key_to_path t key with
    | Error e -> Error e
    | Ok path ->
        try
          (* Ensure parent directory exists *)
          (match Eio.Path.split path with
           | Some (parent, _) ->
               (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent
                with _ -> ())
           | None -> ());

          let path_str = Eio.Path.native_exn path in

          (* Open file for read+write, create if needed *)
          let fd = Unix.openfile path_str [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
          Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->

          (* Acquire exclusive lock with non-blocking retry *)
          let max_retries = 100 in
          let rec try_lock retries =
            if retries <= 0 then
              raise (Failure "Failed to acquire lock after max retries")
            else
              try
                Unix.lockf fd Unix.F_TLOCK 0;
                true
              with Unix.Unix_error (Unix.EAGAIN, _, _)
                 | Unix.Unix_error (Unix.EACCES, _, _) ->
                Unix.sleepf 0.001;
                try_lock (retries - 1)
          in
          let _ = try_lock max_retries in
          Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) @@ fun () ->

          (* Read current content *)
          let _ = Unix.lseek fd 0 Unix.SEEK_SET in
          let stat = Unix.fstat fd in
          let size = stat.Unix.st_size in
          let current =
            if size = 0 then None
            else begin
              let buf = Bytes.create size in
              let n = Unix.read fd buf 0 size in
              if n = 0 then None
              else
                let raw = Bytes.sub_string buf 0 n in
                (* Auto-decompress if ZSTD *)
                Some (Compression.decompress_auto raw)
            end
          in

          (* Apply transform function *)
          let new_content = f current in

          (* Compress and write back *)
          let compressed = Compression.compress_with_header new_content in
          let _ = Unix.lseek fd 0 Unix.SEEK_SET in
          let _ = Unix.ftruncate fd 0 in
          let _ = Unix.write_substring fd compressed 0 (String.length compressed) in

          Ok new_content
        with exn ->
          Error (IOError (Printf.sprintf "atomic_update failed: %s" (Printexc.to_string exn)))

  (** {2 Health Check} *)

  let health_check t =
    let test_key = "_health_check_" ^ t.config.node_id in
    let test_value = string_of_float (Unix.gettimeofday ()) in
    match set t test_key test_value with
    | Ok () ->
        (match delete t test_key with
         | Ok () -> Ok { latency_ms = 0.0; is_healthy = true }
         | Error _ -> Ok { latency_ms = 0.0; is_healthy = false })
    | Error e -> Error e

end

(** {1 Memory Backend (for testing)} *)

module Memory = struct
  type t = {
    mutable data: (string, string) Hashtbl.t;
    mutex: Eio.Mutex.t;
  }

  let create () = {
    data = Hashtbl.create 64;
    mutex = Eio.Mutex.create ();
  }

  let get t key =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match Hashtbl.find_opt t.data key with
      | Some v -> Ok v
      | None -> Error (NotFound key)
    )

  let set t key value =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Hashtbl.replace t.data key value;
      Ok ()
    )

  let exists t key =
    Hashtbl.mem t.data key

  let delete t key =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if Hashtbl.mem t.data key then begin
        Hashtbl.remove t.data key;
        Ok ()
      end else
        Error (NotFound key)
    )

  let list_keys t ~prefix =
    let keys = Hashtbl.fold (fun k _ acc ->
      if String.length k >= String.length prefix &&
         String.sub k 0 (String.length prefix) = prefix then
        k :: acc
      else acc
    ) t.data [] in
    Ok keys

  let clear t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Hashtbl.clear t.data
    )
end

(** {1 PostgreSQL Backend (Eio)} *)

module Postgres = struct
  type t = {
    pool: (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t;
    namespace: string;
    node_id: string;
  }

  let namespaced_key namespace key =
    if namespace = "" || namespace = "default" then key
    else namespace ^ ":" ^ key

  let strip_namespace namespace key =
    let prefix = namespace ^ ":" in
    let prefix_len = String.length prefix in
    if String.length key >= prefix_len && String.sub key 0 prefix_len = prefix then
      String.sub key prefix_len (String.length key - prefix_len)
    else key

  (* Caqti 2.x query definitions *)
  open Caqti_request.Infix

  let get_q =
    (Caqti_type.string ->? Caqti_type.string)
    "SELECT value FROM masc_kv WHERE key = $1 AND (expires_at IS NULL OR expires_at > NOW())"

  let set_q =
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "INSERT INTO masc_kv (key, value, updated_at) VALUES ($1, $2, NOW()) \
     ON CONFLICT (key) DO UPDATE SET value = $2, updated_at = NOW()"

  let delete_q =
    (Caqti_type.string ->. Caqti_type.unit)
    "DELETE FROM masc_kv WHERE key = $1"

  let exists_q =
    (Caqti_type.string ->? Caqti_type.int)
    "SELECT 1 FROM masc_kv WHERE key = $1 AND (expires_at IS NULL OR expires_at > NOW())"

  let list_keys_q =
    (Caqti_type.string ->* Caqti_type.string)
    "SELECT key FROM masc_kv WHERE key LIKE $1 AND (expires_at IS NULL OR expires_at > NOW())"

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

  let health_check_q =
    (Caqti_type.unit ->! Caqti_type.int)
    "SELECT 1"

  (* Schema creation *)
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

  let (let*) = Result.bind

  let caqti_error_to_masc err =
    IOError (Caqti_error.show err)

  let create ~sw ~env ~url config =
    let uri = Uri.of_string url in
    let pool_config = Caqti_pool_config.create ~max_size:10 () in
    match Caqti_eio_unix.connect_pool ~sw ~stdenv:env ~pool_config uri with
    | Error err -> Error (caqti_error_to_masc err)
    | Ok pool ->
        (* Initialize schema *)
        let init_result = Caqti_eio.Pool.use (fun conn ->
          let module C = (val conn : Caqti_eio.CONNECTION) in
          let* () = C.exec create_schema_q () in
          let* () = C.exec create_index_q () in
          Ok ()
        ) pool in
        (match init_result with
         | Error err -> Error (caqti_error_to_masc err)
         | Ok () -> Ok { pool; namespace = config.cluster_name; node_id = config.node_id })

  let get t key =
    let nkey = namespaced_key t.namespace key in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find_opt get_q nkey
    ) t.pool with
    | Ok (Some v) -> Ok (Compression.decompress_auto v)
    | Ok None -> Error (NotFound key)
    | Error err -> Error (caqti_error_to_masc err)

  let set t key value =
    let nkey = namespaced_key t.namespace key in
    let compressed = Compression.compress_with_header value in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.exec set_q (nkey, compressed)
    ) t.pool with
    | Ok () -> Ok ()
    | Error err -> Error (caqti_error_to_masc err)

  let exists t key =
    let nkey = namespaced_key t.namespace key in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find_opt exists_q nkey
    ) t.pool with
    | Ok (Some _) -> true
    | _ -> false

  let delete t key =
    let nkey = namespaced_key t.namespace key in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.exec delete_q nkey
    ) t.pool with
    | Ok () -> Ok ()
    | Error err -> Error (caqti_error_to_masc err)

  let list_keys t ~prefix =
    let nprefix = namespaced_key t.namespace prefix in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.collect_list list_keys_q (nprefix ^ "%")
    ) t.pool with
    | Ok keys -> Ok (List.map (strip_namespace t.namespace) keys)
    | Error err -> Error (caqti_error_to_masc err)

  let set_if_not_exists t key value =
    let nkey = namespaced_key t.namespace key in
    let compressed = Compression.compress_with_header value in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      let* existing = C.find_opt exists_q nkey in
      match existing with
      | Some _ -> Ok false
      | None ->
          let* () = C.exec set_if_not_exists_q (nkey, compressed) in
          Ok true
    ) t.pool with
    | Ok b -> Ok b
    | Error err -> Error (caqti_error_to_masc err)

  let acquire_lock t ~key ~owner ~ttl_seconds =
    let lock_key = namespaced_key t.namespace ("locks:" ^ key) in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      let* existing = C.find_opt get_q lock_key in
      match existing with
      | Some _ -> Ok false
      | None ->
          let* () = C.exec acquire_lock_q (lock_key, owner, ttl_seconds) in
          Ok true
    ) t.pool with
    | Ok b -> Ok b
    | Error err -> Error (caqti_error_to_masc err)

  let release_lock t ~key ~owner =
    let lock_key = namespaced_key t.namespace ("locks:" ^ key) in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.exec release_lock_q (lock_key, owner)
    ) t.pool with
    | Ok () -> Ok true
    | Error err -> Error (caqti_error_to_masc err)

  let extend_lock t ~key ~owner ~ttl_seconds =
    let lock_key = namespaced_key t.namespace ("locks:" ^ key) in
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.exec extend_lock_q (lock_key, ttl_seconds, owner)
    ) t.pool with
    | Ok () -> Ok true
    | Error err -> Error (caqti_error_to_masc err)

  let health_check t =
    match Caqti_eio.Pool.use (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      C.find health_check_q ()
    ) t.pool with
    | Ok 1 -> Ok { latency_ms = 0.0; is_healthy = true }
    | _ -> Ok { latency_ms = 0.0; is_healthy = false }
end

(** {1 Unified Backend} *)

type backend =
  | FS of FileSystem.t
  | Mem of Memory.t
  | PG of Postgres.t

let get = function
  | FS t -> FileSystem.get t
  | Mem t -> Memory.get t
  | PG t -> Postgres.get t

let set = function
  | FS t -> FileSystem.set t
  | Mem t -> Memory.set t
  | PG t -> Postgres.set t

let exists = function
  | FS t -> FileSystem.exists t
  | Mem t -> Memory.exists t
  | PG t -> Postgres.exists t

let delete = function
  | FS t -> FileSystem.delete t
  | Mem t -> Memory.delete t
  | PG t -> Postgres.delete t

let list_keys = function
  | FS t -> FileSystem.list_keys t ~prefix:""
  | Mem t -> Memory.list_keys t ~prefix:""
  | PG t -> Postgres.list_keys t ~prefix:""

let set_if_not_exists backend key value =
  match backend with
  | FS t -> FileSystem.set_if_not_exists t key value
  | Mem t -> 
      (match Memory.get t key with
       | Error (NotFound _) -> Memory.set t key value |> Result.map (fun () -> true)
       | _ -> Ok false)
  | PG t -> Postgres.set_if_not_exists t key value

let acquire_lock backend ~key ~owner ~ttl_seconds =
  match backend with
  | FS t -> FileSystem.acquire_lock t ~key ~owner ~ttl_seconds
  | Mem _ -> Ok true  (* In-memory is single-process *)
  | PG t -> Postgres.acquire_lock t ~key ~owner ~ttl_seconds

let release_lock backend ~key ~owner =
  match backend with
  | FS t -> FileSystem.release_lock t ~key ~owner
  | Mem _ -> Ok true
  | PG t -> Postgres.release_lock t ~key ~owner

let extend_lock backend ~key ~owner ~ttl_seconds =
  match backend with
  | FS t -> FileSystem.extend_lock t ~key ~owner ~ttl_seconds
  | Mem _ -> Ok true
  | PG t -> Postgres.extend_lock t ~key ~owner ~ttl_seconds
