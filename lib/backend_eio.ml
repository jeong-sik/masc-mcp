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

(** {1 Unified Backend} *)

type backend =
  | FS of FileSystem.t
  | Mem of Memory.t

let get = function
  | FS t -> FileSystem.get t
  | Mem t -> Memory.get t

let set = function
  | FS t -> FileSystem.set t
  | Mem t -> Memory.set t

let exists = function
  | FS t -> FileSystem.exists t
  | Mem t -> Memory.exists t

let delete = function
  | FS t -> FileSystem.delete t
  | Mem t -> Memory.delete t

let list_keys = function
  | FS t -> FileSystem.list_keys t
  | Mem t -> Memory.list_keys t
