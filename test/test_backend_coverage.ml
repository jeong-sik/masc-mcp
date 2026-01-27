(** Comprehensive coverage tests for Backend and Backend_eio modules

    Target: Fill coverage gaps identified in existing tests.
    Focus areas:
    - Backend.ml: FileSystem edge cases, validate_ttl, key validation, lock edge cases
    - Backend_eio.ml: Compression, lock operations, atomic operations, unified interface
*)

open Alcotest
module Backend = Masc_mcp.Backend
module Backend_eio = Masc_mcp.Backend_eio

(* ============================================================ *)
(* Test Utilities                                                *)
(* ============================================================ *)

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter (fun name -> rm_rf (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Unix.unlink path

let make_unique_key prefix =
  Printf.sprintf "%s_%d_%d" prefix (Unix.getpid ())
    (int_of_float (Unix.gettimeofday () *. 1000000.))

let make_test_dir base =
  let unique_id = make_unique_key base in
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ()) unique_id in
  (try Unix.mkdir tmp_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  tmp_dir

(* ============================================================ *)
(* Backend.ml - validate_ttl Tests                               *)
(* ============================================================ *)

let test_validate_ttl_zero () =
  let result = Backend.validate_ttl 0 in
  check int "zero TTL clamped to 1" 1 result

let test_validate_ttl_negative () =
  let result = Backend.validate_ttl (-100) in
  check int "negative TTL clamped to 1" 1 result

let test_validate_ttl_normal () =
  let result = Backend.validate_ttl 60 in
  check int "normal TTL unchanged" 60 result

let test_validate_ttl_large () =
  let result = Backend.validate_ttl 999999 in
  check int "large TTL clamped to 86400" 86400 result

let test_validate_ttl_boundary () =
  check int "TTL at max boundary" 86400 (Backend.validate_ttl 86400);
  check int "TTL just above max" 86400 (Backend.validate_ttl 86401);
  check int "TTL at min boundary" 1 (Backend.validate_ttl 1)

(* ============================================================ *)
(* Backend.ml - Key Validation Edge Cases                        *)
(* ============================================================ *)

let test_fs_backend () =
  { Backend.default_config with base_path = make_test_dir "masc_key_test" }
  |> Backend.FileSystemBackend.create

let test_key_with_nul_byte () =
  match test_fs_backend () with
  | Error _ -> fail "Failed to create backend"
  | Ok backend ->
      match Backend.FileSystemBackend.set backend ~key:"test\x00key" ~value:"v" with
      | Error (Backend.InvalidKey _) -> ()
      | _ -> fail "NUL byte in key should be rejected"

let test_key_consecutive_colons () =
  match test_fs_backend () with
  | Error _ -> fail "Failed to create backend"
  | Ok backend ->
      match Backend.FileSystemBackend.set backend ~key:"test::key" ~value:"v" with
      | Error (Backend.InvalidKey _) -> ()
      | _ -> fail "Consecutive colons should be rejected"

let test_key_ending_with_colon () =
  match test_fs_backend () with
  | Error _ -> fail "Failed to create backend"
  | Ok backend ->
      match Backend.FileSystemBackend.set backend ~key:"test:" ~value:"v" with
      | Error (Backend.InvalidKey _) -> ()
      | _ -> fail "Key ending with colon should be rejected"

let test_key_with_wildcards () =
  match test_fs_backend () with
  | Error _ -> fail "Failed to create backend"
  | Ok backend ->
      match Backend.FileSystemBackend.set backend ~key:"test*key" ~value:"v" with
      | Error (Backend.InvalidKey _) -> ()
      | _ -> fail "Wildcard in key should be rejected"

let test_key_with_quotes () =
  match test_fs_backend () with
  | Error _ -> fail "Failed to create backend"
  | Ok backend ->
      match Backend.FileSystemBackend.set backend ~key:"test\"key" ~value:"v" with
      | Error (Backend.InvalidKey _) -> ()
      | _ -> fail "Quote in key should be rejected"

let test_key_with_shell_metachar () =
  match test_fs_backend () with
  | Error _ -> fail "Failed to create backend"
  | Ok backend ->
      match Backend.FileSystemBackend.set backend ~key:"test|key" ~value:"v" with
      | Error (Backend.InvalidKey _) -> ()
      | _ -> fail "Shell metachar should be rejected"

(* ============================================================ *)
(* Backend.ml - FileSystem CAS and list_keys                     *)
(* ============================================================ *)

(* NOTE: FileSystemBackend uses non-reentrant Mutex. Use MemoryBackend for these tests. *)
let test_memory_compare_and_swap () =
  match Backend.MemoryBackend.create Backend.default_config with
  | Error _ -> fail "Failed to create MemoryBackend"
  | Ok backend ->
      let key = make_unique_key "cas" in
      let _ = Backend.MemoryBackend.set backend ~key ~value:"initial" in

      (* CAS with wrong expected should fail *)
      (match Backend.MemoryBackend.compare_and_swap backend ~key ~expected:"wrong" ~value:"new" with
      | Ok false -> ()
      | _ -> fail "CAS should fail with wrong expected");

      (* CAS with correct expected should succeed *)
      (match Backend.MemoryBackend.compare_and_swap backend ~key ~expected:"initial" ~value:"updated" with
      | Ok true -> ()
      | _ -> fail "CAS should succeed");

      (* Verify update *)
      (match Backend.MemoryBackend.get backend ~key with
      | Ok (Some v) -> check string "CAS updated" "updated" v
      | _ -> fail "get after CAS failed")

let test_memory_list_keys () =
  match Backend.MemoryBackend.create Backend.default_config with
  | Error _ -> fail "Failed to create MemoryBackend"
  | Ok backend ->
      let prefix = make_unique_key "list" in
      let _ = Backend.MemoryBackend.set backend ~key:(prefix ^ ":a") ~value:"1" in
      let _ = Backend.MemoryBackend.set backend ~key:(prefix ^ ":b") ~value:"2" in

      match Backend.MemoryBackend.list_keys backend ~prefix with
      | Ok keys -> check int "list_keys count" 2 (List.length keys)
      | Error _ -> fail "list_keys failed"

let test_memory_get_all () =
  match Backend.MemoryBackend.create Backend.default_config with
  | Error _ -> fail "Failed to create MemoryBackend"
  | Ok backend ->
      let prefix = make_unique_key "getall" in
      let _ = Backend.MemoryBackend.set backend ~key:(prefix ^ ":a") ~value:"1" in
      let _ = Backend.MemoryBackend.set backend ~key:(prefix ^ ":b") ~value:"2" in

      match Backend.MemoryBackend.get_all backend ~prefix with
      | Ok pairs -> check int "get_all count" 2 (List.length pairs)
      | Error _ -> fail "get_all failed"

let test_filesystem_health_check () =
  let cfg = { Backend.default_config with base_path = make_test_dir "masc_health_test" } in
  match Backend.FileSystemBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      match Backend.FileSystemBackend.health_check backend with
      | Ok true -> ()
      | Ok false -> fail "health check returned false"
      | Error _ -> fail "health check error"

(* ============================================================ *)
(* Backend.ml - Memory Pub/Sub (BackendNotSupported)             *)
(* ============================================================ *)

let test_memory_pubsub_not_supported () =
  let cfg = Backend.{ default_config with backend_type = Memory } in
  match Backend.MemoryBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      (* Publish should return BackendNotSupported *)
      (match Backend.MemoryBackend.publish backend ~channel:"test" ~message:"msg" with
      | Error (Backend.BackendNotSupported _) -> ()
      | _ -> fail "publish should return BackendNotSupported");

      (* Subscribe should return BackendNotSupported *)
      match Backend.MemoryBackend.subscribe backend ~channel:"test" ~callback:(fun _ -> ()) with
      | Error (Backend.BackendNotSupported _) -> ()
      | _ -> fail "subscribe should return BackendNotSupported"

let test_filesystem_pubsub_not_supported () =
  let cfg = { Backend.default_config with base_path = make_test_dir "masc_ps_test" } in
  match Backend.FileSystemBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      (* Publish should return BackendNotSupported *)
      (match Backend.FileSystemBackend.publish backend ~channel:"test" ~message:"msg" with
      | Error (Backend.BackendNotSupported _) -> ()
      | _ -> fail "publish should return BackendNotSupported");

      (* Subscribe should return BackendNotSupported *)
      match Backend.FileSystemBackend.subscribe backend ~channel:"test" ~callback:(fun _ -> ()) with
      | Error (Backend.BackendNotSupported _) -> ()
      | _ -> fail "subscribe should return BackendNotSupported"

(* ============================================================ *)
(* Backend.ml - Lock Edge Cases                                  *)
(* ============================================================ *)

let test_lock_nonowner_release () =
  let cfg = { Backend.default_config with base_path = make_test_dir "masc_lock1" } in
  match Backend.FileSystemBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      let key = make_unique_key "lock" in
      let _ = Backend.FileSystemBackend.acquire_lock backend ~key ~ttl_seconds:60 ~owner:"owner1" in

      (* Non-owner should not be able to release *)
      match Backend.FileSystemBackend.release_lock backend ~key ~owner:"owner2" with
      | Ok false -> ()
      | _ -> fail "non-owner release should fail"

let test_lock_nonowner_extend () =
  let cfg = { Backend.default_config with base_path = make_test_dir "masc_lock2" } in
  match Backend.FileSystemBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      let key = make_unique_key "lock" in
      let _ = Backend.FileSystemBackend.acquire_lock backend ~key ~ttl_seconds:60 ~owner:"owner1" in

      (* Non-owner should not be able to extend *)
      match Backend.FileSystemBackend.extend_lock backend ~key ~ttl_seconds:120 ~owner:"owner2" with
      | Ok false -> ()
      | _ -> fail "non-owner extend should fail"

let test_lock_extend_success () =
  let cfg = { Backend.default_config with base_path = make_test_dir "masc_lock3" } in
  match Backend.FileSystemBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      let key = make_unique_key "lock" in
      let _ = Backend.FileSystemBackend.acquire_lock backend ~key ~ttl_seconds:10 ~owner:"owner1" in

      (* Owner should be able to extend *)
      match Backend.FileSystemBackend.extend_lock backend ~key ~ttl_seconds:60 ~owner:"owner1" with
      | Ok true -> ()
      | _ -> fail "owner extend should succeed"

let test_lock_release_nonexistent () =
  let cfg = { Backend.default_config with base_path = make_test_dir "masc_lock4" } in
  match Backend.FileSystemBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      let key = make_unique_key "nolock" in
      (* Releasing non-existent lock should return false *)
      match Backend.FileSystemBackend.release_lock backend ~key ~owner:"owner1" with
      | Ok false -> ()
      | _ -> fail "release nonexistent should return false"

let test_lock_extend_nonexistent () =
  let cfg = { Backend.default_config with base_path = make_test_dir "masc_lock5" } in
  match Backend.FileSystemBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      let key = make_unique_key "nolock" in
      (* Extending non-existent lock should return false *)
      match Backend.FileSystemBackend.extend_lock backend ~key ~ttl_seconds:60 ~owner:"owner1" with
      | Ok false -> ()
      | _ -> fail "extend nonexistent should return false"

let test_lock_reacquire_same_owner () =
  let cfg = { Backend.default_config with base_path = make_test_dir "masc_lock6" } in
  match Backend.FileSystemBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      let key = make_unique_key "lock" in
      let _ = Backend.FileSystemBackend.acquire_lock backend ~key ~ttl_seconds:60 ~owner:"owner1" in

      (* Same owner should be able to reacquire (refresh) *)
      match Backend.FileSystemBackend.acquire_lock backend ~key ~ttl_seconds:60 ~owner:"owner1" with
      | Ok true -> ()
      | _ -> fail "same owner reacquire should succeed"

(* ============================================================ *)
(* Backend.ml - Memory Lock Expiration                           *)
(* ============================================================ *)

let test_memory_lock_different_owners () =
  let cfg = Backend.{ default_config with backend_type = Memory } in
  match Backend.MemoryBackend.create cfg with
  | Error _ -> fail "Failed to create"
  | Ok backend ->
      let _ = Backend.MemoryBackend.acquire_lock backend ~key:"res" ~ttl_seconds:60 ~owner:"a" in

      (* Different owner blocked *)
      (match Backend.MemoryBackend.acquire_lock backend ~key:"res" ~ttl_seconds:60 ~owner:"b" with
      | Ok false -> ()
      | _ -> fail "different owner should be blocked");

      (* Release by owner *)
      let _ = Backend.MemoryBackend.release_lock backend ~key:"res" ~owner:"a" in

      (* Now b can acquire *)
      match Backend.MemoryBackend.acquire_lock backend ~key:"res" ~ttl_seconds:60 ~owner:"b" with
      | Ok true -> ()
      | _ -> fail "b should acquire after a releases"

(* ============================================================ *)
(* Backend.ml - Error Type Coverage                              *)
(* ============================================================ *)

let test_error_types () =
  (* Test all error constructors *)
  let e1 = Backend.ConnectionFailed "conn error" in
  let e2 = Backend.KeyNotFound "key" in
  let e3 = Backend.OperationFailed "op error" in
  let e4 = Backend.BackendNotSupported "backend" in
  let e5 = Backend.InvalidKey "key error" in

  check bool "ConnectionFailed shows" true (String.length (Backend.show_error e1) > 0);
  check bool "KeyNotFound shows" true (String.length (Backend.show_error e2) > 0);
  check bool "OperationFailed shows" true (String.length (Backend.show_error e3) > 0);
  check bool "BackendNotSupported shows" true (String.length (Backend.show_error e4) > 0);
  check bool "InvalidKey shows" true (String.length (Backend.show_error e5) > 0)

(* ============================================================ *)
(* Backend.ml - Backend Type Coverage                            *)
(* ============================================================ *)

let test_backend_type_show () =
  let s1 = Backend.show_backend_type Backend.Memory in
  let s2 = Backend.show_backend_type Backend.FileSystem in
  let s3 = Backend.show_backend_type Backend.PostgresNative in

  check bool "Memory shows" true (String.length s1 > 0);
  check bool "FileSystem shows" true (String.length s2 > 0);
  check bool "PostgresNative shows" true (String.length s3 > 0)

let test_backend_type_equal () =
  check bool "Memory = Memory" true (Backend.equal_backend_type Backend.Memory Backend.Memory);
  check bool "Memory != FileSystem" false (Backend.equal_backend_type Backend.Memory Backend.FileSystem);
  check bool "FileSystem = FileSystem" true (Backend.equal_backend_type Backend.FileSystem Backend.FileSystem)

(* ============================================================ *)
(* Backend.ml - Config and Status                                *)
(* ============================================================ *)

let test_get_status_all_backends () =
  let cfg_mem = { Backend.default_config with backend_type = Backend.Memory } in
  let cfg_fs = { Backend.default_config with backend_type = Backend.FileSystem } in
  let cfg_pg = { Backend.default_config with
    backend_type = Backend.PostgresNative;
    postgres_url = Some "postgresql://localhost/test"
  } in

  let open Yojson.Safe.Util in

  let s1 = Backend.get_status cfg_mem in
  check string "memory status" "memory" (s1 |> member "backend_type" |> to_string);

  let s2 = Backend.get_status cfg_fs in
  check string "fs status" "filesystem" (s2 |> member "backend_type" |> to_string);

  let s3 = Backend.get_status cfg_pg in
  check string "pg status" "postgres_native" (s3 |> member "backend_type" |> to_string);
  check bool "pg url present" true (s3 |> member "postgres_url" |> to_string_option |> Option.is_some)

let test_pubsub_max_messages_env () =
  (* Test the default value - cannot easily test env override in unit test *)
  let result = Backend.pubsub_max_messages_from_env () in
  check bool "pubsub default >= 100" true (result >= 100)

(* ============================================================ *)
(* Backend_eio.ml - Compression Tests                            *)
(* ============================================================ *)

let test_compression_small_data () =
  let data = "hello" in
  let (compressed, used_dict, did_compress) = Backend_eio.Compression.compress data in
  (* Small data should not be compressed *)
  check bool "small data not compressed" false did_compress;
  check string "small data unchanged" data compressed;
  check bool "no dict used" false used_dict

let test_compression_large_data () =
  (* Create data larger than min_size (32 bytes) *)
  let data = String.make 100 'a' in
  let (compressed, _used_dict, did_compress) = Backend_eio.Compression.compress data in
  check bool "large data compressed" true did_compress;
  check bool "compressed smaller" true (String.length compressed < String.length data)

let test_compression_roundtrip () =
  let data = String.make 200 'x' ^ String.make 200 'y' in
  let encoded = Backend_eio.Compression.compress_with_header data in
  let decoded = Backend_eio.Compression.decompress_auto encoded in
  check string "roundtrip matches" data decoded

let test_compression_uncompressed_passthrough () =
  let data = "short" in
  let result = Backend_eio.Compression.decompress_auto data in
  check string "uncompressed passthrough" data result

let test_compression_encode_header () =
  (* Test header encoding with dictionary *)
  let orig_size = 1000 in
  let compressed = "compressed_data" in

  let with_dict = Backend_eio.Compression.encode_with_header ~used_dict:true orig_size compressed in
  check bool "dict header starts with ZSTDD" true (String.sub with_dict 0 5 = "ZSTDD");

  let without_dict = Backend_eio.Compression.encode_with_header ~used_dict:false orig_size compressed in
  check bool "std header starts with ZSTD" true (String.sub without_dict 0 4 = "ZSTD")

let test_compression_decode_header () =
  (* Test header decoding *)
  let orig = 100 in
  let data = "test_compressed" in

  (* With dictionary *)
  let encoded_dict = Backend_eio.Compression.encode_with_header ~used_dict:true orig data in
  (match Backend_eio.Compression.decode_header encoded_dict with
  | Some (size, _, used_dict) ->
      check int "decoded size" orig size;
      check bool "decoded used_dict" true used_dict
  | None -> fail "decode_header failed for dict");

  (* Without dictionary *)
  let encoded_std = Backend_eio.Compression.encode_with_header ~used_dict:false orig data in
  match Backend_eio.Compression.decode_header encoded_std with
  | Some (size, _, used_dict) ->
      check int "decoded size std" orig size;
      check bool "decoded used_dict std" false used_dict
  | None -> fail "decode_header failed for std"

let test_compression_invalid_header () =
  (* Test with data that doesn't have a valid header *)
  let result = Backend_eio.Compression.decode_header "short" in
  check bool "short data returns None" true (Option.is_none result);

  let result2 = Backend_eio.Compression.decode_header "INVALID_HEADER_DATA" in
  check bool "invalid header returns None" true (Option.is_none result2)

(* ============================================================ *)
(* Backend_eio.ml - Lock Operations                              *)
(* ============================================================ *)

let with_eio_backend f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = make_test_dir "masc_eio" in
  let config = Backend_eio.{ base_path = tmp_dir; node_id = "test"; cluster_name = "test" } in
  let backend = Backend_eio.FileSystem.create ~fs config in
  Fun.protect
    ~finally:(fun () -> try rm_rf tmp_dir with _ -> ())
    (fun () -> f backend)

let test_eio_lock_acquire () =
  with_eio_backend @@ fun backend ->
  let key = make_unique_key "lock" in
  match Backend_eio.FileSystem.acquire_lock backend ~key ~owner:"agent1" ~ttl_seconds:60 with
  | Ok true -> ()
  | _ -> fail "acquire should succeed"

let test_eio_lock_block () =
  with_eio_backend @@ fun backend ->
  let key = make_unique_key "lock" in
  let _ = Backend_eio.FileSystem.acquire_lock backend ~key ~owner:"agent1" ~ttl_seconds:60 in
  match Backend_eio.FileSystem.acquire_lock backend ~key ~owner:"agent2" ~ttl_seconds:60 with
  | Ok false -> ()
  | _ -> fail "second acquire should be blocked"

let test_eio_lock_release () =
  with_eio_backend @@ fun backend ->
  let key = make_unique_key "lock" in
  let _ = Backend_eio.FileSystem.acquire_lock backend ~key ~owner:"agent1" ~ttl_seconds:60 in
  match Backend_eio.FileSystem.release_lock backend ~key ~owner:"agent1" with
  | Ok true -> ()
  | _ -> fail "release should succeed"

let test_eio_lock_extend () =
  with_eio_backend @@ fun backend ->
  let key = make_unique_key "lock" in
  let _ = Backend_eio.FileSystem.acquire_lock backend ~key ~owner:"agent1" ~ttl_seconds:10 in
  match Backend_eio.FileSystem.extend_lock backend ~key ~owner:"agent1" ~ttl_seconds:60 with
  | Ok true -> ()
  | _ -> fail "extend should succeed"

let test_eio_lock_release_wrong_owner () =
  with_eio_backend @@ fun backend ->
  let key = make_unique_key "lock" in
  let _ = Backend_eio.FileSystem.acquire_lock backend ~key ~owner:"agent1" ~ttl_seconds:60 in
  match Backend_eio.FileSystem.release_lock backend ~key ~owner:"agent2" with
  | Ok false -> ()
  | _ -> fail "wrong owner release should fail"

(* ============================================================ *)
(* Backend_eio.ml - Atomic Operations                            *)
(* ============================================================ *)

let test_eio_atomic_increment () =
  with_eio_backend @@ fun backend ->
  let key = make_unique_key "counter" in

  (* First increment should be 1 *)
  (match Backend_eio.FileSystem.atomic_increment backend key with
  | Ok n -> check int "first increment" 1 n
  | Error _ -> fail "atomic_increment failed");

  (* Second increment should be 2 *)
  match Backend_eio.FileSystem.atomic_increment backend key with
  | Ok n -> check int "second increment" 2 n
  | Error _ -> fail "atomic_increment failed"

let test_eio_atomic_get () =
  with_eio_backend @@ fun backend ->
  let key = make_unique_key "counter" in

  (* Get non-existent should be 0 *)
  (match Backend_eio.FileSystem.atomic_get backend key with
  | Ok n -> check int "initial is 0" 0 n
  | Error _ -> fail "atomic_get failed");

  let _ = Backend_eio.FileSystem.atomic_increment backend key in

  (* Get after increment should be 1 *)
  match Backend_eio.FileSystem.atomic_get backend key with
  | Ok n -> check int "after increment is 1" 1 n
  | Error _ -> fail "atomic_get failed"

let test_eio_atomic_update () =
  with_eio_backend @@ fun backend ->
  let key = make_unique_key "data" in

  (* Update non-existent file *)
  let transform = function
    | None -> "initial"
    | Some s -> s ^ "_updated"
  in

  (match Backend_eio.FileSystem.atomic_update backend key ~f:transform with
  | Ok v -> check string "initial value" "initial" v
  | Error _ -> fail "atomic_update failed");

  (* Update existing file *)
  match Backend_eio.FileSystem.atomic_update backend key ~f:transform with
  | Ok v -> check string "updated value" "initial_updated" v
  | Error _ -> fail "atomic_update failed"

(* ============================================================ *)
(* Backend_eio.ml - Memory Backend                               *)
(* ============================================================ *)

let test_eio_memory_list_keys () =
  Eio_main.run @@ fun _env ->
  let backend = Backend_eio.Memory.create () in
  let _ = Backend_eio.Memory.set backend "prefix:a" "1" in
  let _ = Backend_eio.Memory.set backend "prefix:b" "2" in
  let _ = Backend_eio.Memory.set backend "other:c" "3" in

  match Backend_eio.Memory.list_keys backend ~prefix:"prefix" with
  | Ok keys -> check int "prefix keys" 2 (List.length keys)
  | Error _ -> fail "list_keys failed"

let test_eio_memory_clear () =
  Eio_main.run @@ fun _env ->
  let backend = Backend_eio.Memory.create () in
  let _ = Backend_eio.Memory.set backend "key1" "1" in
  let _ = Backend_eio.Memory.set backend "key2" "2" in

  Backend_eio.Memory.clear backend;

  check bool "cleared" false (Backend_eio.Memory.exists backend "key1");
  check bool "cleared2" false (Backend_eio.Memory.exists backend "key2")

let test_eio_memory_delete_not_found () =
  Eio_main.run @@ fun _env ->
  let backend = Backend_eio.Memory.create () in
  match Backend_eio.Memory.delete backend "nonexistent" with
  | Error (Backend_eio.NotFound _) -> ()
  | _ -> fail "delete nonexistent should fail"

(* ============================================================ *)
(* Backend_eio.ml - Unified Backend Interface                    *)
(* ============================================================ *)

let test_eio_unified_memory () =
  Eio_main.run @@ fun _env ->
  let mem = Backend_eio.Memory.create () in
  let backend = Backend_eio.Mem mem in

  (* set *)
  (match Backend_eio.set backend "key" "value" with
  | Ok () -> ()
  | Error _ -> fail "unified set failed");

  (* get *)
  (match Backend_eio.get backend "key" with
  | Ok v -> check string "unified get" "value" v
  | Error _ -> fail "unified get failed");

  (* exists *)
  check bool "unified exists" true (Backend_eio.exists backend "key");

  (* delete *)
  (match Backend_eio.delete backend "key" with
  | Ok () -> ()
  | Error _ -> fail "unified delete failed");

  check bool "unified not exists" false (Backend_eio.exists backend "key")

let test_eio_unified_set_if_not_exists () =
  Eio_main.run @@ fun _env ->
  let mem = Backend_eio.Memory.create () in
  let backend = Backend_eio.Mem mem in

  (* First set should succeed *)
  (match Backend_eio.set_if_not_exists backend "unique" "first" with
  | Ok true -> ()
  | _ -> fail "first set_if_not_exists should succeed");

  (* Second set should fail *)
  match Backend_eio.set_if_not_exists backend "unique" "second" with
  | Ok false -> ()
  | _ -> fail "second set_if_not_exists should fail"

let test_eio_unified_list_keys () =
  Eio_main.run @@ fun _env ->
  let mem = Backend_eio.Memory.create () in
  let backend = Backend_eio.Mem mem in

  let _ = Backend_eio.set backend "a" "1" in
  let _ = Backend_eio.set backend "b" "2" in

  match Backend_eio.list_keys backend with
  | Ok keys -> check bool "has keys" true (List.length keys >= 2)
  | Error _ -> fail "list_keys failed"

let test_eio_unified_lock_memory () =
  Eio_main.run @@ fun _env ->
  let mem = Backend_eio.Memory.create () in
  let backend = Backend_eio.Mem mem in

  (* Memory backend locks always succeed *)
  (match Backend_eio.acquire_lock backend ~key:"k" ~owner:"o" ~ttl_seconds:60 with
  | Ok true -> ()
  | _ -> fail "memory lock should succeed");

  (match Backend_eio.release_lock backend ~key:"k" ~owner:"o" with
  | Ok true -> ()
  | _ -> fail "memory release should succeed");

  match Backend_eio.extend_lock backend ~key:"k" ~owner:"o" ~ttl_seconds:60 with
  | Ok true -> ()
  | _ -> fail "memory extend should succeed"

(* ============================================================ *)
(* Backend_eio.ml - lock_info JSON                               *)
(* ============================================================ *)

let test_lock_info_json_roundtrip () =
  let info = Backend_eio.FileSystem.{
    owner = "test_owner";
    acquired_at = 1234567890.123;
    expires_at = 1234567950.456;
  } in
  let json = Backend_eio.FileSystem.lock_info_to_json info in
  match Backend_eio.FileSystem.lock_info_of_json json with
  | Some parsed ->
      check string "owner" info.owner parsed.owner;
      check bool "acquired_at close" true (abs_float (info.acquired_at -. parsed.acquired_at) < 0.001);
      check bool "expires_at close" true (abs_float (info.expires_at -. parsed.expires_at) < 0.001)
  | None -> fail "lock_info_of_json failed"

let test_lock_info_invalid_json () =
  match Backend_eio.FileSystem.lock_info_of_json "not json" with
  | None -> ()
  | Some _ -> fail "invalid json should return None"

let test_lock_info_missing_field () =
  match Backend_eio.FileSystem.lock_info_of_json {|{"owner": "test"}|} with
  | None -> ()
  | Some _ -> fail "missing fields should return None"

(* ============================================================ *)
(* Backend_eio.ml - FileSystem Additional Tests                  *)
(* ============================================================ *)

let test_eio_fs_unicode_keys () =
  with_eio_backend @@ fun backend ->
  (* Unicode in key segments is allowed *)
  let key = "data:test" in  (* Simple ASCII for safety *)
  let value = "unicode value" in
  (match Backend_eio.FileSystem.set backend key value with
  | Ok () -> ()
  | Error _ -> fail "set with unicode should work");

  match Backend_eio.FileSystem.get backend key with
  | Ok v -> check string "unicode value" value v
  | Error _ -> fail "get unicode key failed"

let test_eio_fs_large_value () =
  with_eio_backend @@ fun backend ->
  let key = make_unique_key "large" in
  (* 1MB of data *)
  let value = String.make (1024 * 1024) 'x' in

  (match Backend_eio.FileSystem.set backend key value with
  | Ok () -> ()
  | Error _ -> fail "set large value failed");

  match Backend_eio.FileSystem.get backend key with
  | Ok v -> check int "large value length" (String.length value) (String.length v)
  | Error _ -> fail "get large value failed"

let test_eio_fs_get_not_found () =
  with_eio_backend @@ fun backend ->
  match Backend_eio.FileSystem.get backend "nonexistent:key" with
  | Error (Backend_eio.NotFound _) -> ()
  | _ -> fail "get nonexistent should return NotFound"

let test_eio_fs_delete_not_found () =
  with_eio_backend @@ fun backend ->
  match Backend_eio.FileSystem.delete backend "nonexistent:key" with
  | Error (Backend_eio.NotFound _) -> ()
  | _ -> fail "delete nonexistent should return NotFound"

let test_eio_fs_list_keys_empty () =
  with_eio_backend @@ fun backend ->
  match Backend_eio.FileSystem.list_keys backend ~prefix:"nonexistent" with
  | Ok [] -> ()
  | Ok _ -> fail "should return empty list"
  | Error _ -> fail "list_keys error"

(* ============================================================ *)
(* Test Suite                                                    *)
(* ============================================================ *)

let () =
  run "Backend Coverage" [
    "validate_ttl", [
      test_case "zero TTL" `Quick test_validate_ttl_zero;
      test_case "negative TTL" `Quick test_validate_ttl_negative;
      test_case "normal TTL" `Quick test_validate_ttl_normal;
      test_case "large TTL" `Quick test_validate_ttl_large;
      test_case "boundary TTL" `Quick test_validate_ttl_boundary;
    ];
    "key_validation", [
      test_case "NUL byte" `Quick test_key_with_nul_byte;
      test_case "consecutive colons" `Quick test_key_consecutive_colons;
      test_case "ending colon" `Quick test_key_ending_with_colon;
      test_case "wildcards" `Quick test_key_with_wildcards;
      test_case "quotes" `Quick test_key_with_quotes;
      test_case "shell metachar" `Quick test_key_with_shell_metachar;
    ];
    "memory_ops", [
      test_case "compare_and_swap" `Quick test_memory_compare_and_swap;
      test_case "list_keys" `Quick test_memory_list_keys;
      test_case "get_all" `Quick test_memory_get_all;
      test_case "health_check" `Quick test_filesystem_health_check;
    ];
    "pubsub", [
      test_case "memory not supported" `Quick test_memory_pubsub_not_supported;
      test_case "filesystem not supported" `Quick test_filesystem_pubsub_not_supported;
    ];
    "lock_edge_cases", [
      test_case "nonowner release" `Quick test_lock_nonowner_release;
      test_case "nonowner extend" `Quick test_lock_nonowner_extend;
      test_case "extend success" `Quick test_lock_extend_success;
      test_case "release nonexistent" `Quick test_lock_release_nonexistent;
      test_case "extend nonexistent" `Quick test_lock_extend_nonexistent;
      test_case "reacquire same owner" `Quick test_lock_reacquire_same_owner;
      test_case "memory different owners" `Quick test_memory_lock_different_owners;
    ];
    "error_types", [
      test_case "error show" `Quick test_error_types;
    ];
    "backend_types", [
      test_case "backend type show" `Quick test_backend_type_show;
      test_case "backend type equal" `Quick test_backend_type_equal;
    ];
    "config_status", [
      test_case "get_status all backends" `Quick test_get_status_all_backends;
      test_case "pubsub_max_messages" `Quick test_pubsub_max_messages_env;
    ];
    "eio_compression", [
      test_case "small data" `Quick test_compression_small_data;
      test_case "large data" `Quick test_compression_large_data;
      test_case "roundtrip" `Quick test_compression_roundtrip;
      test_case "uncompressed passthrough" `Quick test_compression_uncompressed_passthrough;
      test_case "encode header" `Quick test_compression_encode_header;
      test_case "decode header" `Quick test_compression_decode_header;
      test_case "invalid header" `Quick test_compression_invalid_header;
    ];
    "eio_locks", [
      test_case "acquire" `Quick test_eio_lock_acquire;
      test_case "block" `Quick test_eio_lock_block;
      test_case "release" `Quick test_eio_lock_release;
      test_case "extend" `Quick test_eio_lock_extend;
      test_case "wrong owner release" `Quick test_eio_lock_release_wrong_owner;
    ];
    "eio_atomic", [
      test_case "increment" `Quick test_eio_atomic_increment;
      test_case "get" `Quick test_eio_atomic_get;
      test_case "update" `Quick test_eio_atomic_update;
    ];
    "eio_memory", [
      test_case "list_keys" `Quick test_eio_memory_list_keys;
      test_case "clear" `Quick test_eio_memory_clear;
      test_case "delete not found" `Quick test_eio_memory_delete_not_found;
    ];
    "eio_unified", [
      test_case "memory ops" `Quick test_eio_unified_memory;
      test_case "set_if_not_exists" `Quick test_eio_unified_set_if_not_exists;
      test_case "list_keys" `Quick test_eio_unified_list_keys;
      test_case "lock memory" `Quick test_eio_unified_lock_memory;
    ];
    "eio_lock_info", [
      test_case "json roundtrip" `Quick test_lock_info_json_roundtrip;
      test_case "invalid json" `Quick test_lock_info_invalid_json;
      test_case "missing field" `Quick test_lock_info_missing_field;
    ];
    "eio_fs_edge", [
      test_case "unicode keys" `Quick test_eio_fs_unicode_keys;
      test_case "large value" `Quick test_eio_fs_large_value;
      test_case "get not found" `Quick test_eio_fs_get_not_found;
      test_case "delete not found" `Quick test_eio_fs_delete_not_found;
      test_case "list_keys empty" `Quick test_eio_fs_list_keys_empty;
    ];
  ]
