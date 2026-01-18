(** Tests for Backend_eio: OCaml 5.x Eio-native storage backend *)

open Masc_mcp

(** Recursive directory cleanup *)
let rec rm_rf path =
  if Sys.is_directory path then begin
    Array.iter (fun name -> rm_rf (Filename.concat path name)) (Sys.readdir path);
    Unix.rmdir path
  end else
    Unix.unlink path

(** Generate unique test directory *)
let make_test_dir () =
  let unique_id = Printf.sprintf "masc_eio_test_%d_%d_%d"
    (int_of_float (Unix.gettimeofday () *. 1000000.))
    (Unix.getpid ())
    (Random.int 100000) in
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ()) unique_id in
  (try Unix.mkdir tmp_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  tmp_dir

(** Run test with Eio environment *)
let with_eio_env f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = make_test_dir () in
  let config = Backend_eio.{
    base_path = tmp_dir;
    node_id = "test_node";
    cluster_name = "test";
  } in
  let backend = Backend_eio.FileSystem.create ~fs config in
  Fun.protect
    ~finally:(fun () -> try rm_rf tmp_dir with _ -> ())
    (fun () -> f backend)

(** Test basic set/get operations *)
let test_set_get () =
  with_eio_env @@ fun backend ->
  let key = "test:key" in
  let value = "hello world" in

  (* Set value *)
  (match Backend_eio.FileSystem.set backend key value with
   | Ok () -> ()
   | Error e -> Alcotest.failf "set failed: %s" (match e with
       | Backend_eio.IOError msg -> msg
       | Backend_eio.NotFound k -> "not found: " ^ k
       | Backend_eio.AlreadyExists k -> "already exists: " ^ k
       | Backend_eio.InvalidKey k -> "invalid key: " ^ k));

  (* Get value *)
  match Backend_eio.FileSystem.get backend key with
  | Ok v -> Alcotest.(check string) "value matches" value v
  | Error e -> Alcotest.failf "get failed: %s" (match e with
      | Backend_eio.IOError msg -> msg
      | Backend_eio.NotFound k -> "not found: " ^ k
      | Backend_eio.AlreadyExists k -> "already exists: " ^ k
      | Backend_eio.InvalidKey k -> "invalid key: " ^ k)

(** Test exists function *)
let test_exists () =
  with_eio_env @@ fun backend ->
  let key = "exists:test" in

  (* Should not exist initially *)
  Alcotest.(check bool) "not exists initially" false
    (Backend_eio.FileSystem.exists backend key);

  (* Set value *)
  let _ = Backend_eio.FileSystem.set backend key "value" in

  (* Should exist now *)
  Alcotest.(check bool) "exists after set" true
    (Backend_eio.FileSystem.exists backend key)

(** Test delete operation *)
let test_delete () =
  with_eio_env @@ fun backend ->
  let key = "delete:test" in

  (* Set then delete *)
  let _ = Backend_eio.FileSystem.set backend key "to be deleted" in
  Alcotest.(check bool) "exists before delete" true
    (Backend_eio.FileSystem.exists backend key);

  (match Backend_eio.FileSystem.delete backend key with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "delete failed");

  Alcotest.(check bool) "not exists after delete" false
    (Backend_eio.FileSystem.exists backend key)

(** Test key validation *)
let test_key_validation () =
  with_eio_env @@ fun backend ->

  (* Empty key should fail *)
  (match Backend_eio.FileSystem.set backend "" "value" with
   | Error (Backend_eio.InvalidKey _) -> ()
   | _ -> Alcotest.fail "empty key should fail");

  (* Key with slash should fail *)
  (match Backend_eio.FileSystem.set backend "test/key" "value" with
   | Error (Backend_eio.InvalidKey _) -> ()
   | _ -> Alcotest.fail "key with slash should fail");

  (* Key starting with colon should fail *)
  (match Backend_eio.FileSystem.set backend ":test" "value" with
   | Error (Backend_eio.InvalidKey _) -> ()
   | _ -> Alcotest.fail "key starting with colon should fail");

  (* Path traversal should fail *)
  (match Backend_eio.FileSystem.set backend "..:..:etc:passwd" "value" with
   | Error (Backend_eio.InvalidKey _) -> ()
   | _ -> Alcotest.fail "path traversal should fail")

(** Test nested keys *)
let test_nested_keys () =
  with_eio_env @@ fun backend ->
  let key = "rooms:room1:messages:msg001" in
  let value = "nested value" in

  (* Set nested key *)
  (match Backend_eio.FileSystem.set backend key value with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "set nested key failed");

  (* Get nested key *)
  match Backend_eio.FileSystem.get backend key with
  | Ok v -> Alcotest.(check string) "nested value matches" value v
  | Error _ -> Alcotest.fail "get nested key failed"

(** Test set_if_not_exists *)
let test_set_if_not_exists () =
  with_eio_env @@ fun backend ->
  let key = "exclusive:key" in

  (* First set should succeed *)
  (match Backend_eio.FileSystem.set_if_not_exists backend key "first" with
   | Ok true -> ()
   | _ -> Alcotest.fail "first set_if_not_exists should succeed");

  (* Second set should fail with AlreadyExists *)
  match Backend_eio.FileSystem.set_if_not_exists backend key "second" with
  | Error (Backend_eio.AlreadyExists _) -> ()
  | _ -> Alcotest.fail "second set_if_not_exists should fail"

(** Test Memory backend basic operations *)
let test_memory_backend () =
  Eio_main.run @@ fun _env ->
  let backend = Backend_eio.Memory.create () in

  (* Set value *)
  (match Backend_eio.Memory.set backend "key1" "value1" with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "memory set failed");

  (* Get value *)
  (match Backend_eio.Memory.get backend "key1" with
   | Ok v -> Alcotest.(check string) "memory get" "value1" v
   | Error _ -> Alcotest.fail "memory get failed");

  (* Exists check *)
  Alcotest.(check bool) "memory exists" true
    (Backend_eio.Memory.exists backend "key1");

  (* Delete *)
  (match Backend_eio.Memory.delete backend "key1" with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "memory delete failed");

  Alcotest.(check bool) "memory not exists after delete" false
    (Backend_eio.Memory.exists backend "key1")

(** Test health check *)
let test_health_check () =
  with_eio_env @@ fun backend ->
  match Backend_eio.FileSystem.health_check backend with
  | Ok result -> Alcotest.(check bool) "is healthy" true result.is_healthy
  | Error _ -> Alcotest.fail "health check failed"

(** Test unified backend interface *)
let test_unified_backend () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = make_test_dir () in
  let config = Backend_eio.{
    base_path = tmp_dir;
    node_id = "unified_test";
    cluster_name = "test";
  } in
  let fs_backend = Backend_eio.FileSystem.create ~fs config in
  let backend = Backend_eio.FS fs_backend in

  Fun.protect
    ~finally:(fun () -> try rm_rf tmp_dir with _ -> ())
    (fun () ->
      (* Test through unified interface *)
      (match Backend_eio.set backend "unified:key" "unified value" with
       | Ok () -> ()
       | Error _ -> Alcotest.fail "unified set failed");

      match Backend_eio.get backend "unified:key" with
      | Ok v -> Alcotest.(check string) "unified get" "unified value" v
      | Error _ -> Alcotest.fail "unified get failed")

let () =
  Alcotest.run "Backend_eio" [
    "basic", [
      Alcotest.test_case "set and get" `Quick test_set_get;
      Alcotest.test_case "exists" `Quick test_exists;
      Alcotest.test_case "delete" `Quick test_delete;
    ];
    "validation", [
      Alcotest.test_case "key validation" `Quick test_key_validation;
      Alcotest.test_case "nested keys" `Quick test_nested_keys;
    ];
    "atomic", [
      Alcotest.test_case "set_if_not_exists" `Quick test_set_if_not_exists;
    ];
    "memory", [
      Alcotest.test_case "memory backend" `Quick test_memory_backend;
    ];
    "health", [
      Alcotest.test_case "health check" `Quick test_health_check;
    ];
    "unified", [
      Alcotest.test_case "unified backend" `Quick test_unified_backend;
    ];
  ]
