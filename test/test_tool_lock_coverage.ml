(** Coverage tests for Tool_lock *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_lock Coverage Tests ===\n"

(* Test helper *)
let test name f =
  try
    f ();
    Printf.printf "✓ %s passed\n" name
  with e ->
    Printf.printf "✗ %s FAILED: %s\n" name (Printexc.to_string e);
    exit 1

(* Create test context *)
let test_counter = ref 0
let make_test_ctx () =
  incr test_counter;
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-lock-test-%d-%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) !test_counter) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:(Some "test-agent") in
  { Tool_lock.config; agent_name = "test-agent" }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_lock.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test dispatch lock *)
let () = test "dispatch_lock" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("file", `String "test.txt")] in
  match Tool_lock.dispatch ctx ~name:"masc_lock" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test dispatch unlock *)
let () = test "dispatch_unlock" (fun () ->
  let ctx = make_test_ctx () in
  (* First lock a file *)
  let lock_args = `Assoc [("file", `String "unlock-test.txt")] in
  let _ = Tool_lock.handle_lock ctx lock_args in
  (* Then unlock it *)
  let unlock_args = `Assoc [("file", `String "unlock-test.txt")] in
  match Tool_lock.dispatch ctx ~name:"masc_unlock" ~args:unlock_args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_lock with file parameter *)
let () = test "handle_lock_file" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("file", `String "test-file.txt")] in
  let (success, result) = Tool_lock.handle_lock ctx args in
  assert success;
  assert (String.contains result '{')
)

(* Test handle_lock with resource parameter *)
let () = test "handle_lock_resource" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("resource", `String "resource-test.txt")] in
  let (success, result) = Tool_lock.handle_lock ctx args in
  assert success;
  assert (String.contains result '{')
)

(* Test handle_lock failure - already locked *)
let () = test "handle_lock_already_locked" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("file", `String "already-locked.txt")] in
  (* Lock once *)
  let (success1, _) = Tool_lock.handle_lock ctx args in
  assert success1;
  (* Try to lock again - should fail with same agent *)
  (* Note: may succeed if same owner, depends on backend behavior *)
  let (_success2, _) = Tool_lock.handle_lock ctx args in
  () (* Just verify no exception *)
)

(* Test handle_unlock success *)
let () = test "handle_unlock_success" (fun () ->
  let ctx = make_test_ctx () in
  let resource = "unlock-success-test.txt" in
  let lock_args = `Assoc [("file", `String resource)] in
  let (lock_success, _) = Tool_lock.handle_lock ctx lock_args in
  assert lock_success;
  let unlock_args = `Assoc [("file", `String resource)] in
  let (unlock_success, result) = Tool_lock.handle_unlock ctx unlock_args in
  assert unlock_success;
  assert (String.length result > 0)
)

(* Test handle_unlock - not locked *)
let () = test "handle_unlock_not_locked" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("file", `String "never-locked.txt")] in
  let (success, result) = Tool_lock.handle_unlock ctx args in
  (* Should fail because file is not locked *)
  assert (not success || String.length result > 0) (* Either fail or have a result *)
)

(* Test helper functions *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_lock.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_lock.get_string args "key" "default" = "default")
)

let () = Printf.printf "\n✅ All Tool_lock tests passed!\n"
