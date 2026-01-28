(** Coverage tests for Tool_task *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_task Coverage Tests ===\n"

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
    (Printf.sprintf "masc-task-test-%d-%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) !test_counter) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:(Some "test-agent") in
  { Tool_task.config; agent_name = "test-agent" }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_task.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test dispatch add_task *)
let () = test "dispatch_add_task" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("title", `String "Test task"); ("priority", `Int 2)] in
  match Tool_task.dispatch ctx ~name:"masc_add_task" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test dispatch tasks *)
let () = test "dispatch_tasks" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_task.dispatch ctx ~name:"masc_tasks" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test dispatch claim *)
let () = test "dispatch_claim" (fun () ->
  let ctx = make_test_ctx () in
  (* First add a task *)
  let _ = Tool_task.handle_add_task ctx (`Assoc [("title", `String "Claim test")]) in
  let args = `Assoc [("task_id", `String "task-001")] in
  match Tool_task.dispatch ctx ~name:"masc_claim" ~args with
  | Some (_success, _result) -> () (* May fail if task doesn't exist *)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch claim_next *)
let () = test "dispatch_claim_next" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_task.dispatch ctx ~name:"masc_claim_next" ~args with
  | Some (_success, _result) -> ()
  | None -> failwith "dispatch returned None"
)

(* Test dispatch release *)
let () = test "dispatch_release" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("task_id", `String "task-001")] in
  match Tool_task.dispatch ctx ~name:"masc_release" ~args with
  | Some (_success, _result) -> ()
  | None -> failwith "dispatch returned None"
)

(* Test dispatch transition *)
let () = test "dispatch_transition" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("action", `String "start")] in
  match Tool_task.dispatch ctx ~name:"masc_transition" ~args with
  | Some (_success, _result) -> ()
  | None -> failwith "dispatch returned None"
)

(* Test dispatch update_priority *)
let () = test "dispatch_update_priority" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("priority", `Int 1)] in
  match Tool_task.dispatch ctx ~name:"masc_update_priority" ~args with
  | Some (_success, _result) -> ()
  | None -> failwith "dispatch returned None"
)

(* Test dispatch task_history *)
let () = test "dispatch_task_history" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("task_id", `String "task-001")] in
  match Tool_task.dispatch ctx ~name:"masc_task_history" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test dispatch archive_view *)
let () = test "dispatch_archive_view" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("limit", `Int 10)] in
  match Tool_task.dispatch ctx ~name:"masc_archive_view" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test batch_add_tasks *)
let () = test "handle_batch_add_tasks" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [
    ("tasks", `List [
      `Assoc [("title", `String "Task 1"); ("priority", `Int 1)];
      `Assoc [("title", `String "Task 2"); ("priority", `Int 2)];
    ])
  ] in
  let (success, _) = Tool_task.handle_batch_add_tasks ctx args in
  assert success
)

(* Test helper functions *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_task.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_task.get_string args "key" "default" = "default")
)

let () = test "get_int_present" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_task.get_int args "key" 0 = 42)
)

let () = test "get_int_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_task.get_int args "key" 99 = 99)
)

let () = test "get_int_opt_present" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_task.get_int_opt args "key" = Some 42)
)

let () = test "get_int_opt_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_task.get_int_opt args "key" = None)
)

let () = Printf.printf "\n✅ All Tool_task tests passed!\n"
