(** Coverage tests for Tool_hat *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_hat Coverage Tests ===\n"

(* Test helper *)
let test name f =
  try
    f ();
    Printf.printf "✓ %s passed\n" name
  with e ->
    Printf.printf "✗ %s FAILED: %s\n" name (Printexc.to_string e);
    exit 1

(* Create test context - initialize room to avoid "not initialized" error *)
let make_test_ctx () =
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-hat-test-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  (* Initialize the room to avoid "MASC not initialized" error *)
  let _ = Room.init config ~agent_name:None in
  { Tool_hat.config; agent_name = "test-agent" }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_hat.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test hat_wear dispatch *)
let () = test "dispatch_hat_wear" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("hat", `String "builder")] in
  match Tool_hat.dispatch ctx ~name:"masc_hat_wear" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_hat_wear with different hats *)
let () = test "handle_hat_wear_builder" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("hat", `String "builder")] in
  let (success, result) = Tool_hat.handle_hat_wear ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

let () = test "handle_hat_wear_reviewer" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("hat", `String "reviewer")] in
  let (success, result) = Tool_hat.handle_hat_wear ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

let () = test "handle_hat_wear_researcher" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("hat", `String "researcher")] in
  let (success, result) = Tool_hat.handle_hat_wear ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

let () = test "handle_hat_wear_tester" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("hat", `String "tester")] in
  let (success, result) = Tool_hat.handle_hat_wear ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

let () = test "handle_hat_wear_architect" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("hat", `String "architect")] in
  let (success, result) = Tool_hat.handle_hat_wear ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

let () = test "handle_hat_wear_debugger" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("hat", `String "debugger")] in
  let (success, result) = Tool_hat.handle_hat_wear ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

let () = test "handle_hat_wear_documenter" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("hat", `String "documenter")] in
  let (success, result) = Tool_hat.handle_hat_wear ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

let () = test "handle_hat_wear_custom" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("hat", `String "custom-hat")] in
  let (success, result) = Tool_hat.handle_hat_wear ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

let () = test "handle_hat_wear_default" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_hat.handle_hat_wear ctx args in
  assert success;
  (* Default is builder *)
  assert (String.length result > 0 (* contains emoji *))
)

(* Test hat_status dispatch *)
let () = test "dispatch_hat_status" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_hat.dispatch ctx ~name:"masc_hat_status" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_hat_status with agents *)
let () = test "handle_hat_status_with_agents" (fun () ->
  let ctx = make_test_ctx () in
  (* First wear a hat *)
  let _ = Tool_hat.handle_hat_wear ctx (`Assoc [("hat", `String "tester")]) in
  let args = `Assoc [] in
  let (success, result) = Tool_hat.handle_hat_status ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

(* Test get_string helper *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_hat.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_hat.get_string args "key" "default" = "default")
)

let () = test "get_string_wrong_type" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_hat.get_string args "key" "default" = "default")
)

let () = Printf.printf "\n✅ All Tool_hat tests passed!\n"
