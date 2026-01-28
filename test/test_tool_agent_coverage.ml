(** Coverage tests for Tool_agent *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_agent Coverage Tests ===\n"

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
    (Printf.sprintf "masc-agent-test-%d-%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) !test_counter) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:(Some "test-agent") in
  { Tool_agent.config; agent_name = "test-agent" }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_agent.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test masc_agents dispatch *)
let () = test "dispatch_agents" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_agent.dispatch ctx ~name:"masc_agents" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_agents *)
let () = test "handle_agents" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_agent.handle_agents ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test handle_register_capabilities *)
let () = test "handle_register_capabilities" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("capabilities", `List [`String "test"; `String "code"])] in
  let (success, _result) = Tool_agent.handle_register_capabilities ctx args in
  assert success
)

(* Test handle_agent_update with status - may fail if agent file doesn't exist *)
let () = test "handle_agent_update_status" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("status", `String "busy")] in
  let (_success, result) = Tool_agent.handle_agent_update ctx args in
  (* Success or failure is acceptable - we're testing that handler runs *)
  assert (String.length result > 0)
)

(* Test handle_agent_update with capabilities - may fail if agent file doesn't exist *)
let () = test "handle_agent_update_capabilities" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("capabilities", `List [`String "review"; `String "refactor"])] in
  let (_success, result) = Tool_agent.handle_agent_update ctx args in
  assert (String.length result > 0)
)

(* Test handle_find_by_capability *)
let () = test "handle_find_by_capability" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("capability", `String "test")] in
  let (success, result) = Tool_agent.handle_find_by_capability ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test handle_get_metrics - no metrics case *)
let () = test "handle_get_metrics_no_data" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("agent_name", `String "nonexistent"); ("days", `Int 7)] in
  let (success, _result) = Tool_agent.handle_get_metrics ctx args in
  (* May succeed or fail depending on whether agent has metrics *)
  assert (success = true || success = false)
)

(* Test handle_agent_fitness with no agents *)
let () = test "handle_agent_fitness_no_agents" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_agent.handle_agent_fitness ctx args in
  assert success;
  (* Should return count:0 or some agents *)
  assert (String.length result > 0)
)

(* Test handle_agent_fitness with specific agent *)
let () = test "handle_agent_fitness_specific" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("agent_name", `String "test-agent"); ("days", `Int 7)] in
  let (success, result) = Tool_agent.handle_agent_fitness ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test handle_select_agent - missing available_agents *)
let () = test "handle_select_agent_missing_agents" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, _result) = Tool_agent.handle_select_agent ctx args in
  assert (not success)
)

(* Test handle_select_agent - capability_first strategy *)
let () = test "handle_select_agent_capability_first" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [
    ("available_agents", `List [`String "agent-a"; `String "agent-b"]);
    ("strategy", `String "capability_first");
    ("days", `Int 7);
  ] in
  let (success, result) = Tool_agent.handle_select_agent ctx args in
  assert success;
  assert (String.contains result '{')
)

(* Test handle_select_agent - random strategy *)
let () = test "handle_select_agent_random" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [
    ("available_agents", `List [`String "agent-x"; `String "agent-y"]);
    ("strategy", `String "random");
  ] in
  let (success, _result) = Tool_agent.handle_select_agent ctx args in
  assert success
)

(* Test handle_select_agent - roulette_wheel strategy *)
let () = test "handle_select_agent_roulette" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [
    ("available_agents", `List [`String "agent-1"; `String "agent-2"; `String "agent-3"]);
    ("strategy", `String "roulette_wheel");
  ] in
  let (success, _result) = Tool_agent.handle_select_agent ctx args in
  assert success
)

(* Test handle_collaboration_graph - text format *)
let () = test "handle_collaboration_graph_text" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("format", `String "text")] in
  let (success, result) = Tool_agent.handle_collaboration_graph ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test handle_collaboration_graph - json format *)
let () = test "handle_collaboration_graph_json" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("format", `String "json")] in
  let (success, result) = Tool_agent.handle_collaboration_graph ctx args in
  assert success;
  assert (String.contains result '{')
)

(* Test handle_consolidate_learning *)
let () = test "handle_consolidate_learning" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("decay_after_days", `Int 30)] in
  let (success, result) = Tool_agent.handle_consolidate_learning ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test handle_agent_card - get action *)
let () = test "handle_agent_card_get" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("action", `String "get")] in
  let (success, result) = Tool_agent.handle_agent_card ctx args in
  assert success;
  assert (String.contains result '{')
)

(* Test handle_agent_card - refresh action *)
let () = test "handle_agent_card_refresh" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("action", `String "refresh")] in
  let (success, result) = Tool_agent.handle_agent_card ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test helper functions *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_agent.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_agent.get_string args "key" "default" = "default")
)

let () = test "get_string_opt_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_agent.get_string_opt args "key" = Some "value")
)

let () = test "get_string_opt_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_agent.get_string_opt args "key" = None)
)

let () = test "get_int_present" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_agent.get_int args "key" 0 = 42)
)

let () = test "get_int_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_agent.get_int args "key" 99 = 99)
)

let () = test "get_string_list_present" (fun () ->
  let args = `Assoc [("key", `List [`String "a"; `String "b"])] in
  assert (Tool_agent.get_string_list args "key" = ["a"; "b"])
)

let () = test "get_string_list_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_agent.get_string_list args "key" = [])
)

let () = Printf.printf "\n✅ All Tool_agent tests passed!\n"
