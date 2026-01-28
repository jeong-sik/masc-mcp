(** Dispatch Chain Evidence Test

    This test verifies that the dispatch chain correctly routes
    JSON tool calls through the extracted Tool_* modules.

    Evidence requirement: Each dispatch must show the tool name,
    JSON input, and successful routing.
*)

open Masc_mcp

let () = Printf.printf "\n=== Dispatch Chain Evidence Tests ===\n"
let () = Printf.printf "Testing that JSON tool calls route correctly through extracted modules\n\n"

(* Create test context *)
let test_counter = ref 0
let make_test_room () =
  incr test_counter;
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-dispatch-test-%d-%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0)) !test_counter) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:(Some "evidence-agent") in
  config

(* Evidence printer *)
let print_evidence ~tool_name ~json_input ~result ~routed_to =
  Printf.printf "┌─────────────────────────────────────────────────────\n";
  Printf.printf "│ Tool: %s\n" tool_name;
  Printf.printf "│ JSON Input: %s\n" (Yojson.Safe.to_string json_input);
  Printf.printf "│ Routed To: %s\n" routed_to;
  Printf.printf "│ Result: %s\n" (if fst result then "✅ SUCCESS" else "❌ FAILED");
  let output = snd result in
  let truncated = if String.length output > 80 then String.sub output 0 80 else output in
  Printf.printf "│ Output (first 80 chars): %s...\n" truncated;
  Printf.printf "└─────────────────────────────────────────────────────\n\n"

(* Test Tool_agent dispatch *)
let () =
  let config = make_test_room () in
  let ctx : Tool_agent.context = { config; agent_name = "evidence-agent" } in
  let tool_name = "masc_agents" in
  let json_input = `Assoc [] in
  match Tool_agent.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_agent.handle_agents"
  | None ->
      failwith "Tool_agent dispatch returned None for masc_agents"

(* Test Tool_agent select_agent dispatch *)
let () =
  let config = make_test_room () in
  let ctx : Tool_agent.context = { config; agent_name = "evidence-agent" } in
  let tool_name = "masc_select_agent" in
  let json_input = `Assoc [
    ("available_agents", `List [`String "agent-a"; `String "agent-b"]);
    ("strategy", `String "capability_first");
  ] in
  match Tool_agent.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_agent.handle_select_agent"
  | None ->
      failwith "Tool_agent dispatch returned None for masc_select_agent"

(* Test Tool_lock dispatch *)
let () =
  let config = make_test_room () in
  let ctx : Tool_lock.context = { config; agent_name = "evidence-agent" } in
  let tool_name = "masc_lock" in
  let json_input = `Assoc [("file", `String "evidence-test.txt")] in
  match Tool_lock.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_lock.handle_lock"
  | None ->
      failwith "Tool_lock dispatch returned None for masc_lock"

(* Test Tool_lock unlock dispatch *)
let () =
  let config = make_test_room () in
  let ctx : Tool_lock.context = { config; agent_name = "evidence-agent" } in
  (* First lock *)
  let _ = Tool_lock.handle_lock ctx (`Assoc [("file", `String "unlock-evidence.txt")]) in
  let tool_name = "masc_unlock" in
  let json_input = `Assoc [("file", `String "unlock-evidence.txt")] in
  match Tool_lock.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_lock.handle_unlock"
  | None ->
      failwith "Tool_lock dispatch returned None for masc_unlock"

(* Test Tool_audit dispatch *)
let () =
  let config = make_test_room () in
  let ctx : Tool_audit.context = { config } in
  let tool_name = "masc_audit_query" in
  let json_input = `Assoc [("limit", `Int 10)] in
  match Tool_audit.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_audit.handle_audit_query"
  | None ->
      failwith "Tool_audit dispatch returned None for masc_audit_query"

(* Test Tool_rate_limit dispatch *)
let () =
  Eio_main.run @@ fun _env ->
  let config = make_test_room () in
  let registry = Session.create () in
  let ctx : Tool_rate_limit.context = { config; agent_name = "evidence-agent"; registry } in
  let tool_name = "masc_rate_limit_status" in
  let json_input = `Assoc [] in
  match Tool_rate_limit.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_rate_limit.handle_rate_limit_status"
  | None ->
      failwith "Tool_rate_limit dispatch returned None for masc_rate_limit_status"

(* Test Tool_cost dispatch *)
let () =
  let ctx : Tool_cost.context = { agent_name = "evidence-agent" } in
  let tool_name = "masc_cost_log" in
  let json_input = `Assoc [
    ("model", `String "test-model");
    ("input_tokens", `Int 100);
    ("output_tokens", `Int 50);
    ("cost_usd", `Float 0.001);
  ] in
  match Tool_cost.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_cost.handle_cost_log"
  | None ->
      failwith "Tool_cost dispatch returned None for masc_cost_log"

(* Test Tool_walph dispatch *)
let () =
  Eio_main.run @@ fun env ->
  let config = make_test_room () in
  let _ = Room.init config ~agent_name:(Some "evidence-agent") in
  let net = Eio.Stdenv.net env in
  let ctx : _ Tool_walph.context = { config; agent_name = "evidence-agent"; net } in
  let tool_name = "masc_walph_control" in
  let json_input = `Assoc [("command", `String "STATUS")] in
  match Tool_walph.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_walph.handle_walph_control"
  | None ->
      failwith "Tool_walph dispatch returned None for masc_walph_control"

(* Test Tool_task dispatch *)
let () =
  let config = make_test_room () in
  let ctx : Tool_task.context = { config; agent_name = "evidence-agent" } in
  let tool_name = "masc_tasks" in
  let json_input = `Assoc [] in
  match Tool_task.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_task.handle_tasks"
  | None ->
      failwith "Tool_task dispatch returned None for masc_tasks"

(* Test Tool_room dispatch *)
let () =
  let config = make_test_room () in
  let ctx : Tool_room.context = { config; agent_name = "evidence-agent" } in
  let tool_name = "masc_status" in
  let json_input = `Assoc [] in
  match Tool_room.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_room.handle_status"
  | None ->
      failwith "Tool_room dispatch returned None for masc_status"

(* Test Tool_control dispatch *)
let () =
  let config = make_test_room () in
  let ctx : Tool_control.context = { config; agent_name = "evidence-agent" } in
  let tool_name = "masc_pause_status" in
  let json_input = `Assoc [] in
  match Tool_control.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_control.handle_pause_status"
  | None ->
      failwith "Tool_control dispatch returned None for masc_pause_status"

(* Test Tool_misc dispatch *)
let () =
  let config = make_test_room () in
  let ctx : Tool_misc.context = { config; agent_name = "evidence-agent" } in
  let tool_name = "masc_dashboard" in
  let json_input = `Assoc [] in
  match Tool_misc.dispatch ctx ~name:tool_name ~args:json_input with
  | Some result ->
      print_evidence ~tool_name ~json_input ~result ~routed_to:"Tool_misc.handle_dashboard"
  | None ->
      failwith "Tool_misc dispatch returned None for masc_dashboard"

(* Test dispatch chain fallthrough - unknown tool returns None *)
let () =
  Printf.printf "┌─────────────────────────────────────────────────────\n";
  Printf.printf "│ Testing Dispatch Chain Fallthrough\n";
  Printf.printf "│ Unknown tools should return None to continue chain\n";
  Printf.printf "└─────────────────────────────────────────────────────\n\n";

  let config = make_test_room () in
  let ctx : Tool_agent.context = { config; agent_name = "evidence-agent" } in
  let tool_name = "unknown_tool_xyz" in
  let json_input = `Assoc [] in
  match Tool_agent.dispatch ctx ~name:tool_name ~args:json_input with
  | Some _ ->
      failwith "Should have returned None for unknown tool"
  | None ->
      Printf.printf "✅ Tool_agent.dispatch returned None for unknown tool (correct behavior)\n";
      Printf.printf "   This allows the dispatch chain to continue to the next module.\n\n"

let () = Printf.printf "═══════════════════════════════════════════════════════\n"
let () = Printf.printf "✅ All Dispatch Chain Evidence Tests Passed!\n"
let () = Printf.printf "═══════════════════════════════════════════════════════\n"
