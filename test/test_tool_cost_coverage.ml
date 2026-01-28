(** Coverage tests for Tool_cost *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_cost Coverage Tests ===\n"

(* Test helper *)
let test name f =
  try
    f ();
    Printf.printf "✓ %s passed\n" name
  with e ->
    Printf.printf "✗ %s FAILED: %s\n" name (Printexc.to_string e);
    exit 1

(* Create test context *)
let make_test_ctx () =
  { Tool_cost.agent_name = "test-agent" }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_cost.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test cost_log dispatch - will fail if masc-cost CLI not installed, but tests dispatch *)
let () = test "dispatch_cost_log" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [
    ("model", `String "test-model");
    ("input_tokens", `Int 100);
    ("output_tokens", `Int 50);
    ("cost_usd", `Float 0.001);
  ] in
  match Tool_cost.dispatch ctx ~name:"masc_cost_log" ~args with
  | Some (_success, _result) -> () (* May fail if CLI not installed, but dispatch works *)
  | None -> failwith "dispatch returned None"
)

(* Test cost_report dispatch *)
let () = test "dispatch_cost_report" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [
    ("period", `String "daily");
  ] in
  match Tool_cost.dispatch ctx ~name:"masc_cost_report" ~args with
  | Some (_success, _result) -> () (* May fail if CLI not installed, but dispatch works *)
  | None -> failwith "dispatch returned None"
)

(* Test handle_cost_log with task_id *)
let () = test "handle_cost_log_with_task" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [
    ("model", `String "gpt-4");
    ("input_tokens", `Int 1000);
    ("output_tokens", `Int 500);
    ("cost_usd", `Float 0.05);
    ("task_id", `String "task-001");
  ] in
  let (_success, _result) = Tool_cost.handle_cost_log ctx args in
  () (* Result depends on CLI availability *)
)

(* Test handle_cost_report with filters *)
let () = test "handle_cost_report_with_filters" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [
    ("period", `String "weekly");
    ("agent", `String "test-agent");
    ("task_id", `String "task-001");
  ] in
  let (_success, _result) = Tool_cost.handle_cost_report ctx args in
  () (* Result depends on CLI availability *)
)

(* Test helper functions *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_cost.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_cost.get_string args "key" "default" = "default")
)

let () = test "get_int_present" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_cost.get_int args "key" 0 = 42)
)

let () = test "get_int_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_cost.get_int args "key" 99 = 99)
)

let () = test "get_float_present" (fun () ->
  let args = `Assoc [("key", `Float 3.14)] in
  assert (Tool_cost.get_float args "key" 0.0 = 3.14)
)

let () = test "get_float_from_int" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_cost.get_float args "key" 0.0 = 42.0)
)

let () = test "get_float_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_cost.get_float args "key" 1.5 = 1.5)
)

let () = Printf.printf "\n✅ All Tool_cost tests passed!\n"
