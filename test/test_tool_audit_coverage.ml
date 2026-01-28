(** Coverage tests for Tool_audit *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_audit Coverage Tests ===\n"

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
    (Printf.sprintf "masc-audit-test-%d-%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) !test_counter) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  { Tool_audit.config }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_audit.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test audit_query dispatch *)
let () = test "dispatch_audit_query" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_audit.dispatch ctx ~name:"masc_audit_query" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_audit_query with filters *)
let () = test "handle_audit_query_with_filters" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [
    ("agent", `String "test-agent");
    ("event_type", `String "tool_call");
    ("limit", `Int 10);
    ("since_hours", `Float 12.0);
  ] in
  let (success, result) = Tool_audit.handle_audit_query ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test handle_audit_query empty *)
let () = test "handle_audit_query_empty" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_audit.handle_audit_query ctx args in
  assert success;
  (* Result should be JSON with empty events *)
  assert (String.length result > 0)
)

(* Test audit_stats dispatch *)
let () = test "dispatch_audit_stats" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_audit.dispatch ctx ~name:"masc_audit_stats" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_audit_stats *)
let () = test "handle_audit_stats" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_audit.handle_audit_stats ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test handle_audit_stats with agent filter *)
let () = test "handle_audit_stats_with_agent" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("agent", `String "test-agent")] in
  let (success, result) = Tool_audit.handle_audit_stats ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test audit_event_to_json *)
let () = test "audit_event_to_json" (fun () ->
  let event : Tool_audit.audit_event = {
    timestamp = 1234567890.0;
    agent = "test-agent";
    event_type = "tool_call";
    success = true;
    detail = Some "test detail";
  } in
  let json = Tool_audit.audit_event_to_json event in
  let open Yojson.Safe.Util in
  assert (json |> member "agent" |> to_string = "test-agent");
  assert (json |> member "event_type" |> to_string = "tool_call");
  assert (json |> member "success" |> to_bool = true)
)

(* Test audit_event_to_json without detail *)
let () = test "audit_event_to_json_no_detail" (fun () ->
  let event : Tool_audit.audit_event = {
    timestamp = 1234567890.0;
    agent = "test-agent";
    event_type = "auth_success";
    success = true;
    detail = None;
  } in
  let json = Tool_audit.audit_event_to_json event in
  let open Yojson.Safe.Util in
  assert (json |> member "detail" = `Null)
)

(* Test read_audit_events empty *)
let () = test "read_audit_events_empty" (fun () ->
  let ctx = make_test_ctx () in
  let events = Tool_audit.read_audit_events ctx.config ~since:0.0 in
  assert (events = [])
)

(* Test helper functions *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_audit.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_audit.get_string args "key" "default" = "default")
)

let () = test "get_string_opt_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_audit.get_string_opt args "key" = Some "value")
)

let () = test "get_string_opt_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_audit.get_string_opt args "key" = None)
)

let () = test "get_int_present" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_audit.get_int args "key" 0 = 42)
)

let () = test "get_int_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_audit.get_int args "key" 99 = 99)
)

let () = test "get_float_present" (fun () ->
  let args = `Assoc [("key", `Float 3.14)] in
  assert (Tool_audit.get_float args "key" 0.0 = 3.14)
)

let () = test "get_float_from_int" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_audit.get_float args "key" 0.0 = 42.0)
)

let () = test "get_float_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_audit.get_float args "key" 1.5 = 1.5)
)

let () = Printf.printf "\n✅ All Tool_audit tests passed!\n"
