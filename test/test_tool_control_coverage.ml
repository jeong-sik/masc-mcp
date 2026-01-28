(** Coverage tests for Tool_control *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_control Coverage Tests ===\n"

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
    (Printf.sprintf "masc-control-test-%d-%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) !test_counter) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:(Some "test-agent") in
  { Tool_control.config; agent_name = "test-agent" }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_control.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test dispatch pause *)
let () = test "dispatch_pause" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("reason", `String "Test pause")] in
  match Tool_control.dispatch ctx ~name:"masc_pause" ~args with
  | Some (success, result) ->
      assert success;
      assert (String.length result > 0)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch pause_status when paused *)
let () = test "dispatch_pause_status_paused" (fun () ->
  let ctx = make_test_ctx () in
  (* First pause the room *)
  let _ = Tool_control.handle_pause ctx (`Assoc [("reason", `String "For status test")]) in
  let args = `Assoc [] in
  match Tool_control.dispatch ctx ~name:"masc_pause_status" ~args with
  | Some (success, result) ->
      assert success;
      assert (String.length result > 0)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch resume *)
let () = test "dispatch_resume" (fun () ->
  let ctx = make_test_ctx () in
  (* First pause the room *)
  let _ = Tool_control.handle_pause ctx (`Assoc [("reason", `String "For resume test")]) in
  let args = `Assoc [] in
  match Tool_control.dispatch ctx ~name:"masc_resume" ~args with
  | Some (success, result) ->
      assert success;
      assert (String.length result > 0)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch pause_status when not paused *)
let () = test "dispatch_pause_status_running" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_control.dispatch ctx ~name:"masc_pause_status" ~args with
  | Some (success, result) ->
      assert success;
      assert (String.length result > 0)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch switch_mode *)
let () = test "dispatch_switch_mode" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("mode", `String "collaborative")] in
  match Tool_control.dispatch ctx ~name:"masc_switch_mode" ~args with
  | Some (success, result) ->
      assert success;
      assert (String.length result > 0)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch get_config *)
let () = test "dispatch_get_config" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_control.dispatch ctx ~name:"masc_get_config" ~args with
  | Some (success, result) ->
      assert success;
      assert (String.length result > 0)
  | None -> failwith "dispatch returned None"
)

(* Test helper functions *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_control.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_control.get_string args "key" "default" = "default")
)

let () = Printf.printf "\n✅ All Tool_control tests passed!\n"
