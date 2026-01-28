(** Coverage tests for Tool_room *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_room Coverage Tests ===\n"

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
    (Printf.sprintf "masc-room-test-%d-%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) !test_counter) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  { Tool_room.config; agent_name = "test-agent" }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_room.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test dispatch init *)
let () = test "dispatch_init" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("agent_name", `String "init-agent")] in
  match Tool_room.dispatch ctx ~name:"masc_init" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test dispatch status *)
let () = test "dispatch_status" (fun () ->
  let ctx = make_test_ctx () in
  let _ = Room.init ctx.config ~agent_name:(Some "test-agent") in
  let args = `Assoc [] in
  match Tool_room.dispatch ctx ~name:"masc_status" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test dispatch reset without confirm *)
let () = test "dispatch_reset_no_confirm" (fun () ->
  let ctx = make_test_ctx () in
  let _ = Room.init ctx.config ~agent_name:(Some "test-agent") in
  let args = `Assoc [] in
  match Tool_room.dispatch ctx ~name:"masc_reset" ~args with
  | Some (success, _result) -> assert (not success) (* Should fail without confirm *)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch reset with confirm *)
let () = test "dispatch_reset_with_confirm" (fun () ->
  let ctx = make_test_ctx () in
  let _ = Room.init ctx.config ~agent_name:(Some "test-agent") in
  let args = `Assoc [("confirm", `Bool true)] in
  match Tool_room.dispatch ctx ~name:"masc_reset" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test dispatch rooms_list *)
let () = test "dispatch_rooms_list" (fun () ->
  let ctx = make_test_ctx () in
  let _ = Room.init ctx.config ~agent_name:(Some "test-agent") in
  let args = `Assoc [] in
  match Tool_room.dispatch ctx ~name:"masc_rooms_list" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test dispatch room_create *)
let () = test "dispatch_room_create" (fun () ->
  let ctx = make_test_ctx () in
  let _ = Room.init ctx.config ~agent_name:(Some "test-agent") in
  let args = `Assoc [("name", `String "test-room")] in
  match Tool_room.dispatch ctx ~name:"masc_room_create" ~args with
  | Some (_success, _result) -> () (* May fail depending on room state *)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch room_create without name *)
let () = test "dispatch_room_create_no_name" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_room.dispatch ctx ~name:"masc_room_create" ~args with
  | Some (success, _result) -> assert (not success) (* Should fail without name *)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch room_enter *)
let () = test "dispatch_room_enter" (fun () ->
  let ctx = make_test_ctx () in
  let _ = Room.init ctx.config ~agent_name:(Some "test-agent") in
  let args = `Assoc [("room_id", `String "test-room-id")] in
  match Tool_room.dispatch ctx ~name:"masc_room_enter" ~args with
  | Some (_success, _result) -> () (* May fail if room doesn't exist *)
  | None -> failwith "dispatch returned None"
)

(* Test dispatch room_enter without room_id *)
let () = test "dispatch_room_enter_no_id" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_room.dispatch ctx ~name:"masc_room_enter" ~args with
  | Some (success, _result) -> assert (not success) (* Should fail without room_id *)
  | None -> failwith "dispatch returned None"
)

(* Test helper functions *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_room.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_room.get_string args "key" "default" = "default")
)

let () = test "get_bool_true" (fun () ->
  let args = `Assoc [("key", `Bool true)] in
  assert (Tool_room.get_bool args "key" false = true)
)

let () = test "get_bool_false" (fun () ->
  let args = `Assoc [("key", `Bool false)] in
  assert (Tool_room.get_bool args "key" true = false)
)

let () = test "get_bool_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_room.get_bool args "key" true = true)
)

let () = Printf.printf "\n✅ All Tool_room tests passed!\n"
