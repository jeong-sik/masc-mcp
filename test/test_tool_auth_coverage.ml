(** Coverage tests for Tool_auth *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_auth Coverage Tests ===\n"

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
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-auth-test-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  { Tool_auth.config; agent_name = "test-agent" }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_auth.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test auth_status dispatch *)
let () = test "dispatch_auth_status" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_auth.dispatch ctx ~name:"masc_auth_status" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_auth_status *)
let () = test "handle_auth_status" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_auth.handle_auth_status ctx args in
  assert success;
  assert (String.length result > 0);
  assert (String.length result > 0 (* contains emoji *))
)

(* Test auth_enable dispatch *)
let () = test "dispatch_auth_enable" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_auth.dispatch ctx ~name:"masc_auth_enable" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_auth_enable *)
let () = test "handle_auth_enable" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("require_token", `Bool true)] in
  let (success, result) = Tool_auth.handle_auth_enable ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

(* Test auth_disable dispatch *)
let () = test "dispatch_auth_disable" (fun () ->
  let ctx = make_test_ctx () in
  let _ = Tool_auth.handle_auth_enable ctx (`Assoc []) in (* Enable first *)
  let args = `Assoc [] in
  match Tool_auth.dispatch ctx ~name:"masc_auth_disable" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_auth_disable *)
let () = test "handle_auth_disable" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_auth.handle_auth_disable ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

(* Test auth_list dispatch *)
let () = test "dispatch_auth_list" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_auth.dispatch ctx ~name:"masc_auth_list" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_auth_list empty *)
let () = test "handle_auth_list_empty" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_auth.handle_auth_list ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test auth_revoke dispatch *)
let () = test "dispatch_auth_revoke" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_auth.dispatch ctx ~name:"masc_auth_revoke" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test get_string helper *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_auth.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_auth.get_string args "key" "default" = "default")
)

(* Test get_bool helper *)
let () = test "get_bool_true" (fun () ->
  let args = `Assoc [("key", `Bool true)] in
  assert (Tool_auth.get_bool args "key" false = true)
)

let () = test "get_bool_false" (fun () ->
  let args = `Assoc [("key", `Bool false)] in
  assert (Tool_auth.get_bool args "key" true = false)
)

let () = test "get_bool_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_auth.get_bool args "key" true = true)
)

let () = Printf.printf "\n✅ All Tool_auth tests passed!\n"
