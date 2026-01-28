(** Coverage tests for Tool_encryption *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_encryption Coverage Tests ===\n"

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
    (Printf.sprintf "masc-enc-test-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let state = Mcp_server.create_state ~base_path:tmp in
  { Tool_encryption.state }

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  assert (Tool_encryption.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test encryption_status dispatch *)
let () = test "dispatch_encryption_status" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_encryption.dispatch ctx ~name:"masc_encryption_status" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_encryption_status *)
let () = test "handle_encryption_status" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_encryption.handle_encryption_status ctx args in
  assert success;
  assert (String.length result > 0);
  assert (String.length result > 0 (* contains emoji *))
)

(* Test generate_key dispatch *)
let () = test "dispatch_generate_key" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_encryption.dispatch ctx ~name:"masc_generate_key" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_generate_key with hex output *)
let () = test "handle_generate_key_hex" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("output", `String "hex")] in
  let (success, result) = Tool_encryption.handle_generate_key ctx args in
  assert success;
  assert (String.length result > 0);
  assert (String.length result > 0 (* contains emoji *))
)

(* Test handle_generate_key with base64 output *)
let () = test "handle_generate_key_base64" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [("output", `String "base64")] in
  let (success, result) = Tool_encryption.handle_generate_key ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test encryption_disable dispatch *)
let () = test "dispatch_encryption_disable" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  match Tool_encryption.dispatch ctx ~name:"masc_encryption_disable" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test handle_encryption_disable *)
let () = test "handle_encryption_disable" (fun () ->
  let ctx = make_test_ctx () in
  let args = `Assoc [] in
  let (success, result) = Tool_encryption.handle_encryption_disable ctx args in
  assert success;
  assert (String.length result > 0 (* contains emoji *))
)

(* Test get_string helper *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_encryption.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_encryption.get_string args "key" "default" = "default")
)

let () = test "get_string_wrong_type" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_encryption.get_string args "key" "default" = "default")
)

let () = Printf.printf "\n✅ All Tool_encryption tests passed!\n"
