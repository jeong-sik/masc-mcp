(** Coverage tests for Tool_walph *)

open Masc_mcp

let () = Printf.printf "\n=== Tool_walph Coverage Tests ===\n"

(* Test helper *)
let test name f =
  try
    f ();
    Printf.printf "✓ %s passed\n" name
  with e ->
    Printf.printf "✗ %s FAILED: %s\n" name (Printexc.to_string e);
    exit 1

(* Test dispatch returns None for unknown tool *)
let () = test "dispatch_unknown_tool" (fun () ->
  Eio_main.run @@ fun env ->
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-walph-test-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let net = Eio.Stdenv.net env in
  let ctx = { Tool_walph.config; agent_name = "test-agent"; net } in
  let args = `Assoc [] in
  assert (Tool_walph.dispatch ctx ~name:"unknown_tool" ~args = None)
)

(* Test walph_control dispatch *)
let () = test "dispatch_walph_control" (fun () ->
  Eio_main.run @@ fun env ->
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-walph-test2-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:None in
  let net = Eio.Stdenv.net env in
  let ctx = { Tool_walph.config; agent_name = "test-agent"; net } in
  let args = `Assoc [("command", `String "STATUS")] in
  match Tool_walph.dispatch ctx ~name:"masc_walph_control" ~args with
  | Some (success, _result) -> assert success
  | None -> failwith "dispatch returned None"
)

(* Test walph_natural with stop command *)
let () = test "walph_natural_stop" (fun () ->
  Eio_main.run @@ fun env ->
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-walph-test3-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:None in
  let net = Eio.Stdenv.net env in
  let ctx = { Tool_walph.config; agent_name = "test-agent"; net } in
  let args = `Assoc [("message", `String "stop the loop")] in
  let (success, _result) = Tool_walph.handle_walph_natural ctx args in
  assert success
)

(* Test walph_natural with pause command *)
let () = test "walph_natural_pause" (fun () ->
  Eio_main.run @@ fun env ->
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-walph-test4-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:None in
  let net = Eio.Stdenv.net env in
  let ctx = { Tool_walph.config; agent_name = "test-agent"; net } in
  let args = `Assoc [("message", `String "잠깐 일시정지")] in
  let (success, _result) = Tool_walph.handle_walph_natural ctx args in
  assert success
)

(* Test walph_natural with resume command *)
let () = test "walph_natural_resume" (fun () ->
  Eio_main.run @@ fun env ->
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-walph-test5-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:None in
  let net = Eio.Stdenv.net env in
  let ctx = { Tool_walph.config; agent_name = "test-agent"; net } in
  let args = `Assoc [("message", `String "계속 진행해")] in
  let (success, _result) = Tool_walph.handle_walph_natural ctx args in
  assert success
)

(* Test walph_natural with status command *)
let () = test "walph_natural_status" (fun () ->
  Eio_main.run @@ fun env ->
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-walph-test6-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let _ = Room.init config ~agent_name:None in
  let net = Eio.Stdenv.net env in
  let ctx = { Tool_walph.config; agent_name = "test-agent"; net } in
  let args = `Assoc [("message", `String "뭐해? 상태 알려줘")] in
  let (success, _result) = Tool_walph.handle_walph_natural ctx args in
  assert success
)

(* Test walph_natural with unrecognized message *)
let () = test "walph_natural_ignore" (fun () ->
  Eio_main.run @@ fun env ->
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-walph-test7-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let net = Eio.Stdenv.net env in
  let ctx = { Tool_walph.config; agent_name = "test-agent"; net } in
  let args = `Assoc [("message", `String "random gibberish xyz")] in
  let (success, result) = Tool_walph.handle_walph_natural ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test walph_natural with empty message *)
let () = test "walph_natural_empty" (fun () ->
  Eio_main.run @@ fun env ->
  let tmp = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc-walph-test8-%d" (int_of_float (Unix.gettimeofday () *. 1000000.0))) in
  Unix.mkdir tmp 0o755;
  let config = Room.default_config tmp in
  let net = Eio.Stdenv.net env in
  let ctx = { Tool_walph.config; agent_name = "test-agent"; net } in
  let args = `Assoc [("message", `String "")] in
  let (success, result) = Tool_walph.handle_walph_natural ctx args in
  assert success;
  assert (String.length result > 0)
)

(* Test helper functions *)
let () = test "get_string_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_walph.get_string args "key" "default" = "value")
)

let () = test "get_string_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_walph.get_string args "key" "default" = "default")
)

let () = test "get_string_opt_present" (fun () ->
  let args = `Assoc [("key", `String "value")] in
  assert (Tool_walph.get_string_opt args "key" = Some "value")
)

let () = test "get_string_opt_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_walph.get_string_opt args "key" = None)
)

let () = test "get_int_present" (fun () ->
  let args = `Assoc [("key", `Int 42)] in
  assert (Tool_walph.get_int args "key" 0 = 42)
)

let () = test "get_int_missing" (fun () ->
  let args = `Assoc [] in
  assert (Tool_walph.get_int args "key" 99 = 99)
)

let () = Printf.printf "\n✅ All Tool_walph tests passed!\n"
