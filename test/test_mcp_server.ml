(** Tests for MCP Server SSE broadcast functionality *)

open Masc_mcp
open Lwt.Syntax

(* Test that SSE callback is called on broadcast *)
let test_sse_callback_called () =
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc_test_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir tmp_dir 0o755;

  let state = Mcp_server.create_state ~base_path:tmp_dir in

  (* Track callback invocations *)
  let callback_called = ref false in
  let received_json = ref `Null in

  (* Register SSE callback *)
  Mcp_server.set_sse_callback state (fun json ->
    callback_called := true;
    received_json := json
  );

  (* Initialize room first *)
  let _ = Room.init (state.room_config) ~agent_name:(Some "test_agent") in

  (* Execute broadcast via MCP *)
  let args = `Assoc [
    ("agent_name", `String "test_agent");
    ("message", `String "Hello @world!");
    ("format", `String "verbose");
  ] in

  Lwt_main.run (
    let* (_success, _result) = Mcp_server.execute_tool state ~name:"masc_broadcast" ~arguments:args in
    Lwt.return_unit
  );

  (* Verify callback was called *)
  Alcotest.(check bool) "callback called" true !callback_called;

  (* Verify JSON structure *)
  let open Yojson.Safe.Util in
  let json = !received_json in
  Alcotest.(check string) "type" "masc/broadcast" (json |> member "type" |> to_string);
  Alcotest.(check string) "from" "test_agent" (json |> member "from" |> to_string);
  Alcotest.(check string) "content" "Hello @world!" (json |> member "content" |> to_string);
  Alcotest.(check string) "mention" "world" (json |> member "mention" |> to_string);

  (* Cleanup *)
  let _ = Room.reset (state.room_config) in
  Unix.rmdir tmp_dir

(* Test that SSE callback is not called when not registered *)
let test_sse_no_callback () =
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc_test_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir tmp_dir 0o755;

  let state = Mcp_server.create_state ~base_path:tmp_dir in

  (* Don't register callback - should not crash *)
  let _ = Room.init (state.room_config) ~agent_name:(Some "test_agent") in

  let args = `Assoc [
    ("agent_name", `String "test_agent");
    ("message", `String "No callback test");
    ("format", `String "verbose");
  ] in

  (* Should not crash even without callback *)
  Lwt_main.run (
    let* (_success, _result) = Mcp_server.execute_tool state ~name:"masc_broadcast" ~arguments:args in
    Lwt.return_unit
  );

  (* Cleanup *)
  let _ = Room.reset (state.room_config) in
  Unix.rmdir tmp_dir

(* Test mention parsing in broadcast *)
let test_mention_parsing () =
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc_test_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir tmp_dir 0o755;

  let state = Mcp_server.create_state ~base_path:tmp_dir in

  let received_jsons = ref [] in

  Mcp_server.set_sse_callback state (fun json ->
    received_jsons := json :: !received_jsons
  );

  let _ = Room.init (state.room_config) ~agent_name:(Some "claude") in

  (* Test with mention *)
  let args1 = `Assoc [
    ("agent_name", `String "claude");
    ("message", `String "@codex please review");
    ("format", `String "verbose");
  ] in

  Lwt_main.run (
    let* _ = Mcp_server.execute_tool state ~name:"masc_broadcast" ~arguments:args1 in
    Lwt.return_unit
  );

  (* Test without mention *)
  let args2 = `Assoc [
    ("agent_name", `String "claude");
    ("message", `String "General broadcast");
    ("format", `String "verbose");
  ] in

  Lwt_main.run (
    let* _ = Mcp_server.execute_tool state ~name:"masc_broadcast" ~arguments:args2 in
    Lwt.return_unit
  );

  Alcotest.(check int) "two broadcasts" 2 (List.length !received_jsons);

  let open Yojson.Safe.Util in
  let json1 = List.nth (List.rev !received_jsons) 0 in
  let json2 = List.nth (List.rev !received_jsons) 1 in

  Alcotest.(check string) "first mention" "codex" (json1 |> member "mention" |> to_string);
  Alcotest.(check bool) "second no mention" true (json2 |> member "mention" = `Null);

  (* Cleanup *)
  let _ = Room.reset (state.room_config) in
  Unix.rmdir tmp_dir

let () =
  Alcotest.run "MCP Server" [
    "sse_broadcast", [
      Alcotest.test_case "callback called on broadcast" `Quick test_sse_callback_called;
      Alcotest.test_case "no crash without callback" `Quick test_sse_no_callback;
      Alcotest.test_case "mention parsing" `Quick test_mention_parsing;
    ];
  ]
