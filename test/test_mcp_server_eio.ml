(** Test suite for Mcp_server_eio module

    Tests the Eio-native MCP server implementation.
    Uses Eio_main.run for async test context.
*)

module Mcp_eio = Masc_mcp.Mcp_server_eio
module Mcp = Masc_mcp.Mcp_server

(* ===== Test Helpers ===== *)

let temp_dir () =
  let dir = Filename.temp_file "test_mcp_eio_" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  dir

let cleanup_dir dir =
  let rec rm path =
    if Sys.is_directory path then begin
      Array.iter (fun name -> rm (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Unix.unlink path
  in
  try rm dir with _ -> ()

(* ===== Unit Tests for Type Re-exports ===== *)

let test_create_state () =
  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in
  Alcotest.(check string) "base_path preserved"
    base_path state.room_config.base_path;
  cleanup_dir base_path

let test_type_compatibility () =
  (* Verify Mcp_server_eio.server_state is same type as Mcp_server.server_state *)
  let base_path = temp_dir () in
  let state : Mcp_eio.server_state = Mcp_eio.create_state ~test_mode:true ~base_path () in
  let _state2 : Mcp.server_state = state in  (* Type unification *)
  cleanup_dir base_path;
  Alcotest.(check pass) "types are compatible" () ()

(* ===== Unit Tests for Protocol Helpers ===== *)

let test_is_jsonrpc_v2 () =
  let valid = `Assoc [("jsonrpc", `String "2.0"); ("method", `String "test")] in
  let invalid = `Assoc [("jsonrpc", `String "1.0")] in
  let no_version = `Assoc [("method", `String "test")] in
  Alcotest.(check bool) "valid 2.0" true (Mcp_eio.is_jsonrpc_v2 valid);
  Alcotest.(check bool) "invalid 1.0" false (Mcp_eio.is_jsonrpc_v2 invalid);
  Alcotest.(check bool) "no version" false (Mcp_eio.is_jsonrpc_v2 no_version)

let test_jsonrpc_request_parsing () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "initialize");
    ("params", `Assoc []);
  ] in
  match Mcp_eio.jsonrpc_request_of_yojson json with
  | Ok req ->
      Alcotest.(check string) "method" "initialize" req.method_;
      Alcotest.(check bool) "has id" true (req.id <> None)
  | Error msg ->
      Alcotest.fail ("Parse failed: " ^ msg)

let test_is_notification () =
  let with_id = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "test");
  ] in
  let without_id = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "notifications/initialized");
  ] in
  (match Mcp_eio.jsonrpc_request_of_yojson with_id with
   | Ok req -> Alcotest.(check bool) "with id" false (Mcp_eio.is_notification req)
   | Error _ -> Alcotest.fail "parse error");
  (match Mcp_eio.jsonrpc_request_of_yojson without_id with
   | Ok req -> Alcotest.(check bool) "without id" true (Mcp_eio.is_notification req)
   | Error _ -> Alcotest.fail "parse error")

let test_protocol_version () =
  let params = Some (`Assoc [("protocolVersion", `String "2025-03-26")]) in
  let version = Mcp_eio.protocol_version_from_params params in
  Alcotest.(check string) "version extracted" "2025-03-26" version;

  let normalized = Mcp_eio.normalize_protocol_version "unknown" in
  Alcotest.(check string) "normalized to default" "2025-11-25" normalized

(* ===== Unit Tests for Response Builders ===== *)

let test_make_response () =
  let response = Mcp_eio.make_response ~id:(`Int 42) (`String "result") in
  match response with
  | `Assoc fields ->
      let id = List.assoc "id" fields in
      let result = List.assoc "result" fields in
      Alcotest.(check bool) "has jsonrpc" true (List.mem_assoc "jsonrpc" fields);
      Alcotest.(check bool) "id is 42" true (id = `Int 42);
      Alcotest.(check bool) "result is string" true (result = `String "result")
  | _ -> Alcotest.fail "not an object"

let test_make_error () =
  let response = Mcp_eio.make_error ~id:(`Int 1) (-32600) "Invalid Request" in
  match response with
  | `Assoc fields ->
      let error = List.assoc "error" fields in
      (match error with
       | `Assoc error_fields ->
           let code = List.assoc "code" error_fields in
           Alcotest.(check bool) "error code" true (code = `Int (-32600))
       | _ -> Alcotest.fail "error not an object")
  | _ -> Alcotest.fail "not an object"

(* ===== Eio Integration Tests ===== *)

let test_handle_request_initialize () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "initialize");
    ("params", `Assoc [
      ("protocolVersion", `String "2025-11-25");
      ("capabilities", `Assoc []);
      ("clientInfo", `Assoc [
        ("name", `String "test");
        ("version", `String "1.0");
      ]);
    ]);
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has result" true (List.mem_assoc "result" fields);
       (match List.assoc_opt "result" fields with
        | Some (`Assoc result_fields) ->
            Alcotest.(check bool) "has serverInfo" true
              (List.mem_assoc "serverInfo" result_fields);
            Alcotest.(check bool) "has capabilities" true
              (List.mem_assoc "capabilities" result_fields)
        | _ -> Alcotest.fail "result not an object")
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

let test_handle_request_tools_list () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 2);
    ("method", `String "tools/list");
    ("params", `Assoc []);
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has result" true (List.mem_assoc "result" fields);
       (match List.assoc_opt "result" fields with
        | Some (`Assoc result_fields) ->
            Alcotest.(check bool) "has tools" true
              (List.mem_assoc "tools" result_fields)
        | _ -> Alcotest.fail "result not an object")
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

let test_handle_request_invalid_json () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let response = Mcp_eio.handle_request ~clock ~sw state "not valid json {{{" in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has error" true (List.mem_assoc "error" fields)
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

let test_handle_request_method_not_found () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 99);
    ("method", `String "unknown/method");
    ("params", `Assoc []);
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has error" true (List.mem_assoc "error" fields);
       (match List.assoc_opt "error" fields with
        | Some (`Assoc error_fields) ->
            let code = List.assoc "code" error_fields in
            Alcotest.(check bool) "error code -32601" true (code = `Int (-32601))
        | _ -> Alcotest.fail "error not an object")
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

(* ===== Test Suites ===== *)

let state_tests = [
  "create_state", `Quick, test_create_state;
  "type compatibility", `Quick, test_type_compatibility;
]

let protocol_tests = [
  "is_jsonrpc_v2", `Quick, test_is_jsonrpc_v2;
  "jsonrpc_request parsing", `Quick, test_jsonrpc_request_parsing;
  "is_notification", `Quick, test_is_notification;
  "protocol version", `Quick, test_protocol_version;
]

let response_tests = [
  "make_response", `Quick, test_make_response;
  "make_error", `Quick, test_make_error;
]

let eio_tests = [
  "handle initialize", `Quick, test_handle_request_initialize;
  "handle tools/list", `Quick, test_handle_request_tools_list;
  "handle invalid json", `Quick, test_handle_request_invalid_json;
  "handle method not found", `Quick, test_handle_request_method_not_found;
]

let () =
  Alcotest.run "Mcp_server_eio" [
    "state", state_tests;
    "protocol", protocol_tests;
    "response", response_tests;
    "eio", eio_tests;
  ]
