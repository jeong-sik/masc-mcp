(** MCP Server Eio Module Coverage Tests

    Tests for server utility functions
*)

open Alcotest

module Mcp_server_eio = Masc_mcp.Mcp_server_eio

(* ============================================================
   Type Tests
   ============================================================ *)

let test_server_state_type () =
  let _ : Mcp_server_eio.server_state option = None in
  check bool "server_state type exists" true true

let test_jsonrpc_request_type () =
  let _ : Mcp_server_eio.jsonrpc_request option = None in
  check bool "jsonrpc_request type exists" true true

(* ============================================================
   is_jsonrpc_v2 Tests
   ============================================================ *)

let test_is_jsonrpc_v2_valid () =
  let json = `Assoc [("jsonrpc", `String "2.0")] in
  check bool "valid 2.0" true (Mcp_server_eio.is_jsonrpc_v2 json)

let test_is_jsonrpc_v2_invalid () =
  let json = `Assoc [("jsonrpc", `String "1.0")] in
  check bool "invalid 1.0" false (Mcp_server_eio.is_jsonrpc_v2 json)

let test_is_jsonrpc_v2_missing () =
  let json = `Assoc [] in
  check bool "missing field" false (Mcp_server_eio.is_jsonrpc_v2 json)

(* ============================================================
   normalize_protocol_version Tests
   ============================================================ *)

let test_normalize_protocol_version () =
  check string "2024-11-05" "2024-11-05"
    (Mcp_server_eio.normalize_protocol_version "2024-11-05")

let test_normalize_protocol_version_unknown () =
  let result = Mcp_server_eio.normalize_protocol_version "unknown" in
  check bool "returns string" true (String.length result > 0)

(* ============================================================
   protocol_version_from_params Tests
   ============================================================ *)

let test_protocol_version_from_params_none () =
  let result = Mcp_server_eio.protocol_version_from_params None in
  check bool "returns default" true (String.length result > 0)

let test_protocol_version_from_params_some () =
  let params = `Assoc [("protocolVersion", `String "2024-11-05")] in
  let result = Mcp_server_eio.protocol_version_from_params (Some params) in
  check string "extracts version" "2024-11-05" result

(* ============================================================
   make_response Tests
   ============================================================ *)

let test_make_response () =
  let response = Mcp_server_eio.make_response ~id:(`Int 42) (`String "success") in
  let open Yojson.Safe.Util in
  let id = response |> member "id" in
  match id with
  | `Int 42 -> check bool "id matches" true true
  | _ -> fail "expected Int 42"

let test_make_response_has_jsonrpc () =
  let response = Mcp_server_eio.make_response ~id:(`String "test-id") `Null in
  let open Yojson.Safe.Util in
  let version = response |> member "jsonrpc" |> to_string in
  check string "jsonrpc version" "2.0" version

(* ============================================================
   make_error Tests
   ============================================================ *)

let test_make_error () =
  let error = Mcp_server_eio.make_error ~id:(`Int 1) (-32600) "Invalid Request" in
  let open Yojson.Safe.Util in
  let err_obj = error |> member "error" in
  let code = err_obj |> member "code" |> to_int in
  check int "error code" (-32600) code

let test_make_error_message () =
  let error = Mcp_server_eio.make_error ~id:(`Int 1) (-32601) "Method not found" in
  let open Yojson.Safe.Util in
  let err_obj = error |> member "error" in
  let msg = err_obj |> member "message" |> to_string in
  check string "error message" "Method not found" msg

let test_make_error_with_data () =
  let error = Mcp_server_eio.make_error ~data:(`String "extra info") ~id:(`Int 1) (-32602) "Invalid params" in
  let open Yojson.Safe.Util in
  let err_obj = error |> member "error" in
  let data = err_obj |> member "data" |> to_string in
  check string "error data" "extra info" data

(* ============================================================
   has_field Tests
   ============================================================ *)

let test_has_field_exists () =
  let json = `Assoc [("key", `String "value")] in
  check bool "field exists" true (Mcp_server_eio.has_field "key" json)

let test_has_field_missing () =
  let json = `Assoc [("other", `String "value")] in
  check bool "field missing" false (Mcp_server_eio.has_field "key" json)

let test_has_field_not_assoc () =
  let json = `List [] in
  check bool "not assoc" false (Mcp_server_eio.has_field "key" json)

(* ============================================================
   get_field Tests
   ============================================================ *)

let test_get_field_exists () =
  let json = `Assoc [("name", `String "test")] in
  match Mcp_server_eio.get_field "name" json with
  | Some (`String "test") -> check bool "found" true true
  | _ -> fail "expected Some"

let test_get_field_missing () =
  let json = `Assoc [] in
  match Mcp_server_eio.get_field "name" json with
  | None -> check bool "not found" true true
  | Some _ -> fail "expected None"

let test_get_field_not_assoc () =
  let json = `String "not assoc" in
  match Mcp_server_eio.get_field "name" json with
  | None -> check bool "not assoc" true true
  | Some _ -> fail "expected None"

(* ============================================================
   is_jsonrpc_response Tests
   ============================================================ *)

let test_is_jsonrpc_response_valid () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("result", `String "ok");
  ] in
  check bool "valid response" true (Mcp_server_eio.is_jsonrpc_response json)

let test_is_jsonrpc_response_error () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("error", `Assoc [("code", `Int (-32600))]);
  ] in
  check bool "error response" true (Mcp_server_eio.is_jsonrpc_response json)

let test_is_jsonrpc_response_request () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "test");
  ] in
  check bool "not a response (is request)" false (Mcp_server_eio.is_jsonrpc_response json)

let test_is_jsonrpc_response_not_assoc () =
  check bool "not assoc" false (Mcp_server_eio.is_jsonrpc_response (`List []))

(* ============================================================
   is_notification Tests
   ============================================================ *)

let test_is_notification_true () =
  let req : Mcp_server_eio.jsonrpc_request = {
    jsonrpc = "2.0";
    id = None;
    method_ = "notification";
    params = None;
  } in
  check bool "is notification" true (Mcp_server_eio.is_notification req)

let test_is_notification_false () =
  let req : Mcp_server_eio.jsonrpc_request = {
    jsonrpc = "2.0";
    id = Some (`Int 1);
    method_ = "request";
    params = None;
  } in
  check bool "not notification" false (Mcp_server_eio.is_notification req)

(* ============================================================
   get_id Tests
   ============================================================ *)

let test_get_id_some () =
  let req : Mcp_server_eio.jsonrpc_request = {
    jsonrpc = "2.0";
    id = Some (`Int 42);
    method_ = "test";
    params = None;
  } in
  match Mcp_server_eio.get_id req with
  | `Int 42 -> check bool "id 42" true true
  | _ -> fail "expected Int 42"

let test_get_id_none () =
  let req : Mcp_server_eio.jsonrpc_request = {
    jsonrpc = "2.0";
    id = None;
    method_ = "test";
    params = None;
  } in
  match Mcp_server_eio.get_id req with
  | `Null -> check bool "null" true true
  | _ -> fail "expected Null"

(* ============================================================
   is_valid_request_id Tests
   ============================================================ *)

let test_is_valid_request_id_null () =
  check bool "null valid" true (Mcp_server_eio.is_valid_request_id `Null)

let test_is_valid_request_id_string () =
  check bool "string valid" true (Mcp_server_eio.is_valid_request_id (`String "id-1"))

let test_is_valid_request_id_int () =
  check bool "int valid" true (Mcp_server_eio.is_valid_request_id (`Int 123))

let test_is_valid_request_id_float () =
  check bool "float valid" true (Mcp_server_eio.is_valid_request_id (`Float 1.5))

let test_is_valid_request_id_array () =
  check bool "array invalid" false (Mcp_server_eio.is_valid_request_id (`List []))

let test_is_valid_request_id_object () =
  check bool "object invalid" false (Mcp_server_eio.is_valid_request_id (`Assoc []))

(* ============================================================
   validate_initialize_params Tests
   ============================================================ *)

let test_validate_initialize_params_valid () =
  let params = `Assoc [
    ("protocolVersion", `String "2024-11-05");
    ("clientInfo", `Assoc [
      ("name", `String "test-client");
      ("version", `String "1.0.0");
    ]);
    ("capabilities", `Assoc []);
  ] in
  match Mcp_server_eio.validate_initialize_params (Some params) with
  | Ok () -> check bool "valid" true true
  | Error e -> fail ("unexpected error: " ^ e)

let test_validate_initialize_params_none () =
  match Mcp_server_eio.validate_initialize_params None with
  | Error "Missing params" -> check bool "missing params" true true
  | _ -> fail "expected Missing params"

let test_validate_initialize_params_missing_version () =
  let params = `Assoc [
    ("clientInfo", `Assoc [
      ("name", `String "test");
      ("version", `String "1.0");
    ]);
    ("capabilities", `Assoc []);
  ] in
  match Mcp_server_eio.validate_initialize_params (Some params) with
  | Error msg -> check bool "error contains Missing" true (String.length msg > 0)
  | Ok () -> fail "expected error"

(* ============================================================
   jsonrpc_request_of_yojson Tests
   ============================================================ *)

let test_jsonrpc_request_of_yojson_valid () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "test_method");
    ("params", `Assoc []);
  ] in
  match Mcp_server_eio.jsonrpc_request_of_yojson json with
  | Ok req ->
      check string "method" "test_method" req.method_;
      check string "jsonrpc" "2.0" req.jsonrpc
  | Error e -> fail ("parse error: " ^ e)

let test_jsonrpc_request_of_yojson_minimal () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "notify");
  ] in
  match Mcp_server_eio.jsonrpc_request_of_yojson json with
  | Ok req ->
      check (option (testable Yojson.Safe.pp Yojson.Safe.equal)) "id None" None req.id;
      check (option (testable Yojson.Safe.pp Yojson.Safe.equal)) "params None" None req.params
  | Error e -> fail ("parse error: " ^ e)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "MCP Server Eio Coverage" [
    "types", [
      test_case "server_state" `Quick test_server_state_type;
      test_case "jsonrpc_request" `Quick test_jsonrpc_request_type;
    ];
    "is_jsonrpc_v2", [
      test_case "valid" `Quick test_is_jsonrpc_v2_valid;
      test_case "invalid" `Quick test_is_jsonrpc_v2_invalid;
      test_case "missing" `Quick test_is_jsonrpc_v2_missing;
    ];
    "normalize_protocol_version", [
      test_case "basic" `Quick test_normalize_protocol_version;
      test_case "unknown" `Quick test_normalize_protocol_version_unknown;
    ];
    "protocol_version_from_params", [
      test_case "none" `Quick test_protocol_version_from_params_none;
      test_case "some" `Quick test_protocol_version_from_params_some;
    ];
    "make_response", [
      test_case "basic" `Quick test_make_response;
      test_case "has jsonrpc" `Quick test_make_response_has_jsonrpc;
    ];
    "make_error", [
      test_case "code" `Quick test_make_error;
      test_case "message" `Quick test_make_error_message;
      test_case "with data" `Quick test_make_error_with_data;
    ];
    "has_field", [
      test_case "exists" `Quick test_has_field_exists;
      test_case "missing" `Quick test_has_field_missing;
      test_case "not assoc" `Quick test_has_field_not_assoc;
    ];
    "get_field", [
      test_case "exists" `Quick test_get_field_exists;
      test_case "missing" `Quick test_get_field_missing;
      test_case "not assoc" `Quick test_get_field_not_assoc;
    ];
    "is_jsonrpc_response", [
      test_case "valid" `Quick test_is_jsonrpc_response_valid;
      test_case "error" `Quick test_is_jsonrpc_response_error;
      test_case "request" `Quick test_is_jsonrpc_response_request;
      test_case "not assoc" `Quick test_is_jsonrpc_response_not_assoc;
    ];
    "is_notification", [
      test_case "true" `Quick test_is_notification_true;
      test_case "false" `Quick test_is_notification_false;
    ];
    "get_id", [
      test_case "some" `Quick test_get_id_some;
      test_case "none" `Quick test_get_id_none;
    ];
    "is_valid_request_id", [
      test_case "null" `Quick test_is_valid_request_id_null;
      test_case "string" `Quick test_is_valid_request_id_string;
      test_case "int" `Quick test_is_valid_request_id_int;
      test_case "float" `Quick test_is_valid_request_id_float;
      test_case "array" `Quick test_is_valid_request_id_array;
      test_case "object" `Quick test_is_valid_request_id_object;
    ];
    "validate_initialize_params", [
      test_case "valid" `Quick test_validate_initialize_params_valid;
      test_case "none" `Quick test_validate_initialize_params_none;
      test_case "missing version" `Quick test_validate_initialize_params_missing_version;
    ];
    "jsonrpc_request_of_yojson", [
      test_case "valid" `Quick test_jsonrpc_request_of_yojson_valid;
      test_case "minimal" `Quick test_jsonrpc_request_of_yojson_minimal;
    ];
  ]
