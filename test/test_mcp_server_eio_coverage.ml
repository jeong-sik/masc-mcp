(** MCP Server Eio Module Coverage Tests

    Tests for server utility functions
*)

open Alcotest

module Mcp_server_eio = Masc_mcp.Mcp_server_eio
module Mcp_server = Masc_mcp.Mcp_server

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
  check bool "valid 2.0" true (Mcp_server.is_jsonrpc_v2 json)

let test_is_jsonrpc_v2_invalid () =
  let json = `Assoc [("jsonrpc", `String "1.0")] in
  check bool "invalid 1.0" false (Mcp_server.is_jsonrpc_v2 json)

let test_is_jsonrpc_v2_missing () =
  let json = `Assoc [] in
  check bool "missing field" false (Mcp_server.is_jsonrpc_v2 json)

(* ============================================================
   normalize_protocol_version Tests
   ============================================================ *)

let test_normalize_protocol_version () =
  check string "2024-11-05" "2024-11-05"
    (Mcp_server.normalize_protocol_version "2024-11-05")

let test_normalize_protocol_version_unknown () =
  let result = Mcp_server.normalize_protocol_version "unknown" in
  check bool "returns string" true (String.length result > 0)

(* ============================================================
   protocol_version_from_params Tests
   ============================================================ *)

let test_protocol_version_from_params_none () =
  let result = Mcp_server.protocol_version_from_params None in
  check bool "returns default" true (String.length result > 0)

let test_protocol_version_from_params_some () =
  let params = `Assoc [("protocolVersion", `String "2024-11-05")] in
  let result = Mcp_server.protocol_version_from_params (Some params) in
  check string "extracts version" "2024-11-05" result

(* ============================================================
   make_response Tests
   ============================================================ *)

let test_make_response () =
  let response = Mcp_server.make_response ~id:(`Int 42) (`String "success") in
  let open Yojson.Safe.Util in
  let id = response |> member "id" in
  match id with
  | `Int 42 -> check bool "id matches" true true
  | _ -> fail "expected Int 42"

let test_make_response_has_jsonrpc () =
  let response = Mcp_server.make_response ~id:(`String "test-id") `Null in
  let open Yojson.Safe.Util in
  let version = response |> member "jsonrpc" |> to_string in
  check string "jsonrpc version" "2.0" version

(* ============================================================
   make_error Tests
   ============================================================ *)

let test_make_error () =
  let error = Mcp_server.make_error ~id:(`Int 1) (-32600) "Invalid Request" in
  let open Yojson.Safe.Util in
  let err_obj = error |> member "error" in
  let code = err_obj |> member "code" |> to_int in
  check int "error code" (-32600) code

let test_make_error_message () =
  let error = Mcp_server.make_error ~id:(`Int 1) (-32601) "Method not found" in
  let open Yojson.Safe.Util in
  let err_obj = error |> member "error" in
  let msg = err_obj |> member "message" |> to_string in
  check string "error message" "Method not found" msg

let test_make_error_with_data () =
  let error = Mcp_server.make_error ~data:(`String "extra info") ~id:(`Int 1) (-32602) "Invalid params" in
  let open Yojson.Safe.Util in
  let err_obj = error |> member "error" in
  let data = err_obj |> member "data" |> to_string in
  check string "error data" "extra info" data

(* ============================================================
   has_field Tests
   ============================================================ *)

let test_has_field_exists () =
  let json = `Assoc [("key", `String "value")] in
  check bool "field exists" true (Mcp_server.has_field "key" json)

let test_has_field_missing () =
  let json = `Assoc [("other", `String "value")] in
  check bool "field missing" false (Mcp_server.has_field "key" json)

let test_has_field_not_assoc () =
  let json = `List [] in
  check bool "not assoc" false (Mcp_server.has_field "key" json)

(* ============================================================
   get_field Tests
   ============================================================ *)

let test_get_field_exists () =
  let json = `Assoc [("name", `String "test")] in
  match Mcp_server.get_field "name" json with
  | Some (`String "test") -> check bool "found" true true
  | _ -> fail "expected Some"

let test_get_field_missing () =
  let json = `Assoc [] in
  match Mcp_server.get_field "name" json with
  | None -> check bool "not found" true true
  | Some _ -> fail "expected None"

let test_get_field_not_assoc () =
  let json = `String "not assoc" in
  match Mcp_server.get_field "name" json with
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
  check bool "valid response" true (Mcp_server.is_jsonrpc_response json)

let test_is_jsonrpc_response_error () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("error", `Assoc [("code", `Int (-32600))]);
  ] in
  check bool "error response" true (Mcp_server.is_jsonrpc_response json)

let test_is_jsonrpc_response_request () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "test");
  ] in
  check bool "not a response (is request)" false (Mcp_server.is_jsonrpc_response json)

let test_is_jsonrpc_response_not_assoc () =
  check bool "not assoc" false (Mcp_server.is_jsonrpc_response (`List []))

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
  check bool "is notification" true (Mcp_server.is_notification req)

let test_is_notification_false () =
  let req : Mcp_server_eio.jsonrpc_request = {
    jsonrpc = "2.0";
    id = Some (`Int 1);
    method_ = "request";
    params = None;
  } in
  check bool "not notification" false (Mcp_server.is_notification req)

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
  match Mcp_server.get_id req with
  | `Int 42 -> check bool "id 42" true true
  | _ -> fail "expected Int 42"

let test_get_id_none () =
  let req : Mcp_server_eio.jsonrpc_request = {
    jsonrpc = "2.0";
    id = None;
    method_ = "test";
    params = None;
  } in
  match Mcp_server.get_id req with
  | `Null -> check bool "null" true true
  | _ -> fail "expected Null"

(* ============================================================
   is_valid_request_id Tests
   ============================================================ *)

let test_is_valid_request_id_null () =
  check bool "null valid" true (Mcp_server.is_valid_request_id `Null)

let test_is_valid_request_id_string () =
  check bool "string valid" true (Mcp_server.is_valid_request_id (`String "id-1"))

let test_is_valid_request_id_int () =
  check bool "int valid" true (Mcp_server.is_valid_request_id (`Int 123))

let test_is_valid_request_id_float () =
  check bool "float valid" true (Mcp_server.is_valid_request_id (`Float 1.5))

let test_is_valid_request_id_array () =
  check bool "array invalid" false (Mcp_server.is_valid_request_id (`List []))

let test_is_valid_request_id_object () =
  check bool "object invalid" false (Mcp_server.is_valid_request_id (`Assoc []))

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
  match Mcp_server.validate_initialize_params (Some params) with
  | Ok () -> check bool "valid" true true
  | Error e -> fail ("unexpected error: " ^ e)

let test_validate_initialize_params_none () =
  match Mcp_server.validate_initialize_params None with
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
  match Mcp_server.validate_initialize_params (Some params) with
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
   detect_mode Tests (Mcp_server_eio-specific)
   ============================================================ *)

let test_detect_mode_framed () =
  let result = Mcp_server_eio.detect_mode "Content-Length: 123" in
  check bool "framed mode" true (result = Mcp_server_eio.Framed)

let test_detect_mode_framed_lowercase () =
  let result = Mcp_server_eio.detect_mode "content-length: 456" in
  check bool "framed mode lowercase" true (result = Mcp_server_eio.Framed)

let test_detect_mode_framed_mixed_case () =
  let result = Mcp_server_eio.detect_mode "Content-LENGTH: 789" in
  check bool "framed mode mixed" true (result = Mcp_server_eio.Framed)

let test_detect_mode_line_delimited () =
  let result = Mcp_server_eio.detect_mode "{\"jsonrpc\":\"2.0\"}" in
  check bool "line delimited" true (result = Mcp_server_eio.LineDelimited)

let test_detect_mode_empty () =
  let result = Mcp_server_eio.detect_mode "" in
  check bool "empty is line delimited" true (result = Mcp_server_eio.LineDelimited)

let test_detect_mode_partial_content () =
  let result = Mcp_server_eio.detect_mode "Content" in
  check bool "partial is line delimited" true (result = Mcp_server_eio.LineDelimited)

(* ============================================================
   governance_defaults Tests
   ============================================================ *)

let test_governance_defaults_development () =
  let g = Mcp_server_eio.governance_defaults "development" in
  check string "level" "development" g.level;
  check bool "audit disabled" false g.audit_enabled;
  check bool "anomaly disabled" false g.anomaly_detection

let test_governance_defaults_production () =
  let g = Mcp_server_eio.governance_defaults "production" in
  check string "level" "production" g.level;
  check bool "audit enabled" true g.audit_enabled;
  check bool "anomaly disabled" false g.anomaly_detection

let test_governance_defaults_enterprise () =
  let g = Mcp_server_eio.governance_defaults "enterprise" in
  check string "level" "enterprise" g.level;
  check bool "audit enabled" true g.audit_enabled;
  check bool "anomaly enabled" true g.anomaly_detection

let test_governance_defaults_paranoid () =
  let g = Mcp_server_eio.governance_defaults "paranoid" in
  check string "level" "paranoid" g.level;
  check bool "audit enabled" true g.audit_enabled;
  check bool "anomaly enabled" true g.anomaly_detection

let test_governance_defaults_unknown () =
  let g = Mcp_server_eio.governance_defaults "custom" in
  check string "level lowercase" "custom" g.level;
  check bool "audit disabled" false g.audit_enabled;
  check bool "anomaly disabled" false g.anomaly_detection

let test_governance_defaults_mixed_case () =
  let g = Mcp_server_eio.governance_defaults "PRODUCTION" in
  check string "level normalized" "production" g.level;
  check bool "audit enabled" true g.audit_enabled

(* ============================================================
   audit_event_to_json Tests
   ============================================================ *)

let test_audit_event_to_json_full () =
  let event : Mcp_server_eio.audit_event = {
    timestamp = 1706400000.0;
    agent = "claude-test";
    event_type = "tool_call";
    success = true;
    detail = Some "masc_status";
  } in
  let json = Mcp_server_eio.audit_event_to_json event in
  let open Yojson.Safe.Util in
  check (float 0.1) "timestamp" 1706400000.0 (json |> member "timestamp" |> to_float);
  check string "agent" "claude-test" (json |> member "agent" |> to_string);
  check string "event_type" "tool_call" (json |> member "event_type" |> to_string);
  check bool "success" true (json |> member "success" |> to_bool);
  check string "detail" "masc_status" (json |> member "detail" |> to_string)

let test_audit_event_to_json_no_detail () =
  let event : Mcp_server_eio.audit_event = {
    timestamp = 1706400000.0;
    agent = "gemini";
    event_type = "join";
    success = false;
    detail = None;
  } in
  let json = Mcp_server_eio.audit_event_to_json event in
  let open Yojson.Safe.Util in
  check bool "success false" false (json |> member "success" |> to_bool);
  check bool "detail null" true ((json |> member "detail") = `Null)

(* ============================================================
   mcp_session_to_json Tests
   ============================================================ *)

let test_mcp_session_to_json_full () =
  let session : Mcp_server_eio.mcp_session_record = {
    id = "sess-123";
    agent_name = Some "claude";
    created_at = 1706400000.0;
    last_seen = 1706403600.0;
  } in
  let json = Mcp_server_eio.mcp_session_to_json session in
  let open Yojson.Safe.Util in
  check string "id" "sess-123" (json |> member "id" |> to_string);
  check string "agent_name" "claude" (json |> member "agent_name" |> to_string);
  check (float 0.1) "created_at" 1706400000.0 (json |> member "created_at" |> to_float);
  check (float 0.1) "last_seen" 1706403600.0 (json |> member "last_seen" |> to_float)

let test_mcp_session_to_json_no_agent () =
  let session : Mcp_server_eio.mcp_session_record = {
    id = "sess-456";
    agent_name = None;
    created_at = 1706400000.0;
    last_seen = 1706400000.0;
  } in
  let json = Mcp_server_eio.mcp_session_to_json session in
  let open Yojson.Safe.Util in
  check string "id" "sess-456" (json |> member "id" |> to_string);
  check bool "agent_name null" true ((json |> member "agent_name") = `Null)

(* ============================================================
   mcp_session_of_json Tests
   ============================================================ *)

let test_mcp_session_of_json_valid () =
  let json = `Assoc [
    ("id", `String "sess-789");
    ("agent_name", `String "codex");
    ("created_at", `Float 1706400000.0);
    ("last_seen", `Float 1706407200.0);
  ] in
  match Mcp_server_eio.mcp_session_of_json json with
  | Some s ->
      check string "id" "sess-789" s.id;
      check (option string) "agent_name" (Some "codex") s.agent_name;
      check (float 0.1) "created_at" 1706400000.0 s.created_at;
      check (float 0.1) "last_seen" 1706407200.0 s.last_seen
  | None -> fail "expected Some"

let test_mcp_session_of_json_null_agent () =
  let json = `Assoc [
    ("id", `String "sess-abc");
    ("agent_name", `Null);
    ("created_at", `Float 1706400000.0);
    ("last_seen", `Float 1706400000.0);
  ] in
  match Mcp_server_eio.mcp_session_of_json json with
  | Some s ->
      check string "id" "sess-abc" s.id;
      check (option string) "agent_name" None s.agent_name
  | None -> fail "expected Some"

let test_mcp_session_of_json_invalid_missing_id () =
  let json = `Assoc [
    ("agent_name", `String "test");
    ("created_at", `Float 1706400000.0);
    ("last_seen", `Float 1706400000.0);
  ] in
  match Mcp_server_eio.mcp_session_of_json json with
  | None -> check bool "invalid returns None" true true
  | Some _ -> fail "expected None"

let test_mcp_session_of_json_not_assoc () =
  let json = `String "not an object" in
  match Mcp_server_eio.mcp_session_of_json json with
  | None -> check bool "non-assoc returns None" true true
  | Some _ -> fail "expected None"

let test_mcp_session_roundtrip () =
  let session : Mcp_server_eio.mcp_session_record = {
    id = "roundtrip-test";
    agent_name = Some "test-agent";
    created_at = 1706412345.678;
    last_seen = 1706498765.432;
  } in
  let json = Mcp_server_eio.mcp_session_to_json session in
  match Mcp_server_eio.mcp_session_of_json json with
  | Some decoded ->
      check string "id roundtrip" session.id decoded.id;
      check (option string) "agent roundtrip" session.agent_name decoded.agent_name;
      check (float 0.001) "created roundtrip" session.created_at decoded.created_at;
      check (float 0.001) "last_seen roundtrip" session.last_seen decoded.last_seen
  | None -> fail "roundtrip failed"

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
    "detect_mode", [
      test_case "framed" `Quick test_detect_mode_framed;
      test_case "framed lowercase" `Quick test_detect_mode_framed_lowercase;
      test_case "framed mixed case" `Quick test_detect_mode_framed_mixed_case;
      test_case "line delimited" `Quick test_detect_mode_line_delimited;
      test_case "empty" `Quick test_detect_mode_empty;
      test_case "partial content" `Quick test_detect_mode_partial_content;
    ];
    "governance_defaults", [
      test_case "development" `Quick test_governance_defaults_development;
      test_case "production" `Quick test_governance_defaults_production;
      test_case "enterprise" `Quick test_governance_defaults_enterprise;
      test_case "paranoid" `Quick test_governance_defaults_paranoid;
      test_case "unknown" `Quick test_governance_defaults_unknown;
      test_case "mixed case" `Quick test_governance_defaults_mixed_case;
    ];
    "audit_event_to_json", [
      test_case "full" `Quick test_audit_event_to_json_full;
      test_case "no detail" `Quick test_audit_event_to_json_no_detail;
    ];
    "mcp_session_to_json", [
      test_case "full" `Quick test_mcp_session_to_json_full;
      test_case "no agent" `Quick test_mcp_session_to_json_no_agent;
    ];
    "mcp_session_of_json", [
      test_case "valid" `Quick test_mcp_session_of_json_valid;
      test_case "null agent" `Quick test_mcp_session_of_json_null_agent;
      test_case "missing id" `Quick test_mcp_session_of_json_invalid_missing_id;
      test_case "not assoc" `Quick test_mcp_session_of_json_not_assoc;
      test_case "roundtrip" `Quick test_mcp_session_roundtrip;
    ];
  ]
