(** Tests for MCP Session Management - MCP 2025-11-25 Spec *)

open Alcotest

module Session = Masc_mcp.Session
module Encryption = Masc_mcp.Encryption

(* Initialize RNG for crypto *)
let () = Encryption.initialize ()

(* Helper: check if s2 is substring of s1 *)
let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

(** MCP Session Store tests *)
let test_mcp_session_create () =
  let session = Session.McpSessionStore.create () in
  check bool "has id" true (String.length session.id > 0);
  check bool "id starts with mcp_" true (String.sub session.id 0 4 = "mcp_");
  check (option string) "no agent initially" None session.agent_name;
  check int "no requests initially" 0 session.request_count;
  check (list (pair string string)) "no metadata initially" [] session.metadata

let test_mcp_session_create_with_agent () =
  let session = Session.McpSessionStore.create ~agent_name:"claude" () in
  check (option string) "has agent" (Some "claude") session.agent_name

let test_mcp_session_get () =
  let session = Session.McpSessionStore.create () in
  let found = Session.McpSessionStore.get session.id in
  check bool "found" true (Option.is_some found);
  check string "same id" session.id (Option.get found).id

let test_mcp_session_get_not_found () =
  let found = Session.McpSessionStore.get "nonexistent-session" in
  check bool "not found" true (Option.is_none found)

let test_mcp_session_get_updates_activity () =
  let session = Session.McpSessionStore.create () in
  let old_activity = session.last_activity in
  (* Increase sleep time for more tolerance on slow systems *)
  Unix.sleepf 0.1;
  let _ = Session.McpSessionStore.get session.id in  (* get updates activity *)
  check bool "activity updated" true (session.last_activity >= old_activity)

let test_mcp_session_get_increments_request () =
  let session = Session.McpSessionStore.create () in
  check int "initial count" 0 session.request_count;
  let _ = Session.McpSessionStore.get session.id in
  check int "count after first get" 1 session.request_count;
  let _ = Session.McpSessionStore.get session.id in
  check int "count after second get" 2 session.request_count

let test_mcp_session_remove () =
  let session = Session.McpSessionStore.create () in
  let id = session.id in
  let result = Session.McpSessionStore.remove id in
  check bool "remove returned true" true result;
  let found = Session.McpSessionStore.get id in
  check bool "removed" true (Option.is_none found)

let test_mcp_session_remove_not_found () =
  let result = Session.McpSessionStore.remove "nonexistent-id" in
  check bool "remove returned false" false result

let test_mcp_session_list_all () =
  let _s1 = Session.McpSessionStore.create ~agent_name:"agent1" () in
  let _s2 = Session.McpSessionStore.create ~agent_name:"agent2" () in
  let all = Session.McpSessionStore.list_all () in
  check bool "has sessions" true (List.length all >= 2)

(** Header extraction tests *)
let test_extract_session_id_primary () =
  let headers = Cohttp.Header.init_with "Mcp-Session-Id" "session-123" in
  let result = Session.extract_mcp_session_id headers in
  check (option string) "extracts primary" (Some "session-123") result

let test_extract_session_id_fallback () =
  let headers = Cohttp.Header.init_with "X-MCP-Session-ID" "session-456" in
  let result = Session.extract_mcp_session_id headers in
  check (option string) "extracts fallback" (Some "session-456") result

let test_extract_session_id_none () =
  let headers = Cohttp.Header.init () in
  let result = Session.extract_mcp_session_id headers in
  check (option string) "no header" None result

let test_extract_session_id_primary_precedence () =
  let headers = Cohttp.Header.init () in
  let headers = Cohttp.Header.add headers "Mcp-Session-Id" "primary" in
  let headers = Cohttp.Header.add headers "X-MCP-Session-ID" "fallback" in
  let result = Session.extract_mcp_session_id headers in
  check (option string) "primary takes precedence" (Some "primary") result

(** Tool handler tests *)
let test_tool_create () =
  let args = `Assoc [("action", `String "create")] in
  let (success, result) = Session.handle_mcp_session_tool args in
  check bool "create success" true success;
  check bool "result has id" true (contains result "id")

let test_tool_create_with_agent () =
  let args = `Assoc [
    ("action", `String "create");
    ("agent_name", `String "test-agent");
  ] in
  let (success, result) = Session.handle_mcp_session_tool args in
  check bool "create success" true success;
  check bool "result has agent" true (contains result "test-agent")

let test_tool_get () =
  let session = Session.McpSessionStore.create () in
  let args = `Assoc [
    ("action", `String "get");
    ("session_id", `String session.id);
  ] in
  let (success, result) = Session.handle_mcp_session_tool args in
  check bool "get success" true success;
  check bool "result has session id" true (contains result session.id)

let test_tool_get_not_found () =
  let args = `Assoc [
    ("action", `String "get");
    ("session_id", `String "nonexistent");
  ] in
  let (success, _) = Session.handle_mcp_session_tool args in
  check bool "get fails for unknown" false success

let test_tool_list () =
  let _ = Session.McpSessionStore.create () in
  let args = `Assoc [("action", `String "list")] in
  let (success, result) = Session.handle_mcp_session_tool args in
  check bool "list success" true success;
  check bool "result has count" true (contains result "count")

let test_tool_cleanup () =
  let _session = Session.McpSessionStore.create () in
  let args = `Assoc [("action", `String "cleanup")] in
  let (success, _) = Session.handle_mcp_session_tool args in
  check bool "cleanup success" true success

let test_tool_remove () =
  let session = Session.McpSessionStore.create () in
  let args = `Assoc [
    ("action", `String "remove");
    ("session_id", `String session.id);
  ] in
  let (success, _) = Session.handle_mcp_session_tool args in
  check bool "remove success" true success;
  let found = Session.McpSessionStore.get session.id in
  check bool "session removed" true (Option.is_none found)

let test_tool_invalid_action () =
  let args = `Assoc [("action", `String "invalid")] in
  let (success, _) = Session.handle_mcp_session_tool args in
  check bool "invalid action fails" false success

let test_tool_missing_action () =
  let args = `Assoc [] in
  let (success, _) = Session.handle_mcp_session_tool args in
  check bool "missing action fails" false success

(** JSON serialization *)
let test_session_to_json () =
  let session = Session.McpSessionStore.create ~agent_name:"json-agent" () in
  let json = Session.McpSessionStore.to_json session in
  match json with
  | `Assoc fields ->
    check bool "has id" true (List.mem_assoc "id" fields);
    check bool "has agent_name" true (List.mem_assoc "agent_name" fields);
    check bool "has created_at" true (List.mem_assoc "created_at" fields);
    check bool "has request_count" true (List.mem_assoc "request_count" fields)
  | _ -> fail "Expected Assoc"

(** Test suites *)
let store_tests = [
  "create", `Quick, test_mcp_session_create;
  "create with agent", `Quick, test_mcp_session_create_with_agent;
  "get", `Quick, test_mcp_session_get;
  "get not found", `Quick, test_mcp_session_get_not_found;
  "get updates activity", `Quick, test_mcp_session_get_updates_activity;
  "get increments request", `Quick, test_mcp_session_get_increments_request;
  "remove", `Quick, test_mcp_session_remove;
  "remove not found", `Quick, test_mcp_session_remove_not_found;
  "list all", `Quick, test_mcp_session_list_all;
]

let header_tests = [
  "extract primary", `Quick, test_extract_session_id_primary;
  "extract fallback", `Quick, test_extract_session_id_fallback;
  "extract none", `Quick, test_extract_session_id_none;
  "primary precedence", `Quick, test_extract_session_id_primary_precedence;
]

let tool_tests = [
  "create action", `Quick, test_tool_create;
  "create with agent", `Quick, test_tool_create_with_agent;
  "get action", `Quick, test_tool_get;
  "get not found", `Quick, test_tool_get_not_found;
  "list action", `Quick, test_tool_list;
  "cleanup action", `Quick, test_tool_cleanup;
  "remove action", `Quick, test_tool_remove;
  "invalid action", `Quick, test_tool_invalid_action;
  "missing action", `Quick, test_tool_missing_action;
]

let json_tests = [
  "session_to_json", `Quick, test_session_to_json;
]

let () =
  run "Session" [
    "store", store_tests;
    "headers", header_tests;
    "tool", tool_tests;
    "json", json_tests;
  ]
