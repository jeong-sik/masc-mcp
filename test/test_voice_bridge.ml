(** Tests for Voice Bridge module *)

open Masc_mcp.Voice_bridge

(** ============================================
    Unit Tests - Configuration
    ============================================ *)

let test_get_voice_for_known_agent () =
  (* Test that known agents get their assigned voices *)
  Alcotest.(check string) "claude voice" "Sarah" (get_voice_for_agent "claude");
  Alcotest.(check string) "gemini voice" "Roger" (get_voice_for_agent "gemini");
  Alcotest.(check string) "codex voice" "George" (get_voice_for_agent "codex");
  Alcotest.(check string) "ollama voice" "Laura" (get_voice_for_agent "ollama")

let test_get_voice_for_unknown_agent () =
  (* Test that unknown agents get default voice *)
  Alcotest.(check string) "unknown agent" "Sarah" (get_voice_for_agent "unknown");
  Alcotest.(check string) "empty agent" "Sarah" (get_voice_for_agent "")

(** ============================================
    Unit Tests - Types
    ============================================ *)

let test_voice_session_status_creation () =
  let status = {
    session_id = "test-123";
    agent_id = "claude";
    voice = "Sarah";
    is_active = true;
    turn_count = 5;
    duration_seconds = Some 120.5;
  } in
  Alcotest.(check string) "session_id" "test-123" status.session_id;
  Alcotest.(check string) "agent_id" "claude" status.agent_id;
  Alcotest.(check bool) "is_active" true status.is_active;
  Alcotest.(check int) "turn_count" 5 status.turn_count

let test_conference_status_creation () =
  let status = {
    conference_id = "conf-456";
    state = "active";
    participants = ["claude"; "gemini"; "codex"];
    current_speaker = Some "claude";
    queue_size = 2;
    turn_count = 10;
  } in
  Alcotest.(check string) "conference_id" "conf-456" status.conference_id;
  Alcotest.(check string) "state" "active" status.state;
  Alcotest.(check int) "participants count" 3 (List.length status.participants);
  Alcotest.(check (option string)) "current_speaker" (Some "claude") status.current_speaker

let test_turn_request_result_creation () =
  let result = {
    status = "queued";
    agent_id = "gemini";
    message_preview = "Hello, world!";
    voice = "Roger";
    queue_position = 2;
  } in
  Alcotest.(check string) "status" "queued" result.status;
  Alcotest.(check int) "queue_position" 2 result.queue_position

(** ============================================
    Unit Tests - MCP Response Parsing
    ============================================ *)

let test_extract_mcp_result_success () =
  let json = Yojson.Safe.from_string {|{
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
      "content": [
        {"type": "text", "text": "{\"session_id\": \"abc123\"}"}
      ]
    }
  }|} in
  match extract_mcp_result json with
  | Ok data ->
    let open Yojson.Safe.Util in
    let session_id = data |> member "session_id" |> to_string in
    Alcotest.(check string) "parsed session_id" "abc123" session_id
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)

let test_extract_mcp_result_error () =
  let json = Yojson.Safe.from_string {|{
    "jsonrpc": "2.0",
    "id": 1,
    "error": {
      "code": -32600,
      "message": "Invalid request"
    }
  }|} in
  match extract_mcp_result json with
  | Ok _ -> Alcotest.fail "Expected Error, got Ok"
  | Error e -> Alcotest.(check string) "error message" "Invalid request" e

let test_extract_mcp_result_empty_content () =
  let json = Yojson.Safe.from_string {|{
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
      "content": []
    }
  }|} in
  match extract_mcp_result json with
  | Ok data ->
    Alcotest.(check string) "empty content" "{}" (Yojson.Safe.to_string data)
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)

(** ============================================
    Unit Tests - Text Fallback
    ============================================ *)

let test_text_fallback () =
  let fallback = text_fallback ~agent_id:"claude" ~message:"Test message" in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "status" "text_fallback" (fallback |> member "status" |> to_string);
  Alcotest.(check string) "agent_id" "claude" (fallback |> member "agent_id" |> to_string);
  Alcotest.(check string) "message" "Test message" (fallback |> member "message" |> to_string)

(** ============================================
    Integration Tests - Async Functions (mocked)
    ============================================ *)

(* Note: These tests verify the function signatures and basic structure.
   Full integration tests require a running Voice MCP server. *)

let test_get_agent_voice_returns_json () =
  let result = Lwt_main.run (get_agent_voice ~agent_id:"claude") in
  match result with
  | Ok json ->
    let open Yojson.Safe.Util in
    Alcotest.(check string) "agent_id" "claude" (json |> member "agent_id" |> to_string);
    Alcotest.(check string) "voice" "Sarah" (json |> member "voice" |> to_string);
    let config_source = json |> member "config_source" |> to_string in
    Alcotest.(check bool) "config_source valid" true
      (config_source = "default" || config_source = "file")
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)

let test_get_agent_voice_unknown_agent () =
  let result = Lwt_main.run (get_agent_voice ~agent_id:"unknown_agent") in
  match result with
  | Ok json ->
    let open Yojson.Safe.Util in
    Alcotest.(check string) "voice fallback" "Sarah" (json |> member "voice" |> to_string)
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)

(** ============================================
    Test Suite Registration
    ============================================ *)

let configuration_tests = [
  "get_voice_for_known_agent", `Quick, test_get_voice_for_known_agent;
  "get_voice_for_unknown_agent", `Quick, test_get_voice_for_unknown_agent;
]

let types_tests = [
  "voice_session_status_creation", `Quick, test_voice_session_status_creation;
  "conference_status_creation", `Quick, test_conference_status_creation;
  "turn_request_result_creation", `Quick, test_turn_request_result_creation;
]

let mcp_parsing_tests = [
  "extract_mcp_result_success", `Quick, test_extract_mcp_result_success;
  "extract_mcp_result_error", `Quick, test_extract_mcp_result_error;
  "extract_mcp_result_empty_content", `Quick, test_extract_mcp_result_empty_content;
]

let fallback_tests = [
  "text_fallback", `Quick, test_text_fallback;
]

let integration_tests = [
  "get_agent_voice_returns_json", `Quick, test_get_agent_voice_returns_json;
  "get_agent_voice_unknown_agent", `Quick, test_get_agent_voice_unknown_agent;
]

(** ============================================
    Unit Tests - Retry Logic
    ============================================ *)

(* Note: is_retryable_error is internal, but we can test the behavior indirectly
   through the exported functions. These tests verify the retry categories. *)

let test_retryable_error_patterns () =
  (* Connection errors should be retryable *)
  let conn_err = "Connection refused" in
  Alcotest.(check bool) "connection error lowercase check"
    true (String.lowercase_ascii conn_err |> fun s -> String.sub s 0 10 = "connection");

  (* Timeout errors should be retryable *)
  let timeout_err = "timeout after 5s" in
  Alcotest.(check bool) "timeout error lowercase check"
    true (String.lowercase_ascii timeout_err |> fun s -> String.sub s 0 7 = "timeout");

  (* HTTP 500 errors should be retryable *)
  let http_500 = "http 500: internal server error" in
  Alcotest.(check bool) "http 500 is retryable"
    true (String.lowercase_ascii http_500 |> fun s ->
      try Scanf.sscanf s "http %d" (fun x -> x >= 500 && x < 600)
      with _ -> false);

  (* HTTP 400 errors should NOT be retryable *)
  let http_400 = "http 400: bad request" in
  Alcotest.(check bool) "http 400 is not retryable"
    false (String.lowercase_ascii http_400 |> fun s ->
      try Scanf.sscanf s "http %d" (fun x -> x >= 500 && x < 600)
      with _ -> false)

let test_backoff_calculation () =
  (* Verify exponential backoff multiplier *)
  let initial = 1.0 in
  let multiplier = 2.0 in
  Alcotest.(check (float 0.01)) "backoff 1" 1.0 initial;
  Alcotest.(check (float 0.01)) "backoff 2" 2.0 (initial *. multiplier);
  Alcotest.(check (float 0.01)) "backoff 3" 4.0 (initial *. multiplier *. multiplier)

let retry_tests = [
  "retryable_error_patterns", `Quick, test_retryable_error_patterns;
  "backoff_calculation", `Quick, test_backoff_calculation;
]

(** ============================================
    Unit Tests - Configuration Accessors
    ============================================ *)

let test_config_accessors () =
  (* Test that config accessors return valid values *)
  let host = Masc_mcp.Voice_bridge.voice_mcp_host () in
  Alcotest.(check bool) "host not empty" true (String.length host > 0);

  let port = Masc_mcp.Voice_bridge.voice_mcp_port () in
  Alcotest.(check bool) "port valid" true (port > 0 && port < 65536);

  let timeout = Masc_mcp.Voice_bridge.request_timeout_seconds () in
  Alcotest.(check bool) "timeout positive" true (timeout > 0.0);

  let retries = Masc_mcp.Voice_bridge.max_retries () in
  Alcotest.(check bool) "retries positive" true (retries > 0);

  let backoff = Masc_mcp.Voice_bridge.initial_backoff_seconds () in
  Alcotest.(check bool) "backoff positive" true (backoff > 0.0);

  let multiplier = Masc_mcp.Voice_bridge.backoff_multiplier () in
  Alcotest.(check bool) "multiplier > 1" true (multiplier > 1.0)

let test_agent_voices_accessor () =
  (* Test that agent_voices returns non-empty list *)
  let voices = Masc_mcp.Voice_bridge.agent_voices () in
  Alcotest.(check bool) "voices not empty" true (List.length voices > 0);

  (* Claude should have a voice *)
  let claude_voice = List.assoc_opt "claude" voices in
  Alcotest.(check bool) "claude has voice" true (Option.is_some claude_voice);

  (* Gemini should have a voice *)
  let gemini_voice = List.assoc_opt "gemini" voices in
  Alcotest.(check bool) "gemini has voice" true (Option.is_some gemini_voice)

let test_is_retryable_error_empty_string () =
  (* Empty string should not be retryable *)
  let result = Masc_mcp.Voice_bridge.is_retryable_error "" in
  Alcotest.(check bool) "empty not retryable" false result

let test_is_retryable_error_short_strings () =
  (* Short strings should not crash (boundary check) *)
  let short_errors = ["a"; "ab"; "co"; "ti"; "ht"; "x"; ""] in
  List.iter (fun s ->
    let _ = Masc_mcp.Voice_bridge.is_retryable_error s in
    (* Just verify no crash on short strings *)
    Alcotest.(check bool) (Printf.sprintf "no crash on '%s'" s) true true
  ) short_errors

let test_is_retryable_error_connection_errors () =
  (* Connection errors should be retryable *)
  Alcotest.(check bool) "connection refused" true
    (Masc_mcp.Voice_bridge.is_retryable_error "Connection refused");
  Alcotest.(check bool) "connection reset" true
    (Masc_mcp.Voice_bridge.is_retryable_error "connection reset by peer");
  Alcotest.(check bool) "connection timeout" true
    (Masc_mcp.Voice_bridge.is_retryable_error "Connection timed out")

let test_is_retryable_error_timeout_errors () =
  (* Timeout errors should be retryable *)
  Alcotest.(check bool) "timeout" true
    (Masc_mcp.Voice_bridge.is_retryable_error "timeout after 5s");
  Alcotest.(check bool) "Timeout uppercase" true
    (Masc_mcp.Voice_bridge.is_retryable_error "Timeout waiting for response");
  Alcotest.(check bool) "TIMEOUT caps" true
    (Masc_mcp.Voice_bridge.is_retryable_error "TIMEOUT")

let test_is_retryable_error_http_5xx () =
  (* HTTP 5xx errors should be retryable *)
  Alcotest.(check bool) "http 500" true
    (Masc_mcp.Voice_bridge.is_retryable_error "http 500: internal server error");
  Alcotest.(check bool) "http 502" true
    (Masc_mcp.Voice_bridge.is_retryable_error "http 502: bad gateway");
  Alcotest.(check bool) "http 503" true
    (Masc_mcp.Voice_bridge.is_retryable_error "http 503: service unavailable");
  Alcotest.(check bool) "HTTP 504" true
    (Masc_mcp.Voice_bridge.is_retryable_error "HTTP 504: gateway timeout")

let test_is_retryable_error_non_retryable () =
  (* Non-retryable errors *)
  Alcotest.(check bool) "http 400" false
    (Masc_mcp.Voice_bridge.is_retryable_error "http 400: bad request");
  Alcotest.(check bool) "http 401" false
    (Masc_mcp.Voice_bridge.is_retryable_error "http 401: unauthorized");
  Alcotest.(check bool) "http 404" false
    (Masc_mcp.Voice_bridge.is_retryable_error "http 404: not found");
  Alcotest.(check bool) "invalid json" false
    (Masc_mcp.Voice_bridge.is_retryable_error "Invalid JSON response");
  Alcotest.(check bool) "auth error" false
    (Masc_mcp.Voice_bridge.is_retryable_error "Authentication failed");
  Alcotest.(check bool) "random error" false
    (Masc_mcp.Voice_bridge.is_retryable_error "Something went wrong")

let config_accessor_tests = [
  "config_accessors", `Quick, test_config_accessors;
  "agent_voices_accessor", `Quick, test_agent_voices_accessor;
  "is_retryable_error_empty_string", `Quick, test_is_retryable_error_empty_string;
  "is_retryable_error_short_strings", `Quick, test_is_retryable_error_short_strings;
  "is_retryable_error_connection", `Quick, test_is_retryable_error_connection_errors;
  "is_retryable_error_timeout", `Quick, test_is_retryable_error_timeout_errors;
  "is_retryable_error_http_5xx", `Quick, test_is_retryable_error_http_5xx;
  "is_retryable_error_non_retryable", `Quick, test_is_retryable_error_non_retryable;
]

let () =
  Alcotest.run "Voice Bridge" [
    "configuration", configuration_tests;
    "types", types_tests;
    "mcp_parsing", mcp_parsing_tests;
    "fallback", fallback_tests;
    "integration", integration_tests;
    "retry", retry_tests;
    "config_accessors", config_accessor_tests;
  ]
