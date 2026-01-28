(** Comprehensive Test Suite for Mcp_server_eio module

    Target: 40+ tests covering all public functions and edge cases
    Quality: Real assertions, proper edge case coverage
*)

module Mcp_eio = Masc_mcp.Mcp_server_eio
module Mcp = Masc_mcp.Mcp_server
module Room = Masc_mcp.Room
module Session = Masc_mcp.Session
module Types = Masc_mcp.Types

(* ===== Test Helpers ===== *)

let temp_dir () =
  let dir = Filename.temp_file "test_mcp_cov_" "" in
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

(* ===== 2. Transport Mode Tests ===== *)

let test_detect_mode_framed () =
  let mode = Mcp_eio.detect_mode "Content-Length: 123" in
  match mode with
  | Mcp_eio.Framed -> Alcotest.(check pass) "framed mode" () ()
  | Mcp_eio.LineDelimited -> Alcotest.fail "expected Framed"

let test_detect_mode_line_delimited () =
  let mode = Mcp_eio.detect_mode "{\"jsonrpc\":\"2.0\"}" in
  match mode with
  | Mcp_eio.LineDelimited -> Alcotest.(check pass) "line delimited mode" () ()
  | Mcp_eio.Framed -> Alcotest.fail "expected LineDelimited"

let test_detect_mode_case_insensitive () =
  let mode = Mcp_eio.detect_mode "content-length: 123" in
  match mode with
  | Mcp_eio.Framed -> Alcotest.(check pass) "framed mode case insensitive" () ()
  | Mcp_eio.LineDelimited -> Alcotest.fail "expected Framed"

(* ===== 3. Governance Config Tests ===== *)

let test_governance_defaults_development () =
  let g = Mcp_eio.governance_defaults "development" in
  Alcotest.(check string) "level" "development" g.Mcp_eio.level;
  Alcotest.(check bool) "audit disabled" false g.Mcp_eio.audit_enabled;
  Alcotest.(check bool) "anomaly disabled" false g.Mcp_eio.anomaly_detection

let test_governance_defaults_production () =
  let g = Mcp_eio.governance_defaults "production" in
  Alcotest.(check string) "level" "production" g.Mcp_eio.level;
  Alcotest.(check bool) "audit enabled" true g.Mcp_eio.audit_enabled;
  Alcotest.(check bool) "anomaly disabled" false g.Mcp_eio.anomaly_detection

let test_governance_defaults_enterprise () =
  let g = Mcp_eio.governance_defaults "enterprise" in
  Alcotest.(check string) "level" "enterprise" g.Mcp_eio.level;
  Alcotest.(check bool) "audit enabled" true g.Mcp_eio.audit_enabled;
  Alcotest.(check bool) "anomaly enabled" true g.Mcp_eio.anomaly_detection

let test_governance_defaults_paranoid () =
  let g = Mcp_eio.governance_defaults "PARANOID" in
  Alcotest.(check string) "level" "paranoid" g.Mcp_eio.level;
  Alcotest.(check bool) "audit enabled" true g.Mcp_eio.audit_enabled;
  Alcotest.(check bool) "anomaly enabled" true g.Mcp_eio.anomaly_detection

let test_governance_save_load () =
  let base_path = temp_dir () in
  let config = Room.default_config base_path in
  (* Ensure .masc dir exists *)
  let masc_dir = Filename.concat base_path ".masc" in
  Unix.mkdir masc_dir 0o755;

  let g = { Mcp_eio.level = "production"; audit_enabled = true; anomaly_detection = false } in
  Mcp_eio.save_governance config g;

  let loaded = Mcp_eio.load_governance config in
  Alcotest.(check string) "level preserved" "production" loaded.Mcp_eio.level;
  Alcotest.(check bool) "audit preserved" true loaded.Mcp_eio.audit_enabled;
  Alcotest.(check bool) "anomaly preserved" false loaded.Mcp_eio.anomaly_detection;

  cleanup_dir base_path

(* ===== 4. Audit Event Tests ===== *)

let test_audit_event_to_json () =
  let event : Mcp_eio.audit_event = {
    timestamp = 1234567890.0;
    agent = "test-agent";
    event_type = "tool_call";
    success = true;
    detail = Some "masc_status";
  } in
  let json = Mcp_eio.audit_event_to_json event in
  match json with
  | `Assoc fields ->
      Alcotest.(check bool) "has timestamp" true (List.mem_assoc "timestamp" fields);
      Alcotest.(check bool) "has agent" true (List.mem_assoc "agent" fields);
      let agent = List.assoc "agent" fields in
      Alcotest.(check bool) "agent is test-agent" true (agent = `String "test-agent")
  | _ -> Alcotest.fail "not an object"

let test_audit_event_to_json_no_detail () =
  let event : Mcp_eio.audit_event = {
    timestamp = 1234567890.0;
    agent = "test-agent";
    event_type = "auth_success";
    success = true;
    detail = None;
  } in
  let json = Mcp_eio.audit_event_to_json event in
  match json with
  | `Assoc fields ->
      let detail = List.assoc "detail" fields in
      Alcotest.(check bool) "detail is null" true (detail = `Null)
  | _ -> Alcotest.fail "not an object"

(* ===== 5. MCP Session Tests ===== *)

let test_mcp_session_to_json () =
  let session : Mcp_eio.mcp_session_record = {
    id = "session-123";
    agent_name = Some "claude";
    created_at = 1000.0;
    last_seen = 2000.0;
  } in
  let json = Mcp_eio.mcp_session_to_json session in
  match json with
  | `Assoc fields ->
      let id = List.assoc "id" fields in
      let agent = List.assoc "agent_name" fields in
      Alcotest.(check bool) "id matches" true (id = `String "session-123");
      Alcotest.(check bool) "agent_name matches" true (agent = `String "claude")
  | _ -> Alcotest.fail "not an object"

let test_mcp_session_of_json_valid () =
  let json = `Assoc [
    ("id", `String "session-456");
    ("agent_name", `String "test-agent");
    ("created_at", `Float 1000.0);
    ("last_seen", `Float 2000.0);
  ] in
  match Mcp_eio.mcp_session_of_json json with
  | Some s ->
      Alcotest.(check string) "id" "session-456" s.Mcp_eio.id;
      Alcotest.(check (option string)) "agent_name" (Some "test-agent") s.Mcp_eio.agent_name;
      Alcotest.(check (float 0.001)) "created_at" 1000.0 s.Mcp_eio.created_at
  | None -> Alcotest.fail "should parse successfully"

let test_mcp_session_of_json_null_agent () =
  let json = `Assoc [
    ("id", `String "session-789");
    ("agent_name", `Null);
    ("created_at", `Float 1000.0);
    ("last_seen", `Float 2000.0);
  ] in
  match Mcp_eio.mcp_session_of_json json with
  | Some s ->
      Alcotest.(check (option string)) "agent_name is none" None s.Mcp_eio.agent_name
  | None -> Alcotest.fail "should parse successfully"

let test_mcp_session_of_json_invalid () =
  let json = `String "not an object" in
  match Mcp_eio.mcp_session_of_json json with
  | Some _ -> Alcotest.fail "should fail to parse"
  | None -> Alcotest.(check pass) "returns None" () ()

(* ===== 6. Drift Guard Tests ===== *)

let test_tokenize_empty () =
  let tokens = Mcp_eio.tokenize "" in
  Alcotest.(check int) "empty list" 0 (List.length tokens)

let test_tokenize_single_word () =
  let tokens = Mcp_eio.tokenize "hello" in
  Alcotest.(check int) "one token" 1 (List.length tokens);
  Alcotest.(check string) "token is lowercase" "hello" (List.hd tokens)

let test_tokenize_multiple_words () =
  let tokens = Mcp_eio.tokenize "Hello World Test" in
  Alcotest.(check int) "three tokens" 3 (List.length tokens);
  Alcotest.(check string) "first lowercase" "hello" (List.nth tokens 0);
  Alcotest.(check string) "second lowercase" "world" (List.nth tokens 1)

let test_tokenize_with_punctuation () =
  let tokens = Mcp_eio.tokenize "Hello, World! (Test)" in
  Alcotest.(check bool) "contains hello" true (List.mem "hello" tokens);
  Alcotest.(check bool) "contains world" true (List.mem "world" tokens);
  Alcotest.(check bool) "contains test" true (List.mem "test" tokens)

let test_jaccard_similarity_identical () =
  let a = ["hello"; "world"] in
  let b = ["hello"; "world"] in
  let sim = Mcp_eio.jaccard_similarity a b in
  Alcotest.(check (float 0.001)) "identical = 1.0" 1.0 sim

let test_jaccard_similarity_disjoint () =
  let a = ["hello"; "world"] in
  let b = ["foo"; "bar"] in
  let sim = Mcp_eio.jaccard_similarity a b in
  Alcotest.(check (float 0.001)) "disjoint = 0.0" 0.0 sim

let test_jaccard_similarity_partial () =
  let a = ["hello"; "world"; "test"] in
  let b = ["hello"; "world"; "foo"] in
  let sim = Mcp_eio.jaccard_similarity a b in
  (* intersection = 2, union = 4 -> 0.5 *)
  Alcotest.(check (float 0.001)) "partial = 0.5" 0.5 sim

let test_jaccard_similarity_empty () =
  let a = [] in
  let b = [] in
  let sim = Mcp_eio.jaccard_similarity a b in
  Alcotest.(check (float 0.001)) "empty = 1.0" 1.0 sim

let test_cosine_similarity_identical () =
  let a = ["hello"; "world"] in
  let b = ["hello"; "world"] in
  let sim = Mcp_eio.cosine_similarity a b in
  Alcotest.(check (float 0.001)) "identical = 1.0" 1.0 sim

let test_cosine_similarity_disjoint () =
  let a = ["hello"; "world"] in
  let b = ["foo"; "bar"] in
  let sim = Mcp_eio.cosine_similarity a b in
  Alcotest.(check (float 0.001)) "disjoint = 0.0" 0.0 sim

let test_cosine_similarity_empty () =
  let a = [] in
  let b = ["hello"] in
  let sim = Mcp_eio.cosine_similarity a b in
  Alcotest.(check (float 0.001)) "empty = 0.0" 0.0 sim

(* ===== 7. Network Context Tests ===== *)

let test_get_net_uninitialized () =
  (* Reset the net reference first *)
  Mcp_eio.current_net := None;
  try
    let _ = Mcp_eio.get_net () in
    Alcotest.fail "should raise"
  with
  | Failure msg ->
      Alcotest.(check bool) "contains 'not initialized'" true
        (String.length msg > 0 && (try let _ = Str.search_forward (Str.regexp_string "not initialized") msg 0 in true with Not_found -> false))
  | _ -> Alcotest.fail "wrong exception type"

(* ===== 8. Protocol Helpers Tests (extending existing) ===== *)

let test_is_jsonrpc_response_true () =
  let json = `Assoc [("jsonrpc", `String "2.0"); ("id", `Int 1); ("result", `String "ok")] in
  Alcotest.(check bool) "is response" true (Mcp_eio.is_jsonrpc_response json)

let test_is_jsonrpc_response_false () =
  let json = `Assoc [("jsonrpc", `String "2.0"); ("id", `Int 1); ("method", `String "test")] in
  Alcotest.(check bool) "is not response" false (Mcp_eio.is_jsonrpc_response json)

let test_get_id_int () =
  let req : Mcp.jsonrpc_request = { jsonrpc = "2.0"; id = Some (`Int 42); method_ = "test"; params = None } in
  let id = Mcp_eio.get_id req in
  Alcotest.(check bool) "id is 42" true (id = `Int 42)

let test_get_id_string () =
  let req : Mcp.jsonrpc_request = { jsonrpc = "2.0"; id = Some (`String "abc"); method_ = "test"; params = None } in
  let id = Mcp_eio.get_id req in
  Alcotest.(check bool) "id is abc" true (id = `String "abc")

let test_get_id_none () =
  let req : Mcp.jsonrpc_request = { jsonrpc = "2.0"; id = None; method_ = "test"; params = None } in
  let id = Mcp_eio.get_id req in
  Alcotest.(check bool) "id is null" true (id = `Null)

let test_is_valid_request_id_int () =
  Alcotest.(check bool) "int is valid" true (Mcp_eio.is_valid_request_id (`Int 1))

let test_is_valid_request_id_string () =
  Alcotest.(check bool) "string is valid" true (Mcp_eio.is_valid_request_id (`String "abc"))

let test_is_valid_request_id_null () =
  Alcotest.(check bool) "null is valid" true (Mcp_eio.is_valid_request_id `Null)

let test_is_valid_request_id_invalid () =
  Alcotest.(check bool) "array is invalid" false (Mcp_eio.is_valid_request_id (`List []))

let test_normalize_protocol_version_known () =
  Alcotest.(check string) "2024-11-05" "2024-11-05" (Mcp_eio.normalize_protocol_version "2024-11-05")

let test_normalize_protocol_version_unknown () =
  Alcotest.(check string) "unknown defaults" "2025-11-25" (Mcp_eio.normalize_protocol_version "unknown-version")

(* ===== 9. Eio Integration Tests ===== *)

let test_handle_request_resources_list () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "resources/list");
    ("params", `Assoc []);
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has result" true (List.mem_assoc "result" fields);
       (match List.assoc_opt "result" fields with
        | Some (`Assoc result_fields) ->
            Alcotest.(check bool) "has resources" true (List.mem_assoc "resources" result_fields)
        | _ -> Alcotest.fail "result not an object")
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

let test_handle_request_prompts_list () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "prompts/list");
    ("params", `Assoc []);
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has result" true (List.mem_assoc "result" fields);
       (match List.assoc_opt "result" fields with
        | Some (`Assoc result_fields) ->
            Alcotest.(check bool) "has prompts" true (List.mem_assoc "prompts" result_fields)
        | _ -> Alcotest.fail "result not an object")
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

let test_handle_request_resource_templates_list () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "resources/templates/list");
    ("params", `Assoc []);
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has result" true (List.mem_assoc "result" fields)
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

let test_handle_request_notification_initialized () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  (* Notification without id should return null *)
  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "notifications/initialized");
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in
  Alcotest.(check bool) "notification returns null" true (response = `Null);

  cleanup_dir base_path

let test_handle_request_wrong_jsonrpc_version () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "1.0");
    ("id", `Int 1);
    ("method", `String "test");
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has error" true (List.mem_assoc "error" fields);
       (match List.assoc_opt "error" fields with
        | Some (`Assoc error_fields) ->
            let code = List.assoc "code" error_fields in
            Alcotest.(check bool) "error code -32600" true (code = `Int (-32600))
        | _ -> Alcotest.fail "error not an object")
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

let test_handle_request_missing_params_tools_call () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "tools/call");
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has error" true (List.mem_assoc "error" fields)
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

let test_handle_request_tool_masc_status () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  (* Initialize room first *)
  let _ = Room.init state.Mcp.room_config ~agent_name:None in

  let request = Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "tools/call");
    ("params", `Assoc [
      ("name", `String "masc_status");
      ("arguments", `Assoc []);
    ]);
  ]) in

  let response = Mcp_eio.handle_request ~clock ~sw state request in

  (match response with
   | `Assoc fields ->
       Alcotest.(check bool) "has result" true (List.mem_assoc "result" fields)
   | _ -> Alcotest.fail "response not an object");

  cleanup_dir base_path

(* ===== 10. Execute Tool Tests ===== *)

let test_execute_tool_masc_set_room () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let new_room = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let (success, msg) = Mcp_eio.execute_tool_eio ~sw ~clock state
    ~name:"masc_set_room"
    ~arguments:(`Assoc [("path", `String new_room)]) in

  Alcotest.(check bool) "success" true success;
  Alcotest.(check bool) "contains new path" true
    (try let _ = Str.search_forward (Str.regexp_string new_room) msg 0 in true with Not_found -> false);

  cleanup_dir base_path;
  cleanup_dir new_room

let test_execute_tool_masc_set_room_not_found () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let (success, msg) = Mcp_eio.execute_tool_eio ~sw ~clock state
    ~name:"masc_set_room"
    ~arguments:(`Assoc [("path", `String "/nonexistent/path/xyz123")]) in

  Alcotest.(check bool) "failure" false success;
  Alcotest.(check bool) "contains not found" true
    (try let _ = Str.search_forward (Str.regexp_string "not found") msg 0 in true with Not_found -> false);

  cleanup_dir base_path

let test_execute_tool_unknown_tool () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  let base_path = temp_dir () in
  let state = Mcp_eio.create_state ~test_mode:true ~base_path () in

  let (success, msg) = Mcp_eio.execute_tool_eio ~sw ~clock state
    ~name:"nonexistent_tool"
    ~arguments:(`Assoc []) in

  Alcotest.(check bool) "failure" false success;
  Alcotest.(check bool) "contains unknown" true
    (try let _ = Str.search_forward (Str.regexp_string "Unknown tool") msg 0 in true with Not_found -> false);

  cleanup_dir base_path

(* ===== Test Suites ===== *)


let transport_tests = [
  "detect_mode_framed", `Quick, test_detect_mode_framed;
  "detect_mode_line_delimited", `Quick, test_detect_mode_line_delimited;
  "detect_mode_case_insensitive", `Quick, test_detect_mode_case_insensitive;
]

let governance_tests = [
  "defaults_development", `Quick, test_governance_defaults_development;
  "defaults_production", `Quick, test_governance_defaults_production;
  "defaults_enterprise", `Quick, test_governance_defaults_enterprise;
  "defaults_paranoid", `Quick, test_governance_defaults_paranoid;
  "save_load", `Quick, test_governance_save_load;
]

let audit_tests = [
  "event_to_json", `Quick, test_audit_event_to_json;
  "event_to_json_no_detail", `Quick, test_audit_event_to_json_no_detail;
]

let mcp_session_tests = [
  "to_json", `Quick, test_mcp_session_to_json;
  "of_json_valid", `Quick, test_mcp_session_of_json_valid;
  "of_json_null_agent", `Quick, test_mcp_session_of_json_null_agent;
  "of_json_invalid", `Quick, test_mcp_session_of_json_invalid;
]

let drift_guard_tests = [
  "tokenize_empty", `Quick, test_tokenize_empty;
  "tokenize_single_word", `Quick, test_tokenize_single_word;
  "tokenize_multiple_words", `Quick, test_tokenize_multiple_words;
  "tokenize_with_punctuation", `Quick, test_tokenize_with_punctuation;
  "jaccard_identical", `Quick, test_jaccard_similarity_identical;
  "jaccard_disjoint", `Quick, test_jaccard_similarity_disjoint;
  "jaccard_partial", `Quick, test_jaccard_similarity_partial;
  "jaccard_empty", `Quick, test_jaccard_similarity_empty;
  "cosine_identical", `Quick, test_cosine_similarity_identical;
  "cosine_disjoint", `Quick, test_cosine_similarity_disjoint;
  "cosine_empty", `Quick, test_cosine_similarity_empty;
]

let network_tests = [
  "get_net_uninitialized", `Quick, test_get_net_uninitialized;
]

let protocol_tests = [
  "is_jsonrpc_response_true", `Quick, test_is_jsonrpc_response_true;
  "is_jsonrpc_response_false", `Quick, test_is_jsonrpc_response_false;
  "get_id_int", `Quick, test_get_id_int;
  "get_id_string", `Quick, test_get_id_string;
  "get_id_none", `Quick, test_get_id_none;
  "is_valid_request_id_int", `Quick, test_is_valid_request_id_int;
  "is_valid_request_id_string", `Quick, test_is_valid_request_id_string;
  "is_valid_request_id_null", `Quick, test_is_valid_request_id_null;
  "is_valid_request_id_invalid", `Quick, test_is_valid_request_id_invalid;
  "normalize_protocol_version_known", `Quick, test_normalize_protocol_version_known;
  "normalize_protocol_version_unknown", `Quick, test_normalize_protocol_version_unknown;
]

let eio_tests = [
  "handle_resources_list", `Quick, test_handle_request_resources_list;
  "handle_prompts_list", `Quick, test_handle_request_prompts_list;
  "handle_resource_templates_list", `Quick, test_handle_request_resource_templates_list;
  "handle_notification_initialized", `Quick, test_handle_request_notification_initialized;
  "handle_wrong_jsonrpc_version", `Quick, test_handle_request_wrong_jsonrpc_version;
  "handle_missing_params", `Quick, test_handle_request_missing_params_tools_call;
  "handle_tool_masc_status", `Quick, test_handle_request_tool_masc_status;
]

let execute_tool_tests = [
  "masc_set_room", `Quick, test_execute_tool_masc_set_room;
  "masc_set_room_not_found", `Quick, test_execute_tool_masc_set_room_not_found;
  "unknown_tool", `Quick, test_execute_tool_unknown_tool;
]

let () =
  Alcotest.run "Mcp_server_eio Coverage" [
    "transport", transport_tests;
    "governance", governance_tests;
    "audit", audit_tests;
    "mcp_session", mcp_session_tests;
    "drift_guard", drift_guard_tests;
    "network", network_tests;
    "protocol", protocol_tests;
    "eio", eio_tests;
    "execute_tool", execute_tool_tests;
  ]
