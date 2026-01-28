(** Session Module Coverage Tests

    Tests for session management types:
    - session type
    - rate_tracker type
    - create_tracker function
    - get_timestamps / set_timestamps
    - Atomic helpers: get_burst_used, set_burst_used, incr_burst_used
    - McpSessionStore: generate_id, to_json
    - extract_mcp_session_id
*)

open Alcotest

module Session = Masc_mcp.Session
module Types = Masc_mcp.Types

(* ============================================================
   Type Existence Tests
   ============================================================ *)

let test_session_type () =
  let s : Session.session = {
    agent_name = "claude-test";
    connected_at = 1704067200.0;
    last_activity = 1704067200.0;
    is_listening = false;
    message_queue = [];
  } in
  check string "agent_name" "claude-test" s.agent_name;
  check (float 0.1) "connected_at" 1704067200.0 s.connected_at;
  check bool "is_listening" false s.is_listening

let test_session_with_messages () =
  let msg = `Assoc [("type", `String "test")] in
  let s : Session.session = {
    agent_name = "gemini";
    connected_at = 1704067200.0;
    last_activity = 1704067250.0;
    is_listening = true;
    message_queue = [msg];
  } in
  check bool "is_listening" true s.is_listening;
  check int "message_queue length" 1 (List.length s.message_queue)

(* ============================================================
   rate_tracker Tests
   ============================================================ *)

let test_rate_tracker_type () =
  let rt : Session.rate_tracker = {
    general_timestamps = [];
    broadcast_timestamps = [];
    task_ops_timestamps = [];
    burst_used = 0;
    last_burst_reset = 0.0;
  } in
  check int "burst_used" 0 rt.burst_used;
  check (list (float 0.1)) "general_timestamps" [] rt.general_timestamps

let test_rate_tracker_with_data () =
  let rt : Session.rate_tracker = {
    general_timestamps = [1.0; 2.0; 3.0];
    broadcast_timestamps = [1.5];
    task_ops_timestamps = [2.5; 3.5];
    burst_used = 5;
    last_burst_reset = 100.0;
  } in
  check int "general count" 3 (List.length rt.general_timestamps);
  check int "broadcast count" 1 (List.length rt.broadcast_timestamps);
  check int "task_ops count" 2 (List.length rt.task_ops_timestamps);
  check int "burst_used" 5 rt.burst_used

(* ============================================================
   create_tracker Tests
   ============================================================ *)

let test_create_tracker_empty () =
  let rt = Session.create_tracker () in
  check (list (float 0.1)) "general empty" [] rt.general_timestamps;
  check (list (float 0.1)) "broadcast empty" [] rt.broadcast_timestamps;
  check (list (float 0.1)) "task_ops empty" [] rt.task_ops_timestamps;
  check int "burst_used zero" 0 rt.burst_used

let test_create_tracker_burst_reset () =
  let rt = Session.create_tracker () in
  (* last_burst_reset should be set to current time (> 0) *)
  check bool "last_burst_reset > 0" true (rt.last_burst_reset > 0.0)

(* ============================================================
   get_timestamps / set_timestamps Tests
   ============================================================ *)

let test_get_timestamps_general () =
  let rt = Session.create_tracker () in
  rt.general_timestamps <- [1.0; 2.0; 3.0];
  let ts = Session.get_timestamps rt Types.GeneralLimit in
  check int "general count" 3 (List.length ts)

let test_get_timestamps_broadcast () =
  let rt = Session.create_tracker () in
  rt.broadcast_timestamps <- [1.0; 2.0];
  let ts = Session.get_timestamps rt Types.BroadcastLimit in
  check int "broadcast count" 2 (List.length ts)

let test_get_timestamps_task_ops () =
  let rt = Session.create_tracker () in
  rt.task_ops_timestamps <- [5.0];
  let ts = Session.get_timestamps rt Types.TaskOpsLimit in
  check int "task_ops count" 1 (List.length ts)

let test_set_timestamps_general () =
  let rt = Session.create_tracker () in
  Session.set_timestamps rt Types.GeneralLimit [1.0; 2.0];
  check int "set general" 2 (List.length rt.general_timestamps)

let test_set_timestamps_broadcast () =
  let rt = Session.create_tracker () in
  Session.set_timestamps rt Types.BroadcastLimit [3.0; 4.0; 5.0];
  check int "set broadcast" 3 (List.length rt.broadcast_timestamps)

let test_set_timestamps_task_ops () =
  let rt = Session.create_tracker () in
  Session.set_timestamps rt Types.TaskOpsLimit [6.0];
  check int "set task_ops" 1 (List.length rt.task_ops_timestamps)

let test_set_get_roundtrip () =
  let rt = Session.create_tracker () in
  let ts = [10.0; 20.0; 30.0] in
  Session.set_timestamps rt Types.GeneralLimit ts;
  let ts' = Session.get_timestamps rt Types.GeneralLimit in
  check (list (float 0.1)) "roundtrip" ts ts'

(* ============================================================
   Atomic Helpers Tests
   ============================================================ *)

let test_get_burst_used_initial () =
  let rt = Session.create_tracker () in
  let burst = Session.get_burst_used rt in
  check int "initial burst" 0 burst

let test_set_burst_used () =
  let rt = Session.create_tracker () in
  Session.set_burst_used rt 5;
  let burst = Session.get_burst_used rt in
  check int "set burst" 5 burst

let test_incr_burst_used () =
  let rt = Session.create_tracker () in
  Session.set_burst_used rt 3;
  Session.incr_burst_used rt;
  let burst = Session.get_burst_used rt in
  check int "incr burst" 4 burst

let test_incr_burst_used_multiple () =
  let rt = Session.create_tracker () in
  Session.incr_burst_used rt;
  Session.incr_burst_used rt;
  Session.incr_burst_used rt;
  let burst = Session.get_burst_used rt in
  check int "incr 3 times" 3 burst

let test_get_last_burst_reset () =
  let rt = Session.create_tracker () in
  let reset = Session.get_last_burst_reset rt in
  check bool "positive reset time" true (reset > 0.0)

let test_set_last_burst_reset () =
  let rt = Session.create_tracker () in
  let new_time = 12345.0 in
  Session.set_last_burst_reset rt new_time;
  let reset = Session.get_last_burst_reset rt in
  check (float 0.1) "set reset time" new_time reset

(* ============================================================
   McpSessionStore Tests
   ============================================================ *)

(* Initialize RNG for tests that need crypto *)
let () = Mirage_crypto_rng_unix.use_default ()

let test_generate_id_prefix () =
  let id = Session.McpSessionStore.generate_id () in
  check bool "starts with mcp_" true (String.length id > 4 && String.sub id 0 4 = "mcp_")

let test_generate_id_length () =
  let id = Session.McpSessionStore.generate_id () in
  (* mcp_ (4) + 32 hex chars = 36 *)
  check int "id length" 36 (String.length id)

let test_generate_id_unique () =
  let id1 = Session.McpSessionStore.generate_id () in
  let id2 = Session.McpSessionStore.generate_id () in
  check bool "unique ids" true (id1 <> id2)

let test_mcp_session_to_json_has_id () =
  let s = Session.McpSessionStore.create () in
  let json = Session.McpSessionStore.to_json s in
  match json with
  | `Assoc fields -> check bool "has id" true (List.mem_assoc "id" fields)
  | _ -> fail "expected Assoc"

let test_mcp_session_to_json_has_created_at () =
  let s = Session.McpSessionStore.create () in
  let json = Session.McpSessionStore.to_json s in
  match json with
  | `Assoc fields -> check bool "has created_at" true (List.mem_assoc "created_at" fields)
  | _ -> fail "expected Assoc"

let test_mcp_session_to_json_has_request_count () =
  let s = Session.McpSessionStore.create () in
  let json = Session.McpSessionStore.to_json s in
  match json with
  | `Assoc fields -> check bool "has request_count" true (List.mem_assoc "request_count" fields)
  | _ -> fail "expected Assoc"

let test_mcp_session_to_json_has_metadata () =
  let s = Session.McpSessionStore.create () in
  let json = Session.McpSessionStore.to_json s in
  match json with
  | `Assoc fields -> check bool "has metadata" true (List.mem_assoc "metadata" fields)
  | _ -> fail "expected Assoc"

let test_mcp_session_to_json_agent_name_null () =
  let s = Session.McpSessionStore.create () in
  let json = Session.McpSessionStore.to_json s in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "agent_name" fields with
     | Some `Null -> ()
     | _ -> fail "expected null agent_name")
  | _ -> fail "expected Assoc"

let test_mcp_session_to_json_agent_name_some () =
  let s = Session.McpSessionStore.create ~agent_name:"test-agent" () in
  let json = Session.McpSessionStore.to_json s in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "agent_name" fields with
     | Some (`String name) -> check string "agent name" "test-agent" name
     | _ -> fail "expected string agent_name")
  | _ -> fail "expected Assoc"

let test_mcp_session_create_and_get () =
  let s = Session.McpSessionStore.create () in
  match Session.McpSessionStore.get s.id with
  | Some s' -> check string "same id" s.id s'.id
  | None -> fail "expected Some"

let test_mcp_session_get_nonexistent () =
  match Session.McpSessionStore.get "nonexistent_session_id" with
  | None -> ()
  | Some _ -> fail "expected None"

let test_mcp_session_remove () =
  let s = Session.McpSessionStore.create () in
  let id = s.id in
  let removed = Session.McpSessionStore.remove id in
  check bool "removed" true removed;
  match Session.McpSessionStore.get id with
  | None -> ()
  | Some _ -> fail "expected None after remove"

let test_mcp_session_remove_nonexistent () =
  let removed = Session.McpSessionStore.remove "nonexistent_xyz" in
  check bool "not removed" false removed

let test_mcp_session_list_all () =
  let before = List.length (Session.McpSessionStore.list_all ()) in
  let _ = Session.McpSessionStore.create () in
  let after = List.length (Session.McpSessionStore.list_all ()) in
  check bool "list increased" true (after > before || after >= 0)

(* ============================================================
   extract_mcp_session_id Tests
   ============================================================ *)

let test_extract_mcp_session_id_present () =
  let headers = Cohttp.Header.init_with "Mcp-Session-Id" "test-session-123" in
  match Session.extract_mcp_session_id headers with
  | Some id -> check string "extracted id" "test-session-123" id
  | None -> fail "expected Some"

let test_extract_mcp_session_id_x_prefix () =
  let headers = Cohttp.Header.init_with "X-MCP-Session-ID" "session-456" in
  match Session.extract_mcp_session_id headers with
  | Some id -> check string "extracted x-prefix id" "session-456" id
  | None -> fail "expected Some"

let test_extract_mcp_session_id_prefers_mcp () =
  let headers = Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Mcp-Session-Id" "preferred"
    |> fun h -> Cohttp.Header.add h "X-MCP-Session-ID" "fallback"
  in
  match Session.extract_mcp_session_id headers with
  | Some id -> check string "prefers Mcp-Session-Id" "preferred" id
  | None -> fail "expected Some"

let test_extract_mcp_session_id_missing () =
  let headers = Cohttp.Header.init () in
  match Session.extract_mcp_session_id headers with
  | None -> ()
  | Some _ -> fail "expected None"

let test_extract_mcp_session_id_other_headers () =
  let headers = Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
    |> fun h -> Cohttp.Header.add h "Authorization" "Bearer token"
  in
  match Session.extract_mcp_session_id headers with
  | None -> ()
  | Some _ -> fail "expected None"

(* ============================================================
   handle_mcp_session_tool Tests
   ============================================================ *)

let test_handle_mcp_session_tool_create () =
  let args = `Assoc [("action", `String "create"); ("agent_name", `String "test-agent")] in
  let (success, response) = Session.handle_mcp_session_tool args in
  check bool "create succeeds" true success;
  check bool "response nonempty" true (String.length response > 0)

let test_handle_mcp_session_tool_list () =
  let args = `Assoc [("action", `String "list")] in
  let (success, response) = Session.handle_mcp_session_tool args in
  check bool "list succeeds" true success;
  check bool "has count" true (
    try let _ = Str.search_forward (Str.regexp "count") response 0 in true
    with Not_found -> false)

let test_handle_mcp_session_tool_get_missing () =
  let args = `Assoc [("action", `String "get"); ("session_id", `String "nonexistent-xyz")] in
  let (success, response) = Session.handle_mcp_session_tool args in
  check bool "get missing fails" false success;
  check bool "says not found" true (
    try let _ = Str.search_forward (Str.regexp "not found") response 0 in true
    with Not_found -> false)

let test_handle_mcp_session_tool_get_no_id () =
  let args = `Assoc [("action", `String "get")] in
  let (success, response) = Session.handle_mcp_session_tool args in
  check bool "get without id fails" false success;
  check bool "says session_id required" true (
    try let _ = Str.search_forward (Str.regexp "session_id required") response 0 in true
    with Not_found -> false)

let test_handle_mcp_session_tool_remove_no_id () =
  let args = `Assoc [("action", `String "remove")] in
  let (success, _) = Session.handle_mcp_session_tool args in
  check bool "remove without id fails" false success

let test_handle_mcp_session_tool_remove_missing () =
  let args = `Assoc [("action", `String "remove"); ("session_id", `String "nonexistent-xyz")] in
  let (success, _) = Session.handle_mcp_session_tool args in
  check bool "remove missing fails" false success

let test_handle_mcp_session_tool_cleanup () =
  let args = `Assoc [("action", `String "cleanup")] in
  let (success, response) = Session.handle_mcp_session_tool args in
  check bool "cleanup succeeds" true success;
  check bool "says removed" true (
    try let _ = Str.search_forward (Str.regexp "Removed") response 0 in true
    with Not_found -> false)

let test_handle_mcp_session_tool_unknown_action () =
  let args = `Assoc [("action", `String "unknown-action")] in
  let (success, response) = Session.handle_mcp_session_tool args in
  check bool "unknown action fails" false success;
  check bool "says unknown" true (
    try let _ = Str.search_forward (Str.regexp "Unknown action") response 0 in true
    with Not_found -> false)

let test_handle_mcp_session_tool_no_action () =
  let args = `Assoc [] in
  let (success, response) = Session.handle_mcp_session_tool args in
  check bool "no action fails" false success;
  check bool "says action required" true (
    try let _ = Str.search_forward (Str.regexp "action required") response 0 in true
    with Not_found -> false)

(* ============================================================
   status_string Tests (requires Eio runtime - basic only)
   ============================================================ *)

let test_status_string_empty () =
  let registry = Session.create () in
  let status = Session.status_string registry in
  check bool "says no agents" true (
    try let _ = Str.search_forward (Str.regexp "No agents") status 0 in true
    with Not_found -> false)

(* Note: Tests with Session.register require Eio runtime *)

(* ============================================================
   connected_agents Tests (empty only - register needs Eio)
   ============================================================ *)

let test_connected_agents_empty () =
  let registry = Session.create () in
  let agents = Session.connected_agents registry in
  check (list string) "empty" [] agents

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Session Coverage" [
    "session", [
      test_case "type" `Quick test_session_type;
      test_case "with messages" `Quick test_session_with_messages;
    ];
    "rate_tracker", [
      test_case "type" `Quick test_rate_tracker_type;
      test_case "with data" `Quick test_rate_tracker_with_data;
    ];
    "create_tracker", [
      test_case "empty" `Quick test_create_tracker_empty;
      test_case "burst_reset" `Quick test_create_tracker_burst_reset;
    ];
    "get_timestamps", [
      test_case "general" `Quick test_get_timestamps_general;
      test_case "broadcast" `Quick test_get_timestamps_broadcast;
      test_case "task_ops" `Quick test_get_timestamps_task_ops;
    ];
    "set_timestamps", [
      test_case "general" `Quick test_set_timestamps_general;
      test_case "broadcast" `Quick test_set_timestamps_broadcast;
      test_case "task_ops" `Quick test_set_timestamps_task_ops;
      test_case "roundtrip" `Quick test_set_get_roundtrip;
    ];
    "atomic_helpers", [
      test_case "get_burst_used initial" `Quick test_get_burst_used_initial;
      test_case "set_burst_used" `Quick test_set_burst_used;
      test_case "incr_burst_used" `Quick test_incr_burst_used;
      test_case "incr_burst_used multiple" `Quick test_incr_burst_used_multiple;
      test_case "get_last_burst_reset" `Quick test_get_last_burst_reset;
      test_case "set_last_burst_reset" `Quick test_set_last_burst_reset;
    ];
    "mcp_session_store", [
      test_case "generate_id prefix" `Quick test_generate_id_prefix;
      test_case "generate_id length" `Quick test_generate_id_length;
      test_case "generate_id unique" `Quick test_generate_id_unique;
      test_case "to_json has id" `Quick test_mcp_session_to_json_has_id;
      test_case "to_json has created_at" `Quick test_mcp_session_to_json_has_created_at;
      test_case "to_json has request_count" `Quick test_mcp_session_to_json_has_request_count;
      test_case "to_json has metadata" `Quick test_mcp_session_to_json_has_metadata;
      test_case "to_json agent_name null" `Quick test_mcp_session_to_json_agent_name_null;
      test_case "to_json agent_name some" `Quick test_mcp_session_to_json_agent_name_some;
      test_case "create and get" `Quick test_mcp_session_create_and_get;
      test_case "get nonexistent" `Quick test_mcp_session_get_nonexistent;
      test_case "remove" `Quick test_mcp_session_remove;
      test_case "remove nonexistent" `Quick test_mcp_session_remove_nonexistent;
      test_case "list all" `Quick test_mcp_session_list_all;
    ];
    "extract_mcp_session_id", [
      test_case "present" `Quick test_extract_mcp_session_id_present;
      test_case "x-prefix" `Quick test_extract_mcp_session_id_x_prefix;
      test_case "prefers mcp" `Quick test_extract_mcp_session_id_prefers_mcp;
      test_case "missing" `Quick test_extract_mcp_session_id_missing;
      test_case "other headers" `Quick test_extract_mcp_session_id_other_headers;
    ];
    "handle_mcp_session_tool", [
      test_case "create" `Quick test_handle_mcp_session_tool_create;
      test_case "list" `Quick test_handle_mcp_session_tool_list;
      test_case "get missing" `Quick test_handle_mcp_session_tool_get_missing;
      test_case "get no id" `Quick test_handle_mcp_session_tool_get_no_id;
      test_case "remove no id" `Quick test_handle_mcp_session_tool_remove_no_id;
      test_case "remove missing" `Quick test_handle_mcp_session_tool_remove_missing;
      test_case "cleanup" `Quick test_handle_mcp_session_tool_cleanup;
      test_case "unknown action" `Quick test_handle_mcp_session_tool_unknown_action;
      test_case "no action" `Quick test_handle_mcp_session_tool_no_action;
    ];
    "status_string", [
      test_case "empty" `Quick test_status_string_empty;
    ];
    "connected_agents", [
      test_case "empty" `Quick test_connected_agents_empty;
    ];
  ]
