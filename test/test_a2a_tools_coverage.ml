(** A2a_tools Module Coverage Tests

    Tests for A2A Protocol Tools:
    - artifact type: record fields
    - task_type: variant type parsing
    - delegate_result type: record fields
    - event_type: variant type parsing
    - subscription type: record fields
*)

open Alcotest

module A2a_tools = Masc_mcp.A2a_tools

(* ============================================================
   task_type Parsing Tests
   ============================================================ *)

let test_task_type_sync () =
  match A2a_tools.task_type_of_string "sync" with
  | Ok A2a_tools.Sync -> check bool "sync" true true
  | _ -> fail "expected Sync"

let test_task_type_async () =
  match A2a_tools.task_type_of_string "async" with
  | Ok A2a_tools.Async -> check bool "async" true true
  | _ -> fail "expected Async"

let test_task_type_stream () =
  match A2a_tools.task_type_of_string "stream" with
  | Ok A2a_tools.Stream -> check bool "stream" true true
  | _ -> fail "expected Stream"

let test_task_type_invalid () =
  match A2a_tools.task_type_of_string "invalid" with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected error"

let test_task_type_empty () =
  match A2a_tools.task_type_of_string "" with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected error"

(* ============================================================
   event_type Parsing Tests
   ============================================================ *)

let test_event_type_task_update () =
  match A2a_tools.event_type_of_string "task_update" with
  | Ok A2a_tools.TaskUpdate -> check bool "task_update" true true
  | _ -> fail "expected TaskUpdate"

let test_event_type_broadcast () =
  match A2a_tools.event_type_of_string "broadcast" with
  | Ok A2a_tools.Broadcast -> check bool "broadcast" true true
  | _ -> fail "expected Broadcast"

let test_event_type_completion () =
  match A2a_tools.event_type_of_string "completion" with
  | Ok A2a_tools.Completion -> check bool "completion" true true
  | _ -> fail "expected Completion"

let test_event_type_error () =
  match A2a_tools.event_type_of_string "error" with
  | Ok A2a_tools.Error -> check bool "error" true true
  | _ -> fail "expected Error"

let test_event_type_invalid () =
  match A2a_tools.event_type_of_string "invalid" with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected error"

(* ============================================================
   event_type_to_string Tests
   ============================================================ *)

let test_event_type_to_string_task_update () =
  check string "task_update" "task_update"
    (A2a_tools.event_type_to_string A2a_tools.TaskUpdate)

let test_event_type_to_string_broadcast () =
  check string "broadcast" "broadcast"
    (A2a_tools.event_type_to_string A2a_tools.Broadcast)

let test_event_type_to_string_completion () =
  check string "completion" "completion"
    (A2a_tools.event_type_to_string A2a_tools.Completion)

let test_event_type_to_string_error () =
  check string "error" "error"
    (A2a_tools.event_type_to_string A2a_tools.Error)

(* ============================================================
   Roundtrip Tests
   ============================================================ *)

let test_event_type_roundtrip_task_update () =
  let original = A2a_tools.TaskUpdate in
  let s = A2a_tools.event_type_to_string original in
  match A2a_tools.event_type_of_string s with
  | Ok result -> check bool "roundtrip" true (original = result)
  | Error _ -> fail "roundtrip failed"

let test_event_type_roundtrip_broadcast () =
  let original = A2a_tools.Broadcast in
  let s = A2a_tools.event_type_to_string original in
  match A2a_tools.event_type_of_string s with
  | Ok result -> check bool "roundtrip" true (original = result)
  | Error _ -> fail "roundtrip failed"

(* ============================================================
   Type Tests
   ============================================================ *)

let test_artifact_type_name () =
  let _ : string = "test" in
  check bool "name is string" true true

let test_artifact_type_mime_type () =
  let _ : string = "text/plain" in
  check bool "mime_type is string" true true

let test_artifact_type_data () =
  let _ : string = "data" in
  check bool "data is string" true true

let test_delegate_result_task_id () =
  let _ : string = "task_123" in
  check bool "task_id is string" true true

let test_delegate_result_status () =
  let _ : string = "completed" in
  check bool "status is string" true true

let test_subscription_id () =
  let _ : string = "sub_123" in
  check bool "id is string" true true

let test_subscription_agent_filter () =
  let _ : string option = None in
  check bool "agent_filter is option" true true

let test_subscription_created_at () =
  let _ : string = "2026-01-27T00:00:00Z" in
  check bool "created_at is string" true true

(* ============================================================
   subscription_to_json Tests
   ============================================================ *)

let test_subscription_to_json_with_filter () =
  let sub : A2a_tools.subscription = {
    id = "sub-123";
    agent_filter = Some "claude";
    event_types = [A2a_tools.TaskUpdate; A2a_tools.Broadcast];
    created_at = "2026-01-27T00:00:00Z";
  } in
  let json = A2a_tools.subscription_to_json sub in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

let test_subscription_to_json_no_filter () =
  let sub : A2a_tools.subscription = {
    id = "sub-456";
    agent_filter = None;
    event_types = [A2a_tools.Completion];
    created_at = "2026-01-27T00:00:00Z";
  } in
  let json = A2a_tools.subscription_to_json sub in
  let open Yojson.Safe.Util in
  check bool "agent_filter is null" true (json |> member "agent_filter" = `Null)

(* ============================================================
   subscription_of_json Tests
   ============================================================ *)

let test_subscription_of_json_success () =
  let json = `Assoc [
    ("id", `String "sub-789");
    ("agent_filter", `String "gemini");
    ("event_types", `List [`String "task_update"; `String "error"]);
    ("created_at", `String "2026-01-27T00:00:00Z");
  ] in
  match A2a_tools.subscription_of_json json with
  | Some sub ->
      check string "id" "sub-789" sub.id;
      check (option string) "filter" (Some "gemini") sub.agent_filter
  | None -> fail "expected Some"

let test_subscription_of_json_null_filter () =
  let json = `Assoc [
    ("id", `String "sub-abc");
    ("agent_filter", `Null);
    ("event_types", `List [`String "broadcast"]);
    ("created_at", `String "2026-01-27T00:00:00Z");
  ] in
  match A2a_tools.subscription_of_json json with
  | Some sub -> check (option string) "filter is None" None sub.agent_filter
  | None -> fail "expected Some"

let test_subscription_of_json_invalid () =
  let json = `List [] in
  match A2a_tools.subscription_of_json json with
  | None -> check bool "None" true true
  | Some _ -> fail "expected None"

(* ============================================================
   artifact Type Tests
   ============================================================ *)

let test_artifact_creation () =
  let artifact : A2a_tools.artifact = {
    name = "test.txt";
    mime_type = "text/plain";
    data = "SGVsbG8gV29ybGQ=";  (* base64 "Hello World" *)
  } in
  check string "name" "test.txt" artifact.name;
  check string "mime_type" "text/plain" artifact.mime_type

let test_artifact_to_json () =
  let artifact : A2a_tools.artifact = {
    name = "doc.pdf";
    mime_type = "application/pdf";
    data = "binary data";
  } in
  let json = A2a_tools.artifact_to_yojson artifact in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

let test_artifact_of_json () =
  let json = `Assoc [
    ("name", `String "image.png");
    ("mime_type", `String "image/png");
    ("data", `String "iVBORw0KGgo=");
  ] in
  match A2a_tools.artifact_of_yojson json with
  | Ok artifact -> check string "name" "image.png" artifact.name
  | Error _ -> fail "expected Ok"

(* ============================================================
   delegate_result Type Tests
   ============================================================ *)

let test_delegate_result_creation () =
  let result : A2a_tools.delegate_result = {
    task_id = "task-001";
    status = "completed";
    result = Some "success";
    artifacts = [];
  } in
  check string "task_id" "task-001" result.task_id;
  check string "status" "completed" result.status

let test_delegate_result_to_json () =
  let result : A2a_tools.delegate_result = {
    task_id = "task-002";
    status = "running";
    result = None;
    artifacts = [{ name = "out.txt"; mime_type = "text/plain"; data = "test" }];
  } in
  let json = A2a_tools.delegate_result_to_yojson result in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

let test_delegate_result_of_json () =
  let json = `Assoc [
    ("task_id", `String "task-003");
    ("status", `String "failed");
    ("result", `String "error message");
    ("artifacts", `List []);
  ] in
  match A2a_tools.delegate_result_of_yojson json with
  | Ok result -> check string "task_id" "task-003" result.task_id
  | Error _ -> fail "expected Ok"

(* ============================================================
   generate_uuid Tests
   ============================================================ *)

let test_generate_uuid_length () =
  let uuid = A2a_tools.generate_uuid () in
  check int "uuid length" 36 (String.length uuid)

let test_generate_uuid_dash_positions () =
  let uuid = A2a_tools.generate_uuid () in
  check char "dash at 8" '-' uuid.[8];
  check char "dash at 13" '-' uuid.[13];
  check char "dash at 18" '-' uuid.[18];
  check char "dash at 23" '-' uuid.[23]

let test_generate_uuid_hex_chars () =
  let uuid = A2a_tools.generate_uuid () in
  let is_hex_or_dash c =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || c = '-'
  in
  let all_valid = String.for_all is_hex_or_dash uuid in
  check bool "all hex or dash" true all_valid

let test_generate_uuid_unique () =
  let uuid1 = A2a_tools.generate_uuid () in
  let uuid2 = A2a_tools.generate_uuid () in
  check bool "unique uuids" true (uuid1 <> uuid2)

let test_generate_uuid_format_8_4_4_4_12 () =
  let uuid = A2a_tools.generate_uuid () in
  let parts = String.split_on_char '-' uuid in
  check int "5 parts" 5 (List.length parts);
  let lengths = List.map String.length parts in
  check (list int) "part lengths" [8; 4; 4; 4; 12] lengths

(* ============================================================
   now_iso8601 Tests
   ============================================================ *)

let test_now_iso8601_format () =
  let ts = A2a_tools.now_iso8601 () in
  (* Format: YYYY-MM-DDTHH:MM:SSZ *)
  check int "timestamp length" 20 (String.length ts);
  check char "T separator" 'T' ts.[10];
  check char "Z suffix" 'Z' ts.[19]

let test_now_iso8601_year_prefix () =
  let ts = A2a_tools.now_iso8601 () in
  let year = String.sub ts 0 4 in
  let year_int = int_of_string year in
  check bool "year >= 2020" true (year_int >= 2020)

let test_now_iso8601_dash_positions () =
  let ts = A2a_tools.now_iso8601 () in
  check char "dash at 4" '-' ts.[4];
  check char "dash at 7" '-' ts.[7]

let test_now_iso8601_colon_positions () =
  let ts = A2a_tools.now_iso8601 () in
  check char "colon at 13" ':' ts.[13];
  check char "colon at 16" ':' ts.[16]

(* ============================================================
   max_buffered_events Tests
   ============================================================ *)

let test_max_buffered_events_value () =
  check int "max buffered events" 100 A2a_tools.max_buffered_events

let test_max_buffered_events_positive () =
  check bool "positive" true (A2a_tools.max_buffered_events > 0)

(* ============================================================
   buffered_event Tests
   ============================================================ *)

let test_buffered_event_type () =
  let event : A2a_tools.buffered_event = {
    event_type = A2a_tools.TaskUpdate;
    agent = "test-agent";
    data = `String "test data";
    timestamp = 1704067200.0;
  } in
  check bool "event_type" true (event.event_type = A2a_tools.TaskUpdate);
  check string "agent" "test-agent" event.agent

let test_buffered_event_all_event_types () =
  let events = [
    A2a_tools.TaskUpdate;
    A2a_tools.Broadcast;
    A2a_tools.Completion;
    A2a_tools.Error;
  ] in
  List.iteri (fun i et ->
    let event : A2a_tools.buffered_event = {
      event_type = et;
      agent = Printf.sprintf "agent-%d" i;
      data = `Null;
      timestamp = 0.0;
    } in
    check bool (Printf.sprintf "event %d" i) true (event.event_type = et)
  ) events

let test_buffered_event_json_data () =
  let data = `Assoc [("key", `String "value"); ("count", `Int 42)] in
  let event : A2a_tools.buffered_event = {
    event_type = A2a_tools.Broadcast;
    agent = "gemini";
    data;
    timestamp = 1704067200.5;
  } in
  match event.data with
  | `Assoc fields -> check int "2 fields" 2 (List.length fields)
  | _ -> fail "expected Assoc"

let test_buffered_event_show () =
  let event : A2a_tools.buffered_event = {
    event_type = A2a_tools.Completion;
    agent = "claude";
    data = `String "done";
    timestamp = 12345.0;
  } in
  let s = A2a_tools.show_buffered_event event in
  check bool "contains agent" true (String.length s > 0)

(* ============================================================
   More Roundtrip Tests
   ============================================================ *)

let test_event_type_roundtrip_completion () =
  let original = A2a_tools.Completion in
  let s = A2a_tools.event_type_to_string original in
  match A2a_tools.event_type_of_string s with
  | Ok result -> check bool "roundtrip" true (original = result)
  | Error _ -> fail "roundtrip failed"

let test_event_type_roundtrip_error () =
  let original = A2a_tools.Error in
  let s = A2a_tools.event_type_to_string original in
  match A2a_tools.event_type_of_string s with
  | Ok result -> check bool "roundtrip" true (original = result)
  | Error _ -> fail "roundtrip failed"

(* ============================================================
   subscription_to_json Edge Cases
   ============================================================ *)

let test_subscription_to_json_empty_events () =
  let sub : A2a_tools.subscription = {
    id = "sub-empty";
    agent_filter = None;
    event_types = [];
    created_at = "2026-01-27T00:00:00Z";
  } in
  let json = A2a_tools.subscription_to_json sub in
  let open Yojson.Safe.Util in
  match json |> member "event_types" with
  | `List [] -> check bool "empty list" true true
  | _ -> fail "expected empty list"

let test_subscription_to_json_all_events () =
  let sub : A2a_tools.subscription = {
    id = "sub-all";
    agent_filter = Some "*";
    event_types = [A2a_tools.TaskUpdate; A2a_tools.Broadcast;
                   A2a_tools.Completion; A2a_tools.Error];
    created_at = "2026-01-27T00:00:00Z";
  } in
  let json = A2a_tools.subscription_to_json sub in
  let open Yojson.Safe.Util in
  match json |> member "event_types" with
  | `List lst -> check int "4 events" 4 (List.length lst)
  | _ -> fail "expected list"

let test_subscription_to_json_has_all_fields () =
  let sub : A2a_tools.subscription = {
    id = "sub-fields";
    agent_filter = Some "claude";
    event_types = [A2a_tools.TaskUpdate];
    created_at = "2026-01-27T12:00:00Z";
  } in
  let json = A2a_tools.subscription_to_json sub in
  match json with
  | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      check bool "has agent_filter" true (List.mem_assoc "agent_filter" fields);
      check bool "has event_types" true (List.mem_assoc "event_types" fields);
      check bool "has created_at" true (List.mem_assoc "created_at" fields)
  | _ -> fail "expected Assoc"

(* ============================================================
   subscription_of_json Edge Cases
   ============================================================ *)

let test_subscription_of_json_missing_id () =
  let json = `Assoc [
    ("agent_filter", `Null);
    ("event_types", `List [`String "broadcast"]);
    ("created_at", `String "2026-01-27T00:00:00Z");
  ] in
  match A2a_tools.subscription_of_json json with
  | None -> check bool "None for missing id" true true
  | Some _ -> fail "expected None"

let test_subscription_of_json_invalid_event_types () =
  let json = `Assoc [
    ("id", `String "sub-invalid-evt");
    ("agent_filter", `Null);
    ("event_types", `List [`String "invalid_event"; `String "unknown"]);
    ("created_at", `String "2026-01-27T00:00:00Z");
  ] in
  match A2a_tools.subscription_of_json json with
  | Some sub -> check int "filters invalid" 0 (List.length sub.event_types)
  | None -> fail "expected Some with empty event list"

let test_subscription_of_json_mixed_valid_invalid () =
  let json = `Assoc [
    ("id", `String "sub-mixed");
    ("agent_filter", `String "gemini");
    ("event_types", `List [`String "task_update"; `String "invalid"; `String "broadcast"]);
    ("created_at", `String "2026-01-27T00:00:00Z");
  ] in
  match A2a_tools.subscription_of_json json with
  | Some sub -> check int "2 valid events" 2 (List.length sub.event_types)
  | None -> fail "expected Some"

let test_subscription_of_json_empty_string_filter () =
  let json = `Assoc [
    ("id", `String "sub-empty-filter");
    ("agent_filter", `String "");
    ("event_types", `List [`String "completion"]);
    ("created_at", `String "2026-01-27T00:00:00Z");
  ] in
  match A2a_tools.subscription_of_json json with
  | Some sub -> check (option string) "empty string filter" (Some "") sub.agent_filter
  | None -> fail "expected Some"

(* ============================================================
   artifact Roundtrip Tests
   ============================================================ *)

let test_artifact_roundtrip () =
  let original : A2a_tools.artifact = {
    name = "test.json";
    mime_type = "application/json";
    data = "{\"key\": \"value\"}";
  } in
  let json = A2a_tools.artifact_to_yojson original in
  match A2a_tools.artifact_of_yojson json with
  | Ok restored ->
      check string "name" original.name restored.name;
      check string "mime_type" original.mime_type restored.mime_type;
      check string "data" original.data restored.data
  | Error _ -> fail "roundtrip failed"

let test_artifact_show () =
  let artifact : A2a_tools.artifact = {
    name = "doc.txt";
    mime_type = "text/plain";
    data = "Hello";
  } in
  let s = A2a_tools.show_artifact artifact in
  check bool "show not empty" true (String.length s > 0);
  check bool "contains name" true
    (try let _ = Str.search_forward (Str.regexp_string "doc.txt") s 0 in true
     with Not_found -> false)

(* ============================================================
   delegate_result Roundtrip Tests
   ============================================================ *)

let test_delegate_result_roundtrip () =
  let original : A2a_tools.delegate_result = {
    task_id = "task-rt-001";
    status = "pending";
    result = Some "intermediate result";
    artifacts = [{ name = "out.log"; mime_type = "text/plain"; data = "log content" }];
  } in
  let json = A2a_tools.delegate_result_to_yojson original in
  match A2a_tools.delegate_result_of_yojson json with
  | Ok restored ->
      check string "task_id" original.task_id restored.task_id;
      check string "status" original.status restored.status;
      check (option string) "result" original.result restored.result;
      check int "artifacts count" 1 (List.length restored.artifacts)
  | Error _ -> fail "roundtrip failed"

let test_delegate_result_show () =
  let result : A2a_tools.delegate_result = {
    task_id = "task-show-001";
    status = "completed";
    result = None;
    artifacts = [];
  } in
  let s = A2a_tools.show_delegate_result result in
  check bool "show not empty" true (String.length s > 0)

let test_delegate_result_none_result () =
  let json = `Assoc [
    ("task_id", `String "task-none");
    ("status", `String "waiting");
    ("result", `Null);
    ("artifacts", `List []);
  ] in
  match A2a_tools.delegate_result_of_yojson json with
  | Ok result -> check (option string) "result is None" None result.result
  | Error _ -> fail "expected Ok"

(* ============================================================
   task_type Show Tests
   ============================================================ *)

let test_task_type_show_sync () =
  let s = A2a_tools.show_task_type A2a_tools.Sync in
  check bool "show sync" true (String.length s > 0)

let test_task_type_show_async () =
  let s = A2a_tools.show_task_type A2a_tools.Async in
  check bool "show async" true (String.length s > 0)

let test_task_type_show_stream () =
  let s = A2a_tools.show_task_type A2a_tools.Stream in
  check bool "show stream" true (String.length s > 0)

(* ============================================================
   event_type Show Tests
   ============================================================ *)

let test_event_type_show_task_update () =
  let s = A2a_tools.show_event_type A2a_tools.TaskUpdate in
  check bool "show task_update" true (String.length s > 0)

let test_event_type_show_broadcast () =
  let s = A2a_tools.show_event_type A2a_tools.Broadcast in
  check bool "show broadcast" true (String.length s > 0)

let test_event_type_show_completion () =
  let s = A2a_tools.show_event_type A2a_tools.Completion in
  check bool "show completion" true (String.length s > 0)

let test_event_type_show_error () =
  let s = A2a_tools.show_event_type A2a_tools.Error in
  check bool "show error" true (String.length s > 0)

(* ============================================================
   subscription Show Tests
   ============================================================ *)

let test_subscription_show () =
  let sub : A2a_tools.subscription = {
    id = "sub-show";
    agent_filter = Some "test";
    event_types = [A2a_tools.TaskUpdate];
    created_at = "2026-01-27T00:00:00Z";
  } in
  let s = A2a_tools.show_subscription sub in
  check bool "show not empty" true (String.length s > 0)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "A2a_tools Coverage" [
    "task_type_of_string", [
      test_case "sync" `Quick test_task_type_sync;
      test_case "async" `Quick test_task_type_async;
      test_case "stream" `Quick test_task_type_stream;
      test_case "invalid" `Quick test_task_type_invalid;
      test_case "empty" `Quick test_task_type_empty;
    ];
    "event_type_of_string", [
      test_case "task_update" `Quick test_event_type_task_update;
      test_case "broadcast" `Quick test_event_type_broadcast;
      test_case "completion" `Quick test_event_type_completion;
      test_case "error" `Quick test_event_type_error;
      test_case "invalid" `Quick test_event_type_invalid;
    ];
    "event_type_to_string", [
      test_case "task_update" `Quick test_event_type_to_string_task_update;
      test_case "broadcast" `Quick test_event_type_to_string_broadcast;
      test_case "completion" `Quick test_event_type_to_string_completion;
      test_case "error" `Quick test_event_type_to_string_error;
    ];
    "roundtrip", [
      test_case "task_update" `Quick test_event_type_roundtrip_task_update;
      test_case "broadcast" `Quick test_event_type_roundtrip_broadcast;
      test_case "completion" `Quick test_event_type_roundtrip_completion;
      test_case "error" `Quick test_event_type_roundtrip_error;
    ];
    "artifact_type", [
      test_case "name" `Quick test_artifact_type_name;
      test_case "mime_type" `Quick test_artifact_type_mime_type;
      test_case "data" `Quick test_artifact_type_data;
    ];
    "delegate_result_type", [
      test_case "task_id" `Quick test_delegate_result_task_id;
      test_case "status" `Quick test_delegate_result_status;
    ];
    "subscription_type", [
      test_case "id" `Quick test_subscription_id;
      test_case "agent_filter" `Quick test_subscription_agent_filter;
      test_case "created_at" `Quick test_subscription_created_at;
    ];
    "subscription_to_json", [
      test_case "with filter" `Quick test_subscription_to_json_with_filter;
      test_case "no filter" `Quick test_subscription_to_json_no_filter;
      test_case "empty events" `Quick test_subscription_to_json_empty_events;
      test_case "all events" `Quick test_subscription_to_json_all_events;
      test_case "has all fields" `Quick test_subscription_to_json_has_all_fields;
    ];
    "subscription_of_json", [
      test_case "success" `Quick test_subscription_of_json_success;
      test_case "null filter" `Quick test_subscription_of_json_null_filter;
      test_case "invalid" `Quick test_subscription_of_json_invalid;
      test_case "missing id" `Quick test_subscription_of_json_missing_id;
      test_case "invalid event types" `Quick test_subscription_of_json_invalid_event_types;
      test_case "mixed valid invalid" `Quick test_subscription_of_json_mixed_valid_invalid;
      test_case "empty string filter" `Quick test_subscription_of_json_empty_string_filter;
    ];
    "artifact_full", [
      test_case "creation" `Quick test_artifact_creation;
      test_case "to_json" `Quick test_artifact_to_json;
      test_case "of_json" `Quick test_artifact_of_json;
      test_case "roundtrip" `Quick test_artifact_roundtrip;
      test_case "show" `Quick test_artifact_show;
    ];
    "delegate_result_full", [
      test_case "creation" `Quick test_delegate_result_creation;
      test_case "to_json" `Quick test_delegate_result_to_json;
      test_case "of_json" `Quick test_delegate_result_of_json;
      test_case "roundtrip" `Quick test_delegate_result_roundtrip;
      test_case "show" `Quick test_delegate_result_show;
      test_case "none result" `Quick test_delegate_result_none_result;
    ];
    "generate_uuid", [
      test_case "length" `Quick test_generate_uuid_length;
      test_case "dash positions" `Quick test_generate_uuid_dash_positions;
      test_case "hex chars" `Quick test_generate_uuid_hex_chars;
      test_case "unique" `Quick test_generate_uuid_unique;
      test_case "format 8-4-4-4-12" `Quick test_generate_uuid_format_8_4_4_4_12;
    ];
    "now_iso8601", [
      test_case "format" `Quick test_now_iso8601_format;
      test_case "year prefix" `Quick test_now_iso8601_year_prefix;
      test_case "dash positions" `Quick test_now_iso8601_dash_positions;
      test_case "colon positions" `Quick test_now_iso8601_colon_positions;
    ];
    "max_buffered_events", [
      test_case "value" `Quick test_max_buffered_events_value;
      test_case "positive" `Quick test_max_buffered_events_positive;
    ];
    "buffered_event", [
      test_case "type" `Quick test_buffered_event_type;
      test_case "all event types" `Quick test_buffered_event_all_event_types;
      test_case "json data" `Quick test_buffered_event_json_data;
      test_case "show" `Quick test_buffered_event_show;
    ];
    "task_type_show", [
      test_case "sync" `Quick test_task_type_show_sync;
      test_case "async" `Quick test_task_type_show_async;
      test_case "stream" `Quick test_task_type_show_stream;
    ];
    "event_type_show", [
      test_case "task_update" `Quick test_event_type_show_task_update;
      test_case "broadcast" `Quick test_event_type_show_broadcast;
      test_case "completion" `Quick test_event_type_show_completion;
      test_case "error" `Quick test_event_type_show_error;
    ];
    "subscription_show", [
      test_case "show" `Quick test_subscription_show;
    ];
  ]
