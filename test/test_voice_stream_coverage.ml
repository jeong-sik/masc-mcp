(** Voice Stream Module Coverage Tests

    Tests for voice streaming types and utilities:
    - client type
    - client_message type
    - server_event type
    - generate_client_id function
    - max_pending_sends constant
*)

open Alcotest

module Voice_stream = Masc_mcp.Voice_stream

(* ============================================================
   Constant Tests
   ============================================================ *)

let test_max_pending_sends () =
  check int "max_pending_sends" 100 Voice_stream.max_pending_sends

(* ============================================================
   client Type Tests
   ============================================================ *)

let test_client_type () =
  let c : Voice_stream.client = {
    id = "ws-12345678-abcd1234abcd";
    agent_filter = None;
    connected_at = 1704067200.0;
    last_activity = 1704067200.0;
    pending_sends = 0;
  } in
  check string "id" "ws-12345678-abcd1234abcd" c.id;
  check int "pending_sends" 0 c.pending_sends

let test_client_with_filter () =
  let c : Voice_stream.client = {
    id = "ws-00000000-111122223333";
    agent_filter = Some "claude";
    connected_at = 1704067200.0;
    last_activity = 1704067300.0;
    pending_sends = 5;
  } in
  match c.agent_filter with
  | Some f -> check string "agent_filter" "claude" f
  | None -> fail "expected Some"

(* ============================================================
   client_message Type Tests
   ============================================================ *)

let test_client_message_subscribe () =
  let msg = Voice_stream.Subscribe "gemini" in
  match msg with
  | Voice_stream.Subscribe agent -> check string "subscribe" "gemini" agent
  | _ -> fail "expected Subscribe"

let test_client_message_unsubscribe () =
  let msg = Voice_stream.Unsubscribe in
  match msg with
  | Voice_stream.Unsubscribe -> check bool "unsubscribe" true true
  | _ -> fail "expected Unsubscribe"

let test_client_message_unknown () =
  let msg = Voice_stream.Unknown "invalid json" in
  match msg with
  | Voice_stream.Unknown s -> check string "unknown" "invalid json" s
  | _ -> fail "expected Unknown"

(* ============================================================
   server_event Type Tests
   ============================================================ *)

let test_server_event_connected () =
  let c : Voice_stream.client = {
    id = "ws-test"; agent_filter = None;
    connected_at = 0.0; last_activity = 0.0;
    pending_sends = 0;
  } in
  let ev = Voice_stream.ClientConnected c in
  match ev with
  | Voice_stream.ClientConnected client -> check string "client id" "ws-test" client.id
  | _ -> fail "expected ClientConnected"

let test_server_event_disconnected () =
  let ev = Voice_stream.ClientDisconnected "ws-123" in
  match ev with
  | Voice_stream.ClientDisconnected id -> check string "disconnected id" "ws-123" id
  | _ -> fail "expected ClientDisconnected"

let test_server_event_message () =
  let ev = Voice_stream.MessageReceived ("ws-456", Voice_stream.Subscribe "claude") in
  match ev with
  | Voice_stream.MessageReceived (id, msg) ->
      check string "msg client id" "ws-456" id;
      (match msg with
       | Voice_stream.Subscribe a -> check string "subscribed agent" "claude" a
       | _ -> fail "expected Subscribe msg")
  | _ -> fail "expected MessageReceived"

let test_server_event_error () =
  let ev = Voice_stream.ClientError ("ws-789", "Connection timeout") in
  match ev with
  | Voice_stream.ClientError (id, err) ->
      check string "error client id" "ws-789" id;
      check string "error message" "Connection timeout" err
  | _ -> fail "expected ClientError"

(* ============================================================
   parse_client_message Tests
   ============================================================ *)

let test_parse_subscribe () =
  let json = {|{"type": "subscribe", "agent_id": "claude"}|} in
  match Voice_stream.parse_client_message json with
  | Voice_stream.Subscribe agent -> check string "agent" "claude" agent
  | _ -> fail "expected Subscribe"

let test_parse_unsubscribe () =
  let json = {|{"type": "unsubscribe"}|} in
  match Voice_stream.parse_client_message json with
  | Voice_stream.Unsubscribe -> check bool "ok" true true
  | _ -> fail "expected Unsubscribe"

let test_parse_unknown_type () =
  let json = {|{"type": "unknown_type"}|} in
  match Voice_stream.parse_client_message json with
  | Voice_stream.Unknown _ -> check bool "ok" true true
  | _ -> fail "expected Unknown"

let test_parse_invalid_json () =
  let json = "not valid json" in
  match Voice_stream.parse_client_message json with
  | Voice_stream.Unknown s -> check string "original" json s
  | _ -> fail "expected Unknown"

let test_parse_empty_object () =
  let json = {|{}|} in
  match Voice_stream.parse_client_message json with
  | Voice_stream.Unknown _ -> check bool "ok" true true
  | _ -> fail "expected Unknown"

let test_parse_missing_agent_id () =
  (* subscribe without agent_id should fail *)
  let json = {|{"type": "subscribe"}|} in
  match Voice_stream.parse_client_message json with
  | Voice_stream.Unknown _ -> check bool "fails gracefully" true true
  | Voice_stream.Subscribe _ -> fail "should have failed"
  | _ -> fail "unexpected"

(* ============================================================
   generate_client_id Tests
   ============================================================ *)

let test_generate_client_id_prefix () =
  let id = Voice_stream.generate_client_id () in
  check bool "starts with ws-" true (String.sub id 0 3 = "ws-")

let test_generate_client_id_length () =
  let id = Voice_stream.generate_client_id () in
  (* Format: ws-XXXXXXXX-XXXXXXXXXXXX = 3 + 8 + 1 + 12 = 24 chars *)
  check bool "reasonable length" true (String.length id >= 20)

let test_generate_client_id_uniqueness () =
  let id1 = Voice_stream.generate_client_id () in
  let id2 = Voice_stream.generate_client_id () in
  check bool "different ids" true (id1 <> id2)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Voice Stream Coverage" [
    "constants", [
      test_case "max_pending_sends" `Quick test_max_pending_sends;
    ];
    "client", [
      test_case "type" `Quick test_client_type;
      test_case "with filter" `Quick test_client_with_filter;
    ];
    "client_message", [
      test_case "subscribe" `Quick test_client_message_subscribe;
      test_case "unsubscribe" `Quick test_client_message_unsubscribe;
      test_case "unknown" `Quick test_client_message_unknown;
    ];
    "server_event", [
      test_case "connected" `Quick test_server_event_connected;
      test_case "disconnected" `Quick test_server_event_disconnected;
      test_case "message" `Quick test_server_event_message;
      test_case "error" `Quick test_server_event_error;
    ];
    "parse_client_message", [
      test_case "subscribe" `Quick test_parse_subscribe;
      test_case "unsubscribe" `Quick test_parse_unsubscribe;
      test_case "unknown type" `Quick test_parse_unknown_type;
      test_case "invalid json" `Quick test_parse_invalid_json;
      test_case "empty object" `Quick test_parse_empty_object;
      test_case "missing agent_id" `Quick test_parse_missing_agent_id;
    ];
    "generate_client_id", [
      test_case "prefix" `Quick test_generate_client_id_prefix;
      test_case "length" `Quick test_generate_client_id_length;
      test_case "uniqueness" `Quick test_generate_client_id_uniqueness;
    ];
  ]
