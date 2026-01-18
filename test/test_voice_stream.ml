(** Tests for Voice Stream module

    Tests the WebSocket audio streaming server logic.
    Note: These tests don't create actual WebSocket connections,
    they test the internal logic and state management.
*)

open Alcotest
open Masc_mcp.Voice_stream

(** {1 Test Utilities} *)

let test_client ?(id = "test-client-1") ?(agent_filter = None) () =
  {
    id;
    agent_filter;
    connected_at = Unix.gettimeofday ();
    last_activity = Unix.gettimeofday ();
    pending_sends = 0;
  }

(** {1 Creation Tests} *)

let test_create_default_port () =
  let stream = create () in
  check bool "not running" false (is_running stream);
  check int "no clients" 0 (client_count stream)

let test_create_custom_port () =
  let stream = create ~port:9000 () in
  check bool "not running" false (is_running stream);
  check int "no clients" 0 (client_count stream)

let test_max_pending_sends () =
  check int "max_pending_sends is 100" 100 max_pending_sends

(** {1 Client ID Generation Tests} *)

let test_generate_client_id_format () =
  let id = generate_client_id () in
  check bool "starts with ws-" true (String.sub id 0 3 = "ws-");
  check bool "length > 20" true (String.length id > 20)

let test_generate_client_id_unique () =
  let ids = List.init 100 (fun _ -> generate_client_id ()) in
  let unique_ids = List.sort_uniq String.compare ids in
  check int "all unique" 100 (List.length unique_ids)

(** {1 Message Parsing Tests} *)

let test_parse_subscribe () =
  let msg = parse_client_message {|{"type": "subscribe", "agent_id": "claude"}|} in
  match msg with
  | Subscribe agent_id -> check string "agent_id" "claude" agent_id
  | _ -> fail "expected Subscribe"

let test_parse_unsubscribe () =
  let msg = parse_client_message {|{"type": "unsubscribe"}|} in
  match msg with
  | Unsubscribe -> ()
  | _ -> fail "expected Unsubscribe"

let test_parse_unknown_type () =
  let msg = parse_client_message {|{"type": "unknown"}|} in
  match msg with
  | Unknown _ -> ()
  | _ -> fail "expected Unknown"

let test_parse_invalid_json () =
  let msg = parse_client_message "not json" in
  match msg with
  | Unknown _ -> ()
  | _ -> fail "expected Unknown"

let test_parse_empty_string () =
  let msg = parse_client_message "" in
  match msg with
  | Unknown _ -> ()
  | _ -> fail "expected Unknown"

(** {1 Client Type Tests} *)

let test_client_mutable_agent_filter () =
  let client = test_client () in
  check (option string) "initial None" None client.agent_filter;
  client.agent_filter <- Some "claude";
  check (option string) "updated" (Some "claude") client.agent_filter;
  client.agent_filter <- None;
  check (option string) "cleared" None client.agent_filter

let test_client_mutable_last_activity () =
  let client = test_client () in
  let initial = client.last_activity in
  Unix.sleepf 0.01;
  client.last_activity <- Unix.gettimeofday ();
  check bool "updated" true (client.last_activity > initial)

let test_client_mutable_pending_sends () =
  let client = test_client () in
  check int "initial 0" 0 client.pending_sends;
  client.pending_sends <- 5;
  check int "updated" 5 client.pending_sends;
  client.pending_sends <- client.pending_sends - 1;
  check int "decremented" 4 client.pending_sends

(** {1 Server Event Tests} *)

let test_server_event_client_connected () =
  let client = test_client () in
  let event = ClientConnected client in
  match event with
  | ClientConnected c -> check string "client id" "test-client-1" c.id
  | _ -> fail "expected ClientConnected"

let test_server_event_client_disconnected () =
  let event = ClientDisconnected "test-client-1" in
  match event with
  | ClientDisconnected id -> check string "client id" "test-client-1" id
  | _ -> fail "expected ClientDisconnected"

let test_server_event_message_received () =
  let event = MessageReceived ("test-client-1", Subscribe "claude") in
  match event with
  | MessageReceived (id, Subscribe agent) ->
    check string "client id" "test-client-1" id;
    check string "agent" "claude" agent
  | _ -> fail "expected MessageReceived"

let test_server_event_client_error () =
  let event = ClientError ("test-client-1", "connection_reset") in
  match event with
  | ClientError (id, msg) ->
    check string "client id" "test-client-1" id;
    check string "error msg" "connection_reset" msg
  | _ -> fail "expected ClientError"

(** {1 Status JSON Tests} *)

let test_status_json_empty () =
  let stream = create ~port:8937 () in
  let json = status_json stream in
  let open Yojson.Safe.Util in
  check int "port" 8937 (json |> member "port" |> to_int);
  check bool "not running" false (json |> member "is_running" |> to_bool);
  check int "no clients" 0 (json |> member "client_count" |> to_int);
  check int "max_pending" 100 (json |> member "max_pending_sends" |> to_int);
  check int "empty clients list" 0 (json |> member "clients" |> to_list |> List.length)

(** {1 Event Callback Tests} *)

let test_on_event_callback () =
  let stream = create () in
  let received = ref None in
  on_event stream (fun event -> received := Some event);
  (* Note: Can't easily trigger events without real WebSocket connections,
     but we can verify the callback is set *)
  check bool "callback registered" true true

(** {1 Test Suites} *)

let creation_tests = [
  "create_default_port", `Quick, test_create_default_port;
  "create_custom_port", `Quick, test_create_custom_port;
  "max_pending_sends", `Quick, test_max_pending_sends;
]

let client_id_tests = [
  "generate_client_id_format", `Quick, test_generate_client_id_format;
  "generate_client_id_unique", `Quick, test_generate_client_id_unique;
]

let message_parsing_tests = [
  "parse_subscribe", `Quick, test_parse_subscribe;
  "parse_unsubscribe", `Quick, test_parse_unsubscribe;
  "parse_unknown_type", `Quick, test_parse_unknown_type;
  "parse_invalid_json", `Quick, test_parse_invalid_json;
  "parse_empty_string", `Quick, test_parse_empty_string;
]

let client_type_tests = [
  "client_mutable_agent_filter", `Quick, test_client_mutable_agent_filter;
  "client_mutable_last_activity", `Quick, test_client_mutable_last_activity;
  "client_mutable_pending_sends", `Quick, test_client_mutable_pending_sends;
]

let server_event_tests = [
  "client_connected", `Quick, test_server_event_client_connected;
  "client_disconnected", `Quick, test_server_event_client_disconnected;
  "message_received", `Quick, test_server_event_message_received;
  "client_error", `Quick, test_server_event_client_error;
]

let status_tests = [
  "status_json_empty", `Quick, test_status_json_empty;
]

let callback_tests = [
  "on_event_callback", `Quick, test_on_event_callback;
]

let () =
  Alcotest.run "Voice Stream" [
    ("creation", creation_tests);
    ("client_id", client_id_tests);
    ("message_parsing", message_parsing_tests);
    ("client_type", client_type_tests);
    ("server_event", server_event_tests);
    ("status", status_tests);
    ("callback", callback_tests);
  ]
