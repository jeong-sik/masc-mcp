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
  let json = Yojson.Safe.from_string (get_status_json stream) in
  let open Yojson.Safe.Util in
  check int "port" 8937 (json |> member "port" |> to_int);
  check bool "not running" false (json |> member "running" |> to_bool);
  check int "no clients" 0 (json |> member "client_count" |> to_int);
  check int "max_pending" 100 (json |> member "max_pending_sends" |> to_int);
  check int "empty clients list" 0 (json |> member "clients" |> to_list |> List.length)

(** {1 Event Callback Tests} *)

let test_on_event_callback () =
  let stream = create () in
  let received = ref None in
  on_event stream (fun event -> received := Some event);
  (* Note: Can't trigger events without real WebSocket connections,
     but we can verify callback registration doesn't affect stream state *)
  let status_before = get_status_json stream in
  (* Set another callback to verify multiple registrations work *)
  let count = ref 0 in
  on_event stream (fun _ -> incr count);
  let status_after = get_status_json stream in
  (* Callback registration should not affect observable stream state *)
  check string "stream state unchanged" status_before status_after;
  (* Verify initial callback ref is still None (no events fired) *)
  check bool "no events fired yet" true (Option.is_none !received)

(** {1 Test Suites} *)

let creation_tests = [
  "create_default_port", `Quick, test_create_default_port;
  "create_custom_port", `Quick, test_create_custom_port;
  "max_pending_sends", `Quick, test_max_pending_sends;
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
    ("client_type", client_type_tests);
    ("server_event", server_event_tests);
    ("status", status_tests);
    ("callback", callback_tests);
  ]
