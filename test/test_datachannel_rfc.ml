(** Tests for RFC 8831 DataChannel module *)

open Alcotest
open Masc_mcp.Datachannel

(** {1 Test Utilities} *)

let channel_type_testable = testable (fun fmt ct -> Format.fprintf fmt "%s" (string_of_channel_type ct)) (=)
let channel_state_testable = testable (fun fmt s -> Format.fprintf fmt "%s" (string_of_channel_state s)) (=)
let conn_state_testable = testable (fun fmt s -> Format.fprintf fmt "%s" (string_of_connection_state s)) (=)

(** {1 Channel Type Tests} *)

let test_channel_type_strings () =
  check string "reliable_ordered" "reliable-ordered" (string_of_channel_type Reliable_ordered);
  check string "reliable_unordered" "reliable-unordered" (string_of_channel_type Reliable_unordered);
  check string "unreliable_unordered" "unreliable-unordered" (string_of_channel_type Unreliable_unordered);
  check string "unreliable_ordered" "unreliable-ordered" (string_of_channel_type Unreliable_ordered);
  check string "partial_rexmit" "partial-reliable-rexmit(5)" (string_of_channel_type (Partial_reliable_rexmit 5));
  check string "partial_timed" "partial-reliable-timed(100ms)" (string_of_channel_type (Partial_reliable_timed 100))

let test_is_reliable () =
  check bool "reliable_ordered" true (is_reliable Reliable_ordered);
  check bool "reliable_unordered" true (is_reliable Reliable_unordered);
  check bool "unreliable_unordered" false (is_reliable Unreliable_unordered);
  check bool "unreliable_ordered" false (is_reliable Unreliable_ordered);
  check bool "partial_rexmit" false (is_reliable (Partial_reliable_rexmit 3))

let test_is_ordered () =
  check bool "reliable_ordered" true (is_ordered Reliable_ordered);
  check bool "reliable_unordered" false (is_ordered Reliable_unordered);
  check bool "unreliable_unordered" false (is_ordered Unreliable_unordered);
  check bool "unreliable_ordered" true (is_ordered Unreliable_ordered)

(** {1 State Tests} *)

let test_channel_state_strings () =
  check string "connecting" "connecting" (string_of_channel_state Connecting);
  check string "open" "open" (string_of_channel_state Open);
  check string "closing" "closing" (string_of_channel_state Closing);
  check string "closed" "closed" (string_of_channel_state Closed)

let test_connection_state_strings () =
  check string "connecting" "connecting" (string_of_connection_state Conn_connecting);
  check string "connected" "connected" (string_of_connection_state Conn_connected);
  check string "closed" "closed" (string_of_connection_state Conn_closed)

(** {1 Config Tests} *)

let test_default_config () =
  let c = default_config in
  check int "max_channels" 65535 c.max_channels;
  check int "max_message_size" 262144 c.max_message_size;
  check int "sctp_port" 5000 c.sctp_port;
  check int "remote_sctp_port" 5000 c.remote_sctp_port

(** {1 Connection Tests} *)

let test_create_connection_offerer () =
  let conn = create default_config ~is_offerer:true in
  check conn_state_testable "initial state" Conn_connecting (get_state conn);
  let info = connection_info conn in
  let open Yojson.Safe.Util in
  check bool "is offerer" true (info |> member "isOfferer" |> to_bool);
  check int "next stream id (even)" 0 (info |> member "nextStreamId" |> to_int)

let test_create_connection_answerer () =
  let conn = create default_config ~is_offerer:false in
  let info = connection_info conn in
  let open Yojson.Safe.Util in
  check bool "is not offerer" false (info |> member "isOfferer" |> to_bool);
  check int "next stream id (odd)" 1 (info |> member "nextStreamId" |> to_int)

let test_set_connected () =
  let conn = create default_config ~is_offerer:true in
  set_connected conn;
  check conn_state_testable "connected" Conn_connected (get_state conn)

let test_close_connection () =
  let conn = create default_config ~is_offerer:true in
  close conn;
  check conn_state_testable "closed" Conn_closed (get_state conn)

(** {1 Channel Management Tests} *)

let test_create_channel () =
  let conn = create default_config ~is_offerer:true in
  match create_channel conn ~label:"test" () with
  | Error e -> fail e
  | Ok ch ->
    check string "label" "test" ch.label;
    check string "protocol" "" ch.protocol;
    check channel_type_testable "default type" Reliable_ordered ch.channel_type;
    check int "priority" 0 ch.priority;
    check bool "not negotiated" false ch.negotiated

let test_create_channel_with_options () =
  let conn = create default_config ~is_offerer:true in
  match create_channel conn ~label:"audio" ~protocol:"opus" ~channel_type:Unreliable_unordered ~priority:100 () with
  | Error e -> fail e
  | Ok ch ->
    check string "label" "audio" ch.label;
    check string "protocol" "opus" ch.protocol;
    check channel_type_testable "type" Unreliable_unordered ch.channel_type;
    check int "priority" 100 ch.priority

let test_create_negotiated_channel () =
  let conn = create default_config ~is_offerer:true in
  match create_channel conn ~label:"negotiated" ~negotiated:true ~id:42 () with
  | Error e -> fail e
  | Ok ch ->
    check int "id" 42 ch.id;
    check bool "negotiated" true ch.negotiated;
    check channel_state_testable "state" Open ch.state  (* Negotiated channels start open *)

let test_create_channel_stream_id_allocation () =
  let conn = create default_config ~is_offerer:true in
  match create_channel conn ~label:"ch1" () with
  | Error e -> fail e
  | Ok ch1 ->
    check int "first id (offerer)" 0 ch1.id;
    match create_channel conn ~label:"ch2" () with
    | Error e -> fail e
    | Ok ch2 ->
      check int "second id" 2 ch2.id  (* Skips by 2 for even/odd separation *)

let test_get_channel () =
  let conn = create default_config ~is_offerer:true in
  match create_channel conn ~label:"test" () with
  | Error e -> fail e
  | Ok ch ->
    check bool "channel found" true (get_channel conn ~channel_id:ch.id <> None);
    check bool "channel not found" true (get_channel conn ~channel_id:999 = None)

let test_find_channel_by_label () =
  let conn = create default_config ~is_offerer:true in
  let _ = create_channel conn ~label:"one" () in
  let _ = create_channel conn ~label:"two" () in
  check bool "found by label" true (find_channel_by_label conn ~label:"two" <> None);
  check bool "not found" true (find_channel_by_label conn ~label:"three" = None)

let test_close_channel () =
  let conn = create default_config ~is_offerer:true in
  match create_channel conn ~label:"test" () with
  | Error e -> fail e
  | Ok ch ->
    close_channel conn ~channel_id:ch.id;
    match get_channel conn ~channel_id:ch.id with
    | None -> fail "channel should still exist"
    | Some ch' ->
      check channel_state_testable "state" Closed ch'.state

let test_get_channels () =
  let conn = create default_config ~is_offerer:true in
  let _ = create_channel conn ~label:"a" () in
  let _ = create_channel conn ~label:"b" () in
  let _ = create_channel conn ~label:"c" () in
  let channels = get_channels conn in
  check int "3 channels" 3 (List.length channels)

(** {1 DCEP Encoding Tests} *)

let test_encode_open_message () =
  let msg = {
    channel_type = Reliable_ordered;
    priority = 256;
    reliability_param = 0;
    label = "test-channel";
    protocol = "my-protocol";
  } in
  let encoded = encode_open_message msg in
  check bool "starts with 0x03" true (Bytes.get encoded 0 = '\x03');
  check bool "length correct" true (Bytes.length encoded = 12 + 12 + 11)

let test_decode_open_message () =
  let original = {
    channel_type = Reliable_unordered;
    priority = 512;
    reliability_param = 0;
    label = "audio";
    protocol = "";
  } in
  let encoded = encode_open_message original in
  match decode_open_message encoded with
  | Error e -> fail e
  | Ok decoded ->
    check int "priority" original.priority decoded.priority;
    check string "label" original.label decoded.label;
    check string "protocol" original.protocol decoded.protocol

let test_encode_ack_message () =
  let ack = encode_ack_message () in
  check int "length" 1 (Bytes.length ack);
  check bool "value 0x02" true (Bytes.get ack 0 = '\x02')

(** {1 Send/Receive Tests} *)

let test_send_text () =
  let conn = create default_config ~is_offerer:true in
  set_connected conn;
  match create_channel conn ~label:"test" ~negotiated:true ~id:0 () with
  | Error e -> fail e
  | Ok ch ->
    match send_text conn ~channel_id:ch.id "hello world" with
    | Error e -> fail e
    | Ok () ->
      let outgoing = get_outgoing conn in
      check int "1 message" 1 (List.length outgoing)

let test_send_binary () =
  let conn = create default_config ~is_offerer:true in
  set_connected conn;
  match create_channel conn ~label:"test" ~negotiated:true ~id:0 () with
  | Error e -> fail e
  | Ok ch ->
    match send_binary conn ~channel_id:ch.id (Bytes.of_string "\x00\x01\x02") with
    | Error e -> fail e
    | Ok () ->
      let outgoing = get_outgoing conn in
      check int "1 message" 1 (List.length outgoing);
      let (_, data, ppid) = List.hd outgoing in
      check int32 "binary ppid" 53l ppid;
      check int "data length" 3 (Bytes.length data)

let test_send_empty_string () =
  let conn = create default_config ~is_offerer:true in
  set_connected conn;
  match create_channel conn ~label:"test" ~negotiated:true ~id:0 () with
  | Error e -> fail e
  | Ok ch ->
    match send_text conn ~channel_id:ch.id "" with
    | Error e -> fail e
    | Ok () ->
      let outgoing = get_outgoing conn in
      let (_, _, ppid) = List.hd outgoing in
      check int32 "empty string ppid" 56l ppid

let test_send_to_closed_channel () =
  let conn = create default_config ~is_offerer:true in
  set_connected conn;
  match create_channel conn ~label:"test" ~negotiated:true ~id:0 () with
  | Error e -> fail e
  | Ok ch ->
    close_channel conn ~channel_id:ch.id;
    match send_text conn ~channel_id:ch.id "hello" with
    | Ok () -> fail "should have failed"
    | Error _ -> ()  (* Expected *)

let test_send_to_unknown_channel () =
  let conn = create default_config ~is_offerer:true in
  set_connected conn;
  match send_text conn ~channel_id:999 "hello" with
  | Ok () -> fail "should have failed"
  | Error _ -> ()  (* Expected *)

(** {1 Event Handling Tests} *)

let test_handle_dcep_open () =
  let conn = create default_config ~is_offerer:false in
  set_connected conn;
  let open_msg = {
    channel_type = Reliable_ordered;
    priority = 0;
    reliability_param = 0;
    label = "remote-channel";
    protocol = "";
  } in
  let data = encode_open_message open_msg in
  let events = handle_data conn data ~stream_id:0 in
  check int "1 event" 1 (List.length events);
  match List.hd events with
  | ChannelOpen ch ->
    check string "label" "remote-channel" ch.label;
    check int "stream id" 0 ch.id
  | _ -> fail "expected ChannelOpen event"

let test_handle_dcep_ack () =
  let conn = create default_config ~is_offerer:true in
  set_connected conn;
  (* Create a channel in Connecting state *)
  let _ = create_channel conn ~label:"test" () in
  (* Simulate receiving ACK *)
  let ack = encode_ack_message () in
  let events = handle_data conn ack ~stream_id:0 in
  check int "1 event" 1 (List.length events);
  match List.hd events with
  | ChannelOpen _ -> ()
  | _ -> fail "expected ChannelOpen event"

(** {1 JSON Info Tests} *)

let test_channel_info () =
  let conn = create default_config ~is_offerer:true in
  match create_channel conn ~label:"json-test" ~protocol:"proto" () with
  | Error e -> fail e
  | Ok ch ->
    let info = channel_info ch in
    let open Yojson.Safe.Util in
    check string "label" "json-test" (info |> member "label" |> to_string);
    check string "protocol" "proto" (info |> member "protocol" |> to_string);
    check string "type" "reliable-ordered" (info |> member "type" |> to_string)

let test_connection_info () =
  let conn = create default_config ~is_offerer:true in
  let _ = create_channel conn ~label:"a" () in
  let _ = create_channel conn ~label:"b" () in
  let info = connection_info conn in
  let open Yojson.Safe.Util in
  check string "state" "connecting" (info |> member "state" |> to_string);
  check bool "isOfferer" true (info |> member "isOfferer" |> to_bool);
  check int "channelCount" 2 (info |> member "channelCount" |> to_int)

(** {1 Test Suites} *)

let channel_type_tests = [
  "channel_type_strings", `Quick, test_channel_type_strings;
  "is_reliable", `Quick, test_is_reliable;
  "is_ordered", `Quick, test_is_ordered;
]

let state_tests = [
  "channel_state_strings", `Quick, test_channel_state_strings;
  "connection_state_strings", `Quick, test_connection_state_strings;
]

let config_tests = [
  "default_config", `Quick, test_default_config;
]

let connection_tests = [
  "create_connection_offerer", `Quick, test_create_connection_offerer;
  "create_connection_answerer", `Quick, test_create_connection_answerer;
  "set_connected", `Quick, test_set_connected;
  "close_connection", `Quick, test_close_connection;
]

let channel_mgmt_tests = [
  "create_channel", `Quick, test_create_channel;
  "create_channel_with_options", `Quick, test_create_channel_with_options;
  "create_negotiated_channel", `Quick, test_create_negotiated_channel;
  "create_channel_stream_id_allocation", `Quick, test_create_channel_stream_id_allocation;
  "get_channel", `Quick, test_get_channel;
  "find_channel_by_label", `Quick, test_find_channel_by_label;
  "close_channel", `Quick, test_close_channel;
  "get_channels", `Quick, test_get_channels;
]

let dcep_tests = [
  "encode_open_message", `Quick, test_encode_open_message;
  "decode_open_message", `Quick, test_decode_open_message;
  "encode_ack_message", `Quick, test_encode_ack_message;
]

let send_tests = [
  "send_text", `Quick, test_send_text;
  "send_binary", `Quick, test_send_binary;
  "send_empty_string", `Quick, test_send_empty_string;
  "send_to_closed_channel", `Quick, test_send_to_closed_channel;
  "send_to_unknown_channel", `Quick, test_send_to_unknown_channel;
]

let event_tests = [
  "handle_dcep_open", `Quick, test_handle_dcep_open;
  "handle_dcep_ack", `Quick, test_handle_dcep_ack;
]

let json_tests = [
  "channel_info", `Quick, test_channel_info;
  "connection_info", `Quick, test_connection_info;
]

let () =
  Alcotest.run "DataChannel (RFC 8831)" [
    ("channel_type", channel_type_tests);
    ("state", state_tests);
    ("config", config_tests);
    ("connection", connection_tests);
    ("channel_mgmt", channel_mgmt_tests);
    ("dcep", dcep_tests);
    ("send", send_tests);
    ("event", event_tests);
    ("json", json_tests);
  ]
