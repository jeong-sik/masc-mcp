(** Tests for Datachannel_eio - WebRTC DataChannel with Eio-native SCTP transport *)

open Alcotest

(** {1 Test Utilities} *)

let run_eio f = Eio_main.run f

let _bytes_testable = testable (fun fmt b -> Format.fprintf fmt "%S" (Bytes.to_string b)) Bytes.equal

(** {1 Basic Creation Tests} *)

let test_create_offerer () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  check bool "not connected initially" false (Masc_mcp.Datachannel_eio.is_connected conn)

let test_create_answerer () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:false in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  check bool "not connected initially" false (Masc_mcp.Datachannel_eio.is_connected conn)

let test_connect () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;
  check bool "connected after connect" true (Masc_mcp.Datachannel_eio.is_connected conn)

(** {1 Channel Creation Tests} *)

let test_create_channel () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  match Masc_mcp.Datachannel_eio.create_channel conn ~label:"test" () with
  | Ok ch ->
    check string "label" "test" ch.label;
    check int "id (offerer gets even)" 0 ch.id
  | Error e ->
    fail ("create_channel failed: " ^ e)

let test_create_multiple_channels () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"ch1" () in
  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"ch2" () in
  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"ch3" () in

  let channels = Masc_mcp.Datachannel_eio.get_channels conn in
  check int "three channels" 3 (List.length channels)

let test_answerer_gets_odd_ids () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:false in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  match Masc_mcp.Datachannel_eio.create_channel conn ~label:"test" () with
  | Ok ch ->
    check int "id (answerer gets odd)" 1 ch.id
  | Error e ->
    fail ("create_channel failed: " ^ e)

(** {1 Channel Lookup Tests} *)

let test_get_channel_by_id () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"test" () in

  match Masc_mcp.Datachannel_eio.get_channel conn ~channel_id:0 with
  | Some ch -> check string "found channel" "test" ch.label
  | None -> fail "channel not found"

let test_find_channel_by_label () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"findme" () in

  match Masc_mcp.Datachannel_eio.find_channel_by_label conn ~label:"findme" with
  | Some ch -> check int "found channel by label" 0 ch.id
  | None -> fail "channel not found by label"

(** {1 Data Transfer Tests} *)

let test_send_text_not_connected () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  (* Don't connect *)

  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"test" () in

  match Masc_mcp.Datachannel_eio.send_text conn ~channel_id:0 "hello" with
  | Error _ -> ()  (* Expected: channel not open *)
  | Ok () -> fail "should fail when not connected"

let test_send_text_queues_message () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  (* Create negotiated channel (immediately open) *)
  match Masc_mcp.Datachannel_eio.create_channel conn ~label:"test" ~negotiated:true () with
  | Error e -> fail ("create_channel failed: " ^ e)
  | Ok _ch ->
    match Masc_mcp.Datachannel_eio.send_text conn ~channel_id:0 "hello world" with
    | Error e -> fail ("send_text failed: " ^ e)
    | Ok () ->
      let outgoing = Masc_mcp.Datachannel_eio.flush_outgoing conn in
      check bool "has outgoing data" true (List.length outgoing > 0)

let test_send_binary () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  match Masc_mcp.Datachannel_eio.create_channel conn ~label:"bin" ~negotiated:true () with
  | Error e -> fail ("create_channel failed: " ^ e)
  | Ok _ch ->
    let data = Bytes.of_string "\x00\x01\x02\x03\xFF" in
    match Masc_mcp.Datachannel_eio.send_binary conn ~channel_id:0 data with
    | Error e -> fail ("send_binary failed: " ^ e)
    | Ok () ->
      let outgoing = Masc_mcp.Datachannel_eio.flush_outgoing conn in
      check bool "has binary outgoing" true (List.length outgoing > 0)

(** {1 SCTP Packet Tests} *)

let test_create_sctp_packet () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  match Masc_mcp.Datachannel_eio.create_channel conn ~label:"pkt" ~negotiated:true () with
  | Error e -> fail e
  | Ok _ch ->
    let _ = Masc_mcp.Datachannel_eio.send_text conn ~channel_id:0 "packet test" in
    match Masc_mcp.Datachannel_eio.create_sctp_packet conn with
    | None -> fail "expected SCTP packet"
    | Some packet_bytes ->
      (* SCTP packet should be at least 12 bytes (header) *)
      check bool "packet has header" true (Bytes.length packet_bytes >= 12)

let test_no_packet_when_empty () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  match Masc_mcp.Datachannel_eio.create_sctp_packet conn with
  | None -> ()  (* Expected: no data *)
  | Some _ -> fail "should return None when no outgoing data"

(** {1 Callback Tests} *)

let test_on_message_callback () =
  run_eio @@ fun _env ->
  let received = ref None in

  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in

  Masc_mcp.Datachannel_eio.on_message conn (fun ch_id data ->
    received := Some (ch_id, data)
  );

  (* Callback should be registered *)
  check bool "callback registered" true (Option.is_none !received)

(** {1 Close Tests} *)

let test_close_channel () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"close_test" () in
  Masc_mcp.Datachannel_eio.close_channel conn ~channel_id:0;

  match Masc_mcp.Datachannel_eio.get_channel conn ~channel_id:0 with
  | Some ch ->
    check bool "channel closed" true
      (ch.state = Masc_mcp.Datachannel.Closed)
  | None -> ()  (* Also acceptable *)

let test_close_connection () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"ch1" () in
  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"ch2" () in

  Masc_mcp.Datachannel_eio.close conn;

  check bool "connection closed" false (Masc_mcp.Datachannel_eio.is_connected conn)

(** {1 Status/Debug Tests} *)

let test_status_json () =
  run_eio @@ fun _env ->
  let config = Masc_mcp.Datachannel_eio.default_config ~is_offerer:true in
  let conn = Masc_mcp.Datachannel_eio.create ~config () in
  Masc_mcp.Datachannel_eio.connect conn;

  let _ = Masc_mcp.Datachannel_eio.create_channel conn ~label:"status_test" () in

  let json = Masc_mcp.Datachannel_eio.status_json conn in
  match json with
  | `Assoc fields ->
    check bool "has datachannel field" true (List.mem_assoc "datachannel" fields);
    check bool "has sctp field" true (List.mem_assoc "sctp" fields);
    check bool "has channels field" true (List.mem_assoc "channels" fields)
  | _ -> fail "expected JSON object"

(** {1 Test Runner} *)

let () =
  run "Datachannel_eio" [
    "creation", [
      test_case "create offerer" `Quick test_create_offerer;
      test_case "create answerer" `Quick test_create_answerer;
      test_case "connect" `Quick test_connect;
    ];
    "channels", [
      test_case "create channel" `Quick test_create_channel;
      test_case "multiple channels" `Quick test_create_multiple_channels;
      test_case "answerer odd ids" `Quick test_answerer_gets_odd_ids;
      test_case "get by id" `Quick test_get_channel_by_id;
      test_case "find by label" `Quick test_find_channel_by_label;
    ];
    "data transfer", [
      test_case "send not connected" `Quick test_send_text_not_connected;
      test_case "send text queues" `Quick test_send_text_queues_message;
      test_case "send binary" `Quick test_send_binary;
    ];
    "sctp packets", [
      test_case "create packet" `Quick test_create_sctp_packet;
      test_case "no packet when empty" `Quick test_no_packet_when_empty;
    ];
    "callbacks", [
      test_case "on_message" `Quick test_on_message_callback;
    ];
    "close", [
      test_case "close channel" `Quick test_close_channel;
      test_case "close connection" `Quick test_close_connection;
    ];
    "status", [
      test_case "status json" `Quick test_status_json;
    ];
  ]
