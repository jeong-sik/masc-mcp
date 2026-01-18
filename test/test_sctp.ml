(** Tests for RFC 4960 SCTP module *)

open Alcotest
open Masc_mcp.Sctp

(** {1 Test Utilities} *)

let state_testable = testable (fun fmt s -> Format.fprintf fmt "%s" (string_of_state s)) (=)
let message_type_testable = testable (fun fmt mt -> Format.fprintf fmt "%s" (string_of_message_type mt)) (=)

(** {1 Chunk Type Tests} *)

let test_chunk_types () =
  check string "DATA" "DATA" (string_of_chunk_type DATA);
  check string "INIT" "INIT" (string_of_chunk_type INIT);
  check string "SACK" "SACK" (string_of_chunk_type SACK);
  check string "HEARTBEAT" "HEARTBEAT" (string_of_chunk_type HEARTBEAT);
  check string "SHUTDOWN" "SHUTDOWN" (string_of_chunk_type SHUTDOWN)

let test_unknown_chunk_type () =
  check string "unknown 255" "Unknown(255)" (string_of_chunk_type (Unknown 255))

(** {1 State Tests} *)

let test_state_strings () =
  check string "closed" "closed" (string_of_state Closed);
  check string "cookie_wait" "cookie-wait" (string_of_state Cookie_wait);
  check string "cookie_echoed" "cookie-echoed" (string_of_state Cookie_echoed);
  check string "established" "established" (string_of_state Established);
  check string "shutdown_pending" "shutdown-pending" (string_of_state Shutdown_pending)

(** {1 Message Type Tests} *)

let test_message_types () =
  check string "dcep" "DCEP" (string_of_message_type WebRTC_DCEP);
  check string "string" "String" (string_of_message_type WebRTC_String);
  check string "binary" "Binary" (string_of_message_type WebRTC_Binary)

let test_ppid_conversion () =
  check int32 "dcep ppid" 50l (ppid_of_message_type WebRTC_DCEP);
  check int32 "string ppid" 51l (ppid_of_message_type WebRTC_String);
  check int32 "binary ppid" 53l (ppid_of_message_type WebRTC_Binary);
  check int32 "string_empty ppid" 56l (ppid_of_message_type WebRTC_String_Empty);
  check int32 "binary_empty ppid" 57l (ppid_of_message_type WebRTC_Binary_Empty)

let test_ppid_to_message_type () =
  check (option message_type_testable) "50 -> dcep" (Some WebRTC_DCEP) (message_type_of_ppid 50l);
  check bool "50 -> dcep" true (message_type_of_ppid 50l = Some WebRTC_DCEP);
  check bool "51 -> string" true (message_type_of_ppid 51l = Some WebRTC_String);
  check bool "99 -> none" true (message_type_of_ppid 99l = None)

(** {1 Config Tests} *)

let test_default_config () =
  let c = default_config in
  check int "local port" 5000 c.local_port;
  check int "remote port" 5000 c.remote_port;
  check int "mtu" 1200 c.mtu;
  check int "max_retransmits" 10 c.max_retransmits;
  check int "initial rto" 3000 c.rto_initial_ms;
  check int "a_rwnd" 65536 c.a_rwnd;
  check int "outbound streams" 65535 c.num_outbound_streams;
  check int "inbound streams" 65535 c.num_inbound_streams

(** {1 Association Tests} *)

let test_create_association () =
  let assoc = create default_config in
  check state_testable "initial state" Closed (get_state assoc);
  check bool "not established" false (is_established assoc)

let test_association_info () =
  let assoc = create default_config in
  let info = association_info assoc in
  let open Yojson.Safe.Util in
  check string "state" "closed" (info |> member "state" |> to_string);
  check int "streams" 0 (info |> member "streams" |> to_int)

(** {1 Stream Tests} *)

let test_open_stream () =
  let assoc = create default_config in
  let stream = open_stream assoc 0 () in
  check int "stream id" 0 stream.id;
  check bool "ordered by default" true stream.ordered;
  check int "next ssn" 0 stream.next_ssn

let test_open_unordered_stream () =
  let assoc = create default_config in
  let stream = open_stream assoc 1 ~ordered:false () in
  check int "stream id" 1 stream.id;
  check bool "unordered" false stream.ordered

let test_get_stream () =
  let assoc = create default_config in
  let _ = open_stream assoc 42 () in
  check bool "stream exists" true (get_stream assoc 42 <> None);
  check bool "stream not exists" true (get_stream assoc 99 = None)

let test_close_stream () =
  let assoc = create default_config in
  let _ = open_stream assoc 5 () in
  close_stream assoc 5;
  check bool "stream removed" true (get_stream assoc 5 = None)

let test_get_streams () =
  let assoc = create default_config in
  let _ = open_stream assoc 0 () in
  let _ = open_stream assoc 1 () in
  let _ = open_stream assoc 2 () in
  let streams = get_streams assoc in
  check int "3 streams" 3 (List.length streams)

(** {1 Encoding Tests} *)

let test_encode_data_chunk () =
  let dc = {
    flags = { end_fragment = true; begin_fragment = true; unordered = false; immediate = false };
    tsn = 12345l;
    stream_id = 0;
    stream_seq = 1;
    ppid = 53l;
    user_data = Bytes.of_string "hello";
  } in
  let encoded = encode_data_chunk dc in
  check bool "length > 12" true (Bytes.length encoded > 12)

let test_decode_data_chunk () =
  let dc = {
    flags = { end_fragment = true; begin_fragment = true; unordered = false; immediate = false };
    tsn = 99999l;
    stream_id = 5;
    stream_seq = 10;
    ppid = 51l;
    user_data = Bytes.of_string "test data";
  } in
  let encoded = encode_data_chunk dc in
  match decode_data_chunk encoded with
  | Error e -> fail e
  | Ok decoded ->
    check int32 "tsn matches" dc.tsn decoded.tsn;
    check int "stream_id matches" dc.stream_id decoded.stream_id;
    check int "stream_seq matches" dc.stream_seq decoded.stream_seq;
    check int32 "ppid matches" dc.ppid decoded.ppid;
    check string "user_data matches" (Bytes.to_string dc.user_data) (Bytes.to_string decoded.user_data)

(** {1 SACK Encoding Tests} *)

let test_encode_sack_simple () =
  let sack = create_sack ~cumulative_tsn:1000l ~a_rwnd:65535l in
  let encoded = encode_sack_chunk sack in
  (* Header (4) + fixed (12) = 16 bytes minimum *)
  check bool "length >= 16" true (Bytes.length encoded >= 16);
  (* Check chunk type = 3 (SACK) *)
  check int "chunk type" 3 (Bytes.get_uint8 encoded 0)

let test_decode_sack_simple () =
  let sack = create_sack ~cumulative_tsn:12345l ~a_rwnd:131072l in
  let encoded = encode_sack_chunk sack in
  match decode_sack_chunk encoded with
  | Error e -> fail e
  | Ok decoded ->
    check int32 "cumulative_tsn matches" sack.cumulative_tsn decoded.cumulative_tsn;
    check int32 "a_rwnd matches" sack.a_rwnd decoded.a_rwnd;
    check int "no gap blocks" 0 (List.length decoded.gap_ack_blocks);
    check int "no duplicates" 0 (List.length decoded.duplicate_tsns)

let test_encode_decode_sack_with_gaps () =
  let gaps = [
    { gap_start = 2; gap_end = 3 };   (* TSN 1002-1003 received *)
    { gap_start = 5; gap_end = 8 };   (* TSN 1005-1008 received *)
  ] in
  let sack = create_sack_with_gaps ~cumulative_tsn:1000l ~a_rwnd:65535l ~gap_ack_blocks:gaps in
  let encoded = encode_sack_chunk sack in
  (* 16 + 2 gaps * 4 = 24 bytes *)
  check bool "length >= 24" true (Bytes.length encoded >= 24);
  match decode_sack_chunk encoded with
  | Error e -> fail e
  | Ok decoded ->
    check int32 "cumulative_tsn" sack.cumulative_tsn decoded.cumulative_tsn;
    check int "2 gap blocks" 2 (List.length decoded.gap_ack_blocks);
    let gap1 = List.nth decoded.gap_ack_blocks 0 in
    let gap2 = List.nth decoded.gap_ack_blocks 1 in
    check int "gap1 start" 2 gap1.gap_start;
    check int "gap1 end" 3 gap1.gap_end;
    check int "gap2 start" 5 gap2.gap_start;
    check int "gap2 end" 8 gap2.gap_end

let test_sack_roundtrip () =
  let sack = {
    cumulative_tsn = 999999l;
    a_rwnd = 262144l;
    gap_ack_blocks = [
      { gap_start = 10; gap_end = 15 };
      { gap_start = 20; gap_end = 25 };
      { gap_start = 100; gap_end = 100 };  (* Single TSN gap *)
    ];
    duplicate_tsns = [1000010l; 1000020l];
  } in
  let encoded = encode_sack_chunk sack in
  match decode_sack_chunk encoded with
  | Error e -> fail e
  | Ok decoded ->
    check int32 "cumulative_tsn" sack.cumulative_tsn decoded.cumulative_tsn;
    check int32 "a_rwnd" sack.a_rwnd decoded.a_rwnd;
    check int "gap count" 3 (List.length decoded.gap_ack_blocks);
    check int "dup count" 2 (List.length decoded.duplicate_tsns);
    check int32 "dup1" (List.nth sack.duplicate_tsns 0) (List.nth decoded.duplicate_tsns 0);
    check int32 "dup2" (List.nth sack.duplicate_tsns 1) (List.nth decoded.duplicate_tsns 1)

(** {1 Checksum Tests} *)

let test_calculate_checksum () =
  let data = Bytes.of_string "test packet data" in
  let checksum = calculate_checksum data in
  check bool "checksum non-zero" true (checksum <> 0l)

let test_checksum_consistency () =
  let data = Bytes.of_string "same input same output" in
  let c1 = calculate_checksum data in
  let c2 = calculate_checksum data in
  check int32 "same checksum" c1 c2

let test_checksum_different_data () =
  let d1 = Bytes.of_string "data1" in
  let d2 = Bytes.of_string "data2" in
  let c1 = calculate_checksum d1 in
  let c2 = calculate_checksum d2 in
  check bool "different checksums" true (c1 <> c2)

(** {1 Packet Tests} *)

let test_encode_packet () =
  let packet = {
    header = {
      source_port = 5000;
      dest_port = 5001;
      verification_tag = 123456l;
      checksum = 0l;
    };
    chunks = [];
  } in
  let encoded = encode_packet packet in
  check bool "header size" true (Bytes.length encoded >= 12)

let test_decode_packet () =
  let packet = {
    header = {
      source_port = 5000;
      dest_port = 5001;
      verification_tag = 0xABCDl;
      checksum = 0l;
    };
    chunks = [];
  } in
  let encoded = encode_packet packet in
  match decode_packet encoded with
  | Error e -> fail e
  | Ok decoded ->
    check int "source port" 5000 decoded.header.source_port;
    check int "dest port" 5001 decoded.header.dest_port

(** {1 SCTP Detection Tests} *)

let test_is_sctp_data () =
  (* Valid SCTP-like packet (min 12 bytes) *)
  let valid = Bytes.create 12 in
  check bool "valid sctp" true (is_sctp_data valid);

  (* Too short *)
  let short = Bytes.create 8 in
  check bool "too short" false (is_sctp_data short)

(** {1 Test Suites} *)

let chunk_type_tests = [
  "chunk_types", `Quick, test_chunk_types;
  "unknown_chunk_type", `Quick, test_unknown_chunk_type;
]

let state_tests = [
  "state_strings", `Quick, test_state_strings;
]

let message_type_tests = [
  "message_types", `Quick, test_message_types;
  "ppid_conversion", `Quick, test_ppid_conversion;
  "ppid_to_message_type", `Quick, test_ppid_to_message_type;
]

let config_tests = [
  "default_config", `Quick, test_default_config;
]

let association_tests = [
  "create_association", `Quick, test_create_association;
  "association_info", `Quick, test_association_info;
]

let stream_tests = [
  "open_stream", `Quick, test_open_stream;
  "open_unordered_stream", `Quick, test_open_unordered_stream;
  "get_stream", `Quick, test_get_stream;
  "close_stream", `Quick, test_close_stream;
  "get_streams", `Quick, test_get_streams;
]

let encoding_tests = [
  "encode_data_chunk", `Quick, test_encode_data_chunk;
  "decode_data_chunk", `Quick, test_decode_data_chunk;
]

let sack_tests = [
  "encode_sack_simple", `Quick, test_encode_sack_simple;
  "decode_sack_simple", `Quick, test_decode_sack_simple;
  "encode_decode_sack_with_gaps", `Quick, test_encode_decode_sack_with_gaps;
  "sack_roundtrip", `Quick, test_sack_roundtrip;
]

(** {1 Retransmission Timer Tests} *)

let test_create_rtx_timer () =
  let timer = create_rtx_timer () in
  check (float 0.01) "initial RTO" 1000.0 timer.rto;
  check bool "not running" true (timer.rtx_state = Rtx_stopped);
  check int "n_rtos = 0" 0 timer.n_rtos;
  check int "max_retransmits default" 5 timer.max_retransmits

let test_start_stop_timer () =
  let timer = create_rtx_timer () in
  start_rtx_timer timer;
  check bool "running after start" true (timer.rtx_state = Rtx_running);
  check bool "has start_time" true (timer.start_time <> None);
  stop_rtx_timer timer;
  check bool "stopped after stop" true (timer.rtx_state = Rtx_stopped);
  check bool "no start_time" true (timer.start_time = None)

let test_update_rto_first_measurement () =
  let timer = create_rtx_timer () in
  update_rto timer ~measured_rtt:200.0;
  (* First measurement: SRTT = R, RTTVAR = R/2, RTO = SRTT + 4*RTTVAR *)
  check bool "srtt set" true (timer.srtt = Some 200.0);
  check bool "rttvar set" true (timer.rttvar = Some 100.0);
  (* RTO = 200 + 4*100 = 600 *)
  check (float 0.01) "rto calculated" 600.0 timer.rto

let test_update_rto_subsequent () =
  let timer = create_rtx_timer () in
  update_rto timer ~measured_rtt:200.0;  (* First *)
  update_rto timer ~measured_rtt:180.0;  (* Second *)
  (* SRTT' = 0.875*200 + 0.125*180 = 175 + 22.5 = 197.5 *)
  check bool "srtt updated" true (timer.srtt <> None);
  check bool "rttvar updated" true (timer.rttvar <> None)

let test_handle_timeout_retransmit () =
  let timer = create_rtx_timer () in
  start_rtx_timer timer;
  let should_rtx = handle_timeout timer in
  check bool "should retransmit" true should_rtx;
  check int "n_rtos = 1" 1 timer.n_rtos;
  (* RTO doubled: 1000 * 2 = 2000 *)
  check (float 0.01) "rto doubled" 2000.0 timer.rto

let test_handle_timeout_max_retransmits () =
  let timer = create_rtx_timer ~max_retransmits:2 () in
  start_rtx_timer timer;
  let r1 = handle_timeout timer in
  let r2 = handle_timeout timer in
  let r3 = handle_timeout timer in  (* Should fail *)
  check bool "r1 = true" true r1;
  check bool "r2 = true" true r2;
  check bool "r3 = false (failed)" false r3;
  check bool "timer closed" true (timer.rtx_state = Rtx_closed)

let test_is_expired () =
  let timer = create_rtx_timer () in
  check bool "not expired when stopped" false (is_expired timer);
  start_rtx_timer timer;
  check bool "not immediately expired" false (is_expired timer)

let test_timer_status () =
  let timer = create_rtx_timer () in
  let status = rtx_timer_status timer in
  check bool "contains RTO" true (String.sub status 0 4 = "RTO=");
  check bool "contains stopped" true (String.length status > 0 &&
    (String.sub status (String.length status - 7) 7 = "stopped"))

let rtx_timer_tests = [
  "create_rtx_timer", `Quick, test_create_rtx_timer;
  "start_stop_timer", `Quick, test_start_stop_timer;
  "update_rto_first", `Quick, test_update_rto_first_measurement;
  "update_rto_subsequent", `Quick, test_update_rto_subsequent;
  "handle_timeout_rtx", `Quick, test_handle_timeout_retransmit;
  "handle_timeout_max", `Quick, test_handle_timeout_max_retransmits;
  "is_expired", `Quick, test_is_expired;
  "timer_status", `Quick, test_timer_status;
]

(** {1 INIT Handshake Tests} *)

let test_encode_decode_init () =
  let init = create_init ~initiate_tag:12345l ~initial_tsn:1000l () in
  let encoded = encode_init_chunk init in
  check bool "length = 20" true (Bytes.length encoded = 20);
  check int "chunk type" 1 (Bytes.get_uint8 encoded 0);
  match decode_init_chunk encoded with
  | Error e -> fail e
  | Ok decoded ->
    check int32 "initiate_tag" init.initiate_tag decoded.initiate_tag;
    check int32 "initial_tsn" init.initial_tsn decoded.initial_tsn;
    check int32 "a_rwnd" init.a_rwnd decoded.a_rwnd;
    check int "num_outbound" init.num_outbound_streams decoded.num_outbound_streams;
    check int "num_inbound" init.num_inbound_streams decoded.num_inbound_streams

let test_init_validation () =
  (* Test zero initiate_tag *)
  let bad_init = { initiate_tag = 0l; a_rwnd = 65536l;
                   num_outbound_streams = 100; num_inbound_streams = 100;
                   initial_tsn = 1000l } in
  let encoded = encode_init_chunk bad_init in
  match decode_init_chunk encoded with
  | Error msg -> check bool "error contains tag" true (String.length msg > 0)
  | Ok _ -> fail "Should reject zero initiate_tag"

let test_encode_decode_init_ack () =
  let cookie = generate_state_cookie ~peer_tag:111l ~peer_tsn:222l
      ~my_tag:333l ~my_tsn:444l in
  let init_ack = create_init_ack ~initiate_tag:333l ~initial_tsn:444l ~state_cookie:cookie () in
  let encoded = encode_init_ack_chunk init_ack in
  check bool "length > 20" true (Bytes.length encoded > 20);
  check int "chunk type" 2 (Bytes.get_uint8 encoded 0);
  match decode_init_ack_chunk encoded with
  | Error e -> fail e
  | Ok decoded ->
    check int32 "initiate_tag" init_ack.initiate_tag decoded.initiate_tag;
    check int32 "initial_tsn" init_ack.initial_tsn decoded.initial_tsn;
    check int "cookie length" 32 (Bytes.length decoded.state_cookie)

let test_encode_decode_cookie_echo () =
  let cookie = Bytes.of_string "test_cookie_data_12345678" in
  let cookie_echo = { cookie } in
  let encoded = encode_cookie_echo_chunk cookie_echo in
  check int "chunk type" 10 (Bytes.get_uint8 encoded 0);
  match decode_cookie_echo_chunk encoded with
  | Error e -> fail e
  | Ok decoded ->
    check string "cookie matches" (Bytes.to_string cookie) (Bytes.to_string decoded.cookie)

let test_encode_decode_cookie_ack () =
  let encoded = encode_cookie_ack_chunk () in
  check int "length = 4" 4 (Bytes.length encoded);
  check int "chunk type" 11 (Bytes.get_uint8 encoded 0);
  match decode_cookie_ack_chunk encoded with
  | Error e -> fail e
  | Ok () -> ()

let test_full_init_handshake () =
  (* Simulate 4-way handshake *)
  (* 1. Client creates INIT *)
  let client_tag = 0xABCD1234l in
  let client_tsn = 1000l in
  let init = create_init ~initiate_tag:client_tag ~initial_tsn:client_tsn () in
  let init_encoded = encode_init_chunk init in

  (* 2. Server receives INIT, creates INIT_ACK with cookie *)
  (match decode_init_chunk init_encoded with
   | Error e -> fail e
   | Ok recv_init ->
     let server_tag = 0xDCBA4321l in
     let server_tsn = 5000l in
     let cookie = generate_state_cookie
         ~peer_tag:recv_init.initiate_tag ~peer_tsn:recv_init.initial_tsn
         ~my_tag:server_tag ~my_tsn:server_tsn in
     let init_ack = create_init_ack ~initiate_tag:server_tag
         ~initial_tsn:server_tsn ~state_cookie:cookie () in
     let init_ack_encoded = encode_init_ack_chunk init_ack in

     (* 3. Client receives INIT_ACK, sends COOKIE_ECHO *)
     (match decode_init_ack_chunk init_ack_encoded with
      | Error e -> fail e
      | Ok recv_init_ack ->
        let cookie_echo = { cookie = recv_init_ack.state_cookie } in
        let cookie_echo_encoded = encode_cookie_echo_chunk cookie_echo in

        (* 4. Server receives COOKIE_ECHO, sends COOKIE_ACK *)
        (match decode_cookie_echo_chunk cookie_echo_encoded with
         | Error e -> fail e
         | Ok recv_cookie_echo ->
           check int "cookie preserved" 32 (Bytes.length recv_cookie_echo.cookie);
           let cookie_ack_encoded = encode_cookie_ack_chunk () in

           (* 5. Client receives COOKIE_ACK - association established! *)
           (match decode_cookie_ack_chunk cookie_ack_encoded with
            | Error e -> fail e
            | Ok () ->
              (* Success - handshake complete *)
              ()))))

let init_handshake_tests = [
  "encode_decode_init", `Quick, test_encode_decode_init;
  "init_validation", `Quick, test_init_validation;
  "encode_decode_init_ack", `Quick, test_encode_decode_init_ack;
  "encode_decode_cookie_echo", `Quick, test_encode_decode_cookie_echo;
  "encode_decode_cookie_ack", `Quick, test_encode_decode_cookie_ack;
  "full_init_handshake", `Quick, test_full_init_handshake;
]

let checksum_tests = [
  "calculate_checksum", `Quick, test_calculate_checksum;
  "checksum_consistency", `Quick, test_checksum_consistency;
  "checksum_different_data", `Quick, test_checksum_different_data;
]

let packet_tests = [
  "encode_packet", `Quick, test_encode_packet;
  "decode_packet", `Quick, test_decode_packet;
]

let detection_tests = [
  "is_sctp_data", `Quick, test_is_sctp_data;
]

let () =
  Alcotest.run "SCTP (RFC 4960)" [
    ("chunk_type", chunk_type_tests);
    ("state", state_tests);
    ("message_type", message_type_tests);
    ("config", config_tests);
    ("association", association_tests);
    ("stream", stream_tests);
    ("encoding", encoding_tests);
    ("sack", sack_tests);
    ("init_handshake", init_handshake_tests);
    ("rtx_timer", rtx_timer_tests);
    ("checksum", checksum_tests);
    ("packet", packet_tests);
    ("detection", detection_tests);
  ]
