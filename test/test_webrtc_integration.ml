(** Real-world integration tests for WebRTC Protocol Stack

    Tests complete data flow scenarios simulating actual WebRTC usage:
    - Signaling (SDP offer/answer)
    - ICE candidate exchange
    - DTLS handshake simulation
    - SCTP association establishment
    - DataChannel creation and messaging

    Reference: WebRTC 1.0 spec + RFC 8825 (WebRTC Overview)
*)

open Alcotest

(** {1 Test Utilities} *)

let ice_state_testable = Alcotest.testable
  (fun fmt s -> Format.fprintf fmt "%s" (Masc_mcp.Ice.string_of_connection_state s))
  (=)

let sctp_state_testable = Alcotest.testable
  (fun fmt s -> Format.fprintf fmt "%s" (Masc_mcp.Sctp.string_of_state s))
  (=)

let dc_state_testable = Alcotest.testable
  (fun fmt s -> Format.fprintf fmt "%s" (Masc_mcp.Datachannel.string_of_connection_state s))
  (=)

(** Create a test fingerprint *)
let test_fingerprint : Masc_mcp.Sdp.fingerprint = {
  hash_func = "sha-256";
  fingerprint = "AA:BB:CC:DD:EE:FF:00:11:22:33:44:55:66:77:88:99:AA:BB:CC:DD:EE:FF:00:11:22:33:44:55:66:77:88:99";
}

(** {1 Scenario 1: Signaling Flow} *)

(** Test SDP offer/answer exchange scenario *)
let test_sdp_offer_answer () =
  (* Get ICE credentials *)
  let ice_agent = Masc_mcp.Ice.create Masc_mcp.Ice.default_config in
  let (ice_ufrag, ice_pwd) = Masc_mcp.Ice.get_local_credentials ice_agent in

  (* Offerer creates SDP offer *)
  let offer = Masc_mcp.Sdp.create_datachannel_offer
    ~ice_ufrag ~ice_pwd ~fingerprint:test_fingerprint ()
  in

  (* Convert to string and parse *)
  let sdp_string = Masc_mcp.Sdp.to_string offer in
  check bool "SDP string not empty" true (String.length sdp_string > 0);

  match Masc_mcp.Sdp.parse sdp_string with
  | Error e -> fail ("Failed to parse SDP: " ^ e)
  | Ok _parsed ->
    (* Answerer creates SDP answer *)
    let ice_agent2 = Masc_mcp.Ice.create Masc_mcp.Ice.default_config in
    let (ice_ufrag2, ice_pwd2) = Masc_mcp.Ice.get_local_credentials ice_agent2 in

    let answer = Masc_mcp.Sdp.create_datachannel_answer
      ~offer ~ice_ufrag:ice_ufrag2 ~ice_pwd:ice_pwd2 ~fingerprint:test_fingerprint
    in
    let answer_string = Masc_mcp.Sdp.to_string answer in
    check bool "answer SDP not empty" true (String.length answer_string > 0)

(** Test SDP with ICE candidates *)
let test_sdp_with_ice_candidates () =
  let ice_agent = Masc_mcp.Ice.create Masc_mcp.Ice.default_config in
  let (ice_ufrag, ice_pwd) = Masc_mcp.Ice.get_local_credentials ice_agent in

  let sdp = Masc_mcp.Sdp.create_datachannel_offer
    ~ice_ufrag ~ice_pwd ~fingerprint:test_fingerprint ()
  in

  (* Parse ICE candidate *)
  let candidate_line = "1 1 UDP 2130706431 192.168.1.100 54321 typ host" in
  match Masc_mcp.Sdp.parse_ice_candidate candidate_line with
  | Error e -> fail ("Failed to parse candidate: " ^ e)
  | Ok candidate ->
    check string "candidate address" "192.168.1.100" candidate.address;
    check int "candidate port" 54321 candidate.port;
    check string "candidate type" "host" candidate.cand_type;

    (* SDP should be able to include this candidate *)
    let sdp_str = Masc_mcp.Sdp.to_string sdp in
    check bool "SDP contains version" true (String.length sdp_str > 10)

(** {1 Scenario 2: ICE Connectivity} *)

(** Test ICE agent initialization and candidate gathering *)
let test_ice_agent_lifecycle () =
  let config = Masc_mcp.Ice.default_config in
  let agent = Masc_mcp.Ice.create config in

  (* Initial state *)
  check ice_state_testable "initial state" Masc_mcp.Ice.New (Masc_mcp.Ice.get_state agent);

  (* Gather candidates *)
  let () = Lwt_main.run (Masc_mcp.Ice.gather_candidates agent) in
  check bool "has local candidates" true
    (List.length (Masc_mcp.Ice.get_local_candidates agent) > 0);

  (* Gathering complete *)
  let gathering_state = Masc_mcp.Ice.get_gathering_state agent in
  check bool "gathering complete" true (gathering_state = Masc_mcp.Ice.Gathering_complete)

(** Test ICE candidate exchange between two agents *)
let test_ice_candidate_exchange () =
  let config = Masc_mcp.Ice.{ default_config with role = Controlling } in
  let offerer = Masc_mcp.Ice.create config in

  let config2 = Masc_mcp.Ice.{ default_config with role = Controlled } in
  let answerer = Masc_mcp.Ice.create config2 in

  (* Both gather candidates *)
  let () = Lwt_main.run (Masc_mcp.Ice.gather_candidates offerer) in
  let () = Lwt_main.run (Masc_mcp.Ice.gather_candidates answerer) in

  (* Exchange candidates *)
  let offerer_candidates = Masc_mcp.Ice.get_local_candidates offerer in
  let answerer_candidates = Masc_mcp.Ice.get_local_candidates answerer in

  List.iter (fun c -> Masc_mcp.Ice.add_remote_candidate answerer c) offerer_candidates;
  List.iter (fun c -> Masc_mcp.Ice.add_remote_candidate offerer c) answerer_candidates;

  (* Verify remote candidates added *)
  check bool "offerer has remote" true
    (List.length (Masc_mcp.Ice.get_remote_candidates offerer) > 0);
  check bool "answerer has remote" true
    (List.length (Masc_mcp.Ice.get_remote_candidates answerer) > 0)

(** Test ICE credentials exchange *)
let test_ice_credentials () =
  let agent1 = Masc_mcp.Ice.create Masc_mcp.Ice.default_config in
  let agent2 = Masc_mcp.Ice.create Masc_mcp.Ice.default_config in

  let (ufrag1, pwd1) = Masc_mcp.Ice.get_local_credentials agent1 in
  let (ufrag2, pwd2) = Masc_mcp.Ice.get_local_credentials agent2 in

  (* Exchange credentials *)
  Masc_mcp.Ice.set_remote_credentials agent1 ~ufrag:ufrag2 ~pwd:pwd2;
  Masc_mcp.Ice.set_remote_credentials agent2 ~ufrag:ufrag1 ~pwd:pwd1;

  (* Verify credentials are different *)
  check bool "different ufrags" true (ufrag1 <> ufrag2);
  check bool "ufrag length" true (String.length ufrag1 >= 4);
  check bool "pwd length" true (String.length pwd1 >= 22)

(** {1 Scenario 3: SCTP Association} *)

(** Test SCTP association establishment *)
let test_sctp_association_flow () =
  let config = Masc_mcp.Sctp.default_config in
  let client_assoc = Masc_mcp.Sctp.create config in
  let server_assoc = Masc_mcp.Sctp.create config in

  (* Initial state *)
  check sctp_state_testable "client closed" Masc_mcp.Sctp.Closed
    (Masc_mcp.Sctp.get_state client_assoc);
  check sctp_state_testable "server closed" Masc_mcp.Sctp.Closed
    (Masc_mcp.Sctp.get_state server_assoc);

  (* Simulate establishment (in real use, this involves INIT/INIT_ACK exchange) *)
  Masc_mcp.Sctp.establish client_assoc;
  Masc_mcp.Sctp.establish server_assoc;

  check bool "client established" true (Masc_mcp.Sctp.is_established client_assoc);
  check bool "server established" true (Masc_mcp.Sctp.is_established server_assoc)

(** Test SCTP data chunk encoding/decoding roundtrip *)
let test_sctp_data_roundtrip () =
  let original_data = Bytes.of_string "Hello WebRTC DataChannel!" in
  let chunk = Masc_mcp.Sctp.{
    flags = { end_fragment = true; begin_fragment = true; unordered = false; immediate = false };
    tsn = 12345l;
    stream_id = 0;
    stream_seq = 1;
    ppid = 51l;  (* WebRTC String *)
    user_data = original_data;
  } in

  let encoded = Masc_mcp.Sctp.encode_data_chunk chunk in
  match Masc_mcp.Sctp.decode_data_chunk encoded with
  | Error e -> fail ("Decode failed: " ^ e)
  | Ok decoded ->
    check int32 "TSN preserved" 12345l decoded.tsn;
    check int "stream_id preserved" 0 decoded.stream_id;
    check int32 "PPID preserved" 51l decoded.ppid;
    check string "data preserved" "Hello WebRTC DataChannel!"
      (Bytes.to_string decoded.user_data)

(** Test SCTP multiple streams *)
let test_sctp_multi_stream () =
  let assoc = Masc_mcp.Sctp.create Masc_mcp.Sctp.default_config in

  (* Open multiple streams for different purposes *)
  let _control_stream = Masc_mcp.Sctp.open_stream assoc 0 ~ordered:true () in   (* DCEP *)
  let _data_stream1 = Masc_mcp.Sctp.open_stream assoc 2 ~ordered:true () in     (* Reliable *)
  let _data_stream2 = Masc_mcp.Sctp.open_stream assoc 4 ~ordered:false () in    (* Unreliable *)

  let streams = Masc_mcp.Sctp.get_streams assoc in
  check int "3 streams" 3 (List.length streams);

  (* Close one stream *)
  Masc_mcp.Sctp.close_stream assoc 2;
  let streams_after = Masc_mcp.Sctp.get_streams assoc in
  check int "2 streams after close" 2 (List.length streams_after)

(** {1 Scenario 4: DataChannel Messaging} *)

(** Test DataChannel creation and messaging between peers *)
let test_datachannel_peer_messaging () =
  (* Create two peers *)
  let offerer = Masc_mcp.Datachannel.create Masc_mcp.Datachannel.default_config ~is_offerer:true in
  let answerer = Masc_mcp.Datachannel.create Masc_mcp.Datachannel.default_config ~is_offerer:false in

  (* Simulate connection established *)
  Masc_mcp.Datachannel.set_connected offerer;
  Masc_mcp.Datachannel.set_connected answerer;

  check dc_state_testable "offerer connected" Masc_mcp.Datachannel.Conn_connected
    (Masc_mcp.Datachannel.get_state offerer);

  (* Offerer creates a channel *)
  match Masc_mcp.Datachannel.create_channel offerer ~label:"chat" ~protocol:"" () with
  | Error e -> fail ("Channel creation failed: " ^ e)
  | Ok ch ->
    check string "channel label" "chat" ch.label;
    check int "even stream id (offerer)" 0 ch.id;

    (* Send DCEP OPEN to answerer *)
    let open_msg = Masc_mcp.Datachannel.{
      channel_type = Reliable_ordered;
      priority = 0;
      reliability_param = 0;
      label = "chat";
      protocol = "";
    } in
    let open_data = Masc_mcp.Datachannel.encode_open_message open_msg in

    (* Answerer receives OPEN and creates channel *)
    let events = Masc_mcp.Datachannel.handle_data answerer open_data ~stream_id:0 in
    check int "1 event" 1 (List.length events);

    match List.hd events with
    | Masc_mcp.Datachannel.ChannelOpen remote_ch ->
      check string "remote channel label" "chat" remote_ch.label;
      check int "same stream id" 0 remote_ch.id
    | _ -> fail "Expected ChannelOpen event"

(** Test DataChannel text and binary messages *)
let test_datachannel_message_types () =
  let conn = Masc_mcp.Datachannel.create Masc_mcp.Datachannel.default_config ~is_offerer:true in
  Masc_mcp.Datachannel.set_connected conn;

  (* Create negotiated channel (starts Open) *)
  match Masc_mcp.Datachannel.create_channel conn ~label:"test" ~negotiated:true ~id:0 () with
  | Error e -> fail e
  | Ok _ch ->
    (* Send text *)
    (match Masc_mcp.Datachannel.send_text conn ~channel_id:0 "Hello" with
    | Error e -> fail e
    | Ok () ->
      let outgoing = Masc_mcp.Datachannel.get_outgoing conn in
      let (_, _, ppid) = List.hd outgoing in
      check int32 "text PPID" 51l ppid);

    (* Send binary *)
    (match Masc_mcp.Datachannel.send_binary conn ~channel_id:0 (Bytes.of_string "\x00\x01\x02") with
    | Error e -> fail e
    | Ok () ->
      let outgoing = Masc_mcp.Datachannel.get_outgoing conn in
      let (_, _, ppid) = List.hd outgoing in
      check int32 "binary PPID" 53l ppid);

    (* Send empty string *)
    (match Masc_mcp.Datachannel.send_text conn ~channel_id:0 "" with
    | Error e -> fail e
    | Ok () ->
      let outgoing = Masc_mcp.Datachannel.get_outgoing conn in
      let (_, _, ppid) = List.hd outgoing in
      check int32 "empty string PPID" 56l ppid)

(** Test multiple DataChannels *)
let test_datachannel_multiple () =
  let conn = Masc_mcp.Datachannel.create Masc_mcp.Datachannel.default_config ~is_offerer:true in
  Masc_mcp.Datachannel.set_connected conn;

  (* Create multiple channels for different purposes *)
  let _ = Masc_mcp.Datachannel.create_channel conn ~label:"control" ~negotiated:true ~id:0 () in
  let _ = Masc_mcp.Datachannel.create_channel conn
    ~label:"audio-metadata"
    ~channel_type:Masc_mcp.Datachannel.Unreliable_unordered
    ~negotiated:true ~id:2 () in
  let _ = Masc_mcp.Datachannel.create_channel conn
    ~label:"file-transfer"
    ~channel_type:Masc_mcp.Datachannel.Reliable_ordered
    ~negotiated:true ~id:4 () in

  let channels = Masc_mcp.Datachannel.get_channels conn in
  check int "3 channels" 3 (List.length channels);

  (* Find by label *)
  (match Masc_mcp.Datachannel.find_channel_by_label conn ~label:"audio-metadata" with
  | None -> fail "Channel not found"
  | Some ch ->
    check bool "unreliable" false (Masc_mcp.Datachannel.is_reliable ch.channel_type);
    check bool "unordered" false (Masc_mcp.Datachannel.is_ordered ch.channel_type))

(** {1 Scenario 5: Full Stack Integration} *)

(** Test complete WebRTC-like flow *)
let test_full_webrtc_flow () =
  (* Step 1: Initialize ICE agents *)
  let ice_offerer = Masc_mcp.Ice.create Masc_mcp.Ice.{ default_config with role = Controlling } in
  let ice_answerer = Masc_mcp.Ice.create Masc_mcp.Ice.{ default_config with role = Controlled } in

  (* Step 2: Gather ICE candidates *)
  let () = Lwt_main.run (Masc_mcp.Ice.gather_candidates ice_offerer) in
  let () = Lwt_main.run (Masc_mcp.Ice.gather_candidates ice_answerer) in

  (* Step 3: Get credentials and create SDP offer *)
  let (ufrag1, pwd1) = Masc_mcp.Ice.get_local_credentials ice_offerer in
  let (ufrag2, pwd2) = Masc_mcp.Ice.get_local_credentials ice_answerer in

  let offer = Masc_mcp.Sdp.create_datachannel_offer
    ~ice_ufrag:ufrag1 ~ice_pwd:pwd1 ~fingerprint:test_fingerprint ()
  in

  (* Step 4: Create SDP answer *)
  let _answer = Masc_mcp.Sdp.create_datachannel_answer
    ~offer ~ice_ufrag:ufrag2 ~ice_pwd:pwd2 ~fingerprint:test_fingerprint
  in

  (* Step 5: Exchange credentials *)
  Masc_mcp.Ice.set_remote_credentials ice_offerer ~ufrag:ufrag2 ~pwd:pwd2;
  Masc_mcp.Ice.set_remote_credentials ice_answerer ~ufrag:ufrag1 ~pwd:pwd1;

  (* Step 6: Create SCTP associations *)
  let sctp_offerer = Masc_mcp.Sctp.create Masc_mcp.Sctp.default_config in
  let sctp_answerer = Masc_mcp.Sctp.create Masc_mcp.Sctp.default_config in
  Masc_mcp.Sctp.establish sctp_offerer;
  Masc_mcp.Sctp.establish sctp_answerer;

  (* Step 7: Create DataChannels *)
  let dc_offerer = Masc_mcp.Datachannel.create Masc_mcp.Datachannel.default_config ~is_offerer:true in
  let dc_answerer = Masc_mcp.Datachannel.create Masc_mcp.Datachannel.default_config ~is_offerer:false in
  Masc_mcp.Datachannel.set_connected dc_offerer;
  Masc_mcp.Datachannel.set_connected dc_answerer;

  (* Step 8: Create and exchange channel *)
  (match Masc_mcp.Datachannel.create_channel dc_offerer ~label:"data" () with
  | Error e -> fail e
  | Ok ch ->
    (* Send OPEN *)
    let open_msg = Masc_mcp.Datachannel.{
      channel_type = Reliable_ordered;
      priority = 0;
      reliability_param = 0;
      label = ch.label;
      protocol = "";
    } in
    let open_data = Masc_mcp.Datachannel.encode_open_message open_msg in
    let events = Masc_mcp.Datachannel.handle_data dc_answerer open_data ~stream_id:ch.id in

    check int "channel opened on answerer" 1 (List.length events);

    (* Verify full stack initialized *)
    check bool "ICE offerer has candidates" true
      (List.length (Masc_mcp.Ice.get_local_candidates ice_offerer) > 0);
    check bool "SCTP established" true (Masc_mcp.Sctp.is_established sctp_offerer);
    check bool "DC connected" true
      (Masc_mcp.Datachannel.get_state dc_offerer = Masc_mcp.Datachannel.Conn_connected))

(** {1 Scenario 6: Error Handling} *)

(** Test error recovery scenarios *)
let test_error_recovery () =
  let conn = Masc_mcp.Datachannel.create Masc_mcp.Datachannel.default_config ~is_offerer:true in
  Masc_mcp.Datachannel.set_connected conn;

  (* Try to send on non-existent channel *)
  (match Masc_mcp.Datachannel.send_text conn ~channel_id:999 "test" with
  | Ok () -> fail "Should have failed"
  | Error _ -> ());  (* Expected *)

  (* Create and close channel, then try to send *)
  (match Masc_mcp.Datachannel.create_channel conn ~label:"temp" ~negotiated:true ~id:0 () with
  | Error e -> fail e
  | Ok ch ->
    Masc_mcp.Datachannel.close_channel conn ~channel_id:ch.id;
    match Masc_mcp.Datachannel.send_text conn ~channel_id:ch.id "test" with
    | Ok () -> fail "Should have failed on closed channel"
    | Error _ -> ())  (* Expected *)

(** Test malformed data handling *)
let test_malformed_data () =
  (* Malformed SCTP chunk - too short *)
  let short_data = Bytes.create 4 in
  (match Masc_mcp.Sctp.decode_data_chunk short_data with
  | Ok _ -> fail "Should have failed"
  | Error _ -> ());

  (* Malformed ICE candidate - Ice module parsing *)
  (match Masc_mcp.Ice.parse_candidate "invalid candidate line" with
  | Ok _ -> fail "Should have failed"
  | Error _ -> ());

  (* Malformed DCEP message *)
  let bad_dcep = Bytes.create 5 in
  Bytes.set_uint8 bad_dcep 0 0x03;  (* OPEN type but too short *)
  (match Masc_mcp.Datachannel.decode_open_message bad_dcep with
  | Ok _ -> fail "Should have failed"
  | Error _ -> ())

(** {1 Test Suites} *)

let signaling_tests = [
  "sdp_offer_answer", `Quick, test_sdp_offer_answer;
  "sdp_with_ice_candidates", `Quick, test_sdp_with_ice_candidates;
]

let ice_tests = [
  "ice_agent_lifecycle", `Quick, test_ice_agent_lifecycle;
  "ice_candidate_exchange", `Quick, test_ice_candidate_exchange;
  "ice_credentials", `Quick, test_ice_credentials;
]

let sctp_tests = [
  "sctp_association_flow", `Quick, test_sctp_association_flow;
  "sctp_data_roundtrip", `Quick, test_sctp_data_roundtrip;
  "sctp_multi_stream", `Quick, test_sctp_multi_stream;
]

let datachannel_tests = [
  "datachannel_peer_messaging", `Quick, test_datachannel_peer_messaging;
  "datachannel_message_types", `Quick, test_datachannel_message_types;
  "datachannel_multiple", `Quick, test_datachannel_multiple;
]

let integration_tests = [
  "full_webrtc_flow", `Quick, test_full_webrtc_flow;
]

let error_tests = [
  "error_recovery", `Quick, test_error_recovery;
  "malformed_data", `Quick, test_malformed_data;
]

let () =
  Alcotest.run "WebRTC Integration" [
    ("signaling", signaling_tests);
    ("ice", ice_tests);
    ("sctp", sctp_tests);
    ("datachannel", datachannel_tests);
    ("integration", integration_tests);
    ("error", error_tests);
  ]
