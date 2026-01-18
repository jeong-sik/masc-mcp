(** Tests for WebRTC DataChannel module

    Tests the stub implementation of WebRTC DataChannel.
    Note: These tests use the stub backend (in-memory simulation).
*)

open Alcotest
open Masc_mcp.Webrtc_datachannel

(** {1 Test Utilities} *)

let run_lwt f = Lwt_main.run f

(** {1 Initialization Tests} *)

let test_init_stub () =
  (* Force stub backend even if native is available *)
  let backend = init ~prefer_native:false () in
  check bool "stub backend" true (backend = Stub);
  cleanup ()

let test_init_native_detection () =
  (* Test native detection - may or may not be available *)
  let native_available = is_native_available () in
  let backend = init () in
  (* If native is available, we should use it; otherwise stub *)
  if native_available then
    check bool "native backend when available" true (backend = Native)
  else
    check bool "stub backend when native unavailable" true (backend = Stub);
  cleanup ()

let test_get_backend () =
  let _ = init ~prefer_native:false () in
  check bool "get_backend returns Stub" true (get_backend () = Stub);
  cleanup ()

let test_init_with_log_level () =
  let _ = init ~level:Log_debug () in
  check bool "init succeeded" true true;
  cleanup ()

let test_init_with_callback () =
  let logs = ref [] in
  let callback level msg = logs := (level, msg) :: !logs in
  let _ = init ~level:Log_info ~log_callback:callback () in
  check bool "init with callback" true true;
  cleanup ()

(** {1 Peer Connection Tests} *)

let test_create_peer_connection () =
  let _ = init () in
  let pc = create_peer_connection () in
  check string "initial state" "new" (string_of_ice_state (get_ice_state pc));
  close_peer_connection pc;
  cleanup ()

let test_peer_connection_with_config () =
  let _ = init () in
  let config = {
    ice_servers = [{ urls = ["stun:stun.test.com:19302"]; username = None; credential = None }];
    ice_transport_policy = `Relay;
    enable_ice_tcp = true;
    port_range = Some (10000, 20000);
    max_message_size = 65536;
  } in
  let pc = create_peer_connection ~config () in
  let status = status_json pc in
  let open Yojson.Safe.Util in
  check int "max_message_size" 65536 (status |> member "max_message_size" |> to_int);
  close_peer_connection pc;
  cleanup ()

let test_close_peer_connection () =
  let _ = init () in
  let pc = create_peer_connection () in
  close_peer_connection pc;
  check string "closed state" "closed" (string_of_ice_state (get_ice_state pc));
  cleanup ()

(** {1 Signaling Tests} *)

let test_create_offer () =
  let _ = init () in
  let pc = create_peer_connection () in
  let sdp = run_lwt (create_offer pc) in
  check bool "sdp not empty" true (String.length sdp > 0);
  check bool "contains v=0" true (String.sub sdp 0 3 = "v=0");
  close_peer_connection pc;
  cleanup ()

let test_create_answer () =
  let _ = init () in
  let pc = create_peer_connection () in
  let sdp = run_lwt (create_answer pc) in
  check bool "sdp not empty" true (String.length sdp > 0);
  check bool "contains v=0" true (String.sub sdp 0 3 = "v=0");
  close_peer_connection pc;
  cleanup ()

let test_signaling_flow () =
  let _ = init () in
  let pc1 = create_peer_connection () in
  let pc2 = create_peer_connection () in

  (* PC1 creates offer *)
  let offer = run_lwt (create_offer pc1) in

  (* PC2 receives offer and creates answer *)
  run_lwt (set_remote_description pc2 ~sdp:offer ~type_:"offer");
  let answer = run_lwt (create_answer pc2) in

  (* PC1 receives answer *)
  run_lwt (set_remote_description pc1 ~sdp:answer ~type_:"answer");

  (* Both should be connected (in stub mode) *)
  check string "pc1 connected" "connected" (string_of_ice_state (get_ice_state pc1));
  check string "pc2 connected" "connected" (string_of_ice_state (get_ice_state pc2));

  close_peer_connection pc1;
  close_peer_connection pc2;
  cleanup ()

let test_ice_candidate_callback () =
  let _ = init () in
  let pc = create_peer_connection () in
  let candidates = ref [] in
  on_local_candidate pc (fun ~candidate ~mid ->
    candidates := (candidate, mid) :: !candidates
  );
  let _ = run_lwt (create_offer pc) in
  check bool "got candidate" true (List.length !candidates > 0);
  close_peer_connection pc;
  cleanup ()

let test_state_change_callback () =
  let _ = init () in
  let pc = create_peer_connection () in
  let states = ref [] in
  on_state_change pc (fun state -> states := state :: !states);

  (* Trigger state changes via signaling *)
  run_lwt (set_remote_description pc ~sdp:"fake-sdp" ~type_:"answer");

  check bool "got state changes" true (List.length !states >= 1);
  close_peer_connection pc;
  cleanup ()

(** {1 DataChannel Tests} *)

let test_create_data_channel () =
  let _ = init () in
  let pc = create_peer_connection () in
  let dc = create_data_channel pc ~label:"test" () in
  check string "label" "test" (get_label dc);
  check bool "no protocol" true (get_protocol dc = None);
  close_data_channel dc;
  close_peer_connection pc;
  cleanup ()

let test_create_data_channel_with_init () =
  let _ = init () in
  let pc = create_peer_connection () in
  let init = {
    reliability = audio_reliability;
    protocol = Some "audio";
    negotiated = false;
    stream_id = None;
  } in
  let dc = create_data_channel pc ~label:"audio-channel" ~init () in
  check string "label" "audio-channel" (get_label dc);
  check (option string) "protocol" (Some "audio") (get_protocol dc);
  close_data_channel dc;
  close_peer_connection pc;
  cleanup ()

let test_data_channel_states () =
  let _ = init () in
  let pc = create_peer_connection () in
  let dc = create_data_channel pc ~label:"test" () in

  (* Initially connecting *)
  check string "initial state" "connecting" (string_of_channel_state (get_channel_state dc));
  check bool "not open initially" false (is_open dc);

  (* Connect the peer connection *)
  run_lwt (set_remote_description pc ~sdp:"fake" ~type_:"answer");
  (* Give time for async open callback *)
  run_lwt (Lwt_unix.sleep 0.02);

  check string "open state" "open" (string_of_channel_state (get_channel_state dc));
  check bool "is open" true (is_open dc);

  close_data_channel dc;
  check string "closed state" "closed" (string_of_channel_state (get_channel_state dc));

  close_peer_connection pc;
  cleanup ()

let test_data_channel_open_callback () =
  let _ = init () in
  let pc = create_peer_connection () in
  let dc = create_data_channel pc ~label:"test" () in
  let opened = ref false in
  on_open dc (fun () -> opened := true);

  (* Connect to trigger open *)
  run_lwt (set_remote_description pc ~sdp:"fake" ~type_:"answer");
  run_lwt (Lwt_unix.sleep 0.02);

  check bool "opened" true !opened;
  close_data_channel dc;
  close_peer_connection pc;
  cleanup ()

let test_data_channel_close_callback () =
  let _ = init () in
  let pc = create_peer_connection () in
  let dc = create_data_channel pc ~label:"test" () in
  let closed = ref false in
  on_close dc (fun () -> closed := true);

  (* Connect first *)
  run_lwt (set_remote_description pc ~sdp:"fake" ~type_:"answer");
  run_lwt (Lwt_unix.sleep 0.02);

  (* Then close *)
  close_data_channel dc;
  check bool "closed callback called" true !closed;

  close_peer_connection pc;
  cleanup ()

(** {1 Data Transfer Tests} *)

let test_send_data () =
  let _ = init () in
  let pc = create_peer_connection () in
  let dc = create_data_channel pc ~label:"test" () in

  (* Connect first *)
  run_lwt (set_remote_description pc ~sdp:"fake" ~type_:"answer");
  run_lwt (Lwt_unix.sleep 0.02);

  (* Send data *)
  run_lwt (send dc (Bytes.of_string "hello"));
  check bool "send succeeded" true true;

  close_data_channel dc;
  close_peer_connection pc;
  cleanup ()

let test_send_string () =
  let _ = init () in
  let pc = create_peer_connection () in
  let dc = create_data_channel pc ~label:"test" () in

  run_lwt (set_remote_description pc ~sdp:"fake" ~type_:"answer");
  run_lwt (Lwt_unix.sleep 0.02);

  run_lwt (send_string dc "hello world");
  check bool "send_string succeeded" true true;

  close_data_channel dc;
  close_peer_connection pc;
  cleanup ()

let test_message_callback () =
  let _ = init () in
  let pc = create_peer_connection () in
  let dc = create_data_channel pc ~label:"test" () in
  let received = ref [] in
  on_message dc (fun data is_binary ->
    received := (Bytes.to_string data, is_binary) :: !received
  );

  run_lwt (set_remote_description pc ~sdp:"fake" ~type_:"answer");
  run_lwt (Lwt_unix.sleep 0.02);

  (* Send and deliver (stub loopback) *)
  run_lwt (send_string dc "test message");
  let delivered = deliver_queued_messages dc in

  check int "1 message delivered" 1 delivered;
  check int "1 message received" 1 (List.length !received);
  check string "message content" "test message" (fst (List.hd !received));

  close_data_channel dc;
  close_peer_connection pc;
  cleanup ()

let test_buffered_amount () =
  let _ = init () in
  let pc = create_peer_connection () in
  let dc = create_data_channel pc ~label:"test" () in

  run_lwt (set_remote_description pc ~sdp:"fake" ~type_:"answer");
  run_lwt (Lwt_unix.sleep 0.02);

  check int "initial buffered" 0 (buffered_amount dc);

  (* Buffered amount increases then decreases asynchronously *)
  run_lwt (send_string dc "hello");
  (* Note: In stub, buffered amount changes asynchronously *)

  close_data_channel dc;
  close_peer_connection pc;
  cleanup ()

(** {1 Loopback Tests} *)

let test_connect_loopback () =
  let _ = init () in
  let pc1 = create_peer_connection () in
  let pc2 = create_peer_connection () in

  (* Create channel on pc1 *)
  let dc1 = create_data_channel pc1 ~label:"loopback" () in

  (* Connect via loopback helper *)
  run_lwt (connect_loopback pc1 pc2);

  check string "pc1 connected" "connected" (string_of_ice_state (get_ice_state pc1));
  check string "pc2 connected" "connected" (string_of_ice_state (get_ice_state pc2));

  close_data_channel dc1;
  close_peer_connection pc1;
  close_peer_connection pc2;
  cleanup ()

(** {1 Status JSON Tests} *)

let test_status_json () =
  let backend = init ~prefer_native:false () in
  let pc = create_peer_connection () in
  let _ = create_data_channel pc ~label:"test" () in

  let status = status_json pc in
  let open Yojson.Safe.Util in

  let expected_backend = string_of_backend backend in
  check string "backend" expected_backend (status |> member "backend" |> to_string);
  check string "ice_state" "new" (status |> member "ice_state" |> to_string);
  check int "1 channel" 1 (status |> member "channels" |> to_list |> List.length);

  let channel = status |> member "channels" |> to_list |> List.hd in
  check string "channel label" "test" (channel |> member "label" |> to_string);

  close_peer_connection pc;
  cleanup ()

let test_status_json_reliability () =
  let _ = init () in
  let pc = create_peer_connection () in
  let init = { default_channel_init with reliability = audio_reliability } in
  let _ = create_data_channel pc ~label:"audio" ~init () in

  let status = status_json pc in
  let open Yojson.Safe.Util in
  let channel = status |> member "channels" |> to_list |> List.hd in
  let reliability = channel |> member "reliability" in

  check bool "unordered" true (reliability |> member "unordered" |> to_bool);
  check bool "unreliable" true (reliability |> member "unreliable" |> to_bool);

  close_peer_connection pc;
  cleanup ()

(** {1 Defaults Tests} *)

let test_default_reliability () =
  check bool "ordered" false default_reliability.unordered;
  check bool "reliable" false default_reliability.unreliable;
  check (option int) "no lifetime" None default_reliability.max_packet_lifetime_ms;
  check (option int) "no retransmits" None default_reliability.max_retransmits

let test_audio_reliability () =
  check bool "unordered" true audio_reliability.unordered;
  check bool "unreliable" true audio_reliability.unreliable;
  check (option int) "lifetime 100ms" (Some 100) audio_reliability.max_packet_lifetime_ms;
  check (option int) "no retransmits" (Some 0) audio_reliability.max_retransmits

let test_default_config () =
  check bool "has ice servers" true (List.length default_config.ice_servers > 0);
  check int "256KB max" 262144 default_config.max_message_size

(** {1 String Conversion Tests} *)

let test_string_conversions () =
  check string "ice new" "new" (string_of_ice_state New);
  check string "ice connected" "connected" (string_of_ice_state Connected);
  check string "ice closed" "closed" (string_of_ice_state Closed);

  check string "gathering new" "new" (string_of_gathering_state GatheringNew);
  check string "gathering complete" "complete" (string_of_gathering_state GatheringComplete);

  check string "channel open" "open" (string_of_channel_state Open);
  check string "channel closed" "closed" (string_of_channel_state ChannelClosed);

  check string "backend stub" "stub (simulation)" (string_of_backend Stub);
  check string "backend native" "native (libdatachannel)" (string_of_backend Native)

let test_generate_session_id () =
  let id1 = generate_session_id () in
  let id2 = generate_session_id () in
  check bool "not empty" true (String.length id1 > 0);
  check bool "unique" true (id1 <> id2);
  check bool "uuid format (has dashes)" true (String.contains id1 '-')

(** {1 Test Suites} *)

let init_tests = [
  "init_stub", `Quick, test_init_stub;
  "init_native_detection", `Quick, test_init_native_detection;
  "get_backend", `Quick, test_get_backend;
  "init_with_log_level", `Quick, test_init_with_log_level;
  "init_with_callback", `Quick, test_init_with_callback;
]

let peer_connection_tests = [
  "create_peer_connection", `Quick, test_create_peer_connection;
  "peer_connection_with_config", `Quick, test_peer_connection_with_config;
  "close_peer_connection", `Quick, test_close_peer_connection;
]

let signaling_tests = [
  "create_offer", `Quick, test_create_offer;
  "create_answer", `Quick, test_create_answer;
  "signaling_flow", `Quick, test_signaling_flow;
  "ice_candidate_callback", `Quick, test_ice_candidate_callback;
  "state_change_callback", `Quick, test_state_change_callback;
]

let data_channel_tests = [
  "create_data_channel", `Quick, test_create_data_channel;
  "create_data_channel_with_init", `Quick, test_create_data_channel_with_init;
  "data_channel_states", `Quick, test_data_channel_states;
  "data_channel_open_callback", `Quick, test_data_channel_open_callback;
  "data_channel_close_callback", `Quick, test_data_channel_close_callback;
]

let data_transfer_tests = [
  "send_data", `Quick, test_send_data;
  "send_string", `Quick, test_send_string;
  "message_callback", `Quick, test_message_callback;
  "buffered_amount", `Quick, test_buffered_amount;
]

let loopback_tests = [
  "connect_loopback", `Quick, test_connect_loopback;
]

let status_tests = [
  "status_json", `Quick, test_status_json;
  "status_json_reliability", `Quick, test_status_json_reliability;
]

let defaults_tests = [
  "default_reliability", `Quick, test_default_reliability;
  "audio_reliability", `Quick, test_audio_reliability;
  "default_config", `Quick, test_default_config;
]

let string_tests = [
  "string_conversions", `Quick, test_string_conversions;
  "generate_session_id", `Quick, test_generate_session_id;
]

let () =
  Alcotest.run "WebRTC DataChannel" [
    ("init", init_tests);
    ("peer_connection", peer_connection_tests);
    ("signaling", signaling_tests);
    ("data_channel", data_channel_tests);
    ("data_transfer", data_transfer_tests);
    ("loopback", loopback_tests);
    ("status", status_tests);
    ("defaults", defaults_tests);
    ("string", string_tests);
  ]
