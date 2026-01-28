(** DTLS Module Coverage Tests

    Tests for DTLS (Datagram Transport Layer Security):
    - Content types: string_of_content_type, content_type_of_int, int_of_content_type
    - Handshake types: string_of_handshake_type, handshake_type_of_int
    - State strings: string_of_state, string_of_handshake_state
    - Alert strings: string_of_alert_level, string_of_alert_description
    - Record encoding: encode_record_header, decode_record_header
    - DTLS detection: is_dtls_data
    - Window constants: window_size
*)

open Alcotest

module Dtls = Masc_mcp.Dtls

(* ============================================================
   Content Type Tests
   ============================================================ *)

let test_string_of_content_type_change_cipher_spec () =
  let s = Dtls.string_of_content_type Dtls.ChangeCipherSpec in
  check string "change-cipher-spec" "change-cipher-spec" s

let test_string_of_content_type_alert () =
  let s = Dtls.string_of_content_type Dtls.Alert in
  check string "alert" "alert" s

let test_string_of_content_type_handshake () =
  let s = Dtls.string_of_content_type Dtls.Handshake in
  check string "handshake" "handshake" s

let test_string_of_content_type_application_data () =
  let s = Dtls.string_of_content_type Dtls.ApplicationData in
  check string "application-data" "application-data" s

let test_string_of_content_type_unknown () =
  let s = Dtls.string_of_content_type (Dtls.Unknown 99) in
  check string "unknown(99)" "unknown(99)" s

let test_content_type_of_int_20 () =
  match Dtls.content_type_of_int 20 with
  | Dtls.ChangeCipherSpec -> check bool "ChangeCipherSpec" true true
  | _ -> fail "expected ChangeCipherSpec"

let test_content_type_of_int_21 () =
  match Dtls.content_type_of_int 21 with
  | Dtls.Alert -> check bool "Alert" true true
  | _ -> fail "expected Alert"

let test_content_type_of_int_22 () =
  match Dtls.content_type_of_int 22 with
  | Dtls.Handshake -> check bool "Handshake" true true
  | _ -> fail "expected Handshake"

let test_content_type_of_int_23 () =
  match Dtls.content_type_of_int 23 with
  | Dtls.ApplicationData -> check bool "ApplicationData" true true
  | _ -> fail "expected ApplicationData"

let test_content_type_of_int_unknown () =
  match Dtls.content_type_of_int 99 with
  | Dtls.Unknown 99 -> check bool "Unknown" true true
  | _ -> fail "expected Unknown"

let test_int_of_content_type_change_cipher_spec () =
  check int "20" 20 (Dtls.int_of_content_type Dtls.ChangeCipherSpec)

let test_int_of_content_type_alert () =
  check int "21" 21 (Dtls.int_of_content_type Dtls.Alert)

let test_int_of_content_type_handshake () =
  check int "22" 22 (Dtls.int_of_content_type Dtls.Handshake)

let test_int_of_content_type_application_data () =
  check int "23" 23 (Dtls.int_of_content_type Dtls.ApplicationData)

let test_int_of_content_type_unknown () =
  check int "99" 99 (Dtls.int_of_content_type (Dtls.Unknown 99))

(* ============================================================
   Handshake Type Tests
   ============================================================ *)

let test_string_of_handshake_type_hello_request () =
  let s = Dtls.string_of_handshake_type Dtls.HelloRequest in
  check string "hello-request" "hello-request" s

let test_string_of_handshake_type_client_hello () =
  let s = Dtls.string_of_handshake_type Dtls.ClientHello in
  check string "client-hello" "client-hello" s

let test_string_of_handshake_type_server_hello () =
  let s = Dtls.string_of_handshake_type Dtls.ServerHello in
  check string "server-hello" "server-hello" s

let test_string_of_handshake_type_certificate () =
  let s = Dtls.string_of_handshake_type Dtls.Certificate in
  check string "certificate" "certificate" s

let test_string_of_handshake_type_server_key_exchange () =
  let s = Dtls.string_of_handshake_type Dtls.ServerKeyExchange in
  check string "server-key-exchange" "server-key-exchange" s

let test_string_of_handshake_type_finished () =
  let s = Dtls.string_of_handshake_type Dtls.Finished in
  check string "finished" "finished" s

let test_handshake_type_of_int_0 () =
  match Dtls.handshake_type_of_int 0 with
  | Dtls.HelloRequest -> check bool "HelloRequest" true true
  | _ -> fail "expected HelloRequest"

let test_handshake_type_of_int_1 () =
  match Dtls.handshake_type_of_int 1 with
  | Dtls.ClientHello -> check bool "ClientHello" true true
  | _ -> fail "expected ClientHello"

let test_handshake_type_of_int_2 () =
  match Dtls.handshake_type_of_int 2 with
  | Dtls.ServerHello -> check bool "ServerHello" true true
  | _ -> fail "expected ServerHello"

let test_handshake_type_of_int_11 () =
  match Dtls.handshake_type_of_int 11 with
  | Dtls.Certificate -> check bool "Certificate" true true
  | _ -> fail "expected Certificate"

let test_handshake_type_of_int_20 () =
  match Dtls.handshake_type_of_int 20 with
  | Dtls.Finished -> check bool "Finished" true true
  | _ -> fail "expected Finished"

let test_handshake_type_of_int_unknown () =
  match Dtls.handshake_type_of_int 99 with
  | Dtls.Unknown 99 -> check bool "Unknown" true true
  | _ -> fail "expected Unknown"

(* ============================================================
   State String Tests
   ============================================================ *)

let test_string_of_state_closed () =
  let s = Dtls.string_of_state Dtls.Closed in
  check string "closed" "closed" s

let test_string_of_state_connecting () =
  let s = Dtls.string_of_state Dtls.Connecting in
  check string "connecting" "connecting" s

let test_string_of_state_connected () =
  let s = Dtls.string_of_state Dtls.Connected in
  check string "connected" "connected" s

let test_string_of_state_failed () =
  let s = Dtls.string_of_state Dtls.Failed in
  check string "failed" "failed" s

let test_string_of_handshake_state_initial () =
  let s = Dtls.string_of_handshake_state Dtls.Initial in
  check string "initial" "initial" s

let test_string_of_handshake_state_hello_sent () =
  let s = Dtls.string_of_handshake_state Dtls.HelloSent in
  check string "hello-sent" "hello-sent" s

let test_string_of_handshake_state_hello_verify_received () =
  let s = Dtls.string_of_handshake_state Dtls.HelloVerifyReceived in
  check string "hello-verify-received" "hello-verify-received" s

let test_string_of_handshake_state_complete () =
  let s = Dtls.string_of_handshake_state Dtls.Complete in
  check string "complete" "complete" s

(* ============================================================
   Alert String Tests
   ============================================================ *)

let test_string_of_alert_level_warning () =
  let s = Dtls.string_of_alert_level Dtls.Warning in
  check string "warning" "warning" s

let test_string_of_alert_level_fatal () =
  let s = Dtls.string_of_alert_level Dtls.Fatal in
  check string "fatal" "fatal" s

let test_string_of_alert_description_close_notify () =
  let s = Dtls.string_of_alert_description Dtls.CloseNotify in
  check string "close-notify" "close-notify" s

let test_string_of_alert_description_unexpected_message () =
  let s = Dtls.string_of_alert_description Dtls.UnexpectedMessage in
  check string "unexpected-message" "unexpected-message" s

let test_string_of_alert_description_bad_record_mac () =
  let s = Dtls.string_of_alert_description Dtls.BadRecordMac in
  check string "bad-record-mac" "bad-record-mac" s

let test_string_of_alert_description_handshake_failure () =
  let s = Dtls.string_of_alert_description Dtls.HandshakeFailure in
  check string "handshake-failure" "handshake-failure" s

let test_string_of_alert_description_certificate_expired () =
  let s = Dtls.string_of_alert_description Dtls.CertificateExpired in
  check string "certificate-expired" "certificate-expired" s

let test_string_of_alert_description_unknown_ca () =
  let s = Dtls.string_of_alert_description Dtls.UnknownCA in
  check string "unknown-ca" "unknown-ca" s

(* ============================================================
   Record Header Tests
   ============================================================ *)

let test_encode_record_header_length () =
  let header : Dtls.record_header = {
    content_type = Dtls.ApplicationData;
    version_major = 254;  (* DTLS 1.2 *)
    version_minor = 253;
    epoch = 0;
    sequence_number = 0L;
    length = 100;
  } in
  let encoded = Dtls.encode_record_header header in
  (* 1 (type) + 2 (version) + 2 (epoch) + 6 (seq) + 2 (length) = 13 *)
  check int "length" 13 (Bytes.length encoded)

let test_decode_record_header_valid () =
  let header : Dtls.record_header = {
    content_type = Dtls.Handshake;
    version_major = 254;
    version_minor = 253;
    epoch = 1;
    sequence_number = 42L;
    length = 256;
  } in
  let encoded = Dtls.encode_record_header header in
  match Dtls.decode_record_header encoded with
  | Ok decoded ->
      check int "epoch" 1 decoded.epoch;
      check int "length" 256 decoded.length
  | Error e -> fail ("expected Ok: " ^ e)

let test_decode_record_header_too_short () =
  let short = Bytes.of_string "short" in
  match Dtls.decode_record_header short with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected Error"

(* ============================================================
   Record Encode/Decode Tests
   ============================================================ *)

let test_encode_record_length () =
  let header : Dtls.record_header = {
    content_type = Dtls.ApplicationData;
    version_major = 254;
    version_minor = 253;
    epoch = 0;
    sequence_number = 0L;
    length = 5;
  } in
  let fragment = Bytes.of_string "hello" in
  let record : Dtls.record = { header; fragment } in
  let encoded = Dtls.encode_record record in
  (* 13 (header) + 5 (fragment) = 18 *)
  check int "total length" 18 (Bytes.length encoded)

let test_encode_record_header_preserved () =
  let header : Dtls.record_header = {
    content_type = Dtls.Handshake;
    version_major = 254;
    version_minor = 253;
    epoch = 1;
    sequence_number = 123L;
    length = 4;
  } in
  let fragment = Bytes.of_string "test" in
  let record : Dtls.record = { header; fragment } in
  let encoded = Dtls.encode_record record in
  (* First byte should be content type (22 = Handshake) *)
  check int "content type byte" 22 (Bytes.get_uint8 encoded 0)

let test_encode_record_fragment_at_end () =
  let header : Dtls.record_header = {
    content_type = Dtls.ApplicationData;
    version_major = 254;
    version_minor = 253;
    epoch = 0;
    sequence_number = 0L;
    length = 3;
  } in
  let fragment = Bytes.of_string "abc" in
  let record : Dtls.record = { header; fragment } in
  let encoded = Dtls.encode_record record in
  (* Fragment should be at offset 13 *)
  let extracted = Bytes.sub encoded 13 3 in
  check string "fragment" "abc" (Bytes.to_string extracted)

let test_decode_record_valid () =
  let header : Dtls.record_header = {
    content_type = Dtls.ApplicationData;
    version_major = 254;
    version_minor = 253;
    epoch = 2;
    sequence_number = 99L;
    length = 6;
  } in
  let fragment = Bytes.of_string "foobar" in
  let record : Dtls.record = { header; fragment } in
  let encoded = Dtls.encode_record record in
  match Dtls.decode_record encoded with
  | Ok decoded ->
      check int "epoch" 2 decoded.header.epoch;
      check string "fragment" "foobar" (Bytes.to_string decoded.fragment)
  | Error e -> fail ("expected Ok: " ^ e)

let test_decode_record_too_short () =
  (* Create a valid header but truncate the fragment *)
  let header : Dtls.record_header = {
    content_type = Dtls.ApplicationData;
    version_major = 254;
    version_minor = 253;
    epoch = 0;
    sequence_number = 0L;
    length = 100;  (* Claims 100 bytes but we'll only provide 13 (header only) *)
  } in
  let header_bytes = Dtls.encode_record_header header in
  match Dtls.decode_record header_bytes with
  | Error _ -> check bool "error for truncated" true true
  | Ok _ -> fail "expected Error"

let test_decode_record_roundtrip () =
  let header : Dtls.record_header = {
    content_type = Dtls.Handshake;
    version_major = 254;
    version_minor = 253;
    epoch = 3;
    sequence_number = 12345L;
    length = 11;
  } in
  let fragment = Bytes.of_string "hello world" in
  let original : Dtls.record = { header; fragment } in
  let encoded = Dtls.encode_record original in
  match Dtls.decode_record encoded with
  | Ok decoded ->
      check int "epoch roundtrip" original.header.epoch decoded.header.epoch;
      check int "length roundtrip" original.header.length decoded.header.length;
      check string "fragment roundtrip"
        (Bytes.to_string original.fragment)
        (Bytes.to_string decoded.fragment)
  | Error e -> fail ("roundtrip failed: " ^ e)

let test_decode_record_empty_fragment () =
  let header : Dtls.record_header = {
    content_type = Dtls.ApplicationData;
    version_major = 254;
    version_minor = 253;
    epoch = 0;
    sequence_number = 0L;
    length = 0;  (* Empty fragment *)
  } in
  let fragment = Bytes.create 0 in
  let record : Dtls.record = { header; fragment } in
  let encoded = Dtls.encode_record record in
  match Dtls.decode_record encoded with
  | Ok decoded -> check int "empty fragment" 0 (Bytes.length decoded.fragment)
  | Error e -> fail ("expected Ok: " ^ e)

(* ============================================================
   DTLS Detection Tests
   ============================================================ *)

let test_is_dtls_data_handshake () =
  (* DTLS handshake: content_type=22, version=254.253 *)
  let data = Bytes.create 13 in
  Bytes.set data 0 (Char.chr 22);  (* Handshake *)
  Bytes.set data 1 (Char.chr 254);  (* Version major *)
  Bytes.set data 2 (Char.chr 253);  (* Version minor *)
  check bool "handshake" true (Dtls.is_dtls_data data)

let test_is_dtls_data_application () =
  let data = Bytes.create 13 in
  Bytes.set data 0 (Char.chr 23);  (* Application data *)
  Bytes.set data 1 (Char.chr 254);
  Bytes.set data 2 (Char.chr 253);
  check bool "application" true (Dtls.is_dtls_data data)

let test_is_dtls_data_too_short () =
  let short = Bytes.of_string "hi" in
  check bool "too short" false (Dtls.is_dtls_data short)

let test_is_dtls_data_invalid_type () =
  let data = Bytes.create 13 in
  Bytes.set data 0 (Char.chr 99);  (* Invalid type *)
  Bytes.set data 1 (Char.chr 254);
  Bytes.set data 2 (Char.chr 253);
  check bool "invalid type" false (Dtls.is_dtls_data data)

let test_is_dtls_data_wrong_version () =
  let data = Bytes.create 13 in
  Bytes.set data 0 (Char.chr 22);
  Bytes.set data 1 (Char.chr 3);  (* TLS, not DTLS *)
  Bytes.set data 2 (Char.chr 3);
  check bool "wrong version" false (Dtls.is_dtls_data data)

(* ============================================================
   Window Constants Tests
   ============================================================ *)

let test_window_size () =
  check int "window_size" 64 Dtls.window_size

(* ============================================================
   Default Config Tests
   ============================================================ *)

let test_default_config_mtu () =
  check int "mtu" 1200 Dtls.default_config.mtu

let test_default_config_retransmit_timeout_ms () =
  check int "retransmit_timeout_ms" 1000 Dtls.default_config.retransmit_timeout_ms

let test_default_config_max_retransmits () =
  check int "max_retransmits" 5 Dtls.default_config.max_retransmits

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "DTLS Coverage" [
    "content_type", [
      test_case "string ChangeCipherSpec" `Quick test_string_of_content_type_change_cipher_spec;
      test_case "string Alert" `Quick test_string_of_content_type_alert;
      test_case "string Handshake" `Quick test_string_of_content_type_handshake;
      test_case "string ApplicationData" `Quick test_string_of_content_type_application_data;
      test_case "string Unknown" `Quick test_string_of_content_type_unknown;
      test_case "of_int 20" `Quick test_content_type_of_int_20;
      test_case "of_int 21" `Quick test_content_type_of_int_21;
      test_case "of_int 22" `Quick test_content_type_of_int_22;
      test_case "of_int 23" `Quick test_content_type_of_int_23;
      test_case "of_int unknown" `Quick test_content_type_of_int_unknown;
      test_case "int_of ChangeCipherSpec" `Quick test_int_of_content_type_change_cipher_spec;
      test_case "int_of Alert" `Quick test_int_of_content_type_alert;
      test_case "int_of Handshake" `Quick test_int_of_content_type_handshake;
      test_case "int_of ApplicationData" `Quick test_int_of_content_type_application_data;
      test_case "int_of Unknown" `Quick test_int_of_content_type_unknown;
    ];
    "handshake_type", [
      test_case "string HelloRequest" `Quick test_string_of_handshake_type_hello_request;
      test_case "string ClientHello" `Quick test_string_of_handshake_type_client_hello;
      test_case "string ServerHello" `Quick test_string_of_handshake_type_server_hello;
      test_case "string Certificate" `Quick test_string_of_handshake_type_certificate;
      test_case "string ServerKeyExchange" `Quick test_string_of_handshake_type_server_key_exchange;
      test_case "string Finished" `Quick test_string_of_handshake_type_finished;
      test_case "of_int 0" `Quick test_handshake_type_of_int_0;
      test_case "of_int 1" `Quick test_handshake_type_of_int_1;
      test_case "of_int 2" `Quick test_handshake_type_of_int_2;
      test_case "of_int 11" `Quick test_handshake_type_of_int_11;
      test_case "of_int 20" `Quick test_handshake_type_of_int_20;
      test_case "of_int unknown" `Quick test_handshake_type_of_int_unknown;
    ];
    "state_strings", [
      test_case "state Closed" `Quick test_string_of_state_closed;
      test_case "state Connecting" `Quick test_string_of_state_connecting;
      test_case "state Connected" `Quick test_string_of_state_connected;
      test_case "state Failed" `Quick test_string_of_state_failed;
      test_case "handshake Initial" `Quick test_string_of_handshake_state_initial;
      test_case "handshake HelloSent" `Quick test_string_of_handshake_state_hello_sent;
      test_case "handshake HelloVerifyReceived" `Quick test_string_of_handshake_state_hello_verify_received;
      test_case "handshake Complete" `Quick test_string_of_handshake_state_complete;
    ];
    "alert_strings", [
      test_case "level Warning" `Quick test_string_of_alert_level_warning;
      test_case "level Fatal" `Quick test_string_of_alert_level_fatal;
      test_case "CloseNotify" `Quick test_string_of_alert_description_close_notify;
      test_case "UnexpectedMessage" `Quick test_string_of_alert_description_unexpected_message;
      test_case "BadRecordMac" `Quick test_string_of_alert_description_bad_record_mac;
      test_case "HandshakeFailure" `Quick test_string_of_alert_description_handshake_failure;
      test_case "CertificateExpired" `Quick test_string_of_alert_description_certificate_expired;
      test_case "UnknownCA" `Quick test_string_of_alert_description_unknown_ca;
    ];
    "record_header", [
      test_case "encode length" `Quick test_encode_record_header_length;
      test_case "decode valid" `Quick test_decode_record_header_valid;
      test_case "decode too short" `Quick test_decode_record_header_too_short;
    ];
    "record", [
      test_case "encode length" `Quick test_encode_record_length;
      test_case "encode header preserved" `Quick test_encode_record_header_preserved;
      test_case "encode fragment at end" `Quick test_encode_record_fragment_at_end;
      test_case "decode valid" `Quick test_decode_record_valid;
      test_case "decode too short" `Quick test_decode_record_too_short;
      test_case "roundtrip" `Quick test_decode_record_roundtrip;
      test_case "empty fragment" `Quick test_decode_record_empty_fragment;
    ];
    "is_dtls_data", [
      test_case "handshake" `Quick test_is_dtls_data_handshake;
      test_case "application" `Quick test_is_dtls_data_application;
      test_case "too short" `Quick test_is_dtls_data_too_short;
      test_case "invalid type" `Quick test_is_dtls_data_invalid_type;
      test_case "wrong version" `Quick test_is_dtls_data_wrong_version;
    ];
    "constants", [
      test_case "window_size" `Quick test_window_size;
    ];
    "default_config", [
      test_case "mtu" `Quick test_default_config_mtu;
      test_case "retransmit_timeout_ms" `Quick test_default_config_retransmit_timeout_ms;
      test_case "max_retransmits" `Quick test_default_config_max_retransmits;
    ];
  ]
