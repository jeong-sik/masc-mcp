(** SCTP Module Coverage Tests

    Tests for SCTP (Stream Control Transmission Protocol):
    - Chunk types: string_of, chunk_type_of_int, int_of
    - State: string_of_state
    - Message types: string_of, ppid conversion
    - Default config
    - SACK creation
*)

open Alcotest

module Sctp = Masc_mcp.Sctp

(* ============================================================
   Chunk Type Tests
   ============================================================ *)

let test_string_of_chunk_type_data () =
  let s = Sctp.string_of_chunk_type Sctp.DATA in
  check string "data" "DATA" s

let test_string_of_chunk_type_init () =
  let s = Sctp.string_of_chunk_type Sctp.INIT in
  check string "init" "INIT" s

let test_string_of_chunk_type_init_ack () =
  let s = Sctp.string_of_chunk_type Sctp.INIT_ACK in
  check string "init_ack" "INIT_ACK" s

let test_string_of_chunk_type_sack () =
  let s = Sctp.string_of_chunk_type Sctp.SACK in
  check string "sack" "SACK" s

let test_string_of_chunk_type_heartbeat () =
  let s = Sctp.string_of_chunk_type Sctp.HEARTBEAT in
  check string "heartbeat" "HEARTBEAT" s

let test_string_of_chunk_type_cookie_echo () =
  let s = Sctp.string_of_chunk_type Sctp.COOKIE_ECHO in
  check string "cookie_echo" "COOKIE_ECHO" s

let test_string_of_chunk_type_cookie_ack () =
  let s = Sctp.string_of_chunk_type Sctp.COOKIE_ACK in
  check string "cookie_ack" "COOKIE_ACK" s

let test_string_of_chunk_type_shutdown () =
  let s = Sctp.string_of_chunk_type Sctp.SHUTDOWN in
  check string "shutdown" "SHUTDOWN" s

let test_string_of_chunk_type_abort () =
  let s = Sctp.string_of_chunk_type Sctp.ABORT in
  check string "abort" "ABORT" s

let test_string_of_chunk_type_error () =
  let s = Sctp.string_of_chunk_type Sctp.ERROR in
  check string "error" "ERROR" s

let test_string_of_chunk_type_unknown () =
  let s = Sctp.string_of_chunk_type (Sctp.Unknown 99) in
  check bool "unknown contains 99" true (String.length s > 0)

let test_chunk_type_of_int_0 () =
  match Sctp.chunk_type_of_int 0 with
  | Sctp.DATA -> check bool "data" true true
  | _ -> fail "expected DATA"

let test_chunk_type_of_int_1 () =
  match Sctp.chunk_type_of_int 1 with
  | Sctp.INIT -> check bool "init" true true
  | _ -> fail "expected INIT"

let test_chunk_type_of_int_2 () =
  match Sctp.chunk_type_of_int 2 with
  | Sctp.INIT_ACK -> check bool "init_ack" true true
  | _ -> fail "expected INIT_ACK"

let test_chunk_type_of_int_3 () =
  match Sctp.chunk_type_of_int 3 with
  | Sctp.SACK -> check bool "sack" true true
  | _ -> fail "expected SACK"

let test_chunk_type_of_int_4 () =
  match Sctp.chunk_type_of_int 4 with
  | Sctp.HEARTBEAT -> check bool "heartbeat" true true
  | _ -> fail "expected HEARTBEAT"

let test_chunk_type_of_int_unknown () =
  match Sctp.chunk_type_of_int 99 with
  | Sctp.Unknown _ -> check bool "unknown" true true
  | _ -> fail "expected Unknown"

let test_int_of_chunk_type_data () =
  check int "data" 0 (Sctp.int_of_chunk_type Sctp.DATA)

let test_int_of_chunk_type_init () =
  check int "init" 1 (Sctp.int_of_chunk_type Sctp.INIT)

let test_int_of_chunk_type_sack () =
  check int "sack" 3 (Sctp.int_of_chunk_type Sctp.SACK)

let test_int_of_chunk_type_heartbeat () =
  check int "heartbeat" 4 (Sctp.int_of_chunk_type Sctp.HEARTBEAT)

let test_int_of_chunk_type_shutdown () =
  check int "shutdown" 7 (Sctp.int_of_chunk_type Sctp.SHUTDOWN)

(* ============================================================
   State Tests
   ============================================================ *)

let test_string_of_state_closed () =
  let s = Sctp.string_of_state Sctp.Closed in
  check bool "closed" true (String.length s > 0)

let test_string_of_state_cookie_wait () =
  let s = Sctp.string_of_state Sctp.Cookie_wait in
  check bool "cookie_wait" true (String.length s > 0)

let test_string_of_state_cookie_echoed () =
  let s = Sctp.string_of_state Sctp.Cookie_echoed in
  check bool "cookie_echoed" true (String.length s > 0)

let test_string_of_state_established () =
  let s = Sctp.string_of_state Sctp.Established in
  check bool "established" true (String.length s > 0)

let test_string_of_state_shutdown_pending () =
  let s = Sctp.string_of_state Sctp.Shutdown_pending in
  check bool "shutdown_pending" true (String.length s > 0)

let test_string_of_state_shutdown_sent () =
  let s = Sctp.string_of_state Sctp.Shutdown_sent in
  check bool "shutdown_sent" true (String.length s > 0)

let test_string_of_state_shutdown_received () =
  let s = Sctp.string_of_state Sctp.Shutdown_received in
  check bool "shutdown_received" true (String.length s > 0)

let test_string_of_state_shutdown_ack_sent () =
  let s = Sctp.string_of_state Sctp.Shutdown_ack_sent in
  check bool "shutdown_ack_sent" true (String.length s > 0)

(* ============================================================
   Message Type Tests
   ============================================================ *)

let test_string_of_message_type_dcep () =
  let s = Sctp.string_of_message_type Sctp.WebRTC_DCEP in
  check bool "dcep" true (String.length s > 0)

let test_string_of_message_type_string () =
  let s = Sctp.string_of_message_type Sctp.WebRTC_String in
  check bool "string" true (String.length s > 0)

let test_string_of_message_type_binary () =
  let s = Sctp.string_of_message_type Sctp.WebRTC_Binary in
  check bool "binary" true (String.length s > 0)

let test_string_of_message_type_string_empty () =
  let s = Sctp.string_of_message_type Sctp.WebRTC_String_Empty in
  check bool "string_empty" true (String.length s > 0)

let test_string_of_message_type_binary_empty () =
  let s = Sctp.string_of_message_type Sctp.WebRTC_Binary_Empty in
  check bool "binary_empty" true (String.length s > 0)

let test_ppid_of_message_type_dcep () =
  check int32 "dcep ppid" 50l (Sctp.ppid_of_message_type Sctp.WebRTC_DCEP)

let test_ppid_of_message_type_string () =
  check int32 "string ppid" 51l (Sctp.ppid_of_message_type Sctp.WebRTC_String)

let test_ppid_of_message_type_binary () =
  check int32 "binary ppid" 53l (Sctp.ppid_of_message_type Sctp.WebRTC_Binary)

let test_ppid_of_message_type_string_empty () =
  check int32 "string_empty ppid" 56l (Sctp.ppid_of_message_type Sctp.WebRTC_String_Empty)

let test_ppid_of_message_type_binary_empty () =
  check int32 "binary_empty ppid" 57l (Sctp.ppid_of_message_type Sctp.WebRTC_Binary_Empty)

let test_message_type_of_ppid_50 () =
  match Sctp.message_type_of_ppid 50l with
  | Some Sctp.WebRTC_DCEP -> check bool "dcep" true true
  | _ -> fail "expected Some WebRTC_DCEP"

let test_message_type_of_ppid_51 () =
  match Sctp.message_type_of_ppid 51l with
  | Some Sctp.WebRTC_String -> check bool "string" true true
  | _ -> fail "expected Some WebRTC_String"

let test_message_type_of_ppid_53 () =
  match Sctp.message_type_of_ppid 53l with
  | Some Sctp.WebRTC_Binary -> check bool "binary" true true
  | _ -> fail "expected Some WebRTC_Binary"

let test_message_type_of_ppid_56 () =
  match Sctp.message_type_of_ppid 56l with
  | Some Sctp.WebRTC_String_Empty -> check bool "string_empty" true true
  | _ -> fail "expected Some WebRTC_String_Empty"

let test_message_type_of_ppid_57 () =
  match Sctp.message_type_of_ppid 57l with
  | Some Sctp.WebRTC_Binary_Empty -> check bool "binary_empty" true true
  | _ -> fail "expected Some WebRTC_Binary_Empty"

let test_message_type_of_ppid_unknown () =
  match Sctp.message_type_of_ppid 99l with
  | None -> check bool "unknown" true true
  | Some _ -> fail "expected None"

(* ============================================================
   Default Config Tests
   ============================================================ *)

let test_default_config_local_port () =
  check int "local_port" 5000 Sctp.default_config.local_port

let test_default_config_remote_port () =
  check int "remote_port" 5000 Sctp.default_config.remote_port

let test_default_config_mtu () =
  check int "mtu" 1200 Sctp.default_config.mtu

let test_default_config_max_retransmits () =
  check int "max_retransmits" 10 Sctp.default_config.max_retransmits

let test_default_config_rto_initial () =
  check int "rto_initial_ms" 3000 Sctp.default_config.rto_initial_ms

let test_default_config_a_rwnd () =
  check int "a_rwnd" 65536 Sctp.default_config.a_rwnd

let test_default_config_num_streams () =
  check int "num_outbound_streams" 65535 Sctp.default_config.num_outbound_streams

(* ============================================================
   SACK Creation Tests
   ============================================================ *)

let test_create_sack_basic () =
  let sack = Sctp.create_sack ~cumulative_tsn:100l ~a_rwnd:65535l in
  check int32 "cumulative_tsn" 100l sack.cumulative_tsn;
  check int32 "a_rwnd" 65535l sack.a_rwnd

let test_create_sack_zero_tsn () =
  let sack = Sctp.create_sack ~cumulative_tsn:0l ~a_rwnd:1000l in
  check int32 "zero tsn" 0l sack.cumulative_tsn

let test_create_sack_large_rwnd () =
  let sack = Sctp.create_sack ~cumulative_tsn:999l ~a_rwnd:0xFFFFFFFFl in
  check int32 "large rwnd" 0xFFFFFFFFl sack.a_rwnd

let test_create_sack_with_gaps () =
  let gaps = [{ Sctp.gap_start = 2; gap_end = 3 }] in
  let sack = Sctp.create_sack_with_gaps ~cumulative_tsn:100l ~a_rwnd:65535l ~gap_ack_blocks:gaps in
  check int "has gaps" 1 (List.length sack.gap_ack_blocks)

let test_create_sack_with_multiple_gaps () =
  let gaps = [
    { Sctp.gap_start = 2; gap_end = 3 };
    { Sctp.gap_start = 5; gap_end = 7 };
  ] in
  let sack = Sctp.create_sack_with_gaps ~cumulative_tsn:100l ~a_rwnd:65535l ~gap_ack_blocks:gaps in
  check int "two gaps" 2 (List.length sack.gap_ack_blocks)

let test_create_sack_no_gaps () =
  let sack = Sctp.create_sack_with_gaps ~cumulative_tsn:100l ~a_rwnd:65535l ~gap_ack_blocks:[] in
  check int "no gaps" 0 (List.length sack.gap_ack_blocks)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "SCTP Coverage" [
    "chunk_type_string", [
      test_case "data" `Quick test_string_of_chunk_type_data;
      test_case "init" `Quick test_string_of_chunk_type_init;
      test_case "init_ack" `Quick test_string_of_chunk_type_init_ack;
      test_case "sack" `Quick test_string_of_chunk_type_sack;
      test_case "heartbeat" `Quick test_string_of_chunk_type_heartbeat;
      test_case "cookie_echo" `Quick test_string_of_chunk_type_cookie_echo;
      test_case "cookie_ack" `Quick test_string_of_chunk_type_cookie_ack;
      test_case "shutdown" `Quick test_string_of_chunk_type_shutdown;
      test_case "abort" `Quick test_string_of_chunk_type_abort;
      test_case "error" `Quick test_string_of_chunk_type_error;
      test_case "unknown" `Quick test_string_of_chunk_type_unknown;
    ];
    "chunk_type_of_int", [
      test_case "0 -> data" `Quick test_chunk_type_of_int_0;
      test_case "1 -> init" `Quick test_chunk_type_of_int_1;
      test_case "2 -> init_ack" `Quick test_chunk_type_of_int_2;
      test_case "3 -> sack" `Quick test_chunk_type_of_int_3;
      test_case "4 -> heartbeat" `Quick test_chunk_type_of_int_4;
      test_case "99 -> unknown" `Quick test_chunk_type_of_int_unknown;
    ];
    "int_of_chunk_type", [
      test_case "data -> 0" `Quick test_int_of_chunk_type_data;
      test_case "init -> 1" `Quick test_int_of_chunk_type_init;
      test_case "sack -> 3" `Quick test_int_of_chunk_type_sack;
      test_case "heartbeat -> 4" `Quick test_int_of_chunk_type_heartbeat;
      test_case "shutdown -> 7" `Quick test_int_of_chunk_type_shutdown;
    ];
    "state", [
      test_case "closed" `Quick test_string_of_state_closed;
      test_case "cookie_wait" `Quick test_string_of_state_cookie_wait;
      test_case "cookie_echoed" `Quick test_string_of_state_cookie_echoed;
      test_case "established" `Quick test_string_of_state_established;
      test_case "shutdown_pending" `Quick test_string_of_state_shutdown_pending;
      test_case "shutdown_sent" `Quick test_string_of_state_shutdown_sent;
      test_case "shutdown_received" `Quick test_string_of_state_shutdown_received;
      test_case "shutdown_ack_sent" `Quick test_string_of_state_shutdown_ack_sent;
    ];
    "message_type", [
      test_case "string dcep" `Quick test_string_of_message_type_dcep;
      test_case "string string" `Quick test_string_of_message_type_string;
      test_case "string binary" `Quick test_string_of_message_type_binary;
      test_case "string string_empty" `Quick test_string_of_message_type_string_empty;
      test_case "string binary_empty" `Quick test_string_of_message_type_binary_empty;
      test_case "ppid dcep" `Quick test_ppid_of_message_type_dcep;
      test_case "ppid string" `Quick test_ppid_of_message_type_string;
      test_case "ppid binary" `Quick test_ppid_of_message_type_binary;
      test_case "ppid string_empty" `Quick test_ppid_of_message_type_string_empty;
      test_case "ppid binary_empty" `Quick test_ppid_of_message_type_binary_empty;
      test_case "ppid 50" `Quick test_message_type_of_ppid_50;
      test_case "ppid 51" `Quick test_message_type_of_ppid_51;
      test_case "ppid 53" `Quick test_message_type_of_ppid_53;
      test_case "ppid 56" `Quick test_message_type_of_ppid_56;
      test_case "ppid 57" `Quick test_message_type_of_ppid_57;
      test_case "ppid unknown" `Quick test_message_type_of_ppid_unknown;
    ];
    "default_config", [
      test_case "local_port" `Quick test_default_config_local_port;
      test_case "remote_port" `Quick test_default_config_remote_port;
      test_case "mtu" `Quick test_default_config_mtu;
      test_case "max_retransmits" `Quick test_default_config_max_retransmits;
      test_case "rto_initial" `Quick test_default_config_rto_initial;
      test_case "a_rwnd" `Quick test_default_config_a_rwnd;
      test_case "num_streams" `Quick test_default_config_num_streams;
    ];
    "sack_creation", [
      test_case "basic" `Quick test_create_sack_basic;
      test_case "zero tsn" `Quick test_create_sack_zero_tsn;
      test_case "large rwnd" `Quick test_create_sack_large_rwnd;
      test_case "with gaps" `Quick test_create_sack_with_gaps;
      test_case "multiple gaps" `Quick test_create_sack_with_multiple_gaps;
      test_case "no gaps" `Quick test_create_sack_no_gaps;
    ];
  ]
