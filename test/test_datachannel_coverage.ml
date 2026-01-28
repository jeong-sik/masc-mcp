(** Datachannel Module Coverage Tests

    Tests for WebRTC DataChannel:
    - Compression: min_size, default_level, magic, encode/decode
    - Channel types: string_of, int_of, channel_type_of_int
    - State functions: string_of_channel_state, string_of_connection_state
    - Reliability: is_reliable, is_ordered, reliability_param
    - DCEP constants: dcep_open, dcep_ack, ppid_*
    - Message encoding: encode_open_message, decode_open_message, encode_ack_message
*)

open Alcotest

module Datachannel = Masc_mcp.Datachannel

(* ============================================================
   Compression Constants Tests
   ============================================================ *)

let test_compression_min_size () =
  check int "min_size" 128 Datachannel.Compression.min_size

let test_compression_default_level () =
  check int "default_level" 3 Datachannel.Compression.default_level

let test_compression_magic () =
  check string "magic" "ZSTD" Datachannel.Compression.magic

(* ============================================================
   Compression encode/decode Tests
   ============================================================ *)

let test_encode_with_header_length () =
  let data = Bytes.of_string "test" in
  let result = Datachannel.Compression.encode_with_header 100 data in
  (* 8 bytes header + 4 bytes data *)
  check int "length" 12 (Bytes.length result)

let test_encode_with_header_magic () =
  let data = Bytes.of_string "test" in
  let result = Datachannel.Compression.encode_with_header 100 data in
  check string "magic header" "ZSTD" (Bytes.sub_string result 0 4)

let test_decode_header_valid () =
  let data = Bytes.of_string "test data" in
  let encoded = Datachannel.Compression.encode_with_header 256 data in
  match Datachannel.Compression.decode_header encoded with
  | Some (size, _) -> check int "orig_size" 256 size
  | None -> fail "expected Some"

let test_decode_header_too_short () =
  let data = Bytes.of_string "short" in
  match Datachannel.Compression.decode_header data with
  | None -> check bool "too short" true true
  | Some _ -> fail "expected None"

let test_decode_header_wrong_magic () =
  let data = Bytes.of_string "XXXX\x00\x00\x00\x10data" in
  match Datachannel.Compression.decode_header data with
  | None -> check bool "wrong magic" true true
  | Some _ -> fail "expected None"

let test_compress_small_data () =
  let data = Bytes.of_string "small" in
  let (result, compressed) = Datachannel.Compression.compress data in
  check bool "not compressed" false compressed;
  check int "same length" (Bytes.length data) (Bytes.length result)

let test_decompress_auto_no_header () =
  let data = Bytes.of_string "plain data without header" in
  let result = Datachannel.Compression.decompress_auto data in
  check string "unchanged" (Bytes.to_string data) (Bytes.to_string result)

(* ============================================================
   Channel Type Tests
   ============================================================ *)

let test_string_of_channel_type_reliable_ordered () =
  let s = Datachannel.string_of_channel_type Datachannel.Reliable_ordered in
  check string "reliable-ordered" "reliable-ordered" s

let test_string_of_channel_type_reliable_unordered () =
  let s = Datachannel.string_of_channel_type Datachannel.Reliable_unordered in
  check string "reliable-unordered" "reliable-unordered" s

let test_string_of_channel_type_unreliable_ordered () =
  let s = Datachannel.string_of_channel_type Datachannel.Unreliable_ordered in
  check string "unreliable-ordered" "unreliable-ordered" s

let test_string_of_channel_type_unreliable_unordered () =
  let s = Datachannel.string_of_channel_type Datachannel.Unreliable_unordered in
  check string "unreliable-unordered" "unreliable-unordered" s

let test_string_of_channel_type_partial_rexmit () =
  let s = Datachannel.string_of_channel_type (Datachannel.Partial_reliable_rexmit 5) in
  check string "partial-reliable-rexmit(5)" "partial-reliable-rexmit(5)" s

let test_string_of_channel_type_partial_timed () =
  let s = Datachannel.string_of_channel_type (Datachannel.Partial_reliable_timed 100) in
  check string "partial-reliable-timed(100ms)" "partial-reliable-timed(100ms)" s

let test_channel_type_of_int_reliable_ordered () =
  match Datachannel.channel_type_of_int 0x00 with
  | Datachannel.Reliable_ordered -> check bool "match" true true
  | _ -> fail "expected Reliable_ordered"

let test_channel_type_of_int_reliable_unordered () =
  match Datachannel.channel_type_of_int 0x80 with
  | Datachannel.Reliable_unordered -> check bool "match" true true
  | _ -> fail "expected Reliable_unordered"

let test_channel_type_of_int_unreliable_ordered () =
  match Datachannel.channel_type_of_int 0x01 with
  | Datachannel.Unreliable_ordered -> check bool "match" true true
  | _ -> fail "expected Unreliable_ordered"

let test_channel_type_of_int_partial_rexmit () =
  match Datachannel.channel_type_of_int ~param:10 0x02 with
  | Datachannel.Partial_reliable_rexmit 10 -> check bool "match" true true
  | _ -> fail "expected Partial_reliable_rexmit"

let test_channel_type_of_int_unknown () =
  match Datachannel.channel_type_of_int 0xFF with
  | Datachannel.Reliable_ordered -> check bool "default" true true
  | _ -> fail "expected default Reliable_ordered"

let test_int_of_channel_type_reliable_ordered () =
  check int "0x00" 0x00 (Datachannel.int_of_channel_type Datachannel.Reliable_ordered)

let test_int_of_channel_type_reliable_unordered () =
  check int "0x80" 0x80 (Datachannel.int_of_channel_type Datachannel.Reliable_unordered)

let test_int_of_channel_type_unreliable_ordered () =
  check int "0x01" 0x01 (Datachannel.int_of_channel_type Datachannel.Unreliable_ordered)

let test_int_of_channel_type_unreliable_unordered () =
  check int "0x81" 0x81 (Datachannel.int_of_channel_type Datachannel.Unreliable_unordered)

let test_int_of_channel_type_partial_rexmit () =
  check int "0x02" 0x02 (Datachannel.int_of_channel_type (Datachannel.Partial_reliable_rexmit 5))

let test_int_of_channel_type_partial_timed () =
  check int "0x03" 0x03 (Datachannel.int_of_channel_type (Datachannel.Partial_reliable_timed 100))

(* ============================================================
   State String Tests
   ============================================================ *)

let test_string_of_channel_state_connecting () =
  let s = Datachannel.string_of_channel_state Datachannel.Connecting in
  check string "connecting" "connecting" s

let test_string_of_channel_state_open () =
  let s = Datachannel.string_of_channel_state Datachannel.Open in
  check string "open" "open" s

let test_string_of_channel_state_closing () =
  let s = Datachannel.string_of_channel_state Datachannel.Closing in
  check string "closing" "closing" s

let test_string_of_channel_state_closed () =
  let s = Datachannel.string_of_channel_state Datachannel.Closed in
  check string "closed" "closed" s

let test_string_of_connection_state_connecting () =
  let s = Datachannel.string_of_connection_state Datachannel.Conn_connecting in
  check string "connecting" "connecting" s

let test_string_of_connection_state_connected () =
  let s = Datachannel.string_of_connection_state Datachannel.Conn_connected in
  check string "connected" "connected" s

let test_string_of_connection_state_closed () =
  let s = Datachannel.string_of_connection_state Datachannel.Conn_closed in
  check string "closed" "closed" s

(* ============================================================
   Reliability Tests
   ============================================================ *)

let test_is_reliable_ordered () =
  check bool "reliable_ordered" true (Datachannel.is_reliable Datachannel.Reliable_ordered)

let test_is_reliable_unordered () =
  check bool "reliable_unordered" true (Datachannel.is_reliable Datachannel.Reliable_unordered)

let test_is_reliable_unreliable_ordered () =
  check bool "unreliable_ordered" false (Datachannel.is_reliable Datachannel.Unreliable_ordered)

let test_is_reliable_unreliable_unordered () =
  check bool "unreliable_unordered" false (Datachannel.is_reliable Datachannel.Unreliable_unordered)

let test_is_reliable_partial_rexmit () =
  check bool "partial_rexmit" false (Datachannel.is_reliable (Datachannel.Partial_reliable_rexmit 5))

let test_is_ordered_reliable_ordered () =
  check bool "reliable_ordered" true (Datachannel.is_ordered Datachannel.Reliable_ordered)

let test_is_ordered_reliable_unordered () =
  check bool "reliable_unordered" false (Datachannel.is_ordered Datachannel.Reliable_unordered)

let test_is_ordered_unreliable_ordered () =
  check bool "unreliable_ordered" true (Datachannel.is_ordered Datachannel.Unreliable_ordered)

let test_is_ordered_unreliable_unordered () =
  check bool "unreliable_unordered" false (Datachannel.is_ordered Datachannel.Unreliable_unordered)

let test_reliability_param_reliable () =
  check int "reliable" 0 (Datachannel.reliability_param Datachannel.Reliable_ordered)

let test_reliability_param_partial_rexmit () =
  check int "partial_rexmit" 10 (Datachannel.reliability_param (Datachannel.Partial_reliable_rexmit 10))

let test_reliability_param_partial_timed () =
  check int "partial_timed" 500 (Datachannel.reliability_param (Datachannel.Partial_reliable_timed 500))

(* ============================================================
   DCEP Constants Tests
   ============================================================ *)

let test_dcep_open () =
  check int "dcep_open" 0x03 Datachannel.dcep_open

let test_dcep_ack () =
  check int "dcep_ack" 0x02 Datachannel.dcep_ack

let test_ppid_dcep () =
  check int32 "ppid_dcep" 50l Datachannel.ppid_dcep

let test_ppid_string () =
  check int32 "ppid_string" 51l Datachannel.ppid_string

let test_ppid_binary () =
  check int32 "ppid_binary" 53l Datachannel.ppid_binary

let test_ppid_string_empty () =
  check int32 "ppid_string_empty" 56l Datachannel.ppid_string_empty

let test_ppid_binary_empty () =
  check int32 "ppid_binary_empty" 57l Datachannel.ppid_binary_empty

(* ============================================================
   Default Config Tests
   ============================================================ *)

let test_default_config_max_channels () =
  check int "max_channels" 65535 Datachannel.default_config.max_channels

let test_default_config_max_message_size () =
  check int "max_message_size" 262144 Datachannel.default_config.max_message_size

let test_default_config_sctp_port () =
  check int "sctp_port" 5000 Datachannel.default_config.sctp_port

(* ============================================================
   Message Encoding Tests
   ============================================================ *)

let test_encode_ack_message_length () =
  let ack = Datachannel.encode_ack_message () in
  check int "length" 1 (Bytes.length ack)

let test_encode_ack_message_type () =
  let ack = Datachannel.encode_ack_message () in
  check int "type byte" 0x02 (Char.code (Bytes.get ack 0))

let test_encode_open_message_roundtrip () =
  let msg : Datachannel.open_message = {
    channel_type = Datachannel.Reliable_ordered;
    priority = 0;
    reliability_param = 0;
    label = "test-label";
    protocol = "test-proto";
  } in
  let encoded = Datachannel.encode_open_message msg in
  match Datachannel.decode_open_message encoded with
  | Ok decoded ->
      check string "label" "test-label" decoded.label;
      check string "protocol" "test-proto" decoded.protocol
  | Error e -> fail ("decode error: " ^ e)

let test_decode_open_message_too_short () =
  let short = Bytes.of_string "short" in
  match Datachannel.decode_open_message short with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected error"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Datachannel Coverage" [
    "compression_constants", [
      test_case "min_size" `Quick test_compression_min_size;
      test_case "default_level" `Quick test_compression_default_level;
      test_case "magic" `Quick test_compression_magic;
    ];
    "compression_encode_decode", [
      test_case "encode length" `Quick test_encode_with_header_length;
      test_case "encode magic" `Quick test_encode_with_header_magic;
      test_case "decode valid" `Quick test_decode_header_valid;
      test_case "decode too short" `Quick test_decode_header_too_short;
      test_case "decode wrong magic" `Quick test_decode_header_wrong_magic;
      test_case "compress small" `Quick test_compress_small_data;
      test_case "decompress auto no header" `Quick test_decompress_auto_no_header;
    ];
    "channel_type", [
      test_case "string reliable_ordered" `Quick test_string_of_channel_type_reliable_ordered;
      test_case "string reliable_unordered" `Quick test_string_of_channel_type_reliable_unordered;
      test_case "string unreliable_ordered" `Quick test_string_of_channel_type_unreliable_ordered;
      test_case "string unreliable_unordered" `Quick test_string_of_channel_type_unreliable_unordered;
      test_case "string partial_rexmit" `Quick test_string_of_channel_type_partial_rexmit;
      test_case "string partial_timed" `Quick test_string_of_channel_type_partial_timed;
      test_case "of_int reliable_ordered" `Quick test_channel_type_of_int_reliable_ordered;
      test_case "of_int reliable_unordered" `Quick test_channel_type_of_int_reliable_unordered;
      test_case "of_int unreliable_ordered" `Quick test_channel_type_of_int_unreliable_ordered;
      test_case "of_int partial_rexmit" `Quick test_channel_type_of_int_partial_rexmit;
      test_case "of_int unknown" `Quick test_channel_type_of_int_unknown;
      test_case "int_of reliable_ordered" `Quick test_int_of_channel_type_reliable_ordered;
      test_case "int_of reliable_unordered" `Quick test_int_of_channel_type_reliable_unordered;
      test_case "int_of unreliable_ordered" `Quick test_int_of_channel_type_unreliable_ordered;
      test_case "int_of unreliable_unordered" `Quick test_int_of_channel_type_unreliable_unordered;
      test_case "int_of partial_rexmit" `Quick test_int_of_channel_type_partial_rexmit;
      test_case "int_of partial_timed" `Quick test_int_of_channel_type_partial_timed;
    ];
    "state_strings", [
      test_case "channel connecting" `Quick test_string_of_channel_state_connecting;
      test_case "channel open" `Quick test_string_of_channel_state_open;
      test_case "channel closing" `Quick test_string_of_channel_state_closing;
      test_case "channel closed" `Quick test_string_of_channel_state_closed;
      test_case "connection connecting" `Quick test_string_of_connection_state_connecting;
      test_case "connection connected" `Quick test_string_of_connection_state_connected;
      test_case "connection closed" `Quick test_string_of_connection_state_closed;
    ];
    "reliability", [
      test_case "is_reliable ordered" `Quick test_is_reliable_ordered;
      test_case "is_reliable unordered" `Quick test_is_reliable_unordered;
      test_case "is_reliable unreliable_ordered" `Quick test_is_reliable_unreliable_ordered;
      test_case "is_reliable unreliable_unordered" `Quick test_is_reliable_unreliable_unordered;
      test_case "is_reliable partial_rexmit" `Quick test_is_reliable_partial_rexmit;
      test_case "is_ordered reliable_ordered" `Quick test_is_ordered_reliable_ordered;
      test_case "is_ordered reliable_unordered" `Quick test_is_ordered_reliable_unordered;
      test_case "is_ordered unreliable_ordered" `Quick test_is_ordered_unreliable_ordered;
      test_case "is_ordered unreliable_unordered" `Quick test_is_ordered_unreliable_unordered;
      test_case "param reliable" `Quick test_reliability_param_reliable;
      test_case "param partial_rexmit" `Quick test_reliability_param_partial_rexmit;
      test_case "param partial_timed" `Quick test_reliability_param_partial_timed;
    ];
    "dcep_constants", [
      test_case "dcep_open" `Quick test_dcep_open;
      test_case "dcep_ack" `Quick test_dcep_ack;
      test_case "ppid_dcep" `Quick test_ppid_dcep;
      test_case "ppid_string" `Quick test_ppid_string;
      test_case "ppid_binary" `Quick test_ppid_binary;
      test_case "ppid_string_empty" `Quick test_ppid_string_empty;
      test_case "ppid_binary_empty" `Quick test_ppid_binary_empty;
    ];
    "default_config", [
      test_case "max_channels" `Quick test_default_config_max_channels;
      test_case "max_message_size" `Quick test_default_config_max_message_size;
      test_case "sctp_port" `Quick test_default_config_sctp_port;
    ];
    "message_encoding", [
      test_case "ack length" `Quick test_encode_ack_message_length;
      test_case "ack type" `Quick test_encode_ack_message_type;
      test_case "open roundtrip" `Quick test_encode_open_message_roundtrip;
      test_case "decode too short" `Quick test_decode_open_message_too_short;
    ];
  ]
