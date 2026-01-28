(** Webrtc_common Module Coverage Tests

    Tests for WebRTC common utilities:
    - write_uint16_be, read_uint16_be: 16-bit big-endian
    - write_uint32_be, read_uint32_be: 32-bit big-endian
    - write_uint48_be, read_uint48_be: 48-bit big-endian
    - safe_sub_bytes: safe byte extraction
    - crc32c: CRC32C checksum
*)

open Alcotest

module Webrtc_common = Masc_mcp.Webrtc_common

(* ============================================================
   16-bit Big-Endian Tests
   ============================================================ *)

let test_write_read_uint16_zero () =
  let buf = Bytes.create 2 in
  Webrtc_common.write_uint16_be buf 0 0;
  let result = Webrtc_common.read_uint16_be buf 0 in
  check int "zero" 0 result

let test_write_read_uint16_max () =
  let buf = Bytes.create 2 in
  Webrtc_common.write_uint16_be buf 0 0xFFFF;
  let result = Webrtc_common.read_uint16_be buf 0 in
  check int "max" 0xFFFF result

let test_write_read_uint16_arbitrary () =
  let buf = Bytes.create 2 in
  Webrtc_common.write_uint16_be buf 0 0x1234;
  let result = Webrtc_common.read_uint16_be buf 0 in
  check int "arbitrary" 0x1234 result

let test_write_uint16_offset () =
  let buf = Bytes.create 4 in
  Webrtc_common.write_uint16_be buf 2 0xABCD;
  let result = Webrtc_common.read_uint16_be buf 2 in
  check int "with offset" 0xABCD result

let test_uint16_big_endian_order () =
  let buf = Bytes.create 2 in
  Webrtc_common.write_uint16_be buf 0 0x1234;
  check int "high byte" 0x12 (Char.code (Bytes.get buf 0));
  check int "low byte" 0x34 (Char.code (Bytes.get buf 1))

(* ============================================================
   32-bit Big-Endian Tests
   ============================================================ *)

let test_write_read_uint32_zero () =
  let buf = Bytes.create 4 in
  Webrtc_common.write_uint32_be buf 0 0l;
  let result = Webrtc_common.read_uint32_be buf 0 in
  check int32 "zero" 0l result

let test_write_read_uint32_max () =
  let buf = Bytes.create 4 in
  Webrtc_common.write_uint32_be buf 0 0xFFFFFFFFl;
  let result = Webrtc_common.read_uint32_be buf 0 in
  check int32 "max" 0xFFFFFFFFl result

let test_write_read_uint32_arbitrary () =
  let buf = Bytes.create 4 in
  Webrtc_common.write_uint32_be buf 0 0x12345678l;
  let result = Webrtc_common.read_uint32_be buf 0 in
  check int32 "arbitrary" 0x12345678l result

let test_uint32_big_endian_order () =
  let buf = Bytes.create 4 in
  Webrtc_common.write_uint32_be buf 0 0x12345678l;
  check int "byte 0" 0x12 (Char.code (Bytes.get buf 0));
  check int "byte 1" 0x34 (Char.code (Bytes.get buf 1));
  check int "byte 2" 0x56 (Char.code (Bytes.get buf 2));
  check int "byte 3" 0x78 (Char.code (Bytes.get buf 3))

(* ============================================================
   48-bit Big-Endian Tests
   ============================================================ *)

let test_write_read_uint48_zero () =
  let buf = Bytes.create 6 in
  Webrtc_common.write_uint48_be buf 0 0L;
  let result = Webrtc_common.read_uint48_be buf 0 in
  check int64 "zero" 0L result

let test_write_read_uint48_arbitrary () =
  let buf = Bytes.create 6 in
  Webrtc_common.write_uint48_be buf 0 0x123456789ABCL;
  let result = Webrtc_common.read_uint48_be buf 0 in
  check int64 "arbitrary" 0x123456789ABCL result

let test_write_read_uint48_max () =
  let buf = Bytes.create 6 in
  Webrtc_common.write_uint48_be buf 0 0xFFFFFFFFFFFFL;
  let result = Webrtc_common.read_uint48_be buf 0 in
  check int64 "max 48-bit" 0xFFFFFFFFFFFFL result

let test_uint48_big_endian_order () =
  let buf = Bytes.create 6 in
  Webrtc_common.write_uint48_be buf 0 0x123456789ABCL;
  check int "byte 0" 0x12 (Char.code (Bytes.get buf 0));
  check int "byte 1" 0x34 (Char.code (Bytes.get buf 1));
  check int "byte 2" 0x56 (Char.code (Bytes.get buf 2));
  check int "byte 3" 0x78 (Char.code (Bytes.get buf 3));
  check int "byte 4" 0x9A (Char.code (Bytes.get buf 4));
  check int "byte 5" 0xBC (Char.code (Bytes.get buf 5))

(* ============================================================
   safe_sub_bytes Tests
   ============================================================ *)

let test_safe_sub_bytes_valid () =
  let buf = Bytes.of_string "hello world" in
  match Webrtc_common.safe_sub_bytes buf 0 5 with
  | Some sub -> check string "valid" "hello" (Bytes.to_string sub)
  | None -> fail "expected Some"

let test_safe_sub_bytes_offset () =
  let buf = Bytes.of_string "hello world" in
  match Webrtc_common.safe_sub_bytes buf 6 5 with
  | Some sub -> check string "with offset" "world" (Bytes.to_string sub)
  | None -> fail "expected Some"

let test_safe_sub_bytes_exact () =
  let buf = Bytes.of_string "test" in
  match Webrtc_common.safe_sub_bytes buf 0 4 with
  | Some sub -> check string "exact" "test" (Bytes.to_string sub)
  | None -> fail "expected Some"

let test_safe_sub_bytes_too_long () =
  let buf = Bytes.of_string "short" in
  match Webrtc_common.safe_sub_bytes buf 0 10 with
  | None -> check bool "none" true true
  | Some _ -> fail "expected None"

let test_safe_sub_bytes_offset_overflow () =
  let buf = Bytes.of_string "test" in
  match Webrtc_common.safe_sub_bytes buf 3 5 with
  | None -> check bool "overflow" true true
  | Some _ -> fail "expected None"

let test_safe_sub_bytes_zero_length () =
  let buf = Bytes.of_string "test" in
  match Webrtc_common.safe_sub_bytes buf 2 0 with
  | Some sub -> check int "zero length" 0 (Bytes.length sub)
  | None -> fail "expected Some"

(* ============================================================
   CRC32C Tests
   ============================================================ *)

let test_crc32c_empty () =
  let data = Bytes.of_string "" in
  let crc = Webrtc_common.crc32c data in
  check int32 "empty" 0l crc

let test_crc32c_deterministic () =
  let data = Bytes.of_string "hello" in
  let crc1 = Webrtc_common.crc32c data in
  let crc2 = Webrtc_common.crc32c data in
  check int32 "deterministic" crc1 crc2

let test_crc32c_different_data () =
  let data1 = Bytes.of_string "hello" in
  let data2 = Bytes.of_string "world" in
  let crc1 = Webrtc_common.crc32c data1 in
  let crc2 = Webrtc_common.crc32c data2 in
  check bool "different" true (crc1 <> crc2)

let test_crc32c_single_byte () =
  let data = Bytes.of_string "x" in
  let crc = Webrtc_common.crc32c data in
  check bool "nonzero" true (crc <> 0l)

let test_crc32c_binary_data () =
  let data = Bytes.create 4 in
  Bytes.set data 0 (Char.chr 0x00);
  Bytes.set data 1 (Char.chr 0xFF);
  Bytes.set data 2 (Char.chr 0x12);
  Bytes.set data 3 (Char.chr 0x34);
  let crc = Webrtc_common.crc32c data in
  check bool "binary nonzero" true (crc <> 0l)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Webrtc_common Coverage" [
    "uint16_be", [
      test_case "zero" `Quick test_write_read_uint16_zero;
      test_case "max" `Quick test_write_read_uint16_max;
      test_case "arbitrary" `Quick test_write_read_uint16_arbitrary;
      test_case "with offset" `Quick test_write_uint16_offset;
      test_case "byte order" `Quick test_uint16_big_endian_order;
    ];
    "uint32_be", [
      test_case "zero" `Quick test_write_read_uint32_zero;
      test_case "max" `Quick test_write_read_uint32_max;
      test_case "arbitrary" `Quick test_write_read_uint32_arbitrary;
      test_case "byte order" `Quick test_uint32_big_endian_order;
    ];
    "uint48_be", [
      test_case "zero" `Quick test_write_read_uint48_zero;
      test_case "arbitrary" `Quick test_write_read_uint48_arbitrary;
      test_case "max" `Quick test_write_read_uint48_max;
      test_case "byte order" `Quick test_uint48_big_endian_order;
    ];
    "safe_sub_bytes", [
      test_case "valid" `Quick test_safe_sub_bytes_valid;
      test_case "with offset" `Quick test_safe_sub_bytes_offset;
      test_case "exact" `Quick test_safe_sub_bytes_exact;
      test_case "too long" `Quick test_safe_sub_bytes_too_long;
      test_case "offset overflow" `Quick test_safe_sub_bytes_offset_overflow;
      test_case "zero length" `Quick test_safe_sub_bytes_zero_length;
    ];
    "crc32c", [
      test_case "empty" `Quick test_crc32c_empty;
      test_case "deterministic" `Quick test_crc32c_deterministic;
      test_case "different data" `Quick test_crc32c_different_data;
      test_case "single byte" `Quick test_crc32c_single_byte;
      test_case "binary data" `Quick test_crc32c_binary_data;
    ];
  ]
