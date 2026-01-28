(** Mcp_session Module Coverage Tests

    Tests for MCP HTTP Session ID management:
    - base62_chars: character set constant
    - encode_base62: integer encoding
    - is_valid: session ID validation per MCP spec
    - generate: unique ID generation
    - get_or_generate: optional header handling
*)

open Alcotest

module Mcp_session = Masc_mcp.Mcp_session

(* ============================================================
   base62_chars Tests
   ============================================================ *)

let test_base62_chars_length () =
  check int "62 chars" 62 (String.length Mcp_session.base62_chars)

let test_base62_chars_digits () =
  check bool "starts with digits" true
    (String.sub Mcp_session.base62_chars 0 10 = "0123456789")

let test_base62_chars_uppercase () =
  check bool "has uppercase" true
    (String.sub Mcp_session.base62_chars 10 26 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

let test_base62_chars_lowercase () =
  check bool "has lowercase" true
    (String.sub Mcp_session.base62_chars 36 26 = "abcdefghijklmnopqrstuvwxyz")

(* ============================================================
   encode_base62 Tests
   ============================================================ *)

let test_encode_base62_zero () =
  check string "zero" "0" (Mcp_session.encode_base62 0)

let test_encode_base62_one () =
  check string "one" "1" (Mcp_session.encode_base62 1)

let test_encode_base62_ten () =
  check string "ten" "A" (Mcp_session.encode_base62 10)

let test_encode_base62_36 () =
  check string "36" "a" (Mcp_session.encode_base62 36)

let test_encode_base62_62 () =
  check string "62" "10" (Mcp_session.encode_base62 62)

let test_encode_base62_large () =
  let result = Mcp_session.encode_base62 1000000 in
  check bool "non-empty" true (String.length result > 0)

(* ============================================================
   is_valid Tests
   ============================================================ *)

let test_is_valid_simple () =
  check bool "simple" true (Mcp_session.is_valid "abc123")

let test_is_valid_with_prefix () =
  check bool "mcp prefix" true (Mcp_session.is_valid "mcp_123_456")

let test_is_valid_empty () =
  check bool "empty" false (Mcp_session.is_valid "")

let test_is_valid_space () =
  check bool "space" false (Mcp_session.is_valid "test id")

let test_is_valid_tab () =
  check bool "tab" false (Mcp_session.is_valid "test\tid")

let test_is_valid_newline () =
  check bool "newline" false (Mcp_session.is_valid "test\nid")

let test_is_valid_null () =
  check bool "null char" false (Mcp_session.is_valid "test\x00id")

let test_is_valid_del () =
  check bool "del char (0x7F)" false (Mcp_session.is_valid "test\x7Fid")

let test_is_valid_printable () =
  (* All printable ASCII except space: ! to ~ (0x21-0x7E) *)
  check bool "exclaim" true (Mcp_session.is_valid "!")

let test_is_valid_tilde () =
  check bool "tilde" true (Mcp_session.is_valid "~")

let test_is_valid_mixed_printable () =
  check bool "mixed" true (Mcp_session.is_valid "test-123_abc.xyz!")

(* ============================================================
   generate Tests
   ============================================================ *)

let test_generate_format () =
  let id = Mcp_session.generate () in
  check bool "starts with mcp_" true (String.length id >= 4 && String.sub id 0 4 = "mcp_")

let test_generate_valid () =
  let id = Mcp_session.generate () in
  check bool "is valid" true (Mcp_session.is_valid id)

let test_generate_unique () =
  let id1 = Mcp_session.generate () in
  let id2 = Mcp_session.generate () in
  check bool "different" true (id1 <> id2)

let test_generate_nonempty () =
  let id = Mcp_session.generate () in
  check bool "nonempty" true (String.length id > 0)

(* ============================================================
   get_or_generate Tests
   ============================================================ *)

let test_get_or_generate_none () =
  let id = Mcp_session.get_or_generate None in
  check bool "generates valid" true (Mcp_session.is_valid id)

let test_get_or_generate_valid () =
  let result = Mcp_session.get_or_generate (Some "valid_id") in
  check string "returns same" "valid_id" result

let test_get_or_generate_invalid_space () =
  let result = Mcp_session.get_or_generate (Some "invalid id") in
  check bool "generates new" true (result <> "invalid id");
  check bool "is valid" true (Mcp_session.is_valid result)

let test_get_or_generate_empty () =
  let result = Mcp_session.get_or_generate (Some "") in
  check bool "not empty" true (String.length result > 0);
  check bool "is valid" true (Mcp_session.is_valid result)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Mcp_session Coverage" [
    "base62_chars", [
      test_case "length" `Quick test_base62_chars_length;
      test_case "digits" `Quick test_base62_chars_digits;
      test_case "uppercase" `Quick test_base62_chars_uppercase;
      test_case "lowercase" `Quick test_base62_chars_lowercase;
    ];
    "encode_base62", [
      test_case "zero" `Quick test_encode_base62_zero;
      test_case "one" `Quick test_encode_base62_one;
      test_case "ten" `Quick test_encode_base62_ten;
      test_case "36" `Quick test_encode_base62_36;
      test_case "62" `Quick test_encode_base62_62;
      test_case "large" `Quick test_encode_base62_large;
    ];
    "is_valid", [
      test_case "simple" `Quick test_is_valid_simple;
      test_case "with prefix" `Quick test_is_valid_with_prefix;
      test_case "empty" `Quick test_is_valid_empty;
      test_case "space" `Quick test_is_valid_space;
      test_case "tab" `Quick test_is_valid_tab;
      test_case "newline" `Quick test_is_valid_newline;
      test_case "null" `Quick test_is_valid_null;
      test_case "del" `Quick test_is_valid_del;
      test_case "printable" `Quick test_is_valid_printable;
      test_case "tilde" `Quick test_is_valid_tilde;
      test_case "mixed printable" `Quick test_is_valid_mixed_printable;
    ];
    "generate", [
      test_case "format" `Quick test_generate_format;
      test_case "valid" `Quick test_generate_valid;
      test_case "unique" `Quick test_generate_unique;
      test_case "nonempty" `Quick test_generate_nonempty;
    ];
    "get_or_generate", [
      test_case "none" `Quick test_get_or_generate_none;
      test_case "valid" `Quick test_get_or_generate_valid;
      test_case "invalid space" `Quick test_get_or_generate_invalid_space;
      test_case "empty" `Quick test_get_or_generate_empty;
    ];
  ]
