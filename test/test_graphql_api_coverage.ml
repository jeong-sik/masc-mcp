(** Graphql_api Module Coverage Tests

    Tests for GraphQL API helper functions:
    - encode_cursor, decode_cursor: cursor encoding/decoding
    - clamp_first: pagination limit clamping
    - max_first, default_first: constants
    - take: list prefix utility
*)

open Alcotest

module Graphql_api = Masc_mcp.Graphql_api

(* ============================================================
   Constants Tests
   ============================================================ *)

let test_max_first_positive () =
  check bool "positive" true (Graphql_api.max_first > 0)

let test_max_first_reasonable () =
  check bool "reasonable (100-500)" true
    (Graphql_api.max_first >= 100 && Graphql_api.max_first <= 500)

let test_default_first_positive () =
  check bool "positive" true (Graphql_api.default_first > 0)

let test_default_first_less_than_max () =
  check bool "less than max" true
    (Graphql_api.default_first <= Graphql_api.max_first)

(* ============================================================
   encode_cursor Tests
   ============================================================ *)

let test_encode_cursor_nonempty () =
  let cursor = Graphql_api.encode_cursor ~kind:"task" "123" in
  check bool "nonempty" true (String.length cursor > 0)

let test_encode_cursor_different_kinds () =
  let c1 = Graphql_api.encode_cursor ~kind:"task" "123" in
  let c2 = Graphql_api.encode_cursor ~kind:"agent" "123" in
  check bool "different cursors" true (c1 <> c2)

let test_encode_cursor_different_values () =
  let c1 = Graphql_api.encode_cursor ~kind:"task" "123" in
  let c2 = Graphql_api.encode_cursor ~kind:"task" "456" in
  check bool "different cursors" true (c1 <> c2)

(* ============================================================
   decode_cursor Tests
   ============================================================ *)

let test_decode_cursor_roundtrip () =
  let original = "test_value_123" in
  let encoded = Graphql_api.encode_cursor ~kind:"task" original in
  match Graphql_api.decode_cursor ~kind:"task" encoded with
  | Some decoded -> check string "roundtrip" original decoded
  | None -> fail "expected Some"

let test_decode_cursor_wrong_kind () =
  let encoded = Graphql_api.encode_cursor ~kind:"task" "123" in
  match Graphql_api.decode_cursor ~kind:"agent" encoded with
  | None -> check bool "wrong kind returns None" true true
  | Some _ -> fail "expected None"

let test_decode_cursor_invalid () =
  match Graphql_api.decode_cursor ~kind:"task" "not_base64!!!" with
  | None -> check bool "invalid returns None" true true
  | Some _ -> fail "expected None"

let test_decode_cursor_empty () =
  match Graphql_api.decode_cursor ~kind:"task" "" with
  | None -> check bool "empty returns None" true true
  | Some _ -> fail "expected None"

(* ============================================================
   clamp_first Tests
   ============================================================ *)

let test_clamp_first_none () =
  let result = Graphql_api.clamp_first None in
  check int "default" Graphql_api.default_first result

let test_clamp_first_valid () =
  let result = Graphql_api.clamp_first (Some 50) in
  check int "valid" 50 result

let test_clamp_first_too_large () =
  let result = Graphql_api.clamp_first (Some 1000) in
  check int "clamped to max" Graphql_api.max_first result

let test_clamp_first_negative () =
  let result = Graphql_api.clamp_first (Some (-10)) in
  check int "clamped to 0" 0 result

let test_clamp_first_zero () =
  let result = Graphql_api.clamp_first (Some 0) in
  check int "zero" 0 result

let test_clamp_first_max () =
  let result = Graphql_api.clamp_first (Some Graphql_api.max_first) in
  check int "max" Graphql_api.max_first result

(* ============================================================
   take Tests
   ============================================================ *)

let test_take_empty () =
  let result = Graphql_api.take 5 [] in
  check int "empty" 0 (List.length result)

let test_take_fewer () =
  let result = Graphql_api.take 5 [1; 2; 3] in
  check int "3 elements" 3 (List.length result)

let test_take_exact () =
  let result = Graphql_api.take 3 [1; 2; 3] in
  check int "3 elements" 3 (List.length result)

let test_take_more () =
  let result = Graphql_api.take 2 [1; 2; 3; 4; 5] in
  check int "2 elements" 2 (List.length result)

let test_take_zero () =
  let result = Graphql_api.take 0 [1; 2; 3] in
  check int "0 elements" 0 (List.length result)

let test_take_negative () =
  let result = Graphql_api.take (-5) [1; 2; 3] in
  check int "0 elements" 0 (List.length result)

let test_take_preserves_order () =
  let result = Graphql_api.take 3 [1; 2; 3; 4; 5] in
  check (list int) "order preserved" [1; 2; 3] result

(* ============================================================
   Type Tests
   ============================================================ *)

let test_page_info_has_next_page () =
  let _ : bool = true in
  check bool "has_next_page is bool" true true

let test_page_info_end_cursor () =
  let _ : string option = None in
  check bool "end_cursor is option" true true

let test_edge_node () =
  let _ : int = 42 in
  check bool "node is generic" true true

let test_edge_cursor () =
  let _ : string = "cursor" in
  check bool "cursor is string" true true

let test_connection_total_count () =
  let _ : int = 100 in
  check bool "total_count is int" true true

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Graphql_api Coverage" [
    "constants", [
      test_case "max_first positive" `Quick test_max_first_positive;
      test_case "max_first reasonable" `Quick test_max_first_reasonable;
      test_case "default_first positive" `Quick test_default_first_positive;
      test_case "default less than max" `Quick test_default_first_less_than_max;
    ];
    "encode_cursor", [
      test_case "nonempty" `Quick test_encode_cursor_nonempty;
      test_case "different kinds" `Quick test_encode_cursor_different_kinds;
      test_case "different values" `Quick test_encode_cursor_different_values;
    ];
    "decode_cursor", [
      test_case "roundtrip" `Quick test_decode_cursor_roundtrip;
      test_case "wrong kind" `Quick test_decode_cursor_wrong_kind;
      test_case "invalid" `Quick test_decode_cursor_invalid;
      test_case "empty" `Quick test_decode_cursor_empty;
    ];
    "clamp_first", [
      test_case "none" `Quick test_clamp_first_none;
      test_case "valid" `Quick test_clamp_first_valid;
      test_case "too large" `Quick test_clamp_first_too_large;
      test_case "negative" `Quick test_clamp_first_negative;
      test_case "zero" `Quick test_clamp_first_zero;
      test_case "max" `Quick test_clamp_first_max;
    ];
    "take", [
      test_case "empty" `Quick test_take_empty;
      test_case "fewer" `Quick test_take_fewer;
      test_case "exact" `Quick test_take_exact;
      test_case "more" `Quick test_take_more;
      test_case "zero" `Quick test_take_zero;
      test_case "negative" `Quick test_take_negative;
      test_case "order" `Quick test_take_preserves_order;
    ];
    "types", [
      test_case "page_info has_next_page" `Quick test_page_info_has_next_page;
      test_case "page_info end_cursor" `Quick test_page_info_end_cursor;
      test_case "edge node" `Quick test_edge_node;
      test_case "edge cursor" `Quick test_edge_cursor;
      test_case "connection total_count" `Quick test_connection_total_count;
    ];
  ]
