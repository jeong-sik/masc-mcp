(** Cache Eio Module Coverage Tests

    Tests for cache types and pure functions:
    - cache_entry type
    - sanitize_key function
    - entry_to_json function
*)

open Alcotest

module Cache_eio = Masc_mcp.Cache_eio

(* ============================================================
   cache_entry Type Tests
   ============================================================ *)

let test_cache_entry_basic () =
  let entry : Cache_eio.cache_entry = {
    key = "test-key";
    value = "test-value";
    created_at = 1704067200.0;
    expires_at = None;
    tags = [];
  } in
  check string "key" "test-key" entry.key;
  check string "value" "test-value" entry.value;
  check (list string) "tags" [] entry.tags

let test_cache_entry_with_expiry () =
  let entry : Cache_eio.cache_entry = {
    key = "expiring-key";
    value = "some-data";
    created_at = 1704067200.0;
    expires_at = Some 1704153600.0;
    tags = ["api"; "jira"];
  } in
  match entry.expires_at with
  | Some exp -> check (float 0.1) "expires_at" 1704153600.0 exp
  | None -> fail "expected Some"

let test_cache_entry_tags () =
  let entry : Cache_eio.cache_entry = {
    key = "tagged";
    value = "{}";
    created_at = 0.0;
    expires_at = None;
    tags = ["embedding"; "code"; "summary"];
  } in
  check int "tags count" 3 (List.length entry.tags)

(* ============================================================
   sanitize_key Tests
   ============================================================ *)

let test_sanitize_key_simple () =
  let result = Cache_eio.sanitize_key "simple_key" in
  check string "simple" "simple_key" result

let test_sanitize_key_with_special_chars () =
  let result = Cache_eio.sanitize_key "key/with:special@chars!" in
  check string "special replaced" "key_with_special_chars_" result

let test_sanitize_key_truncate () =
  let long_key = String.make 100 'x' in
  let result = Cache_eio.sanitize_key long_key in
  check int "truncated to 64" 64 (String.length result)

let test_sanitize_key_preserves_safe () =
  let result = Cache_eio.sanitize_key "ABC-123_test" in
  check string "preserved" "ABC-123_test" result

let test_sanitize_key_dots () =
  let result = Cache_eio.sanitize_key "file.name.json" in
  check string "dots replaced" "file_name_json" result

(* ============================================================
   entry_to_json Tests
   ============================================================ *)

let test_entry_to_json_basic () =
  let entry : Cache_eio.cache_entry = {
    key = "test";
    value = "data";
    created_at = 1704067200.0;
    expires_at = None;
    tags = [];
  } in
  let json = Cache_eio.entry_to_json entry in
  match json with
  | `Assoc fields ->
      check bool "has key" true (List.mem_assoc "key" fields);
      check bool "has value" true (List.mem_assoc "value" fields);
      check bool "has created_at" true (List.mem_assoc "created_at" fields);
      check bool "has expires_at" true (List.mem_assoc "expires_at" fields);
      check bool "has tags" true (List.mem_assoc "tags" fields)
  | _ -> fail "expected Assoc"

let test_entry_to_json_with_expiry () =
  let entry : Cache_eio.cache_entry = {
    key = "exp";
    value = "v";
    created_at = 0.0;
    expires_at = Some 3600.0;
    tags = [];
  } in
  let json = Cache_eio.entry_to_json entry in
  let open Yojson.Safe.Util in
  let exp = json |> member "expires_at" in
  match exp with
  | `Float f -> check (float 0.1) "expires_at float" 3600.0 f
  | _ -> fail "expected Float"

let test_entry_to_json_with_tags () =
  let entry : Cache_eio.cache_entry = {
    key = "k";
    value = "v";
    created_at = 0.0;
    expires_at = None;
    tags = ["a"; "b"; "c"];
  } in
  let json = Cache_eio.entry_to_json entry in
  let open Yojson.Safe.Util in
  let tags = json |> member "tags" |> to_list in
  check int "tags count" 3 (List.length tags)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Cache Eio Coverage" [
    "cache_entry", [
      test_case "basic" `Quick test_cache_entry_basic;
      test_case "with expiry" `Quick test_cache_entry_with_expiry;
      test_case "tags" `Quick test_cache_entry_tags;
    ];
    "sanitize_key", [
      test_case "simple" `Quick test_sanitize_key_simple;
      test_case "special chars" `Quick test_sanitize_key_with_special_chars;
      test_case "truncate" `Quick test_sanitize_key_truncate;
      test_case "preserves safe" `Quick test_sanitize_key_preserves_safe;
      test_case "dots" `Quick test_sanitize_key_dots;
    ];
    "entry_to_json", [
      test_case "basic" `Quick test_entry_to_json_basic;
      test_case "with expiry" `Quick test_entry_to_json_with_expiry;
      test_case "with tags" `Quick test_entry_to_json_with_tags;
    ];
  ]
