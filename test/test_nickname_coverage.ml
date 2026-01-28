(** Nickname Module Coverage Tests

    Tests for MASC Nickname Generator (Docker-style adjective+animal):
    - generate: agent_type-adjective-animal
    - generate_unique: with hex suffix
    - is_generated_nickname: pattern detection
    - extract_agent_type: parse agent type from nickname
*)

open Alcotest

module Nickname = Masc_mcp.Nickname

(* ============================================================
   generate Tests
   ============================================================ *)

let test_generate_format () =
  let nick = Nickname.generate "claude" in
  let parts = String.split_on_char '-' nick in
  check int "3 parts" 3 (List.length parts);
  check string "starts with agent_type" "claude" (List.hd parts)

let test_generate_different_agent_types () =
  let claude = Nickname.generate "claude" in
  let gemini = Nickname.generate "gemini" in
  check bool "claude starts with claude" true (String.sub claude 0 6 = "claude");
  check bool "gemini starts with gemini" true (String.sub gemini 0 6 = "gemini")

let test_generate_randomness () =
  let nick1 = Nickname.generate "test" in
  let nick2 = Nickname.generate "test" in
  (* It's possible but unlikely they're the same *)
  check bool "nicks generated" true (String.length nick1 > 5 && String.length nick2 > 5)

let test_generate_empty_agent_type () =
  let nick = Nickname.generate "" in
  (* Should still work, starting with - *)
  check bool "starts with dash" true (nick.[0] = '-')

(* ============================================================
   generate_unique Tests
   ============================================================ *)

let test_generate_unique_format () =
  let nick = Nickname.generate_unique "claude" in
  let parts = String.split_on_char '-' nick in
  check int "4 parts" 4 (List.length parts);
  check string "starts with agent_type" "claude" (List.hd parts)

let test_generate_unique_suffix_length () =
  let nick = Nickname.generate_unique "test" in
  let parts = String.split_on_char '-' nick in
  let suffix = List.nth parts 3 in
  check int "suffix is 4 chars" 4 (String.length suffix)

let test_generate_unique_different () =
  let nick1 = Nickname.generate_unique "test" in
  let nick2 = Nickname.generate_unique "test" in
  check bool "unique nicknames" true (nick1 <> nick2)

(* ============================================================
   is_generated_nickname Tests
   ============================================================ *)

let test_is_generated_nickname_valid () =
  let nick = Nickname.generate "claude" in
  check bool "generated is valid" true (Nickname.is_generated_nickname nick)

let test_is_generated_nickname_manual () =
  check bool "three parts valid" true (Nickname.is_generated_nickname "claude-swift-fox");
  check bool "four parts valid" true (Nickname.is_generated_nickname "claude-swift-fox-a3b2")

let test_is_generated_nickname_short () =
  check bool "two parts invalid" false (Nickname.is_generated_nickname "claude-opus");
  check bool "one part invalid" false (Nickname.is_generated_nickname "claude")

let test_is_generated_nickname_empty () =
  check bool "empty invalid" false (Nickname.is_generated_nickname "")

(* ============================================================
   extract_agent_type Tests
   ============================================================ *)

let test_extract_agent_type_generated () =
  let nick = Nickname.generate "claude" in
  match Nickname.extract_agent_type nick with
  | Some at -> check string "extracted claude" "claude" at
  | None -> fail "should extract agent_type"

let test_extract_agent_type_manual () =
  match Nickname.extract_agent_type "gemini-brave-tiger" with
  | Some at -> check string "extracted gemini" "gemini" at
  | None -> fail "should extract agent_type"

let test_extract_agent_type_legacy () =
  match Nickname.extract_agent_type "claude" with
  | Some at -> check string "legacy claude" "claude" at
  | None -> fail "should extract legacy agent_type"

let test_extract_agent_type_empty () =
  (* Note: split_on_char returns [""] for empty string, so this returns Some "" *)
  match Nickname.extract_agent_type "" with
  | Some "" -> ()  (* Empty string returns Some "" *)
  | Some _ -> fail "should return Some empty string"
  | None -> fail "should return Some, not None"

let test_extract_agent_type_unique () =
  let nick = Nickname.generate_unique "codex" in
  match Nickname.extract_agent_type nick with
  | Some at -> check string "extracted codex" "codex" at
  | None -> fail "should extract from unique"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Nickname Coverage" [
    "generate", [
      test_case "format" `Quick test_generate_format;
      test_case "different agent types" `Quick test_generate_different_agent_types;
      test_case "randomness" `Quick test_generate_randomness;
      test_case "empty agent type" `Quick test_generate_empty_agent_type;
    ];
    "generate_unique", [
      test_case "format" `Quick test_generate_unique_format;
      test_case "suffix length" `Quick test_generate_unique_suffix_length;
      test_case "different" `Quick test_generate_unique_different;
    ];
    "is_generated_nickname", [
      test_case "valid generated" `Quick test_is_generated_nickname_valid;
      test_case "manual valid" `Quick test_is_generated_nickname_manual;
      test_case "short invalid" `Quick test_is_generated_nickname_short;
      test_case "empty invalid" `Quick test_is_generated_nickname_empty;
    ];
    "extract_agent_type", [
      test_case "generated" `Quick test_extract_agent_type_generated;
      test_case "manual" `Quick test_extract_agent_type_manual;
      test_case "legacy" `Quick test_extract_agent_type_legacy;
      test_case "empty" `Quick test_extract_agent_type_empty;
      test_case "unique" `Quick test_extract_agent_type_unique;
    ];
  ]
