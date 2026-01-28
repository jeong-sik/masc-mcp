(** Hat Module Coverage Tests

    Tests for Hat System - Role-based Persona:
    - to_string, of_string, to_emoji
    - default_config
*)

open Alcotest

module Hat = Masc_mcp.Hat

(* ============================================================
   Serialization Tests
   ============================================================ *)

let test_to_string_builder () =
  check string "builder" "builder" (Hat.to_string Hat.Builder)

let test_to_string_reviewer () =
  check string "reviewer" "reviewer" (Hat.to_string Hat.Reviewer)

let test_to_string_researcher () =
  check string "researcher" "researcher" (Hat.to_string Hat.Researcher)

let test_to_string_tester () =
  check string "tester" "tester" (Hat.to_string Hat.Tester)

let test_to_string_architect () =
  check string "architect" "architect" (Hat.to_string Hat.Architect)

let test_to_string_debugger () =
  check string "debugger" "debugger" (Hat.to_string Hat.Debugger)

let test_to_string_documenter () =
  check string "documenter" "documenter" (Hat.to_string Hat.Documenter)

let test_to_string_custom () =
  check string "custom" "my-hat" (Hat.to_string (Hat.Custom "my-hat"))

let test_of_string_builder () =
  match Hat.of_string "builder" with
  | Hat.Builder -> check bool "builder" true true
  | _ -> fail "expected Builder"

let test_of_string_build () =
  match Hat.of_string "build" with
  | Hat.Builder -> check bool "build alias" true true
  | _ -> fail "expected Builder"

let test_of_string_impl () =
  match Hat.of_string "impl" with
  | Hat.Builder -> check bool "impl alias" true true
  | _ -> fail "expected Builder"

let test_of_string_reviewer () =
  match Hat.of_string "reviewer" with
  | Hat.Reviewer -> check bool "reviewer" true true
  | _ -> fail "expected Reviewer"

let test_of_string_review () =
  match Hat.of_string "review" with
  | Hat.Reviewer -> check bool "review alias" true true
  | _ -> fail "expected Reviewer"

let test_of_string_researcher () =
  match Hat.of_string "researcher" with
  | Hat.Researcher -> check bool "researcher" true true
  | _ -> fail "expected Researcher"

let test_of_string_research () =
  match Hat.of_string "research" with
  | Hat.Researcher -> check bool "research alias" true true
  | _ -> fail "expected Researcher"

let test_of_string_explore () =
  match Hat.of_string "explore" with
  | Hat.Researcher -> check bool "explore alias" true true
  | _ -> fail "expected Researcher"

let test_of_string_tester () =
  match Hat.of_string "tester" with
  | Hat.Tester -> check bool "tester" true true
  | _ -> fail "expected Tester"

let test_of_string_test () =
  match Hat.of_string "test" with
  | Hat.Tester -> check bool "test alias" true true
  | _ -> fail "expected Tester"

let test_of_string_architect () =
  match Hat.of_string "architect" with
  | Hat.Architect -> check bool "architect" true true
  | _ -> fail "expected Architect"

let test_of_string_arch () =
  match Hat.of_string "arch" with
  | Hat.Architect -> check bool "arch alias" true true
  | _ -> fail "expected Architect"

let test_of_string_design () =
  match Hat.of_string "design" with
  | Hat.Architect -> check bool "design alias" true true
  | _ -> fail "expected Architect"

let test_of_string_debugger () =
  match Hat.of_string "debugger" with
  | Hat.Debugger -> check bool "debugger" true true
  | _ -> fail "expected Debugger"

let test_of_string_debug () =
  match Hat.of_string "debug" with
  | Hat.Debugger -> check bool "debug alias" true true
  | _ -> fail "expected Debugger"

let test_of_string_fix () =
  match Hat.of_string "fix" with
  | Hat.Debugger -> check bool "fix alias" true true
  | _ -> fail "expected Debugger"

let test_of_string_documenter () =
  match Hat.of_string "documenter" with
  | Hat.Documenter -> check bool "documenter" true true
  | _ -> fail "expected Documenter"

let test_of_string_docs () =
  match Hat.of_string "docs" with
  | Hat.Documenter -> check bool "docs alias" true true
  | _ -> fail "expected Documenter"

let test_of_string_doc () =
  match Hat.of_string "doc" with
  | Hat.Documenter -> check bool "doc alias" true true
  | _ -> fail "expected Documenter"

let test_of_string_unknown () =
  match Hat.of_string "unknown-hat" with
  | Hat.Custom s -> check string "custom" "unknown-hat" s
  | _ -> fail "expected Custom"

(* ============================================================
   Emoji Tests
   ============================================================ *)

let test_to_emoji_builder () =
  check string "builder emoji" "🔨" (Hat.to_emoji Hat.Builder)

let test_to_emoji_reviewer () =
  check string "reviewer emoji" "🔍" (Hat.to_emoji Hat.Reviewer)

let test_to_emoji_researcher () =
  check string "researcher emoji" "🔬" (Hat.to_emoji Hat.Researcher)

let test_to_emoji_tester () =
  check string "tester emoji" "🧪" (Hat.to_emoji Hat.Tester)

let test_to_emoji_architect () =
  check string "architect emoji" "📐" (Hat.to_emoji Hat.Architect)

let test_to_emoji_debugger () =
  check string "debugger emoji" "🐛" (Hat.to_emoji Hat.Debugger)

let test_to_emoji_documenter () =
  check string "documenter emoji" "📝" (Hat.to_emoji Hat.Documenter)

let test_to_emoji_custom () =
  check string "custom emoji" "🎭" (Hat.to_emoji (Hat.Custom "any"))

(* ============================================================
   Default Config Tests
   ============================================================ *)

let test_default_config_builder () =
  let cfg = Hat.default_config Hat.Builder in
  check string "name" "builder" cfg.name;
  check bool "has instructions" true (String.length cfg.instructions > 0);
  check bool "has triggers" true (List.length cfg.triggers > 0)

let test_default_config_reviewer () =
  let cfg = Hat.default_config Hat.Reviewer in
  check string "name" "reviewer" cfg.name;
  check bool "has instructions" true (String.length cfg.instructions > 0)

let test_default_config_researcher () =
  let cfg = Hat.default_config Hat.Researcher in
  check string "name" "researcher" cfg.name;
  check bool "has backend" true (cfg.backend <> None)

let test_default_config_tester () =
  let cfg = Hat.default_config Hat.Tester in
  check string "name" "tester" cfg.name

let test_default_config_architect () =
  let cfg = Hat.default_config Hat.Architect in
  check string "name" "architect" cfg.name

let test_default_config_debugger () =
  let cfg = Hat.default_config Hat.Debugger in
  check string "name" "debugger" cfg.name

let test_default_config_documenter () =
  let cfg = Hat.default_config Hat.Documenter in
  check string "name" "documenter" cfg.name

let test_default_config_custom () =
  let cfg = Hat.default_config (Hat.Custom "my-custom") in
  check string "name" "my-custom" cfg.name

(* ============================================================
   Roundtrip Tests
   ============================================================ *)

let test_roundtrip_builder () =
  let h = Hat.Builder in
  let s = Hat.to_string h in
  match Hat.of_string s with
  | Hat.Builder -> check bool "roundtrip" true true
  | _ -> fail "roundtrip failed"

let test_roundtrip_reviewer () =
  let h = Hat.Reviewer in
  let s = Hat.to_string h in
  match Hat.of_string s with
  | Hat.Reviewer -> check bool "roundtrip" true true
  | _ -> fail "roundtrip failed"

let test_roundtrip_all_hats () =
  let hats = [
    Hat.Builder; Hat.Reviewer; Hat.Researcher;
    Hat.Tester; Hat.Architect; Hat.Debugger; Hat.Documenter
  ] in
  let all_match = List.for_all (fun h ->
    let s = Hat.to_string h in
    match Hat.of_string s with
    | h2 -> h = h2
  ) hats in
  check bool "all roundtrips" true all_match

(* ============================================================
   parse_hatted_mention Tests
   ============================================================ *)

let test_parse_hatted_mention_simple () =
  match Hat.parse_hatted_mention "@claude:builder" with
  | Some (agent, Hat.Builder) -> check string "agent" "claude" agent
  | _ -> fail "expected Some (claude, Builder)"

let test_parse_hatted_mention_reviewer () =
  match Hat.parse_hatted_mention "hello @gemini:reviewer world" with
  | Some (agent, Hat.Reviewer) -> check string "agent" "gemini" agent
  | _ -> fail "expected Some (gemini, Reviewer)"

let test_parse_hatted_mention_custom () =
  match Hat.parse_hatted_mention "@agent:custom-hat" with
  | Some (agent, Hat.Custom s) ->
    check string "agent" "agent" agent;
    check string "hat" "custom-hat" s
  | _ -> fail "expected Some with Custom"

let test_parse_hatted_mention_none () =
  match Hat.parse_hatted_mention "no mention here" with
  | None -> ()
  | Some _ -> fail "expected None"

let test_parse_hatted_mention_no_colon () =
  match Hat.parse_hatted_mention "@claude" with
  | None -> ()
  | Some _ -> fail "expected None for @mention without colon"

(* ============================================================
   extract_hatted_mentions Tests
   ============================================================ *)

let test_extract_hatted_mentions_single () =
  let mentions = Hat.extract_hatted_mentions "@claude:builder" in
  check int "count" 1 (List.length mentions)

let test_extract_hatted_mentions_multiple () =
  let mentions = Hat.extract_hatted_mentions "@claude:builder @gemini:reviewer @codex:tester" in
  check int "count" 3 (List.length mentions)

let test_extract_hatted_mentions_empty () =
  let mentions = Hat.extract_hatted_mentions "no mentions here" in
  check int "count" 0 (List.length mentions)

let test_extract_hatted_mentions_mixed () =
  let mentions = Hat.extract_hatted_mentions "@claude:builder some text @gemini:debugger more text" in
  check int "count" 2 (List.length mentions);
  let (agent1, _) = List.hd mentions in
  check string "first agent" "claude" agent1

(* ============================================================
   rotation_to_string Tests
   ============================================================ *)

let test_rotation_to_string_single () =
  let s = Hat.rotation_to_string (Hat.Single Hat.Builder) in
  check bool "contains single" true
    (try let _ = Str.search_forward (Str.regexp "single") s 0 in true
     with Not_found -> false)

let test_rotation_to_string_alternate () =
  let s = Hat.rotation_to_string (Hat.Alternate (Hat.Builder, Hat.Reviewer)) in
  check bool "contains alt" true
    (try let _ = Str.search_forward (Str.regexp "alt") s 0 in true
     with Not_found -> false)

let test_rotation_to_string_sequence () =
  let s = Hat.rotation_to_string (Hat.Sequence [Hat.Builder; Hat.Tester]) in
  check bool "contains seq" true
    (try let _ = Str.search_forward (Str.regexp "seq") s 0 in true
     with Not_found -> false)

let test_rotation_to_string_tdd () =
  check string "tdd" "tdd" (Hat.rotation_to_string Hat.TDD)

let test_rotation_to_string_review_loop () =
  check string "review-loop" "review-loop" (Hat.rotation_to_string Hat.ReviewLoop)

(* ============================================================
   next_in_rotation Tests
   ============================================================ *)

let test_next_in_rotation_single () =
  let (hat, idx) = Hat.next_in_rotation (Hat.Single Hat.Builder) 0 in
  check bool "always builder" true (hat = Hat.Builder);
  check int "idx 0" 0 idx

let test_next_in_rotation_alternate_even () =
  let (hat, idx) = Hat.next_in_rotation (Hat.Alternate (Hat.Builder, Hat.Reviewer)) 0 in
  check bool "first is builder" true (hat = Hat.Builder);
  check int "idx 1" 1 idx

let test_next_in_rotation_alternate_odd () =
  let (hat, idx) = Hat.next_in_rotation (Hat.Alternate (Hat.Builder, Hat.Reviewer)) 1 in
  check bool "second is reviewer" true (hat = Hat.Reviewer);
  check int "idx 2" 2 idx

let test_next_in_rotation_sequence () =
  let seq = Hat.Sequence [Hat.Builder; Hat.Tester; Hat.Reviewer] in
  let (h0, _) = Hat.next_in_rotation seq 0 in
  let (h1, _) = Hat.next_in_rotation seq 1 in
  let (h2, _) = Hat.next_in_rotation seq 2 in
  let (h3, _) = Hat.next_in_rotation seq 3 in
  check bool "idx 0 builder" true (h0 = Hat.Builder);
  check bool "idx 1 tester" true (h1 = Hat.Tester);
  check bool "idx 2 reviewer" true (h2 = Hat.Reviewer);
  check bool "idx 3 wraps to builder" true (h3 = Hat.Builder)

let test_next_in_rotation_empty_sequence () =
  let (hat, _) = Hat.next_in_rotation (Hat.Sequence []) 0 in
  check bool "empty defaults to builder" true (hat = Hat.Builder)

let test_next_in_rotation_tdd_even () =
  let (hat, _) = Hat.next_in_rotation Hat.TDD 0 in
  check bool "TDD even is tester" true (hat = Hat.Tester)

let test_next_in_rotation_tdd_odd () =
  let (hat, _) = Hat.next_in_rotation Hat.TDD 1 in
  check bool "TDD odd is builder" true (hat = Hat.Builder)

let test_next_in_rotation_review_loop_even () =
  let (hat, _) = Hat.next_in_rotation Hat.ReviewLoop 0 in
  check bool "ReviewLoop even is builder" true (hat = Hat.Builder)

let test_next_in_rotation_review_loop_odd () =
  let (hat, _) = Hat.next_in_rotation Hat.ReviewLoop 1 in
  check bool "ReviewLoop odd is reviewer" true (hat = Hat.Reviewer)

(* ============================================================
   JSON Tests
   ============================================================ *)

let test_to_json_has_fields () =
  let json = Hat.to_json Hat.Builder in
  match json with
  | `Assoc fields ->
    check bool "has hat" true (List.mem_assoc "hat" fields);
    check bool "has emoji" true (List.mem_assoc "emoji" fields)
  | _ -> fail "expected Assoc"

let test_config_to_json_has_fields () =
  let cfg = Hat.default_config Hat.Builder in
  let json = Hat.config_to_json cfg in
  match json with
  | `Assoc fields ->
    check bool "has name" true (List.mem_assoc "name" fields);
    check bool "has instructions" true (List.mem_assoc "instructions" fields);
    check bool "has triggers" true (List.mem_assoc "triggers" fields);
    check bool "has emits" true (List.mem_assoc "emits" fields);
    check bool "has backend" true (List.mem_assoc "backend" fields)
  | _ -> fail "expected Assoc"

let test_config_to_json_backend_null () =
  let cfg = Hat.default_config Hat.Builder in
  let json = Hat.config_to_json cfg in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "backend" fields with
     | Some `Null -> ()
     | _ -> fail "expected Null backend for Builder")
  | _ -> fail "expected Assoc"

let test_config_to_json_backend_some () =
  let cfg = Hat.default_config Hat.Researcher in
  let json = Hat.config_to_json cfg in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "backend" fields with
     | Some (`String _) -> ()
     | _ -> fail "expected String backend for Researcher")
  | _ -> fail "expected Assoc"

(* ============================================================
   Hat Registry Tests (Stateful)
   ============================================================ *)

let test_wear_hat () =
  let result = Hat.wear ~agent_name:"test-agent-1" Hat.Builder in
  check bool "contains agent name" true
    (try let _ = Str.search_forward (Str.regexp "test-agent-1") result 0 in true
     with Not_found -> false)

let test_wear_hat_changes_current () =
  let _ = Hat.wear ~agent_name:"test-agent-2" Hat.Builder in
  let _ = Hat.wear ~agent_name:"test-agent-2" Hat.Reviewer in
  let current = Hat.current ~agent_name:"test-agent-2" in
  check bool "current is reviewer" true (current = Hat.Reviewer)

let test_current_default_builder () =
  let current = Hat.current ~agent_name:"new-agent-never-seen" in
  check bool "default builder" true (current = Hat.Builder)

let test_format_agent () =
  let _ = Hat.wear ~agent_name:"format-test" Hat.Tester in
  let formatted = Hat.format_agent "format-test" in
  check bool "contains agent name" true
    (try let _ = Str.search_forward (Str.regexp "format-test") formatted 0 in true
     with Not_found -> false);
  check bool "contains hat name" true
    (try let _ = Str.search_forward (Str.regexp "tester") formatted 0 in true
     with Not_found -> false)

let test_hatted_agent_to_json () =
  let _ = Hat.wear ~agent_name:"json-test-agent" Hat.Architect in
  let agent = Hat.get_agent "json-test-agent" in
  let json = Hat.hatted_agent_to_json agent in
  match json with
  | `Assoc fields ->
    check bool "has agent_name" true (List.mem_assoc "agent_name" fields);
    check bool "has current_hat" true (List.mem_assoc "current_hat" fields);
    check bool "has hat_changes" true (List.mem_assoc "hat_changes" fields)
  | _ -> fail "expected Assoc"

let test_list_all () =
  let _ = Hat.wear ~agent_name:"list-test-1" Hat.Builder in
  let _ = Hat.wear ~agent_name:"list-test-2" Hat.Debugger in
  let agents = Hat.list_all () in
  (* Should have at least 2 agents (might have more from other tests) *)
  check bool "at least 2 agents" true (List.length agents >= 2)

let test_get_agent_creates_if_missing () =
  let agent = Hat.get_agent "totally-new-agent" in
  check string "agent_name" "totally-new-agent" agent.agent_name;
  check bool "default hat is builder" true (agent.current_hat = Hat.Builder)

let test_wear_builds_history () =
  let _ = Hat.wear ~agent_name:"history-agent" Hat.Builder in
  let _ = Hat.wear ~agent_name:"history-agent" Hat.Tester in
  let _ = Hat.wear ~agent_name:"history-agent" Hat.Reviewer in
  let agent = Hat.get_agent "history-agent" in
  check bool "has history" true (List.length agent.hat_history >= 3)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Hat Coverage" [
    "to_string", [
      test_case "builder" `Quick test_to_string_builder;
      test_case "reviewer" `Quick test_to_string_reviewer;
      test_case "researcher" `Quick test_to_string_researcher;
      test_case "tester" `Quick test_to_string_tester;
      test_case "architect" `Quick test_to_string_architect;
      test_case "debugger" `Quick test_to_string_debugger;
      test_case "documenter" `Quick test_to_string_documenter;
      test_case "custom" `Quick test_to_string_custom;
    ];
    "of_string", [
      test_case "builder" `Quick test_of_string_builder;
      test_case "build" `Quick test_of_string_build;
      test_case "impl" `Quick test_of_string_impl;
      test_case "reviewer" `Quick test_of_string_reviewer;
      test_case "review" `Quick test_of_string_review;
      test_case "researcher" `Quick test_of_string_researcher;
      test_case "research" `Quick test_of_string_research;
      test_case "explore" `Quick test_of_string_explore;
      test_case "tester" `Quick test_of_string_tester;
      test_case "test" `Quick test_of_string_test;
      test_case "architect" `Quick test_of_string_architect;
      test_case "arch" `Quick test_of_string_arch;
      test_case "design" `Quick test_of_string_design;
      test_case "debugger" `Quick test_of_string_debugger;
      test_case "debug" `Quick test_of_string_debug;
      test_case "fix" `Quick test_of_string_fix;
      test_case "documenter" `Quick test_of_string_documenter;
      test_case "docs" `Quick test_of_string_docs;
      test_case "doc" `Quick test_of_string_doc;
      test_case "unknown" `Quick test_of_string_unknown;
    ];
    "to_emoji", [
      test_case "builder" `Quick test_to_emoji_builder;
      test_case "reviewer" `Quick test_to_emoji_reviewer;
      test_case "researcher" `Quick test_to_emoji_researcher;
      test_case "tester" `Quick test_to_emoji_tester;
      test_case "architect" `Quick test_to_emoji_architect;
      test_case "debugger" `Quick test_to_emoji_debugger;
      test_case "documenter" `Quick test_to_emoji_documenter;
      test_case "custom" `Quick test_to_emoji_custom;
    ];
    "default_config", [
      test_case "builder" `Quick test_default_config_builder;
      test_case "reviewer" `Quick test_default_config_reviewer;
      test_case "researcher" `Quick test_default_config_researcher;
      test_case "tester" `Quick test_default_config_tester;
      test_case "architect" `Quick test_default_config_architect;
      test_case "debugger" `Quick test_default_config_debugger;
      test_case "documenter" `Quick test_default_config_documenter;
      test_case "custom" `Quick test_default_config_custom;
    ];
    "roundtrip", [
      test_case "builder" `Quick test_roundtrip_builder;
      test_case "reviewer" `Quick test_roundtrip_reviewer;
      test_case "all hats" `Quick test_roundtrip_all_hats;
    ];
    "parse_hatted_mention", [
      test_case "simple" `Quick test_parse_hatted_mention_simple;
      test_case "reviewer" `Quick test_parse_hatted_mention_reviewer;
      test_case "custom" `Quick test_parse_hatted_mention_custom;
      test_case "none" `Quick test_parse_hatted_mention_none;
      test_case "no colon" `Quick test_parse_hatted_mention_no_colon;
    ];
    "extract_hatted_mentions", [
      test_case "single" `Quick test_extract_hatted_mentions_single;
      test_case "multiple" `Quick test_extract_hatted_mentions_multiple;
      test_case "empty" `Quick test_extract_hatted_mentions_empty;
      test_case "mixed" `Quick test_extract_hatted_mentions_mixed;
    ];
    "rotation_to_string", [
      test_case "single" `Quick test_rotation_to_string_single;
      test_case "alternate" `Quick test_rotation_to_string_alternate;
      test_case "sequence" `Quick test_rotation_to_string_sequence;
      test_case "tdd" `Quick test_rotation_to_string_tdd;
      test_case "review loop" `Quick test_rotation_to_string_review_loop;
    ];
    "next_in_rotation", [
      test_case "single" `Quick test_next_in_rotation_single;
      test_case "alternate even" `Quick test_next_in_rotation_alternate_even;
      test_case "alternate odd" `Quick test_next_in_rotation_alternate_odd;
      test_case "sequence" `Quick test_next_in_rotation_sequence;
      test_case "empty sequence" `Quick test_next_in_rotation_empty_sequence;
      test_case "tdd even" `Quick test_next_in_rotation_tdd_even;
      test_case "tdd odd" `Quick test_next_in_rotation_tdd_odd;
      test_case "review loop even" `Quick test_next_in_rotation_review_loop_even;
      test_case "review loop odd" `Quick test_next_in_rotation_review_loop_odd;
    ];
    "json", [
      test_case "to_json has fields" `Quick test_to_json_has_fields;
      test_case "config_to_json has fields" `Quick test_config_to_json_has_fields;
      test_case "config backend null" `Quick test_config_to_json_backend_null;
      test_case "config backend some" `Quick test_config_to_json_backend_some;
    ];
    "hat_registry", [
      test_case "wear hat" `Quick test_wear_hat;
      test_case "wear changes current" `Quick test_wear_hat_changes_current;
      test_case "current default builder" `Quick test_current_default_builder;
      test_case "format agent" `Quick test_format_agent;
      test_case "hatted_agent_to_json" `Quick test_hatted_agent_to_json;
      test_case "list all" `Quick test_list_all;
      test_case "get agent creates if missing" `Quick test_get_agent_creates_if_missing;
      test_case "wear builds history" `Quick test_wear_builds_history;
    ];
  ]
