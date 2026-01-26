(** Hat System Tests *)

open Masc_mcp.Hat

let test_of_string () =
  Alcotest.(check string) "builder" "builder" (to_string (of_string "builder"));
  Alcotest.(check string) "reviewer" "reviewer" (to_string (of_string "reviewer"));
  Alcotest.(check string) "review alias" "reviewer" (to_string (of_string "review"));
  Alcotest.(check string) "custom" "my-hat" (to_string (of_string "my-hat"));
  ()

let test_to_emoji () =
  Alcotest.(check string) "builder emoji" "🔨" (to_emoji Builder);
  Alcotest.(check string) "reviewer emoji" "🔍" (to_emoji Reviewer);
  Alcotest.(check string) "tester emoji" "🧪" (to_emoji Tester);
  Alcotest.(check string) "custom emoji" "🎭" (to_emoji (Custom "foo"));
  ()

let test_wear_and_current () =
  let agent = "test-agent-1" in
  let _ = wear ~agent_name:agent Builder in
  Alcotest.(check string) "current is builder" "builder" (to_string (current ~agent_name:agent));
  let _ = wear ~agent_name:agent Reviewer in
  Alcotest.(check string) "current is reviewer" "reviewer" (to_string (current ~agent_name:agent));
  ()

let test_parse_hatted_mention () =
  let result = parse_hatted_mention "@claude:builder needs to fix this" in
  (match result with
   | Some (agent, hat) ->
       Alcotest.(check string) "agent" "claude" agent;
       Alcotest.(check string) "hat" "builder" (to_string hat)
   | None -> Alcotest.fail "Should parse hatted mention");

  let result2 = parse_hatted_mention "no mention here" in
  Alcotest.(check bool) "no match" true (result2 = None);
  ()

let test_extract_multiple_mentions () =
  let mentions = extract_hatted_mentions "@claude:builder and @gemini:reviewer should collaborate" in
  Alcotest.(check int) "two mentions" 2 (List.length mentions);
  ()

let test_rotation_single () =
  let (hat, idx) = next_in_rotation (Single Tester) 0 in
  Alcotest.(check string) "single always tester" "tester" (to_string hat);
  Alcotest.(check int) "idx stays 0" 0 idx;
  ()

let test_rotation_tdd () =
  let (hat0, idx1) = next_in_rotation TDD 0 in
  Alcotest.(check string) "tdd starts with tester" "tester" (to_string hat0);
  let (hat1, idx2) = next_in_rotation TDD idx1 in
  Alcotest.(check string) "tdd then builder" "builder" (to_string hat1);
  let (hat2, _) = next_in_rotation TDD idx2 in
  Alcotest.(check string) "tdd back to tester" "tester" (to_string hat2);
  ()

let test_default_config () =
  let cfg = default_config Builder in
  Alcotest.(check string) "builder name" "builder" cfg.name;
  Alcotest.(check bool) "has triggers" true (List.length cfg.triggers > 0);
  Alcotest.(check bool) "has emits" true (List.length cfg.emits > 0);
  ()

let () =
  Alcotest.run "Hat System" [
    "serialization", [
      Alcotest.test_case "of_string" `Quick test_of_string;
      Alcotest.test_case "to_emoji" `Quick test_to_emoji;
    ];
    "wear", [
      Alcotest.test_case "wear_and_current" `Quick test_wear_and_current;
    ];
    "parsing", [
      Alcotest.test_case "parse_hatted_mention" `Quick test_parse_hatted_mention;
      Alcotest.test_case "extract_multiple" `Quick test_extract_multiple_mentions;
    ];
    "rotation", [
      Alcotest.test_case "single" `Quick test_rotation_single;
      Alcotest.test_case "tdd" `Quick test_rotation_tdd;
    ];
    "config", [
      Alcotest.test_case "default_config" `Quick test_default_config;
    ];
  ]
