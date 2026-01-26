(** Tests for @mention parsing - Stateless/Stateful/Broadcast modes *)

open Masc_mcp

let mention_mode_equal a b =
  match a, b with
  | Mention.Stateless s1, Mention.Stateless s2 -> s1 = s2
  | Mention.Stateful s1, Mention.Stateful s2 -> s1 = s2
  | Mention.Broadcast s1, Mention.Broadcast s2 -> s1 = s2
  | Mention.None, Mention.None -> true
  | _ -> false

(* ===== Stateless Mode Tests ===== *)

let test_stateless_basic () =
  let cases = [
    ("@ollama", Mention.Stateless "ollama");
    ("@gemini", Mention.Stateless "gemini");
    ("@claude", Mention.Stateless "claude");
    ("@codex", Mention.Stateless "codex");
    ("Hello @ollama what is 2+2?", Mention.Stateless "ollama");
    ("@glm 안녕하세요", Mention.Stateless "glm");
  ] in
  List.iter (fun (content, expected) ->
    let result = Mention.parse content in
    if not (mention_mode_equal result expected) then
      Alcotest.fail (Printf.sprintf
        "Stateless parse failed: '%s' => got %s, expected %s"
        content (Mention.mode_to_string result) (Mention.mode_to_string expected))
  ) cases

let test_stateless_with_underscore () =
  let cases = [
    ("@my_agent", Mention.Stateless "my_agent");
    ("@claude_v2", Mention.Stateless "claude_v2");
  ] in
  List.iter (fun (content, expected) ->
    let result = Mention.parse content in
    if not (mention_mode_equal result expected) then
      Alcotest.fail (Printf.sprintf
        "Underscore parse failed: '%s' => got %s, expected %s"
        content (Mention.mode_to_string result) (Mention.mode_to_string expected))
  ) cases

(* ===== Stateful Mode Tests ===== *)

let test_stateful_nickname () =
  let cases = [
    ("@ollama-gentle-gecko", Mention.Stateful "ollama-gentle-gecko");
    ("@gemini-swift-tiger", Mention.Stateful "gemini-swift-tiger");
    ("@claude-bold-shark", Mention.Stateful "claude-bold-shark");
    ("Hey @codex-keen-otter can you help?", Mention.Stateful "codex-keen-otter");
  ] in
  List.iter (fun (content, expected) ->
    let result = Mention.parse content in
    if not (mention_mode_equal result expected) then
      Alcotest.fail (Printf.sprintf
        "Stateful parse failed: '%s' => got %s, expected %s"
        content (Mention.mode_to_string result) (Mention.mode_to_string expected))
  ) cases

(* ===== Broadcast Mode Tests ===== *)

let test_broadcast_basic () =
  let cases = [
    ("@@ollama", Mention.Broadcast "ollama");
    ("@@gemini", Mention.Broadcast "gemini");
    ("Hey @@claude everyone respond!", Mention.Broadcast "claude");
    ("@@all 전체 공지입니다", Mention.Broadcast "all");
  ] in
  List.iter (fun (content, expected) ->
    let result = Mention.parse content in
    if not (mention_mode_equal result expected) then
      Alcotest.fail (Printf.sprintf
        "Broadcast parse failed: '%s' => got %s, expected %s"
        content (Mention.mode_to_string result) (Mention.mode_to_string expected))
  ) cases

(* ===== No Mention Tests ===== *)

let test_no_mention () =
  let cases = [
    "Hello world";
    "No mention here";
    "";
  ] in
  List.iter (fun content ->
    let result = Mention.parse content in
    if result <> Mention.None then
      Alcotest.fail (Printf.sprintf
        "Expected None for '%s', got %s"
        content (Mention.mode_to_string result))
  ) cases

(* ===== Priority Tests ===== *)

let test_broadcast_priority () =
  (* @@agent should take priority over @agent *)
  let content = "@@ollama @gemini" in
  let result = Mention.parse content in
  if not (mention_mode_equal result (Mention.Broadcast "ollama")) then
    Alcotest.fail (Printf.sprintf
      "Broadcast should have priority: got %s"
      (Mention.mode_to_string result))

let test_stateful_priority () =
  (* @agent-adj-animal should be Stateful, not Stateless *)
  let content = "@ollama-gentle-gecko" in
  let result = Mention.parse content in
  if not (mention_mode_equal result (Mention.Stateful "ollama-gentle-gecko")) then
    Alcotest.fail (Printf.sprintf
      "Stateful should be detected: got %s"
      (Mention.mode_to_string result))

(* ===== Edge Cases ===== *)

let test_korean_message () =
  let cases = [
    ("@ollama 안녕하세요 질문있어요", Mention.Stateless "ollama");
    ("@@gemini 모두에게 공지", Mention.Broadcast "gemini");
  ] in
  List.iter (fun (content, expected) ->
    let result = Mention.parse content in
    if not (mention_mode_equal result expected) then
      Alcotest.fail (Printf.sprintf
        "Korean message failed: '%s' => got %s"
        content (Mention.mode_to_string result))
  ) cases

let test_multiple_mentions () =
  (* First mention should be extracted *)
  let content = "@ollama @gemini @claude" in
  let result = Mention.parse content in
  if not (mention_mode_equal result (Mention.Stateless "ollama")) then
    Alcotest.fail (Printf.sprintf
      "First mention should be extracted: got %s"
      (Mention.mode_to_string result))

(* ===== Target Resolution Tests ===== *)

let test_resolve_stateless () =
  let available = ["ollama-gentle-gecko"; "ollama-bold-shark"; "gemini-swift-tiger"] in
  let targets = Mention.resolve_targets (Mention.Stateless "ollama") ~available_agents:available in
  match targets with
  | [first] when String.sub first 0 6 = "ollama" -> ()
  | _ -> Alcotest.fail "Should resolve to one ollama agent"

let test_resolve_stateful () =
  let available = ["ollama-gentle-gecko"; "ollama-bold-shark"; "gemini-swift-tiger"] in
  let targets = Mention.resolve_targets (Mention.Stateful "ollama-gentle-gecko") ~available_agents:available in
  match targets with
  | ["ollama-gentle-gecko"] -> ()
  | _ -> Alcotest.fail "Should resolve to exact match only"

let test_resolve_broadcast () =
  let available = ["ollama-gentle-gecko"; "ollama-bold-shark"; "gemini-swift-tiger"] in
  let targets = Mention.resolve_targets (Mention.Broadcast "ollama") ~available_agents:available in
  if List.length targets = 2 && List.for_all (fun n -> String.sub n 0 6 = "ollama") targets then
    ()
  else
    Alcotest.fail "Should resolve to all ollama agents"

(* ===== Agent Type Extraction Tests ===== *)

let test_agent_type_extraction () =
  let cases = [
    ("ollama", "ollama");
    ("ollama-gentle-gecko", "ollama");
    ("gemini-swift-tiger", "gemini");
    ("claude_v2", "claude_v2");  (* underscore stays *)
  ] in
  List.iter (fun (input, expected) ->
    let result = Mention.agent_type_of_mention input in
    if result <> expected then
      Alcotest.fail (Printf.sprintf
        "Agent type extraction failed: '%s' => got '%s', expected '%s'"
        input result expected)
  ) cases

(* ===== Backward Compatibility Tests ===== *)

let test_extract_backward_compat () =
  let cases = [
    ("@ollama", Some "ollama");
    ("@ollama-gentle-gecko", Some "ollama-gentle-gecko");
    ("@@gemini", Some "gemini");
    ("Hello world", None);
  ] in
  List.iter (fun (content, expected) ->
    let result = Mention.extract content in
    if result <> expected then
      Alcotest.fail (Printf.sprintf
        "Extract backward compat failed: '%s' => got %s"
        content (match result with Some s -> s | None -> "None"))
  ) cases

(* ===== Test Suite ===== *)

let () =
  let open Alcotest in
  run "Mention Parsing" [
    "stateless", [
      test_case "basic" `Quick test_stateless_basic;
      test_case "underscore" `Quick test_stateless_with_underscore;
    ];
    "stateful", [
      test_case "nickname" `Quick test_stateful_nickname;
    ];
    "broadcast", [
      test_case "basic" `Quick test_broadcast_basic;
    ];
    "none", [
      test_case "no_mention" `Quick test_no_mention;
    ];
    "priority", [
      test_case "broadcast_over_stateless" `Quick test_broadcast_priority;
      test_case "stateful_detection" `Quick test_stateful_priority;
    ];
    "edge_cases", [
      test_case "korean" `Quick test_korean_message;
      test_case "multiple" `Quick test_multiple_mentions;
    ];
    "resolution", [
      test_case "stateless" `Quick test_resolve_stateless;
      test_case "stateful" `Quick test_resolve_stateful;
      test_case "broadcast" `Quick test_resolve_broadcast;
    ];
    "utility", [
      test_case "agent_type" `Quick test_agent_type_extraction;
      test_case "backward_compat" `Quick test_extract_backward_compat;
    ];
  ]
