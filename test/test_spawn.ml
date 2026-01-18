(** Tests for Spawn module - Agent spawning *)

open Masc_mcp

let test_get_config_known_agents () =
  (* Test get_config for known agents *)
  let claude = Spawn.get_config "claude" in
  Alcotest.(check bool) "claude config exists" true (Option.is_some claude);
  let claude = Option.get claude in
  Alcotest.(check string) "claude agent_name" "claude" claude.Spawn.agent_name;
  Alcotest.(check bool) "claude has mcp_tools" true (List.length claude.mcp_tools > 0);

  let gemini = Spawn.get_config "gemini" in
  Alcotest.(check bool) "gemini config exists" true (Option.is_some gemini);

  let codex = Spawn.get_config "codex" in
  Alcotest.(check bool) "codex config exists" true (Option.is_some codex);

  let ollama = Spawn.get_config "ollama" in
  Alcotest.(check bool) "ollama config exists" true (Option.is_some ollama);
  ()

let test_get_config_unknown_agent () =
  (* Test get_config for unknown agents returns None *)
  let unknown = Spawn.get_config "unknown-agent" in
  Alcotest.(check bool) "unknown config is None" true (Option.is_none unknown);
  ()

let contains s1 s2 =
  try
    let _ = Str.search_forward (Str.regexp_string s2) s1 0 in true
  with Not_found -> false

let test_result_to_string_success () =
  (* Test result formatting for success case *)
  let result = {
    Spawn.success = true;
    output = "Hello from agent";
    exit_code = 0;
    elapsed_ms = 1234;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  let s = Spawn.result_to_string result in
  Alcotest.(check bool) "contains completed text" true (contains s "completed");
  Alcotest.(check bool) "contains output" true (contains s "Hello from agent");
  ()

let test_result_to_string_failure () =
  (* Test result formatting for failure case *)
  let result = {
    Spawn.success = false;
    output = "Command not found";
    exit_code = 1;
    elapsed_ms = 100;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  let s = Spawn.result_to_string result in
  Alcotest.(check bool) "contains failed text" true (contains s "failed");
  Alcotest.(check bool) "contains error output" true (contains s "Command not found");
  Alcotest.(check int) "exit code is 1" 1 result.exit_code;
  ()

let test_result_to_json () =
  (* Test result JSON conversion *)
  let result = {
    Spawn.success = true;
    output = "test output";
    exit_code = 0;
    elapsed_ms = 500;
    input_tokens = Some 100;
    output_tokens = Some 50;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = Some 0.0025;
  } in
  let json = Spawn.result_to_json result in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "json success" true (json |> member "success" |> to_bool);
  Alcotest.(check int) "json exit_code" 0 (json |> member "exit_code" |> to_int);
  Alcotest.(check int) "json input_tokens" 100 (json |> member "input_tokens" |> to_int);
  ()

let test_masc_mcp_tools () =
  (* Test that masc_mcp_tools list is populated *)
  Alcotest.(check bool) "tools list not empty" true (List.length Spawn.masc_mcp_tools > 0);
  Alcotest.(check bool) "contains masc_status" true
    (List.mem "mcp__masc__masc_status" Spawn.masc_mcp_tools);
  Alcotest.(check bool) "contains masc_claim" true
    (List.mem "mcp__masc__masc_claim" Spawn.masc_mcp_tools);
  ()

let tests = [
  Alcotest.test_case "get_config known agents" `Quick test_get_config_known_agents;
  Alcotest.test_case "get_config unknown agent" `Quick test_get_config_unknown_agent;
  Alcotest.test_case "result_to_string success" `Quick test_result_to_string_success;
  Alcotest.test_case "result_to_string failure" `Quick test_result_to_string_failure;
  Alcotest.test_case "result_to_json" `Quick test_result_to_json;
  Alcotest.test_case "masc_mcp_tools populated" `Quick test_masc_mcp_tools;
]

let () =
  Alcotest.run "Spawn" [
    "Config", tests;
  ]
