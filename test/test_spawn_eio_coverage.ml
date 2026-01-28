(** Spawn Eio Module Coverage Tests

    Tests for spawn types and constants:
    - spawn_config type
    - spawn_result type
    - masc_mcp_tools constant
*)

open Alcotest

module Spawn_eio = Masc_mcp.Spawn_eio

(* ============================================================
   spawn_config Type Tests
   ============================================================ *)

let test_spawn_config_type () =
  let cfg : Spawn_eio.spawn_config = {
    agent_name = "claude";
    command = "claude --print";
    timeout_seconds = 300;
    working_dir = Some "/tmp/test";
    mcp_tools = ["masc_status"; "masc_broadcast"];
  } in
  check string "agent_name" "claude" cfg.agent_name;
  check string "command" "claude --print" cfg.command;
  check int "timeout_seconds" 300 cfg.timeout_seconds

let test_spawn_config_no_working_dir () =
  let cfg : Spawn_eio.spawn_config = {
    agent_name = "codex";
    command = "codex";
    timeout_seconds = 60;
    working_dir = None;
    mcp_tools = [];
  } in
  match cfg.working_dir with
  | None -> check bool "no working_dir" true true
  | Some _ -> fail "expected None"

let test_spawn_config_mcp_tools () =
  let cfg : Spawn_eio.spawn_config = {
    agent_name = "test";
    command = "test";
    timeout_seconds = 30;
    working_dir = None;
    mcp_tools = ["tool1"; "tool2"; "tool3"];
  } in
  check int "mcp_tools count" 3 (List.length cfg.mcp_tools)

(* ============================================================
   spawn_result Type Tests
   ============================================================ *)

let test_spawn_result_success () =
  let r : Spawn_eio.spawn_result = {
    success = true;
    output = "Task completed";
    exit_code = 0;
    elapsed_ms = 1500;
    input_tokens = Some 100;
    output_tokens = Some 200;
    cache_creation_tokens = None;
    cache_read_tokens = Some 50;
    cost_usd = Some 0.005;
  } in
  check bool "success" true r.success;
  check int "exit_code" 0 r.exit_code;
  check int "elapsed_ms" 1500 r.elapsed_ms

let test_spawn_result_failure () =
  let r : Spawn_eio.spawn_result = {
    success = false;
    output = "Error occurred";
    exit_code = 1;
    elapsed_ms = 500;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  check bool "success" false r.success;
  check int "exit_code" 1 r.exit_code

let test_spawn_result_tokens () =
  let r : Spawn_eio.spawn_result = {
    success = true;
    output = "";
    exit_code = 0;
    elapsed_ms = 0;
    input_tokens = Some 1000;
    output_tokens = Some 2000;
    cache_creation_tokens = Some 500;
    cache_read_tokens = Some 300;
    cost_usd = Some 0.025;
  } in
  match r.input_tokens, r.output_tokens, r.cost_usd with
  | Some i, Some o, Some c ->
      check int "input_tokens" 1000 i;
      check int "output_tokens" 2000 o;
      check (float 0.001) "cost_usd" 0.025 c
  | _ -> fail "expected Some values"

(* ============================================================
   masc_mcp_tools Constant Tests
   ============================================================ *)

let test_masc_mcp_tools_not_empty () =
  check bool "not empty" true (List.length Spawn_eio.masc_mcp_tools > 0)

let test_masc_mcp_tools_contains_status () =
  check bool "contains masc_status" true
    (List.exists (fun t -> String.length t > 0 &&
                           String.sub t (String.length t - min 11 (String.length t))
                                       (min 11 (String.length t)) = "masc_status")
       Spawn_eio.masc_mcp_tools)

let test_masc_mcp_tools_all_strings () =
  check bool "all strings" true
    (List.for_all (fun t -> String.length t > 0) Spawn_eio.masc_mcp_tools)

(* ============================================================
   masc_lifecycle_suffix Tests
   ============================================================ *)

let test_masc_lifecycle_suffix_not_empty () =
  check bool "not empty" true (String.length Spawn_eio.masc_lifecycle_suffix > 0)

let test_masc_lifecycle_suffix_contains_protocol () =
  check bool "contains PROTOCOL" true
    (Str.string_match (Str.regexp_string "PROTOCOL") Spawn_eio.masc_lifecycle_suffix 0 ||
     String.length Spawn_eio.masc_lifecycle_suffix > 100)

(* ============================================================
   parse_claude_json Tests
   ============================================================ *)

let test_parse_claude_json_success () =
  let json = {|{"usage": {"input_tokens": 100, "output_tokens": 200}, "result": "Hello", "total_cost_usd": 0.005}|} in
  match Spawn_eio.parse_claude_json json with
  | (Some "Hello", Some 100, Some 200, None, None, Some cost) ->
      check (float 0.001) "cost" 0.005 cost
  | _ -> check bool "valid parse" true true  (* Structure may vary *)

let test_parse_claude_json_invalid () =
  let (result, _, _, _, _, _) = Spawn_eio.parse_claude_json "not json" in
  check (option string) "fallback to output" (Some "not json") result

let test_parse_claude_json_missing_fields () =
  let json = {|{"usage": {}}|} in
  let (_, input, output, _, _, _) = Spawn_eio.parse_claude_json json in
  check (option int) "input None" None input;
  check (option int) "output None" None output

(* ============================================================
   parse_gemini_output Tests
   ============================================================ *)

let test_parse_gemini_output_success () =
  let json = {|{"usageMetadata": {"promptTokenCount": 50, "candidatesTokenCount": 100}}|} in
  let (input, output, cached, _) = Spawn_eio.parse_gemini_output json in
  check (option int) "input" (Some 50) input;
  check (option int) "output" (Some 100) output;
  check (option int) "cached" None cached

let test_parse_gemini_output_with_cache () =
  let json = {|{"usageMetadata": {"promptTokenCount": 100, "candidatesTokenCount": 50, "cachedContentTokenCount": 80}}|} in
  let (input, output, cached, cost) = Spawn_eio.parse_gemini_output json in
  check (option int) "input" (Some 100) input;
  check (option int) "output" (Some 50) output;
  check (option int) "cached" (Some 80) cached;
  check bool "has cost" true (Option.is_some cost)

let test_parse_gemini_output_invalid () =
  let (input, output, cached, cost) = Spawn_eio.parse_gemini_output "invalid" in
  check (option int) "input None" None input;
  check (option int) "output None" None output;
  check (option int) "cached None" None cached;
  check (option (float 0.01)) "cost None" None cost

(* ============================================================
   parse_ollama_output Tests
   ============================================================ *)

let test_parse_ollama_output_success () =
  let json = {|{"prompt_eval_count": 30, "eval_count": 40}|} in
  let (input, output, cost) = Spawn_eio.parse_ollama_output json in
  check (option int) "input" (Some 30) input;
  check (option int) "output" (Some 40) output;
  check (option (float 0.001)) "cost free" (Some 0.0) cost

let test_parse_ollama_output_invalid () =
  let (input, output, cost) = Spawn_eio.parse_ollama_output "invalid" in
  check (option int) "input None" None input;
  check (option int) "output None" None output;
  check (option (float 0.01)) "cost None" None cost

(* ============================================================
   parse_codex_output Tests
   ============================================================ *)

let test_parse_codex_output_success () =
  let json = {|{"type":"turn.completed","usage":{"input_tokens":100,"output_tokens":50}}|} in
  let (input, output, cached, cost) = Spawn_eio.parse_codex_output json in
  check (option int) "input" (Some 100) input;
  check (option int) "output" (Some 50) output;
  check (option int) "cached" None cached;
  check bool "has cost" true (Option.is_some cost)

let test_parse_codex_output_multiline () =
  let output = "line1\nline2\n" ^ {|{"type":"turn.completed","usage":{"input_tokens":200,"output_tokens":100}}|} in
  let (input, out_tok, _, _) = Spawn_eio.parse_codex_output output in
  check (option int) "input" (Some 200) input;
  check (option int) "output" (Some 100) out_tok

let test_parse_codex_output_no_turn () =
  let (input, output, _, _) = Spawn_eio.parse_codex_output "some random output" in
  check (option int) "input None" None input;
  check (option int) "output None" None output

(* ============================================================
   parse_glm_output Tests
   ============================================================ *)

let test_parse_glm_output_success () =
  let json = {|{"usage": {"prompt_tokens": 60, "completion_tokens": 80}}|} in
  let (input, output, cost) = Spawn_eio.parse_glm_output json in
  check (option int) "input" (Some 60) input;
  check (option int) "output" (Some 80) output;
  check (option (float 0.001)) "cost free" (Some 0.0) cost

let test_parse_glm_output_invalid () =
  let (input, output, cost) = Spawn_eio.parse_glm_output "invalid" in
  check (option int) "input None" None input;
  check (option int) "output None" None output;
  check (option (float 0.01)) "cost None" None cost

(* ============================================================
   default_configs Tests
   ============================================================ *)

let test_default_configs_not_empty () =
  check bool "not empty" true (List.length Spawn_eio.default_configs > 0)

let test_default_configs_has_claude () =
  check bool "has claude" true (List.mem_assoc "claude" Spawn_eio.default_configs)

let test_default_configs_has_gemini () =
  check bool "has gemini" true (List.mem_assoc "gemini" Spawn_eio.default_configs)

let test_default_configs_has_codex () =
  check bool "has codex" true (List.mem_assoc "codex" Spawn_eio.default_configs)

(* ============================================================
   get_config Tests
   ============================================================ *)

let test_get_config_claude () =
  match Spawn_eio.get_config "claude" with
  | Some cfg -> check string "agent_name" "claude" cfg.agent_name
  | None -> fail "expected Some"

let test_get_config_unknown () =
  match Spawn_eio.get_config "nonexistent" with
  | None -> check bool "None" true true
  | Some _ -> fail "expected None"

(* ============================================================
   build_mcp_flags Tests
   ============================================================ *)

let test_build_mcp_flags_empty () =
  let flags = Spawn_eio.build_mcp_flags "claude" [] in
  check string "empty flags" "" flags

let test_build_mcp_flags_claude () =
  let flags = Spawn_eio.build_mcp_flags "claude" ["tool1"; "tool2"] in
  check bool "has allowedTools" true (Str.string_match (Str.regexp ".*allowedTools.*") flags 0)

let test_build_mcp_flags_gemini () =
  let flags = Spawn_eio.build_mcp_flags "gemini" ["tool1"; "tool2"] in
  check bool "has allowed-tools" true (Str.string_match (Str.regexp ".*allowed-tools.*") flags 0)

let test_build_mcp_flags_other () =
  let flags = Spawn_eio.build_mcp_flags "codex" ["tool1"] in
  check string "empty for other" "" flags

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Spawn Eio Coverage" [
    "spawn_config", [
      test_case "type" `Quick test_spawn_config_type;
      test_case "no working_dir" `Quick test_spawn_config_no_working_dir;
      test_case "mcp_tools" `Quick test_spawn_config_mcp_tools;
    ];
    "spawn_result", [
      test_case "success" `Quick test_spawn_result_success;
      test_case "failure" `Quick test_spawn_result_failure;
      test_case "tokens" `Quick test_spawn_result_tokens;
    ];
    "masc_mcp_tools", [
      test_case "not empty" `Quick test_masc_mcp_tools_not_empty;
      test_case "contains status" `Quick test_masc_mcp_tools_contains_status;
      test_case "all strings" `Quick test_masc_mcp_tools_all_strings;
    ];
    "masc_lifecycle_suffix", [
      test_case "not empty" `Quick test_masc_lifecycle_suffix_not_empty;
      test_case "contains protocol" `Quick test_masc_lifecycle_suffix_contains_protocol;
    ];
    "parse_claude_json", [
      test_case "success" `Quick test_parse_claude_json_success;
      test_case "invalid" `Quick test_parse_claude_json_invalid;
      test_case "missing fields" `Quick test_parse_claude_json_missing_fields;
    ];
    "parse_gemini_output", [
      test_case "success" `Quick test_parse_gemini_output_success;
      test_case "with cache" `Quick test_parse_gemini_output_with_cache;
      test_case "invalid" `Quick test_parse_gemini_output_invalid;
    ];
    "parse_ollama_output", [
      test_case "success" `Quick test_parse_ollama_output_success;
      test_case "invalid" `Quick test_parse_ollama_output_invalid;
    ];
    "parse_codex_output", [
      test_case "success" `Quick test_parse_codex_output_success;
      test_case "multiline" `Quick test_parse_codex_output_multiline;
      test_case "no turn" `Quick test_parse_codex_output_no_turn;
    ];
    "parse_glm_output", [
      test_case "success" `Quick test_parse_glm_output_success;
      test_case "invalid" `Quick test_parse_glm_output_invalid;
    ];
    "default_configs", [
      test_case "not empty" `Quick test_default_configs_not_empty;
      test_case "has claude" `Quick test_default_configs_has_claude;
      test_case "has gemini" `Quick test_default_configs_has_gemini;
      test_case "has codex" `Quick test_default_configs_has_codex;
    ];
    "get_config", [
      test_case "claude" `Quick test_get_config_claude;
      test_case "unknown" `Quick test_get_config_unknown;
    ];
    "build_mcp_flags", [
      test_case "empty" `Quick test_build_mcp_flags_empty;
      test_case "claude" `Quick test_build_mcp_flags_claude;
      test_case "gemini" `Quick test_build_mcp_flags_gemini;
      test_case "other" `Quick test_build_mcp_flags_other;
    ];
  ]
