(** Spawn Module Coverage Tests

    Tests for MASC Spawn - Agent subprocess management:
    - spawn_config type
    - spawn_result type
    - masc_mcp_tools list
    - masc_lifecycle_suffix
*)

open Alcotest

module Spawn = Masc_mcp.Spawn

(* ============================================================
   spawn_config Tests
   ============================================================ *)

let test_spawn_config_creation () =
  let cfg : Spawn.spawn_config = {
    agent_name = "test-agent";
    command = "claude -p";
    timeout_seconds = 60;
    working_dir = Some "/tmp";
    mcp_tools = ["tool1"; "tool2"];
  } in
  check string "agent_name" "test-agent" cfg.agent_name;
  check string "command" "claude -p" cfg.command;
  check int "timeout" 60 cfg.timeout_seconds

let test_spawn_config_no_working_dir () =
  let cfg : Spawn.spawn_config = {
    agent_name = "agent";
    command = "cmd";
    timeout_seconds = 30;
    working_dir = None;
    mcp_tools = [];
  } in
  match cfg.working_dir with
  | None -> check bool "no dir" true true
  | Some _ -> fail "expected None"

let test_spawn_config_empty_tools () =
  let cfg : Spawn.spawn_config = {
    agent_name = "a";
    command = "c";
    timeout_seconds = 1;
    working_dir = None;
    mcp_tools = [];
  } in
  check int "empty tools" 0 (List.length cfg.mcp_tools)

(* ============================================================
   spawn_result Tests
   ============================================================ *)

let test_spawn_result_success () =
  let res : Spawn.spawn_result = {
    success = true;
    output = "done";
    exit_code = 0;
    elapsed_ms = 100;
    input_tokens = Some 500;
    output_tokens = Some 200;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = Some 0.01;
  } in
  check bool "success" true res.success;
  check int "exit code" 0 res.exit_code

let test_spawn_result_failure () =
  let res : Spawn.spawn_result = {
    success = false;
    output = "error";
    exit_code = 1;
    elapsed_ms = 50;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  check bool "not success" false res.success;
  check int "exit code" 1 res.exit_code

let test_spawn_result_tokens () =
  let res : Spawn.spawn_result = {
    success = true;
    output = "ok";
    exit_code = 0;
    elapsed_ms = 200;
    input_tokens = Some 1000;
    output_tokens = Some 500;
    cache_creation_tokens = Some 100;
    cache_read_tokens = Some 200;
    cost_usd = Some 0.05;
  } in
  match res.input_tokens, res.output_tokens with
  | Some i, Some o ->
    check int "input tokens" 1000 i;
    check int "output tokens" 500 o
  | _ -> fail "expected Some tokens"

(* ============================================================
   masc_mcp_tools Tests
   ============================================================ *)

let test_masc_mcp_tools_not_empty () =
  check bool "not empty" true (List.length Spawn.masc_mcp_tools > 0)

let test_masc_mcp_tools_has_status () =
  check bool "has status" true (List.mem "mcp__masc__masc_status" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_join () =
  check bool "has join" true (List.mem "mcp__masc__masc_join" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_leave () =
  check bool "has leave" true (List.mem "mcp__masc__masc_leave" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_broadcast () =
  check bool "has broadcast" true (List.mem "mcp__masc__masc_broadcast" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_claim () =
  check bool "has claim" true (List.mem "mcp__masc__masc_claim" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_done () =
  check bool "has done" true (List.mem "mcp__masc__masc_done" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_heartbeat () =
  check bool "has heartbeat" true (List.mem "mcp__masc__masc_heartbeat" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_tasks () =
  check bool "has tasks" true (List.mem "mcp__masc__masc_tasks" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_worktree () =
  check bool "has worktree" true (List.mem "mcp__masc__masc_worktree_create" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_handover () =
  check bool "has handover" true (List.mem "mcp__masc__masc_handover_create" Spawn.masc_mcp_tools)

let test_masc_mcp_tools_has_memento () =
  check bool "has memento" true (List.mem "mcp__masc__masc_memento_mori" Spawn.masc_mcp_tools)

(* ============================================================
   masc_lifecycle_suffix Tests
   ============================================================ *)

let test_lifecycle_suffix_not_empty () =
  check bool "not empty" true (String.length Spawn.masc_lifecycle_suffix > 0)

let test_lifecycle_suffix_has_protocol () =
  check bool "has protocol" true
    (try let _ = Str.search_forward (Str.regexp_string "MASC LIFECYCLE") Spawn.masc_lifecycle_suffix 0 in true
     with Not_found -> false)

let test_lifecycle_suffix_has_join () =
  check bool "has join" true
    (try let _ = Str.search_forward (Str.regexp_string "masc_join") Spawn.masc_lifecycle_suffix 0 in true
     with Not_found -> false)

let test_lifecycle_suffix_has_heartbeat () =
  check bool "has heartbeat" true
    (try let _ = Str.search_forward (Str.regexp_string "heartbeat") Spawn.masc_lifecycle_suffix 0 in true
     with Not_found -> false)

let test_lifecycle_suffix_has_memento () =
  check bool "has memento" true
    (try let _ = Str.search_forward (Str.regexp_string "memento_mori") Spawn.masc_lifecycle_suffix 0 in true
     with Not_found -> false)

let test_lifecycle_suffix_has_context_ratio () =
  check bool "has context ratio" true
    (try let _ = Str.search_forward (Str.regexp_string "context_ratio") Spawn.masc_lifecycle_suffix 0 in true
     with Not_found -> false)

(* ============================================================
   default_configs Tests
   ============================================================ *)

let test_default_configs_not_empty () =
  check bool "not empty" true (List.length Spawn.default_configs > 0)

let test_default_configs_has_claude () =
  check bool "has claude" true (List.mem_assoc "claude" Spawn.default_configs)

let test_default_configs_has_gemini () =
  check bool "has gemini" true (List.mem_assoc "gemini" Spawn.default_configs)

let test_default_configs_has_codex () =
  check bool "has codex" true (List.mem_assoc "codex" Spawn.default_configs)

let test_default_configs_has_ollama () =
  check bool "has ollama" true (List.mem_assoc "ollama" Spawn.default_configs)

let test_default_configs_claude_command () =
  match List.assoc_opt "claude" Spawn.default_configs with
  | Some cfg -> check bool "has claude command" true
      (try let _ = Str.search_forward (Str.regexp "claude") cfg.command 0 in true
       with Not_found -> false)
  | None -> fail "no claude config"

let test_default_configs_gemini_command () =
  match List.assoc_opt "gemini" Spawn.default_configs with
  | Some cfg -> check bool "has gemini command" true
      (try let _ = Str.search_forward (Str.regexp "gemini") cfg.command 0 in true
       with Not_found -> false)
  | None -> fail "no gemini config"

(* ============================================================
   get_config Tests
   ============================================================ *)

let test_get_config_claude () =
  match Spawn.get_config "claude" with
  | Some cfg -> check string "agent name" "claude" cfg.agent_name
  | None -> fail "expected Some"

let test_get_config_gemini () =
  match Spawn.get_config "gemini" with
  | Some cfg -> check string "agent name" "gemini" cfg.agent_name
  | None -> fail "expected Some"

let test_get_config_codex () =
  match Spawn.get_config "codex" with
  | Some cfg -> check string "agent name" "codex" cfg.agent_name
  | None -> fail "expected Some"

let test_get_config_ollama () =
  match Spawn.get_config "ollama" with
  | Some cfg -> check string "agent name" "ollama" cfg.agent_name
  | None -> fail "expected Some"

let test_get_config_unknown () =
  match Spawn.get_config "unknown-agent" with
  | None -> check bool "returns none" true true
  | Some _ -> fail "expected None"

(* ============================================================
   build_mcp_flags Tests
   ============================================================ *)

let test_build_mcp_flags_empty () =
  let flags = Spawn.build_mcp_flags "claude" [] in
  check string "empty flags" "" flags

let test_build_mcp_flags_claude () =
  let flags = Spawn.build_mcp_flags "claude" ["tool1"; "tool2"] in
  check bool "has allowedTools" true
    (try let _ = Str.search_forward (Str.regexp "allowedTools") flags 0 in true
     with Not_found -> false)

let test_build_mcp_flags_gemini () =
  let flags = Spawn.build_mcp_flags "gemini" ["tool1"] in
  check bool "has allowed-mcp-server-names" true
    (try let _ = Str.search_forward (Str.regexp "allowed-mcp-server-names") flags 0 in true
     with Not_found -> false)

let test_build_mcp_flags_gemini_allowed_tools () =
  let flags = Spawn.build_mcp_flags "gemini" ["tool1"] in
  check bool "has allowed-tools" true
    (try let _ = Str.search_forward (Str.regexp "allowed-tools") flags 0 in true
     with Not_found -> false)

let test_build_mcp_flags_codex () =
  let flags = Spawn.build_mcp_flags "codex" ["tool1"; "tool2"] in
  check string "codex empty" "" flags

let test_build_mcp_flags_ollama () =
  let flags = Spawn.build_mcp_flags "ollama" ["tool1"] in
  check string "ollama empty" "" flags

let test_build_mcp_flags_unknown () =
  let flags = Spawn.build_mcp_flags "unknown" ["tool1"] in
  check string "unknown empty" "" flags

(* ============================================================
   parse_claude_json Tests
   ============================================================ *)

let test_parse_claude_json_valid () =
  let json_str = {|{"usage":{"input_tokens":100,"output_tokens":50},"total_cost_usd":0.01,"result":"done"}|} in
  let (result_text, input, output, cache_c, cache_r, _cost) = Spawn.parse_claude_json json_str in
  check (option string) "result text" (Some "done") result_text;
  check (option int) "input tokens" (Some 100) input;
  check (option int) "output tokens" (Some 50) output;
  check (option int) "cache creation" None cache_c;
  check (option int) "cache read" None cache_r

let test_parse_claude_json_with_cache () =
  let json_str = {|{"usage":{"input_tokens":100,"output_tokens":50,"cache_creation_input_tokens":20,"cache_read_input_tokens":30},"total_cost_usd":0.02,"result":"ok"}|} in
  let (_result, _input, _output, cache_c, cache_r, _cost) = Spawn.parse_claude_json json_str in
  check (option int) "cache creation" (Some 20) cache_c;
  check (option int) "cache read" (Some 30) cache_r

let test_parse_claude_json_invalid () =
  let json_str = "not valid json" in
  let (result_text, input, output, cache_c, cache_r, cost) = Spawn.parse_claude_json json_str in
  check (option string) "returns raw" (Some "not valid json") result_text;
  check (option int) "no input" None input;
  check (option int) "no output" None output;
  check (option int) "no cache c" None cache_c;
  check (option int) "no cache r" None cache_r;
  check bool "no cost" true (cost = None)

let test_parse_claude_json_missing_fields () =
  let json_str = {|{"usage":{}}|} in
  let (_result, input, output, _cache_c, _cache_r, _cost) = Spawn.parse_claude_json json_str in
  check (option int) "no input" None input;
  check (option int) "no output" None output

(* ============================================================
   int_opt_to_json / float_opt_to_json Tests
   ============================================================ *)

let test_int_opt_to_json_some () =
  let json = Spawn.int_opt_to_json (Some 42) in
  check string "int 42" "42" (Yojson.Safe.to_string json)

let test_int_opt_to_json_none () =
  let json = Spawn.int_opt_to_json None in
  check string "null" "null" (Yojson.Safe.to_string json)

let test_float_opt_to_json_some () =
  let json = Spawn.float_opt_to_json (Some 3.14) in
  let str = Yojson.Safe.to_string json in
  check bool "has 3.14" true
    (try let _ = Str.search_forward (Str.regexp "3.14") str 0 in true
     with Not_found -> false)

let test_float_opt_to_json_none () =
  let json = Spawn.float_opt_to_json None in
  check string "null" "null" (Yojson.Safe.to_string json)

(* ============================================================
   result_to_json Tests
   ============================================================ *)

let test_result_to_json_success () =
  let result : Spawn.spawn_result = {
    success = true;
    output = "test output";
    exit_code = 0;
    elapsed_ms = 100;
    input_tokens = Some 500;
    output_tokens = Some 200;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = Some 0.01;
  } in
  let json = Spawn.result_to_json result in
  let str = Yojson.Safe.to_string json in
  check bool "has success" true
    (try let _ = Str.search_forward (Str.regexp "\"success\":true") str 0 in true
     with Not_found -> false);
  check bool "has output" true
    (try let _ = Str.search_forward (Str.regexp "test output") str 0 in true
     with Not_found -> false)

let test_result_to_json_failure () =
  let result : Spawn.spawn_result = {
    success = false;
    output = "error";
    exit_code = 1;
    elapsed_ms = 50;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  let json = Spawn.result_to_json result in
  let str = Yojson.Safe.to_string json in
  check bool "has success false" true
    (try let _ = Str.search_forward (Str.regexp "\"success\":false") str 0 in true
     with Not_found -> false)

let test_result_to_json_with_cache () =
  let result : Spawn.spawn_result = {
    success = true;
    output = "ok";
    exit_code = 0;
    elapsed_ms = 200;
    input_tokens = Some 1000;
    output_tokens = Some 500;
    cache_creation_tokens = Some 100;
    cache_read_tokens = Some 200;
    cost_usd = Some 0.05;
  } in
  let json = Spawn.result_to_json result in
  let str = Yojson.Safe.to_string json in
  check bool "has cache_creation_tokens" true
    (try let _ = Str.search_forward (Str.regexp "cache_creation_tokens") str 0 in true
     with Not_found -> false)

(* ============================================================
   format_token_info Tests
   ============================================================ *)

let test_format_token_info_with_tokens () =
  let result : Spawn.spawn_result = {
    success = true;
    output = "";
    exit_code = 0;
    elapsed_ms = 100;
    input_tokens = Some 500;
    output_tokens = Some 200;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = Some 0.01;
  } in
  let info = Spawn.format_token_info result in
  check bool "has tokens" true
    (try let _ = Str.search_forward (Str.regexp "Tokens") info 0 in true
     with Not_found -> false);
  check bool "has cost" true
    (try let _ = Str.search_forward (Str.regexp "Cost") info 0 in true
     with Not_found -> false)

let test_format_token_info_with_cache () =
  let result : Spawn.spawn_result = {
    success = true;
    output = "";
    exit_code = 0;
    elapsed_ms = 100;
    input_tokens = Some 500;
    output_tokens = Some 200;
    cache_creation_tokens = Some 50;
    cache_read_tokens = Some 100;
    cost_usd = Some 0.02;
  } in
  let info = Spawn.format_token_info result in
  check bool "has cache" true
    (try let _ = Str.search_forward (Str.regexp "cache") info 0 in true
     with Not_found -> false)

let test_format_token_info_no_tokens () =
  let result : Spawn.spawn_result = {
    success = true;
    output = "";
    exit_code = 0;
    elapsed_ms = 100;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  let info = Spawn.format_token_info result in
  check string "empty" "" info

(* ============================================================
   result_to_string Tests
   ============================================================ *)

let test_result_to_string_success () =
  let result : Spawn.spawn_result = {
    success = true;
    output = "completed";
    exit_code = 0;
    elapsed_ms = 100;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  let str = Spawn.result_to_string result in
  check bool "has checkmark" true
    (try let _ = Str.search_forward (Str.regexp "✅") str 0 in true
     with Not_found -> false)

let test_result_to_string_failure () =
  let result : Spawn.spawn_result = {
    success = false;
    output = "failed";
    exit_code = 1;
    elapsed_ms = 50;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  let str = Spawn.result_to_string result in
  check bool "has x" true
    (try let _ = Str.search_forward (Str.regexp "❌") str 0 in true
     with Not_found -> false)

let test_result_to_string_has_elapsed () =
  let result : Spawn.spawn_result = {
    success = true;
    output = "";
    exit_code = 0;
    elapsed_ms = 12345;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  let str = Spawn.result_to_string result in
  check bool "has elapsed" true
    (try let _ = Str.search_forward (Str.regexp "12345") str 0 in true
     with Not_found -> false)

let test_result_to_string_has_output () =
  let result : Spawn.spawn_result = {
    success = true;
    output = "unique_output_xyz";
    exit_code = 0;
    elapsed_ms = 100;
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = None;
  } in
  let str = Spawn.result_to_string result in
  check bool "has output" true
    (try let _ = Str.search_forward (Str.regexp "unique_output_xyz") str 0 in true
     with Not_found -> false)

(* ============================================================
   spawn_sync alias Tests
   ============================================================ *)

let test_spawn_sync_is_spawn () =
  (* Just verify spawn_sync is the same function as spawn (alias test) *)
  let _ : (agent_name:string -> prompt:string -> ?timeout_seconds:int -> ?working_dir:string -> unit -> Spawn.spawn_result) = Spawn.spawn_sync in
  check bool "spawn_sync exists" true true

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Spawn Coverage" [
    "spawn_config", [
      test_case "creation" `Quick test_spawn_config_creation;
      test_case "no working dir" `Quick test_spawn_config_no_working_dir;
      test_case "empty tools" `Quick test_spawn_config_empty_tools;
    ];
    "spawn_result", [
      test_case "success" `Quick test_spawn_result_success;
      test_case "failure" `Quick test_spawn_result_failure;
      test_case "tokens" `Quick test_spawn_result_tokens;
    ];
    "masc_mcp_tools", [
      test_case "not empty" `Quick test_masc_mcp_tools_not_empty;
      test_case "has status" `Quick test_masc_mcp_tools_has_status;
      test_case "has join" `Quick test_masc_mcp_tools_has_join;
      test_case "has leave" `Quick test_masc_mcp_tools_has_leave;
      test_case "has broadcast" `Quick test_masc_mcp_tools_has_broadcast;
      test_case "has claim" `Quick test_masc_mcp_tools_has_claim;
      test_case "has done" `Quick test_masc_mcp_tools_has_done;
      test_case "has heartbeat" `Quick test_masc_mcp_tools_has_heartbeat;
      test_case "has tasks" `Quick test_masc_mcp_tools_has_tasks;
      test_case "has worktree" `Quick test_masc_mcp_tools_has_worktree;
      test_case "has handover" `Quick test_masc_mcp_tools_has_handover;
      test_case "has memento" `Quick test_masc_mcp_tools_has_memento;
    ];
    "lifecycle_suffix", [
      test_case "not empty" `Quick test_lifecycle_suffix_not_empty;
      test_case "has protocol" `Quick test_lifecycle_suffix_has_protocol;
      test_case "has join" `Quick test_lifecycle_suffix_has_join;
      test_case "has heartbeat" `Quick test_lifecycle_suffix_has_heartbeat;
      test_case "has memento" `Quick test_lifecycle_suffix_has_memento;
      test_case "has context ratio" `Quick test_lifecycle_suffix_has_context_ratio;
    ];
    "default_configs", [
      test_case "not empty" `Quick test_default_configs_not_empty;
      test_case "has claude" `Quick test_default_configs_has_claude;
      test_case "has gemini" `Quick test_default_configs_has_gemini;
      test_case "has codex" `Quick test_default_configs_has_codex;
      test_case "has ollama" `Quick test_default_configs_has_ollama;
      test_case "claude command" `Quick test_default_configs_claude_command;
      test_case "gemini command" `Quick test_default_configs_gemini_command;
    ];
    "get_config", [
      test_case "claude" `Quick test_get_config_claude;
      test_case "gemini" `Quick test_get_config_gemini;
      test_case "codex" `Quick test_get_config_codex;
      test_case "ollama" `Quick test_get_config_ollama;
      test_case "unknown" `Quick test_get_config_unknown;
    ];
    "build_mcp_flags", [
      test_case "empty" `Quick test_build_mcp_flags_empty;
      test_case "claude" `Quick test_build_mcp_flags_claude;
      test_case "gemini" `Quick test_build_mcp_flags_gemini;
      test_case "gemini allowed tools" `Quick test_build_mcp_flags_gemini_allowed_tools;
      test_case "codex" `Quick test_build_mcp_flags_codex;
      test_case "ollama" `Quick test_build_mcp_flags_ollama;
      test_case "unknown" `Quick test_build_mcp_flags_unknown;
    ];
    "parse_claude_json", [
      test_case "valid" `Quick test_parse_claude_json_valid;
      test_case "with cache" `Quick test_parse_claude_json_with_cache;
      test_case "invalid" `Quick test_parse_claude_json_invalid;
      test_case "missing fields" `Quick test_parse_claude_json_missing_fields;
    ];
    "int_opt_to_json", [
      test_case "some" `Quick test_int_opt_to_json_some;
      test_case "none" `Quick test_int_opt_to_json_none;
    ];
    "float_opt_to_json", [
      test_case "some" `Quick test_float_opt_to_json_some;
      test_case "none" `Quick test_float_opt_to_json_none;
    ];
    "result_to_json", [
      test_case "success" `Quick test_result_to_json_success;
      test_case "failure" `Quick test_result_to_json_failure;
      test_case "with cache" `Quick test_result_to_json_with_cache;
    ];
    "format_token_info", [
      test_case "with tokens" `Quick test_format_token_info_with_tokens;
      test_case "with cache" `Quick test_format_token_info_with_cache;
      test_case "no tokens" `Quick test_format_token_info_no_tokens;
    ];
    "result_to_string", [
      test_case "success" `Quick test_result_to_string_success;
      test_case "failure" `Quick test_result_to_string_failure;
      test_case "has elapsed" `Quick test_result_to_string_has_elapsed;
      test_case "has output" `Quick test_result_to_string_has_output;
    ];
    "spawn_sync", [
      test_case "alias" `Quick test_spawn_sync_is_spawn;
    ];
  ]
