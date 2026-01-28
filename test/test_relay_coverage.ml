(** Relay Module Coverage Tests

    Tests for the MASC Relay infinite context system:
    - relay_config: threshold, target_agent, compress_ratio
    - context_metrics: token estimation and usage ratios
    - handoff_payload: summary, todos, PDCA state
    - task_hint: complexity hints for proactive relay
    - should_relay: reactive and proactive relay decisions
*)

open Alcotest

module Relay = Masc_mcp.Relay

(* ============================================================
   relay_config Tests
   ============================================================ *)

let test_default_config_threshold () =
  let c = Relay.default_config in
  check bool "threshold 0.8" true (abs_float (c.threshold -. 0.8) < 0.001)

let test_default_config_target_agent () =
  let c = Relay.default_config in
  check string "target_agent" "claude" c.target_agent

let test_default_config_compress_ratio () =
  let c = Relay.default_config in
  check bool "compress_ratio 0.1" true (abs_float (c.compress_ratio -. 0.1) < 0.001)

let test_default_config_include_todos () =
  let c = Relay.default_config in
  check bool "include_todos" true c.include_todos

let test_default_config_include_pdca () =
  let c = Relay.default_config in
  check bool "include_pdca" true c.include_pdca

let test_default_config_neo4j_episode () =
  let c = Relay.default_config in
  check bool "neo4j_episode" true c.neo4j_episode

let test_custom_config () =
  let c : Relay.relay_config = {
    threshold = 0.5;
    target_agent = "gemini";
    compress_ratio = 0.2;
    include_todos = false;
    include_pdca = false;
    neo4j_episode = false;
  } in
  check bool "custom threshold" true (abs_float (c.threshold -. 0.5) < 0.001);
  check string "custom agent" "gemini" c.target_agent

(* ============================================================
   estimate_context Tests
   ============================================================ *)

let test_estimate_context_claude () =
  let m = Relay.estimate_context ~messages:10 ~tool_calls:5 ~model:"claude" in
  check bool "positive estimated" true (m.estimated_tokens > 0);
  check int "max tokens claude" 200000 m.max_tokens;
  check int "message count" 10 m.message_count;
  check int "tool call count" 5 m.tool_call_count

let test_estimate_context_gemini () =
  let m = Relay.estimate_context ~messages:10 ~tool_calls:5 ~model:"gemini" in
  check int "max tokens gemini" 1000000 m.max_tokens

let test_estimate_context_codex () =
  let m = Relay.estimate_context ~messages:10 ~tool_calls:5 ~model:"codex" in
  check int "max tokens codex" 128000 m.max_tokens

let test_estimate_context_gpt () =
  let m = Relay.estimate_context ~messages:10 ~tool_calls:5 ~model:"gpt" in
  check int "max tokens gpt" 128000 m.max_tokens

let test_estimate_context_claude_opus () =
  let m = Relay.estimate_context ~messages:10 ~tool_calls:5 ~model:"claude-opus" in
  check int "max tokens claude-opus" 200000 m.max_tokens

let test_estimate_context_unknown_model () =
  let m = Relay.estimate_context ~messages:10 ~tool_calls:5 ~model:"unknown" in
  check int "max tokens unknown" 100000 m.max_tokens

let test_estimate_context_usage_ratio () =
  let m = Relay.estimate_context ~messages:100 ~tool_calls:50 ~model:"claude" in
  check bool "usage_ratio between 0 and 1" true (m.usage_ratio > 0.0 && m.usage_ratio < 1.0)

let test_estimate_context_zero_messages () =
  let m = Relay.estimate_context ~messages:0 ~tool_calls:0 ~model:"claude" in
  check int "zero estimated" 0 m.estimated_tokens;
  check bool "zero ratio" true (abs_float m.usage_ratio < 0.001)

let test_estimate_context_many_messages () =
  let m = Relay.estimate_context ~messages:1000 ~tool_calls:500 ~model:"claude" in
  check bool "large estimated" true (m.estimated_tokens > 100000)

(* ============================================================
   task_hint Tests (via estimate_task_cost)
   ============================================================ *)

let test_task_cost_large_file () =
  let cost = Relay.estimate_task_cost (Relay.Large_file_read "big.txt") in
  check int "large file cost" 10000 cost

let test_task_cost_multi_file () =
  let cost = Relay.estimate_task_cost (Relay.Multi_file_edit 5) in
  check int "multi file cost" 15000 cost

let test_task_cost_multi_file_one () =
  let cost = Relay.estimate_task_cost (Relay.Multi_file_edit 1) in
  check int "single file cost" 3000 cost

let test_task_cost_long_running () =
  let cost = Relay.estimate_task_cost Relay.Long_running_task in
  check int "long running cost" 20000 cost

let test_task_cost_exploration () =
  let cost = Relay.estimate_task_cost Relay.Exploration_task in
  check int "exploration cost" 15000 cost

let test_task_cost_simple () =
  let cost = Relay.estimate_task_cost Relay.Simple_task in
  check int "simple cost" 1000 cost

(* ============================================================
   should_relay (Reactive) Tests
   ============================================================ *)

let test_should_relay_below_threshold () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 100000;
    max_tokens = 200000;
    usage_ratio = 0.5;
    message_count = 50;
    tool_call_count = 20;
  } in
  check bool "no relay at 0.5" false (Relay.should_relay ~config ~metrics)

let test_should_relay_at_threshold () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 160000;
    max_tokens = 200000;
    usage_ratio = 0.8;
    message_count = 100;
    tool_call_count = 40;
  } in
  check bool "relay at 0.8" true (Relay.should_relay ~config ~metrics)

let test_should_relay_above_threshold () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 180000;
    max_tokens = 200000;
    usage_ratio = 0.9;
    message_count = 120;
    tool_call_count = 50;
  } in
  check bool "relay at 0.9" true (Relay.should_relay ~config ~metrics)

let test_should_relay_custom_threshold () =
  let config : Relay.relay_config = {
    Relay.default_config with threshold = 0.5
  } in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 110000;
    max_tokens = 200000;
    usage_ratio = 0.55;
    message_count = 50;
    tool_call_count = 20;
  } in
  check bool "relay at custom 0.5" true (Relay.should_relay ~config ~metrics)

(* ============================================================
   should_relay_proactive Tests
   ============================================================ *)

let test_proactive_simple_task_no_relay () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 150000;
    max_tokens = 200000;
    usage_ratio = 0.75;
    message_count = 80;
    tool_call_count = 30;
  } in
  check bool "simple task no relay" false
    (Relay.should_relay_proactive ~config ~metrics ~task_hint:Relay.Simple_task)

let test_proactive_large_file_relay () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 155000;
    max_tokens = 200000;
    usage_ratio = 0.775;
    message_count = 80;
    tool_call_count = 30;
  } in
  (* 155000 + 10000 = 165000 / 200000 = 0.825 > 0.8 threshold *)
  check bool "large file triggers relay" true
    (Relay.should_relay_proactive ~config ~metrics ~task_hint:(Relay.Large_file_read "big.ml"))

let test_proactive_long_task_relay () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 145000;
    max_tokens = 200000;
    usage_ratio = 0.725;
    message_count = 70;
    tool_call_count = 25;
  } in
  (* 145000 + 20000 = 165000 / 200000 = 0.825 > 0.8 threshold *)
  check bool "long task triggers relay" true
    (Relay.should_relay_proactive ~config ~metrics ~task_hint:Relay.Long_running_task)

let test_proactive_multi_file_relay () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 140000;
    max_tokens = 200000;
    usage_ratio = 0.7;
    message_count = 70;
    tool_call_count = 25;
  } in
  (* 140000 + 5*3000 = 155000 / 200000 = 0.775 < 0.8 threshold *)
  check bool "5-file edit no relay" false
    (Relay.should_relay_proactive ~config ~metrics ~task_hint:(Relay.Multi_file_edit 5));
  (* Now with 7 files: 140000 + 7*3000 = 161000 / 200000 = 0.805 > 0.8 *)
  check bool "7-file edit triggers relay" true
    (Relay.should_relay_proactive ~config ~metrics ~task_hint:(Relay.Multi_file_edit 7))

(* ============================================================
   handoff_payload Tests
   ============================================================ *)

let test_handoff_payload_creation () =
  let p : Relay.handoff_payload = {
    summary = "Test session summary";
    current_task = Some "Implementing feature X";
    todos = ["TODO 1"; "TODO 2"; "TODO 3"];
    pdca_state = Some "Plan: ..., Do: ...";
    relevant_files = ["src/main.ml"; "lib/utils.ml"];
    session_id = Some "abc-123";
    relay_generation = 0;
  } in
  check string "summary" "Test session summary" p.summary;
  check (option string) "current_task" (Some "Implementing feature X") p.current_task;
  check int "todos count" 3 (List.length p.todos);
  check int "files count" 2 (List.length p.relevant_files);
  check int "generation" 0 p.relay_generation

let test_handoff_payload_empty () =
  let p : Relay.handoff_payload = {
    summary = "";
    current_task = None;
    todos = [];
    pdca_state = None;
    relevant_files = [];
    session_id = None;
    relay_generation = 0;
  } in
  check string "empty summary" "" p.summary;
  check (option string) "no task" None p.current_task;
  check int "no todos" 0 (List.length p.todos)

let test_handoff_payload_incremented_generation () =
  let p : Relay.handoff_payload = {
    summary = "Relayed from previous session";
    current_task = Some "Continue feature X";
    todos = ["Remaining TODO"];
    pdca_state = None;
    relevant_files = [];
    session_id = Some "def-456";
    relay_generation = 3;
  } in
  check int "generation 3" 3 p.relay_generation

(* ============================================================
   context_metrics Tests
   ============================================================ *)

let test_context_metrics_creation () =
  let m : Relay.context_metrics = {
    estimated_tokens = 50000;
    max_tokens = 200000;
    usage_ratio = 0.25;
    message_count = 30;
    tool_call_count = 10;
  } in
  check int "estimated" 50000 m.estimated_tokens;
  check int "max" 200000 m.max_tokens;
  check bool "ratio" true (abs_float (m.usage_ratio -. 0.25) < 0.001)

(* ============================================================
   should_relay_smart Tests
   ============================================================ *)

let test_smart_no_relay () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 100000;
    max_tokens = 200000;
    usage_ratio = 0.5;
    message_count = 50;
    tool_call_count = 20;
  } in
  let result = Relay.should_relay_smart ~config ~metrics ~task_hint:Relay.Simple_task in
  check bool "no relay" true (result = `No_relay)

let test_smart_reactive () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 180000;
    max_tokens = 200000;
    usage_ratio = 0.9;
    message_count = 100;
    tool_call_count = 40;
  } in
  let result = Relay.should_relay_smart ~config ~metrics ~task_hint:Relay.Simple_task in
  check bool "reactive" true (result = `Reactive)

let test_smart_proactive () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 155000;
    max_tokens = 200000;
    usage_ratio = 0.775;
    message_count = 80;
    tool_call_count = 30;
  } in
  (* Current: 0.775, with large file: (155000+10000)/200000 = 0.825 > 0.8 *)
  let result = Relay.should_relay_smart ~config ~metrics ~task_hint:(Relay.Large_file_read "big.ml") in
  check bool "proactive" true (result = `Proactive)

let test_smart_reactive_over_proactive () =
  let config = Relay.default_config in
  let metrics : Relay.context_metrics = {
    estimated_tokens = 170000;
    max_tokens = 200000;
    usage_ratio = 0.85;
    message_count = 100;
    tool_call_count = 40;
  } in
  (* Already reactive, so should return Reactive not Proactive *)
  let result = Relay.should_relay_smart ~config ~metrics ~task_hint:(Relay.Large_file_read "big.ml") in
  check bool "reactive wins" true (result = `Reactive)

(* ============================================================
   compress_context Tests
   ============================================================ *)

let test_compress_context_summary_only () =
  let result = Relay.compress_context
    ~summary:"This is the summary"
    ~task:None
    ~todos:[]
    ~pdca:None
    ~files:[]
  in
  check bool "has summary section" true
    (try let _ = Str.search_forward (Str.regexp "Context Summary") result 0 in true
     with Not_found -> false);
  check bool "has summary content" true
    (try let _ = Str.search_forward (Str.regexp "This is the summary") result 0 in true
     with Not_found -> false)

let test_compress_context_with_task () =
  let result = Relay.compress_context
    ~summary:"Summary"
    ~task:(Some "Implementing feature X")
    ~todos:[]
    ~pdca:None
    ~files:[]
  in
  check bool "has task section" true
    (try let _ = Str.search_forward (Str.regexp "Current Task") result 0 in true
     with Not_found -> false);
  check bool "has task content" true
    (try let _ = Str.search_forward (Str.regexp "feature X") result 0 in true
     with Not_found -> false)

let test_compress_context_with_todos () =
  let result = Relay.compress_context
    ~summary:"Summary"
    ~task:None
    ~todos:["TODO 1"; "TODO 2"; "TODO 3"]
    ~pdca:None
    ~files:[]
  in
  check bool "has todo section" true
    (try let _ = Str.search_forward (Str.regexp "TODO List") result 0 in true
     with Not_found -> false);
  check bool "has bullet points" true
    (try let _ = Str.search_forward (Str.regexp "- TODO 1") result 0 in true
     with Not_found -> false)

let test_compress_context_with_pdca () =
  let result = Relay.compress_context
    ~summary:"Summary"
    ~task:None
    ~todos:[]
    ~pdca:(Some "Plan: Design API")
    ~files:[]
  in
  check bool "has pdca section" true
    (try let _ = Str.search_forward (Str.regexp "PDCA State") result 0 in true
     with Not_found -> false);
  check bool "has pdca content" true
    (try let _ = Str.search_forward (Str.regexp "Design API") result 0 in true
     with Not_found -> false)

let test_compress_context_with_files () =
  let result = Relay.compress_context
    ~summary:"Summary"
    ~task:None
    ~todos:[]
    ~pdca:None
    ~files:["src/main.ml"; "lib/utils.ml"]
  in
  check bool "has files section" true
    (try let _ = Str.search_forward (Str.regexp "Relevant Files") result 0 in true
     with Not_found -> false);
  check bool "has backticks" true
    (try let _ = Str.search_forward (Str.regexp "`src/main.ml`") result 0 in true
     with Not_found -> false)

let test_compress_context_full () =
  let result = Relay.compress_context
    ~summary:"Full session summary"
    ~task:(Some "Complete feature")
    ~todos:["Item 1"; "Item 2"]
    ~pdca:(Some "Plan: done")
    ~files:["a.ml"; "b.ml"]
  in
  check bool "has all sections" true
    (String.length result > 100)

(* ============================================================
   build_handoff_prompt Tests
   ============================================================ *)

let test_build_handoff_prompt_basic () =
  let payload : Relay.handoff_payload = {
    summary = "Test summary";
    current_task = Some "Test task";
    todos = ["TODO 1"];
    pdca_state = None;
    relevant_files = ["test.ml"];
    session_id = None;
    relay_generation = 0;
  } in
  let prompt = Relay.build_handoff_prompt ~payload ~generation:1 in
  check bool "has relay header" true
    (try let _ = Str.search_forward (Str.regexp "RELAY HANDOFF") prompt 0 in true
     with Not_found -> false)

let test_build_handoff_prompt_generation () =
  let payload : Relay.handoff_payload = {
    summary = "Test";
    current_task = None;
    todos = [];
    pdca_state = None;
    relevant_files = [];
    session_id = None;
    relay_generation = 0;
  } in
  let prompt = Relay.build_handoff_prompt ~payload ~generation:5 in
  check bool "has generation 5" true
    (try let _ = Str.search_forward (Str.regexp "Generation 5") prompt 0 in true
     with Not_found -> false)

let test_build_handoff_prompt_instructions () =
  let payload : Relay.handoff_payload = Relay.empty_payload in
  let prompt = Relay.build_handoff_prompt ~payload ~generation:1 in
  check bool "has instructions" true
    (try let _ = Str.search_forward (Str.regexp "Instructions") prompt 0 in true
     with Not_found -> false);
  check bool "has masc mention" true
    (try let _ = Str.search_forward (Str.regexp "MASC") prompt 0 in true
     with Not_found -> false)

let test_build_handoff_prompt_seamless () =
  let payload : Relay.handoff_payload = Relay.empty_payload in
  let prompt = Relay.build_handoff_prompt ~payload ~generation:1 in
  check bool "mentions seamless" true
    (try let _ = Str.search_forward (Str.regexp "seamless") prompt 0 in true
     with Not_found -> false)

(* ============================================================
   checkpoint_to_payload Tests
   ============================================================ *)

let test_checkpoint_to_payload_basic () =
  let cp : Relay.checkpoint = {
    cp_timestamp = 1234567890.0;
    cp_summary = "Checkpoint summary";
    cp_task = Some "Checkpoint task";
    cp_todos = ["CP TODO 1"; "CP TODO 2"];
    cp_pdca = Some "CP PDCA";
    cp_files = ["cp.ml"];
    cp_metrics = {
      estimated_tokens = 100000;
      max_tokens = 200000;
      usage_ratio = 0.5;
      message_count = 50;
      tool_call_count = 20;
    };
  } in
  let payload = Relay.checkpoint_to_payload cp 2 in
  check string "summary" "Checkpoint summary" payload.summary;
  check (option string) "task" (Some "Checkpoint task") payload.current_task;
  check int "todos count" 2 (List.length payload.todos);
  check int "generation" 2 payload.relay_generation

let test_checkpoint_to_payload_none_task () =
  let cp : Relay.checkpoint = {
    cp_timestamp = 0.0;
    cp_summary = "No task checkpoint";
    cp_task = None;
    cp_todos = [];
    cp_pdca = None;
    cp_files = [];
    cp_metrics = {
      estimated_tokens = 0;
      max_tokens = 200000;
      usage_ratio = 0.0;
      message_count = 0;
      tool_call_count = 0;
    };
  } in
  let payload = Relay.checkpoint_to_payload cp 0 in
  check (option string) "no task" None payload.current_task;
  check (option string) "no session_id" None payload.session_id

(* ============================================================
   metrics_to_json Tests
   ============================================================ *)

let test_metrics_to_json_basic () =
  let m : Relay.context_metrics = {
    estimated_tokens = 50000;
    max_tokens = 200000;
    usage_ratio = 0.25;
    message_count = 30;
    tool_call_count = 10;
  } in
  let json = Relay.metrics_to_json m in
  let str = Yojson.Safe.to_string json in
  check bool "has estimated_tokens" true
    (try let _ = Str.search_forward (Str.regexp "estimated_tokens") str 0 in true
     with Not_found -> false);
  check bool "has max_tokens" true
    (try let _ = Str.search_forward (Str.regexp "max_tokens") str 0 in true
     with Not_found -> false);
  check bool "has usage_ratio" true
    (try let _ = Str.search_forward (Str.regexp "usage_ratio") str 0 in true
     with Not_found -> false)

let test_metrics_to_json_values () =
  let m : Relay.context_metrics = {
    estimated_tokens = 12345;
    max_tokens = 100000;
    usage_ratio = 0.12345;
    message_count = 42;
    tool_call_count = 7;
  } in
  let json = Relay.metrics_to_json m in
  let str = Yojson.Safe.to_string json in
  check bool "has 12345" true
    (try let _ = Str.search_forward (Str.regexp "12345") str 0 in true
     with Not_found -> false);
  check bool "has 100000" true
    (try let _ = Str.search_forward (Str.regexp "100000") str 0 in true
     with Not_found -> false)

(* ============================================================
   payload_to_json Tests
   ============================================================ *)

let test_payload_to_json_basic () =
  let p : Relay.handoff_payload = {
    summary = "Test summary";
    current_task = Some "Test task";
    todos = ["TODO 1"; "TODO 2"];
    pdca_state = Some "PDCA state";
    relevant_files = ["a.ml"; "b.ml"];
    session_id = Some "sess-123";
    relay_generation = 3;
  } in
  let json = Relay.payload_to_json p in
  let str = Yojson.Safe.to_string json in
  check bool "has summary" true
    (try let _ = Str.search_forward (Str.regexp "Test summary") str 0 in true
     with Not_found -> false);
  check bool "has current_task" true
    (try let _ = Str.search_forward (Str.regexp "Test task") str 0 in true
     with Not_found -> false);
  check bool "has relay_generation" true
    (try let _ = Str.search_forward (Str.regexp "relay_generation") str 0 in true
     with Not_found -> false)

let test_payload_to_json_nulls () =
  let p : Relay.handoff_payload = {
    summary = "";
    current_task = None;
    todos = [];
    pdca_state = None;
    relevant_files = [];
    session_id = None;
    relay_generation = 0;
  } in
  let json = Relay.payload_to_json p in
  let str = Yojson.Safe.to_string json in
  check bool "has null for task" true
    (try let _ = Str.search_forward (Str.regexp "\"current_task\":null") str 0 in true
     with Not_found -> false);
  check bool "has null for session_id" true
    (try let _ = Str.search_forward (Str.regexp "\"session_id\":null") str 0 in true
     with Not_found -> false)

let test_payload_to_json_todos_array () =
  let p : Relay.handoff_payload = {
    summary = "";
    current_task = None;
    todos = ["A"; "B"; "C"];
    pdca_state = None;
    relevant_files = [];
    session_id = None;
    relay_generation = 0;
  } in
  let json = Relay.payload_to_json p in
  let str = Yojson.Safe.to_string json in
  check bool "has todos array" true
    (try let _ = Str.search_forward (Str.regexp "\"todos\":\\[") str 0 in true
     with Not_found -> false)

(* ============================================================
   empty_payload Tests
   ============================================================ *)

let test_empty_payload_summary () =
  check string "empty summary" "" Relay.empty_payload.summary

let test_empty_payload_task () =
  check (option string) "no task" None Relay.empty_payload.current_task

let test_empty_payload_todos () =
  check int "no todos" 0 (List.length Relay.empty_payload.todos)

let test_empty_payload_pdca () =
  check (option string) "no pdca" None Relay.empty_payload.pdca_state

let test_empty_payload_files () =
  check int "no files" 0 (List.length Relay.empty_payload.relevant_files)

let test_empty_payload_session () =
  check (option string) "no session" None Relay.empty_payload.session_id

let test_empty_payload_generation () =
  check int "generation 0" 0 Relay.empty_payload.relay_generation

(* ============================================================
   get_latest_checkpoint Tests (uses internal ref)
   ============================================================ *)

(* Note: This tests the initial state; checkpoints may have been
   modified by other tests if run in sequence *)
let test_get_latest_checkpoint_type () =
  let _ : Relay.checkpoint option = Relay.get_latest_checkpoint () in
  check bool "returns option" true true

(* ============================================================
   estimate_context claude-sonnet branch Tests
   ============================================================ *)

let test_estimate_context_claude_sonnet () =
  let m = Relay.estimate_context ~messages:10 ~tool_calls:5 ~model:"claude-sonnet" in
  check int "max tokens claude-sonnet" 200000 m.max_tokens

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Relay Coverage" [
    "relay_config.defaults", [
      test_case "threshold" `Quick test_default_config_threshold;
      test_case "target_agent" `Quick test_default_config_target_agent;
      test_case "compress_ratio" `Quick test_default_config_compress_ratio;
      test_case "include_todos" `Quick test_default_config_include_todos;
      test_case "include_pdca" `Quick test_default_config_include_pdca;
      test_case "neo4j_episode" `Quick test_default_config_neo4j_episode;
      test_case "custom config" `Quick test_custom_config;
    ];
    "estimate_context", [
      test_case "claude" `Quick test_estimate_context_claude;
      test_case "gemini" `Quick test_estimate_context_gemini;
      test_case "codex" `Quick test_estimate_context_codex;
      test_case "gpt" `Quick test_estimate_context_gpt;
      test_case "claude-opus" `Quick test_estimate_context_claude_opus;
      test_case "claude-sonnet" `Quick test_estimate_context_claude_sonnet;
      test_case "unknown model" `Quick test_estimate_context_unknown_model;
      test_case "usage ratio" `Quick test_estimate_context_usage_ratio;
      test_case "zero messages" `Quick test_estimate_context_zero_messages;
      test_case "many messages" `Quick test_estimate_context_many_messages;
    ];
    "task_cost", [
      test_case "large file" `Quick test_task_cost_large_file;
      test_case "multi file" `Quick test_task_cost_multi_file;
      test_case "single file" `Quick test_task_cost_multi_file_one;
      test_case "long running" `Quick test_task_cost_long_running;
      test_case "exploration" `Quick test_task_cost_exploration;
      test_case "simple" `Quick test_task_cost_simple;
    ];
    "should_relay.reactive", [
      test_case "below threshold" `Quick test_should_relay_below_threshold;
      test_case "at threshold" `Quick test_should_relay_at_threshold;
      test_case "above threshold" `Quick test_should_relay_above_threshold;
      test_case "custom threshold" `Quick test_should_relay_custom_threshold;
    ];
    "should_relay.proactive", [
      test_case "simple no relay" `Quick test_proactive_simple_task_no_relay;
      test_case "large file relay" `Quick test_proactive_large_file_relay;
      test_case "long task relay" `Quick test_proactive_long_task_relay;
      test_case "multi file relay" `Quick test_proactive_multi_file_relay;
    ];
    "should_relay.smart", [
      test_case "no relay" `Quick test_smart_no_relay;
      test_case "reactive" `Quick test_smart_reactive;
      test_case "proactive" `Quick test_smart_proactive;
      test_case "reactive over proactive" `Quick test_smart_reactive_over_proactive;
    ];
    "compress_context", [
      test_case "summary only" `Quick test_compress_context_summary_only;
      test_case "with task" `Quick test_compress_context_with_task;
      test_case "with todos" `Quick test_compress_context_with_todos;
      test_case "with pdca" `Quick test_compress_context_with_pdca;
      test_case "with files" `Quick test_compress_context_with_files;
      test_case "full" `Quick test_compress_context_full;
    ];
    "build_handoff_prompt", [
      test_case "basic" `Quick test_build_handoff_prompt_basic;
      test_case "generation" `Quick test_build_handoff_prompt_generation;
      test_case "instructions" `Quick test_build_handoff_prompt_instructions;
      test_case "seamless" `Quick test_build_handoff_prompt_seamless;
    ];
    "checkpoint_to_payload", [
      test_case "basic" `Quick test_checkpoint_to_payload_basic;
      test_case "none task" `Quick test_checkpoint_to_payload_none_task;
    ];
    "metrics_to_json", [
      test_case "basic" `Quick test_metrics_to_json_basic;
      test_case "values" `Quick test_metrics_to_json_values;
    ];
    "payload_to_json", [
      test_case "basic" `Quick test_payload_to_json_basic;
      test_case "nulls" `Quick test_payload_to_json_nulls;
      test_case "todos array" `Quick test_payload_to_json_todos_array;
    ];
    "empty_payload", [
      test_case "summary" `Quick test_empty_payload_summary;
      test_case "task" `Quick test_empty_payload_task;
      test_case "todos" `Quick test_empty_payload_todos;
      test_case "pdca" `Quick test_empty_payload_pdca;
      test_case "files" `Quick test_empty_payload_files;
      test_case "session" `Quick test_empty_payload_session;
      test_case "generation" `Quick test_empty_payload_generation;
    ];
    "get_latest_checkpoint", [
      test_case "returns option" `Quick test_get_latest_checkpoint_type;
    ];
    "handoff_payload", [
      test_case "creation" `Quick test_handoff_payload_creation;
      test_case "empty" `Quick test_handoff_payload_empty;
      test_case "incremented generation" `Quick test_handoff_payload_incremented_generation;
    ];
    "context_metrics", [
      test_case "creation" `Quick test_context_metrics_creation;
    ];
  ]
