(** Metrics Store Eio Module Coverage Tests

    Tests for metrics types with deriving:
    - task_metric type
    - agent_metrics type
    - JSON serialization roundtrips
*)

open Alcotest

module Metrics_store_eio = Masc_mcp.Metrics_store_eio

(* ============================================================
   task_metric Type Tests
   ============================================================ *)

let test_task_metric_basic () =
  let m : Metrics_store_eio.task_metric = {
    id = "metric-001";
    agent_id = "claude";
    task_id = "task-123";
    started_at = 1704067200.0;
    completed_at = None;
    success = false;
    error_message = None;
    collaborators = [];
    handoff_from = None;
    handoff_to = None;
  } in
  check string "id" "metric-001" m.id;
  check string "agent_id" "claude" m.agent_id;
  check bool "success" false m.success

let test_task_metric_completed () =
  let m : Metrics_store_eio.task_metric = {
    id = "metric-002";
    agent_id = "gemini";
    task_id = "task-456";
    started_at = 1704067200.0;
    completed_at = Some 1704067500.0;
    success = true;
    error_message = None;
    collaborators = ["claude"; "codex"];
    handoff_from = Some "claude";
    handoff_to = None;
  } in
  check bool "success" true m.success;
  check int "collaborators count" 2 (List.length m.collaborators);
  match m.completed_at, m.handoff_from with
  | Some c, Some h ->
      check (float 0.1) "completed_at" 1704067500.0 c;
      check string "handoff_from" "claude" h
  | _, _ -> fail "expected Some"

let test_task_metric_failed () =
  let m : Metrics_store_eio.task_metric = {
    id = "metric-003";
    agent_id = "codex";
    task_id = "task-789";
    started_at = 1704067200.0;
    completed_at = Some 1704067300.0;
    success = false;
    error_message = Some "Task timeout exceeded";
    collaborators = [];
    handoff_from = None;
    handoff_to = Some "claude";
  } in
  check bool "success" false m.success;
  match m.error_message, m.handoff_to with
  | Some err, Some h ->
      check string "error_message" "Task timeout exceeded" err;
      check string "handoff_to" "claude" h
  | _, _ -> fail "expected Some"

(* ============================================================
   agent_metrics Type Tests
   ============================================================ *)

let test_agent_metrics_type () =
  let am : Metrics_store_eio.agent_metrics = {
    agent_id = "claude";
    period_start = 1704067200.0;
    period_end = 1704153600.0;
    total_tasks = 100;
    completed_tasks = 85;
    failed_tasks = 15;
    avg_completion_time_s = 120.5;
    task_completion_rate = 0.85;
    error_rate = 0.15;
    handoff_success_rate = 0.90;
    unique_collaborators = ["gemini"; "codex"];
  } in
  check string "agent_id" "claude" am.agent_id;
  check int "total_tasks" 100 am.total_tasks;
  check int "completed_tasks" 85 am.completed_tasks;
  check int "failed_tasks" 15 am.failed_tasks

let test_agent_metrics_rates () =
  let am : Metrics_store_eio.agent_metrics = {
    agent_id = "gemini";
    period_start = 0.0;
    period_end = 0.0;
    total_tasks = 50;
    completed_tasks = 45;
    failed_tasks = 5;
    avg_completion_time_s = 60.0;
    task_completion_rate = 0.90;
    error_rate = 0.10;
    handoff_success_rate = 0.95;
    unique_collaborators = [];
  } in
  check (float 0.01) "completion_rate" 0.90 am.task_completion_rate;
  check (float 0.01) "error_rate" 0.10 am.error_rate;
  check (float 0.01) "handoff_success_rate" 0.95 am.handoff_success_rate

(* ============================================================
   JSON Serialization Tests (deriving yojson)
   ============================================================ *)

let test_task_metric_json_roundtrip () =
  let original : Metrics_store_eio.task_metric = {
    id = "m-rt-001";
    agent_id = "claude";
    task_id = "t-001";
    started_at = 1704067200.0;
    completed_at = Some 1704067300.0;
    success = true;
    error_message = None;
    collaborators = ["gemini"];
    handoff_from = None;
    handoff_to = None;
  } in
  let json = Metrics_store_eio.task_metric_to_yojson original in
  match Metrics_store_eio.task_metric_of_yojson json with
  | Ok decoded ->
      check string "id roundtrip" original.id decoded.id;
      check string "agent_id roundtrip" original.agent_id decoded.agent_id;
      check bool "success roundtrip" original.success decoded.success
  | Error _ -> fail "json decode failed"

let test_agent_metrics_json_roundtrip () =
  let original : Metrics_store_eio.agent_metrics = {
    agent_id = "codex";
    period_start = 0.0;
    period_end = 1000.0;
    total_tasks = 10;
    completed_tasks = 8;
    failed_tasks = 2;
    avg_completion_time_s = 50.0;
    task_completion_rate = 0.8;
    error_rate = 0.2;
    handoff_success_rate = 1.0;
    unique_collaborators = [];
  } in
  let json = Metrics_store_eio.agent_metrics_to_yojson original in
  match Metrics_store_eio.agent_metrics_of_yojson json with
  | Ok decoded ->
      check string "agent_id roundtrip" original.agent_id decoded.agent_id;
      check int "total_tasks roundtrip" original.total_tasks decoded.total_tasks
  | Error _ -> fail "json decode failed"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Metrics Store Eio Coverage" [
    "task_metric", [
      test_case "basic" `Quick test_task_metric_basic;
      test_case "completed" `Quick test_task_metric_completed;
      test_case "failed" `Quick test_task_metric_failed;
    ];
    "agent_metrics", [
      test_case "type" `Quick test_agent_metrics_type;
      test_case "rates" `Quick test_agent_metrics_rates;
    ];
    "json_roundtrip", [
      test_case "task_metric" `Quick test_task_metric_json_roundtrip;
      test_case "agent_metrics" `Quick test_agent_metrics_json_roundtrip;
    ];
  ]
