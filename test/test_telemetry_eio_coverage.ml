(** Telemetry Eio Module Coverage Tests

    Tests for telemetry event types with deriving yojson:
    - event type variants
    - event_record type
    - metrics type
    - JSON roundtrip tests
*)

open Alcotest

module Telemetry_eio = Masc_mcp.Telemetry_eio

(* ============================================================
   event Type Tests
   ============================================================ *)

let test_event_agent_joined () =
  let e = Telemetry_eio.Agent_joined {
    agent_id = "claude-001";
    capabilities = ["code"; "review"];
  } in
  match e with
  | Telemetry_eio.Agent_joined r ->
      check string "agent_id" "claude-001" r.agent_id;
      check int "capabilities" 2 (List.length r.capabilities)
  | _ -> fail "expected Agent_joined"

let test_event_agent_left () =
  let e = Telemetry_eio.Agent_left {
    agent_id = "claude-001";
    reason = "session ended";
  } in
  match e with
  | Telemetry_eio.Agent_left r ->
      check string "reason" "session ended" r.reason
  | _ -> fail "expected Agent_left"

let test_event_task_started () =
  let e = Telemetry_eio.Task_started {
    task_id = "task-001";
    agent_id = "claude-001";
  } in
  match e with
  | Telemetry_eio.Task_started r ->
      check string "task_id" "task-001" r.task_id
  | _ -> fail "expected Task_started"

let test_event_task_completed () =
  let e = Telemetry_eio.Task_completed {
    task_id = "task-001";
    duration_ms = 5000;
    success = true;
  } in
  match e with
  | Telemetry_eio.Task_completed r ->
      check int "duration_ms" 5000 r.duration_ms;
      check bool "success" true r.success
  | _ -> fail "expected Task_completed"

let test_event_handoff_triggered () =
  let e = Telemetry_eio.Handoff_triggered {
    from_agent = "claude-001";
    to_agent = "codex-001";
    reason = "context limit";
  } in
  match e with
  | Telemetry_eio.Handoff_triggered r ->
      check string "from_agent" "claude-001" r.from_agent;
      check string "to_agent" "codex-001" r.to_agent
  | _ -> fail "expected Handoff_triggered"

let test_event_error_occurred () =
  let e = Telemetry_eio.Error_occurred {
    code = "E001";
    message = "Something failed";
    context = "test";
  } in
  match e with
  | Telemetry_eio.Error_occurred r ->
      check string "code" "E001" r.code;
      check string "message" "Something failed" r.message
  | _ -> fail "expected Error_occurred"

let test_event_tool_called () =
  let e = Telemetry_eio.Tool_called {
    tool_name = "masc_status";
    success = true;
    duration_ms = 100;
    agent_id = Some "claude-001";
  } in
  match e with
  | Telemetry_eio.Tool_called r ->
      check string "tool_name" "masc_status" r.tool_name;
      check bool "success" true r.success
  | _ -> fail "expected Tool_called"

(* ============================================================
   event_record Type Tests
   ============================================================ *)

let test_event_record_type () =
  let r : Telemetry_eio.event_record = {
    timestamp = 1704067200.0;
    event = Telemetry_eio.Agent_joined {
      agent_id = "test";
      capabilities = [];
    };
  } in
  check (float 0.1) "timestamp" 1704067200.0 r.timestamp

(* ============================================================
   metrics Type Tests
   ============================================================ *)

let test_metrics_type () =
  let m : Telemetry_eio.metrics = {
    active_agents = 3;
    tasks_in_progress = 5;
    tasks_completed_24h = 42;
    avg_task_duration_ms = 3500.0;
    handoff_rate = 0.15;
    error_rate = 0.02;
  } in
  check int "active_agents" 3 m.active_agents;
  check int "tasks_in_progress" 5 m.tasks_in_progress;
  check int "tasks_completed_24h" 42 m.tasks_completed_24h;
  check (float 0.01) "avg_task_duration_ms" 3500.0 m.avg_task_duration_ms;
  check (float 0.01) "handoff_rate" 0.15 m.handoff_rate;
  check (float 0.01) "error_rate" 0.02 m.error_rate

(* ============================================================
   JSON Roundtrip Tests
   ============================================================ *)

let test_event_json_roundtrip () =
  let original = Telemetry_eio.Task_completed {
    task_id = "task-roundtrip";
    duration_ms = 1234;
    success = true;
  } in
  let json = Telemetry_eio.event_to_yojson original in
  match Telemetry_eio.event_of_yojson json with
  | Ok decoded ->
      (match decoded with
       | Telemetry_eio.Task_completed r ->
           check string "task_id" "task-roundtrip" r.task_id;
           check int "duration_ms" 1234 r.duration_ms
       | _ -> fail "wrong event type")
  | Error e -> fail ("json decode failed: " ^ e)

let test_metrics_json_roundtrip () =
  let original : Telemetry_eio.metrics = {
    active_agents = 10;
    tasks_in_progress = 7;
    tasks_completed_24h = 100;
    avg_task_duration_ms = 2500.0;
    handoff_rate = 0.1;
    error_rate = 0.01;
  } in
  let json = Telemetry_eio.metrics_to_yojson original in
  match Telemetry_eio.metrics_of_yojson json with
  | Ok decoded ->
      check int "active_agents" 10 decoded.active_agents;
      check int "tasks_completed_24h" 100 decoded.tasks_completed_24h
  | Error e -> fail ("json decode failed: " ^ e)

(* ============================================================
   event_to_json Tests
   ============================================================ *)

let test_event_to_json_agent_joined () =
  let e = Telemetry_eio.Agent_joined {
    agent_id = "test";
    capabilities = ["a"; "b"];
  } in
  let json_str = Telemetry_eio.event_to_json e in
  check bool "nonempty" true (String.length json_str > 0);
  check bool "is json" true (String.contains json_str '{')

let test_event_to_json_task_completed () =
  let e = Telemetry_eio.Task_completed {
    task_id = "t1";
    duration_ms = 100;
    success = true;
  } in
  let json_str = Telemetry_eio.event_to_json e in
  check bool "nonempty" true (String.length json_str > 0);
  check bool "contains timestamp" true (String.length json_str > 10)

(* ============================================================
   count_active_agents Tests
   ============================================================ *)

let test_count_active_agents_empty () =
  let events : Telemetry_eio.event_record list = [] in
  check int "empty" 0 (Telemetry_eio.count_active_agents events)

let test_count_active_agents_one_joined () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Agent_joined { agent_id = "a1"; capabilities = [] } };
  ] in
  check int "one joined" 1 (Telemetry_eio.count_active_agents events)

let test_count_active_agents_joined_then_left () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Agent_joined { agent_id = "a1"; capabilities = [] } };
    { timestamp = 2.0; event = Agent_left { agent_id = "a1"; reason = "done" } };
  ] in
  check int "joined then left" 0 (Telemetry_eio.count_active_agents events)

let test_count_active_agents_multiple () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Agent_joined { agent_id = "a1"; capabilities = [] } };
    { timestamp = 2.0; event = Agent_joined { agent_id = "a2"; capabilities = [] } };
    { timestamp = 3.0; event = Agent_left { agent_id = "a1"; reason = "x" } };
  ] in
  check int "multiple" 1 (Telemetry_eio.count_active_agents events)

(* ============================================================
   count_tasks_in_progress Tests
   ============================================================ *)

let test_count_tasks_in_progress_empty () =
  let events : Telemetry_eio.event_record list = [] in
  check int "empty" 0 (Telemetry_eio.count_tasks_in_progress events)

let test_count_tasks_in_progress_one_started () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_started { task_id = "t1"; agent_id = "a1" } };
  ] in
  check int "one started" 1 (Telemetry_eio.count_tasks_in_progress events)

let test_count_tasks_in_progress_started_completed () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_started { task_id = "t1"; agent_id = "a1" } };
    { timestamp = 2.0; event = Task_completed { task_id = "t1"; duration_ms = 100; success = true } };
  ] in
  check int "completed" 0 (Telemetry_eio.count_tasks_in_progress events)

(* ============================================================
   count_completed_tasks Tests
   ============================================================ *)

let test_count_completed_tasks_empty () =
  let events : Telemetry_eio.event_record list = [] in
  check int "empty" 0 (Telemetry_eio.count_completed_tasks events)

let test_count_completed_tasks_one () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_completed { task_id = "t1"; duration_ms = 100; success = true } };
  ] in
  check int "one" 1 (Telemetry_eio.count_completed_tasks events)

let test_count_completed_tasks_multiple () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_completed { task_id = "t1"; duration_ms = 100; success = true } };
    { timestamp = 2.0; event = Task_completed { task_id = "t2"; duration_ms = 200; success = false } };
    { timestamp = 3.0; event = Task_started { task_id = "t3"; agent_id = "a1" } };
  ] in
  check int "two completed" 2 (Telemetry_eio.count_completed_tasks events)

(* ============================================================
   avg_duration Tests
   ============================================================ *)

let test_avg_duration_empty () =
  let events : Telemetry_eio.event_record list = [] in
  let avg = Telemetry_eio.avg_duration events in
  check bool "zero" true (abs_float avg < 0.01)

let test_avg_duration_one () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_completed { task_id = "t1"; duration_ms = 1000; success = true } };
  ] in
  let avg = Telemetry_eio.avg_duration events in
  check bool "1000" true (abs_float (avg -. 1000.0) < 0.01)

let test_avg_duration_multiple () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_completed { task_id = "t1"; duration_ms = 1000; success = true } };
    { timestamp = 2.0; event = Task_completed { task_id = "t2"; duration_ms = 2000; success = true } };
  ] in
  let avg = Telemetry_eio.avg_duration events in
  check bool "avg 1500" true (abs_float (avg -. 1500.0) < 0.01)

(* ============================================================
   calculate_handoff_rate Tests
   ============================================================ *)

let test_calculate_handoff_rate_empty () =
  let events : Telemetry_eio.event_record list = [] in
  let rate = Telemetry_eio.calculate_handoff_rate events in
  check bool "zero" true (abs_float rate < 0.01)

let test_calculate_handoff_rate_no_handoffs () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_completed { task_id = "t1"; duration_ms = 100; success = true } };
  ] in
  let rate = Telemetry_eio.calculate_handoff_rate events in
  check bool "zero" true (abs_float rate < 0.01)

let test_calculate_handoff_rate_some_handoffs () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_completed { task_id = "t1"; duration_ms = 100; success = true } };
    { timestamp = 2.0; event = Handoff_triggered { from_agent = "a1"; to_agent = "a2"; reason = "x" } };
  ] in
  let rate = Telemetry_eio.calculate_handoff_rate events in
  check bool "positive" true (rate > 0.0)

(* ============================================================
   calculate_error_rate Tests
   ============================================================ *)

let test_calculate_error_rate_empty () =
  let events : Telemetry_eio.event_record list = [] in
  let rate = Telemetry_eio.calculate_error_rate events in
  check bool "zero" true (abs_float rate < 0.01)

let test_calculate_error_rate_no_errors () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_completed { task_id = "t1"; duration_ms = 100; success = true } };
  ] in
  let rate = Telemetry_eio.calculate_error_rate events in
  check bool "zero" true (abs_float rate < 0.01)

let test_calculate_error_rate_some_errors () =
  let events : Telemetry_eio.event_record list = [
    { timestamp = 1.0; event = Task_completed { task_id = "t1"; duration_ms = 100; success = true } };
    { timestamp = 2.0; event = Error_occurred { code = "E1"; message = "err"; context = "ctx" } };
  ] in
  let rate = Telemetry_eio.calculate_error_rate events in
  check bool "positive" true (rate > 0.0)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Telemetry Eio Coverage" [
    "event", [
      test_case "agent_joined" `Quick test_event_agent_joined;
      test_case "agent_left" `Quick test_event_agent_left;
      test_case "task_started" `Quick test_event_task_started;
      test_case "task_completed" `Quick test_event_task_completed;
      test_case "handoff_triggered" `Quick test_event_handoff_triggered;
      test_case "error_occurred" `Quick test_event_error_occurred;
      test_case "tool_called" `Quick test_event_tool_called;
    ];
    "event_record", [
      test_case "type" `Quick test_event_record_type;
    ];
    "metrics", [
      test_case "type" `Quick test_metrics_type;
    ];
    "json_roundtrip", [
      test_case "event" `Quick test_event_json_roundtrip;
      test_case "metrics" `Quick test_metrics_json_roundtrip;
    ];
    "event_to_json", [
      test_case "agent_joined" `Quick test_event_to_json_agent_joined;
      test_case "task_completed" `Quick test_event_to_json_task_completed;
    ];
    "count_active_agents", [
      test_case "empty" `Quick test_count_active_agents_empty;
      test_case "one joined" `Quick test_count_active_agents_one_joined;
      test_case "joined then left" `Quick test_count_active_agents_joined_then_left;
      test_case "multiple" `Quick test_count_active_agents_multiple;
    ];
    "count_tasks_in_progress", [
      test_case "empty" `Quick test_count_tasks_in_progress_empty;
      test_case "one started" `Quick test_count_tasks_in_progress_one_started;
      test_case "started completed" `Quick test_count_tasks_in_progress_started_completed;
    ];
    "count_completed_tasks", [
      test_case "empty" `Quick test_count_completed_tasks_empty;
      test_case "one" `Quick test_count_completed_tasks_one;
      test_case "multiple" `Quick test_count_completed_tasks_multiple;
    ];
    "avg_duration", [
      test_case "empty" `Quick test_avg_duration_empty;
      test_case "one" `Quick test_avg_duration_one;
      test_case "multiple" `Quick test_avg_duration_multiple;
    ];
    "calculate_handoff_rate", [
      test_case "empty" `Quick test_calculate_handoff_rate_empty;
      test_case "no handoffs" `Quick test_calculate_handoff_rate_no_handoffs;
      test_case "some handoffs" `Quick test_calculate_handoff_rate_some_handoffs;
    ];
    "calculate_error_rate", [
      test_case "empty" `Quick test_calculate_error_rate_empty;
      test_case "no errors" `Quick test_calculate_error_rate_no_errors;
      test_case "some errors" `Quick test_calculate_error_rate_some_errors;
    ];
  ]
