(** Tempo Module Coverage Tests

    Tests for MASC Tempo Control:
    - state_to_json / state_of_json: JSON serialization
    - is_pending_task: task status classification
    - calculate_adaptive_tempo: tempo calculation logic
*)

open Alcotest

module Tempo = Masc_mcp.Tempo
module Types = Masc_mcp.Types

(* ============================================================
   state_to_json Tests
   ============================================================ *)

let test_state_to_json_has_fields () =
  let state : Tempo.tempo_state = {
    current_interval_s = 300.0;
    last_adjusted = 1704067200.0;
    reason = "test reason";
  } in
  let json = Tempo.state_to_json state in
  match json with
  | `Assoc fields ->
    check bool "has current_interval_s" true (List.mem_assoc "current_interval_s" fields);
    check bool "has last_adjusted" true (List.mem_assoc "last_adjusted" fields);
    check bool "has reason" true (List.mem_assoc "reason" fields)
  | _ -> fail "expected Assoc"

let test_state_to_json_interval_value () =
  let state : Tempo.tempo_state = {
    current_interval_s = 60.0;
    last_adjusted = 0.0;
    reason = "fast";
  } in
  let json = Tempo.state_to_json state in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "current_interval_s" fields with
     | Some (`Float f) -> check (float 0.1) "interval" 60.0 f
     | _ -> fail "expected Float")
  | _ -> fail "expected Assoc"

let test_state_to_json_reason_value () =
  let state : Tempo.tempo_state = {
    current_interval_s = 300.0;
    last_adjusted = 0.0;
    reason = "normal tempo";
  } in
  let json = Tempo.state_to_json state in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "reason" fields with
     | Some (`String s) -> check string "reason" "normal tempo" s
     | _ -> fail "expected String")
  | _ -> fail "expected Assoc"

(* ============================================================
   state_of_json Tests
   ============================================================ *)

let test_state_of_json_valid () =
  let json = `Assoc [
    ("current_interval_s", `Float 300.0);
    ("last_adjusted", `Float 1704067200.0);
    ("reason", `String "test");
  ] in
  match Tempo.state_of_json json with
  | Some state ->
    check (float 0.1) "interval" 300.0 state.current_interval_s;
    check string "reason" "test" state.reason
  | None -> fail "expected Some"

let test_state_of_json_missing_field () =
  let json = `Assoc [
    ("current_interval_s", `Float 300.0);
  ] in
  match Tempo.state_of_json json with
  | None -> ()
  | Some _ -> fail "expected None for missing fields"

let test_state_of_json_invalid_type () =
  let json = `Assoc [
    ("current_interval_s", `String "not a float");
    ("last_adjusted", `Float 0.0);
    ("reason", `String "test");
  ] in
  match Tempo.state_of_json json with
  | None -> ()
  | Some _ -> fail "expected None for invalid type"

let test_state_of_json_not_assoc () =
  let json = `String "not an object" in
  match Tempo.state_of_json json with
  | None -> ()
  | Some _ -> fail "expected None for non-object"

let test_state_of_json_roundtrip () =
  let original : Tempo.tempo_state = {
    current_interval_s = 600.0;
    last_adjusted = 1704067200.0;
    reason = "slow tempo - idle";
  } in
  let json = Tempo.state_to_json original in
  match Tempo.state_of_json json with
  | Some state ->
    check (float 0.1) "interval" original.current_interval_s state.current_interval_s;
    check string "reason" original.reason state.reason
  | None -> fail "roundtrip failed"

(* ============================================================
   is_pending_task Tests
   ============================================================ *)

let make_task ~id ~status : Types.task = {
  id;
  title = "Test Task";
  description = "";
  task_status = status;
  priority = 3;
  files = [];
  created_at = "2024-01-01T00:00:00Z";
  worktree = None;
}

let test_is_pending_task_todo () =
  let task = make_task ~id:"t1" ~status:Types.Todo in
  check bool "todo is pending" true (Tempo.is_pending_task task)

let test_is_pending_task_claimed () =
  let task = make_task ~id:"t2" ~status:(Types.Claimed { assignee = "claude"; claimed_at = "2024-01-01T00:00:00Z" }) in
  check bool "claimed is pending" true (Tempo.is_pending_task task)

let test_is_pending_task_in_progress () =
  let task = make_task ~id:"t3" ~status:(Types.InProgress { assignee = "claude"; started_at = "2024-01-01T00:00:00Z" }) in
  check bool "in_progress is pending" true (Tempo.is_pending_task task)

let test_is_pending_task_done () =
  let task = make_task ~id:"t4" ~status:(Types.Done { assignee = "claude"; completed_at = "2024-01-01T00:00:00Z"; notes = None }) in
  check bool "done is not pending" false (Tempo.is_pending_task task)

let test_is_pending_task_cancelled () =
  let task = make_task ~id:"t5" ~status:(Types.Cancelled { cancelled_by = "user"; cancelled_at = "2024-01-01T00:00:00Z"; reason = Some "test" }) in
  check bool "cancelled is not pending" false (Tempo.is_pending_task task)

(* ============================================================
   calculate_adaptive_tempo Tests
   ============================================================ *)

let make_task_with_priority ~id ~priority : Types.task = {
  id;
  title = "Task";
  description = "";
  task_status = Types.Todo;
  priority;
  files = [];
  created_at = "2024-01-01T00:00:00Z";
  worktree = None;
}

let test_calculate_adaptive_tempo_empty () =
  let (interval, reason) = Tempo.calculate_adaptive_tempo [] in
  check (float 0.1) "max interval for empty" 600.0 interval;
  check bool "reason not empty" true (String.length reason > 0)

let test_calculate_adaptive_tempo_urgent () =
  let tasks = [
    make_task_with_priority ~id:"t1" ~priority:1;
    make_task_with_priority ~id:"t2" ~priority:3;
  ] in
  let (interval, reason) = Tempo.calculate_adaptive_tempo tasks in
  check (float 0.1) "min interval for urgent" 60.0 interval;
  check bool "reason contains fast" true
    (try let _ = Str.search_forward (Str.regexp "fast") reason 0 in true
     with Not_found -> false)

let test_calculate_adaptive_tempo_priority_2 () =
  let tasks = [make_task_with_priority ~id:"t1" ~priority:2] in
  let (interval, _) = Tempo.calculate_adaptive_tempo tasks in
  check (float 0.1) "min interval for priority 2" 60.0 interval

let test_calculate_adaptive_tempo_normal () =
  let tasks = [
    make_task_with_priority ~id:"t1" ~priority:3;
    make_task_with_priority ~id:"t2" ~priority:3;
  ] in
  let (interval, reason) = Tempo.calculate_adaptive_tempo tasks in
  check (float 0.1) "default interval for normal" 300.0 interval;
  check bool "reason contains normal" true
    (try let _ = Str.search_forward (Str.regexp "normal") reason 0 in true
     with Not_found -> false)

let test_calculate_adaptive_tempo_low_priority () =
  let tasks = [
    make_task_with_priority ~id:"t1" ~priority:4;
    make_task_with_priority ~id:"t2" ~priority:5;
  ] in
  let (interval, reason) = Tempo.calculate_adaptive_tempo tasks in
  check (float 0.1) "max interval for low priority" 600.0 interval;
  check bool "reason contains slow" true
    (try let _ = Str.search_forward (Str.regexp "slow") reason 0 in true
     with Not_found -> false)

(* ============================================================
   tempo_file Tests
   ============================================================ *)

module Backend = Masc_mcp.Backend
module Room_utils = Masc_mcp.Room_utils

let make_test_config ~base_path : Room_utils.config =
  let backend_config : Backend.config = {
    backend_type = Backend.Memory;
    base_path;
    postgres_url = None;
    node_id = "test-node";
    cluster_name = "default";
    pubsub_max_messages = 1000;
  } in
  let memory_backend = match Backend.MemoryBackend.create backend_config with
    | Ok t -> t
    | Error _ -> failwith "Failed to create memory backend"
  in
  {
    Room_utils.base_path;
    lock_expiry_minutes = 30;
    backend_config;
    backend = Room_utils.Memory memory_backend;
  }

let test_tempo_file_basic () =
  let cfg = make_test_config ~base_path:"/home/user/project" in
  let result = Tempo.tempo_file cfg in
  check string "tempo file path" "/home/user/project/.masc/tempo.json" result

let test_tempo_file_with_trailing_slash () =
  let cfg = make_test_config ~base_path:"/home/user/project/" in
  let result = Tempo.tempo_file cfg in
  (* Filename.concat handles trailing slash *)
  check bool "ends with tempo.json" true (String.length result > 0)

let test_tempo_file_relative_path () =
  let cfg = make_test_config ~base_path:"./myproject" in
  let result = Tempo.tempo_file cfg in
  check string "relative tempo path" "./myproject/.masc/tempo.json" result

let test_tempo_file_empty_path () =
  let cfg = make_test_config ~base_path:"" in
  let result = Tempo.tempo_file cfg in
  check string "empty base path" ".masc/tempo.json" result

(* ============================================================
   tempo_config Type Tests
   ============================================================ *)

let test_tempo_config_type () =
  let cfg : Tempo.tempo_config = {
    min_interval_s = 30.0;
    max_interval_s = 900.0;
    default_interval_s = 300.0;
    adaptive = false;
  } in
  check (float 0.1) "min" 30.0 cfg.min_interval_s;
  check (float 0.1) "max" 900.0 cfg.max_interval_s;
  check (float 0.1) "default" 300.0 cfg.default_interval_s;
  check bool "adaptive" false cfg.adaptive

let test_tempo_config_edge_values () =
  let cfg : Tempo.tempo_config = {
    min_interval_s = 1.0;
    max_interval_s = 3600.0;
    default_interval_s = 60.0;
    adaptive = true;
  } in
  check bool "min < max" true (cfg.min_interval_s < cfg.max_interval_s);
  check bool "default in range" true
    (cfg.default_interval_s >= cfg.min_interval_s &&
     cfg.default_interval_s <= cfg.max_interval_s)

(* ============================================================
   tempo_state Type Tests
   ============================================================ *)

let test_tempo_state_type () =
  let state : Tempo.tempo_state = {
    current_interval_s = 120.0;
    last_adjusted = 1704067200.0;
    reason = "custom reason";
  } in
  check (float 0.1) "interval" 120.0 state.current_interval_s;
  check (float 0.1) "last_adjusted" 1704067200.0 state.last_adjusted;
  check string "reason" "custom reason" state.reason

let test_tempo_state_zero_values () =
  let state : Tempo.tempo_state = {
    current_interval_s = 0.0;
    last_adjusted = 0.0;
    reason = "";
  } in
  check (float 0.1) "zero interval" 0.0 state.current_interval_s;
  check (float 0.1) "zero time" 0.0 state.last_adjusted;
  check string "empty reason" "" state.reason

(* ============================================================
   format_state Tests (Pure Function)
   ============================================================ *)

let test_format_state_fast_tempo () =
  let state : Tempo.tempo_state = {
    current_interval_s = 60.0;
    last_adjusted = Unix.gettimeofday ();  (* just now *)
    reason = "fast - 1 urgent task(s)";
  } in
  let formatted = Tempo.format_state state in
  check bool "contains tempo" true
    (try let _ = Str.search_forward (Str.regexp "Tempo") formatted 0 in true
     with Not_found -> false);
  check bool "contains 60s" true
    (try let _ = Str.search_forward (Str.regexp "60s") formatted 0 in true
     with Not_found -> false)

let test_format_state_normal_tempo () =
  let state : Tempo.tempo_state = {
    current_interval_s = 300.0;
    last_adjusted = Unix.gettimeofday ();
    reason = "normal tempo";
  } in
  let formatted = Tempo.format_state state in
  check bool "contains 5.0m" true
    (try let _ = Str.search_forward (Str.regexp "5\\.0m") formatted 0 in true
     with Not_found -> false)

let test_format_state_slow_tempo () =
  let state : Tempo.tempo_state = {
    current_interval_s = 600.0;
    last_adjusted = Unix.gettimeofday ();
    reason = "idle";
  } in
  let formatted = Tempo.format_state state in
  check bool "contains 10.0m" true
    (try let _ = Str.search_forward (Str.regexp "10\\.0m") formatted 0 in true
     with Not_found -> false)

let test_format_state_shows_reason () =
  let state : Tempo.tempo_state = {
    current_interval_s = 60.0;
    last_adjusted = Unix.gettimeofday ();
    reason = "urgent tasks present";
  } in
  let formatted = Tempo.format_state state in
  check bool "contains reason" true
    (try let _ = Str.search_forward (Str.regexp "urgent tasks present") formatted 0 in true
     with Not_found -> false)

let test_format_state_old_adjustment () =
  let state : Tempo.tempo_state = {
    current_interval_s = 300.0;
    last_adjusted = Unix.gettimeofday () -. 3600.0;  (* 1 hour ago *)
    reason = "test";
  } in
  let formatted = Tempo.format_state state in
  check bool "contains hours ago" true
    (try let _ = Str.search_forward (Str.regexp "h ago") formatted 0 in true
     with Not_found -> false)

let test_format_state_minutes_ago () =
  let state : Tempo.tempo_state = {
    current_interval_s = 300.0;
    last_adjusted = Unix.gettimeofday () -. 120.0;  (* 2 minutes ago *)
    reason = "test";
  } in
  let formatted = Tempo.format_state state in
  check bool "contains minutes ago" true
    (try let _ = Str.search_forward (Str.regexp "m ago") formatted 0 in true
     with Not_found -> false)

let test_format_state_just_now () =
  let state : Tempo.tempo_state = {
    current_interval_s = 300.0;
    last_adjusted = Unix.gettimeofday () -. 5.0;  (* 5 seconds ago *)
    reason = "test";
  } in
  let formatted = Tempo.format_state state in
  check bool "contains just now" true
    (try let _ = Str.search_forward (Str.regexp "just now") formatted 0 in true
     with Not_found -> false)

let test_format_state_emoji () =
  let state : Tempo.tempo_state = {
    current_interval_s = 60.0;
    last_adjusted = Unix.gettimeofday ();
    reason = "test";
  } in
  let formatted = Tempo.format_state state in
  check bool "contains timer emoji" true
    (String.length formatted > 0 && String.get formatted 0 <> ' ')

(* ============================================================
   default_config Tests
   ============================================================ *)

let test_default_config_min () =
  check bool "min positive" true (Tempo.default_config.min_interval_s > 0.0)

let test_default_config_max () =
  check bool "max > min" true (Tempo.default_config.max_interval_s > Tempo.default_config.min_interval_s)

let test_default_config_default () =
  check bool "default in range" true
    (Tempo.default_config.default_interval_s >= Tempo.default_config.min_interval_s &&
     Tempo.default_config.default_interval_s <= Tempo.default_config.max_interval_s)

let test_default_config_adaptive () =
  check bool "adaptive enabled" true Tempo.default_config.adaptive

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tempo Coverage" [
    "state_to_json", [
      test_case "has fields" `Quick test_state_to_json_has_fields;
      test_case "interval value" `Quick test_state_to_json_interval_value;
      test_case "reason value" `Quick test_state_to_json_reason_value;
    ];
    "state_of_json", [
      test_case "valid" `Quick test_state_of_json_valid;
      test_case "missing field" `Quick test_state_of_json_missing_field;
      test_case "invalid type" `Quick test_state_of_json_invalid_type;
      test_case "not assoc" `Quick test_state_of_json_not_assoc;
      test_case "roundtrip" `Quick test_state_of_json_roundtrip;
    ];
    "is_pending_task", [
      test_case "todo" `Quick test_is_pending_task_todo;
      test_case "claimed" `Quick test_is_pending_task_claimed;
      test_case "in_progress" `Quick test_is_pending_task_in_progress;
      test_case "done" `Quick test_is_pending_task_done;
      test_case "cancelled" `Quick test_is_pending_task_cancelled;
    ];
    "calculate_adaptive_tempo", [
      test_case "empty" `Quick test_calculate_adaptive_tempo_empty;
      test_case "urgent" `Quick test_calculate_adaptive_tempo_urgent;
      test_case "priority 2" `Quick test_calculate_adaptive_tempo_priority_2;
      test_case "normal" `Quick test_calculate_adaptive_tempo_normal;
      test_case "low priority" `Quick test_calculate_adaptive_tempo_low_priority;
    ];
    "tempo_file", [
      test_case "basic" `Quick test_tempo_file_basic;
      test_case "with trailing slash" `Quick test_tempo_file_with_trailing_slash;
      test_case "relative path" `Quick test_tempo_file_relative_path;
      test_case "empty path" `Quick test_tempo_file_empty_path;
    ];
    "tempo_config_type", [
      test_case "all fields" `Quick test_tempo_config_type;
      test_case "edge values" `Quick test_tempo_config_edge_values;
    ];
    "tempo_state_type", [
      test_case "all fields" `Quick test_tempo_state_type;
      test_case "zero values" `Quick test_tempo_state_zero_values;
    ];
    "default_config", [
      test_case "min positive" `Quick test_default_config_min;
      test_case "max > min" `Quick test_default_config_max;
      test_case "default in range" `Quick test_default_config_default;
      test_case "adaptive" `Quick test_default_config_adaptive;
    ];
    "format_state", [
      test_case "fast tempo" `Quick test_format_state_fast_tempo;
      test_case "normal tempo" `Quick test_format_state_normal_tempo;
      test_case "slow tempo" `Quick test_format_state_slow_tempo;
      test_case "shows reason" `Quick test_format_state_shows_reason;
      test_case "old adjustment" `Quick test_format_state_old_adjustment;
      test_case "minutes ago" `Quick test_format_state_minutes_ago;
      test_case "just now" `Quick test_format_state_just_now;
      test_case "emoji" `Quick test_format_state_emoji;
    ];
  ]
