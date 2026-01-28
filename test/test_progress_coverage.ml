(** Progress Module Coverage Tests

    Tests for MCP Progress Notifications:
    - progress: record type
    - validation_error: TaskIdEmpty, TaskIdTooLong, TaskIdInvalidChars, ProgressOutOfRange
    - validate_task_id, validate_progress
    - validation_error_to_string
    - progress_to_jsonrpc
    - Tracker module
*)

open Alcotest

module Progress = Masc_mcp.Progress

(* ============================================================
   validate_task_id Tests
   ============================================================ *)

let test_validate_task_id_valid () =
  match Progress.validate_task_id "task-001" with
  | Ok s -> check string "valid" "task-001" s
  | Error _ -> fail "should be valid"

let test_validate_task_id_printable_ascii () =
  match Progress.validate_task_id "Task_123-test" with
  | Ok s -> check string "printable" "Task_123-test" s
  | Error _ -> fail "should be valid"

let test_validate_task_id_empty () =
  match Progress.validate_task_id "" with
  | Ok _ -> fail "should reject empty"
  | Error Progress.TaskIdEmpty -> ()
  | Error _ -> fail "expected TaskIdEmpty"

let test_validate_task_id_too_long () =
  let long = String.make 300 'a' in
  match Progress.validate_task_id long with
  | Ok _ -> fail "should reject too long"
  | Error (Progress.TaskIdTooLong n) -> check bool "length > 256" true (n > 256)
  | Error _ -> fail "expected TaskIdTooLong"

let test_validate_task_id_invalid_chars () =
  match Progress.validate_task_id "task\x00id" with
  | Ok _ -> fail "should reject invalid chars"
  | Error Progress.TaskIdInvalidChars -> ()
  | Error _ -> fail "expected TaskIdInvalidChars"

let test_validate_task_id_max_length () =
  let max = String.make 256 'x' in
  match Progress.validate_task_id max with
  | Ok s -> check int "max length ok" 256 (String.length s)
  | Error _ -> fail "should accept 256 chars"

(* ============================================================
   validate_progress Tests
   ============================================================ *)

let test_validate_progress_valid () =
  match Progress.validate_progress 0.5 with
  | Ok p -> check bool "valid" true (abs_float (p -. 0.5) < 0.001)
  | Error _ -> fail "should be valid"

let test_validate_progress_zero () =
  match Progress.validate_progress 0.0 with
  | Ok p -> check bool "zero ok" true (p = 0.0)
  | Error _ -> fail "should accept 0.0"

let test_validate_progress_one () =
  match Progress.validate_progress 1.0 with
  | Ok p -> check bool "one ok" true (p = 1.0)
  | Error _ -> fail "should accept 1.0"

let test_validate_progress_negative () =
  match Progress.validate_progress (-0.1) with
  | Ok _ -> fail "should reject negative"
  | Error (Progress.ProgressOutOfRange p) -> check bool "negative" true (p < 0.0)
  | Error _ -> fail "expected ProgressOutOfRange"

let test_validate_progress_over_one () =
  match Progress.validate_progress 1.5 with
  | Ok _ -> fail "should reject > 1.0"
  | Error (Progress.ProgressOutOfRange p) -> check bool "over one" true (p > 1.0)
  | Error _ -> fail "expected ProgressOutOfRange"

(* ============================================================
   validation_error_to_string Tests
   ============================================================ *)

let test_error_to_string_empty () =
  let s = Progress.validation_error_to_string Progress.TaskIdEmpty in
  check bool "contains empty" true (String.length s > 0)

let test_error_to_string_too_long () =
  let s = Progress.validation_error_to_string (Progress.TaskIdTooLong 300) in
  check bool "contains 300" true (String.length s > 0)

let test_error_to_string_invalid_chars () =
  let s = Progress.validation_error_to_string Progress.TaskIdInvalidChars in
  check bool "contains invalid" true (String.length s > 0)

let test_error_to_string_out_of_range () =
  let s = Progress.validation_error_to_string (Progress.ProgressOutOfRange 1.5) in
  check bool "contains range" true (String.length s > 0)

(* ============================================================
   progress_to_jsonrpc Tests
   ============================================================ *)

let test_progress_to_jsonrpc_basic () =
  let p : Progress.progress = {
    task_id = "task-001";
    progress = 0.5;
    message = None;
    estimated_remaining = None;
  } in
  let json = Progress.progress_to_jsonrpc p in
  match json with
  | `Assoc fields ->
    check bool "has jsonrpc" true (List.mem_assoc "jsonrpc" fields);
    check bool "has method" true (List.mem_assoc "method" fields);
    check bool "has params" true (List.mem_assoc "params" fields)
  | _ -> fail "expected Assoc"

let test_progress_to_jsonrpc_with_message () =
  let p : Progress.progress = {
    task_id = "task-002";
    progress = 0.75;
    message = Some "Processing...";
    estimated_remaining = None;
  } in
  let json = Progress.progress_to_jsonrpc p in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "params" fields with
     | Some (`Assoc params) ->
       check bool "has message" true (List.mem_assoc "message" params)
     | _ -> fail "expected params Assoc")
  | _ -> fail "expected Assoc"

let test_progress_to_jsonrpc_with_remaining () =
  let p : Progress.progress = {
    task_id = "task-003";
    progress = 0.25;
    message = None;
    estimated_remaining = Some 30.0;
  } in
  let json = Progress.progress_to_jsonrpc p in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "params" fields with
     | Some (`Assoc params) ->
       check bool "has estimatedRemaining" true (List.mem_assoc "estimatedRemaining" params)
     | _ -> fail "expected params Assoc")
  | _ -> fail "expected Assoc"

let test_progress_to_jsonrpc_method () =
  let p : Progress.progress = {
    task_id = "task-004";
    progress = 1.0;
    message = None;
    estimated_remaining = None;
  } in
  let json = Progress.progress_to_jsonrpc p in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "method" fields with
     | Some (`String m) -> check string "method" "notifications/progress" m
     | _ -> fail "expected method string")
  | _ -> fail "expected Assoc"

(* ============================================================
   Tracker Tests
   ============================================================ *)

let test_tracker_create () =
  let t = Progress.Tracker.create ~task_id:"task-001" () in
  check string "task_id" "task-001" t.task_id;
  check bool "current is 0" true (t.current = 0.0);
  check int "total_steps default" 100 t.total_steps;
  check int "completed_steps" 0 t.completed_steps

let test_tracker_create_custom_steps () =
  let t = Progress.Tracker.create ~task_id:"task-002" ~total_steps:50 () in
  check int "total_steps custom" 50 t.total_steps

let test_tracker_start_time () =
  let before = Unix.gettimeofday () in
  let t = Progress.Tracker.create ~task_id:"task-003" () in
  let after = Unix.gettimeofday () in
  check bool "start_time reasonable" true (t.start_time >= before && t.start_time <= after)

(* ============================================================
   progress Record Tests
   ============================================================ *)

let test_progress_record_creation () =
  let p : Progress.progress = {
    task_id = "test-task";
    progress = 0.42;
    message = Some "Halfway there";
    estimated_remaining = Some 60.0;
  } in
  check string "task_id" "test-task" p.task_id;
  check bool "progress" true (abs_float (p.progress -. 0.42) < 0.001);
  check (option string) "message" (Some "Halfway there") p.message;
  check (option (float 0.001)) "remaining" (Some 60.0) p.estimated_remaining

let test_progress_record_minimal () =
  let p : Progress.progress = {
    task_id = "minimal";
    progress = 0.0;
    message = None;
    estimated_remaining = None;
  } in
  check (option string) "no message" None p.message;
  check (option (float 0.001)) "no remaining" None p.estimated_remaining

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Progress Coverage" [
    "validate_task_id", [
      test_case "valid" `Quick test_validate_task_id_valid;
      test_case "printable ascii" `Quick test_validate_task_id_printable_ascii;
      test_case "empty" `Quick test_validate_task_id_empty;
      test_case "too long" `Quick test_validate_task_id_too_long;
      test_case "invalid chars" `Quick test_validate_task_id_invalid_chars;
      test_case "max length" `Quick test_validate_task_id_max_length;
    ];
    "validate_progress", [
      test_case "valid" `Quick test_validate_progress_valid;
      test_case "zero" `Quick test_validate_progress_zero;
      test_case "one" `Quick test_validate_progress_one;
      test_case "negative" `Quick test_validate_progress_negative;
      test_case "over one" `Quick test_validate_progress_over_one;
    ];
    "validation_error_to_string", [
      test_case "empty" `Quick test_error_to_string_empty;
      test_case "too long" `Quick test_error_to_string_too_long;
      test_case "invalid chars" `Quick test_error_to_string_invalid_chars;
      test_case "out of range" `Quick test_error_to_string_out_of_range;
    ];
    "progress_to_jsonrpc", [
      test_case "basic" `Quick test_progress_to_jsonrpc_basic;
      test_case "with message" `Quick test_progress_to_jsonrpc_with_message;
      test_case "with remaining" `Quick test_progress_to_jsonrpc_with_remaining;
      test_case "method" `Quick test_progress_to_jsonrpc_method;
    ];
    "tracker", [
      test_case "create" `Quick test_tracker_create;
      test_case "custom steps" `Quick test_tracker_create_custom_steps;
      test_case "start time" `Quick test_tracker_start_time;
    ];
    "progress_record", [
      test_case "creation" `Quick test_progress_record_creation;
      test_case "minimal" `Quick test_progress_record_minimal;
    ];
  ]
