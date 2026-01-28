(** Planning Eio Module Coverage Tests

    Tests for planning types and JSON serialization:
    - error_entry type
    - planning_context type
    - JSON roundtrip functions
*)

open Alcotest

module Planning_eio = Masc_mcp.Planning_eio

(* ============================================================
   error_entry Type Tests
   ============================================================ *)

let test_error_entry_basic () =
  let e : Planning_eio.error_entry = {
    timestamp = "2024-01-01T12:00:00Z";
    error_type = "build";
    message = "Compilation failed";
    context = None;
    resolved = false;
  } in
  check string "timestamp" "2024-01-01T12:00:00Z" e.timestamp;
  check string "error_type" "build" e.error_type;
  check bool "resolved" false e.resolved

let test_error_entry_with_context () =
  let e : Planning_eio.error_entry = {
    timestamp = "2024-01-01T12:30:00Z";
    error_type = "test";
    message = "Test assertion failed";
    context = Some "test_main.ml:42";
    resolved = true;
  } in
  match e.context with
  | Some ctx -> check string "context" "test_main.ml:42" ctx
  | None -> fail "expected Some"

let test_error_entry_types () =
  let types = ["build"; "test"; "runtime"; "logic"] in
  let entries = List.map (fun t : Planning_eio.error_entry ->
    { timestamp = ""; error_type = t; message = ""; context = None; resolved = false }
  ) types in
  check int "error types count" 4 (List.length entries)

(* ============================================================
   planning_context Type Tests
   ============================================================ *)

let test_planning_context_basic () =
  let ctx : Planning_eio.planning_context = {
    task_id = "task-001";
    task_plan = "# Plan\n1. Do something";
    notes = [];
    errors = [];
    deliverable = "";
    created_at = "2024-01-01T10:00:00Z";
    updated_at = "2024-01-01T10:00:00Z";
  } in
  check string "task_id" "task-001" ctx.task_id;
  check (list string) "notes empty" [] ctx.notes

let test_planning_context_with_notes () =
  let ctx : Planning_eio.planning_context = {
    task_id = "task-002";
    task_plan = "Plan content";
    notes = ["Note 1"; "Note 2"; "Observation"];
    errors = [];
    deliverable = "";
    created_at = "";
    updated_at = "";
  } in
  check int "notes count" 3 (List.length ctx.notes)

let test_planning_context_with_errors () =
  let err : Planning_eio.error_entry = {
    timestamp = "2024-01-01T12:00:00Z";
    error_type = "runtime";
    message = "Null pointer";
    context = Some "main.ml:10";
    resolved = false;
  } in
  let ctx : Planning_eio.planning_context = {
    task_id = "task-003";
    task_plan = "Plan";
    notes = [];
    errors = [err];
    deliverable = "Output";
    created_at = "";
    updated_at = "";
  } in
  check int "errors count" 1 (List.length ctx.errors);
  check string "deliverable" "Output" ctx.deliverable

(* ============================================================
   JSON Serialization Tests
   ============================================================ *)

let test_error_entry_json_roundtrip () =
  let original : Planning_eio.error_entry = {
    timestamp = "2024-01-01T00:00:00Z";
    error_type = "test";
    message = "Assertion failed";
    context = Some "test.ml";
    resolved = true;
  } in
  let json = Planning_eio.error_entry_to_yojson original in
  match Planning_eio.error_entry_of_yojson json with
  | Ok decoded ->
      check string "timestamp" original.timestamp decoded.timestamp;
      check string "error_type" original.error_type decoded.error_type;
      check bool "resolved" original.resolved decoded.resolved
  | Error _ -> fail "json decode failed"

let test_error_entry_json_no_context () =
  let original : Planning_eio.error_entry = {
    timestamp = "2024-01-01T00:00:00Z";
    error_type = "build";
    message = "Error";
    context = None;
    resolved = false;
  } in
  let json = Planning_eio.error_entry_to_yojson original in
  match Planning_eio.error_entry_of_yojson json with
  | Ok decoded ->
      check bool "context is None" true (decoded.context = None)
  | Error _ -> fail "json decode failed"

let test_planning_context_json_roundtrip () =
  let original : Planning_eio.planning_context = {
    task_id = "roundtrip-001";
    task_plan = "# My Plan\n- Step 1\n- Step 2";
    notes = ["Note A"; "Note B"];
    errors = [];
    deliverable = "Final output here";
    created_at = "2024-01-01T09:00:00Z";
    updated_at = "2024-01-01T10:00:00Z";
  } in
  let json = Planning_eio.planning_context_to_yojson original in
  match Planning_eio.planning_context_of_yojson json with
  | Ok decoded ->
      check string "task_id" original.task_id decoded.task_id;
      check string "task_plan" original.task_plan decoded.task_plan;
      check int "notes count" 2 (List.length decoded.notes);
      check string "deliverable" original.deliverable decoded.deliverable
  | Error _ -> fail "json decode failed"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Planning Eio Coverage" [
    "error_entry", [
      test_case "basic" `Quick test_error_entry_basic;
      test_case "with context" `Quick test_error_entry_with_context;
      test_case "types" `Quick test_error_entry_types;
    ];
    "planning_context", [
      test_case "basic" `Quick test_planning_context_basic;
      test_case "with notes" `Quick test_planning_context_with_notes;
      test_case "with errors" `Quick test_planning_context_with_errors;
    ];
    "json_roundtrip", [
      test_case "error_entry" `Quick test_error_entry_json_roundtrip;
      test_case "error_entry no context" `Quick test_error_entry_json_no_context;
      test_case "planning_context" `Quick test_planning_context_json_roundtrip;
    ];
  ]
