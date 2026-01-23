(** Tests for Checkpoint_types module *)

open Masc_mcp.Checkpoint_types

(** Test status_of_string and status_to_string roundtrip *)
let test_status_roundtrip () =
  let statuses = [Pending; InProgress; Interrupted; Completed; Rejected] in
  List.iter (fun status ->
    let str = status_to_string status in
    match status_of_string str with
    | Some parsed -> Alcotest.(check bool) "roundtrip" true (equal_checkpoint_status status parsed)
    | None -> Alcotest.fail (Printf.sprintf "Failed to parse %s" str)
  ) statuses

(** Test status_of_string with invalid input *)
let test_status_of_string_invalid () =
  (* Use a simple option check since we just need to verify None *)
  let is_none opt = match opt with None -> true | Some _ -> false in
  Alcotest.(check bool) "invalid" true (is_none (status_of_string "invalid"));
  Alcotest.(check bool) "empty" true (is_none (status_of_string ""));
  Alcotest.(check bool) "PENDING" true (is_none (status_of_string "PENDING"))  (* case sensitive *)

(** Test make_checkpoint_id format *)
let test_make_checkpoint_id () =
  let id = make_checkpoint_id ~task_id:"task-001" ~step:3 ~timestamp:1704067200 in
  Alcotest.(check string) "format" "cp-task-001-3-1704067200" id

(** Test make_checkpoint_id with edge cases *)
let test_make_checkpoint_id_edge_cases () =
  (* Step 1 *)
  let id1 = make_checkpoint_id ~task_id:"t" ~step:1 ~timestamp:0 in
  Alcotest.(check string) "minimal" "cp-t-1-0" id1;

  (* Large step number *)
  let id2 = make_checkpoint_id ~task_id:"task" ~step:999 ~timestamp:123 in
  Alcotest.(check string) "large step" "cp-task-999-123" id2

(** Test format_date *)
let test_format_date () =
  (* 2024-01-01 00:00:00 UTC = 1704067200 *)
  let date = format_date 1704067200.0 in
  (* Note: result depends on local timezone, just check format *)
  let parts = String.split_on_char '-' date in
  Alcotest.(check int) "parts count" 3 (List.length parts);
  Alcotest.(check int) "year length" 4 (String.length (List.nth parts 0));
  Alcotest.(check int) "month length" 2 (String.length (List.nth parts 1));
  Alcotest.(check int) "day length" 2 (String.length (List.nth parts 2))

(** Test is_valid_task_id *)
let test_is_valid_task_id () =
  (* Valid cases *)
  Alcotest.(check bool) "simple" true (is_valid_task_id "task-001");
  Alcotest.(check bool) "alphanumeric" true (is_valid_task_id "PK12345");
  Alcotest.(check bool) "with underscore" true (is_valid_task_id "task_001");
  Alcotest.(check bool) "mixed" true (is_valid_task_id "Task-001_v2");

  (* Invalid cases *)
  Alcotest.(check bool) "empty" false (is_valid_task_id "");
  Alcotest.(check bool) "space" false (is_valid_task_id "task 001");
  Alcotest.(check bool) "special char" false (is_valid_task_id "task@001");
  Alcotest.(check bool) "unicode" false (is_valid_task_id "태스크-001")

(** Test is_valid_step *)
let test_is_valid_step () =
  Alcotest.(check bool) "positive" true (is_valid_step 1);
  Alcotest.(check bool) "large" true (is_valid_step 9999);
  Alcotest.(check bool) "zero" false (is_valid_step 0);
  Alcotest.(check bool) "negative" false (is_valid_step (-1))

(** Test can_transition state machine *)
let test_can_transition () =
  (* Valid transitions *)
  Alcotest.(check bool) "pending->in_progress" true (can_transition ~from:Pending ~to_:InProgress);
  Alcotest.(check bool) "in_progress->interrupted" true (can_transition ~from:InProgress ~to_:Interrupted);
  Alcotest.(check bool) "in_progress->completed" true (can_transition ~from:InProgress ~to_:Completed);
  Alcotest.(check bool) "interrupted->completed" true (can_transition ~from:Interrupted ~to_:Completed);
  Alcotest.(check bool) "interrupted->rejected" true (can_transition ~from:Interrupted ~to_:Rejected);

  (* Invalid transitions *)
  Alcotest.(check bool) "pending->completed" false (can_transition ~from:Pending ~to_:Completed);
  Alcotest.(check bool) "completed->pending" false (can_transition ~from:Completed ~to_:Pending);
  Alcotest.(check bool) "rejected->completed" false (can_transition ~from:Rejected ~to_:Completed);
  Alcotest.(check bool) "interrupted->pending" false (can_transition ~from:Interrupted ~to_:Pending)

(** Test is_terminal *)
let test_is_terminal () =
  Alcotest.(check bool) "completed" true (is_terminal Completed);
  Alcotest.(check bool) "rejected" true (is_terminal Rejected);
  Alcotest.(check bool) "pending" false (is_terminal Pending);
  Alcotest.(check bool) "in_progress" false (is_terminal InProgress);
  Alcotest.(check bool) "interrupted" false (is_terminal Interrupted)

(** Test requires_user_action *)
let test_requires_user_action () =
  Alcotest.(check bool) "interrupted" true (requires_user_action Interrupted);
  Alcotest.(check bool) "pending" false (requires_user_action Pending);
  Alcotest.(check bool) "completed" false (requires_user_action Completed)

(** Test is_timed_out *)
let test_is_timed_out () =
  let now = Unix.time () in
  (* Created 31 minutes ago with 30 minute timeout *)
  Alcotest.(check bool) "expired" true
    (is_timed_out ~created_at:(now -. 1860.0) ~timeout_minutes:30);
  (* Created 29 minutes ago with 30 minute timeout *)
  Alcotest.(check bool) "not expired" false
    (is_timed_out ~created_at:(now -. 1740.0) ~timeout_minutes:30);
  (* Edge case: exactly at timeout *)
  Alcotest.(check bool) "at boundary" false
    (is_timed_out ~created_at:(now -. 1800.0) ~timeout_minutes:30)

(** Test is_valid_json_state *)
let test_is_valid_json_state () =
  Alcotest.(check bool) "empty object" true (is_valid_json_state "{}");
  Alcotest.(check bool) "with data" true (is_valid_json_state {|{"key": "value"}|});
  Alcotest.(check bool) "array" true (is_valid_json_state "[1, 2, 3]");
  Alcotest.(check bool) "invalid" false (is_valid_json_state "{invalid}");
  Alcotest.(check bool) "empty" false (is_valid_json_state "")

(** Test make_checkpoint_info *)
let test_make_checkpoint_info () =
  let info = make_checkpoint_info
    ~id:"cp-001" ~task_id:"task-001" ~step:1
    ~action:"test" ~agent:"claude" ~status:Pending () in
  Alcotest.(check string) "id" "cp-001" info.id;
  Alcotest.(check string) "task_id" "task-001" info.task_id;
  Alcotest.(check int) "step" 1 info.step;
  Alcotest.(check string) "action" "test" info.action;
  Alcotest.(check string) "agent" "claude" info.agent;
  Alcotest.(check bool) "status" true (equal_checkpoint_status Pending info.status);
  Alcotest.(check (option string)) "no interrupt_message" None info.interrupt_message;

  (* With optional fields *)
  let info2 = make_checkpoint_info
    ~id:"cp-002" ~task_id:"task-002" ~step:2
    ~action:"delete" ~agent:"gemini" ~status:Interrupted
    ~interrupt_message:"Really delete?" ~created_at:1704067200.0 () in
  Alcotest.(check (option string)) "with interrupt_message" (Some "Really delete?") info2.interrupt_message;
  Alcotest.(check (option (float 0.01))) "with created_at" (Some 1704067200.0) info2.created_at

let () =
  Alcotest.run "Checkpoint" [
    "status", [
      Alcotest.test_case "roundtrip" `Quick test_status_roundtrip;
      Alcotest.test_case "invalid input" `Quick test_status_of_string_invalid;
    ];
    "checkpoint_id", [
      Alcotest.test_case "format" `Quick test_make_checkpoint_id;
      Alcotest.test_case "edge cases" `Quick test_make_checkpoint_id_edge_cases;
    ];
    "date", [
      Alcotest.test_case "format" `Quick test_format_date;
    ];
    "validation", [
      Alcotest.test_case "task_id" `Quick test_is_valid_task_id;
      Alcotest.test_case "step" `Quick test_is_valid_step;
      Alcotest.test_case "json_state" `Quick test_is_valid_json_state;
    ];
    "state_machine", [
      Alcotest.test_case "transitions" `Quick test_can_transition;
      Alcotest.test_case "terminal" `Quick test_is_terminal;
      Alcotest.test_case "user_action" `Quick test_requires_user_action;
    ];
    "timeout", [
      Alcotest.test_case "is_timed_out" `Quick test_is_timed_out;
    ];
    "checkpoint_info", [
      Alcotest.test_case "make" `Quick test_make_checkpoint_info;
    ];
  ]
