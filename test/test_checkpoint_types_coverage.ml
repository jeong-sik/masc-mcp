(** Checkpoint_types Module Coverage Tests

    Tests for MASC Checkpoint Types:
    - checkpoint_status: Pending, InProgress, Interrupted, Completed, Rejected, Reverted, Branched
    - status_of_string, status_to_string: parsing and serialization
    - make_checkpoint_id: ID generation
    - is_valid_task_id, is_valid_step: validation
    - can_transition: state machine transitions
    - is_terminal, requires_user_action: status queries
    - checkpoint_info: record type
*)

open Alcotest

module Checkpoint_types = Masc_mcp.Checkpoint_types

(* ============================================================
   status_to_string Tests
   ============================================================ *)

let test_status_to_string_pending () =
  check string "pending" "pending" (Checkpoint_types.status_to_string Checkpoint_types.Pending)

let test_status_to_string_in_progress () =
  check string "in_progress" "in_progress" (Checkpoint_types.status_to_string Checkpoint_types.InProgress)

let test_status_to_string_interrupted () =
  check string "interrupted" "interrupted" (Checkpoint_types.status_to_string Checkpoint_types.Interrupted)

let test_status_to_string_completed () =
  check string "completed" "completed" (Checkpoint_types.status_to_string Checkpoint_types.Completed)

let test_status_to_string_rejected () =
  check string "rejected" "rejected" (Checkpoint_types.status_to_string Checkpoint_types.Rejected)

let test_status_to_string_reverted () =
  check string "reverted" "reverted" (Checkpoint_types.status_to_string Checkpoint_types.Reverted)

let test_status_to_string_branched () =
  check string "branched" "branched" (Checkpoint_types.status_to_string Checkpoint_types.Branched)

(* ============================================================
   status_of_string Tests
   ============================================================ *)

let test_status_of_string_pending () =
  match Checkpoint_types.status_of_string "pending" with
  | Some Checkpoint_types.Pending -> ()
  | _ -> fail "expected Pending"

let test_status_of_string_in_progress () =
  match Checkpoint_types.status_of_string "in_progress" with
  | Some Checkpoint_types.InProgress -> ()
  | _ -> fail "expected InProgress"

let test_status_of_string_interrupted () =
  match Checkpoint_types.status_of_string "interrupted" with
  | Some Checkpoint_types.Interrupted -> ()
  | _ -> fail "expected Interrupted"

let test_status_of_string_completed () =
  match Checkpoint_types.status_of_string "completed" with
  | Some Checkpoint_types.Completed -> ()
  | _ -> fail "expected Completed"

let test_status_of_string_rejected () =
  match Checkpoint_types.status_of_string "rejected" with
  | Some Checkpoint_types.Rejected -> ()
  | _ -> fail "expected Rejected"

let test_status_of_string_reverted () =
  match Checkpoint_types.status_of_string "reverted" with
  | Some Checkpoint_types.Reverted -> ()
  | _ -> fail "expected Reverted"

let test_status_of_string_branched () =
  match Checkpoint_types.status_of_string "branched" with
  | Some Checkpoint_types.Branched -> ()
  | _ -> fail "expected Branched"

let test_status_of_string_invalid () =
  match Checkpoint_types.status_of_string "invalid" with
  | None -> ()
  | Some _ -> fail "should return None"

let test_status_of_string_empty () =
  match Checkpoint_types.status_of_string "" with
  | None -> ()
  | Some _ -> fail "should return None"

(* ============================================================
   make_checkpoint_id Tests
   ============================================================ *)

let test_make_checkpoint_id_format () =
  let id = Checkpoint_types.make_checkpoint_id ~task_id:"task-001" ~step:1 ~timestamp:1234567890 in
  check string "format" "cp-task-001-1-1234567890" id

let test_make_checkpoint_id_different_values () =
  let id1 = Checkpoint_types.make_checkpoint_id ~task_id:"a" ~step:1 ~timestamp:100 in
  let id2 = Checkpoint_types.make_checkpoint_id ~task_id:"b" ~step:2 ~timestamp:200 in
  check bool "different" true (id1 <> id2)

let test_make_checkpoint_id_now () =
  let id = Checkpoint_types.make_checkpoint_id_now ~task_id:"test" ~step:5 in
  check bool "starts with cp-test" true (String.sub id 0 7 = "cp-test")

(* ============================================================
   format_date Tests
   ============================================================ *)

let test_format_date () =
  (* Known timestamp: 2020-01-01 00:00:00 UTC *)
  let date = Checkpoint_types.format_date 1577836800.0 in
  check int "date length" 10 (String.length date);
  check bool "has dash at 4" true (date.[4] = '-');
  check bool "has dash at 7" true (date.[7] = '-')

let test_today () =
  let today = Checkpoint_types.today () in
  check int "today length" 10 (String.length today);
  check bool "starts with 20" true (String.sub today 0 2 = "20")

(* ============================================================
   is_valid_task_id Tests
   ============================================================ *)

let test_valid_task_id_simple () =
  check bool "simple" true (Checkpoint_types.is_valid_task_id "task001")

let test_valid_task_id_with_dash () =
  check bool "with dash" true (Checkpoint_types.is_valid_task_id "task-001")

let test_valid_task_id_with_underscore () =
  check bool "with underscore" true (Checkpoint_types.is_valid_task_id "task_001")

let test_invalid_task_id_empty () =
  check bool "empty" false (Checkpoint_types.is_valid_task_id "")

let test_invalid_task_id_too_long () =
  let long = String.make 101 'a' in
  check bool "too long" false (Checkpoint_types.is_valid_task_id long)

let test_invalid_task_id_special_chars () =
  check bool "space" false (Checkpoint_types.is_valid_task_id "task 001");
  check bool "slash" false (Checkpoint_types.is_valid_task_id "task/001");
  check bool "at" false (Checkpoint_types.is_valid_task_id "task@001")

(* ============================================================
   is_valid_step Tests
   ============================================================ *)

let test_valid_step_positive () =
  check bool "1" true (Checkpoint_types.is_valid_step 1);
  check bool "100" true (Checkpoint_types.is_valid_step 100)

let test_invalid_step_zero () =
  check bool "0" false (Checkpoint_types.is_valid_step 0)

let test_invalid_step_negative () =
  check bool "-1" false (Checkpoint_types.is_valid_step (-1))

(* ============================================================
   can_transition Tests
   ============================================================ *)

let test_can_transition_pending_to_inprogress () =
  check bool "pending->inprogress" true
    (Checkpoint_types.can_transition ~from:Checkpoint_types.Pending ~to_:Checkpoint_types.InProgress)

let test_can_transition_inprogress_to_interrupted () =
  check bool "inprogress->interrupted" true
    (Checkpoint_types.can_transition ~from:Checkpoint_types.InProgress ~to_:Checkpoint_types.Interrupted)

let test_can_transition_inprogress_to_completed () =
  check bool "inprogress->completed" true
    (Checkpoint_types.can_transition ~from:Checkpoint_types.InProgress ~to_:Checkpoint_types.Completed)

let test_can_transition_interrupted_to_completed () =
  check bool "interrupted->completed" true
    (Checkpoint_types.can_transition ~from:Checkpoint_types.Interrupted ~to_:Checkpoint_types.Completed)

let test_can_transition_interrupted_to_rejected () =
  check bool "interrupted->rejected" true
    (Checkpoint_types.can_transition ~from:Checkpoint_types.Interrupted ~to_:Checkpoint_types.Rejected)

let test_cannot_transition_completed_to_pending () =
  check bool "completed->pending" false
    (Checkpoint_types.can_transition ~from:Checkpoint_types.Completed ~to_:Checkpoint_types.Pending)

let test_cannot_transition_pending_to_completed () =
  check bool "pending->completed" false
    (Checkpoint_types.can_transition ~from:Checkpoint_types.Pending ~to_:Checkpoint_types.Completed)

(* ============================================================
   is_terminal Tests
   ============================================================ *)

let test_is_terminal_completed () =
  check bool "completed" true (Checkpoint_types.is_terminal Checkpoint_types.Completed)

let test_is_terminal_rejected () =
  check bool "rejected" true (Checkpoint_types.is_terminal Checkpoint_types.Rejected)

let test_is_terminal_reverted () =
  check bool "reverted" true (Checkpoint_types.is_terminal Checkpoint_types.Reverted)

let test_not_terminal_pending () =
  check bool "pending" false (Checkpoint_types.is_terminal Checkpoint_types.Pending)

let test_not_terminal_inprogress () =
  check bool "inprogress" false (Checkpoint_types.is_terminal Checkpoint_types.InProgress)

let test_not_terminal_interrupted () =
  check bool "interrupted" false (Checkpoint_types.is_terminal Checkpoint_types.Interrupted)

(* ============================================================
   requires_user_action Tests
   ============================================================ *)

let test_requires_user_action_interrupted () =
  check bool "interrupted" true (Checkpoint_types.requires_user_action Checkpoint_types.Interrupted)

let test_not_requires_user_action_pending () =
  check bool "pending" false (Checkpoint_types.requires_user_action Checkpoint_types.Pending)

let test_not_requires_user_action_completed () =
  check bool "completed" false (Checkpoint_types.requires_user_action Checkpoint_types.Completed)

(* ============================================================
   is_timed_out Tests
   ============================================================ *)

let test_is_timed_out_old () =
  let old_time = Unix.time () -. 3700.0 in  (* More than 1 hour ago *)
  check bool "timed out" true (Checkpoint_types.is_timed_out ~created_at:old_time ~timeout_minutes:60)

let test_is_not_timed_out_recent () =
  let recent_time = Unix.time () -. 30.0 in  (* 30 seconds ago *)
  check bool "not timed out" false (Checkpoint_types.is_timed_out ~created_at:recent_time ~timeout_minutes:60)

(* ============================================================
   JSON Helpers Tests
   ============================================================ *)

let test_parse_json_string_valid () =
  match Checkpoint_types.parse_json_string "{\"key\":\"value\"}" with
  | Some _ -> ()
  | None -> fail "should parse valid JSON"

let test_parse_json_string_invalid () =
  match Checkpoint_types.parse_json_string "not json" with
  | Some _ -> fail "should return None for invalid"
  | None -> ()

let test_is_valid_json_state_valid () =
  check bool "valid" true (Checkpoint_types.is_valid_json_state "{}")

let test_is_valid_json_state_invalid () =
  check bool "invalid" false (Checkpoint_types.is_valid_json_state "invalid")

(* ============================================================
   checkpoint_info Tests
   ============================================================ *)

let test_make_checkpoint_info () =
  let info = Checkpoint_types.make_checkpoint_info
    ~id:"cp-001"
    ~task_id:"task-001"
    ~step:1
    ~action:"create_file"
    ~agent:"claude"
    ~status:Checkpoint_types.Pending
    () in
  check string "id" "cp-001" info.id;
  check string "task_id" "task-001" info.task_id;
  check int "step" 1 info.step;
  check string "action" "create_file" info.action;
  check string "agent" "claude" info.agent

let test_make_checkpoint_info_with_optionals () =
  let info = Checkpoint_types.make_checkpoint_info
    ~id:"cp-002"
    ~task_id:"task-002"
    ~step:2
    ~action:"edit"
    ~agent:"gemini"
    ~status:Checkpoint_types.Interrupted
    ~interrupt_message:"User review needed"
    ~created_at:1234567890.0
    () in
  check (option string) "interrupt_message" (Some "User review needed") info.interrupt_message;
  check (option (float 0.001)) "created_at" (Some 1234567890.0) info.created_at

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Checkpoint_types Coverage" [
    "status_to_string", [
      test_case "pending" `Quick test_status_to_string_pending;
      test_case "in_progress" `Quick test_status_to_string_in_progress;
      test_case "interrupted" `Quick test_status_to_string_interrupted;
      test_case "completed" `Quick test_status_to_string_completed;
      test_case "rejected" `Quick test_status_to_string_rejected;
      test_case "reverted" `Quick test_status_to_string_reverted;
      test_case "branched" `Quick test_status_to_string_branched;
    ];
    "status_of_string", [
      test_case "pending" `Quick test_status_of_string_pending;
      test_case "in_progress" `Quick test_status_of_string_in_progress;
      test_case "interrupted" `Quick test_status_of_string_interrupted;
      test_case "completed" `Quick test_status_of_string_completed;
      test_case "rejected" `Quick test_status_of_string_rejected;
      test_case "reverted" `Quick test_status_of_string_reverted;
      test_case "branched" `Quick test_status_of_string_branched;
      test_case "invalid" `Quick test_status_of_string_invalid;
      test_case "empty" `Quick test_status_of_string_empty;
    ];
    "make_checkpoint_id", [
      test_case "format" `Quick test_make_checkpoint_id_format;
      test_case "different" `Quick test_make_checkpoint_id_different_values;
      test_case "now" `Quick test_make_checkpoint_id_now;
    ];
    "format_date", [
      test_case "format" `Quick test_format_date;
      test_case "today" `Quick test_today;
    ];
    "is_valid_task_id", [
      test_case "simple" `Quick test_valid_task_id_simple;
      test_case "with dash" `Quick test_valid_task_id_with_dash;
      test_case "with underscore" `Quick test_valid_task_id_with_underscore;
      test_case "empty" `Quick test_invalid_task_id_empty;
      test_case "too long" `Quick test_invalid_task_id_too_long;
      test_case "special chars" `Quick test_invalid_task_id_special_chars;
    ];
    "is_valid_step", [
      test_case "positive" `Quick test_valid_step_positive;
      test_case "zero" `Quick test_invalid_step_zero;
      test_case "negative" `Quick test_invalid_step_negative;
    ];
    "can_transition", [
      test_case "pending->inprogress" `Quick test_can_transition_pending_to_inprogress;
      test_case "inprogress->interrupted" `Quick test_can_transition_inprogress_to_interrupted;
      test_case "inprogress->completed" `Quick test_can_transition_inprogress_to_completed;
      test_case "interrupted->completed" `Quick test_can_transition_interrupted_to_completed;
      test_case "interrupted->rejected" `Quick test_can_transition_interrupted_to_rejected;
      test_case "completed->pending invalid" `Quick test_cannot_transition_completed_to_pending;
      test_case "pending->completed invalid" `Quick test_cannot_transition_pending_to_completed;
    ];
    "is_terminal", [
      test_case "completed" `Quick test_is_terminal_completed;
      test_case "rejected" `Quick test_is_terminal_rejected;
      test_case "reverted" `Quick test_is_terminal_reverted;
      test_case "pending not" `Quick test_not_terminal_pending;
      test_case "inprogress not" `Quick test_not_terminal_inprogress;
      test_case "interrupted not" `Quick test_not_terminal_interrupted;
    ];
    "requires_user_action", [
      test_case "interrupted" `Quick test_requires_user_action_interrupted;
      test_case "pending not" `Quick test_not_requires_user_action_pending;
      test_case "completed not" `Quick test_not_requires_user_action_completed;
    ];
    "is_timed_out", [
      test_case "old" `Quick test_is_timed_out_old;
      test_case "recent" `Quick test_is_not_timed_out_recent;
    ];
    "json_helpers", [
      test_case "parse valid" `Quick test_parse_json_string_valid;
      test_case "parse invalid" `Quick test_parse_json_string_invalid;
      test_case "is_valid valid" `Quick test_is_valid_json_state_valid;
      test_case "is_valid invalid" `Quick test_is_valid_json_state_invalid;
    ];
    "checkpoint_info", [
      test_case "basic" `Quick test_make_checkpoint_info;
      test_case "with optionals" `Quick test_make_checkpoint_info_with_optionals;
    ];
  ]
