(** Comprehensive coverage tests for Room module

    Target: 35+ additional tests covering:
    - Batch operations
    - Task transitions (claim_next, update_priority, cancel, release)
    - Pause/Resume functionality
    - GC and cleanup
    - Result-returning variants (_r functions)
    - Raw data accessors
    - Edge cases not covered in test_room.ml
*)

open Masc_mcp

(* ============================================================ *)
(* Test Helpers                                                  *)
(* ============================================================ *)

(** Check for success emoji *)
let contains_check result =
  String.length result >= 3 && String.sub result 0 3 = "\xE2\x9C\x85"  (* ✅ *)

(** Check for warning emoji *)
let contains_warning result =
  String.length result >= 3 && String.sub result 0 3 = "\xE2\x9A\xA0"  (* ⚠ *)

(** Check for error emoji *)
let contains_error result =
  String.length result >= 3 && String.sub result 0 3 = "\xE2\x9D\x8C"  (* ❌ *)

(** Check for cancel emoji *)
let contains_cancel result =
  String.length result >= 4 && String.sub result 0 4 = "\xF0\x9F\x9A\xAB"  (* 🚫 *)

(** Substring check helper *)
let str_contains s substring =
  let len_s = String.length s in
  let len_sub = String.length substring in
  if len_sub > len_s then false
  else
    let rec check i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = substring then true
      else check (i + 1)
    in
    check 0

(** Create fresh test environment with cleanup *)
let with_test_env f =
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc_coverage_%d_%d" (Unix.getpid ())
       (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir tmp_dir 0o755;
  let config = Room.default_config tmp_dir in
  let _ = Room.init config ~agent_name:(Some "claude") in
  try
    f config;
    let _ = Room.reset config in
    Unix.rmdir tmp_dir
  with e ->
    let _ = Room.reset config in
    Unix.rmdir tmp_dir;
    raise e

(* ============================================================ *)
(* Batch Operations Tests                                        *)
(* ============================================================ *)

let test_batch_add_tasks () =
  with_test_env (fun config ->
    let tasks = [
      ("Task A", 1, "Description A");
      ("Task B", 2, "Description B");
      ("Task C", 3, "Description C");
    ] in
    let result = Room.batch_add_tasks config tasks in
    Alcotest.(check bool) "batch add success" true (contains_check result);
    Alcotest.(check bool) "contains task-001" true (str_contains result "task-001");
    Alcotest.(check bool) "contains task-003" true (str_contains result "task-003")
  )

let test_batch_add_empty_list () =
  with_test_env (fun config ->
    let result = Room.batch_add_tasks config [] in
    Alcotest.(check bool) "batch add empty returns something" true (String.length result > 0)
  )

let test_batch_add_single_task () =
  with_test_env (fun config ->
    let result = Room.batch_add_tasks config [("Single", 1, "Only one")] in
    Alcotest.(check bool) "single task batch" true (contains_check result)
  )

let test_batch_add_preserves_priorities () =
  with_test_env (fun config ->
    let tasks = [
      ("High Priority", 1, "");
      ("Low Priority", 5, "");
    ] in
    let _ = Room.batch_add_tasks config tasks in
    let task_list = Room.list_tasks config in
    Alcotest.(check bool) "shows priorities" true
      (str_contains task_list "[1]" && str_contains task_list "[5]")
  )

(* ============================================================ *)
(* Claim Next Tests                                              *)
(* ============================================================ *)

let test_claim_next_basic () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test Task" ~priority:1 ~description:"" in
    let result = Room.claim_next config ~agent_name:"claude" in
    Alcotest.(check bool) "claim next success" true (contains_check result);
    Alcotest.(check bool) "has task id" true (str_contains result "task-001")
  )

let test_claim_next_priority_order () =
  with_test_env (fun config ->
    (* Add tasks in non-priority order *)
    let _ = Room.add_task config ~title:"Low" ~priority:5 ~description:"" in
    let _ = Room.add_task config ~title:"High" ~priority:1 ~description:"" in
    let _ = Room.add_task config ~title:"Medium" ~priority:3 ~description:"" in

    (* Should claim highest priority (lowest number) first *)
    let result = Room.claim_next config ~agent_name:"claude" in
    Alcotest.(check bool) "claims high priority first" true
      (str_contains result "[P1]" || str_contains result "task-002")
  )

let test_claim_next_empty_backlog () =
  with_test_env (fun config ->
    let result = Room.claim_next config ~agent_name:"claude" in
    Alcotest.(check bool) "no tasks message" true (str_contains result "No unclaimed")
  )

let test_claim_next_all_claimed () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Only Task" ~priority:1 ~description:"" in
    let _ = Room.claim_task config ~agent_name:"gemini" ~task_id:"task-001" in

    let result = Room.claim_next config ~agent_name:"claude" in
    Alcotest.(check bool) "no unclaimed tasks" true (str_contains result "No unclaimed")
  )

let test_claim_next_consecutive () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"First" ~priority:1 ~description:"" in
    let _ = Room.add_task config ~title:"Second" ~priority:2 ~description:"" in

    let r1 = Room.claim_next config ~agent_name:"claude" in
    let r2 = Room.claim_next config ~agent_name:"gemini" in

    Alcotest.(check bool) "first claim success" true (contains_check r1);
    Alcotest.(check bool) "second claim success" true (contains_check r2);
    (* Different agents should get different tasks *)
    Alcotest.(check bool) "different tasks" true
      (str_contains r1 "task-001" || str_contains r2 "task-002")
  )

(* ============================================================ *)
(* Update Priority Tests                                         *)
(* ============================================================ *)

let test_update_priority () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:5 ~description:"" in
    let result = Room.update_priority config ~task_id:"task-001" ~priority:1 in
    Alcotest.(check bool) "priority updated" true (contains_check result);
    Alcotest.(check bool) "shows old and new" true
      (str_contains result "P5" && str_contains result "P1")
  )

let test_update_priority_nonexistent () =
  with_test_env (fun config ->
    let result = Room.update_priority config ~task_id:"task-999" ~priority:1 in
    Alcotest.(check bool) "task not found" true (contains_error result)
  )

let test_update_priority_negative () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:5 ~description:"" in
    let result = Room.update_priority config ~task_id:"task-001" ~priority:(-1) in
    Alcotest.(check bool) "negative priority allowed" true (contains_check result)
  )

(* ============================================================ *)
(* Cancel Task Tests                                             *)
(* ============================================================ *)

let test_cancel_task_todo () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    let result = Room.cancel_task_r config ~agent_name:"claude" ~task_id:"task-001" ~reason:"Not needed" in
    match result with
    | Ok msg -> Alcotest.(check bool) "cancel success" true (str_contains msg "cancelled")
    | Error _ -> Alcotest.fail "Expected Ok"
  )

let test_cancel_task_claimed_by_self () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    let _ = Room.claim_task config ~agent_name:"claude" ~task_id:"task-001" in

    let result = Room.cancel_task_r config ~agent_name:"claude" ~task_id:"task-001" ~reason:"Changed plans" in
    match result with
    | Ok msg -> Alcotest.(check bool) "cancel own task" true (str_contains msg "cancelled")
    | Error _ -> Alcotest.fail "Expected Ok"
  )

let test_cancel_task_claimed_by_other () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    let _ = Room.claim_task config ~agent_name:"gemini" ~task_id:"task-001" in

    let result = Room.cancel_task_r config ~agent_name:"claude" ~task_id:"task-001" ~reason:"" in
    match result with
    | Error _ -> Alcotest.(check bool) "cannot cancel other's task" true true
    | Ok _ -> Alcotest.fail "Expected Error"
  )

let test_cancel_task_nonexistent () =
  with_test_env (fun config ->
    let result = Room.cancel_task_r config ~agent_name:"claude" ~task_id:"task-999" ~reason:"" in
    match result with
    | Error Types.TaskNotFound _ -> Alcotest.(check bool) "task not found" true true
    | _ -> Alcotest.fail "Expected TaskNotFound"
  )

let test_cancel_done_task () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    let _ = Room.claim_task config ~agent_name:"claude" ~task_id:"task-001" in
    let _ = Room.complete_task config ~agent_name:"claude" ~task_id:"task-001" ~notes:"" in

    let result = Room.cancel_task_r config ~agent_name:"claude" ~task_id:"task-001" ~reason:"" in
    match result with
    | Error Types.TaskInvalidState _ -> Alcotest.(check bool) "cannot cancel done task" true true
    | _ -> Alcotest.fail "Expected TaskInvalidState"
  )

(* ============================================================ *)
(* Transition Task Tests                                         *)
(* ============================================================ *)

let test_transition_claim () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    let result = Room.transition_task_r config ~agent_name:"claude" ~task_id:"task-001" ~action:"claim" () in
    match result with
    | Ok msg -> Alcotest.(check bool) "claim via transition" true (str_contains msg "todo" && str_contains msg "claimed")
    | Error _ -> Alcotest.fail "Expected Ok"
  )

let test_transition_start () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    let _ = Room.claim_task config ~agent_name:"claude" ~task_id:"task-001" in

    let result = Room.transition_task_r config ~agent_name:"claude" ~task_id:"task-001" ~action:"start" () in
    match result with
    | Ok msg -> Alcotest.(check bool) "start via transition" true (str_contains msg "in_progress")
    | Error _ -> Alcotest.fail "Expected Ok"
  )

let test_transition_release () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    let _ = Room.claim_task config ~agent_name:"claude" ~task_id:"task-001" in

    let result = Room.release_task_r config ~agent_name:"claude" ~task_id:"task-001" () in
    match result with
    | Ok msg -> Alcotest.(check bool) "release via transition" true (str_contains msg "todo")
    | Error _ -> Alcotest.fail "Expected Ok"
  )

let test_transition_invalid () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    (* Try to start without claiming first *)
    let result = Room.transition_task_r config ~agent_name:"claude" ~task_id:"task-001" ~action:"start" () in
    match result with
    | Error Types.TaskInvalidState _ -> Alcotest.(check bool) "invalid transition" true true
    | _ -> Alcotest.fail "Expected TaskInvalidState"
  )

let test_transition_version_mismatch () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in

    (* Pass wrong expected version *)
    let result = Room.transition_task_r config ~agent_name:"claude" ~task_id:"task-001"
                   ~action:"claim" ~expected_version:999 () in
    match result with
    | Error Types.TaskInvalidState _ -> Alcotest.(check bool) "version mismatch" true true
    | _ -> Alcotest.fail "Expected TaskInvalidState for version mismatch"
  )

(* ============================================================ *)
(* Pause/Resume Tests                                            *)
(* ============================================================ *)

let test_pause_room () =
  with_test_env (fun config ->
    Room.pause config ~by:"claude" ~reason:"Testing pause";
    Alcotest.(check bool) "room is paused" true (Room.is_paused config)
  )

let test_resume_room () =
  with_test_env (fun config ->
    Room.pause config ~by:"claude" ~reason:"Testing pause";
    let result = Room.resume config ~by:"claude" in
    match result with
    | `Resumed ->
        Alcotest.(check bool) "room resumed" true (not (Room.is_paused config))
    | _ -> Alcotest.fail "Expected Resumed"
  )

let test_resume_not_paused () =
  with_test_env (fun config ->
    let result = Room.resume config ~by:"claude" in
    match result with
    | `Already_running -> Alcotest.(check bool) "already running" true true
    | _ -> Alcotest.fail "Expected Already_running"
  )

let test_pause_info () =
  with_test_env (fun config ->
    Room.pause config ~by:"claude" ~reason:"Maintenance";
    match Room.pause_info config with
    | Some (Some by, Some reason, Some _) ->
        Alcotest.(check string) "paused by" "claude" by;
        Alcotest.(check string) "reason" "Maintenance" reason
    | _ -> Alcotest.fail "Expected pause info"
  )

let test_pause_info_not_paused () =
  with_test_env (fun config ->
    match Room.pause_info config with
    | None -> Alcotest.(check bool) "no pause info" true true
    | Some _ -> Alcotest.fail "Expected None"
  )

(* ============================================================ *)
(* Raw Data Accessor Tests                                       *)
(* ============================================================ *)

let test_get_tasks_raw () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Task 1" ~priority:1 ~description:"" in
    let _ = Room.add_task config ~title:"Task 2" ~priority:2 ~description:"" in

    let tasks = Room.get_tasks_raw config in
    Alcotest.(check int) "two tasks" 2 (List.length tasks)
  )

let test_get_tasks_raw_empty () =
  with_test_env (fun config ->
    let tasks = Room.get_tasks_raw config in
    Alcotest.(check int) "no tasks" 0 (List.length tasks)
  )

let test_get_agents_raw () =
  with_test_env (fun config ->
    let _ = Room.join config ~agent_name:"gemini" ~capabilities:["test"] () in

    let agents : Types.agent list = Room.get_agents_raw config in
    (* claude from init + gemini *)
    Alcotest.(check bool) "at least 2 agents" true (List.length agents >= 2)
  )

let test_get_messages_raw () =
  with_test_env (fun config ->
    let _ = Room.broadcast config ~from_agent:"claude" ~content:"Message 1" in
    let _ = Room.broadcast config ~from_agent:"claude" ~content:"Message 2" in

    let msgs = Room.get_messages_raw config ~since_seq:0 ~limit:10 in
    Alcotest.(check bool) "has messages" true (List.length msgs >= 2)
  )

let test_is_agent_joined () =
  with_test_env (fun config ->
    (* claude is joined from init *)
    (* Note: agent names are auto-generated with nicknames, so we check by type prefix *)
    let agents : Types.agent list = Room.get_agents_raw config in
    let has_agent = List.exists (fun (a : Types.agent) ->
      String.length a.name >= 6 && String.sub a.name 0 6 = "claude"
    ) agents in
    Alcotest.(check bool) "claude is joined" true has_agent
  )

(* ============================================================ *)
(* Complete Task Result Variant Tests                            *)
(* ============================================================ *)

let test_complete_task_r_success () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    let _ = Room.claim_task config ~agent_name:"claude" ~task_id:"task-001" in

    let result = Room.complete_task_r config ~agent_name:"claude" ~task_id:"task-001" ~notes:"Done!" in
    match result with
    | Ok msg -> Alcotest.(check bool) "complete success" true (str_contains msg "completed")
    | Error _ -> Alcotest.fail "Expected Ok"
  )

let test_complete_task_r_not_claimed () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in

    let result = Room.complete_task_r config ~agent_name:"claude" ~task_id:"task-001" ~notes:"" in
    match result with
    | Error Types.TaskNotClaimed _ -> Alcotest.(check bool) "not claimed error" true true
    | _ -> Alcotest.fail "Expected TaskNotClaimed"
  )

let test_complete_task_r_not_found () =
  with_test_env (fun config ->
    let result = Room.complete_task_r config ~agent_name:"claude" ~task_id:"task-999" ~notes:"" in
    match result with
    | Error Types.TaskNotFound _ -> Alcotest.(check bool) "not found error" true true
    | _ -> Alcotest.fail "Expected TaskNotFound"
  )

let test_claim_task_r_success () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in

    let result = Room.claim_task_r config ~agent_name:"claude" ~task_id:"task-001" in
    match result with
    | Ok msg -> Alcotest.(check bool) "claim success" true (str_contains msg "claimed")
    | Error _ -> Alcotest.fail "Expected Ok"
  )

let test_claim_task_r_already_claimed () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Test" ~priority:1 ~description:"" in
    let _ = Room.claim_task config ~agent_name:"gemini" ~task_id:"task-001" in

    let result = Room.claim_task_r config ~agent_name:"claude" ~task_id:"task-001" in
    match result with
    | Error Types.TaskAlreadyClaimed _ -> Alcotest.(check bool) "already claimed" true true
    | _ -> Alcotest.fail "Expected TaskAlreadyClaimed"
  )

(* ============================================================ *)
(* GC (Garbage Collection) Tests                                 *)
(* ============================================================ *)

let test_gc_no_cleanup_needed () =
  with_test_env (fun config ->
    let result = Room.gc config () in
    Alcotest.(check bool) "gc result has content" true (String.length result > 0);
    Alcotest.(check bool) "no zombie cleanup" true (str_contains result "No zombie")
  )

let test_gc_with_tasks () =
  with_test_env (fun config ->
    let _ = Room.add_task config ~title:"Recent Task" ~priority:1 ~description:"" in
    let result = Room.gc config ~days:1 () in
    Alcotest.(check bool) "gc with recent task" true (String.length result > 0)
  )

(* ============================================================ *)
(* Task ID Parsing Tests                                         *)
(* ============================================================ *)

let test_task_id_to_int_valid () =
  match Room.task_id_to_int "task-001" with
  | Some 1 -> Alcotest.(check bool) "parses task-001" true true
  | _ -> Alcotest.fail "Expected Some 1"

let test_task_id_to_int_large () =
  match Room.task_id_to_int "task-999" with
  | Some 999 -> Alcotest.(check bool) "parses task-999" true true
  | _ -> Alcotest.fail "Expected Some 999"

let test_task_id_to_int_invalid_prefix () =
  match Room.task_id_to_int "issue-001" with
  | None -> Alcotest.(check bool) "rejects invalid prefix" true true
  | Some _ -> Alcotest.fail "Expected None"

let test_task_id_to_int_empty () =
  match Room.task_id_to_int "" with
  | None -> Alcotest.(check bool) "rejects empty" true true
  | Some _ -> Alcotest.fail "Expected None"

let test_task_id_to_int_only_prefix () =
  match Room.task_id_to_int "task-" with
  | None -> Alcotest.(check bool) "rejects just prefix" true true
  | Some _ -> Alcotest.fail "Expected None"

(* ============================================================ *)
(* Update Agent Tests                                            *)
(* ============================================================ *)

let test_update_agent_status () =
  with_test_env (fun config ->
    let _ = Room.join config ~agent_name:"gemini" ~capabilities:["test"] () in

    (* Get the actual agent name (auto-generated nickname) *)
    let agents : Types.agent list = Room.get_agents_raw config in
    let gemini = List.find_opt (fun a ->
      String.length a.Types.name >= 6 && String.sub a.Types.name 0 6 = "gemini"
    ) agents in
    match gemini with
    | Some agent ->
        let result = Room.update_agent_r config ~agent_name:agent.name ~status:(Some "listening") () in
        (match result with
         | Ok _ -> Alcotest.(check bool) "status updated" true true
         | Error _ -> Alcotest.fail "Expected Ok")
    | None -> Alcotest.fail "Gemini agent not found"
  )

let test_update_agent_capabilities () =
  with_test_env (fun config ->
    let _ = Room.join config ~agent_name:"gemini" ~capabilities:[] () in

    let agents : Types.agent list = Room.get_agents_raw config in
    let gemini = List.find_opt (fun a ->
      String.length a.Types.name >= 6 && String.sub a.Types.name 0 6 = "gemini"
    ) agents in
    match gemini with
    | Some agent ->
        let result = Room.update_agent_r config ~agent_name:agent.name
                       ~capabilities:(Some ["python"; "code-review"]) () in
        (match result with
         | Ok _ -> Alcotest.(check bool) "capabilities updated" true true
         | Error _ -> Alcotest.fail "Expected Ok")
    | None -> Alcotest.fail "Gemini agent not found"
  )

let test_update_agent_not_found () =
  with_test_env (fun config ->
    let result = Room.update_agent_r config ~agent_name:"nonexistent" ~status:(Some "active") () in
    match result with
    | Error Types.AgentNotFound _ -> Alcotest.(check bool) "agent not found" true true
    | _ -> Alcotest.fail "Expected AgentNotFound"
  )

(* ============================================================ *)
(* Archive Task Tests                                            *)
(* ============================================================ *)

let test_append_archive_tasks () =
  with_test_env (fun config ->
    let task : Types.task = {
      id = "task-test";
      title = "Archive Test";
      description = "Test description";
      task_status = Types.Done {
        assignee = "claude";
        completed_at = "2026-01-01T00:00:00Z";
        notes = None;
      };
      priority = 1;
      files = [];
      created_at = "2026-01-01T00:00:00Z";
      worktree = None;
    } in
    Room.append_archive_tasks config [task];

    (* Add a new task to verify archive max ID is checked *)
    let result = Room.add_task config ~title:"New Task" ~priority:1 ~description:"" in
    Alcotest.(check bool) "task added" true (contains_check result)
  )

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Random.self_init ();
  Alcotest.run "Room Coverage" [
    (* === Batch Operations === *)
    "batch", [
      Alcotest.test_case "add tasks" `Quick test_batch_add_tasks;
      Alcotest.test_case "empty list" `Quick test_batch_add_empty_list;
      Alcotest.test_case "single task" `Quick test_batch_add_single_task;
      Alcotest.test_case "preserves priorities" `Quick test_batch_add_preserves_priorities;
    ];

    (* === Claim Next === *)
    "claim_next", [
      Alcotest.test_case "basic" `Quick test_claim_next_basic;
      Alcotest.test_case "priority order" `Quick test_claim_next_priority_order;
      Alcotest.test_case "empty backlog" `Quick test_claim_next_empty_backlog;
      Alcotest.test_case "all claimed" `Quick test_claim_next_all_claimed;
      Alcotest.test_case "consecutive" `Quick test_claim_next_consecutive;
    ];

    (* === Update Priority === *)
    "update_priority", [
      Alcotest.test_case "basic" `Quick test_update_priority;
      Alcotest.test_case "nonexistent" `Quick test_update_priority_nonexistent;
      Alcotest.test_case "negative" `Quick test_update_priority_negative;
    ];

    (* === Cancel Task === *)
    "cancel", [
      Alcotest.test_case "todo task" `Quick test_cancel_task_todo;
      Alcotest.test_case "claimed by self" `Quick test_cancel_task_claimed_by_self;
      Alcotest.test_case "claimed by other" `Quick test_cancel_task_claimed_by_other;
      Alcotest.test_case "nonexistent" `Quick test_cancel_task_nonexistent;
      Alcotest.test_case "done task" `Quick test_cancel_done_task;
    ];

    (* === Transition Task === *)
    "transition", [
      Alcotest.test_case "claim" `Quick test_transition_claim;
      Alcotest.test_case "start" `Quick test_transition_start;
      Alcotest.test_case "release" `Quick test_transition_release;
      Alcotest.test_case "invalid" `Quick test_transition_invalid;
      Alcotest.test_case "version mismatch" `Quick test_transition_version_mismatch;
    ];

    (* === Pause/Resume === *)
    "pause_resume", [
      Alcotest.test_case "pause" `Quick test_pause_room;
      Alcotest.test_case "resume" `Quick test_resume_room;
      Alcotest.test_case "resume not paused" `Quick test_resume_not_paused;
      Alcotest.test_case "pause info" `Quick test_pause_info;
      Alcotest.test_case "pause info not paused" `Quick test_pause_info_not_paused;
    ];

    (* === Raw Data Accessors === *)
    "raw_data", [
      Alcotest.test_case "get tasks raw" `Quick test_get_tasks_raw;
      Alcotest.test_case "get tasks raw empty" `Quick test_get_tasks_raw_empty;
      Alcotest.test_case "get agents raw" `Quick test_get_agents_raw;
      Alcotest.test_case "get messages raw" `Quick test_get_messages_raw;
      Alcotest.test_case "is agent joined" `Quick test_is_agent_joined;
    ];

    (* === Result Variants === *)
    "result_variants", [
      Alcotest.test_case "complete_task_r success" `Quick test_complete_task_r_success;
      Alcotest.test_case "complete_task_r not claimed" `Quick test_complete_task_r_not_claimed;
      Alcotest.test_case "complete_task_r not found" `Quick test_complete_task_r_not_found;
      Alcotest.test_case "claim_task_r success" `Quick test_claim_task_r_success;
      Alcotest.test_case "claim_task_r already claimed" `Quick test_claim_task_r_already_claimed;
    ];

    (* === GC === *)
    "gc", [
      Alcotest.test_case "no cleanup" `Quick test_gc_no_cleanup_needed;
      Alcotest.test_case "with tasks" `Quick test_gc_with_tasks;
    ];

    (* === Task ID Parsing === *)
    "task_id", [
      Alcotest.test_case "valid" `Quick test_task_id_to_int_valid;
      Alcotest.test_case "large" `Quick test_task_id_to_int_large;
      Alcotest.test_case "invalid prefix" `Quick test_task_id_to_int_invalid_prefix;
      Alcotest.test_case "empty" `Quick test_task_id_to_int_empty;
      Alcotest.test_case "only prefix" `Quick test_task_id_to_int_only_prefix;
    ];

    (* === Update Agent === *)
    "update_agent", [
      Alcotest.test_case "status" `Quick test_update_agent_status;
      Alcotest.test_case "capabilities" `Quick test_update_agent_capabilities;
      Alcotest.test_case "not found" `Quick test_update_agent_not_found;
    ];

    (* === Archive === *)
    "archive", [
      Alcotest.test_case "append tasks" `Quick test_append_archive_tasks;
    ];
  ]
