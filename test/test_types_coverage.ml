(** Types Module Coverage Tests

    Tests for MASC Types - Domain Model:
    - Agent_id: of_string, to_string, equal, to_yojson, of_yojson
    - Task_id: of_string, to_string, equal, generate, to_yojson, of_yojson
    - now_iso, parse_iso8601
    - agent_status
*)

open Alcotest

module Types = Masc_mcp.Types

(* ============================================================
   Agent_id Tests
   ============================================================ *)

let test_agent_id_of_string () =
  let id = Types.Agent_id.of_string "claude-1" in
  let s = Types.Agent_id.to_string id in
  check string "roundtrip" "claude-1" s

let test_agent_id_equal_same () =
  let id1 = Types.Agent_id.of_string "agent-x" in
  let id2 = Types.Agent_id.of_string "agent-x" in
  check bool "equal" true (Types.Agent_id.equal id1 id2)

let test_agent_id_equal_diff () =
  let id1 = Types.Agent_id.of_string "agent-x" in
  let id2 = Types.Agent_id.of_string "agent-y" in
  check bool "not equal" false (Types.Agent_id.equal id1 id2)

let test_agent_id_to_yojson () =
  let id = Types.Agent_id.of_string "test-agent" in
  let json = Types.Agent_id.to_yojson id in
  match json with
  | `String s -> check string "to json" "test-agent" s
  | _ -> fail "expected String"

let test_agent_id_of_yojson_ok () =
  let json = `String "my-agent" in
  match Types.Agent_id.of_yojson json with
  | Ok id -> check string "from json" "my-agent" (Types.Agent_id.to_string id)
  | Error _ -> fail "expected Ok"

let test_agent_id_of_yojson_err () =
  let json = `Int 123 in
  match Types.Agent_id.of_yojson json with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected Error"

let test_agent_id_empty () =
  let id = Types.Agent_id.of_string "" in
  check string "empty" "" (Types.Agent_id.to_string id)

let test_agent_id_special_chars () =
  let id = Types.Agent_id.of_string "agent@domain:123" in
  check string "special chars" "agent@domain:123" (Types.Agent_id.to_string id)

(* ============================================================
   Task_id Tests
   ============================================================ *)

let test_task_id_of_string () =
  let id = Types.Task_id.of_string "task-001" in
  let s = Types.Task_id.to_string id in
  check string "roundtrip" "task-001" s

let test_task_id_equal_same () =
  let id1 = Types.Task_id.of_string "task-x" in
  let id2 = Types.Task_id.of_string "task-x" in
  check bool "equal" true (Types.Task_id.equal id1 id2)

let test_task_id_equal_diff () =
  let id1 = Types.Task_id.of_string "task-x" in
  let id2 = Types.Task_id.of_string "task-y" in
  check bool "not equal" false (Types.Task_id.equal id1 id2)

let test_task_id_generate () =
  let id = Types.Task_id.generate () in
  let s = Types.Task_id.to_string id in
  check bool "starts with task-" true (String.sub s 0 5 = "task-")

let test_task_id_generate_unique () =
  let id1 = Types.Task_id.generate () in
  let id2 = Types.Task_id.generate () in
  check bool "unique" false (Types.Task_id.equal id1 id2)

let test_task_id_to_yojson () =
  let id = Types.Task_id.of_string "task-123" in
  let json = Types.Task_id.to_yojson id in
  match json with
  | `String s -> check string "to json" "task-123" s
  | _ -> fail "expected String"

let test_task_id_of_yojson_ok () =
  let json = `String "my-task" in
  match Types.Task_id.of_yojson json with
  | Ok id -> check string "from json" "my-task" (Types.Task_id.to_string id)
  | Error _ -> fail "expected Ok"

let test_task_id_of_yojson_err () =
  let json = `Bool true in
  match Types.Task_id.of_yojson json with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected Error"

(* ============================================================
   Timestamp Tests
   ============================================================ *)

let test_now_iso_format () =
  let ts = Types.now_iso () in
  (* Should be like 2024-01-15T12:30:45Z *)
  check bool "length" true (String.length ts = 20);
  check bool "ends with Z" true (String.get ts 19 = 'Z');
  check bool "has T" true (String.get ts 10 = 'T')

let test_parse_iso8601_valid () =
  let ts = "2024-01-15T12:30:45Z" in
  let parsed = Types.parse_iso8601 ts in
  check bool "is float" true (parsed > 0.0)

let test_parse_iso8601_invalid () =
  let ts = "not-a-date" in
  let default = 123.0 in
  let parsed = Types.parse_iso8601 ~default_time:default ts in
  check (float 0.001) "uses default" default parsed

let test_parse_iso8601_empty () =
  let ts = "" in
  let default = 999.0 in
  let parsed = Types.parse_iso8601 ~default_time:default ts in
  check (float 0.001) "uses default" default parsed

(* ============================================================
   Agent Status Tests
   ============================================================ *)

let test_show_agent_status_active () =
  let s = Types.show_agent_status Types.Active in
  check bool "active" true (String.length s > 0)

let test_show_agent_status_busy () =
  let s = Types.show_agent_status Types.Busy in
  check bool "busy" true (String.length s > 0)

let test_show_agent_status_listening () =
  let s = Types.show_agent_status Types.Listening in
  check bool "listening" true (String.length s > 0)

let test_show_agent_status_inactive () =
  let s = Types.show_agent_status Types.Inactive in
  check bool "inactive" true (String.length s > 0)

(* ============================================================
   agent_status_to_string Tests
   ============================================================ *)

let test_agent_status_to_string_active () =
  check string "active" "active" (Types.agent_status_to_string Types.Active)

let test_agent_status_to_string_busy () =
  check string "busy" "busy" (Types.agent_status_to_string Types.Busy)

let test_agent_status_to_string_listening () =
  check string "listening" "listening" (Types.agent_status_to_string Types.Listening)

let test_agent_status_to_string_inactive () =
  check string "inactive" "inactive" (Types.agent_status_to_string Types.Inactive)

(* ============================================================
   agent_status_of_string_opt Tests
   ============================================================ *)

let test_agent_status_of_string_opt_active () =
  match Types.agent_status_of_string_opt "active" with
  | Some s -> check bool "is Active" true (s = Types.Active)
  | None -> fail "expected Some"

let test_agent_status_of_string_opt_busy () =
  match Types.agent_status_of_string_opt "busy" with
  | Some s -> check bool "is Busy" true (s = Types.Busy)
  | None -> fail "expected Some"

let test_agent_status_of_string_opt_listening () =
  match Types.agent_status_of_string_opt "listening" with
  | Some s -> check bool "is Listening" true (s = Types.Listening)
  | None -> fail "expected Some"

let test_agent_status_of_string_opt_inactive () =
  match Types.agent_status_of_string_opt "inactive" with
  | Some s -> check bool "is Inactive" true (s = Types.Inactive)
  | None -> fail "expected Some"

let test_agent_status_of_string_opt_unknown () =
  match Types.agent_status_of_string_opt "unknown-status" with
  | None -> check bool "is None" true true
  | Some _ -> fail "expected None"

(* ============================================================
   agent_status_of_string Tests
   ============================================================ *)

let test_agent_status_of_string_active () =
  let s = Types.agent_status_of_string "active" in
  check bool "is Active" true (s = Types.Active)

let test_agent_status_of_string_unknown_defaults () =
  (* Unknown strings default to Active *)
  let s = Types.agent_status_of_string "invalid" in
  check bool "defaults to Active" true (s = Types.Active)

(* ============================================================
   agent_status_to_yojson Tests
   ============================================================ *)

let test_agent_status_to_yojson_active () =
  match Types.agent_status_to_yojson Types.Active with
  | `String s -> check string "active json" "active" s
  | _ -> fail "expected String"

let test_agent_status_to_yojson_busy () =
  match Types.agent_status_to_yojson Types.Busy with
  | `String s -> check string "busy json" "busy" s
  | _ -> fail "expected String"

(* ============================================================
   agent_status_of_yojson Tests
   ============================================================ *)

let test_agent_status_of_yojson_active () =
  match Types.agent_status_of_yojson (`String "active") with
  | Ok s -> check bool "is Active" true (s = Types.Active)
  | Error e -> fail e

let test_agent_status_of_yojson_busy () =
  match Types.agent_status_of_yojson (`String "busy") with
  | Ok s -> check bool "is Busy" true (s = Types.Busy)
  | Error e -> fail e

let test_agent_status_of_yojson_unknown () =
  match Types.agent_status_of_yojson (`String "invalid") with
  | Error _ -> check bool "is Error" true true
  | Ok _ -> fail "expected Error"

let test_agent_status_of_yojson_wrong_type () =
  match Types.agent_status_of_yojson (`Int 123) with
  | Error _ -> check bool "is Error" true true
  | Ok _ -> fail "expected Error"

(* ============================================================
   task_status_to_string Tests
   ============================================================ *)

let test_task_status_to_string_todo () =
  check string "todo" "todo" (Types.task_status_to_string Types.Todo)

let test_task_status_to_string_claimed () =
  let status = Types.Claimed { assignee = "claude"; claimed_at = "2024-01-01" } in
  check string "claimed" "claimed" (Types.task_status_to_string status)

let test_task_status_to_string_in_progress () =
  let status = Types.InProgress { assignee = "claude"; started_at = "2024-01-01" } in
  check string "in_progress" "in_progress" (Types.task_status_to_string status)

let test_task_status_to_string_done () =
  let status = Types.Done { assignee = "claude"; completed_at = "2024-01-01"; notes = None } in
  check string "done" "done" (Types.task_status_to_string status)

let test_task_status_to_string_cancelled () =
  let status = Types.Cancelled { cancelled_by = "user"; cancelled_at = "2024-01-01"; reason = None } in
  check string "cancelled" "cancelled" (Types.task_status_to_string status)

(* ============================================================
   task_status_to_yojson Tests
   ============================================================ *)

let test_task_status_to_yojson_todo () =
  let json = Types.task_status_to_yojson Types.Todo in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "status" fields with
     | Some (`String "todo") -> ()
     | _ -> fail "expected status=todo")
  | _ -> fail "expected Assoc"

let test_task_status_to_yojson_claimed () =
  let status = Types.Claimed { assignee = "claude"; claimed_at = "2024-01-01T00:00:00Z" } in
  let json = Types.task_status_to_yojson status in
  match json with
  | `Assoc fields ->
    check bool "has status" true (List.mem_assoc "status" fields);
    check bool "has assignee" true (List.mem_assoc "assignee" fields);
    check bool "has claimed_at" true (List.mem_assoc "claimed_at" fields)
  | _ -> fail "expected Assoc"

let test_task_status_to_yojson_in_progress () =
  let status = Types.InProgress { assignee = "gemini"; started_at = "2024-01-01T12:00:00Z" } in
  let json = Types.task_status_to_yojson status in
  match json with
  | `Assoc fields ->
    check bool "has started_at" true (List.mem_assoc "started_at" fields)
  | _ -> fail "expected Assoc"

let test_task_status_to_yojson_done_with_notes () =
  let status = Types.Done { assignee = "codex"; completed_at = "2024-01-01"; notes = Some "All tests pass" } in
  let json = Types.task_status_to_yojson status in
  match json with
  | `Assoc fields ->
    check bool "has completed_at" true (List.mem_assoc "completed_at" fields);
    check bool "has notes" true (List.mem_assoc "notes" fields)
  | _ -> fail "expected Assoc"

let test_task_status_to_yojson_done_no_notes () =
  let status = Types.Done { assignee = "codex"; completed_at = "2024-01-01"; notes = None } in
  let json = Types.task_status_to_yojson status in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "notes" fields with
     | Some `Null -> ()
     | _ -> fail "expected notes=Null")
  | _ -> fail "expected Assoc"

let test_task_status_to_yojson_cancelled_with_reason () =
  let status = Types.Cancelled { cancelled_by = "user"; cancelled_at = "2024-01-01"; reason = Some "No longer needed" } in
  let json = Types.task_status_to_yojson status in
  match json with
  | `Assoc fields ->
    check bool "has cancelled_by" true (List.mem_assoc "cancelled_by" fields);
    check bool "has reason" true (List.mem_assoc "reason" fields)
  | _ -> fail "expected Assoc"

(* ============================================================
   task_status_of_yojson Tests
   ============================================================ *)

let test_task_status_of_yojson_todo () =
  let json = `Assoc [("status", `String "todo")] in
  match Types.task_status_of_yojson json with
  | Ok Types.Todo -> ()
  | Ok _ -> fail "expected Todo"
  | Error e -> fail e

let test_task_status_of_yojson_claimed () =
  let json = `Assoc [
    ("status", `String "claimed");
    ("assignee", `String "claude");
    ("claimed_at", `String "2024-01-01")
  ] in
  match Types.task_status_of_yojson json with
  | Ok (Types.Claimed { assignee; _ }) -> check string "assignee" "claude" assignee
  | Ok _ -> fail "expected Claimed"
  | Error e -> fail e

let test_task_status_of_yojson_in_progress () =
  let json = `Assoc [
    ("status", `String "in_progress");
    ("assignee", `String "gemini");
    ("started_at", `String "2024-01-01")
  ] in
  match Types.task_status_of_yojson json with
  | Ok (Types.InProgress { assignee; _ }) -> check string "assignee" "gemini" assignee
  | Ok _ -> fail "expected InProgress"
  | Error e -> fail e

let test_task_status_of_yojson_done () =
  let json = `Assoc [
    ("status", `String "done");
    ("assignee", `String "codex");
    ("completed_at", `String "2024-01-01");
    ("notes", `String "Done!")
  ] in
  match Types.task_status_of_yojson json with
  | Ok (Types.Done { notes; _ }) -> check bool "has notes" true (notes = Some "Done!")
  | Ok _ -> fail "expected Done"
  | Error e -> fail e

let test_task_status_of_yojson_cancelled () =
  let json = `Assoc [
    ("status", `String "cancelled");
    ("cancelled_by", `String "user");
    ("cancelled_at", `String "2024-01-01");
    ("reason", `Null)
  ] in
  match Types.task_status_of_yojson json with
  | Ok (Types.Cancelled { reason; _ }) -> check bool "no reason" true (reason = None)
  | Ok _ -> fail "expected Cancelled"
  | Error e -> fail e

let test_task_status_of_yojson_unknown () =
  let json = `Assoc [("status", `String "invalid_status")] in
  match Types.task_status_of_yojson json with
  | Error _ -> check bool "is Error" true true
  | Ok _ -> fail "expected Error"

(* ============================================================
   worktree_info_to_yojson Tests
   ============================================================ *)

let test_worktree_info_to_yojson () =
  let wt : Types.worktree_info = {
    branch = "feature-x";
    path = ".worktrees/feature-x";
    git_root = "/home/user/project";
    repo_name = "project";
  } in
  let json = Types.worktree_info_to_yojson wt in
  match json with
  | `Assoc fields ->
    check bool "has branch" true (List.mem_assoc "branch" fields);
    check bool "has path" true (List.mem_assoc "path" fields);
    check bool "has git_root" true (List.mem_assoc "git_root" fields);
    check bool "has repo_name" true (List.mem_assoc "repo_name" fields)
  | _ -> fail "expected Assoc"

(* ============================================================
   worktree_info_of_yojson Tests
   ============================================================ *)

let test_worktree_info_of_yojson () =
  let json = `Assoc [
    ("branch", `String "main");
    ("path", `String ".worktrees/main");
    ("git_root", `String "/repo");
    ("repo_name", `String "repo");
  ] in
  match Types.worktree_info_of_yojson json with
  | Ok wt -> check string "branch" "main" wt.branch
  | Error e -> fail e

let test_worktree_info_of_yojson_missing_field () =
  let json = `Assoc [("branch", `String "main")] in
  match Types.worktree_info_of_yojson json with
  | Error _ -> check bool "is Error" true true
  | Ok _ -> fail "expected Error"

(* ============================================================
   show_task_status Tests
   ============================================================ *)

let test_show_task_status_todo () =
  let s = Types.show_task_status Types.Todo in
  check bool "non-empty" true (String.length s > 0)

let test_show_task_status_claimed () =
  let status = Types.Claimed { assignee = "claude"; claimed_at = "2024-01-01" } in
  let s = Types.show_task_status status in
  check bool "contains assignee" true
    (try let _ = Str.search_forward (Str.regexp "claude") s 0 in true
     with Not_found -> false)

(* ============================================================
   string_of_agent_status Tests
   ============================================================ *)

let test_string_of_agent_status () =
  (* string_of_agent_status is alias for agent_status_to_string *)
  check string "alias works" "active" (Types.string_of_agent_status Types.Active)

(* ============================================================
   string_of_task_status Tests
   ============================================================ *)

let test_string_of_task_status () =
  (* string_of_task_status is alias for task_status_to_string *)
  check string "alias works" "todo" (Types.string_of_task_status Types.Todo)

(* ============================================================
   tempo_mode Tests
   ============================================================ *)

let test_tempo_mode_to_string_normal () =
  check string "normal" "normal" (Types.tempo_mode_to_string Types.Normal)

let test_tempo_mode_to_string_slow () =
  check string "slow" "slow" (Types.tempo_mode_to_string Types.Slow)

let test_tempo_mode_to_string_fast () =
  check string "fast" "fast" (Types.tempo_mode_to_string Types.Fast)

let test_tempo_mode_to_string_paused () =
  check string "paused" "paused" (Types.tempo_mode_to_string Types.Paused)

let test_string_of_tempo_mode () =
  (* Alias for tempo_mode_to_string *)
  check string "alias" "normal" (Types.string_of_tempo_mode Types.Normal)

let test_tempo_mode_of_string_normal () =
  match Types.tempo_mode_of_string "normal" with
  | Ok Types.Normal -> ()
  | _ -> fail "expected Ok Normal"

let test_tempo_mode_of_string_slow () =
  match Types.tempo_mode_of_string "slow" with
  | Ok Types.Slow -> ()
  | _ -> fail "expected Ok Slow"

let test_tempo_mode_of_string_fast () =
  match Types.tempo_mode_of_string "fast" with
  | Ok Types.Fast -> ()
  | _ -> fail "expected Ok Fast"

let test_tempo_mode_of_string_paused () =
  match Types.tempo_mode_of_string "paused" with
  | Ok Types.Paused -> ()
  | _ -> fail "expected Ok Paused"

let test_tempo_mode_of_string_unknown () =
  match Types.tempo_mode_of_string "invalid" with
  | Error e -> check bool "has error msg" true (String.length e > 0)
  | Ok _ -> fail "expected Error"

let test_tempo_mode_to_yojson_normal () =
  match Types.tempo_mode_to_yojson Types.Normal with
  | `String "normal" -> ()
  | _ -> fail "expected String normal"

let test_tempo_mode_to_yojson_paused () =
  match Types.tempo_mode_to_yojson Types.Paused with
  | `String "paused" -> ()
  | _ -> fail "expected String paused"

let test_tempo_mode_of_yojson_ok () =
  match Types.tempo_mode_of_yojson (`String "slow") with
  | Ok Types.Slow -> ()
  | _ -> fail "expected Ok Slow"

let test_tempo_mode_of_yojson_unknown () =
  match Types.tempo_mode_of_yojson (`String "xyz") with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

let test_tempo_mode_of_yojson_wrong_type () =
  match Types.tempo_mode_of_yojson (`Int 42) with
  | Error e -> check bool "has error" true (String.length e > 0)
  | Ok _ -> fail "expected Error"

(* ============================================================
   backlog Tests
   ============================================================ *)

let test_backlog_to_yojson_empty () =
  let b : Types.backlog = { tasks = []; last_updated = "2024-01-15T12:00:00Z"; version = 1 } in
  let json = Types.backlog_to_yojson b in
  match json with
  | `Assoc fields ->
    check bool "has tasks" true (List.mem_assoc "tasks" fields);
    check bool "has last_updated" true (List.mem_assoc "last_updated" fields);
    check bool "has version" true (List.mem_assoc "version" fields)
  | _ -> fail "expected Assoc"

let test_backlog_to_yojson_with_tasks () =
  let task : Types.task = {
    id = "t1";
    title = "Test task";
    description = "Test description";
    task_status = Types.Todo;
    priority = 1;
    files = [];
    created_at = "2024-01-15T12:00:00Z";
    worktree = None;
  } in
  let b : Types.backlog = { tasks = [task]; last_updated = "2024-01-15T12:00:00Z"; version = 2 } in
  let json = Types.backlog_to_yojson b in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "tasks" fields with
     | Some (`List [_]) -> ()
     | _ -> fail "expected tasks list with 1 item");
    (match List.assoc_opt "version" fields with
     | Some (`Int 2) -> ()
     | _ -> fail "expected version 2")
  | _ -> fail "expected Assoc"

let test_backlog_of_yojson_ok () =
  let json = `Assoc [
    ("tasks", `List []);
    ("last_updated", `String "2024-01-15T12:00:00Z");
    ("version", `Int 3);
  ] in
  match Types.backlog_of_yojson json with
  | Ok b ->
    check int "tasks empty" 0 (List.length b.tasks);
    check int "version" 3 b.version
  | Error e -> fail ("expected Ok, got: " ^ e)

let test_backlog_of_yojson_with_task () =
  let task_json = `Assoc [
    ("id", `String "task-1");
    ("title", `String "Test");
    ("description", `String "Description");
    ("status", `String "todo");
    ("priority", `Int 1);
    ("files", `List []);
    ("created_at", `String "2024-01-15T12:00:00Z");
  ] in
  let json = `Assoc [
    ("tasks", `List [task_json]);
    ("last_updated", `String "2024-01-15T12:00:00Z");
    ("version", `Int 1);
  ] in
  match Types.backlog_of_yojson json with
  | Ok b -> check int "has 1 task" 1 (List.length b.tasks)
  | Error e -> fail ("expected Ok, got: " ^ e)

let test_backlog_of_yojson_error () =
  let json = `String "not an object" in
  match Types.backlog_of_yojson json with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

(* ============================================================
   a2a_task_status Tests
   ============================================================ *)

let test_a2a_task_status_to_string_pending () =
  check string "pending" "pending" (Types.a2a_task_status_to_string Types.A2APending)

let test_a2a_task_status_to_string_running () =
  check string "running" "running" (Types.a2a_task_status_to_string Types.A2ARunning)

let test_a2a_task_status_to_string_completed () =
  check string "completed" "completed" (Types.a2a_task_status_to_string Types.A2ACompleted)

let test_a2a_task_status_to_string_failed () =
  check string "failed" "failed" (Types.a2a_task_status_to_string Types.A2AFailed)

let test_a2a_task_status_to_string_canceled () =
  check string "canceled" "canceled" (Types.a2a_task_status_to_string Types.A2ACanceled)

let test_a2a_task_status_of_string_pending () =
  match Types.a2a_task_status_of_string "pending" with
  | Ok Types.A2APending -> ()
  | _ -> fail "expected Ok A2APending"

let test_a2a_task_status_of_string_running () =
  match Types.a2a_task_status_of_string "running" with
  | Ok Types.A2ARunning -> ()
  | _ -> fail "expected Ok A2ARunning"

let test_a2a_task_status_of_string_completed () =
  match Types.a2a_task_status_of_string "completed" with
  | Ok Types.A2ACompleted -> ()
  | _ -> fail "expected Ok A2ACompleted"

let test_a2a_task_status_of_string_failed () =
  match Types.a2a_task_status_of_string "failed" with
  | Ok Types.A2AFailed -> ()
  | _ -> fail "expected Ok A2AFailed"

let test_a2a_task_status_of_string_canceled () =
  match Types.a2a_task_status_of_string "canceled" with
  | Ok Types.A2ACanceled -> ()
  | _ -> fail "expected Ok A2ACanceled"

let test_a2a_task_status_of_string_unknown () =
  match Types.a2a_task_status_of_string "invalid" with
  | Error e -> check bool "has error msg" true (String.length e > 0)
  | Ok _ -> fail "expected Error"

let test_a2a_task_status_to_yojson_pending () =
  match Types.a2a_task_status_to_yojson Types.A2APending with
  | `String "pending" -> ()
  | _ -> fail "expected String pending"

let test_a2a_task_status_to_yojson_completed () =
  match Types.a2a_task_status_to_yojson Types.A2ACompleted with
  | `String "completed" -> ()
  | _ -> fail "expected String completed"

let test_a2a_task_status_of_yojson_ok () =
  match Types.a2a_task_status_of_yojson (`String "running") with
  | Ok Types.A2ARunning -> ()
  | _ -> fail "expected Ok A2ARunning"

let test_a2a_task_status_of_yojson_unknown () =
  match Types.a2a_task_status_of_yojson (`String "xyz") with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

let test_a2a_task_status_of_yojson_wrong_type () =
  match Types.a2a_task_status_of_yojson (`Int 42) with
  | Error e -> check bool "has error" true (String.length e > 0)
  | Ok _ -> fail "expected Error"

(* ============================================================
   portal_state Tests
   ============================================================ *)

let test_portal_state_to_string_open () =
  check string "open" "open" (Types.portal_state_to_string Types.PortalOpen)

let test_portal_state_to_string_closed () =
  check string "closed" "closed" (Types.portal_state_to_string Types.PortalClosed)

let test_portal_state_of_string_open () =
  match Types.portal_state_of_string "open" with
  | Ok Types.PortalOpen -> ()
  | _ -> fail "expected Ok PortalOpen"

let test_portal_state_of_string_closed () =
  match Types.portal_state_of_string "closed" with
  | Ok Types.PortalClosed -> ()
  | _ -> fail "expected Ok PortalClosed"

let test_portal_state_of_string_unknown () =
  match Types.portal_state_of_string "invalid" with
  | Error e -> check bool "has error" true (String.length e > 0)
  | Ok _ -> fail "expected Error"

let test_portal_state_to_yojson () =
  match Types.portal_state_to_yojson Types.PortalOpen with
  | `String "open" -> ()
  | _ -> fail "expected String open"

let test_portal_state_of_yojson_ok () =
  match Types.portal_state_of_yojson (`String "closed") with
  | Ok Types.PortalClosed -> ()
  | _ -> fail "expected Ok PortalClosed"

let test_portal_state_of_yojson_wrong_type () =
  match Types.portal_state_of_yojson (`Int 1) with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

(* ============================================================
   masc_error_to_string Tests
   ============================================================ *)

let test_masc_error_not_initialized () =
  let s = Types.masc_error_to_string Types.NotInitialized in
  check bool "contains not initialized" true (String.length s > 0)

let test_masc_error_already_initialized () =
  let s = Types.masc_error_to_string Types.AlreadyInitialized in
  check bool "contains already" true (String.length s > 0)

let test_masc_error_agent_not_found () =
  let s = Types.masc_error_to_string (Types.AgentNotFound "claude") in
  check bool "contains claude" true
    (try let _ = Str.search_forward (Str.regexp "claude") s 0 in true
     with Not_found -> false)

let test_masc_error_task_not_found () =
  let s = Types.masc_error_to_string (Types.TaskNotFound "task-1") in
  check bool "contains task-1" true
    (try let _ = Str.search_forward (Str.regexp "task-1") s 0 in true
     with Not_found -> false)

let test_masc_error_task_already_claimed () =
  let s = Types.masc_error_to_string (Types.TaskAlreadyClaimed { task_id = "t1"; by = "agent" }) in
  check bool "contains both" true (String.length s > 0)

let test_masc_error_file_locked () =
  let s = Types.masc_error_to_string (Types.FileLocked { file = "f.txt"; by = "agent" }) in
  check bool "contains file" true (String.length s > 0)

let test_masc_error_rate_limit () =
  let s = Types.masc_error_to_string
    (Types.RateLimitExceeded { limit = 100; current = 101; wait_seconds = 5; category = Types.GeneralLimit }) in
  check bool "contains limit" true (String.length s > 0)

(* ============================================================
   agent_role Tests
   ============================================================ *)

let test_agent_role_to_string_reader () =
  check string "reader" "reader" (Types.agent_role_to_string Types.Reader)

let test_agent_role_to_string_worker () =
  check string "worker" "worker" (Types.agent_role_to_string Types.Worker)

let test_agent_role_to_string_admin () =
  check string "admin" "admin" (Types.agent_role_to_string Types.Admin)

let test_agent_role_of_string_reader () =
  match Types.agent_role_of_string "reader" with
  | Ok Types.Reader -> ()
  | _ -> fail "expected Ok Reader"

let test_agent_role_of_string_worker () =
  match Types.agent_role_of_string "worker" with
  | Ok Types.Worker -> ()
  | _ -> fail "expected Ok Worker"

let test_agent_role_of_string_admin () =
  match Types.agent_role_of_string "admin" with
  | Ok Types.Admin -> ()
  | _ -> fail "expected Ok Admin"

let test_agent_role_of_string_unknown () =
  match Types.agent_role_of_string "superuser" with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

let test_agent_role_to_yojson () =
  match Types.agent_role_to_yojson Types.Worker with
  | `String "worker" -> ()
  | _ -> fail "expected String worker"

let test_agent_role_of_yojson_ok () =
  match Types.agent_role_of_yojson (`String "admin") with
  | Ok Types.Admin -> ()
  | _ -> fail "expected Ok Admin"

let test_agent_role_of_yojson_wrong_type () =
  match Types.agent_role_of_yojson (`Int 1) with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

(* ============================================================
   agent_credential Tests
   ============================================================ *)

let test_agent_credential_to_yojson () =
  let cred : Types.agent_credential = {
    agent_name = "claude";
    token = "abc123";
    role = Types.Worker;
    created_at = "2024-01-15T12:00:00Z";
    expires_at = None;
  } in
  let json = Types.agent_credential_to_yojson cred in
  match json with
  | `Assoc fields ->
    check bool "has agent_name" true (List.mem_assoc "agent_name" fields);
    check bool "has token" true (List.mem_assoc "token" fields);
    check bool "has role" true (List.mem_assoc "role" fields)
  | _ -> fail "expected Assoc"

let test_agent_credential_to_yojson_with_expiry () =
  let cred : Types.agent_credential = {
    agent_name = "claude";
    token = "abc123";
    role = Types.Admin;
    created_at = "2024-01-15T12:00:00Z";
    expires_at = Some "2024-01-16T12:00:00Z";
  } in
  let json = Types.agent_credential_to_yojson cred in
  match json with
  | `Assoc fields -> check bool "has expires_at" true (List.mem_assoc "expires_at" fields)
  | _ -> fail "expected Assoc"

let test_agent_credential_of_yojson_ok () =
  let json = `Assoc [
    ("agent_name", `String "gemini");
    ("token", `String "xyz");
    ("role", `String "reader");
    ("created_at", `String "2024-01-15T12:00:00Z");
  ] in
  match Types.agent_credential_of_yojson json with
  | Ok cred ->
    check string "agent_name" "gemini" cred.agent_name;
    check bool "is reader" true (cred.role = Types.Reader)
  | Error e -> fail ("expected Ok, got: " ^ e)

let test_agent_credential_of_yojson_error () =
  let json = `String "not an object" in
  match Types.agent_credential_of_yojson json with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

(* ============================================================
   auth_config Tests
   ============================================================ *)

let test_auth_config_to_yojson () =
  let config = Types.default_auth_config in
  let json = Types.auth_config_to_yojson config in
  match json with
  | `Assoc fields ->
    check bool "has enabled" true (List.mem_assoc "enabled" fields);
    check bool "has default_role" true (List.mem_assoc "default_role" fields)
  | _ -> fail "expected Assoc"

let test_auth_config_of_yojson_ok () =
  let json = `Assoc [
    ("enabled", `Bool true);
    ("room_secret_hash", `Null);
    ("require_token", `Bool false);
    ("default_role", `String "admin");
    ("token_expiry_hours", `Int 48);
  ] in
  match Types.auth_config_of_yojson json with
  | Ok config ->
    check bool "enabled" true config.enabled;
    check int "expiry" 48 config.token_expiry_hours
  | Error e -> fail ("expected Ok, got: " ^ e)

let test_auth_config_of_yojson_error () =
  let json = `Int 42 in
  match Types.auth_config_of_yojson json with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

(* ============================================================
   permissions Tests
   ============================================================ *)

let test_permissions_for_role_reader () =
  let perms = Types.permissions_for_role Types.Reader in
  check bool "has CanReadState" true (List.mem Types.CanReadState perms);
  check bool "no CanInit" false (List.mem Types.CanInit perms)

let test_permissions_for_role_worker () =
  let perms = Types.permissions_for_role Types.Worker in
  check bool "has CanClaimTask" true (List.mem Types.CanClaimTask perms);
  check bool "has CanBroadcast" true (List.mem Types.CanBroadcast perms)

let test_permissions_for_role_admin () =
  let perms = Types.permissions_for_role Types.Admin in
  check bool "has CanInit" true (List.mem Types.CanInit perms);
  check bool "has CanReset" true (List.mem Types.CanReset perms)

let test_has_permission_reader () =
  check bool "reader can read" true (Types.has_permission Types.Reader Types.CanReadState);
  check bool "reader cannot init" false (Types.has_permission Types.Reader Types.CanInit)

let test_has_permission_worker () =
  check bool "worker can broadcast" true (Types.has_permission Types.Worker Types.CanBroadcast);
  check bool "worker cannot reset" false (Types.has_permission Types.Worker Types.CanReset)

let test_has_permission_admin () =
  check bool "admin can init" true (Types.has_permission Types.Admin Types.CanInit);
  check bool "admin can approve" true (Types.has_permission Types.Admin Types.CanApprove)

(* ============================================================
   rate_limit role integration Tests
   ============================================================ *)

let test_multiplier_for_role () =
  let config : Types.rate_limit_config = {
    per_minute = 60;
    burst_allowed = 10;
    priority_agents = [];
    reader_multiplier = 0.5;
    worker_multiplier = 1.0;
    admin_multiplier = 2.0;
    broadcast_per_minute = 30;
    task_ops_per_minute = 100;
  } in
  check (float 0.01) "reader mult" 0.5 (Types.multiplier_for_role config Types.Reader);
  check (float 0.01) "worker mult" 1.0 (Types.multiplier_for_role config Types.Worker);
  check (float 0.01) "admin mult" 2.0 (Types.multiplier_for_role config Types.Admin)

let test_effective_limit () =
  let config : Types.rate_limit_config = {
    per_minute = 60;
    burst_allowed = 10;
    priority_agents = [];
    reader_multiplier = 0.5;
    worker_multiplier = 1.0;
    admin_multiplier = 2.0;
    broadcast_per_minute = 30;
    task_ops_per_minute = 100;
  } in
  check int "reader general" 30 (Types.effective_limit config ~role:Types.Reader ~category:Types.GeneralLimit);
  check int "admin general" 120 (Types.effective_limit config ~role:Types.Admin ~category:Types.GeneralLimit)

(* ============================================================
   limit_for_category Tests
   ============================================================ *)

let test_limit_for_category_general () =
  let config : Types.rate_limit_config = {
    per_minute = 60;
    burst_allowed = 10;
    priority_agents = [];
    reader_multiplier = 1.0;
    worker_multiplier = 1.0;
    admin_multiplier = 1.0;
    broadcast_per_minute = 30;
    task_ops_per_minute = 100;
  } in
  check int "general" 60 (Types.limit_for_category config Types.GeneralLimit)

let test_limit_for_category_broadcast () =
  let config : Types.rate_limit_config = {
    per_minute = 60;
    burst_allowed = 10;
    priority_agents = [];
    reader_multiplier = 1.0;
    worker_multiplier = 1.0;
    admin_multiplier = 1.0;
    broadcast_per_minute = 30;
    task_ops_per_minute = 100;
  } in
  check int "broadcast" 30 (Types.limit_for_category config Types.BroadcastLimit)

let test_limit_for_category_task_ops () =
  let config : Types.rate_limit_config = {
    per_minute = 60;
    burst_allowed = 10;
    priority_agents = [];
    reader_multiplier = 1.0;
    worker_multiplier = 1.0;
    admin_multiplier = 1.0;
    broadcast_per_minute = 30;
    task_ops_per_minute = 100;
  } in
  check int "task_ops" 100 (Types.limit_for_category config Types.TaskOpsLimit)

(* ============================================================
   category_for_tool Tests
   ============================================================ *)

let test_category_for_tool_broadcast () =
  check bool "masc_broadcast" true (Types.category_for_tool "masc_broadcast" = Types.BroadcastLimit);
  check bool "masc_listen" true (Types.category_for_tool "masc_listen" = Types.BroadcastLimit)

let test_category_for_tool_task_ops () =
  check bool "masc_add_task" true (Types.category_for_tool "masc_add_task" = Types.TaskOpsLimit);
  check bool "masc_claim" true (Types.category_for_tool "masc_claim" = Types.TaskOpsLimit);
  check bool "masc_done" true (Types.category_for_tool "masc_done" = Types.TaskOpsLimit);
  check bool "masc_transition" true (Types.category_for_tool "masc_transition" = Types.TaskOpsLimit)

let test_category_for_tool_general () =
  check bool "masc_status" true (Types.category_for_tool "masc_status" = Types.GeneralLimit);
  check bool "unknown" true (Types.category_for_tool "unknown_tool" = Types.GeneralLimit)

(* ============================================================
   more masc_error_to_string Tests (coverage for all variants)
   ============================================================ *)

let test_masc_error_agent_already_joined () =
  let s = Types.masc_error_to_string (Types.AgentAlreadyJoined "agent1") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_task_not_claimed () =
  let s = Types.masc_error_to_string (Types.TaskNotClaimed "t1") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_task_invalid_state () =
  let s = Types.masc_error_to_string (Types.TaskInvalidState "cancelled") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_file_not_locked () =
  let s = Types.masc_error_to_string (Types.FileNotLocked "file.txt") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_portal_not_open () =
  let s = Types.masc_error_to_string (Types.PortalNotOpen "agent") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_portal_already_open () =
  let s = Types.masc_error_to_string (Types.PortalAlreadyOpen { agent = "a1"; target = "a2" }) in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_portal_closed () =
  let s = Types.masc_error_to_string (Types.PortalClosed "agent") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_invalid_json () =
  let s = Types.masc_error_to_string (Types.InvalidJson "bad json") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_io_error () =
  let s = Types.masc_error_to_string (Types.IoError "read failed") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_invalid_agent_name () =
  let s = Types.masc_error_to_string (Types.InvalidAgentName "bad name") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_invalid_task_id () =
  let s = Types.masc_error_to_string (Types.InvalidTaskId "bad id") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_invalid_file_path () =
  let s = Types.masc_error_to_string (Types.InvalidFilePath "bad path") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_unauthorized () =
  let s = Types.masc_error_to_string (Types.Unauthorized "missing token") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_forbidden () =
  let s = Types.masc_error_to_string (Types.Forbidden { agent = "a1"; action = "reset" }) in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_token_expired () =
  let s = Types.masc_error_to_string (Types.TokenExpired "agent") in
  check bool "nonempty" true (String.length s > 0)

let test_masc_error_invalid_token () =
  let s = Types.masc_error_to_string (Types.InvalidToken "bad token") in
  check bool "nonempty" true (String.length s > 0)

(* ============================================================
   default_tempo_config Tests
   ============================================================ *)

let test_default_tempo_config_mode () =
  let c = Types.default_tempo_config in
  check string "mode normal" "normal" (Types.tempo_mode_to_string c.mode)

let test_default_tempo_config_delay () =
  let c = Types.default_tempo_config in
  check int "delay_ms" 0 c.delay_ms

let test_default_tempo_config_reason () =
  let c = Types.default_tempo_config in
  check (option string) "reason none" None c.reason

(* ============================================================
   tempo_config_to/of_yojson Tests
   ============================================================ *)

let test_tempo_config_to_yojson () =
  let c : Types.tempo_config = {
    mode = Types.Slow;
    delay_ms = 100;
    reason = Some "testing";
    set_by = Some "claude";
    set_at = Some "2024-01-15T12:00:00Z";
  } in
  let json = Types.tempo_config_to_yojson c in
  match json with
  | `Assoc fields ->
    check bool "has mode" true (List.mem_assoc "mode" fields);
    check bool "has delay_ms" true (List.mem_assoc "delay_ms" fields)
  | _ -> fail "expected Assoc"

let test_tempo_config_of_yojson_ok () =
  let json = `Assoc [
    ("mode", `String "fast");
    ("delay_ms", `Int 50);
    ("reason", `String "quick work");
    ("set_by", `Null);
    ("set_at", `Null);
  ] in
  match Types.tempo_config_of_yojson json with
  | Ok c ->
    check string "mode" "fast" (Types.tempo_mode_to_string c.mode);
    check int "delay_ms" 50 c.delay_ms
  | Error e -> fail ("expected Ok: " ^ e)

let test_tempo_config_of_yojson_error () =
  let json = `Assoc [("mode", `String "invalid_mode")] in
  match Types.tempo_config_of_yojson json with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

(* ============================================================
   default_rate_limit Tests
   ============================================================ *)

let test_default_rate_limit_per_minute () =
  let c = Types.default_rate_limit in
  check int "per_minute" 10 c.per_minute

let test_default_rate_limit_burst () =
  let c = Types.default_rate_limit in
  check int "burst_allowed" 5 c.burst_allowed

let test_default_rate_limit_multipliers () =
  let c = Types.default_rate_limit in
  check (float 0.01) "reader" 0.5 c.reader_multiplier;
  check (float 0.01) "worker" 1.0 c.worker_multiplier;
  check (float 0.01) "admin" 2.0 c.admin_multiplier

(* ============================================================
   rate_limit_config_to/of_yojson Tests
   ============================================================ *)

let test_rate_limit_config_to_yojson () =
  let c : Types.rate_limit_config = {
    per_minute = 60;
    burst_allowed = 10;
    priority_agents = ["claude"; "gemini"];
    reader_multiplier = 0.5;
    worker_multiplier = 1.0;
    admin_multiplier = 2.0;
    broadcast_per_minute = 30;
    task_ops_per_minute = 100;
  } in
  let json = Types.rate_limit_config_to_yojson c in
  match json with
  | `Assoc fields ->
    check bool "has per_minute" true (List.mem_assoc "per_minute" fields);
    check bool "has priority_agents" true (List.mem_assoc "priority_agents" fields)
  | _ -> fail "expected Assoc"

let test_rate_limit_config_of_yojson_ok () =
  let json = `Assoc [
    ("per_minute", `Int 120);
    ("burst_allowed", `Int 20);
    ("priority_agents", `List [`String "admin"]);
    ("reader_multiplier", `Float 0.25);
    ("worker_multiplier", `Float 1.5);
    ("admin_multiplier", `Float 3.0);
    ("broadcast_per_minute", `Int 60);
    ("task_ops_per_minute", `Int 200);
  ] in
  match Types.rate_limit_config_of_yojson json with
  | Ok c ->
    check int "per_minute" 120 c.per_minute;
    check int "broadcast_per_minute" 60 c.broadcast_per_minute
  | Error e -> fail ("expected Ok: " ^ e)

let test_rate_limit_config_of_yojson_defaults () =
  let json = `Assoc [] in
  match Types.rate_limit_config_of_yojson json with
  | Ok c ->
    check int "default per_minute" 10 c.per_minute
  | Error e -> fail ("expected Ok with defaults: " ^ e)

(* ============================================================
   tool_result_to_yojson Tests
   ============================================================ *)

let test_tool_result_to_yojson_success () =
  let r : Types.tool_result = {
    success = true;
    message = "Operation completed";
    data = None;
  } in
  let json = Types.tool_result_to_yojson r in
  match json with
  | `Assoc fields ->
    check bool "has success" true (List.mem_assoc "success" fields);
    check bool "has message" true (List.mem_assoc "message" fields)
  | _ -> fail "expected Assoc"

let test_tool_result_to_yojson_with_data () =
  let r : Types.tool_result = {
    success = false;
    message = "Error occurred";
    data = Some (`Assoc [("error_code", `Int 500)]);
  } in
  let json = Types.tool_result_to_yojson r in
  match json with
  | `Assoc fields ->
    check bool "has data" true (List.mem_assoc "data" fields)
  | _ -> fail "expected Assoc"

(* ============================================================
   task_to/of_yojson Tests
   ============================================================ *)

let test_task_to_yojson () =
  let t : Types.task = {
    id = "task-001";
    title = "Test Task";
    description = "A test task";
    task_status = Types.Todo;
    priority = 2;
    files = ["file1.ml"; "file2.ml"];
    created_at = "2024-01-15T12:00:00Z";
    worktree = None;
  } in
  let json = Types.task_to_yojson t in
  match json with
  | `Assoc fields ->
    check bool "has id" true (List.mem_assoc "id" fields);
    check bool "has title" true (List.mem_assoc "title" fields);
    check bool "has files" true (List.mem_assoc "files" fields)
  | _ -> fail "expected Assoc"

let test_task_to_yojson_with_worktree () =
  let wt : Types.worktree_info = {
    branch = "feature";
    path = ".worktrees/feature";
    git_root = "/repo";
    repo_name = "repo";
  } in
  let t : Types.task = {
    id = "task-002";
    title = "Task with Worktree";
    description = "";
    task_status = Types.InProgress { assignee = "claude"; started_at = "2024-01-15T12:00:00Z" };
    priority = 1;
    files = [];
    created_at = "2024-01-15T12:00:00Z";
    worktree = Some wt;
  } in
  let json = Types.task_to_yojson t in
  match json with
  | `Assoc fields ->
    check bool "has worktree" true (List.mem_assoc "worktree" fields)
  | _ -> fail "expected Assoc"

let test_task_of_yojson_ok () =
  let json = `Assoc [
    ("id", `String "task-003");
    ("title", `String "Parse Task");
    ("description", `String "desc");
    ("status", `String "todo");
    ("priority", `Int 3);
    ("files", `List [`String "a.ml"]);
    ("created_at", `String "2024-01-15T12:00:00Z");
  ] in
  match Types.task_of_yojson json with
  | Ok t ->
    check string "id" "task-003" t.id;
    check string "title" "Parse Task" t.title
  | Error e -> fail ("expected Ok: " ^ e)

let test_task_of_yojson_error () =
  let json = `Assoc [("id", `String "bad")] in
  match Types.task_of_yojson json with
  | Error _ -> ()
  | Ok _ -> fail "expected Error (missing title)"

(* ============================================================
   a2a_task_to/of_yojson Tests
   ============================================================ *)

let test_a2a_task_to_yojson () =
  let t : Types.a2a_task = {
    a2a_id = "a2a-001";
    from_agent = "claude";
    to_agent = "gemini";
    a2a_message = "Please review this";
    a2a_status = Types.A2APending;
    a2a_result = None;
    created_at = "2024-01-15T12:00:00Z";
    updated_at = "2024-01-15T12:00:00Z";
  } in
  let json = Types.a2a_task_to_yojson t in
  match json with
  | `Assoc fields ->
    check bool "has id" true (List.mem_assoc "id" fields);
    check bool "has from" true (List.mem_assoc "from" fields);
    check bool "has to" true (List.mem_assoc "to" fields)
  | _ -> fail "expected Assoc"

let test_a2a_task_to_yojson_with_result () =
  let t : Types.a2a_task = {
    a2a_id = "a2a-002";
    from_agent = "gemini";
    to_agent = "claude";
    a2a_message = "Task completed";
    a2a_status = Types.A2ACompleted;
    a2a_result = Some "Done successfully";
    created_at = "2024-01-15T12:00:00Z";
    updated_at = "2024-01-15T13:00:00Z";
  } in
  let json = Types.a2a_task_to_yojson t in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "result" fields with
     | Some (`String _) -> ()
     | _ -> fail "expected string result")
  | _ -> fail "expected Assoc"

let test_a2a_task_of_yojson_ok () =
  let json = `Assoc [
    ("id", `String "a2a-003");
    ("from", `String "ollama");
    ("to", `String "claude");
    ("message", `String "Help needed");
    ("status", `String "running");
    ("result", `Null);
    ("createdAt", `String "2024-01-15T12:00:00Z");
    ("updatedAt", `String "2024-01-15T12:00:00Z");
  ] in
  match Types.a2a_task_of_yojson json with
  | Ok t ->
    check string "id" "a2a-003" t.a2a_id;
    check string "from" "ollama" t.from_agent
  | Error e -> fail ("expected Ok: " ^ e)

let test_a2a_task_of_yojson_error () =
  let json = `Assoc [("id", `String "bad")] in
  match Types.a2a_task_of_yojson json with
  | Error _ -> ()
  | Ok _ -> fail "expected Error (missing fields)"

(* ============================================================
   portal_to/of_yojson Tests
   ============================================================ *)

let test_portal_to_yojson () =
  let p : Types.portal = {
    portal_from = "claude";
    portal_target = "gemini";
    portal_opened_at = "2024-01-15T12:00:00Z";
    portal_status = Types.PortalOpen;
    task_count = 5;
  } in
  let json = Types.portal_to_yojson p in
  match json with
  | `Assoc fields ->
    check bool "has from" true (List.mem_assoc "from" fields);
    check bool "has target" true (List.mem_assoc "target" fields);
    check bool "has taskCount" true (List.mem_assoc "taskCount" fields)
  | _ -> fail "expected Assoc"

let test_portal_of_yojson_ok () =
  let json = `Assoc [
    ("from", `String "ollama");
    ("target", `String "codex");
    ("openedAt", `String "2024-01-15T12:00:00Z");
    ("status", `String "open");
    ("taskCount", `Int 3);
  ] in
  match Types.portal_of_yojson json with
  | Ok p ->
    check string "from" "ollama" p.portal_from;
    check int "task_count" 3 p.task_count
  | Error e -> fail ("expected Ok: " ^ e)

let test_portal_of_yojson_closed () =
  let json = `Assoc [
    ("from", `String "a");
    ("target", `String "b");
    ("openedAt", `String "2024-01-15T12:00:00Z");
    ("status", `String "closed");
    ("taskCount", `Int 0);
  ] in
  match Types.portal_of_yojson json with
  | Ok p ->
    check string "status" "closed" (Types.portal_state_to_string p.portal_status)
  | Error e -> fail ("expected Ok: " ^ e)

let test_portal_of_yojson_error () =
  let json = `Assoc [("from", `String "x")] in
  match Types.portal_of_yojson json with
  | Error _ -> ()
  | Ok _ -> fail "expected Error (missing fields)"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Types Coverage" [
    "agent_id", [
      test_case "of_string" `Quick test_agent_id_of_string;
      test_case "equal same" `Quick test_agent_id_equal_same;
      test_case "equal diff" `Quick test_agent_id_equal_diff;
      test_case "to_yojson" `Quick test_agent_id_to_yojson;
      test_case "of_yojson ok" `Quick test_agent_id_of_yojson_ok;
      test_case "of_yojson err" `Quick test_agent_id_of_yojson_err;
      test_case "empty" `Quick test_agent_id_empty;
      test_case "special chars" `Quick test_agent_id_special_chars;
    ];
    "task_id", [
      test_case "of_string" `Quick test_task_id_of_string;
      test_case "equal same" `Quick test_task_id_equal_same;
      test_case "equal diff" `Quick test_task_id_equal_diff;
      test_case "generate" `Quick test_task_id_generate;
      test_case "generate unique" `Quick test_task_id_generate_unique;
      test_case "to_yojson" `Quick test_task_id_to_yojson;
      test_case "of_yojson ok" `Quick test_task_id_of_yojson_ok;
      test_case "of_yojson err" `Quick test_task_id_of_yojson_err;
    ];
    "timestamp", [
      test_case "now_iso format" `Quick test_now_iso_format;
      test_case "parse valid" `Quick test_parse_iso8601_valid;
      test_case "parse invalid" `Quick test_parse_iso8601_invalid;
      test_case "parse empty" `Quick test_parse_iso8601_empty;
    ];
    "agent_status_show", [
      test_case "active" `Quick test_show_agent_status_active;
      test_case "busy" `Quick test_show_agent_status_busy;
      test_case "listening" `Quick test_show_agent_status_listening;
      test_case "inactive" `Quick test_show_agent_status_inactive;
    ];
    "agent_status_to_string", [
      test_case "active" `Quick test_agent_status_to_string_active;
      test_case "busy" `Quick test_agent_status_to_string_busy;
      test_case "listening" `Quick test_agent_status_to_string_listening;
      test_case "inactive" `Quick test_agent_status_to_string_inactive;
    ];
    "agent_status_of_string_opt", [
      test_case "active" `Quick test_agent_status_of_string_opt_active;
      test_case "busy" `Quick test_agent_status_of_string_opt_busy;
      test_case "listening" `Quick test_agent_status_of_string_opt_listening;
      test_case "inactive" `Quick test_agent_status_of_string_opt_inactive;
      test_case "unknown" `Quick test_agent_status_of_string_opt_unknown;
    ];
    "agent_status_of_string", [
      test_case "active" `Quick test_agent_status_of_string_active;
      test_case "unknown defaults" `Quick test_agent_status_of_string_unknown_defaults;
    ];
    "agent_status_to_yojson", [
      test_case "active" `Quick test_agent_status_to_yojson_active;
      test_case "busy" `Quick test_agent_status_to_yojson_busy;
    ];
    "agent_status_of_yojson", [
      test_case "active" `Quick test_agent_status_of_yojson_active;
      test_case "busy" `Quick test_agent_status_of_yojson_busy;
      test_case "unknown" `Quick test_agent_status_of_yojson_unknown;
      test_case "wrong type" `Quick test_agent_status_of_yojson_wrong_type;
    ];
    "task_status_to_string", [
      test_case "todo" `Quick test_task_status_to_string_todo;
      test_case "claimed" `Quick test_task_status_to_string_claimed;
      test_case "in_progress" `Quick test_task_status_to_string_in_progress;
      test_case "done" `Quick test_task_status_to_string_done;
      test_case "cancelled" `Quick test_task_status_to_string_cancelled;
    ];
    "task_status_to_yojson", [
      test_case "todo" `Quick test_task_status_to_yojson_todo;
      test_case "claimed" `Quick test_task_status_to_yojson_claimed;
      test_case "in_progress" `Quick test_task_status_to_yojson_in_progress;
      test_case "done with notes" `Quick test_task_status_to_yojson_done_with_notes;
      test_case "done no notes" `Quick test_task_status_to_yojson_done_no_notes;
      test_case "cancelled with reason" `Quick test_task_status_to_yojson_cancelled_with_reason;
    ];
    "task_status_of_yojson", [
      test_case "todo" `Quick test_task_status_of_yojson_todo;
      test_case "claimed" `Quick test_task_status_of_yojson_claimed;
      test_case "in_progress" `Quick test_task_status_of_yojson_in_progress;
      test_case "done" `Quick test_task_status_of_yojson_done;
      test_case "cancelled" `Quick test_task_status_of_yojson_cancelled;
      test_case "unknown" `Quick test_task_status_of_yojson_unknown;
    ];
    "worktree_info", [
      test_case "to_yojson" `Quick test_worktree_info_to_yojson;
      test_case "of_yojson" `Quick test_worktree_info_of_yojson;
      test_case "missing field" `Quick test_worktree_info_of_yojson_missing_field;
    ];
    "show_task_status", [
      test_case "todo" `Quick test_show_task_status_todo;
      test_case "claimed" `Quick test_show_task_status_claimed;
    ];
    "aliases", [
      test_case "string_of_agent_status" `Quick test_string_of_agent_status;
      test_case "string_of_task_status" `Quick test_string_of_task_status;
    ];
    "tempo_mode_to_string", [
      test_case "normal" `Quick test_tempo_mode_to_string_normal;
      test_case "slow" `Quick test_tempo_mode_to_string_slow;
      test_case "fast" `Quick test_tempo_mode_to_string_fast;
      test_case "paused" `Quick test_tempo_mode_to_string_paused;
    ];
    "string_of_tempo_mode", [
      test_case "alias" `Quick test_string_of_tempo_mode;
    ];
    "tempo_mode_of_string", [
      test_case "normal" `Quick test_tempo_mode_of_string_normal;
      test_case "slow" `Quick test_tempo_mode_of_string_slow;
      test_case "fast" `Quick test_tempo_mode_of_string_fast;
      test_case "paused" `Quick test_tempo_mode_of_string_paused;
      test_case "unknown" `Quick test_tempo_mode_of_string_unknown;
    ];
    "tempo_mode_to_yojson", [
      test_case "normal" `Quick test_tempo_mode_to_yojson_normal;
      test_case "paused" `Quick test_tempo_mode_to_yojson_paused;
    ];
    "tempo_mode_of_yojson", [
      test_case "ok" `Quick test_tempo_mode_of_yojson_ok;
      test_case "unknown" `Quick test_tempo_mode_of_yojson_unknown;
      test_case "wrong type" `Quick test_tempo_mode_of_yojson_wrong_type;
    ];
    "backlog_to_yojson", [
      test_case "empty" `Quick test_backlog_to_yojson_empty;
      test_case "with tasks" `Quick test_backlog_to_yojson_with_tasks;
    ];
    "backlog_of_yojson", [
      test_case "ok" `Quick test_backlog_of_yojson_ok;
      test_case "with task" `Quick test_backlog_of_yojson_with_task;
      test_case "error" `Quick test_backlog_of_yojson_error;
    ];
    "a2a_task_status_to_string", [
      test_case "pending" `Quick test_a2a_task_status_to_string_pending;
      test_case "running" `Quick test_a2a_task_status_to_string_running;
      test_case "completed" `Quick test_a2a_task_status_to_string_completed;
      test_case "failed" `Quick test_a2a_task_status_to_string_failed;
      test_case "canceled" `Quick test_a2a_task_status_to_string_canceled;
    ];
    "a2a_task_status_of_string", [
      test_case "pending" `Quick test_a2a_task_status_of_string_pending;
      test_case "running" `Quick test_a2a_task_status_of_string_running;
      test_case "completed" `Quick test_a2a_task_status_of_string_completed;
      test_case "failed" `Quick test_a2a_task_status_of_string_failed;
      test_case "canceled" `Quick test_a2a_task_status_of_string_canceled;
      test_case "unknown" `Quick test_a2a_task_status_of_string_unknown;
    ];
    "a2a_task_status_to_yojson", [
      test_case "pending" `Quick test_a2a_task_status_to_yojson_pending;
      test_case "completed" `Quick test_a2a_task_status_to_yojson_completed;
    ];
    "a2a_task_status_of_yojson", [
      test_case "ok" `Quick test_a2a_task_status_of_yojson_ok;
      test_case "unknown" `Quick test_a2a_task_status_of_yojson_unknown;
      test_case "wrong type" `Quick test_a2a_task_status_of_yojson_wrong_type;
    ];
    "portal_state_to_string", [
      test_case "open" `Quick test_portal_state_to_string_open;
      test_case "closed" `Quick test_portal_state_to_string_closed;
    ];
    "portal_state_of_string", [
      test_case "open" `Quick test_portal_state_of_string_open;
      test_case "closed" `Quick test_portal_state_of_string_closed;
      test_case "unknown" `Quick test_portal_state_of_string_unknown;
    ];
    "portal_state_yojson", [
      test_case "to_yojson" `Quick test_portal_state_to_yojson;
      test_case "of_yojson ok" `Quick test_portal_state_of_yojson_ok;
      test_case "of_yojson wrong type" `Quick test_portal_state_of_yojson_wrong_type;
    ];
    "masc_error_to_string", [
      test_case "not initialized" `Quick test_masc_error_not_initialized;
      test_case "already initialized" `Quick test_masc_error_already_initialized;
      test_case "agent not found" `Quick test_masc_error_agent_not_found;
      test_case "task not found" `Quick test_masc_error_task_not_found;
      test_case "task already claimed" `Quick test_masc_error_task_already_claimed;
      test_case "file locked" `Quick test_masc_error_file_locked;
      test_case "rate limit" `Quick test_masc_error_rate_limit;
    ];
    "agent_role_to_string", [
      test_case "reader" `Quick test_agent_role_to_string_reader;
      test_case "worker" `Quick test_agent_role_to_string_worker;
      test_case "admin" `Quick test_agent_role_to_string_admin;
    ];
    "agent_role_of_string", [
      test_case "reader" `Quick test_agent_role_of_string_reader;
      test_case "worker" `Quick test_agent_role_of_string_worker;
      test_case "admin" `Quick test_agent_role_of_string_admin;
      test_case "unknown" `Quick test_agent_role_of_string_unknown;
    ];
    "agent_role_yojson", [
      test_case "to_yojson" `Quick test_agent_role_to_yojson;
      test_case "of_yojson ok" `Quick test_agent_role_of_yojson_ok;
      test_case "of_yojson wrong type" `Quick test_agent_role_of_yojson_wrong_type;
    ];
    "agent_credential", [
      test_case "to_yojson" `Quick test_agent_credential_to_yojson;
      test_case "to_yojson with expiry" `Quick test_agent_credential_to_yojson_with_expiry;
      test_case "of_yojson ok" `Quick test_agent_credential_of_yojson_ok;
      test_case "of_yojson error" `Quick test_agent_credential_of_yojson_error;
    ];
    "auth_config", [
      test_case "to_yojson" `Quick test_auth_config_to_yojson;
      test_case "of_yojson ok" `Quick test_auth_config_of_yojson_ok;
      test_case "of_yojson error" `Quick test_auth_config_of_yojson_error;
    ];
    "permissions", [
      test_case "for_role reader" `Quick test_permissions_for_role_reader;
      test_case "for_role worker" `Quick test_permissions_for_role_worker;
      test_case "for_role admin" `Quick test_permissions_for_role_admin;
      test_case "has_permission reader" `Quick test_has_permission_reader;
      test_case "has_permission worker" `Quick test_has_permission_worker;
      test_case "has_permission admin" `Quick test_has_permission_admin;
    ];
    "rate_limit_role", [
      test_case "multiplier_for_role" `Quick test_multiplier_for_role;
      test_case "effective_limit" `Quick test_effective_limit;
    ];
    "limit_for_category", [
      test_case "general" `Quick test_limit_for_category_general;
      test_case "broadcast" `Quick test_limit_for_category_broadcast;
      test_case "task_ops" `Quick test_limit_for_category_task_ops;
    ];
    "category_for_tool", [
      test_case "broadcast" `Quick test_category_for_tool_broadcast;
      test_case "task_ops" `Quick test_category_for_tool_task_ops;
      test_case "general" `Quick test_category_for_tool_general;
    ];
    "masc_error_extended", [
      test_case "agent already joined" `Quick test_masc_error_agent_already_joined;
      test_case "task not claimed" `Quick test_masc_error_task_not_claimed;
      test_case "task invalid state" `Quick test_masc_error_task_invalid_state;
      test_case "file not locked" `Quick test_masc_error_file_not_locked;
      test_case "portal not open" `Quick test_masc_error_portal_not_open;
      test_case "portal already open" `Quick test_masc_error_portal_already_open;
      test_case "portal closed" `Quick test_masc_error_portal_closed;
      test_case "invalid json" `Quick test_masc_error_invalid_json;
      test_case "io error" `Quick test_masc_error_io_error;
      test_case "invalid agent name" `Quick test_masc_error_invalid_agent_name;
      test_case "invalid task id" `Quick test_masc_error_invalid_task_id;
      test_case "invalid file path" `Quick test_masc_error_invalid_file_path;
      test_case "unauthorized" `Quick test_masc_error_unauthorized;
      test_case "forbidden" `Quick test_masc_error_forbidden;
      test_case "token expired" `Quick test_masc_error_token_expired;
      test_case "invalid token" `Quick test_masc_error_invalid_token;
    ];
    "default_tempo_config", [
      test_case "mode" `Quick test_default_tempo_config_mode;
      test_case "delay" `Quick test_default_tempo_config_delay;
      test_case "reason" `Quick test_default_tempo_config_reason;
    ];
    "tempo_config_yojson", [
      test_case "to_yojson" `Quick test_tempo_config_to_yojson;
      test_case "of_yojson ok" `Quick test_tempo_config_of_yojson_ok;
      test_case "of_yojson error" `Quick test_tempo_config_of_yojson_error;
    ];
    "default_rate_limit", [
      test_case "per_minute" `Quick test_default_rate_limit_per_minute;
      test_case "burst" `Quick test_default_rate_limit_burst;
      test_case "multipliers" `Quick test_default_rate_limit_multipliers;
    ];
    "rate_limit_config_yojson", [
      test_case "to_yojson" `Quick test_rate_limit_config_to_yojson;
      test_case "of_yojson ok" `Quick test_rate_limit_config_of_yojson_ok;
      test_case "of_yojson defaults" `Quick test_rate_limit_config_of_yojson_defaults;
    ];
    "tool_result", [
      test_case "to_yojson success" `Quick test_tool_result_to_yojson_success;
      test_case "to_yojson with data" `Quick test_tool_result_to_yojson_with_data;
    ];
    "task_yojson", [
      test_case "to_yojson" `Quick test_task_to_yojson;
      test_case "to_yojson with worktree" `Quick test_task_to_yojson_with_worktree;
      test_case "of_yojson ok" `Quick test_task_of_yojson_ok;
      test_case "of_yojson error" `Quick test_task_of_yojson_error;
    ];
    "a2a_task_yojson", [
      test_case "to_yojson" `Quick test_a2a_task_to_yojson;
      test_case "to_yojson with result" `Quick test_a2a_task_to_yojson_with_result;
      test_case "of_yojson ok" `Quick test_a2a_task_of_yojson_ok;
      test_case "of_yojson error" `Quick test_a2a_task_of_yojson_error;
    ];
    "portal_yojson", [
      test_case "to_yojson" `Quick test_portal_to_yojson;
      test_case "of_yojson ok" `Quick test_portal_of_yojson_ok;
      test_case "of_yojson closed" `Quick test_portal_of_yojson_closed;
      test_case "of_yojson error" `Quick test_portal_of_yojson_error;
    ];
  ]
