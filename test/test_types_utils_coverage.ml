(** Comprehensive coverage tests for Types, Room_utils, and Room_eio modules *)

open Alcotest
open Masc_mcp

(* ============================================================ *)
(* Test Helpers                                                  *)
(* ============================================================ *)

let json_string = testable Yojson.Safe.pp Yojson.Safe.equal

let _result_ok testable_a =
  testable
    (fun fmt r ->
      match r with
      | Ok a -> Fmt.pf fmt "Ok(%a)" (Alcotest.pp testable_a) a
      | Error e -> Fmt.pf fmt "Error(%s)" e)
    (fun a b ->
      match a, b with
      | Ok x, Ok y -> Alcotest.equal testable_a x y
      | Error x, Error y -> x = y
      | _ -> false)

let is_ok result =
  match result with Ok _ -> true | Error _ -> false

let is_error result =
  match result with Ok _ -> false | Error _ -> true

(* ============================================================ *)
(* Types.Agent_id Tests                                          *)
(* ============================================================ *)

let test_agent_id_of_string () =
  let id = Types.Agent_id.of_string "claude-123" in
  check string "to_string" "claude-123" (Types.Agent_id.to_string id)

let test_agent_id_equal () =
  let a = Types.Agent_id.of_string "agent-a" in
  let b = Types.Agent_id.of_string "agent-a" in
  let c = Types.Agent_id.of_string "agent-b" in
  check bool "equal same" true (Types.Agent_id.equal a b);
  check bool "not equal different" false (Types.Agent_id.equal a c)

let test_agent_id_to_yojson () =
  let id = Types.Agent_id.of_string "test-agent" in
  let json = Types.Agent_id.to_yojson id in
  check json_string "json string" (`String "test-agent") json

let test_agent_id_of_yojson_valid () =
  let result = Types.Agent_id.of_yojson (`String "valid-agent") in
  check bool "is ok" true (is_ok result)

let test_agent_id_of_yojson_invalid () =
  let result = Types.Agent_id.of_yojson (`Int 123) in
  check bool "is error" true (is_error result)

(* ============================================================ *)
(* Types.Task_id Tests                                           *)
(* ============================================================ *)

let test_task_id_of_string () =
  let id = Types.Task_id.of_string "task-456" in
  check string "to_string" "task-456" (Types.Task_id.to_string id)

let test_task_id_equal () =
  let a = Types.Task_id.of_string "task-a" in
  let b = Types.Task_id.of_string "task-a" in
  let c = Types.Task_id.of_string "task-b" in
  check bool "equal same" true (Types.Task_id.equal a b);
  check bool "not equal different" false (Types.Task_id.equal a c)

let test_task_id_generate () =
  let id1 = Types.Task_id.generate () in
  let id2 = Types.Task_id.generate () in
  let s1 = Types.Task_id.to_string id1 in
  let s2 = Types.Task_id.to_string id2 in
  check bool "starts with task-" true (String.length s1 > 5);
  check bool "different ids" false (s1 = s2)

let test_task_id_to_yojson () =
  let id = Types.Task_id.of_string "task-test" in
  let json = Types.Task_id.to_yojson id in
  check json_string "json string" (`String "task-test") json

let test_task_id_of_yojson_valid () =
  let result = Types.Task_id.of_yojson (`String "valid-task") in
  check bool "is ok" true (is_ok result)

let test_task_id_of_yojson_invalid () =
  let result = Types.Task_id.of_yojson (`Bool true) in
  check bool "is error" true (is_error result)

(* ============================================================ *)
(* Types.Timestamp Tests                                         *)
(* ============================================================ *)

let test_now_iso_format () =
  let ts = Types.now_iso () in
  (* ISO8601 format: YYYY-MM-DDTHH:MM:SSZ *)
  check bool "length >= 20" true (String.length ts >= 20);
  check bool "contains T" true (String.contains ts 'T');
  check bool "ends with Z" true (ts.[String.length ts - 1] = 'Z')

let test_parse_iso8601_valid () =
  let ts = "2024-12-25T10:30:45Z" in
  let result = Types.parse_iso8601 ts in
  check bool "positive timestamp" true (result > 0.0)

let test_parse_iso8601_invalid () =
  let ts = "invalid-timestamp" in
  let default = 12345.0 in
  let result = Types.parse_iso8601 ~default_time:default ts in
  check (float 0.1) "uses default" default result

(* ============================================================ *)
(* Types.agent_status Tests                                      *)
(* ============================================================ *)

let test_agent_status_to_string () =
  check string "Active" "active" (Types.agent_status_to_string Types.Active);
  check string "Busy" "busy" (Types.agent_status_to_string Types.Busy);
  check string "Listening" "listening" (Types.agent_status_to_string Types.Listening);
  check string "Inactive" "inactive" (Types.agent_status_to_string Types.Inactive)

let test_agent_status_of_string () =
  check bool "active" true (Types.agent_status_of_string "active" = Types.Active);
  check bool "busy" true (Types.agent_status_of_string "busy" = Types.Busy);
  check bool "listening" true (Types.agent_status_of_string "listening" = Types.Listening);
  check bool "inactive" true (Types.agent_status_of_string "inactive" = Types.Inactive);
  check bool "unknown defaults to Active" true (Types.agent_status_of_string "unknown" = Types.Active)

let test_agent_status_of_string_opt () =
  check bool "active Some" true (Types.agent_status_of_string_opt "active" = Some Types.Active);
  check bool "unknown None" true (Types.agent_status_of_string_opt "unknown" = None)

let test_agent_status_to_yojson () =
  let json = Types.agent_status_to_yojson Types.Busy in
  check json_string "busy json" (`String "busy") json

let test_agent_status_of_yojson_valid () =
  let result = Types.agent_status_of_yojson (`String "listening") in
  check bool "is ok" true (is_ok result);
  match result with
  | Ok status -> check bool "is Listening" true (status = Types.Listening)
  | Error _ -> failwith "should be ok"

let test_agent_status_of_yojson_invalid () =
  let result = Types.agent_status_of_yojson (`String "garbage") in
  check bool "is error" true (is_error result)

(* ============================================================ *)
(* Types.task_status Tests                                       *)
(* ============================================================ *)

let test_task_status_todo () =
  let status = Types.Todo in
  check string "to_string" "todo" (Types.task_status_to_string status);
  let json = Types.task_status_to_yojson status in
  let result = Types.task_status_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

let test_task_status_claimed () =
  let status = Types.Claimed { assignee = "claude"; claimed_at = "2024-01-01T00:00:00Z" } in
  check string "to_string" "claimed" (Types.task_status_to_string status);
  let json = Types.task_status_to_yojson status in
  let result = Types.task_status_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

let test_task_status_in_progress () =
  let status = Types.InProgress { assignee = "gemini"; started_at = "2024-01-01T01:00:00Z" } in
  check string "to_string" "in_progress" (Types.task_status_to_string status);
  let json = Types.task_status_to_yojson status in
  let result = Types.task_status_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

let test_task_status_done () =
  let status = Types.Done {
    assignee = "codex";
    completed_at = "2024-01-01T02:00:00Z";
    notes = Some "All tests pass"
  } in
  check string "to_string" "done" (Types.task_status_to_string status);
  let json = Types.task_status_to_yojson status in
  let result = Types.task_status_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

let test_task_status_done_no_notes () =
  let status = Types.Done {
    assignee = "codex";
    completed_at = "2024-01-01T02:00:00Z";
    notes = None
  } in
  let json = Types.task_status_to_yojson status in
  let result = Types.task_status_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

let test_task_status_cancelled () =
  let status = Types.Cancelled {
    cancelled_by = "admin";
    cancelled_at = "2024-01-01T03:00:00Z";
    reason = Some "Duplicate task"
  } in
  check string "to_string" "cancelled" (Types.task_status_to_string status);
  let json = Types.task_status_to_yojson status in
  let result = Types.task_status_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

let test_task_status_of_yojson_unknown () =
  let json = `Assoc [("status", `String "unknown_status")] in
  let result = Types.task_status_of_yojson json in
  check bool "is error" true (is_error result)

(* ============================================================ *)
(* Types.tempo_mode Tests                                        *)
(* ============================================================ *)

let test_tempo_mode_to_string () =
  check string "Normal" "normal" (Types.tempo_mode_to_string Types.Normal);
  check string "Slow" "slow" (Types.tempo_mode_to_string Types.Slow);
  check string "Fast" "fast" (Types.tempo_mode_to_string Types.Fast);
  check string "Paused" "paused" (Types.tempo_mode_to_string Types.Paused)

let test_tempo_mode_of_string () =
  check bool "normal ok" true (is_ok (Types.tempo_mode_of_string "normal"));
  check bool "slow ok" true (is_ok (Types.tempo_mode_of_string "slow"));
  check bool "fast ok" true (is_ok (Types.tempo_mode_of_string "fast"));
  check bool "paused ok" true (is_ok (Types.tempo_mode_of_string "paused"));
  check bool "unknown error" true (is_error (Types.tempo_mode_of_string "turbo"))

let test_tempo_mode_roundtrip () =
  let modes = [Types.Normal; Types.Slow; Types.Fast; Types.Paused] in
  List.iter (fun mode ->
    let json = Types.tempo_mode_to_yojson mode in
    let result = Types.tempo_mode_of_yojson json in
    check bool "roundtrip ok" true (is_ok result)
  ) modes

(* ============================================================ *)
(* Types.tempo_config Tests                                      *)
(* ============================================================ *)

let test_default_tempo_config () =
  let cfg = Types.default_tempo_config in
  check bool "mode is Normal" true (cfg.mode = Types.Normal);
  check int "delay_ms is 0" 0 cfg.delay_ms;
  check bool "reason is None" true (cfg.reason = None)

let test_tempo_config_roundtrip () =
  let cfg = Types.{
    mode = Slow;
    delay_ms = 500;
    reason = Some "Heavy workload";
    set_by = Some "admin";
    set_at = Some "2024-01-01T00:00:00Z";
  } in
  let json = Types.tempo_config_to_yojson cfg in
  let result = Types.tempo_config_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Types.a2a_task_status Tests                                   *)
(* ============================================================ *)

let test_a2a_task_status_all () =
  let statuses = [
    (Types.A2APending, "pending");
    (Types.A2ARunning, "running");
    (Types.A2ACompleted, "completed");
    (Types.A2AFailed, "failed");
    (Types.A2ACanceled, "canceled");
  ] in
  List.iter (fun (status, expected) ->
    check string "to_string" expected (Types.a2a_task_status_to_string status);
    let json = Types.a2a_task_status_to_yojson status in
    let result = Types.a2a_task_status_of_yojson json in
    check bool "roundtrip ok" true (is_ok result)
  ) statuses

let test_a2a_task_status_of_string_unknown () =
  let result = Types.a2a_task_status_of_string "unknown" in
  check bool "is error" true (is_error result)

(* ============================================================ *)
(* Types.portal_state Tests                                      *)
(* ============================================================ *)

let test_portal_state_all () =
  let states = [(Types.PortalOpen, "open"); (Types.PortalClosed, "closed")] in
  List.iter (fun (state, expected) ->
    check string "to_string" expected (Types.portal_state_to_string state);
    let json = Types.portal_state_to_yojson state in
    let result = Types.portal_state_of_yojson json in
    check bool "roundtrip ok" true (is_ok result)
  ) states

let test_portal_state_of_string_unknown () =
  let result = Types.portal_state_of_string "half-open" in
  check bool "is error" true (is_error result)

(* ============================================================ *)
(* Types.agent_role Tests                                        *)
(* ============================================================ *)

let test_agent_role_all () =
  let roles = [(Types.Reader, "reader"); (Types.Worker, "worker"); (Types.Admin, "admin")] in
  List.iter (fun (role, expected) ->
    check string "to_string" expected (Types.agent_role_to_string role);
    let json = Types.agent_role_to_yojson role in
    let result = Types.agent_role_of_yojson json in
    check bool "roundtrip ok" true (is_ok result)
  ) roles

let test_agent_role_of_string_unknown () =
  let result = Types.agent_role_of_string "superadmin" in
  check bool "is error" true (is_error result)

(* ============================================================ *)
(* Types.permissions Tests                                       *)
(* ============================================================ *)

let test_permissions_reader () =
  let perms = Types.permissions_for_role Types.Reader in
  check bool "can read" true (List.mem Types.CanReadState perms);
  check bool "can join" true (List.mem Types.CanJoin perms);
  check bool "cannot add task" false (List.mem Types.CanAddTask perms);
  check bool "cannot init" false (List.mem Types.CanInit perms)

let test_permissions_worker () =
  let perms = Types.permissions_for_role Types.Worker in
  check bool "can read" true (List.mem Types.CanReadState perms);
  check bool "can add task" true (List.mem Types.CanAddTask perms);
  check bool "can broadcast" true (List.mem Types.CanBroadcast perms);
  check bool "cannot init" false (List.mem Types.CanInit perms)

let test_permissions_admin () =
  let perms = Types.permissions_for_role Types.Admin in
  check bool "can init" true (List.mem Types.CanInit perms);
  check bool "can reset" true (List.mem Types.CanReset perms);
  check bool "can approve" true (List.mem Types.CanApprove perms)

let test_has_permission () =
  check bool "reader can read" true (Types.has_permission Types.Reader Types.CanReadState);
  check bool "reader cannot init" false (Types.has_permission Types.Reader Types.CanInit);
  check bool "admin can init" true (Types.has_permission Types.Admin Types.CanInit)

(* ============================================================ *)
(* Types.rate_limit Tests                                        *)
(* ============================================================ *)

let test_default_rate_limit () =
  let cfg = Types.default_rate_limit in
  check int "per_minute" 10 cfg.per_minute;
  check int "burst_allowed" 5 cfg.burst_allowed;
  check (float 0.01) "reader_multiplier" 0.5 cfg.reader_multiplier;
  check (float 0.01) "worker_multiplier" 1.0 cfg.worker_multiplier;
  check (float 0.01) "admin_multiplier" 2.0 cfg.admin_multiplier

let test_limit_for_category () =
  let cfg = Types.default_rate_limit in
  check int "general" cfg.per_minute (Types.limit_for_category cfg Types.GeneralLimit);
  check int "broadcast" cfg.broadcast_per_minute (Types.limit_for_category cfg Types.BroadcastLimit);
  check int "task_ops" cfg.task_ops_per_minute (Types.limit_for_category cfg Types.TaskOpsLimit)

let test_category_for_tool () =
  check bool "broadcast" true (Types.category_for_tool "masc_broadcast" = Types.BroadcastLimit);
  check bool "claim" true (Types.category_for_tool "masc_claim" = Types.TaskOpsLimit);
  check bool "status" true (Types.category_for_tool "masc_status" = Types.GeneralLimit)

let test_multiplier_for_role () =
  let cfg = Types.default_rate_limit in
  check (float 0.01) "reader" 0.5 (Types.multiplier_for_role cfg Types.Reader);
  check (float 0.01) "worker" 1.0 (Types.multiplier_for_role cfg Types.Worker);
  check (float 0.01) "admin" 2.0 (Types.multiplier_for_role cfg Types.Admin)

let test_effective_limit () =
  let cfg = Types.default_rate_limit in
  let eff = Types.effective_limit cfg ~role:Types.Admin ~category:Types.GeneralLimit in
  check int "admin general" 20 eff (* 10 * 2.0 *)

let test_rate_limit_config_roundtrip () =
  let cfg = Types.{
    per_minute = 20;
    burst_allowed = 10;
    priority_agents = ["admin-1"; "admin-2"];
    reader_multiplier = 0.25;
    worker_multiplier = 1.5;
    admin_multiplier = 3.0;
    broadcast_per_minute = 30;
    task_ops_per_minute = 60;
  } in
  let json = Types.rate_limit_config_to_yojson cfg in
  let result = Types.rate_limit_config_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Types.masc_error Tests                                        *)
(* ============================================================ *)

let test_masc_error_to_string () =
  check bool "NotInitialized" true
    (String.length (Types.masc_error_to_string Types.NotInitialized) > 0);
  check bool "AgentNotFound" true
    (String.length (Types.masc_error_to_string (Types.AgentNotFound "test")) > 0);
  check bool "TaskAlreadyClaimed" true
    (String.length (Types.masc_error_to_string
      (Types.TaskAlreadyClaimed { task_id = "t1"; by = "agent" })) > 0);
  check bool "RateLimitExceeded" true
    (String.length (Types.masc_error_to_string
      (Types.RateLimitExceeded {
        limit = 10; current = 15; wait_seconds = 30;
        category = Types.BroadcastLimit
      })) > 0)

(* ============================================================ *)
(* Types.worktree_info Tests                                     *)
(* ============================================================ *)

let test_worktree_info_roundtrip () =
  let wt = Types.{
    branch = "feature/test";
    path = ".worktrees/feature-test";
    git_root = "/home/user/project";
    repo_name = "project";
  } in
  let json = Types.worktree_info_to_yojson wt in
  let result = Types.worktree_info_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Types.task Tests                                              *)
(* ============================================================ *)

let test_task_roundtrip () =
  let task = Types.{
    id = "task-123";
    title = "Test Task";
    description = "A test task for coverage";
    task_status = Todo;
    priority = 2;
    files = ["file1.ml"; "file2.ml"];
    created_at = "2024-01-01T00:00:00Z";
    worktree = None;
  } in
  let json = Types.task_to_yojson task in
  let result = Types.task_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

let test_task_with_worktree () =
  let task = Types.{
    id = "task-456";
    title = "Worktree Task";
    description = "Task with worktree";
    task_status = InProgress { assignee = "claude"; started_at = "2024-01-01T01:00:00Z" };
    priority = 1;
    files = [];
    created_at = "2024-01-01T00:00:00Z";
    worktree = Some {
      branch = "feature/wt";
      path = ".worktrees/wt";
      git_root = "/project";
      repo_name = "project";
    };
  } in
  let json = Types.task_to_yojson task in
  let result = Types.task_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Types.backlog Tests                                           *)
(* ============================================================ *)

let test_backlog_roundtrip () =
  let backlog = Types.{
    tasks = [
      { id = "t1"; title = "Task 1"; description = "Desc 1";
        task_status = Todo; priority = 1; files = [];
        created_at = "2024-01-01T00:00:00Z"; worktree = None };
      { id = "t2"; title = "Task 2"; description = "Desc 2";
        task_status = Done { assignee = "a"; completed_at = "2024-01-02T00:00:00Z"; notes = None };
        priority = 2; files = []; created_at = "2024-01-01T01:00:00Z"; worktree = None };
    ];
    last_updated = "2024-01-02T00:00:00Z";
    version = 5;
  } in
  let json = Types.backlog_to_yojson backlog in
  let result = Types.backlog_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Types.a2a_task Tests                                          *)
(* ============================================================ *)

let test_a2a_task_roundtrip () =
  let task = Types.{
    a2a_id = "a2a-123";
    from_agent = "claude";
    to_agent = "gemini";
    a2a_message = "Please review this code";
    a2a_status = A2ARunning;
    a2a_result = None;
    created_at = "2024-01-01T00:00:00Z";
    updated_at = "2024-01-01T01:00:00Z";
  } in
  let json = Types.a2a_task_to_yojson task in
  let result = Types.a2a_task_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

let test_a2a_task_with_result () =
  let task = Types.{
    a2a_id = "a2a-456";
    from_agent = "gemini";
    to_agent = "codex";
    a2a_message = "Implement feature X";
    a2a_status = A2ACompleted;
    a2a_result = Some "Feature implemented successfully";
    created_at = "2024-01-01T00:00:00Z";
    updated_at = "2024-01-01T02:00:00Z";
  } in
  let json = Types.a2a_task_to_yojson task in
  let result = Types.a2a_task_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Types.portal Tests                                            *)
(* ============================================================ *)

let test_portal_roundtrip () =
  let portal = Types.{
    portal_from = "claude";
    portal_target = "gemini";
    portal_opened_at = "2024-01-01T00:00:00Z";
    portal_status = PortalOpen;
    task_count = 5;
  } in
  let json = Types.portal_to_yojson portal in
  let result = Types.portal_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Types.tool_result Tests                                       *)
(* ============================================================ *)

let test_tool_result_success () =
  let result = Types.{
    success = true;
    message = "Operation completed";
    data = Some (`Assoc [("count", `Int 42)]);
  } in
  let json = Types.tool_result_to_yojson result in
  let open Yojson.Safe.Util in
  check bool "has success" true (json |> member "success" |> to_bool);
  check string "message" "Operation completed" (json |> member "message" |> to_string)

let test_tool_result_no_data () =
  let result = Types.{
    success = false;
    message = "Operation failed";
    data = None;
  } in
  let json = Types.tool_result_to_yojson result in
  let open Yojson.Safe.Util in
  check bool "success false" false (json |> member "success" |> to_bool)

(* ============================================================ *)
(* Types.agent_credential Tests                                  *)
(* ============================================================ *)

let test_agent_credential_roundtrip () =
  let cred = Types.{
    agent_name = "claude-secure";
    token = "sha256:abc123";
    role = Admin;
    created_at = "2024-01-01T00:00:00Z";
    expires_at = Some "2024-02-01T00:00:00Z";
  } in
  let json = Types.agent_credential_to_yojson cred in
  let result = Types.agent_credential_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

let test_agent_credential_no_expiry () =
  let cred = Types.{
    agent_name = "worker-1";
    token = "sha256:xyz789";
    role = Worker;
    created_at = "2024-01-01T00:00:00Z";
    expires_at = None;
  } in
  let json = Types.agent_credential_to_yojson cred in
  let result = Types.agent_credential_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Types.auth_config Tests                                       *)
(* ============================================================ *)

let test_default_auth_config () =
  let cfg = Types.default_auth_config in
  check bool "not enabled" false cfg.enabled;
  check bool "default role is Worker" true (cfg.default_role = Types.Worker);
  check int "token_expiry_hours" 24 cfg.token_expiry_hours

let test_auth_config_roundtrip () =
  let cfg = Types.{
    enabled = true;
    room_secret_hash = Some "sha256:secret";
    require_token = true;
    default_role = Reader;
    token_expiry_hours = 48;
  } in
  let json = Types.auth_config_to_yojson cfg in
  let result = Types.auth_config_of_yojson json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Room_utils.parse_gitdir Tests                                 *)
(* ============================================================ *)

let test_parse_gitdir_worktree () =
  let line = "gitdir: /home/user/project/.git/worktrees/feature-branch" in
  let result = Room_utils.parse_gitdir_to_main_root line in
  check bool "is Some" true (Option.is_some result)

let test_parse_gitdir_invalid () =
  let line = "not a gitdir line" in
  let result = Room_utils.parse_gitdir_to_main_root line in
  check bool "is None" true (Option.is_none result)

(* ============================================================ *)
(* Room_utils.sanitize Tests                                     *)
(* ============================================================ *)

let test_sanitize_html () =
  let input = "<script>alert('xss')</script>" in
  let result = Room_utils.sanitize_html input in
  check bool "no angle brackets" true
    (not (String.contains result '<') && not (String.contains result '>'));
  check bool "escaped lt" true (String.length result > String.length input)

let test_sanitize_html_quotes () =
  let input = "Hello \"world\" & 'friends'" in
  let result = Room_utils.sanitize_html input in
  check bool "quotes escaped" true
    (not (String.contains result '"') && not (String.contains result '\''))

let test_safe_filename () =
  let input = "file with spaces & special<chars>.txt" in
  let result = Room_utils.safe_filename input in
  check bool "no spaces" false (String.contains result ' ');
  check bool "no ampersand" false (String.contains result '&');
  check bool "no angle" false (String.contains result '<')

let test_safe_filename_valid () =
  let input = "valid_file-name.txt" in
  let result = Room_utils.safe_filename input in
  check string "unchanged" input result

(* ============================================================ *)
(* Room_utils.validation Tests                                   *)
(* ============================================================ *)

let test_validate_agent_name_valid () =
  let result = Room_utils.validate_agent_name "claude-123" in
  check bool "valid" true (is_ok result)

let test_validate_agent_name_r_valid () =
  let result = Room_utils.validate_agent_name_r "valid-agent" in
  check bool "ok result" true (is_ok result)

let test_validate_task_id_valid () =
  let result = Room_utils.validate_task_id "task-12345" in
  check bool "valid" true (is_ok result)

let test_validate_task_id_r_valid () =
  let result = Room_utils.validate_task_id_r "task-abc" in
  check bool "ok result" true (is_ok result)

let test_validate_file_path_valid () =
  let result = Room_utils.validate_file_path "src/main.ml" in
  check bool "valid" true (is_ok result)

let test_validate_file_path_too_long () =
  let long_path = String.make 501 'a' in
  let result = Room_utils.validate_file_path long_path in
  check bool "invalid too long" true (is_error result)

let test_validate_file_path_angle_brackets () =
  let result = Room_utils.validate_file_path "file<name>.txt" in
  check bool "invalid angle brackets" true (is_error result)

(* ============================================================ *)
(* Room_utils.contains_substring Tests                           *)
(* ============================================================ *)

let test_contains_substring_true () =
  check bool "contains" true (Room_utils.contains_substring "hello world" "world")

let test_contains_substring_false () =
  check bool "not contains" false (Room_utils.contains_substring "hello world" "foo")

let test_contains_substring_empty () =
  check bool "empty needle" true (Room_utils.contains_substring "hello" "")

(* ============================================================ *)
(* Room_eio.event_type Tests                                     *)
(* ============================================================ *)

let test_room_eio_event_type_to_string () =
  check string "AgentJoin" "agent_join" (Room_eio.event_type_to_string Room_eio.AgentJoin);
  check string "AgentLeave" "agent_leave" (Room_eio.event_type_to_string Room_eio.AgentLeave);
  check string "Broadcast" "broadcast" (Room_eio.event_type_to_string Room_eio.Broadcast);
  check string "TaskClaim" "task_claim" (Room_eio.event_type_to_string Room_eio.TaskClaim);
  check string "TaskDone" "task_done" (Room_eio.event_type_to_string Room_eio.TaskDone);
  check string "LockAcquire" "lock_acquire" (Room_eio.event_type_to_string Room_eio.LockAcquire);
  check string "LockRelease" "lock_release" (Room_eio.event_type_to_string Room_eio.LockRelease)

(* ============================================================ *)
(* Room_eio.now_iso Tests                                        *)
(* ============================================================ *)

let test_room_eio_now_iso () =
  let ts = Room_eio.now_iso () in
  check bool "contains T" true (String.contains ts 'T');
  check bool "ends with Z" true (ts.[String.length ts - 1] = 'Z');
  (* Has milliseconds: YYYY-MM-DDTHH:MM:SS.mmmZ *)
  check bool "has ms" true (String.contains ts '.')

(* ============================================================ *)
(* Room_eio.room_state JSON Tests                                *)
(* ============================================================ *)

let test_room_eio_default_room_state () =
  let state = Room_eio.default_room_state () in
  check string "protocol_version" "1.0.0" state.protocol_version;
  check bool "not paused" false state.paused;
  check (list string) "no active agents" [] state.active_agents

let test_room_eio_room_state_roundtrip () =
  let state = Room_eio.{
    protocol_version = "1.0.0";
    started_at = 1704067200.0;
    last_updated = 1704070800.0;
    active_agents = ["claude"; "gemini"];
    message_seq = 42;
    event_seq = 10;
    mode = "collaborative";
    paused = true;
    paused_by = Some "admin";
    paused_at = Some 1704070000.0;
    pause_reason = Some "Maintenance";
  } in
  let json = Room_eio.room_state_to_json state in
  let result = Room_eio.room_state_of_json json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Room_eio.agent_state JSON Tests                               *)
(* ============================================================ *)

let test_room_eio_agent_state_roundtrip () =
  let agent = Room_eio.{
    name = "test-agent";
    last_seen = 1704067200.0;
    capabilities = ["code"; "review"; "test"];
    status = "active";
  } in
  let json = Room_eio.agent_state_to_json agent in
  let result = Room_eio.agent_state_of_json json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Room_eio.lock_info JSON Tests                                 *)
(* ============================================================ *)

let test_room_eio_lock_info_roundtrip () =
  let lock = Room_eio.{
    resource = "src/main.ml";
    owner = "claude";
    acquired_at = 1704067200.0;
    expires_at = 1704070800.0;
  } in
  let json = Room_eio.lock_info_to_json lock in
  let result = Room_eio.lock_info_of_json json in
  check bool "roundtrip ok" true (is_ok result)

let test_room_eio_lock_info_int_floats () =
  (* Test parsing when floats are encoded as ints *)
  let json = `Assoc [
    ("resource", `String "file.ml");
    ("owner", `String "agent");
    ("acquired_at", `Int 1704067200);
    ("expires_at", `Intlit "1704070800");
  ] in
  let result = Room_eio.lock_info_of_json json in
  check bool "parses int as float" true (is_ok result)

(* ============================================================ *)
(* Room_eio.message JSON Tests                                   *)
(* ============================================================ *)

let test_room_eio_message_roundtrip () =
  let msg = Room_eio.{
    seq = 42;
    from_agent = "claude";
    content = "Hello @gemini, please review this";
    mention = Some "gemini";
    timestamp = 1704067200.0;
  } in
  let json = Room_eio.message_to_json msg in
  let result = Room_eio.message_of_json json in
  check bool "roundtrip ok" true (is_ok result)

let test_room_eio_message_no_mention () =
  let msg = Room_eio.{
    seq = 1;
    from_agent = "gemini";
    content = "General broadcast";
    mention = None;
    timestamp = 1704067200.0;
  } in
  let json = Room_eio.message_to_json msg in
  let result = Room_eio.message_of_json json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Room_eio.task_status JSON Tests                               *)
(* ============================================================ *)

let test_room_eio_task_status_pending () =
  let status = Room_eio.Pending in
  let json = Room_eio.task_status_to_json status in
  let result = Room_eio.task_status_of_json json in
  check bool "roundtrip ok" true (is_ok result)

let test_room_eio_task_status_in_progress () =
  let status = Room_eio.InProgress "claude" in
  let json = Room_eio.task_status_to_json status in
  let result = Room_eio.task_status_of_json json in
  check bool "roundtrip ok" true (is_ok result)

let test_room_eio_task_status_completed () =
  let status = Room_eio.Completed "gemini" in
  let json = Room_eio.task_status_to_json status in
  let result = Room_eio.task_status_of_json json in
  check bool "roundtrip ok" true (is_ok result)

let test_room_eio_task_status_failed () =
  let status = Room_eio.Failed ("codex", "Test failed") in
  let json = Room_eio.task_status_to_json status in
  let result = Room_eio.task_status_of_json json in
  check bool "roundtrip ok" true (is_ok result)

let test_room_eio_task_status_unknown () =
  let json = `Assoc [("type", `String "unknown_type")] in
  let result = Room_eio.task_status_of_json json in
  check bool "is error" true (is_error result)

(* ============================================================ *)
(* Room_eio.task JSON Tests                                      *)
(* ============================================================ *)

let test_room_eio_task_roundtrip () =
  let task = Room_eio.{
    id = "task-123";
    description = "Implement feature X";
    status = InProgress "claude";
    created_at = 1704067200.0;
    updated_at = 1704070800.0;
    priority = 2;
  } in
  let json = Room_eio.task_to_json task in
  let result = Room_eio.task_of_json json in
  check bool "roundtrip ok" true (is_ok result)

(* ============================================================ *)
(* Test Suite                                                    *)
(* ============================================================ *)

let agent_id_tests = [
  "of_string/to_string", `Quick, test_agent_id_of_string;
  "equal", `Quick, test_agent_id_equal;
  "to_yojson", `Quick, test_agent_id_to_yojson;
  "of_yojson valid", `Quick, test_agent_id_of_yojson_valid;
  "of_yojson invalid", `Quick, test_agent_id_of_yojson_invalid;
]

let task_id_tests = [
  "of_string/to_string", `Quick, test_task_id_of_string;
  "equal", `Quick, test_task_id_equal;
  "generate unique", `Quick, test_task_id_generate;
  "to_yojson", `Quick, test_task_id_to_yojson;
  "of_yojson valid", `Quick, test_task_id_of_yojson_valid;
  "of_yojson invalid", `Quick, test_task_id_of_yojson_invalid;
]

let timestamp_tests = [
  "now_iso format", `Quick, test_now_iso_format;
  "parse_iso8601 valid", `Quick, test_parse_iso8601_valid;
  "parse_iso8601 invalid", `Quick, test_parse_iso8601_invalid;
]

let agent_status_tests = [
  "to_string all", `Quick, test_agent_status_to_string;
  "of_string all", `Quick, test_agent_status_of_string;
  "of_string_opt", `Quick, test_agent_status_of_string_opt;
  "to_yojson", `Quick, test_agent_status_to_yojson;
  "of_yojson valid", `Quick, test_agent_status_of_yojson_valid;
  "of_yojson invalid", `Quick, test_agent_status_of_yojson_invalid;
]

let task_status_tests = [
  "Todo roundtrip", `Quick, test_task_status_todo;
  "Claimed roundtrip", `Quick, test_task_status_claimed;
  "InProgress roundtrip", `Quick, test_task_status_in_progress;
  "Done roundtrip", `Quick, test_task_status_done;
  "Done no notes", `Quick, test_task_status_done_no_notes;
  "Cancelled roundtrip", `Quick, test_task_status_cancelled;
  "unknown status error", `Quick, test_task_status_of_yojson_unknown;
]

let tempo_tests = [
  "mode to_string", `Quick, test_tempo_mode_to_string;
  "mode of_string", `Quick, test_tempo_mode_of_string;
  "mode roundtrip", `Quick, test_tempo_mode_roundtrip;
  "default config", `Quick, test_default_tempo_config;
  "config roundtrip", `Quick, test_tempo_config_roundtrip;
]

let a2a_tests = [
  "status all", `Quick, test_a2a_task_status_all;
  "status unknown error", `Quick, test_a2a_task_status_of_string_unknown;
  "task roundtrip", `Quick, test_a2a_task_roundtrip;
  "task with result", `Quick, test_a2a_task_with_result;
]

let portal_tests = [
  "state all", `Quick, test_portal_state_all;
  "state unknown error", `Quick, test_portal_state_of_string_unknown;
  "roundtrip", `Quick, test_portal_roundtrip;
]

let role_tests = [
  "all roles", `Quick, test_agent_role_all;
  "unknown error", `Quick, test_agent_role_of_string_unknown;
  "permissions reader", `Quick, test_permissions_reader;
  "permissions worker", `Quick, test_permissions_worker;
  "permissions admin", `Quick, test_permissions_admin;
  "has_permission", `Quick, test_has_permission;
]

let rate_limit_tests = [
  "default config", `Quick, test_default_rate_limit;
  "limit_for_category", `Quick, test_limit_for_category;
  "category_for_tool", `Quick, test_category_for_tool;
  "multiplier_for_role", `Quick, test_multiplier_for_role;
  "effective_limit", `Quick, test_effective_limit;
  "config roundtrip", `Quick, test_rate_limit_config_roundtrip;
]

let error_tests = [
  "masc_error to_string", `Quick, test_masc_error_to_string;
]

let worktree_tests = [
  "roundtrip", `Quick, test_worktree_info_roundtrip;
]

let task_tests = [
  "roundtrip", `Quick, test_task_roundtrip;
  "with worktree", `Quick, test_task_with_worktree;
]

let backlog_tests = [
  "roundtrip", `Quick, test_backlog_roundtrip;
]

let tool_result_tests = [
  "success", `Quick, test_tool_result_success;
  "no data", `Quick, test_tool_result_no_data;
]

let credential_tests = [
  "roundtrip", `Quick, test_agent_credential_roundtrip;
  "no expiry", `Quick, test_agent_credential_no_expiry;
]

let auth_config_tests = [
  "default", `Quick, test_default_auth_config;
  "roundtrip", `Quick, test_auth_config_roundtrip;
]

let gitdir_tests = [
  "parse worktree", `Quick, test_parse_gitdir_worktree;
  "parse invalid", `Quick, test_parse_gitdir_invalid;
]

let sanitize_tests = [
  "html script", `Quick, test_sanitize_html;
  "html quotes", `Quick, test_sanitize_html_quotes;
  "safe_filename special", `Quick, test_safe_filename;
  "safe_filename valid", `Quick, test_safe_filename_valid;
]

let validation_tests = [
  "agent_name valid", `Quick, test_validate_agent_name_valid;
  "agent_name_r valid", `Quick, test_validate_agent_name_r_valid;
  "task_id valid", `Quick, test_validate_task_id_valid;
  "task_id_r valid", `Quick, test_validate_task_id_r_valid;
  "file_path valid", `Quick, test_validate_file_path_valid;
  "file_path too long", `Quick, test_validate_file_path_too_long;
  "file_path angle brackets", `Quick, test_validate_file_path_angle_brackets;
]

let substring_tests = [
  "contains true", `Quick, test_contains_substring_true;
  "contains false", `Quick, test_contains_substring_false;
  "empty needle", `Quick, test_contains_substring_empty;
]

let room_eio_event_tests = [
  "event_type_to_string", `Quick, test_room_eio_event_type_to_string;
]

let room_eio_time_tests = [
  "now_iso", `Quick, test_room_eio_now_iso;
]

let room_eio_state_tests = [
  "default_room_state", `Quick, test_room_eio_default_room_state;
  "room_state roundtrip", `Quick, test_room_eio_room_state_roundtrip;
]

let room_eio_agent_tests = [
  "agent_state roundtrip", `Quick, test_room_eio_agent_state_roundtrip;
]

let room_eio_lock_tests = [
  "lock_info roundtrip", `Quick, test_room_eio_lock_info_roundtrip;
  "lock_info int floats", `Quick, test_room_eio_lock_info_int_floats;
]

let room_eio_message_tests = [
  "message roundtrip", `Quick, test_room_eio_message_roundtrip;
  "message no mention", `Quick, test_room_eio_message_no_mention;
]

let room_eio_task_status_tests = [
  "pending", `Quick, test_room_eio_task_status_pending;
  "in_progress", `Quick, test_room_eio_task_status_in_progress;
  "completed", `Quick, test_room_eio_task_status_completed;
  "failed", `Quick, test_room_eio_task_status_failed;
  "unknown error", `Quick, test_room_eio_task_status_unknown;
]

let room_eio_task_tests = [
  "task roundtrip", `Quick, test_room_eio_task_roundtrip;
]

let () =
  run "Types & Utils Coverage" [
    "Agent_id", agent_id_tests;
    "Task_id", task_id_tests;
    "Timestamp", timestamp_tests;
    "agent_status", agent_status_tests;
    "task_status", task_status_tests;
    "tempo", tempo_tests;
    "a2a", a2a_tests;
    "portal", portal_tests;
    "agent_role", role_tests;
    "rate_limit", rate_limit_tests;
    "masc_error", error_tests;
    "worktree_info", worktree_tests;
    "task", task_tests;
    "backlog", backlog_tests;
    "tool_result", tool_result_tests;
    "agent_credential", credential_tests;
    "auth_config", auth_config_tests;
    "gitdir_parse", gitdir_tests;
    "sanitize", sanitize_tests;
    "validation", validation_tests;
    "contains_substring", substring_tests;
    "Room_eio.event", room_eio_event_tests;
    "Room_eio.time", room_eio_time_tests;
    "Room_eio.state", room_eio_state_tests;
    "Room_eio.agent", room_eio_agent_tests;
    "Room_eio.lock", room_eio_lock_tests;
    "Room_eio.message", room_eio_message_tests;
    "Room_eio.task_status", room_eio_task_status_tests;
    "Room_eio.task", room_eio_task_tests;
  ]
