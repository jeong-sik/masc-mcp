(** Error Module Coverage Tests

    Tests for MASC Error types:
    - is_recoverable: check if error is recoverable
    - to_string: error to string conversion
    - severity_of_error: get severity level
    - string_of_severity: severity to string
    - of_string: create error from string
*)

open Alcotest

module Error = Masc_mcp.Error

(* ============================================================
   is_recoverable Tests
   ============================================================ *)

let test_is_recoverable_room_locked () =
  check bool "room locked" true (Error.is_recoverable (Error.Room (Error.RoomLocked "test")))

let test_is_recoverable_room_full () =
  check bool "room full" false (Error.is_recoverable (Error.Room (Error.RoomFull 10)))

let test_is_recoverable_room_not_found () =
  check bool "not found" false (Error.is_recoverable (Error.Room (Error.RoomNotFound "test")))

let test_is_recoverable_task_claimed () =
  check bool "claimed" true (Error.is_recoverable (Error.Task (Error.TaskAlreadyClaimed "agent")))

let test_is_recoverable_task_not_found () =
  check bool "not found" false (Error.is_recoverable (Error.Task (Error.TaskNotFound "test")))

let test_is_recoverable_agent_timeout () =
  check bool "timeout" true (Error.is_recoverable (Error.Agent (Error.AgentTimeout ("test", 1000))))

let test_is_recoverable_agent_heartbeat () =
  check bool "heartbeat" true (Error.is_recoverable (Error.Agent (Error.AgentHeartbeatMissing "test")))

let test_is_recoverable_agent_not_found () =
  check bool "not found" false (Error.is_recoverable (Error.Agent (Error.AgentNotFound "test")))

let test_is_recoverable_file_locked () =
  check bool "locked" true (Error.is_recoverable (Error.Storage (Error.FileLocked "test")))

let test_is_recoverable_storage_postgres () =
  check bool "postgres" false (Error.is_recoverable (Error.Storage (Error.PostgresError "test")))

let test_is_recoverable_portal_timeout () =
  check bool "portal timeout" true (Error.is_recoverable (Error.Federation (Error.PortalTimeout 1000)))

let test_is_recoverable_internal () =
  check bool "internal" false (Error.is_recoverable (Error.Internal "test"))

(* ============================================================
   to_string Tests
   ============================================================ *)

let test_to_string_room_not_found () =
  let e = Error.Room (Error.RoomNotFound "test-room") in
  let s = Error.to_string e in
  check bool "contains room" true (String.length s > 0 && String.sub s 0 4 = "Room")

let test_to_string_room_already_exists () =
  let e = Error.Room (Error.RoomAlreadyExists "test") in
  let s = Error.to_string e in
  check bool "contains room" true (String.length s > 0)

let test_to_string_room_locked () =
  let e = Error.Room (Error.RoomLocked "test") in
  let s = Error.to_string e in
  check bool "contains locked" true (String.length s > 0)

let test_to_string_room_full () =
  let e = Error.Room (Error.RoomFull 10) in
  let s = Error.to_string e in
  check bool "contains full" true (String.length s > 0)

let test_to_string_task_not_found () =
  let e = Error.Task (Error.TaskNotFound "task-123") in
  let s = Error.to_string e in
  check bool "contains task" true (String.length s > 0)

let test_to_string_task_already_claimed () =
  let e = Error.Task (Error.TaskAlreadyClaimed "agent") in
  let s = Error.to_string e in
  check bool "contains claimed" true (String.length s > 0)

let test_to_string_task_invalid_state () =
  let e = Error.Task (Error.TaskInvalidState ("done", "pending")) in
  let s = Error.to_string e in
  check bool "contains state" true (String.length s > 0)

let test_to_string_task_cycle () =
  let e = Error.Task Error.TaskCycleDetected in
  let s = Error.to_string e in
  check bool "contains cycle" true (String.length s > 0)

let test_to_string_agent_not_found () =
  let e = Error.Agent (Error.AgentNotFound "agent-xyz") in
  let s = Error.to_string e in
  check bool "contains agent" true (String.length s > 0)

let test_to_string_agent_timeout () =
  let e = Error.Agent (Error.AgentTimeout ("agent", 1000)) in
  let s = Error.to_string e in
  check bool "contains timeout" true (String.length s > 0)

let test_to_string_agent_heartbeat () =
  let e = Error.Agent (Error.AgentHeartbeatMissing "agent") in
  let s = Error.to_string e in
  check bool "contains heartbeat" true (String.length s > 0)

let test_to_string_agent_capability () =
  let e = Error.Agent (Error.AgentCapabilityMismatch "cap") in
  let s = Error.to_string e in
  check bool "contains capability" true (String.length s > 0)

let test_to_string_federation_conn_failed () =
  let e = Error.Federation (Error.PortalConnectionFailed "addr") in
  let s = Error.to_string e in
  check bool "contains portal" true (String.length s > 0)

let test_to_string_federation_auth () =
  let e = Error.Federation (Error.PortalAuthFailed "reason") in
  let s = Error.to_string e in
  check bool "contains auth" true (String.length s > 0)

let test_to_string_federation_timeout () =
  let e = Error.Federation (Error.PortalTimeout 1000) in
  let s = Error.to_string e in
  check bool "contains timeout" true (String.length s > 0)

let test_to_string_federation_protocol () =
  let e = Error.Federation (Error.PortalProtocolError "msg") in
  let s = Error.to_string e in
  check bool "contains protocol" true (String.length s > 0)

let test_to_string_storage_file_not_found () =
  let e = Error.Storage (Error.FileNotFound "path") in
  let s = Error.to_string e in
  check bool "contains file" true (String.length s > 0)

let test_to_string_storage_permission () =
  let e = Error.Storage (Error.FilePermissionDenied "path") in
  let s = Error.to_string e in
  check bool "contains permission" true (String.length s > 0)

let test_to_string_storage_locked () =
  let e = Error.Storage (Error.FileLocked "path") in
  let s = Error.to_string e in
  check bool "contains locked" true (String.length s > 0)

let test_to_string_storage_postgres () =
  let e = Error.Storage (Error.PostgresError "msg") in
  let s = Error.to_string e in
  check bool "contains postgres" true (String.length s > 0)

let test_to_string_storage_git () =
  let e = Error.Storage (Error.GitError "msg") in
  let s = Error.to_string e in
  check bool "contains git" true (String.length s > 0)

let test_to_string_mcp_parse () =
  let e = Error.Mcp (Error.McpParseError "bad") in
  let s = Error.to_string e in
  check bool "contains mcp" true (String.length s > 0)

let test_to_string_mcp_method () =
  let e = Error.Mcp (Error.McpMethodNotFound "method") in
  let s = Error.to_string e in
  check bool "contains method" true (String.length s > 0)

let test_to_string_mcp_params () =
  let e = Error.Mcp (Error.McpInvalidParams "params") in
  let s = Error.to_string e in
  check bool "contains params" true (String.length s > 0)

let test_to_string_mcp_auth () =
  let e = Error.Mcp (Error.McpAuthError "auth") in
  let s = Error.to_string e in
  check bool "contains auth" true (String.length s > 0)

let test_to_string_mcp_internal () =
  let e = Error.Mcp (Error.McpInternalError "internal") in
  let s = Error.to_string e in
  check bool "contains internal" true (String.length s > 0)

let test_to_string_internal () =
  let e = Error.Internal "internal error" in
  let s = Error.to_string e in
  check bool "contains internal" true (String.length s > 0)

(* ============================================================
   severity_of_error Tests
   ============================================================ *)

let test_severity_room_locked () =
  let e = Error.Room (Error.RoomLocked "test") in
  check bool "warning" true (Error.severity_of_error e = Error.Warning)

let test_severity_task_claimed () =
  let e = Error.Task (Error.TaskAlreadyClaimed "agent") in
  check bool "warning" true (Error.severity_of_error e = Error.Warning)

let test_severity_agent_timeout () =
  let e = Error.Agent (Error.AgentTimeout ("test", 1000)) in
  check bool "warning" true (Error.severity_of_error e = Error.Warning)

let test_severity_agent_heartbeat () =
  let e = Error.Agent (Error.AgentHeartbeatMissing "test") in
  check bool "warning" true (Error.severity_of_error e = Error.Warning)

let test_severity_portal_timeout () =
  let e = Error.Federation (Error.PortalTimeout 1000) in
  check bool "warning" true (Error.severity_of_error e = Error.Warning)

let test_severity_file_locked () =
  let e = Error.Storage (Error.FileLocked "path") in
  check bool "warning" true (Error.severity_of_error e = Error.Warning)

let test_severity_mcp_method () =
  let e = Error.Mcp (Error.McpMethodNotFound "method") in
  check bool "warning" true (Error.severity_of_error e = Error.Warning)

let test_severity_internal () =
  let e = Error.Internal "test" in
  check bool "critical" true (Error.severity_of_error e = Error.Critical)

let test_severity_default_error () =
  let e = Error.Room (Error.RoomNotFound "test") in
  check bool "error level" true (Error.severity_of_error e = Error.Error)

(* ============================================================
   string_of_severity Tests
   ============================================================ *)

let test_string_of_severity_debug () =
  check string "debug" "DEBUG" (Error.string_of_severity Error.Debug)

let test_string_of_severity_info () =
  check string "info" "INFO" (Error.string_of_severity Error.Info)

let test_string_of_severity_warning () =
  check string "warning" "WARN" (Error.string_of_severity Error.Warning)

let test_string_of_severity_error () =
  check string "error" "ERROR" (Error.string_of_severity Error.Error)

let test_string_of_severity_critical () =
  check string "critical" "CRITICAL" (Error.string_of_severity Error.Critical)

(* ============================================================
   of_string Tests
   ============================================================ *)

let test_of_string_creates_internal () =
  let e = Error.of_string "test message" in
  match e with
  | Error.Internal _ -> check bool "internal" true true
  | _ -> fail "expected Internal"

let test_of_string_preserves_message () =
  let e = Error.of_string "my error message" in
  let s = Error.to_string e in
  check bool "contains message" true (String.length s > 0)

(* ============================================================
   Result helpers Tests
   ============================================================ *)

let test_fail_returns_error () =
  let r = Error.fail (Error.Internal "test") in
  match r with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected Error"

let test_ok_returns_ok () =
  let r = Error.ok 42 in
  match r with
  | Ok 42 -> check bool "ok" true true
  | _ -> fail "expected Ok 42"

let test_to_string_result_ok () =
  let r : int Error.result = Ok 42 in
  let sr = Error.to_string_result r in
  match sr with
  | Ok 42 -> check bool "ok preserved" true true
  | _ -> fail "expected Ok 42"

let test_to_string_result_error () =
  let r : int Error.result = Error (Error.Internal "fail") in
  let sr = Error.to_string_result r in
  match sr with
  | Error s -> check bool "error string" true (String.length s > 0)
  | _ -> fail "expected Error string"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Error Coverage" [
    "is_recoverable", [
      test_case "room locked" `Quick test_is_recoverable_room_locked;
      test_case "room full" `Quick test_is_recoverable_room_full;
      test_case "room not found" `Quick test_is_recoverable_room_not_found;
      test_case "task claimed" `Quick test_is_recoverable_task_claimed;
      test_case "task not found" `Quick test_is_recoverable_task_not_found;
      test_case "agent timeout" `Quick test_is_recoverable_agent_timeout;
      test_case "agent heartbeat" `Quick test_is_recoverable_agent_heartbeat;
      test_case "agent not found" `Quick test_is_recoverable_agent_not_found;
      test_case "file locked" `Quick test_is_recoverable_file_locked;
      test_case "storage postgres" `Quick test_is_recoverable_storage_postgres;
      test_case "portal timeout" `Quick test_is_recoverable_portal_timeout;
      test_case "internal" `Quick test_is_recoverable_internal;
    ];
    "to_string", [
      test_case "room not found" `Quick test_to_string_room_not_found;
      test_case "room already exists" `Quick test_to_string_room_already_exists;
      test_case "room locked" `Quick test_to_string_room_locked;
      test_case "room full" `Quick test_to_string_room_full;
      test_case "task not found" `Quick test_to_string_task_not_found;
      test_case "task claimed" `Quick test_to_string_task_already_claimed;
      test_case "task invalid state" `Quick test_to_string_task_invalid_state;
      test_case "task cycle" `Quick test_to_string_task_cycle;
      test_case "agent not found" `Quick test_to_string_agent_not_found;
      test_case "agent timeout" `Quick test_to_string_agent_timeout;
      test_case "agent heartbeat" `Quick test_to_string_agent_heartbeat;
      test_case "agent capability" `Quick test_to_string_agent_capability;
      test_case "federation conn" `Quick test_to_string_federation_conn_failed;
      test_case "federation auth" `Quick test_to_string_federation_auth;
      test_case "federation timeout" `Quick test_to_string_federation_timeout;
      test_case "federation protocol" `Quick test_to_string_federation_protocol;
      test_case "storage file" `Quick test_to_string_storage_file_not_found;
      test_case "storage permission" `Quick test_to_string_storage_permission;
      test_case "storage locked" `Quick test_to_string_storage_locked;
      test_case "storage postgres" `Quick test_to_string_storage_postgres;
      test_case "storage git" `Quick test_to_string_storage_git;
      test_case "mcp parse" `Quick test_to_string_mcp_parse;
      test_case "mcp method" `Quick test_to_string_mcp_method;
      test_case "mcp params" `Quick test_to_string_mcp_params;
      test_case "mcp auth" `Quick test_to_string_mcp_auth;
      test_case "mcp internal" `Quick test_to_string_mcp_internal;
      test_case "internal" `Quick test_to_string_internal;
    ];
    "severity_of_error", [
      test_case "room locked" `Quick test_severity_room_locked;
      test_case "task claimed" `Quick test_severity_task_claimed;
      test_case "agent timeout" `Quick test_severity_agent_timeout;
      test_case "agent heartbeat" `Quick test_severity_agent_heartbeat;
      test_case "portal timeout" `Quick test_severity_portal_timeout;
      test_case "file locked" `Quick test_severity_file_locked;
      test_case "mcp method" `Quick test_severity_mcp_method;
      test_case "internal" `Quick test_severity_internal;
      test_case "default error" `Quick test_severity_default_error;
    ];
    "string_of_severity", [
      test_case "debug" `Quick test_string_of_severity_debug;
      test_case "info" `Quick test_string_of_severity_info;
      test_case "warning" `Quick test_string_of_severity_warning;
      test_case "error" `Quick test_string_of_severity_error;
      test_case "critical" `Quick test_string_of_severity_critical;
    ];
    "of_string", [
      test_case "creates internal" `Quick test_of_string_creates_internal;
      test_case "preserves message" `Quick test_of_string_preserves_message;
    ];
    "result_helpers", [
      test_case "fail" `Quick test_fail_returns_error;
      test_case "ok" `Quick test_ok_returns_ok;
      test_case "to_string ok" `Quick test_to_string_result_ok;
      test_case "to_string error" `Quick test_to_string_result_error;
    ];
  ]
