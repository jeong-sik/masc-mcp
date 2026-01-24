(** Centralized error types for masc-mcp

    This module provides structured error types to replace string-based errors.
    Benefits:
    - Compile-time exhaustiveness checking
    - Rich error context (not just messages)
    - Better debugging and logging
    - Type-safe error handling

    @since 0.4.0
*)

(** {1 Domain-Specific Errors} *)

(** Room/Coordination errors *)
type room_error =
  | RoomNotFound of string         (** Room doesn't exist *)
  | RoomAlreadyExists of string    (** Duplicate room creation *)
  | RoomLocked of string           (** Room is locked by another agent *)
  | RoomFull of int                (** Max agents reached *)

(** Task errors *)
type task_error =
  | TaskNotFound of string         (** Task doesn't exist *)
  | TaskAlreadyClaimed of string   (** Task owned by another agent *)
  | TaskInvalidState of string * string  (** Current state, expected state *)
  | TaskCycleDetected              (** Dependency cycle *)

(** Agent errors *)
type agent_error =
  | AgentNotFound of string        (** Agent doesn't exist *)
  | AgentTimeout of string * int   (** Agent ID, timeout ms *)
  | AgentHeartbeatMissing of string  (** Agent stopped responding *)
  | AgentCapabilityMismatch of string  (** Required capability not found *)

(** Federation/Portal errors *)
type federation_error =
  | PortalConnectionFailed of string  (** Target address *)
  | PortalAuthFailed of string        (** Reason *)
  | PortalTimeout of int              (** Timeout ms *)
  | PortalProtocolError of string     (** Protocol mismatch *)

(** Storage/Backend errors *)
type storage_error =
  | FileNotFound of string
  | FilePermissionDenied of string
  | FileLocked of string
  | PostgresError of string
  | GitError of string

(** MCP Protocol errors *)
type mcp_error =
  | McpParseError of string         (** Invalid JSON-RPC *)
  | McpMethodNotFound of string     (** Unknown method *)
  | McpInvalidParams of string      (** Invalid parameters *)
  | McpAuthError of string          (** Authentication failed *)
  | McpInternalError of string      (** Internal server error *)

(** {1 Unified Error Type} *)

(** Top-level error type combining all domains *)
type t =
  | Room of room_error
  | Task of task_error
  | Agent of agent_error
  | Federation of federation_error
  | Storage of storage_error
  | Mcp of mcp_error
  | Internal of string              (** Unexpected internal error *)

(** {1 Error Utilities} *)

(** Check if an error is recoverable (safe to retry) *)
let is_recoverable = function
  | Room (RoomLocked _) -> true
  | Task (TaskAlreadyClaimed _) -> true
  | Agent (AgentTimeout _) -> true
  | Agent (AgentHeartbeatMissing _) -> true
  | Federation (PortalTimeout _) -> true
  | Storage (FileLocked _) -> true
  | _ -> false

(** Get a human-readable error message *)
let to_string = function
  | Room e -> (
      match e with
      | RoomNotFound id -> Printf.sprintf "Room not found: %s" id
      | RoomAlreadyExists id -> Printf.sprintf "Room already exists: %s" id
      | RoomLocked id -> Printf.sprintf "Room locked: %s" id
      | RoomFull max -> Printf.sprintf "Room full (max %d agents)" max)
  | Task e -> (
      match e with
      | TaskNotFound id -> Printf.sprintf "Task not found: %s" id
      | TaskAlreadyClaimed owner -> Printf.sprintf "Task already claimed by: %s" owner
      | TaskInvalidState (current, expected) ->
          Printf.sprintf "Invalid task state: %s (expected %s)" current expected
      | TaskCycleDetected -> "Task dependency cycle detected")
  | Agent e -> (
      match e with
      | AgentNotFound id -> Printf.sprintf "Agent not found: %s" id
      | AgentTimeout (id, ms) -> Printf.sprintf "Agent %s timed out after %dms" id ms
      | AgentHeartbeatMissing id -> Printf.sprintf "Agent %s heartbeat missing" id
      | AgentCapabilityMismatch cap -> Printf.sprintf "No agent with capability: %s" cap)
  | Federation e -> (
      match e with
      | PortalConnectionFailed addr -> Printf.sprintf "Portal connection failed: %s" addr
      | PortalAuthFailed reason -> Printf.sprintf "Portal auth failed: %s" reason
      | PortalTimeout ms -> Printf.sprintf "Portal timeout after %dms" ms
      | PortalProtocolError msg -> Printf.sprintf "Portal protocol error: %s" msg)
  | Storage e -> (
      match e with
      | FileNotFound path -> Printf.sprintf "File not found: %s" path
      | FilePermissionDenied path -> Printf.sprintf "Permission denied: %s" path
      | FileLocked path -> Printf.sprintf "File locked: %s" path
      | PostgresError msg -> Printf.sprintf "PostgreSQL error: %s" msg
      | GitError msg -> Printf.sprintf "Git error: %s" msg)
  | Mcp e -> (
      match e with
      | McpParseError msg -> Printf.sprintf "MCP parse error: %s" msg
      | McpMethodNotFound method_name -> Printf.sprintf "MCP method not found: %s" method_name
      | McpInvalidParams msg -> Printf.sprintf "MCP invalid params: %s" msg
      | McpAuthError msg -> Printf.sprintf "MCP auth error: %s" msg
      | McpInternalError msg -> Printf.sprintf "MCP internal error: %s" msg)
  | Internal msg -> Printf.sprintf "Internal error: %s" msg

(** {1 Result Helpers} *)

(** Shorthand for error result type *)
type 'a result = ('a, t) Stdlib.result

(** Create an error result *)
let fail e = Error e

(** Create a success result *)
let ok v = Ok v

(** Map error to string for legacy compatibility *)
let to_string_result = function
  | Ok v -> Ok v
  | Error e -> Error (to_string e)

(** Convert string error to Internal error (for migration) *)
let of_string msg = Internal msg

(** {1 Logging Integration} *)

(** Get error severity level *)
type severity = Debug | Info | Warning | Error | Critical

let severity_of_error = function
  | Room (RoomLocked _) -> Warning
  | Task (TaskAlreadyClaimed _) -> Warning
  | Agent (AgentTimeout _) -> Warning
  | Agent (AgentHeartbeatMissing _) -> Warning
  | Federation (PortalTimeout _) -> Warning
  | Storage (FileLocked _) -> Warning
  | Mcp (McpMethodNotFound _) -> Warning
  | Internal _ -> Critical
  | _ -> Error

let string_of_severity = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warning -> "WARN"
  | Error -> "ERROR"
  | Critical -> "CRITICAL"
