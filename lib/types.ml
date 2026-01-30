(** MASC MCP Types - Domain Model *)

(* ============================================ *)
(* Newtypes - Prevent string mixups             *)
(* ============================================ *)

(** Agent identifier - prevents mixing with task_id, file_path, etc. *)
module Agent_id : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end = struct
  type t = string
  let of_string s = s
  let to_string t = t
  let equal = String.equal
  let to_yojson t = `String t
  let of_yojson = function
    | `String s -> Ok s
    | _ -> Error "Expected string for Agent_id"
end

(** Task identifier - prevents mixing with agent_id, etc. *)
module Task_id : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val generate : unit -> t  (* Auto-generate unique ID *)
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end = struct
  type t = string
  let of_string s = s
  let to_string t = t
  let equal = String.equal
  let () = Random.self_init ()  (* Seed RNG on module load *)
  let generate () =
    let timestamp = int_of_float (Unix.gettimeofday () *. 1000.0) in
    let random = Random.int 10000 in  (* 4 digits for less collision *)
    Printf.sprintf "task-%d-%04d" timestamp random
  let to_yojson t = `String t
  let of_yojson = function
    | `String s -> Ok s
    | _ -> Error "Expected string for Task_id"
end

(* ============================================ *)
(* Timestamp utilities                          *)
(* ============================================ *)

(** Timestamp utilities *)
let now_iso () =
  let open Unix in
  let tm = gmtime (gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

(** Parse ISO8601 timestamp to Unix float. Returns default_time on parse failure. *)
let parse_iso8601 ?(default_time = Unix.gettimeofday () -. 60.0) timestamp =
  try
    Scanf.sscanf timestamp "%d-%d-%dT%d:%d:%d"
      (fun y m d h mi s ->
        let tm = Unix.{ tm_sec=s; tm_min=mi; tm_hour=h;
          tm_mday=d; tm_mon=m-1; tm_year=y-1900;
          tm_wday=0; tm_yday=0; tm_isdst=false } in
        fst (Unix.mktime tm))
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> default_time

(** Agent status - compile-time state machine *)
type agent_status =
  | Active
  | Busy
  | Listening
  | Inactive
[@@deriving show { with_path = false }]

let agent_status_to_string = function
  | Active -> "active"
  | Busy -> "busy"
  | Listening -> "listening"
  | Inactive -> "inactive"

(* Alias for dashboard compatibility *)
let string_of_agent_status = agent_status_to_string

let agent_status_of_string_opt = function
  | "active" -> Some Active
  | "busy" -> Some Busy
  | "listening" -> Some Listening
  | "inactive" -> Some Inactive
  | _ -> None

let agent_status_of_string s =
  match agent_status_of_string_opt s with
  | Some status -> status
  | None -> Active  (* Safe default instead of failwith *)

(* Custom yojson converters for lowercase JSON compatibility *)
let agent_status_to_yojson status = `String (agent_status_to_string status)

let agent_status_of_yojson = function
  | `String s ->
      (match agent_status_of_string_opt s with
       | Some status -> Ok status
       | None -> Error ("Unknown agent status: " ^ s))
  | _ -> Error "agent_status: expected string"

(** Agent metadata - session identification and environment info *)
type agent_meta = {
  session_id: string;                     (* short UUID for unique identification *)
  agent_type: string;                     (* claude, gemini, codex *)
  pid: int option; [@default None]        (* process ID *)
  hostname: string option; [@default None] (* machine hostname *)
  tty: string option; [@default None]     (* terminal identifier *)
  worktree: string option; [@default None] (* git worktree path *)
  parent_task: string option; [@default None] (* task that spawned this agent *)
} [@@deriving yojson { strict = false }, show]

(** Agent info *)
type agent = {
  name: string;                           (* unique nickname: claude-swift-fox *)
  agent_type: string; [@default "unknown"] (* original type: claude, gemini, codex *)
  status: agent_status;
  capabilities: string list;
  current_task: string option; [@default None]
  joined_at: string;
  last_seen: string;
  meta: agent_meta option; [@default None] (* session metadata *)
} [@@deriving yojson { strict = false }, show]

(* ============================================ *)
(* Multi-Room Types                             *)
(* ============================================ *)

(** Room metadata - information about a coordination room *)
type room_info = {
  id: string;                                 (* unique ID: slugified name *)
  name: string;                               (* display name *)
  description: string option; [@default None] (* optional description *)
  created_at: string;                         (* ISO timestamp *)
  created_by: string option; [@default None]  (* agent who created the room *)
  agent_count: int; [@default 0]              (* current agent count *)
  task_count: int; [@default 0]               (* active task count *)
} [@@deriving yojson { strict = false }, show]

(** Room registry - tracks all available rooms *)
type room_registry = {
  rooms: room_info list; [@default []]        (* list of rooms *)
  default_room: string; [@default "default"]  (* default room ID *)
  current_room: string option; [@default None] (* currently active room *)
} [@@deriving yojson { strict = false }, show]

(** Task status - state transitions enforced by types *)
type task_status =
  | Todo
  | Claimed of { assignee: string; claimed_at: string }
  | InProgress of { assignee: string; started_at: string }
  | Done of { assignee: string; completed_at: string; notes: string option }
  | Cancelled of { cancelled_by: string; cancelled_at: string; reason: string option }
[@@deriving show]

(* Simple string representation for dashboard *)
let task_status_to_string = function
  | Todo -> "todo"
  | Claimed _ -> "claimed"
  | InProgress _ -> "in_progress"
  | Done _ -> "done"
  | Cancelled _ -> "cancelled"

let string_of_task_status = task_status_to_string

(* Manual yojson conversion for task_status (sum type with records) *)
let task_status_to_yojson = function
  | Todo -> `Assoc [("status", `String "todo")]
  | Claimed { assignee; claimed_at } ->
      `Assoc [
        ("status", `String "claimed");
        ("assignee", `String assignee);
        ("claimed_at", `String claimed_at);
      ]
  | InProgress { assignee; started_at } ->
      `Assoc [
        ("status", `String "in_progress");
        ("assignee", `String assignee);
        ("started_at", `String started_at);
      ]
  | Done { assignee; completed_at; notes } ->
      `Assoc [
        ("status", `String "done");
        ("assignee", `String assignee);
        ("completed_at", `String completed_at);
        ("notes", match notes with Some n -> `String n | None -> `Null);
      ]
  | Cancelled { cancelled_by; cancelled_at; reason } ->
      `Assoc [
        ("status", `String "cancelled");
        ("cancelled_by", `String cancelled_by);
        ("cancelled_at", `String cancelled_at);
        ("reason", match reason with Some r -> `String r | None -> `Null);
      ]

let task_status_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let status = json |> member "status" |> to_string in
    match status with
    | "todo" -> Ok Todo
    | "claimed" ->
        let assignee = json |> member "assignee" |> to_string in
        let claimed_at = json |> member "claimed_at" |> to_string in
        Ok (Claimed { assignee; claimed_at })
    | "in_progress" ->
        let assignee = json |> member "assignee" |> to_string in
        let started_at = json |> member "started_at" |> to_string in
        Ok (InProgress { assignee; started_at })
    | "done" ->
        let assignee = json |> member "assignee" |> to_string in
        let completed_at = json |> member "completed_at" |> to_string in
        let notes = json |> member "notes" |> to_string_option in
        Ok (Done { assignee; completed_at; notes })
    | "cancelled" ->
        let cancelled_by = json |> member "cancelled_by" |> to_string in
        let cancelled_at = json |> member "cancelled_at" |> to_string in
        let reason = json |> member "reason" |> to_string_option in
        Ok (Cancelled { cancelled_by; cancelled_at; reason })
    | s -> Error ("Unknown task status: " ^ s)
  with e -> Error (Printexc.to_string e)

(** Worktree info - tracks which worktree is used for a task *)
type worktree_info = {
  branch: string;                              (* git branch name *)
  path: string;                                (* worktree path relative to git root *)
  git_root: string;                            (* absolute path to .git parent *)
  repo_name: string;                           (* repository name (basename of git_root) *)
} [@@deriving show]

let worktree_info_to_yojson wt =
  `Assoc [
    ("branch", `String wt.branch);
    ("path", `String wt.path);
    ("git_root", `String wt.git_root);
    ("repo_name", `String wt.repo_name);
  ]

let worktree_info_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let branch = json |> member "branch" |> to_string in
    let path = json |> member "path" |> to_string in
    let git_root = json |> member "git_root" |> to_string in
    let repo_name = json |> member "repo_name" |> to_string in
    Ok { branch; path; git_root; repo_name }
  with e -> Error (Printexc.to_string e)

(** Task definition *)
type task = {
  id: string;
  title: string;
  description: string;
  task_status: task_status; [@key "status"]
  priority: int; [@default 3]
  files: string list; [@default []]
  created_at: string;
  worktree: worktree_info option; [@default None]  (* linked worktree info *)
} [@@deriving show]

(* Manual yojson for task *)
let task_to_yojson t =
  let status_json = task_status_to_yojson t.task_status in
  let base = [
    ("id", `String t.id);
    ("title", `String t.title);
    ("description", `String t.description);
    ("priority", `Int t.priority);
    ("files", `List (List.map (fun s -> `String s) t.files));
    ("created_at", `String t.created_at);
  ] in
  (* Add worktree field if present *)
  let with_worktree = match t.worktree with
    | None -> base
    | Some wt -> base @ [("worktree", worktree_info_to_yojson wt)]
  in
  (* Merge status fields into task *)
  match status_json with
  | `Assoc status_fields -> `Assoc (with_worktree @ status_fields)
  | _ -> `Assoc with_worktree

let task_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let id = json |> member "id" |> to_string in
    let title = json |> member "title" |> to_string in
    let description = json |> member "description" |> to_string_option |> Option.value ~default:"" in
    let priority = json |> member "priority" |> to_int_option |> Option.value ~default:3 in
    let files = json |> member "files" |> to_list |> List.map to_string in
    let created_at = json |> member "created_at" |> to_string in
    (* Parse optional worktree field *)
    let worktree = match json |> member "worktree" with
      | `Null -> None
      | wt_json ->
          match worktree_info_of_yojson wt_json with
          | Ok wt -> Some wt
          | Error _ -> None  (* Graceful fallback for backwards compat *)
    in
    match task_status_of_yojson json with
    | Ok task_status -> Ok { id; title; description; task_status; priority; files; created_at; worktree }
    | Error e -> Error e
  with e -> Error (Printexc.to_string e)

(** Message - broadcast or direct *)
type message = {
  seq: int;
  from_agent: string; [@key "from"]
  msg_type: string; [@key "type"] [@default "broadcast"]
  content: string;
  mention: string option; [@default None]
  timestamp: string;
} [@@deriving yojson { strict = false }, show]

(** Room state *)
type room_state = {
  protocol_version: string;
  project: string;
  started_at: string;
  message_seq: int;
  active_agents: string list;
  paused: bool; [@default false]  (** Global pause flag - when true, orchestrator won't spawn *)
  pause_reason: string option; [@default None]  (** Reason for pause *)
  paused_by: string option; [@default None]  (** Who paused the room *)
  paused_at: string option; [@default None]  (** When paused *)
} [@@deriving yojson { strict = false }, show]

(* ============================================ *)
(* Tempo configuration for cluster pace control *)
(* ============================================ *)

(** Tempo mode - controls cluster execution pace *)
type tempo_mode =
  | Normal    (* Default speed *)
  | Slow      (* Slow pace - careful work *)
  | Fast      (* Fast pace - simple tasks *)
  | Paused    (* Temporarily paused *)
[@@deriving show { with_path = false }]

let tempo_mode_to_string = function
  | Normal -> "normal"
  | Slow -> "slow"
  | Fast -> "fast"
  | Paused -> "paused"

(* Alias for dashboard compatibility *)
let string_of_tempo_mode = tempo_mode_to_string

let tempo_mode_of_string = function
  | "normal" -> Ok Normal
  | "slow" -> Ok Slow
  | "fast" -> Ok Fast
  | "paused" -> Ok Paused
  | s -> Error ("Unknown tempo mode: " ^ s)

let tempo_mode_to_yojson mode = `String (tempo_mode_to_string mode)

let tempo_mode_of_yojson = function
  | `String s -> tempo_mode_of_string s
  | _ -> Error "Expected string for tempo_mode"

(** Tempo configuration *)
type tempo_config = {
  mode: tempo_mode;
  delay_ms: int;             (* Delay between operations in milliseconds *)
  reason: string option;     (* Why this tempo was set *)
  set_by: string option;     (* Who set this tempo *)
  set_at: string option;     (* When this tempo was set *)
} [@@deriving show]

let default_tempo_config = {
  mode = Normal;
  delay_ms = 0;
  reason = None;
  set_by = None;
  set_at = None;
}

let tempo_config_to_yojson c =
  `Assoc [
    ("mode", tempo_mode_to_yojson c.mode);
    ("delay_ms", `Int c.delay_ms);
    ("reason", match c.reason with Some r -> `String r | None -> `Null);
    ("set_by", match c.set_by with Some s -> `String s | None -> `Null);
    ("set_at", match c.set_at with Some t -> `String t | None -> `Null);
  ]

let tempo_config_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let mode_str = json |> member "mode" |> to_string in
    let delay_ms = json |> member "delay_ms" |> to_int_option |> Option.value ~default:0 in
    let reason = json |> member "reason" |> to_string_option in
    let set_by = json |> member "set_by" |> to_string_option in
    let set_at = json |> member "set_at" |> to_string_option in
    match tempo_mode_of_string mode_str with
    | Ok mode -> Ok { mode; delay_ms; reason; set_by; set_at }
    | Error e -> Error e
  with e -> Error (Printexc.to_string e)

(** Backlog (task collection) *)
type backlog = {
  tasks: task list;
  last_updated: string;
  version: int;
} [@@deriving show]

let backlog_to_yojson b =
  `Assoc [
    ("tasks", `List (List.map task_to_yojson b.tasks));
    ("last_updated", `String b.last_updated);
    ("version", `Int b.version);
  ]

let backlog_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let tasks_json = json |> member "tasks" |> to_list in
    let tasks = List.filter_map (fun j ->
      match task_of_yojson j with Ok t -> Some t | Error _ -> None
    ) tasks_json in
    let last_updated = json |> member "last_updated" |> to_string in
    let version = json |> member "version" |> to_int in
    Ok { tasks; last_updated; version }
  with e -> Error (Printexc.to_string e)

(** A2A Task status - enforced at compile time *)
type a2a_task_status =
  | A2APending
  | A2ARunning
  | A2ACompleted
  | A2AFailed
  | A2ACanceled
[@@deriving show { with_path = false }]

let a2a_task_status_to_string = function
  | A2APending -> "pending"
  | A2ARunning -> "running"
  | A2ACompleted -> "completed"
  | A2AFailed -> "failed"
  | A2ACanceled -> "canceled"

let a2a_task_status_of_string = function
  | "pending" -> Ok A2APending
  | "running" -> Ok A2ARunning
  | "completed" -> Ok A2ACompleted
  | "failed" -> Ok A2AFailed
  | "canceled" -> Ok A2ACanceled
  | s -> Error ("Unknown A2A task status: " ^ s)

let a2a_task_status_to_yojson s = `String (a2a_task_status_to_string s)

let a2a_task_status_of_yojson = function
  | `String s -> a2a_task_status_of_string s
  | _ -> Error "Expected string for A2A task status"

(** Portal status - enforced at compile time *)
type portal_state =
  | PortalOpen
  | PortalClosed
[@@deriving show { with_path = false }]

let portal_state_to_string = function
  | PortalOpen -> "open"
  | PortalClosed -> "closed"

let portal_state_of_string = function
  | "open" -> Ok PortalOpen
  | "closed" -> Ok PortalClosed
  | s -> Error ("Unknown portal state: " ^ s)

let portal_state_to_yojson s = `String (portal_state_to_string s)

let portal_state_of_yojson = function
  | `String s -> portal_state_of_string s
  | _ -> Error "Expected string for portal state"

(** A2A Task - Google A2A Protocol task object *)
type a2a_task = {
  a2a_id: string; [@key "id"]
  from_agent: string; [@key "from"]
  to_agent: string; [@key "to"]
  a2a_message: string; [@key "message"]
  a2a_status: a2a_task_status; [@key "status"]
  a2a_result: string option; [@key "result"] [@default None]
  created_at: string; [@key "createdAt"]
  updated_at: string; [@key "updatedAt"]
} [@@deriving show]

(* Manual JSON conversion for a2a_task *)
let a2a_task_to_yojson t =
  `Assoc [
    ("id", `String t.a2a_id);
    ("from", `String t.from_agent);
    ("to", `String t.to_agent);
    ("message", `String t.a2a_message);
    ("status", a2a_task_status_to_yojson t.a2a_status);
    ("result", match t.a2a_result with Some r -> `String r | None -> `Null);
    ("createdAt", `String t.created_at);
    ("updatedAt", `String t.updated_at);
  ]

let a2a_task_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let a2a_id = json |> member "id" |> to_string in
    let from_agent = json |> member "from" |> to_string in
    let to_agent = json |> member "to" |> to_string in
    let a2a_message = json |> member "message" |> to_string in
    let status_str = json |> member "status" |> to_string in
    let a2a_result = json |> member "result" |> to_string_option in
    let created_at = json |> member "createdAt" |> to_string in
    let updated_at = json |> member "updatedAt" |> to_string in
    match a2a_task_status_of_string status_str with
    | Ok a2a_status -> Ok { a2a_id; from_agent; to_agent; a2a_message; a2a_status; a2a_result; created_at; updated_at }
    | Error e -> Error e
  with e -> Error (Printexc.to_string e)

(** Portal - bidirectional A2A connection *)
type portal = {
  portal_from: string; [@key "from"]
  portal_target: string; [@key "target"]
  portal_opened_at: string; [@key "openedAt"]
  portal_status: portal_state; [@key "status"]
  task_count: int; [@key "taskCount"]
} [@@deriving show]

(* Manual JSON conversion for portal *)
let portal_to_yojson p =
  `Assoc [
    ("from", `String p.portal_from);
    ("target", `String p.portal_target);
    ("openedAt", `String p.portal_opened_at);
    ("status", portal_state_to_yojson p.portal_status);
    ("taskCount", `Int p.task_count);
  ]

let portal_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let portal_from = json |> member "from" |> to_string in
    let portal_target = json |> member "target" |> to_string in
    let portal_opened_at = json |> member "openedAt" |> to_string in
    let status_str = json |> member "status" |> to_string in
    let task_count = json |> member "taskCount" |> to_int in
    match portal_state_of_string status_str with
    | Ok portal_status -> Ok { portal_from; portal_target; portal_opened_at; portal_status; task_count }
    | Error e -> Error e
  with e -> Error (Printexc.to_string e)

(** SSE Session info (for tracking connected agents) *)
type sse_session = {
  agent_name: string;
  connected_at: string;
  last_activity: float; (* Unix timestamp for easy comparison *)
  is_listening: bool;
} [@@deriving show]

(** MCP Tool result *)
type tool_result = {
  success: bool;
  message: string;
  data: Yojson.Safe.t option; [@default None]
} [@@deriving show]

let tool_result_to_yojson r =
  let base = [
    ("success", `Bool r.success);
    ("message", `String r.message);
  ] in
  match r.data with
  | Some d -> `Assoc (base @ [("data", d)])
  | None -> `Assoc base

(** Tool schema for MCP *)
type tool_schema = {
  name: string;
  description: string;
  input_schema: Yojson.Safe.t;
}

(** Rate limit config *)
type rate_limit_config = {
  per_minute: int;
  burst_allowed: int;
  priority_agents: string list;
  (* Role-based multipliers *)
  reader_multiplier: float;
  worker_multiplier: float;
  admin_multiplier: float;
  (* Tool category limits *)
  broadcast_per_minute: int;
  task_ops_per_minute: int;
} [@@deriving show]

let default_rate_limit = {
  per_minute = 10;
  burst_allowed = 5;
  priority_agents = [];
  reader_multiplier = 0.5;   (* Readers get 50% of base *)
  worker_multiplier = 1.0;   (* Workers get 100% *)
  admin_multiplier = 2.0;    (* Admins get 200% *)
  broadcast_per_minute = 15;
  task_ops_per_minute = 30;
}

let rate_limit_config_to_yojson c =
  `Assoc [
    ("per_minute", `Int c.per_minute);
    ("burst_allowed", `Int c.burst_allowed);
    ("priority_agents", `List (List.map (fun s -> `String s) c.priority_agents));
    ("reader_multiplier", `Float c.reader_multiplier);
    ("worker_multiplier", `Float c.worker_multiplier);
    ("admin_multiplier", `Float c.admin_multiplier);
    ("broadcast_per_minute", `Int c.broadcast_per_minute);
    ("task_ops_per_minute", `Int c.task_ops_per_minute);
  ]

let rate_limit_config_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let per_minute = json |> member "per_minute" |> to_int_option |> Option.value ~default:10 in
    let burst_allowed = json |> member "burst_allowed" |> to_int_option |> Option.value ~default:5 in
    let priority_agents =
      match json |> member "priority_agents" with
      | `Null -> default_rate_limit.priority_agents
      | `List items -> List.map to_string items
      | _ -> failwith "priority_agents must be a list"
    in
    let reader_multiplier = json |> member "reader_multiplier" |> to_float_option |> Option.value ~default:0.5 in
    let worker_multiplier = json |> member "worker_multiplier" |> to_float_option |> Option.value ~default:1.0 in
    let admin_multiplier = json |> member "admin_multiplier" |> to_float_option |> Option.value ~default:2.0 in
    let broadcast_per_minute = json |> member "broadcast_per_minute" |> to_int_option |> Option.value ~default:15 in
    let task_ops_per_minute = json |> member "task_ops_per_minute" |> to_int_option |> Option.value ~default:30 in
    Ok { per_minute; burst_allowed; priority_agents; reader_multiplier; worker_multiplier; admin_multiplier;
         broadcast_per_minute; task_ops_per_minute }
  with e -> Error (Printexc.to_string e)

(** Rate limit categories *)
type rate_limit_category =
  | GeneralLimit
  | BroadcastLimit
  | TaskOpsLimit
[@@deriving show { with_path = false }]

(** Get base limit for category *)
let limit_for_category config = function
  | GeneralLimit -> config.per_minute
  | BroadcastLimit -> config.broadcast_per_minute
  | TaskOpsLimit -> config.task_ops_per_minute

(** Map tool to rate limit category *)
let category_for_tool = function
  | "masc_broadcast" | "masc_listen" -> BroadcastLimit
  | "masc_add_task" | "masc_claim" | "masc_claim_next" | "masc_done"
  | "masc_update_priority" | "masc_transition" | "masc_release" -> TaskOpsLimit
  | _ -> GeneralLimit

(** Rate limit error - returned when limit exceeded *)
type rate_limit_error = {
  limit: int;
  current: int;
  wait_seconds: int;
  category: rate_limit_category;
} [@@deriving show]

(** MASC Error types - compile-time error handling *)
type masc_error =
  | NotInitialized
  | AlreadyInitialized
  | AgentNotFound of string
  | AgentAlreadyJoined of string
  | TaskNotFound of string
  | TaskAlreadyClaimed of { task_id: string; by: string }
  | TaskNotClaimed of string
  | TaskInvalidState of string  (* For cancelled tasks or invalid state transitions *)
  | PortalNotOpen of string
  | PortalAlreadyOpen of { agent: string; target: string }
  | PortalClosed of string
  | InvalidJson of string
  | IoError of string
  | InvalidAgentName of string
  | InvalidTaskId of string
  | InvalidFilePath of string
  (* Auth errors *)
  | Unauthorized of string        (* Missing or invalid token *)
  | Forbidden of { agent: string; action: string }  (* Valid token but no permission *)
  | TokenExpired of string
  | InvalidToken of string
  (* Rate limit errors *)
  | RateLimitExceeded of rate_limit_error
[@@deriving show { with_path = false }]

(** Convert error to user-friendly message *)
let masc_error_to_string = function
  | NotInitialized -> "❌ MASC not initialized. Use masc_init first."
  | AlreadyInitialized -> "MASC already initialized."
  | AgentNotFound name -> Printf.sprintf "❌ Agent not found: %s" name
  | AgentAlreadyJoined name -> Printf.sprintf "⚠ %s is already in the room" name
  | TaskNotFound id -> Printf.sprintf "❌ Task not found: %s" id
  | TaskAlreadyClaimed { task_id; by } -> Printf.sprintf "❌ Task %s already claimed by %s" task_id by
  | TaskNotClaimed id -> Printf.sprintf "❌ Task %s is not claimed" id
  | TaskInvalidState msg -> Printf.sprintf "❌ Invalid task state: %s" msg
  | PortalNotOpen agent -> Printf.sprintf "❌ No portal open for %s. Use masc_portal_open first." agent
  | PortalAlreadyOpen { agent; target } -> Printf.sprintf "⚠ Portal already open: %s ↔ %s" agent target
  | PortalClosed agent -> Printf.sprintf "❌ Portal is closed for %s. Use masc_portal_open to reopen." agent
  | InvalidJson msg -> Printf.sprintf "❌ Invalid JSON: %s" msg
  | IoError msg -> Printf.sprintf "❌ IO error: %s" msg
  | InvalidAgentName reason -> Printf.sprintf "❌ Invalid agent name: %s" reason
  | InvalidTaskId reason -> Printf.sprintf "❌ Invalid task ID: %s" reason
  | InvalidFilePath reason -> Printf.sprintf "❌ Invalid file path: %s" reason
  | Unauthorized reason -> Printf.sprintf "🔐 Unauthorized: %s" reason
  | Forbidden { agent; action } -> Printf.sprintf "🚫 Forbidden: %s cannot %s" agent action
  | TokenExpired agent -> Printf.sprintf "⏰ Token expired for %s. Use masc_auth_refresh." agent
  | InvalidToken reason -> Printf.sprintf "🔑 Invalid token: %s" reason
  | RateLimitExceeded { limit; current; wait_seconds; category } ->
      Printf.sprintf "⏳ Rate limit exceeded (%s): %d/%d requests. Wait %d seconds."
        (show_rate_limit_category category) current limit wait_seconds

(** Result type alias for MASC operations *)
type 'a masc_result = ('a, masc_error) result

(* ============================================ *)
(* Authentication & Authorization Types         *)
(* ============================================ *)

(** Agent role - enforced permission levels *)
type agent_role =
  | Reader    (* Can read state, cannot modify *)
  | Worker    (* Can claim tasks, lock files, broadcast *)
  | Admin     (* Full access: init, reset, manage agents *)
[@@deriving show { with_path = false }]

let agent_role_to_string = function
  | Reader -> "reader"
  | Worker -> "worker"
  | Admin -> "admin"

let agent_role_of_string = function
  | "reader" -> Ok Reader
  | "worker" -> Ok Worker
  | "admin" -> Ok Admin
  | s -> Error ("Unknown agent role: " ^ s)

let agent_role_to_yojson r = `String (agent_role_to_string r)

let agent_role_of_yojson = function
  | `String s -> agent_role_of_string s
  | _ -> Error "Expected string for agent_role"

(** Agent credential - stored in .masc/auth/ *)
type agent_credential = {
  agent_name: string;
  token: string;        (* SHA256 hash of secret *)
  role: agent_role;
  created_at: string;
  expires_at: string option; [@default None]
} [@@deriving show]

let agent_credential_to_yojson c =
  let base = [
    ("agent_name", `String c.agent_name);
    ("token", `String c.token);
    ("role", agent_role_to_yojson c.role);
    ("created_at", `String c.created_at);
  ] in
  match c.expires_at with
  | Some exp -> `Assoc (base @ [("expires_at", `String exp)])
  | None -> `Assoc base

let agent_credential_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let agent_name = json |> member "agent_name" |> to_string in
    let token = json |> member "token" |> to_string in
    let role_str = json |> member "role" |> to_string in
    let created_at = json |> member "created_at" |> to_string in
    let expires_at = json |> member "expires_at" |> to_string_option in
    match agent_role_of_string role_str with
    | Ok role -> Ok { agent_name; token; role; created_at; expires_at }
    | Error e -> Error e
  with e -> Error (Printexc.to_string e)

(** Auth config - room-level settings *)
type auth_config = {
  enabled: bool;
  room_secret_hash: string option; [@default None]  (* SHA256 of room secret *)
  require_token: bool; [@default false]
  default_role: agent_role; [@default Worker]
  token_expiry_hours: int; [@default 24]
} [@@deriving show]

let default_auth_config = {
  enabled = false;
  room_secret_hash = None;
  require_token = false;
  default_role = Worker;
  token_expiry_hours = 24;
}

let auth_config_to_yojson c =
  `Assoc [
    ("enabled", `Bool c.enabled);
    ("room_secret_hash", match c.room_secret_hash with Some h -> `String h | None -> `Null);
    ("require_token", `Bool c.require_token);
    ("default_role", agent_role_to_yojson c.default_role);
    ("token_expiry_hours", `Int c.token_expiry_hours);
  ]

let auth_config_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let enabled = json |> member "enabled" |> to_bool in
    let room_secret_hash = json |> member "room_secret_hash" |> to_string_option in
    let require_token = json |> member "require_token" |> to_bool_option |> Option.value ~default:false in
    let default_role_str = json |> member "default_role" |> to_string_option |> Option.value ~default:"worker" in
    let token_expiry_hours = json |> member "token_expiry_hours" |> to_int_option |> Option.value ~default:24 in
    match agent_role_of_string default_role_str with
    | Ok default_role -> Ok { enabled; room_secret_hash; require_token; default_role; token_expiry_hours }
    | Error e -> Error e
  with e -> Error (Printexc.to_string e)

(** Permission matrix - what each role can do *)
type permission =
  | CanInit
  | CanReset
  | CanJoin
  | CanLeave
  | CanReadState
  | CanAddTask
  | CanClaimTask
  | CanCompleteTask
  | CanBroadcast
  | CanOpenPortal
  | CanSendPortal
  | CanCreateWorktree
  | CanRemoveWorktree
  | CanVote
  | CanInterrupt
  | CanApprove
[@@deriving show { with_path = false }]

(** Get permissions for a role *)
let permissions_for_role = function
  | Reader -> [CanReadState; CanJoin; CanLeave]
  | Worker -> [
      CanReadState; CanJoin; CanLeave;
      CanAddTask; CanClaimTask; CanCompleteTask;
      CanBroadcast;
      CanOpenPortal; CanSendPortal;
      CanCreateWorktree; CanRemoveWorktree;
      CanVote;
    ]
  | Admin -> [
      CanInit; CanReset;
      CanReadState; CanJoin; CanLeave;
      CanAddTask; CanClaimTask; CanCompleteTask;
      CanBroadcast;
      CanOpenPortal; CanSendPortal;
      CanCreateWorktree; CanRemoveWorktree;
      CanVote; CanInterrupt; CanApprove;
    ]

(** Check if role has permission *)
let has_permission role permission =
  List.mem permission (permissions_for_role role)

(* ============================================ *)
(* Rate limit role integration                  *)
(* ============================================ *)

(** Get role multiplier for rate limits *)
let multiplier_for_role config = function
  | Reader -> config.reader_multiplier
  | Worker -> config.worker_multiplier
  | Admin -> config.admin_multiplier

(** Compute effective limit for role and category *)
let effective_limit config ~role ~category =
  let base = limit_for_category config category in
  let mult = multiplier_for_role config role in
  int_of_float (float_of_int base *. mult)
