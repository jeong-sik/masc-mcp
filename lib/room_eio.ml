(** Room_eio: OCaml 5.x Eio-native Room implementation

    Direct-style async I/O using Eio, no Lwt monads!

    This module provides coordination primitives for multi-agent systems:
    - Agent registration/heartbeat
    - File locking
    - Message broadcasting
    - Task management

    Migration path: Room -> Room_eio
*)

(** {1 Types} *)

(** Room configuration for Eio backend *)
type config = {
  base_path: string;
  lock_expiry_minutes: int;
  backend: Backend_eio.FileSystem.t;
  fs: Eio.Fs.dir_ty Eio.Path.t;
}

(** Agent state *)
type agent_state = {
  name: string;
  last_seen: float;
  capabilities: string list;
  status: string;
}

(** Room state *)
type room_state = {
  protocol_version: string;
  started_at: float;
  last_updated: float;
  active_agents: string list;
  message_seq: int;
  mode: string;
  paused: bool;
  paused_by: string option;
  paused_at: float option;
  pause_reason: string option;
}

(** {1 Helpers} *)

let now_iso () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    (int_of_float ((t -. floor t) *. 1000.))

(** {1 Configuration} *)

(** Create Eio-native room configuration *)
let create_config ~fs base_path =
  let backend_config = Backend_eio.{
    base_path = Filename.concat base_path ".masc";
    node_id = Printf.sprintf "node_%d" (Unix.getpid ());
    cluster_name = "default";
  } in
  let backend = Backend_eio.FileSystem.create ~fs backend_config in
  {
    base_path;
    lock_expiry_minutes = 30;
    backend;
    fs;
  }

(** Create test configuration (isolated) *)
let test_config ~fs base_path =
  let backend_config = Backend_eio.{
    base_path = Filename.concat base_path ".masc";
    node_id = Printf.sprintf "test_node_%d" (Random.int 100000);
    cluster_name = "test";
  } in
  let backend = Backend_eio.FileSystem.create ~fs backend_config in
  {
    base_path;
    lock_expiry_minutes = 5;  (* Shorter for tests *)
    backend;
    fs;
  }

(** {1 Key Utilities} *)

let agents_key = "agents"
let tasks_key = "tasks"
let messages_key = "messages"
let locks_key = "locks"
let state_key = "state"

let agent_key name = Printf.sprintf "%s:%s" agents_key name
let task_key id = Printf.sprintf "%s:%s" tasks_key id
let message_key seq = Printf.sprintf "%s:%06d" messages_key seq
let lock_key resource = Printf.sprintf "%s:%s" locks_key resource

(** {1 State Management} *)

let default_room_state () = {
  protocol_version = "1.0.0";
  started_at = Unix.gettimeofday ();
  last_updated = Unix.gettimeofday ();
  active_agents = [];
  message_seq = 0;
  mode = "collaborative";
  paused = false;
  paused_by = None;
  paused_at = None;
  pause_reason = None;
}

let room_state_to_json state =
  `Assoc [
    ("protocol_version", `String state.protocol_version);
    ("started_at", `Float state.started_at);
    ("last_updated", `Float state.last_updated);
    ("active_agents", `List (List.map (fun s -> `String s) state.active_agents));
    ("message_seq", `Int state.message_seq);
    ("mode", `String state.mode);
    ("paused", `Bool state.paused);
    ("paused_by", match state.paused_by with Some s -> `String s | None -> `Null);
    ("paused_at", match state.paused_at with Some f -> `Float f | None -> `Null);
    ("pause_reason", match state.pause_reason with Some s -> `String s | None -> `Null);
  ]

let room_state_of_json json =
  let open Yojson.Safe.Util in
  try
    Ok {
      protocol_version = json |> member "protocol_version" |> to_string;
      started_at = json |> member "started_at" |> to_float;
      last_updated = json |> member "last_updated" |> to_float;
      active_agents = json |> member "active_agents" |> to_list |> List.map to_string;
      message_seq = json |> member "message_seq" |> to_int;
      mode = json |> member "mode" |> to_string;
      paused = json |> member "paused" |> to_bool;
      paused_by = json |> member "paused_by" |> to_string_option;
      paused_at = json |> member "paused_at" |> to_float_option;
      pause_reason = json |> member "pause_reason" |> to_string_option;
    }
  with e ->
    Error (Printexc.to_string e)

(** Read room state *)
let read_state config =
  match Backend_eio.FileSystem.get config.backend state_key with
  | Ok json_str ->
      (try
        let json = Yojson.Safe.from_string json_str in
        room_state_of_json json
      with e -> Error (Printexc.to_string e))
  | Error (Backend_eio.NotFound _) ->
      Ok (default_room_state ())
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | Backend_eio.NotFound k -> "Not found: " ^ k
        | Backend_eio.AlreadyExists k -> "Already exists: " ^ k
        | Backend_eio.InvalidKey k -> "Invalid key: " ^ k)

(** Write room state *)
let write_state config state =
  let state = { state with last_updated = Unix.gettimeofday () } in
  let json_str = Yojson.Safe.to_string (room_state_to_json state) in
  Backend_eio.FileSystem.set config.backend state_key json_str

(** {1 Agent Operations} *)

let agent_state_to_json agent =
  `Assoc [
    ("name", `String agent.name);
    ("last_seen", `Float agent.last_seen);
    ("capabilities", `List (List.map (fun s -> `String s) agent.capabilities));
    ("status", `String agent.status);
  ]

let agent_state_of_json json =
  let open Yojson.Safe.Util in
  try
    Ok {
      name = json |> member "name" |> to_string;
      last_seen = json |> member "last_seen" |> to_float;
      capabilities = json |> member "capabilities" |> to_list |> List.map to_string;
      status = json |> member "status" |> to_string;
    }
  with e ->
    Error (Printexc.to_string e)

(** Register agent or update heartbeat *)
let register_agent config ~name ?(capabilities=[]) () =
  let agent = {
    name;
    last_seen = Unix.gettimeofday ();
    capabilities;
    status = "active";
  } in
  let json_str = Yojson.Safe.to_string (agent_state_to_json agent) in
  match Backend_eio.FileSystem.set config.backend (agent_key name) json_str with
  | Ok () ->
      (* Update room state to include this agent *)
      (match read_state config with
       | Ok state ->
           let active_agents =
             if List.mem name state.active_agents then state.active_agents
             else name :: state.active_agents
           in
           let _ = write_state config { state with active_agents } in
           Ok agent
       | Error e -> Error e)
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Failed to register agent")

(** Get agent state *)
let get_agent config ~name =
  match Backend_eio.FileSystem.get config.backend (agent_key name) with
  | Ok json_str ->
      (try
        let json = Yojson.Safe.from_string json_str in
        agent_state_of_json json
      with e -> Error (Printexc.to_string e))
  | Error (Backend_eio.NotFound _) ->
      Error "Agent not found"
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Failed to get agent")

(** Remove agent *)
let remove_agent config ~name =
  match Backend_eio.FileSystem.delete config.backend (agent_key name) with
  | Ok () ->
      (* Update room state to remove this agent *)
      (match read_state config with
       | Ok state ->
           let active_agents = List.filter (fun n -> n <> name) state.active_agents in
           let _ = write_state config { state with active_agents } in
           Ok ()
       | Error e -> Error e)
  | Error (Backend_eio.NotFound _) ->
      Ok ()  (* Already removed *)
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Failed to remove agent")

(** {1 Lock Operations} *)

type lock_info = {
  resource: string;
  owner: string;
  acquired_at: float;
  expires_at: float;
}

let lock_info_to_json lock =
  `Assoc [
    ("resource", `String lock.resource);
    ("owner", `String lock.owner);
    ("acquired_at", `Float lock.acquired_at);
    ("expires_at", `Float lock.expires_at);
  ]

let lock_info_of_json json =
  let open Yojson.Safe.Util in
  try
    Ok {
      resource = json |> member "resource" |> to_string;
      owner = json |> member "owner" |> to_string;
      acquired_at = json |> member "acquired_at" |> to_float;
      expires_at = json |> member "expires_at" |> to_float;
    }
  with e ->
    Error (Printexc.to_string e)

(** Acquire lock on a resource *)
let acquire_lock config ~resource ~owner =
  let ttl_seconds = config.lock_expiry_minutes * 60 in
  match Backend_eio.FileSystem.acquire_lock config.backend
          ~key:resource ~owner ~ttl_seconds with
  | Ok true ->
      let lock = {
        resource;
        owner;
        acquired_at = Unix.gettimeofday ();
        expires_at = Unix.gettimeofday () +. float_of_int ttl_seconds;
      } in
      Ok (Some lock)
  | Ok false ->
      Ok None  (* Lock held by someone else *)
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Failed to acquire lock")

(** Release lock *)
let release_lock config ~resource ~owner =
  match Backend_eio.FileSystem.release_lock config.backend ~key:resource ~owner with
  | Ok true -> Ok ()
  | Ok false -> Error "Not lock owner"
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Failed to release lock")

(** Extend lock TTL *)
let extend_lock config ~resource ~owner =
  let ttl_seconds = config.lock_expiry_minutes * 60 in
  match Backend_eio.FileSystem.extend_lock config.backend
          ~key:resource ~owner ~ttl_seconds with
  | Ok true -> Ok ()
  | Ok false -> Error "Not lock owner or lock expired"
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Failed to extend lock")

(** {1 Message Operations} *)

type message = {
  seq: int;
  from_agent: string;
  content: string;
  mention: string option;
  timestamp: float;
}

let message_to_json msg =
  `Assoc [
    ("seq", `Int msg.seq);
    ("from", `String msg.from_agent);
    ("content", `String msg.content);
    ("mention", match msg.mention with Some m -> `String m | None -> `Null);
    ("timestamp", `Float msg.timestamp);
  ]

let message_of_json json =
  let open Yojson.Safe.Util in
  try
    Ok {
      seq = json |> member "seq" |> to_int;
      from_agent = json |> member "from" |> to_string;
      content = json |> member "content" |> to_string;
      mention = json |> member "mention" |> to_string_option;
      timestamp = json |> member "timestamp" |> to_float;
    }
  with e ->
    Error (Printexc.to_string e)

(** Extract @mention from message content *)
let extract_mention content =
  let re = Str.regexp "@\\([a-zA-Z0-9_-]+\\)" in
  try
    let _ = Str.search_forward re content 0 in
    Some (Str.matched_group 1 content)
  with Not_found -> None

(** Broadcast message to room *)
let broadcast config ~from_agent ~content =
  match read_state config with
  | Error e -> Error e
  | Ok state ->
      let seq = state.message_seq + 1 in
      let msg = {
        seq;
        from_agent;
        content;
        mention = extract_mention content;
        timestamp = Unix.gettimeofday ();
      } in
      let json_str = Yojson.Safe.to_string (message_to_json msg) in
      match Backend_eio.FileSystem.set config.backend (message_key seq) json_str with
      | Ok () ->
          let _ = write_state config { state with message_seq = seq } in
          Ok msg
      | Error e ->
          Error (match e with
            | Backend_eio.IOError msg -> msg
            | _ -> "Failed to broadcast message")

(** Get message by sequence number *)
let get_message config ~seq =
  match Backend_eio.FileSystem.get config.backend (message_key seq) with
  | Ok json_str ->
      (try
        let json = Yojson.Safe.from_string json_str in
        message_of_json json
      with e -> Error (Printexc.to_string e))
  | Error (Backend_eio.NotFound _) ->
      Error "Message not found"
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Failed to get message")

(** {1 Task Operations} *)

type task_status =
  | Pending
  | InProgress of string  (* agent_id *)
  | Completed of string   (* agent_id *)
  | Failed of string * string  (* agent_id, reason *)

type task = {
  id: string;
  description: string;
  status: task_status;
  created_at: float;
  updated_at: float;
  priority: int;
}

let task_status_to_json = function
  | Pending -> `Assoc [("type", `String "pending")]
  | InProgress agent -> `Assoc [("type", `String "in_progress"); ("agent", `String agent)]
  | Completed agent -> `Assoc [("type", `String "completed"); ("agent", `String agent)]
  | Failed (agent, reason) -> `Assoc [("type", `String "failed"); ("agent", `String agent); ("reason", `String reason)]

let task_status_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string with
  | "pending" -> Ok Pending
  | "in_progress" -> Ok (InProgress (json |> member "agent" |> to_string))
  | "completed" -> Ok (Completed (json |> member "agent" |> to_string))
  | "failed" -> Ok (Failed (json |> member "agent" |> to_string, json |> member "reason" |> to_string))
  | s -> Error ("Unknown task status: " ^ s)

let task_to_json task =
  `Assoc [
    ("id", `String task.id);
    ("description", `String task.description);
    ("status", task_status_to_json task.status);
    ("created_at", `Float task.created_at);
    ("updated_at", `Float task.updated_at);
    ("priority", `Int task.priority);
  ]

let task_of_json json =
  let open Yojson.Safe.Util in
  try
    match task_status_of_json (json |> member "status") with
    | Error e -> Error e
    | Ok status ->
        Ok {
          id = json |> member "id" |> to_string;
          description = json |> member "description" |> to_string;
          status;
          created_at = json |> member "created_at" |> to_float;
          updated_at = json |> member "updated_at" |> to_float;
          priority = json |> member "priority" |> to_int;
        }
  with e ->
    Error (Printexc.to_string e)

(** Create a new task *)
let create_task config ~description ?(priority=1) () =
  let id = Printf.sprintf "task_%d_%d" (int_of_float (Unix.gettimeofday () *. 1000.)) (Random.int 10000) in
  let now = Unix.gettimeofday () in
  let task = {
    id;
    description;
    status = Pending;
    created_at = now;
    updated_at = now;
    priority;
  } in
  let json_str = Yojson.Safe.to_string (task_to_json task) in
  match Backend_eio.FileSystem.set config.backend (task_key id) json_str with
  | Ok () -> Ok task
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Failed to create task")

(** Claim a task (set to in_progress) *)
let claim_task config ~task_id ~agent =
  match Backend_eio.FileSystem.get config.backend (task_key task_id) with
  | Error (Backend_eio.NotFound _) -> Error "Task not found"
  | Error e -> Error (match e with Backend_eio.IOError msg -> msg | _ -> "Get failed")
  | Ok json_str ->
      (try
        let json = Yojson.Safe.from_string json_str in
        match task_of_json json with
        | Error e -> Error e
        | Ok task ->
            match task.status with
            | Pending ->
                let updated = { task with
                  status = InProgress agent;
                  updated_at = Unix.gettimeofday ();
                } in
                let new_json = Yojson.Safe.to_string (task_to_json updated) in
                (match Backend_eio.FileSystem.set config.backend (task_key task_id) new_json with
                 | Ok () -> Ok updated
                 | Error _ -> Error "Failed to update task")
            | InProgress other when other = agent ->
                Ok task  (* Already claimed by this agent *)
            | InProgress _ ->
                Error "Task already claimed by another agent"
            | Completed _ ->
                Error "Task already completed"
            | Failed _ ->
                Error "Task failed"
      with e -> Error (Printexc.to_string e))

(** Complete a task *)
let complete_task config ~task_id ~agent =
  match Backend_eio.FileSystem.get config.backend (task_key task_id) with
  | Error (Backend_eio.NotFound _) -> Error "Task not found"
  | Error e -> Error (match e with Backend_eio.IOError msg -> msg | _ -> "Get failed")
  | Ok json_str ->
      (try
        let json = Yojson.Safe.from_string json_str in
        match task_of_json json with
        | Error e -> Error e
        | Ok task ->
            match task.status with
            | InProgress claimer when claimer = agent ->
                let updated = { task with
                  status = Completed agent;
                  updated_at = Unix.gettimeofday ();
                } in
                let new_json = Yojson.Safe.to_string (task_to_json updated) in
                (match Backend_eio.FileSystem.set config.backend (task_key task_id) new_json with
                 | Ok () -> Ok updated
                 | Error _ -> Error "Failed to update task")
            | InProgress _ ->
                Error "Task claimed by another agent"
            | Pending ->
                Error "Task not claimed"
            | Completed _ ->
                Error "Task already completed"
            | Failed _ ->
                Error "Task failed"
      with e -> Error (Printexc.to_string e))

(** Get task by ID *)
let get_task config ~task_id =
  match Backend_eio.FileSystem.get config.backend (task_key task_id) with
  | Ok json_str ->
      (try
        let json = Yojson.Safe.from_string json_str in
        task_of_json json
      with e -> Error (Printexc.to_string e))
  | Error (Backend_eio.NotFound _) ->
      Error "Task not found"
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Failed to get task")

(** {1 Health Check} *)

let health_check config =
  match Backend_eio.FileSystem.health_check config.backend with
  | Ok result -> Ok result
  | Error e ->
      Error (match e with
        | Backend_eio.IOError msg -> msg
        | _ -> "Health check failed")

(** {1 Room Status} *)

let status config =
  match read_state config with
  | Error e ->
      `Assoc [("error", `String e)]
  | Ok state ->
      `Assoc [
        ("protocol_version", `String state.protocol_version);
        ("started_at", `String (now_iso ()));
        ("active_agents", `List (List.map (fun s -> `String s) state.active_agents));
        ("message_count", `Int state.message_seq);
        ("mode", `String state.mode);
        ("paused", `Bool state.paused);
      ]
