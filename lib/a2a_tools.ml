(** A2A MCP Tools - A2A Protocol Wrapped as MCP Tools

    Enables MCP clients (Claude Code, Cursor) to perform A2A-style
    agent-to-agent communication without needing a separate gRPC client.

    Tools:
    - discover: Find available agents and their capabilities
    - query_skill: Get detailed skill information from an agent
    - delegate: Delegate a task to another agent
    - subscribe: Subscribe to agent events

    @see https://github.com/google/A2A for A2A specification
*)

open Types

(** Artifact type for delegate *)
type artifact = {
  name: string;
  mime_type: string;
  data: string;  (* base64 encoded or raw text *)
} [@@deriving yojson, show]

(** Delegate task type *)
type task_type =
  | Sync       (* Wait for completion *)
  | Async      (* Return immediately with task ID *)
  | Stream     (* Stream results as they arrive *)
[@@deriving show]

let task_type_of_string = function
  | "sync" -> Ok Sync
  | "async" -> Ok Async
  | "stream" -> Ok Stream
  | s -> Error (Printf.sprintf "Unknown task type: %s" s)

(** Delegate result *)
type delegate_result = {
  task_id: string;
  status: string;
  result: string option;
  artifacts: artifact list;
} [@@deriving yojson, show]

(** Subscription event types *)
type event_type =
  | TaskUpdate
  | Broadcast
  | Completion
  | Error
[@@deriving show]

let event_type_of_string = function
  | "task_update" -> Ok TaskUpdate
  | "broadcast" -> Ok Broadcast
  | "completion" -> Ok Completion
  | "error" -> Ok Error
  | s -> Error (Printf.sprintf "Unknown event type: %s" s)

(** Subscription *)
type subscription = {
  id: string;
  agent_filter: string option;  (* None = all agents *)
  event_types: event_type list;
  created_at: string;
} [@@deriving show]

(* Global subscription store *)
let subscriptions : (string, subscription) Hashtbl.t = Hashtbl.create 16

(** Event record for buffering *)
type buffered_event = {
  event_type: event_type;
  agent: string;
  data: Yojson.Safe.t;
  timestamp: float;
} [@@deriving show]

(* Event buffer per subscription - key: subscription_id, value: event list *)
let event_buffers : (string, buffered_event list) Hashtbl.t = Hashtbl.create 16

(* Max events per subscription to prevent memory bloat *)
let max_buffered_events = 100

(** Generate UUID using stdlib Random + timestamp (no Mirage_crypto dependency)
    For A2A subscriptions, cryptographic randomness is not required.
*)
let generate_uuid () =
  (* Initialize Random once with high-resolution timestamp *)
  let () =
    let now = Unix.gettimeofday () in
    let seed = int_of_float (now *. 1_000_000.) land 0x3FFFFFFF in
    Random.init seed
  in
  let hex_char () =
    let n = Random.int 16 in
    if n < 10 then Char.chr (n + 48) else Char.chr (n + 87)
  in
  let buf = Buffer.create 36 in
  for i = 0 to 35 do
    if i = 8 || i = 13 || i = 18 || i = 23 then
      Buffer.add_char buf '-'
    else
      Buffer.add_char buf (hex_char ())
  done;
  Buffer.contents buf

(** Get current ISO8601 timestamp *)
let now_iso8601 () : string =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(** Discover available agents

    Combines local room agents with remote agent card fetching.

    @param endpoint Optional remote endpoint URL
    @param capability Optional filter by capability
    @return List of agent cards
*)
let discover config ?(endpoint : string option) ?(capability : string option) ()
    : (Yojson.Safe.t, string) result =
  match endpoint with
  | Some url ->
    (* Remote discovery - fetch agent card from URL *)
    (* For now, return a placeholder since we can't do HTTP in sync *)
    Ok (`Assoc [
      ("type", `String "remote_discovery");
      ("endpoint", `String url);
      ("note", `String "Remote discovery requires async HTTP. Use the endpoint directly.");
      ("well_known_url", `String (url ^ "/.well-known/agent-card.json"));
    ])
  | None ->
    (* Local discovery - list agents in room *)
    let agents = Room.get_agents_raw config in
    (* Filter by capability if specified *)
    let filtered = match capability with
      | None -> agents
      | Some cap ->
        List.filter (fun (a : agent) ->
          List.mem cap a.capabilities
        ) agents
    in
    (* Include local agent card *)
    let local_card = Agent_card.generate_default () in
    let agents_json = List.map (fun (a : agent) ->
      `Assoc [
        ("name", `String a.name);
        ("status", `String (agent_status_to_string a.status));
        ("capabilities", `List (List.map (fun s -> `String s) a.capabilities));
        ("current_task", match a.current_task with
          | None -> `Null
          | Some t -> `String t);
        ("joined_at", `String a.joined_at);
        ("last_seen", `String a.last_seen);
      ]
    ) filtered in
    Ok (`Assoc [
      ("type", `String "local_discovery");
      ("agent_count", `Int (List.length filtered));
      ("agents", `List agents_json);
      ("local_card", Agent_card.to_json local_card);
      ("capability_filter", match capability with
        | None -> `Null
        | Some c -> `String c);
    ])

(** Query skill details from an agent

    @param agent_name Target agent name
    @param skill_id Skill ID to query
    @return Skill details
*)
let query_skill config ~agent_name ~skill_id : (Yojson.Safe.t, string) result =
  (* First, find the agent *)
  let agents = Room.get_agents_raw config in
  let agent_opt = List.find_opt (fun (a : agent) -> a.name = agent_name) agents in
  match agent_opt with
  | None -> Error (Printf.sprintf "Agent '%s' not found" agent_name)
  | Some _agent ->
    (* Look up skill from MASC skills *)
    let skills = Agent_card.masc_skills in
    let skill_opt = List.find_opt (fun (s : Agent_card.skill) -> s.id = skill_id) skills in
    match skill_opt with
    | None -> Error (Printf.sprintf "Skill '%s' not found" skill_id)
    | Some skill ->
      Ok (`Assoc [
        ("agent", `String agent_name);
        ("skill", `Assoc [
          ("id", `String skill.id);
          ("name", `String skill.name);
          ("description", match skill.description with
            | None -> `Null
            | Some d -> `String d);
          ("input_modes", `List (List.map (fun s -> `String s) skill.input_modes));
          ("output_modes", `List (List.map (fun s -> `String s) skill.output_modes));
        ]);
        ("examples", `List [
          `Assoc [
            ("input", `String "Example input for skill");
            ("output", `String "Example output");
          ]
        ]);
      ])

(** Delegate a task to another agent

    Uses Portal for communication. Opens portal, sends task, optionally waits.

    @param target Target agent name
    @param message Task description/prompt
    @param task_type sync/async/stream
    @param artifacts Optional input files/data
    @param timeout Timeout in seconds
    @return Delegate result
*)
let delegate config ~agent_name ~target ~message
    ?(task_type_str = "async")
    ?(artifacts : artifact list = [])
    ?(timeout = 300)
    () : (Yojson.Safe.t, string) result =
  (* Timeout is stored and returned in response for client-side enforcement *)
  let timeout_ms = timeout * 1000 in
  let deadline = Unix.gettimeofday () +. (float_of_int timeout) in
  let task_type_result = task_type_of_string task_type_str in
  match task_type_result with
  | Error e -> Error e
  | Ok task_type ->
    (* Open portal to target agent *)
    let artifacts_json =
      if artifacts = [] then ""
      else Printf.sprintf "\n\nArtifacts: %s"
        (Yojson.Safe.to_string (`List (List.map artifact_to_yojson artifacts)))
    in
    let full_message = message ^ artifacts_json in
    let portal_result = Room.portal_open_r config
      ~agent_name
      ~target_agent:target
      ~initial_message:(Some full_message)
    in
    match portal_result with
    | Error e -> Error (masc_error_to_string e)
    | Ok msg ->
      let task_id = generate_uuid () in
      match task_type with
      | Sync ->
        (* For sync, we'd need to wait for response. For now, return task ID *)
        Ok (`Assoc [
          ("task_id", `String task_id);
          ("status", `String "delegated");
          ("type", `String "sync");
          ("target", `String target);
          ("portal_message", `String msg);
          ("timeout_ms", `Int timeout_ms);
          ("deadline", `Float deadline);
          ("note", `String "Use masc_portal_status to check for response");
        ])
      | Async ->
        Ok (`Assoc [
          ("task_id", `String task_id);
          ("status", `String "delegated");
          ("type", `String "async");
          ("target", `String target);
          ("portal_message", `String msg);
          ("timeout_ms", `Int timeout_ms);
          ("deadline", `Float deadline);
        ])
      | Stream ->
        Ok (`Assoc [
          ("task_id", `String task_id);
          ("status", `String "delegated");
          ("type", `String "stream");
          ("target", `String target);
          ("portal_message", `String msg);
          ("timeout_ms", `Int timeout_ms);
          ("deadline", `Float deadline);
          ("stream_endpoint", `String "/sse/portal");
        ])

(** Subscribe to agent events

    @param agent_filter Optional agent name filter (asterisk for all)
    @param events List of event types to subscribe to
    @return Subscription info
*)
let subscribe ?(agent_filter : string option) ~(events : string list) ()
    : (Yojson.Safe.t, string) result =
  (* Parse event types *)
  let event_types_result : (event_type list, string) result =
    List.fold_left (fun (acc : (event_type list, string) result) e ->
      match acc with
      | Error _ -> acc
      | Ok types ->
        match event_type_of_string e with
        | Error err -> Error err
        | Ok et -> Ok (et :: types)
    ) (Ok []) events
  in
  match event_types_result with
  | Error e -> Error e
  | Ok event_types ->
    let sub_id = generate_uuid () in
    let sub = {
      id = sub_id;
      agent_filter;
      event_types = List.rev event_types;
      created_at = now_iso8601 ();
    } in
    Hashtbl.add subscriptions sub_id sub;
    Ok (`Assoc [
      ("subscription_id", `String sub_id);
      ("agent_filter", match agent_filter with
        | None -> `String "*"
        | Some a -> `String a);
      ("events", `List (List.map (fun e -> `String (show_event_type e)) event_types));
      ("created_at", `String sub.created_at);
      ("sse_endpoint", `String "/sse/subscriptions");
      ("note", `String "Connect to SSE endpoint to receive events");
    ])

(** Unsubscribe from events

    @param subscription_id Subscription ID to remove
*)
let unsubscribe ~subscription_id : (Yojson.Safe.t, string) result =
  if Hashtbl.mem subscriptions subscription_id then begin
    Hashtbl.remove subscriptions subscription_id;
    (* Also clean up buffered events *)
    Hashtbl.remove event_buffers subscription_id;
    Ok (`Assoc [
      ("unsubscribed", `Bool true);
      ("subscription_id", `String subscription_id);
    ])
  end else
    Error (Printf.sprintf "Subscription '%s' not found" subscription_id)

(** List active subscriptions *)
let list_subscriptions () : Yojson.Safe.t =
  let subs = Hashtbl.fold (fun _k v acc ->
    let sub_json = `Assoc [
      ("id", `String v.id);
      ("agent_filter", match v.agent_filter with
        | None -> `String "*"
        | Some a -> `String a);
      ("events", `List (List.map (fun e -> `String (show_event_type e)) v.event_types));
      ("created_at", `String v.created_at);
      ("buffered_count", `Int (
        match Hashtbl.find_opt event_buffers v.id with
        | None -> 0
        | Some events -> List.length events
      ));
    ] in
    sub_json :: acc
  ) subscriptions [] in
  `Assoc [
    ("count", `Int (List.length subs));
    ("subscriptions", `List subs);
  ]

(** Poll buffered events for a subscription

    Retrieves all buffered events and clears the buffer.
    Use this for background subscription workflow:
    1. subscribe (returns immediately)
    2. do work (claim, broadcast, etc.)
    3. poll_events periodically to check for updates

    @param subscription_id Subscription ID
    @param clear Whether to clear buffer after reading (default: true)
    @return List of buffered events
*)
let poll_events ~subscription_id ?(clear = true) ()
    : (Yojson.Safe.t, string) result =
  if not (Hashtbl.mem subscriptions subscription_id) then
    Error (Printf.sprintf "Subscription '%s' not found" subscription_id)
  else
    let events = match Hashtbl.find_opt event_buffers subscription_id with
      | None -> []
      | Some events -> events
    in
    (* Optionally clear buffer *)
    if clear then Hashtbl.replace event_buffers subscription_id [];
    (* Convert to JSON *)
    let events_json = List.map (fun e ->
      `Assoc [
        ("type", `String (show_event_type e.event_type));
        ("agent", `String e.agent);
        ("data", e.data);
        ("timestamp", `Float e.timestamp);
      ]
    ) events in
    Ok (`Assoc [
      ("subscription_id", `String subscription_id);
      ("event_count", `Int (List.length events));
      ("events", `List events_json);
      ("cleared", `Bool clear);
    ])

(** Buffer an event for a subscription (with max limit enforcement) *)
let buffer_event sub_id event =
  let current = match Hashtbl.find_opt event_buffers sub_id with
    | None -> []
    | Some events -> events
  in
  (* Keep only last (max - 1) events + new one *)
  let trimmed =
    if List.length current >= max_buffered_events then
      List.tl (List.rev current) |> List.rev
    else current
  in
  Hashtbl.replace event_buffers sub_id (trimmed @ [event])

(** Notify subscribers of an event (internal use)
    Now also buffers events for polling in addition to SSE broadcast *)
let notify_event ~(event_type : event_type) ~(agent : string) ~(data : Yojson.Safe.t) : unit =
  let timestamp = Unix.gettimeofday () in
  Hashtbl.iter (fun _id sub ->
    (* Check agent filter *)
    let agent_match = match sub.agent_filter with
      | None -> true
      | Some filter -> filter = "*" || filter = agent
    in
    (* Check event type *)
    let event_match = List.mem event_type sub.event_types in
    if agent_match && event_match then begin
      (* Buffer event for polling *)
      let event = { event_type; agent; data; timestamp } in
      buffer_event sub.id event;

      (* Push event as MCP JSON-RPC Notification (no id = notification) *)
      let event_params = `Assoc [
        ("type", `String (show_event_type event_type));
        ("agent", `String agent);
        ("data", data);
        ("timestamp", `Float timestamp);
        ("subscription_id", `String sub.id);
      ] in
      let mcp_notification = `Assoc [
        ("jsonrpc", `String "2.0");
        ("method", `String "masc/event");
        ("params", event_params);
      ] in
      Sse.broadcast mcp_notification;
      Log.debug ~ctx:"a2a" "Event %s from %s buffered+SSE (sub: %s)"
        (show_event_type event_type) agent sub.id
    end
  ) subscriptions
