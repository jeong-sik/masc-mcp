(** MCP Protocol Server Implementation *)

open Types
(* Compact Protocol v1.3 - 64% token savings for agent-to-agent communication *)
module Compact = Compact_protocol.Compact

(** Global state for Mitosis cell lifecycle *)
let current_cell = ref (Mitosis.create_stem_cell ~generation:0)
let stem_pool = ref (Mitosis.init_pool ~config:Mitosis.default_config)

(** JSON-RPC request *)
type jsonrpc_request = {
  jsonrpc : string;
  id : Yojson.Safe.t option; [@default None]
  method_ : string; [@key "method"]
  params : Yojson.Safe.t option; [@default None]
} [@@deriving yojson { strict = false }]

let has_field key = function
  | `Assoc fields -> List.exists (fun (k, _) -> k = key) fields
  | _ -> false

let get_field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let is_jsonrpc_v2 json =
  match get_field "jsonrpc" json with
  | Some (`String "2.0") -> true
  | _ -> false

let is_jsonrpc_response json =
  match json with
  | `Assoc _ ->
      let has_result = has_field "result" json in
      let has_error = has_field "error" json in
      let has_method = has_field "method" json in
      let has_id = has_field "id" json in
      is_jsonrpc_v2 json && has_id && (has_result || has_error) && not has_method
  | _ -> false

(** Check if request is a notification (no id) *)
let is_notification req = req.id = None

(** Get id or null *)
let get_id req = match req.id with Some id -> id | None -> `Null

(** JSON-RPC response builders *)
let make_response ~id result =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result);
  ]

let make_error ~id code message =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message);
    ]);
  ]

(** Convert Result to MCP response tuple - serialization boundary *)
let result_to_response = function
  | Ok msg -> (true, msg)
  | Error e -> (false, Types.masc_error_to_string e)

(** Enhanced response with Response module - provides structured errors with recovery hints *)
let response_ok ?(message="OK") data =
  let r = Response.ok ~message data in
  (true, Response.to_string r)

let response_error ~code ~message ?(hints=[]) () =
  let r = Response.error ~code ~message ~hints () in
  (false, Response.to_string r)

(** Common error patterns with recovery hints *)
let error_not_initialized () =
  response_error
    ~code:"MASC_NOT_INITIALIZED"
    ~message:"MASC room not initialized"
    ~hints:["Run masc_init first"; "Check .masc/ directory exists"]
    ()

let error_task_not_found task_id =
  response_error
    ~code:"TASK_NOT_FOUND"
    ~message:(Printf.sprintf "Task %s not found" task_id)
    ~hints:["Run masc_tasks to see available tasks"; "Check task ID spelling"]
    ()

let error_agent_not_found agent_name =
  response_error
    ~code:"AGENT_NOT_FOUND"
    ~message:(Printf.sprintf "Agent %s not found" agent_name)
    ~hints:["Run masc_join first"; "Check agent name spelling"]
    ()

(** Safe external command execution - bypasses shell to prevent injection
    Only whitelisted commands are allowed.
    Uses argv array instead of shell string.
    Validates resolved path is in trusted directory to prevent PATH hijacking. *)
let allowed_commands = ["masc-checkpoint"; "masc-cost"]

(* Trusted directories where MASC binaries can be located *)
let trusted_dirs = [
  "/usr/local/bin";
  "/usr/bin";
  "/opt/homebrew/bin";  (* macOS ARM homebrew *)
  "/home/linuxbrew/.linuxbrew/bin";  (* Linux homebrew *)
]

(* Check if path is in a trusted directory *)
let is_trusted_path path =
  let normalized =
    try Filename.dirname (Unix.realpath path)
    with Unix.Unix_error _ -> Filename.dirname path
  in
  List.exists (fun dir -> normalized = dir) trusted_dirs

let safe_exec args =
  let open Lwt.Syntax in
  match args with
  | [] -> Lwt.return (false, "No command provided")
  | cmd :: _ ->
    if not (List.mem cmd allowed_commands) then
      Lwt.return (false, Printf.sprintf "Command not allowed: %s" cmd)
    else
      (* Find the command in PATH using /usr/bin/which (not relative) *)
      let* cmd_path =
        Lwt.catch
          (fun () ->
            let* output = Lwt_process.pread ("/usr/bin/which", [|"/usr/bin/which"; cmd|]) in
            Lwt.return (String.trim output))
          (fun _ -> Lwt.return "")
      in
      if cmd_path = "" then
        Lwt.return (false, Printf.sprintf "Command not found: %s" cmd)
      else if not (is_trusted_path cmd_path) then
        Lwt.return (false, Printf.sprintf "Command path not trusted: %s" cmd_path)
      else begin
        (* Execute directly without shell - prevents injection *)
        let argv = Array.of_list (cmd_path :: List.tl args) in
        Lwt.catch
          (fun () ->
            let* output = Lwt_process.pread (cmd_path, argv) in
            Lwt.return (true, output))
          (fun exn ->
            Lwt.return (false, Printf.sprintf "Command failed: %s" (Printexc.to_string exn)))
      end

(** MCP Server state *)
type server_state = {
  mutable room_config: Room.config;
  session_registry: Session.registry;
  mutable on_sse_broadcast: (Yojson.Safe.t -> unit) option;  (* SSE push callback *)
  mutable encryption_config: Encryption.config;  (* P3: Data encryption *)
}

let create_state ~base_path =
  let config = Room.default_config base_path in
  let registry = Session.create () in
  (* Restore sessions from disk for persistence across restarts *)
  let agents_path = Filename.concat base_path ".masc/agents" in
  Session.restore_from_disk registry ~agents_path;
  {
    room_config = config;
    session_registry = registry;
    on_sse_broadcast = None;
    encryption_config = Encryption.default_config;
  }

(** Create state with Eio context - required for PostgresNative backend *)
let create_state_eio ~sw ~env ~base_path =
  let config = Room.default_config_eio ~sw ~env base_path in
  let registry = Session.create () in
  let agents_path = Filename.concat base_path ".masc/agents" in
  Session.restore_from_disk registry ~agents_path;
  {
    room_config = config;
    session_registry = registry;
    on_sse_broadcast = None;
    encryption_config = Encryption.default_config;
  }

(** Register SSE broadcast callback *)
let set_sse_callback state callback =
  state.on_sse_broadcast <- Some callback

(** Broadcast to all SSE clients *)
let sse_broadcast state notification =
  match state.on_sse_broadcast with
  | Some push -> push notification
  | None -> ()

(** MCP protocol version support (legacy + current) *)
let supported_protocol_versions = [
  "2024-11-05";
  "2025-03-26";
  "2025-11-25";
]

let default_protocol_version = "2025-11-25"

let normalize_protocol_version version =
  if List.mem version supported_protocol_versions then version
  else default_protocol_version

let protocol_version_from_params params =
  let open Yojson.Safe.Util in
  match params with
  | Some (`Assoc _ as p) ->
      (try p |> member "protocolVersion" |> to_string
       with _ -> default_protocol_version)
  | _ -> default_protocol_version

(** Server info *)
let server_info = `Assoc [
  ("name", `String "masc-mcp");
  ("version", `String "2.0.1");
]

let capabilities = `Assoc [
  ("tools", `Assoc [
    ("listChanged", `Bool false);
  ]);
  ("resources", `Assoc [
    ("listChanged", `Bool false);
  ]);
  ("prompts", `Assoc [
    ("listChanged", `Bool false);
  ]);
]

(** MCP Resources (read-only context) *)
type mcp_resource = {
  uri : string;
  name : string;
  description : string;
  mime_type : string;
}

type mcp_resource_template = {
  uri_template : string;
  name : string;
  description : string;
  mime_type : string;
}

let resource_to_json (r : mcp_resource) =
  `Assoc [
    ("uri", `String r.uri);
    ("name", `String r.name);
    ("description", `String r.description);
    ("mimeType", `String r.mime_type);
  ]

let resource_template_to_json (t : mcp_resource_template) =
  `Assoc [
    ("uriTemplate", `String t.uri_template);
    ("name", `String t.name);
    ("description", `String t.description);
    ("mimeType", `String t.mime_type);
  ]

let resources : mcp_resource list = [
  {
    uri = "masc://status";
    name = "MASC Status";
    description = "Current room status snapshot (same as masc_status)";
    mime_type = "text/markdown";
  };
  {
    uri = "masc://status.json";
    name = "MASC Status (JSON)";
    description = "Current room status snapshot as JSON (for data collection)";
    mime_type = "application/json";
  };
  {
    uri = "masc://tasks";
    name = "Quest Board";
    description = "Task board snapshot (same as masc_tasks)";
    mime_type = "text/markdown";
  };
  {
    uri = "masc://tasks.json";
    name = "Quest Board (JSON)";
    description = "Task board snapshot as JSON (backlog.json)";
    mime_type = "application/json";
  };
  {
    uri = "masc://who";
    name = "Active Agents";
    description = "In-memory agent/session status (same as masc_who)";
    mime_type = "text/markdown";
  };
  {
    uri = "masc://who.json";
    name = "Active Agents (JSON)";
    description = "In-memory agent/session status as JSON";
    mime_type = "application/json";
  };
  {
    uri = "masc://messages?since_seq=0&limit=10";
    name = "Recent Messages";
    description = "Recent messages snapshot (same as masc_messages)";
    mime_type = "text/markdown";
  };
  {
    uri = "masc://messages.json?since_seq=0&limit=10";
    name = "Recent Messages (JSON)";
    description = "Recent messages snapshot as JSON (for data collection)";
    mime_type = "application/json";
  };
]

let resource_templates : mcp_resource_template list = [
  {
    uri_template = "masc://messages{?since_seq,limit}";
    name = "Messages (range)";
    description = "Read messages with optional since_seq and limit";
    mime_type = "text/markdown";
  };
  {
    uri_template = "masc://messages.json{?since_seq,limit}";
    name = "Messages (range, JSON)";
    description = "Read messages as JSON with optional since_seq and limit";
    mime_type = "application/json";
  };
]

(** Parse a masc:// resource URI into (resource_id, Uri.t) *)
let parse_masc_resource_uri uri_str =
  let uri = Uri.of_string uri_str in
  match Uri.scheme uri with
  | Some "masc" ->
      let host_segments =
        match Uri.host uri with
        | Some h when h <> "" -> [h]
        | _ -> []
      in
      let path_segments =
        Uri.path uri
        |> String.split_on_char '/'
        |> List.filter (fun s -> s <> "")
      in
      let segments = host_segments @ path_segments in
      let id = String.concat "/" segments in
      (id, uri)
  | _ -> (uri_str, uri)

let int_query_param uri key ~default =
  match Uri.get_query_param uri key with
  | None -> default
  | Some s -> (try int_of_string s with _ -> default)

(** Handle initialize request *)
let handle_initialize id params =
  let protocol_version =
    params |> protocol_version_from_params |> normalize_protocol_version
  in
  Lwt.return @@ make_response ~id (`Assoc [
    ("protocolVersion", `String protocol_version);
    ("serverInfo", server_info);
    ("capabilities", capabilities);
    ("instructions", `String "MASC (Multi-Agent Streaming Coordination) enables AI agent collaboration. \
      WORKFLOW: masc_status → masc_claim (task) → masc_worktree_create (isolation) → work → masc_done. \
      Use @agent mentions in masc_broadcast for cross-agent communication. \
      Prefer worktrees over file locks for parallel work.");
  ])

(** Handle tools/list request - filtered by mode config *)
let handle_list_tools state id =
  let room_path = Room.masc_dir state.room_config in
  let config = Config.load room_path in
  let enabled_categories = config.enabled_categories in
  let filtered_schemas = List.filter (fun (schema : tool_schema) ->
    Mode.is_tool_enabled enabled_categories schema.name
  ) Tools.all_schemas in
  let tools = List.map (fun (schema : tool_schema) ->
    `Assoc [
      ("name", `String schema.name);
      ("description", `String schema.description);
      ("inputSchema", schema.input_schema);
    ]
  ) filtered_schemas in
  Lwt.return @@ make_response ~id (`Assoc [("tools", `List tools)])

(** Handle resources/list request *)
let handle_list_resources id =
  let resources_json = List.map resource_to_json resources in
  Lwt.return @@ make_response ~id (`Assoc [("resources", `List resources_json)])

(** Handle resources/templates/list request *)
let handle_list_resource_templates id =
  let templates_json = List.map resource_template_to_json resource_templates in
  Lwt.return @@ make_response ~id (`Assoc [("resourceTemplates", `List templates_json)])

(** Handle resources/read request *)
let handle_read_resource state id params =
  let open Yojson.Safe.Util in
  match params with
  | None -> Lwt.return @@ make_error ~id (-32602) "Missing params"
  | Some (`Assoc _ as p) ->
      let uri_str =
        try p |> member "uri" |> to_string
        with _ -> ""
      in
      if uri_str = "" then
        Lwt.return @@ make_error ~id (-32602) "Missing uri"
      else begin
        let resource_id, uri = parse_masc_resource_uri uri_str in
        let config = state.room_config in
        let registry = state.session_registry in

        let read_messages_json ~since_seq ~limit =
          let msgs_path = Room.messages_dir config in
          if Sys.file_exists msgs_path then
            let files = Sys.readdir msgs_path |> Array.to_list |> List.sort compare |> List.rev in
            let count = ref 0 in
            let msgs = ref [] in
            List.iter (fun name ->
              if !count < limit then begin
                let path = Filename.concat msgs_path name in
                let json = Room.read_json config path in
                match message_of_yojson json with
                | Ok msg when msg.seq > since_seq ->
                    msgs := (message_to_yojson msg) :: !msgs;
                    incr count
                | _ -> ()
              end
            ) files;
            `List (List.rev !msgs)
          else
            `List []
        in

        let (mime_type, text_opt) =
          match resource_id with
          | "status" -> ("text/markdown", Some (Room.status config))
          | "status.json" ->
              let state_json = room_state_to_yojson (Room.read_state config) in
              let backlog_json = backlog_to_yojson (Room.read_backlog config) in
              let connected_agents = Session.get_agent_statuses registry in
              let json = `Assoc [
                ("base_path", `String config.base_path);
                ("state", state_json);
                ("backlog", backlog_json);
                ("connected_agents", `List connected_agents);
              ] in
              ("application/json", Some (Yojson.Safe.pretty_to_string json))
          | "tasks" -> ("text/markdown", Some (Room.list_tasks config))
          | "tasks.json" ->
              let backlog_json = backlog_to_yojson (Room.read_backlog config) in
              ("application/json", Some (Yojson.Safe.pretty_to_string backlog_json))
          | "who" -> ("text/markdown", Some (Session.status_string registry))
          | "who.json" ->
              let statuses = Session.get_agent_statuses registry in
              ("application/json", Some (Yojson.Safe.pretty_to_string (`List statuses)))
          | "messages" | "messages/recent" ->
              let since_seq = int_query_param uri "since_seq" ~default:0 in
              let limit = int_query_param uri "limit" ~default:10 in
              ("text/markdown", Some (Room.get_messages config ~since_seq ~limit))
          | "messages.json" | "messages.json/recent" ->
              let since_seq = int_query_param uri "since_seq" ~default:0 in
              let limit = int_query_param uri "limit" ~default:10 in
              let json = read_messages_json ~since_seq ~limit in
              ("application/json", Some (Yojson.Safe.pretty_to_string json))
          | _ -> ("text/plain", None)
        in

        match text_opt with
        | None -> Lwt.return @@ make_error ~id (-32602) ("Unknown resource: " ^ uri_str)
        | Some text ->
            let contents = `List [
              `Assoc [
                ("uri", `String uri_str);
                ("mimeType", `String mime_type);
                ("text", `String text);
              ]
            ] in
            Lwt.return @@ make_response ~id (`Assoc [("contents", contents)])
      end
  | Some _ ->
      Lwt.return @@ make_error ~id (-32602) "Invalid params"

(** Handle prompts/list request *)
let handle_list_prompts id =
  Lwt.return @@ make_response ~id (`Assoc [("prompts", `List [])])

(** Execute MASC tool *)
let execute_tool state ~name ~arguments =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in

  let config = state.room_config in
  let registry = state.session_registry in

  (* Helper to get string with default *)
  let get_string key default =
    try arguments |> member key |> to_string
    with _ -> default
  in
  let get_int key default =
    try arguments |> member key |> to_int
    with _ -> default
  in
  let get_float key default =
    try arguments |> member key |> to_float
    with _ -> default
  in
  let get_bool key default =
    try arguments |> member key |> to_bool
    with _ -> default
  in
  let get_string_list key =
    try arguments |> member key |> to_list |> List.map to_string
    with _ -> []
  in
  let get_string_opt key =
    try
      let v = arguments |> member key |> to_string in
      if v = "" then None else Some v
    with _ -> None
  in
  let get_int_opt key =
    try Some (arguments |> member key |> to_int)
    with _ -> None
  in

  let agent_name = get_string "agent_name" "unknown" in

  (* Log tool call *)
  Log.Mcp.debug "[%s] %s" agent_name name;

  (* Update activity for any tool call - both in-memory and on-disk *)
  if agent_name <> "unknown" then begin
    Session.update_activity registry ~agent_name ();
    (* Also update disk-based heartbeat for zombie detection across restarts *)
    if Room.is_initialized config then
      ignore (Room.heartbeat config ~agent_name)
  end;

  (* Auto-increment tool call count and write mitosis status for hook consumption *)
  let updated_cell = Mitosis.record_activity ~cell:!current_cell ~task_done:false ~tool_called:true in
  current_cell := updated_cell;
  if Room.is_initialized config then
    (* Use backend-aware write for cross-machine collaboration *)
    Mitosis.write_status_with_backend ~room_config:config ~cell:updated_cell ~config:Mitosis.default_config;

  (* Auto-join: if agent not in session and tool requires agent, auto-register *)
  let read_only_tools = ["masc_status"; "masc_tasks"; "masc_who"; "masc_agents";
                         "masc_messages"; "masc_votes"; "masc_vote_status";
                         "masc_worktree_list"; "masc_pending_interrupts";
                         "masc_cost_report"; "masc_portal_status"] in
  let* () =
    if agent_name <> "unknown" && not (List.mem name read_only_tools) then
      match Hashtbl.find_opt registry.sessions agent_name with
      | Some _ -> Lwt.return_unit  (* Already joined *)
      | None ->
          (* Auto-join silently *)
          let agent_file = Filename.concat config.base_path ".masc/agents" |> fun d ->
            Filename.concat d (agent_name ^ ".json") in
          if not (Sys.file_exists agent_file) && Room.is_initialized config then begin
            let _ = Room.join config ~agent_name ~capabilities:[] () in
            Log.Mcp.info "Auto-joined: %s" agent_name
          end;
          Session.register registry ~agent_name |> Lwt.map ignore
    else
      Lwt.return_unit
  in

  match name with
  | "masc_set_room" ->
      let path = get_string "path" "" in
      let expanded =
        if String.length path > 0 && path.[0] = '~' then
          Filename.concat (Sys.getenv "HOME") (String.sub path 1 (String.length path - 1))
        else if Filename.is_relative path then
          Filename.concat (Sys.getcwd ()) path
        else
          path
      in
      if not (Sys.file_exists expanded && Sys.is_directory expanded) then
        Lwt.return (false, Printf.sprintf "❌ Directory not found: %s" expanded)
      else begin
        state.room_config <- Room.default_config expanded;
        let status = if Room.is_initialized state.room_config then "✅" else "⚠️ (not initialized)" in
        Lwt.return (true, Printf.sprintf "🎯 MASC room set to: %s\n   .masc/ status: %s" expanded status)
      end

  | "masc_init" ->
      let agent = match get_string "agent_name" "" with "" -> None | s -> Some s in
      let result = Room.init config ~agent_name:agent in
      let* _ = match agent with
        | Some a -> Session.register registry ~agent_name:a |> Lwt.map ignore
        | None -> Lwt.return_unit
      in
      Lwt.return (true, result)

  | "masc_join" ->
      let caps = get_string_list "capabilities" in
      let result = Room.join config ~agent_name ~capabilities:caps () in
      let* _ = Session.register registry ~agent_name in
      Lwt.return (true, result)

  | "masc_leave" ->
      let result = Room.leave config ~agent_name in
      let* () = Session.unregister registry ~agent_name in
      Lwt.return (true, result)

  | "masc_status" ->
      Lwt.return (true, Room.status config)

  | "masc_pause" ->
      let reason = get_string "reason" "Manual pause" in
      Room.pause config ~by:agent_name ~reason;
      Lwt.return (true, Printf.sprintf "⏸️ Room paused by %s: %s" agent_name reason)

  | "masc_resume" ->
      (match Room.resume config ~by:agent_name with
      | `Resumed -> Lwt.return (true, Printf.sprintf "▶️ Room resumed by %s" agent_name)
      | `Already_running -> Lwt.return (true, "Room is not paused"))

  | "masc_pause_status" ->
      (match Room.pause_info config with
      | Some (by, reason, at) ->
          let by_str = Option.value by ~default:"unknown" in
          let reason_str = Option.value reason ~default:"no reason" in
          let at_str = Option.value at ~default:"unknown" in
          Lwt.return (true, Printf.sprintf "⏸️ PAUSED\n  By: %s\n  Reason: %s\n  Since: %s" by_str reason_str at_str)
      | None ->
          Lwt.return (true, "▶️ Room is running (not paused)"))

  | "masc_add_task" ->
      let title = get_string "title" "" in
      let priority = get_int "priority" 3 in
      let description = get_string "description" "" in
      Lwt.return (true, Room.add_task config ~title ~priority ~description)

  | "masc_claim" ->
      let task_id = get_string "task_id" "" in
      Lwt.return (result_to_response (Room.claim_task_r config ~agent_name ~task_id))

  | "masc_done" ->
      let task_id = get_string "task_id" "" in
      let notes = get_string "notes" "" in
      (* Get task info BEFORE completion to extract actual start time *)
      let tasks = Room.get_tasks_raw config in
      let task_opt = List.find_opt (fun (t : task) -> t.id = task_id) tasks in
      (* Extract start time and collaborators from task status *)
      let default_time = Unix.gettimeofday () -. 60.0 in
      let (started_at_actual, collaborators_from_task) = match task_opt with
        | Some t -> (match t.task_status with
            | InProgress { started_at; assignee } ->
                let ts = Types.parse_iso8601 ~default_time started_at in
                (* Add previous assignee if different from completer and not empty *)
                let collabs = if assignee <> "" && assignee <> agent_name then [assignee] else [] in
                (ts, collabs)
            | Claimed { claimed_at; assignee } ->
                let ts = Types.parse_iso8601 ~default_time claimed_at in
                (* Add claimer if different from completer and not empty *)
                let collabs = if assignee <> "" && assignee <> agent_name then [assignee] else [] in
                (ts, collabs)
            | _ -> (default_time, []))
        | None -> (default_time, [])
      in
      let result = Room.complete_task_r config ~agent_name ~task_id ~notes in
      (* Record metrics on successful completion (async, fire-and-forget) *)
      (match result with
       | Ok _ ->
           Lwt.async (fun () ->
             let metric : Metrics_store.task_metric = {
               id = Printf.sprintf "metric-%s-%d" task_id (int_of_float (Unix.gettimeofday () *. 1000.));
               agent_id = agent_name;
               task_id;
               started_at = started_at_actual;
               completed_at = Some (Unix.gettimeofday ());
               success = true;
               error_message = None;
               collaborators = collaborators_from_task;
               handoff_from = None;
               handoff_to = None;
             } in
             Metrics_store.record config metric
           )
       | Error _ -> ());
      Lwt.return (result_to_response result)

  (* A2A CancelTask API *)
  | "masc_cancel_task" ->
      let task_id = get_string "task_id" "" in
      let reason = get_string "reason" "" in
      (* Get task info BEFORE cancellation to extract actual start time *)
      let tasks = Room.get_tasks_raw config in
      let task_opt = List.find_opt (fun (t : task) -> t.id = task_id) tasks in
      let started_at_actual = match task_opt with
        | Some t -> (match t.task_status with
            | InProgress { started_at; _ } ->
                (try Scanf.sscanf started_at "%d-%d-%dT%d:%d:%d"
                  (fun y m d h mi s ->
                    let tm = Unix.{ tm_sec=s; tm_min=mi; tm_hour=h;
                      tm_mday=d; tm_mon=m-1; tm_year=y-1900;
                      tm_wday=0; tm_yday=0; tm_isdst=false } in
                    fst (Unix.mktime tm))
                with _ -> Unix.gettimeofday () -. 60.0)
            | Claimed { claimed_at; _ } ->
                (try Scanf.sscanf claimed_at "%d-%d-%dT%d:%d:%d"
                  (fun y m d h mi s ->
                    let tm = Unix.{ tm_sec=s; tm_min=mi; tm_hour=h;
                      tm_mday=d; tm_mon=m-1; tm_year=y-1900;
                      tm_wday=0; tm_yday=0; tm_isdst=false } in
                    fst (Unix.mktime tm))
                with _ -> Unix.gettimeofday () -. 60.0)
            | _ -> Unix.gettimeofday () -. 60.0)
        | None -> Unix.gettimeofday () -. 60.0
      in
      let result = Room.cancel_task_r config ~agent_name ~task_id ~reason in
      (* Record failed metric on cancellation (async) *)
      (match result with
       | Ok _ ->
           Lwt.async (fun () ->
             let metric : Metrics_store.task_metric = {
               id = Printf.sprintf "metric-%s-%d" task_id (int_of_float (Unix.gettimeofday () *. 1000.));
               agent_id = agent_name;
               task_id;
               started_at = started_at_actual;
               completed_at = Some (Unix.gettimeofday ());
               success = false;
               error_message = Some (if reason = "" then "Cancelled" else reason);
               collaborators = [];
               handoff_from = None;
               handoff_to = None;
             } in
             Metrics_store.record config metric
           )
       | Error _ -> ());
      Lwt.return (result_to_response result)

  | "masc_tasks" ->
      Lwt.return (true, Room.list_tasks config)

  | "masc_archive_view" ->
      let limit = get_int "limit" 20 in
      let archive_path = Filename.concat config.base_path ".masc/tasks-archive.json" in
      if Sys.file_exists archive_path then begin
        let content = In_channel.with_open_text archive_path In_channel.input_all in
        let parse_archive json =
          let open Yojson.Safe.Util in
          match json with
          | `List tasks -> Ok (tasks, None)
          | `Assoc _ as obj -> begin
              match obj |> member "tasks" with
              | `List tasks ->
                  let archived_at = obj |> member "archived_at" |> to_string_option in
                  Ok (tasks, archived_at)
              | _ -> Error ()
            end
          | _ -> Error ()
        in
        match parse_archive (Yojson.Safe.from_string content) with
        | Ok (tasks, archive_timestamp) ->
            let total = List.length tasks in
            let shown = List.filteri (fun i _ -> i < limit) tasks in
            let buf = Buffer.create 1024 in
            Buffer.add_string buf (Printf.sprintf "📦 **Archived Tasks** (%d total, showing %d)\n" total (List.length shown));
            Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
            List.iter (fun task ->
              let open Yojson.Safe.Util in
              let id = task |> member "id" |> to_string_option |> Option.value ~default:"?" in
              let title = task |> member "title" |> to_string_option |> Option.value ~default:"?" in
              let archived =
                match task |> member "archived_at" |> to_string_option with
                | Some value -> value
                | None -> begin
                    match archive_timestamp with
                    | Some value -> value
                    | None ->
                        task |> member "completed_at" |> to_string_option |> Option.value ~default:"?"
                  end
              in
              Buffer.add_string buf (Printf.sprintf "  📋 %s: %s\n     archived: %s\n" id title archived)
            ) shown;
            if total > limit then
              Buffer.add_string buf (Printf.sprintf "\n... and %d more (use limit parameter to see more)\n" (total - limit));
            Lwt.return (true, Buffer.contents buf)
        | Error () -> Lwt.return (false, "⚠ Invalid archive format")
        | exception _ -> Lwt.return (false, "⚠ Failed to parse archive")
      end else
        Lwt.return (true, "📦 No archived tasks yet (run masc_gc to archive old completed tasks)")

  | "masc_claim_next" ->
      Lwt.return (true, Room.claim_next config ~agent_name)

  | "masc_update_priority" ->
      let task_id = get_string "task_id" "" in
      let priority = get_int "priority" 3 in
      Lwt.return (true, Room.update_priority config ~task_id ~priority)

  | "masc_broadcast" ->
      let message = get_string "message" "" in
      let format = get_string "format" "compact" in
      (* Check rate limit *)
      let allowed, wait_secs = Session.check_rate_limit registry ~agent_name in
      if not allowed then
        Lwt.return (false, Printf.sprintf "⏳ Rate limited! %d초 후 다시 시도하세요." wait_secs)
      else begin
        let result = Room.broadcast config ~from_agent:agent_name ~content:message in
        (* Parse @mention for push *)
        let mention =
          let re = Str.regexp "@\\([a-zA-Z0-9_]+\\)" in
          try
            let _ = Str.search_forward re message 0 in
            Some (Str.matched_group 1 message)
          with Not_found -> None
        in
        let* _ = Session.push_message registry ~from_agent:agent_name ~content:message ~mention in
        (* Push to SSE clients - compact format for 64% token savings *)
        let notification = match format with
          | "compact" ->
              (* Agent-First: Compact Protocol v1.3 - MSG|from|to|timestamp|content *)
              let compact_msg = Compact.broadcast ~from:agent_name message in
              `String (Compact.encode_message compact_msg)
          | _ ->
              (* Verbose JSON for debugging/human reading *)
              `Assoc [
                ("type", `String "masc/broadcast");
                ("from", `String agent_name);
                ("content", `String message);
                ("mention", match mention with Some m -> `String m | None -> `Null);
                ("timestamp", `String (Types.now_iso ()));
              ]
        in
        sse_broadcast state notification;
        (* macOS notification for @mention *)
        (match mention with
         | Some target -> Notify.notify_mention ~from_agent:agent_name ~target_agent:target ~message ()
         | None -> ());
        Lwt.return (true, result)
      end

  | "masc_messages" ->
      let since_seq = get_int "since_seq" 0 in
      let limit = get_int "limit" 10 in
      Lwt.return (true, Room.get_messages config ~since_seq ~limit)

  | "masc_listen" ->
      let timeout = float_of_int (get_int "timeout" 300) in
      Log.Mcp.info "%s is now listening (timeout: %.0fs)..." agent_name timeout;
      let* msg_opt = Session.wait_for_message registry ~agent_name ~timeout in
      (match msg_opt with
       | Some msg ->
           let from = Yojson.Safe.Util.(msg |> member "from" |> to_string) in
           let content = Yojson.Safe.Util.(msg |> member "content" |> to_string) in
           let timestamp = Yojson.Safe.Util.(msg |> member "timestamp" |> to_string) in
           Lwt.return (true, Printf.sprintf {|
🔔 **MESSAGE RECEIVED!**
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
From: %s
Time: %s
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
%s
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

💡 Call masc_listen again to continue listening.
|} from timestamp content)
       | None ->
           Lwt.return (true, Printf.sprintf "⏰ Listening timed out after %.0fs. No messages received." timeout))

  | "masc_who" ->
      Lwt.return (true, Session.status_string registry)

  | "masc_reset" ->
      let confirm = get_bool "confirm" false in
      if not confirm then
        Lwt.return (false, "⚠️ This will DELETE the entire .masc/ folder!\nCall with confirm=true to proceed.")
      else
        Lwt.return (true, Room.reset config)

  (* Portal/A2A tools *)
  | "masc_portal_open" ->
      let target_agent = get_string "target_agent" "" in
      let initial_message = match get_string "initial_message" "" with
        | "" -> None
        | s -> Some s
      in
      Lwt.return (result_to_response (Room.portal_open_r config ~agent_name ~target_agent ~initial_message))

  | "masc_portal_send" ->
      let message = get_string "message" "" in
      let format = get_string "format" "compact" in
      (* Encode message with Compact Protocol for 64% token savings *)
      let encoded_message = match format with
        | "compact" ->
            (match Room.get_portal_target config ~agent_name with
             | Some target ->
                 let compact_msg = Compact.direct ~from:agent_name ~to_:target message in
                 Compact.encode_message compact_msg
             | None -> message)  (* Fallback to raw if no target *)
        | _ -> message
      in
      (* macOS notification for portal message *)
      (match Room.get_portal_target config ~agent_name with
       | Some target -> Notify.notify_portal ~from_agent:agent_name ~target_agent:target ~message ()
       | None -> ());
      Lwt.return (result_to_response (Room.portal_send_r config ~agent_name ~message:encoded_message))

  | "masc_portal_close" ->
      Lwt.return (true, Room.portal_close config ~agent_name)

  | "masc_portal_status" ->
      let json = Room.portal_status config ~agent_name in
      Lwt.return (true, Yojson.Safe.pretty_to_string json)

  (* Git Worktree tools - v2 Agent Isolation *)
  | "masc_worktree_create" ->
      let task_id = get_string "task_id" "" in
      let base_branch = get_string "base_branch" "develop" in
      Lwt.return (result_to_response (Room.worktree_create_r config ~agent_name ~task_id ~base_branch))

  | "masc_worktree_remove" ->
      let task_id = get_string "task_id" "" in
      Lwt.return (result_to_response (Room.worktree_remove_r config ~agent_name ~task_id))

  | "masc_worktree_list" ->
      let json = Room.worktree_list config in
      Lwt.return (true, Yojson.Safe.pretty_to_string json)

  (* Heartbeat & Agent Health tools *)
  | "masc_heartbeat" ->
      Lwt.return (true, Room.heartbeat config ~agent_name)

  | "masc_cleanup_zombies" ->
      Lwt.return (true, Room.cleanup_zombies config)

  | "masc_gc" ->
      let days = get_int "days" 7 in
      Lwt.return (true, Room.gc config ~days ())

  | "masc_agents" ->
      let json = Room.get_agents_status config in
      Lwt.return (true, Yojson.Safe.pretty_to_string json)

  (* Agent Discovery tools *)
  | "masc_register_capabilities" ->
      let capabilities = get_string_list "capabilities" in
      Lwt.return (true, Room.register_capabilities config ~agent_name ~capabilities)

  | "masc_find_by_capability" ->
      let capability = get_string "capability" "" in
      let json = Room.find_agents_by_capability config ~capability in
      Lwt.return (true, Yojson.Safe.pretty_to_string json)

  (* A2A Agent Card - Discovery *)
  | "masc_agent_card" ->
      let action = get_string "action" "get" in
      let card = Agent_card.generate_default () in
      let json = Agent_card.to_json card in
      let response = match action with
        | "refresh" ->
            `Assoc [
              ("status", `String "refreshed");
              ("card", json);
              ("endpoint", `String "/.well-known/agent-card.json");
            ]
        | _ -> (* "get" *)
            `Assoc [
              ("card", json);
              ("endpoint", `String "/.well-known/agent-card.json");
            ]
      in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  (* ============================================ *)
  (* A2A MCP Tools (A2A Protocol via MCP)        *)
  (* ============================================ *)

  | "masc_a2a_discover" ->
      let endpoint = get_string_opt "endpoint" in
      let capability = get_string_opt "capability" in
      (match A2a_tools.discover config ?endpoint ?capability () with
       | Ok json -> Lwt.return (true, Yojson.Safe.pretty_to_string json)
       | Error e -> Lwt.return (false, Printf.sprintf "❌ Discovery failed: %s" e))

  | "masc_a2a_query_skill" ->
      let agent_name = get_string "agent_name" "" in
      let skill_id = get_string "skill_id" "" in
      (match A2a_tools.query_skill config ~agent_name ~skill_id with
       | Ok json -> Lwt.return (true, Yojson.Safe.pretty_to_string json)
       | Error e -> Lwt.return (false, Printf.sprintf "❌ Query skill failed: %s" e))

  | "masc_a2a_delegate" ->
      let agent_name = get_string "agent_name" "claude" in
      let target = get_string "target_agent" "" in
      let message = get_string "message" "" in
      let task_type_str = get_string "task_type" "async" in
      let timeout = get_int "timeout" 300 in
      (* Parse artifacts if present *)
      let artifacts = match Yojson.Safe.Util.member "artifacts" arguments with
        | `Null -> []
        | `List items ->
            List.filter_map (fun item ->
              match A2a_tools.artifact_of_yojson item with
              | Ok a -> Some a
              | Error _ -> None) items
        | _ -> []
      in
      (match A2a_tools.delegate config ~agent_name ~target ~message
               ~task_type_str ~artifacts ~timeout () with
       | Ok json -> Lwt.return (true, Yojson.Safe.pretty_to_string json)
       | Error e -> Lwt.return (false, Printf.sprintf "❌ Delegation failed: %s" e))

  | "masc_a2a_subscribe" ->
      let agent_filter = get_string_opt "agent_name" in
      let events = match Yojson.Safe.Util.member "events" arguments with
        | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []
      in
      (match A2a_tools.subscribe ?agent_filter ~events () with
       | Ok json -> Lwt.return (true, Yojson.Safe.pretty_to_string json)
       | Error e -> Lwt.return (false, Printf.sprintf "❌ Subscribe failed: %s" e))

  | "masc_a2a_unsubscribe" ->
      let subscription_id = get_string "subscription_id" "" in
      (match A2a_tools.unsubscribe ~subscription_id with
       | Ok json -> Lwt.return (true, Yojson.Safe.pretty_to_string json)
       | Error e -> Lwt.return (false, Printf.sprintf "❌ Unsubscribe failed: %s" e))

  (* Planning with Files (Manus AI pattern) *)
  | "masc_plan_init" ->
      let task_id = get_string "task_id" "" in
      let* result = Planning.init config ~task_id in
      (match result with
       | Ok _ctx ->
           let response = `Assoc [
             ("status", `String "initialized");
             ("task_id", `String task_id);
             ("message", `String (Printf.sprintf "Planning context created for %s" task_id));
           ] in
           Lwt.return (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           Lwt.return (false, Printf.sprintf "❌ Failed to init planning: %s" e))

  | "masc_plan_update" ->
      let task_id = get_string "task_id" "" in
      let content = get_string "content" "" in
      let* result = Planning.update_plan config ~task_id ~content in
      (match result with
       | Ok ctx ->
           let response = `Assoc [
             ("status", `String "updated");
             ("task_id", `String task_id);
             ("updated_at", `String ctx.updated_at);
           ] in
           Lwt.return (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           Lwt.return (false, Printf.sprintf "❌ Failed to update plan: %s" e))

  | "masc_note_add" ->
      let task_id = get_string "task_id" "" in
      let note = get_string "note" "" in
      let* result = Planning.add_note config ~task_id ~note in
      (match result with
       | Ok ctx ->
           let response = `Assoc [
             ("status", `String "added");
             ("task_id", `String task_id);
             ("note_count", `Int (List.length ctx.notes));
           ] in
           Lwt.return (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           Lwt.return (false, Printf.sprintf "❌ Failed to add note: %s" e))

  | "masc_deliver" ->
      let task_id = get_string "task_id" "" in
      let content = get_string "content" "" in
      let* result = Planning.set_deliverable config ~task_id ~content in
      (match result with
       | Ok ctx ->
           let response = `Assoc [
             ("status", `String "delivered");
             ("task_id", `String task_id);
             ("updated_at", `String ctx.updated_at);
           ] in
           Lwt.return (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           Lwt.return (false, Printf.sprintf "❌ Failed to set deliverable: %s" e))

  | "masc_plan_get" ->
      let task_id_input = get_string "task_id" "" in
      (match Planning.resolve_task_id config ~task_id:task_id_input with
       | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e)
       | Ok task_id ->
           let* result = Planning.load config ~task_id in
           (match result with
            | Ok ctx ->
                let markdown = Planning.get_context_markdown ctx in
                let response = `Assoc [
                  ("task_id", `String task_id);
                  ("context", Planning.planning_context_to_yojson ctx);
                  ("markdown", `String markdown);
                ] in
                Lwt.return (true, Yojson.Safe.pretty_to_string response)
            | Error e ->
                Lwt.return (false, Printf.sprintf "❌ Planning context not found: %s" e)))

  (* PDCA Check phase - Error tracking *)
  | "masc_error_add" ->
      let task_id = get_string "task_id" "" in
      let error_type = get_string "error_type" "" in
      let message = get_string "message" "" in
      let context = match get_string "context" "" with "" -> None | s -> Some s in
      let* result = Planning.add_error config ~task_id ~error_type ~message ?context () in
      (match result with
       | Ok ctx ->
           let unresolved_count = List.length (List.filter (fun (e : Planning.error_entry) -> not e.resolved) ctx.errors) in
           let response = `Assoc [
             ("status", `String "added");
             ("task_id", `String task_id);
             ("error_type", `String error_type);
             ("total_errors", `Int (List.length ctx.errors));
             ("unresolved_count", `Int unresolved_count);
           ] in
           Lwt.return (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           Lwt.return (false, Printf.sprintf "❌ Failed to add error: %s" e))

  | "masc_error_resolve" ->
      let task_id = get_string "task_id" "" in
      let error_index = get_int "error_index" 0 in
      let* result = Planning.resolve_error config ~task_id ~index:error_index in
      (match result with
       | Ok ctx ->
           let unresolved_count = List.length (List.filter (fun (e : Planning.error_entry) -> not e.resolved) ctx.errors) in
           let response = `Assoc [
             ("status", `String "resolved");
             ("task_id", `String task_id);
             ("error_index", `Int error_index);
             ("remaining_unresolved", `Int unresolved_count);
           ] in
           Lwt.return (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           Lwt.return (false, Printf.sprintf "❌ Failed to resolve error: %s" e))

  (* Session-level Context *)
  | "masc_plan_set_task" ->
      let task_id = get_string "task_id" "" in
      if task_id = "" then
        Lwt.return (false, "❌ task_id is required")
      else begin
        Planning.set_current_task config ~task_id;
        let response = `Assoc [
          ("status", `String "set");
          ("current_task", `String task_id);
        ] in
        Lwt.return (true, Yojson.Safe.pretty_to_string response)
      end

  | "masc_plan_get_task" ->
      (match Planning.get_current_task config with
       | Some task_id ->
           let response = `Assoc [
             ("current_task", `String task_id);
           ] in
           Lwt.return (true, Yojson.Safe.pretty_to_string response)
       | None ->
           let response = `Assoc [
             ("current_task", `Null);
             ("message", `String "No current task set. Use masc_plan_set_task first.");
           ] in
           Lwt.return (true, Yojson.Safe.pretty_to_string response))

  | "masc_plan_clear_task" ->
      Planning.clear_current_task config;
      let response = `Assoc [
        ("status", `String "cleared");
        ("message", `String "Current task cleared");
      ] in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  (* Voting/Consensus tools *)
  | "masc_vote_create" ->
      let proposer = get_string "proposer" agent_name in
      let topic = get_string "topic" "" in
      let options = get_string_list "options" in
      let required_votes = get_int "required_votes" 2 in
      Lwt.return (true, Room.vote_create config ~proposer ~topic ~options ~required_votes)

  | "masc_vote_cast" ->
      let vote_id = get_string "vote_id" "" in
      let choice = get_string "choice" "" in
      Lwt.return (true, Room.vote_cast config ~agent_name ~vote_id ~choice)

  | "masc_vote_status" ->
      let vote_id = get_string "vote_id" "" in
      let json = Room.vote_status config ~vote_id in
      Lwt.return (true, Yojson.Safe.pretty_to_string json)

  | "masc_votes" ->
      let json = Room.list_votes config in
      Lwt.return (true, Yojson.Safe.pretty_to_string json)

  (* Tempo Control - Cluster Pace Management *)
  | "masc_tempo" ->
      let action = get_string "action" "get" in
      (match action with
       | "get" ->
           let json = Room.get_tempo config in
           Lwt.return (true, Yojson.Safe.pretty_to_string json)
       | "set" ->
           let mode = get_string "mode" "normal" in
           let reason = get_string_opt "reason" in
           Lwt.return (true, Room.set_tempo config ~mode ~reason ~agent_name)
       | _ ->
           Lwt.return (false, "❌ Unknown action. Use 'get' or 'set'"))

  (* LangGraph Interrupt Pattern handlers - shell out to masc-checkpoint binary *)
  | "masc_interrupt" ->
      let task_id = get_string "task_id" "" in
      let step = get_int "step" 1 in
      let action = get_string "action" "" in
      let message = get_string "message" "" in
      (* macOS notification for interrupt - needs user approval *)
      Notify.notify_interrupt ~agent:agent_name ~action;
      (* Safe exec without shell - argv array prevents injection *)
      safe_exec ["masc-checkpoint"; "--masc-dir"; Room.masc_dir config;
                 "--task-id"; task_id; "--step"; string_of_int step;
                 "--action"; action; "--agent"; agent_name; "--interrupt"; message]

  | "masc_approve" ->
      let task_id = get_string "task_id" "" in
      safe_exec ["masc-checkpoint"; "--masc-dir"; Room.masc_dir config;
                 "--task-id"; task_id; "--approve"]

  | "masc_reject" ->
      let task_id = get_string "task_id" "" in
      let reason = get_string "reason" "" in
      let args = if reason = "" then
        ["masc-checkpoint"; "--masc-dir"; Room.masc_dir config; "--task-id"; task_id; "--reject"]
      else
        ["masc-checkpoint"; "--masc-dir"; Room.masc_dir config; "--task-id"; task_id; "--reject"; "--reason"; reason]
      in
      safe_exec args

  | "masc_pending_interrupts" ->
      safe_exec ["masc-checkpoint"; "--masc-dir"; Room.masc_dir config; "--pending"]

  | "masc_branch" ->
      let task_id = get_string "task_id" "" in
      let source_step = get_int "source_step" 0 in
      let branch_name = get_string "branch_name" "" in
      if task_id = "" || source_step = 0 || branch_name = "" then
        Lwt.return (false, "❌ task_id, source_step, and branch_name are required")
      else
        safe_exec ["masc-checkpoint"; "--masc-dir"; Room.masc_dir config;
                   "--task-id"; task_id; "--branch"; string_of_int source_step;
                   "--branch-name"; branch_name; "--agent"; agent_name]

  (* ============================================ *)
  (* Cost Tracking                               *)
  (* ============================================ *)

  | "masc_cost_log" ->
      let model = get_string "model" "unknown" in
      let input_tokens = get_int "input_tokens" 0 in
      let output_tokens = get_int "output_tokens" 0 in
      let cost_usd = get_float "cost_usd" 0.0 in
      let task_id = get_string "task_id" "" in
      let base_args = ["masc-cost"; "--log"; "--agent"; agent_name; "--model"; model;
                       "--input-tokens"; string_of_int input_tokens;
                       "--output-tokens"; string_of_int output_tokens;
                       "--cost"; Printf.sprintf "%.4f" cost_usd] in
      let args = if task_id = "" then base_args else base_args @ ["--task"; task_id] in
      safe_exec args

  | "masc_cost_report" ->
      let period = get_string "period" "daily" in
      let agent = get_string "agent" "" in
      let task_id = get_string "task_id" "" in
      let base_args = ["masc-cost"; "--report"; "--period"; period; "--json"] in
      let args = base_args
                 |> (fun a -> if agent = "" then a else a @ ["--agent"; agent])
                 |> (fun a -> if task_id = "" then a else a @ ["--task"; task_id]) in
      safe_exec args

  (* ============================================ *)
  (* Authentication & Authorization              *)
  (* ============================================ *)

  | "masc_auth_enable" ->
      let require_token = get_bool "require_token" false in
      let secret = Auth.enable_auth config.base_path ~require_token in
      let msg = Printf.sprintf {|🔐 **Authentication Enabled**

Room Secret (SAVE THIS - shown only once):
`%s`

Share this secret securely with authorized agents.
Require token for actions: %b

Use `masc_auth_create_token` to create agent tokens.
|} secret require_token in
      Lwt.return (true, msg)

  | "masc_auth_disable" ->
      Auth.disable_auth config.base_path;
      Lwt.return (true, "🔓 Authentication disabled. All agents can perform any action.")

  | "masc_auth_status" ->
      let cfg = Auth.load_auth_config config.base_path in
      let status = if cfg.enabled then "✅ Enabled" else "❌ Disabled" in
      let require = if cfg.require_token then "Yes" else "No (optional)" in
      let default = Types.agent_role_to_string cfg.default_role in
      let msg = Printf.sprintf {|🔐 **Authentication Status**

Status: %s
Require Token: %s
Default Role: %s
Token Expiry: %d hours
|} status require default cfg.token_expiry_hours in
      Lwt.return (true, msg)

  | "masc_auth_create_token" ->
      let role_str = get_string "role" "worker" in
      let role = match Types.agent_role_of_string role_str with
        | Ok r -> r
        | Error _ -> Types.Worker
      in
      (match Auth.create_token config.base_path ~agent_name ~role with
       | Ok (raw_token, cred) ->
           let expires = match cred.expires_at with
             | Some exp -> exp
             | None -> "never"
           in
           let msg = Printf.sprintf {|🔑 **Token Created for %s**

Token (SAVE THIS - shown only once):
`%s`

Role: %s
Expires: %s

Pass this token in requests to authenticate.
|} agent_name raw_token (Types.agent_role_to_string role) expires in
           Lwt.return (true, msg)
       | Error e ->
           Lwt.return (false, Types.masc_error_to_string e))

  | "masc_auth_refresh" ->
      let token = get_string "token" "" in
      (match Auth.refresh_token config.base_path ~agent_name ~old_token:token with
       | Ok (new_token, cred) ->
           let expires = match cred.expires_at with
             | Some exp -> exp
             | None -> "never"
           in
           let msg = Printf.sprintf {|🔄 **Token Refreshed for %s**

New Token:
`%s`

Expires: %s
|} agent_name new_token expires in
           Lwt.return (true, msg)
       | Error e ->
           Lwt.return (false, Types.masc_error_to_string e))

  | "masc_auth_revoke" ->
      Auth.delete_credential config.base_path agent_name;
      Lwt.return (true, Printf.sprintf "🗑️ Token revoked for %s" agent_name)

  | "masc_auth_list" ->
      let creds = Auth.list_credentials config.base_path in
      if creds = [] then
        Lwt.return (true, "No agent credentials found.")
      else begin
        let buf = Buffer.create 512 in
        Buffer.add_string buf "👥 **Agent Credentials**\n";
        Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
        List.iter (fun (c : Types.agent_credential) ->
          let expires = match c.expires_at with Some exp -> exp | None -> "never" in
          Buffer.add_string buf (Printf.sprintf "  • %s (%s) - expires: %s\n"
            c.agent_name (Types.agent_role_to_string c.role) expires)
        ) creds;
        Lwt.return (true, Buffer.contents buf)
      end

  (* Rate limit tools *)
  | "masc_rate_limit_status" ->
      (* Determine role from auth if available *)
      let role = match Auth.load_credential config.base_path agent_name with
        | Some cred -> cred.role
        | None -> Types.Worker
      in
      let status = Session.get_rate_limit_status registry ~agent_name ~role in
      let buf = Buffer.create 512 in
      Buffer.add_string buf "📊 **Rate Limit Status**\n";
      Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
      let open Yojson.Safe.Util in
      Buffer.add_string buf (Printf.sprintf "Agent: %s (Role: %s)\n"
        (status |> member "agent" |> to_string)
        (status |> member "role" |> to_string));
      Buffer.add_string buf (Printf.sprintf "Burst remaining: %d\n\n"
        (status |> member "burst_remaining" |> to_int));
      Buffer.add_string buf "Categories:\n";
      status |> member "categories" |> to_list |> List.iter (fun cat ->
        let name = cat |> member "category" |> to_string in
        let current = cat |> member "current" |> to_int in
        let limit = cat |> member "limit" |> to_int in
        let remaining = cat |> member "remaining" |> to_int in
        Buffer.add_string buf (Printf.sprintf "  • %s: %d/%d (remaining: %d)\n"
          name current limit remaining)
      );
      Lwt.return (true, Buffer.contents buf)

  | "masc_rate_limit_config" ->
      (* Show current config *)
      let cfg = registry.config in
      let buf = Buffer.create 512 in
      Buffer.add_string buf "⚙️ **Rate Limit Configuration**\n";
      Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
      Buffer.add_string buf (Printf.sprintf "Base limit: %d/min\n" cfg.per_minute);
      Buffer.add_string buf (Printf.sprintf "Burst allowed: %d\n\n" cfg.burst_allowed);
      Buffer.add_string buf "Category limits:\n";
      Buffer.add_string buf (Printf.sprintf "  • Broadcast: %d/min\n" cfg.broadcast_per_minute);
      Buffer.add_string buf (Printf.sprintf "  • Task ops: %d/min\n\n" cfg.task_ops_per_minute);
      Buffer.add_string buf "Role multipliers:\n";
      Buffer.add_string buf (Printf.sprintf "  • Reader: %.1fx\n" cfg.reader_multiplier);
      Buffer.add_string buf (Printf.sprintf "  • Worker: %.1fx\n" cfg.worker_multiplier);
      Buffer.add_string buf (Printf.sprintf "  • Admin: %.1fx\n" cfg.admin_multiplier);
      Lwt.return (true, Buffer.contents buf)

  (* ============================================ *)
  (* Encryption tools                            *)
  (* ============================================ *)

  | "masc_encryption_status" ->
      let status = Encryption.get_status state.encryption_config in
      let msg = Printf.sprintf "🔐 Encryption Status\n%s"
        (Yojson.Safe.pretty_to_string status) in
      Lwt.return (true, msg)

  | "masc_encryption_enable" ->
      let key_source = get_string "key_source" "env" in
      let new_config =
        if String.sub key_source 0 (min 5 (String.length key_source)) = "file:" then
          let path = String.sub key_source 5 (String.length key_source - 5) in
          { Encryption.enabled = true; key_source = `File path; version = 1 }
        else if key_source = "generate" then begin
          Encryption.initialize ();
          match Encryption.generate_key_hex () with
          | Error e ->
              let msg = Printf.sprintf "❌ Failed to generate key: %s" (Encryption.show_encryption_error e) in
              ignore (Lwt.return (false, msg));
              Encryption.default_config  (* fallback *)
          | Ok hex_key ->
              Log.Mcp.info "Generated new key (store securely!): %s" hex_key;
              { Encryption.enabled = true; key_source = `Direct (
                  (* Convert hex to bytes *)
                  let bytes = Bytes.create 32 in
                  for i = 0 to 31 do
                    let hex = String.sub hex_key (i * 2) 2 in
                    Bytes.set bytes i (Char.chr (int_of_string ("0x" ^ hex)))
                  done;
                  Bytes.to_string bytes
                ); version = 1 }
        end
        else
          { Encryption.enabled = true; key_source = `Env "MASC_ENCRYPTION_KEY"; version = 1 }
      in
      state.encryption_config <- new_config;
      Encryption.initialize ();
      let status = Encryption.get_status new_config in
      let msg = Printf.sprintf "✅ Encryption enabled\n%s"
        (Yojson.Safe.pretty_to_string status) in
      Lwt.return (true, msg)

  | "masc_encryption_disable" ->
      state.encryption_config <- { state.encryption_config with Encryption.enabled = false };
      Lwt.return (true, "🔓 Encryption disabled. New data will be stored in plain text.")

  | "masc_generate_key" ->
      Encryption.initialize ();
      (match Encryption.generate_key_hex () with
      | Error e ->
          Lwt.return (false, Printf.sprintf "❌ Failed: %s" (Encryption.show_encryption_error e))
      | Ok hex_key ->
          let output = get_string "output" "hex" in
          let key_str =
            if output = "base64" then
              (* Convert hex to bytes then base64 *)
              let bytes = Bytes.create 32 in
              for i = 0 to 31 do
                let hex = String.sub hex_key (i * 2) 2 in
                Bytes.set bytes i (Char.chr (int_of_string ("0x" ^ hex)))
              done;
              Base64.encode_string (Bytes.to_string bytes)
            else
              hex_key
          in
          let msg = Printf.sprintf "🔑 Generated 256-bit AES key (%s):\n\n%s\n\n⚠️ Store this securely! Losing the key = losing encrypted data." output key_str in
          Lwt.return (true, msg))

  (* Mode management tools (Serena-style) *)
  | "masc_switch_mode" ->
      let mode_str = get_string "mode" "standard" in
      let categories_list = get_string_list "categories" in
      let room_path = Room.masc_dir config in
      let result =
        if categories_list <> [] then
          (* Custom mode with explicit categories *)
          let categories = List.filter_map Mode.category_of_string categories_list in
          if List.length categories <> List.length categories_list then
            Error "❌ Invalid category name(s). Valid: core, comm, portal, worktree, health, discovery, voting, interrupt, cost, auth, ratelimit, encryption"
          else begin
            let cfg = { Config.mode = Mode.Custom; enabled_categories = categories } in
            Config.save room_path cfg;
            Ok (Yojson.Safe.pretty_to_string (Config.get_config_summary room_path))
          end
        else
          (* Preset mode *)
          match Mode.mode_of_string mode_str with
          | Some mode ->
              let _ = Config.switch_mode room_path mode in
              Ok (Yojson.Safe.pretty_to_string (Config.get_config_summary room_path))
          | None ->
              Error "❌ Invalid mode. Valid: minimal, standard, full, solo, custom"
      in
      (match result with
       | Ok msg -> Lwt.return (true, msg)
       | Error msg -> Lwt.return (false, msg))

  | "masc_get_config" ->
      let room_path = Room.masc_dir config in
      let summary = Config.get_config_summary room_path in
      Lwt.return (true, Yojson.Safe.pretty_to_string summary)

  | "masc_spawn" ->
      let agent_name = get_string "agent_name" "" in
      let prompt = get_string "prompt" "" in
      let timeout_seconds = get_int "timeout_seconds" 300 in
      let working_dir = match arguments |> member "working_dir" with
        | `String s when s <> "" -> Some s
        | _ -> None
      in
      let result = Spawn.spawn ~agent_name ~prompt ~timeout_seconds ?working_dir () in
      Lwt.return (result.success, Spawn.result_to_string result)

  (* Relay Tools - Infinite Context via Handoff *)
  | "masc_relay_status" ->
      let messages = get_int "messages" 10 in
      let tool_calls = get_int "tool_calls" 5 in
      let model = get_string "model" "claude" in
      let metrics = Relay.estimate_context ~messages ~tool_calls ~model in
      let should_relay = Relay.should_relay ~config:Relay.default_config ~metrics in
      let response = `Assoc [
        ("metrics", Relay.metrics_to_json metrics);
        ("should_relay", `Bool should_relay);
        ("threshold", `Float Relay.default_config.threshold);
        ("recommendation", `String (
          if metrics.usage_ratio > 0.9 then "CRITICAL: Relay immediately!"
          else if metrics.usage_ratio > 0.8 then "WARNING: Consider relay soon"
          else if metrics.usage_ratio > 0.6 then "OK: Monitor context usage"
          else "HEALTHY: Plenty of context remaining"
        ));
      ] in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  | "masc_relay_checkpoint" ->
      let summary = get_string "summary" "" in
      let task = match arguments |> member "current_task" with
        | `String s when s <> "" -> Some s
        | _ -> None
      in
      let todos = match arguments |> member "todos" with
        | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []
      in
      let pdca = match arguments |> member "pdca_state" with
        | `String s when s <> "" -> Some s
        | _ -> None
      in
      let files = match arguments |> member "relevant_files" with
        | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []
      in
      (* Estimate current metrics for checkpoint *)
      let metrics = Relay.estimate_context ~messages:20 ~tool_calls:10 ~model:"claude" in
      let _cp = Relay.save_checkpoint ~summary ~task ~todos ~pdca ~files ~metrics in
      Lwt.return (true, Printf.sprintf "✅ Checkpoint saved at %.1f%% context usage" (metrics.usage_ratio *. 100.0))

  | "masc_relay_now" ->
      let summary = get_string "summary" "" in
      let task = match arguments |> member "current_task" with
        | `String s when s <> "" -> Some s
        | _ -> None
      in
      let target = get_string "target_agent" "claude" in
      let generation = get_int "generation" 0 in
      let payload : Relay.handoff_payload = {
        summary;
        current_task = task;
        todos = [];
        pdca_state = None;
        relevant_files = [];
        session_id = None;
        relay_generation = generation;
      } in
      let config = { Relay.default_config with target_agent = target } in
      let (result, new_gen) = Relay.execute_relay ~config ~payload in
      if result.Spawn.success then
        Lwt.return (true, Printf.sprintf "✅ Relayed to %s (generation %d)\n\n%s" target new_gen result.output)
      else
        Lwt.return (false, Printf.sprintf "❌ Relay failed: %s" result.output)

  | "masc_relay_smart_check" ->
      let messages = get_int "messages" 10 in
      let tool_calls = get_int "tool_calls" 5 in
      let hint_str = get_string "task_hint" "simple" in
      let file_count = get_int "file_count" 1 in
      let task_hint = match hint_str with
        | "large_file" -> Relay.Large_file_read ""
        | "multi_file" -> Relay.Multi_file_edit file_count
        | "long_running" -> Relay.Long_running_task
        | "exploration" -> Relay.Exploration_task
        | _ -> Relay.Simple_task
      in
      let metrics = Relay.estimate_context ~messages ~tool_calls ~model:"claude" in
      let decision = Relay.should_relay_smart ~config:Relay.default_config ~metrics ~task_hint in
      let (decision_str, recommendation) = match decision with
        | `Proactive ->
          ("proactive", "⚠️ RELAY RECOMMENDED: Upcoming task will likely overflow context. Relay NOW before starting.")
        | `Reactive ->
          ("reactive", "🔴 RELAY REQUIRED: Context already at limit. Relay immediately.")
        | `No_relay ->
          ("no_relay", "✅ SAFE TO PROCEED: Sufficient context for upcoming task.")
      in
      let response = `Assoc [
        ("decision", `String decision_str);
        ("recommendation", `String recommendation);
        ("current_usage", `Float metrics.usage_ratio);
        ("task_hint", `String hint_str);
        ("estimated_task_cost", `Int (Relay.estimate_task_cost task_hint));
      ] in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  (* Mitosis Tools - Cell Division Pattern *)
  | "masc_mitosis_status" ->
      let cell = !current_cell in
      let pool = !stem_pool in
      let response = `Assoc [
        ("cell", Mitosis.cell_to_json cell);
        ("pool", Mitosis.pool_to_json pool);
        ("config", Mitosis.config_to_json Mitosis.default_config);
      ] in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  | "masc_mitosis_all" ->
      (* Get all agents' mitosis statuses from backend (cross-machine collaboration) *)
      let all_statuses = Mitosis.get_all_statuses ~room_config:config in
      let my_node_id = config.backend_config.Backend.node_id in
      let statuses_json = List.map (fun (node_id, status, ratio) ->
        `Assoc [
          ("node_id", `String node_id);
          ("status", `String status);
          ("estimated_ratio", `Float ratio);
          ("is_me", `Bool (node_id = my_node_id));
        ]
      ) all_statuses in
      let response = `Assoc [
        ("my_node_id", `String my_node_id);
        ("cluster_name", `String config.backend_config.Backend.cluster_name);
        ("agents", `List statuses_json);
        ("total_agents", `Int (List.length all_statuses));
        ("agents_in_warning", `Int (List.length (List.filter (fun (_, s, _) -> s = "warning") all_statuses)));
        ("agents_in_critical", `Int (List.length (List.filter (fun (_, s, _) -> s = "critical") all_statuses)));
      ] in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  | "masc_mitosis_pool" ->
      let pool = !stem_pool in
      let response = Mitosis.pool_to_json pool in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  | "masc_mitosis_divide" ->
      let summary = get_string "summary" "" in
      let current_task = get_string_opt "current_task" in
      let full_context = match current_task with
        | Some task -> Printf.sprintf "Summary: %s\n\nCurrent Task: %s" summary task
        | None -> Printf.sprintf "Summary: %s" summary
      in
      let spawn_fn ~prompt =
        (* Use existing spawn infrastructure *)
        let result = Spawn.spawn ~agent_name:"claude" ~prompt ~timeout_seconds:600 () in
        result
      in
      let (spawn_result, new_cell, new_pool) =
        Mitosis.execute_mitosis
          ~config:Mitosis.default_config
          ~pool:!stem_pool
          ~parent:!current_cell
          ~full_context
          ~spawn_fn
      in
      current_cell := new_cell;
      stem_pool := new_pool;
      let response = `Assoc [
        ("status", `String "divided");
        ("new_cell", Mitosis.cell_to_json new_cell);
        ("spawn_result", `Assoc [
          ("success", `Bool spawn_result.Spawn.success);
          ("output", `String spawn_result.Spawn.output);
          ("exit_code", `Int spawn_result.Spawn.exit_code);
          ("elapsed_ms", `Int spawn_result.Spawn.elapsed_ms);
        ]);
      ] in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  | "masc_mitosis_check" ->
      let context_ratio = get_float "context_ratio" 0.0 in
      let cell = !current_cell in
      let config = Mitosis.default_config in
      (* 2-Phase check *)
      let should_prepare = Mitosis.should_prepare ~config ~cell ~context_ratio in
      let should_handoff = Mitosis.should_handoff ~config ~cell ~context_ratio in
      let current_phase = Mitosis.phase_to_string cell.phase in
      let response = `Assoc [
        (* Legacy field for backward compat *)
        ("should_divide", `Bool should_handoff);
        (* 2-Phase fields *)
        ("should_prepare", `Bool should_prepare);
        ("should_handoff", `Bool should_handoff);
        ("current_phase", `String current_phase);
        ("prepare_threshold", `Float config.prepare_threshold);
        ("handoff_threshold", `Float config.handoff_threshold);
        (* Status *)
        ("cell_age_seconds", `Float (Unix.gettimeofday () -. cell.born_at));
        ("task_count", `Int cell.task_count);
        ("tool_call_count", `Int cell.tool_call_count);
        ("context_ratio", `Float context_ratio);
      ] in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  | "masc_mitosis_record" ->
      let task_done = get_bool "task_done" false in
      let tool_called = get_bool "tool_called" false in
      let updated = Mitosis.record_activity ~cell:!current_cell ~task_done ~tool_called in
      current_cell := updated;
      let response = `Assoc [
        ("recorded", `Bool true);
        ("task_count", `Int updated.task_count);
        ("tool_call_count", `Int updated.tool_call_count);
      ] in
      Lwt.return (true, Yojson.Safe.pretty_to_string response)

  | "masc_mitosis_prepare" ->
      (* Phase 1: Prepare for division - extract DNA without handoff *)
      let full_context = get_string "full_context" "" in
      let cell = !current_cell in
      let config = Mitosis.default_config in
      if full_context = "" then
        Lwt.return (false, "❌ full_context is required for prepare")
      else begin
        let prepared_cell = Mitosis.prepare_for_division ~config ~cell ~full_context in
        current_cell := prepared_cell;
        let response = `Assoc [
          ("prepared", `Bool true);
          ("phase", `String (Mitosis.phase_to_string prepared_cell.phase));
          ("state", `String (Mitosis.state_to_string prepared_cell.state));
          ("dna_extracted", `Bool (prepared_cell.prepared_dna <> None));
          ("message", `String "Cell prepared. DNA extracted. Waiting for handoff threshold (80%).");
        ] in
        Lwt.return (true, Yojson.Safe.pretty_to_string response)
      end

  | "masc_memento_mori" ->
      (* Memento Mori - Agent self-awareness of mortality *)
      let context_ratio = get_float "context_ratio" 0.0 in
      let full_context = get_string "full_context" "" in
      let summary = get_string "summary" "" in
      let current_task = get_string "current_task" "" in
      let target_agent = get_string "target_agent" "claude" in
      let cell = !current_cell in
      let config = Mitosis.default_config in

      (* Use should_prepare and should_handoff directly *)
      let should_prepare_now = Mitosis.should_prepare ~config ~cell ~context_ratio in
      let should_handoff_now = Mitosis.should_handoff ~config ~cell ~context_ratio in

      if not should_prepare_now && not should_handoff_now then begin
        (* <50%: Continue working, no action needed *)
        let response = `Assoc [
          ("status", `String "continue");
          ("context_ratio", `Float context_ratio);
          ("threshold_prepare", `Float config.prepare_threshold);
          ("threshold_handoff", `Float config.handoff_threshold);
          ("message", `String (Printf.sprintf "💚 Context healthy (%.0f%%). Continue working." (context_ratio *. 100.0)));
        ] in
        Lwt.return (true, Yojson.Safe.pretty_to_string response)
      end
      else if should_prepare_now && not should_handoff_now then begin
        (* 50-80%: Prepare DNA but don't handoff yet *)
        if full_context = "" then
          Lwt.return (false, "❌ full_context required when context_ratio > 50%")
        else begin
          let prepared_cell = Mitosis.prepare_for_division ~config ~cell ~full_context in
          current_cell := prepared_cell;
          let response = `Assoc [
            ("status", `String "prepared");
            ("context_ratio", `Float context_ratio);
            ("phase", `String (Mitosis.phase_to_string prepared_cell.phase));
            ("dna_extracted", `Bool (prepared_cell.prepared_dna <> None));
            ("message", `String (Printf.sprintf "🟡 Context at %.0f%%. DNA prepared. Handoff at 80%%." (context_ratio *. 100.0)));
          ] in
          Lwt.return (true, Yojson.Safe.pretty_to_string response)
        end
      end
      else begin
        (* >80%: Execute division and spawn successor *)
        if full_context = "" then
          Lwt.return (false, "❌ full_context required for handoff")
        else begin
          (* Create spawn function *)
          let spawn_fn ~prompt =
            Spawn.spawn ~agent_name:target_agent ~prompt ~timeout_seconds:600 ()
          in

          (* Execute full mitosis *)
          let (spawn_result, new_cell, new_pool) =
            Mitosis.execute_mitosis
              ~config
              ~pool:!stem_pool
              ~parent:cell
              ~full_context:(Printf.sprintf "Summary: %s\n\nCurrent Task: %s\n\nContext:\n%s"
                  (if summary = "" then "Memento mori - context limit reached" else summary)
                  current_task full_context)
              ~spawn_fn
          in
          current_cell := new_cell;
          stem_pool := new_pool;

          let response = `Assoc [
            ("status", `String "divided");
            ("context_ratio", `Float context_ratio);
            ("previous_generation", `Int cell.generation);
            ("new_generation", `Int new_cell.generation);
            ("successor_spawned", `Bool spawn_result.Spawn.success);
            ("successor_agent", `String target_agent);
            ("successor_output", `String (String.sub spawn_result.Spawn.output 0 (min 500 (String.length spawn_result.Spawn.output))));
            ("message", `String (Printf.sprintf "🔴 Context critical (%.0f%%). Cell divided. %s successor spawned." (context_ratio *. 100.0) target_agent));
          ] in
          Lwt.return (true, Yojson.Safe.pretty_to_string response)
        end
      end

  (* MCP 2025-11-25 Spec Compliance Tools *)
  | "masc_mcp_session" ->
      let (success, msg) = Session.handle_mcp_session_tool arguments in
      Lwt.return (success, msg)

  | "masc_cancellation" ->
      let (success, msg) = Cancellation.handle_cancellation_tool arguments in
      Lwt.return (success, msg)

  | "masc_subscription" ->
      let (success, msg) = Subscriptions.handle_subscription_tool arguments in
      Lwt.return (success, msg)

  | "masc_progress" ->
      let (success, msg) = Progress.handle_progress_tool arguments in
      Lwt.return (success, msg)

  (* Cellular Agent - Handover *)
  | "masc_handover_create" ->
      let task_id = get_string "task_id" "" in
      let session_id = get_string "session_id" "" in
      let reason_str = get_string "reason" "explicit" in
      let reason = match reason_str with
        | "context_limit" -> Handover.ContextLimit (get_int "context_pct" 90)
        | "timeout" -> Handover.Timeout 0
        | "error" -> Handover.FatalError "unspecified"
        | "complete" -> Handover.TaskComplete
        | _ -> Handover.Explicit
      in
      let goal = get_string "goal" "" in
      let progress = get_string "progress" "" in
      let completed = get_string_list "completed_steps" in
      let pending = get_string_list "pending_steps" in
      let decisions = get_string_list "decisions" in
      let assumptions = get_string_list "assumptions" in
      let warnings = get_string_list "warnings" in
      let errors = get_string_list "errors" in
      let files = get_string_list "files" in
      let context_pct = get_int "context_pct" 0 in
      let* result = Handover.create_from_planning config
        ~from_agent:agent_name ~task_id ~session_id ~reason
        ~goal ~progress ~completed ~pending ~decisions ~assumptions ~warnings
        ~errors ~files ~context_pct in
      (match result with
       | Ok h -> Lwt.return (true, Printf.sprintf "🧬 Handover created: %s\n\n%s" h.Handover.id (Handover.format_as_markdown h))
       | Error e -> Lwt.return (false, Printf.sprintf "❌ Failed to create handover: %s" e))

  | "masc_handover_list" ->
      let pending_only = get_bool "pending_only" false in
      let* handovers =
        if pending_only then Handover.get_pending_handovers config
        else Handover.list_handovers config
      in
      if handovers = [] then
        Lwt.return (true, "📭 No handovers found")
      else
        let summary = List.map (fun (h : Handover.handover_record) ->
          Printf.sprintf "- **%s** [%s → %s] %s (%s)"
            h.id h.from_agent (Option.value h.to_agent ~default:"unclaimed")
            h.current_goal h.handover_reason
        ) handovers |> String.concat "\n" in
        Lwt.return (true, Printf.sprintf "🧬 %d handover(s):\n\n%s" (List.length handovers) summary)

  | "masc_handover_claim" ->
      let handover_id = get_string "handover_id" "" in
      let* result = Handover.claim_handover config ~handover_id ~agent_name in
      (match result with
       | Ok h -> Lwt.return (true, Printf.sprintf "✅ Handover claimed!\n\n%s" (Handover.format_as_markdown h))
       | Error e -> Lwt.return (false, Printf.sprintf "❌ Failed to claim: %s" e))

  | "masc_handover_get" ->
      let handover_id = get_string "handover_id" "" in
      let* result = Handover.load_handover config handover_id in
      (match result with
       | Ok h -> Lwt.return (true, Handover.format_as_markdown h)
       | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_handover_claim_and_spawn" ->
      let handover_id = get_string "handover_id" "" in
      let agent_name = get_string "agent_name" "claude" in
      let additional_instructions = get_string_opt "additional_instructions" in
      let timeout_seconds = get_int_opt "timeout_seconds" in
      let* result = Handover.claim_and_spawn config ~handover_id ~agent_name
        ?additional_instructions ?timeout_seconds () in
      (match result with
       | Ok spawn_result ->
           let status = if spawn_result.Spawn.success then "✅" else "❌" in
           Lwt.return (spawn_result.Spawn.success,
             Printf.sprintf "%s Agent %s spawned (exit %d, %dms)\n\n%s"
               status agent_name spawn_result.Spawn.exit_code
               spawn_result.Spawn.elapsed_ms spawn_result.Spawn.output)
       | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  (* ===== Execution Memory Tools ===== *)

  | "masc_run_init" ->
      let task_id = get_string "task_id" "" in
      let agent_name = get_string "agent_name" "claude" in
      let* result = Execution_memory.init_run config ~task_id ~agent_name in
      (match result with
       | Ok run ->
           Lwt.return (true, Printf.sprintf "✅ Run initialized for task %s\n\n%s"
             task_id (Execution_memory.format_as_markdown run))
       | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_run_plan" ->
      let task_id = get_string "task_id" "" in
      let plan = get_string "plan" "" in
      let* result = Execution_memory.set_plan config ~task_id ~plan in
      (match result with
       | Ok run ->
           Lwt.return (true, Printf.sprintf "✅ Plan set for task %s\n\n%s"
             task_id (Execution_memory.format_as_markdown run))
       | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_run_log" ->
      let task_id = get_string "task_id" "" in
      let note = get_string "note" "" in
      let* result = Execution_memory.add_note config ~task_id ~note in
      (match result with
       | Ok run ->
           let n = List.length run.Execution_memory.notes in
           Lwt.return (true, Printf.sprintf "✅ Note added (%d total)\n\n%s"
             n (Execution_memory.format_as_markdown run))
       | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_run_deliverable" ->
      let task_id = get_string "task_id" "" in
      let deliverable = get_string "deliverable" "" in
      let* result = Execution_memory.set_deliverable config ~task_id ~deliverable in
      (match result with
       | Ok run ->
           Lwt.return (true, Printf.sprintf "✅ Deliverable recorded, run completed\n\n%s"
             (Execution_memory.format_as_markdown run))
       | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_run_get" ->
      let task_id = get_string "task_id" "" in
      let* result = Execution_memory.load_run config task_id in
      (match result with
       | Ok run -> Lwt.return (true, Execution_memory.format_as_markdown run)
       | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_run_list" ->
      let* runs = Execution_memory.list_runs config in
      let output = if runs = [] then
        "📋 No runs found"
      else
        let header = "📋 Execution Runs\n\n" in
        let rows = List.map (fun (run : Execution_memory.run_record) ->
          let duration = Unix.gettimeofday () -. run.started_at in
          let dur_str =
            if duration < 60.0 then Printf.sprintf "%.0fs" duration
            else if duration < 3600.0 then Printf.sprintf "%.1fm" (duration /. 60.0)
            else Printf.sprintf "%.1fh" (duration /. 3600.0)
          in
          Printf.sprintf "- **%s** [%s] %s (%s)"
            run.task_id
            (Execution_memory.status_to_string run.status)
            run.agent_name
            dur_str
        ) runs in
        header ^ String.concat "\n" rows
      in
      Lwt.return (true, output)

  (* ===== Cache Tools (Phase 11) ===== *)
  | "masc_cache_set" ->
      let key = get_string "key" "" in
      let value = get_string "value" "" in
      let ttl_seconds = match get_int_opt "ttl_seconds" with
        | Some 0 -> None
        | opt -> opt
      in
      let tags = match arguments |> Yojson.Safe.Util.member "tags" with
        | `List l -> List.filter_map (function `String s -> Some s | _ -> None) l
        | _ -> []
      in
      if key = "" || value = "" then
        Lwt.return (false, "❌ key and value are required")
      else
        let* result = Cache.set config ~key ~value ?ttl_seconds ~tags () in
        (match result with
         | Ok entry ->
             let ttl_info = match entry.Cache.expires_at with
               | Some exp ->
                   let remaining = exp -. Unix.gettimeofday () in
                   Printf.sprintf " (expires in %.0fs)" remaining
               | None -> ""
             in
             Lwt.return (true, Printf.sprintf "✅ Cached: %s%s" key ttl_info)
         | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_cache_get" ->
      let key = get_string "key" "" in
      if key = "" then
        Lwt.return (false, "❌ key is required")
      else
        let* result = Cache.get config ~key in
        (match result with
         | Ok (Some entry) ->
             let age = Unix.gettimeofday () -. entry.Cache.created_at in
             let age_str =
               if age < 60.0 then Printf.sprintf "%.0fs" age
               else if age < 3600.0 then Printf.sprintf "%.1fm" (age /. 60.0)
               else Printf.sprintf "%.1fh" (age /. 3600.0)
             in
             let output = Printf.sprintf "📦 **%s** (age: %s)\n\n%s"
               entry.Cache.key age_str entry.Cache.value in
             Lwt.return (true, output)
         | Ok None -> Lwt.return (true, Printf.sprintf "📦 Not found: %s" key)
         | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_cache_delete" ->
      let key = get_string "key" "" in
      if key = "" then
        Lwt.return (false, "❌ key is required")
      else
        let* result = Cache.delete config ~key in
        (match result with
         | Ok true -> Lwt.return (true, Printf.sprintf "🗑️ Deleted: %s" key)
         | Ok false -> Lwt.return (true, Printf.sprintf "📦 Not found: %s" key)
         | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_cache_list" ->
      let tag = match get_string "tag" "" with "" -> None | s -> Some s in
      let* entries = Cache.list config ?tag () in
      let output = if entries = [] then
        "📦 Cache is empty"
      else
        let header = Printf.sprintf "📦 Cache Entries (%d)\n\n" (List.length entries) in
        let rows = List.map (fun (entry : Cache.cache_entry) ->
          let age = Unix.gettimeofday () -. entry.created_at in
          let age_str =
            if age < 60.0 then Printf.sprintf "%.0fs" age
            else if age < 3600.0 then Printf.sprintf "%.1fm" (age /. 60.0)
            else Printf.sprintf "%.1fh" (age /. 3600.0)
          in
          let tags_str = if entry.tags = [] then ""
            else " [" ^ String.concat ", " entry.tags ^ "]" in
          let value_preview =
            let v = entry.value in
            if String.length v <= 50 then v
            else String.sub v 0 47 ^ "..."
          in
          Printf.sprintf "- **%s** (%s)%s: %s"
            entry.key age_str tags_str value_preview
        ) entries in
        header ^ String.concat "\n" rows
      in
      Lwt.return (true, output)

  | "masc_cache_clear" ->
      let* result = Cache.clear config in
      (match result with
       | Ok count -> Lwt.return (true, Printf.sprintf "🗑️ Cleared %d entries" count)
       | Error e -> Lwt.return (false, Printf.sprintf "❌ %s" e))

  | "masc_cache_stats" ->
      let* stats = Cache.stats config in
      let output = Cache.format_stats stats in
      Lwt.return (true, output)

  (* ===== Tempo Tools (Phase 12) ===== *)

  | "masc_tempo_get" ->
      let state = Tempo.get_tempo config in
      let output = Tempo.format_state state in
      Lwt.return (true, output)

  | "masc_tempo_set" ->
      let interval = arguments |> Yojson.Safe.Util.member "interval_seconds" |> Yojson.Safe.Util.to_float in
      let reason = match arguments |> Yojson.Safe.Util.member "reason" with
        | `String s -> s
        | _ -> "manual adjustment"
      in
      let state = Tempo.set_tempo config ~interval_s:interval ~reason in
      let output = Tempo.format_state state in
      Lwt.return (true, output)

  | "masc_tempo_adjust" ->
      let* state = Tempo.adjust_tempo config in
      let output = Tempo.format_state state in
      Lwt.return (true, output)

  | "masc_tempo_reset" ->
      let state = Tempo.reset_tempo config in
      let output = Tempo.format_state state in
      Lwt.return (true, output)

  (* ===== Dashboard Tools (Phase 13) ===== *)
  | "masc_dashboard" ->
      let compact = match arguments |> Yojson.Safe.Util.member "compact" with
        | `Bool b -> b
        | _ -> false
      in
      let output =
        if compact then Dashboard.generate_compact config
        else Dashboard.generate config
      in
      Lwt.return (true, output)

  (* ===== Level 2: Organization Tools ===== *)

  | "masc_agent_fitness" ->
      let days = match arguments |> Yojson.Safe.Util.member "days" with
        | `Int d -> d
        | _ -> 7
      in
      let agent_name = match arguments |> Yojson.Safe.Util.member "agent_name" with
        | `String s -> Some s
        | _ -> None
      in
      let* results = Fitness.get_all_fitness config ~days () in
      let filtered = match agent_name with
        | Some name -> List.filter (fun r -> r.Fitness.agent_id = name) results
        | None -> results
      in
      let json = `List (List.map Fitness.fitness_to_json filtered) in
      Lwt.return (true, Yojson.Safe.pretty_to_string json)

  | "masc_select_agent" ->
      let days = match arguments |> Yojson.Safe.Util.member "days" with
        | `Int d -> d
        | _ -> 7
      in
      let strategy = match arguments |> Yojson.Safe.Util.member "strategy" with
        | `String s -> Fitness.strategy_of_string s
        | _ -> Fitness.Capability_first
      in
      let available_agents = match arguments |> Yojson.Safe.Util.member "available_agents" with
        | `List l -> List.filter_map (function `String s -> Some s | _ -> None) l
        | _ -> []
      in
      if List.length available_agents = 0 then
        Lwt.return (false, "❌ No available_agents provided")
      else
        let* result = Fitness.select_agent config ~strategy ~days ~available_agents () in
        (match result with
        | Some f ->
          let output = Printf.sprintf "✅ Selected: %s (fitness: %.3f)\n\n%s"
            f.agent_id f.fitness_score (Yojson.Safe.pretty_to_string (Fitness.fitness_to_json f)) in
          Lwt.return (true, output)
        | None ->
          Lwt.return (false, "❌ No suitable agent found"))

  | "masc_collaboration_graph" ->
      let format = match arguments |> Yojson.Safe.Util.member "format" with
        | `String "json" -> "json"
        | _ -> "text"
      in
      if format = "json" then
        let* (synapses, agents) = Hebbian.get_graph_data config in
        let json = `Assoc [
          ("agents", `List (List.map (fun a -> `String a) agents));
          ("synapses", `List (List.map (fun s -> `Assoc [
            ("from", `String s.Hebbian.from_agent);
            ("to", `String s.Hebbian.to_agent);
            ("weight", `Float s.Hebbian.weight);
            ("successes", `Int s.Hebbian.success_count);
            ("failures", `Int s.Hebbian.failure_count);
          ]) synapses));
        ] in
        Lwt.return (true, Yojson.Safe.pretty_to_string json)
      else
        let* output = Hebbian.graph_to_text config in
        Lwt.return (true, output)

  | "masc_consolidate_learning" ->
      let decay_after_days = match arguments |> Yojson.Safe.Util.member "decay_after_days" with
        | `Int d -> d
        | _ -> 7
      in
      let* pruned = Hebbian.consolidate config ~decay_after_days () in
      Lwt.return (true, Printf.sprintf "✅ Consolidation complete. Pruned %d weak connections." pruned)

  | "masc_verify_handoff" ->
      let original = arguments |> Yojson.Safe.Util.member "original" |> Yojson.Safe.Util.to_string in
      let received = arguments |> Yojson.Safe.Util.member "received" |> Yojson.Safe.Util.to_string in
      let threshold = match arguments |> Yojson.Safe.Util.member "threshold" with
        | `Float f -> f
        | `Int i -> float_of_int i
        | _ -> 0.85
      in
      let result = Drift_guard.verify_handoff ~original ~received ~threshold () in
      (match result with
      | Drift_guard.Verified { similarity } ->
        Lwt.return (true, Printf.sprintf "✅ Verified (similarity: %.3f)" similarity)
      | Drift_guard.Drift_detected { similarity; drift_type; details } ->
        let output = Printf.sprintf "⚠️ Drift Detected!\nSimilarity: %.3f (threshold: %.2f)\nType: %s\nDetails: %s"
          similarity threshold (Drift_guard.drift_type_to_string drift_type) details in
        Lwt.return (false, output))

  | "masc_get_metrics" ->
      let agent_name = arguments |> Yojson.Safe.Util.member "agent_name" |> Yojson.Safe.Util.to_string in
      let days = match arguments |> Yojson.Safe.Util.member "days" with
        | `Int d -> d
        | _ -> 7
      in
      let* metrics = Metrics_store.calculate_agent_metrics config ~agent_id:agent_name ~days in
      (match metrics with
      | Some m ->
        let json = Metrics_store.agent_metrics_to_yojson m in
        Lwt.return (true, Yojson.Safe.pretty_to_string json)
      | None ->
        Lwt.return (false, Printf.sprintf "❌ No metrics found for agent: %s" agent_name))

  (* ===== Level 4: Swarm Intelligence ===== *)

  | "masc_swarm_init" ->
      let behavior_str = match arguments |> Yojson.Safe.Util.member "behavior" with
        | `String s -> s
        | _ -> "flocking"
      in
      let behavior = match behavior_str with
        | "foraging" -> Swarm.Foraging
        | "stigmergy" -> Swarm.Stigmergy
        | "quorum_sensing" -> Swarm.Quorum_sensing
        | _ -> Swarm.Flocking
      in
      let selection_pressure = match arguments |> Yojson.Safe.Util.member "selection_pressure" with
        | `Float f -> f
        | `Int i -> float_of_int i
        | _ -> 0.3
      in
      let mutation_rate = match arguments |> Yojson.Safe.Util.member "mutation_rate" with
        | `Float f -> f
        | `Int i -> float_of_int i
        | _ -> 0.1
      in
      let swarm_cfg = { (Swarm.default_config ()) with
        Swarm.behavior; selection_pressure; mutation_rate
      } in
      let* swarm = Swarm.create config ~swarm_config:swarm_cfg () in
      let agent_count = List.length swarm.Swarm.agents in
      Lwt.return (true, Printf.sprintf "✅ Swarm initialized: behavior=%s, agents=%d, selection=%.2f, mutation=%.2f"
        behavior_str agent_count selection_pressure mutation_rate)

  | "masc_swarm_join" ->
      let agent_name = arguments |> Yojson.Safe.Util.member "agent_name" |> Yojson.Safe.Util.to_string in
      let* result = Swarm.join config ~agent_id:agent_name ~agent_name in
      (match result with
      | Some swarm ->
        let count = List.length swarm.Swarm.agents in
        Lwt.return (true, Printf.sprintf "✅ Agent '%s' joined swarm (total: %d agents)" agent_name count)
      | None ->
        Lwt.return (false, Printf.sprintf "❌ Failed to join swarm (full or not initialized)"))

  | "masc_swarm_leave" ->
      let agent_name = arguments |> Yojson.Safe.Util.member "agent_name" |> Yojson.Safe.Util.to_string in
      let* result = Swarm.leave config ~agent_id:agent_name in
      (match result with
      | Some swarm ->
        let count = List.length swarm.Swarm.agents in
        Lwt.return (true, Printf.sprintf "✅ Agent '%s' left swarm (remaining: %d agents)" agent_name count)
      | None ->
        Lwt.return (false, Printf.sprintf "❌ Failed to leave swarm (not a member or not initialized)"))

  | "masc_swarm_status" ->
      let* swarm_opt = Swarm.load_swarm config in
      (match swarm_opt with
      | Some (swarm : Swarm.swarm) ->
        let cfg = swarm.swarm_cfg in
        let behavior_str = match cfg.behavior with
          | Swarm.Flocking -> "flocking"
          | Swarm.Foraging -> "foraging"
          | Swarm.Stigmergy -> "stigmergy"
          | Swarm.Quorum_sensing -> "quorum_sensing"
        in
        let agent_names = List.map (fun (a : Swarm.swarm_agent) -> a.name) swarm.agents in
        let pending = List.filter (fun (p : Swarm.quorum_proposal) -> p.status = `Pending) swarm.proposals in
        let json = `Assoc [
          ("behavior", `String behavior_str);
          ("agent_count", `Int (List.length swarm.agents));
          ("agents", `List (List.map (fun n -> `String n) agent_names));
          ("pheromone_count", `Int (List.length swarm.pheromones));
          ("pending_proposals", `Int (List.length pending));
          ("selection_pressure", `Float cfg.selection_pressure);
          ("mutation_rate", `Float cfg.mutation_rate);
        ] in
        Lwt.return (true, Yojson.Safe.pretty_to_string json)
      | None ->
        Lwt.return (false, "❌ No swarm initialized. Use masc_swarm_init first."))

  | "masc_swarm_evolve" ->
      let* result = Swarm.evolve config in
      (match result with
      | Some swarm ->
        let rankings = Swarm.Pure.fitness_rankings swarm in
        let top3 = List.filteri (fun i _ -> i < 3) rankings in
        let top3_str = String.concat ", " (List.map (fun (name, fit) ->
          Printf.sprintf "%s(%.2f)" name fit) top3) in
        Lwt.return (true, Printf.sprintf "✅ Evolution cycle complete. Top agents: %s" top3_str)
      | None ->
        Lwt.return (false, "❌ Evolution failed (no swarm initialized)"))

  | "masc_swarm_propose" ->
      let description = arguments |> Yojson.Safe.Util.member "description" |> Yojson.Safe.Util.to_string in
      let threshold = match arguments |> Yojson.Safe.Util.member "threshold" with
        | `Float f -> f
        | `Int i -> float_of_int i
        | _ -> 0.6
      in
      let* proposal = Swarm.propose config ~description ~proposed_by:"claude" ~threshold () in
      (match proposal with
      | Some p ->
        Lwt.return (true, Printf.sprintf "✅ Proposal created: id=%s, threshold=%.0f%%" p.Swarm.proposal_id (threshold *. 100.0))
      | None ->
        Lwt.return (false, "❌ Failed to create proposal (no swarm initialized)"))

  | "masc_swarm_vote" ->
      let proposal_id = arguments |> Yojson.Safe.Util.member "proposal_id" |> Yojson.Safe.Util.to_string in
      let vote_for = arguments |> Yojson.Safe.Util.member "vote_for" |> Yojson.Safe.Util.to_bool in
      let* result = Swarm.vote config ~proposal_id ~agent_id:"claude" ~vote_for in
      (match result with
      | Some proposal ->
        let status_str = match proposal.Swarm.status with
          | `Pending -> "pending"
          | `Passed -> "PASSED ✅"
          | `Rejected -> "REJECTED ❌"
          | `Expired -> "EXPIRED ⏰"
        in
        let for_count = List.length proposal.Swarm.votes_for in
        let against_count = List.length proposal.Swarm.votes_against in
        Lwt.return (true, Printf.sprintf "✅ Vote recorded. Status: %s (%d for, %d against)"
          status_str for_count against_count)
      | None ->
        Lwt.return (false, Printf.sprintf "❌ Failed to vote (proposal '%s' not found)" proposal_id))

  | "masc_swarm_deposit" ->
      let path_id = arguments |> Yojson.Safe.Util.member "path_id" |> Yojson.Safe.Util.to_string in
      let strength = match arguments |> Yojson.Safe.Util.member "strength" with
        | `Float f -> f
        | `Int i -> float_of_int i
        | _ -> 0.2
      in
      let* result = Swarm.deposit_pheromone config ~path_id ~agent_id:"claude" ~strength in
      (match result with
      | Some _ ->
        Lwt.return (true, Printf.sprintf "✅ Pheromone deposited on '%s' (strength: %.2f)" path_id strength)
      | None ->
        Lwt.return (false, "❌ Failed to deposit pheromone (no swarm initialized)"))

  | "masc_swarm_trails" ->
      let limit = match arguments |> Yojson.Safe.Util.member "limit" with
        | `Int i -> i
        | _ -> 5
      in
      let* trails = Swarm.get_strongest_trails config ~limit in
      if List.length trails = 0 then
        Lwt.return (true, "No pheromone trails found.")
      else
        let trail_list = List.map (fun p ->
          `Assoc [
            ("path_id", `String p.Swarm.path_id);
            ("strength", `Float p.Swarm.strength);
            ("deposited_by", `String p.Swarm.deposited_by);
          ]
        ) trails in
        let json = `Assoc [
          ("trails", `List trail_list);
          ("count", `Int (List.length trails));
        ] in
        Lwt.return (true, Yojson.Safe.pretty_to_string json)

  (* ============================================ *)
  (* Multi-Room Management                        *)
  (* ============================================ *)

  | "masc_rooms_list" ->
      let result = Room.rooms_list config in
      Lwt.return (true, Yojson.Safe.pretty_to_string result)

  | "masc_room_create" ->
      let name = get_string "name" "" in
      if name = "" then
        Lwt.return (false, "❌ Room name is required")
      else
        let description = match arguments |> Yojson.Safe.Util.member "description" with
          | `String d -> Some d
          | _ -> None
        in
        let result = Room.room_create config ~name ~description in
        let success = match result with
          | `Assoc fields -> not (List.mem_assoc "error" fields)
          | _ -> false
        in
        Lwt.return (success, Yojson.Safe.pretty_to_string result)

  | "masc_room_enter" ->
      let room_id = get_string "room_id" "" in
      if room_id = "" then
        Lwt.return (false, "❌ Room ID is required")
      else
        let agent_type = get_string "agent_type" "claude" in
        let result = Room.room_enter config ~room_id ~agent_type in
        let success = match result with
          | `Assoc fields -> not (List.mem_assoc "error" fields)
          | _ -> false
        in
        Lwt.return (success, Yojson.Safe.pretty_to_string result)

  | _ ->
      Lwt.return (false, Printf.sprintf "❌ Unknown tool: %s" name)

(** Handle tools/call request *)
let handle_call_tool state id params =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in

  let name = params |> member "name" |> to_string in
  let arguments = params |> member "arguments" in

  let* (success, message) = execute_tool state ~name ~arguments in

  let result = make_response ~id (`Assoc [
    ("content", `List [
      `Assoc [
        ("type", `String "text");
        ("text", `String message);
      ]
    ]);
    ("isError", `Bool (not success));
  ]) in

  (* Log result *)
  let preview =
    if String.length message > 80
    then String.sub message 0 80 ^ "..."
    else message
  in
  let preview = String.map (function '\n' -> ' ' | c -> c) preview in
  Log.Mcp.info "%s → %s" name preview;

  Lwt.return result

(** Route JSON-RPC request to handler *)
let handle_request state request_str =
  Lwt.catch
    (fun () ->
      let json =
        try Ok (Yojson.Safe.from_string request_str)
        with exn -> Error (Printexc.to_string exn)
      in
      match json with
      | Error msg ->
          Lwt.return @@ make_error ~id:`Null (-32700) ("Parse error: " ^ msg)
      | Ok json ->
          if is_jsonrpc_response json then
            Lwt.return `Null
          else if not (is_jsonrpc_v2 json) then
            Lwt.return @@ make_error ~id:`Null (-32600) "Invalid Request: jsonrpc must be 2.0"
          else
            match jsonrpc_request_of_yojson json with
            | Error msg -> Lwt.return @@ make_error ~id:`Null (-32600) ("Invalid Request: " ^ msg)
            | Ok req ->
                let id = get_id req in
                if is_notification req then
                  Lwt.return `Null
                else
                  (match req.method_ with
                  | "initialize" -> handle_initialize id req.params
                  | "initialized"
                  | "notifications/initialized" -> Lwt.return (make_response ~id `Null)
                  | "resources/list" -> handle_list_resources id
                  | "resources/read" -> handle_read_resource state id req.params
                  | "resources/templates/list" -> handle_list_resource_templates id
                  | "prompts/list" -> handle_list_prompts id
                  | "tools/list" -> handle_list_tools state id
                  | "tools/call" ->
                      (match req.params with
                      | Some params -> handle_call_tool state id params
                      | None -> Lwt.return @@ make_error ~id (-32602) "Missing params")
                  | method_ -> Lwt.return @@ make_error ~id (-32601) ("Method not found: " ^ method_)))
    (fun exn ->
      Lwt.return @@ make_error ~id:`Null (-32603) ("Internal error: " ^ Printexc.to_string exn))

(** Run MCP server in stdio mode *)
let run_stdio state =
  let open Lwt.Syntax in
  Log.Mcp.info "MASC MCP Server (stdio mode)";
  Log.Mcp.info "Default room: %s" state.room_config.base_path;

  let write_framed json =
    let body = Yojson.Safe.to_string json in
    let header = Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body) in
    let* () = Lwt_io.write Lwt_io.stdout header in
    let* () = Lwt_io.write Lwt_io.stdout body in
    Lwt_io.flush Lwt_io.stdout
  in

  let write_line json =
    let body = Yojson.Safe.to_string json in
    let* () = Lwt_io.write_line Lwt_io.stdout body in
    Lwt_io.flush Lwt_io.stdout
  in

  let starts_with ~prefix s =
    let prefix_len = String.length prefix in
    String.length s >= prefix_len && String.sub s 0 prefix_len = prefix
  in

  let parse_content_length line =
    let line = String.trim line in
    let lower = String.lowercase_ascii line in
    let prefix = "content-length:" in
    if starts_with ~prefix lower then
      let value =
        String.sub line (String.length prefix) (String.length line - String.length prefix)
        |> String.trim
      in
      try Some (int_of_string value) with _ -> None
    else
      None
  in
  
  let read_line_opt () =
    Lwt.catch
      (fun () -> Lwt_io.read_line_opt Lwt_io.stdin)
      (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn)
  in

  let rec read_first_non_empty_line () =
    let* line_opt = read_line_opt () in
    match line_opt with
    | None -> Lwt.return_none
    | Some line ->
        let line = String.trim line in
        if line = "" then read_first_non_empty_line () else Lwt.return_some line
  in

  let is_json_line line =
    match String.trim line with
    | "" -> false
    | s ->
        let first = s.[0] in
        first = '{' || first = '['
  in

  let rec read_headers_until_blank ~seen_header content_length =
    let* line_opt = read_line_opt () in
    match line_opt with
    | None -> Lwt.return_none
    | Some line ->
        let line = String.trim line in
        if line = "" then
          if not seen_header then read_headers_until_blank ~seen_header:false content_length
          else Lwt.return_some content_length
        else
          let content_length =
            match content_length with
            | Some _ -> content_length
            | None -> parse_content_length line
          in
          read_headers_until_blank ~seen_header:true content_length
  in

  let read_framed_body first_line =
    let* content_length_opt =
      read_headers_until_blank ~seen_header:true (parse_content_length first_line)
    in
    match content_length_opt with
    | None -> Lwt.return_none
    | Some None ->
        Log.Mcp.warn "Missing Content-Length header; closing stdio transport.";
        Lwt.return_none
    | Some (Some len) ->
        let* body = Lwt_io.read ~count:len Lwt_io.stdin in
        if String.length body < len then (
          Log.Mcp.warn "EOF while reading body (expected %d bytes, got %d)" len (String.length body);
          Lwt.return_none
        ) else
          Lwt.return_some body
  in

  let read_next_message () =
    let* first_opt = read_first_non_empty_line () in
    match first_opt with
    | None -> Lwt.return_none
    | Some first_line ->
        if is_json_line first_line then
          Lwt.return_some (`Line, first_line)
        else
          let* body_opt = read_framed_body first_line in
          (match body_opt with
          | None -> Lwt.return_none
          | Some body -> Lwt.return_some (`Framed, body))
  in

  let write_response transport json =
    match transport with
    | `Framed -> write_framed json
    | `Line -> write_line json
  in

  let rec loop () =
    let* msg_opt = read_next_message () in
    match msg_opt with
    | None -> Lwt.return_unit
    | Some (transport, request_str) ->
        let* response = handle_request state request_str in
        let* () =
          match response with
          | `Null -> Lwt.return_unit
          | json -> write_response transport json
        in
        loop ()
  in
  loop ()

(** Health check response *)
let health_response state =
  Yojson.Safe.to_string (`Assoc [
    ("status", `String "ok");
    ("server", `String "masc-mcp");
    ("room", `String state.room_config.base_path);
    ("language", `String "ocaml");
  ])

(** REST API helper - Execute tool and return JSON result
    This is a thin wrapper around execute_tool for REST endpoints *)
let rest_execute state ~name ~arguments : Yojson.Safe.t Lwt.t =
  Lwt.catch
    (fun () ->
      let open Lwt.Syntax in
      let* (success, message) = execute_tool state ~name ~arguments in
      (* Try to parse message as JSON, otherwise wrap as text *)
      let result_json =
        try Yojson.Safe.from_string message
        with _ -> `Assoc [("text", `String message)]
      in
      Lwt.return (`Assoc [
        ("success", `Bool success);
        ("result", result_json);
      ]))
    (fun exn ->
      let error_msg = Printexc.to_string exn in
      Log.Mcp.error "REST API error in %s: %s" name error_msg;
      Lwt.return (`Assoc [
        ("success", `Bool false);
        ("error", `String error_msg);
      ]))
