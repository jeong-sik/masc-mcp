(** MCP Protocol Server Core (Eio-only)

    This module provides shared types/config/resources for the Eio server.
    Legacy handlers have been removed.
*)

(* Compact Protocol v1.3 - used for agent-to-agent communication *)
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

(** JSON-RPC id must be string, number, or null. *)
let is_valid_request_id = function
  | `Null
  | `String _
  | `Int _
  | `Intlit _
  | `Float _ -> true
  | _ -> false

(** Validate initialize params per MCP spec. *)
let validate_initialize_params params =
  let ( let* ) = Result.bind in
  let require_string label = function
    | Some (`String _) -> Ok ()
    | None | Some `Null -> Error ("Missing " ^ label)
    | Some _ -> Error ("Invalid " ^ label)
  in
  let require_assoc label = function
    | Some (`Assoc _ as v) -> Ok v
    | None | Some `Null -> Error ("Missing " ^ label)
    | Some _ -> Error ("Invalid " ^ label)
  in
  match params with
  | None -> Error "Missing params"
  | Some (`Assoc _ as p) ->
      let* () = require_string "protocolVersion" (get_field "protocolVersion" p) in
      let* client_info = require_assoc "clientInfo" (get_field "clientInfo" p) in
      let* () = require_string "clientInfo.name" (get_field "name" client_info) in
      let* () = require_string "clientInfo.version" (get_field "version" client_info) in
      let* _ = require_assoc "capabilities" (get_field "capabilities" p) in
      Ok ()
  | Some _ -> Error "Invalid params: expected object"

(** JSON-RPC response builders *)
let make_response ~id result =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result);
  ]

let make_error ?data ~id code message =
  let error_fields =
    [("code", `Int code); ("message", `String message)]
  in
  let error_fields =
    match data with
    | None -> error_fields
    | Some payload -> error_fields @ [("data", payload)]
  in
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("error", `Assoc error_fields);
  ]

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
  ("version", `String "2.2.1");
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
    uri = "masc://agents";
    name = "Agents (Metadata)";
    description = "Agent registry snapshot (capabilities, tasks, last_seen)";
    mime_type = "text/markdown";
  };
  {
    uri = "masc://agents.json";
    name = "Agents (Metadata, JSON)";
    description = "Agent registry snapshot as JSON";
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
  {
    uri = "masc://events?limit=50";
    name = "Recent Events";
    description = "Recent event log snapshot (task/agent/worktree transitions)";
    mime_type = "text/markdown";
  };
  {
    uri = "masc://events.json?limit=50";
    name = "Recent Events (JSON)";
    description = "Recent event log snapshot as JSON";
    mime_type = "application/json";
  };
  {
    uri = "masc://worktrees";
    name = "Worktrees";
    description = "Git worktree snapshot for the current repo";
    mime_type = "text/markdown";
  };
  {
    uri = "masc://worktrees.json";
    name = "Worktrees (JSON)";
    description = "Git worktree snapshot as JSON";
    mime_type = "application/json";
  };
  {
    uri = "masc://schema";
    name = "Task FSM Schema";
    description = "Task state machine rules (markdown)";
    mime_type = "text/markdown";
  };
  {
    uri = "masc://schema.json";
    name = "Task FSM Schema (JSON)";
    description = "Task state machine rules as JSON";
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
  {
    uri_template = "masc://events{?limit}";
    name = "Events (range)";
    description = "Read recent event log entries with optional limit";
    mime_type = "text/markdown";
  };
  {
    uri_template = "masc://events.json{?limit}";
    name = "Events (range, JSON)";
    description = "Read recent event log entries as JSON with optional limit";
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

(** Read recent event log lines from .masc/events *)
let read_event_lines config ~limit =
  let events_dir = Filename.concat (Room.masc_dir config) "events" in
  if not (Sys.file_exists events_dir) then []
  else
    let month_dirs =
      Sys.readdir events_dir |> Array.to_list |> List.sort compare |> List.rev
    in
    let collected = ref [] in
    let remaining = ref limit in
    let read_lines path =
      let ic = open_in path in
      let rec loop acc =
        match input_line ic with
        | line -> loop (line :: acc)
        | exception End_of_file ->
            close_in ic;
            List.rev acc
      in
      loop []
    in
    let add_lines path =
      if !remaining <= 0 then ()
      else
        let lines = read_lines path in
        let rec take rev_lines =
          match rev_lines with
          | [] -> ()
          | line :: rest ->
              if !remaining > 0 then begin
                collected := line :: !collected;
                decr remaining;
                take rest
              end
        in
        take (List.rev lines)
    in
    List.iter (fun month ->
      if !remaining > 0 then
        let month_path = Filename.concat events_dir month in
        if Sys.file_exists month_path && Sys.is_directory month_path then
          let files =
            Sys.readdir month_path |> Array.to_list |> List.sort compare |> List.rev
          in
          List.iter (fun file ->
            if !remaining > 0 then
              let path = Filename.concat month_path file in
              if Sys.file_exists path then add_lines path
          ) files
    ) month_dirs;
    List.rev !collected

let schema_json =
  `Assoc [
    ("task_statuses", `List [
      `String "todo";
      `String "claimed";
      `String "in_progress";
      `String "done";
      `String "cancelled";
    ]);
    ("actions", `List [
      `String "claim";
      `String "start";
      `String "done";
      `String "cancel";
      `String "release";
    ]);
    ("transitions", `List [
      `Assoc [("action", `String "claim"); ("from", `List [`String "todo"]); ("to", `String "claimed")];
      `Assoc [("action", `String "start"); ("from", `List [`String "claimed"]); ("to", `String "in_progress")];
      `Assoc [("action", `String "done"); ("from", `List [`String "claimed"; `String "in_progress"]); ("to", `String "done")];
      `Assoc [("action", `String "cancel"); ("from", `List [`String "todo"; `String "claimed"; `String "in_progress"]); ("to", `String "cancelled")];
      `Assoc [("action", `String "release"); ("from", `List [`String "claimed"; `String "in_progress"]); ("to", `String "todo")];
    ]);
    ("cas", `Assoc [
      ("field", `String "backlog.version");
      ("parameter", `String "expected_version");
    ]);
  ]

let schema_markdown =
  String.concat "\n" [
    "# Task FSM";
    "";
    "- claim: todo -> claimed";
    "- start: claimed(by you) -> in_progress";
    "- done: claimed/in_progress(by you) -> done";
    "- cancel: todo/claimed/in_progress(by you) -> cancelled";
    "- release: claimed/in_progress(by you) -> todo";
    "";
    "CAS guard: expected_version == backlog.version";
  ]

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
