(** MCP Protocol Server Implementation - Eio Native

    Direct-style async MCP server using OCaml 5.x Effect Handlers.
    Legacy bridges have been eliminated as of 2026-01-11.

    Key adapters for Session.registry compatibility:
    - unregister_sync: Direct hashtable removal without extra mutex layer
    - wait_for_message_eio: Polling with Eio.Time.sleep
*)

[@@@warning "-32"]  (* Suppress unused values - kept for potential future use *)

(** Re-export types from Mcp_server for compatibility *)
type server_state = Mcp_server.server_state
type jsonrpc_request = Mcp_server.jsonrpc_request

(** Re-export pure functions from Mcp_server *)
let create_state ?test_mode:_ ~base_path () =
  (* test_mode is ignored - Mcp_server.create_state doesn't support it *)
  Mcp_server.create_state ~base_path

(** Create state with Eio context - required for PostgresNative backend *)
let create_state_eio ~sw ~env ~proc_mgr ~fs ~clock ~base_path =
  Mcp_server.create_state_eio ~sw ~env ~proc_mgr ~fs ~clock ~base_path

let is_jsonrpc_v2 = Mcp_server.is_jsonrpc_v2
let is_jsonrpc_response = Mcp_server.is_jsonrpc_response
let is_notification = Mcp_server.is_notification
let get_id = Mcp_server.get_id
let is_valid_request_id = Mcp_server.is_valid_request_id
let jsonrpc_request_of_yojson = Mcp_server.jsonrpc_request_of_yojson
let protocol_version_from_params = Mcp_server.protocol_version_from_params
let normalize_protocol_version = Mcp_server.normalize_protocol_version
let validate_initialize_params = Mcp_server.validate_initialize_params
let make_response = Mcp_server.make_response
let make_error = Mcp_server.make_error
let is_valid_request_id = Mcp_server.is_valid_request_id
let validate_initialize_params = Mcp_server.validate_initialize_params

(** Heartbeat management module - periodic background broadcasts *)
module Heartbeat = struct
  type t = {
    id: string;
    agent_name: string;
    interval: int;
    message: string;
    mutable active: bool;
    created_at: float;
  }

  let heartbeats : (string, t) Hashtbl.t = Hashtbl.create 16
  let heartbeat_counter = ref 0

  let generate_id () =
    incr heartbeat_counter;
    Printf.sprintf "hb-%d-%d" (int_of_float (Unix.gettimeofday ())) !heartbeat_counter

  let start ~agent_name ~interval ~message =
    let id = generate_id () in
    let hb = { id; agent_name; interval; message; active = true; created_at = Unix.gettimeofday () } in
    Hashtbl.add heartbeats id hb;
    id

  let stop id =
    match Hashtbl.find_opt heartbeats id with
    | Some hb ->
        hb.active <- false;
        Hashtbl.remove heartbeats id;
        true
    | None -> false

  let list () =
    Hashtbl.fold (fun _ hb acc -> hb :: acc) heartbeats []

  let get id = Hashtbl.find_opt heartbeats id
end

(** Unregister agent synchronously - adapter for Session.registry

    Directly removes from hashtable without extra mutex layer.
    Safe in Eio single-fiber context.
*)
let unregister_sync (registry : Session.registry) ~agent_name =
  Hashtbl.remove registry.Session.sessions agent_name;
  Log.Session.info "Session unregistered (sync): %s (total: %d)"
    agent_name (Hashtbl.length registry.sessions)

(** Wait for message using Eio sleep - adapter for Session.registry

    Uses existing Session.pop_message but with Eio.Time.sleep for polling.
    This avoids the legacy bridge while keeping the existing registry structure.
*)
let wait_for_message_eio ~clock (registry : Session.registry) ~agent_name ~timeout =
  let start_time = Unix.gettimeofday () in
  let check_interval = 2.0 in

  (* Ensure session exists *)
  (match Hashtbl.find_opt registry.Session.sessions agent_name with
   | Some _ -> ()
   | None -> ignore (Session.register registry ~agent_name));

  Session.update_activity registry ~agent_name ~is_listening:(Some true) ();

  let rec wait_loop () =
    let elapsed = Unix.gettimeofday () -. start_time in
    if elapsed >= timeout then begin
      Session.update_activity registry ~agent_name ~is_listening:(Some false) ();
      None
    end else begin
      match Session.pop_message registry ~agent_name with
      | Some msg ->
          Session.update_activity registry ~agent_name ~is_listening:(Some false) ();
          Some msg
      | None ->
          Eio.Time.sleep clock check_interval;
          wait_loop ()
    end
  in

  try wait_loop ()
  with _ ->
    Session.update_activity registry ~agent_name ~is_listening:(Some false) ();
    None

(** Handle resources/read - Eio native (pure sync)

    Reads various MASC resources: status, tasks, who, messages.
    All underlying operations are already synchronous.
*)
let handle_read_resource_eio state id params =
  let open Yojson.Safe.Util in
  match params with
  | None -> make_error ~id (-32602) "Missing params"
  | Some (`Assoc _ as p) ->
      let uri_str =
        try p |> member "uri" |> to_string
        with _ -> ""
      in
      if uri_str = "" then
        make_error ~id (-32602) "Missing uri"
      else begin
        let resource_id, uri = Mcp_server.parse_masc_resource_uri uri_str in
        let config = state.Mcp_server.room_config in
        let registry = state.Mcp_server.session_registry in

        let read_messages_json ~since_seq ~limit =
          let msgs_path = Room.messages_dir config in
          if Sys.file_exists msgs_path then
            (* Extract seq number from filename like "000001885_unknown_broadcast.json" or "1664_codex_broadcast.json" *)
            let extract_seq name =
              try
                let idx = String.index name '_' in
                int_of_string (String.sub name 0 idx)
              with _ -> 0
            in
            let files = Sys.readdir msgs_path |> Array.to_list
              |> List.sort (fun a b -> compare (extract_seq b) (extract_seq a)) in
            let count = ref 0 in
            let msgs = ref [] in
            List.iter (fun name ->
              if !count < limit then begin
                let path = Filename.concat msgs_path name in
                let json = Room.read_json config path in
                match Types.message_of_yojson json with
                | Ok msg when msg.Types.seq > since_seq ->
                    msgs := (Types.message_to_yojson msg) :: !msgs;
                    incr count
                | _ -> ()
              end
            ) files;
            `List (List.rev !msgs)
          else
            `List []
        in

        let read_events_json ~limit =
          let lines = Mcp_server.read_event_lines config ~limit in
          let events =
            List.filter_map (fun line ->
              try Some (Yojson.Safe.from_string line) with _ -> None
            ) lines
          in
          `List events
        in

        let read_events_markdown ~limit =
          let lines = Mcp_server.read_event_lines config ~limit in
          if lines = [] then "(no events)"
          else String.concat "\n" (List.map (fun line -> "- " ^ line) lines)
        in

        let (mime_type, text_opt) =
          match resource_id with
          | "status" -> ("text/markdown", Some (Room.status config))
          | "status.json" ->
              let state_json = Types.room_state_to_yojson (Room.read_state config) in
              let backlog_json = Types.backlog_to_yojson (Room.read_backlog config) in
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
              let backlog_json = Types.backlog_to_yojson (Room.read_backlog config) in
              ("application/json", Some (Yojson.Safe.pretty_to_string backlog_json))
          | "who" -> ("text/markdown", Some (Session.status_string registry))
          | "who.json" ->
              let statuses = Session.get_agent_statuses registry in
              ("application/json", Some (Yojson.Safe.pretty_to_string (`List statuses)))
          | "agents" ->
              let json = Room.get_agents_status config in
              ("text/markdown", Some (Yojson.Safe.pretty_to_string json))
          | "agents.json" ->
              let json = Room.get_agents_status config in
              ("application/json", Some (Yojson.Safe.pretty_to_string json))
          | "messages" | "messages/recent" ->
              let since_seq = Mcp_server.int_query_param uri "since_seq" ~default:0 in
              let limit = Mcp_server.int_query_param uri "limit" ~default:10 in
              ("text/markdown", Some (Room.get_messages config ~since_seq ~limit))
          | "messages.json" | "messages.json/recent" ->
              let since_seq = Mcp_server.int_query_param uri "since_seq" ~default:0 in
              let limit = Mcp_server.int_query_param uri "limit" ~default:10 in
              let json = read_messages_json ~since_seq ~limit in
              ("application/json", Some (Yojson.Safe.pretty_to_string json))
          | "events" ->
              let limit = Mcp_server.int_query_param uri "limit" ~default:50 in
              ("text/markdown", Some (read_events_markdown ~limit))
          | "events.json" ->
              let limit = Mcp_server.int_query_param uri "limit" ~default:50 in
              let json = read_events_json ~limit in
              ("application/json", Some (Yojson.Safe.pretty_to_string json))
          | "worktrees" ->
              let json = Room.worktree_list config in
              ("text/markdown", Some (Yojson.Safe.pretty_to_string json))
          | "worktrees.json" ->
              let json = Room.worktree_list config in
              ("application/json", Some (Yojson.Safe.pretty_to_string json))
          | "schema" ->
              ("text/markdown", Some Mcp_server.schema_markdown)
          | "schema.json" ->
              ("application/json", Some (Yojson.Safe.pretty_to_string Mcp_server.schema_json))
          | _ -> ("text/plain", None)
        in

        match text_opt with
        | None -> make_error ~id (-32602) ("Unknown resource: " ^ uri_str)
        | Some text ->
            let contents = `List [
              `Assoc [
                ("uri", `String uri_str);
                ("mimeType", `String mime_type);
                ("text", `String text);
              ]
            ] in
            make_response ~id (`Assoc [("contents", contents)])
      end
  | Some _ ->
      make_error ~id (-32602) "Invalid params"

(** Read Content-Length prefixed message from Eio flow *)
let read_framed_message buf =
  (* Read headers until empty line *)
  let rec read_headers acc =
    let line = Eio.Buf_read.line buf in
    if String.length line = 0 || line = "\r" then
      acc
    else
      read_headers (line :: acc)
  in
  let headers = read_headers [] in

  (* Parse Content-Length *)
  let content_length =
    List.find_map (fun header ->
      let header = String.trim header in
      if String.length header > 16 &&
         String.lowercase_ascii (String.sub header 0 15) = "content-length:" then
        let len_str = String.trim (String.sub header 15 (String.length header - 15)) in
        int_of_string_opt len_str
      else
        None
    ) headers
    |> Option.value ~default:0
  in

  if content_length > 0 then begin
    (* Read exact number of bytes *)
    let body = Eio.Buf_read.take content_length buf in
    Some body
  end else
    None

(** Write Content-Length prefixed message to Eio flow *)
let write_framed_message flow json =
  let body = Yojson.Safe.to_string json in
  let header = Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body) in
  Eio.Flow.copy_string header flow;
  Eio.Flow.copy_string body flow

(** Read newline-delimited message from Eio flow *)
let read_line_message buf =
  try Some (Eio.Buf_read.line buf)
  with End_of_file -> None

(** Write newline-delimited message to Eio flow *)
let write_line_message flow json =
  let body = Yojson.Safe.to_string json in
  Eio.Flow.copy_string body flow;
  Eio.Flow.copy_string "\n" flow

(** Detect transport mode from first line *)
type transport_mode =
  | Framed      (* Content-Length prefixed - MCP stdio mode *)
  | LineDelimited  (* One JSON per line - simple mode *)

let detect_mode first_line =
  let lower = String.lowercase_ascii first_line in
  if String.length lower >= 14 &&
     String.sub lower 0 14 = "content-length" then
    Framed
  else
    LineDelimited

(* ============================================ *)
(* Governance & Audit (Lightweight)            *)
(* ============================================ *)

type governance_config = {
  level: string;
  audit_enabled: bool;
  anomaly_detection: bool;
}

let governance_defaults level =
  let level_lc = String.lowercase_ascii level in
  let audit_enabled =
    match level_lc with
    | "production" | "enterprise" | "paranoid" -> true
    | _ -> false
  in
  let anomaly_detection =
    match level_lc with
    | "enterprise" | "paranoid" -> true
    | _ -> false
  in
  { level = level_lc; audit_enabled; anomaly_detection }

let governance_path (config : Room.config) =
  Filename.concat (Room_utils.masc_dir config) "governance.json"

let ensure_masc_dir (config : Room.config) =
  let dir = Room_utils.masc_dir config in
  if not (Sys.file_exists dir) then
    Room_utils.mkdir_p dir

let load_governance (config : Room.config) : governance_config =
  let path = governance_path config in
  if Room_utils.path_exists config path then
    let json = Room_utils.read_json config path in
    let open Yojson.Safe.Util in
    let level = json |> member "level" |> to_string_option |> Option.value ~default:"development" in
    let defaults = governance_defaults level in
    let audit_enabled =
      match json |> member "audit_enabled" with
      | `Bool b -> b
      | _ -> defaults.audit_enabled
    in
    let anomaly_detection =
      match json |> member "anomaly_detection" with
      | `Bool b -> b
      | _ -> defaults.anomaly_detection
    in
    { level = String.lowercase_ascii level; audit_enabled; anomaly_detection }
  else
    governance_defaults "development"

let save_governance (config : Room.config) (g : governance_config) =
  ensure_masc_dir config;
  let json = `Assoc [
    ("level", `String g.level);
    ("audit_enabled", `Bool g.audit_enabled);
    ("anomaly_detection", `Bool g.anomaly_detection);
    ("updated_at", `String (Types.now_iso ()));
  ] in
  Room_utils.write_json config (governance_path config) json

type audit_event = {
  timestamp: float;
  agent: string;
  event_type: string;
  success: bool;
  detail: string option;
}

let audit_log_path (config : Room.config) =
  Filename.concat (Room_utils.masc_dir config) "audit.log"

let audit_event_to_json (e : audit_event) : Yojson.Safe.t =
  `Assoc [
    ("timestamp", `Float e.timestamp);
    ("agent", `String e.agent);
    ("event_type", `String e.event_type);
    ("success", `Bool e.success);
    ("detail", match e.detail with Some d -> `String d | None -> `Null);
  ]

let append_audit_event (config : Room.config) (e : audit_event) =
  let g = load_governance config in
  if g.audit_enabled then begin
    ensure_masc_dir config;
    let path = audit_log_path config in
    let line = Yojson.Safe.to_string (audit_event_to_json e) ^ "\n" in
    Room_utils.with_file_lock config path (fun () ->
      let oc = open_out_gen [Open_creat; Open_append; Open_wronly] 0o600 path in
      output_string oc line;
      close_out oc
    )
  end

let read_audit_events (config : Room.config) ~since : audit_event list =
  let path = audit_log_path config in
  if not (Sys.file_exists path) then []
  else
    let content = In_channel.with_open_text path In_channel.input_all in
    let lines = String.split_on_char '\n' content |> List.filter (fun s -> String.trim s <> "") in
    List.filter_map (fun line ->
      try
        let json = Yojson.Safe.from_string line in
        let open Yojson.Safe.Util in
        let timestamp = json |> member "timestamp" |> to_float in
        if timestamp < since then None
        else
          let agent = json |> member "agent" |> to_string in
          let event_type = json |> member "event_type" |> to_string in
          let success = json |> member "success" |> to_bool in
          let detail = json |> member "detail" |> to_string_option in
          Some { timestamp; agent; event_type; success; detail }
      with _ -> None
    ) lines

(* ============================================ *)
(* MCP Session (HTTP Session ID) helpers        *)
(* ============================================ *)

type mcp_session_record = {
  id: string;
  agent_name: string option;
  created_at: float;
  last_seen: float;
}

let mcp_sessions_path (config : Room.config) =
  Filename.concat (Room_utils.masc_dir config) "mcp-sessions.json"

let mcp_session_to_json (s : mcp_session_record) : Yojson.Safe.t =
  `Assoc [
    ("id", `String s.id);
    ("agent_name", match s.agent_name with Some a -> `String a | None -> `Null);
    ("created_at", `Float s.created_at);
    ("last_seen", `Float s.last_seen);
  ]

let mcp_session_of_json (json : Yojson.Safe.t) : mcp_session_record option =
  let open Yojson.Safe.Util in
  try
    let id = json |> member "id" |> to_string in
    let agent_name = json |> member "agent_name" |> to_string_option in
    let created_at = json |> member "created_at" |> to_float in
    let last_seen = json |> member "last_seen" |> to_float in
    Some { id; agent_name; created_at; last_seen }
  with _ -> None

let load_mcp_sessions (config : Room.config) : mcp_session_record list =
  let path = mcp_sessions_path config in
  if Room_utils.path_exists config path then
    let json = Room_utils.read_json config path in
    match json with
    | `List items -> List.filter_map mcp_session_of_json items
    | _ -> []
  else
    []

let save_mcp_sessions (config : Room.config) (sessions : mcp_session_record list) =
  ensure_masc_dir config;
  let json = `List (List.map mcp_session_to_json sessions) in
  Room_utils.write_json config (mcp_sessions_path config) json

(* ============================================ *)
(* Drift Guard: similarity helpers              *)
(* ============================================ *)

let tokenize (s : string) : string list =
  let trimmed = String.trim s in
  if trimmed = "" then []
  else
    let tokens = Str.split (Str.regexp "[ \t\r\n]+") trimmed in
    let trim_punct token =
      let is_punct = function
        | '.' | ',' | ';' | ':' | '!' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '"' | '\'' | '`' | '-' | '_' | '/' | '\\' -> true
        | _ -> false
      in
      let len = String.length token in
      if len = 0 then token
      else
        let rec left i =
          if i >= len then len
          else if is_punct token.[i] then left (i + 1) else i
        in
        let rec right i =
          if i < 0 then -1
          else if is_punct token.[i] then right (i - 1) else i
        in
        let l = left 0 in
        let r = right (len - 1) in
        if r < l then "" else String.sub token l (r - l + 1)
    in
    tokens
    |> List.map String.lowercase_ascii
    |> List.map trim_punct
    |> List.filter (fun t -> t <> "")

let jaccard_similarity a b =
  let set_a = Hashtbl.create 128 in
  let set_b = Hashtbl.create 128 in
  List.iter (fun t -> Hashtbl.replace set_a t ()) a;
  List.iter (fun t -> Hashtbl.replace set_b t ()) b;
  let intersection = Hashtbl.fold (fun k _ acc -> if Hashtbl.mem set_b k then acc + 1 else acc) set_a 0 in
  let union = (Hashtbl.length set_a) + (Hashtbl.length set_b) - intersection in
  if union = 0 then 1.0 else float_of_int intersection /. float_of_int union

let cosine_similarity a b =
  let freq tbl t =
    let v = match Hashtbl.find_opt tbl t with Some n -> n | None -> 0 in
    Hashtbl.replace tbl t (v + 1)
  in
  let fa = Hashtbl.create 128 in
  let fb = Hashtbl.create 128 in
  List.iter (freq fa) a;
  List.iter (freq fb) b;
  let dot = Hashtbl.fold (fun k va acc ->
    match Hashtbl.find_opt fb k with
    | Some vb -> acc +. (float_of_int (va * vb))
    | None -> acc
  ) fa 0.0 in
  let norm tbl =
    Hashtbl.fold (fun _ v acc -> acc +. (float_of_int (v * v))) tbl 0.0 |> sqrt
  in
  let na = norm fa in
  let nb = norm fb in
  if na = 0.0 || nb = 0.0 then 0.0 else dot /. (na *. nb)

(** Execute tool - Eio native version.

    Direct-style implementation using Eio-native modules:
    - wait_for_message_eio for session listening (Eio.Time.sleep)
    - Metrics_store_eio for metrics recording (pure sync)
    - Planning_eio for planning operations (pure sync)
    - handle_read_resource_eio for resource reading (pure sync)

    All legacy bridges have been removed.
*)
let execute_tool_eio ~sw ~clock ?mcp_session_id state ~name ~arguments =
  (* clock parameter used for Session_eio.wait_for_message *)
  (* mcp_session_id: HTTP MCP session ID for agent_name persistence across tool calls *)
  let open Yojson.Safe.Util in

  let config = state.Mcp_server.room_config in
  let registry = state.Mcp_server.session_registry in  (* TODO: Use session_registry_eio when migrated *)

  (* Helper: Read agent_name from MCP session file *)
  let read_mcp_session_agent () =
    match mcp_session_id with
    | None -> None
    | Some sid ->
        let file = Printf.sprintf "/tmp/.masc_agent_mcp_%s" sid in
        try
          let ic = open_in file in
          let name = input_line ic in
          close_in ic;
          if name = "" then None else Some name
        with _ -> None
  in

  (* Helper: Write agent_name to MCP session file *)
  let write_mcp_session_agent agent_name =
    match mcp_session_id with
    | None -> ()
    | Some sid ->
        let file = Printf.sprintf "/tmp/.masc_agent_mcp_%s" sid in
        try
          let oc = open_out file in
          output_string oc agent_name;
          close_out oc
        with _ -> ()
  in

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
  let _get_int_opt key =
    try Some (arguments |> member key |> to_int)
    with _ -> None
  in

  (* Resolve agent_name with MCP session persistence:
     1. Use explicit agent_name from arguments if provided
     2. Otherwise, try to read from MCP session file (HTTP session persistence)
     3. Otherwise, try TERM_SESSION_ID file (terminal session persistence)
     4. Finally, generate new UUID if all else fails *)
  let raw_agent_name = get_string "agent_name" "" in
  let agent_name =
    if raw_agent_name <> "" && raw_agent_name <> "unknown" then
      raw_agent_name
    else
      match read_mcp_session_agent () with
      | Some name ->
          Printf.eprintf "[DEBUG] agent_name from MCP session: %s\n%!" name;
          name
      | None ->
          (* Fallback: try TERM_SESSION_ID file *)
          let term_session_id = try Sys.getenv "TERM_SESSION_ID" with Not_found -> "" in
          let term_file = Printf.sprintf "/tmp/.masc_agent_%s" term_session_id in
          (try
            let ic = open_in term_file in
            let name = input_line ic in
            close_in ic;
            if name <> "" then begin
              Printf.eprintf "[DEBUG] agent_name from TERM session: %s\n%!" name;
              name
            end else raise Not_found
          with _ ->
            (* Generate new UUID only as last resort *)
            let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
            let short = String.sub (Uuidm.to_string uuid) 0 8 in
            let generated = Printf.sprintf "agent-%s" short in
            Printf.eprintf "[DEBUG] agent_name generated: %s\n%!" generated;
            generated)
  in

  (* Log tool call *)
  Log.Mcp.debug "[%s] %s" agent_name name;

  (* Update activity for any tool call - TODO: migrate to Session_eio when ready *)
  if agent_name <> "unknown" then begin
    Session.update_activity registry ~agent_name ();
    (* Also update disk-based heartbeat for zombie detection across restarts *)
    if Room.is_initialized config then
      ignore (Room.heartbeat config ~agent_name)
  end;

  (* Auto-join: if agent not in session and tool requires agent, auto-register *)
  let read_only_tools = ["masc_status"; "masc_tasks"; "masc_who"; "masc_agents";
                         "masc_messages"; "masc_task_history"; "masc_votes"; "masc_vote_status";
                         "masc_worktree_list"; "masc_pending_interrupts";
                         "masc_cost_report"; "masc_portal_status"] in
  if agent_name <> "unknown" && not (List.mem name read_only_tools) then begin
    match Hashtbl.find_opt registry.sessions agent_name with
    | Some _ -> ()  (* Already joined *)
    | None ->
        (* Auto-join silently *)
        let agent_file = Filename.concat config.base_path ".masc/agents" |> fun d ->
          Filename.concat d (agent_name ^ ".json") in
        if not (Sys.file_exists agent_file) && Room.is_initialized config then begin
          let _ = Room.join config ~agent_name ~capabilities:[] () in
          Log.Mcp.info "Auto-joined: %s" agent_name
        end;
        let _ = Session.register registry ~agent_name in
        ()
  end;

  (* Helper for result conversion *)
  let result_to_response = function
    | Ok msg -> (true, msg)
    | Error e -> (false, Types.masc_error_to_string e)
  in

  (* Tools that require agent to be joined first *)
  let requires_join = [
    "masc_claim"; "masc_claim_next"; "masc_done"; "masc_transition";
    "masc_broadcast"; "masc_add_task"; "masc_batch_add_tasks";
    "masc_worktree_create"; "masc_worktree_remove";
    "masc_lock"; "masc_unlock"; "masc_release"; "masc_cancel_task";
    "masc_vote_create"; "masc_vote_cast"; "masc_interrupt"; "masc_approve"; "masc_reject";
    "masc_portal_open"; "masc_portal_send"; "masc_portal_close";
    "masc_deliver"; "masc_note_add"; "masc_error_add"; "masc_error_resolve";
  ] in

  (* Check if agent must join first *)
  let join_required = List.mem name requires_join in
  let is_joined = Room.is_agent_joined config ~agent_name in

  (* Debug: log join check *)
  Printf.eprintf "[DEBUG] tool=%s agent_name=%s join_required=%b is_joined=%b\n%!"
    name agent_name join_required is_joined;

  if join_required && not is_joined then
    (false, Printf.sprintf "❌ Join required: Call masc_join first before using %s.\n\n💡 Workflow: masc_join → masc_status → %s\n📚 See: @~/me/instructions/masc-workflow.md\n[DEBUG] agent_name=%s is_joined=%b" name name agent_name is_joined)
  else

  (* Safe exec for checkpoint commands *)
  let safe_exec args =
    try
      let argv = Array.of_list args in
      let cmd = argv.(0) in
      let ic = Unix.open_process_args_in cmd argv in
      let output = In_channel.input_all ic in
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> (true, output)
      | _ -> (false, output)
    with e ->
      (false, Printf.sprintf "❌ Command failed: %s" (Printexc.to_string e))
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
        (false, Printf.sprintf "❌ Directory not found: %s" expanded)
      else begin
        state.Mcp_server.room_config <- Room.default_config expanded;
        let status = if Room.is_initialized state.Mcp_server.room_config then "✅" else "⚠️ (not initialized)" in
        (true, Printf.sprintf "🎯 MASC room set to: %s\n   .masc/ status: %s" expanded status)
      end

  | "masc_init" ->
      let agent = match get_string "agent_name" "" with "" -> None | s -> Some s in
      let result = Room.init config ~agent_name:agent in
      (match agent with
       | Some a -> let _ = Session.register registry ~agent_name:a in ()
       | None -> ());
      (true, result)

  | "masc_join" ->
      let caps = get_string_list "capabilities" in
      let result = Room.join config ~agent_name ~capabilities:caps () in
      (* Extract nickname from join result (format: "  Nickname: xxx\n...") *)
      let nickname =
        try
          let prefix = "  Nickname: " in
          let start_idx =
            let idx = ref 0 in
            while !idx < String.length result - String.length prefix &&
                  String.sub result !idx (String.length prefix) <> prefix do
              incr idx
            done;
            !idx + String.length prefix
          in
          let end_idx = String.index_from result start_idx '\n' in
          String.sub result start_idx (end_idx - start_idx)
        with _ -> agent_name (* Fallback to original if parsing fails *)
      in
      let _ = Session.register registry ~agent_name:nickname in
      (* Save nickname to MCP session file (HTTP persistence) *)
      write_mcp_session_agent nickname;
      Printf.eprintf "[DEBUG] masc_join: saved nickname=%s to MCP session (original=%s)\n%!" nickname agent_name;
      (* Also save to TERM_SESSION_ID file (terminal persistence) *)
      let term_session_id = try Sys.getenv "TERM_SESSION_ID" with Not_found -> "default" in
      let agent_file = Printf.sprintf "/tmp/.masc_agent_%s" term_session_id in
      (try
        let oc = open_out agent_file in
        output_string oc nickname;
        close_out oc
      with _ -> ());
      (true, result)

  | "masc_leave" ->
      let result = Room.leave config ~agent_name in
      unregister_sync registry ~agent_name;
      (* Clean up self-echo filter file *)
      let session_id = try Sys.getenv "TERM_SESSION_ID" with Not_found -> "default" in
      let agent_file = Printf.sprintf "/tmp/.masc_agent_%s" session_id in
      (try Sys.remove agent_file with _ -> ());
      (true, result)

  | "masc_status" ->
      (true, Room.status config)

  | "masc_pause" ->
      let reason = get_string "reason" "Manual pause" in
      Room.pause config ~by:agent_name ~reason;
      (true, Printf.sprintf "⏸️ Room paused by %s: %s" agent_name reason)

  | "masc_resume" ->
      (match Room.resume config ~by:agent_name with
       | `Resumed -> (true, Printf.sprintf "▶️ Room resumed by %s" agent_name)
       | `Already_running -> (true, "Room is not paused"))

  | "masc_pause_status" ->
      (match Room.pause_info config with
       | Some (by, reason, at) ->
           let by_str = Option.value by ~default:"unknown" in
           let reason_str = Option.value reason ~default:"no reason" in
           let at_str = Option.value at ~default:"unknown" in
           (true, Printf.sprintf "⏸️ PAUSED\n  By: %s\n  Reason: %s\n  Since: %s" by_str reason_str at_str)
       | None ->
           (true, "▶️ Room is running (not paused)"))

  | "masc_add_task" ->
      let title = get_string "title" "" in
      let priority = get_int "priority" 3 in
      let description = get_string "description" "" in
      (true, Room.add_task config ~title ~priority ~description)

  | "masc_batch_add_tasks" ->
      let tasks_json = match arguments |> member "tasks" with
        | `List l -> l
        | _ -> []
      in
      let tasks = List.map (fun t ->
        let title = t |> member "title" |> to_string in
        let priority = t |> member "priority" |> to_int_option |> Option.value ~default:3 in
        let description = t |> member "description" |> to_string_option |> Option.value ~default:"" in
        (title, priority, description)
      ) tasks_json in
      (true, Room.batch_add_tasks config tasks)

  | "masc_claim" ->
      let task_id = get_string "task_id" "" in
      result_to_response (Room.claim_task_r config ~agent_name ~task_id)

  | "masc_transition" ->
      let task_id = get_string "task_id" "" in
      let action = get_string "action" "" in
      let notes = get_string "notes" "" in
      let reason = get_string "reason" "" in
      let expected_version = _get_int_opt "expected_version" in
      let action_lc = String.lowercase_ascii action in
      let tasks = Room.get_tasks_raw config in
      let task_opt = List.find_opt (fun (t : Types.task) -> t.id = task_id) tasks in
      let default_time = Unix.gettimeofday () -. 60.0 in
      let (started_at_actual, collaborators_from_task) = match task_opt with
        | Some t -> (match t.task_status with
            | Types.InProgress { started_at; assignee } ->
                let ts = Types.parse_iso8601 ~default_time started_at in
                let collabs = if assignee <> "" && assignee <> agent_name then [assignee] else [] in
                (ts, collabs)
            | Types.Claimed { claimed_at; assignee } ->
                let ts = Types.parse_iso8601 ~default_time claimed_at in
                let collabs = if assignee <> "" && assignee <> agent_name then [assignee] else [] in
                (ts, collabs)
            | _ -> (default_time, []))
        | None -> (default_time, [])
      in
      let result =
        Room.transition_task_r config ~agent_name ~task_id ~action ?expected_version ~notes ~reason ()
      in
      (* Notify A2A subscribers on successful transition *)
      (match result with
       | Ok _ ->
           A2a_tools.notify_event
             ~event_type:A2a_tools.TaskUpdate
             ~agent:agent_name
             ~data:(`Assoc [
               ("task_id", `String task_id);
               ("action", `String action);
               ("notes", `String notes);
             ])
       | Error _ -> ());
      (* Record metrics *)
      (match result, action_lc with
       | Ok _, "done" ->
           let metric : Metrics_store_eio.task_metric = {
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
           ignore (Metrics_store_eio.record config metric)
       | Ok _, "cancel" ->
           let metric : Metrics_store_eio.task_metric = {
             id = Printf.sprintf "metric-%s-%d" task_id (int_of_float (Unix.gettimeofday () *. 1000.));
             agent_id = agent_name;
             task_id;
             started_at = started_at_actual;
             completed_at = Some (Unix.gettimeofday ());
             success = false;
             error_message = Some (if reason = "" then "Cancelled" else reason);
             collaborators = collaborators_from_task;
             handoff_from = None;
             handoff_to = None;
           } in
           ignore (Metrics_store_eio.record config metric)
       | _ -> ());
      result_to_response result

  | "masc_release" ->
      let task_id = get_string "task_id" "" in
      let expected_version = _get_int_opt "expected_version" in
      result_to_response
        (Room.release_task_r config ~agent_name ~task_id ?expected_version ())

  | "masc_done" ->
      let task_id = get_string "task_id" "" in
      let notes = get_string "notes" "" in
      (* Get task info BEFORE completion to extract actual start time *)
      let tasks = Room.get_tasks_raw config in
      let task_opt = List.find_opt (fun (t : Types.task) -> t.id = task_id) tasks in
      let default_time = Unix.gettimeofday () -. 60.0 in
      let (started_at_actual, collaborators_from_task) = match task_opt with
        | Some t -> (match t.task_status with
            | Types.InProgress { started_at; assignee } ->
                let ts = Types.parse_iso8601 ~default_time started_at in
                let collabs = if assignee <> "" && assignee <> agent_name then [assignee] else [] in
                (ts, collabs)
            | Types.Claimed { claimed_at; assignee } ->
                let ts = Types.parse_iso8601 ~default_time claimed_at in
                let collabs = if assignee <> "" && assignee <> agent_name then [assignee] else [] in
                (ts, collabs)
            | _ -> (default_time, []))
        | None -> (default_time, [])
      in
      let result = Room.complete_task_r config ~agent_name ~task_id ~notes in
      (* Notify A2A subscribers on successful completion *)
      (match result with
       | Ok _ ->
           A2a_tools.notify_event
             ~event_type:A2a_tools.TaskUpdate
             ~agent:agent_name
             ~data:(`Assoc [
               ("task_id", `String task_id);
               ("action", `String "done");
               ("notes", `String notes);
             ])
       | Error _ -> ());
      (* Record metrics on successful completion - Eio native (pure sync) *)
      (match result with
       | Ok _ ->
           let metric : Metrics_store_eio.task_metric = {
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
           ignore (Metrics_store_eio.record config metric)
       | Error _ -> ());
      result_to_response result

  | "masc_cancel_task" ->
      let task_id = get_string "task_id" "" in
      let reason = get_string "reason" "" in
      let tasks = Room.get_tasks_raw config in
      let task_opt = List.find_opt (fun (t : Types.task) -> t.id = task_id) tasks in
      let started_at_actual = match task_opt with
        | Some t -> (match t.task_status with
            | Types.InProgress { started_at; _ } ->
                (try Scanf.sscanf started_at "%d-%d-%dT%d:%d:%d"
                  (fun y m d h mi s ->
                    let tm = Unix.{ tm_sec=s; tm_min=mi; tm_hour=h;
                      tm_mday=d; tm_mon=m-1; tm_year=y-1900;
                      tm_wday=0; tm_yday=0; tm_isdst=false } in
                    fst (Unix.mktime tm))
                with _ -> Unix.gettimeofday () -. 60.0)
            | Types.Claimed { claimed_at; _ } ->
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
      (* Record failed metric on cancellation - Eio native (pure sync) *)
      (match result with
       | Ok _ ->
           let metric : Metrics_store_eio.task_metric = {
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
           ignore (Metrics_store_eio.record config metric)
       | Error _ -> ());
      result_to_response result

  | "masc_task_history" ->
      let open Yojson.Safe.Util in
      let task_id = get_string "task_id" "" in
      let limit = get_int "limit" 50 in
      let scan_limit = min 500 (limit * 5) in
      let lines = Mcp_server.read_event_lines config ~limit:scan_limit in
      let parsed =
        List.filter_map (fun line ->
          try Some (Yojson.Safe.from_string line) with _ -> None
        ) lines
      in
      let matches_task json =
        let task = json |> member "task" |> to_string_option in
        let task_id_field = json |> member "task_id" |> to_string_option in
        match task, task_id_field with
        | Some t, _ when t = task_id -> true
        | _, Some t when t = task_id -> true
        | _ -> false
      in
      let rec take n xs =
        match xs with
        | [] -> []
        | _ when n <= 0 -> []
        | x :: rest -> x :: take (n - 1) rest
      in
      let events = parsed |> List.filter matches_task |> take limit in
      (true, Yojson.Safe.pretty_to_string (`List events))

  | "masc_tasks" ->
      (true, Room.list_tasks config)

  | "masc_archive_view" ->
      let limit = get_int "limit" 20 in
      let archive_path = Room_utils.archive_path config in
      if not (Room_utils.path_exists config archive_path) then
        (true, Yojson.Safe.pretty_to_string (`Assoc [("count", `Int 0); ("tasks", `List [])]))
      else
        let open Yojson.Safe.Util in
        let json = Room_utils.read_json config archive_path in
        let tasks =
          match json with
          | `List items -> items
          | `Assoc _ ->
              (match json |> member "tasks" with
               | `List items -> items
               | _ -> [])
          | _ -> []
        in
        let total = List.length tasks in
        let tasks =
          if total <= limit then tasks
          else
            let rec drop n xs =
              match xs with
              | [] -> []
              | _ when n <= 0 -> xs
              | _ :: rest -> drop (n - 1) rest
            in
            drop (total - limit) tasks
        in
        let response = `Assoc [
          ("count", `Int (List.length tasks));
          ("total", `Int total);
          ("tasks", `List tasks);
        ] in
        (true, Yojson.Safe.pretty_to_string response)

  | "masc_claim_next" ->
      (true, Room.claim_next config ~agent_name)

  | "masc_ralph_loop" ->
      let preset = get_string "preset" "drain" in
      let max_iterations = get_int "max_iterations" 10 in
      let target = match get_string "target" "" with "" -> None | t -> Some t in
      (true, Room.ralph_loop config ~agent_name ~preset ~max_iterations ?target ())

  | "masc_ralph_control" ->
      let command = get_string "command" "STATUS" in
      (true, Room.ralph_control config ~from_agent:agent_name ~command ~args:"")

  | "masc_update_priority" ->
      let task_id = get_string "task_id" "" in
      let priority = get_int "priority" 3 in
      (true, Room.update_priority config ~task_id ~priority)

  | "masc_broadcast" ->
      let message = get_string "message" "" in
      (* Check rate limit - Eio native *)
      let allowed, wait_secs = Session.check_rate_limit registry ~agent_name in
      if not allowed then
        (false, Printf.sprintf "⏳ Rate limited! %d초 후 다시 시도하세요." wait_secs)
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
        let _ = Session.push_message registry ~from_agent:agent_name ~content:message ~mention in
        (* Push to SSE clients immediately *)
        let notification = `Assoc [
          ("type", `String "masc/broadcast");
          ("from", `String agent_name);
          ("content", `String message);
          ("mention", match mention with Some m -> `String m | None -> `Null);
          ("timestamp", `String (Types.now_iso ()));
        ] in
        Mcp_server.sse_broadcast state notification;
        (* macOS notification for @mention *)
        (match mention with
         | Some target -> Notify.notify_mention ~from_agent:agent_name ~target_agent:target ~message ()
         | None -> ());
        (* Notify A2A subscribers for polling *)
        A2a_tools.notify_event
          ~event_type:A2a_tools.Broadcast
          ~agent:agent_name
          ~data:(`Assoc [
            ("message", `String message);
            ("mention", match mention with Some m -> `String m | None -> `Null);
          ]);
        (* Auto-Responder: spawn mentioned agent if enabled *)
        let _ = Auto_responder.maybe_respond
          ~base_path:config.base_path
          ~from_agent:agent_name
          ~content:message
          ~mention
        in
        (true, result)
      end

  | "masc_messages" ->
      let since_seq = get_int "since_seq" 0 in
      let limit = get_int "limit" 10 in
      (true, Room.get_messages config ~since_seq ~limit)

  | "masc_lock" ->
      let file = get_string "file" "" in
      let resource =
        if file <> "" then file else get_string "resource" ""
      in
      let result =
        let ( let* ) = Result.bind in
        let lock_owner_of_value value =
          try
            let open Yojson.Safe.Util in
            match Yojson.Safe.from_string value |> member "owner" with
            | `String s -> Some s
            | _ -> None
          with _ -> None
        in
        let* resource = Room_utils.validate_file_path_r resource in
        let ttl_seconds = config.lock_expiry_minutes * 60 in
        match Room_utils.backend_acquire_lock config ~key:resource ~ttl_seconds ~owner:agent_name with
        | Ok true ->
            let now = Unix.gettimeofday () in
            let payload = `Assoc [
              ("resource", `String resource);
              ("owner", `String agent_name);
              ("acquired_at", `Float now);
              ("expires_at", `Float (now +. float_of_int ttl_seconds));
            ] in
            Ok (Yojson.Safe.pretty_to_string payload)
        | Ok false ->
            let owner =
              match Room_utils.backend_get config ~key:("locks:" ^ resource) with
              | Ok (Some v) -> lock_owner_of_value v
              | _ -> None
            in
            let by = Option.value owner ~default:"unknown" in
            Error (Types.FileLocked { file = resource; by })
        | Error msg ->
            Error (Types.IoError (Backend.show_error msg))
      in
      result_to_response result

  | "masc_unlock" ->
      let file = get_string "file" "" in
      let resource =
        if file <> "" then file else get_string "resource" ""
      in
      let result =
        let ( let* ) = Result.bind in
        let lock_owner_of_value value =
          try
            let open Yojson.Safe.Util in
            match Yojson.Safe.from_string value |> member "owner" with
            | `String s -> Some s
            | _ -> None
          with _ -> None
        in
        let* resource = Room_utils.validate_file_path_r resource in
        match Room_utils.backend_release_lock config ~key:resource ~owner:agent_name with
        | Ok true ->
            Ok (Printf.sprintf "🔓 Unlocked: %s" resource)
        | Ok false ->
            let owner =
              match Room_utils.backend_get config ~key:("locks:" ^ resource) with
              | Ok (Some v) -> lock_owner_of_value v
              | _ -> None
            in
            (match owner with
             | Some by -> Error (Types.FileLocked { file = resource; by })
             | None -> Error (Types.FileNotLocked resource))
        | Error msg ->
            Error (Types.IoError (Backend.show_error msg))
      in
      result_to_response result

  | "masc_listen" ->
      let timeout = float_of_int (get_int "timeout" 300) in
      Log.Mcp.info "%s is now listening (timeout: %.0fs)..." agent_name timeout;
      (* Eio native - uses Session registry with Eio.Time.sleep *)
      let msg_opt = wait_for_message_eio ~clock registry ~agent_name ~timeout in
      (match msg_opt with
       | Some msg ->
           let from = Yojson.Safe.Util.(msg |> member "from" |> to_string) in
           let content = Yojson.Safe.Util.(msg |> member "content" |> to_string) in
           let timestamp = Yojson.Safe.Util.(msg |> member "timestamp" |> to_string) in
           (true, Printf.sprintf {|
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
           (true, Printf.sprintf "⏰ Listening timed out after %.0fs. No messages received." timeout))

  | "masc_who" ->
      (true, Session.status_string registry)

  | "masc_reset" ->
      let confirm = get_bool "confirm" false in
      if not confirm then
        (false, "⚠️ This will DELETE the entire .masc/ folder!\nCall with confirm=true to proceed.")
      else
        (true, Room.reset config)

  (* Portal/A2A tools *)
  | "masc_portal_open" ->
      let target_agent = get_string "target_agent" "" in
      let initial_message = match get_string "initial_message" "" with
        | "" -> None
        | s -> Some s
      in
      result_to_response (Room.portal_open_r config ~agent_name ~target_agent ~initial_message)

  | "masc_portal_send" ->
      let message = get_string "message" "" in
      (* macOS notification for portal message *)
      (match Room.get_portal_target config ~agent_name with
       | Some target -> Notify.notify_portal ~from_agent:agent_name ~target_agent:target ~message ()
       | None -> ());
      result_to_response (Room.portal_send_r config ~agent_name ~message)

  | "masc_portal_close" ->
      (true, Room.portal_close config ~agent_name)

  | "masc_portal_status" ->
      let json = Room.portal_status config ~agent_name in
      (true, Yojson.Safe.pretty_to_string json)

  (* Git Worktree tools *)
  | "masc_worktree_create" ->
      let task_id = get_string "task_id" "" in
      let base_branch = get_string "base_branch" "develop" in
      result_to_response (Room.worktree_create_r config ~agent_name ~task_id ~base_branch)

  | "masc_worktree_remove" ->
      let task_id = get_string "task_id" "" in
      result_to_response (Room.worktree_remove_r config ~agent_name ~task_id)

  | "masc_worktree_list" ->
      let json = Room.worktree_list config in
      (true, Yojson.Safe.pretty_to_string json)

  (* Cellular Agent Handover tools - Eio native *)
  | "masc_handover_create" ->
      let task_id = get_string "task_id" "" in
      let session_id = get_string "session_id" "" in
      let reason_str = get_string "reason" "explicit" in
      let reason = match reason_str with
        | "context_limit" -> Handover_eio.ContextLimit (get_int "context_pct" 80)
        | "timeout" -> Handover_eio.Timeout 300
        | "error" -> Handover_eio.FatalError "Unknown error"
        | "complete" -> Handover_eio.TaskComplete
        | _ -> Handover_eio.Explicit
      in
      let h = {
        (Handover_eio.create_handover ~from_agent:agent_name ~task_id ~session_id ~reason) with
        current_goal = get_string "goal" "";
        progress_summary = get_string "progress" "";
        completed_steps = get_string_list "completed_steps";
        pending_steps = get_string_list "pending_steps";
        key_decisions = get_string_list "decisions";
        assumptions = get_string_list "assumptions";
        warnings = get_string_list "warnings";
        unresolved_errors = get_string_list "errors";
        modified_files = get_string_list "files";
        context_usage_percent = get_int "context_pct" 0;
      } in
      (match state.Mcp_server.fs with
       | Some fs ->
           (match Handover_eio.save_handover ~fs config h with
            | Ok () -> (true, Printf.sprintf "✅ Handover DNA created: %s" h.id)
            | Error e -> (false, Printf.sprintf "❌ Failed to save handover: %s" e))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_handover_list" ->
      let pending_only = get_bool "pending_only" false in
      (match state.Mcp_server.fs with
       | Some fs ->
           let handovers =
             if pending_only then Handover_eio.get_pending_handovers ~fs config
             else Handover_eio.list_handovers ~fs config
           in
           let json = `List (List.map Handover_eio.handover_to_json handovers) in
           (true, Yojson.Safe.pretty_to_string json)
       | None -> (false, "❌ Filesystem not available"))

  | "masc_handover_claim" ->
      let handover_id = get_string "handover_id" "" in
      (match state.Mcp_server.fs with
       | Some fs ->
           (match Handover_eio.claim_handover ~fs config ~handover_id ~agent_name with
            | Ok h -> (true, Printf.sprintf "✅ Handover %s claimed by %s" h.id agent_name)
            | Error e -> (false, Printf.sprintf "❌ Failed to claim handover: %s" e))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_handover_claim_and_spawn" ->
      let handover_id = get_string "handover_id" "" in
      let additional_instructions = get_string_opt "additional_instructions" in
      let timeout_seconds = _get_int_opt "timeout_seconds" in
      (match state.Mcp_server.fs, state.Mcp_server.proc_mgr with
       | Some fs, Some pm ->
           (match Handover_eio.claim_and_spawn ~sw ~fs ~proc_mgr:pm config
                    ~handover_id ~agent_name ?additional_instructions ?timeout_seconds () with
            | Ok result -> (true, Spawn_eio.result_to_human_string result)
            | Error e -> (false, Printf.sprintf "❌ Failed to claim/spawn: %s" e))
       | None, _ -> (false, "❌ Filesystem not available")
       | _, None -> (false, "❌ Process manager not available in this environment"))

  | "masc_handover_get" ->
      let handover_id = get_string "handover_id" "" in
      (match state.Mcp_server.fs with
       | Some fs ->
           (match Handover_eio.load_handover ~fs config handover_id with
            | Ok h -> (true, Handover_eio.format_as_markdown h)
            | Error e -> (false, Printf.sprintf "❌ Failed to get handover: %s" e))
       | None -> (false, "❌ Filesystem not available"))

  (* Heartbeat & Agent Health tools *)
  | "masc_heartbeat" ->
      (true, Room.heartbeat config ~agent_name)

  | "masc_heartbeat_start" ->
      let interval = get_int "interval" 30 in
      let message = get_string "message" "🏓 heartbeat" in
      (* Validate interval: min 5, max 300 *)
      let interval = max 5 (min 300 interval) in
      let hb_id = Heartbeat.start ~agent_name ~interval ~message in
      (* Start background fiber for actual heartbeat *)
      Eio.Fiber.fork ~sw (fun () ->
        let rec loop () =
          match Heartbeat.get hb_id with
          | Some hb when hb.Heartbeat.active ->
              (* Send heartbeat broadcast *)
              let _ = Room.broadcast config ~from_agent:agent_name ~content:message in
              Eio.Time.sleep clock (float_of_int interval);
              loop ()
          | _ -> () (* Heartbeat stopped or not found *)
        in
        loop ()
      );
      (true, Printf.sprintf "✅ Heartbeat started: %s (interval: %ds, message: %s)" hb_id interval message)

  | "masc_heartbeat_stop" ->
      let hb_id = get_string "heartbeat_id" "" in
      if hb_id = "" then
        (false, "❌ heartbeat_id required")
      else if Heartbeat.stop hb_id then
        (true, Printf.sprintf "✅ Heartbeat stopped: %s" hb_id)
      else
        (false, Printf.sprintf "❌ Heartbeat not found: %s" hb_id)

  | "masc_heartbeat_list" ->
      let hbs = Heartbeat.list () in
      let fmt_hb hb =
        let uptime = int_of_float (Unix.gettimeofday () -. hb.Heartbeat.created_at) in
        Printf.sprintf "  • %s: agent=%s interval=%ds message=\"%s\" uptime=%ds"
          hb.Heartbeat.id hb.agent_name hb.interval hb.message uptime
      in
      let list_str =
        if List.length hbs = 0 then "No active heartbeats"
        else "Active heartbeats:\n" ^ String.concat "\n" (List.map fmt_hb hbs)
      in
      (true, list_str)

  | "masc_cleanup_zombies" ->
      (true, Room.cleanup_zombies config)

  | "masc_gc" ->
      let days = get_int "days" 7 in
      (true, Room.gc config ~days ())

  | "masc_agents" ->
      let json = Room.get_agents_status config in
      (true, Yojson.Safe.pretty_to_string json)

  (* Agent Discovery tools *)
  | "masc_register_capabilities" ->
      let capabilities = get_string_list "capabilities" in
      (true, Room.register_capabilities config ~agent_name ~capabilities)

  | "masc_agent_update" ->
      let status = get_string_opt "status" in
      let capabilities =
        match arguments |> Yojson.Safe.Util.member "capabilities" with
        | `Null -> None
        | `List _ -> Some (get_string_list "capabilities")
        | _ -> None
      in
      result_to_response (Room.update_agent_r config ~agent_name ?status ?capabilities ())

  | "masc_find_by_capability" ->
      let capability = get_string "capability" "" in
      let json = Room.find_agents_by_capability config ~capability in
      (true, Yojson.Safe.pretty_to_string json)

  (* Metrics & Fitness tools *)
  | "masc_get_metrics" ->
      let target = get_string "agent_name" "" in
      let days = get_int "days" 7 in
      (match Metrics_store_eio.calculate_agent_metrics config ~agent_id:target ~days with
       | Some metrics ->
           (true, Yojson.Safe.pretty_to_string (Metrics_store_eio.agent_metrics_to_yojson metrics))
       | None ->
           (false, Printf.sprintf "❌ No metrics found for agent: %s" target))

  | "masc_agent_fitness" ->
      let agent_opt = get_string_opt "agent_name" in
      let days = get_int "days" 7 in
      let agents =
        match agent_opt with
        | Some a -> [a]
        | None -> Metrics_store_eio.get_all_agents config
      in
      if agents = [] then
        (true, Yojson.Safe.pretty_to_string (`Assoc [("count", `Int 0); ("agents", `List [])]))
      else
        let metrics_for agent_id =
          match Metrics_store_eio.calculate_agent_metrics config ~agent_id ~days with
          | Some m -> m
          | None ->
              let now = Unix.gettimeofday () in
              { Metrics_store_eio.agent_id = agent_id;
                period_start = now -. (float_of_int days *. 86400.0);
                period_end = now;
                total_tasks = 0;
                completed_tasks = 0;
                failed_tasks = 0;
                avg_completion_time_s = 0.0;
                task_completion_rate = 0.0;
                error_rate = 0.0;
                handoff_success_rate = 0.0;
                unique_collaborators = [];
              }
        in
        let metrics_list = List.map (fun a -> (a, metrics_for a)) agents in
        let min_avg_time =
          metrics_list
          |> List.map (fun (_, m) -> m.Metrics_store_eio.avg_completion_time_s)
          |> List.filter (fun t -> t > 0.0)
          |> List.fold_left (fun acc t -> if acc = 0.0 || t < acc then t else acc) 0.0
        in
        let max_collabs =
          metrics_list
          |> List.map (fun (_, m) -> List.length m.Metrics_store_eio.unique_collaborators)
          |> List.fold_left max 0
        in
        let score_for metrics =
          let has_data = metrics.Metrics_store_eio.total_tasks > 0 in
          let completion = metrics.Metrics_store_eio.task_completion_rate in
          let reliability = if has_data then 1.0 -. metrics.Metrics_store_eio.error_rate else 0.0 in
          let handoff = if has_data then metrics.Metrics_store_eio.handoff_success_rate else 0.0 in
          let speed =
            if has_data && metrics.Metrics_store_eio.avg_completion_time_s > 0.0 && min_avg_time > 0.0 then
              min 1.0 (min_avg_time /. metrics.Metrics_store_eio.avg_completion_time_s)
            else 0.0
          in
          let collab_count = List.length metrics.Metrics_store_eio.unique_collaborators in
          let collaboration =
            if max_collabs = 0 then 0.0
            else float_of_int collab_count /. float_of_int max_collabs
          in
          let score =
            (0.35 *. completion) +. (0.25 *. reliability) +. (0.15 *. speed)
            +. (0.15 *. handoff) +. (0.10 *. collaboration)
          in
          (score, completion, reliability, speed, handoff, collaboration)
        in
        let agents_json =
          List.map (fun (agent_id, metrics) ->
            let (score, completion, reliability, speed, handoff, collaboration) = score_for metrics in
            `Assoc [
              ("agent_id", `String agent_id);
              ("fitness", `Float score);
              ("components", `Assoc [
                ("completion", `Float completion);
                ("reliability", `Float reliability);
                ("speed", `Float speed);
                ("handoff", `Float handoff);
                ("collaboration", `Float collaboration);
              ]);
              ("metrics", Metrics_store_eio.agent_metrics_to_yojson metrics);
            ]
          ) metrics_list
        in
        let json = `Assoc [
          ("count", `Int (List.length agents_json));
          ("agents", `List agents_json);
        ] in
        (true, Yojson.Safe.pretty_to_string json)

  | "masc_select_agent" ->
      let available = match arguments |> Yojson.Safe.Util.member "available_agents" with
        | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []
      in
      let strategy = get_string "strategy" "capability_first" in
      let days = get_int "days" 7 in
      if available = [] then
        (false, "❌ available_agents required")
      else
        let metrics_for agent_id =
          match Metrics_store_eio.calculate_agent_metrics config ~agent_id ~days with
          | Some m -> m
          | None ->
              let now = Unix.gettimeofday () in
              { Metrics_store_eio.agent_id = agent_id;
                period_start = now -. (float_of_int days *. 86400.0);
                period_end = now;
                total_tasks = 0;
                completed_tasks = 0;
                failed_tasks = 0;
                avg_completion_time_s = 0.0;
                task_completion_rate = 0.0;
                error_rate = 0.0;
                handoff_success_rate = 0.0;
                unique_collaborators = [];
              }
        in
        let metrics_list = List.map (fun a -> (a, metrics_for a)) available in
        let min_avg_time =
          metrics_list
          |> List.map (fun (_, m) -> m.Metrics_store_eio.avg_completion_time_s)
          |> List.filter (fun t -> t > 0.0)
          |> List.fold_left (fun acc t -> if acc = 0.0 || t < acc then t else acc) 0.0
        in
        let max_collabs =
          metrics_list
          |> List.map (fun (_, m) -> List.length m.Metrics_store_eio.unique_collaborators)
          |> List.fold_left max 0
        in
        let score_for metrics =
          let has_data = metrics.Metrics_store_eio.total_tasks > 0 in
          let completion = metrics.Metrics_store_eio.task_completion_rate in
          let reliability = if has_data then 1.0 -. metrics.Metrics_store_eio.error_rate else 0.0 in
          let handoff = if has_data then metrics.Metrics_store_eio.handoff_success_rate else 0.0 in
          let speed =
            if has_data && metrics.Metrics_store_eio.avg_completion_time_s > 0.0 && min_avg_time > 0.0 then
              min 1.0 (min_avg_time /. metrics.Metrics_store_eio.avg_completion_time_s)
            else 0.0
          in
          let collab_count = List.length metrics.Metrics_store_eio.unique_collaborators in
          let collaboration =
            if max_collabs = 0 then 0.0
            else float_of_int collab_count /. float_of_int max_collabs
          in
          let score =
            (0.35 *. completion) +. (0.25 *. reliability) +. (0.15 *. speed)
            +. (0.15 *. handoff) +. (0.10 *. collaboration)
          in
          (score, completion, reliability, speed, handoff, collaboration)
        in
        let scored =
          List.map (fun (agent_id, metrics) ->
            let (score, completion, reliability, speed, handoff, collaboration) = score_for metrics in
            (agent_id, score,
             `Assoc [
               ("completion", `Float completion);
               ("reliability", `Float reliability);
               ("speed", `Float speed);
               ("handoff", `Float handoff);
               ("collaboration", `Float collaboration);
             ])
          ) metrics_list
        in
        let pick_random lst =
          let idx = Random.int (List.length lst) in
          List.nth lst idx
        in
        let selected =
          match strategy with
          | "random" -> pick_random scored
          | "roulette_wheel" ->
              let total = List.fold_left (fun acc (_, s, _) -> acc +. max 0.0 s) 0.0 scored in
              if total <= 0.0 then pick_random scored
              else
                let target = Random.float total in
                let rec pick acc = function
                  | [] -> List.hd scored
                  | (id, s, comp) :: rest ->
                      let acc' = acc +. max 0.0 s in
                      if acc' >= target then (id, s, comp) else pick acc' rest
                in
                pick 0.0 scored
          | "elite_1" | "capability_first" | _ ->
              List.fold_left (fun best candidate ->
                match best with
                | None -> Some candidate
                | Some (_, best_score, _) ->
                    let (_, score, _) = candidate in
                    if score > best_score then Some candidate else best
              ) None scored |> Option.get
        in
        let (agent_id, score, components) = selected in
        let scores_json =
          `List (List.map (fun (id, s, comp) ->
            `Assoc [
              ("agent_id", `String id);
              ("fitness", `Float s);
              ("components", comp);
            ]) scored)
        in
        let json = `Assoc [
          ("selected_agent", `String agent_id);
          ("fitness", `Float score);
          ("components", components);
          ("strategy", `String strategy);
          ("scores", scores_json);
        ] in
        (true, Yojson.Safe.pretty_to_string json)

  (* Hebbian learning tools *)
  | "masc_collaboration_graph" ->
      let format = get_string "format" "text" in
      let (synapses, agents) = Hebbian_eio.get_graph_data config in
      if format = "json" then
        let json = `Assoc [
          ("agents", `List (List.map (fun a -> `String a) agents));
          ("synapses", `List (List.map Hebbian_eio.synapse_to_json synapses));
        ] in
        (true, Yojson.Safe.pretty_to_string json)
      else
        let lines =
          synapses
          |> List.sort (fun a b -> compare b.Hebbian_eio.weight a.Hebbian_eio.weight)
          |> List.map (fun s ->
              Printf.sprintf "%s → %s (%.2f, success:%d, failure:%d)"
                s.Hebbian_eio.from_agent s.Hebbian_eio.to_agent
                s.Hebbian_eio.weight s.Hebbian_eio.success_count s.Hebbian_eio.failure_count)
        in
        if lines = [] then
          (true, "No collaboration data yet.")
        else
          (true, String.concat "\n" lines)

  | "masc_consolidate_learning" ->
      let decay_after_days = get_int "decay_after_days" 7 in
      let pruned = Hebbian_eio.consolidate config ~decay_after_days () in
      (true, Printf.sprintf "✅ Consolidated. Pruned %d weak connections." pruned)

  (* Drift guard *)
  | "masc_verify_handoff" ->
      let original = get_string "original" "" in
      let received = get_string "received" "" in
      let threshold = get_float "threshold" (Level2_config.Drift_guard.default_threshold ()) in
      let tokens_a = tokenize original in
      let tokens_b = tokenize received in
      let jacc = jaccard_similarity tokens_a tokens_b in
      let cos = cosine_similarity tokens_a tokens_b in
      let weights = Level2_config.Drift_guard.weights () in
      let combined = (weights.jaccard *. jacc)
                     +. (weights.cosine *. cos) in
      let passed = combined >= threshold in
      let json = `Assoc [
        ("similarity", `Float combined);
        ("jaccard", `Float jacc);
        ("cosine", `Float cos);
        ("threshold", `Float threshold);
        ("passed", `Bool passed);
      ] in
      (true, Yojson.Safe.pretty_to_string json)

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
        | _ ->
            `Assoc [
              ("card", json);
              ("endpoint", `String "/.well-known/agent-card.json");
            ]
      in
      (true, Yojson.Safe.pretty_to_string response)

  (* A2A MCP Tools *)
  | "masc_a2a_discover" ->
      let endpoint = get_string_opt "endpoint" in
      let capability = get_string_opt "capability" in
      (match A2a_tools.discover config ?endpoint ?capability () with
       | Ok json -> (true, Yojson.Safe.pretty_to_string json)
       | Error e -> (false, Printf.sprintf "❌ Discovery failed: %s" e))

  | "masc_a2a_query_skill" ->
      let skill_agent_name = get_string "agent_name" "" in
      let skill_id = get_string "skill_id" "" in
      (match A2a_tools.query_skill config ~agent_name:skill_agent_name ~skill_id with
       | Ok json -> (true, Yojson.Safe.pretty_to_string json)
       | Error e -> (false, Printf.sprintf "❌ Query skill failed: %s" e))

  | "masc_a2a_delegate" ->
      let delegate_agent_name = get_string "agent_name" "claude" in
      let target = get_string "target_agent" "" in
      let message = get_string "message" "" in
      let task_type_str = get_string "task_type" "async" in
      let timeout = get_int "timeout" 300 in
      let artifacts = match Yojson.Safe.Util.member "artifacts" arguments with
        | `Null -> []
        | `List items ->
            List.filter_map (fun item ->
              match A2a_tools.artifact_of_yojson item with
              | Ok a -> Some a
              | Error _ -> None) items
        | _ -> []
      in
      (match A2a_tools.delegate config ~agent_name:delegate_agent_name ~target ~message
               ~task_type_str ~artifacts ~timeout () with
       | Ok json -> (true, Yojson.Safe.pretty_to_string json)
       | Error e -> (false, Printf.sprintf "❌ Delegation failed: %s" e))

  | "masc_a2a_subscribe" ->
      let agent_filter = get_string_opt "agent_name" in
      let events = match Yojson.Safe.Util.member "events" arguments with
        | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []
      in
      Printf.eprintf "[MCP] subscribe: agent_filter=%s, events=%d\n%!"
        (Option.value ~default:"*" agent_filter) (List.length events);
      (try
        match A2a_tools.subscribe ?agent_filter ~events () with
        | Ok json ->
            Printf.eprintf "[MCP] subscribe: OK\n%!";
            (true, Yojson.Safe.pretty_to_string json)
        | Error e ->
            Printf.eprintf "[MCP] subscribe: Error=%s\n%!" e;
            (false, Printf.sprintf "❌ Subscribe failed: %s" e)
      with exn ->
        Printf.eprintf "[MCP] subscribe: Exception=%s\n%!" (Printexc.to_string exn);
        (false, Printf.sprintf "❌ Subscribe exception: %s" (Printexc.to_string exn)))

  | "masc_a2a_unsubscribe" ->
      let subscription_id = get_string "subscription_id" "" in
      (match A2a_tools.unsubscribe ~subscription_id with
       | Ok json -> (true, Yojson.Safe.pretty_to_string json)
       | Error e -> (false, Printf.sprintf "❌ Unsubscribe failed: %s" e))

  | "masc_poll_events" ->
      let subscription_id = get_string "subscription_id" "" in
      let clear = get_bool "clear" true in
      (match A2a_tools.poll_events ~subscription_id ~clear () with
       | Ok json -> (true, Yojson.Safe.pretty_to_string json)
       | Error e -> (false, Printf.sprintf "❌ Poll events failed: %s" e))

  (* Planning tools - Eio native (pure sync) *)
  | "masc_plan_init" ->
      let task_id = get_string "task_id" "" in
      let result = Planning_eio.init config ~task_id in
      (match result with
       | Ok _ctx ->
           let response = `Assoc [
             ("status", `String "initialized");
             ("task_id", `String task_id);
             ("message", `String (Printf.sprintf "Planning context created for %s" task_id));
           ] in
           (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           (false, Printf.sprintf "❌ Failed to init planning: %s" e))

  | "masc_plan_update" ->
      let task_id = get_string "task_id" "" in
      let content = get_string "content" "" in
      let result = Planning_eio.update_plan config ~task_id ~content in
      (match result with
       | Ok ctx ->
           let response = `Assoc [
             ("status", `String "updated");
             ("task_id", `String task_id);
             ("updated_at", `String ctx.Planning_eio.updated_at);
           ] in
           (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           (false, Printf.sprintf "❌ Failed to update plan: %s" e))

  | "masc_note_add" ->
      let task_id = get_string "task_id" "" in
      let note = get_string "note" "" in
      let result = Planning_eio.add_note config ~task_id ~note in
      (match result with
       | Ok ctx ->
           let response = `Assoc [
             ("status", `String "added");
             ("task_id", `String task_id);
             ("note_count", `Int (List.length ctx.Planning_eio.notes));
           ] in
           (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           (false, Printf.sprintf "❌ Failed to add note: %s" e))

  | "masc_deliver" ->
      let task_id = get_string "task_id" "" in
      let content = get_string "content" "" in
      let result = Planning_eio.set_deliverable config ~task_id ~content in
      (match result with
       | Ok ctx ->
           let response = `Assoc [
             ("status", `String "delivered");
             ("task_id", `String task_id);
             ("updated_at", `String ctx.Planning_eio.updated_at);
           ] in
           (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           (false, Printf.sprintf "❌ Failed to set deliverable: %s" e))

  | "masc_plan_get" ->
      let task_id_input = get_string "task_id" "" in
      (match Planning_eio.resolve_task_id config ~task_id:task_id_input with
       | Error e -> (false, Printf.sprintf "❌ %s" e)
       | Ok task_id ->
           let result = Planning_eio.load config ~task_id in
           (match result with
            | Ok ctx ->
                let markdown = Planning_eio.get_context_markdown ctx in
                let response = `Assoc [
                  ("task_id", `String task_id);
                  ("context", Planning_eio.planning_context_to_yojson ctx);
                  ("markdown", `String markdown);
                ] in
                (true, Yojson.Safe.pretty_to_string response)
            | Error e ->
                (false, Printf.sprintf "❌ Planning context not found: %s" e)))

  | "masc_error_add" ->
      let task_id = get_string "task_id" "" in
      let error_type = get_string "error_type" "" in
      let message = get_string "message" "" in
      let context = match get_string "context" "" with "" -> None | s -> Some s in
      let result = Planning_eio.add_error config ~task_id ~error_type ~message ?context () in
      (match result with
       | Ok ctx ->
           let unresolved_count = List.length (List.filter (fun (e : Planning_eio.error_entry) -> not e.resolved) ctx.errors) in
           let response = `Assoc [
             ("status", `String "added");
             ("task_id", `String task_id);
             ("error_type", `String error_type);
             ("total_errors", `Int (List.length ctx.errors));
             ("unresolved_count", `Int unresolved_count);
           ] in
           (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           (false, Printf.sprintf "❌ Failed to add error: %s" e))

  | "masc_error_resolve" ->
      let task_id = get_string "task_id" "" in
      let error_index = get_int "error_index" 0 in
      let result = Planning_eio.resolve_error config ~task_id ~index:error_index in
      (match result with
       | Ok ctx ->
           let unresolved_count = List.length (List.filter (fun (e : Planning_eio.error_entry) -> not e.resolved) ctx.errors) in
           let response = `Assoc [
             ("status", `String "resolved");
             ("task_id", `String task_id);
             ("error_index", `Int error_index);
             ("remaining_unresolved", `Int unresolved_count);
           ] in
           (true, Yojson.Safe.pretty_to_string response)
       | Error e ->
           (false, Printf.sprintf "❌ Failed to resolve error: %s" e))

  | "masc_plan_set_task" ->
      let task_id = get_string "task_id" "" in
      if task_id = "" then
        (false, "❌ task_id is required")
      else begin
        Planning_eio.set_current_task config ~task_id;
        let response = `Assoc [
          ("status", `String "set");
          ("current_task", `String task_id);
        ] in
        (true, Yojson.Safe.pretty_to_string response)
      end

  | "masc_plan_get_task" ->
      (match Planning_eio.get_current_task config with
       | Some task_id ->
           let response = `Assoc [
             ("current_task", `String task_id);
           ] in
           (true, Yojson.Safe.pretty_to_string response)
       | None ->
           let response = `Assoc [
             ("current_task", `Null);
             ("message", `String "No current task set. Use masc_plan_set_task first.");
           ] in
           (true, Yojson.Safe.pretty_to_string response))

  | "masc_plan_clear_task" ->
      Planning_eio.clear_current_task config;
      let response = `Assoc [
        ("status", `String "cleared");
        ("message", `String "Current task cleared");
      ] in
      (true, Yojson.Safe.pretty_to_string response)

  (* Run tracking tools - Eio native *)
  | "masc_run_init" ->
      let task_id = get_string "task_id" "" in
      let agent = get_string_opt "agent_name" in
      (match Run_eio.init config ~task_id ~agent_name:agent with
       | Ok run ->
           (true, Yojson.Safe.pretty_to_string (Run_eio.run_record_to_json run))
       | Error e ->
           (false, Printf.sprintf "❌ Failed to init run: %s" e))

  | "masc_run_plan" ->
      let task_id = get_string "task_id" "" in
      let plan = get_string "plan" "" in
      (match Run_eio.update_plan config ~task_id ~content:plan with
       | Ok run ->
           (true, Yojson.Safe.pretty_to_string (Run_eio.run_record_to_json run))
       | Error e ->
           (false, Printf.sprintf "❌ Failed to update run plan: %s" e))

  | "masc_run_log" ->
      let task_id = get_string "task_id" "" in
      let note = get_string "note" "" in
      (match Run_eio.append_log config ~task_id ~note with
       | Ok entry ->
           (true, Yojson.Safe.pretty_to_string (Run_eio.log_entry_to_json entry))
       | Error e ->
           (false, Printf.sprintf "❌ Failed to append run log: %s" e))

  | "masc_run_deliverable" ->
      let task_id = get_string "task_id" "" in
      let deliverable = get_string "deliverable" "" in
      (match Run_eio.set_deliverable config ~task_id ~content:deliverable with
       | Ok run ->
           (true, Yojson.Safe.pretty_to_string (Run_eio.run_record_to_json run))
       | Error e ->
           (false, Printf.sprintf "❌ Failed to set run deliverable: %s" e))

  | "masc_run_get" ->
      let task_id = get_string "task_id" "" in
      (match Run_eio.get config ~task_id with
       | Ok json -> (true, Yojson.Safe.pretty_to_string json)
       | Error e -> (false, Printf.sprintf "❌ Failed to get run: %s" e))

  | "masc_run_list" ->
      let json = Run_eio.list config in
      (true, Yojson.Safe.pretty_to_string json)

  (* Cache tools - Eio native *)
  | "masc_cache_set" ->
      let key = get_string "key" "" in
      let value = get_string "value" "" in
      let ttl_seconds = _get_int_opt "ttl_seconds" in
      let tags = get_string_list "tags" in
      (match Cache_eio.set config ~key ~value ?ttl_seconds ~tags () with
       | Ok entry ->
           (true, Yojson.Safe.pretty_to_string (Cache_eio.entry_to_json entry))
       | Error e ->
           (false, Printf.sprintf "❌ Cache set failed: %s" e))

  | "masc_cache_get" ->
      let key = get_string "key" "" in
      (match Cache_eio.get config ~key with
       | Ok (Some entry) ->
           (true, Yojson.Safe.pretty_to_string (`Assoc [
             ("hit", `Bool true);
             ("entry", Cache_eio.entry_to_json entry);
           ]))
       | Ok None ->
           (true, Yojson.Safe.pretty_to_string (`Assoc [
             ("hit", `Bool false);
             ("key", `String key);
           ]))
       | Error e ->
           (false, Printf.sprintf "❌ Cache get failed: %s" e))

  | "masc_cache_delete" ->
      let key = get_string "key" "" in
      (match Cache_eio.delete config ~key with
       | Ok removed ->
           let json = `Assoc [
             ("removed", `Bool removed);
             ("key", `String key);
           ] in
           (true, Yojson.Safe.pretty_to_string json)
       | Error e ->
           (false, Printf.sprintf "❌ Cache delete failed: %s" e))

  | "masc_cache_list" ->
      let tag = get_string_opt "tag" in
      let entries = Cache_eio.list config ?tag () in
      let json = `Assoc [
        ("count", `Int (List.length entries));
        ("entries", `List (List.map Cache_eio.entry_to_json entries));
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_cache_clear" ->
      (match Cache_eio.clear config with
       | Ok count ->
           (true, Printf.sprintf "Cleared %d cache entries" count)
       | Error e ->
           (false, Printf.sprintf "❌ Cache clear failed: %s" e))

  | "masc_cache_stats" ->
      (match Cache_eio.stats config with
       | Ok (total, expired, size_bytes) ->
           let json = `Assoc [
             ("total_entries", `Int total);
             ("expired_entries", `Int expired);
             ("size_bytes", `Float size_bytes);
             ("size_kb", `Float (size_bytes /. 1024.0));
           ] in
           (true, Yojson.Safe.pretty_to_string json)
       | Error e ->
           (false, Printf.sprintf "❌ Cache stats failed: %s" e))

  (* MCP Session / Cancellation / Subscriptions / Progress *)
  | "masc_mcp_session" ->
      let action = get_string "action" "" in
      let now = Unix.gettimeofday () in
      let sessions = load_mcp_sessions config in
      let save sessions = save_mcp_sessions config sessions in
      let response =
        match action with
        | "create" ->
            let agent_name = get_string_opt "agent_name" in
            let id = Mcp_session.generate () in
            let record = { id; agent_name; created_at = now; last_seen = now } in
            save (record :: sessions);
            Ok (`Assoc [
              ("status", `String "created");
              ("session", mcp_session_to_json record);
            ])
        | "get" ->
            let session_id = get_string "session_id" "" in
            (match List.find_opt (fun s -> s.id = session_id) sessions with
             | None -> Error (Printf.sprintf "MCP session '%s' not found" session_id)
             | Some s ->
                 let updated = { s with last_seen = now } in
                 let others = List.filter (fun x -> x.id <> session_id) sessions in
                 save (updated :: others);
                 Ok (`Assoc [
                   ("status", `String "ok");
                   ("session", mcp_session_to_json updated);
                 ]))
        | "list" ->
            Ok (`Assoc [
              ("count", `Int (List.length sessions));
              ("sessions", `List (List.map mcp_session_to_json sessions));
            ])
        | "cleanup" ->
            let cutoff = now -. (7.0 *. 86400.0) in
            let remaining = List.filter (fun s -> s.last_seen >= cutoff) sessions in
            let removed = List.length sessions - List.length remaining in
            save remaining;
            Ok (`Assoc [
              ("status", `String "cleaned");
              ("removed", `Int removed);
              ("remaining", `Int (List.length remaining));
            ])
        | "remove" ->
            let session_id = get_string "session_id" "" in
            let remaining = List.filter (fun s -> s.id <> session_id) sessions in
            if List.length remaining = List.length sessions then
              Error (Printf.sprintf "MCP session '%s' not found" session_id)
            else begin
              save remaining;
              Ok (`Assoc [
                ("status", `String "removed");
                ("session_id", `String session_id);
              ])
            end
        | other ->
            Error (Printf.sprintf "Unknown action: %s" other)
      in
      (match response with
       | Ok json -> (true, Yojson.Safe.pretty_to_string json)
       | Error e -> (false, e))

  | "masc_cancellation" ->
      Cancellation.handle_cancellation_tool arguments

  | "masc_subscription" ->
      Subscriptions.handle_subscription_tool arguments

  | "masc_progress" ->
      Progress.set_sse_callback (Mcp_server.sse_broadcast state);
      Progress.handle_progress_tool arguments

  (* Voting/Consensus tools *)
  | "masc_vote_create" ->
      let proposer = get_string "proposer" agent_name in
      let topic = get_string "topic" "" in
      let options = get_string_list "options" in
      let required_votes = get_int "required_votes" 2 in
      (true, Room.vote_create config ~proposer ~topic ~options ~required_votes)

  | "masc_vote_cast" ->
      let vote_id = get_string "vote_id" "" in
      let choice = get_string "choice" "" in
      (true, Room.vote_cast config ~agent_name ~vote_id ~choice)

  | "masc_vote_status" ->
      let vote_id = get_string "vote_id" "" in
      let json = Room.vote_status config ~vote_id in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_votes" ->
      let json = Room.list_votes config in
      (true, Yojson.Safe.pretty_to_string json)

  (* Tempo Control *)
  | "masc_tempo_get" ->
      let state = Tempo.get_tempo config in
      (true, Yojson.Safe.pretty_to_string (Tempo.state_to_json state))

  | "masc_tempo_set" ->
      let interval = get_float "interval_seconds" 0.0 in
      let reason = get_string "reason" "manual" in
      if interval <= 0.0 then
        (false, "❌ interval_seconds must be > 0")
      else
        let state = Tempo.set_tempo config ~interval_s:interval ~reason in
        (true, Yojson.Safe.pretty_to_string (Tempo.state_to_json state))

  | "masc_tempo_adjust" ->
      let state = Tempo.adjust_tempo config in
      (true, Yojson.Safe.pretty_to_string (Tempo.state_to_json state))

  | "masc_tempo_reset" ->
      let state = Tempo.reset_tempo config in
      (true, Yojson.Safe.pretty_to_string (Tempo.state_to_json state))

  | "masc_tempo" ->
      let action = get_string "action" "get" in
      (match action with
       | "get" ->
           let json = Room.get_tempo config in
           (true, Yojson.Safe.pretty_to_string json)
       | "set" ->
           let mode = get_string "mode" "normal" in
           let reason = get_string_opt "reason" in
           (true, Room.set_tempo config ~mode ~reason ~agent_name)
       | _ ->
           (false, "❌ Unknown action. Use 'get' or 'set'"))

  (* LangGraph Interrupt Pattern handlers *)
  | "masc_interrupt" ->
      let task_id = get_string "task_id" "" in
      let step = get_int "step" 1 in
      let action = get_string "action" "" in
      let message = get_string "message" "" in
      Notify.notify_interrupt ~agent:agent_name ~action;
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
        (false, "❌ task_id, source_step, and branch_name are required")
      else
        safe_exec ["masc-checkpoint"; "--masc-dir"; Room.masc_dir config;
                   "--task-id"; task_id; "--branch"; string_of_int source_step;
                   "--branch-name"; branch_name; "--agent"; agent_name]

  (* Cost Tracking *)
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

  (* Authentication & Authorization *)
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
      (true, msg)

  | "masc_auth_disable" ->
      Auth.disable_auth config.base_path;
      (true, "🔓 Authentication disabled. All agents can perform any action.")

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
      (true, msg)

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
           (true, msg)
       | Error e ->
           (false, Types.masc_error_to_string e))

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
           (true, msg)
       | Error e ->
           (false, Types.masc_error_to_string e))

  | "masc_auth_revoke" ->
      Auth.delete_credential config.base_path agent_name;
      (true, Printf.sprintf "🗑️ Token revoked for %s" agent_name)

  | "masc_auth_list" ->
      let creds = Auth.list_credentials config.base_path in
      if creds = [] then
        (true, "No agent credentials found.")
      else begin
        let buf = Buffer.create 512 in
        Buffer.add_string buf "👥 **Agent Credentials**\n";
        Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
        List.iter (fun (c : Types.agent_credential) ->
          let expires = match c.expires_at with Some exp -> exp | None -> "never" in
          Buffer.add_string buf (Printf.sprintf "  • %s (%s) - expires: %s\n"
            c.agent_name (Types.agent_role_to_string c.role) expires)
        ) creds;
        (true, Buffer.contents buf)
      end

  (* Rate limit tools - Eio native *)
  | "masc_rate_limit_status" ->
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
        let cat_name = cat |> member "category" |> to_string in
        let current = cat |> member "current" |> to_int in
        let cat_limit = cat |> member "limit" |> to_int in
        let remaining = cat |> member "remaining" |> to_int in
        Buffer.add_string buf (Printf.sprintf "  • %s: %d/%d (remaining: %d)\n"
          cat_name current cat_limit remaining)
      );
      (true, Buffer.contents buf)

  | "masc_rate_limit_config" ->
      let cfg = registry.Session.config in
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
      (true, Buffer.contents buf)

  (* Audit & Governance tools *)
  | "masc_audit_query" ->
      let agent_filter = get_string_opt "agent" in
      let event_type = get_string "event_type" "all" in
      let limit = get_int "limit" 50 in
      let since_hours = get_float "since_hours" 24.0 in
      let since = Unix.gettimeofday () -. (since_hours *. 3600.0) in
      let events = read_audit_events config ~since in
      let filtered =
        events
        |> List.filter (fun e ->
            match agent_filter with
            | Some a -> e.agent = a
            | None -> true)
        |> List.filter (fun e ->
            if event_type = "all" then true
            else e.event_type = event_type)
      in
      let limited =
        let rec take n xs =
          match xs with
          | [] -> []
          | _ when n <= 0 -> []
          | x :: rest -> x :: take (n - 1) rest
        in
        take limit filtered
      in
      let json = `Assoc [
        ("count", `Int (List.length limited));
        ("events", `List (List.map audit_event_to_json limited));
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_audit_stats" ->
      let agent_filter = get_string_opt "agent" in
      let since = Unix.gettimeofday () -. (24.0 *. 3600.0) in
      let events = read_audit_events config ~since in
      let agents =
        let from_events = List.map (fun e -> e.agent) events in
        let from_metrics = Metrics_store_eio.get_all_agents config in
        let combined = from_events @ from_metrics in
        List.sort_uniq String.compare combined
      in
      let agents = match agent_filter with
        | Some a -> [a]
        | None -> agents
      in
      let stats_for agent_id =
        let agent_events = List.filter (fun e -> e.agent = agent_id) events in
        let count_type t =
          List.fold_left (fun acc e -> if e.event_type = t then acc + 1 else acc) 0 agent_events
        in
        let auth_success = count_type "auth_success" in
        let auth_failure = count_type "auth_failure" in
        let anomaly = count_type "anomaly_detected" in
        let violations = count_type "security_violation" in
        let tool_calls = count_type "tool_call" in
        let auth_total = auth_success + auth_failure in
        let auth_rate =
          if auth_total = 0 then `Null
          else `Float (float_of_int auth_success /. float_of_int auth_total)
        in
        let task_rate =
          match Metrics_store_eio.calculate_agent_metrics config ~agent_id ~days:7 with
          | Some m -> `Float m.Metrics_store_eio.task_completion_rate
          | None -> `Null
        in
        `Assoc [
          ("agent_id", `String agent_id);
          ("auth_success", `Int auth_success);
          ("auth_failure", `Int auth_failure);
          ("auth_success_rate", auth_rate);
          ("anomaly_count", `Int anomaly);
          ("security_violations", `Int violations);
          ("tool_calls", `Int tool_calls);
          ("task_completion_rate", task_rate);
        ]
      in
      let json = `Assoc [
        ("count", `Int (List.length agents));
        ("agents", `List (List.map stats_for agents));
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_governance_set" ->
      let level = get_string "level" "production" in
      let defaults = governance_defaults level in
      let audit_enabled = get_bool "audit_enabled" defaults.audit_enabled in
      let anomaly_detection = get_bool "anomaly_detection" defaults.anomaly_detection in
      let g = {
        level = String.lowercase_ascii level;
        audit_enabled;
        anomaly_detection;
      } in
      save_governance config g;
      let json = `Assoc [
        ("status", `String "ok");
        ("governance", `Assoc [
          ("level", `String g.level);
          ("audit_enabled", `Bool g.audit_enabled);
          ("anomaly_detection", `Bool g.anomaly_detection);
        ]);
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  (* Encryption tools *)
  | "masc_encryption_status" ->
      let status = Encryption.get_status state.Mcp_server.encryption_config in
      let msg = Printf.sprintf "🔐 Encryption Status\n%s"
        (Yojson.Safe.pretty_to_string status) in
      (true, msg)

  | "masc_encryption_enable" ->
      Encryption.initialize ();
      let key_source = get_string "key_source" "env" in
      let parse_hex_to_bytes hex =
        if String.length hex <> 64 then
          Error (Printf.sprintf "Invalid hex key length: %d (expected 64)" (String.length hex))
        else
          try
            let bytes = Bytes.create 32 in
            for i = 0 to 31 do
              let hex_pair = String.sub hex (i * 2) 2 in
              Bytes.set bytes i (Char.chr (int_of_string ("0x" ^ hex_pair)))
            done;
            Ok (Bytes.to_string bytes)
          with _ -> Error "Failed to decode hex key"
      in
      let (key_source_variant, generated_key_opt) =
        if key_source = "env" then
          (`Env "MASC_ENCRYPTION_KEY", None)
        else if String.length key_source > 5 && String.sub key_source 0 5 = "file:" then
          let path = String.sub key_source 5 (String.length key_source - 5) in
          (`File path, None)
        else if key_source = "generate" then
          match Encryption.generate_key_hex () with
          | Error e -> (`Env "MASC_ENCRYPTION_KEY", Some (Error (Encryption.show_encryption_error e)))
          | Ok hex ->
              (match parse_hex_to_bytes hex with
               | Ok bytes -> (`Direct bytes, Some (Ok hex))
               | Error err -> (`Env "MASC_ENCRYPTION_KEY", Some (Error err)))
        else
          (`Env "MASC_ENCRYPTION_KEY", Some (Error "Invalid key_source"))
      in
      (match generated_key_opt with
       | Some (Error e) -> (false, Printf.sprintf "❌ %s" e)
       | _ ->
           let new_config = { state.Mcp_server.encryption_config with
             Encryption.enabled = true;
             key_source = key_source_variant;
           } in
           (match Encryption.load_key new_config with
            | Error e ->
                (false, Printf.sprintf "❌ Failed: %s" (Encryption.show_encryption_error e))
            | Ok _ ->
                state.Mcp_server.encryption_config <- new_config;
                let msg =
                  match generated_key_opt with
                  | Some (Ok hex) ->
                      Printf.sprintf "🔐 Encryption enabled (generated key).\n\n🔑 Key (hex): %s\n\n⚠️ Store this securely!" hex
                  | _ -> "🔐 Encryption enabled."
                in
                (true, msg)))

  | "masc_encryption_disable" ->
      state.Mcp_server.encryption_config <- { state.Mcp_server.encryption_config with Encryption.enabled = false };
      (true, "🔓 Encryption disabled. New data will be stored in plain text.")

  | "masc_generate_key" ->
      Encryption.initialize ();
      (match Encryption.generate_key_hex () with
      | Error e ->
          (false, Printf.sprintf "❌ Failed: %s" (Encryption.show_encryption_error e))
      | Ok hex_key ->
          let output = get_string "output" "hex" in
          let key_str =
            if output = "base64" then
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
          (true, msg))

  (* Mode management tools *)
  | "masc_switch_mode" ->
      let mode_str = get_string "mode" "standard" in
      let categories_list = get_string_list "categories" in
      let room_path = Room.masc_dir config in
      let result =
        if categories_list <> [] then
          let categories = List.filter_map Mode.category_of_string categories_list in
          if List.length categories <> List.length categories_list then
            Error "❌ Invalid category name(s). Valid: core, comm, portal, worktree, health, discovery, voting, interrupt, cost, auth, ratelimit, encryption"
          else begin
            let cfg = { Config.mode = Mode.Custom; enabled_categories = categories } in
            Config.save room_path cfg;
            Ok (Yojson.Safe.pretty_to_string (Config.get_config_summary room_path))
          end
        else
          match Mode.mode_of_string mode_str with
          | Some mode ->
              let _ = Config.switch_mode room_path mode in
              Ok (Yojson.Safe.pretty_to_string (Config.get_config_summary room_path))
          | None ->
              Error "❌ Invalid mode. Valid: minimal, standard, parallel, full, solo, custom"
      in
      (match result with
       | Ok msg -> (true, msg)
       | Error msg -> (false, msg))

  | "masc_get_config" ->
      let room_path = Room.masc_dir config in
      let summary = Config.get_config_summary room_path in
      (true, Yojson.Safe.pretty_to_string summary)

  | "masc_spawn" ->
      let spawn_agent_name = get_string "agent_name" "" in
      let prompt = get_string "prompt" "" in
      let timeout_seconds = get_int "timeout_seconds" 300 in
      let working_dir = match arguments |> member "working_dir" with
        | `String s when s <> "" -> Some s
        | _ -> None
      in
      (match state.Mcp_server.proc_mgr with
       | Some pm ->
           let result = Spawn_eio.spawn ~sw ~proc_mgr:pm ~agent_name:spawn_agent_name ~prompt ~timeout_seconds ?working_dir () in
           (result.Spawn_eio.success, Spawn_eio.result_to_human_string result)
       | None ->
           (false, "❌ Process manager not available in this environment"))

  (* Dashboard tool *)
  | "masc_dashboard" ->
      let compact = match arguments |> Yojson.Safe.Util.member "compact" with
        | `Bool b -> b
        | _ -> false
      in
      let output =
        if compact then Dashboard.generate_compact config
        else Dashboard.generate config
      in
      (true, output)

  (* Relay tools - Infinite Context via Handoff *)
  | "masc_relay_status" ->
      let messages = get_int "messages" 0 in
      let tool_calls = get_int "tool_calls" 0 in
      let model = get_string "model" "claude" in
      let metrics = Relay.estimate_context ~messages ~tool_calls ~model in
      let should_relay = Relay.should_relay ~config:Relay.default_config ~metrics in
      let json = `Assoc [
        ("estimated_tokens", `Int metrics.Relay.estimated_tokens);
        ("max_tokens", `Int metrics.Relay.max_tokens);
        ("usage_ratio", `Float metrics.Relay.usage_ratio);
        ("message_count", `Int metrics.Relay.message_count);
        ("tool_call_count", `Int metrics.Relay.tool_call_count);
        ("should_relay", `Bool should_relay);
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_relay_checkpoint" ->
      let summary = get_string "summary" "" in
      let current_task = get_string_opt "current_task" in
      let todos = get_string_list "todos" in
      let pdca_state = get_string_opt "pdca_state" in
      let relevant_files = get_string_list "relevant_files" in
      let cell = !(Mcp_server.current_cell) in
      let messages = get_int "messages" cell.Mitosis.task_count in
      let tool_calls = get_int "tool_calls" cell.Mitosis.tool_call_count in
      let metrics = Relay.estimate_context ~messages ~tool_calls ~model:"claude" in
      let _ = Relay.save_checkpoint ~summary ~task:current_task ~todos ~pdca:pdca_state ~files:relevant_files ~metrics in
      let json = `Assoc [
        ("status", `String "checkpoint_saved");
        ("usage_ratio", `Float metrics.Relay.usage_ratio);
        ("estimated_tokens", `Int metrics.Relay.estimated_tokens);
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_relay_now" ->
      let summary = get_string "summary" "" in
      let current_task = get_string_opt "current_task" in
      let target_agent = get_string "target_agent" "claude" in
      let generation = get_int "generation" 0 in
      let payload : Relay.handoff_payload = {
        summary;
        current_task;
        todos = [];
        pdca_state = None;
        relevant_files = [];
        session_id = None;
        relay_generation = generation;
      } in
      let prompt = Relay.build_handoff_prompt ~payload ~generation:(generation + 1) in
      let result = Spawn.spawn ~agent_name:target_agent ~prompt ~timeout_seconds:600 () in
      let output_preview =
        if String.length result.Spawn.output > 500 then
          String.sub result.Spawn.output 0 500
        else result.Spawn.output
      in
      let json = `Assoc [
        ("success", `Bool result.Spawn.success);
        ("exit_code", `Int result.Spawn.exit_code);
        ("elapsed_ms", `Int result.Spawn.elapsed_ms);
        ("target_agent", `String target_agent);
        ("generation", `Int (generation + 1));
        ("output_preview", `String output_preview);
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_relay_smart_check" ->
      let messages = get_int "messages" 0 in
      let tool_calls = get_int "tool_calls" 0 in
      let hint_str = get_string "task_hint" "simple" in
      let file_count = get_int "file_count" 1 in
      let hint =
        match hint_str with
        | "large_file" -> Relay.Large_file_read "unknown"
        | "multi_file" -> Relay.Multi_file_edit (max 1 file_count)
        | "long_running" -> Relay.Long_running_task
        | "exploration" -> Relay.Exploration_task
        | _ -> Relay.Simple_task
      in
      let metrics = Relay.estimate_context ~messages ~tool_calls ~model:"claude" in
      let decision = Relay.should_relay_smart ~config:Relay.default_config ~metrics ~task_hint:hint in
      let decision_str = match decision with
        | `Proactive -> "proactive"
        | `Reactive -> "reactive"
        | `No_relay -> "no_relay"
      in
      let json = `Assoc [
        ("decision", `String decision_str);
        ("usage_ratio", `Float metrics.Relay.usage_ratio);
        ("estimated_tokens", `Int metrics.Relay.estimated_tokens);
        ("max_tokens", `Int metrics.Relay.max_tokens);
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  (* Mitosis tools - Cell Division Pattern *)
  | "masc_mitosis_status" ->
      let cell = !(Mcp_server.current_cell) in
      let pool = !(Mcp_server.stem_pool) in
      let json = `Assoc [
        ("cell", Mitosis.cell_to_json cell);
        ("pool", Mitosis.pool_to_json pool);
        ("config", Mitosis.config_to_json Mitosis.default_config);
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_mitosis_all" ->
      let statuses = Mitosis.get_all_statuses ~room_config:config in
      let json =
        `List (List.map (fun (node_id, status, ratio) ->
          `Assoc [
            ("node_id", `String node_id);
            ("status", `String status);
            ("estimated_ratio", `Float ratio);
          ]) statuses)
      in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_mitosis_pool" ->
      let pool = !(Mcp_server.stem_pool) in
      (true, Yojson.Safe.pretty_to_string (Mitosis.pool_to_json pool))

  | "masc_mitosis_divide" ->
      let summary = get_string "summary" "" in
      let current_task = get_string "current_task" "" in
      let full_context =
        if current_task = "" then summary
        else Printf.sprintf "Summary: %s\n\nCurrent Task: %s" summary current_task
      in
      let cell = !(Mcp_server.current_cell) in
      let config_mitosis = Mitosis.default_config in
      let spawn_fn ~prompt =
        Spawn.spawn ~agent_name:"claude" ~prompt ~timeout_seconds:600 ()
      in
      let (spawn_result, new_cell, new_pool) =
        Mitosis.execute_mitosis ~config:config_mitosis ~pool:!(Mcp_server.stem_pool)
          ~parent:cell ~full_context ~spawn_fn
      in
      Mcp_server.current_cell := new_cell;
      Mcp_server.stem_pool := new_pool;
      Mitosis.write_status_with_backend ~room_config:config ~cell:new_cell ~config:config_mitosis;
      let json = `Assoc [
        ("success", `Bool spawn_result.Spawn.success);
        ("previous_generation", `Int cell.Mitosis.generation);
        ("new_generation", `Int new_cell.Mitosis.generation);
        ("successor_output", `String (String.sub spawn_result.Spawn.output 0 (min 500 (String.length spawn_result.Spawn.output))));
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_mitosis_check" ->
      let context_ratio = get_float "context_ratio" 0.0 in
      let cell = !(Mcp_server.current_cell) in
      let config_mitosis = Mitosis.default_config in
      let should_prepare = Mitosis.should_prepare ~config:config_mitosis ~cell ~context_ratio in
      let should_handoff = Mitosis.should_handoff ~config:config_mitosis ~cell ~context_ratio in
      let json = `Assoc [
        ("should_prepare", `Bool should_prepare);
        ("should_handoff", `Bool should_handoff);
        ("context_ratio", `Float context_ratio);
        ("threshold_prepare", `Float config_mitosis.Mitosis.prepare_threshold);
        ("threshold_handoff", `Float config_mitosis.Mitosis.handoff_threshold);
        ("phase", `String (Mitosis.phase_to_string cell.Mitosis.phase));
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_mitosis_record" ->
      let task_done = get_bool "task_done" false in
      let tool_called = get_bool "tool_called" false in
      let cell = !(Mcp_server.current_cell) in
      let updated = Mitosis.record_activity ~cell ~task_done ~tool_called in
      Mcp_server.current_cell := updated;
      Mitosis.write_status_with_backend ~room_config:config ~cell:updated ~config:Mitosis.default_config;
      let json = `Assoc [
        ("task_count", `Int updated.Mitosis.task_count);
        ("tool_call_count", `Int updated.Mitosis.tool_call_count);
        ("last_activity", `Float updated.Mitosis.last_activity);
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  | "masc_mitosis_prepare" ->
      let full_context = get_string "full_context" "" in
      let cell = !(Mcp_server.current_cell) in
      let prepared = Mitosis.prepare_for_division ~config:Mitosis.default_config ~cell ~full_context in
      Mcp_server.current_cell := prepared;
      Mitosis.write_status_with_backend ~room_config:config ~cell:prepared ~config:Mitosis.default_config;
      let json = `Assoc [
        ("status", `String "prepared");
        ("phase", `String (Mitosis.phase_to_string prepared.Mitosis.phase));
        ("dna_length", `Int (String.length (Option.value ~default:"" prepared.Mitosis.prepared_dna)));
      ] in
      (true, Yojson.Safe.pretty_to_string json)

  (* Memento Mori - Agent self-awareness of mortality *)
  | "masc_memento_mori" ->
      let context_ratio = get_float "context_ratio" 0.0 in
      let full_context = get_string "full_context" "" in
      let summary = get_string "summary" "" in
      let current_task = get_string "current_task" "" in
      let target_agent = get_string "target_agent" "claude" in
      let cell = !(Mcp_server.current_cell) in
      let mitosis_config = Mitosis.default_config in

      (* Use should_prepare and should_handoff directly *)
      let should_prepare_now = Mitosis.should_prepare ~config:mitosis_config ~cell ~context_ratio in
      let should_handoff_now = Mitosis.should_handoff ~config:mitosis_config ~cell ~context_ratio in

      if not should_prepare_now && not should_handoff_now then begin
        (* <50%: Continue working, no action needed *)
        let response = `Assoc [
          ("status", `String "continue");
          ("context_ratio", `Float context_ratio);
          ("threshold_prepare", `Float mitosis_config.prepare_threshold);
          ("threshold_handoff", `Float mitosis_config.handoff_threshold);
          ("message", `String (Printf.sprintf "💚 Context healthy (%.0f%%). Continue working." (context_ratio *. 100.0)));
        ] in
        (true, Yojson.Safe.pretty_to_string response)
      end
      else if should_prepare_now && not should_handoff_now then begin
        (* 50-80%: Prepare DNA but don't handoff yet *)
        if full_context = "" then
          (false, "❌ full_context required when context_ratio > 50%")
        else begin
          let prepared_cell = Mitosis.prepare_for_division ~config:mitosis_config ~cell ~full_context in
          Mcp_server.current_cell := prepared_cell;
          let response = `Assoc [
            ("status", `String "prepared");
            ("context_ratio", `Float context_ratio);
            ("phase", `String (Mitosis.phase_to_string prepared_cell.phase));
            ("dna_extracted", `Bool (prepared_cell.prepared_dna <> None));
            ("message", `String (Printf.sprintf "🟡 Context at %.0f%%. DNA prepared. Handoff at 80%%." (context_ratio *. 100.0)));
          ] in
          (true, Yojson.Safe.pretty_to_string response)
        end
      end
      else begin
        (* >80%: Execute division and spawn successor *)
        if full_context = "" then
          (false, "❌ full_context required for handoff")
        else begin
          (* Create spawn function *)
          let spawn_fn ~prompt =
            Spawn.spawn ~agent_name:target_agent ~prompt ~timeout_seconds:600 ()
          in

          (* Execute full mitosis *)
          let (spawn_result, new_cell, new_pool) =
            Mitosis.execute_mitosis
              ~config:mitosis_config
              ~pool:!(Mcp_server.stem_pool)
              ~parent:cell
              ~full_context:(Printf.sprintf "Summary: %s\n\nCurrent Task: %s\n\nContext:\n%s"
                  (if summary = "" then "Memento mori - context limit reached" else summary)
                  current_task full_context)
              ~spawn_fn
          in
          Mcp_server.current_cell := new_cell;
          Mcp_server.stem_pool := new_pool;

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
          (true, Yojson.Safe.pretty_to_string response)
        end
      end

  (* ============================================ *)
  (* Multi-Room Management                        *)
  (* ============================================ *)

  | "masc_rooms_list" ->
      let result = Room.rooms_list config in
      (true, Yojson.Safe.pretty_to_string result)

  | "masc_room_create" ->
      let name = get_string "name" "" in
      if name = "" then
        (false, "❌ Room name is required")
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
        (success, Yojson.Safe.pretty_to_string result)

  | "masc_room_enter" ->
      let room_id = get_string "room_id" "" in
      if room_id = "" then
        (false, "❌ Room ID is required")
      else
        let agent_type = get_string "agent_type" "claude" in
        let result = Room.room_enter config ~room_id ~agent_type in
        let success = match result with
          | `Assoc fields -> not (List.mem_assoc "error" fields)
          | _ -> false
        in
        (success, Yojson.Safe.pretty_to_string result)

  (* Swarm / Level 4 Emergent Intelligence tools - Eio native *)
  | "masc_swarm_init" ->
      let behavior = get_string "behavior" "flocking" |> Swarm_eio.behavior_of_string in
      let selection_pressure = get_float "selection_pressure" 0.3 in
      let mutation_rate = get_float "mutation_rate" 0.1 in
      let swarm_cfg = {
        (Swarm_eio.default_config ()) with
        behavior;
        selection_pressure;
        mutation_rate;
      } in
      (match state.Mcp_server.fs with
       | Some fs ->
           let swarm = Swarm_eio.create ~fs config ~swarm_config:swarm_cfg () in
           (true, Printf.sprintf "✅ Swarm %s initialized with %s behavior" swarm.swarm_cfg.name (Swarm_eio.behavior_to_string behavior))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_swarm_join" ->
      let join_agent_name = get_string "agent_name" agent_name in
      (match state.Mcp_server.fs with
       | Some fs ->
           (match Swarm_eio.join ~fs config ~agent_id:join_agent_name ~agent_name:join_agent_name with
            | Some _ -> (true, Printf.sprintf "✅ Agent %s joined the swarm" join_agent_name)
            | None -> (false, "❌ Failed to join swarm (full or nonexistent)"))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_swarm_leave" ->
      let leave_agent_name = get_string "agent_name" agent_name in
      (match state.Mcp_server.fs with
       | Some fs ->
           (match Swarm_eio.leave ~fs config ~agent_id:leave_agent_name with
            | Some _ -> (true, Printf.sprintf "✅ Agent %s left the swarm" leave_agent_name)
            | None -> (false, "❌ Failed to leave swarm"))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_swarm_status" ->
      (match state.Mcp_server.fs with
       | Some fs -> (true, Yojson.Safe.pretty_to_string (Swarm_eio.status ~fs config))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_swarm_evolve" ->
      (match state.Mcp_server.fs with
       | Some fs ->
           (match Swarm_eio.evolve ~fs config with
            | Some s -> (true, Printf.sprintf "✅ Swarm evolved to generation %d" s.generation)
            | None -> (false, "❌ Evolution failed"))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_swarm_propose" ->
      let description = get_string "description" "" in
      let threshold = match arguments |> member "threshold" with `Float f -> Some f | _ -> None in
      (match state.Mcp_server.fs with
       | Some fs ->
           (match Swarm_eio.propose ~fs config ~description ~proposed_by:agent_name ?threshold () with
            | Some p -> (true, Printf.sprintf "✅ Proposal %s created" p.proposal_id)
            | None -> (false, "❌ Failed to create proposal"))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_swarm_vote" ->
      let proposal_id = get_string "proposal_id" "" in
      let vote_for = get_bool "vote_for" true in
      (match state.Mcp_server.fs with
       | Some fs ->
           (match Swarm_eio.vote ~fs config ~proposal_id ~agent_id:agent_name ~vote_for with
            | Some p -> (true, Printf.sprintf "✅ Vote recorded. Status: %s" (Swarm_eio.status_to_string p.status))
            | None -> (false, "❌ Failed to record vote"))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_swarm_deposit" ->
      let path_id = get_string "path_id" "" in
      let strength = get_float "strength" 0.2 in
      (match state.Mcp_server.fs with
       | Some fs ->
           (match Swarm_eio.deposit_pheromone ~fs config ~path_id ~agent_id:agent_name ~strength with
            | Some _ -> (true, Printf.sprintf "✅ Pheromone deposited on path: %s" path_id)
            | None -> (false, "❌ Failed to deposit pheromone"))
       | None -> (false, "❌ Filesystem not available"))

  | "masc_swarm_trails" ->
      let limit = get_int "limit" 5 in
      (match state.Mcp_server.fs with
       | Some fs ->
           let trails = Swarm_eio.get_strongest_trails ~fs config ~limit in
           let json = `List (List.map Swarm_eio.pheromone_to_json trails) in
           (true, Yojson.Safe.pretty_to_string json)
       | None -> (false, "❌ Filesystem not available"))

  | _ ->
      (false, Printf.sprintf "❌ Unknown tool: %s" name)

(** {1 Eio-Native JSON-RPC Handlers} *)

(** Eio-native handler for tools/call - uses execute_tool_eio directly *)
let handle_call_tool_eio ~sw ~clock ?mcp_session_id state id params =
  let open Yojson.Safe.Util in
  let name = params |> member "name" |> to_string in
  let arguments = params |> member "arguments" in

  (* Measure execution time for telemetry *)
  let start_time = Eio.Time.now clock in
  let (success, message) = execute_tool_eio ~sw ~clock ?mcp_session_id state ~name ~arguments in
  let end_time = Eio.Time.now clock in
  let duration_ms = int_of_float ((end_time -. start_time) *. 1000.0) in

  (* Audit log (tool_call) if enabled *)
  let agent_name =
    try arguments |> member "agent_name" |> to_string
    with _ -> "unknown"
  in
  append_audit_event state.Mcp_server.room_config {
    timestamp = Unix.gettimeofday ();
    agent = agent_name;
    event_type = "tool_call";
    success;
    detail = Some name;
  };

  (* Track tool call in telemetry (controlled by MASC_TELEMETRY_ENABLED) *)
  let telemetry_enabled =
    match Sys.getenv_opt "MASC_TELEMETRY_ENABLED" with
    | Some "false" | Some "0" -> false
    | _ -> true  (* Default: enabled *)
  in
  if telemetry_enabled then
    (match state.Mcp_server.fs with
     | Some fs ->
         (try Telemetry_eio.track_tool_called ~fs state.Mcp_server.room_config
                ~tool_name:name ~success ~duration_ms ()
          with _ -> ())
     | None -> ());

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

  result

(** Eio-native handlers for simple methods *)
let handle_initialize_eio id params =
  match validate_initialize_params params with
  | Error msg -> make_error ~id (-32602) msg
  | Ok () ->
      let protocol_version =
        params |> protocol_version_from_params |> normalize_protocol_version
      in
      make_response ~id (`Assoc [
        ("protocolVersion", `String protocol_version);
        ("serverInfo", Mcp_server.server_info);
        ("capabilities", Mcp_server.capabilities);
        ("instructions", `String "MASC (Multi-Agent Streaming Coordination) enables AI agent collaboration. \
          ROOM: Agents sharing the same base path (.masc/ folder) or PostgreSQL cluster coordinate together. \
          CLUSTER: Set MASC_CLUSTER_NAME for multi-machine coordination (defaults to basename of ME_ROOT). \
          READ: use resources/list + resources/read (status/tasks/agents/events/schema) for snapshots. \
          WRITE: prefer masc_transition (claim/start/done/cancel/release) with expected_version for CAS. \
          WORKFLOW: masc_status → masc_transition(claim) → masc_worktree_create (isolation) → work → masc_transition(done). \
          Use masc_heartbeat periodically; use @agent mentions in masc_broadcast. \
          Prefer worktrees over file locks for parallel work.");
      ])

let handle_list_tools_eio state id =
  let room_path = Room.masc_dir state.Mcp_server.room_config in
  let config = Config.load room_path in
  let enabled_categories = config.enabled_categories in
  let filtered_schemas = List.filter (fun (schema : Types.tool_schema) ->
    Mode.is_tool_enabled enabled_categories schema.name
  ) Tools.all_schemas in
  let tools = List.map (fun (schema : Types.tool_schema) ->
    `Assoc [
      ("name", `String schema.name);
      ("description", `String schema.description);
      ("inputSchema", schema.input_schema);
    ]
  ) filtered_schemas in
  make_response ~id (`Assoc [("tools", `List tools)])

let handle_list_resources_eio id =
  let resources_json = List.map Mcp_server.resource_to_json Mcp_server.resources in
  make_response ~id (`Assoc [("resources", `List resources_json)])

let handle_list_resource_templates_eio id =
  let templates_json = List.map Mcp_server.resource_template_to_json Mcp_server.resource_templates in
  make_response ~id (`Assoc [("resourceTemplates", `List templates_json)])

let handle_list_prompts_eio id =
  make_response ~id (`Assoc [("prompts", `List [])])

(** Handle incoming JSON-RPC request - Pure Eio Native

    Direct-style async using OCaml 5.x Effect Handlers.
    Uses execute_tool_eio for tool calls.
    mcp_session_id: HTTP MCP session ID for agent_name persistence
*)
let handle_request ~clock ~sw ?mcp_session_id state request_str =
  try
    let json =
      try Ok (Yojson.Safe.from_string request_str)
      with exn -> Error (Printexc.to_string exn)
    in
    match json with
    | Error msg ->
        make_error ~id:`Null ~data:(`String msg) (-32700) "Parse error"
    | Ok json ->
        if is_jsonrpc_response json then
          `Null
        else if not (is_jsonrpc_v2 json) then
          make_error ~id:`Null (-32600) "Invalid Request: jsonrpc must be 2.0"
        else
          match jsonrpc_request_of_yojson json with
          | Error msg -> make_error ~id:`Null ~data:(`String msg) (-32600) "Invalid Request"
          | Ok req ->
              let id = get_id req in
              if not (is_valid_request_id id) then
                make_error ~id:`Null (-32600) "Invalid Request: id must be string, number, or null"
              else if is_notification req then
                `Null
              else
                (match req.method_ with
                | "initialize" -> handle_initialize_eio id req.params
                | "initialized"
                | "notifications/initialized" -> make_response ~id `Null
                | "resources/list" -> handle_list_resources_eio id
                | "resources/read" ->
                    (* Eio native - pure sync resource reading *)
                    handle_read_resource_eio state id req.params
                | "resources/templates/list" -> handle_list_resource_templates_eio id
                | "prompts/list" -> handle_list_prompts_eio id
                | "tools/list" -> handle_list_tools_eio state id
                | "tools/call" ->
                    (match req.params with
                    | Some params ->
                        let name = Yojson.Safe.Util.(params |> member "name" |> to_string) in
                        Printf.eprintf "[MCP] tools/call: %s (id=%s, session=%s)\n%!" name
                          (match id with `Int i -> string_of_int i | `String s -> s | _ -> "?")
                          (match mcp_session_id with Some s -> s | None -> "none");
                        let result = handle_call_tool_eio ~sw ~clock ?mcp_session_id state id params in
                        Printf.eprintf "[MCP] tools/call done: %s\n%!" name;
                        result
                    | None -> make_error ~id (-32602) "Missing params")
                | method_ -> make_error ~id (-32601) ("Method not found: " ^ method_))
  with exn ->
    make_error ~id:`Null ~data:(`String (Printexc.to_string exn)) (-32603) "Internal error"

(** {1 Server Entry Points} *)

(** Run MCP server in stdio mode with Eio

    Supports both:
    - Framed mode (Content-Length header) - standard MCP
    - Line-delimited mode - for simple testing
*)
let run_stdio ~sw ~env state =
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  let clock = Eio.Stdenv.clock env in

  Log.Mcp.info "MASC MCP Server (Eio stdio mode)";
  Log.Mcp.info "Default room: %s" Mcp_server.(state.room_config.Room.base_path);

  (* Buffer for reading - framed mode (Content-Length) only for now *)
  let buf = Eio.Buf_read.of_flow stdin ~max_size:(16 * 1024 * 1024) in

  Log.Mcp.debug "Transport mode: framed (Content-Length)";

  (* Main loop - framed mode only *)
  let rec loop () =
    match read_framed_message buf with
    | None ->
        Log.Mcp.info "EOF received, shutting down";
        ()
    | Some "" ->
        (* Empty body, skip *)
        loop ()
    | Some request_str ->
        (* Handle request with Eio clock - use "stdio" as session ID for agent persistence *)
        let response = handle_request ~clock ~sw ~mcp_session_id:"stdio" state request_str in

        (* Write response if not null *)
        (match response with
         | `Null -> ()
         | json -> write_framed_message stdout json);

        loop ()
  in

  try loop ()
  with
  | End_of_file ->
      Log.Mcp.info "Connection closed"
  | exn ->
      Log.Mcp.error "Server error: %s" (Printexc.to_string exn)
