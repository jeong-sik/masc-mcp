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

(** {1 Network Context for LLM Chain Calls} *)

(** Type alias for generic Eio network capability *)
type eio_net = [`Generic] Eio.Net.ty Eio.Resource.t

(** Global Eio network reference for Walph chain execution.
    Set by main_eio.ml during server initialization.
    Used by walph_loop to call llm-mcp chain.orchestrate. *)
let current_net : eio_net option ref = ref None

(** Set the Eio network reference. Called from main_eio.ml. *)
let set_net net = current_net := Some (net :> eio_net)

(** Get the Eio network reference. Raises if not set. *)
let get_net () : eio_net =
  match !current_net with
  | Some net -> net
  | None -> failwith "Eio net not initialized - call set_net first"

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
let has_field = Mcp_server.has_field
let get_field = Mcp_server.get_field

(* Heartbeat module extracted to lib/heartbeat.ml for testability *)

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
              match Yojson.Safe.from_string line with
              | json -> Some json
              | exception Yojson.Json_error msg ->
                let preview = if String.length line > 50 then String.sub line 0 50 ^ "..." else line in
                Eio.traceln "[WARN] Failed to parse event JSON: %s (line: %s)" msg preview;
                None
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
let execute_tool_eio ~sw ~clock ?mcp_session_id ?auth_token state ~name ~arguments =
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

  let token =
    match get_string_opt "token" with
    | Some t -> Some t
    | None -> auth_token
  in

  (* Enforce tool authorization when enabled *)
  let auth_result =
    if Auth.is_auth_enabled config.base_path then
      match Auth.authorize_tool config.base_path ~agent_name ~token ~tool_name:name with
      | Ok () -> Ok ()
      | Error err -> Error err
    else
      Ok ()
  in

  match auth_result with
  | Error err -> (false, Types.masc_error_to_string err)
  | Ok () ->
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
  let room_initialized = Room.is_initialized config in
  let is_joined =
    if room_initialized then
      (* Some tools (e.g., masc_init) must run before initialization.
         Guard the join check to avoid raising and crashing the server. *)
      try Room.is_agent_joined config ~agent_name with _ -> false
    else
      false
  in

  (* Debug: log join check *)
  Printf.eprintf
    "[DEBUG] tool=%s agent_name=%s join_required=%b room_initialized=%b is_joined=%b\n%!"
    name agent_name join_required room_initialized is_joined;

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

  (* Delegate to extracted tool modules first *)
  let swarm_ctx : Tool_swarm.context = {
    config;
    fs = state.Mcp_server.fs;
    agent_name;
  } in
  let simple_ctx_config = { Tool_plan.config } in
  let simple_ctx_run = { Tool_run.config } in
  let simple_ctx_cache = { Tool_cache.config } in
  let simple_ctx_tempo = { Tool_tempo.config; agent_name } in
  let simple_ctx_mitosis = { Tool_mitosis.config } in
  let simple_ctx_portal : Tool_portal.context = { config; agent_name } in
  let simple_ctx_worktree : Tool_worktree.context = { config; agent_name } in
  let simple_ctx_vote : Tool_vote.context = { config; agent_name } in
  let simple_ctx_a2a : Tool_a2a.context = { config; agent_name } in
  let handover_ctx : Tool_handover.context = {
    config; agent_name;
    fs = state.Mcp_server.fs;
    proc_mgr = state.Mcp_server.proc_mgr;
    sw;
  } in
  let simple_ctx_relay : Tool_relay.context = { config; agent_name } in
  let simple_ctx_heartbeat = { Tool_heartbeat.config; agent_name; sw; clock } in
  let simple_ctx_encryption : Tool_encryption.context = { state } in
  let simple_ctx_auth : Tool_auth.context = { config; agent_name } in
  let simple_ctx_hat : Tool_hat.context = { config; agent_name } in
  let simple_ctx_audit : Tool_audit.context = { config } in
  let simple_ctx_rate_limit : Tool_rate_limit.context = { config; agent_name; registry } in
  let simple_ctx_cost : Tool_cost.context = { agent_name } in
  let simple_ctx_walph : _ Tool_walph.context = { config; agent_name; net = get_net (); clock } in
  let simple_ctx_agent : Tool_agent.context = { config; agent_name } in
  let simple_ctx_lock : Tool_lock.context = { config; agent_name } in
  let simple_ctx_task : Tool_task.context = { config; agent_name } in
  let simple_ctx_room : Tool_room.context = { config; agent_name } in
  let simple_ctx_control : Tool_control.context = { config; agent_name } in
  let simple_ctx_misc : Tool_misc.context = { config; agent_name } in

  (* Chain through all extracted tool modules *)
  match Tool_swarm.dispatch swarm_ctx ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_plan.dispatch simple_ctx_config ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_run.dispatch simple_ctx_run ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_cache.dispatch simple_ctx_cache ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_tempo.dispatch simple_ctx_tempo ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_mitosis.dispatch simple_ctx_mitosis ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_portal.dispatch simple_ctx_portal ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_worktree.dispatch simple_ctx_worktree ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_vote.dispatch simple_ctx_vote ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_a2a.dispatch simple_ctx_a2a ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_handover.dispatch handover_ctx ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_relay.dispatch simple_ctx_relay ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_heartbeat.dispatch simple_ctx_heartbeat ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_encryption.dispatch simple_ctx_encryption ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_auth.dispatch simple_ctx_auth ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_hat.dispatch simple_ctx_hat ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_audit.dispatch simple_ctx_audit ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_rate_limit.dispatch simple_ctx_rate_limit ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_cost.dispatch simple_ctx_cost ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_walph.dispatch simple_ctx_walph ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_agent.dispatch simple_ctx_agent ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_lock.dispatch simple_ctx_lock ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_task.dispatch simple_ctx_task ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_room.dispatch simple_ctx_room ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_control.dispatch simple_ctx_control ~name ~args:arguments with
  | Some result -> result
  | None ->
  match Tool_misc.dispatch simple_ctx_misc ~name ~args:arguments with
  | Some result -> result
  | None ->

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





















  | "masc_bounded_run" ->
      let agents = match arguments |> member "agents" with
        | `List l -> List.filter_map (function `String s -> Some s | _ -> None) l
        | _ -> []
      in
      let prompt = get_string "prompt" "" in
      let constraints_json = arguments |> member "constraints" in
      let goal_json = arguments |> member "goal" in
      let constraints = Bounded.constraints_of_json constraints_json in
      let goal = Bounded.goal_of_json goal_json in
      (match state.Mcp_server.proc_mgr with
       | Some pm ->
           (* Create spawn function that uses proc_mgr *)
           let spawn_fn agent_name prompt =
             Spawn_eio.spawn ~sw ~proc_mgr:pm ~agent_name ~prompt ~timeout_seconds:300 ()
           in
           let result = Bounded.bounded_run ~constraints ~goal ~agents ~prompt ~spawn_fn in
           let json = Bounded.result_to_json result in
           (result.Bounded.status = `Goal_reached, Yojson.Safe.pretty_to_string json)
       | None ->
           (false, "❌ Process manager not available"))


  | "masc_broadcast" ->
      let message = get_string "message" "" in
      (* Check rate limit - Eio native *)
      let allowed, wait_secs = Session.check_rate_limit registry ~agent_name in
      if not allowed then
        (false, Printf.sprintf "⏳ Rate limited! %d초 후 다시 시도하세요." wait_secs)
      else begin
        let result = Room.broadcast config ~from_agent:agent_name ~content:message in
        (* Use Mention module for consistent parsing (Stateless/Stateful/Broadcast) *)
        let mention = Mention.extract message in
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

  (* Swarm tools delegated to Tool_swarm module *)

  | _ ->
      (false, Printf.sprintf "❌ Unknown tool: %s" name)

(** {1 Eio-Native JSON-RPC Handlers} *)

(** Eio-native handler for tools/call - uses execute_tool_eio directly *)
let handle_call_tool_eio ~sw ~clock ?mcp_session_id ?auth_token state id params =
  let open Yojson.Safe.Util in
  let name = params |> member "name" |> to_string in
  let arguments = params |> member "arguments" in

  (* Measure execution time for telemetry *)
  let start_time = Eio.Time.now clock in
  let (success, message) =
    try execute_tool_eio ~sw ~clock ?mcp_session_id ?auth_token state ~name ~arguments
    with exn ->
      (* Never let a tool exception crash the MCP server. *)
      let err = Printexc.to_string exn in
      Log.Mcp.error "tools/call crashed: %s" err;
      (false, Printf.sprintf "❌ Internal error: %s" err)
  in
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
let handle_request ~clock ~sw ?mcp_session_id ?auth_token state request_str =
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
                        let result = handle_call_tool_eio ~sw ~clock ?mcp_session_id ?auth_token state id params in
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
