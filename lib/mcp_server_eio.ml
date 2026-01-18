(** MCP Protocol Server Implementation - Eio Native (100% Lwt-Free!)

    Direct-style async MCP server using OCaml 5.x Effect Handlers.
    All Lwt bridges have been eliminated as of 2026-01-11.

    Key adapters for Session.registry compatibility:
    - unregister_sync: Direct hashtable removal without Lwt_mutex
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
let create_state_eio ~sw ~env ~base_path =
  Mcp_server.create_state_eio ~sw ~env ~base_path

let is_jsonrpc_v2 = Mcp_server.is_jsonrpc_v2
let is_jsonrpc_response = Mcp_server.is_jsonrpc_response
let is_notification = Mcp_server.is_notification
let get_id = Mcp_server.get_id
let jsonrpc_request_of_yojson = Mcp_server.jsonrpc_request_of_yojson
let protocol_version_from_params = Mcp_server.protocol_version_from_params
let normalize_protocol_version = Mcp_server.normalize_protocol_version
let make_response = Mcp_server.make_response
let make_error = Mcp_server.make_error

(** Unregister agent synchronously - adapter for Session.registry

    Directly removes from hashtable without Lwt_mutex.
    Safe in Eio single-fiber context.
*)
let unregister_sync (registry : Session.registry) ~agent_name =
  Hashtbl.remove registry.Session.sessions agent_name;
  Log.Session.info "Session unregistered (sync): %s (total: %d)"
    agent_name (Hashtbl.length registry.sessions)

(** Wait for message using Eio sleep - adapter for Session.registry

    Uses existing Session.pop_message but with Eio.Time.sleep for polling.
    This avoids the Lwt bridge while keeping the existing registry structure.
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
            let files = Sys.readdir msgs_path |> Array.to_list |> List.sort compare |> List.rev in
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
          | "messages" | "messages/recent" ->
              let since_seq = Mcp_server.int_query_param uri "since_seq" ~default:0 in
              let limit = Mcp_server.int_query_param uri "limit" ~default:10 in
              ("text/markdown", Some (Room.get_messages config ~since_seq ~limit))
          | "messages.json" | "messages.json/recent" ->
              let since_seq = Mcp_server.int_query_param uri "since_seq" ~default:0 in
              let limit = Mcp_server.int_query_param uri "limit" ~default:10 in
              let json = read_messages_json ~since_seq ~limit in
              ("application/json", Some (Yojson.Safe.pretty_to_string json))
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

(** Execute tool - Eio native version (100% Lwt-free!)

    Direct-style implementation using Eio-native modules:
    - wait_for_message_eio for session listening (Eio.Time.sleep)
    - Metrics_store_eio for metrics recording (pure sync)
    - Planning_eio for planning operations (pure sync)
    - handle_read_resource_eio for resource reading (pure sync)

    All Lwt bridges have been removed! 🎉
*)
let execute_tool_eio ~clock state ~name ~arguments =
  (* clock parameter used for Session_eio.wait_for_message *)
  let open Yojson.Safe.Util in

  let config = state.Mcp_server.room_config in
  let registry = state.Mcp_server.session_registry in  (* TODO: Use session_registry_eio when migrated *)

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

  let agent_name = get_string "agent_name" "unknown" in

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
                         "masc_messages"; "masc_votes"; "masc_vote_status";
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
      let _ = Session.register registry ~agent_name in
      (true, result)

  | "masc_leave" ->
      let result = Room.leave config ~agent_name in
      unregister_sync registry ~agent_name;
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

  | "masc_claim" ->
      let task_id = get_string "task_id" "" in
      result_to_response (Room.claim_task_r config ~agent_name ~task_id)

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

  | "masc_tasks" ->
      (true, Room.list_tasks config)

  | "masc_claim_next" ->
      (true, Room.claim_next config ~agent_name)

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
        (true, result)
      end

  | "masc_messages" ->
      let since_seq = get_int "since_seq" 0 in
      let limit = get_int "limit" 10 in
      (true, Room.get_messages config ~since_seq ~limit)

  | "masc_lock" ->
      let file_path = get_string "file_path" "" in
      result_to_response (Room.lock_file_r config ~agent_name ~file_path)

  | "masc_unlock" ->
      let file_path = get_string "file_path" "" in
      result_to_response (Room.unlock_file_r config ~agent_name ~file_path)

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

  (* Heartbeat & Agent Health tools *)
  | "masc_heartbeat" ->
      (true, Room.heartbeat config ~agent_name)

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

  | "masc_find_by_capability" ->
      let capability = get_string "capability" "" in
      let json = Room.find_agents_by_capability config ~capability in
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
      (match A2a_tools.subscribe ?agent_filter ~events () with
       | Ok json -> (true, Yojson.Safe.pretty_to_string json)
       | Error e -> (false, Printf.sprintf "❌ Subscribe failed: %s" e))

  | "masc_a2a_unsubscribe" ->
      let subscription_id = get_string "subscription_id" "" in
      (match A2a_tools.unsubscribe ~subscription_id with
       | Ok json -> (true, Yojson.Safe.pretty_to_string json)
       | Error e -> (false, Printf.sprintf "❌ Unsubscribe failed: %s" e))

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
      Buffer.add_string buf (Printf.sprintf "  • Task ops: %d/min\n" cfg.task_ops_per_minute);
      Buffer.add_string buf (Printf.sprintf "  • File locks: %d/min\n\n" cfg.file_lock_per_minute);
      Buffer.add_string buf "Role multipliers:\n";
      Buffer.add_string buf (Printf.sprintf "  • Reader: %.1fx\n" cfg.reader_multiplier);
      Buffer.add_string buf (Printf.sprintf "  • Worker: %.1fx\n" cfg.worker_multiplier);
      Buffer.add_string buf (Printf.sprintf "  • Admin: %.1fx\n" cfg.admin_multiplier);
      (true, Buffer.contents buf)

  (* Encryption tools *)
  | "masc_encryption_status" ->
      let status = Encryption.get_status state.Mcp_server.encryption_config in
      let msg = Printf.sprintf "🔐 Encryption Status\n%s"
        (Yojson.Safe.pretty_to_string status) in
      (true, msg)

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
              Error "❌ Invalid mode. Valid: minimal, standard, full, solo, custom"
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
      let result = Spawn.spawn ~agent_name:spawn_agent_name ~prompt ~timeout_seconds ?working_dir () in
      (result.Spawn.success, Spawn.result_to_string result)

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

  | _ ->
      (false, Printf.sprintf "❌ Unknown tool: %s" name)

(** {1 Eio-Native JSON-RPC Handlers} *)

(** Eio-native handler for tools/call - uses execute_tool_eio directly *)
let handle_call_tool_eio ~clock state id params =
  let open Yojson.Safe.Util in
  let name = params |> member "name" |> to_string in
  let arguments = params |> member "arguments" in

  let (success, message) = execute_tool_eio ~clock state ~name ~arguments in

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
  let protocol_version =
    params |> protocol_version_from_params |> normalize_protocol_version
  in
  make_response ~id (`Assoc [
    ("protocolVersion", `String protocol_version);
    ("serverInfo", Mcp_server.server_info);
    ("capabilities", Mcp_server.capabilities);
    ("instructions", `String "MASC (Multi-Agent Streaming Coordination) enables AI agent collaboration. \
      ROOM: Agents sharing the same base path (.masc/ folder) or Redis cluster coordinate together. \
      CLUSTER: Set MASC_CLUSTER_NAME for multi-machine coordination (defaults to basename of ME_ROOT). \
      WORKFLOW: masc_status → masc_claim (task) → masc_worktree_create (isolation) → work → masc_done. \
      Use @agent mentions in masc_broadcast for cross-agent communication. \
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
    Uses execute_tool_eio for tool calls, eliminating Lwt bridge for tools.
*)
let handle_request ~clock ~sw:_ state request_str =
  try
    let json =
      try Ok (Yojson.Safe.from_string request_str)
      with exn -> Error (Printexc.to_string exn)
    in
    match json with
    | Error msg ->
        make_error ~id:`Null (-32700) ("Parse error: " ^ msg)
    | Ok json ->
        if is_jsonrpc_response json then
          `Null
        else if not (is_jsonrpc_v2 json) then
          make_error ~id:`Null (-32600) "Invalid Request: jsonrpc must be 2.0"
        else
          match jsonrpc_request_of_yojson json with
          | Error msg -> make_error ~id:`Null (-32600) ("Invalid Request: " ^ msg)
          | Ok req ->
              let id = get_id req in
              if is_notification req then
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
                    | Some params -> handle_call_tool_eio ~clock state id params
                    | None -> make_error ~id (-32602) "Missing params")
                | method_ -> make_error ~id (-32601) ("Method not found: " ^ method_))
  with exn ->
    make_error ~id:`Null (-32603) ("Internal error: " ^ Printexc.to_string exn)

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
        (* Handle request with Eio clock *)
        let response = handle_request ~clock ~sw state request_str in

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
