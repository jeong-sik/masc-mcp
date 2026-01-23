(** Room Portal - A2A Protocol Implementation

    Agent-to-Agent (A2A) direct communication channel.
    Extracted from room.ml for modularity.
*)

open Types
open Room_utils

(** Directory paths for portal data *)
let portals_dir config = Filename.concat (masc_dir config) "portals"
let a2a_tasks_dir config = Filename.concat (masc_dir config) "a2a_tasks"

(** Generate A2A task ID *)
let gen_a2a_task_id () =
  let now = Unix.gettimeofday () in
  let tm = Unix.gmtime now in
  Printf.sprintf "a2a-%04d%02d%02d%02d%02d%02d-%04d"
    (tm.Unix.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec
    (Random.int 10000)

(** Open portal - establish bidirectional A2A connection (Result version) *)
let portal_open_r config ~agent_name ~target_agent ~initial_message : string masc_result =
  if not (is_initialized config) then
    Error NotInitialized
  else match validate_agent_name_r agent_name, validate_agent_name_r target_agent with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok _, Ok _ -> begin
    mkdir_p (portals_dir config);
    mkdir_p (a2a_tasks_dir config);
    let portal_path = Filename.concat (portals_dir config) (safe_filename agent_name ^ ".json") in
    with_file_lock config portal_path (fun () ->
      match read_json_opt config portal_path with
      | Some json ->
          (match portal_of_yojson json with
          | Ok p -> Error (PortalAlreadyOpen { agent = agent_name; target = p.portal_target })
          | Error e -> Error (InvalidJson e))
      | None ->
          let now = now_iso () in
          let portal = {
            portal_from = agent_name;
            portal_target = target_agent;
            portal_opened_at = now;
            portal_status = PortalOpen;
            task_count = 0;
          } in
          write_json config portal_path (portal_to_yojson portal);
          (* Create reverse portal for target agent *)
          let target_portal_path = Filename.concat (portals_dir config) (target_agent ^ ".json") in
          if not (Sys.file_exists target_portal_path) then begin
            let reverse_portal = {
              portal_from = target_agent;
              portal_target = agent_name;
              portal_opened_at = now;
              portal_status = PortalOpen;
              task_count = 0;
            } in
            write_json config target_portal_path (portal_to_yojson reverse_portal)
          end;
          (* Send initial message if provided *)
          match initial_message with
          | Some msg ->
              let task_id = gen_a2a_task_id () in
              let task = {
                a2a_id = task_id;
                from_agent = agent_name;
                to_agent = target_agent;
                a2a_message = msg;
                a2a_status = A2APending;
                a2a_result = None;
                created_at = now;
                updated_at = now;
              } in
              let task_path = Filename.concat (a2a_tasks_dir config) (task_id ^ ".json") in
              write_json config task_path (a2a_task_to_yojson task);
              let updated_portal = { portal with task_count = 1 } in
              write_json config portal_path (portal_to_yojson updated_portal);
              Ok (Printf.sprintf "🌀 Portal opened: %s ↔ %s (initial task: %s)" agent_name target_agent task_id)
          | None ->
              Ok (Printf.sprintf "🌀 Portal opened: %s ↔ %s" agent_name target_agent)
    )
  end

(** Backward-compatible wrapper
    @deprecated Use portal_open_r for type-safe error handling *)
let portal_open config ~agent_name ~target_agent ~initial_message =
  match portal_open_r config ~agent_name ~target_agent ~initial_message with
  | Ok msg -> msg
  | Error e -> masc_error_to_string e

(** Send through portal - send message to connected agent (Result version) *)
let portal_send_r config ~agent_name ~message : string masc_result =
  if not (is_initialized config) then
    Error NotInitialized
  else match validate_agent_name_r agent_name with
  | Error e -> Error e
  | Ok _ ->
    let portal_path = Filename.concat (portals_dir config) (safe_filename agent_name ^ ".json") in
    with_file_lock config portal_path (fun () ->
      match read_json_opt config portal_path with
      | None -> Error (PortalNotOpen agent_name)
      | Some json ->
          match portal_of_yojson json with
          | Ok portal when portal.portal_status = PortalOpen ->
              let now = now_iso () in
              let task_id = gen_a2a_task_id () in
              let task = {
                a2a_id = task_id;
                from_agent = agent_name;
                to_agent = portal.portal_target;
                a2a_message = message;
                a2a_status = A2APending;
                a2a_result = None;
                created_at = now;
                updated_at = now;
              } in
              let task_path = Filename.concat (a2a_tasks_dir config) (task_id ^ ".json") in
              write_json config task_path (a2a_task_to_yojson task);
              let updated_portal = { portal with task_count = portal.task_count + 1 } in
              write_json config portal_path (portal_to_yojson updated_portal);
              Ok (Printf.sprintf "📤 Sent to %s (task: %s)" portal.portal_target task_id)
          | Ok _ -> Error (PortalClosed agent_name)
          | Error e -> Error (InvalidJson e)
    )

(** Backward-compatible wrapper
    @deprecated Use portal_send_r for type-safe error handling *)
let portal_send config ~agent_name ~message =
  match portal_send_r config ~agent_name ~message with
  | Ok msg -> msg
  | Error e -> masc_error_to_string e

(** Get portal target agent - returns Some target_name if portal is open *)
let get_portal_target config ~agent_name =
  let portal_path = Filename.concat (portals_dir config) (safe_filename agent_name ^ ".json") in
  if Sys.file_exists portal_path then
    match read_json_opt config portal_path with
    | Some json ->
        (match portal_of_yojson json with
         | Ok portal when portal.portal_status = PortalOpen -> Some portal.portal_target
         | _ -> None)
    | None -> None
  else None

(** Close portal *)
let portal_close config ~agent_name =
  ensure_initialized config;

  let portal_path = Filename.concat (portals_dir config) (safe_filename agent_name ^ ".json") in

  (* Use file lock to prevent race conditions *)
  with_file_lock config portal_path (fun () ->
    match read_json_opt config portal_path with
    | None -> Printf.sprintf "⚠ No portal open for %s" agent_name
    | Some json ->
        match portal_of_yojson json with
        | Ok portal ->
            (* Close this portal *)
            Sys.remove portal_path;

            (* Also close reverse portal *)
            let target_portal_path = Filename.concat (portals_dir config) (portal.portal_target ^ ".json") in
            if Sys.file_exists target_portal_path then
              Sys.remove target_portal_path;

            Printf.sprintf "🌀 Portal closed: %s ↔ %s (%d tasks sent)"
              agent_name portal.portal_target portal.task_count
        | Error _ ->
            Sys.remove portal_path;
            Printf.sprintf "🌀 Portal closed (cleanup)"
  )

(** Get portal status *)
let portal_status config ~agent_name =
  ensure_initialized config;

  let portal_path = Filename.concat (portals_dir config) (safe_filename agent_name ^ ".json") in

  if not (Sys.file_exists portal_path) then
    `Assoc [
      ("status", `String "no_portal");
      ("message", `String (Printf.sprintf "No portal open for %s" agent_name));
    ]
  else begin
    let json = read_json config portal_path in
    match portal_of_yojson json with
    | Ok portal ->
        (* Count pending tasks for this agent *)
        let pending_tasks =
          if Sys.file_exists (a2a_tasks_dir config) then
            Array.fold_left (fun acc f ->
              if Filename.check_suffix f ".json" then begin
                let task_path = Filename.concat (a2a_tasks_dir config) f in
                let tj = read_json config task_path in
                match a2a_task_of_yojson tj with
                | Ok t when t.to_agent = agent_name && t.a2a_status = A2APending -> acc + 1
                | _ -> acc
              end else acc
            ) 0 (Sys.readdir (a2a_tasks_dir config))
          else 0
        in
        `Assoc [
          ("status", `String "open");
          ("portal", portal_to_yojson portal);
          ("pending_tasks", `Int pending_tasks);
        ]
    | Error e ->
        `Assoc [
          ("status", `String "error");
          ("message", `String e);
        ]
  end
