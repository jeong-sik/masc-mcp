(** MASC Room - Core coordination logic *)

open Types

(* Include all utilities from Room_utils *)
include Room_utils

(** Read room state *)
let read_state config =
  let json = read_json config (state_path config) in
  match room_state_of_yojson json with
  | Ok state -> state
  | Error _ -> {
      protocol_version = "0.1.0";
      project = Filename.basename config.base_path;
      started_at = now_iso ();
      message_seq = 0;
      active_agents = [];
      paused = false;
      pause_reason = None;
      paused_by = None;
      paused_at = None;
    }

(** Write room state *)
let write_state config state =
  write_json config (state_path config) (room_state_to_yojson state)

(** Update state with function *)
let update_state config f =
  let state = read_state config in
  let new_state = f state in
  write_state config new_state;
  new_state

(** Get next message sequence *)
let next_seq config =
  let state = update_state config (fun s -> { s with message_seq = s.message_seq + 1 }) in
  state.message_seq

(** Check if room is paused - placed before broadcast since it doesn't need broadcast *)
let is_paused config =
  let state = read_state config in
  state.paused

(** Get pause info *)
let pause_info config =
  let state = read_state config in
  if state.paused then
    Some (state.paused_by, state.pause_reason, state.paused_at)
  else
    None

(** Read backlog *)
let read_backlog config =
  let json = read_json config (backlog_path config) in
  match backlog_of_yojson json with
  | Ok backlog -> backlog
  | Error _ -> { tasks = []; last_updated = now_iso (); version = 1 }

(** Write backlog *)
let write_backlog config backlog =
  write_json config (backlog_path config) (backlog_to_yojson backlog)

(** Parse task id like "task-001" -> 1 *)
let task_id_to_int id =
  let prefix = "task-" in
  let prefix_len = String.length prefix in
  if String.length id <= prefix_len then None
  else if String.sub id 0 prefix_len <> prefix then None
  else int_of_string_opt (String.sub id prefix_len (String.length id - prefix_len))

(** Read archived task ids for collision-free task numbering *)
let read_archive_task_ids config =
  if not (Sys.file_exists (archive_path config)) then []
  else
    let open Yojson.Safe.Util in
    let json = read_json config (archive_path config) in
    let tasks =
      match json with
      | `List tasks -> tasks
      | `Assoc _ -> begin
          match json |> member "tasks" with
          | `List tasks -> tasks
          | _ -> []
        end
      | _ -> []
    in
    List.filter_map (fun task ->
      match task |> member "id" |> to_string_option with
      | Some id -> task_id_to_int id
      | None -> None
    ) tasks

(** Append tasks to archive file (tasks-archive.json) *)
let append_archive_tasks config (tasks : task list) =
  if tasks = [] then ()
  else begin
    let open Yojson.Safe.Util in
    let path = archive_path config in
    let existing = read_json config path in
    let existing_tasks =
      match existing with
      | `List items -> items
      | `Assoc _ -> begin
          match existing |> member "tasks" with
          | `List items -> items
          | _ -> []
        end
      | _ -> []
    in
    let new_tasks = List.map task_to_yojson tasks in
    (* Deduplicate by task id, preserving first occurrence *)
    let seen = Hashtbl.create 64 in
    let dedup = List.filter (fun json ->
      match json |> member "id" |> to_string_option with
      | Some id ->
          if Hashtbl.mem seen id then false
          else (Hashtbl.add seen id (); true)
      | None -> false
    ) (existing_tasks @ new_tasks)
    in
    let archive_json = `Assoc [
      ("tasks", `List dedup);
      ("last_updated", `String (now_iso ()));
    ] in
    write_json config path archive_json
  end

(** Calculate next task id using backlog + archive to avoid reuse *)
let next_task_number config backlog =
  let backlog_ids = List.filter_map (fun task -> task_id_to_int task.id) backlog.tasks in
  let archive_ids = read_archive_task_ids config in
  let max_id = List.fold_left max 0 (backlog_ids @ archive_ids) in
  max_id + 1

(** Generate short session ID *)
let generate_session_id () =
  Printf.sprintf "%04x%04x" (Random.int 0xFFFF) (Random.int 0xFFFF)

(** Get hostname *)
let get_hostname () =
  try Some (Unix.gethostname ()) with _ -> None

(** Get current TTY - uses TTY environment variable or /dev/tty check *)
let get_tty () =
  try
    match Sys.getenv_opt "TTY" with
    | Some tty -> Some tty
    | None ->
        (* Try to read from /dev/tty symlink *)
        let ic = Unix.open_process_in "tty 2>/dev/null" in
        let result = try Some (input_line ic) with _ -> None in
        ignore (Unix.close_process_in ic);
        result
  with _ -> None

(** Resolve agent name - supports both exact nickname and agent_type prefix match.
    Returns the actual agent name (nickname) if found, otherwise original name. *)
let resolve_agent_name config agent_name =
  let exact_file = Filename.concat (agents_dir config) (safe_filename agent_name ^ ".json") in
  if Sys.file_exists exact_file then
    agent_name
  else begin
    (* Try to find agent by type prefix (e.g., "gemini" matches "gemini-swift-fox") *)
    let dir = agents_dir config in
    if Sys.file_exists dir then
      let files = Sys.readdir dir in
      let prefix = agent_name ^ "-" in
      match Array.find_opt (fun f ->
        String.length f > String.length prefix &&
        String.sub f 0 (String.length prefix) = prefix
      ) files with
      | Some file -> String.sub file 0 (String.length file - 5) (* remove .json *)
      | None -> agent_name
    else
      agent_name
  end

(** Initialize MASC room *)
let rec init config ~agent_name =
  if is_initialized config then
    "MASC already initialized."
  else begin
    (* Create directories *)
    List.iter mkdir_p [
      agents_dir config;
      tasks_dir config;
      messages_dir config;
    ];

    (* Create initial state *)
    let state = {
      protocol_version = "0.1.0";
      project = Filename.basename config.base_path;
      started_at = now_iso ();
      message_seq = 0;
      active_agents = [];
      paused = false;
      pause_reason = None;
      paused_by = None;
      paused_at = None;
    } in
    write_state config state;

    (* Create empty backlog *)
    let backlog = { tasks = []; last_updated = now_iso (); version = 1 } in
    write_backlog config backlog;

    let result = "✅ MASC room created!" in

    (* Auto-join if agent specified *)
    match agent_name with
    | Some name -> result ^ "\n" ^ (join config ~agent_name:name ~capabilities:[] ())
    | None -> result
  end

(** Join room - now with auto-generated nickname and metadata *)
and join config ~agent_name ?(agent_type_override=None) ~capabilities
    ?(pid=None) ?(hostname=None) ?(tty=None) ?(worktree=None) ?(parent_task=None) () =
  ensure_initialized config;

  (* Determine if this is a legacy call (agent_name = type) or new style *)
  let agent_type = match agent_type_override with
    | Some t -> t
    | None ->
        (* Check if agent_name looks like a nickname (has dashes) *)
        if Nickname.is_generated_nickname agent_name then
          Option.value (Nickname.extract_agent_type agent_name) ~default:agent_name
        else
          agent_name  (* Legacy: agent_name is the type *)
  in

  (* Generate unique nickname if agent_name is just a type *)
  let nickname =
    if Nickname.is_generated_nickname agent_name then
      agent_name  (* Already a nickname, use as-is *)
    else
      Nickname.generate agent_type  (* Generate new nickname *)
  in

  (* Collect metadata *)
  let session_id = generate_session_id () in
  let meta : agent_meta = {
    session_id;
    agent_type;
    pid;
    hostname = (match hostname with Some h -> Some h | None -> get_hostname ());
    tty = (match tty with Some t -> Some t | None -> get_tty ());
    worktree;
    parent_task;
  } in

  let agent_file = Filename.concat (agents_dir config) (safe_filename nickname ^ ".json") in
  let agent = {
    name = nickname;
    agent_type;
    status = Active;
    capabilities;
    current_task = None;
    joined_at = now_iso ();
    last_seen = now_iso ();
    meta = Some meta;
  } in
  write_json config agent_file (agent_to_yojson agent);

  (* Update state *)
  let _ = update_state config (fun s ->
    let agents = nickname :: (List.filter ((<>) nickname) s.active_agents) in
    { s with active_agents = agents }
  ) in

  (* Broadcast join *)
  let _ = broadcast config ~from_agent:nickname ~content:(Printf.sprintf "👋 %s joined the room" nickname) in

  (* Log event with metadata *)
  log_event config (Printf.sprintf
    "{\"type\":\"agent_join\",\"agent\":\"%s\",\"agent_type\":\"%s\",\"session_id\":\"%s\",\"capabilities\":%s,\"ts\":\"%s\"}"
    nickname
    agent_type
    session_id
    (Yojson.Safe.to_string (`List (List.map (fun s -> `String s) capabilities)))
    (now_iso ()));

  Printf.sprintf {|✅ %s joined the room!

🎫 **Your Identity**
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Nickname: %s
  Type: %s
  Session: %s
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📋 **MASC 협업 가이드 (필독!)**
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

🔴 **필수 행동**:
1. 작업 시작/완료 시 브로드캐스트
2. @mention 받으면 응답
3. 긴 작업 중 진행률 공유

💡 **협업 패턴**:
• 도움 요청: "@에이전트 이거 도와줘"
• 리뷰 요청: "@에이전트 검토 부탁"
• 완료 보고: "✅ [작업] 완료!"
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
|} nickname nickname agent_type session_id

(** Leave room *)
and leave config ~agent_name =
  ensure_initialized config;

  (* Support both exact nickname match and agent_type prefix match *)
  let actual_name = resolve_agent_name config agent_name in

  let agent_file = Filename.concat (agents_dir config) (safe_filename actual_name ^ ".json") in
  if Sys.file_exists agent_file then begin
    Sys.remove agent_file;

    let _ = update_state config (fun s ->
      { s with active_agents = List.filter ((<>) actual_name) s.active_agents }
    ) in

    let _ = broadcast config ~from_agent:"system" ~content:(Printf.sprintf "👋 %s left the room" actual_name) in

    (* Log event *)
    log_event config (Printf.sprintf
      "{\"type\":\"agent_leave\",\"agent\":\"%s\",\"ts\":\"%s\"}"
      actual_name (now_iso ()));

    Printf.sprintf "✅ %s left the room" actual_name
  end else
    Printf.sprintf "⚠ %s was not in the room" agent_name

(** Broadcast message *)
and broadcast config ~from_agent ~content =
  ensure_initialized config;

  let seq = next_seq config in

  (* Use Mention module for consistent parsing (Stateless/Stateful/Broadcast) *)
  let mention = Mention.extract content in

  (* Sanitize content to prevent XSS attacks *)
  let safe_content = sanitize_message content in
  let safe_agent = sanitize_agent_name from_agent in

  let msg = {
    seq;
    from_agent = safe_agent;
    msg_type = "broadcast";
    content = safe_content;
    mention;
    timestamp = now_iso ();
  } in

  let msg_file = Filename.concat (messages_dir config)
    (Printf.sprintf "%09d_%s_broadcast.json" seq (safe_filename from_agent)) in
  write_json config msg_file (message_to_yojson msg);

  (* Publish to distributed backend if available *)
  let _ = backend_publish config ~channel:"broadcast" ~message:(Yojson.Safe.to_string (message_to_yojson msg)) in

  Printf.sprintf "📢 [%s] %s" safe_agent safe_content

(** Pause the room - stops orchestrator from spawning new agents *)
let pause config ~by ~reason =
  let _ = update_state config (fun s -> {
    s with
    paused = true;
    pause_reason = Some reason;
    paused_by = Some by;
    paused_at = Some (now_iso ());
  }) in
  (* Broadcast pause notification *)
  let _ = broadcast config ~from_agent:"system"
    ~content:(Printf.sprintf "⏸️ Room PAUSED by %s: %s" by reason) in
  ()

(** Resume the room *)
let resume config ~by =
  let state = read_state config in
  if not state.paused then
    `Already_running
  else begin
    let _ = update_state config (fun s -> {
      s with
      paused = false;
      pause_reason = None;
      paused_by = None;
      paused_at = None;
    }) in
    (* Broadcast resume notification *)
    let _ = broadcast config ~from_agent:"system"
      ~content:(Printf.sprintf "▶️ Room RESUMED by %s" by) in
    `Resumed
  end

(** Reset room - delete .masc/ folder *)
let reset config =
  if not (is_initialized config) then
    "⚠ MASC not initialized. Nothing to reset."
  else begin
    (* Recursive delete *)
    let rec rm_rf path =
      if Sys.is_directory path then begin
        Sys.readdir path |> Array.iter (fun name ->
          rm_rf (Filename.concat path name)
        );
        Unix.rmdir path
      end else
        Sys.remove path
    in
    rm_rf (masc_dir config);
    Printf.sprintf "🗑️ MASC room reset! (.masc/ deleted at %s)" config.base_path
  end

(* ============================================ *)
(* Zombie Detection (moved up for status use)  *)
(* ============================================ *)

(** Default heartbeat timeout in seconds (5 minutes) - DEPRECATED: Use Resilience.default_zombie_threshold *)
let heartbeat_timeout_seconds = Resilience.default_zombie_threshold

(** Parse ISO timestamp to Unix time - returns None if parsing fails *)
let parse_iso_time_opt = Resilience.Time.parse_iso8601_opt

(** Parse ISO timestamp - returns current time if parsing fails (safe default) *)
let parse_iso_time iso_str =
  match parse_iso_time_opt iso_str with
  | Some t -> t
  | None -> Resilience.Time.now ()

(** Check if agent is zombie (no heartbeat for timeout period) *)
let is_zombie_agent last_seen_iso =
  Resilience.Zombie.is_zombie last_seen_iso

(** Get room status *)
let status config =
  ensure_initialized config;

  let state = read_state config in
  let backlog = read_backlog config in
  (* Read current room inline - can't use read_current_room due to definition order *)
  let current_room =
    let path = Filename.concat config.base_path "current_room" in
    if Sys.file_exists path then
      try
        let ic = open_in path in
        let room_id = input_line ic in
        close_in ic;
        String.trim room_id
      with _ -> "default"
    else
      "default"
  in

  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf "🏢 Cluster: %s\n" state.project);
  Buffer.add_string buf (Printf.sprintf "📍 Room: %s\n" current_room);
  Buffer.add_string buf (Printf.sprintf "📁 Path: %s\n" config.base_path);
  Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n";
  Buffer.add_string buf "📌 Players:\n";

  (* List agents *)
  let agents_path = agents_dir config in
  if Sys.file_exists agents_path then begin
    Sys.readdir agents_path |> Array.iter (fun name ->
      if Filename.check_suffix name ".json" then
        let path = Filename.concat agents_path name in
        let json = read_json config path in
        match agent_of_yojson json with
        | Ok agent ->
            (* Check zombie status first - overrides normal status *)
            let is_zombie = is_zombie_agent agent.last_seen in
            let icon = if is_zombie then "💀"
              else match agent.status with
                | Busy -> "🔴"
                | Active -> "🟢"
                | Listening -> "🎧"
                | Inactive -> "⚫"
            in
            let task = if is_zombie then "zombie"
              else Option.value agent.current_task ~default:"idle"
            in
            Buffer.add_string buf (Printf.sprintf "  %s %s → %s\n" icon agent.name task)
        | Error _ -> ()
    )
  end;

  Buffer.add_string buf "\n📋 Quest Board:\n";

  (* List tasks *)
  List.iter (fun task ->
    let status_icon = match task.task_status with
      | Done _ -> "✅"
      | Claimed _ | InProgress _ -> "🔄"
      | Todo -> "📋"
      | Cancelled _ -> "🚫"
    in
    let assignee = match task.task_status with
      | Claimed { assignee; _ } | InProgress { assignee; _ } | Done { assignee; _ } -> assignee
      | Cancelled { cancelled_by; _ } -> cancelled_by
      | Todo -> "unclaimed"
    in
    Buffer.add_string buf (Printf.sprintf "  %s %s: %s (%s)\n" status_icon task.id task.title assignee)
  ) backlog.tasks;

  if backlog.tasks = [] then
    Buffer.add_string buf "  (no tasks)\n";

  (* Message summary - count only to save tokens *)
  let msgs_path = messages_dir config in
  if Sys.file_exists msgs_path then begin
    let files = Sys.readdir msgs_path |> Array.to_list in
    let total = List.length files in
    if total > 0 then begin
      (* Count by agent from filename pattern: {seq}_{agent}_broadcast.json *)
      let agent_counts = Hashtbl.create 8 in
      List.iter (fun name ->
        (* Extract agent name from filename *)
        let parts = String.split_on_char '_' name in
        if List.length parts >= 2 then begin
          let agent = List.nth parts 1 in
          let current = try Hashtbl.find agent_counts agent with Not_found -> 0 in
          Hashtbl.replace agent_counts agent (current + 1)
        end
      ) files;
      (* Format agent counts *)
      let counts_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) agent_counts [] in
      let counts_str = String.concat ", " (List.map (fun (a, c) -> Printf.sprintf "%s:%d" a c) counts_list) in
      Buffer.add_string buf (Printf.sprintf "\n💬 Messages: %d (%s)\n" total counts_str);
      Buffer.add_string buf "   Use masc_messages for details\n"
    end else
      Buffer.add_string buf "\n💬 Messages: 0\n"
  end else
    Buffer.add_string buf "\n💬 Messages: 0\n";

  Buffer.contents buf

(** Add task *)
let add_task config ~title ~priority ~description =
  ensure_initialized config;

  let backlog = read_backlog config in
  let task_id = Printf.sprintf "task-%03d" (next_task_number config backlog) in

  let new_task = {
    id = task_id;
    title;
    description;
    task_status = Todo;
    priority;
    files = [];
    created_at = now_iso ();
    worktree = None;  (* Linked when worktree is created *)
  } in

  let new_backlog = {
    tasks = backlog.tasks @ [new_task];
    last_updated = now_iso ();
    version = backlog.version + 1;
  } in
  write_backlog config new_backlog;

  let _ = broadcast config ~from_agent:"system" ~content:(Printf.sprintf "📋 New quest: %s" title) in
  Printf.sprintf "✅ Added %s: %s" task_id title

(** Add multiple tasks in a batch *)
let batch_add_tasks config tasks =
  ensure_initialized config;

  let lock_file = Filename.concat (tasks_dir config) ".backlog.lock" in
  mkdir_p (Filename.dirname lock_file);

  let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
  Unix.lockf fd Unix.F_LOCK 0;

  let result =
    try
      let backlog = read_backlog config in
      let next_num = ref (next_task_number config backlog) in
      let added_tasks = List.map (fun (title, priority, description) ->
        let task_id = Printf.sprintf "task-%03d" !next_num in
        incr next_num;
        {
          id = task_id;
          title;
          description;
          task_status = Todo;
          priority;
          files = [];
          created_at = now_iso ();
          worktree = None;
        }
      ) tasks in

      let new_backlog = {
        tasks = backlog.tasks @ added_tasks;
        last_updated = now_iso ();
        version = backlog.version + 1;
      } in
      write_backlog config new_backlog;

      let summary = String.concat ", " (List.map (fun (t : Types.task) -> t.id) added_tasks) in
      let msg = Printf.sprintf "📋 New batch of %d quests added: %s" (List.length added_tasks) summary in
      let _ = broadcast config ~from_agent:"system" ~content:msg in
      Printf.sprintf "✅ Added %d tasks: %s" (List.length added_tasks) summary
    with e ->
      Printf.sprintf "❌ Error adding batch tasks: %s" (Printexc.to_string e)
  in

  Unix.lockf fd Unix.F_ULOCK 0;
  Unix.close fd;
  result

(** Claim task with file locking (TOCTOU prevention) *)
let claim_task config ~agent_name ~task_id =
  ensure_initialized config;

  (* Validate inputs *)
  match validate_agent_name agent_name, validate_task_id task_id with
  | Error e, _ -> Printf.sprintf "❌ %s" e
  | _, Error e -> Printf.sprintf "❌ %s" e
  | Ok _, Ok _ ->

  let lock_file = Filename.concat (tasks_dir config) ".backlog.lock" in
  mkdir_p (Filename.dirname lock_file);

  (* File-based lock using flock *)
  let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
  Unix.lockf fd Unix.F_LOCK 0;

  let result =
    try
      let backlog = read_backlog config in
      let found = ref false in
      let already_claimed = ref None in

      let new_tasks = List.map (fun task ->
        if task.id = task_id then begin
          found := true;
          match task.task_status with
          | Todo ->
              { task with task_status = Claimed { assignee = agent_name; claimed_at = now_iso () } }
          | Claimed { assignee; _ } | InProgress { assignee; _ } | Done { assignee; _ } | Cancelled { cancelled_by = assignee; _ } ->
              already_claimed := Some assignee;
              task
        end else task
      ) backlog.tasks in

      if not !found then
        Printf.sprintf "❌ Task %s not found" task_id
      else match !already_claimed with
        | Some other -> Printf.sprintf "⚠ Task %s is already claimed by %s" task_id other
        | None ->
            let new_backlog = {
              tasks = new_tasks;
              last_updated = now_iso ();
              version = backlog.version + 1;
            } in
            write_backlog config new_backlog;

            (* Update agent status *)
            let agent_file = Filename.concat (agents_dir config) (safe_filename agent_name ^ ".json") in
            if Sys.file_exists agent_file then begin
              let json = read_json config agent_file in
              match agent_of_yojson json with
              | Ok agent ->
                  let updated = { agent with status = Busy; current_task = Some task_id } in
                  write_json config agent_file (agent_to_yojson updated)
              | Error _ -> ()
            end;

            let _ = broadcast config ~from_agent:agent_name ~content:(Printf.sprintf "📋 Claimed %s" task_id) in

            (* Log event *)
            log_event config (Printf.sprintf
              "{\"type\":\"task_claim\",\"agent\":\"%s\",\"task\":\"%s\",\"ts\":\"%s\"}"
              agent_name task_id (now_iso ()));

            Printf.sprintf "✅ %s claimed %s" agent_name task_id
    with e ->
      Printf.sprintf "❌ Error: %s" (Printexc.to_string e)
  in

  Unix.lockf fd Unix.F_ULOCK 0;
  Unix.close fd;
  result

(** Result-returning version of claim_task for type-safe error handling *)
let claim_task_r config ~agent_name ~task_id : string Types.masc_result =
  if not (is_initialized config) then Error Types.NotInitialized
  else match validate_agent_name_r agent_name, validate_task_id_r task_id with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok _, Ok _ ->
    let lock_file = Filename.concat (tasks_dir config) ".backlog.lock" in
    mkdir_p (Filename.dirname lock_file);
    let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
    Unix.lockf fd Unix.F_LOCK 0;
    let result =
      try
        let backlog = read_backlog config in
        let found = ref false in
        let already_claimed = ref None in
        let new_tasks = List.map (fun task ->
          if task.id = task_id then begin
            found := true;
            match task.task_status with
            | Todo -> { task with task_status = Claimed { assignee = agent_name; claimed_at = now_iso () } }
            | Claimed { assignee; _ } | InProgress { assignee; _ } | Done { assignee; _ } | Cancelled { cancelled_by = assignee; _ } ->
                already_claimed := Some assignee; task
          end else task
        ) backlog.tasks in
        if not !found then Error (Types.TaskNotFound task_id)
        else match !already_claimed with
          | Some other -> Error (Types.TaskAlreadyClaimed { task_id; by = other })
          | None ->
              let new_backlog = {
                tasks = new_tasks;
                last_updated = now_iso ();
                version = backlog.version + 1;
              } in
              write_backlog config new_backlog;
              let agent_file = Filename.concat (agents_dir config) (safe_filename agent_name ^ ".json") in
              if Sys.file_exists agent_file then begin
                let json = read_json config agent_file in
                match agent_of_yojson json with
                | Ok agent ->
                    let updated = { agent with status = Busy; current_task = Some task_id } in
                    write_json config agent_file (agent_to_yojson updated)
                | Error _ -> ()
              end;
              let _ = broadcast config ~from_agent:agent_name ~content:(Printf.sprintf "📋 Claimed %s" task_id) in
              log_event config (Printf.sprintf "{\"type\":\"task_claim\",\"agent\":\"%s\",\"task\":\"%s\",\"ts\":\"%s\"}" agent_name task_id (now_iso ()));
              Ok (Printf.sprintf "✅ %s claimed %s" agent_name task_id)
      with e -> Error (Types.IoError (Printexc.to_string e))
    in
    Unix.lockf fd Unix.F_ULOCK 0;
    Unix.close fd;
    result

(** Unified task transition (single entrypoint) *)
let transition_task_r config ~agent_name ~task_id ~action
    ?expected_version ?(notes="") ?(reason="") () : string Types.masc_result =
  if not (is_initialized config) then Error Types.NotInitialized
  else match validate_agent_name_r agent_name, validate_task_id_r task_id with
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Ok _, Ok _ ->
        let lock_file = Filename.concat (tasks_dir config) ".backlog.lock" in
        mkdir_p (Filename.dirname lock_file);
        let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
        Unix.lockf fd Unix.F_LOCK 0;
        let result =
          try
            let backlog = read_backlog config in
            (match expected_version with
             | Some v when backlog.version <> v ->
                 Error (Types.TaskInvalidState
                   (Printf.sprintf "Version mismatch (expected %d, got %d)" v backlog.version))
             | _ ->
                 let task_opt = List.find_opt (fun t -> t.id = task_id) backlog.tasks in
                 match task_opt with
                 | None -> Error (Types.TaskNotFound task_id)
                 | Some task ->
                     let now = now_iso () in
                     let action = String.lowercase_ascii action in
                     let status_to_string = function
                       | Types.Todo -> "todo"
                       | Types.Claimed _ -> "claimed"
                       | Types.InProgress _ -> "in_progress"
                       | Types.Done _ -> "done"
                       | Types.Cancelled _ -> "cancelled"
                     in
                     let transition =
                       match action, task.task_status with
                       | "claim", Types.Todo ->
                           Ok (Types.Claimed { assignee = agent_name; claimed_at = now }, Some task_id)
                       | "start", Types.Claimed { assignee; _ } when assignee = agent_name ->
                           Ok (Types.InProgress { assignee = agent_name; started_at = now }, Some task_id)
                       | "done", Types.Claimed { assignee; _ }
                       | "done", Types.InProgress { assignee; _ } when assignee = agent_name ->
                           Ok (Types.Done {
                             assignee = agent_name;
                             completed_at = now;
                             notes = if notes = "" then None else Some notes;
                           }, None)
                       | "cancel", Types.Todo ->
                           Ok (Types.Cancelled {
                             cancelled_by = agent_name;
                             cancelled_at = now;
                             reason = if reason = "" then None else Some reason;
                           }, None)
                       | "cancel", Types.Claimed { assignee; _ }
                       | "cancel", Types.InProgress { assignee; _ } when assignee = agent_name ->
                           Ok (Types.Cancelled {
                             cancelled_by = agent_name;
                             cancelled_at = now;
                             reason = if reason = "" then None else Some reason;
                           }, None)
                       | "release", Types.Claimed { assignee; _ }
                       | "release", Types.InProgress { assignee; _ } when assignee = agent_name ->
                           Ok (Types.Todo, None)
                       | _ ->
                           Error (Types.TaskInvalidState
                             (Printf.sprintf "Invalid transition: %s -> %s (%s)"
                               (status_to_string task.task_status) action task_id))
                     in
                     (match transition with
                      | Error e -> Error e
                      | Ok (new_status, set_current) ->
                          let new_tasks = List.map (fun t ->
                            if t.id = task_id then { t with task_status = new_status } else t
                          ) backlog.tasks in
                          let new_backlog = {
                            tasks = new_tasks;
                            last_updated = now_iso ();
                            version = backlog.version + 1;
                          } in
                          write_backlog config new_backlog;
                          let agent_file = Filename.concat (agents_dir config) (safe_filename agent_name ^ ".json") in
                          if Sys.file_exists agent_file then begin
                            let json = read_json config agent_file in
                            match agent_of_yojson json with
                            | Ok agent ->
                                let updated =
                                  match set_current with
                                  | Some _ -> { agent with status = Busy; current_task = Some task_id }
                                  | None ->
                                      if agent.current_task = Some task_id then
                                        { agent with status = Active; current_task = None }
                                      else
                                        agent
                                in
                                write_json config agent_file (agent_to_yojson updated)
                            | Error _ -> ()
                          end;
                          log_event config (Printf.sprintf
                            "{\"type\":\"task_transition\",\"agent\":\"%s\",\"task\":\"%s\",\"action\":\"%s\",\"from\":\"%s\",\"to\":\"%s\",\"ts\":\"%s\"}"
                            agent_name task_id action
                            (status_to_string task.task_status)
                            (status_to_string new_status)
                            now);
                          Ok (Printf.sprintf "✅ %s %s → %s" task_id
                                (status_to_string task.task_status)
                                (status_to_string new_status))
                     ))
          with e -> Error (Types.IoError (Printexc.to_string e))
        in
        Unix.lockf fd Unix.F_ULOCK 0;
        Unix.close fd;
        result

(** Release task back to backlog - transition wrapper *)
let release_task_r config ~agent_name ~task_id ?expected_version () : string Types.masc_result =
  transition_task_r config ~agent_name ~task_id ~action:"release" ?expected_version ()

(** Complete task with file locking *)
let complete_task config ~agent_name ~task_id ~notes =
  ensure_initialized config;

  let lock_file = Filename.concat (tasks_dir config) ".backlog.lock" in
  mkdir_p (Filename.dirname lock_file);

  (* File-based lock using flock - same as claim_task *)
  let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
  Unix.lockf fd Unix.F_LOCK 0;

  let result =
    try
      let backlog = read_backlog config in

  (* Find the task and check ownership *)
  let task_opt = List.find_opt (fun t -> t.id = task_id) backlog.tasks in

  match task_opt with
  | None ->
      Printf.sprintf "❌ Task %s not found" task_id
  | Some task ->
      (* Verify the task is claimed/in-progress by this agent *)
      let can_complete = match task.task_status with
        | Claimed { assignee; _ } -> assignee = agent_name
        | InProgress { assignee; _ } -> assignee = agent_name
        | Todo -> false  (* Cannot complete unclaimed task *)
        | Done _ | Cancelled _ -> false  (* Already done or cancelled *)
      in

      if not can_complete then
        Printf.sprintf "⚠ Task %s is not claimed by %s. Claim it first!" task_id agent_name
      else begin
        let new_tasks = List.map (fun t ->
          if t.id = task_id then
            { t with task_status = Done {
                assignee = agent_name;
                completed_at = now_iso ();
                notes = if notes = "" then None else Some notes
              }
            }
          else t
        ) backlog.tasks in

        let new_backlog = {
          tasks = new_tasks;
          last_updated = now_iso ();
          version = backlog.version + 1;
        } in
        write_backlog config new_backlog;

        (* Update agent status *)
        let agent_file = Filename.concat (agents_dir config) (safe_filename agent_name ^ ".json") in
        if Sys.file_exists agent_file then begin
          let json = read_json config agent_file in
          match agent_of_yojson json with
          | Ok agent ->
              let updated = { agent with status = Active; current_task = None } in
              write_json config agent_file (agent_to_yojson updated)
          | Error _ -> ()
        end;

        let msg = if notes = "" then Printf.sprintf "✅ Completed %s" task_id
                  else Printf.sprintf "✅ Completed %s - %s" task_id notes in
        let _ = broadcast config ~from_agent:agent_name ~content:msg in

        (* Log event *)
        log_event config (Printf.sprintf
          "{\"type\":\"task_done\",\"agent\":\"%s\",\"task\":\"%s\",\"notes\":%s,\"ts\":\"%s\"}"
          agent_name task_id
          (if notes = "" then "null" else Printf.sprintf "\"%s\"" notes)
          (now_iso ()));

        Printf.sprintf "✅ %s completed %s" agent_name task_id
      end
    with e ->
      Printf.sprintf "❌ Error: %s" (Printexc.to_string e)
  in

  Unix.lockf fd Unix.F_ULOCK 0;
  Unix.close fd;
  result

(** Result-returning version of complete_task for type-safe error handling *)
let complete_task_r config ~agent_name ~task_id ~notes : string Types.masc_result =
  if not (is_initialized config) then Error Types.NotInitialized
  else
    let lock_file = Filename.concat (tasks_dir config) ".backlog.lock" in
    mkdir_p (Filename.dirname lock_file);
    let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
    Unix.lockf fd Unix.F_LOCK 0;
    let result =
      try
        let backlog = read_backlog config in
        let task_opt = List.find_opt (fun t -> t.id = task_id) backlog.tasks in
        match task_opt with
        | None -> Error (Types.TaskNotFound task_id)
        | Some task ->
            let can_complete = match task.task_status with
              | Claimed { assignee; _ } | InProgress { assignee; _ } -> assignee = agent_name
              | Todo | Done _ | Cancelled _ -> false
            in
            if not can_complete then Error (Types.TaskNotClaimed task_id)
            else begin
              let new_tasks = List.map (fun t ->
                if t.id = task_id then
                  { t with task_status = Done { assignee = agent_name; completed_at = now_iso (); notes = if notes = "" then None else Some notes } }
                else t
              ) backlog.tasks in
              let new_backlog = {
                tasks = new_tasks;
                last_updated = now_iso ();
                version = backlog.version + 1;
              } in
              write_backlog config new_backlog;
              let agent_file = Filename.concat (agents_dir config) (safe_filename agent_name ^ ".json") in
              if Sys.file_exists agent_file then begin
                let json = read_json config agent_file in
                match agent_of_yojson json with
                | Ok agent -> let updated = { agent with status = Active; current_task = None } in write_json config agent_file (agent_to_yojson updated)
                | Error _ -> ()
              end;
              let msg = if notes = "" then Printf.sprintf "✅ Completed %s" task_id else Printf.sprintf "✅ Completed %s - %s" task_id notes in
              let _ = broadcast config ~from_agent:agent_name ~content:msg in
              log_event config (Printf.sprintf "{\"type\":\"task_done\",\"agent\":\"%s\",\"task\":\"%s\",\"notes\":%s,\"ts\":\"%s\"}" agent_name task_id (if notes = "" then "null" else Printf.sprintf "\"%s\"" notes) (now_iso ()));
              Ok (Printf.sprintf "✅ %s completed %s" agent_name task_id)
            end
      with e -> Error (Types.IoError (Printexc.to_string e))
    in
    Unix.lockf fd Unix.F_ULOCK 0;
    Unix.close fd;
    result

(** Cancel a task - A2A compatible *)
let cancel_task_r config ~agent_name ~task_id ~reason : string Types.masc_result =
  if not (is_initialized config) then Error Types.NotInitialized
  else
    let lock_file = Filename.concat (tasks_dir config) ".backlog.lock" in
    mkdir_p (Filename.dirname lock_file);
    let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
    Unix.lockf fd Unix.F_LOCK 0;
    let result =
      try
        let backlog = read_backlog config in
        let task_opt = List.find_opt (fun t -> t.id = task_id) backlog.tasks in
        match task_opt with
        | None -> Error (Types.TaskNotFound task_id)
        | Some task ->
            (* Can cancel if: Todo, Claimed by me, or InProgress by me *)
            let can_cancel = match task.task_status with
              | Types.Todo -> true
              | Types.Claimed { assignee; _ } | Types.InProgress { assignee; _ } -> assignee = agent_name
              | Types.Done _ | Types.Cancelled _ -> false
            in
            if not can_cancel then
              Error (Types.TaskInvalidState (Printf.sprintf "Cannot cancel task %s (already done/cancelled or owned by another agent)" task_id))
            else begin
              let new_tasks = List.map (fun t ->
                if t.id = task_id then
                  { t with task_status = Types.Cancelled {
                    cancelled_by = agent_name;
                    cancelled_at = now_iso ();
                    reason = if reason = "" then None else Some reason
                  }}
                else t
              ) backlog.tasks in
              let new_backlog = {
                tasks = new_tasks;
                last_updated = now_iso ();
                version = backlog.version + 1;
              } in
              write_backlog config new_backlog;
              (* Update agent status if they had this task *)
              let agent_file = Filename.concat (agents_dir config) (safe_filename agent_name ^ ".json") in
              if Sys.file_exists agent_file then begin
                let json = read_json config agent_file in
                match agent_of_yojson json with
                | Ok agent when agent.current_task = Some task_id ->
                    let updated = { agent with status = Active; current_task = None } in
                    write_json config agent_file (agent_to_yojson updated)
                | _ -> ()
              end;
              let msg = if reason = "" then Printf.sprintf "🚫 Cancelled %s" task_id else Printf.sprintf "🚫 Cancelled %s - %s" task_id reason in
              let _ = broadcast config ~from_agent:agent_name ~content:msg in
              log_event config (Printf.sprintf "{\"type\":\"task_cancelled\",\"agent\":\"%s\",\"task\":\"%s\",\"reason\":%s,\"ts\":\"%s\"}"
                agent_name task_id (if reason = "" then "null" else Printf.sprintf "\"%s\"" reason) (now_iso ()));
              Ok (Printf.sprintf "🚫 %s cancelled %s" agent_name task_id)
            end
      with e -> Error (Types.IoError (Printexc.to_string e))
    in
    Unix.lockf fd Unix.F_ULOCK 0;
    Unix.close fd;
    result

(** Claim next highest priority unclaimed task *)
let claim_next config ~agent_name =
  ensure_initialized config;

  let lock_file = Filename.concat (tasks_dir config) ".backlog.lock" in
  mkdir_p (Filename.dirname lock_file);

  let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
  Unix.lockf fd Unix.F_LOCK 0;

  let result =
    try
      let backlog = read_backlog config in

      (* Starvation prevention: Calculate effective priority
         Tasks waiting >24h get priority boost (-1 per 24h, min 1) *)
      let now = Unix.gettimeofday () in
      let parse_time iso =
        try
          (* Parse ISO 8601: 2026-01-05T12:30:00Z *)
          Scanf.sscanf iso "%d-%d-%dT%d:%d:%d"
            (fun y mo d h mi _ ->
              let tm = { Unix.tm_sec = 0; tm_min = mi; tm_hour = h;
                         tm_mday = d; tm_mon = mo - 1; tm_year = y - 1900;
                         tm_wday = 0; tm_yday = 0; tm_isdst = false } in
              Some (fst (Unix.mktime tm)))
        with _ -> None
      in
      let effective_priority (task : Types.task) =
        let age_hours =
          match parse_time task.created_at with
          | Some created -> (now -. created) /. 3600.0
          | None -> 0.0
        in
        let boost = int_of_float (age_hours /. 24.0) in
        max 1 (task.priority - boost)
      in

      (* Find highest priority (lowest number) unclaimed task
         Within same priority, prefer older tasks (FIFO) *)
      let sorted = List.sort (fun a b ->
        let priority_cmp = compare (effective_priority a) (effective_priority b) in
        if priority_cmp <> 0 then priority_cmp
        else compare a.created_at b.created_at  (* Older first for same priority *)
      ) backlog.tasks in
      let unclaimed = List.filter (fun t ->
        match t.task_status with
        | Todo -> true
        | _ -> false
      ) sorted in

      match unclaimed with
      | [] ->
          "📋 No unclaimed tasks available"
      | task :: _ ->
          (* Claim this task *)
          let new_tasks = List.map (fun t ->
            if t.id = task.id then
              { t with task_status = Claimed {
                  assignee = agent_name;
                  claimed_at = now_iso ()
                }
              }
            else t
          ) backlog.tasks in

          let new_backlog = {
            tasks = new_tasks;
            last_updated = now_iso ();
            version = backlog.version + 1;
          } in
          write_backlog config new_backlog;

          (* Update agent status *)
          let agent_file = Filename.concat (agents_dir config) (safe_filename agent_name ^ ".json") in
          if Sys.file_exists agent_file then begin
            let json = read_json config agent_file in
            match agent_of_yojson json with
            | Ok agent ->
                let updated = { agent with status = Busy; current_task = Some task.id } in
                write_json config agent_file (agent_to_yojson updated)
            | Error _ -> ()
          end;

          let _ = broadcast config ~from_agent:agent_name
            ~content:(Printf.sprintf "📋 Auto-claimed [P%d] %s: %s" task.priority task.id task.title) in

          log_event config (Printf.sprintf
            "{\"type\":\"task_claim_next\",\"agent\":\"%s\",\"task\":\"%s\",\"priority\":%d,\"ts\":\"%s\"}"
            agent_name task.id task.priority (now_iso ()));

          Printf.sprintf "✅ %s auto-claimed [P%d] %s: %s" agent_name task.priority task.id task.title
    with e ->
      Printf.sprintf "❌ Error: %s" (Printexc.to_string e)
  in

  Unix.lockf fd Unix.F_ULOCK 0;
  Unix.close fd;
  result

(* ======== Walph Control System ======== *)

(** Walph loop state *)
(** Walph state machine for iterative task processing
    Thread-safe implementation using stdlib Mutex for production use.

    Design notes:
    - Uses Mutex for thread-safe state access (stdlib, not Eio)
    - Condition variable for pause/resume (no busy-wait)
    - Fun.protect for exception safety (no zombie states)
    - Atomic check-and-set pattern to prevent double-start race
*)
type walph_state = {
  mutable running : bool;
  mutable paused : bool;
  mutable stop_requested : bool;
  mutable current_preset : string;
  mutable iterations : int;
  mutable completed : int;
  mutex : Mutex.t;       (* Thread safety for state access *)
  cond : Condition.t;    (* Proper wait for pause/resume, no busy-wait *)
}

(** Global Walph state table with its own mutex for thread-safe access *)
let walph_states : (string, walph_state) Hashtbl.t = Hashtbl.create 16
let walph_states_mutex = Mutex.create ()

(** Get or create Walph state for a room (thread-safe) *)
let get_walph_state config =
  let key = config.base_path in
  Mutex.lock walph_states_mutex;
  let state =
    match Hashtbl.find_opt walph_states key with
    | Some s -> s
    | None ->
        let s = {
          running = false; paused = false; stop_requested = false;
          current_preset = ""; iterations = 0; completed = 0;
          mutex = Mutex.create ();
          cond = Condition.create ();
        } in
        Hashtbl.replace walph_states key s;
        s
  in
  Mutex.unlock walph_states_mutex;
  state

(** Remove Walph state for a room (cleanup) *)
let remove_walph_state config =
  let key = config.base_path in
  Mutex.lock walph_states_mutex;
  Hashtbl.remove walph_states key;
  Mutex.unlock walph_states_mutex

(** Run with Walph state mutex locked *)
let with_walph_lock state f =
  Mutex.lock state.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock state.mutex) f

(** Parse @walph command from broadcast message
    Returns: (command, args) or None if not a walph command *)
let parse_walph_command content =
  (* Match @walph COMMAND [args] pattern *)
  if not (try ignore (Str.search_forward (Str.regexp_case_fold "@walph") content 0); true
          with Not_found -> false) then
    None
  else begin
    (* Extract command after @walph *)
    (* Match @walph followed by command word (any non-whitespace, excluding newlines) *)
    let re = Str.regexp_case_fold "@walph[ \t]+\\([^ \t\n\r]+\\)\\(.*\\)" in
    if Str.string_match re content 0 then
      let cmd = String.uppercase_ascii (Str.matched_group 1 content) in
      let args = String.trim (try Str.matched_group 2 content with Not_found -> "") in
      Some (cmd, args)
    (* Only bare @walph (nothing after except optional whitespace) = STATUS *)
    else if Str.string_match (Str.regexp_case_fold "@walph[ \t]*$") content 0 then
      Some ("STATUS", "")
    else
      None
  end

(** Handle @walph control command (thread-safe)
    @param config Room configuration
    @param from_agent Agent sending the command
    @param command Command (STOP, PAUSE, RESUME, STATUS)
    @param args Command arguments
    @return Response message *)
let walph_control config ~from_agent ~command ~args =
  let state = get_walph_state config in
  let response = with_walph_lock state (fun () ->
    match command with
    | "STOP" ->
        if state.running then begin
          state.stop_requested <- true;
          Condition.broadcast state.cond;  (* Wake up pause wait *)
          Printf.sprintf "🛑 @walph STOP requested by %s (will stop after current iteration)" from_agent
        end else
          "ℹ️ @walph is not currently running"
    | "PAUSE" ->
        if state.running && not state.paused then begin
          state.paused <- true;
          Printf.sprintf "⏸️ @walph PAUSED by %s (use @walph RESUME to continue)" from_agent
        end else if state.paused then
          "ℹ️ @walph is already paused"
        else
          "ℹ️ @walph is not currently running"
    | "RESUME" ->
        if state.paused then begin
          state.paused <- false;
          Condition.broadcast state.cond;  (* Wake up pause wait *)
          Printf.sprintf "▶️ @walph RESUMED by %s" from_agent
        end else if state.running then
          "ℹ️ @walph is already running"
        else
          "ℹ️ @walph is not currently running"
    | "STATUS" ->
        if state.running then
          Printf.sprintf "📊 @walph STATUS: %s (iter: %d, done: %d, paused: %b)"
            state.current_preset state.iterations state.completed state.paused
        else
          "ℹ️ @walph is idle (use @walph START <preset> to begin)"
    | "START" ->
        (* START is handled by walph_loop, just acknowledge here *)
        if state.running then
          Printf.sprintf "⚠️ @walph is already running %s. Use @walph STOP first." state.current_preset
        else
          Printf.sprintf "✨ @walph START acknowledged. Args: %s" args
    | _ ->
        Printf.sprintf "❓ Unknown @walph command: %s. Valid: START, STOP, PAUSE, RESUME, STATUS" command
  ) in
  (* Broadcast the response *)
  let _ = broadcast config ~from_agent:"walph" ~content:response in
  response

(** Check if Walph should continue looping (thread-safe, no busy-wait)
    Uses Condition.wait for proper pause synchronization.
    @return true if should continue, false if should stop *)
let walph_should_continue config =
  let state = get_walph_state config in
  with_walph_lock state (fun () ->
    if state.stop_requested then false
    else if state.paused then begin
      (* Wait on condition variable - no busy-wait! *)
      (* Condition.wait atomically releases mutex and waits *)
      while state.paused && not state.stop_requested do
        Condition.wait state.cond state.mutex
      done;
      not state.stop_requested
    end else true
  )

(** Map Walph preset to llm-mcp chain ID
    @param preset The loop preset (coverage, refactor, docs, review, figma, drain)
    @return Some chain_id for presets with corresponding chains, None for drain *)
let get_chain_id_for_preset = function
  | "coverage" -> Some "walph-coverage"
  | "refactor" -> Some "walph-refactor"
  | "docs" -> Some "walph-docs"
  | "review" -> Some "pr-review-pipeline"  (* PR self-review *)
  | "figma" -> Some "walph-figma"  (* Vision-first Figma loop *)
  | "drain" -> None  (* No chain for simple drain *)
  | _ -> None

(** Walph pattern: Keep claiming tasks until stop condition
    Thread-safe with atomic check-and-set and exception safety.

    @param preset Loop preset (drain, coverage, refactor, docs)
    @param max_iterations Maximum iterations before forced stop
    @param target Target file/directory for preset
    @return Status string with loop results *)
let walph_loop config ~agent_name ?(preset="drain") ?(max_iterations=10) ?target () =
  ensure_initialized config;

  (* Get Walph state *)
  let walph_state = get_walph_state config in

  (* Atomic check-and-set to prevent double-start race condition *)
  let start_result = with_walph_lock walph_state (fun () ->
    if walph_state.running then
      Error (Printf.sprintf "⚠️ @walph is already running %s. Use @walph STOP first." walph_state.current_preset)
    else begin
      (* Atomically set running=true under lock *)
      walph_state.running <- true;
      walph_state.paused <- false;
      walph_state.stop_requested <- false;
      walph_state.current_preset <- preset;
      walph_state.iterations <- 0;
      walph_state.completed <- 0;
      Ok ()
    end
  ) in

  match start_result with
  | Error msg ->
      let _ = broadcast config ~from_agent:"walph" ~content:msg in
      msg
  | Ok () ->
      (* Use Fun.protect to ensure running <- false even on exceptions (zombie prevention) *)
      let stop_reason = ref "" in

      Fun.protect
        ~finally:(fun () ->
          (* Always reset running state, even on exception *)
          with_walph_lock walph_state (fun () ->
            walph_state.running <- false
          ))
        (fun () ->
          let _ = broadcast config ~from_agent:agent_name
            ~content:(Printf.sprintf "🔄 @walph START %s%s (max: %d)"
              preset
              (match target with Some t -> " --target " ^ t | None -> "")
              max_iterations) in

          (* UTF-8 safe prefix check: 📋=4bytes, ✅=3bytes, ❌=3bytes *)
          let starts_with prefix s =
            let plen = String.length prefix in
            String.length s >= plen && String.sub s 0 plen = prefix
          in

          (* Run the loop *)
          let rec loop () =
            (* Check control state before each iteration *)
            if not (walph_should_continue config) then begin
              stop_reason := if walph_state.stop_requested then "stop requested" else "paused indefinitely";
              ()
            end else begin
              (* Check max iterations with lock *)
              let should_stop = with_walph_lock walph_state (fun () ->
                if walph_state.iterations >= max_iterations then begin
                  stop_reason := Printf.sprintf "max_iterations reached (%d)" max_iterations;
                  true
                end else begin
                  walph_state.iterations <- walph_state.iterations + 1;
                  false
                end
              ) in
              if should_stop then ()
              else begin
                (* Try to claim next task *)
                let claim_result = claim_next config ~agent_name in

                if starts_with "📋" claim_result || starts_with "No unclaimed" claim_result then begin
                  (* No more unclaimed tasks - drain complete *)
                  stop_reason := "backlog drained";
                  ()
                end else if starts_with "✅" claim_result then begin
                  with_walph_lock walph_state (fun () ->
                    walph_state.completed <- walph_state.completed + 1
                  );

                  (* Broadcast progress *)
                  let _ = broadcast config ~from_agent:agent_name
                    ~content:(Printf.sprintf "📊 @walph Iteration %d: %s" walph_state.iterations claim_result) in

                  (* Continue loop *)
                  loop ()
                end else begin
                  (* Error or unexpected result *)
                  stop_reason := Printf.sprintf "claim error: %s" claim_result;
                  ()
                end
              end
            end
          in

          loop ();

          (* Final broadcast and log *)
          let result = Printf.sprintf
            "🛑 @walph STOPPED. Preset: %s, Iterations: %d, Tasks completed: %d, Reason: %s"
            preset walph_state.iterations walph_state.completed !stop_reason in

          let _ = broadcast config ~from_agent:agent_name ~content:result in

          log_event config (Printf.sprintf
            "{\"type\":\"walph_loop_complete\",\"agent\":\"%s\",\"preset\":\"%s\",\"iterations\":%d,\"completed\":%d,\"reason\":\"%s\",\"ts\":\"%s\"}"
            agent_name preset walph_state.iterations walph_state.completed !stop_reason (now_iso ()));

          result
        )

(** Update task priority *)
let update_priority config ~task_id ~priority =
  ensure_initialized config;

  let lock_file = Filename.concat (tasks_dir config) ".backlog.lock" in
  mkdir_p (Filename.dirname lock_file);

  let fd = Unix.openfile lock_file [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
  Unix.lockf fd Unix.F_LOCK 0;

  let result =
    try
      let backlog = read_backlog config in

      let task_opt = List.find_opt (fun t -> t.id = task_id) backlog.tasks in

      match task_opt with
      | None ->
          Printf.sprintf "❌ Task %s not found" task_id
      | Some task ->
          let old_priority = task.priority in
          let new_tasks = List.map (fun t ->
            if t.id = task_id then { t with priority }
            else t
          ) backlog.tasks in

          let new_backlog = {
            tasks = new_tasks;
            last_updated = now_iso ();
            version = backlog.version + 1;
          } in
          write_backlog config new_backlog;

          log_event config (Printf.sprintf
            "{\"type\":\"priority_change\",\"task\":\"%s\",\"old\":%d,\"new\":%d,\"ts\":\"%s\"}"
            task_id old_priority priority (now_iso ()));

          Printf.sprintf "✅ Task %s priority: P%d → P%d" task_id old_priority priority
    with e ->
      Printf.sprintf "❌ Error: %s" (Printexc.to_string e)
  in

  Unix.lockf fd Unix.F_ULOCK 0;
  Unix.close fd;
  result

(** Get raw task list (for orchestrator) *)
let get_tasks_raw config =
  ensure_initialized config;
  let backlog = read_backlog config in
  backlog.tasks

(** Get raw agent list (for orchestrator) *)
let get_agents_raw config =
  ensure_initialized config;
  let agents_path = agents_dir config in
  if not (Sys.file_exists agents_path) then []
  else
    Sys.readdir agents_path
    |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".json")
    |> List.filter_map (fun name ->
        let path = Filename.concat agents_path name in
        let json = read_json config path in
        match agent_of_yojson json with
        | Ok agent -> Some agent
        | Error _ -> None
      )

(** Check if an agent has joined the room *)
let is_agent_joined config ~agent_name =
  ensure_initialized config;
  let agents_path = agents_dir config in
  if not (Sys.file_exists agents_path) then false
  else
    let filename = safe_filename agent_name ^ ".json" in
    let path = Filename.concat agents_path filename in
    Sys.file_exists path

(** Check if filename is valid (no special characters) *)
let is_valid_filename name =
  String.for_all (fun c ->
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    (c >= '0' && c <= '9') ||
    c = '_' || c = '-' || c = '.'
  ) name

(** Extract seq number from filename like "000001885_unknown_broadcast.json" or "1664_codex_broadcast.json" *)
let extract_seq_from_filename name =
  try
    let idx = String.index name '_' in
    int_of_string (String.sub name 0 idx)
  with _ -> 0

(** Get raw message list (for dashboard) *)
let get_messages_raw config ~since_seq ~limit =
  ensure_initialized config;
  let msgs_path = messages_dir config in
  if not (Sys.file_exists msgs_path) then []
  else
    Sys.readdir msgs_path
    |> Array.to_list
    |> List.filter is_valid_filename  (* Skip files with invalid chars *)
    |> List.sort (fun a b -> compare (extract_seq_from_filename b) (extract_seq_from_filename a))
    |> List.filter_map (fun name ->
        let path = Filename.concat msgs_path name in
        try
          let json = read_json config path in
          match message_of_yojson json with
          | Ok msg when msg.seq > since_seq -> Some msg
          | _ -> None
        with _ -> None  (* Skip files that fail to read *)
      )
    |> (fun msgs -> List.filteri (fun i _ -> i < limit) msgs)

(** List tasks *)
let list_tasks config =
  ensure_initialized config;

  let backlog = read_backlog config in
  if backlog.tasks = [] then
    "📋 No tasks yet."
  else begin
    let buf = Buffer.create 256 in
    Buffer.add_string buf "📋 Quest Board\n";
    Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";

    let sorted = List.sort (fun a b -> compare a.priority b.priority) backlog.tasks in
    List.iter (fun task ->
      let status_icon = match task.task_status with
        | Done _ -> "✅"
        | Claimed _ | InProgress _ -> "🔄"
        | Todo -> "📋"
        | Cancelled _ -> "🚫"
      in
      let assignee = match task.task_status with
        | Claimed { assignee; _ } | InProgress { assignee; _ } | Done { assignee; _ } -> assignee
        | Cancelled { cancelled_by; _ } -> cancelled_by
        | Todo -> "unclaimed"
      in
      let status_str = match task.task_status with
        | Todo -> "todo"
        | Claimed _ -> "claimed"
        | InProgress _ -> "in_progress"
        | Done _ -> "done"
        | Cancelled _ -> "cancelled"
      in
      Buffer.add_string buf (Printf.sprintf "%s [%d] %s: %s\n" status_icon task.priority task.id task.title);
      Buffer.add_string buf (Printf.sprintf "   └─ %s | %s\n" status_str assignee)
    ) sorted;

    Buffer.contents buf
  end

(** Get recent messages *)
let get_messages config ~since_seq ~limit =
  ensure_initialized config;

  let buf = Buffer.create 256 in
  Buffer.add_string buf "💬 Recent Messages\n";
  Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";

  let msgs_path = messages_dir config in
  if Sys.file_exists msgs_path then begin
    let files = Sys.readdir msgs_path |> Array.to_list
      |> List.sort (fun a b -> compare (extract_seq_from_filename b) (extract_seq_from_filename a)) in
    let count = ref 0 in
    List.iter (fun name ->
      if !count < limit then begin
        let path = Filename.concat msgs_path name in
        let json = read_json config path in
        match message_of_yojson json with
        | Ok msg when msg.seq > since_seq ->
            let time_part = String.sub msg.timestamp 0 (min 16 (String.length msg.timestamp)) in
            let time_str = String.map (function 'T' -> ' ' | c -> c) time_part in
            Buffer.add_string buf (Printf.sprintf "[%s] %s: %s\n" time_str msg.from_agent msg.content);
            incr count
        | _ -> ()
      end
    ) files
  end;

  if Buffer.length buf = 73 then (* Only header *)
    Buffer.add_string buf "(no new messages)\n";

  Buffer.contents buf

(* ============================================================ *)
(* Portal / A2A Protocol - Extracted to Room_portal module      *)
(* ============================================================ *)
include Room_portal

(* ============================================ *)
(* Git Worktree - Extracted to Room_worktree module *)
(* ============================================ *)
include Room_worktree

(* Portal and Worktree functions are now in their respective modules.
   See Room_portal and Room_worktree for implementations. *)

(* ============================================ *)
(* Heartbeat & Zombie Agent Cleanup             *)
(* ============================================ *)

(* Note: heartbeat_timeout_seconds, parse_iso_time_opt, parse_iso_time,
   and is_zombie_agent are defined earlier in the file for use in status *)

(** Update agent heartbeat - must be called periodically *)
let heartbeat config ~agent_name =
  ensure_initialized config;

  (* Support both exact nickname and agent_type prefix match *)
  let actual_name = resolve_agent_name config agent_name in
  let agent_file = Filename.concat (agents_dir config) (safe_filename actual_name ^ ".json") in
  if Sys.file_exists agent_file then begin
    with_file_lock config agent_file (fun () ->
      let json = read_json config agent_file in
      match agent_of_yojson json with
      | Ok agent ->
          let updated = { agent with last_seen = now_iso () } in
          write_json config agent_file (agent_to_yojson updated);
          Printf.sprintf "💓 %s heartbeat updated" actual_name
      | Error _ ->
          Printf.sprintf "⚠ Invalid agent file for %s" actual_name
    )
  end else
    Printf.sprintf "⚠ Agent %s not found" agent_name

(** Cleanup zombie agents - removes stale agents *)
let cleanup_zombies config =
  ensure_initialized config;

  let agents_path = agents_dir config in
  if not (Sys.file_exists agents_path) then
    "📋 No agents directory"
  else begin
    let zombies = ref [] in

    (* Find zombie agents *)
    Sys.readdir agents_path |> Array.iter (fun name ->
      if Filename.check_suffix name ".json" then begin
        let path = Filename.concat agents_path name in
        let json = read_json config path in
        match agent_of_yojson json with
        | Ok agent when is_zombie_agent agent.last_seen ->
            zombies := agent.name :: !zombies;
            (* Remove agent file *)
            Sys.remove path
        | _ -> ()
      end
    );

    (* Update state to remove zombie agents *)
    if !zombies <> [] then begin
      let _ = update_state config (fun s ->
        { s with active_agents = List.filter (fun a -> not (List.mem a !zombies)) s.active_agents }
      ) in

      (* Log event *)
      log_event config (Printf.sprintf
        "{\"type\":\"zombie_cleanup\",\"agents\":%s,\"ts\":\"%s\"}"
        (Yojson.Safe.to_string (`List (List.map (fun s -> `String s) !zombies)))
        (now_iso ()));

      Printf.sprintf "🧟 Cleaned up %d zombie agent(s): %s"
        (List.length !zombies) (String.concat ", " !zombies)
    end else
      "✅ No zombie agents found"
  end

(** Garbage collection - cleanup zombies, stale tasks, old messages *)
let gc config ?(days=7) () =
  ensure_initialized config;

  let results = ref [] in

  (* 1. Cleanup zombies *)
  let zombie_result = cleanup_zombies config in
  results := zombie_result :: !results;

  (* 2. Archive stale tasks (older than N days, not completed) *)
  let cutoff_time =
    let now = Unix.gettimeofday () in
    now -. (float_of_int days *. 24. *. 60. *. 60.)
  in
  let cutoff_iso =
    let tm = Unix.gmtime cutoff_time in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in

  let backlog = read_backlog config in
  let stale_count = ref 0 in
  let archived_tasks = ref [] in
  let kept_tasks = List.filter (fun task ->
    let is_done = match task.task_status with Done _ -> true | _ -> false in
    let is_old = task.created_at < cutoff_iso in
    if is_old && not is_done then begin
      incr stale_count;
      archived_tasks := task :: !archived_tasks;
      false  (* Remove stale task *)
    end else
      true   (* Keep task *)
  ) backlog.tasks in

  if !stale_count > 0 then begin
    append_archive_tasks config (List.rev !archived_tasks);
    let new_backlog = {
      tasks = kept_tasks;
      last_updated = now_iso ();
      version = backlog.version + 1;
    } in
    write_backlog config new_backlog;
    results := Printf.sprintf "📦 Archived %d stale task(s) (older than %d days)" !stale_count days :: !results
  end else
    results := Printf.sprintf "✅ No stale tasks (threshold: %d days)" days :: !results;

  (* 3. Cleanup old messages - but preserve messages referencing open tasks *)
  let messages_path = messages_dir config in
  let old_msg_count = ref 0 in
  let preserved_count = ref 0 in

  (* Get open task IDs (not Done or Cancelled) *)
  let open_task_ids =
    List.filter_map (fun task ->
      match task.task_status with
      | Done _ | Cancelled _ -> None
      | _ -> Some task.id
    ) backlog.tasks
  in

  (* Helper to check if content mentions an open task *)
  let mentions_open_task content =
    List.exists (fun task_id ->
      try ignore (Str.search_forward (Str.regexp_string task_id) content 0); true
      with Not_found -> false
    ) open_task_ids
  in

  if Sys.file_exists messages_path then begin
    Sys.readdir messages_path |> Array.iter (fun name ->
      if Filename.check_suffix name ".json" then begin
        let path = Filename.concat messages_path name in
        let json = read_json config path in
        let ts = Yojson.Safe.Util.(member "timestamp" json |> to_string_option) in
        let content = Yojson.Safe.Util.(member "content" json |> to_string_option)
                      |> Option.value ~default:"" in
        match ts with
        | Some ts when ts < cutoff_iso ->
            (* Preserve if message references an open task *)
            if mentions_open_task content then
              incr preserved_count
            else begin
              Sys.remove path;
              incr old_msg_count
            end
        | _ -> ()
      end
    )
  end;

  if !old_msg_count > 0 || !preserved_count > 0 then begin
    if !old_msg_count > 0 then
      results := Printf.sprintf "🗑️ Deleted %d old message(s) (older than %d days)" !old_msg_count days :: !results;
    if !preserved_count > 0 then
      results := Printf.sprintf "🔒 Preserved %d message(s) referencing open tasks" !preserved_count :: !results
  end else
    results := Printf.sprintf "✅ No old messages (threshold: %d days)" days :: !results;

  (* 4. Cleanup backend pubsub - PostgreSQL specific, no-op for others *)
  let pubsub_cleanup_count = ref 0 in
  (match backend_cleanup_pubsub config ~days ~max_messages:10000 with
   | Ok count when count > 0 ->
       pubsub_cleanup_count := count;
       results := Printf.sprintf "🗃️ Cleaned %d pubsub message(s) from backend" count :: !results
   | Ok _ -> ()  (* No messages to clean *)
   | Error e ->
       results := Printf.sprintf "⚠️ Backend pubsub cleanup failed: %s" (Backend.show_error e) :: !results);

  (* Log event *)
  log_event config (Printf.sprintf
    "{\"type\":\"gc\",\"stale_tasks\":%d,\"old_messages\":%d,\"preserved\":%d,\"pubsub_cleaned\":%d,\"days\":%d,\"ts\":\"%s\"}"
    !stale_count !old_msg_count !preserved_count !pubsub_cleanup_count days (now_iso ()));

  String.concat "\n" (List.rev !results)

(** Get all agents with their status *)
let get_agents_status config =
  ensure_initialized config;

  let agents_path = agents_dir config in
  if not (Sys.file_exists agents_path) then
    `Assoc [("agents", `List []); ("count", `Int 0)]
  else begin
    let agents = ref [] in
    Sys.readdir agents_path |> Array.iter (fun name ->
      if Filename.check_suffix name ".json" then begin
        let path = Filename.concat agents_path name in
        let json = read_json config path in
        match agent_of_yojson json with
        | Ok agent ->
            let is_zombie = is_zombie_agent agent.last_seen in
            let status = if is_zombie then "zombie" else agent_status_to_string agent.status in
            agents := `Assoc [
              ("name", `String agent.name);
              ("status", `String status);
              ("is_zombie", `Bool is_zombie);
              ("current_task", match agent.current_task with Some t -> `String t | None -> `Null);
              ("last_seen", `String agent.last_seen);
              ("capabilities", `List (List.map (fun s -> `String s) agent.capabilities));
            ] :: !agents
        | Error _ -> ()
      end
    );
    `Assoc [
      ("agents", `List (List.rev !agents));
      ("count", `Int (List.length !agents));
    ]
  end

(* ============================================ *)
(* Agent Discovery - Capability Broadcasting   *)
(* ============================================ *)

(** Register agent capabilities *)
let register_capabilities config ~agent_name ~capabilities =
  ensure_initialized config;

  (* Support both exact nickname and agent_type prefix match *)
  let actual_name = resolve_agent_name config agent_name in
  let agent_file = Filename.concat (agents_dir config) (safe_filename actual_name ^ ".json") in
  if Sys.file_exists agent_file then begin
    with_file_lock config agent_file (fun () ->
      let json = read_json config agent_file in
      match agent_of_yojson json with
      | Ok agent ->
          let updated = { agent with capabilities; last_seen = now_iso () } in
          write_json config agent_file (agent_to_yojson updated);

          (* Log event *)
          log_event config (Printf.sprintf
            "{\"type\":\"capabilities_registered\",\"agent\":\"%s\",\"capabilities\":%s,\"ts\":\"%s\"}"
            actual_name
            (Yojson.Safe.to_string (`List (List.map (fun s -> `String s) capabilities)))
            (now_iso ()));

          Printf.sprintf "📡 %s capabilities: %s" actual_name (String.concat ", " capabilities)
      | Error _ ->
          Printf.sprintf "⚠ Invalid agent file for %s" actual_name
    )
  end else
    Printf.sprintf "⚠ Agent %s not found. Join first!" agent_name

(** Update agent metadata (status/capabilities). *)
let update_agent_r config ~agent_name ?status ?capabilities () : string Types.masc_result =
  if not (is_initialized config) then Error Types.NotInitialized
  else match validate_agent_name_r agent_name with
    | Error e -> Error e
    | Ok _ ->
        let actual_name = resolve_agent_name config agent_name in
        let agent_file = Filename.concat (agents_dir config) (safe_filename actual_name ^ ".json") in
        if not (Sys.file_exists agent_file) then
          Error (Types.AgentNotFound actual_name)
        else
          let locked =
            with_file_lock_r config agent_file (fun () ->
              let json = read_json config agent_file in
              match agent_of_yojson json with
              | Error _ -> Error (Types.InvalidJson "Invalid agent file")
              | Ok agent ->
                  let status_opt =
                    match status with
                    | None -> Ok None
                    | Some s ->
                        (match Types.agent_status_of_string_opt (String.lowercase_ascii s) with
                         | Some st -> Ok (Some st)
                         | None -> Error (Types.InvalidJson ("Unknown status: " ^ s)))
                  in
                  (match status_opt with
                   | Error e -> Error e
                   | Ok maybe_status ->
                       let invalid =
                         match agent.current_task, maybe_status with
                         | Some _, Some Types.Inactive ->
                             Some "Cannot set inactive while a task is assigned"
                         | None, Some Types.Busy ->
                             Some "Cannot set busy without an active task"
                         | _ -> None
                       in
                       (match invalid with
                        | Some msg -> Error (Types.TaskInvalidState msg)
                        | None ->
                            let updated_caps =
                              match capabilities with
                              | None -> agent.capabilities
                              | Some caps -> caps
                            in
                            let updated_status =
                              match maybe_status with
                              | None -> agent.status
                              | Some st -> st
                            in
                            let updated = {
                              agent with
                              status = updated_status;
                              capabilities = updated_caps;
                              last_seen = now_iso ();
                            } in
                            write_json config agent_file (agent_to_yojson updated);
                            log_event config (Printf.sprintf
                              "{\"type\":\"agent_update\",\"agent\":\"%s\",\"status\":\"%s\",\"capabilities\":%s,\"ts\":\"%s\"}"
                              actual_name
                              (Types.agent_status_to_string updated_status)
                              (Yojson.Safe.to_string (`List (List.map (fun s -> `String s) updated_caps)))
                              (now_iso ()));
                            Ok (Printf.sprintf "✅ %s updated" actual_name)
                       ))
            )
          in
          (match locked with
           | Ok (Ok msg) -> Ok msg
           | Ok (Error e) -> Error e
           | Error e -> Error e)

(** Find agents by capability *)
let find_agents_by_capability config ~capability =
  ensure_initialized config;

  let agents_path = agents_dir config in
  if not (Sys.file_exists agents_path) then
    `Assoc [("agents", `List []); ("count", `Int 0)]
  else begin
    let matching = ref [] in
    Sys.readdir agents_path |> Array.iter (fun name ->
      if Filename.check_suffix name ".json" then begin
        let path = Filename.concat agents_path name in
        let json = read_json config path in
        match agent_of_yojson json with
        | Ok agent when List.mem capability agent.capabilities && not (is_zombie_agent agent.last_seen) ->
            matching := `Assoc [
              ("name", `String agent.name);
              ("status", `String (agent_status_to_string agent.status));
              ("capabilities", `List (List.map (fun s -> `String s) agent.capabilities));
            ] :: !matching
        | _ -> ()
      end
    );
    `Assoc [
      ("capability", `String capability);
      ("agents", `List (List.rev !matching));
      ("count", `Int (List.length !matching));
    ]
  end

(* ============================================ *)
(* Consensus / Voting System                    *)
(* ============================================ *)

(** Voting directory *)
let votes_dir config = Filename.concat (masc_dir config) "votes"

(** Vote status type *)
type vote_status = VotePending | VoteApproved | VoteRejected | VoteTied
[@@deriving show { with_path = false }]

let vote_status_to_string = function
  | VotePending -> "pending"
  | VoteApproved -> "approved"
  | VoteRejected -> "rejected"
  | VoteTied -> "tied"

(** Create a vote proposal *)
let vote_create config ~proposer ~topic ~options ~required_votes =
  ensure_initialized config;

  mkdir_p (votes_dir config);

  let vote_id = Printf.sprintf "vote-%s-%d" (String.sub (now_iso ()) 0 10)
    (Random.int 10000) in
  let vote_path = Filename.concat (votes_dir config) (vote_id ^ ".json") in

  let vote_json = `Assoc [
    ("id", `String vote_id);
    ("proposer", `String proposer);
    ("topic", `String topic);
    ("options", `List (List.map (fun s -> `String s) options));
    ("votes", `Assoc []);  (* agent -> option *)
    ("required_votes", `Int required_votes);
    ("status", `String "pending");
    ("created_at", `String (now_iso ()));
    ("resolved_at", `Null);
  ] in

  write_json config vote_path vote_json;

  (* Broadcast *)
  let _ = broadcast config ~from_agent:proposer
    ~content:(Printf.sprintf "🗳️ Vote started: %s (options: %s)" topic (String.concat ", " options)) in

  (* Log event *)
  log_event config (Printf.sprintf
    "{\"type\":\"vote_created\",\"id\":\"%s\",\"proposer\":\"%s\",\"topic\":\"%s\",\"ts\":\"%s\"}"
    vote_id proposer topic (now_iso ()));

  Printf.sprintf "🗳️ Vote created: %s\n  Topic: %s\n  Options: %s\n  Required: %d votes"
    vote_id topic (String.concat ", " options) required_votes

(** Cast a vote *)
let vote_cast config ~agent_name ~vote_id ~choice =
  ensure_initialized config;

  let vote_path = Filename.concat (votes_dir config) (vote_id ^ ".json") in
  if not (Sys.file_exists vote_path) then
    Printf.sprintf "❌ Vote %s not found" vote_id
  else begin
    with_file_lock config vote_path (fun () ->
      let json = read_json config vote_path in
      let open Yojson.Safe.Util in

      let status = json |> member "status" |> to_string in
      if status <> "pending" then
        Printf.sprintf "⚠ Vote %s already resolved (%s)" vote_id status
      else begin
        let options = json |> member "options" |> to_list |> List.map to_string in
        if not (List.mem choice options) then
          Printf.sprintf "❌ Invalid choice: %s. Options: %s" choice (String.concat ", " options)
        else begin
          let votes = json |> member "votes" in
          let current_votes = match votes with
            | `Assoc kvs -> kvs
            | _ -> []
          in

          (* Add/update vote *)
          let new_votes = (agent_name, `String choice) ::
            (List.filter (fun (k, _) -> k <> agent_name) current_votes) in

          let required = json |> member "required_votes" |> to_int in
          let vote_count = List.length new_votes in

          (* Check if vote is resolved *)
          let resolved, new_status, winner =
            if vote_count >= required then begin
              (* Count votes per option *)
              let counts = List.fold_left (fun acc (_, v) ->
                let opt = to_string v in
                let curr = try List.assoc opt acc with Not_found -> 0 in
                (opt, curr + 1) :: (List.remove_assoc opt acc)
              ) [] new_votes in

              let max_count = List.fold_left (fun acc (_, c) -> max acc c) 0 counts in
              let winners = List.filter (fun (_, c) -> c = max_count) counts in

              if List.length winners > 1 then
                (true, VoteTied, None)
              else
                (true, VoteApproved, Some (fst (List.hd winners)))
            end else
              (false, VotePending, None)
          in

          let updated_json = `Assoc [
            ("id", json |> member "id");
            ("proposer", json |> member "proposer");
            ("topic", json |> member "topic");
            ("options", json |> member "options");
            ("votes", `Assoc new_votes);
            ("required_votes", json |> member "required_votes");
            ("status", `String (vote_status_to_string new_status));
            ("created_at", json |> member "created_at");
            ("resolved_at", if resolved then `String (now_iso ()) else `Null);
            ("winner", match winner with Some w -> `String w | None -> `Null);
          ] in

          write_json config vote_path updated_json;

          (* Log event *)
          log_event config (Printf.sprintf
            "{\"type\":\"vote_cast\",\"id\":\"%s\",\"agent\":\"%s\",\"choice\":\"%s\",\"ts\":\"%s\"}"
            vote_id agent_name choice (now_iso ()));

          if resolved then begin
            let topic = json |> member "topic" |> to_string in
            let result_msg = match new_status with
              | VoteApproved -> Printf.sprintf "Winner: %s" (Option.get winner)
              | VoteTied -> "Result: Tied!"
              | _ -> "Resolved"
            in
            let _ = broadcast config ~from_agent:"system"
              ~content:(Printf.sprintf "🗳️ Vote resolved: %s - %s" topic result_msg) in
            Printf.sprintf "✅ Vote cast: %s for %s\n🎉 Vote resolved! %s" agent_name choice result_msg
          end else
            Printf.sprintf "✅ Vote cast: %s for %s (%d/%d votes)"
              agent_name choice vote_count required
        end
      end
    )
  end

(** Get vote status *)
let vote_status config ~vote_id =
  ensure_initialized config;

  let vote_path = Filename.concat (votes_dir config) (vote_id ^ ".json") in
  if not (Sys.file_exists vote_path) then
    `Assoc [("error", `String (Printf.sprintf "Vote %s not found" vote_id))]
  else
    read_json config vote_path

(** List active votes *)
let list_votes config =
  ensure_initialized config;

  let votes_path = votes_dir config in
  if not (Sys.file_exists votes_path) then
    `Assoc [("votes", `List []); ("count", `Int 0)]
  else begin
    let votes = ref [] in
    Sys.readdir votes_path |> Array.iter (fun name ->
      if Filename.check_suffix name ".json" then begin
        let path = Filename.concat votes_path name in
        votes := read_json config path :: !votes
      end
    );
    `Assoc [
      ("votes", `List (List.rev !votes));
      ("count", `Int (List.length !votes));
    ]
  end

(* ============================================ *)
(* Tempo Control (Cluster Pace Management)     *)
(* ============================================ *)

(** Path to tempo.json *)
let tempo_path config = Filename.concat (masc_dir config) "tempo.json"

(** Read tempo config from file *)
let read_tempo config : tempo_config =
  let path = tempo_path config in
  if Sys.file_exists path then
    try
      match tempo_config_of_yojson (read_json config path) with
      | Ok t -> t
      | Error _ -> default_tempo_config
    with _ -> default_tempo_config
  else
    default_tempo_config

(** Write tempo config to file *)
let write_tempo config (tempo : tempo_config) =
  write_json config (tempo_path config) (tempo_config_to_yojson tempo)

(** Get current tempo - returns JSON for MCP response *)
let get_tempo config =
  ensure_initialized config;
  let tempo = read_tempo config in
  tempo_config_to_yojson tempo

(** Set tempo with mode, reason, and agent tracking *)
let set_tempo config ~mode ~reason ~agent_name =
  ensure_initialized config;
  match tempo_mode_of_string mode with
  | Error e -> Printf.sprintf "❌ Invalid tempo mode: %s" e
  | Ok tempo_mode ->
      (* Set delay based on mode *)
      let delay_ms = match tempo_mode with
        | Normal -> 0
        | Slow -> 2000    (* 2 second delay for careful work *)
        | Fast -> 0       (* No delay *)
        | Paused -> 0     (* No delay, but paused state *)
      in
      let tempo = {
        mode = tempo_mode;
        delay_ms;
        reason;
        set_by = Some agent_name;
        set_at = Some (now_iso ());
      } in
      write_tempo config tempo;

      (* Broadcast tempo change *)
      let emoji = match tempo_mode with
        | Normal -> "🎵"
        | Slow -> "🐢"
        | Fast -> "🚀"
        | Paused -> "⏸️"
      in
      let reason_str = match reason with
        | Some r -> Printf.sprintf " (%s)" r
        | None -> ""
      in
      let _ = broadcast config ~from_agent:agent_name
        ~content:(Printf.sprintf "%s Tempo → %s%s" emoji mode reason_str) in

      Printf.sprintf "✅ Tempo set to %s (delay: %dms)%s" mode delay_ms reason_str

(* ============================================ *)
(* Multi-Room Management                        *)
(* ============================================ *)

(** Slugify a string for use as room ID *)
let slugify name =
  String.lowercase_ascii name
  |> String.map (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then c
      else '-')
  |> (fun s ->
      (* Remove leading/trailing dashes and collapse multiple dashes *)
      let rec collapse acc prev_dash = function
        | [] -> List.rev acc
        | '-' :: rest when prev_dash -> collapse acc true rest
        | '-' :: rest -> collapse ('-' :: acc) true rest
        | c :: rest -> collapse (c :: acc) false rest
      in
      String.to_seq s |> List.of_seq |> collapse [] true |> List.to_seq |> String.of_seq)
  |> (fun s ->
      let len = String.length s in
      if len > 0 && s.[0] = '-' then String.sub s 1 (len - 1) else s)
  |> (fun s ->
      let len = String.length s in
      if len > 0 && s.[len - 1] = '-' then String.sub s 0 (len - 1) else s)

(** Get rooms directory path *)
let rooms_dir config = Filename.concat config.base_path "rooms"

(** Get room registry file path *)
let registry_path config = Filename.concat config.base_path "rooms.json"

(** Get current room file path *)
let current_room_path config = Filename.concat config.base_path "current_room"

(** Read current room ID *)
let read_current_room config =
  let path = current_room_path config in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let room_id = input_line ic in
      close_in ic;
      Some (String.trim room_id)
    with _ -> Some "default"
  else
    Some "default"

(** Write current room ID *)
let write_current_room config room_id =
  let path = current_room_path config in
  let oc = open_out path in
  output_string oc room_id;
  output_char oc '\n';
  close_out oc

(** Get path for a specific room *)
let room_path config room_id =
  if room_id = "default" then
    (* Legacy backward compatibility: default room uses root .masc/ *)
    config.base_path
  else
    Filename.concat (rooms_dir config) room_id

(** Count agents in a room *)
let count_agents_in_room config room_id =
  let rpath = room_path config room_id in
  let agents_path = Filename.concat rpath "agents" in
  if Sys.file_exists agents_path && Sys.is_directory agents_path then
    Array.length (Sys.readdir agents_path)
  else
    0

(** Count tasks in a room *)
let count_tasks_in_room config room_id =
  let rpath = room_path config room_id in
  let tasks_path = Filename.concat rpath "tasks" in
  if Sys.file_exists tasks_path && Sys.is_directory tasks_path then
    Array.length (Sys.readdir tasks_path)
  else
    0

(** Load room registry *)
let load_registry config : Types.room_registry =
  let path = registry_path config in
  if Sys.file_exists path then
    try
      let json = read_json config path in
      match Types.room_registry_of_yojson json with
      | Ok registry -> registry
      | Error _ -> { rooms = []; default_room = "default"; current_room = Some "default" }
    with _ -> { rooms = []; default_room = "default"; current_room = Some "default" }
  else
    { rooms = []; default_room = "default"; current_room = Some "default" }

(** Save room registry *)
let save_registry config (registry : Types.room_registry) =
  let path = registry_path config in
  write_json config path (Types.room_registry_to_yojson registry)

(** List all available rooms *)
let rooms_list config : Yojson.Safe.t =
  if not (is_initialized config) then
    `Assoc [
      ("rooms", `List []);
      ("current_room", `Null);
      ("error", `String "MASC not initialized")
    ]
  else begin
    let registry = load_registry config in
    let current = read_current_room config in

    (* Always include default room even if not in registry *)
    let default_room : Types.room_info = {
      id = "default";
      name = "Default Room";
      description = Some "Default coordination room";
      created_at = now_iso ();  (* Current time instead of epoch *)
      created_by = None;
      agent_count = count_agents_in_room config "default";
      task_count = count_tasks_in_room config "default";
    } in

    (* Update room counts and merge with default *)
    let rooms_with_counts = List.map (fun (r : Types.room_info) ->
      { r with
        agent_count = count_agents_in_room config r.id;
        task_count = count_tasks_in_room config r.id;
      }
    ) registry.rooms in

    (* Ensure default is in the list *)
    let all_rooms =
      if List.exists (fun (r : Types.room_info) -> r.id = "default") rooms_with_counts then
        rooms_with_counts
      else
        default_room :: rooms_with_counts
    in

    `Assoc [
      ("rooms", `List (List.map Types.room_info_to_yojson all_rooms));
      ("current_room", match current with Some r -> `String r | None -> `String "default");
    ]
  end

(** Create a new room *)
let room_create config ~name ~description : Yojson.Safe.t =
  if not (is_initialized config) then
    `Assoc [("error", `String "MASC not initialized")]
  else begin
    let room_id = slugify name in

    (* Check if room already exists *)
    let registry = load_registry config in
    if List.exists (fun (r : Types.room_info) -> r.id = room_id) registry.rooms then
      `Assoc [("error", `String (Printf.sprintf "Room '%s' already exists" room_id))]
    else if room_id = "default" then
      `Assoc [("error", `String "Cannot create room with reserved name 'default'")]
    else begin
      (* Create room directory structure *)
      let rpath = room_path config room_id in
      mkdir_p rpath;
      mkdir_p (Filename.concat rpath "agents");
      mkdir_p (Filename.concat rpath "tasks");
      mkdir_p (Filename.concat rpath "locks");

      (* Create room info *)
      let room_info : Types.room_info = {
        id = room_id;
        name;
        description;
        created_at = now_iso ();
        created_by = None;
        agent_count = 0;
        task_count = 0;
      } in

      (* Update registry *)
      let updated_registry = {
        registry with
        rooms = room_info :: registry.rooms;
      } in
      save_registry config updated_registry;

      `Assoc [
        ("id", `String room_id);
        ("name", `String name);
        ("message", `String (Printf.sprintf "✅ Room '%s' created" room_id));
      ]
    end
  end

(** Enter a room (switch context) *)
let room_enter config ~room_id ~agent_type : Yojson.Safe.t =
  if not (is_initialized config) then
    `Assoc [("error", `String "MASC not initialized")]
  else begin
    (* Check if room exists *)
    let registry = load_registry config in
    let room_exists =
      room_id = "default" ||
      List.exists (fun (r : Types.room_info) -> r.id = room_id) registry.rooms
    in

    if not room_exists then
      `Assoc [("error", `String (Printf.sprintf "Room '%s' does not exist" room_id))]
    else begin
      let previous_room = read_current_room config in

      (* Update current room *)
      write_current_room config room_id;

      (* Join the new room *)
      let join_result = join config ~agent_name:agent_type ~capabilities:[] () in

      (* Extract nickname from join result (format: "  Nickname: xxx\n...") *)
      let nickname =
        try
          let prefix = "  Nickname: " in
          let start_idx =
            let idx = ref 0 in
            while !idx < String.length join_result - String.length prefix &&
                  String.sub join_result !idx (String.length prefix) <> prefix do
              incr idx
            done;
            !idx + String.length prefix
          in
          let end_idx = String.index_from join_result start_idx '\n' in
          String.sub join_result start_idx (end_idx - start_idx)
        with _ -> agent_type ^ "-unknown"
      in

      `Assoc [
        ("previous_room", match previous_room with Some r -> `String r | None -> `Null);
        ("current_room", `String room_id);
        ("nickname", `String nickname);
        ("message", `String (Printf.sprintf "✅ Entered room '%s' as %s" room_id nickname));
      ]
    end
  end
