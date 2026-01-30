(** MASC Dashboard - Terminal-based Status Visualization

    Usage:
    - MCP: masc_dashboard
    - CLI: masc dashboard | watch -n 1 masc dashboard

    Shows:
    - Active agents (with zombie detection)
    - Task board (by status)
    - File locks
    - Recent broadcasts
    - Tempo status
    - Active worktrees
*)

(* ===== Constants ===== *)

(** Maximum path length before truncation *)
let max_path_length = 30

(** Maximum message content length before truncation *)
let max_message_length = 35

(** Maximum pending tasks to show *)
let max_pending_tasks = 5

(** Maximum recent messages to show *)
let max_recent_messages = 5

(** Minimum section border length *)
let min_border_length = 45

(* ===== Types ===== *)

(** Dashboard section *)
type section = {
  title: string;
  content: string list;
  empty_msg: string;
}

(** Format a section *)
let format_section (s : section) : string =
  let header = Printf.sprintf "== %s ==" s.title in
  let border_len = max min_border_length (String.length header + 4) in
  let top_border = header ^ String.make (border_len - String.length header) '=' in
  let bottom_border = String.make border_len '-' in
  let content =
    if List.length s.content = 0 then
      [Printf.sprintf "  %s" s.empty_msg]
    else
      List.map (fun line ->
        Printf.sprintf "  %s" line
      ) s.content
  in
  String.concat "\n" ([top_border] @ content @ [bottom_border])

(** Parse ISO timestamp to Unix time (UTC) *)
let parse_iso_timestamp (s : string) : float option =
  (* Format: 2025-01-09T12:00:00Z or 2025-01-09T12:00:00.123Z *)
  try
    let open Scanf in
    sscanf s "%d-%d-%dT%d:%d:%d" (fun y m d h min sec ->
      let tm = {
        Unix.tm_sec = sec; tm_min = min; tm_hour = h;
        tm_mday = d; tm_mon = m - 1; tm_year = y - 1900;
        tm_wday = 0; tm_yday = 0; tm_isdst = false
      } in
      (* Unix.mktime interprets tm as local time, but ISO 8601 'Z' means UTC.
         We need to adjust by the local timezone offset. *)
      let (local_t, _) = Unix.mktime tm in
      let utc_tm = Unix.gmtime local_t in
      let (utc_as_local, _) = Unix.mktime utc_tm in
      let tz_offset = local_t -. utc_as_local in
      Some (local_t -. tz_offset)
    )
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> None

(** Get agents section *)
let agents_section (config : Room_utils.config) : section =
  let agents = Room.get_agents_raw config in
  let now = Unix.gettimeofday () in
  let content = List.map (fun (a : Types.agent) ->
    let status_str = Types.agent_status_to_string a.status in
    let elapsed_str = match parse_iso_timestamp a.last_seen with
      | Some ts ->
          let elapsed = now -. ts in
          if elapsed < 60.0 then Printf.sprintf "%.0fs ago" elapsed
          else if elapsed < 3600.0 then Printf.sprintf "%.0fm ago" (elapsed /. 60.0)
          else Printf.sprintf "%.1fh ago" (elapsed /. 3600.0)
      | None -> a.last_seen
    in
    Printf.sprintf "[%s] %s (%s)" status_str a.name elapsed_str
  ) agents in
  { title = "Agents"; content; empty_msg = "(no agents)" }

(** Get tasks section *)
let tasks_section (config : Room_utils.config) : section =
  let tasks = Room.get_tasks_raw config in
  let in_progress = List.filter (fun t ->
    match t.Types.task_status with
    | Types.InProgress _ -> true
    | Types.Claimed _ -> true
    | _ -> false
  ) tasks in
  let pending = List.filter (fun t ->
    match t.Types.task_status with
    | Types.Todo -> true
    | _ -> false
  ) tasks in
  let content =
    (List.map (fun (t : Types.task) ->
      let agent = match t.task_status with
        | Types.InProgress { assignee; _ } -> assignee
        | Types.Claimed { assignee; _ } -> assignee
        | _ -> "?"
      in
      Printf.sprintf "[P%d] %s (@%s)" t.priority t.title agent
    ) in_progress) @
    (List.map (fun (t : Types.task) ->
      Printf.sprintf "[P%d] %s (pending)" t.priority t.title
    ) (List.filteri (fun i _ -> i < max_pending_tasks) pending))
  in
  let pending_more = List.length pending - max_pending_tasks in
  let content = if pending_more > 0 then
    content @ [Printf.sprintf "   ... +%d more pending" pending_more]
  else content in
  { title = "Tasks"; content; empty_msg = "(no tasks)" }

(** Truncate path to max_path_length with leading ellipsis *)
let truncate_path (path : string) : string =
  if String.length path > max_path_length then
    let suffix_len = max_path_length - 3 in (* 3 for "..." *)
    "..." ^ String.sub path (String.length path - suffix_len) suffix_len
  else path

(** Truncate message to max_message_length with trailing ellipsis *)
let truncate_message (msg : string) : string =
  if String.length msg > max_message_length then
    let prefix_len = max_message_length - 3 in (* 3 for "..." *)
    String.sub msg 0 prefix_len ^ "..."
  else msg

(** Check string suffix *)
let ends_with ~suffix s =
  let suffix_len = String.length suffix in
  let s_len = String.length s in
  s_len >= suffix_len &&
  String.sub s (s_len - suffix_len) suffix_len = suffix

let rec count_lock_files path =
  if not (Sys.file_exists path) then
    0
  else if Sys.is_directory path then
    Sys.readdir path
    |> Array.fold_left (fun acc entry ->
      let child = Filename.concat path entry in
      acc + count_lock_files child
    ) 0
  else if ends_with ~suffix:".flock" (Filename.basename path) then
    0
  else
    1

(** Get lock count across backends *)
let lock_count (config : Room_utils.config) : int =
  match config.backend with
  | Room_utils.FileSystem _ ->
      let locks_dir = Filename.concat (Room_utils.masc_dir config) "locks" in
      count_lock_files locks_dir
  | Room_utils.Memory _ | Room_utils.PostgresNative _ ->
      let prefix = Printf.sprintf "locks:%s:" (Room_utils.project_prefix config) in
      (match Room_utils.backend_list_keys config ~prefix with
       | Ok keys -> List.length keys
       | Error _ -> 0)

(** Get locks section *)
let locks_section (config : Room_utils.config) : section =
  let count = lock_count config in
  let content = [Printf.sprintf "%d active locks" count] in
  { title = "Locks"; content; empty_msg = "" }

(** Get messages section *)
let messages_section (config : Room_utils.config) : section =
  let messages = Room.get_messages_raw config ~limit:max_recent_messages ~since_seq:0 in
  let content = List.map (fun (m : Types.message) ->
    Printf.sprintf "%s: %s" m.from_agent (truncate_message m.content)
  ) messages in
  { title = "Recent Messages"; content; empty_msg = "(no messages)" }

(** Get tempo section *)
let tempo_section (config : Room_utils.config) : section =
  let state = Tempo.get_tempo config in
  let content = [Tempo.format_state state] in
  { title = "Tempo"; content; empty_msg = "" }

(** Parse worktree list JSON to extract worktrees.
    Tolerant of malformed entries - skips invalid items rather than failing. *)
let parse_worktrees (json : Yojson.Safe.t) : (string * string) list =
  let open Yojson.Safe.Util in
  match json |> member "worktrees" with
  | `List items ->
      List.filter_map (fun item ->
        try
          let worktree = item |> member "worktree" |> to_string in
          let branch = item |> member "branch" |> to_string in
          (* Skip main worktree (usually the repo root) *)
          if String.length branch > 0 && not (String.equal branch "HEAD") then
            Some (branch, worktree)
          else None
        with
        | Type_error _ -> None  (* JSON field missing or wrong type *)
        | _ -> None  (* Other unexpected errors *)
      ) items
  | `Null -> []  (* No worktrees key - git worktree not used *)
  | _ -> []  (* Unexpected JSON structure *)

(** Get worktrees section *)
let worktrees_section (config : Room_utils.config) : section =
  let json = Room.worktree_list config in
  let worktrees = parse_worktrees json in
  let content = List.map (fun ((branch, path) : string * string) ->
    Printf.sprintf "%s -> %s" branch (truncate_path path)
  ) worktrees in
  { title = "Worktrees"; content; empty_msg = "(no worktrees)" }

(** Generate full dashboard *)
let generate (config : Room_utils.config) : string =
  let now = Unix.gettimeofday () in
  let timestamp =
    let tm = Unix.localtime now in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in
  let header = Printf.sprintf "========================================\n   MASC Dashboard   %s\n========================================" timestamp in
  let sections = [
    agents_section config;
    tasks_section config;
    locks_section config;
    messages_section config;
    tempo_section config;
    worktrees_section config;
  ] in
  let section_strs = List.map format_section sections in
  String.concat "\n\n" ([header] @ section_strs @ ["\nRefresh: watch -n 1 masc dashboard"])

(** Compact dashboard (single line per section) *)
let generate_compact (config : Room_utils.config) : string =
  let agents = Room.get_agents_raw config in
  let tasks = Room.get_tasks_raw config in
  let tempo = Tempo.get_tempo config in
  let locks = lock_count config in
  let pending = List.filter (fun t -> t.Types.task_status = Types.Todo) tasks in
  let active = List.filter (fun t ->
    match t.Types.task_status with
    | Types.InProgress _ -> true
    | Types.Claimed _ -> true
    | _ -> false
  ) tasks in
  Printf.sprintf "Agents: %d | Tasks: %d active, %d pending | Locks: %d | Tempo: %.0fs"
    (List.length agents)
    (List.length active)
    (List.length pending)
    locks
    tempo.Tempo.current_interval_s
