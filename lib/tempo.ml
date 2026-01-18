(** MASC Tempo Control - Dynamic Orchestrator Interval

    Cluster tempo control for adaptive orchestration:
    - Urgent tasks (priority 1-2) → Fast tempo (60s)
    - Normal tasks (priority 3) → Normal tempo (300s)
    - Idle (no tasks) → Slow tempo (600s)

    Storage: .masc/tempo.json
*)

(** Tempo configuration *)
type tempo_config = {
  min_interval_s: float;      (* Minimum interval (fast tempo) *)
  max_interval_s: float;      (* Maximum interval (slow tempo) *)
  default_interval_s: float;  (* Default interval *)
  adaptive: bool;             (* Enable adaptive tempo *)
}

(** Current tempo state *)
type tempo_state = {
  current_interval_s: float;
  last_adjusted: float;
  reason: string;
}

(** Default configuration *)
let default_config = {
  min_interval_s = 60.0;      (* 1 minute for urgent *)
  max_interval_s = 600.0;     (* 10 minutes for idle *)
  default_interval_s = 300.0; (* 5 minutes default *)
  adaptive = true;
}

(** Get tempo file path *)
let tempo_file (config : Room_utils.config) =
  Filename.concat config.base_path ".masc/tempo.json"

(** State to JSON *)
let state_to_json (state : tempo_state) : Yojson.Safe.t =
  `Assoc [
    ("current_interval_s", `Float state.current_interval_s);
    ("last_adjusted", `Float state.last_adjusted);
    ("reason", `String state.reason);
  ]

(** State from JSON *)
let state_of_json (json : Yojson.Safe.t) : tempo_state option =
  let open Yojson.Safe.Util in
  try
    let current_interval_s = json |> member "current_interval_s" |> to_float in
    let last_adjusted = json |> member "last_adjusted" |> to_float in
    let reason = json |> member "reason" |> to_string in
    Some { current_interval_s; last_adjusted; reason }
  with _ -> None

(** Load current tempo state *)
let load_state (config : Room_utils.config) : tempo_state =
  let path = tempo_file config in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      match state_of_json json with
      | Some state -> state
      | None ->
        { current_interval_s = default_config.default_interval_s;
          last_adjusted = 0.0;
          reason = "default" }
    with _ ->
      { current_interval_s = default_config.default_interval_s;
        last_adjusted = 0.0;
        reason = "default" }
  else
    { current_interval_s = default_config.default_interval_s;
      last_adjusted = 0.0;
      reason = "default" }

(** Save tempo state *)
let save_state (config : Room_utils.config) (state : tempo_state) : unit =
  let path = tempo_file config in
  let masc_dir = Filename.concat config.base_path ".masc" in
  if not (Sys.file_exists masc_dir) then
    Unix.mkdir masc_dir 0o755;
  let json = state_to_json state in
  let content = Yojson.Safe.pretty_to_string json in
  let oc = open_out path in
  output_string oc content;
  close_out oc

(** Set tempo manually *)
let set_tempo (config : Room_utils.config) ~interval_s ~reason : tempo_state =
  let clamped =
    max default_config.min_interval_s
      (min default_config.max_interval_s interval_s)
  in
  let state = {
    current_interval_s = clamped;
    last_adjusted = Unix.gettimeofday ();
    reason;
  } in
  save_state config state;
  state

(** Get current tempo *)
let get_tempo (config : Room_utils.config) : tempo_state =
  load_state config

(** Check if task is pending (not done/cancelled) *)
let is_pending_task (task : Types.task) : bool =
  match task.task_status with
  | Types.Todo -> true
  | Types.Claimed _ -> true
  | Types.InProgress _ -> true
  | Types.Done _ -> false
  | Types.Cancelled _ -> false

(** Calculate adaptive tempo based on task urgency *)
let calculate_adaptive_tempo (tasks : Types.task list) : float * string =
  if List.length tasks = 0 then
    (default_config.max_interval_s, "idle - no pending tasks")
  else
    let urgent_count = List.filter (fun t -> t.Types.priority <= 2) tasks |> List.length in
    let high_count = List.filter (fun t -> t.Types.priority = 3) tasks |> List.length in
    if urgent_count > 0 then
      (default_config.min_interval_s,
       Printf.sprintf "fast - %d urgent task(s)" urgent_count)
    else if high_count > 0 then
      (default_config.default_interval_s,
       Printf.sprintf "normal - %d pending task(s)" high_count)
    else
      (default_config.max_interval_s,
       Printf.sprintf "slow - %d low priority task(s)" (List.length tasks))

(** Adjust tempo adaptively based on current tasks *)
let adjust_tempo (config : Room_utils.config) : tempo_state Lwt.t =
  let tasks = Room.get_tasks_raw config in
  let pending = List.filter is_pending_task tasks in
  let (interval, reason) = calculate_adaptive_tempo pending in
  let state = set_tempo config ~interval_s:interval ~reason in
  Lwt.return state

(** Format tempo state for display *)
let format_state (state : tempo_state) : string =
  let interval_str =
    if state.current_interval_s < 120.0 then
      Printf.sprintf "%.0fs" state.current_interval_s
    else
      Printf.sprintf "%.1fm" (state.current_interval_s /. 60.0)
  in
  let age =
    let elapsed = Unix.gettimeofday () -. state.last_adjusted in
    if elapsed < 60.0 then "just now"
    else if elapsed < 3600.0 then Printf.sprintf "%.0fm ago" (elapsed /. 60.0)
    else Printf.sprintf "%.1fh ago" (elapsed /. 3600.0)
  in
  Printf.sprintf "⏱️ Tempo: %s (%s, adjusted %s)" interval_str state.reason age

(** Reset tempo to default *)
let reset_tempo (config : Room_utils.config) : tempo_state =
  set_tempo config
    ~interval_s:default_config.default_interval_s
    ~reason:"reset to default"
