(** MASC Telemetry - Event Tracking and Analytics (Eio Native)

    Tracks:
    - Agent lifecycle events
    - Task progress and completion
    - Handoff triggers
    - Error occurrences

    Storage: .masc/telemetry.jsonl (append-only log)
*)


(** Config type alias *)
type config = Room_utils.config

(** Telemetry event types *)
type event =
  | Agent_joined of { agent_id: string; capabilities: string list }
  | Agent_left of { agent_id: string; reason: string }
  | Task_started of { task_id: string; agent_id: string }
  | Task_completed of { task_id: string; duration_ms: int; success: bool }
  | Handoff_triggered of { from_agent: string; to_agent: string; reason: string }
  | Error_occurred of { code: string; message: string; context: string }
  | Tool_called of { tool_name: string; success: bool; duration_ms: int; agent_id: string option }
[@@deriving yojson, show]

(** Timestamped event record for storage *)
type event_record = {
  timestamp: float;
  event: event;
} [@@deriving yojson, show]

(** Aggregated metrics *)
type metrics = {
  active_agents: int;
  tasks_in_progress: int;
  tasks_completed_24h: int;
  avg_task_duration_ms: float;
  handoff_rate: float;
  error_rate: float;
} [@@deriving yojson, show]

let telemetry_file config =
  Filename.concat (Room_utils.masc_dir config) "telemetry.jsonl"

let ensure_masc_dir fs config =
  let dir = Room_utils.masc_dir config in
  let path = Eio.Path.(fs / dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path

let event_to_json event =
  let record = {
    timestamp = Unix.gettimeofday ();
    event;
  } in
  Yojson.Safe.to_string (event_record_to_yojson record)

(** Track an event - appends to telemetry.jsonl *)
let track ~fs config event : unit =
  ensure_masc_dir fs config;
  let file = telemetry_file config in
  let json_line = event_to_json event ^ "\n" in

  (* Use Room_utils.with_file_lock for concurrent safety *)
  Room_utils.with_file_lock config file (fun () ->
    let path = Eio.Path.(fs / file) in
    Eio.Path.save ~append:true ~create:(`If_missing 0o600) path json_line
  )

(** Read all events from telemetry file *)
let read_all_events ~fs config : event_record list =
  let file = telemetry_file config in
  let path = Eio.Path.(fs / file) in
  try
    let content = Eio.Path.load path in
    let lines = String.split_on_char '\n' content
                |> List.filter (fun s -> String.trim s <> "") in
    List.filter_map (fun line ->
      try
        let json = Yojson.Safe.from_string line in
        match event_record_of_yojson json with
        | Ok record -> Some record
        | Error _ -> None
      with Yojson.Json_error _ -> None
    ) lines
  with Sys_error _ | Eio.Io _ -> []

(** Read events since a timestamp *)
let read_events_since ~fs config ~since : event_record list =
  let all = read_all_events ~fs config in
  List.filter (fun r -> r.timestamp >= since) all

(** Metrics calculation functions (pure) *)
let count_active_agents events =
  let joined = List.filter_map (fun r ->
    match r.event with
    | Agent_joined { agent_id; _ } -> Some agent_id
    | _ -> None
  ) events in
  let left = List.filter_map (fun r ->
    match r.event with
    | Agent_left { agent_id; _ } -> Some agent_id
    | _ -> None
  ) events in
  let active = List.filter (fun id -> not (List.mem id left)) joined in
  List.length (List.sort_uniq String.compare active)

let count_tasks_in_progress events =
  let started = List.filter_map (fun r ->
    match r.event with
    | Task_started { task_id; _ } -> Some task_id
    | _ -> None
  ) events in
  let completed = List.filter_map (fun r ->
    match r.event with
    | Task_completed { task_id; _ } -> Some task_id
    | _ -> None
  ) events in
  let in_progress = List.filter (fun id -> not (List.mem id completed)) started in
  List.length (List.sort_uniq String.compare in_progress)

let count_completed_tasks events =
  List.length (List.filter (fun r ->
    match r.event with
    | Task_completed _ -> true
    | _ -> false
  ) events)

let avg_duration events =
  let durations = List.filter_map (fun r ->
    match r.event with
    | Task_completed { duration_ms; _ } -> Some (float_of_int duration_ms)
    | _ -> None
  ) events in
  match durations with
  | [] -> 0.0
  | times ->
      let sum = List.fold_left (+.) 0.0 times in
      sum /. float_of_int (List.length times)

let calculate_handoff_rate events =
  let handoffs = List.length (List.filter (fun r ->
    match r.event with
    | Handoff_triggered _ -> true
    | _ -> false
  ) events) in
  let task_events = List.length (List.filter (fun r ->
    match r.event with
    | Task_started _ | Task_completed _ -> true
    | _ -> false
  ) events) in
  if task_events = 0 then 0.0
  else float_of_int handoffs /. float_of_int task_events

let calculate_error_rate events =
  let errors = List.length (List.filter (fun r ->
    match r.event with
    | Error_occurred _ -> true
    | _ -> false
  ) events) in
  let total = List.length events in
  if total = 0 then 0.0
  else float_of_int errors /. float_of_int total

(** Get aggregated metrics for last 24 hours *)
let get_metrics ~fs config : metrics =
  let now = Unix.gettimeofday () in
  let since_24h = now -. 86400.0 in
  let events = read_events_since ~fs config ~since:since_24h in
  {
    active_agents = count_active_agents events;
    tasks_in_progress = count_tasks_in_progress events;
    tasks_completed_24h = count_completed_tasks events;
    avg_task_duration_ms = avg_duration events;
    handoff_rate = calculate_handoff_rate events;
    error_rate = calculate_error_rate events;
  }

(** Export metrics in Prometheus format *)
let export_prometheus ~fs config : string =
  let m = get_metrics ~fs config in
  Printf.sprintf {|# HELP masc_active_agents Number of active agents
# TYPE masc_active_agents gauge
masc_active_agents %d

# HELP masc_tasks_in_progress Tasks currently in progress
# TYPE masc_tasks_in_progress gauge
masc_tasks_in_progress %d

# HELP masc_tasks_completed Tasks completed in last 24h
# TYPE masc_tasks_completed counter
masc_tasks_completed %d

# HELP masc_avg_task_duration Average task duration in ms
# TYPE masc_avg_task_duration gauge
masc_avg_task_duration %.2f

# HELP masc_handoff_rate Handoff rate (0-1)
# TYPE masc_handoff_rate gauge
masc_handoff_rate %.4f

# HELP masc_error_rate Error rate (0-1)
# TYPE masc_error_rate gauge
masc_error_rate %.4f
|} m.active_agents m.tasks_in_progress m.tasks_completed_24h
     m.avg_task_duration_ms m.handoff_rate m.error_rate

(** Convenience tracking functions *)
let track_agent_joined ~fs config ~agent_id ?(capabilities=[]) () =
  track ~fs config (Agent_joined { agent_id; capabilities })

let track_agent_left ~fs config ~agent_id ~reason =
  track ~fs config (Agent_left { agent_id; reason })

let track_task_started ~fs config ~task_id ~agent_id =
  track ~fs config (Task_started { task_id; agent_id })

let track_task_completed ~fs config ~task_id ~duration_ms ~success =
  track ~fs config (Task_completed { task_id; duration_ms; success })

let track_handoff ~fs config ~from_agent ~to_agent ~reason =
  track ~fs config (Handoff_triggered { from_agent; to_agent; reason })

let track_error ~fs config ~code ~message ~context =
  track ~fs config (Error_occurred { code; message; context })

let track_tool_called ~fs config ~tool_name ~success ~duration_ms ?agent_id () =
  track ~fs config (Tool_called { tool_name; success; duration_ms; agent_id })

(** Rotate telemetry file *)
let rotate ~fs config ~max_age_days : unit =
  let now = Unix.gettimeofday () in
  let cutoff = now -. (float_of_int max_age_days *. 86400.0) in
  let all = read_all_events ~fs config in
  let recent = List.filter (fun r -> r.timestamp >= cutoff) all in

  let file = telemetry_file config in
  let tmp_file = file ^ ".tmp" in
  let content = 
    List.map (fun record -> Yojson.Safe.to_string (event_record_to_yojson record)) recent
    |> String.concat "\n"
    |> (fun s -> if s = "" then "" else s ^ "\n")
  in
  let tmp_path = Eio.Path.(fs / tmp_file) in
  Eio.Path.save ~create:(`Or_truncate 0o600) tmp_path content;
  Unix.rename tmp_file file
