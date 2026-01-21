(** MASC Telemetry - Event Tracking and Analytics

    Based on research:
    - R2 (ACM TOSEM): Orchestration platform facilitation
    - R10 (Frontiers): Auto-scaling LLM-based MAS

    Tracks:
    - Agent lifecycle events
    - Task progress and completion
    - Handoff triggers
    - Error occurrences

    Storage: .masc/telemetry.jsonl (append-only log)
    Export: Prometheus-compatible metrics
*)

open Lwt.Syntax

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

(* ============================================ *)
(* File paths                                   *)
(* ============================================ *)

let telemetry_file config =
  Filename.concat (Room_utils.masc_dir config) "telemetry.jsonl"

let ensure_masc_dir config =
  let dir = Room_utils.masc_dir config in
  if not (Sys.file_exists dir) then
    Room_utils.mkdir_p dir

(* ============================================ *)
(* Event serialization                          *)
(* ============================================ *)

let event_type_name = function
  | Agent_joined _ -> "agent_joined"
  | Agent_left _ -> "agent_left"
  | Task_started _ -> "task_started"
  | Task_completed _ -> "task_completed"
  | Handoff_triggered _ -> "handoff_triggered"
  | Error_occurred _ -> "error_occurred"
  | Tool_called _ -> "tool_called"

let event_to_json event =
  let record = {
    timestamp = Unix.gettimeofday ();
    event;
  } in
  Yojson.Safe.to_string (event_record_to_yojson record)

(* ============================================ *)
(* Track events                                 *)
(* ============================================ *)

(** Track an event - appends to telemetry.jsonl *)
let track config event : unit Lwt.t =
  let* () = Lwt.return (ensure_masc_dir config) in
  let file = telemetry_file config in
  let json_line = event_to_json event ^ "\n" in

  (* Use file locking for concurrent safety *)
  let* fd = Lwt_unix.openfile file [Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT] 0o600 in
  let* () = Lwt_unix.lockf fd Lwt_unix.F_LOCK 0 in
  let* _ = Lwt_unix.write_string fd json_line 0 (String.length json_line) in
  let* () = Lwt_unix.lockf fd Lwt_unix.F_ULOCK 0 in
  let* () = Lwt_unix.close fd in

  (* Optional: POST to remote telemetry endpoint *)
  (match Sys.getenv_opt "MASC_TELEMETRY_URL" with
   | Some _url ->
       (* Future: HTTP POST to remote *)
       (* Lwt.async (fun () -> Http.post url json_line) *)
       ()
   | None -> ());

  Lwt.return_unit

(* ============================================ *)
(* Read events                                  *)
(* ============================================ *)

(** Read all events from telemetry file *)
let read_all_events config : event_record list Lwt.t =
  let file = telemetry_file config in
  if not (Sys.file_exists file) then
    Lwt.return []
  else
    let* content = Lwt_io.with_file ~mode:Lwt_io.Input file Lwt_io.read in
    let lines = String.split_on_char '\n' content
                |> List.filter (fun s -> String.trim s <> "") in
    let events = List.filter_map (fun line ->
      try
        let json = Yojson.Safe.from_string line in
        match event_record_of_yojson json with
        | Ok record -> Some record
        | Error e ->
            Printf.eprintf "[telemetry] Parse error: %s\n" e;
            None
      with exn ->
        Printf.eprintf "[telemetry] JSON error: %s\n" (Printexc.to_string exn);
        None
    ) lines in
    Lwt.return events

(** Read events since a timestamp *)
let read_events_since config ~since : event_record list Lwt.t =
  let* all = read_all_events config in
  Lwt.return (List.filter (fun r -> r.timestamp >= since) all)

(* ============================================ *)
(* Calculate metrics                            *)
(* ============================================ *)

(** Count active agents (joined but not left in last 24h) *)
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

(** Count tasks in progress (started but not completed) *)
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

(** Count completed tasks *)
let count_completed_tasks events =
  List.length (List.filter (fun r ->
    match r.event with
    | Task_completed _ -> true
    | _ -> false
  ) events)

(** Calculate average task duration in ms *)
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

(** Calculate handoff rate (handoffs / total task events) *)
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

(** Calculate error rate (errors / total events) *)
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
let get_metrics config : metrics Lwt.t =
  let now = Unix.gettimeofday () in
  let since_24h = now -. 86400.0 in
  let* events = read_events_since config ~since:since_24h in
  Lwt.return {
    active_agents = count_active_agents events;
    tasks_in_progress = count_tasks_in_progress events;
    tasks_completed_24h = count_completed_tasks events;
    avg_task_duration_ms = avg_duration events;
    handoff_rate = calculate_handoff_rate events;
    error_rate = calculate_error_rate events;
  }

(* ============================================ *)
(* Prometheus export                            *)
(* ============================================ *)

(** Export metrics in Prometheus format *)
let export_prometheus config : string Lwt.t =
  let* m = get_metrics config in
  Lwt.return (Printf.sprintf {|# HELP masc_active_agents Number of active agents
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
     m.avg_task_duration_ms m.handoff_rate m.error_rate)

(* ============================================ *)
(* Convenience tracking functions               *)
(* ============================================ *)

(** Track agent joining *)
let track_agent_joined config ~agent_id ?(capabilities=[]) () =
  track config (Agent_joined { agent_id; capabilities })

(** Track agent leaving *)
let track_agent_left config ~agent_id ~reason =
  track config (Agent_left { agent_id; reason })

(** Track task starting *)
let track_task_started config ~task_id ~agent_id =
  track config (Task_started { task_id; agent_id })

(** Track task completion *)
let track_task_completed config ~task_id ~duration_ms ~success =
  track config (Task_completed { task_id; duration_ms; success })

(** Track handoff *)
let track_handoff config ~from_agent ~to_agent ~reason =
  track config (Handoff_triggered { from_agent; to_agent; reason })

(** Track error *)
let track_error config ~code ~message ~context =
  track config (Error_occurred { code; message; context })

(** Track tool call *)
let track_tool_called config ~tool_name ~success ~duration_ms ?agent_id () =
  track config (Tool_called { tool_name; success; duration_ms; agent_id })

(** Sync version of track - for use in Eio context *)
let track_sync config event : unit =
  ensure_masc_dir config;
  let file = telemetry_file config in
  let json_line = event_to_json event ^ "\n" in
  let oc = open_out_gen [Open_append; Open_creat] 0o600 file in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc json_line;
    flush oc
  )

(** Sync version of track_tool_called - for use in Eio/mcp_server_eio.ml *)
let track_tool_called_sync config ~tool_name ~success ~duration_ms ?agent_id () =
  track_sync config (Tool_called { tool_name; success; duration_ms; agent_id })

(* ============================================ *)
(* Log rotation / cleanup                       *)
(* ============================================ *)

(** Rotate telemetry file (archive old entries) *)
let rotate config ~max_age_days : unit Lwt.t =
  let now = Unix.gettimeofday () in
  let cutoff = now -. (float_of_int max_age_days *. 86400.0) in
  let* all = read_all_events config in
  let recent = List.filter (fun r -> r.timestamp >= cutoff) all in

  (* Rewrite file with only recent events *)
  let file = telemetry_file config in
  let tmp_file = file ^ ".tmp" in
  let* () =
    Lwt_io.with_file ~mode:Lwt_io.Output tmp_file (fun oc ->
      Lwt_list.iter_s (fun record ->
        let line = Yojson.Safe.to_string (event_record_to_yojson record) ^ "\n" in
        Lwt_io.write oc line
      ) recent
    )
  in
  let () = Unix.rename tmp_file file in
  Lwt.return_unit
