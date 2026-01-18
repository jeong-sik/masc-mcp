(** MASC Metrics Store - Agent Performance Tracking

    에이전트 성과 측정 데이터 저장:
    - Task completion rates
    - Response times
    - Error rates
    - Collaboration patterns (for Hebbian learning)

    Storage: .masc/metrics/{agent_name}/YYYY-MM.jsonl

    Design: Research-based (see RESEARCH-BASED-IMPROVEMENTS.md)
    - ACM TOSEM: Role-based performance tracking
    - EvoAgent: Fitness metrics for selection
*)

open Lwt.Syntax

(** Task completion metric *)
type task_metric = {
  id: string;               (* Unique metric ID *)
  agent_id: string;         (* Agent name: claude, gemini, codex *)
  task_id: string;          (* Task being measured *)
  started_at: float;        (* Unix timestamp *)
  completed_at: float option;  (* None if still in progress *)
  success: bool;            (* Task succeeded? *)
  error_message: string option;  (* Error if failed *)
  collaborators: string list;  (* Other agents involved - for Hebbian *)
  handoff_from: string option;  (* Previous agent if handoff *)
  handoff_to: string option;    (* Next agent if handoff out *)
} [@@deriving yojson, show]

(** Aggregated metrics for fitness calculation *)
type agent_metrics = {
  agent_id: string;
  period_start: float;      (* Start of measurement period *)
  period_end: float;        (* End of measurement period *)
  total_tasks: int;
  completed_tasks: int;
  failed_tasks: int;
  avg_completion_time_s: float;
  task_completion_rate: float;  (* 0.0-1.0 *)
  error_rate: float;            (* 0.0-1.0 *)
  handoff_success_rate: float;  (* 0.0-1.0 *)
  unique_collaborators: string list;
} [@@deriving yojson, show]

(** Config type alias for clarity *)
type config = Room_utils.config

(** Get metrics directory *)
let metrics_dir (config : config) =
  Filename.concat config.Room_utils.base_path ".masc/metrics"

(** Get agent-specific metrics directory *)
let agent_metrics_dir config agent_id =
  Filename.concat (metrics_dir config) agent_id

(** Ensure metrics directories exist *)
let ensure_metrics_dir config agent_id =
  let masc_dir = Filename.concat config.Room_utils.base_path ".masc" in
  let metrics = metrics_dir config in
  let agent_dir = agent_metrics_dir config agent_id in
  List.iter (fun dir ->
    if not (Sys.file_exists dir) then
      Unix.mkdir dir 0o755
  ) [masc_dir; metrics; agent_dir]

(** Get current month's file path *)
let current_month_file config agent_id =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  let filename = Printf.sprintf "%04d-%02d.jsonl"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) in
  Filename.concat (agent_metrics_dir config agent_id) filename

(** Generate unique metric ID *)
let generate_id () =
  let timestamp = Unix.gettimeofday () in
  let random = Random.int 100000 in
  Printf.sprintf "metric-%d-%05d" (int_of_float (timestamp *. 1000.)) random

(** Record a new task metric (with file locking for concurrent safety) *)
let record config (metric : task_metric) : unit Lwt.t =
  let* () = Lwt.return (ensure_metrics_dir config metric.agent_id) in
  let file = current_month_file config metric.agent_id in
  let json = task_metric_to_yojson metric in
  let line = Yojson.Safe.to_string json ^ "\n" in
  (* Use file locking to prevent concurrent write corruption *)
  (* Security: 0o600 - only owner can read/write metrics data *)
  let* fd = Lwt_unix.openfile file [Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT] 0o600 in
  let* () = Lwt_unix.lockf fd Lwt_unix.F_LOCK 0 in
  let* _ = Lwt_unix.write_string fd line 0 (String.length line) in
  let* () = Lwt_unix.lockf fd Lwt_unix.F_ULOCK 0 in
  let* () = Lwt_unix.close fd in
  Lwt.return_unit

(** Create a new task metric (helper) *)
let create_metric ~agent_id ~task_id ?(collaborators=[]) ?handoff_from () =
  {
    id = generate_id ();
    agent_id;
    task_id;
    started_at = Unix.gettimeofday ();
    completed_at = None;
    success = false;
    error_message = None;
    collaborators;
    handoff_from;
    handoff_to = None;
  }

(** Mark task as completed *)
let complete_metric metric ~success ?error_message ?handoff_to () =
  { metric with
    completed_at = Some (Unix.gettimeofday ());
    success;
    error_message;
    handoff_to;
  }

(** Read metrics from a file *)
let read_metrics_file file : task_metric list Lwt.t =
  if not (Sys.file_exists file) then
    Lwt.return []
  else
    let* content = Lwt_io.with_file ~mode:Lwt_io.Input file Lwt_io.read in
    let lines = String.split_on_char '\n' content
      |> List.filter (fun s -> String.trim s <> "") in
    let metrics = List.filter_map (fun line ->
      try
        let json = Yojson.Safe.from_string line in
        match task_metric_of_yojson json with
        | Ok m -> Some m
        | Error e ->
          let preview = if String.length line > 50 then String.sub line 0 50 ^ "..." else line in
          Printf.eprintf "[metrics_store] Failed to parse metric: %s (line: %s)\n" e preview;
          None
      with exn ->
        let preview = if String.length line > 50 then String.sub line 0 50 ^ "..." else line in
        Printf.eprintf "[metrics_store] JSON parse error: %s (line: %s)\n"
          (Printexc.to_string exn) preview;
        None
    ) lines in
    Lwt.return metrics

(** Get recent metrics for an agent *)
let get_recent config ~agent_id ~days : task_metric list Lwt.t =
  let now = Unix.gettimeofday () in
  let cutoff = now -. (float_of_int days *. 86400.0) in
  let dir = agent_metrics_dir config agent_id in
  if not (Sys.file_exists dir) then
    Lwt.return []
  else
    let files = Sys.readdir dir |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".jsonl")
      |> List.map (fun f -> Filename.concat dir f) in
    let* all_metrics = Lwt_list.map_s read_metrics_file files in
    let recent = List.flatten all_metrics
      |> List.filter (fun m -> m.started_at >= cutoff) in
    Lwt.return recent

(** Calculate aggregated metrics for fitness *)
let calculate_agent_metrics config ~agent_id ~days : agent_metrics option Lwt.t =
  let* metrics = get_recent config ~agent_id ~days in
  if List.length metrics = 0 then
    Lwt.return None
  else
    let now = Unix.gettimeofday () in
    let period_start = now -. (float_of_int days *. 86400.0) in
    let total = List.length metrics in
    let completed = List.filter (fun m -> Option.is_some m.completed_at) metrics in
    let successful = List.filter (fun m -> m.success) completed in
    let failed = List.filter (fun m -> not m.success) completed in

    (* Calculate average completion time *)
    let completion_times = List.filter_map (fun m ->
      match m.completed_at with
      | Some t -> Some (t -. m.started_at)
      | None -> None
    ) metrics in
    let avg_time = match completion_times with
      | [] -> 0.0
      | times ->
        let sum = List.fold_left (+.) 0.0 times in
        sum /. (float_of_int (List.length times)) in

    (* Calculate handoff success rate *)
    let handoffs = List.filter (fun m -> Option.is_some m.handoff_from || Option.is_some m.handoff_to) metrics in
    let successful_handoffs = List.filter (fun m -> m.success) handoffs in
    let handoff_rate = if List.length handoffs > 0 then
      float_of_int (List.length successful_handoffs) /. float_of_int (List.length handoffs)
    else 1.0 in  (* No handoffs = perfect handoff rate *)

    (* Unique collaborators *)
    let all_collaborators = List.flatten (List.map (fun m -> m.collaborators) metrics) in
    let unique_collabs = List.sort_uniq String.compare all_collaborators in

    Lwt.return (Some {
      agent_id;
      period_start;
      period_end = now;
      total_tasks = total;
      completed_tasks = List.length completed;
      failed_tasks = List.length failed;
      avg_completion_time_s = avg_time;
      task_completion_rate = float_of_int (List.length successful) /. float_of_int total;
      error_rate = float_of_int (List.length failed) /. float_of_int total;
      handoff_success_rate = handoff_rate;
      unique_collaborators = unique_collabs;
    })

(** Get all agents with metrics *)
let get_all_agents config : string list Lwt.t =
  let dir = metrics_dir config in
  if not (Sys.file_exists dir) then
    Lwt.return []
  else
    let entries = Sys.readdir dir |> Array.to_list in
    let agents = List.filter (fun e ->
      Sys.is_directory (Filename.concat dir e)
    ) entries in
    Lwt.return agents
