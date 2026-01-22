(** MASC Execution Memory - Task Execution Tracking

    Tracks task execution process and artifacts:
    - Plan: What we intended to do
    - Notes: Running log of actions/observations
    - Deliverable: Final output/result

    Storage: .masc/runs/{task_id}/
*)

open Lwt.Syntax

(** Run record - execution state for a task *)
type run_record = {
  task_id: string;
  agent_name: string;
  started_at: float;
  updated_at: float;
  plan: string;
  notes: string list;  (* Append-only log *)
  deliverable: string option;
  status: run_status;
}

and run_status =
  | Running
  | Completed
  | Failed of string
  | Paused

let status_to_string = function
  | Running -> "running"
  | Completed -> "completed"
  | Failed msg -> Printf.sprintf "failed: %s" msg
  | Paused -> "paused"

let status_of_string s =
  if s = "running" then Running
  else if s = "completed" then Completed
  else if s = "paused" then Paused
  else if String.length s > 8 && String.sub s 0 8 = "failed: " then
    Failed (String.sub s 8 (String.length s - 8))
  else Running

(** Create new run record *)
let create_run ~task_id ~agent_name : run_record =
  let now = Unix.gettimeofday () in
  {
    task_id;
    agent_name;
    started_at = now;
    updated_at = now;
    plan = "";
    notes = [];
    deliverable = None;
    status = Running;
  }

(** Get runs directory *)
let runs_dir (config : Room_utils.config) =
  Filename.concat config.base_path ".masc/runs"

(** Get specific run directory *)
let run_dir config task_id =
  Filename.concat (runs_dir config) task_id

(** Ensure runs directory exists *)
let ensure_runs_dir config =
  let dir = runs_dir config in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755

(** Ensure specific run directory exists *)
let ensure_run_dir config task_id =
  ensure_runs_dir config;
  let dir = run_dir config task_id in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755

(** Convert run to JSON *)
let run_to_json (run : run_record) : Yojson.Safe.t =
  `Assoc [
    ("task_id", `String run.task_id);
    ("agent_name", `String run.agent_name);
    ("started_at", `Float run.started_at);
    ("updated_at", `Float run.updated_at);
    ("plan", `String run.plan);
    ("notes", `List (List.map (fun n -> `String n) run.notes));
    ("deliverable", match run.deliverable with
      | Some d -> `String d
      | None -> `Null);
    ("status", `String (status_to_string run.status));
  ]

(** Convert JSON to run *)
let run_of_json (json : Yojson.Safe.t) : run_record option =
  let open Yojson.Safe.Util in
  try
    let task_id = json |> member "task_id" |> to_string in
    let agent_name = json |> member "agent_name" |> to_string in
    let started_at = json |> member "started_at" |> to_float in
    let updated_at = json |> member "updated_at" |> to_float in
    let plan = json |> member "plan" |> to_string in
    let notes = json |> member "notes" |> to_list |> List.map to_string in
    let deliverable =
      match json |> member "deliverable" with
      | `Null -> None
      | `String s -> Some s
      | _ -> None
    in
    let status = json |> member "status" |> to_string |> status_of_string in
    Some {
      task_id; agent_name; started_at; updated_at;
      plan; notes; deliverable; status;
    }
  with _ -> None

(** Save run metadata *)
let save_run config (run : run_record) : (unit, string) result Lwt.t =
  ensure_run_dir config run.task_id;
  let meta_path = Filename.concat (run_dir config run.task_id) "meta.json" in
  let json = run_to_json run in
  let content = Yojson.Safe.pretty_to_string json in
  try
    let oc = open_out meta_path in
    output_string oc content;
    close_out oc;
    Lwt.return (Ok ())
  with e ->
    Lwt.return (Error (Printexc.to_string e))

(** Load run metadata *)
let load_run config task_id : (run_record, string) result Lwt.t =
  let meta_path = Filename.concat (run_dir config task_id) "meta.json" in
  if not (Sys.file_exists meta_path) then
    Lwt.return (Error (Printf.sprintf "Run not found: %s" task_id))
  else
    try
      let ic = open_in meta_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      match run_of_json json with
      | Some run -> Lwt.return (Ok run)
      | None -> Lwt.return (Error "Failed to parse run metadata")
    with e ->
      Lwt.return (Error (Printexc.to_string e))

(** Initialize a new run *)
let init_run config ~task_id ~agent_name : (run_record, string) result Lwt.t =
  let run = create_run ~task_id ~agent_name in
  let* result = save_run config run in
  match result with
  | Ok () -> Lwt.return (Ok run)
  | Error e -> Lwt.return (Error e)

(** Add a note to the run *)
let add_note config ~task_id ~note : (run_record, string) result Lwt.t =
  let* result = load_run config task_id in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok run ->
      let timestamp = Unix.gettimeofday () in
      let time_str =
        let tm = Unix.localtime timestamp in
        Printf.sprintf "[%02d:%02d:%02d]" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      in
      let timestamped_note = Printf.sprintf "%s %s" time_str note in
      let updated = {
        run with
        notes = run.notes @ [timestamped_note];
        updated_at = timestamp;
      } in
      let* save_result = save_run config updated in
      match save_result with
      | Ok () -> Lwt.return (Ok updated)
      | Error e -> Lwt.return (Error e)

(** Set the plan *)
let set_plan config ~task_id ~plan : (run_record, string) result Lwt.t =
  let* result = load_run config task_id in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok run ->
      let updated = {
        run with
        plan;
        updated_at = Unix.gettimeofday ();
      } in
      let* save_result = save_run config updated in
      match save_result with
      | Ok () -> Lwt.return (Ok updated)
      | Error e -> Lwt.return (Error e)

(** Set the deliverable *)
let set_deliverable config ~task_id ~deliverable : (run_record, string) result Lwt.t =
  let* result = load_run config task_id in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok run ->
      let updated = {
        run with
        deliverable = Some deliverable;
        status = Completed;
        updated_at = Unix.gettimeofday ();
      } in
      let* save_result = save_run config updated in
      match save_result with
      | Ok () -> Lwt.return (Ok updated)
      | Error e -> Lwt.return (Error e)

(** Update run status *)
let set_status config ~task_id ~status : (run_record, string) result Lwt.t =
  let* result = load_run config task_id in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok run ->
      let updated = {
        run with
        status;
        updated_at = Unix.gettimeofday ();
      } in
      let* save_result = save_run config updated in
      match save_result with
      | Ok () -> Lwt.return (Ok updated)
      | Error e -> Lwt.return (Error e)

(** List all runs *)
let list_runs config : run_record list Lwt.t =
  let dir = runs_dir config in
  if not (Sys.file_exists dir) then
    Lwt.return []
  else
    let entries = Sys.readdir dir |> Array.to_list in
    let* runs = Lwt_list.filter_map_s (fun task_id ->
      let* result = load_run config task_id in
      match result with
      | Ok run -> Lwt.return (Some run)
      | Error _ -> Lwt.return None
    ) entries in
    Lwt.return runs

(** Format run as markdown *)
let format_as_markdown (run : run_record) : string =
  let started =
    let tm = Unix.localtime run.started_at in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in
  let duration = Unix.gettimeofday () -. run.started_at in
  let duration_str =
    if duration < 60.0 then Printf.sprintf "%.0fs" duration
    else if duration < 3600.0 then Printf.sprintf "%.1fm" (duration /. 60.0)
    else Printf.sprintf "%.1fh" (duration /. 3600.0)
  in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "# 📋 Run: %s\n\n" run.task_id);
  Buffer.add_string buf "## Meta\n";
  Buffer.add_string buf (Printf.sprintf "- **Agent**: %s\n" run.agent_name);
  Buffer.add_string buf (Printf.sprintf "- **Started**: %s\n" started);
  Buffer.add_string buf (Printf.sprintf "- **Duration**: %s\n" duration_str);
  Buffer.add_string buf (Printf.sprintf "- **Status**: %s\n" (status_to_string run.status));
  Buffer.add_string buf "\n";

  if run.plan <> "" then begin
    Buffer.add_string buf "## 📝 Plan\n\n";
    Buffer.add_string buf run.plan;
    Buffer.add_string buf "\n\n"
  end;

  if run.notes <> [] then begin
    Buffer.add_string buf "## 📓 Notes\n\n";
    List.iter (fun note ->
      Buffer.add_string buf (Printf.sprintf "- %s\n" note)
    ) run.notes;
    Buffer.add_string buf "\n"
  end;

  (match run.deliverable with
   | Some d ->
       Buffer.add_string buf "## ✅ Deliverable\n\n";
       Buffer.add_string buf d;
       Buffer.add_string buf "\n"
   | None -> ());

  Buffer.contents buf
