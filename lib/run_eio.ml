(** Execution Memory (Run) - Track task runs in .masc/runs/{task_id}

    Pure synchronous operations.
    Stores:
    - run.json (metadata)
    - plan.md
    - deliverable.md
    - log.jsonl (append-only)
*)

open Room_utils

(** Run metadata *)
type run_record = {
  task_id: string;
  agent_name: string option;
  plan: string;
  deliverable: string;
  created_at: string;
  updated_at: string;
}

(** Log entry *)
type log_entry = {
  timestamp: string;
  note: string;
}

let run_record_to_json (r : run_record) : Yojson.Safe.t =
  `Assoc [
    ("task_id", `String r.task_id);
    ("agent_name", match r.agent_name with Some a -> `String a | None -> `Null);
    ("plan", `String r.plan);
    ("deliverable", `String r.deliverable);
    ("created_at", `String r.created_at);
    ("updated_at", `String r.updated_at);
  ]

let run_record_of_json (json : Yojson.Safe.t) : run_record option =
  let open Yojson.Safe.Util in
  try
    let task_id = json |> member "task_id" |> to_string in
    let agent_name = json |> member "agent_name" |> to_string_option in
    let plan = json |> member "plan" |> to_string_option |> Option.value ~default:"" in
    let deliverable = json |> member "deliverable" |> to_string_option |> Option.value ~default:"" in
    let created_at = json |> member "created_at" |> to_string in
    let updated_at = json |> member "updated_at" |> to_string in
    Some { task_id; agent_name; plan; deliverable; created_at; updated_at }
  with Yojson.Safe.Util.Type_error (msg, _) ->
    Printf.eprintf "[WARN] run_of_json type error: %s\n%!" msg;
    None

let log_entry_to_json (e : log_entry) : Yojson.Safe.t =
  `Assoc [
    ("timestamp", `String e.timestamp);
    ("note", `String e.note);
  ]

let log_entry_of_json (json : Yojson.Safe.t) : log_entry option =
  let open Yojson.Safe.Util in
  try
    let timestamp = json |> member "timestamp" |> to_string in
    let note = json |> member "note" |> to_string in
    Some { timestamp; note }
  with Yojson.Safe.Util.Type_error (msg, _) ->
    Printf.eprintf "[WARN] log_entry_of_json type error: %s\n%!" msg;
    None

let runs_dir (config : config) =
  Filename.concat config.base_path ".masc/runs"

let run_dir (config : config) task_id =
  Filename.concat (runs_dir config) task_id

let run_json_path config task_id =
  Filename.concat (run_dir config task_id) "run.json"

let plan_path config task_id =
  Filename.concat (run_dir config task_id) "plan.md"

let deliverable_path config task_id =
  Filename.concat (run_dir config task_id) "deliverable.md"

let log_path config task_id =
  Filename.concat (run_dir config task_id) "log.jsonl"

let ensure_run_dir config task_id =
  let dir = run_dir config task_id in
  if not (Sys.file_exists dir) then mkdir_p dir

let now_iso () = Types.now_iso ()

let read_text_file path =
  if Sys.file_exists path then
    In_channel.with_open_text path In_channel.input_all
  else
    ""

let write_text_file path content =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_text path (fun oc ->
    Out_channel.output_string oc content
  )

let write_run config (run : run_record) =
  let path = run_json_path config run.task_id in
  write_json config path (run_record_to_json run)

let read_run config task_id : (run_record, string) result =
  let path = run_json_path config task_id in
  if not (path_exists config path) then
    Error (Printf.sprintf "Run not found for task %s" task_id)
  else
    match run_record_of_json (read_json config path) with
    | Some r -> Ok r
    | None -> Error "Failed to parse run.json"

(** Initialize run for task *)
let init config ~task_id ~agent_name : (run_record, string) result =
  try
    ensure_initialized config;
    ensure_run_dir config task_id;
    let created_at = now_iso () in
    let run = {
      task_id;
      agent_name;
      plan = "";
      deliverable = "";
      created_at;
      updated_at = created_at;
    } in
    (* Create default files *)
    let plan_file = plan_path config task_id in
    if not (path_exists config plan_file) then
      write_text_file plan_file "# Run Plan\n\n";
    let deliverable_file = deliverable_path config task_id in
    if not (path_exists config deliverable_file) then
      write_text_file deliverable_file "";
    let log_file = log_path config task_id in
    if not (path_exists config log_file) then begin
      mkdir_p (Filename.dirname log_file);
      Out_channel.with_open_text log_file (fun oc -> Out_channel.output_string oc "")
    end;
    write_run config run;
    Ok run
  with e -> Error (Printexc.to_string e)

(** Update plan *)
let update_plan config ~task_id ~content : (run_record, string) result =
  try
    match read_run config task_id with
    | Error e -> Error e
    | Ok run ->
        let updated = { run with plan = content; updated_at = now_iso () } in
        let path = plan_path config task_id in
        write_text_file path content;
        write_run config updated;
        Ok updated
  with e -> Error (Printexc.to_string e)

(** Append log entry *)
let append_log config ~task_id ~note : (log_entry, string) result =
  try
    ensure_initialized config;
    ensure_run_dir config task_id;
    let entry = { timestamp = now_iso (); note } in
    let file = log_path config task_id in
    let line = Yojson.Safe.to_string (log_entry_to_json entry) ^ "\n" in
    with_file_lock config file (fun () ->
      let oc = open_out_gen [Open_creat; Open_append; Open_wronly] 0o600 file in
      output_string oc line;
      close_out oc
    );
    Ok entry
  with e -> Error (Printexc.to_string e)

(** Set deliverable *)
let set_deliverable config ~task_id ~content : (run_record, string) result =
  try
    match read_run config task_id with
    | Error e -> Error e
    | Ok run ->
        let updated = { run with deliverable = content; updated_at = now_iso () } in
        let path = deliverable_path config task_id in
        write_text_file path content;
        write_run config updated;
        Ok updated
  with e -> Error (Printexc.to_string e)

(** Read logs (optionally tail N) *)
let read_logs config ~task_id ?limit () : log_entry list =
  let file = log_path config task_id in
  if not (Sys.file_exists file) then []
  else
    let content = In_channel.with_open_text file In_channel.input_all in
    let lines = String.split_on_char '\n' content |> List.filter (fun s -> String.trim s <> "") in
    let entries = List.filter_map (fun line ->
      match Safe_ops.parse_json_safe ~context:"run_log" line with
      | Ok json -> log_entry_of_json json
      | Error _ -> None
    ) lines in
    match limit with
    | None -> entries
    | Some n ->
        let total = List.length entries in
        if total <= n then entries else
          let start = total - n in
          entries |> List.mapi (fun i e -> (i, e)) |> List.filter (fun (i, _) -> i >= start)
          |> List.map snd

(** Get run details *)
let get config ~task_id : (Yojson.Safe.t, string) result =
  match read_run config task_id with
  | Error e -> Error e
  | Ok run ->
      let plan_content =
        let text = read_text_file (plan_path config task_id) in
        if text = "" then run.plan else text
      in
      let deliverable_content =
        let text = read_text_file (deliverable_path config task_id) in
        if text = "" then run.deliverable else text
      in
      let logs = read_logs config ~task_id ~limit:50 () in
      let json = `Assoc [
        ("run", run_record_to_json run);
        ("plan", `String plan_content);
        ("deliverable", `String deliverable_content);
        ("logs", `List (List.map log_entry_to_json logs));
        ("log_count", `Int (List.length logs));
      ] in
      Ok json

(** List runs *)
let list config : Yojson.Safe.t =
  let dir = runs_dir config in
  if not (Sys.file_exists dir) then
    `Assoc [("count", `Int 0); ("runs", `List [])]
  else
    let entries = Sys.readdir dir |> Array.to_list in
    let runs = List.filter_map (fun task_id ->
      let path = run_json_path config task_id in
      if path_exists config path then
        run_record_of_json (read_json config path)
      else None
    ) entries in
    `Assoc [
      ("count", `Int (List.length runs));
      ("runs", `List (List.map run_record_to_json runs));
    ]
