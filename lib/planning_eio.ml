(** Planning_eio - OCaml 5.x Pure Synchronous Planning Module

    Eio-native version of Planning module.
    Implements the planning-with-files pattern for structured AI collaboration:
    - task_plan.md: Main execution plan
    - notes.md: Observations, learnings, context
    - errors.md: Error tracking (PDCA Check phase)
    - deliverable.md: Final output/result

    All operations are pure synchronous - no monads, no async wrappers.
    Uses Fun.protect for resource cleanup and Unix.lockf for file locking.
*)

(** Error entry for failure tracking *)
type error_entry = {
  timestamp: string;
  error_type: string;     (* e.g., "build", "test", "runtime", "logic" *)
  message: string;
  context: string option; (* Optional context like file path, function name *)
  resolved: bool;
}

(** Planning context for a task *)
type planning_context = {
  task_id: string;
  task_plan: string;      (* Main plan content *)
  notes: string list;     (* List of notes/observations *)
  errors: error_entry list; (* List of errors/failures - PDCA Check phase *)
  deliverable: string;    (* Final deliverable *)
  created_at: string;
  updated_at: string;
}

(* ===== JSON Serialization ===== *)

let error_entry_to_yojson e =
  `Assoc [
    ("timestamp", `String e.timestamp);
    ("error_type", `String e.error_type);
    ("message", `String e.message);
    ("context", match e.context with Some c -> `String c | None -> `Null);
    ("resolved", `Bool e.resolved);
  ]

let error_entry_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let timestamp = json |> member "timestamp" |> to_string in
    let error_type = json |> member "error_type" |> to_string in
    let message = json |> member "message" |> to_string in
    let context = json |> member "context" |> to_string_option in
    let resolved = json |> member "resolved" |> to_bool in
    Ok { timestamp; error_type; message; context; resolved }
  with e -> Error (Printexc.to_string e)

let planning_context_to_yojson ctx =
  `Assoc [
    ("task_id", `String ctx.task_id);
    ("task_plan", `String ctx.task_plan);
    ("notes", `List (List.map (fun n -> `String n) ctx.notes));
    ("errors", `List (List.map error_entry_to_yojson ctx.errors));
    ("deliverable", `String ctx.deliverable);
    ("created_at", `String ctx.created_at);
    ("updated_at", `String ctx.updated_at);
  ]

let planning_context_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let task_id = json |> member "task_id" |> to_string in
    let task_plan = json |> member "task_plan" |> to_string in
    let notes = json |> member "notes" |> to_list |> List.map to_string in
    let errors_json = json |> member "errors" in
    let errors =
      if errors_json = `Null then []
      else
        errors_json |> to_list |> List.filter_map (fun j ->
          match error_entry_of_yojson j with
          | Ok e -> Some e
          | Error _ -> None)
    in
    let deliverable = json |> member "deliverable" |> to_string in
    let created_at = json |> member "created_at" |> to_string in
    let updated_at = json |> member "updated_at" |> to_string in
    Ok { task_id; task_plan; notes; errors; deliverable; created_at; updated_at }
  with e -> Error (Printexc.to_string e)

(* ===== Utility Functions ===== *)

let now_iso () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(** Create empty planning context *)
let create_context ~task_id =
  let now = now_iso () in
  {
    task_id;
    task_plan = "";
    notes = [];
    errors = [];
    deliverable = "";
    created_at = now;
    updated_at = now;
  }

(* ===== File System Helpers ===== *)

let planning_dir (config : Room.config) task_id =
  Filename.concat config.base_path (Printf.sprintf "planning/%s" task_id)

let ensure_dir path =
  if not (Sys.file_exists path) then
    let rec mkdir_p dir =
      if not (Sys.file_exists dir) then begin
        mkdir_p (Filename.dirname dir);
        Unix.mkdir dir 0o755
      end
    in
    mkdir_p path

(** Safe file read with Fun.protect *)
let read_file_content path =
  if Sys.file_exists path then
    let ic = open_in path in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      really_input_string ic (in_channel_length ic))
  else ""

(** Safe file write with Fun.protect and exclusive lock *)
let write_file_content path content =
  ensure_dir (Filename.dirname path);
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    (* Use exclusive lock for concurrent writes *)
    let fd = Unix.descr_of_out_channel oc in
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) (fun () ->
      output_string oc content))

(* ===== Core Operations (Pure Sync) ===== *)

(** Initialize planning context for a task *)
let init (config : Room.config) ~task_id : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    ensure_dir dir;
    let ctx = create_context ~task_id in
    (* Create empty files - PDCA structure *)
    write_file_content (Filename.concat dir "task_plan.md") "# Task Plan\n\n";
    write_file_content (Filename.concat dir "notes.md") "# Notes & Observations\n\n";
    write_file_content (Filename.concat dir "errors.md") "# Errors & Failures (PDCA Check)\n\n";
    write_file_content (Filename.concat dir "deliverable.md") "";
    (* Save context *)
    let json = planning_context_to_yojson ctx in
    write_file_content (Filename.concat dir "context.json") (Yojson.Safe.pretty_to_string json);
    Ok ctx
  with e -> Error (Printexc.to_string e)

(** Load planning context *)
let load (config : Room.config) ~task_id : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    let ctx_path = Filename.concat dir "context.json" in
    if not (Sys.file_exists ctx_path) then
      Error (Printf.sprintf "Planning context not found for task %s" task_id)
    else begin
      let content = read_file_content ctx_path in
      let json = Yojson.Safe.from_string content in
      planning_context_of_yojson json
    end
  with e -> Error (Printexc.to_string e)

(** Update task plan *)
let update_plan (config : Room.config) ~task_id ~content : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    match load config ~task_id with
    | Error e -> Error e
    | Ok ctx ->
        let updated = { ctx with task_plan = content; updated_at = now_iso () } in
        write_file_content (Filename.concat dir "task_plan.md") content;
        write_file_content (Filename.concat dir "context.json")
          (Yojson.Safe.pretty_to_string (planning_context_to_yojson updated));
        Ok updated
  with e -> Error (Printexc.to_string e)

(** Add note *)
let add_note (config : Room.config) ~task_id ~note : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    match load config ~task_id with
    | Error e -> Error e
    | Ok ctx ->
        let timestamp = now_iso () in
        let formatted_note = Printf.sprintf "## [%s]\n%s\n" timestamp note in
        let updated = { ctx with
          notes = ctx.notes @ [note];
          updated_at = now_iso ()
        } in
        (* Append to notes.md *)
        let notes_path = Filename.concat dir "notes.md" in
        let existing = read_file_content notes_path in
        write_file_content notes_path (existing ^ formatted_note ^ "\n");
        write_file_content (Filename.concat dir "context.json")
          (Yojson.Safe.pretty_to_string (planning_context_to_yojson updated));
        Ok updated
  with e -> Error (Printexc.to_string e)

(** Add error - PDCA Check phase *)
let add_error (config : Room.config) ~task_id ~error_type ~message ?context () : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    match load config ~task_id with
    | Error e -> Error e
    | Ok ctx ->
        let timestamp = now_iso () in
        let error_entry = { timestamp; error_type; message; context; resolved = false } in
        let formatted_error = Printf.sprintf "## [%s] %s\n**Type**: %s\n%s\n%s\n---\n"
          timestamp
          (if error_type = "" then "Error" else error_type)
          error_type
          message
          (match context with Some c -> Printf.sprintf "**Context**: %s" c | None -> "")
        in
        let updated = { ctx with
          errors = ctx.errors @ [error_entry];
          updated_at = now_iso ()
        } in
        (* Append to errors.md *)
        let errors_path = Filename.concat dir "errors.md" in
        let existing = read_file_content errors_path in
        write_file_content errors_path (existing ^ formatted_error ^ "\n");
        write_file_content (Filename.concat dir "context.json")
          (Yojson.Safe.pretty_to_string (planning_context_to_yojson updated));
        Ok updated
  with e -> Error (Printexc.to_string e)

(** Mark error as resolved *)
let resolve_error (config : Room.config) ~task_id ~index : (planning_context, string) result =
  try
    match load config ~task_id with
    | Error e -> Error e
    | Ok ctx ->
        if index < 0 || index >= List.length ctx.errors then
          Error (Printf.sprintf "Error index %d out of bounds" index)
        else begin
          let errors = List.mapi (fun i e ->
            if i = index then { e with resolved = true } else e
          ) ctx.errors in
          let updated = { ctx with errors; updated_at = now_iso () } in
          let dir = planning_dir config task_id in
          write_file_content (Filename.concat dir "context.json")
            (Yojson.Safe.pretty_to_string (planning_context_to_yojson updated));
          Ok updated
        end
  with e -> Error (Printexc.to_string e)

(** Set deliverable *)
let set_deliverable (config : Room.config) ~task_id ~content : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    match load config ~task_id with
    | Error e -> Error e
    | Ok ctx ->
        let updated = { ctx with deliverable = content; updated_at = now_iso () } in
        write_file_content (Filename.concat dir "deliverable.md") content;
        write_file_content (Filename.concat dir "context.json")
          (Yojson.Safe.pretty_to_string (planning_context_to_yojson updated));
        Ok updated
  with e -> Error (Printexc.to_string e)

(* ===== Session-level Context ===== *)

let current_task_file (config : Room.config) =
  Filename.concat config.base_path ".masc/current_task"

(** Get current task_id for session *)
let get_current_task (config : Room.config) : string option =
  let path = current_task_file config in
  if Sys.file_exists path then
    Some (String.trim (read_file_content path))
  else
    None

(** Set current task_id for session *)
let set_current_task (config : Room.config) ~task_id : unit =
  let path = current_task_file config in
  ensure_dir (Filename.dirname path);
  write_file_content path task_id

(** Clear current task *)
let clear_current_task (config : Room.config) : unit =
  let path = current_task_file config in
  if Sys.file_exists path then
    Sys.remove path

(** Resolve task_id - use provided or fall back to current *)
let resolve_task_id (config : Room.config) ~task_id : (string, string) result =
  match task_id with
  | "" ->
      (match get_current_task config with
       | Some t -> Ok t
       | None -> Error "No task_id provided and no current task set. Use masc_plan_set_task first.")
  | t -> Ok t

(* ===== Display Helpers ===== *)

(** Format error entry for display *)
let format_error_entry i (e : error_entry) =
  let status = if e.resolved then "✅" else "❌" in
  let ctx_str = match e.context with Some c -> Printf.sprintf " (%s)" c | None -> "" in
  Printf.sprintf "%d. %s [%s] **%s**%s: %s" (i+1) status e.timestamp e.error_type ctx_str e.message

(** Get full context as markdown for LLM consumption *)
let get_context_markdown ctx =
  let unresolved = List.filter (fun e -> not e.resolved) ctx.errors in
  let resolved = List.filter (fun e -> e.resolved) ctx.errors in
  Printf.sprintf {|# Planning Context: %s

## Task Plan (PDCA: Plan)
%s

## Notes & Observations (PDCA: Do)
%s

## Errors & Failures (PDCA: Check)
### Unresolved (%d)
%s

### Resolved (%d)
%s

## Deliverable (PDCA: Act)
%s

---
*Created: %s | Updated: %s*
|}
    ctx.task_id
    (if ctx.task_plan = "" then "_No plan yet_" else ctx.task_plan)
    (if ctx.notes = [] then "_No notes yet_" else String.concat "\n\n" (List.mapi (fun i n -> Printf.sprintf "%d. %s" (i+1) n) ctx.notes))
    (List.length unresolved)
    (if unresolved = [] then "_No unresolved errors_" else String.concat "\n" (List.mapi format_error_entry unresolved))
    (List.length resolved)
    (if resolved = [] then "_No resolved errors_" else String.concat "\n" (List.mapi format_error_entry resolved))
    (if ctx.deliverable = "" then "_No deliverable yet_" else ctx.deliverable)
    ctx.created_at
    ctx.updated_at
