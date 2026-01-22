(** Planning with Files - Manus AI 3-file pattern

    Implements the planning-with-files pattern for structured AI collaboration:
    - task_plan.md: Main execution plan
    - notes.md: Observations, learnings, context
    - deliverable.md: Final output/result

    Supports multi-backend: file system and Redis
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

(* ===== Backend Interface (First-Class Module Pattern) ===== *)

(** Planning backend interface - enables Open/Closed principle.
    All operations return Lwt.t for proper async support in MCP server context. *)
module type PLANNING_BACKEND = sig
  val init : Room.config -> task_id:string -> (planning_context, string) result Lwt.t
  val load : Room.config -> task_id:string -> (planning_context, string) result Lwt.t
  val update_plan : Room.config -> task_id:string -> content:string -> (planning_context, string) result Lwt.t
  val add_note : Room.config -> task_id:string -> note:string -> (planning_context, string) result Lwt.t
  val set_deliverable : Room.config -> task_id:string -> content:string -> (planning_context, string) result Lwt.t
  val add_error : Room.config -> task_id:string -> error_type:string -> message:string -> context:string option -> (planning_context, string) result Lwt.t
  val resolve_error : Room.config -> task_id:string -> index:int -> (planning_context, string) result Lwt.t
end

(* ===== File Backend ===== *)

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

let read_file_content path =
  if Sys.file_exists path then
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  else ""

let write_file_content path content =
  ensure_dir (Filename.dirname path);
  let oc = open_out path in
  output_string oc content;
  close_out oc

(** Initialize planning context for a task (File backend) *)
let init_file config ~task_id : (planning_context, string) result =
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

(** Load planning context (File backend) *)
let load_file config ~task_id : (planning_context, string) result =
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

(** Update task plan (File backend) *)
let update_plan_file config ~task_id ~content : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    match load_file config ~task_id with
    | Error e -> Error e
    | Ok ctx ->
        let updated = { ctx with task_plan = content; updated_at = now_iso () } in
        write_file_content (Filename.concat dir "task_plan.md") content;
        write_file_content (Filename.concat dir "context.json")
          (Yojson.Safe.pretty_to_string (planning_context_to_yojson updated));
        Ok updated
  with e -> Error (Printexc.to_string e)

(** Add note (File backend) *)
let add_note_file config ~task_id ~note : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    match load_file config ~task_id with
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

(** Add error (File backend) - PDCA Check phase *)
let add_error_file config ~task_id ~error_type ~message ?context () : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    match load_file config ~task_id with
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

(** Mark error as resolved (File backend) *)
let resolve_error_file config ~task_id ~index : (planning_context, string) result =
  try
    match load_file config ~task_id with
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

(** Set deliverable (File backend) *)
let set_deliverable_file config ~task_id ~content : (planning_context, string) result =
  try
    let dir = planning_dir config task_id in
    match load_file config ~task_id with
    | Error e -> Error e
    | Ok ctx ->
        let updated = { ctx with deliverable = content; updated_at = now_iso () } in
        write_file_content (Filename.concat dir "deliverable.md") content;
        write_file_content (Filename.concat dir "context.json")
          (Yojson.Safe.pretty_to_string (planning_context_to_yojson updated));
        Ok updated
  with e -> Error (Printexc.to_string e)

(** File backend module - implements PLANNING_BACKEND (async via Lwt.return) *)
module File_backend : PLANNING_BACKEND = struct
  (* Use Lwt_preemptive.detach to offload blocking file I/O to thread pool *)
  let init config ~task_id =
    Lwt_preemptive.detach (fun () -> init_file config ~task_id) ()
  let load config ~task_id =
    Lwt_preemptive.detach (fun () -> load_file config ~task_id) ()
  let update_plan config ~task_id ~content =
    Lwt_preemptive.detach (fun () -> update_plan_file config ~task_id ~content) ()
  let add_note config ~task_id ~note =
    Lwt_preemptive.detach (fun () -> add_note_file config ~task_id ~note) ()
  let set_deliverable config ~task_id ~content =
    Lwt_preemptive.detach (fun () -> set_deliverable_file config ~task_id ~content) ()
  let add_error config ~task_id ~error_type ~message ~context =
    Lwt_preemptive.detach (fun () -> add_error_file config ~task_id ~error_type ~message ?context ()) ()
  let resolve_error config ~task_id ~index =
    Lwt_preemptive.detach (fun () -> resolve_error_file config ~task_id ~index) ()
end

(* ===== Redis Backend ===== *)

let planning_key task_id = Printf.sprintf "planning:%s" task_id

(** Initialize planning context for a task (Redis backend) *)
let init_redis config ~task_id : (planning_context, string) result =
  try
    let ctx = create_context ~task_id in
    let json = Yojson.Safe.to_string (planning_context_to_yojson ctx) in
    let key = planning_key task_id in
    match Room_utils.backend_set config ~key ~value:json with
    | Ok () -> Ok ctx
    | Error e -> Error (Backend.show_error e)
  with e -> Error (Printexc.to_string e)

(** Load planning context (Redis backend) *)
let load_redis config ~task_id : (planning_context, string) result =
  try
    let key = planning_key task_id in
    match Room_utils.backend_get config ~key with
    | Ok (Some value) ->
        let json = Yojson.Safe.from_string value in
        planning_context_of_yojson json
    | Ok None -> Error (Printf.sprintf "Planning context not found for task %s" task_id)
    | Error e -> Error (Backend.show_error e)
  with e -> Error (Printexc.to_string e)

(** Higher-order function: load → transform → save pattern for Redis backend.
    The transform function returns Ok updated_context or Error message. *)
let with_redis_state config ~task_id ~f : (planning_context, string) result =
  match load_redis config ~task_id with
  | Error e -> Error e
  | Ok ctx ->
      match f ctx with
      | Error e -> Error e
      | Ok updated ->
          let json = Yojson.Safe.to_string (planning_context_to_yojson updated) in
          let key = planning_key task_id in
          match Room_utils.backend_set config ~key ~value:json with
          | Ok () -> Ok updated
          | Error e -> Error (Backend.show_error e)

(** Update task plan (Redis backend) *)
let update_plan_redis config ~task_id ~content : (planning_context, string) result =
  with_redis_state config ~task_id ~f:(fun ctx ->
    Ok { ctx with task_plan = content; updated_at = now_iso () })

(** Add a note (Redis backend) *)
let add_note_redis config ~task_id ~note : (planning_context, string) result =
  let timestamped = Printf.sprintf "[%s] %s" (now_iso ()) note in
  with_redis_state config ~task_id ~f:(fun ctx ->
    Ok { ctx with notes = ctx.notes @ [timestamped]; updated_at = now_iso () })

(** Add an error entry (Redis backend) *)
let add_error_redis config ~task_id ~error_type ~message ~context : (planning_context, string) result =
  let entry = { timestamp = now_iso (); error_type; message; context; resolved = false } in
  with_redis_state config ~task_id ~f:(fun ctx ->
    Ok { ctx with errors = ctx.errors @ [entry]; updated_at = now_iso () })

(** Resolve an error (Redis backend) *)
let resolve_error_redis config ~task_id ~index : (planning_context, string) result =
  with_redis_state config ~task_id ~f:(fun ctx ->
    if index < 0 || index >= List.length ctx.errors then
      Error (Printf.sprintf "Error index %d out of range" index)
    else
      let errors = List.mapi (fun i e ->
        if i = index then { e with resolved = true } else e
      ) ctx.errors in
      Ok { ctx with errors; updated_at = now_iso () })

(** Set deliverable (Redis backend) *)
let set_deliverable_redis config ~task_id ~content : (planning_context, string) result =
  with_redis_state config ~task_id ~f:(fun ctx ->
    Ok { ctx with deliverable = content; updated_at = now_iso () })

(** Redis backend module - implements PLANNING_BACKEND (async via Lwt.return).
    Note: Currently wraps sync Redis operations. Future: use BACKEND_ASYNC directly. *)
module Redis_backend : PLANNING_BACKEND = struct
  (* Use Lwt_preemptive.detach to offload blocking Redis I/O to thread pool *)
  let init config ~task_id =
    Lwt_preemptive.detach (fun () -> init_redis config ~task_id) ()
  let load config ~task_id =
    Lwt_preemptive.detach (fun () -> load_redis config ~task_id) ()
  let update_plan config ~task_id ~content =
    Lwt_preemptive.detach (fun () -> update_plan_redis config ~task_id ~content) ()
  let add_note config ~task_id ~note =
    Lwt_preemptive.detach (fun () -> add_note_redis config ~task_id ~note) ()
  let set_deliverable config ~task_id ~content =
    Lwt_preemptive.detach (fun () -> set_deliverable_redis config ~task_id ~content) ()
  let add_error config ~task_id ~error_type ~message ~context =
    Lwt_preemptive.detach (fun () -> add_error_redis config ~task_id ~error_type ~message ~context) ()
  let resolve_error config ~task_id ~index =
    Lwt_preemptive.detach (fun () -> resolve_error_redis config ~task_id ~index) ()
end

(* ===== Backend Selection ===== *)

let is_redis_backend config =
  match config.Room_utils.backend with
  | Room_utils.RedisRest _ | Room_utils.RedisNative _ | Room_utils.PostgresNative _ -> true
  | Room_utils.Memory _ | Room_utils.FileSystem _ -> false

(** Get appropriate backend module based on config *)
let get_backend config : (module PLANNING_BACKEND) =
  if is_redis_backend config then (module Redis_backend)
  else (module File_backend)

(* ===== Session-level Context ===== *)
(** Current task tracking - allows omitting task_id in repeated calls *)

let current_task_file config =
  Filename.concat config.Room_utils.base_path ".masc/current_task"

(** Get current task_id for session *)
let get_current_task config : string option =
  let path = current_task_file config in
  if Sys.file_exists path then
    Some (String.trim (read_file_content path))
  else
    None

(** Set current task_id for session *)
let set_current_task config ~task_id : unit =
  let path = current_task_file config in
  ensure_dir (Filename.dirname path);
  write_file_content path task_id

(** Clear current task *)
let clear_current_task config : unit =
  let path = current_task_file config in
  if Sys.file_exists path then
    Sys.remove path

(** Resolve task_id - use provided or fall back to current *)
let resolve_task_id config ~task_id : (string, string) result =
  match task_id with
  | "" ->
      (match get_current_task config with
       | Some t -> Ok t
       | None -> Error "No task_id provided and no current task set. Use masc_plan_set_task first.")
  | t -> Ok t

(* ===== Unified API (First-Class Module dispatch) ===== *)

(** Initialize planning context *)
let init config ~task_id : (planning_context, string) result Lwt.t =
  let module B = (val get_backend config : PLANNING_BACKEND) in
  B.init config ~task_id

(** Load planning context *)
let load config ~task_id : (planning_context, string) result Lwt.t =
  let module B = (val get_backend config : PLANNING_BACKEND) in
  B.load config ~task_id

(** Update task plan *)
let update_plan config ~task_id ~content : (planning_context, string) result Lwt.t =
  let module B = (val get_backend config : PLANNING_BACKEND) in
  B.update_plan config ~task_id ~content

(** Add note *)
let add_note config ~task_id ~note : (planning_context, string) result Lwt.t =
  let module B = (val get_backend config : PLANNING_BACKEND) in
  B.add_note config ~task_id ~note

(** Set deliverable *)
let set_deliverable config ~task_id ~content : (planning_context, string) result Lwt.t =
  let module B = (val get_backend config : PLANNING_BACKEND) in
  B.set_deliverable config ~task_id ~content

(** Add error - PDCA Check phase *)
let add_error config ~task_id ~error_type ~message ?context () : (planning_context, string) result Lwt.t =
  let module B = (val get_backend config : PLANNING_BACKEND) in
  B.add_error config ~task_id ~error_type ~message ~context

(** Mark error as resolved *)
let resolve_error config ~task_id ~index : (planning_context, string) result Lwt.t =
  let module B = (val get_backend config : PLANNING_BACKEND) in
  B.resolve_error config ~task_id ~index

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
