(** Tool_task - Core task CRUD operations

    Handles: add_task, batch_add_tasks, cancel_task, claim, claim_next,
    done, release, task_history, tasks, transition, update_priority, archive_view
*)

open Yojson.Safe.Util

type result = bool * string

type context = {
  config: Room.config;
  agent_name: string;
}

(* JSON helpers *)
let get_string args key default =
  match args |> member key with
  | `String s -> s
  | _ -> default

let get_int args key default =
  match args |> member key with
  | `Int i -> i
  | _ -> default

let get_int_opt args key =
  match args |> member key with
  | `Int i -> Some i
  | _ -> None

let result_to_response = function
  | Ok msg -> (true, msg)
  | Error e -> (false, Types.masc_error_to_string e)

(* Handlers *)

let handle_add_task ctx args =
  let title = get_string args "title" "" in
  let priority = get_int args "priority" 3 in
  let description = get_string args "description" "" in
  (true, Room.add_task ctx.config ~title ~priority ~description)

let handle_batch_add_tasks ctx args =
  let tasks_json = match args |> member "tasks" with
    | `List l -> l
    | _ -> []
  in
  let tasks = List.map (fun t ->
    let title = t |> member "title" |> to_string in
    let priority = t |> member "priority" |> to_int_option |> Option.value ~default:3 in
    let description = t |> member "description" |> to_string_option |> Option.value ~default:"" in
    (title, priority, description)
  ) tasks_json in
  (true, Room.batch_add_tasks ctx.config tasks)

let handle_claim ctx args =
  let task_id = get_string args "task_id" "" in
  result_to_response (Room.claim_task_r ctx.config ~agent_name:ctx.agent_name ~task_id)

let handle_claim_next ctx _args =
  (true, Room.claim_next ctx.config ~agent_name:ctx.agent_name)

let handle_release ctx args =
  let task_id = get_string args "task_id" "" in
  let expected_version = get_int_opt args "expected_version" in
  result_to_response
    (Room.release_task_r ctx.config ~agent_name:ctx.agent_name ~task_id ?expected_version ())

let handle_done ctx args =
  let task_id = get_string args "task_id" "" in
  let notes = get_string args "notes" "" in
  (* Get task info BEFORE completion to extract actual start time *)
  let tasks = Room.get_tasks_raw ctx.config in
  let task_opt = List.find_opt (fun (t : Types.task) -> t.id = task_id) tasks in
  let default_time = Unix.gettimeofday () -. 60.0 in
  let (started_at_actual, collaborators_from_task) = match task_opt with
    | Some t -> (match t.task_status with
        | Types.InProgress { started_at; assignee } ->
            let ts = Types.parse_iso8601 ~default_time started_at in
            let collabs = if assignee <> "" && assignee <> ctx.agent_name then [assignee] else [] in
            (ts, collabs)
        | Types.Claimed { claimed_at; assignee } ->
            let ts = Types.parse_iso8601 ~default_time claimed_at in
            let collabs = if assignee <> "" && assignee <> ctx.agent_name then [assignee] else [] in
            (ts, collabs)
        | _ -> (default_time, []))
    | None -> (default_time, [])
  in
  let result = Room.complete_task_r ctx.config ~agent_name:ctx.agent_name ~task_id ~notes in
  (* Notify A2A subscribers on successful completion *)
  (match result with
   | Ok _ ->
       A2a_tools.notify_event
         ~event_type:A2a_tools.TaskUpdate
         ~agent:ctx.agent_name
         ~data:(`Assoc [
           ("task_id", `String task_id);
           ("action", `String "done");
           ("notes", `String notes);
         ])
   | Error _ -> ());
  (* Record metrics on successful completion *)
  (match result with
   | Ok _ ->
       let metric : Metrics_store_eio.task_metric = {
         id = Printf.sprintf "metric-%s-%d" task_id (int_of_float (Unix.gettimeofday () *. 1000.));
         agent_id = ctx.agent_name;
         task_id;
         started_at = started_at_actual;
         completed_at = Some (Unix.gettimeofday ());
         success = true;
         error_message = None;
         collaborators = collaborators_from_task;
         handoff_from = None;
         handoff_to = None;
       } in
       ignore (Metrics_store_eio.record ctx.config metric)
   | Error _ -> ());
  result_to_response result

let handle_cancel_task ctx args =
  let task_id = get_string args "task_id" "" in
  let reason = get_string args "reason" "" in
  let tasks = Room.get_tasks_raw ctx.config in
  let task_opt = List.find_opt (fun (t : Types.task) -> t.id = task_id) tasks in
  let started_at_actual = match task_opt with
    | Some t -> (match t.task_status with
        | Types.InProgress { started_at; _ } ->
            Types.parse_iso8601 ~default_time:(Unix.gettimeofday () -. 60.0) started_at
        | Types.Claimed { claimed_at; _ } ->
            Types.parse_iso8601 ~default_time:(Unix.gettimeofday () -. 60.0) claimed_at
        | _ -> Unix.gettimeofday () -. 60.0)
    | None -> Unix.gettimeofday () -. 60.0
  in
  let result = Room.cancel_task_r ctx.config ~agent_name:ctx.agent_name ~task_id ~reason in
  (* Record failed metric on cancellation *)
  (match result with
   | Ok _ ->
       let metric : Metrics_store_eio.task_metric = {
         id = Printf.sprintf "metric-%s-%d" task_id (int_of_float (Unix.gettimeofday () *. 1000.));
         agent_id = ctx.agent_name;
         task_id;
         started_at = started_at_actual;
         completed_at = Some (Unix.gettimeofday ());
         success = false;
         error_message = Some (if reason = "" then "Cancelled" else reason);
         collaborators = [];
         handoff_from = None;
         handoff_to = None;
       } in
       ignore (Metrics_store_eio.record ctx.config metric)
   | Error _ -> ());
  result_to_response result

let handle_transition ctx args =
  let task_id = get_string args "task_id" "" in
  let action = get_string args "action" "" in
  let notes = get_string args "notes" "" in
  let reason = get_string args "reason" "" in
  let expected_version = get_int_opt args "expected_version" in
  let action_lc = String.lowercase_ascii action in
  let tasks = Room.get_tasks_raw ctx.config in
  let task_opt = List.find_opt (fun (t : Types.task) -> t.id = task_id) tasks in
  let default_time = Unix.gettimeofday () -. 60.0 in
  let (started_at_actual, collaborators_from_task) = match task_opt with
    | Some t -> (match t.task_status with
        | Types.InProgress { started_at; assignee } ->
            let ts = Types.parse_iso8601 ~default_time started_at in
            let collabs = if assignee <> "" && assignee <> ctx.agent_name then [assignee] else [] in
            (ts, collabs)
        | Types.Claimed { claimed_at; assignee } ->
            let ts = Types.parse_iso8601 ~default_time claimed_at in
            let collabs = if assignee <> "" && assignee <> ctx.agent_name then [assignee] else [] in
            (ts, collabs)
        | _ -> (default_time, []))
    | None -> (default_time, [])
  in
  let result =
    Room.transition_task_r ctx.config ~agent_name:ctx.agent_name ~task_id ~action ?expected_version ~notes ~reason ()
  in
  (* Notify A2A subscribers on successful transition *)
  (match result with
   | Ok _ ->
       A2a_tools.notify_event
         ~event_type:A2a_tools.TaskUpdate
         ~agent:ctx.agent_name
         ~data:(`Assoc [
           ("task_id", `String task_id);
           ("action", `String action);
           ("notes", `String notes);
         ])
   | Error _ -> ());
  (* Record metrics *)
  (match result, action_lc with
   | Ok _, "done" ->
       let metric : Metrics_store_eio.task_metric = {
         id = Printf.sprintf "metric-%s-%d" task_id (int_of_float (Unix.gettimeofday () *. 1000.));
         agent_id = ctx.agent_name;
         task_id;
         started_at = started_at_actual;
         completed_at = Some (Unix.gettimeofday ());
         success = true;
         error_message = None;
         collaborators = collaborators_from_task;
         handoff_from = None;
         handoff_to = None;
       } in
       ignore (Metrics_store_eio.record ctx.config metric)
   | Ok _, "cancel" ->
       let metric : Metrics_store_eio.task_metric = {
         id = Printf.sprintf "metric-%s-%d" task_id (int_of_float (Unix.gettimeofday () *. 1000.));
         agent_id = ctx.agent_name;
         task_id;
         started_at = started_at_actual;
         completed_at = Some (Unix.gettimeofday ());
         success = false;
         error_message = Some (if reason = "" then "Cancelled" else reason);
         collaborators = collaborators_from_task;
         handoff_from = None;
         handoff_to = None;
       } in
       ignore (Metrics_store_eio.record ctx.config metric)
   | _ -> ());
  result_to_response result

let handle_update_priority ctx args =
  let task_id = get_string args "task_id" "" in
  let priority = get_int args "priority" 3 in
  (true, Room.update_priority ctx.config ~task_id ~priority)

let handle_tasks ctx _args =
  (true, Room.list_tasks ctx.config)

let handle_task_history ctx args =
  let task_id = get_string args "task_id" "" in
  let limit = get_int args "limit" 50 in
  let scan_limit = min 500 (limit * 5) in
  let lines = Mcp_server.read_event_lines ctx.config ~limit:scan_limit in
  let parsed =
    List.filter_map (fun line ->
      try Some (Yojson.Safe.from_string line) with Yojson.Json_error _ -> None
    ) lines
  in
  let matches_task json =
    let task = json |> member "task" |> to_string_option in
    let task_id_field = json |> member "task_id" |> to_string_option in
    match task, task_id_field with
    | Some t, _ when t = task_id -> true
    | _, Some t when t = task_id -> true
    | _ -> false
  in
  let rec take n xs =
    match xs with
    | [] -> []
    | _ when n <= 0 -> []
    | x :: rest -> x :: take (n - 1) rest
  in
  let events = parsed |> List.filter matches_task |> take limit in
  (true, Yojson.Safe.pretty_to_string (`List events))

let handle_archive_view ctx args =
  let limit = get_int args "limit" 20 in
  let archive_path = Room_utils.archive_path ctx.config in
  if not (Room_utils.path_exists ctx.config archive_path) then
    (true, Yojson.Safe.pretty_to_string (`Assoc [("count", `Int 0); ("tasks", `List [])]))
  else
    let json = Room_utils.read_json ctx.config archive_path in
    let tasks =
      match json with
      | `List items -> items
      | `Assoc _ ->
          (match json |> member "tasks" with
           | `List items -> items
           | _ -> [])
      | _ -> []
    in
    let total = List.length tasks in
    let tasks =
      if total <= limit then tasks
      else
        let rec drop n xs =
          match xs with
          | [] -> []
          | _ when n <= 0 -> xs
          | _ :: rest -> drop (n - 1) rest
        in
        drop (total - limit) tasks
    in
    let response = `Assoc [
      ("count", `Int (List.length tasks));
      ("total", `Int total);
      ("tasks", `List tasks);
    ] in
    (true, Yojson.Safe.pretty_to_string response)

(* Dispatch function *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_add_task" -> Some (handle_add_task ctx args)
  | "masc_batch_add_tasks" -> Some (handle_batch_add_tasks ctx args)
  | "masc_claim" -> Some (handle_claim ctx args)
  | "masc_claim_next" -> Some (handle_claim_next ctx args)
  | "masc_release" -> Some (handle_release ctx args)
  | "masc_done" -> Some (handle_done ctx args)
  | "masc_cancel_task" -> Some (handle_cancel_task ctx args)
  | "masc_transition" -> Some (handle_transition ctx args)
  | "masc_update_priority" -> Some (handle_update_priority ctx args)
  | "masc_tasks" -> Some (handle_tasks ctx args)
  | "masc_task_history" -> Some (handle_task_history ctx args)
  | "masc_archive_view" -> Some (handle_archive_view ctx args)
  | _ -> None
