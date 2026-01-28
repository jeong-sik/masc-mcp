(** Plan Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    11 tools: plan_init, plan_update, note_add, deliver, plan_get,
              error_add, error_resolve, plan_set_task, plan_get_task, plan_clear_task
*)

(** Tool handler context *)
type context = {
  config: Room.config;
}

(** Tool result type *)
type result = bool * string

(** {1 Argument Helpers} *)

let get_string args key default =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) -> s
       | _ -> default)
  | _ -> default

let get_int args key default =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Int i) -> i
       | _ -> default)
  | _ -> default

(** {1 Individual Handlers} *)

let handle_plan_init ctx args : result =
  let task_id = get_string args "task_id" "" in
  let result = Planning_eio.init ctx.config ~task_id in
  match result with
  | Ok _ctx ->
      let response = `Assoc [
        ("status", `String "initialized");
        ("task_id", `String task_id);
        ("message", `String (Printf.sprintf "Planning context created for %s" task_id));
      ] in
      (true, Yojson.Safe.pretty_to_string response)
  | Error e ->
      (false, Printf.sprintf "❌ Failed to init planning: %s" e)

let handle_plan_update ctx args : result =
  let task_id = get_string args "task_id" "" in
  let content = get_string args "content" "" in
  let result = Planning_eio.update_plan ctx.config ~task_id ~content in
  match result with
  | Ok plan_ctx ->
      let response = `Assoc [
        ("status", `String "updated");
        ("task_id", `String task_id);
        ("updated_at", `String plan_ctx.Planning_eio.updated_at);
      ] in
      (true, Yojson.Safe.pretty_to_string response)
  | Error e ->
      (false, Printf.sprintf "❌ Failed to update plan: %s" e)

let handle_note_add ctx args : result =
  let task_id = get_string args "task_id" "" in
  let note = get_string args "note" "" in
  let result = Planning_eio.add_note ctx.config ~task_id ~note in
  match result with
  | Ok plan_ctx ->
      let response = `Assoc [
        ("status", `String "added");
        ("task_id", `String task_id);
        ("note_count", `Int (List.length plan_ctx.Planning_eio.notes));
      ] in
      (true, Yojson.Safe.pretty_to_string response)
  | Error e ->
      (false, Printf.sprintf "❌ Failed to add note: %s" e)

let handle_deliver ctx args : result =
  let task_id = get_string args "task_id" "" in
  let content = get_string args "content" "" in
  let result = Planning_eio.set_deliverable ctx.config ~task_id ~content in
  match result with
  | Ok plan_ctx ->
      let response = `Assoc [
        ("status", `String "delivered");
        ("task_id", `String task_id);
        ("updated_at", `String plan_ctx.Planning_eio.updated_at);
      ] in
      (true, Yojson.Safe.pretty_to_string response)
  | Error e ->
      (false, Printf.sprintf "❌ Failed to set deliverable: %s" e)

let handle_plan_get ctx args : result =
  let task_id_input = get_string args "task_id" "" in
  match Planning_eio.resolve_task_id ctx.config ~task_id:task_id_input with
  | Error e -> (false, Printf.sprintf "❌ %s" e)
  | Ok task_id ->
      let result = Planning_eio.load ctx.config ~task_id in
      match result with
      | Ok plan_ctx ->
          let markdown = Planning_eio.get_context_markdown plan_ctx in
          let response = `Assoc [
            ("task_id", `String task_id);
            ("context", Planning_eio.planning_context_to_yojson plan_ctx);
            ("markdown", `String markdown);
          ] in
          (true, Yojson.Safe.pretty_to_string response)
      | Error e ->
          (false, Printf.sprintf "❌ Planning context not found: %s" e)

let handle_error_add ctx args : result =
  let task_id = get_string args "task_id" "" in
  let error_type = get_string args "error_type" "" in
  let message = get_string args "message" "" in
  let context = match get_string args "context" "" with "" -> None | s -> Some s in
  let result = Planning_eio.add_error ctx.config ~task_id ~error_type ~message ?context () in
  match result with
  | Ok plan_ctx ->
      let unresolved_count = List.length (List.filter (fun (e : Planning_eio.error_entry) -> not e.resolved) plan_ctx.errors) in
      let response = `Assoc [
        ("status", `String "added");
        ("task_id", `String task_id);
        ("error_type", `String error_type);
        ("total_errors", `Int (List.length plan_ctx.errors));
        ("unresolved_count", `Int unresolved_count);
      ] in
      (true, Yojson.Safe.pretty_to_string response)
  | Error e ->
      (false, Printf.sprintf "❌ Failed to add error: %s" e)

let handle_error_resolve ctx args : result =
  let task_id = get_string args "task_id" "" in
  let error_index = get_int args "error_index" 0 in
  let result = Planning_eio.resolve_error ctx.config ~task_id ~index:error_index in
  match result with
  | Ok plan_ctx ->
      let unresolved_count = List.length (List.filter (fun (e : Planning_eio.error_entry) -> not e.resolved) plan_ctx.errors) in
      let response = `Assoc [
        ("status", `String "resolved");
        ("task_id", `String task_id);
        ("error_index", `Int error_index);
        ("remaining_unresolved", `Int unresolved_count);
      ] in
      (true, Yojson.Safe.pretty_to_string response)
  | Error e ->
      (false, Printf.sprintf "❌ Failed to resolve error: %s" e)

let handle_plan_set_task ctx args : result =
  let task_id = get_string args "task_id" "" in
  if task_id = "" then
    (false, "❌ task_id is required")
  else begin
    Planning_eio.set_current_task ctx.config ~task_id;
    let response = `Assoc [
      ("status", `String "set");
      ("current_task", `String task_id);
    ] in
    (true, Yojson.Safe.pretty_to_string response)
  end

let handle_plan_get_task ctx _args : result =
  match Planning_eio.get_current_task ctx.config with
  | Some task_id ->
      let response = `Assoc [
        ("current_task", `String task_id);
      ] in
      (true, Yojson.Safe.pretty_to_string response)
  | None ->
      let response = `Assoc [
        ("current_task", `Null);
        ("message", `String "No current task set. Use masc_plan_set_task first.");
      ] in
      (true, Yojson.Safe.pretty_to_string response)

let handle_plan_clear_task ctx _args : result =
  Planning_eio.clear_current_task ctx.config;
  let response = `Assoc [
    ("status", `String "cleared");
    ("message", `String "Current task cleared");
  ] in
  (true, Yojson.Safe.pretty_to_string response)

(** {1 Dispatcher} *)

let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_plan_init" -> Some (handle_plan_init ctx args)
  | "masc_plan_update" -> Some (handle_plan_update ctx args)
  | "masc_note_add" -> Some (handle_note_add ctx args)
  | "masc_deliver" -> Some (handle_deliver ctx args)
  | "masc_plan_get" -> Some (handle_plan_get ctx args)
  | "masc_error_add" -> Some (handle_error_add ctx args)
  | "masc_error_resolve" -> Some (handle_error_resolve ctx args)
  | "masc_plan_set_task" -> Some (handle_plan_set_task ctx args)
  | "masc_plan_get_task" -> Some (handle_plan_get_task ctx args)
  | "masc_plan_clear_task" -> Some (handle_plan_clear_task ctx args)
  | _ -> None
