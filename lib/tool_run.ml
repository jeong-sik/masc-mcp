(** Run Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    6 tools: run_init, run_plan, run_log, run_deliverable, run_get, run_list
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

let get_string_opt args key =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) when s <> "" -> Some s
       | _ -> None)
  | _ -> None

(** {1 Individual Handlers} *)

let handle_run_init ctx args : result =
  let task_id = get_string args "task_id" "" in
  let agent = get_string_opt args "agent_name" in
  match Run_eio.init ctx.config ~task_id ~agent_name:agent with
  | Ok run ->
      (true, Yojson.Safe.pretty_to_string (Run_eio.run_record_to_json run))
  | Error e ->
      (false, Printf.sprintf "❌ Failed to init run: %s" e)

let handle_run_plan ctx args : result =
  let task_id = get_string args "task_id" "" in
  let plan = get_string args "plan" "" in
  match Run_eio.update_plan ctx.config ~task_id ~content:plan with
  | Ok run ->
      (true, Yojson.Safe.pretty_to_string (Run_eio.run_record_to_json run))
  | Error e ->
      (false, Printf.sprintf "❌ Failed to update run plan: %s" e)

let handle_run_log ctx args : result =
  let task_id = get_string args "task_id" "" in
  let note = get_string args "note" "" in
  match Run_eio.append_log ctx.config ~task_id ~note with
  | Ok entry ->
      (true, Yojson.Safe.pretty_to_string (Run_eio.log_entry_to_json entry))
  | Error e ->
      (false, Printf.sprintf "❌ Failed to append run log: %s" e)

let handle_run_deliverable ctx args : result =
  let task_id = get_string args "task_id" "" in
  let deliverable = get_string args "deliverable" "" in
  match Run_eio.set_deliverable ctx.config ~task_id ~content:deliverable with
  | Ok run ->
      (true, Yojson.Safe.pretty_to_string (Run_eio.run_record_to_json run))
  | Error e ->
      (false, Printf.sprintf "❌ Failed to set run deliverable: %s" e)

let handle_run_get ctx args : result =
  let task_id = get_string args "task_id" "" in
  match Run_eio.get ctx.config ~task_id with
  | Ok json -> (true, Yojson.Safe.pretty_to_string json)
  | Error e -> (false, Printf.sprintf "❌ Failed to get run: %s" e)

let handle_run_list ctx _args : result =
  let json = Run_eio.list ctx.config in
  (true, Yojson.Safe.pretty_to_string json)

(** {1 Dispatcher} *)

let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_run_init" -> Some (handle_run_init ctx args)
  | "masc_run_plan" -> Some (handle_run_plan ctx args)
  | "masc_run_log" -> Some (handle_run_log ctx args)
  | "masc_run_deliverable" -> Some (handle_run_deliverable ctx args)
  | "masc_run_get" -> Some (handle_run_get ctx args)
  | "masc_run_list" -> Some (handle_run_list ctx args)
  | _ -> None
