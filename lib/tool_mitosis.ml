(** Mitosis Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    7 tools: mitosis_status, mitosis_all, mitosis_pool, mitosis_divide,
             mitosis_check, mitosis_record, mitosis_prepare
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

let get_float args key default =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Float f) -> f
       | Some (`Int i) -> Float.of_int i
       | _ -> default)
  | _ -> default

let get_bool args key default =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Bool b) -> b
       | _ -> default)
  | _ -> default

(** {1 Individual Handlers} *)

let handle_mitosis_status _ctx _args : result =
  let cell = !(Mcp_server.current_cell) in
  let pool = !(Mcp_server.stem_pool) in
  let json = `Assoc [
    ("cell", Mitosis.cell_to_json cell);
    ("pool", Mitosis.pool_to_json pool);
    ("config", Mitosis.config_to_json Mitosis.default_config);
  ] in
  (true, Yojson.Safe.pretty_to_string json)

let handle_mitosis_all ctx _args : result =
  let statuses = Mitosis.get_all_statuses ~room_config:ctx.config in
  let json =
    `List (List.map (fun (node_id, status, ratio) ->
      `Assoc [
        ("node_id", `String node_id);
        ("status", `String status);
        ("estimated_ratio", `Float ratio);
      ]) statuses)
  in
  (true, Yojson.Safe.pretty_to_string json)

let handle_mitosis_pool _ctx _args : result =
  let pool = !(Mcp_server.stem_pool) in
  (true, Yojson.Safe.pretty_to_string (Mitosis.pool_to_json pool))

let handle_mitosis_divide ctx args : result =
  let summary = get_string args "summary" "" in
  let current_task = get_string args "current_task" "" in
  let full_context =
    if current_task = "" then summary
    else Printf.sprintf "Summary: %s\n\nCurrent Task: %s" summary current_task
  in
  let cell = !(Mcp_server.current_cell) in
  let config_mitosis = Mitosis.default_config in
  let spawn_fn ~prompt =
    Spawn.spawn ~agent_name:"claude" ~prompt ~timeout_seconds:600 ()
  in
  let (spawn_result, new_cell, new_pool) =
    Mitosis.execute_mitosis ~config:config_mitosis ~pool:!(Mcp_server.stem_pool)
      ~parent:cell ~full_context ~spawn_fn
  in
  Mcp_server.current_cell := new_cell;
  Mcp_server.stem_pool := new_pool;
  Mitosis.write_status_with_backend ~room_config:ctx.config ~cell:new_cell ~config:config_mitosis;
  let json = `Assoc [
    ("success", `Bool spawn_result.Spawn.success);
    ("previous_generation", `Int cell.Mitosis.generation);
    ("new_generation", `Int new_cell.Mitosis.generation);
    ("successor_output", `String (String.sub spawn_result.Spawn.output 0 (min 500 (String.length spawn_result.Spawn.output))));
  ] in
  (true, Yojson.Safe.pretty_to_string json)

let handle_mitosis_check _ctx args : result =
  let context_ratio = get_float args "context_ratio" 0.0 in
  let cell = !(Mcp_server.current_cell) in
  let config_mitosis = Mitosis.default_config in
  let should_prepare = Mitosis.should_prepare ~config:config_mitosis ~cell ~context_ratio in
  let should_handoff = Mitosis.should_handoff ~config:config_mitosis ~cell ~context_ratio in
  let json = `Assoc [
    ("should_prepare", `Bool should_prepare);
    ("should_handoff", `Bool should_handoff);
    ("context_ratio", `Float context_ratio);
    ("threshold_prepare", `Float config_mitosis.Mitosis.prepare_threshold);
    ("threshold_handoff", `Float config_mitosis.Mitosis.handoff_threshold);
    ("phase", `String (Mitosis.phase_to_string cell.Mitosis.phase));
  ] in
  (true, Yojson.Safe.pretty_to_string json)

let handle_mitosis_record ctx _args : result =
  let task_done = get_bool _args "task_done" false in
  let tool_called = get_bool _args "tool_called" false in
  let cell = !(Mcp_server.current_cell) in
  let updated = Mitosis.record_activity ~cell ~task_done ~tool_called in
  Mcp_server.current_cell := updated;
  Mitosis.write_status_with_backend ~room_config:ctx.config ~cell:updated ~config:Mitosis.default_config;
  let json = `Assoc [
    ("task_count", `Int updated.Mitosis.task_count);
    ("tool_call_count", `Int updated.Mitosis.tool_call_count);
    ("last_activity", `Float updated.Mitosis.last_activity);
  ] in
  (true, Yojson.Safe.pretty_to_string json)

let handle_mitosis_prepare ctx args : result =
  let full_context = get_string args "full_context" "" in
  let cell = !(Mcp_server.current_cell) in
  let prepared = Mitosis.prepare_for_division ~config:Mitosis.default_config ~cell ~full_context in
  Mcp_server.current_cell := prepared;
  Mitosis.write_status_with_backend ~room_config:ctx.config ~cell:prepared ~config:Mitosis.default_config;
  let json = `Assoc [
    ("status", `String "prepared");
    ("phase", `String (Mitosis.phase_to_string prepared.Mitosis.phase));
    ("dna_length", `Int (String.length (Option.value ~default:"" prepared.Mitosis.prepared_dna)));
  ] in
  (true, Yojson.Safe.pretty_to_string json)

(** {1 Dispatcher} *)

let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_mitosis_status" -> Some (handle_mitosis_status ctx args)
  | "masc_mitosis_all" -> Some (handle_mitosis_all ctx args)
  | "masc_mitosis_pool" -> Some (handle_mitosis_pool ctx args)
  | "masc_mitosis_divide" -> Some (handle_mitosis_divide ctx args)
  | "masc_mitosis_check" -> Some (handle_mitosis_check ctx args)
  | "masc_mitosis_record" -> Some (handle_mitosis_record ctx args)
  | "masc_mitosis_prepare" -> Some (handle_mitosis_prepare ctx args)
  | _ -> None
