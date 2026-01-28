(** Relay tools - Infinite context via handoff *)

let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

let get_string_opt args key =
  match Yojson.Safe.Util.member key args with
  | `String s when s <> "" -> Some s
  | _ -> None

let get_int args key default =
  match Yojson.Safe.Util.member key args with
  | `Int n -> n
  | _ -> default

let get_string_list args key =
  match Yojson.Safe.Util.member key args with
  | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

let handle_relay_status _ctx args =
  let messages = get_int args "messages" 0 in
  let tool_calls = get_int args "tool_calls" 0 in
  let model = get_string args "model" "claude" in
  let metrics = Relay.estimate_context ~messages ~tool_calls ~model in
  let should_relay = Relay.should_relay ~config:Relay.default_config ~metrics in
  let json = `Assoc [
    ("estimated_tokens", `Int metrics.Relay.estimated_tokens);
    ("max_tokens", `Int metrics.Relay.max_tokens);
    ("usage_ratio", `Float metrics.Relay.usage_ratio);
    ("message_count", `Int metrics.Relay.message_count);
    ("tool_call_count", `Int metrics.Relay.tool_call_count);
    ("should_relay", `Bool should_relay);
  ] in
  (true, Yojson.Safe.pretty_to_string json)

let handle_relay_checkpoint _ctx args =
  let summary = get_string args "summary" "" in
  let current_task = get_string_opt args "current_task" in
  let todos = get_string_list args "todos" in
  let pdca_state = get_string_opt args "pdca_state" in
  let relevant_files = get_string_list args "relevant_files" in
  let cell = !(Mcp_server.current_cell) in
  let messages = get_int args "messages" cell.Mitosis.task_count in
  let tool_calls = get_int args "tool_calls" cell.Mitosis.tool_call_count in
  let metrics = Relay.estimate_context ~messages ~tool_calls ~model:"claude" in
  let _ = Relay.save_checkpoint ~summary ~task:current_task ~todos ~pdca:pdca_state ~files:relevant_files ~metrics in
  let json = `Assoc [
    ("status", `String "checkpoint_saved");
    ("usage_ratio", `Float metrics.Relay.usage_ratio);
    ("estimated_tokens", `Int metrics.Relay.estimated_tokens);
  ] in
  (true, Yojson.Safe.pretty_to_string json)

let handle_relay_now _ctx args =
  let summary = get_string args "summary" "" in
  let current_task = get_string_opt args "current_task" in
  let target_agent = get_string args "target_agent" "claude" in
  let generation = get_int args "generation" 0 in
  let payload : Relay.handoff_payload = {
    summary;
    current_task;
    todos = [];
    pdca_state = None;
    relevant_files = [];
    session_id = None;
    relay_generation = generation;
  } in
  let prompt = Relay.build_handoff_prompt ~payload ~generation:(generation + 1) in
  let result = Spawn.spawn ~agent_name:target_agent ~prompt ~timeout_seconds:600 () in
  let output_preview =
    if String.length result.Spawn.output > 500 then
      String.sub result.Spawn.output 0 500
    else result.Spawn.output
  in
  let json = `Assoc [
    ("success", `Bool result.Spawn.success);
    ("exit_code", `Int result.Spawn.exit_code);
    ("elapsed_ms", `Int result.Spawn.elapsed_ms);
    ("target_agent", `String target_agent);
    ("generation", `Int (generation + 1));
    ("output_preview", `String output_preview);
  ] in
  (true, Yojson.Safe.pretty_to_string json)

let handle_relay_smart_check _ctx args =
  let messages = get_int args "messages" 0 in
  let tool_calls = get_int args "tool_calls" 0 in
  let hint_str = get_string args "task_hint" "simple" in
  let file_count = get_int args "file_count" 1 in
  let task_hint =
    match hint_str with
    | "large_file" -> Relay.Large_file_read "unknown"
    | "multi_file" -> Relay.Multi_file_edit (max 1 file_count)
    | "long_running" -> Relay.Long_running_task
    | "exploration" -> Relay.Exploration_task
    | _ -> Relay.Simple_task
  in
  let metrics = Relay.estimate_context ~messages ~tool_calls ~model:"claude" in
  let decision = Relay.should_relay_smart ~config:Relay.default_config ~metrics ~task_hint in
  let decision_str = match decision with
    | `Proactive -> "proactive"
    | `Reactive -> "reactive"
    | `No_relay -> "no_relay"
  in
  let json = `Assoc [
    ("decision", `String decision_str);
    ("usage_ratio", `Float metrics.Relay.usage_ratio);
    ("estimated_tokens", `Int metrics.Relay.estimated_tokens);
    ("max_tokens", `Int metrics.Relay.max_tokens);
  ] in
  (true, Yojson.Safe.pretty_to_string json)

let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_relay_status" -> Some (handle_relay_status ctx args)
  | "masc_relay_checkpoint" -> Some (handle_relay_checkpoint ctx args)
  | "masc_relay_now" -> Some (handle_relay_now ctx args)
  | "masc_relay_smart_check" -> Some (handle_relay_smart_check ctx args)
  | _ -> None
