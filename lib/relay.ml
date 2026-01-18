(** MASC Relay - Infinite Context via Agent Handoff *)

(** Relay configuration *)
type relay_config = {
  threshold: float;          (* Context usage threshold (0.0-1.0), default 0.8 *)
  target_agent: string;      (* Agent to relay to, default "claude" *)
  compress_ratio: float;     (* Target compression ratio, default 0.1 *)
  include_todos: bool;       (* Include TODO list in handoff *)
  include_pdca: bool;        (* Include PDCA state in handoff *)
  neo4j_episode: bool;       (* Create Neo4j Episode for continuity *)
}

(** Default relay configuration *)
let default_config = {
  threshold = 0.8;
  target_agent = "claude";
  compress_ratio = 0.1;
  include_todos = true;
  include_pdca = true;
  neo4j_episode = true;
}

(** Context metrics *)
type context_metrics = {
  estimated_tokens: int;
  max_tokens: int;
  usage_ratio: float;
  message_count: int;
  tool_call_count: int;
}

(** Handoff payload *)
type handoff_payload = {
  summary: string;
  current_task: string option;
  todos: string list;
  pdca_state: string option;
  relevant_files: string list;
  session_id: string option;
  relay_generation: int;  (* How many times relayed *)
}

(** Estimate context usage based on heuristics *)
let estimate_context ~messages ~tool_calls ~model =
  (* Rough token estimates per message type *)
  let tokens_per_user_msg = 150 in
  let tokens_per_assistant_msg = 500 in
  let tokens_per_tool_call = 200 in
  let tokens_per_tool_result = 300 in

  let message_tokens = messages * (tokens_per_user_msg + tokens_per_assistant_msg) in
  let tool_tokens = tool_calls * (tokens_per_tool_call + tokens_per_tool_result) in
  let estimated = message_tokens + tool_tokens in

  (* Model-specific max context *)
  let max_tokens = match model with
    | "claude" | "claude-sonnet" -> 200000
    | "claude-opus" -> 200000
    | "gemini" -> 1000000
    | "codex" | "gpt" -> 128000
    | _ -> 100000  (* conservative default *)
  in

  {
    estimated_tokens = estimated;
    max_tokens;
    usage_ratio = float_of_int estimated /. float_of_int max_tokens;
    message_count = messages;
    tool_call_count = tool_calls;
  }

(** Task complexity hints for proactive relay *)
type task_hint =
  | Large_file_read of string    (* About to read large file *)
  | Multi_file_edit of int       (* Editing N files *)
  | Long_running_task            (* Task marked as long *)
  | Exploration_task             (* Codebase exploration *)
  | Simple_task                  (* Quick task, no relay needed *)

(** Estimate additional context consumption *)
let estimate_task_cost = function
  | Large_file_read _ -> 10000   (* ~10K tokens for large file *)
  | Multi_file_edit n -> n * 3000  (* ~3K per file *)
  | Long_running_task -> 20000  (* Conservative estimate *)
  | Exploration_task -> 15000   (* Search results, etc *)
  | Simple_task -> 1000         (* Minimal *)

(** Proactive relay decision - key insight: predict before hitting limit *)
let should_relay_proactive ~config ~metrics ~task_hint =
  let predicted_cost = estimate_task_cost task_hint in
  let predicted_tokens = metrics.estimated_tokens + predicted_cost in
  let predicted_ratio = float_of_int predicted_tokens /. float_of_int metrics.max_tokens in

  (* Proactive: if predicted usage > threshold, relay NOW before the task *)
  predicted_ratio >= config.threshold

(** Check if relay is needed - reactive (legacy) *)
let should_relay ~config ~metrics =
  metrics.usage_ratio >= config.threshold

(** Smart relay decision - combines reactive + proactive *)
let should_relay_smart ~config ~metrics ~task_hint =
  (* Proactive: predict before task *)
  let proactive = should_relay_proactive ~config ~metrics ~task_hint in
  (* Reactive: already at limit *)
  let reactive = should_relay ~config ~metrics in

  if proactive && not reactive then
    `Proactive  (* Relay NOW before task consumes more *)
  else if reactive then
    `Reactive   (* Already at limit *)
  else
    `No_relay   (* Safe to continue *)

(** Compress context for handoff - extract essentials *)
let compress_context ~summary ~task ~todos ~pdca ~files =
  let sections = [] in

  (* Summary section *)
  let sections = ("## Context Summary\n" ^ summary) :: sections in

  (* Current task *)
  let sections = match task with
    | Some t -> ("## Current Task\n" ^ t) :: sections
    | None -> sections
  in

  (* TODOs *)
  let sections = match todos with
    | [] -> sections
    | _ ->
      let todo_str = String.concat "\n" (List.map (fun t -> "- " ^ t) todos) in
      ("## TODO List\n" ^ todo_str) :: sections
  in

  (* PDCA state *)
  let sections = match pdca with
    | Some p -> ("## PDCA State\n" ^ p) :: sections
    | None -> sections
  in

  (* Relevant files *)
  let sections = match files with
    | [] -> sections
    | _ ->
      let files_str = String.concat "\n" (List.map (fun f -> "- `" ^ f ^ "`") files) in
      ("## Relevant Files\n" ^ files_str) :: sections
  in

  String.concat "\n\n" (List.rev sections)

(** Build handoff prompt for the new agent *)
let build_handoff_prompt ~payload ~generation =
  let header = Printf.sprintf
    "🔄 **RELAY HANDOFF** (Generation %d)\n\n\
     You are continuing work from a previous agent session.\n\
     The previous agent's context was getting full, so it handed off to you.\n\n\
     **IMPORTANT**: Continue the work seamlessly. The user should not notice the transition.\n\n"
    generation
  in

  let context = compress_context
    ~summary:payload.summary
    ~task:payload.current_task
    ~todos:payload.todos
    ~pdca:payload.pdca_state
    ~files:payload.relevant_files
  in

  let footer = "\n\n---\n\
    **Instructions**:\n\
    1. Read the context above carefully\n\
    2. Continue working on the current task\n\
    3. Maintain the same tone and approach\n\
    4. If context is unclear, ask the user for clarification\n\
    5. Use MASC tools to coordinate if needed\n"
  in

  header ^ context ^ footer

(** Execute relay to new agent *)
let execute_relay ~config ~payload =
  let generation = payload.relay_generation + 1 in
  let prompt = build_handoff_prompt ~payload ~generation in

  (* Broadcast relay event *)
  let _ = Printf.printf "[RELAY] Handing off to %s (generation %d)\n%!"
    config.target_agent generation in

  (* Spawn new agent with handoff prompt *)
  let result = Spawn.spawn
    ~agent_name:config.target_agent
    ~prompt
    ~timeout_seconds:600  (* Longer timeout for complex continuations *)
    ()
  in

  (result, generation)

(** Checkpoint - saved state for smooth handoff *)
type checkpoint = {
  cp_timestamp: float;
  cp_summary: string;
  cp_task: string option;
  cp_todos: string list;
  cp_pdca: string option;
  cp_files: string list;
  cp_metrics: context_metrics;
}

(** Checkpoint storage (in-memory, could be persisted) *)
let checkpoints : checkpoint list ref = ref []

(** Save a checkpoint *)
let save_checkpoint ~summary ~task ~todos ~pdca ~files ~metrics =
  let cp = {
    cp_timestamp = Unix.gettimeofday ();
    cp_summary = summary;
    cp_task = task;
    cp_todos = todos;
    cp_pdca = pdca;
    cp_files = files;
    cp_metrics = metrics;
  } in
  checkpoints := cp :: !checkpoints;
  Printf.printf "[CHECKPOINT] Saved at %.1f%% context usage\n%!"
    (metrics.usage_ratio *. 100.0);
  cp

(** Get latest checkpoint *)
let get_latest_checkpoint () =
  match !checkpoints with
  | [] -> None
  | cp :: _ -> Some cp

(** Checkpoint to payload *)
let checkpoint_to_payload cp generation =
  {
    summary = cp.cp_summary;
    current_task = cp.cp_task;
    todos = cp.cp_todos;
    pdca_state = cp.cp_pdca;
    relevant_files = cp.cp_files;
    session_id = None;
    relay_generation = generation;
  }

(** Auto-relay check - call this periodically (reactive mode) *)
let auto_relay_check ~config ~messages ~tool_calls ~model ~payload_builder =
  let metrics = estimate_context ~messages ~tool_calls ~model in

  if should_relay ~config ~metrics then begin
    Printf.printf "[RELAY] Context at %.1f%%, triggering relay...\n%!"
      (metrics.usage_ratio *. 100.0);

    let payload = payload_builder () in
    Some (execute_relay ~config ~payload)
  end
  else
    None

(** Smart relay check - proactive mode with task hints *)
let smart_relay_check ~config ~messages ~tool_calls ~model ~task_hint ~payload_builder =
  let metrics = estimate_context ~messages ~tool_calls ~model in

  match should_relay_smart ~config ~metrics ~task_hint with
  | `Proactive ->
    Printf.printf "[RELAY/PROACTIVE] Predicted overflow before task, relaying now...\n%!";
    Printf.printf "  Current: %.1f%%, Task cost: ~%d tokens\n%!"
      (metrics.usage_ratio *. 100.0)
      (estimate_task_cost task_hint);
    let payload = payload_builder () in
    Some (`Proactive, execute_relay ~config ~payload)

  | `Reactive ->
    Printf.printf "[RELAY/REACTIVE] Context at %.1f%%, triggering relay...\n%!"
      (metrics.usage_ratio *. 100.0);
    let payload = payload_builder () in
    Some (`Reactive, execute_relay ~config ~payload)

  | `No_relay ->
    None

(** Auto-checkpoint at key moments *)
let auto_checkpoint ~metrics ~summary ~task ~todos ~pdca ~files =
  (* Save checkpoint at 50%, 70% thresholds *)
  let ratio = metrics.usage_ratio in
  let should_cp =
    (ratio >= 0.5 && ratio < 0.55) ||
    (ratio >= 0.7 && ratio < 0.75)
  in
  if should_cp then
    Some (save_checkpoint ~summary ~task ~todos ~pdca ~files ~metrics)
  else
    None

(** Metrics to JSON *)
let metrics_to_json metrics =
  `Assoc [
    ("estimated_tokens", `Int metrics.estimated_tokens);
    ("max_tokens", `Int metrics.max_tokens);
    ("usage_ratio", `Float metrics.usage_ratio);
    ("message_count", `Int metrics.message_count);
    ("tool_call_count", `Int metrics.tool_call_count);
  ]

(** Payload to JSON *)
let payload_to_json payload =
  `Assoc [
    ("summary", `String payload.summary);
    ("current_task", match payload.current_task with Some t -> `String t | None -> `Null);
    ("todos", `List (List.map (fun t -> `String t) payload.todos));
    ("pdca_state", match payload.pdca_state with Some p -> `String p | None -> `Null);
    ("relevant_files", `List (List.map (fun f -> `String f) payload.relevant_files));
    ("session_id", match payload.session_id with Some s -> `String s | None -> `Null);
    ("relay_generation", `Int payload.relay_generation);
  ]

(** Create empty payload *)
let empty_payload = {
  summary = "";
  current_task = None;
  todos = [];
  pdca_state = None;
  relevant_files = [];
  session_id = None;
  relay_generation = 0;
}
