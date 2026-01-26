(** MASC Spawn - Eio Native Agent subprocess management *)

(** Spawn configuration for an agent *)
type spawn_config = {
  agent_name: string;
  command: string;
  timeout_seconds: int;
  working_dir: string option;
  mcp_tools: string list;
}

(** Spawn result with token tracking *)
type spawn_result = {
  success: bool;
  output: string;
  exit_code: int;
  elapsed_ms: int;
  input_tokens: int option;
  output_tokens: int option;
  cache_creation_tokens: int option;
  cache_read_tokens: int option;
  cost_usd: float option;
}

(** MASC MCP tools available for spawned agents *)
let masc_mcp_tools = [
  "mcp__masc__masc_status";
  "mcp__masc__masc_tasks";
  "mcp__masc__masc_claim";
  "mcp__masc__masc_claim_next";
  "mcp__masc__masc_transition";
  "mcp__masc__masc_release";
  "mcp__masc__masc_task_history";
  "mcp__masc__masc_done";
  "mcp__masc__masc_broadcast";
  "mcp__masc__masc_join";
  "mcp__masc__masc_leave";
  "mcp__masc__masc_who";
  "mcp__masc__masc_agent_update";
  "mcp__masc__masc_lock";
  "mcp__masc__masc_unlock";
  "mcp__masc__masc_add_task";
  "mcp__masc__masc_heartbeat";
  "mcp__masc__masc_messages";
  "mcp__masc__masc_worktree_create";
  "mcp__masc__masc_worktree_remove";
  "mcp__masc__masc_worktree_list";
  "mcp__masc__masc_handover_create";
  "mcp__masc__masc_handover_list";
  "mcp__masc__masc_handover_claim";
  "mcp__masc__masc_handover_get";
  "mcp__masc__masc_memento_mori";
  "mcp__masc__masc_relay_status";
  "mcp__masc__masc_relay_checkpoint";
]

let masc_lifecycle_suffix = {| 
--- 
[MASC LIFECYCLE PROTOCOL - Auto-injected] 

You are running as a MASC-managed agent. Follow these rules strictly:

1. **Session Start**: Call `mcp__masc__masc_join` with your agent name
2. **Heartbeat**: Call `mcp__masc__masc_heartbeat` every 2 minutes during long tasks
3. **Context Monitoring**: Periodically call `mcp__masc__masc_memento_mori` with estimated context_ratio:
   - 0.0-0.5: Continue normally
   - 0.5-0.8: Prepare DNA (context summary) - will auto-prepare
   - 0.8+: Auto-handoff to successor agent
4. **Task Completion**: Call `mcp__masc__masc_transition` with action="done" then `mcp__masc__masc_leave`

IMPORTANT: If context_ratio exceeds 0.8, you MUST handoff. Do not ignore this. 
--- 
|}

let parse_claude_json output = 
  try
    let json = Yojson.Safe.from_string output in
    let open Yojson.Safe.Util in
    let usage = json |> member "usage" in
    let input_tokens = usage |> member "input_tokens" |> to_int_option in
    let output_tokens = usage |> member "output_tokens" |> to_int_option in
    let cache_creation = usage |> member "cache_creation_input_tokens" |> to_int_option in
    let cache_read = usage |> member "cache_read_input_tokens" |> to_int_option in
    let cost_usd = json |> member "total_cost_usd" |> to_float_option in
    let result_text = json |> member "result" |> to_string_option in
    (result_text, input_tokens, output_tokens, cache_creation, cache_read, cost_usd)
  with _ ->
    (Some output, None, None, None, None, None)

let default_configs = [
  ("claude", {
    agent_name = "claude";
    command = "claude --output-format json -p";
    timeout_seconds = 300;
    working_dir = None;
    mcp_tools = masc_mcp_tools;
  });
  ("gemini", {
    agent_name = "gemini";
    command = "gemini --yolo";
    timeout_seconds = 300;
    working_dir = None;
    mcp_tools = masc_mcp_tools;
  });
  ("codex", {
    agent_name = "codex";
    command = "codex exec";
    timeout_seconds = 300;
    working_dir = None;
    mcp_tools = masc_mcp_tools;
  });
  ("ollama", {
    agent_name = "ollama";
    command = "ollama run devstral";
    timeout_seconds = 300;
    working_dir = None;
    mcp_tools = [];
  });
]

let get_config agent_name = 
  List.assoc_opt agent_name default_configs

let build_mcp_flags agent_name tools = 
  if tools = [] then ""
  else match agent_name with 
  | "claude" -> 
    let tools_str = String.concat "," tools in 
    Printf.sprintf " --allowedTools %s" (Filename.quote tools_str)
  | "gemini" -> 
    let tools_args = String.concat " " (List.map Filename.quote tools) in 
    Printf.sprintf " --allowed-mcp-server-names masc --allowed-tools %s" tools_args
  | _ -> ""

(** Spawn an agent using Eio.Process *)
let spawn ~sw ~proc_mgr ~agent_name ~prompt ?timeout_seconds ?working_dir () =
  let start_time = Unix.gettimeofday () in

  let config = match get_config agent_name with
    | Some c -> c
    | None -> {
        agent_name;
        command = agent_name;
        timeout_seconds = Option.value timeout_seconds ~default:300;
        working_dir;
        mcp_tools = [];
      }
  in

  let timeout = Option.value timeout_seconds ~default:config.timeout_seconds in
  let mcp_flags = build_mcp_flags agent_name config.mcp_tools in
  let augmented_prompt = prompt ^ masc_lifecycle_suffix in

  (* Prepare arguments for shell execution *)
  let full_cmd_str = Printf.sprintf "echo %s | timeout %d %s%s"
    (Filename.quote augmented_prompt) timeout config.command mcp_flags in

  let original_dir = Sys.getcwd () in
  (match working_dir with Some d -> Sys.chdir d | None -> ());

  let result =
    try
      let output_buf = Buffer.create 4096 in
      let process = Eio.Process.spawn ~sw proc_mgr
        ~stdout:(Eio.Flow.buffer_sink output_buf)
        ["bash"; "-c"; full_cmd_str]
      in
      let status = Eio.Process.await process in
      let raw_output = Buffer.contents output_buf in
      let exit_code = match status with 
        | `Exited code -> code
        | `Signaled _ -> -1
      in 

      let (output, input_tokens, output_tokens, cache_creation, cache_read, cost_usd) = 
        if agent_name = "claude" then 
          let (result_opt, inp, out, cache_c, cache_r, cost) = parse_claude_json raw_output in 
          (Option.value result_opt ~default:raw_output, inp, out, cache_c, cache_r, cost)
        else 
          (raw_output, None, None, None, None, None)
      in 

      {
        success = (exit_code = 0);
        output;
        exit_code;
        elapsed_ms = int_of_float ((Unix.gettimeofday () -. start_time) *. 1000.0);
        input_tokens;
        output_tokens;
        cache_creation_tokens = cache_creation;
        cache_read_tokens = cache_read;
        cost_usd;
      }
    with e -> 
      {
        success = false;
        output = Printf.sprintf "Spawn error (Eio): %s" (Printexc.to_string e);
        exit_code = -99;
        elapsed_ms = int_of_float ((Unix.gettimeofday () -. start_time) *. 1000.0);
        input_tokens = None;
        output_tokens = None;
        cache_creation_tokens = None;
        cache_read_tokens = None;
        cost_usd = None;
      }
  in 
  Sys.chdir original_dir;
  result

let result_to_human_string result = 
  let token_info = 
    match result.input_tokens, result.output_tokens, result.cost_usd with 
    | Some inp, Some out, Some cost -> 
      let cache = match result.cache_creation_tokens, result.cache_read_tokens with 
        | Some cc, Some cr when cc > 0 || cr > 0 -> Printf.sprintf " (cache: +%d, %d)" cc cr
        | _ -> ""
      in 
      Printf.sprintf "\n📊 Tokens: %d in / %d out%s | Cost: $%.4f" inp out cache cost
    | _ -> ""
  in 
  if result.success then 
    Printf.sprintf "✅ Agent completed in %dms%s\n\n%s"
      result.elapsed_ms token_info result.output
  else 
    Printf.sprintf "❌ Agent failed (exit %d) in %dms%s\n\n%s"
      result.exit_code result.elapsed_ms token_info result.output
