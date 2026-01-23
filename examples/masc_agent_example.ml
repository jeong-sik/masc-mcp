(** MASC Agent Example - agent_core integration for multi-agent coordination

    This example demonstrates how to integrate llm_mcp.agent_core with
    MASC-MCP for coordinated multi-agent workflows.

    Prerequisites:
    1. opam pin add llm_mcp git+ssh://git@github.com/jeong-sik/llm-mcp.git#main
    2. Add "llm_mcp.agent_core" to your dune libraries

    Usage:
    dune exec ./examples/masc_agent_example.exe
*)

open Lwt.Syntax

(** {1 MASC Tool Executor} *)

module Masc_Tools = struct
  open Agent_core.Types

  (** Simulated room/task management tools *)
  let execute (tc : tool_call) : (tool_result, string) result Lwt.t =
    let open Yojson.Safe.Util in
    match tc.name with
    | "create_room" ->
      let name = tc.arguments |> member "name" |> to_string_option |> Option.value ~default:"default" in
      let room_id = Printf.sprintf "room_%d" (Random.int 10000) in
      Lwt.return (Result.Ok (ToolSuccess (Printf.sprintf "Created room '%s' with ID: %s" name room_id)))

    | "broadcast" ->
      let room_id = tc.arguments |> member "room_id" |> to_string_option |> Option.value ~default:"" in
      let message = tc.arguments |> member "message" |> to_string_option |> Option.value ~default:"" in
      if room_id = "" || message = "" then
        Lwt.return (Result.Ok (ToolError "Missing room_id or message"))
      else
        Lwt.return (Result.Ok (ToolSuccess (Printf.sprintf "Broadcasted to %s: %s" room_id message)))

    | "create_task" ->
      let title = tc.arguments |> member "title" |> to_string_option |> Option.value ~default:"" in
      let assignee = tc.arguments |> member "assignee" |> to_string_option in
      let task_id = Printf.sprintf "task_%d" (Random.int 10000) in
      let msg = match assignee with
        | Some a -> Printf.sprintf "Created task '%s' (ID: %s) assigned to %s" title task_id a
        | None -> Printf.sprintf "Created task '%s' (ID: %s) unassigned" title task_id
      in
      Lwt.return (Result.Ok (ToolSuccess msg))

    | "claim_task" ->
      let task_id = tc.arguments |> member "task_id" |> to_string_option |> Option.value ~default:"" in
      let agent = tc.arguments |> member "agent" |> to_string_option |> Option.value ~default:"default_agent" in
      if task_id = "" then
        Lwt.return (Result.Ok (ToolError "Missing task_id"))
      else
        Lwt.return (Result.Ok (ToolSuccess (Printf.sprintf "Task %s claimed by %s" task_id agent)))

    | "complete_task" ->
      let task_id = tc.arguments |> member "task_id" |> to_string_option |> Option.value ~default:"" in
      let result = tc.arguments |> member "result" |> to_string_option |> Option.value ~default:"completed" in
      if task_id = "" then
        Lwt.return (Result.Ok (ToolError "Missing task_id"))
      else
        Lwt.return (Result.Ok (ToolSuccess (Printf.sprintf "Task %s completed: %s" task_id result)))

    | "list_agents" ->
      Lwt.return (Result.Ok (ToolSuccess "Available agents: coordinator, worker_1, worker_2, reviewer"))

    | _ -> Lwt.return (Result.Error ("Unknown tool: " ^ tc.name))

  let to_message (tc : tool_call) (result : tool_result) : message =
    let content = match result with
      | ToolSuccess s -> s
      | ToolError e -> "Error: " ^ e
    in
    { role = Tool; content; tool_calls = None; name = Some tc.id }

  let available_tools () =
    ["create_room"; "broadcast"; "create_task"; "claim_task"; "complete_task"; "list_agents"]
end

(** {1 Tool Definitions} *)

let masc_tools : Agent_core.Types.tool list = [
  {
    name = "create_room";
    description = "Create a new collaboration room for agents";
    parameters = [
      ("name", { Agent_core.Types.param_type = "string"; description = "Room name"; required = true; enum = None });
    ];
  };
  {
    name = "broadcast";
    description = "Broadcast a message to all agents in a room";
    parameters = [
      ("room_id", { Agent_core.Types.param_type = "string"; description = "Room ID"; required = true; enum = None });
      ("message", { Agent_core.Types.param_type = "string"; description = "Message to broadcast"; required = true; enum = None });
    ];
  };
  {
    name = "create_task";
    description = "Create a new task that can be claimed by agents";
    parameters = [
      ("title", { Agent_core.Types.param_type = "string"; description = "Task title"; required = true; enum = None });
      ("assignee", { Agent_core.Types.param_type = "string"; description = "Optional agent to assign"; required = false; enum = None });
    ];
  };
  {
    name = "claim_task";
    description = "Claim a task for an agent to work on";
    parameters = [
      ("task_id", { Agent_core.Types.param_type = "string"; description = "Task ID"; required = true; enum = None });
      ("agent", { Agent_core.Types.param_type = "string"; description = "Agent claiming the task"; required = true; enum = None });
    ];
  };
  {
    name = "complete_task";
    description = "Mark a task as completed with results";
    parameters = [
      ("task_id", { Agent_core.Types.param_type = "string"; description = "Task ID"; required = true; enum = None });
      ("result", { Agent_core.Types.param_type = "string"; description = "Completion result or output"; required = true; enum = None });
    ];
  };
  {
    name = "list_agents";
    description = "List all available agents in the system";
    parameters = [];
  };
]

(** {1 Agent Loop Setup} *)

module Coordinator_Agent = Agent_core.Agent_loop_functor.Make
    (Agent_core.Ollama_backend)
    (Masc_Tools)
    (Agent_core.Default_state)

(** {1 Main} *)

let () =
  Random.self_init ();

  let model = ref "qwen3:1.7b" in
  let task = ref "Coordinate a team to review and improve code quality" in

  let usage = "MASC Agent Example - Multi-agent coordination demo" in
  let specs = [
    ("--model", Arg.Set_string model, "Ollama model name (default: qwen3:1.7b)");
    ("--task", Arg.Set_string task, "Coordination task description");
  ] in
  Arg.parse specs (fun _ -> ()) usage;

  let backend_config = Agent_core.Ollama_backend.{
    base_url = "http://127.0.0.1:11434";
    model = !model;
    temperature = 0.5;  (* Slightly higher for creative coordination *)
    stream = false;
    timeout_ms = Some 120_000;
  } in

  let loop_config = Agent_core.Types.{
    default_loop_config with
    max_turns = 15;  (* More turns for coordination *)
    timeout_ms = 300_000;
    max_messages = 100;
  } in

  let system_prompt = {|You are a MASC (Multi-Agent System Coordinator). Your role is to:
1. Create rooms for collaboration
2. Create and assign tasks to available agents
3. Track task completion and coordinate handoffs
4. Broadcast important updates to the team

Available agents: coordinator (you), worker_1, worker_2, reviewer

Use the tools to orchestrate the team and complete the given objective.|} in

  let prompt = Printf.sprintf "%s\n\nObjective: %s" system_prompt !task in

  Printf.printf "Starting MASC Coordinator Agent with model: %s\n" !model;
  Printf.printf "Task: %s\n\n" !task;

  let result = Lwt_main.run (
    Coordinator_Agent.run
      ~config:loop_config
      ~backend_config
      ~initial_prompt:prompt
      ~tools:masc_tools
      ()
  ) in

  match result with
  | Agent_core.Types.Completed { response; turns_used } ->
    Printf.printf "\n=== Coordination completed in %d turns ===\n%s\n" turns_used response
  | Agent_core.Types.MaxTurnsReached { last_response; turns_used } ->
    Printf.printf "\n=== Max turns (%d) reached ===\n%s\n" turns_used last_response
  | Agent_core.Types.TimedOut { turns_completed } ->
    Printf.printf "\n=== Timed out after %d turns ===\n" turns_completed
  | Agent_core.Types.Error msg ->
    Printf.printf "\n=== Error ===\n%s\n" msg
  | Agent_core.Types.CircuitOpen ->
    Printf.printf "\n=== Circuit breaker open ===\n"
