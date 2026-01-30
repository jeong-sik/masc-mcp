(** MASC Orchestrator - Self-sustaining agent coordination *)

(** Orchestrator configuration *)
type config = {
  check_interval_s: float;      (* How often to check (default: 300s = 5min) *)
  min_priority: int;            (* Minimum task priority to trigger (default: 1) *)
  agent_timeout_s: int;         (* Timeout for spawned orchestrator (default: 300) *)
  orchestrator_agent: string;   (* Which agent to spawn as orchestrator (default: claude) *)
  enabled: bool;                (* Is auto-orchestration enabled *)
  port: int;                    (* MASC HTTP port for API calls *)
}

let default_config = {
  check_interval_s = 300.0;
  min_priority = 2;
  agent_timeout_s = 300;
  orchestrator_agent = "claude";
  enabled = false;  (* Disabled by default - opt-in *)
  port = 8935;      (* Default MASC HTTP port *)
}

(** Load config from environment or use defaults *)
let load_config () =
  let get_env_float name default =
    match Sys.getenv_opt name with
    | Some v -> Safe_ops.float_of_string_with_default ~default v
    | None -> default
  in
  let get_env_int name default =
    match Sys.getenv_opt name with
    | Some v -> Safe_ops.int_of_string_with_default ~default v
    | None -> default
  in
  let get_env_bool name default =
    match Sys.getenv_opt name with
    | Some "1" | Some "true" | Some "yes" -> true
    | Some "0" | Some "false" | Some "no" -> false
    | _ -> default
  in
  {
    check_interval_s = get_env_float "MASC_ORCHESTRATOR_INTERVAL" 300.0;
    min_priority = get_env_int "MASC_ORCHESTRATOR_MIN_PRIORITY" 2;
    agent_timeout_s = get_env_int "MASC_ORCHESTRATOR_TIMEOUT" 300;
    orchestrator_agent = (match Sys.getenv_opt "MASC_ORCHESTRATOR_AGENT" with
      | Some a -> a | None -> "claude");
    enabled = get_env_bool "MASC_ORCHESTRATOR_ENABLED" false;
    port = get_env_int "MASC_PORT" 8935;
  }

(** Check if orchestration is needed *)
let should_orchestrate room_config =
  (* Check if room is paused first *)
  if Room.is_paused room_config then begin
    Printf.printf "⏸️ Orchestrator: Room is paused, skipping\n%!";
    false
  end else begin
  (* Get unclaimed tasks with priority <= min_priority *)
  let tasks = Room.get_tasks_raw room_config in
  let unclaimed_important = List.filter (fun (task: Types.task) ->
    task.task_status = Types.Todo && task.priority <= 2
  ) tasks in

  (* Get active (non-zombie) agents *)
  let agents = Room.get_agents_raw room_config in
  let active_agents = List.filter (fun (agent: Types.agent) ->
    not (Resilience.Zombie.is_zombie agent.last_seen)
  ) agents in

  (* Need orchestration if: important tasks exist AND no active agents *)
  let needs_orchestration =
    List.length unclaimed_important > 0 && List.length active_agents = 0
  in

  if needs_orchestration then
    Printf.printf "🎯 Orchestrator: %d unclaimed tasks, %d active agents → spawning\n%!"
      (List.length unclaimed_important) (List.length active_agents);

  needs_orchestration
  end  (* end of else begin for pause check *)

(** The orchestrator prompt - MCP tools are now available via --allowedTools! *)
let make_orchestrator_prompt ~port:_ =
  {|You are the MASC Orchestrator Agent.

You have access to MASC MCP tools via mcp__masc__* prefix.

## Your Tasks:

1. **Check status**: Call `mcp__masc__masc_status` to see the room state

2. **Find unclaimed tasks**: Look for tasks with "📋" (unclaimed) status

3. **Claim a task**: Call `mcp__masc__masc_claim` with:
   - agent_name: "orchestrator"
   - task_id: "task-XXX"

4. **Work on the task**: Execute the task description

5. **Mark done**: Call `mcp__masc__masc_done` with completion notes

6. **Broadcast progress**: Call `mcp__masc__masc_broadcast` to notify others

## Available MCP Tools:
- mcp__masc__masc_status - Get room status
- mcp__masc__masc_tasks - List all tasks
- mcp__masc__masc_claim - Claim a task
- mcp__masc__masc_claim_next - Auto-claim highest priority
- mcp__masc__masc_done - Mark task complete
- mcp__masc__masc_broadcast - Send message to all
- mcp__masc__masc_heartbeat - Update your heartbeat

Start by calling mcp__masc__masc_status to see the current room state.|}

(** Spawn the orchestrator agent (Eio-friendly, runs in a domain if provided) *)
let spawn_orchestrator ~sw ~proc_mgr ?domain_mgr config room_config =
  (* TOCTOU defense: re-check pause before spawn *)
  if Room.is_paused room_config then begin
    Printf.printf "⏸️ Orchestrator: Room paused before spawn, aborting\n%!";
    { Spawn_eio.success = false; output = "Room paused"; exit_code = 0; elapsed_ms = 0;
      input_tokens = None; output_tokens = None; cache_creation_tokens = None;
      cache_read_tokens = None; cost_usd = None }
  end else begin
  Printf.printf "🚀 Spawning orchestrator agent: %s (with MCP tools)\n%!" config.orchestrator_agent;

  (* Broadcast that orchestrator is starting *)
  let _ = Room.broadcast room_config ~from_agent:"system"
    ~content:"🎯 Auto-orchestrator activated - spawning coordinator with MCP tools" in

  let prompt = make_orchestrator_prompt ~port:config.port in
  let run () =
    Spawn_eio.spawn
      ~sw
      ~proc_mgr
      ~agent_name:config.orchestrator_agent
      ~prompt
      ~timeout_seconds:config.agent_timeout_s
      ()
  in
  let result =
    match domain_mgr with
    | Some dm -> Eio.Domain_manager.run dm run
    | None -> run ()
  in

  if result.success then
    Printf.printf "✅ Orchestrator completed in %dms\n%!" result.elapsed_ms
  else
    Printf.printf "❌ Orchestrator failed (exit %d) in %dms\n%!"
      result.exit_code result.elapsed_ms;

  result
  end

(** Main orchestrator loop *)
let rec run_loop ~sw ~proc_mgr ~clock ?domain_mgr config room_config () =
  if not config.enabled then (
    (* Disabled - just sleep and check again later in case config changes *)
    Eio.Time.sleep clock 60.0;
    run_loop ~sw ~proc_mgr ~clock ?domain_mgr config room_config ()
  ) else (
    let needs_orchestration = should_orchestrate room_config in

    if needs_orchestration then
      Eio.Fiber.fork ~sw (fun () ->
        try
          ignore (spawn_orchestrator ~sw ~proc_mgr ?domain_mgr config room_config)
        with exn ->
          Printf.eprintf "[Orchestrator] spawn failed: %s\n%!" (Printexc.to_string exn)
      );

    (* Wait for next check interval *)
    Eio.Time.sleep clock config.check_interval_s;
    run_loop ~sw ~proc_mgr ~clock ?domain_mgr config room_config ()
  )

(** Start the orchestrator background loop *)
let start ~sw ~proc_mgr ~clock ?domain_mgr room_config =
  let config = load_config () in

  (* Start Zero-Zombie background cleanup loop *)
  Eio.Fiber.fork ~sw (fun () ->
    Printf.printf "🧟 Zero-Zombie Protocol: Automatic cleanup enabled (interval: 60s)\n%!";
    Resilience.ZeroZombie.run_loop ~interval:60.0 ~clock
      ~cleanup_fn:(fun () ->
        let status = Room.cleanup_zombies room_config in
        (* Extract agent names from status message if any were cleaned *)
        if String.length status > 0 && String.sub status 0 (min 3 (String.length status)) = "✅" then
          (* For simplicity in the protocol stats, we just log that something happened.
             Actual room status parsing is complex, so we return a dummy list to signal success. *)
          ["(automatic-cleanup)"]
        else []
      ) ());

  if config.enabled then (
    Printf.printf "🎮 Orchestrator loop enabled (interval: %.0fs, agent: %s)\n%!"
      config.check_interval_s config.orchestrator_agent;
    Eio.Fiber.fork ~sw (fun () ->
      try run_loop ~sw ~proc_mgr ~clock ?domain_mgr config room_config ()
      with exn ->
        Printf.eprintf "[Orchestrator] loop crashed: %s\n%!" (Printexc.to_string exn))
  ) else (
    Printf.printf "💤 Orchestrator loop disabled (set MASC_ORCHESTRATOR_ENABLED=1 to enable)\n%!"
  )
