(** Room_walph_eio: Eio-native Walph (Walph Wiggum variant) implementation

    Inspired by Geoffrey Huntley's Walph (Walph Wiggum variant) technique:
    @see https://ghuntley.com/ralph/

    Production-ready implementation using Eio concurrency primitives:
    - Eio.Mutex for thread-safe state access (fiber-friendly, non-blocking)
    - Eio.Condition for pause/resume (no busy-wait, proper fiber scheduling)
    - Fun.protect for exception safety (no zombie states)

    This module is designed to run inside Eio fiber context.
    For pure sync/testing, use Room.walph_* functions instead.

    @see Room for sync implementation (testing)
    @see mcp_server_eio for production usage
*)

(** {1 Types} *)

(** Walph (Walph Wiggum variant) state with Eio concurrency primitives *)
type walph_state = {
  mutable running : bool;
  mutable paused : bool;
  mutable stop_requested : bool;
  mutable current_preset : string;
  mutable iterations : int;
  mutable completed : int;
  mutex : Eio.Mutex.t;        (** Eio-native mutex (fiber-friendly) *)
  cond : Eio.Condition.t;     (** Eio-native condition variable *)
}

(** {1 State Management} *)

(** Global Walph state table with Eio mutex for thread-safe access *)
let walph_states : (string, walph_state) Hashtbl.t = Hashtbl.create 16
let walph_states_mutex = Eio.Mutex.create ()

(** Escape colon in agent_name to prevent key collision.
    "agent:foo" -> "agent::foo" (double colon as escape)
    Key format: "room||agent" using || as separator (unlikely in paths/names) *)
let escape_agent_name name =
  (* Replace | with || to escape, then use | as separator *)
  let buf = Buffer.create (String.length name) in
  String.iter (fun c ->
    if c = '|' then Buffer.add_string buf "||"
    else Buffer.add_char buf c
  ) name;
  Buffer.contents buf

(** Generate state key: room||agent for independent Walph per agent
    Uses || separator to avoid collision with : in paths *)
let make_walph_key config ~agent_name =
  if agent_name = "" then
    failwith "Walph: agent_name cannot be empty"
  else
    Printf.sprintf "%s||%s" config.Room_utils.base_path (escape_agent_name agent_name)

(** Get or create Walph state for a specific agent (thread-safe)
    Each agent has independent Walph state, enabling parallel loops *)
let get_walph_state config ~agent_name =
  let key = make_walph_key config ~agent_name in
  Eio.Mutex.use_rw ~protect:true walph_states_mutex (fun () ->
    match Hashtbl.find_opt walph_states key with
    | Some s -> s
    | None ->
        let s = {
          running = false; paused = false; stop_requested = false;
          current_preset = ""; iterations = 0; completed = 0;
          mutex = Eio.Mutex.create ();
          cond = Eio.Condition.create ();
        } in
        Hashtbl.replace walph_states key s;
        s
  )

(** Remove Walph state for an agent (cleanup to prevent memory leak)
    @return Error if Walph is still running (zombie prevention), Ok () otherwise *)
let remove_walph_state config ~agent_name =
  let key = make_walph_key config ~agent_name in
  let result = Eio.Mutex.use_rw ~protect:true walph_states_mutex (fun () ->
    match Hashtbl.find_opt walph_states key with
    | None -> Ok ()  (* Already removed, no-op *)
    | Some state ->
        if state.running then
          Error (Printf.sprintf "Walph: cannot remove state for %s while running. Call STOP first." agent_name)
        else begin
          Hashtbl.remove walph_states key;
          Ok ()
        end
  ) in
  match result with
  | Ok () -> ()
  | Error msg -> failwith msg  (* Raise outside mutex to avoid poisoning *)

(** List all active Walph states in a room (for swarm coordination)
    Uses || as separator to match make_walph_key *)
let list_walph_states config =
  let prefix = config.Room_utils.base_path ^ "||" in
  let prefix_len = String.length prefix in
  Eio.Mutex.use_rw ~protect:true walph_states_mutex (fun () ->
    Hashtbl.fold (fun key state acc ->
      if String.length key > prefix_len &&
         String.sub key 0 prefix_len = prefix then
        (* Extract agent name (still escaped, but usable as identifier) *)
        let agent = String.sub key prefix_len (String.length key - prefix_len) in
        (agent, state) :: acc
      else acc
    ) walph_states []
  )

(** Run function with Walph state mutex locked (Eio-native) *)
let with_walph_lock state f =
  Eio.Mutex.use_rw ~protect:true state.mutex f

(** {1 Control Commands} *)

(** Handle @walph control command (Eio-native, fiber-safe)
    @param config Room configuration
    @param from_agent Agent sending the command (controls own Walph)
    @param command Command (STOP, PAUSE, RESUME, STATUS)
    @param args Command arguments
    @param target_agent Optional: control another agent's Walph (default: self)
    @return Response message *)
let walph_control config ~from_agent ~command ~args ?(target_agent=None) () =
  let agent_name = match target_agent with Some a -> a | None -> from_agent in
  let state = get_walph_state config ~agent_name in
  let response = with_walph_lock state (fun () ->
    match command with
    | "STOP" ->
        if state.running then begin
          state.stop_requested <- true;
          Eio.Condition.broadcast state.cond;  (* Wake up paused fibers *)
          Printf.sprintf "🛑 @walph STOP requested by %s (will stop after current iteration)" from_agent
        end else
          "ℹ️ @walph is not currently running"
    | "PAUSE" ->
        if state.running && not state.paused then begin
          state.paused <- true;
          Printf.sprintf "⏸️ @walph PAUSED by %s (use @walph RESUME to continue)" from_agent
        end else if state.paused then
          "ℹ️ @walph is already paused"
        else
          "ℹ️ @walph is not currently running"
    | "RESUME" ->
        if state.paused then begin
          state.paused <- false;
          Eio.Condition.broadcast state.cond;  (* Wake up paused fibers *)
          Printf.sprintf "▶️ @walph RESUMED by %s" from_agent
        end else if state.running then
          "ℹ️ @walph is already running"
        else
          "ℹ️ @walph is not currently running"
    | "STATUS" ->
        if state.running then
          Printf.sprintf "📊 @walph STATUS: %s (iter: %d, done: %d, paused: %b)"
            state.current_preset state.iterations state.completed state.paused
        else
          "ℹ️ @walph is idle (use @walph START <preset> to begin)"
    | "START" ->
        (* START is handled by walph_loop, just acknowledge here *)
        if state.running then
          Printf.sprintf "⚠️ @walph is already running %s. Use @walph STOP first." state.current_preset
        else
          Printf.sprintf "✨ @walph START acknowledged. Args: %s" args
    | _ ->
        Printf.sprintf "❓ Unknown @walph command: %s. Valid: START, STOP, PAUSE, RESUME, STATUS" command
  ) in
  (* Broadcast the response *)
  let _ = Room.broadcast config ~from_agent:"walph" ~content:response in
  response

(** Check if Walph should continue looping (Eio-native, no busy-wait)
    Uses Eio.Condition for proper fiber scheduling.
    @param agent_name The agent whose Walph state to check
    @return true if should continue, false if should stop *)
let walph_should_continue config ~agent_name =
  let state = get_walph_state config ~agent_name in
  (* Note: Eio.Condition.await requires mutex to be held, and atomically
     releases it while waiting, then reacquires before returning *)
  Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
    if state.stop_requested then false
    else if state.paused then begin
      (* Wait on condition variable - Eio-native, no busy-wait!
         This yields to other fibers while waiting *)
      while state.paused && not state.stop_requested do
        Eio.Condition.await_no_mutex state.cond
      done;
      not state.stop_requested
    end else true
  )

(** {1 Preset Mapping} *)

(** Map Walph preset to llm-mcp chain ID
    @param preset The loop preset (coverage, refactor, docs, drain)
    @return Some chain_id for presets with corresponding chains, None for drain *)
let get_chain_id_for_preset = function
  | "coverage" -> Some "walph-coverage"
  | "refactor" -> Some "walph-refactor"
  | "docs" -> Some "walph-docs"
  | "drain" -> None  (* No chain for simple drain *)
  | "figma" -> Some "walph-figma"
  | _ -> None

(** {1 Main Loop} *)

(** Walph (Walph Wiggum variant) pattern: Keep claiming tasks until stop condition
    Eio-native implementation with fiber-safe concurrency.

    @param net Eio network capability (for llm-mcp chain calls)
    @param preset Loop preset (drain, coverage, refactor, docs)
    @param max_iterations Maximum iterations before forced stop
    @param target Target file/directory for preset
    @return Status string with loop results *)
let walph_loop config ~net ~agent_name ?(preset="drain") ?(max_iterations=10) ?target () =
  Room.ensure_initialized config;

  (* Get Walph state for this specific agent *)
  let walph_state = get_walph_state config ~agent_name in

  (* Atomic check-and-set to prevent double-start race condition *)
  let start_result = with_walph_lock walph_state (fun () ->
    if walph_state.running then
      Error (Printf.sprintf "⚠️ @walph is already running %s. Use @walph STOP first." walph_state.current_preset)
    else begin
      (* Atomically set running=true under lock *)
      walph_state.running <- true;
      walph_state.paused <- false;
      walph_state.stop_requested <- false;
      walph_state.current_preset <- preset;
      walph_state.iterations <- 0;
      walph_state.completed <- 0;
      Ok ()
    end
  ) in

  match start_result with
  | Error msg ->
      let _ = Room.broadcast config ~from_agent:"walph" ~content:msg in
      msg
  | Ok () ->
      (* Use Fun.protect to ensure running <- false even on exceptions (zombie prevention) *)
      let stop_reason = ref "" in

      Fun.protect
        ~finally:(fun () ->
          (* Always reset running state, even on exception *)
          with_walph_lock walph_state (fun () ->
            walph_state.running <- false
          ))
        (fun () ->
          let _ = Room.broadcast config ~from_agent:agent_name
            ~content:(Printf.sprintf "🔄 @walph START %s%s (max: %d)"
              preset
              (match target with Some t -> " --target " ^ t | None -> "")
              max_iterations) in

          (* UTF-8 safe prefix check: 📋=4bytes, ✅=3bytes, ❌=3bytes *)
          let starts_with prefix s =
            let plen = String.length prefix in
            String.length s >= plen && String.sub s 0 plen = prefix
          in

          (* Run the loop *)
          let rec loop () =
            (* Check control state before each iteration *)
            if not (walph_should_continue config ~agent_name) then begin
              stop_reason := if walph_state.stop_requested then "stop requested" else "paused indefinitely";
              ()
            end else begin
              (* Check max iterations with lock *)
              let should_stop = with_walph_lock walph_state (fun () ->
                if walph_state.iterations >= max_iterations then begin
                  stop_reason := Printf.sprintf "max_iterations reached (%d)" max_iterations;
                  true
                end else begin
                  walph_state.iterations <- walph_state.iterations + 1;
                  false
                end
              ) in
              if should_stop then ()
              else begin
                (* Try to claim next task *)
                let claim_result = Room.claim_next config ~agent_name in

                if starts_with "📋" claim_result || starts_with "No unclaimed" claim_result then begin
                  (* No more unclaimed tasks - drain complete *)
                  stop_reason := "backlog drained";
                  ()
                end else if starts_with "✅" claim_result then begin
                  (* Extract task ID from claim result.
                     Format: "✅ agent auto-claimed [P3] task-XXX: Title" *)
                  let task_id =
                    try
                      let re = Str.regexp {|\(task-[0-9]+\)|} in
                      if Str.string_match re claim_result 0 then
                        Some (Str.matched_group 1 claim_result)
                      else if Str.search_forward re claim_result 0 >= 0 then
                        Some (Str.matched_group 1 claim_result)
                      else None
                    with _ -> None
                  in

                  (* Execute chain if preset has one (not drain) *)
                  let should_chain = get_chain_id_for_preset preset <> None in
                  let chain_result =
                    if not should_chain then
                      (* Drain mode: no chain, just claim/done *)
                      Ok "drain mode - no chain"
                    else begin
                      (* Build goal from task info and preset *)
                      let task_title, task_desc = match task_id with
                        | Some tid ->
                            let tasks = Room.get_tasks_raw config in
                            (match List.find_opt (fun (t : Types.task) -> t.id = tid) tasks with
                             | Some t -> (t.title, t.description)
                             | None -> (tid, ""))
                        | None -> ("unknown task", "")
                      in
                      let goal = match preset with
                        | "coverage" ->
                            Printf.sprintf "Improve test coverage for: %s. %s. Add comprehensive tests with edge cases." task_title task_desc
                        | "refactor" ->
                            Printf.sprintf "Refactor the following: %s. %s. Improve code quality, reduce complexity, follow best practices." task_title task_desc
                        | "docs" ->
                            Printf.sprintf "Create or improve documentation for: %s. %s. Include examples and clear explanations." task_title task_desc
                        | _ ->
                            Printf.sprintf "Complete this task: %s. %s" task_title task_desc
                      in
                      let _ = Room.broadcast config ~from_agent:agent_name
                        ~content:(Printf.sprintf "🔗 @walph calling chain.orchestrate for '%s'..." task_title) in
                      Llm_client_eio.call_chain ~net ~goal ()
                    end
                  in

                  (match chain_result with
                   | Ok result ->
                       with_walph_lock walph_state (fun () ->
                         walph_state.completed <- walph_state.completed + 1
                       );
                       (* Mark task as done *)
                       (match task_id with
                        | Some tid ->
                            let notes_str = Printf.sprintf "Chain result: %s" (String.sub result 0 (min 100 (String.length result))) in
                            let _ = Room.transition_task_r config ~agent_name ~task_id:tid ~action:"done" ~notes:notes_str () in
                            ()
                        | None -> ());
                       (* Broadcast progress *)
                       let _ = Room.broadcast config ~from_agent:agent_name
                         ~content:(Printf.sprintf "📊 @walph Iteration %d: %s ✅" walph_state.iterations claim_result) in
                       loop ()
                   | Error e ->
                       let err_msg = match e with
                         | Llm_client_eio.ConnectionError s -> "Connection: " ^ s
                         | Llm_client_eio.ParseError s -> "Parse: " ^ s
                         | Llm_client_eio.ServerError (code, s) -> Printf.sprintf "Server(%d): %s" code s
                         | Llm_client_eio.Timeout -> "Timeout"
                       in
                       let _ = Room.broadcast config ~from_agent:agent_name
                         ~content:(Printf.sprintf "⚠️ @walph chain error: %s (continuing...)" err_msg) in
                       (* Continue even on chain error *)
                       loop ())
                end else begin
                  (* Error or unexpected result *)
                  stop_reason := Printf.sprintf "claim error: %s" claim_result;
                  ()
                end
              end
            end
          in

          loop ();

          (* Final broadcast and log *)
          let result = Printf.sprintf
            "🛑 @walph STOPPED. Preset: %s, Iterations: %d, Tasks completed: %d, Reason: %s"
            preset walph_state.iterations walph_state.completed !stop_reason in

          let _ = Room.broadcast config ~from_agent:agent_name ~content:result in

          Room.log_event config (Printf.sprintf
            "{\"type\":\"walph_loop_complete\",\"agent\":\"%s\",\"preset\":\"%s\",\"iterations\":%d,\"completed\":%d,\"reason\":\"%s\",\"ts\":\"%s\"}"
            agent_name preset walph_state.iterations walph_state.completed !stop_reason (Types.now_iso ()));

          result
        )

(** {1 Swarm Walph - Multi-Agent Coordination} *)

(** Swarm status summary for all active Walph instances in a room *)
type swarm_status = {
  total_agents: int;
  running_count: int;
  paused_count: int;
  completed_tasks: int;
  total_iterations: int;
  agents: (string * walph_state) list;
}

(** Get comprehensive swarm status across all Walph instances
    @param config Room configuration
    @return Swarm status summary *)
let swarm_walph_status config =
  let agents = list_walph_states config in
  let running = List.filter (fun (_, s) -> s.running) agents in
  let paused = List.filter (fun (_, s) -> s.paused) agents in
  let total_completed = List.fold_left (fun acc (_, s) -> acc + s.completed) 0 agents in
  let total_iters = List.fold_left (fun acc (_, s) -> acc + s.iterations) 0 agents in
  {
    total_agents = List.length agents;
    running_count = List.length running;
    paused_count = List.length paused;
    completed_tasks = total_completed;
    total_iterations = total_iters;
    agents;
  }

(** Format swarm status for display *)
let format_swarm_status status =
  let agent_lines = List.map (fun (name, s) ->
    Printf.sprintf "  • %s: %s (iter: %d, done: %d)%s"
      name
      (if s.running then (if s.paused then "⏸️ PAUSED" else "🔄 RUNNING") else "⚪ IDLE")
      s.iterations s.completed
      (if s.running then " [" ^ s.current_preset ^ "]" else "")
  ) status.agents in
  Printf.sprintf
    "🐝 **Swarm Walph Status**\n\
     ├─ Agents: %d total, %d running, %d paused\n\
     ├─ Tasks completed: %d\n\
     ├─ Total iterations: %d\n\
     └─ Details:\n%s"
    status.total_agents status.running_count status.paused_count
    status.completed_tasks status.total_iterations
    (if agent_lines = [] then "  (no agents)" else String.concat "\n" agent_lines)

(** Stop all running Walph instances in a swarm
    @param config Room configuration
    @param from_agent Agent issuing the stop command
    @return Summary of stopped instances *)
let swarm_walph_stop config ~from_agent =
  let agents = list_walph_states config in
  let running_agents = List.filter (fun (_, s) -> s.running) agents in

  if running_agents = [] then
    "ℹ️ No running Walph instances to stop"
  else begin
    let stopped = List.map (fun (agent_name, state) ->
      with_walph_lock state (fun () ->
        if state.running then begin
          state.stop_requested <- true;
          Eio.Condition.broadcast state.cond;
          agent_name
        end else ""
      )
    ) running_agents in

    let stopped_list = List.filter ((<>) "") stopped in
    let _ = Room.broadcast config ~from_agent:"walph-swarm"
      ~content:(Printf.sprintf "🛑 SWARM STOP by %s: %d agents signaled" from_agent (List.length stopped_list)) in

    Printf.sprintf "🛑 Swarm stop requested for %d agents: %s"
      (List.length stopped_list)
      (String.concat ", " stopped_list)
  end

(** Pause all running Walph instances in a swarm
    @param config Room configuration
    @param from_agent Agent issuing the pause command
    @return Summary of paused instances *)
let swarm_walph_pause config ~from_agent =
  let agents = list_walph_states config in
  let running_agents = List.filter (fun (_, s) -> s.running && not s.paused) agents in

  if running_agents = [] then
    "ℹ️ No running (un-paused) Walph instances to pause"
  else begin
    let paused = List.map (fun (agent_name, state) ->
      with_walph_lock state (fun () ->
        if state.running && not state.paused then begin
          state.paused <- true;
          agent_name
        end else ""
      )
    ) running_agents in

    let paused_list = List.filter ((<>) "") paused in
    let _ = Room.broadcast config ~from_agent:"walph-swarm"
      ~content:(Printf.sprintf "⏸️ SWARM PAUSE by %s: %d agents paused" from_agent (List.length paused_list)) in

    Printf.sprintf "⏸️ Swarm pause applied to %d agents: %s"
      (List.length paused_list)
      (String.concat ", " paused_list)
  end

(** Resume all paused Walph instances in a swarm
    @param config Room configuration
    @param from_agent Agent issuing the resume command
    @return Summary of resumed instances *)
let swarm_walph_resume config ~from_agent =
  let agents = list_walph_states config in
  let paused_agents = List.filter (fun (_, s) -> s.paused) agents in

  if paused_agents = [] then
    "ℹ️ No paused Walph instances to resume"
  else begin
    let resumed = List.map (fun (agent_name, state) ->
      with_walph_lock state (fun () ->
        if state.paused then begin
          state.paused <- false;
          Eio.Condition.broadcast state.cond;
          agent_name
        end else ""
      )
    ) paused_agents in

    let resumed_list = List.filter ((<>) "") resumed in
    let _ = Room.broadcast config ~from_agent:"walph-swarm"
      ~content:(Printf.sprintf "▶️ SWARM RESUME by %s: %d agents resumed" from_agent (List.length resumed_list)) in

    Printf.sprintf "▶️ Swarm resume applied to %d agents: %s"
      (List.length resumed_list)
      (String.concat ", " resumed_list)
  end

(** Command pattern for swarm control *)
let swarm_walph_control config ~from_agent ~command () =
  match String.uppercase_ascii command with
  | "STATUS" ->
      let status = swarm_walph_status config in
      let formatted = format_swarm_status status in
      let _ = Room.broadcast config ~from_agent:"walph-swarm" ~content:formatted in
      formatted
  | "STOP" ->
      swarm_walph_stop config ~from_agent
  | "PAUSE" ->
      swarm_walph_pause config ~from_agent
  | "RESUME" ->
      swarm_walph_resume config ~from_agent
  | cmd ->
      Printf.sprintf "❓ Unknown swarm command: %s. Valid: STATUS, STOP, PAUSE, RESUME" cmd
