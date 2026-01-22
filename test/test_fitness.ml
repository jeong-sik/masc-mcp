(** Test Fitness Selection Module *)

open Masc_mcp

let () = Random.self_init ()

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
    end else
      Sys.remove path

let with_temp_masc_dir f =
  let base =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "masc-fitness-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.)))
  in
  Unix.mkdir base 0o755;
  let config = Room.default_config base in
  let _ = Room.init config ~agent_name:None in
  try
    let result = f config in
    let _ = Room.reset config in
    rm_rf base;
    result
  with e ->
    let _ = Room.reset config in
    rm_rf base;
    raise e

let test_calculate_fitness () =
  let metrics : Metrics_store.agent_metrics = {
    agent_id = "claude";
    period_start = 0.0;
    period_end = 0.0;
    total_tasks = 10;
    completed_tasks = 8;
    failed_tasks = 2;
    avg_completion_time_s = 60.0;  (* 60s = good speed *)
    task_completion_rate = 0.8;    (* 80% *)
    error_rate = 0.2;              (* 20% *)
    handoff_success_rate = 0.9;    (* 90% *)
    unique_collaborators = ["gemini"; "codex"; "ollama"];  (* 3 collaborators *)
  } in
  let result = Fitness.calculate_fitness metrics in

  (* Verify components *)
  (* completion: 0.8 * 0.35 = 0.28 *)
  assert (result.breakdown.completion_component > 0.27 && result.breakdown.completion_component < 0.29);
  (* error: (1 - 0.2) * 0.25 = 0.2 *)
  assert (result.breakdown.error_component > 0.19 && result.breakdown.error_component < 0.21);
  (* speed: (60/60) * 0.15 = 0.15 *)
  assert (result.breakdown.speed_component > 0.14 && result.breakdown.speed_component < 0.16);
  (* handoff: 0.9 * 0.15 = 0.135 *)
  assert (result.breakdown.handoff_component > 0.13 && result.breakdown.handoff_component < 0.14);
  (* collaboration: (3/5) * 0.10 = 0.06 *)
  assert (result.breakdown.collaboration_component > 0.05 && result.breakdown.collaboration_component < 0.07);

  (* Total should be around 0.825 *)
  assert (result.fitness_score > 0.80 && result.fitness_score < 0.85);
  print_endline "✓ test_calculate_fitness passed"

let test_default_fitness () =
  let result = Fitness.default_fitness "new_agent" in
  assert (result.agent_id = "new_agent");
  assert (result.fitness_score = 0.5);  (* Neutral *)
  assert (Option.is_none result.metrics);
  print_endline "✓ test_default_fitness passed"

let test_roulette_selection () =
  (* Create agents with different fitness *)
  let agents = [
    { Fitness.agent_id = "high"; fitness_score = 0.9; metrics = None;
      breakdown = { completion_component = 0.0; error_component = 0.0;
                    speed_component = 0.0; handoff_component = 0.0;
                    collaboration_component = 0.0 } };
    { Fitness.agent_id = "low"; fitness_score = 0.1; metrics = None;
      breakdown = { completion_component = 0.0; error_component = 0.0;
                    speed_component = 0.0; handoff_component = 0.0;
                    collaboration_component = 0.0 } };
  ] in

  (* Run multiple selections *)
  let high_count = ref 0 in
  for _ = 1 to 100 do
    match Fitness.roulette_select agents with
    | Some r when r.agent_id = "high" -> incr high_count
    | _ -> ()
  done;

  (* High fitness agent should be selected more often (~90%) *)
  assert (!high_count > 70);
  print_endline "✓ test_roulette_selection passed"

let test_select_agent_elite () =
  with_temp_masc_dir (fun config ->
    Lwt_main.run (
      let open Lwt.Syntax in
      let now = Unix.gettimeofday () in

      (* Record metrics with different success rates *)
      let claude_metrics = List.init 10 (fun i ->
        { Metrics_store.id = Printf.sprintf "c%d" i; agent_id = "claude"; task_id = Printf.sprintf "ct%d" i;
          started_at = now -. 60.0; completed_at = Some now;
          success = i < 9;  (* 9/10 = 90% *)
          error_message = None; collaborators = []; handoff_from = None; handoff_to = None }
      ) in
      let gemini_metrics = List.init 10 (fun i ->
        { Metrics_store.id = Printf.sprintf "g%d" i; agent_id = "gemini"; task_id = Printf.sprintf "gt%d" i;
          started_at = now -. 60.0; completed_at = Some now;
          success = i < 5;  (* 5/10 = 50% *)
          error_message = None; collaborators = []; handoff_from = None; handoff_to = None }
      ) in
      let* () = Lwt_list.iter_s (Metrics_store.record config) claude_metrics in
      let* () = Lwt_list.iter_s (Metrics_store.record config) gemini_metrics in

      (* Select with Elite strategy *)
      let* result = Fitness.select_agent config
          ~strategy:(Fitness.Elite 1)
          ~days:7
          ~available_agents:["claude"; "gemini"]
          () in
      match result with
      | Some r ->
        (* Claude should be selected (higher fitness) *)
        assert (r.agent_id = "claude");
        Lwt.return_unit
      | None -> failwith "Expected agent selection"
    )
  );
  print_endline "✓ test_select_agent_elite passed"

let test_strategy_conversion () =
  assert (Fitness.strategy_to_string Fitness.Roulette_wheel = "roulette_wheel");
  assert (Fitness.strategy_to_string (Fitness.Elite 3) = "elite_3");
  assert (Fitness.strategy_to_string Fitness.Capability_first = "capability_first");
  assert (Fitness.strategy_to_string Fitness.Random = "random");

  assert (Fitness.strategy_of_string "roulette_wheel" = Fitness.Roulette_wheel);
  assert (Fitness.strategy_of_string "elite_5" = Fitness.Elite 5);
  assert (Fitness.strategy_of_string "random" = Fitness.Random);
  print_endline "✓ test_strategy_conversion passed"

let () =
  print_endline "\n=== Fitness Selection Tests ===\n";
  test_calculate_fitness ();
  test_default_fitness ();
  test_roulette_selection ();
  test_select_agent_elite ();
  test_strategy_conversion ();
  print_endline "\nAll fitness tests passed! ✓"
