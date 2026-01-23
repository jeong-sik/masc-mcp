(** Integration Tests for Level 2 Modules

    End-to-end tests that verify Level 2 modules work together:
    - Metrics → Fitness selection
    - Task completion → Hebbian learning
    - Handoff → Drift guard
*)

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
      (Printf.sprintf "masc-integration-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.)))
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

(** Integration: Metrics → Fitness Selection *)
let test_metrics_to_fitness () =
  with_temp_masc_dir (fun config ->
    Lwt_main.run (
      let open Lwt.Syntax in
      let now = Unix.gettimeofday () in

      (* Record metrics for two agents with different performance *)
      let claude_metrics = [
        { Metrics_store.id = "m1"; agent_id = "claude"; task_id = "t1";
          started_at = now -. 100.0; completed_at = Some (now -. 50.0);
          success = true; error_message = None; collaborators = [];
          handoff_from = None; handoff_to = None };
        { Metrics_store.id = "m2"; agent_id = "claude"; task_id = "t2";
          started_at = now -. 50.0; completed_at = Some now;
          success = true; error_message = None; collaborators = [];
          handoff_from = None; handoff_to = None };
      ] in
      let gemini_metrics = [
        { Metrics_store.id = "m3"; agent_id = "gemini"; task_id = "t3";
          started_at = now -. 100.0; completed_at = Some now;
          success = false; error_message = Some "error"; collaborators = [];
          handoff_from = None; handoff_to = None };
      ] in

      let* () = Lwt_list.iter_s (Metrics_store.record config) claude_metrics in
      let* () = Lwt_list.iter_s (Metrics_store.record config) gemini_metrics in

      (* Calculate fitness - claude should be higher (100% success vs 0%) *)
      let* claude_metrics_opt = Metrics_store.calculate_agent_metrics config ~agent_id:"claude" ~days:7 in
      let* gemini_metrics_opt = Metrics_store.calculate_agent_metrics config ~agent_id:"gemini" ~days:7 in

      match claude_metrics_opt, gemini_metrics_opt with
      | Some cm, Some gm ->
        let claude_result = Fitness.calculate_fitness cm in
        let gemini_result = Fitness.calculate_fitness gm in

        (* Claude's fitness should be significantly higher *)
        assert (claude_result.fitness_score > gemini_result.fitness_score);
        assert (claude_result.fitness_score > 0.5);  (* Good performer *)
        assert (gemini_result.fitness_score < 0.3);  (* Poor performer *)
        Lwt.return_unit
      | _ -> failwith "Expected metrics for both agents"
    )
  );
  print_endline "✓ test_metrics_to_fitness passed"

(** Integration: Task completion → Hebbian learning *)
let test_task_hebbian_integration () =
  with_temp_masc_dir (fun config ->
    Lwt_main.run (
      let open Lwt.Syntax in

      (* Simulate successful collaboration: claude hands off to gemini *)
      let* () = Hebbian.strengthen config ~from_agent:"claude" ~to_agent:"gemini" () in

      (* Simulate failed collaboration *)
      let* () = Hebbian.weaken config ~from_agent:"claude" ~to_agent:"codex" () in

      (* Check preferred partner - should be gemini (higher weight) *)
      let* preferred = Hebbian.get_preferred_partner config ~agent_id:"claude" in
      let () = assert (preferred = Some "gemini") in

      (* Record metrics with collaborator info *)
      let now = Unix.gettimeofday () in
      let metric = {
        Metrics_store.id = "m-collab";
        agent_id = "claude";
        task_id = "task-collab";
        started_at = now -. 60.0;
        completed_at = Some now;
        success = true;
        error_message = None;
        collaborators = ["gemini"];
        handoff_from = None;
        handoff_to = Some "gemini";
      } in
      let* () = Metrics_store.record config metric in

      (* Verify collaborator tracked in metrics *)
      let* agent_metrics = Metrics_store.calculate_agent_metrics config ~agent_id:"claude" ~days:7 in
      let () = match agent_metrics with
        | Some m -> assert (List.mem "gemini" m.unique_collaborators)
        | None -> failwith "Expected metrics"
      in
      Lwt.return_unit
    )
  );
  print_endline "✓ test_task_hebbian_integration passed"

(** Integration: Handoff → Drift guard *)
let test_handoff_drift_integration () =
  with_temp_masc_dir (fun config ->
    Lwt_main.run (
      let open Lwt.Syntax in

      (* Simulate handoff with good context preservation *)
      let original = "Task PK-12345: Implement user authentication with OAuth2.0 support. Current status: Frontend login form completed." in
      let received = "Task PK-12345: Implement user authentication with OAuth2.0 support. Current status: Frontend login form done." in

      let* result = Drift_guard.verify_and_log config
        ~from_agent:"claude" ~to_agent:"gemini"
        ~task_id:"PK-12345"
        ~original ~received () in

      let () = match result with
        | Drift_guard.Verified { similarity } ->
          assert (similarity > 0.85)  (* Should be verified - contexts are similar *)
        | Drift_guard.Drift_detected _ ->
          failwith "Expected verification to pass for similar contexts"
      in

      (* Simulate handoff with significant drift *)
      let bad_original = "We need to use PostgreSQL for database and Redis for caching." in
      let bad_received = "We'll use MongoDB for database and Memcached for caching." in

      let* result2 = Drift_guard.verify_and_log config
        ~from_agent:"gemini" ~to_agent:"codex"
        ~task_id:"PK-12346"
        ~original:bad_original ~received:bad_received () in

      let () = match result2 with
        | Drift_guard.Drift_detected { similarity; _ } ->
          assert (similarity < 0.85)  (* Should detect drift *)
        | Drift_guard.Verified _ ->
          failwith "Expected drift detection for different contexts"
      in

      (* Check drift statistics *)
      let* (total, drift_count, _avg_sim) = Drift_guard.get_drift_stats config ~days:7 in
      let () = assert (total = 2) in
      let () = assert (drift_count = 1) in  (* One drift detected *)

      Lwt.return_unit
    )
  );
  print_endline "✓ test_handoff_drift_integration passed"

(** Integration: Full workflow simulation *)
let test_full_workflow () =
  with_temp_masc_dir (fun config ->
    Lwt_main.run (
      let open Lwt.Syntax in
      let now = Unix.gettimeofday () in

      (* Step 1: Record initial metrics for agents *)
      let metrics = [
        (* Claude: 2 success, 1 fail *)
        { Metrics_store.id = "wf-1"; agent_id = "claude"; task_id = "t1";
          started_at = now -. 300.0; completed_at = Some (now -. 250.0);
          success = true; error_message = None; collaborators = ["gemini"];
          handoff_from = None; handoff_to = None };
        { Metrics_store.id = "wf-2"; agent_id = "claude"; task_id = "t2";
          started_at = now -. 200.0; completed_at = Some (now -. 150.0);
          success = true; error_message = None; collaborators = [];
          handoff_from = None; handoff_to = None };
        { Metrics_store.id = "wf-3"; agent_id = "claude"; task_id = "t3";
          started_at = now -. 100.0; completed_at = Some (now -. 50.0);
          success = false; error_message = Some "timeout"; collaborators = [];
          handoff_from = None; handoff_to = None };
        (* Gemini: 3 success *)
        { Metrics_store.id = "wf-4"; agent_id = "gemini"; task_id = "t4";
          started_at = now -. 300.0; completed_at = Some (now -. 280.0);
          success = true; error_message = None; collaborators = [];
          handoff_from = None; handoff_to = None };
        { Metrics_store.id = "wf-5"; agent_id = "gemini"; task_id = "t5";
          started_at = now -. 200.0; completed_at = Some (now -. 180.0);
          success = true; error_message = None; collaborators = [];
          handoff_from = None; handoff_to = None };
        { Metrics_store.id = "wf-6"; agent_id = "gemini"; task_id = "t6";
          started_at = now -. 100.0; completed_at = Some (now -. 80.0);
          success = true; error_message = None; collaborators = [];
          handoff_from = None; handoff_to = None };
      ] in
      let* () = Lwt_list.iter_s (Metrics_store.record config) metrics in

      (* Step 2: Record Hebbian relationships *)
      let* () = Hebbian.strengthen config ~from_agent:"claude" ~to_agent:"gemini" () in
      let* () = Hebbian.strengthen config ~from_agent:"claude" ~to_agent:"gemini" () in
      let* () = Hebbian.strengthen config ~from_agent:"gemini" ~to_agent:"claude" () in

      (* Step 3: Select best agent based on fitness *)
      let available_agents = ["claude"; "gemini"; "codex"] in

      (* Use Fitness.select_agent to pick best agent *)
      let* selected = Fitness.select_agent config
        ~strategy:(Fitness.Elite 1)
        ~available_agents
        () in

      (* Gemini should be selected (100% success vs 66.7%) *)
      let () = match selected with
        | Some result -> assert (result.agent_id = "gemini")
        | None -> failwith "Expected agent to be selected"
      in

      (* Step 4: Verify Hebbian graph reflects collaboration *)
      let* graph_text = Hebbian.graph_to_text config in
      let () = assert (String.length graph_text > 0) in

      Lwt.return_unit
    )
  );
  print_endline "✓ test_full_workflow passed"

let () =
  print_endline "\n=== Level 2 Integration Tests ===\n";
  test_metrics_to_fitness ();
  test_task_hebbian_integration ();
  test_handoff_drift_integration ();
  test_full_workflow ();
  print_endline "\nAll Level 2 integration tests passed! ✓"
