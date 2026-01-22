(** Test Metrics_store_eio Module - Pure Synchronous Tests *)

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
      (Printf.sprintf "masc-metrics-eio-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.)))
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

let test_create_metric () =
  let metric = Metrics_store_eio.create_metric
    ~agent_id:"claude"
    ~task_id:"task-001"
    ~collaborators:["gemini"]
    ()
  in
  assert (metric.agent_id = "claude");
  assert (metric.task_id = "task-001");
  assert (List.mem "gemini" metric.collaborators);
  assert (metric.success = false);  (* Default *)
  assert (Option.is_none metric.completed_at);
  print_endline "✓ test_create_metric passed"

let test_complete_metric () =
  let metric = Metrics_store_eio.create_metric
    ~agent_id:"gemini"
    ~task_id:"task-002"
    ()
  in
  let completed = Metrics_store_eio.complete_metric metric
    ~success:true
    ~handoff_to:"codex"
    ()
  in
  assert (completed.success = true);
  assert (Option.is_some completed.completed_at);
  assert (completed.handoff_to = Some "codex");
  print_endline "✓ test_complete_metric passed"

let test_record_and_get () =
  with_temp_masc_dir (fun config ->
    (* Create and record a metric *)
    let metric = Metrics_store_eio.create_metric
      ~agent_id:"claude"
      ~task_id:"task-record"
      ()
    in
    let completed = Metrics_store_eio.complete_metric metric ~success:true () in
    Metrics_store_eio.record config completed;

    (* Get recent metrics *)
    let recent = Metrics_store_eio.get_recent config ~agent_id:"claude" ~days:1 in
    assert (List.length recent >= 1);

    (* Find our metric *)
    let found = List.exists (fun m -> m.Metrics_store_eio.task_id = "task-record") recent in
    assert found
  );
  print_endline "✓ test_record_and_get passed"

let test_calculate_agent_metrics () =
  with_temp_masc_dir (fun config ->
    (* Record several metrics *)
    for i = 1 to 5 do
      let metric = Metrics_store_eio.create_metric
        ~agent_id:"claude"
        ~task_id:(Printf.sprintf "calc-task-%d" i)
        ()
      in
      let success = (i mod 2 = 0) in  (* Alternate success/fail *)
      let completed = Metrics_store_eio.complete_metric metric ~success () in
      Metrics_store_eio.record config completed
    done;

    (* Calculate aggregated metrics *)
    match Metrics_store_eio.calculate_agent_metrics config ~agent_id:"claude" ~days:1 with
    | Some metrics ->
      assert (metrics.total_tasks = 5);
      assert (metrics.completed_tasks = 5);
      (* 2 and 4 are successful (even numbers) *)
      assert (metrics.task_completion_rate >= 0.3 && metrics.task_completion_rate <= 0.5);
      print_endline (Printf.sprintf "  Completion rate: %.1f%%" (metrics.task_completion_rate *. 100.0))
    | None -> failwith "Expected to get agent metrics"
  );
  print_endline "✓ test_calculate_agent_metrics passed"

let test_get_all_agents () =
  with_temp_masc_dir (fun config ->
    (* Record metrics for multiple agents *)
    let agents = ["claude"; "gemini"; "codex"] in
    List.iter (fun agent ->
      let metric = Metrics_store_eio.create_metric
        ~agent_id:agent
        ~task_id:(Printf.sprintf "%s-task" agent)
        ()
      in
      let completed = Metrics_store_eio.complete_metric metric ~success:true () in
      Metrics_store_eio.record config completed
    ) agents;

    (* Get all agents *)
    let all = Metrics_store_eio.get_all_agents config in
    assert (List.length all = 3);
    List.iter (fun a -> assert (List.mem a all)) agents
  );
  print_endline "✓ test_get_all_agents passed"

let test_collaborators () =
  with_temp_masc_dir (fun config ->
    let metric = Metrics_store_eio.create_metric
      ~agent_id:"claude"
      ~task_id:"collab-task"
      ~collaborators:["gemini"; "codex"]
      ()
    in
    let completed = Metrics_store_eio.complete_metric metric ~success:true () in
    Metrics_store_eio.record config completed;

    match Metrics_store_eio.calculate_agent_metrics config ~agent_id:"claude" ~days:1 with
    | Some metrics ->
      assert (List.mem "gemini" metrics.unique_collaborators);
      assert (List.mem "codex" metrics.unique_collaborators)
    | None -> failwith "Expected metrics"
  );
  print_endline "✓ test_collaborators passed"

let test_handoff_tracking () =
  with_temp_masc_dir (fun config ->
    (* Record handoff metrics *)
    let m1 = Metrics_store_eio.create_metric
      ~agent_id:"claude"
      ~task_id:"handoff-1"
      ~handoff_from:"gemini"
      ()
    in
    let c1 = Metrics_store_eio.complete_metric m1 ~success:true () in
    Metrics_store_eio.record config c1;

    let m2 = Metrics_store_eio.create_metric
      ~agent_id:"claude"
      ~task_id:"handoff-2"
      ()
    in
    let c2 = Metrics_store_eio.complete_metric m2 ~success:true ~handoff_to:"codex" () in
    Metrics_store_eio.record config c2;

    match Metrics_store_eio.calculate_agent_metrics config ~agent_id:"claude" ~days:1 with
    | Some metrics ->
      assert (metrics.handoff_success_rate = 1.0);  (* Both handoffs successful *)
      print_endline (Printf.sprintf "  Handoff success rate: %.0f%%" (metrics.handoff_success_rate *. 100.0))
    | None -> failwith "Expected metrics"
  );
  print_endline "✓ test_handoff_tracking passed"

let test_generate_id () =
  let id1 = Metrics_store_eio.generate_id () in
  let id2 = Metrics_store_eio.generate_id () in
  assert (id1 <> id2);  (* Should be unique *)
  assert (String.length id1 > 10);
  print_endline "✓ test_generate_id passed"

let () =
  print_endline "\n=== Metrics_store_eio Tests (Pure Sync) ===\n";
  test_create_metric ();
  test_complete_metric ();
  test_record_and_get ();
  test_calculate_agent_metrics ();
  test_get_all_agents ();
  test_collaborators ();
  test_handoff_tracking ();
  test_generate_id ();
  print_endline "\n✅ All 8 Metrics_store_eio tests passed!\n"
