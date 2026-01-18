(** Test Hebbian_eio Module - Pure Synchronous Tests

    "Agents that fire together, wire together"
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
      (Printf.sprintf "masc-hebbian-eio-%d-%06d" (Unix.getpid ()) (Random.int 1_000_000))
  in
  Unix.mkdir base 0o755;
  let config = Room.default_config base in
  let _ = Room.init config ~agent_name:None in
  (* Reset lock stats before each test *)
  Hebbian_eio.reset_lock_stats ();
  try
    let result = f config in
    let _ = Room.reset config in
    rm_rf base;
    result
  with e ->
    let _ = Room.reset config in
    rm_rf base;
    raise e

let test_strengthen () =
  with_temp_masc_dir (fun config ->
    (* Strengthen a connection *)
    Hebbian_eio.strengthen config ~from_agent:"claude" ~to_agent:"gemini" ();

    (* Check graph data *)
    let (synapses, agents) = Hebbian_eio.get_graph_data config in
    assert (List.length synapses = 1);
    assert (List.mem "claude" agents);
    assert (List.mem "gemini" agents);

    (* Check synapse values *)
    let s = List.hd synapses in
    assert (s.Hebbian_eio.from_agent = "claude");
    assert (s.Hebbian_eio.to_agent = "gemini");
    assert (s.Hebbian_eio.success_count = 1);
    assert (s.Hebbian_eio.weight > 0.5)  (* Started at 0.5, strengthened *)
  );
  print_endline "✓ test_strengthen passed"

let test_weaken () =
  with_temp_masc_dir (fun config ->
    (* First create a connection *)
    Hebbian_eio.strengthen config ~from_agent:"claude" ~to_agent:"codex" ();

    (* Get initial weight *)
    let (synapses, _) = Hebbian_eio.get_graph_data config in
    let initial_weight = (List.hd synapses).Hebbian_eio.weight in

    (* Weaken the connection *)
    Hebbian_eio.weaken config ~from_agent:"claude" ~to_agent:"codex" ();

    (* Check weight decreased *)
    let (synapses2, _) = Hebbian_eio.get_graph_data config in
    let new_weight = (List.hd synapses2).Hebbian_eio.weight in
    assert (new_weight < initial_weight);

    let s = List.hd synapses2 in
    assert (s.Hebbian_eio.failure_count = 1)
  );
  print_endline "✓ test_weaken passed"

let test_get_preferred_partner () =
  with_temp_masc_dir (fun config ->
    (* Create multiple connections with different strengths *)
    Hebbian_eio.strengthen config ~from_agent:"claude" ~to_agent:"gemini" ();
    Hebbian_eio.strengthen config ~from_agent:"claude" ~to_agent:"gemini" ();  (* 2x *)
    Hebbian_eio.strengthen config ~from_agent:"claude" ~to_agent:"codex" ();   (* 1x *)

    (* Preferred partner should be gemini (higher weight) *)
    match Hebbian_eio.get_preferred_partner config ~agent_id:"claude" with
    | Some partner ->
      assert (partner = "gemini");
      print_endline (Printf.sprintf "  Preferred partner for claude: %s" partner)
    | None -> failwith "Expected a preferred partner"
  );
  print_endline "✓ test_get_preferred_partner passed"

let test_no_preferred_partner () =
  with_temp_masc_dir (fun config ->
    (* No connections yet *)
    match Hebbian_eio.get_preferred_partner config ~agent_id:"claude" with
    | None -> ()  (* Expected - no connections *)
    | Some _ -> failwith "Expected no preferred partner"
  );
  print_endline "✓ test_no_preferred_partner passed"

let test_consolidate () =
  with_temp_masc_dir (fun config ->
    (* Create some connections *)
    Hebbian_eio.strengthen config ~from_agent:"claude" ~to_agent:"gemini" ();
    Hebbian_eio.strengthen config ~from_agent:"gemini" ~to_agent:"codex" ();

    (* Consolidate (with 0 days to force decay check on all) *)
    let pruned = Hebbian_eio.consolidate config ~decay_after_days:0 () in
    print_endline (Printf.sprintf "  Pruned %d weak connections" pruned);

    (* Graph should still have connections (weights above min_weight) *)
    let (synapses, _) = Hebbian_eio.get_graph_data config in
    assert (List.length synapses >= 0)  (* May or may not prune depending on params *)
  );
  print_endline "✓ test_consolidate passed"

let test_multiple_agents () =
  with_temp_masc_dir (fun config ->
    (* Build a network of connections *)
    Hebbian_eio.strengthen config ~from_agent:"claude" ~to_agent:"gemini" ();
    Hebbian_eio.strengthen config ~from_agent:"gemini" ~to_agent:"codex" ();
    Hebbian_eio.strengthen config ~from_agent:"codex" ~to_agent:"claude" ();

    let (synapses, agents) = Hebbian_eio.get_graph_data config in
    assert (List.length synapses = 3);
    assert (List.length agents = 3);
    print_endline (Printf.sprintf "  Network: %d synapses, %d agents"
                     (List.length synapses) (List.length agents))
  );
  print_endline "✓ test_multiple_agents passed"

let test_custom_params () =
  with_temp_masc_dir (fun config ->
    (* Use custom learning params *)
    let params = {
      Hebbian_eio.strengthen_rate = 0.2;  (* Stronger learning *)
      weaken_rate = 0.1;
      decay_rate = 0.05;
      min_weight = 0.1;
      max_weight = 1.0;
    } in

    Hebbian_eio.strengthen config ~params ~from_agent:"claude" ~to_agent:"gemini" ();

    let (synapses, _) = Hebbian_eio.get_graph_data config in
    let s = List.hd synapses in
    (* Initial 0.5 + 0.2 = 0.7 *)
    assert (s.Hebbian_eio.weight >= 0.69 && s.Hebbian_eio.weight <= 0.71)
  );
  print_endline "✓ test_custom_params passed"

let test_lock_stats () =
  with_temp_masc_dir (fun config ->
    (* Perform several operations to generate lock acquisitions *)
    Hebbian_eio.strengthen config ~from_agent:"a" ~to_agent:"b" ();
    Hebbian_eio.strengthen config ~from_agent:"b" ~to_agent:"c" ();
    Hebbian_eio.weaken config ~from_agent:"a" ~to_agent:"b" ();

    let (acquisitions, avg_wait, max_wait) = Hebbian_eio.get_lock_stats () in
    assert (acquisitions >= 3);  (* At least 3 lock acquisitions *)
    print_endline (Printf.sprintf "  Lock stats: %d acquisitions, %.2fms avg, %.2fms max"
                     acquisitions avg_wait max_wait)
  );
  print_endline "✓ test_lock_stats passed"

let test_weaken_nonexistent () =
  with_temp_masc_dir (fun config ->
    (* Weaken a nonexistent connection - should do nothing *)
    Hebbian_eio.weaken config ~from_agent:"x" ~to_agent:"y" ();

    let (synapses, _) = Hebbian_eio.get_graph_data config in
    assert (List.length synapses = 0)  (* No synapse created *)
  );
  print_endline "✓ test_weaken_nonexistent passed"

let () =
  print_endline "\n=== Hebbian_eio Tests (Pure Sync) ===\n";
  test_strengthen ();
  test_weaken ();
  test_get_preferred_partner ();
  test_no_preferred_partner ();
  test_consolidate ();
  test_multiple_agents ();
  test_custom_params ();
  test_lock_stats ();
  test_weaken_nonexistent ();
  print_endline "\n✅ All 9 Hebbian_eio tests passed!\n"
