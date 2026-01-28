(** Test Swarm (Level 4) Module - Pure Function Tests *)

open Masc_mcp

let () = Random.init 42

(** Helper: Create a fresh swarm for testing *)
let make_test_swarm ?(behavior = Swarm.Flocking) () : Swarm.swarm =
  let now = Unix.gettimeofday () in
  let swarm_cfg : Swarm.swarm_config = {
    id = "test-swarm";
    name = "Test Swarm";
    selection_pressure = 0.5;
    mutation_rate = 0.1;
    evaporation_rate = 0.05;
    quorum_threshold = 0.6;
    max_agents = 10;
    behavior;
  } in
  {
    swarm_cfg;
    agents = [];
    pheromones = [];
    proposals = [];
    generation = 0;
    created_at = now;
    last_evolution = now;
  }

(** Helper: Create a test agent *)
let make_test_agent ~id ~name ~fitness ~generation : Swarm.swarm_agent =
  let now = Unix.gettimeofday () in
  {
    id;
    name;
    fitness;
    generation;
    mutations = [];
    joined_at = now;
    last_active = now;
  }

(* ============================================ *)
(* Pure.join_agent tests                        *)
(* ============================================ *)

let test_join_agent_empty_swarm () =
  let swarm = make_test_swarm () in
  let now = Unix.gettimeofday () in
  match Swarm.Pure.join_agent swarm ~agent_id:"claude" ~agent_name:"Claude" ~now with
  | Swarm.Pure.Joined new_swarm ->
    assert (List.length new_swarm.agents = 1);
    let agent = List.hd new_swarm.agents in
    assert (agent.id = "claude");
    assert (agent.name = "Claude");
    assert (agent.fitness = 0.5);  (* default fitness *)
    Printf.printf "✓ test_join_agent_empty_swarm\n%!"
  | _ -> failwith "Expected Joined result"

let test_join_agent_already_member () =
  let swarm = make_test_swarm () in
  let now = Unix.gettimeofday () in
  (* First join *)
  let swarm = match Swarm.Pure.join_agent swarm ~agent_id:"claude" ~agent_name:"Claude" ~now with
    | Swarm.Pure.Joined s -> s
    | _ -> failwith "First join should succeed"
  in
  (* Second join - should return Already_member *)
  match Swarm.Pure.join_agent swarm ~agent_id:"claude" ~agent_name:"Claude" ~now with
  | Swarm.Pure.Already_member _ ->
    Printf.printf "✓ test_join_agent_already_member\n%!"
  | _ -> failwith "Expected Already_member result"

let test_join_agent_swarm_full () =
  let base = make_test_swarm () in
  let swarm = { base with
    swarm_cfg = { base.swarm_cfg with max_agents = 2 };
    agents = [
      make_test_agent ~id:"a1" ~name:"A1" ~fitness:0.5 ~generation:0;
      make_test_agent ~id:"a2" ~name:"A2" ~fitness:0.5 ~generation:0;
    ]
  } in
  let now = Unix.gettimeofday () in
  match Swarm.Pure.join_agent swarm ~agent_id:"a3" ~agent_name:"A3" ~now with
  | Swarm.Pure.Swarm_full ->
    Printf.printf "✓ test_join_agent_swarm_full\n%!"
  | _ -> failwith "Expected Swarm_full result"

(* ============================================ *)
(* Pure.leave_agent tests                       *)
(* ============================================ *)

let test_leave_agent () =
  let swarm = make_test_swarm () in
  let now = Unix.gettimeofday () in
  let swarm = match Swarm.Pure.join_agent swarm ~agent_id:"claude" ~agent_name:"Claude" ~now with
    | Swarm.Pure.Joined s -> s
    | _ -> failwith "Join should succeed"
  in
  assert (List.length swarm.agents = 1);
  let swarm = Swarm.Pure.leave_agent swarm ~agent_id:"claude" in
  assert (List.length swarm.agents = 0);
  Printf.printf "✓ test_leave_agent\n%!"

let test_leave_agent_not_member () =
  let swarm = make_test_swarm () in
  let swarm = Swarm.Pure.leave_agent swarm ~agent_id:"nonexistent" in
  assert (List.length swarm.agents = 0);
  Printf.printf "✓ test_leave_agent_not_member\n%!"

(* ============================================ *)
(* Pure.update_agent_fitness tests              *)
(* ============================================ *)

let test_update_fitness () =
  let swarm = make_test_swarm () in
  let now = Unix.gettimeofday () in
  let swarm = match Swarm.Pure.join_agent swarm ~agent_id:"claude" ~agent_name:"Claude" ~now with
    | Swarm.Pure.Joined s -> s
    | _ -> failwith "Join should succeed"
  in
  match Swarm.Pure.update_agent_fitness swarm ~agent_id:"claude" ~fitness:0.85 ~now with
  | Some updated ->
    let agent = List.find (fun (a : Swarm.swarm_agent) -> a.id = "claude") updated.agents in
    assert (agent.fitness = 0.85);
    Printf.printf "✓ test_update_fitness\n%!"
  | None -> failwith "Update should succeed"

let test_update_fitness_not_found () =
  let swarm = make_test_swarm () in
  let now = Unix.gettimeofday () in
  match Swarm.Pure.update_agent_fitness swarm ~agent_id:"nonexistent" ~fitness:0.85 ~now with
  | None -> Printf.printf "✓ test_update_fitness_not_found\n%!"
  | Some _ -> failwith "Expected None for nonexistent agent"

(* ============================================ *)
(* Pure.fitness_rankings tests                  *)
(* ============================================ *)

let test_fitness_rankings () =
  let swarm = { (make_test_swarm ()) with
    agents = [
      make_test_agent ~id:"a1" ~name:"A1" ~fitness:0.3 ~generation:0;
      make_test_agent ~id:"a2" ~name:"A2" ~fitness:0.9 ~generation:0;
      make_test_agent ~id:"a3" ~name:"A3" ~fitness:0.6 ~generation:0;
    ]
  } in
  let rankings = Swarm.Pure.fitness_rankings swarm in
  assert (List.length rankings = 3);
  (* Should be sorted descending by fitness *)
  let (id1, f1) = List.nth rankings 0 in
  let (id2, f2) = List.nth rankings 1 in
  let (id3, f3) = List.nth rankings 2 in
  assert (id1 = "a2" && f1 = 0.9);
  assert (id2 = "a3" && f2 = 0.6);
  assert (id3 = "a1" && f3 = 0.3);
  Printf.printf "✓ test_fitness_rankings\n%!"

(* ============================================ *)
(* Pure.select_elite_agents tests               *)
(* ============================================ *)

let test_select_elite () =
  let swarm = { (make_test_swarm ()) with
    agents = [
      make_test_agent ~id:"a1" ~name:"A1" ~fitness:0.3 ~generation:0;
      make_test_agent ~id:"a2" ~name:"A2" ~fitness:0.9 ~generation:0;
      make_test_agent ~id:"a3" ~name:"A3" ~fitness:0.6 ~generation:0;
      make_test_agent ~id:"a4" ~name:"A4" ~fitness:0.8 ~generation:0;
    ]
  } in
  let elite = Swarm.Pure.select_elite_agents swarm in
  (* With selection_pressure 0.5, top 50% = 2 agents *)
  assert (List.length elite = 2);
  let ids = List.map (fun (a : Swarm.swarm_agent) -> a.id) elite in
  assert (List.mem "a2" ids);  (* fitness 0.9 *)
  assert (List.mem "a4" ids);  (* fitness 0.8 *)
  Printf.printf "✓ test_select_elite\n%!"

(* ============================================ *)
(* Pure.pheromone tests (Stigmergy)             *)
(* ============================================ *)

let test_deposit_pheromone () =
  let swarm = make_test_swarm ~behavior:Swarm.Stigmergy () in
  let now = Unix.gettimeofday () in
  let swarm = Swarm.Pure.deposit_pheromone swarm
    ~path_id:"path-a" ~agent_id:"claude" ~strength:0.8 ~now in
  assert (List.length swarm.pheromones = 1);
  let p = List.hd swarm.pheromones in
  assert (p.path_id = "path-a");
  assert (p.strength = 0.8);
  assert (p.deposited_by = "claude");
  Printf.printf "✓ test_deposit_pheromone\n%!"

let test_deposit_pheromone_accumulate () =
  let swarm = make_test_swarm ~behavior:Swarm.Stigmergy () in
  let now = Unix.gettimeofday () in
  let swarm = Swarm.Pure.deposit_pheromone swarm
    ~path_id:"path-a" ~agent_id:"claude" ~strength:0.5 ~now in
  let swarm = Swarm.Pure.deposit_pheromone swarm
    ~path_id:"path-a" ~agent_id:"gemini" ~strength:0.3 ~now in
  assert (List.length swarm.pheromones = 1);
  let p = List.hd swarm.pheromones in
  (* Strength should accumulate with cap at 1.0 *)
  assert (p.strength >= 0.8 && p.strength <= 1.0);
  Printf.printf "✓ test_deposit_pheromone_accumulate\n%!"

let test_evaporate_pheromones () =
  let swarm = make_test_swarm ~behavior:Swarm.Stigmergy () in
  let past = Unix.gettimeofday () -. 100.0 in  (* 100 seconds ago *)
  let swarm = { swarm with
    pheromones = [{
      path_id = "path-a";
      strength = 0.8;
      deposited_by = "claude";
      deposited_at = past;
      evaporation_rate = 0.05;
    }]
  } in
  let now = Unix.gettimeofday () in
  let evaporated = Swarm.Pure.evaporate_pheromones swarm ~now in
  (* After 100 seconds with rate 0.05, should decay significantly *)
  assert (List.length evaporated.pheromones = 1);
  let p = List.hd evaporated.pheromones in
  assert (p.strength < 0.8);  (* Should have decayed *)
  Printf.printf "✓ test_evaporate_pheromones\n%!"

let test_strongest_trails () =
  let swarm = { (make_test_swarm ()) with
    pheromones = [
      { path_id = "p1"; strength = 0.3; deposited_by = "a"; deposited_at = 0.0; evaporation_rate = 0.05 };
      { path_id = "p2"; strength = 0.9; deposited_by = "b"; deposited_at = 0.0; evaporation_rate = 0.05 };
      { path_id = "p3"; strength = 0.6; deposited_by = "c"; deposited_at = 0.0; evaporation_rate = 0.05 };
    ]
  } in
  let top2 = Swarm.Pure.strongest_trails swarm ~limit:2 in
  assert (List.length top2 = 2);
  assert ((List.nth top2 0).path_id = "p2");  (* 0.9 *)
  assert ((List.nth top2 1).path_id = "p3");  (* 0.6 *)
  Printf.printf "✓ test_strongest_trails\n%!"

(* ============================================ *)
(* Pure.quorum tests (Quorum Sensing)           *)
(* ============================================ *)

let test_add_proposal () =
  let swarm = make_test_swarm ~behavior:Swarm.Quorum_sensing () in
  let now = Unix.gettimeofday () in
  let proposal : Swarm.quorum_proposal = {
    proposal_id = "prop-1";
    description = "Switch to foraging mode";
    proposed_by = "claude";
    proposed_at = now;
    votes_for = ["claude"];
    votes_against = [];
    threshold = 0.6;
    deadline = None;
    status = `Pending;
  } in
  let swarm = Swarm.Pure.add_proposal swarm ~proposal in
  assert (List.length swarm.proposals = 1);
  let p = List.hd swarm.proposals in
  assert (p.proposal_id = "prop-1");
  Printf.printf "✓ test_add_proposal\n%!"

let test_vote_for () =
  let swarm = make_test_swarm ~behavior:Swarm.Quorum_sensing () in
  let now = Unix.gettimeofday () in
  let proposal : Swarm.quorum_proposal = {
    proposal_id = "prop-1";
    description = "Test proposal";
    proposed_by = "claude";
    proposed_at = now;
    votes_for = [];
    votes_against = [];
    threshold = 0.6;
    deadline = None;
    status = `Pending;
  } in
  let swarm = Swarm.Pure.add_proposal swarm ~proposal in
  let swarm = Swarm.Pure.record_vote swarm ~proposal_id:"prop-1" ~agent_id:"gemini" ~vote_for:true in
  let p = List.find (fun (p : Swarm.quorum_proposal) -> p.proposal_id = "prop-1") swarm.proposals in
  assert (List.mem "gemini" p.votes_for);
  assert (not (List.mem "gemini" p.votes_against));
  Printf.printf "✓ test_vote_for\n%!"

let test_vote_against () =
  let swarm = make_test_swarm ~behavior:Swarm.Quorum_sensing () in
  let now = Unix.gettimeofday () in
  let proposal : Swarm.quorum_proposal = {
    proposal_id = "prop-1";
    description = "Test proposal";
    proposed_by = "claude";
    proposed_at = now;
    votes_for = [];
    votes_against = [];
    threshold = 0.6;
    deadline = None;
    status = `Pending;
  } in
  let swarm = Swarm.Pure.add_proposal swarm ~proposal in
  let swarm = Swarm.Pure.record_vote swarm ~proposal_id:"prop-1" ~agent_id:"codex" ~vote_for:false in
  let p = List.find (fun (p : Swarm.quorum_proposal) -> p.proposal_id = "prop-1") swarm.proposals in
  assert (List.mem "codex" p.votes_against);
  assert (not (List.mem "codex" p.votes_for));
  Printf.printf "✓ test_vote_against\n%!"

(* ============================================ *)
(* Pure.evolve tests                            *)
(* ============================================ *)

let test_evolve_increments_generation () =
  let swarm = { (make_test_swarm ()) with
    agents = [
      make_test_agent ~id:"a1" ~name:"A1" ~fitness:0.8 ~generation:0;
      make_test_agent ~id:"a2" ~name:"A2" ~fitness:0.6 ~generation:0;
    ]
  } in
  let now = Unix.gettimeofday () in
  let evolved = Swarm.Pure.evolve_agents swarm ~now in
  assert (evolved.generation = 1);
  Printf.printf "✓ test_evolve_increments_generation\n%!"

(* ============================================ *)
(* Behavior serialization tests                 *)
(* ============================================ *)

let test_behavior_serialization () =
  let behaviors = [Swarm.Flocking; Swarm.Foraging; Swarm.Stigmergy; Swarm.Quorum_sensing] in
  List.iter (fun b ->
    let s = Swarm.behavior_to_string b in
    let b' = Swarm.behavior_of_string s in
    assert (b = b')
  ) behaviors;
  Printf.printf "✓ test_behavior_serialization\n%!"

(* ============================================ *)
(* Run all tests                                *)
(* ============================================ *)

let () =
  Printf.printf "\n=== Swarm (Level 4) Pure Function Tests ===\n\n";

  (* Join/Leave tests *)
  test_join_agent_empty_swarm ();
  test_join_agent_already_member ();
  test_join_agent_swarm_full ();
  test_leave_agent ();
  test_leave_agent_not_member ();

  (* Fitness tests *)
  test_update_fitness ();
  test_update_fitness_not_found ();
  test_fitness_rankings ();
  test_select_elite ();

  (* Stigmergy tests *)
  test_deposit_pheromone ();
  test_deposit_pheromone_accumulate ();
  test_evaporate_pheromones ();
  test_strongest_trails ();

  (* Quorum tests *)
  test_add_proposal ();
  test_vote_for ();
  test_vote_against ();

  (* Evolution tests *)
  test_evolve_increments_generation ();

  (* Serialization *)
  test_behavior_serialization ();

  Printf.printf "\n✅ All Swarm tests passed!\n\n"
