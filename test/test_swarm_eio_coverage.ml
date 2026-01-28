(** Swarm Eio Module Coverage Tests

    Tests for swarm types and configuration:
    - swarm_behavior type
    - swarm_agent type
    - pheromone type
    - quorum_proposal type
    - swarm_config type
    - swarm type
    - default_config function
*)

open Alcotest

module Swarm_eio = Masc_mcp.Swarm_eio

(* ============================================================
   swarm_behavior Type Tests
   ============================================================ *)

let test_swarm_behavior_flocking () =
  let b = Swarm_eio.Flocking in
  match b with
  | Swarm_eio.Flocking -> check bool "flocking" true true
  | _ -> fail "expected Flocking"

let test_swarm_behavior_foraging () =
  let b = Swarm_eio.Foraging in
  match b with
  | Swarm_eio.Foraging -> check bool "foraging" true true
  | _ -> fail "expected Foraging"

let test_swarm_behavior_stigmergy () =
  let b = Swarm_eio.Stigmergy in
  match b with
  | Swarm_eio.Stigmergy -> check bool "stigmergy" true true
  | _ -> fail "expected Stigmergy"

let test_swarm_behavior_quorum () =
  let b = Swarm_eio.Quorum_sensing in
  match b with
  | Swarm_eio.Quorum_sensing -> check bool "quorum_sensing" true true
  | _ -> fail "expected Quorum_sensing"

(* ============================================================
   swarm_agent Type Tests
   ============================================================ *)

let test_swarm_agent_type () =
  let a : Swarm_eio.swarm_agent = {
    id = "agent-001";
    name = "claude";
    fitness = 0.85;
    generation = 5;
    mutations = ["mutation_a"; "mutation_b"];
    joined_at = 1704067200.0;
    last_active = 1704070800.0;
  } in
  check string "id" "agent-001" a.id;
  check string "name" "claude" a.name;
  check (float 0.01) "fitness" 0.85 a.fitness;
  check int "generation" 5 a.generation;
  check int "mutations count" 2 (List.length a.mutations)

(* ============================================================
   pheromone Type Tests
   ============================================================ *)

let test_pheromone_type () =
  let p : Swarm_eio.pheromone = {
    path_id = "path-001";
    strength = 0.9;
    deposited_by = "agent-001";
    deposited_at = 1704067200.0;
    evaporation_rate = 0.1;
  } in
  check string "path_id" "path-001" p.path_id;
  check (float 0.01) "strength" 0.9 p.strength;
  check (float 0.01) "evaporation_rate" 0.1 p.evaporation_rate

(* ============================================================
   quorum_proposal Type Tests
   ============================================================ *)

let test_quorum_proposal_pending () =
  let q : Swarm_eio.quorum_proposal = {
    proposal_id = "proposal-001";
    description = "Adopt new strategy";
    proposed_by = "agent-001";
    proposed_at = 1704067200.0;
    votes_for = ["agent-002"; "agent-003"];
    votes_against = ["agent-004"];
    threshold = 0.6;
    deadline = Some 1704153600.0;
    status = `Pending;
  } in
  check string "proposal_id" "proposal-001" q.proposal_id;
  check int "votes_for" 2 (List.length q.votes_for);
  check int "votes_against" 1 (List.length q.votes_against);
  match q.status with
  | `Pending -> check bool "pending" true true
  | _ -> fail "expected Pending"

let test_quorum_proposal_passed () =
  let q : Swarm_eio.quorum_proposal = {
    proposal_id = "proposal-002";
    description = "";
    proposed_by = "";
    proposed_at = 0.0;
    votes_for = [];
    votes_against = [];
    threshold = 0.5;
    deadline = None;
    status = `Passed;
  } in
  match q.status with
  | `Passed -> check bool "passed" true true
  | _ -> fail "expected Passed"

(* ============================================================
   swarm_config Type Tests
   ============================================================ *)

let test_swarm_config_type () =
  let c : Swarm_eio.swarm_config = {
    id = "swarm-001";
    name = "test-swarm";
    selection_pressure = 0.3;
    mutation_rate = 0.1;
    evaporation_rate = 0.05;
    quorum_threshold = 0.6;
    max_agents = 10;
    behavior = Swarm_eio.Flocking;
  } in
  check string "id" "swarm-001" c.id;
  check string "name" "test-swarm" c.name;
  check int "max_agents" 10 c.max_agents;
  check (float 0.01) "selection_pressure" 0.3 c.selection_pressure

(* ============================================================
   swarm Type Tests
   ============================================================ *)

let test_swarm_type_empty () =
  let cfg : Swarm_eio.swarm_config = {
    id = "swarm-002";
    name = "empty-swarm";
    selection_pressure = 0.3;
    mutation_rate = 0.1;
    evaporation_rate = 0.05;
    quorum_threshold = 0.6;
    max_agents = 5;
    behavior = Swarm_eio.Stigmergy;
  } in
  let s : Swarm_eio.swarm = {
    swarm_cfg = cfg;
    agents = [];
    pheromones = [];
    proposals = [];
    generation = 0;
    created_at = 1704067200.0;
    last_evolution = 1704067200.0;
  } in
  check int "agents empty" 0 (List.length s.agents);
  check int "generation" 0 s.generation

(* ============================================================
   default_config Tests
   ============================================================ *)

let test_default_config () =
  let cfg = Swarm_eio.default_config () in
  check bool "has name" true (String.length cfg.name > 0);
  check bool "has id" true (String.length cfg.id > 0);
  check bool "selection_pressure > 0" true (cfg.selection_pressure > 0.0);
  check bool "max_agents > 0" true (cfg.max_agents > 0)

let test_default_config_custom_name () =
  let cfg = Swarm_eio.default_config ~name:"my-swarm" () in
  check string "custom name" "my-swarm" cfg.name

let test_default_config_custom_id () =
  let cfg = Swarm_eio.default_config ~id:"custom-id" () in
  check string "custom id" "custom-id" cfg.id

(* ============================================================
   Serialization Tests (behavior_to_string, behavior_of_string)
   ============================================================ *)

let test_behavior_to_string_flocking () =
  check string "flocking" "flocking" (Swarm_eio.behavior_to_string Swarm_eio.Flocking)

let test_behavior_to_string_foraging () =
  check string "foraging" "foraging" (Swarm_eio.behavior_to_string Swarm_eio.Foraging)

let test_behavior_to_string_stigmergy () =
  check string "stigmergy" "stigmergy" (Swarm_eio.behavior_to_string Swarm_eio.Stigmergy)

let test_behavior_to_string_quorum () =
  check string "quorum_sensing" "quorum_sensing" (Swarm_eio.behavior_to_string Swarm_eio.Quorum_sensing)

let test_behavior_of_string_flocking () =
  match Swarm_eio.behavior_of_string "flocking" with
  | Swarm_eio.Flocking -> check bool "flocking" true true
  | _ -> fail "expected Flocking"

let test_behavior_of_string_foraging () =
  match Swarm_eio.behavior_of_string "foraging" with
  | Swarm_eio.Foraging -> check bool "foraging" true true
  | _ -> fail "expected Foraging"

let test_behavior_of_string_stigmergy () =
  match Swarm_eio.behavior_of_string "stigmergy" with
  | Swarm_eio.Stigmergy -> check bool "stigmergy" true true
  | _ -> fail "expected Stigmergy"

let test_behavior_of_string_quorum () =
  match Swarm_eio.behavior_of_string "quorum_sensing" with
  | Swarm_eio.Quorum_sensing -> check bool "quorum_sensing" true true
  | _ -> fail "expected Quorum_sensing"

let test_behavior_of_string_unknown () =
  match Swarm_eio.behavior_of_string "unknown" with
  | Swarm_eio.Flocking -> check bool "default flocking" true true
  | _ -> fail "expected default Flocking"

let test_status_to_string () =
  check string "pending" "pending" (Swarm_eio.status_to_string `Pending);
  check string "passed" "passed" (Swarm_eio.status_to_string `Passed);
  check string "rejected" "rejected" (Swarm_eio.status_to_string `Rejected);
  check string "expired" "expired" (Swarm_eio.status_to_string `Expired)

let test_status_of_string () =
  check bool "pending" true (Swarm_eio.status_of_string "pending" = `Pending);
  check bool "passed" true (Swarm_eio.status_of_string "passed" = `Passed);
  check bool "rejected" true (Swarm_eio.status_of_string "rejected" = `Rejected);
  check bool "expired" true (Swarm_eio.status_of_string "expired" = `Expired);
  check bool "unknown" true (Swarm_eio.status_of_string "unknown" = `Pending)

(* ============================================================
   JSON Roundtrip Tests
   ============================================================ *)

let test_agent_json_roundtrip () =
  let a : Swarm_eio.swarm_agent = {
    id = "agent-rt-001";
    name = "test-agent";
    fitness = 0.75;
    generation = 3;
    mutations = ["mut1"; "mut2"];
    joined_at = 1704067200.0;
    last_active = 1704070800.0;
  } in
  let json = Swarm_eio.agent_to_json a in
  let decoded = Swarm_eio.agent_of_json json in
  check string "id" a.id decoded.id;
  check string "name" a.name decoded.name;
  check (float 0.001) "fitness" a.fitness decoded.fitness;
  check int "generation" a.generation decoded.generation;
  check int "mutations count" (List.length a.mutations) (List.length decoded.mutations)

let test_pheromone_json_roundtrip () =
  let p : Swarm_eio.pheromone = {
    path_id = "path-rt-001";
    strength = 0.65;
    deposited_by = "agent-001";
    deposited_at = 1704067200.0;
    evaporation_rate = 0.15;
  } in
  let json = Swarm_eio.pheromone_to_json p in
  let decoded = Swarm_eio.pheromone_of_json json in
  check string "path_id" p.path_id decoded.path_id;
  check (float 0.001) "strength" p.strength decoded.strength;
  check string "deposited_by" p.deposited_by decoded.deposited_by

let test_proposal_json_roundtrip () =
  let p : Swarm_eio.quorum_proposal = {
    proposal_id = "prop-rt-001";
    description = "Test proposal";
    proposed_by = "agent-001";
    proposed_at = 1704067200.0;
    votes_for = ["a1"; "a2"];
    votes_against = ["a3"];
    threshold = 0.6;
    deadline = Some 1704153600.0;
    status = `Pending;
  } in
  let json = Swarm_eio.proposal_to_json p in
  let decoded = Swarm_eio.proposal_of_json json in
  check string "proposal_id" p.proposal_id decoded.proposal_id;
  check string "description" p.description decoded.description;
  check int "votes_for" (List.length p.votes_for) (List.length decoded.votes_for);
  check int "votes_against" (List.length p.votes_against) (List.length decoded.votes_against)

let test_proposal_json_no_deadline () =
  let p : Swarm_eio.quorum_proposal = {
    proposal_id = "prop-no-dl";
    description = "";
    proposed_by = "";
    proposed_at = 0.0;
    votes_for = [];
    votes_against = [];
    threshold = 0.5;
    deadline = None;
    status = `Passed;
  } in
  let json = Swarm_eio.proposal_to_json p in
  let decoded = Swarm_eio.proposal_of_json json in
  check bool "no deadline" true (decoded.deadline = None);
  check bool "passed status" true (decoded.status = `Passed)

let test_config_json_roundtrip () =
  let c : Swarm_eio.swarm_config = {
    id = "cfg-rt-001";
    name = "test-config";
    selection_pressure = 0.35;
    mutation_rate = 0.12;
    evaporation_rate = 0.08;
    quorum_threshold = 0.65;
    max_agents = 25;
    behavior = Swarm_eio.Stigmergy;
  } in
  let json = Swarm_eio.config_to_json c in
  let decoded = Swarm_eio.config_of_json json in
  check string "id" c.id decoded.id;
  check string "name" c.name decoded.name;
  check (float 0.001) "selection_pressure" c.selection_pressure decoded.selection_pressure;
  check int "max_agents" c.max_agents decoded.max_agents;
  check string "behavior" "stigmergy" (Swarm_eio.behavior_to_string decoded.behavior)

let test_swarm_json_roundtrip () =
  let cfg : Swarm_eio.swarm_config = {
    id = "swarm-rt";
    name = "roundtrip-swarm";
    selection_pressure = 0.3;
    mutation_rate = 0.1;
    evaporation_rate = 0.1;
    quorum_threshold = 0.6;
    max_agents = 10;
    behavior = Swarm_eio.Flocking;
  } in
  let agent : Swarm_eio.swarm_agent = {
    id = "a1"; name = "agent1"; fitness = 0.8;
    generation = 1; mutations = [];
    joined_at = 1000.0; last_active = 1000.0;
  } in
  let s : Swarm_eio.swarm = {
    swarm_cfg = cfg;
    agents = [agent];
    pheromones = [];
    proposals = [];
    generation = 2;
    created_at = 500.0;
    last_evolution = 900.0;
  } in
  let json = Swarm_eio.swarm_to_json s in
  let decoded = Swarm_eio.swarm_of_json json in
  check string "config id" s.swarm_cfg.id decoded.swarm_cfg.id;
  check int "agents count" 1 (List.length decoded.agents);
  check int "generation" s.generation decoded.generation

(* ============================================================
   Pure Module Tests
   ============================================================ *)

let make_test_swarm () : Swarm_eio.swarm =
  let cfg : Swarm_eio.swarm_config = {
    id = "test-swarm";
    name = "pure-test";
    selection_pressure = 0.3;
    mutation_rate = 0.1;
    evaporation_rate = 0.1;
    quorum_threshold = 0.6;
    max_agents = 5;
    behavior = Swarm_eio.Flocking;
  } in
  {
    swarm_cfg = cfg;
    agents = [];
    pheromones = [];
    proposals = [];
    generation = 0;
    created_at = 1000.0;
    last_evolution = 1000.0;
  }

let test_pure_join_agent () =
  let swarm = make_test_swarm () in
  match Swarm_eio.Pure.join_agent swarm ~agent_id:"a1" ~agent_name:"claude" ~now:2000.0 with
  | Swarm_eio.Pure.Joined updated ->
      check int "one agent" 1 (List.length updated.agents)
  | _ -> fail "expected Joined"

let test_pure_join_agent_already_member () =
  let swarm = make_test_swarm () in
  match Swarm_eio.Pure.join_agent swarm ~agent_id:"a1" ~agent_name:"claude" ~now:2000.0 with
  | Swarm_eio.Pure.Joined s1 ->
      (match Swarm_eio.Pure.join_agent s1 ~agent_id:"a1" ~agent_name:"claude" ~now:2001.0 with
       | Swarm_eio.Pure.Already_member _ -> check bool "already member" true true
       | _ -> fail "expected Already_member")
  | _ -> fail "expected Joined first"

let test_pure_join_agent_swarm_full () =
  let cfg : Swarm_eio.swarm_config = {
    id = "full"; name = "full"; selection_pressure = 0.3;
    mutation_rate = 0.1; evaporation_rate = 0.1;
    quorum_threshold = 0.6; max_agents = 1;
    behavior = Swarm_eio.Flocking;
  } in
  let swarm : Swarm_eio.swarm = {
    swarm_cfg = cfg; agents = []; pheromones = [];
    proposals = []; generation = 0;
    created_at = 1000.0; last_evolution = 1000.0;
  } in
  match Swarm_eio.Pure.join_agent swarm ~agent_id:"a1" ~agent_name:"c1" ~now:2000.0 with
  | Swarm_eio.Pure.Joined s1 ->
      (match Swarm_eio.Pure.join_agent s1 ~agent_id:"a2" ~agent_name:"c2" ~now:2001.0 with
       | Swarm_eio.Pure.Swarm_full -> check bool "swarm full" true true
       | _ -> fail "expected Swarm_full")
  | _ -> fail "expected first join"

let test_pure_leave_agent () =
  let swarm = make_test_swarm () in
  match Swarm_eio.Pure.join_agent swarm ~agent_id:"a1" ~agent_name:"claude" ~now:2000.0 with
  | Swarm_eio.Pure.Joined s1 ->
      let s2 = Swarm_eio.Pure.leave_agent s1 ~agent_id:"a1" in
      check int "no agents" 0 (List.length s2.agents)
  | _ -> fail "expected Joined"

let test_pure_fitness_rankings () =
  let swarm = make_test_swarm () in
  match Swarm_eio.Pure.join_agent swarm ~agent_id:"a1" ~agent_name:"c1" ~now:2000.0 with
  | Swarm_eio.Pure.Joined s1 ->
      (match Swarm_eio.Pure.join_agent s1 ~agent_id:"a2" ~agent_name:"c2" ~now:2001.0 with
       | Swarm_eio.Pure.Joined s2 ->
           let rankings = Swarm_eio.Pure.fitness_rankings s2 in
           check int "two rankings" 2 (List.length rankings)
       | _ -> fail "expected second join")
  | _ -> fail "expected first join"

let test_pure_select_elite_agents () =
  let swarm = make_test_swarm () in
  match Swarm_eio.Pure.join_agent swarm ~agent_id:"a1" ~agent_name:"c1" ~now:2000.0 with
  | Swarm_eio.Pure.Joined s1 ->
      let elite = Swarm_eio.Pure.select_elite_agents s1 in
      check bool "at least one elite" true (List.length elite >= 1)
  | _ -> fail "expected Joined"

let test_pure_evolve_agents () =
  let swarm = make_test_swarm () in
  match Swarm_eio.Pure.join_agent swarm ~agent_id:"a1" ~agent_name:"c1" ~now:2000.0 with
  | Swarm_eio.Pure.Joined s1 ->
      let evolved = Swarm_eio.Pure.evolve_agents s1 ~now:3000.0 in
      check int "generation +1" 1 evolved.generation
  | _ -> fail "expected Joined"

let test_pure_deposit_pheromone () =
  let swarm = make_test_swarm () in
  let updated = Swarm_eio.Pure.deposit_pheromone swarm
    ~path_id:"path1" ~agent_id:"a1" ~strength:0.5 ~now:2000.0 in
  check int "one pheromone" 1 (List.length updated.pheromones)

let test_pure_deposit_pheromone_update () =
  let swarm = make_test_swarm () in
  let s1 = Swarm_eio.Pure.deposit_pheromone swarm
    ~path_id:"path1" ~agent_id:"a1" ~strength:0.5 ~now:2000.0 in
  let s2 = Swarm_eio.Pure.deposit_pheromone s1
    ~path_id:"path1" ~agent_id:"a2" ~strength:0.3 ~now:2001.0 in
  check int "still one pheromone" 1 (List.length s2.pheromones);
  let p = List.hd s2.pheromones in
  check bool "strength increased" true (p.strength > 0.5)

let test_pure_evaporate_pheromones () =
  let swarm = make_test_swarm () in
  let s1 = Swarm_eio.Pure.deposit_pheromone swarm
    ~path_id:"path1" ~agent_id:"a1" ~strength:0.1 ~now:1000.0 in
  (* After many hours, pheromone should evaporate *)
  let s2 = Swarm_eio.Pure.evaporate_pheromones s1 ~now:100000.0 in
  check int "pheromone evaporated" 0 (List.length s2.pheromones)

let test_pure_strongest_trails () =
  let swarm = make_test_swarm () in
  let s1 = Swarm_eio.Pure.deposit_pheromone swarm
    ~path_id:"path1" ~agent_id:"a1" ~strength:0.9 ~now:2000.0 in
  let s2 = Swarm_eio.Pure.deposit_pheromone s1
    ~path_id:"path2" ~agent_id:"a1" ~strength:0.3 ~now:2001.0 in
  let trails = Swarm_eio.Pure.strongest_trails s2 ~limit:1 in
  check int "one trail" 1 (List.length trails);
  check string "strongest path" "path1" (List.hd trails).path_id

let test_pure_add_proposal () =
  let swarm = make_test_swarm () in
  let proposal : Swarm_eio.quorum_proposal = {
    proposal_id = "p1"; description = "Test";
    proposed_by = "a1"; proposed_at = 2000.0;
    votes_for = []; votes_against = [];
    threshold = 0.6; deadline = None;
    status = `Pending;
  } in
  let updated = Swarm_eio.Pure.add_proposal swarm ~proposal in
  check int "one proposal" 1 (List.length updated.proposals)

let test_pure_record_vote_for () =
  let swarm = make_test_swarm () in
  let proposal : Swarm_eio.quorum_proposal = {
    proposal_id = "p1"; description = "Test";
    proposed_by = "a1"; proposed_at = 2000.0;
    votes_for = []; votes_against = [];
    threshold = 0.6; deadline = None;
    status = `Pending;
  } in
  let s1 = Swarm_eio.Pure.add_proposal swarm ~proposal in
  let s2 = Swarm_eio.Pure.record_vote s1 ~proposal_id:"p1" ~agent_id:"a2" ~vote_for:true in
  let p = List.hd s2.proposals in
  check int "one vote for" 1 (List.length p.votes_for)

let test_pure_record_vote_against () =
  let swarm = make_test_swarm () in
  let proposal : Swarm_eio.quorum_proposal = {
    proposal_id = "p1"; description = "Test";
    proposed_by = "a1"; proposed_at = 2000.0;
    votes_for = []; votes_against = [];
    threshold = 0.6; deadline = None;
    status = `Pending;
  } in
  let s1 = Swarm_eio.Pure.add_proposal swarm ~proposal in
  let s2 = Swarm_eio.Pure.record_vote s1 ~proposal_id:"p1" ~agent_id:"a2" ~vote_for:false in
  let p = List.hd s2.proposals in
  check int "one vote against" 1 (List.length p.votes_against)

let test_pure_update_proposal_status_passed () =
  let swarm = make_test_swarm () in
  (* Add two agents first *)
  match Swarm_eio.Pure.join_agent swarm ~agent_id:"a1" ~agent_name:"c1" ~now:2000.0 with
  | Swarm_eio.Pure.Joined s1 ->
      (match Swarm_eio.Pure.join_agent s1 ~agent_id:"a2" ~agent_name:"c2" ~now:2001.0 with
       | Swarm_eio.Pure.Joined s2 ->
           let proposal : Swarm_eio.quorum_proposal = {
             proposal_id = "p1"; description = "Test";
             proposed_by = "a1"; proposed_at = 2000.0;
             votes_for = ["a1"; "a2"]; votes_against = [];
             threshold = 0.6; deadline = None;
             status = `Pending;
           } in
           let s3 = Swarm_eio.Pure.add_proposal s2 ~proposal in
           let s4 = Swarm_eio.Pure.update_proposal_status s3 ~proposal_id:"p1" ~now:3000.0 in
           let p = List.hd s4.proposals in
           check bool "passed" true (p.status = `Passed)
       | _ -> fail "expected second join")
  | _ -> fail "expected first join"

let test_pure_update_proposal_status_expired () =
  let swarm = make_test_swarm () in
  match Swarm_eio.Pure.join_agent swarm ~agent_id:"a1" ~agent_name:"c1" ~now:2000.0 with
  | Swarm_eio.Pure.Joined s1 ->
      let proposal : Swarm_eio.quorum_proposal = {
        proposal_id = "p1"; description = "Test";
        proposed_by = "a1"; proposed_at = 2000.0;
        votes_for = []; votes_against = [];
        threshold = 0.6; deadline = Some 2500.0;
        status = `Pending;
      } in
      let s2 = Swarm_eio.Pure.add_proposal s1 ~proposal in
      let s3 = Swarm_eio.Pure.update_proposal_status s2 ~proposal_id:"p1" ~now:3000.0 in
      let p = List.hd s3.proposals in
      check bool "expired" true (p.status = `Expired)
  | _ -> fail "expected join"

(* ============================================================
   Eio IO Tests
   ============================================================ *)

module Room = Masc_mcp.Room

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter (fun name -> rm_rf (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Unix.unlink path

let make_test_dir () =
  let unique_id = Printf.sprintf "masc_swarm_test_%d_%d"
    (Unix.getpid ())
    (int_of_float (Unix.gettimeofday () *. 1000000.)) in
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ()) unique_id in
  (try Unix.mkdir tmp_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  tmp_dir

let with_eio_env f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = make_test_dir () in
  let config = Room.default_config tmp_dir in
  let _ = Room.init config ~agent_name:None in
  Fun.protect
    ~finally:(fun () ->
      try
        let _ = Room.reset config in
        rm_rf tmp_dir
      with _ -> ())
    (fun () -> f ~fs config)

let test_eio_create () =
  with_eio_env @@ fun ~fs config ->
  let swarm = Swarm_eio.create ~fs config () in
  check bool "has id" true (String.length swarm.swarm_cfg.id > 0);
  check int "no agents" 0 (List.length swarm.agents)

let test_eio_create_custom_config () =
  with_eio_env @@ fun ~fs config ->
  let swarm_cfg = Swarm_eio.default_config ~name:"custom-swarm" () in
  let swarm = Swarm_eio.create ~fs config ~swarm_config:swarm_cfg () in
  check string "custom name" "custom-swarm" swarm.swarm_cfg.name

let test_eio_join () =
  with_eio_env @@ fun ~fs config ->
  let _ = Swarm_eio.create ~fs config () in
  match Swarm_eio.join ~fs config ~agent_id:"a1" ~agent_name:"claude" with
  | Some swarm -> check int "one agent" 1 (List.length swarm.agents)
  | None -> fail "expected join success"

let test_eio_join_no_swarm () =
  with_eio_env @@ fun ~fs config ->
  (* Don't create swarm first *)
  match Swarm_eio.join ~fs config ~agent_id:"a1" ~agent_name:"claude" with
  | None -> check bool "no swarm" true true
  | Some _ -> fail "expected None"

let test_eio_leave () =
  with_eio_env @@ fun ~fs config ->
  let _ = Swarm_eio.create ~fs config () in
  let _ = Swarm_eio.join ~fs config ~agent_id:"a1" ~agent_name:"claude" in
  match Swarm_eio.leave ~fs config ~agent_id:"a1" with
  | Some swarm -> check int "no agents" 0 (List.length swarm.agents)
  | None -> fail "expected leave success"

let test_eio_dissolve () =
  with_eio_env @@ fun ~fs config ->
  let _ = Swarm_eio.create ~fs config () in
  Swarm_eio.dissolve ~fs config;
  (* join should fail after dissolve *)
  match Swarm_eio.join ~fs config ~agent_id:"a1" ~agent_name:"claude" with
  | None -> check bool "dissolved" true true
  | Some _ -> fail "expected dissolved"

let test_eio_get_fitness_rankings () =
  with_eio_env @@ fun ~fs config ->
  let _ = Swarm_eio.create ~fs config () in
  let _ = Swarm_eio.join ~fs config ~agent_id:"a1" ~agent_name:"c1" in
  let _ = Swarm_eio.join ~fs config ~agent_id:"a2" ~agent_name:"c2" in
  let rankings = Swarm_eio.get_fitness_rankings ~fs config in
  check int "two rankings" 2 (List.length rankings)

let test_eio_select_elite () =
  with_eio_env @@ fun ~fs config ->
  let _ = Swarm_eio.create ~fs config () in
  let _ = Swarm_eio.join ~fs config ~agent_id:"a1" ~agent_name:"c1" in
  let elite = Swarm_eio.select_elite ~fs config in
  check bool "at least one elite" true (List.length elite >= 1)

let test_eio_evolve () =
  with_eio_env @@ fun ~fs config ->
  let _ = Swarm_eio.create ~fs config () in
  let _ = Swarm_eio.join ~fs config ~agent_id:"a1" ~agent_name:"claude" in
  match Swarm_eio.evolve ~fs config with
  | Some swarm -> check int "generation 1" 1 swarm.generation
  | None -> fail "expected evolve success"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Swarm Eio Coverage" [
    "swarm_behavior", [
      test_case "flocking" `Quick test_swarm_behavior_flocking;
      test_case "foraging" `Quick test_swarm_behavior_foraging;
      test_case "stigmergy" `Quick test_swarm_behavior_stigmergy;
      test_case "quorum_sensing" `Quick test_swarm_behavior_quorum;
    ];
    "swarm_agent", [
      test_case "type" `Quick test_swarm_agent_type;
    ];
    "pheromone", [
      test_case "type" `Quick test_pheromone_type;
    ];
    "quorum_proposal", [
      test_case "pending" `Quick test_quorum_proposal_pending;
      test_case "passed" `Quick test_quorum_proposal_passed;
    ];
    "swarm_config", [
      test_case "type" `Quick test_swarm_config_type;
    ];
    "swarm", [
      test_case "empty" `Quick test_swarm_type_empty;
    ];
    "default_config", [
      test_case "basic" `Quick test_default_config;
      test_case "custom name" `Quick test_default_config_custom_name;
      test_case "custom id" `Quick test_default_config_custom_id;
    ];
    "serialization", [
      test_case "behavior_to_string flocking" `Quick test_behavior_to_string_flocking;
      test_case "behavior_to_string foraging" `Quick test_behavior_to_string_foraging;
      test_case "behavior_to_string stigmergy" `Quick test_behavior_to_string_stigmergy;
      test_case "behavior_to_string quorum" `Quick test_behavior_to_string_quorum;
      test_case "behavior_of_string flocking" `Quick test_behavior_of_string_flocking;
      test_case "behavior_of_string foraging" `Quick test_behavior_of_string_foraging;
      test_case "behavior_of_string stigmergy" `Quick test_behavior_of_string_stigmergy;
      test_case "behavior_of_string quorum" `Quick test_behavior_of_string_quorum;
      test_case "behavior_of_string unknown" `Quick test_behavior_of_string_unknown;
      test_case "status_to_string" `Quick test_status_to_string;
      test_case "status_of_string" `Quick test_status_of_string;
    ];
    "json_roundtrip", [
      test_case "agent" `Quick test_agent_json_roundtrip;
      test_case "pheromone" `Quick test_pheromone_json_roundtrip;
      test_case "proposal" `Quick test_proposal_json_roundtrip;
      test_case "proposal no deadline" `Quick test_proposal_json_no_deadline;
      test_case "config" `Quick test_config_json_roundtrip;
      test_case "swarm" `Quick test_swarm_json_roundtrip;
    ];
    "pure_join_leave", [
      test_case "join agent" `Quick test_pure_join_agent;
      test_case "already member" `Quick test_pure_join_agent_already_member;
      test_case "swarm full" `Quick test_pure_join_agent_swarm_full;
      test_case "leave agent" `Quick test_pure_leave_agent;
    ];
    "pure_fitness", [
      test_case "rankings" `Quick test_pure_fitness_rankings;
      test_case "select elite" `Quick test_pure_select_elite_agents;
      test_case "evolve" `Quick test_pure_evolve_agents;
    ];
    "pure_pheromone", [
      test_case "deposit" `Quick test_pure_deposit_pheromone;
      test_case "deposit update" `Quick test_pure_deposit_pheromone_update;
      test_case "evaporate" `Quick test_pure_evaporate_pheromones;
      test_case "strongest trails" `Quick test_pure_strongest_trails;
    ];
    "pure_quorum", [
      test_case "add proposal" `Quick test_pure_add_proposal;
      test_case "vote for" `Quick test_pure_record_vote_for;
      test_case "vote against" `Quick test_pure_record_vote_against;
      test_case "status passed" `Quick test_pure_update_proposal_status_passed;
      test_case "status expired" `Quick test_pure_update_proposal_status_expired;
    ];
    "eio_lifecycle", [
      test_case "create" `Quick test_eio_create;
      test_case "create custom config" `Quick test_eio_create_custom_config;
      test_case "join" `Quick test_eio_join;
      test_case "join no swarm" `Quick test_eio_join_no_swarm;
      test_case "leave" `Quick test_eio_leave;
      test_case "dissolve" `Quick test_eio_dissolve;
      test_case "fitness rankings" `Quick test_eio_get_fitness_rankings;
      test_case "select elite" `Quick test_eio_select_elite;
      test_case "evolve" `Quick test_eio_evolve;
    ];
  ]
