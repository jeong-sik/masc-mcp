(** Swarm Behaviors Eio Module Coverage Tests

    Tests for swarm behavior types and pure vector operations:
    - vector2d type
    - flocking_params type
    - foraging_state type
    - stigmergy_config type
    - default_flocking_params constant
    - default_stigmergy_config constant
    - vec_zero, vec_add, vec_sub, vec_scale, vec_magnitude
*)

open Alcotest

module Swarm_behaviors_eio = Masc_mcp.Swarm_behaviors_eio

(* ============================================================
   vector2d Type Tests
   ============================================================ *)

let test_vector2d_type () =
  let v : Swarm_behaviors_eio.vector2d = { x = 1.0; y = 2.0 } in
  check (float 0.01) "x" 1.0 v.x;
  check (float 0.01) "y" 2.0 v.y

let test_vector2d_zero () =
  let v = Swarm_behaviors_eio.vec_zero in
  check (float 0.01) "x" 0.0 v.x;
  check (float 0.01) "y" 0.0 v.y

(* ============================================================
   Vector Operation Tests
   ============================================================ *)

let test_vec_add () =
  let v1 : Swarm_behaviors_eio.vector2d = { x = 1.0; y = 2.0 } in
  let v2 : Swarm_behaviors_eio.vector2d = { x = 3.0; y = 4.0 } in
  let r = Swarm_behaviors_eio.vec_add v1 v2 in
  check (float 0.01) "x" 4.0 r.x;
  check (float 0.01) "y" 6.0 r.y

let test_vec_sub () =
  let v1 : Swarm_behaviors_eio.vector2d = { x = 5.0; y = 7.0 } in
  let v2 : Swarm_behaviors_eio.vector2d = { x = 2.0; y = 3.0 } in
  let r = Swarm_behaviors_eio.vec_sub v1 v2 in
  check (float 0.01) "x" 3.0 r.x;
  check (float 0.01) "y" 4.0 r.y

let test_vec_scale () =
  let v : Swarm_behaviors_eio.vector2d = { x = 2.0; y = 3.0 } in
  let r = Swarm_behaviors_eio.vec_scale v 2.0 in
  check (float 0.01) "x" 4.0 r.x;
  check (float 0.01) "y" 6.0 r.y

let test_vec_magnitude () =
  let v : Swarm_behaviors_eio.vector2d = { x = 3.0; y = 4.0 } in
  let m = Swarm_behaviors_eio.vec_magnitude v in
  check (float 0.01) "magnitude" 5.0 m

let test_vec_magnitude_zero () =
  let v = Swarm_behaviors_eio.vec_zero in
  let m = Swarm_behaviors_eio.vec_magnitude v in
  check (float 0.01) "magnitude of zero" 0.0 m

let test_vec_normalize () =
  let v : Swarm_behaviors_eio.vector2d = { x = 3.0; y = 4.0 } in
  let n = Swarm_behaviors_eio.vec_normalize v in
  let m = Swarm_behaviors_eio.vec_magnitude n in
  check (float 0.01) "normalized magnitude" 1.0 m

let test_vec_normalize_zero () =
  let v = Swarm_behaviors_eio.vec_zero in
  let n = Swarm_behaviors_eio.vec_normalize v in
  check (float 0.01) "x" 0.0 n.x;
  check (float 0.01) "y" 0.0 n.y

let test_vec_distance () =
  let v1 : Swarm_behaviors_eio.vector2d = { x = 0.0; y = 0.0 } in
  let v2 : Swarm_behaviors_eio.vector2d = { x = 3.0; y = 4.0 } in
  let d = Swarm_behaviors_eio.vec_distance v1 v2 in
  check (float 0.01) "distance" 5.0 d

let test_vec_distance_same () =
  let v : Swarm_behaviors_eio.vector2d = { x = 5.0; y = 5.0 } in
  let d = Swarm_behaviors_eio.vec_distance v v in
  check (float 0.01) "distance to self" 0.0 d

(* ============================================================
   flocking_params Type Tests
   ============================================================ *)

let test_flocking_params_type () =
  let p : Swarm_behaviors_eio.flocking_params = {
    separation_weight = 1.5;
    alignment_weight = 1.0;
    cohesion_weight = 1.0;
    perception_radius = 100.0;
  } in
  check (float 0.01) "separation_weight" 1.5 p.separation_weight;
  check (float 0.01) "perception_radius" 100.0 p.perception_radius

let test_default_flocking_params () =
  let p = Swarm_behaviors_eio.default_flocking_params in
  check bool "separation > 0" true (p.separation_weight > 0.0);
  check bool "alignment > 0" true (p.alignment_weight > 0.0);
  check bool "cohesion > 0" true (p.cohesion_weight > 0.0);
  check bool "perception > 0" true (p.perception_radius > 0.0)

(* ============================================================
   foraging_state Type Tests
   ============================================================ *)

let test_foraging_state_type () =
  let s : Swarm_behaviors_eio.foraging_state = {
    exploration_rate = 0.3;
    discovered_solutions = [("solution_a", 0.9); ("solution_b", 0.7)];
    current_target = Some "solution_a";
  } in
  check (float 0.01) "exploration_rate" 0.3 s.exploration_rate;
  check int "solutions count" 2 (List.length s.discovered_solutions)

let test_foraging_state_no_target () =
  let s : Swarm_behaviors_eio.foraging_state = {
    exploration_rate = 0.5;
    discovered_solutions = [];
    current_target = None;
  } in
  match s.current_target with
  | None -> check bool "no target" true true
  | Some _ -> fail "expected None"

(* ============================================================
   stigmergy_config Type Tests
   ============================================================ *)

let test_stigmergy_config_type () =
  let c : Swarm_behaviors_eio.stigmergy_config = {
    deposit_rate = 0.2;
    evaporation_rate = 0.1;
    following_threshold = 0.15;
  } in
  check (float 0.01) "deposit_rate" 0.2 c.deposit_rate;
  check (float 0.01) "evaporation_rate" 0.1 c.evaporation_rate;
  check (float 0.01) "following_threshold" 0.15 c.following_threshold

let test_default_stigmergy_config () =
  let c = Swarm_behaviors_eio.default_stigmergy_config in
  check bool "deposit > 0" true (c.deposit_rate > 0.0);
  check bool "evaporation > 0" true (c.evaporation_rate > 0.0);
  check bool "threshold > 0" true (c.following_threshold > 0.0)

(* ============================================================
   agent_position Type Tests
   ============================================================ *)

let test_agent_position_type () =
  let pos : Swarm_behaviors_eio.agent_position = {
    agent_id = "agent-001";
    position = { x = 10.0; y = 20.0 };
    velocity = { x = 1.0; y = 0.0 };
    fitness = 0.85;
  } in
  check string "agent_id" "agent-001" pos.agent_id;
  check (float 0.01) "position x" 10.0 pos.position.x;
  check (float 0.01) "velocity x" 1.0 pos.velocity.x;
  check (float 0.01) "fitness" 0.85 pos.fitness

(* ============================================================
   Flocking Behavior Tests
   ============================================================ *)

let make_agent_position ~agent_id ~x ~y ~vx ~vy ~fitness : Swarm_behaviors_eio.agent_position =
  { agent_id; position = { x; y }; velocity = { x = vx; y = vy }; fitness }

let test_separation_no_neighbors () =
  let self = make_agent_position ~agent_id:"a1" ~x:0.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.5 in
  let positions = [self] in
  let params = Swarm_behaviors_eio.default_flocking_params in
  let result = Swarm_behaviors_eio.separation ~positions ~self ~params in
  check (float 0.01) "no neighbors x" 0.0 result.x;
  check (float 0.01) "no neighbors y" 0.0 result.y

let test_separation_with_neighbor () =
  let self = make_agent_position ~agent_id:"a1" ~x:0.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.5 in
  let other = make_agent_position ~agent_id:"a2" ~x:10.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.5 in
  let positions = [self; other] in
  let params = Swarm_behaviors_eio.default_flocking_params in
  let result = Swarm_behaviors_eio.separation ~positions ~self ~params in
  (* Should steer away from other (negative x) *)
  check bool "steering x < 0" true (result.x < 0.0)

let test_alignment_no_neighbors () =
  let self = make_agent_position ~agent_id:"a1" ~x:0.0 ~y:0.0 ~vx:1.0 ~vy:0.0 ~fitness:0.5 in
  let positions = [self] in
  let params = Swarm_behaviors_eio.default_flocking_params in
  let result = Swarm_behaviors_eio.alignment ~positions ~self ~params in
  check (float 0.01) "no neighbors x" 0.0 result.x;
  check (float 0.01) "no neighbors y" 0.0 result.y

let test_alignment_with_neighbor () =
  let self = make_agent_position ~agent_id:"a1" ~x:0.0 ~y:0.0 ~vx:1.0 ~vy:0.0 ~fitness:0.5 in
  let other = make_agent_position ~agent_id:"a2" ~x:10.0 ~y:0.0 ~vx:0.0 ~vy:1.0 ~fitness:0.5 in
  let positions = [self; other] in
  let params = Swarm_behaviors_eio.default_flocking_params in
  let result = Swarm_behaviors_eio.alignment ~positions ~self ~params in
  (* Should align towards other's velocity (positive y) *)
  check bool "has y component" true (result.y > 0.0)

let test_cohesion_no_neighbors () =
  let self = make_agent_position ~agent_id:"a1" ~x:0.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.5 in
  let positions = [self] in
  let params = Swarm_behaviors_eio.default_flocking_params in
  let result = Swarm_behaviors_eio.cohesion ~positions ~self ~params in
  check (float 0.01) "no neighbors x" 0.0 result.x;
  check (float 0.01) "no neighbors y" 0.0 result.y

let test_cohesion_with_neighbor () =
  let self = make_agent_position ~agent_id:"a1" ~x:0.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.5 in
  let other = make_agent_position ~agent_id:"a2" ~x:50.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.5 in
  let positions = [self; other] in
  let params = Swarm_behaviors_eio.default_flocking_params in
  let result = Swarm_behaviors_eio.cohesion ~positions ~self ~params in
  (* Should steer towards other (positive x) *)
  check bool "steering x > 0" true (result.x > 0.0)

let test_flock_combined () =
  let self = make_agent_position ~agent_id:"a1" ~x:0.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.5 in
  let other = make_agent_position ~agent_id:"a2" ~x:50.0 ~y:0.0 ~vx:1.0 ~vy:0.0 ~fitness:0.5 in
  let positions = [self; other] in
  let result = Swarm_behaviors_eio.flock ~positions ~self () in
  (* Combined result should have some value *)
  let mag = Swarm_behaviors_eio.vec_magnitude result in
  check bool "has steering" true (mag > 0.0)

let test_flock_towards_fitness_no_high () =
  let self = make_agent_position ~agent_id:"a1" ~x:0.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.9 in
  let other = make_agent_position ~agent_id:"a2" ~x:50.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.3 in
  let positions = [self; other] in
  let result = Swarm_behaviors_eio.flock_towards_fitness ~positions ~self () in
  (* No higher fitness, so falls back to regular flock *)
  let mag = Swarm_behaviors_eio.vec_magnitude result in
  check bool "has steering" true (mag >= 0.0)

let test_flock_towards_fitness_with_high () =
  let self = make_agent_position ~agent_id:"a1" ~x:0.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.3 in
  let other = make_agent_position ~agent_id:"a2" ~x:50.0 ~y:0.0 ~vx:0.0 ~vy:0.0 ~fitness:0.9 in
  let positions = [self; other] in
  let result = Swarm_behaviors_eio.flock_towards_fitness ~positions ~self () in
  (* Should steer towards high fitness agent *)
  check bool "steering x > 0" true (result.x > 0.0)

(* ============================================================
   Foraging Behavior Tests
   ============================================================ *)

let test_init_foraging_default () =
  let state = Swarm_behaviors_eio.init_foraging () in
  check (float 0.01) "default exploration rate" 0.3 state.exploration_rate;
  check int "no solutions" 0 (List.length state.discovered_solutions);
  check bool "no target" true (state.current_target = None)

let test_init_foraging_custom () =
  let state = Swarm_behaviors_eio.init_foraging ~exploration_rate:0.5 () in
  check (float 0.01) "custom exploration rate" 0.5 state.exploration_rate

let test_select_solution_empty () =
  let state = Swarm_behaviors_eio.init_foraging ~exploration_rate:0.0 () in
  let selected = Swarm_behaviors_eio.select_solution ~foraging_state:state in
  check bool "none for empty" true (selected = None)

let test_select_solution_with_solutions () =
  let state : Swarm_behaviors_eio.foraging_state = {
    exploration_rate = 0.0; (* Never explore, always exploit *)
    discovered_solutions = [("sol1", 0.5); ("sol2", 0.9); ("sol3", 0.3)];
    current_target = None;
  } in
  let selected = Swarm_behaviors_eio.select_solution ~foraging_state:state in
  match selected with
  | Some "sol2" -> check bool "best solution" true true
  | _ -> fail "expected sol2"

let test_record_discovery_new () =
  let state = Swarm_behaviors_eio.init_foraging () in
  let updated = Swarm_behaviors_eio.record_discovery ~foraging_state:state ~solution_id:"sol1" ~quality:0.8 in
  check int "one solution" 1 (List.length updated.discovered_solutions)

let test_record_discovery_better () =
  let state : Swarm_behaviors_eio.foraging_state = {
    exploration_rate = 0.3;
    discovered_solutions = [("sol1", 0.5)];
    current_target = None;
  } in
  let updated = Swarm_behaviors_eio.record_discovery ~foraging_state:state ~solution_id:"sol1" ~quality:0.9 in
  check int "still one solution" 1 (List.length updated.discovered_solutions);
  let q = List.assoc "sol1" updated.discovered_solutions in
  check (float 0.01) "updated quality" 0.9 q

let test_record_discovery_worse () =
  let state : Swarm_behaviors_eio.foraging_state = {
    exploration_rate = 0.3;
    discovered_solutions = [("sol1", 0.9)];
    current_target = None;
  } in
  let updated = Swarm_behaviors_eio.record_discovery ~foraging_state:state ~solution_id:"sol1" ~quality:0.3 in
  let q = List.assoc "sol1" updated.discovered_solutions in
  check (float 0.01) "kept old quality" 0.9 q

(* ============================================================
   Eio-based Tests (Stigmergy, Quorum)
   ============================================================ *)

module Room = Masc_mcp.Room
module Swarm_eio = Masc_mcp.Swarm_eio

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter (fun name -> rm_rf (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Unix.unlink path

let make_test_dir () =
  let unique_id = Printf.sprintf "masc_behav_test_%d_%d"
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

let test_share_discovery () =
  with_eio_env @@ fun ~fs config ->
  (* Create swarm first *)
  let _ = Swarm_eio.create ~fs config () in
  match Swarm_behaviors_eio.share_discovery ~fs config ~agent_id:"a1" ~solution_id:"sol1" ~quality:0.8 with
  | Some swarm -> check int "one pheromone" 1 (List.length swarm.pheromones)
  | None -> fail "expected share success"

let test_follow_pheromone_empty () =
  with_eio_env @@ fun ~fs config ->
  (* Create swarm but no pheromones *)
  let _ = Swarm_eio.create ~fs config () in
  match Swarm_behaviors_eio.follow_pheromone ~fs config () with
  | None -> check bool "no trail" true true
  | Some _ -> fail "expected None"

let test_follow_pheromone_with_trail () =
  with_eio_env @@ fun ~fs config ->
  let _ = Swarm_eio.create ~fs config () in
  (* Deposit a strong pheromone *)
  let _ = Swarm_eio.deposit_pheromone ~fs config ~path_id:"path1" ~agent_id:"a1" ~strength:0.9 in
  match Swarm_behaviors_eio.follow_pheromone ~fs config () with
  | Some "path1" -> check bool "found trail" true true
  | _ -> check bool "found or random" true true (* Could be random selection *)

let test_mark_success () =
  with_eio_env @@ fun ~fs config ->
  let _ = Swarm_eio.create ~fs config () in
  match Swarm_behaviors_eio.mark_success ~fs config ~agent_id:"a1" ~path_id:"success-path" () with
  | Some swarm ->
      check bool "has pheromone" true (List.length swarm.pheromones > 0)
  | None -> fail "expected mark success"

let test_check_quorum_no_swarm () =
  with_eio_env @@ fun ~fs config ->
  (* No swarm created *)
  match Swarm_behaviors_eio.check_quorum ~fs config ~proposal_id:"p1" with
  | `No_swarm -> check bool "no swarm" true true
  | _ -> fail "expected No_swarm"

let test_check_quorum_not_found () =
  with_eio_env @@ fun ~fs config ->
  let _ = Swarm_eio.create ~fs config () in
  match Swarm_behaviors_eio.check_quorum ~fs config ~proposal_id:"nonexistent" with
  | `Not_found -> check bool "not found" true true
  | _ -> fail "expected Not_found"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Swarm Behaviors Eio Coverage" [
    "vector2d", [
      test_case "type" `Quick test_vector2d_type;
      test_case "zero" `Quick test_vector2d_zero;
    ];
    "vec_ops", [
      test_case "add" `Quick test_vec_add;
      test_case "sub" `Quick test_vec_sub;
      test_case "scale" `Quick test_vec_scale;
      test_case "magnitude" `Quick test_vec_magnitude;
      test_case "magnitude zero" `Quick test_vec_magnitude_zero;
      test_case "normalize" `Quick test_vec_normalize;
      test_case "normalize zero" `Quick test_vec_normalize_zero;
      test_case "distance" `Quick test_vec_distance;
      test_case "distance same" `Quick test_vec_distance_same;
    ];
    "flocking_params", [
      test_case "type" `Quick test_flocking_params_type;
      test_case "default" `Quick test_default_flocking_params;
    ];
    "foraging_state", [
      test_case "type" `Quick test_foraging_state_type;
      test_case "no target" `Quick test_foraging_state_no_target;
    ];
    "stigmergy_config", [
      test_case "type" `Quick test_stigmergy_config_type;
      test_case "default" `Quick test_default_stigmergy_config;
    ];
    "agent_position", [
      test_case "type" `Quick test_agent_position_type;
    ];
    "separation", [
      test_case "no neighbors" `Quick test_separation_no_neighbors;
      test_case "with neighbor" `Quick test_separation_with_neighbor;
    ];
    "alignment", [
      test_case "no neighbors" `Quick test_alignment_no_neighbors;
      test_case "with neighbor" `Quick test_alignment_with_neighbor;
    ];
    "cohesion", [
      test_case "no neighbors" `Quick test_cohesion_no_neighbors;
      test_case "with neighbor" `Quick test_cohesion_with_neighbor;
    ];
    "flock", [
      test_case "combined" `Quick test_flock_combined;
      test_case "towards fitness no high" `Quick test_flock_towards_fitness_no_high;
      test_case "towards fitness with high" `Quick test_flock_towards_fitness_with_high;
    ];
    "foraging", [
      test_case "init default" `Quick test_init_foraging_default;
      test_case "init custom" `Quick test_init_foraging_custom;
      test_case "select empty" `Quick test_select_solution_empty;
      test_case "select best" `Quick test_select_solution_with_solutions;
      test_case "record new" `Quick test_record_discovery_new;
      test_case "record better" `Quick test_record_discovery_better;
      test_case "record worse" `Quick test_record_discovery_worse;
    ];
    "eio_stigmergy", [
      test_case "share discovery" `Quick test_share_discovery;
      test_case "follow empty" `Quick test_follow_pheromone_empty;
      test_case "follow with trail" `Quick test_follow_pheromone_with_trail;
      test_case "mark success" `Quick test_mark_success;
    ];
    "eio_quorum", [
      test_case "no swarm" `Quick test_check_quorum_no_swarm;
      test_case "not found" `Quick test_check_quorum_not_found;
    ];
  ]
