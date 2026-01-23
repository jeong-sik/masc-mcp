(** MASC Swarm Behaviors (Eio Native) *)

(** {1 Types} *)

type vector2d = { x: float; y: float }

type flocking_params = {
  separation_weight: float;
  alignment_weight: float;
  cohesion_weight: float;
  perception_radius: float;
}

type foraging_state = {
  exploration_rate: float;
  discovered_solutions: (string * float) list;
  current_target: string option;
}

type stigmergy_config = {
  deposit_rate: float;
  evaporation_rate: float;
  following_threshold: float;
}

type config = Room_utils.config

(** {1 Default Parameters} *)

let default_flocking_params = {
  separation_weight = 1.5;
  alignment_weight = 1.0;
  cohesion_weight = 1.0;
  perception_radius = 100.0;
}

let default_stigmergy_config = {
  deposit_rate = 0.2;
  evaporation_rate = 0.1;
  following_threshold = 0.1;
}

(** {1 Vector Operations} *)

let vec_zero = { x = 0.0; y = 0.0 }
let vec_add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y }
let vec_sub v1 v2 = { x = v1.x -. v2.x; y = v1.y -. v2.y }
let vec_scale v s = { x = v.x *. s; y = v.y *. s }
let vec_magnitude v = sqrt (v.x *. v.x +. v.y *. v.y)
let vec_normalize v =
  let mag = vec_magnitude v in
  if mag > 0.0 then vec_scale v (1.0 /. mag) else vec_zero
let vec_distance v1 v2 = vec_magnitude (vec_sub v1 v2)

(** {1 Flocking Behavior} *)

type agent_position = {
  agent_id: string;
  position: vector2d;
  velocity: vector2d;
  fitness: float;
}

let separation ~positions ~self ~params =
  let nearby = List.filter (fun p ->
    p.agent_id <> self.agent_id &&
    vec_distance p.position self.position < params.perception_radius
  ) positions in
  if nearby = [] then vec_zero
  else
    let steer = List.fold_left (fun acc p ->
      let d = vec_distance p.position self.position in
      if d > 0.0 then
        let diff = vec_sub self.position p.position in
        vec_add acc (vec_scale (vec_normalize diff) (1.0 /. d))
      else acc
    ) vec_zero nearby in
    vec_scale (vec_normalize steer) params.separation_weight

let alignment ~positions ~self ~params =
  let nearby = List.filter (fun p ->
    p.agent_id <> self.agent_id &&
    vec_distance p.position self.position < params.perception_radius
  ) positions in
  if nearby = [] then vec_zero
  else
    let avg_velocity =
      List.fold_left (fun acc p -> vec_add acc p.velocity) vec_zero nearby
      |> fun v -> vec_scale v (1.0 /. float_of_int (List.length nearby))
    in
    vec_scale (vec_normalize avg_velocity) params.alignment_weight

let cohesion ~positions ~self ~params =
  let nearby = List.filter (fun p ->
    p.agent_id <> self.agent_id &&
    vec_distance p.position self.position < params.perception_radius
  ) positions in
  if nearby = [] then vec_zero
  else
    let center =
      List.fold_left (fun acc p -> vec_add acc p.position) vec_zero nearby
      |> fun v -> vec_scale v (1.0 /. float_of_int (List.length nearby))
    in
    let desired = vec_sub center self.position in
    vec_scale (vec_normalize desired) params.cohesion_weight

let flock ~positions ~self ?(params = default_flocking_params) () =
  let sep = separation ~positions ~self ~params in
  let ali = alignment ~positions ~self ~params in
  let coh = cohesion ~positions ~self ~params in
  vec_add (vec_add sep ali) coh

let flock_towards_fitness ~positions ~self ?(params = default_flocking_params) () =
  let high_fitness = List.filter (fun p -> p.fitness > self.fitness) positions in
  if high_fitness = [] then flock ~positions ~self ~params ()
  else
    let enhanced = { params with cohesion_weight = params.cohesion_weight *. 2.0 } in
    flock ~positions:high_fitness ~self ~params:enhanced ()

(** {1 Foraging Behavior} *)

let init_foraging ?(exploration_rate = 0.3) () = {
  exploration_rate;
  discovered_solutions = [];
  current_target = None;
}

let select_solution ~foraging_state =
  if Level4_config.random_float 1.0 < foraging_state.exploration_rate then None
  else
    match List.sort (fun (_, q1) (_, q2) -> compare q2 q1) foraging_state.discovered_solutions with
    | (id, _) :: _ -> Some id
    | [] -> None

let record_discovery ~foraging_state ~solution_id ~quality =
  let existing = List.assoc_opt solution_id foraging_state.discovered_solutions in
  let solutions = match existing with
    | Some old_q when quality > old_q ->
        (solution_id, quality) :: List.remove_assoc solution_id foraging_state.discovered_solutions
    | Some _ -> foraging_state.discovered_solutions
    | None -> (solution_id, quality) :: foraging_state.discovered_solutions
  in
  { foraging_state with discovered_solutions = solutions }

let share_discovery ~fs config ~agent_id ~solution_id ~quality =
  Swarm_eio.deposit_pheromone ~fs config ~path_id:solution_id ~agent_id ~strength:quality

(** {1 Stigmergy Behavior} *)

let follow_pheromone ~fs config ?(stigmergy = default_stigmergy_config) () =
  let trails = Swarm_eio.get_strongest_trails ~fs config ~limit:5 in
  let viable = List.filter (fun p -> p.Swarm_eio.strength >= stigmergy.following_threshold) trails in
  match viable with
  | [] -> None
  | trails ->
      let total = List.fold_left (fun acc t -> acc +. t.Swarm_eio.strength) 0.0 trails in
      let r = Level4_config.random_float total in
      let rec select acc = function
        | [] -> None
        | t :: rest ->
            let acc' = acc +. t.Swarm_eio.strength in
            if r < acc' then Some t.Swarm_eio.path_id else select acc' rest
      in
      select 0.0 trails

let mark_success ~fs config ~agent_id ~path_id ?(stigmergy = default_stigmergy_config) () =
  Swarm_eio.deposit_pheromone ~fs config ~path_id ~agent_id ~strength:stigmergy.deposit_rate

(** {1 Quorum Sensing Behavior} *)

let check_quorum ~fs config ~proposal_id =
  match Swarm_eio.load_swarm ~fs config with
  | None -> `No_swarm
  | Some swarm ->
      match List.find_opt (fun p -> p.Swarm_eio.proposal_id = proposal_id) swarm.proposals with
      | None -> `Not_found
      | Some proposal ->
          let total = List.length swarm.agents in
          let votes = List.length proposal.votes_for in
          let ratio = if total > 0 then float_of_int votes /. float_of_int total else 0.0 in
          if ratio >= proposal.threshold then `Quorum_reached
          else `Progress (votes, total, proposal.threshold)

let propose_action ~fs config ~agent_id ~description =
  Swarm_eio.propose ~fs config ~description ~proposed_by:agent_id ()

let vote_on_proposal ~fs config ~agent_id ~proposal_id ~support =
  Swarm_eio.vote ~fs config ~proposal_id ~agent_id ~vote_for:support

(** {1 Behavior Execution} *)

let execute_behavior ~fs config ~agent_id =
  match Swarm_eio.load_swarm ~fs config with
  | None -> `Error "No swarm exists"
  | Some swarm ->
      match swarm.swarm_cfg.behavior with
      | Swarm_eio.Flocking -> `Guidance "Cluster around high-fitness agents"
      | Swarm_eio.Foraging ->
          (match follow_pheromone ~fs config () with
           | Some path -> `Follow_path path
           | None -> `Explore "No strong trails, explore new paths")
      | Swarm_eio.Stigmergy ->
          let trails = Swarm_eio.get_strongest_trails ~fs config ~limit:3 in
          if trails <> [] then `Follow_trails (List.map (fun t -> t.Swarm_eio.path_id) trails)
          else `Deposit "No trails found, deposit new pheromones"
      | Swarm_eio.Quorum_sensing ->
          let pending = Swarm_eio.get_pending_proposals ~fs config in
          let unvoted = List.filter (fun (p : Swarm_eio.quorum_proposal) ->
            not (List.mem agent_id p.votes_for) && not (List.mem agent_id p.votes_against)
          ) pending in
          if unvoted <> [] then `Vote_needed (List.map (fun (p : Swarm_eio.quorum_proposal) -> p.proposal_id) unvoted)
          else `No_action "No pending votes"

let recommend_behavior ~fs config =
  match Swarm_eio.load_swarm ~fs config with
  | None -> Swarm_eio.Flocking
  | Some swarm ->
      let agent_count = List.length swarm.agents in
      let pheromone_count = List.length swarm.pheromones in
      let pending_proposals = List.length (List.filter (fun (p : Swarm_eio.quorum_proposal) -> p.status = `Pending) swarm.proposals) in
      if pending_proposals > 0 then Swarm_eio.Quorum_sensing
      else if pheromone_count > agent_count then Swarm_eio.Stigmergy
      else if agent_count < 5 then Swarm_eio.Foraging
      else Swarm_eio.Flocking
