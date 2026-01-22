(** MASC Swarm Behaviors - Emergent Collective Patterns

    Implementation of four core swarm behaviors:
    1. Flocking: Agents cluster around successful patterns
    2. Foraging: Distributed search for solutions
    3. Stigmergy: Indirect communication through environment
    4. Quorum Sensing: Collective decision thresholds

    Research basis:
    - Boids algorithm (Reynolds, 1987)
    - Ant Colony Optimization (Dorigo, 1992)
    - EvoAgent (arxiv.org/abs/2406.14228)

    @author Second Brain
    @since MASC v3.1 (Level 4)
*)

open Lwt.Syntax

(** {1 Types} *)

(** Vector for flocking calculations *)
type vector2d = { x: float; y: float }

(** Flocking parameters *)
type flocking_params = {
  separation_weight: float;  (** Avoid crowding neighbors *)
  alignment_weight: float;   (** Steer towards average heading *)
  cohesion_weight: float;    (** Steer towards average position *)
  perception_radius: float;  (** How far to look for neighbors *)
}

(** Foraging state *)
type foraging_state = {
  exploration_rate: float;   (** 0.0-1.0: exploration vs exploitation *)
  discovered_solutions: (string * float) list;  (** (solution_id, quality) *)
  current_target: string option;
}

(** Stigmergy configuration *)
type stigmergy_config = {
  deposit_rate: float;       (** Pheromone deposit per success *)
  evaporation_rate: float;   (** Decay rate per hour *)
  following_threshold: float; (** Min pheromone to follow *)
}

(** Config type alias *)
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

let vec_distance v1 v2 =
  let d = vec_sub v1 v2 in
  vec_magnitude d

(** {1 Flocking Behavior} *)

(** Agent position for flocking (abstract representation) *)
type agent_position = {
  agent_id: string;
  position: vector2d;
  velocity: vector2d;
  fitness: float;
}

(** Calculate separation force - avoid crowding neighbors *)
let separation ~positions ~self ~params =
  let nearby = List.filter (fun p ->
    p.agent_id <> self.agent_id &&
    vec_distance p.position self.position < params.perception_radius
  ) positions in
  if List.length nearby = 0 then vec_zero
  else
    let steer = List.fold_left (fun acc p ->
      let d = vec_distance p.position self.position in
      if d > 0.0 then
        let diff = vec_sub self.position p.position in
        let weighted = vec_scale (vec_normalize diff) (1.0 /. d) in
        vec_add acc weighted
      else acc
    ) vec_zero nearby in
    vec_scale (vec_normalize steer) params.separation_weight

(** Calculate alignment force - steer towards average heading *)
let alignment ~positions ~self ~params =
  let nearby = List.filter (fun p ->
    p.agent_id <> self.agent_id &&
    vec_distance p.position self.position < params.perception_radius
  ) positions in
  if List.length nearby = 0 then vec_zero
  else
    let avg_velocity = List.fold_left (fun acc p ->
      vec_add acc p.velocity
    ) vec_zero nearby in
    let avg = vec_scale avg_velocity (1.0 /. float_of_int (List.length nearby)) in
    vec_scale (vec_normalize avg) params.alignment_weight

(** Calculate cohesion force - steer towards center of mass *)
let cohesion ~positions ~self ~params =
  let nearby = List.filter (fun p ->
    p.agent_id <> self.agent_id &&
    vec_distance p.position self.position < params.perception_radius
  ) positions in
  if List.length nearby = 0 then vec_zero
  else
    let center = List.fold_left (fun acc p ->
      vec_add acc p.position
    ) vec_zero nearby in
    let avg = vec_scale center (1.0 /. float_of_int (List.length nearby)) in
    let desired = vec_sub avg self.position in
    vec_scale (vec_normalize desired) params.cohesion_weight

(** Calculate combined flocking force *)
let flock ~positions ~self ?(params = default_flocking_params) () =
  let sep = separation ~positions ~self ~params in
  let ali = alignment ~positions ~self ~params in
  let coh = cohesion ~positions ~self ~params in
  vec_add (vec_add sep ali) coh

(** Bias flocking towards high-fitness agents *)
let flock_towards_fitness ~positions ~self ?(params = default_flocking_params) () =
  (* Filter to only high-fitness neighbors *)
  let high_fitness = List.filter (fun p ->
    p.fitness > self.fitness
  ) positions in
  if List.length high_fitness = 0 then
    flock ~positions ~self ~params ()
  else
    (* Stronger cohesion towards high-fitness agents *)
    let enhanced_params = { params with cohesion_weight = params.cohesion_weight *. 2.0 } in
    flock ~positions:high_fitness ~self ~params:enhanced_params ()

(** {1 Foraging Behavior} *)

(** Initialize foraging state *)
let init_foraging ?(exploration_rate = 0.3) () = {
  exploration_rate;
  discovered_solutions = [];
  current_target = None;
}

(** Epsilon-greedy solution selection *)
let select_solution ~foraging_state =
  if Level4_config.random_float 1.0 < foraging_state.exploration_rate then
    (* Explore: random selection *)
    None
  else
    (* Exploit: best known solution *)
    match foraging_state.discovered_solutions with
    | [] -> None
    | solutions ->
      let sorted = List.sort (fun (_, q1) (_, q2) -> compare q2 q1) solutions in
      match sorted with
      | (id, _) :: _ -> Some id
      | [] -> None  (* Already handled by outer match, but exhaustive *)

(** Record discovered solution *)
let record_discovery ~foraging_state ~solution_id ~quality =
  let existing = List.assoc_opt solution_id foraging_state.discovered_solutions in
  let solutions = match existing with
    | Some old_quality ->
      (* Update if better *)
      if quality > old_quality then
        (solution_id, quality) ::
        List.filter (fun (id, _) -> id <> solution_id) foraging_state.discovered_solutions
      else
        foraging_state.discovered_solutions
    | None ->
      (solution_id, quality) :: foraging_state.discovered_solutions
  in
  { foraging_state with discovered_solutions = solutions }

(** Share discoveries with swarm via pheromones *)
let share_discovery config ~agent_id ~solution_id ~quality =
  let strength = quality in  (* Use quality as pheromone strength *)
  Swarm.deposit_pheromone config ~path_id:solution_id ~agent_id ~strength

(** {1 Stigmergy Behavior} *)

(** Get best path based on pheromones *)
let follow_pheromone config ?(stigmergy = default_stigmergy_config) () =
  let* trails = Swarm.get_strongest_trails config ~limit:5 in
  let viable = List.filter (fun p ->
    p.Swarm.strength >= stigmergy.following_threshold
  ) trails in
  match viable with
  | [] -> Lwt.return None
  | trails ->
    (* Probabilistic selection weighted by pheromone strength *)
    let total = List.fold_left (fun acc t -> acc +. t.Swarm.strength) 0.0 trails in
    let r = Level4_config.random_float total in
    let rec select acc = function
      | [] -> None
      | t :: rest ->
        let acc' = acc +. t.Swarm.strength in
        if r < acc' then Some t.Swarm.path_id
        else select acc' rest
    in
    Lwt.return (select 0.0 trails)

(** Deposit pheromone after successful task *)
let mark_success config ~agent_id ~path_id ?(stigmergy = default_stigmergy_config) () =
  Swarm.deposit_pheromone config ~path_id ~agent_id ~strength:stigmergy.deposit_rate

(** {1 Quorum Sensing Behavior} *)

(** Check if enough agents support an action *)
let check_quorum config ~proposal_id =
  let* swarm_opt = Swarm.load_swarm config in
  match swarm_opt with
  | None -> Lwt.return `No_swarm
  | Some swarm ->
    match List.find_opt (fun p -> p.Swarm.proposal_id = proposal_id) swarm.Swarm.proposals with
    | None -> Lwt.return `Not_found
    | Some proposal ->
      let total = List.length swarm.Swarm.agents in
      let votes = List.length proposal.Swarm.votes_for in
      let ratio = if total > 0 then float_of_int votes /. float_of_int total else 0.0 in
      if ratio >= proposal.Swarm.threshold then
        Lwt.return `Quorum_reached
      else
        Lwt.return (`Progress (votes, total, proposal.Swarm.threshold))

(** Propose action and auto-vote *)
let propose_action config ~agent_id ~description =
  Swarm.propose config ~description ~proposed_by:agent_id ()

(** Vote on proposal *)
let vote_on_proposal config ~agent_id ~proposal_id ~support =
  Swarm.vote config ~proposal_id ~agent_id ~vote_for:support

(** {1 Behavior Execution} *)

(** Execute behavior based on swarm mode *)
let execute_behavior config ~agent_id =
  let* swarm_opt = Swarm.load_swarm config in
  match swarm_opt with
  | None -> Lwt.return (`Error "No swarm exists")
  | Some swarm ->
    match swarm.Swarm.swarm_cfg.Swarm.behavior with
    | Swarm.Flocking ->
      (* For flocking, we'd need position data - return guidance *)
      Lwt.return (`Guidance "Cluster around high-fitness agents")

    | Swarm.Foraging ->
      (* Check pheromones for best path *)
      let* best_path = follow_pheromone config () in
      (match best_path with
       | Some path -> Lwt.return (`Follow_path path)
       | None -> Lwt.return (`Explore "No strong trails, explore new paths"))

    | Swarm.Stigmergy ->
      (* Read environment for strongest signals *)
      let* trails = Swarm.get_strongest_trails config ~limit:3 in
      if List.length trails > 0 then
        let paths = List.map (fun t -> t.Swarm.path_id) trails in
        Lwt.return (`Follow_trails paths)
      else
        Lwt.return (`Deposit "No trails found, deposit new pheromones")

    | Swarm.Quorum_sensing ->
      (* Check pending proposals *)
      let* pending = Swarm.get_pending_proposals config in
      let my_unvoted = List.filter (fun p ->
        not (List.mem agent_id p.Swarm.votes_for) &&
        not (List.mem agent_id p.Swarm.votes_against)
      ) pending in
      if List.length my_unvoted > 0 then
        let proposals = List.map (fun p -> p.Swarm.proposal_id) my_unvoted in
        Lwt.return (`Vote_needed proposals)
      else
        Lwt.return (`No_action "No pending votes")

(** {1 Adaptive Behavior Selection} *)

(** Recommend behavior based on swarm state *)
let recommend_behavior config =
  let* swarm_opt = Swarm.load_swarm config in
  match swarm_opt with
  | None -> Lwt.return Swarm.Flocking
  | Some swarm ->
    let agent_count = List.length swarm.Swarm.agents in
    let pheromone_count = List.length swarm.Swarm.pheromones in
    let pending_proposals = List.length (List.filter (fun p ->
      p.Swarm.status = `Pending
    ) swarm.Swarm.proposals) in

    (* Heuristic selection *)
    if pending_proposals > 0 then
      (* Urgent decisions pending *)
      Lwt.return Swarm.Quorum_sensing
    else if pheromone_count > agent_count then
      (* Rich environment, follow trails *)
      Lwt.return Swarm.Stigmergy
    else if agent_count < 5 then
      (* Small swarm, explore *)
      Lwt.return Swarm.Foraging
    else
      (* Default to flocking for coordination *)
      Lwt.return Swarm.Flocking
