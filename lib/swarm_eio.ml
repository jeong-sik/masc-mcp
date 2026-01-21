(** MASC Swarm - Level 4 Emergent Collective Intelligence (Eio Native) *)


(** {1 Types} *)

type swarm_behavior =
  | Flocking
  | Foraging
  | Stigmergy
  | Quorum_sensing

type swarm_agent = {
  id: string;
  name: string;
  fitness: float;
  generation: int;
  mutations: string list;
  joined_at: float;
  last_active: float;
}

type pheromone = {
  path_id: string;
  strength: float;
  deposited_by: string;
  deposited_at: float;
  evaporation_rate: float;
}

type quorum_proposal = {
  proposal_id: string;
  description: string;
  proposed_by: string;
  proposed_at: float;
  votes_for: string list;
  votes_against: string list;
  threshold: float;
  deadline: float option;
  status: [`Pending | `Passed | `Rejected | `Expired];
}

type swarm_config = {
  id: string;
  name: string;
  selection_pressure: float;
  mutation_rate: float;
  evaporation_rate: float;
  quorum_threshold: float;
  max_agents: int;
  behavior: swarm_behavior;
}

type swarm = {
  swarm_cfg: swarm_config;
  agents: swarm_agent list;
  pheromones: pheromone list;
  proposals: quorum_proposal list;
  generation: int;
  created_at: float;
  last_evolution: float;
}

type config = Room_utils.config

(** {1 Default Configuration} *)

let default_config ?(id = "") ?(name = "default-swarm") () = {
  id = if id = "" then Printf.sprintf "swarm-%d" (Level4_config.random_int 100000) else id;
  name;
  selection_pressure = 0.3;
  mutation_rate = 0.1;
  evaporation_rate = 0.1;
  quorum_threshold = 0.6;
  max_agents = 50;
  behavior = Flocking;
}

(** {1 Serialization} *)

let behavior_to_string = function
  | Flocking -> "flocking"
  | Foraging -> "foraging"
  | Stigmergy -> "stigmergy"
  | Quorum_sensing -> "quorum_sensing"

let behavior_of_string = function
  | "flocking" -> Flocking
  | "foraging" -> Foraging
  | "stigmergy" -> Stigmergy
  | "quorum_sensing" -> Quorum_sensing
  | _ -> Flocking

let status_to_string = function
  | `Pending -> "pending"
  | `Passed -> "passed"
  | `Rejected -> "rejected"
  | `Expired -> "expired"

let status_of_string = function
  | "pending" -> `Pending
  | "passed" -> `Passed
  | "rejected" -> `Rejected
  | "expired" -> `Expired
  | _ -> `Pending

let agent_to_json (a : swarm_agent) : Yojson.Safe.t =
  `Assoc [
    ("id", `String a.id);
    ("name", `String a.name);
    ("fitness", `Float a.fitness);
    ("generation", `Int a.generation);
    ("mutations", `List (List.map (fun m -> `String m) a.mutations));
    ("joined_at", `Float a.joined_at);
    ("last_active", `Float a.last_active);
  ]

let agent_of_json json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    fitness = json |> member "fitness" |> to_float;
    generation = json |> member "generation" |> to_int;
    mutations = json |> member "mutations" |> to_list |> List.map to_string;
    joined_at = json |> member "joined_at" |> to_float;
    last_active = json |> member "last_active" |> to_float;
  }

let pheromone_to_json (p : pheromone) : Yojson.Safe.t =
  `Assoc [
    ("path_id", `String p.path_id);
    ("strength", `Float p.strength);
    ("deposited_by", `String p.deposited_by);
    ("deposited_at", `Float p.deposited_at);
    ("evaporation_rate", `Float p.evaporation_rate);
  ]

let pheromone_of_json json =
  let open Yojson.Safe.Util in
  {
    path_id = json |> member "path_id" |> to_string;
    strength = json |> member "strength" |> to_float;
    deposited_by = json |> member "deposited_by" |> to_string;
    deposited_at = json |> member "deposited_at" |> to_float;
    evaporation_rate = json |> member "evaporation_rate" |> to_float;
  }

let proposal_to_json (p : quorum_proposal) : Yojson.Safe.t =
  `Assoc [
    ("proposal_id", `String p.proposal_id);
    ("description", `String p.description);
    ("proposed_by", `String p.proposed_by);
    ("proposed_at", `Float p.proposed_at);
    ("votes_for", `List (List.map (fun v -> `String v) p.votes_for));
    ("votes_against", `List (List.map (fun v -> `String v) p.votes_against));
    ("threshold", `Float p.threshold);
    ("deadline", match p.deadline with Some d -> `Float d | None -> `Null);
    ("status", `String (status_to_string p.status));
  ]

let proposal_of_json json =
  let open Yojson.Safe.Util in
  {
    proposal_id = json |> member "proposal_id" |> to_string;
    description = json |> member "description" |> to_string;
    proposed_by = json |> member "proposed_by" |> to_string;
    proposed_at = json |> member "proposed_at" |> to_float;
    votes_for = json |> member "votes_for" |> to_list |> List.map to_string;
    votes_against = json |> member "votes_against" |> to_list |> List.map to_string;
    threshold = json |> member "threshold" |> to_float;
    deadline = json |> member "deadline" |> to_float_option;
    status = json |> member "status" |> to_string |> status_of_string;
  }

let config_to_json (c : swarm_config) : Yojson.Safe.t =
  `Assoc [
    ("id", `String c.id);
    ("name", `String c.name);
    ("selection_pressure", `Float c.selection_pressure);
    ("mutation_rate", `Float c.mutation_rate);
    ("evaporation_rate", `Float c.evaporation_rate);
    ("quorum_threshold", `Float c.quorum_threshold);
    ("max_agents", `Int c.max_agents);
    ("behavior", `String (behavior_to_string c.behavior));
  ]

let config_of_json json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    selection_pressure = json |> member "selection_pressure" |> to_float;
    mutation_rate = json |> member "mutation_rate" |> to_float;
    evaporation_rate = json |> member "evaporation_rate" |> to_float;
    quorum_threshold = json |> member "quorum_threshold" |> to_float;
    max_agents = json |> member "max_agents" |> to_int;
    behavior = json |> member "behavior" |> to_string |> behavior_of_string;
  }

let swarm_to_json (s : swarm) : Yojson.Safe.t =
  `Assoc [
    ("config", config_to_json s.swarm_cfg);
    ("agents", `List (List.map agent_to_json s.agents));
    ("pheromones", `List (List.map pheromone_to_json s.pheromones));
    ("proposals", `List (List.map proposal_to_json s.proposals));
    ("generation", `Int s.generation);
    ("created_at", `Float s.created_at);
    ("last_evolution", `Float s.last_evolution);
  ]

let swarm_of_json json =
  let open Yojson.Safe.Util in
  {
    swarm_cfg = json |> member "config" |> config_of_json;
    agents = json |> member "agents" |> to_list |> List.map agent_of_json;
    pheromones = json |> member "pheromones" |> to_list |> List.map pheromone_of_json;
    proposals = json |> member "proposals" |> to_list |> List.map proposal_of_json;
    generation = json |> member "generation" |> to_int;
    created_at = json |> member "created_at" |> to_float;
    last_evolution = json |> member "last_evolution" |> to_float;
  }

(** {1 Persistence (Eio Native)} *)

let swarm_file (config : config) =
  Filename.concat config.base_path ".masc/swarm.json"

let load_swarm ~fs (config : config) : swarm option =
  let file = swarm_file config in
  let path = Eio.Path.(fs / file) in
  try
    let content = Eio.Path.load path in
    let json = Yojson.Safe.from_string content in
    Some (swarm_of_json json)
  with _ -> None

let save_swarm ~fs (config : config) (swarm : swarm) : unit =
  let file = swarm_file config in
  let dir = Filename.dirname file in
  let dir_path = Eio.Path.(fs / dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir_path;
  let json = swarm_to_json swarm in
  let path = Eio.Path.(fs / file) in
  Eio.Path.save ~create:(`Or_truncate 0o600) path (Yojson.Safe.pretty_to_string json)

(** {1 Pure Transformations} *)

module Pure = struct
  type join_result =
    | Joined of swarm
    | Already_member of swarm
    | Swarm_full

  let join_agent swarm ~agent_id ~agent_name ~now =
    if List.length swarm.agents >= swarm.swarm_cfg.max_agents then
      Swarm_full
    else if List.exists (fun (a : swarm_agent) -> a.id = agent_id) swarm.agents then
      Already_member swarm
    else
      let initial_fitness = Level4_config.Swarm.initial_fitness () in
      let agent = {
        id = agent_id;
        name = agent_name;
        fitness = initial_fitness;
        generation = swarm.generation;
        mutations = [];
        joined_at = now;
        last_active = now;
      } in
      Joined { swarm with agents = agent :: swarm.agents }

  let leave_agent swarm ~agent_id =
    let agents = List.filter (fun (a : swarm_agent) -> a.id <> agent_id) swarm.agents in
    { swarm with agents }

  let update_agent_fitness swarm ~agent_id ~fitness ~now =
    if not (List.exists (fun (a : swarm_agent) -> a.id = agent_id) swarm.agents) then
      None
    else
      match Level4_config.Fitness.of_float fitness with
      | None -> None
      | Some validated_fitness ->
        let fitness_float = Level4_config.Fitness.to_float validated_fitness in
        let agents = List.map (fun (a : swarm_agent) ->
          if a.id = agent_id then
            { a with fitness = fitness_float; last_active = now }
          else a
        ) swarm.agents in
        Some { swarm with agents }

  let fitness_rankings swarm =
    let rankings = List.map (fun (a : swarm_agent) -> (a.id, a.fitness)) swarm.agents in
    List.sort (fun (_, f1) (_, f2) -> compare f2 f1) rankings

  let select_elite_agents swarm =
    let sorted = List.sort (fun a b -> compare b.fitness a.fitness) swarm.agents in
    let elite_count = max 1 (int_of_float (
      float_of_int (List.length sorted) *. swarm.swarm_cfg.selection_pressure
    )) in
    List.filteri (fun i _ -> i < elite_count) sorted

  let evolve_agents swarm ~now =
    let agents = List.map (fun (a : swarm_agent) ->
      if Level4_config.random_float 1.0 < swarm.swarm_cfg.mutation_rate then
        let mutation = Printf.sprintf "gen%d-mut%d" (swarm.generation + 1) (Level4_config.random_int 1000) in
        { a with
          mutations = mutation :: a.mutations;
          generation = swarm.generation + 1;
        }
      else
        { a with generation = swarm.generation + 1 }
    ) swarm.agents in
    { swarm with
      agents;
      generation = swarm.generation + 1;
      last_evolution = now;
    }

  let deposit_pheromone swarm ~path_id ~agent_id ~strength ~now =
    let evap_rate = swarm.swarm_cfg.evaporation_rate in
    let existing = List.find_opt (fun p -> p.path_id = path_id) swarm.pheromones in
    let pheromones = match existing with
      | Some p ->
        let updated = { p with
          strength = min 1.0 (p.strength +. strength);
          deposited_by = agent_id;
          deposited_at = now;
        } in
        updated :: List.filter (fun x -> x.path_id <> path_id) swarm.pheromones
      | None ->
        let new_pheromone = {
          path_id;
          strength = min 1.0 strength;
          deposited_by = agent_id;
          deposited_at = now;
          evaporation_rate = evap_rate;
        } in
        new_pheromone :: swarm.pheromones
    in
    { swarm with pheromones }

  let evaporate_pheromones swarm ~now =
    let pheromones = List.filter_map (fun p ->
      let elapsed_hours = (now -. p.deposited_at) /. 3600.0 in
      let decay = p.evaporation_rate *. elapsed_hours in
      let new_strength = p.strength -. decay in
      if new_strength <= 0.0 then None
      else Some { p with strength = new_strength }
    ) swarm.pheromones in
    { swarm with pheromones }

  let strongest_trails swarm ~limit =
    let sorted = List.sort (fun a b -> compare b.strength a.strength) swarm.pheromones in
    List.filteri (fun i _ -> i < limit) sorted

  let add_proposal swarm ~proposal =
    { swarm with proposals = proposal :: swarm.proposals }

  let record_vote swarm ~proposal_id ~agent_id ~vote_for =
    let proposals = List.map (fun p ->
      if p.proposal_id = proposal_id then
        let votes_for = List.filter ((<>) agent_id) p.votes_for in
        let votes_against = List.filter ((<>) agent_id) p.votes_against in
        if vote_for then
          { p with votes_for = agent_id :: votes_for; votes_against }
        else
          { p with votes_for; votes_against = agent_id :: votes_against }
      else p
    ) swarm.proposals in
    { swarm with proposals }

  let update_proposal_status swarm ~proposal_id ~now =
    let total_agents = List.length swarm.agents in
    if total_agents = 0 then swarm
    else
      let proposals = List.map (fun p ->
        if p.proposal_id = proposal_id && p.status = `Pending then
          let for_ratio = float_of_int (List.length p.votes_for) /. float_of_int total_agents in
          let against_ratio = float_of_int (List.length p.votes_against) /. float_of_int total_agents in
          let expired = match p.deadline with
            | Some d -> now > d
            | None -> false
          in
          let status =
            if for_ratio >= p.threshold then `Passed
            else if against_ratio > (1.0 -. p.threshold) then `Rejected
            else if expired then `Expired
            else `Pending
          in
          { p with status }
        else p
      ) swarm.proposals in
      { swarm with proposals }
end

(** {1 Lifecycle (Eio)} *)

let create ~fs (config : config) ?(swarm_config = default_config ()) () : swarm =
  let now = Unix.gettimeofday () in
  let swarm = {
    swarm_cfg = swarm_config;
    agents = [];
    pheromones = [];
    proposals = [];
    generation = 0;
    created_at = now;
    last_evolution = now;
  } in
  save_swarm ~fs config swarm;
  swarm

let join ~fs (config : config) ~agent_id ~agent_name : swarm option =
  match load_swarm ~fs config with
  | None -> None
  | Some swarm ->
    let now = Unix.gettimeofday () in
    match Pure.join_agent swarm ~agent_id ~agent_name ~now with
    | Pure.Joined updated ->
      save_swarm ~fs config updated;
      Some updated
    | Pure.Already_member s -> Some s
    | Pure.Swarm_full -> None

let leave ~fs (config : config) ~agent_id : swarm option =
  match load_swarm ~fs config with
  | None -> None
  | Some swarm ->
    let updated = Pure.leave_agent swarm ~agent_id in
    save_swarm ~fs config updated;
    Some updated

let dissolve ~fs (config : config) : unit =
  let file = swarm_file config in
  let path = Eio.Path.(fs / file) in
  try Eio.Path.unlink path with _ -> ()

let update_fitness ~fs (config : config) ~agent_id ~fitness : swarm option =
  match load_swarm ~fs config with
  | None -> None
  | Some swarm ->
    let now = Unix.gettimeofday () in
    match Pure.update_agent_fitness swarm ~agent_id ~fitness ~now with
    | None -> None
    | Some updated ->
      save_swarm ~fs config updated;
      Some updated

let get_fitness_rankings ~fs (config : config) : (string * float) list =
  match load_swarm ~fs config with
  | None -> []
  | Some swarm -> Pure.fitness_rankings swarm

let select_elite ~fs (config : config) : swarm_agent list =
  match load_swarm ~fs config with
  | None -> []
  | Some swarm -> Pure.select_elite_agents swarm

let evolve ~fs (config : config) : swarm option =
  match load_swarm ~fs config with
  | None -> None
  | Some swarm ->
    let now = Unix.gettimeofday () in
    let updated = Pure.evolve_agents swarm ~now in
    save_swarm ~fs config updated;
    Some updated

let deposit_pheromone ~fs (config : config) ~path_id ~agent_id ~strength : swarm option =
  match load_swarm ~fs config with
  | None -> None
  | Some swarm ->
    let now = Unix.gettimeofday () in
    let strength = max 0.0 (min 1.0 strength) in
    let updated = Pure.deposit_pheromone swarm ~path_id ~agent_id ~strength ~now in
    save_swarm ~fs config updated;
    Some updated

let read_pheromone ~fs (config : config) ~path_id : float =
  match load_swarm ~fs config with
  | None -> 0.0
  | Some swarm ->
    let now = Unix.gettimeofday () in
    match List.find_opt (fun p -> p.path_id = path_id) swarm.pheromones with
    | None -> 0.0
    | Some p ->
      let hours_elapsed = (now -. p.deposited_at) /. 3600.0 in
      let decayed = p.strength *. exp (-. p.evaporation_rate *. hours_elapsed) in
      max 0.0 decayed

let evaporate_pheromones ~fs (config : config) : swarm option =
  match load_swarm ~fs config with
  | None -> None
  | Some swarm ->
    let now = Unix.gettimeofday () in
    let updated = Pure.evaporate_pheromones swarm ~now in
    save_swarm ~fs config updated;
    Some updated

let get_strongest_trails ~fs (config : config) ~limit : pheromone list =
  match load_swarm ~fs config with
  | None -> []
  | Some swarm -> Pure.strongest_trails swarm ~limit

let propose ~fs (config : config) ~description ~proposed_by ?threshold ?deadline ()
    : quorum_proposal option =
  match load_swarm ~fs config with
  | None -> None
  | Some swarm ->
    let now = Unix.gettimeofday () in
    let proposal = {
      proposal_id = Printf.sprintf "prop-%d-%d" (int_of_float (now *. 1000.0)) (Level4_config.random_int 10000);
      description;
      proposed_by;
      proposed_at = now;
      votes_for = [proposed_by];
      votes_against = [];
      threshold = Option.value threshold ~default:swarm.swarm_cfg.quorum_threshold;
      deadline;
      status = `Pending;
    } in
    let updated = Pure.add_proposal swarm ~proposal in
    save_swarm ~fs config updated;
    Some proposal

let vote ~fs (config : config) ~proposal_id ~agent_id ~vote_for : quorum_proposal option =
  match load_swarm ~fs config with
  | None -> None
  | Some swarm ->
    let now = Unix.gettimeofday () in
    let with_vote = Pure.record_vote swarm ~proposal_id ~agent_id ~vote_for in
    let updated = Pure.update_proposal_status with_vote ~proposal_id ~now in
    save_swarm ~fs config updated;
    List.find_opt (fun p -> p.proposal_id = proposal_id) updated.proposals

let get_pending_proposals ~fs (config : config) : quorum_proposal list =
  match load_swarm ~fs config with
  | None -> []
  | Some swarm -> List.filter (fun p -> p.status = `Pending) swarm.proposals

let set_behavior ~fs (config : config) ~behavior : swarm option =
  match load_swarm ~fs config with
  | None -> None
  | Some swarm ->
    let updated = { swarm with swarm_cfg = { swarm.swarm_cfg with behavior } } in
    save_swarm ~fs config updated;
    Some updated

let status ~fs (config : config) : Yojson.Safe.t =
  match load_swarm ~fs config with
  | None -> `Assoc [("exists", `Bool false); ("message", `String "No swarm exists")]
  | Some swarm ->
    let elite = select_elite ~fs config in
    `Assoc [
      ("exists", `Bool true);
      ("id", `String swarm.swarm_cfg.id);
      ("name", `String swarm.swarm_cfg.name);
      ("behavior", `String (behavior_to_string swarm.swarm_cfg.behavior));
      ("generation", `Int swarm.generation);
      ("agent_count", `Int (List.length swarm.agents));
      ("max_agents", `Int swarm.swarm_cfg.max_agents);
      ("pheromone_count", `Int (List.length swarm.pheromones));
      ("pending_proposals", `Int (List.length (List.filter (fun p -> p.status = `Pending) swarm.proposals)));
      ("selection_pressure", `Float swarm.swarm_cfg.selection_pressure);
      ("mutation_rate", `Float swarm.swarm_cfg.mutation_rate);
      ("elite_agents", `List (List.map (fun (a : swarm_agent) -> `String a.id) elite));
      ("created_at", `Float swarm.created_at);
      ("last_evolution", `Float swarm.last_evolution);
    ]
