(** MASC Swarm - Level 4 Emergent Collective Intelligence

    @author Second Brain
    @since MASC v3.1 (Level 4)
*)

(** {1 Types} *)

(** Swarm behavior modes *)
type swarm_behavior =
  | Flocking       (** Agents cluster around successful patterns *)
  | Foraging       (** Distributed search for solutions *)
  | Stigmergy      (** Indirect communication through environment *)
  | Quorum_sensing (** Collective decision thresholds *)

(** Swarm agent with fitness tracking *)
type swarm_agent = {
  id: string;
  name: string;
  fitness: float;
  generation: int;
  mutations: string list;
  joined_at: float;
  last_active: float;
}

(** Pheromone trail for stigmergy *)
type pheromone = {
  path_id: string;
  strength: float;
  deposited_by: string;
  deposited_at: float;
  evaporation_rate: float;
}

(** Quorum proposal for collective decisions *)
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

(** Swarm configuration *)
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

(** Swarm state *)
type swarm = {
  swarm_cfg: swarm_config;  (** Named to avoid shadowing config type alias *)
  agents: swarm_agent list;
  pheromones: pheromone list;
  proposals: quorum_proposal list;
  generation: int;
  created_at: float;
  last_evolution: float;
}

(** Config type alias *)
type config = Room_utils.config

(** {1 Configuration} *)

val default_config : ?id:string -> ?name:string -> unit -> swarm_config

(** {1 Serialization} *)

val behavior_to_string : swarm_behavior -> string
val behavior_of_string : string -> swarm_behavior
val swarm_to_json : swarm -> Yojson.Safe.t
val swarm_of_json : Yojson.Safe.t -> swarm

(** {1 Persistence} *)

val load_swarm : config -> swarm option Lwt.t
val save_swarm : config -> swarm -> unit Lwt.t

(** {1 Swarm Lifecycle} *)

val create : config -> ?swarm_config:swarm_config -> unit -> swarm Lwt.t
val join : config -> agent_id:string -> agent_name:string -> swarm option Lwt.t
val leave : config -> agent_id:string -> swarm option Lwt.t
val dissolve : config -> unit Lwt.t

(** {1 Fitness Operations} *)

val update_fitness : config -> agent_id:string -> fitness:float -> swarm option Lwt.t
val get_fitness_rankings : config -> (string * float) list Lwt.t
val select_elite : config -> swarm_agent list Lwt.t

(** {1 Evolution} *)

val evolve : config -> swarm option Lwt.t

(** {1 Pheromone Operations (Stigmergy)} *)

val deposit_pheromone : config -> path_id:string -> agent_id:string -> strength:float -> swarm option Lwt.t
val read_pheromone : config -> path_id:string -> float Lwt.t
val evaporate_pheromones : config -> swarm option Lwt.t
val get_strongest_trails : config -> limit:int -> pheromone list Lwt.t

(** {1 Quorum Sensing} *)

val propose : config -> description:string -> proposed_by:string -> ?threshold:float -> ?deadline:float -> unit -> quorum_proposal option Lwt.t
val vote : config -> proposal_id:string -> agent_id:string -> vote_for:bool -> quorum_proposal option Lwt.t
val get_pending_proposals : config -> quorum_proposal list Lwt.t

(** {1 Behavior Operations} *)

val set_behavior : config -> behavior:swarm_behavior -> swarm option Lwt.t

(** {1 Status} *)

val status : config -> Yojson.Safe.t Lwt.t

(** {1 Pure Functions Module}

    I/O-free transformations for testing and composition.
    All functions are pure - they take state and return new state.
*)
module Pure : sig
  type join_result =
    | Joined of swarm
    | Already_member of swarm
    | Swarm_full

  val join_agent : swarm -> agent_id:string -> agent_name:string -> now:float -> join_result
  val leave_agent : swarm -> agent_id:string -> swarm
  val update_agent_fitness : swarm -> agent_id:string -> fitness:float -> now:float -> swarm option
  val fitness_rankings : swarm -> (string * float) list
  val select_elite_agents : swarm -> swarm_agent list
  val evolve_agents : swarm -> now:float -> swarm
  val deposit_pheromone : swarm -> path_id:string -> agent_id:string -> strength:float -> now:float -> swarm
  val evaporate_pheromones : swarm -> now:float -> swarm
  val strongest_trails : swarm -> limit:int -> pheromone list
  val add_proposal : swarm -> proposal:quorum_proposal -> swarm
  val record_vote : swarm -> proposal_id:string -> agent_id:string -> vote_for:bool -> swarm
  val update_proposal_status : swarm -> proposal_id:string -> now:float -> swarm
end
