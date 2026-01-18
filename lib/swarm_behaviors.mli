(** MASC Swarm Behaviors - Emergent Collective Patterns

    @author Second Brain
    @since MASC v3.1 (Level 4)
*)

(** {1 Types} *)

(** Vector for flocking calculations *)
type vector2d = { x: float; y: float }

(** Flocking parameters *)
type flocking_params = {
  separation_weight: float;
  alignment_weight: float;
  cohesion_weight: float;
  perception_radius: float;
}

(** Foraging state *)
type foraging_state = {
  exploration_rate: float;
  discovered_solutions: (string * float) list;
  current_target: string option;
}

(** Stigmergy configuration *)
type stigmergy_config = {
  deposit_rate: float;
  evaporation_rate: float;
  following_threshold: float;
}

(** Config type alias *)
type config = Room_utils.config

(** {1 Default Parameters} *)

val default_flocking_params : flocking_params
val default_stigmergy_config : stigmergy_config

(** {1 Vector Operations} *)

val vec_zero : vector2d
val vec_add : vector2d -> vector2d -> vector2d
val vec_sub : vector2d -> vector2d -> vector2d
val vec_scale : vector2d -> float -> vector2d
val vec_magnitude : vector2d -> float
val vec_normalize : vector2d -> vector2d
val vec_distance : vector2d -> vector2d -> float

(** {1 Flocking Behavior} *)

type agent_position = {
  agent_id: string;
  position: vector2d;
  velocity: vector2d;
  fitness: float;
}

val separation : positions:agent_position list -> self:agent_position -> params:flocking_params -> vector2d
val alignment : positions:agent_position list -> self:agent_position -> params:flocking_params -> vector2d
val cohesion : positions:agent_position list -> self:agent_position -> params:flocking_params -> vector2d
val flock : positions:agent_position list -> self:agent_position -> ?params:flocking_params -> unit -> vector2d
val flock_towards_fitness : positions:agent_position list -> self:agent_position -> ?params:flocking_params -> unit -> vector2d

(** {1 Foraging Behavior} *)

val init_foraging : ?exploration_rate:float -> unit -> foraging_state
val select_solution : foraging_state:foraging_state -> string option
val record_discovery : foraging_state:foraging_state -> solution_id:string -> quality:float -> foraging_state
val share_discovery : config -> agent_id:string -> solution_id:string -> quality:float -> Swarm.swarm option Lwt.t

(** {1 Stigmergy Behavior} *)

val follow_pheromone : config -> ?stigmergy:stigmergy_config -> unit -> string option Lwt.t
val mark_success : config -> agent_id:string -> path_id:string -> ?stigmergy:stigmergy_config -> unit -> Swarm.swarm option Lwt.t

(** {1 Quorum Sensing Behavior} *)

val check_quorum : config -> proposal_id:string ->
  [`No_swarm | `Not_found | `Quorum_reached | `Progress of int * int * float] Lwt.t
val propose_action : config -> agent_id:string -> description:string -> Swarm.quorum_proposal option Lwt.t
val vote_on_proposal : config -> agent_id:string -> proposal_id:string -> support:bool -> Swarm.quorum_proposal option Lwt.t

(** {1 Behavior Execution} *)

val execute_behavior : config -> agent_id:string ->
  [`Error of string
  | `Guidance of string
  | `Follow_path of string
  | `Explore of string
  | `Follow_trails of string list
  | `Deposit of string
  | `Vote_needed of string list
  | `No_action of string] Lwt.t

(** {1 Adaptive Behavior Selection} *)

val recommend_behavior : config -> Swarm.swarm_behavior Lwt.t
