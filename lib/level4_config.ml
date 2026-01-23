(** MASC Level 4 Configuration - Externalized Magic Numbers

    All configurable parameters for Swarm behavior.
    Environment variables override defaults.

    Based on extreme reviewer feedback:
    - Rich Hickey: "Magic numbers are complected configuration"
    - John Carmack: "Hardcoded values prevent tuning"

    @author Second Brain
    @since MASC v3.1 (Level 4)
*)

(** {1 Environment Helpers} *)

let get_env_float name default =
  match Sys.getenv_opt name with
  | Some s -> (try float_of_string s with _ -> default)
  | None -> default

let get_env_int name default =
  match Sys.getenv_opt name with
  | Some s -> (try int_of_string s with _ -> default)
  | None -> default

(** {1 Random Number Generator} *)

(** Initialize RNG with seed for reproducibility *)
let rng_initialized = ref false

let ensure_rng_init () =
  if not !rng_initialized then begin
    let seed = get_env_int "MASC_SWARM_SEED" (int_of_float (Unix.gettimeofday () *. 1000.0)) in
    Random.init seed;
    rng_initialized := true
  end

(** Get random float with guaranteed initialization *)
let random_float max =
  ensure_rng_init ();
  Random.float max

(** Get random int with guaranteed initialization *)
let random_int max =
  ensure_rng_init ();
  Random.int max

(** {1 Swarm Configuration} *)

module Swarm = struct
  (** Initial fitness for new agents (0.0-1.0) *)
  let initial_fitness () = get_env_float "MASC_SWARM_INITIAL_FITNESS" 0.5

  (** Default selection pressure (0.0-1.0) *)
  let selection_pressure () = get_env_float "MASC_SWARM_SELECTION_PRESSURE" 0.3

  (** Default mutation rate (0.0-1.0) *)
  let mutation_rate () = get_env_float "MASC_SWARM_MUTATION_RATE" 0.1

  (** Default pheromone evaporation rate per hour *)
  let evaporation_rate () = get_env_float "MASC_SWARM_EVAPORATION_RATE" 0.1

  (** Default quorum threshold (0.0-1.0) *)
  let quorum_threshold () = get_env_float "MASC_SWARM_QUORUM_THRESHOLD" 0.6

  (** Maximum agents per swarm *)
  let max_agents () = get_env_int "MASC_SWARM_MAX_AGENTS" 50

  (** Minimum agents for foraging behavior switch *)
  let min_agents_for_flocking () = get_env_int "MASC_SWARM_MIN_AGENTS_FLOCKING" 5
end

(** {1 Flocking Configuration} *)

module Flocking = struct
  (** Separation force weight *)
  let separation_weight () = get_env_float "MASC_FLOCK_SEPARATION" 1.5

  (** Alignment force weight *)
  let alignment_weight () = get_env_float "MASC_FLOCK_ALIGNMENT" 1.0

  (** Cohesion force weight *)
  let cohesion_weight () = get_env_float "MASC_FLOCK_COHESION" 1.0

  (** Perception radius for neighbor detection *)
  let perception_radius () = get_env_float "MASC_FLOCK_PERCEPTION_RADIUS" 100.0

  (** Fitness cohesion multiplier *)
  let fitness_cohesion_multiplier () = get_env_float "MASC_FLOCK_FITNESS_MULT" 2.0
end

(** {1 Foraging Configuration} *)

module Foraging = struct
  (** Default exploration rate (epsilon for epsilon-greedy) *)
  let exploration_rate () = get_env_float "MASC_FORAGE_EXPLORATION" 0.3
end

(** {1 Stigmergy Configuration} *)

module Stigmergy = struct
  (** Pheromone deposit rate per success *)
  let deposit_rate () = get_env_float "MASC_STIG_DEPOSIT" 0.2

  (** Minimum pheromone strength to follow *)
  let following_threshold () = get_env_float "MASC_STIG_THRESHOLD" 0.1

  (** Number of top trails to consider *)
  let top_trails_limit () = get_env_int "MASC_STIG_TOP_TRAILS" 5
end

(** {1 Validation} *)

(** Check if float is finite (not NaN or Inf) *)
let is_finite f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan -> false

(** {1 Abstract Fitness Type}

    Parse Don't Validate: Invalid fitness values cannot exist.
    - Bartosz Milewski: "Make invalid states unrepresentable"
    - Alexis King: "Parse don't validate"
*)

module Fitness : sig
  (** Abstract type - guaranteed to be in [0.0, 1.0] *)
  type t

  (** Create fitness from float. Returns None if invalid. *)
  val of_float : float -> t option

  (** Create fitness from float, clamping to valid range. *)
  val of_float_clamped : float -> t

  (** Extract the underlying float value. *)
  val to_float : t -> float

  (** Initial fitness for new agents *)
  val initial : unit -> t

  (** Combine two fitness values (average) *)
  val combine : t -> t -> t

  (** Adjust fitness by delta, clamping result *)
  val adjust : t -> delta:float -> t

  (** Compare fitness values *)
  val compare : t -> t -> int

  (** JSON serialization *)
  val to_json : t -> Yojson.Safe.t
  val of_json : Yojson.Safe.t -> t option
end = struct
  type t = float

  let of_float f =
    if not (is_finite f) || f < 0.0 || f > 1.0 then None
    else Some f

  let of_float_clamped f =
    if not (is_finite f) then 0.5  (* Default on invalid *)
    else max 0.0 (min 1.0 f)

  let to_float t = t

  let initial () =
    Swarm.initial_fitness ()

  let combine a b = (a +. b) /. 2.0

  let adjust t ~delta =
    of_float_clamped (t +. delta)

  let compare = Float.compare

  let to_json t = `Float t

  let of_json = function
    | `Float f -> of_float f
    | `Int i -> of_float (float_of_int i)
    | _ -> None
end

(** Clamp float to valid range *)
let clamp_float ~min ~max f =
  if f < min then min
  else if f > max then max
  else f

(** Validate fitness value (0.0-1.0) *)
let validate_fitness f =
  if not (is_finite f) then None
  else Some (clamp_float ~min:0.0 ~max:1.0 f)

(** Validate non-negative float *)
let validate_positive f =
  if not (is_finite f) || f < 0.0 then None
  else Some f
