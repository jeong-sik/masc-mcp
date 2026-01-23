(** MASC Level 2 Configuration - Externalized Constants

    MAGI Recommendation: Externalize hardcoded constants for runtime tuning.

    Environment variables:
    - MASC_METRICS_CACHE_TTL: Metrics cache TTL in seconds (default: 300)
    - MASC_TOKEN_CACHE_SIZE: Max token cache entries (default: 1000)
    - MASC_DRIFT_THRESHOLD: Drift detection threshold (default: 0.85)
    - MASC_LOCK_WARN_MS: Lock contention warning threshold in ms (default: 100)
    - MASC_HEBBIAN_RATE: Symmetric Hebbian learning rate (default: 0.075)
*)

(** Get environment variable with default *)
let get_env_float name default =
  match Sys.getenv_opt name with
  | Some s -> (try float_of_string s with _ -> default)
  | None -> default

let get_env_int name default =
  match Sys.getenv_opt name with
  | Some s -> (try int_of_string s with _ -> default)
  | None -> default

(** Metrics cache configuration *)
module Metrics_cache = struct
  let ttl_seconds () = get_env_float "MASC_METRICS_CACHE_TTL" 300.0
end

(** Token cache configuration *)
module Token_cache = struct
  let max_size () = get_env_int "MASC_TOKEN_CACHE_SIZE" 1000
end

(** Drift guard configuration *)
module Drift_guard = struct
  let default_threshold () = get_env_float "MASC_DRIFT_THRESHOLD" 0.85

  (** Similarity weights for Jaccard/Cosine combination *)
  type weights = { jaccard: float; cosine: float }
  let weights () = {
    jaccard = get_env_float "MASC_DRIFT_JACCARD_WEIGHT" 0.4;
    cosine = get_env_float "MASC_DRIFT_COSINE_WEIGHT" 0.6;
  }
end

(** Lock configuration *)
module Lock = struct
  let warn_threshold_ms () = get_env_float "MASC_LOCK_WARN_MS" 100.0
end

(** Hebbian learning configuration *)
module Hebbian = struct
  let learning_rate () = get_env_float "MASC_HEBBIAN_RATE" 0.075
  let decay_rate () = get_env_float "MASC_HEBBIAN_DECAY" 0.01
  let min_weight () = get_env_float "MASC_HEBBIAN_MIN_WEIGHT" 0.05
  let max_weight () = get_env_float "MASC_HEBBIAN_MAX_WEIGHT" 1.0
end

(** Fitness configuration *)
module Fitness = struct
  let recency_halflife_days () = get_env_float "MASC_FITNESS_HALFLIFE" 7.0
end

(** Get all config as JSON for debugging *)
let to_json () : Yojson.Safe.t =
  `Assoc [
    ("metrics_cache_ttl", `Float (Metrics_cache.ttl_seconds ()));
    ("token_cache_max_size", `Int (Token_cache.max_size ()));
    ("drift_threshold", `Float (Drift_guard.default_threshold ()));
    ("lock_warn_ms", `Float (Lock.warn_threshold_ms ()));
    ("hebbian_rate", `Float (Hebbian.learning_rate ()));
    ("hebbian_decay", `Float (Hebbian.decay_rate ()));
    ("fitness_halflife_days", `Float (Fitness.recency_halflife_days ()));
  ]

(** Print config to stderr for debugging *)
let print_config () =
  Printf.eprintf "[level2_config] Configuration:\n";
  Printf.eprintf "  MASC_METRICS_CACHE_TTL=%.0f\n" (Metrics_cache.ttl_seconds ());
  Printf.eprintf "  MASC_TOKEN_CACHE_SIZE=%d\n" (Token_cache.max_size ());
  Printf.eprintf "  MASC_DRIFT_THRESHOLD=%.2f\n" (Drift_guard.default_threshold ());
  Printf.eprintf "  MASC_LOCK_WARN_MS=%.0f\n" (Lock.warn_threshold_ms ());
  Printf.eprintf "  MASC_HEBBIAN_RATE=%.3f\n" (Hebbian.learning_rate ())
