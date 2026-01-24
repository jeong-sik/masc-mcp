(** MASC Environment Configuration

    Centralized environment variable management following 12-Factor App principles.
    All env vars use MASC_* prefix for consistency.

    Usage:
      let threshold = Env_config.Zombie.threshold_seconds
      let lock_timeout = Env_config.Lock.timeout_seconds
*)

(** Safe getters with defaults *)
let get_string ~default name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None -> default

let get_int ~default name =
  match Sys.getenv_opt name with
  | Some v -> (try int_of_string v with _ -> default)
  | None -> default

let get_float ~default name =
  match Sys.getenv_opt name with
  | Some v -> (try float_of_string v with _ -> default)
  | None -> default

let get_bool ~default name =
  match Sys.getenv_opt name with
  | Some v ->
      (match String.lowercase_ascii v with
       | "true" | "1" | "yes" -> true
       | "false" | "0" | "no" -> false
       | _ -> default)
  | None -> default

(** {1 Zombie Detection / Cleanup Configuration} *)

module Zombie = struct
  (** Threshold for considering a resource as zombie (seconds) *)
  let threshold_seconds =
    get_float ~default:300.0 "MASC_ZOMBIE_THRESHOLD_SEC"

  (** Cleanup loop interval (seconds) *)
  let cleanup_interval_seconds =
    get_float ~default:60.0 "MASC_ZOMBIE_CLEANUP_INTERVAL_SEC"
end

(** {1 Lock Configuration} *)

module Lock = struct
  (** Default lock timeout (seconds) *)
  let timeout_seconds =
    get_float ~default:1800.0 "MASC_LOCK_TIMEOUT_SEC"

  (** Lock expiry warning threshold (seconds before expiry) *)
  let expiry_warning_seconds =
    get_float ~default:300.0 "MASC_LOCK_EXPIRY_WARNING_SEC"
end

(** {1 Session Configuration} *)

module Session = struct
  (** Maximum session age before cleanup (seconds) *)
  let max_age_seconds =
    get_float ~default:3600.0 "MASC_SESSION_MAX_AGE_SEC"

  (** Rate limit window (seconds) *)
  let rate_limit_window_seconds =
    get_float ~default:60.0 "MASC_SESSION_RATE_LIMIT_WINDOW_SEC"
end

(** {1 Tempo (Polling Interval) Configuration} *)

module Tempo = struct
  (** Minimum polling interval (seconds) - for urgent tempo *)
  let min_interval_seconds =
    get_float ~default:60.0 "MASC_TEMPO_MIN_INTERVAL_SEC"

  (** Maximum polling interval (seconds) - for idle tempo *)
  let max_interval_seconds =
    get_float ~default:600.0 "MASC_TEMPO_MAX_INTERVAL_SEC"

  (** Default polling interval (seconds) *)
  let default_interval_seconds =
    get_float ~default:300.0 "MASC_TEMPO_DEFAULT_INTERVAL_SEC"
end

(** {1 Orchestrator Configuration} *)

module Orchestrator = struct
  (** Orchestrator check interval (seconds) *)
  let check_interval_seconds =
    get_float ~default:300.0 "MASC_ORCHESTRATOR_INTERVAL"

  (** Orchestrator agent name *)
  let agent_name =
    get_string ~default:"orchestrator" "MASC_ORCHESTRATOR_AGENT"
end

(** {1 Mitosis (Cell Division) Configuration} *)

module Mitosis = struct
  (** Time-based trigger interval (seconds) *)
  let trigger_interval_seconds =
    get_float ~default:300.0 "MASC_MITOSIS_INTERVAL_SEC"
end

(** {1 Federation Configuration} *)

module Federation = struct
  (** Cross-cluster request timeout (seconds) *)
  let timeout_seconds =
    get_float ~default:3600.0 "MASC_FEDERATION_TIMEOUT_SEC"
end

(** {1 Cancellation Token Configuration} *)

module Cancellation = struct
  (** Token cleanup max age (seconds) *)
  let token_max_age_seconds =
    get_float ~default:3600.0 "MASC_CANCELLATION_TOKEN_MAX_AGE_SEC"
end

(** Print configuration summary for debugging *)
let print_summary () =
  Printf.eprintf "[env_config] Zombie: threshold=%.0fs cleanup_interval=%.0fs\n%!"
    Zombie.threshold_seconds Zombie.cleanup_interval_seconds;
  Printf.eprintf "[env_config] Lock: timeout=%.0fs expiry_warning=%.0fs\n%!"
    Lock.timeout_seconds Lock.expiry_warning_seconds;
  Printf.eprintf "[env_config] Session: max_age=%.0fs rate_limit_window=%.0fs\n%!"
    Session.max_age_seconds Session.rate_limit_window_seconds;
  Printf.eprintf "[env_config] Tempo: min=%.0fs max=%.0fs default=%.0fs\n%!"
    Tempo.min_interval_seconds Tempo.max_interval_seconds Tempo.default_interval_seconds
