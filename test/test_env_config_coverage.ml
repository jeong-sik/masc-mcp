(** Env_config Module Coverage Tests

    Tests for MASC Environment Configuration:
    - get_string, get_int, get_float, get_bool: env var readers
    - Zombie, Lock, Session, Tempo, Orchestrator, Mitosis, Federation, Cancellation modules
*)

open Alcotest

module Env_config = Masc_mcp.Env_config

(* ============================================================
   get_string Tests
   ============================================================ *)

let test_get_string_default () =
  let result = Env_config.get_string ~default:"fallback" "NONEXISTENT_VAR_XYZ_12345" in
  check string "default" "fallback" result

let test_get_string_empty_default () =
  let result = Env_config.get_string ~default:"" "NONEXISTENT_VAR_XYZ_12345" in
  check string "empty default" "" result

(* ============================================================
   get_int Tests
   ============================================================ *)

let test_get_int_default () =
  let result = Env_config.get_int ~default:42 "NONEXISTENT_VAR_XYZ_12345" in
  check int "default" 42 result

let test_get_int_negative_default () =
  let result = Env_config.get_int ~default:(-10) "NONEXISTENT_VAR_XYZ_12345" in
  check int "negative default" (-10) result

let test_get_int_zero_default () =
  let result = Env_config.get_int ~default:0 "NONEXISTENT_VAR_XYZ_12345" in
  check int "zero default" 0 result

(* ============================================================
   get_float Tests
   ============================================================ *)

let test_get_float_default () =
  let result = Env_config.get_float ~default:3.14 "NONEXISTENT_VAR_XYZ_12345" in
  check (float 0.001) "default" 3.14 result

let test_get_float_negative_default () =
  let result = Env_config.get_float ~default:(-2.5) "NONEXISTENT_VAR_XYZ_12345" in
  check (float 0.001) "negative default" (-2.5) result

let test_get_float_zero_default () =
  let result = Env_config.get_float ~default:0.0 "NONEXISTENT_VAR_XYZ_12345" in
  check (float 0.001) "zero default" 0.0 result

(* ============================================================
   get_bool Tests
   ============================================================ *)

let test_get_bool_default_true () =
  let result = Env_config.get_bool ~default:true "NONEXISTENT_VAR_XYZ_12345" in
  check bool "default true" true result

let test_get_bool_default_false () =
  let result = Env_config.get_bool ~default:false "NONEXISTENT_VAR_XYZ_12345" in
  check bool "default false" false result

(* ============================================================
   print_summary Tests
   ============================================================ *)

let test_print_summary_no_error () =
  Env_config.print_summary ();
  check bool "no error" true true

(* ============================================================
   Zombie Module Tests
   ============================================================ *)

let test_zombie_threshold_positive () =
  check bool "threshold positive" true (Env_config.Zombie.threshold_seconds > 0.0)

let test_zombie_cleanup_interval_positive () =
  check bool "cleanup interval positive" true (Env_config.Zombie.cleanup_interval_seconds > 0.0)

let test_zombie_threshold_reasonable () =
  (* Default is 300.0 seconds = 5 minutes *)
  check bool "threshold reasonable" true (Env_config.Zombie.threshold_seconds >= 60.0)

(* ============================================================
   Lock Module Tests
   ============================================================ *)

let test_lock_timeout_positive () =
  check bool "timeout positive" true (Env_config.Lock.timeout_seconds > 0.0)

let test_lock_expiry_warning_positive () =
  check bool "expiry warning positive" true (Env_config.Lock.expiry_warning_seconds > 0.0)

let test_lock_warning_less_than_timeout () =
  check bool "warning < timeout" true
    (Env_config.Lock.expiry_warning_seconds < Env_config.Lock.timeout_seconds)

(* ============================================================
   Session Module Tests
   ============================================================ *)

let test_session_max_age_positive () =
  check bool "max age positive" true (Env_config.Session.max_age_seconds > 0.0)

let test_session_rate_limit_window_positive () =
  check bool "rate limit window positive" true (Env_config.Session.rate_limit_window_seconds > 0.0)

(* ============================================================
   Tempo Module Tests
   ============================================================ *)

let test_tempo_min_positive () =
  check bool "min positive" true (Env_config.Tempo.min_interval_seconds > 0.0)

let test_tempo_max_positive () =
  check bool "max positive" true (Env_config.Tempo.max_interval_seconds > 0.0)

let test_tempo_default_positive () =
  check bool "default positive" true (Env_config.Tempo.default_interval_seconds > 0.0)

let test_tempo_min_less_than_max () =
  check bool "min < max" true
    (Env_config.Tempo.min_interval_seconds < Env_config.Tempo.max_interval_seconds)

let test_tempo_default_in_range () =
  check bool "default in range" true
    (Env_config.Tempo.default_interval_seconds >= Env_config.Tempo.min_interval_seconds &&
     Env_config.Tempo.default_interval_seconds <= Env_config.Tempo.max_interval_seconds)

(* ============================================================
   Orchestrator Module Tests
   ============================================================ *)

let test_orchestrator_interval_positive () =
  check bool "interval positive" true (Env_config.Orchestrator.check_interval_seconds > 0.0)

let test_orchestrator_agent_name_nonempty () =
  check bool "agent name nonempty" true (String.length Env_config.Orchestrator.agent_name > 0)

(* ============================================================
   Mitosis Module Tests
   ============================================================ *)

let test_mitosis_interval_positive () =
  check bool "interval positive" true (Env_config.Mitosis.trigger_interval_seconds > 0.0)

(* ============================================================
   Federation Module Tests
   ============================================================ *)

let test_federation_timeout_positive () =
  check bool "timeout positive" true (Env_config.Federation.timeout_seconds > 0.0)

(* ============================================================
   Cancellation Module Tests
   ============================================================ *)

let test_cancellation_token_max_age_positive () =
  check bool "max age positive" true (Env_config.Cancellation.token_max_age_seconds > 0.0)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Env_config Coverage" [
    "get_string", [
      test_case "default" `Quick test_get_string_default;
      test_case "empty default" `Quick test_get_string_empty_default;
    ];
    "get_int", [
      test_case "default" `Quick test_get_int_default;
      test_case "negative default" `Quick test_get_int_negative_default;
      test_case "zero default" `Quick test_get_int_zero_default;
    ];
    "get_float", [
      test_case "default" `Quick test_get_float_default;
      test_case "negative default" `Quick test_get_float_negative_default;
      test_case "zero default" `Quick test_get_float_zero_default;
    ];
    "get_bool", [
      test_case "default true" `Quick test_get_bool_default_true;
      test_case "default false" `Quick test_get_bool_default_false;
    ];
    "print_summary", [
      test_case "no error" `Quick test_print_summary_no_error;
    ];
    "zombie", [
      test_case "threshold positive" `Quick test_zombie_threshold_positive;
      test_case "cleanup interval positive" `Quick test_zombie_cleanup_interval_positive;
      test_case "threshold reasonable" `Quick test_zombie_threshold_reasonable;
    ];
    "lock", [
      test_case "timeout positive" `Quick test_lock_timeout_positive;
      test_case "expiry warning positive" `Quick test_lock_expiry_warning_positive;
      test_case "warning < timeout" `Quick test_lock_warning_less_than_timeout;
    ];
    "session", [
      test_case "max age positive" `Quick test_session_max_age_positive;
      test_case "rate limit window positive" `Quick test_session_rate_limit_window_positive;
    ];
    "tempo", [
      test_case "min positive" `Quick test_tempo_min_positive;
      test_case "max positive" `Quick test_tempo_max_positive;
      test_case "default positive" `Quick test_tempo_default_positive;
      test_case "min < max" `Quick test_tempo_min_less_than_max;
      test_case "default in range" `Quick test_tempo_default_in_range;
    ];
    "orchestrator", [
      test_case "interval positive" `Quick test_orchestrator_interval_positive;
      test_case "agent name nonempty" `Quick test_orchestrator_agent_name_nonempty;
    ];
    "mitosis", [
      test_case "interval positive" `Quick test_mitosis_interval_positive;
    ];
    "federation", [
      test_case "timeout positive" `Quick test_federation_timeout_positive;
    ];
    "cancellation", [
      test_case "max age positive" `Quick test_cancellation_token_max_age_positive;
    ];
  ]
