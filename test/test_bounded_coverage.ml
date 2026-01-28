(** Bounded Module Coverage Tests

    Tests for Bounded Autonomy - Constrained multi-agent execution:
    - comparison type
    - default configs
    - calc_backoff_delay
    - is_retryable_error
*)

open Alcotest

module Bounded = Masc_mcp.Bounded

(* ============================================================
   Default Config Tests
   ============================================================ *)

let test_default_retry_config_max () =
  check int "max_retries" 3 Bounded.default_retry_config.max_retries

let test_default_retry_config_base_delay () =
  check int "base_delay_ms" 1000 Bounded.default_retry_config.base_delay_ms

let test_default_retry_config_max_delay () =
  check int "max_delay_ms" 30000 Bounded.default_retry_config.max_delay_ms

let test_default_retry_config_jitter () =
  check (float 0.01) "jitter_factor" 0.2 Bounded.default_retry_config.jitter_factor

let test_default_constraints_max_turns () =
  match Bounded.default_constraints.max_turns with
  | Some n -> check int "max_turns" 10 n
  | None -> fail "expected Some"

let test_default_constraints_max_tokens () =
  match Bounded.default_constraints.max_tokens with
  | Some n -> check int "max_tokens" 100000 n
  | None -> fail "expected Some"

let test_default_constraints_max_cost () =
  match Bounded.default_constraints.max_cost_usd with
  | Some f -> check (float 0.01) "max_cost" 1.0 f
  | None -> fail "expected Some"

let test_default_constraints_max_time () =
  match Bounded.default_constraints.max_time_seconds with
  | Some f -> check (float 0.01) "max_time" 300.0 f
  | None -> fail "expected Some"

let test_default_constraints_buffer () =
  check int "token_buffer" 5000 Bounded.default_constraints.token_buffer

let test_default_constraints_hard_max () =
  check int "hard_max_iterations" 100 Bounded.default_constraints.hard_max_iterations

(* ============================================================
   calc_backoff_delay Tests
   ============================================================ *)

let test_calc_backoff_attempt_0 () =
  let config = { Bounded.default_retry_config with jitter_factor = 0.0 } in
  let delay = Bounded.calc_backoff_delay config 0 in
  check int "attempt 0" 1000 delay

let test_calc_backoff_attempt_1 () =
  let config = { Bounded.default_retry_config with jitter_factor = 0.0 } in
  let delay = Bounded.calc_backoff_delay config 1 in
  check int "attempt 1 (doubled)" 2000 delay

let test_calc_backoff_attempt_2 () =
  let config = { Bounded.default_retry_config with jitter_factor = 0.0 } in
  let delay = Bounded.calc_backoff_delay config 2 in
  check int "attempt 2 (4x)" 4000 delay

let test_calc_backoff_capped () =
  let config = { Bounded.default_retry_config with
    jitter_factor = 0.0;
    max_delay_ms = 5000;
  } in
  let delay = Bounded.calc_backoff_delay config 10 in
  check bool "capped at max" true (delay <= 5000)

let test_calc_backoff_with_jitter () =
  (* With jitter, delay varies but should be reasonable *)
  let config = Bounded.default_retry_config in
  let delay = Bounded.calc_backoff_delay config 0 in
  check bool "reasonable delay" true (delay >= 800 && delay <= 1200)

(* ============================================================
   is_retryable_error Tests
   ============================================================ *)

let test_retryable_timeout () =
  check bool "timeout" true (Bounded.is_retryable_error "Request timeout")

let test_retryable_timed_out () =
  check bool "timed out" true (Bounded.is_retryable_error "Connection timed out")

let test_retryable_connection_refused () =
  check bool "conn refused" true (Bounded.is_retryable_error "connection refused")

let test_retryable_connection_reset () =
  check bool "conn reset" true (Bounded.is_retryable_error "connection reset by peer")

let test_retryable_network () =
  check bool "network" true (Bounded.is_retryable_error "Network error occurred")

let test_retryable_econnrefused () =
  check bool "ECONNREFUSED" true (Bounded.is_retryable_error "ECONNREFUSED")

let test_retryable_etimedout () =
  check bool "ETIMEDOUT" true (Bounded.is_retryable_error "ETIMEDOUT")

let test_retryable_rate_limit () =
  check bool "rate limit" true (Bounded.is_retryable_error "rate limit exceeded")

let test_retryable_429 () =
  check bool "429" true (Bounded.is_retryable_error "HTTP 429 Too Many Requests")

let test_retryable_502 () =
  check bool "502" true (Bounded.is_retryable_error "Error 502 Bad Gateway")

let test_retryable_503 () =
  check bool "503" true (Bounded.is_retryable_error "503 Service Unavailable")

let test_retryable_504 () =
  check bool "504" true (Bounded.is_retryable_error "504 Gateway Timeout")

let test_not_retryable_auth () =
  check bool "auth error" false (Bounded.is_retryable_error "Authentication failed")

let test_not_retryable_not_found () =
  check bool "not found" false (Bounded.is_retryable_error "Resource not found")

let test_not_retryable_validation () =
  check bool "validation" false (Bounded.is_retryable_error "Validation error: invalid input")

let test_retryable_case_insensitive () =
  check bool "TIMEOUT uppercase" true (Bounded.is_retryable_error "TIMEOUT ERROR")

(* ============================================================
   comparison Type Tests
   ============================================================ *)

let test_comparison_eq () =
  let c = Bounded.Eq (`String "test") in
  match c with
  | Bounded.Eq _ -> check bool "eq" true true
  | _ -> fail "expected Eq"

let test_comparison_neq () =
  let c = Bounded.Neq (`Int 42) in
  match c with
  | Bounded.Neq _ -> check bool "neq" true true
  | _ -> fail "expected Neq"

let test_comparison_lt () =
  let c = Bounded.Lt 10.0 in
  match c with
  | Bounded.Lt f -> check (float 0.01) "lt" 10.0 f
  | _ -> fail "expected Lt"

let test_comparison_lte () =
  let c = Bounded.Lte 20.0 in
  match c with
  | Bounded.Lte f -> check (float 0.01) "lte" 20.0 f
  | _ -> fail "expected Lte"

let test_comparison_gt () =
  let c = Bounded.Gt 5.0 in
  match c with
  | Bounded.Gt f -> check (float 0.01) "gt" 5.0 f
  | _ -> fail "expected Gt"

let test_comparison_gte () =
  let c = Bounded.Gte 15.0 in
  match c with
  | Bounded.Gte f -> check (float 0.01) "gte" 15.0 f
  | _ -> fail "expected Gte"

let test_comparison_between () =
  let c = Bounded.Between (1.0, 10.0) in
  match c with
  | Bounded.Between (lo, hi) ->
    check (float 0.01) "lo" 1.0 lo;
    check (float 0.01) "hi" 10.0 hi
  | _ -> fail "expected Between"

let test_comparison_in () =
  let c = Bounded.In [`String "a"; `String "b"] in
  match c with
  | Bounded.In lst -> check int "in list" 2 (List.length lst)
  | _ -> fail "expected In"

(* ============================================================
   goal Type Tests
   ============================================================ *)

let test_goal_type () =
  let g : Bounded.goal = {
    path = "$.result.status";
    condition = Bounded.Eq (`String "success");
  } in
  check string "path" "$.result.status" g.path

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Bounded Coverage" [
    "default_retry_config", [
      test_case "max_retries" `Quick test_default_retry_config_max;
      test_case "base_delay" `Quick test_default_retry_config_base_delay;
      test_case "max_delay" `Quick test_default_retry_config_max_delay;
      test_case "jitter" `Quick test_default_retry_config_jitter;
    ];
    "default_constraints", [
      test_case "max_turns" `Quick test_default_constraints_max_turns;
      test_case "max_tokens" `Quick test_default_constraints_max_tokens;
      test_case "max_cost" `Quick test_default_constraints_max_cost;
      test_case "max_time" `Quick test_default_constraints_max_time;
      test_case "buffer" `Quick test_default_constraints_buffer;
      test_case "hard_max" `Quick test_default_constraints_hard_max;
    ];
    "calc_backoff_delay", [
      test_case "attempt 0" `Quick test_calc_backoff_attempt_0;
      test_case "attempt 1" `Quick test_calc_backoff_attempt_1;
      test_case "attempt 2" `Quick test_calc_backoff_attempt_2;
      test_case "capped" `Quick test_calc_backoff_capped;
      test_case "with jitter" `Quick test_calc_backoff_with_jitter;
    ];
    "is_retryable_error", [
      test_case "timeout" `Quick test_retryable_timeout;
      test_case "timed out" `Quick test_retryable_timed_out;
      test_case "conn refused" `Quick test_retryable_connection_refused;
      test_case "conn reset" `Quick test_retryable_connection_reset;
      test_case "network" `Quick test_retryable_network;
      test_case "ECONNREFUSED" `Quick test_retryable_econnrefused;
      test_case "ETIMEDOUT" `Quick test_retryable_etimedout;
      test_case "rate limit" `Quick test_retryable_rate_limit;
      test_case "429" `Quick test_retryable_429;
      test_case "502" `Quick test_retryable_502;
      test_case "503" `Quick test_retryable_503;
      test_case "504" `Quick test_retryable_504;
      test_case "not auth" `Quick test_not_retryable_auth;
      test_case "not not_found" `Quick test_not_retryable_not_found;
      test_case "not validation" `Quick test_not_retryable_validation;
      test_case "case insensitive" `Quick test_retryable_case_insensitive;
    ];
    "comparison", [
      test_case "eq" `Quick test_comparison_eq;
      test_case "neq" `Quick test_comparison_neq;
      test_case "lt" `Quick test_comparison_lt;
      test_case "lte" `Quick test_comparison_lte;
      test_case "gt" `Quick test_comparison_gt;
      test_case "gte" `Quick test_comparison_gte;
      test_case "between" `Quick test_comparison_between;
      test_case "in" `Quick test_comparison_in;
    ];
    "goal", [
      test_case "type" `Quick test_goal_type;
    ];
  ]
