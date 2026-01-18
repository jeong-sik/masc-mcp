(** Tests for Retry module *)

module Retry = Masc_mcp.Retry
open Alcotest

(* Test: calculate delay with exponential backoff *)
let test_exponential_backoff () =
  let policy = {
    Retry.max_attempts = 5;
    initial_delay_ms = 100;
    max_delay_ms = 10000;
    backoff_multiplier = 2.0;
    jitter = false;
  } in

  let d1 = Retry.calculate_delay policy 1 in
  check int "attempt 1" 100 d1;

  let d2 = Retry.calculate_delay policy 2 in
  check int "attempt 2" 200 d2;

  let d3 = Retry.calculate_delay policy 3 in
  check int "attempt 3" 400 d3;

  let d4 = Retry.calculate_delay policy 4 in
  check int "attempt 4" 800 d4

(* Test: delay capped at max *)
let test_delay_capped () =
  let policy = {
    Retry.max_attempts = 10;
    initial_delay_ms = 1000;
    max_delay_ms = 5000;
    backoff_multiplier = 3.0;
    jitter = false;
  } in

  let d = Retry.calculate_delay policy 5 in
  check int "capped at max" 5000 d

(* Test: jitter adds randomness *)
let test_jitter () =
  let policy = {
    Retry.max_attempts = 3;
    initial_delay_ms = 1000;
    max_delay_ms = 10000;
    backoff_multiplier = 2.0;
    jitter = true;
  } in

  let delays = List.init 10 (fun _ -> Retry.calculate_delay policy 1) in
  let unique_count = List.length (List.sort_uniq compare delays) in
  check bool "jitter produces variation" true (unique_count > 1)

(* Test: circuit breaker starts closed *)
let test_circuit_starts_closed () =
  let cb = Retry.create_circuit_breaker ~name:"test" () in
  check bool "initially allows" true (Retry.circuit_allows cb)

(* Test: circuit opens after failures *)
let test_circuit_opens () =
  let cb = Retry.create_circuit_breaker ~name:"test" ~failure_threshold:3 () in

  Retry.circuit_record_failure cb;
  check bool "1 failure: still allows" true (Retry.circuit_allows cb);

  Retry.circuit_record_failure cb;
  check bool "2 failures: still allows" true (Retry.circuit_allows cb);

  Retry.circuit_record_failure cb;
  check bool "3 failures: opens" false (Retry.circuit_allows cb)

(* Test: circuit resets on success *)
let test_circuit_resets_on_success () =
  let cb = Retry.create_circuit_breaker ~name:"test" ~failure_threshold:3 () in

  Retry.circuit_record_failure cb;
  Retry.circuit_record_failure cb;
  Retry.circuit_record_success cb;

  Retry.circuit_record_failure cb;
  Retry.circuit_record_failure cb;
  check bool "reset: 2 new failures still allows" true (Retry.circuit_allows cb)

(* Test: manual circuit reset *)
let test_circuit_manual_reset () =
  let cb = Retry.create_circuit_breaker ~name:"test" ~failure_threshold:2 () in

  Retry.circuit_record_failure cb;
  Retry.circuit_record_failure cb;
  check bool "circuit open" false (Retry.circuit_allows cb);

  Retry.reset_circuit_breaker cb;
  check bool "after reset: allows" true (Retry.circuit_allows cb)

(* Test: circuit status JSON *)
let test_circuit_status_json () =
  let cb = Retry.create_circuit_breaker ~name:"test_cb" ~failure_threshold:5 () in
  Retry.circuit_record_failure cb;

  let json = Retry.circuit_status cb in
  let open Yojson.Safe.Util in

  check string "name" "test_cb" (json |> member "name" |> to_string);
  check string "state" "closed" (json |> member "state" |> to_string);
  check int "failure_count" 1 (json |> member "failure_count" |> to_int)

(* Test: idempotency key generation *)
let test_idempotency_key () =
  let k1 = Retry.generate_idempotency_key () in
  let k2 = Retry.generate_idempotency_key () in
  check bool "keys are unique" true (k1 <> k2)

(* Test: idempotency check *)
let test_idempotency_check () =
  let key = "test-op-" ^ string_of_int (Random.int 1000000) in

  (match Retry.check_idempotency ~key ~ttl_seconds:60.0 with
  | `NotSeen -> ()
  | _ -> fail "should be NotSeen");

  Retry.record_idempotency ~key;

  (match Retry.check_idempotency ~key ~ttl_seconds:60.0 with
  | `AlreadyExecuted -> ()
  | _ -> fail "should be AlreadyExecuted")

(* Test: idempotency cleanup *)
let test_idempotency_cleanup () =
  let key1 = "cleanup-test-" ^ string_of_int (Random.int 1000000) in
  let key2 = "cleanup-test-" ^ string_of_int (Random.int 1000000) in
  Retry.record_idempotency ~key:key1;
  Retry.record_idempotency ~key:key2;

  Unix.sleepf 0.02;
  let removed = Retry.cleanup_idempotency ~max_age_seconds:0.01 in

  check bool "removed some keys" true (removed >= 2)

(* Test: global stats *)
let test_global_stats () =
  Retry.reset_stats ();

  let stats = Retry.get_stats () in
  let open Yojson.Safe.Util in

  check int "initial total_attempts" 0 (stats |> member "total_attempts" |> to_int);
  check bool "has circuit_breakers" true
    (stats |> member "circuit_breakers" |> to_assoc |> List.length > 0)

(* Test: default policy values *)
let test_default_policy () =
  check int "default max_attempts" 3 Retry.default_policy.max_attempts;
  check int "default initial_delay" 100 Retry.default_policy.initial_delay_ms;
  check bool "default jitter" true Retry.default_policy.jitter

(* Test: sync retry success *)
let test_sync_retry_success () =
  Retry.reset_stats ();
  let counter = ref 0 in

  let result = Retry.with_retry_sync ~op_name:"test" (fun () ->
    incr counter;
    "success"
  ) in

  (match result with
  | Retry.Success s -> check string "result" "success" s
  | _ -> fail "should succeed");

  check int "only 1 attempt" 1 !counter

(* Test: sync retry eventual success *)
let test_sync_retry_eventual_success () =
  Retry.reset_stats ();
  let counter = ref 0 in

  let policy = { Retry.default_policy with
    max_attempts = 3;
    initial_delay_ms = 1;
    jitter = false;
  } in

  let result = Retry.with_retry_sync ~policy ~op_name:"test" (fun () ->
    incr counter;
    if !counter < 3 then failwith "not yet"
    else "finally"
  ) in

  (match result with
  | Retry.Success s -> check string "result" "finally" s
  | _ -> fail "should succeed");

  check int "3 attempts" 3 !counter

(* Test: sync retry exhausted *)
let test_sync_retry_exhausted () =
  Retry.reset_stats ();

  let policy = { Retry.default_policy with
    max_attempts = 2;
    initial_delay_ms = 1;
  } in

  let result = Retry.with_retry_sync ~policy ~op_name:"test" (fun () ->
    failwith "always fails"
  ) in

  (match result with
  | Retry.Exhausted { attempts; _ } -> check int "attempts" 2 attempts
  | _ -> fail "should be exhausted")

(* Test: scheduler status when not configured *)
let test_scheduler_status_unconfigured () =
  Retry.stop_scheduler ();
  let status = Retry.scheduler_status () in
  let open Yojson.Safe.Util in
  check bool "not running" false (status |> member "running" |> to_bool)

(* Test: scheduler start and status *)
let test_scheduler_start () =
  Retry.start_scheduler ~interval_seconds:0.1 ~max_age_seconds:1.0 ();

  let status = Retry.scheduler_status () in
  let open Yojson.Safe.Util in
  check bool "is running" true (status |> member "running" |> to_bool);
  check bool "is configured" true (status |> member "configured" |> to_bool);

  Retry.stop_scheduler ()

(* Test: scheduler stop *)
let test_scheduler_stop () =
  Retry.start_scheduler ~interval_seconds:0.1 ~max_age_seconds:1.0 ();
  Retry.stop_scheduler ();

  let status = Retry.scheduler_status () in
  let open Yojson.Safe.Util in
  check bool "stopped" false (status |> member "running" |> to_bool)

(* All tests *)
let () =
  run "Retry" [
    "backoff", [
      test_case "exponential backoff" `Quick test_exponential_backoff;
      test_case "delay capped" `Quick test_delay_capped;
      test_case "jitter variation" `Quick test_jitter;
    ];
    "circuit_breaker", [
      test_case "starts closed" `Quick test_circuit_starts_closed;
      test_case "opens after failures" `Quick test_circuit_opens;
      test_case "resets on success" `Quick test_circuit_resets_on_success;
      test_case "manual reset" `Quick test_circuit_manual_reset;
      test_case "status JSON" `Quick test_circuit_status_json;
    ];
    "idempotency", [
      test_case "key generation" `Quick test_idempotency_key;
      test_case "check" `Quick test_idempotency_check;
      test_case "cleanup" `Quick test_idempotency_cleanup;
    ];
    "stats", [
      test_case "global stats" `Quick test_global_stats;
      test_case "default policy" `Quick test_default_policy;
    ];
    "retry_sync", [
      test_case "success" `Quick test_sync_retry_success;
      test_case "eventual success" `Quick test_sync_retry_eventual_success;
      test_case "exhausted" `Quick test_sync_retry_exhausted;
    ];
    "scheduler", [
      test_case "status unconfigured" `Quick test_scheduler_status_unconfigured;
      test_case "start" `Quick test_scheduler_start;
      test_case "stop" `Quick test_scheduler_stop;
    ];
  ]
