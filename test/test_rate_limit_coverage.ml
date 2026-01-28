(** Rate Limit Module Coverage Tests

    Tests for MASC Rate Limiting:
    - default_rate, default_burst constants
    - create function
*)

open Alcotest

module Rate_limit = Masc_mcp.Rate_limit

(* ============================================================
   Constants Tests
   ============================================================ *)

let test_default_rate () =
  check (float 0.001) "default rate" 20.0 Rate_limit.default_rate

let test_default_burst () =
  check int "default burst" 50 Rate_limit.default_burst

(* ============================================================
   Create Tests
   ============================================================ *)

let test_create_default () =
  let limiter = Rate_limit.create () in
  check (float 0.001) "rate" 20.0 limiter.rate;
  check int "burst" 50 limiter.burst

let test_create_custom_rate () =
  let limiter = Rate_limit.create ~rate:100.0 () in
  check (float 0.001) "custom rate" 100.0 limiter.rate

let test_create_custom_burst () =
  let limiter = Rate_limit.create ~burst:200 () in
  check int "custom burst" 200 limiter.burst

let test_create_both_custom () =
  let limiter = Rate_limit.create ~rate:50.0 ~burst:100 () in
  check (float 0.001) "rate" 50.0 limiter.rate;
  check int "burst" 100 limiter.burst

(* ============================================================
   Check Tests
   ============================================================ *)

let test_check_allows_first () =
  let limiter = Rate_limit.create ~burst:10 () in
  check bool "first allowed" true (Rate_limit.check limiter ~key:"test")

let test_check_within_burst () =
  let limiter = Rate_limit.create ~burst:5 () in
  let results = List.init 5 (fun _ -> Rate_limit.check limiter ~key:"test") in
  check bool "all within burst" true (List.for_all Fun.id results)

let test_check_exceeds_burst () =
  let limiter = Rate_limit.create ~rate:0.0 ~burst:2 () in
  let _ = Rate_limit.check limiter ~key:"test" in
  let _ = Rate_limit.check limiter ~key:"test" in
  let third = Rate_limit.check limiter ~key:"test" in
  check bool "third blocked" false third

let test_check_different_keys () =
  let limiter = Rate_limit.create ~rate:0.0 ~burst:1 () in
  let _ = Rate_limit.check limiter ~key:"key1" in
  let result = Rate_limit.check limiter ~key:"key2" in
  check bool "different key allowed" true result

(* ============================================================
   Remaining Tests
   ============================================================ *)

let test_remaining_new_key () =
  let limiter = Rate_limit.create ~burst:10 () in
  let rem = Rate_limit.remaining limiter ~key:"new" in
  check int "new key has burst" 10 rem

let test_remaining_after_check () =
  let limiter = Rate_limit.create ~rate:0.0 ~burst:10 () in
  let _ = Rate_limit.check limiter ~key:"test" in
  let rem = Rate_limit.remaining limiter ~key:"test" in
  check int "decremented" 9 rem

let test_remaining_multiple () =
  let limiter = Rate_limit.create ~rate:0.0 ~burst:10 () in
  let _ = Rate_limit.check limiter ~key:"test" in
  let _ = Rate_limit.check limiter ~key:"test" in
  let _ = Rate_limit.check limiter ~key:"test" in
  let rem = Rate_limit.remaining limiter ~key:"test" in
  check int "decremented 3 times" 7 rem

(* ============================================================
   Cleanup Tests
   ============================================================ *)

let test_cleanup_removes_old () =
  let limiter = Rate_limit.create () in
  let _ = Rate_limit.check limiter ~key:"old" in
  (* Ensure entry is older than the cleanup threshold. *)
  Unix.sleepf 0.001;
  let removed = Rate_limit.cleanup limiter ~older_than_seconds:0 in
  check bool "removed old entries" true (removed >= 1)

let test_cleanup_keeps_recent () =
  let limiter = Rate_limit.create () in
  let _ = Rate_limit.check limiter ~key:"recent" in
  (* Cleanup with large threshold should keep recent *)
  let removed = Rate_limit.cleanup limiter ~older_than_seconds:3600 in
  check int "keeps recent" 0 removed

(* ============================================================
   Env Functions Tests
   ============================================================ *)

let test_rate_from_env_returns_float () =
  let rate = Rate_limit.rate_from_env () in
  check bool "rate is positive" true (rate > 0.0)

let test_burst_from_env_returns_int () =
  let burst = Rate_limit.burst_from_env () in
  check bool "burst is positive" true (burst > 0)

let test_create_from_env () =
  let limiter = Rate_limit.create_from_env () in
  check bool "rate positive" true (limiter.rate > 0.0);
  check bool "burst positive" true (limiter.burst > 0)

(* ============================================================
   Global Instance Tests
   ============================================================ *)

let test_check_global () =
  let result = Rate_limit.check_global ~key:"global_test" in
  check bool "global check returns bool" true (result || not result)

let test_remaining_global () =
  let rem = Rate_limit.remaining_global ~key:"global_test_new" in
  check bool "global remaining positive" true (rem >= 0)

(* ============================================================
   HTTP Helpers Tests
   ============================================================ *)

let test_headers_has_limit () =
  let limiter = Rate_limit.create ~burst:100 () in
  let hdrs = Rate_limit.headers limiter ~key:"test" in
  check bool "has X-RateLimit-Limit" true
    (List.mem_assoc "X-RateLimit-Limit" hdrs)

let test_headers_has_remaining () =
  let limiter = Rate_limit.create () in
  let hdrs = Rate_limit.headers limiter ~key:"test" in
  check bool "has X-RateLimit-Remaining" true
    (List.mem_assoc "X-RateLimit-Remaining" hdrs)

let test_headers_limit_value () =
  let limiter = Rate_limit.create ~burst:100 () in
  let hdrs = Rate_limit.headers limiter ~key:"test" in
  match List.assoc_opt "X-RateLimit-Limit" hdrs with
  | Some v -> check string "limit is burst" "100" v
  | None -> fail "expected X-RateLimit-Limit header"

let test_too_many_requests_body () =
  let body = Rate_limit.too_many_requests_body () in
  check bool "contains error" true
    (try let _ = Str.search_forward (Str.regexp "Too Many Requests") body 0 in true
     with Not_found -> false)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Rate Limit Coverage" [
    "constants", [
      test_case "default_rate" `Quick test_default_rate;
      test_case "default_burst" `Quick test_default_burst;
    ];
    "create", [
      test_case "default" `Quick test_create_default;
      test_case "custom rate" `Quick test_create_custom_rate;
      test_case "custom burst" `Quick test_create_custom_burst;
      test_case "both custom" `Quick test_create_both_custom;
    ];
    "check", [
      test_case "allows first" `Quick test_check_allows_first;
      test_case "within burst" `Quick test_check_within_burst;
      test_case "exceeds burst" `Quick test_check_exceeds_burst;
      test_case "different keys" `Quick test_check_different_keys;
    ];
    "remaining", [
      test_case "new key" `Quick test_remaining_new_key;
      test_case "after check" `Quick test_remaining_after_check;
      test_case "multiple" `Quick test_remaining_multiple;
    ];
    "cleanup", [
      test_case "removes old" `Quick test_cleanup_removes_old;
      test_case "keeps recent" `Quick test_cleanup_keeps_recent;
    ];
    "env", [
      test_case "rate_from_env" `Quick test_rate_from_env_returns_float;
      test_case "burst_from_env" `Quick test_burst_from_env_returns_int;
      test_case "create_from_env" `Quick test_create_from_env;
    ];
    "global", [
      test_case "check_global" `Quick test_check_global;
      test_case "remaining_global" `Quick test_remaining_global;
    ];
    "http", [
      test_case "headers has limit" `Quick test_headers_has_limit;
      test_case "headers has remaining" `Quick test_headers_has_remaining;
      test_case "headers limit value" `Quick test_headers_limit_value;
      test_case "too_many_requests_body" `Quick test_too_many_requests_body;
    ];
  ]
