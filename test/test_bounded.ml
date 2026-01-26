(** Bounded Autonomy Module Tests *)

open Alcotest
module Bounded = Masc_mcp.Bounded

(* ============================================ *)
(* Constraint checking tests                    *)
(* ============================================ *)

let test_default_constraints () =
  let c = Bounded.default_constraints in
  check (option int) "default max_turns" (Some 10) c.max_turns;
  check (option int) "default max_tokens" (Some 100000) c.max_tokens;
  check int "default hard_max_iterations" 100 c.hard_max_iterations;
  check int "default token_buffer" 5000 c.token_buffer

let test_constraints_of_json () =
  let json = Yojson.Safe.from_string {|
    {
      "max_turns": 5,
      "max_tokens": 10000,
      "max_cost_usd": 0.50,
      "hard_max_iterations": 20
    }
  |} in
  let c = Bounded.constraints_of_json json in
  check (option int) "parsed max_turns" (Some 5) c.max_turns;
  check (option int) "parsed max_tokens" (Some 10000) c.max_tokens;
  check int "parsed hard_max_iterations" 20 c.hard_max_iterations

let test_constraints_of_json_partial () =
  let json = Yojson.Safe.from_string {|{"max_turns": 3}|} in
  let c = Bounded.constraints_of_json json in
  check (option int) "only max_turns set" (Some 3) c.max_turns;
  check (option int) "max_tokens is None" None c.max_tokens;
  (* Defaults should be used for unspecified fields *)
  check int "hard_max_iterations default"
    Bounded.default_constraints.hard_max_iterations c.hard_max_iterations

(* ============================================ *)
(* Goal parsing tests                           *)
(* ============================================ *)

let test_goal_of_json_eq () =
  let json = Yojson.Safe.from_string {|
    {"path": "$.status", "condition": {"eq": "done"}}
  |} in
  let goal = Bounded.goal_of_json json in
  check string "path parsed" "$.status" goal.path;
  match goal.condition with
  | Bounded.Eq v ->
      check string "eq value" "\"done\"" (Yojson.Safe.to_string v)
  | _ -> fail "expected Eq condition"

let test_goal_of_json_gte () =
  let json = Yojson.Safe.from_string {|
    {"path": "$.confidence", "condition": {"gte": 0.95}}
  |} in
  let goal = Bounded.goal_of_json json in
  check string "path parsed" "$.confidence" goal.path;
  match goal.condition with
  | Bounded.Gte v ->
      check (float 0.001) "gte value" 0.95 v
  | _ -> fail "expected Gte condition"

let test_goal_of_json_between () =
  let json = Yojson.Safe.from_string {|
    {"path": "$.score", "condition": {"between": [0.5, 1.0]}}
  |} in
  let goal = Bounded.goal_of_json json in
  match goal.condition with
  | Bounded.Between (lo, hi) ->
      check (float 0.001) "between lo" 0.5 lo;
      check (float 0.001) "between hi" 1.0 hi
  | _ -> fail "expected Between condition"

(* ============================================ *)
(* Goal checking tests                          *)
(* ============================================ *)

let test_check_goal_eq_match () =
  let goal = { Bounded.path = "$.status"; condition = Bounded.Eq (`String "done") } in
  let result = Yojson.Safe.from_string {|{"status": "done"}|} in
  check bool "goal should match" true (Bounded.check_goal result goal)

let test_check_goal_eq_no_match () =
  let goal = { Bounded.path = "$.status"; condition = Bounded.Eq (`String "done") } in
  let result = Yojson.Safe.from_string {|{"status": "pending"}|} in
  check bool "goal should not match" false (Bounded.check_goal result goal)

let test_check_goal_gte_match () =
  let goal = { Bounded.path = "$.score"; condition = Bounded.Gte 0.8 } in
  let result = Yojson.Safe.from_string {|{"score": 0.95}|} in
  check bool "gte should match" true (Bounded.check_goal result goal)

let test_check_goal_gte_no_match () =
  let goal = { Bounded.path = "$.score"; condition = Bounded.Gte 0.8 } in
  let result = Yojson.Safe.from_string {|{"score": 0.5}|} in
  check bool "gte should not match" false (Bounded.check_goal result goal)

let test_check_goal_nested_path () =
  let goal = { Bounded.path = "$.result.data.done"; condition = Bounded.Eq (`Bool true) } in
  let result = Yojson.Safe.from_string {|{"result": {"data": {"done": true}}}|} in
  check bool "nested path should match" true (Bounded.check_goal result goal)

let test_check_goal_missing_path () =
  let goal = { Bounded.path = "$.nonexistent"; condition = Bounded.Eq (`String "x") } in
  let result = Yojson.Safe.from_string {|{"status": "done"}|} in
  check bool "missing path returns false" false (Bounded.check_goal result goal)

(* ============================================ *)
(* Bounded run tests (mock spawn)              *)
(* ============================================ *)

let mock_spawn_result ?(success=true) ?(output="{}") () =
  Masc_mcp.Spawn_eio.{
    success;
    output;
    exit_code = if success then 0 else 1;
    elapsed_ms = 100;
    input_tokens = Some 50;
    output_tokens = Some 30;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    cost_usd = Some 0.001;
  }

let test_bounded_run_empty_agents () =
  let constraints = Bounded.default_constraints in
  let goal = { Bounded.path = "$.x"; condition = Bounded.Eq (`Bool true) } in
  let spawn_fn _ _ = mock_spawn_result () in
  let result = Bounded.bounded_run ~constraints ~goal ~agents:[] ~prompt:"test" ~spawn_fn in
  check bool "should fail with empty agents" true (result.status = `Error);
  check bool "reason mentions agents" true
    (String.sub result.reason 0 2 = "No")

let test_bounded_run_goal_reached () =
  let constraints = { Bounded.default_constraints with max_turns = Some 5 } in
  let goal = { Bounded.path = "$.done"; condition = Bounded.Eq (`Bool true) } in
  let spawn_fn _ _ = mock_spawn_result ~output:{|{"done": true}|} () in
  let result = Bounded.bounded_run ~constraints ~goal ~agents:["test"] ~prompt:"test" ~spawn_fn in
  check bool "should reach goal" true (result.status = `Goal_reached);
  check int "should take 1 turn" 1 result.stats.turns

let test_bounded_run_constraint_exceeded () =
  let constraints = { Bounded.default_constraints with max_turns = Some 2; hard_max_iterations = 10 } in
  let goal = { Bounded.path = "$.done"; condition = Bounded.Eq (`Bool true) } in
  let spawn_fn _ _ = mock_spawn_result ~output:{|{"done": false}|} () in
  let result = Bounded.bounded_run ~constraints ~goal ~agents:["test"] ~prompt:"test" ~spawn_fn in
  check bool "should exceed constraint" true (result.status = `Constraint_exceeded);
  check int "should take 2 turns" 2 result.stats.turns;
  check bool "reason mentions turns" true
    (String.length result.reason > 0 && String.sub result.reason 0 5 = "turns")

let test_bounded_run_hard_limit () =
  let constraints = { Bounded.default_constraints with
    max_turns = None;
    max_tokens = None;
    max_cost_usd = None;
    max_time_seconds = None;
    hard_max_iterations = 3
  } in
  let goal = { Bounded.path = "$.never"; condition = Bounded.Eq (`Bool true) } in
  let spawn_fn _ _ = mock_spawn_result ~output:{|{}|} () in
  let result = Bounded.bounded_run ~constraints ~goal ~agents:["test"] ~prompt:"test" ~spawn_fn in
  check bool "should hit hard limit" true (result.status = `Constraint_exceeded);
  check int "should take 3 turns" 3 result.stats.turns;
  check bool "reason mentions hard" true
    (String.lowercase_ascii result.reason |> fun s ->
     String.length s > 0 && (String.sub s 0 4 = "hard"))

let test_bounded_run_token_tracking () =
  let constraints = { Bounded.default_constraints with max_turns = Some 3 } in
  let goal = { Bounded.path = "$.done"; condition = Bounded.Eq (`Bool true) } in
  let call_count = ref 0 in
  let spawn_fn _ _ =
    incr call_count;
    if !call_count >= 2 then
      mock_spawn_result ~output:{|{"done": true}|} ()
    else
      mock_spawn_result ~output:{|{"done": false}|} ()
  in
  let result = Bounded.bounded_run ~constraints ~goal ~agents:["test"] ~prompt:"test" ~spawn_fn in
  check int "should take 2 turns" 2 result.stats.turns;
  check int "tokens_in tracked" 100 result.stats.tokens_in;  (* 50 * 2 *)
  check int "tokens_out tracked" 60 result.stats.tokens_out  (* 30 * 2 *)

let test_bounded_run_round_robin () =
  let constraints = { Bounded.default_constraints with max_turns = Some 4 } in
  let goal = { Bounded.path = "$.x"; condition = Bounded.Eq (`Bool true) } in
  let agents_called = ref [] in
  let spawn_fn agent _ =
    agents_called := agent :: !agents_called;
    mock_spawn_result ~output:{|{}|} ()
  in
  let _ = Bounded.bounded_run ~constraints ~goal ~agents:["a"; "b"] ~prompt:"test" ~spawn_fn in
  let called = List.rev !agents_called in
  check (list string) "round robin order" ["a"; "b"; "a"; "b"] called

let test_bounded_run_spawn_exception () =
  let constraints = Bounded.default_constraints in
  let goal = { Bounded.path = "$.x"; condition = Bounded.Eq (`Bool true) } in
  let spawn_fn _ _ = failwith "Simulated spawn error" in
  let result = Bounded.bounded_run ~constraints ~goal ~agents:["test"] ~prompt:"test" ~spawn_fn in
  check bool "should be error" true (result.status = `Error);
  check bool "reason mentions error" true
    (String.lowercase_ascii result.reason |> fun s ->
     try let _ = Str.search_forward (Str.regexp "error\\|fail") s 0 in true
     with Not_found -> false)

(* ============================================ *)
(* Retry logic tests                            *)
(* ============================================ *)

let test_is_retryable_error () =
  (* Retryable errors *)
  check bool "timeout is retryable" true (Bounded.is_retryable_error "Connection timeout");
  check bool "rate limit is retryable" true (Bounded.is_retryable_error "Rate limit exceeded 429");
  check bool "503 is retryable" true (Bounded.is_retryable_error "Service unavailable 503");
  check bool "connection refused is retryable" true (Bounded.is_retryable_error "ECONNREFUSED");
  (* Non-retryable errors *)
  check bool "generic error not retryable" false (Bounded.is_retryable_error "Invalid JSON");
  check bool "auth error not retryable" false (Bounded.is_retryable_error "Unauthorized 401")

let test_retry_config_parsing () =
  let json = Yojson.Safe.from_string {|
    {
      "max_turns": 5,
      "retry": {
        "max_retries": 5,
        "base_delay_ms": 500,
        "max_delay_ms": 10000,
        "jitter_factor": 0.3
      }
    }
  |} in
  let c = Bounded.constraints_of_json json in
  check int "retry max_retries" 5 c.retry.max_retries;
  check int "retry base_delay_ms" 500 c.retry.base_delay_ms;
  check int "retry max_delay_ms" 10000 c.retry.max_delay_ms;
  check (float 0.01) "retry jitter_factor" 0.3 c.retry.jitter_factor

let test_retry_success_after_failures () =
  let retry_cfg = { Bounded.default_retry_config with max_retries = 3; base_delay_ms = 1 } in
  let constraints = { Bounded.default_constraints with
    max_turns = Some 5;
    retry = retry_cfg;
  } in
  let goal = { Bounded.path = "$.done"; condition = Bounded.Eq (`Bool true) } in
  let call_count = ref 0 in
  let spawn_fn _ _ =
    incr call_count;
    if !call_count <= 2 then
      (* First 2 calls fail with retryable error *)
      mock_spawn_result ~success:false ~output:"Connection timeout" ()
    else
      (* Third call succeeds *)
      mock_spawn_result ~success:true ~output:{|{"done": true}|} ()
  in
  let result = Bounded.bounded_run ~constraints ~goal ~agents:["test"] ~prompt:"test" ~spawn_fn in
  check bool "should reach goal after retries" true (result.status = `Goal_reached);
  check int "total retries recorded" 2 result.stats.total_retries;
  check int "history entry retries" 2 (List.hd result.history).retries

let test_retry_exhausted () =
  let retry_cfg = { Bounded.default_retry_config with max_retries = 2; base_delay_ms = 1 } in
  let constraints = { Bounded.default_constraints with
    max_turns = Some 5;
    retry = retry_cfg;
  } in
  let goal = { Bounded.path = "$.done"; condition = Bounded.Eq (`Bool true) } in
  let spawn_fn _ _ =
    (* Always fail with retryable error *)
    mock_spawn_result ~success:false ~output:"Connection timeout" ()
  in
  let result = Bounded.bounded_run ~constraints ~goal ~agents:["test"] ~prompt:"test" ~spawn_fn in
  check bool "should be error after exhausted retries" true (result.status = `Error);
  check bool "reason mentions attempts" true
    (try let _ = Str.search_forward (Str.regexp "3 attempts") result.reason 0 in true
     with Not_found -> false)

let test_backoff_calculation () =
  let retry_cfg = {
    Bounded.max_retries = 5;
    base_delay_ms = 1000;
    max_delay_ms = 10000;
    jitter_factor = 0.0;  (* No jitter for deterministic test *)
  } in
  (* attempt 0: 1000ms, attempt 1: 2000ms, attempt 2: 4000ms, etc *)
  check int "backoff attempt 0" 1000 (Bounded.calc_backoff_delay retry_cfg 0);
  check int "backoff attempt 1" 2000 (Bounded.calc_backoff_delay retry_cfg 1);
  check int "backoff attempt 2" 4000 (Bounded.calc_backoff_delay retry_cfg 2);
  check int "backoff capped at max" 10000 (Bounded.calc_backoff_delay retry_cfg 5)

(* ============================================ *)
(* Result serialization tests                   *)
(* ============================================ *)

let test_result_to_json () =
  let constraints = { Bounded.default_constraints with max_turns = Some 1 } in
  let goal = { Bounded.path = "$.done"; condition = Bounded.Eq (`Bool true) } in
  let spawn_fn _ _ = mock_spawn_result ~output:{|{"done": true}|} () in
  let result = Bounded.bounded_run ~constraints ~goal ~agents:["test"] ~prompt:"test" ~spawn_fn in
  let json = Bounded.result_to_json result in
  let open Yojson.Safe.Util in
  check string "status in json" "goal_reached" (json |> member "status" |> to_string);
  check int "turns in stats" 1 (json |> member "stats" |> member "turns" |> to_int);
  check int "history length" 1 (json |> member "history" |> to_list |> List.length)

(* ============================================ *)
(* Test suite                                   *)
(* ============================================ *)

let constraint_tests = [
  "default constraints", `Quick, test_default_constraints;
  "constraints from json", `Quick, test_constraints_of_json;
  "constraints partial json", `Quick, test_constraints_of_json_partial;
]

let goal_parsing_tests = [
  "goal eq from json", `Quick, test_goal_of_json_eq;
  "goal gte from json", `Quick, test_goal_of_json_gte;
  "goal between from json", `Quick, test_goal_of_json_between;
]

let goal_checking_tests = [
  "goal eq match", `Quick, test_check_goal_eq_match;
  "goal eq no match", `Quick, test_check_goal_eq_no_match;
  "goal gte match", `Quick, test_check_goal_gte_match;
  "goal gte no match", `Quick, test_check_goal_gte_no_match;
  "goal nested path", `Quick, test_check_goal_nested_path;
  "goal missing path", `Quick, test_check_goal_missing_path;
]

let bounded_run_tests = [
  "empty agents error", `Quick, test_bounded_run_empty_agents;
  "goal reached", `Quick, test_bounded_run_goal_reached;
  "constraint exceeded", `Quick, test_bounded_run_constraint_exceeded;
  "hard limit", `Quick, test_bounded_run_hard_limit;
  "token tracking", `Quick, test_bounded_run_token_tracking;
  "round robin agents", `Quick, test_bounded_run_round_robin;
  "spawn exception", `Quick, test_bounded_run_spawn_exception;
]

let retry_tests = [
  "is_retryable_error", `Quick, test_is_retryable_error;
  "retry config parsing", `Quick, test_retry_config_parsing;
  "retry success after failures", `Quick, test_retry_success_after_failures;
  "retry exhausted", `Quick, test_retry_exhausted;
  "backoff calculation", `Quick, test_backoff_calculation;
]

let serialization_tests = [
  "result to json", `Quick, test_result_to_json;
]

let () =
  run "Bounded" [
    "constraints", constraint_tests;
    "goal_parsing", goal_parsing_tests;
    "goal_checking", goal_checking_tests;
    "bounded_run", bounded_run_tests;
    "retry", retry_tests;
    "serialization", serialization_tests;
  ]
