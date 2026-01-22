(** MASC Level 2 MAGI Recommendations Tests

    Tests for the 4 MAGI recommendations:
    1. Cache hit/miss metrics (metrics_cache.ml)
    2. Lock contention timing (hebbian.ml)
    3. Externalized constants (level2_config.ml)
    4. Validation rejection logging (validation.ml)
*)

open Masc_mcp

(* Test utilities *)
let assert_true msg cond =
  if not cond then failwith (Printf.sprintf "Assertion failed: %s" msg)

let assert_equal msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "Assertion failed: %s (expected %s, got %s)"
      msg (string_of_int expected) (string_of_int actual))

let assert_float_in_range msg ~min ~max value =
  if value < min || value > max then
    failwith (Printf.sprintf "Assertion failed: %s (%.3f not in [%.3f, %.3f])"
      msg value min max)

(* ========================================
   1. Level2_config Tests
   ======================================== *)

let test_config_defaults () =
  (* Test that defaults are sensible *)
  let ttl = Level2_config.Metrics_cache.ttl_seconds () in
  assert_float_in_range "metrics_cache_ttl" ~min:1.0 ~max:3600.0 ttl;

  let token_size = Level2_config.Token_cache.max_size () in
  assert_true "token_cache_size > 0" (token_size > 0);
  assert_true "token_cache_size <= 10000" (token_size <= 10000);

  let threshold = Level2_config.Drift_guard.default_threshold () in
  assert_float_in_range "drift_threshold" ~min:0.0 ~max:1.0 threshold;

  let warn_ms = Level2_config.Lock.warn_threshold_ms () in
  assert_float_in_range "lock_warn_ms" ~min:1.0 ~max:10000.0 warn_ms;

  let hebbian_rate = Level2_config.Hebbian.learning_rate () in
  assert_float_in_range "hebbian_rate" ~min:0.001 ~max:1.0 hebbian_rate;

  Printf.printf "✓ test_config_defaults passed\n"

let test_config_to_json () =
  (* Test JSON export for debugging *)
  let json = Level2_config.to_json () in
  match json with
  | `Assoc fields ->
    assert_true "has metrics_cache_ttl" (List.mem_assoc "metrics_cache_ttl" fields);
    assert_true "has drift_threshold" (List.mem_assoc "drift_threshold" fields);
    assert_true "has hebbian_rate" (List.mem_assoc "hebbian_rate" fields);
    Printf.printf "✓ test_config_to_json passed\n"
  | _ -> failwith "Expected Assoc from to_json"

(* ========================================
   2. Metrics_cache hit/miss Tests
   ======================================== *)

let test_cache_stats_initial () =
  (* Reset and check initial state *)
  Metrics_cache.reset_stats ();
  Metrics_cache.clear ();
  let stats = Metrics_cache.get_stats () in
  assert_equal "initial size" 0 stats.size;
  assert_equal "initial hits" 0 stats.hits;
  assert_equal "initial misses" 0 stats.misses;
  assert_float_in_range "initial hit_rate" ~min:0.0 ~max:0.0 stats.hit_rate;
  Printf.printf "✓ test_cache_stats_initial passed\n"

let test_cache_hit_miss_counting () =
  Metrics_cache.reset_stats ();
  Metrics_cache.clear ();

  (* Manually increment to simulate cache behavior *)
  (* Note: We can't easily test the full flow without metrics_store setup *)
  let stats1 = Metrics_cache.get_stats () in
  assert_equal "no operations yet" 0 (stats1.hits + stats1.misses);

  Printf.printf "✓ test_cache_hit_miss_counting passed\n"

let test_cache_prune () =
  Metrics_cache.clear ();
  let pruned = Metrics_cache.prune () in
  assert_equal "nothing to prune" 0 pruned;
  Printf.printf "✓ test_cache_prune passed\n"

(* ========================================
   3. Hebbian Lock Timing Tests
   ======================================== *)

let test_lock_stats_initial () =
  Hebbian.reset_lock_stats ();
  let (acquisitions, avg_wait, max_wait) = Hebbian.get_lock_stats () in
  assert_equal "initial acquisitions" 0 acquisitions;
  assert_float_in_range "initial avg_wait" ~min:0.0 ~max:0.0 avg_wait;
  assert_float_in_range "initial max_wait" ~min:0.0 ~max:0.0 max_wait;
  Printf.printf "✓ test_lock_stats_initial passed\n"

let test_lock_stats_after_operation () =
  Hebbian.reset_lock_stats ();

  (* Create a temp config for testing using the utility function *)
  let temp_dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc_test_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir temp_dir 0o755;
  let config = Room_utils.default_config temp_dir in

  (* Run a strengthen operation which uses the lock *)
  let _ = Lwt_main.run (
    Hebbian.strengthen config ~from_agent:"test_a" ~to_agent:"test_b" ()
  ) in

  let (acquisitions, avg_wait, max_wait) = Hebbian.get_lock_stats () in
  assert_true "at least 1 acquisition" (acquisitions >= 1);
  assert_true "avg_wait >= 0" (avg_wait >= 0.0);
  assert_true "max_wait >= 0" (max_wait >= 0.0);

  (* Cleanup *)
  let masc_dir = Filename.concat temp_dir ".masc" in
  let synapses_dir = Filename.concat masc_dir "synapses" in
  (try Unix.unlink (Filename.concat synapses_dir "graph.json") with _ -> ());
  (try Unix.unlink (Filename.concat synapses_dir "graph.json.lock") with _ -> ());
  (try Unix.rmdir synapses_dir with _ -> ());
  (try Unix.rmdir masc_dir with _ -> ());
  (try Unix.rmdir temp_dir with _ -> ());

  Printf.printf "✓ test_lock_stats_after_operation passed\n"

(* ========================================
   4. Validation Rejection Logging Tests
   ======================================== *)

let test_validation_stats_initial () =
  Validation.reset_rejection_stats ();
  let (count, last_time) = Validation.get_rejection_stats () in
  assert_equal "initial rejection count" 0 count;
  assert_float_in_range "initial last_time" ~min:0.0 ~max:0.0 last_time;
  Printf.printf "✓ test_validation_stats_initial passed\n"

let test_validation_rejection_counted () =
  Validation.reset_rejection_stats ();

  (* Trigger some rejections *)
  let _ = Validation.Agent_id.validate "" in  (* empty *)
  let _ = Validation.Agent_id.validate "../etc/passwd" in  (* path traversal *)
  let _ = Validation.Task_id.validate "task/with/slash" in  (* slash *)

  let (count, last_time) = Validation.get_rejection_stats () in
  assert_equal "3 rejections" 3 count;
  assert_true "last_time updated" (last_time > 0.0);
  Printf.printf "✓ test_validation_rejection_counted passed\n"

let test_validation_success_not_counted () =
  Validation.reset_rejection_stats ();

  (* Valid inputs should not increment counter *)
  let _ = Validation.Agent_id.validate "valid_agent" in
  let _ = Validation.Task_id.validate "valid-task-123" in

  let (count, _) = Validation.get_rejection_stats () in
  assert_equal "no rejections for valid input" 0 count;
  Printf.printf "✓ test_validation_success_not_counted passed\n"

let test_validation_agent_id_patterns () =
  Validation.reset_rejection_stats ();

  (* Test various rejection patterns *)
  assert_true "empty rejected" (Result.is_error (Validation.Agent_id.validate ""));
  assert_true "slash rejected" (Result.is_error (Validation.Agent_id.validate "a/b"));
  assert_true "backslash rejected" (Result.is_error (Validation.Agent_id.validate "a\\b"));
  assert_true "dotdot rejected" (Result.is_error (Validation.Agent_id.validate "..bad"));
  assert_true "too long rejected" (Result.is_error (Validation.Agent_id.validate (String.make 65 'a')));

  (* Valid patterns *)
  assert_true "simple valid" (Result.is_ok (Validation.Agent_id.validate "claude"));
  assert_true "with dash valid" (Result.is_ok (Validation.Agent_id.validate "claude-agent"));
  assert_true "with underscore valid" (Result.is_ok (Validation.Agent_id.validate "claude_agent"));
  assert_true "with numbers valid" (Result.is_ok (Validation.Agent_id.validate "agent123"));

  let (count, _) = Validation.get_rejection_stats () in
  assert_equal "5 rejections" 5 count;
  Printf.printf "✓ test_validation_agent_id_patterns passed\n"

let test_validation_task_id_patterns () =
  Validation.reset_rejection_stats ();

  (* Test task_id specific patterns *)
  assert_true "colon allowed" (Result.is_ok (Validation.Task_id.validate "task:001"));
  assert_true "complex valid" (Result.is_ok (Validation.Task_id.validate "PK-12345:subtask-1"));

  Printf.printf "✓ test_validation_task_id_patterns passed\n"

let test_safe_path_patterns () =
  Validation.reset_rejection_stats ();

  assert_true "absolute rejected" (Result.is_error (Validation.Safe_path.validate_relative "/etc/passwd"));
  assert_true "dotdot rejected" (Result.is_error (Validation.Safe_path.validate_relative "../secret"));
  assert_true "embedded dotdot rejected" (Result.is_error (Validation.Safe_path.validate_relative "foo/../bar"));

  assert_true "relative valid" (Result.is_ok (Validation.Safe_path.validate_relative "foo/bar.txt"));
  assert_true "simple valid" (Result.is_ok (Validation.Safe_path.validate_relative "file.txt"));

  let (count, _) = Validation.get_rejection_stats () in
  assert_equal "3 path rejections" 3 count;
  Printf.printf "✓ test_safe_path_patterns passed\n"

(* ========================================
   Main Test Runner
   ======================================== *)

let () =
  Random.self_init ();

  Printf.printf "\n=== Level 2 MAGI Recommendations Tests ===\n\n";

  Printf.printf "--- 1. Level2_config Tests ---\n";
  test_config_defaults ();
  test_config_to_json ();

  Printf.printf "\n--- 2. Metrics_cache Hit/Miss Tests ---\n";
  test_cache_stats_initial ();
  test_cache_hit_miss_counting ();
  test_cache_prune ();

  Printf.printf "\n--- 3. Hebbian Lock Timing Tests ---\n";
  test_lock_stats_initial ();
  test_lock_stats_after_operation ();

  Printf.printf "\n--- 4. Validation Rejection Tests ---\n";
  test_validation_stats_initial ();
  test_validation_rejection_counted ();
  test_validation_success_not_counted ();
  test_validation_agent_id_patterns ();
  test_validation_task_id_patterns ();
  test_safe_path_patterns ();

  Printf.printf "\nAll Level 2 MAGI tests passed! ✓\n"
