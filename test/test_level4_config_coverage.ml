(** Level4_config Module Coverage Tests

    Tests for MASC Level 4 Configuration:
    - get_env_float, get_env_int: environment helpers
    - ensure_rng_init, random_float, random_int: RNG helpers
    - Swarm module: swarm configuration
    - Flocking module: flocking configuration
*)

open Alcotest

module Level4_config = Masc_mcp.Level4_config

(* ============================================================
   get_env_float Tests
   ============================================================ *)

let test_get_env_float_default () =
  let result = Level4_config.get_env_float "NONEXISTENT_VAR_XYZ_12345" 42.5 in
  check (float 0.001) "default" 42.5 result

let test_get_env_float_negative_default () =
  let result = Level4_config.get_env_float "NONEXISTENT_VAR_XYZ" (-10.0) in
  check (float 0.001) "negative default" (-10.0) result

let test_get_env_float_zero_default () =
  let result = Level4_config.get_env_float "NONEXISTENT_VAR_XYZ" 0.0 in
  check (float 0.001) "zero default" 0.0 result

(* ============================================================
   get_env_int Tests
   ============================================================ *)

let test_get_env_int_default () =
  let result = Level4_config.get_env_int "NONEXISTENT_VAR_XYZ_12345" 100 in
  check int "default" 100 result

let test_get_env_int_negative_default () =
  let result = Level4_config.get_env_int "NONEXISTENT_VAR_XYZ" (-5) in
  check int "negative default" (-5) result

let test_get_env_int_zero_default () =
  let result = Level4_config.get_env_int "NONEXISTENT_VAR_XYZ" 0 in
  check int "zero default" 0 result

(* ============================================================
   Random Number Tests
   ============================================================ *)

let test_random_float_positive () =
  let result = Level4_config.random_float 10.0 in
  check bool "positive" true (result >= 0.0 && result < 10.0)

let test_random_float_range () =
  let result = Level4_config.random_float 1.0 in
  check bool "in range" true (result >= 0.0 && result < 1.0)

let test_random_int_positive () =
  let result = Level4_config.random_int 100 in
  check bool "positive" true (result >= 0 && result < 100)

let test_random_int_range () =
  let result = Level4_config.random_int 10 in
  check bool "in range" true (result >= 0 && result < 10)

(* ============================================================
   Swarm Module Tests
   ============================================================ *)

let test_swarm_initial_fitness_range () =
  let fitness = Level4_config.Swarm.initial_fitness () in
  check bool "0-1 range" true (fitness >= 0.0 && fitness <= 1.0)

let test_swarm_selection_pressure_range () =
  let pressure = Level4_config.Swarm.selection_pressure () in
  check bool "0-1 range" true (pressure >= 0.0 && pressure <= 1.0)

let test_swarm_mutation_rate_range () =
  let rate = Level4_config.Swarm.mutation_rate () in
  check bool "0-1 range" true (rate >= 0.0 && rate <= 1.0)

let test_swarm_evaporation_rate_positive () =
  let rate = Level4_config.Swarm.evaporation_rate () in
  check bool "positive" true (rate >= 0.0)

let test_swarm_quorum_threshold_range () =
  let threshold = Level4_config.Swarm.quorum_threshold () in
  check bool "0-1 range" true (threshold >= 0.0 && threshold <= 1.0)

let test_swarm_max_agents_positive () =
  let max = Level4_config.Swarm.max_agents () in
  check bool "positive" true (max > 0)

let test_swarm_min_agents_positive () =
  let min = Level4_config.Swarm.min_agents_for_flocking () in
  check bool "positive" true (min > 0)

(* ============================================================
   Flocking Module Tests
   ============================================================ *)

let test_flocking_separation_weight_positive () =
  let weight = Level4_config.Flocking.separation_weight () in
  check bool "positive" true (weight >= 0.0)

let test_flocking_alignment_weight_positive () =
  let weight = Level4_config.Flocking.alignment_weight () in
  check bool "positive" true (weight >= 0.0)

let test_flocking_cohesion_weight_positive () =
  let weight = Level4_config.Flocking.cohesion_weight () in
  check bool "positive" true (weight >= 0.0)

let test_flocking_perception_radius_positive () =
  let radius = Level4_config.Flocking.perception_radius () in
  check bool "positive" true (radius > 0.0)

let test_flocking_fitness_cohesion_multiplier_positive () =
  let mult = Level4_config.Flocking.fitness_cohesion_multiplier () in
  check bool "positive" true (mult > 0.0)

(* ============================================================
   Foraging Module Tests
   ============================================================ *)

let test_foraging_exploration_rate_range () =
  let rate = Level4_config.Foraging.exploration_rate () in
  check bool "0-1 range" true (rate >= 0.0 && rate <= 1.0)

(* ============================================================
   Stigmergy Module Tests
   ============================================================ *)

let test_stigmergy_deposit_rate_range () =
  let rate = Level4_config.Stigmergy.deposit_rate () in
  check bool "0-1 range" true (rate >= 0.0 && rate <= 1.0)

let test_stigmergy_following_threshold_positive () =
  let threshold = Level4_config.Stigmergy.following_threshold () in
  check bool "positive" true (threshold >= 0.0)

let test_stigmergy_top_trails_limit_positive () =
  let limit = Level4_config.Stigmergy.top_trails_limit () in
  check bool "positive" true (limit > 0)

(* ============================================================
   is_finite Tests
   ============================================================ *)

let test_is_finite_normal () =
  check bool "normal" true (Level4_config.is_finite 1.0)

let test_is_finite_zero () =
  check bool "zero" true (Level4_config.is_finite 0.0)

let test_is_finite_negative () =
  check bool "negative" true (Level4_config.is_finite (-1.0))

let test_is_finite_infinity () =
  check bool "infinity" false (Level4_config.is_finite infinity)

let test_is_finite_neg_infinity () =
  check bool "neg infinity" false (Level4_config.is_finite neg_infinity)

let test_is_finite_nan () =
  check bool "nan" false (Level4_config.is_finite nan)

(* ============================================================
   clamp_float Tests
   ============================================================ *)

let test_clamp_float_in_range () =
  let result = Level4_config.clamp_float ~min:0.0 ~max:1.0 0.5 in
  check (float 0.001) "in range" 0.5 result

let test_clamp_float_below_min () =
  let result = Level4_config.clamp_float ~min:0.0 ~max:1.0 (-0.5) in
  check (float 0.001) "clamped to min" 0.0 result

let test_clamp_float_above_max () =
  let result = Level4_config.clamp_float ~min:0.0 ~max:1.0 1.5 in
  check (float 0.001) "clamped to max" 1.0 result

let test_clamp_float_at_min () =
  let result = Level4_config.clamp_float ~min:0.0 ~max:1.0 0.0 in
  check (float 0.001) "at min" 0.0 result

let test_clamp_float_at_max () =
  let result = Level4_config.clamp_float ~min:0.0 ~max:1.0 1.0 in
  check (float 0.001) "at max" 1.0 result

(* ============================================================
   validate_fitness Tests
   ============================================================ *)

let test_validate_fitness_valid () =
  match Level4_config.validate_fitness 0.5 with
  | Some f -> check (float 0.001) "valid" 0.5 f
  | None -> fail "expected Some"

let test_validate_fitness_below_zero () =
  match Level4_config.validate_fitness (-0.5) with
  | Some f -> check (float 0.001) "clamped to 0" 0.0 f
  | None -> fail "expected Some"

let test_validate_fitness_above_one () =
  match Level4_config.validate_fitness 1.5 with
  | Some f -> check (float 0.001) "clamped to 1" 1.0 f
  | None -> fail "expected Some"

let test_validate_fitness_nan () =
  match Level4_config.validate_fitness nan with
  | None -> check bool "nan invalid" true true
  | Some _ -> fail "expected None"

let test_validate_fitness_infinity () =
  match Level4_config.validate_fitness infinity with
  | None -> check bool "infinity invalid" true true
  | Some _ -> fail "expected None"

(* ============================================================
   validate_positive Tests
   ============================================================ *)

let test_validate_positive_valid () =
  match Level4_config.validate_positive 1.0 with
  | Some f -> check (float 0.001) "valid" 1.0 f
  | None -> fail "expected Some"

let test_validate_positive_zero () =
  match Level4_config.validate_positive 0.0 with
  | Some f -> check (float 0.001) "zero" 0.0 f
  | None -> fail "expected Some"

let test_validate_positive_negative () =
  match Level4_config.validate_positive (-1.0) with
  | None -> check bool "negative invalid" true true
  | Some _ -> fail "expected None"

let test_validate_positive_nan () =
  match Level4_config.validate_positive nan with
  | None -> check bool "nan invalid" true true
  | Some _ -> fail "expected None"

(* ============================================================
   Fitness Module Tests
   ============================================================ *)

let test_fitness_of_float_valid () =
  match Level4_config.Fitness.of_float 0.5 with
  | Some _ -> check bool "valid" true true
  | None -> fail "expected Some"

let test_fitness_of_float_zero () =
  match Level4_config.Fitness.of_float 0.0 with
  | Some _ -> check bool "zero valid" true true
  | None -> fail "expected Some"

let test_fitness_of_float_one () =
  match Level4_config.Fitness.of_float 1.0 with
  | Some _ -> check bool "one valid" true true
  | None -> fail "expected Some"

let test_fitness_of_float_negative () =
  match Level4_config.Fitness.of_float (-0.5) with
  | None -> check bool "negative invalid" true true
  | Some _ -> fail "expected None"

let test_fitness_of_float_above_one () =
  match Level4_config.Fitness.of_float 1.5 with
  | None -> check bool "above one invalid" true true
  | Some _ -> fail "expected None"

let test_fitness_of_float_nan () =
  match Level4_config.Fitness.of_float nan with
  | None -> check bool "nan invalid" true true
  | Some _ -> fail "expected None"

let test_fitness_of_float_clamped_valid () =
  let f = Level4_config.Fitness.of_float_clamped 0.5 in
  check (float 0.001) "valid" 0.5 (Level4_config.Fitness.to_float f)

let test_fitness_of_float_clamped_negative () =
  let f = Level4_config.Fitness.of_float_clamped (-0.5) in
  check (float 0.001) "clamped to 0" 0.0 (Level4_config.Fitness.to_float f)

let test_fitness_of_float_clamped_above_one () =
  let f = Level4_config.Fitness.of_float_clamped 1.5 in
  check (float 0.001) "clamped to 1" 1.0 (Level4_config.Fitness.to_float f)

let test_fitness_of_float_clamped_nan () =
  let f = Level4_config.Fitness.of_float_clamped nan in
  check (float 0.001) "nan -> 0.5" 0.5 (Level4_config.Fitness.to_float f)

let test_fitness_to_float () =
  match Level4_config.Fitness.of_float 0.75 with
  | Some f -> check (float 0.001) "round trip" 0.75 (Level4_config.Fitness.to_float f)
  | None -> fail "expected Some"

let test_fitness_initial () =
  let f = Level4_config.Fitness.initial () in
  let v = Level4_config.Fitness.to_float f in
  check bool "0-1 range" true (v >= 0.0 && v <= 1.0)

let test_fitness_combine () =
  match Level4_config.Fitness.of_float 0.4, Level4_config.Fitness.of_float 0.6 with
  | Some a, Some b ->
    let combined = Level4_config.Fitness.combine a b in
    check (float 0.001) "average" 0.5 (Level4_config.Fitness.to_float combined)
  | _ -> fail "expected Some"

let test_fitness_adjust_positive () =
  match Level4_config.Fitness.of_float 0.5 with
  | Some f ->
    let adjusted = Level4_config.Fitness.adjust f ~delta:0.2 in
    check (float 0.001) "adjusted up" 0.7 (Level4_config.Fitness.to_float adjusted)
  | None -> fail "expected Some"

let test_fitness_adjust_negative () =
  match Level4_config.Fitness.of_float 0.5 with
  | Some f ->
    let adjusted = Level4_config.Fitness.adjust f ~delta:(-0.2) in
    check (float 0.001) "adjusted down" 0.3 (Level4_config.Fitness.to_float adjusted)
  | None -> fail "expected Some"

let test_fitness_adjust_clamped () =
  match Level4_config.Fitness.of_float 0.9 with
  | Some f ->
    let adjusted = Level4_config.Fitness.adjust f ~delta:0.5 in
    check (float 0.001) "clamped to 1" 1.0 (Level4_config.Fitness.to_float adjusted)
  | None -> fail "expected Some"

let test_fitness_compare_less () =
  match Level4_config.Fitness.of_float 0.3, Level4_config.Fitness.of_float 0.7 with
  | Some a, Some b -> check bool "less" true (Level4_config.Fitness.compare a b < 0)
  | _ -> fail "expected Some"

let test_fitness_compare_equal () =
  match Level4_config.Fitness.of_float 0.5, Level4_config.Fitness.of_float 0.5 with
  | Some a, Some b -> check int "equal" 0 (Level4_config.Fitness.compare a b)
  | _ -> fail "expected Some"

let test_fitness_compare_greater () =
  match Level4_config.Fitness.of_float 0.7, Level4_config.Fitness.of_float 0.3 with
  | Some a, Some b -> check bool "greater" true (Level4_config.Fitness.compare a b > 0)
  | _ -> fail "expected Some"

let test_fitness_to_json () =
  match Level4_config.Fitness.of_float 0.75 with
  | Some f ->
    let json = Level4_config.Fitness.to_json f in
    let str = Yojson.Safe.to_string json in
    check bool "has 0.75" true
      (try let _ = Str.search_forward (Str.regexp "0.75") str 0 in true
       with Not_found -> false)
  | None -> fail "expected Some"

let test_fitness_of_json_float () =
  match Level4_config.Fitness.of_json (`Float 0.5) with
  | Some f -> check (float 0.001) "from float" 0.5 (Level4_config.Fitness.to_float f)
  | None -> fail "expected Some"

let test_fitness_of_json_int () =
  match Level4_config.Fitness.of_json (`Int 1) with
  | Some f -> check (float 0.001) "from int" 1.0 (Level4_config.Fitness.to_float f)
  | None -> fail "expected Some"

let test_fitness_of_json_invalid () =
  match Level4_config.Fitness.of_json (`String "invalid") with
  | None -> check bool "invalid json" true true
  | Some _ -> fail "expected None"

let test_fitness_of_json_out_of_range () =
  match Level4_config.Fitness.of_json (`Float 2.0) with
  | None -> check bool "out of range" true true
  | Some _ -> fail "expected None"

(* ============================================================
   RNG Initialization Tests
   ============================================================ *)

let test_ensure_rng_init_no_error () =
  Level4_config.ensure_rng_init ();
  check bool "no error" true true

let test_ensure_rng_init_idempotent () =
  Level4_config.ensure_rng_init ();
  Level4_config.ensure_rng_init ();
  check bool "idempotent" true true

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Level4_config Coverage" [
    "get_env_float", [
      test_case "default" `Quick test_get_env_float_default;
      test_case "negative default" `Quick test_get_env_float_negative_default;
      test_case "zero default" `Quick test_get_env_float_zero_default;
    ];
    "get_env_int", [
      test_case "default" `Quick test_get_env_int_default;
      test_case "negative default" `Quick test_get_env_int_negative_default;
      test_case "zero default" `Quick test_get_env_int_zero_default;
    ];
    "random", [
      test_case "float positive" `Quick test_random_float_positive;
      test_case "float range" `Quick test_random_float_range;
      test_case "int positive" `Quick test_random_int_positive;
      test_case "int range" `Quick test_random_int_range;
    ];
    "swarm", [
      test_case "initial_fitness" `Quick test_swarm_initial_fitness_range;
      test_case "selection_pressure" `Quick test_swarm_selection_pressure_range;
      test_case "mutation_rate" `Quick test_swarm_mutation_rate_range;
      test_case "evaporation_rate" `Quick test_swarm_evaporation_rate_positive;
      test_case "quorum_threshold" `Quick test_swarm_quorum_threshold_range;
      test_case "max_agents" `Quick test_swarm_max_agents_positive;
      test_case "min_agents" `Quick test_swarm_min_agents_positive;
    ];
    "flocking", [
      test_case "separation_weight" `Quick test_flocking_separation_weight_positive;
      test_case "alignment_weight" `Quick test_flocking_alignment_weight_positive;
      test_case "cohesion_weight" `Quick test_flocking_cohesion_weight_positive;
      test_case "perception_radius" `Quick test_flocking_perception_radius_positive;
      test_case "fitness_cohesion_multiplier" `Quick test_flocking_fitness_cohesion_multiplier_positive;
    ];
    "foraging", [
      test_case "exploration_rate" `Quick test_foraging_exploration_rate_range;
    ];
    "stigmergy", [
      test_case "deposit_rate" `Quick test_stigmergy_deposit_rate_range;
      test_case "following_threshold" `Quick test_stigmergy_following_threshold_positive;
      test_case "top_trails_limit" `Quick test_stigmergy_top_trails_limit_positive;
    ];
    "is_finite", [
      test_case "normal" `Quick test_is_finite_normal;
      test_case "zero" `Quick test_is_finite_zero;
      test_case "negative" `Quick test_is_finite_negative;
      test_case "infinity" `Quick test_is_finite_infinity;
      test_case "neg_infinity" `Quick test_is_finite_neg_infinity;
      test_case "nan" `Quick test_is_finite_nan;
    ];
    "clamp_float", [
      test_case "in_range" `Quick test_clamp_float_in_range;
      test_case "below_min" `Quick test_clamp_float_below_min;
      test_case "above_max" `Quick test_clamp_float_above_max;
      test_case "at_min" `Quick test_clamp_float_at_min;
      test_case "at_max" `Quick test_clamp_float_at_max;
    ];
    "validate_fitness", [
      test_case "valid" `Quick test_validate_fitness_valid;
      test_case "below_zero" `Quick test_validate_fitness_below_zero;
      test_case "above_one" `Quick test_validate_fitness_above_one;
      test_case "nan" `Quick test_validate_fitness_nan;
      test_case "infinity" `Quick test_validate_fitness_infinity;
    ];
    "validate_positive", [
      test_case "valid" `Quick test_validate_positive_valid;
      test_case "zero" `Quick test_validate_positive_zero;
      test_case "negative" `Quick test_validate_positive_negative;
      test_case "nan" `Quick test_validate_positive_nan;
    ];
    "fitness", [
      test_case "of_float valid" `Quick test_fitness_of_float_valid;
      test_case "of_float zero" `Quick test_fitness_of_float_zero;
      test_case "of_float one" `Quick test_fitness_of_float_one;
      test_case "of_float negative" `Quick test_fitness_of_float_negative;
      test_case "of_float above_one" `Quick test_fitness_of_float_above_one;
      test_case "of_float nan" `Quick test_fitness_of_float_nan;
      test_case "of_float_clamped valid" `Quick test_fitness_of_float_clamped_valid;
      test_case "of_float_clamped negative" `Quick test_fitness_of_float_clamped_negative;
      test_case "of_float_clamped above_one" `Quick test_fitness_of_float_clamped_above_one;
      test_case "of_float_clamped nan" `Quick test_fitness_of_float_clamped_nan;
      test_case "to_float" `Quick test_fitness_to_float;
      test_case "initial" `Quick test_fitness_initial;
      test_case "combine" `Quick test_fitness_combine;
      test_case "adjust positive" `Quick test_fitness_adjust_positive;
      test_case "adjust negative" `Quick test_fitness_adjust_negative;
      test_case "adjust clamped" `Quick test_fitness_adjust_clamped;
      test_case "compare less" `Quick test_fitness_compare_less;
      test_case "compare equal" `Quick test_fitness_compare_equal;
      test_case "compare greater" `Quick test_fitness_compare_greater;
      test_case "to_json" `Quick test_fitness_to_json;
      test_case "of_json float" `Quick test_fitness_of_json_float;
      test_case "of_json int" `Quick test_fitness_of_json_int;
      test_case "of_json invalid" `Quick test_fitness_of_json_invalid;
      test_case "of_json out_of_range" `Quick test_fitness_of_json_out_of_range;
    ];
    "rng_init", [
      test_case "no error" `Quick test_ensure_rng_init_no_error;
      test_case "idempotent" `Quick test_ensure_rng_init_idempotent;
    ];
  ]
