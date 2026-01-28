(** Hebbian Eio Module Coverage Tests

    Tests for Hebbian learning types:
    - synapse type
    - synapse_graph type
    - learning_params type
    - default_params function
*)

open Alcotest

module Hebbian_eio = Masc_mcp.Hebbian_eio

(* ============================================================
   synapse Type Tests
   ============================================================ *)

let test_synapse_type () =
  let s : Hebbian_eio.synapse = {
    from_agent = "claude";
    to_agent = "gemini";
    weight = 0.5;
    success_count = 10;
    failure_count = 2;
    last_updated = 1704067200.0;
    created_at = 1704000000.0;
  } in
  check string "from_agent" "claude" s.from_agent;
  check string "to_agent" "gemini" s.to_agent;
  check (float 0.01) "weight" 0.5 s.weight

let test_synapse_counts () =
  let s : Hebbian_eio.synapse = {
    from_agent = "a";
    to_agent = "b";
    weight = 0.8;
    success_count = 100;
    failure_count = 5;
    last_updated = 0.0;
    created_at = 0.0;
  } in
  check int "success_count" 100 s.success_count;
  check int "failure_count" 5 s.failure_count

let test_synapse_timestamps () =
  let s : Hebbian_eio.synapse = {
    from_agent = "x";
    to_agent = "y";
    weight = 0.1;
    success_count = 0;
    failure_count = 0;
    last_updated = 1704153600.0;
    created_at = 1704067200.0;
  } in
  check bool "last_updated > created_at" true (s.last_updated > s.created_at)

(* ============================================================
   synapse_graph Type Tests
   ============================================================ *)

let test_synapse_graph_empty () =
  let g : Hebbian_eio.synapse_graph = {
    synapses = [];
    last_consolidation = 0.0;
  } in
  check int "synapses empty" 0 (List.length g.synapses)

let test_synapse_graph_with_data () =
  let s1 : Hebbian_eio.synapse = {
    from_agent = "a"; to_agent = "b"; weight = 0.5;
    success_count = 1; failure_count = 0;
    last_updated = 0.0; created_at = 0.0;
  } in
  let s2 : Hebbian_eio.synapse = {
    from_agent = "b"; to_agent = "c"; weight = 0.3;
    success_count = 2; failure_count = 1;
    last_updated = 0.0; created_at = 0.0;
  } in
  let g : Hebbian_eio.synapse_graph = {
    synapses = [s1; s2];
    last_consolidation = 1704067200.0;
  } in
  check int "synapses count" 2 (List.length g.synapses)

(* ============================================================
   learning_params Type Tests
   ============================================================ *)

let test_learning_params_type () =
  let p : Hebbian_eio.learning_params = {
    strengthen_rate = 0.1;
    weaken_rate = 0.05;
    decay_rate = 0.01;
    min_weight = 0.05;
    max_weight = 1.0;
  } in
  check (float 0.001) "strengthen_rate" 0.1 p.strengthen_rate;
  check (float 0.001) "weaken_rate" 0.05 p.weaken_rate;
  check (float 0.001) "decay_rate" 0.01 p.decay_rate

let test_learning_params_constraints () =
  let p : Hebbian_eio.learning_params = {
    strengthen_rate = 0.2;
    weaken_rate = 0.1;
    decay_rate = 0.02;
    min_weight = 0.0;
    max_weight = 1.0;
  } in
  check bool "min < max" true (p.min_weight < p.max_weight);
  check bool "strengthen > 0" true (p.strengthen_rate > 0.0);
  check bool "decay > 0" true (p.decay_rate > 0.0)

(* ============================================================
   default_params Tests
   ============================================================ *)

let test_default_params () =
  let p = Hebbian_eio.default_params () in
  check bool "strengthen > 0" true (p.strengthen_rate > 0.0);
  check bool "weaken > 0" true (p.weaken_rate > 0.0);
  check bool "decay > 0" true (p.decay_rate > 0.0);
  check bool "min_weight >= 0" true (p.min_weight >= 0.0);
  check bool "max_weight <= 1" true (p.max_weight <= 1.0)

let test_default_params_ordering () =
  let p = Hebbian_eio.default_params () in
  check bool "min < max" true (p.min_weight < p.max_weight)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Hebbian Eio Coverage" [
    "synapse", [
      test_case "type" `Quick test_synapse_type;
      test_case "counts" `Quick test_synapse_counts;
      test_case "timestamps" `Quick test_synapse_timestamps;
    ];
    "synapse_graph", [
      test_case "empty" `Quick test_synapse_graph_empty;
      test_case "with data" `Quick test_synapse_graph_with_data;
    ];
    "learning_params", [
      test_case "type" `Quick test_learning_params_type;
      test_case "constraints" `Quick test_learning_params_constraints;
    ];
    "default_params", [
      test_case "values" `Quick test_default_params;
      test_case "ordering" `Quick test_default_params_ordering;
    ];
  ]
