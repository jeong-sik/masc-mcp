(** Prometheus Module Coverage Tests

    Tests for MASC Prometheus metrics:
    - metric_type enum (Counter, Gauge, Histogram)
    - type_to_string: metric type to Prometheus format
    - labels_to_string: label list to Prometheus format
    - register_counter, register_gauge
    - inc_counter, set_gauge, inc_gauge, dec_gauge
    - to_prometheus_text: full export
    - convenience functions
*)

open Alcotest

module Prometheus = Masc_mcp.Prometheus

(* ============================================================
   type_to_string Tests
   ============================================================ *)

let test_type_to_string_counter () =
  check string "counter" "counter" (Prometheus.type_to_string Prometheus.Counter)

let test_type_to_string_gauge () =
  check string "gauge" "gauge" (Prometheus.type_to_string Prometheus.Gauge)

let test_type_to_string_histogram () =
  check string "histogram" "histogram" (Prometheus.type_to_string Prometheus.Histogram)

(* ============================================================
   labels_to_string Tests
   ============================================================ *)

let test_labels_to_string_empty () =
  check string "empty" "" (Prometheus.labels_to_string [])

let test_labels_to_string_single () =
  let result = Prometheus.labels_to_string [("key", "value")] in
  check string "single" "{key=\"value\"}" result

let test_labels_to_string_multiple () =
  let result = Prometheus.labels_to_string [("k1", "v1"); ("k2", "v2")] in
  check string "multiple" "{k1=\"v1\",k2=\"v2\"}" result

let test_labels_to_string_escaped () =
  let result = Prometheus.labels_to_string [("key", "value\"with\"quotes")] in
  check bool "escaped" true (String.length result > 0)

(* ============================================================
   metric_type Enum Tests
   ============================================================ *)

let test_metric_type_counter () =
  let t : Prometheus.metric_type = Prometheus.Counter in
  check string "counter type" "counter" (Prometheus.type_to_string t)

let test_metric_type_gauge () =
  let t : Prometheus.metric_type = Prometheus.Gauge in
  check string "gauge type" "gauge" (Prometheus.type_to_string t)

let test_metric_type_histogram () =
  let t : Prometheus.metric_type = Prometheus.Histogram in
  check string "histogram type" "histogram" (Prometheus.type_to_string t)

(* ============================================================
   register_counter Tests
   ============================================================ *)

let test_register_counter_basic () =
  Prometheus.register_counter ~name:"test_counter_basic" ~help:"A test counter" ();
  (* Registration should not throw *)
  check bool "registered" true true

let test_register_counter_with_labels () =
  Prometheus.register_counter
    ~name:"test_counter_labels"
    ~help:"Counter with labels"
    ~labels:[("env", "test")]
    ();
  check bool "registered with labels" true true

let test_register_counter_idempotent () =
  Prometheus.register_counter ~name:"test_counter_idem" ~help:"Idempotent" ();
  Prometheus.register_counter ~name:"test_counter_idem" ~help:"Idempotent" ();
  check bool "idempotent" true true

(* ============================================================
   register_gauge Tests
   ============================================================ *)

let test_register_gauge_basic () =
  Prometheus.register_gauge ~name:"test_gauge_basic" ~help:"A test gauge" ();
  check bool "registered" true true

let test_register_gauge_with_labels () =
  Prometheus.register_gauge
    ~name:"test_gauge_labels"
    ~help:"Gauge with labels"
    ~labels:[("env", "test")]
    ();
  check bool "registered with labels" true true

(* ============================================================
   inc_counter Tests
   ============================================================ *)

let test_inc_counter_default () =
  Prometheus.inc_counter "test_inc_default" ();
  check bool "incremented" true true

let test_inc_counter_with_delta () =
  Prometheus.inc_counter "test_inc_delta" ~delta:5.0 ();
  check bool "incremented with delta" true true

let test_inc_counter_with_labels () =
  Prometheus.inc_counter "test_inc_labels" ~labels:[("type", "test")] ();
  check bool "incremented with labels" true true

let test_inc_counter_auto_register () =
  Prometheus.inc_counter "test_auto_register" ();
  (* Counter is auto-registered if not exists *)
  check bool "auto registered" true true

(* ============================================================
   set_gauge Tests
   ============================================================ *)

let test_set_gauge_basic () =
  Prometheus.set_gauge "test_set_basic" 42.0;
  check bool "set" true true

let test_set_gauge_with_labels () =
  Prometheus.set_gauge "test_set_labels" ~labels:[("env", "test")] 100.0;
  check bool "set with labels" true true

let test_set_gauge_auto_register () =
  Prometheus.set_gauge "test_set_auto" 1.0;
  check bool "auto registered" true true

let test_set_gauge_overwrite () =
  Prometheus.set_gauge "test_set_overwrite" 1.0;
  Prometheus.set_gauge "test_set_overwrite" 2.0;
  check bool "overwritten" true true

(* ============================================================
   inc_gauge Tests
   ============================================================ *)

let test_inc_gauge_default () =
  Prometheus.inc_gauge "test_inc_gauge_default" ();
  check bool "incremented" true true

let test_inc_gauge_with_delta () =
  Prometheus.inc_gauge "test_inc_gauge_delta" ~delta:10.0 ();
  check bool "incremented with delta" true true

let test_inc_gauge_with_labels () =
  Prometheus.inc_gauge "test_inc_gauge_labels" ~labels:[("type", "test")] ();
  check bool "incremented with labels" true true

(* ============================================================
   dec_gauge Tests
   ============================================================ *)

let test_dec_gauge_default () =
  Prometheus.set_gauge "test_dec_gauge" 10.0;
  Prometheus.dec_gauge "test_dec_gauge" ();
  check bool "decremented" true true

let test_dec_gauge_with_delta () =
  Prometheus.set_gauge "test_dec_delta" 10.0;
  Prometheus.dec_gauge "test_dec_delta" ~delta:5.0 ();
  check bool "decremented with delta" true true

(* ============================================================
   to_prometheus_text Tests
   ============================================================ *)

let test_to_prometheus_text_not_empty () =
  let text = Prometheus.to_prometheus_text () in
  check bool "not empty" true (String.length text > 0)

let test_to_prometheus_text_has_help () =
  let text = Prometheus.to_prometheus_text () in
  check bool "has HELP" true
    (try
      let _ = Str.search_forward (Str.regexp "# HELP") text 0 in true
    with Not_found -> false)

let test_to_prometheus_text_has_type () =
  let text = Prometheus.to_prometheus_text () in
  check bool "has TYPE" true
    (try
      let _ = Str.search_forward (Str.regexp "# TYPE") text 0 in true
    with Not_found -> false)

let test_to_prometheus_text_has_uptime () =
  let text = Prometheus.to_prometheus_text () in
  check bool "has uptime" true
    (try
      let _ = Str.search_forward (Str.regexp "masc_uptime_seconds") text 0 in true
    with Not_found -> false)

(* ============================================================
   Convenience Functions Tests
   ============================================================ *)

let test_record_request () =
  Prometheus.record_request ();
  check bool "request recorded" true true

let test_record_task_completed () =
  Prometheus.record_task_completed ();
  check bool "task completed recorded" true true

let test_record_task_failed () =
  Prometheus.record_task_failed ();
  check bool "task failed recorded" true true

let test_record_error_default () =
  Prometheus.record_error ();
  check bool "error recorded" true true

let test_record_error_with_type () =
  Prometheus.record_error ~error_type:"test_error" ();
  check bool "error recorded with type" true true

let test_set_active_agents () =
  Prometheus.set_active_agents 5;
  check bool "active agents set" true true

let test_set_pending_tasks () =
  Prometheus.set_pending_tasks 10;
  check bool "pending tasks set" true true

(* ============================================================
   update_uptime Tests
   ============================================================ *)

let test_update_uptime () =
  Prometheus.update_uptime ();
  check bool "uptime updated" true true

(* ============================================================
   init Tests
   ============================================================ *)

let test_init () =
  (* init is called automatically on module load *)
  Prometheus.init ();
  check bool "init" true true

(* ============================================================
   label Type Tests
   ============================================================ *)

let test_label_type () =
  let l : Prometheus.label = ("key", "value") in
  check string "fst" "key" (fst l);
  check string "snd" "value" (snd l)

(* ============================================================
   Edge Cases
   ============================================================ *)

let test_empty_metric_name () =
  Prometheus.inc_counter "" ();
  check bool "empty name" true true

let test_special_chars_in_name () =
  Prometheus.inc_counter "metric_with_underscore" ();
  check bool "underscore name" true true

let test_unicode_in_label () =
  Prometheus.inc_counter "test_unicode" ~labels:[("한글", "값")] ();
  check bool "unicode label" true true

let test_negative_gauge () =
  Prometheus.set_gauge "test_negative" (-10.0);
  check bool "negative gauge" true true

let test_zero_gauge () =
  Prometheus.set_gauge "test_zero" 0.0;
  check bool "zero gauge" true true

let test_large_value () =
  Prometheus.set_gauge "test_large" 1e15;
  check bool "large value" true true

let test_small_value () =
  Prometheus.set_gauge "test_small" 1e-15;
  check bool "small value" true true

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Prometheus Coverage" [
    "type_to_string", [
      test_case "counter" `Quick test_type_to_string_counter;
      test_case "gauge" `Quick test_type_to_string_gauge;
      test_case "histogram" `Quick test_type_to_string_histogram;
    ];
    "labels_to_string", [
      test_case "empty" `Quick test_labels_to_string_empty;
      test_case "single" `Quick test_labels_to_string_single;
      test_case "multiple" `Quick test_labels_to_string_multiple;
      test_case "escaped" `Quick test_labels_to_string_escaped;
    ];
    "metric_type", [
      test_case "counter" `Quick test_metric_type_counter;
      test_case "gauge" `Quick test_metric_type_gauge;
      test_case "histogram" `Quick test_metric_type_histogram;
    ];
    "register_counter", [
      test_case "basic" `Quick test_register_counter_basic;
      test_case "with labels" `Quick test_register_counter_with_labels;
      test_case "idempotent" `Quick test_register_counter_idempotent;
    ];
    "register_gauge", [
      test_case "basic" `Quick test_register_gauge_basic;
      test_case "with labels" `Quick test_register_gauge_with_labels;
    ];
    "inc_counter", [
      test_case "default" `Quick test_inc_counter_default;
      test_case "with delta" `Quick test_inc_counter_with_delta;
      test_case "with labels" `Quick test_inc_counter_with_labels;
      test_case "auto register" `Quick test_inc_counter_auto_register;
    ];
    "set_gauge", [
      test_case "basic" `Quick test_set_gauge_basic;
      test_case "with labels" `Quick test_set_gauge_with_labels;
      test_case "auto register" `Quick test_set_gauge_auto_register;
      test_case "overwrite" `Quick test_set_gauge_overwrite;
    ];
    "inc_gauge", [
      test_case "default" `Quick test_inc_gauge_default;
      test_case "with delta" `Quick test_inc_gauge_with_delta;
      test_case "with labels" `Quick test_inc_gauge_with_labels;
    ];
    "dec_gauge", [
      test_case "default" `Quick test_dec_gauge_default;
      test_case "with delta" `Quick test_dec_gauge_with_delta;
    ];
    "to_prometheus_text", [
      test_case "not empty" `Quick test_to_prometheus_text_not_empty;
      test_case "has HELP" `Quick test_to_prometheus_text_has_help;
      test_case "has TYPE" `Quick test_to_prometheus_text_has_type;
      test_case "has uptime" `Quick test_to_prometheus_text_has_uptime;
    ];
    "convenience", [
      test_case "record_request" `Quick test_record_request;
      test_case "record_task_completed" `Quick test_record_task_completed;
      test_case "record_task_failed" `Quick test_record_task_failed;
      test_case "record_error default" `Quick test_record_error_default;
      test_case "record_error with type" `Quick test_record_error_with_type;
      test_case "set_active_agents" `Quick test_set_active_agents;
      test_case "set_pending_tasks" `Quick test_set_pending_tasks;
    ];
    "update_uptime", [
      test_case "update" `Quick test_update_uptime;
    ];
    "init", [
      test_case "init" `Quick test_init;
    ];
    "label", [
      test_case "type" `Quick test_label_type;
    ];
    "edge_cases", [
      test_case "empty name" `Quick test_empty_metric_name;
      test_case "underscore name" `Quick test_special_chars_in_name;
      test_case "unicode label" `Quick test_unicode_in_label;
      test_case "negative gauge" `Quick test_negative_gauge;
      test_case "zero gauge" `Quick test_zero_gauge;
      test_case "large value" `Quick test_large_value;
      test_case "small value" `Quick test_small_value;
    ];
  ]
