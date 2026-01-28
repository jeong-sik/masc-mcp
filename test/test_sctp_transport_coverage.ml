(** SCTP Transport Module Coverage Tests

    Tests for SCTP flow control configuration:
    - config type
    - default_config constants
    - stats type and operations
    - transport operations
*)

open Alcotest

module Sctp_transport = Masc_mcp.Sctp_transport

(* ============================================================
   config Type Tests
   ============================================================ *)

let test_config_type () =
  let cfg : Sctp_transport.config = {
    initial_cwnd = 32768;
    max_cwnd = 524288;
    initial_rto = 0.5;
    min_rto = 0.05;
    max_rto = 5.0;
    max_burst = 2;
  } in
  check int "initial_cwnd" 32768 cfg.initial_cwnd;
  check int "max_cwnd" 524288 cfg.max_cwnd;
  check int "max_burst" 2 cfg.max_burst

let test_config_rto () =
  let cfg : Sctp_transport.config = {
    initial_cwnd = 65536;
    max_cwnd = 1048576;
    initial_rto = 1.5;
    min_rto = 0.1;
    max_rto = 15.0;
    max_burst = 4;
  } in
  check (float 0.01) "initial_rto" 1.5 cfg.initial_rto;
  check (float 0.01) "min_rto" 0.1 cfg.min_rto;
  check (float 0.01) "max_rto" 15.0 cfg.max_rto

(* ============================================================
   default_config Tests
   ============================================================ *)

let test_default_initial_cwnd () =
  check int "initial_cwnd" 65536 Sctp_transport.default_config.initial_cwnd

let test_default_max_cwnd () =
  check int "max_cwnd" 1048576 Sctp_transport.default_config.max_cwnd

let test_default_initial_rto () =
  check (float 0.01) "initial_rto" 1.0 Sctp_transport.default_config.initial_rto

let test_default_min_rto () =
  check (float 0.01) "min_rto" 0.1 Sctp_transport.default_config.min_rto

let test_default_max_rto () =
  check (float 0.01) "max_rto" 10.0 Sctp_transport.default_config.max_rto

let test_default_max_burst () =
  check int "max_burst" 4 Sctp_transport.default_config.max_burst

(* ============================================================
   stats Type Tests
   ============================================================ *)

let test_stats_type () =
  let s : Sctp_transport.stats = {
    cwnd = 65536;
    ssthresh = 65536;
    in_flight_bytes = 1024;
    in_flight_count = 5;
    buffered_bytes = 512;
    buffered_count = 3;
    rto = 1.0;
    srtt = 0.5;
  } in
  check int "cwnd" 65536 s.cwnd;
  check int "ssthresh" 65536 s.ssthresh;
  check int "in_flight_bytes" 1024 s.in_flight_bytes;
  check int "in_flight_count" 5 s.in_flight_count;
  check int "buffered_bytes" 512 s.buffered_bytes;
  check int "buffered_count" 3 s.buffered_count

let test_stats_to_json () =
  let t = Sctp_transport.create () in
  let stats = Sctp_transport.get_stats t in
  let json = Sctp_transport.stats_to_json stats in
  let open Yojson.Safe.Util in
  check int "cwnd json" stats.cwnd (json |> member "cwnd" |> to_int);
  check int "inFlightBytes json" stats.in_flight_bytes (json |> member "inFlightBytes" |> to_int)

let test_pp_stats () =
  let s : Sctp_transport.stats = {
    cwnd = 65536;
    ssthresh = 65536;
    in_flight_bytes = 100;
    in_flight_count = 2;
    buffered_bytes = 50;
    buffered_count = 1;
    rto = 1.0;
    srtt = 0.5;
  } in
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  Sctp_transport.pp_stats fmt s;
  Format.pp_print_flush fmt ();
  let output = Buffer.contents buf in
  check bool "output not empty" true (String.length output > 0)

(* ============================================================
   create Tests
   ============================================================ *)

let test_create_default () =
  let t = Sctp_transport.create () in
  let stats = Sctp_transport.get_stats t in
  check int "initial in_flight_bytes" 0 stats.in_flight_bytes;
  check int "initial in_flight_count" 0 stats.in_flight_count

let test_create_custom_config () =
  let cfg : Sctp_transport.config = {
    initial_cwnd = 32768;
    max_cwnd = 262144;
    initial_rto = 0.5;
    min_rto = 0.05;
    max_rto = 5.0;
    max_burst = 2;
  } in
  let t = Sctp_transport.create ~config:cfg () in
  let stats = Sctp_transport.get_stats t in
  check int "cwnd from config" 32768 stats.cwnd

(* ============================================================
   can_send and available_window Tests
   ============================================================ *)

let test_can_send_initial () =
  let t = Sctp_transport.create () in
  check bool "can send initially" true (Sctp_transport.can_send t)

let test_available_window_initial () =
  let t = Sctp_transport.create () in
  let window = Sctp_transport.available_window t in
  check bool "positive window" true (window > 0)

(* NOTE: send, process_ack, handle_timeout, flush_buffer require Eio context
   and cannot be tested without Eio effect handlers installed. *)

(* ============================================================
   Config Validation Tests (semantic)
   ============================================================ *)

let test_cwnd_ordering () =
  let cfg = Sctp_transport.default_config in
  check bool "initial < max" true (cfg.initial_cwnd < cfg.max_cwnd)

let test_rto_ordering () =
  let cfg = Sctp_transport.default_config in
  check bool "min < initial" true (cfg.min_rto < cfg.initial_rto);
  check bool "initial < max" true (cfg.initial_rto < cfg.max_rto)

let test_reasonable_burst () =
  let cfg = Sctp_transport.default_config in
  check bool "burst > 0" true (cfg.max_burst > 0);
  check bool "burst < 100" true (cfg.max_burst < 100)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "SCTP Transport Coverage" [
    "config", [
      test_case "type" `Quick test_config_type;
      test_case "rto" `Quick test_config_rto;
    ];
    "default_config", [
      test_case "initial_cwnd" `Quick test_default_initial_cwnd;
      test_case "max_cwnd" `Quick test_default_max_cwnd;
      test_case "initial_rto" `Quick test_default_initial_rto;
      test_case "min_rto" `Quick test_default_min_rto;
      test_case "max_rto" `Quick test_default_max_rto;
      test_case "max_burst" `Quick test_default_max_burst;
    ];
    "stats", [
      test_case "type" `Quick test_stats_type;
      test_case "to_json" `Quick test_stats_to_json;
      test_case "pp_stats" `Quick test_pp_stats;
    ];
    "create", [
      test_case "default" `Quick test_create_default;
      test_case "custom config" `Quick test_create_custom_config;
    ];
    "can_send", [
      test_case "initial" `Quick test_can_send_initial;
    ];
    "available_window", [
      test_case "initial" `Quick test_available_window_initial;
    ];
    (* NOTE: send, process_ack, handle_timeout, flush_buffer tests removed - require Eio context *)
    "validation", [
      test_case "cwnd ordering" `Quick test_cwnd_ordering;
      test_case "rto ordering" `Quick test_rto_ordering;
      test_case "reasonable burst" `Quick test_reasonable_burst;
    ];
  ]
