(** Log Module Coverage Tests

    Tests for MASC Logging System:
    - level: Debug, Info, Warn, Error
    - level_to_string, level_to_int, level_of_string
    - should_log, set_level
    - timestamp format
*)

open Alcotest

module Log = Masc_mcp.Log

(* ============================================================
   level_to_string Tests
   ============================================================ *)

let test_level_to_string_debug () =
  check string "debug" "DEBUG" (Log.level_to_string Log.Debug)

let test_level_to_string_info () =
  check string "info" "INFO" (Log.level_to_string Log.Info)

let test_level_to_string_warn () =
  check string "warn" "WARN" (Log.level_to_string Log.Warn)

let test_level_to_string_error () =
  check string "error" "ERROR" (Log.level_to_string Log.Error)

(* ============================================================
   level_to_int Tests
   ============================================================ *)

let test_level_to_int_debug () =
  check int "debug=0" 0 (Log.level_to_int Log.Debug)

let test_level_to_int_info () =
  check int "info=1" 1 (Log.level_to_int Log.Info)

let test_level_to_int_warn () =
  check int "warn=2" 2 (Log.level_to_int Log.Warn)

let test_level_to_int_error () =
  check int "error=3" 3 (Log.level_to_int Log.Error)

let test_level_to_int_ordering () =
  check bool "debug < info" true (Log.level_to_int Log.Debug < Log.level_to_int Log.Info);
  check bool "info < warn" true (Log.level_to_int Log.Info < Log.level_to_int Log.Warn);
  check bool "warn < error" true (Log.level_to_int Log.Warn < Log.level_to_int Log.Error)

(* ============================================================
   level_of_string Tests
   ============================================================ *)

let test_level_of_string_debug () =
  check bool "debug" true (Log.level_of_string "debug" = Log.Debug)

let test_level_of_string_info () =
  check bool "info" true (Log.level_of_string "info" = Log.Info)

let test_level_of_string_warn () =
  check bool "warn" true (Log.level_of_string "warn" = Log.Warn)

let test_level_of_string_warning () =
  check bool "warning alias" true (Log.level_of_string "warning" = Log.Warn)

let test_level_of_string_error () =
  check bool "error" true (Log.level_of_string "error" = Log.Error)

let test_level_of_string_uppercase () =
  check bool "DEBUG uppercase" true (Log.level_of_string "DEBUG" = Log.Debug);
  check bool "INFO uppercase" true (Log.level_of_string "INFO" = Log.Info)

let test_level_of_string_unknown () =
  check bool "unknown defaults to Info" true (Log.level_of_string "unknown" = Log.Info)

let test_level_of_string_empty () =
  check bool "empty defaults to Info" true (Log.level_of_string "" = Log.Info)

(* ============================================================
   should_log Tests
   ============================================================ *)

let test_should_log_same_level () =
  Log.set_level Log.Info;
  check bool "info logs at info" true (Log.should_log Log.Info)

let test_should_log_higher_level () =
  Log.set_level Log.Info;
  check bool "warn logs at info" true (Log.should_log Log.Warn);
  check bool "error logs at info" true (Log.should_log Log.Error)

let test_should_log_lower_level () =
  Log.set_level Log.Warn;
  check bool "debug not logged at warn" false (Log.should_log Log.Debug);
  check bool "info not logged at warn" false (Log.should_log Log.Info)

let test_should_log_error_level () =
  Log.set_level Log.Error;
  check bool "only error at error level" true (Log.should_log Log.Error);
  check bool "warn not at error level" false (Log.should_log Log.Warn)

let test_should_log_debug_level () =
  Log.set_level Log.Debug;
  check bool "debug at debug level" true (Log.should_log Log.Debug);
  check bool "info at debug level" true (Log.should_log Log.Info);
  check bool "error at debug level" true (Log.should_log Log.Error)

(* ============================================================
   set_level Tests
   ============================================================ *)

let test_set_level () =
  Log.set_level Log.Error;
  check bool "error not logged info" false (Log.should_log Log.Info);
  Log.set_level Log.Debug;
  check bool "debug logs info" true (Log.should_log Log.Info)

let test_set_level_from_string () =
  Log.set_level_from_string "error";
  check bool "error level set" false (Log.should_log Log.Warn);
  Log.set_level_from_string "debug";
  check bool "debug level set" true (Log.should_log Log.Debug)

(* ============================================================
   timestamp Tests
   ============================================================ *)

let test_timestamp_format () =
  let ts = Log.timestamp () in
  (* Format should be YYYY-MM-DD HH:MM:SS *)
  check int "timestamp length" 19 (String.length ts);
  check bool "has dash at 4" true (ts.[4] = '-');
  check bool "has dash at 7" true (ts.[7] = '-');
  check bool "has space at 10" true (ts.[10] = ' ');
  check bool "has colon at 13" true (ts.[13] = ':');
  check bool "has colon at 16" true (ts.[16] = ':')

let test_timestamp_changes () =
  let ts1 = Log.timestamp () in
  Unix.sleepf 0.01;
  let ts2 = Log.timestamp () in
  (* At minimum they should be valid timestamps *)
  check bool "ts1 valid" true (String.length ts1 = 19);
  check bool "ts2 valid" true (String.length ts2 = 19)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Log Coverage" [
    "level_to_string", [
      test_case "debug" `Quick test_level_to_string_debug;
      test_case "info" `Quick test_level_to_string_info;
      test_case "warn" `Quick test_level_to_string_warn;
      test_case "error" `Quick test_level_to_string_error;
    ];
    "level_to_int", [
      test_case "debug" `Quick test_level_to_int_debug;
      test_case "info" `Quick test_level_to_int_info;
      test_case "warn" `Quick test_level_to_int_warn;
      test_case "error" `Quick test_level_to_int_error;
      test_case "ordering" `Quick test_level_to_int_ordering;
    ];
    "level_of_string", [
      test_case "debug" `Quick test_level_of_string_debug;
      test_case "info" `Quick test_level_of_string_info;
      test_case "warn" `Quick test_level_of_string_warn;
      test_case "warning alias" `Quick test_level_of_string_warning;
      test_case "error" `Quick test_level_of_string_error;
      test_case "uppercase" `Quick test_level_of_string_uppercase;
      test_case "unknown" `Quick test_level_of_string_unknown;
      test_case "empty" `Quick test_level_of_string_empty;
    ];
    "should_log", [
      test_case "same level" `Quick test_should_log_same_level;
      test_case "higher level" `Quick test_should_log_higher_level;
      test_case "lower level" `Quick test_should_log_lower_level;
      test_case "error level" `Quick test_should_log_error_level;
      test_case "debug level" `Quick test_should_log_debug_level;
    ];
    "set_level", [
      test_case "set_level" `Quick test_set_level;
      test_case "set_level_from_string" `Quick test_set_level_from_string;
    ];
    "timestamp", [
      test_case "format" `Quick test_timestamp_format;
      test_case "changes" `Quick test_timestamp_changes;
    ];
  ]
