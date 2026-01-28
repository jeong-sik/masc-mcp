(** WebRTC Bindings Module Coverage Tests

    Tests for native WebRTC FFI constants:
    - error codes
    - rtcState values
    - rtcIceState values
    - rtcGatheringState values
    - rtcLogLevel values
*)

open Alcotest

module Webrtc_bindings = Masc_mcp.Webrtc_bindings

(* ============================================================
   Error Code Constants Tests
   ============================================================ *)

let test_rtc_err_success () =
  check int "success" 0 Webrtc_bindings.rtc_err_success

let test_rtc_err_invalid () =
  check int "invalid" (-1) Webrtc_bindings.rtc_err_invalid

let test_rtc_err_failure () =
  check int "failure" (-2) Webrtc_bindings.rtc_err_failure

(* ============================================================
   rtcState Constants Tests
   ============================================================ *)

let test_rtc_new () =
  check int "new" 0 Webrtc_bindings.rtc_new

let test_rtc_connecting () =
  check int "connecting" 1 Webrtc_bindings.rtc_connecting

let test_rtc_connected () =
  check int "connected" 2 Webrtc_bindings.rtc_connected

let test_rtc_disconnected () =
  check int "disconnected" 3 Webrtc_bindings.rtc_disconnected

let test_rtc_failed () =
  check int "failed" 4 Webrtc_bindings.rtc_failed

let test_rtc_closed () =
  check int "closed" 5 Webrtc_bindings.rtc_closed

(* ============================================================
   rtcIceState Constants Tests
   ============================================================ *)

let test_rtc_ice_new () =
  check int "ice new" 0 Webrtc_bindings.rtc_ice_new

let test_rtc_ice_checking () =
  check int "ice checking" 1 Webrtc_bindings.rtc_ice_checking

let test_rtc_ice_connected () =
  check int "ice connected" 2 Webrtc_bindings.rtc_ice_connected

let test_rtc_ice_completed () =
  check int "ice completed" 3 Webrtc_bindings.rtc_ice_completed

let test_rtc_ice_failed () =
  check int "ice failed" 4 Webrtc_bindings.rtc_ice_failed

let test_rtc_ice_disconnected () =
  check int "ice disconnected" 5 Webrtc_bindings.rtc_ice_disconnected

let test_rtc_ice_closed () =
  check int "ice closed" 6 Webrtc_bindings.rtc_ice_closed

(* ============================================================
   rtcGatheringState Constants Tests
   ============================================================ *)

let test_rtc_gathering_new () =
  check int "gathering new" 0 Webrtc_bindings.rtc_gathering_new

let test_rtc_gathering_inprogress () =
  check int "gathering in progress" 1 Webrtc_bindings.rtc_gathering_inprogress

let test_rtc_gathering_complete () =
  check int "gathering complete" 2 Webrtc_bindings.rtc_gathering_complete

(* ============================================================
   rtcLogLevel Constants Tests
   ============================================================ *)

let test_rtc_log_none () =
  check int "log none" 0 Webrtc_bindings.rtc_log_none

let test_rtc_log_fatal () =
  check int "log fatal" 1 Webrtc_bindings.rtc_log_fatal

let test_rtc_log_error () =
  check int "log error" 2 Webrtc_bindings.rtc_log_error

let test_rtc_log_warning () =
  check int "log warning" 3 Webrtc_bindings.rtc_log_warning

let test_rtc_log_info () =
  check int "log info" 4 Webrtc_bindings.rtc_log_info

let test_rtc_log_debug () =
  check int "log debug" 5 Webrtc_bindings.rtc_log_debug

let test_rtc_log_verbose () =
  check int "log verbose" 6 Webrtc_bindings.rtc_log_verbose

(* ============================================================
   State Ordering Tests (semantic checks)
   ============================================================ *)

let test_state_ordering () =
  check bool "new < connecting" true
    (Webrtc_bindings.rtc_new < Webrtc_bindings.rtc_connecting);
  check bool "connecting < connected" true
    (Webrtc_bindings.rtc_connecting < Webrtc_bindings.rtc_connected)

let test_log_level_ordering () =
  check bool "none < fatal" true
    (Webrtc_bindings.rtc_log_none < Webrtc_bindings.rtc_log_fatal);
  check bool "fatal < error" true
    (Webrtc_bindings.rtc_log_fatal < Webrtc_bindings.rtc_log_error);
  check bool "error < warning" true
    (Webrtc_bindings.rtc_log_error < Webrtc_bindings.rtc_log_warning)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "WebRTC Bindings Coverage" [
    "error_codes", [
      test_case "success" `Quick test_rtc_err_success;
      test_case "invalid" `Quick test_rtc_err_invalid;
      test_case "failure" `Quick test_rtc_err_failure;
    ];
    "rtc_state", [
      test_case "new" `Quick test_rtc_new;
      test_case "connecting" `Quick test_rtc_connecting;
      test_case "connected" `Quick test_rtc_connected;
      test_case "disconnected" `Quick test_rtc_disconnected;
      test_case "failed" `Quick test_rtc_failed;
      test_case "closed" `Quick test_rtc_closed;
    ];
    "rtc_ice_state", [
      test_case "new" `Quick test_rtc_ice_new;
      test_case "checking" `Quick test_rtc_ice_checking;
      test_case "connected" `Quick test_rtc_ice_connected;
      test_case "completed" `Quick test_rtc_ice_completed;
      test_case "failed" `Quick test_rtc_ice_failed;
      test_case "disconnected" `Quick test_rtc_ice_disconnected;
      test_case "closed" `Quick test_rtc_ice_closed;
    ];
    "rtc_gathering_state", [
      test_case "new" `Quick test_rtc_gathering_new;
      test_case "inprogress" `Quick test_rtc_gathering_inprogress;
      test_case "complete" `Quick test_rtc_gathering_complete;
    ];
    "rtc_log_level", [
      test_case "none" `Quick test_rtc_log_none;
      test_case "fatal" `Quick test_rtc_log_fatal;
      test_case "error" `Quick test_rtc_log_error;
      test_case "warning" `Quick test_rtc_log_warning;
      test_case "info" `Quick test_rtc_log_info;
      test_case "debug" `Quick test_rtc_log_debug;
      test_case "verbose" `Quick test_rtc_log_verbose;
    ];
    "ordering", [
      test_case "state" `Quick test_state_ordering;
      test_case "log_level" `Quick test_log_level_ordering;
    ];
  ]
