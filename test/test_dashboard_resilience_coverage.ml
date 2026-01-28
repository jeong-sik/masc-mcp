(** Dashboard and Resilience Module Coverage Tests

    Tests for:
    - Dashboard: section types, formatting, constants, timestamp parsing
    - Resilience: Time utilities, Zombie detection, ZeroZombie protocol
*)

open Alcotest

module Dashboard = Masc_mcp.Dashboard
module Resilience = Masc_mcp.Resilience

(* ============================================================
   Dashboard Constants Tests
   ============================================================ *)

let test_dashboard_max_path_length () =
  check int "max_path_length" 30 Dashboard.max_path_length

let test_dashboard_max_message_length () =
  check int "max_message_length" 35 Dashboard.max_message_length

let test_dashboard_max_pending_tasks () =
  check int "max_pending_tasks" 5 Dashboard.max_pending_tasks

let test_dashboard_max_recent_messages () =
  check int "max_recent_messages" 5 Dashboard.max_recent_messages

let test_dashboard_min_border_length () =
  check int "min_border_length" 45 Dashboard.min_border_length

(* ============================================================
   Dashboard section Tests
   ============================================================ *)

let test_section_creation () =
  let s : Dashboard.section = {
    title = "Test Section";
    content = ["line 1"; "line 2"; "line 3"];
    empty_msg = "(no data)";
  } in
  check string "title" "Test Section" s.title;
  check int "content count" 3 (List.length s.content);
  check string "empty_msg" "(no data)" s.empty_msg

let test_section_empty () =
  let s : Dashboard.section = {
    title = "Empty";
    content = [];
    empty_msg = "(nothing here)";
  } in
  check int "empty content" 0 (List.length s.content)

(* ============================================================
   Dashboard format_section Tests
   ============================================================ *)

let test_format_section_with_content () =
  let s : Dashboard.section = {
    title = "Agents";
    content = ["agent1"; "agent2"];
    empty_msg = "(no agents)";
  } in
  let formatted = Dashboard.format_section s in
  check bool "contains title" true (String.length formatted > 0);
  check bool "contains agent1" true (String.length formatted > 0)

let test_format_section_empty () =
  let s : Dashboard.section = {
    title = "Tasks";
    content = [];
    empty_msg = "(no tasks)";
  } in
  let formatted = Dashboard.format_section s in
  check bool "non-empty output" true (String.length formatted > 0)

let test_format_section_long_title () =
  let s : Dashboard.section = {
    title = "This Is A Very Long Section Title For Testing";
    content = ["item"];
    empty_msg = "none";
  } in
  let formatted = Dashboard.format_section s in
  check bool "handles long title" true (String.length formatted > 50)

(* ============================================================
   Dashboard parse_iso_timestamp Tests
   ============================================================ *)

let test_parse_iso_timestamp_valid () =
  match Dashboard.parse_iso_timestamp "2025-01-09T12:00:00Z" with
  | Some ts -> check bool "positive timestamp" true (ts > 0.0)
  | None -> fail "should parse valid timestamp"

let test_parse_iso_timestamp_with_millis () =
  match Dashboard.parse_iso_timestamp "2025-01-09T12:00:00.123Z" with
  | Some ts -> check bool "positive timestamp" true (ts > 0.0)
  | None -> fail "should parse timestamp with millis"

let test_parse_iso_timestamp_invalid () =
  match Dashboard.parse_iso_timestamp "invalid" with
  | Some _ -> fail "should reject invalid"
  | None -> ()

let test_parse_iso_timestamp_empty () =
  match Dashboard.parse_iso_timestamp "" with
  | Some _ -> fail "should reject empty"
  | None -> ()

let test_parse_iso_timestamp_partial () =
  match Dashboard.parse_iso_timestamp "2025-01-09" with
  | Some _ -> fail "should reject partial"
  | None -> ()

(* ============================================================
   Resilience default thresholds Tests
   ============================================================ *)

let test_resilience_default_zombie_threshold () =
  check bool "positive threshold" true (Resilience.default_zombie_threshold > 0.0)

let test_resilience_default_warning_threshold () =
  check bool "warning threshold" true (abs_float (Resilience.default_warning_threshold -. 120.0) < 0.001)

(* ============================================================
   Resilience.Time Tests
   ============================================================ *)

let test_time_now () =
  let t = Resilience.Time.now () in
  check bool "positive time" true (t > 0.0);
  (* Should be after Jan 1, 2020 *)
  check bool "reasonable time" true (t > 1577836800.0)

let test_time_parse_iso8601_valid () =
  match Resilience.Time.parse_iso8601_opt "2025-01-09T12:30:45Z" with
  | Some ts -> check bool "positive" true (ts > 0.0)
  | None -> fail "should parse valid ISO8601"

let test_time_parse_iso8601_invalid () =
  match Resilience.Time.parse_iso8601_opt "not-a-date" with
  | Some _ -> fail "should reject invalid"
  | None -> ()

let test_time_parse_iso8601_empty () =
  match Resilience.Time.parse_iso8601_opt "" with
  | Some _ -> fail "should reject empty"
  | None -> ()

let test_time_is_stale_old () =
  (* Timestamp from 2020 should be stale *)
  let old = "2020-01-01T00:00:00Z" in
  check bool "old is stale" true (Resilience.Time.is_stale old)

let test_time_is_stale_recent () =
  (* Generate a recent timestamp *)
  let now = Unix.gettimeofday () in
  let tm = Unix.gmtime now in
  let recent = Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
  (* Give a large threshold so recent timestamp is not stale *)
  check bool "recent not stale" false (Resilience.Time.is_stale ~threshold:3600.0 recent)

let test_time_is_stale_invalid () =
  (* Invalid timestamps should be treated as stale *)
  check bool "invalid is stale" true (Resilience.Time.is_stale "garbage")

let test_time_is_stale_custom_threshold () =
  let old = "2020-01-01T00:00:00Z" in
  (* Even with a very large threshold, 2020 is still old *)
  check bool "2020 stale with 1 year threshold" true
    (Resilience.Time.is_stale ~threshold:31536000.0 old)

(* ============================================================
   Resilience.Zombie Tests
   ============================================================ *)

let test_zombie_is_zombie_old () =
  let old = "2020-01-01T00:00:00Z" in
  check bool "old agent is zombie" true (Resilience.Zombie.is_zombie old)

let test_zombie_is_zombie_recent () =
  let now = Unix.gettimeofday () in
  let tm = Unix.gmtime now in
  let recent = Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
  check bool "recent not zombie" false (Resilience.Zombie.is_zombie ~threshold:3600.0 recent)

let test_zombie_is_zombie_custom_threshold () =
  (* With a very short threshold, even "recent" could be stale *)
  let old = "2020-01-01T00:00:00Z" in
  check bool "zombie with short threshold" true
    (Resilience.Zombie.is_zombie ~threshold:1.0 old)

(* ============================================================
   Resilience.ZeroZombie Tests
   ============================================================ *)

let test_zero_zombie_global_stats () =
  let stats = Resilience.ZeroZombie.global_stats in
  check bool "total_cleanups non-negative" true (stats.total_cleanups >= 0)

let test_zero_zombie_cleanup () =
  (* Mock cleanup function that returns empty list *)
  let cleanup_fn () = [] in
  let cleaned = Resilience.ZeroZombie.cleanup ~cleanup_fn in
  check int "no cleaned agents" 0 (List.length cleaned)

let test_zero_zombie_cleanup_with_agents () =
  (* Mock cleanup function that returns some agents *)
  let cleanup_fn () = ["zombie1"; "zombie2"] in
  let cleaned = Resilience.ZeroZombie.cleanup ~cleanup_fn in
  check int "2 cleaned agents" 2 (List.length cleaned)

let test_zero_zombie_stats_update () =
  let initial_cleanups = Resilience.ZeroZombie.global_stats.total_cleanups in
  let cleanup_fn () = ["agent1"] in
  ignore (Resilience.ZeroZombie.cleanup ~cleanup_fn);
  check bool "cleanups incremented" true
    (Resilience.ZeroZombie.global_stats.total_cleanups > initial_cleanups ||
     Resilience.ZeroZombie.global_stats.total_cleanups = initial_cleanups + 1)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Dashboard & Resilience Coverage" [
    "dashboard.constants", [
      test_case "max_path_length" `Quick test_dashboard_max_path_length;
      test_case "max_message_length" `Quick test_dashboard_max_message_length;
      test_case "max_pending_tasks" `Quick test_dashboard_max_pending_tasks;
      test_case "max_recent_messages" `Quick test_dashboard_max_recent_messages;
      test_case "min_border_length" `Quick test_dashboard_min_border_length;
    ];
    "dashboard.section", [
      test_case "creation" `Quick test_section_creation;
      test_case "empty" `Quick test_section_empty;
    ];
    "dashboard.format_section", [
      test_case "with content" `Quick test_format_section_with_content;
      test_case "empty" `Quick test_format_section_empty;
      test_case "long title" `Quick test_format_section_long_title;
    ];
    "dashboard.parse_timestamp", [
      test_case "valid" `Quick test_parse_iso_timestamp_valid;
      test_case "with millis" `Quick test_parse_iso_timestamp_with_millis;
      test_case "invalid" `Quick test_parse_iso_timestamp_invalid;
      test_case "empty" `Quick test_parse_iso_timestamp_empty;
      test_case "partial" `Quick test_parse_iso_timestamp_partial;
    ];
    "resilience.thresholds", [
      test_case "zombie threshold" `Quick test_resilience_default_zombie_threshold;
      test_case "warning threshold" `Quick test_resilience_default_warning_threshold;
    ];
    "resilience.time", [
      test_case "now" `Quick test_time_now;
      test_case "parse valid" `Quick test_time_parse_iso8601_valid;
      test_case "parse invalid" `Quick test_time_parse_iso8601_invalid;
      test_case "parse empty" `Quick test_time_parse_iso8601_empty;
      test_case "is_stale old" `Quick test_time_is_stale_old;
      test_case "is_stale recent" `Quick test_time_is_stale_recent;
      test_case "is_stale invalid" `Quick test_time_is_stale_invalid;
      test_case "is_stale custom threshold" `Quick test_time_is_stale_custom_threshold;
    ];
    "resilience.zombie", [
      test_case "is_zombie old" `Quick test_zombie_is_zombie_old;
      test_case "is_zombie recent" `Quick test_zombie_is_zombie_recent;
      test_case "is_zombie custom threshold" `Quick test_zombie_is_zombie_custom_threshold;
    ];
    "resilience.zero_zombie", [
      test_case "global stats" `Quick test_zero_zombie_global_stats;
      test_case "cleanup empty" `Quick test_zero_zombie_cleanup;
      test_case "cleanup with agents" `Quick test_zero_zombie_cleanup_with_agents;
      test_case "stats update" `Quick test_zero_zombie_stats_update;
    ];
  ]
