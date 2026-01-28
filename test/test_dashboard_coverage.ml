(** Dashboard Module Coverage Tests

    Tests for MASC Dashboard - Terminal-based Status Visualization:
    - Constants: max_path_length, max_message_length, etc.
    - section type and format_section
    - parse_iso_timestamp
*)

open Alcotest

module Dashboard = Masc_mcp.Dashboard

(* ============================================================
   Constants Tests
   ============================================================ *)

let test_max_path_length () =
  check bool "positive" true (Dashboard.max_path_length > 0)

let test_max_message_length () =
  check bool "positive" true (Dashboard.max_message_length > 0)

let test_max_pending_tasks () =
  check bool "positive" true (Dashboard.max_pending_tasks > 0)

let test_max_recent_messages () =
  check bool "positive" true (Dashboard.max_recent_messages > 0)

let test_min_border_length () =
  check bool "positive" true (Dashboard.min_border_length > 0)

(* ============================================================
   section Tests
   ============================================================ *)

let test_section_type () =
  let s : Dashboard.section = {
    title = "Test Section";
    content = ["line1"; "line2"];
    empty_msg = "No items";
  } in
  check string "title" "Test Section" s.title;
  check int "content length" 2 (List.length s.content)

let test_section_empty () =
  let s : Dashboard.section = {
    title = "Empty";
    content = [];
    empty_msg = "Nothing here";
  } in
  check int "empty content" 0 (List.length s.content)

(* ============================================================
   format_section Tests
   ============================================================ *)

let test_format_section_with_content () =
  let s : Dashboard.section = {
    title = "Agents";
    content = ["[active] claude"; "[busy] gemini"];
    empty_msg = "(no agents)";
  } in
  let result = Dashboard.format_section s in
  check bool "contains title" true
    (try let _ = Str.search_forward (Str.regexp_string "Agents") result 0 in true
     with Not_found -> false);
  check bool "contains content" true
    (try let _ = Str.search_forward (Str.regexp_string "claude") result 0 in true
     with Not_found -> false)

let test_format_section_empty_shows_msg () =
  let s : Dashboard.section = {
    title = "Tasks";
    content = [];
    empty_msg = "(no tasks)";
  } in
  let result = Dashboard.format_section s in
  check bool "contains empty msg" true
    (try let _ = Str.search_forward (Str.regexp_string "no tasks") result 0 in true
     with Not_found -> false)

let test_format_section_has_border () =
  let s : Dashboard.section = {
    title = "Test";
    content = ["line"];
    empty_msg = "";
  } in
  let result = Dashboard.format_section s in
  check bool "has equals" true
    (try let _ = Str.search_forward (Str.regexp_string "==") result 0 in true
     with Not_found -> false)

(* ============================================================
   parse_iso_timestamp Tests
   ============================================================ *)

let test_parse_iso_timestamp_valid () =
  match Dashboard.parse_iso_timestamp "2024-01-15T12:30:45Z" with
  | Some ts -> check bool "is positive" true (ts > 0.0)
  | None -> fail "expected Some"

let test_parse_iso_timestamp_another () =
  match Dashboard.parse_iso_timestamp "2025-06-20T08:15:30Z" with
  | Some ts -> check bool "is positive" true (ts > 0.0)
  | None -> fail "expected Some"

let test_parse_iso_timestamp_invalid () =
  match Dashboard.parse_iso_timestamp "not-a-timestamp" with
  | None -> check bool "none" true true
  | Some _ -> fail "expected None"

let test_parse_iso_timestamp_empty () =
  match Dashboard.parse_iso_timestamp "" with
  | None -> check bool "none" true true
  | Some _ -> fail "expected None"

let test_parse_iso_timestamp_partial () =
  match Dashboard.parse_iso_timestamp "2024-01-15" with
  | None -> check bool "none" true true
  | Some _ -> fail "expected None for partial"

(* ============================================================
   truncate_path Tests
   ============================================================ *)

let test_truncate_path_short () =
  let short = "/tmp/test" in
  let result = Dashboard.truncate_path short in
  check string "unchanged" short result

let test_truncate_path_exact () =
  (* Create a path that's exactly max_path_length *)
  let exact = String.make Dashboard.max_path_length 'x' in
  let result = Dashboard.truncate_path exact in
  check string "unchanged exact" exact result

let test_truncate_path_long () =
  (* Create a path longer than max_path_length *)
  let long = String.make (Dashboard.max_path_length + 20) 'x' in
  let result = Dashboard.truncate_path long in
  check int "truncated length" Dashboard.max_path_length (String.length result);
  check bool "starts with ellipsis" true
    (String.length result >= 3 && String.sub result 0 3 = "...")

let test_truncate_path_preserves_suffix () =
  let long = "/very/long/path/that/exceeds/max/length/file.txt" in
  let result = Dashboard.truncate_path long in
  check bool "ends with file.txt" true
    (let len = String.length result in
     let suffix = ".txt" in
     len >= 4 && String.sub result (len - 4) 4 = suffix)

(* ============================================================
   truncate_message Tests
   ============================================================ *)

let test_truncate_message_short () =
  let short = "Hello" in
  let result = Dashboard.truncate_message short in
  check string "unchanged" short result

let test_truncate_message_exact () =
  let exact = String.make Dashboard.max_message_length 'y' in
  let result = Dashboard.truncate_message exact in
  check string "unchanged exact" exact result

let test_truncate_message_long () =
  let long = String.make (Dashboard.max_message_length + 10) 'z' in
  let result = Dashboard.truncate_message long in
  check int "truncated length" Dashboard.max_message_length (String.length result);
  check bool "ends with ellipsis" true
    (let len = String.length result in
     len >= 3 && String.sub result (len - 3) 3 = "...")

let test_truncate_message_preserves_prefix () =
  let prefix = "Important: " in
  let long = prefix ^ String.make (Dashboard.max_message_length + 10) 'x' in
  let result = Dashboard.truncate_message long in
  check bool "starts with prefix" true
    (String.length result >= String.length prefix &&
     String.sub result 0 (String.length prefix) = prefix)

(* ============================================================
   ends_with Tests
   ============================================================ *)

let test_ends_with_true () =
  check bool "ends with .txt" true (Dashboard.ends_with ~suffix:".txt" "file.txt")

let test_ends_with_false () =
  check bool "not ends with .txt" false (Dashboard.ends_with ~suffix:".txt" "file.json")

let test_ends_with_empty_suffix () =
  check bool "empty suffix" true (Dashboard.ends_with ~suffix:"" "anything")

let test_ends_with_empty_string () =
  check bool "empty string, non-empty suffix" false (Dashboard.ends_with ~suffix:".txt" "")

let test_ends_with_equal () =
  check bool "equal strings" true (Dashboard.ends_with ~suffix:"test" "test")

let test_ends_with_longer_suffix () =
  check bool "suffix longer than string" false
    (Dashboard.ends_with ~suffix:"verylongsuffix" "short")

let test_ends_with_flock () =
  check bool "ends with .flock" true (Dashboard.ends_with ~suffix:".flock" "agent.flock")

(* ============================================================
   parse_worktrees Tests
   ============================================================ *)

let test_parse_worktrees_empty_list () =
  let json = `Assoc [("worktrees", `List [])] in
  let result = Dashboard.parse_worktrees json in
  check int "empty list" 0 (List.length result)

let test_parse_worktrees_null () =
  let json = `Assoc [("worktrees", `Null)] in
  let result = Dashboard.parse_worktrees json in
  check int "null worktrees" 0 (List.length result)

let test_parse_worktrees_missing () =
  let json = `Assoc [] in
  let result = Dashboard.parse_worktrees json in
  check int "missing worktrees" 0 (List.length result)

let test_parse_worktrees_valid () =
  let json = `Assoc [
    ("worktrees", `List [
      `Assoc [("worktree", `String "/path/to/wt"); ("branch", `String "feature-x")];
      `Assoc [("worktree", `String "/path/to/wt2"); ("branch", `String "bugfix")];
    ])
  ] in
  let result = Dashboard.parse_worktrees json in
  check int "two worktrees" 2 (List.length result)

let test_parse_worktrees_skips_head () =
  let json = `Assoc [
    ("worktrees", `List [
      `Assoc [("worktree", `String "/repo"); ("branch", `String "HEAD")];
      `Assoc [("worktree", `String "/wt"); ("branch", `String "main")];
    ])
  ] in
  let result = Dashboard.parse_worktrees json in
  check int "skips HEAD" 1 (List.length result)

let test_parse_worktrees_skips_empty_branch () =
  let json = `Assoc [
    ("worktrees", `List [
      `Assoc [("worktree", `String "/wt"); ("branch", `String "")];
      `Assoc [("worktree", `String "/wt2"); ("branch", `String "valid")];
    ])
  ] in
  let result = Dashboard.parse_worktrees json in
  check int "skips empty branch" 1 (List.length result)

let test_parse_worktrees_malformed_item () =
  let json = `Assoc [
    ("worktrees", `List [
      `String "not an object";
      `Assoc [("worktree", `String "/wt"); ("branch", `String "ok")];
    ])
  ] in
  let result = Dashboard.parse_worktrees json in
  check int "skips malformed" 1 (List.length result)

let test_parse_worktrees_missing_branch () =
  let json = `Assoc [
    ("worktrees", `List [
      `Assoc [("worktree", `String "/wt")];
      `Assoc [("worktree", `String "/wt2"); ("branch", `String "valid")];
    ])
  ] in
  let result = Dashboard.parse_worktrees json in
  check int "skips missing branch" 1 (List.length result)

let test_parse_worktrees_unexpected_structure () =
  (* The function throws Type_error on non-object JSON at top level *)
  let json = `String "just a string" in
  try
    let _result = Dashboard.parse_worktrees json in
    fail "expected Type_error"
  with
  | Yojson.Safe.Util.Type_error _ -> check bool "raises Type_error" true true
  | _ -> fail "unexpected exception"

(* ============================================================
   count_lock_files Tests (pure recursive function)
   ============================================================ *)

let test_count_lock_files_nonexistent () =
  let count = Dashboard.count_lock_files "/nonexistent/path/xyz" in
  check int "zero for nonexistent" 0 count

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Dashboard Coverage" [
    "constants", [
      test_case "max_path_length" `Quick test_max_path_length;
      test_case "max_message_length" `Quick test_max_message_length;
      test_case "max_pending_tasks" `Quick test_max_pending_tasks;
      test_case "max_recent_messages" `Quick test_max_recent_messages;
      test_case "min_border_length" `Quick test_min_border_length;
    ];
    "section_type", [
      test_case "basic" `Quick test_section_type;
      test_case "empty" `Quick test_section_empty;
    ];
    "format_section", [
      test_case "with content" `Quick test_format_section_with_content;
      test_case "empty shows msg" `Quick test_format_section_empty_shows_msg;
      test_case "has border" `Quick test_format_section_has_border;
    ];
    "parse_iso_timestamp", [
      test_case "valid" `Quick test_parse_iso_timestamp_valid;
      test_case "another valid" `Quick test_parse_iso_timestamp_another;
      test_case "invalid" `Quick test_parse_iso_timestamp_invalid;
      test_case "empty" `Quick test_parse_iso_timestamp_empty;
      test_case "partial" `Quick test_parse_iso_timestamp_partial;
    ];
    "truncate_path", [
      test_case "short" `Quick test_truncate_path_short;
      test_case "exact" `Quick test_truncate_path_exact;
      test_case "long" `Quick test_truncate_path_long;
      test_case "preserves suffix" `Quick test_truncate_path_preserves_suffix;
    ];
    "truncate_message", [
      test_case "short" `Quick test_truncate_message_short;
      test_case "exact" `Quick test_truncate_message_exact;
      test_case "long" `Quick test_truncate_message_long;
      test_case "preserves prefix" `Quick test_truncate_message_preserves_prefix;
    ];
    "ends_with", [
      test_case "true" `Quick test_ends_with_true;
      test_case "false" `Quick test_ends_with_false;
      test_case "empty suffix" `Quick test_ends_with_empty_suffix;
      test_case "empty string" `Quick test_ends_with_empty_string;
      test_case "equal" `Quick test_ends_with_equal;
      test_case "longer suffix" `Quick test_ends_with_longer_suffix;
      test_case "flock" `Quick test_ends_with_flock;
    ];
    "parse_worktrees", [
      test_case "empty list" `Quick test_parse_worktrees_empty_list;
      test_case "null" `Quick test_parse_worktrees_null;
      test_case "missing" `Quick test_parse_worktrees_missing;
      test_case "valid" `Quick test_parse_worktrees_valid;
      test_case "skips HEAD" `Quick test_parse_worktrees_skips_head;
      test_case "skips empty branch" `Quick test_parse_worktrees_skips_empty_branch;
      test_case "malformed item" `Quick test_parse_worktrees_malformed_item;
      test_case "missing branch" `Quick test_parse_worktrees_missing_branch;
      test_case "unexpected structure" `Quick test_parse_worktrees_unexpected_structure;
    ];
    "count_lock_files", [
      test_case "nonexistent" `Quick test_count_lock_files_nonexistent;
    ];
  ]
