(** Dashboard Tests using Alcotest *)

module Lib = Masc_mcp

(** Helper to check if str contains substring *)
let contains str substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) str 0);
    true
  with Not_found -> false

let test_dir () =
  let tmp = Filename.temp_file "masc_dashboard_test" "" in
  Sys.remove tmp;
  Unix.mkdir tmp 0o755;
  tmp

let cleanup_dir dir =
  let rec rm path =
    if Sys.is_directory path then begin
      Sys.readdir path |> Array.iter (fun f -> rm (Filename.concat path f));
      Unix.rmdir path
    end else
      Sys.remove path
  in
  if Sys.file_exists dir then rm dir

let setup_room config =
  (* Use Room.init to properly initialize MASC *)
  ignore (Lib.Room.init config ~agent_name:(Some "test-agent"))

(* ===== format_section Tests ===== *)

let test_format_section () =
  let section = Lib.Dashboard.{
    title = "Test Section";
    content = ["Item 1"; "Item 2"; "Item 3"];
    empty_msg = "(empty)";
  } in
  let output = Lib.Dashboard.format_section section in
  Alcotest.(check bool) "has content" true (String.length output > 0);
  Alcotest.(check bool) "contains Item 1" true (contains output "Item 1");
  Alcotest.(check bool) "contains Item 2" true (contains output "Item 2")

let test_format_section_empty () =
  let section = Lib.Dashboard.{
    title = "Empty Section";
    content = [];
    empty_msg = "(nothing here)";
  } in
  let output = Lib.Dashboard.format_section section in
  Alcotest.(check bool) "has empty message" true (contains output "(nothing here)")

(* ===== parse_iso_timestamp Tests ===== *)

let test_parse_timestamp_valid () =
  let valid_ts = "2026-01-09T12:30:45Z" in
  let result = Lib.Dashboard.parse_iso_timestamp valid_ts in
  Alcotest.(check bool) "valid timestamp parses" true (Option.is_some result)

let test_parse_timestamp_invalid () =
  let invalid_ts = "not-a-timestamp" in
  let result = Lib.Dashboard.parse_iso_timestamp invalid_ts in
  Alcotest.(check bool) "invalid timestamp returns None" true (Option.is_none result)

(* ===== generate Tests ===== *)

let test_generate_compact () =
  let dir = test_dir () in
  let config = Lib.Room_utils.default_config dir in
  setup_room config;
  let output = Lib.Dashboard.generate_compact config in
  Alcotest.(check bool) "contains Agents" true (contains output "Agents:");
  Alcotest.(check bool) "contains Tasks" true (contains output "Tasks:");
  Alcotest.(check bool) "contains Locks" true (contains output "Locks:");
  Alcotest.(check bool) "contains Tempo" true (contains output "Tempo:");
  cleanup_dir dir

let test_generate_full () =
  let dir = test_dir () in
  let config = Lib.Room_utils.default_config dir in
  setup_room config;
  let output = Lib.Dashboard.generate config in
  Alcotest.(check bool) "contains MASC Dashboard" true (contains output "MASC Dashboard");
  Alcotest.(check bool) "contains Agents section" true (contains output "Agents");
  Alcotest.(check bool) "contains Tasks section" true (contains output "Tasks");
  Alcotest.(check bool) "contains watch hint" true (contains output "watch");
  cleanup_dir dir

(* ===== Section Tests ===== *)

let test_agents_section_empty () =
  let dir = test_dir () in
  let config = Lib.Room_utils.default_config dir in
  setup_room config;
  let section = Lib.Dashboard.agents_section config in
  Alcotest.(check string) "title" "Agents" section.title;
  Alcotest.(check string) "empty_msg" "(no agents)" section.empty_msg;
  cleanup_dir dir

let test_tasks_section_empty () =
  let dir = test_dir () in
  let config = Lib.Room_utils.default_config dir in
  setup_room config;
  let masc_dir = Filename.concat dir ".masc" in
  let backlog_path = Filename.concat masc_dir "backlog.json" in
  let backlog_json = `Assoc [("tasks", `List [])] in
  let oc = open_out backlog_path in
  output_string oc (Yojson.Safe.to_string backlog_json);
  close_out oc;
  let section = Lib.Dashboard.tasks_section config in
  Alcotest.(check string) "title" "Tasks" section.title;
  Alcotest.(check string) "empty_msg" "(no tasks)" section.empty_msg;
  cleanup_dir dir

let test_locks_section_empty () =
  let dir = test_dir () in
  let config = Lib.Room_utils.default_config dir in
  setup_room config;
  let section = Lib.Dashboard.locks_section config in
  Alcotest.(check string) "title" "File Locks" section.title;
  Alcotest.(check string) "empty_msg" "(no locks)" section.empty_msg;
  cleanup_dir dir

let test_messages_section_empty () =
  let dir = test_dir () in
  let config = Lib.Room_utils.default_config dir in
  setup_room config;
  let section = Lib.Dashboard.messages_section config in
  Alcotest.(check string) "title" "Recent Messages" section.title;
  Alcotest.(check string) "empty_msg" "(no messages)" section.empty_msg;
  cleanup_dir dir

let test_worktrees_section_empty () =
  let dir = test_dir () in
  let config = Lib.Room_utils.default_config dir in
  setup_room config;
  let section = Lib.Dashboard.worktrees_section config in
  Alcotest.(check string) "title" "Worktrees" section.title;
  Alcotest.(check string) "empty_msg" "(no worktrees)" section.empty_msg;
  cleanup_dir dir

(* ===== Test Suite ===== *)

let format_tests = [
  "format_section with content", `Quick, test_format_section;
  "format_section empty", `Quick, test_format_section_empty;
]

let timestamp_tests = [
  "parse valid timestamp", `Quick, test_parse_timestamp_valid;
  "parse invalid timestamp", `Quick, test_parse_timestamp_invalid;
]

let generate_tests = [
  "generate compact", `Quick, test_generate_compact;
  "generate full", `Quick, test_generate_full;
]

let section_tests = [
  "agents section empty", `Quick, test_agents_section_empty;
  "tasks section empty", `Quick, test_tasks_section_empty;
  "locks section empty", `Quick, test_locks_section_empty;
  "messages section empty", `Quick, test_messages_section_empty;
  "worktrees section empty", `Quick, test_worktrees_section_empty;
]

let () =
  Alcotest.run "Dashboard" [
    "Format", format_tests;
    "Timestamp", timestamp_tests;
    "Generate", generate_tests;
    "Sections", section_tests;
  ]
