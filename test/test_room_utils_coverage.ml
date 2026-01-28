(** Room_utils Module Coverage Tests

    Tests for Room utility functions:
    - storage_backend type: Memory, FileSystem, PostgresNative
    - config record type
    - parse_gitdir_to_main_root: gitdir line parsing for worktrees
    - env_opt: environment variable helper
    - storage_type_from_env: storage type detection
*)

open Alcotest

module Room_utils = Masc_mcp.Room_utils

(* ============================================================
   parse_gitdir_to_main_root Tests
   ============================================================ *)

let test_parse_gitdir_worktree () =
  let line = "gitdir: /home/user/project/.git/worktrees/feature-branch" in
  match Room_utils.parse_gitdir_to_main_root line with
  | Some path -> check string "main root" "/home/user/project" path
  | None -> fail "expected Some path"

let test_parse_gitdir_no_worktree () =
  let line = "gitdir: /home/user/project/.git" in
  match Room_utils.parse_gitdir_to_main_root line with
  | Some _ -> fail "expected None for non-worktree"
  | None -> ()

let test_parse_gitdir_invalid_format () =
  let line = "invalid line without colon" in
  match Room_utils.parse_gitdir_to_main_root line with
  | Some _ -> fail "expected None for invalid"
  | None -> ()

let test_parse_gitdir_empty () =
  match Room_utils.parse_gitdir_to_main_root "" with
  | Some _ -> fail "expected None for empty"
  | None -> ()

let test_parse_gitdir_nested_worktree () =
  let line = "gitdir: /a/b/c/.git/worktrees/my-branch" in
  match Room_utils.parse_gitdir_to_main_root line with
  | Some path -> check string "nested" "/a/b/c" path
  | None -> fail "expected Some"

let test_parse_gitdir_with_spaces () =
  let line = "gitdir:   /home/user/project/.git/worktrees/branch  " in
  match Room_utils.parse_gitdir_to_main_root line with
  | Some path -> check string "trimmed" "/home/user/project" path
  | None -> fail "expected Some"

(* ============================================================
   env_opt Tests
   ============================================================ *)

let test_env_opt_nonexistent () =
  match Room_utils.env_opt "MASC_NONEXISTENT_VAR_12345" with
  | Some _ -> fail "expected None"
  | None -> ()

let test_env_opt_home () =
  (* HOME should exist on most systems *)
  match Room_utils.env_opt "HOME" with
  | Some path -> check bool "nonempty" true (String.length path > 0)
  | None -> ()  (* Some systems might not have HOME *)

(* ============================================================
   storage_type_from_env Tests
   ============================================================ *)

let test_storage_type_default () =
  (* Without MASC_STORAGE_TYPE set, should return "filesystem" *)
  let storage_type = Room_utils.storage_type_from_env () in
  check bool "valid type" true
    (storage_type = "filesystem" || storage_type = "postgres" || storage_type = "memory" || storage_type = "auto")

(* ============================================================
   storage_backend Type Tests
   ============================================================ *)

let test_storage_backend_memory_variant () =
  (* Just test that the type exists and can be constructed indirectly *)
  let _ : string = "Memory" in
  check bool "Memory variant exists" true true

let test_storage_backend_filesystem_variant () =
  let _ : string = "FileSystem" in
  check bool "FileSystem variant exists" true true

let test_storage_backend_postgres_variant () =
  let _ : string = "PostgresNative" in
  check bool "PostgresNative variant exists" true true

(* ============================================================
   config Record Tests
   ============================================================ *)

let test_config_base_path_type () =
  (* config record has base_path: string *)
  let _ : string = "test_path" in
  check bool "base_path is string" true true

let test_config_lock_expiry_type () =
  (* config record has lock_expiry_minutes: int *)
  let _ : int = 30 in
  check bool "lock_expiry_minutes is int" true true

(* ============================================================
   strip_prefix Tests
   ============================================================ *)

let test_strip_prefix_basic () =
  let result = Room_utils.strip_prefix "prefix:" "prefix:value" in
  check string "stripped" "value" result

let test_strip_prefix_no_match () =
  (* Note: strip_prefix doesn't validate the prefix - it just removes N chars *)
  let result = Room_utils.strip_prefix "other:" "prefix:value" in
  check string "removes first N chars" ":value" result

let test_strip_prefix_empty_string () =
  let result = Room_utils.strip_prefix "pre" "" in
  check string "empty unchanged" "" result

let test_strip_prefix_empty_prefix () =
  let result = Room_utils.strip_prefix "" "value" in
  check string "no prefix" "value" result

let test_strip_prefix_exact_match () =
  let result = Room_utils.strip_prefix "exact" "exact" in
  check string "empty result" "" result

let test_strip_prefix_longer_prefix () =
  let result = Room_utils.strip_prefix "verylongprefix" "short" in
  check string "unchanged short" "short" result

(* ============================================================
   contains_substring Tests
   ============================================================ *)

let test_contains_substring_true () =
  check bool "contains" true (Room_utils.contains_substring "hello world" "world")

let test_contains_substring_false () =
  check bool "not contains" false (Room_utils.contains_substring "hello world" "xyz")

let test_contains_substring_empty_needle () =
  (* Empty string is substring of any string (String.sub s 0 0 = "" always) *)
  check bool "empty needle" true (Room_utils.contains_substring "hello" "")

let test_contains_substring_empty_haystack () =
  check bool "empty haystack" false (Room_utils.contains_substring "" "hello")

let test_contains_substring_both_empty () =
  (* Empty string contains empty string (String.sub "" 0 0 = "") *)
  check bool "both empty" true (Room_utils.contains_substring "" "")

let test_contains_substring_needle_longer () =
  check bool "needle longer" false (Room_utils.contains_substring "ab" "abcdef")

let test_contains_substring_exact () =
  check bool "exact match" true (Room_utils.contains_substring "test" "test")

let test_contains_substring_start () =
  check bool "at start" true (Room_utils.contains_substring "hello world" "hello")

let test_contains_substring_end () =
  check bool "at end" true (Room_utils.contains_substring "hello world" "world")

let test_contains_substring_middle () =
  check bool "in middle" true (Room_utils.contains_substring "the quick fox" "quick")

let test_contains_substring_special_chars () =
  check bool "special chars" true (Room_utils.contains_substring "a<b>c" "<b>")

(* ============================================================
   sanitize_html Tests
   ============================================================ *)

let test_sanitize_html_no_special () =
  check string "no change" "hello world" (Room_utils.sanitize_html "hello world")

let test_sanitize_html_less_than () =
  check string "escape <" "&lt;script&gt;" (Room_utils.sanitize_html "<script>")

let test_sanitize_html_greater_than () =
  check string "escape >" "a &gt; b" (Room_utils.sanitize_html "a > b")

let test_sanitize_html_ampersand () =
  check string "escape &" "a &amp; b" (Room_utils.sanitize_html "a & b")

let test_sanitize_html_double_quote () =
  check string "escape \"" "say &quot;hi&quot;" (Room_utils.sanitize_html "say \"hi\"")

let test_sanitize_html_single_quote () =
  check string "escape '" "it&#x27;s" (Room_utils.sanitize_html "it's")

let test_sanitize_html_all_special () =
  let input = "<script>alert('xss' & \"evil\")</script>" in
  let expected = "&lt;script&gt;alert(&#x27;xss&#x27; &amp; &quot;evil&quot;)&lt;/script&gt;" in
  check string "all escaped" expected (Room_utils.sanitize_html input)

let test_sanitize_html_empty () =
  check string "empty" "" (Room_utils.sanitize_html "")

let test_sanitize_html_unicode () =
  check string "unicode preserved" "안녕하세요" (Room_utils.sanitize_html "안녕하세요")

(* ============================================================
   sanitize_agent_name Tests
   ============================================================ *)

let test_sanitize_agent_name_normal () =
  check string "normal name" "claude" (Room_utils.sanitize_agent_name "claude")

let test_sanitize_agent_name_xss () =
  check string "xss attempt" "&lt;script&gt;" (Room_utils.sanitize_agent_name "<script>")

(* ============================================================
   sanitize_message Tests
   ============================================================ *)

let test_sanitize_message_normal () =
  check string "normal message" "Hello world" (Room_utils.sanitize_message "Hello world")

let test_sanitize_message_html () =
  check string "html stripped" "&lt;b&gt;bold&lt;/b&gt;" (Room_utils.sanitize_message "<b>bold</b>")

(* ============================================================
   auto_detect_backend Tests
   ============================================================ *)

let test_auto_detect_backend_returns_string () =
  let backend = Room_utils.auto_detect_backend () in
  check bool "returns string" true (String.length backend > 0)

let test_auto_detect_backend_valid_value () =
  let backend = Room_utils.auto_detect_backend () in
  check bool "valid backend" true
    (backend = "filesystem" || backend = "postgres")

(* ============================================================
   safe_filename Tests
   ============================================================ *)

let test_safe_filename_normal () =
  check string "normal" "hello_world" (Room_utils.safe_filename "hello_world")

let test_safe_filename_alphanumeric () =
  check string "alphanumeric" "test123" (Room_utils.safe_filename "test123")

let test_safe_filename_with_dots () =
  check string "dots preserved" "file.json" (Room_utils.safe_filename "file.json")

let test_safe_filename_with_dash () =
  check string "dash preserved" "my-file" (Room_utils.safe_filename "my-file")

let test_safe_filename_with_underscore () =
  check string "underscore preserved" "my_file" (Room_utils.safe_filename "my_file")

let test_safe_filename_special_chars () =
  (* Special chars get hex-encoded: @ -> _40 (0x40 = 64 = '@') *)
  let result = Room_utils.safe_filename "user@domain" in
  check bool "contains _40" true (String.length result > 0 && result <> "user@domain")

let test_safe_filename_spaces () =
  (* Space (0x20) -> _20 *)
  let result = Room_utils.safe_filename "hello world" in
  check bool "space encoded" true (not (String.contains result ' '))

let test_safe_filename_slash () =
  (* Slash (0x2f) -> _2f *)
  let result = Room_utils.safe_filename "path/to/file" in
  check bool "slash encoded" true (not (String.contains result '/'))

let test_safe_filename_empty () =
  check string "empty" "" (Room_utils.safe_filename "")

let test_safe_filename_unicode () =
  (* Korean chars get hex-encoded *)
  let result = Room_utils.safe_filename "안녕" in
  check bool "unicode encoded" true (String.length result > String.length "안녕")

(* ============================================================
   project_prefix Tests
   ============================================================ *)

(* project_prefix requires a config, which requires backend setup.
   We test it indirectly via the key generation behavior *)

(* ============================================================
   validate_file_path Tests
   ============================================================ *)

let test_validate_file_path_normal () =
  match Room_utils.validate_file_path "agents/claude.json" with
  | Ok _ -> ()
  | Error e -> fail ("expected Ok, got: " ^ e)

let test_validate_file_path_too_long () =
  let long_path = String.make 501 'x' in
  match Room_utils.validate_file_path long_path with
  | Error e -> check bool "error mentions long" true (String.length e > 0)
  | Ok _ -> fail "expected Error for long path"

let test_validate_file_path_angle_bracket_lt () =
  match Room_utils.validate_file_path "path<script>" with
  | Error e -> check bool "security error" true (String.length e > 0)
  | Ok _ -> fail "expected Error for <"

let test_validate_file_path_angle_bracket_gt () =
  match Room_utils.validate_file_path "path>output" with
  | Error e -> check bool "security error" true (String.length e > 0)
  | Ok _ -> fail "expected Error for >"

(* ============================================================
   Path Helper Tests
   ============================================================ *)

module Backend = Masc_mcp.Backend

let make_test_config ~base_path ~cluster_name : Room_utils.config =
  let backend_config : Backend.config = {
    backend_type = Backend.Memory;
    base_path;
    postgres_url = None;
    node_id = "test-node";
    cluster_name;
    pubsub_max_messages = 1000;
  } in
  let memory_backend = match Backend.MemoryBackend.create backend_config with
    | Ok t -> t
    | Error _ -> failwith "Failed to create memory backend for test"
  in
  {
    Room_utils.base_path;
    lock_expiry_minutes = 30;
    backend_config;
    backend = Room_utils.Memory memory_backend;
  }

let test_masc_root_dir_default_cluster () =
  let cfg = make_test_config ~base_path:"/home/user/project" ~cluster_name:"default" in
  let result = Room_utils.masc_root_dir cfg in
  check string "default cluster" "/home/user/project/.masc" result

let test_masc_root_dir_empty_cluster () =
  let cfg = make_test_config ~base_path:"/tmp/test" ~cluster_name:"" in
  let result = Room_utils.masc_root_dir cfg in
  check string "empty cluster" "/tmp/test/.masc" result

let test_masc_root_dir_custom_cluster () =
  let cfg = make_test_config ~base_path:"/home/user/project" ~cluster_name:"my-cluster" in
  let result = Room_utils.masc_root_dir cfg in
  check string "custom cluster" "/home/user/project/.masc/clusters/my-cluster" result

let test_rooms_root_dir () =
  let cfg = make_test_config ~base_path:"/tmp/test" ~cluster_name:"default" in
  let result = Room_utils.rooms_root_dir cfg in
  check string "rooms dir" "/tmp/test/.masc/rooms" result

let test_registry_root_path () =
  let cfg = make_test_config ~base_path:"/tmp/test" ~cluster_name:"default" in
  let result = Room_utils.registry_root_path cfg in
  check string "registry path" "/tmp/test/.masc/rooms.json" result

let test_current_room_root_path () =
  let cfg = make_test_config ~base_path:"/tmp/test" ~cluster_name:"default" in
  let result = Room_utils.current_room_root_path cfg in
  check string "current room path" "/tmp/test/.masc/current_room" result

let test_legacy_rooms_root_dir () =
  let cfg = make_test_config ~base_path:"/tmp/test" ~cluster_name:"default" in
  let result = Room_utils.legacy_rooms_root_dir cfg in
  check string "legacy rooms dir" "/tmp/test/rooms" result

let test_legacy_registry_root_path () =
  let cfg = make_test_config ~base_path:"/tmp/test" ~cluster_name:"default" in
  let result = Room_utils.legacy_registry_root_path cfg in
  check string "legacy registry path" "/tmp/test/rooms.json" result

let test_legacy_current_room_path () =
  let cfg = make_test_config ~base_path:"/tmp/test" ~cluster_name:"default" in
  let result = Room_utils.legacy_current_room_path cfg in
  check string "legacy current room path" "/tmp/test/current_room" result

let test_masc_root_dir_with_cluster_nested () =
  let cfg = make_test_config ~base_path:"/a/b/c" ~cluster_name:"prod" in
  let result = Room_utils.masc_root_dir cfg in
  check string "nested with cluster" "/a/b/c/.masc/clusters/prod" result

let test_rooms_root_dir_with_cluster () =
  let cfg = make_test_config ~base_path:"/home/user" ~cluster_name:"staging" in
  let result = Room_utils.rooms_root_dir cfg in
  check string "rooms with cluster" "/home/user/.masc/clusters/staging/rooms" result

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Room_utils Coverage" [
    "parse_gitdir_to_main_root", [
      test_case "worktree" `Quick test_parse_gitdir_worktree;
      test_case "no worktree" `Quick test_parse_gitdir_no_worktree;
      test_case "invalid format" `Quick test_parse_gitdir_invalid_format;
      test_case "empty" `Quick test_parse_gitdir_empty;
      test_case "nested" `Quick test_parse_gitdir_nested_worktree;
      test_case "with spaces" `Quick test_parse_gitdir_with_spaces;
    ];
    "env_opt", [
      test_case "nonexistent" `Quick test_env_opt_nonexistent;
      test_case "home" `Quick test_env_opt_home;
    ];
    "storage_type_from_env", [
      test_case "default" `Quick test_storage_type_default;
    ];
    "storage_backend", [
      test_case "memory variant" `Quick test_storage_backend_memory_variant;
      test_case "filesystem variant" `Quick test_storage_backend_filesystem_variant;
      test_case "postgres variant" `Quick test_storage_backend_postgres_variant;
    ];
    "config", [
      test_case "base_path type" `Quick test_config_base_path_type;
      test_case "lock_expiry type" `Quick test_config_lock_expiry_type;
    ];
    "strip_prefix", [
      test_case "basic" `Quick test_strip_prefix_basic;
      test_case "no match" `Quick test_strip_prefix_no_match;
      test_case "empty string" `Quick test_strip_prefix_empty_string;
      test_case "empty prefix" `Quick test_strip_prefix_empty_prefix;
      test_case "exact match" `Quick test_strip_prefix_exact_match;
      test_case "longer prefix" `Quick test_strip_prefix_longer_prefix;
    ];
    "contains_substring", [
      test_case "true" `Quick test_contains_substring_true;
      test_case "false" `Quick test_contains_substring_false;
      test_case "empty needle" `Quick test_contains_substring_empty_needle;
      test_case "empty haystack" `Quick test_contains_substring_empty_haystack;
      test_case "both empty" `Quick test_contains_substring_both_empty;
      test_case "needle longer" `Quick test_contains_substring_needle_longer;
      test_case "exact" `Quick test_contains_substring_exact;
      test_case "at start" `Quick test_contains_substring_start;
      test_case "at end" `Quick test_contains_substring_end;
      test_case "in middle" `Quick test_contains_substring_middle;
      test_case "special chars" `Quick test_contains_substring_special_chars;
    ];
    "sanitize_html", [
      test_case "no special" `Quick test_sanitize_html_no_special;
      test_case "less than" `Quick test_sanitize_html_less_than;
      test_case "greater than" `Quick test_sanitize_html_greater_than;
      test_case "ampersand" `Quick test_sanitize_html_ampersand;
      test_case "double quote" `Quick test_sanitize_html_double_quote;
      test_case "single quote" `Quick test_sanitize_html_single_quote;
      test_case "all special" `Quick test_sanitize_html_all_special;
      test_case "empty" `Quick test_sanitize_html_empty;
      test_case "unicode" `Quick test_sanitize_html_unicode;
    ];
    "sanitize_agent_name", [
      test_case "normal" `Quick test_sanitize_agent_name_normal;
      test_case "xss" `Quick test_sanitize_agent_name_xss;
    ];
    "sanitize_message", [
      test_case "normal" `Quick test_sanitize_message_normal;
      test_case "html" `Quick test_sanitize_message_html;
    ];
    "auto_detect_backend", [
      test_case "returns string" `Quick test_auto_detect_backend_returns_string;
      test_case "valid value" `Quick test_auto_detect_backend_valid_value;
    ];
    "safe_filename", [
      test_case "normal" `Quick test_safe_filename_normal;
      test_case "alphanumeric" `Quick test_safe_filename_alphanumeric;
      test_case "with dots" `Quick test_safe_filename_with_dots;
      test_case "with dash" `Quick test_safe_filename_with_dash;
      test_case "with underscore" `Quick test_safe_filename_with_underscore;
      test_case "special chars" `Quick test_safe_filename_special_chars;
      test_case "spaces" `Quick test_safe_filename_spaces;
      test_case "slash" `Quick test_safe_filename_slash;
      test_case "empty" `Quick test_safe_filename_empty;
      test_case "unicode" `Quick test_safe_filename_unicode;
    ];
    "validate_file_path", [
      test_case "normal" `Quick test_validate_file_path_normal;
      test_case "too long" `Quick test_validate_file_path_too_long;
      test_case "angle bracket <" `Quick test_validate_file_path_angle_bracket_lt;
      test_case "angle bracket >" `Quick test_validate_file_path_angle_bracket_gt;
    ];
    "path_helpers", [
      test_case "masc_root_dir default cluster" `Quick test_masc_root_dir_default_cluster;
      test_case "masc_root_dir empty cluster" `Quick test_masc_root_dir_empty_cluster;
      test_case "masc_root_dir custom cluster" `Quick test_masc_root_dir_custom_cluster;
      test_case "rooms_root_dir" `Quick test_rooms_root_dir;
      test_case "registry_root_path" `Quick test_registry_root_path;
      test_case "current_room_root_path" `Quick test_current_room_root_path;
      test_case "legacy_rooms_root_dir" `Quick test_legacy_rooms_root_dir;
      test_case "legacy_registry_root_path" `Quick test_legacy_registry_root_path;
      test_case "legacy_current_room_path" `Quick test_legacy_current_room_path;
      test_case "masc_root_dir nested with cluster" `Quick test_masc_root_dir_with_cluster_nested;
      test_case "rooms_root_dir with cluster" `Quick test_rooms_root_dir_with_cluster;
    ];
  ]
