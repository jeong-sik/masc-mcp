(** Config Module Coverage Tests

    Tests for MASC Configuration Management:
    - t: config record type
    - default: default configuration
    - to_json, of_json: serialization
    - config_path: file path generation
*)

open Alcotest

module Config = Masc_mcp.Config
module Mode = Masc_mcp.Mode

(* ============================================================
   default Tests
   ============================================================ *)

let test_default_mode () =
  check bool "default mode is Standard" true (Config.default.mode = Mode.Standard)

let test_default_has_categories () =
  check bool "has categories" true (List.length Config.default.enabled_categories > 0)

(* ============================================================
   config_path Tests
   ============================================================ *)

let test_config_path () =
  let path = Config.config_path "/tmp/masc" in
  check string "config path" "/tmp/masc/config.json" path

let test_config_path_trailing_slash () =
  let path = Config.config_path "/tmp/masc/" in
  check bool "contains config.json" true (String.length path > 0)

(* ============================================================
   to_json Tests
   ============================================================ *)

let test_to_json_returns_assoc () =
  let json = Config.to_json Config.default in
  match json with
  | `Assoc _ -> ()
  | _ -> fail "expected Assoc"

let test_to_json_has_mode () =
  let json = Config.to_json Config.default in
  match json with
  | `Assoc fields -> check bool "has mode" true (List.mem_assoc "mode" fields)
  | _ -> fail "expected Assoc"

let test_to_json_has_enabled_categories () =
  let json = Config.to_json Config.default in
  match json with
  | `Assoc fields -> check bool "has enabled_categories" true (List.mem_assoc "enabled_categories" fields)
  | _ -> fail "expected Assoc"

let test_to_json_mode_is_string () =
  let json = Config.to_json Config.default in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "mode" fields with
     | Some (`String _) -> ()
     | _ -> fail "mode should be string")
  | _ -> fail "expected Assoc"

(* ============================================================
   of_json Tests
   ============================================================ *)

let test_of_json_valid () =
  let json = `Assoc [
    ("mode", `String "standard");
    ("enabled_categories", `List []);
  ] in
  let config = Config.of_json json in
  check bool "parsed mode" true (config.mode = Mode.Standard)

let test_of_json_minimal () =
  let json = `Assoc [("mode", `String "minimal")] in
  let config = Config.of_json json in
  check bool "minimal mode" true (config.mode = Mode.Minimal)

let test_of_json_full () =
  let json = `Assoc [("mode", `String "full")] in
  let config = Config.of_json json in
  check bool "full mode" true (config.mode = Mode.Full)

let test_of_json_parallel () =
  let json = `Assoc [("mode", `String "parallel")] in
  let config = Config.of_json json in
  check bool "parallel mode" true (config.mode = Mode.Parallel)

let test_of_json_solo () =
  let json = `Assoc [("mode", `String "solo")] in
  let config = Config.of_json json in
  check bool "solo mode" true (config.mode = Mode.Solo)

let test_of_json_invalid_mode () =
  let json = `Assoc [("mode", `String "invalid")] in
  let config = Config.of_json json in
  check bool "defaults to Standard" true (config.mode = Mode.Standard)

let test_of_json_missing_mode () =
  let json = `Assoc [] in
  let config = Config.of_json json in
  check bool "defaults to Standard" true (config.mode = Mode.Standard)

let test_of_json_custom_with_categories () =
  let json = `Assoc [
    ("mode", `String "custom");
    ("enabled_categories", `List [`String "task"; `String "agent"]);
  ] in
  let config = Config.of_json json in
  check bool "custom mode" true (config.mode = Mode.Custom)

(* ============================================================
   Roundtrip Tests
   ============================================================ *)

let test_roundtrip_default () =
  let json = Config.to_json Config.default in
  let config = Config.of_json json in
  check bool "roundtrip mode" true (config.mode = Config.default.mode)

let test_roundtrip_minimal () =
  let config : Config.t = {
    mode = Mode.Minimal;
    enabled_categories = Mode.categories_for_mode Mode.Minimal;
  } in
  let json = Config.to_json config in
  let config' = Config.of_json json in
  check bool "roundtrip minimal" true (config'.mode = Mode.Minimal)

let test_roundtrip_full () =
  let config : Config.t = {
    mode = Mode.Full;
    enabled_categories = Mode.categories_for_mode Mode.Full;
  } in
  let json = Config.to_json config in
  let config' = Config.of_json json in
  check bool "roundtrip full" true (config'.mode = Mode.Full)

(* ============================================================
   load / save Tests
   ============================================================ *)

let setup_temp_dir () =
  let base = Filename.concat (Sys.getcwd ()) ".tmp" in
  (try Unix.mkdir base 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let dir = Filename.concat base ("config_test_" ^ string_of_int (Random.int 100000)) in
  Unix.mkdir dir 0o755;
  dir

let cleanup_temp_dir dir =
  let config_file = Filename.concat dir "config.json" in
  (try Sys.remove config_file with _ -> ());
  (try Unix.rmdir dir with _ -> ())

let test_load_nonexistent () =
  let dir = setup_temp_dir () in
  let config = Config.load dir in
  cleanup_temp_dir dir;
  check bool "defaults to Standard" true (config.mode = Mode.Standard)

let test_save_and_load () =
  let dir = setup_temp_dir () in
  let config : Config.t = {
    mode = Mode.Full;
    enabled_categories = Mode.categories_for_mode Mode.Full;
  } in
  Config.save dir config;
  let loaded = Config.load dir in
  cleanup_temp_dir dir;
  check bool "saved and loaded mode" true (loaded.mode = Mode.Full)

let test_save_creates_file () =
  let dir = setup_temp_dir () in
  Config.save dir Config.default;
  let exists = Sys.file_exists (Filename.concat dir "config.json") in
  cleanup_temp_dir dir;
  check bool "file created" true exists

(* ============================================================
   switch_mode Tests
   ============================================================ *)

let test_switch_mode_minimal () =
  let dir = setup_temp_dir () in
  let config = Config.switch_mode dir Mode.Minimal in
  cleanup_temp_dir dir;
  check bool "switched to Minimal" true (config.mode = Mode.Minimal)

let test_switch_mode_full () =
  let dir = setup_temp_dir () in
  let config = Config.switch_mode dir Mode.Full in
  cleanup_temp_dir dir;
  check bool "switched to Full" true (config.mode = Mode.Full)

let test_switch_mode_persists () =
  let dir = setup_temp_dir () in
  let _ = Config.switch_mode dir Mode.Parallel in
  let loaded = Config.load dir in
  cleanup_temp_dir dir;
  check bool "persisted Parallel" true (loaded.mode = Mode.Parallel)

(* ============================================================
   set_categories Tests
   ============================================================ *)

let test_set_categories () =
  let dir = setup_temp_dir () in
  let cats = [Mode.Core; Mode.Comm] in
  let config = Config.set_categories dir cats in
  cleanup_temp_dir dir;
  check bool "Custom mode" true (config.mode = Mode.Custom);
  check int "2 categories" 2 (List.length config.enabled_categories)

let test_set_categories_persists () =
  let dir = setup_temp_dir () in
  let cats = [Mode.Core] in
  let _ = Config.set_categories dir cats in
  let loaded = Config.load dir in
  cleanup_temp_dir dir;
  check bool "Custom mode loaded" true (loaded.mode = Mode.Custom)

(* ============================================================
   enable_category / disable_category Tests
   ============================================================ *)

let test_enable_category () =
  let dir = setup_temp_dir () in
  let _ = Config.set_categories dir [Mode.Core] in
  let config = Config.enable_category dir Mode.Comm in
  cleanup_temp_dir dir;
  check bool "has Comm" true (List.mem Mode.Comm config.enabled_categories);
  check bool "has Core" true (List.mem Mode.Core config.enabled_categories)

let test_enable_category_duplicate () =
  let dir = setup_temp_dir () in
  let _ = Config.set_categories dir [Mode.Core] in
  let config = Config.enable_category dir Mode.Core in
  cleanup_temp_dir dir;
  let core_count = List.length (List.filter (fun c -> c = Mode.Core) config.enabled_categories) in
  check int "no duplicate" 1 core_count

let test_disable_category () =
  let dir = setup_temp_dir () in
  let _ = Config.set_categories dir [Mode.Core; Mode.Comm] in
  let config = Config.disable_category dir Mode.Core in
  cleanup_temp_dir dir;
  check bool "removed Core" false (List.mem Mode.Core config.enabled_categories);
  check bool "kept Comm" true (List.mem Mode.Comm config.enabled_categories)

let test_disable_category_not_present () =
  let dir = setup_temp_dir () in
  let _ = Config.set_categories dir [Mode.Core] in
  let config = Config.disable_category dir Mode.Comm in
  cleanup_temp_dir dir;
  check bool "Core still there" true (List.mem Mode.Core config.enabled_categories)

(* ============================================================
   get_config_summary Tests
   ============================================================ *)

let test_get_config_summary_has_mode () =
  let dir = setup_temp_dir () in
  Config.save dir Config.default;
  let summary = Config.get_config_summary dir in
  cleanup_temp_dir dir;
  match summary with
  | `Assoc fields -> check bool "has mode" true (List.mem_assoc "mode" fields)
  | _ -> fail "expected Assoc"

let test_get_config_summary_has_tool_count () =
  let dir = setup_temp_dir () in
  Config.save dir Config.default;
  let summary = Config.get_config_summary dir in
  cleanup_temp_dir dir;
  match summary with
  | `Assoc fields -> check bool "has enabled_tool_count" true (List.mem_assoc "enabled_tool_count" fields)
  | _ -> fail "expected Assoc"

let test_get_config_summary_has_available_modes () =
  let dir = setup_temp_dir () in
  Config.save dir Config.default;
  let summary = Config.get_config_summary dir in
  cleanup_temp_dir dir;
  match summary with
  | `Assoc fields -> check bool "has available_modes" true (List.mem_assoc "available_modes" fields)
  | _ -> fail "expected Assoc"

let test_get_config_summary_has_categories () =
  let dir = setup_temp_dir () in
  Config.save dir Config.default;
  let summary = Config.get_config_summary dir in
  cleanup_temp_dir dir;
  match summary with
  | `Assoc fields ->
      check bool "has enabled_categories" true (List.mem_assoc "enabled_categories" fields);
      check bool "has disabled_categories" true (List.mem_assoc "disabled_categories" fields)
  | _ -> fail "expected Assoc"

let test_get_config_summary_mode_description () =
  let dir = setup_temp_dir () in
  Config.save dir Config.default;
  let summary = Config.get_config_summary dir in
  cleanup_temp_dir dir;
  match summary with
  | `Assoc fields ->
      check bool "has mode_description" true (List.mem_assoc "mode_description" fields)
  | _ -> fail "expected Assoc"

(* ============================================================
   of_json edge cases
   ============================================================ *)

let test_of_json_mode_not_string () =
  let json = `Assoc [("mode", `Int 123)] in
  let config = Config.of_json json in
  check bool "defaults to Standard" true (config.mode = Mode.Standard)

let test_of_json_completely_invalid () =
  let json = `String "not an object" in
  let config = Config.of_json json in
  check bool "defaults to Standard" true (config.mode = Mode.Standard)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Config Coverage" [
    "default", [
      test_case "mode" `Quick test_default_mode;
      test_case "categories" `Quick test_default_has_categories;
    ];
    "config_path", [
      test_case "basic" `Quick test_config_path;
      test_case "trailing slash" `Quick test_config_path_trailing_slash;
    ];
    "to_json", [
      test_case "returns assoc" `Quick test_to_json_returns_assoc;
      test_case "has mode" `Quick test_to_json_has_mode;
      test_case "has enabled_categories" `Quick test_to_json_has_enabled_categories;
      test_case "mode is string" `Quick test_to_json_mode_is_string;
    ];
    "of_json", [
      test_case "valid" `Quick test_of_json_valid;
      test_case "minimal" `Quick test_of_json_minimal;
      test_case "full" `Quick test_of_json_full;
      test_case "parallel" `Quick test_of_json_parallel;
      test_case "solo" `Quick test_of_json_solo;
      test_case "invalid mode" `Quick test_of_json_invalid_mode;
      test_case "missing mode" `Quick test_of_json_missing_mode;
      test_case "custom with categories" `Quick test_of_json_custom_with_categories;
      test_case "mode not string" `Quick test_of_json_mode_not_string;
      test_case "completely invalid" `Quick test_of_json_completely_invalid;
    ];
    "roundtrip", [
      test_case "default" `Quick test_roundtrip_default;
      test_case "minimal" `Quick test_roundtrip_minimal;
      test_case "full" `Quick test_roundtrip_full;
    ];
    "load_save", [
      test_case "load nonexistent" `Quick test_load_nonexistent;
      test_case "save and load" `Quick test_save_and_load;
      test_case "save creates file" `Quick test_save_creates_file;
    ];
    "switch_mode", [
      test_case "minimal" `Quick test_switch_mode_minimal;
      test_case "full" `Quick test_switch_mode_full;
      test_case "persists" `Quick test_switch_mode_persists;
    ];
    "set_categories", [
      test_case "basic" `Quick test_set_categories;
      test_case "persists" `Quick test_set_categories_persists;
    ];
    "enable_disable", [
      test_case "enable" `Quick test_enable_category;
      test_case "enable duplicate" `Quick test_enable_category_duplicate;
      test_case "disable" `Quick test_disable_category;
      test_case "disable not present" `Quick test_disable_category_not_present;
    ];
    "get_config_summary", [
      test_case "has mode" `Quick test_get_config_summary_has_mode;
      test_case "has tool count" `Quick test_get_config_summary_has_tool_count;
      test_case "has available modes" `Quick test_get_config_summary_has_available_modes;
      test_case "has categories" `Quick test_get_config_summary_has_categories;
      test_case "has mode description" `Quick test_get_config_summary_mode_description;
    ];
  ]
