(** Safe_ops Module Tests *)

open Alcotest

let test_int_of_string_safe_valid () =
  let open Masc_mcp.Safe_ops in
  check (option int) "parses valid int" (Some 42) (int_of_string_safe "42")

let test_int_of_string_safe_invalid () =
  let open Masc_mcp.Safe_ops in
  check (option int) "None on invalid" None (int_of_string_safe "abc")

let test_int_of_string_with_default () =
  let open Masc_mcp.Safe_ops in
  check int "default on invalid" 0 (int_of_string_with_default ~default:0 "abc")

let test_float_of_string_safe_valid () =
  let open Masc_mcp.Safe_ops in
  check (option (float 0.001)) "parses valid float" (Some 3.14) (float_of_string_safe "3.14")

let test_float_of_string_safe_invalid () =
  let open Masc_mcp.Safe_ops in
  check (option (float 0.001)) "None on invalid" None (float_of_string_safe "abc")

let test_parse_json_safe_valid () =
  let open Masc_mcp.Safe_ops in
  let result = parse_json_safe ~context:"test" {|{"foo": "bar"}|} in
  check bool "Ok on valid JSON" true (Result.is_ok result)

let test_parse_json_safe_invalid () =
  let open Masc_mcp.Safe_ops in
  let result = parse_json_safe ~context:"test" "not json" in
  check bool "Error on invalid JSON" true (Result.is_error result)

let test_read_file_safe_not_found () =
  let open Masc_mcp.Safe_ops in
  let result = read_file_safe "/nonexistent/path/file.txt" in
  check bool "Error on missing file" true (Result.is_error result)

(* JSON extraction tests *)
let sample_json = Yojson.Safe.from_string {|{"name": "test", "count": 42, "rate": 3.14, "active": true}|}

let test_json_string () =
  let open Masc_mcp.Safe_ops in
  check string "extracts string" "test" (json_string "name" sample_json)

let test_json_string_missing () =
  let open Masc_mcp.Safe_ops in
  check string "default on missing" "default" (json_string ~default:"default" "missing" sample_json)

let test_json_int () =
  let open Masc_mcp.Safe_ops in
  check int "extracts int" 42 (json_int "count" sample_json)

let test_json_float () =
  let open Masc_mcp.Safe_ops in
  check (float 0.001) "extracts float" 3.14 (json_float "rate" sample_json)

let test_json_bool () =
  let open Masc_mcp.Safe_ops in
  check bool "extracts bool" true (json_bool "active" sample_json)

let test_json_string_opt_present () =
  let open Masc_mcp.Safe_ops in
  check (option string) "Some on present" (Some "test") (json_string_opt "name" sample_json)

let test_json_string_opt_missing () =
  let open Masc_mcp.Safe_ops in
  check (option string) "None on missing" None (json_string_opt "missing" sample_json)

let () =
  run "Safe_ops" [
    "int_of_string_safe", [
      test_case "valid" `Quick test_int_of_string_safe_valid;
      test_case "invalid" `Quick test_int_of_string_safe_invalid;
      test_case "with default" `Quick test_int_of_string_with_default;
    ];
    "float_of_string_safe", [
      test_case "valid" `Quick test_float_of_string_safe_valid;
      test_case "invalid" `Quick test_float_of_string_safe_invalid;
    ];
    "parse_json_safe", [
      test_case "valid json" `Quick test_parse_json_safe_valid;
      test_case "invalid json" `Quick test_parse_json_safe_invalid;
    ];
    "read_file_safe", [
      test_case "not found" `Quick test_read_file_safe_not_found;
    ];
    "json_extraction", [
      test_case "string" `Quick test_json_string;
      test_case "string missing" `Quick test_json_string_missing;
      test_case "int" `Quick test_json_int;
      test_case "float" `Quick test_json_float;
      test_case "bool" `Quick test_json_bool;
      test_case "string_opt present" `Quick test_json_string_opt_present;
      test_case "string_opt missing" `Quick test_json_string_opt_missing;
    ];
  ]
