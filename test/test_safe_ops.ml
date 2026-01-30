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
  ]
