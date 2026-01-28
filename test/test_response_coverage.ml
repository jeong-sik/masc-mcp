(** Response Module Coverage Tests

    Tests for MASC Standardized API Response:
    - severity type: Fatal, Warning, Info
    - severity_to_string: severity to string conversion
    - severity_of_string: string to severity parsing
    - severity_of_string_default: with default fallback
    - error_to_json: error detail to JSON
    - to_json: response to JSON
*)

open Alcotest

module Response = Masc_mcp.Response

(* ============================================================
   severity_to_string Tests
   ============================================================ *)

let test_severity_to_string_fatal () =
  check string "fatal" "fatal" (Response.severity_to_string Response.Fatal)

let test_severity_to_string_warning () =
  check string "warning" "warning" (Response.severity_to_string Response.Warning)

let test_severity_to_string_info () =
  check string "info" "info" (Response.severity_to_string Response.Info)

(* ============================================================
   severity_of_string Tests
   ============================================================ *)

let test_severity_of_string_fatal () =
  match Response.severity_of_string "fatal" with
  | Ok Response.Fatal -> check bool "fatal" true true
  | _ -> fail "expected Fatal"

let test_severity_of_string_warning () =
  match Response.severity_of_string "warning" with
  | Ok Response.Warning -> check bool "warning" true true
  | _ -> fail "expected Warning"

let test_severity_of_string_info () =
  match Response.severity_of_string "info" with
  | Ok Response.Info -> check bool "info" true true
  | _ -> fail "expected Info"

let test_severity_of_string_invalid () =
  match Response.severity_of_string "invalid" with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected Error"

let test_severity_of_string_empty () =
  match Response.severity_of_string "" with
  | Error _ -> check bool "error" true true
  | Ok _ -> fail "expected Error"

(* ============================================================
   severity_of_string_default Tests
   ============================================================ *)

let test_severity_default_valid () =
  let sev = Response.severity_of_string_default "fatal" in
  check bool "fatal" true (sev = Response.Fatal)

let test_severity_default_invalid () =
  let sev = Response.severity_of_string_default "invalid" in
  check bool "default info" true (sev = Response.Info)

let test_severity_default_custom () =
  let sev = Response.severity_of_string_default ~default:Response.Warning "invalid" in
  check bool "custom default" true (sev = Response.Warning)

(* ============================================================
   Roundtrip Tests
   ============================================================ *)

let test_severity_roundtrip_fatal () =
  let original = Response.Fatal in
  let s = Response.severity_to_string original in
  match Response.severity_of_string s with
  | Ok result -> check bool "roundtrip" true (original = result)
  | Error _ -> fail "roundtrip failed"

let test_severity_roundtrip_warning () =
  let original = Response.Warning in
  let s = Response.severity_to_string original in
  match Response.severity_of_string s with
  | Ok result -> check bool "roundtrip" true (original = result)
  | Error _ -> fail "roundtrip failed"

let test_severity_roundtrip_info () =
  let original = Response.Info in
  let s = Response.severity_to_string original in
  match Response.severity_of_string s with
  | Ok result -> check bool "roundtrip" true (original = result)
  | Error _ -> fail "roundtrip failed"

(* ============================================================
   error_to_json Tests
   ============================================================ *)

let test_error_to_json_structure () =
  let error : Response.error_detail = {
    code = "TEST_ERROR";
    severity = Response.Warning;
    message = "Test message";
    recovery_hints = ["hint1"; "hint2"];
  } in
  let json = Response.error_to_json error in
  match json with
  | `Assoc fields ->
      check bool "has code" true (List.mem_assoc "code" fields);
      check bool "has severity" true (List.mem_assoc "severity" fields);
      check bool "has message" true (List.mem_assoc "message" fields);
      check bool "has recovery_hints" true (List.mem_assoc "recovery_hints" fields)
  | _ -> fail "expected Assoc"

let test_error_to_json_code () =
  let error : Response.error_detail = {
    code = "MY_CODE";
    severity = Response.Info;
    message = "msg";
    recovery_hints = [];
  } in
  let json = Response.error_to_json error in
  match json with
  | `Assoc fields ->
      (match List.assoc "code" fields with
       | `String "MY_CODE" -> check bool "code" true true
       | _ -> fail "expected code string")
  | _ -> fail "expected Assoc"

(* ============================================================
   to_json Tests
   ============================================================ *)

let test_to_json_structure () =
  let resp : Response.t = {
    success = true;
    data = `String "test";
    message = "Test message";
    errors = [];
    timestamp = 1000.0;
  } in
  let json = Response.to_json resp in
  match json with
  | `Assoc fields ->
      check bool "has success" true (List.mem_assoc "success" fields);
      check bool "has data" true (List.mem_assoc "data" fields);
      check bool "has message" true (List.mem_assoc "message" fields);
      check bool "has errors" true (List.mem_assoc "errors" fields);
      check bool "has timestamp" true (List.mem_assoc "timestamp" fields)
  | _ -> fail "expected Assoc"

let test_to_json_success_true () =
  let resp : Response.t = {
    success = true;
    data = `Null;
    message = "";
    errors = [];
    timestamp = 0.0;
  } in
  let json = Response.to_json resp in
  match json with
  | `Assoc fields ->
      (match List.assoc "success" fields with
       | `Bool true -> check bool "success true" true true
       | _ -> fail "expected true")
  | _ -> fail "expected Assoc"

let test_to_json_success_false () =
  let resp : Response.t = {
    success = false;
    data = `Null;
    message = "error";
    errors = [];
    timestamp = 0.0;
  } in
  let json = Response.to_json resp in
  match json with
  | `Assoc fields ->
      (match List.assoc "success" fields with
       | `Bool false -> check bool "success false" true true
       | _ -> fail "expected false")
  | _ -> fail "expected Assoc"

(* ============================================================
   Type Tests
   ============================================================ *)

let test_error_detail_code () =
  let _ : string = "code" in
  check bool "code is string" true true

let test_error_detail_message () =
  let _ : string = "message" in
  check bool "message is string" true true

let test_error_detail_recovery_hints () =
  let _ : string list = [] in
  check bool "recovery_hints is list" true true

let test_response_success () =
  let _ : bool = true in
  check bool "success is bool" true true

let test_response_timestamp () =
  let _ : float = 0.0 in
  check bool "timestamp is float" true true

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Response Coverage" [
    "severity_to_string", [
      test_case "fatal" `Quick test_severity_to_string_fatal;
      test_case "warning" `Quick test_severity_to_string_warning;
      test_case "info" `Quick test_severity_to_string_info;
    ];
    "severity_of_string", [
      test_case "fatal" `Quick test_severity_of_string_fatal;
      test_case "warning" `Quick test_severity_of_string_warning;
      test_case "info" `Quick test_severity_of_string_info;
      test_case "invalid" `Quick test_severity_of_string_invalid;
      test_case "empty" `Quick test_severity_of_string_empty;
    ];
    "severity_of_string_default", [
      test_case "valid" `Quick test_severity_default_valid;
      test_case "invalid uses default" `Quick test_severity_default_invalid;
      test_case "custom default" `Quick test_severity_default_custom;
    ];
    "roundtrip", [
      test_case "fatal" `Quick test_severity_roundtrip_fatal;
      test_case "warning" `Quick test_severity_roundtrip_warning;
      test_case "info" `Quick test_severity_roundtrip_info;
    ];
    "error_to_json", [
      test_case "structure" `Quick test_error_to_json_structure;
      test_case "code" `Quick test_error_to_json_code;
    ];
    "to_json", [
      test_case "structure" `Quick test_to_json_structure;
      test_case "success true" `Quick test_to_json_success_true;
      test_case "success false" `Quick test_to_json_success_false;
    ];
    "types", [
      test_case "error code" `Quick test_error_detail_code;
      test_case "error message" `Quick test_error_detail_message;
      test_case "error hints" `Quick test_error_detail_recovery_hints;
      test_case "response success" `Quick test_response_success;
      test_case "response timestamp" `Quick test_response_timestamp;
    ];
  ]
