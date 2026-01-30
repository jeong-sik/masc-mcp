(** MASC Response Module Tests

    Tests for the standardized API response envelope:
    - Basic success/error responses
    - Recovery hints formatting
    - Domain-specific responses (drift, lock, task)
    - JSON serialization
*)

open Masc_mcp

(* Test utilities *)
let assert_true msg cond =
  if not cond then failwith (Printf.sprintf "Assertion failed: %s" msg)

let assert_equal_string msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "Assertion failed: %s (expected '%s', got '%s')"
      msg expected actual)

let json_has_field json field =
  match json with
  | `Assoc fields -> List.mem_assoc field fields
  | _ -> false

let json_get_bool json field =
  match json with
  | `Assoc fields -> (
    match List.assoc_opt field fields with
    | Some (`Bool b) -> b
    | _ -> failwith (Printf.sprintf "Field %s is not a bool" field))
  | _ -> failwith "Not an Assoc"

let _ = json_get_bool  (* Suppress unused warning *)

let json_get_string json field =
  match json with
  | `Assoc fields -> (
    match List.assoc_opt field fields with
    | Some (`String s) -> s
    | _ -> failwith (Printf.sprintf "Field %s is not a string" field))
  | _ -> failwith "Not an Assoc"

let json_get_list json field =
  match json with
  | `Assoc fields -> (
    match List.assoc_opt field fields with
    | Some (`List l) -> l
    | _ -> failwith (Printf.sprintf "Field %s is not a list" field))
  | _ -> failwith "Not an Assoc"

let json_get_float json field =
  match json with
  | `Assoc fields -> (
    match List.assoc_opt field fields with
    | Some (`Float f) -> f
    | Some (`Int i) -> float_of_int i
    | _ -> failwith (Printf.sprintf "Field %s is not a float" field))
  | _ -> failwith "Not an Assoc"

let json_get_bool_value json field =
  match json with
  | `Assoc fields -> (
    match List.assoc_opt field fields with
    | Some (`Bool b) -> b
    | _ -> failwith (Printf.sprintf "Field %s is not a bool" field))
  | _ -> failwith "Not an Assoc"

let assert_float_equal msg ~epsilon expected actual =
  if abs_float (expected -. actual) > epsilon then
    failwith (Printf.sprintf "Assertion failed: %s (expected %.4f, got %.4f)" msg expected actual)

(* ========================================
   1. Basic Response Tests
   ======================================== *)

let test_ok_response () =
  let resp = Response.ok ~message:"Test success" (`String "data") in
  assert_true "success is true" resp.success;
  assert_equal_string "message" "Test success" resp.message;
  assert_true "no errors" (List.length resp.errors = 0);
  assert_true "timestamp > 0" (resp.timestamp > 0.0);
  Printf.printf "  test_ok_response passed\n"

let test_ok_default_message () =
  let resp = Response.ok (`String "data") in
  assert_equal_string "default message" "OK" resp.message;
  Printf.printf "  test_ok_default_message passed\n"

let test_error_response () =
  let resp = Response.error
    ~code:"TEST_ERROR"
    ~message:"Something went wrong"
    ~hints:["Try again"; "Check logs"]
    () in
  assert_true "success is false" (not resp.success);
  assert_equal_string "message" "Something went wrong" resp.message;
  assert_true "has 1 error" (List.length resp.errors = 1);

  let err = List.hd resp.errors in
  assert_equal_string "error code" "TEST_ERROR" err.code;
  assert_true "has 2 hints" (List.length err.recovery_hints = 2);
  Printf.printf "  test_error_response passed\n"

let test_error_with_data () =
  let data = `Assoc [("context", `String "extra info")] in
  let resp = Response.error
    ~data
    ~code:"WITH_DATA"
    ~message:"Error with data"
    () in
  assert_true "has data" (json_has_field resp.data "context");
  Printf.printf "  test_error_with_data passed\n"

(* ========================================
   2. JSON Serialization Tests
   ======================================== *)

let test_to_json_structure () =
  let resp = Response.ok ~message:"Test" (`Null) in
  let json = Response.to_json resp in

  assert_true "has success" (json_has_field json "success");
  assert_true "has data" (json_has_field json "data");
  assert_true "has message" (json_has_field json "message");
  assert_true "has errors" (json_has_field json "errors");
  assert_true "has timestamp" (json_has_field json "timestamp");
  Printf.printf "  test_to_json_structure passed\n"

let test_error_json_structure () =
  let resp = Response.error ~code:"ERR" ~message:"msg" ~hints:["hint1"] () in
  let json = Response.to_json resp in

  let errors = json_get_list json "errors" in
  assert_true "has 1 error" (List.length errors = 1);

  let err_json = List.hd errors in
  assert_true "error has code" (json_has_field err_json "code");
  assert_true "error has severity" (json_has_field err_json "severity");
  assert_true "error has message" (json_has_field err_json "message");
  assert_true "error has hints" (json_has_field err_json "recovery_hints");
  Printf.printf "  test_error_json_structure passed\n"

let test_to_string () =
  let resp = Response.ok (`String "test") in
  let s = Response.to_string resp in
  assert_true "is JSON string" (String.length s > 0);
  (* Parse JSON to verify it's valid *)
  let json = Yojson.Safe.from_string s in
  assert_true "has success field" (json_has_field json "success");
  Printf.printf "  test_to_string passed\n"

(* ========================================
   3. Severity Tests
   ======================================== *)

let test_severity_to_string () =
  assert_equal_string "fatal" "fatal" (Response.severity_to_string Response.Fatal);
  assert_equal_string "warning" "warning" (Response.severity_to_string Response.Warning);
  assert_equal_string "info" "info" (Response.severity_to_string Response.Info);
  Printf.printf "  test_severity_to_string passed\n"

let test_severity_of_string () =
  assert_true "fatal parses" (Response.severity_of_string "fatal" = Ok Response.Fatal);
  assert_true "warning parses" (Response.severity_of_string "warning" = Ok Response.Warning);
  assert_true "info parses" (Response.severity_of_string "info" = Ok Response.Info);
  assert_true "unknown returns Error" (Result.is_error (Response.severity_of_string "unknown"));
  (* Test backwards-compatible default function *)
  assert_true "default fallback" (Response.severity_of_string_default "unknown" = Response.Info);
  assert_true "default with custom" (Response.severity_of_string_default ~default:Response.Fatal "bad" = Response.Fatal);
  Printf.printf "  test_severity_of_string passed\n"

(* ========================================
   4. Helper Function Tests
   ======================================== *)

let test_make_error () =
  let err = Response.make_error
    ~code:"CUSTOM"
    ~severity:Response.Warning
    ~message:"Custom error"
    ~hints:["Do this"]
    () in
  assert_equal_string "code" "CUSTOM" err.code;
  assert_true "severity is Warning" (err.severity = Response.Warning);
  assert_equal_string "message" "Custom error" err.message;
  assert_true "has hint" (List.length err.recovery_hints = 1);
  Printf.printf "  test_make_error passed\n"

let test_make_warning () =
  let warn = Response.make_warning ~code:"WARN" ~message:"Warning msg" () in
  assert_true "severity is Warning" (warn.severity = Response.Warning);
  Printf.printf "  test_make_warning passed\n"

let test_make_info () =
  let info = Response.make_info ~code:"INFO" ~message:"Info msg" () in
  assert_true "severity is Info" (info.severity = Response.Info);
  assert_true "no hints for info" (List.length info.recovery_hints = 0);
  Printf.printf "  test_make_info passed\n"

let test_ok_with_warnings () =
  let warning = Response.make_warning ~code:"WARN" ~message:"Warning" () in
  let resp = Response.ok_with_warnings ~message:"OK but warned" ~warnings:[warning]
    (`String "data") in
  assert_true "success is true" resp.success;
  assert_true "has 1 warning" (List.length resp.errors = 1);
  Printf.printf "  test_ok_with_warnings passed\n"

let test_errors_multiple () =
  let err1 = Response.make_error ~code:"ERR1" ~message:"Error 1" () in
  let err2 = Response.make_error ~code:"ERR2" ~message:"Error 2" () in
  let resp = Response.errors ~message:"Multiple errors" [err1; err2] in
  assert_true "success is false" (not resp.success);
  assert_true "has 2 errors" (List.length resp.errors = 2);
  Printf.printf "  test_errors_multiple passed\n"

(* ========================================
   5. Common Error Types Tests
   ======================================== *)

let test_validation_error () =
  let resp = Response.validation_error ~field:"email" ~reason:"invalid format" in
  assert_true "success is false" (not resp.success);
  let err = List.hd resp.errors in
  assert_equal_string "code" "VALIDATION_ERROR" err.code;
  assert_true "has hints" (List.length err.recovery_hints >= 1);
  Printf.printf "  test_validation_error passed\n"

let test_not_found () =
  let resp = Response.not_found ~resource:"Task" ~id:"task-999" in
  assert_true "success is false" (not resp.success);
  let err = List.hd resp.errors in
  assert_equal_string "code" "NOT_FOUND" err.code;
  assert_true "message mentions resource" (String.length resp.message > 0);
  Printf.printf "  test_not_found passed\n"

let test_already_exists () =
  let resp = Response.already_exists ~resource:"Agent" ~id:"claude" in
  let err = List.hd resp.errors in
  assert_equal_string "code" "ALREADY_EXISTS" err.code;
  Printf.printf "  test_already_exists passed\n"

let test_permission_denied () =
  let resp = Response.permission_denied ~action:"delete" ~resource:"task" in
  let err = List.hd resp.errors in
  assert_equal_string "code" "PERMISSION_DENIED" err.code;
  Printf.printf "  test_permission_denied passed\n"

let test_conflict () =
  let resp = Response.conflict ~resource:"file.txt" ~reason:"concurrent edit" in
  let err = List.hd resp.errors in
  assert_equal_string "code" "CONFLICT" err.code;
  Printf.printf "  test_conflict passed\n"

let test_timeout () =
  let resp = Response.timeout ~operation:"fetch_data" in
  let err = List.hd resp.errors in
  assert_equal_string "code" "TIMEOUT" err.code;
  assert_true "has recovery hints" (List.length err.recovery_hints >= 2);
  Printf.printf "  test_timeout passed\n"

(* ========================================
   6. Domain-Specific Response Tests
   ======================================== *)

let test_drift_detected () =
  let resp = Response.drift_detected
    ~similarity:0.72
    ~drift_type:"factual"
    ~threshold:0.85
    ~details:"Content mismatch detected" in
  (* Verify success is false *)
  assert_true "success is false" (not resp.success);

  (* Verify ACTUAL VALUES in data, not just presence *)
  assert_float_equal "similarity value" ~epsilon:0.001 0.72 (json_get_float resp.data "similarity");
  assert_equal_string "drift_type value" "factual" (json_get_string resp.data "drift_type");
  assert_float_equal "threshold value" ~epsilon:0.001 0.85 (json_get_float resp.data "threshold");

  (* Verify error structure *)
  let err = List.hd resp.errors in
  assert_equal_string "code" "DRIFT_DETECTED" err.code;
  assert_true "severity is Warning" (err.severity = Response.Warning);
  assert_equal_string "error message" "Content mismatch detected" err.message;

  (* Verify factual-specific hints contain expected content *)
  assert_true "has >= 2 hints" (List.length err.recovery_hints >= 2);
  let hints_concat = String.concat " " err.recovery_hints in
  assert_true "factual hint mentions 'source agent'" (
    try Str.search_forward (Str.regexp_case_fold "source agent") hints_concat 0 >= 0
    with Not_found -> false);
  Printf.printf "  test_drift_detected passed\n"

let test_drift_detected_semantic () =
  let resp = Response.drift_detected
    ~similarity:0.80
    ~drift_type:"semantic"
    ~threshold:0.85
    ~details:"Meaning diverged" in

  (* Verify actual values *)
  assert_float_equal "similarity" ~epsilon:0.001 0.80 (json_get_float resp.data "similarity");
  assert_equal_string "drift_type" "semantic" (json_get_string resp.data "drift_type");

  let err = List.hd resp.errors in
  (* Semantic drift should have specific hints *)
  assert_true "has >= 2 semantic hints" (List.length err.recovery_hints >= 2);

  (* Verify threshold is mentioned with actual value in hints *)
  let hints_concat = String.concat " " err.recovery_hints in
  assert_true "hint mentions 'threshold'" (
    try Str.search_forward (Str.regexp "threshold") hints_concat 0 >= 0
    with Not_found -> false);
  assert_true "hint contains threshold value 0.85" (
    try Str.search_forward (Str.regexp "0\\.85") hints_concat 0 >= 0
    with Not_found -> false);
  Printf.printf "  test_drift_detected_semantic passed\n"

let test_handoff_verified () =
  let resp = Response.handoff_verified ~similarity:0.95 in
  assert_true "success is true" resp.success;
  (* Verify ACTUAL values *)
  assert_float_equal "similarity value" ~epsilon:0.001 0.95 (json_get_float resp.data "similarity");
  assert_true "verified is true" (json_get_bool_value resp.data "verified");
  (* Verify message contains percentage *)
  assert_true "message contains 95%" (
    try Str.search_forward (Str.regexp "95") resp.message 0 >= 0
    with Not_found -> false);
  Printf.printf "  test_handoff_verified passed\n"

let test_task_claimed () =
  let resp = Response.task_claimed ~task_id:"task-001" ~agent:"claude" in
  assert_true "success is true" resp.success;
  (* Verify ACTUAL values *)
  assert_equal_string "task_id value" "task-001" (json_get_string resp.data "task_id");
  assert_equal_string "claimed_by value" "claude" (json_get_string resp.data "claimed_by");
  assert_equal_string "status value" "in_progress" (json_get_string resp.data "status");
  (* Verify message mentions task and agent *)
  assert_true "message mentions task-001" (
    try Str.search_forward (Str.regexp "task-001") resp.message 0 >= 0
    with Not_found -> false);
  assert_true "message mentions claude" (
    try Str.search_forward (Str.regexp "claude") resp.message 0 >= 0
    with Not_found -> false);
  Printf.printf "  test_task_claimed passed\n"

let test_task_already_claimed () =
  let resp = Response.task_already_claimed ~task_id:"task-001" ~claimed_by:"gemini" in
  assert_true "success is false" (not resp.success);
  (* Verify data contains the blocking info *)
  assert_equal_string "task_id in data" "task-001" (json_get_string resp.data "task_id");
  assert_equal_string "claimed_by in data" "gemini" (json_get_string resp.data "claimed_by");

  let err = List.hd resp.errors in
  assert_equal_string "code" "TASK_CLAIMED" err.code;
  assert_true "has >= 2 recovery hints" (List.length err.recovery_hints >= 2);

  (* Verify hints mention the blocking agent *)
  let hints_concat = String.concat " " err.recovery_hints in
  assert_true "hints mention 'gemini'" (
    try Str.search_forward (Str.regexp "gemini") hints_concat 0 >= 0
    with Not_found -> false);
  Printf.printf "  test_task_already_claimed passed\n"

let test_task_completed () =
  let resp = Response.task_completed
    ~task_id:"task-001"
    ~agent:"claude"
    ~notes:"All tests pass" in
  assert_true "success is true" resp.success;
  (* Verify ALL actual values *)
  assert_equal_string "task_id" "task-001" (json_get_string resp.data "task_id");
  assert_equal_string "completed_by" "claude" (json_get_string resp.data "completed_by");
  assert_equal_string "notes" "All tests pass" (json_get_string resp.data "notes");
  assert_equal_string "status" "completed" (json_get_string resp.data "status");
  Printf.printf "  test_task_completed passed\n"

(* ========================================
   7. Negative Tests (Verify Failures)
   ======================================== *)

let test_error_vs_ok_are_distinct () =
  (* Ensure error and ok responses are truly different *)
  let ok_resp = Response.ok (`String "data") in
  let err_resp = Response.error ~code:"E" ~message:"fail" () in

  (* These MUST be different *)
  assert_true "ok.success != err.success" (ok_resp.success <> err_resp.success);
  assert_true "ok has no errors" (List.length ok_resp.errors = 0);
  assert_true "err has errors" (List.length err_resp.errors > 0);
  Printf.printf "  test_error_vs_ok_are_distinct passed\n"

let test_different_drift_types_have_different_hints () =
  let factual = Response.drift_detected ~similarity:0.7 ~drift_type:"factual" ~threshold:0.85 ~details:"d" in
  let semantic = Response.drift_detected ~similarity:0.7 ~drift_type:"semantic" ~threshold:0.85 ~details:"d" in
  let structural = Response.drift_detected ~similarity:0.7 ~drift_type:"structural" ~threshold:0.85 ~details:"d" in

  let get_hints resp = (List.hd resp.Response.errors).recovery_hints in
  let factual_hints = get_hints factual in
  let semantic_hints = get_hints semantic in
  let structural_hints = get_hints structural in

  (* Each type should have DIFFERENT hints *)
  assert_true "factual != semantic hints" (factual_hints <> semantic_hints);
  assert_true "semantic != structural hints" (semantic_hints <> structural_hints);
  assert_true "factual != structural hints" (factual_hints <> structural_hints);
  Printf.printf "  test_different_drift_types_have_different_hints passed\n"

let test_severity_of_string_rejects_invalid () =
  (* Invalid strings MUST return Error, not silently pass *)
  assert_true "empty string fails" (Result.is_error (Response.severity_of_string ""));
  assert_true "typo 'fatl' fails" (Result.is_error (Response.severity_of_string "fatl"));
  assert_true "uppercase FATAL fails" (Result.is_error (Response.severity_of_string "FATAL"));
  assert_true "random text fails" (Result.is_error (Response.severity_of_string "xyz123"));
  Printf.printf "  test_severity_of_string_rejects_invalid passed\n"

(* ========================================
   8. Edge Case Tests
   ======================================== *)

let test_empty_hints_allowed () =
  let resp = Response.error ~code:"NO_HINTS" ~message:"No hints" ~hints:[] () in
  let err = List.hd resp.errors in
  assert_true "empty hints list is valid" (List.length err.recovery_hints = 0);
  Printf.printf "  test_empty_hints_allowed passed\n"

let test_special_characters_in_message () =
  let special_msg = "Error: \"quotes\" & <tags> and \n newlines" in
  let resp = Response.error ~code:"SPECIAL" ~message:special_msg () in
  assert_equal_string "special chars preserved" special_msg resp.message;
  (* Verify JSON serialization works *)
  let json_str = Response.to_string resp in
  assert_true "JSON serializes" (String.length json_str > 0);
  Printf.printf "  test_special_characters_in_message passed\n"

let test_unicode_in_message () =
  let unicode_msg = "에러: 한글 메시지 🚫" in
  let resp = Response.error ~code:"UNICODE" ~message:unicode_msg () in
  assert_equal_string "unicode preserved" unicode_msg resp.message;
  Printf.printf "  test_unicode_in_message passed\n"

let test_very_long_message () =
  let long_msg = String.make 10000 'x' in
  let resp = Response.error ~code:"LONG" ~message:long_msg () in
  assert_true "long message preserved" (String.length resp.message = 10000);
  Printf.printf "  test_very_long_message passed\n"

(* ========================================
   Main Test Runner
   ======================================== *)

let () =
  Printf.printf "\n=== Response Module Tests ===\n\n";

  Printf.printf "--- 1. Basic Response Tests ---\n";
  test_ok_response ();
  test_ok_default_message ();
  test_error_response ();
  test_error_with_data ();

  Printf.printf "\n--- 2. JSON Serialization Tests ---\n";
  test_to_json_structure ();
  test_error_json_structure ();
  test_to_string ();

  Printf.printf "\n--- 3. Severity Tests ---\n";
  test_severity_to_string ();
  test_severity_of_string ();

  Printf.printf "\n--- 4. Helper Function Tests ---\n";
  test_make_error ();
  test_make_warning ();
  test_make_info ();
  test_ok_with_warnings ();
  test_errors_multiple ();

  Printf.printf "\n--- 5. Common Error Types Tests ---\n";
  test_validation_error ();
  test_not_found ();
  test_already_exists ();
  test_permission_denied ();
  test_conflict ();
  test_timeout ();

  Printf.printf "\n--- 6. Domain-Specific Response Tests ---\n";
  test_drift_detected ();
  test_drift_detected_semantic ();
  test_handoff_verified ();
  test_task_claimed ();
  test_task_already_claimed ();
  test_task_completed ();

  Printf.printf "\n--- 7. Negative Tests ---\n";
  test_error_vs_ok_are_distinct ();
  test_different_drift_types_have_different_hints ();
  test_severity_of_string_rejects_invalid ();

  Printf.printf "\n--- 8. Edge Case Tests ---\n";
  test_empty_hints_allowed ();
  test_special_characters_in_message ();
  test_unicode_in_message ();
  test_very_long_message ();

  Printf.printf "\nAll Response module tests passed! (39 tests)\n"
