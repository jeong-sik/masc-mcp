(** Tests for Cancellation Tokens - MCP 2025-11-25 Spec *)

open Alcotest

module Cancellation = Masc_mcp.Cancellation
module Encryption = Masc_mcp.Encryption

(* Initialize RNG for crypto *)
let () = Encryption.initialize ()

(* Helper: check if s2 is substring of s1 *)
let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

(** Token creation tests *)
let test_token_create () =
  let token = Cancellation.TokenStore.create () in
  check bool "not cancelled initially" false token.cancelled;
  check (option string) "no reason initially" None token.reason;
  check bool "has id" true (String.length token.id > 0);
  check bool "id starts with cancel_" true (String.sub token.id 0 7 = "cancel_")

let test_token_cancel () =
  let token = Cancellation.TokenStore.create () in
  Cancellation.cancel token;
  check bool "is cancelled" true token.cancelled;
  check (option string) "no reason" None token.reason

let test_token_cancel_with_reason () =
  let token = Cancellation.TokenStore.create () in
  Cancellation.cancel ~reason:"timeout" token;
  check bool "is cancelled" true token.cancelled;
  check (option string) "has reason" (Some "timeout") token.reason

let test_token_callback () =
  let token = Cancellation.TokenStore.create () in
  let callback_called = ref false in
  Cancellation.on_cancel token (fun () -> callback_called := true);
  check bool "callback not called yet" false !callback_called;
  Cancellation.cancel token;
  check bool "callback was called" true !callback_called

let test_token_multiple_callbacks () =
  let token = Cancellation.TokenStore.create () in
  let call_order = ref [] in
  Cancellation.on_cancel token (fun () -> call_order := !call_order @ [1]);
  Cancellation.on_cancel token (fun () -> call_order := !call_order @ [2]);
  Cancellation.on_cancel token (fun () -> call_order := !call_order @ [3]);
  Cancellation.cancel token;
  (* LIFO order: 3, 2, 1 *)
  check (list int) "LIFO callback order" [3; 2; 1] !call_order

let test_token_double_cancel () =
  let token = Cancellation.TokenStore.create () in
  let callback_count = ref 0 in
  Cancellation.on_cancel token (fun () -> incr callback_count);
  Cancellation.cancel token;
  Cancellation.cancel token;  (* Second cancel should be no-op *)
  check int "callback called only once" 1 !callback_count

(** TokenStore tests *)
let test_store_get () =
  let token = Cancellation.TokenStore.create () in
  let found = Cancellation.TokenStore.get token.id in
  check bool "token found" true (Option.is_some found);
  check string "same id" token.id (Option.get found).id

let test_store_remove () =
  let token = Cancellation.TokenStore.create () in
  let id = token.id in
  Cancellation.TokenStore.remove id;
  let found = Cancellation.TokenStore.get id in
  check bool "token removed" true (Option.is_none found)

let test_cancel_by_id () =
  let token = Cancellation.TokenStore.create () in
  let result = Cancellation.cancel_by_id token.id in
  check bool "cancel succeeded" true result;
  check bool "token is cancelled" true token.cancelled

let test_cancel_by_id_not_found () =
  let result = Cancellation.cancel_by_id "nonexistent_id" in
  check bool "cancel failed" false result

let test_is_cancelled () =
  let token = Cancellation.TokenStore.create () in
  check bool "not cancelled" false (Cancellation.is_cancelled token);
  Cancellation.cancel token;
  check bool "is cancelled" true (Cancellation.is_cancelled token)

(** Tool handler tests *)
let test_tool_create () =
  let args = `Assoc [("action", `String "create")] in
  let (success, result) = Cancellation.handle_cancellation_tool args in
  check bool "success" true success;
  check bool "result contains id" true (String.length result > 0)

let test_tool_cancel () =
  let token = Cancellation.TokenStore.create () in
  let args = `Assoc [
    ("action", `String "cancel");
    ("token_id", `String token.id);
  ] in
  let (success, _) = Cancellation.handle_cancellation_tool args in
  check bool "cancel success" true success;
  check bool "token cancelled" true token.cancelled

let test_tool_check () =
  let token = Cancellation.TokenStore.create () in
  let args = `Assoc [
    ("action", `String "check");
    ("token_id", `String token.id);
  ] in
  let (success, result) = Cancellation.handle_cancellation_tool args in
  check bool "check success" true success;
  check bool "contains cancelled field" true (contains result "cancelled")

let test_tool_list () =
  let _ = Cancellation.TokenStore.create () in
  let args = `Assoc [("action", `String "list")] in
  let (success, result) = Cancellation.handle_cancellation_tool args in
  check bool "list success" true success;
  check bool "contains count" true (contains result "count")

let test_tool_invalid_action () =
  let args = `Assoc [("action", `String "invalid")] in
  let (success, _) = Cancellation.handle_cancellation_tool args in
  check bool "invalid action fails" false success

let test_tool_missing_action () =
  let args = `Assoc [] in
  let (success, _) = Cancellation.handle_cancellation_tool args in
  check bool "missing action fails" false success

(** JSON serialization *)
let test_token_to_json () =
  let token = Cancellation.TokenStore.create () in
  let json = Cancellation.token_to_json token in
  match json with
  | `Assoc fields ->
    check bool "has id" true (List.mem_assoc "id" fields);
    check bool "has cancelled" true (List.mem_assoc "cancelled" fields);
    check bool "has created_at" true (List.mem_assoc "created_at" fields)
  | _ -> fail "Expected Assoc"

(** Test suites *)
let token_tests = [
  "create", `Quick, test_token_create;
  "cancel", `Quick, test_token_cancel;
  "cancel with reason", `Quick, test_token_cancel_with_reason;
  "callback", `Quick, test_token_callback;
  "multiple callbacks LIFO", `Quick, test_token_multiple_callbacks;
  "double cancel idempotent", `Quick, test_token_double_cancel;
  "is_cancelled", `Quick, test_is_cancelled;
]

let store_tests = [
  "get", `Quick, test_store_get;
  "remove", `Quick, test_store_remove;
  "cancel_by_id", `Quick, test_cancel_by_id;
  "cancel_by_id not found", `Quick, test_cancel_by_id_not_found;
]

let tool_tests = [
  "create action", `Quick, test_tool_create;
  "cancel action", `Quick, test_tool_cancel;
  "check action", `Quick, test_tool_check;
  "list action", `Quick, test_tool_list;
  "invalid action", `Quick, test_tool_invalid_action;
  "missing action", `Quick, test_tool_missing_action;
]

let json_tests = [
  "token_to_json", `Quick, test_token_to_json;
]

let () =
  run "Cancellation" [
    "token", token_tests;
    "store", store_tests;
    "tool", tool_tests;
    "json", json_tests;
  ]
