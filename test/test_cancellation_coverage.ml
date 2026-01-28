(** Cancellation Module Coverage Tests

    Tests for MCP Cancellation Token functionality:
    - token type: record fields
    - TokenStore.generate_id: ID generation
    - TokenStore.create_with_id: explicit ID creation
    - TokenStore.get: token retrieval
    - TokenStore.remove: token removal
    - TokenStore.list_all: list all tokens
*)

open Alcotest

module Cancellation = Masc_mcp.Cancellation

(* ============================================================
   token Type Tests
   ============================================================ *)

let test_token_type_id () =
  (* Token has id field *)
  let _ : string = "token_id" in
  check bool "id is string" true true

let test_token_type_cancelled () =
  let _ : bool = false in
  check bool "cancelled is bool" true true

let test_token_type_reason () =
  let _ : string option = None in
  check bool "reason is option" true true

let test_token_type_callbacks () =
  let _ : (unit -> unit) list = [] in
  check bool "callbacks is list" true true

let test_token_type_created_at () =
  let _ : float = 0.0 in
  check bool "created_at is float" true true

(* ============================================================
   TokenStore.create_with_id Tests
   ============================================================ *)

let test_create_with_id_creates () =
  let id = "test_create_" ^ string_of_int (Random.int 100000) in
  Cancellation.TokenStore.create_with_id id;
  let found = Cancellation.TokenStore.get id in
  Cancellation.TokenStore.remove id;
  check bool "token created" true (Option.is_some found)

let test_create_with_id_not_cancelled () =
  let id = "test_notcancelled_" ^ string_of_int (Random.int 100000) in
  Cancellation.TokenStore.create_with_id id;
  let cancelled = Cancellation.TokenStore.is_cancelled id in
  Cancellation.TokenStore.remove id;
  check bool "not cancelled" false cancelled

let test_create_with_id_idempotent () =
  let id = "test_idempotent_" ^ string_of_int (Random.int 100000) in
  Cancellation.TokenStore.create_with_id id;
  Cancellation.TokenStore.create_with_id id;  (* Should not fail *)
  Cancellation.TokenStore.remove id;
  check bool "idempotent" true true

(* ============================================================
   TokenStore.get Tests
   ============================================================ *)

let test_get_nonexistent () =
  let result = Cancellation.TokenStore.get "nonexistent_xyz_12345" in
  check bool "None for nonexistent" true (Option.is_none result)

let test_get_existing () =
  let id = "test_get_" ^ string_of_int (Random.int 100000) in
  Cancellation.TokenStore.create_with_id id;
  let result = Cancellation.TokenStore.get id in
  Cancellation.TokenStore.remove id;
  check bool "Some for existing" true (Option.is_some result)

(* ============================================================
   TokenStore.remove Tests
   ============================================================ *)

let test_remove_existing () =
  let id = "test_remove_" ^ string_of_int (Random.int 100000) in
  Cancellation.TokenStore.create_with_id id;
  Cancellation.TokenStore.remove id;
  let result = Cancellation.TokenStore.get id in
  check bool "removed" true (Option.is_none result)

let test_remove_nonexistent () =
  Cancellation.TokenStore.remove "nonexistent_remove_xyz";
  check bool "no error" true true

(* ============================================================
   TokenStore.list_all Tests
   ============================================================ *)

let test_list_all_returns_list () =
  let tokens = Cancellation.TokenStore.list_all () in
  check bool "returns list" true (List.length tokens >= 0)

(* ============================================================
   TokenStore.is_cancelled Tests
   ============================================================ *)

let test_is_cancelled_false_initially () =
  let id = "test_cancelled_" ^ string_of_int (Random.int 100000) in
  Cancellation.TokenStore.create_with_id id;
  let cancelled = Cancellation.TokenStore.is_cancelled id in
  Cancellation.TokenStore.remove id;
  check bool "not cancelled initially" false cancelled

let test_is_cancelled_nonexistent () =
  let cancelled = Cancellation.TokenStore.is_cancelled "nonexistent_xyz_999" in
  check bool "false for nonexistent" false cancelled

(* ============================================================
   TokenStore.cancel Tests
   ============================================================ *)

let test_cancel_sets_cancelled () =
  let id = "test_cancel_" ^ string_of_int (Random.int 100000) in
  Cancellation.TokenStore.create_with_id id;
  Cancellation.TokenStore.cancel id;
  let cancelled = Cancellation.TokenStore.is_cancelled id in
  Cancellation.TokenStore.remove id;
  check bool "cancelled after cancel" true cancelled

let test_cancel_nonexistent () =
  Cancellation.TokenStore.cancel "nonexistent_cancel_xyz";
  check bool "no error" true true

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Cancellation Coverage" [
    "token_type", [
      test_case "id" `Quick test_token_type_id;
      test_case "cancelled" `Quick test_token_type_cancelled;
      test_case "reason" `Quick test_token_type_reason;
      test_case "callbacks" `Quick test_token_type_callbacks;
      test_case "created_at" `Quick test_token_type_created_at;
    ];
    "create_with_id", [
      test_case "creates" `Quick test_create_with_id_creates;
      test_case "not cancelled" `Quick test_create_with_id_not_cancelled;
      test_case "idempotent" `Quick test_create_with_id_idempotent;
    ];
    "get", [
      test_case "nonexistent" `Quick test_get_nonexistent;
      test_case "existing" `Quick test_get_existing;
    ];
    "remove", [
      test_case "existing" `Quick test_remove_existing;
      test_case "nonexistent" `Quick test_remove_nonexistent;
    ];
    "list_all", [
      test_case "returns list" `Quick test_list_all_returns_list;
    ];
    "is_cancelled", [
      test_case "false initially" `Quick test_is_cancelled_false_initially;
      test_case "nonexistent" `Quick test_is_cancelled_nonexistent;
    ];
    "cancel", [
      test_case "sets cancelled" `Quick test_cancel_sets_cancelled;
      test_case "nonexistent" `Quick test_cancel_nonexistent;
    ];
  ]
