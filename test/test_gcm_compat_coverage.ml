(** GCM Compat Module Coverage Tests

    Tests for mirage-crypto AES-GCM wrapper:
    - Module existence
    - Type availability
*)

open Alcotest

module Gcm_compat = Masc_mcp.Gcm_compat

(* ============================================================
   Module Existence Tests
   ============================================================ *)

let test_key_type_exists () =
  (* Gcm_compat.key type should exist *)
  let _ : Gcm_compat.key option = None in
  check bool "key type exists" true true

let test_authenticate_encrypt_exists () =
  (* Function should exist at module level *)
  let _ = Gcm_compat.authenticate_encrypt in
  check bool "authenticate_encrypt exists" true true

let test_authenticate_decrypt_exists () =
  let _ = Gcm_compat.authenticate_decrypt in
  check bool "authenticate_decrypt exists" true true

let test_of_secret_exists () =
  let _ = Gcm_compat.of_secret in
  check bool "of_secret exists" true true

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "GCM Compat Coverage" [
    "module", [
      test_case "key type" `Quick test_key_type_exists;
      test_case "authenticate_encrypt" `Quick test_authenticate_encrypt_exists;
      test_case "authenticate_decrypt" `Quick test_authenticate_decrypt_exists;
      test_case "of_secret" `Quick test_of_secret_exists;
    ];
  ]
