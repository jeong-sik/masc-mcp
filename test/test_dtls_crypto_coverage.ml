(** DTLS Crypto Module Coverage Tests

    Tests for DTLS 1.2 cryptographic functions:
    - cipher_suite type and conversions
    - constants
    - string_of_cipher_suite
*)

open Alcotest

module Dtls_crypto = Masc_mcp.Dtls_crypto

(* ============================================================
   Constants Tests
   ============================================================ *)

let test_aes_128_key_size () =
  check int "aes 128 key" 16 Dtls_crypto.aes_128_key_size

let test_aes_256_key_size () =
  check int "aes 256 key" 32 Dtls_crypto.aes_256_key_size

let test_gcm_iv_size () =
  check int "gcm iv" 4 Dtls_crypto.gcm_iv_size

let test_gcm_nonce_explicit () =
  check int "gcm nonce" 8 Dtls_crypto.gcm_nonce_explicit

let test_gcm_tag_size () =
  check int "gcm tag" 16 Dtls_crypto.gcm_tag_size

let test_master_secret_size () =
  check int "master secret" 48 Dtls_crypto.master_secret_size

let test_random_size () =
  check int "random" 32 Dtls_crypto.random_size

(* ============================================================
   cipher_suite_to_int Tests
   ============================================================ *)

let test_cipher_suite_null () =
  check int "null" 0x0000
    (Dtls_crypto.cipher_suite_to_int Dtls_crypto.TLS_NULL_WITH_NULL_NULL)

let test_cipher_suite_aes_128_gcm () =
  check int "aes 128 gcm" 0x009C
    (Dtls_crypto.cipher_suite_to_int Dtls_crypto.TLS_RSA_WITH_AES_128_GCM_SHA256)

let test_cipher_suite_aes_256_gcm () =
  check int "aes 256 gcm" 0x009D
    (Dtls_crypto.cipher_suite_to_int Dtls_crypto.TLS_RSA_WITH_AES_256_GCM_SHA384)

let test_cipher_suite_ecdhe_128 () =
  check int "ecdhe 128" 0xC02F
    (Dtls_crypto.cipher_suite_to_int Dtls_crypto.TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256)

let test_cipher_suite_ecdhe_256 () =
  check int "ecdhe 256" 0xC030
    (Dtls_crypto.cipher_suite_to_int Dtls_crypto.TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384)

(* ============================================================
   cipher_suite_of_int Tests
   ============================================================ *)

let test_of_int_null () =
  match Dtls_crypto.cipher_suite_of_int 0x0000 with
  | Some Dtls_crypto.TLS_NULL_WITH_NULL_NULL -> check bool "null" true true
  | _ -> fail "expected TLS_NULL"

let test_of_int_aes_128 () =
  match Dtls_crypto.cipher_suite_of_int 0x009C with
  | Some Dtls_crypto.TLS_RSA_WITH_AES_128_GCM_SHA256 -> check bool "aes 128" true true
  | _ -> fail "expected AES_128"

let test_of_int_aes_256 () =
  match Dtls_crypto.cipher_suite_of_int 0x009D with
  | Some Dtls_crypto.TLS_RSA_WITH_AES_256_GCM_SHA384 -> check bool "aes 256" true true
  | _ -> fail "expected AES_256"

let test_of_int_ecdhe_128 () =
  match Dtls_crypto.cipher_suite_of_int 0xC02F with
  | Some Dtls_crypto.TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 -> check bool "ecdhe 128" true true
  | _ -> fail "expected ECDHE_128"

let test_of_int_ecdhe_256 () =
  match Dtls_crypto.cipher_suite_of_int 0xC030 with
  | Some Dtls_crypto.TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384 -> check bool "ecdhe 256" true true
  | _ -> fail "expected ECDHE_256"

let test_of_int_invalid () =
  match Dtls_crypto.cipher_suite_of_int 0xFFFF with
  | None -> check bool "none" true true
  | Some _ -> fail "expected None"

(* ============================================================
   string_of_cipher_suite Tests
   ============================================================ *)

let test_string_null () =
  check string "null string" "TLS_NULL_WITH_NULL_NULL"
    (Dtls_crypto.string_of_cipher_suite Dtls_crypto.TLS_NULL_WITH_NULL_NULL)

let test_string_aes_128 () =
  check string "aes 128 string" "TLS_RSA_WITH_AES_128_GCM_SHA256"
    (Dtls_crypto.string_of_cipher_suite Dtls_crypto.TLS_RSA_WITH_AES_128_GCM_SHA256)

let test_string_aes_256 () =
  check string "aes 256 string" "TLS_RSA_WITH_AES_256_GCM_SHA384"
    (Dtls_crypto.string_of_cipher_suite Dtls_crypto.TLS_RSA_WITH_AES_256_GCM_SHA384)

let test_string_ecdhe_128 () =
  check string "ecdhe 128 string" "TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256"
    (Dtls_crypto.string_of_cipher_suite Dtls_crypto.TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256)

let test_string_ecdhe_256 () =
  check string "ecdhe 256 string" "TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384"
    (Dtls_crypto.string_of_cipher_suite Dtls_crypto.TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384)

(* ============================================================
   Round-trip Tests
   ============================================================ *)

let test_roundtrip_null () =
  let cs = Dtls_crypto.TLS_NULL_WITH_NULL_NULL in
  let n = Dtls_crypto.cipher_suite_to_int cs in
  match Dtls_crypto.cipher_suite_of_int n with
  | Some cs' -> check bool "roundtrip null" (cs = cs') true
  | None -> fail "roundtrip failed"

let test_roundtrip_ecdhe_128 () =
  let cs = Dtls_crypto.TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 in
  let n = Dtls_crypto.cipher_suite_to_int cs in
  match Dtls_crypto.cipher_suite_of_int n with
  | Some cs' -> check bool "roundtrip ecdhe" (cs = cs') true
  | None -> fail "roundtrip failed"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "DTLS Crypto Coverage" [
    "constants", [
      test_case "aes_128_key_size" `Quick test_aes_128_key_size;
      test_case "aes_256_key_size" `Quick test_aes_256_key_size;
      test_case "gcm_iv_size" `Quick test_gcm_iv_size;
      test_case "gcm_nonce_explicit" `Quick test_gcm_nonce_explicit;
      test_case "gcm_tag_size" `Quick test_gcm_tag_size;
      test_case "master_secret_size" `Quick test_master_secret_size;
      test_case "random_size" `Quick test_random_size;
    ];
    "cipher_suite_to_int", [
      test_case "null" `Quick test_cipher_suite_null;
      test_case "aes_128_gcm" `Quick test_cipher_suite_aes_128_gcm;
      test_case "aes_256_gcm" `Quick test_cipher_suite_aes_256_gcm;
      test_case "ecdhe_128" `Quick test_cipher_suite_ecdhe_128;
      test_case "ecdhe_256" `Quick test_cipher_suite_ecdhe_256;
    ];
    "cipher_suite_of_int", [
      test_case "null" `Quick test_of_int_null;
      test_case "aes_128" `Quick test_of_int_aes_128;
      test_case "aes_256" `Quick test_of_int_aes_256;
      test_case "ecdhe_128" `Quick test_of_int_ecdhe_128;
      test_case "ecdhe_256" `Quick test_of_int_ecdhe_256;
      test_case "invalid" `Quick test_of_int_invalid;
    ];
    "string_of_cipher_suite", [
      test_case "null" `Quick test_string_null;
      test_case "aes_128" `Quick test_string_aes_128;
      test_case "aes_256" `Quick test_string_aes_256;
      test_case "ecdhe_128" `Quick test_string_ecdhe_128;
      test_case "ecdhe_256" `Quick test_string_ecdhe_256;
    ];
    "roundtrip", [
      test_case "null" `Quick test_roundtrip_null;
      test_case "ecdhe_128" `Quick test_roundtrip_ecdhe_128;
    ];
  ]
