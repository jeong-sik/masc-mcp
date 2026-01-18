(** DTLS Crypto Tests - AES-GCM Encryption/Decryption *)

open Alcotest

(* === Cipher Suite Tests === *)

let test_cipher_suite_conversion () =
  let open Masc_mcp.Dtls_crypto in
  check int "TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256"
    0xC02F (cipher_suite_to_int TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256);
  check int "TLS_NULL"
    0x0000 (cipher_suite_to_int TLS_NULL_WITH_NULL_NULL)

let test_cipher_suite_of_int () =
  let open Masc_mcp.Dtls_crypto in
  check (option (Alcotest.testable pp_cipher_suite (=)))
    "0xC02F" (Some TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256)
    (cipher_suite_of_int 0xC02F);
  check (option (Alcotest.testable pp_cipher_suite (=)))
    "unknown" None (cipher_suite_of_int 0xFFFF)

let test_key_size () =
  let open Masc_mcp.Dtls_crypto in
  check int "AES-128" 16 (key_size_of_cipher_suite TLS_RSA_WITH_AES_128_GCM_SHA256);
  check int "AES-256" 32 (key_size_of_cipher_suite TLS_RSA_WITH_AES_256_GCM_SHA384);
  check int "NULL" 0 (key_size_of_cipher_suite TLS_NULL_WITH_NULL_NULL)

(* === Context Tests === *)

let test_create_context () =
  let open Masc_mcp.Dtls_crypto in
  let ctx = create ~is_client:true in
  check bool "not encrypted initially" false (is_encrypted ctx);
  check (option (Alcotest.testable pp_cipher_suite (=)))
    "no cipher suite" None (get_cipher_suite ctx)

let test_random_generation () =
  let open Masc_mcp.Dtls_crypto in
  let random = generate_random () in
  check int "random length" 32 (Cstruct.length random);
  (* Generate another and verify they're different *)
  let random2 = generate_random () in
  check bool "randoms are different"
    false (Cstruct.equal random random2)

let test_random_bytes () =
  let open Masc_mcp.Dtls_crypto in
  let bytes = random_bytes 16 in
  check int "bytes length" 16 (Cstruct.length bytes)

(* === Key Derivation Tests === *)

let test_derive_master_secret () =
  let open Masc_mcp.Dtls_crypto in
  let pre_master = Cstruct.of_string (String.make 48 'p') in
  let client_random = Cstruct.of_string (String.make 32 'c') in
  let server_random = Cstruct.of_string (String.make 32 's') in
  let master = derive_master_secret ~pre_master_secret:pre_master
    ~client_random ~server_random in
  check int "master secret length" 48 (Cstruct.length master);
  (* Verify deterministic *)
  let master2 = derive_master_secret ~pre_master_secret:pre_master
    ~client_random ~server_random in
  check bool "deterministic" true (Cstruct.equal master master2)

let test_derive_key_material () =
  let open Masc_mcp.Dtls_crypto in
  let master = Cstruct.of_string (String.make 48 'm') in
  let client_random = Cstruct.of_string (String.make 32 'c') in
  let server_random = Cstruct.of_string (String.make 32 's') in
  let km = derive_key_material ~master_secret:master
    ~server_random ~client_random
    ~cipher_suite:TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 in
  check int "client key" 16 (Cstruct.length km.client_write_key);
  check int "server key" 16 (Cstruct.length km.server_write_key);
  check int "client iv" 4 (Cstruct.length km.client_write_iv);
  check int "server iv" 4 (Cstruct.length km.server_write_iv)

(* === Encryption/Decryption Tests === *)

let test_encrypt_decrypt_roundtrip () =
  let open Masc_mcp.Dtls_crypto in
  (* Setup contexts *)
  let client_ctx = create ~is_client:true in
  let server_ctx = create ~is_client:false in

  (* Setup encryption with same keys *)
  let pre_master = random_bytes 48 in
  let client_random = generate_random () in
  let server_random = generate_random () in
  let master = derive_master_secret ~pre_master_secret:pre_master
    ~client_random ~server_random in

  set_params client_ctx
    ~cipher_suite:TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
    ~master_secret:master
    ~client_random ~server_random;

  set_params server_ctx
    ~cipher_suite:TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
    ~master_secret:master
    ~client_random ~server_random;

  (* Test data *)
  let plaintext = Cstruct.of_string "Hello, WebRTC DataChannel!" in

  (* Client encrypts *)
  let encrypted = encrypt_record client_ctx
    ~epoch:1
    ~seq_num:0L
    ~content_type:23  (* ApplicationData *)
    ~version_major:254
    ~version_minor:253
    ~plaintext in

  (match encrypted with
   | Error e -> fail ("Encryption failed: " ^ e)
   | Ok ciphertext ->
     (* Verify ciphertext is different from plaintext *)
     check bool "ciphertext different"
       false (Cstruct.equal plaintext ciphertext);

     (* Server decrypts *)
     let decrypted = decrypt_record server_ctx
       ~epoch:1
       ~seq_num:0L
       ~content_type:23
       ~version_major:254
       ~version_minor:253
       ~ciphertext in

     match decrypted with
     | Error e -> fail ("Decryption failed: " ^ e)
     | Ok result ->
       check bool "roundtrip matches"
         true (Cstruct.equal plaintext result))

let test_no_encryption_passthrough () =
  let open Masc_mcp.Dtls_crypto in
  let ctx = create ~is_client:true in
  (* No encryption setup - should pass through *)
  let data = Cstruct.of_string "unencrypted data" in
  match encrypt_record ctx ~epoch:0 ~seq_num:0L ~content_type:23
    ~version_major:254 ~version_minor:253 ~plaintext:data with
  | Error e -> fail ("Unexpected error: " ^ e)
  | Ok result ->
    check bool "passthrough" true (Cstruct.equal data result)

let test_authentication_failure () =
  let open Masc_mcp.Dtls_crypto in
  let client_ctx = create ~is_client:true in
  let server_ctx = create ~is_client:false in

  (* Setup with DIFFERENT keys *)
  let client_random = generate_random () in
  let server_random = generate_random () in

  let client_master = derive_master_secret
    ~pre_master_secret:(random_bytes 48)
    ~client_random ~server_random in
  let server_master = derive_master_secret
    ~pre_master_secret:(random_bytes 48)  (* Different pre-master! *)
    ~client_random ~server_random in

  set_params client_ctx
    ~cipher_suite:TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
    ~master_secret:client_master
    ~client_random ~server_random;

  set_params server_ctx
    ~cipher_suite:TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
    ~master_secret:server_master
    ~client_random ~server_random;

  let plaintext = Cstruct.of_string "test message" in

  match encrypt_record client_ctx ~epoch:1 ~seq_num:0L ~content_type:23
    ~version_major:254 ~version_minor:253 ~plaintext with
  | Error _ -> fail "Encryption should succeed"
  | Ok ciphertext ->
    match decrypt_record server_ctx ~epoch:1 ~seq_num:0L ~content_type:23
      ~version_major:254 ~version_minor:253 ~ciphertext with
    | Ok _ -> fail "Decryption should fail with wrong key"
    | Error _ -> ()  (* Expected *)

(* === Test Runner === *)

let () =
  run "DTLS Crypto" [
    "cipher_suite", [
      test_case "conversion" `Quick test_cipher_suite_conversion;
      test_case "of_int" `Quick test_cipher_suite_of_int;
      test_case "key_size" `Quick test_key_size;
    ];
    "context", [
      test_case "create" `Quick test_create_context;
      test_case "random generation" `Quick test_random_generation;
      test_case "random bytes" `Quick test_random_bytes;
    ];
    "key_derivation", [
      test_case "master secret" `Quick test_derive_master_secret;
      test_case "key material" `Quick test_derive_key_material;
    ];
    "encryption", [
      test_case "roundtrip" `Quick test_encrypt_decrypt_roundtrip;
      test_case "passthrough" `Quick test_no_encryption_passthrough;
      test_case "auth failure" `Quick test_authentication_failure;
    ];
  ]
