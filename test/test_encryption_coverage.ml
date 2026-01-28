(** Encryption Module Coverage Tests

    Tests for MASC Encryption - AES-256-GCM for data protection:
    - config: enabled, key_source, version
    - envelope: encrypted, version, nonce, ciphertext, adata
    - encryption_error: KeyNotFound, InvalidKeyLength, DecryptionFailed
    - show_encryption_error
    - base64_encode/decode
    - initialize / rng state
*)

open Alcotest

module Encryption = Masc_mcp.Encryption

(* ============================================================
   config Tests
   ============================================================ *)

let test_default_config_enabled () =
  let c = Encryption.default_config in
  check bool "disabled by default" false c.enabled

let test_default_config_version () =
  let c = Encryption.default_config in
  check int "version 1" 1 c.version

let test_default_config_key_source () =
  let c = Encryption.default_config in
  match c.key_source with
  | `Env var_name -> check string "env var" "MASC_ENCRYPTION_KEY" var_name
  | _ -> fail "expected Env key source"

let test_custom_config_direct_key () =
  let c : Encryption.config = {
    enabled = true;
    key_source = `Direct "01234567890123456789012345678901";  (* 32 bytes *)
    version = 2;
  } in
  check bool "enabled" true c.enabled;
  check int "version" 2 c.version

let test_custom_config_file_source () =
  let c : Encryption.config = {
    enabled = true;
    key_source = `File "/tmp/key.txt";
    version = 1;
  } in
  match c.key_source with
  | `File path -> check string "file path" "/tmp/key.txt" path
  | _ -> fail "expected File key source"

(* ============================================================
   encryption_error Tests
   ============================================================ *)

let test_error_key_not_found () =
  let e = Encryption.KeyNotFound "missing key" in
  let s = Encryption.show_encryption_error e in
  check bool "contains KeyNotFound" true (String.length s > 0);
  check bool "contains message" true (String.sub s 0 11 = "KeyNotFound")

let test_error_invalid_key_length () =
  let e = Encryption.InvalidKeyLength 16 in
  let s = Encryption.show_encryption_error e in
  check bool "contains length" true (String.length s > 0);
  check bool "contains 16" true (String.length s > 0 && String.sub s 0 16 = "InvalidKeyLength")

let test_error_decryption_failed () =
  let e = Encryption.DecryptionFailed in
  let s = Encryption.show_encryption_error e in
  check bool "contains Decryption" true (String.sub s 0 16 = "DecryptionFailed")

let test_error_invalid_envelope () =
  let e = Encryption.InvalidEnvelope "bad format" in
  let s = Encryption.show_encryption_error e in
  check bool "contains Invalid" true (String.sub s 0 15 = "InvalidEnvelope")

let test_error_rng_not_initialized () =
  let e = Encryption.RngNotInitialized in
  let s = Encryption.show_encryption_error e in
  check bool "contains RNG" true (String.sub s 0 17 = "RngNotInitialized")

(* ============================================================
   envelope Tests
   ============================================================ *)

let test_envelope_creation () =
  let e : Encryption.envelope = {
    encrypted = true;
    version = 1;
    nonce = "YmFzZTY0bm9uY2U=";
    ciphertext = "Y2lwaGVydGV4dA==";
    adata = "room:default";
  } in
  check bool "encrypted" true e.encrypted;
  check int "version" 1 e.version;
  check string "adata" "room:default" e.adata

let test_envelope_unencrypted () =
  let e : Encryption.envelope = {
    encrypted = false;
    version = 1;
    nonce = "";
    ciphertext = "";
    adata = "";
  } in
  check bool "not encrypted" false e.encrypted

(* ============================================================
   initialize Tests
   ============================================================ *)

let test_initialize () =
  (* Should not throw *)
  Encryption.initialize ();
  (* Multiple calls should be safe *)
  Encryption.initialize ();
  ()

(* ============================================================
   base64 Tests
   ============================================================ *)

let test_base64_encode () =
  let cs = Cstruct.of_string "hello" in
  let encoded = Encryption.base64_encode cs in
  check string "encoded" "aGVsbG8=" encoded

let test_base64_encode_empty () =
  let cs = Cstruct.of_string "" in
  let encoded = Encryption.base64_encode cs in
  check string "empty encoded" "" encoded

let test_base64_encode_binary () =
  let cs = Cstruct.of_string "\x00\x01\x02\xff" in
  let encoded = Encryption.base64_encode cs in
  check bool "has output" true (String.length encoded > 0)

let test_base64_decode_valid () =
  match Encryption.base64_decode "aGVsbG8=" with
  | Some cs -> check string "decoded" "hello" (Cstruct.to_string cs)
  | None -> fail "decode failed"

let test_base64_decode_empty () =
  match Encryption.base64_decode "" with
  | Some cs -> check int "empty decoded length" 0 (Cstruct.length cs)
  | None -> fail "empty decode failed"

let test_base64_decode_invalid () =
  match Encryption.base64_decode "invalid!!!" with
  | Some _ -> fail "should reject invalid base64"
  | None -> ()

let test_base64_roundtrip () =
  let original = "test data 123" in
  let cs = Cstruct.of_string original in
  let encoded = Encryption.base64_encode cs in
  match Encryption.base64_decode encoded with
  | Some decoded -> check string "roundtrip" original (Cstruct.to_string decoded)
  | None -> fail "roundtrip failed"

let test_base64_roundtrip_binary () =
  let original = "\x00\x01\x02\x03\xff\xfe\xfd" in
  let cs = Cstruct.of_string original in
  let encoded = Encryption.base64_encode cs in
  match Encryption.base64_decode encoded with
  | Some decoded -> check string "binary roundtrip" original (Cstruct.to_string decoded)
  | None -> fail "binary roundtrip failed"

(* ============================================================
   load_key Tests
   ============================================================ *)

let test_load_key_direct_valid () =
  Encryption.initialize ();
  let key_string = String.make 32 'x' in
  let config : Encryption.config = {
    enabled = true;
    key_source = `Direct key_string;
    version = 1;
  } in
  match Encryption.load_key config with
  | Ok _ -> ()  (* Key loaded successfully *)
  | Error e -> fail (Encryption.show_encryption_error e)

let test_load_key_direct_wrong_length () =
  let config : Encryption.config = {
    enabled = true;
    key_source = `Direct "too_short";
    version = 1;
  } in
  match Encryption.load_key config with
  | Ok _ -> fail "should reject wrong length"
  | Error (Encryption.InvalidKeyLength n) -> check bool "short length" true (n < 32)
  | Error _ -> fail "unexpected error type"

let test_load_key_env_not_found () =
  let config : Encryption.config = {
    enabled = true;
    key_source = `Env "NONEXISTENT_ENV_VAR_12345";
    version = 1;
  } in
  match Encryption.load_key config with
  | Ok _ -> fail "should fail for missing env"
  | Error (Encryption.KeyNotFound _) -> ()
  | Error _ -> fail "unexpected error type"

let test_load_key_file_not_found () =
  let config : Encryption.config = {
    enabled = true;
    key_source = `File "/nonexistent/path/key.txt";
    version = 1;
  } in
  match Encryption.load_key config with
  | Ok _ -> fail "should fail for missing file"
  | Error (Encryption.KeyNotFound _) -> ()
  | Error _ -> fail "unexpected error type"

(* ============================================================
   envelope_to_json / envelope_of_json Tests
   ============================================================ *)

let test_envelope_to_json () =
  let env : Encryption.envelope = {
    encrypted = true;
    version = 1;
    nonce = "bm9uY2U=";
    ciphertext = "Y3Q=";
    adata = "test";
  } in
  let json = Encryption.envelope_to_json env in
  let open Yojson.Safe.Util in
  check bool "_encrypted" true (json |> member "_encrypted" |> to_bool);
  check int "version" 1 (json |> member "v" |> to_int);
  check string "nonce" "bm9uY2U=" (json |> member "nonce" |> to_string);
  check string "ct" "Y3Q=" (json |> member "ct" |> to_string);
  check string "adata" "test" (json |> member "adata" |> to_string)

let test_envelope_of_json_valid () =
  let json = `Assoc [
    ("_encrypted", `Bool true);
    ("v", `Int 2);
    ("nonce", `String "abc");
    ("ct", `String "xyz");
    ("adata", `String "data");
  ] in
  match Encryption.envelope_of_json json with
  | Some env ->
      check bool "encrypted" true env.encrypted;
      check int "version" 2 env.version;
      check string "nonce" "abc" env.nonce;
      check string "ciphertext" "xyz" env.ciphertext;
      check string "adata" "data" env.adata
  | None -> fail "should parse valid json"

let test_envelope_of_json_invalid () =
  let json = `Assoc [("foo", `String "bar")] in
  match Encryption.envelope_of_json json with
  | Some _ -> fail "should reject invalid json"
  | None -> ()

let test_envelope_of_json_wrong_type () =
  let json = `String "not an object" in
  match Encryption.envelope_of_json json with
  | Some _ -> fail "should reject non-object"
  | None -> ()

let test_envelope_roundtrip () =
  let original : Encryption.envelope = {
    encrypted = true;
    version = 3;
    nonce = "dGVzdG5vbmNl";
    ciphertext = "Y2lwaGVy";
    adata = "roundtrip";
  } in
  let json = Encryption.envelope_to_json original in
  match Encryption.envelope_of_json json with
  | Some restored ->
      check bool "encrypted" original.encrypted restored.encrypted;
      check int "version" original.version restored.version;
      check string "nonce" original.nonce restored.nonce;
      check string "ciphertext" original.ciphertext restored.ciphertext;
      check string "adata" original.adata restored.adata
  | None -> fail "envelope roundtrip failed"

(* ============================================================
   is_encrypted_json Tests
   ============================================================ *)

let test_is_encrypted_json_true () =
  let json = `Assoc [("_encrypted", `Bool true); ("other", `Int 1)] in
  check bool "is encrypted" true (Encryption.is_encrypted_json json)

let test_is_encrypted_json_false () =
  let json = `Assoc [("_encrypted", `Bool false)] in
  check bool "not encrypted" false (Encryption.is_encrypted_json json)

let test_is_encrypted_json_missing () =
  let json = `Assoc [("foo", `String "bar")] in
  check bool "no field" false (Encryption.is_encrypted_json json)

let test_is_encrypted_json_wrong_type () =
  let json = `Assoc [("_encrypted", `String "true")] in
  check bool "wrong type" false (Encryption.is_encrypted_json json)

let test_is_encrypted_json_not_object () =
  let json = `List [`Bool true] in
  check bool "not object" false (Encryption.is_encrypted_json json)

(* ============================================================
   generate_nonce Tests
   ============================================================ *)

let test_generate_nonce_after_init () =
  Encryption.initialize ();
  match Encryption.generate_nonce () with
  | Ok nonce ->
      check int "12 bytes" 12 (String.length nonce)
  | Error e -> fail (Encryption.show_encryption_error e)

let test_generate_nonce_unique () =
  Encryption.initialize ();
  match Encryption.generate_nonce (), Encryption.generate_nonce () with
  | Ok n1, Ok n2 ->
      check bool "different nonces" true (n1 <> n2)
  | _ -> fail "nonce generation failed"

(* ============================================================
   generate_key_hex Tests
   ============================================================ *)

let test_generate_key_hex () =
  Encryption.initialize ();
  match Encryption.generate_key_hex () with
  | Ok hex ->
      check int "64 hex chars" 64 (String.length hex);
      (* Verify all characters are hex *)
      let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
      check bool "all hex" true (String.for_all is_hex hex)
  | Error e -> fail (Encryption.show_encryption_error e)

let test_generate_key_hex_unique () =
  Encryption.initialize ();
  match Encryption.generate_key_hex (), Encryption.generate_key_hex () with
  | Ok k1, Ok k2 ->
      check bool "different keys" true (k1 <> k2)
  | _ -> fail "key generation failed"

(* ============================================================
   get_status Tests
   ============================================================ *)

let test_get_status_disabled () =
  let config : Encryption.config = {
    enabled = false;
    key_source = `Env "NONEXISTENT_VAR";
    version = 1;
  } in
  let status = Encryption.get_status config in
  let open Yojson.Safe.Util in
  check bool "enabled" false (status |> member "enabled" |> to_bool);
  check int "version" 1 (status |> member "version" |> to_int);
  check bool "has rng_initialized" true (status |> member "rng_initialized" |> to_bool |> ignore; true);
  check bool "has key_status" true (String.length (status |> member "key_status" |> to_string) > 0)

let test_get_status_with_valid_key () =
  Encryption.initialize ();
  let config : Encryption.config = {
    enabled = true;
    key_source = `Direct (String.make 32 'k');
    version = 2;
  } in
  let status = Encryption.get_status config in
  let open Yojson.Safe.Util in
  check bool "enabled" true (status |> member "enabled" |> to_bool);
  check int "version" 2 (status |> member "version" |> to_int);
  check string "key_status" "loaded" (status |> member "key_status" |> to_string)

let test_get_status_with_invalid_key () =
  let config : Encryption.config = {
    enabled = true;
    key_source = `Direct "short";
    version = 1;
  } in
  let status = Encryption.get_status config in
  let open Yojson.Safe.Util in
  let key_status = status |> member "key_status" |> to_string in
  check bool "shows error" true (String.length key_status > 0 && key_status <> "loaded")

(* ============================================================
   encrypt_json / decrypt_envelope Tests
   ============================================================ *)

let test_encrypt_decrypt_roundtrip () =
  Encryption.initialize ();
  let key_string = String.make 32 'x' in
  let config : Encryption.config = {
    enabled = true;
    key_source = `Direct key_string;
    version = 1;
  } in
  match Encryption.load_key config with
  | Error e -> fail (Encryption.show_encryption_error e)
  | Ok key ->
      let original = `Assoc [("message", `String "hello"); ("count", `Int 42)] in
      match Encryption.encrypt_json ~key ~adata:"test" original with
      | Error e -> fail (Encryption.show_encryption_error e)
      | Ok envelope ->
          check bool "encrypted flag" true envelope.encrypted;
          check string "adata" "test" envelope.adata;
          match Encryption.decrypt_envelope ~key envelope with
          | Error e -> fail (Encryption.show_encryption_error e)
          | Ok decrypted ->
              check string "roundtrip" (Yojson.Safe.to_string original) (Yojson.Safe.to_string decrypted)

let test_decrypt_unencrypted_envelope () =
  Encryption.initialize ();
  let key_string = String.make 32 'x' in
  let config : Encryption.config = {
    enabled = true;
    key_source = `Direct key_string;
    version = 1;
  } in
  match Encryption.load_key config with
  | Error _ -> fail "key load failed"
  | Ok key ->
      let envelope : Encryption.envelope = {
        encrypted = false;
        version = 1;
        nonce = "";
        ciphertext = "";
        adata = "";
      } in
      match Encryption.decrypt_envelope ~key envelope with
      | Ok _ -> fail "should reject unencrypted"
      | Error (Encryption.InvalidEnvelope msg) ->
          check bool "correct error" true (String.length msg > 0)
      | Error _ -> fail "wrong error type"

let test_decrypt_invalid_nonce () =
  Encryption.initialize ();
  let key_string = String.make 32 'x' in
  let config : Encryption.config = {
    enabled = true;
    key_source = `Direct key_string;
    version = 1;
  } in
  match Encryption.load_key config with
  | Error _ -> fail "key load failed"
  | Ok key ->
      let envelope : Encryption.envelope = {
        encrypted = true;
        version = 1;
        nonce = "!!!invalid!!!";
        ciphertext = "Y3Q=";
        adata = "test";
      } in
      match Encryption.decrypt_envelope ~key envelope with
      | Ok _ -> fail "should reject invalid nonce"
      | Error (Encryption.InvalidEnvelope _) -> ()
      | Error _ -> fail "wrong error type"

let test_decrypt_invalid_ciphertext () =
  Encryption.initialize ();
  let key_string = String.make 32 'x' in
  let config : Encryption.config = {
    enabled = true;
    key_source = `Direct key_string;
    version = 1;
  } in
  match Encryption.load_key config with
  | Error _ -> fail "key load failed"
  | Ok key ->
      let envelope : Encryption.envelope = {
        encrypted = true;
        version = 1;
        nonce = "YWFhYWFhYWFhYWFh";  (* valid base64 for "aaaaaaaaaaaa" *)
        ciphertext = "!!!invalid!!!";
        adata = "test";
      } in
      match Encryption.decrypt_envelope ~key envelope with
      | Ok _ -> fail "should reject invalid ciphertext"
      | Error (Encryption.InvalidEnvelope _) -> ()
      | Error _ -> fail "wrong error type"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Encryption Coverage" [
    "config", [
      test_case "default enabled" `Quick test_default_config_enabled;
      test_case "default version" `Quick test_default_config_version;
      test_case "default key source" `Quick test_default_config_key_source;
      test_case "custom direct key" `Quick test_custom_config_direct_key;
      test_case "custom file source" `Quick test_custom_config_file_source;
    ];
    "encryption_error", [
      test_case "key not found" `Quick test_error_key_not_found;
      test_case "invalid key length" `Quick test_error_invalid_key_length;
      test_case "decryption failed" `Quick test_error_decryption_failed;
      test_case "invalid envelope" `Quick test_error_invalid_envelope;
      test_case "rng not initialized" `Quick test_error_rng_not_initialized;
    ];
    "envelope", [
      test_case "creation" `Quick test_envelope_creation;
      test_case "unencrypted" `Quick test_envelope_unencrypted;
    ];
    "initialize", [
      test_case "initialize" `Quick test_initialize;
    ];
    "base64.encode", [
      test_case "encode" `Quick test_base64_encode;
      test_case "encode empty" `Quick test_base64_encode_empty;
      test_case "encode binary" `Quick test_base64_encode_binary;
    ];
    "base64.decode", [
      test_case "decode valid" `Quick test_base64_decode_valid;
      test_case "decode empty" `Quick test_base64_decode_empty;
      test_case "decode invalid" `Quick test_base64_decode_invalid;
    ];
    "base64.roundtrip", [
      test_case "text" `Quick test_base64_roundtrip;
      test_case "binary" `Quick test_base64_roundtrip_binary;
    ];
    "load_key", [
      test_case "direct valid" `Quick test_load_key_direct_valid;
      test_case "direct wrong length" `Quick test_load_key_direct_wrong_length;
      test_case "env not found" `Quick test_load_key_env_not_found;
      test_case "file not found" `Quick test_load_key_file_not_found;
    ];
    "envelope_json", [
      test_case "to_json" `Quick test_envelope_to_json;
      test_case "of_json valid" `Quick test_envelope_of_json_valid;
      test_case "of_json invalid" `Quick test_envelope_of_json_invalid;
      test_case "of_json wrong type" `Quick test_envelope_of_json_wrong_type;
      test_case "roundtrip" `Quick test_envelope_roundtrip;
    ];
    "is_encrypted_json", [
      test_case "true" `Quick test_is_encrypted_json_true;
      test_case "false" `Quick test_is_encrypted_json_false;
      test_case "missing" `Quick test_is_encrypted_json_missing;
      test_case "wrong type" `Quick test_is_encrypted_json_wrong_type;
      test_case "not object" `Quick test_is_encrypted_json_not_object;
    ];
    "generate_nonce", [
      test_case "after init" `Quick test_generate_nonce_after_init;
      test_case "unique" `Quick test_generate_nonce_unique;
    ];
    "generate_key_hex", [
      test_case "basic" `Quick test_generate_key_hex;
      test_case "unique" `Quick test_generate_key_hex_unique;
    ];
    "get_status", [
      test_case "disabled" `Quick test_get_status_disabled;
      test_case "valid key" `Quick test_get_status_with_valid_key;
      test_case "invalid key" `Quick test_get_status_with_invalid_key;
    ];
    "encrypt_decrypt", [
      test_case "roundtrip" `Quick test_encrypt_decrypt_roundtrip;
      test_case "unencrypted envelope" `Quick test_decrypt_unencrypted_envelope;
      test_case "invalid nonce" `Quick test_decrypt_invalid_nonce;
      test_case "invalid ciphertext" `Quick test_decrypt_invalid_ciphertext;
    ];
  ]
