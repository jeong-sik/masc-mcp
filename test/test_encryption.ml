(** Tests for Encryption module *)

module Encryption = Masc_mcp__Encryption
open Alcotest

(* Initialize RNG once *)
let () = Encryption.initialize ()

(* Test key for testing purposes (32 bytes = 256 bits) *)
let test_key_string = "01234567890123456789012345678901"

let test_config = {
  Encryption.enabled = true;
  key_source = `Direct test_key_string;
  version = 1;
}

(* Helper to get key *)
let get_test_key () =
  match Encryption.load_key test_config with
  | Ok key -> key
  | Error e -> failwith (Encryption.show_encryption_error e)

(* Test: encrypt and decrypt JSON *)
let test_encrypt_decrypt_json () =
  let key = get_test_key () in
  let adata = "test-context" in
  let original_json = `Assoc [
    ("token", `String "secret123");
    ("role", `String "admin");
  ] in

  (* Encrypt *)
  let envelope = match Encryption.encrypt_json ~key ~adata original_json with
    | Ok env -> env
    | Error e -> failwith (Encryption.show_encryption_error e)
  in

  check bool "encrypted flag" true envelope.Encryption.encrypted;
  check int "version" 1 envelope.version;
  check string "adata" adata envelope.adata;

  (* Decrypt *)
  let decrypted = match Encryption.decrypt_envelope ~key envelope with
    | Ok json -> json
    | Error e -> failwith (Encryption.show_encryption_error e)
  in

  check string "roundtrip"
    (Yojson.Safe.to_string original_json)
    (Yojson.Safe.to_string decrypted)

(* Test: different nonces produce different ciphertext *)
let test_unique_nonces () =
  let key = get_test_key () in
  let adata = "test" in
  let json = `String "hello" in

  let env1 = match Encryption.encrypt_json ~key ~adata json with
    | Ok e -> e | Error e -> failwith (Encryption.show_encryption_error e)
  in
  let env2 = match Encryption.encrypt_json ~key ~adata json with
    | Ok e -> e | Error e -> failwith (Encryption.show_encryption_error e)
  in

  (* Nonces should be different *)
  check bool "different nonces" true (env1.nonce <> env2.nonce);
  (* Ciphertexts should be different *)
  check bool "different ciphertexts" true (env1.ciphertext <> env2.ciphertext)

(* Test: tampered ciphertext fails authentication *)
let test_tamper_detection () =
  let key = get_test_key () in
  let adata = "test" in
  let json = `String "secret" in

  let envelope = match Encryption.encrypt_json ~key ~adata json with
    | Ok e -> e | Error e -> failwith (Encryption.show_encryption_error e)
  in

  (* Tamper with ciphertext (flip a character in base64) *)
  let tampered_ct =
    let s = Bytes.of_string envelope.ciphertext in
    if Bytes.length s > 5 then begin
      let c = Bytes.get s 5 in
      Bytes.set s 5 (if c = 'A' then 'B' else 'A')
    end;
    Bytes.to_string s
  in

  let tampered_envelope = { envelope with ciphertext = tampered_ct } in

  match Encryption.decrypt_envelope ~key tampered_envelope with
  | Error Encryption.DecryptionFailed -> ()  (* Expected *)
  | Error (Encryption.InvalidEnvelope _) -> ()  (* Also acceptable for base64 decode failure *)
  | Ok _ -> fail "should have detected tampering"
  | Error e -> fail (Encryption.show_encryption_error e)

(* Test: wrong adata fails authentication *)
let test_wrong_adata () =
  let key = get_test_key () in
  let json = `String "data" in

  let envelope = match Encryption.encrypt_json ~key ~adata:"context-1" json with
    | Ok e -> e | Error e -> failwith (Encryption.show_encryption_error e)
  in

  (* Try to decrypt with wrong adata *)
  let wrong_adata_envelope = { envelope with adata = "context-2" } in

  match Encryption.decrypt_envelope ~key wrong_adata_envelope with
  | Error Encryption.DecryptionFailed -> ()  (* Expected - GCM tag verification fails *)
  | Ok _ -> fail "should have failed with wrong adata"
  | Error e -> fail (Encryption.show_encryption_error e)

(* Test: envelope JSON serialization *)
let test_envelope_serialization () =
  let key = get_test_key () in
  let adata = "serialize-test" in
  let original = `Assoc [("key", `String "value")] in

  let envelope = match Encryption.encrypt_json ~key ~adata original with
    | Ok e -> e | Error e -> failwith (Encryption.show_encryption_error e)
  in

  (* Serialize to JSON *)
  let json = Encryption.envelope_to_json envelope in

  (* Check structure *)
  let open Yojson.Safe.Util in
  check bool "_encrypted field" true (json |> member "_encrypted" |> to_bool);
  check int "v field" 1 (json |> member "v" |> to_int);

  (* Deserialize back *)
  match Encryption.envelope_of_json json with
  | None -> fail "failed to deserialize envelope"
  | Some parsed ->
      check string "nonce roundtrip" envelope.nonce parsed.nonce;
      check string "ct roundtrip" envelope.ciphertext parsed.ciphertext

(* Test: is_encrypted_json detection *)
let test_is_encrypted_detection () =
  let plain_json = `Assoc [("data", `String "plain")] in
  let encrypted_json = `Assoc [
    ("_encrypted", `Bool true);
    ("v", `Int 1);
    ("nonce", `String "xxx");
    ("ct", `String "yyy");
    ("adata", `String "zzz");
  ] in

  check bool "plain not detected" false (Encryption.is_encrypted_json plain_json);
  check bool "encrypted detected" true (Encryption.is_encrypted_json encrypted_json)

(* Test: invalid key length *)
let test_invalid_key_length () =
  let bad_config = {
    Encryption.enabled = true;
    key_source = `Direct "too-short";
    version = 1;
  } in

  match Encryption.load_key bad_config with
  | Error (Encryption.InvalidKeyLength _) -> ()  (* Expected *)
  | Ok _ -> fail "should reject short key"
  | Error e -> fail (Encryption.show_encryption_error e)

(* Test: generate_key_hex produces valid hex *)
let test_generate_key_hex () =
  match Encryption.generate_key_hex () with
  | Error e -> fail (Encryption.show_encryption_error e)
  | Ok hex ->
      check int "hex length" 64 (String.length hex);
      (* Check all chars are hex *)
      let is_hex c =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
      in
      let all_hex = String.for_all is_hex hex in
      check bool "all hex chars" true all_hex

(* Test: get_status returns valid JSON *)
let test_get_status () =
  let status = Encryption.get_status test_config in
  let open Yojson.Safe.Util in
  check bool "enabled field" true (status |> member "enabled" |> to_bool);
  check bool "rng_initialized" true (status |> member "rng_initialized" |> to_bool);
  check string "key_status" "loaded" (status |> member "key_status" |> to_string)

(* All tests *)
let () =
  run "Encryption" [
    "roundtrip", [
      test_case "encrypt and decrypt JSON" `Quick test_encrypt_decrypt_json;
      test_case "unique nonces" `Quick test_unique_nonces;
    ];
    "security", [
      test_case "tamper detection" `Quick test_tamper_detection;
      test_case "wrong adata detection" `Quick test_wrong_adata;
    ];
    "serialization", [
      test_case "envelope JSON roundtrip" `Quick test_envelope_serialization;
      test_case "is_encrypted detection" `Quick test_is_encrypted_detection;
    ];
    "key_management", [
      test_case "invalid key length" `Quick test_invalid_key_length;
      test_case "generate_key_hex" `Quick test_generate_key_hex;
    ];
    "diagnostics", [
      test_case "get_status" `Quick test_get_status;
    ];
  ]
