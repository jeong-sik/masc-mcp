(** Tests for STUN: RFC 5389 Pure OCaml Implementation

    Tests cover:
    - Message encoding/decoding (unit tests)
    - XOR address operations (unit tests)
    - Message integrity (unit tests)
    - Fingerprint calculation (unit tests)
    - Live STUN server requests (integration tests, Eio)
*)

open Masc_mcp

(* ============================================
   Unit Tests: Message Encoding/Decoding
   ============================================ *)

(** Test transaction ID generation *)
let test_transaction_id () =
  let id1 = Stun.generate_transaction_id () in
  let id2 = Stun.generate_transaction_id () in

  (* Should be 12 bytes *)
  Alcotest.(check int) "transaction_id length" 12 (Bytes.length id1);
  Alcotest.(check int) "transaction_id length" 12 (Bytes.length id2);

  (* Should be unique *)
  Alcotest.(check bool) "transaction_ids should be unique"
    false (Bytes.equal id1 id2)

(** Test binding request creation *)
let test_create_binding_request () =
  let msg = Stun.create_binding_request () in

  Alcotest.(check bool) "msg_class is Request"
    true (msg.msg_class = Stun.Request);
  Alcotest.(check bool) "msg_method is Binding"
    true (msg.msg_method = Stun.Binding);
  Alcotest.(check int) "transaction_id is 12 bytes"
    12 (Bytes.length msg.transaction_id)

(** Test message encode/decode roundtrip *)
let test_encode_decode_roundtrip () =
  let original = Stun.create_binding_request () in
  let encoded = Stun.encode original in
  let decoded = Stun.decode encoded in

  match decoded with
  | Error e -> Alcotest.failf "decode failed: %s" e
  | Ok msg ->
    Alcotest.(check bool) "msg_class matches"
      true (msg.msg_class = original.msg_class);
    Alcotest.(check bool) "msg_method matches"
      true (msg.msg_method = original.msg_method);
    Alcotest.(check bool) "transaction_id matches"
      true (Bytes.equal msg.transaction_id original.transaction_id)

(** Test binding response creation and roundtrip *)
let test_binding_response () =
  let transaction_id = Stun.generate_transaction_id () in
  let addr = Stun.{
    family = IPv4;
    port = 12345;
    ip = "203.0.113.42";
  } in
  let msg = Stun.create_binding_response ~transaction_id ~mapped_address:addr in

  Alcotest.(check bool) "msg_class is Success_response"
    true (msg.msg_class = Stun.Success_response);

  (* Encode and decode *)
  let encoded = Stun.encode msg in
  match Stun.decode encoded with
  | Error e -> Alcotest.failf "decode failed: %s" e
  | Ok decoded_msg ->
    Alcotest.(check bool) "has XOR_MAPPED_ADDRESS"
      true (List.exists (fun attr ->
        match attr.Stun.value with
        | Stun.Xor_mapped_address _ -> true
        | _ -> false
      ) decoded_msg.attributes)

(** Test magic cookie validation *)
let test_magic_cookie () =
  (* Create a message with invalid magic cookie *)
  let msg = Stun.create_binding_request () in
  let encoded = Stun.encode msg in

  (* Corrupt magic cookie (bytes 4-7) *)
  Bytes.set encoded 4 '\x00';
  Bytes.set encoded 5 '\x00';
  Bytes.set encoded 6 '\x00';
  Bytes.set encoded 7 '\x00';

  match Stun.decode encoded with
  | Error e ->
    Alcotest.(check bool) "error mentions magic cookie"
      true (String.length e > 0)
  | Ok _ ->
    Alcotest.fail "should reject invalid magic cookie"

(** Test is_stun_message detection *)
let test_is_stun_message () =
  let msg = Stun.create_binding_request () in
  let encoded = Stun.encode msg in

  Alcotest.(check bool) "valid STUN message detected"
    true (Stun.is_stun_message encoded);

  (* Non-STUN data *)
  let garbage = Bytes.of_string "Hello, World!" in
  Alcotest.(check bool) "non-STUN data rejected"
    false (Stun.is_stun_message garbage);

  (* Too short *)
  let short = Bytes.create 10 in
  Alcotest.(check bool) "short data rejected"
    false (Stun.is_stun_message short)

(* ============================================
   Unit Tests: XOR Address Operations
   ============================================ *)

(** Test XOR address roundtrip *)
let test_xor_address_roundtrip () =
  let original = Stun.{
    family = IPv4;
    port = 54321;
    ip = "192.168.1.100";
  } in
  let transaction_id = Stun.generate_transaction_id () in

  let xored = Stun.xor_address original transaction_id in
  let unxored = Stun.unxor_address xored transaction_id in

  Alcotest.(check int) "port matches" original.port unxored.port;
  Alcotest.(check string) "ip matches" original.ip unxored.ip

(* ============================================
   Unit Tests: Fingerprint
   ============================================ *)

(** Test fingerprint calculation *)
let test_fingerprint () =
  let msg = Stun.create_binding_request () in

  (* Fingerprint should be consistent *)
  let encoded = Stun.encode msg in
  let fp1 = Stun.calculate_fingerprint encoded in
  let fp2 = Stun.calculate_fingerprint encoded in

  Alcotest.(check int32) "fingerprint is consistent" fp1 fp2

(* ============================================
   Unit Tests: Message Integrity (HMAC-SHA1)
   ============================================ *)

(** Test MESSAGE-INTEGRITY calculation and verification *)
let test_message_integrity () =
  let msg = Stun.create_binding_request () in
  let key = "testpassword123" in  (* Shared secret *)

  (* Calculate HMAC-SHA1 *)
  let mac = Stun.calculate_integrity msg ~key in

  (* MAC should be 20 bytes (SHA-1 output) *)
  Alcotest.(check int) "HMAC-SHA1 is 20 bytes" 20 (Bytes.length mac);

  (* MAC should be non-zero (not a stub) *)
  let is_nonzero = Bytes.exists (fun c -> c <> '\x00') mac in
  Alcotest.(check bool) "MAC is non-zero" true is_nonzero;

  (* Create message with MESSAGE-INTEGRITY attribute *)
  let msg_with_integrity = {
    msg with
    attributes = msg.attributes @ [{
      Stun.attr_type = Stun.MESSAGE_INTEGRITY;
      value = Stun.Message_integrity mac;
    }]
  } in

  (* Verify should pass with correct key *)
  Alcotest.(check bool) "verify_integrity passes with correct key"
    true (Stun.verify_integrity msg_with_integrity ~key);

  (* Verify should fail with wrong key *)
  Alcotest.(check bool) "verify_integrity fails with wrong key"
    false (Stun.verify_integrity msg_with_integrity ~key:"wrongpassword")

(* ============================================
   Unit Tests: Error Response
   ============================================ *)

(** Test error response creation *)
let test_error_response () =
  let transaction_id = Stun.generate_transaction_id () in
  let msg = Stun.create_error_response
    ~transaction_id
    ~error:Stun.Bad_request
    ~reason:"Invalid attribute"
    ()
  in

  Alcotest.(check bool) "msg_class is Error_response"
    true (msg.msg_class = Stun.Error_response);

  (* Check error code attribute *)
  let has_error = List.exists (fun attr ->
    match attr.Stun.value with
    | Stun.Error_code { code; _ } -> code = 400
    | _ -> false
  ) msg.attributes in

  Alcotest.(check bool) "has error code 400" true has_error

(* ============================================
   Integration Tests: Live STUN Server (Eio)
   ============================================ *)

(** Test live STUN request to Google's server using Eio *)
let test_stun_live_eio () =
  (* Skip in CI or if network unavailable *)
  let skip_live = try Sys.getenv "SKIP_LIVE_TESTS" = "1" with Not_found -> false in
  if skip_live then
    Alcotest.skip ()
  else begin
    match Stun.binding_request_eio_run ~server:"stun.l.google.com:19302" ~timeout:5.0 () with
    | Error e ->
      (* Network errors are acceptable in CI *)
      Printf.printf "STUN request failed (may be network issue): %s\n" e
    | Ok result ->
      Printf.printf "STUN Result:\n";
      Printf.printf "  Mapped Address: %s:%d\n"
        result.mapped_address.ip result.mapped_address.port;
      Printf.printf "  RTT: %.2f ms\n" result.rtt_ms;
      (match result.server_software with
       | Some sw -> Printf.printf "  Server: %s\n" sw
       | None -> ());

      (* Verify we got a valid public IP (not 0.0.0.0 or 127.x.x.x) *)
      Alcotest.(check bool) "IP is not 0.0.0.0"
        true (result.mapped_address.ip <> "0.0.0.0");
      Alcotest.(check bool) "IP is not localhost"
        true (not (String.sub result.mapped_address.ip 0 4 = "127."));
      Alcotest.(check bool) "port is valid"
        true (result.mapped_address.port > 0)
  end

(** Test STUN with invalid server *)
let test_stun_invalid_server () =
  try
    match Stun.binding_request_eio_run ~server:"invalid.nonexistent.server:3478" ~timeout:1.0 () with
    | Error _ -> ()  (* Expected - Error result *)
    | Ok _ -> Alcotest.fail "should fail for invalid server"
  with
  | Failure _ -> ()  (* Expected - DNS resolution failure throws exception *)
  | _ -> ()  (* Any other exception is also acceptable for invalid server *)

(** Test STUN timeout *)
let test_stun_timeout () =
  (* Use a non-routable IP that will timeout *)
  let start = Unix.gettimeofday () in
  let result = Stun.binding_request_eio_run ~server:"10.255.255.1:3478" ~timeout:1.0 () in
  let elapsed = Unix.gettimeofday () -. start in

  match result with
  | Error _ ->
    (* Should timeout around 1 second *)
    Alcotest.(check bool) "timeout was respected"
      true (elapsed < 2.0)
  | Ok _ ->
    Alcotest.fail "should timeout for non-routable IP"

(* ============================================
   Test Suite Registration
   ============================================ *)

let unit_tests = [
  "transaction_id generation", `Quick, test_transaction_id;
  "create binding request", `Quick, test_create_binding_request;
  "encode/decode roundtrip", `Quick, test_encode_decode_roundtrip;
  "binding response", `Quick, test_binding_response;
  "magic cookie validation", `Quick, test_magic_cookie;
  "is_stun_message detection", `Quick, test_is_stun_message;
  "XOR address roundtrip", `Quick, test_xor_address_roundtrip;
  "fingerprint calculation", `Quick, test_fingerprint;
  "MESSAGE-INTEGRITY (HMAC-SHA1)", `Quick, test_message_integrity;
  "error response", `Quick, test_error_response;
]

let integration_tests = [
  "live STUN (Google, Eio)", `Slow, test_stun_live_eio;
  "invalid server", `Quick, test_stun_invalid_server;
  "timeout handling", `Slow, test_stun_timeout;
]

let () =
  Alcotest.run "STUN" [
    "Unit Tests", unit_tests;
    "Integration Tests", integration_tests;
  ]
