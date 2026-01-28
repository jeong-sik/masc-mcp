(** Transport gRPC Next Module Coverage Tests

    Tests for gRPC transport configuration:
    - config type
    - default_config constants
    - tls_config function
*)

open Alcotest

module Transport_grpc_next = Masc_mcp.Transport_grpc_next

(* ============================================================
   config Type Tests
   ============================================================ *)

let test_config_type () =
  let cfg : Transport_grpc_next.config = {
    host = "localhost";
    port = 9999;
    tls = false;
    cert_file = None;
    key_file = None;
  } in
  check string "host" "localhost" cfg.host;
  check int "port" 9999 cfg.port;
  check bool "tls" false cfg.tls

let test_config_with_tls () =
  let cfg : Transport_grpc_next.config = {
    host = "0.0.0.0";
    port = 443;
    tls = true;
    cert_file = Some "/path/to/cert.pem";
    key_file = Some "/path/to/key.pem";
  } in
  check bool "tls enabled" true cfg.tls;
  match cfg.cert_file, cfg.key_file with
  | Some c, Some k ->
    check bool "has cert" true (String.length c > 0);
    check bool "has key" true (String.length k > 0)
  | _ -> fail "expected Some"

(* ============================================================
   default_config Tests
   ============================================================ *)

let test_default_config_host () =
  check string "default host" "127.0.0.1" Transport_grpc_next.default_config.host

let test_default_config_port () =
  check int "default port" 9935 Transport_grpc_next.default_config.port

let test_default_config_tls () =
  check bool "default tls" false Transport_grpc_next.default_config.tls

let test_default_config_cert_file () =
  match Transport_grpc_next.default_config.cert_file with
  | None -> check bool "no cert" true true
  | Some _ -> fail "expected None"

let test_default_config_key_file () =
  match Transport_grpc_next.default_config.key_file with
  | None -> check bool "no key" true true
  | Some _ -> fail "expected None"

(* ============================================================
   tls_config Tests
   ============================================================ *)

let test_tls_config () =
  let cfg = Transport_grpc_next.tls_config
    ~host:"grpc.example.com"
    ~port:8443
    ~cert_file:"/etc/ssl/cert.pem"
    ~key_file:"/etc/ssl/key.pem" in
  check string "host" "grpc.example.com" cfg.host;
  check int "port" 8443 cfg.port;
  check bool "tls" true cfg.tls;
  match cfg.cert_file, cfg.key_file with
  | Some c, Some k ->
    check string "cert" "/etc/ssl/cert.pem" c;
    check string "key" "/etc/ssl/key.pem" k
  | _ -> fail "expected Some"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Transport gRPC Next Coverage" [
    "config", [
      test_case "basic" `Quick test_config_type;
      test_case "with tls" `Quick test_config_with_tls;
    ];
    "default_config", [
      test_case "host" `Quick test_default_config_host;
      test_case "port" `Quick test_default_config_port;
      test_case "tls" `Quick test_default_config_tls;
      test_case "cert_file" `Quick test_default_config_cert_file;
      test_case "key_file" `Quick test_default_config_key_file;
    ];
    "tls_config", [
      test_case "create" `Quick test_tls_config;
    ];
  ]
