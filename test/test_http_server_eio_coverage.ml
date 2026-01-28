(** Http Server Eio Module Coverage Tests

    Tests for http_server_eio types and functions:
    - config type
    - default_config constant
*)

open Alcotest

module Http_server_eio = Masc_mcp.Http_server_eio

(* ============================================================
   config Type Tests
   ============================================================ *)

let test_config_type () =
  let cfg : Http_server_eio.config = {
    port = 8080;
    host = "0.0.0.0";
    max_connections = 256;
  } in
  check int "port" 8080 cfg.port;
  check string "host" "0.0.0.0" cfg.host;
  check int "max_connections" 256 cfg.max_connections

let test_config_localhost () =
  let cfg : Http_server_eio.config = {
    port = 3000;
    host = "localhost";
    max_connections = 64;
  } in
  check string "host" "localhost" cfg.host

(* ============================================================
   default_config Tests
   ============================================================ *)

let test_default_config_port () =
  check int "default port" 8935 Http_server_eio.default_config.port

let test_default_config_host () =
  check string "default host" "127.0.0.1" Http_server_eio.default_config.host

let test_default_config_max_connections () =
  check int "default max_connections" 128 Http_server_eio.default_config.max_connections

let test_default_config_valid () =
  let cfg = Http_server_eio.default_config in
  check bool "port > 0" true (cfg.port > 0);
  check bool "host not empty" true (String.length cfg.host > 0);
  check bool "max_connections > 0" true (cfg.max_connections > 0)

(* ============================================================
   Compression Module Tests
   ============================================================ *)

module Compression = Http_server_eio.Compression

let test_compress_zstd_small_data () =
  (* Small data should not be compressed *)
  let small_data = "hello" in
  let (result, compressed) = Compression.compress_zstd small_data in
  check bool "small data not compressed" false compressed;
  check string "unchanged" small_data result

let test_compress_zstd_threshold () =
  (* Data exactly at 256 bytes threshold *)
  let data = String.make 256 'a' in
  let (_, compressed) = Compression.compress_zstd data in
  (* Repetitive data compresses well *)
  check bool "threshold data compressed" true compressed

let test_compress_zstd_large_repetitive () =
  (* Large repetitive data compresses well *)
  let large_data = String.make 1000 'x' in
  let (result, compressed) = Compression.compress_zstd large_data in
  check bool "compressed" true compressed;
  check bool "smaller" true (String.length result < String.length large_data)

let test_compress_zstd_incompressible () =
  (* Random-like data doesn't compress well *)
  let data = String.init 500 (fun i -> Char.chr ((i * 17 + 31) mod 256)) in
  let (result, compressed) = Compression.compress_zstd data in
  (* Either compressed or not - depends on data *)
  check bool "result not empty" true (String.length result > 0);
  (* At least we shouldn't crash *)
  ignore compressed

let test_compress_zstd_empty () =
  (* Empty string should not be compressed *)
  let (result, compressed) = Compression.compress_zstd "" in
  check bool "empty not compressed" false compressed;
  check string "empty unchanged" "" result

let test_compress_zstd_below_threshold () =
  (* Data just below threshold (255 bytes) *)
  let data = String.make 255 'b' in
  let (result, compressed) = Compression.compress_zstd data in
  check bool "below threshold not compressed" false compressed;
  check string "unchanged" data result

let test_compress_zstd_level () =
  (* Test with different compression level *)
  let data = String.make 500 'c' in
  let (result1, _) = Compression.compress_zstd ~level:1 data in
  let (result9, _) = Compression.compress_zstd ~level:19 data in
  check bool "level 1 result exists" true (String.length result1 > 0);
  check bool "level 19 result exists" true (String.length result9 > 0)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Http Server Eio Coverage" [
    "config", [
      test_case "type" `Quick test_config_type;
      test_case "localhost" `Quick test_config_localhost;
    ];
    "default_config", [
      test_case "port" `Quick test_default_config_port;
      test_case "host" `Quick test_default_config_host;
      test_case "max_connections" `Quick test_default_config_max_connections;
      test_case "valid" `Quick test_default_config_valid;
    ];
    "compress_zstd", [
      test_case "small data" `Quick test_compress_zstd_small_data;
      test_case "threshold" `Quick test_compress_zstd_threshold;
      test_case "large repetitive" `Quick test_compress_zstd_large_repetitive;
      test_case "incompressible" `Quick test_compress_zstd_incompressible;
      test_case "empty" `Quick test_compress_zstd_empty;
      test_case "below threshold" `Quick test_compress_zstd_below_threshold;
      test_case "level" `Quick test_compress_zstd_level;
    ];
  ]
