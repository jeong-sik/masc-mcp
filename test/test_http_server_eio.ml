(** Test suite for Http_server_eio module

    Tests the Eio-native HTTP server infrastructure using httpun-eio.
*)

open Masc_mcp.Http_server_eio

(* ===== Unit Tests for Router ===== *)

let test_router_empty () =
  let routes = Router.empty in
  Alcotest.(check int) "empty router" 0 (List.length routes)

let test_router_add_get () =
  let handler _req _reqd = () in
  let routes =
    Router.empty
    |> Router.get "/test" handler
  in
  Alcotest.(check int) "one route" 1 (List.length routes)

let test_router_add_post () =
  let handler _req _reqd = () in
  let routes =
    Router.empty
    |> Router.post "/api" handler
  in
  Alcotest.(check int) "one route" 1 (List.length routes)

let test_router_add_multiple () =
  let handler _req _reqd = () in
  let routes =
    Router.empty
    |> Router.get "/health" handler
    |> Router.post "/api/call" handler
    |> Router.any "/any" handler
  in
  Alcotest.(check int) "three routes" 3 (List.length routes)

(* ===== Unit Tests for Config ===== *)

let test_default_config () =
  Alcotest.(check int) "default port" 8935 default_config.port;
  Alcotest.(check string) "default host" "127.0.0.1" default_config.host;
  Alcotest.(check int) "default max_connections" 128 default_config.max_connections

let test_custom_config () =
  let config = { port = 9000; host = "0.0.0.0"; max_connections = 64 } in
  Alcotest.(check int) "custom port" 9000 config.port;
  Alcotest.(check string) "custom host" "0.0.0.0" config.host

(* ===== Unit Tests for Request helpers ===== *)

let test_request_path_simple () =
  let request = Httpun.Request.create `GET "/health" in
  Alcotest.(check string) "simple path" "/health" (Request.path request)

let test_request_path_with_query () =
  let request = Httpun.Request.create `GET "/api?key=value" in
  Alcotest.(check string) "path without query" "/api" (Request.path request)

let test_request_method () =
  let get_req = Httpun.Request.create `GET "/" in
  let post_req = Httpun.Request.create `POST "/" in
  Alcotest.(check bool) "GET method" true (Request.method_ get_req = `GET);
  Alcotest.(check bool) "POST method" true (Request.method_ post_req = `POST)

let test_request_header () =
  let headers = Httpun.Headers.of_list [("content-type", "application/json")] in
  let request = Httpun.Request.create ~headers `GET "/" in
  Alcotest.(check (option string)) "header found"
    (Some "application/json") (Request.header request "content-type");
  Alcotest.(check (option string)) "header not found"
    None (Request.header request "x-custom")

(* ===== Unit Tests for Compression (Compact Protocol v4) ===== *)

let test_compression_skip_small () =
  let small_data = "Hello, World!" in  (* 13 bytes, below 256 threshold *)
  let (result, compressed) = Compression.compress_zstd small_data in
  Alcotest.(check bool) "small data not compressed" false compressed;
  Alcotest.(check string) "data unchanged" small_data result

let test_compression_large_data () =
  let large_data = String.make 1000 'x' in  (* 1000 bytes of 'x' - highly compressible *)
  let (result, compressed) = Compression.compress_zstd large_data in
  Alcotest.(check bool) "large data compressed" true compressed;
  Alcotest.(check bool) "result smaller" true (String.length result < String.length large_data)

let test_compression_roundtrip () =
  (* Use highly repetitive data that will definitely compress *)
  let original = String.make 500 'A' ^ String.make 500 'B' ^ String.make 500 'C' in
  let (compressed_data, did_compress) = Compression.compress_zstd original in
  Alcotest.(check bool) "data should compress" true did_compress;
  let decompressed = Zstd.decompress (String.length original) compressed_data in
  Alcotest.(check string) "roundtrip preserves data" original decompressed

let test_accepts_zstd_positive () =
  let headers = Httpun.Headers.of_list [("accept-encoding", "gzip, deflate, zstd")] in
  let request = Httpun.Request.create ~headers `GET "/" in
  Alcotest.(check bool) "accepts zstd" true (Compression.accepts_zstd request)

let test_accepts_zstd_only_zstd () =
  let headers = Httpun.Headers.of_list [("accept-encoding", "zstd")] in
  let request = Httpun.Request.create ~headers `GET "/" in
  Alcotest.(check bool) "accepts zstd only" true (Compression.accepts_zstd request)

let test_accepts_zstd_negative () =
  let headers = Httpun.Headers.of_list [("accept-encoding", "gzip, deflate, br")] in
  let request = Httpun.Request.create ~headers `GET "/" in
  Alcotest.(check bool) "no zstd" false (Compression.accepts_zstd request)

let test_accepts_zstd_no_header () =
  let request = Httpun.Request.create `GET "/" in
  Alcotest.(check bool) "no header" false (Compression.accepts_zstd request)

(* ===== Test Suites ===== *)

let compression_tests = [
  "skip small data", `Quick, test_compression_skip_small;
  "compress large data", `Quick, test_compression_large_data;
  "roundtrip", `Quick, test_compression_roundtrip;
  "accepts zstd (positive)", `Quick, test_accepts_zstd_positive;
  "accepts zstd (only)", `Quick, test_accepts_zstd_only_zstd;
  "accepts zstd (negative)", `Quick, test_accepts_zstd_negative;
  "accepts zstd (no header)", `Quick, test_accepts_zstd_no_header;
]

let router_tests = [
  "empty router", `Quick, test_router_empty;
  "add GET route", `Quick, test_router_add_get;
  "add POST route", `Quick, test_router_add_post;
  "add multiple routes", `Quick, test_router_add_multiple;
]

let config_tests = [
  "default config", `Quick, test_default_config;
  "custom config", `Quick, test_custom_config;
]

let request_tests = [
  "path simple", `Quick, test_request_path_simple;
  "path with query", `Quick, test_request_path_with_query;
  "method", `Quick, test_request_method;
  "header", `Quick, test_request_header;
]

let () =
  Alcotest.run "Http_server_eio" [
    "compression", compression_tests;  (* Compact Protocol v4 *)
    "router", router_tests;
    "config", config_tests;
    "request", request_tests;
  ]
