(** Tests for OCaml 5.x Eio-based UDP Socket

    Showcases Eio direct-style async programming:
    - No Lwt monads or >>= operators
    - Direct fiber concurrency
    - Simple control flow
*)

open Alcotest

module Udp = Masc_mcp.Udp_socket_eio

(** {1 Type Tests} *)

let test_default_config () =
  let config = Udp.default_config in
  check string "bind_addr" "0.0.0.0" config.bind_addr;
  check int "bind_port" 0 config.bind_port;
  check int "recv_buffer_size" 65536 config.recv_buffer_size;
  check int "send_buffer_size" 65536 config.send_buffer_size

let test_state_strings () =
  check string "Unbound" "Unbound" (Udp.string_of_state Udp.Unbound);
  check string "Bound" "Bound" (Udp.string_of_state Udp.Bound);
  check string "Connected" "Connected" (Udp.string_of_state Udp.Connected);
  check string "Closed" "Closed" (Udp.string_of_state Udp.Closed)

let test_endpoint_string () =
  let ep = Udp.{ addr = "192.168.1.1"; port = 12345 } in
  check string "endpoint" "192.168.1.1:12345" (Udp.string_of_endpoint ep)

(** {1 Statistics Tests} *)

let test_initial_stats () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw () in
  let stats = Udp.get_stats sock in
  check int "packets_sent" 0 stats.packets_sent;
  check int "packets_recv" 0 stats.packets_recv;
  check int "bytes_sent" 0 stats.bytes_sent;
  check int "bytes_recv" 0 stats.bytes_recv;
  check int "errors" 0 stats.errors

let test_stats_to_json () =
  let stats = Udp.{
    packets_sent = 10;
    packets_recv = 20;
    bytes_sent = 1000;
    bytes_recv = 2000;
    errors = 1;
  } in
  let json = Udp.stats_to_json stats in
  let open Yojson.Safe.Util in
  check int "packetsSent" 10 (json |> member "packetsSent" |> to_int);
  check int "packetsRecv" 20 (json |> member "packetsRecv" |> to_int);
  check int "bytesSent" 1000 (json |> member "bytesSent" |> to_int);
  check int "bytesRecv" 2000 (json |> member "bytesRecv" |> to_int);
  check int "errors" 1 (json |> member "errors" |> to_int)

(** {1 Socket Lifecycle Tests} *)

let test_create_socket () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw () in
  check (of_pp Fmt.string) "state" "Unbound" (Udp.string_of_state (Udp.get_state sock));
  check bool "local_endpoint" true (Option.is_none (Udp.local_endpoint sock));
  check bool "remote_endpoint" true (Option.is_none (Udp.remote_endpoint sock))

let test_bind_socket () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw () in
  let ep = Udp.bind sock in
  check (of_pp Fmt.string) "state" "Bound" (Udp.string_of_state (Udp.get_state sock));
  check bool "local_endpoint" true (Option.is_some (Udp.local_endpoint sock));
  check string "addr" "0.0.0.0" ep.addr

let test_connect_socket () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw () in
  let remote = Udp.{ addr = "127.0.0.1"; port = 12345 } in
  Udp.connect sock remote;
  check (of_pp Fmt.string) "state" "Connected" (Udp.string_of_state (Udp.get_state sock));
  check bool "remote_endpoint" true (Option.is_some (Udp.remote_endpoint sock))

let test_close_socket () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw () in
  let _ = Udp.bind sock in
  Udp.close sock;
  check (of_pp Fmt.string) "state" "Closed" (Udp.string_of_state (Udp.get_state sock))

(** {1 Custom Config Tests} *)

let test_custom_config () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config = Udp.{
    bind_addr = "127.0.0.1";
    bind_port = 0;  (* Auto-assign *)
    recv_buffer_size = 4096;
    send_buffer_size = 4096;
  } in
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw ~config () in
  check (of_pp Fmt.string) "state" "Unbound" (Udp.string_of_state (Udp.get_state sock))

(** {1 Error Handling Tests} *)

let test_send_unbound_fails () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw () in
  let dest = Udp.{ addr = "127.0.0.1"; port = 12345 } in
  check_raises "send_to unbound" (Failure "Socket not bound") (fun () ->
    ignore (Udp.send_to sock (Bytes.of_string "test") dest))

let test_send_not_connected_fails () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw () in
  let _ = Udp.bind sock in
  check_raises "send not connected" (Invalid_argument "Socket not connected - use send_to instead") (fun () ->
    ignore (Udp.send sock (Bytes.of_string "test")))

let test_recv_unbound_fails () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw () in
  check_raises "recv unbound" (Failure "Socket not bound") (fun () ->
    ignore (Udp.recv sock))

let test_bind_twice_fails () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let sock = Udp.create ~net:(Eio.Stdenv.net env) ~sw () in
  let _ = Udp.bind sock in
  check_raises "bind twice" (Failure "Socket already bound or closed") (fun () ->
    ignore (Udp.bind sock))

(** {1 Pretty Printer Tests} *)

let test_pp_datagram () =
  let dgram = Udp.{
    data = Bytes.of_string "Hello";
    source = { addr = "192.168.1.100"; port = 5000 };
    timestamp = 1234567890.123;
  } in
  let str = Format.asprintf "%a" Udp.pp_datagram dgram in
  check bool "contains source" true (String.length str > 0)

let test_pp_stats () =
  let stats = Udp.{
    packets_sent = 5;
    packets_recv = 10;
    bytes_sent = 500;
    bytes_recv = 1000;
    errors = 0;
  } in
  let str = Format.asprintf "%a" Udp.pp_stats stats in
  check bool "contains stats" true (String.length str > 0)

(** {1 Loopback Test - Real UDP} *)

let test_loopback_send_recv () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Create two sockets with fixed ports for testing
     Note: Eio's API doesn't easily expose auto-assigned ports,
     so we use fixed ports for the integration test *)
  let net = Eio.Stdenv.net env in
  let server_port = 54399 in  (* Fixed port for test - high ephemeral range *)

  let server_config = Udp.{
    bind_addr = "127.0.0.1";
    bind_port = server_port;
    recv_buffer_size = 65536;
    send_buffer_size = 65536;
  } in
  let client_config = Udp.{
    bind_addr = "127.0.0.1";
    bind_port = 0;  (* Client can use ephemeral *)
    recv_buffer_size = 65536;
    send_buffer_size = 65536;
  } in

  let server = Udp.create ~net ~sw ~config:server_config () in
  let client = Udp.create ~net ~sw ~config:client_config () in

  (* Bind both sockets *)
  let _ = Udp.bind server in
  let _ = Udp.bind client in

  (* Send from client to server using fixed port *)
  let dest = Udp.{ addr = "127.0.0.1"; port = server_port } in
  let msg = Bytes.of_string "Hello, Eio!" in
  let sent = Udp.send_to client msg dest in
  check int "bytes sent" 11 sent;

  (* Receive on server (with timeout to prevent hanging) *)
  let result = Udp.recv_timeout server 2.0 in
  match result with
  | Some dgram ->
    check string "received data" "Hello, Eio!" (Bytes.to_string dgram.data);
    check string "source addr" "127.0.0.1" dgram.source.addr
  | None ->
    fail "Timeout waiting for UDP packet"

(** {1 Test Suite} *)

let type_tests = [
  "default_config", `Quick, test_default_config;
  "state_strings", `Quick, test_state_strings;
  "endpoint_string", `Quick, test_endpoint_string;
]

let stats_tests = [
  "initial_stats", `Quick, test_initial_stats;
  "stats_to_json", `Quick, test_stats_to_json;
]

let lifecycle_tests = [
  "create_socket", `Quick, test_create_socket;
  "bind_socket", `Quick, test_bind_socket;
  "connect_socket", `Quick, test_connect_socket;
  "close_socket", `Quick, test_close_socket;
  "custom_config", `Quick, test_custom_config;
]

let error_tests = [
  "send_unbound_fails", `Quick, test_send_unbound_fails;
  "send_not_connected_fails", `Quick, test_send_not_connected_fails;
  "recv_unbound_fails", `Quick, test_recv_unbound_fails;
  "bind_twice_fails", `Quick, test_bind_twice_fails;
]

let pp_tests = [
  "pp_datagram", `Quick, test_pp_datagram;
  "pp_stats", `Quick, test_pp_stats;
]

let integration_tests = [
  "loopback_send_recv", `Slow, test_loopback_send_recv;
]

let () =
  run "Udp_socket_eio" [
    "Type", type_tests;
    "Stats", stats_tests;
    "Lifecycle", lifecycle_tests;
    "Error", error_tests;
    "PrettyPrint", pp_tests;
    "Integration", integration_tests;
  ]
