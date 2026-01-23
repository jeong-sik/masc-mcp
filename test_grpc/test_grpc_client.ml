(* Test for gRPC client module *)

open Lwt.Syntax
module Grpc = Masc_mcp.Transport_grpc

let test_client_creation () =
  let config : Grpc.config = {
    host = "localhost";
    port = 50051;
    tls = false;
    cert_file = None;
    key_file = None;
  } in
  let client = Grpc.Client.create config in
  Printf.printf "Client created with host=%s port=%d\n%!" config.host config.port;
  client

let test_connect_failure () =
  let* () = Lwt_io.printf "Testing connection to non-existent server...\n%!" in
  let client = test_client_creation () in
  let* result = Grpc.Client.connect client in
  match result with
  | Ok _ ->
    Lwt_io.printf "Unexpected: connected to non-existent server\n%!"
  | Error msg ->
    Lwt_io.printf "Expected connection failure: %s\n%!" msg

let test_call_without_connect () =
  let* () = Lwt_io.printf "\nTesting RPC call without connection...\n%!" in
  let client = test_client_creation () in
  let* result = Grpc.Client.get_status client in
  match result with
  | Ok _ ->
    Lwt_io.printf "Unexpected: RPC succeeded without connection\n%!"
  | Error msg ->
    Lwt_io.printf "Expected error: %s\n%!" msg

let () =
  Lwt_main.run begin
    let* () = Lwt_io.printf "=== gRPC Client Tests ===\n%!" in
    let* () = test_connect_failure () in
    let* () = test_call_without_connect () in
    Lwt_io.printf "\n=== Tests Complete ===\n%!"
  end
