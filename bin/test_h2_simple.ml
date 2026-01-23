(** Simple h2-eio test server - minimal implementation to debug :status issue *)

let request_handler _client_addr reqd =
  let open H2 in
  let response = Response.create
    ~headers:(Headers.of_list [("content-type", "text/plain")])
    `OK
  in
  let body = Reqd.respond_with_streaming ~flush_headers_immediately:true reqd response in
  Body.Writer.write_string body "hello";
  Body.Writer.close body

let error_handler _client_addr ?request:_ error respond =
  let open H2 in
  let message = match error with
    | `Exn exn -> Printexc.to_string exn
    | _ -> "Server error"
  in
  Printf.eprintf "Error: %s\n%!" message;
  let headers = Headers.of_list [("content-type", "text/plain")] in
  let body = respond headers in
  Body.Writer.write_string body message;
  Body.Writer.close body

let () =
  let port = 9937 in
  Printf.printf "Starting simple h2-eio test server on port %d...\n%!" port;

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in

  Printf.printf "Listening on 127.0.0.1:%d\n%!" port;

  let handler =
    H2_eio.Server.create_connection_handler
      ~request_handler
      ~error_handler
  in

  while true do
    Eio.Net.accept_fork socket ~sw ~on_error:(fun exn ->
      Printf.eprintf "Connection error: %s\n%!" (Printexc.to_string exn)
    ) (fun client_socket client_addr ->
      handler ~sw client_addr client_socket
    )
  done
