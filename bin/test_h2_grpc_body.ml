(** Test h2-eio with request body reading + trailers (mimics grpc-eio server.ml) *)

let request_handler _client_addr reqd =
  let open H2 in
  let request = Reqd.request reqd in
  let path = request.target in

  Printf.printf "Request: %s %s\n%!" (H2.Method.to_string request.meth) path;

  (* Check if it's a gRPC request *)
  let content_type =
    Headers.get request.headers "content-type"
    |> Option.value ~default:""
  in

  if String.starts_with ~prefix:"application/grpc" content_type then begin
    (* Read request body first (like grpc-eio server.ml does) *)
    let body_parts = ref [] in
    let body = Reqd.request_body reqd in

    let rec read_body () =
      Body.Reader.schedule_read body
        ~on_eof:(fun () ->
          let full_body = String.concat "" (List.rev !body_parts) in
          Printf.printf "Body received: %d bytes\n%!" (String.length full_body);

          (* Send response WITH trailers *)
          let headers = Headers.of_list [
            "content-type", "application/grpc+proto";
          ] in
          let response = Response.create ~headers `OK in
          let response_body = Reqd.respond_with_streaming ~flush_headers_immediately:true reqd response in

          (* Schedule trailers BEFORE closing body *)
          let trailers = Headers.of_list [
            "grpc-status", "0";
            "grpc-message", "OK";
          ] in
          Reqd.schedule_trailers reqd trailers;
          Body.Writer.close response_body
        )
        ~on_read:(fun bs ~off ~len ->
          let chunk = Bigstringaf.substring bs ~off ~len in
          body_parts := chunk :: !body_parts;
          read_body ()
        )
    in
    read_body ()
  end else begin
    (* Regular HTTP response *)
    let response = Response.create
      ~headers:(Headers.of_list [("content-type", "text/plain")])
      `OK
    in
    let body = Reqd.respond_with_streaming ~flush_headers_immediately:true reqd response in
    Body.Writer.write_string body "hello from test server";
    Body.Writer.close body
  end

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
  let port = 9939 in
  Printf.printf "Starting test gRPC h2-eio server (with body reading) on port %d...\n%!" port;

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
