(** Http_server_eio - Eio-native HTTP server using httpun-eio

    Conflict-free with httpun-ws-eio (no cohttp 6.x dependency).
    Phase 2 of Eio migration.

    @see <https://github.com/anmonteiro/httpun> httpun documentation
*)

(** Server configuration *)
type config = {
  port: int;
  host: string;
  max_connections: int;
}

let default_config = {
  port = 8935;
  host = "127.0.0.1";
  max_connections = 128;
}

(** HTTP request handler type *)
type request_handler =
  Httpun.Request.t ->
  Httpun.Reqd.t ->
  unit

(** Compact Protocol v4: HTTP Compression with Dictionary Support

    For small messages (32-2048 bytes), trained dictionary compression
    achieves +70%p better compression than standard zstd.

    Multi-format dictionary trained on:
    - MASC broadcasts, task updates, lock events
    - MCP JSON-RPC (tools/call, results, errors)
    - HTTP API responses (status, data, error)
    - Slack-style messages
*)
module Compression = struct
  (** Check if client accepts zstd encoding *)
  let accepts_zstd (request : Httpun.Request.t) : bool =
    match Httpun.Headers.get request.headers "accept-encoding" with
    | Some accept_encoding ->
        String.lowercase_ascii accept_encoding
        |> fun s -> String.split_on_char ',' s
        |> List.exists (fun enc ->
             String.trim enc |> String.lowercase_ascii |> fun e ->
             e = "zstd" || String.sub e 0 (min 4 (String.length e)) = "zstd")
    | None -> false

  (** Check if client accepts dictionary-enhanced zstd *)
  let accepts_zstd_dict (request : Httpun.Request.t) : bool =
    match Httpun.Headers.get request.headers "accept-encoding" with
    | Some accept_encoding ->
        String.lowercase_ascii accept_encoding
        |> String.split_on_char ','
        |> List.exists (fun enc ->
             let e = String.trim enc in
             e = "zstd-dict" || e = "zstd;dict=masc")
    | None -> false

  (** Compress with dictionary if beneficial
      @return (compressed_data, encoding_name option) *)
  let compress ?(level = 3) (data : string) : string * string option =
    let (compressed, used_dict, did_compress) =
      Compression_dict.compress ~level data
    in
    if did_compress then
      let encoding = if used_dict then "zstd-dict" else "zstd" in
      (compressed, Some encoding)
    else
      (data, None)

  (** Legacy: Standard zstd without dictionary *)
  let compress_zstd ?(level = 3) (data : string) : (string * bool) =
    if String.length data < 256 then
      (data, false)
    else
      try
        let compressed = Zstd.compress ~level data in
        if String.length compressed < String.length data then
          (compressed, true)
        else
          (data, false)
      with Zstd.Error _ -> (data, false)
end

(** Simple response helpers *)
module Response = struct
  let text ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  (** JSON response with optional zstd compression (dictionary-enhanced)

      Uses trained multi-format dictionary for small messages (32-2048 bytes)
      achieving ~70% compression vs ~6% with standard zstd.

      @param compress Enable compression if client accepts (default: true)
      @param request Optional request to check Accept-Encoding header *)
  let json ?(status = `OK) ?(compress = true) ?request body reqd =
    let should_compress =
      compress &&
      match request with
      | Some req -> Compression.accepts_zstd req
      | None -> false
    in
    let final_body, encoding =
      if should_compress then
        (* Use dictionary-based compression for better small message handling *)
        Compression.compress body
      else
        (body, None)
    in
    let base_headers = [
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length final_body));
      ("vary", "Accept-Encoding");
    ] in
    let headers = match encoding with
      | Some enc -> ("content-encoding", enc) :: base_headers
      | None -> base_headers
    in
    let response = Httpun.Response.create ~headers:(Httpun.Headers.of_list headers) status in
    Httpun.Reqd.respond_with_string reqd response final_body

  (** Legacy JSON response without compression check (backwards compatible) *)
  let json_raw ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let not_found reqd =
    text ~status:`Not_found "404 Not Found" reqd

  let method_not_allowed reqd =
    text ~status:`Method_not_allowed "405 Method Not Allowed" reqd

  let internal_error msg reqd =
    text ~status:`Internal_server_error ("500 Internal Server Error: " ^ msg) reqd
end

(** Request helpers *)
module Request = struct
  (** Read request body - loops until EOF (httpun requires repeated schedule_read) *)
  let read_body_async reqd callback =
    let body = Httpun.Reqd.request_body reqd in
    let chunks = ref [] in
    let rec read_loop () =
      Httpun.Body.Reader.schedule_read body
        ~on_eof:(fun () ->
          callback (String.concat "" (List.rev !chunks)))
        ~on_read:(fun buffer ~off ~len ->
          let chunk = Bigstringaf.substring buffer ~off ~len in
          chunks := chunk :: !chunks;
          (* Continue reading until EOF *)
          read_loop ())
    in
    read_loop ()

  (** Read request body synchronously - uses Condition for proper synchronization *)
  let read_body_sync reqd =
    let result = ref None in
    let mutex = Eio.Mutex.create () in
    let cond = Eio.Condition.create () in

    read_body_async reqd (fun body_str ->
      Eio.Mutex.use_rw ~protect:false mutex (fun () ->
        result := Some body_str;
        Eio.Condition.broadcast cond));

    Eio.Mutex.use_rw ~protect:false mutex (fun () ->
      while !result = None do
        Eio.Condition.await_no_mutex cond
      done);

    match !result with
    | Some s -> s
    | None -> failwith "read_body_sync: impossible state"

  (** Get path from request target *)
  let path (request : Httpun.Request.t) =
    request.target |> String.split_on_char '?' |> List.hd

  (** Get HTTP method *)
  let method_ (request : Httpun.Request.t) =
    request.meth

  (** Get header value *)
  let header (request : Httpun.Request.t) name =
    Httpun.Headers.get request.headers name
end

(** Router for simple path-based routing *)
module Router = struct
  type route = {
    path: string;
    methods: Httpun.Method.t list;
    handler: request_handler;
  }

  type t = route list

  let empty : t = []

  let add ~path ~methods ~handler routes =
    { path; methods; handler } :: routes

  let get path handler routes =
    add ~path ~methods:[`GET] ~handler routes

  let post path handler routes =
    add ~path ~methods:[`POST] ~handler routes

  let any path handler routes =
    add ~path ~methods:[`GET; `POST; `PUT; `DELETE; `OPTIONS] ~handler routes

  let dispatch routes request reqd =
    let req_path = Request.path request in
    let req_method = Request.method_ request in
    match List.find_opt (fun r -> String.equal r.path req_path) routes with
    | Some route ->
        if List.mem req_method route.methods then
          route.handler request reqd
        else
          Response.method_not_allowed reqd
    | None ->
        Response.not_found reqd
end

(** Health check endpoint - JSON response *)
let health_handler _request reqd =
  let json = {|{"status":"ok","server":"masc-mcp","version":"2.2.1"}|} in
  Response.json json reqd

(** Default routes for MCP server *)
let default_routes =
  Router.empty
  |> Router.get "/health" health_handler
  |> Router.get "/" (fun _req reqd ->
      Response.text "MASC MCP Server (Eio)" reqd)

(** Create httpun request handler from router
    Note: httpun-eio wraps reqd in Gluten.Reqd.t, extract with .reqd field *)
let make_request_handler routes =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    Router.dispatch routes request reqd

(** Create error handler *)
let error_handler _client_addr ?request:_ error start_response =
  let response_body = start_response Httpun.Headers.empty in
  let msg = match error with
    | `Exn exn -> Printexc.to_string exn
    | `Bad_request -> "Bad Request"
    | `Bad_gateway -> "Bad Gateway"
    | `Internal_server_error -> "Internal Server Error"
  in
  Httpun.Body.Writer.write_string response_body msg;
  Httpun.Body.Writer.close response_body

(** Run the HTTP server with Eio *)
let run ~sw ~net config routes =
  let request_handler = make_request_handler routes in
  (* Parse IP address using Ipaddr library then convert to Eio format *)
  let ip = match Ipaddr.of_string config.host with
    | Ok addr -> Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets addr)
    | Error _ -> Eio.Net.Ipaddr.V4.loopback (* fallback to 127.0.0.1 *)
  in
  let addr = `Tcp (ip, config.port) in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:config.max_connections addr in
  Printf.printf "🚀 MASC MCP Server (Eio) listening on http://%s:%d\n" config.host config.port;
  Printf.printf "   Graceful shutdown: SIGTERM/SIGINT supported\n%!";

  let rec accept_loop () =
    let flow, client_addr = Eio.Net.accept ~sw socket in
    Eio.Fiber.fork ~sw (fun () ->
      try
        Httpun_eio.Server.create_connection_handler
          ~sw
          ~request_handler
          ~error_handler
          client_addr
          flow
      with exn ->
        Printf.eprintf "Connection error: %s\n%!" (Printexc.to_string exn)
    );
    accept_loop ()
  in
  accept_loop ()

(** Graceful shutdown exception *)
exception Shutdown

(** Convenience function to start server *)
let start ?(config = default_config) ?(routes = default_routes) () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in

  (* Graceful shutdown setup *)
  let switch_ref = ref None in
  let shutdown_initiated = ref false in
  let initiate_shutdown signal_name =
    if not !shutdown_initiated then begin
      shutdown_initiated := true;
      Printf.eprintf "\n🚀 MASC MCP: Received %s, shutting down gracefully...\n%!" signal_name;
      match !switch_ref with
      | Some sw -> Eio.Switch.fail sw Shutdown
      | None -> ()
    end
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGTERM"));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGINT"));

  (try
    Eio.Switch.run @@ fun sw ->
    switch_ref := Some sw;
    run ~sw ~net config routes
  with
  | Shutdown ->
      Printf.eprintf "🚀 MASC MCP: Shutdown complete.\n%!"
  | Eio.Cancel.Cancelled _ ->
      Printf.eprintf "🚀 MASC MCP: Shutdown complete.\n%!")
