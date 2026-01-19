(** MASC MCP Server - Eio Native Entry Point
    MCP Streamable HTTP Transport with Eio concurrency (OCaml 5.x)

    Uses httpun-eio for conflict-free operation with websocket-lwt-unix.
*)

[@@@warning "-32-69"]  (* Suppress unused values/fields during migration *)

open Cmdliner

(** Module aliases *)
module Http = Masc_mcp.Http_server_eio
module Mcp_session = Masc_mcp.Mcp_session
module Mcp_server = Masc_mcp.Mcp_server
module Mcp_eio = Masc_mcp.Mcp_server_eio
module Room_utils = Masc_mcp.Room_utils

(** MCP Protocol Versions *)
let mcp_protocol_versions = [
  "2024-11-05";
  "2025-03-26";
  "2025-11-25";
]

let mcp_protocol_version_default = "2025-11-25"

let protocol_version_by_session : (string, string) Hashtbl.t = Hashtbl.create 128

(** Get default base path from ME_ROOT or current directory *)
let default_base_path () =
  match Sys.getenv_opt "ME_ROOT" with
  | Some path -> path
  | None -> Sys.getcwd ()

(** Validate MCP-Protocol-Version *)
let is_valid_protocol_version version =
  List.mem version mcp_protocol_versions

let remember_protocol_version session_id version =
  if is_valid_protocol_version version then
    Hashtbl.replace protocol_version_by_session session_id version

(** Extract protocol version from initialize request body *)
let protocol_version_from_body body_str =
  try
    let json = Yojson.Safe.from_string body_str in
    match Mcp_server.jsonrpc_request_of_yojson json with
    | Ok req when String.equal req.method_ "initialize" ->
        let version =
          Mcp_server.protocol_version_from_params req.params
          |> Mcp_server.normalize_protocol_version
        in
        Some version
    | _ -> None
  with _ -> None

(** Get session_id from query string *)
let get_session_id_query target =
  match String.split_on_char '?' target with
  | [_; query] ->
      query
      |> String.split_on_char '&'
      |> List.find_map (fun param ->
          match String.split_on_char '=' param with
          | ["session_id"; v] | ["sessionId"; v] -> Some v
          | _ -> None)
  | _ -> None

(** Get session_id from either query param or header *)
let get_session_id_any (request : Httpun.Request.t) =
  match get_session_id_query request.target with
  | Some _ as id -> id
  | None -> Httpun.Headers.get request.headers "mcp-session-id"

(** Get protocol version from headers *)
let get_protocol_version (request : Httpun.Request.t) =
  match Httpun.Headers.get request.headers "mcp-protocol-version" with
  | Some v -> v
  | None -> mcp_protocol_version_default

let get_protocol_version_for_session ?session_id request =
  match session_id with
  | Some id ->
      (match Hashtbl.find_opt protocol_version_by_session id with
      | Some v -> v
      | None -> get_protocol_version request)
  | None -> get_protocol_version request

(** CORS origin *)
let get_origin (request : Httpun.Request.t) =
  match Httpun.Headers.get request.headers "origin" with
  | Some o -> o
  | None -> "*"

(** CORS headers *)
let cors_headers origin = [
  ("access-control-allow-origin", origin);
  ("access-control-allow-methods", "GET, POST, DELETE, OPTIONS");
  ("access-control-allow-headers", "Content-Type, Accept, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-Id");
  ("access-control-expose-headers", "Mcp-Session-Id, Mcp-Protocol-Version");
  ("access-control-allow-credentials", "true");
]

(** Server state - initialized at startup *)
let server_state : Mcp_server.server_state option ref = ref None

(** Health check handler *)
let health_handler _request reqd =
  let json = {|{"status":"ok","server":"masc-mcp-eio","version":"2.2.1"}|} in
  Http.Response.json json reqd

(** CORS preflight handler *)
let options_handler request reqd =
  let origin = get_origin request in
  let headers = Httpun.Headers.of_list (
    ("content-length", "0") :: cors_headers origin
  ) in
  let response = Httpun.Response.create ~headers `No_content in
  Httpun.Reqd.respond_with_string reqd response ""

(** Eio switch and clock references for MCP handlers *)
let current_sw : Eio.Switch.t option ref = ref None
let current_clock : float Eio.Time.clock_ty Eio.Resource.t option ref = ref None

(** MCP POST handler - async body reading with callback-based response *)
let mcp_post_handler request reqd =
  let origin = get_origin request in
  let session_id =
    match get_session_id_any request with
    | Some id -> id
    | None -> Mcp_session.generate ()
  in

  (* Read body asynchronously and respond from callback *)
  Http.Request.read_body_async reqd (fun body_str ->
    (* Get server state *)
    let state = match !server_state with
      | Some s -> s
      | None -> failwith "Server state not initialized"
    in

    (* Handle MCP request via Mcp_eio (pure Eio native) *)
    let sw = match !current_sw with
      | Some s -> s
      | None -> failwith "Eio switch not initialized"
    in
    let clock = match !current_clock with
      | Some c -> c
      | None -> failwith "Eio clock not initialized"
    in
    let response_json = Mcp_eio.handle_request ~clock ~sw state body_str in

    (* Convert to string *)
    let response_str = Yojson.Safe.to_string response_json in

    (* Remember protocol version from initialize request *)
    (match protocol_version_from_body body_str with
     | Some v -> remember_protocol_version session_id v
     | None -> ());

    let headers = Httpun.Headers.of_list (
      [("content-type", "application/json; charset=utf-8");
       ("content-length", string_of_int (String.length response_str));
       ("mcp-session-id", session_id)] @ cors_headers origin
    ) in
    let response = Httpun.Response.create ~headers `OK in
    Httpun.Reqd.respond_with_string reqd response response_str
  )

(** SSE client registry for broadcasting notifications *)
type sse_client = {
  session_id: string;
  writer: Httpun.Body.Writer.t;
  mutable connected: bool;
}

let sse_clients : (string, sse_client) Hashtbl.t = Hashtbl.create 64

(** Send SSE event to a client *)
let send_sse_event writer event_type data =
  let event = Printf.sprintf "event: %s\ndata: %s\n\n" event_type data in
  Httpun.Body.Writer.write_string writer event

(** Broadcast SSE event to all connected clients *)
let broadcast_sse_event event_type data =
  Hashtbl.iter (fun _ client ->
    if client.connected then
      send_sse_event client.writer event_type data
  ) sse_clients

(** SSE streaming handler with chunked transfer *)
let sse_streaming_handler request reqd =
  let origin = get_origin request in
  let session_id =
    match get_session_id_any request with
    | Some id -> id
    | None -> Mcp_session.generate ()
  in
  let protocol_version = get_protocol_version_for_session ~session_id request in

  (* SSE headers - no Content-Length for streaming *)
  let headers = Httpun.Headers.of_list (
    [("content-type", "text/event-stream; charset=utf-8");
     ("cache-control", "no-cache, no-store, must-revalidate");
     ("connection", "keep-alive");
     ("x-accel-buffering", "no");
     ("mcp-session-id", session_id);
     ("mcp-protocol-version", protocol_version)] @ cors_headers origin
  ) in

  let response = Httpun.Response.create ~headers `OK in
  let body = Httpun.Reqd.respond_with_streaming reqd response in

  (* Register client *)
  let client = { session_id; writer = body; connected = true } in
  Hashtbl.replace sse_clients session_id client;

  (* Send connected event *)
  let connected_data = Printf.sprintf {|{"session_id":"%s","protocol_version":"%s"}|}
    session_id protocol_version in
  send_sse_event body "connected" connected_data;
  Httpun.Body.Writer.flush body (fun () ->
    ()  (* Connection established, client is now listening *)
  )

(** SSE simple handler - for compatibility, returns single event *)
let sse_simple_handler request reqd =
  let origin = get_origin request in
  let session_id =
    match get_session_id_any request with
    | Some id -> id
    | None -> Mcp_session.generate ()
  in
  let protocol_version = get_protocol_version_for_session ~session_id request in

  let event = Printf.sprintf "event: connected\ndata: {\"session_id\":\"%s\"}\n\n" session_id in

  let headers = Httpun.Headers.of_list (
    [("content-type", "text/event-stream");
     ("cache-control", "no-cache");
     ("connection", "keep-alive");
     ("x-accel-buffering", "no");
     ("mcp-session-id", session_id);
     ("mcp-protocol-version", protocol_version);
     ("content-length", string_of_int (String.length event))] @ cors_headers origin
  ) in
  let response = Httpun.Response.create ~headers `OK in
  Httpun.Reqd.respond_with_string reqd response event

(** Build routes for MCP server *)
let make_routes () =
  Http.Router.empty
  |> Http.Router.get "/health" health_handler
  |> Http.Router.get "/" (fun _req reqd -> Http.Response.text "MASC MCP Server (Eio)" reqd)
  |> Http.Router.post "/" mcp_post_handler
  |> Http.Router.post "/mcp" mcp_post_handler
  |> Http.Router.get "/sse" sse_streaming_handler      (* Streaming SSE *)
  |> Http.Router.get "/sse/simple" sse_simple_handler  (* Simple single-event *)
  |> Http.Router.get "/messages" sse_streaming_handler

(** Extended router to handle OPTIONS *)
let make_extended_handler routes =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    match request.meth with
    | `OPTIONS -> options_handler request reqd
    | _ -> Http.Router.dispatch routes request reqd

(** Main server loop *)
let run_server ~sw ~env ~port ~base_path =
  (* Extract components from Eio environment *)
  let clock = Eio.Stdenv.clock env in
  let mono_clock = Eio.Stdenv.mono_clock env in
  let net = Eio.Stdenv.net env in

  (* Store switch and clock references for handlers *)
  current_sw := Some sw;
  current_clock := Some clock;

  (* Create Caqti-compatible stdenv adapter
     Note: net type coercion from [Generic|Unix] to [Generic] is safe
     because Caqti only uses the generic network capabilities *)
  let caqti_env : Caqti_eio.stdenv = object
    method net = (net :> [`Generic] Eio.Net.ty Eio.Resource.t)
    method clock = clock
    method mono_clock = mono_clock
  end in

  (* Initialize server state with Eio context *)
  let state = Mcp_eio.create_state_eio ~sw ~env:caqti_env ~base_path in
  server_state := Some state;

  let config = { Http.default_config with port; host = "127.0.0.1" } in
  let routes = make_routes () in
  let request_handler = make_extended_handler routes in

  let ip = Eio.Net.Ipaddr.V4.loopback in
  let addr = `Tcp (ip, config.port) in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:config.max_connections addr in

  let resolved_base = state.room_config.base_path in
  let masc_dir = Filename.concat resolved_base ".masc" in
  Printf.printf "🚀 MASC MCP Server (Eio) listening on http://%s:%d\n%!" config.host config.port;
  Printf.printf "   Base path: %s\n%!" resolved_base;
  if resolved_base <> base_path then
    Printf.printf "   Base path (input): %s\n%!" base_path;
  Printf.printf "   MASC dir: %s\n%!" masc_dir;

  let rec accept_loop () =
    let flow, client_addr = Eio.Net.accept ~sw socket in
    Eio.Fiber.fork ~sw (fun () ->
      try
        Httpun_eio.Server.create_connection_handler
          ~sw
          ~request_handler
          ~error_handler:Http.error_handler
          client_addr
          flow
      with exn ->
        Printf.eprintf "Connection error: %s\n%!" (Printexc.to_string exn)
    );
    accept_loop ()
  in
  accept_loop ()

(** CLI options *)
let port =
  let doc = "Port to listen on" in
  Arg.(value & opt int 8935 & info ["p"; "port"] ~docv:"PORT" ~doc)

let base_path =
  let doc = "Base path for MASC data (.masc folder location; worktrees resolve to git root)" in
  Arg.(value & opt string (default_base_path ()) & info ["base-path"] ~docv:"PATH" ~doc)

(** Graceful shutdown exception *)
exception Shutdown

let run_cmd port base_path =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in

  (* Graceful shutdown setup *)
  let switch_ref = ref None in
  let shutdown_initiated = ref false in
  let initiate_shutdown signal_name =
    if not !shutdown_initiated then begin
      shutdown_initiated := true;
      Printf.eprintf "\n🚀 MASC MCP: Received %s, shutting down gracefully...\n%!" signal_name;

      (* Broadcast shutdown notification to all SSE clients *)
      let shutdown_data = Printf.sprintf
        {|{"jsonrpc":"2.0","method":"notifications/shutdown","params":{"reason":"%s","message":"Server is shutting down, please reconnect"}}|}
        signal_name
      in
      broadcast_sse_event "notification" shutdown_data;
      Printf.eprintf "🚀 MASC MCP: Sent shutdown notification to %d SSE clients\n%!" (Hashtbl.length sse_clients);

      (* Give clients 500ms to receive the notification before killing connections *)
      Unix.sleepf 0.5;

      match !switch_ref with
      | Some sw -> Eio.Switch.fail sw Shutdown
      | None -> ()
    end
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGTERM"));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGINT"));

  (* Initialize Lwt event loop for Lwt_eio bridge *)
  Lwt_eio.with_event_loop ~clock @@ fun _ ->
  (try
    Eio.Switch.run @@ fun sw ->
    switch_ref := Some sw;
    run_server ~sw ~env ~port ~base_path
  with
  | Shutdown ->
      Printf.eprintf "🚀 MASC MCP: Shutdown complete.\n%!"
  | Eio.Cancel.Cancelled _ ->
      Printf.eprintf "🚀 MASC MCP: Shutdown complete.\n%!")

let cmd =
  let doc = "MASC MCP Server (Eio native)" in
  let info = Cmd.info "masc-mcp-eio" ~version:"2.2.1-eio" ~doc in
  Cmd.v info Term.(const run_cmd $ port $ base_path)

let () = exit (Cmd.eval cmd)
