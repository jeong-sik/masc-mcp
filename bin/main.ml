(** MASC MCP Server - Entry Point
    MCP Streamable HTTP Transport with legacy compatibility *)

[@@@ocaml.alert "-deprecated"]

open Cmdliner

(** Use Shutdown from library *)
module Shutdown = Masc_mcp.Shutdown

(** MCP Protocol Versions (legacy + current) *)
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

(** Module aliases for cleaner code *)
module Mcp_session = Masc_mcp.Mcp_session
module Sse = Masc_mcp.Sse
module Http_negotiation = Masc_mcp.Mcp_protocol.Http_negotiation

(** Get Mcp-Session-Id from headers *)
let get_session_id_header req =
  Cohttp.Header.get (Cohttp.Request.headers req) "mcp-session-id"

(** Get session_id from legacy SSE query parameter *)
let get_session_id_query req =
  let uri = Cohttp.Request.uri req in
  match Uri.get_query_param uri "session_id" with
  | Some _ as id -> id
  | None -> Uri.get_query_param uri "sessionId"

(** Get session_id from either query param (legacy) or header (streamable) *)
let get_session_id_any req =
  match get_session_id_query req with
  | Some _ as id -> id
  | None -> get_session_id_header req

(** Build legacy SSE messages endpoint URL (event: endpoint) *)
let legacy_messages_endpoint_url req session_id =
  let host =
    match Cohttp.Header.get (Cohttp.Request.headers req) "host" with
    | Some h -> h
    | None -> ""
  in
  if String.equal host "" then
    Printf.sprintf "/messages?session_id=%s" session_id
  else
    Printf.sprintf "http://%s/messages?session_id=%s" host session_id

(** Get Last-Event-ID from headers for resumability *)
let get_last_event_id req =
  match Cohttp.Header.get (Cohttp.Request.headers req) "last-event-id" with
  | Some id -> (try Some (int_of_string id) with _ -> None)
  | None -> None

(** Get MCP-Protocol-Version from headers *)
let get_protocol_version req =
  match Cohttp.Header.get (Cohttp.Request.headers req) "mcp-protocol-version" with
  | Some v -> v
  | None -> mcp_protocol_version_default

let get_protocol_version_for_session ?session_id req =
  match session_id with
  | Some id ->
      (match Hashtbl.find_opt protocol_version_by_session id with
      | Some v -> v
      | None -> get_protocol_version req)
  | None -> get_protocol_version req

(** Validate MCP-Protocol-Version *)
let is_valid_protocol_version version =
  List.mem version mcp_protocol_versions

let remember_protocol_version session_id version =
  if is_valid_protocol_version version then
    Hashtbl.replace protocol_version_by_session session_id version

let protocol_version_from_initialize body_str =
  try
    let json = Yojson.Safe.from_string body_str in
    match Masc_mcp.Mcp_server.jsonrpc_request_of_yojson json with
    | Ok req when String.equal req.method_ "initialize" ->
        let version =
          Masc_mcp.Mcp_server.protocol_version_from_params req.params
          |> Masc_mcp.Mcp_server.normalize_protocol_version
        in
        Some version
    | _ -> None
  with _ -> None

let remember_protocol_version_from_body session_id body_str =
  match protocol_version_from_initialize body_str with
  | Some version -> remember_protocol_version session_id version
  | None -> ()

(** Debug logging toggle (MASC_DEBUG=1) *)
let debug_enabled =
  match Sys.getenv_opt "MASC_DEBUG" with
  | Some "1" -> true
  | _ ->
      (match Sys.getenv_opt "MCP_DEBUG" with
      | Some "1" -> true
      | _ -> false)

let log_debug fmt =
  if debug_enabled then Printf.printf fmt else Printf.ifprintf stdout fmt

(** Keep debug logs single-line even if headers contain CR/LF. *)
let sanitize_header value =
  String.map (fun c -> if c = '\n' || c = '\r' then ' ' else c) value

let header_value req name =
  match Cohttp.Header.get (Cohttp.Request.headers req) name with
  | Some v -> sanitize_header v
  | None -> "-"

let log_request_debug ~label req session_id protocol_version =
  if debug_enabled then
    let path = Uri.path (Cohttp.Request.uri req) in
    let session_header = header_value req "mcp-session-id" in
    let protocol_header = header_value req "mcp-protocol-version" in
    log_debug
      "MASC_DEBUG: %s %s session=%s header-session=%s protocol=%s header-protocol=%s accept=%s content-type=%s last-event-id=%s user-agent=%s origin=%s\n%!"
      label
      path
      session_id
      session_header
      protocol_version
      protocol_header
      (header_value req "accept")
      (header_value req "content-type")
      (header_value req "last-event-id")
      (header_value req "user-agent")
      (header_value req "origin")

(** Safe string prefix check (OCaml 4.13+ has String.starts_with) *)
let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

(** Allowed localhost origins for DNS rebinding protection *)
let allowed_origins = [
  "http://localhost";
  "https://localhost";
  "http://127.0.0.1";
  "https://127.0.0.1";
]

(** Validate Origin header for DNS rebinding protection (2025-11-25 spec MUST) *)
let validate_origin req =
  match Cohttp.Header.get (Cohttp.Request.headers req) "origin" with
  | None -> true  (* No origin header = not a browser request, allow *)
  | Some origin ->
      (* Allow localhost origins for local development *)
      List.exists (fun prefix -> starts_with ~prefix origin) allowed_origins

(** Check if client accepts SSE *)
let accepts_sse req =
  Http_negotiation.accepts_sse_header
    (Cohttp.Header.get (Cohttp.Request.headers req) "accept")

(** Check if client accepts MCP Streamable HTTP (JSON + SSE) *)
let accepts_streamable_mcp req =
  Http_negotiation.accepts_streamable_mcp
    (Cohttp.Header.get (Cohttp.Request.headers req) "accept")

(** Force JSON responses for POST /mcp (compatibility fallback). *)
let force_json_response =
  match Sys.getenv_opt "MASC_FORCE_JSON_RESPONSE" with
  | Some "1" -> true
  | _ ->
      (match Sys.getenv_opt "MCP_FORCE_JSON_RESPONSE" with
      | Some "1" -> true
      | _ -> false)

(** SSE retry interval in milliseconds (for connection closure) *)
let sse_retry_ms = 3000

(** Format SSE priming event (id + retry, no data payload). *)
let sse_prime_event () =
  let id = Sse.next_id () in
  Printf.sprintf "retry: %d\nid: %d\n\n" sse_retry_ms id

(** SSE keep-alive ping interval in seconds *)
let sse_ping_interval_s = 30.0

(** SSE connection tracking (prevents FD leaks / select EINVAL) *)
type sse_conn_info = {
  session_id : string;
  client_id : int;
  stop : unit Lwt.u;
  close : unit -> unit;
}

let sse_conn_by_conn_id : (string, sse_conn_info) Hashtbl.t = Hashtbl.create 128
let sse_conn_id_by_session : (string, string) Hashtbl.t = Hashtbl.create 128

let sse_conn_key (_flow, conn_id) =
  Cohttp.Connection.to_string conn_id

let remove_session_mapping_if_matches session_id conn_key =
  match Hashtbl.find_opt sse_conn_id_by_session session_id with
  | Some key when String.equal key conn_key -> Hashtbl.remove sse_conn_id_by_session session_id
  | _ -> ()

let stop_sse_conn_by_key conn_key =
  match Hashtbl.find_opt sse_conn_by_conn_id conn_key with
  | None -> ()
  | Some info ->
      Hashtbl.remove sse_conn_by_conn_id conn_key;
      remove_session_mapping_if_matches info.session_id conn_key;
      (try Lwt.wakeup_later info.stop () with _ -> ());
      (try info.close () with _ -> ());
      Sse.unregister_if_current info.session_id info.client_id;
      Printf.printf "📴 SSE disconnected: %s\n%!" info.session_id

let stop_sse_session session_id =
  match Hashtbl.find_opt sse_conn_id_by_session session_id with
  | None -> ()
  | Some conn_key -> stop_sse_conn_by_key conn_key

(* ===== Header Builders (DRY) ===== *)

(** Common MCP headers for all responses *)
let mcp_headers session_id protocol_version =
  [
    ("Mcp-Session-Id", session_id);
    ("Mcp-Protocol-Version", protocol_version);
  ]

(** CORS headers for cross-origin requests *)
let cors_headers () =
  [
    ("Access-Control-Allow-Origin", "*");
    ("Access-Control-Expose-Headers", "Mcp-Session-Id, Mcp-Protocol-Version");
  ]

(** SSE response headers *)
let sse_headers session_id protocol_version =
  [("Content-Type", Http_negotiation.sse_content_type)]
  @ mcp_headers session_id protocol_version
  @ cors_headers ()

(** SSE stream headers (with keep-alive) *)
let sse_stream_headers session_id protocol_version =
  [
    ("Content-Type", Http_negotiation.sse_content_type);
    ("Cache-Control", "no-cache");
    ("Connection", "keep-alive");
  ]
  @ mcp_headers session_id protocol_version
  @ cors_headers ()

(** JSON response headers *)
let json_headers session_id protocol_version =
  [("Content-Type", "application/json")]
  @ mcp_headers session_id protocol_version
  @ cors_headers ()

(** CORS preflight response headers *)
let cors_preflight_headers () =
  [
    ("Access-Control-Allow-Origin", "*");
    ("Access-Control-Allow-Methods", "GET, POST, DELETE, OPTIONS");
    ("Access-Control-Allow-Headers", "Content-Type, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-ID, Accept, Origin");
    ("Access-Control-Expose-Headers", "Mcp-Session-Id, Mcp-Protocol-Version");
  ]

(** JSON-RPC error response *)
let json_rpc_error code message =
  Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message);
    ]);
  ])

let jsonrpc_error_code = function
  | `Assoc fields -> (
      match List.assoc_opt "error" fields with
      | Some (`Assoc err_fields) -> (
          match List.assoc_opt "code" err_fields with
          | Some (`Int code) -> Some code
          | _ -> None)
      | _ -> None)
  | _ -> None

let is_http_error_response = function
  | `Assoc fields ->
      let id_is_null =
        match List.assoc_opt "id" fields with
        | Some `Null -> true
        | _ -> false
      in
      let code = jsonrpc_error_code (`Assoc fields) in
      id_is_null && (code = Some (-32700) || code = Some (-32600))
  | _ -> false

(* ===== Route Handlers ===== *)

(** GET /mcp - SSE stream for server->client notifications *)
let handle_get_mcp ?legacy_messages_endpoint _state conn req =
  let open Lwt.Syntax in
  let session_id = Mcp_session.get_or_generate (get_session_id_any req) in
  let protocol_version = get_protocol_version_for_session ~session_id req in
  let last_event_id = get_last_event_id req in
  let conn_key = sse_conn_key conn in
  log_request_debug ~label:"GET /mcp" req session_id protocol_version;

  (* Ensure only one live SSE connection per session_id *)
  (match Hashtbl.find_opt sse_conn_id_by_session session_id with
  | Some old_key when not (String.equal old_key conn_key) ->
      stop_sse_conn_by_key old_key
  | _ -> ());

  let stream, push_to_stream = Lwt_stream.create () in
  let stop, stop_wakener = Lwt.wait () in
  let ic_ref : Cohttp_lwt_unix.Server.IO.ic option ref = ref None in
  let close_connection () =
    (try push_to_stream None with _ -> ());
    match !ic_ref with
    | None -> ()
    | Some ic ->
        Lwt.async (fun () ->
          Cohttp_lwt_unix.Net.close_in ic;
          Lwt.return_unit)
  in

  (* Register SSE client *)
  let client_id =
    Sse.register session_id
    ~push:(fun s -> push_to_stream (Some s))
    ~last_event_id:(Option.value last_event_id ~default:0);
  in
  Hashtbl.replace sse_conn_by_conn_id conn_key {
    session_id;
    client_id;
    stop = stop_wakener;
    close = close_connection;
  };
  Hashtbl.replace sse_conn_id_by_session session_id conn_key;

  (* Replay missed events if Last-Event-ID provided (MCP spec MUST) *)
  let replayed_count = match last_event_id with
    | Some last_id ->
        let missed_events = Sse.get_events_after last_id in
        List.iter (fun ev -> push_to_stream (Some ev)) missed_events;
        List.length missed_events
    | None -> 0
  in

  (* Send SSE priming event first (2025-11-25 spec SHOULD) *)
  push_to_stream (Some (sse_prime_event ()));

  (* Legacy SSE transport: provide messages endpoint (event: endpoint) *)
  (match legacy_messages_endpoint with
  | None -> ()
  | Some f ->
      let endpoint_url = f session_id in
      push_to_stream (Some (Sse.format_event ~event_type:"endpoint" endpoint_url)));

  (* Keep-alive ping loop *)
  Lwt.async (fun () ->
    let rec loop () =
      let* result =
        Lwt.pick [
          (let* () = stop in Lwt.return `Stop);
          (let* () = Lwt_unix.sleep sse_ping_interval_s in Lwt.return `Tick);
        ]
      in
      match result with
      | `Stop -> Lwt.return_unit
      | `Tick ->
          push_to_stream (Some ": ping\n\n");
          loop ()
    in
    loop ()
  );

  Printf.printf "📡 SSE connected: %s (last_event_id: %s, replayed: %d)\n%!"
    session_id
    (match last_event_id with Some id -> string_of_int id | None -> "none")
    replayed_count;

  let headers = Cohttp.Header.of_list (sse_stream_headers session_id protocol_version) in
  let response =
    Cohttp.Response.make ~status:`OK
      ~encoding:Cohttp.Transfer.Chunked ~headers ()
  in

  let io_handler ic oc =
    ic_ref := Some ic;

    (* Detect disconnect via read-side EOF (avoids lingering CLOSE_WAIT fds) *)
    Lwt.async (fun () ->
      let rec wait_for_eof () =
        Lwt.catch
          (fun () ->
            let* chunk = Cohttp_lwt_unix.Server.IO.read ic 1 in
            if String.equal chunk "" then Lwt.return_unit else wait_for_eof ()
          )
          (function
            | Lwt_io.Channel_closed _ | End_of_file ->
                (* Expected: client disconnected (abruptly or cleanly) *)
                Lwt.return_unit
            | exn ->
                (* Unexpected error - log for debugging *)
                Printf.eprintf "[WARN] SSE wait_for_eof unexpected: %s\n%!"
                  (Printexc.to_string exn);
                Lwt.return_unit
          )
      in
      (* Use finalize to guarantee cleanup even if wait_for_eof raises *)
      Lwt.finalize
        (fun () -> wait_for_eof ())
        (fun () ->
          stop_sse_conn_by_key conn_key;
          Lwt.return_unit
        )
    );

    let write_chunk s =
      let header = Printf.sprintf "%x\r\n" (String.length s) in
      let* () = Cohttp_lwt_unix.Server.IO.write oc header in
      let* () = Cohttp_lwt_unix.Server.IO.write oc s in
      let* () = Cohttp_lwt_unix.Server.IO.write oc "\r\n" in
      Cohttp_lwt_unix.Server.IO.flush oc
    in

    let rec write_loop () =
      let* next = Lwt_stream.get stream in
      match next with
      | None -> Lwt.return_unit
      | Some s ->
          Lwt.catch
            (fun () ->
              let* () = write_chunk s in
              write_loop ()
            )
            (fun _ ->
              stop_sse_conn_by_key conn_key;
              Lwt.return_unit)
    in

    Lwt.finalize
      (fun () ->
        let* () = write_loop () in
        Lwt.catch
          (fun () ->
            let* () = Lwt_io.write oc "0\r\n\r\n" in
            Lwt_io.flush oc
          )
          (fun _ -> Lwt.return_unit)
      )
      (fun () ->
        stop_sse_conn_by_key conn_key;
        Lwt.catch (fun () -> Cohttp_lwt_unix.Net.close_in ic; Lwt.return_unit) (fun _ -> Lwt.return_unit)
      )
  in

  Lwt.return (`Expert (response, io_handler))

(** POST /mcp - JSON-RPC request handler *)
let handle_post_mcp state req body =
  let open Lwt.Syntax in
  let open Cohttp_lwt_unix in
  let session_id = Mcp_session.get_or_generate (get_session_id_header req) in
  let* body_str = Cohttp_lwt.Body.to_string body in
  remember_protocol_version_from_body session_id body_str;
  let protocol_version = get_protocol_version_for_session ~session_id req in
  let accept_header = Cohttp.Header.get (Cohttp.Request.headers req) "accept" in
  log_request_debug ~label:"POST /mcp" req session_id protocol_version;
  if not (accepts_streamable_mcp req) then
    let headers = Cohttp.Header.of_list (json_headers session_id protocol_version) in
    let body =
      json_rpc_error (-32600)
        "Invalid Accept header: must include application/json and text/event-stream"
    in
    Server.respond_string ~status:`Bad_request ~headers ~body ()
  else
    let wants_sse = accepts_sse req && not force_json_response in
    if force_json_response then
      log_debug "MASC_DEBUG: POST /mcp force_json_response=true\n%!";
    let* response = Masc_mcp.Mcp_server.handle_request state body_str in

    if wants_sse then begin
      match response with
      | `Null ->
          log_debug "MASC_DEBUG: POST /mcp accept=%s sse=true null-response -> 202\n%!"
            (Option.value ~default:"-" accept_header);
          let headers = Cohttp.Header.of_list (mcp_headers session_id protocol_version) in
          Server.respond_string ~status:`Accepted ~headers ~body:"" ()
      | json when is_http_error_response json ->
          let headers = Cohttp.Header.of_list (json_headers session_id protocol_version) in
          let body = Yojson.Safe.to_string json in
          Server.respond_string ~status:`Bad_request ~headers ~body ()
      | json ->
          log_debug "MASC_DEBUG: POST /mcp accept=%s sse=true json-response\n%!"
            (Option.value ~default:"-" accept_header);
          (* SSE response mode *)
          let stream, push_to_stream = Lwt_stream.create () in
          push_to_stream (Some (sse_prime_event ()));
          let event = Sse.format_event ~event_type:"message" (Yojson.Safe.to_string json) in
          push_to_stream (Some event);
          push_to_stream None;
          let headers = Cohttp.Header.of_list (sse_headers session_id protocol_version) in
          let body = Cohttp_lwt.Body.of_stream stream in
          Server.respond ~status:`OK ~headers ~body ()
    end else begin
      log_debug "MASC_DEBUG: POST /mcp accept=%s sse=false\n%!"
        (Option.value ~default:"-" accept_header);
      (* JSON response mode *)
      match response with
      | `Null ->
          let headers = Cohttp.Header.of_list (mcp_headers session_id protocol_version) in
          Server.respond_string ~status:`Accepted ~headers ~body:"" ()
      | json when is_http_error_response json ->
          let body = Yojson.Safe.to_string json in
          let headers = Cohttp.Header.of_list (json_headers session_id protocol_version) in
          Server.respond_string ~status:`Bad_request ~headers ~body ()
      | json ->
          let body = Yojson.Safe.to_string json in
          let headers = Cohttp.Header.of_list (json_headers session_id protocol_version) in
          Server.respond_string ~status:`OK ~headers ~body ()
    end

(** POST /messages - Legacy SSE transport (client->server messages) *)
let handle_post_messages state req body =
  let open Lwt.Syntax in
  let open Cohttp_lwt_unix in
  match get_session_id_any req with
  | None ->
      Server.respond_string ~status:`Bad_request ~body:"session_id required" ()
  | Some session_id when not (Mcp_session.is_valid session_id) ->
      Server.respond_string ~status:`Bad_request ~body:"invalid session_id" ()
  | Some session_id ->
      let protocol_version = get_protocol_version_for_session ~session_id req in
      log_request_debug ~label:"POST /messages" req session_id protocol_version;
      let* body_str = Cohttp_lwt.Body.to_string body in
      let* response = Masc_mcp.Mcp_server.handle_request state body_str in
      (match response with
      | `Null -> ()
      | json -> Sse.send_to session_id json);
      let headers = Cohttp.Header.of_list (mcp_headers session_id protocol_version) in
      Server.respond_string ~status:`Accepted ~headers ~body:"" ()

(** DELETE /mcp - Session termination *)
let handle_delete_mcp req =
  let open Cohttp_lwt_unix in
  match get_session_id_header req with
  | Some session_id ->
      stop_sse_session session_id;
      Sse.unregister session_id;
      Hashtbl.remove protocol_version_by_session session_id;
      Printf.printf "🔚 Session terminated: %s\n%!" session_id;
      log_request_debug ~label:"DELETE /mcp" req session_id (get_protocol_version req);
      Server.respond_string ~status:`No_content ~body:"" ()
  | None ->
      Server.respond_string ~status:`Bad_request ~body:"Mcp-Session-Id required" ()

(** HTTP server mode with MCP Streamable HTTP Transport (2025-11-25 spec) *)
let run_http ~port ~base_path =
  let state = Masc_mcp.Mcp_server.create_state ~base_path in
  let resolved_base = state.room_config.base_path in
  let masc_dir = Filename.concat resolved_base ".masc" in

  (* Register SSE broadcast callback *)
  Masc_mcp.Mcp_server.set_sse_callback state Sse.broadcast;
  Masc_mcp.Progress.set_sse_callback Sse.broadcast;

  (* Start orchestrator background loop *)
  Masc_mcp.Orchestrator.start state.room_config;

  Printf.printf "🎮 MASC MCP Server (Streamable HTTP 2025-11-25)\n%!";
  Printf.printf "📁 Room: %s\n%!" resolved_base;
  if resolved_base <> base_path then
    Printf.printf "   Base path (input): %s\n%!" base_path;
  Printf.printf "   MASC dir: %s\n%!" masc_dir;
  Printf.printf "🌐 http://127.0.0.1:%d/mcp\n%!" port;
  Printf.printf "   GET  /mcp → SSE stream (notifications)\n%!";
  Printf.printf "   POST /mcp → JSON-RPC (Accept: text/event-stream for SSE)\n%!";
  Printf.printf "   GET  /sse → legacy SSE stream (event: endpoint)\n%!";
  Printf.printf "   POST /messages → legacy client->server messages\n%!";
  Printf.printf "   GET  /health → Health check\n%!";
  Printf.printf "   GET  /.well-known/agent-card.json → A2A Agent Card\n%!";
  Printf.printf "   📢 MCP-Protocol-Version: default %s (supported: %s)\n%!"
    mcp_protocol_version_default (String.concat ", " mcp_protocol_versions);
  Printf.printf "   🔒 Origin validation: enabled (DNS rebinding protection)\n%!";

  let open Cohttp_lwt_unix in

  let callback conn req body =
    let open Lwt.Syntax in
    let path = Uri.path (Cohttp.Request.uri req) in
    let meth = Cohttp.Request.meth req in
    let is_mcp_like_endpoint =
      String.equal path "/mcp" || String.equal path "/sse" || String.equal path "/messages"
    in
    let session_id_for_version = get_session_id_any req in
    let protocol_version =
      get_protocol_version_for_session ?session_id:session_id_for_version req
    in

    (* Security: Origin + Protocol version validation *)
    if is_mcp_like_endpoint && not (validate_origin req) then
      let* rsp =
        Server.respond_string ~status:`Forbidden
          ~body:(json_rpc_error (-32600) "Invalid origin") ()
      in
      Lwt.return (`Response rsp)
    else if is_mcp_like_endpoint && meth <> `OPTIONS &&
            not (is_valid_protocol_version protocol_version) then
      let* rsp =
        Server.respond_string ~status:`Bad_request
          ~body:(json_rpc_error (-32600) "Unsupported protocol version") ()
      in
      Lwt.return (`Response rsp)
    else match meth, path with
    | `GET, "/health" ->
        let* rsp =
          Server.respond_string ~status:`OK
            ~body:(Masc_mcp.Mcp_server.health_response state) ()
        in
        Lwt.return (`Response rsp)
    | `GET, "/dashboard" ->
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "text/html; charset=utf-8");
          ("Cache-Control", "no-cache");
        ] in
        let* rsp = Server.respond_string ~status:`OK ~headers
          ~body:(Masc_mcp.Web_dashboard.html ()) () in
        Lwt.return (`Response rsp)
    | `GET, "/.well-known/agent-card.json" ->
        let card = Masc_mcp.Agent_card.generate_default ~port () in
        let json = Masc_mcp.Agent_card.to_json card in
        let body = Yojson.Safe.pretty_to_string json in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`OK ~headers ~body () in
        Lwt.return (`Response rsp)
    | `GET, "/mcp" -> handle_get_mcp state conn req
    | `POST, "/mcp" ->
        let* rsp = handle_post_mcp state req body in
        Lwt.return (`Response rsp)
    | `DELETE, "/mcp" ->
        let* rsp = handle_delete_mcp req in
        Lwt.return (`Response rsp)
    | `GET, "/sse" ->
        handle_get_mcp
          ~legacy_messages_endpoint:(fun session_id -> legacy_messages_endpoint_url req session_id)
          state conn req
    | `POST, "/messages" ->
        let* rsp = handle_post_messages state req body in
        Lwt.return (`Response rsp)
    | `OPTIONS, _ ->
        let headers = Cohttp.Header.of_list (cors_preflight_headers ()) in
        let* rsp = Server.respond_string ~status:`OK ~headers ~body:"" () in
        Lwt.return (`Response rsp)

    (* ===== REST API v1 Endpoints ===== *)
    | `GET, "/api/v1/status" ->
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_status" ~arguments:(`Assoc []) in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`OK ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `GET, "/api/v1/tasks" ->
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_tasks" ~arguments:(`Assoc []) in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`OK ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `POST, "/api/v1/tasks" ->
        let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_add_task" ~arguments:params in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`Created ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `GET, "/api/v1/agents" ->
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_who" ~arguments:(`Assoc []) in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`OK ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `GET, "/api/v1/agents/detailed" ->
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_agents" ~arguments:(`Assoc []) in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`OK ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `POST, "/api/v1/agents" ->
        let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_join" ~arguments:params in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`Created ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `GET, "/api/v1/messages" ->
        let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_messages" ~arguments:params in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`OK ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `POST, "/api/v1/messages" ->
        let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_broadcast" ~arguments:params in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`Created ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `GET, "/api/v1/votes" ->
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_votes" ~arguments:(`Assoc []) in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`OK ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `POST, "/api/v1/votes" ->
        let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_vote_create" ~arguments:params in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`Created ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `GET, "/api/v1/worktrees" ->
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_worktree_list" ~arguments:(`Assoc []) in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`OK ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `POST, "/api/v1/worktrees" ->
        let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_worktree_create" ~arguments:params in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`Created ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    | `POST, "/api/v1/locks" ->
        let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
        let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_lock" ~arguments:params in
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "application/json");
          ("Access-Control-Allow-Origin", "*");
        ] in
        let* rsp = Server.respond_string ~status:`Created ~headers
          ~body:(Yojson.Safe.to_string result) () in
        Lwt.return (`Response rsp)

    (* REST API: Dynamic path segments *)
    | meth, path when String.length path > 14 && String.sub path 0 14 = "/api/v1/tasks/" ->
        let rest = String.sub path 14 (String.length path - 14) in
        (match meth, String.split_on_char '/' rest with
        | `POST, [task_id; "claim"] ->
            let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
            let params = match params with
              | `Assoc l -> `Assoc (("task_id", `String task_id) :: l)
              | _ -> `Assoc [("task_id", `String task_id)]
            in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_claim" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`OK ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | `POST, [task_id; "done"] ->
            let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
            let params = match params with
              | `Assoc l -> `Assoc (("task_id", `String task_id) :: l)
              | _ -> `Assoc [("task_id", `String task_id)]
            in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_done" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`OK ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | `POST, [task_id; "cancel"] ->
            let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
            let params = match params with
              | `Assoc l -> `Assoc (("task_id", `String task_id) :: l)
              | _ -> `Assoc [("task_id", `String task_id)]
            in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_cancel_task" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`OK ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | _ ->
            let* rsp = Server.respond_string ~status:`Not_found ~body:"Not Found" () in
            Lwt.return (`Response rsp))

    | meth, path when String.length path > 15 && String.sub path 0 15 = "/api/v1/agents/" ->
        let agent_name = String.sub path 15 (String.length path - 15) in
        (match meth with
        | `DELETE ->
            let params = `Assoc [("agent_name", `String agent_name)] in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_leave" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`OK ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | _ ->
            let* rsp = Server.respond_string ~status:`Method_not_allowed ~body:"Method Not Allowed" () in
            Lwt.return (`Response rsp))

    | meth, path when String.length path > 14 && String.sub path 0 14 = "/api/v1/votes/" ->
        let rest = String.sub path 14 (String.length path - 14) in
        (match meth, String.split_on_char '/' rest with
        | `GET, [vote_id] ->
            let params = `Assoc [("vote_id", `String vote_id)] in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_vote_status" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`OK ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | `POST, [vote_id; "cast"] ->
            let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
            let params = match params with
              | `Assoc l -> `Assoc (("vote_id", `String vote_id) :: l)
              | _ -> `Assoc [("vote_id", `String vote_id)]
            in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_vote_cast" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`OK ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | _ ->
            let* rsp = Server.respond_string ~status:`Not_found ~body:"Not Found" () in
            Lwt.return (`Response rsp))

    | meth, path when String.length path > 17 && String.sub path 0 17 = "/api/v1/planning/" ->
        let rest = String.sub path 17 (String.length path - 17) in
        (match meth, String.split_on_char '/' rest with
        | `POST, [task_id] ->
            let params = `Assoc [("task_id", `String task_id)] in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_plan_init" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`Created ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | `GET, [task_id] ->
            let params = `Assoc [("task_id", `String task_id)] in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_plan_get" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`OK ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | `PUT, [task_id; "plan"] ->
            let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
            let params = match params with
              | `Assoc l -> `Assoc (("task_id", `String task_id) :: l)
              | _ -> `Assoc [("task_id", `String task_id)]
            in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_plan_update" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`OK ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | `POST, [task_id; "notes"] ->
            let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
            let params = match params with
              | `Assoc l -> `Assoc (("task_id", `String task_id) :: l)
              | _ -> `Assoc [("task_id", `String task_id)]
            in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_note_add" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`Created ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | `PUT, [task_id; "deliverable"] ->
            let* body_str = Cohttp_lwt.Body.to_string body in
        let params = try Yojson.Safe.from_string body_str with _ -> `Assoc [] in
            let params = match params with
              | `Assoc l -> `Assoc (("task_id", `String task_id) :: l)
              | _ -> `Assoc [("task_id", `String task_id)]
            in
            let* result = Masc_mcp.Mcp_server.rest_execute state ~name:"masc_deliver" ~arguments:params in
            let headers = Cohttp.Header.of_list [
              ("Content-Type", "application/json");
              ("Access-Control-Allow-Origin", "*");
            ] in
            let* rsp = Server.respond_string ~status:`OK ~headers
              ~body:(Yojson.Safe.to_string result) () in
            Lwt.return (`Response rsp)
        | _ ->
            let* rsp = Server.respond_string ~status:`Not_found ~body:"Not Found" () in
            Lwt.return (`Response rsp))

    | _ ->
        let* rsp = Server.respond_string ~status:`Not_found ~body:"Not Found" () in
        Lwt.return (`Response rsp)
  in

  let conn_closed conn =
    stop_sse_conn_by_key (sse_conn_key conn)
  in

  (* Wrap callback to track active requests, reject during shutdown, and handle errors *)
  let callback_with_tracking conn req body =
    let open Lwt.Syntax in
    if Shutdown.is_shutting_down () then begin
      let headers = Cohttp.Header.of_list [
        ("Content-Type", "application/json");
        ("Connection", "close");
      ] in
      let body = {|{"error":"Server is shutting down"}|} in
      let* rsp = Server.respond_string ~status:`Service_unavailable ~headers ~body () in
      Lwt.return (`Response rsp)
    end else begin
      Shutdown.incr_requests ();
      Lwt.finalize
        (fun () ->
          Lwt.catch
            (fun () -> callback conn req body)
            (fun exn ->
              (* Log the error with details for debugging *)
              let path = Uri.path (Cohttp.Request.uri req) in
              let meth = Cohttp.Code.string_of_method (Cohttp.Request.meth req) in
              let error_msg = Printexc.to_string exn in
              let backtrace = Printexc.get_backtrace () in
              Printf.eprintf "[MASC ERROR] %s %s: %s\n%s%!" meth path error_msg backtrace;
              (* Return 500 Internal Server Error instead of crashing *)
              let headers = Cohttp.Header.of_list [
                ("Content-Type", "application/json");
                ("Access-Control-Allow-Origin", "*");
              ] in
              let error_body = Printf.sprintf
                {|{"jsonrpc":"2.0","error":{"code":-32603,"message":"Internal error: %s"}}|}
                (String.escaped error_msg)
              in
              let* rsp = Server.respond_string ~status:`Internal_server_error ~headers ~body:error_body () in
              Lwt.return (`Response rsp)))
        (fun () -> Shutdown.decr_requests (); Lwt.return_unit)
    end
  in

  (* Setup signal handlers for graceful shutdown *)
  let setup_signal_handlers () =
    let handle_signal _ =
      Shutdown.request_shutdown ()
    in
    ignore (Lwt_unix.on_signal Sys.sigterm handle_signal);
    ignore (Lwt_unix.on_signal Sys.sigint handle_signal)
  in

  let server_loop () =
    let open Lwt.Syntax in
    setup_signal_handlers ();
    Lwt.catch
      (fun () ->
        let server = Server.create ~mode:(`TCP (`Port port))
          (Server.make_response_action ~callback:callback_with_tracking ~conn_closed ())
        in
        (* Wait for either server completion or shutdown signal *)
        let* () = Lwt.pick [
          server;
          (let* () = Shutdown.wait_for_shutdown () in
           Printf.eprintf "📤 Notifying SSE clients of shutdown...\n%!";
           Sse.broadcast (`Assoc [
             ("type", `String "shutdown");
             ("message", `String "Server is shutting down");
           ]);
           Printf.eprintf "⏳ Waiting for %d pending requests (max 30s)...\n%!" (Shutdown.active_count ());
           let* () = Shutdown.wait_for_pending ~timeout_sec:30.0 in
           Printf.eprintf "✅ Graceful shutdown complete\n%!";
           Lwt.return_unit)
        ] in
        Lwt.return_unit)
      (fun exn ->
        let error_msg = Printexc.to_string exn in
        (match exn with
        | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
            Printf.eprintf "❌ [MASC FATAL] Port %d is already in use. Another instance may be running.\n%!" port;
            Printf.eprintf "   Try: lsof -i :%d | grep LISTEN\n%!" port
        | Unix.Unix_error (Unix.EACCES, _, _) ->
            Printf.eprintf "❌ [MASC FATAL] Permission denied binding to port %d.\n%!" port;
            Printf.eprintf "   Try a port > 1024 or run with elevated privileges.\n%!"
        | _ ->
            Printf.eprintf "❌ [MASC FATAL] Server crashed: %s\n%!" error_msg;
            Printf.eprintf "   Backtrace: %s\n%!" (Printexc.get_backtrace ()));
        Lwt.return_unit)
  in

  Lwt_main.run (server_loop ())

(** Stdio mode *)
let run_stdio ~base_path =
  let state = Masc_mcp.Mcp_server.create_state ~base_path in
  Lwt_main.run (Masc_mcp.Mcp_server.run_stdio state)

(** Command line arguments *)
let http_flag =
  let doc = "Run as HTTP server (default; kept for compatibility)." in
  Arg.(value & flag & info ["http"] ~doc)

let stdio_flag =
  let doc = "Run as stdio server (legacy; no SSE)." in
  Arg.(value & flag & info ["stdio"] ~doc)

let port_arg =
  let doc = "HTTP port (default: 8935)." in
  Arg.(value & opt int 8935 & info ["port"; "p"] ~docv:"PORT" ~doc)

let path_arg =
  let doc = "Base path for MASC room (default: $ME_ROOT or current directory; worktrees resolve to git root)." in
  Arg.(value & opt string (default_base_path ()) & info ["path"; "d"] ~docv:"PATH" ~doc)

let grpc_port_arg =
  let doc = "gRPC server port (default: none - disabled)." in
  Arg.(value & opt (some int) None & info ["grpc-port"] ~docv:"PORT" ~doc)

(** Main command *)
let main _http stdio port grpc_port base_path =
  if stdio then
    run_stdio ~base_path
  else begin
    (* gRPC server temporarily disabled due to h2 version conflict with httpun-eio *)
    (* TODO: Re-enable when grpc-eio or compatible h2 version available *)
    (match grpc_port with
    | Some gport ->
      Printf.printf "⚠️  gRPC server (port %d) temporarily disabled due to h2 version conflict\n%!" gport
    | None -> ());
    (* Start HTTP server *)
    run_http ~port ~base_path
  end

(** Dashboard command - show status in terminal *)
let dashboard compact base_path =
  let config = Masc_mcp.Room_utils.default_config base_path in
  let output =
    if compact then Masc_mcp.Dashboard.generate_compact config
    else Masc_mcp.Dashboard.generate config
  in
  print_endline output

let compact_flag =
  let doc = "Show compact single-line summary." in
  Arg.(value & flag & info ["compact"; "c"] ~doc)

let dashboard_cmd =
  let doc = "Show MASC dashboard with agents, tasks, locks, messages, tempo, and worktrees." in
  let info = Cmd.info "dashboard" ~doc in
  Cmd.v info Term.(const dashboard $ compact_flag $ path_arg)

(** Server command (default) *)
let server_cmd =
  let doc = "Start MASC MCP server (HTTP or stdio)." in
  let info = Cmd.info "server" ~doc in
  Cmd.v info Term.(const main $ http_flag $ stdio_flag $ port_arg $ grpc_port_arg $ path_arg)

let cmd =
  let doc = "MASC - Multi-Agent Streaming Coordination (MCP 2025-11-25)" in
  let info = Cmd.info "masc-mcp" ~version:"2.2.2" ~doc in
  Cmd.group info ~default:Term.(const main $ http_flag $ stdio_flag $ port_arg $ grpc_port_arg $ path_arg) [
    server_cmd;
    dashboard_cmd;
  ]

let () = exit (Cmd.eval cmd)
