(** MASC MCP Server - Eio Native Entry Point
    MCP Streamable HTTP Transport with Eio concurrency (OCaml 5.x)

    Uses httpun-eio with httpun-ws-eio for WebSocket upgrades.
*)

[@@@warning "-32-69"]  (* Suppress unused values/fields during migration *)

open Cmdliner

(** Module aliases *)
module Http = Masc_mcp.Http_server_eio
module Mcp_session = Masc_mcp.Mcp_session
module Mcp_server = Masc_mcp.Mcp_server
module Mcp_eio = Masc_mcp.Mcp_server_eio
module Room_utils = Masc_mcp.Room_utils
module Graphql_api = Masc_mcp.Graphql_api
module Http_negotiation = Masc_mcp.Mcp_protocol.Http_negotiation
module Progress = Masc_mcp.Progress
module Sse = Masc_mcp.Sse

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

(** Build legacy SSE messages endpoint URL (event: endpoint) *)
let legacy_messages_endpoint_url (request : Httpun.Request.t) session_id =
  match Httpun.Headers.get request.headers "host" with
  | Some host -> Printf.sprintf "http://%s/messages?session_id=%s" host session_id
  | None -> Printf.sprintf "/messages?session_id=%s" session_id

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

(** Utility: string prefix check *)
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

(** Validate Origin header for DNS rebinding protection *)
let validate_origin (request : Httpun.Request.t) =
  match Httpun.Headers.get request.headers "origin" with
  | None -> true
  | Some origin ->
      List.exists (fun prefix -> starts_with ~prefix origin) allowed_origins

(** Check if client accepts SSE *)
let accepts_sse (request : Httpun.Request.t) =
  Http_negotiation.accepts_sse_header
    (Httpun.Headers.get request.headers "accept")

(** Check if client accepts MCP Streamable HTTP (JSON + SSE) *)
let accepts_streamable_mcp (request : Httpun.Request.t) =
  Http_negotiation.accepts_streamable_mcp
    (Httpun.Headers.get request.headers "accept")

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

(** Get Last-Event-ID from headers for resumability *)
let get_last_event_id (request : Httpun.Request.t) =
  match Httpun.Headers.get request.headers "last-event-id" with
  | Some id -> (try Some (int_of_string id) with _ -> None)
  | None -> None

(** CORS origin *)
let get_origin (request : Httpun.Request.t) =
  match Httpun.Headers.get request.headers "origin" with
  | Some o -> o
  | None -> "*"

(** CORS headers *)
let cors_headers origin = [
  ("access-control-allow-origin", origin);
  ("access-control-allow-methods", "GET, POST, DELETE, OPTIONS");
  ("access-control-allow-headers",
   "Content-Type, Accept, Origin, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-Id");
  ("access-control-expose-headers", "Mcp-Session-Id, Mcp-Protocol-Version");
  ("access-control-allow-credentials", "true");
]

(** Common MCP headers *)
let mcp_headers session_id protocol_version = [
  ("mcp-session-id", session_id);
  ("mcp-protocol-version", protocol_version);
]

(** SSE response headers *)
let sse_headers session_id protocol_version origin =
  [("content-type", Http_negotiation.sse_content_type)]
  @ mcp_headers session_id protocol_version
  @ cors_headers origin

(** SSE stream headers (with keep-alive) *)
let sse_stream_headers session_id protocol_version origin =
  [
    ("content-type", Http_negotiation.sse_content_type);
    ("cache-control", "no-cache");
    ("connection", "keep-alive");
  ]
  @ mcp_headers session_id protocol_version
  @ cors_headers origin

(** JSON response headers *)
let json_headers session_id protocol_version origin =
  [("content-type", "application/json")]
  @ mcp_headers session_id protocol_version
  @ cors_headers origin

(** GraphQL response headers *)
let graphql_headers origin =
  [("content-type", "application/json")]
  @ cors_headers origin

(** GraphiQL playground HTML (GET /graphql) *)
let graphql_playground_html ~nonce =
  Printf.sprintf {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>MASC GraphQL Playground</title>
    <link rel="stylesheet" href="/graphiql/graphiql.min.css" />
    <style nonce="%s">
      html, body, #graphiql { height: 100%%; margin: 0; }
    </style>
  </head>
  <body>
    <div id="graphiql">Loading...</div>
    <script src="/graphiql/react.production.min.js"></script>
    <script src="/graphiql/react-dom.production.min.js"></script>
    <script src="/graphiql/graphiql.min.js"></script>
    <script nonce="%s">
      const graphQLFetcher = function (graphQLParams) {
        return fetch("/graphql", {
          method: "post",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(graphQLParams),
        }).then(function (response) {
          return response.json();
        });
      };
      const defaultQuery =
        "{ status { protocolVersion project messageSeq activeAgents paused } " +
        "tasks(first: 10) { totalCount edges { node { id title priority status { status assignee } } } } }";
      ReactDOM.render(
        React.createElement(GraphiQL, {
          fetcher: graphQLFetcher,
          defaultQuery: defaultQuery
        }),
        document.getElementById("graphiql")
      );
    </script>
  </body>
</html>
|} nonce nonce

let graphql_csp_header nonce =
  Printf.sprintf
    "default-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'; \
     connect-src 'self'; img-src 'self' data:; \
     script-src 'self' 'nonce-%s' 'unsafe-eval'; \
     style-src 'self' 'nonce-%s'; \
     font-src 'self' data:"
    nonce nonce

(** Local GraphiQL assets *)
let graphiql_asset_root () =
  Filename.concat (Sys.getcwd ()) "assets/graphiql"

let graphiql_asset_path name =
  Filename.concat (graphiql_asset_root ()) name

let graphiql_asset_content_type name =
  if Filename.check_suffix name ".css" then
    "text/css; charset=utf-8"
  else if Filename.check_suffix name ".js" then
    "application/javascript; charset=utf-8"
  else
    "application/octet-stream"

let read_file path =
  try Ok (In_channel.with_open_bin path In_channel.input_all)
  with exn -> Error (Printexc.to_string exn)

let serve_graphiql_asset name _request reqd =
  let path = graphiql_asset_path name in
  match read_file path with
  | Ok body ->
      Http.Response.bytes ~content_type:(graphiql_asset_content_type name) body reqd
  | Error _ ->
      Http.Response.not_found reqd

(** CORS preflight response headers *)
let cors_preflight_headers origin =
  [
    ("access-control-allow-origin", origin);
    ("access-control-allow-methods", "GET, POST, DELETE, OPTIONS");
    ("access-control-allow-headers",
     "Content-Type, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-Id, Accept, Origin");
    ("access-control-expose-headers", "Mcp-Session-Id, Mcp-Protocol-Version");
  ]

(** Server state - initialized at startup *)
let server_state : Mcp_server.server_state option ref = ref None

(** JSON-RPC error response helper *)
let json_rpc_error code message =
  Printf.sprintf
    {|{"jsonrpc":"2.0","error":{"code":%d,"message":"%s"},"id":null}|}
    code
    (String.escaped message)

let is_http_error_response = function
  | `Assoc fields ->
      let id_is_null =
        match List.assoc_opt "id" fields with
        | Some `Null -> true
        | _ -> false
      in
      let code =
        match List.assoc_opt "error" fields with
        | Some (`Assoc err_fields) ->
            (match List.assoc_opt "code" err_fields with
             | Some (`Int c) -> Some c
             | _ -> None)
        | _ -> None
      in
      id_is_null && (code = Some (-32700) || code = Some (-32600))
  | _ -> false

(** Health check handler *)
let health_handler _request reqd =
  let json = {|{"status":"ok","server":"masc-mcp","version":"2.2.1"}|} in
  Http.Response.json json reqd

(** CORS preflight handler *)
let options_handler request reqd =
  let origin = get_origin request in
  let headers = Httpun.Headers.of_list (
    ("content-length", "0") :: cors_preflight_headers origin
  ) in
  let response = Httpun.Response.create ~headers `No_content in
  Httpun.Reqd.respond_with_string reqd response ""

(** Eio switch and clock references for MCP handlers *)
let current_sw : Eio.Switch.t option ref = ref None
let current_clock : float Eio.Time.clock_ty Eio.Resource.t option ref = ref None

let http_status_of_graphql = function
  | `OK -> `OK
  | `Bad_request -> `Bad_request

let handle_get_graphql _request reqd =
  let nonce =
    let rng = Random.State.make_self_init () in
    let bytes = Bytes.init 16 (fun _ -> Char.chr (Random.State.int rng 256)) in
    Base64.encode_string (Bytes.to_string bytes)
  in
  let headers = [
    ("content-security-policy", graphql_csp_header nonce);
  ] in
  let body = graphql_playground_html ~nonce in
  Http.Response.html ~headers body reqd

let handle_post_graphql request reqd =
  let origin = get_origin request in
  Http.Request.read_body_async reqd (fun body_str ->
    let state = match !server_state with
      | Some s -> s
      | None -> failwith "Server state not initialized"
    in
    let response = Graphql_api.handle_request ~config:state.room_config body_str in
    let status = http_status_of_graphql response.status in
    let headers = Httpun.Headers.of_list (
      ("content-length", string_of_int (String.length response.body))
      :: graphql_headers origin
    ) in
    let http_response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd http_response response.body
  )

let handle_graphql request reqd =
  match Http.Request.method_ request with
  | `GET -> handle_get_graphql request reqd
  | `POST -> handle_post_graphql request reqd
  | _ -> Http.Response.method_not_allowed reqd

(** MCP POST handler - async body reading with callback-based response *)
let handle_post_mcp request reqd =
  let origin = get_origin request in
  let session_id =
    match get_session_id_any request with
    | Some id -> id
    | None -> Mcp_session.generate ()
  in

  Http.Request.read_body_async reqd (fun body_str ->
    try
      let state = match !server_state with
        | Some s -> s
        | None -> failwith "Server state not initialized"
      in
      let sw = match !current_sw with
        | Some s -> s
        | None -> failwith "Eio switch not initialized"
      in
      let clock = match !current_clock with
        | Some c -> c
        | None -> failwith "Eio clock not initialized"
      in
      let response_json = Mcp_eio.handle_request ~clock ~sw state body_str in
      (match protocol_version_from_body body_str with
       | Some v -> remember_protocol_version session_id v
       | None -> ());
      let protocol_version = get_protocol_version_for_session ~session_id request in
      if not (accepts_streamable_mcp request) then
        let body = json_rpc_error (-32600)
          "Invalid Accept header: must include application/json and text/event-stream"
        in
        let headers = Httpun.Headers.of_list (
          ("content-length", string_of_int (String.length body))
          :: json_headers session_id protocol_version origin
        ) in
        let response = Httpun.Response.create ~headers `Bad_request in
        Httpun.Reqd.respond_with_string reqd response body
      else
        let wants_sse = accepts_sse request && not force_json_response in
        if wants_sse then begin
          match response_json with
          | `Null ->
              let headers = Httpun.Headers.of_list (
                ("content-length", "0")
                :: mcp_headers session_id protocol_version
              ) in
              let response = Httpun.Response.create ~headers `Accepted in
              Httpun.Reqd.respond_with_string reqd response ""
          | json when is_http_error_response json ->
              let body = Yojson.Safe.to_string json in
              let headers = Httpun.Headers.of_list (
                ("content-length", string_of_int (String.length body))
                :: json_headers session_id protocol_version origin
              ) in
              let response = Httpun.Response.create ~headers `Bad_request in
              Httpun.Reqd.respond_with_string reqd response body
          | json ->
              let event = Sse.format_event ~event_type:"message" (Yojson.Safe.to_string json) in
              let body = sse_prime_event () ^ event in
              let headers = Httpun.Headers.of_list (
                ("content-length", string_of_int (String.length body))
                :: sse_headers session_id protocol_version origin
              ) in
              let response = Httpun.Response.create ~headers `OK in
              Httpun.Reqd.respond_with_string reqd response body
        end else begin
          match response_json with
          | `Null ->
              let headers = Httpun.Headers.of_list (
                ("content-length", "0")
                :: mcp_headers session_id protocol_version
              ) in
              let response = Httpun.Response.create ~headers `Accepted in
              Httpun.Reqd.respond_with_string reqd response ""
          | json when is_http_error_response json ->
              let body = Yojson.Safe.to_string json in
              let headers = Httpun.Headers.of_list (
                ("content-length", string_of_int (String.length body))
                :: json_headers session_id protocol_version origin
              ) in
              let response = Httpun.Response.create ~headers `Bad_request in
              Httpun.Reqd.respond_with_string reqd response body
          | json ->
              let body = Yojson.Safe.to_string json in
              let headers = Httpun.Headers.of_list (
                ("content-length", string_of_int (String.length body))
                :: json_headers session_id protocol_version origin
              ) in
              let response = Httpun.Response.create ~headers `OK in
              Httpun.Reqd.respond_with_string reqd response body
        end
    with exn ->
      let protocol_version = get_protocol_version_for_session ~session_id request in
      let body = json_rpc_error (-32603) ("Internal error: " ^ Printexc.to_string exn) in
      let headers = Httpun.Headers.of_list (
        ("content-length", string_of_int (String.length body))
        :: json_headers session_id protocol_version origin
      ) in
        let response = Httpun.Response.create ~headers `Internal_server_error in
        Httpun.Reqd.respond_with_string reqd response body
  )

(** SSE connection tracking (prevents leaks / stale sessions) *)
type sse_conn_info = {
  session_id: string;
  client_id: int;
  writer: Httpun.Body.Writer.t;
  mutex: Eio.Mutex.t;
  stop: bool ref;
  mutable closed: bool;
}

let sse_conn_by_session : (string, sse_conn_info) Hashtbl.t = Hashtbl.create 128

let close_sse_conn info =
  if not info.closed then begin
    info.closed <- true;
    info.stop := true;
    (try Httpun.Body.Writer.close info.writer with
     | exn ->
         (* Expected during client disconnect - log for debugging *)
         Printf.eprintf "[DEBUG] close_sse_conn: %s\n%!" (Printexc.to_string exn))
  end

let stop_sse_session session_id =
  match Hashtbl.find_opt sse_conn_by_session session_id with
  | None -> ()
  | Some info ->
      Hashtbl.remove sse_conn_by_session session_id;
      close_sse_conn info;
      Sse.unregister_if_current info.session_id info.client_id;
      Printf.printf "📴 SSE disconnected: %s\n%!" info.session_id

let send_raw info data =
  if info.closed || !(info.stop) || Httpun.Body.Writer.is_closed info.writer then
    (close_sse_conn info; false)
  else
    try
      Eio.Mutex.use_rw ~protect:true info.mutex (fun () ->
        Httpun.Body.Writer.write_string info.writer data;
        Httpun.Body.Writer.flush info.writer (fun _ -> ())
      );
      true
    with exn ->
      (* Expected during client disconnect - log for debugging *)
      Printf.eprintf "[DEBUG] send_raw failed (%s): %s\n%!"
        info.session_id (Printexc.to_string exn);
      close_sse_conn info;
      false

let handle_get_mcp ?legacy_messages_endpoint request reqd =
  let origin = get_origin request in
  let session_id = Mcp_session.get_or_generate (get_session_id_any request) in
  let protocol_version = get_protocol_version_for_session ~session_id request in
  let last_event_id = get_last_event_id request in

  (* Replace existing connection for session_id *)
  stop_sse_session session_id;

  let headers = Httpun.Headers.of_list (sse_stream_headers session_id protocol_version origin) in
  let response = Httpun.Response.create ~headers `OK in
  let writer = Httpun.Reqd.respond_with_streaming reqd response in
  let mutex = Eio.Mutex.create () in
  let info_ref : sse_conn_info option ref = ref None in
  let push event =
    match !info_ref with
    | None -> ()
    | Some info -> ignore (send_raw info event)
  in
  let client_id =
    Sse.register session_id ~push
      ~last_event_id:(Option.value ~default:0 last_event_id)
  in
  let info = {
    session_id;
    client_id;
    writer;
    mutex;
    stop = ref false;
    closed = false;
  } in
  info_ref := Some info;
  Hashtbl.replace sse_conn_by_session session_id info;

  (* Send priming event first *)
  ignore (send_raw info (sse_prime_event ()));

  (* Legacy SSE transport: provide messages endpoint (event: endpoint) *)
  (match legacy_messages_endpoint with
   | None -> ()
   | Some f ->
       let endpoint_url = f session_id in
       ignore (send_raw info (Sse.format_event ~event_type:"endpoint" endpoint_url)));

  (* Replay missed events if Last-Event-ID provided (MCP spec MUST) *)
  (match last_event_id with
   | Some last_id ->
       let missed = Sse.get_events_after last_id in
       List.iter (fun ev -> ignore (send_raw info ev)) missed
   | None -> ());

  (* Keep-alive ping loop *)
  (match !current_sw, !current_clock with
   | Some sw, Some clock ->
       Eio.Fiber.fork ~sw (fun () ->
         while not !(info.stop) do
           Eio.Time.sleep clock sse_ping_interval_s;
           if info.closed then
             stop_sse_session info.session_id
           else if not !(info.stop) then
             ignore (send_raw info ": ping\n\n")
         done;
         if info.closed then stop_sse_session info.session_id)
   | _ -> ());

  Printf.printf "📡 SSE connected: %s (last_event_id: %s)\n%!"
    session_id
    (match last_event_id with Some id -> string_of_int id | None -> "none")

(** SSE simple handler - for compatibility, returns single event *)
let sse_simple_handler request reqd =
  let origin = get_origin request in
  let session_id = Mcp_session.get_or_generate (get_session_id_any request) in
  let protocol_version = get_protocol_version_for_session ~session_id request in
  let event = sse_prime_event ()
              ^ Sse.format_event ~event_type:"connected"
                  (Printf.sprintf {|{"session_id":"%s"}|} session_id)
  in
  let headers = Httpun.Headers.of_list (
    ("content-length", string_of_int (String.length event))
    :: sse_headers session_id protocol_version origin
  ) in
  let response = Httpun.Response.create ~headers `OK in
  Httpun.Reqd.respond_with_string reqd response event

(** POST /messages - Legacy SSE transport (client->server messages) *)
let handle_post_messages request reqd =
  let origin = get_origin request in
  match get_session_id_any request with
  | None ->
      let body = "session_id required" in
      let headers = Httpun.Headers.of_list (
        ("content-length", string_of_int (String.length body))
        :: cors_headers origin
      ) in
      let response = Httpun.Response.create ~headers `Bad_request in
      Httpun.Reqd.respond_with_string reqd response body
  | Some session_id when not (Mcp_session.is_valid session_id) ->
      let body = "invalid session_id" in
      let headers = Httpun.Headers.of_list (
        ("content-length", string_of_int (String.length body))
        :: cors_headers origin
      ) in
      let response = Httpun.Response.create ~headers `Bad_request in
      Httpun.Reqd.respond_with_string reqd response body
  | Some session_id ->
      let protocol_version = get_protocol_version_for_session ~session_id request in
      Http.Request.read_body_async reqd (fun body_str ->
        let state = match !server_state with
          | Some s -> s
          | None -> failwith "Server state not initialized"
        in
        let sw = match !current_sw with
          | Some s -> s
          | None -> failwith "Eio switch not initialized"
        in
        let clock = match !current_clock with
          | Some c -> c
          | None -> failwith "Eio clock not initialized"
        in
        let response_json = Mcp_eio.handle_request ~clock ~sw state body_str in
        (match response_json with
         | `Null -> ()
         | json -> Sse.send_to session_id json);
        let headers = Httpun.Headers.of_list (
          ("content-length", "0")
          :: mcp_headers session_id protocol_version
        ) in
        let response = Httpun.Response.create ~headers `Accepted in
        Httpun.Reqd.respond_with_string reqd response ""
      )

(** DELETE /mcp - Session termination *)
let handle_delete_mcp request reqd =
  match get_session_id_any request with
  | Some session_id ->
      stop_sse_session session_id;
      Sse.unregister session_id;
      Hashtbl.remove protocol_version_by_session session_id;
      Printf.printf "🔚 Session terminated: %s\n%!" session_id;
      let headers = Httpun.Headers.of_list (
        ("content-length", "0")
        :: mcp_headers session_id (get_protocol_version request)
      ) in
      let response = Httpun.Response.create ~headers `No_content in
      Httpun.Reqd.respond_with_string reqd response ""
  | None ->
      let body = "Mcp-Session-Id required" in
      let headers = Httpun.Headers.of_list [
        ("content-length", string_of_int (String.length body));
      ] in
      let response = Httpun.Response.create ~headers `Bad_request in
      Httpun.Reqd.respond_with_string reqd response body

(** Build routes for MCP server *)
let make_routes () =
  Http.Router.empty
  |> Http.Router.get "/health" health_handler
  |> Http.Router.get "/" (fun _req reqd -> Http.Response.text "MASC MCP Server" reqd)
  |> Http.Router.get "/graphiql/graphiql.min.css"
       (serve_graphiql_asset "graphiql.min.css")
  |> Http.Router.get "/graphiql/graphiql.min.js"
       (serve_graphiql_asset "graphiql.min.js")
  |> Http.Router.get "/graphiql/react.production.min.js"
       (serve_graphiql_asset "react.production.min.js")
  |> Http.Router.get "/graphiql/react-dom.production.min.js"
       (serve_graphiql_asset "react-dom.production.min.js")
  |> Http.Router.get "/mcp" (fun request reqd -> handle_get_mcp request reqd)
  |> Http.Router.post "/" handle_post_mcp
  |> Http.Router.post "/mcp" handle_post_mcp
  |> Http.Router.add ~path:"/graphql" ~methods:[`GET; `POST] ~handler:handle_graphql
  |> Http.Router.post "/messages" handle_post_messages
  |> Http.Router.get "/sse"
       (fun request reqd ->
         handle_get_mcp
           ~legacy_messages_endpoint:(legacy_messages_endpoint_url request)
           request reqd)
  |> Http.Router.get "/sse/simple" sse_simple_handler

(** Extended router to handle OPTIONS *)
let make_extended_handler routes =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    let path = Http.Request.path request in
    let is_mcp_like =
      String.equal path "/mcp"
      || String.equal path "/sse"
      || String.equal path "/messages"
    in
    let session_id_for_version = get_session_id_any request in
    let protocol_version =
      get_protocol_version_for_session ?session_id:session_id_for_version request
    in
    let origin = get_origin request in
    if is_mcp_like && not (validate_origin request) then
      let body = json_rpc_error (-32600) "Invalid origin" in
      let headers = Httpun.Headers.of_list (
        ("content-length", string_of_int (String.length body))
        :: json_headers "-" protocol_version origin
      ) in
      let response = Httpun.Response.create ~headers `Forbidden in
      Httpun.Reqd.respond_with_string reqd response body
    else if is_mcp_like && request.meth <> `OPTIONS &&
            not (is_valid_protocol_version protocol_version) then
      let body = json_rpc_error (-32600) "Unsupported protocol version" in
      let headers = Httpun.Headers.of_list (
        ("content-length", string_of_int (String.length body))
        :: json_headers "-" protocol_version origin
      ) in
      let response = Httpun.Response.create ~headers `Bad_request in
      Httpun.Reqd.respond_with_string reqd response body
    else
      match request.meth, path with
      | `OPTIONS, _ -> options_handler request reqd
      | `DELETE, "/mcp" -> handle_delete_mcp request reqd
      | _ -> Http.Router.dispatch routes request reqd

(** Main server loop *)
let run_server ~sw ~env ~port ~base_path =
  (* Extract components from Eio environment *)
  let clock = Eio.Stdenv.clock env in
  let mono_clock = Eio.Stdenv.mono_clock env in
  let net = Eio.Stdenv.net env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in

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
  Mcp_server.set_sse_callback state Sse.broadcast;
  Progress.set_sse_callback Sse.broadcast;
  Masc_mcp.Orchestrator.start ~sw ~clock ~domain_mgr state.room_config;

  let config = { Http.default_config with port; host = "127.0.0.1" } in
  let routes = make_routes () in
  let request_handler = make_extended_handler routes in

  let ip = Eio.Net.Ipaddr.V4.loopback in
  let addr = `Tcp (ip, config.port) in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:config.max_connections addr in

  let resolved_base = state.room_config.base_path in
  let masc_dir = Filename.concat resolved_base ".masc" in
  Printf.printf "🚀 MASC MCP Server listening on http://%s:%d\n%!" config.host config.port;
  Printf.printf "   Base path: %s\n%!" resolved_base;
  if resolved_base <> base_path then
    Printf.printf "   Base path (input): %s\n%!" base_path;
  Printf.printf "   MASC dir: %s\n%!" masc_dir;
  Printf.printf "   GET  /mcp → SSE stream (notifications)\n%!";
  Printf.printf "   POST /mcp → JSON-RPC (Accept: text/event-stream for SSE)\n%!";
  Printf.printf "   DELETE /mcp → Session termination\n%!";
  Printf.printf "   POST /graphql → GraphQL (read-only)\n%!";
  Printf.printf "   GET  /sse → legacy SSE stream (event: endpoint)\n%!";
  Printf.printf "   POST /messages → legacy client->server messages\n%!";
  Printf.printf "   GET  /health → Health check\n%!";

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
  let doc = "Base path for MASC data (.masc folder location)" in
  Arg.(value & opt string (default_base_path ()) & info ["base-path"] ~docv:"PATH" ~doc)

(** Graceful shutdown exception *)
exception Shutdown

let run_cmd port base_path =
  Eio_main.run @@ fun env ->
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
      Sse.broadcast (Yojson.Safe.from_string shutdown_data);
      Printf.eprintf "🚀 MASC MCP: Sent shutdown notification to %d SSE clients\n%!" (Sse.client_count ());

      (* Give clients 500ms to receive the notification before killing connections *)
      Unix.sleepf 0.5;

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
    run_server ~sw ~env ~port ~base_path
  with
  | Shutdown ->
      Printf.eprintf "🚀 MASC MCP: Shutdown complete.\n%!"
  | Eio.Cancel.Cancelled _ ->
      Printf.eprintf "🚀 MASC MCP: Shutdown complete.\n%!")

let cmd =
  let doc = "MASC MCP Server" in
  let info = Cmd.info "masc-mcp" ~version:"2.2.1" ~doc in
  Cmd.v info Term.(const run_cmd $ port $ base_path)

let () = exit (Cmd.eval cmd)
