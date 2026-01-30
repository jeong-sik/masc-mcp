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
module Room = Masc_mcp.Room
module Room_utils = Masc_mcp.Room_utils
module Graphql_api = Masc_mcp.Graphql_api
module Types = Masc_mcp.Types
module Tempo = Masc_mcp.Tempo
module Auth = Masc_mcp.Auth
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

(** Parse query param from request target *)
let query_param request key =
  let uri = Uri.of_string request.Httpun.Request.target in
  Uri.get_query_param uri key

let int_query_param request key ~default =
  match query_param request key with
  | None -> default
  | Some s -> (try int_of_string s with _ -> default)

let bearer_token_from_header value =
  let prefix = "Bearer " in
  let prefix_lower = "bearer " in
  if String.length value >= String.length prefix &&
     String.sub value 0 (String.length prefix) = prefix then
    Some (String.sub value (String.length prefix) (String.length value - String.length prefix))
  else if String.length value >= String.length prefix_lower &&
          String.sub value 0 (String.length prefix_lower) = prefix_lower then
    Some (String.sub value (String.length prefix_lower) (String.length value - String.length prefix_lower))
  else
    None

let auth_token_from_request request =
  match Httpun.Headers.get request.Httpun.Request.headers "authorization" with
  | Some v -> bearer_token_from_header v
  | None -> query_param request "token"

let agent_from_request request =
  match Httpun.Headers.get request.Httpun.Request.headers "x-masc-agent" with
  | Some v -> Some v
  | None ->
      match Httpun.Headers.get request.Httpun.Request.headers "x-masc-agent-name" with
      | Some v -> Some v
      | None ->
          (match query_param request "agent" with
           | Some v -> Some v
           | None -> query_param request "agent_name")

let http_status_of_auth_error = function
  | Types.Unauthorized _ | Types.InvalidToken _ | Types.TokenExpired _ -> `Unauthorized
  | Types.Forbidden _ -> `Forbidden
  | _ -> `Internal_server_error

(** Server state - initialized at startup *)
let server_state : Mcp_server.server_state option ref = ref None

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

let respond_auth_error request reqd err =
  let status = http_status_of_auth_error err in
  let origin = get_origin request in
  let body = Yojson.Safe.to_string (`Assoc [
    ("error", `String (Types.masc_error_to_string err));
  ]) in
  let headers = Httpun.Headers.of_list (
    ("content-length", string_of_int (String.length body))
    :: cors_headers origin
  ) in
  let response = Httpun.Response.create ~headers status in
  Httpun.Reqd.respond_with_string reqd response body


let with_read_auth handler request reqd =
  match !server_state with
  | None -> Http.Response.json {|{"error":"not initialized"}|} reqd
  | Some state ->
      let base_path = state.Mcp_server.room_config.base_path in
      let auth_cfg = Auth.load_auth_config base_path in
      let agent_name_opt = agent_from_request request in
      let token = auth_token_from_request request in
      let agent_name = Option.value ~default:"dashboard" agent_name_opt in
      if auth_cfg.enabled && auth_cfg.require_token && agent_name_opt = None then
        respond_auth_error request reqd (Types.Unauthorized "Agent name required")
      else
        match Auth.check_permission base_path ~agent_name ~token ~permission:Types.CanReadState with
        | Ok () -> handler state request reqd
        | Error err -> respond_auth_error request reqd err

let parse_host_port host_header default_host default_port =
  match host_header with
  | None -> (default_host, default_port)
  | Some host_value ->
      (match String.split_on_char ':' host_value with
       | [host] -> (host, default_port)
       | host :: port_str :: _ ->
           let port = try int_of_string port_str with _ -> default_port in
           (host, port)
       | _ -> (default_host, default_port))

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

(** GraphQL Playground HTML (GET /graphql) *)
let graphql_playground_html ~nonce =
  String.concat "" [
    {|
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="user-scalable=no,initial-scale=1,minimum-scale=1,maximum-scale=1" />
    <title>MASC GraphQL Playground</title>
    <link rel="stylesheet" href="/static/css/middleware.css" />
  </head>
  <body>
    <style>
      html { font-family: "Open Sans", sans-serif; overflow: hidden; }
      body { margin: 0; background: #172a3a; }
      .playgroundIn { animation: playgroundIn .5s ease-out forwards; }
      @keyframes playgroundIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
    </style>
    <style>
      .fadeOut { animation: fadeOut .5s ease-out forwards; }
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(-10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      @keyframes fadeOut {
        from { opacity: 1; transform: translateY(0); }
        to { opacity: 0; transform: translateY(-10px); }
      }
      @keyframes appearIn {
        from { opacity: 0; transform: translateY(0); }
        to { opacity: 1; transform: translateY(0); }
      }
      @keyframes scaleIn {
        from { transform: scale(0); }
        to { transform: scale(1); }
      }
      @keyframes innerDrawIn {
        0% { stroke-dashoffset: 70; }
        50% { stroke-dashoffset: 140; }
        100% { stroke-dashoffset: 210; }
      }
      @keyframes outerDrawIn {
        0% { stroke-dashoffset: 76; }
        100% { stroke-dashoffset: 152; }
      }
      #loading-wrapper {
        position: absolute;
        width: 100vw;
        height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
      }
      .logo {
        width: 75px;
        height: 75px;
        margin-bottom: 20px;
        opacity: 0;
        animation: fadeIn .5s ease-out forwards;
      }
      .text {
        font-size: 32px;
        font-weight: 200;
        text-align: center;
        color: rgba(255, 255, 255, .6);
        opacity: 0;
        animation: fadeIn .5s ease-out forwards;
      }
      .text strong { font-weight: 400; }
    </style>
    <div id="loading-wrapper">
      <svg class="logo" viewBox="0 0 128 128" xmlns:xlink="http://www.w3.org/1999/xlink">
        <title>GraphQL Playground Logo</title>
        <defs>
          <linearGradient id="linearGradient-1" x1="4.86%" x2="96.21%" y1="0%" y2="99.66%">
            <stop stop-color="#E00082" stop-opacity=".8" offset="0%"></stop>
            <stop stop-color="#E00082" offset="100%"></stop>
          </linearGradient>
        </defs>
        <g>
          <rect id="Gradient" width="127.96" height="127.96" y="1" fill="url(#linearGradient-1)" rx="4"></rect>
          <path id="Border" fill="#E00082" fill-rule="nonzero" d="M4.7 2.84c-1.58 0-2.86 1.28-2.86 2.85v116.57c0 1.57 1.28 2.84 2.85 2.84h116.57c1.57 0 2.84-1.26 2.84-2.83V5.67c0-1.55-1.26-2.83-2.83-2.83H4.67zM4.7 0h116.58c3.14 0 5.68 2.55 5.68 5.7v116.58c0 3.14-2.54 5.68-5.68 5.68H4.68c-3.13 0-5.68-2.54-5.68-5.68V5.68C-1 2.56 1.55 0 4.7 0z"></path>
          <path class="bglIGM" x="64" y="28" fill="#fff" d="M64 36c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8"></path>
          <path class="ksxRII" x="95.98500061035156" y="46.510000228881836" fill="#fff" d="M89.04 50.52c-2.2-3.84-.9-8.73 2.94-10.96 3.83-2.2 8.72-.9 10.95 2.94 2.2 3.84.9 8.73-2.94 10.96-3.85 2.2-8.76.9-10.97-2.94"></path>
          <path class="cWrBmb" x="95.97162628173828" y="83.4900016784668" fill="#fff" d="M102.9 87.5c-2.2 3.84-7.1 5.15-10.94 2.94-3.84-2.2-5.14-7.12-2.94-10.96 2.2-3.84 7.12-5.15 10.95-2.94 3.86 2.23 5.16 7.12 2.94 10.96"></path>
          <path class="Wnusb" x="64" y="101.97999572753906" fill="#fff" d="M64 110c-4.43 0-8-3.6-8-8.02 0-4.44 3.57-8.02 8-8.02s8 3.58 8 8.02c0 4.4-3.57 8.02-8 8.02"></path>
          <path class="bfPqf" x="32.03982162475586" y="83.4900016784668" fill="#fff" d="M25.1 87.5c-2.2-3.84-.9-8.73 2.93-10.96 3.83-2.2 8.72-.9 10.95 2.94 2.2 3.84.9 8.73-2.94 10.96-3.85 2.2-8.74.9-10.95-2.94"></path>
          <path class="edRCTN" x="32.033552169799805" y="46.510000228881836" fill="#fff" d="M38.96 50.52c-2.2 3.84-7.12 5.15-10.95 2.94-3.82-2.2-5.12-7.12-2.92-10.96 2.2-3.84 7.12-5.15 10.95-2.94 3.83 2.23 5.14 7.12 2.94 10.96"></path>
          <path class="iEGVWn" stroke="#fff" stroke-width="4" stroke-linecap="round" stroke-linejoin="round" d="M63.55 27.5l32.9 19-32.9-19z"></path>
          <path class="bsocdx" stroke="#fff" stroke-width="4" stroke-linecap="round" stroke-linejoin="round" d="M96 46v38-38z"></path>
          <path class="jAZXmP" stroke="#fff" stroke-width="4" stroke-linecap="round" stroke-linejoin="round" d="M96.45 84.5l-32.9 19 32.9-19z"></path>
          <path class="hSeArx" stroke="#fff" stroke-width="4" stroke-linecap="round" stroke-linejoin="round" d="M64.45 103.5l-32.9-19 32.9 19z"></path>
          <path class="bVgqGk" stroke="#fff" stroke-width="4" stroke-linecap="round" stroke-linejoin="round" d="M32 84V46v38z"></path>
          <path class="hEFqBt" stroke="#fff" stroke-width="4" stroke-linecap="round" stroke-linejoin="round" d="M31.55 46.5l32.9-19-32.9 19z"></path>
          <path class="dzEKCM" id="Triangle-Bottom" stroke="#fff" stroke-width="4" d="M30 84h70" stroke-linecap="round"></path>
          <path class="DYnPx" id="Triangle-Left" stroke="#fff" stroke-width="4" d="M65 26L30 87" stroke-linecap="round"></path>
          <path class="hjPEAQ" id="Triangle-Right" stroke="#fff" stroke-width="4" d="M98 87L63 26" stroke-linecap="round"></path>
        </g>
      </svg>
      <div class="text">Loading <strong>GraphQL Playground</strong></div>
    </div>
    <div id="root"></div>
    <script nonce="|};
    nonce;
    {|">
      window.addEventListener("load", function () {
        var loading = document.getElementById("loading-wrapper");
        if (loading) {
          loading.classList.add("fadeOut");
        }
        var root = document.getElementById("root");
        if (!root) {
          return;
        }
        root.classList.add("playgroundIn");
        GraphQLPlayground.init(root, {
          endpoint: "/graphql",
          settings: { "request.credentials": "same-origin" }
        });
      });
    </script>
    <script src="/static/js/middleware.js"></script>
  </body>
</html>
|};
  ]

let graphql_csp_header nonce =
  Printf.sprintf
    "default-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'; \
     connect-src 'self'; img-src 'self' data:; \
     script-src 'self' 'nonce-%s' 'unsafe-eval'; \
     style-src 'self' 'unsafe-inline'; \
     font-src 'self' data:; \
     worker-src 'self' blob:"
    nonce

(** Resolve assets root *)
let assets_root () =
  let is_dir path =
    Sys.file_exists path && Sys.is_directory path
  in
  let exe_assets =
    let exe_dir = Filename.dirname Sys.executable_name in
    let root = Filename.dirname (Filename.dirname (Filename.dirname exe_dir)) in
    Filename.concat root "assets"
  in
  match Sys.getenv_opt "MASC_ASSETS_DIR" with
  | Some path when is_dir path -> path
  | _ when is_dir (Filename.concat (Sys.getcwd ()) "assets") ->
      Filename.concat (Sys.getcwd ()) "assets"
  | _ when is_dir exe_assets -> exe_assets
  | _ -> Filename.concat (Sys.getcwd ()) "assets"

(** Local GraphiQL assets *)
let graphiql_asset_root () =
  Filename.concat (assets_root ()) "graphiql"

let graphiql_asset_path name =
  Filename.concat (graphiql_asset_root ()) name

let asset_content_type name =
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
      Http.Response.bytes ~content_type:(asset_content_type name) body reqd
  | Error _ ->
      Http.Response.not_found reqd

(** Local GraphQL Playground assets *)
let playground_asset_root () =
  Filename.concat (assets_root ()) "playground"

let playground_asset_path name =
  Filename.concat (playground_asset_root ()) name

let serve_playground_asset name _request reqd =
  let path = playground_asset_path name in
  match read_file path with
  | Ok body ->
      Http.Response.bytes ~content_type:(asset_content_type name) body reqd
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
  let json = Printf.sprintf {|{"status":"ok","server":"masc-mcp","version":"%s"}|} Masc_mcp.Version.version in
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
let current_net : _ Eio.Net.t option ref = ref None

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
  let auth_token = auth_token_from_request request in

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
      let response_json =
        Mcp_eio.handle_request ~clock ~sw ~mcp_session_id:session_id ?auth_token state body_str
      in
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

(** Close all SSE connections gracefully - for shutdown *)
let close_all_sse_connections () =
  let sessions = Hashtbl.fold (fun k _ acc -> k :: acc) sse_conn_by_session [] in
  List.iter (fun session_id ->
    stop_sse_session session_id
  ) sessions;
  Printf.eprintf "🚀 MASC MCP: Closed %d SSE connections\n%!" (List.length sessions)

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
         let is_cancelled exn =
           match exn with
           | Eio.Cancel.Cancelled _ -> true
           | _ -> false
         in
         let rec loop () =
           if not !(info.stop) then begin
             (try
                Eio.Time.sleep clock sse_ping_interval_s
              with exn ->
                if is_cancelled exn then raise exn;
                Printf.eprintf "[SSE] ping sleep error: %s\n%!" (Printexc.to_string exn));
             (try
                if info.closed then
                  stop_sse_session info.session_id
                else if not !(info.stop) then
                  ignore (send_raw info ": ping\n\n")
              with exn ->
                if is_cancelled exn then raise exn;
                Printf.eprintf "[SSE] ping send error: %s\n%!" (Printexc.to_string exn);
                stop_sse_session info.session_id);
             loop ()
           end
         in
         try loop () with exn ->
           if is_cancelled exn then ()
           else Printf.eprintf "[SSE] ping loop error: %s\n%!" (Printexc.to_string exn))
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
      let auth_token = auth_token_from_request request in
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
        let response_json =
          Mcp_eio.handle_request ~clock ~sw ~mcp_session_id:session_id ?auth_token state body_str
        in
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
let make_routes ~port ~host =
  Http.Router.empty
  |> Http.Router.get "/health" health_handler
  |> Http.Router.get "/metrics" (fun request reqd ->
       with_read_auth (fun _state _req reqd ->
         let body = Masc_mcp.Prometheus.to_prometheus_text () in
         Http.Response.bytes ~content_type:"text/plain; version=0.0.4; charset=utf-8" body reqd
       ) request reqd)
  |> Http.Router.get "/.well-known/agent-card.json" (fun request reqd ->
       with_read_auth (fun _state req reqd ->
         let host_header = Httpun.Headers.get req.Httpun.Request.headers "host" in
         let (resolved_host, resolved_port) = parse_host_port host_header host port in
         let card = Masc_mcp.Agent_card.generate_default ~host:resolved_host ~port:resolved_port () in
         let json = Masc_mcp.Agent_card.to_json card |> Yojson.Safe.to_string in
         Http.Response.json json reqd
       ) request reqd)
  |> Http.Router.get "/dashboard" (fun request reqd ->
       with_read_auth (fun _state _req reqd ->
         Http.Response.html (Masc_mcp.Web_dashboard.html ()) reqd
       ) request reqd)
  |> Http.Router.get "/dashboard/credits" (fun request reqd ->
       with_read_auth (fun _state _req reqd ->
         Http.Response.html (Masc_mcp.Credits_dashboard.html ()) reqd
       ) request reqd)
  |> Http.Router.get "/api/v1/credits" (fun request reqd ->
       with_read_auth (fun _state _req reqd ->
         Http.Response.json (Masc_mcp.Credits_dashboard.json_api ()) reqd
       ) request reqd)
  |> Http.Router.get "/" (fun _req reqd -> Http.Response.text "MASC MCP Server" reqd)
  |> Http.Router.get "/static/css/middleware.css"
       (serve_playground_asset "static/css/middleware.css")
  |> Http.Router.get "/static/js/middleware.js"
       (serve_playground_asset "static/js/middleware.js")
  |> Http.Router.get "/graphiql/graphiql.min.css"
       (serve_graphiql_asset "graphiql.min.css")
  |> Http.Router.get "/graphiql/graphiql.min.js"
       (serve_graphiql_asset "graphiql.min.js")
  |> Http.Router.get "/graphiql/react.production.min.js"
       (serve_graphiql_asset "react.production.min.js")
  |> Http.Router.get "/graphiql/react-dom.production.min.js"
       (serve_graphiql_asset "react-dom.production.min.js")
  |> Http.Router.get "/mcp" (fun request reqd ->
       with_read_auth (fun _state req reqd -> handle_get_mcp req reqd) request reqd)
  |> Http.Router.post "/" handle_post_mcp
  |> Http.Router.post "/mcp" handle_post_mcp
  |> Http.Router.add ~path:"/graphql" ~methods:[`GET; `POST]
       ~handler:(fun request reqd ->
         with_read_auth (fun _state req reqd -> handle_graphql req reqd) request reqd)
  |> Http.Router.post "/messages" handle_post_messages
  |> Http.Router.get "/sse"
       (fun request reqd ->
         with_read_auth (fun _state req reqd ->
           handle_get_mcp
             ~legacy_messages_endpoint:(legacy_messages_endpoint_url req)
             req reqd
         ) request reqd)
  |> Http.Router.get "/sse/simple" (fun request reqd ->
       with_read_auth (fun _state req reqd -> sse_simple_handler req reqd) request reqd)
  (* REST API for dashboard - direct Room access *)
  |> Http.Router.get "/api/v1/status" (fun request reqd ->
       with_read_auth (fun state _req reqd ->
         let config = state.Mcp_server.room_config in
         let room_state = Masc_mcp.Room.read_state config in
         let tempo = Masc_mcp.Tempo.get_tempo config in
         let json = `Assoc [
           ("cluster", `String (Option.value ~default:"unknown" (Sys.getenv_opt "MASC_CLUSTER_NAME")));
           ("project", `String room_state.project);
           ("tempo_interval_s", `Float tempo.current_interval_s);
           ("paused", `Bool room_state.paused);
         ] in
         Http.Response.json (Yojson.Safe.to_string json) reqd
       ) request reqd)
  |> Http.Router.get "/api/v1/tasks" (fun request reqd ->
       with_read_auth (fun state req reqd ->
         let config = state.Mcp_server.room_config in
         let status_filter = query_param req "status" in
         let limit = int_query_param req "limit" ~default:50 in
         let offset = int_query_param req "offset" ~default:0 in
         let tasks = Masc_mcp.Room.get_tasks_raw config in
         let filtered =
           match status_filter with
           | None -> tasks
           | Some status ->
               List.filter (fun (t : Masc_mcp.Types.task) ->
                 String.equal status (Masc_mcp.Types.string_of_task_status t.task_status)
               ) tasks
         in
         let total = List.length filtered in
         let page =
           filtered
           |> List.filteri (fun idx _ -> idx >= offset && idx < offset + limit)
         in
         let tasks_json = List.map (fun (t : Masc_mcp.Types.task) ->
           `Assoc [
             ("id", `String t.id);
             ("title", `String t.title);
             ("status", `String (Masc_mcp.Types.string_of_task_status t.task_status));
             ("priority", `Int t.priority);
             ("assignee", match t.task_status with
               | Claimed { assignee; _ } | InProgress { assignee; _ } | Done { assignee; _ } -> `String assignee
               | _ -> `Null);
           ]
         ) page in
         let json = `Assoc [
           ("tasks", `List tasks_json);
           ("limit", `Int limit);
           ("offset", `Int offset);
           ("total", `Int total);
         ] in
         Http.Response.json (Yojson.Safe.to_string json) reqd
       ) request reqd)
  |> Http.Router.get "/api/v1/agents" (fun request reqd ->
       with_read_auth (fun state req reqd ->
         let config = state.Mcp_server.room_config in
         let status_filter = query_param req "status" in
         let limit = int_query_param req "limit" ~default:50 in
         let offset = int_query_param req "offset" ~default:0 in
         let agents = Masc_mcp.Room.get_agents_raw config in
         let filtered =
           match status_filter with
           | None -> agents
           | Some status ->
               List.filter (fun (a : Masc_mcp.Types.agent) ->
                 String.equal status (Masc_mcp.Types.string_of_agent_status a.status)
               ) agents
         in
         let total = List.length filtered in
         let page =
           filtered
           |> List.filteri (fun idx _ -> idx >= offset && idx < offset + limit)
         in
         let agents_json = List.map (fun (a : Masc_mcp.Types.agent) ->
           `Assoc [
             ("name", `String a.name);
             ("status", `String (Masc_mcp.Types.string_of_agent_status a.status));
             ("current_task", match a.current_task with Some t -> `String t | None -> `Null);
           ]
         ) page in
         let json = `Assoc [
           ("agents", `List agents_json);
           ("limit", `Int limit);
           ("offset", `Int offset);
           ("total", `Int total);
         ] in
         Http.Response.json (Yojson.Safe.to_string json) reqd
       ) request reqd)
  |> Http.Router.get "/api/v1/messages" (fun request reqd ->
       with_read_auth (fun state req reqd ->
         let config = state.Mcp_server.room_config in
         let since_seq = int_query_param req "since_seq" ~default:0 in
         let limit = int_query_param req "limit" ~default:20 in
         let agent_filter = query_param req "agent" in
         let msgs = Masc_mcp.Room.get_messages_raw config ~since_seq ~limit:500 in
         let filtered =
           match agent_filter with
           | None -> msgs
           | Some agent ->
               List.filter (fun (m : Masc_mcp.Types.message) ->
                 String.equal agent m.from_agent
               ) msgs
         in
         let total = List.length filtered in
         let page = filtered |> List.filteri (fun idx _ -> idx < limit) in
         let msgs_json = List.map (fun (m : Masc_mcp.Types.message) ->
           `Assoc [
             ("from", `String m.from_agent);
             ("content", `String m.content);
             ("timestamp", `String m.timestamp);
             ("seq", `Int m.seq);
           ]
         ) page in
         let json = `Assoc [
           ("messages", `List msgs_json);
           ("limit", `Int limit);
           ("since_seq", `Int since_seq);
           ("total", `Int total);
         ] in
         Http.Response.json (Yojson.Safe.to_string json) reqd
       ) request reqd)
  |> Http.Router.post "/api/v1/broadcast" (fun request reqd ->
       (* POST /api/v1/broadcast - HTTP API for external tools like autocov *)
       with_read_auth (fun state _req reqd ->
         Http.Request.read_body_async reqd (fun body_str ->
           try
             let json = Yojson.Safe.from_string body_str in
             let agent_name = json |> Yojson.Safe.Util.member "agent_name" |> Yojson.Safe.Util.to_string in
             let message = json |> Yojson.Safe.Util.member "message" |> Yojson.Safe.Util.to_string in
             let config = state.Mcp_server.room_config in
             let _ = Masc_mcp.Room.broadcast config ~from_agent:agent_name ~content:message in
             Http.Response.json {|{"ok":true}|} reqd
           with e ->
             Http.Response.json
               (Printf.sprintf {|{"ok":false,"error":"%s"}|} (Printexc.to_string e))
               reqd
         )
       ) request reqd)
  |> Http.Router.post "/broadcast" (fun request reqd ->
       (* POST /broadcast - Alias for autocov compatibility *)
       with_read_auth (fun state _req reqd ->
         Http.Request.read_body_async reqd (fun body_str ->
           try
             let json = Yojson.Safe.from_string body_str in
             let agent_name = json |> Yojson.Safe.Util.member "agent_name" |> Yojson.Safe.Util.to_string in
             let message = json |> Yojson.Safe.Util.member "message" |> Yojson.Safe.Util.to_string in
             let config = state.Mcp_server.room_config in
             let _ = Masc_mcp.Room.broadcast config ~from_agent:agent_name ~content:message in
             Http.Response.json {|{"ok":true}|} reqd
           with e ->
             Http.Response.json
               (Printf.sprintf {|{"ok":false,"error":"%s"}|} (Printexc.to_string e))
               reqd
         )
       ) request reqd)

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
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in

  (* Store switch, clock, and net references for handlers *)
  current_sw := Some sw;
  current_clock := Some clock;
  current_net := Some net;

  (* Set net reference in Mcp_eio for Walph chain execution *)
  Mcp_eio.set_net net;

  (* Create Caqti-compatible stdenv adapter
     Note: net type coercion from [Generic|Unix] to [Generic] is safe
     because Caqti only uses the generic network capabilities *)
  let caqti_env : Caqti_eio.stdenv = object
    method net = (net :> [`Generic] Eio.Net.ty Eio.Resource.t)
    method clock = clock
    method mono_clock = mono_clock
  end in

  (* Initialize server state with Eio context *)
  let state = Mcp_eio.create_state_eio ~sw ~env:caqti_env ~proc_mgr ~fs ~clock ~base_path in
  server_state := Some state;
  Mcp_server.set_sse_callback state Sse.broadcast;
  Progress.set_sse_callback Sse.broadcast;
  Masc_mcp.Orchestrator.start ~sw ~proc_mgr ~clock ~domain_mgr state.room_config;

  let config = { Http.default_config with port; host = "127.0.0.1" } in
  let routes = make_routes ~port:config.port ~host:config.host in
  let request_handler = make_extended_handler routes in

  let ip = Eio.Net.Ipaddr.V4.loopback in
  let addr = `Tcp (ip, config.port) in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:config.max_connections addr in

  let resolved_base = state.room_config.base_path in
  let masc_dir = Filename.concat resolved_base ".masc" in

  (* Initialize A2A subscription persistence *)
  Masc_mcp.A2a_tools.init ~masc_dir;

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

  let rec accept_loop backoff_s =
    try
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
      accept_loop 0.05
    with exn ->
      Printf.eprintf "Accept error: %s\n%!" (Printexc.to_string exn);
      (try Eio.Time.sleep clock backoff_s with _ -> ());
      let next_backoff = Float.min 2.0 (backoff_s *. 1.5) in
      accept_loop next_backoff
  in
  accept_loop 0.05

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
  (* Initialize Mirage_crypto RNG - MUST be inside Eio_main.run for thread-local state *)
  Mirage_crypto_rng_unix.use_default ();

  (* Initialize thread-safe token store for cancellation support *)
  Masc_mcp.Cancellation.TokenStore.init ();

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

      (* Give clients 200ms to receive the notification *)
      Unix.sleepf 0.2;

      (* Gracefully close all SSE connections before Switch.fail *)
      close_all_sse_connections ();

      (* Give connections 200ms to complete close handshake *)
      Unix.sleepf 0.2;

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
  let info = Cmd.info "masc-mcp" ~version:Masc_mcp.Version.version ~doc in
  Cmd.v info Term.(const run_cmd $ port $ base_path)

let () = exit (Cmd.eval cmd)
