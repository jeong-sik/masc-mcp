(** MASC Voice Bridge - Eio-native Implementation

    Enables multi-agent voice collaboration via turn-based speaking.
    Core constraint: "병렬 수집 → 순차 출력" (parallel collection → sequential output)

    Voice MCP Server: http://127.0.0.1:8936/mcp

    Eio Migration Notes:
    - Direct style (no monads)
    - Cohttp_eio.Client for HTTP
    - Eio.Time.sleep for delays
    - Eio.Fiber.first for timeouts
*)

(** ============================================
    Configuration (Externalized)
    ============================================ *)

(** Voice Bridge configuration type *)
type voice_bridge_config = {
  host: string;
  port: int;
  timeout_seconds: float;
  max_retries: int;
  initial_backoff_seconds: float;
  backoff_multiplier: float;
  agent_voices: (string * string) list;
}

(** Default configuration values *)
let default_config = {
  host = "127.0.0.1";
  port = 8936;
  timeout_seconds = 5.0;
  max_retries = 3;
  initial_backoff_seconds = 1.0;
  backoff_multiplier = 2.0;
  agent_voices = [
    ("claude", "Sarah");      (* BALTHASAR - 차분한 여성 *)
    ("gemini", "Roger");      (* CASPER - 깊은 남성 *)
    ("codex", "George");      (* MELCHIOR - 명확한 남성 *)
    ("ollama", "Laura");      (* ARTABAN - 따뜻한 여성 *)
  ];
}

(** Load configuration from $ME_ROOT/.masc/voice_config.json *)
let load_config () =
  let config_path =
    match Sys.getenv_opt "ME_ROOT" with
    | Some root -> Filename.concat root ".masc/voice_config.json"
    | None -> ".masc/voice_config.json"
  in
  if Sys.file_exists config_path then
    try
      let json = Yojson.Safe.from_file config_path in
      let open Yojson.Safe.Util in
      let get_float key default =
        try json |> member "server" |> member key |> to_float
        with _ -> try json |> member "retry" |> member key |> to_float
        with _ -> default
      in
      let get_int key default =
        try json |> member "server" |> member key |> to_int
        with _ -> try json |> member "retry" |> member key |> to_int
        with _ -> default
      in
      let voices =
        try json |> member "agent_voices" |> to_assoc
            |> List.map (fun (agent, voice) -> (agent, to_string voice))
        with _ -> default_config.agent_voices
      in
      {
        host = (try json |> member "server" |> member "host" |> to_string
                with _ -> default_config.host);
        port = get_int "port" default_config.port;
        timeout_seconds = get_float "timeout_seconds" default_config.timeout_seconds;
        max_retries = get_int "max_retries" default_config.max_retries;
        initial_backoff_seconds = get_float "initial_backoff_seconds" default_config.initial_backoff_seconds;
        backoff_multiplier = get_float "backoff_multiplier" default_config.backoff_multiplier;
        agent_voices = voices;
      }
    with e ->
      Printf.eprintf "[VoiceBridge] Config parse failed: %s, using defaults\n%!"
        (Printexc.to_string e);
      default_config
  else
    default_config

(** Cached configuration (lazy-loaded once) *)
let config = lazy (load_config ())

(** Configuration accessors *)
let voice_mcp_host () = (Lazy.force config).host
let voice_mcp_port () = (Lazy.force config).port
let request_timeout_seconds () = (Lazy.force config).timeout_seconds
let max_retries () = (Lazy.force config).max_retries
let initial_backoff_seconds () = (Lazy.force config).initial_backoff_seconds
let backoff_multiplier () = (Lazy.force config).backoff_multiplier
let agent_voices () = (Lazy.force config).agent_voices

(** ============================================
    Structured Logging
    ============================================ *)

let log_prefix = "[VoiceBridge]"

let log_info msg =
  Log.info "%s %s" log_prefix msg

let log_error msg =
  Log.error "%s %s" log_prefix msg

let log_debug msg =
  Log.debug "%s %s" log_prefix msg

(** Get voice for agent, defaults to "Sarah" *)
let get_voice_for_agent agent_id =
  let voices = (Lazy.force config).agent_voices in
  match List.assoc_opt agent_id voices with
  | Some voice -> voice
  | None -> "Sarah"

(** ============================================
    Types
    ============================================ *)

(** Voice session status *)
type voice_session_status = {
  session_id: string;
  agent_id: string;
  voice: string;
  is_active: bool;
  turn_count: int;
  duration_seconds: float option;
}

(** Conference status *)
type conference_status = {
  conference_id: string;
  state: string;  (* idle, active, paused, ended *)
  participants: string list;
  current_speaker: string option;
  queue_size: int;
  turn_count: int;
}

(** Turn request result *)
type turn_request_result = {
  status: string;
  agent_id: string;
  message_preview: string;
  voice: string;
  queue_position: int;
}

(** ============================================
    HTTP Client with Timeout and Retry (Eio-native)
    ============================================ *)

(** Timeout exception for Eio.Fiber.first pattern *)
exception Timeout of string

(** Safe string prefix check *)
let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

(** Check if an error is retryable (transient)
    Retryable errors: connection failures, timeouts, HTTP 5xx *)
let is_retryable_error error =
  if String.length error = 0 then false
  else
    let s = String.lowercase_ascii error in
    starts_with ~prefix:"connection" s ||
    starts_with ~prefix:"timeout" s ||
    (try
      Scanf.sscanf s "http %d" (fun code -> code >= 500 && code < 600)
    with _ -> false)

(** Timeout helper using Eio.Fiber.first - returns Error after specified seconds *)
let with_timeout ~clock ?timeout operation =
  let timeout_sec = match timeout with Some t -> t | None -> request_timeout_seconds () in
  Eio.Fiber.first
    (fun () -> operation ())
    (fun () ->
      Eio.Time.sleep clock timeout_sec;
      Error (Printf.sprintf "Request timeout after %.1fs" timeout_sec))

(** Retry with exponential backoff - Eio version *)
let rec retry_with_backoff ~clock ~attempt ~max_attempts ~backoff_sec operation =
  let result = operation () in
  match result with
  | Ok _ as success -> success
  | Error e when attempt < max_attempts && is_retryable_error e ->
    log_info (Printf.sprintf "Retry %d/%d after %.1fs (error: %s)"
      attempt max_attempts backoff_sec e);
    Eio.Time.sleep clock backoff_sec;
    retry_with_backoff ~clock
      ~attempt:(attempt + 1)
      ~max_attempts
      ~backoff_sec:(backoff_sec *. backoff_multiplier ())
      operation
  | Error _ as failure ->
    log_error (Printf.sprintf "All %d retries exhausted" max_attempts);
    failure

(** Make single HTTP POST request to Voice MCP server - Eio version *)
let single_voice_mcp_call ~sw ~client ~uri ~headers ~body_str =
  try
    let body = Cohttp_eio.Body.of_string body_str in
    let resp, resp_body = Cohttp_eio.Client.post ~sw ~body ~headers client uri in
    let status = Cohttp.Response.status resp in
    (* Read body using Eio.Buf_read *)
    let body_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:max_int in
    if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
      Ok (Yojson.Safe.from_string body_str)
    else
      Error (Printf.sprintf "HTTP %d: %s"
        (Cohttp.Code.code_of_status status) body_str)
  with exn ->
    Error (Printf.sprintf "Connection error: %s" (Printexc.to_string exn))

(** Make HTTP POST request to Voice MCP server with timeout and retry - Eio version *)
let call_voice_mcp ~sw ~clock ~net ~tool_name ~arguments =
  log_debug (Printf.sprintf "Calling tool: %s" tool_name);
  let uri = Uri.make
    ~scheme:"http"
    ~host:(voice_mcp_host ())
    ~port:(voice_mcp_port ())
    ~path:"/mcp"
    ()
  in
  let request_body = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "tools/call");
    ("id", `Int 1);
    ("params", `Assoc [
      ("name", `String tool_name);
      ("arguments", arguments);
    ]);
  ] in
  let body_str = Yojson.Safe.to_string request_body in
  let headers = Cohttp.Header.of_list [
    ("Content-Type", "application/json");
    ("Accept", "application/json");
  ] in
  let client = Cohttp_eio.Client.make ~https:None net in
  let operation () =
    with_timeout ~clock (fun () ->
      single_voice_mcp_call ~sw ~client ~uri ~headers ~body_str)
  in
  let result = retry_with_backoff ~clock
    ~attempt:1
    ~max_attempts:(max_retries ())
    ~backoff_sec:(initial_backoff_seconds ())
    operation
  in
  (match result with
  | Ok _ -> log_debug (Printf.sprintf "Tool %s succeeded" tool_name)
  | Error e -> log_error (Printf.sprintf "Tool %s failed: %s" tool_name e));
  result

(** Extract result from MCP response *)
let extract_mcp_result json =
  let open Yojson.Safe.Util in
  try
    let result = json |> member "result" in
    if result = `Null then
      let error = json |> member "error" |> member "message" |> to_string_option in
      Error (Option.value error ~default:"Unknown error")
    else
      (* Get content from result *)
      let content = result |> member "content" |> to_list in
      match content with
      | [] -> Ok (`Assoc [])
      | first :: _ ->
        let text = first |> member "text" |> to_string_option in
        (match text with
        | Some t ->
          (try Ok (Yojson.Safe.from_string t)
           with _ -> Ok (`String t))
        | None -> Ok result)
  with e ->
    Error (Printf.sprintf "Parse error: %s" (Printexc.to_string e))

(** ============================================
    Fallback Strategies
    ============================================ *)

(** Check if Voice MCP server is available (non-blocking, cached)
    Circuit Breaker pattern - shorter cache on failure for faster recovery *)
let voice_server_available = ref None
let voice_server_check_time = ref 0.0

(** Cache duration: 30s on success, 5s on failure (Circuit Breaker) *)
let cache_duration () =
  match !voice_server_available with
  | Some true -> 30.0   (* Success: cache longer *)
  | Some false -> 5.0   (* Failure: retry sooner *)
  | None -> 0.0         (* No cache: check immediately *)

let is_voice_server_available ~sw ~clock ~net =
  let now = Unix.gettimeofday () in
  if now -. !voice_server_check_time < cache_duration () then
    Option.value !voice_server_available ~default:false
  else begin
    voice_server_check_time := now;
    let check () =
      try
        let uri = Uri.make
          ~scheme:"http"
          ~host:(voice_mcp_host ())
          ~port:(voice_mcp_port ())
          ~path:"/health"
          ()
        in
        let client = Cohttp_eio.Client.make ~https:None net in
        let resp, _ = Cohttp_eio.Client.get ~sw client uri in
        let status = Cohttp.Response.status resp in
        let available = Cohttp.Code.is_success (Cohttp.Code.code_of_status status) in
        voice_server_available := Some available;
        Ok available
      with _ ->
        voice_server_available := Some false;
        Ok false
    in
    with_timeout ~clock ~timeout:2.0 check |> function
      | Ok available -> available
      | Error _ ->
        voice_server_available := Some false;
        false
  end

(** Text-only fallback for when voice is unavailable *)
let text_fallback ~agent_id ~message =
  let voice = get_voice_for_agent agent_id in
  log_info (Printf.sprintf "[Text fallback] %s (%s): %s"
    agent_id voice
    (String.sub message 0 (min 50 (String.length message))));
  Ok (`Assoc [
    ("status", `String "text_fallback");
    ("agent_id", `String agent_id);
    ("voice", `String voice);
    ("message_preview", `String (String.sub message 0 (min 50 (String.length message))));
  ])

(** ============================================
    Voice Session Management (Eio-native)
    ============================================ *)

(** Start a voice session for an agent *)
let start_voice_session ~sw ~clock ~net ~agent_id ?session_name () =
  if not (is_voice_server_available ~sw ~clock ~net) then
    Error "Voice MCP server unavailable"
  else
    let voice = get_voice_for_agent agent_id in
    let args = `Assoc [
      ("agent_id", `String agent_id);
      ("voice", `String voice);
      ("session_name", match session_name with Some n -> `String n | None -> `Null);
    ] in
    match call_voice_mcp ~sw ~clock ~net ~tool_name:"voice_session_start" ~arguments:args with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data ->
        let open Yojson.Safe.Util in
        let session_id = data |> member "session_id" |> to_string_option |> Option.value ~default:"unknown" in
        Ok (`Assoc [
          ("session_id", `String session_id);
          ("agent_id", `String agent_id);
          ("voice", `String voice);
          ("is_active", `Bool true);
          ("turn_count", `Int 0);
          ("duration_seconds", `Null);
        ])
      | Error e -> Error e)
    | Error e -> Error e

(** End a voice session *)
let end_voice_session ~sw ~clock ~net ~agent_id =
  if not (is_voice_server_available ~sw ~clock ~net) then
    Ok (`Assoc [("status", `String "skipped"); ("reason", `String "Voice server unavailable")])
  else
    let args = `Assoc [("agent_id", `String agent_id)] in
    match call_voice_mcp ~sw ~clock ~net ~tool_name:"voice_session_end" ~arguments:args with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data -> Ok data
      | Error e -> Error e)
    | Error e -> Error e

(** Request speaking turn *)
let agent_speak ~sw ~clock ~net ~agent_id ~message ?(priority=1) () =
  if not (is_voice_server_available ~sw ~clock ~net) then begin
    log_info "Voice server unavailable, using text fallback";
    text_fallback ~agent_id ~message
  end else begin
    let voice = get_voice_for_agent agent_id in
    let args = `Assoc [
      ("agent_id", `String agent_id);
      ("message", `String message);
      ("voice", `String voice);
      ("priority", `Int priority);
    ] in
    match call_voice_mcp ~sw ~clock ~net ~tool_name:"agent_speak" ~arguments:args with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data ->
        let open Yojson.Safe.Util in
        let status = data |> member "status" |> to_string_option |> Option.value ~default:"queued" in
        let queue_pos = data |> member "queue_position" |> to_int_option |> Option.value ~default:0 in
        Ok (`Assoc [
          ("status", `String status);
          ("agent_id", `String agent_id);
          ("message_preview", `String (String.sub message 0 (min 50 (String.length message))));
          ("voice", `String voice);
          ("queue_position", `Int queue_pos);
        ])
      | Error e -> Error e)
    | Error e -> Error e
  end

(** List active voice sessions *)
let list_voice_sessions ~sw ~clock ~net =
  if not (is_voice_server_available ~sw ~clock ~net) then
    Ok (`Assoc [("sessions", `List []); ("status", `String "voice_server_unavailable")])
  else
    let args = `Assoc [] in
    match call_voice_mcp ~sw ~clock ~net ~tool_name:"voice_session_list" ~arguments:args with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data -> Ok data
      | Error e -> Error e)
    | Error e -> Error e

(** Get voice configuration for an agent *)
let get_agent_voice ~agent_id =
  let voice = get_voice_for_agent agent_id in
  Ok (`Assoc [
    ("agent_id", `String agent_id);
    ("voice", `String voice);
    ("available_voices", `List (List.map (fun (_, v) -> `String v) (agent_voices ())));
  ])

(** ============================================
    Conference Management (Eio-native)
    ============================================ *)

(** Start a multi-agent voice conference *)
let start_conference ~sw ~clock ~net ~agent_ids ?conference_name () =
  if not (is_voice_server_available ~sw ~clock ~net) then
    Error "Voice MCP server unavailable - cannot start conference"
  else
    let agent_voices_list = List.map (fun id ->
      `Assoc [
        ("agent_id", `String id);
        ("voice", `String (get_voice_for_agent id));
      ]
    ) agent_ids in
    let args = `Assoc [
      ("agent_ids", `List (List.map (fun id -> `String id) agent_ids));
      ("agent_voices", `List agent_voices_list);
      ("conference_name", match conference_name with Some n -> `String n | None -> `Null);
    ] in
    match call_voice_mcp ~sw ~clock ~net ~tool_name:"conference_start" ~arguments:args with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data ->
        let open Yojson.Safe.Util in
        let conference_id = data |> member "conference_id" |> to_string_option |> Option.value ~default:"unknown" in
        Ok (`Assoc [
          ("conference_id", `String conference_id);
          ("state", `String "active");
          ("participants", `List (List.map (fun id -> `String id) agent_ids));
          ("current_speaker", `Null);
          ("queue_size", `Int 0);
          ("turn_count", `Int 0);
        ])
      | Error e -> Error e)
    | Error e -> Error e

(** End a multi-agent voice conference *)
let end_conference ~sw ~clock ~net ~agent_ids () =
  let results = List.map (fun agent_id ->
    end_voice_session ~sw ~clock ~net ~agent_id
  ) agent_ids in
  Ok (`Assoc [
    ("ended", `Int (List.length (List.filter Result.is_ok results)));
    ("total", `Int (List.length agent_ids));
  ])

(** Get transcript of voice conversation *)
let get_transcript ~sw ~clock ~net () =
  if not (is_voice_server_available ~sw ~clock ~net) then
    Ok (`Assoc [("transcript", `List []); ("turn_count", `Int 0)])
  else
    let args = `Assoc [] in
    match call_voice_mcp ~sw ~clock ~net ~tool_name:"get_transcript" ~arguments:args with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data -> Ok data
      | Error _ -> Ok (`Assoc [("transcript", `List []); ("turn_count", `Int 0)]))
    | Error _ -> Ok (`Assoc [("transcript", `List []); ("turn_count", `Int 0)])

(** ============================================
    Health Check (Eio-native)
    ============================================ *)

(** Health check for Voice MCP server *)
let health_check ~sw ~clock:_ ~net () =
  let uri = Uri.make
    ~scheme:"http"
    ~host:(voice_mcp_host ())
    ~port:(voice_mcp_port ())
    ~path:"/health"
    ()
  in
  try
    let client = Cohttp_eio.Client.make ~https:None net in
    let resp, body = Cohttp_eio.Client.get ~sw client uri in
    let status = Cohttp.Response.status resp in
    let body_str = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
      Ok (`Assoc [
        ("status", `String "healthy");
        ("server", `String (Printf.sprintf "%s:%d" (voice_mcp_host ()) (voice_mcp_port ())));
        ("response", (try Yojson.Safe.from_string body_str with _ -> `String body_str));
      ])
    else
      Error (Printf.sprintf "Unhealthy: HTTP %d" (Cohttp.Code.code_of_status status))
  with exn ->
    Error (Printf.sprintf "Not reachable: %s" (Printexc.to_string exn))
