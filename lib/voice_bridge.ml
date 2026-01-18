(** MASC Voice Bridge - ElevenLabs Conversational AI Integration

    Enables multi-agent voice collaboration via turn-based speaking.
    Core constraint: "병렬 수집 → 순차 출력" (parallel collection → sequential output)

    Voice MCP Server: http://127.0.0.1:8936/mcp
*)

open Lwt.Syntax

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
      (* P0 Fix: Log config parse errors instead of silent fallback *)
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
    HTTP Client with Timeout and Retry
    ============================================ *)

(** Timeout helper - returns Error after specified seconds *)
let with_timeout ?timeout promise =
  let timeout = match timeout with Some t -> t | None -> request_timeout_seconds () in
  let timeout_promise =
    let* () = Lwt_unix.sleep timeout in
    Lwt.return_error (Printf.sprintf "Request timeout after %.1fs" timeout)
  in
  Lwt.pick [promise; timeout_promise]

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

(** Retry with exponential backoff *)
let rec retry_with_backoff ~attempt ~max_attempts ~backoff_sec operation =
  let* result = operation () in
  match result with
  | Ok _ as success -> Lwt.return success
  | Error e when attempt < max_attempts && is_retryable_error e ->
    log_info (Printf.sprintf "Retry %d/%d after %.1fs (error: %s)"
      attempt max_attempts backoff_sec e);
    let* () = Lwt_unix.sleep backoff_sec in
    retry_with_backoff
      ~attempt:(attempt + 1)
      ~max_attempts
      ~backoff_sec:(backoff_sec *. backoff_multiplier ())
      operation
  | Error _ as failure ->
    log_error (Printf.sprintf "All %d retries exhausted" max_attempts);
    Lwt.return failure

(** Make single HTTP POST request to Voice MCP server *)
let single_voice_mcp_call ~uri ~headers ~body_str =
  let body = Cohttp_lwt.Body.of_string body_str in
  Lwt.catch
    (fun () ->
      let* (resp, resp_body) = Cohttp_lwt_unix.Client.post ~headers ~body uri in
      let status = Cohttp.Response.status resp in
      let* body_str = Cohttp_lwt.Body.to_string resp_body in
      if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
        Lwt.return_ok (Yojson.Safe.from_string body_str)
      else
        Lwt.return_error (Printf.sprintf "HTTP %d: %s"
          (Cohttp.Code.code_of_status status) body_str))
    (fun exn ->
      Lwt.return_error (Printf.sprintf "Connection error: %s" (Printexc.to_string exn)))

(** Make HTTP POST request to Voice MCP server with timeout and retry *)
let call_voice_mcp ~tool_name ~arguments =
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
  let operation () =
    with_timeout (single_voice_mcp_call ~uri ~headers ~body_str)
  in
  let* result = retry_with_backoff
    ~attempt:1
    ~max_attempts:(max_retries ())
    ~backoff_sec:(initial_backoff_seconds ())
    operation
  in
  (match result with
  | Ok _ -> log_debug (Printf.sprintf "Tool %s succeeded" tool_name)
  | Error e -> log_error (Printf.sprintf "Tool %s failed: %s" tool_name e));
  Lwt.return result

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
    P1 Fix: Circuit Breaker pattern - shorter cache on failure for faster recovery *)
let voice_server_available = ref None
let voice_server_check_time = ref 0.0

(** Cache duration: 30s on success, 5s on failure (Circuit Breaker) *)
let cache_duration () =
  match !voice_server_available with
  | Some true -> 30.0   (* Success: cache longer *)
  | Some false -> 5.0   (* Failure: retry sooner *)
  | None -> 0.0         (* No cache: check immediately *)

let is_voice_server_available () =
  let now = Unix.gettimeofday () in
  (* P1 Fix: Adaptive cache duration based on last result *)
  if now -. !voice_server_check_time < cache_duration () then
    Lwt.return (Option.value !voice_server_available ~default:false)
  else begin
    voice_server_check_time := now;
    let uri = Uri.make
      ~scheme:"http"
      ~host:(voice_mcp_host ())
      ~port:(voice_mcp_port ())
      ~path:"/health"
      ()
    in
    let check =
      Lwt.catch
        (fun () ->
          let* (resp, _) = Cohttp_lwt_unix.Client.get uri in
          let available = Cohttp.Code.is_success
            (Cohttp.Code.code_of_status (Cohttp.Response.status resp)) in
          voice_server_available := Some available;
          Lwt.return_ok available)
        (fun _ ->
          voice_server_available := Some false;
          Lwt.return_ok false)
    in
    with_timeout ~timeout:2.0 check |> Lwt.map (function
      | Ok v -> v
      | Error _ ->
        voice_server_available := Some false;
        false)
  end

(** Text-only fallback response when voice is unavailable *)
let text_fallback ~agent_id ~message =
  `Assoc [
    ("status", `String "text_fallback");
    ("agent_id", `String agent_id);
    ("message", `String message);
    ("reason", `String "Voice MCP server unavailable");
  ]

(** ============================================
    Voice Session Functions
    ============================================ *)

(** Start voice session for a MASC agent *)
let start_voice_session ~agent_id ?session_name () =
  let* available = is_voice_server_available () in
  if not available then
    Lwt.return_error "Voice MCP server unavailable"
  else begin
    let voice = get_voice_for_agent agent_id in
    let args = `Assoc [
      ("masc_agent_id", `String agent_id);
      ("voice", `String voice);
      ("session_name", match session_name with
        | Some n -> `String n
        | None -> `String (Printf.sprintf "MASC-%s" agent_id));
    ] in
    let* result = call_voice_mcp ~tool_name:"elevenlabs_agent_start_session" ~arguments:args in
    match result with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data ->
        let open Yojson.Safe.Util in
        let session_id = data |> member "session_id" |> to_string_option |> Option.value ~default:"" in
        Lwt.return_ok {
          session_id;
          agent_id;
          voice;
          is_active = true;
          turn_count = 0;
          duration_seconds = None;
        }
      | Error e -> Lwt.return_error e)
    | Error e -> Lwt.return_error e
  end

(** End voice session for a MASC agent *)
let end_voice_session ~agent_id =
  let* available = is_voice_server_available () in
  if not available then
    Lwt.return_ok (`Assoc [("status", `String "skipped"); ("reason", `String "Voice server unavailable")])
  else begin
    let args = `Assoc [("masc_agent_id", `String agent_id)] in
    let* result = call_voice_mcp ~tool_name:"elevenlabs_agent_end_session" ~arguments:args in
    match result with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data -> Lwt.return_ok data
      | Error e -> Lwt.return_error e)
    | Error e -> Lwt.return_error e
  end

(** Have an agent speak (request turn in queue) - with fallback *)
let agent_speak ~agent_id ~message ?(priority=1) () =
  let* available = is_voice_server_available () in
  if not available then
    (* Graceful degradation: return text fallback *)
    Lwt.return_ok {
      status = "text_fallback";
      agent_id;
      message_preview = message;
      voice = "none";
      queue_position = 0;
    }
  else begin
    let voice = get_voice_for_agent agent_id in
    let args = `Assoc [
      ("agent_id", `String agent_id);
      ("message", `String message);
      ("voice", `String voice);
      ("priority", `Int priority);
    ] in
    let* result = call_voice_mcp ~tool_name:"elevenlabs_agent_speak" ~arguments:args in
    match result with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data ->
        let open Yojson.Safe.Util in
        let status = data |> member "status" |> to_string_option |> Option.value ~default:"unknown" in
        let msg_preview = data |> member "message" |> to_string_option |> Option.value ~default:"" in
        let queue_pos = data |> member "queue_position" |> to_int_option |> Option.value ~default:0 in
        Lwt.return_ok {
          status;
          agent_id;
          message_preview = msg_preview;
          voice;
          queue_position = queue_pos;
        }
      | Error e -> Lwt.return_error e)
    | Error e -> Lwt.return_error e
  end

(** List all active agent voice sessions *)
let list_voice_sessions () =
  let* available = is_voice_server_available () in
  if not available then
    Lwt.return_ok (`Assoc [("sessions", `List []); ("status", `String "voice_server_unavailable")])
  else begin
    let args = `Assoc [] in
    let* result = call_voice_mcp ~tool_name:"elevenlabs_list_agent_sessions" ~arguments:args in
    match result with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data -> Lwt.return_ok data
      | Error e -> Lwt.return_error e)
    | Error e -> Lwt.return_error e
  end

(** Get voice assignment for an agent *)
let get_agent_voice ~agent_id =
  let voice = get_voice_for_agent agent_id in
  let voices = agent_voices () in
  Lwt.return_ok (`Assoc [
    ("agent_id", `String agent_id);
    ("voice", `String voice);
    ("voices_available", `List (List.map (fun (a, v) ->
      `Assoc [("agent", `String a); ("voice", `String v)]
    ) voices));
    ("config_source", `String (
      (* P0 Fix: Use structural equality (=) instead of physical equality (==) *)
      if voices = default_config.agent_voices then "default" else "file"
    ));
  ])

(** ============================================
    Conference Functions
    ============================================ *)

(** Start a voice conference with multiple agents *)
let start_conference ~agent_ids ?conference_name () =
  let* available = is_voice_server_available () in
  if not available then
    Lwt.return_error "Voice MCP server unavailable - cannot start conference"
  else begin
    let conf_name = match conference_name with
      | Some n -> n
      | None -> Printf.sprintf "MASC-Conference-%d" (int_of_float (Unix.gettimeofday ()))
    in
    (* Call the voice MCP to start conference *)
    let args = `Assoc [
      ("agent_ids", `List (List.map (fun a -> `String a) agent_ids));
      ("conference_name", `String conf_name);
      ("voices", `Assoc (List.map (fun a -> (a, `String (get_voice_for_agent a))) agent_ids));
    ] in
    let* result = call_voice_mcp ~tool_name:"elevenlabs_start_conference" ~arguments:args in
    match result with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data ->
        let open Yojson.Safe.Util in
        let conf_id = data |> member "conference_id" |> to_string_option
          |> Option.value ~default:(Printf.sprintf "conf-%f" (Unix.gettimeofday ())) in
        Lwt.return_ok {
          conference_id = conf_id;
          state = "active";
          participants = agent_ids;
          current_speaker = None;
          queue_size = 0;
          turn_count = 0;
        }
      | Error _ ->
        (* Fallback: start individual sessions if conference endpoint not available *)
        let* sessions = Lwt_list.map_s (fun agent_id ->
          start_voice_session ~agent_id ~session_name:conf_name ()
        ) agent_ids in
        let successful = List.filter_map (function Ok s -> Some s | Error _ -> None) sessions in
        let conf_id = Printf.sprintf "conf-fallback-%f" (Unix.gettimeofday ()) in
        Lwt.return_ok {
          conference_id = conf_id;
          state = "active";
          participants = List.map (fun (s : voice_session_status) -> s.agent_id) successful;
          current_speaker = None;
          queue_size = 0;
          turn_count = 0;
        })
    | Error e ->
      (* Fallback: start individual sessions *)
      let* _ = Lwt_list.map_s (fun agent_id ->
        start_voice_session ~agent_id ()
      ) agent_ids in
      let conf_id = Printf.sprintf "conf-fallback-%f" (Unix.gettimeofday ()) in
      Lwt.return_ok {
        conference_id = conf_id;
        state = Printf.sprintf "active (fallback: %s)" e;
        participants = agent_ids;
        current_speaker = None;
        queue_size = 0;
        turn_count = 0;
      }
  end

(** End a voice conference *)
let end_conference ~agent_ids () =
  let* results = Lwt_list.map_s (fun agent_id ->
    end_voice_session ~agent_id
  ) agent_ids in
  let successful = List.filter_map (function Ok _ -> Some true | Error _ -> None) results in
  Lwt.return_ok (`Assoc [
    ("status", `String "ended");
    ("participants", `List (List.map (fun a -> `String a) agent_ids));
    ("sessions_closed", `Int (List.length successful));
  ])

(** Get conference transcript *)
let get_transcript () =
  let* available = is_voice_server_available () in
  if not available then
    Lwt.return_ok (`Assoc [
      ("transcript", `List []);
      ("turn_count", `Int 0);
      ("status", `String "voice_server_unavailable");
    ])
  else begin
    let args = `Assoc [("format", `String "structured")] in
    let* result = call_voice_mcp ~tool_name:"elevenlabs_get_transcript" ~arguments:args in
    match result with
    | Ok json ->
      (match extract_mcp_result json with
      | Ok data -> Lwt.return_ok data
      | Error _ -> Lwt.return_ok (`Assoc [("transcript", `List []); ("turn_count", `Int 0)]))
    | Error _ -> Lwt.return_ok (`Assoc [("transcript", `List []); ("turn_count", `Int 0)])
  end

(** ============================================
    Health Check
    ============================================ *)

(** Check if voice MCP server is available *)
let health_check () =
  let uri = Uri.make
    ~scheme:"http"
    ~host:(voice_mcp_host ())
    ~port:(voice_mcp_port ())
    ~path:"/health"
    ()
  in
  let check =
    Lwt.catch
      (fun () ->
        let* (resp, body) = Cohttp_lwt_unix.Client.get uri in
        let status = Cohttp.Response.status resp in
        let* body_str = Cohttp_lwt.Body.to_string body in
        if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then begin
          voice_server_available := Some true;
          Lwt.return_ok (`Assoc [
            ("status", `String "healthy");
            ("server", `String (Printf.sprintf "%s:%d" (voice_mcp_host ()) (voice_mcp_port ())));
            ("response", Yojson.Safe.from_string body_str);
            ("timeout_seconds", `Float (request_timeout_seconds ()));
          ])
        end else begin
          voice_server_available := Some false;
          Lwt.return_error (Printf.sprintf "Unhealthy: HTTP %d"
            (Cohttp.Code.code_of_status status))
        end)
      (fun exn ->
        voice_server_available := Some false;
        Lwt.return_error (Printf.sprintf "Not reachable: %s" (Printexc.to_string exn)))
  in
  with_timeout ~timeout:3.0 check
