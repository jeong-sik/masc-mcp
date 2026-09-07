type subscription =
  { auth_method : string
  ; subscription_type : string
  ; api_provider : string
  }

type config =
  { cli_path : string
  ; cwd : string
  ; model : string option
  ; system_prompt : string option
  ; admission_timeout_s : float
  ; native : Runtime_native_tools.posture
  ; setting_sources : Runtime_native_tools.claude_setting_source list
  ; timeout_s : float option
  ; wall_clock_ceiling_s : float option
  ; output_schema : Yojson.Safe.t option
  }

let default_timeout_s = 300.0
let process_termination_grace_s = 2.0

(* [None] installs no deadline. The vendor client owns when its own turn ends;
   a declared bound is the operator's optional liveness guard over a silent
   client, not a limit on how long legitimate work may take. *)
let stderr_chunk_bytes = 4096
let stderr_tail_bytes = 4096
let max_wire_line_bytes = 8 * 1024 * 1024
let mcp_server_name = "masc"

let default_config ~cwd =
  { cli_path = "claude"
  ; cwd
  ; model = None
  ; system_prompt = None
  ; native = Runtime_native_tools.claude_code_default
  ; setting_sources = []
  ; admission_timeout_s = default_timeout_s
  ; timeout_s = Some default_timeout_s
  ; wall_clock_ceiling_s = None
  ; output_schema = None
  }
;;

let timeout_s_for_phase config ~turn_admitted =
  if turn_admitted
  then config.timeout_s
  else Some config.admission_timeout_s
;;

type image_input =
  { media_type : string
  ; base64_data : string
  }

type session_mode =
  | Start
  | Resume of { session_id : string }

type rate_limit_status =
  | Allowed
  | Allowed_warning
  | Rejected

let rate_limit_status_to_string = function
  | Allowed -> "allowed"
  | Allowed_warning -> "allowed_warning"
  | Rejected -> "rejected"
;;

type rate_limit =
  { status : rate_limit_status
  ; rate_limit_type : string option
  ; resets_at : int option
  ; overage_status : string option
  ; overage_disabled_reason : string option
  }

type turn_usage =
  { input_tokens : int
  ; output_tokens : int
  ; cache_creation_input_tokens : int
  ; cache_read_input_tokens : int
  }

(* Token counts summed over the assistant frames of one turn. Each assistant
   frame carries the usage of the API call that produced it, and parallel
   tool calls repeat one message id with identical usage, so ids are
   deduplicated (code.claude.com/docs/en/agent-sdk/cost-tracking, read
   2026-09-03). A host stop reports this sum: the result frame that would
   carry the turn total never arrives once the host ends the turn, which is
   why every host-stopped turn recorded output_tokens = 0 until now.
   Declared here, before handle_control_request reads assistant_usage.total,
   so the field is in scope at its use site. *)
type assistant_usage =
  { seen_message_ids : (string, unit) Hashtbl.t
  ; mutable total : turn_usage option
  }

type turn_result =
  { session_id : string
  ; turn_id : string
  ; model : string
  ; text : string
  ; dynamic_tool_calls : int
  ; subscription : subscription
  ; rate_limit : rate_limit option
  ; resumed : bool
  ; usage : turn_usage option
  }

type terminal_boundary_outcome = Runtime_official_client_tool.terminal_boundary_outcome =
  | Terminal_completed
  | Durable_stimulus_deferred
  | Terminal_failed of
      { failure_class : Tool_result.tool_failure_class
      ; effect_disposition : Tool_result.failure_effect_disposition
      ; diagnostic : string
      }

type host_stop = Runtime_official_client_tool.host_stop =
  | Repeated_tool_call of
      { tool_name : string
      ; repeated_count : int
      }
  | Terminal_tool_boundary of
      { tool_name : string
      ; outcome : terminal_boundary_outcome
      }

type dynamic_tool_result = Runtime_official_client_tool.dynamic_tool_result =
  { success : bool
  ; content : string
  ; abort_turn : host_stop option
  }

type dynamic_tool = Runtime_official_client_tool.dynamic_tool =
  { name : string
  ; description : string
  ; input_schema : Yojson.Safe.t
  ; call : call_id:string -> Yojson.Safe.t -> dynamic_tool_result
  }

type stream_event =
  | Turn_started of
      { turn_id : string
      ; model : string
      }
  | Text_delta of string
  | Dynamic_tool_started of
      { call_id : string
      ; tool_name : string
      ; arguments : Yojson.Safe.t
      }
  | Dynamic_tool_finished of { call_id : string }
  | Native_tool_started of Runtime_native_tools.observation
  | Native_tool_finished of Runtime_native_tools.observation
  | Turn_finished of { text : string }

let emit_stream_event on_stream_event event =
  match on_stream_event with
  | None -> ()
  | Some emit ->
    (try emit event with
     | Eio.Cancel.Cancelled _ as exn -> raise exn
     | exn ->
       Log.Runtime_agent.warn
         "Claude Code stream callback raised (error=%s)"
         (Printexc.to_string exn))
;;

let dynamic_tool_bytes = Runtime_official_client_tool.dynamic_tool_bytes

type error =
  | Invalid_config of string
  | Spawn_failed of string
  | Protocol_error of
      { stage : string
      ; detail : string
      }
  | Subscription_required of string
  | Unsupported_control_request of string
  | Turn_transport_interrupted of
      { stage : string
      ; tool_effect_attempted : bool
      ; detail : string
      }
  | Context_window_exceeded of
      { message : string
      ; tool_effect_attempted : bool
      ; response_emitted : bool
      }
  | Turn_failed of string
  | Turn_failed_with_observation of
      { detail : string
      ; tool_effect_attempted : bool
      ; response_emitted : bool
      }
  | Stopped_by_host of
      { stop : host_stop
      ; usage : turn_usage option
        (** Token counts summed over the assistant frames seen before the
            host ended the turn. The result frame that would carry the turn
            total never arrives after a host stop. *)
      }
  | Quota_blocked of
      { api_error_status : int option
      ; rate_limit : rate_limit option
      ; tool_effect_attempted : bool
      ; response_emitted : bool
      }
  | Process_exited of string
  | Timeout of float

exception Runtime_error of error

let error_to_string = function
  | Invalid_config detail -> "invalid Claude Code config: " ^ detail
  | Spawn_failed detail -> "failed to start Claude Code: " ^ detail
  | Protocol_error { stage; detail } ->
    Printf.sprintf "Claude Code protocol error during %s: %s" stage detail
  | Subscription_required detail ->
    "Claude Code subscription login required: " ^ detail
  | Unsupported_control_request subtype ->
    "Claude Code requested unsupported control action: " ^ subtype
  | Turn_transport_interrupted { stage; tool_effect_attempted; detail } ->
    Printf.sprintf
      "Claude Code turn transport interrupted during %s (tool_effect_attempted=%b): %s"
      stage
      tool_effect_attempted
      detail
  | Context_window_exceeded
      { message; tool_effect_attempted; response_emitted } ->
    Printf.sprintf
      "Claude Code context window exceeded (tool_effect_attempted=%b response_emitted=%b): %s"
      tool_effect_attempted
      response_emitted
      message
  | Turn_failed detail -> "Claude Code turn failed: " ^ detail
  | Turn_failed_with_observation { detail; _ } ->
    "Claude Code turn failed: " ^ detail
  | Stopped_by_host { stop = Repeated_tool_call { tool_name; repeated_count }; _ } ->
    Printf.sprintf
      "Claude Code stopped after repeated tool call: tool=%s count=%d"
      tool_name
      repeated_count
  | Stopped_by_host { stop = Terminal_tool_boundary { tool_name; _ }; _ } ->
    Printf.sprintf "Claude Code stopped at terminal tool boundary: tool=%s" tool_name
  | Quota_blocked
      { api_error_status; rate_limit; tool_effect_attempted; response_emitted } ->
    let status =
      Option.fold
        ~none:"unknown"
        ~some:(fun value -> rate_limit_status_to_string value.status)
        rate_limit
    in
    let api_status =
      Option.fold ~none:"unknown" ~some:string_of_int api_error_status
    in
    Printf.sprintf
      "Claude Code subscription quota blocked (rate_limit=%s api_status=%s tool_effect_attempted=%b response_emitted=%b)"
      status
      api_status
      tool_effect_attempted
      response_emitted
  | Process_exited detail ->
    "Claude Code exited before terminal result: " ^ detail
  | Timeout seconds ->
    Printf.sprintf "Claude Code stream was idle for %.3fs" seconds
;;

let error_kind = function
  | Invalid_config _ -> "invalid_config"
  | Spawn_failed _ -> "spawn_failed"
  | Protocol_error _ -> "protocol_error"
  | Subscription_required _ -> "subscription_required"
  | Unsupported_control_request _ -> "unsupported_control_request"
  | Turn_transport_interrupted _ -> "turn_transport_interrupted"
  | Context_window_exceeded _ -> "context_window_exceeded"
  | Turn_failed _ -> "turn_failed"
  | Turn_failed_with_observation _ -> "turn_failed"
  | Stopped_by_host _ -> "stopped_by_host"
  | Quota_blocked _ -> "quota_blocked"
  | Process_exited _ -> "process_exited"
  | Timeout _ -> "timeout"
;;

let protocol_error stage detail = Error (Protocol_error { stage; detail })
let ( let* ) result f = Result.bind result f

(* The three official-client runtimes speak the same line-delimited JSON
   protocol and differ only in the error constructor they fail with. The shape
   checks live once in {!Runtime_official_client_json}; this instantiates them
   against this runtime's own [error]. *)
module Shared_json = Runtime_official_client_json.Make (struct
  type t = error

  let protocol ~stage ~detail = Protocol_error { stage; detail }
end)

open Shared_json

let bounded_tail = Runtime_official_client_json.bounded_tail

let parse_json ~stage text =
  let parsed =
    try Ok (Yojson.Safe.from_string text) with
    | Yojson.Json_error detail -> protocol_error stage ("invalid JSON: " ^ detail)
  in
  let* json = parsed in
  let* () = validate_unique_object_keys ~stage ~path:"$" json in
  Ok json
;;

let optional_int stage name fields =
  match List.assoc_opt name fields with
  | None | Some `Null -> Ok None
  | Some (`Int value) -> Ok (Some value)
  | Some _ ->
    protocol_error stage (Printf.sprintf "field %S must be an integer or null" name)
;;

let subscription_only_environment () =
  let inherited_names =
    [ "HOME"
    ; "USER"
    ; "PATH"
    ; "TMPDIR"
    ; "XDG_CONFIG_HOME"
    ; "XDG_DATA_HOME"
    ; "XDG_CACHE_HOME"
    ; "SSL_CERT_FILE"
    ; "SSL_CERT_DIR"
    ; "LANG"
    ; "LC_ALL"
    ; "LC_CTYPE"
    ; "TERM"
    ; "NO_COLOR"
    ]
  in
  inherited_names
  |> List.filter_map (fun name ->
    Option.map (fun value -> name ^ "=" ^ value) (Sys.getenv_opt name))
  |> fun inherited ->
  ("CLAUDE_CODE_ENTRYPOINT=masc" :: "CLAUDE_AGENT_SDK_VERSION=masc-ocaml" :: inherited)
  |> Array.of_list
;;

let parse_subscription json =
  let stage = "auth status" in
  let* fields = assoc_at stage json in
  let* logged_in = required_bool stage "loggedIn" fields in
  if not logged_in
  then Error (Subscription_required "claude auth status reported loggedIn=false")
  else
    let* auth_method = required_string stage "authMethod" fields in
    let* subscription_type = required_string stage "subscriptionType" fields in
    let* api_provider = required_string stage "apiProvider" fields in
    if auth_method <> "claude.ai"
    then
      Error
        (Subscription_required
           (Printf.sprintf "authMethod must be claude.ai, got %S" auth_method))
    else if api_provider <> "firstParty"
    then
      Error
        (Subscription_required
           (Printf.sprintf "apiProvider must be firstParty, got %S" api_provider))
    else Ok { auth_method; subscription_type; api_provider }
;;

let read_subscription ~mgr ~cwd config =
  try
    Eio.Process.parse_out
      mgr
      Eio.Buf_read.take_all
      ~cwd
      ~env:(subscription_only_environment ())
      [ config.cli_path; "auth"; "status"; "--json" ]
    |> String.trim
    |> parse_json ~stage:"auth status"
    |> fun result -> Result.bind result parse_subscription
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Eio.Exn.Io
      (Eio.Process.E (Eio.Process.Executable_not_found executable), _) ->
    Error (Spawn_failed (Printf.sprintf "executable %S was not found" executable))
  | exn -> Error (Spawn_failed (Printexc.to_string exn))
;;

let dynamic_tool_spec (tool : dynamic_tool) =
  `Assoc
    [ "name", `String tool.name
    ; "description", `String tool.description
    ; "inputSchema", tool.input_schema
    ]
;;

let find_dynamic_tool tools name =
  List.find_opt (fun (tool : dynamic_tool) -> String.equal tool.name name) tools
;;

type io =
  { send : Yojson.Safe.t -> unit
  ; receive : unit -> (Yojson.Safe.t, error) result
  }

let send_control_success io ~request_id response =
  io.send
    (`Assoc
       [ "type", `String "control_response"
       ; ( "response"
         , `Assoc
             [ "subtype", `String "success"
             ; "request_id", `String request_id
             ; "response", response
             ] )
       ])
;;

let send_control_error io ~request_id detail =
  io.send
    (`Assoc
       [ "type", `String "control_response"
       ; ( "response"
         , `Assoc
             [ "subtype", `String "error"
             ; "request_id", `String request_id
             ; "error", `String detail
             ] )
       ])
;;

type control_phase =
  | Before_turn_admission
  | Turn_admitted

let send_control_response
      io
      ~control_phase
      ~stage
      ~tool_effect_attempted
      response
  =
  try
    response ();
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Idle_timeout _ as exn -> raise exn
  | Eio.Time.Timeout as exn -> raise exn
  | exn ->
    let detail = Printexc.to_string exn in
    (match control_phase with
     | Before_turn_admission -> protocol_error stage detail
     | Turn_admitted ->
       Error
         (Turn_transport_interrupted
            { stage; tool_effect_attempted; detail }))
;;

let handle_control_request
      io
      ~control_phase
      ~mcp_session
      ~tools
      ~tool_call_count
      ~assistant_usage
      ~on_stream_event
      fields
  =
  let stage = "control request" in
  let* request_id = required_string stage "request_id" fields in
  let* request = required_member stage "request" fields in
  let* request_fields = assoc_at stage request in
  let* subtype = required_string stage "subtype" request_fields in
  match subtype with
  | "mcp_message" ->
    let* server_name = required_string stage "server_name" request_fields in
    if server_name <> mcp_server_name
    then
      let detail = Printf.sprintf "unknown SDK MCP server %S" server_name in
      let* () =
        send_control_response
          io
          ~control_phase
          ~stage:"control error response"
          ~tool_effect_attempted:false
          (fun () -> send_control_error io ~request_id detail)
      in
      Error (Unsupported_control_request subtype)
    else
      let* message = required_member stage "message" request_fields in
      let tool_specs () = List.map dynamic_tool_spec tools in
      let abort_turn = ref None in
      let call_tool ~name ~call_id ~arguments =
        match find_dynamic_tool tools name with
        | None -> None
        | Some tool ->
          emit_stream_event
            on_stream_event
            (Dynamic_tool_started { call_id; tool_name = name; arguments });
          let result =
            try tool.call ~call_id arguments with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | exn ->
              Log.Runtime_agent.warn
                "Claude Code MCP tool handler raised (tool=%s error=%s)"
                name
                (Printexc.to_string exn);
              { success = false
              ; content = "MASC tool handler raised"
              ; abort_turn = None
              }
          in
          abort_turn := result.abort_turn;
          emit_stream_event on_stream_event (Dynamic_tool_finished { call_id });
          Some
            { Runtime_official_client_mcp.success = result.success
            ; content = result.content
            }
      in
      let* dispatch =
        Runtime_official_client_mcp.handle_message
          ~session:mcp_session
          ~server_name:mcp_server_name
          ~tool_call_policy:
            (match control_phase with
             | Before_turn_admission ->
               Runtime_official_client_mcp.Reject_tool_calls
             | Turn_admitted -> Runtime_official_client_mcp.Allow_tool_calls)
          ~tool_specs
          ~call_tool
          message
        |> Result.map_error (fun { Runtime_official_client_mcp.stage; detail } ->
          Protocol_error { stage; detail })
      in
      if dispatch.tool_called then incr tool_call_count;
      (* The control channel and the MCP payload have different reply rules and
         the two must not be conflated. An MCP notification carries no id and so
         produces no JSON-RPC response, but the client wraps *every* MCP message
         in a control_request that carries a [request_id] and blocks until that
         id is answered. Returning without sending anything therefore parks the
         client forever: it waits for the control_response, MASC waits for the
         next message, and the stream-idle timeout eventually fires.

         Measured against the real client (2.1.226): with the notification
         unanswered the turn stops after `notifications/initialized` and never
         reaches a result; answering it with an empty payload lets the same turn
         run tools/list -> init -> result in 5.2s. Every other variable held. *)
      let control_payload =
        match dispatch.response with
        | Some mcp_response -> `Assoc [ "mcp_response", mcp_response ]
        | None -> `Assoc []
      in
      let* () =
        send_control_response
          io
          ~control_phase
          ~stage:"control success response"
          ~tool_effect_attempted:dispatch.tool_called
          (fun () -> send_control_success io ~request_id control_payload)
      in
      (match !abort_turn with
       | None -> Ok ()
       | Some stop -> Error (Stopped_by_host { stop; usage = assistant_usage.total }))
  | unsupported ->
    let* () =
      send_control_response
        io
        ~control_phase
        ~stage:"control error response"
        ~tool_effect_attempted:false
        (fun () ->
          send_control_error
            io
            ~request_id
            (Printf.sprintf "MASC does not support control request %S" unsupported))
    in
    Error (Unsupported_control_request unsupported)
;;

let parse_wire_line line = parse_json ~stage:"stream-json message" line

let wire_fields json =
  let stage = "stream-json message" in
  let* fields = assoc_at stage json in
  let* type_ = required_string stage "type" fields in
  Ok (type_, fields)
;;

let initialize_request request_id =
  `Assoc
    [ "type", `String "control_request"
    ; "request_id", `String request_id
    ; ( "request"
      , `Assoc [ "subtype", `String "initialize"; "hooks", `Null ] )
    ]
;;

let parse_control_response ~expected_request_id fields =
  let stage = "initialize control response" in
  let* response = required_member stage "response" fields in
  let* response_fields = assoc_at stage response in
  let* request_id = required_string stage "request_id" response_fields in
  if request_id <> expected_request_id
  then
    protocol_error
      stage
      (Printf.sprintf
         "response request_id %S does not match %S"
         request_id
         expected_request_id)
  else
    let* subtype = required_string stage "subtype" response_fields in
    match subtype with
    | "success" -> Ok ()
    | "error" ->
      let* detail = required_string stage "error" response_fields in
      Error (Turn_failed ("control initialization failed: " ^ detail))
    | other ->
      protocol_error stage (Printf.sprintf "unknown response subtype %S" other)
;;

let rec await_initialize io ~mcp_session ~tools ~tool_call_count ~assistant_usage ~request_id
    ~on_stream_event ~ignored =
  let* json = io.receive () in
  let* type_, fields = wire_fields json in
  match type_ with
  | "control_response" ->
    parse_control_response ~expected_request_id:request_id fields
  | "control_request" ->
    let* () =
      handle_control_request
        io
        ~control_phase:Before_turn_admission
        ~mcp_session
        ~tools
        ~tool_call_count
        ~assistant_usage
        ~on_stream_event
        fields
    in
    await_initialize
      io ~mcp_session ~tools ~tool_call_count ~assistant_usage ~request_id ~on_stream_event
      ~ignored
  | ("system" | "rate_limit_event") when ignored < 32 ->
    await_initialize
      io
      ~mcp_session
      ~tools
      ~tool_call_count
      ~assistant_usage
      ~request_id
      ~on_stream_event
      ~ignored:(ignored + 1)
  | other ->
    protocol_error
      "initialize"
      (Printf.sprintf "unexpected message type %S before control response" other)
;;

(* The stream-json user message always carries a content-block array, never the
   bare-string form. Both are valid on the wire, but one shape means one code
   path: an image turn and a text turn differ only in how many blocks precede
   the text. Images come first — the same order the vision docs use, and the
   order a reader needs to make sense of the text that refers to them. *)
let image_block (image : image_input) =
  `Assoc
    [ "type", `String "image"
    ; ( "source"
      , `Assoc
          [ "type", `String "base64"
          ; "media_type", `String image.media_type
          ; "data", `String image.base64_data
          ] )
    ]
;;

let user_message ~images prompt =
  let blocks =
    List.map image_block images @ [ `Assoc [ "type", `String "text"; "text", `String prompt ] ]
  in
  `Assoc
    [ "type", `String "user"
    ; "message", `Assoc [ "role", `String "user"; "content", `List blocks ]
    ; "parent_tool_use_id", `Null
    ; "session_id", `String "default"
    ]
;;

let parse_rate_limit ~expected_session_id fields =
  let stage = "rate_limit_event" in
  let* session_id = required_string stage "session_id" fields in
  let* () =
    if session_id = expected_session_id
    then Ok ()
    else protocol_error stage "session_id does not match the active Claude session"
  in
  let* info = required_member stage "rate_limit_info" fields in
  let* info_fields = assoc_at stage info in
  let* status_wire = required_string stage "status" info_fields in
  let* status =
    match status_wire with
    | "allowed" -> Ok Allowed
    | "allowed_warning" -> Ok Allowed_warning
    | "rejected" -> Ok Rejected
    | unknown -> protocol_error stage (Printf.sprintf "unknown status %S" unknown)
  in
    let* rate_limit_type = optional_string stage "rateLimitType" info_fields in
    let* resets_at = optional_int stage "resetsAt" info_fields in
    let* overage_status = optional_string stage "overageStatus" info_fields in
    let* overage_disabled_reason =
      optional_string stage "overageDisabledReason" info_fields
    in
    Ok
      { status
      ; rate_limit_type
      ; resets_at
      ; overage_status
      ; overage_disabled_reason
      }
;;

type assistant_block =
  | Assistant_text of string
  | Assistant_native_tool of Runtime_native_tools.observation

type assistant_origin =
  | Model_response
  | Api_error_diagnostic

(* Claude Code 2.1.263's assistant envelope declares this optional boolean;
   its producer maps isApiErrorMessage=true to is_api_error_message=true.
   The text of an API diagnostic is not a model response. Only the terminal
   result classifies the failure; tool effects remain independent evidence. *)
let assistant_origin ~stage fields =
  match List.assoc_opt "is_api_error_message" fields with
  | None | Some (`Bool false) -> Ok Model_response
  | Some (`Bool true) -> Ok Api_error_diagnostic
  | Some _ -> protocol_error stage "field \"is_api_error_message\" must be a boolean"
;;

let non_blank = function
  | Some value when String.trim value <> "" -> Some value
  | Some _ | None -> None
;;

let allowed_tool_name (tool : dynamic_tool) =
  Printf.sprintf "mcp__%s__%s" mcp_server_name tool.name
;;

let assistant_blocks ~stage ~mcp_tool_names content =
  match content with
  | `List blocks ->
    let rec loop parsed = function
      | [] -> Ok (List.rev parsed)
      | `Assoc fields :: rest ->
        let* type_ = required_string stage "type" fields in
        (match type_ with
         | "text" ->
           let* text = required_string stage "text" fields in
           loop (Assistant_text text :: parsed) rest
         | "tool_use" ->
           let* call_id = optional_string stage "id" fields in
           let* tool_name = optional_string stage "name" fields in
           let tool_name = non_blank tool_name in
           let origin =
             match tool_name with
             | Some name when List.mem name mcp_tool_names ->
               Runtime_native_tools.Mcp_wrapper
             | Some _ | None -> Runtime_native_tools.Built_in
           in
           loop
             (Assistant_native_tool
                { identity =
                    Option.map
                      (fun call_id -> Runtime_native_tools.Call_id call_id)
                      (non_blank call_id)
                ; tool_name
                ; origin
                }
              :: parsed)
             rest
         | "thinking" -> loop parsed rest
         | other ->
           protocol_error
             stage
             (Printf.sprintf "unsupported assistant content type %S" other))
      | _ :: _ -> protocol_error stage "assistant content block must be an object"
    in
    loop [] blocks
  | _ -> protocol_error stage "assistant content must be an array"
;;

(* The keeper side of this runtime hardcoded [usage = None] while the
   antigravity runtime fills the same slot from its CLI stream, which is why
   official-client turns carry no input_tokens (#28023) and why a window
   overflow is discovered from the result frame's own verdict below rather
   than from token counts (#27427). Read leniently: a turn must not fail
   because its usage block is absent or shaped differently than expected,
   and an absent value after this lands means the CLI does not send one. *)
let turn_usage_of_fields fields =
  match List.assoc_opt "usage" fields with
  | None | Some `Null -> None
  | Some (`Assoc usage_fields) ->
    let int_field name =
      match List.assoc_opt name usage_fields with
      | Some (`Int value) when value >= 0 -> Some value
      | _ -> None
    in
    (* The CLI mirrors Anthropic Messages usage: input_tokens is the
       exclusive count (tokens after the last cache breakpoint) and the
       cache components arrive as their own fields. Both are carried so
       the keeper can build the canonical inclusive api_usage
       (Backend_anthropic.usage_of_wire_counts); an absent cache field
       reads as 0, the same convention every Anthropic-format parse in
       agent_core uses. Presence of a usage block still requires both
       base counts. *)
    (match int_field "input_tokens", int_field "output_tokens" with
     | Some input_tokens, Some output_tokens ->
       Some
         { input_tokens
         ; output_tokens
         ; cache_creation_input_tokens =
             (* DET-OK: absent wire field is 0 by api_usage convention *)
             Option.value (int_field "cache_creation_input_tokens") ~default:0
         ; cache_read_input_tokens =
             (* DET-OK: absent wire field is 0 by api_usage convention *)
             Option.value (int_field "cache_read_input_tokens") ~default:0
         }
     | Some _, None | None, Some _ | None, None -> None)
  | Some _ -> None
;;

let new_assistant_usage () = { seen_message_ids = Hashtbl.create 8; total = None }

let add_turn_usage (a : turn_usage) (b : turn_usage) =
  { input_tokens = a.input_tokens + b.input_tokens
  ; output_tokens = a.output_tokens + b.output_tokens
  ; cache_creation_input_tokens =
      a.cache_creation_input_tokens + b.cache_creation_input_tokens
  ; cache_read_input_tokens = a.cache_read_input_tokens + b.cache_read_input_tokens
  }
;;

let observe_assistant_usage acc ~message_id usage =
  let count usage =
    acc.total
    <- Some
         (match acc.total with
          | None -> usage
          | Some total -> add_turn_usage total usage)
  in
  match usage with
  | None -> ()
  | Some usage ->
    (match message_id with
     | Some id when Hashtbl.mem acc.seen_message_ids id -> ()
     | Some id ->
       Hashtbl.replace acc.seen_message_ids id ();
       count usage
     | None ->
       (* A frame without a message id cannot be matched to a sibling, so
          it counts on its own. *)
       count usage)
;;

let parse_assistant ~expected_session_id ~tools fields =
  let stage = "assistant message" in
  let* session_id = required_string stage "session_id" fields in
  if session_id <> expected_session_id
  then protocol_error stage "session_id does not match the active Claude session"
  else
    let* origin = assistant_origin ~stage fields in
    let* uuid = required_string stage "uuid" fields in
    let* message = required_member stage "message" fields in
    let* message_fields = assoc_at stage message in
    let* model = required_string stage "model" message_fields in
    let* content = required_member stage "content" message_fields in
    let message_id =
      match List.assoc_opt "id" message_fields with
      | Some (`String id) -> Some id
      | Some _ | None -> None
    in
    let usage = turn_usage_of_fields message_fields in
    let* blocks =
      assistant_blocks
        ~stage
        ~mcp_tool_names:(List.map allowed_tool_name tools)
        content
    in
    Ok (origin, uuid, model, blocks, message_id, usage)
;;

let native_tool_result_ids ~expected_session_id fields =
  let stage = "user message" in
  let* session_id = optional_string stage "session_id" fields in
  let* () =
    match non_blank session_id with
    | Some session_id when session_id <> expected_session_id ->
      protocol_error stage "session_id does not match the active Claude session"
    | Some _ | None -> Ok ()
  in
  match List.assoc_opt "message" fields with
  | Some (`Assoc message_fields) ->
    (match List.assoc_opt "content" message_fields with
     | Some (`List blocks) ->
       let rec loop ids = function
         | [] -> Ok (List.rev ids)
         | `Assoc block_fields :: rest ->
           (match List.assoc_opt "type" block_fields with
            | Some (`String "tool_result") ->
              let* call_id = optional_string stage "tool_use_id" block_fields in
              loop (Option.to_list (non_blank call_id) @ ids) rest
            | Some (`String _) | None -> loop ids rest
            | Some _ -> protocol_error stage "content block type must be a string")
         | _ :: _ -> protocol_error stage "user content block must be an object"
       in
       loop [] blocks
     | Some `Null | None -> Ok []
     | Some _ -> protocol_error stage "user content must be an array")
  | Some `Null | None -> Ok []
  | Some _ -> protocol_error stage "user message must be an object"
;;

let parse_result ~expected_session_id ~rate_limit ~tool_effect_attempted
    ~response_emitted fields =
  let stage = "result message" in
  let* subtype = required_string stage "subtype" fields in
  let* is_error = required_bool stage "is_error" fields in
  let* session_id = required_string stage "session_id" fields in
  if session_id <> expected_session_id
  then protocol_error stage "session_id does not match the active Claude session"
  else
    let* turn_id = required_string stage "uuid" fields in
    let* api_error_status = optional_int stage "api_error_status" fields in
    let* terminal_reason = optional_string stage "terminal_reason" fields in
    let result =
      match List.assoc_opt "result" fields with
      | None | Some `Null -> Ok None
      | Some (`String value) -> Ok (Some value)
      | Some _ -> protocol_error stage "field \"result\" must be a string or null"
    in
    let* result = result in
    (* Under --json-schema the client validates its own answer and re-prompts,
       and [structured_output] carries what passed. The docs say a run can end
       [subtype=success] with the field absent and that this counts as a
       failure, so an absent field is left to the caller's own contract rather
       than silently read as "no schema was asked for". Measured 2026-08-30 in
       this argv shape: a prompt asking for a key the schema forbids came back
       without it, and both fields agreed. Prefer the parsed value anyway --
       the sibling antigravity adapter's narrated text does not agree. *)
    let result =
      match List.assoc_opt "structured_output" fields with
      | None | Some `Null -> result
      | Some value -> Some (Yojson.Safe.to_string value)
    in
    let usage = turn_usage_of_fields fields in
    let structurally_quota_blocked =
      Option.equal Int.equal api_error_status (Some 429)
      || Option.exists
           (fun (limit : rate_limit) -> limit.status = Rejected)
           rate_limit
    in
    let terminal_failure_detail () =
      (* [terminal_reason] decides whether an overflow is typed as one, and it
         used to be absent from what a reader sees. A live failure carrying
         "Prompt is too long" that did not reach the overflow path was
         indistinguishable from one where the sentence never appeared: both
         printed the same line, and the field that separates them was not in
         it (one keeper, 2026-08-24). Naming it costs one field and answers the
         question the next reader will have. *)
      Printf.sprintf
        "terminal subtype=%s api_status=%s reason=%s%s"
        subtype
        (Option.fold
           ~none:"unknown"
           ~some:string_of_int
           api_error_status)
        (Option.value terminal_reason ~default:"none")
        (match result with
         | Some detail when String.trim detail <> "" -> ": " ^ String.trim detail
         | Some _ | None -> "")
    in
    let provider_reported_context_window_exceeded =
      (* The CLI states why its query loop terminated as a typed enum on this
         frame; [prompt_too_long] is the CLI's own promotion of every provider
         context-window rejection ("Prompt is too long" / "Input is too long
         for requested model", either status, or a 413 naming the window). A
         frame that carries the verdict is authoritative in both directions,
         like the codex lane's [codexErrorInfo].

         Frames without the enum fall back to the CLI's own sentence table,
         with no status requirement. The historical 400 requirement missed
         real frames: on 2026-08-14 the CLI reported three resumed-session
         overflows as [subtype=success, is_error=true] with no
         [api_error_status] and no [terminal_reason] — only the sentence —
         so the overflow fell to the generic-failure path and surfaced as an
         unmapped internal error. Both sentences appear verbatim in the CLI
         binary (2.1.232). *)
      match terminal_reason with
      | Some reason -> String.equal reason "prompt_too_long"
      | None ->
        Option.exists
          (fun detail ->
             let detail = String.trim detail in
             String.starts_with ~prefix:"Prompt is too long" detail
             || String.starts_with ~prefix:"Input is too long for requested model" detail)
          result
    in
    if structurally_quota_blocked
    then
      Error
        (Quota_blocked
           { api_error_status
           ; rate_limit
           ; tool_effect_attempted
           ; response_emitted
           })
    else if is_error && provider_reported_context_window_exceeded
    then
      Error
        (Context_window_exceeded
           { message = terminal_failure_detail ()
           ; tool_effect_attempted
           ; response_emitted
           })
    else if is_error
    then
      (* [result] is the one field on this frame that says why the turn failed.
         The prompt-too-long verdict above (typed [terminal_reason], or the
         exact 400 prefix for pre-enum CLIs) has its own typed path;
         unrelated 400s and every other terminal rejection still retain the
         provider's sentence instead of collapsing to the status code
         (#28071). *)
      Error
        (Turn_failed_with_observation
           { detail = terminal_failure_detail ()
           ; tool_effect_attempted
           ; response_emitted
           })
    else if subtype <> "success"
    then Error (Turn_failed (Printf.sprintf "terminal subtype=%s" subtype))
    else Ok (turn_id, result, usage)
;;

let max_ignored_messages = 256

let rec await_terminal io ~mcp_session ~tools ~tool_call_count ~assistant_usage
    ~expected_session_id
    ~subscription ~resumed ~rate_limit ~assistant_model ~assistant_texts
    ~native_tool_calls ~native_tool_attempted ~on_turn_started ~on_stream_event
    ~stream_started ~response_emitted ~ignored =
  let* json = io.receive () in
  let* type_, fields = wire_fields json in
  match type_ with
  | "control_request" ->
    let* () =
      handle_control_request
        io
        ~control_phase:Turn_admitted
        ~mcp_session
        ~tools
        ~tool_call_count
        ~assistant_usage
        ~on_stream_event
        fields
    in
    await_terminal
      io ~mcp_session ~tools ~tool_call_count ~assistant_usage ~expected_session_id
      ~subscription ~resumed
      ~rate_limit ~assistant_model ~assistant_texts ~on_turn_started ~ignored
      ~native_tool_calls ~native_tool_attempted ~on_stream_event ~stream_started
      ~response_emitted
  | "control_response" ->
    protocol_error "turn" "received an unsolicited control response"
  | "assistant" ->
    let* origin, uuid, model, blocks, message_id, usage =
      parse_assistant ~expected_session_id ~tools fields
    in
    let assistant_model =
      match origin with
      | Api_error_diagnostic -> assistant_model
      | Model_response ->
        observe_assistant_usage assistant_usage ~message_id usage;
        if not !stream_started
        then (
          stream_started := true;
          emit_stream_event on_stream_event (Turn_started { turn_id = uuid; model }));
        Some model
    in
    let texts_rev = ref [] in
    List.iter
      (function
        | Assistant_text text ->
          (match origin with
           | Api_error_diagnostic -> ()
           | Model_response ->
             texts_rev := text :: !texts_rev;
             emit_stream_event on_stream_event (Text_delta text))
        | Assistant_native_tool observation ->
          native_tool_attempted := true;
          Option.iter
            (fun call_id -> Hashtbl.replace native_tool_calls call_id observation)
            (Runtime_native_tools.call_id observation);
          emit_stream_event on_stream_event (Native_tool_started observation))
      blocks;
    let texts = List.rev !texts_rev in
    if List.exists (fun text -> String.length text > 0) texts then response_emitted := true;
    await_terminal
      io ~mcp_session ~tools ~tool_call_count ~assistant_usage ~expected_session_id
      ~subscription ~resumed
      ~rate_limit ~assistant_model
      ~assistant_texts:(assistant_texts @ texts)
      ~native_tool_calls ~native_tool_attempted ~on_turn_started ~on_stream_event
      ~stream_started ~response_emitted ~ignored
  | "rate_limit_event" ->
    let* rate_limit = parse_rate_limit ~expected_session_id fields in
    await_terminal
      io ~mcp_session ~tools ~tool_call_count ~assistant_usage ~expected_session_id
      ~subscription ~resumed
      ~rate_limit:(Some rate_limit) ~assistant_model ~assistant_texts
      ~native_tool_calls ~native_tool_attempted ~on_turn_started ~on_stream_event
      ~stream_started ~response_emitted ~ignored
  | "result" ->
    let parsed_result =
      parse_result
        ~expected_session_id
        ~rate_limit
        ~tool_effect_attempted:(!tool_call_count > 0 || !native_tool_attempted)
        ~response_emitted:!response_emitted
        fields
    in
    let* turn_id, result, usage =
      match parsed_result with
      | Error
          (Context_window_exceeded
             { tool_effect_attempted = false; response_emitted = false; _ } as error) ->
        if !stream_started
        then emit_stream_event on_stream_event (Turn_finished { text = "" });
        Error error
      | result -> result
    in
    let* () =
      invoke_state_callback ~stage:"turn started callback" (fun () ->
        on_turn_started ~session_id:expected_session_id ~turn_id)
    in
    let* model =
      match assistant_model with
      | Some model -> Ok model
      | None -> protocol_error "result message" "no measured assistant model"
    in
    let text =
      match result with
      | Some text when String.trim text <> "" -> Some text
      | None | Some _ ->
        assistant_texts
        |> String.concat "\n"
        |> String.trim
        |> fun value -> if value = "" then None else Some value
    in
    let* text =
      match text with
      | Some text -> Ok text
      | None -> protocol_error "result message" "successful turn has no text"
    in
    emit_stream_event on_stream_event (Turn_finished { text });
    Ok
      { session_id = expected_session_id
      ; turn_id
      ; model
      ; text
      ; dynamic_tool_calls = !tool_call_count
      ; subscription
      ; rate_limit
      ; resumed
      ; usage
      }
  | "user" when ignored < max_ignored_messages ->
    let* finished_ids = native_tool_result_ids ~expected_session_id fields in
    List.iter
      (fun call_id ->
         match Hashtbl.find_opt native_tool_calls call_id with
         | None -> ()
         | Some observation ->
           Hashtbl.remove native_tool_calls call_id;
           emit_stream_event on_stream_event (Native_tool_finished observation))
      finished_ids;
    await_terminal
      io ~mcp_session ~tools ~tool_call_count ~assistant_usage ~expected_session_id
      ~subscription ~resumed
      ~rate_limit ~assistant_model ~assistant_texts ~native_tool_calls
      ~native_tool_attempted ~on_turn_started ~on_stream_event ~stream_started
      ~response_emitted ~ignored:(ignored + 1)
  | ("system" | "tool_progress") when ignored < max_ignored_messages ->
    (* Claude Code emits [tool_progress] while a built-in tool is still
       running.  It is observation-only: tool ownership and completion still
       arrive through assistant/user messages.  Consume it as bounded stream
       activity without treating an in-flight tool as a protocol failure. *)
    await_terminal
      io ~mcp_session ~tools ~tool_call_count ~assistant_usage ~expected_session_id
      ~subscription ~resumed
      ~rate_limit ~assistant_model ~assistant_texts ~on_turn_started
      ~native_tool_calls ~native_tool_attempted ~on_stream_event ~stream_started
      ~response_emitted ~ignored:(ignored + 1)
  | other ->
    protocol_error
      "turn"
      (Printf.sprintf "unsupported stream message type %S" other)
;;

let mcp_config tools =
  match tools with
  | [] -> None
  | _ ->
    Some
      (Yojson.Safe.to_string
         (`Assoc
            [ ( "mcpServers"
              , `Assoc
                  [ ( mcp_server_name
                    , `Assoc
                        [ "type", `String "sdk"
                        ; "name", `String mcp_server_name
                        ] )
                  ] )
            ]))
;;

(* The CLI's effort vocabulary as a total snap: [minimal] is the one effort
   the CLI refuses, and its nearest admitted neighbour is [low]. Every other
   effort is itself. [reasoning_args] below stays the enforcing boundary
   (a caller that skips this snap still fails loudly rather than sending a
   flag the CLI rejects); the keeper lane applies the snap so an
   operator-declared [minimal] survives as a turn instead of killing it,
   mirroring the Codex lane's catalog clamp. *)
let cli_admitted_reasoning_effort = function
  | Llm_provider.Reasoning_effort.Minimal -> Llm_provider.Reasoning_effort.Low
  | effort -> effort
;;

let reasoning_args = function
  | None -> Ok []
  | Some Llm_provider.Reasoning_effort.None_ ->
    Ok [ "--thinking"; "disabled" ]
  | Some Llm_provider.Reasoning_effort.Minimal ->
    Error
      (Invalid_config
         "Claude Code does not admit reasoning effort minimal; use low, medium, high, xhigh, max, or none")
  | Some effort ->
    Ok [ "--effort"; Llm_provider.Reasoning_effort.to_string effort ]
;;

let command config ~dynamic_tools ~reasoning_effort ~session_mode ~session_id =
  let* reasoning_args = reasoning_args reasoning_effort in
  let args =
    [ config.cli_path; "--output-format"; "stream-json"; "--verbose" ]
    (* Passing [--system-prompt] replaces the CLI's built-in prompt outright,
       so omitting the flag is what selects that prompt. An empty string is not
       the same as omitting it: claude 2.1.260 picks the prompt with
       [typeof r === "string" ? [r] : Array.isArray(r) ? r : o], where [r] is
       the given prompt and [o] the built-in one, so "" takes the string branch
       and the built-in prompt is discarded. The CLI's own --help says as much
       twice — "Only applies with the default system prompt (ignored with
       --system-prompt)" under --exclude-dynamic-system-prompt-sections, and
       "passing --system-prompt or --append-system-prompt turns it off" under
       --system-prompt-snapshot, whose default-on is lost with the flag
       present. Docs: https://code.claude.com/docs/en/cli-reference —
       "--system-prompt: Replace the entire system prompt with custom text".
       [None] therefore drops the flag instead of sending "". *)
    @ (match config.system_prompt with
       | None -> []
       | Some prompt -> [ "--system-prompt"; prompt ])
    @ [ "--tools"; Runtime_native_tools.claude_code_tools_arg config.native ]
    @ ((* [Native_read] pre-approves its built-in read tools alongside the
          MCP tools so [dontAsk] never has a prompt to suppress.
          [Native_full] enables the whole built-in set via [--tools default],
          which cannot be enumerated here; execution is governed by
          [--permission-mode dontAsk] and admitted only for Yolo keepers. *)
       let native_allowed =
         match config.native with
         | Runtime_native_tools.Native_read ->
           Runtime_native_tools.claude_code_read_tool_names
         | Runtime_native_tools.Native_none
         | Runtime_native_tools.Native_full -> []
       in
       match native_allowed @ List.map allowed_tool_name dynamic_tools with
       | [] -> []
       | allowed -> [ "--allowedTools"; String.concat "," allowed ])
    @ (match config.model with
       | None -> []
       | Some model -> [ "--model"; model ])
    @ [ "--permission-mode"; "dontAsk" ]
    @ (match session_mode with
       | Start -> [ "--session-id=" ^ session_id ]
       | Resume { session_id } -> [ "--resume=" ^ session_id ])
    @ (match mcp_config dynamic_tools with
       | None -> []
       | Some value -> [ "--mcp-config"; value; "--strict-mcp-config" ])
    (* Empty renders the historical [--setting-sources=]: no settings layer,
       so disk-level skills/hooks/subagents/CLAUDE.md stay off. A non-empty
       list arrives only through keeper-profile opt-in gated on the yolo
       approval mode ([Keeper_official_client_host.admit_claude_setting_sources]). *)
    @ [ Runtime_native_tools.claude_setting_sources_arg config.setting_sources ]
    @ reasoning_args
    @ (match config.output_schema with
       | None -> []
       (* --json-schema is documented as print-mode only, and this invocation
          is print mode: --input-format stream-json only works with --print.
          Measured 2026-08-30 in exactly this argv shape -- a prompt asking for
          a key the schema forbids came back without it, and the result frame
          carried structured_output. *)
       | Some schema -> [ "--json-schema"; Yojson.Safe.to_string schema ])
    @ [ "--input-format"; "stream-json" ]
  in
  Ok args
;;

let drain_stderr flow tail =
  let chunk = Cstruct.create stderr_chunk_bytes in
  try
    while true do
      let count = Eio.Flow.single_read flow chunk in
      let text = Cstruct.to_string (Cstruct.sub chunk 0 count) in
      tail := bounded_tail ~limit:stderr_tail_bytes !tail text
    done
  with
  | End_of_file -> ()
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn ->
    Log.Runtime_agent.debug
      "Claude Code stderr drain failed: %s"
      (Printexc.to_string exn)
;;

let terminate_spawned_process ~clock proc stdin_w =
  let owning_switch_cancelled = Eio.Fiber.is_cancelled () in
  Eio.Cancel.protect (fun () ->
    (try Eio.Flow.close stdin_w with
     | exn ->
       Log.Runtime_agent.debug
         "Claude Code stdin close failed: %s"
         (Printexc.to_string exn));
    (try Eio.Process.signal proc Sys.sigterm with
     | exn ->
       Log.Runtime_agent.debug
         "Claude Code termination signal failed: %s"
         (Printexc.to_string exn));
    if not owning_switch_cancelled
    then
      try
        Eio.Time.with_timeout_exn clock process_termination_grace_s (fun () ->
          Eio.Process.await proc |> ignore)
      with
      | Eio.Time.Timeout ->
        (try
           Eio.Process.signal proc Sys.sigkill;
           Eio.Process.await proc |> ignore
         with
         | exn ->
           Log.Runtime_agent.warn
             "Claude Code forced reap failed: %s"
             (Printexc.to_string exn))
      | exn ->
        Log.Runtime_agent.debug
          "Claude Code reap observed an already-closed process: %s"
          (Printexc.to_string exn))
;;

let run_protocol io ~dynamic_tools ~subscription ~session_mode ~session_id
    ~prompt ~images ~on_session_ready ~on_turn_starting ~on_turn_started
    ~on_stream_event ~turn_admitted =
  let tool_call_count = ref 0 in
  let assistant_usage = new_assistant_usage () in
  let mcp_session = Runtime_official_client_mcp.create_session () in
  let initialize_id = Random_id.prefixed ~prefix:"masc-init-" ~bytes:12 in
  io.send (initialize_request initialize_id);
  let* () =
    await_initialize
      io
      ~mcp_session
      ~tools:dynamic_tools
      ~tool_call_count
      ~assistant_usage
      ~request_id:initialize_id
      ~on_stream_event
      ~ignored:0
  in
  let* () =
    invoke_state_callback ~stage:"session ready callback" (fun () ->
      on_session_ready ~session_id)
  in
  let* () =
    invoke_state_callback ~stage:"turn starting callback" (fun () ->
      on_turn_starting ~session_id)
  in
  let* () =
    try
      io.send (user_message ~images prompt);
      turn_admitted := true;
      Ok ()
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | Idle_timeout _ as exn -> raise exn
    | Eio.Time.Timeout as exn -> raise exn
    | exn ->
      Error
        (Turn_transport_interrupted
           { stage = "user message write"
           ; tool_effect_attempted = false
           ; detail = Printexc.to_string exn
           })
  in
  await_terminal
    io
    ~mcp_session
    ~tools:dynamic_tools
    ~tool_call_count
    ~assistant_usage
    ~expected_session_id:session_id
    ~subscription
    ~resumed:
      (match session_mode with
       | Start -> false
       | Resume _ -> true)
    ~rate_limit:None
    ~assistant_model:None
    ~assistant_texts:[]
    ~native_tool_calls:(Hashtbl.create 8)
    ~native_tool_attempted:(ref false)
    ~on_turn_started
    ~on_stream_event
    ~stream_started:(ref false)
    ~response_emitted:(ref false)
    ~ignored:0
;;

let run_spawned ?on_spawned ~mgr ~clock ~cwd config ~dynamic_tools
    ~reasoning_effort ~session_mode ~session_id ~subscription ~prompt ~images
    ~on_session_ready ~on_turn_starting ~on_turn_started ~on_stream_event =
  let* argv =
    command config ~dynamic_tools ~reasoning_effort ~session_mode ~session_id
  in
  let turn_admitted = ref false in
  try
    Eio.Switch.run (fun sw ->
    let stdin_r, stdin_w = Eio.Process.pipe ~sw mgr in
    let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
    let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
    let stderr_tail = ref "" in
    let proc =
      try
        Eio.Process.spawn
          ~sw
          mgr
          ~cwd
          ~env:(subscription_only_environment ())
          ~stdin:stdin_r
          ~stdout:stdout_w
          ~stderr:stderr_w
          argv
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> raise (Runtime_error (Spawn_failed (Printexc.to_string exn)))
    in
    Option.iter (fun callback -> callback ()) on_spawned;
    Eio.Flow.close stdin_r;
    Eio.Flow.close stdout_w;
    Eio.Flow.close stderr_w;
    Eio.Fiber.fork ~sw (fun () -> drain_stderr stderr_r stderr_tail);
    let reader = Eio.Buf_read.of_flow ~max_size:max_wire_line_bytes stdout_r in
    let wall_clock =
      Runtime_wall_clock.make ?ceiling_s:config.wall_clock_ceiling_s ~now:(fun () -> Eio.Time.now clock) ()
    in
    let current_timeout_s () =
      timeout_s_for_phase config ~turn_admitted:!turn_admitted
      |> Runtime_wall_clock.cap_window wall_clock
    in
    let send json =
      with_optional_timeout clock (current_timeout_s ()) (fun () ->
        Eio.Flow.copy_string (Yojson.Safe.to_string json) stdin_w;
        Eio.Flow.copy_string "\n" stdin_w)
    in
    let receive () =
      if Runtime_wall_clock.expired wall_clock
      then
        Error
          (Timeout
             (Option.value config.wall_clock_ceiling_s
                ~default:Runtime_wall_clock.default_ceiling_s))
      else
      let timeout_s = current_timeout_s () in
      try
        with_optional_timeout clock timeout_s (fun () ->
          Eio.Buf_read.line reader)
        |> parse_wire_line
      with
      | End_of_file ->
        let detail = String.trim !stderr_tail in
        Error (Process_exited (if detail = "" then "stdout closed" else detail))
      | Idle_timeout seconds -> Error (Timeout seconds)
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | Eio.Time.Timeout as exn -> raise exn
      | exn -> protocol_error "stdout read" (Printexc.to_string exn)
    in
    Fun.protect
      ~finally:(fun () -> terminate_spawned_process ~clock proc stdin_w)
      (fun () ->
        let with_admission_timeout callback =
          with_optional_timeout clock (Some config.admission_timeout_s) callback
        in
        run_protocol
          { send; receive }
          ~dynamic_tools
          ~subscription
          ~session_mode
          ~session_id
          ~prompt
          ~images
          ~on_session_ready:(fun ~session_id ->
            with_admission_timeout (fun () -> on_session_ready ~session_id))
          ~on_turn_starting:(fun ~session_id ->
            with_admission_timeout (fun () -> on_turn_starting ~session_id))
          ~on_turn_started:(fun ~session_id ~turn_id ->
            with_admission_timeout (fun () -> on_turn_started ~session_id ~turn_id))
          ~on_stream_event
          ~turn_admitted))
  with
  | Idle_timeout seconds -> Error (Timeout seconds)
  | Eio.Time.Timeout as exn -> raise exn
;;

let validate_process_config config =
  if String.trim config.cli_path = ""
  then Error (Invalid_config "cli_path must not be empty")
  else if String.trim config.cwd = "" || Filename.is_relative config.cwd
  then Error (Invalid_config "cwd must be an absolute path")
  else if
    not (Float.is_finite config.admission_timeout_s)
    || config.admission_timeout_s <= 0.0
  then Error (Invalid_config "admission_timeout_s must be positive and finite")
  else if
    (match config.timeout_s with
     | None -> false
     | Some seconds -> not (Float.is_finite seconds) || seconds <= 0.0)
  then Error (Invalid_config "a declared timeout_s must be positive and finite")
  else Ok ()
;;

let validate_config config ~session_mode ~prompt =
  let* () = validate_process_config config in
  if String.trim prompt = ""
  then Error (Invalid_config "prompt must not be empty")
  else
    match session_mode with
    | Resume { session_id } when String.trim session_id = "" ->
      Error (Invalid_config "resume session_id must not be empty")
    | Start | Resume _ -> Ok ()
;;

(* Media types the stream-json image block accepts. Same closed set as the
   analyze_image tool schema and the dashboard composer, so a file the operator
   can attach is a file this transport can carry. *)
let supported_image_media_types =
  [ "image/png"; "image/jpeg"; "image/gif"; "image/webp" ]
;;

(* Fail closed before spawning the CLI. A malformed image reaches the provider
   as a 400 several seconds later, with the turn already started and the error
   attributed to the model rather than to the caller that built the block. *)
let validate_images images =
  let rec loop index = function
    | [] -> Ok ()
    | image :: rest ->
      let where = Printf.sprintf "images[%d]" index in
      if not (List.mem image.media_type supported_image_media_types)
      then
        Error
          (Invalid_config
             (Printf.sprintf
                "%s.media_type %S is not one of %s"
                where
                image.media_type
                (String.concat ", " supported_image_media_types)))
      else if String.trim image.base64_data = ""
      then Error (Invalid_config (where ^ ".base64_data must not be empty"))
      else if String.exists (fun c -> c = '\n' || c = '\r') image.base64_data
      then
        Error
          (Invalid_config
             (where ^ ".base64_data must not contain newlines"))
      else loop (index + 1) rest
  in
  loop 0 images
;;

let validate_dynamic_tools tools =
  let valid_tool_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  let rec loop seen = function
    | [] -> Ok ()
    | (tool : dynamic_tool) :: rest ->
      let name = String.trim tool.name in
      if name = ""
      then Error (Invalid_config "dynamic tool name must not be empty")
      else if List.mem name seen
      then
        Error
          (Invalid_config (Printf.sprintf "duplicate dynamic tool name %S" name))
      else if not (String.for_all valid_tool_character name)
      then
        Error
          (Invalid_config
             (Printf.sprintf
                "dynamic tool name %S contains a character unsupported by Claude Code allowedTools"
                name))
      else loop (name :: seen) rest
  in
  loop [] tools
;;

let validate_turn ?(dynamic_tools = []) ?(session_mode = Start) config ~prompt ~images =
  let* () = validate_config config ~session_mode ~prompt in
  let* () = validate_images images in
  validate_dynamic_tools dynamic_tools
;;

let bounded_subscription_probe_config
      ~fallback_timeout_s
      (turn_config : config)
  =
  match turn_config.timeout_s with
  | Some _ -> turn_config
  | None -> { turn_config with timeout_s = Some fallback_timeout_s }
;;

let probe_subscription ~mgr ~clock ~cwd config =
  let* () = validate_process_config config in
  let timeout_s = Some config.admission_timeout_s in
  try
    with_optional_timeout clock timeout_s (fun () ->
      read_subscription ~mgr ~cwd config)
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Idle_timeout seconds -> Error (Timeout seconds)
  | Eio.Time.Timeout as exn -> raise exn
;;

let run_turn ?(dynamic_tools = []) ?reasoning_effort ?(session_mode = Start)
    ?admitted_subscription ?on_spawned ~mgr ~clock ~cwd
    ?(on_session_ready = fun ~session_id:_ -> Ok ())
    ?(on_turn_starting = fun ~session_id:_ -> Ok ())
    ?(on_turn_started = fun ~session_id:_ ~turn_id:_ -> Ok ()) ?on_stream_event config
    ~prompt ~images =
  let result =
    let* () = validate_turn ~dynamic_tools ~session_mode config ~prompt ~images in
    let* _ = reasoning_args reasoning_effort in
    let* subscription =
      match admitted_subscription with
      | Some subscription -> Ok subscription
      | None ->
        let probe_config =
          bounded_subscription_probe_config
            ~fallback_timeout_s:default_timeout_s
            config
        in
        probe_subscription ~mgr ~clock ~cwd probe_config
    in
    let session_id =
      match session_mode with
      | Start -> Random_id.uuid_v7 ()
      | Resume { session_id } -> session_id
    in
    Log.Runtime_agent.info
      "Claude Code subscription turn starting (subscription_type=%s)"
      subscription.subscription_type;
    try
      run_spawned
        ?on_spawned
        ~mgr
        ~clock
        ~cwd
        config
        ~dynamic_tools
        ~reasoning_effort
        ~session_mode
        ~session_id
        ~subscription
        ~prompt
        ~images
        ~on_session_ready
        ~on_turn_starting
        ~on_turn_started
        ~on_stream_event
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | Idle_timeout seconds -> Error (Timeout seconds)
    | Eio.Time.Timeout as exn -> raise exn
    | Runtime_error error -> Error error
    | exn ->
      Error
        (Protocol_error
           { stage = "runtime boundary"; detail = Printexc.to_string exn })
  in
  (match result with
   | Ok turn ->
     Log.Runtime_agent.info
       "Claude Code subscription turn completed (session_id=%s turn_id=%s model=%s)"
       turn.session_id
       turn.turn_id
       turn.model
   | Error
       (Stopped_by_host
          { stop = Terminal_tool_boundary { outcome = Terminal_failed _; _ }; _ }
        as failed) ->
     (* A terminal tool that failed is a host stop the keeper settles as
        [Terminal_effect_failed]; it stays a warning. *)
     Log.Runtime_agent.warn
       "Claude Code subscription turn failed (kind=%s): %s"
       (error_kind failed)
       (error_to_string failed)
   | Error
       (Stopped_by_host
          { stop =
              ( Repeated_tool_call _
              | Terminal_tool_boundary
                  { outcome =
                      (Terminal_completed | Durable_stimulus_deferred)
                  ; _
                  } )
          ; _
          } as stop) ->
     (* The host ended the turn on purpose, at a terminal tool boundary or
        after a repeated tool call, and the keeper settles it as a completed
        or yielded turn. Fifty of these read as failures on 2026-09-02. *)
     Log.Runtime_agent.info
       "Claude Code subscription turn stopped by host: %s"
       (error_to_string stop)
   | Error error ->
     Log.Runtime_agent.warn
       "Claude Code subscription turn failed (kind=%s)"
       (error_kind error));
  result
;;
