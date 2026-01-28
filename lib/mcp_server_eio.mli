(** MCP Protocol Server Implementation - Eio Native

    Direct-style async MCP server using OCaml 5.x Effect Handlers.
    Provides same functionality as Mcp_server but Eio-native.

    Key differences from legacy version:
    - Direct-style async (no monads, no let-star)
    - Eio.Switch for structured concurrency
    - Eio.Buf_write for buffered I/O
    - Eio.Process for subprocess execution

    Implementation notes:
    - Core request handling is Eio-native
    - Tool implementations are direct-style Eio
*)

[@@@warning "-32"]  (* Suppress unused value warnings for re-exported helpers *)

(** {1 Types} *)

(** Server state - same as Mcp_server.server_state for compatibility *)
type server_state = Mcp_server.server_state

(** JSON-RPC request (re-exported for convenience) *)
type jsonrpc_request = Mcp_server.jsonrpc_request

(** {1 JSON-RPC Helpers (re-exported)} *)

val jsonrpc_request_of_yojson : Yojson.Safe.t -> (jsonrpc_request, string) result
val is_jsonrpc_v2 : Yojson.Safe.t -> bool
val is_jsonrpc_response : Yojson.Safe.t -> bool
val is_notification : jsonrpc_request -> bool
val get_id : jsonrpc_request -> Yojson.Safe.t
val is_valid_request_id : Yojson.Safe.t -> bool
val make_response : id:Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t
val make_error : ?data:Yojson.Safe.t -> id:Yojson.Safe.t -> int -> string -> Yojson.Safe.t
val protocol_version_from_params : Yojson.Safe.t option -> string
val normalize_protocol_version : string -> string
val validate_initialize_params : Yojson.Safe.t option -> (unit, string) result

(** JSON helper: field existence check (re-exported) *)
val has_field : string -> Yojson.Safe.t -> bool

(** JSON helper: get field as Yojson (re-exported) *)
val get_field : string -> Yojson.Safe.t -> Yojson.Safe.t option

(** {1 Network Context} *)

(** Type alias for generic Eio network capability *)
type eio_net = [`Generic] Eio.Net.ty Eio.Resource.t

(** Set the Eio network reference for Walph chain execution.
    Must be called from main_eio.ml during server initialization.
    Accepts any Eio.Net.t (Unix, Generic, etc.) and stores as generic.
    @param net Eio network capability *)
val set_net : _ Eio.Net.t -> unit

(** {1 State Management} *)

(** Create server state (synchronous, no effect)
    @param test_mode Optional flag (ignored, for API compatibility)
    @param base_path Base path for MASC data directory *)
val create_state : ?test_mode:bool -> base_path:string -> unit -> server_state

(** Create server state with Eio context - required for PostgresNative backend

    This variant provides the Eio environment needed for caqti-eio database
    connection pooling. Use this when running in an Eio context with PostgreSQL.

    @param sw Eio.Switch for structured concurrency
    @param env Caqti-compatible Eio environment (net, clock, mono_clock)
    @param proc_mgr Eio process manager for agent spawning
    @param fs Eio filesystem for file operations
    @param clock Eio time clock for timestamps/sleep
    @param base_path Base path for MASC data directory *)
val create_state_eio :
  sw:Eio.Switch.t ->
  env:Caqti_eio.stdenv ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  base_path:string ->
  server_state

(** {1 Request Handling - Eio Native} *)

(** Handle incoming JSON-RPC request string (Eio direct-style)

    This is the main entry point for Eio-native request handling.
    Uses execute_tool_eio for tool calls.

    @param clock Eio time clock for Session_eio timeout operations
    @param sw Eio.Switch for structured concurrency
    @param mcp_session_id Optional HTTP MCP session ID for agent_name persistence
    @param state Server state
    @param request_str Raw JSON-RPC request string
    @return JSON response *)
val handle_request : clock:_ Eio.Time.clock -> sw:Eio.Switch.t -> ?mcp_session_id:string -> server_state -> string -> Yojson.Safe.t

(** Execute a single tool by name (for REST API)
    @return (success, result_json_string) *)
val execute_tool_eio :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?mcp_session_id:string ->
  server_state ->
  name:string ->
  arguments:Yojson.Safe.t ->
  bool * string

(** {1 Stdio Transport - Eio Native} *)

(** Run MCP server in stdio mode with Eio

    Reads JSON-RPC requests from stdin, writes responses to stdout.
    Uses length-prefixed framing (Content-Length header).

    @param sw Eio.Switch for structured concurrency
    @param env Eio environment (for stdin/stdout)
    @param state Server state *)
val run_stdio : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> server_state -> unit

(** {1 Protocol Helpers - Re-exported} *)

(** Check if JSON is valid JSON-RPC 2.0 *)
val is_jsonrpc_v2 : Yojson.Safe.t -> bool

(** Check if request is a notification (no id) *)
val is_notification : jsonrpc_request -> bool

(** Parse JSON-RPC request from JSON *)
val jsonrpc_request_of_yojson : Yojson.Safe.t -> (jsonrpc_request, string) result

(** Extract protocol version from initialize params *)
val protocol_version_from_params : Yojson.Safe.t option -> string

(** Normalize protocol version to supported version *)
val normalize_protocol_version : string -> string

(** {1 Response Builders} *)

(** Make successful JSON-RPC response *)
val make_response : id:Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t

(** Make error JSON-RPC response *)
val make_error : ?data:Yojson.Safe.t -> id:Yojson.Safe.t -> int -> string -> Yojson.Safe.t

(** {1 Protocol Detection} *)

(** Transport mode for stdio transport *)
type transport_mode = Framed | LineDelimited

(** Detect transport mode from first line of input.
    If line starts with "Content-Length" (case-insensitive), returns Framed.
    Otherwise returns LineDelimited. *)
val detect_mode : string -> transport_mode

(** {1 Governance} *)

(** Governance configuration *)
type governance_config = {
  level: string;
  audit_enabled: bool;
  anomaly_detection: bool;
}

(** Get default governance config for a given level.
    - "development" (default): audit=false, anomaly=false
    - "production": audit=true, anomaly=false
    - "enterprise"/"paranoid": audit=true, anomaly=true *)
val governance_defaults : string -> governance_config

(** {1 Audit Events} *)

(** Audit event record *)
type audit_event = {
  timestamp: float;
  agent: string;
  event_type: string;
  success: bool;
  detail: string option;
}

(** Serialize audit event to JSON *)
val audit_event_to_json : audit_event -> Yojson.Safe.t

(** {1 MCP Sessions} *)

(** MCP session record for HTTP session persistence *)
type mcp_session_record = {
  id: string;
  agent_name: string option;
  created_at: float;
  last_seen: float;
}

(** Serialize MCP session to JSON *)
val mcp_session_to_json : mcp_session_record -> Yojson.Safe.t

(** Deserialize MCP session from JSON *)
val mcp_session_of_json : Yojson.Safe.t -> mcp_session_record option
