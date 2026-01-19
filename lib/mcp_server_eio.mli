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

(** {1 Types} *)

(** Server state - same as Mcp_server.server_state for compatibility *)
type server_state = Mcp_server.server_state

(** JSON-RPC request (re-exported for convenience) *)
type jsonrpc_request = Mcp_server.jsonrpc_request

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
    @param base_path Base path for MASC data directory *)
val create_state_eio : sw:Eio.Switch.t -> env:Caqti_eio.stdenv -> base_path:string -> server_state

(** {1 Request Handling - Eio Native} *)

(** Handle incoming JSON-RPC request string (Eio direct-style)

    This is the main entry point for Eio-native request handling.
    Uses execute_tool_eio for tool calls.

    @param clock Eio time clock for Session_eio timeout operations
    @param sw Eio.Switch for structured concurrency
    @param state Server state
    @param request_str Raw JSON-RPC request string
    @return JSON response *)
val handle_request : clock:_ Eio.Time.clock -> sw:Eio.Switch.t -> server_state -> string -> Yojson.Safe.t

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
val make_error : id:Yojson.Safe.t -> int -> string -> Yojson.Safe.t
