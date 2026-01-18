(** Session Management - Eio Native Version

    Track connected agents and rate limiting using Eio primitives.
    Direct-style async with Eio.Mutex instead of Lwt_mutex.
*)

open Types

(** {1 Types} *)

(** Session info stored in registry *)
type session = {
  agent_name: string;
  connected_at: float;
  mutable last_activity: float;
  mutable is_listening: bool;
  mutable message_queue: Yojson.Safe.t list;
}

(** Rate limit tracking per category *)
type rate_tracker = {
  mutable general_timestamps: float list;
  mutable broadcast_timestamps: float list;
  mutable task_ops_timestamps: float list;
  mutable burst_used: int;
  mutable last_burst_reset: float;
}

(** Session registry *)
type registry

(** {1 Registry Management} *)

(** Create new registry *)
val create : ?config:rate_limit_config -> unit -> registry

(** Get rate limit config from registry *)
val get_config : registry -> rate_limit_config

(** {1 Session Operations - Eio Native} *)

(** Register a new session (direct-style, no Lwt.t) *)
val register : registry -> agent_name:string -> session

(** Unregister session *)
val unregister : registry -> agent_name:string -> unit

(** Update activity timestamp *)
val update_activity : registry -> agent_name:string -> ?is_listening:bool option -> unit -> unit

(** {1 Message Queue} *)

(** Push message to session queue *)
val push_message : registry -> from_agent:string -> content:string -> mention:string option -> string list

(** Pop message from queue *)
val pop_message : registry -> agent_name:string -> Yojson.Safe.t option

(** Wait for message with timeout (Eio blocking) *)
val wait_for_message : clock:_ Eio.Time.clock -> registry -> agent_name:string -> timeout:float -> Yojson.Safe.t option

(** {1 Rate Limiting} *)

(** Check rate limit - simple version *)
val check_rate_limit : registry -> agent_name:string -> bool * int

(** Check rate limit - extended with category and role *)
val check_rate_limit_ex : registry -> agent_name:string -> category:rate_limit_category -> role:agent_role -> bool * int

(** Get rate limit status *)
val get_rate_limit_status : registry -> agent_name:string -> role:agent_role -> Yojson.Safe.t

(** {1 Status and Queries} *)

(** Get inactive agents *)
val get_inactive_agents : registry -> threshold:float -> string list

(** Get all agent statuses *)
val get_agent_statuses : registry -> Yojson.Safe.t list

(** Format status for display *)
val status_string : registry -> string

(** Connected agent names *)
val connected_agents : registry -> string list

(** Restore sessions from disk *)
val restore_from_disk : registry -> agents_path:string -> unit

(** Find session by agent name *)
val find_opt : registry -> string -> session option

(** {1 MCP Session Store} *)

module McpSessionStore : sig
  type mcp_session = {
    id: string;
    created_at: float;
    mutable last_activity: float;
    mutable agent_name: string option;
    mutable metadata: (string * string) list;
    mutable request_count: int;
  }

  val generate_id : unit -> string
  val create : ?agent_name:string -> unit -> mcp_session
  val get : string -> mcp_session option
  val cleanup_stale : unit -> int
  val to_json : mcp_session -> Yojson.Safe.t
  val list_all : unit -> mcp_session list
  val remove : string -> bool
end

(** Extract MCP Session ID from HTTP headers *)
val extract_mcp_session_id : Cohttp.Header.t -> string option

(** Get or create MCP session from headers *)
val get_or_create_mcp_session : Cohttp.Header.t -> McpSessionStore.mcp_session

(** Add MCP session ID to response headers *)
val add_mcp_session_header : Cohttp.Header.t -> McpSessionStore.mcp_session -> Cohttp.Header.t

(** MCP tool handler for session management *)
val handle_mcp_session_tool : Yojson.Safe.t -> bool * string
