(** MASC Voice Session Manager - Multi-Agent Session Tracking

    Each agent can have one active voice session.
    Sessions are persisted to .masc/voice_sessions/ for recovery.

    @author Second Brain
    @since MASC v3.0
*)

(** {1 Types} *)

(** Session state *)
type session_status =
  | Active      (** Session is active and speaking *)
  | Idle        (** Session is idle, not speaking *)
  | Suspended   (** Session is temporarily suspended *)

(** Voice session for an agent *)
type session = {
  session_id: string;              (** Unique session ID *)
  agent_id: string;                (** Agent owning this session *)
  voice: string;                   (** Voice name for TTS *)
  started_at: float;               (** Unix timestamp when session started *)
  mutable last_activity: float;    (** Unix timestamp of last activity *)
  mutable turn_count: int;         (** Number of turns taken *)
  mutable status: session_status;  (** Current session status *)
}

(** Manager state (opaque) *)
type t

(** {1 Creation} *)

(** Create a new session manager.
    @param config_path Path to .masc directory
    @return A new session manager *)
val create : config_path:string -> t

(** {1 Session Lifecycle} *)

(** Start a voice session for an agent.
    If agent already has an active session, returns that session.

    @param t The manager
    @param agent_id Agent starting the session
    @param voice Voice name (defaults to agent's configured voice)
    @return The created or existing session *)
val start_session : t -> agent_id:string -> ?voice:string -> unit -> session

(** End a voice session for an agent.
    Removes the session from active sessions and cleanup file.

    @param t The manager
    @param agent_id Agent ending the session
    @return [true] if session was found and ended, [false] if no session *)
val end_session : t -> agent_id:string -> bool

(** Suspend a session temporarily.
    Session remains in memory but marked as suspended.

    @param t The manager
    @param agent_id Agent to suspend *)
val suspend_session : t -> agent_id:string -> unit

(** Resume a suspended session.

    @param t The manager
    @param agent_id Agent to resume *)
val resume_session : t -> agent_id:string -> unit

(** {1 Session Query} *)

(** Get session for an agent.

    @param t The manager
    @param agent_id Agent to look up
    @return [Some session] if found, [None] otherwise *)
val get_session : t -> agent_id:string -> session option

(** List all active sessions.

    @param t The manager
    @return List of all active sessions *)
val list_sessions : t -> session list

(** Check if agent has an active session.

    @param t The manager
    @param agent_id Agent to check
    @return [true] if agent has active session *)
val has_session : t -> agent_id:string -> bool

(** Count total active sessions.

    @param t The manager
    @return Number of active sessions *)
val session_count : t -> int

(** {1 Activity Tracking} *)

(** Update last activity timestamp for session.
    Should be called when agent speaks.

    @param t The manager
    @param agent_id Agent to update *)
val heartbeat : t -> agent_id:string -> unit

(** Increment turn count for session.
    Called when agent completes a turn.

    @param t The manager
    @param agent_id Agent to update *)
val increment_turn : t -> agent_id:string -> unit

(** {1 Zombie Cleanup} *)

(** Cleanup zombie sessions (inactive for too long).
    Sessions without activity for over [timeout] seconds are removed.

    @param t The manager
    @param timeout Seconds of inactivity before cleanup (default: 300)
    @return Number of sessions cleaned up *)
val cleanup_zombies : t -> ?timeout:float -> unit -> int

(** {1 Persistence} *)

(** Persist all sessions to disk.
    Saves to .masc/voice_sessions/

    @param t The manager *)
val persist : t -> unit

(** Load sessions from disk.
    Restores sessions from .masc/voice_sessions/

    @param t The manager *)
val restore : t -> unit

(** {1 Status} *)

(** Get manager status as JSON.

    @param t The manager
    @return JSON with session info *)
val status_json : t -> Yojson.Safe.t

(** Convert session to JSON.

    @param session The session
    @return JSON representation *)
val session_to_json : session -> Yojson.Safe.t

(** {1 Utilities} *)

(** Generate a unique session ID.
    @return UUID string *)
val generate_session_id : unit -> string

(** Convert session status to string.
    @param status The status
    @return String representation *)
val string_of_status : session_status -> string
