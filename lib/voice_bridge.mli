(** MASC Voice Bridge - ElevenLabs Conversational AI Integration

    Enables multi-agent voice collaboration via turn-based speaking.
    Core constraint: "병렬 수집 → 순차 출력" (parallel collection → sequential output)

    @author Second Brain
    @since MASC v3.0
*)

(** {1 Types} *)

(** Voice session status returned from session operations *)
type voice_session_status = {
  session_id: string;
  agent_id: string;
  voice: string;
  is_active: bool;
  turn_count: int;
  duration_seconds: float option;
}

(** Voice conference status *)
type conference_status = {
  conference_id: string;
  state: string;  (** One of: idle, active, paused, ended *)
  participants: string list;
  current_speaker: string option;
  queue_size: int;
  turn_count: int;
}

(** Result of requesting a speaking turn *)
type turn_request_result = {
  status: string;
  agent_id: string;
  message_preview: string;
  voice: string;
  queue_position: int;
}

(** {1 Configuration} *)

(** Get the assigned voice for an agent.
    Returns the voice name from config or "Sarah" as default.

    Voice assignments can be configured via [$ME_ROOT/.masc/voice_config.json]:
    {[
      {
        "agent_voices": {
          "claude": "Sarah",
          "gemini": "Roger",
          "codex": "George"
        }
      }
    ]}
*)
val get_voice_for_agent : string -> string

(** {2 Configuration Accessors}

    These functions provide access to externalized configuration values.
    Configuration is lazy-loaded once from [$ME_ROOT/.masc/voice_config.json].
*)

(** Voice MCP server host. Default: "127.0.0.1" *)
val voice_mcp_host : unit -> string

(** Voice MCP server port. Default: 8936 *)
val voice_mcp_port : unit -> int

(** Request timeout in seconds. Default: 5.0 *)
val request_timeout_seconds : unit -> float

(** Maximum retry attempts. Default: 3 *)
val max_retries : unit -> int

(** Initial backoff delay in seconds. Default: 1.0 *)
val initial_backoff_seconds : unit -> float

(** Backoff multiplier for exponential backoff. Default: 2.0 *)
val backoff_multiplier : unit -> float

(** Get all agent voice assignments as (agent_id, voice_name) list *)
val agent_voices : unit -> (string * string) list

(** {2 Retry Logic} *)

(** Check if an error message indicates a retryable (transient) error.
    Retryable errors include:
    - Connection failures ("connection refused", "connection reset", etc.)
    - Timeouts ("timeout", "timed out", etc.)
    - HTTP 5xx errors ("http 500", "http 503", etc.)

    @param error The error message string
    @return [true] if the error is transient and retry is appropriate
*)
val is_retryable_error : string -> bool

(** {1 MCP Response Parsing} *)

(** Extract result data from MCP JSON-RPC response.
    Handles both success and error responses.

    @return [Ok json] with extracted result data
    @return [Error msg] if response indicates error or parse fails
*)
val extract_mcp_result : Yojson.Safe.t -> (Yojson.Safe.t, string) result

(** {1 Fallback & Health} *)

(** Generate a text-only fallback response when voice is unavailable.
    Use this to gracefully degrade when Voice MCP server is down.

    @param agent_id The agent generating the message
    @param message The text content
    @return JSON object with status="text_fallback"
*)
val text_fallback : agent_id:string -> message:string -> Yojson.Safe.t

(** Check if Voice MCP server is available.
    Result is cached for 30 seconds to avoid hammering the server.

    @return [true] if server responded to health check
*)
val is_voice_server_available : unit -> bool Lwt.t

(** Perform a health check on the Voice MCP server.

    @return JSON with status information
*)
val health_check : unit -> (Yojson.Safe.t, string) result Lwt.t

(** {1 Session Management} *)

(** Get voice configuration for an agent.

    @param agent_id Agent identifier
    @return JSON with agent_id, voice, and config_source
*)
val get_agent_voice : agent_id:string -> (Yojson.Safe.t, string) result Lwt.t

(** Start a voice session for an agent.

    @param agent_id Agent identifier
    @param session_name Optional session name for grouping
    @return Session status on success
*)
val start_voice_session :
  agent_id:string ->
  ?session_name:string ->
  unit ->
  (voice_session_status, string) result Lwt.t

(** End an active voice session.

    @param agent_id Agent whose session to end
    @return JSON response from Voice MCP or skip status
*)
val end_voice_session :
  agent_id:string ->
  (Yojson.Safe.t, string) result Lwt.t

(** List all active voice sessions.

    @return JSON with list of active sessions
*)
val list_voice_sessions : unit -> (Yojson.Safe.t, string) result Lwt.t

(** {1 Turn-Based Speaking} *)

(** Request a speaking turn for an agent.
    Messages are queued and played sequentially (no parallel speech).
    Falls back to text mode if voice server unavailable.

    @param agent_id Agent requesting to speak
    @param message Text to be spoken via TTS
    @param priority Optional priority (lower = higher priority, default 1)
    @return Turn request result with queue position
*)
val agent_speak :
  agent_id:string ->
  message:string ->
  ?priority:int ->
  unit ->
  (turn_request_result, string) result Lwt.t

(** {1 Conference Mode} *)

(** Start a multi-agent voice conference.
    Falls back to individual sessions if conference endpoint unavailable.

    @param agent_ids List of participating agent IDs
    @param conference_name Optional name for the conference
    @return Conference status on success
*)
val start_conference :
  agent_ids:string list ->
  ?conference_name:string ->
  unit ->
  (conference_status, string) result Lwt.t

(** End an active voice conference.

    @param agent_ids List of agent IDs in the conference
    @return JSON with conference status
*)
val end_conference :
  agent_ids:string list ->
  unit ->
  (Yojson.Safe.t, string) result Lwt.t

(** {1 Transcripts} *)

(** Get transcript of the current session or conference.

    @return JSON with transcript entries
*)
val get_transcript : unit -> (Yojson.Safe.t, string) result Lwt.t
