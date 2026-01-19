(** MASC Voice Stream - Real-time Audio Delivery via WebSocket

    Provides low-latency audio streaming from TTS to connected clients.
    Uses httpun-ws-eio for persistent connections.

    Architecture:
    {v
    +--------------+     +--------------+     +-------------+
    | TTS Service  | --> | Voice Stream | --> | WS Clients  |
    | (ElevenLabs) |     | (OCaml/Eio)  |     | (Browser)   |
    +--------------+     +--------------+     +-------------+
    v}

    Protocol:
    - Client -> Server (text frames):
      - {v {"type": "subscribe", "agent_id": "claude"} v}
      - {v {"type": "unsubscribe"} v}
    - Server -> Client (text frames):
      - {v {"type": "speaking", "agent_id": "claude", "voice": "Sarah"} v}
      - {v {"type": "done", "agent_id": "claude", "duration_ms": 1500} v}
    - Server -> Client (binary frames):
      - Raw audio bytes (PCM/MP3)

    MAGI Review Applied (2026-01-10):
    - P0: clients List -> Hashtbl for O(1) lookup
    - P1: Send failure -> client removal + logging
    - P1: Backpressure mechanism (max pending sends)

    @author Second Brain
    @since MASC v3.0
*)

(** {1 Configuration} *)

(** Maximum pending sends per client before backpressure kicks in *)
val max_pending_sends : int

(** {1 Types} *)

(** Client connection state *)
type client = {
  id: string;                              (** Unique client ID *)
  mutable agent_filter: string option;     (** If set, only receives audio from this agent *)
  connected_at: float;                     (** Unix timestamp of connection *)
  mutable last_activity: float;            (** Last activity timestamp *)
  mutable pending_sends: int;              (** Backpressure: pending send count *)
}

(** Stream server state (opaque) *)
type t

(** Message type for client communication *)
type client_message =
  | Subscribe of string    (** Subscribe to specific agent *)
  | Unsubscribe            (** Unsubscribe from agent filter *)
  | Unknown of string      (** Unknown message *)

(** Server event for external notification *)
type server_event =
  | ClientConnected of client
  | ClientDisconnected of string  (** client_id *)
  | MessageReceived of string * client_message  (** client_id, message *)
  | ClientError of string * string  (** client_id, error_message - P1: Error events *)

(** {1 Creation} *)

(** Create a new voice stream server.

    @param port Port to listen on (default: 8937)
    @return A new stream server instance *)
val create : ?port:int -> unit -> t

(** {1 Server Lifecycle} *)

(** Start the WebSocket server.

    @param sw Eio switch controlling server lifetime
    @param net Eio network provider
    @param t The stream server *)
val start : sw:Eio.Switch.t -> net:[> `Generic] Eio.Net.t -> t -> unit

(** Stop the server and disconnect all clients.

    @param t The stream server *)
val stop : t -> unit

(** Check if server is running.

    @param t The stream server
    @return true if server is accepting connections *)
val is_running : t -> bool

(** {1 Broadcasting} *)

(** Broadcast audio chunk to all connected clients.
    Unhealthy clients are automatically removed after broadcast.

    @param t The stream server
    @param audio Raw audio bytes *)
val broadcast_audio : t -> bytes -> unit

(** Send audio to clients subscribed to a specific agent.
    Unhealthy clients are automatically removed after send.

    @param t The stream server
    @param agent_id Target agent
    @param audio Raw audio bytes *)
val send_to_agent_subscribers : t -> agent_id:string -> bytes -> unit

(** Broadcast text message to all clients.
    Used for sync messages, metadata, speaking notifications.

    @param t The stream server
    @param message Text message (typically JSON) *)
val broadcast_text : t -> string -> unit

(** Send speaking notification.
    {v {"type": "speaking", "agent_id": "...", "voice": "..."} v}

    @param t The stream server
    @param agent_id Agent who is speaking
    @param voice Voice name being used *)
val notify_speaking : t -> agent_id:string -> voice:string -> unit

(** Send done notification.
    {v {"type": "done", "agent_id": "...", "duration_ms": ...} v}

    @param t The stream server
    @param agent_id Agent who finished speaking
    @param duration_ms Duration in milliseconds (optional) *)
val notify_done : t -> agent_id:string -> ?duration_ms:int -> unit -> unit

(** {1 Client Management} *)

(** Get connected client count.

    @param t The stream server
    @return Number of connected clients *)
val client_count : t -> int

(** List all connected clients.

    @param t The stream server
    @return List of connected clients *)
val list_clients : t -> client list

(** Get client by ID.

    @param t The stream server
    @param client_id Client ID to look up
    @return [Some client] if found, [None] otherwise *)
val get_client : t -> client_id:string -> client option

(** Subscribe a client to a specific agent's audio.

    @param t The stream server
    @param client_id Client to update
    @param agent_id Agent to subscribe to *)
val subscribe_to_agent : t -> client_id:string -> agent_id:string -> unit

(** Unsubscribe a client from agent filtering.

    @param t The stream server
    @param client_id Client to update *)
val unsubscribe : t -> client_id:string -> unit

(** Disconnect a client.

    @param t The stream server
    @param client_id Client to disconnect *)
val disconnect_client : t -> client_id:string -> unit

(** Cleanup zombie clients (inactive for too long or unhealthy).

    @param t The stream server
    @param timeout Seconds of inactivity before cleanup (default: 300)
    @return Number of clients cleaned up *)
val cleanup_zombies : t -> ?timeout:float -> unit -> int

(** {1 Event Handling} *)

(** Set event callback for server events.
    Called when clients connect, disconnect, send messages, or encounter errors.

    @param t The stream server
    @param callback Function to call on events *)
val on_event : t -> (server_event -> unit) -> unit

(** {1 Status} *)

(** Get server status as JSON.
    Includes port, running state, client count, backpressure config, and client details.

    @param t The stream server
    @return JSON string *)
val get_status_json : t -> string
