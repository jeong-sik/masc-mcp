(** RFC 8831/8832 - WebRTC Data Channels with Eio-Native SCTP Transport

    Production-ready DataChannel implementation integrating:
    - DCEP protocol (RFC 8832) with zstd compression
    - Thread-safe SCTP association (RFC 4960)
    - Eio-native async I/O

    Protocol Stack:
    {v
    Application
        │
    ┌───▼───────────────────┐
    │  Datachannel_eio      │  ← Channel management
    ├───────────────────────┤
    │  Datachannel          │  ← DCEP protocol
    │  (RFC 8832)           │
    ├───────────────────────┤
    │  Sctp_eio             │  ← Thread-safe SCTP
    │  (RFC 4960)           │
    ├───────────────────────┤
    │  DTLS                 │  ← Encryption
    │  (RFC 6347)           │
    └───────────────────────┘
    v}

    Example usage:
    {[
      Eio_main.run @@ fun _env ->

      (* Create DataChannel connection as offerer *)
      let config = Datachannel_eio.default_config ~is_offerer:true in
      let conn = Datachannel_eio.create ~config () in

      (* Register message callback *)
      Datachannel_eio.on_message conn (fun channel_id data ->
        Printf.printf "Channel %d: %s\n" channel_id (Bytes.to_string data)
      );

      (* Connect (establishes SCTP association) *)
      Datachannel_eio.connect conn;

      (* Create a channel *)
      match Datachannel_eio.create_channel conn ~label:"chat" () with
      | Ok _channel ->
        (* Send message *)
        let _ = Datachannel_eio.send_text conn ~channel_id:0 "Hello!" in
        ()
      | Error e ->
        Printf.printf "Error: %s\n" e
    ]}

    @author Second Brain
    @since MASC v3.3
*)

(** {1 Types} *)

(** DataChannel connection with integrated SCTP transport *)
type t

(** Configuration for DataChannel connection *)
type config = {
  dc_config: Datachannel.config;      (** DataChannel layer config *)
  sctp_config: Sctp.config;           (** SCTP layer config *)
  is_offerer: bool;                   (** True if this side creates offers *)
}

(** {1 Configuration} *)

(** Default configuration.
    @param is_offerer True for the side that initiates the connection *)
val default_config : is_offerer:bool -> config

(** {1 Creation and Lifecycle} *)

(** Create a new DataChannel connection.
    @param config Configuration (default: offerer mode) *)
val create : ?config:config -> unit -> t

(** Establish SCTP association and connect.
    Must be called before sending/receiving data. *)
val connect : t -> unit

(** Check if connection is established *)
val is_connected : t -> bool

(** Close connection and all channels *)
val close : t -> unit

(** {1 Channel Management} *)

(** Create a new data channel.

    @param label Human-readable channel name
    @param protocol Sub-protocol identifier (optional)
    @param channel_type Reliability mode (default: Reliable_ordered)
    @param priority Channel priority (default: 0)
    @param negotiated If true, skip DCEP negotiation (default: false)
    @param id Explicit stream ID (default: auto-assigned)

    Stream ID assignment:
    - Offerer gets even IDs: 0, 2, 4, ...
    - Answerer gets odd IDs: 1, 3, 5, ...

    @return Created channel or error message *)
val create_channel :
  t ->
  label:string ->
  ?protocol:string ->
  ?channel_type:Datachannel.channel_type ->
  ?priority:int ->
  ?negotiated:bool ->
  ?id:int ->
  unit ->
  (Datachannel.channel, string) result

(** Get channel by ID *)
val get_channel : t -> channel_id:int -> Datachannel.channel option

(** Find channel by label *)
val find_channel_by_label : t -> label:string -> Datachannel.channel option

(** Close a channel *)
val close_channel : t -> channel_id:int -> unit

(** Get all channels *)
val get_channels : t -> Datachannel.channel list

(** {1 Data Transfer} *)

(** Send text message on a channel.

    @param compress Enable zstd compression (default: false).
                    Compression is applied for messages > 128 bytes.
    @param channel_id Target channel
    @param text Message to send
    @return Ok () on success, Error message on failure *)
val send_text :
  ?compress:bool ->
  t ->
  channel_id:int ->
  string ->
  (unit, string) result

(** Send binary message on a channel.

    @param compress Enable zstd compression (default: false)
    @param channel_id Target channel
    @param data Binary data to send
    @return Ok () on success, Error message on failure *)
val send_binary :
  ?compress:bool ->
  t ->
  channel_id:int ->
  bytes ->
  (unit, string) result

(** {1 SCTP Integration} *)

(** Flush outgoing messages to SCTP DATA chunks.
    Returns encoded chunks ready to send over DTLS.

    Typical usage with DTLS:
    {[
      let chunks = Datachannel_eio.flush_outgoing conn in
      List.iter (fun chunk_bytes ->
        let encrypted = Dtls.encrypt dtls_ctx chunk_bytes in
        send_to_peer encrypted
      ) chunks
    ]} *)
val flush_outgoing : t -> bytes list

(** Create complete SCTP packet from outgoing data.
    Returns None if no data to send.

    This is preferred over [flush_outgoing] when you need
    a properly formatted SCTP packet with header. *)
val create_sctp_packet : t -> bytes option

(** Process incoming SCTP packet.
    Returns list of DataChannel events.

    Typical usage with DTLS:
    {[
      let decrypted = Dtls.decrypt dtls_ctx received_data in
      if Datachannel_eio.is_sctp_data decrypted then
        let events = Datachannel_eio.handle_sctp_packet conn decrypted in
        List.iter handle_event events
    ]} *)
val handle_sctp_packet : t -> bytes -> Datachannel.event list

(** Check if data looks like SCTP (for demultiplexing) *)
val is_sctp_data : bytes -> bool

(** {1 Event Callbacks} *)

(** Register callback for channel open events.
    Called when a new channel is opened (local or remote). *)
val on_channel_open : t -> (Datachannel.channel -> unit) -> unit

(** Register callback for channel close events *)
val on_channel_close : t -> (int -> unit) -> unit

(** Register callback for incoming messages.
    Callback receives (channel_id, message_data).
    Messages are automatically decompressed if zstd-encoded. *)
val on_message : t -> (int -> bytes -> unit) -> unit

(** Register callback for errors *)
val on_error : t -> (string -> unit) -> unit

(** {1 Status and Debugging} *)

(** Get connection status as JSON *)
val status_json : t -> Yojson.Safe.t

(** Pretty print connection status *)
val pp : Format.formatter -> t -> unit

(** {1 Advanced: Direct Access} *)

(** Get underlying SCTP association.
    Use for low-level SCTP operations. *)
val get_sctp : t -> Sctp_eio.t

(** Get underlying DataChannel connection.
    Use for direct DCEP operations. *)
val get_dc_conn : t -> Datachannel.connection
