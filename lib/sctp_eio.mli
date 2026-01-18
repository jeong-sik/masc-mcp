(** RFC 4960 SCTP - Eio-Native Implementation with Thread-Safe State

    This module provides an Eio-native SCTP association with:
    - Eio.Mutex for thread-safe state access
    - Direct-style async (no Lwt monads)
    - Stream multiplexing support

    Example usage:
    {[
      Eio_main.run @@ fun _env ->

      (* Create SCTP association *)
      let assoc = Sctp_eio.create () in

      (* Register callbacks *)
      Sctp_eio.on_state_change assoc (fun state ->
        Printf.printf "State: %s\n" (Sctp.string_of_state state)
      );

      (* Establish association *)
      Sctp_eio.establish assoc;

      (* Open a stream *)
      let _stream = Sctp_eio.open_stream assoc 0 () in

      (* Create and encode DATA chunk *)
      let chunk = Sctp_eio.create_data_chunk assoc
        ~stream_id:0
        ~ppid:51l  (* WebRTC_String *)
        ~data:(Bytes.of_string "Hello, SCTP!")
        ()
      in
      let chunk_bytes = Sctp_eio.encode_data_chunk chunk in
      Printf.printf "Encoded %d bytes\n" (Bytes.length chunk_bytes)
    ]}

    @author Second Brain
    @since MASC v3.2
*)

(** {1 Types} *)

(** Thread-safe SCTP association *)
type t

(** {1 Creation} *)

(** Create a new thread-safe SCTP association.
    @param config Optional SCTP configuration
*)
val create : ?config:Sctp.config -> unit -> t

(** {1 State Access (Thread-Safe)} *)

(** Get association state *)
val get_state : t -> Sctp.state

(** Check if association is established *)
val is_established : t -> bool

(** Get association info as JSON *)
val association_info : t -> Yojson.Safe.t

(** Get my verification tag *)
val get_my_vtag : t -> int32

(** Get peer verification tag *)
val get_peer_vtag : t -> int32

(** Get next TSN *)
val get_next_tsn : t -> int32

(** Get last received TSN *)
val get_last_rcvd_tsn : t -> int32

(** Get congestion window size *)
val get_cwnd : t -> int

(** Get slow-start threshold *)
val get_ssthresh : t -> int

(** Get configuration *)
val get_config : t -> Sctp.config

(** {1 Stream Management (Thread-Safe)} *)

(** Open a stream.
    @param stream_id Stream identifier (0-65535)
    @param ordered Whether to use ordered delivery (default: true)
*)
val open_stream : t -> int -> ?ordered:bool -> unit -> Sctp.stream

(** Get stream by ID *)
val get_stream : t -> int -> Sctp.stream option

(** Close a stream *)
val close_stream : t -> int -> unit

(** Set association state (for testing/simulation) *)
val set_state : t -> Sctp.state -> unit

(** Get all streams *)
val get_streams : t -> Sctp.stream list

(** Get stream count *)
val stream_count : t -> int

(** {1 State Mutation (Thread-Safe)} *)

(** Establish association *)
val establish : t -> unit

(** Begin shutdown *)
val begin_shutdown : t -> unit

(** Set peer verification tag *)
val set_peer_vtag : t -> int32 -> unit

(** Increment next TSN and return current value *)
val advance_tsn : t -> int32

(** Update congestion window *)
val set_cwnd : t -> int -> unit

(** Update slow-start threshold *)
val set_ssthresh : t -> int -> unit

(** Update last received TSN *)
val update_last_rcvd_tsn : t -> int32 -> unit

(** {1 Data Encoding/Decoding (Pure)} *)

(** Encode DATA chunk *)
val encode_data_chunk : Sctp.data_chunk -> bytes

(** Decode DATA chunk *)
val decode_data_chunk : bytes -> (Sctp.data_chunk, string) result

(** Encode SCTP packet *)
val encode_packet : Sctp.packet -> bytes

(** Decode SCTP packet *)
val decode_packet : bytes -> (Sctp.packet, string) result

(** Check if data looks like SCTP *)
val is_sctp_data : bytes -> bool

(** Calculate checksum (CRC32-C) *)
val calculate_checksum : bytes -> int32

(** {1 Chunk Type Utilities} *)

val string_of_chunk_type : Sctp.chunk_type -> string
val chunk_type_of_int : int -> Sctp.chunk_type
val int_of_chunk_type : Sctp.chunk_type -> int

(** {1 State Utilities} *)

val string_of_state : Sctp.state -> string

(** {1 Message Type Utilities} *)

val string_of_message_type : Sctp.message_type -> string
val ppid_of_message_type : Sctp.message_type -> int32
val message_type_of_ppid : int32 -> Sctp.message_type option

(** {1 Event Callbacks} *)

(** Register callback for state changes *)
val on_state_change : t -> (Sctp.state -> unit) -> unit

(** Register callback for received data *)
val on_data_received : t -> (Sctp.data_chunk -> unit) -> unit

(** Register callback for stream opened *)
val on_stream_opened : t -> (int -> unit) -> unit

(** Register callback for stream closed *)
val on_stream_closed : t -> (int -> unit) -> unit

(** {1 Send/Receive Helpers} *)

(** Create a DATA chunk for sending.

    @param stream_id Stream identifier
    @param ppid Payload Protocol Identifier
    @param data User data to send
    @param ordered Whether to use ordered delivery (default: true)
*)
val create_data_chunk :
  t ->
  stream_id:int ->
  ppid:int32 ->
  data:bytes ->
  ?ordered:bool ->
  unit ->
  Sctp.data_chunk

(** Process incoming DATA chunk *)
val process_data_chunk : t -> Sctp.data_chunk -> unit

(** Create SCTP packet for sending *)
val create_packet : t -> Sctp.raw_chunk list -> Sctp.packet

(** {1 Concurrent Access Safety} *)

(** Safely iterate over all streams *)
val iter_streams : t -> (Sctp.stream -> unit) -> unit

(** Map over all streams *)
val map_streams : t -> (Sctp.stream -> 'a) -> 'a list

(** {1 Debug/Status} *)

(** Get association status as JSON *)
val status_json : t -> Yojson.Safe.t

(** Pretty print association status *)
val pp : Format.formatter -> t -> unit

(** Connection info for debugging *)
val connection_info : t -> Yojson.Safe.t
