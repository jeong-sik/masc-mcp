(** OCaml 5.x Eio-based UDP Socket for WebRTC

    Pure OCaml 5.x implementation using:
    - Eio for direct-style async (no Lwt monads)
    - Fibers for lightweight concurrency
    - Effect handlers (implicit via Eio)

    This module provides real UDP networking for ICE connectivity checks,
    STUN transactions, and DTLS/SCTP data channel traffic.

    @author Second Brain
    @since MASC v3.2 (OCaml 5.x)
*)

(** {1 Types} *)

(** UDP socket configuration *)
type config = {
  bind_addr: string;      (** Local address to bind ("0.0.0.0" for any) *)
  bind_port: int;         (** Local port (0 for auto-assign) *)
  recv_buffer_size: int;  (** Receive buffer size in bytes *)
  send_buffer_size: int;  (** Send buffer size in bytes *)
}

(** Socket state *)
type state =
  | Unbound      (** Created but not bound *)
  | Bound        (** Bound to local address *)
  | Connected    (** Associated with remote peer *)
  | Closed       (** Socket closed *)

(** Remote endpoint *)
type endpoint = {
  addr: string;
  port: int;
}

(** Received datagram *)
type datagram = {
  data: bytes;
  source: endpoint;
  timestamp: float;
}

(** Socket statistics *)
type stats = {
  packets_sent: int;
  packets_recv: int;
  bytes_sent: int;
  bytes_recv: int;
  errors: int;
}

(** UDP socket handle *)
type t

(** {1 Configuration} *)

(** Default configuration *)
val default_config : config

(** {1 Socket Lifecycle} *)

(** Create a new UDP socket (within Eio environment).
    @param net Eio network capability
    @param sw Switch for resource management
    @param config Socket configuration
    @return New socket handle *)
val create : net:Eio_unix.Net.t -> sw:Eio.Switch.t -> ?config:config -> unit -> t

(** Bind socket to local address.
    @param socket Socket to bind
    @return Actual bound endpoint *)
val bind : t -> endpoint

(** Connect socket to remote peer (for send without explicit address).
    @param socket Socket
    @param remote Remote endpoint *)
val connect : t -> endpoint -> unit

(** Close socket and release resources *)
val close : t -> unit

(** {1 Data Transfer} *)

(** Send datagram to specific endpoint.
    @param socket Socket
    @param data Data to send
    @param dest Destination endpoint
    @return Number of bytes sent *)
val send_to : t -> bytes -> endpoint -> int

(** Send datagram to connected peer.
    @param socket Connected socket
    @param data Data to send
    @return Number of bytes sent
    @raise Invalid_argument if socket not connected *)
val send : t -> bytes -> int

(** Receive datagram (blocks until data available).
    @param socket Socket
    @return Received datagram *)
val recv : t -> datagram

(** Zero-copy receive - returns Cstruct slice directly from internal buffer.
    WARNING: Data is only valid until next recv call!
    @param socket Socket
    @return (data_slice, source_endpoint) *)
val recv_zerocopy : t -> (Cstruct.t * endpoint)

(** Receive with timeout.
    @param socket Socket
    @param timeout_sec Timeout in seconds
    @return Some datagram or None on timeout *)
val recv_timeout : t -> float -> datagram option

(** {1 Concurrent Operations} *)

(** Run receive loop in a fiber, calling handler for each datagram.
    @param socket Socket
    @param handler Callback for received datagrams
    @return Fiber handle for cancellation *)
val recv_loop : t -> (datagram -> unit) -> unit

(** {1 State and Statistics} *)

(** Get current socket state *)
val get_state : t -> state

(** Get local bound endpoint *)
val local_endpoint : t -> endpoint option

(** Get connected remote endpoint *)
val remote_endpoint : t -> endpoint option

(** Get socket statistics *)
val get_stats : t -> stats

(** Get statistics as JSON *)
val stats_to_json : stats -> Yojson.Safe.t

(** {1 STUN/ICE Integration} *)

(** Send STUN binding request and wait for response.
    @param socket Socket
    @param server STUN server endpoint
    @param timeout_sec Timeout in seconds
    @return STUN response bytes or error *)
val stun_request : t -> endpoint -> float -> (bytes, string) result

(** {1 Pretty Printing} *)

val string_of_state : state -> string
val string_of_endpoint : endpoint -> string
val pp_datagram : Format.formatter -> datagram -> unit
val pp_stats : Format.formatter -> stats -> unit
