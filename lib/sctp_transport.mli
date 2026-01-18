(** SCTP Transport with Flow Control - RFC 4960 Compliant

    Provides reliable, flow-controlled data transfer over UDP using SCTP.
    Key features:
    - Congestion window (cwnd) based flow control
    - In-flight data tracking
    - Automatic pacing to prevent buffer overflow
    - Selective acknowledgment (SACK) support

    @author Second Brain
    @since MASC v3.4
*)

(** Transport configuration *)
type config = {
  initial_cwnd: int;      (** Initial congestion window (bytes) *)
  max_cwnd: int;          (** Maximum congestion window *)
  initial_rto: float;     (** Initial retransmission timeout (seconds) *)
  min_rto: float;         (** Minimum RTO *)
  max_rto: float;         (** Maximum RTO *)
  max_burst: int;         (** Maximum packets per burst *)
}

(** Default configuration *)
val default_config : config

(** Transport state (opaque) *)
type t

(** Create transport with optional config *)
val create : ?config:config -> unit -> t

(** Check if we can send more data within cwnd *)
val can_send : t -> bool

(** Available send window in bytes *)
val available_window : t -> int

(** Send data with flow control.
    Returns immediately if window allows, otherwise buffers.
    @param stream_id SCTP stream identifier
    @param data Data to send
    @return true if sent immediately, false if buffered *)
val send : t -> stream_id:int -> data:bytes -> bool

(** Process acknowledgment.
    Updates cwnd based on slow-start or congestion avoidance.
    @param cum_tsn Cumulative TSN acknowledged
    @return Number of bytes acknowledged *)
val process_ack : t -> cum_tsn:int32 -> int

(** Handle timeout - reduce cwnd (congestion detected) *)
val handle_timeout : t -> unit

(** Try to send buffered data
    @return Number of chunks sent from buffer *)
val flush_buffer : t -> int

(** Transport statistics *)
type stats = {
  cwnd: int;
  ssthresh: int;
  in_flight_bytes: int;
  in_flight_count: int;
  buffered_bytes: int;
  buffered_count: int;
  rto: float;
  srtt: float;
}

(** Get current transport statistics *)
val get_stats : t -> stats

(** Pretty-print statistics *)
val pp_stats : Format.formatter -> stats -> unit

(** Convert statistics to JSON *)
val stats_to_json : stats -> Yojson.Safe.t
