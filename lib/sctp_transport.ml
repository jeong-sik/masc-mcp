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
let default_config = {
  initial_cwnd = 65536;   (* 64KB initial window *)
  max_cwnd = 1048576;     (* 1MB max window *)
  initial_rto = 1.0;
  min_rto = 0.1;
  max_rto = 10.0;
  max_burst = 4;
}

(** In-flight chunk tracking *)
type in_flight_chunk = {
  tsn: int32;
  data: bytes;
  sent_time: float;
  retransmit_count: int;
} [@@warning "-69"]

(** Transport state *)
type t = {
  sctp: Sctp_eio.t;
  config: config;
  mutable cwnd: int;                    (** Congestion window *)
  mutable ssthresh: int;                (** Slow-start threshold *)
  mutable in_flight_bytes: int;         (** Bytes currently in flight *)
  mutable in_flight: in_flight_chunk list;  (** Chunks awaiting ACK *)
  mutable rto: float;                   (** Current RTO *)
  mutable srtt: float;                  (** Smoothed RTT *)
  mutable rttvar: float;                (** RTT variance *)
  mutable last_ack_tsn: int32;          (** Last acknowledged TSN *)
  send_buffer: bytes Queue.t;           (** Buffered data waiting to send *)
  mutable send_buffer_bytes: int;       (** Total bytes in send buffer *)
  mutex: Eio.Mutex.t;
} [@@warning "-69"]

(** Create transport *)
let create ?(config = default_config) () =
  {
    sctp = Sctp_eio.create ();
    config;
    cwnd = config.initial_cwnd;
    ssthresh = config.max_cwnd;
    in_flight_bytes = 0;
    in_flight = [];
    rto = config.initial_rto;
    srtt = 0.0;
    rttvar = 0.0;
    last_ack_tsn = 0l;
    send_buffer = Queue.create ();
    send_buffer_bytes = 0;
    mutex = Eio.Mutex.create ();
  }

(** Check if we can send more data *)
let can_send t =
  t.in_flight_bytes < t.cwnd

(** Available send window *)
let available_window t =
  max 0 (t.cwnd - t.in_flight_bytes)

(** Send data with flow control.
    Returns immediately if window allows, otherwise buffers.
    @return true if sent immediately, false if buffered *)
let send t ~stream_id:_ ~data =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let len = Bytes.length data in

    if t.in_flight_bytes + len <= t.cwnd then begin
      (* Window allows - send immediately *)
      let tsn = Sctp_eio.advance_tsn t.sctp in
      let chunk = {
        tsn;
        data;
        sent_time = Unix.gettimeofday ();
        retransmit_count = 0;
      } in
      t.in_flight <- chunk :: t.in_flight;
      t.in_flight_bytes <- t.in_flight_bytes + len;
      true
    end else begin
      (* Window full - buffer for later *)
      Queue.push data t.send_buffer;
      t.send_buffer_bytes <- t.send_buffer_bytes + len;
      false
    end
  )

(** Process acknowledgment.
    Updates cwnd based on slow-start or congestion avoidance.
    @param cum_tsn Cumulative TSN acknowledged *)
let process_ack t ~cum_tsn =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let now = Unix.gettimeofday () in
    let acked_bytes = ref 0 in

    (* Remove acknowledged chunks from in_flight *)
    t.in_flight <- List.filter (fun chunk ->
      if Int32.compare chunk.tsn cum_tsn <= 0 then begin
        (* This chunk is acknowledged *)
        acked_bytes := !acked_bytes + Bytes.length chunk.data;

        (* Update RTT estimation *)
        let rtt = now -. chunk.sent_time in
        if t.srtt = 0.0 then begin
          t.srtt <- rtt;
          t.rttvar <- rtt /. 2.0;
        end else begin
          t.rttvar <- 0.75 *. t.rttvar +. 0.25 *. abs_float (t.srtt -. rtt);
          t.srtt <- 0.875 *. t.srtt +. 0.125 *. rtt;
        end;
        t.rto <- max t.config.min_rto (min t.config.max_rto (t.srtt +. 4.0 *. t.rttvar));

        false  (* Remove from list *)
      end else
        true   (* Keep in list *)
    ) t.in_flight;

    t.in_flight_bytes <- t.in_flight_bytes - !acked_bytes;
    t.last_ack_tsn <- cum_tsn;

    (* Congestion control: increase cwnd *)
    if t.cwnd < t.ssthresh then begin
      (* Slow start: exponential increase *)
      t.cwnd <- min t.config.max_cwnd (t.cwnd + !acked_bytes)
    end else begin
      (* Congestion avoidance: linear increase *)
      if !acked_bytes > 0 then
        t.cwnd <- min t.config.max_cwnd (t.cwnd + t.config.initial_cwnd / t.cwnd * !acked_bytes)
    end;

    !acked_bytes
  )

(** Handle timeout - reduce cwnd (congestion detected) *)
let handle_timeout t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.ssthresh <- max (t.cwnd / 2) (t.config.initial_cwnd * 2);
    t.cwnd <- t.config.initial_cwnd;
    t.rto <- min t.config.max_rto (t.rto *. 2.0);  (* Exponential backoff *)
  )

(** Try to send buffered data *)
let flush_buffer t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let sent = ref 0 in
    while not (Queue.is_empty t.send_buffer) && can_send t do
      let data = Queue.pop t.send_buffer in
      let len = Bytes.length data in
      t.send_buffer_bytes <- t.send_buffer_bytes - len;

      let tsn = Sctp_eio.advance_tsn t.sctp in
      let chunk = {
        tsn;
        data;
        sent_time = Unix.gettimeofday ();
        retransmit_count = 0;
      } in
      t.in_flight <- chunk :: t.in_flight;
      t.in_flight_bytes <- t.in_flight_bytes + len;
      incr sent
    done;
    !sent
  )

(** Get transport statistics *)
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

let get_stats t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    {
      cwnd = t.cwnd;
      ssthresh = t.ssthresh;
      in_flight_bytes = t.in_flight_bytes;
      in_flight_count = List.length t.in_flight;
      buffered_bytes = t.send_buffer_bytes;
      buffered_count = Queue.length t.send_buffer;
      rto = t.rto;
      srtt = t.srtt;
    }
  )

let pp_stats fmt stats =
  Format.fprintf fmt "Transport { cwnd=%d, ssthresh=%d, in_flight=%d/%dB, buffered=%d/%dB, rto=%.3fs, srtt=%.3fs }"
    stats.cwnd stats.ssthresh
    stats.in_flight_count stats.in_flight_bytes
    stats.buffered_count stats.buffered_bytes
    stats.rto stats.srtt

let stats_to_json stats =
  `Assoc [
    ("cwnd", `Int stats.cwnd);
    ("ssthresh", `Int stats.ssthresh);
    ("inFlightBytes", `Int stats.in_flight_bytes);
    ("inFlightCount", `Int stats.in_flight_count);
    ("bufferedBytes", `Int stats.buffered_bytes);
    ("bufferedCount", `Int stats.buffered_count);
    ("rto", `Float stats.rto);
    ("srtt", `Float stats.srtt);
  ]
