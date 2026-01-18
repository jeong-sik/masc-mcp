(** RFC 4960 - Stream Control Transmission Protocol (SCTP)

    Pure OCaml implementation of SCTP for WebRTC data channels.

    SCTP provides:
    - Reliable, in-order delivery (configurable per stream)
    - Multiple streams within a single association
    - Message-oriented (not byte-stream)
    - Congestion control

    For WebRTC, SCTP runs over DTLS (RFC 8261).

    Reference: https://datatracker.ietf.org/doc/html/rfc4960
*)

(** {1 Types} *)

(** SCTP Chunk Types - RFC 4960 Section 3.2 *)
type chunk_type =
  | DATA          (** Payload data *)
  | INIT          (** Initiate association *)
  | INIT_ACK      (** Acknowledge INIT *)
  | SACK          (** Selective acknowledgment *)
  | HEARTBEAT     (** Heartbeat request *)
  | HEARTBEAT_ACK (** Heartbeat acknowledgment *)
  | ABORT         (** Abort association *)
  | SHUTDOWN      (** Shutdown association *)
  | SHUTDOWN_ACK  (** Acknowledge shutdown *)
  | ERROR         (** Error indication *)
  | COOKIE_ECHO   (** State cookie *)
  | COOKIE_ACK    (** Cookie acknowledgment *)
  | SHUTDOWN_COMPLETE
  | FORWARD_TSN   (** RFC 3758 - Partial reliability *)
  | RE_CONFIG     (** RFC 6525 - Stream reconfiguration *)
  | Unknown of int
[@@deriving show, eq]

(** Association states - RFC 4960 Section 4 *)
type state =
  | Closed
  | Cookie_wait
  | Cookie_echoed
  | Established
  | Shutdown_pending
  | Shutdown_sent
  | Shutdown_received
  | Shutdown_ack_sent
[@@deriving show, eq]

(** WebRTC Data Channel PPID message types - RFC 8831 *)
type message_type =
  | WebRTC_DCEP           (** 50 - Data Channel Establishment Protocol *)
  | WebRTC_String         (** 51 - UTF-8 string *)
  | WebRTC_Binary         (** 53 - Binary data *)
  | WebRTC_String_Empty   (** 56 - Empty string *)
  | WebRTC_Binary_Empty   (** 57 - Empty binary *)
[@@deriving show, eq]

(** Data chunk flags *)
type data_flags = {
  end_fragment: bool;     (** E bit - last fragment *)
  begin_fragment: bool;   (** B bit - first fragment *)
  unordered: bool;        (** U bit - unordered delivery *)
  immediate: bool;        (** I bit - immediate transmission *)
}

(** DATA chunk structure *)
type data_chunk = {
  flags: data_flags;
  tsn: int32;             (** Transmission Sequence Number *)
  stream_id: int;         (** Stream identifier *)
  stream_seq: int;        (** Stream sequence number *)
  ppid: int32;            (** Payload Protocol Identifier *)
  user_data: bytes;
}

(** Gap Ack Block - RFC 4960 Section 3.3.4 *)
type gap_ack_block = {
  gap_start: int;         (** Start offset from cumulative TSN *)
  gap_end: int;           (** End offset from cumulative TSN *)
}

(** SACK chunk structure - RFC 4960 Section 3.3.4 *)
type sack_chunk = {
  cumulative_tsn: int32;  (** Highest TSN received in sequence *)
  a_rwnd: int32;          (** Advertised Receiver Window Credit *)
  gap_ack_blocks: gap_ack_block list;
  duplicate_tsns: int32 list;
}

(** SCTP packet header - RFC 4960 Section 3 *)
type packet_header = {
  source_port: int;
  dest_port: int;
  verification_tag: int32;
  checksum: int32;
}

(** Raw chunk for encoding/decoding *)
type raw_chunk = {
  chunk_type: int;
  chunk_flags: int;
  chunk_length: int;
  chunk_value: bytes;
}

(** SCTP Packet *)
type packet = {
  header: packet_header;
  chunks: raw_chunk list;
}

(** Stream within an association *)
type stream = {
  id: int;
  ordered: bool;
  mutable next_ssn: int;
  mutable next_tsn: int32;
}

(** SCTP configuration *)
type config = {
  local_port: int;
  remote_port: int;
  mtu: int;                   (** Maximum Transmission Unit *)
  max_retransmits: int;       (** Max retransmission attempts *)
  rto_initial_ms: int;        (** Initial RTO *)
  rto_min_ms: int;
  rto_max_ms: int;
  a_rwnd: int;                (** Advertised Receiver Window Credit *)
  num_outbound_streams: int;
  num_inbound_streams: int;
}

(** SCTP Association *)
type association = {
  mutable state: state;
  streams: (int, stream) Hashtbl.t;
  config: config;
  mutable my_vtag: int32;
  mutable peer_vtag: int32;
  mutable next_tsn: int32;
  mutable last_rcvd_tsn: int32;
  mutable cwnd: int;          (** Congestion window *)
  mutable ssthresh: int;      (** Slow start threshold *)
}

(** {1 Default Configuration} *)

let default_config = {
  local_port = 5000;
  remote_port = 5000;
  mtu = 1200;
  max_retransmits = 10;
  rto_initial_ms = 3000;
  rto_min_ms = 1000;
  rto_max_ms = 60000;
  a_rwnd = 65536;
  num_outbound_streams = 65535;
  num_inbound_streams = 65535;
}

(** {1 String Conversions} *)

let string_of_chunk_type = function
  | DATA -> "DATA"
  | INIT -> "INIT"
  | INIT_ACK -> "INIT_ACK"
  | SACK -> "SACK"
  | HEARTBEAT -> "HEARTBEAT"
  | HEARTBEAT_ACK -> "HEARTBEAT_ACK"
  | ABORT -> "ABORT"
  | SHUTDOWN -> "SHUTDOWN"
  | SHUTDOWN_ACK -> "SHUTDOWN_ACK"
  | ERROR -> "ERROR"
  | COOKIE_ECHO -> "COOKIE_ECHO"
  | COOKIE_ACK -> "COOKIE_ACK"
  | SHUTDOWN_COMPLETE -> "SHUTDOWN_COMPLETE"
  | FORWARD_TSN -> "FORWARD_TSN"
  | RE_CONFIG -> "RE_CONFIG"
  | Unknown n -> Printf.sprintf "Unknown(%d)" n

let chunk_type_of_int = function
  | 0 -> DATA
  | 1 -> INIT
  | 2 -> INIT_ACK
  | 3 -> SACK
  | 4 -> HEARTBEAT
  | 5 -> HEARTBEAT_ACK
  | 6 -> ABORT
  | 7 -> SHUTDOWN
  | 8 -> SHUTDOWN_ACK
  | 9 -> ERROR
  | 10 -> COOKIE_ECHO
  | 11 -> COOKIE_ACK
  | 14 -> SHUTDOWN_COMPLETE
  | 192 -> FORWARD_TSN
  | 130 -> RE_CONFIG
  | n -> Unknown n

let int_of_chunk_type = function
  | DATA -> 0
  | INIT -> 1
  | INIT_ACK -> 2
  | SACK -> 3
  | HEARTBEAT -> 4
  | HEARTBEAT_ACK -> 5
  | ABORT -> 6
  | SHUTDOWN -> 7
  | SHUTDOWN_ACK -> 8
  | ERROR -> 9
  | COOKIE_ECHO -> 10
  | COOKIE_ACK -> 11
  | SHUTDOWN_COMPLETE -> 14
  | FORWARD_TSN -> 192
  | RE_CONFIG -> 130
  | Unknown n -> n

let string_of_state = function
  | Closed -> "closed"
  | Cookie_wait -> "cookie-wait"
  | Cookie_echoed -> "cookie-echoed"
  | Established -> "established"
  | Shutdown_pending -> "shutdown-pending"
  | Shutdown_sent -> "shutdown-sent"
  | Shutdown_received -> "shutdown-received"
  | Shutdown_ack_sent -> "shutdown-ack-sent"

let string_of_message_type = function
  | WebRTC_DCEP -> "DCEP"
  | WebRTC_String -> "String"
  | WebRTC_Binary -> "Binary"
  | WebRTC_String_Empty -> "String_Empty"
  | WebRTC_Binary_Empty -> "Binary_Empty"

(** {1 PPID Conversions - RFC 8831} *)

let ppid_of_message_type = function
  | WebRTC_DCEP -> 50l
  | WebRTC_String -> 51l
  | WebRTC_Binary -> 53l
  | WebRTC_String_Empty -> 56l
  | WebRTC_Binary_Empty -> 57l

let message_type_of_ppid = function
  | 50l -> Some WebRTC_DCEP
  | 51l -> Some WebRTC_String
  | 53l -> Some WebRTC_Binary
  | 56l -> Some WebRTC_String_Empty
  | 57l -> Some WebRTC_Binary_Empty
  | _ -> None

(** {1 Association Management} *)

(** Create new SCTP association *)
let create config =
  Random.self_init ();
  {
    state = Closed;
    streams = Hashtbl.create 16;
    config;
    my_vtag = Random.int32 Int32.max_int;
    peer_vtag = 0l;
    next_tsn = Random.int32 Int32.max_int;
    last_rcvd_tsn = 0l;
    cwnd = config.mtu * 4;
    ssthresh = config.a_rwnd;
  }

(** Get association state *)
let get_state assoc = assoc.state

(** Check if association is established *)
let is_established assoc = assoc.state = Established

(** Get association info as JSON *)
let association_info assoc =
  `Assoc [
    ("state", `String (string_of_state assoc.state));
    ("streams", `Int (Hashtbl.length assoc.streams));
    ("myVtag", `String (Int32.to_string assoc.my_vtag));
    ("peerVtag", `String (Int32.to_string assoc.peer_vtag));
    ("nextTsn", `String (Int32.to_string assoc.next_tsn));
    ("cwnd", `Int assoc.cwnd);
    ("ssthresh", `Int assoc.ssthresh);
  ]

(** {1 Stream Management} *)

(** Open a stream *)
let open_stream assoc stream_id ?(ordered=true) () =
  let stream = {
    id = stream_id;
    ordered;
    next_ssn = 0;
    next_tsn = assoc.next_tsn;
  } in
  Hashtbl.replace assoc.streams stream_id stream;
  stream

(** Get stream by ID *)
let get_stream assoc stream_id =
  Hashtbl.find_opt assoc.streams stream_id

(** Close a stream *)
let close_stream assoc stream_id =
  Hashtbl.remove assoc.streams stream_id

(** Get all streams *)
let get_streams assoc =
  Hashtbl.fold (fun _ stream acc -> stream :: acc) assoc.streams []

(** {1 Encoding/Decoding} *)

(* Use shared helpers from Webrtc_common *)
let write_uint16_be = Webrtc_common.write_uint16_be
let write_uint32_be = Webrtc_common.write_uint32_be
let read_uint16_be = Webrtc_common.read_uint16_be
let read_uint32_be = Webrtc_common.read_uint32_be
let crc32c = Webrtc_common.crc32c

(** Encode DATA chunk *)
let encode_data_chunk dc =
  let data_len = Bytes.length dc.user_data in
  (* DATA chunk: 4 byte header + 12 byte DATA header + user data + padding *)
  let chunk_len = 16 + data_len in
  let padded_len = (chunk_len + 3) land (lnot 3) in
  let buf = Bytes.create padded_len in

  (* Chunk header *)
  Bytes.set_uint8 buf 0 (int_of_chunk_type DATA);
  let flags =
    (if dc.flags.end_fragment then 0x01 else 0) lor
    (if dc.flags.begin_fragment then 0x02 else 0) lor
    (if dc.flags.unordered then 0x04 else 0) lor
    (if dc.flags.immediate then 0x08 else 0)
  in
  Bytes.set_uint8 buf 1 flags;
  write_uint16_be buf 2 chunk_len;

  (* DATA chunk specific *)
  write_uint32_be buf 4 dc.tsn;
  write_uint16_be buf 8 dc.stream_id;
  write_uint16_be buf 10 dc.stream_seq;
  write_uint32_be buf 12 dc.ppid;

  (* User data *)
  Bytes.blit dc.user_data 0 buf 16 data_len;

  buf

(** Decode DATA chunk *)
let decode_data_chunk buf =
  if Bytes.length buf < 16 then
    Error "DATA chunk too short"
  else
    let chunk_type = Bytes.get_uint8 buf 0 in
    if chunk_type <> 0 then
      Error (Printf.sprintf "Not a DATA chunk (type=%d)" chunk_type)
    else
      let flags_byte = Bytes.get_uint8 buf 1 in
      let chunk_len = read_uint16_be buf 2 in
      let tsn = read_uint32_be buf 4 in
      let stream_id = read_uint16_be buf 8 in
      let stream_seq = read_uint16_be buf 10 in
      let ppid = read_uint32_be buf 12 in
      let data_len = chunk_len - 16 in
      let user_data = Bytes.sub buf 16 data_len in
      Ok {
        flags = {
          end_fragment = flags_byte land 0x01 <> 0;
          begin_fragment = flags_byte land 0x02 <> 0;
          unordered = flags_byte land 0x04 <> 0;
          immediate = flags_byte land 0x08 <> 0;
        };
        tsn;
        stream_id;
        stream_seq;
        ppid;
        user_data;
      }

(** {2 SACK Chunk Encoding/Decoding} *)

(** Encode SACK chunk - RFC 4960 Section 3.3.4
    Format:
    - Chunk header (4 bytes): type=3, flags=0, length
    - Cumulative TSN Ack (4 bytes)
    - Advertised Receiver Window Credit (4 bytes)
    - Number of Gap Ack Blocks (2 bytes)
    - Number of Duplicate TSNs (2 bytes)
    - Gap Ack Blocks (4 bytes each: start offset, end offset)
    - Duplicate TSNs (4 bytes each)
*)
let encode_sack_chunk sack =
  let num_gaps = List.length sack.gap_ack_blocks in
  let num_dups = List.length sack.duplicate_tsns in
  (* Header (4) + fixed (12) + gaps (4 each) + dups (4 each) *)
  let chunk_len = 4 + 12 + (num_gaps * 4) + (num_dups * 4) in
  let padded_len = (chunk_len + 3) land (lnot 3) in
  let buf = Bytes.create padded_len in

  (* Chunk header *)
  Bytes.set_uint8 buf 0 (int_of_chunk_type SACK);
  Bytes.set_uint8 buf 1 0;  (* No flags for SACK *)
  write_uint16_be buf 2 chunk_len;

  (* SACK fixed part *)
  write_uint32_be buf 4 sack.cumulative_tsn;
  write_uint32_be buf 8 sack.a_rwnd;
  write_uint16_be buf 12 num_gaps;
  write_uint16_be buf 14 num_dups;

  (* Gap Ack Blocks *)
  let offset = ref 16 in
  List.iter (fun gap ->
    write_uint16_be buf !offset gap.gap_start;
    write_uint16_be buf (!offset + 2) gap.gap_end;
    offset := !offset + 4
  ) sack.gap_ack_blocks;

  (* Duplicate TSNs *)
  List.iter (fun tsn ->
    write_uint32_be buf !offset tsn;
    offset := !offset + 4
  ) sack.duplicate_tsns;

  buf

(** Decode SACK chunk *)
let decode_sack_chunk buf =
  if Bytes.length buf < 16 then
    Error "SACK chunk too short"
  else
    let chunk_type = Bytes.get_uint8 buf 0 in
    if chunk_type <> 3 then
      Error (Printf.sprintf "Not a SACK chunk (type=%d)" chunk_type)
    else
      let chunk_len = read_uint16_be buf 2 in
      let cumulative_tsn = read_uint32_be buf 4 in
      let a_rwnd = read_uint32_be buf 8 in
      let num_gaps = read_uint16_be buf 12 in
      let num_dups = read_uint16_be buf 14 in

      (* Verify size *)
      let expected_len = 16 + (num_gaps * 4) + (num_dups * 4) in
      if chunk_len < expected_len then
        Error "SACK chunk size mismatch"
      else begin
        (* Parse Gap Ack Blocks *)
        let gap_ack_blocks = ref [] in
        for i = 0 to num_gaps - 1 do
          let offset = 16 + (i * 4) in
          let gap_start = read_uint16_be buf offset in
          let gap_end = read_uint16_be buf (offset + 2) in
          gap_ack_blocks := { gap_start; gap_end } :: !gap_ack_blocks
        done;

        (* Parse Duplicate TSNs *)
        let dup_offset = 16 + (num_gaps * 4) in
        let duplicate_tsns = ref [] in
        for i = 0 to num_dups - 1 do
          let offset = dup_offset + (i * 4) in
          let tsn = read_uint32_be buf offset in
          duplicate_tsns := tsn :: !duplicate_tsns
        done;

        Ok {
          cumulative_tsn;
          a_rwnd;
          gap_ack_blocks = List.rev !gap_ack_blocks;
          duplicate_tsns = List.rev !duplicate_tsns;
        }
      end

(** Create a simple SACK acknowledging up to a given TSN *)
let create_sack ~cumulative_tsn ~a_rwnd =
  {
    cumulative_tsn;
    a_rwnd;
    gap_ack_blocks = [];
    duplicate_tsns = [];
  }

(** Create SACK with gap ack blocks for non-contiguous received TSNs *)
let create_sack_with_gaps ~cumulative_tsn ~a_rwnd ~gap_ack_blocks =
  {
    cumulative_tsn;
    a_rwnd;
    gap_ack_blocks;
    duplicate_tsns = [];
  }

(** {2 INIT/INIT_ACK Chunks - RFC 4960 Section 3.3.2 & 3.3.3} *)

(** INIT chunk - sent to initiate an association *)
type init_chunk = {
  initiate_tag: int32;         (** Sender's verification tag *)
  a_rwnd: int32;               (** Advertised Receiver Window Credit *)
  num_outbound_streams: int;   (** Number of outbound streams *)
  num_inbound_streams: int;    (** Number of inbound streams *)
  initial_tsn: int32;          (** Initial TSN *)
}

(** INIT_ACK chunk - response to INIT *)
type init_ack_chunk = {
  initiate_tag: int32;
  a_rwnd: int32;
  num_outbound_streams: int;
  num_inbound_streams: int;
  initial_tsn: int32;
  state_cookie: bytes;         (** State Cookie for 4-way handshake *)
}

(** COOKIE_ECHO chunk - echoes the cookie from INIT_ACK *)
type cookie_echo_chunk = {
  cookie: bytes;
}

(** COOKIE_ACK chunk - acknowledges COOKIE_ECHO (no data, just header) *)
type cookie_ack_chunk = unit

(** Encode INIT chunk - RFC 4960 Section 3.3.2
    Format:
    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |   Type = 1    |  Chunk Flags  |         Chunk Length          |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                         Initiate Tag                          |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |           A-RWND              |  Number of Outbound Streams   |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |  Number of Inbound Streams    |          Initial TSN          |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    *)
let encode_init_chunk (init : init_chunk) =
  let chunk_len = 20 in  (* 4 header + 16 fixed fields *)
  let buf = Bytes.create chunk_len in
  (* Chunk header *)
  Bytes.set_uint8 buf 0 1;  (* Type = INIT *)
  Bytes.set_uint8 buf 1 0;  (* Flags = 0 *)
  write_uint16_be buf 2 chunk_len;
  (* Fixed parameters *)
  write_uint32_be buf 4 init.initiate_tag;
  write_uint32_be buf 8 init.a_rwnd;
  write_uint16_be buf 12 init.num_outbound_streams;
  write_uint16_be buf 14 init.num_inbound_streams;
  write_uint32_be buf 16 init.initial_tsn;
  buf

(** Decode INIT chunk *)
let decode_init_chunk (buf : bytes) : (init_chunk, string) result =
  if Bytes.length buf < 20 then
    Error "INIT chunk too short"
  else
    let chunk_type = Bytes.get_uint8 buf 0 in
    if chunk_type <> 1 then
      Error (Printf.sprintf "Not an INIT chunk (type=%d)" chunk_type)
    else
      let initiate_tag = read_uint32_be buf 4 in
      let a_rwnd = read_uint32_be buf 8 in
      let num_outbound_streams = read_uint16_be buf 12 in
      let num_inbound_streams = read_uint16_be buf 14 in
      let initial_tsn = read_uint32_be buf 16 in
      if initiate_tag = 0l then
        Error "INIT: Initiate Tag must not be zero"
      else if num_outbound_streams = 0 || num_inbound_streams = 0 then
        Error "INIT: Stream counts must be > 0"
      else if Int32.to_int a_rwnd < 1500 then
        Error "INIT: A-RWND must be >= 1500"
      else
        Ok { initiate_tag; a_rwnd; num_outbound_streams; num_inbound_streams; initial_tsn }

(** Encode INIT_ACK chunk with state cookie *)
let encode_init_ack_chunk (init_ack : init_ack_chunk) =
  let cookie_len = Bytes.length init_ack.state_cookie in
  (* Cookie parameter: Type(2) + Length(2) + Cookie + padding *)
  let cookie_param_len = 4 + cookie_len in
  let padded_cookie_len = (cookie_param_len + 3) land (lnot 3) in  (* 4-byte align *)
  let chunk_len = 20 + padded_cookie_len in
  let buf = Bytes.create chunk_len in
  (* Chunk header *)
  Bytes.set_uint8 buf 0 2;  (* Type = INIT_ACK *)
  Bytes.set_uint8 buf 1 0;  (* Flags = 0 *)
  write_uint16_be buf 2 chunk_len;
  (* Fixed parameters *)
  write_uint32_be buf 4 init_ack.initiate_tag;
  write_uint32_be buf 8 init_ack.a_rwnd;
  write_uint16_be buf 12 init_ack.num_outbound_streams;
  write_uint16_be buf 14 init_ack.num_inbound_streams;
  write_uint32_be buf 16 init_ack.initial_tsn;
  (* State Cookie parameter (Type = 7) *)
  write_uint16_be buf 20 7;  (* Parameter Type = State Cookie *)
  write_uint16_be buf 22 (4 + cookie_len);  (* Parameter Length *)
  Bytes.blit init_ack.state_cookie 0 buf 24 cookie_len;
  buf

(** Decode INIT_ACK chunk *)
let decode_init_ack_chunk (buf : bytes) : (init_ack_chunk, string) result =
  if Bytes.length buf < 20 then
    Error "INIT_ACK chunk too short"
  else
    let chunk_type = Bytes.get_uint8 buf 0 in
    if chunk_type <> 2 then
      Error (Printf.sprintf "Not an INIT_ACK chunk (type=%d)" chunk_type)
    else
      let initiate_tag = read_uint32_be buf 4 in
      let a_rwnd = read_uint32_be buf 8 in
      let num_outbound_streams = read_uint16_be buf 12 in
      let num_inbound_streams = read_uint16_be buf 14 in
      let initial_tsn = read_uint32_be buf 16 in
      (* Parse State Cookie parameter *)
      if Bytes.length buf < 24 then
        Error "INIT_ACK: No State Cookie parameter"
      else
        let param_type = read_uint16_be buf 20 in
        let param_len = read_uint16_be buf 22 in
        if param_type <> 7 then
          Error (Printf.sprintf "INIT_ACK: Expected State Cookie (7), got type %d" param_type)
        else
          let cookie_len = param_len - 4 in
          if Bytes.length buf < 24 + cookie_len then
            Error "INIT_ACK: State Cookie truncated"
          else
            let state_cookie = Bytes.sub buf 24 cookie_len in
            Ok { initiate_tag; a_rwnd; num_outbound_streams; num_inbound_streams;
                 initial_tsn; state_cookie }

(** Encode COOKIE_ECHO chunk - Type = 10 *)
let encode_cookie_echo_chunk (cookie_echo : cookie_echo_chunk) =
  let cookie_len = Bytes.length cookie_echo.cookie in
  let chunk_len = 4 + cookie_len in
  let buf = Bytes.create chunk_len in
  Bytes.set_uint8 buf 0 10;  (* Type = COOKIE_ECHO *)
  Bytes.set_uint8 buf 1 0;   (* Flags = 0 *)
  write_uint16_be buf 2 chunk_len;
  Bytes.blit cookie_echo.cookie 0 buf 4 cookie_len;
  buf

(** Decode COOKIE_ECHO chunk *)
let decode_cookie_echo_chunk (buf : bytes) : (cookie_echo_chunk, string) result =
  if Bytes.length buf < 4 then
    Error "COOKIE_ECHO chunk too short"
  else
    let chunk_type = Bytes.get_uint8 buf 0 in
    if chunk_type <> 10 then
      Error (Printf.sprintf "Not a COOKIE_ECHO chunk (type=%d)" chunk_type)
    else
      let chunk_len = read_uint16_be buf 2 in
      let cookie_len = chunk_len - 4 in
      if cookie_len <= 0 then
        Error "COOKIE_ECHO: Empty cookie"
      else if Bytes.length buf < chunk_len then
        Error "COOKIE_ECHO: Cookie truncated"
      else
        let cookie = Bytes.sub buf 4 cookie_len in
        Ok { cookie }

(** Encode COOKIE_ACK chunk - Type = 11, no data *)
let encode_cookie_ack_chunk () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 11;  (* Type = COOKIE_ACK *)
  Bytes.set_uint8 buf 1 0;   (* Flags = 0 *)
  write_uint16_be buf 2 4;   (* Length = header only *)
  buf

(** Decode COOKIE_ACK chunk *)
let decode_cookie_ack_chunk (buf : bytes) : (cookie_ack_chunk, string) result =
  if Bytes.length buf < 4 then
    Error "COOKIE_ACK chunk too short"
  else
    let chunk_type = Bytes.get_uint8 buf 0 in
    if chunk_type <> 11 then
      Error (Printf.sprintf "Not a COOKIE_ACK chunk (type=%d)" chunk_type)
    else
      Ok ()

(** Create INIT chunk with default values *)
let create_init ~initiate_tag ~initial_tsn ?(a_rwnd = 65536l)
    ?(num_outbound_streams = 65535) ?(num_inbound_streams = 65535) () =
  { initiate_tag; a_rwnd; num_outbound_streams; num_inbound_streams; initial_tsn }

(** Create INIT_ACK from received INIT *)
let create_init_ack ~initiate_tag ~initial_tsn ~state_cookie
    ?(a_rwnd = 65536l) ?(num_outbound_streams = 65535) ?(num_inbound_streams = 65535) () =
  { initiate_tag; a_rwnd; num_outbound_streams; num_inbound_streams;
    initial_tsn; state_cookie }

(** Generate a simple state cookie (in production, should be HMAC-signed) *)
let generate_state_cookie ~peer_tag ~peer_tsn ~my_tag ~my_tsn =
  let buf = Bytes.create 32 in
  write_uint32_be buf 0 peer_tag;
  write_uint32_be buf 4 peer_tsn;
  write_uint32_be buf 8 my_tag;
  write_uint32_be buf 12 my_tsn;
  (* Timestamp for expiration *)
  let timestamp = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
  Bytes.set_int64_be buf 16 timestamp;
  (* Random nonce for uniqueness *)
  let nonce = Random.int32 Int32.max_int in
  write_uint32_be buf 24 nonce;
  (* Reserved *)
  write_uint32_be buf 28 0l;
  buf

(** {2 Retransmission Timer (T3-rtx) - RFC 4960 Section 6.3} *)

(** RTO constants - RFC 4960 Section 6.3.1 *)
module Rto = struct
  let initial_ms = 1000.0      (** Initial RTO: 1 second *)
  let min_ms = 1000.0          (** Minimum RTO: 1 second *)
  let max_ms = 60000.0         (** Maximum RTO: 60 seconds *)
  let alpha = 0.125            (** RTT smoothing factor *)
  let beta = 0.25              (** RTTVAR smoothing factor *)
  let max_init_retransmits = 8 (** Max retransmits for INIT *)
  let max_retransmits = 5      (** Path.Max.Retrans: 5 *)
end

(** Timer state *)
type rtx_timer_state =
  | Rtx_stopped
  | Rtx_running
  | Rtx_closed

(** Retransmission timer *)
type rtx_timer = {
  mutable rto: float;                    (** Current RTO in milliseconds *)
  mutable srtt: float option;            (** Smoothed RTT *)
  mutable rttvar: float option;          (** RTT variance *)
  mutable n_rtos: int;                   (** Number of retransmissions *)
  mutable rtx_state: rtx_timer_state;
  mutable start_time: float option;      (** When timer started *)
  max_retransmits: int;
}

(** Create new retransmission timer *)
let create_rtx_timer ?(max_retransmits = Rto.max_retransmits) () = {
  rto = Rto.initial_ms;
  srtt = None;
  rttvar = None;
  n_rtos = 0;
  rtx_state = Rtx_stopped;
  start_time = None;
  max_retransmits;
}

(** Start the timer *)
let start_rtx_timer timer =
  timer.rtx_state <- Rtx_running;
  timer.start_time <- Some (Unix.gettimeofday () *. 1000.0);
  timer.n_rtos <- 0

(** Stop the timer *)
let stop_rtx_timer timer =
  timer.rtx_state <- Rtx_stopped;
  timer.start_time <- None

(** Reset timer for new RTO value *)
let reset_rtx_timer timer ~rto =
  timer.rto <- rto;
  timer.rtx_state <- Rtx_running;
  timer.start_time <- Some (Unix.gettimeofday () *. 1000.0)

(** Update RTO based on RTT measurement - RFC 4960 Section 6.3.1 *)
let update_rto timer ~measured_rtt =
  match timer.srtt, timer.rttvar with
  | None, None ->
    (* First measurement *)
    timer.srtt <- Some measured_rtt;
    timer.rttvar <- Some (measured_rtt /. 2.0);
    timer.rto <- measured_rtt +. (4.0 *. measured_rtt /. 2.0)
  | Some srtt, Some rttvar ->
    (* Subsequent measurements *)
    let rttvar' = (1.0 -. Rto.beta) *. rttvar +. Rto.beta *. Float.abs (srtt -. measured_rtt) in
    let srtt' = (1.0 -. Rto.alpha) *. srtt +. Rto.alpha *. measured_rtt in
    timer.srtt <- Some srtt';
    timer.rttvar <- Some rttvar';
    timer.rto <- srtt' +. (4.0 *. rttvar')
  | _ -> ()  (* Should not happen *)

(** Handle timer expiration - returns true if should retransmit, false if failed *)
let handle_timeout timer =
  if timer.n_rtos >= timer.max_retransmits then begin
    timer.rtx_state <- Rtx_closed;
    false  (* Retransmission failure *)
  end else begin
    timer.n_rtos <- timer.n_rtos + 1;
    (* Exponential backoff: double RTO, cap at max *)
    timer.rto <- Float.min (timer.rto *. 2.0) Rto.max_ms;
    timer.start_time <- Some (Unix.gettimeofday () *. 1000.0);
    true  (* Should retransmit *)
  end

(** Check if timer has expired *)
let is_expired timer =
  match timer.rtx_state, timer.start_time with
  | Rtx_running, Some start ->
    let now = Unix.gettimeofday () *. 1000.0 in
    now -. start >= timer.rto
  | _ -> false

(** Get remaining time until expiration in ms (or None if not running) *)
let time_until_expiry timer =
  match timer.rtx_state, timer.start_time with
  | Rtx_running, Some start ->
    let now = Unix.gettimeofday () *. 1000.0 in
    let elapsed = now -. start in
    if elapsed >= timer.rto then Some 0.0
    else Some (timer.rto -. elapsed)
  | _ -> None

(** Timer status for debugging *)
let rtx_timer_status timer =
  Printf.sprintf "RTO=%.0fms n_rtos=%d state=%s"
    timer.rto
    timer.n_rtos
    (match timer.rtx_state with
     | Rtx_stopped -> "stopped"
     | Rtx_running -> "running"
     | Rtx_closed -> "closed")

(** CRC32-C calculation for SCTP checksum - RFC 4960 Appendix B *)
let calculate_checksum = crc32c

(** Encode SCTP packet *)
let encode_packet packet =
  (* Calculate total chunk data size *)
  let chunks_size = List.fold_left (fun acc c ->
    acc + 4 + Bytes.length c.chunk_value
  ) 0 packet.chunks in

  (* SCTP header is 12 bytes *)
  let buf = Bytes.create (12 + chunks_size) in

  (* Header *)
  write_uint16_be buf 0 packet.header.source_port;
  write_uint16_be buf 2 packet.header.dest_port;
  write_uint32_be buf 4 packet.header.verification_tag;
  (* Checksum placeholder - will be computed over whole packet *)
  write_uint32_be buf 8 0l;

  (* Chunks *)
  let offset = ref 12 in
  List.iter (fun chunk ->
    Bytes.set_uint8 buf !offset chunk.chunk_type;
    Bytes.set_uint8 buf (!offset + 1) chunk.chunk_flags;
    write_uint16_be buf (!offset + 2) (4 + Bytes.length chunk.chunk_value);
    Bytes.blit chunk.chunk_value 0 buf (!offset + 4) (Bytes.length chunk.chunk_value);
    offset := !offset + 4 + Bytes.length chunk.chunk_value
  ) packet.chunks;

  (* Calculate and insert checksum *)
  let checksum = calculate_checksum buf in
  write_uint32_be buf 8 checksum;

  buf

(** Decode SCTP packet *)
let decode_packet buf =
  if Bytes.length buf < 12 then
    Error "Packet too short for SCTP header"
  else
    let source_port = read_uint16_be buf 0 in
    let dest_port = read_uint16_be buf 2 in
    let verification_tag = read_uint32_be buf 4 in
    let checksum = read_uint32_be buf 8 in

    (* Parse chunks *)
    let chunks = ref [] in
    let offset = ref 12 in
    while !offset + 4 <= Bytes.length buf do
      let chunk_type = Bytes.get_uint8 buf !offset in
      let chunk_flags = Bytes.get_uint8 buf (!offset + 1) in
      let chunk_length = read_uint16_be buf (!offset + 2) in
      let value_len = chunk_length - 4 in
      if !offset + chunk_length <= Bytes.length buf && value_len >= 0 then begin
        let chunk_value = Bytes.sub buf (!offset + 4) value_len in
        chunks := { chunk_type; chunk_flags; chunk_length; chunk_value } :: !chunks;
        (* Round up to 4-byte boundary *)
        offset := !offset + ((chunk_length + 3) land (lnot 3))
      end else
        offset := Bytes.length buf (* Stop parsing *)
    done;

    Ok {
      header = {
        source_port;
        dest_port;
        verification_tag;
        checksum;
      };
      chunks = List.rev !chunks;
    }

(** Check if data looks like SCTP *)
let is_sctp_data buf =
  Bytes.length buf >= 12  (* Minimum SCTP header size *)

(** {1 State Machine} *)

(** Set association state *)
let set_state assoc new_state =
  assoc.state <- new_state

(** Establish association (for testing) *)
let establish assoc =
  assoc.state <- Established

(** Begin shutdown *)
let begin_shutdown assoc =
  if assoc.state = Established then
    assoc.state <- Shutdown_pending

(** {1 Pretty Printing} *)

let pp_data_chunk fmt dc =
  Format.fprintf fmt "DATA(tsn=%ld, sid=%d, ssn=%d, ppid=%ld, len=%d)"
    dc.tsn dc.stream_id dc.stream_seq dc.ppid (Bytes.length dc.user_data)

let pp_stream fmt s =
  Format.fprintf fmt "Stream(id=%d, ordered=%b, ssn=%d)"
    s.id s.ordered s.next_ssn

let pp_association fmt a =
  Format.fprintf fmt "Association(state=%s, streams=%d, vtag=%ld)"
    (string_of_state a.state)
    (Hashtbl.length a.streams)
    a.my_vtag
