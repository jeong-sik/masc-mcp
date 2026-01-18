(** RFC 8831/8832 - WebRTC Data Channels

    Pure OCaml implementation of WebRTC Data Channels.

    Data Channels provide:
    - Bidirectional data communication
    - Multiple channels over single SCTP association
    - Configurable reliability and ordering
    - In-band negotiation via DCEP

    Compact Protocol v4:
    - Optional zstd compression for large payloads (>128 bytes)
    - Transparent decompression via ZSTD header detection
    - 60-70% bandwidth savings for text/JSON data

    Reference:
    - https://datatracker.ietf.org/doc/html/rfc8831 (Overview)
    - https://datatracker.ietf.org/doc/html/rfc8832 (DCEP Protocol)
*)

(** {1 Compression - Compact Protocol v4} *)

module Compression = struct
  (** Minimum size for compression *)
  let min_size = 128

  (** Default compression level *)
  let default_level = 3

  (** ZSTD header magic *)
  let magic = "ZSTD"

  (** Compress with zstd if beneficial *)
  let compress ?(level = default_level) (data : bytes) : (bytes * bool) =
    let len = Bytes.length data in
    if len < min_size then (data, false)
    else
      try
        let str = Bytes.to_string data in
        let compressed = Zstd.compress ~level str in
        if String.length compressed < len then
          (Bytes.of_string compressed, true)
        else
          (data, false)
      with Zstd.Error _ -> (data, false)

  (** Encode with size header: ZSTD (4) + orig_size (4 BE) + compressed *)
  let encode_with_header (orig_size : int) (compressed : bytes) : bytes =
    let header = Bytes.create 8 in
    Bytes.blit_string magic 0 header 0 4;
    Bytes.set header 4 (Char.chr ((orig_size lsr 24) land 0xFF));
    Bytes.set header 5 (Char.chr ((orig_size lsr 16) land 0xFF));
    Bytes.set header 6 (Char.chr ((orig_size lsr 8) land 0xFF));
    Bytes.set header 7 (Char.chr (orig_size land 0xFF));
    let result = Bytes.create (8 + Bytes.length compressed) in
    Bytes.blit header 0 result 0 8;
    Bytes.blit compressed 0 result 8 (Bytes.length compressed);
    result

  (** Decode header, returns (orig_size, compressed_data) if valid *)
  let decode_header (data : bytes) : (int * bytes) option =
    if Bytes.length data < 8 then None
    else if Bytes.sub_string data 0 4 <> magic then None
    else
      let orig_size =
        (Char.code (Bytes.get data 4) lsl 24) lor
        (Char.code (Bytes.get data 5) lsl 16) lor
        (Char.code (Bytes.get data 6) lsl 8) lor
        Char.code (Bytes.get data 7)
      in
      let compressed = Bytes.sub data 8 (Bytes.length data - 8) in
      Some (orig_size, compressed)

  (** Decompress with known original size *)
  let decompress ~(orig_size : int) (compressed : bytes) : bytes option =
    try
      let str = Bytes.to_string compressed in
      Some (Bytes.of_string (Zstd.decompress orig_size str))
    with Zstd.Error _ -> None

  (** Auto-decompress if ZSTD header present *)
  let decompress_auto (data : bytes) : bytes =
    match decode_header data with
    | Some (orig_size, compressed) ->
        (match decompress ~orig_size compressed with
         | Some decompressed -> decompressed
         | None -> data)  (* Return original on failure *)
    | None -> data

  (** Compress and add header if beneficial *)
  let compress_with_header ?(level = default_level) (data : bytes) : bytes =
    let (compressed, did_compress) = compress ~level data in
    if did_compress then
      encode_with_header (Bytes.length data) compressed
    else
      data
end

(** {1 Types} *)

(** Channel types - RFC 8832 Section 5.1 *)
type channel_type =
  | Reliable_ordered           (** 0x00 - TCP-like semantics *)
  | Reliable_unordered         (** 0x80 - Reliable but unordered *)
  | Unreliable_ordered         (** 0x01 - Unreliable but ordered (partial) *)
  | Unreliable_unordered       (** 0x81 - UDP-like semantics *)
  | Partial_reliable_rexmit of int   (** 0x02 - Limit retransmissions *)
  | Partial_reliable_timed of int    (** 0x03 - Time-limited reliability (ms) *)
[@@deriving show, eq]

(** Channel state *)
type channel_state =
  | Connecting   (** Awaiting ACK *)
  | Open         (** Ready for data *)
  | Closing      (** Shutdown initiated *)
  | Closed       (** Fully closed *)
[@@deriving show, eq]

(** Connection state *)
type connection_state =
  | Conn_connecting
  | Conn_connected
  | Conn_closed
[@@deriving show, eq]

(** Data channel configuration *)
type config = {
  max_channels: int;
  max_message_size: int;
  sctp_port: int;
  remote_sctp_port: int;
}

(** DCEP DATA_CHANNEL_OPEN message *)
type open_message = {
  channel_type: channel_type;
  priority: int;
  reliability_param: int;   (** Max retransmits or lifetime ms *)
  label: string;
  protocol: string;
}

(** Data channel *)
type channel = {
  id: int;               (** Stream identifier *)
  label: string;
  protocol: string;
  channel_type: channel_type;
  priority: int;
  negotiated: bool;
  mutable state: channel_state;
  mutable buffered_amount: int;
}

(** Events from data channel *)
type event =
  | ChannelOpen of channel
  | ChannelClose of int
  | Message of int * bytes
  | Error of string

(** Data channel connection *)
type connection = {
  mutable state: connection_state;
  channels: (int, channel) Hashtbl.t;
  config: config;
  is_offerer: bool;
  mutable next_stream_id: int;   (** Offerer: even, Answerer: odd *)
  mutable outgoing: (int * bytes * int32) list;  (** (stream_id, data, ppid) *)
}

(** {1 Constants - RFC 8832 Section 5.1} *)

let dcep_open = 0x03
let dcep_ack = 0x02

(* SCTP PPIDs for WebRTC *)
let ppid_dcep = 50l
let ppid_string = 51l
let ppid_binary = 53l
let ppid_string_empty = 56l
let ppid_binary_empty = 57l

(** {1 Default Configuration} *)

let default_config = {
  max_channels = 65535;
  max_message_size = 262144;  (* 256 KB *)
  sctp_port = 5000;
  remote_sctp_port = 5000;
}

(** {1 String Conversions} *)

let string_of_channel_type = function
  | Reliable_ordered -> "reliable-ordered"
  | Reliable_unordered -> "reliable-unordered"
  | Unreliable_ordered -> "unreliable-ordered"
  | Unreliable_unordered -> "unreliable-unordered"
  | Partial_reliable_rexmit n -> Printf.sprintf "partial-reliable-rexmit(%d)" n
  | Partial_reliable_timed ms -> Printf.sprintf "partial-reliable-timed(%dms)" ms

let channel_type_of_int ?param = function
  | 0x00 -> Reliable_ordered
  | 0x80 -> Reliable_unordered
  | 0x01 -> Unreliable_ordered
  | 0x81 -> Unreliable_unordered
  | 0x02 -> Partial_reliable_rexmit (Option.value param ~default:0)
  | 0x03 -> Partial_reliable_timed (Option.value param ~default:0)
  | 0x82 -> Partial_reliable_rexmit (Option.value param ~default:0)  (* Unordered variant *)
  | 0x83 -> Partial_reliable_timed (Option.value param ~default:0)   (* Unordered variant *)
  | _ -> Reliable_ordered  (* Default fallback *)

let int_of_channel_type = function
  | Reliable_ordered -> 0x00
  | Reliable_unordered -> 0x80
  | Unreliable_ordered -> 0x01
  | Unreliable_unordered -> 0x81
  | Partial_reliable_rexmit _ -> 0x02
  | Partial_reliable_timed _ -> 0x03

let string_of_channel_state = function
  | Connecting -> "connecting"
  | Open -> "open"
  | Closing -> "closing"
  | Closed -> "closed"

let string_of_connection_state = function
  | Conn_connecting -> "connecting"
  | Conn_connected -> "connected"
  | Conn_closed -> "closed"

(** {1 Channel Type Helpers} *)

let is_reliable = function
  | Reliable_ordered | Reliable_unordered -> true
  | Unreliable_ordered | Unreliable_unordered -> false
  | Partial_reliable_rexmit _ | Partial_reliable_timed _ -> false

let is_ordered = function
  | Reliable_ordered | Unreliable_ordered -> true
  | Reliable_unordered | Unreliable_unordered -> false
  | Partial_reliable_rexmit _ | Partial_reliable_timed _ -> true

let reliability_param = function
  | Partial_reliable_rexmit n | Partial_reliable_timed n -> n
  | _ -> 0

(** {1 Connection Management} *)

(** Create new data channel connection *)
let create config ~is_offerer =
  {
    state = Conn_connecting;
    channels = Hashtbl.create 16;
    config;
    is_offerer;
    next_stream_id = if is_offerer then 0 else 1;
    outgoing = [];
  }

(** Get connection state *)
let get_state conn = conn.state

(** Set connection as connected *)
let set_connected conn =
  conn.state <- Conn_connected

(** Close connection *)
let close conn =
  conn.state <- Conn_closed;
  Hashtbl.iter (fun _ (ch : channel) -> ch.state <- Closed) conn.channels

(** Get connection info as JSON *)
let connection_info conn =
  `Assoc [
    ("state", `String (string_of_connection_state conn.state));
    ("isOfferer", `Bool conn.is_offerer);
    ("channelCount", `Int (Hashtbl.length conn.channels));
    ("nextStreamId", `Int conn.next_stream_id);
  ]

(** {1 Channel Management} *)

(** Create a new data channel *)
let create_channel conn ~label ?(protocol="") ?(channel_type=Reliable_ordered)
    ?(priority=0) ?(negotiated=false) ?id () : (channel, string) result =
  let id = match id with
    | Some id -> id
    | None ->
      let id = conn.next_stream_id in
      conn.next_stream_id <- conn.next_stream_id + 2;  (* Increment by 2 for even/odd separation *)
      id
  in
  if id >= conn.config.max_channels then
    Error "Maximum channels exceeded"
  else if Hashtbl.mem conn.channels id then
    Error "Channel ID already in use"
  else
    let state = if negotiated then Open else Connecting in
    let channel = {
      id;
      label;
      protocol;
      channel_type;
      priority;
      negotiated;
      state;
      buffered_amount = 0;
    } in
    Hashtbl.replace conn.channels id channel;
    Ok channel

(** Get channel by ID *)
let get_channel conn ~channel_id =
  Hashtbl.find_opt conn.channels channel_id

(** Find channel by label *)
let find_channel_by_label conn ~label =
  Hashtbl.fold (fun _ ch acc ->
    match acc with
    | Some _ -> acc
    | None -> if ch.label = label then Some ch else None
  ) conn.channels None

(** Close a channel *)
let close_channel conn ~channel_id =
  match Hashtbl.find_opt conn.channels channel_id with
  | None -> ()
  | Some ch ->
    ch.state <- Closed

(** Get all channels *)
let get_channels conn =
  Hashtbl.fold (fun _ ch acc -> ch :: acc) conn.channels []

(** Get channel info as JSON *)
let channel_info ch =
  `Assoc [
    ("id", `Int ch.id);
    ("label", `String ch.label);
    ("protocol", `String ch.protocol);
    ("type", `String (string_of_channel_type ch.channel_type));
    ("state", `String (string_of_channel_state ch.state));
    ("priority", `Int ch.priority);
    ("negotiated", `Bool ch.negotiated);
    ("bufferedAmount", `Int ch.buffered_amount);
  ]

(** {1 DCEP Encoding/Decoding - RFC 8832} *)

(* Use shared helpers from Webrtc_common *)
let write_uint16_be = Webrtc_common.write_uint16_be
let read_uint16_be = Webrtc_common.read_uint16_be

(** Encode DATA_CHANNEL_OPEN message *)
let encode_open_message (msg : open_message) =
  let label_len = String.length msg.label in
  let protocol_len = String.length msg.protocol in
  let buf = Bytes.create (12 + label_len + protocol_len) in

  (* Message type *)
  Bytes.set_uint8 buf 0 dcep_open;
  (* Channel type *)
  Bytes.set_uint8 buf 1 (int_of_channel_type msg.channel_type);
  (* Priority *)
  write_uint16_be buf 2 msg.priority;
  (* Reliability parameter *)
  Webrtc_common.write_uint32_be buf 4 (Int32.of_int msg.reliability_param);
  (* Label length *)
  write_uint16_be buf 8 label_len;
  (* Protocol length *)
  write_uint16_be buf 10 protocol_len;
  (* Label *)
  Bytes.blit_string msg.label 0 buf 12 label_len;
  (* Protocol *)
  Bytes.blit_string msg.protocol 0 buf (12 + label_len) protocol_len;

  buf

(** Decode DATA_CHANNEL_OPEN message *)
let decode_open_message buf : (open_message, string) result =
  if Bytes.length buf < 12 then
    Error "DCEP OPEN message too short"
  else
    let msg_type = Bytes.get_uint8 buf 0 in
    if msg_type <> dcep_open then
      Error (Printf.sprintf "Not a DCEP OPEN message (type=%d)" msg_type)
    else
      let channel_type_byte = Bytes.get_uint8 buf 1 in
      let priority = read_uint16_be buf 2 in
      let reliability_param = Int32.to_int (Bytes.get_int32_be buf 4) in
      let label_len = read_uint16_be buf 8 in
      let protocol_len = read_uint16_be buf 10 in

      if Bytes.length buf < 12 + label_len + protocol_len then
        Error "DCEP OPEN message truncated"
      else
        let label = Bytes.sub_string buf 12 label_len in
        let protocol = Bytes.sub_string buf (12 + label_len) protocol_len in
        let channel_type = channel_type_of_int ~param:reliability_param channel_type_byte in

        Ok {
          channel_type;
          priority;
          reliability_param;
          label;
          protocol;
        }

(** Encode DATA_CHANNEL_ACK message *)
let encode_ack_message () =
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 dcep_ack;
  buf

(** {1 Send Functions} *)

(** Queue outgoing message *)
let queue_outgoing conn ~stream_id ~data ~ppid =
  conn.outgoing <- conn.outgoing @ [(stream_id, data, ppid)]

(** Get and clear outgoing messages *)
let get_outgoing conn =
  let msgs = conn.outgoing in
  conn.outgoing <- [];
  msgs

(** Send text message (Compact Protocol v4: optional compression) *)
let send_text ?(compress = false) conn ~channel_id text : (unit, string) result =
  match get_channel conn ~channel_id with
  | None -> Error "Channel not found"
  | Some ch when ch.state <> Open -> Error "Channel not open"
  | Some _ch ->
    let ppid = if String.length text = 0 then ppid_string_empty else ppid_string in
    let raw_data = Bytes.of_string text in
    let data =
      if compress then Compression.compress_with_header raw_data
      else raw_data
    in
    queue_outgoing conn ~stream_id:channel_id ~data ~ppid;
    Ok ()

(** Send binary message (Compact Protocol v4: optional compression) *)
let send_binary ?(compress = false) conn ~channel_id data : (unit, string) result =
  match get_channel conn ~channel_id with
  | None -> Error "Channel not found"
  | Some ch when ch.state <> Open -> Error "Channel not open"
  | Some _ch ->
    let ppid = if Bytes.length data = 0 then ppid_binary_empty else ppid_binary in
    let final_data =
      if compress then Compression.compress_with_header data
      else data
    in
    queue_outgoing conn ~stream_id:channel_id ~data:final_data ~ppid;
    Ok ()

(** {1 Receive/Event Handling} *)

(** Handle incoming DCEP or data message *)
let handle_data conn data ~stream_id =
  if Bytes.length data = 0 then
    []
  else
    let msg_type = Bytes.get_uint8 data 0 in
    if msg_type = dcep_open then
      (* Received OPEN - create channel and send ACK *)
      match decode_open_message data with
      | Error _ -> []
      | Ok open_msg ->
        let channel = {
          id = stream_id;
          label = open_msg.label;
          protocol = open_msg.protocol;
          channel_type = open_msg.channel_type;
          priority = open_msg.priority;
          negotiated = false;
          state = Open;
          buffered_amount = 0;
        } in
        Hashtbl.replace conn.channels stream_id channel;
        (* Queue ACK *)
        let ack = encode_ack_message () in
        queue_outgoing conn ~stream_id ~data:ack ~ppid:ppid_dcep;
        [ChannelOpen channel]
    else if msg_type = dcep_ack then
      (* Received ACK - mark channel as open *)
      match get_channel conn ~channel_id:stream_id with
      | None -> []
      | Some ch ->
        ch.state <- Open;
        [ChannelOpen ch]
    else
      (* Regular data message - Compact Protocol v4: auto-decompress *)
      match get_channel conn ~channel_id:stream_id with
      | None -> []
      | Some _ch ->
        let decompressed = Compression.decompress_auto data in
        [Message (stream_id, decompressed)]

(** {1 Pretty Printing} *)

let pp_channel fmt ch =
  Format.fprintf fmt "DataChannel(id=%d, label=%s, type=%s, state=%s)"
    ch.id ch.label
    (string_of_channel_type ch.channel_type)
    (string_of_channel_state ch.state)

let pp_connection fmt conn =
  Format.fprintf fmt "DataChannelConnection(state=%s, channels=%d)"
    (string_of_connection_state conn.state)
    (Hashtbl.length conn.channels)

let pp_event fmt = function
  | ChannelOpen ch -> Format.fprintf fmt "ChannelOpen(%s)" ch.label
  | ChannelClose id -> Format.fprintf fmt "ChannelClose(%d)" id
  | Message (id, data) -> Format.fprintf fmt "Message(ch=%d, len=%d)" id (Bytes.length data)
  | Error msg -> Format.fprintf fmt "Error(%s)" msg
