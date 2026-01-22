(** MASC WebRTC DataChannel - Stub Implementation

    This is a stub implementation that simulates WebRTC DataChannel behavior.
    It can be replaced with native libdatachannel bindings when available.

    Current: Stub backend (in-memory simulation)
    Future: Native backend (libdatachannel C bindings via ctypes)

    @author Second Brain
    @since MASC v3.0
*)

(* Eio-style direct implementation - no Lwt dependency *)

(** {1 Types} *)

type log_level =
  | Log_none
  | Log_fatal
  | Log_error
  | Log_warning
  | Log_info
  | Log_debug
  | Log_verbose

type ice_state =
  | New
  | Checking
  | Connected
  | Completed
  | Failed
  | Disconnected
  | Closed

type gathering_state =
  | GatheringNew
  | GatheringInProgress
  | GatheringComplete

type channel_state =
  | Connecting
  | Open
  | Closing
  | ChannelClosed

type reliability = {
  unordered: bool;
  unreliable: bool;
  max_packet_lifetime_ms: int option;
  max_retransmits: int option;
}

type channel_init = {
  reliability: reliability;
  protocol: string option;
  negotiated: bool;
  stream_id: int option;
}

type ice_server = {
  urls: string list;
  username: string option;
  credential: string option;
}

type config = {
  ice_servers: ice_server list;
  ice_transport_policy: [`All | `Relay];
  enable_ice_tcp: bool;
  port_range: (int * int) option;
  max_message_size: int;
}

type backend =
  | Native
  | Stub

(** Internal DataChannel state *)
type data_channel_internal = {
  id: int;
  label: string;
  protocol: string option;
  reliability: reliability;
  mutable state: channel_state;
  mutable on_message: (bytes -> bool -> unit) option;
  mutable on_open: (unit -> unit) option;
  mutable on_close: (unit -> unit) option;
  mutable on_error: (string -> unit) option;
  mutable on_buffered_low: (unit -> unit) option;
  mutable buffered: int;
  mutable buffered_threshold: int;
  (* Stub: message queue for local testing *)
  mutable message_queue: (bytes * bool) list;
}

(** DataChannel handle *)
type data_channel = data_channel_internal

(** Internal PeerConnection state *)
type peer_connection_internal = {
  pc_id: int;
  config: config;
  mutable ice_state: ice_state;
  mutable gathering_state: gathering_state;
  mutable local_sdp: string option;
  mutable remote_sdp: string option;
  mutable local_candidates: (string * string) list;  (* candidate, mid *)
  mutable channels: data_channel list;
  mutable next_channel_id: int;
  mutable on_local_candidate: (candidate:string -> mid:string -> unit) option;
  mutable on_state_change: (ice_state -> unit) option;
  mutable on_gathering_change: (gathering_state -> unit) option;
  mutable on_data_channel: (data_channel -> unit) option;
}

(** PeerConnection handle *)
type peer_connection = peer_connection_internal

(** {1 Global State} *)

let current_backend : backend ref = ref Stub
let next_pc_id : int ref = ref 0
let log_level_ref : log_level ref = ref Log_warning

type log_callback_fn = log_level -> string -> unit

let log_callback_ref : log_callback_fn option ref = ref None
let peer_connections : (int, peer_connection) Hashtbl.t = Hashtbl.create 16

(** {1 Logging} *)

let log level msg =
  let level_num = function
    | Log_none -> 0 | Log_fatal -> 1 | Log_error -> 2 | Log_warning -> 3
    | Log_info -> 4 | Log_debug -> 5 | Log_verbose -> 6
  in
  if level_num level <= level_num !log_level_ref then
    match !log_callback_ref with
    | Some cb -> cb level msg
    | None -> ()

(** {1 Configuration Defaults} *)

let default_reliability = {
  unordered = false;
  unreliable = false;
  max_packet_lifetime_ms = None;
  max_retransmits = None;
}

let default_channel_init = {
  reliability = default_reliability;
  protocol = None;
  negotiated = false;
  stream_id = None;
}

let audio_reliability = {
  unordered = true;    (* Allow reordering for lower latency *)
  unreliable = true;   (* Allow some packet loss *)
  max_packet_lifetime_ms = Some 100;  (* 100ms max age *)
  max_retransmits = Some 0;  (* No retransmits *)
}

let default_config = {
  ice_servers = [
    { urls = ["stun:stun.l.google.com:19302"]; username = None; credential = None };
    { urls = ["stun:stun1.l.google.com:19302"]; username = None; credential = None };
  ];
  ice_transport_policy = `All;
  enable_ice_tcp = false;
  port_range = None;
  max_message_size = 262144;  (* 256KB *)
}

(** {1 String Conversions} *)

let string_of_ice_state = function
  | New -> "new"
  | Checking -> "checking"
  | Connected -> "connected"
  | Completed -> "completed"
  | Failed -> "failed"
  | Disconnected -> "disconnected"
  | Closed -> "closed"

let string_of_gathering_state = function
  | GatheringNew -> "new"
  | GatheringInProgress -> "gathering"
  | GatheringComplete -> "complete"

let string_of_channel_state = function
  | Connecting -> "connecting"
  | Open -> "open"
  | Closing -> "closing"
  | ChannelClosed -> "closed"

let string_of_backend = function
  | Native -> "native (libdatachannel)"
  | Stub -> "stub (simulation)"

(** {1 Initialization} *)

let is_native_available () = Webrtc_bindings.is_available ()

let get_backend () = !current_backend

let init ?(level = Log_warning) ?log_callback ?(prefer_native = true) () =
  log_level_ref := level;
  log_callback_ref := log_callback;
  (* Try native backend if available and preferred *)
  let backend =
    if prefer_native && is_native_available () then begin
      (* Initialize native logger *)
      (match Webrtc_bindings.rtc_init_logger with
       | Some init_fn ->
         let level_int = match level with
           | Log_none -> 0 | Log_fatal -> 1 | Log_error -> 2 | Log_warning -> 3
           | Log_info -> 4 | Log_debug -> 5 | Log_verbose -> 6
         in
         init_fn level_int None
       | None -> ());
      Native
    end else
      Stub
  in
  current_backend := backend;
  log Log_info (Printf.sprintf "[WebRTC] Initialized with %s backend" (string_of_backend backend));
  backend

let cleanup () =
  Hashtbl.iter (fun _ pc ->
    pc.ice_state <- Closed;
    List.iter (fun dc -> dc.state <- ChannelClosed) pc.channels
  ) peer_connections;
  Hashtbl.clear peer_connections;
  log Log_info "[WebRTC] Cleanup complete"

(** {1 Peer Connection} *)

let create_peer_connection ?(config = default_config) () =
  let pc_id = !next_pc_id in
  incr next_pc_id;
  let pc = {
    pc_id;
    config;
    ice_state = New;
    gathering_state = GatheringNew;
    local_sdp = None;
    remote_sdp = None;
    local_candidates = [];
    channels = [];
    next_channel_id = 0;
    on_local_candidate = None;
    on_state_change = None;
    on_gathering_change = None;
    on_data_channel = None;
  } in
  Hashtbl.add peer_connections pc_id pc;
  log Log_debug (Printf.sprintf "[WebRTC] Created PeerConnection %d" pc_id);
  pc

let close_peer_connection pc =
  pc.ice_state <- Closed;
  (match pc.on_state_change with Some cb -> cb Closed | None -> ());
  List.iter (fun dc ->
    dc.state <- ChannelClosed;
    match dc.on_close with Some cb -> cb () | None -> ()
  ) pc.channels;
  Hashtbl.remove peer_connections pc.pc_id;
  log Log_debug (Printf.sprintf "[WebRTC] Closed PeerConnection %d" pc.pc_id)

let generate_session_id () =
  let uuid = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set uuid i (Char.chr (Random.int 256))
  done;
  let hex = Bytes.to_string uuid |> String.to_seq
    |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
    |> List.of_seq |> String.concat "" in
  String.sub hex 0 8 ^ "-" ^
  String.sub hex 8 4 ^ "-" ^
  String.sub hex 12 4 ^ "-" ^
  String.sub hex 16 4 ^ "-" ^
  String.sub hex 20 12

(** Generate stub SDP *)
let generate_stub_sdp ~is_offer pc =
  let session_id = generate_session_id () in
  let setup = if is_offer then "actpass" else "active" in
  Printf.sprintf {|v=0
o=- %s 2 IN IP4 127.0.0.1
s=MASC Voice Conference (Stub)
t=0 0
a=group:BUNDLE 0
a=msid-semantic: WMS
m=application 9 UDP/DTLS/SCTP webrtc-datachannel
c=IN IP4 0.0.0.0
a=ice-ufrag:stub%d
a=ice-pwd:stubpassword%d
a=fingerprint:sha-256 00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00
a=setup:%s
a=mid:0
a=sctp-port:5000
a=max-message-size:%d
|}
    session_id
    pc.pc_id pc.pc_id
    setup
    pc.config.max_message_size

(** Generate stub ICE candidate *)
let generate_stub_candidate pc =
  Printf.sprintf "candidate:1 1 UDP 2130706431 127.0.0.1 %d typ host"
    (9000 + pc.pc_id)

let set_local_description pc ~type_ =
  let is_offer = type_ = "offer" in
  let sdp = generate_stub_sdp ~is_offer pc in
  pc.local_sdp <- Some sdp;

  (* Simulate ICE gathering *)
  pc.gathering_state <- GatheringInProgress;
  (match pc.on_gathering_change with Some cb -> cb GatheringInProgress | None -> ());

  (* Generate local candidate *)
  let candidate = generate_stub_candidate pc in
  pc.local_candidates <- [(candidate, "0")];
  (match pc.on_local_candidate with Some cb -> cb ~candidate ~mid:"0" | None -> ());

  (* Complete gathering *)
  pc.gathering_state <- GatheringComplete;
  (match pc.on_gathering_change with Some cb -> cb GatheringComplete | None -> ());

  log Log_debug (Printf.sprintf "[WebRTC] PC %d: set local description (%s)" pc.pc_id type_);
  sdp

let set_remote_description pc ~sdp ~type_ =
  pc.remote_sdp <- Some sdp;

  (* Simulate connection establishment *)
  pc.ice_state <- Checking;
  (match pc.on_state_change with Some cb -> cb Checking | None -> ());

  (* Stub: immediately connect *)
  pc.ice_state <- Connected;
  (match pc.on_state_change with Some cb -> cb Connected | None -> ());

  (* Open any pending channels *)
  List.iter (fun dc ->
    if dc.state = Connecting then begin
      dc.state <- Open;
      match dc.on_open with Some cb -> cb () | None -> ()
    end
  ) pc.channels;

  log Log_debug (Printf.sprintf "[WebRTC] PC %d: set remote description (%s)" pc.pc_id type_);
  ()

let add_remote_candidate pc ~candidate ~mid =
  ignore (candidate, mid);  (* Stub: candidates are simulated *)
  log Log_debug (Printf.sprintf "[WebRTC] PC %d: added remote candidate" pc.pc_id)

let on_local_candidate pc callback =
  pc.on_local_candidate <- Some callback

let on_state_change pc callback =
  pc.on_state_change <- Some callback

let on_gathering_state_change pc callback =
  pc.on_gathering_change <- Some callback

let get_ice_state pc = pc.ice_state

let create_offer pc =
  set_local_description pc ~type_:"offer"

let create_answer pc =
  set_local_description pc ~type_:"answer"

(** {1 DataChannel} *)

let create_data_channel pc ~label ?(init = default_channel_init) () =
  let id = pc.next_channel_id in
  pc.next_channel_id <- pc.next_channel_id + 1;
  let dc = {
    id;
    label;
    protocol = init.protocol;
    reliability = init.reliability;
    state = Connecting;
    on_message = None;
    on_open = None;
    on_close = None;
    on_error = None;
    on_buffered_low = None;
    buffered = 0;
    buffered_threshold = 0;
    message_queue = [];
  } in
  pc.channels <- dc :: pc.channels;

  (* If already connected, open immediately *)
  if pc.ice_state = Connected || pc.ice_state = Completed then begin
    dc.state <- Open;
    (* Eio: synchronous callback - no async simulation needed for stub *)
    (match dc.on_open with Some cb -> cb () | None -> ())
  end;

  log Log_debug (Printf.sprintf "[WebRTC] PC %d: created DataChannel '%s' (id=%d)" pc.pc_id label id);
  dc

let on_data_channel pc callback =
  pc.on_data_channel <- Some callback

let close_data_channel dc =
  dc.state <- Closing;
  dc.state <- ChannelClosed;
  (match dc.on_close with Some cb -> cb () | None -> ())

let get_label dc = dc.label

let get_protocol dc = dc.protocol

let get_channel_state dc = dc.state

let is_open dc = dc.state = Open

(** {1 Data Transfer} *)

let send dc data =
  if dc.state <> Open then
    raise (Failure "DataChannel not open")
  else begin
    dc.buffered <- dc.buffered + Bytes.length data;
    (* Stub: add to message queue for loopback testing *)
    dc.message_queue <- dc.message_queue @ [(data, true)];

    (* Eio: synchronous buffer management - no async simulation needed for stub *)
    dc.buffered <- dc.buffered - Bytes.length data;
    if dc.buffered <= dc.buffered_threshold then
      (match dc.on_buffered_low with Some cb -> cb () | None -> ())
  end

let send_string dc data =
  send dc (Bytes.of_string data)

let on_message dc callback =
  dc.on_message <- Some callback

let on_open dc callback =
  dc.on_open <- Some callback;
  (* If already open, call immediately *)
  if dc.state = Open then callback ()

let on_close dc callback =
  dc.on_close <- Some callback

let on_error dc callback =
  dc.on_error <- Some callback

let buffered_amount dc = dc.buffered

let set_buffered_amount_low_threshold dc threshold =
  dc.buffered_threshold <- threshold

let on_buffered_amount_low dc callback =
  dc.on_buffered_low <- Some callback

(** {1 Stub: Loopback for Testing} *)

(** Deliver queued messages (for stub testing).
    In real WebRTC, messages come from the network.

    @param dc DataChannel to process
    @return Number of messages delivered *)
let deliver_queued_messages dc =
  let msgs = dc.message_queue in
  dc.message_queue <- [];
  List.iter (fun (data, is_binary) ->
    match dc.on_message with
    | Some cb -> cb data is_binary
    | None -> ()
  ) msgs;
  List.length msgs

(** Connect two PeerConnections for loopback testing.
    This simulates the signaling server exchange.

    @param pc1 First peer connection
    @param pc2 Second peer connection *)
let connect_loopback pc1 pc2 =
  let offer = create_offer pc1 in
  set_remote_description pc2 ~sdp:offer ~type_:"offer";
  let answer = create_answer pc2 in
  set_remote_description pc1 ~sdp:answer ~type_:"answer";

  (* Notify each side of the other's channels *)
  List.iter (fun dc ->
    match pc2.on_data_channel with
    | Some cb ->
      (* Create mirror channel on pc2 *)
      let dc2 = create_data_channel pc2 ~label:dc.label () in
      cb dc2
    | None -> ()
  ) pc1.channels

(** {1 Status} *)

let status_json pc =
  let reliability_json r =
    `Assoc [
      ("unordered", `Bool r.unordered);
      ("unreliable", `Bool r.unreliable);
      ("max_packet_lifetime_ms",
       match r.max_packet_lifetime_ms with Some ms -> `Int ms | None -> `Null);
      ("max_retransmits",
       match r.max_retransmits with Some n -> `Int n | None -> `Null);
    ]
  in
  let channels_json = List.map (fun dc ->
    `Assoc [
      ("id", `Int dc.id);
      ("label", `String dc.label);
      ("protocol", match dc.protocol with Some p -> `String p | None -> `Null);
      ("state", `String (string_of_channel_state dc.state));
      ("buffered", `Int dc.buffered);
      ("queued_messages", `Int (List.length dc.message_queue));
      ("reliability", reliability_json dc.reliability);
      ("has_error_callback", `Bool (Option.is_some dc.on_error));
    ]
  ) pc.channels in

  let candidates_json = List.map (fun (cand, mid) ->
    `Assoc [("candidate", `String cand); ("mid", `String mid)]
  ) pc.local_candidates in

  `Assoc [
    ("pc_id", `Int pc.pc_id);
    ("backend", `String (string_of_backend !current_backend));
    ("ice_state", `String (string_of_ice_state pc.ice_state));
    ("gathering_state", `String (string_of_gathering_state pc.gathering_state));
    ("has_local_sdp", `Bool (Option.is_some pc.local_sdp));
    ("has_remote_sdp", `Bool (Option.is_some pc.remote_sdp));
    ("local_candidates", `List candidates_json);
    ("channels", `List channels_json);
    ("max_message_size", `Int pc.config.max_message_size);
  ]
