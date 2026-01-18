(** MASC WebRTC DataChannel - NAT Traversal and P2P Audio

    Provides WebRTC DataChannel support for peer-to-peer audio streaming.
    Complements Voice_stream's WebSocket server with P2P capabilities.

    Architecture:
    {v
    ┌──────────────┐     ┌──────────────┐     ┌──────────────┐
    │ Agent A      │────▶│ STUN/TURN    │◀────│ Agent B      │
    │ (DataChannel)│     │ Server       │     │ (DataChannel)│
    └──────────────┘     └──────────────┘     └──────────────┘
           │                                          │
           └──────────── P2P Audio Stream ───────────┘
    v}

    Backend Options:
    - Native: libdatachannel C bindings (requires system library)
    - Stub: WebSocket fallback (always available)

    Based on libdatachannel C API:
    https://github.com/paullouisageneau/libdatachannel

    @author Second Brain
    @since MASC v3.0
*)

(** {1 Types} *)

(** Log levels for WebRTC stack *)
type log_level =
  | Log_none
  | Log_fatal
  | Log_error
  | Log_warning
  | Log_info
  | Log_debug
  | Log_verbose

(** ICE connection state *)
type ice_state =
  | New
  | Checking
  | Connected
  | Completed
  | Failed
  | Disconnected
  | Closed

(** ICE gathering state *)
type gathering_state =
  | GatheringNew
  | GatheringInProgress
  | GatheringComplete

(** DataChannel state *)
type channel_state =
  | Connecting
  | Open
  | Closing
  | ChannelClosed

(** DataChannel reliability options *)
type reliability = {
  unordered: bool;          (** Allow out-of-order delivery *)
  unreliable: bool;         (** Allow packet loss *)
  max_packet_lifetime_ms: int option;  (** Max age before discard *)
  max_retransmits: int option;         (** Max retransmission attempts *)
}

(** DataChannel initialization options *)
type channel_init = {
  reliability: reliability;
  protocol: string option;   (** Sub-protocol (e.g., "audio", "json") *)
  negotiated: bool;          (** Pre-negotiated channel *)
  stream_id: int option;     (** Manual stream ID *)
}

(** ICE server configuration *)
type ice_server = {
  urls: string list;         (** STUN/TURN URLs *)
  username: string option;   (** TURN username *)
  credential: string option; (** TURN credential *)
}

(** Peer connection configuration *)
type config = {
  ice_servers: ice_server list;
  ice_transport_policy: [`All | `Relay];  (** Use all or only relay *)
  enable_ice_tcp: bool;      (** Enable ICE over TCP *)
  port_range: (int * int) option;  (** UDP port range *)
  max_message_size: int;     (** Max DataChannel message size *)
}

(** Peer connection handle (opaque) *)
type peer_connection

(** DataChannel handle (opaque) *)
type data_channel

(** Backend implementation type *)
type backend =
  | Native    (** libdatachannel C bindings *)
  | Stub      (** WebSocket fallback *)

(** {1 Configuration Defaults} *)

(** Default reliability (ordered, reliable) *)
val default_reliability : reliability

(** Default channel init *)
val default_channel_init : channel_init

(** Default configuration with public STUN servers *)
val default_config : config

(** Audio-optimized reliability (unordered, some loss allowed) *)
val audio_reliability : reliability

(** {1 Initialization} *)

(** Initialize the WebRTC subsystem.
    Must be called before any other functions.

    @param level Log level (default: Warning)
    @param log_callback Optional callback for log messages
    @param prefer_native If true (default), use native libdatachannel when available
    @return Currently active backend type *)
val init : ?level:log_level -> ?log_callback:(log_level -> string -> unit) -> ?prefer_native:bool -> unit -> backend

(** Check if native WebRTC is available.

    @return true if libdatachannel is linked *)
val is_native_available : unit -> bool

(** Get current backend type.

    @return Active backend *)
val get_backend : unit -> backend

(** Cleanup WebRTC resources.
    Call before program exit. *)
val cleanup : unit -> unit

(** {1 Peer Connection} *)

(** Create a new peer connection.

    @param config Connection configuration
    @return New peer connection *)
val create_peer_connection : ?config:config -> unit -> peer_connection

(** Close a peer connection.

    @param pc Peer connection to close *)
val close_peer_connection : peer_connection -> unit

(** Set local description (creates offer/answer).

    @param pc Peer connection
    @param type_ "offer" or "answer"
    @return SDP string Lwt promise *)
val set_local_description : peer_connection -> type_:string -> string Lwt.t

(** Set remote description (from signaling).

    @param pc Peer connection
    @param sdp SDP string
    @param type_ "offer" or "answer" *)
val set_remote_description : peer_connection -> sdp:string -> type_:string -> unit Lwt.t

(** Add ICE candidate from signaling.

    @param pc Peer connection
    @param candidate ICE candidate string
    @param mid Media ID *)
val add_remote_candidate : peer_connection -> candidate:string -> mid:string -> unit

(** Get local ICE candidates as they're gathered.

    @param pc Peer connection
    @param callback Called for each candidate *)
val on_local_candidate : peer_connection -> (candidate:string -> mid:string -> unit) -> unit

(** Set state change callback.

    @param pc Peer connection
    @param callback Called on state changes *)
val on_state_change : peer_connection -> (ice_state -> unit) -> unit

(** Set gathering state callback.

    @param pc Peer connection
    @param callback Called on gathering state changes *)
val on_gathering_state_change : peer_connection -> (gathering_state -> unit) -> unit

(** Get current ICE state.

    @param pc Peer connection
    @return Current state *)
val get_ice_state : peer_connection -> ice_state

(** {1 DataChannel} *)

(** Create a DataChannel.

    @param pc Peer connection
    @param label Channel label (identifier)
    @param init Channel options
    @return New DataChannel *)
val create_data_channel : peer_connection -> label:string -> ?init:channel_init -> unit -> data_channel

(** Set callback for incoming DataChannels.

    @param pc Peer connection
    @param callback Called when remote peer creates a channel *)
val on_data_channel : peer_connection -> (data_channel -> unit) -> unit

(** Close a DataChannel.

    @param dc DataChannel to close *)
val close_data_channel : data_channel -> unit

(** Get DataChannel label.

    @param dc DataChannel
    @return Label string *)
val get_label : data_channel -> string

(** Get DataChannel protocol.

    @param dc DataChannel
    @return Protocol string option *)
val get_protocol : data_channel -> string option

(** Get DataChannel state.

    @param dc DataChannel
    @return Current state *)
val get_channel_state : data_channel -> channel_state

(** Check if DataChannel is open.

    @param dc DataChannel
    @return true if open and ready *)
val is_open : data_channel -> bool

(** {1 Data Transfer} *)

(** Send binary data.

    @param dc DataChannel
    @param data Bytes to send
    @return Unit Lwt promise *)
val send : data_channel -> bytes -> unit Lwt.t

(** Send string data.

    @param dc DataChannel
    @param data String to send
    @return Unit Lwt promise *)
val send_string : data_channel -> string -> unit Lwt.t

(** Set message callback.

    @param dc DataChannel
    @param callback Called on incoming messages (data, is_binary) *)
val on_message : data_channel -> (bytes -> bool -> unit) -> unit

(** Set open callback.

    @param dc DataChannel
    @param callback Called when channel opens *)
val on_open : data_channel -> (unit -> unit) -> unit

(** Set close callback.

    @param dc DataChannel
    @param callback Called when channel closes *)
val on_close : data_channel -> (unit -> unit) -> unit

(** Set error callback.

    @param dc DataChannel
    @param callback Called on errors *)
val on_error : data_channel -> (string -> unit) -> unit

(** Get buffered amount.

    @param dc DataChannel
    @return Bytes waiting to be sent *)
val buffered_amount : data_channel -> int

(** Set buffered amount low threshold.

    @param dc DataChannel
    @param threshold Byte threshold *)
val set_buffered_amount_low_threshold : data_channel -> int -> unit

(** Set buffered amount low callback.

    @param dc DataChannel
    @param callback Called when buffer drains below threshold *)
val on_buffered_amount_low : data_channel -> (unit -> unit) -> unit

(** {1 Signaling Helpers} *)

(** Generate a random session ID for signaling.

    @return Unique session ID *)
val generate_session_id : unit -> string

(** Create offer SDP.

    @param pc Peer connection
    @return Offer SDP string Lwt promise *)
val create_offer : peer_connection -> string Lwt.t

(** Create answer SDP.

    @param pc Peer connection
    @return Answer SDP string Lwt promise *)
val create_answer : peer_connection -> string Lwt.t

(** {1 Status} *)

(** Get status as JSON.

    @param pc Peer connection
    @return JSON with state, candidates, channels *)
val status_json : peer_connection -> Yojson.Safe.t

(** String representation of ICE state *)
val string_of_ice_state : ice_state -> string

(** String representation of gathering state *)
val string_of_gathering_state : gathering_state -> string

(** String representation of channel state *)
val string_of_channel_state : channel_state -> string

(** String representation of backend type *)
val string_of_backend : backend -> string

(** {1 Stub Testing Helpers} *)

(** Deliver queued messages for loopback testing.
    In stub mode, messages are queued locally. This function
    delivers them to the message callback.

    @param dc DataChannel to process
    @return Number of messages delivered *)
val deliver_queued_messages : data_channel -> int

(** Connect two PeerConnections for loopback testing.
    Simulates the signaling server exchange.

    @param pc1 First peer connection (offerer)
    @param pc2 Second peer connection (answerer)
    @return Unit Lwt promise *)
val connect_loopback : peer_connection -> peer_connection -> unit Lwt.t
