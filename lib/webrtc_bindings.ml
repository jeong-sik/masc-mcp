(** WebRTC Native Bindings via ctypes

    Low-level FFI bindings to libdatachannel C API.
    This module provides direct access to the native WebRTC implementation.

    @see https://github.com/paullouisageneau/libdatachannel
*)

open Ctypes
open Foreign

(** {1 Constants} *)

let rtc_err_success = 0
let rtc_err_invalid = -1
let rtc_err_failure = -2

(** {1 Enums as integers} *)

(* rtcState *)
let rtc_new = 0
let rtc_connecting = 1
let rtc_connected = 2
let rtc_disconnected = 3
let rtc_failed = 4
let rtc_closed = 5

(* rtcIceState *)
let rtc_ice_new = 0
let rtc_ice_checking = 1
let rtc_ice_connected = 2
let rtc_ice_completed = 3
let rtc_ice_failed = 4
let rtc_ice_disconnected = 5
let rtc_ice_closed = 6

(* rtcGatheringState *)
let rtc_gathering_new = 0
let rtc_gathering_inprogress = 1
let rtc_gathering_complete = 2

(* rtcLogLevel *)
let rtc_log_none = 0
let rtc_log_fatal = 1
let rtc_log_error = 2
let rtc_log_warning = 3
let rtc_log_info = 4
let rtc_log_debug = 5
let rtc_log_verbose = 6

(** {1 Structures} *)

(* rtcReliability structure *)
type rtc_reliability
let rtc_reliability : rtc_reliability structure typ = structure "rtcReliability"
let reliability_unordered = field rtc_reliability "unordered" bool
let reliability_unreliable = field rtc_reliability "unreliable" bool
let reliability_max_packet_life_time = field rtc_reliability "maxPacketLifeTime" uint
let reliability_max_retransmits = field rtc_reliability "maxRetransmits" uint
let () = seal rtc_reliability

(* rtcDataChannelInit structure *)
type rtc_data_channel_init
let rtc_data_channel_init : rtc_data_channel_init structure typ = structure "rtcDataChannelInit"
let dc_init_reliability = field rtc_data_channel_init "reliability" rtc_reliability
let dc_init_protocol = field rtc_data_channel_init "protocol" string_opt
let dc_init_negotiated = field rtc_data_channel_init "negotiated" bool
let dc_init_manual_stream = field rtc_data_channel_init "manualStream" bool
let dc_init_stream = field rtc_data_channel_init "stream" uint16_t
let () = seal rtc_data_channel_init

(* rtcConfiguration structure *)
type rtc_configuration
let rtc_configuration : rtc_configuration structure typ = structure "rtcConfiguration"
let config_ice_servers = field rtc_configuration "iceServers" (ptr (ptr char))
let config_ice_servers_count = field rtc_configuration "iceServersCount" int
let config_proxy_server = field rtc_configuration "proxyServer" string_opt
let config_bind_address = field rtc_configuration "bindAddress" string_opt
let config_certificate_type = field rtc_configuration "certificateType" int
let config_ice_transport_policy = field rtc_configuration "iceTransportPolicy" int
let config_enable_ice_tcp = field rtc_configuration "enableIceTcp" bool
let config_enable_ice_udp_mux = field rtc_configuration "enableIceUdpMux" bool
let config_disable_auto_negotiation = field rtc_configuration "disableAutoNegotiation" bool
let config_force_media_transport = field rtc_configuration "forceMediaTransport" bool
let config_port_range_begin = field rtc_configuration "portRangeBegin" uint16_t
let config_port_range_end = field rtc_configuration "portRangeEnd" uint16_t
let config_mtu = field rtc_configuration "mtu" int
let config_max_message_size = field rtc_configuration "maxMessageSize" int
let () = seal rtc_configuration

(** {1 Callback types} *)

type log_callback = int -> string -> unit
type description_callback = int -> string -> string -> unit
type candidate_callback = int -> string -> string -> unit
type state_change_callback = int -> int -> unit ptr -> unit
type ice_state_change_callback = int -> int -> unit ptr -> unit
type gathering_state_callback = int -> int -> unit ptr -> unit
type data_channel_callback = int -> int -> unit ptr -> unit
type open_callback = int -> unit ptr -> unit
type closed_callback = int -> unit ptr -> unit
type error_callback = int -> string -> unit ptr -> unit
type message_callback = int -> string -> int -> unit ptr -> unit

(** {1 Library loading} *)

let lib_path =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
  home ^ "/local/lib/libdatachannel.dylib"

let lib =
  try Some (Dl.dlopen ~filename:lib_path ~flags:[Dl.RTLD_NOW; Dl.RTLD_GLOBAL])
  with _ -> None

let is_available () = Option.is_some lib

(** {1 Function bindings} *)

(* Only bind functions if library is available *)
let rtc_init_logger =
  match lib with
  | Some _ -> Some (foreign "rtcInitLogger" (int @-> funptr_opt (int @-> string @-> returning void) @-> returning void))
  | None -> None

let rtc_create_peer_connection =
  match lib with
  | Some _ -> Some (foreign "rtcCreatePeerConnection" (ptr rtc_configuration @-> returning int))
  | None -> None

let rtc_close_peer_connection =
  match lib with
  | Some _ -> Some (foreign "rtcClosePeerConnection" (int @-> returning int))
  | None -> None

let rtc_delete_peer_connection =
  match lib with
  | Some _ -> Some (foreign "rtcDeletePeerConnection" (int @-> returning int))
  | None -> None

let rtc_set_local_description =
  match lib with
  | Some _ -> Some (foreign "rtcSetLocalDescription" (int @-> string_opt @-> returning int))
  | None -> None

let rtc_set_remote_description =
  match lib with
  | Some _ -> Some (foreign "rtcSetRemoteDescription" (int @-> string @-> string @-> returning int))
  | None -> None

let rtc_add_remote_candidate =
  match lib with
  | Some _ -> Some (foreign "rtcAddRemoteCandidate" (int @-> string @-> string @-> returning int))
  | None -> None

let rtc_get_local_description =
  match lib with
  | Some _ -> Some (foreign "rtcGetLocalDescription" (int @-> ptr char @-> int @-> returning int))
  | None -> None

let rtc_create_data_channel =
  match lib with
  | Some _ -> Some (foreign "rtcCreateDataChannel" (int @-> string @-> returning int))
  | None -> None

let rtc_create_data_channel_ex =
  match lib with
  | Some _ -> Some (foreign "rtcCreateDataChannelEx" (int @-> string @-> ptr rtc_data_channel_init @-> returning int))
  | None -> None

let rtc_delete_data_channel =
  match lib with
  | Some _ -> Some (foreign "rtcDeleteDataChannel" (int @-> returning int))
  | None -> None

let rtc_get_data_channel_label =
  match lib with
  | Some _ -> Some (foreign "rtcGetDataChannelLabel" (int @-> ptr char @-> int @-> returning int))
  | None -> None

let rtc_get_data_channel_protocol =
  match lib with
  | Some _ -> Some (foreign "rtcGetDataChannelProtocol" (int @-> ptr char @-> int @-> returning int))
  | None -> None

let rtc_send_message =
  match lib with
  | Some _ -> Some (foreign "rtcSendMessage" (int @-> string @-> int @-> returning int))
  | None -> None

let rtc_is_open =
  match lib with
  | Some _ -> Some (foreign "rtcIsOpen" (int @-> returning bool))
  | None -> None

let rtc_is_closed =
  match lib with
  | Some _ -> Some (foreign "rtcIsClosed" (int @-> returning bool))
  | None -> None

let rtc_get_buffered_amount =
  match lib with
  | Some _ -> Some (foreign "rtcGetBufferedAmount" (int @-> returning int))
  | None -> None

let rtc_set_buffered_amount_low_threshold =
  match lib with
  | Some _ -> Some (foreign "rtcSetBufferedAmountLowThreshold" (int @-> int @-> returning int))
  | None -> None

(* Callback setters - these need special handling for function pointers *)
let rtc_set_open_callback =
  match lib with
  | Some _ -> Some (foreign "rtcSetOpenCallback" (int @-> funptr_opt (int @-> ptr void @-> returning void) @-> returning int))
  | None -> None

let rtc_set_closed_callback =
  match lib with
  | Some _ -> Some (foreign "rtcSetClosedCallback" (int @-> funptr_opt (int @-> ptr void @-> returning void) @-> returning int))
  | None -> None

let rtc_set_error_callback =
  match lib with
  | Some _ -> Some (foreign "rtcSetErrorCallback" (int @-> funptr_opt (int @-> string @-> ptr void @-> returning void) @-> returning int))
  | None -> None

let rtc_set_message_callback =
  match lib with
  | Some _ -> Some (foreign "rtcSetMessageCallback" (int @-> funptr_opt (int @-> string @-> int @-> ptr void @-> returning void) @-> returning int))
  | None -> None

let rtc_set_local_candidate_callback =
  match lib with
  | Some _ -> Some (foreign "rtcSetLocalCandidateCallback" (int @-> funptr_opt (int @-> string @-> string @-> ptr void @-> returning void) @-> returning int))
  | None -> None

let rtc_set_state_change_callback =
  match lib with
  | Some _ -> Some (foreign "rtcSetStateChangeCallback" (int @-> funptr_opt (int @-> int @-> ptr void @-> returning void) @-> returning int))
  | None -> None

let rtc_set_ice_state_change_callback =
  match lib with
  | Some _ -> Some (foreign "rtcSetIceStateChangeCallback" (int @-> funptr_opt (int @-> int @-> ptr void @-> returning void) @-> returning int))
  | None -> None

let rtc_set_gathering_state_change_callback =
  match lib with
  | Some _ -> Some (foreign "rtcSetGatheringStateChangeCallback" (int @-> funptr_opt (int @-> int @-> ptr void @-> returning void) @-> returning int))
  | None -> None

let rtc_set_data_channel_callback =
  match lib with
  | Some _ -> Some (foreign "rtcSetDataChannelCallback" (int @-> funptr_opt (int @-> int @-> ptr void @-> returning void) @-> returning int))
  | None -> None

(** {1 Helper functions} *)

let call_if_available f default =
  match f with
  | Some fn -> fn
  | None -> fun _ -> default

let get_string_result fn id buf_size =
  match fn with
  | Some f ->
    let buf = allocate_n char ~count:buf_size in
    let len = f id buf buf_size in
    if len > 0 then Some (string_from_ptr buf ~length:len)
    else None
  | None -> None
