(** RFC 8866 SDP - Session Description Protocol

    Pure OCaml implementation of SDP for WebRTC.

    SDP describes multimedia sessions for WebRTC:
    - Session metadata (origin, name)
    - Media descriptions (audio, video, data)
    - Connection info (ICE candidates)
    - Security (DTLS fingerprint)

    Implements:
    - RFC 8866: SDP (core protocol, 2021)
    - RFC 8839: SDP for ICE
    - RFC 8841: SDP for SCTP

    @author Second Brain
    @since MASC v3.1
*)

(** {1 Types} *)

(** Network type *)
type net_type =
  | IN  (** Internet *)

(** Convert network type to string *)
val string_of_net_type : net_type -> string

(** Parse network type from string *)
val net_type_of_string : string -> net_type

(** Address type *)
type addr_type =
  | IP4
  | IP6

(** Convert address type to string *)
val string_of_addr_type : addr_type -> string

(** Parse address type from string *)
val addr_type_of_string : string -> addr_type

(** Media type *)
type media_type =
  | Audio
  | Video
  | Application  (** For DataChannel *)
  | Text
  | Message

(** Transport protocol *)
type transport =
  | UDP
  | TCP
  | DTLS_SCTP    (** WebRTC DataChannel *)
  | UDP_DTLS_SCTP (** RFC 8841 *)
  | RTP_AVP
  | RTP_SAVP
  | RTP_SAVPF    (** WebRTC audio/video *)

(** Connection data (c=) *)
type connection = {
  net_type: net_type;
  addr_type: addr_type;
  address: string;
  ttl: int option;
  num_addresses: int option;
}

(** Origin (o=) *)
type origin = {
  username: string;
  sess_id: string;
  sess_version: string;
  net_type: net_type;
  addr_type: addr_type;
  unicast_address: string;
}

(** Bandwidth (b=) *)
type bandwidth = {
  bw_type: string;  (** CT, AS, TIAS *)
  bandwidth: int;
}

(** ICE candidate attribute *)
type ice_candidate = {
  foundation: string;
  component: int;
  transport: string;
  priority: int;
  address: string;
  port: int;
  cand_type: string;  (** host, srflx, prflx, relay *)
  rel_addr: string option;
  rel_port: int option;
  extensions: (string * string) list;
}

(** DTLS setup role *)
type setup_role =
  | Active    (** Client role *)
  | Passive   (** Server role *)
  | Actpass   (** Either (offer) *)
  | Holdconn  (** Hold connection *)

(** DTLS fingerprint *)
type fingerprint = {
  hash_func: string;  (** sha-256, sha-1 *)
  fingerprint: string;
}

(** Media direction *)
type direction =
  | Sendrecv
  | Sendonly
  | Recvonly
  | Inactive

(** SCTP port for DataChannel *)
type sctp_port = int

(** Max message size for DataChannel *)
type max_message_size = int

(** Media description (m=) *)
type media = {
  media_type: media_type;
  port: int;
  proto: transport;
  fmt: string list;  (** Format list *)

  (* Session-level can be overridden *)
  connection: connection option;
  bandwidth: bandwidth list;

  (* ICE attributes *)
  ice_ufrag: string option;
  ice_pwd: string option;
  ice_options: string list;
  ice_candidates: ice_candidate list;

  (* DTLS attributes *)
  setup: setup_role option;
  fingerprint: fingerprint option;

  (* RTP attributes *)
  rtcp: (int * string) option;  (** port, address *)
  rtcp_mux: bool;

  (* DataChannel (SCTP) attributes *)
  sctp_port: sctp_port option;
  max_message_size: max_message_size option;

  (* Direction *)
  direction: direction;

  (* Mid for bundle *)
  mid: string option;

  (* Generic attributes *)
  attributes: (string * string option) list;
}

(** Session description *)
type session = {
  version: int;  (** v= (always 0) *)
  origin: origin;
  name: string;  (** s= *)
  info: string option;  (** i= *)
  uri: string option;  (** u= *)
  email: string list;  (** e= *)
  phone: string list;  (** p= *)
  connection: connection option;  (** c= *)
  bandwidth: bandwidth list;  (** b= *)
  timing: (int * int);  (** t= start, stop *)

  (* ICE attributes (session-level) *)
  ice_lite: bool;
  ice_ufrag: string option;
  ice_pwd: string option;
  ice_options: string list;

  (* DTLS attributes (session-level) *)
  fingerprint: fingerprint option;

  (* Bundle group *)
  bundle_group: string list option;

  (* Media descriptions *)
  media: media list;

  (* Generic attributes *)
  attributes: (string * string option) list;
}

(** {1 Defaults} *)

(** Default media description (for incremental parsing) *)
val default_media : media

(** Default session (for incremental parsing) *)
val default_session : session

(** {1 Parsing} *)

(** Parse SDP string to session.
    @param sdp SDP text
    @return Parsed session or error *)
val parse : string -> (session, string) result

(** Parse single media description.
    @param lines SDP lines starting with m=
    @param media Initial media record to update
    @return Parsed media and remaining lines *)
val parse_media : string list -> media -> (media * string list, string) result

(** {1 Generation} *)

(** Generate SDP string from session.
    @param session Session description
    @return SDP text *)
val to_string : session -> string

(** Generate media description string.
    @param media Media description
    @return SDP text for this media *)
val media_to_string : media -> string

(** {1 WebRTC Helpers} *)

(** Create a WebRTC offer for DataChannel.
    @param ice_ufrag ICE username fragment
    @param ice_pwd ICE password
    @param fingerprint DTLS fingerprint
    @param sctp_port SCTP port (default: 5000)
    @return Session description *)
val create_datachannel_offer :
  ice_ufrag:string ->
  ice_pwd:string ->
  fingerprint:fingerprint ->
  ?sctp_port:int ->
  ?max_message_size:int ->
  unit ->
  session

(** Create a WebRTC answer from offer.
    @param offer Received offer
    @param ice_ufrag Local ICE username fragment
    @param ice_pwd Local ICE password
    @param fingerprint Local DTLS fingerprint
    @return Session description *)
val create_datachannel_answer :
  offer:session ->
  ice_ufrag:string ->
  ice_pwd:string ->
  fingerprint:fingerprint ->
  session

(** Add ICE candidate to media.
    @param media Media description
    @param candidate ICE candidate
    @return Updated media *)
val add_ice_candidate : media -> ice_candidate -> media

(** Extract ICE candidates from session.
    @param session Session description
    @return List of (mid, candidate) pairs *)
val get_ice_candidates : session -> (string * ice_candidate) list

(** {1 ICE Candidate Helpers} *)

(** Parse ICE candidate line (a=candidate:...).
    @param line Candidate attribute line
    @return Parsed candidate or error *)
val parse_ice_candidate : string -> (ice_candidate, string) result

(** Generate ICE candidate line.
    @param candidate ICE candidate
    @return Candidate attribute string (without "a=candidate:") *)
val ice_candidate_to_string : ice_candidate -> string

(** {1 Fingerprint Helpers} *)

(** Parse fingerprint (a=fingerprint:...).
    @param line Fingerprint attribute line
    @return Parsed fingerprint or error *)
val parse_fingerprint : string -> (fingerprint, string) result

(** Generate fingerprint line.
    @param fp Fingerprint
    @return Fingerprint string (without "a=fingerprint:") *)
val fingerprint_to_string : fingerprint -> string

(** {1 Utilities} *)

(** Get media by mid.
    @param session Session description
    @param mid Media ID
    @return Media description or None *)
val get_media_by_mid : session -> string -> media option

(** Check if session is WebRTC DataChannel compatible.
    @param session Session description
    @return true if contains SCTP/DTLS media *)
val is_datachannel_session : session -> bool

(** String representation of media type *)
val string_of_media_type : media_type -> string

(** Parse media type from string *)
val media_type_of_string : string -> media_type

(** String representation of transport *)
val string_of_transport : transport -> string

(** Parse transport from string *)
val transport_of_string : string -> transport

(** String representation of direction *)
val string_of_direction : direction -> string

(** Parse direction from string *)
val direction_of_string : string -> direction

(** String representation of setup role *)
val string_of_setup : setup_role -> string

(** Parse setup role from string *)
val setup_of_string : string -> setup_role

(** Split string at first occurrence of a delimiter *)
val split_first : char -> string -> string * string option

(** Split by spaces, removing empty segments *)
val split_spaces : string -> string list

(** Pretty-print session *)
val pp_session : Format.formatter -> session -> unit
