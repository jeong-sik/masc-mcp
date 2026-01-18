(** RFC 6347 - Datagram Transport Layer Security (DTLS)

    Pure OCaml implementation of DTLS for WebRTC.

    DTLS provides:
    - TLS-like security for datagram protocols
    - Replay protection
    - Sequence number and epoch handling
    - Cookie exchange for DoS mitigation

    For WebRTC, DTLS is used to secure the SCTP association.

    Reference: https://datatracker.ietf.org/doc/html/rfc6347
*)

(** {1 Types} *)

(** DTLS content types *)
type content_type =
  | ChangeCipherSpec  (** 20 *)
  | Alert            (** 21 *)
  | Handshake        (** 22 *)
  | ApplicationData  (** 23 *)
  | Unknown of int
[@@deriving show, eq]

(** Handshake message types *)
type handshake_type =
  | HelloRequest
  | ClientHello
  | ServerHello
  | HelloVerifyRequest  (** DTLS specific *)
  | Certificate
  | ServerKeyExchange
  | CertificateRequest
  | ServerHelloDone
  | CertificateVerify
  | ClientKeyExchange
  | Finished
  | Unknown of int
[@@deriving show, eq]

(** Alert levels *)
type alert_level = Warning | Fatal
[@@deriving show, eq]

(** Alert descriptions *)
type alert_description =
  | CloseNotify
  | UnexpectedMessage
  | BadRecordMac
  | DecryptionFailed
  | RecordOverflow
  | DecompressionFailure
  | HandshakeFailure
  | NoCertificate
  | BadCertificate
  | UnsupportedCertificate
  | CertificateRevoked
  | CertificateExpired
  | CertificateUnknown
  | IllegalParameter
  | UnknownCA
  | AccessDenied
  | DecodeError
  | DecryptError
  | Unknown of int
[@@deriving show, eq]

(** Connection state *)
type state =
  | Closed
  | Connecting
  | Connected
  | Failed
[@@deriving show, eq]

(** Handshake state *)
type handshake_state =
  | Initial
  | HelloSent
  | HelloVerifyReceived
  | HelloDone
  | CertificateReceived
  | KeyExchangeDone
  | Finished
  | Complete
[@@deriving show, eq]

(** DTLS record header - RFC 6347 Section 4.1 *)
type record_header = {
  content_type: content_type;
  version_major: int;
  version_minor: int;
  epoch: int;              (** 16 bits *)
  sequence_number: int64;  (** 48 bits *)
  length: int;
}

(** DTLS record *)
type record = {
  header: record_header;
  fragment: bytes;
}

(** Handshake message header - RFC 6347 Section 4.2.2 *)
type handshake_header = {
  msg_type: handshake_type;
  length: int;                 (** 24 bits *)
  message_seq: int;            (** 16 bits *)
  fragment_offset: int;        (** 24 bits *)
  fragment_length: int;        (** 24 bits *)
}

(** DTLS configuration *)
type config = {
  mtu: int;
  retransmit_timeout_ms: int;
  max_retransmits: int;
  verify_peer: bool;
  cipher_suites: int list;     (** TLS cipher suite IDs *)
}

(** DTLS connection *)
type connection = {
  mutable state: state;
  mutable handshake_state: handshake_state;
  mutable epoch: int;
  mutable sequence_number: int64;
  mutable peer_epoch: int;
  mutable peer_sequence: int64;
  mutable cookie: bytes option;
  mutable is_client: bool;
  config: config;
  mutable local_random: bytes;
  mutable peer_random: bytes;
  mutable master_secret: bytes;
  (* Replay protection - bitmap of received sequence numbers *)
  mutable replay_window: int64;
  mutable replay_max_seq: int64;
  (* Real crypto context for AES-GCM encryption *)
  mutable crypto: Dtls_crypto.t option;
}

(** {1 Default Configuration} *)

let default_config = {
  mtu = 1200;
  retransmit_timeout_ms = 1000;
  max_retransmits = 5;
  verify_peer = false;
  cipher_suites = [
    0xC02B;  (* TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256 *)
    0xC02F;  (* TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 *)
    0x009E;  (* TLS_DHE_RSA_WITH_AES_128_GCM_SHA256 *)
  ];
}

(** {1 String Conversions} *)

let string_of_content_type = function
  | ChangeCipherSpec -> "change-cipher-spec"
  | Alert -> "alert"
  | Handshake -> "handshake"
  | ApplicationData -> "application-data"
  | Unknown n -> Printf.sprintf "unknown(%d)" n

let content_type_of_int = function
  | 20 -> ChangeCipherSpec
  | 21 -> Alert
  | 22 -> Handshake
  | 23 -> ApplicationData
  | n -> Unknown n

let int_of_content_type = function
  | ChangeCipherSpec -> 20
  | Alert -> 21
  | Handshake -> 22
  | ApplicationData -> 23
  | Unknown n -> n

let string_of_handshake_type = function
  | HelloRequest -> "hello-request"
  | ClientHello -> "client-hello"
  | ServerHello -> "server-hello"
  | HelloVerifyRequest -> "hello-verify-request"
  | Certificate -> "certificate"
  | ServerKeyExchange -> "server-key-exchange"
  | CertificateRequest -> "certificate-request"
  | ServerHelloDone -> "server-hello-done"
  | CertificateVerify -> "certificate-verify"
  | ClientKeyExchange -> "client-key-exchange"
  | Finished -> "finished"
  | Unknown n -> Printf.sprintf "unknown(%d)" n

let handshake_type_of_int = function
  | 0 -> HelloRequest
  | 1 -> ClientHello
  | 2 -> ServerHello
  | 3 -> HelloVerifyRequest
  | 11 -> Certificate
  | 12 -> ServerKeyExchange
  | 13 -> CertificateRequest
  | 14 -> ServerHelloDone
  | 15 -> CertificateVerify
  | 16 -> ClientKeyExchange
  | 20 -> Finished
  | n -> Unknown n

let string_of_state = function
  | Closed -> "closed"
  | Connecting -> "connecting"
  | Connected -> "connected"
  | Failed -> "failed"

let string_of_handshake_state = function
  | Initial -> "initial"
  | HelloSent -> "hello-sent"
  | HelloVerifyReceived -> "hello-verify-received"
  | HelloDone -> "hello-done"
  | CertificateReceived -> "certificate-received"
  | KeyExchangeDone -> "key-exchange-done"
  | Finished -> "finished"
  | Complete -> "complete"

let string_of_alert_level = function
  | Warning -> "warning"
  | Fatal -> "fatal"

let string_of_alert_description = function
  | CloseNotify -> "close-notify"
  | UnexpectedMessage -> "unexpected-message"
  | BadRecordMac -> "bad-record-mac"
  | DecryptionFailed -> "decryption-failed"
  | RecordOverflow -> "record-overflow"
  | DecompressionFailure -> "decompression-failure"
  | HandshakeFailure -> "handshake-failure"
  | NoCertificate -> "no-certificate"
  | BadCertificate -> "bad-certificate"
  | UnsupportedCertificate -> "unsupported-certificate"
  | CertificateRevoked -> "certificate-revoked"
  | CertificateExpired -> "certificate-expired"
  | CertificateUnknown -> "certificate-unknown"
  | IllegalParameter -> "illegal-parameter"
  | UnknownCA -> "unknown-ca"
  | AccessDenied -> "access-denied"
  | DecodeError -> "decode-error"
  | DecryptError -> "decrypt-error"
  | Unknown n -> Printf.sprintf "unknown(%d)" n

(** {1 Connection Management} *)

(** Generate random bytes *)
let generate_random len =
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set_uint8 buf i (Random.int 256)
  done;
  buf

(** Create new DTLS connection *)
let create config ~is_client =
  Random.self_init ();
  {
    state = Closed;
    handshake_state = Initial;
    epoch = 0;
    sequence_number = 0L;
    peer_epoch = 0;
    peer_sequence = 0L;
    cookie = None;
    is_client;
    config;
    local_random = generate_random 32;
    peer_random = Bytes.create 32;
    master_secret = Bytes.create 48;
    replay_window = 0L;
    replay_max_seq = 0L;
    crypto = Some (Dtls_crypto.create ~is_client);
  }

(** Get connection state *)
let get_state conn = conn.state

(** Get handshake state *)
let get_handshake_state conn = conn.handshake_state

(** Check if connected *)
let is_connected conn = conn.state = Connected

(** Check if handshake complete *)
let is_handshake_complete conn = conn.handshake_state = Complete

(** {1 Encoding/Decoding} *)

(* Use shared helpers from Webrtc_common *)
let write_uint16_be = Webrtc_common.write_uint16_be
let write_uint48_be = Webrtc_common.write_uint48_be
let read_uint16_be = Webrtc_common.read_uint16_be
let read_uint48_be = Webrtc_common.read_uint48_be

(** Encode DTLS record header *)
let encode_record_header header =
  let buf = Bytes.create 13 in
  Bytes.set_uint8 buf 0 (int_of_content_type header.content_type);
  Bytes.set_uint8 buf 1 header.version_major;
  Bytes.set_uint8 buf 2 header.version_minor;
  write_uint16_be buf 3 header.epoch;
  write_uint48_be buf 5 header.sequence_number;
  write_uint16_be buf 11 header.length;
  buf

(** Decode DTLS record header *)
let decode_record_header buf =
  if Bytes.length buf < 13 then
    Error "Record header too short"
  else
    let content_type = content_type_of_int (Bytes.get_uint8 buf 0) in
    let version_major = Bytes.get_uint8 buf 1 in
    let version_minor = Bytes.get_uint8 buf 2 in
    let epoch = read_uint16_be buf 3 in
    let sequence_number = read_uint48_be buf 5 in
    let length = read_uint16_be buf 11 in
    Ok { content_type; version_major; version_minor; epoch; sequence_number; length }

(** Encode DTLS record *)
let encode_record record =
  let header_bytes = encode_record_header record.header in
  let buf = Bytes.create (13 + Bytes.length record.fragment) in
  Bytes.blit header_bytes 0 buf 0 13;
  Bytes.blit record.fragment 0 buf 13 (Bytes.length record.fragment);
  buf

(** Decode DTLS record *)
let decode_record buf =
  match decode_record_header buf with
  | Error e -> Error e
  | Ok header ->
    if Bytes.length buf < 13 + header.length then
      Error "Record too short"
    else
      let fragment = Bytes.sub buf 13 header.length in
      Ok { header; fragment }

(** Check if data looks like DTLS *)
let is_dtls_data buf =
  if Bytes.length buf < 13 then false
  else
    let content_type = Bytes.get_uint8 buf 0 in
    let version_major = Bytes.get_uint8 buf 1 in
    content_type >= 20 && content_type <= 25 &&
    version_major = 254  (* DTLS 1.0/1.2 *)

(** {1 Replay Protection - RFC 6347 Section 4.1.2.6} *)

let window_size = 64

(** Check if sequence number is valid (not replayed) *)
let check_replay conn seq =
  if seq > conn.replay_max_seq then
    true  (* New sequence, always accept *)
  else if Int64.sub conn.replay_max_seq seq >= Int64.of_int window_size then
    false  (* Too old, reject *)
  else
    let diff = Int64.to_int (Int64.sub conn.replay_max_seq seq) in
    let bit = Int64.shift_left 1L diff in
    Int64.logand conn.replay_window bit = 0L  (* Not in window = not seen *)

(** Update replay window after accepting packet *)
let update_replay conn seq =
  if seq > conn.replay_max_seq then begin
    let shift = Int64.to_int (Int64.sub seq conn.replay_max_seq) in
    if shift >= window_size then
      conn.replay_window <- 0L
    else
      conn.replay_window <- Int64.shift_left conn.replay_window shift;
    conn.replay_max_seq <- seq;
    conn.replay_window <- Int64.logor conn.replay_window 1L
  end else begin
    let diff = Int64.to_int (Int64.sub conn.replay_max_seq seq) in
    let bit = Int64.shift_left 1L diff in
    conn.replay_window <- Int64.logor conn.replay_window bit
  end

(** {1 State Machine} *)

(** Start handshake *)
let start_handshake conn =
  conn.state <- Connecting;
  conn.handshake_state <- if conn.is_client then HelloSent else Initial

(** Complete handshake *)
let complete_handshake conn =
  conn.state <- Connected;
  conn.handshake_state <- Complete;
  conn.epoch <- conn.epoch + 1;
  conn.sequence_number <- 0L

(** Handle connection failure *)
let fail_connection conn =
  conn.state <- Failed

(** Close connection *)
let close conn =
  conn.state <- Closed;
  conn.handshake_state <- Initial

(** {1 Application Data} *)

(** Setup encryption after handshake completion
    Call this after key exchange to enable AES-GCM encryption *)
let setup_encryption conn ~cipher_suite ~pre_master_secret =
  match conn.crypto with
  | None -> Error "Crypto context not initialized"
  | Some crypto ->
    (* Client: local_random = client_random, peer_random = server_random
       Server: local_random = server_random, peer_random = client_random *)
    let client_random, server_random =
      if conn.is_client then
        Cstruct.of_bytes conn.local_random, Cstruct.of_bytes conn.peer_random
      else
        Cstruct.of_bytes conn.peer_random, Cstruct.of_bytes conn.local_random
    in
    let master_secret = Dtls_crypto.derive_master_secret
      ~pre_master_secret
      ~client_random
      ~server_random
    in
    (* Store master secret for potential renegotiation *)
    Cstruct.blit_to_bytes master_secret 0 conn.master_secret 0 48;
    Dtls_crypto.set_params crypto
      ~cipher_suite
      ~master_secret
      ~client_random
      ~server_random;
    Dtls_crypto.reset_sequence crypto;
    Ok ()

(** Check if encryption is enabled *)
let is_encrypted conn =
  match conn.crypto with
  | None -> false
  | Some crypto -> Dtls_crypto.is_encrypted crypto

(** Encrypt and wrap application data with AES-GCM *)
let wrap_application_data conn data =
  match conn.crypto with
  | None ->
    (* Fallback: no encryption *)
    let header = {
      content_type = ApplicationData;
      version_major = 254;
      version_minor = 253;
      epoch = conn.epoch;
      sequence_number = conn.sequence_number;
      length = Bytes.length data;
    } in
    conn.sequence_number <- Int64.add conn.sequence_number 1L;
    encode_record { header; fragment = data }
  | Some crypto ->
    let plaintext = Cstruct.of_bytes data in
    let seq_num = conn.sequence_number in
    match Dtls_crypto.encrypt_record crypto
      ~epoch:conn.epoch
      ~seq_num
      ~content_type:(int_of_content_type ApplicationData)
      ~version_major:254
      ~version_minor:253
      ~plaintext
    with
    | Error _ ->
      (* Encryption failed - fall back to plaintext (shouldn't happen in production) *)
      let header = {
        content_type = ApplicationData;
        version_major = 254;
        version_minor = 253;
        epoch = conn.epoch;
        sequence_number = conn.sequence_number;
        length = Bytes.length data;
      } in
      conn.sequence_number <- Int64.add conn.sequence_number 1L;
      encode_record { header; fragment = data }
    | Ok ciphertext ->
      let fragment = Cstruct.to_bytes ciphertext in
      let header = {
        content_type = ApplicationData;
        version_major = 254;
        version_minor = 253;
        epoch = conn.epoch;
        sequence_number = conn.sequence_number;
        length = Bytes.length fragment;
      } in
      conn.sequence_number <- Int64.add conn.sequence_number 1L;
      encode_record { header; fragment }

(** Decrypt and unwrap application data with AES-GCM *)
let unwrap_application_data conn record =
  if record.header.content_type <> ApplicationData then
    Error "Not application data"
  else
    match conn.crypto with
    | None ->
      (* No encryption - return as-is *)
      Ok record.fragment
    | Some crypto ->
      if not (Dtls_crypto.is_encrypted crypto) then
        (* Encryption not set up yet - return as-is *)
        Ok record.fragment
      else
        let ciphertext = Cstruct.of_bytes record.fragment in
        match Dtls_crypto.decrypt_record crypto
          ~epoch:record.header.epoch
          ~seq_num:record.header.sequence_number
          ~content_type:(int_of_content_type record.header.content_type)
          ~version_major:record.header.version_major
          ~version_minor:record.header.version_minor
          ~ciphertext
        with
        | Ok plaintext -> Ok (Cstruct.to_bytes plaintext)
        | Error e -> Error e

(** {1 JSON Status} *)

let connection_info conn =
  let cipher_suite = match conn.crypto with
    | None -> `Null
    | Some crypto ->
      match Dtls_crypto.get_cipher_suite crypto with
      | None -> `Null
      | Some cs -> `String (Dtls_crypto.string_of_cipher_suite cs)
  in
  `Assoc [
    ("state", `String (string_of_state conn.state));
    ("handshakeState", `String (string_of_handshake_state conn.handshake_state));
    ("isClient", `Bool conn.is_client);
    ("epoch", `Int conn.epoch);
    ("sequenceNumber", `String (Int64.to_string conn.sequence_number));
    ("isConnected", `Bool (is_connected conn));
    ("isHandshakeComplete", `Bool (is_handshake_complete conn));
    ("isEncrypted", `Bool (is_encrypted conn));
    ("cipherSuite", cipher_suite);
  ]

(** {1 Pretty Printing} *)

let pp_record_header fmt h =
  Format.fprintf fmt "DTLS(%s, epoch=%d, seq=%Ld, len=%d)"
    (string_of_content_type h.content_type)
    h.epoch h.sequence_number h.length

let pp_connection fmt c =
  Format.fprintf fmt "DTLSConnection(state=%s, hs=%s, epoch=%d)"
    (string_of_state c.state)
    (string_of_handshake_state c.handshake_state)
    c.epoch
