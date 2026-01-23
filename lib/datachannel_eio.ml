(** RFC 8831/8832 - WebRTC Data Channels with Eio-Native SCTP Transport

    Pure OCaml implementation integrating:
    - Datachannel module (DCEP protocol, compression)
    - Sctp_eio module (thread-safe SCTP association)

    This provides a complete, production-ready DataChannel stack:
    ```
    Application
        │
    ┌───▼───────────────────┐
    │  Datachannel_eio      │  ← This module
    │  (Channel management) │
    ├───────────────────────┤
    │  Datachannel          │  ← DCEP protocol + compression
    │  (RFC 8832)           │
    ├───────────────────────┤
    │  Sctp_eio             │  ← Thread-safe SCTP
    │  (RFC 4960)           │
    ├───────────────────────┤
    │  DTLS                 │  ← Encryption (dtls.ml)
    │  (RFC 6347)           │
    └───────────────────────┘
    ```

    @author Second Brain
    @since MASC v3.3
*)

(** {1 Types} *)

(** DataChannel connection with SCTP transport *)
type t = {
  dc_conn: Datachannel.connection;
  sctp: Sctp_eio.t;
  mutex: Eio.Mutex.t;
  mutable on_channel_open: (Datachannel.channel -> unit) option;
  mutable on_channel_close: (int -> unit) option;
  mutable on_message: (int -> bytes -> unit) option;
  mutable on_error: (string -> unit) option;
}

(** Configuration *)
type config = {
  dc_config: Datachannel.config;
  sctp_config: Sctp.config;
  is_offerer: bool;
}

(** {1 Default Configuration} *)

let default_config ~is_offerer = {
  dc_config = Datachannel.default_config;
  sctp_config = Sctp.default_config;
  is_offerer;
}

(** {1 Creation} *)

(** Create a new DataChannel connection with SCTP transport *)
let create ?(config = default_config ~is_offerer:true) () =
  let dc_conn = Datachannel.create config.dc_config ~is_offerer:config.is_offerer in
  let sctp = Sctp_eio.create ~config:config.sctp_config () in
  {
    dc_conn;
    sctp;
    mutex = Eio.Mutex.create ();
    on_channel_open = None;
    on_channel_close = None;
    on_message = None;
    on_error = None;
  }

(** {1 Connection Lifecycle} *)

(** Establish SCTP association and mark DataChannel connection as ready *)
let connect t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    (* Establish SCTP association *)
    Sctp_eio.establish t.sctp;
    (* Mark DataChannel connection as connected *)
    Datachannel.set_connected t.dc_conn
  )

(** Check if connection is established *)
let is_connected t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Sctp_eio.is_established t.sctp &&
    Datachannel.get_state t.dc_conn = Datachannel.Conn_connected
  )

(** Close connection *)
let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Sctp_eio.begin_shutdown t.sctp;
    Datachannel.close t.dc_conn
  )

(** {1 Channel Management} *)

(** Create a new data channel.
    For offerer: uses even stream IDs (0, 2, 4, ...)
    For answerer: uses odd stream IDs (1, 3, 5, ...) *)
let create_channel t ~label ?protocol ?channel_type ?priority ?negotiated ?id () =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Datachannel.create_channel t.dc_conn ~label ?protocol ?channel_type
            ?priority ?negotiated ?id () with
    | Error e -> Error e
    | Ok channel ->
      (* Open corresponding SCTP stream *)
      let _stream = Sctp_eio.open_stream t.sctp channel.id () in

      (* If not negotiated, queue DCEP OPEN message *)
      if not channel.negotiated then begin
        let open_msg = Datachannel.{
          channel_type = channel.channel_type;
          priority = channel.priority;
          reliability_param = Datachannel.reliability_param channel.channel_type;
          label = channel.label;
          protocol = channel.protocol;
        } in
        let open_data = Datachannel.encode_open_message open_msg in
        Datachannel.queue_outgoing t.dc_conn
          ~stream_id:channel.id
          ~data:open_data
          ~ppid:Datachannel.ppid_dcep
      end;
      Ok channel
  )

(** Get channel by ID *)
let get_channel t ~channel_id =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Datachannel.get_channel t.dc_conn ~channel_id
  )

(** Find channel by label *)
let find_channel_by_label t ~label =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Datachannel.find_channel_by_label t.dc_conn ~label
  )

(** Close a channel *)
let close_channel t ~channel_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Datachannel.close_channel t.dc_conn ~channel_id;
    Sctp_eio.close_stream t.sctp channel_id;
    match t.on_channel_close with
    | Some cb -> cb channel_id
    | None -> ()
  )

(** Get all channels *)
let get_channels t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Datachannel.get_channels t.dc_conn
  )

(** {1 Data Transfer} *)

(** Send text message on a channel.
    @param compress Enable zstd compression for large messages (default: false) *)
let send_text ?(compress = false) t ~channel_id text =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Datachannel.send_text ~compress t.dc_conn ~channel_id text
  )

(** Send binary message on a channel.
    @param compress Enable zstd compression for large messages (default: false) *)
let send_binary ?(compress = false) t ~channel_id data =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Datachannel.send_binary ~compress t.dc_conn ~channel_id data
  )

(** {1 SCTP Integration} *)

(** Flush outgoing messages to SCTP.
    Returns list of SCTP packets ready to send over DTLS. *)
let flush_outgoing t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let outgoing = Datachannel.get_outgoing t.dc_conn in
    List.map (fun (stream_id, data, ppid) ->
      (* Create SCTP DATA chunk *)
      let chunk = Sctp_eio.create_data_chunk t.sctp
        ~stream_id
        ~ppid
        ~data
        ()
      in
      (* Encode to bytes *)
      Sctp_eio.encode_data_chunk chunk
    ) outgoing
  )

(** Convert data_chunk to raw_chunk *)
let data_chunk_to_raw (chunk : Sctp.data_chunk) : Sctp.raw_chunk =
  let encoded = Sctp.encode_data_chunk chunk in
  {
    Sctp.chunk_type = 0;  (* DATA = 0 *)
    chunk_flags = Bytes.get_uint8 encoded 1;
    chunk_length = Bytes.length encoded;
    chunk_value = Bytes.sub encoded 4 (Bytes.length encoded - 4);
  }

(** Create complete SCTP packet from outgoing data.
    This wraps DATA chunks in a proper SCTP packet with header. *)
let create_sctp_packet t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let outgoing = Datachannel.get_outgoing t.dc_conn in
    if outgoing = [] then None
    else
      let chunks = List.map (fun (stream_id, data, ppid) ->
        let chunk = Sctp_eio.create_data_chunk t.sctp
          ~stream_id
          ~ppid
          ~data
          ()
        in
        data_chunk_to_raw chunk
      ) outgoing in
      let packet = Sctp_eio.create_packet t.sctp chunks in
      Some (Sctp_eio.encode_packet packet)
  )

(** Reconstruct full chunk bytes from raw_chunk for decoding *)
let raw_chunk_to_bytes (chunk : Sctp.raw_chunk) : bytes =
  let len = 4 + Bytes.length chunk.chunk_value in
  let buf = Bytes.create len in
  Bytes.set_uint8 buf 0 chunk.chunk_type;
  Bytes.set_uint8 buf 1 chunk.chunk_flags;
  Bytes.set_uint16_be buf 2 chunk.chunk_length;
  Bytes.blit chunk.chunk_value 0 buf 4 (Bytes.length chunk.chunk_value);
  buf

(** Process incoming SCTP packet.
    Returns list of DataChannel events. *)
let handle_sctp_packet t packet_bytes =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Sctp_eio.decode_packet packet_bytes with
    | Error e -> [Datachannel.Error e]
    | Ok packet ->
      (* Process each chunk *)
      List.concat_map (fun (chunk : Sctp.raw_chunk) ->
        (* Only process DATA chunks (type = 0) *)
        if chunk.chunk_type = 0 then
          let chunk_bytes = raw_chunk_to_bytes chunk in
          match Sctp_eio.decode_data_chunk chunk_bytes with
          | Error _ -> []
          | Ok data_chunk ->
            (* Process DATA chunk through SCTP *)
            Sctp_eio.process_data_chunk t.sctp data_chunk;

            (* Handle in DataChannel layer *)
            let events = Datachannel.handle_data t.dc_conn
              data_chunk.user_data
              ~stream_id:data_chunk.stream_id
            in

            (* Dispatch callbacks *)
            List.iter (function
              | Datachannel.ChannelOpen ch ->
                (match t.on_channel_open with Some cb -> cb ch | None -> ())
              | Datachannel.ChannelClose id ->
                (match t.on_channel_close with Some cb -> cb id | None -> ())
              | Datachannel.Message (id, data) ->
                (match t.on_message with Some cb -> cb id data | None -> ())
              | Datachannel.Error msg ->
                (match t.on_error with Some cb -> cb msg | None -> ())
            ) events;
            events
        else
          []  (* Ignore non-DATA chunks for now *)
      ) packet.chunks
  )

(** Check if data looks like SCTP *)
let is_sctp_data = Sctp_eio.is_sctp_data

(** {1 Event Callbacks} *)

(** Register callback for channel open events *)
let on_channel_open t callback =
  t.on_channel_open <- Some callback

(** Register callback for channel close events *)
let on_channel_close t callback =
  t.on_channel_close <- Some callback

(** Register callback for incoming messages *)
let on_message t callback =
  t.on_message <- Some callback

(** Register callback for errors *)
let on_error t callback =
  t.on_error <- Some callback

(** {1 Status and Debugging} *)

(** Get connection status as JSON *)
let status_json t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    let dc_info = Datachannel.connection_info t.dc_conn in
    let sctp_info = Sctp_eio.status_json t.sctp in
    let channels = Datachannel.get_channels t.dc_conn in
    `Assoc [
      ("datachannel", dc_info);
      ("sctp", sctp_info);
      ("channels", `List (List.map Datachannel.channel_info channels));
    ]
  )

(** Pretty print connection status *)
let pp fmt t =
  let json = status_json t in
  Format.fprintf fmt "%s" (Yojson.Safe.pretty_to_string json)

(** Get underlying SCTP association (for advanced use) *)
let get_sctp t = t.sctp

(** Get underlying DataChannel connection (for advanced use) *)
let get_dc_conn t = t.dc_conn
