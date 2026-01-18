(** RFC 4960 SCTP - Eio-Native Implementation with Thread-Safe State

    This module provides an Eio-native wrapper around SCTP with:
    - Eio.Mutex for thread-safe state access
    - Direct-style async (no Lwt monads)
    - Stream multiplexing support

    The underlying SCTP logic is reused from sctp.ml, but all mutable
    state access is protected by a Mutex.

    @author Second Brain
    @since MASC v3.2
*)

open Sctp

(** {1 Thread-Safe SCTP Association} *)

(** SCTP association with Eio-native concurrency and Mutex protection *)
type t = {
  assoc: Sctp.association;
  mutex: Eio.Mutex.t;
  mutable on_state_change: (Sctp.state -> unit) option;
  mutable on_data_received: (Sctp.data_chunk -> unit) option;
  mutable on_stream_opened: (int -> unit) option;
  mutable on_stream_closed: (int -> unit) option;
}

(** Create a new thread-safe SCTP association *)
let create ?(config = Sctp.default_config) () =
  let assoc = Sctp.create config in
  {
    assoc;
    mutex = Eio.Mutex.create ();
    on_state_change = None;
    on_data_received = None;
    on_stream_opened = None;
    on_stream_closed = None;
  }

(** {1 State Access (Mutex-Protected)} *)

(** Get association state *)
let get_state t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Sctp.get_state t.assoc
  )

(** Check if association is established *)
let is_established t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Sctp.is_established t.assoc
  )

(** Get association info as JSON *)
let association_info t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Sctp.association_info t.assoc
  )

(** Get my verification tag *)
let get_my_vtag t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    t.assoc.my_vtag
  )

(** Get peer verification tag *)
let get_peer_vtag t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    t.assoc.peer_vtag
  )

(** Get next TSN *)
let get_next_tsn t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    t.assoc.next_tsn
  )

(** Get last received TSN *)
let get_last_rcvd_tsn t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    t.assoc.last_rcvd_tsn
  )

(** Get congestion window size *)
let get_cwnd t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    t.assoc.cwnd
  )

(** Get slow-start threshold *)
let get_ssthresh t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    t.assoc.ssthresh
  )

(** Get configuration *)
let get_config t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    t.assoc.config
  )

(** {1 Stream Management (Mutex-Protected)} *)

(** Open a stream *)
let open_stream t stream_id ?(ordered=true) () =
  let stream = Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Sctp.open_stream t.assoc stream_id ~ordered ()
  ) in
  (* Notify callback *)
  (match t.on_stream_opened with Some cb -> cb stream_id | None -> ());
  stream

(** Get stream by ID *)
let get_stream t stream_id =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Sctp.get_stream t.assoc stream_id
  )

(** Close a stream *)
let close_stream t stream_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Sctp.close_stream t.assoc stream_id
  );
  (* Notify callback *)
  (match t.on_stream_closed with Some cb -> cb stream_id | None -> ())

(** Get all streams *)
let get_streams t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Sctp.get_streams t.assoc
  )

(** Get stream count *)
let stream_count t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Hashtbl.length t.assoc.streams
  )

(** {1 State Mutation (Mutex-Protected)} *)

(** Set state and notify callback *)
let set_state t new_state =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Sctp.set_state t.assoc new_state
  );
  match t.on_state_change with
  | Some cb -> cb new_state
  | None -> ()

(** Establish association *)
let establish t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Sctp.establish t.assoc
  );
  (* Notify callback *)
  (match t.on_state_change with Some cb -> cb Established | None -> ())

(** Begin shutdown *)
let begin_shutdown t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Sctp.begin_shutdown t.assoc
  );
  (* Notify callback if state changed *)
  let state = get_state t in
  if state = Shutdown_pending then
    (match t.on_state_change with Some cb -> cb Shutdown_pending | None -> ())

(** Set peer verification tag *)
let set_peer_vtag t vtag =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.assoc.peer_vtag <- vtag
  )

(** Increment next TSN and return current value *)
let advance_tsn t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let current = t.assoc.next_tsn in
    t.assoc.next_tsn <- Int32.add current 1l;
    current
  )

(** Update congestion window *)
let set_cwnd t cwnd =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.assoc.cwnd <- cwnd
  )

(** Update slow-start threshold *)
let set_ssthresh t ssthresh =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.assoc.ssthresh <- ssthresh
  )

(** Update last received TSN *)
let update_last_rcvd_tsn t tsn =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.assoc.last_rcvd_tsn <- tsn
  )

(** {1 Data Encoding/Decoding (Pure - No Mutex)} *)

(** Encode DATA chunk *)
let encode_data_chunk = Sctp.encode_data_chunk

(** Decode DATA chunk *)
let decode_data_chunk = Sctp.decode_data_chunk

(** Encode SCTP packet *)
let encode_packet = Sctp.encode_packet

(** Decode SCTP packet *)
let decode_packet = Sctp.decode_packet

(** Check if data looks like SCTP *)
let is_sctp_data = Sctp.is_sctp_data

(** Calculate checksum (CRC32-C) *)
let calculate_checksum = Sctp.calculate_checksum

(** {1 Chunk Type Utilities (Pure)} *)

let string_of_chunk_type = Sctp.string_of_chunk_type
let chunk_type_of_int = Sctp.chunk_type_of_int
let int_of_chunk_type = Sctp.int_of_chunk_type

(** {1 State Utilities (Pure)} *)

let string_of_state = Sctp.string_of_state

(** {1 Message Type Utilities (Pure)} *)

let string_of_message_type = Sctp.string_of_message_type
let ppid_of_message_type = Sctp.ppid_of_message_type
let message_type_of_ppid = Sctp.message_type_of_ppid

(** {1 Event Callbacks} *)

(** Register callback for state changes *)
let on_state_change t callback =
  t.on_state_change <- Some callback

(** Register callback for received data *)
let on_data_received t callback =
  t.on_data_received <- Some callback

(** Register callback for stream opened *)
let on_stream_opened t callback =
  t.on_stream_opened <- Some callback

(** Register callback for stream closed *)
let on_stream_closed t callback =
  t.on_stream_closed <- Some callback

(** {1 Send/Receive Helpers (Mutex-Protected)} *)

(** Create a DATA chunk for sending.

    @param stream_id Stream identifier
    @param ppid Payload Protocol Identifier
    @param data User data to send
    @param ordered Whether to use ordered delivery
*)
let create_data_chunk t ~stream_id ~ppid ~data ?(ordered=true) () =
  let tsn = advance_tsn t in
  let stream_seq =
    match get_stream t stream_id with
    | Some s ->
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let seq = s.next_ssn in
        s.next_ssn <- seq + 1;
        seq
      )
    | None -> 0
  in
  {
    flags = {
      end_fragment = true;
      begin_fragment = true;
      unordered = not ordered;
      immediate = false;
    };
    tsn;
    stream_id;
    stream_seq;
    ppid;
    user_data = data;
  }

(** Process incoming DATA chunk *)
let process_data_chunk t chunk =
  (* Update last received TSN *)
  update_last_rcvd_tsn t chunk.tsn;
  (* Notify callback *)
  (match t.on_data_received with Some cb -> cb chunk | None -> ())

(** Create SCTP packet for sending *)
let create_packet t chunks =
  let config = get_config t in
  let my_vtag = get_my_vtag t in
  {
    header = {
      source_port = config.local_port;
      dest_port = config.remote_port;
      verification_tag = my_vtag;
      checksum = 0l;  (* Will be computed by encode_packet *)
    };
    chunks;
  }

(** {1 Concurrent Access Safety} *)

(** Safely iterate over all streams *)
let iter_streams t f =
  let streams = get_streams t in
  List.iter f streams

(** Map over all streams *)
let map_streams t f =
  let streams = get_streams t in
  List.map f streams

(** {1 Debug/Status} *)

(** Get association status as JSON *)
let status_json t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    let state = Sctp.string_of_state t.assoc.state in
    let streams = Hashtbl.fold (fun id s acc ->
      `Assoc [
        ("id", `Int id);
        ("ordered", `Bool s.ordered);
        ("next_ssn", `Int s.next_ssn);
      ] :: acc
    ) t.assoc.streams [] in
    `Assoc [
      ("state", `String state);
      ("my_vtag", `String (Int32.to_string t.assoc.my_vtag));
      ("peer_vtag", `String (Int32.to_string t.assoc.peer_vtag));
      ("next_tsn", `String (Int32.to_string t.assoc.next_tsn));
      ("last_rcvd_tsn", `String (Int32.to_string t.assoc.last_rcvd_tsn));
      ("cwnd", `Int t.assoc.cwnd);
      ("ssthresh", `Int t.assoc.ssthresh);
      ("stream_count", `Int (Hashtbl.length t.assoc.streams));
      ("streams", `List streams);
    ]
  )

(** Pretty print association status *)
let pp fmt t =
  let json = status_json t in
  Format.fprintf fmt "%s" (Yojson.Safe.pretty_to_string json)

(** Connection info for debugging *)
let connection_info t =
  association_info t
