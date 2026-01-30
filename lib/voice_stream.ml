(** MASC Voice Stream - Real-time Audio Delivery via WebSocket

    Implementation of WebSocket server for audio streaming.
    Uses httpun-ws-eio for persistent connections.

    MAGI Review Applied (2026-01-10):
    - P0: clients List -> Hashtbl for O(1) lookup
    - P1: Send failure -> client removal + logging
    - P1: Backpressure mechanism (max pending sends)

    @author Second Brain
    @since MASC v3.0
*)

module Ws = Httpun_ws
module Ws_eio = Httpun_ws_eio

(** {1 Configuration} *)

let max_pending_sends = 100  (* Backpressure: max queued sends per client *)

(** {1 Types} *)

type client = {
  id: string;
  mutable agent_filter: string option;  (* Mutable for subscription updates *)
  connected_at: float;
  mutable last_activity: float;
  mutable pending_sends: int;  (* Backpressure counter *)
}

(** Internal client with websocket handle *)
type internal_client = {
  client: client;
  wsd: Ws.Wsd.t;
  send_mutex: Eio.Mutex.t;
  mutable is_healthy: bool;  (* Mark unhealthy on send failure *)
  mutable partial_text: Buffer.t option;
}

type client_message =
  | Subscribe of string
  | Unsubscribe
  | Unknown of string

type server_event =
  | ClientConnected of client
  | ClientDisconnected of string
  | MessageReceived of string * client_message
  | ClientError of string * string  (* client_id, error_message *)

type any_listening_socket =
  | Listening_socket : 'a Eio.Net.listening_socket -> any_listening_socket

type t = {
  port: int;
  clients: (string, internal_client) Hashtbl.t;
  mutable is_running: bool;
  mutable should_stop: bool;
  mutable event_callback: (server_event -> unit) option;
  mutable server_socket: any_listening_socket option;
}

(** {1 Utilities} *)

let generate_client_id () =
  Random.self_init ();
  let high = Random.int 0xFFFF in
  let mid = Random.int 0xFFFF in
  let low = Random.int 0xFFFF in
  Printf.sprintf "ws-%08x-%04x%04x%04x"
    (Random.int 0x3FFFFFFF)
    high mid low

let parse_client_message s =
  try
    let json = Yojson.Safe.from_string s in
    let open Yojson.Safe.Util in
    let msg_type = json |> member "type" |> to_string_option in
    match msg_type with
    | Some "subscribe" ->
      let agent_id = json |> member "agent_id" |> to_string in
      Subscribe agent_id
    | Some "unsubscribe" -> Unsubscribe
    | _ -> Unknown s
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> Unknown s

(** {1 Creation} *)

let create ?(port = 8937) () =
  {
    port;
    clients = Hashtbl.create 64;  (* Initial capacity *)
    is_running = false;
    should_stop = false;
    event_callback = None;
    server_socket = None;
  }

(** {1 Internal Helpers} *)

let fire_event t event =
  match t.event_callback with
  | Some callback -> callback event
  | None -> ()

let find_internal_client t client_id =
  Hashtbl.find_opt t.clients client_id

let remove_client t client_id =
  if Hashtbl.mem t.clients client_id then begin
    Hashtbl.remove t.clients client_id;
    Log.info ~ctx:"voice_stream" "Client disconnected: %s (total: %d)" client_id (Hashtbl.length t.clients);
    fire_event t (ClientDisconnected client_id)
  end

let safe_close_client t ic =
  ic.is_healthy <- false;
  (try
     if not (Ws.Wsd.is_closed ic.wsd) then
       Ws.Wsd.close ic.wsd
   with exn -> Eio.traceln "[WARN] ws close: %s" (Printexc.to_string exn));
  remove_client t ic.client.id

let mark_client_error t ic message =
  fire_event t (ClientError (ic.client.id, message));
  safe_close_client t ic

let update_activity ic =
  ic.client.last_activity <- Unix.gettimeofday ()

let buffer_of_payload payload ~len ~on_complete =
  let buffer = Bytes.create len in
  let offset = ref 0 in
  let on_read bs ~off ~len =
    Bigstringaf.blit_to_bytes bs ~src_off:off buffer ~dst_off:!offset ~len;
    offset := !offset + len
  in
  let on_eof () =
    let slice =
      if !offset = len then buffer else Bytes.sub buffer 0 !offset
    in
    on_complete slice
  in
  Ws.Payload.schedule_read payload ~on_eof ~on_read

let handle_text_message t ic message =
  let parsed = parse_client_message message in
  update_activity ic;
  (match parsed with
   | Subscribe agent_id -> ic.client.agent_filter <- Some agent_id
   | Unsubscribe -> ic.client.agent_filter <- None
   | Unknown _ -> ());
  fire_event t (MessageReceived (ic.client.id, parsed))

let handle_text_payload t ic ~is_fin bytes =
  let chunk = Bytes.to_string bytes in
  match ic.partial_text, is_fin with
  | None, true -> handle_text_message t ic chunk
  | None, false ->
    let buf = Buffer.create (String.length chunk * 2) in
    Buffer.add_string buf chunk;
    ic.partial_text <- Some buf
  | Some buf, _ ->
    Buffer.add_string buf chunk;
    if is_fin then begin
      let message = Buffer.contents buf in
      ic.partial_text <- None;
      handle_text_message t ic message
    end

let handle_frame t ic ~opcode ~is_fin ~len payload =
  match (opcode : Ws.Websocket.Opcode.t) with
  | `Text | `Continuation ->
    buffer_of_payload payload ~len ~on_complete:(fun bytes ->
        handle_text_payload t ic ~is_fin bytes)
  | `Binary ->
    Ws.Payload.schedule_read payload ~on_eof:ignore ~on_read:(fun _ ~off:_ ~len:_ -> ())
  | `Ping -> Ws.Wsd.send_pong ic.wsd
  | `Pong -> ()
  | `Connection_close -> safe_close_client t ic
  | `Other _ ->
    Ws.Payload.schedule_read payload ~on_eof:ignore ~on_read:(fun _ ~off:_ ~len:_ -> ())

let handle_eof t ic = function
  | Some (`Exn exn) -> mark_client_error t ic (Printexc.to_string exn)
  | None -> safe_close_client t ic

let send_bytes t ic ~kind payload =
  if not ic.is_healthy then
    ()
  else if Ws.Wsd.is_closed ic.wsd then
    mark_client_error t ic "WebSocket closed"
  else if ic.client.pending_sends >= max_pending_sends then begin
    Log.warn ~ctx:"voice_stream" "Client %s exceeded backpressure limit, disconnecting" ic.client.id;
    mark_client_error t ic "Backpressure limit exceeded"
  end else begin
    ic.client.pending_sends <- ic.client.pending_sends + 1;
    let finish () =
      ic.client.pending_sends <- max 0 (ic.client.pending_sends - 1)
    in
    try
      Eio.Mutex.use_rw ~protect:true ic.send_mutex (fun () ->
        Ws.Wsd.send_bytes ic.wsd ~kind payload ~off:0 ~len:(Bytes.length payload);
        Ws.Wsd.flushed ic.wsd finish);
    with exn ->
      finish ();
      mark_client_error t ic (Printexc.to_string exn)
  end

let send_text t ic message =
  let payload = Bytes.of_string message in
  send_bytes t ic ~kind:`Text payload

let add_client t wsd =
  let client_id = generate_client_id () in
  let client = {
    id = client_id;
    agent_filter = None;
    connected_at = Unix.gettimeofday ();
    last_activity = Unix.gettimeofday ();
    pending_sends = 0;
  } in
  let ic = {
    client;
    wsd;
    send_mutex = Eio.Mutex.create ();
    is_healthy = true;
    partial_text = None;
  } in
  Hashtbl.add t.clients client_id ic;
  fire_event t (ClientConnected client);
  Log.info ~ctx:"voice_stream" "Client connected: %s (total: %d)" client_id (Hashtbl.length t.clients);
  ic

let websocket_handler t _client_addr wsd =
  let ic = add_client t wsd in
  let frame ~opcode ~is_fin ~len payload =
    handle_frame t ic ~opcode ~is_fin ~len payload
  in
  let eof ?error () =
    handle_eof t ic error
  in
  { Ws.Websocket_connection.frame; eof }

let ipaddr_of_host host =
  match Ipaddr.of_string host with
  | Ok addr -> Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets addr)
  | Error _ -> Eio.Net.Ipaddr.V4.loopback

(** {1 Server Lifecycle} *)

let start ~sw ~net ~clock t =
  if t.is_running then
    ()
  else begin
    t.is_running <- true;
    t.should_stop <- false;
    let addr = `Tcp (ipaddr_of_host "127.0.0.1", t.port) in
    let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:128 addr in
    t.server_socket <- Some (Listening_socket socket);
    let connection_handler =
      Ws_eio.Server.create_connection_handler ~sw (websocket_handler t)
    in
    Log.info ~ctx:"voice_stream" "WebSocket server starting on port %d" t.port;
    let rec accept_loop backoff_s =
      if t.should_stop then
        ()
      else
        try
          let flow, client_addr = Eio.Net.accept ~sw socket in
          Eio.Fiber.fork ~sw (fun () ->
            try connection_handler client_addr flow
            with exn ->
              Log.error ~ctx:"voice_stream" "Handler error: %s" (Printexc.to_string exn));
          accept_loop 0.05
        with exn ->
          if t.should_stop then
            ()
          else begin
            Log.error ~ctx:"voice_stream" "Accept error: %s" (Printexc.to_string exn);
            (try Eio.Time.sleep clock backoff_s
             with exn -> Eio.traceln "[WARN] sleep interrupted: %s" (Printexc.to_string exn));
            let next_backoff = Float.min 2.0 (backoff_s *. 1.5) in
            accept_loop next_backoff
          end
    in
    Eio.Fiber.fork ~sw (fun () -> accept_loop 0.05);
    Log.info ~ctx:"voice_stream" "WebSocket server started on port %d" t.port
  end

let stop t =
  t.should_stop <- true;
  t.is_running <- false;
  (match t.server_socket with
   | Some (Listening_socket socket) ->
     t.server_socket <- None;
     (try Eio.Resource.close socket
      with exn -> Eio.traceln "[WARN] socket close: %s" (Printexc.to_string exn))
   | None -> ());
  let clients = Hashtbl.fold (fun _ ic acc -> ic :: acc) t.clients [] in
  List.iter (fun ic -> safe_close_client t ic) clients;
  Hashtbl.clear t.clients;
  Log.info ~ctx:"voice_stream" "WebSocket server stopped"

let is_running t = t.is_running

(** {1 Broadcasting} *)

let broadcast_audio t audio =
  let clients = Hashtbl.fold (fun _ ic acc -> ic :: acc) t.clients [] in
  List.iter (fun ic -> send_bytes t ic ~kind:`Binary audio) clients

let send_to_agent_subscribers t ~agent_id audio =
  let clients = Hashtbl.fold (fun _ ic acc -> ic :: acc) t.clients [] in
  List.iter (fun ic ->
      match ic.client.agent_filter with
      | Some id when String.equal id agent_id ->
        send_bytes t ic ~kind:`Binary audio
      | _ -> ()) clients

let broadcast_text t message =
  let clients = Hashtbl.fold (fun _ ic acc -> ic :: acc) t.clients [] in
  List.iter (fun ic -> send_text t ic message) clients

let notify_speaking t ~agent_id ~voice =
  let msg =
    Printf.sprintf
      {|{"type":"speaking","agent_id":"%s","voice":"%s"}|}
      agent_id
      voice
  in
  broadcast_text t msg

let notify_done t ~agent_id ?duration_ms () =
  let msg =
    match duration_ms with
    | Some duration ->
      Printf.sprintf
        {|{"type":"done","agent_id":"%s","duration_ms":%d}|}
        agent_id
        duration
    | None ->
      Printf.sprintf
        {|{"type":"done","agent_id":"%s"}|}
        agent_id
  in
  broadcast_text t msg

(** {1 Client Management} *)

let client_count t = Hashtbl.length t.clients

let list_clients t =
  Hashtbl.fold (fun _id ic acc -> ic.client :: acc) t.clients []

let get_client t ~client_id =
  match find_internal_client t client_id with
  | Some ic -> Some ic.client
  | None -> None

let subscribe_to_agent t ~client_id ~agent_id =
  match find_internal_client t client_id with
  | Some ic ->
    ic.client.agent_filter <- Some agent_id;
    update_activity ic
  | None -> ()

let unsubscribe t ~client_id =
  match find_internal_client t client_id with
  | Some ic ->
    ic.client.agent_filter <- None;
    update_activity ic
  | None -> ()

let disconnect_client t ~client_id =
  match find_internal_client t client_id with
  | Some ic -> safe_close_client t ic
  | None -> ()

let cleanup_zombies t ?(timeout = Resilience.default_zombie_threshold) () =
  let now = Unix.gettimeofday () in
  let stale_before = now -. timeout in
  let to_remove = ref [] in
  Hashtbl.iter (fun _id ic ->
    if (not ic.is_healthy) || ic.client.last_activity < stale_before then
      to_remove := ic.client.id :: !to_remove) t.clients;
  List.iter (fun client_id -> disconnect_client t ~client_id) !to_remove;
  List.length !to_remove

(** {1 Event Handling} *)

let on_event t callback =
  t.event_callback <- Some callback

(** {1 Status} *)

let get_status_json t =
  let clients_json =
    list_clients t
    |> List.map (fun c ->
        Printf.sprintf
          {|{"id":"%s","agent_filter":%s,"connected_at":%.0f,"last_activity":%.0f,"pending_sends":%d}|}
          c.id
          (match c.agent_filter with
           | Some a -> Printf.sprintf "\"%s\"" a
           | None -> "null")
          c.connected_at
          c.last_activity
          c.pending_sends)
    |> String.concat ","
  in
  Printf.sprintf
    {|{"port":%d,"running":%s,"client_count":%d,"max_pending_sends":%d,"clients":[%s]}|}
    t.port
    (if t.is_running then "true" else "false")
    (client_count t)
    max_pending_sends
    clients_json
