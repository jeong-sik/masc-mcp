(** MASC Voice Stream - Real-time Audio Delivery via WebSocket

    Implementation of WebSocket server for audio streaming.
    Uses websocket-lwt-unix for persistent connections.

    MAGI Review Applied (2026-01-10):
    - P0: clients List -> Hashtbl for O(1) lookup
    - P1: Send failure -> client removal + logging
    - P1: Backpressure mechanism (max pending sends)

    @author Second Brain
    @since MASC v3.0
*)

module Ws = Websocket
module Ws_lwt = Websocket_lwt_unix

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

(** Internal client with connected_client handle *)
type internal_client = {
  client: client;
  conn: Ws_lwt.Connected_client.t;
  mutable is_healthy: bool;  (* Mark unhealthy on send failure *)
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

type t = {
  port: int;
  clients: (string, internal_client) Hashtbl.t;  (* P0: Hashtbl for O(1) lookup *)
  mutable is_running: bool;
  mutable should_stop: bool;
  mutable event_callback: (server_event -> unit) option;
  mutable _server_handle: Conduit_lwt_unix.server Lwt.t option;
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
  with _ -> Unknown s

(** {1 Creation} *)

let create ?(port = 8937) () =
  {
    port;
    clients = Hashtbl.create 64;  (* Initial capacity *)
    is_running = false;
    should_stop = false;
    event_callback = None;
    _server_handle = None;
  }

(** {1 Internal Helpers} *)

let fire_event t event =
  match t.event_callback with
  | Some callback -> callback event
  | None -> ()

let find_internal_client t client_id =
  Hashtbl.find_opt t.clients client_id

let remove_client t client_id =
  Hashtbl.remove t.clients client_id;
  fire_event t (ClientDisconnected client_id)

let make_text_frame content =
  Ws.Frame.create
    ~opcode:Ws.Frame.Opcode.Text
    ~content
    ()

let make_binary_frame content =
  Ws.Frame.create
    ~opcode:Ws.Frame.Opcode.Binary
    ~content
    ()

let make_close_frame () =
  Ws.Frame.close 1000

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
    ic.client.last_activity <- Unix.gettimeofday ()
  | None -> ()

let unsubscribe t ~client_id =
  match find_internal_client t client_id with
  | Some ic ->
    ic.client.agent_filter <- None;
    ic.client.last_activity <- Unix.gettimeofday ()
  | None -> ()

let disconnect_client t ~client_id =
  let open Lwt.Syntax in
  match find_internal_client t client_id with
  | Some ic ->
    remove_client t client_id;
    let* () =
      Lwt.catch
        (fun () -> Ws_lwt.Connected_client.send ic.conn (make_close_frame ()))
        (fun _exn -> Lwt.return_unit)
    in
    Lwt.return_unit
  | None -> Lwt.return_unit

let cleanup_zombies t ?(timeout = 300.0) () =
  let now = Unix.gettimeofday () in
  let to_remove = Hashtbl.fold (fun id ic acc ->
    if now -. ic.client.last_activity > timeout || not ic.is_healthy then
      id :: acc
    else
      acc
  ) t.clients [] in
  List.iter (fun id -> remove_client t id) to_remove;
  List.length to_remove

(** {1 Safe Send with Error Handling} *)

(** P1: Send with failure handling - removes client on error *)
let safe_send t ic frame =
  let open Lwt.Syntax in
  (* Backpressure check *)
  if ic.client.pending_sends >= max_pending_sends then begin
    Log.warn ~ctx:"voice_stream" "Client %s exceeded backpressure limit, disconnecting" ic.client.id;
    ic.is_healthy <- false;
    fire_event t (ClientError (ic.client.id, "backpressure_exceeded"));
    Lwt.return_false
  end else begin
    ic.client.pending_sends <- ic.client.pending_sends + 1;
    let* result =
      Lwt.catch
        (fun () ->
          let* () = Ws_lwt.Connected_client.send ic.conn frame in
          Lwt.return_true)
        (fun exn ->
          Log.error ~ctx:"voice_stream" "Send failed for client %s: %s"
            ic.client.id (Printexc.to_string exn);
          ic.is_healthy <- false;
          fire_event t (ClientError (ic.client.id, Printexc.to_string exn));
          Lwt.return_false)
    in
    ic.client.pending_sends <- ic.client.pending_sends - 1;
    Lwt.return result
  end

(** {1 Broadcasting} *)

let broadcast_frame t frame =
  let open Lwt.Syntax in
  let clients_list = Hashtbl.fold (fun _id ic acc -> ic :: acc) t.clients [] in
  let* _results = Lwt_list.map_p (fun ic ->
    if ic.is_healthy then
      safe_send t ic frame
    else
      Lwt.return_false
  ) clients_list in
  (* Cleanup unhealthy clients after broadcast *)
  let unhealthy = Hashtbl.fold (fun id ic acc ->
    if not ic.is_healthy then id :: acc else acc
  ) t.clients [] in
  List.iter (fun id -> remove_client t id) unhealthy;
  Lwt.return_unit

let broadcast_audio t audio =
  let frame = make_binary_frame (Bytes.to_string audio) in
  broadcast_frame t frame

let send_to_agent_subscribers t ~agent_id audio =
  let open Lwt.Syntax in
  let frame = make_binary_frame (Bytes.to_string audio) in
  let subscribers = Hashtbl.fold (fun _id ic acc ->
    if ic.is_healthy then
      match ic.client.agent_filter with
      | Some filter when filter = agent_id -> ic :: acc
      | None -> ic :: acc  (* No filter = receive all *)
      | Some _ -> acc  (* Different agent filter *)
    else acc
  ) t.clients [] in
  let* _results = Lwt_list.map_p (fun ic -> safe_send t ic frame) subscribers in
  (* Cleanup unhealthy clients *)
  let unhealthy = List.filter (fun ic -> not ic.is_healthy) subscribers in
  List.iter (fun ic -> remove_client t ic.client.id) unhealthy;
  Lwt.return_unit

let broadcast_text t message =
  let frame = make_text_frame message in
  broadcast_frame t frame

let notify_speaking t ~agent_id ~voice =
  let json = `Assoc [
    ("type", `String "speaking");
    ("agent_id", `String agent_id);
    ("voice", `String voice);
  ] in
  broadcast_text t (Yojson.Safe.to_string json)

let notify_done t ~agent_id ?duration_ms () =
  let json = `Assoc [
    ("type", `String "done");
    ("agent_id", `String agent_id);
    ("duration_ms", match duration_ms with
      | Some ms -> `Int ms
      | None -> `Null);
  ] in
  broadcast_text t (Yojson.Safe.to_string json)

(** {1 Event Handling} *)

let on_event t callback =
  t.event_callback <- Some callback

(** {1 Server Lifecycle} *)

let is_running t = t.is_running

(** Handle incoming WebSocket frames from a client *)
let handle_client t ic =
  let open Lwt.Syntax in
  let rec loop () =
    if t.should_stop || not ic.is_healthy then Lwt.return_unit
    else
      let* frame_opt =
        Lwt.catch
          (fun () ->
            let* frame = Ws_lwt.Connected_client.recv ic.conn in
            Lwt.return (Some frame))
          (fun exn ->
            Log.debug ~ctx:"voice_stream" "Recv error for %s: %s"
              ic.client.id (Printexc.to_string exn);
            Lwt.return None)
      in
      match frame_opt with
      | None ->
        (* Connection closed or error *)
        remove_client t ic.client.id;
        Lwt.return_unit
      | Some frame ->
        ic.client.last_activity <- Unix.gettimeofday ();
        (match frame.Ws.Frame.opcode with
         | Ws.Frame.Opcode.Close ->
           remove_client t ic.client.id;
           Lwt.return_unit
         | Ws.Frame.Opcode.Text ->
           let msg = parse_client_message frame.Ws.Frame.content in
           fire_event t (MessageReceived (ic.client.id, msg));
           (match msg with
            | Subscribe agent_id ->
              subscribe_to_agent t ~client_id:ic.client.id ~agent_id
            | Unsubscribe ->
              unsubscribe t ~client_id:ic.client.id
            | Unknown _ -> ());
           loop ()
         | Ws.Frame.Opcode.Ping ->
           let pong = Ws.Frame.create
             ~opcode:Ws.Frame.Opcode.Pong
             ~content:frame.Ws.Frame.content
             () in
           let* _ = safe_send t ic pong in
           loop ()
         | _ -> loop ())
  in
  loop ()

(** WebSocket connection handler *)
let client_handler t conn =
  let open Lwt.Syntax in
  (* Create client *)
  let client_id = generate_client_id () in
  let now = Unix.gettimeofday () in
  let client = {
    id = client_id;
    agent_filter = None;
    connected_at = now;
    last_activity = now;
    pending_sends = 0;
  } in

  let internal_client = { client; conn; is_healthy = true } in
  Hashtbl.add t.clients client_id internal_client;
  fire_event t (ClientConnected client);
  Log.info ~ctx:"voice_stream" "Client connected: %s (total: %d)" client_id (client_count t);

  (* Handle frames until disconnect *)
  let* () = handle_client t internal_client in
  Log.info ~ctx:"voice_stream" "Client disconnected: %s (total: %d)" client_id (client_count t);
  Lwt.return_unit

let start t =
  let open Lwt.Syntax in
  if t.is_running then Lwt.return_unit
  else begin
    t.is_running <- true;
    t.should_stop <- false;

    let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
    let mode = `TCP (`Port t.port) in

    let handler conn =
      Lwt.catch
        (fun () -> client_handler t conn)
        (fun exn ->
          Log.error ~ctx:"voice_stream" "Handler error: %s" (Printexc.to_string exn);
          Lwt.return_unit)
    in

    Lwt.async (fun () ->
      let* _server = Ws_lwt.establish_server ~ctx ~mode handler in
      Log.info ~ctx:"voice_stream" "WebSocket server started on port %d" t.port;
      Lwt.return_unit
    );

    Lwt.return_unit
  end

let stop t =
  let open Lwt.Syntax in
  t.should_stop <- true;
  t.is_running <- false;

  (* Disconnect all clients *)
  let clients_list = Hashtbl.fold (fun _id ic acc -> ic :: acc) t.clients [] in
  let* () = Lwt_list.iter_p (fun ic ->
    Lwt.catch
      (fun () -> Ws_lwt.Connected_client.send ic.conn (make_close_frame ()))
      (fun _exn -> Lwt.return_unit)
  ) clients_list in

  Hashtbl.clear t.clients;
  Log.info ~ctx:"voice_stream" "WebSocket server stopped";
  Lwt.return_unit

(** {1 Status} *)

let status_json t =
  let clients_list = Hashtbl.fold (fun _id ic acc ->
    `Assoc [
      ("id", `String ic.client.id);
      ("agent_filter", match ic.client.agent_filter with
        | Some a -> `String a
        | None -> `Null);
      ("connected_at", `Float ic.client.connected_at);
      ("last_activity", `Float ic.client.last_activity);
      ("pending_sends", `Int ic.client.pending_sends);
      ("is_healthy", `Bool ic.is_healthy);
    ] :: acc
  ) t.clients [] in
  `Assoc [
    ("port", `Int t.port);
    ("is_running", `Bool t.is_running);
    ("client_count", `Int (client_count t));
    ("max_pending_sends", `Int max_pending_sends);
    ("clients", `List clients_list);
  ]
