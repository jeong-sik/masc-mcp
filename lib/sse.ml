(** SSE (Server-Sent Events) module for MCP Streamable HTTP Transport
    MCP Spec 2025-03-26 compliant *)

(** SSE client state *)
type client = {
  id: int;
  push: string -> unit;
  mutable last_event_id: int;
}

(** Client registry - maps session_id to client *)
let clients : (string, client) Hashtbl.t = Hashtbl.create 16

(** Monotonic client id for safe replacement/unregister *)
let client_id_counter = ref 0

(** Global event counter for resumability *)
let event_counter = ref 0

(** Event buffer for resumability - stores (event_id, event_string) pairs *)
let max_buffer_size = 100
let event_buffer : (int * string) Queue.t = Queue.create ()

(** Add event to buffer, maintaining max size *)
let buffer_event event_id event_str =
  if Queue.length event_buffer >= max_buffer_size then
    ignore (Queue.pop event_buffer);
  Queue.push (event_id, event_str) event_buffer

(** Get events after given ID for replay (MCP spec MUST) *)
let get_events_after last_id =
  Queue.fold (fun acc (id, ev) ->
    if id > last_id then ev :: acc else acc
  ) [] event_buffer
  |> List.rev

(** Format SSE event with optional ID and event type *)
let format_event ?id ?event_type data =
  incr event_counter;
  let id_line = Printf.sprintf "id: %d\n"
    (match id with Some i -> i | None -> !event_counter) in
  let event_line = match event_type with
    | Some e -> Printf.sprintf "event: %s\n" e
    | None -> ""
  in
  Printf.sprintf "%s%sdata: %s\n\n" id_line event_line data

(** Get current event ID *)
let current_id () = !event_counter

(** Allocate next event ID without emitting data. *)
let next_id () =
  incr event_counter;
  !event_counter

(** Register a new SSE client *)
let register session_id ~push ~last_event_id =
  incr client_id_counter;
  let client = { id = !client_id_counter; push; last_event_id } in
  Hashtbl.replace clients session_id client;
  client.id

(** Unregister an SSE client *)
let unregister session_id =
  Hashtbl.remove clients session_id

(** Unregister only if the current client matches the given client_id.
    Prevents an old connection's cleanup from unregistering a newer connection
    that re-used the same session_id. *)
let unregister_if_current session_id client_id =
  match Hashtbl.find_opt clients session_id with
  | Some client when client.id = client_id -> Hashtbl.remove clients session_id
  | _ -> ()

(** Check if client exists *)
let exists session_id =
  Hashtbl.mem clients session_id

(** Update client's last event ID *)
let update_last_event_id session_id event_id =
  match Hashtbl.find_opt clients session_id with
  | Some client -> client.last_event_id <- event_id
  | None -> ()

(** Broadcast event to all connected clients *)
let broadcast json =
  let data = Yojson.Safe.to_string json in
  let current_event_id = !event_counter + 1 in
  let event = format_event ~id:current_event_id ~event_type:"message" data in
  buffer_event current_event_id event;
  Hashtbl.iter (fun session_id client ->
    if current_event_id > client.last_event_id then begin
      try
        client.push event;
        update_last_event_id session_id current_event_id
      with _ -> ()
    end
  ) clients

(** Send a JSON-RPC message to a specific session (legacy SSE transport) *)
let send_to session_id json =
  let data = Yojson.Safe.to_string json in
  let current_event_id = !event_counter + 1 in
  let event = format_event ~id:current_event_id ~event_type:"message" data in
  buffer_event current_event_id event;
  match Hashtbl.find_opt clients session_id with
  | None -> ()
  | Some client ->
      (try
         client.push event;
         update_last_event_id session_id current_event_id
       with _ -> ())

(** Get client count *)
let client_count () =
  Hashtbl.length clients
