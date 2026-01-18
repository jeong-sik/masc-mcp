(** MASC Voice Session Manager - Multi-Agent Session Tracking

    Implementation of multi-agent voice session management.
    Each agent can have one active voice session at a time.

    @author Second Brain
    @since MASC v3.0
*)

(** {1 Types} *)

type session_status =
  | Active
  | Idle
  | Suspended

type session = {
  session_id: string;
  agent_id: string;
  voice: string;
  started_at: float;
  mutable last_activity: float;
  mutable turn_count: int;
  mutable status: session_status;
}

type t = {
  sessions: (string, session) Hashtbl.t;  (* agent_id -> session *)
  config_path: string;
  session_dir: string;
}

(** {1 Utilities} *)

let generate_session_id () =
  (* Use smaller random values for cross-platform safety *)
  let high = Random.int 0xFFFF in
  let mid = Random.int 0xFFFF in
  let low = Random.int 0xFFFF in
  Printf.sprintf "vs-%08x-%04x%04x%04x"
    (Random.int 0x3FFFFFFF)
    high mid low

let string_of_status = function
  | Active -> "active"
  | Idle -> "idle"
  | Suspended -> "suspended"

let status_of_string = function
  | "active" -> Active
  | "idle" -> Idle
  | "suspended" -> Suspended
  | _ -> Idle

let session_to_json session =
  `Assoc [
    ("session_id", `String session.session_id);
    ("agent_id", `String session.agent_id);
    ("voice", `String session.voice);
    ("started_at", `Float session.started_at);
    ("last_activity", `Float session.last_activity);
    ("turn_count", `Int session.turn_count);
    ("status", `String (string_of_status session.status));
  ]

let session_of_json json =
  let open Yojson.Safe.Util in
  {
    session_id = json |> member "session_id" |> to_string;
    agent_id = json |> member "agent_id" |> to_string;
    voice = json |> member "voice" |> to_string;
    started_at = json |> member "started_at" |> to_float;
    last_activity = json |> member "last_activity" |> to_float;
    turn_count = json |> member "turn_count" |> to_int;
    status = json |> member "status" |> to_string |> status_of_string;
  }

(** {1 Creation} *)

let create ~config_path =
  Random.self_init ();
  let session_dir = Filename.concat config_path "voice_sessions" in
  {
    sessions = Hashtbl.create 16;
    config_path;
    session_dir;
  }

(** {1 Internal Helpers} *)

let ensure_session_dir t =
  if not (Sys.file_exists t.session_dir) then
    Unix.mkdir t.session_dir 0o755

let session_file t agent_id =
  Filename.concat t.session_dir (agent_id ^ ".json")

let save_session t session =
  ensure_session_dir t;
  let json = session_to_json session in
  let content = Yojson.Safe.pretty_to_string json in
  let filepath = session_file t session.agent_id in
  let oc = open_out filepath in
  output_string oc content;
  close_out oc

let load_session t agent_id =
  let filepath = session_file t agent_id in
  if Sys.file_exists filepath then begin
    let ic = open_in filepath in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    try
      let json = Yojson.Safe.from_string content in
      Some (session_of_json json)
    with _ -> None
  end else
    None

let delete_session_file t agent_id =
  let filepath = session_file t agent_id in
  if Sys.file_exists filepath then
    Sys.remove filepath

(** {1 Session Lifecycle} *)

let start_session t ~agent_id ?voice () =
  (* Check if session already exists *)
  match Hashtbl.find_opt t.sessions agent_id with
  | Some existing ->
    existing.status <- Active;
    existing.last_activity <- Unix.gettimeofday ();
    save_session t existing;
    existing
  | None ->
    (* Get default voice from Voice_bridge *)
    let voice = match voice with
      | Some v -> v
      | None -> Voice_bridge.get_voice_for_agent agent_id
    in
    let now = Unix.gettimeofday () in
    let session = {
      session_id = generate_session_id ();
      agent_id;
      voice;
      started_at = now;
      last_activity = now;
      turn_count = 0;
      status = Active;
    } in
    Hashtbl.add t.sessions agent_id session;
    save_session t session;
    session

let end_session t ~agent_id =
  match Hashtbl.find_opt t.sessions agent_id with
  | Some _ ->
    Hashtbl.remove t.sessions agent_id;
    delete_session_file t agent_id;
    true
  | None -> false

let suspend_session t ~agent_id =
  match Hashtbl.find_opt t.sessions agent_id with
  | Some session ->
    session.status <- Suspended;
    save_session t session
  | None -> ()

let resume_session t ~agent_id =
  match Hashtbl.find_opt t.sessions agent_id with
  | Some session ->
    session.status <- Active;
    session.last_activity <- Unix.gettimeofday ();
    save_session t session
  | None -> ()

(** {1 Session Query} *)

let get_session t ~agent_id =
  Hashtbl.find_opt t.sessions agent_id

let list_sessions t =
  Hashtbl.fold (fun _ session acc -> session :: acc) t.sessions []

let has_session t ~agent_id =
  Hashtbl.mem t.sessions agent_id

let session_count t =
  Hashtbl.length t.sessions

(** {1 Activity Tracking} *)

let heartbeat t ~agent_id =
  match Hashtbl.find_opt t.sessions agent_id with
  | Some session ->
    session.last_activity <- Unix.gettimeofday ();
    save_session t session
  | None -> ()

let increment_turn t ~agent_id =
  match Hashtbl.find_opt t.sessions agent_id with
  | Some session ->
    session.turn_count <- session.turn_count + 1;
    session.last_activity <- Unix.gettimeofday ();
    save_session t session
  | None -> ()

(** {1 Zombie Cleanup} *)

let cleanup_zombies t ?(timeout = 300.0) () =
  let now = Unix.gettimeofday () in
  let to_remove = Hashtbl.fold (fun agent_id session acc ->
    if now -. session.last_activity > timeout then
      agent_id :: acc
    else
      acc
  ) t.sessions [] in
  List.iter (fun agent_id ->
    Hashtbl.remove t.sessions agent_id;
    delete_session_file t agent_id
  ) to_remove;
  List.length to_remove

(** {1 Persistence} *)

let persist t =
  ensure_session_dir t;
  Hashtbl.iter (fun _ session -> save_session t session) t.sessions

let restore t =
  if Sys.file_exists t.session_dir && Sys.is_directory t.session_dir then begin
    let files = Sys.readdir t.session_dir in
    Array.iter (fun filename ->
      if Filename.check_suffix filename ".json" then begin
        let agent_id = Filename.chop_suffix filename ".json" in
        match load_session t agent_id with
        | Some session ->
          Hashtbl.add t.sessions agent_id session
        | None -> ()
      end
    ) files
  end

(** {1 Status} *)

let status_json t =
  let sessions_json = list_sessions t
    |> List.map session_to_json
    |> (fun l -> `List l)
  in
  `Assoc [
    ("session_count", `Int (session_count t));
    ("config_path", `String t.config_path);
    ("sessions", sessions_json);
  ]
