(** MASC Voice Conference - Multi-Agent Voice Orchestration

    Implementation of multi-agent voice conference mode.
    Combines Turn Queue + Session Manager for coordinated voice output.

    @author Second Brain
    @since MASC v3.0
*)

(** {1 Types} *)

type conference_state =
  | Idle
  | Active
  | Paused
  | Ended

type transcript_entry = {
  seq: int;
  agent_id: string;
  message: string;
  voice: string;
  timestamp: float;
  duration_ms: int option;
}

type conference_config = {
  name: string;
  agent_ids: string list;
  auto_persist_transcript: bool;
  max_turn_duration_ms: int option;
}

type t = {
  config: conference_config;
  config_path: string;
  turn_queue: Turn_queue.t;
  session_manager: Voice_session_manager.t;
  mutable state: conference_state;
  mutable transcript: transcript_entry list;
  mutable next_seq: int;
  mutable started_at: float option;
  mutable ended_at: float option;
  mutable pause_reason: string option;
  mutable stop_signal: bool ref;  (** Stop signal for turn processing *)
}

(** {1 Utilities} *)

let string_of_state = function
  | Idle -> "idle"
  | Active -> "active"
  | Paused -> "paused"
  | Ended -> "ended"

let transcript_entry_to_json entry =
  `Assoc [
    ("seq", `Int entry.seq);
    ("agent_id", `String entry.agent_id);
    ("message", `String entry.message);
    ("voice", `String entry.voice);
    ("timestamp", `Float entry.timestamp);
    ("duration_ms", match entry.duration_ms with
      | Some ms -> `Int ms
      | None -> `Null);
  ]

(** {1 Creation} *)

let create ~config ~config_path =
  {
    config;
    config_path;
    turn_queue = Turn_queue.create ();
    session_manager = Voice_session_manager.create ~config_path;
    state = Idle;
    transcript = [];
    next_seq = 1;
    started_at = None;
    ended_at = None;
    pause_reason = None;
    stop_signal = ref false;
  }

(** {1 Internal Helpers} *)

let add_transcript_entry t ~agent_id ~message ~voice ?duration_ms () =
  let entry = {
    seq = t.next_seq;
    agent_id;
    message;
    voice;
    timestamp = Unix.gettimeofday ();
    duration_ms;
  } in
  t.transcript <- t.transcript @ [entry];
  t.next_seq <- t.next_seq + 1;
  (* Update session turn count *)
  Voice_session_manager.increment_turn t.session_manager ~agent_id

(** TTS callback for processing turns *)
let make_tts_callback t =
  fun (req : Turn_queue.turn_request) ->
    let open Lwt.Syntax in
    (* Add to transcript before speaking *)
    add_transcript_entry t
      ~agent_id:req.agent_id
      ~message:req.message
      ~voice:req.voice
      ();
    (* Call voice bridge for actual TTS *)
    let* result = Voice_bridge.agent_speak ~agent_id:req.agent_id ~message:req.message () in
    (match result with
     | Ok _ -> ()
     | Error e -> Log.error ~ctx:"voice_conference" "TTS error: %s" e);
    Lwt.return_unit

(** {1 Conference Lifecycle} *)

let start t =
  match t.state with
  | Active ->
    Lwt.return (Error "Conference already active")
  | Ended ->
    Lwt.return (Error "Conference has ended, create a new one")
  | Idle | Paused ->
    (* Start sessions for all participants *)
    List.iter (fun agent_id ->
      let _ = Voice_session_manager.start_session t.session_manager ~agent_id () in
      ()
    ) t.config.agent_ids;

    t.state <- Active;
    t.started_at <- Some (Unix.gettimeofday ());
    t.pause_reason <- None;

    (* Start the turn processing loop in background *)
    t.stop_signal <- ref false;
    Lwt.async (fun () ->
      Turn_queue.process_loop t.turn_queue ~handler:(make_tts_callback t) ~stop_signal:t.stop_signal
    );

    Lwt.return (Ok ())

let rec end_conference t =
  (* Stop turn processing *)
  t.stop_signal := true;

  (* End all sessions *)
  List.iter (fun agent_id ->
    let _ = Voice_session_manager.end_session t.session_manager ~agent_id in
    ()
  ) t.config.agent_ids;

  t.state <- Ended;
  t.ended_at <- Some (Unix.gettimeofday ());

  (* Auto-persist transcript if configured *)
  if t.config.auto_persist_transcript then begin
    let transcript_path = Filename.concat t.config_path
      (Printf.sprintf "transcript_%s_%d.json" t.config.name
        (int_of_float (Unix.gettimeofday ()))) in
    let json = export_transcript_json t in
    let oc = open_out transcript_path in
    output_string oc (Yojson.Safe.pretty_to_string json);
    close_out oc
  end;

  Lwt.return_unit

and export_transcript_json t =
  `Assoc [
    ("conference_name", `String t.config.name);
    ("started_at", match t.started_at with
      | Some ts -> `Float ts
      | None -> `Null);
    ("ended_at", match t.ended_at with
      | Some ts -> `Float ts
      | None -> `Null);
    ("participants", `List (List.map (fun a -> `String a) t.config.agent_ids));
    ("entries", `List (List.map transcript_entry_to_json t.transcript));
    ("total_turns", `Int (List.length t.transcript));
  ]

let pause t ~reason =
  match t.state with
  | Active ->
    t.stop_signal := true;
    t.state <- Paused;
    t.pause_reason <- Some reason
  | _ -> ()

let resume t =
  match t.state with
  | Paused ->
    t.state <- Active;
    t.pause_reason <- None;
    (* Restart processing with new stop signal *)
    t.stop_signal <- ref false;
    Lwt.async (fun () ->
      Turn_queue.process_loop t.turn_queue ~handler:(make_tts_callback t) ~stop_signal:t.stop_signal
    )
  | _ -> ()

(** {1 Turn Management} *)

let speak t ~agent_id ~message ?priority () =
  (* Get voice from session *)
  let voice = match Voice_session_manager.get_session t.session_manager ~agent_id with
    | Some session -> session.voice
    | None -> Voice_bridge.get_voice_for_agent agent_id
  in
  Turn_queue.enqueue t.turn_queue ~agent_id ~message ~voice ?priority ()

let current_speaker t =
  match Turn_queue.current_speaker t.turn_queue with
  | Some req -> Some req.agent_id
  | None -> None

let pending_turns t =
  Turn_queue.length t.turn_queue

(** {1 Transcript} *)

let get_transcript t = t.transcript

let export_transcript_md t =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "# Conference: %s\n\n" t.config.name);

  (match t.started_at with
   | Some ts ->
     let tm = Unix.localtime ts in
     Buffer.add_string buf (Printf.sprintf "**Started**: %04d-%02d-%02d %02d:%02d:%02d\n\n"
       (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
       tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
   | None -> ());

  Buffer.add_string buf (Printf.sprintf "**Participants**: %s\n\n"
    (String.concat ", " t.config.agent_ids));

  Buffer.add_string buf "---\n\n";

  List.iter (fun entry ->
    let tm = Unix.localtime entry.timestamp in
    Buffer.add_string buf (Printf.sprintf "**[%02d:%02d:%02d] %s** (%s):\n> %s\n\n"
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      entry.agent_id entry.voice entry.message)
  ) t.transcript;

  Buffer.add_string buf "---\n\n";
  Buffer.add_string buf (Printf.sprintf "*Total turns: %d*\n" (List.length t.transcript));

  Buffer.contents buf

let clear_transcript t =
  t.transcript <- [];
  t.next_seq <- 1

(** {1 Participants} *)

let participants t = t.config.agent_ids

let add_participant t ~agent_id =
  if List.mem agent_id t.config.agent_ids then
    false
  else begin
    (* Mutate the config's agent_ids - need to use a ref or mutable *)
    (* For now, we don't support adding participants to running conference *)
    (* Just start session for new participant *)
    let _ = Voice_session_manager.start_session t.session_manager ~agent_id () in
    true
  end

let remove_participant t ~agent_id =
  if not (List.mem agent_id t.config.agent_ids) then
    false
  else begin
    let _ = Voice_session_manager.end_session t.session_manager ~agent_id in
    true
  end

(** {1 Status} *)

let state t = t.state

let status_json t =
  `Assoc [
    ("name", `String t.config.name);
    ("state", `String (string_of_state t.state));
    ("participants", `List (List.map (fun a -> `String a) t.config.agent_ids));
    ("current_speaker", match current_speaker t with
      | Some a -> `String a
      | None -> `Null);
    ("pending_turns", `Int (pending_turns t));
    ("transcript_length", `Int (List.length t.transcript));
    ("started_at", match t.started_at with
      | Some ts -> `Float ts
      | None -> `Null);
    ("pause_reason", match t.pause_reason with
      | Some r -> `String r
      | None -> `Null);
  ]
