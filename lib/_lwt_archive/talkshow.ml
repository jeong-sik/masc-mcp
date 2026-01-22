(** MASC Talk Show - Turn-by-turn AI conversation with voice

    Integrates with MASC room broadcast and uses Cohttp for TTS.
    Thread-safe with proper resource management.
*)

open Lwt.Syntax

(* ============================================ *)
(* Types                                        *)
(* ============================================ *)

type agent_config = {
  name: string;
  voice: string;
  emoji: string;
  role: string;
  cmd: string list;  (* Command to invoke agent *)
}

type message = {
  speaker: string;
  text: string;
  timestamp: float;
}

type show_state = {
  mutable topic: string;
  mutable status: [`Idle | `Live | `Ended];
  mutable messages: message list;  (* Stored in reverse order for O(1) prepend *)
  mutable current_turn: int;
  mutable started_at: float option;
}

(* Thread-safety mutex for global state *)
let state_mutex = Lwt_mutex.create ()

(* ============================================ *)
(* Agent configurations                         *)
(* ============================================ *)

let agents = [
  ("claude", {
    name = "claude";
    voice = "Sarah";
    emoji = "🔬";
    role = "기술 분석가";
    cmd = ["claude"; "-p"];
  });
  ("gemini", {
    name = "gemini";
    voice = "Roger";
    emoji = "🎯";
    role = "전략가";
    cmd = ["gemini"];
  });
  ("codex", {
    name = "codex";
    voice = "Charlie";
    emoji = "⚡";
    role = "코드 전문가";
    cmd = ["codex"; "-q"];
  });
]

let host_voice = "전대한"

(* Global state *)
let state = {
  topic = "";
  status = `Idle;
  messages = [];
  current_turn = 0;
  started_at = None;
}

(* MASC config reference (set when talkshow starts) *)
let masc_config : Room_utils.config option ref = ref None

(* Broadcast to MASC room if config is set *)
let masc_broadcast ~from_agent ~content =
  match !masc_config with
  | Some config ->
      let _ = Room.broadcast config ~from_agent ~content in
      ()
  | None -> ()

(* ============================================ *)
(* Helpers                                      *)
(* ============================================ *)

let timestamp_to_string ts =
  let tm = Unix.localtime ts in
  Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* UTF-8 safe string truncation - finds character boundary *)
let utf8_sub s max_bytes =
  if String.length s <= max_bytes then s
  else
    let rec find_boundary pos =
      if pos <= 0 then 0
      else
        let byte = Char.code s.[pos] in
        (* UTF-8 continuation byte starts with 10xxxxxx (0x80-0xBF) *)
        if byte land 0xC0 <> 0x80 then pos
        else find_boundary (pos - 1)
    in
    String.sub s 0 (find_boundary max_bytes)

(* Thread-safe message addition - O(1) prepend *)
let add_message speaker text =
  Lwt_mutex.with_lock state_mutex (fun () ->
    let msg = {
      speaker;
      text;
      timestamp = Unix.gettimeofday ();
    } in
    state.messages <- msg :: state.messages;  (* O(1) prepend *)
    Lwt.return msg
  )

(* Shell-safe command execution with proper resource cleanup *)
let run_command cmd args =
  (* Use Filename.quote for shell injection prevention *)
  let full_cmd = String.concat " " (List.map Filename.quote (cmd @ [args])) in
  let* () = Lwt_io.printlf "[talkshow] Running: %s" full_cmd in
  try
    let ic = Unix.open_process_in full_cmd in
    Fun.protect ~finally:(fun () ->
      (* Always close process, check exit status *)
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code ->
          Printf.eprintf "[talkshow] Command exited with code %d\n%!" code
      | Unix.WSIGNALED signal ->
          Printf.eprintf "[talkshow] Command killed by signal %d\n%!" signal
      | Unix.WSTOPPED _ ->
          Printf.eprintf "[talkshow] Command stopped\n%!"
    ) (fun () ->
      let buf = Buffer.create 1024 in
      (try
        while true do
          Buffer.add_string buf (input_line ic);
          Buffer.add_char buf '\n'
        done
      with End_of_file -> ());
      let output = Buffer.contents buf |> String.trim in
      Lwt.return (if String.length output > 0 then output else "(응답 없음)")
    )
  with exn ->
    let* () = Lwt_io.printlf "[talkshow] Command error: %s" (Printexc.to_string exn) in
    Lwt.return "(에러 발생)"

(* TTS via ElevenLabs proxy using Cohttp (proper UTF-8 encoding) *)
let speak ~voice text =
  let tts_url = "https://elevenlabs-proxy-production-443b.up.railway.app/v1/audio/speech" in
  (* UTF-8 safe truncation for Korean text *)
  let text = utf8_sub text 500 in

  (* Build JSON body properly with Yojson for correct encoding *)
  let body_json = `Assoc [
    ("model", `String "eleven_multilingual_v2");
    ("input", `String text);
    ("voice", `String voice);
  ] in
  let body_str = Yojson.Safe.to_string body_json in

  try%lwt
    let uri = Uri.of_string tts_url in
    let headers = Cohttp.Header.of_list [
      ("Content-Type", "application/json");
    ] in
    let body = Cohttp_lwt.Body.of_string body_str in
    let* (resp, resp_body) = Cohttp_lwt_unix.Client.post ~headers ~body uri in
    let status = Cohttp.Response.status resp in
    if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then begin
      let* body_bytes = Cohttp_lwt.Body.to_string resp_body in
      (* Check if it's audio (MP3 starts with ID3 or 0xFF) *)
      if String.length body_bytes > 3 &&
         (String.sub body_bytes 0 3 = "ID3" ||
          Char.code (String.get body_bytes 0) = 0xff) then begin
        (* Use unique temp file with cleanup *)
        let audio_file = Filename.temp_file "talkshow_" ".mp3" in
        Fun.protect ~finally:(fun () ->
          try Sys.remove audio_file with _ -> ()
        ) (fun () ->
          let oc = open_out_bin audio_file in
          Fun.protect ~finally:(fun () -> close_out_noerr oc)
            (fun () -> output_string oc body_bytes);
          (* Use Lwt_unix.system for proper async handling *)
          let cmd = Printf.sprintf "afplay %s" (Filename.quote audio_file) in
          let* exit_status = Lwt_unix.system cmd in
          (match exit_status with
          | Unix.WEXITED 0 -> ()
          | _ -> Printf.eprintf "[TTS] afplay failed\n%!");
          Lwt.return_unit
        )
      end else begin
        let preview = utf8_sub body_bytes 100 in
        let* () = Lwt_io.printlf "[TTS] Not audio response: %s..." preview in
        Lwt.return_unit
      end
    end else begin
      let* () = Lwt_io.printlf "[TTS] HTTP %s" (Cohttp.Code.string_of_status status) in
      Lwt.return_unit
    end
  with exn ->
    let* () = Lwt_io.printlf "[TTS Error] %s" (Printexc.to_string exn) in
    Lwt.return_unit

(* ============================================ *)
(* Core functions                               *)
(* ============================================ *)

let start_show ?config ~topic ~voice_enabled:_voice_enabled () =
  (* Store MASC config for broadcasting *)
  masc_config := config;

  (* Thread-safe state initialization *)
  let* () = Lwt_mutex.with_lock state_mutex (fun () ->
    state.topic <- topic;
    state.status <- `Live;
    state.messages <- [];
    state.current_turn <- 0;
    state.started_at <- Some (Unix.gettimeofday ());
    Lwt.return_unit
  ) in

  let* () = Lwt_io.printlf "\n============================================================" in
  let* () = Lwt_io.printlf "🎙️  MASC Voice Talk Show" in
  let* () = Lwt_io.printlf "============================================================" in
  let* () = Lwt_io.printlf "📡 Topic: %s" topic in

  (* Broadcast show start to MASC *)
  masc_broadcast ~from_agent:"talkshow" ~content:(Printf.sprintf "🎭 Talk Show 시작! 주제: %s" topic);

  Lwt.return (Printf.sprintf {|{"ok": true, "status": "live", "topic": "%s"}|} topic)

let host_say ~voice_enabled message =
  let* () = Lwt_io.printlf "\n🎤 [방장]: %s" message in
  let* _msg = add_message "host" message in
  (* Broadcast to MASC *)
  masc_broadcast ~from_agent:"host" ~content:(Printf.sprintf "🎤 %s" message);
  let* () = if voice_enabled then speak ~voice:host_voice message else Lwt.return_unit in
  Lwt.return_unit

let agent_turn ~agent_name ~question ~voice_enabled =
  match List.assoc_opt agent_name agents with
  | None -> Lwt.return (Printf.sprintf {|{"error": "Unknown agent: %s"}|} agent_name)
  | Some config ->
      let* () = Lwt_io.printlf "\n%s [%s] (%s) 생각 중..."
        config.emoji (String.uppercase_ascii config.name) config.role in

      (* Call the agent *)
      let* response = run_command config.cmd question in

      (* UTF-8 safe truncation for display *)
      let display = (utf8_sub response 300) ^ (if String.length response > 300 then "..." else "") in
      let* () = Lwt_io.printlf "%s [%s]: %s" config.emoji (String.uppercase_ascii config.name) display in

      (* Record message (thread-safe) *)
      let* msg = add_message agent_name response in
      let* () = Lwt_mutex.with_lock state_mutex (fun () ->
        state.current_turn <- state.current_turn + 1;
        Lwt.return_unit
      ) in

      (* UTF-8 safe broadcast text *)
      let broadcast_text = (utf8_sub response 200) ^ (if String.length response > 200 then "..." else "") in
      masc_broadcast ~from_agent:agent_name ~content:(Printf.sprintf "%s %s" config.emoji broadcast_text);

      (* TTS *)
      let* () = if voice_enabled then speak ~voice:config.voice response else Lwt.return_unit in

      Lwt.return (Printf.sprintf
        {|{"speaker": "%s", "text": "%s", "time": "%s"}|}
        agent_name (String.escaped response) (timestamp_to_string msg.timestamp))

let round_robin ~topic ~voice_enabled =
  let agent_names = ["claude"; "gemini"; "codex"] in
  let* () = host_say ~voice_enabled (Printf.sprintf "자, '%s' 주제로 각자 의견 주세요." topic) in

  (* Use Lwt_list.fold_left_s for cleaner iteration *)
  Lwt_list.fold_left_s (fun context agent ->
    (* UTF-8 safe context truncation - take last 300 bytes safely *)
    let ctx_len = String.length context in
    let context_tail = if ctx_len > 300 then
      utf8_sub (String.sub context (ctx_len - 300) 300) 300
    else context in
    let question = Printf.sprintf "%s\n\n맥락: %s" topic context_tail in
    let* result = agent_turn ~agent_name:agent ~question ~voice_enabled in
    let* () = Lwt_unix.sleep 1.0 in
    Lwt.return (context ^ "\n" ^ agent ^ ": " ^ result)
  ) "" agent_names

let end_show ~voice_enabled =
  let* () = host_say ~voice_enabled "오늘 토크쇼는 여기까지입니다. 감사합니다!" in

  (* Thread-safe state update *)
  let* () = Lwt_mutex.with_lock state_mutex (fun () ->
    state.status <- `Ended;
    Lwt.return_unit
  ) in

  (* Broadcast show end to MASC *)
  masc_broadcast ~from_agent:"talkshow" ~content:(Printf.sprintf "🎭 Talk Show 종료! %d개 메시지" (List.length state.messages));

  (* Clear MASC config reference *)
  masc_config := None;

  let* () = Lwt_io.printlf "\n============================================================" in
  let* () = Lwt_io.printlf "📝 대화 기록 (%d messages):" (List.length state.messages) in
  (* Messages stored in reverse order - take first 10, then reverse for display *)
  let recent = List.filteri (fun i _ -> i < 10) state.messages in
  List.iter (fun msg ->
    Printf.printf "  [%s]: %s...\n" msg.speaker (utf8_sub msg.text 60)
  ) (List.rev recent);
  let* () = Lwt_io.printlf "============================================================\n" in

  Lwt.return {|{"ok": true, "status": "ended"}|}

let get_history () =
  (* Messages stored in reverse order - reverse for chronological display *)
  let msgs = List.map (fun msg ->
    Printf.sprintf {|{"speaker":"%s","text":"%s","time":"%s"}|}
      msg.speaker (String.escaped msg.text) (timestamp_to_string msg.timestamp)
  ) (List.rev state.messages) in
  Printf.sprintf {|{"topic":"%s","status":"%s","messages":[%s]}|}
    state.topic
    (match state.status with `Idle -> "idle" | `Live -> "live" | `Ended -> "ended")
    (String.concat "," msgs)

let get_status () =
  Printf.sprintf {|{"topic":"%s","status":"%s","turn":%d,"message_count":%d}|}
    state.topic
    (match state.status with `Idle -> "idle" | `Live -> "live" | `Ended -> "ended")
    state.current_turn
    (List.length state.messages)

(* ============================================ *)
(* Quick demo                                   *)
(* ============================================ *)

let run_demo ?config ~topic ~voice_enabled () =
  let* _ = start_show ?config ~topic ~voice_enabled () in

  let* () = host_say ~voice_enabled (Printf.sprintf "오늘의 주제는 '%s'입니다." topic) in
  let* () = host_say ~voice_enabled "먼저 Claude부터 시작하죠." in

  let question1 = Printf.sprintf "%s - 기술적 관점에서 설명해줘. 3문장으로." topic in
  let* r1 = agent_turn ~agent_name:"claude" ~question:question1 ~voice_enabled in

  let* () = host_say ~voice_enabled "Gemini, 어떻게 생각하세요?" in

  let question2 = Printf.sprintf "%s - Claude의 의견에 다른 관점으로 의견 줘. 3문장으로." topic in
  let* _r2 = agent_turn ~agent_name:"gemini" ~question:question2 ~voice_enabled in

  let* () = host_say ~voice_enabled "마지막으로 Codex, 정리해주세요." in

  let question3 = Printf.sprintf "%s - 두 의견을 종합해서 결론 내려줘. 3문장으로." topic in
  let* _r3 = agent_turn ~agent_name:"codex" ~question:question3 ~voice_enabled in

  let* _ = end_show ~voice_enabled in

  Lwt.return r1
