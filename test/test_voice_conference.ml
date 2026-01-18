(** Tests for Voice Conference Module

    Tests for multi-agent voice conference orchestration.
*)

open Alcotest

(** Helper for String.is_substring (needed before tests use it) *)
module String = struct
  include String
  let is_substring ~affix s =
    let len_affix = String.length affix in
    let len_s = String.length s in
    if len_affix > len_s then false
    else begin
      let rec check i =
        if i > len_s - len_affix then false
        else if String.sub s i len_affix = affix then true
        else check (i + 1)
      in
      check 0
    end
end

(** {1 Test Setup} *)

let tmp_dir () =
  let dir = Filename.temp_file "masc_voice_conf_test" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  dir

let cleanup_dir dir =
  if Sys.file_exists dir then begin
    let files = Sys.readdir dir in
    Array.iter (fun f ->
      let path = Filename.concat dir f in
      if Sys.is_directory path then begin
        (* Clean subdirs *)
        let subfiles = Sys.readdir path in
        Array.iter (fun sf -> Sys.remove (Filename.concat path sf)) subfiles;
        Unix.rmdir path
      end else
        Sys.remove path
    ) files;
    Unix.rmdir dir
  end

let make_config ?(name = "test_conf") ?(auto_persist = false) agents =
  Masc_mcp.Voice_conference.{
    name;
    agent_ids = agents;
    auto_persist_transcript = auto_persist;
    max_turn_duration_ms = None;
  }

(** {1 State Tests} *)

let test_string_of_state () =
  let open Masc_mcp.Voice_conference in
  check string "idle" "idle" (string_of_state Idle);
  check string "active" "active" (string_of_state Active);
  check string "paused" "paused" (string_of_state Paused);
  check string "ended" "ended" (string_of_state Ended)

(** {1 Creation Tests} *)

let test_create_conference () =
  let dir = tmp_dir () in
  let config = make_config ["claude"; "gemini"; "codex"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  check (list string) "participants" ["claude"; "gemini"; "codex"]
    (Masc_mcp.Voice_conference.participants conf);
  check int "pending_turns" 0 (Masc_mcp.Voice_conference.pending_turns conf);
  check (option string) "current_speaker" None
    (Masc_mcp.Voice_conference.current_speaker conf);
  check (list pass) "transcript empty" []
    (Masc_mcp.Voice_conference.get_transcript conf);
  cleanup_dir dir

let test_initial_state_idle () =
  let dir = tmp_dir () in
  let config = make_config ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  check bool "state is idle" true
    (Masc_mcp.Voice_conference.state conf = Masc_mcp.Voice_conference.Idle);
  cleanup_dir dir

(** {1 Participant Tests} *)

let test_participants_list () =
  let dir = tmp_dir () in
  let config = make_config ["agent1"; "agent2"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  check (list string) "two participants" ["agent1"; "agent2"]
    (Masc_mcp.Voice_conference.participants conf);
  cleanup_dir dir

let test_add_participant () =
  let dir = tmp_dir () in
  let config = make_config ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  let added = Masc_mcp.Voice_conference.add_participant conf ~agent_id:"gemini" in
  check bool "participant added" true added;
  cleanup_dir dir

let test_add_duplicate_participant () =
  let dir = tmp_dir () in
  let config = make_config ["claude"; "gemini"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  let added = Masc_mcp.Voice_conference.add_participant conf ~agent_id:"claude" in
  check bool "duplicate not added" false added;
  cleanup_dir dir

let test_remove_participant () =
  let dir = tmp_dir () in
  let config = make_config ["claude"; "gemini"; "codex"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  (* Remove one of the original participants *)
  let removed = Masc_mcp.Voice_conference.remove_participant conf ~agent_id:"codex" in
  check bool "participant removed" true removed;
  cleanup_dir dir

let test_remove_nonexistent_participant () =
  let dir = tmp_dir () in
  let config = make_config ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  let removed = Masc_mcp.Voice_conference.remove_participant conf ~agent_id:"unknown" in
  check bool "nonexistent not removed" false removed;
  cleanup_dir dir

(** {1 Turn Management Tests} *)

let test_speak_enqueues_turn () =
  let dir = tmp_dir () in
  let config = make_config ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  let _turn = Masc_mcp.Voice_conference.speak conf ~agent_id:"claude" ~message:"Hello" () in
  check int "one pending turn" 1 (Masc_mcp.Voice_conference.pending_turns conf);
  cleanup_dir dir

let test_speak_multiple_agents () =
  let dir = tmp_dir () in
  let config = make_config ["claude"; "gemini"; "codex"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  let _ = Masc_mcp.Voice_conference.speak conf ~agent_id:"claude" ~message:"First" () in
  let _ = Masc_mcp.Voice_conference.speak conf ~agent_id:"gemini" ~message:"Second" () in
  let _ = Masc_mcp.Voice_conference.speak conf ~agent_id:"codex" ~message:"Third" () in
  check int "three pending turns" 3 (Masc_mcp.Voice_conference.pending_turns conf);
  cleanup_dir dir

let test_speak_with_priority () =
  let dir = tmp_dir () in
  let config = make_config ["claude"; "gemini"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  (* Lower priority number = higher priority *)
  let _ = Masc_mcp.Voice_conference.speak conf ~agent_id:"gemini" ~message:"Low" ~priority:5 () in
  let _ = Masc_mcp.Voice_conference.speak conf ~agent_id:"claude" ~message:"High" ~priority:1 () in
  check int "two pending" 2 (Masc_mcp.Voice_conference.pending_turns conf);
  cleanup_dir dir

(** {1 Pause/Resume Tests} *)

let test_pause_conference () =
  let dir = tmp_dir () in
  let config = make_config ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  Masc_mcp.Voice_conference.pause conf ~reason:"testing";
  (* Pause from Idle goes to Idle (only Active can pause) *)
  check bool "still idle (can't pause idle)" true
    (Masc_mcp.Voice_conference.state conf = Masc_mcp.Voice_conference.Idle);
  cleanup_dir dir

let test_resume_conference () =
  let dir = tmp_dir () in
  let config = make_config ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  (* Resume from Idle does nothing *)
  Masc_mcp.Voice_conference.resume conf;
  check bool "still idle" true
    (Masc_mcp.Voice_conference.state conf = Masc_mcp.Voice_conference.Idle);
  cleanup_dir dir

(** {1 Transcript Tests} *)

let test_empty_transcript () =
  let dir = tmp_dir () in
  let config = make_config ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  check (list pass) "empty transcript" []
    (Masc_mcp.Voice_conference.get_transcript conf);
  cleanup_dir dir

let test_clear_transcript () =
  let dir = tmp_dir () in
  let config = make_config ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  Masc_mcp.Voice_conference.clear_transcript conf;
  check (list pass) "transcript cleared" []
    (Masc_mcp.Voice_conference.get_transcript conf);
  cleanup_dir dir

let test_export_transcript_json () =
  let dir = tmp_dir () in
  let config = make_config ~name:"test_export" ["claude"; "gemini"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  let json = Masc_mcp.Voice_conference.export_transcript_json conf in
  let open Yojson.Safe.Util in
  let name = json |> member "conference_name" |> to_string in
  check string "conference name" "test_export" name;
  let participants = json |> member "participants" |> to_list |> List.map to_string in
  check (list string) "participants in json" ["claude"; "gemini"] participants;
  cleanup_dir dir

let test_export_transcript_md () =
  let dir = tmp_dir () in
  let config = make_config ~name:"markdown_test" ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  let md = Masc_mcp.Voice_conference.export_transcript_md conf in
  check bool "contains header" true (String.length md > 0);
  check bool "contains conference name" true
    (String.is_substring ~affix:"markdown_test" md);
  cleanup_dir dir

(** {1 Transcript Entry Tests} *)

let test_transcript_entry_to_json () =
  let entry = Masc_mcp.Voice_conference.{
    seq = 1;
    agent_id = "claude";
    message = "Hello world";
    voice = "Sarah";
    timestamp = 1704067200.0;  (* 2024-01-01 00:00:00 UTC *)
    duration_ms = Some 1500;
  } in
  let json = Masc_mcp.Voice_conference.transcript_entry_to_json entry in
  let open Yojson.Safe.Util in
  check int "seq" 1 (json |> member "seq" |> to_int);
  check string "agent_id" "claude" (json |> member "agent_id" |> to_string);
  check string "message" "Hello world" (json |> member "message" |> to_string);
  check string "voice" "Sarah" (json |> member "voice" |> to_string);
  check int "duration_ms" 1500 (json |> member "duration_ms" |> to_int)

let test_transcript_entry_no_duration () =
  let entry = Masc_mcp.Voice_conference.{
    seq = 2;
    agent_id = "gemini";
    message = "Test";
    voice = "Roger";
    timestamp = 1704067200.0;
    duration_ms = None;
  } in
  let json = Masc_mcp.Voice_conference.transcript_entry_to_json entry in
  let open Yojson.Safe.Util in
  let duration = json |> member "duration_ms" in
  check bool "duration is null" true (duration = `Null)

(** {1 Status Tests} *)

let test_status_json () =
  let dir = tmp_dir () in
  let config = make_config ~name:"status_test" ["claude"; "gemini"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  let json = Masc_mcp.Voice_conference.status_json conf in
  let open Yojson.Safe.Util in
  check string "name" "status_test" (json |> member "name" |> to_string);
  check string "state" "idle" (json |> member "state" |> to_string);
  check int "pending_turns" 0 (json |> member "pending_turns" |> to_int);
  check int "transcript_length" 0 (json |> member "transcript_length" |> to_int);
  cleanup_dir dir

let test_status_json_with_pending () =
  let dir = tmp_dir () in
  let config = make_config ["claude"] in
  let conf = Masc_mcp.Voice_conference.create ~config ~config_path:dir in
  let _ = Masc_mcp.Voice_conference.speak conf ~agent_id:"claude" ~message:"Test" () in
  let json = Masc_mcp.Voice_conference.status_json conf in
  let open Yojson.Safe.Util in
  check int "pending_turns" 1 (json |> member "pending_turns" |> to_int);
  cleanup_dir dir

(** {1 Test Suite} *)

let () =
  run "Voice_conference" [
    "state", [
      test_case "string_of_state" `Quick test_string_of_state;
    ];
    "creation", [
      test_case "create conference" `Quick test_create_conference;
      test_case "initial state is idle" `Quick test_initial_state_idle;
    ];
    "participants", [
      test_case "participants list" `Quick test_participants_list;
      test_case "add participant" `Quick test_add_participant;
      test_case "add duplicate participant" `Quick test_add_duplicate_participant;
      test_case "remove participant" `Quick test_remove_participant;
      test_case "remove nonexistent" `Quick test_remove_nonexistent_participant;
    ];
    "turn_management", [
      test_case "speak enqueues turn" `Quick test_speak_enqueues_turn;
      test_case "speak multiple agents" `Quick test_speak_multiple_agents;
      test_case "speak with priority" `Quick test_speak_with_priority;
    ];
    "pause_resume", [
      test_case "pause conference" `Quick test_pause_conference;
      test_case "resume conference" `Quick test_resume_conference;
    ];
    "transcript", [
      test_case "empty transcript" `Quick test_empty_transcript;
      test_case "clear transcript" `Quick test_clear_transcript;
      test_case "export json" `Quick test_export_transcript_json;
      test_case "export markdown" `Quick test_export_transcript_md;
    ];
    "transcript_entry", [
      test_case "entry to json" `Quick test_transcript_entry_to_json;
      test_case "entry no duration" `Quick test_transcript_entry_no_duration;
    ];
    "status", [
      test_case "status json" `Quick test_status_json;
      test_case "status with pending" `Quick test_status_json_with_pending;
    ];
  ]
