(** E2E Integration Tests for Voice System

    Tests the complete voice conference workflow:
    - Voice Conference creation with multiple agents
    - Turn-based speaking with priority
    - Session management integration
    - State transitions (pause/resume)

    Note: These tests don't create actual WebSocket/audio connections,
    they test the complete logical flow of the voice system.
*)

open Alcotest
open Masc_mcp

(** {1 Test Utilities} *)

let magi_agents = ["claude"; "gemini"; "codex"]

let test_config_path = "/tmp/test_voice_e2e"

(** Ensure test directories exist *)
let setup_test_dirs () =
  (try Unix.mkdir test_config_path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let voice_sessions_dir = Filename.concat test_config_path "voice_sessions" in
  (try Unix.mkdir voice_sessions_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let () = setup_test_dirs ()

let make_config ?(name = "MAGI") ?(agents = magi_agents) () : Voice_conference.conference_config =
  {
    name;
    agent_ids = agents;
    auto_persist_transcript = false;
    max_turn_duration_ms = None;
  }

(** {1 Scenario 1: MAGI Conference Creation} *)

let test_magi_conference_creation () =
  let config = make_config ~name:"MAGI Deliberation" () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in
  (* Verify via status_json *)
  let status = Voice_conference.status_json conf in
  let open Yojson.Safe.Util in
  let name = status |> member "name" |> to_string in
  check string "name" "MAGI Deliberation" name;
  let participants = Voice_conference.participants conf in
  check int "3 participants" 3 (List.length participants);
  check bool "claude in" true (List.mem "claude" participants);
  check bool "gemini in" true (List.mem "gemini" participants);
  check bool "codex in" true (List.mem "codex" participants)

let test_magi_conference_state () =
  let config = make_config () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in
  check string "initial state" "idle" (Voice_conference.string_of_state (Voice_conference.state conf))

(** {1 Scenario 2: Turn-Based Speaking} *)

let test_speaking_adds_to_queue () =
  let config = make_config () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in
  (* All agents speak - adds to turn queue *)
  let _ = Voice_conference.speak conf ~agent_id:"claude" ~message:"First" () in
  let _ = Voice_conference.speak conf ~agent_id:"gemini" ~message:"Second" () in
  let _ = Voice_conference.speak conf ~agent_id:"codex" ~message:"Third" () in

  let pending = Voice_conference.pending_turns conf in
  check int "3 turns queued" 3 pending

let test_speaking_with_priority () =
  let config = make_config () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in
  (* codex speaks first but with low priority *)
  let _ = Voice_conference.speak conf ~agent_id:"codex" ~message:"Low priority" ~priority:3 () in
  (* claude speaks with high priority *)
  let _ = Voice_conference.speak conf ~agent_id:"claude" ~message:"High priority" ~priority:1 () in
  (* gemini speaks with medium priority *)
  let _ = Voice_conference.speak conf ~agent_id:"gemini" ~message:"Medium" ~priority:2 () in

  check int "3 pending" 3 (Voice_conference.pending_turns conf)

(** {1 Scenario 3: Session Manager Integration} *)

let test_session_manager_multi_agent () =
  let manager = Voice_session_manager.create ~config_path:test_config_path in

  (* Start sessions for all MAGI agents *)
  let _ = Voice_session_manager.start_session manager ~agent_id:"claude" ~voice:"Sarah" () in
  let _ = Voice_session_manager.start_session manager ~agent_id:"gemini" ~voice:"Roger" () in
  let _ = Voice_session_manager.start_session manager ~agent_id:"codex" ~voice:"George" () in

  let sessions = Voice_session_manager.list_sessions manager in
  check int "3 sessions" 3 (List.length sessions);

  (* Each agent has their own session *)
  check bool "claude session" true (Voice_session_manager.has_session manager ~agent_id:"claude");
  check bool "gemini session" true (Voice_session_manager.has_session manager ~agent_id:"gemini");
  check bool "codex session" true (Voice_session_manager.has_session manager ~agent_id:"codex")

let test_session_heartbeat_activity () =
  let manager = Voice_session_manager.create ~config_path:test_config_path in
  let session = Voice_session_manager.start_session manager ~agent_id:"claude" () in
  let initial_activity = session.Voice_session_manager.last_activity in

  Unix.sleepf 0.01;
  Voice_session_manager.heartbeat manager ~agent_id:"claude";

  match Voice_session_manager.get_session manager ~agent_id:"claude" with
  | Some updated -> check bool "activity updated" true (updated.Voice_session_manager.last_activity > initial_activity)
  | None -> fail "session should exist"

let test_session_end () =
  let manager = Voice_session_manager.create ~config_path:test_config_path in
  let _ = Voice_session_manager.start_session manager ~agent_id:"claude" () in
  check bool "has session before" true (Voice_session_manager.has_session manager ~agent_id:"claude");

  let ended = Voice_session_manager.end_session manager ~agent_id:"claude" in
  check bool "end returned true" true ended;
  check bool "no session after" false (Voice_session_manager.has_session manager ~agent_id:"claude")

(** {1 Scenario 4: Conference with Sessions} *)

let test_conference_with_sessions () =
  (* Create session manager and conference *)
  let config = make_config ~name:"MAGI Deliberation" () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in

  (* Check initial state *)
  check string "initial state" "idle" (Voice_conference.string_of_state (Voice_conference.state conf));

  (* Add turns *)
  let _ = Voice_conference.speak conf ~agent_id:"codex" ~message:"Technical analysis" ~priority:1 () in
  let _ = Voice_conference.speak conf ~agent_id:"claude" ~message:"Values perspective" ~priority:1 () in
  let _ = Voice_conference.speak conf ~agent_id:"gemini" ~message:"Strategic view" ~priority:1 () in

  check int "3 pending turns" 3 (Voice_conference.pending_turns conf)

(** {1 Scenario 5: Pause/Resume} *)

let test_conference_pause_from_idle () =
  let config = make_config () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in

  (* Pause from Idle should stay Idle (can't pause idle) *)
  Voice_conference.pause conf ~reason:"User interrupt";

  (* Check state - should still be idle since we can only pause Active *)
  check string "still idle" "idle" (Voice_conference.string_of_state (Voice_conference.state conf))

let test_conference_resume_from_idle () =
  let config = make_config () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in

  (* Resume from Idle should stay Idle (nothing to resume) *)
  Voice_conference.resume conf;

  check string "still idle" "idle" (Voice_conference.string_of_state (Voice_conference.state conf))

(** {1 Scenario 6: Turn Queue Priority Order} *)

let test_turn_queue_priority_dequeue () =
  let queue = Turn_queue.create () in

  (* Add turns with different priorities *)
  let _ = Turn_queue.enqueue queue ~agent_id:"low" ~message:"Low" ~priority:3 () in
  let _ = Turn_queue.enqueue queue ~agent_id:"high" ~message:"High" ~priority:1 () in
  let _ = Turn_queue.enqueue queue ~agent_id:"medium" ~message:"Medium" ~priority:2 () in

  (* Dequeue should return in priority order *)
  match Turn_queue.dequeue queue with
  | Some turn1 ->
    check string "first: high" "high" turn1.Turn_queue.agent_id;
    (match Turn_queue.dequeue queue with
     | Some turn2 ->
       check string "second: medium" "medium" turn2.Turn_queue.agent_id;
       (match Turn_queue.dequeue queue with
        | Some turn3 ->
          check string "third: low" "low" turn3.Turn_queue.agent_id
        | None -> fail "expected third turn")
     | None -> fail "expected second turn")
  | None -> fail "expected first turn"

let test_turn_queue_peek () =
  let queue = Turn_queue.create () in
  let _ = Turn_queue.enqueue queue ~agent_id:"claude" ~message:"Hello" ~priority:1 () in
  let _ = Turn_queue.enqueue queue ~agent_id:"gemini" ~message:"World" ~priority:2 () in

  (* Peek should not remove *)
  match Turn_queue.peek queue with
  | Some peeked ->
    check string "peeked agent" "claude" peeked.Turn_queue.agent_id;
    check int "still 2 in queue" 2 (Turn_queue.length queue)
  | None -> fail "peek should return turn"

(** {1 Scenario 7: Participant Management} *)

(* Note: add_participant/remove_participant only start/end sessions,
   they don't modify the config's agent_ids list *)

let test_add_participant () =
  let config = make_config ~agents:["claude"; "gemini"] () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in

  (* Adding a new participant (starts session) *)
  let added = Voice_conference.add_participant conf ~agent_id:"codex" in
  check bool "added (session started)" true added;

  (* Adding duplicate returns false *)
  let added_dup = Voice_conference.add_participant conf ~agent_id:"claude" in
  check bool "duplicate not added" false added_dup

let test_remove_participant () =
  let config = make_config () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in

  (* Removing a participant (ends session) *)
  let removed = Voice_conference.remove_participant conf ~agent_id:"codex" in
  check bool "removed (session ended)" true removed;

  (* Removing nonexistent returns false *)
  let removed_again = Voice_conference.remove_participant conf ~agent_id:"nonexistent" in
  check bool "nonexistent not removed" false removed_again

(** {1 Scenario 8: Voice Bridge Configuration} *)

let test_voice_bridge_agent_voices () =
  let voices = Voice_bridge.agent_voices () in
  check bool "has claude" true (List.exists (fun (a, _) -> a = "claude") voices);
  check bool "has gemini" true (List.exists (fun (a, _) -> a = "gemini") voices);
  check bool "has codex" true (List.exists (fun (a, _) -> a = "codex") voices)

let test_voice_bridge_get_voice () =
  let claude_voice = Voice_bridge.get_voice_for_agent "claude" in
  let gemini_voice = Voice_bridge.get_voice_for_agent "gemini" in
  let codex_voice = Voice_bridge.get_voice_for_agent "codex" in

  (* Check they have voices (actual values may vary) *)
  check bool "claude has voice" true (String.length claude_voice > 0);
  check bool "gemini has voice" true (String.length gemini_voice > 0);
  check bool "codex has voice" true (String.length codex_voice > 0)

(** {1 Scenario 9: Transcript Export Formats} *)

let test_transcript_export_json_empty () =
  let config = make_config () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in

  let json = Voice_conference.export_transcript_json conf in
  let open Yojson.Safe.Util in
  check int "zero entries" 0 (json |> member "total_turns" |> to_int)

let test_transcript_export_md_empty () =
  let config = make_config () in
  let conf = Voice_conference.create ~config ~config_path:test_config_path in

  let md = Voice_conference.export_transcript_md conf in
  check bool "has header" true (String.length md > 0 && String.sub md 0 1 = "#")

(** {1 Test Suites} *)

let conference_tests = [
  "magi_conference_creation", `Quick, test_magi_conference_creation;
  "magi_conference_state", `Quick, test_magi_conference_state;
]

let speaking_tests = [
  "speaking_adds_to_queue", `Quick, test_speaking_adds_to_queue;
  "speaking_with_priority", `Quick, test_speaking_with_priority;
]

let session_tests = [
  "session_manager_multi_agent", `Quick, test_session_manager_multi_agent;
  "session_heartbeat_activity", `Quick, test_session_heartbeat_activity;
  "session_end", `Quick, test_session_end;
]

let workflow_tests = [
  "conference_with_sessions", `Quick, test_conference_with_sessions;
]

let pause_resume_tests = [
  "conference_pause_from_idle", `Quick, test_conference_pause_from_idle;
  "conference_resume_from_idle", `Quick, test_conference_resume_from_idle;
]

let turn_queue_tests = [
  "turn_queue_priority_dequeue", `Quick, test_turn_queue_priority_dequeue;
  "turn_queue_peek", `Quick, test_turn_queue_peek;
]

let participant_tests = [
  "add_participant", `Quick, test_add_participant;
  "remove_participant", `Quick, test_remove_participant;
]

let voice_bridge_tests = [
  "voice_bridge_agent_voices", `Quick, test_voice_bridge_agent_voices;
  "voice_bridge_get_voice", `Quick, test_voice_bridge_get_voice;
]

let transcript_tests = [
  "transcript_export_json_empty", `Quick, test_transcript_export_json_empty;
  "transcript_export_md_empty", `Quick, test_transcript_export_md_empty;
]

let () =
  Alcotest.run "Voice E2E" [
    ("conference", conference_tests);
    ("speaking", speaking_tests);
    ("session", session_tests);
    ("workflow", workflow_tests);
    ("pause_resume", pause_resume_tests);
    ("turn_queue", turn_queue_tests);
    ("participant", participant_tests);
    ("voice_bridge", voice_bridge_tests);
    ("transcript", transcript_tests);
  ]
