(** Tests for Voice Session Manager module *)

open Masc_mcp.Voice_session_manager

(** Helper to create a temp directory - deterministic using PID + timestamp *)
let with_temp_dir f =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc_test_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir dir 0o755;
  Fun.protect ~finally:(fun () ->
    (* Cleanup: remove files and dir *)
    (try
      Array.iter (fun file ->
        Sys.remove (Filename.concat dir file)
      ) (Sys.readdir dir);
      Unix.rmdir dir
    with _ -> ())
  ) (fun () -> f dir)

(** ============================================
    Unit Tests - Creation
    ============================================ *)

let test_create_manager () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    Alcotest.(check int) "no sessions initially" 0 (session_count m)
  )

let test_generate_session_id_unique () =
  let id1 = generate_session_id () in
  let id2 = generate_session_id () in
  let id3 = generate_session_id () in
  Alcotest.(check bool) "id1 != id2" true (id1 <> id2);
  Alcotest.(check bool) "id2 != id3" true (id2 <> id3);
  (* Check prefix *)
  Alcotest.(check bool) "starts with vs-" true (String.sub id1 0 3 = "vs-")

(** ============================================
    Unit Tests - Session Lifecycle
    ============================================ *)

let test_start_session () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let session = start_session m ~agent_id:"claude" () in
    Alcotest.(check string) "agent_id" "claude" session.agent_id;
    Alcotest.(check int) "turn_count" 0 session.turn_count;
    Alcotest.(check string) "status" "active" (string_of_status session.status);
    Alcotest.(check int) "session count" 1 (session_count m)
  )

let test_start_session_with_voice () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let session = start_session m ~agent_id:"claude" ~voice:"Custom" () in
    Alcotest.(check string) "voice" "Custom" session.voice
  )

let test_start_session_returns_existing () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let s1 = start_session m ~agent_id:"claude" () in
    let s2 = start_session m ~agent_id:"claude" () in
    Alcotest.(check string) "same session_id" s1.session_id s2.session_id;
    Alcotest.(check int) "still one session" 1 (session_count m)
  )

let test_end_session () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let _ = start_session m ~agent_id:"claude" () in
    Alcotest.(check int) "one session" 1 (session_count m);
    let ended = end_session m ~agent_id:"claude" in
    Alcotest.(check bool) "ended" true ended;
    Alcotest.(check int) "no sessions" 0 (session_count m)
  )

let test_end_nonexistent_session () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let ended = end_session m ~agent_id:"ghost" in
    Alcotest.(check bool) "not ended" false ended
  )

let test_suspend_resume () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let _ = start_session m ~agent_id:"claude" () in

    suspend_session m ~agent_id:"claude";
    let s1 = get_session m ~agent_id:"claude" in
    Alcotest.(check (option string)) "suspended"
      (Some "suspended")
      (Option.map (fun s -> string_of_status s.status) s1);

    resume_session m ~agent_id:"claude";
    let s2 = get_session m ~agent_id:"claude" in
    Alcotest.(check (option string)) "active"
      (Some "active")
      (Option.map (fun s -> string_of_status s.status) s2)
  )

(** ============================================
    Unit Tests - Session Query
    ============================================ *)

let test_get_session () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let _ = start_session m ~agent_id:"claude" () in
    let session = get_session m ~agent_id:"claude" in
    Alcotest.(check (option string)) "found"
      (Some "claude")
      (Option.map (fun s -> s.agent_id) session)
  )

let test_get_nonexistent_session () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let session = get_session m ~agent_id:"ghost" in
    Alcotest.(check (option string)) "not found" None
      (Option.map (fun s -> s.agent_id) session)
  )

let test_list_sessions () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let _ = start_session m ~agent_id:"claude" () in
    let _ = start_session m ~agent_id:"gemini" () in
    let _ = start_session m ~agent_id:"codex" () in
    let sessions = list_sessions m in
    Alcotest.(check int) "three sessions" 3 (List.length sessions)
  )

let test_has_session () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let _ = start_session m ~agent_id:"claude" () in
    Alcotest.(check bool) "has claude" true (has_session m ~agent_id:"claude");
    Alcotest.(check bool) "no gemini" false (has_session m ~agent_id:"gemini")
  )

(** ============================================
    Unit Tests - Activity Tracking
    ============================================ *)

let test_heartbeat () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let s1 = start_session m ~agent_id:"claude" () in
    let old_activity = s1.last_activity in
    Unix.sleepf 0.01;
    heartbeat m ~agent_id:"claude";
    let s2 = get_session m ~agent_id:"claude" in
    match s2 with
    | Some s ->
      Alcotest.(check bool) "activity updated" true (s.last_activity > old_activity)
    | None ->
      Alcotest.fail "session not found"
  )

let test_increment_turn () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let _ = start_session m ~agent_id:"claude" () in
    increment_turn m ~agent_id:"claude";
    increment_turn m ~agent_id:"claude";
    increment_turn m ~agent_id:"claude";
    let session = get_session m ~agent_id:"claude" in
    Alcotest.(check (option int)) "turn count"
      (Some 3)
      (Option.map (fun s -> s.turn_count) session)
  )

(** ============================================
    Unit Tests - Zombie Cleanup
    ============================================ *)

let test_cleanup_zombies_none () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let _ = start_session m ~agent_id:"claude" () in
    let cleaned = cleanup_zombies m ~timeout:300.0 () in
    Alcotest.(check int) "no zombies" 0 cleaned;
    Alcotest.(check int) "session remains" 1 (session_count m)
  )

let test_cleanup_zombies_removes_old () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let _ = start_session m ~agent_id:"claude" () in
    (* Force session to be old by directly modifying *)
    (match get_session m ~agent_id:"claude" with
     | Some s -> s.last_activity <- Unix.gettimeofday () -. 400.0
     | None -> ());
    let cleaned = cleanup_zombies m ~timeout:300.0 () in
    Alcotest.(check int) "one zombie" 1 cleaned;
    Alcotest.(check int) "no sessions" 0 (session_count m)
  )

(** ============================================
    Unit Tests - Persistence
    ============================================ *)

let test_persist_restore () =
  with_temp_dir (fun dir ->
    (* Create and persist *)
    let m1 = create ~config_path:dir in
    let _ = start_session m1 ~agent_id:"claude" () in
    let _ = start_session m1 ~agent_id:"gemini" () in
    persist m1;

    (* Create new manager and restore *)
    let m2 = create ~config_path:dir in
    Alcotest.(check int) "empty before restore" 0 (session_count m2);
    restore m2;
    Alcotest.(check int) "two sessions restored" 2 (session_count m2);
    Alcotest.(check bool) "claude restored" true (has_session m2 ~agent_id:"claude");
    Alcotest.(check bool) "gemini restored" true (has_session m2 ~agent_id:"gemini")
  )

(** ============================================
    Unit Tests - JSON Serialization
    ============================================ *)

let test_session_to_json () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let session = start_session m ~agent_id:"claude" ~voice:"Sarah" () in
    let json = session_to_json session in
    let open Yojson.Safe.Util in
    Alcotest.(check string) "agent_id" "claude" (json |> member "agent_id" |> to_string);
    Alcotest.(check string) "voice" "Sarah" (json |> member "voice" |> to_string);
    Alcotest.(check string) "status" "active" (json |> member "status" |> to_string)
  )

let test_status_json () =
  with_temp_dir (fun dir ->
    let m = create ~config_path:dir in
    let _ = start_session m ~agent_id:"claude" () in
    let _ = start_session m ~agent_id:"gemini" () in
    let json = status_json m in
    let open Yojson.Safe.Util in
    Alcotest.(check int) "session_count" 2 (json |> member "session_count" |> to_int)
  )

(** ============================================
    Unit Tests - String Conversions
    ============================================ *)

let test_string_of_status () =
  Alcotest.(check string) "active" "active" (string_of_status Active);
  Alcotest.(check string) "idle" "idle" (string_of_status Idle);
  Alcotest.(check string) "suspended" "suspended" (string_of_status Suspended)

(** ============================================
    Test Suite Registration
    ============================================ *)

let creation_tests = [
  "create_manager", `Quick, test_create_manager;
  "generate_session_id_unique", `Quick, test_generate_session_id_unique;
]

let lifecycle_tests = [
  "start_session", `Quick, test_start_session;
  "start_session_with_voice", `Quick, test_start_session_with_voice;
  "start_session_returns_existing", `Quick, test_start_session_returns_existing;
  "end_session", `Quick, test_end_session;
  "end_nonexistent_session", `Quick, test_end_nonexistent_session;
  "suspend_resume", `Quick, test_suspend_resume;
]

let query_tests = [
  "get_session", `Quick, test_get_session;
  "get_nonexistent_session", `Quick, test_get_nonexistent_session;
  "list_sessions", `Quick, test_list_sessions;
  "has_session", `Quick, test_has_session;
]

let activity_tests = [
  "heartbeat", `Quick, test_heartbeat;
  "increment_turn", `Quick, test_increment_turn;
]

let zombie_tests = [
  "cleanup_zombies_none", `Quick, test_cleanup_zombies_none;
  "cleanup_zombies_removes_old", `Quick, test_cleanup_zombies_removes_old;
]

let persistence_tests = [
  "persist_restore", `Quick, test_persist_restore;
]

let json_tests = [
  "session_to_json", `Quick, test_session_to_json;
  "status_json", `Quick, test_status_json;
]

let string_tests = [
  "string_of_status", `Quick, test_string_of_status;
]

let () =
  Alcotest.run "Voice Session Manager" [
    "creation", creation_tests;
    "lifecycle", lifecycle_tests;
    "query", query_tests;
    "activity", activity_tests;
    "zombie", zombie_tests;
    "persistence", persistence_tests;
    "json", json_tests;
    "strings", string_tests;
  ]
