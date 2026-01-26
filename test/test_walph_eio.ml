(** Eio-native Walph Wiggum Tests

    Verifies that Room_walph_eio works correctly in Eio fiber context:
    1. Non-blocking mutex (fiber-friendly)
    2. Condition variable based pause/resume
    3. Concurrent control from multiple fibers
    4. No zombie states on exceptions
*)

open Alcotest

(** Test fixture: Create isolated config with Eio environment *)
let with_test_config name f =
  Eio_main.run @@ fun env ->
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "walph_eio_%s_%d_%d" name (Unix.getpid ())
      (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir tmp_dir 0o755;
  let fs = Eio.Stdenv.fs env in
  let config = Masc_mcp.Room.default_config tmp_dir in
  let _ = Masc_mcp.Room.init config ~agent_name:(Some "test-eio-agent") in
  Fun.protect
    ~finally:(fun () ->
      let _ = Masc_mcp.Room.reset config in
      let path = Masc_mcp.Room.masc_dir config in
      if Sys.file_exists path then
        let _ = Sys.command (Printf.sprintf "rm -rf %s" path) in ();
      let parent = Filename.dirname path in
      if Sys.file_exists parent then
        try Unix.rmdir parent with _ -> ())
    (fun () -> f env fs config)

(** Test: Basic state machine in Eio context *)
let test_eio_basic_state () =
  with_test_config "basic" @@ fun _env _fs config ->
  let state = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"tester" in
  check bool "not running initially" false state.running;
  check bool "not paused initially" false state.paused

(** Test: Control commands work in Eio context *)
let test_eio_control_commands () =
  with_test_config "control" @@ fun _env _fs config ->
  let state = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"tester" in

  (* Simulate running state *)
  Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
    state.running <- true
  );

  (* Test PAUSE *)
  let _ = Masc_mcp.Room_walph_eio.walph_control config
    ~from_agent:"tester" ~command:"PAUSE" ~args:"" () in
  check bool "paused after PAUSE" true state.paused;

  (* Test RESUME *)
  let _ = Masc_mcp.Room_walph_eio.walph_control config
    ~from_agent:"tester" ~command:"RESUME" ~args:"" () in
  check bool "not paused after RESUME" false state.paused;

  (* Test STOP *)
  let _ = Masc_mcp.Room_walph_eio.walph_control config
    ~from_agent:"tester" ~command:"STOP" ~args:"" () in
  check bool "stop requested after STOP" true state.stop_requested

(** Test: Concurrent control from multiple fibers *)
let test_eio_concurrent_control () =
  with_test_config "concurrent" @@ fun _env _fs config ->
  let state = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"tester" in

  (* Set running *)
  Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
    state.running <- true
  );

  (* Run PAUSE and RESUME concurrently in separate fibers *)
  let pause_count = ref 0 in
  let resume_count = ref 0 in

  Eio.Fiber.both
    (fun () ->
      for _ = 1 to 10 do
        let _ = Masc_mcp.Room_walph_eio.walph_control config
          ~from_agent:"fiber1" ~command:"PAUSE" ~args:"" () in
        incr pause_count
      done)
    (fun () ->
      for _ = 1 to 10 do
        let _ = Masc_mcp.Room_walph_eio.walph_control config
          ~from_agent:"fiber2" ~command:"RESUME" ~args:"" () in
        incr resume_count
      done);

  check int "all pauses executed" 10 !pause_count;
  check int "all resumes executed" 10 !resume_count

(** Test: State isolation between agents in Eio context *)
let test_eio_room_isolation () =
  with_test_config "isolation" @@ fun _env _fs config ->
  (* Now we test agent isolation within same room *)
  let state1 = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"agent1" in
  let state2 = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"agent2" in

  (* Modify state1 *)
  Eio.Mutex.use_rw ~protect:true state1.mutex (fun () ->
    state1.running <- true;
    state1.iterations <- 100
  );

  (* state2 should be unaffected - different agent, same room *)
  check bool "state2 not running" false state2.running;
  check int "state2 zero iterations" 0 state2.iterations

(** Test: Cleanup function with zombie prevention *)
let test_eio_cleanup () =
  with_test_config "cleanup" @@ fun _env _fs config ->
  (* Create and modify state *)
  let state = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"tester" in
  Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
    state.running <- true
  );

  (* Remove should FAIL when running (zombie prevention) *)
  let remove_failed = try
    Masc_mcp.Room_walph_eio.remove_walph_state config ~agent_name:"tester";
    false  (* Should not reach here *)
  with Failure _ -> true in
  check bool "remove fails when running" true remove_failed;

  (* Stop the state first *)
  Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
    state.running <- false
  );

  (* Now remove should succeed *)
  Masc_mcp.Room_walph_eio.remove_walph_state config ~agent_name:"tester";

  (* Getting state again should return fresh state *)
  let new_state = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"tester" in
  check bool "new state not running" false new_state.running

(** Test: 3 agents running Walph simultaneously (Phase 1 feature) *)
let test_eio_multi_agent_walph () =
  with_test_config "multi_agent" @@ fun _env _fs config ->
  (* 3 agents get their own independent Walph states *)
  let state_claude = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"claude" in
  let state_codex = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"codex" in
  let state_gemini = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"gemini" in

  (* All start as not running *)
  check bool "claude not running initially" false state_claude.running;
  check bool "codex not running initially" false state_codex.running;
  check bool "gemini not running initially" false state_gemini.running;

  (* Each agent can set running=true independently (simulating walph_loop start) *)
  Eio.Mutex.use_rw ~protect:true state_claude.mutex (fun () ->
    state_claude.running <- true;
    state_claude.current_preset <- "drain"
  );
  Eio.Mutex.use_rw ~protect:true state_codex.mutex (fun () ->
    state_codex.running <- true;
    state_codex.current_preset <- "coverage"
  );
  Eio.Mutex.use_rw ~protect:true state_gemini.mutex (fun () ->
    state_gemini.running <- true;
    state_gemini.current_preset <- "refactor"
  );

  (* All 3 are now running simultaneously - no conflict! *)
  check bool "claude running" true state_claude.running;
  check bool "codex running" true state_codex.running;
  check bool "gemini running" true state_gemini.running;

  (* Each has different preset *)
  check string "claude preset" "drain" state_claude.current_preset;
  check string "codex preset" "coverage" state_codex.current_preset;
  check string "gemini preset" "refactor" state_gemini.current_preset;

  (* Pause only claude - others unaffected *)
  let _ = Masc_mcp.Room_walph_eio.walph_control config
    ~from_agent:"claude" ~command:"PAUSE" ~args:"" () in
  check bool "claude paused" true state_claude.paused;
  check bool "codex not paused" false state_codex.paused;
  check bool "gemini not paused" false state_gemini.paused

(** Test: list_walph_states returns all active agents *)
let test_eio_list_walph_states () =
  with_test_config "list_states" @@ fun _env _fs config ->
  (* Create states for 3 agents *)
  let _ = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"agent-a" in
  let _ = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"agent-b" in
  let _ = Masc_mcp.Room_walph_eio.get_walph_state config ~agent_name:"agent-c" in

  (* List all states *)
  let states = Masc_mcp.Room_walph_eio.list_walph_states config in
  check int "3 agents in room" 3 (List.length states);

  (* Check agent names are present *)
  let agent_names = List.map fst states in
  check bool "has agent-a" true (List.mem "agent-a" agent_names);
  check bool "has agent-b" true (List.mem "agent-b" agent_names);
  check bool "has agent-c" true (List.mem "agent-c" agent_names)

(* ============================================
   Test Registration
   ============================================ *)

let eio_tests = [
  "basic state in Eio", `Quick, test_eio_basic_state;
  "control commands in Eio", `Quick, test_eio_control_commands;
  "concurrent control", `Quick, test_eio_concurrent_control;
  "room isolation in Eio", `Quick, test_eio_room_isolation;
  "cleanup function", `Quick, test_eio_cleanup;
  "multi-agent Walph", `Quick, test_eio_multi_agent_walph;
  "list walph states", `Quick, test_eio_list_walph_states;
]

let () =
  run "Walph Eio" [
    "Eio Native", eio_tests;
  ]
