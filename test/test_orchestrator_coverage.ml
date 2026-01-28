(** Orchestrator Module Coverage Tests

    Tests for MASC Self-sustaining Agent Coordination:
    - config record type and fields
    - default_config: default values
    - load_config: environment-based configuration
*)

open Alcotest

module Orchestrator = Masc_mcp.Orchestrator

(* ============================================================
   default_config Tests
   ============================================================ *)

let test_default_config_interval () =
  check bool "interval positive" true
    (Orchestrator.default_config.check_interval_s > 0.0)

let test_default_config_interval_value () =
  check bool "interval is 300s" true
    (Orchestrator.default_config.check_interval_s = 300.0)

let test_default_config_min_priority () =
  check int "min_priority" 2 Orchestrator.default_config.min_priority

let test_default_config_agent_timeout () =
  check int "agent_timeout" 300 Orchestrator.default_config.agent_timeout_s

let test_default_config_agent () =
  check string "orchestrator_agent" "claude"
    Orchestrator.default_config.orchestrator_agent

let test_default_config_enabled () =
  check bool "disabled by default" false Orchestrator.default_config.enabled

let test_default_config_port () =
  check int "port" 8935 Orchestrator.default_config.port

(* ============================================================
   load_config Tests
   ============================================================ *)

let test_load_config_returns_config () =
  let cfg = Orchestrator.load_config () in
  check bool "interval positive" true (cfg.check_interval_s > 0.0)

let test_load_config_interval_positive () =
  let cfg = Orchestrator.load_config () in
  check bool "positive interval" true (cfg.check_interval_s >= 1.0)

let test_load_config_min_priority_positive () =
  let cfg = Orchestrator.load_config () in
  check bool "positive priority" true (cfg.min_priority >= 0)

let test_load_config_agent_timeout_positive () =
  let cfg = Orchestrator.load_config () in
  check bool "positive timeout" true (cfg.agent_timeout_s > 0)

let test_load_config_agent_nonempty () =
  let cfg = Orchestrator.load_config () in
  check bool "nonempty agent" true (String.length cfg.orchestrator_agent > 0)

let test_load_config_port_valid () =
  let cfg = Orchestrator.load_config () in
  check bool "valid port" true (cfg.port > 0 && cfg.port < 65536)

(* ============================================================
   config Record Tests
   ============================================================ *)

let test_config_check_interval_type () =
  let cfg = Orchestrator.default_config in
  let _ : float = cfg.check_interval_s in
  check bool "check_interval_s is float" true true

let test_config_min_priority_type () =
  let cfg = Orchestrator.default_config in
  let _ : int = cfg.min_priority in
  check bool "min_priority is int" true true

let test_config_agent_timeout_type () =
  let cfg = Orchestrator.default_config in
  let _ : int = cfg.agent_timeout_s in
  check bool "agent_timeout_s is int" true true

let test_config_orchestrator_agent_type () =
  let cfg = Orchestrator.default_config in
  let _ : string = cfg.orchestrator_agent in
  check bool "orchestrator_agent is string" true true

let test_config_enabled_type () =
  let cfg = Orchestrator.default_config in
  let _ : bool = cfg.enabled in
  check bool "enabled is bool" true true

let test_config_port_type () =
  let cfg = Orchestrator.default_config in
  let _ : int = cfg.port in
  check bool "port is int" true true

(* ============================================================
   make_orchestrator_prompt Tests
   ============================================================ *)

let test_make_orchestrator_prompt_basic () =
  let prompt = Orchestrator.make_orchestrator_prompt ~port:8935 in
  check bool "returns string" true (String.length prompt > 0)

let test_make_orchestrator_prompt_contains_mcp () =
  let prompt = Orchestrator.make_orchestrator_prompt ~port:8931 in
  check bool "contains mcp__masc" true
    (try
      let _ = Str.search_forward (Str.regexp "mcp__masc") prompt 0 in true
    with Not_found -> false)

let test_make_orchestrator_prompt_contains_tools () =
  let prompt = Orchestrator.make_orchestrator_prompt ~port:8935 in
  check bool "mentions masc_status" true
    (try
      let _ = Str.search_forward (Str.regexp "masc_status") prompt 0 in true
    with Not_found -> false)

let test_make_orchestrator_prompt_contains_claim () =
  let prompt = Orchestrator.make_orchestrator_prompt ~port:8935 in
  check bool "mentions masc_claim" true
    (try
      let _ = Str.search_forward (Str.regexp "masc_claim") prompt 0 in true
    with Not_found -> false)

let test_make_orchestrator_prompt_contains_done () =
  let prompt = Orchestrator.make_orchestrator_prompt ~port:8935 in
  check bool "mentions masc_done" true
    (try
      let _ = Str.search_forward (Str.regexp "masc_done") prompt 0 in true
    with Not_found -> false)

let test_make_orchestrator_prompt_mentions_broadcast () =
  let prompt = Orchestrator.make_orchestrator_prompt ~port:8935 in
  check bool "mentions masc_broadcast" true
    (try
      let _ = Str.search_forward (Str.regexp "masc_broadcast") prompt 0 in true
    with Not_found -> false)

(* ============================================================
   Config Field Bounds Tests
   ============================================================ *)

let test_config_reasonable_interval () =
  let cfg = Orchestrator.default_config in
  check bool "interval 1-3600s" true
    (cfg.check_interval_s >= 1.0 && cfg.check_interval_s <= 3600.0)

let test_config_reasonable_priority () =
  let cfg = Orchestrator.default_config in
  check bool "priority 0-10" true
    (cfg.min_priority >= 0 && cfg.min_priority <= 10)

let test_config_reasonable_timeout () =
  let cfg = Orchestrator.default_config in
  check bool "timeout 1-3600" true
    (cfg.agent_timeout_s >= 1 && cfg.agent_timeout_s <= 3600)

(* ============================================================
   should_orchestrate Tests (requires MASC Room)
   ============================================================ *)

module Room = Masc_mcp.Room

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter (fun name -> rm_rf (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Unix.unlink path

let make_test_dir () =
  let unique_id = Printf.sprintf "masc_orch_test_%d_%d"
    (Unix.getpid ())
    (int_of_float (Unix.gettimeofday () *. 1000000.)) in
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ()) unique_id in
  (try Unix.mkdir tmp_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  tmp_dir

let with_initialized_room f =
  let tmp_dir = make_test_dir () in
  let config = Room.default_config tmp_dir in
  let _ = Room.init config ~agent_name:None in
  Fun.protect
    ~finally:(fun () ->
      try
        let _ = Room.reset config in
        rm_rf tmp_dir
      with _ -> ())
    (fun () -> f config)

let test_should_orchestrate_empty_room () =
  with_initialized_room @@ fun config ->
  (* Empty room with no tasks and no agents should return false *)
  let result = Orchestrator.should_orchestrate config in
  check bool "no orchestration needed" false result

let test_should_orchestrate_with_task_no_agent () =
  with_initialized_room @@ fun config ->
  (* Add a high priority task *)
  let _ = Room.add_task config ~title:"Important Task" ~priority:1 ~description:"Test" in
  (* No active agents → should return true *)
  let result = Orchestrator.should_orchestrate config in
  check bool "orchestration needed" true result

let test_should_orchestrate_with_task_and_agent () =
  with_initialized_room @@ fun config ->
  (* Add task and join as agent *)
  let _ = Room.add_task config ~title:"Task" ~priority:1 ~description:"Test" in
  let _ = Room.join config ~agent_name:"active-agent" ~capabilities:[] () in
  (* Active agent exists → should return false *)
  let result = Orchestrator.should_orchestrate config in
  check bool "no orchestration with active agent" false result

let test_should_orchestrate_paused_room () =
  with_initialized_room @@ fun config ->
  (* Add task *)
  let _ = Room.add_task config ~title:"Task" ~priority:1 ~description:"Test" in
  (* Pause the room *)
  let _ = Room.pause config ~by:"test" ~reason:"Testing" in
  (* Paused room should return false *)
  let result = Orchestrator.should_orchestrate config in
  check bool "no orchestration when paused" false result

let test_should_orchestrate_low_priority_task () =
  with_initialized_room @@ fun config ->
  (* Add low priority task (priority 5 > threshold 2) *)
  let _ = Room.add_task config ~title:"Low Priority" ~priority:5 ~description:"Test" in
  (* Low priority tasks don't trigger orchestration *)
  let result = Orchestrator.should_orchestrate config in
  check bool "no orchestration for low priority" false result

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Orchestrator Coverage" [
    "default_config", [
      test_case "interval" `Quick test_default_config_interval;
      test_case "interval value" `Quick test_default_config_interval_value;
      test_case "min_priority" `Quick test_default_config_min_priority;
      test_case "agent_timeout" `Quick test_default_config_agent_timeout;
      test_case "agent" `Quick test_default_config_agent;
      test_case "enabled" `Quick test_default_config_enabled;
      test_case "port" `Quick test_default_config_port;
    ];
    "load_config", [
      test_case "returns config" `Quick test_load_config_returns_config;
      test_case "interval positive" `Quick test_load_config_interval_positive;
      test_case "priority positive" `Quick test_load_config_min_priority_positive;
      test_case "timeout positive" `Quick test_load_config_agent_timeout_positive;
      test_case "agent nonempty" `Quick test_load_config_agent_nonempty;
      test_case "port valid" `Quick test_load_config_port_valid;
    ];
    "config_types", [
      test_case "check_interval_s" `Quick test_config_check_interval_type;
      test_case "min_priority" `Quick test_config_min_priority_type;
      test_case "agent_timeout_s" `Quick test_config_agent_timeout_type;
      test_case "orchestrator_agent" `Quick test_config_orchestrator_agent_type;
      test_case "enabled" `Quick test_config_enabled_type;
      test_case "port" `Quick test_config_port_type;
    ];
    "make_orchestrator_prompt", [
      test_case "basic" `Quick test_make_orchestrator_prompt_basic;
      test_case "contains mcp" `Quick test_make_orchestrator_prompt_contains_mcp;
      test_case "contains tools" `Quick test_make_orchestrator_prompt_contains_tools;
      test_case "contains claim" `Quick test_make_orchestrator_prompt_contains_claim;
      test_case "contains done" `Quick test_make_orchestrator_prompt_contains_done;
      test_case "mentions broadcast" `Quick test_make_orchestrator_prompt_mentions_broadcast;
    ];
    "config_bounds", [
      test_case "reasonable interval" `Quick test_config_reasonable_interval;
      test_case "reasonable priority" `Quick test_config_reasonable_priority;
      test_case "reasonable timeout" `Quick test_config_reasonable_timeout;
    ];
    "should_orchestrate", [
      test_case "empty room" `Quick test_should_orchestrate_empty_room;
      test_case "task no agent" `Quick test_should_orchestrate_with_task_no_agent;
      test_case "task and agent" `Quick test_should_orchestrate_with_task_and_agent;
      test_case "paused room" `Quick test_should_orchestrate_paused_room;
      test_case "low priority task" `Quick test_should_orchestrate_low_priority_task;
    ];
  ]
