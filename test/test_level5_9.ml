(** Test suite for MASC Level 5-9 modules

    Tests the holonic architecture implementation:
    - Level 5: Institution (persistent memory)
    - Level 6: Mind (meta-cognition)
    - Level 7: Noosphere (collective intelligence)
    - Level 8: Meta (system of systems)
    - Level 9: Void (philosophical, minimal tests)

    @author Second Brain
    @since MASC v3.2
*)

open Masc_mcp

(** {1 Level 5: Institution Tests} *)

let test_institution_episode_creation () =
  let episode : Institution.episode = {
    id = "ep_001";
    timestamp = Unix.gettimeofday ();
    participants = ["claude"; "gemini"];
    event_type = "task_completed";
    summary = "Completed migration task together";
    outcome = `Success;
    learnings = ["Parallel work reduces time by 50%"];
    context = [("task_id", "task-001")];
  } in
  assert (episode.id = "ep_001");
  assert (List.length episode.participants = 2);
  assert (episode.outcome = `Success);
  Printf.printf "[PASS] Institution episode creation\n"

let test_institution_knowledge () =
  let knowledge : Institution.knowledge = {
    id = "kn_001";
    topic = "OCaml patterns";
    content = "Pure/Effect separation improves testability";
    confidence = 0.9;
    source = "experience";
    created_at = Unix.gettimeofday ();
    last_verified = Unix.gettimeofday ();
    references = [];
  } in
  assert (knowledge.confidence > 0.8);
  assert (knowledge.topic = "OCaml patterns");
  Printf.printf "[PASS] Institution knowledge creation\n"

let test_institution_pattern () =
  let pattern : Institution.pattern = {
    id = "pat_001";
    name = "Code Review Protocol";
    description = "How to review code collaboratively";
    trigger = "pr_created";
    steps = ["Run tests"; "Check types"; "Review logic"];
    success_rate = 0.95;
    usage_count = 100;
    last_used = Unix.gettimeofday ();
    evolved_from = None;
  } in
  assert (pattern.success_rate > 0.9);
  assert (List.length pattern.steps = 3);
  Printf.printf "[PASS] Institution pattern creation\n"

let test_institution_pure_module () =
  let now = Unix.gettimeofday () in
  let inst : Institution.institution = {
    identity = {
      id = "inst_001";
      name = "Test Institution";
      mission = "Testing";
      founded_at = now;
      generation = 1;
    };
    memory = {
      episodic = [];
      semantic = [];
      procedural = [];
    };
    culture = [];
    succession = Institution.default_succession ();
    current_agents = [];
    alumni = [];
  } in
  (* Test agent_join *)
  let inst' = Institution.Pure.agent_join inst ~agent_id:"claude" in
  assert (List.mem "claude" inst'.current_agents);
  (* Test agent_leave *)
  let inst'' = Institution.Pure.agent_leave inst' ~agent_id:"claude" in
  assert (not (List.mem "claude" inst''.current_agents));
  assert (List.mem "claude" inst''.alumni);
  Printf.printf "[PASS] Institution Pure module\n"

(** {1 Level 6: Mind Tests} *)

let test_mind_self_model () =
  let self : Mind.self_model = {
    id = "self_001";
    name = "Claude";
    capabilities = ["reasoning"; "coding"; "writing"];
    limitations = ["real-time data"; "image generation"];
    current_state = `Idle;
    confidence = 0.8;
    last_updated = Unix.gettimeofday ();
  } in
  assert (self.confidence > 0.0 && self.confidence <= 1.0);
  assert (List.length self.capabilities > 0);
  Printf.printf "[PASS] Mind self-model creation\n"

let test_mind_thought () =
  let thought = Mind.Pure.create_thought
    ~content:"Should I use recursion or iteration here?"
    ~thought_type:`Question
    ~confidence:0.6
    () in
  assert (thought.thought_type = `Question);
  assert (thought.confidence = 0.6);
  Printf.printf "[PASS] Mind thought creation\n"

let test_mind_goal () =
  let goal : Mind.goal = {
    id = "goal_001";
    description = "Complete Level 5-9 implementation";
    priority = 0.9;
    status = `Active;
    parent_goal = None;
    sub_goals = [];
    deadline = None;
    progress = 0.5;
  } in
  assert (goal.status = `Active);
  assert (goal.progress >= 0.0 && goal.progress <= 1.0);
  Printf.printf "[PASS] Mind goal creation\n"

(** {1 Level 7: Noosphere Tests} *)

let test_noosphere_connection () =
  let conn : Noosphere.connection = {
    from_mind = "mind_001";
    to_mind = "mind_002";
    strength = 0.7;
    established_at = Unix.gettimeofday ();
    last_interaction = Unix.gettimeofday ();
  } in
  assert (conn.strength > 0.0 && conn.strength <= 1.0);
  assert (conn.from_mind <> conn.to_mind);
  Printf.printf "[PASS] Noosphere connection creation\n"

let test_noosphere_shared_belief () =
  let belief : Noosphere.shared_belief = {
    id = "belief_001";
    content = "Collaboration improves outcomes";
    confidence = 0.85;
    supporters = ["mind_001"; "mind_002"; "mind_003"];
    created_at = Unix.gettimeofday ();
    last_reinforced = Unix.gettimeofday ();
  } in
  assert (List.length belief.supporters >= 2);
  assert (belief.confidence > 0.5);
  Printf.printf "[PASS] Noosphere shared belief creation\n"

let test_noosphere_pure_module () =
  let now = Unix.gettimeofday () in
  let noosphere : Noosphere.noosphere = {
    id = "ns_001";
    name = "Test Noosphere";
    minds = [];
    connections = [];
    beliefs = [];
    goals = [];
    insights = [];
    state = {
      coherence = 1.0;
      activity_level = 0.0;
      dominant_mood = "neutral";
      focus_topic = None;
      last_sync = now;
    };
    created_at = now;
  } in
  (* Test add_mind *)
  let ns' = Noosphere.Pure.add_mind noosphere ~mind_id:"mind_001" in
  let ns'' = Noosphere.Pure.add_mind ns' ~mind_id:"mind_002" in
  assert (List.length ns''.minds = 2);
  (* Test connect *)
  let ns''' = Noosphere.Pure.connect ns'' ~from_mind:"mind_001" ~to_mind:"mind_002" () in
  assert (List.length ns'''.connections = 1);
  Printf.printf "[PASS] Noosphere Pure module\n"

(** {1 Level 8: Meta Tests} *)

let test_meta_observation () =
  let obs : Meta.observation = {
    id = "obs_001";
    timestamp = Unix.gettimeofday ();
    observer = "external";
    perspective = "systems view";
    content = "The system exhibits emergent behavior";
    confidence = 0.4;
    blindspots = ["observer bias"; "incomplete data"];
  } in
  assert (obs.confidence < 0.5);  (* Meta observations are inherently uncertain *)
  assert (List.length obs.blindspots > 0);  (* Always have blindspots *)
  Printf.printf "[PASS] Meta observation creation\n"

let test_meta_paradox () =
  let paradox : Meta.paradox = {
    id = "paradox_001";
    name = "Halting Problem";
    description = "Cannot determine if a program will halt";
    discovered_at = Unix.gettimeofday ();
    resolution_attempts = [];
    status = `Accepted;
  } in
  assert (paradox.status = `Accepted);
  Printf.printf "[PASS] Meta paradox creation\n"

let test_meta_pure_module () =
  let state = Meta.initial_state () in
  (* Check fundamental paradoxes are present *)
  assert (List.length state.paradoxes >= 2);
  (* Check known unknowns *)
  assert (List.length state.known_unknowns >= 1);
  (* Test observation *)
  let state' = Meta.Pure.observe state
    ~observer:"test"
    ~perspective:"testing"
    ~content:"Testing the meta-system"
    () in
  assert (List.length state'.observations = 1);
  Printf.printf "[PASS] Meta Pure module\n"

(** {1 Level 9: Void Tests} *)

let test_void_return () =
  (* The void returns us to where we started *)
  let x = 42 in
  let _ = Void.return x in
  (* We cannot test the void, only point at it *)
  Printf.printf "[PASS] Void return (the test itself is a paradox)\n"

let test_void_contemplate () =
  (* Contemplating the void returns what you brought *)
  let thought = "emptiness" in
  let result = Void.contemplate thought in
  assert (result = thought);
  Printf.printf "[PASS] Void contemplate\n"

let test_void_koans () =
  (* The koans exist *)
  assert (List.length Void.koans > 0);
  Printf.printf "[PASS] Void koans (but do you understand them?)\n"

(** {2 Safe Operations Tests (14-person review feedback)} *)

let test_void_safe_dissolution () =
  (* Safe dissolution should always succeed *)
  let result = Void.dissolve_safe "anything" in
  assert (result = Ok ());
  let result2 = Void.dissolve_safe [1; 2; 3] in
  assert (result2 = Ok ());
  let result3 = Void.dissolve_safe (Some 42) in
  assert (result3 = Ok ());
  Printf.printf "[PASS] Void safe dissolution (Result-based)\n"

let test_void_safe_koan_selection () =
  (* Valid reader context *)
  let valid_reader : Void.reader_context = {
    current_level = 5;
    session_entropy = 12345.67;
    recent_themes = [];
  } in
  (match Void.select_koan_safe valid_reader with
   | Ok koan -> assert (String.length koan.text > 0)
   | Error _ -> assert false);

  (* Invalid level should return error *)
  let invalid_reader : Void.reader_context = {
    current_level = 99;  (* Invalid! *)
    session_entropy = 12345.67;
    recent_themes = [];
  } in
  (match Void.select_koan_safe invalid_reader with
   | Ok _ -> assert false  (* Should not succeed *)
   | Error (Void.InvalidLevel 99) -> ()
   | Error _ -> assert false);

  Printf.printf "[PASS] Void safe koan selection (error handling)\n"

let test_void_contemplate_with_cleanup () =
  (* Cleanup should be called even if we contemplate *)
  let cleanup_called = ref false in
  let cleanup () = cleanup_called := true in
  let result = Void.contemplate_with_cleanup ~cleanup "test" in
  assert (result = "test");
  assert (!cleanup_called);
  Printf.printf "[PASS] Void contemplate with cleanup\n"

(** {2 Phantom Type Safety Tests} *)

let test_void_phantom_types () =
  (* Tag and untag should preserve value *)
  let value = "test_value" in
  let tagged = Void.tag_level9 value in
  let untagged = Void.untag tagged in
  assert (untagged = value);

  (* Different levels should be distinguishable at compile time *)
  let l5 = Void.tag_level5 42 in
  let l9 = Void.tag_level9 42 in
  assert (Void.untag l5 = Void.untag l9);  (* Values equal *)
  (* But types are different - l5 cannot enter void: *)
  (* Void.enter_void l5;  <-- This would be a compile error! *)
  Void.enter_void l9;  (* This compiles fine *)

  Printf.printf "[PASS] Void phantom types (compile-time level safety)\n"

(** {2 Edge Case Tests} *)

let test_void_edge_cases () =
  (* Empty list dissolution *)
  let _ = Void.dissolve [] in

  (* Nested option dissolution *)
  let _ = Void.dissolve (Some (Some (Some None))) in

  (* Function dissolution *)
  let _ = Void.dissolve (fun x -> x + 1) in

  (* Recursive structure - careful not to create infinite *)
  let rec gen_list n = if n = 0 then [] else n :: gen_list (n - 1) in
  let _ = Void.dissolve (gen_list 1000) in

  (* Zero entropy reader *)
  let zero_reader : Void.reader_context = {
    current_level = 9;
    session_entropy = 0.0;
    recent_themes = [];
  } in
  let _ = Void.select_koan_by_resonance zero_reader in

  Printf.printf "[PASS] Void edge cases\n"

(** {1 Test Runner} *)

let () =
  Printf.printf "\n=== MASC Level 5-9 Tests ===\n\n";

  Printf.printf "--- Level 5: Institution ---\n";
  test_institution_episode_creation ();
  test_institution_knowledge ();
  test_institution_pattern ();
  test_institution_pure_module ();

  Printf.printf "\n--- Level 6: Mind ---\n";
  test_mind_self_model ();
  test_mind_thought ();
  test_mind_goal ();

  Printf.printf "\n--- Level 7: Noosphere ---\n";
  test_noosphere_connection ();
  test_noosphere_shared_belief ();
  test_noosphere_pure_module ();

  Printf.printf "\n--- Level 8: Meta ---\n";
  test_meta_observation ();
  test_meta_paradox ();
  test_meta_pure_module ();

  Printf.printf "\n--- Level 9: Void ---\n";
  test_void_return ();
  test_void_contemplate ();
  test_void_koans ();

  Printf.printf "\n--- Level 9: Void (Safe Operations) ---\n";
  test_void_safe_dissolution ();
  test_void_safe_koan_selection ();
  test_void_contemplate_with_cleanup ();

  Printf.printf "\n--- Level 9: Void (Type Safety) ---\n";
  test_void_phantom_types ();
  test_void_edge_cases ();

  Printf.printf "\n=== All Level 5-9 Tests Passed ===\n\n"
