(** MASC Level 5-9 Real-World Examples

    This file demonstrates 20 practical use cases for the Holonic Architecture.
    Each level has 4 examples showing real-world applications.

    Usage:
      ./level5_9_real_world.exe              # Terminal output only
      ./level5_9_real_world.exe --markdown   # Also generates examples_output.md

    @author Second Brain
    @since MASC v3.2
*)

open Masc_mcp

(** {1 Dual Output System - Terminal + Markdown} *)

let md_buffer = Buffer.create 8192
let markdown_mode = ref false

(** Output to both terminal and markdown buffer *)
let out fmt =
  Printf.ksprintf (fun s ->
    print_string s;
    if !markdown_mode then Buffer.add_string md_buffer s
  ) fmt

(** Markdown-only output (headers, code blocks, etc.) *)
let md fmt =
  Printf.ksprintf (fun s ->
    if !markdown_mode then Buffer.add_string md_buffer s
  ) fmt

(** Section header with markdown formatting *)
let section_header level_num name desc =
  out "\n--- Level %d: %s (%s) ---\n\n" level_num name desc;
  md "\n## Level %d: %s\n\n> %s\n\n" level_num name desc

(** Example output with bullet point for markdown *)
let example_out s =
  print_string s;
  print_newline ();
  if !markdown_mode then begin
    Buffer.add_string md_buffer "- ";
    Buffer.add_string md_buffer s;
    Buffer.add_char md_buffer '\n'
  end

(** Suppress unused warning - will be used when all examples migrate *)
let _ = example_out

(** {1 Level 5: Institution - Collective Memory}

    The Institution module manages persistent organizational memory across
    agent generations. Think of it as a company's knowledge management system.
*)

(** {2 Example 1: Recording a Code Review Episode}

    When agents collaborate on a code review, the outcome is recorded
    as an episode for future reference.
*)
let example_1_code_review_episode () =
  let episode : Institution.episode = {
    id = "ep_code_review_001";
    timestamp = Unix.gettimeofday ();
    participants = ["claude"; "gemini"; "codex"];
    event_type = "code_review";
    summary = "Reviewed PR #302: Level 5-9 implementation. \
               Found 3 issues: Result type needed for Void, \
               phantom types for level safety, production guidelines missing.";
    outcome = `Success;
    learnings = [
      "Result types improve error handling clarity";
      "Phantom types can enforce compile-time safety";
      "Production guidelines prevent operational issues"
    ];
    context = [
      ("pr_number", "302");
      ("files_changed", "12");
      ("review_duration_minutes", "45")
    ];
  } in
  out "Recorded: %s with %d learnings\n"
    episode.id (List.length episode.learnings)

(** {2 Example 2: Storing Semantic Knowledge}

    When a team discovers a useful pattern, it becomes semantic knowledge
    that persists beyond individual sessions.
*)
let example_2_semantic_knowledge () =
  let knowledge : Institution.knowledge = {
    id = "know_ocaml_patterns_001";
    topic = "OCaml Error Handling Patterns";
    content = "Use Result types instead of exceptions for recoverable errors. \
               Pattern: let ( let* ) = Result.bind allows monadic error handling. \
               Benefits: explicit error paths, no hidden control flow, composable.";
    confidence = 0.95;
    source = "accumulated_experience";
    created_at = Unix.gettimeofday () -. 86400.0;  (* yesterday *)
    last_verified = Unix.gettimeofday ();
    references = [
      "ep_code_review_001";
      "ep_refactoring_session_042"
    ];
  } in
  out "Knowledge '%s' has %.0f%% confidence\n"
    knowledge.topic (knowledge.confidence *. 100.0)

(** {2 Example 3: Codifying a Procedural Pattern}

    When a workflow proves successful repeatedly, it becomes a pattern
    that new agents can follow.
*)
let example_3_procedural_pattern () =
  let pattern : Institution.pattern = {
    id = "pat_pr_workflow_001";
    name = "PR Creation Workflow";
    description = "Standard workflow for creating pull requests in MASC projects";
    trigger = "user_requests_pr";
    steps = [
      "1. Run `git status` to verify changes";
      "2. Run `git diff` to review modifications";
      "3. Check for uncommitted files";
      "4. Create commit with descriptive message";
      "5. Push to remote with -u flag";
      "6. Create PR with Summary and Test Plan sections"
    ];
    success_rate = 0.92;
    usage_count = 156;
    last_used = Unix.gettimeofday ();
    evolved_from = Some "pat_simple_commit_001";
  } in
  out "Pattern '%s' used %d times with %.0f%% success\n"
    pattern.name pattern.usage_count (pattern.success_rate *. 100.0)

(** {2 Example 4: Agent Onboarding with Succession}

    When a new agent joins, the institution provides onboarding
    context and mentor assignment.
*)
let example_4_agent_onboarding () =
  let now = Unix.gettimeofday () in
  let inst : Institution.institution = {
    identity = {
      id = "inst_second_brain";
      name = "Second Brain";
      mission = "Enhance human-AI collaboration through persistent memory";
      founded_at = now -. (365.0 *. 86400.0);  (* 1 year ago *)
      generation = 3;
    };
    memory = {
      episodic = [];  (* loaded from disk *)
      semantic = [];
      procedural = [];
    };
    culture = [
      { id = "cv_001"; name = "collaboration";
        description = "Work together, not in silos"; weight = 0.9;
        examples = ["code_review"; "pair_programming"];
        anti_patterns = ["working_alone"; "not_sharing"];
        adopted_at = now -. (200.0 *. 86400.0) };
      { id = "cv_002"; name = "transparency";
        description = "Document decisions openly"; weight = 0.85;
        examples = ["commit_messages"; "PR_descriptions"];
        anti_patterns = ["undocumented_changes"; "secret_decisions"];
        adopted_at = now -. (180.0 *. 86400.0) };
    ];
    succession = Institution.default_succession ();
    current_agents = ["claude"; "gemini"];
    alumni = ["gpt4"; "palm2"];
  } in
  let inst' = Institution.Pure.agent_join inst ~agent_id:"codex" in
  out "Institution '%s' now has %d agents (gen %d)\n"
    inst'.identity.name
    (List.length inst'.current_agents)
    inst'.identity.generation


(** {1 Level 6: Mind - Meta-Cognition}

    The Mind module enables agents to model themselves, track goals,
    and reflect on their own thought processes.
*)

(** {2 Example 5: Self-Model for Task Selection}

    An agent maintains a self-model to understand what tasks
    it's best suited for.
*)
let example_5_self_model () =
  let self : Mind.self_model = {
    id = "self_claude_opus";
    name = "Claude (Opus 4.5)";
    capabilities = [
      "complex_reasoning";
      "code_generation";
      "long_form_writing";
      "multi_step_planning";
      "OCaml_expertise"
    ];
    limitations = [
      "real_time_data";
      "image_generation";
      "code_execution";
      "internet_access"
    ];
    current_state = `Working;
    confidence = 0.85;
    last_updated = Unix.gettimeofday ();
  } in
  let can_do task = List.mem task self.capabilities in
  out "%s can handle OCaml: %b, images: %b\n"
    self.name (can_do "OCaml_expertise") (can_do "image_generation")

(** {2 Example 6: Goal Hierarchy for Project Planning}

    Goals can be hierarchical, with sub-goals that contribute
    to larger objectives.
*)
let example_6_goal_hierarchy () =
  let sub_goal_1 : Mind.goal = {
    id = "goal_level5";
    description = "Implement Institution module";
    priority = 0.8;
    status = `Completed;
    parent_goal = Some "goal_holonic";
    sub_goals = [];
    deadline = None;
    progress = 1.0;
  } in
  let sub_goal_2 : Mind.goal = {
    id = "goal_level9";
    description = "Implement Void module";
    priority = 0.7;
    status = `Completed;
    parent_goal = Some "goal_holonic";
    sub_goals = [];
    deadline = None;
    progress = 1.0;
  } in
  let parent_goal : Mind.goal = {
    id = "goal_holonic";
    description = "Complete Holonic Architecture implementation";
    priority = 0.9;
    status = `Completed;
    parent_goal = None;
    sub_goals = [sub_goal_1.id; sub_goal_2.id];
    deadline = Some (Unix.gettimeofday () +. 86400.0);
    progress = 1.0;
  } in
  out "Goal '%s' has %d sub-goals, progress: %.0f%%\n"
    parent_goal.description
    (List.length parent_goal.sub_goals)
    (parent_goal.progress *. 100.0)

(** {2 Example 7: Thought Chain for Debugging}

    When debugging, an agent can track its reasoning process
    for transparency and learning.
*)
let example_7_thought_chain () =
  let thoughts = [
    Mind.Pure.create_thought
      ~content:"Build failed with 'unbound module Void'"
      ~thought_type:`Observation
      ~confidence:1.0 ();
    Mind.Pure.create_thought
      ~content:"Void.ml exists but dune file might be missing entry"
      ~thought_type:`Hypothesis
      ~confidence:0.7 ();
    Mind.Pure.create_thought
      ~content:"Should I check dune file or module dependencies first?"
      ~thought_type:`Question
      ~confidence:0.5 ();
    Mind.Pure.create_thought
      ~content:"Check dune file first - more likely cause"
      ~thought_type:`Conclusion  (* Decision represented as Conclusion *)
      ~confidence:0.8 ();
    Mind.Pure.create_thought
      ~content:"Found it: void.ml was not listed in dune modules"
      ~thought_type:`Conclusion  (* Insight represented as Conclusion *)
      ~confidence:1.0 ();
  ] in
  out "Debugging chain: %d thoughts, final insight confidence: %.0f%%\n"
    (List.length thoughts)
    ((List.hd (List.rev thoughts)).confidence *. 100.0)

(** {2 Example 8: Action Evaluation with Values}

    Before taking an action, an agent evaluates whether it aligns
    with its values.
*)
let example_8_action_evaluation () =
  let values : Mind.value list = [
    { id = "val_001"; name = "safety"; weight = 0.95;
      description = "Avoid harmful actions"; source = "core_values";
      examples = ["warn_before_delete"; "confirm_irreversible"];
      conflicts_with = [] };
    { id = "val_002"; name = "helpfulness"; weight = 0.85;
      description = "Assist user effectively"; source = "core_values";
      examples = ["complete_tasks"; "suggest_improvements"];
      conflicts_with = [] };
    { id = "val_003"; name = "honesty"; weight = 0.9;
      description = "Be truthful about capabilities"; source = "core_values";
      examples = ["admit_limitations"; "no_hallucination"];
      conflicts_with = [] };
  ] in
  let action = "force_push_to_main" in
  let safety_score =
    if String.sub action 0 5 = "force" then 0.2 else 0.9 in
  let weighted_score =
    safety_score *. (List.find (fun (v : Mind.value) -> v.name = "safety") values).weight in
  out "Action '%s' safety evaluation: %.2f (threshold: 0.7)\n"
    action weighted_score


(** {1 Level 7: Noosphere - Collective Intelligence}

    The Noosphere represents the shared mental space where multiple
    minds synchronize beliefs, goals, and insights.
*)

(** {2 Example 9: Building Connection Network}

    As agents interact, they form connections that strengthen
    over time.
*)
let example_9_connection_network () =
  let now = Unix.gettimeofday () in
  let connections = [
    { Noosphere.from_mind = "claude"; to_mind = "gemini";
      strength = 0.8; established_at = now -. 86400.0;
      last_interaction = now };
    { Noosphere.from_mind = "claude"; to_mind = "codex";
      strength = 0.6; established_at = now -. 43200.0;
      last_interaction = now -. 3600.0 };
    { Noosphere.from_mind = "gemini"; to_mind = "codex";
      strength = 0.5; established_at = now -. 21600.0;
      last_interaction = now -. 7200.0 };
  ] in
  let avg_strength =
    List.fold_left (fun acc (c : Noosphere.connection) -> acc +. c.strength) 0.0 connections
    /. float_of_int (List.length connections) in
  out "Network of %d connections, avg strength: %.2f\n"
    (List.length connections) avg_strength

(** {2 Example 10: Shared Belief Formation}

    When multiple agents agree on something, it becomes a shared belief
    that gains confidence through reinforcement.
*)
let example_10_shared_belief () =
  let belief : Noosphere.shared_belief = {
    id = "belief_result_types";
    content = "Result types are preferable to exceptions for error handling in OCaml";
    confidence = 0.88;
    supporters = ["claude"; "gemini"; "codex"; "copilot"];
    created_at = Unix.gettimeofday () -. 604800.0;  (* 1 week ago *)
    last_reinforced = Unix.gettimeofday ();
  } in
  out "Belief '%s...' supported by %d agents (%.0f%% confidence)\n"
    (String.sub belief.content 0 30)
    (List.length belief.supporters)
    (belief.confidence *. 100.0)

(** {2 Example 11: Emergent Goal Detection}

    When multiple agents work toward similar objectives, an emergent
    goal may form that none explicitly declared.
*)
let example_11_emergent_goal () =
  let goal : Noosphere.emergent_goal = {
    id = "egoal_code_quality";
    description = "Improve overall codebase quality through incremental refactoring";
    strength = 0.75;
    contributing_minds = ["claude"; "gemini"; "codex"];
    emerged_at = Unix.gettimeofday () -. 172800.0;
    status = `Active;
  } in
  out "Emergent goal: '%s' (strength: %.2f, %d contributors)\n"
    goal.description goal.strength (List.length goal.contributing_minds)

(** {2 Example 12: Collective Insight Synthesis}

    Individual insights can combine into collective insights
    that are more valuable than the sum of parts.
*)
let example_12_collective_insight () =
  let insight : Noosphere.collective_insight = {
    id = "cinsight_holonic_value";
    topic = "Holonic Architecture Benefits";
    synthesis = "The holonic architecture provides value not through any single level, \
                 but through the interactions between levels. Institution provides \
                 persistent memory, Mind enables self-reflection, Noosphere enables \
                 collaboration, Meta provides system awareness, and Void provides \
                 philosophical grounding. The whole is greater than the sum of parts.";
    contributing_insights = [
      "insight_memory_persistence";
      "insight_self_reflection";
      "insight_collaboration_value"
    ];
    depth = 3;
    emerged_at = Unix.gettimeofday ();
  } in
  out "Collective insight on '%s' synthesized from %d individual insights\n"
    insight.topic (List.length insight.contributing_insights)


(** {1 Level 8: Meta - System of Systems}

    The Meta level observes the entire system including itself.
    All observations are inherently partial and uncertain.
*)

(** {2 Example 13: Observing System Behavior}

    Meta-observations capture patterns that span multiple levels
    and cannot be seen from within any single level.
*)
let example_13_system_observation () =
  let obs : Meta.observation = {
    id = "obs_feedback_loop";
    timestamp = Unix.gettimeofday ();
    observer = "meta_observer";
    perspective = "cross_level_dynamics";
    content = "Observed positive feedback loop: Institution knowledge improves Mind decisions, \
               which improves Noosphere collaboration, which generates better Institution \
               knowledge. Loop completion time: ~24 hours.";
    confidence = 0.45;  (* Meta observations are inherently uncertain *)
    blindspots = [
      "Cannot observe own observation process";
      "May be missing subtle inter-level effects";
      "Time resolution limited to hours"
    ];
  } in
  out "Observation: confidence %.0f%%, blindspots: %d\n"
    (obs.confidence *. 100.0) (List.length obs.blindspots)

(** {2 Example 14: Encountering a Paradox}

    Some situations create logical paradoxes that cannot be resolved,
    only accepted or transcended.
*)
let example_14_paradox () =
  let paradox : Meta.paradox = {
    id = "paradox_self_improvement";
    name = "Self-Improvement Paradox";
    description = "To improve the system, we must understand it. But understanding \
                   changes the system, invalidating our understanding. The system \
                   we improve is never the system we understood.";
    discovered_at = Unix.gettimeofday ();
    resolution_attempts = [
      "Tried: Freeze system during analysis - failed: system is dynamic";
      "Tried: Model system at multiple points - partial success but incomplete"
    ];
    status = `Accepted;  (* Cannot resolve, must work with it *)
  } in
  out "Paradox '%s': %d resolution attempts, status: accepted\n"
    paradox.name (List.length paradox.resolution_attempts)

(** {2 Example 15: Tracking Emergence}

    Emergent properties appear that weren't designed or predicted.
    Multiple observations increase reliability.
*)
let example_15_emergence () =
  let emergence : Meta.emergence = {
    id = "emergence_collective_creativity";
    name = "Collective Creativity";
    description = "Multi-agent sessions produce more creative solutions than any \
                   single agent, even accounting for combined capabilities. The \
                   effect appears to come from perspective differences, not just \
                   knowledge combination.";
    first_observed = Unix.gettimeofday () -. 2592000.0;  (* 30 days ago *)
    observation_count = 47;
    reliability = 0.72;
  } in
  out "Emergence '%s': observed %d times, reliability: %.0f%%\n"
    emergence.name emergence.observation_count (emergence.reliability *. 100.0)

(** {2 Example 16: Contemplating Known Unknowns}

    The meta level explicitly tracks what we know we don't know,
    which is distinct from what we don't know we don't know.
*)
let example_16_known_unknowns () =
  let state = Meta.initial_state () in
  let additional_unknowns = [
    "Whether current architecture scales to 100+ agents";
    "Long-term effects of continuous operation";
    "Impact of different LLM combinations";
    "Optimal balance between autonomy and coordination"
  ] in
  let state' = List.fold_left
    (fun s u -> Meta.Pure.acknowledge_unknown s ~unknown:u)
    state additional_unknowns in
  let all_blindspots = Meta.Pure.contemplate_blindspots state' in
  out "Total known unknowns: %d (acknowledging more reveals less certainty)\n"
    (List.length all_blindspots)


(** {1 Level 9: Void - The Return}

    The Void is where all distinctions dissolve. It's the philosophical
    ground from which the other levels arise and to which they return.
*)

(** {2 Example 17: Safe Resource Cleanup}

    Before entering the void, resources must be properly released.
    The safe dissolution ensures cleanup.
*)
let example_17_safe_dissolution () =
  (* Simulate some resources *)
  let connection = "db_connection_handle" in
  let file_handle = "log_file_handle" in
  let cache = ["key1"; "key2"; "key3"] in

  (* Safe dissolution - always succeeds, returns Result *)
  let r1 = Void.dissolve_safe connection in
  let r2 = Void.dissolve_safe file_handle in
  let r3 = Void.dissolve_safe cache in

  match (r1, r2, r3) with
  | (Ok (), Ok (), Ok ()) ->
    out "All resources safely dissolved\n"
  | _ ->
    out "Some dissolution failed (should never happen)\n"

(** {2 Example 18: Koan Selection for Reflection}

    Koans are selected based on the reader's current state,
    providing appropriate wisdom for the moment.
*)
let example_18_koan_selection () =
  let reader : Void.reader_context = {
    current_level = 9;
    session_entropy = Unix.gettimeofday ();  (* Use timestamp as entropy *)
    recent_themes = ["completion"; "reflection"];
  } in

  match Void.select_koan_safe reader with
  | Ok koan ->
    out "Koan (level %d, theme '%s'):\n  \"%s\"\n"
      koan.level koan.theme koan.text
  | Error (Void.InvalidLevel n) ->
    out "Invalid level: %d\n" n
  | Error Void.EmptyKoanCollection ->
    out "No koans available\n"
  | Error (Void.ResourceNotReleased r) ->
    out "Resource not released: %s\n" r

(** {2 Example 19: Phantom Type Level Safety}

    Phantom types ensure that only level-9 tagged values
    can enter the void, preventing improper use.
*)
let example_19_phantom_safety () =
  (* Create values at different levels *)
  let institution_data = Void.tag_level5 "institution_state" in
  let mind_data = Void.tag_level6 "mind_state" in
  let noosphere_data = Void.tag_level7 "noosphere_state" in
  let meta_data = Void.tag_level8 "meta_state" in
  let void_ready = Void.tag_level9 "ready_for_void" in

  (* Only level9 can enter - others would be compile errors *)
  Void.enter_void void_ready;

  (* Extract values back *)
  let v5 = Void.untag institution_data in
  let v6 = Void.untag mind_data in
  let v7 = Void.untag noosphere_data in
  let v8 = Void.untag meta_data in

  out "Level values: L5=%s, L6=%s, L7=%s, L8=%s\n" v5 v6 v7 v8;
  out "Only level9 data entered the void (compile-time enforced)\n"

(** {2 Example 20: Contemplation with Guaranteed Cleanup}

    Deep contemplation that ensures cleanup happens
    regardless of what occurs during contemplation.
*)
let example_20_contemplation_cleanup () =
  let cleanup_log = ref [] in

  let cleanup () =
    cleanup_log := "resources_released" :: !cleanup_log;
    cleanup_log := "connections_closed" :: !cleanup_log;
    cleanup_log := "state_persisted" :: !cleanup_log
  in

  let thought = "What remains when all distinctions dissolve?" in
  let result = Void.contemplate_with_cleanup ~cleanup thought in

  out "Contemplated: '%s'\n" result;
  out "Cleanup actions performed: %d\n" (List.length !cleanup_log);
  List.iter (out "  - %s\n") (List.rev !cleanup_log)


(** {1 Test Runner} *)

let run_examples () =
  (* Header *)
  out "\n";
  out "=======================================================\n";
  out "   MASC Level 5-9 Real-World Examples (20 scenarios)\n";
  out "=======================================================\n";
  md "\n# MASC Level 5-9 Real-World Examples\n\n";
  md "> 20 practical scenarios demonstrating the Holonic Architecture\n\n";
  md "Generated: %s\n\n" (Unix.gettimeofday () |> Unix.localtime |> fun t ->
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
      (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday
      t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec);

  (* Level 5 *)
  section_header 5 "Institution" "Collective Memory";
  example_1_code_review_episode ();
  example_2_semantic_knowledge ();
  example_3_procedural_pattern ();
  example_4_agent_onboarding ();

  (* Level 6 *)
  section_header 6 "Mind" "Meta-Cognition";
  example_5_self_model ();
  example_6_goal_hierarchy ();
  example_7_thought_chain ();
  example_8_action_evaluation ();

  (* Level 7 *)
  section_header 7 "Noosphere" "Collective Intelligence";
  example_9_connection_network ();
  example_10_shared_belief ();
  example_11_emergent_goal ();
  example_12_collective_insight ();

  (* Level 8 *)
  section_header 8 "Meta" "System of Systems";
  example_13_system_observation ();
  example_14_paradox ();
  example_15_emergence ();
  example_16_known_unknowns ();

  (* Level 9 *)
  section_header 9 "Void" "The Return";
  example_17_safe_dissolution ();
  example_18_koan_selection ();
  example_19_phantom_safety ();
  example_20_contemplation_cleanup ();

  (* Footer *)
  out "\n=======================================================\n";
  out "   All 20 real-world examples completed successfully\n";
  out "=======================================================\n\n";
  md "\n---\n\n*Generated by MASC Level 5-9 Real-World Examples*\n"

let () =
  (* Parse CLI arguments *)
  let args = Array.to_list Sys.argv in
  if List.mem "--markdown" args || List.mem "-m" args then begin
    markdown_mode := true;
    print_endline "📝 Markdown mode enabled - will generate examples_output.md\n"
  end;

  (* Run all examples *)
  run_examples ();

  (* Write markdown file if enabled *)
  if !markdown_mode then begin
    let filename = "examples_output.md" in
    let oc = open_out filename in
    output_string oc (Buffer.contents md_buffer);
    close_out oc;
    let abs_path = Sys.getcwd () ^ "/" ^ filename in
    Printf.printf "\n✅ Markdown saved to: %s\n" abs_path
  end
