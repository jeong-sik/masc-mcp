(** Noosphere Eio Module Coverage Tests

    Tests for collective intelligence types and utilities:
    - connection type
    - shared_belief type
    - emergent_goal type
    - collective_insight type
    - global_state type
    - noosphere type
    - generate_id function
    - *_to_json serialization functions
    - *_of_json deserialization functions
    - get_* helper functions
*)

open Alcotest

module Noosphere_eio = Masc_mcp.Noosphere_eio

(* ============================================================
   generate_id Tests
   ============================================================ *)

let test_generate_id_prefix () =
  let id = Noosphere_eio.generate_id "conn" in
  check bool "starts with prefix" true (String.sub id 0 4 = "conn")

let test_generate_id_underscore () =
  let id = Noosphere_eio.generate_id "belief" in
  check bool "has underscore after prefix" true (String.get id 6 = '_')

let test_generate_id_length () =
  let id = Noosphere_eio.generate_id "x" in
  (* Format: prefix_XXXXXXXX (8 chars of UUID) = 1 + 1 + 8 = 10 for "x" *)
  check bool "reasonable length" true (String.length id >= 10)

let test_generate_id_unique () =
  let id1 = Noosphere_eio.generate_id "test" in
  let id2 = Noosphere_eio.generate_id "test" in
  check bool "different ids" true (id1 <> id2)

(* ============================================================
   connection Type Tests
   ============================================================ *)

let test_connection_type () =
  let c : Noosphere_eio.connection = {
    from_mind = "mind-a";
    to_mind = "mind-b";
    strength = 0.8;
    established_at = 1704067200.0;
    last_interaction = 1704067300.0;
  } in
  check string "from_mind" "mind-a" c.from_mind;
  check string "to_mind" "mind-b" c.to_mind

let test_connection_to_json () =
  let c : Noosphere_eio.connection = {
    from_mind = "mind-1";
    to_mind = "mind-2";
    strength = 0.5;
    established_at = 1000.0;
    last_interaction = 2000.0;
  } in
  let json = Noosphere_eio.connection_to_json c in
  match json with
  | `Assoc fields ->
      check bool "has from_mind" true (List.mem_assoc "from_mind" fields);
      check bool "has to_mind" true (List.mem_assoc "to_mind" fields);
      check bool "has strength" true (List.mem_assoc "strength" fields)
  | _ -> fail "expected Assoc"

let test_connection_of_json () =
  let json = `Assoc [
    ("from_mind", `String "a");
    ("to_mind", `String "b");
    ("strength", `Float 0.7);
    ("established_at", `Float 100.0);
    ("last_interaction", `Float 200.0);
  ] in
  let c = Noosphere_eio.connection_of_json json in
  check string "from_mind" "a" c.from_mind;
  check string "to_mind" "b" c.to_mind

(* ============================================================
   shared_belief Type Tests
   ============================================================ *)

let test_shared_belief_type () =
  let b : Noosphere_eio.shared_belief = {
    id = "belief-1";
    content = "AI is helpful";
    confidence = 0.9;
    supporters = ["mind-a"; "mind-b"];
    created_at = 1000.0;
    last_reinforced = 2000.0;
  } in
  check string "id" "belief-1" b.id;
  check int "supporters count" 2 (List.length b.supporters)

let test_shared_belief_to_json () =
  let b : Noosphere_eio.shared_belief = {
    id = "b1";
    content = "test";
    confidence = 0.5;
    supporters = ["a"];
    created_at = 1.0;
    last_reinforced = 2.0;
  } in
  let json = Noosphere_eio.shared_belief_to_json b in
  match json with
  | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      check bool "has content" true (List.mem_assoc "content" fields)
  | _ -> fail "expected Assoc"

let test_shared_belief_of_json () =
  let json = `Assoc [
    ("id", `String "b2");
    ("content", `String "belief content");
    ("confidence", `Float 0.8);
    ("supporters", `List [`String "x"; `String "y"]);
    ("created_at", `Float 10.0);
    ("last_reinforced", `Float 20.0);
  ] in
  let b = Noosphere_eio.shared_belief_of_json json in
  check string "id" "b2" b.id;
  check int "supporters" 2 (List.length b.supporters)

(* ============================================================
   emergent_goal Type Tests
   ============================================================ *)

let test_emergent_goal_emerging () =
  let g : Noosphere_eio.emergent_goal = {
    id = "goal-1";
    description = "Learn OCaml";
    emerged_at = 1000.0;
    contributing_minds = ["m1"];
    strength = 0.3;
    status = `Emerging;
  } in
  match g.status with
  | `Emerging -> check bool "ok" true true
  | _ -> fail "expected Emerging"

let test_emergent_goal_active () =
  let g : Noosphere_eio.emergent_goal = {
    id = "g2";
    description = "Build system";
    emerged_at = 0.0;
    contributing_minds = [];
    strength = 0.8;
    status = `Active;
  } in
  match g.status with
  | `Active -> check bool "ok" true true
  | _ -> fail "expected Active"

let test_emergent_goal_fading () =
  let g : Noosphere_eio.emergent_goal = {
    id = "g3";
    description = "x";
    emerged_at = 0.0;
    contributing_minds = [];
    strength = 0.1;
    status = `Fading;
  } in
  match g.status with
  | `Fading -> check bool "ok" true true
  | _ -> fail "expected Fading"

let test_emergent_goal_dissolved () =
  let g : Noosphere_eio.emergent_goal = {
    id = "g4";
    description = "y";
    emerged_at = 0.0;
    contributing_minds = [];
    strength = 0.0;
    status = `Dissolved;
  } in
  match g.status with
  | `Dissolved -> check bool "ok" true true
  | _ -> fail "expected Dissolved"

let test_emergent_goal_to_json () =
  let g : Noosphere_eio.emergent_goal = {
    id = "g5";
    description = "test goal";
    emerged_at = 100.0;
    contributing_minds = ["a"; "b"];
    strength = 0.6;
    status = `Active;
  } in
  let json = Noosphere_eio.emergent_goal_to_json g in
  match json with
  | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      check bool "has status" true (List.mem_assoc "status" fields)
  | _ -> fail "expected Assoc"

let test_emergent_goal_of_json_emerging () =
  let json = `Assoc [
    ("id", `String "g1");
    ("description", `String "desc");
    ("emerged_at", `Float 0.0);
    ("contributing_minds", `List []);
    ("strength", `Float 0.5);
    ("status", `String "emerging");
  ] in
  let g = Noosphere_eio.emergent_goal_of_json json in
  check string "id" "g1" g.id;
  match g.status with
  | `Emerging -> check bool "ok" true true
  | _ -> fail "expected Emerging"

let test_emergent_goal_of_json_active () =
  let json = `Assoc [
    ("id", `String "g2");
    ("description", `String "x");
    ("emerged_at", `Float 0.0);
    ("contributing_minds", `List []);
    ("strength", `Float 1.0);
    ("status", `String "active");
  ] in
  let g = Noosphere_eio.emergent_goal_of_json json in
  match g.status with
  | `Active -> check bool "ok" true true
  | _ -> fail "expected Active"

let test_emergent_goal_of_json_fading () =
  let json = `Assoc [
    ("id", `String "g3");
    ("description", `String "y");
    ("emerged_at", `Float 0.0);
    ("contributing_minds", `List []);
    ("strength", `Float 0.0);
    ("status", `String "fading");
  ] in
  let g = Noosphere_eio.emergent_goal_of_json json in
  match g.status with
  | `Fading -> check bool "ok" true true
  | _ -> fail "expected Fading"

let test_emergent_goal_of_json_dissolved () =
  let json = `Assoc [
    ("id", `String "g4");
    ("description", `String "z");
    ("emerged_at", `Float 0.0);
    ("contributing_minds", `List []);
    ("strength", `Float 0.0);
    ("status", `String "dissolved");
  ] in
  let g = Noosphere_eio.emergent_goal_of_json json in
  match g.status with
  | `Dissolved -> check bool "ok" true true
  | _ -> fail "expected Dissolved"

let test_emergent_goal_of_json_unknown () =
  let json = `Assoc [
    ("id", `String "g5");
    ("description", `String "w");
    ("emerged_at", `Float 0.0);
    ("contributing_minds", `List []);
    ("strength", `Float 0.0);
    ("status", `String "unknown_status");
  ] in
  let g = Noosphere_eio.emergent_goal_of_json json in
  (* Default to Emerging for unknown status *)
  check string "id" "g5" g.id

(* ============================================================
   collective_insight Type Tests
   ============================================================ *)

let test_collective_insight_type () =
  let i : Noosphere_eio.collective_insight = {
    id = "insight-1";
    topic = "AI ethics";
    synthesis = "We should be careful";
    contributing_insights = ["i1"; "i2"];
    emerged_at = 1000.0;
    depth = 3;
  } in
  check string "id" "insight-1" i.id;
  check int "depth" 3 i.depth

let test_collective_insight_to_json () =
  let i : Noosphere_eio.collective_insight = {
    id = "i1";
    topic = "t";
    synthesis = "s";
    contributing_insights = [];
    emerged_at = 0.0;
    depth = 1;
  } in
  let json = Noosphere_eio.collective_insight_to_json i in
  match json with
  | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      check bool "has depth" true (List.mem_assoc "depth" fields)
  | _ -> fail "expected Assoc"

let test_collective_insight_of_json () =
  let json = `Assoc [
    ("id", `String "ci1");
    ("topic", `String "topic1");
    ("synthesis", `String "synth");
    ("contributing_insights", `List [`String "x"]);
    ("emerged_at", `Float 50.0);
    ("depth", `Int 2);
  ] in
  let i = Noosphere_eio.collective_insight_of_json json in
  check string "id" "ci1" i.id;
  check int "depth" 2 i.depth

(* ============================================================
   global_state Type Tests
   ============================================================ *)

let test_global_state_type () =
  let s : Noosphere_eio.global_state = {
    coherence = 0.7;
    activity_level = 0.5;
    dominant_mood = "curious";
    focus_topic = Some "OCaml";
    last_sync = 1000.0;
  } in
  check string "mood" "curious" s.dominant_mood;
  match s.focus_topic with
  | Some t -> check string "focus" "OCaml" t
  | None -> fail "expected Some"

let test_global_state_no_focus () =
  let s : Noosphere_eio.global_state = {
    coherence = 0.0;
    activity_level = 0.0;
    dominant_mood = "idle";
    focus_topic = None;
    last_sync = 0.0;
  } in
  match s.focus_topic with
  | None -> check bool "ok" true true
  | Some _ -> fail "expected None"

let test_global_state_to_json () =
  let s : Noosphere_eio.global_state = {
    coherence = 0.5;
    activity_level = 0.3;
    dominant_mood = "test";
    focus_topic = None;
    last_sync = 100.0;
  } in
  let json = Noosphere_eio.global_state_to_json s in
  match json with
  | `Assoc fields ->
      check bool "has coherence" true (List.mem_assoc "coherence" fields)
  | _ -> fail "expected Assoc"

let test_global_state_of_json () =
  let json = `Assoc [
    ("coherence", `Float 0.8);
    ("activity_level", `Float 0.6);
    ("dominant_mood", `String "happy");
    ("focus_topic", `Null);
    ("last_sync", `Float 999.0);
  ] in
  let s = Noosphere_eio.global_state_of_json json in
  check string "mood" "happy" s.dominant_mood

(* ============================================================
   noosphere Type Tests
   ============================================================ *)

let test_noosphere_type () =
  let n : Noosphere_eio.noosphere = {
    id = "ns-1";
    name = "test noosphere";
    minds = ["m1"; "m2"];
    connections = [];
    beliefs = [];
    goals = [];
    insights = [];
    state = {
      coherence = 0.5;
      activity_level = 0.5;
      dominant_mood = "neutral";
      focus_topic = None;
      last_sync = 0.0;
    };
    created_at = 1000.0;
  } in
  check string "id" "ns-1" n.id;
  check int "minds" 2 (List.length n.minds)

let test_noosphere_to_json () =
  let n : Noosphere_eio.noosphere = {
    id = "ns-2";
    name = "json test";
    minds = [];
    connections = [];
    beliefs = [];
    goals = [];
    insights = [];
    state = {
      coherence = 0.0;
      activity_level = 0.0;
      dominant_mood = "";
      focus_topic = None;
      last_sync = 0.0;
    };
    created_at = 0.0;
  } in
  let json = Noosphere_eio.noosphere_to_json n in
  match json with
  | `Assoc fields ->
      check bool "has id" true (List.mem_assoc "id" fields);
      check bool "has name" true (List.mem_assoc "name" fields);
      check bool "has minds" true (List.mem_assoc "minds" fields)
  | _ -> fail "expected Assoc"

let test_noosphere_of_json () =
  let json = `Assoc [
    ("id", `String "ns-3");
    ("name", `String "parsed");
    ("minds", `List [`String "x"]);
    ("connections", `List []);
    ("beliefs", `List []);
    ("goals", `List []);
    ("insights", `List []);
    ("state", `Assoc [
      ("coherence", `Float 0.1);
      ("activity_level", `Float 0.2);
      ("dominant_mood", `String "m");
      ("focus_topic", `Null);
      ("last_sync", `Float 0.0);
    ]);
    ("created_at", `Float 500.0);
  ] in
  let n = Noosphere_eio.noosphere_of_json json in
  check string "id" "ns-3" n.id;
  check string "name" "parsed" n.name

(* ============================================================
   get_* Helper Tests
   ============================================================ *)

let test_get_string_present () =
  let json = `Assoc [("key", `String "value")] in
  check string "value" "value" (Noosphere_eio.get_string json "key")

let test_get_string_missing () =
  let json = `Assoc [] in
  check string "empty" "" (Noosphere_eio.get_string json "key")

let test_get_float_present () =
  let json = `Assoc [("num", `Float 3.14)] in
  let v = Noosphere_eio.get_float json "num" in
  check bool "close to 3.14" true (abs_float (v -. 3.14) < 0.01)

let test_get_float_from_int () =
  let json = `Assoc [("num", `Int 42)] in
  let v = Noosphere_eio.get_float json "num" in
  check bool "42.0" true (abs_float (v -. 42.0) < 0.01)

let test_get_float_missing () =
  let json = `Assoc [] in
  let v = Noosphere_eio.get_float json "x" in
  check bool "0.0" true (abs_float v < 0.01)

let test_get_int_present () =
  let json = `Assoc [("n", `Int 100)] in
  check int "100" 100 (Noosphere_eio.get_int json "n")

let test_get_int_missing () =
  let json = `Assoc [] in
  check int "0" 0 (Noosphere_eio.get_int json "n")

let test_get_string_list_present () =
  let json = `Assoc [("tags", `List [`String "a"; `String "b"])] in
  check int "2 items" 2 (List.length (Noosphere_eio.get_string_list json "tags"))

let test_get_string_list_empty () =
  let json = `Assoc [("tags", `List [])] in
  check int "0 items" 0 (List.length (Noosphere_eio.get_string_list json "tags"))

let test_get_string_list_missing () =
  let json = `Assoc [] in
  check int "0 items" 0 (List.length (Noosphere_eio.get_string_list json "x"))

let test_get_string_opt_some () =
  let json = `Assoc [("opt", `String "val")] in
  match Noosphere_eio.get_string_opt json "opt" with
  | Some s -> check string "val" "val" s
  | None -> fail "expected Some"

let test_get_string_opt_null () =
  let json = `Assoc [("opt", `Null)] in
  match Noosphere_eio.get_string_opt json "opt" with
  | None -> check bool "ok" true true
  | Some _ -> fail "expected None"

let test_get_string_opt_missing () =
  let json = `Assoc [] in
  match Noosphere_eio.get_string_opt json "opt" with
  | None -> check bool "ok" true true
  | Some _ -> fail "expected None"

(* ============================================================
   Pure Module Tests
   ============================================================ *)

let make_test_noosphere () : Noosphere_eio.noosphere = {
  id = "ns-test";
  name = "Test Noosphere";
  minds = ["mind-a"; "mind-b"];
  connections = [{
    from_mind = "mind-a";
    to_mind = "mind-b";
    strength = 0.5;
    established_at = 1000.0;
    last_interaction = 2000.0;
  }];
  beliefs = [{
    id = "b1";
    content = "AI is useful";
    confidence = 0.8;
    supporters = ["mind-a"];
    created_at = 1000.0;
    last_reinforced = 2000.0;
  }];
  goals = [];
  insights = [];
  state = {
    coherence = 0.5;
    activity_level = 0.3;
    dominant_mood = "curious";
    focus_topic = Some "testing";
    last_sync = 1000.0;
  };
  created_at = 500.0;
}

(* Pure.add_mind tests *)
let test_pure_add_mind_new () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.add_mind n ~mind_id:"mind-c" in
  check int "minds count" 3 (List.length n'.minds);
  check bool "mind-c present" true (List.mem "mind-c" n'.minds)

let test_pure_add_mind_existing () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.add_mind n ~mind_id:"mind-a" in
  (* Idempotent - should not add duplicate *)
  check int "minds count unchanged" 2 (List.length n'.minds)

let test_pure_add_mind_preserves_others () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.add_mind n ~mind_id:"mind-c" in
  check bool "mind-a still present" true (List.mem "mind-a" n'.minds);
  check bool "mind-b still present" true (List.mem "mind-b" n'.minds)

(* Pure.remove_mind tests *)
let test_pure_remove_mind () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.remove_mind n ~mind_id:"mind-a" in
  check int "minds count" 1 (List.length n'.minds);
  check bool "mind-a removed" false (List.mem "mind-a" n'.minds);
  check bool "mind-b still present" true (List.mem "mind-b" n'.minds)

let test_pure_remove_mind_removes_connections () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.remove_mind n ~mind_id:"mind-a" in
  (* Connection from mind-a to mind-b should be removed *)
  check int "connections cleared" 0 (List.length n'.connections)

let test_pure_remove_mind_nonexistent () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.remove_mind n ~mind_id:"mind-x" in
  (* No change *)
  check int "minds unchanged" 2 (List.length n'.minds);
  check int "connections unchanged" 1 (List.length n'.connections)

(* Pure.connect tests *)
let test_pure_connect_new () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.connect n ~from_mind:"mind-b" ~to_mind:"mind-a" () in
  check int "connections count" 2 (List.length n'.connections);
  let conn = List.find (fun (c : Noosphere_eio.connection) ->
    c.from_mind = "mind-b" && c.to_mind = "mind-a"
  ) n'.connections in
  check (float 0.01) "default strength" 0.5 conn.strength

let test_pure_connect_custom_strength () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.connect n ~from_mind:"mind-b" ~to_mind:"mind-a" ~strength:0.9 () in
  let conn = List.find (fun (c : Noosphere_eio.connection) ->
    c.from_mind = "mind-b" && c.to_mind = "mind-a"
  ) n'.connections in
  check (float 0.01) "custom strength" 0.9 conn.strength

let test_pure_connect_strengthen_existing () =
  let n = make_test_noosphere () in
  (* Existing connection: mind-a -> mind-b with strength 0.5 *)
  let n' = Noosphere_eio.Pure.connect n ~from_mind:"mind-a" ~to_mind:"mind-b" () in
  check int "no new connection" 1 (List.length n'.connections);
  let conn = List.hd n'.connections in
  (* Strength should increase by 0.1 *)
  check (float 0.01) "strength increased" 0.6 conn.strength

(* Pure.share_belief tests *)
let test_pure_share_belief_new () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.share_belief n ~content:"OCaml is great" ~mind_id:"mind-b" in
  check int "beliefs count" 2 (List.length n'.beliefs);
  let belief = List.find (fun (b : Noosphere_eio.shared_belief) ->
    b.content = "OCaml is great"
  ) n'.beliefs in
  check (float 0.01) "default confidence" 0.5 belief.confidence;
  check bool "mind-b supporter" true (List.mem "mind-b" belief.supporters)

let test_pure_share_belief_reinforce_existing () =
  let n = make_test_noosphere () in
  (* Existing belief: "AI is useful" with mind-a as supporter, confidence 0.8 *)
  let n' = Noosphere_eio.Pure.share_belief n ~content:"AI is useful" ~mind_id:"mind-b" in
  check int "no new belief" 1 (List.length n'.beliefs);
  let belief = List.hd n'.beliefs in
  check (float 0.01) "confidence increased" 0.9 belief.confidence;
  check bool "mind-a still supporter" true (List.mem "mind-a" belief.supporters);
  check bool "mind-b added supporter" true (List.mem "mind-b" belief.supporters)

let test_pure_share_belief_case_insensitive () =
  let n = make_test_noosphere () in
  (* Existing belief: "AI is useful" - try with different case *)
  let n' = Noosphere_eio.Pure.share_belief n ~content:"ai is USEFUL" ~mind_id:"mind-b" in
  check int "matched existing" 1 (List.length n'.beliefs)

let test_pure_share_belief_same_supporter () =
  let n = make_test_noosphere () in
  (* mind-a already supports "AI is useful" *)
  let n' = Noosphere_eio.Pure.share_belief n ~content:"AI is useful" ~mind_id:"mind-a" in
  let belief = List.hd n'.beliefs in
  (* Should not duplicate supporter *)
  check int "supporters unchanged" 1 (List.length belief.supporters)

(* Pure.synthesize tests *)
let test_pure_synthesize_success () =
  let n = make_test_noosphere () in
  match Noosphere_eio.Pure.synthesize n ~topic:"test topic" ~contributions:["i1"; "i2"; "i3"] with
  | Some insight ->
      check bool "id starts with collective_insight" true
        (String.sub insight.id 0 18 = "collective_insight");
      check string "topic" "test topic" insight.topic;
      check int "contributions" 3 (List.length insight.contributing_insights);
      check int "depth" 5 insight.depth  (* min 10 (3 + 2) = 5 *)
  | None -> fail "expected Some insight"

let test_pure_synthesize_insufficient_contributions () =
  let n = make_test_noosphere () in
  let result = Noosphere_eio.Pure.synthesize n ~topic:"test" ~contributions:["i1"] in
  match result with
  | None -> check bool "ok" true true  (* Less than 2 contributions *)
  | Some _ -> fail "expected None"

let test_pure_synthesize_exactly_two () =
  let n = make_test_noosphere () in
  match Noosphere_eio.Pure.synthesize n ~topic:"minimal" ~contributions:["a"; "b"] with
  | Some insight ->
      check int "depth 4" 4 insight.depth  (* min 10 (2 + 2) = 4 *)
  | None -> fail "expected Some"

let test_pure_synthesize_depth_cap () =
  let n = make_test_noosphere () in
  (* 20 contributions should cap depth at 10 *)
  let contribs = List.init 20 (fun i -> Printf.sprintf "c%d" i) in
  match Noosphere_eio.Pure.synthesize n ~topic:"deep" ~contributions:contribs with
  | Some insight ->
      check int "depth capped at 10" 10 insight.depth
  | None -> fail "expected Some"

(* Pure.calculate_coherence tests *)
let test_pure_calculate_coherence_single_mind () =
  let n : Noosphere_eio.noosphere = {
    (make_test_noosphere ()) with
    minds = ["mind-a"];
    connections = [];
  } in
  let coherence = Noosphere_eio.Pure.calculate_coherence n in
  check (float 0.01) "single mind = 1.0" 1.0 coherence

let test_pure_calculate_coherence_no_minds () =
  let n : Noosphere_eio.noosphere = {
    (make_test_noosphere ()) with
    minds = [];
    connections = [];
  } in
  let coherence = Noosphere_eio.Pure.calculate_coherence n in
  check (float 0.01) "no minds = 1.0" 1.0 coherence

let test_pure_calculate_coherence_no_connections () =
  let n : Noosphere_eio.noosphere = {
    (make_test_noosphere ()) with
    minds = ["a"; "b"; "c"];
    connections = [];
  } in
  let coherence = Noosphere_eio.Pure.calculate_coherence n in
  (* total_possible = 3*2 = 6, actual = 0, connectivity = 0, result = 0 *)
  check (float 0.01) "no connections = 0.0" 0.0 coherence

let test_pure_calculate_coherence_with_connections () =
  let n = make_test_noosphere () in
  (* 2 minds, total_possible = 2*1 = 2, actual = 1, avg_strength = 0.5 *)
  (* connectivity = 1/2 = 0.5, coherence = 0.5 * 0.5 = 0.25 *)
  let coherence = Noosphere_eio.Pure.calculate_coherence n in
  check (float 0.01) "partial coherence" 0.25 coherence

(* Pure.sync_state tests *)
let test_pure_sync_state_updates_coherence () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.sync_state n in
  (* Should recalculate coherence *)
  check (float 0.01) "coherence updated" 0.25 n'.state.coherence

let test_pure_sync_state_updates_last_sync () =
  let n = make_test_noosphere () in
  let n' = Noosphere_eio.Pure.sync_state n in
  check bool "last_sync updated" true (n'.state.last_sync > n.state.last_sync)

let test_pure_sync_state_activity_no_connections () =
  let n : Noosphere_eio.noosphere = {
    (make_test_noosphere ()) with
    connections = [];
  } in
  let n' = Noosphere_eio.Pure.sync_state n in
  check (float 0.01) "activity 0 with no connections" 0.0 n'.state.activity_level

(* ============================================================
   JSON Roundtrip Tests
   ============================================================ *)

let test_noosphere_roundtrip () =
  let original = make_test_noosphere () in
  let json = Noosphere_eio.noosphere_to_json original in
  let decoded = Noosphere_eio.noosphere_of_json json in
  check string "id" original.id decoded.id;
  check string "name" original.name decoded.name;
  check int "minds count" (List.length original.minds) (List.length decoded.minds);
  check int "connections count" (List.length original.connections) (List.length decoded.connections);
  check int "beliefs count" (List.length original.beliefs) (List.length decoded.beliefs)

let test_connection_roundtrip () =
  let original : Noosphere_eio.connection = {
    from_mind = "a";
    to_mind = "b";
    strength = 0.77;
    established_at = 123.0;
    last_interaction = 456.0;
  } in
  let json = Noosphere_eio.connection_to_json original in
  let decoded = Noosphere_eio.connection_of_json json in
  check string "from" original.from_mind decoded.from_mind;
  check string "to" original.to_mind decoded.to_mind;
  check (float 0.01) "strength" original.strength decoded.strength

let test_shared_belief_roundtrip () =
  let original : Noosphere_eio.shared_belief = {
    id = "belief-rt";
    content = "Test content";
    confidence = 0.85;
    supporters = ["x"; "y"; "z"];
    created_at = 100.0;
    last_reinforced = 200.0;
  } in
  let json = Noosphere_eio.shared_belief_to_json original in
  let decoded = Noosphere_eio.shared_belief_of_json json in
  check string "id" original.id decoded.id;
  check string "content" original.content decoded.content;
  check int "supporters" (List.length original.supporters) (List.length decoded.supporters)

let test_emergent_goal_roundtrip () =
  let original : Noosphere_eio.emergent_goal = {
    id = "goal-rt";
    description = "Test goal";
    emerged_at = 50.0;
    contributing_minds = ["m1"; "m2"];
    strength = 0.65;
    status = `Active;
  } in
  let json = Noosphere_eio.emergent_goal_to_json original in
  let decoded = Noosphere_eio.emergent_goal_of_json json in
  check string "id" original.id decoded.id;
  check string "description" original.description decoded.description;
  (match decoded.status with
   | `Active -> check bool "status active" true true
   | _ -> fail "expected Active")

let test_collective_insight_roundtrip () =
  let original : Noosphere_eio.collective_insight = {
    id = "ci-rt";
    topic = "roundtrip test";
    synthesis = "synthesized content";
    contributing_insights = ["a"; "b"; "c"];
    emerged_at = 999.0;
    depth = 7;
  } in
  let json = Noosphere_eio.collective_insight_to_json original in
  let decoded = Noosphere_eio.collective_insight_of_json json in
  check string "id" original.id decoded.id;
  check string "topic" original.topic decoded.topic;
  check int "depth" original.depth decoded.depth

let test_global_state_roundtrip () =
  let original : Noosphere_eio.global_state = {
    coherence = 0.88;
    activity_level = 0.55;
    dominant_mood = "focused";
    focus_topic = Some "OCaml";
    last_sync = 12345.0;
  } in
  let json = Noosphere_eio.global_state_to_json original in
  let decoded = Noosphere_eio.global_state_of_json json in
  check (float 0.01) "coherence" original.coherence decoded.coherence;
  check string "mood" original.dominant_mood decoded.dominant_mood

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Noosphere Eio Coverage" [
    "generate_id", [
      test_case "prefix" `Quick test_generate_id_prefix;
      test_case "underscore" `Quick test_generate_id_underscore;
      test_case "length" `Quick test_generate_id_length;
      test_case "unique" `Quick test_generate_id_unique;
    ];
    "connection", [
      test_case "type" `Quick test_connection_type;
      test_case "to_json" `Quick test_connection_to_json;
      test_case "of_json" `Quick test_connection_of_json;
    ];
    "shared_belief", [
      test_case "type" `Quick test_shared_belief_type;
      test_case "to_json" `Quick test_shared_belief_to_json;
      test_case "of_json" `Quick test_shared_belief_of_json;
    ];
    "emergent_goal", [
      test_case "emerging" `Quick test_emergent_goal_emerging;
      test_case "active" `Quick test_emergent_goal_active;
      test_case "fading" `Quick test_emergent_goal_fading;
      test_case "dissolved" `Quick test_emergent_goal_dissolved;
      test_case "to_json" `Quick test_emergent_goal_to_json;
      test_case "of_json emerging" `Quick test_emergent_goal_of_json_emerging;
      test_case "of_json active" `Quick test_emergent_goal_of_json_active;
      test_case "of_json fading" `Quick test_emergent_goal_of_json_fading;
      test_case "of_json dissolved" `Quick test_emergent_goal_of_json_dissolved;
      test_case "of_json unknown" `Quick test_emergent_goal_of_json_unknown;
    ];
    "collective_insight", [
      test_case "type" `Quick test_collective_insight_type;
      test_case "to_json" `Quick test_collective_insight_to_json;
      test_case "of_json" `Quick test_collective_insight_of_json;
    ];
    "global_state", [
      test_case "type" `Quick test_global_state_type;
      test_case "no focus" `Quick test_global_state_no_focus;
      test_case "to_json" `Quick test_global_state_to_json;
      test_case "of_json" `Quick test_global_state_of_json;
    ];
    "noosphere", [
      test_case "type" `Quick test_noosphere_type;
      test_case "to_json" `Quick test_noosphere_to_json;
      test_case "of_json" `Quick test_noosphere_of_json;
    ];
    "get_string", [
      test_case "present" `Quick test_get_string_present;
      test_case "missing" `Quick test_get_string_missing;
    ];
    "get_float", [
      test_case "present" `Quick test_get_float_present;
      test_case "from int" `Quick test_get_float_from_int;
      test_case "missing" `Quick test_get_float_missing;
    ];
    "get_int", [
      test_case "present" `Quick test_get_int_present;
      test_case "missing" `Quick test_get_int_missing;
    ];
    "get_string_list", [
      test_case "present" `Quick test_get_string_list_present;
      test_case "empty" `Quick test_get_string_list_empty;
      test_case "missing" `Quick test_get_string_list_missing;
    ];
    "get_string_opt", [
      test_case "some" `Quick test_get_string_opt_some;
      test_case "null" `Quick test_get_string_opt_null;
      test_case "missing" `Quick test_get_string_opt_missing;
    ];
    "pure_add_mind", [
      test_case "new" `Quick test_pure_add_mind_new;
      test_case "existing" `Quick test_pure_add_mind_existing;
      test_case "preserves others" `Quick test_pure_add_mind_preserves_others;
    ];
    "pure_remove_mind", [
      test_case "remove" `Quick test_pure_remove_mind;
      test_case "removes connections" `Quick test_pure_remove_mind_removes_connections;
      test_case "nonexistent" `Quick test_pure_remove_mind_nonexistent;
    ];
    "pure_connect", [
      test_case "new" `Quick test_pure_connect_new;
      test_case "custom strength" `Quick test_pure_connect_custom_strength;
      test_case "strengthen existing" `Quick test_pure_connect_strengthen_existing;
    ];
    "pure_share_belief", [
      test_case "new" `Quick test_pure_share_belief_new;
      test_case "reinforce existing" `Quick test_pure_share_belief_reinforce_existing;
      test_case "case insensitive" `Quick test_pure_share_belief_case_insensitive;
      test_case "same supporter" `Quick test_pure_share_belief_same_supporter;
    ];
    "pure_synthesize", [
      test_case "success" `Quick test_pure_synthesize_success;
      test_case "insufficient" `Quick test_pure_synthesize_insufficient_contributions;
      test_case "exactly two" `Quick test_pure_synthesize_exactly_two;
      test_case "depth cap" `Quick test_pure_synthesize_depth_cap;
    ];
    "pure_calculate_coherence", [
      test_case "single mind" `Quick test_pure_calculate_coherence_single_mind;
      test_case "no minds" `Quick test_pure_calculate_coherence_no_minds;
      test_case "no connections" `Quick test_pure_calculate_coherence_no_connections;
      test_case "with connections" `Quick test_pure_calculate_coherence_with_connections;
    ];
    "pure_sync_state", [
      test_case "updates coherence" `Quick test_pure_sync_state_updates_coherence;
      test_case "updates last_sync" `Quick test_pure_sync_state_updates_last_sync;
      test_case "activity no connections" `Quick test_pure_sync_state_activity_no_connections;
    ];
    "roundtrip", [
      test_case "noosphere" `Quick test_noosphere_roundtrip;
      test_case "connection" `Quick test_connection_roundtrip;
      test_case "shared_belief" `Quick test_shared_belief_roundtrip;
      test_case "emergent_goal" `Quick test_emergent_goal_roundtrip;
      test_case "collective_insight" `Quick test_collective_insight_roundtrip;
      test_case "global_state" `Quick test_global_state_roundtrip;
    ];
  ]
