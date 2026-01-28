(** Mind_eio Module Coverage Tests

    Tests for MASC Level 6 Self-Aware Meta-Cognition:
    - Type definitions and record construction
    - JSON serialization functions
    - JSON deserialization helpers
    - ID generation
*)

open Alcotest

module Mind_eio = Masc_mcp.Mind_eio

(* ============================================================
   generate_id Tests
   ============================================================ *)

let test_generate_id_prefix () =
  let id = Mind_eio.generate_id "thought" in
  check bool "starts with prefix" true (String.sub id 0 7 = "thought")

let test_generate_id_underscore () =
  let id = Mind_eio.generate_id "test" in
  check bool "has underscore" true (String.contains id '_')

let test_generate_id_length () =
  let id = Mind_eio.generate_id "abc" in
  (* "abc_" + 8 chars = 12 *)
  check int "length" 12 (String.length id)

let test_generate_id_unique () =
  let id1 = Mind_eio.generate_id "test" in
  let id2 = Mind_eio.generate_id "test" in
  check bool "unique" true (id1 <> id2)

(* ============================================================
   self_model Type Tests
   ============================================================ *)

let test_self_model_idle () =
  let s : Mind_eio.self_model = {
    id = "self-001";
    name = "agent";
    capabilities = ["code"; "test"];
    limitations = ["memory"];
    current_state = `Idle;
    confidence = 0.8;
    last_updated = 1704067200.0;
  } in
  check string "id" "self-001" s.id;
  check string "name" "agent" s.name;
  check int "capabilities" 2 (List.length s.capabilities)

let test_self_model_working () =
  let s : Mind_eio.self_model = {
    id = "self-002";
    name = "worker";
    capabilities = [];
    limitations = [];
    current_state = `Working;
    confidence = 0.9;
    last_updated = 0.0;
  } in
  check bool "working state" true (s.current_state = `Working)

let test_self_model_reflecting () =
  let s : Mind_eio.self_model = {
    id = "self-003";
    name = "thinker";
    capabilities = [];
    limitations = [];
    current_state = `Reflecting;
    confidence = 0.5;
    last_updated = 0.0;
  } in
  check bool "reflecting state" true (s.current_state = `Reflecting)

let test_self_model_learning () =
  let s : Mind_eio.self_model = {
    id = "self-004";
    name = "learner";
    capabilities = [];
    limitations = [];
    current_state = `Learning;
    confidence = 0.7;
    last_updated = 0.0;
  } in
  check bool "learning state" true (s.current_state = `Learning)

(* ============================================================
   self_model_to_json Tests
   ============================================================ *)

let test_self_model_to_json_basic () =
  let s : Mind_eio.self_model = {
    id = "self-json";
    name = "test-agent";
    capabilities = ["a"; "b"];
    limitations = ["x"];
    current_state = `Idle;
    confidence = 0.75;
    last_updated = 1000.0;
  } in
  let json = Mind_eio.self_model_to_json s in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

let test_self_model_to_json_has_id () =
  let s : Mind_eio.self_model = {
    id = "my-id";
    name = "n";
    capabilities = [];
    limitations = [];
    current_state = `Idle;
    confidence = 0.0;
    last_updated = 0.0;
  } in
  let json = Mind_eio.self_model_to_json s in
  let open Yojson.Safe.Util in
  check string "id" "my-id" (json |> member "id" |> to_string)

let test_self_model_to_json_state_idle () =
  let s : Mind_eio.self_model = {
    id = "s";
    name = "n";
    capabilities = [];
    limitations = [];
    current_state = `Idle;
    confidence = 0.0;
    last_updated = 0.0;
  } in
  let json = Mind_eio.self_model_to_json s in
  let open Yojson.Safe.Util in
  check string "state" "idle" (json |> member "current_state" |> to_string)

let test_self_model_to_json_state_working () =
  let s : Mind_eio.self_model = {
    id = "s";
    name = "n";
    capabilities = [];
    limitations = [];
    current_state = `Working;
    confidence = 0.0;
    last_updated = 0.0;
  } in
  let json = Mind_eio.self_model_to_json s in
  let open Yojson.Safe.Util in
  check string "state" "working" (json |> member "current_state" |> to_string)

(* ============================================================
   thought Type Tests
   ============================================================ *)

let test_thought_observation () =
  let t : Mind_eio.thought = {
    id = "thought-001";
    timestamp = 1000.0;
    content = "I see something";
    thought_type = `Observation;
    confidence = 0.9;
    related_to = [];
  } in
  check bool "observation" true (t.thought_type = `Observation)

let test_thought_question () =
  let t : Mind_eio.thought = {
    id = "thought-002";
    timestamp = 2000.0;
    content = "Why?";
    thought_type = `Question;
    confidence = 0.5;
    related_to = ["thought-001"];
  } in
  check bool "question" true (t.thought_type = `Question)

let test_thought_hypothesis () =
  let t : Mind_eio.thought = {
    id = "thought-003";
    timestamp = 3000.0;
    content = "Maybe because...";
    thought_type = `Hypothesis;
    confidence = 0.6;
    related_to = [];
  } in
  check bool "hypothesis" true (t.thought_type = `Hypothesis)

let test_thought_conclusion () =
  let t : Mind_eio.thought = {
    id = "thought-004";
    timestamp = 4000.0;
    content = "Therefore...";
    thought_type = `Conclusion;
    confidence = 0.8;
    related_to = [];
  } in
  check bool "conclusion" true (t.thought_type = `Conclusion)

let test_thought_doubt () =
  let t : Mind_eio.thought = {
    id = "thought-005";
    timestamp = 5000.0;
    content = "But wait...";
    thought_type = `Doubt;
    confidence = 0.3;
    related_to = [];
  } in
  check bool "doubt" true (t.thought_type = `Doubt)

(* ============================================================
   thought_to_json Tests
   ============================================================ *)

let test_thought_to_json_basic () =
  let t : Mind_eio.thought = {
    id = "t1";
    timestamp = 100.0;
    content = "test";
    thought_type = `Observation;
    confidence = 0.5;
    related_to = ["a"; "b"];
  } in
  let json = Mind_eio.thought_to_json t in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

let test_thought_to_json_type_observation () =
  let t : Mind_eio.thought = {
    id = "t";
    timestamp = 0.0;
    content = "c";
    thought_type = `Observation;
    confidence = 0.0;
    related_to = [];
  } in
  let json = Mind_eio.thought_to_json t in
  let open Yojson.Safe.Util in
  check string "type" "observation" (json |> member "thought_type" |> to_string)

let test_thought_to_json_type_question () =
  let t : Mind_eio.thought = {
    id = "t";
    timestamp = 0.0;
    content = "c";
    thought_type = `Question;
    confidence = 0.0;
    related_to = [];
  } in
  let json = Mind_eio.thought_to_json t in
  let open Yojson.Safe.Util in
  check string "type" "question" (json |> member "thought_type" |> to_string)

(* ============================================================
   goal Type Tests
   ============================================================ *)

let test_goal_active () =
  let g : Mind_eio.goal = {
    id = "goal-001";
    description = "Complete task";
    priority = 1.0;
    status = `Active;
    parent_goal = None;
    sub_goals = [];
    deadline = None;
    progress = 0.5;
  } in
  check bool "active" true (g.status = `Active)

let test_goal_paused () =
  let g : Mind_eio.goal = {
    id = "goal-002";
    description = "Paused";
    priority = 0.5;
    status = `Paused;
    parent_goal = Some "goal-001";
    sub_goals = ["goal-003"];
    deadline = Some 1704067200.0;
    progress = 0.2;
  } in
  check bool "paused" true (g.status = `Paused);
  check (option string) "parent" (Some "goal-001") g.parent_goal

let test_goal_completed () =
  let g : Mind_eio.goal = {
    id = "goal-003";
    description = "Done";
    priority = 1.0;
    status = `Completed;
    parent_goal = None;
    sub_goals = [];
    deadline = None;
    progress = 1.0;
  } in
  check bool "completed" true (g.status = `Completed)

let test_goal_abandoned () =
  let g : Mind_eio.goal = {
    id = "goal-004";
    description = "Gave up";
    priority = 0.1;
    status = `Abandoned;
    parent_goal = None;
    sub_goals = [];
    deadline = None;
    progress = 0.0;
  } in
  check bool "abandoned" true (g.status = `Abandoned)

(* ============================================================
   goal_to_json Tests
   ============================================================ *)

let test_goal_to_json_basic () =
  let g : Mind_eio.goal = {
    id = "g1";
    description = "test goal";
    priority = 0.8;
    status = `Active;
    parent_goal = None;
    sub_goals = [];
    deadline = None;
    progress = 0.3;
  } in
  let json = Mind_eio.goal_to_json g in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

let test_goal_to_json_status_active () =
  let g : Mind_eio.goal = {
    id = "g";
    description = "d";
    priority = 0.0;
    status = `Active;
    parent_goal = None;
    sub_goals = [];
    deadline = None;
    progress = 0.0;
  } in
  let json = Mind_eio.goal_to_json g in
  let open Yojson.Safe.Util in
  check string "status" "active" (json |> member "status" |> to_string)

let test_goal_to_json_status_completed () =
  let g : Mind_eio.goal = {
    id = "g";
    description = "d";
    priority = 0.0;
    status = `Completed;
    parent_goal = None;
    sub_goals = [];
    deadline = None;
    progress = 0.0;
  } in
  let json = Mind_eio.goal_to_json g in
  let open Yojson.Safe.Util in
  check string "status" "completed" (json |> member "status" |> to_string)

let test_goal_to_json_with_parent () =
  let g : Mind_eio.goal = {
    id = "child";
    description = "sub goal";
    priority = 0.5;
    status = `Active;
    parent_goal = Some "parent-id";
    sub_goals = [];
    deadline = None;
    progress = 0.0;
  } in
  let json = Mind_eio.goal_to_json g in
  let open Yojson.Safe.Util in
  check string "parent" "parent-id" (json |> member "parent_goal" |> to_string)

let test_goal_to_json_null_parent () =
  let g : Mind_eio.goal = {
    id = "orphan";
    description = "no parent";
    priority = 1.0;
    status = `Active;
    parent_goal = None;
    sub_goals = [];
    deadline = None;
    progress = 0.0;
  } in
  let json = Mind_eio.goal_to_json g in
  let open Yojson.Safe.Util in
  check bool "parent is null" true (json |> member "parent_goal" = `Null)

(* ============================================================
   anomaly Type Tests
   ============================================================ *)

let test_anomaly_low () =
  let a : Mind_eio.anomaly = {
    id = "anomaly-001";
    detected_at = 1000.0;
    severity = `Low;
    description = "Minor issue";
    affected_systems = ["logging"];
    suggested_action = Some "Monitor";
  } in
  check bool "low severity" true (a.severity = `Low)

let test_anomaly_medium () =
  let a : Mind_eio.anomaly = {
    id = "anomaly-002";
    detected_at = 2000.0;
    severity = `Medium;
    description = "Moderate issue";
    affected_systems = [];
    suggested_action = None;
  } in
  check bool "medium severity" true (a.severity = `Medium)

let test_anomaly_high () =
  let a : Mind_eio.anomaly = {
    id = "anomaly-003";
    detected_at = 3000.0;
    severity = `High;
    description = "Serious issue";
    affected_systems = ["database"; "api"];
    suggested_action = Some "Investigate immediately";
  } in
  check bool "high severity" true (a.severity = `High)

let test_anomaly_critical () =
  let a : Mind_eio.anomaly = {
    id = "anomaly-004";
    detected_at = 4000.0;
    severity = `Critical;
    description = "System down";
    affected_systems = ["all"];
    suggested_action = Some "Emergency response";
  } in
  check bool "critical severity" true (a.severity = `Critical)

(* ============================================================
   anomaly_to_json Tests
   ============================================================ *)

let test_anomaly_to_json_basic () =
  let a : Mind_eio.anomaly = {
    id = "a1";
    detected_at = 100.0;
    severity = `Low;
    description = "test";
    affected_systems = [];
    suggested_action = None;
  } in
  let json = Mind_eio.anomaly_to_json a in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

let test_anomaly_to_json_severity_low () =
  let a : Mind_eio.anomaly = {
    id = "a";
    detected_at = 0.0;
    severity = `Low;
    description = "";
    affected_systems = [];
    suggested_action = None;
  } in
  let json = Mind_eio.anomaly_to_json a in
  let open Yojson.Safe.Util in
  check string "severity" "low" (json |> member "severity" |> to_string)

let test_anomaly_to_json_severity_critical () =
  let a : Mind_eio.anomaly = {
    id = "a";
    detected_at = 0.0;
    severity = `Critical;
    description = "";
    affected_systems = [];
    suggested_action = None;
  } in
  let json = Mind_eio.anomaly_to_json a in
  let open Yojson.Safe.Util in
  check string "severity" "critical" (json |> member "severity" |> to_string)

let test_anomaly_to_json_with_action () =
  let a : Mind_eio.anomaly = {
    id = "a";
    detected_at = 0.0;
    severity = `Medium;
    description = "";
    affected_systems = [];
    suggested_action = Some "fix it";
  } in
  let json = Mind_eio.anomaly_to_json a in
  let open Yojson.Safe.Util in
  check string "action" "fix it" (json |> member "suggested_action" |> to_string)

(* ============================================================
   Deserialization Helpers Tests
   ============================================================ *)

let test_get_string_present () =
  let json = `Assoc [("key", `String "value")] in
  check string "get_string" "value" (Mind_eio.get_string json "key")

let test_get_string_missing () =
  let json = `Assoc [] in
  check string "get_string missing" "" (Mind_eio.get_string json "key")

let test_get_string_wrong_type () =
  let json = `Assoc [("key", `Int 42)] in
  check string "get_string wrong type" "" (Mind_eio.get_string json "key")

let test_get_string_not_assoc () =
  let json = `List [] in
  check string "get_string not assoc" "" (Mind_eio.get_string json "key")

let test_get_float_present () =
  let json = `Assoc [("num", `Float 3.14)] in
  check (float 0.01) "get_float" 3.14 (Mind_eio.get_float json "num")

let test_get_float_from_int () =
  let json = `Assoc [("num", `Int 42)] in
  check (float 0.01) "get_float from int" 42.0 (Mind_eio.get_float json "num")

let test_get_float_missing () =
  let json = `Assoc [] in
  check (float 0.01) "get_float missing" 0.0 (Mind_eio.get_float json "num")

let test_get_int_present () =
  let json = `Assoc [("num", `Int 42)] in
  check int "get_int" 42 (Mind_eio.get_int json "num")

let test_get_int_from_float () =
  let json = `Assoc [("num", `Float 42.7)] in
  check int "get_int from float" 42 (Mind_eio.get_int json "num")

let test_get_int_missing () =
  let json = `Assoc [] in
  check int "get_int missing" 0 (Mind_eio.get_int json "num")

let test_get_bool_true () =
  let json = `Assoc [("flag", `Bool true)] in
  check bool "get_bool true" true (Mind_eio.get_bool json "flag")

let test_get_bool_false () =
  let json = `Assoc [("flag", `Bool false)] in
  check bool "get_bool false" false (Mind_eio.get_bool json "flag")

let test_get_bool_missing () =
  let json = `Assoc [] in
  check bool "get_bool missing" false (Mind_eio.get_bool json "flag")

let test_get_string_list_present () =
  let json = `Assoc [("items", `List [`String "a"; `String "b"])] in
  check (list string) "get_string_list" ["a"; "b"] (Mind_eio.get_string_list json "items")

let test_get_string_list_empty () =
  let json = `Assoc [("items", `List [])] in
  check (list string) "get_string_list empty" [] (Mind_eio.get_string_list json "items")

let test_get_string_list_mixed () =
  let json = `Assoc [("items", `List [`String "a"; `Int 1; `String "b"])] in
  check (list string) "get_string_list mixed" ["a"; "b"] (Mind_eio.get_string_list json "items")

let test_get_string_list_missing () =
  let json = `Assoc [] in
  check (list string) "get_string_list missing" [] (Mind_eio.get_string_list json "items")

let test_get_string_opt_some () =
  let json = `Assoc [("key", `String "value")] in
  check (option string) "get_string_opt some" (Some "value") (Mind_eio.get_string_opt json "key")

let test_get_string_opt_null () =
  let json = `Assoc [("key", `Null)] in
  check (option string) "get_string_opt null" None (Mind_eio.get_string_opt json "key")

let test_get_string_opt_missing () =
  let json = `Assoc [] in
  check (option string) "get_string_opt missing" None (Mind_eio.get_string_opt json "key")

let test_get_float_opt_some () =
  let json = `Assoc [("num", `Float 1.5)] in
  check (option (float 0.01)) "get_float_opt some" (Some 1.5) (Mind_eio.get_float_opt json "num")

let test_get_float_opt_from_int () =
  let json = `Assoc [("num", `Int 10)] in
  check (option (float 0.01)) "get_float_opt int" (Some 10.0) (Mind_eio.get_float_opt json "num")

let test_get_float_opt_null () =
  let json = `Assoc [("num", `Null)] in
  check (option (float 0.01)) "get_float_opt null" None (Mind_eio.get_float_opt json "num")

(* ============================================================
   insight Type Tests
   ============================================================ *)

let test_insight_basic () =
  let i : Mind_eio.insight = {
    id = "insight-001";
    timestamp = 1000.0;
    topic = "performance";
    content = "System is slow";
    depth = 2;
    actionable = true;
    related_insights = ["insight-000"];
  } in
  check string "id" "insight-001" i.id;
  check int "depth" 2 i.depth;
  check bool "actionable" true i.actionable

let test_insight_to_json () =
  let i : Mind_eio.insight = {
    id = "i1";
    timestamp = 100.0;
    topic = "test";
    content = "content";
    depth = 1;
    actionable = false;
    related_insights = [];
  } in
  let json = Mind_eio.insight_to_json i in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

(* ============================================================
   learning Type Tests
   ============================================================ *)

let test_learning_basic () =
  let l : Mind_eio.learning = {
    id = "learn-001";
    learned_at = 1000.0;
    trigger = "error encountered";
    insight = "Need better error handling";
    confidence = 0.85;
    applicable_to = ["error-handling"; "validation"];
  } in
  check string "id" "learn-001" l.id;
  check (float 0.01) "confidence" 0.85 l.confidence

let test_learning_to_json () =
  let l : Mind_eio.learning = {
    id = "l1";
    learned_at = 100.0;
    trigger = "t";
    insight = "i";
    confidence = 0.5;
    applicable_to = [];
  } in
  let json = Mind_eio.learning_to_json l in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

(* ============================================================
   value Type Tests
   ============================================================ *)

let test_value_basic () =
  let v : Mind_eio.value = {
    id = "value-001";
    name = "quality";
    description = "High quality code";
    weight = 0.9;
    source = "principles";
    examples = ["clean code"; "tests"];
    conflicts_with = ["speed"];
  } in
  check string "name" "quality" v.name;
  check (float 0.01) "weight" 0.9 v.weight

let test_value_to_json () =
  let v : Mind_eio.value = {
    id = "v1";
    name = "n";
    description = "d";
    weight = 0.5;
    source = "s";
    examples = [];
    conflicts_with = [];
  } in
  let json = Mind_eio.value_to_json v in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

(* ============================================================
   meta_cognition Type Tests
   ============================================================ *)

let test_meta_cognition_basic () =
  let m : Mind_eio.meta_cognition = {
    monitoring_active = true;
    evaluation_threshold = 0.7;
    reflection_interval = 60.0;
    last_reflection = 1000.0;
  } in
  check bool "monitoring" true m.monitoring_active;
  check (float 0.01) "threshold" 0.7 m.evaluation_threshold

(* ============================================================
   evaluation Type Tests
   ============================================================ *)

let test_evaluation_basic () =
  let e : Mind_eio.evaluation = {
    target_id = "task-001";
    score = 0.8;
    reasoning = "Good progress";
    alternatives = ["option-a"; "option-b"];
    should_revise = false;
  } in
  check string "target" "task-001" e.target_id;
  check bool "revise" false e.should_revise

(* ============================================================
   self_model_of_json Tests
   ============================================================ *)

let test_self_model_of_json_basic () =
  let json = `Assoc [
    ("id", `String "test-id");
    ("name", `String "test-name");
    ("capabilities", `List [`String "cap1"]);
    ("limitations", `List [`String "lim1"]);
    ("current_state", `String "idle");
    ("confidence", `Float 0.75);
    ("last_updated", `Float 1000.0);
  ] in
  let s = Mind_eio.self_model_of_json json in
  check string "id" "test-id" s.id;
  check string "name" "test-name" s.name

let test_self_model_of_json_working () =
  let json = `Assoc [
    ("id", `String "s"); ("name", `String "n");
    ("capabilities", `List []); ("limitations", `List []);
    ("current_state", `String "working");
    ("confidence", `Float 0.5); ("last_updated", `Float 0.0);
  ] in
  let s = Mind_eio.self_model_of_json json in
  check bool "working state" true (s.current_state = `Working)

let test_self_model_of_json_reflecting () =
  let json = `Assoc [
    ("id", `String "s"); ("name", `String "n");
    ("capabilities", `List []); ("limitations", `List []);
    ("current_state", `String "reflecting");
    ("confidence", `Float 0.5); ("last_updated", `Float 0.0);
  ] in
  let s = Mind_eio.self_model_of_json json in
  check bool "reflecting state" true (s.current_state = `Reflecting)

let test_self_model_of_json_learning () =
  let json = `Assoc [
    ("id", `String "s"); ("name", `String "n");
    ("capabilities", `List []); ("limitations", `List []);
    ("current_state", `String "learning");
    ("confidence", `Float 0.5); ("last_updated", `Float 0.0);
  ] in
  let s = Mind_eio.self_model_of_json json in
  check bool "learning state" true (s.current_state = `Learning)

(* ============================================================
   thought_of_json Tests
   ============================================================ *)

let test_thought_of_json_observation () =
  let json = `Assoc [
    ("id", `String "t1"); ("timestamp", `Float 100.0);
    ("content", `String "test"); ("thought_type", `String "observation");
    ("confidence", `Float 0.8); ("related_to", `List []);
  ] in
  let t = Mind_eio.thought_of_json json in
  check bool "observation" true (t.thought_type = `Observation)

let test_thought_of_json_question () =
  let json = `Assoc [
    ("id", `String "t2"); ("timestamp", `Float 100.0);
    ("content", `String "why?"); ("thought_type", `String "question");
    ("confidence", `Float 0.5); ("related_to", `List [`String "t1"]);
  ] in
  let t = Mind_eio.thought_of_json json in
  check bool "question" true (t.thought_type = `Question);
  check int "related_to length" 1 (List.length t.related_to)

let test_thought_of_json_hypothesis () =
  let json = `Assoc [
    ("id", `String "t3"); ("timestamp", `Float 200.0);
    ("content", `String "maybe"); ("thought_type", `String "hypothesis");
    ("confidence", `Float 0.6); ("related_to", `List []);
  ] in
  let t = Mind_eio.thought_of_json json in
  check bool "hypothesis" true (t.thought_type = `Hypothesis)

let test_thought_of_json_conclusion () =
  let json = `Assoc [
    ("id", `String "t4"); ("timestamp", `Float 300.0);
    ("content", `String "therefore"); ("thought_type", `String "conclusion");
    ("confidence", `Float 0.9); ("related_to", `List []);
  ] in
  let t = Mind_eio.thought_of_json json in
  check bool "conclusion" true (t.thought_type = `Conclusion)

let test_thought_of_json_doubt () =
  let json = `Assoc [
    ("id", `String "t5"); ("timestamp", `Float 400.0);
    ("content", `String "but wait"); ("thought_type", `String "doubt");
    ("confidence", `Float 0.3); ("related_to", `List []);
  ] in
  let t = Mind_eio.thought_of_json json in
  check bool "doubt" true (t.thought_type = `Doubt)

(* ============================================================
   goal_of_json Tests
   ============================================================ *)

let test_goal_of_json_active () =
  let json = `Assoc [
    ("id", `String "g1"); ("description", `String "do thing");
    ("priority", `Float 0.8); ("status", `String "active");
    ("parent_goal", `Null); ("sub_goals", `List []);
    ("deadline", `Null); ("progress", `Float 0.5);
  ] in
  let g = Mind_eio.goal_of_json json in
  check bool "active" true (g.status = `Active);
  check (option string) "no parent" None g.parent_goal

let test_goal_of_json_paused () =
  let json = `Assoc [
    ("id", `String "g2"); ("description", `String "paused");
    ("priority", `Float 0.5); ("status", `String "paused");
    ("parent_goal", `String "g1"); ("sub_goals", `List [`String "g3"]);
    ("deadline", `Float 2000.0); ("progress", `Float 0.2);
  ] in
  let g = Mind_eio.goal_of_json json in
  check bool "paused" true (g.status = `Paused);
  check (option string) "has parent" (Some "g1") g.parent_goal;
  check (option (float 0.01)) "has deadline" (Some 2000.0) g.deadline

let test_goal_of_json_completed () =
  let json = `Assoc [
    ("id", `String "g3"); ("description", `String "done");
    ("priority", `Float 1.0); ("status", `String "completed");
    ("parent_goal", `Null); ("sub_goals", `List []);
    ("deadline", `Null); ("progress", `Float 1.0);
  ] in
  let g = Mind_eio.goal_of_json json in
  check bool "completed" true (g.status = `Completed)

let test_goal_of_json_abandoned () =
  let json = `Assoc [
    ("id", `String "g4"); ("description", `String "gave up");
    ("priority", `Float 0.1); ("status", `String "abandoned");
    ("parent_goal", `Null); ("sub_goals", `List []);
    ("deadline", `Null); ("progress", `Float 0.0);
  ] in
  let g = Mind_eio.goal_of_json json in
  check bool "abandoned" true (g.status = `Abandoned)

(* ============================================================
   insight_of_json Tests
   ============================================================ *)

let test_insight_of_json_basic () =
  let json = `Assoc [
    ("id", `String "i1"); ("timestamp", `Float 100.0);
    ("topic", `String "perf"); ("content", `String "slow");
    ("depth", `Int 3); ("actionable", `Bool true);
    ("related_insights", `List [`String "i0"]);
  ] in
  let i = Mind_eio.insight_of_json json in
  check string "id" "i1" i.id;
  check int "depth" 3 i.depth;
  check bool "actionable" true i.actionable

(* ============================================================
   learning_of_json Tests
   ============================================================ *)

let test_learning_of_json_basic () =
  let json = `Assoc [
    ("id", `String "l1"); ("learned_at", `Float 100.0);
    ("trigger", `String "error"); ("insight", `String "handle it");
    ("confidence", `Float 0.8); ("applicable_to", `List [`String "errors"]);
  ] in
  let l = Mind_eio.learning_of_json json in
  check string "id" "l1" l.id;
  check string "trigger" "error" l.trigger

(* ============================================================
   value_of_json Tests
   ============================================================ *)

let test_value_of_json_basic () =
  let json = `Assoc [
    ("id", `String "v1"); ("name", `String "quality");
    ("description", `String "high quality"); ("weight", `Float 0.9);
    ("source", `String "core"); ("examples", `List [`String "tests"]);
    ("conflicts_with", `List [`String "speed"]);
  ] in
  let v = Mind_eio.value_of_json json in
  check string "name" "quality" v.name;
  check (float 0.01) "weight" 0.9 v.weight

(* ============================================================
   mind_to_json / mind_of_json_opt Roundtrip Tests
   ============================================================ *)

let make_test_mind () : Mind_eio.mind =
  {
    self = {
      id = "self-test"; name = "test-agent";
      capabilities = ["code"]; limitations = ["mem"];
      current_state = `Working;
      confidence = 0.75; last_updated = 1000.0;
    };
    thoughts = [{
      id = "t1"; timestamp = 100.0; content = "think";
      thought_type = `Observation; confidence = 0.8; related_to = [];
    }];
    evaluations = [];
    anomalies = [];
    learnings = [{
      id = "l1"; learned_at = 200.0; trigger = "err";
      insight = "fix"; confidence = 0.7; applicable_to = [];
    }];
    insights = [{
      id = "i1"; timestamp = 300.0; topic = "perf";
      content = "fast"; depth = 2; actionable = true;
      related_insights = [];
    }];
    goals = [{
      id = "g1"; description = "complete";
      priority = 1.0; status = `Active;
      parent_goal = None; sub_goals = [];
      deadline = None; progress = 0.5;
    }];
    values = [{
      id = "v1"; name = "quality"; description = "good";
      weight = 0.9; source = "core"; examples = [];
      conflicts_with = [];
    }];
    meta = {
      monitoring_active = true;
      evaluation_threshold = 0.6;
      reflection_interval = 24.0;
      last_reflection = 500.0;
    };
  }

let test_mind_to_json_returns_assoc () =
  let m = make_test_mind () in
  let json = Mind_eio.mind_to_json m in
  match json with
  | `Assoc _ -> check bool "is assoc" true true
  | _ -> fail "expected Assoc"

let test_mind_to_json_has_self () =
  let m = make_test_mind () in
  let json = Mind_eio.mind_to_json m in
  let open Yojson.Safe.Util in
  let self = json |> member "self" in
  check bool "has self" true (self <> `Null)

let test_mind_to_json_has_meta () =
  let m = make_test_mind () in
  let json = Mind_eio.mind_to_json m in
  let open Yojson.Safe.Util in
  let meta = json |> member "meta" in
  check bool "has meta" true (meta <> `Null)

let test_mind_roundtrip () =
  let original = make_test_mind () in
  let json = Mind_eio.mind_to_json original in
  match Mind_eio.mind_of_json_opt json with
  | Some decoded ->
      check string "self.id" original.self.id decoded.self.id;
      check string "self.name" original.self.name decoded.self.name;
      check int "thoughts count" (List.length original.thoughts) (List.length decoded.thoughts);
      check int "goals count" (List.length original.goals) (List.length decoded.goals)
  | None -> fail "roundtrip failed"

let test_mind_of_json_opt_invalid () =
  let json = `List [] in
  check (option unit) "invalid json" None
    (match Mind_eio.mind_of_json_opt json with Some _ -> Some () | None -> None)

let test_mind_of_json_opt_no_self () =
  let json = `Assoc [("thoughts", `List [])] in
  check (option unit) "no self" None
    (match Mind_eio.mind_of_json_opt json with Some _ -> Some () | None -> None)

(* ============================================================
   Pure Module Tests
   ============================================================ *)

let test_pure_think () =
  let m = make_test_mind () in
  let thought : Mind_eio.thought = {
    id = "new-thought"; timestamp = 999.0; content = "new";
    thought_type = `Question; confidence = 0.5; related_to = [];
  } in
  let m' = Mind_eio.Pure.think m ~thought in
  check int "thoughts increased" (List.length m.thoughts + 1) (List.length m'.thoughts)

let test_pure_create_thought () =
  let t = Mind_eio.Pure.create_thought ~content:"test" ~thought_type:`Observation () in
  check bool "has id" true (String.length t.id > 0);
  check string "content" "test" t.content;
  check bool "type observation" true (t.thought_type = `Observation)

let test_pure_create_thought_with_options () =
  let t = Mind_eio.Pure.create_thought
    ~content:"test" ~thought_type:`Hypothesis
    ~confidence:0.9 ~related_to:["t1"; "t2"] () in
  check (float 0.01) "confidence" 0.9 t.confidence;
  check int "related_to count" 2 (List.length t.related_to)

let test_pure_add_goal () =
  let m = make_test_mind () in
  let goal : Mind_eio.goal = {
    id = "new-goal"; description = "new";
    priority = 0.5; status = `Active;
    parent_goal = None; sub_goals = [];
    deadline = None; progress = 0.0;
  } in
  let m' = Mind_eio.Pure.add_goal m ~goal in
  check int "goals increased" (List.length m.goals + 1) (List.length m'.goals)

let test_pure_update_goal_progress () =
  let m = make_test_mind () in
  let m' = Mind_eio.Pure.update_goal_progress m ~goal_id:"g1" ~progress:0.8 in
  let g = List.find (fun (g : Mind_eio.goal) -> g.id = "g1") m'.goals in
  check (float 0.01) "progress updated" 0.8 g.progress

let test_pure_update_goal_progress_clamp_max () =
  let m = make_test_mind () in
  let m' = Mind_eio.Pure.update_goal_progress m ~goal_id:"g1" ~progress:1.5 in
  let g = List.find (fun (g : Mind_eio.goal) -> g.id = "g1") m'.goals in
  check (float 0.01) "clamped to 1.0" 1.0 g.progress

let test_pure_update_goal_progress_clamp_min () =
  let m = make_test_mind () in
  let m' = Mind_eio.Pure.update_goal_progress m ~goal_id:"g1" ~progress:(-0.5) in
  let g = List.find (fun (g : Mind_eio.goal) -> g.id = "g1") m'.goals in
  check (float 0.01) "clamped to 0.0" 0.0 g.progress

let test_pure_complete_goal () =
  let m = make_test_mind () in
  let m' = Mind_eio.Pure.complete_goal m ~goal_id:"g1" in
  let g = List.find (fun (g : Mind_eio.goal) -> g.id = "g1") m'.goals in
  check bool "completed" true (g.status = `Completed);
  check (float 0.01) "progress 1.0" 1.0 g.progress

let test_pure_adopt_value () =
  let m = make_test_mind () in
  let value : Mind_eio.value = {
    id = "new-v"; name = "honesty"; description = "be honest";
    weight = 0.8; source = "core"; examples = []; conflicts_with = [];
  } in
  let m' = Mind_eio.Pure.adopt_value m ~value in
  check int "values increased" (List.length m.values + 1) (List.length m'.values)

let test_pure_learn () =
  let m = make_test_mind () in
  let learning : Mind_eio.learning = {
    id = "new-l"; learned_at = 999.0; trigger = "test";
    insight = "learned"; confidence = 0.7; applicable_to = [];
  } in
  let m' = Mind_eio.Pure.learn m ~learning in
  check int "learnings increased" (List.length m.learnings + 1) (List.length m'.learnings)

let test_pure_add_insight () =
  let m = make_test_mind () in
  let insight : Mind_eio.insight = {
    id = "new-i"; timestamp = 999.0; topic = "test";
    content = "insight"; depth = 1; actionable = false;
    related_insights = [];
  } in
  let m' = Mind_eio.Pure.add_insight m ~insight in
  check int "insights increased" (List.length m.insights + 1) (List.length m'.insights)

let test_pure_update_state () =
  let m = make_test_mind () in
  let m' = Mind_eio.Pure.update_state m ~state:`Reflecting in
  check bool "state updated" true (m'.self.current_state = `Reflecting)

let test_pure_monitor_no_anomaly () =
  let m = make_test_mind () in
  let anomaly = Mind_eio.Pure.monitor m in
  check (option unit) "no anomaly" None
    (match anomaly with Some _ -> Some () | None -> None)

let test_pure_reflect_few_learnings () =
  let m = make_test_mind () in
  (* With only 1 learning, should not generate insight *)
  let insight = Mind_eio.Pure.reflect m in
  check (option unit) "no insight" None
    (match insight with Some _ -> Some () | None -> None)

let test_pure_evaluate_action () =
  let m = make_test_mind () in
  let eval = Mind_eio.Pure.evaluate_action m ~action_description:"random action" in
  check string "target_id" "action" eval.target_id;
  let _ : float = eval.score in
  check bool "has score" true true

let test_pure_active_goals () =
  let m = make_test_mind () in
  let active = Mind_eio.Pure.active_goals m in
  check bool "has active goals" true (List.length active > 0);
  List.iter (fun (g : Mind_eio.goal) ->
    check bool "is active" true (g.status = `Active)
  ) active

let test_pure_self_summary () =
  let m = make_test_mind () in
  let summary = Mind_eio.Pure.self_summary m in
  check bool "nonempty summary" true (String.length summary > 0)

let test_pure_self_summary_contains_name () =
  let m = make_test_mind () in
  let summary = Mind_eio.Pure.self_summary m in
  check bool "contains name" true
    (try
      let _ = Str.search_forward (Str.regexp "test-agent") summary 0 in true
    with Not_found -> false)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Mind_eio Coverage" [
    "generate_id", [
      test_case "prefix" `Quick test_generate_id_prefix;
      test_case "underscore" `Quick test_generate_id_underscore;
      test_case "length" `Quick test_generate_id_length;
      test_case "unique" `Quick test_generate_id_unique;
    ];
    "self_model", [
      test_case "idle" `Quick test_self_model_idle;
      test_case "working" `Quick test_self_model_working;
      test_case "reflecting" `Quick test_self_model_reflecting;
      test_case "learning" `Quick test_self_model_learning;
    ];
    "self_model_to_json", [
      test_case "basic" `Quick test_self_model_to_json_basic;
      test_case "has id" `Quick test_self_model_to_json_has_id;
      test_case "state idle" `Quick test_self_model_to_json_state_idle;
      test_case "state working" `Quick test_self_model_to_json_state_working;
    ];
    "thought", [
      test_case "observation" `Quick test_thought_observation;
      test_case "question" `Quick test_thought_question;
      test_case "hypothesis" `Quick test_thought_hypothesis;
      test_case "conclusion" `Quick test_thought_conclusion;
      test_case "doubt" `Quick test_thought_doubt;
    ];
    "thought_to_json", [
      test_case "basic" `Quick test_thought_to_json_basic;
      test_case "type observation" `Quick test_thought_to_json_type_observation;
      test_case "type question" `Quick test_thought_to_json_type_question;
    ];
    "goal", [
      test_case "active" `Quick test_goal_active;
      test_case "paused" `Quick test_goal_paused;
      test_case "completed" `Quick test_goal_completed;
      test_case "abandoned" `Quick test_goal_abandoned;
    ];
    "goal_to_json", [
      test_case "basic" `Quick test_goal_to_json_basic;
      test_case "status active" `Quick test_goal_to_json_status_active;
      test_case "status completed" `Quick test_goal_to_json_status_completed;
      test_case "with parent" `Quick test_goal_to_json_with_parent;
      test_case "null parent" `Quick test_goal_to_json_null_parent;
    ];
    "anomaly", [
      test_case "low" `Quick test_anomaly_low;
      test_case "medium" `Quick test_anomaly_medium;
      test_case "high" `Quick test_anomaly_high;
      test_case "critical" `Quick test_anomaly_critical;
    ];
    "anomaly_to_json", [
      test_case "basic" `Quick test_anomaly_to_json_basic;
      test_case "severity low" `Quick test_anomaly_to_json_severity_low;
      test_case "severity critical" `Quick test_anomaly_to_json_severity_critical;
      test_case "with action" `Quick test_anomaly_to_json_with_action;
    ];
    "get_string", [
      test_case "present" `Quick test_get_string_present;
      test_case "missing" `Quick test_get_string_missing;
      test_case "wrong type" `Quick test_get_string_wrong_type;
      test_case "not assoc" `Quick test_get_string_not_assoc;
    ];
    "get_float", [
      test_case "present" `Quick test_get_float_present;
      test_case "from int" `Quick test_get_float_from_int;
      test_case "missing" `Quick test_get_float_missing;
    ];
    "get_int", [
      test_case "present" `Quick test_get_int_present;
      test_case "from float" `Quick test_get_int_from_float;
      test_case "missing" `Quick test_get_int_missing;
    ];
    "get_bool", [
      test_case "true" `Quick test_get_bool_true;
      test_case "false" `Quick test_get_bool_false;
      test_case "missing" `Quick test_get_bool_missing;
    ];
    "get_string_list", [
      test_case "present" `Quick test_get_string_list_present;
      test_case "empty" `Quick test_get_string_list_empty;
      test_case "mixed" `Quick test_get_string_list_mixed;
      test_case "missing" `Quick test_get_string_list_missing;
    ];
    "get_string_opt", [
      test_case "some" `Quick test_get_string_opt_some;
      test_case "null" `Quick test_get_string_opt_null;
      test_case "missing" `Quick test_get_string_opt_missing;
    ];
    "get_float_opt", [
      test_case "some" `Quick test_get_float_opt_some;
      test_case "from int" `Quick test_get_float_opt_from_int;
      test_case "null" `Quick test_get_float_opt_null;
    ];
    "insight", [
      test_case "basic" `Quick test_insight_basic;
      test_case "to_json" `Quick test_insight_to_json;
    ];
    "learning", [
      test_case "basic" `Quick test_learning_basic;
      test_case "to_json" `Quick test_learning_to_json;
    ];
    "value", [
      test_case "basic" `Quick test_value_basic;
      test_case "to_json" `Quick test_value_to_json;
    ];
    "meta_cognition", [
      test_case "basic" `Quick test_meta_cognition_basic;
    ];
    "evaluation", [
      test_case "basic" `Quick test_evaluation_basic;
    ];
    "self_model_of_json", [
      test_case "basic" `Quick test_self_model_of_json_basic;
      test_case "working" `Quick test_self_model_of_json_working;
      test_case "reflecting" `Quick test_self_model_of_json_reflecting;
      test_case "learning" `Quick test_self_model_of_json_learning;
    ];
    "thought_of_json", [
      test_case "observation" `Quick test_thought_of_json_observation;
      test_case "question" `Quick test_thought_of_json_question;
      test_case "hypothesis" `Quick test_thought_of_json_hypothesis;
      test_case "conclusion" `Quick test_thought_of_json_conclusion;
      test_case "doubt" `Quick test_thought_of_json_doubt;
    ];
    "goal_of_json", [
      test_case "active" `Quick test_goal_of_json_active;
      test_case "paused" `Quick test_goal_of_json_paused;
      test_case "completed" `Quick test_goal_of_json_completed;
      test_case "abandoned" `Quick test_goal_of_json_abandoned;
    ];
    "insight_of_json", [
      test_case "basic" `Quick test_insight_of_json_basic;
    ];
    "learning_of_json", [
      test_case "basic" `Quick test_learning_of_json_basic;
    ];
    "value_of_json", [
      test_case "basic" `Quick test_value_of_json_basic;
    ];
    "mind_json", [
      test_case "to_json returns assoc" `Quick test_mind_to_json_returns_assoc;
      test_case "to_json has self" `Quick test_mind_to_json_has_self;
      test_case "to_json has meta" `Quick test_mind_to_json_has_meta;
      test_case "roundtrip" `Quick test_mind_roundtrip;
      test_case "of_json_opt invalid" `Quick test_mind_of_json_opt_invalid;
      test_case "of_json_opt no self" `Quick test_mind_of_json_opt_no_self;
    ];
    "pure_think", [
      test_case "think" `Quick test_pure_think;
      test_case "create_thought" `Quick test_pure_create_thought;
      test_case "create_thought with options" `Quick test_pure_create_thought_with_options;
    ];
    "pure_goals", [
      test_case "add_goal" `Quick test_pure_add_goal;
      test_case "update_goal_progress" `Quick test_pure_update_goal_progress;
      test_case "clamp max" `Quick test_pure_update_goal_progress_clamp_max;
      test_case "clamp min" `Quick test_pure_update_goal_progress_clamp_min;
      test_case "complete_goal" `Quick test_pure_complete_goal;
      test_case "active_goals" `Quick test_pure_active_goals;
    ];
    "pure_values", [
      test_case "adopt_value" `Quick test_pure_adopt_value;
    ];
    "pure_learning", [
      test_case "learn" `Quick test_pure_learn;
      test_case "add_insight" `Quick test_pure_add_insight;
    ];
    "pure_state", [
      test_case "update_state" `Quick test_pure_update_state;
      test_case "monitor no anomaly" `Quick test_pure_monitor_no_anomaly;
      test_case "reflect few learnings" `Quick test_pure_reflect_few_learnings;
    ];
    "pure_evaluate", [
      test_case "evaluate_action" `Quick test_pure_evaluate_action;
      test_case "self_summary" `Quick test_pure_self_summary;
      test_case "summary contains name" `Quick test_pure_self_summary_contains_name;
    ];
  ]
