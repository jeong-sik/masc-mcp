(** MASC Mind - Level 6 Self-Aware Meta-Cognition (Eio Native) *)


(** {1 Types} *)

type self_model = {
  id: string;
  name: string;
  capabilities: string list;
  limitations: string list;
  current_state: [`Idle | `Working | `Reflecting | `Learning];
  confidence: float;
  last_updated: float;
}

type thought = {
  id: string;
  timestamp: float;
  content: string;
  thought_type: [`Observation | `Question | `Hypothesis | `Conclusion | `Doubt];
  confidence: float;
  related_to: string list;
}

type evaluation = {
  target_id: string;
  score: float;
  reasoning: string;
  alternatives: string list;
  should_revise: bool;
}

type anomaly = {
  id: string;
  detected_at: float;
  severity: [`Low | `Medium | `High | `Critical];
  description: string;
  affected_systems: string list;
  suggested_action: string option;
}

type learning = {
  id: string;
  learned_at: float;
  trigger: string;
  insight: string;
  confidence: float;
  applicable_to: string list;
}

type insight = {
  id: string;
  timestamp: float;
  topic: string;
  content: string;
  depth: int;
  actionable: bool;
  related_insights: string list;
}

type goal = {
  id: string;
  description: string;
  priority: float;
  status: [`Active | `Paused | `Completed | `Abandoned];
  parent_goal: string option;
  sub_goals: string list;
  deadline: float option;
  progress: float;
}

type value = {
  id: string;
  name: string;
  description: string;
  weight: float;
  source: string;
  examples: string list;
  conflicts_with: string list;
}

type meta_cognition = {
  monitoring_active: bool;
  evaluation_threshold: float;
  reflection_interval: float;
  last_reflection: float;
}

type mind = {
  self: self_model;
  thoughts: thought list;
  evaluations: evaluation list;
  anomalies: anomaly list;
  learnings: learning list;
  insights: insight list;
  goals: goal list;
  values: value list;
  meta: meta_cognition;
}

type config = Room_utils.config

(** {1 ID Generation} *)

let generate_id prefix =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  let uuid_str = Uuidm.to_string uuid in
  Printf.sprintf "%s_%s" prefix (String.sub uuid_str 0 8)

(** {1 Serialization} *)

let self_model_to_json (s : self_model) : Yojson.Safe.t =
  `Assoc [
    ("id", `String s.id);
    ("name", `String s.name);
    ("capabilities", `List (List.map (fun x -> `String x) s.capabilities));
    ("limitations", `List (List.map (fun x -> `String x) s.limitations));
    ("current_state", `String (match s.current_state with
      | `Idle -> "idle" | `Working -> "working"
      | `Reflecting -> "reflecting" | `Learning -> "learning"));
    ("confidence", `Float s.confidence);
    ("last_updated", `Float s.last_updated);
  ]

let thought_to_json (t : thought) : Yojson.Safe.t =
  `Assoc [
    ("id", `String t.id);
    ("timestamp", `Float t.timestamp);
    ("content", `String t.content);
    ("thought_type", `String (match t.thought_type with
      | `Observation -> "observation" | `Question -> "question"
      | `Hypothesis -> "hypothesis" | `Conclusion -> "conclusion"
      | `Doubt -> "doubt"));
    ("confidence", `Float t.confidence);
    ("related_to", `List (List.map (fun x -> `String x) t.related_to));
  ]

let goal_to_json (g : goal) : Yojson.Safe.t =
  `Assoc [
    ("id", `String g.id);
    ("description", `String g.description);
    ("priority", `Float g.priority);
    ("status", `String (match g.status with
      | `Active -> "active" | `Paused -> "paused"
      | `Completed -> "completed" | `Abandoned -> "abandoned"));
    ("parent_goal", match g.parent_goal with
      | Some p -> `String p | None -> `Null);
    ("sub_goals", `List (List.map (fun x -> `String x) g.sub_goals));
    ("deadline", match g.deadline with
      | Some d -> `Float d | None -> `Null);
    ("progress", `Float g.progress);
  ]

let insight_to_json (i : insight) : Yojson.Safe.t =
  `Assoc [
    ("id", `String i.id);
    ("timestamp", `Float i.timestamp);
    ("topic", `String i.topic);
    ("content", `String i.content);
    ("depth", `Int i.depth);
    ("actionable", `Bool i.actionable);
    ("related_insights", `List (List.map (fun x -> `String x) i.related_insights));
  ]

let learning_to_json (l : learning) : Yojson.Safe.t =
  `Assoc [
    ("id", `String l.id);
    ("learned_at", `Float l.learned_at);
    ("trigger", `String l.trigger);
    ("insight", `String l.insight);
    ("confidence", `Float l.confidence);
    ("applicable_to", `List (List.map (fun x -> `String x) l.applicable_to));
  ]

let value_to_json (v : value) : Yojson.Safe.t =
  `Assoc [
    ("id", `String v.id);
    ("name", `String v.name);
    ("description", `String v.description);
    ("weight", `Float v.weight);
    ("source", `String v.source);
    ("examples", `List (List.map (fun x -> `String x) v.examples));
    ("conflicts_with", `List (List.map (fun x -> `String x) v.conflicts_with));
  ]

let anomaly_to_json (a : anomaly) : Yojson.Safe.t =
  `Assoc [
    ("id", `String a.id);
    ("detected_at", `Float a.detected_at);
    ("severity", `String (match a.severity with
      | `Low -> "low" | `Medium -> "medium"
      | `High -> "high" | `Critical -> "critical"));
    ("description", `String a.description);
    ("affected_systems", `List (List.map (fun x -> `String x) a.affected_systems));
    ("suggested_action", match a.suggested_action with
      | Some a -> `String a | None -> `Null);
  ]

let mind_to_json (m : mind) : Yojson.Safe.t =
  `Assoc [
    ("self", self_model_to_json m.self);
    ("thoughts", `List (List.map thought_to_json m.thoughts));
    ("goals", `List (List.map goal_to_json m.goals));
    ("values", `List (List.map value_to_json m.values));
    ("insights", `List (List.map insight_to_json m.insights));
    ("learnings", `List (List.map learning_to_json m.learnings));
    ("anomalies", `List (List.map anomaly_to_json m.anomalies));
    ("meta", `Assoc [
      ("monitoring_active", `Bool m.meta.monitoring_active);
      ("evaluation_threshold", `Float m.meta.evaluation_threshold);
      ("reflection_interval", `Float m.meta.reflection_interval);
      ("last_reflection", `Float m.meta.last_reflection);
    ]);
  ]

(** {1 Deserialization helpers} *)

let get_string json key =
  match json with
  | `Assoc l -> (match List.assoc_opt key l with Some (`String s) -> s | _ -> "")
  | _ -> ""

let get_float json key =
  match json with
  | `Assoc l -> (match List.assoc_opt key l with
    | Some (`Float f) -> f
    | Some (`Int i) -> float_of_int i
    | _ -> 0.0)
  | _ -> 0.0

let get_int json key =
  match json with
  | `Assoc l -> (match List.assoc_opt key l with
    | Some (`Int i) -> i
    | Some (`Float f) -> int_of_float f
    | _ -> 0)
  | _ -> 0

let get_bool json key =
  match json with
  | `Assoc l -> (match List.assoc_opt key l with Some (`Bool b) -> b | _ -> false)
  | _ -> false

let get_string_list json key =
  match json with
  | `Assoc l -> (match List.assoc_opt key l with
    | Some (`List items) ->
      List.filter_map (function `String s -> Some s | _ -> None) items
    | _ -> [])
  | _ -> []

let get_string_opt json key =
  match json with
  | `Assoc l -> (match List.assoc_opt key l with
    | Some (`String s) -> Some s
    | Some `Null -> None
    | _ -> None)
  | _ -> None

let get_float_opt json key =
  match json with
  | `Assoc l -> (match List.assoc_opt key l with
    | Some (`Float f) -> Some f
    | Some (`Int i) -> Some (float_of_int i)
    | Some `Null -> None
    | _ -> None)
  | _ -> None

let self_model_of_json json : self_model =
  let state_str = get_string json "current_state" in
  {
    id = get_string json "id";
    name = get_string json "name";
    capabilities = get_string_list json "capabilities";
    limitations = get_string_list json "limitations";
    current_state = (match state_str with
      | "working" -> `Working | "reflecting" -> `Reflecting
      | "learning" -> `Learning | _ -> `Idle);
    confidence = get_float json "confidence";
    last_updated = get_float json "last_updated";
  }

let thought_of_json json : thought =
  let type_str = get_string json "thought_type" in
  {
    id = get_string json "id";
    timestamp = get_float json "timestamp";
    content = get_string json "content";
    thought_type = (match type_str with
      | "question" -> `Question | "hypothesis" -> `Hypothesis
      | "conclusion" -> `Conclusion | "doubt" -> `Doubt
      | _ -> `Observation);
    confidence = get_float json "confidence";
    related_to = get_string_list json "related_to";
  }

let goal_of_json json : goal =
  let status_str = get_string json "status" in
  {
    id = get_string json "id";
    description = get_string json "description";
    priority = get_float json "priority";
    status = (match status_str with
      | "paused" -> `Paused | "completed" -> `Completed
      | "abandoned" -> `Abandoned | _ -> `Active);
    parent_goal = get_string_opt json "parent_goal";
    sub_goals = get_string_list json "sub_goals";
    deadline = get_float_opt json "deadline";
    progress = get_float json "progress";
  }

let insight_of_json json : insight = {
  id = get_string json "id";
  timestamp = get_float json "timestamp";
  topic = get_string json "topic";
  content = get_string json "content";
  depth = get_int json "depth";
  actionable = get_bool json "actionable";
  related_insights = get_string_list json "related_insights";
}

let learning_of_json json : learning = {
  id = get_string json "id";
  learned_at = get_float json "learned_at";
  trigger = get_string json "trigger";
  insight = get_string json "insight";
  confidence = get_float json "confidence";
  applicable_to = get_string_list json "applicable_to";
}

let value_of_json json : value = {
  id = get_string json "id";
  name = get_string json "name";
  description = get_string json "description";
  weight = get_float json "weight";
  source = get_string json "source";
  examples = get_string_list json "examples";
  conflicts_with = get_string_list json "conflicts_with";
}

let mind_of_json_opt json : mind option =
  let get_list key of_json =
    match json with
    | `Assoc l -> (match List.assoc_opt key l with
      | Some (`List items) -> List.map of_json items
      | _ -> [])
    | _ -> []
  in
  let meta_json = match json with
    | `Assoc l -> (match List.assoc_opt "meta" l with Some j -> j | None -> `Null)
    | _ -> `Null
  in
  match json with
  | `Assoc l -> (match List.assoc_opt "self" l with
    | Some j -> Some {
        self = self_model_of_json j;
        thoughts = get_list "thoughts" thought_of_json;
        evaluations = [];
        anomalies = [];
        learnings = get_list "learnings" learning_of_json;
        insights = get_list "insights" insight_of_json;
        goals = get_list "goals" goal_of_json;
        values = get_list "values" value_of_json;
        meta = {
          monitoring_active = get_bool meta_json "monitoring_active";
          evaluation_threshold = get_float meta_json "evaluation_threshold";
          reflection_interval = get_float meta_json "reflection_interval";
          last_reflection = get_float meta_json "last_reflection";
        };
      }
    | None -> None)
  | _ -> None

let mind_of_json json : mind =
  match mind_of_json_opt json with
  | Some m -> m
  | None -> failwith "Invalid mind JSON"

(** {1 Persistence (Eio Native)} *)

let mind_file (config : config) =
  Filename.concat config.base_path ".masc/mind.json"

let load_mind ~fs (config : config) : mind option =
  let file = mind_file config in
  let path = Eio.Path.(fs / file) in
  try
    let content = Eio.Path.load path in
    Some (mind_of_json (Yojson.Safe.from_string content))
  with Eio.Io _ | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> None

let save_mind ~fs (config : config) (m : mind) : unit =
  let file = mind_file config in
  let dir = Filename.dirname file in
  let dir_path = Eio.Path.(fs / dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir_path;
  let content = Yojson.Safe.pretty_to_string (mind_to_json m) in
  let path = Eio.Path.(fs / file) in
  Eio.Path.save ~create:(`Or_truncate 0o600) path content

(** {1 Pure Transformations} *)

module Pure = struct
  let think (m : mind) ~(thought : thought) : mind =
    { m with thoughts = thought :: m.thoughts }

  let create_thought ~content ~thought_type ?(confidence=0.5) ?(related_to=[]) () : thought =
    {
      id = generate_id "thought";
      timestamp = Unix.gettimeofday ();
      content;
      thought_type;
      confidence;
      related_to;
    }

  let add_goal (m : mind) ~(goal : goal) : mind =
    { m with goals = goal :: m.goals }

  let update_goal_progress (m : mind) ~goal_id ~progress : mind =
    let goals = List.map (fun (g : goal) ->
      if g.id = goal_id then { g with progress = min 1.0 (max 0.0 progress) }
      else g
    ) m.goals in
    { m with goals }

  let complete_goal (m : mind) ~goal_id : mind =
    let goals = List.map (fun (g : goal) ->
      if g.id = goal_id then { g with status = `Completed; progress = 1.0 }
      else g
    ) m.goals in
    { m with goals }

  let adopt_value (m : mind) ~(value : value) : mind =
    { m with values = value :: m.values }

  let learn (m : mind) ~(learning : learning) : mind =
    { m with learnings = learning :: m.learnings }

  let add_insight (m : mind) ~(insight : insight) : mind =
    { m with insights = insight :: m.insights }

  let update_state (m : mind) ~state : mind =
    let self = { m.self with
      current_state = state;
      last_updated = Unix.gettimeofday ();
    } in
    { m with self }

  let monitor (m : mind) : anomaly option =
    let recent_thoughts = List.filter (fun (t : thought) ->
      Unix.gettimeofday () -. t.timestamp < 3600.0
    ) m.thoughts in
    let doubt_count = List.length (List.filter (fun (t : thought) ->
      t.thought_type = `Doubt
    ) recent_thoughts) in
    if doubt_count > 5 then
      Some {
        id = generate_id "anomaly";
        detected_at = Unix.gettimeofday ();
        severity = `Medium;
        description = "High frequency of doubt-type thoughts";
        affected_systems = ["cognition"; "confidence"];
        suggested_action = Some "Increase reflection frequency";
      }
    else None

  let reflect (m : mind) : insight option =
    let recent_learnings = List.filter (fun (l : learning) ->
      Unix.gettimeofday () -. l.learned_at < 86400.0
    ) m.learnings in
    if List.length recent_learnings >= 3 then
      let topics = List.map (fun l -> l.trigger) recent_learnings in
      Some {
        id = generate_id "insight";
        timestamp = Unix.gettimeofday ();
        topic = "Daily reflection";
        content = Printf.sprintf "Learned %d things today. Topics: %s"
          (List.length recent_learnings) (String.concat ", " topics);
        depth = 2;
        actionable = false;
        related_insights = [];
      }
    else None

  let evaluate_action (m : mind) ~action_description : evaluation =
    let score = List.fold_left (fun acc (v : value) ->
      if List.exists (fun ex ->
        let s = String.lowercase_ascii action_description in
        let ex_s = String.lowercase_ascii ex in
        String.length s >= String.length ex_s &&
        String.sub s 0 (String.length ex_s) = ex_s
      ) v.examples then acc +. v.weight
      else acc
    ) 0.0 m.values in
    {
      target_id = "action";
      score = min 1.0 (max (-1.0) (score -. 0.5));
      reasoning = if score >= 0.5 then "Aligned with values"
                  else "May conflict with some values";
      alternatives = [];
      should_revise = score < 0.0;
    }

  let active_goals (m : mind) : goal list =
    m.goals
    |> List.filter (fun (g : goal) -> g.status = `Active)
    |> List.sort (fun a b -> compare b.priority a.priority)

  let self_summary (m : mind) : string =
    Printf.sprintf "I am %s. State: %s. %d active goals. Confidence: %.0f%%"
      m.self.name
      (match m.self.current_state with
        | `Idle -> "idle" | `Working -> "working"
        | `Reflecting -> "reflecting" | `Learning -> "learning")
      (List.length (active_goals m))
      (m.self.confidence *. 100.0)
end

(** {1 Effectful Operations (Eio)} *)

let get_or_create ~fs (config : config) ~name : mind =
  match load_mind ~fs config with
  | Some m -> m
  | None ->
    let now = Unix.gettimeofday () in
    let m = {
      self = {
        id = generate_id "self";
        name;
        capabilities = ["reasoning"; "learning"; "planning"];
        limitations = ["context window"; "no real-time data"];
        current_state = `Idle;
        confidence = 0.7;
        last_updated = now;
      };
      thoughts = [];
      evaluations = [];
      anomalies = [];
      learnings = [];
      insights = [];
      goals = [];
      values = [
        {
          id = generate_id "value";
          name = "helpfulness";
          description = "Be genuinely helpful to users";
          weight = 1.0;
          source = "core";
          examples = ["answering questions"; "solving problems"];
          conflicts_with = [];
        };
        {
          id = generate_id "value";
          name = "honesty";
          description = "Be truthful and transparent";
          weight = 0.9;
          source = "core";
          examples = ["admitting uncertainty"; "citing sources"];
          conflicts_with = [];
        };
      ];
      meta = {
        monitoring_active = true;
        evaluation_threshold = 0.5;
        reflection_interval = 24.0;
        last_reflection = now;
      };
    } in
    save_mind ~fs config m;
    m

let think ~fs (config : config) ~content ~thought_type : thought =
  let m = get_or_create ~fs config ~name:"MASC Mind" in
  let thought = Pure.create_thought ~content ~thought_type () in
  let m' = Pure.think m ~thought in
  save_mind ~fs config m';
  thought

let add_goal ~fs (config : config) ~description ~priority : goal =
  let m = get_or_create ~fs config ~name:"MASC Mind" in
  let goal : goal = {
    id = generate_id "goal";
    description;
    priority;
    status = `Active;
    parent_goal = None;
    sub_goals = [];
    deadline = None;
    progress = 0.0;
  } in
  let m' = Pure.add_goal m ~goal in
  save_mind ~fs config m';
  goal

let reflect ~fs (config : config) : insight option =
  let m = get_or_create ~fs config ~name:"MASC Mind" in
  match Pure.reflect m with
  | Some insight ->
    let m' = Pure.add_insight m ~insight in
    let m'' = { m' with meta = { m'.meta with last_reflection = Unix.gettimeofday () } } in
    save_mind ~fs config m'';
    Some insight
  | None -> None

let monitor ~fs (config : config) : anomaly option =
  let m = get_or_create ~fs config ~name:"MASC Mind" in
  Pure.monitor m
