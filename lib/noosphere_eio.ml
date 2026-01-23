(** MASC Noosphere - Level 7 Collective Intelligence (Eio Native) *)


(** {1 Types} *)

type connection = {
  from_mind: string;
  to_mind: string;
  strength: float;
  established_at: float;
  last_interaction: float;
}

type shared_belief = {
  id: string;
  content: string;
  confidence: float;
  supporters: string list;
  created_at: float;
  last_reinforced: float;
}

type emergent_goal = {
  id: string;
  description: string;
  emerged_at: float;
  contributing_minds: string list;
  strength: float;
  status: [`Emerging | `Active | `Fading | `Dissolved];
}

type collective_insight = {
  id: string;
  topic: string;
  synthesis: string;
  contributing_insights: string list;
  emerged_at: float;
  depth: int;
}

type global_state = {
  coherence: float;
  activity_level: float;
  dominant_mood: string;
  focus_topic: string option;
  last_sync: float;
}

type noosphere = {
  id: string;
  name: string;
  minds: string list;
  connections: connection list;
  beliefs: shared_belief list;
  goals: emergent_goal list;
  insights: collective_insight list;
  state: global_state;
  created_at: float;
}

type config = Room_utils.config

(** {1 ID Generation} *)

let generate_id prefix =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  let uuid_str = Uuidm.to_string uuid in
  Printf.sprintf "%s_%s" prefix (String.sub uuid_str 0 8)

(** {1 Serialization} *)

let connection_to_json (c : connection) : Yojson.Safe.t =
  `Assoc [
    ("from_mind", `String c.from_mind);
    ("to_mind", `String c.to_mind);
    ("strength", `Float c.strength);
    ("established_at", `Float c.established_at);
    ("last_interaction", `Float c.last_interaction);
  ]

let shared_belief_to_json (b : shared_belief) : Yojson.Safe.t =
  `Assoc [
    ("id", `String b.id);
    ("content", `String b.content);
    ("confidence", `Float b.confidence);
    ("supporters", `List (List.map (fun s -> `String s) b.supporters));
    ("created_at", `Float b.created_at);
    ("last_reinforced", `Float b.last_reinforced);
  ]

let emergent_goal_to_json (g : emergent_goal) : Yojson.Safe.t =
  `Assoc [
    ("id", `String g.id);
    ("description", `String g.description);
    ("emerged_at", `Float g.emerged_at);
    ("contributing_minds", `List (List.map (fun s -> `String s) g.contributing_minds));
    ("strength", `Float g.strength);
    ("status", `String (match g.status with
      | `Emerging -> "emerging" | `Active -> "active"
      | `Fading -> "fading" | `Dissolved -> "dissolved"));
  ]

let collective_insight_to_json (i : collective_insight) : Yojson.Safe.t =
  `Assoc [
    ("id", `String i.id);
    ("topic", `String i.topic);
    ("synthesis", `String i.synthesis);
    ("contributing_insights", `List (List.map (fun s -> `String s) i.contributing_insights));
    ("emerged_at", `Float i.emerged_at);
    ("depth", `Int i.depth);
  ]

let global_state_to_json (s : global_state) : Yojson.Safe.t =
  `Assoc [
    ("coherence", `Float s.coherence);
    ("activity_level", `Float s.activity_level);
    ("dominant_mood", `String s.dominant_mood);
    ("focus_topic", match s.focus_topic with Some t -> `String t | None -> `Null);
    ("last_sync", `Float s.last_sync);
  ]

let noosphere_to_json (n : noosphere) : Yojson.Safe.t =
  `Assoc [
    ("id", `String n.id);
    ("name", `String n.name);
    ("minds", `List (List.map (fun s -> `String s) n.minds));
    ("connections", `List (List.map connection_to_json n.connections));
    ("beliefs", `List (List.map shared_belief_to_json n.beliefs));
    ("goals", `List (List.map emergent_goal_to_json n.goals));
    ("insights", `List (List.map collective_insight_to_json n.insights));
    ("state", global_state_to_json n.state);
    ("created_at", `Float n.created_at);
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

let get_string_list json key =
  match json with
  | `Assoc l -> (match List.assoc_opt key l with
    | Some (`List items) -> List.filter_map (function `String s -> Some s | _ -> None) items
    | _ -> [])
  | _ -> []

let get_string_opt json key =
  match json with
  | `Assoc l -> (match List.assoc_opt key l with
    | Some (`String s) -> Some s
    | _ -> None)
  | _ -> None

let connection_of_json json : connection = {
  from_mind = get_string json "from_mind";
  to_mind = get_string json "to_mind";
  strength = get_float json "strength";
  established_at = get_float json "established_at";
  last_interaction = get_float json "last_interaction";
}

let shared_belief_of_json json : shared_belief = {
  id = get_string json "id";
  content = get_string json "content";
  confidence = get_float json "confidence";
  supporters = get_string_list json "supporters";
  created_at = get_float json "created_at";
  last_reinforced = get_float json "last_reinforced";
}

let emergent_goal_of_json json : emergent_goal =
  let status_str = get_string json "status" in
  {
    id = get_string json "id";
    description = get_string json "description";
    emerged_at = get_float json "emerged_at";
    contributing_minds = get_string_list json "contributing_minds";
    strength = get_float json "strength";
    status = (match status_str with
      | "active" -> `Active | "fading" -> `Fading
      | "dissolved" -> `Dissolved | _ -> `Emerging);
  }

let collective_insight_of_json json : collective_insight = {
  id = get_string json "id";
  topic = get_string json "topic";
  synthesis = get_string json "synthesis";
  contributing_insights = get_string_list json "contributing_insights";
  emerged_at = get_float json "emerged_at";
  depth = get_int json "depth";
}

let global_state_of_json json : global_state = {
  coherence = get_float json "coherence";
  activity_level = get_float json "activity_level";
  dominant_mood = get_string json "dominant_mood";
  focus_topic = get_string_opt json "focus_topic";
  last_sync = get_float json "last_sync";
}

let noosphere_of_json json : noosphere =
  let get_list key of_json =
    match json with
    | `Assoc l -> (match List.assoc_opt key l with
      | Some (`List items) -> List.map of_json items
      | _ -> [])
    | _ -> []
  in
  let state_json = match json with
    | `Assoc l -> (match List.assoc_opt "state" l with Some j -> j | None -> `Null)
    | _ -> `Null
  in
  {
    id = get_string json "id";
    name = get_string json "name";
    minds = get_string_list json "minds";
    connections = get_list "connections" connection_of_json;
    beliefs = get_list "beliefs" shared_belief_of_json;
    goals = get_list "goals" emergent_goal_of_json;
    insights = get_list "insights" collective_insight_of_json;
    state = global_state_of_json state_json;
    created_at = get_float json "created_at";
  }

(** {1 Persistence (Eio Native)} *)

let noosphere_file (config : config) =
  Filename.concat config.base_path ".masc/noosphere.json"

let load_noosphere ~fs (config : config) : noosphere option =
  let file = noosphere_file config in
  let path = Eio.Path.(fs / file) in
  try
    let content = Eio.Path.load path in
    Some (noosphere_of_json (Yojson.Safe.from_string content))
  with _ -> None

let save_noosphere ~fs (config : config) (n : noosphere) : unit =
  let file = noosphere_file config in
  let dir = Filename.dirname file in
  let dir_path = Eio.Path.(fs / dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir_path;
  let content = Yojson.Safe.pretty_to_string (noosphere_to_json n) in
  let path = Eio.Path.(fs / file) in
  Eio.Path.save ~create:(`Or_truncate 0o600) path content

(** {1 Pure Transformations} *)

module Pure = struct
  let add_mind (n : noosphere) ~mind_id : noosphere =
    if List.mem mind_id n.minds then n
    else { n with minds = mind_id :: n.minds }

  let remove_mind (n : noosphere) ~mind_id : noosphere =
    { n with
      minds = List.filter (fun m -> m <> mind_id) n.minds;
      connections = List.filter (fun c ->
        c.from_mind <> mind_id && c.to_mind <> mind_id
      ) n.connections;
    }

  let connect (n : noosphere) ~from_mind ~to_mind ?(strength=0.5) () : noosphere =
    let now = Unix.gettimeofday () in
    let existing = List.exists (fun c -> c.from_mind = from_mind && c.to_mind = to_mind) n.connections in
    if existing then
      let connections = List.map (fun conn ->
        if conn.from_mind = from_mind && conn.to_mind = to_mind then
          { conn with strength = min 1.0 (conn.strength +. 0.1); last_interaction = now }
        else conn
      ) n.connections in
      { n with connections }
    else
      let conn = { from_mind; to_mind; strength; established_at = now; last_interaction = now } in
      { n with connections = conn :: n.connections }

  let share_belief (n : noosphere) ~content ~mind_id : noosphere =
    let now = Unix.gettimeofday () in
    let existing = List.find_opt (fun (b : shared_belief) ->
      String.lowercase_ascii b.content = String.lowercase_ascii content
    ) n.beliefs in
    match existing with
    | Some b ->
      let beliefs = List.map (fun (belief : shared_belief) ->
        if belief.id = b.id then
          { belief with
            supporters = if List.mem mind_id belief.supporters then belief.supporters else mind_id :: belief.supporters;
            confidence = min 1.0 (belief.confidence +. 0.1);
            last_reinforced = now;
          }
        else belief
      ) n.beliefs in
      { n with beliefs }
    | None ->
      let belief = {
        id = generate_id "belief"; content; confidence = 0.5;
        supporters = [mind_id]; created_at = now; last_reinforced = now;
      } in
      { n with beliefs = belief :: n.beliefs }

  let synthesize (_n : noosphere) ~topic ~contributions : collective_insight option =
    if List.length contributions < 2 then None
    else
      Some {
        id = generate_id "collective_insight";
        topic;
        synthesis = Printf.sprintf "Collective understanding of %s from %d minds" topic (List.length contributions);
        contributing_insights = contributions;
        emerged_at = Unix.gettimeofday ();
        depth = min 10 (List.length contributions + 2);
      }

  let calculate_coherence (n : noosphere) : float =
    if List.length n.minds < 2 then 1.0
    else
      let total_possible = List.length n.minds * (List.length n.minds - 1) in
      let actual_connections = List.length n.connections in
      let avg_strength = if actual_connections = 0 then 0.0
        else List.fold_left (fun acc (c : connection) -> acc +. c.strength) 0.0 n.connections
             /. float_of_int actual_connections in
      let connectivity = float_of_int actual_connections /. float_of_int total_possible in
      connectivity *. avg_strength

  let sync_state (n : noosphere) : noosphere =
    let coherence = calculate_coherence n in
    let activity = if List.length n.connections = 0 then 0.0
      else
        let recent = List.filter (fun c -> Unix.gettimeofday () -. c.last_interaction < 3600.0) n.connections in
        float_of_int (List.length recent) /. float_of_int (List.length n.connections)
    in
    let state = { n.state with coherence; activity_level = activity; last_sync = Unix.gettimeofday () } in
    { n with state }
end

(** {1 Effectful Operations (Eio)} *)

let get_or_create ~fs (config : config) ~name : noosphere =
  match load_noosphere ~fs config with
  | Some n -> n
  | None ->
    let now = Unix.gettimeofday () in
    let n = {
      id = generate_id "noosphere"; name; minds = []; connections = []; beliefs = [];
      goals = []; insights = []; created_at = now;
      state = { coherence = 1.0; activity_level = 0.0; dominant_mood = "neutral"; focus_topic = None; last_sync = now };
    } in
    save_noosphere ~fs config n;
    n

let join ~fs (config : config) ~mind_id : unit =
  let n = get_or_create ~fs config ~name:"MASC Noosphere" in
  let n' = Pure.add_mind n ~mind_id |> Pure.sync_state in
  save_noosphere ~fs config n'

let share_belief ~fs (config : config) ~content ~mind_id : shared_belief =
  let n = get_or_create ~fs config ~name:"MASC Noosphere" in
  let n' = Pure.share_belief n ~content ~mind_id in
  save_noosphere ~fs config n';
  List.find (fun (b : shared_belief) -> String.lowercase_ascii b.content = String.lowercase_ascii content) n'.beliefs

let synthesize ~fs (config : config) ~topic ~contributions : collective_insight option =
  let n = get_or_create ~fs config ~name:"MASC Noosphere" in
  match Pure.synthesize n ~topic ~contributions with
  | Some insight ->
    let n' = { n with insights = insight :: n.insights } in
    save_noosphere ~fs config n';
    Some insight
  | None -> None
