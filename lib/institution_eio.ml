(** MASC Institution - Level 5 Persistent Collective Memory (Eio Native) *)

(** {1 Types} *)

type episode = {
  id: string;
  timestamp: float;
  participants: string list;
  event_type: string;
  summary: string;
  outcome: [`Success | `Failure | `Partial];
  learnings: string list;
  context: (string * string) list;
}

type knowledge = {
  id: string;
  topic: string;
  content: string;
  confidence: float;
  source: string;
  created_at: float;
  last_verified: float;
  references: string list;
}

type pattern = {
  id: string;
  name: string;
  description: string;
  trigger: string;
  steps: string list;
  success_rate: float;
  usage_count: int;
  last_used: float;
  evolved_from: string option;
}

type cultural_value = {
  id: string;
  name: string;
  description: string;
  weight: float;
  examples: string list;
  anti_patterns: string list;
  adopted_at: float;
}

type succession_policy = {
  onboarding_steps: string list;
  required_knowledge: string list;
  mentor_assignment: [`Random | `Best_fit | `Round_robin];
  probation_period: float;
  graduation_criteria: string list;
}

type long_term_memory = {
  episodic: episode list;
  semantic: knowledge list;
  procedural: pattern list;
}

type identity = {
  id: string;
  name: string;
  mission: string;
  founded_at: float;
  generation: int;
}

type institution = {
  identity: identity;
  memory: long_term_memory;
  culture: cultural_value list;
  succession: succession_policy;
  current_agents: string list;
  alumni: string list;
}

type config = Room_utils.config

(** {1 Default Values} *)

let default_succession () : succession_policy = {
  onboarding_steps = [
    "Read mission statement";
    "Review recent episodes";
    "Learn top 5 patterns";
    "Shadow mentor for 3 tasks";
  ];
  required_knowledge = [];
  mentor_assignment = `Best_fit;
  probation_period = 24.0;
  graduation_criteria = [
    "Complete 3 tasks successfully";
    "Demonstrate 2 patterns";
    "Receive mentor approval";
  ];
}

let create_institution ~name ~mission () : institution =
  let now = Unix.gettimeofday () in
  {
    identity = {
      id = Printf.sprintf "inst-%d" (Level4_config.random_int 100000);
      name;
      mission;
      founded_at = now;
      generation = 0;
    };
    memory = { episodic = []; semantic = []; procedural = [] };
    culture = [];
    succession = default_succession ();
    current_agents = [];
    alumni = [];
  }

(** {1 Persistence (Eio Native)} *)

(** {1 Serialization (Helpers)} *)

let outcome_to_string = function `Success -> "success" | `Failure -> "failure" | `Partial -> "partial"
let outcome_of_string = function "success" -> `Success | "failure" -> `Failure | _ -> `Partial
let mentor_to_string = function `Random -> "random" | `Best_fit -> "best_fit" | `Round_robin -> "round_robin"
let mentor_of_string = function "random" -> `Random | "best_fit" -> `Best_fit | "round_robin" -> `Round_robin | _ -> `Best_fit

let rec episode_to_json (e : episode) : Yojson.Safe.t =
  `Assoc [
    ("id", `String e.id);
    ("timestamp", `Float e.timestamp);
    ("participants", `List (List.map (fun p -> `String p) e.participants));
    ("event_type", `String e.event_type);
    ("summary", `String e.summary);
    ("outcome", `String (outcome_to_string e.outcome));
    ("learnings", `List (List.map (fun l -> `String l) e.learnings));
    ("context", `Assoc (List.map (fun (k, v) -> (k, `String v)) e.context));
  ]

and episode_of_json json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    timestamp = json |> member "timestamp" |> to_float;
    participants = json |> member "participants" |> to_list |> List.map to_string;
    event_type = json |> member "event_type" |> to_string;
    summary = json |> member "summary" |> to_string;
    outcome = json |> member "outcome" |> to_string |> outcome_of_string;
    learnings = json |> member "learnings" |> to_list |> List.map to_string;
    context = json |> member "context" |> to_assoc |> List.map (fun (k, v) -> (k, to_string v));
  }

and knowledge_to_json (k : knowledge) : Yojson.Safe.t =
  `Assoc [
    ("id", `String k.id);
    ("topic", `String k.topic);
    ("content", `String k.content);
    ("confidence", `Float k.confidence);
    ("source", `String k.source);
    ("created_at", `Float k.created_at);
    ("last_verified", `Float k.last_verified);
    ("references", `List (List.map (fun r -> `String r) k.references));
  ]

and knowledge_of_json json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    topic = json |> member "topic" |> to_string;
    content = json |> member "content" |> to_string;
    confidence = json |> member "confidence" |> to_float;
    source = json |> member "source" |> to_string;
    created_at = json |> member "created_at" |> to_float;
    last_verified = json |> member "last_verified" |> to_float;
    references = json |> member "references" |> to_list |> List.map to_string;
  }

and pattern_to_json (p : pattern) : Yojson.Safe.t =
  `Assoc [
    ("id", `String p.id);
    ("name", `String p.name);
    ("description", `String p.description);
    ("trigger", `String p.trigger);
    ("steps", `List (List.map (fun s -> `String s) p.steps));
    ("success_rate", `Float p.success_rate);
    ("usage_count", `Int p.usage_count);
    ("last_used", `Float p.last_used);
    ("evolved_from", match p.evolved_from with Some e -> `String e | None -> `Null);
  ]

and pattern_of_json json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    trigger = json |> member "trigger" |> to_string;
    steps = json |> member "steps" |> to_list |> List.map to_string;
    success_rate = json |> member "success_rate" |> to_float;
    usage_count = json |> member "usage_count" |> to_int;
    last_used = json |> member "last_used" |> to_float;
    evolved_from = json |> member "evolved_from" |> to_string_option;
  }

and cultural_value_to_json (c : cultural_value) : Yojson.Safe.t =
  `Assoc [
    ("id", `String c.id);
    ("name", `String c.name);
    ("description", `String c.description);
    ("weight", `Float c.weight);
    ("examples", `List (List.map (fun e -> `String e) c.examples));
    ("anti_patterns", `List (List.map (fun a -> `String a) c.anti_patterns));
    ("adopted_at", `Float c.adopted_at);
  ]

and cultural_value_of_json json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    weight = json |> member "weight" |> to_float;
    examples = json |> member "examples" |> to_list |> List.map to_string;
    anti_patterns = json |> member "anti_patterns" |> to_list |> List.map to_string;
    adopted_at = json |> member "adopted_at" |> to_float;
  }

and succession_to_json (s : succession_policy) : Yojson.Safe.t =
  `Assoc [
    ("onboarding_steps", `List (List.map (fun s -> `String s) s.onboarding_steps));
    ("required_knowledge", `List (List.map (fun k -> `String k) s.required_knowledge));
    ("mentor_assignment", `String (mentor_to_string s.mentor_assignment));
    ("probation_period", `Float s.probation_period);
    ("graduation_criteria", `List (List.map (fun c -> `String c) s.graduation_criteria));
  ]

and succession_of_json json =
  let open Yojson.Safe.Util in
  {
    onboarding_steps = json |> member "onboarding_steps" |> to_list |> List.map to_string;
    required_knowledge = json |> member "required_knowledge" |> to_list |> List.map to_string;
    mentor_assignment = json |> member "mentor_assignment" |> to_string |> mentor_of_string;
    probation_period = json |> member "probation_period" |> to_float;
    graduation_criteria = json |> member "graduation_criteria" |> to_list |> List.map to_string;
  }

and identity_to_json (i : identity) : Yojson.Safe.t =
  `Assoc [
    ("id", `String i.id);
    ("name", `String i.name);
    ("mission", `String i.mission);
    ("founded_at", `Float i.founded_at);
    ("generation", `Int i.generation);
  ]

and identity_of_json json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    mission = json |> member "mission" |> to_string;
    founded_at = json |> member "founded_at" |> to_float;
    generation = json |> member "generation" |> to_int;
  }

and memory_to_json (m : long_term_memory) : Yojson.Safe.t =
  `Assoc [
    ("episodic", `List (List.map episode_to_json m.episodic));
    ("semantic", `List (List.map knowledge_to_json m.semantic));
    ("procedural", `List (List.map pattern_to_json m.procedural));
  ]

and memory_of_json json =
  let open Yojson.Safe.Util in
  {
    episodic = json |> member "episodic" |> to_list |> List.map episode_of_json;
    semantic = json |> member "semantic" |> to_list |> List.map knowledge_of_json;
    procedural = json |> member "procedural" |> to_list |> List.map pattern_of_json;
  }

and institution_to_json (inst : institution) : Yojson.Safe.t =
  `Assoc [
    ("identity", identity_to_json inst.identity);
    ("memory", memory_to_json inst.memory);
    ("culture", `List (List.map cultural_value_to_json inst.culture));
    ("succession", succession_to_json inst.succession);
    ("current_agents", `List (List.map (fun a -> `String a) inst.current_agents));
    ("alumni", `List (List.map (fun a -> `String a) inst.alumni));
  ]

and institution_of_json json =
  let open Yojson.Safe.Util in
  {
    identity = json |> member "identity" |> identity_of_json;
    memory = json |> member "memory" |> memory_of_json;
    culture = json |> member "culture" |> to_list |> List.map cultural_value_of_json;
    succession = json |> member "succession" |> succession_of_json;
    current_agents = json |> member "current_agents" |> to_list |> List.map to_string;
    alumni = json |> member "alumni" |> to_list |> List.map to_string;
  }

(** {1 Persistence (Eio Native)} *)

let institution_file (config : config) =
  Filename.concat config.base_path ".masc/institution.json"

let load_institution ~fs (config : config) : institution option =
  let file = institution_file config in
  let path = Eio.Path.(fs / file) in
  try
    let content = Eio.Path.load path in
    let json = Yojson.Safe.from_string content in
    Some (institution_of_json json)
  with Eio.Io _ | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> None

let save_institution ~fs (config : config) (inst : institution) =
  let file = institution_file config in
  let path = Eio.Path.(fs / file) in
  let dir = Filename.dirname file in
  let dir_path = Eio.Path.(fs / dir) in
  (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir_path
   with Eio.Io _ as e -> Eio.traceln "[WARN] mkdirs %s: %s" dir (Printexc.to_string e));
  let json = institution_to_json inst in
  let content = Yojson.Safe.pretty_to_string json in
  Eio.Path.save ~create:(`Or_truncate 0o644) path content

(** {1 Pure Transformations} *)

module Pure = struct
  let record_episode inst ~episode =
    let memory = { inst.memory with episodic = episode :: inst.memory.episodic } in
    { inst with memory }

  let add_knowledge inst ~knowledge =
    let memory = { inst.memory with semantic = knowledge :: inst.memory.semantic } in
    { inst with memory }

  let add_pattern inst ~pattern =
    let memory = { inst.memory with procedural = pattern :: inst.memory.procedural } in
    { inst with memory }

  let update_pattern_stats (inst : institution) ~pattern_id ~success ~now =
    let procedural = List.map (fun (p : pattern) ->
      if p.id = pattern_id then
        let new_count = p.usage_count + 1 in
        let new_rate =
          if success then (p.success_rate *. float_of_int p.usage_count +. 1.0) /. float_of_int new_count
          else (p.success_rate *. float_of_int p.usage_count) /. float_of_int new_count
        in
        { p with usage_count = new_count; success_rate = new_rate; last_used = now }
      else p
    ) inst.memory.procedural in
    { inst with memory = { inst.memory with procedural } }

  let agent_join inst ~agent_id =
    if List.mem agent_id inst.current_agents then inst
    else { inst with current_agents = agent_id :: inst.current_agents }

  let agent_leave inst ~agent_id =
    if List.mem agent_id inst.current_agents then
      { inst with
        current_agents = List.filter ((<>) agent_id) inst.current_agents;
        alumni = agent_id :: inst.alumni;
      }
    else inst
end

(** {1 Effectful Operations (Eio)} *)

let get_or_create ~fs (config : config) ~name ~mission =
  match load_institution ~fs config with
  | Some inst -> inst
  | None ->
    let inst = create_institution ~name ~mission () in
    save_institution ~fs config inst;
    inst

let record_episode ~fs config inst ~event_type ~summary ~participants ~outcome ~learnings =
  let episode : episode = {
    id = Printf.sprintf "ep-%d" (Level4_config.random_int 100000);
    timestamp = Unix.gettimeofday ();
    participants; event_type; summary; outcome; learnings; context = [];
  } in
  let inst' = Pure.record_episode inst ~episode in
  save_institution ~fs config inst';
  inst'

let learn_knowledge ~fs config inst ~topic ~content ~source =
  let now = Unix.gettimeofday (); in
  let knowledge : knowledge = {
    id = Printf.sprintf "know-%d" (Level4_config.random_int 100000);
    topic; content; confidence = 0.5; source; created_at = now; last_verified = now; references = [];
  } in
  let inst' = Pure.add_knowledge inst ~knowledge in
  save_institution ~fs config inst';
  inst'

let codify_pattern ~fs config inst ~name ~description ~trigger ~steps =
  let pattern : pattern = {
    id = Printf.sprintf "pat-%d" (Level4_config.random_int 100000);
    name; description; trigger; steps; success_rate = 0.5; usage_count = 0;
    last_used = Unix.gettimeofday (); evolved_from = None;
  } in
  let inst' = Pure.add_pattern inst ~pattern in
  save_institution ~fs config inst';
  inst'

let join ~fs config inst ~agent_id =
  let inst' = Pure.agent_join inst ~agent_id in
  save_institution ~fs config inst';
  inst'

let leave ~fs config inst ~agent_id =
  let inst' = Pure.agent_leave inst ~agent_id in
  save_institution ~fs config inst';
  inst'
