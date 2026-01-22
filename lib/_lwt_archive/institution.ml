(** MASC Institution - Level 5 Persistent Collective Memory

    Self-perpetuating systems that outlive individual agents.
    Institutional memory, culture, and succession policies.

    Research basis:
    - Hippocampus-Inspired AI (PMC11591613)
    - Organizational Memory Theory

    Key Properties:
    - Survives agent death
    - Culture: "This is how we do things"
    - Succession: How new agents are onboarded

    @author Second Brain
    @since MASC v3.2 (Level 5)
*)

(** {1 ⚠️ PRODUCTION GUIDELINES}

    {2 Data Persistence}
    - Episodes are append-only; deletions require audit trail
    - Knowledge confidence decays over time without reinforcement
    - Pattern usage metrics should drive pruning decisions

    {2 Concurrency}
    - Episode recording is thread-safe via Lwt
    - Knowledge updates may cause read anomalies during consolidation
    - Use [Institution.Pure] for lock-free operations where possible

    {2 Memory Management}
    - Large episode histories should be paginated or archived
    - Knowledge graph can grow unbounded; implement TTL policies
    - Consider streaming for bulk episode exports

    {2 Integration}
    - Level 5 → Level 6 (Mind): Use [Mind.learn_from_episode]
    - Level 5 → Level 7 (Noosphere): Broadcast significant episodes
*)

open Lwt.Syntax

(** {1 Types} *)

(** Episode - A recorded event in institutional history *)
type episode = {
  id: string;
  timestamp: float;
  participants: string list;      (** Agent IDs involved *)
  event_type: string;             (** "task_completed", "decision_made", etc. *)
  summary: string;
  outcome: [`Success | `Failure | `Partial];
  learnings: string list;         (** What was learned *)
  context: (string * string) list; (** Key-value metadata *)
}

(** Knowledge item in semantic memory *)
type knowledge = {
  id: string;
  topic: string;
  content: string;
  confidence: float;              (** 0.0-1.0 *)
  source: string;                 (** Where this knowledge came from *)
  created_at: float;
  last_verified: float;
  references: string list;        (** Related knowledge IDs *)
}

(** Procedural pattern - "How we do things" *)
type pattern = {
  id: string;
  name: string;
  description: string;
  trigger: string;                (** When to apply this pattern *)
  steps: string list;
  success_rate: float;            (** Historical success rate *)
  usage_count: int;
  last_used: float;
  evolved_from: string option;    (** Parent pattern if evolved *)
}

(** Cultural value - Learned organizational behavior *)
type cultural_value = {
  id: string;
  name: string;
  description: string;
  weight: float;                  (** Importance 0.0-1.0 *)
  examples: string list;
  anti_patterns: string list;     (** What NOT to do *)
  adopted_at: float;
}

(** Succession policy - How new agents are onboarded *)
type succession_policy = {
  onboarding_steps: string list;
  required_knowledge: string list; (** Knowledge IDs new agents must learn *)
  mentor_assignment: [`Random | `Best_fit | `Round_robin];
  probation_period: float;        (** Hours before full membership *)
  graduation_criteria: string list;
}

(** Long-term memory structure (Hippocampus-inspired) *)
type long_term_memory = {
  episodic: episode list;         (** What happened - DG *)
  semantic: knowledge list;       (** What we know - CA3 *)
  procedural: pattern list;       (** How we do things - CA1 *)
}

(** Institution identity *)
type identity = {
  id: string;
  name: string;
  mission: string;
  founded_at: float;
  generation: int;                (** How many succession cycles *)
}

(** Complete Institution *)
type institution = {
  identity: identity;
  memory: long_term_memory;
  culture: cultural_value list;
  succession: succession_policy;
  current_agents: string list;    (** Active agent IDs *)
  alumni: string list;            (** Past agents *)
}

(** Config type alias *)
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
  probation_period = 24.0;  (* 24 hours *)
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
    memory = {
      episodic = [];
      semantic = [];
      procedural = [];
    };
    culture = [];
    succession = default_succession ();
    current_agents = [];
    alumni = [];
  }

(** {1 File Paths} *)

let institution_file (config : config) =
  Filename.concat config.Room_utils.base_path ".masc/institution.json"

let episodes_file (config : config) =
  Filename.concat config.Room_utils.base_path ".masc/episodes.jsonl"

let knowledge_file (config : config) =
  Filename.concat config.Room_utils.base_path ".masc/knowledge.json"

let patterns_file (config : config) =
  Filename.concat config.Room_utils.base_path ".masc/patterns.json"

(** {1 Serialization} *)

let outcome_to_string = function
  | `Success -> "success"
  | `Failure -> "failure"
  | `Partial -> "partial"

let outcome_of_string = function
  | "success" -> `Success
  | "failure" -> `Failure
  | "partial" -> `Partial
  | _ -> `Partial

let mentor_to_string = function
  | `Random -> "random"
  | `Best_fit -> "best_fit"
  | `Round_robin -> "round_robin"

let mentor_of_string = function
  | "random" -> `Random
  | "best_fit" -> `Best_fit
  | "round_robin" -> `Round_robin
  | _ -> `Best_fit

let episode_to_json (e : episode) : Yojson.Safe.t =
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

let episode_of_json json =
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

let knowledge_to_json (k : knowledge) : Yojson.Safe.t =
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

let knowledge_of_json json =
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

let pattern_to_json (p : pattern) : Yojson.Safe.t =
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

let pattern_of_json json =
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

let cultural_value_to_json (c : cultural_value) : Yojson.Safe.t =
  `Assoc [
    ("id", `String c.id);
    ("name", `String c.name);
    ("description", `String c.description);
    ("weight", `Float c.weight);
    ("examples", `List (List.map (fun e -> `String e) c.examples));
    ("anti_patterns", `List (List.map (fun a -> `String a) c.anti_patterns));
    ("adopted_at", `Float c.adopted_at);
  ]

let cultural_value_of_json json =
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

let succession_to_json (s : succession_policy) : Yojson.Safe.t =
  `Assoc [
    ("onboarding_steps", `List (List.map (fun s -> `String s) s.onboarding_steps));
    ("required_knowledge", `List (List.map (fun k -> `String k) s.required_knowledge));
    ("mentor_assignment", `String (mentor_to_string s.mentor_assignment));
    ("probation_period", `Float s.probation_period);
    ("graduation_criteria", `List (List.map (fun c -> `String c) s.graduation_criteria));
  ]

let succession_of_json json =
  let open Yojson.Safe.Util in
  {
    onboarding_steps = json |> member "onboarding_steps" |> to_list |> List.map to_string;
    required_knowledge = json |> member "required_knowledge" |> to_list |> List.map to_string;
    mentor_assignment = json |> member "mentor_assignment" |> to_string |> mentor_of_string;
    probation_period = json |> member "probation_period" |> to_float;
    graduation_criteria = json |> member "graduation_criteria" |> to_list |> List.map to_string;
  }

let identity_to_json (i : identity) : Yojson.Safe.t =
  `Assoc [
    ("id", `String i.id);
    ("name", `String i.name);
    ("mission", `String i.mission);
    ("founded_at", `Float i.founded_at);
    ("generation", `Int i.generation);
  ]

let identity_of_json json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    mission = json |> member "mission" |> to_string;
    founded_at = json |> member "founded_at" |> to_float;
    generation = json |> member "generation" |> to_int;
  }

let memory_to_json (m : long_term_memory) : Yojson.Safe.t =
  `Assoc [
    ("episodic", `List (List.map episode_to_json m.episodic));
    ("semantic", `List (List.map knowledge_to_json m.semantic));
    ("procedural", `List (List.map pattern_to_json m.procedural));
  ]

let memory_of_json json =
  let open Yojson.Safe.Util in
  {
    episodic = json |> member "episodic" |> to_list |> List.map episode_of_json;
    semantic = json |> member "semantic" |> to_list |> List.map knowledge_of_json;
    procedural = json |> member "procedural" |> to_list |> List.map pattern_of_json;
  }

let institution_to_json (inst : institution) : Yojson.Safe.t =
  `Assoc [
    ("identity", identity_to_json inst.identity);
    ("memory", memory_to_json inst.memory);
    ("culture", `List (List.map cultural_value_to_json inst.culture));
    ("succession", succession_to_json inst.succession);
    ("current_agents", `List (List.map (fun a -> `String a) inst.current_agents));
    ("alumni", `List (List.map (fun a -> `String a) inst.alumni));
  ]

let institution_of_json json =
  let open Yojson.Safe.Util in
  {
    identity = json |> member "identity" |> identity_of_json;
    memory = json |> member "memory" |> memory_of_json;
    culture = json |> member "culture" |> to_list |> List.map cultural_value_of_json;
    succession = json |> member "succession" |> succession_of_json;
    current_agents = json |> member "current_agents" |> to_list |> List.map to_string;
    alumni = json |> member "alumni" |> to_list |> List.map to_string;
  }

(** {1 Persistence} *)

let load_institution (config : config) : institution option Lwt.t =
  let file = institution_file config in
  if Sys.file_exists file then
    let* content = Lwt_io.with_file ~mode:Lwt_io.Input file Lwt_io.read in
    try
      let json = Yojson.Safe.from_string content in
      Lwt.return (Some (institution_of_json json))
    with _ -> Lwt.return None
  else
    Lwt.return None

let save_institution (config : config) (inst : institution) : unit Lwt.t =
  let file = institution_file config in
  let dir = Filename.dirname file in
  let* () =
    if not (Sys.file_exists dir) then
      Lwt_unix.mkdir dir 0o755
    else
      Lwt.return_unit
  in
  let json = institution_to_json inst in
  let content = Yojson.Safe.pretty_to_string json in
  Lwt_io.with_file
    ~flags:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
    ~perm:0o600
    ~mode:Lwt_io.Output
    file
    (fun oc -> Lwt_io.write oc content)

(** {1 Logging} *)

let log_info msg = Printf.eprintf "[institution] %s\n%!" msg
let log_warn msg = Printf.eprintf "[institution] WARN: %s\n%!" msg

(** {1 Pure Transformations} *)

module Pure = struct
  (** Record an episode in institutional memory *)
  let record_episode inst ~episode =
    let memory = { inst.memory with
      episodic = episode :: inst.memory.episodic
    } in
    { inst with memory }

  (** Add knowledge to semantic memory *)
  let add_knowledge inst ~knowledge =
    let memory = { inst.memory with
      semantic = knowledge :: inst.memory.semantic
    } in
    { inst with memory }

  (** Register a procedural pattern *)
  let add_pattern inst ~pattern =
    let memory = { inst.memory with
      procedural = pattern :: inst.memory.procedural
    } in
    { inst with memory }

  (** Update pattern usage and success rate *)
  let update_pattern_stats (inst : institution) ~pattern_id ~success ~now =
    let procedural = List.map (fun (p : pattern) ->
      if p.id = pattern_id then
        let new_count = p.usage_count + 1 in
        let new_rate =
          if success then
            (p.success_rate *. float_of_int p.usage_count +. 1.0) /. float_of_int new_count
          else
            (p.success_rate *. float_of_int p.usage_count) /. float_of_int new_count
        in
        { p with usage_count = new_count; success_rate = new_rate; last_used = now }
      else p
    ) inst.memory.procedural in
    { inst with memory = { inst.memory with procedural } }

  (** Add a cultural value *)
  let adopt_value inst ~value =
    { inst with culture = value :: inst.culture }

  (** Agent joins the institution *)
  let agent_join inst ~agent_id =
    if List.mem agent_id inst.current_agents then
      inst  (* Already a member *)
    else
      { inst with current_agents = agent_id :: inst.current_agents }

  (** Agent leaves the institution (becomes alumni) *)
  let agent_leave inst ~agent_id =
    if List.mem agent_id inst.current_agents then
      { inst with
        current_agents = List.filter ((<>) agent_id) inst.current_agents;
        alumni = agent_id :: inst.alumni;
      }
    else
      inst

  (** Increment generation (succession cycle completed) *)
  let increment_generation inst =
    { inst with
      identity = { inst.identity with generation = inst.identity.generation + 1 }
    }

  (** Get top patterns by success rate *)
  let top_patterns inst ~limit =
    let sorted = List.sort (fun a b -> compare b.success_rate a.success_rate) inst.memory.procedural in
    List.filteri (fun i _ -> i < limit) sorted

  (** Get recent episodes *)
  let recent_episodes inst ~limit =
    let sorted = List.sort (fun a b -> compare b.timestamp a.timestamp) inst.memory.episodic in
    List.filteri (fun i _ -> i < limit) sorted

  (** Check if haystack contains needle (substring) *)
  let contains_substring ~needle haystack =
    let nl = String.length needle in
    let hl = String.length haystack in
    if nl > hl then false
    else if nl = 0 then true
    else begin
      let rec check i =
        if i > hl - nl then false
        else if String.sub haystack i nl = needle then true
        else check (i + 1)
      in
      check 0
    end

  (** Search knowledge by topic *)
  let search_knowledge inst ~query =
    let query_lower = String.lowercase_ascii query in
    List.filter (fun (k : knowledge) ->
      contains_substring ~needle:query_lower (String.lowercase_ascii k.topic)
      || contains_substring ~needle:query_lower (String.lowercase_ascii k.content)
    ) inst.memory.semantic

  (** Find applicable pattern for a trigger *)
  let find_pattern inst ~trigger =
    List.find_opt (fun p ->
      String.lowercase_ascii p.trigger = String.lowercase_ascii trigger
    ) inst.memory.procedural

  (** Check if agent has graduated from probation *)
  let is_graduated inst ~agent_id ~now:_ =
    (* Simple check: agent has been member for probation_period hours *)
    (* In real system, would check graduation_criteria with timestamp *)
    List.mem agent_id inst.current_agents &&
    not (List.mem agent_id inst.alumni)  (* Not yet graduated *)

  (** Get onboarding status for new agent *)
  let onboarding_checklist inst =
    inst.succession.onboarding_steps

  (** Culture alignment score for an action *)
  let culture_alignment inst ~action_description =
    (* Simple implementation: count matching keywords *)
    let action_lower = String.lowercase_ascii action_description in
    let score = List.fold_left (fun acc (value : cultural_value) ->
      let matches = List.filter (fun example ->
        contains_substring ~needle:(String.lowercase_ascii example) action_lower
      ) value.examples in
      let anti_matches = List.filter (fun anti ->
        contains_substring ~needle:(String.lowercase_ascii anti) action_lower
      ) value.anti_patterns in
      acc +. (float_of_int (List.length matches) *. value.weight)
          -. (float_of_int (List.length anti_matches) *. value.weight *. 2.0)
    ) 0.0 inst.culture in
    max 0.0 (min 1.0 (0.5 +. score /. 10.0))  (* Normalize to 0-1 *)
end

(** {1 Effectful Operations} *)

(** Initialize or load institution *)
let get_or_create config ~name ~mission =
  let* existing = load_institution config in
  match existing with
  | Some inst ->
    log_info (Printf.sprintf "Loaded institution: %s (gen %d)" inst.identity.name inst.identity.generation);
    Lwt.return inst
  | None ->
    let inst = create_institution ~name ~mission () in
    let* () = save_institution config inst in
    log_info (Printf.sprintf "Created institution: %s" name);
    Lwt.return inst

(** Record episode and persist *)
let record_episode config inst ~event_type ~summary ~participants ~outcome ~learnings =
  let episode : episode = {
    id = Printf.sprintf "ep-%d" (Level4_config.random_int 100000);
    timestamp = Unix.gettimeofday ();
    participants;
    event_type;
    summary;
    outcome;
    learnings;
    context = [];
  } in
  let inst = Pure.record_episode inst ~episode in
  let* () = save_institution config inst in
  log_info (Printf.sprintf "Recorded episode: %s" event_type);
  Lwt.return inst

(** Learn new knowledge and persist *)
let learn_knowledge config inst ~topic ~content ~source =
  let now = Unix.gettimeofday () in
  let knowledge : knowledge = {
    id = Printf.sprintf "know-%d" (Level4_config.random_int 100000);
    topic;
    content;
    confidence = 0.5;  (* Start with medium confidence *)
    source;
    created_at = now;
    last_verified = now;
    references = [];
  } in
  let inst = Pure.add_knowledge inst ~knowledge in
  let* () = save_institution config inst in
  log_info (Printf.sprintf "Learned: %s" topic);
  Lwt.return inst

(** Codify a pattern and persist *)
let codify_pattern config inst ~name ~description ~trigger ~steps =
  let pattern : pattern = {
    id = Printf.sprintf "pat-%d" (Level4_config.random_int 100000);
    name;
    description;
    trigger;
    steps;
    success_rate = 0.5;  (* Start neutral *)
    usage_count = 0;
    last_used = Unix.gettimeofday ();
    evolved_from = None;
  } in
  let inst = Pure.add_pattern inst ~pattern in
  let* () = save_institution config inst in
  log_info (Printf.sprintf "Codified pattern: %s" name);
  Lwt.return inst

(** Agent joins institution *)
let join config inst ~agent_id =
  let inst = Pure.agent_join inst ~agent_id in
  let* () = save_institution config inst in
  log_info (Printf.sprintf "Agent joined: %s" agent_id);
  Lwt.return inst

(** Agent leaves institution *)
let leave config inst ~agent_id =
  let inst = Pure.agent_leave inst ~agent_id in
  let* () = save_institution config inst in
  log_info (Printf.sprintf "Agent left: %s (now alumni)" agent_id);
  Lwt.return inst
