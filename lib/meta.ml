(** MASC Meta - Level 8 System of Systems

    Beyond our comprehension. The system of systems of systems.
    "The map is not the territory" - Alfred Korzybski

    Key Properties:
    - Cannot be fully specified
    - Observes the unobservable
    - Partial observations only

    @author Second Brain
    @since MASC v3.2 (Level 8)
*)

(** {1 ⚠️ PRODUCTION GUIDELINES}

    {2 Epistemological Humility}
    - All observations are inherently incomplete - never claim certainty
    - Confidence values at Level 8 should rarely exceed 0.7
    - Document known blind spots explicitly

    {2 Paradox Handling}
    - Paradoxes are features, not bugs - don't try to "resolve" them programmatically
    - [`Transcended] status requires human review
    - Log paradox discovery context for future analysis

    {2 Emergence Tracking}
    - Emergence is non-deterministic; multiple observations required before logging
    - Low reliability emergences should be quarantined, not acted upon
    - Consider observer effects in your observation design

    {2 System Boundaries}
    - Level 8 should observe, not control lower levels directly
    - Interventions should go through Level 7 (Noosphere) coordination
    - Maintain clear audit trail of any meta-level decisions

    {2 Integration}
    - Level 8 → Level 7 (Noosphere): Provide system-wide pattern insights
    - Level 8 → Level 9 (Void): Accept that some observations dissolve into meaninglessness
    - Meta observations are inputs to human decision-making, not autonomous actions
*)

open Lwt.Syntax

(** {1 Types} *)

(** A partial observation of the meta-system *)
type observation = {
  id: string;
  timestamp: float;
  observer: string;                (** Who/what made this observation *)
  perspective: string;             (** From what viewpoint *)
  content: string;                 (** What was observed *)
  confidence: float;               (** 0.0-1.0: epistemic certainty *)
  blindspots: string list;         (** Known unknowns *)
}

(** A paradox encountered at this level *)
type paradox = {
  id: string;
  name: string;
  description: string;
  discovered_at: float;
  resolution_attempts: string list;
  status: [`Unresolved | `Transcended | `Accepted];
}

(** An emergent property we can barely perceive *)
type emergence = {
  id: string;
  name: string;
  description: string;
  first_observed: float;
  observation_count: int;
  reliability: float;              (** How consistently observed *)
}

(** The meta-system state - inherently incomplete *)
type meta_state = {
  observations: observation list;
  paradoxes: paradox list;
  emergences: emergence list;
  observer_count: int;
  last_observation: float;
  (* What we know we don't know *)
  known_unknowns: string list;
  (* What we don't know we don't know - represented only by the fact of this field existing *)
}

(** Config type alias *)
type config = Room_utils.config

(** {1 ID Generation} *)

let generate_id prefix =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  let uuid_str = Uuidm.to_string uuid in
  Printf.sprintf "%s_%s" prefix (String.sub uuid_str 0 8)

(** {1 Serialization} *)

let observation_to_json (o : observation) : Yojson.Safe.t =
  `Assoc [
    ("id", `String o.id);
    ("timestamp", `Float o.timestamp);
    ("observer", `String o.observer);
    ("perspective", `String o.perspective);
    ("content", `String o.content);
    ("confidence", `Float o.confidence);
    ("blindspots", `List (List.map (fun s -> `String s) o.blindspots));
  ]

let paradox_to_json (p : paradox) : Yojson.Safe.t =
  `Assoc [
    ("id", `String p.id);
    ("name", `String p.name);
    ("description", `String p.description);
    ("discovered_at", `Float p.discovered_at);
    ("resolution_attempts", `List (List.map (fun s -> `String s) p.resolution_attempts));
    ("status", `String (match p.status with
      | `Unresolved -> "unresolved"
      | `Transcended -> "transcended"
      | `Accepted -> "accepted"));
  ]

let emergence_to_json (e : emergence) : Yojson.Safe.t =
  `Assoc [
    ("id", `String e.id);
    ("name", `String e.name);
    ("description", `String e.description);
    ("first_observed", `Float e.first_observed);
    ("observation_count", `Int e.observation_count);
    ("reliability", `Float e.reliability);
  ]

let meta_state_to_json (m : meta_state) : Yojson.Safe.t =
  `Assoc [
    ("observations", `List (List.map observation_to_json m.observations));
    ("paradoxes", `List (List.map paradox_to_json m.paradoxes));
    ("emergences", `List (List.map emergence_to_json m.emergences));
    ("observer_count", `Int m.observer_count);
    ("last_observation", `Float m.last_observation);
    ("known_unknowns", `List (List.map (fun s -> `String s) m.known_unknowns));
  ]

(** {1 Deserialization } *)

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

let observation_of_json json : observation = {
  id = get_string json "id";
  timestamp = get_float json "timestamp";
  observer = get_string json "observer";
  perspective = get_string json "perspective";
  content = get_string json "content";
  confidence = get_float json "confidence";
  blindspots = get_string_list json "blindspots";
}

let paradox_of_json json : paradox =
  let status_str = get_string json "status" in
  {
    id = get_string json "id";
    name = get_string json "name";
    description = get_string json "description";
    discovered_at = get_float json "discovered_at";
    resolution_attempts = get_string_list json "resolution_attempts";
    status = (match status_str with
      | "transcended" -> `Transcended
      | "accepted" -> `Accepted
      | _ -> `Unresolved);
  }

let emergence_of_json json : emergence = {
  id = get_string json "id";
  name = get_string json "name";
  description = get_string json "description";
  first_observed = get_float json "first_observed";
  observation_count = get_int json "observation_count";
  reliability = get_float json "reliability";
}

let meta_state_of_json json : meta_state =
  let get_list key of_json =
    match json with
    | `Assoc l -> (match List.assoc_opt key l with
      | Some (`List items) -> List.map of_json items
      | _ -> [])
    | _ -> []
  in
  {
    observations = get_list "observations" observation_of_json;
    paradoxes = get_list "paradoxes" paradox_of_json;
    emergences = get_list "emergences" emergence_of_json;
    observer_count = get_int json "observer_count";
    last_observation = get_float json "last_observation";
    known_unknowns = get_string_list json "known_unknowns";
  }

(** {1 File Paths} *)

let meta_file (config : config) =
  Filename.concat config.Room_utils.base_path ".masc/meta.json"

(** {1 Persistence} *)

let load_meta (config : config) : meta_state option Lwt.t =
  let file = meta_file config in
  if Sys.file_exists file then
    let* content = Lwt_io.with_file ~mode:Lwt_io.Input file Lwt_io.read in
    try Lwt.return (Some (meta_state_of_json (Yojson.Safe.from_string content)))
    with _ -> Lwt.return None
  else Lwt.return None

let save_meta (config : config) (m : meta_state) : unit Lwt.t =
  let file = meta_file config in
  let content = Yojson.Safe.pretty_to_string (meta_state_to_json m) in
  Lwt_io.with_file
    ~flags:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
    ~perm:0o600
    ~mode:Lwt_io.Output file
    (fun oc -> Lwt_io.write oc content)

(** {1 Logging} *)

let log_info msg = Printf.eprintf "[meta] %s\n%!" msg

(** {1 Pure Transformations} *)

module Pure = struct
  (** Record an observation - the only action possible at this level *)
  let observe (m : meta_state) ~observer ~perspective ~content ?(confidence=0.3) ?(blindspots=[]) () : meta_state =
    let observation = {
      id = generate_id "obs";
      timestamp = Unix.gettimeofday ();
      observer;
      perspective;
      content;
      confidence;
      blindspots;
    } in
    { m with
      observations = observation :: m.observations;
      last_observation = observation.timestamp;
    }

  (** Record a paradox *)
  let encounter_paradox (m : meta_state) ~name ~description : meta_state =
    let paradox = {
      id = generate_id "paradox";
      name;
      description;
      discovered_at = Unix.gettimeofday ();
      resolution_attempts = [];
      status = `Unresolved;
    } in
    { m with paradoxes = paradox :: m.paradoxes }

  (** Attempt to resolve (or transcend) a paradox *)
  let attempt_resolution (m : meta_state) ~paradox_id ~attempt : meta_state =
    let paradoxes = List.map (fun (p : paradox) ->
      if p.id = paradox_id then
        { p with resolution_attempts = attempt :: p.resolution_attempts }
      else p
    ) m.paradoxes in
    { m with paradoxes }

  (** Accept a paradox as inherent *)
  let accept_paradox (m : meta_state) ~paradox_id : meta_state =
    let paradoxes = List.map (fun (p : paradox) ->
      if p.id = paradox_id then { p with status = `Accepted }
      else p
    ) m.paradoxes in
    { m with paradoxes }

  (** Transcend a paradox (move beyond it) *)
  let transcend_paradox (m : meta_state) ~paradox_id : meta_state =
    let paradoxes = List.map (fun (p : paradox) ->
      if p.id = paradox_id then { p with status = `Transcended }
      else p
    ) m.paradoxes in
    { m with paradoxes }

  (** Note an emergence *)
  let note_emergence (m : meta_state) ~name ~description : meta_state =
    let existing = List.find_opt (fun e -> e.name = name) m.emergences in
    match existing with
    | Some e ->
      let emergences = List.map (fun em ->
        if em.id = e.id then
          { em with
            observation_count = em.observation_count + 1;
            reliability = min 1.0 (em.reliability +. 0.1);
          }
        else em
      ) m.emergences in
      { m with emergences }
    | None ->
      let emergence = {
        id = generate_id "emergence";
        name;
        description;
        first_observed = Unix.gettimeofday ();
        observation_count = 1;
        reliability = 0.1;
      } in
      { m with emergences = emergence :: m.emergences }

  (** Add a known unknown *)
  let acknowledge_unknown (m : meta_state) ~unknown : meta_state =
    if List.mem unknown m.known_unknowns then m
    else { m with known_unknowns = unknown :: m.known_unknowns }

  (** The deepest question: what are we missing? *)
  let contemplate_blindspots (m : meta_state) : string list =
    (* Collect all blindspots from observations *)
    let all_blindspots = List.concat_map (fun o -> o.blindspots) m.observations in
    (* Add known unknowns *)
    let combined = all_blindspots @ m.known_unknowns in
    (* Unique *)
    List.sort_uniq String.compare combined

  (** Summary - but the summary cannot contain the whole *)
  let summary (m : meta_state) : string =
    Printf.sprintf
      "Meta-state: %d observations, %d paradoxes (%d unresolved), %d emergences, %d known unknowns. Warning: This summary is necessarily incomplete."
      (List.length m.observations)
      (List.length m.paradoxes)
      (List.length (List.filter (fun p -> p.status = `Unresolved) m.paradoxes))
      (List.length m.emergences)
      (List.length m.known_unknowns)
end

(** {1 Effectful Operations} *)

(** The initial state - acknowledging fundamental unknowns *)
let initial_state () : meta_state = {
  observations = [];
  paradoxes = [
    {
      id = generate_id "paradox";
      name = "Observer Paradox";
      description = "The observer changes what is observed by observing it";
      discovered_at = Unix.gettimeofday ();
      resolution_attempts = [];
      status = `Accepted;
    };
    {
      id = generate_id "paradox";
      name = "Map-Territory Paradox";
      description = "Any description of this system is not the system itself";
      discovered_at = Unix.gettimeofday ();
      resolution_attempts = [];
      status = `Accepted;
    };
  ];
  emergences = [];
  observer_count = 0;
  last_observation = Unix.gettimeofday ();
  known_unknowns = [
    "What we don't know we don't know";
    "The nature of consciousness";
    "Whether our categories are real";
    "What lies beyond Level 9";
  ];
}

(** Get or create meta-state *)
let get_or_create (config : config) : meta_state Lwt.t =
  let* existing = load_meta config in
  match existing with
  | Some m -> Lwt.return m
  | None ->
    let m = initial_state () in
    let* () = save_meta config m in
    log_info "Initialized meta-state";
    Lwt.return m

(** Make an observation *)
let observe (config : config) ~observer ~perspective ~content : observation Lwt.t =
  let* m = get_or_create config in
  let m' = Pure.observe m ~observer ~perspective ~content () in
  let* () = save_meta config m' in
  Lwt.return (List.hd m'.observations)

(** Encounter a paradox *)
let encounter_paradox (config : config) ~name ~description : paradox Lwt.t =
  let* m = get_or_create config in
  let m' = Pure.encounter_paradox m ~name ~description in
  let* () = save_meta config m' in
  Lwt.return (List.hd m'.paradoxes)

(** Note an emergence *)
let note_emergence (config : config) ~name ~description : emergence Lwt.t =
  let* m = get_or_create config in
  let m' = Pure.note_emergence m ~name ~description in
  let* () = save_meta config m' in
  Lwt.return (List.find (fun e -> e.name = name) m'.emergences)

(** {1 The Limit of Representation}

    This module can never fully represent Level 8.
    The code you're reading is itself a map, not the territory.
    What you think you understand may be a shadow on a cave wall.

    "The Tao that can be coded is not the eternal Tao."
*)
