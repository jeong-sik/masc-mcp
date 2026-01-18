(** MASC Hebbian Learning - Collaboration Pattern Learning

    "Agents that fire together, wire together"

    Tracks successful collaboration patterns:
    - Strengthen connections on successful tasks
    - Weaken connections on failures
    - Consolidate (decay) old connections

    Research basis:
    - Hippocampus-Inspired AI (PMC11591613): Synaptic plasticity
    - Neural Reshaping (PMC11751442): Adaptive learning

    Storage: .masc/synapses/graph.json
*)

open Lwt.Syntax

(** Config type alias *)
type config = Room_utils.config

(** Synapse between two agents *)
type synapse = {
  from_agent: string;
  to_agent: string;
  weight: float;        (* 0.0-1.0, higher = stronger connection *)
  success_count: int;   (* Number of successful collaborations *)
  failure_count: int;   (* Number of failed collaborations *)
  last_updated: float;  (* Unix timestamp *)
  created_at: float;
}

(** Synapse graph *)
type synapse_graph = {
  synapses: synapse list;
  last_consolidation: float;
}

(** Learning parameters *)
type learning_params = {
  strengthen_rate: float;  (* How much to increase on success, default 0.1 *)
  weaken_rate: float;      (* How much to decrease on failure, default 0.05 *)
  decay_rate: float;       (* Daily decay rate for consolidation, default 0.01 *)
  min_weight: float;       (* Minimum weight before pruning, default 0.05 *)
  max_weight: float;       (* Maximum weight, default 1.0 *)
}

let default_params () = {
  (* P1 Fix: Symmetric rates to prevent convergence oscillation *)
  (* Now externalized via Level2_config *)
  strengthen_rate = Level2_config.Hebbian.learning_rate ();
  weaken_rate = Level2_config.Hebbian.learning_rate ();  (* Symmetric *)
  decay_rate = Level2_config.Hebbian.decay_rate ();
  min_weight = Level2_config.Hebbian.min_weight ();
  max_weight = Level2_config.Hebbian.max_weight ();
}

(** Get synapses file path *)
let synapses_file (config : config) =
  Filename.concat config.Room_utils.base_path ".masc/synapses/graph.json"

(** Get lock file path *)
let synapses_lock_file (config : config) =
  synapses_file config ^ ".lock"

(** Ensure synapses directory exists *)
let ensure_synapses_dir config =
  let masc_dir = Filename.concat config.Room_utils.base_path ".masc" in
  let synapses_dir = Filename.concat masc_dir "synapses" in
  List.iter (fun dir ->
    if not (Sys.file_exists dir) then
      Unix.mkdir dir 0o755
  ) [masc_dir; synapses_dir]

(** MAGI Recommendation: Lock contention metrics *)
let lock_acquisitions = ref 0
let lock_total_wait_ms = ref 0.0
let lock_max_wait_ms = ref 0.0

(** Get lock statistics *)
let get_lock_stats () =
  let avg_wait = if !lock_acquisitions = 0 then 0.0
    else !lock_total_wait_ms /. float_of_int !lock_acquisitions in
  (!lock_acquisitions, avg_wait, !lock_max_wait_ms)

(** Reset lock statistics *)
let reset_lock_stats () =
  lock_acquisitions := 0;
  lock_total_wait_ms := 0.0;
  lock_max_wait_ms := 0.0

(** P0 Fix: Transactional lock for graph operations *)
let with_graph_lock config f =
  let* () = Lwt.return (ensure_synapses_dir config) in
  let lock_file = synapses_lock_file config in
  let* fd = Lwt_unix.openfile lock_file
    [Unix.O_WRONLY; Unix.O_CREAT] 0o600 in
  (* MAGI: Track lock acquisition time *)
  let start_time = Unix.gettimeofday () in
  let* () = Lwt_unix.lockf fd Lwt_unix.F_LOCK 0 in
  let wait_ms = (Unix.gettimeofday () -. start_time) *. 1000.0 in
  incr lock_acquisitions;
  lock_total_wait_ms := !lock_total_wait_ms +. wait_ms;
  if wait_ms > !lock_max_wait_ms then lock_max_wait_ms := wait_ms;
  (* Log if wait was significant - threshold from config *)
  let warn_threshold = Level2_config.Lock.warn_threshold_ms () in
  if wait_ms > warn_threshold then
    Printf.eprintf "[hebbian] WARN: Lock contention detected: %.1fms wait (threshold: %.0fms)\n" wait_ms warn_threshold;
  Lwt.finalize
    (fun () -> f ())
    (fun () ->
      let* () = Lwt_unix.lockf fd Lwt_unix.F_ULOCK 0 in
      Lwt_unix.close fd)

(** Synapse to JSON *)
let synapse_to_json (s : synapse) : Yojson.Safe.t =
  `Assoc [
    ("from_agent", `String s.from_agent);
    ("to_agent", `String s.to_agent);
    ("weight", `Float s.weight);
    ("success_count", `Int s.success_count);
    ("failure_count", `Int s.failure_count);
    ("last_updated", `Float s.last_updated);
    ("created_at", `Float s.created_at);
  ]

(** Synapse from JSON *)
let synapse_of_json json : synapse option =
  let open Yojson.Safe.Util in
  try
    Some {
      from_agent = json |> member "from_agent" |> to_string;
      to_agent = json |> member "to_agent" |> to_string;
      weight = json |> member "weight" |> to_float;
      success_count = json |> member "success_count" |> to_int;
      failure_count = json |> member "failure_count" |> to_int;
      last_updated = json |> member "last_updated" |> to_float;
      created_at = json |> member "created_at" |> to_float;
    }
  with exn ->
    Printf.eprintf "[hebbian] Failed to parse synapse: %s\n" (Printexc.to_string exn);
    None

(** Graph to JSON *)
let graph_to_json (g : synapse_graph) : Yojson.Safe.t =
  `Assoc [
    ("synapses", `List (List.map synapse_to_json g.synapses));
    ("last_consolidation", `Float g.last_consolidation);
  ]

(** Graph from JSON *)
let graph_of_json json : synapse_graph =
  let open Yojson.Safe.Util in
  try
    let synapses = json |> member "synapses" |> to_list
      |> List.filter_map synapse_of_json in
    let last_consolidation = json |> member "last_consolidation" |> to_float in
    { synapses; last_consolidation }
  with exn ->
    Printf.eprintf "[hebbian] Failed to parse graph: %s\n" (Printexc.to_string exn);
    { synapses = []; last_consolidation = 0.0 }

(** Load synapse graph *)
let load_graph config : synapse_graph Lwt.t =
  let file = synapses_file config in
  if not (Sys.file_exists file) then
    Lwt.return { synapses = []; last_consolidation = 0.0 }
  else
    let* content = Lwt_io.with_file ~mode:Lwt_io.Input file Lwt_io.read in
    try
      let json = Yojson.Safe.from_string content in
      Lwt.return (graph_of_json json)
    with exn ->
      Printf.eprintf "[hebbian] Failed to load graph from %s: %s\n" file (Printexc.to_string exn);
      Lwt.return { synapses = []; last_consolidation = 0.0 }

(** Save synapse graph *)
let save_graph config (graph : synapse_graph) : unit Lwt.t =
  let* () = Lwt.return (ensure_synapses_dir config) in
  let file = synapses_file config in
  let json = graph_to_json graph in
  let content = Yojson.Safe.pretty_to_string json in
  (* Security: 0o600 - only owner can read/write synapse data *)
  Lwt_io.with_file ~perm:0o600 ~mode:Lwt_io.Output file (fun oc ->
    Lwt_io.write oc content
  )

(** Find synapse between two agents *)
let find_synapse graph ~from_agent ~to_agent : synapse option =
  List.find_opt (fun s ->
    s.from_agent = from_agent && s.to_agent = to_agent
  ) graph.synapses

(** Create new synapse *)
let create_synapse ~from_agent ~to_agent : synapse =
  let now = Unix.gettimeofday () in
  {
    from_agent;
    to_agent;
    weight = 0.5;  (* Start at neutral *)
    success_count = 0;
    failure_count = 0;
    last_updated = now;
    created_at = now;
  }

(** Update synapse in graph *)
let update_synapse graph (synapse : synapse) : synapse_graph =
  let updated = synapse :: (List.filter (fun s ->
    not (s.from_agent = synapse.from_agent && s.to_agent = synapse.to_agent)
  ) graph.synapses) in
  { graph with synapses = updated }

(** Strengthen connection (on successful collaboration) *)
let strengthen config ?params ~from_agent ~to_agent () : unit Lwt.t =
  let params = Option.value params ~default:(default_params ()) in
  (* P0 Fix: Use transactional lock to prevent RMW race conditions *)
  with_graph_lock config (fun () ->
    let* graph = load_graph config in
    let synapse = match find_synapse graph ~from_agent ~to_agent with
      | Some s -> s
      | None -> create_synapse ~from_agent ~to_agent
    in
    let new_weight = min params.max_weight (synapse.weight +. params.strengthen_rate) in
    let updated = {
      synapse with
      weight = new_weight;
      success_count = synapse.success_count + 1;
      last_updated = Unix.gettimeofday ();
    } in
    let new_graph = update_synapse graph updated in
    save_graph config new_graph
  )

(** Weaken connection (on failed collaboration) *)
let weaken config ?params ~from_agent ~to_agent () : unit Lwt.t =
  let params = Option.value params ~default:(default_params ()) in
  (* P0 Fix: Use transactional lock to prevent RMW race conditions *)
  with_graph_lock config (fun () ->
    let* graph = load_graph config in
    match find_synapse graph ~from_agent ~to_agent with
    | None -> Lwt.return_unit  (* No synapse to weaken *)
    | Some synapse ->
      let new_weight = max 0.0 (synapse.weight -. params.weaken_rate) in
      let updated = {
        synapse with
        weight = new_weight;
        failure_count = synapse.failure_count + 1;
        last_updated = Unix.gettimeofday ();
      } in
      let new_graph = update_synapse graph updated in
      save_graph config new_graph
  )

(** Get preferred collaboration partner for an agent *)
let get_preferred_partner config ~agent_id : string option Lwt.t =
  let* graph = load_graph config in
  let outgoing = List.filter (fun s -> s.from_agent = agent_id) graph.synapses in
  if List.length outgoing = 0 then
    Lwt.return None
  else
    let sorted = List.sort (fun a b -> compare b.weight a.weight) outgoing in
    Lwt.return (Some (List.hd sorted).to_agent)

(** Consolidate - apply decay to old connections *)
let consolidate config ?params ~decay_after_days () : int Lwt.t =
  let params = Option.value params ~default:(default_params ()) in
  let* graph = load_graph config in
  let now = Unix.gettimeofday () in
  let cutoff = now -. (float_of_int decay_after_days *. 86400.0) in

  let (decayed, pruned_count) = List.fold_left (fun (acc, count) synapse ->
    if synapse.last_updated < cutoff then
      (* Apply decay *)
      let days_since = (now -. synapse.last_updated) /. 86400.0 in
      let decay = params.decay_rate *. days_since in
      let new_weight = max 0.0 (synapse.weight -. decay) in
      if new_weight < params.min_weight then
        (* Prune weak synapse *)
        (acc, count + 1)
      else
        let updated = { synapse with weight = new_weight; last_updated = now } in
        (updated :: acc, count)
    else
      (synapse :: acc, count)
  ) ([], 0) graph.synapses in

  let new_graph = { synapses = decayed; last_consolidation = now } in
  let* () = save_graph config new_graph in
  Lwt.return pruned_count

(** Get collaboration graph as visualization data *)
let get_graph_data config : (synapse list * string list) Lwt.t =
  let* graph = load_graph config in
  let agents = List.sort_uniq String.compare (
    List.flatten (List.map (fun s -> [s.from_agent; s.to_agent]) graph.synapses)
  ) in
  Lwt.return (graph.synapses, agents)

(** Graph to text visualization *)
let graph_to_text config : string Lwt.t =
  let* (synapses, agents) = get_graph_data config in
  if List.length synapses = 0 then
    Lwt.return "No collaboration patterns recorded yet."
  else
    let buf = Buffer.create 256 in
    Buffer.add_string buf "=== Hebbian Collaboration Graph ===\n\n";
    Buffer.add_string buf (Printf.sprintf "Agents: %s\n\n" (String.concat ", " agents));
    Buffer.add_string buf "Connections:\n";
    List.iter (fun s ->
      let strength = if s.weight >= 0.7 then "+++"
        else if s.weight >= 0.5 then "++"
        else if s.weight >= 0.3 then "+"
        else "-" in
      Buffer.add_string buf (Printf.sprintf "  %s -> %s [%s] (%.2f, %d/%d)\n"
        s.from_agent s.to_agent strength s.weight s.success_count s.failure_count)
    ) synapses;
    Lwt.return (Buffer.contents buf)
