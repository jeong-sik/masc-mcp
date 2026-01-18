(** MASC Fitness Selection - Agent Performance-Based Selection

    에이전트 선택을 위한 적합도 계산:
    - Weighted scoring based on metrics
    - Multiple selection strategies
    - Capability matching

    Research basis:
    - EvoAgent (arXiv 2406.14228): Fitness-based selection
    - ACM TOSEM: Role-based orchestration

    Weight distribution (empirical):
    - task_completion_rate: 35%
    - error_rate: 25%
    - avg_response_time: 15%
    - handoff_success: 15%
    - collaboration_score: 10%
*)

open Lwt.Syntax

(** Config type alias *)
type config = Room_utils.config

(** Fitness weights - configurable *)
type weights = {
  completion: float;   (* Default: 0.35 *)
  error: float;        (* Default: 0.25 *)
  speed: float;        (* Default: 0.15 *)
  handoff: float;      (* Default: 0.15 *)
  collaboration: float; (* Default: 0.10 *)
}

(** Default weights *)
let default_weights = {
  completion = 0.35;
  error = 0.25;
  speed = 0.15;
  handoff = 0.15;
  collaboration = 0.10;
}

(** Selection strategy *)
type selection_strategy =
  | Roulette_wheel    (* Probabilistic selection based on fitness *)
  | Elite of int      (* Select top N agents *)
  | Capability_first  (* Match capabilities first, then fitness *)
  | Random            (* Random selection (baseline) *)

(** Agent fitness result *)
type fitness_result = {
  agent_id: string;
  fitness_score: float;       (* 0.0-1.0, higher is better *)
  metrics: Metrics_store.agent_metrics option;
  breakdown: fitness_breakdown;
}

and fitness_breakdown = {
  completion_component: float;
  error_component: float;
  speed_component: float;
  handoff_component: float;
  collaboration_component: float;
}

(** P0 Fix: Safe float validation wrapper *)
let safe_float f name =
  Validation.Safe_float.validate f ~name

(** P1 Fix: Recency decay - recent metrics matter more *)
let recency_decay ~age_days =
  let tau = 7.0 in  (* 7-day half-life *)
  exp (-.age_days /. tau)

(** Normalize speed score (faster = better, diminishing returns) *)
let normalize_speed avg_time_s =
  (* Target: 60s average is "good" (1.0), diminishing returns beyond *)
  let time = safe_float avg_time_s "avg_time_s" in
  if time <= 0.0 then 1.0
  else min 1.0 (60.0 /. time)

(** Calculate fitness score from metrics *)
let calculate_fitness ?(weights=default_weights) (metrics : Metrics_store.agent_metrics) : fitness_result =
  (* P0 Fix: Validate all input floats to prevent NaN/Inf propagation *)
  let completion_rate = safe_float metrics.task_completion_rate "completion_rate" in
  let error_rate = safe_float metrics.error_rate "error_rate" in
  let handoff_rate = safe_float metrics.handoff_success_rate "handoff_rate" in

  let completion_component = safe_float (completion_rate *. weights.completion) "completion_component" in
  let error_component = safe_float ((1.0 -. error_rate) *. weights.error) "error_component" in
  let speed_component = safe_float ((normalize_speed metrics.avg_completion_time_s) *. weights.speed) "speed_component" in
  let handoff_component = safe_float (handoff_rate *. weights.handoff) "handoff_component" in

  (* Collaboration score: based on number of unique collaborators *)
  let collab_count = List.length metrics.unique_collaborators in
  let collab_score = min 1.0 (float_of_int collab_count /. 5.0) in  (* 5+ collaborators = 1.0 *)
  let collaboration_component = safe_float (collab_score *. weights.collaboration) "collaboration_component" in

  let total = safe_float (completion_component +. error_component +. speed_component
              +. handoff_component +. collaboration_component) "total_fitness" in

  {
    agent_id = metrics.agent_id;
    fitness_score = total;
    metrics = Some metrics;
    breakdown = {
      completion_component;
      error_component;
      speed_component;
      handoff_component;
      collaboration_component;
    };
  }

(** Calculate fitness with recency weighting *)
let calculate_fitness_with_recency (metrics : Metrics_store.agent_metrics) =
  let age_days = (Unix.gettimeofday () -. metrics.period_end) /. 86400.0 in
  let recency = recency_decay ~age_days in
  let base = calculate_fitness metrics in
  { base with fitness_score = safe_float (base.fitness_score *. recency) "recency_weighted_fitness" }

(** Calculate fitness for an agent without metrics (new agent) *)
let default_fitness agent_id =
  {
    agent_id;
    fitness_score = 0.5;  (* Neutral score for new agents *)
    metrics = None;
    breakdown = {
      completion_component = 0.175;  (* Half of max for each component *)
      error_component = 0.125;
      speed_component = 0.075;
      handoff_component = 0.075;
      collaboration_component = 0.05;
    };
  }

(** Get fitness for all available agents *)
let get_all_fitness config ?(days=7) () : fitness_result list Lwt.t =
  let* all_agents = Metrics_store.get_all_agents config in
  let* results = Lwt_list.map_s (fun agent_id ->
    let* metrics_opt = Metrics_store.calculate_agent_metrics config ~agent_id ~days in
    match metrics_opt with
    | Some metrics -> Lwt.return (calculate_fitness metrics)
    | None -> Lwt.return (default_fitness agent_id)
  ) all_agents in
  (* Sort by fitness descending *)
  let sorted = List.sort (fun a b ->
    compare b.fitness_score a.fitness_score
  ) results in
  Lwt.return sorted

(** Roulette wheel selection *)
let roulette_select (agents : fitness_result list) : fitness_result option =
  if List.length agents = 0 then None
  else
    let total_fitness = List.fold_left (fun acc a -> acc +. a.fitness_score) 0.0 agents in
    if total_fitness <= 0.0 then
      (* All zero fitness, random selection *)
      Some (List.nth agents (Random.int (List.length agents)))
    else
      let threshold = Random.float total_fitness in
      let rec select acc = function
        | [] -> None
        | [x] -> Some x
        | x :: rest ->
          let acc' = acc +. x.fitness_score in
          if acc' >= threshold then Some x
          else select acc' rest
      in
      select 0.0 agents

(** Select best agent for a task *)
let select_agent config
    ?(strategy=Capability_first)
    ?(required_capabilities=[])
    ?(days=7)
    ~available_agents
    () : fitness_result option Lwt.t =
  (* First, get fitness for all available agents *)
  let* all_fitness = get_all_fitness config ~days () in

  (* Filter to only available agents *)
  let filtered = List.filter (fun f ->
    List.mem f.agent_id available_agents
  ) all_fitness in

  (* Apply capability filtering if required *)
  let capability_filtered =
    if required_capabilities = [] then filtered
    else
      (* Get agents that have ALL required capabilities *)
      let agents_with_caps = List.fold_left (fun acc cap ->
        let json = Room.find_agents_by_capability config ~capability:cap in
        match json with
        | `Assoc fields ->
          (match List.assoc_opt "agents" fields with
           | Some (`List agents) ->
             let agent_ids = List.filter_map (function
               | `String id -> Some id
               | _ -> None
             ) agents in
             if acc = [] then agent_ids  (* First capability *)
             else List.filter (fun a -> List.mem a agent_ids) acc  (* Intersection *)
           | _ -> acc)
        | _ -> acc
      ) [] required_capabilities in
      List.filter (fun f -> List.mem f.agent_id agents_with_caps) filtered
  in

  (* Add any available agents without metrics (only those passing capability filter) *)
  let known_ids = List.map (fun f -> f.agent_id) capability_filtered in
  let unknown = List.filter (fun a -> not (List.mem a known_ids)) available_agents in
  (* For unknown agents, only add if they pass capability check *)
  let unknown_filtered =
    if required_capabilities = [] then unknown
    else
      List.filter (fun agent_id ->
        List.for_all (fun cap ->
          let json = Room.find_agents_by_capability config ~capability:cap in
          match json with
          | `Assoc fields ->
            (match List.assoc_opt "agents" fields with
             | Some (`List agents) ->
               List.exists (function `String id -> id = agent_id | _ -> false) agents
             | _ -> false)
          | _ -> false
        ) required_capabilities
      ) unknown
  in
  let with_defaults = capability_filtered @ (List.map default_fitness unknown_filtered) in

  if List.length with_defaults = 0 then
    Lwt.return None
  else
    (* Apply selection strategy *)
    let selected = match strategy with
      | Random ->
        Some (List.nth with_defaults (Random.int (List.length with_defaults)))

      | Elite n ->
        let sorted = List.sort (fun a b ->
          compare b.fitness_score a.fitness_score
        ) with_defaults in
        if n <= 0 then List.nth_opt sorted 0
        else List.nth_opt sorted (min (n - 1) (List.length sorted - 1))

      | Roulette_wheel ->
        roulette_select with_defaults

      | Capability_first ->
        (* Capability filtering already applied above, select best by fitness *)
        let sorted = List.sort (fun a b ->
          compare b.fitness_score a.fitness_score
        ) with_defaults in
        List.nth_opt sorted 0
    in
    Lwt.return selected

(** Fitness result to JSON *)
let fitness_to_json (f : fitness_result) : Yojson.Safe.t =
  `Assoc [
    ("agent_id", `String f.agent_id);
    ("fitness_score", `Float f.fitness_score);
    ("has_metrics", `Bool (Option.is_some f.metrics));
    ("breakdown", `Assoc [
      ("completion", `Float f.breakdown.completion_component);
      ("error", `Float f.breakdown.error_component);
      ("speed", `Float f.breakdown.speed_component);
      ("handoff", `Float f.breakdown.handoff_component);
      ("collaboration", `Float f.breakdown.collaboration_component);
    ]);
  ]

(** Strategy to string *)
let strategy_to_string = function
  | Roulette_wheel -> "roulette_wheel"
  | Elite n -> Printf.sprintf "elite_%d" n
  | Capability_first -> "capability_first"
  | Random -> "random"

(** String to strategy *)
let strategy_of_string s =
  if s = "roulette_wheel" || s = "roulette" then Roulette_wheel
  else if s = "random" then Random
  else if s = "capability_first" || s = "capability" then Capability_first
  else if String.length s > 6 && String.sub s 0 6 = "elite_" then
    let n = int_of_string_opt (String.sub s 6 (String.length s - 6)) in
    Elite (Option.value ~default:1 n)
  else Capability_first  (* Default *)
