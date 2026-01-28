(** Tool_agent - Agent management, metrics, and capability discovery handlers *)

type context = {
  config: Room.config;
  agent_name: string;
}

(** Helper: get string from args *)
let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

(** Helper: get string option from args *)
let get_string_opt args key =
  match Yojson.Safe.Util.member key args with
  | `String s -> Some s
  | _ -> None

(** Helper: get int from args *)
let get_int args key default =
  match Yojson.Safe.Util.member key args with
  | `Int i -> i
  | _ -> default

(** Helper: get string list from args *)
let get_string_list args key =
  match Yojson.Safe.Util.member key args with
  | `List items ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

(** Helper: result to response *)
let result_to_response = function
  | Ok msg -> (true, msg)
  | Error e -> (false, Types.masc_error_to_string e)

(** Handle masc_agents *)
let handle_agents ctx _args =
  let json = Room.get_agents_status ctx.config in
  (true, Yojson.Safe.pretty_to_string json)

(** Handle masc_register_capabilities *)
let handle_register_capabilities ctx args =
  let capabilities = get_string_list args "capabilities" in
  (true, Room.register_capabilities ctx.config ~agent_name:ctx.agent_name ~capabilities)

(** Handle masc_agent_update *)
let handle_agent_update ctx args =
  let status = get_string_opt args "status" in
  let capabilities =
    match Yojson.Safe.Util.member "capabilities" args with
    | `Null -> None
    | `List _ -> Some (get_string_list args "capabilities")
    | _ -> None
  in
  result_to_response (Room.update_agent_r ctx.config ~agent_name:ctx.agent_name ?status ?capabilities ())

(** Handle masc_find_by_capability *)
let handle_find_by_capability ctx args =
  let capability = get_string args "capability" "" in
  let json = Room.find_agents_by_capability ctx.config ~capability in
  (true, Yojson.Safe.pretty_to_string json)

(** Handle masc_get_metrics *)
let handle_get_metrics ctx args =
  let target = get_string args "agent_name" "" in
  let days = get_int args "days" 7 in
  match Metrics_store_eio.calculate_agent_metrics ctx.config ~agent_id:target ~days with
  | Some metrics ->
      (true, Yojson.Safe.pretty_to_string (Metrics_store_eio.agent_metrics_to_yojson metrics))
  | None ->
      (false, Printf.sprintf "❌ No metrics found for agent: %s" target)

(** Create default metrics for agent *)
let create_default_metrics ~agent_id ~days =
  let now = Unix.gettimeofday () in
  { Metrics_store_eio.agent_id = agent_id;
    period_start = now -. (float_of_int days *. 86400.0);
    period_end = now;
    total_tasks = 0;
    completed_tasks = 0;
    failed_tasks = 0;
    avg_completion_time_s = 0.0;
    task_completion_rate = 0.0;
    error_rate = 0.0;
    handoff_success_rate = 0.0;
    unique_collaborators = [];
  }

(** Get metrics for agent, with default fallback *)
let metrics_for ctx ~days agent_id =
  match Metrics_store_eio.calculate_agent_metrics ctx.config ~agent_id ~days with
  | Some m -> m
  | None -> create_default_metrics ~agent_id ~days

(** Calculate min avg time from metrics list *)
let min_avg_time metrics_list =
  metrics_list
  |> List.map (fun (_, m) -> m.Metrics_store_eio.avg_completion_time_s)
  |> List.filter (fun t -> t > 0.0)
  |> List.fold_left (fun acc t -> if acc = 0.0 || t < acc then t else acc) 0.0

(** Calculate max collaborators from metrics list *)
let max_collabs metrics_list =
  metrics_list
  |> List.map (fun (_, m) -> List.length m.Metrics_store_eio.unique_collaborators)
  |> List.fold_left max 0

(** Score function for fitness calculation *)
let score_for ~min_avg ~max_collabs metrics =
  let has_data = metrics.Metrics_store_eio.total_tasks > 0 in
  let completion = metrics.Metrics_store_eio.task_completion_rate in
  let reliability = if has_data then 1.0 -. metrics.Metrics_store_eio.error_rate else 0.0 in
  let handoff = if has_data then metrics.Metrics_store_eio.handoff_success_rate else 0.0 in
  let speed =
    if has_data && metrics.Metrics_store_eio.avg_completion_time_s > 0.0 && min_avg > 0.0 then
      min 1.0 (min_avg /. metrics.Metrics_store_eio.avg_completion_time_s)
    else 0.0
  in
  let collab_count = List.length metrics.Metrics_store_eio.unique_collaborators in
  let collaboration =
    if max_collabs = 0 then 0.0
    else float_of_int collab_count /. float_of_int max_collabs
  in
  let score =
    (0.35 *. completion) +. (0.25 *. reliability) +. (0.15 *. speed)
    +. (0.15 *. handoff) +. (0.10 *. collaboration)
  in
  (score, completion, reliability, speed, handoff, collaboration)

(** Handle masc_agent_fitness *)
let handle_agent_fitness ctx args =
  let agent_opt = get_string_opt args "agent_name" in
  let days = get_int args "days" 7 in
  let agents =
    match agent_opt with
    | Some a -> [a]
    | None -> Metrics_store_eio.get_all_agents ctx.config
  in
  if agents = [] then
    (true, Yojson.Safe.pretty_to_string (`Assoc [("count", `Int 0); ("agents", `List [])]))
  else
    let metrics_list = List.map (fun a -> (a, metrics_for ctx ~days a)) agents in
    let min_avg = min_avg_time metrics_list in
    let max_col = max_collabs metrics_list in
    let agents_json =
      List.map (fun (agent_id, metrics) ->
        let (score, completion, reliability, speed, handoff, collaboration) = score_for ~min_avg ~max_collabs:max_col metrics in
        `Assoc [
          ("agent_id", `String agent_id);
          ("fitness", `Float score);
          ("components", `Assoc [
            ("completion", `Float completion);
            ("reliability", `Float reliability);
            ("speed", `Float speed);
            ("handoff", `Float handoff);
            ("collaboration", `Float collaboration);
          ]);
          ("metrics", Metrics_store_eio.agent_metrics_to_yojson metrics);
        ]
      ) metrics_list
    in
    let json = `Assoc [
      ("count", `Int (List.length agents_json));
      ("agents", `List agents_json);
    ] in
    (true, Yojson.Safe.pretty_to_string json)

(** Pick random from list *)
let pick_random lst =
  let idx = Random.int (List.length lst) in
  List.nth lst idx

(** Handle masc_select_agent *)
let handle_select_agent ctx args =
  let available = match Yojson.Safe.Util.member "available_agents" args with
    | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
    | _ -> []
  in
  let strategy = get_string args "strategy" "capability_first" in
  let days = get_int args "days" 7 in
  if available = [] then
    (false, "❌ available_agents required")
  else
    let metrics_list = List.map (fun a -> (a, metrics_for ctx ~days a)) available in
    let min_avg = min_avg_time metrics_list in
    let max_col = max_collabs metrics_list in
    let scored =
      List.map (fun (agent_id, metrics) ->
        let (score, completion, reliability, speed, handoff, collaboration) = score_for ~min_avg ~max_collabs:max_col metrics in
        (agent_id, score,
         `Assoc [
           ("completion", `Float completion);
           ("reliability", `Float reliability);
           ("speed", `Float speed);
           ("handoff", `Float handoff);
           ("collaboration", `Float collaboration);
         ])
      ) metrics_list
    in
    let selected =
      match strategy with
      | "random" -> pick_random scored
      | "roulette_wheel" ->
          let total = List.fold_left (fun acc (_, s, _) -> acc +. max 0.0 s) 0.0 scored in
          if total <= 0.0 then pick_random scored
          else
            let target = Random.float total in
            let rec pick acc = function
              | [] -> List.hd scored
              | (id, s, comp) :: rest ->
                  let acc' = acc +. max 0.0 s in
                  if acc' >= target then (id, s, comp) else pick acc' rest
            in
            pick 0.0 scored
      | "elite_1" | "capability_first" | _ ->
          List.fold_left (fun best candidate ->
            match best with
            | None -> Some candidate
            | Some (_, best_score, _) ->
                let (_, score, _) = candidate in
                if score > best_score then Some candidate else best
          ) None scored |> Option.get
    in
    let (agent_id, score, components) = selected in
    let scores_json =
      `List (List.map (fun (id, s, comp) ->
        `Assoc [
          ("agent_id", `String id);
          ("fitness", `Float s);
          ("components", comp);
        ]) scored)
    in
    let json = `Assoc [
      ("selected_agent", `String agent_id);
      ("fitness", `Float score);
      ("components", components);
      ("strategy", `String strategy);
      ("scores", scores_json);
    ] in
    (true, Yojson.Safe.pretty_to_string json)

(** Handle masc_collaboration_graph *)
let handle_collaboration_graph ctx args =
  let format = get_string args "format" "text" in
  let (synapses, agents) = Hebbian_eio.get_graph_data ctx.config in
  if format = "json" then
    let json = `Assoc [
      ("agents", `List (List.map (fun a -> `String a) agents));
      ("synapses", `List (List.map Hebbian_eio.synapse_to_json synapses));
    ] in
    (true, Yojson.Safe.pretty_to_string json)
  else
    let lines =
      synapses
      |> List.sort (fun a b -> compare b.Hebbian_eio.weight a.Hebbian_eio.weight)
      |> List.map (fun s ->
          Printf.sprintf "%s → %s (%.2f, success:%d, failure:%d)"
            s.Hebbian_eio.from_agent s.Hebbian_eio.to_agent
            s.Hebbian_eio.weight s.Hebbian_eio.success_count s.Hebbian_eio.failure_count)
    in
    if lines = [] then
      (true, "No collaboration data yet.")
    else
      (true, String.concat "\n" lines)

(** Handle masc_consolidate_learning *)
let handle_consolidate_learning ctx args =
  let decay_after_days = get_int args "decay_after_days" 7 in
  let pruned = Hebbian_eio.consolidate ctx.config ~decay_after_days () in
  (true, Printf.sprintf "✅ Consolidated. Pruned %d weak connections." pruned)

(** Handle masc_agent_card *)
let handle_agent_card _ctx args =
  let action = get_string args "action" "get" in
  let card = Agent_card.generate_default () in
  let json = Agent_card.to_json card in
  let response = match action with
    | "refresh" ->
        `Assoc [
          ("status", `String "refreshed");
          ("card", json);
          ("endpoint", `String "/.well-known/agent-card.json");
        ]
    | _ ->
        `Assoc [
          ("card", json);
          ("endpoint", `String "/.well-known/agent-card.json");
        ]
  in
  (true, Yojson.Safe.pretty_to_string response)

(** Dispatch handler. Returns Some (success, result) if handled, None otherwise *)
let dispatch ctx ~name ~args =
  match name with
  | "masc_agents" -> Some (handle_agents ctx args)
  | "masc_register_capabilities" -> Some (handle_register_capabilities ctx args)
  | "masc_agent_update" -> Some (handle_agent_update ctx args)
  | "masc_find_by_capability" -> Some (handle_find_by_capability ctx args)
  | "masc_get_metrics" -> Some (handle_get_metrics ctx args)
  | "masc_agent_fitness" -> Some (handle_agent_fitness ctx args)
  | "masc_select_agent" -> Some (handle_select_agent ctx args)
  | "masc_collaboration_graph" -> Some (handle_collaboration_graph ctx args)
  | "masc_consolidate_learning" -> Some (handle_consolidate_learning ctx args)
  | "masc_agent_card" -> Some (handle_agent_card ctx args)
  | _ -> None
