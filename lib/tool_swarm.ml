(** Swarm Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    10 tools: init, join, leave, status, evolve, propose, vote, deposit, trails, walph
*)

open Yojson.Safe.Util

(** Tool handler context - shared dependencies *)
type context = {
  config: Room.config;
  fs: Eio.Fs.dir_ty Eio.Path.t option;
  agent_name: string;
}

(** Argument extraction helpers *)
let get_string args key default = Safe_ops.json_string ~default key args
let get_float args key default = Safe_ops.json_float ~default key args
let get_int args key default = Safe_ops.json_int ~default key args
let get_bool args key default = Safe_ops.json_bool ~default key args

(** Tool result type *)
type result = bool * string

(** Handle masc_swarm_init *)
let handle_init ctx args : result =
  let behavior = get_string args "behavior" "flocking" |> Swarm_eio.behavior_of_string in
  let selection_pressure = get_float args "selection_pressure" 0.3 in
  let mutation_rate = get_float args "mutation_rate" 0.1 in
  let swarm_cfg = {
    (Swarm_eio.default_config ()) with
    behavior;
    selection_pressure;
    mutation_rate;
  } in
  match ctx.fs with
  | Some fs ->
      let swarm = Swarm_eio.create ~fs ctx.config ~swarm_config:swarm_cfg () in
      (true, Printf.sprintf "✅ Swarm %s initialized with %s behavior"
        swarm.swarm_cfg.name (Swarm_eio.behavior_to_string behavior))
  | None -> (false, "❌ Filesystem not available")

(** Handle masc_swarm_join *)
let handle_join ctx args : result =
  let join_agent_name = get_string args "agent_name" ctx.agent_name in
  match ctx.fs with
  | Some fs ->
      (match Swarm_eio.join ~fs ctx.config ~agent_id:join_agent_name ~agent_name:join_agent_name with
       | Some _ -> (true, Printf.sprintf "✅ Agent %s joined the swarm" join_agent_name)
       | None -> (false, "❌ Failed to join swarm (full or nonexistent)"))
  | None -> (false, "❌ Filesystem not available")

(** Handle masc_swarm_leave *)
let handle_leave ctx args : result =
  let leave_agent_name = get_string args "agent_name" ctx.agent_name in
  match ctx.fs with
  | Some fs ->
      (match Swarm_eio.leave ~fs ctx.config ~agent_id:leave_agent_name with
       | Some _ -> (true, Printf.sprintf "✅ Agent %s left the swarm" leave_agent_name)
       | None -> (false, "❌ Failed to leave swarm"))
  | None -> (false, "❌ Filesystem not available")

(** Handle masc_swarm_status *)
let handle_status ctx _args : result =
  match ctx.fs with
  | Some fs -> (true, Yojson.Safe.pretty_to_string (Swarm_eio.status ~fs ctx.config))
  | None -> (false, "❌ Filesystem not available")

(** Handle masc_swarm_evolve *)
let handle_evolve ctx _args : result =
  match ctx.fs with
  | Some fs ->
      (match Swarm_eio.evolve ~fs ctx.config with
       | Some s -> (true, Printf.sprintf "✅ Swarm evolved to generation %d" s.generation)
       | None -> (false, "❌ Evolution failed"))
  | None -> (false, "❌ Filesystem not available")

(** Handle masc_swarm_propose *)
let handle_propose ctx args : result =
  let description = get_string args "description" "" in
  let threshold = match args |> member "threshold" with `Float f -> Some f | _ -> None in
  match ctx.fs with
  | Some fs ->
      (match Swarm_eio.propose ~fs ctx.config ~description ~proposed_by:ctx.agent_name ?threshold () with
       | Some p -> (true, Printf.sprintf "✅ Proposal %s created" p.proposal_id)
       | None -> (false, "❌ Failed to create proposal"))
  | None -> (false, "❌ Filesystem not available")

(** Handle masc_swarm_vote *)
let handle_vote ctx args : result =
  let proposal_id = get_string args "proposal_id" "" in
  let vote_for = get_bool args "vote_for" true in
  match ctx.fs with
  | Some fs ->
      (match Swarm_eio.vote ~fs ctx.config ~proposal_id ~agent_id:ctx.agent_name ~vote_for with
       | Some p -> (true, Printf.sprintf "✅ Vote recorded. Status: %s" (Swarm_eio.status_to_string p.status))
       | None -> (false, "❌ Failed to record vote"))
  | None -> (false, "❌ Filesystem not available")

(** Handle masc_swarm_deposit *)
let handle_deposit ctx args : result =
  let path_id = get_string args "path_id" "" in
  let strength = get_float args "strength" 0.2 in
  match ctx.fs with
  | Some fs ->
      (match Swarm_eio.deposit_pheromone ~fs ctx.config ~path_id ~agent_id:ctx.agent_name ~strength with
       | Some _ -> (true, Printf.sprintf "✅ Pheromone deposited on path: %s" path_id)
       | None -> (false, "❌ Failed to deposit pheromone"))
  | None -> (false, "❌ Filesystem not available")

(** Handle masc_swarm_trails *)
let handle_trails ctx args : result =
  let limit = get_int args "limit" 5 in
  match ctx.fs with
  | Some fs ->
      let trails = Swarm_eio.get_strongest_trails ~fs ctx.config ~limit in
      let json = `List (List.map Swarm_eio.pheromone_to_json trails) in
      (true, Yojson.Safe.pretty_to_string json)
  | None -> (false, "❌ Filesystem not available")

(** Handle masc_swarm_walph *)
let handle_walph ctx args : result =
  let command = get_string args "command" "STATUS" in
  (true, Room_walph_eio.swarm_walph_control ctx.config ~from_agent:ctx.agent_name ~command ())

(** Dispatch swarm tool by name. Returns None if not a swarm tool. *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_swarm_init" -> Some (handle_init ctx args)
  | "masc_swarm_join" -> Some (handle_join ctx args)
  | "masc_swarm_leave" -> Some (handle_leave ctx args)
  | "masc_swarm_status" -> Some (handle_status ctx args)
  | "masc_swarm_evolve" -> Some (handle_evolve ctx args)
  | "masc_swarm_propose" -> Some (handle_propose ctx args)
  | "masc_swarm_vote" -> Some (handle_vote ctx args)
  | "masc_swarm_deposit" -> Some (handle_deposit ctx args)
  | "masc_swarm_trails" -> Some (handle_trails ctx args)
  | "masc_swarm_walph" -> Some (handle_walph ctx args)
  | _ -> None
