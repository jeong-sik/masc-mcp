(** Vote tools - Consensus voting system *)

(* Argument helpers *)
let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

let get_int args key default =
  match Yojson.Safe.Util.member key args with
  | `Int n -> n
  | _ -> default

let get_string_list args key =
  match Yojson.Safe.Util.member key args with
  | `List items ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

(* Context required by vote tools *)
type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

(* Individual handlers *)
let handle_vote_create ctx args =
  let proposer = get_string args "proposer" ctx.agent_name in
  let topic = get_string args "topic" "" in
  let options = get_string_list args "options" in
  let required_votes = get_int args "required_votes" 2 in
  (true, Room.vote_create ctx.config ~proposer ~topic ~options ~required_votes)

let handle_vote_cast ctx args =
  let vote_id = get_string args "vote_id" "" in
  let choice = get_string args "choice" "" in
  (true, Room.vote_cast ctx.config ~agent_name:ctx.agent_name ~vote_id ~choice)

let handle_vote_status ctx args =
  let vote_id = get_string args "vote_id" "" in
  let json = Room.vote_status ctx.config ~vote_id in
  (true, Yojson.Safe.pretty_to_string json)

let handle_votes ctx _args =
  let json = Room.list_votes ctx.config in
  (true, Yojson.Safe.pretty_to_string json)

(* Dispatch function - returns None if tool not handled *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_vote_create" -> Some (handle_vote_create ctx args)
  | "masc_vote_cast" -> Some (handle_vote_cast ctx args)
  | "masc_vote_status" -> Some (handle_vote_status ctx args)
  | "masc_votes" -> Some (handle_votes ctx args)
  | _ -> None
