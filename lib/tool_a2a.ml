(** A2A tools - Agent-to-Agent protocol *)

(* Argument helpers *)
let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

let get_string_opt args key =
  match Yojson.Safe.Util.member key args with
  | `String s when s <> "" -> Some s
  | _ -> None

let get_int args key default =
  match Yojson.Safe.Util.member key args with
  | `Int n -> n
  | _ -> default

let get_bool args key default =
  match Yojson.Safe.Util.member key args with
  | `Bool b -> b
  | _ -> default

(* Context required by a2a tools *)
type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

(* Individual handlers *)
let handle_a2a_discover ctx args =
  let endpoint = get_string_opt args "endpoint" in
  let capability = get_string_opt args "capability" in
  match A2a_tools.discover ctx.config ?endpoint ?capability () with
  | Ok json -> (true, Yojson.Safe.pretty_to_string json)
  | Error e -> (false, Printf.sprintf "❌ Discovery failed: %s" e)

let handle_a2a_query_skill ctx args =
  let skill_agent_name = get_string args "agent_name" "" in
  let skill_id = get_string args "skill_id" "" in
  match A2a_tools.query_skill ctx.config ~agent_name:skill_agent_name ~skill_id with
  | Ok json -> (true, Yojson.Safe.pretty_to_string json)
  | Error e -> (false, Printf.sprintf "❌ Query skill failed: %s" e)

let handle_a2a_delegate ctx args =
  let delegate_agent_name = get_string args "agent_name" ctx.agent_name in
  let target = get_string args "target_agent" "" in
  let message = get_string args "message" "" in
  let task_type_str = get_string args "task_type" "async" in
  let timeout = get_int args "timeout" 300 in
  let artifacts = match Yojson.Safe.Util.member "artifacts" args with
    | `Null -> []
    | `List items ->
        List.filter_map (fun item ->
          match A2a_tools.artifact_of_yojson item with
          | Ok a -> Some a
          | Error _ -> None) items
    | _ -> []
  in
  match A2a_tools.delegate ctx.config ~agent_name:delegate_agent_name ~target ~message
           ~task_type_str ~artifacts ~timeout () with
  | Ok json -> (true, Yojson.Safe.pretty_to_string json)
  | Error e -> (false, Printf.sprintf "❌ Delegation failed: %s" e)

let handle_a2a_subscribe _ctx args =
  let agent_filter = get_string_opt args "agent_name" in
  let events = match Yojson.Safe.Util.member "events" args with
    | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
    | _ -> []
  in
  (try
    match A2a_tools.subscribe ?agent_filter ~events () with
    | Ok json -> (true, Yojson.Safe.pretty_to_string json)
    | Error e -> (false, Printf.sprintf "❌ Subscribe failed: %s" e)
  with exn ->
    (false, Printf.sprintf "❌ Subscribe exception: %s" (Printexc.to_string exn)))

let handle_a2a_unsubscribe _ctx args =
  let subscription_id = get_string args "subscription_id" "" in
  match A2a_tools.unsubscribe ~subscription_id with
  | Ok json -> (true, Yojson.Safe.pretty_to_string json)
  | Error e -> (false, Printf.sprintf "❌ Unsubscribe failed: %s" e)

let handle_poll_events _ctx args =
  let subscription_id = get_string args "subscription_id" "" in
  let clear = get_bool args "clear" true in
  match A2a_tools.poll_events ~subscription_id ~clear () with
  | Ok json -> (true, Yojson.Safe.pretty_to_string json)
  | Error e -> (false, Printf.sprintf "❌ Poll events failed: %s" e)

(* Dispatch function - returns None if tool not handled *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_a2a_discover" -> Some (handle_a2a_discover ctx args)
  | "masc_a2a_query_skill" -> Some (handle_a2a_query_skill ctx args)
  | "masc_a2a_delegate" -> Some (handle_a2a_delegate ctx args)
  | "masc_a2a_subscribe" -> Some (handle_a2a_subscribe ctx args)
  | "masc_a2a_unsubscribe" -> Some (handle_a2a_unsubscribe ctx args)
  | "masc_poll_events" -> Some (handle_poll_events ctx args)
  | _ -> None
