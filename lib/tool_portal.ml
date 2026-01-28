(** Portal tools - Agent-to-agent direct messaging *)

(* Argument helpers *)
let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

let get_string_opt args key =
  match Yojson.Safe.Util.member key args with
  | `String s when s <> "" -> Some s
  | _ -> None

(* Context required by portal tools *)
type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

(* Individual handlers *)
let handle_portal_open ctx args =
  let target_agent = get_string args "target_agent" "" in
  let initial_message = get_string_opt args "initial_message" in
  match Room.portal_open_r ctx.config ~agent_name:ctx.agent_name ~target_agent ~initial_message with
  | Ok msg -> (true, msg)
  | Error e -> (false, Types.masc_error_to_string e)

let handle_portal_send ctx args =
  let message = get_string args "message" "" in
  (* macOS notification for portal message *)
  (match Room.get_portal_target ctx.config ~agent_name:ctx.agent_name with
   | Some target -> Notify.notify_portal ~from_agent:ctx.agent_name ~target_agent:target ~message ()
   | None -> ());
  match Room.portal_send_r ctx.config ~agent_name:ctx.agent_name ~message with
  | Ok msg -> (true, msg)
  | Error e -> (false, Types.masc_error_to_string e)

let handle_portal_close ctx _args =
  (true, Room.portal_close ctx.config ~agent_name:ctx.agent_name)

let handle_portal_status ctx _args =
  let json = Room.portal_status ctx.config ~agent_name:ctx.agent_name in
  (true, Yojson.Safe.pretty_to_string json)

(* Dispatch function - returns None if tool not handled *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_portal_open" -> Some (handle_portal_open ctx args)
  | "masc_portal_send" -> Some (handle_portal_send ctx args)
  | "masc_portal_close" -> Some (handle_portal_close ctx args)
  | "masc_portal_status" -> Some (handle_portal_status ctx args)
  | _ -> None
