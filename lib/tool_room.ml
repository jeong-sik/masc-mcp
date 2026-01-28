(** Tool_room - Room management operations

    Handles: status, reset, init, rooms_list, room_create, room_enter

    Note: join, leave, set_room, who require state/registry and remain in mcp_server_eio.ml
*)

open Yojson.Safe.Util

type result = bool * string

type context = {
  config: Room.config;
  agent_name: string;
}

(* JSON helpers *)
let get_string args key default =
  match args |> member key with
  | `String s -> s
  | _ -> default

let get_bool args key default =
  match args |> member key with
  | `Bool b -> b
  | _ -> default

(* Handlers *)

let handle_status ctx _args =
  (true, Room.status ctx.config)

let handle_init ctx args =
  let agent = match get_string args "agent_name" "" with
    | "" -> None
    | s -> Some s
  in
  (true, Room.init ctx.config ~agent_name:agent)

let handle_reset ctx args =
  let confirm = get_bool args "confirm" false in
  if not confirm then
    (false, "⚠️ This will DELETE the entire .masc/ folder!\nCall with confirm=true to proceed.")
  else
    (true, Room.reset ctx.config)

let handle_rooms_list ctx _args =
  let result = Room.rooms_list ctx.config in
  (true, Yojson.Safe.pretty_to_string result)

let handle_room_create ctx args =
  let name = get_string args "name" "" in
  if name = "" then
    (false, "❌ Room name is required")
  else
    let description = match args |> member "description" with
      | `String d -> Some d
      | _ -> None
    in
    let result = Room.room_create ctx.config ~name ~description in
    let success = match result with
      | `Assoc fields -> not (List.mem_assoc "error" fields)
      | _ -> false
    in
    (success, Yojson.Safe.pretty_to_string result)

let handle_room_enter ctx args =
  let room_id = get_string args "room_id" "" in
  if room_id = "" then
    (false, "❌ Room ID is required")
  else
    let agent_type = get_string args "agent_type" "claude" in
    let result = Room.room_enter ctx.config ~room_id ~agent_type in
    let success = match result with
      | `Assoc fields -> not (List.mem_assoc "error" fields)
      | _ -> false
    in
    (success, Yojson.Safe.pretty_to_string result)

(* Dispatch function *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_status" -> Some (handle_status ctx args)
  | "masc_init" -> Some (handle_init ctx args)
  | "masc_reset" -> Some (handle_reset ctx args)
  | "masc_rooms_list" -> Some (handle_rooms_list ctx args)
  | "masc_room_create" -> Some (handle_room_create ctx args)
  | "masc_room_enter" -> Some (handle_room_enter ctx args)
  | _ -> None
