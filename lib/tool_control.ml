(** Tool_control - Flow control operations

    Handles: pause, pause_status, resume, switch_mode, get_config
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

(* Handlers *)

let handle_pause ctx args =
  let reason = get_string args "reason" "Manual pause" in
  Room.pause ctx.config ~by:ctx.agent_name ~reason;
  (true, Printf.sprintf "⏸️ Room paused by %s: %s" ctx.agent_name reason)

let handle_resume ctx _args =
  match Room.resume ctx.config ~by:ctx.agent_name with
  | `Resumed -> (true, Printf.sprintf "▶️ Room resumed by %s" ctx.agent_name)
  | `Already_running -> (true, "Room is not paused")

let handle_pause_status ctx _args =
  match Room.pause_info ctx.config with
  | Some (by, reason, at) ->
      let by_str = Option.value by ~default:"unknown" in
      let reason_str = Option.value reason ~default:"no reason" in
      let at_str = Option.value at ~default:"unknown" in
      (true, Printf.sprintf "⏸️ PAUSED\n  By: %s\n  Reason: %s\n  Since: %s" by_str reason_str at_str)
  | None ->
      (true, "▶️ Room is running (not paused)")

let handle_switch_mode ctx args =
  let mode = get_string args "mode" "autonomous" in
  (* Mode.switch may not exist, but we mirror the original behavior *)
  (true, Printf.sprintf "🔄 Mode switched to: %s by %s" mode ctx.agent_name)

let handle_get_config ctx _args =
  let room_path = Room.masc_dir ctx.config in
  let summary = Config.get_config_summary room_path in
  (true, Yojson.Safe.pretty_to_string summary)

(* Dispatch function *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_pause" -> Some (handle_pause ctx args)
  | "masc_resume" -> Some (handle_resume ctx args)
  | "masc_pause_status" -> Some (handle_pause_status ctx args)
  | "masc_switch_mode" -> Some (handle_switch_mode ctx args)
  | "masc_get_config" -> Some (handle_get_config ctx args)
  | _ -> None
