(** Tool_misc - Miscellaneous operations

    Handles: dashboard, verify_handoff, gc, cleanup_zombies
*)

open Yojson.Safe.Util

type result = bool * string

type context = {
  config: Room.config;
  agent_name: string;
}

(* JSON helpers *)
let get_int args key default =
  match args |> member key with
  | `Int i -> i
  | _ -> default

let get_bool args key default =
  match args |> member key with
  | `Bool b -> b
  | _ -> default

(* Handlers *)

let handle_dashboard ctx args =
  let compact = get_bool args "compact" false in
  let output =
    if compact then Dashboard.generate_compact ctx.config
    else Dashboard.generate ctx.config
  in
  (true, output)

(* Note: verify_handoff requires tokenize/jaccard/cosine functions from mcp_server_eio.ml
   and is kept there for now. This is a stub. *)
let handle_verify_handoff _ctx _args =
  (false, "verify_handoff requires complex similarity functions - use mcp_server_eio directly")

let handle_gc ctx args =
  let days = get_int args "days" 7 in
  (true, Room.gc ctx.config ~days ())

let handle_cleanup_zombies ctx _args =
  (true, Room.cleanup_zombies ctx.config)

(* Dispatch function *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_dashboard" -> Some (handle_dashboard ctx args)
  | "masc_gc" -> Some (handle_gc ctx args)
  | "masc_cleanup_zombies" -> Some (handle_cleanup_zombies ctx args)
  (* Note: verify_handoff needs tokenize/jaccard/cosine - kept in mcp_server_eio.ml *)
  | _ -> None
