(** Heartbeat - Agent health monitoring

    Extracted from mcp_server_eio.ml for testability.
*)

type t = {
  id: string;
  agent_name: string;
  interval: int;
  message: string;
  mutable active: bool;
  created_at: float;
}

let heartbeats : (string, t) Hashtbl.t = Hashtbl.create 16
let heartbeat_counter = ref 0

let generate_id () =
  incr heartbeat_counter;
  Printf.sprintf "hb-%d-%d" (int_of_float (Unix.gettimeofday ())) !heartbeat_counter

let start ~agent_name ~interval ~message =
  let id = generate_id () in
  let hb = { id; agent_name; interval; message; active = true; created_at = Unix.gettimeofday () } in
  Hashtbl.add heartbeats id hb;
  id

let stop id =
  match Hashtbl.find_opt heartbeats id with
  | Some hb ->
      hb.active <- false;
      Hashtbl.remove heartbeats id;
      true
  | None -> false

let list () =
  Hashtbl.fold (fun _ hb acc -> hb :: acc) heartbeats []

let get id = Hashtbl.find_opt heartbeats id
