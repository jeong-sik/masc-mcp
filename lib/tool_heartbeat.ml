(** Heartbeat tools - Agent health monitoring *)

let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

let get_int args key default =
  match Yojson.Safe.Util.member key args with
  | `Int n -> n
  | _ -> default

type 'a context = {
  config: Room.config;
  agent_name: string;
  sw: Eio.Switch.t;
  clock: 'a Eio.Time.clock;
}

type result = bool * string

let handle_heartbeat ctx _args =
  (true, Room.heartbeat ctx.config ~agent_name:ctx.agent_name)

let handle_heartbeat_start ctx args =
  let interval = get_int args "interval" 30 in
  let message = get_string args "message" "🏓 heartbeat" in
  (* Validate interval: min 5, max 300 *)
  let interval = max 5 (min 300 interval) in
  let hb_id = Heartbeat.start ~agent_name:ctx.agent_name ~interval ~message in
  (* Start background fiber for actual heartbeat *)
  Eio.Fiber.fork ~sw:ctx.sw (fun () ->
    let rec loop () =
      match Heartbeat.get hb_id with
      | Some hb when hb.Heartbeat.active ->
          (try
             ignore (Room.broadcast ctx.config ~from_agent:ctx.agent_name ~content:message)
           with exn ->
             Printf.eprintf "[Heartbeat] broadcast error: %s\n%!"
               (Printexc.to_string exn));
          Eio.Time.sleep ctx.clock (float_of_int interval);
          loop ()
      | _ -> ()
    in
    try loop () with exn ->
      Printf.eprintf "[Heartbeat] loop error: %s\n%!" (Printexc.to_string exn)
  );
  (true, Printf.sprintf "✅ Heartbeat started: %s (interval: %ds, message: %s)" hb_id interval message)

let handle_heartbeat_stop _ctx args =
  let hb_id = get_string args "heartbeat_id" "" in
  if hb_id = "" then
    (false, "❌ heartbeat_id required")
  else if Heartbeat.stop hb_id then
    (true, Printf.sprintf "✅ Heartbeat stopped: %s" hb_id)
  else
    (false, Printf.sprintf "❌ Heartbeat not found: %s" hb_id)

let handle_heartbeat_list _ctx _args =
  let hbs = Heartbeat.list () in
  let fmt_hb hb =
    let uptime = int_of_float (Unix.gettimeofday () -. hb.Heartbeat.created_at) in
    Printf.sprintf "  • %s: agent=%s interval=%ds message=\"%s\" uptime=%ds"
      hb.Heartbeat.id hb.agent_name hb.interval hb.message uptime
  in
  let list_str =
    if List.length hbs = 0 then "No active heartbeats"
    else "Active heartbeats:\n" ^ String.concat "\n" (List.map fmt_hb hbs)
  in
  (true, list_str)

let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_heartbeat" -> Some (handle_heartbeat ctx args)
  | "masc_heartbeat_start" -> Some (handle_heartbeat_start ctx args)
  | "masc_heartbeat_stop" -> Some (handle_heartbeat_stop ctx args)
  | "masc_heartbeat_list" -> Some (handle_heartbeat_list ctx args)
  | _ -> None
