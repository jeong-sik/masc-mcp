(** Hat tools - Agent persona management *)

let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

let handle_hat_wear ctx args =
  let hat_str = get_string args "hat" "builder" in
  let hat = Hat.of_string hat_str in
  let result = Hat.wear ~agent_name:ctx.agent_name hat in
  let _ = Room.broadcast ctx.config ~from_agent:ctx.agent_name
    ~content:(Printf.sprintf "%s %s" (Hat.to_emoji hat) result) in
  (true, result)

let handle_hat_status _ctx _args =
  let agents = Hat.list_all () in
  if agents = [] then
    (true, "🎩 No agents have worn hats yet")
  else begin
    let buf = Buffer.create 256 in
    Buffer.add_string buf "🎩 **Current Hats**\n";
    List.iter (fun (agent : Hat.hatted_agent) ->
      Buffer.add_string buf (Printf.sprintf "  %s %s: %s\n"
        (Hat.to_emoji agent.current_hat) agent.agent_name (Hat.to_string agent.current_hat))
    ) agents;
    (true, Buffer.contents buf)
  end

let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_hat_wear" -> Some (handle_hat_wear ctx args)
  | "masc_hat_status" -> Some (handle_hat_status ctx args)
  | _ -> None
