(** Portal tools - Agent-to-agent direct messaging *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_string_opt : Yojson.Safe.t -> string -> string option

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

val handle_portal_open : context -> Yojson.Safe.t -> result
val handle_portal_send : context -> Yojson.Safe.t -> result
val handle_portal_close : context -> Yojson.Safe.t -> result
val handle_portal_status : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
