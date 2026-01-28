(** A2A tools - Agent-to-Agent protocol *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_string_opt : Yojson.Safe.t -> string -> string option
val get_int : Yojson.Safe.t -> string -> int -> int
val get_bool : Yojson.Safe.t -> string -> bool -> bool

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

val handle_a2a_discover : context -> Yojson.Safe.t -> result
val handle_a2a_query_skill : context -> Yojson.Safe.t -> result
val handle_a2a_delegate : context -> Yojson.Safe.t -> result
val handle_a2a_subscribe : context -> Yojson.Safe.t -> result
val handle_a2a_unsubscribe : context -> Yojson.Safe.t -> result
val handle_poll_events : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
