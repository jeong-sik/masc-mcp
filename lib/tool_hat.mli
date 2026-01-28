(** Hat tools - Agent persona management *)

val get_string : Yojson.Safe.t -> string -> string -> string

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

val handle_hat_wear : context -> Yojson.Safe.t -> result
val handle_hat_status : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
