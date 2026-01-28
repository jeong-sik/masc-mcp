(** Relay tools - Infinite context via handoff *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_string_opt : Yojson.Safe.t -> string -> string option
val get_int : Yojson.Safe.t -> string -> int -> int
val get_string_list : Yojson.Safe.t -> string -> string list

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

val handle_relay_status : context -> Yojson.Safe.t -> result
val handle_relay_checkpoint : context -> Yojson.Safe.t -> result
val handle_relay_now : context -> Yojson.Safe.t -> result
val handle_relay_smart_check : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
