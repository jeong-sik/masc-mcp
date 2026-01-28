(** Vote tools - Consensus voting system *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_int : Yojson.Safe.t -> string -> int -> int
val get_string_list : Yojson.Safe.t -> string -> string list

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

val handle_vote_create : context -> Yojson.Safe.t -> result
val handle_vote_cast : context -> Yojson.Safe.t -> result
val handle_vote_status : context -> Yojson.Safe.t -> result
val handle_votes : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
