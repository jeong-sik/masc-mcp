(** Auth tools - Authentication and authorization *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_bool : Yojson.Safe.t -> string -> bool -> bool

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

val handle_auth_enable : context -> Yojson.Safe.t -> result
val handle_auth_disable : context -> Yojson.Safe.t -> result
val handle_auth_status : context -> Yojson.Safe.t -> result
val handle_auth_create_token : context -> Yojson.Safe.t -> result
val handle_auth_refresh : context -> Yojson.Safe.t -> result
val handle_auth_revoke : context -> Yojson.Safe.t -> result
val handle_auth_list : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
