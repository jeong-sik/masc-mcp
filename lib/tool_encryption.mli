(** Encryption tools - Data encryption management *)

val get_string : Yojson.Safe.t -> string -> string -> string

type context = {
  state: Mcp_server.server_state;
}

type result = bool * string

val handle_encryption_status : context -> Yojson.Safe.t -> result
val handle_encryption_enable : context -> Yojson.Safe.t -> result
val handle_encryption_disable : context -> Yojson.Safe.t -> result
val handle_generate_key : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
