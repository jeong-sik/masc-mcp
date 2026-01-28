(** Heartbeat tools - Agent health monitoring *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_int : Yojson.Safe.t -> string -> int -> int

type 'a context = {
  config: Room.config;
  agent_name: string;
  sw: Eio.Switch.t;
  clock: 'a Eio.Time.clock;
}

type result = bool * string

val handle_heartbeat : _ context -> Yojson.Safe.t -> result
val handle_heartbeat_start : _ context -> Yojson.Safe.t -> result
val handle_heartbeat_stop : _ context -> Yojson.Safe.t -> result
val handle_heartbeat_list : _ context -> Yojson.Safe.t -> result

val dispatch : _ context -> name:string -> args:Yojson.Safe.t -> result option
