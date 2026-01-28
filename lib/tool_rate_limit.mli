(** Tool_rate_limit - Rate limiting status and configuration handlers *)

type context = {
  config: Room.config;
  agent_name: string;
  registry: Session.registry;
}

(** Dispatch handler. Returns Some (success, result) if handled, None otherwise *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> (bool * string) option

(** Handle masc_rate_limit_status *)
val handle_rate_limit_status : context -> Yojson.Safe.t -> bool * string

(** Handle masc_rate_limit_config *)
val handle_rate_limit_config : context -> Yojson.Safe.t -> bool * string
