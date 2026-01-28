(** Tool_lock - Resource locking handlers *)

type context = {
  config: Room.config;
  agent_name: string;
}

(** Dispatch handler. Returns Some (success, result) if handled, None otherwise *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> (bool * string) option

(** Handle masc_lock *)
val handle_lock : context -> Yojson.Safe.t -> bool * string

(** Handle masc_unlock *)
val handle_unlock : context -> Yojson.Safe.t -> bool * string

(** Helper: get string from args *)
val get_string : Yojson.Safe.t -> string -> string -> string
