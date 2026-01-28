(** Tool_walph - Walph loop control handlers *)

type 'a context = {
  config: Room.config;
  agent_name: string;
  net: 'a Eio.Net.t;
}

(** Dispatch handler. Returns Some (success, result) if handled, None otherwise *)
val dispatch : 'a context -> name:string -> args:Yojson.Safe.t -> (bool * string) option

(** Handle masc_walph_loop *)
val handle_walph_loop : 'a context -> Yojson.Safe.t -> bool * string

(** Handle masc_walph_control *)
val handle_walph_control : 'a context -> Yojson.Safe.t -> bool * string

(** Handle masc_walph_natural *)
val handle_walph_natural : 'a context -> Yojson.Safe.t -> bool * string

(** Helper: get string from args *)
val get_string : Yojson.Safe.t -> string -> string -> string

(** Helper: get string option from args *)
val get_string_opt : Yojson.Safe.t -> string -> string option

(** Helper: get int from args *)
val get_int : Yojson.Safe.t -> string -> int -> int
