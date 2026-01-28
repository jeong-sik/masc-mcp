(** Tool_cost - Cost tracking and reporting handlers *)

type context = {
  agent_name: string;
}

(** Dispatch handler. Returns Some (success, result) if handled, None otherwise *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> (bool * string) option

(** Handle masc_cost_log *)
val handle_cost_log : context -> Yojson.Safe.t -> bool * string

(** Handle masc_cost_report *)
val handle_cost_report : context -> Yojson.Safe.t -> bool * string

(** Helper: get string from args *)
val get_string : Yojson.Safe.t -> string -> string -> string

(** Helper: get int from args *)
val get_int : Yojson.Safe.t -> string -> int -> int

(** Helper: get float from args *)
val get_float : Yojson.Safe.t -> string -> float -> float
