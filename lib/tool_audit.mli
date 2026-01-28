(** Tool_audit - Audit query and statistics handlers *)

type context = {
  config: Room.config;
}

(** Audit event record *)
type audit_event = {
  timestamp: float;
  agent: string;
  event_type: string;
  success: bool;
  detail: string option;
}

(** Dispatch handler. Returns Some (success, result) if handled, None otherwise *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> (bool * string) option

(** Read audit events since given timestamp *)
val read_audit_events : Room.config -> since:float -> audit_event list

(** Convert audit event to JSON *)
val audit_event_to_json : audit_event -> Yojson.Safe.t

(** Handle masc_audit_query *)
val handle_audit_query : context -> Yojson.Safe.t -> bool * string

(** Handle masc_audit_stats *)
val handle_audit_stats : context -> Yojson.Safe.t -> bool * string

(** Helper: get string from args *)
val get_string : Yojson.Safe.t -> string -> string -> string

(** Helper: get string option from args *)
val get_string_opt : Yojson.Safe.t -> string -> string option

(** Helper: get int from args *)
val get_int : Yojson.Safe.t -> string -> int -> int

(** Helper: get float from args *)
val get_float : Yojson.Safe.t -> string -> float -> float
