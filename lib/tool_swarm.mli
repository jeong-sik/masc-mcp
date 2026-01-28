(** Swarm Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    10 tools: init, join, leave, status, evolve, propose, vote, deposit, trails, walph
*)

(** Tool handler context - shared dependencies *)
type context = {
  config: Room.config;
  fs: Eio.Fs.dir_ty Eio.Path.t option;
  agent_name: string;
}

(** Tool result type: (success, message) *)
type result = bool * string

(** {1 Argument Helpers} *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_float : Yojson.Safe.t -> string -> float -> float
val get_int : Yojson.Safe.t -> string -> int -> int
val get_bool : Yojson.Safe.t -> string -> bool -> bool

(** {1 Individual Handlers} *)

val handle_init : context -> Yojson.Safe.t -> result
val handle_join : context -> Yojson.Safe.t -> result
val handle_leave : context -> Yojson.Safe.t -> result
val handle_status : context -> Yojson.Safe.t -> result
val handle_evolve : context -> Yojson.Safe.t -> result
val handle_propose : context -> Yojson.Safe.t -> result
val handle_vote : context -> Yojson.Safe.t -> result
val handle_deposit : context -> Yojson.Safe.t -> result
val handle_trails : context -> Yojson.Safe.t -> result
val handle_walph : context -> Yojson.Safe.t -> result

(** {1 Dispatcher} *)

(** Dispatch swarm tool by name. Returns None if not a swarm tool. *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
