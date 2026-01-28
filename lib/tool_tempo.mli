(** Tempo Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    5 tools: tempo, tempo_get, tempo_set, tempo_adjust, tempo_reset
*)

(** Tool handler context *)
type context = {
  config: Room.config;
  agent_name: string;
}

(** Tool result type *)
type result = bool * string

(** {1 Argument Helpers} *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_string_opt : Yojson.Safe.t -> string -> string option
val get_float : Yojson.Safe.t -> string -> float -> float

(** {1 Individual Handlers} *)

val handle_tempo_get : context -> Yojson.Safe.t -> result
val handle_tempo_set : context -> Yojson.Safe.t -> result
val handle_tempo_adjust : context -> Yojson.Safe.t -> result
val handle_tempo_reset : context -> Yojson.Safe.t -> result
val handle_tempo : context -> Yojson.Safe.t -> result

(** {1 Dispatcher} *)

(** Dispatch tempo tool by name. Returns None if not a tempo tool. *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
