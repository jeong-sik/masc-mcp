(** Run Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    6 tools: run_init, run_plan, run_log, run_deliverable, run_get, run_list
*)

(** Tool handler context *)
type context = {
  config: Room.config;
}

(** Tool result type *)
type result = bool * string

(** {1 Argument Helpers} *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_string_opt : Yojson.Safe.t -> string -> string option

(** {1 Individual Handlers} *)

val handle_run_init : context -> Yojson.Safe.t -> result
val handle_run_plan : context -> Yojson.Safe.t -> result
val handle_run_log : context -> Yojson.Safe.t -> result
val handle_run_deliverable : context -> Yojson.Safe.t -> result
val handle_run_get : context -> Yojson.Safe.t -> result
val handle_run_list : context -> Yojson.Safe.t -> result

(** {1 Dispatcher} *)

(** Dispatch run tool by name. Returns None if not a run tool. *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
