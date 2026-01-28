(** Plan Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    11 tools: plan_init, plan_update, note_add, deliver, plan_get,
              error_add, error_resolve, plan_set_task, plan_get_task, plan_clear_task
*)

(** Tool handler context *)
type context = {
  config: Room.config;
}

(** Tool result type *)
type result = bool * string

(** {1 Argument Helpers} *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_int : Yojson.Safe.t -> string -> int -> int

(** {1 Individual Handlers} *)

val handle_plan_init : context -> Yojson.Safe.t -> result
val handle_plan_update : context -> Yojson.Safe.t -> result
val handle_note_add : context -> Yojson.Safe.t -> result
val handle_deliver : context -> Yojson.Safe.t -> result
val handle_plan_get : context -> Yojson.Safe.t -> result
val handle_error_add : context -> Yojson.Safe.t -> result
val handle_error_resolve : context -> Yojson.Safe.t -> result
val handle_plan_set_task : context -> Yojson.Safe.t -> result
val handle_plan_get_task : context -> Yojson.Safe.t -> result
val handle_plan_clear_task : context -> Yojson.Safe.t -> result

(** {1 Dispatcher} *)

(** Dispatch plan tool by name. Returns None if not a plan tool. *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
