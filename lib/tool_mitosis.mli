(** Mitosis Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    7 tools: mitosis_status, mitosis_all, mitosis_pool, mitosis_divide,
             mitosis_check, mitosis_record, mitosis_prepare
*)

(** Tool handler context *)
type context = {
  config: Room.config;
}

(** Tool result type *)
type result = bool * string

(** {1 Argument Helpers} *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_float : Yojson.Safe.t -> string -> float -> float
val get_bool : Yojson.Safe.t -> string -> bool -> bool

(** {1 Individual Handlers} *)

val handle_mitosis_status : context -> Yojson.Safe.t -> result
val handle_mitosis_all : context -> Yojson.Safe.t -> result
val handle_mitosis_pool : context -> Yojson.Safe.t -> result
val handle_mitosis_divide : context -> Yojson.Safe.t -> result
val handle_mitosis_check : context -> Yojson.Safe.t -> result
val handle_mitosis_record : context -> Yojson.Safe.t -> result
val handle_mitosis_prepare : context -> Yojson.Safe.t -> result

(** {1 Dispatcher} *)

(** Dispatch mitosis tool by name. Returns None if not a mitosis tool. *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
