(** Cache Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    6 tools: cache_set, cache_get, cache_delete, cache_list, cache_clear, cache_stats
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
val get_int_opt : Yojson.Safe.t -> string -> int option
val get_string_list : Yojson.Safe.t -> string -> string list

(** {1 Individual Handlers} *)

val handle_cache_set : context -> Yojson.Safe.t -> result
val handle_cache_get : context -> Yojson.Safe.t -> result
val handle_cache_delete : context -> Yojson.Safe.t -> result
val handle_cache_list : context -> Yojson.Safe.t -> result
val handle_cache_clear : context -> Yojson.Safe.t -> result
val handle_cache_stats : context -> Yojson.Safe.t -> result

(** {1 Dispatcher} *)

(** Dispatch cache tool by name. Returns None if not a cache tool. *)
val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
