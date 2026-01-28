(** Handover tools - Cellular agent handover DNA *)

val get_string : Yojson.Safe.t -> string -> string -> string
val get_string_opt : Yojson.Safe.t -> string -> string option
val get_int : Yojson.Safe.t -> string -> int -> int
val get_bool : Yojson.Safe.t -> string -> bool -> bool
val get_string_list : Yojson.Safe.t -> string -> string list

type context = {
  config: Room.config;
  agent_name: string;
  fs: Eio.Fs.dir_ty Eio.Path.t option;
  proc_mgr: Eio_unix.Process.mgr_ty Eio.Resource.t option;
  sw: Eio.Switch.t;
}

type result = bool * string

val handle_handover_create : context -> Yojson.Safe.t -> result
val handle_handover_list : context -> Yojson.Safe.t -> result
val handle_handover_claim : context -> Yojson.Safe.t -> result
val handle_handover_claim_and_spawn : context -> Yojson.Safe.t -> result
val handle_handover_get : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
