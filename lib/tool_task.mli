(** Tool_task - Core task CRUD operations *)

type result = bool * string

type context = {
  config: Room.config;
  agent_name: string;
}

val get_string : Yojson.Safe.t -> string -> string -> string
val get_int : Yojson.Safe.t -> string -> int -> int
val get_int_opt : Yojson.Safe.t -> string -> int option

val handle_add_task : context -> Yojson.Safe.t -> result
val handle_batch_add_tasks : context -> Yojson.Safe.t -> result
val handle_claim : context -> Yojson.Safe.t -> result
val handle_claim_next : context -> Yojson.Safe.t -> result
val handle_release : context -> Yojson.Safe.t -> result
val handle_done : context -> Yojson.Safe.t -> result
val handle_cancel_task : context -> Yojson.Safe.t -> result
val handle_transition : context -> Yojson.Safe.t -> result
val handle_update_priority : context -> Yojson.Safe.t -> result
val handle_tasks : context -> Yojson.Safe.t -> result
val handle_task_history : context -> Yojson.Safe.t -> result
val handle_archive_view : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
