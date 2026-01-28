(** Heartbeat - Agent health monitoring *)

type t = {
  id: string;
  agent_name: string;
  interval: int;
  message: string;
  mutable active: bool;
  created_at: float;
}

val generate_id : unit -> string
val start : agent_name:string -> interval:int -> message:string -> string
val stop : string -> bool
val list : unit -> t list
val get : string -> t option
