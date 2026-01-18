(** Turn Queue - Sequential Voice Output with Priority

    Core constraint: "병렬 수집 → 순차 출력"
    Multiple agents can request turns, but output is serialized.
*)

(** {1 Types} *)

type turn_status =
  | Queued
  | Speaking
  | Completed
  | Failed of string
[@@deriving show, eq]

type turn_request = {
  id: string;
  agent_id: string;
  message: string;
  voice: string;
  priority: int;
  timestamp: float;
  mutable status: turn_status;
  mutable duration_ms: int option;
}

type t

(** {1 Constants} *)

val default_priority : int
val default_voice : string

(** {1 Queue Creation} *)

val create : unit -> t
(** Create empty queue *)

(** {1 Enqueue Operations} *)

val enqueue : t -> agent_id:string -> message:string -> ?voice:string -> ?priority:int -> unit -> turn_request
(** Enqueue turn request with explicit priority *)

val enqueue_auto_priority : t -> agent_id:string -> message:string -> ?voice:string -> unit -> turn_request
(** Enqueue with auto-priority based on agent name (MAGI agents) *)

(** {1 Dequeue Operations} *)

val dequeue : t -> turn_request option
(** Get and remove next turn *)

val peek : t -> turn_request option
(** Peek at next turn without removing *)

(** {1 Queue Status} *)

val length : t -> int
val is_empty : t -> bool
val is_processing : t -> bool
val current_speaker : t -> turn_request option

(** {1 Turn Completion} *)

val complete_turn : t -> duration_ms:int -> unit
(** Mark current turn as completed *)

val fail_turn : t -> error:string -> unit
(** Mark current turn as failed *)

(** {1 Queue Management} *)

val clear : t -> unit
(** Clear all pending turns *)

val remove_agent_turns : t -> agent_id:string -> unit
(** Remove all turns from specific agent *)

val get_agent_turns : t -> agent_id:string -> turn_request list
(** Get all turns from specific agent *)

val get_all_turns : t -> turn_request list
(** Get all queued turns *)

(** {1 Processing} *)

val process_loop : t -> handler:(turn_request -> unit Lwt.t) -> stop_signal:bool ref -> unit Lwt.t
(** Process queue loop - calls handler for each turn *)

val start_processing : t -> handler:(turn_request -> unit Lwt.t) -> bool ref
(** Start processing in background, returns stop signal *)

(** {1 Status} *)

val status_json : t -> Yojson.Safe.t
(** Get queue status as JSON *)

val string_of_status : turn_status -> string

(** {1 Pretty Printing} *)

val pp_turn : Format.formatter -> turn_request -> unit
val pp : Format.formatter -> t -> unit
