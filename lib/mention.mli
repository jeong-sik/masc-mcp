(** Mention parsing module - Stateless/Stateful/Broadcast routing modes *)

(** Mention routing mode *)
type mode =
  | Stateless of string       (** @agent → pick one available *)
  | Stateful of string        (** @agent-adj-animal → specific agent *)
  | Broadcast of string       (** @@agent → all of type *)
  | None                      (** No mention found *)

val mode_to_string : mode -> string
(** Convert mode to human-readable string *)

val agent_type_of_mention : string -> string
(** Extract base agent type from mention/nickname
    e.g., "ollama-gentle-gecko" → "ollama" *)

val is_nickname : string -> bool
(** Check if mention follows nickname pattern (agent-adj-animal) *)

val parse : string -> mode
(** Parse @mention from message content

    Priority:
    1. @@agent → Broadcast
    2. @agent-adj-animal → Stateful
    3. @agent → Stateless
*)

val extract : string -> string option
(** Extract raw mention target (backward-compatible) *)

val resolve_targets : mode -> available_agents:string list -> string list
(** Get target agents based on mode and available agents *)

val spawnable_agents : string list
(** List of agent types that can be auto-spawned *)

val is_spawnable : string -> bool
(** Check if an agent type is spawnable by Auto-Responder *)
