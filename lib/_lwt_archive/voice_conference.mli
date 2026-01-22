(** MASC Voice Conference - Multi-Agent Voice Orchestration

    Combines Turn Queue + Session Manager for conference mode.
    Manages participant lifecycle, turn-based speaking, and transcript recording.

    @author Second Brain
    @since MASC v3.0
*)

(** {1 Types} *)

(** Conference state *)
type conference_state =
  | Idle       (** Not started *)
  | Active     (** Conference in progress *)
  | Paused     (** Temporarily paused *)
  | Ended      (** Conference ended *)

(** Transcript entry for a spoken turn *)
type transcript_entry = {
  seq: int;                (** Sequence number *)
  agent_id: string;        (** Agent who spoke *)
  message: string;         (** Message content *)
  voice: string;           (** Voice used *)
  timestamp: float;        (** Unix timestamp *)
  duration_ms: int option; (** Duration in milliseconds (if known) *)
}

(** Conference configuration *)
type conference_config = {
  name: string;                       (** Conference name *)
  agent_ids: string list;             (** Participating agents *)
  auto_persist_transcript: bool;      (** Auto-save transcript *)
  max_turn_duration_ms: int option;   (** Max turn duration limit *)
}

(** Conference state (opaque) *)
type t

(** {1 Creation} *)

(** Create a new conference.

    @param config Conference configuration
    @param config_path Path to .masc directory
    @return A new conference *)
val create : config:conference_config -> config_path:string -> t

(** {1 Conference Lifecycle} *)

(** Start the conference.
    Initializes sessions for all agents and begins processing turns.

    @param t The conference
    @return [Ok ()] on success, [Error msg] on failure *)
val start : t -> (unit, string) result Lwt.t

(** End the conference.
    Ends all sessions and stops turn processing.

    @param t The conference *)
val end_conference : t -> unit Lwt.t

(** Pause the conference.
    Stops processing new turns but keeps sessions alive.

    @param t The conference
    @param reason Reason for pause *)
val pause : t -> reason:string -> unit

(** Resume a paused conference.

    @param t The conference *)
val resume : t -> unit

(** {1 Turn Management} *)

(** Request a speaking turn.
    Adds to turn queue with optional priority.

    @param t The conference
    @param agent_id Agent requesting turn
    @param message Message to speak
    @param priority Priority (1=highest, default: 1)
    @return The created turn request *)
val speak : t -> agent_id:string -> message:string -> ?priority:int -> unit ->
  Turn_queue.turn_request

(** Get current speaker (if any).

    @param t The conference
    @return [Some agent_id] if someone is speaking *)
val current_speaker : t -> string option

(** Get pending turn count.

    @param t The conference
    @return Number of pending turns *)
val pending_turns : t -> int

(** {1 Transcript} *)

(** Get the transcript.

    @param t The conference
    @return List of transcript entries in order *)
val get_transcript : t -> transcript_entry list

(** Export transcript to markdown format.

    @param t The conference
    @return Markdown string *)
val export_transcript_md : t -> string

(** Export transcript to JSON.

    @param t The conference
    @return JSON representation *)
val export_transcript_json : t -> Yojson.Safe.t

(** Clear the transcript.

    @param t The conference *)
val clear_transcript : t -> unit

(** {1 Participants} *)

(** List participant agents.

    @param t The conference
    @return List of agent IDs *)
val participants : t -> string list

(** Add a participant to the conference.

    @param t The conference
    @param agent_id Agent to add
    @return [true] if added, [false] if already present *)
val add_participant : t -> agent_id:string -> bool

(** Remove a participant from the conference.

    @param t The conference
    @param agent_id Agent to remove
    @return [true] if removed, [false] if not found *)
val remove_participant : t -> agent_id:string -> bool

(** {1 Status} *)

(** Get current conference state.

    @param t The conference
    @return Current state *)
val state : t -> conference_state

(** Get conference status as JSON.

    @param t The conference
    @return JSON with full status *)
val status_json : t -> Yojson.Safe.t

(** {1 Utilities} *)

(** Convert state to string.
    @param state The state
    @return String representation *)
val string_of_state : conference_state -> string

(** Convert transcript entry to JSON.
    @param entry The entry
    @return JSON representation *)
val transcript_entry_to_json : transcript_entry -> Yojson.Safe.t
