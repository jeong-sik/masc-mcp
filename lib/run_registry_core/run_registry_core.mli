(** Shared current-state engine for bounded, append-only observation registries. *)

module Json : sig
  val object_fields
    :  Yojson.Safe.t
    -> ((string * Yojson.Safe.t) list, string) result

  val exact_fields
    :  required:string list
    -> ?optional:string list
    -> (string * Yojson.Safe.t) list
    -> (unit, string) result

  val string_field
    :  string
    -> (string * Yojson.Safe.t) list
    -> (string, string) result

  val float_field
    :  string
    -> (string * Yojson.Safe.t) list
    -> (float, string) result

  val optional_string_field
    :  string
    -> (string * Yojson.Safe.t) list
    -> (string option, string) result
end

module type Payload = sig
  type registration
  type completion

  val name : string
  val registration_to_yojson : registration -> Yojson.Safe.t
  val registration_of_yojson : Yojson.Safe.t -> (registration, string) result
  val completion_to_yojson : completion -> Yojson.Safe.t
  val completion_of_yojson : Yojson.Safe.t -> (completion, string) result

  val shed_registration : registration -> registration
  val shed_completion : completion -> completion
  (** The value with any heavy payload dropped, for the copy the store keeps
      in memory.

      A registry whose rows are small returns the value unchanged and nothing
      about it changes. One whose rows carry model input or output drops those
      fields: the store retains thousands of rows to serve a list that does
      not read them, and a detail view that reads one at a time. Measured
      2026-09-05, the exact-lane registry held 498 MB of live heap and 98% of
      the file behind it was the prompt text.

      What was shed is not lost -- it is on the row this entry was replayed
      from, and {!Make.get} reads it back. So a shed value must still carry
      everything a list projection reads, and a payload the store cannot
      re-read from a row must not be shed. *)
  val running_noun : string
  val restart_reason : string
  val replayed_running_completion
    : (started_at:float -> registration -> completion) option
  (** [Some] converts a replayed [Running] entry into explicit terminal
      evidence. [None] drops it when the subsystem's authoritative state lives
      elsewhere. *)
  val completed_retention : [ `All | `Latest of int ]

  val retention_group : (registration -> string) option
  (** [Some group_of] makes [`Latest n] a PER-GROUP bound: the newest [n]
      completed entries survive within each group instead of globally, so a
      busy group cannot evict a quiet one's entire history (the exact-lane
      registry's librarian runs every few turns per keeper — under a global
      bound the quiet
      lane's zero retained runs was indistinguishable from "never ran").
      [None] keeps the single global bound. *)
end

type persistence_state =
  | Not_persisted
  | Durability_unknown

type persistence_failure =
  { detail : string
  ; state : persistence_state
  }

type cut_report =
  { lines_read : int
  ; malformed_lines : int
  ; retained_entries : int
  ; reached_end : bool
  ; rewritten : bool
  }
(** What a deployment-time store cut read and what it kept. *)

module Make (Payload : Payload) : sig
  type status =
    | Running
    | Completed of Payload.completion

  type entry =
    { id : string
    ; started_at : float
    ; registration : Payload.registration
    ; status : status
    }

  type t

  val max_completed_retained : int
  val create : ?path:string -> unit -> t
  val replay : string -> t
  (** Retains a lightweight in-memory projection while compaction streams the
      selected original register/complete rows. Dropped payload fields are
      never serialized from the projection over their durable source.
      A replayed running entry gets the subsystem's explicit restart verdict.
      As with {!cut_replay_log}, replay requires exclusive ownership of the log. *)

  val register
    :  t
    -> id:string
    -> started_at:float
    -> registration:Payload.registration
    -> unit
(** Registry mutations and their JSONL events are serialized per [t], so a
    concurrent completion cannot overtake its registration on disk. *)

  val complete
    :  t
    -> id:string
    -> completion:Payload.completion
    -> [ `Completed | `Persistence_failed of persistence_failure | `Unknown ]
  (** Completion persistence is an observation-plane mutation. A durable
      append failure is returned explicitly with whether rollback established
      that it was not persisted or durability remains unknown. The in-memory
      entry remains [Running], so the caller chooses how to expose that failed
      observation without claiming a replayable completion. *)

  val list_entries : t -> entry list
  val get : t -> id:string -> entry option
  val cut_replay_log : execute:bool -> string -> cut_report
  (** Rewrites [path] from the state a replay of it produces. A hard-cut field
      leaves rows that can never decode again; [replay] declines to compact
      while any of them is on disk, so without this the store keeps them and
      its retention bound stops applying.

      The rewrite is [replay]'s own compaction, so it keeps exactly what
      [replay] keeps and nothing else. Beyond the rows no decoder reads, that
      settles every entry still [Running] according to
      [Payload.replayed_running_completion] (a fiber does not survive a
      restart), drops every completed entry past [completed_retention], and
      collapses the append history to one register — and one complete — per
      surviving entry. On a store that has been compacting normally this is
      what the next boot would write anyway; on a poisoned store it is not,
      because that store has not compacted since the field was cut.

      [execute:false] measures and reports without writing. A file whose last
      line is unterminated is never rewritten, because a partial read must not
      become a truncating rewrite — [reached_end] is [false], [rewritten] is
      [false], and the counts still stand.

      Run this only while no server holds the store. The rewrite replaces the
      inode, and a running server keeps appending through the writer it opened
      before — those appends land in the unlinked file. *)
end

(** Single-owner lifecycle for a process-wide registry. The first installation
    replaces the inert pre-boot registry; every later installation is rejected
    without changing the active registry. *)
module Global (Registry : sig
    type t

    val initial : t
  end) : sig
  type t = Registry.t
  type install_error = Already_installed

  val current : unit -> t
  val install : t -> (unit, install_error) result
end
