(** MASC Input Validation — security module.

    Prevents path injection and invalid-input attacks for MCP tool
    arguments. Each validator returns a [Result] type and logs
    rejections at WARN level for security monitoring.

    Internal helpers and the rejection counters are hidden — callers interact with the
    typed sub-modules and the two read-only stat accessors. *)

(** {1 Rejection statistics (observability)} *)

val get_rejection_stats : unit -> int * float
(** [(count, last_rejection_unix_ts)]. *)

val reset_rejection_stats : unit -> unit

(** {1 Agent ID} *)

module Id_shape : sig
  type t

  val parse : string -> (t, string) result
  (** Pure shape parsing with the same acceptance rules as [validate].
      Does not log or change rejection statistics. Use for candidates read
      from prose, where a non-identifier is expected input. *)

  val validate : string -> (t, string) result
  (** Validates [a-zA-Z0-9._-]+ with optional single colon namespace
      ([keeper:keeper-test-X]), length 1–64. Rejects path separators
      and traversal segments. Quoted or otherwise malformed values are
      rejected unchanged. Logs and records each rejection at an external
      input boundary. *)

  val to_string : t -> string

  val of_string_unsafe : string -> t
  (** Bypasses validation. Internal use only — never call with
      untrusted input. *)
end

(** {1 Task ID} *)

module Task_id : sig
  type t

  val validate : string -> (t, string) result
  (** Validates [a-zA-Z0-9_:-]+, length 1–128. Quoted or otherwise
      malformed values are rejected unchanged. *)

  val to_string : t -> string

  val of_string_unsafe : string -> t
  (** Internal use only. *)
end

(** {1 Filesystem paths} *)

module Safe_path : sig
  val validate_relative : string -> (string, string) result
  (** Reject empty paths, absolute paths, and any segment containing
      [..]. Returns the unchanged input on success. *)

  val sanitize_filename : string -> string
  (** Replace path separators, [..] sequences, and characters outside
      [a-zA-Z0-9_.\-] with [_]. *)
end

(** {1 Numeric validation} *)

module Safe_float : sig
  val validate : float -> name:string -> float
  (** Map [NaN] / [Inf] to [0.0] with a WARN log; pass through
      otherwise. *)

  val clamp : float -> min:float -> max:float -> float
end
