(** MASC Input Validation - Security Module

    Prevents path injection and invalid input attacks:
    - Validates agent_id format (alphanumeric, dash, underscore only)
    - Validates task_id format (safe characters only)
    - Returns Result types for safe error handling

    Security basis:
    - OWASP Path Traversal Prevention
    - Input validation best practices

    MAGI Recommendation: All validation rejections are logged at WARN level
    for security monitoring and debugging.
*)

(** MAGI: Validation rejection counters for observability.

    Keep the related values in one immutable snapshot.  A count Atomic beside a
    plain timestamp ref allowed cross-domain readers to observe a count from one
    rejection and a timestamp from another (and the ref itself was a data race). *)
type rejection_stats =
  { count : int
  ; last_rejection_time : float
  }

let rejection_stats =
  Atomic.make { count = 0; last_rejection_time = 0.0 }

(** Get validation statistics *)
let get_rejection_stats () =
  let snapshot = Atomic.get rejection_stats in
  snapshot.count, snapshot.last_rejection_time

(** Reset validation statistics *)
let reset_rejection_stats () =
  Atomic.set rejection_stats { count = 0; last_rejection_time = 0.0 }

(** Internal: Log validation rejection at WARN level *)
let log_rejection ~validator ~input ~reason =
  let now = Time_compat.now () in
  Atomic_util.update rejection_stats (fun snapshot ->
    { count = snapshot.count + 1; last_rejection_time = now });
  let safe_input =
    String_util.utf8_safe ~max_bytes:35 ~suffix:"..." input
    |> String_util.to_string
    |> String.map (fun char ->
      if Char.code char < 0x20 || Char.code char = 0x7f then '?' else char)
  in
  Log.Misc.warn "%s rejected input '%s': %s"
    validator safe_input reason

(** Whether a string is usable as an identifier at all — length, path
    separators, "..", character set. A gate: it refuses input, and says so in
    the log when it does.

    Not "the id of an agent". Three modules were called [Agent_id] and only
    one of them meant that: [Ids.Agent_id] mints a UUID, [Board_types.Agent_id]
    is a board author, and this one asks about the shape of any string. The
    name sent a reader to the wrong one twice in a single session (2026-08-28,
    chasing why edgar.a.poe could not claim its own task) and later put a gate
    where a parser belonged, which cost 208 WARN lines in a day (#31815). *)
module Id_shape : sig
  type t
  val parse : string -> (t, string) result
  val validate : string -> (t, string) result
  val to_string : t -> string
  val of_string_unsafe : string -> t  (* For internal use only *)
end = struct
  type t = string

  (* This is a shape gate, not an identity. It answers "can this string be used
     as an id" -- it does not answer "who is this" and does not know whether the
     name belongs to a keeper, a human, or an external bot. That question is a
     registry lookup at the caller (RFC-0393).

     Its counterpart on the other axis is [Keeper_identity.Keeper_id], which
     folds two names for comparison and validates nothing. The two names read
     like a keeper/agent pair and are not; see the comment on [Keeper_id]. *)

  (* Allow alphanumeric, dot, dash, underscore, with optional single colon for
     namespacing e.g. keeper:keeper-test-98295-0. Bare colons, multiple colons,
     or leading colons are rejected.

     Dots: keeper names carry them (edgar.a.poe), and RFC-0393 made a keeper name
     its own id rather than an encoded string, so the dot reaches this validator
     directly. Board already widened to the same set in #8633 and called its
     pattern "a strict superset of both"; this closes the half that never followed.
     Path traversal stays blocked by the ".." check above, which this pattern
     previously made unreachable. *)
  let valid_pattern = Re.Pcre.re {|^[a-zA-Z0-9._-]+(:[a-zA-Z0-9._-]+)?$|} |> Re.compile

  let parse s =
    if String.length s = 0 then
      Error "identifier cannot be empty"
    else if String.length s > 64 then
      Error (Printf.sprintf "identifier too long: %d chars (max 64)" (String.length s))
    else if String.contains s '/' || String.contains s '\\' then
      Error "identifier cannot contain path separators"
    else if String.contains s '.' && String.starts_with s ~prefix:".." then
      Error "identifier cannot contain path traversal"
    else if not (Re.execp valid_pattern s) then
      Error "identifier contains characters outside [A-Za-z0-9_:-]"
    else
      Ok s

  let validate s =
    match parse s with
    | Ok t -> Ok t
    | Error reason ->
      log_rejection ~validator:"Id_shape" ~input:s ~reason;
      Error reason

  let to_string t = t
  let of_string_unsafe s = s
end

(** Task ID validation *)
module Task_id : sig
  type t
  val validate : string -> (t, string) result
  val to_string : t -> string
  val of_string_unsafe : string -> t  (* For internal use only *)
end = struct
  type t = string

  (* Allow alphanumeric, dash, underscore, colon (for namespacing) *)
  let valid_pattern = Re.Pcre.re {|^[a-zA-Z0-9_:-]+$|} |> Re.compile

  let strict s =
    if String.length s = 0 then
      Error "task_id cannot be empty"
    else if String.length s > 128 then
      Error (Printf.sprintf "task_id too long: %d chars (max 128)" (String.length s))
    else if String.contains s '/' || String.contains s '\\' then
      Error "task_id cannot contain path separators"
    else if String.contains s '.' && String.starts_with s ~prefix:".." then
      Error "task_id cannot contain path traversal"
    else if not (Re.execp valid_pattern s) then
      Error "task_id contains characters outside [A-Za-z0-9_:-]"
    else
      Ok s

  let validate s =
    match strict s with
    | Ok t -> Ok t
    | Error reason ->
      log_rejection ~validator:"Task_id" ~input:s ~reason;
      Error reason

  let to_string t = t
  let of_string_unsafe s = s
end

(** File path validation (for user-provided paths) *)
module Safe_path : sig
  val validate_relative : string -> (string, string) result
  val sanitize_filename : string -> string
end = struct
  (* Static patterns hoisted to module load.  [Safe_path] is on every
     MCP tool argument-validation path, so the prior per-call form
     paid 4 [Re.compile] for every parameter check. *)
  let traversal_re = Re.Pcre.re {|\.\./|} |> Re.compile

  let path_separator_re = Re.Pcre.re {|[/\\]|} |> Re.compile

  let dotdot_re = Re.Pcre.re {|\.\.|} |> Re.compile

  let unsafe_char_re = Re.Pcre.re {|[^a-zA-Z0-9_.\-]|} |> Re.compile

  let validate_relative path =
    let reject reason =
      log_rejection ~validator:"Safe_path" ~input:path ~reason;
      Error reason
    in
    if String.length path = 0 then
      reject "path cannot be empty"
    else if path.[0] = '/' then
      reject "absolute paths not allowed"
    else if String.starts_with path ~prefix:".." then
      reject "path traversal not allowed"
    else if Re.execp traversal_re path then
      reject "path traversal not allowed"
    else
      Ok path

  let sanitize_filename name =
    name
    |> Re.replace_string path_separator_re ~by:"_"
    |> Re.replace_string dotdot_re ~by:"_"
    |> Re.replace_string unsafe_char_re ~by:"_"
end

(** Numeric validation *)
module Safe_float : sig
  val validate : float -> name:string -> float
  val clamp : float -> min:float -> max:float -> float
end = struct
  let validate f ~name =
    if Float.is_nan f then begin
      Log.Misc.warn "NaN detected for %s, using 0.0" name;
      0.0
    end else if Float.is_infinite f then begin
      Log.Misc.warn "Inf detected for %s, using 0.0" name;
      0.0
    end else
      f

  let clamp f ~min ~max =
    if f < min then min
    else if f > max then max
    else f
end
