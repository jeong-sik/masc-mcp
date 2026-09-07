type request_header = {
  id : string;
  task_id : string;
  worker : string;
  created_at : float;
}

type evidence_read_failure =
  | Evidence_missing
  | Evidence_not_regular_file
  | Evidence_outside_worker_playground
  | Evidence_invalid_utf8
  | Evidence_symbolic_link
  | Evidence_changed_during_read
  | Evidence_read_error of string

type submitted_evidence_item =
  | Evidence_note of string
  | Evidence_artifact of
      { reference : string
      ; content : string
      ; bytes : int
      ; truncated : bool
      }
  | Evidence_invalid_reference
  | Evidence_artifact_unreadable of
      { reference : string
      ; reason : evidence_read_failure
      }
  | Evidence_artifact_binary of
      { reference : string
      ; bytes : int
      ; sha256 : string
      ; format : string
      ; body : string option
      }

type evidence_access_failure =
  | Completion_authority_identity_missing
  | Request_not_found
  | Request_header_invalid of string
  | Evidence_snapshot_invalid of string
  | Request_load_error of string
  | Request_scope_mismatch

type submitted_evidence_access =
  | Evidence_available of
      { request : request_header
      ; items : submitted_evidence_item list
      }
  | Evidence_unavailable of
      { request_id : string
      ; reason : evidence_access_failure
      }

val request_header_of_yojson :
  Yojson.Safe.t -> (request_header, string) result

val submitted_evidence_access_to_yojson :
  submitted_evidence_access -> Yojson.Safe.t

val submitted_evidence_access_transport_to_yojson :
  submitted_evidence_access -> Yojson.Safe.t
(** Judge-transport projection of the snapshot: a [truncated=true] artifact
    carries [content_omitted] and an explanatory [content_note] (file size,
    snapshot cap, how to read ranges of the real file) instead of the prefix
    content. {!submitted_evidence_access_to_yojson} remains the persistence
    serializer and keeps the prefix for the audit record (#29615). *)

val submitted_evidence_access_metadata_to_yojson :
  submitted_evidence_access -> Yojson.Safe.t

val truncated_snapshot_items : Yojson.Safe.t -> (string * int) list
(** [(reference, bytes)] for every truncated artifact in a persisted evidence
    snapshot. Lets the submit path warn the producer at submission time
    instead of leaving the "too large to read at once" fact discoverable only
    after a review stalls. *)

val evidence_read_failure_code : evidence_read_failure -> string
(** Stable bounded code for diagnostics and metadata. Error detail is carried
    only by the structured durable reason object. *)
val evidence_access_failure_to_string :
  request_id:string -> evidence_access_failure -> string
val evidence_read_failure_of_owned_read_failure :
  Fs_compat.owned_regular_file_read_failure -> evidence_read_failure

val verification_evidence_max_bytes : int
(** Byte cap on the snapshot taken of a producer-owned evidence artifact. The
    artifact itself is not truncated; only the copy persisted in the
    verification request is. The judge-transport projection omits that prefix
    entirely for a truncated artifact. Exposed so a test states the truncation
    property against this value instead of copying the number, which made the
    constant unchangeable without editing an assertion that never described
    the constant. *)

val project_root_of_base_path : string -> string
(** The project root a BasePath names, whether the caller passed the project
    root itself or its [.masc] directory. Exposed so a producer's ownership
    root is derived by this function everywhere rather than re-derived beside
    it. *)

(** The reference shapes this store can read, decided without touching the
    filesystem. *)
type reference_form =
  | Artifact_reference of string
  | Note_reference of string
  | Unresolvable_reference

val classify_evidence_reference : string -> reference_form
(** Shape of an evidence reference as {!snapshot_submitted_evidence_json} will
    read it. This module is the only producer of evidence snapshots, so the
    submit boundaries call this rather than restating the accepted prefixes:
    a reference cannot be admitted at submit and then be unreadable at review,
    and a new form added here reaches every caller. *)

val note_reference_form : string
(** The accepted form for narrative evidence, spelled from the prefix this
    module matches on, so an error message naming it cannot drift from the
    classifier. The artifact form has no such caller: a message that has to
    name both uses {!resolvable_reference_forms}. *)

val resolvable_reference_forms : string list
(** The accepted forms, spelled for an error message that has to tell a caller
    what to write instead. *)

type artifact_payload =
  | Text_payload of string * int * bool
  | Binary_payload of
      { data : string; bytes : int; sha256 : string; format : string }

type artifact_read_result = (artifact_payload, evidence_read_failure) result
(** One artifact's read: text as [(content, bytes, truncated)] — [bytes] may
    exceed the content length when the read was capped — or binary as raw
    bytes with their hash and format (RFC-0436 §4.1). A reader that cannot
    answer returns the typed failure instead. *)

type utf8_scan =
  | Utf8_valid
  | Utf8_incomplete_at of int
  | Utf8_invalid

val scan_utf8 : string -> utf8_scan
(** The text line every artifact read answers under. Exposed so an injected
    reader classifies its bytes with the same scan the store applies, rather
    than a second opinion ({#33816}, RFC-0436 §4.1). *)

val snapshot_submitted_evidence_json :
  ?artifact_read:(worker:string -> relative:string -> artifact_read_result) ->
  ?request_id:string ->
  base_path:string ->
  worker:string ->
  string list ->
  Yojson.Safe.t
(** Materialize submitted evidence once at the producer's submit boundary.
    ["artifact:<relative-path>"] is rooted at the producer's declared sandbox;
    ["note:<text>"] preserves non-file evidence explicitly. [bytes] reports the
    source size, which exceeds the persisted [content] length when [truncated]
    is set by the projection cap. Bare and absolute references are persisted as
    a payload-free typed invalid-reference item. *)

val artifact_reference_size :
  ?artifact_read:(worker:string -> relative:string -> artifact_read_result) ->
  base_path:string ->
  worker:string ->
  string ->
  int option
(** Real byte size of an artifact reference, measured on the same validated
    descriptor the snapshot reads, without materializing content. [None] for
    non-artifact references, invalid paths, and files that cannot be read —
    the snapshot layer owns those failure shapes. Used by the
    keeper_task_done boundary to refuse oversized evidence before it stalls
    a completion authority (task-540). *)

val submitted_evidence_identity_lines :
  Yojson.Safe.t -> (string list, string) result
(** Project a persisted [submitted_evidence] snapshot into one identity line
    per item, for surfaces that name evidence without reading its payload —
    artifacts project to their reference, notes to their prefixed text, and
    unreadable items carry the typed reason code so an operator sees the
    failure rather than a silent gap. Payloads are served separately by the
    authority-scoped evidence route.

    This module writes the snapshot, so it owns the shape: a caller must not
    re-derive it. Returns [Error] on any item this module did not write and
    never partially projects a malformed array. *)

val inspect_submitted_evidence_for_authority :
  base_path:string ->
  request_id:string ->
  task_id:string ->
  task_worker:string ->
  authority:Masc_domain.completion_authority ->
  submitted_evidence_access
(** Read submitted evidence after a trusted boundary has produced a completion
    authority. The task id and producer must still match the durable request. *)
val verifications_dir : string -> string
val request_path : string -> string -> string