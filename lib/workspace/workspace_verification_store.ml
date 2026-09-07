open Result.Syntax

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

type artifact_payload =
  | Text_payload of string * int * bool
      (** [(content, bytes, truncated)] — [bytes] may exceed the content
          length when the read was capped. *)
  | Binary_payload of
      { data : string  (** the raw bytes as read; the store persists them as the evidence body *)
      ; bytes : int  (** the byte count, measured on the original *)
      ; sha256 : string
      ; format : string  (** derived from the reference's extension; "unknown" when bare *)
      }

type artifact_read_result = (artifact_payload, evidence_read_failure) result

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
      (* base_path-relative path of the persisted bytes; [None] when the
         caller gave no request to file the body under (RFC-0436 §4.2). *)
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

(* Reason code for a reference this module refused to materialize. Unlike the
   [evidence_read_failure] codes it sits beside on the wire, it has no variant:
   the rejected reference is never persisted, so there is nothing to carry. *)
let invalid_reference_code = "invalid_reference"

let evidence_read_failure_code = function
  | Evidence_missing -> "missing"
  | Evidence_not_regular_file -> "not_regular_file"
  | Evidence_outside_worker_playground -> "outside_worker_playground"
  | Evidence_invalid_utf8 -> "invalid_utf8"
  | Evidence_symbolic_link -> "symbolic_link"
  | Evidence_changed_during_read -> "changed_during_read"
  | Evidence_read_error _ -> "read_error"

let evidence_read_failure_to_yojson reason =
  match reason with
  | Evidence_read_error detail ->
    `Assoc [ "code", `String "read_error"; "detail", `String detail ]
  | ( Evidence_missing
    | Evidence_not_regular_file
    | Evidence_outside_worker_playground
    | Evidence_invalid_utf8
    | Evidence_symbolic_link
    | Evidence_changed_during_read ) ->
    `Assoc [ "code", `String (evidence_read_failure_code reason) ]

let evidence_read_failure_of_yojson = function
  | `Assoc fields ->
    (match List.sort (fun (left, _) (right, _) -> String.compare left right) fields with
     | [ "code", `String "missing" ] -> Ok Evidence_missing
     | [ "code", `String "not_regular_file" ] -> Ok Evidence_not_regular_file
     | [ "code", `String "outside_worker_playground" ] ->
       Ok Evidence_outside_worker_playground
     | [ "code", `String "invalid_utf8" ] -> Ok Evidence_invalid_utf8
     | [ "code", `String "symbolic_link" ] -> Ok Evidence_symbolic_link
     | [ "code", `String "changed_during_read" ] -> Ok Evidence_changed_during_read
     | [ "code", `String "read_error"; "detail", `String detail ] ->
       Ok (Evidence_read_error detail)
     | _ -> Error "submitted evidence snapshot has an invalid unreadable reason")
  | _ -> Error "submitted evidence snapshot unreadable reason must be an object"

let submitted_evidence_item_to_yojson = function
  | Evidence_note note ->
    `Assoc [ "kind", `String "note"; "content", `String note ]
  | Evidence_artifact { reference; content; bytes; truncated } ->
    `Assoc
      [ "kind", `String "artifact"
      ; "reference", `String reference
      ; "content", `String content
      ; "bytes", `Int bytes
      ; "truncated", `Bool truncated
      ]
  | Evidence_invalid_reference ->
    `Assoc
      [ "kind", `String "artifact_unreadable"
      ; "reason", `Assoc [ "code", `String invalid_reference_code ]
      ]
  | Evidence_artifact_unreadable { reference; reason } ->
    `Assoc
      [ "kind", `String "artifact_unreadable"
      ; "reference", `String reference
      ; "reason", evidence_read_failure_to_yojson reason
      ]
  | Evidence_artifact_binary { reference; bytes; sha256; format; body } ->
    `Assoc
      ([ "kind", `String "artifact_binary"
       ; "reference", `String reference
       ; "bytes", `Int bytes
       ; "sha256", `String sha256
       ; "format", `String format
       ]
      @ (match body with
         | Some path -> [ ("body", `String path) ]
         | None -> []))

let request_header_to_yojson request =
  `Assoc
    [ "id", `String request.id
    ; "task_id", `String request.task_id
    ; "worker", `String request.worker
    ; "created_at", `Float request.created_at
    ]
;;

let evidence_access_failure_code = function
  | Completion_authority_identity_missing ->
    "completion_authority_identity_missing"
  | Request_not_found -> "request_not_found"
  | Request_header_invalid _ -> "request_header_invalid"
  | Evidence_snapshot_invalid _ -> "evidence_snapshot_invalid"
  | Request_load_error _ -> "request_load_error"
  | Request_scope_mismatch -> "request_scope_mismatch"

let evidence_access_failure_to_string ~request_id = function
  | Completion_authority_identity_missing -> "completion authority identity is empty"
  | Request_not_found -> Printf.sprintf "Verification %s not found" request_id
  | Request_header_invalid detail ->
    Printf.sprintf
      "Failed to decode verification %s request header: %s"
      request_id
      detail
  | Evidence_snapshot_invalid detail ->
    Printf.sprintf
      "Failed to decode verification %s evidence snapshot: %s"
      request_id
      detail
  | Request_load_error detail ->
    Printf.sprintf
      "Failed to load verification %s evidence: %s"
      request_id
      detail
  | Request_scope_mismatch ->
    "verification request does not match the awaiting task and producer"

(* Bounded snapshot cap for producer-owned evidence artifacts. Raised from
   20_000 to 200_000 so contract artifacts in the observed 40-128KB range are
   persisted in full (truncated=false) instead of being marked unusable by the
   completion authority. The cap stays bounded to bound verifier memory and
   backlog storage.

   It is the Read ceiling, taken from the same value rather than written out
   again: the authority reads live through Read and the operator reviews the
   snapshot, so a Read allowed past this cap would let a verdict rest on bytes
   the snapshot does not hold (#27397). *)
let verification_evidence_max_bytes = Tool_shard_limits.verification_evidence_max_bytes

let submitted_evidence_access_to_yojson = function
  | Evidence_available { request; items } ->
    `Assoc
      [ "access", `String "available"
      ; "request", request_header_to_yojson request
      ; "items", `List (List.map submitted_evidence_item_to_yojson items)
      ]
  | Evidence_unavailable { request_id; reason } ->
    `Assoc
      [ "access", `String "unavailable"
      ; "request_id", `String request_id
      ; "reason", `String (evidence_access_failure_to_string ~request_id reason)
      ]
;;

(* Judge-transport projection. The serializer above persists the truncated
   prefix so the audit record keeps what was readable at submission time. The
   review request must not carry it: the instructions already order the judge
   to treat [truncated=true] as unavailable evidence, so transmitting the
   prefix ships bytes the judgement cannot use — and one 22 MB artifact
   (200 KB prefix, listed under two references) inflated the review turn past
   every verifier_exact slot's input budget this way (#29615). The prefix
   stays in the store; the judge receives the size, the fact, and how to read
   the real file. *)
let submitted_evidence_item_transport_to_yojson = function
  | Evidence_note note ->
    `Assoc [ "kind", `String "note"; "content", `String note ]
  | Evidence_artifact { reference; content; bytes; truncated } ->
    if truncated
    then
      `Assoc
        [ "kind", `String "artifact"
        ; "reference", `String reference
        ; "bytes", `Int bytes
        ; "truncated", `Bool true
        ; "content_omitted", `Bool true
        ; ( "content_note"
          , `String
              (Printf.sprintf
                 "file is %d bytes; only a %d-byte prefix fits the evidence \
                  snapshot and that prefix is withheld from this request; \
                  treat the item as unavailable and read ranges of the actual \
                  file with the listed verification tools"
                 bytes
                 verification_evidence_max_bytes) )
        ]
    else
      `Assoc
        [ "kind", `String "artifact"
        ; "reference", `String reference
        ; "content", `String content
        ; "bytes", `Int bytes
        ; "truncated", `Bool false
        ]
  | Evidence_invalid_reference ->
    `Assoc
      [ "kind", `String "artifact_unreadable"
      ; "reason", `String invalid_reference_code
      ]
  | Evidence_artifact_unreadable { reference; reason } ->
    `Assoc
      [ "kind", `String "artifact_unreadable"
      ; "reference", `String reference
      ; "reason", `String (evidence_read_failure_code reason)
      ]
  | Evidence_artifact_binary { reference; bytes; sha256; format; body } ->
    `Assoc
      ([ "kind", `String "artifact_binary"
       ; "reference", `String reference
       ; "bytes", `Int bytes
       ; "sha256", `String sha256
       ; "format", `String format
       ]
      @ (match body with
         | Some path -> [ ("body", `String path) ]
         | None -> []))
;;

(* The per-item cap above bounds one artifact. Nothing bounded their sum, so a
   submission of many sub-cap artifacts still built a request no slot could
   carry: 12 artifacts, every one [truncated=false], 204,834 bytes of content
   in one measured bundle and 1,005,015 in the atom that stalled task-465
   (2026-08-25). #29615 closed the single-22MB-artifact shape; this closes the
   many-small-artifacts shape behind it.

   The ceiling is the per-item cap itself rather than a new number: the whole
   evidence block is now bounded by what one artifact was already allowed to
   be. That bounds the request; it does not promise a fit for every slot,
   because a slot's remaining budget also depends on the conversation in front
   of it — that is a routing concern and is not decided here. *)
let evidence_transport_max_bytes = verification_evidence_max_bytes

(* Withheld for the aggregate budget, not for its own size. The judge is given
   the same three things a per-item withholding gives it — the reference, the
   real size, and the instruction to read the file — so the shape it has to
   understand does not grow. *)
let submitted_evidence_item_withheld_to_yojson = function
  | Evidence_artifact { reference; content = _; bytes; truncated = _ } ->
    `Assoc
      [ "kind", `String "artifact"
      ; "reference", `String reference
      ; "bytes", `Int bytes
      ; "truncated", `Bool false
      ; "content_omitted", `Bool true
      ; ( "content_note"
        , `String
            (Printf.sprintf
               "file is %d bytes; the evidence block for this request is \
                capped at %d bytes and this item is past the cap, so its \
                content is withheld; read ranges of the actual file with the \
                listed verification tools"
               bytes
               evidence_transport_max_bytes) )
      ]
  | (Evidence_note _ | Evidence_invalid_reference | Evidence_artifact_unreadable _
    | Evidence_artifact_binary _) as item ->
    (* Only a full-content artifact can be withheld for the aggregate budget;
       the rest carry no content to withhold. A binary item is already
       payload-free on the wire -- its bytes live in the evidence body. *)
    submitted_evidence_item_transport_to_yojson item
;;

(* Which artifacts keep their content, by index.

   What this buys is one thing: the same evidence set makes the same choice
   every run. Filling in submission order let one large-but-under-cap artifact
   spend the whole budget, and which items the judge saw then depended on the
   order the producer happened to list them in. Nothing declares that order —
   the contract does not ask for most-important-first, and the item type
   carries no priority — so it was incidental rather than intent.

   Smallest-first is the tiebreak, not the goal, and it is not an optimum.
   The contract requires support for every item and judges each one
   independently, so a budget that forces any drop already fails that item;
   which one is dropped changes who fails, not whether. Size is what is
   available to sort on, ties break on index, and the emitted list stays in
   submission order.

   A dropped item is not silent: it carries [content_omitted] and a note
   naming the cap and the tools that read the file directly. *)
let carried_artifact_indices items =
  let weighed =
    List.mapi (fun index item -> index, item) items
    |> List.filter_map (function
         | index, Evidence_artifact { content; truncated = false; _ } ->
           Some (index, String.length content)
         | ( _
           , ( Evidence_note _
             | Evidence_artifact _
             | Evidence_invalid_reference
             | Evidence_artifact_unreadable _
             | Evidence_artifact_binary _ ) ) -> None)
  in
  let sorted =
    List.stable_sort
      (fun (index_a, weight_a) (index_b, weight_b) ->
        match Int.compare weight_a weight_b with
        | 0 -> Int.compare index_a index_b
        | order -> order)
      weighed
  in
  let carried, _ =
    List.fold_left
      (fun (carried, spent) (index, weight) ->
        if spent + weight > evidence_transport_max_bytes
        then carried, spent
        else index :: carried, spent + weight)
      ([], 0)
      sorted
  in
  carried
;;

let submitted_evidence_items_transport_to_yojson items =
  let carried = carried_artifact_indices items in
  List.mapi
    (fun index item ->
      match item with
      | Evidence_artifact { truncated = false; _ } ->
        if List.mem index carried
        then submitted_evidence_item_transport_to_yojson item
        else submitted_evidence_item_withheld_to_yojson item
      | Evidence_note _
      | Evidence_artifact _
      | Evidence_invalid_reference
      | Evidence_artifact_unreadable _
      | Evidence_artifact_binary _ -> submitted_evidence_item_transport_to_yojson item)
    items
;;

let submitted_evidence_access_transport_to_yojson = function
  | Evidence_available { request; items } ->
    `Assoc
      [ "access", `String "available"
      ; "request", request_header_to_yojson request
      ; "items", `List (submitted_evidence_items_transport_to_yojson items)
      ]
  | Evidence_unavailable { request_id; reason } ->
    `Assoc
      [ "access", `String "unavailable"
      ; "request_id", `String request_id
      ; "reason", `String (evidence_access_failure_to_string ~request_id reason)
      ]
;;

let submitted_evidence_item_metadata_to_yojson = function
  | Evidence_note note ->
    `Assoc [ "kind", `String "note"; "bytes", `Int (String.length note) ]
  | Evidence_artifact { reference; bytes; truncated; _ } ->
    `Assoc
      [ "kind", `String "artifact"
      ; "reference", `String reference
      ; "bytes", `Int bytes
      ; "truncated", `Bool truncated
      ]
  | Evidence_invalid_reference ->
    `Assoc
      [ "kind", `String "artifact_unreadable"
      ; "reason", `String invalid_reference_code
      ]
  | Evidence_artifact_unreadable { reference; reason } ->
    `Assoc
      [ "kind", `String "artifact_unreadable"
      ; "reference", `String reference
      ; "reason", `String (evidence_read_failure_code reason)
      ]
  | Evidence_artifact_binary { reference; bytes; sha256; format; body } ->
    `Assoc
      ([ "kind", `String "artifact_binary"
       ; "reference", `String reference
       ; "bytes", `Int bytes
       ; "sha256", `String sha256
       ; "format", `String format
       ]
      @ (match body with
         | Some path -> [ ("body", `String path) ]
         | None -> []))
;;

let submitted_evidence_access_metadata_to_yojson = function
  | Evidence_available { request; items } ->
    `Assoc
      [ "access", `String "available"
      ; "request_id", `String request.id
      ; "task_id", `String request.task_id
      ; "worker", `String request.worker
      ; "created_at", `Float request.created_at
      ; ( "items"
        , `List (List.map submitted_evidence_item_metadata_to_yojson items) )
      ]
  | Evidence_unavailable { request_id; reason } ->
    `Assoc
      [ "access", `String "unavailable"
      ; "request_id", `String request_id
      ; "reason_code", `String (evidence_access_failure_code reason)
      ]
;;

let submitted_evidence_item_of_yojson = function
  | `Assoc fields ->
    let string_field key =
      match List.assoc_opt key fields with
      | Some (`String value) -> Ok value
      | Some value ->
        Error
          (Printf.sprintf
             "submitted evidence snapshot field %s must be a string, got %s"
             key
             (Json_util.excerpt value))
      | None ->
        Error
          (Printf.sprintf
             "submitted evidence snapshot is missing string field %s"
             key)
    in
    (match List.assoc_opt "kind" fields with
     | Some (`String "note") ->
       let open Result.Syntax in
       let* () =
         Json_util.reject_unknown_fields
           ~surface:"submitted evidence note"
           ~allowed:[ "kind"; "content" ]
           fields
       in
       Result.map (fun note -> Evidence_note note) (string_field "content")
     | Some (`String "artifact") ->
       let open Result.Syntax in
       let* () =
         Json_util.reject_unknown_fields
           ~surface:"submitted evidence artifact"
           ~allowed:[ "kind"; "reference"; "content"; "bytes"; "truncated" ]
           fields
       in
       let* reference = string_field "reference" in
       let* content = string_field "content" in
       let* bytes =
         match List.assoc_opt "bytes" fields with
         | Some (`Int value) when value >= 0 -> Ok value
         | Some value ->
           Error
             (Printf.sprintf
                "submitted evidence snapshot bytes must be a non-negative integer, got %s"
                (Json_util.excerpt value))
         | None -> Error "submitted evidence snapshot is missing bytes"
       in
       let* truncated =
         match List.assoc_opt "truncated" fields with
         | Some (`Bool value) -> Ok value
         | Some value ->
           Error
             (Printf.sprintf
                "submitted evidence snapshot truncated must be a boolean, got %s"
                (Json_util.excerpt value))
         | None -> Error "submitted evidence snapshot is missing truncated"
       in
       Ok (Evidence_artifact { reference; content; bytes; truncated })
     | Some (`String "artifact_unreadable") ->
       let open Result.Syntax in
       let field_names =
         fields |> List.map fst |> List.sort String.compare
       in
       let* reason_json =
         match List.assoc_opt "reason" fields with
         | Some reason -> Ok reason
         | None -> Error "submitted evidence snapshot is missing unreadable reason"
       in
       (match reason_json, List.assoc_opt "reference" fields, field_names with
        | ( `Assoc [ "code", `String "invalid_reference" ]
          , None
          , [ "kind"; "reason" ] ) ->
          Ok Evidence_invalid_reference
        | `Assoc [ "code", `String "invalid_reference" ], Some _, _ ->
          Error "invalid submitted evidence references must not persist the rejected value"
        | `Assoc [ "code", `String "invalid_reference" ], None, _ ->
          Error
            "invalid submitted evidence references must be payload-free"
        | reason_json, _, [ "kind"; "reason"; "reference" ] ->
          let* reason = evidence_read_failure_of_yojson reason_json in
          let* reference = string_field "reference" in
          Ok (Evidence_artifact_unreadable { reference; reason })
        | _, _, _ ->
          Error
            "submitted evidence unreadable item has unexpected fields")
     | Some (`String "artifact_binary") ->
       let open Result.Syntax in
       let* () =
         Json_util.reject_unknown_fields
           ~surface:"submitted evidence binary artifact"
           ~allowed:[ "kind"; "reference"; "bytes"; "sha256"; "format"; "body" ]
           fields
       in
       let* reference = string_field "reference" in
       let* bytes =
         match List.assoc_opt "bytes" fields with
         | Some (`Int value) when value >= 0 -> Ok value
         | Some value ->
           Error
             (Printf.sprintf
                "submitted evidence binary artifact bytes must be a non-negative integer, got %s"
                (Json_util.excerpt value))
         | None -> Error "submitted evidence binary artifact is missing bytes"
       in
       let* sha256 = string_field "sha256" in
       let* format = string_field "format" in
       let* body =
         match List.assoc_opt "body" fields with
         | Some (`String path) -> Ok (Some path)
         | Some value ->
           Error
             (Printf.sprintf
                "submitted evidence binary artifact body must be a string, got %s"
                (Json_util.excerpt value))
         | None -> Ok None
       in
       Ok (Evidence_artifact_binary { reference; bytes; sha256; format; body })
     | Some (`String kind) ->
       Error (Printf.sprintf "unknown submitted evidence snapshot kind %S" kind)
     | Some value ->
       Error
         (Printf.sprintf
            "submitted evidence snapshot kind must be a string, got %s"
            (Json_util.excerpt value))
     | None -> Error "submitted evidence snapshot is missing kind")
  | value ->
    Error
      (Printf.sprintf
         "submitted evidence snapshot item must be an object, got %s"
         (Json_util.excerpt value))

let project_root_of_base_path base_path =
  if Filename.basename base_path = Common.masc_dirname then
    Filename.dirname base_path
  else
    base_path

let active_verifications_dir base_path =
  let base_path = project_root_of_base_path base_path in
  Filename.concat (Workspace_utils.masc_dir_from_base_path ~base_path) "verifications"

let verifications_dir base_path =
  active_verifications_dir base_path

let request_path base_path req_id =
  Filename.concat (verifications_dir base_path) (req_id ^ ".json")

let request_header_of_yojson = function
  | `Assoc fields ->
      let required_field key =
        match List.assoc_opt key fields with
        | Some value -> Ok value
        | None ->
          Error
            (Printf.sprintf
               "verification request missing required field %s (object had keys: [%s])"
               key
               (String.concat ", " (List.map fst fields)))
      in
      let required_string key =
        let* value = required_field key in
        match value with
        | `String value when not (String.equal (String.trim value) "") -> Ok value
        | `String _ -> Error (Printf.sprintf "verification request field %s is blank" key)
        | other ->
          Error
            (Printf.sprintf
               "verification request field %s must be a non-empty string, got %s"
               key
               (Json_util.excerpt other))
      in
      let* id = required_string "id" in
      let* task_id = required_string "task_id" in
      let* worker = required_string "worker" in
      let* created_at = required_field "created_at" in
      let* created_at =
        match created_at with
        | `Float value -> Ok value
        | `Int value -> Ok (Float.of_int value)
        | other ->
          Error
            (Printf.sprintf
               "verification request field created_at must be a number, got %s"
               (Json_util.excerpt other))
      in
      (match classify_float created_at with
       | FP_nan | FP_infinite ->
         Error "verification request field created_at must be finite"
       | FP_normal | FP_subnormal | FP_zero ->
         Ok { id; task_id; worker; created_at })
  | other ->
      Error
        (Printf.sprintf
           "verification request must be a JSON object, got %s: %s"
           (Json_util.kind_name other)
           (Json_util.excerpt other))

let submitted_evidence_snapshot_of_request_json = function
  | `Assoc fields ->
    (match List.assoc_opt "output" fields with
     | Some (`Assoc output_fields) ->
       (match List.assoc_opt "submitted_evidence" output_fields with
        | Some (`List values) ->
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | value :: rest ->
              (match submitted_evidence_item_of_yojson value with
               | Ok item -> collect (item :: acc) rest
               | Error _ as error -> error)
          in
          collect [] values
        | Some other ->
          Error
            (Printf.sprintf
               "submitted_evidence must be a typed snapshot list, got %s"
               (Json_util.excerpt other))
        | None ->
          Error "verification request output has no submitted_evidence")
     | Some other ->
       Error
         (Printf.sprintf
            "verification request output must be an object, got %s"
            (Json_util.excerpt other))
     | None -> Error "verification request has no output")
  | other ->
    Error
      (Printf.sprintf
         "verification request must be an object, got %s"
         (Json_util.excerpt other))

let load_request_for_evidence base_path req_id =
  let path = request_path base_path req_id in
  if not (Sys.file_exists path) then
    Error Request_not_found
  else
    try
      let json = Safe_ops.read_json_eio path in
      match request_header_of_yojson json with
      | Error detail -> Error (Request_header_invalid detail)
      | Ok request ->
        (match submitted_evidence_snapshot_of_request_json json with
         | Error detail -> Error (Evidence_snapshot_invalid detail)
         | Ok snapshot -> Ok (request, snapshot))
    with
    | Eio.Cancel.Cancelled _ as e -> raise e
    | exn ->
      Error (Request_load_error (Printexc.to_string exn))

type utf8_scan =
  | Utf8_valid
  | Utf8_incomplete_at of int
  | Utf8_invalid

let scan_utf8 bytes =
  let length = String.length bytes in
  let byte index = Char.code bytes.[index] in
  let continuation index =
    index < length && byte index land 0xC0 = 0x80
  in
  let rec loop index =
    if index = length then Utf8_valid
    else
      let first = byte index in
      if first <= 0x7F then loop (index + 1)
      else
        let required, second_min, second_max =
          if first >= 0xC2 && first <= 0xDF then 2, 0x80, 0xBF
          else if first = 0xE0 then 3, 0xA0, 0xBF
          else if (first >= 0xE1 && first <= 0xEC) || (first >= 0xEE && first <= 0xEF)
          then 3, 0x80, 0xBF
          else if first = 0xED then 3, 0x80, 0x9F
          else if first = 0xF0 then 4, 0x90, 0xBF
          else if first >= 0xF1 && first <= 0xF3 then 4, 0x80, 0xBF
          else if first = 0xF4 then 4, 0x80, 0x8F
          else 0, 0, 0
        in
        if required = 0 then Utf8_invalid
        else if index + required > length then Utf8_incomplete_at index
        else
          let second = byte (index + 1) in
          if
            second < second_min
            || second > second_max
            || (required >= 3 && not (continuation (index + 2)))
            || (required = 4 && not (continuation (index + 3)))
          then Utf8_invalid
          else loop (index + required)
  in
  loop 0

let evidence_read_failure_of_owned_read_failure = function
  | Fs_compat.Ownership_boundary_rejected _ ->
    Evidence_outside_worker_playground
  | Path_is_not_regular_file { kind; _ } ->
    if kind = Unix.S_LNK then Evidence_symbolic_link else Evidence_not_regular_file
  | Filesystem_identity_changed _ ->
    Evidence_changed_during_read
  | Owned_file_operation_failed { cause; _ } ->
    Evidence_read_error (Printexc.to_string cause)

let read_regular_file_prefix ~ownership_root path =
  match
    Fs_compat.load_owned_regular_file_prefix
      ~ownership_root
      ~max_bytes:verification_evidence_max_bytes
      path
  with
  | Error error ->
    Error (evidence_read_failure_of_owned_read_failure error.failure)
  | Ok None -> Error Evidence_missing
  | Ok (Some prefix) ->
    (match scan_utf8 prefix.content with
     | Utf8_valid ->
       Ok (prefix.content, prefix.file_size, prefix.truncated)
     | Utf8_incomplete_at index when prefix.truncated ->
       Ok
         ( String.sub prefix.content 0 index
         , prefix.file_size
         , true )
     | Utf8_incomplete_at _ | Utf8_invalid ->
       Error Evidence_invalid_utf8)

let artifact_reference_prefix = "artifact:"
let note_reference_prefix = "note:"

let strip_prefix ~prefix value =
  if String.starts_with ~prefix value
  then
    Some
      (String.sub
         value
         (String.length prefix)
         (String.length value - String.length prefix))
  else None

(* The shape this store can read, decided without touching the filesystem.
   [snapshot_submitted_evidence_item] below is the only producer of evidence
   snapshots and answers [Evidence_invalid_reference] for anything else, so the
   submit boundaries ask this instead of restating the prefixes: a reference
   form added here reaches every caller, and one cannot be accepted at submit
   and then be unreadable at review. *)
type reference_form =
  | Artifact_reference of string
  | Note_reference of string
  | Unresolvable_reference

let classify_evidence_reference reference =
  match strip_prefix ~prefix:artifact_reference_prefix reference with
  | Some relative_path -> Artifact_reference relative_path
  | None ->
    (match strip_prefix ~prefix:note_reference_prefix reference with
     | Some note when not (String.equal (String.trim note) "") ->
       Note_reference note
     | Some _ | None -> Unresolvable_reference)
;;

let artifact_reference_form =
  artifact_reference_prefix ^ "<producer-root-relative-path>"
;;

let note_reference_form = note_reference_prefix ^ "<text>"
let resolvable_reference_forms = [ artifact_reference_form; note_reference_form ]

let valid_producer_relative_path path =
  Filename.is_relative path
  && not (String.equal path "")
  && (String.split_on_char '/' path
      |> List.for_all (fun segment ->
        not
          (String.equal segment ""
           || String.equal segment "."
           || String.equal segment "..")))

(* A producer-relative artifact path resolves against the producer's sandbox
   root and nowhere else.

   A submitter often writes the path relative to the checkout it worked in
   ([lib/foo.ml] rather than [masc/lib/foo.ml]), and the direct concat then
   misses. This store used to guess the rest: it enumerated [repos/*] and
   accepted the read when exactly one entry held the file. That guess was
   wrong twice over. It hardcoded a [repos/] segment, which
   {!Keeper_playground_checkouts} exists to remove — a keeper puts its
   checkouts where it likes, and the ones at the top level were invisible to
   it. And where a keeper holds the same repository several times, the file
   sits in several checkouts with different content, so the arm that reads
   "exactly one match" is the arm that never fires for the layout that most
   needs it.

   Reading a file the submitter did not name is worse than not reading one:
   the judge receives content without knowing which checkout produced it.
   So the miss stays a typed [Evidence_artifact_unreadable] and travels to
   the judge, which holds Read/Grep on this same root and a [root_layout]
   naming every checkout under it. Resolving a checkout-relative path is the
   judge's call, and [verification.lookup.producer_tree] already tells it to
   make that call. *)

(* The reader is injected because where an artifact lives depends on the
   producer's sandbox profile, and that policy sits above this library: a
   Docker keeper's files sit on the host playground this code can reach, while
   a microvm/remote-ssh keeper's files sit inside the guest's work volume,
   which only the sandbox backend can read. Without a reader the direct host
   read stands (tests, host-profile keepers); with one, every artifact read --
   snapshot and size pre-check alike -- goes through it, so the two can never
   disagree about where a file is. *)
(* Where a binary artifact's bytes are filed (RFC-0436 §4.2): the
   verification-owned evidence directory, one file per item index. The path
   recorded on the item is spelled relative to the workspace's masc
   directory so the judge's side resolves it under any base_path. Returns
   [None] when no request id was given -- the hash still stands, the body
   is simply not filed. *)
let persist_binary_body ~base_path ?request_id ?index data =
  match (request_id, index) with
  | Some request_id, Some index ->
    let masc_dir = Workspace_utils.masc_dir_from_base_path ~base_path in
    let dir = Filename.concat (Filename.concat masc_dir "evidence") request_id in
    Fs_compat.mkdir_p dir;
    let file = Printf.sprintf "%d.bin" index in
    Fs_compat.save_file (Filename.concat dir file) data;
    Some (Filename.concat (Filename.concat "evidence" request_id) file)
  | _ -> None

let inspect_producer_relative_artifact ?artifact_read ?request_id ?index ~base_path
    ~worker ~reference relative_path =
  if not (valid_producer_relative_path relative_path)
  then Evidence_invalid_reference
  else
    match artifact_read with
    | Some read -> (
        match read ~worker ~relative:relative_path with
        | Error reason -> Evidence_artifact_unreadable { reference; reason }
        | Ok (Binary_payload { data; bytes; sha256; format }) ->
            (* Bytes that are not text are evidence too: the snapshot keeps
               the hash and the size, and the bytes are filed as the body
               (RFC-0436 §4.1). *)
            let body =
              persist_binary_body ~base_path ?request_id ?index data
            in
            Evidence_artifact_binary { reference; bytes; sha256; format; body }
        | Ok (Text_payload (content, bytes, truncated)) -> (
            (* An artifact is text evidence, and the direct host read holds
               that line with [scan_utf8]; an injected reader hands back bytes
               from another lane (a guest [cat]), so the same scan holds the
               line here too: every reader answers under one boundary, and a
               truncated read may stop mid-character the way the direct
               prefix can. *)
            match scan_utf8 content with
            | Utf8_valid ->
              Evidence_artifact { reference; content; bytes; truncated }
            | Utf8_incomplete_at at when truncated ->
              Evidence_artifact
                { reference
                ; content = String.sub content 0 at
                ; bytes
                ; truncated = true
                }
            | Utf8_incomplete_at _ | Utf8_invalid ->
              Evidence_artifact_unreadable
                { reference; reason = Evidence_invalid_utf8 }))
    | None -> (
    let project_root = project_root_of_base_path base_path in
    let ownership_root =
      Keeper_sandbox_config.host_root_abs_of_agent
        ~base_path:project_root
        ~agent_name:worker
      |> Env_config_core.strip_trailing_slashes
    in
    let target = Filename.concat ownership_root relative_path in
    match read_regular_file_prefix ~ownership_root target with
    | Ok (content, bytes, truncated) ->
      Evidence_artifact { reference; content; bytes; truncated }
    | Error (Evidence_missing as reason)
    | Error (Evidence_not_regular_file as reason)
    | Error (Evidence_outside_worker_playground as reason)
    | Error (Evidence_invalid_utf8 as reason)
    | Error (Evidence_symbolic_link as reason)
    | Error (Evidence_changed_during_read as reason)
    | Error (Evidence_read_error _ as reason) ->
      Evidence_artifact_unreadable { reference; reason } )

let snapshot_submitted_evidence_item ?artifact_read ?request_id ?index ~base_path
    ~worker reference =
  match classify_evidence_reference reference with
  | Artifact_reference relative_path ->
    inspect_producer_relative_artifact
      ?artifact_read
      ?request_id
      ?index
      ~base_path
      ~worker
      ~reference
      relative_path
  | Note_reference note -> Evidence_note note
  | Unresolvable_reference -> Evidence_invalid_reference

(* Size-only pre-check for the submit boundary. Reads the same
   descriptor-validated file the snapshot would read — with a 1-byte prefix so
   no content is materialized — and reports the descriptor's real [file_size],
   so an oversized artifact is refused at keeper_task_done time with the byte
   count the operator needs, instead of stalling the completion authority on a
   truncated prefix it cannot use (task-540: four tasks sat 8-16h in
   evaluator_unavailable). A file that cannot be read is not this check's
   business: the snapshot layer reports those as typed unreadable reasons, and
   duplicating that taxonomy here would drift. [None] means "no artifact
   reference / not measurable here", which callers treat as pass-through. *)
let artifact_reference_size ?artifact_read ~base_path ~worker reference =
  match classify_evidence_reference reference with
  | Artifact_reference relative_path ->
    if not (valid_producer_relative_path relative_path)
    then None
    else
      (match artifact_read with
       | Some read -> (
           match read ~worker ~relative:relative_path with
           | Ok (Text_payload (_content, bytes, _truncated)) -> Some bytes
           | Ok (Binary_payload { bytes; _ }) -> Some bytes
           | Error _ -> None)
       | None ->
      let project_root = project_root_of_base_path base_path in
      let ownership_root =
        Keeper_sandbox_config.host_root_abs_of_agent
          ~base_path:project_root
          ~agent_name:worker
        |> Env_config_core.strip_trailing_slashes
      in
      let target = Filename.concat ownership_root relative_path in
      (match
         Fs_compat.load_owned_regular_file_prefix
           ~ownership_root
           ~max_bytes:1
           target
       with
       | Ok (Some prefix) -> Some prefix.file_size
       | Ok None | Error _ -> None))
  | Note_reference _ | Unresolvable_reference -> None

let snapshot_submitted_evidence_json ?artifact_read ?request_id ~base_path ~worker
    references =
  `List
    (List.mapi
       (fun index reference ->
         snapshot_submitted_evidence_item
           ?artifact_read
           ?request_id
           ~index
           ~base_path
           ~worker
           reference
         |> submitted_evidence_item_to_yojson)
       references)

(* Decode through this module's own snapshot decoder instead of re-reading the
   JSON fields. Both surfaces read the same persisted bytes, so they have to
   agree on which snapshots are well-formed: re-deriving the shape here let an
   artifact item with a [reference] but missing or invalid [content]/[bytes]/
   [truncated] render as an ordinary identity line while the authority-scoped
   payload route rejected the very same item. Matching on the typed value makes
   that divergence unrepresentable, and a new variant becomes a compile error
   here rather than an [unknown kind] string at runtime. *)
let submitted_evidence_identity_line (item : Yojson.Safe.t) =
  match submitted_evidence_item_of_yojson item with
  | Error detail -> Error detail
  | Ok (Evidence_note note) -> Ok (note_reference_prefix ^ note)
  | Ok (Evidence_artifact { reference; _ }) -> Ok reference
  | Ok Evidence_invalid_reference ->
    Ok (Printf.sprintf "(unreadable: %s)" invalid_reference_code)
  | Ok (Evidence_artifact_unreadable { reference; reason }) ->
    Ok
      (Printf.sprintf
         "%s (unreadable: %s)"
         reference
         (evidence_read_failure_code reason))
  | Ok (Evidence_artifact_binary { reference; sha256; _ }) ->
    Ok
      (Printf.sprintf
         "%s (binary: %s)"
         reference
         (String.sub sha256 0 (min 12 (String.length sha256))))
;;

let submitted_evidence_identity_lines (json : Yojson.Safe.t) =
  match json with
  | `List items ->
    List.fold_left
      (fun acc item ->
        match acc, submitted_evidence_identity_line item with
        | Error _, _ -> acc
        | Ok lines, Ok line -> Ok (line :: lines)
        | Ok _, Error detail -> Error detail)
      (Ok [])
      items
    |> Result.map List.rev
  | _ -> Error "submitted evidence must be an array"
;;

(* Decoded through this module's own snapshot decoder for the same reason as
   the identity lines above: the typed value is the one truth about which
   items are truncated, so a shape change is a compile error here rather than
   a silently empty warning at the submit site. *)
let truncated_snapshot_items (json : Yojson.Safe.t) : (string * int) list =
  match json with
  | `List items ->
    List.filter_map
      (fun item ->
         match submitted_evidence_item_of_yojson item with
         | Ok (Evidence_artifact { reference; bytes; truncated = true; _ }) ->
           Some (reference, bytes)
         | Ok (Evidence_artifact { truncated = false; _ }) -> None
         | Ok (Evidence_note _) -> None
         | Ok Evidence_invalid_reference -> None
         | Ok (Evidence_artifact_unreadable _) -> None
         | Ok (Evidence_artifact_binary _) -> None
         | Error _ -> None)
      items
  | _ -> []
;;

let inspect_submitted_evidence_for_authority ~base_path ~request_id ~task_id
    ~task_worker ~(authority : Masc_domain.completion_authority) =
  if not (Masc_domain.completion_authority_has_identity authority)
  then
    Evidence_unavailable
      { request_id; reason = Completion_authority_identity_missing }
  else
    match load_request_for_evidence base_path request_id with
    | Error reason -> Evidence_unavailable { request_id; reason }
    | Ok (request, snapshot) ->
      if
        String.equal request.task_id task_id
        && String.equal request.worker task_worker
      then Evidence_available { request; items = snapshot }
      else
        Evidence_unavailable
          { request_id
          ; reason = Request_scope_mismatch
          }
;;
