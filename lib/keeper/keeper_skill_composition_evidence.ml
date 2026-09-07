type t =
  { reference : Skill_reference.t
  ; composition_run_id : string
  ; parent_tool_use_id : string
  ; parent_turn : int
  ; parent_planned_index : int
  ; request_id : string option
  ; keeper_name : string
  ; composition_tool : string
  ; composition_execution : Keeper_tool_composition_catalog.execution_mode
  ; result : Yojson.Safe.t
  ; executor_settlements : Yojson.Safe.t list
  ; recorded_at : float
  }

type error =
  | Invalid_record of string
  | Read_failed of Fs_compat.owned_regular_file_read_error
  | Directory_prepare_failed of Keeper_fs_durable_directory.failure
  | Lock_failed of File_lock_eio.durable_lock_error
  | Write_failed of Keeper_fs.durable_write_error

type save_outcome =
  | Saved
  | Saved_with_lock_release_error of File_lock_eio.durable_lock_error

let schema = "masc.skill-composition-evidence/v1"
let store_dirname = "skill-composition-evidence-v1"

let error_to_string = function
  | Invalid_record detail -> "invalid Skill composition evidence: " ^ detail
  | Read_failed error -> Fs_compat.owned_regular_file_read_error_to_string error
  | Directory_prepare_failed _ ->
    "Skill composition evidence directory preparation failed"
  | Lock_failed error -> File_lock_eio.durable_lock_error_to_string error
  | Write_failed error -> Keeper_fs.durable_write_error_to_string error
;;

let reference value = value.reference

let execution_to_string = function
  | Keeper_tool_composition_catalog.Inline -> "inline"
  | Keeper_tool_composition_catalog.Async -> "async"
;;

let execution_of_string = function
  | "inline" -> Some Keeper_tool_composition_catalog.Inline
  | "async" -> Some Keeper_tool_composition_catalog.Async
  | _ -> None
;;

let nonblank field value =
  if String.trim value = ""
  then Error (Invalid_record (field ^ " must not be blank"))
  else Ok value
;;

let valid_schedule = function
  | `Assoc fields ->
    (match List.sort (fun (a, _) (b, _) -> String.compare a b) fields with
     | [ "batch_index", `Int batch_index
       ; "batch_size", `Int batch_size
       ; "execution_mode", `String ("serial" | "concurrent")
       ; "planned_index", `Int planned_index
       ] ->
       batch_index >= 0
       && batch_size > 0
       (* batch_index identifies the batch in the whole plan. A later serial
          batch has size 1 and a nonzero index; it is valid evidence. *)
       && planned_index >= 0
     | _ -> false)
  | _ -> false
;;

let valid_result = function
  | `Assoc fields ->
    let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
    let without_metadata = List.remove_assoc "metadata" fields in
    let metadata_count =
      List.fold_left
        (fun count (name, _) -> if String.equal name "metadata" then count + 1 else count)
        0
        fields
    in
    let valid_duration duration_ms =
      Float.is_finite duration_ms && duration_ms >= 0.0
    in
    metadata_count <= 1
    &&
    (match without_metadata with
     | [ "data", _
       ; "disposition", `String ("completed" | "deferred")
       ; "duration_ms", `Float duration_ms
       ; "tool_name", `String tool_name
       ] ->
       String.trim tool_name <> "" && valid_duration duration_ms
     | [ "data", _
       ; "disposition", `String "failed"
       ; "duration_ms", `Float duration_ms
       ; "failure_class", `String failure_class
       ; "message", `String _
       ; "tool_name", `String tool_name
       ] ->
       String.trim tool_name <> ""
       && valid_duration duration_ms
       &&
       (match Tool_result.tool_failure_class_of_string failure_class with
        | Some value ->
          String.equal failure_class (Tool_result.tool_failure_class_to_string value)
        | None -> false)
     | _ -> false)
  | _ -> false
;;

let valid_node = function
  | `Assoc fields ->
    (match List.sort (fun (a, _) (b, _) -> String.compare a b) fields with
     | [ "deferred_kind", deferred_kind
       ; "execution_id", execution_id
       ; "failure_effect_disposition", failure_effect_disposition
       ; "input", _
       ; "node_id", `String node_id
       ; "result", result
       ; "result_bytes", `Int result_bytes
       ; "schedule", schedule
       ; "tool_name", `String tool_name
       ; "tool_use_id", `String _tool_use_id
       ; "truncated_to", truncated_to
       ] ->
       let valid_failure_effect =
         match failure_effect_disposition with
         | `Null -> true
         | `String value ->
           Option.is_some (Tool_result.failure_effect_disposition_of_string value)
         | _ -> false
       in
       let valid_deferred_kind =
         match deferred_kind with
         | `Null -> true
         | `String ("generic_deferred" | "external_effect_deferred") -> true
         | _ -> false
       in
       let valid_truncation =
         match truncated_to with
         | `Null -> true
         | `Int value -> value >= 0 && value <= result_bytes
         | _ -> false
       in
       String.trim node_id <> ""
       && String.trim tool_name <> ""
       && result_bytes >= 0
       && valid_failure_effect
       && valid_deferred_kind
       && valid_truncation
       && Result.is_ok (Ids.Execution_id.of_yojson execution_id)
       && valid_schedule schedule
       && valid_result result
       && Json_util.assoc_member_opt "tool_name" result = Some (`String tool_name)
     | _ -> false)
  | _ -> false
;;

let validate_nodes nodes =
  if List.for_all valid_node nodes
  then Ok nodes
  else Error (Invalid_record "every node must carry typed identity, schedule, and result")
;;

let validate_composition_result ~composition_tool result =
  if not (valid_result result)
  then Error (Invalid_record "top-level result does not match Tool_result.to_json")
  else if Json_util.assoc_member_opt "tool_name" result <> Some (`String composition_tool)
  then Error (Invalid_record "top-level result tool does not match composition tool")
  else Ok ()
;;

let make
      ~reference
      ~composition_run_id
      ~parent_invocation
      ~request_id
      ~keeper_name
      ~composition_tool
      ~composition_execution
      ~result
      ~executor_settlements =
  let ( let* ) = Result.bind in
  let raw_run_id =
    Keeper_tool_plan.Composition_run_id.to_string composition_run_id
  in
  let* composition_run_id =
    Random_id.parse_uuid_v7 raw_run_id
    |> Result.map_error (fun detail -> Invalid_record detail)
  in
  let parent_tool_use_id =
    Agent_core.Tool_contract.Invocation.tool_use_id parent_invocation
  in
  let parent_turn = Agent_core.Tool_contract.Invocation.turn parent_invocation in
  let parent_planned_index =
    (Agent_core.Tool_contract.Invocation.schedule parent_invocation).planned_index
  in
  let* keeper_name = nonblank "keeper_name" keeper_name in
  let* composition_tool = nonblank "composition_tool" composition_tool in
  let result = Tool_result.to_json result in
  let* () = validate_composition_result ~composition_tool result in
  let* executor_settlements = validate_nodes executor_settlements in
  let* request_id =
    match composition_execution, request_id with
    | Keeper_tool_composition_catalog.Inline, None -> Ok None
    | Keeper_tool_composition_catalog.Async, Some request_id ->
      nonblank "request_id" request_id |> Result.map Option.some
    | Keeper_tool_composition_catalog.Inline, Some _ ->
      Error (Invalid_record "inline evidence must not carry request_id")
    | Keeper_tool_composition_catalog.Async, None ->
      Error (Invalid_record "async evidence requires request_id")
  in
  Ok
    { reference
    ; composition_run_id
    ; parent_tool_use_id
    ; parent_turn
    ; parent_planned_index
    ; request_id
    ; keeper_name
    ; composition_tool
    ; composition_execution
    ; result
    ; executor_settlements
    ; recorded_at = Time_compat.now ()
    }
;;

let to_yojson value =
  `Assoc
    [ "schema", `String schema
    ; "reference", Skill_reference.to_yojson value.reference
    ; "composition_run_id", `String value.composition_run_id
    ; "parent_tool_use_id", `String value.parent_tool_use_id
    ; "parent_turn", `Int value.parent_turn
    ; "parent_planned_index", `Int value.parent_planned_index
    ; "request_id", Option.fold ~none:`Null ~some:(fun value -> `String value) value.request_id
    ; "keeper", `String value.keeper_name
    ; "composition_tool", `String value.composition_tool
    ; "composition_execution", `String (execution_to_string value.composition_execution)
    ; "executor_settlements", `List value.executor_settlements
    ; "result", value.result
    ; "recorded_at", `Float value.recorded_at
    ]
;;

let of_yojson json =
  let ( let* ) = Result.bind in
  let fields =
    match json with
    | `Assoc fields -> Ok (List.sort (fun (a, _) (b, _) -> String.compare a b) fields)
    | _ -> Error (Invalid_record "record must be an object")
  in
  let* fields = fields in
  match fields with
  | [ "composition_execution", `String execution
    ; "composition_run_id", `String composition_run_id
    ; "composition_tool", `String composition_tool
    ; "executor_settlements", `List executor_settlements
    ; "keeper", `String keeper_name
    ; "parent_planned_index", `Int parent_planned_index
    ; "parent_tool_use_id", `String parent_tool_use_id
    ; "parent_turn", `Int parent_turn
    ; "recorded_at", `Float recorded_at
    ; "reference", reference_json
    ; "request_id", request_id_json
    ; "result", (`Assoc _ as result)
    ; "schema", `String observed_schema
    ] when String.equal observed_schema schema ->
    let* reference =
      Skill_reference.of_yojson reference_json
      |> Result.map_error (fun _ -> Invalid_record "reference is invalid")
    in
    let* composition_run_id =
      Random_id.parse_uuid_v7 composition_run_id
      |> Result.map_error (fun detail -> Invalid_record detail)
    in
    let* keeper_name = nonblank "keeper_name" keeper_name in
    let* composition_tool = nonblank "composition_tool" composition_tool in
    let* composition_execution =
      execution_of_string execution
      |> Option.to_result ~none:(Invalid_record "composition_execution is invalid")
    in
    let* request_id =
      match composition_execution, request_id_json with
      | Keeper_tool_composition_catalog.Inline, `Null -> Ok None
      | Keeper_tool_composition_catalog.Async, `String request_id ->
        nonblank "request_id" request_id |> Result.map Option.some
      | Keeper_tool_composition_catalog.Inline, _ ->
        Error (Invalid_record "inline evidence must not carry request_id")
      | Keeper_tool_composition_catalog.Async, _ ->
        Error (Invalid_record "async evidence requires request_id")
    in
    let* () = validate_composition_result ~composition_tool result in
    let* executor_settlements = validate_nodes executor_settlements in
    if parent_turn < 0 || parent_planned_index < 0
    then Error (Invalid_record "parent occurrence indices must not be negative")
    else if not (Float.is_finite recorded_at)
    then Error (Invalid_record "recorded_at must be finite")
    else
      Ok
        { reference
        ; composition_run_id
        ; parent_tool_use_id
        ; parent_turn
        ; parent_planned_index
        ; request_id
        ; keeper_name
        ; composition_tool
        ; composition_execution
        ; result
        ; executor_settlements
        ; recorded_at
        }
  | _ -> Error (Invalid_record "record fields do not match the v1 schema")
;;

let partition reference =
  Skill_reference.to_yojson reference
  |> Yojson.Safe.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let store_dir config =
  Filename.concat (Workspace.masc_root_dir config) store_dirname
;;

let path config reference =
  Filename.concat (store_dir config) (partition reference ^ ".json")
;;

let load_path config expected_reference =
  let ownership_root = Workspace.masc_root_dir config in
  match Fs_compat.load_owned_regular_file ~ownership_root (path config expected_reference) with
  | Error error -> Error (Read_failed error)
  | Ok None -> Ok None
  | Ok (Some bytes) ->
    (match Yojson.Safe.from_string bytes with
     | exception Yojson.Json_error detail -> Error (Invalid_record detail)
     | json ->
       let ( let* ) = Result.bind in
       let* value = of_yojson json in
       if Skill_reference.equal expected_reference value.reference
       then Ok (Some value)
       else Error (Invalid_record "reference does not match its partition"))
;;

let load_latest config reference = load_path config reference

let prepare_store_directory config =
  let ownership_root = Workspace.masc_root_dir config in
  Keeper_fs_durable_directory.ensure
    ~before_prepare:(fun () -> ())
    ~before_directory_fsync:(fun _ -> ())
    ~ownership_root
    (store_dir config)
  |> Result.map_error (fun error -> Directory_prepare_failed error)
;;

let save_latest config value =
  let ( let* ) = Result.bind in
  let* _lease = prepare_store_directory config in
  let target = path config value.reference in
  let lock_path = target ^ ".lock" in
  match
    File_lock_eio.with_durable_lock_observed ~lock_path (fun () ->
      let* _existing = load_path config value.reference in
      Keeper_fs.save_json_durable_atomic
        ~ownership_root:(Workspace.masc_root_dir config)
        ~pretty:false
        target
        (to_yojson value)
      |> Result.map_error (fun error -> Write_failed error))
  with
  | File_lock_eio.Lock_not_acquired error -> Error (Lock_failed error)
  | File_lock_eio.Body_completed { value = Error error; _ } -> Error error
  | File_lock_eio.Body_completed { value = Ok (); release_error = None } -> Ok Saved
  | File_lock_eio.Body_completed
      { value = Ok (); release_error = Some error } ->
    Ok (Saved_with_lock_release_error error)
;;
