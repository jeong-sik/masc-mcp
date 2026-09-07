open Keeper_types
open Keeper_meta_contract
open Keeper_types_profile
open Keeper_tool_shared_runtime

let workflow_rejection_error_json
      ?(rule_id = "keeper_task_argument_rejected")
      ?(typed_outcome : Keeper_tool_outcome.t option)
      message
  =
  (* RFC-0239 / audit D1: [typed_outcome] carries a top-level
     [typed_outcome] field (extracted by the PostToolUse hook into
     [tool_call_detail.typed_outcome]) so a rejected completion is seen
     as no-progress by the loop detector rather than counted as
     evidence by tool name alone. *)
  let extra_fields =
    match typed_outcome with
    | Some outcome -> [ "typed_outcome", Keeper_tool_outcome.to_json outcome ]
    | None -> []
  in
  Masc_task_handlers.Workflow_rejection_payload.payload
    ~rule_id
    ~extra_fields
    message
  |> Yojson.Safe.to_string
;;

let keeper_tool_result_json
      ?(typed_outcome = (None : Keeper_tool_outcome.t option))
      (result : Tool_result.result)
  =
  let has_json_field name fields =
    List.exists (fun (field, _) -> String.equal field name) fields
  in
  let message = Tool_result.message result in
  let disposition_field =
    "disposition", `String (Tool_result.string_of_disposition result)
  in
  let failure_class_fields =
    match result with
    | Tool_result.Failed { class_ = cls; _ } ->
      [
        ( "failure_class"
        , `String (Tool_result.tool_failure_class_to_string cls) );
      ]
    | Tool_result.Completed _ | Tool_result.Deferred _ -> []
  in
  let typed_outcome_fields =
    match typed_outcome with
    | Some outcome -> [ "typed_outcome", Keeper_tool_outcome.to_json outcome ]
    | None -> []
  in
  match Tool_result.data result with
  | `Assoc payload_fields ->
    let payload_fields =
      List.fold_left
        (fun acc (key, value) ->
           if has_json_field key acc then acc else acc @ [ key, value ])
        payload_fields
        (disposition_field :: failure_class_fields @ typed_outcome_fields)
    in
    Yojson.Safe.to_string (`Assoc payload_fields)
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _) ->
    Yojson.Safe.to_string
      (`Assoc
         ([ disposition_field
          ; (match result with
             | Tool_result.Completed _ | Tool_result.Deferred _ ->
               "result", `String message
             | Tool_result.Failed _ -> "error", `String message)
          ]
          @ failure_class_fields
          @ typed_outcome_fields))
;;

(* Caller-input validation errors carry [Tool_result.Policy_rejection], matching
   the schema-layer [Tool_input_validation] producer. The typed failure class is
   observational metadata; dispatch returns the producer payload unchanged. *)
let validation_error_json message =
  Yojson.Safe.to_string
    (`Assoc
       [ "ok", `Bool false
       ; "error", `String message
       ; ( "failure_class"
         , `String
             (Tool_result.tool_failure_class_to_string
                Tool_result.Policy_rejection) )
       ])
;;

(* [Workspace_task.add_task_error] mixes two different failure shapes:
   caller-input workflow violations and file-IO/exception failures. They
   route to different [Tool_result.tool_failure_class] values -- folding
   both into [Workflow_rejection] would make
   [Tool_result.log_level_of_failure_class] demote a durability incident to
   WARN (only [Runtime_failure] logs ERROR) and would misroute the
   terminal-effect buckets in [Keeper_runtime_failure_route]. Matched
   exhaustively (no catch-all) so a future [add_task_error] variant forces a
   classification decision here rather than defaulting into one bucket.

   [keeper_task_create] never threads [predecessor_task_id] (RFC-0323 W2
   scopes that arg to [masc_add_task]), so [Unknown_predecessor] /
   [Predecessor_not_terminal] cannot be produced through this tool's live
   args today. Exposed (like [validation_error_json] above) so the route
   split can be tested directly against all six variants without depending
   on that unreachable path. *)
type task_create_failure_route =
  | Task_create_workflow_rejection
  | Task_create_runtime_failure

let task_create_failure_route : Workspace_task.add_task_error -> task_create_failure_route
  = function
  | Workspace_task.Unknown_predecessor _
  | Workspace_task.Predecessor_not_terminal _ -> Task_create_workflow_rejection
  | Workspace_task.Backlog_read_failed _
  | Workspace_task.Goal_link_write_failed _
  | Workspace_task.Backlog_write_failed _
  | Workspace_task.Unexpected_error _ -> Task_create_runtime_failure
;;

let validate_goal_id config goal_id =
  match Goal_store.get_goal config ~goal_id with
  | Some _ -> Ok goal_id
  | None -> Error (Printf.sprintf "unknown goal_id: %s" goal_id)
;;

let resolve_task_create_goal_id ~config ~(meta : keeper_meta) args =
  match Safe_ops.json_string_opt "goal_id" args with
  | Some s when String.trim s <> "" ->
      validate_goal_id config (String.trim s) |> Result.map Option.some
  | _ -> Ok None
;;

let no_eligible_exclusion_summary =
  Masc_task_handlers.Tool_task_no_eligible.no_eligible_exclusion_summary
;;


let find_task_goal_id config task_id =
  let index = Workspace_goal_index.build_task_goal_index_for_config config in
  (* [Not_found] only covers the missing-key case. [List.hd] raises
     [Failure "hd"], which this arm would not catch, so match the list
     instead: the builder appends every goal id, but that invariant lives in
     Workspace_goal_index and is not visible in the type here. *)
  match Hashtbl.find_opt index task_id with
  | Some (goal_id :: _) -> Some goal_id
  | Some [] | None -> None
;;

let sync_keeper_meta_current_task
    ~(config : Workspace.config)
    ~(meta : keeper_meta)
    ~(task_id : string)
  =
  match Keeper_id.Task_id.of_string task_id with
  | Error msg ->
    Log.Keeper.warn ~keeper_name:meta.name
      "could not sync claimed task %s into current_task_id: %s"
      task_id msg
  | Ok current_task_id ->
    (match
       Keeper_owner_registry.apply_meta
         ~base_path:config.base_path
         ~keeper_name:meta.name
         (Keeper_owner_reducer.Set_current_task
            { task_id = Some current_task_id; updated_at = now_iso () })
     with
     | Ok (Some _) -> ()
     | Ok None ->
       Otel_metric_store.inc_counter
         Keeper_metrics.(to_string WriteMetaFailures)
         ~labels:[ "keeper", meta.name; "phase", "claim_task_id" ]
         ();
       Log.Keeper.warn
         ~keeper_name:meta.name
         "owner removed metadata while syncing claimed current_task_id=%s"
         task_id
     | Error error ->
       Otel_metric_store.inc_counter
         Keeper_metrics.(to_string WriteMetaFailures)
         ~labels:[ "keeper", meta.name; "phase", "claim_task_id" ]
         ();
       Log.Keeper.warn
         ~keeper_name:meta.name
         "failed to persist claimed current_task_id=%s: %s"
         task_id
         (Keeper_owner_registry.command_error_to_string error))
;;

(* Row shape of a [Tasks_list] page. [Compact] is the default: identity,
   ordering and claim state only ([Masc_domain.task_compact_to_yojson]).
   [Full] is the whole record, for the task(s) a keeper is about to work on.
   The argument is decoded once here; an unknown value is a validation error,
   not a silent fallback to either shape. *)
type task_projection =
  | Compact
  | Full

let task_projection_of_args args : (task_projection, string) result =
  match Safe_ops.json_string_opt "projection" args with
  | None | Some "compact" -> Ok Compact
  | Some "full" -> Ok Full
  | Some other ->
    Error
      (Printf.sprintf
         "projection must be \"compact\" or \"full\", got %S"
         other)
;;

let task_projection_to_string = function
  | Compact -> "compact"
  | Full -> "full"
;;

(* Cluster sub-dispatch via closed sum type — string [name] is converted
   into [task_op] exactly once at the entry boundary; downstream match
   is exhaustive, so adding a new op forces the compiler to flag every
   site that did not handle it.  Removes the substring-classifier
   anti-pattern (CLAUDE.md §2) from this cluster. *)
type task_op =
  | Tasks_list
  | Tasks_audit
  | Broadcast
  | Task_create
  | Task_claim
  | Task_done
  | Task_cancel
  | Task_release

let task_op_of_keeper_tool = function
  | Keeper_tooling.Name.Tasks_list -> Some Tasks_list
  | Keeper_tooling.Name.Tasks_audit -> Some Tasks_audit
  | Keeper_tooling.Name.Broadcast -> Some Broadcast
  | Keeper_tooling.Name.Task_create -> Some Task_create
  | Keeper_tooling.Name.Task_claim -> Some Task_claim
  | Keeper_tooling.Name.Task_done -> Some Task_done
  | Keeper_tooling.Name.Task_cancel -> Some Task_cancel
  | Keeper_tooling.Name.Task_release -> Some Task_release
;;

let task_op_of_name name =
  match Keeper_tooling.Name.of_string name with
  | Some tool -> task_op_of_keeper_tool tool
  | None -> None
;;

(* Entry VALIDITY is an objective parse fact and stays rejected here; a
   count floor is not. RFC-0337 (Withdrawn) removed the mandatory
   evidence floor: "local evidence shape can reject work before the
   configured judge evaluates the actual Task" is the hierarchy it
   withdrew. Completion is decided by the Task transition, so a
   missing or empty evidence_refs list proceeds to that judgment. *)
let parse_keeper_task_done_evidence_refs args =
  match args with
  | `Assoc fields ->
    (match List.assoc_opt "evidence_refs" fields with
     | None -> Ok []
     | Some (`List refs) ->
       let rec collect acc = function
         | [] -> Ok (List.rev acc)
         | `String ref_ :: rest ->
           if Task.Completion_review.blank_evidence_ref ref_
           then Error "evidence_refs must contain only non-empty strings."
           else if Task.Completion_review.unresolvable_evidence_ref ref_
           then
             (* The masc_transition boundary refuses the same entries; this
                parser states it in keeper vocabulary. Without it the store
                snapshots a payload-free invalid reference and the reviewer
                rejects for missing evidence, which reads as a verdict on the
                work rather than on the reference form. *)
             Error
               (Printf.sprintf
                  "evidence_refs entries must be %s. Nothing else can be read \
                   back at review. Wrap a Board post id, a commit, a URL, or \
                   any narrative as %s."
                  Task.Completion_review.resolvable_evidence_ref_forms
                  Task.Completion_review.note_evidence_ref_form)
           else collect (String.trim ref_ :: acc) rest
         | _ :: _ -> Error "evidence_refs must be an array of non-empty strings."
       in
       collect [] refs
     | Some _ -> Error "evidence_refs must be an array of non-empty strings.")
  | _ -> Error "keeper_task_done arguments must be an object."
;;

(* task-540: total-size pre-check on artifact: evidence at the
   keeper_task_done boundary. Oversized artifacts snapshot as truncated
   prefixes the completion authority cannot act on, and four tasks stalled
   8-16h in evaluator_unavailable before anyone saw why. Refusing here names
   the byte count and the note: escape hatch while the caller can still fix
   the call. Unmeasurable references (missing file, unreadable, non-artifact)
   pass through untouched: the snapshot layer reports those as typed
   unreadable reasons at review time, and restating that taxonomy here would
   drift. *)
(* Where an evidence artifact lives depends on the producer's sandbox
   profile, and that policy sits above the store's library. An
   endpoint-owned tree (microvm, remote-ssh) keeps its files inside the
   guest's work volume, which only the sandbox backend can read -- the
   store's direct host read reached the bookkeeping bundle instead and
   recorded every artifact as unreadable (live capture 2026-09-02 onward,
   #33745). A shared-mount (Docker) tree stays on the host playground the
   store already reads, so that keeper keeps the direct read. [None] means
   "no reader: read the host directly", which is also the store's default. *)
let evidence_artifact_reader ~config ~(meta : keeper_meta) () =
  match
    Keeper_types_profile_sandbox.tree_location_of_profile meta.sandbox_profile
  with
  | Keeper_types_profile_sandbox.Endpoint_owned ->
      Some
        (fun ~worker ~relative ->
           let module Store = Workspace_verification_store in
           let max_bytes = Store.verification_evidence_max_bytes in
           match
             Keeper_sandbox_read_backend.read_file ~config ~meta
               ~host_path:relative ~max_bytes ~timeout_sec:30. ()
           with
           | Error reason ->
               Error
                 (Store.Evidence_read_error
                    (Printf.sprintf "sandbox_backend_read: %s: %s" worker reason))
           | Ok content -> (
               (* The reader classifies its bytes with the store's own scan:
                   text answers as text, and non-text bytes become a binary
                   payload -- hash, size, format -- instead of being dropped
                   (RFC-0436 §4.1). *)
               match Store.scan_utf8 content with
               | Store.Utf8_valid ->
                   Ok (Store.Text_payload (content, String.length content, false))
               | _ ->
                   let format =
                     let ext = String.lowercase_ascii (Filename.extension relative) in
                     if ext = "" then "unknown" else ext
                   in
                   let sha256 =
                     Digestif.SHA256.(digest_string content |> to_hex)
                   in
                   Ok
                     (Store.Binary_payload
                        { data = content
                        ; bytes = String.length content
                        ; sha256
                        ; format
                        })))
  | Keeper_types_profile_sandbox.Shared_mount -> None

let evidence_artifact_total_bytes ~(config : Workspace.config)
      ~(meta : keeper_meta) evidence_refs
  =
  (* [artifact_reference_size] itself resolves the project root from
     [base_path], so no separate normalization is needed here. *)
  let artifact_read = evidence_artifact_reader ~config ~meta () in
  List.filter_map
    (fun reference ->
       Workspace_verification_store.artifact_reference_size
         ?artifact_read
         ~base_path:config.base_path
         ~worker:meta.name
         reference)
    evidence_refs
  |> List.fold_left ( + ) 0
;;

(* What one submission may hand the reviewer as artifact bytes in total. A
   resource boundary on the reviewer's inline window, not a behavioural
   gate: over it, the submitter is told to hand over an excerpt and a
   pointer instead. *)
let evidence_total_bytes_limit = 50 * 1024

let evidence_total_size_rejection ~total ~limit =
  Printf.sprintf
    "artifact total size %d bytes exceeds limit %d bytes — use note: for \
     large files. Submit a small excerpt or summary as an artifact: and the \
     pointer (path, URL, board post) as note:."
    total limit
;;

let handle_keeper_task_tool_with_outcome
      ~(config : Workspace.config)
      ~(meta : keeper_meta)
      ~(name : string)
      ~(args : Yojson.Safe.t)
  =
  match task_op_of_name name with
  | None ->
    Keeper_tool_execution.failure
      ~class_:Tool_result.Policy_rejection
      (error_json ~fields:[ "tool", `String name ] "unknown_task_tool")
  | Some op ->
    match op with
    | Tasks_list ->
    let status_filter = Safe_ops.json_string_opt "status" args in
    let include_done = Safe_ops.json_bool ~default:false "include_done" args in
    let limit = Safe_ops.json_int ~default:50 "limit" args |> max 1 |> min 100 in
    let projection_and_revision =
      match task_projection_of_args args, Snapshot_protocol.if_revision args with
      | Error message, _ | _, Error message -> Error message
      | Ok projection, Ok if_revision -> Ok (projection, if_revision)
    in
    (match projection_and_revision with
     | Error message ->
       Keeper_tool_execution.failure
         ~class_:Tool_result.Policy_rejection
         (validation_error_json message)
     | Ok (projection, if_revision) ->
       let call_filter =
         { Keeper_tasks_list_cursor.status = status_filter
         ; include_done
         ; projection = task_projection_to_string projection
         }
       in
       (* A cursor names the row the previous page ended on and the filter it
          was issued under. Anything else -- a string this runtime did not
          issue, a non-string, a cursor from another filter -- is refused as a
          rejected argument; it never falls back to the first page, which the
          caller would read as the whole backlog (#29101). *)
       let cursor =
         match Safe_ops.safe_member "cursor" args with
         | `Null -> Ok None
         | `String raw ->
           Result.map Option.some (Keeper_tasks_list_cursor.of_string ~call:call_filter raw)
         | _ -> Error Keeper_tasks_list_cursor.Cursor_unparseable
       in
       (match cursor with
        | Error error ->
          let data = Keeper_tasks_list_cursor.rejection_json error in
          Keeper_tool_execution.failure_data
            ~class_:Tool_result.Policy_rejection
            ~message:(Yojson.Safe.to_string data)
            data
        | Ok cursor ->
       match Workspace.read_backlog_observation_with_source_r config with
     | Error message ->
       let data =
         `Assoc
           [ "ok", `Bool false
           ; "error", `String message
           ; ( "failure_class"
             , `String
                 (Tool_result.tool_failure_class_to_string
                    Tool_result.Runtime_failure) )
           ]
       in
       Keeper_tool_execution.failure_data
         ~class_:Tool_result.Runtime_failure
         ~message:(Yojson.Safe.to_string data)
         data
     | Ok { Workspace.observed_backlog = backlog; recovered_from } ->
       let visible (task : Masc_domain.task) =
         match status_filter with
         | Some status ->
           String.equal status (Masc_domain.task_status_to_string task.task_status)
         | None ->
           let is_cancelled =
             match task.task_status with
             | Masc_domain.Cancelled _ -> true
             | ( Masc_domain.Todo
               | Masc_domain.Claimed _
               | Masc_domain.InProgress _
               | Masc_domain.AwaitingVerification _
               | Masc_domain.Done _ ) -> false
           in
           (include_done || not (Masc_domain.task_status_is_done task.task_status))
           && not is_cancelled
       in
       let page_key (task : Masc_domain.task) =
         { Keeper_tasks_list_cursor.priority = task.priority
         ; created_at = task.created_at
         ; id = task.id
         }
       in
       let matching =
         backlog.tasks
         |> List.filter visible
         |> List.sort (fun (left : Masc_domain.task) right ->
           Keeper_tasks_list_cursor.compare_key (page_key left) (page_key right))
       in
       let matching_count = List.length matching in
       (* The order is total -- priority, then created_at, then id -- so a page
          is "the first [limit] rows after the cursor's key" and the same row
          never appears on two pages nor vanishes between them. Eight tasks
          once stayed invisible across nineteen todo listings because the
          caller read the first page as the whole backlog (#29101); the page
          now names the row it ended on, and the next call starts there. *)
       let after_cursor =
         match cursor with
         | None -> matching
         | Some (cursor : Keeper_tasks_list_cursor.t) ->
           List.filter
             (fun task -> Keeper_tasks_list_cursor.compare_key (page_key task) cursor.after > 0)
             matching
       in
       let tasks = after_cursor |> List.filteri (fun index _ -> index < limit) in
       let remaining = List.length after_cursor - List.length tasks in
       let next_cursor =
         match remaining > 0, List.rev tasks with
         | true, last :: _ ->
           Some
             (Keeper_tasks_list_cursor.to_string
                { Keeper_tasks_list_cursor.after = page_key last; filter = call_filter })
         | true, [] | false, _ -> None
       in
       let row_to_yojson =
         match projection with
         | Compact -> Masc_domain.task_compact_to_yojson
         | Full -> Masc_domain.task_to_yojson
       in
       let tasks_json = `List (List.map row_to_yojson tasks) in
       let revision =
         Snapshot_protocol.revision_of_json
           ~namespace:"tasks"
           (`Assoc
             [ ( "backlog_authority"
               , `String
                   (if Option.is_none recovered_from
                    then "primary"
                    else "recovery_non_authoritative") )
             ; "status", Option.fold ~none:`Null ~some:(fun value -> `String value) status_filter
             ; "include_done", `Bool include_done
             ; "limit", `Int limit
             ; "projection", `String (task_projection_to_string projection)
               (* The backlog size is part of the answer, not only the page:
                  matching_count travels in every snapshot and truncated is
                  derived from it. Without it in the hash, tasks appearing or
                  leaving beyond the page kept the revision stable and an
                  if_revision caller polled `unchanged` while the backlog
                  moved — the #29101 blindness rebuilt on the conditional-read
                  path. With it, `unchanged` states that the whole response —
                  rows and statistics — is the one the caller already holds. *)
             ; "matching_count", `Int matching_count
               (* A later page is a different answer from the first one even
                  when the backlog is the same, so the cursor it was read
                  through is part of the revision. *)
             ; ( "cursor_after"
               , match cursor with
                 | None -> `Null
                 | Some cursor -> Keeper_tasks_list_cursor.to_yojson cursor )
             ; "snapshot", tasks_json
             ])
       in
       let response =
         match recovered_from with
         | None -> Snapshot_protocol.respond ~revision ~if_revision tasks_json
         | Some _ -> Snapshot_protocol.Snapshot { revision; value = tasks_json }
       in
       let data =
         match Snapshot_protocol.to_yojson response with
         | `Assoc fields ->
           let provenance =
             [ ( "backlog_authority"
               , `String
                   (if Option.is_none recovered_from
                    then "primary"
                    else "recovery_non_authoritative") )
             ; "degraded", `Bool (Option.is_some recovered_from)
             ; "projection", `String (task_projection_to_string projection)
             ]
           in
           (* Row statistics describe the rows travelling in this response.
              An [unchanged] response carries no rows, so echoing the
              snapshot-path statistics produced a self-contradiction —
              `truncated:true, returned_count:N` beside zero rows — that
              models resolved by re-issuing the identical call (211 identical
              back-to-back pairs on 2026-09-01 alone). Because the revision
              hash covers [matching_count] alongside the page rows, an
              [unchanged] response guarantees the caller's cached statistics
              are still exact — omitting them here loses nothing. *)
           let row_stats =
             match response with
             | Snapshot_protocol.Snapshot _ ->
               [ "matching_count", `Int matching_count
               ; "returned_count", `Int (List.length tasks)
               ; "truncated", `Bool (remaining > 0)
               ]
               @ (match next_cursor with
                  | Some next_cursor -> [ "next_cursor", `String next_cursor ]
                  | None -> [])
             | Snapshot_protocol.Unchanged _ -> []
           in
           `Assoc (provenance @ row_stats @ fields)
         | payload -> payload
       in
       Keeper_tool_execution.success_data data))
    | Tasks_audit ->
    let limit = Safe_ops.json_int ~default:20 "limit" args |> max 1 |> min 50 in
    let orphans =
      Workspace.audit_orphan_tasks config
      |> List.filter (fun (_, assignee) -> assignee <> meta.name)
    in
    let orphans = List.filteri (fun i _ -> i < limit) orphans in
    let items =
      List.map
        (fun (task, assignee) ->
           let task : Masc_domain.task = task in
           `Assoc
             [ "task_id", `String task.id
             ; "title", `String task.title
             ; "assignee", `String assignee
             ; "status", `String (Masc_domain.string_of_task_status task.task_status)
             ])
        orphans
    in
    Keeper_tool_execution.success
      (Yojson.Safe.to_string
         (`Assoc
            [ "orphan_count", `Int (List.length orphans)
            ; "orphans", `List items
            ; ( "typed_outcome"
              , Keeper_tool_outcome.to_json
                  (if orphans = []
                   then Keeper_tool_outcome.No_progress { reason = No_work_available }
                   else Keeper_tool_outcome.Progress) )
            ]))
    | Broadcast ->
    let message = Safe_ops.json_string ~default:"" "content" args |> String.trim in
    let task_cache_signal_result = Workspace_broadcast.task_cache_signal_of_args args in
    if message = ""
    then
      Keeper_tool_execution.failure
        ~class_:Tool_result.Policy_rejection
        (error_json (Tool_guidance.to_string Tool_guidance.Broadcast_content_required))
    else (
      match task_cache_signal_result with
      | Error detail ->
        Keeper_tool_execution.failure
          ~class_:Tool_result.Policy_rejection
          (error_json detail)
      | Ok task_cache_signal ->
      match
        (* A Keeper calling keeper_broadcast is speaking to the workspace,
           so this reaches every Keeper's conversation window. *)
        Workspace.broadcast
          ?task_cache_signal
          ~audience:Workspace_broadcast.Fleet_conversation
          config
          ~from_agent:(keeper_agent_sender ~meta)
          ~content:message
      with
      | Error (Workspace_broadcast.Broadcast_policy_rejected detail) ->
        Keeper_tool_execution.failure
          ~class_:Tool_result.Policy_rejection
          (error_json ("broadcast rejected: " ^ detail))
      | Error error ->
        Keeper_tool_execution.failure
          ~class_:Tool_result.Runtime_failure
          (error_json
             ("broadcast was not persisted: "
              ^ Workspace.broadcast_error_to_string error))
      | Ok delivery ->
        let data = Workspace_broadcast.broadcast_delivery_to_yojson delivery in
        (match delivery.mention_delivery with
         | Workspace_broadcast.Passive
         | Workspace_broadcast.Accepted
         | Workspace_broadcast.Already_accepted ->
           Keeper_tool_execution.success_data data
         | Workspace_broadcast.Pending
         | Workspace_broadcast.Deferred _ ->
           Keeper_tool_execution.deferred_data data
         | Workspace_broadcast.Rejected _ ->
           Keeper_tool_execution.failure_data
             ~class_:Tool_result.Workflow_rejection
             ~effect_disposition:Tool_result.Proven_post_effect
             ~message:
               (Tool_guidance.to_string
                  (Tool_guidance.Broadcast_delivery_rejected
                     { request_id = delivery.request_id }))
             data))
    | Task_create ->
    let title = Safe_ops.json_string ~default:"" "title" args |> String.trim in
    let description = Safe_ops.json_string ~default:"" "description" args |> String.trim in
    let priority = Safe_ops.json_int ~default:3 "priority" args |> max 1 |> min 5 in
    if title = ""
    then
      Keeper_tool_execution.failure
        ~class_:Tool_result.Policy_rejection
        (validation_error_json "title is required. Provide a clear, actionable task title.")
    else if description = ""
    then
      Keeper_tool_execution.failure
        ~class_:Tool_result.Policy_rejection
        (validation_error_json
           "description is required. Explain what needs to be done and why.")
    else (
      match resolve_task_create_goal_id ~config ~meta args with
      | Error message ->
        Keeper_tool_execution.failure
          ~class_:Tool_result.Policy_rejection
          (validation_error_json message)
      | Ok goal_id ->
          (* De-duplicated: this keeper-internal path now shares the canonical
             [Task.Args.parse_task_contract] used by the public
             masc_task_create facade. The previous local copy
             [parse_task_contract_arg] had regressed — it rejected an OMITTED
             optional [contract] via a catch-all that conflated None(omitted)
             with a wrong-typed value, which falsely failed keeper_task_create.
             Same lib, no
             dependency wall; the canonical parser handles [None | Some `Null]. *)
          (match Task.Args.parse_task_contract args with
           | Error message ->
             Keeper_tool_execution.failure
               ~class_:Tool_result.Policy_rejection
               (validation_error_json message)
           | Ok contract ->
             (match
                Workspace_task.add_task_with_result
                  ?contract
                  ?goal_id
                  config
                  ~title
                  ~priority
                  ~description
                    (* Attribute keeper-created tasks so the self-author filter
                       (below, and in claimable counting) can recognize them.
                       Without this the row has [created_by = None] and a keeper
                       is offered its own routing/report tasks back as work. *)
                  ~created_by:meta.name
              with
              | Ok created ->
                Keeper_tool_execution.success
                  (Yojson.Safe.to_string
                     (`Assoc
                        [
                          "ok", `Bool true;
                          "result", `String created.summary;
                          "goal_id", Json_util.string_opt_to_json created.goal_id;
                          ( "typed_outcome"
                          , Keeper_tool_outcome.to_json Keeper_tool_outcome.Progress );
                        ]))
              | Error err ->
                (* RFC-0239 / audit D1 shape (Task_done, above): a task-creation
                   failure is not progress, regardless of failure class.
                   Previously [Workspace_task.add_task] folded [Error err] into
                   a string and this branch always returned [ok:true,
                   typed_outcome:Progress] regardless -- the keeper could not
                   distinguish a durable write from a failed one. Class split
                   is [task_create_failure_route] above. *)
                let message = Workspace_task.add_task_error_to_string err in
                let typed_outcome = Keeper_tool_outcome.Error { reason = message } in
                (match task_create_failure_route err with
                 | Task_create_workflow_rejection ->
                   (* Same payload shape as the rejected-transition branches
                      in [Task_done] below. *)
                   Keeper_tool_execution.failure
                     ~class_:Tool_result.Workflow_rejection
                     (workflow_rejection_error_json ~typed_outcome message)
                 | Task_create_runtime_failure ->
                   (* Same failure class as the broadcast-persistence-failure
                      branch above (this file, [Broadcast] case). *)
                   Keeper_tool_execution.failure
                     ~class_:Tool_result.Runtime_failure
                     (error_json
                        ~fields:
                          [ "ok", `Bool false
                          ; ( "failure_class"
                            , `String
                                (Tool_result.tool_failure_class_to_string
                                   Tool_result.Runtime_failure) )
                          ; "typed_outcome", Keeper_tool_outcome.to_json typed_outcome
                          ]
                        message)))))
    | Task_claim ->
    let auto_claim_eligible task =
      not
        (Keeper_world_observation_inputs.task_is_self_authored_todo
           ~meta
           task)
    in
    let requested_task_id =
      Safe_ops.json_string ~default:"" "task_id" args |> String.trim
    in
    let explicit_claim_result () =
      let tasks = Workspace.get_tasks_raw config in
      let claim_specific (task : Masc_domain.task) =
        match
          Workspace.claim_task_r
            config
            ~agent_name:meta.name
            ~task_id:requested_task_id
            ()
        with
        | Ok message ->
          Workspace.Claim_next_claimed
            { task_id = requested_task_id
            ; title = task.title
            ; priority = task.priority
            ; message
            ; scope_widened = false
            }
        | Error e -> Workspace.Claim_next_error (Masc_domain.masc_error_to_string e)
      in
      match
        List.find_opt
          (fun (task : Masc_domain.task) -> String.equal task.id requested_task_id)
          tasks
      with
      | None ->
        Workspace.Claim_next_error
          (Printf.sprintf "unknown task_id: %s" requested_task_id)
      | Some task -> claim_specific task
      in
      let claim_requested_task () =
        if requested_task_id <> "" then
          explicit_claim_result ()
        else
          (* Auto-claim (no explicit task_id) must not select the keeper's own
             authored tasks: that closes the routing/report feedback loop
             (#25429) the claimable count already excludes. Explicit claim by
             task_id above is intentional and left unfiltered. The self-author
             exclusion is a [hard_filter], not a [task_filter]: a hard exclusion
             must survive the [allow_scope_fallback] widening below, otherwise a
             keeper whose backlog holds only its own routing/report tasks falls
             into the fallback, drops the goal-scope filter, and claims its own
             task right back — exactly the case this is meant to prevent. *)
          Workspace.claim_next_r config ~agent_name:meta.name
            ~hard_filter:auto_claim_eligible
            ()
      in
      let result = claim_requested_task () in
    let auto_started_ok = ref false in
    (match result with
     | Workspace.Claim_next_claimed { task_id; scope_widened; _ } ->
       sync_keeper_meta_current_task ~config ~meta ~task_id;
       ignore scope_widened;
       (* Guard: claim_next_r returns existing active tasks via Existing_claim
          (task_state_schedule.ml:302). When the task is already InProgress,
          dispatching Start produces an InvalidState transition error every
          cycle. Only auto-start when the task is in a pre-start state. *)
       let needs_start =
         let tasks = Workspace.get_tasks_raw config in
         match List.find_opt (fun (t : Masc_domain.task) -> String.equal t.id task_id) tasks with
         | Some { task_status = Masc_domain.InProgress _; _ } -> false
         | Some { task_status = Masc_domain.Done _ | Masc_domain.Cancelled _
                 | Masc_domain.AwaitingVerification _; _ } -> false
         | Some { task_status = Masc_domain.Todo | Masc_domain.Claimed _; _ } -> true
         (* The claim above succeeded, so the task was in the backlog a moment
            ago. If a concurrent write removed it, the Start dispatch below
            fails and auto_started_ok records that -- the message then does not
            claim the task was auto-started. *)
         | None -> true
       in
       if needs_start then begin
         let start_result =
           Task.Tool.handle_transition
             ~tool_name:"keeper_auto_start"
             ~start_time:0.0
             { Task.Tool.config; agent_name = keeper_agent_sender ~meta;
               sw = Eio_context.get_switch_opt () }
             (`Assoc ["task_id", `String task_id; "action", `String "start"])
         in
         auto_started_ok := Tool_result.is_success start_result
       end else
         auto_started_ok := true;
       ()
     | Workspace.Claim_next_no_unclaimed
     | Workspace.Claim_next_no_eligible _
     | Workspace.Claim_next_error _ -> ());
    let message =
      match result with
      | Workspace.Claim_next_claimed { message; _ } ->
          if !auto_started_ok then
            message ^ " Task auto-started — begin work now."
          else message
      | Workspace.Claim_next_no_unclaimed -> "No unclaimed tasks."
      | Workspace.Claim_next_no_eligible
          { excluded_count
          ; scope_excluded_count
          ; _
          } ->
        Printf.sprintf
          "No eligible tasks; searched all tasks, blocked/excluded=%d. %s"
          excluded_count
          (no_eligible_exclusion_summary ~scope_excluded_count)
      | Workspace.Claim_next_error e -> Printf.sprintf "Error: %s" e
    in
    let claimed_task_fields =
      match result with
      | Workspace.Claim_next_claimed
          { task_id; title; priority; scope_widened; _ } ->
          let matched_goal_id = find_task_goal_id config task_id in
          [
            ( "claim_observation",
              Task.Tool.build_claim_observation_payload
                ~now:(Time_compat.now ()) ~agent_name:meta.name
                ~task_id ~scope_widened );
            ( "claimed_task",
              `Assoc
                [
                  ("task_id", `String task_id);
                  ("title", `String title);
                  ("priority", `Int priority);
                  ("goal_id", Json_util.string_opt_to_json matched_goal_id);
                ] );
          ]
      | Workspace.Claim_next_no_eligible _
      | Workspace.Claim_next_no_unclaimed
      | Workspace.Claim_next_error _ -> []
    in
    let typed_outcome_field =
      match result with
      | Workspace.Claim_next_no_eligible
          { scope_excluded_count
          ; _
          } ->
        (* No goal narrows the claim pool any more, so nothing can have been
           excluded for being outside one. The field stays until its readers
           are retired with the rest of the scope surface. *)
        let all_goals_excluded = false in
        Some
          ( "typed_outcome"
          , Keeper_tool_outcome.to_json
              (Keeper_tool_outcome.No_progress
                 { reason =
                     Keeper_tool_outcome.No_eligible_tasks
                       { scope_excluded_count
                       ; all_goals_excluded
                       }
                 }) )
      | Workspace.Claim_next_no_unclaimed ->
        Some
          ( "typed_outcome"
          , Keeper_tool_outcome.to_json
              (Keeper_tool_outcome.No_progress
                 { reason = Keeper_tool_outcome.No_work_available }) )
      | Workspace.Claim_next_error e ->
        Some
          ( "typed_outcome"
          , Keeper_tool_outcome.to_json
              (Keeper_tool_outcome.Error
                 { reason = Printf.sprintf "keeper_task_claim rejected: %s" e }) )
      (* A claimed task carries no typed_outcome field. Keeper_tool_outcome
         .is_nonprogress reads its absence the same as Progress, and adding the
         field here would change the tool payload. *)
      | Workspace.Claim_next_claimed _ -> None
    in
    let payload =
      Yojson.Safe.to_string
        (`Assoc
           ([
              ("result", `String message);
            ]
             @ (match typed_outcome_field with
                | Some field -> [ field ]
                | None -> [])
           @ claimed_task_fields))
    in
    (match result with
     | Workspace.Claim_next_error _ ->
       Keeper_tool_execution.failure ~class_:Tool_result.Workflow_rejection payload
     | Workspace.Claim_next_claimed _
     | Workspace.Claim_next_no_unclaimed
     | Workspace.Claim_next_no_eligible _ -> Keeper_tool_execution.success payload)
    | Task_release ->
    (* Handing a task back is the other half of the claim refusal in
       [Workspace_task_claim.held_tasks_refusal_message]: a keeper that
       cannot finish what it holds is also barred from claiming anything
       else, and before this tool existed its only way out was for the
       keeper to be shut down (keeper_shutdown_finalize.ml:137). The
       release is explicit and carries a summary; #18839 removed the
       implicit auto-release because a task with no handoff note travelled
       between keepers losing its progress. *)
    let task_id = Safe_ops.json_string ~default:"" "task_id" args |> String.trim in
    let summary = Safe_ops.json_string ~default:"" "summary" args |> String.trim in
    let optional_field name =
      match Safe_ops.json_string ~default:"" name args |> String.trim with
      | "" -> []
      | value -> [ name, `String value ]
    in
    if task_id = ""
    then
      Keeper_tool_execution.failure
        ~class_:Tool_result.Workflow_rejection
        (workflow_rejection_error_json
           ~typed_outcome:
             (Keeper_tool_outcome.Error
                { reason = "keeper_task_release rejected: task_id required" })
           "task_id is required.")
    else if summary = ""
    then
      (* The next owner reads this and nothing else about where the work
         stands, so an empty summary is refused here rather than stored. *)
      Keeper_tool_execution.failure
        ~class_:Tool_result.Workflow_rejection
        (workflow_rejection_error_json
           ~typed_outcome:
             (Keeper_tool_outcome.Error
                { reason = "keeper_task_release rejected: summary required" })
           "summary is required. Say where the task stands so the next owner \
            can start from it. Example: summary='reproduced on the merged-cell \
            table, fix not started'.")
    else (
      let args_for_transition =
        [ "task_id", `String task_id
        ; "action", `String "release"
        ; ( "handoff_context"
          , `Assoc
              (("summary", `String summary)
               :: (optional_field "reason" @ optional_field "next_step")) )
        ]
      in
      let transition_result =
        Task.Tool.handle_transition
          ~tool_name:"keeper_task_release"
          ~start_time:0.0
          { Task.Tool.config
          ; agent_name = keeper_agent_sender ~meta
          ; sw = Eio_context.get_switch_opt ()
          }
          (`Assoc args_for_transition)
      in
      let payload =
        keeper_tool_result_json
          ~typed_outcome:
            (match transition_result with
             | Tool_result.Completed _ -> Some Keeper_tool_outcome.Progress
             | Tool_result.Deferred _ -> None
             | Tool_result.Failed _ ->
               (* A refused release (not the owner, stale version) left the
                  keeper holding the task, so it is not progress. *)
               Some
                 (Keeper_tool_outcome.Error
                    { reason = Tool_result.message transition_result }))
          transition_result
      in
      match transition_result with
      | Tool_result.Completed _ -> Keeper_tool_execution.success payload
      | Tool_result.Deferred { metadata; _ } ->
        Keeper_tool_execution.deferred_data ?metadata (Tool_result.data transition_result)
      | Tool_result.Failed { class_; _ } ->
        Keeper_tool_execution.failure ~class_ payload)
    | Task_cancel ->
    let task_id = Safe_ops.json_string ~default:"" "task_id" args |> String.trim in
    let reason = Safe_ops.json_string ~default:"" "reason" args |> String.trim in
    if task_id = ""
    then
      Keeper_tool_execution.failure
        ~class_:Tool_result.Workflow_rejection
        (workflow_rejection_error_json
           ~typed_outcome:
             (Keeper_tool_outcome.Error
                { reason = "keeper_task_cancel rejected: task_id required" })
           "task_id is required.")
    else if reason = ""
    then
      (* The authority judges this and nothing else, so an empty reason is
         refused here rather than sent as a case with no argument. *)
      Keeper_tool_execution.failure
        ~class_:Tool_result.Workflow_rejection
        (workflow_rejection_error_json
           ~typed_outcome:
             (Keeper_tool_outcome.Error
                { reason = "keeper_task_cancel rejected: reason required" })
           "reason is required. Say why this task should stop existing rather \
            than move to someone else. Example: reason='the defect was fixed \
            in #32078; what this task describes no longer occurs'.")
    else (
      let args_for_transition =
        [ "task_id", `String task_id
        ; "action", `String "cancel"
        ; "reason", `String reason
        ]
      in
      let transition_result =
        Task.Tool.handle_transition
          ~tool_name:"keeper_task_cancel"
          ~start_time:0.0
          { Task.Tool.config
          ; agent_name = keeper_agent_sender ~meta
          ; sw = Eio_context.get_switch_opt ()
          }
          (`Assoc args_for_transition)
      in
      let payload =
        keeper_tool_result_json
          ~typed_outcome:
            (match transition_result with
             (* The task is waiting for a verdict, not cancelled. Reporting
                progress here would tell the keeper it is finished with a
                task it still holds. *)
             | Tool_result.Completed _ -> Some Keeper_tool_outcome.Progress
             | Tool_result.Deferred _ -> None
             | Tool_result.Failed _ ->
               Some
                 (Keeper_tool_outcome.Error
                    { reason = Tool_result.message transition_result }))
          transition_result
      in
      match transition_result with
      | Tool_result.Completed _ -> Keeper_tool_execution.success payload
      | Tool_result.Deferred { metadata; _ } ->
        Keeper_tool_execution.deferred_data ?metadata (Tool_result.data transition_result)
      | Tool_result.Failed { class_; _ } ->
        Keeper_tool_execution.failure ~class_ payload)
    | Task_done ->
    let task_id = Safe_ops.json_string ~default:"" "task_id" args |> String.trim in
    let result_text = Safe_ops.json_string ~default:"" "result" args |> String.trim in
    if task_id = ""
    then
      Keeper_tool_execution.failure
        ~class_:Tool_result.Workflow_rejection
        (workflow_rejection_error_json
           ~typed_outcome:
             (Keeper_tool_outcome.Error
                { reason = "keeper_task_done rejected: task_id required" })
           "task_id is required.")
    else if result_text = ""
    then
      (* Schema (tool_shard_types.ml:1447) declares [result] as a
         required, minLength:1 field. Other agents verify completion
         from this field, so an empty result hides the audit trail.
         Previously the handler accepted an empty result and either
         (a) silently passed non-strict tasks done with no summary or
         (b) deferred the rejection to parse_handoff_context for
         strict-contract tasks (where keepers received the confusing
         "handoff_context.summary is required" message instead of a
         keeper-vocabulary error). Enforce the schema here so the
         error names the field the keeper actually sent. *)
      Keeper_tool_execution.failure
        ~class_:Tool_result.Workflow_rejection
        (workflow_rejection_error_json
           ~typed_outcome:
             (Keeper_tool_outcome.Error
                { reason = "keeper_task_done rejected: result required" })
           "result is required. Audit trail: describe what you completed. \
            Example: result='Refactored module X, all tests green, no flake'.")
    else (
      match parse_keeper_task_done_evidence_refs args with
      | Error message ->
        Keeper_tool_execution.failure
          ~class_:Tool_result.Workflow_rejection
          (workflow_rejection_error_json
             ~typed_outcome:
               (Keeper_tool_outcome.Error
                  { reason = "keeper_task_done rejected: evidence_refs required" })
             message)
      | Ok evidence_refs ->(
      (* task-540: refuse oversized artifact: evidence here, before the
         transition, so the caller learns the byte count and the note: escape
         hatch while it can still fix the call — instead of the completion
         authority later staring at a truncated prefix. *)
      let total = evidence_artifact_total_bytes ~config ~meta evidence_refs in
      let limit = evidence_total_bytes_limit in
      if total > limit then
        Keeper_tool_execution.failure
          ~class_:Tool_result.Workflow_rejection
          (workflow_rejection_error_json
             ~typed_outcome:
               (Keeper_tool_outcome.Error
                  { reason = "keeper_task_done rejected: evidence too large" })
             (evidence_total_size_rejection ~total ~limit))
      else (
      (* A Keeper submits evidence; only the completion authority can issue the
         terminal verdict. *)
      let action = "submit_for_verification" in
      let args_for_transition =
        [
          "task_id", `String task_id;
          "action", `String action;
          "notes", `String result_text;
          ( "handoff_context",
            `Assoc
              [ "summary", `String result_text
              ; "evidence_refs", Json_util.json_string_list evidence_refs
              ] );
        ]
      in
      let transition_result =
        Task.Tool.handle_transition
          ~tool_name:"keeper_task_done"
          ~start_time:0.0
          {
            Task.Tool.config;
            agent_name = keeper_agent_sender ~meta;
            sw = Eio_context.get_switch_opt ();
          }
          (`Assoc args_for_transition)
      in
      let payload =
        keeper_tool_result_json
          ~typed_outcome:
            (match transition_result with
             | Tool_result.Completed _ -> Some Keeper_tool_outcome.Progress
             | Tool_result.Deferred _ -> None
             | Tool_result.Failed _ ->
               (* RFC-0239 / audit D1: a rejected completion (wrong owner, stale
                  or invalid transition) is not progress. Emit a typed Error so the
                  no-progress detector demotes it instead of counting the tool name
                  as evidence. *)
               Some
                 (Keeper_tool_outcome.Error
                    { reason = Tool_result.message transition_result }))
          transition_result
      in
      match transition_result with
      | Tool_result.Completed _ -> Keeper_tool_execution.success payload
      | Tool_result.Deferred { metadata; _ } ->
        Keeper_tool_execution.deferred_data ?metadata (Tool_result.data transition_result)
      | Tool_result.Failed { class_; _ } ->
        Keeper_tool_execution.failure ~class_ payload)))
;;

let handle_keeper_task_tool ~config ~meta ~name ~args =
  (handle_keeper_task_tool_with_outcome ~config ~meta ~name ~args).raw_output
;;
