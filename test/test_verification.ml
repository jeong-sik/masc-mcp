(** Tests for Verification module *)

(* Mirage_crypto_rng is consumed by V.generate_id (#7544). *)
let () = Mirage_crypto_rng_unix.use_default ()

module V = Masc.Verification
module P = Masc.Otel_metric_store
module VS = Workspace_verification_store
module CU = Workspace_utils
module W = Workspace_core
module VP = Masc.Verification_protocol
module CA = Masc.Completion_authority_agent

let persistence_surface = "verification"

let persistence_counter reason =
  P.metric_value_or_zero P.metric_persistence_read_drops
    ~labels:[("surface", persistence_surface); ("reason", reason)] ()

(* Initialize mirage-crypto-rng once (needed by Verification.generate_id). *)
let () = Mirage_crypto_rng_unix.use_default ()

let active_verifications_dir base_path =
  Filename.concat (CU.masc_dir_from_base_path ~base_path) "verifications"

(** Use a temporary directory for each test.

    Cleanup goes through [Masc_test_deps.cleanup_test_workspace], which stats
    with [Unix.lstat]. The previous local [rm_rf] tested [Sys.file_exists],
    which resolves symlinks: a dangling link read as absent, was skipped, and
    left its parent non-empty, so [Unix.rmdir] raised [ENOTEMPTY] out of the
    [Fun.protect] finally and masked the test result. *)
let with_temp_dir f =
  let dir = Filename.temp_dir "masc_verify_test" "" in
  Fun.protect
    ~finally:(fun () -> Masc_test_deps.cleanup_test_workspace dir)
    (fun () -> f dir)

let with_eio_temp_dir f =
  Eio_main.run @@ fun env ->
  Fs_compat.set_fs (Eio.Stdenv.fs env);
  Fun.protect
    ~finally:Fs_compat.clear_fs
    (fun () -> with_temp_dir f)

let with_eio_temp_dir_and_clock f =
  Eio_main.run @@ fun env ->
  Fs_compat.set_fs (Eio.Stdenv.fs env);
  Fun.protect
    ~finally:Fs_compat.clear_fs
    (fun () -> with_temp_dir (f ~clock:(Eio.Stdenv.clock env)))

let scan_exn base_path =
  match V.list_requests base_path with
  | Ok scan -> scan
  | Error detail -> Alcotest.fail detail
;;

(* Most cases here care only about the requests that read. Asserting the scan
   found nothing unreadable keeps them honest: without it a case could pass
   while quietly rejecting the record it meant to be reading. *)
let list_requests_exn base_path =
  let scan = scan_exn base_path in
  Alcotest.(check int)
    "no unreadable requests" 0 (List.length scan.V.unreadable);
  scan.V.readable
;;

let ensure_keeper_meta (config : Workspace_core.config) name =
  let profile_path =
    Keeper_sandbox_config.keeper_toml_path
      ~base_path:config.base_path
      ~agent_name:name
  in
  Fs_compat.mkdir_p (Filename.dirname profile_path);
  Out_channel.with_open_text profile_path (fun channel ->
    Printf.fprintf
      channel
      "[keeper]\ninstructions = \"verification test producer\"\nsandbox_profile = \"docker\"\n");
  match
    Result.bind
      (Masc_test_deps.meta_of_json_fixture
         (`Assoc [ "name", `String name; "always_allow", `Bool true ]))
      (Masc.Keeper_meta_store.replace_snapshot config)
  with
  | Ok _ -> ()
  | Error detail -> Alcotest.failf "write keeper meta failed: %s" detail
;;

let ensure_producer_playground (config : Workspace_core.config) producer =
  let path =
    Keeper_sandbox_config.host_root_abs_of_agent
      ~base_path:
        (Workspace_verification_store.project_root_of_base_path config.base_path)
      ~agent_name:producer
  in
  let rec mkdir_p dir =
    if not (Sys.file_exists dir)
    then (
      mkdir_p (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  mkdir_p path
;;

let test_verdict_event_preserves_typed_authority () =
  let event =
    VP.For_testing.verdict_event_json
      ~authority:(Masc_domain.System_llm_agent { agent_run_id = "agent_core-agent-run-7" })
      ~task_id:"task-001"
      ~verification_id:"vrf-001"
      ~verdict:Masc_domain.Verdict_approved
      ~notes:"reviewed evidence"
      ~timestamp:1234.5
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "event type"
    "masc/verification/verdict"
    (event |> member "type" |> to_string);
  Alcotest.(check string)
    "authority kind"
    "system_llm_agent"
    (event |> member "authority_kind" |> to_string);
  Alcotest.(check string)
    "authority actor"
    "agent_core-agent-run-7"
    (event |> member "authority_actor" |> to_string);
  Alcotest.(check bool)
    "event does not expose verifier role"
    false
    (match event with
     | `Assoc fields -> List.mem_assoc "verifier" fields
     | _ -> Alcotest.fail "verdict event must be an object")

(* The stalled-review board projection is the only surface that tells the
   assignee a non-retryable deferral happened and how to move forward.
   Pin the content naming both forward paths and the typed metadata. *)
let test_stalled_projection_names_forward_paths () =
  let content =
    VP.For_testing.stalled_board_content
      ~task_id:"task-101"
      ~verification_id:"vrf-101"
      ~gate:"artifact_unreadable"
      ~detail:"evidence path escapes the playground"
  in
  let contains needle =
    let nl = String.length needle and hl = String.length content in
    let rec loop i =
      i + nl <= hl
      && (String.equal (String.sub content i nl) needle || loop (i + 1))
    in
    nl = 0 || loop 0
  in
  List.iter
    (fun needle ->
       Alcotest.(check bool)
         (Printf.sprintf "content names %S" needle)
         true
         (contains needle))
    [ "task-101"
    ; "vrf:vrf-101"
    ; "artifact_unreadable"
    ; "evidence path escapes the playground"
    ; "submit_for_verification"
    ; "HITL"
    ]

let test_stalled_metadata_preserves_typed_authority () =
  let metadata =
    VP.For_testing.stalled_metadata
      ~authority:(Masc_domain.System_llm_agent { agent_run_id = "agent_core-agent-run-9" })
      ~task_id:"task-102"
      ~verification_id:"vrf-102"
      ~gate:"review_preparation"
      ~detail:"required artifact list is empty"
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "metadata type"
    "verification_stalled"
    (metadata |> member "type" |> to_string);
  Alcotest.(check string)
    "task id"
    "task-102"
    (metadata |> member "task_id" |> to_string);
  Alcotest.(check string)
    "authority kind"
    "system_llm_agent"
    (metadata |> member "authority_kind" |> to_string);
  Alcotest.(check string)
    "gate"
    "review_preparation"
    (metadata |> member "gate" |> to_string);
  Alcotest.(check string)
    "detail"
    "required artifact list is empty"
    (metadata |> member "detail" |> to_string)

(* A stalled review is rediscovered by every backlog walk, because the Task
   keeps its verification_id while it waits for its producer. Posting on each
   rediscovery turned one stall into 40+ Board posts over 1h44m
   (goal-failure-storm-cost-20260828). The Board is what the repeat is about,
   so the Board is what the notice asks. *)
let stall_authority =
  Masc_domain.System_llm_agent { agent_run_id = "agent_core-agent-run-stall" }
;;

(* Count this test's own stall posts, by the same typed metadata the notice
   reads, so sibling tests posting to the shared hearth cannot move the
   number. *)
let stalled_posts_for ~verification_id =
  Masc.Board_dispatch.list_posts ~hearth:"verification" ~limit:200 ()
  |> List.filter (fun (post : Masc.Board.post) ->
    match post.meta_json with
    | Some (`Assoc fields) ->
      List.assoc_opt "type" fields = Some (`String "verification_stalled")
      && List.assoc_opt "verification_id" fields
         = Some (`String verification_id)
    | Some _ | None -> false)
  |> List.length
;;

let test_the_same_stall_is_posted_once () =
  Eio_main.run @@ fun _env ->
  Masc.Board_dispatch.reset_for_test ();
  for _ = 1 to 3 do
    VP.notify_stalled_verification ~authority:stall_authority
      ~task_id:"task-stall" ~verification_id:"vrf-stall-1"
      ~gate:"evaluator_unavailable"
      ~detail:"requested runtime or lane not found"
  done;
  Alcotest.(check int) "three rediscoveries, one post" 1
    (stalled_posts_for ~verification_id:"vrf-stall-1")
;;

let test_a_different_stall_still_reaches_the_board () =
  Eio_main.run @@ fun _env ->
  Masc.Board_dispatch.reset_for_test ();
  VP.notify_stalled_verification ~authority:stall_authority
    ~task_id:"task-stall" ~verification_id:"vrf-stall-1"
    ~gate:"evaluator_unavailable"
    ~detail:"requested runtime or lane not found";
  VP.notify_stalled_verification ~authority:stall_authority
    ~task_id:"task-stall" ~verification_id:"vrf-stall-1"
    ~gate:"review_preparation"
    ~detail:"requested runtime or lane not found";
  VP.notify_stalled_verification ~authority:stall_authority
    ~task_id:"task-stall" ~verification_id:"vrf-stall-1"
    ~gate:"evaluator_unavailable"
    ~detail:"the evaluator answered with an empty verdict";
  Alcotest.(check int)
    "a new gate and a new detail are each their own stall" 3
    (stalled_posts_for ~verification_id:"vrf-stall-1")
;;

let test_rejected_verdict_event_preserves_wire_type () =
  let event =
    VP.For_testing.verdict_event_json
      ~authority:(Masc_domain.System_llm_agent { agent_run_id = "agent_core-agent-run-8" })
      ~task_id:"task-002"
      ~verification_id:"vrf-002"
      ~verdict:(Masc_domain.Verdict_rejected { reason = "insufficient evidence" })
      ~notes:""
      ~timestamp:1234.5
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "rejected event type"
    "masc/verification/rejected"
    (event |> member "type" |> to_string);
  Alcotest.(check string)
    "rejected reason"
    "insufficient evidence"
    (event |> member "reason" |> to_string)

(* --- Criterion tests --- *)

let test_criterion_roundtrip () =
  let criteria = [ "output should be helpful"; "artifact exists" ] in
  List.iter (fun c ->
    let json = V.criterion_to_yojson c in
    match V.criterion_of_yojson json with
    | Ok result ->
        Alcotest.(check bool) "criterion roundtrip" true
          (V.equal_criterion c result)
    | Error e -> Alcotest.fail e
  ) criteria

let test_criterion_of_yojson_errors () =
  let bad_cases = [
    (`String "", "blank");
    (`String "  ", "whitespace");
    (`Assoc [], "object");
    (`Null, "null");
  ] in
  List.iter (fun (json, label) ->
    match V.criterion_of_yojson json with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail (Printf.sprintf "%s should fail" label)
  ) bad_cases

let valid_request_json =
  `Assoc
    [ "id", `String "vrf-1"
    ; "task_id", `String "task-1"
    ; "output", `Assoc [ "evidence_refs", `List [ `String "note:done" ] ]
    ; "criteria", `List [ `String "done" ]
    ; "worker", `String "worker-1"
    ; "created_at", `Float 1234.5
    ]

let test_request_of_yojson_is_strict () =
  let expect_error label json =
    match V.request_of_yojson json with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail (label ^ " must be rejected")
  in
  expect_error "missing output"
    (`Assoc
      [ "id", `String "vrf-1"
      ; "task_id", `String "task-1"
      ; "criteria", `List []
      ; "worker", `String "worker-1"
      ; "created_at", `Float 1234.5
      ]);
  expect_error "missing criteria"
    (`Assoc
      [ "id", `String "vrf-1"
      ; "task_id", `String "task-1"
      ; "output", `Null
      ; "worker", `String "worker-1"
      ; "created_at", `Float 1234.5
      ]);
  expect_error "malformed criterion"
    (`Assoc
      [ "id", `String "vrf-1"
      ; "task_id", `String "task-1"
      ; "output", `Null
      ; "criteria", `List [ `String "" ]
      ; "worker", `String "worker-1"
      ; "created_at", `Float 1234.5
      ]);
  expect_error "missing created_at"
    (`Assoc
      [ "id", `String "vrf-1"
      ; "task_id", `String "task-1"
      ; "output", `Null
      ; "criteria", `List []
      ; "worker", `String "worker-1"
      ]);
  expect_error "blank worker"
    (`Assoc
      [ "id", `String "vrf-1"
      ; "task_id", `String "task-1"
      ; "output", `Null
      ; "criteria", `List []
      ; "worker", `String "   "
      ; "created_at", `Float 1234.5
      ]);
  match V.request_of_yojson valid_request_json with
  | Ok request ->
    Alcotest.(check string) "strict parser preserves request id" "vrf-1" request.id
  | Error detail -> Alcotest.fail detail

let test_system_llm_authority_helpers_are_typed () =
  (match
     Masc.Completion_authority_agent.For_testing.evidence_refs_of_output
       (`Assoc [ "evidence_refs", `List [ `String "note:one"; `String "artifact:two" ] ])
   with
   | Ok [ "note:one"; "artifact:two" ] -> ()
   | Ok _ | Error _ -> Alcotest.fail "evidence refs must preserve the typed list"
  );
  match
    Masc.Completion_authority_agent.For_testing.completion_verdict_of_review
      (Masc.Task.Anti_rationalization.Reject "missing evidence")
  with
  | Masc_domain.Verdict_rejected { reason } ->
    Alcotest.(check string) "typed rejection reason" "missing evidence" reason
  | Masc_domain.Verdict_approved -> Alcotest.fail "reject must remain a rejection"

let test_system_llm_retry_disposition_is_typed () =
  let module For_testing = Masc.Completion_authority_agent.For_testing in
  (match For_testing.process_outcome_of_evaluator_retryable (Some true) with
   | For_testing.Retryable_deferred -> ()
   | For_testing.Committed | For_testing.Deferred ->
     Alcotest.fail "typed retryable evaluator failure must re-arm the lane");
  List.iter
    (fun retryable ->
       match For_testing.process_outcome_of_evaluator_retryable retryable with
       | For_testing.Deferred -> ()
       | For_testing.Committed | For_testing.Retryable_deferred ->
         Alcotest.fail "non-retryable or unclassified deferral must await action")
    [ Some false; None ]

(* One submission must review that submission. The backlog read stays whole —
   the daemon still needs fresh task state — but the scope decides which awaiting
   entries it acts on, so an unrelated submit no longer re-reviews every other
   awaiting Task (task-443, 2026-08-23: 45 attempts in 5h on the same input). *)
let test_scan_scope_limits_a_submission_to_its_own_verification () =
  let module For_testing = Masc.Completion_authority_agent.For_testing in
  let key task_id verification_id : For_testing.review_key =
    { task_id; verification_id }
  in
  let stuck = key "task-443" "vrf-1" in
  let fresh = key "task-465" "vrf-9" in
  let entries = [ stuck, "producer-a"; fresh, "producer-b" ] in
  let names selected =
    List.map
      (fun ((k : For_testing.review_key), _) -> k.task_id ^ "/" ^ k.verification_id)
      selected
  in
  Alcotest.(check (list string))
    "a named target reviews only itself"
    [ "task-465/vrf-9" ]
    (names (For_testing.entries_in_scope ~scope:(For_testing.Targets [ fresh ]) entries));
  Alcotest.(check (list string))
    "boot recovery still reads everything"
    [ "task-443/vrf-1"; "task-465/vrf-9" ]
    (names (For_testing.entries_in_scope ~scope:For_testing.Whole_backlog entries));
  (* A re-submission carries a new verification_id, so it is a different key and
     is admitted; the stale key matches nothing and drops out on its own. *)
  Alcotest.(check (list string))
    "a stale target admits nothing"
    []
    (names
       (For_testing.entries_in_scope
          ~scope:(For_testing.Targets [ key "task-443" "vrf-0" ])
          entries));
  Alcotest.(check (list string))
    "an empty target list reviews nothing"
    []
    (names (For_testing.entries_in_scope ~scope:(For_testing.Targets []) entries))


(* RFC-0417 §4.1/§6.3: the system lane's authority ends where a cancellation
   begins. The routing is pure and read off the status; the runtime path
   consults it before any review starts, records a cancel claim as
   [Operator_routed], and leaves the Task pending for the operator's one
   click — no timer behind it, and no prompt: the question type has no arm a
   cancel could render through. *)
let test_cancel_claim_is_routed_to_the_operator () =
  let module For_testing = Masc.Completion_authority_agent.For_testing in
  let awaiting intent =
    Masc_domain.AwaitingVerification
      { assignee = "keeper-a"
      ; started_at = "2026-09-05T00:00:00Z"
      ; submitted_at = "2026-09-05T00:01:00Z"
      ; intent
      ; verification_id = "vrf-routing"
      }
  in
  Alcotest.(check bool)
    "completion review stays with the system lane"
    true
    (For_testing.admission_of_status (awaiting Masc_domain.Complete_task)
     = For_testing.Review_completion);
  Alcotest.(check bool)
    "a cancel claim is the operator's, and no review starts"
    true
    (For_testing.admission_of_status (awaiting Masc_domain.Cancel_task)
     = For_testing.Operator_routed);
  Alcotest.(check bool)
    "a Task that is not awaiting anything is not an obligation"
    true
    (For_testing.admission_of_status
       (Masc_domain.InProgress
          { assignee = "keeper-a"; started_at = "2026-09-05T00:00:00Z" })
     = For_testing.Not_awaiting)


let test_system_llm_review_notes_are_metadata_only () =
  let request : V.verification_request =
    { id = "vrf-metadata-only"
    ; task_id = "task-metadata-only"
    ; output =
        `Assoc [ "secret_output", `String "must not be duplicated" ]
    ; criteria = [ "secret criterion should stay in the audit store" ]
    ; worker = "omega"
    ; created_at = 1234.5
    }
  in
  let evidence_access : VS.submitted_evidence_access =
    VS.Evidence_available
      { request =
          { id = request.id
          ; task_id = request.task_id
          ; worker = request.worker
          ; created_at = request.created_at
          }
      ; items =
          [ VS.Evidence_note "secret narrative must not be duplicated"
          ; VS.Evidence_artifact
              { reference = "artifact:proof.txt"
              ; content = "secret artifact content must not be duplicated"
              ; bytes = 42
              ; truncated = true
              }
          ; VS.Evidence_artifact_unreadable
              { reference = "artifact:missing.txt"; reason = VS.Evidence_missing }
          ; VS.Evidence_artifact_unreadable
              { reference = "artifact:unreadable.txt"
              ; reason =
                  VS.Evidence_read_error
                    "Unix.Unix_error(ENOENT, open, /private/producer/secret.txt)"
              }
          ; VS.Evidence_invalid_reference
          ]
      }
  in
  let result : Masc.Task.Anti_rationalization.review_result =
    { verdict = Some (Masc.Task.Anti_rationalization.Reject "insufficient proof")
    ; evaluator_runtime = "review-runtime"
    ; generator_runtime = None
    ; gate = Masc.Task.Anti_rationalization.Structured_tool
    ; fallback_reason = None
    ; evaluator_error_retryable = None
    }
  in
  let notes =
    CA.For_testing.review_notes
      ~request
      ~evidence_access
      ~result
      ~authority:(Masc_domain.System_llm_agent { agent_run_id = "system-run" })
  in
  Alcotest.(check bool)
    "artifact content is not duplicated into task notes"
    false
    (String_util.contains_substring notes "secret artifact content must not be duplicated");
  Alcotest.(check bool)
    "narrative content is not duplicated into task notes"
    false
    (String_util.contains_substring notes "secret narrative must not be duplicated");
  Alcotest.(check bool)
    "verification output is not duplicated into task notes"
    false
    (String_util.contains_substring notes "must not be duplicated");
  Alcotest.(check bool)
    "verification criteria are not duplicated into task notes"
    false
    (String_util.contains_substring notes "secret criterion should stay in the audit store");
  Alcotest.(check bool)
    "artifact reference remains observable"
    true
    (String_util.contains_substring notes "artifact:proof.txt");
  Alcotest.(check bool)
    "truncation remains observable"
    true
    (String_util.contains_substring notes "truncated");
  Alcotest.(check bool)
    "verification creation time remains observable"
    true
    (String_util.contains_substring notes "1234.5");
  Alcotest.(check bool)
    "rejection reason remains observable"
    true
    (String_util.contains_substring notes "insufficient proof");
  Alcotest.(check bool)
    "verification identity remains observable"
    true
    (String_util.contains_substring notes "vrf-metadata-only");
  Alcotest.(check bool)
    "read error detail is not duplicated into task notes"
    false
    (String_util.contains_substring notes "/private/producer/secret.txt");
  Alcotest.(check bool)
    "stable read error code remains observable"
    true
    (String_util.contains_substring notes "read_error");
  Alcotest.(check bool)
    "invalid raw reference is not duplicated into task notes"
    false
    (String_util.contains_substring notes "/private/producer/invalid-reference.txt");
  Alcotest.(check bool)
    "stable invalid-reference code remains observable"
    true
    (String_util.contains_substring notes "invalid_reference");
  let unavailable_metadata =
    VS.submitted_evidence_access_metadata_to_yojson
      (VS.Evidence_unavailable
         { request_id = request.id
         ; reason =
             VS.Request_load_error "failed to read /private/producer/request.json"
         })
    |> Yojson.Safe.to_string
  in
  Alcotest.(check bool)
    "unavailable detail is not duplicated into metadata"
    false
    (String_util.contains_substring unavailable_metadata "/private/producer/request.json")
  ; Alcotest.(check bool)
      "unavailable reason code remains observable"
      true
      (String_util.contains_substring unavailable_metadata "request_load_error")
  ; let unavailable_audit =
      VS.submitted_evidence_access_to_yojson
        (VS.Evidence_unavailable
           { request_id = request.id
           ; reason =
               VS.Request_load_error "failed to read /private/producer/request.json"
           })
      |> Yojson.Safe.to_string
    in
    Alcotest.(check bool)
      "full audit keeps the unavailable detail"
      true
      (String_util.contains_substring unavailable_audit "/private/producer/request.json")

let test_unreadable_evidence_uses_structured_current_contract () =
  let request : VS.request_header =
    { id = "vrf-structured-reason"
    ; task_id = "task-structured-reason"
    ; worker = "omega"
    ; created_at = 1.0
    }
  in
  let json =
    VS.submitted_evidence_access_to_yojson
      (VS.Evidence_available
         { request
         ; items =
             [ VS.Evidence_artifact_unreadable
                 { reference = "artifact:proof.txt"
                 ; reason = VS.Evidence_read_error "permission denied"
                 }
             ; VS.Evidence_invalid_reference
             ]
         })
  in
  match Yojson.Safe.Util.member "items" json |> Yojson.Safe.Util.to_list with
  | [ readable; invalid ] ->
    Alcotest.(check (list (pair string string)))
      "read error payload is structured"
      [ "code", "read_error"; "detail", "permission denied" ]
      (Yojson.Safe.Util.member "reason" readable
       |> Yojson.Safe.Util.to_assoc
       |> List.map (fun (key, value) -> key, Yojson.Safe.Util.to_string value));
    Alcotest.(check bool)
      "invalid references persist no payload"
      false
      (Yojson.Safe.Util.to_assoc invalid |> List.mem_assoc "reference");
    Alcotest.(check string)
      "invalid reference remains typed"
      "invalid_reference"
      (Yojson.Safe.Util.member "reason" invalid
       |> Yojson.Safe.Util.member "code"
       |> Yojson.Safe.Util.to_string)
  | _ -> Alcotest.fail "expected one readable failure and one invalid reference"

let test_invalid_reference_snapshot_rejects_hidden_payload () =
  with_eio_temp_dir (fun base_path ->
    let request_id = "vrf-invalid-reference-hidden-payload" in
    let output =
      `Assoc
        [ ( "submitted_evidence"
          , `List
              [ `Assoc
                  [ "kind", `String "artifact_unreadable"
                  ; "reason", `Assoc [ "code", `String "invalid_reference" ]
                  ; "raw_reference", `String "/private/producer/secret.txt"
                  ]
              ] )
        ]
    in
    (match
       V.create_request
         ~base_path
         ~request_id
         ~task_id:"task-001"
         ~output
         ~criteria:[]
         ~worker:"omega"
         ()
     with
     | Ok _ -> ()
     | Error detail -> Alcotest.fail detail);
    match
      VS.inspect_submitted_evidence_for_authority
        ~base_path
        ~request_id
        ~task_id:"task-001"
        ~task_worker:"omega"
        ~authority:(Masc_domain.Human_operator { operator_id = "operator-test" })
    with
    | VS.Evidence_unavailable { reason = VS.Evidence_snapshot_invalid detail; _ } ->
      Alcotest.(check bool)
        "hidden invalid-reference payload is rejected"
        true
        (String_util.contains_substring detail "payload-free")
    | _ -> Alcotest.fail "hidden invalid-reference payload was accepted")

let test_system_llm_rejection_is_durably_delivered_to_producer_keeper () =
  with_eio_temp_dir (fun base_path ->
    let config = W.default_config base_path in
    (* RFC-0393: the producer is recorded under its keeper_name, and
       keeper-ness is decided by the persisted meta at that name. *)
    let keeper_name = "persisted-canonical-producer" in
    let producer = keeper_name in
    let meta_json =
      `Assoc
        [ "name", `String keeper_name
        ; "trace_id", `String "trace-persisted-producer"
        ]
    in
    let meta =
      match Masc_test_deps.meta_of_json_fixture meta_json with
      | Ok meta -> meta
      | Error detail -> Alcotest.fail detail
    in
    (match
       Masc.Keeper_fs.save_json_atomic
         (Masc.Keeper_types_profile.keeper_meta_path config keeper_name)
         (Masc.Keeper_meta_json.meta_to_json meta)
     with
     | Ok () -> ()
     | Error detail -> Alcotest.fail detail);
    let delivery =
      Masc.Completion_authority_wakeup.wake_rejected_producer
        ~config
        ~producer
        ~task_id:"task-rejected"
        ~verification_id:"vrf-rejected"
        ~reason:"evidence did not demonstrate the required invariant"
        ~authority:(Masc_domain.System_llm_agent { agent_run_id = "system-agent-test" })
    in
    (match delivery with
     | Masc.Completion_authority_wakeup.Durable_deferred
         { keeper_name = actual_keeper
         ; wakeup = Masc.Keeper_registry.Deferred_unregistered
         }
       ->
       Alcotest.(check string)
         "rejection routes to the canonical producer Keeper"
         keeper_name
         actual_keeper
     | Masc.Completion_authority_wakeup.Durable_deferred _ ->
       Alcotest.fail "unregistered producer Keeper wake should be deferred"
     | Masc.Completion_authority_wakeup.Signaled _ ->
       Alcotest.fail "unregistered producer Keeper cannot be signaled"
     | Masc.Completion_authority_wakeup.Durable_wake_failed { detail; _ }
     | Masc.Completion_authority_wakeup.Durable_queue_failed { detail; _ } ->
       Alcotest.fail detail
     | Masc.Completion_authority_wakeup.Producer_identity_lookup_failed { detail; _ } ->
       Alcotest.fail detail
     | Masc.Completion_authority_wakeup.Unroutable_producer { producer; _ } ->
       Alcotest.failf "producer was unexpectedly unroutable: %s" producer);
    match Keeper_event_queue_persistence.load_result ~base_path ~keeper_name with
    | Error detail -> Alcotest.fail detail
    | Ok queue ->
      (match Keeper_event_queue.to_list queue with
       | [ { payload = Completion_authority_rejected rejection; _ } ] ->
         Alcotest.(check string)
           "durable rejection preserves task identity"
           "task-rejected"
           rejection.car_task_id;
         Alcotest.(check string)
           "durable rejection preserves verification identity"
           "vrf-rejected"
           rejection.car_verification_id;
         Alcotest.(check string)
           "durable rejection preserves reason"
           "evidence did not demonstrate the required invariant"
           rejection.car_reason;
         Alcotest.(check string)
           "durable rejection preserves system authority actor"
           "system-agent-test"
           (Masc_domain.completion_authority_actor rejection.car_authority)
       | _ -> Alcotest.fail "system rejection was not durably queued")
  )

let test_system_llm_rejection_prefers_registered_producer_binding () =
  with_eio_temp_dir (fun base_path ->
    let config = W.default_config base_path in
    let keeper_name = "registered-producer" in
    let producer = keeper_name in
    let meta_json =
      `Assoc
        [ "name", `String keeper_name
        ; "trace_id", `String "trace-registered-producer"
        ]
    in
    let meta =
      match Masc_test_deps.meta_of_json_fixture meta_json with
      | Ok meta -> meta
      | Error detail -> Alcotest.fail detail
    in
    Masc.Keeper_registry.For_testing.clear ();
    Fun.protect
      ~finally:Masc.Keeper_registry.For_testing.clear
      (fun () ->
         ignore
           (Masc.Keeper_registry.For_testing.register
              ~base_path
              keeper_name
              meta);
         let delivery =
           Masc.Completion_authority_wakeup.wake_rejected_producer
             ~config
             ~producer
             ~task_id:"task-registered-producer"
             ~verification_id:"vrf-registered-producer"
             ~reason:"registered binding must own the rejection queue"
             ~authority:
               (Masc_domain.System_llm_agent
                  { agent_run_id = "system-agent-registered" })
         in
         (match delivery with
          | Masc.Completion_authority_wakeup.Signaled { keeper_name = actual }
          | Masc.Completion_authority_wakeup.Durable_deferred
              { keeper_name = actual; _ } ->
            Alcotest.(check string)
              "registered agent binding selects the registry keeper"
              keeper_name
              actual
          | Masc.Completion_authority_wakeup.Durable_wake_failed { detail; _ }
          | Masc.Completion_authority_wakeup.Durable_queue_failed { detail; _ } ->
            Alcotest.fail detail
          | Masc.Completion_authority_wakeup.Producer_identity_lookup_failed { detail; _ } ->
            Alcotest.fail detail
          | Masc.Completion_authority_wakeup.Unroutable_producer { producer; _ } ->
            Alcotest.failf "registered producer was unexpectedly unroutable: %s" producer);
         match
           Keeper_event_queue_persistence.load_result
             ~base_path
             ~keeper_name
         with
         | Error detail -> Alcotest.fail detail
         | Ok queue ->
           (match Keeper_event_queue.to_list queue with
            | [ { payload = Completion_authority_rejected rejection; _ } ] ->
              Alcotest.(check string)
                "registered queue preserves task identity"
                "task-registered-producer"
                rejection.car_task_id
            | _ -> Alcotest.fail "registered producer rejection was not queued");
         let discovery =
           Keeper_event_queue_persistence.discover_keeper_names_with_durable_state
             ~base_path
         in
         (match discovery.read_error with
          | Some detail -> Alcotest.fail detail
          | None ->
            Alcotest.(check (list string))
              "durable rejection has only the registered Keeper snapshot"
              [ keeper_name ]
              discovery.keeper_names))
  )

let test_system_llm_rejection_does_not_derive_unregistered_keeper () =
  with_eio_temp_dir (fun base_path ->
    let config = W.default_config base_path in
    let producer = "keeper-omega-agent-agent" in
    Masc.Keeper_registry.For_testing.clear ();
    Fun.protect
      ~finally:Masc.Keeper_registry.For_testing.clear
      (fun () ->
         match
           Masc.Completion_authority_wakeup.wake_rejected_producer
             ~config
             ~producer
             ~task_id:"task-unregistered-producer"
             ~verification_id:"vrf-unregistered-producer"
             ~reason:"unregistered producer must remain unroutable"
             ~authority:
               (Masc_domain.System_llm_agent
                  { agent_run_id = "system-agent-unregistered" })
         with
         | Masc.Completion_authority_wakeup.Unroutable_producer
             { producer = actual; task_id } ->
           Alcotest.(check string) "unroutable producer identity" producer actual;
           Alcotest.(check string)
             "unroutable task identity"
             "task-unregistered-producer"
             task_id
         | Masc.Completion_authority_wakeup.Producer_identity_lookup_failed { detail; _ } ->
           Alcotest.fail detail
         | Masc.Completion_authority_wakeup.Signaled _
         | Masc.Completion_authority_wakeup.Durable_deferred _
         | Masc.Completion_authority_wakeup.Durable_wake_failed _
         | Masc.Completion_authority_wakeup.Durable_queue_failed _ ->
           Alcotest.fail "unregistered producer must not derive or enqueue a Keeper name")
  )

let test_system_llm_agent_commits_without_a_keeper_verifier () =
  with_eio_temp_dir_and_clock (fun ~clock base_path ->
    Masc.Workspace_metric_hooks.install ();
    let prompt_dir =
      match Sys.getenv_opt "DUNE_SOURCEROOT" with
      | Some root -> Filename.concat root "config/prompts"
      | None -> Filename.concat (Sys.getcwd ()) "config/prompts"
    in
    Prompt_registry.set_markdown_dir prompt_dir;
    Masc.Prompt_defaults.init ();
    let previous_runtime = Atomic.get Workspace_hooks.get_default_runtime_id_fn in
    let previous_lane_slots =
      Atomic.get Workspace_hooks.get_verifier_exact_lane_slot_ids_fn
    in
    let previous_reviewer =
      Atomic.get Masc.Task.Anti_rationalization.run_llm_reviewer_fn
    in
    let previous_notification =
      Atomic.get Workspace_hooks.verification_notify_verdict_fn
    in
    let previous_submitted = Atomic.get Workspace_hooks.verification_submitted_fn in
    let previous_change_observer =
      Atomic.get Masc.Verification_run_registry.change_observer_fn
    in
    Fun.protect
      ~finally:(fun () ->
        Atomic.set Workspace_hooks.get_default_runtime_id_fn previous_runtime;
        Atomic.set
          Workspace_hooks.get_verifier_exact_lane_slot_ids_fn
          previous_lane_slots;
        Atomic.set Masc.Task.Anti_rationalization.run_llm_reviewer_fn previous_reviewer;
        Atomic.set Workspace_hooks.verification_notify_verdict_fn previous_notification;
        Atomic.set Workspace_hooks.verification_submitted_fn previous_submitted;
        Atomic.set
          Masc.Verification_run_registry.change_observer_fn
          previous_change_observer)
      (fun () ->
        Atomic.set Workspace_hooks.get_default_runtime_id_fn
          (fun () -> "test-system-evaluator");
        (* RFC-0361 D7(a): the completion-authority review resolves only the
           verifier_exact lane; the default runtime hook above no longer
           reaches it. *)
        Atomic.set
          Workspace_hooks.get_verifier_exact_lane_slot_ids_fn
          (fun () -> Ok [ "test-system-evaluator" ]);
        Eio.Switch.run (fun sw ->
          let reviewer_called, resolve_reviewer_called = Eio.Promise.create () in
          let verdict_committed, resolve_verdict_committed = Eio.Promise.create () in
          let run_completed, resolve_run_completed = Eio.Promise.create () in
          let committed_verification_id = ref None in
          (* The verdict notification fires inside Workspace.commit_verdict_r,
             which the authority evaluates as an argument to its own [complete]
             -- so mark_completed necessarily runs after it. That order is not
             incidental: a failed commit becomes the Commit_failed outcome, so
             the outcome cannot be known before the commit is attempted. Waiting
             on the notification and then reading the registry therefore always
             observes Running. Wait for the registry's own change instead. *)
          Atomic.set Masc.Verification_run_registry.change_observer_fn (fun () ->
            previous_change_observer ();
            match !committed_verification_id with
            | None -> ()
            | Some verification_id ->
              (match
                 Masc.Verification_run_registry.get
                   (Masc.Verification_run_registry.global ())
                   ~verification_id
               with
               | Some { status = Masc.Verification_run_registry.Completed _; _ }
                 when not (Eio.Promise.is_resolved run_completed) ->
                 Eio.Promise.resolve resolve_run_completed ()
               | _ -> ()));
          Atomic.set Masc.Task.Anti_rationalization.run_llm_reviewer_fn
            (fun ~base_path:_ ?sw:_ ~evaluator_runtime:_ ~prompt:_ ~report_tool_schema:_ ~lookup:_ ~on_tool_result ~on_runtime_attempt_error:_ () ->
               on_tool_result
                 ~input:(`Assoc [ "path", `String "evidence.md" ])
                 (Tool_result.ok
                    ~tool_name:"verification_read_file"
                    ~start_time:0.0
                    "verified evidence");
               Eio.Promise.resolve resolve_reviewer_called ();
               Ok (Some (Masc.Task.Anti_rationalization.Approve "")));
          Atomic.set Workspace_hooks.verification_notify_verdict_fn
            (fun ~task_id ~authority ~verification_id ~decision ->
               committed_verification_id := Some verification_id;
               previous_notification
                 ~task_id
                 ~authority
                 ~verification_id
               ~decision;
               Eio.Promise.resolve resolve_verdict_committed ());
          let contract : Masc_domain.task_contract =
            { strict = true
            ; completion_contract = [ "system evaluator approves the submitted evidence" ]
            ; required_evidence = [ "system review evidence" ]
            ; inspect_gate_evidence = []
            ; verify_gate_evidence = []
            }
          in
          let config = W.default_config base_path in
          ignore (W.init config ~agent_name:(Some "system-test-worker"));
          ensure_keeper_meta config "system-test-worker";
          ensure_producer_playground config "system-test-worker";
          ignore
            (W.add_task
               config
               ~contract
               ~title:"system authority test"
               ~priority:1
               ~description:"the system LLM must review this evidence");
          (match
             W.claim_task_r config ~agent_name:"system-test-worker" ~task_id:"task-001" ()
           with
           | Ok _ -> ()
           | Error error -> Alcotest.fail (Masc_domain.masc_error_to_string error));
          CA.start ~sw ~clock ~config;
          (match
             W.transition_task_r
               config
               ~agent_name:"system-test-worker"
               ~task_id:"task-001"
               ~action:Masc_domain.Submit_for_verification
               ~notes:"note:system-review-evidence"
               ()
           with
           | Ok _ -> ()
           | Error error -> Alcotest.fail (Masc_domain.masc_error_to_string error));
          Eio.Time.with_timeout_exn clock 5.0 (fun () ->
            Eio.Promise.await reviewer_called;
            Eio.Promise.await verdict_committed;
            Eio.Promise.await run_completed);
          let verification_id =
            match !committed_verification_id with
            | Some verification_id -> verification_id
            | None -> Alcotest.fail "completion notification omitted verification id"
          in
          (match
             Masc.Verification_run_registry.get
               (Masc.Verification_run_registry.global ())
               ~verification_id
           with
           | Some
               { status = Masc.Verification_run_registry.Completed { tools = [ tool ]; _ }
               ; _
               } ->
             Alcotest.(check string)
               "lookup evidence tool"
               "verification_read_file"
               tool.tool_name
           | Some
               { status = Masc.Verification_run_registry.Completed { tools; _ }
               ; _
               } ->
             Alcotest.failf "expected one lookup observation, got %d" (List.length tools)
           | Some { status = Masc.Verification_run_registry.Running; _ } ->
             Alcotest.fail "verification run stayed running after verdict commit"
           | None -> Alcotest.fail "verification run record was not persisted");
          match W.get_tasks_raw config with
          | [ { task_status = Masc_domain.Done _; _ } ] -> ()
          | [ task ] ->
            Alcotest.failf
              "system authority did not complete task: %s"
              (Masc_domain.task_status_to_string task.task_status)
          | tasks -> Alcotest.failf "expected one task, got %d" (List.length tasks)))
  )

let test_system_llm_agent_defers_invalid_contract_without_rejecting_task () =
  with_eio_temp_dir_and_clock (fun ~clock base_path ->
    Masc.Workspace_metric_hooks.install ();
    let previous_submitted = Atomic.get Workspace_hooks.verification_submitted_fn in
    Fun.protect
      ~finally:(fun () ->
        Atomic.set Workspace_hooks.verification_submitted_fn previous_submitted)
      (fun () ->
        Eio.Switch.run (fun sw ->
          let config = W.default_config base_path in
          ignore (W.init config ~agent_name:(Some "contract-retry-worker"));
          ignore
            (W.add_task
               config
               ~title:"invalid completion contract"
               ~priority:1
               ~description:"the producer must be able to resubmit");
          let original_started_at = "2026-08-04T00:00:00Z" in
          let verification_id = "vrf-missing-contract" in
          let backlog = W.read_backlog config in
          let tasks =
            List.map
              (fun (task : Masc_domain.task) ->
                 { task with
                   task_status =
                     Masc_domain.AwaitingVerification
                       { assignee = "contract-retry-worker"
                       ; started_at = original_started_at
                       ; submitted_at = "2026-08-04T00:01:00Z"
                       ; intent = Complete_task
                       ; verification_id
                       }
                 })
              backlog.tasks
          in
          W.write_backlog config { backlog with tasks };
          let task =
            match tasks with
            | [ task ] -> task
            | tasks -> Alcotest.failf "expected one task, got %d" (List.length tasks)
          in
          CA.start ~sw ~clock ~config;
          let submitted = Atomic.get Workspace_hooks.verification_submitted_fn in
          submitted
            config
            ~task
            ~assignee:"contract-retry-worker"
            ~verification_id;
          let outcome =
            Eio.Time.with_timeout_exn clock 5.0 (fun () ->
              let rec await () =
                match
                  Masc.Verification_run_registry.get
                    (Masc.Verification_run_registry.global ())
                    ~verification_id
                with
                | Some
                    { status =
                        Masc.Verification_run_registry.Completed { outcome; _ }
                    ; _
                    } -> outcome
                | Some { status = Masc.Verification_run_registry.Running; _ }
                | None ->
                  Eio.Time.sleep clock 0.01;
                  await ()
              in
              await ())
          in
          (match outcome with
           | Masc.Verification_run_registry.Infrastructure_unavailable
               { stage = Masc.Verification_run_registry.Review_preparation; _ } -> ()
           | _ -> Alcotest.fail "missing request was not a preparation failure");
          match W.get_tasks_raw config with
          | [ { task_status =
                  Masc_domain.AwaitingVerification
                    { assignee; started_at; verification_id = observed_id; _ }
              ; _
              } ] ->
            Alcotest.(check string)
              "infrastructure failure keeps the submitted producer"
              "contract-retry-worker"
              assignee;
            Alcotest.(check string)
              "infrastructure failure preserves original claim time"
              original_started_at
              started_at;
            Alcotest.(check string)
              "infrastructure failure keeps the verification obligation"
              verification_id
              observed_id
          | [ task ] ->
            Alcotest.failf
              "infrastructure failure changed task authority: %s"
              (Masc_domain.task_status_to_string task.task_status)
          | tasks -> Alcotest.failf "expected one task, got %d" (List.length tasks)))
  )

let test_system_llm_agent_uses_persisted_request_contract_snapshot () =
  with_eio_temp_dir_and_clock (fun ~clock base_path ->
    Masc.Workspace_metric_hooks.install ();
    let prompt_dir =
      match Sys.getenv_opt "DUNE_SOURCEROOT" with
      | Some root -> Filename.concat root "config/prompts"
      | None -> Filename.concat (Sys.getcwd ()) "config/prompts"
    in
    Prompt_registry.set_markdown_dir prompt_dir;
    Masc.Prompt_defaults.init ();
    let previous_runtime = Atomic.get Workspace_hooks.get_default_runtime_id_fn in
    let previous_lane_slots =
      Atomic.get Workspace_hooks.get_verifier_exact_lane_slot_ids_fn
    in
    let previous_reviewer =
      Atomic.get Masc.Task.Anti_rationalization.run_llm_reviewer_fn
    in
    let previous_notification =
      Atomic.get Workspace_hooks.verification_notify_verdict_fn
    in
    let previous_submitted = Atomic.get Workspace_hooks.verification_submitted_fn in
    Fun.protect
      ~finally:(fun () ->
        Atomic.set Workspace_hooks.get_default_runtime_id_fn previous_runtime;
        Atomic.set
          Workspace_hooks.get_verifier_exact_lane_slot_ids_fn
          previous_lane_slots;
        Atomic.set Masc.Task.Anti_rationalization.run_llm_reviewer_fn previous_reviewer;
        Atomic.set Workspace_hooks.verification_notify_verdict_fn previous_notification;
        Atomic.set Workspace_hooks.verification_submitted_fn previous_submitted)
      (fun () ->
        Atomic.set Workspace_hooks.get_default_runtime_id_fn
          (fun () -> "test-system-evaluator");
        Atomic.set
          Workspace_hooks.get_verifier_exact_lane_slot_ids_fn
          (fun () -> Ok [ "test-system-evaluator" ]);
        Eio.Switch.run (fun sw ->
          let reviewer_called, resolve_reviewer_called = Eio.Promise.create () in
          let verdict_committed, resolve_verdict_committed = Eio.Promise.create () in
          let captured_prompt = ref None in
          Atomic.set Masc.Task.Anti_rationalization.run_llm_reviewer_fn
            (fun ~base_path:_ ?sw:_ ~evaluator_runtime:_ ~prompt ~report_tool_schema:_ ~lookup:_ ~on_tool_result:_ ~on_runtime_attempt_error:_ () ->
               captured_prompt := Some prompt;
               Eio.Promise.resolve resolve_reviewer_called ();
               Ok (Some (Masc.Task.Anti_rationalization.Approve "")));
          Atomic.set Workspace_hooks.verification_notify_verdict_fn
            (fun ~task_id ~authority ~verification_id ~decision ->
               previous_notification
                 ~task_id
                 ~authority
                 ~verification_id
                 ~decision;
               Eio.Promise.resolve resolve_verdict_committed ());
          let original_contract : Masc_domain.task_contract =
            { strict = true
            ; completion_contract = [ "persisted completion criterion" ]
            ; required_evidence = [ "persisted required artifact" ]
            ; inspect_gate_evidence = []
            ; verify_gate_evidence = [ "persisted gate artifact" ]
            }
          in
          let config = W.default_config base_path in
          ignore (W.init config ~agent_name:(Some "snapshot-test-worker"));
          ensure_keeper_meta config "snapshot-test-worker";
          ensure_producer_playground config "snapshot-test-worker";
          ignore
            (W.add_task
               config
               ~contract:original_contract
               ~title:"persisted contract snapshot"
               ~priority:1
               ~description:"the verifier must use the submit-time contract");
          (match
             W.claim_task_r config ~agent_name:"snapshot-test-worker" ~task_id:"task-001" ()
           with
           | Ok _ -> ()
           | Error error -> Alcotest.fail (Masc_domain.masc_error_to_string error));
          (match
             W.transition_task_r
               config
               ~agent_name:"snapshot-test-worker"
               ~task_id:"task-001"
               ~action:Masc_domain.Submit_for_verification
               ~notes:"note:snapshot-evidence"
               ()
           with
           | Ok _ -> ()
           | Error error -> Alcotest.fail (Masc_domain.masc_error_to_string error));
          let mutated_contract =
            { original_contract with
              completion_contract = [ "mutated live completion criterion" ]
            ; required_evidence = [ "mutated live required artifact" ]
            ; verify_gate_evidence = [ "mutated live gate artifact" ]
            }
          in
          let backlog = W.read_backlog config in
          let tasks =
            List.map
              (fun (task : Masc_domain.task) ->
                 if String.equal task.id "task-001"
                 then { task with contract = Some mutated_contract }
                 else task)
              backlog.tasks
          in
          W.write_backlog config { backlog with tasks };
          CA.start ~sw ~clock ~config;
          Eio.Time.with_timeout_exn clock 5.0 (fun () ->
            Eio.Promise.await reviewer_called;
            Eio.Promise.await verdict_committed);
          (match !captured_prompt with
           | None -> Alcotest.fail "system LLM reviewer did not receive a prompt"
           | Some prompt ->
             Alcotest.(check bool)
               "prompt uses persisted completion criterion"
               true
               (String_util.contains_substring prompt "persisted completion criterion");
             Alcotest.(check bool)
               "prompt does not use mutated live completion criterion"
               false
               (String_util.contains_substring prompt "mutated live completion criterion");
             Alcotest.(check bool)
               "prompt uses persisted required artifact"
               true
               (String_util.contains_substring prompt "persisted required artifact");
             Alcotest.(check bool)
               "prompt does not use mutated live required artifact"
               false
               (String_util.contains_substring prompt "mutated live required artifact"));
          match W.get_tasks_raw config with
          | [ { task_status = Masc_domain.Done _; _ } ] -> ()
          | [ task ] ->
            Alcotest.failf
              "snapshot review did not complete task: %s"
              (Masc_domain.task_status_to_string task.task_status)
          | tasks -> Alcotest.failf "expected one task, got %d" (List.length tasks)))
  )

let test_rejected_verdict_audit_preserves_reason () =
  with_eio_temp_dir (fun base_path ->
    let config = W.default_config base_path in
    ignore (W.init config ~agent_name:(Some "audit-producer"));
    ignore
      (W.add_task
         config
         ~title:"preserve rejected verdict fact"
         ~priority:1
         ~description:"the durable audit must retain the system decision");
    let backlog = W.read_backlog config in
    let tasks =
      List.map
        (fun (task : Masc_domain.task) ->
           if String.equal task.id "task-001"
           then
             { task with
               task_status =
                 Masc_domain.AwaitingVerification
                   { assignee = "audit-producer"
                   ; started_at = "2026-07-27T23:59:00Z"
                   ; submitted_at = Masc_domain.now_iso ()
                   ; intent = Complete_task
                   ; verification_id = "vrf-audit-rejected"
                   }
             }
           else task)
        backlog.tasks
    in
    W.write_backlog config { backlog with tasks };
    (match
       W.commit_verdict_r
         config
         ~authority:
           (Masc_domain.System_llm_agent { agent_run_id = "system-audit-agent" })
         ~verdict:
           (Masc_domain.Verdict_rejected
              { reason = "required deployment evidence is missing" })
         ~task_id:"task-001"
         ~verification_id:"vrf-audit-rejected"
         ()
     with
     | Error error -> Alcotest.fail (Masc_domain.masc_error_to_string error)
     | Ok _ -> ());
    (match W.get_tasks_raw config with
     | [ { handoff_context = Some handoff; _ } ] ->
       Alcotest.(check string)
         "task keeps the exact rejection reason"
         "required deployment evidence is missing"
         handoff.summary;
       Alcotest.(check (list string))
         "task keeps the rejected verification identity"
         [ "vrf-audit-rejected" ]
         handoff.evidence_refs;
       Alcotest.(check (option string))
         "task keeps the rejecting authority"
         (Some "system-audit-agent")
         handoff.updated_by
     | [ _ ] -> Alcotest.fail "rejected task lost its durable continuation"
     | tasks -> Alcotest.failf "expected one task, got %d" (List.length tasks));
    let open Unix in
    let tm = gmtime (gettimeofday ()) in
    let month = Printf.sprintf "%04d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) in
    let day = Printf.sprintf "%02d.jsonl" tm.tm_mday in
    let events_dir = Filename.concat (CU.masc_dir_from_base_path ~base_path) "events" in
    let event_path = Filename.concat (Filename.concat events_dir month) day in
    let event =
      Fs_compat.load_jsonl event_path
      |> List.find_opt (fun json ->
           match json with
           | `Assoc fields ->
             (match List.assoc_opt "type" fields with
              | Some (`String value) -> String.equal value "task_completion_verdict"
              | _ -> false)
           | _ -> false)
    in
    match event with
    | None -> Alcotest.fail "rejected verdict audit event was not persisted"
    | Some json ->
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "audit keeps the typed rejected verdict"
        "rejected"
        (json |> member "verdict" |> to_string);
      Alcotest.(check string)
        "audit keeps the exact rejection reason"
        "required deployment evidence is missing"
        (json |> member "reason" |> to_string);
      Alcotest.(check string)
        "audit keeps the system authority boundary"
        "system_llm_agent"
        (json |> member "authority_kind" |> to_string))

(* The judging runtime is the axis a verdict history can be grouped on:
   since RFC-0361 D7(b) [authority_actor] is the fixed [verifier_exact]
   identity, so 74 verdicts carry one actor. The runtime was already computed
   and carried inside the review notes blob, but every structured projection
   dropped it. *)
let test_verdict_audit_names_the_judging_runtime () =
  with_eio_temp_dir (fun base_path ->
    let config = W.default_config base_path in
    ignore (W.init config ~agent_name:(Some "runtime-producer"));
    ignore
      (W.add_task
         config
         ~title:"name the judging runtime"
         ~priority:1
         ~description:"the durable audit must say which runtime judged");
    let backlog = W.read_backlog config in
    let tasks =
      List.map
        (fun (task : Masc_domain.task) ->
           if String.equal task.id "task-001"
           then
             { task with
               task_status =
                 Masc_domain.AwaitingVerification
                   { assignee = "runtime-producer"
                   ; started_at = "2026-08-05T00:00:00Z"
                   ; submitted_at = Masc_domain.now_iso ()
                   ; intent = Complete_task
                   ; verification_id = "vrf-runtime-named"
                   }
             }
           else task)
        backlog.tasks
    in
    W.write_backlog config { backlog with tasks };
    (match
       W.commit_verdict_r
         config
         ~authority:
           (Masc_domain.System_llm_agent { agent_run_id = "system-runtime-agent" })
         ~verdict:Masc_domain.Verdict_approved
         ~task_id:"task-001"
         ~verification_id:"vrf-runtime-named"
         ~evaluator_runtime:"judge-runtime-model"
         ()
     with
     | Error error -> Alcotest.fail (Masc_domain.masc_error_to_string error)
     | Ok _ -> ());
    let open Unix in
    let tm = gmtime (gettimeofday ()) in
    let month = Printf.sprintf "%04d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) in
    let day = Printf.sprintf "%02d.jsonl" tm.tm_mday in
    let events_dir = Filename.concat (CU.masc_dir_from_base_path ~base_path) "events" in
    let event_path = Filename.concat (Filename.concat events_dir month) day in
    let event =
      Fs_compat.load_jsonl event_path
      |> List.find_opt (fun json ->
        match json with
        | `Assoc fields ->
          (match List.assoc_opt "type" fields with
           | Some (`String value) -> String.equal value "task_completion_verdict"
           | _ -> false)
        | _ -> false)
    in
    match event with
    | None -> Alcotest.fail "verdict audit event was not persisted"
    | Some json ->
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "audit names the runtime that judged"
        "judge-runtime-model"
        (json |> member "evaluator_runtime" |> to_string);
      Alcotest.(check string)
        "audit still carries the run-scoped actor"
        "system-runtime-agent"
        (json |> member "authority_actor" |> to_string))
;;

(* RFC-0361 D7(b): the completion authority no longer mints a random actor
   per judgement. N judgements registered with the agent's authority identity
   must all record the same fixed [authority_actor] — the [verifier_exact]
   lane id — so verdicts aggregate by actor; run identity stays with
   [verification_id]. *)
let test_judgements_share_fixed_authority_actor () =
  let module R = Masc.Verification_run_registry in
  let registry = R.create () in
  let authority =
    Masc_domain.System_llm_agent
      { agent_run_id = CA.For_testing.authority_actor }
  in
  for i = 1 to 3 do
    let verification_id = Printf.sprintf "vrf-fixed-actor-%d" i in
    R.register_running
      registry
      ~verification_id
      ~task_id:(Printf.sprintf "task-fixed-actor-%d" i)
      ~producer:"keeper-producer-agent"
      ~authority_kind:(Masc_domain.completion_authority_kind authority)
      ~authority_actor:(Masc_domain.completion_authority_actor authority)
      ~started_at:(100.0 +. Float.of_int i);
    R.mark_completed
      registry
      ~verification_id
      ~outcome:(R.Approved { reason = "" })
      ~tools:[]
      ~elapsed_s:1.0
      ()
  done;
  let actors =
    R.list_runs registry
    |> List.map (fun (run : R.run) -> run.authority_actor)
    |> List.sort_uniq String.compare
  in
  Alcotest.(check int)
    "all three judgements were recorded"
    3
    (List.length (R.list_runs registry));
  Alcotest.(check (list string))
    "every judgement records the same fixed authority actor"
    [ "verifier_exact" ]
    actors
;;

let test_raw_workspace_submission_notifies_once () =
  with_eio_temp_dir (fun base_path ->
    Masc.Workspace_metric_hooks.install ();
    let previous_notification =
      Atomic.get Workspace_hooks.verification_notify_submit_fn
    in
    let notifications = ref [] in
    Fun.protect
      ~finally:(fun () ->
        Atomic.set Workspace_hooks.verification_notify_submit_fn previous_notification)
      (fun () ->
        Atomic.set Workspace_hooks.verification_notify_submit_fn
          (fun _config ~task ~assignee ~verification_id ~claim ->
             notifications :=
               (task.id, assignee, verification_id, claim) :: !notifications);
        let config = W.default_config base_path in
        ignore (W.init config ~agent_name:(Some "raw-workspace-worker"));
        ignore
          (W.add_task
             config
             ~title:"raw Workspace submission"
             ~priority:1
             ~description:"the raw Workspace boundary must publish one submit notification");
        (match
           W.claim_task_r
             config
             ~agent_name:"raw-workspace-worker"
             ~task_id:"task-001"
             ()
         with
         | Ok _ -> ()
         | Error error -> Alcotest.fail (Masc_domain.masc_error_to_string error));
        (match
           W.transition_task_r
             config
             ~agent_name:"raw-workspace-worker"
             ~task_id:"task-001"
             ~action:Masc_domain.Submit_for_verification
             ~notes:"note:raw-workspace-evidence"
             ()
         with
         | Error error -> Alcotest.fail (Masc_domain.masc_error_to_string error)
         | Ok _ -> ());
        Alcotest.(check int)
          "raw Workspace transition publishes exactly one submit notification"
          1
          (List.length !notifications);
        match !notifications with
        | [ task_id, assignee, verification_id, _claim ] ->
          Alcotest.(check string) "notification task" "task-001" task_id;
          Alcotest.(check string)
            "notification producer"
            "raw-workspace-worker"
            assignee;
          Alcotest.(check bool)
            "notification has the persisted verification id"
            true
            (String.length verification_id > 0)
        | _ -> Alcotest.fail "expected one raw Workspace submit notification"))

(* --- Storage tests --- *)

let test_create_and_load () =
  with_temp_dir (fun base_path ->
    match V.create_request ~base_path ~task_id:"task-1"
        ~output:(`String "result") ~criteria:[ "result" ]
        ~worker:"claude" () with
    | Error e -> Alcotest.fail e
    | Ok req ->
        Alcotest.(check bool) "persisted under .masc/verifications" true
          (Sys.file_exists
             (Filename.concat (active_verifications_dir base_path)
                (req.id ^ ".json")));
        match V.load_request base_path req.id with
        | Error e -> Alcotest.fail e
        | Ok loaded ->
            Alcotest.(check string) "id matches" req.id loaded.id;
            Alcotest.(check string) "task_id" "task-1" loaded.task_id;
            Alcotest.(check string) "worker" "claude" loaded.worker)

let test_create_rejects_blank_criterion_before_write () =
  with_temp_dir (fun base_path ->
    match
      V.create_request
        ~base_path
        ~task_id:"task-blank-criterion"
        ~output:`Null
        ~criteria:[ " " ]
        ~worker:"worker"
        ()
    with
    | Ok _ -> Alcotest.fail "blank completion criterion reached persistence"
    | Error _ ->
      Alcotest.(check int)
        "no request was written"
        0
        (List.length (list_requests_exn base_path)))

(* RFC-0221 §3.1: [delete_request] removes the record (compensation) and is
   idempotent — deleting a missing record is success, so a caller can compensate
   without first checking existence. *)
let test_delete_request () =
  with_temp_dir (fun base_path ->
    match V.create_request ~base_path ~task_id:"task-1"
        ~output:(`String "result") ~criteria:[ "result" ]
        ~worker:"claude" () with
    | Error e -> Alcotest.fail e
    | Ok req ->
        let path =
          Filename.concat (active_verifications_dir base_path) (req.id ^ ".json")
        in
        Alcotest.(check bool) "record present before delete" true (Sys.file_exists path);
        (match V.delete_request base_path req.id with
         | Error e -> Alcotest.fail e
         | Ok () -> ());
        Alcotest.(check bool) "record gone after delete" false (Sys.file_exists path);
        (match V.delete_request base_path req.id with
         | Error e -> Alcotest.fail ("second delete must be idempotent Ok: " ^ e)
         | Ok () -> ());
        match V.load_request base_path req.id with
        | Ok _ -> Alcotest.fail "load after delete should report not-found"
        | Error _ -> ())

let test_list_requests () =
  with_temp_dir (fun base_path ->
    let _ = V.create_request ~base_path ~task_id:"t1"
        ~output:`Null ~criteria:[] ~worker:"a" () in
    let _ = V.create_request ~base_path ~task_id:"t2"
        ~output:`Null ~criteria:[] ~worker:"b" () in
    let reqs = list_requests_exn base_path in
    Alcotest.(check int) "two requests" 2 (List.length reqs))

let test_list_requests_missing_dir_stays_quiet () =
  with_temp_dir (fun base_path ->
    let before =
      persistence_counter (Read_drop_reason.to_wire Read_drop_reason.List_dir_error)
    in
    let reqs = list_requests_exn base_path in
    Alcotest.(check int) "no requests" 0 (List.length reqs);
    Alcotest.(check (float 0.1)) "missing dir does not increment metric"
      before
      (persistence_counter (Read_drop_reason.to_wire Read_drop_reason.List_dir_error)))

let test_verifications_dir_resolves_active_store () =
  with_temp_dir (fun base_path ->
    let active_dir = active_verifications_dir base_path in
    let resolved = VS.verifications_dir base_path in
    Alcotest.(check string) "resolved current store" active_dir resolved)

let test_request_path_uses_current_store () =
  with_temp_dir (fun base_path ->
    let req_id = "vrf-current" in
    Alcotest.(check string) "request path uses active store"
      (Filename.concat (active_verifications_dir base_path) (req_id ^ ".json"))
      (VS.request_path base_path req_id))

(* A file the schema cannot read is reported, not silently skipped, and it does
   not take the readable requests down with it.

   The earlier contract failed the whole scan here. That kept the failure loud
   but made it total: 171 records written by a producer removed on 2026-08-07
   were enough to answer /api/v1/dashboard/proof with 500 for five days, naming
   one path while saying nothing about the other 170 or the 122 that read fine. *)
let test_list_requests_isolates_bad_entry_with_metric () =
  with_temp_dir (fun base_path ->
    let _ = V.create_request ~base_path ~task_id:"t1"
        ~output:`Null ~criteria:[] ~worker:"a" () in
    let dir = active_verifications_dir base_path in
    Fs_compat.save_file (Filename.concat dir "broken.json") "{not-json";
    let before =
      persistence_counter (Read_drop_reason.to_wire Read_drop_reason.Entry_load_error)
    in
    (match V.list_requests base_path with
     | Error detail -> Alcotest.fail detail
     | Ok scan ->
       Alcotest.(check int)
         "the readable request survives its broken neighbour"
         1
         (List.length scan.V.readable);
       Alcotest.(check int)
         "the broken file is reported, not dropped"
         1
         (List.length scan.V.unreadable);
       (match scan.V.unreadable with
        | [ entry ] ->
          Alcotest.(check bool)
            "malformed path is named"
            true
            (Astring.String.is_infix ~affix:"broken.json" entry.V.unreadable_path);
          Alcotest.(check bool)
            "the parse detail is carried, not discarded"
            false
            (String.equal (String.trim entry.V.unreadable_detail) "")
        | entries ->
          Alcotest.failf "expected exactly one unreadable entry, got %d"
            (List.length entries)));
    Alcotest.(check (float 0.1)) "broken file increments metric" 1.0
      (persistence_counter (Read_drop_reason.to_wire Read_drop_reason.Entry_load_error)
       -. before))

(* The scan reports every unreadable file, not just the one it stopped at. The
   old contract could only ever name one, which is what made a 171-record
   problem look like a 1-record problem. *)
let test_list_requests_reports_every_unreadable_entry () =
  with_temp_dir (fun base_path ->
    let _ = V.create_request ~base_path ~task_id:"t1"
        ~output:`Null ~criteria:[] ~worker:"a" () in
    let dir = active_verifications_dir base_path in
    Fs_compat.save_file (Filename.concat dir "broken-a.json") "{not-json";
    Fs_compat.save_file (Filename.concat dir "broken-b.json") "{also-not-json";
    (* Well-formed JSON that the request schema still rejects — the shape that
       actually accumulated on disk, as opposed to a truncated write. *)
    Fs_compat.save_file (Filename.concat dir "broken-c.json")
      {|{"id":"x","task_id":"t","output":null,"criteria":[{"type":"custom","description":"d"}],"worker":"w","created_at":1.0}|};
    match V.list_requests base_path with
    | Error detail -> Alcotest.fail detail
    | Ok scan ->
      Alcotest.(check int)
        "readable request still returned" 1 (List.length scan.V.readable);
      Alcotest.(check int)
        "all three unreadable files reported" 3 (List.length scan.V.unreadable))

let test_list_requests_rereads_current_request_content () =
  with_temp_dir (fun base_path ->
    let request =
      match
        V.create_request
          ~base_path
          ~task_id:"t1"
          ~output:`Null
          ~criteria:[]
          ~worker:"a"
          ()
      with
      | Ok request -> request
      | Error detail -> Alcotest.fail detail
    in
    Alcotest.(check int)
      "initial request is readable"
      1
      (List.length (list_requests_exn base_path));
    Fs_compat.save_file (VS.request_path base_path request.id) "{not-json";
    (* The point of the case is that the second scan reads the file again
       instead of reusing the first result. It used to observe that through the
       scan failing; now the same re-read shows as the record moving out of
       [readable] and into [unreadable], which says where it went as well as
       that it moved. *)
    match V.list_requests base_path with
    | Error detail -> Alcotest.fail detail
    | Ok scan ->
      Alcotest.(check int)
        "changed malformed request reused an earlier list"
        0
        (List.length scan.V.readable);
      Alcotest.(check int)
        "the rewritten file is accounted for"
        1
        (List.length scan.V.unreadable))

let create_evidence_request ~base_path ~request_id ~artifact_path =
  let profile_path =
    Keeper_sandbox_config.keeper_toml_path
      ~base_path
      ~agent_name:"omega"
  in
  Fs_compat.mkdir_p (Filename.dirname profile_path);
  Fs_compat.save_file profile_path "[keeper]\nsandbox_profile = \"docker\"\n";
  let submitted_evidence =
    match
      Playground_paths.parse_playground_file_path
        ~base_path
        ~abs_path:artifact_path
    with
    | Some { keeper_name = "omega"; relative_path } ->
      [ "artifact:" ^ relative_path; "note:producer summary" ]
    | Some _ | None ->
      [ "artifact:../outside-worker-playground"; "note:producer summary" ]
  in
  let evidence_snapshot =
    VS.snapshot_submitted_evidence_json
      ~base_path
      ~worker:"omega"
      submitted_evidence
  in
  match
    V.create_request
      ~base_path
      ~request_id
      ~task_id:"task-001"
      ~output:
        (`Assoc
            [ "submitted_evidence", evidence_snapshot ])
      ~criteria:[ "inspect artifact" ]
      ~worker:"omega"
      ()
  with
  | Ok request -> request
  | Error detail -> Alcotest.fail detail

let write_keeper_profile ~base_path ~keeper_name ~sandbox_profile =
  let path =
    Keeper_sandbox_config.keeper_toml_path
      ~base_path
      ~agent_name:keeper_name
  in
  Fs_compat.mkdir_p (Filename.dirname path);
  Fs_compat.save_file
    path
    (Printf.sprintf
       "[keeper]\ninstructions = \"verification test producer\"\nsandbox_profile = %S\n"
       sandbox_profile)

let create_protocol_evidence_request ~base_path ~request_id ~evidence_refs =
  let config = W.default_config base_path in
  ignore (W.init config ~agent_name:None);
  ignore
    (W.add_task
       config
       ~title:"Produce typed verification evidence"
       ~priority:1
       ~description:"");
  let task =
    match (W.read_backlog config).tasks with
    | [ task ] -> task
    | tasks ->
      Alcotest.failf "expected one task, got %d" (List.length tasks)
  in
  (match
     VP.create_submit_request
       ~config
       ~task
       ~assignee:"omega"
       ~verification_id:request_id
       ~claim:(Masc_domain.Completion_evidence { evidence_refs })
   with
   | Ok () -> ()
   | Error detail -> Alcotest.fail detail);
  task

let inspect_evidence ?(task_id = "task-001")
    ?(task_worker = "omega") ~base_path ~request_id
    () =
  VS.inspect_submitted_evidence_for_authority
    ~base_path
    ~request_id
    ~task_id
    ~task_worker
    ~authority:(Masc_domain.Human_operator { operator_id = "operator-test" })

let test_submitted_evidence_inspection_is_authority_scoped_and_contained () =
  with_eio_temp_dir (fun base_path ->
    let artifact_dir =
      Filename.concat
        base_path
        ".masc/playground/docker/omega"
    in
    Fs_compat.mkdir_p artifact_dir;
    let artifact_path = Filename.concat artifact_dir "artifact-task-001.txt" in
    Fs_compat.save_file artifact_path "verified artifact\nsecond line";
    let request_id = "vrf-evidence-inspection" in
    ignore (create_evidence_request ~base_path ~request_id ~artifact_path);
    (match
       inspect_evidence
         ~base_path
         ~request_id
         ()
     with
     | VS.Evidence_available
         { items =
             VS.Evidence_artifact { content; truncated = false; _ }
             :: VS.Evidence_note "producer summary"
             :: []
         ; _
         } ->
       Alcotest.(check string)
         "completion authority reads producer artifact"
         "verified artifact\nsecond line"
         content
     | _ -> Alcotest.fail "expected completion-authority evidence projection");
    match
      VS.inspect_submitted_evidence_for_authority
        ~base_path
        ~request_id
        ~task_id:"task-001"
        ~task_worker:"omega"
        ~authority:(Masc_domain.Human_operator { operator_id = "" })
    with
    | VS.Evidence_unavailable _ -> ()
    | _ -> Alcotest.fail "empty completion-authority identity must expose no evidence")

let test_submit_snapshot_resolves_docker_relative_artifact_and_explicit_note () =
  with_eio_temp_dir (fun base_path ->
    write_keeper_profile
      ~base_path
      ~keeper_name:"omega"
      ~sandbox_profile:"docker";
    let artifact_dir =
      Filename.concat
        (Keeper_sandbox_config.host_root_abs_of_agent
           ~base_path
           ~agent_name:"omega")
        "artifacts"
    in
    Fs_compat.mkdir_p artifact_dir;
    Fs_compat.save_file
      (Filename.concat artifact_dir "proof.txt")
      "docker-relative-proof";
    let request_id = "vrf-docker-relative-snapshot" in
    let task =
      create_protocol_evidence_request
        ~base_path
        ~request_id
        ~evidence_refs:
          [ "artifact:artifacts/proof.txt"; "note:producer summary" ]
    in
    match
      inspect_evidence
        ~task_id:task.id
        ~base_path
        ~request_id
        ()
    with
    | VS.Evidence_available
        { items =
            VS.Evidence_artifact
              { reference = "artifact:artifacts/proof.txt"; content; _ }
            :: VS.Evidence_note "producer summary"
            :: []
        ; _
        } ->
      Alcotest.(check string)
        "snapshot persists the bounded artifact content"
        "docker-relative-proof"
        content
    | _ ->
      (match
         inspect_evidence
           ~task_id:task.id
           ~base_path
           ~request_id
           ()
       with
       | VS.Evidence_available
           { items =
               VS.Evidence_artifact_unreadable { reason; _ } :: _
           ; _
           } ->
         Alcotest.failf
           "Docker-relative artifact snapshot unreadable: %s"
           (VS.evidence_read_failure_code reason)
       | VS.Evidence_available { items; _ } ->
         Alcotest.failf
           "expected explicit artifact and note snapshot, got %d items"
           (List.length items)
       | VS.Evidence_unavailable { request_id; reason } ->
         Alcotest.failf
           "persisted evidence snapshot unavailable: %s"
           (VS.evidence_access_failure_to_string ~request_id reason)))

let test_submit_snapshot_survives_mutation_deletion_and_authority_cwd () =
  with_eio_temp_dir (fun base_path ->
    write_keeper_profile
      ~base_path
      ~keeper_name:"omega"
      ~sandbox_profile:"docker";
    let artifact_dir =
      Filename.concat
        (Keeper_sandbox_config.host_root_abs_of_agent
           ~base_path
           ~agent_name:"omega")
        "artifacts"
    in
    Fs_compat.mkdir_p artifact_dir;
    let artifact_path = Filename.concat artifact_dir "immutable.txt" in
    Fs_compat.save_file artifact_path "submit-time-content";
    let request_id = "vrf-immutable-snapshot" in
    let task =
      create_protocol_evidence_request
        ~base_path
        ~request_id
        ~evidence_refs:[ "artifact:artifacts/immutable.txt" ]
    in
    Fs_compat.save_file artifact_path "mutated-after-submit";
    Sys.remove artifact_path;
    let verifier_cwd = Filename.concat base_path "verifier-cwd" in
    Fs_compat.mkdir_p verifier_cwd;
    let original_cwd = Sys.getcwd () in
    Fun.protect
      ~finally:(fun () -> Sys.chdir original_cwd)
      (fun () ->
        Sys.chdir verifier_cwd;
        match
          inspect_evidence
            ~task_id:task.id
            ~base_path
            ~request_id
            ()
        with
        | VS.Evidence_available
            { items =
                VS.Evidence_artifact
                  { content = "submit-time-content"; _ }
                :: []
            ; _
            } ->
          ()
        | _ ->
          (match
             inspect_evidence
               ~task_id:task.id
               ~base_path
               ~request_id
               ()
           with
           | VS.Evidence_available
               { items =
                   VS.Evidence_artifact_unreadable { reason; _ } :: _
               ; _
               } ->
             Alcotest.failf
               "immutable artifact snapshot unreadable: %s"
               (VS.evidence_read_failure_code reason)
           | VS.Evidence_available { items; _ } ->
             Alcotest.failf
               "expected one immutable artifact snapshot, got %d items"
               (List.length items)
           | VS.Evidence_unavailable { request_id; reason } ->
             Alcotest.failf
               "persisted evidence snapshot unavailable: %s"
               (VS.evidence_access_failure_to_string ~request_id reason))))

let test_submit_snapshot_rejects_relative_traversal_and_symlink_escape () =
  with_eio_temp_dir (fun base_path ->
    write_keeper_profile
      ~base_path
      ~keeper_name:"omega"
      ~sandbox_profile:"docker";
    let producer_root =
      Keeper_sandbox_config.host_root_abs_of_agent
        ~base_path
        ~agent_name:"omega"
    in
    let artifact_dir = Filename.concat producer_root "artifacts" in
    Fs_compat.mkdir_p artifact_dir;
    let outside_path = Filename.concat base_path "outside-relative-secret.txt" in
    Fs_compat.save_file outside_path "outside";
    let symlink_path = Filename.concat artifact_dir "escape.txt" in
    Unix.symlink outside_path symlink_path;
    let request_id = "vrf-relative-boundary-snapshot" in
    let task =
      create_protocol_evidence_request
        ~base_path
        ~request_id
        ~evidence_refs:
          [ "artifact:../outside-relative-secret.txt"
          ; "artifact:artifacts/escape.txt"
          ]
    in
    Fun.protect
      ~finally:(fun () ->
        try Unix.unlink symlink_path with
        | Unix.Unix_error (Unix.ENOENT, _, _) -> ())
      (fun () ->
        match
          inspect_evidence
            ~task_id:task.id
            ~base_path
            ~request_id
            ()
        with
        | VS.Evidence_available
            { items =
                VS.Evidence_invalid_reference
                :: VS.Evidence_artifact_unreadable
                     { reason = VS.Evidence_symbolic_link; _ }
                :: []
            ; _
            } ->
          ()
        | _ ->
          Alcotest.fail
            "relative traversal and symlink escape must persist typed unreadable snapshots"))

let test_submit_snapshot_rejects_bare_and_absolute_references () =
  with_eio_temp_dir (fun base_path ->
    write_keeper_profile
      ~base_path
      ~keeper_name:"omega"
      ~sandbox_profile:"docker";
    let producer_root =
      Keeper_sandbox_config.host_root_abs_of_agent
        ~base_path
        ~agent_name:"omega"
    in
    Fs_compat.mkdir_p producer_root;
    let absolute_path = Filename.concat producer_root "absolute.txt" in
    Fs_compat.save_file absolute_path "must-not-be-read";
    let request_id = "vrf-explicit-reference-hard-cut" in
    let snapshot =
      VS.snapshot_submitted_evidence_json
        ~base_path
        ~worker:"omega"
        [ "artifacts/bare.txt"; absolute_path ]
    in
    let persisted_snapshot = Yojson.Safe.to_string snapshot in
    Alcotest.(check bool)
      "invalid references are absent from the persisted snapshot"
      false
      (String_util.contains_substring persisted_snapshot "artifacts/bare.txt"
       || String_util.contains_substring persisted_snapshot absolute_path);
    ignore
      (match
         V.create_request
           ~base_path
           ~request_id
           ~task_id:"task-001"
           ~output:(`Assoc [ "submitted_evidence", snapshot ])
           ~criteria:[ "inspect explicit refs" ]
           ~worker:"omega"
           ()
       with
       | Ok request -> request
       | Error detail -> Alcotest.fail detail);
    match
      inspect_evidence
        ~base_path
        ~request_id
        ()
    with
    | VS.Evidence_available
        { items =
            VS.Evidence_invalid_reference
            :: VS.Evidence_invalid_reference
            :: []
        ; _
        } ->
      ()
    | _ ->
      Alcotest.fail
        "bare and absolute references must remain typed invalid without file reads")

let test_submitted_evidence_inspection_rejects_cross_playground_path () =
  with_eio_temp_dir (fun base_path ->
    let other_dir =
      Filename.concat
        base_path
        ".masc/playground/docker/other"
    in
    Fs_compat.mkdir_p other_dir;
    let artifact_path = Filename.concat other_dir "secret.txt" in
    Fs_compat.save_file artifact_path "must not leak";
    let request_id = "vrf-cross-playground" in
    ignore (create_evidence_request ~base_path ~request_id ~artifact_path);
    match
      inspect_evidence
        ~base_path
        ~request_id
        ()
    with
    | VS.Evidence_available
        { items =
            VS.Evidence_invalid_reference
            :: _
        ; _
        } ->
      ()
    | _ -> Alcotest.fail "cross-playground artifact must remain unreadable")

let test_submitted_evidence_rejects_unknown_artifact_field () =
  with_eio_temp_dir (fun base_path ->
    let profile_path =
      Keeper_sandbox_config.keeper_toml_path
        ~base_path
        ~agent_name:"omega"
    in
    Fs_compat.mkdir_p (Filename.dirname profile_path);
    Fs_compat.save_file profile_path "[keeper]\nsandbox_profile = \"docker\"\n";
    let request_id = "vrf-unknown-artifact-field" in
    let content = "artifact body" in
    let snapshot =
      `List
        [ `Assoc
            [ "kind", `String "artifact"
            ; "reference", `String "artifact:artifacts/current.txt"
            ; "content", `String content
            ; "bytes", `Int (String.length content)
            ; "truncated", `Bool false
            ; "unexpected_field", `String "not part of the current contract"
            ]
        ; `Assoc [ "kind", `String "note"; "content", `String "producer summary" ]
        ]
    in
    (match
       V.create_request
         ~base_path
         ~request_id
         ~task_id:"task-001"
         ~output:(`Assoc [ "submitted_evidence", snapshot ])
         ~criteria:[ "inspect artifact" ]
         ~worker:"omega"
         ()
     with
     | Ok _ -> ()
     | Error detail -> Alcotest.fail detail);
    match inspect_evidence ~base_path ~request_id () with
    | VS.Evidence_unavailable { reason = VS.Evidence_snapshot_invalid detail; _ } ->
      Alcotest.(check bool)
        "unknown artifact field is named"
        true
        (Astring.String.is_infix ~affix:"unexpected_field" detail)
    | VS.Evidence_unavailable { reason; _ } ->
      Alcotest.failf
        "wrong rejection: %s"
        (VS.evidence_access_failure_to_string ~request_id reason)
    | VS.Evidence_available _ -> Alcotest.fail "unknown artifact field accepted")

let test_submitted_evidence_inspection_is_bounded_and_utf8_safe () =
  with_eio_temp_dir (fun base_path ->
    let artifact_dir =
      Filename.concat
        base_path
        ".masc/playground/docker/omega"
    in
    Fs_compat.mkdir_p artifact_dir;
    let artifact_path = Filename.concat artifact_dir "large-artifact.txt" in
    (* One byte short of the cap, so the multibyte character that follows
       straddles it and the scanner has to drop the partial codepoint.
       Derived from the cap so raising the cap does not require editing an
       assertion about UTF-8. *)
    let cap = VS.verification_evidence_max_bytes in
    let ascii_prefix = String.make (cap - 1) 'a' in
    let full_artifact = ascii_prefix ^ "한글" ^ String.make 250_000 'z' in
    Fs_compat.save_file artifact_path full_artifact;
    let request_id = "vrf-bounded-evidence" in
    ignore (create_evidence_request ~base_path ~request_id ~artifact_path);
    match
      inspect_evidence
        ~base_path
        ~request_id
        ()
    with
    | VS.Evidence_available
        { items =
            VS.Evidence_artifact
              { content; bytes; truncated = true; _ }
            :: _
        ; _
        } ->
      Alcotest.(check int) "full artifact byte count preserved"
        (String.length full_artifact) bytes;
      Alcotest.(check int) "truncation stops before the partial codepoint" (cap - 1)
        (String.length content);
      Alcotest.(check string) "incomplete UTF-8 codepoint removed"
        ascii_prefix content
    | _ -> Alcotest.fail "expected bounded UTF-8-safe artifact projection")

(* Filling the budget in submission order let one large-but-under-cap artifact
   spend it all and push every later item to a link, so which evidence the
   judge could read depended on the order the producer happened to list it in.
   The big artifact goes first here; the small ones must still arrive with
   their content. *)
let test_transport_projection_keeps_the_most_items_it_can () =
  with_eio_temp_dir (fun base_path ->
    let artifact_dir = Filename.concat base_path ".masc/playground/docker/omega" in
    Fs_compat.mkdir_p artifact_dir;
    write_keeper_profile
      ~base_path
      ~keeper_name:"omega"
      ~sandbox_profile:"docker";
    let cap = VS.verification_evidence_max_bytes in
    let sizes = [ "big", cap - 1_000; "s1", 400; "s2", 400; "s3", 400 ] in
    List.iter
      (fun (name, size) ->
        Fs_compat.save_file
          (Filename.concat artifact_dir (name ^ "-proof.json"))
          (String.make size 'x'))
      sizes;
    let request_id = "vrf-transport-most-items" in
    let evidence_snapshot =
      VS.snapshot_submitted_evidence_json
        ~base_path
        ~worker:"omega"
        (List.map (fun (name, _) -> "artifact:" ^ name ^ "-proof.json") sizes)
    in
    (match
       V.create_request
         ~base_path
         ~request_id
         ~task_id:"task-001"
         ~output:(`Assoc [ "submitted_evidence", evidence_snapshot ])
         ~criteria:[ "inspect artifact" ]
         ~worker:"omega"
         ()
     with
     | Ok _ -> ()
     | Error detail -> Alcotest.fail detail);
    match inspect_evidence ~base_path ~request_id () with
    | VS.Evidence_available _ as access ->
      let rendered =
        match VS.submitted_evidence_access_transport_to_yojson access with
        | `Assoc fields ->
          (match List.assoc "items" fields with
           | `List rendered ->
             List.map
               (function
                 | `Assoc item -> item
                 | _ -> Alcotest.fail "transport item must be an object")
               rendered
           | _ -> Alcotest.fail "transport items must be a list")
        | _ -> Alcotest.fail "transport access must be an object"
      in
      let reference_of item =
        match List.assoc_opt "reference" item with
        | Some (`String value) -> value
        | _ -> ""
      in
      let carried =
        List.filter_map
          (fun item ->
            if List.mem_assoc "content" item then Some (reference_of item) else None)
          rendered
      in
      Alcotest.(check int) "three small artifacts keep their content" 3 (List.length carried);
      List.iter
        (fun name ->
          Alcotest.(check bool)
            ("small artifact " ^ name ^ " is readable")
            true
            (List.mem ("artifact:" ^ name ^ "-proof.json") carried))
        [ "s1"; "s2"; "s3" ];
      Alcotest.(check bool)
        "the one that does not fit is the large one"
        false
        (List.mem "artifact:big-proof.json" carried);
      Alcotest.(check bool)
        "submission order is preserved in the emitted list"
        true
        (List.map reference_of rendered
         = List.map (fun (name, _) -> "artifact:" ^ name ^ "-proof.json") sizes)
    | VS.Evidence_unavailable _ -> Alcotest.fail "evidence must be available")
;;

(* #29596: the per-item cap bounds one artifact, not their sum. A submission of
   many sub-cap artifacts still built a request no verifier_exact slot could
   carry — 12 artifacts, every one [truncated=false], 1,005,015 bytes in the
   atom that stalled task-465 (2026-08-25). Each artifact here is far under the
   cap and none is truncated; only their total crosses it. *)
let test_transport_projection_bounds_the_evidence_total () =
  with_eio_temp_dir (fun base_path ->
    let artifact_dir = Filename.concat base_path ".masc/playground/docker/omega" in
    Fs_compat.mkdir_p artifact_dir;
    write_keeper_profile
      ~base_path
      ~keeper_name:"omega"
      ~sandbox_profile:"docker";
    let cap = VS.verification_evidence_max_bytes in
    let each = cap / 3 in
    let names = [ "a"; "b"; "c"; "d"; "e" ] in
    List.iter
      (fun name ->
        Fs_compat.save_file
          (Filename.concat artifact_dir (name ^ "-proof.json"))
          (String.make each 'x'))
      names;
    let request_id = "vrf-transport-total" in
    let evidence_snapshot =
      VS.snapshot_submitted_evidence_json
        ~base_path
        ~worker:"omega"
        (List.map (fun name -> "artifact:" ^ name ^ "-proof.json") names)
    in
    (match
       V.create_request
         ~base_path
         ~request_id
         ~task_id:"task-001"
         ~output:(`Assoc [ "submitted_evidence", evidence_snapshot ])
         ~criteria:[ "inspect artifact" ]
         ~worker:"omega"
         ()
     with
     | Ok _ -> ()
     | Error detail -> Alcotest.fail detail);
    match inspect_evidence ~base_path ~request_id () with
    | VS.Evidence_available { items; _ } as access ->
      List.iter
        (fun item ->
          match item with
          | VS.Evidence_artifact { truncated; _ } ->
            Alcotest.(check bool) "no item is truncated on its own" false truncated
          | VS.Evidence_note _ | VS.Evidence_invalid_reference
          | VS.Evidence_artifact_unreadable _ | VS.Evidence_artifact_binary _ -> ())
        items;
      Alcotest.(check bool)
        "the stored snapshot carries every byte"
        true
        (String.length
           (Yojson.Safe.to_string (VS.submitted_evidence_access_to_yojson access))
         > cap);
      let transport =
        Yojson.Safe.to_string (VS.submitted_evidence_access_transport_to_yojson access)
      in
      Alcotest.(check bool)
        "the judge request stays inside the cap"
        true
        (String.length transport <= cap + 4_096);
      let rendered =
        match VS.submitted_evidence_access_transport_to_yojson access with
        | `Assoc fields ->
          (match List.assoc "items" fields with
           | `List rendered -> List.map (function
               | `Assoc item -> item
               | _ -> Alcotest.fail "transport item must be an object") rendered
           | _ -> Alcotest.fail "transport items must be a list")
        | _ -> Alcotest.fail "transport access must be an object"
      in
      let carried = List.filter (List.mem_assoc "content") rendered in
      let withheld = List.filter (List.mem_assoc "content_omitted") rendered in
      Alcotest.(check int) "every artifact is still listed" 5 (List.length rendered);
      Alcotest.(check bool)
        "the ones that fit keep their content"
        true
        (List.length carried > 0);
      Alcotest.(check bool)
        "the ones past the cap are withheld"
        true
        (List.length withheld > 0);
      Alcotest.(check int)
        "each artifact is carried or withheld, never both"
        5
        (List.length carried + List.length withheld);
      List.iter
        (fun item ->
          Alcotest.(check bool)
            "a withheld artifact still names its reference"
            true
            (List.mem_assoc "reference" item && List.mem_assoc "bytes" item))
        withheld
    | VS.Evidence_unavailable _ -> Alcotest.fail "evidence must be available")
;;

(* #29615: a truncated artifact's prefix must not travel to the judge. The
   persistence serializer keeps it for the audit record; the transport
   projection replaces it with the size, the fact, and how to read the real
   file. The length guard is the regression: one over-cap artifact plus a
   readable one must not push the review request toward any slot's input
   budget. *)
let test_transport_projection_omits_truncated_prefix () =
  with_eio_temp_dir (fun base_path ->
    let artifact_dir =
      Filename.concat base_path ".masc/playground/docker/omega"
    in
    Fs_compat.mkdir_p artifact_dir;
    write_keeper_profile
      ~base_path
      ~keeper_name:"omega"
      ~sandbox_profile:"docker";
    let cap = VS.verification_evidence_max_bytes in
    let large = String.make (cap + 50_000) 'x' in
    Fs_compat.save_file (Filename.concat artifact_dir "large-proof.json") large;
    Fs_compat.save_file
      (Filename.concat artifact_dir "small-proof.txt")
      "small readable proof";
    let request_id = "vrf-transport-projection" in
    let evidence_snapshot =
      VS.snapshot_submitted_evidence_json
        ~base_path
        ~worker:"omega"
        [ "artifact:large-proof.json"
        ; "artifact:small-proof.txt"
        ; "note:producer summary"
        ]
    in
    (match
       V.create_request
         ~base_path
         ~request_id
         ~task_id:"task-001"
         ~output:(`Assoc [ "submitted_evidence", evidence_snapshot ])
         ~criteria:[ "inspect artifact" ]
         ~worker:"omega"
         ()
     with
    | Ok _ -> ()
    | Error detail -> Alcotest.fail detail);
    match inspect_evidence ~base_path ~request_id () with
    | VS.Evidence_available _ as access ->
      Alcotest.(check bool)
        "persistence serializer still carries the truncated prefix"
        true
        (String.length
           (Yojson.Safe.to_string
              (VS.submitted_evidence_access_to_yojson access))
         > cap);
      let transport =
        Yojson.Safe.to_string
          (VS.submitted_evidence_access_transport_to_yojson access)
      in
      Alcotest.(check bool)
        "transport stays far under the snapshot cap"
        true
        (String.length transport < cap / 2);
      let items =
        match
          VS.submitted_evidence_access_transport_to_yojson access
        with
        | `Assoc fields ->
          (match List.assoc "items" fields with
           | `List items -> items
           | _ -> Alcotest.fail "transport items must be a list")
        | _ -> Alcotest.fail "transport access must be an object"
      in
      let fields_of = function
        | `Assoc fields -> fields
        | _ -> Alcotest.fail "transport item must be an object"
      in
      let by_reference reference =
        List.find_opt
          (fun item ->
             match List.assoc_opt "reference" (fields_of item) with
             | Some (`String value) -> String.equal value reference
             | _ -> false)
          items
        |> Option.get
      in
      let large_item = fields_of (by_reference "artifact:large-proof.json") in
      Alcotest.(check bool)
        "truncated artifact carries no content key"
        true
        (not (List.mem_assoc "content" large_item));
      Alcotest.(check bool)
        "truncated artifact declares content_omitted"
        true
        (List.assoc_opt "content_omitted" large_item = Some (`Bool true));
      (match List.assoc_opt "bytes" large_item with
       | Some (`Int bytes) ->
         Alcotest.(check int) "transport keeps the full byte count"
           (String.length large) bytes
       | _ -> Alcotest.fail "truncated artifact must keep its byte count");
      (match List.assoc_opt "content_note" large_item with
       | Some (`String note) ->
         Alcotest.(check bool)
           "note states the file size"
           true
           (Astring.String.is_infix
              ~affix:(string_of_int (String.length large))
              note);
         Alcotest.(check bool)
           "note points at the verification tools"
           true
           (Astring.String.is_infix ~affix:"verification tools" note)
       | _ -> Alcotest.fail "truncated artifact must explain the omission");
      let small_item = fields_of (by_reference "artifact:small-proof.txt") in
      Alcotest.(check string)
        "readable artifact still travels in full"
        "small readable proof"
        (match List.assoc "content" small_item with
         | `String content -> content
         | _ -> Alcotest.fail "readable artifact content must be a string")
    | VS.Evidence_unavailable { reason; _ } ->
      Alcotest.failf
        "evidence unavailable: %s"
        (VS.evidence_access_failure_to_string
           ~request_id
           reason))

let test_truncated_snapshot_items_names_only_truncated_artifacts () =
  with_eio_temp_dir (fun base_path ->
    let artifact_dir =
      Filename.concat base_path ".masc/playground/docker/omega"
    in
    Fs_compat.mkdir_p artifact_dir;
    write_keeper_profile
      ~base_path
      ~keeper_name:"omega"
      ~sandbox_profile:"docker";
    let cap = VS.verification_evidence_max_bytes in
    let large = String.make (cap + 1_000) 'y' in
    Fs_compat.save_file (Filename.concat artifact_dir "bulky.json") large;
    Fs_compat.save_file
      (Filename.concat artifact_dir "compact.json")
      "{\"ok\":true}";
    let evidence_snapshot =
      VS.snapshot_submitted_evidence_json
        ~base_path
        ~worker:"omega"
        [ "artifact:bulky.json"; "artifact:compact.json" ]
    in
    match VS.truncated_snapshot_items evidence_snapshot with
    | [ (reference, bytes) ] ->
      Alcotest.(check string)
        "only the over-cap artifact is reported"
        "artifact:bulky.json"
        reference;
      Alcotest.(check int) "reported size is the full file" (String.length large)
        bytes
    | other ->
      Alcotest.failf "expected exactly one truncated item, got %d" (List.length other))

let test_submitted_evidence_rejects_malformed_utf8 () =
  with_eio_temp_dir (fun base_path ->
    let artifact_dir =
      Filename.concat
        base_path
        ".masc/playground/docker/omega"
    in
    Fs_compat.mkdir_p artifact_dir;
    let artifact_path = Filename.concat artifact_dir "malformed.txt" in
    Fs_compat.save_file artifact_path "\xC3\x28";
    let request_id = "vrf-malformed-evidence" in
    ignore (create_evidence_request ~base_path ~request_id ~artifact_path);
    match
      inspect_evidence
        ~base_path
        ~request_id
        ()
    with
    | VS.Evidence_available
        { items =
            VS.Evidence_artifact_unreadable
              { reason = VS.Evidence_invalid_utf8; _ }
            :: _
        ; _
        } ->
      ()
    | _ -> Alcotest.fail "malformed UTF-8 must remain unreadable")

let test_submitted_evidence_rejects_symlink_escape_and_fifo () =
  with_eio_temp_dir (fun base_path ->
    let artifact_dir =
      Filename.concat
        base_path
        ".masc/playground/docker/omega"
    in
    Fs_compat.mkdir_p artifact_dir;
    let outside_path = Filename.concat base_path "outside-secret.txt" in
    Fs_compat.save_file outside_path "outside-secret";
    let symlink_path = Filename.concat artifact_dir "outside-link.txt" in
    Unix.symlink outside_path symlink_path;
    let symlink_request_id = "vrf-symlink-evidence" in
    ignore
      (create_evidence_request
         ~base_path
         ~request_id:symlink_request_id
         ~artifact_path:symlink_path);
    (match
       inspect_evidence
         ~base_path
         ~request_id:symlink_request_id
         ()
     with
     | VS.Evidence_available
         { items =
             VS.Evidence_artifact_unreadable
               { reason = VS.Evidence_symbolic_link; _ }
             :: _
         ; _
         } ->
       ()
     | _ -> Alcotest.fail "symlink escape must remain unreadable");
    let fifo_path = Filename.concat artifact_dir "evidence.fifo" in
    Unix.mkfifo fifo_path 0o600;
    let fifo_request_id = "vrf-fifo-evidence" in
    ignore
      (create_evidence_request
         ~base_path
         ~request_id:fifo_request_id
         ~artifact_path:fifo_path);
    match
      inspect_evidence
        ~base_path
        ~request_id:fifo_request_id
        ()
    with
    | VS.Evidence_available
        { items =
            VS.Evidence_artifact_unreadable
              { reason = VS.Evidence_not_regular_file; _ }
            :: _
        ; _
        } ->
      ()
    | _ -> Alcotest.fail "FIFO evidence must remain unreadable")

let test_changed_during_read_maps_to_typed_unreadable_reason () =
  Alcotest.(check string)
    "exact-read race remains typed"
    "changed_during_read"
    (VS.evidence_read_failure_of_owned_read_failure
       (Fs_compat.Filesystem_identity_changed { path = "artifact.txt" })
     |> VS.evidence_read_failure_code)

let test_submitted_evidence_requires_exact_task_assignment_identity () =
  with_eio_temp_dir (fun base_path ->
    let config = W.default_config base_path in
    ensure_keeper_meta config "omega";
    let artifact_dir =
      Filename.concat
        base_path
        ".masc/playground/docker/omega"
    in
    Fs_compat.mkdir_p artifact_dir;
    let artifact_path = Filename.concat artifact_dir "assignment.txt" in
    Fs_compat.save_file artifact_path "assignment-secret";
    let request_id = "vrf-assignment-authority" in
    let pending =
      match
        V.create_request
          ~base_path
          ~request_id
          ~task_id:"task-001"
          ~output:
            (`Assoc
                [ ( "submitted_evidence"
                  , VS.snapshot_submitted_evidence_json
                      ~base_path
                      ~worker:"omega"
                      [ "artifact:assignment.txt" ] )
                ])
          ~criteria:[ "inspect artifact" ]
          ~worker:"omega"
          ()
      with
      | Ok request -> request
      | Error detail -> Alcotest.fail detail
    in
    let check_unavailable label = function
      | VS.Evidence_unavailable _ -> ()
      | _ -> Alcotest.fail label
    in
    (match
       inspect_evidence
         ~base_path
         ~request_id:pending.id
         ()
     with
     | VS.Evidence_available _ -> ()
     | _ -> Alcotest.fail "pending evidence must be available to the completion authority");
    check_unavailable
      "task id mismatch must not expose bytes"
      (inspect_evidence
         ~task_id:"task-other"
         ~base_path
         ~request_id:pending.id
         ());
    check_unavailable
      "producer mismatch must not expose bytes"
      (inspect_evidence
         ~task_worker:"keeper-other-agent"
         ~base_path
         ~request_id:pending.id
         ()))

let test_keeper_task_projection_never_exposes_snapshot_or_verdict_action () =
  with_eio_temp_dir (fun base_path ->
    let config = W.default_config base_path in
    ignore (W.init config ~agent_name:None);
    ignore
      (W.add_task
         config
         ~title:"Produce one verifiable artifact"
         ~priority:1
         ~description:"");
    let artifact_dir =
      Filename.concat
        base_path
        ".masc/playground/docker/omega"
    in
    Fs_compat.mkdir_p artifact_dir;
    let artifact_path = Filename.concat artifact_dir "artifact-task-001.txt" in
    Fs_compat.save_file artifact_path "full-cycle-evidence";
    let request_id = "vrf-task-list-projection" in
    ignore (create_evidence_request ~base_path ~request_id ~artifact_path);
    let backlog = W.read_backlog config in
    let tasks =
      List.map
        (fun (task : Masc_domain.task) ->
           { task with
             task_status =
               Masc_domain.AwaitingVerification
                 { assignee = "omega"
                 ; started_at = "2026-07-27T23:59:00Z"
                 ; submitted_at = "2026-07-28T00:00:00Z"
                 ; intent = Complete_task
                 ; verification_id = request_id
                 }
           })
        backlog.tasks
    in
    W.write_backlog config { backlog with tasks };
    let projection = W.list_tasks config in
    Alcotest.(check bool)
      "row identifies the completion-authority wait"
      true
      (String_util.contains_substring projection
         "awaiting_completion_authority task_id=task-001");
    Alcotest.(check bool)
      "keeper row does not choose a verdict action"
      false
      (String_util.contains_substring projection "ACTION:");
    (match
       W.claim_task_r config ~agent_name:"omega" ~task_id:"task-001" ()
     with
     | Error _ -> ()
     | Ok _ -> Alcotest.fail "producer must not claim its pending obligation");
    (match
       W.claim_task_r config ~agent_name:"keeper-verifier-agent" ~task_id:"task-001" ()
     with
     | Error _ -> ()
     | Ok _ -> Alcotest.fail "no Keeper may claim a pending obligation");
    (match
       W.claim_task_r config ~agent_name:"keeper-alpha-agent" ~task_id:"task-001" ()
     with
     | Error _ -> ()
     | Ok _ -> Alcotest.fail "another Keeper claimed the pending obligation");
    Alcotest.(check bool)
      "task projection contains no evidence bytes"
      false
      (String_util.contains_substring projection "full-cycle-evidence");
    Alcotest.(check bool)
      "task projection keeps request metadata"
      true
      (String_util.contains_substring projection request_id);
    Alcotest.(check bool)
      "task projection has no assigned verifier"
      false
      (String_util.contains_substring projection "assigned_verifier=");
    Alcotest.(check bool)
      "task projection has no verdict action"
      false
      (String_util.contains_substring projection "ACTION:"))

(* --- ID generation property test (#7544) --- *)

module StringSet = Set.Make (String)

let test_generate_id_prefix () =
  let id = V.generate_id () in
  Alcotest.(check bool) "vrf- prefix" true
    (String.length id > 4 && String.sub id 0 4 = "vrf-")

let test_generate_id_no_collisions () =
  (* 10000 consecutive ids must be unique — the old Hashtbl.hash-based
     generator collided within the same millisecond. *)
  let n = 10_000 in
  let seen = ref StringSet.empty in
  for _ = 1 to n do
    let id = V.generate_id () in
    seen := StringSet.add id !seen
  done;
  Alcotest.(check int) "all 10k ids unique" n (StringSet.cardinal !seen)

(* [tool_task_completion_review.ml] says of [verification_evidence_fields]:
   "Shares the derived [verification_evidence_to_yojson] so the serialization
   tested by the roundtrip is the one production emits."

   There was no such roundtrip. The type reaches the verification request
   output, the Board meta and SSE through [verification_evidence_fields], so its
   wire shape is a contract with readers outside this repo's control, and
   nothing pinned it.

   Both expectations below are literals. Encoding with the function under test
   and comparing against itself would pass whatever the encoder became. *)

module CR = Masc.Task.Completion_review

let evidence_fixture : CR.verification_evidence =
  { required_artifacts = [ "artifact:spec.md"; "artifact:bench.json" ]
  ; submitted_evidence = [ "note:tsc passes"; "artifact:run.log" ]
  }

let evidence_wire : Yojson.Safe.t =
  `Assoc
    [ ( "required_artifacts"
      , `List [ `String "artifact:spec.md"; `String "artifact:bench.json" ] )
    ; ( "submitted_evidence"
      , `List [ `String "note:tsc passes"; `String "artifact:run.log" ] )
    ]

let test_verification_evidence_roundtrip () =
  Alcotest.(check string)
    "encodes to the stated wire shape"
    (Yojson.Safe.to_string evidence_wire)
    (Yojson.Safe.to_string (CR.verification_evidence_to_yojson evidence_fixture));
  match CR.verification_evidence_of_yojson evidence_wire with
  | Error detail -> Alcotest.failf "the stated wire shape must decode: %s" detail
  | Ok decoded ->
    Alcotest.(check (list string))
      "required_artifacts survives the trip"
      evidence_fixture.required_artifacts
      decoded.required_artifacts;
    Alcotest.(check (list string))
      "submitted_evidence survives the trip"
      evidence_fixture.submitted_evidence
      decoded.submitted_evidence

(* The half the comment is actually about: what production splices into the
   request, the Board meta and SSE is the encoder this roundtrip pins, not a
   second hand-built object beside it. *)
let test_verification_evidence_fields_are_the_encoded_object () =
  let fields = CR.verification_evidence_fields evidence_fixture in
  Alcotest.(check string)
    "the spliced fields are the encoded object"
    (Yojson.Safe.to_string evidence_wire)
    (Yojson.Safe.to_string (`Assoc fields))

(* An absent list is not an empty one: a reader that sees no key cannot tell
   "nothing was required" from "the producer did not say". *)
let test_verification_evidence_decode_requires_both_keys () =
  List.iter
    (fun (label, json) ->
       match CR.verification_evidence_of_yojson json with
       | Ok _ -> Alcotest.failf "%s must not decode" label
       | Error _ -> ())
    [ ("a missing submitted_evidence", `Assoc [ ("required_artifacts", `List []) ])
    ; ("a missing required_artifacts", `Assoc [ ("submitted_evidence", `List []) ])
    ; ("a non-object", `List [])
    ]

(* The store resolves an artifact reference against the producer's sandbox
   root and nowhere else.

   It used to guess further: on a miss it enumerated [<root>/repos/*] and read
   the file when exactly one entry held it. A keeper puts its checkouts where
   it likes — [Keeper_playground_checkouts] exists because three scans each
   hardcoded [repos/] and disagreed — so that guess missed every top-level
   checkout, and where a keeper holds one repository several times the file
   sits in several of them with different content. Reading a file the
   submitter did not name hands the judge content without its provenance.

   The miss is now typed and travels to the judge, which holds Read/Grep on
   this root and a root_layout naming every checkout under it. *)
(* The injected artifact read: where the producer's sandbox keeps the file,
   the snapshot records what the reader answered -- content, bytes, truncated
   on Ok, the typed reason on Error -- instead of reading the host bundle the
   store can reach (#33745). *)
let test_injected_artifact_read_answers_the_snapshot () =
  with_temp_dir (fun base_path ->
      let artifact_read =
        Some
          (fun ~worker ~relative ->
             if
               String.equal worker "endpoint-worker"
               && String.equal relative "evidence.txt"
             then Ok (VS.Text_payload ("captured-by-backend", 20, false))
             else Error (VS.Evidence_read_error "backend: not found"))
      in
      let json =
        VS.snapshot_submitted_evidence_json
          ?artifact_read
          ~base_path
          ~worker:"endpoint-worker"
          [ "artifact:evidence.txt"; "artifact:missing.txt" ]
      in
      let open Yojson.Safe.Util in
      let items = json |> to_list in
      let kind_of item = item |> member "kind" |> to_string in
      Alcotest.(check string) "the reader's artifact is recorded"
        "artifact" (kind_of (List.nth items 0));
      Alcotest.(check string) "with the reader's content"
        "captured-by-backend"
        (List.nth items 0 |> member "content" |> to_string);
      Alcotest.(check string) "the reader's failure is typed"
        "artifact_unreadable" (kind_of (List.nth items 1));
      Alcotest.(check string) "carrying the reader's reason code"
        "read_error"
        (List.nth items 1 |> member "reason" |> member "code" |> to_string))

(* An injected reader answers under the same text line the direct read
   holds: binary bytes are typed invalid_utf8, and a truncated read may stop
   mid-character exactly where the direct prefix can. *)
let test_an_injected_reader_answers_under_the_text_line () =
  with_temp_dir (fun base_path ->
      let artifact_read =
        Some
          (fun ~worker ~relative ->
             ignore worker;
             if String.equal relative "logo.png" then
               (* a reader that hands non-text bytes to the text payload
                  still meets the store's own line *)
               Ok (VS.Text_payload ("\xff\xd8\xff\xe0garbage", 12, false))
             else if String.equal relative "cut.txt" then
               Ok (VS.Text_payload ("abc\xe2\x82", 5, true))
             else Error (VS.Evidence_read_error "backend: not found"))
      in
      let json =
        VS.snapshot_submitted_evidence_json
          ?artifact_read
          ~base_path
          ~worker:"endpoint-worker"
          [ "artifact:logo.png"; "artifact:cut.txt" ]
      in
      let open Yojson.Safe.Util in
      let items = json |> to_list in
      Alcotest.(check string) "binary bytes are refused, typed"
        "artifact_unreadable"
        (List.nth items 0 |> member "kind" |> to_string);
      Alcotest.(check string) "with the text-line reason"
        "invalid_utf8"
        (List.nth items 0 |> member "reason" |> member "code" |> to_string);
      Alcotest.(check string) "a truncated multibyte cut keeps the whole characters"
        "abc" (List.nth items 1 |> member "content" |> to_string);
      Alcotest.(check bool) "and stays marked truncated"
        true (List.nth items 1 |> member "truncated" |> to_bool))

let test_artifact_reference_size_uses_the_injected_reader () =
  with_temp_dir (fun base_path ->
      let artifact_read =
        Some
          (fun ~worker ~relative ->
             ignore worker;
             if String.equal relative "big.log" then Ok (VS.Text_payload ("", 90_000, true))
             else Error (VS.Evidence_read_error "backend: not found"))
      in
      Alcotest.(check (option int))
        "size comes from the reader, not the host bundle"
        (Some 90_000)
        (VS.artifact_reference_size
           ?artifact_read
           ~base_path
           ~worker:"endpoint-worker"
           "artifact:big.log");
      Alcotest.(check (option int))
        "a reader failure is not a size"
        None
        (VS.artifact_reference_size
           ?artifact_read
           ~base_path
           ~worker:"endpoint-worker"
           "artifact:gone.log"))

(* Endpoint-owned trees (microvm, remote-ssh) read through the backend;
   a shared-mount (Docker) tree keeps the store's direct host read. The
   returned closure is not called here -- the routing is the unit. *)
let test_reader_routes_by_where_the_tree_lives () =
  with_temp_dir (fun base_path ->
      let config = W.default_config base_path in
      ignore (W.init config ~agent_name:None);
      let route profile =
        ensure_keeper_meta config "route-worker";
        write_keeper_profile
          ~base_path
          ~keeper_name:"route-worker"
          ~sandbox_profile:profile;
        match Masc.Keeper_meta_store.read_effective_meta config "route-worker" with
        | Ok (Some meta) ->
            Option.is_some
              (Masc.Keeper_tool_task_runtime.evidence_artifact_reader ~config ~meta ())
        | Ok None -> Alcotest.fail "meta did not load (none)"
        | Error detail -> Alcotest.failf "meta did not load: %s" detail
      in
      Alcotest.(check bool) "microvm reads through the backend" true
        (route "microvm");
      Alcotest.(check bool) "docker keeps the direct host read" false
        (route "docker"))

(* RFC-0436 §4.1-4.2: a binary payload is adopted, not refused -- the
   snapshot keeps the hash, size and format, and files the bytes as the
   evidence body when the caller names the request. *)
let test_a_binary_payload_is_adopted_and_filed () =
  with_temp_dir (fun base_path ->
      let png_bytes = "\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR" in
      let artifact_read =
        Some
          (fun ~worker ~relative ->
             ignore worker;
             if String.equal relative "shot.png" then
               Ok
                 (VS.Binary_payload
                    { data = png_bytes
                    ; bytes = String.length png_bytes
                    ; sha256 = Digestif.SHA256.(digest_string png_bytes |> to_hex)
                    ; format = "png"
                    })
             else Error (VS.Evidence_read_error "backend: not found"))
      in
      let filed =
        VS.snapshot_submitted_evidence_json
          ?artifact_read
          ~request_id:"vrf-binary-test"
          ~base_path
          ~worker:"endpoint-worker"
          [ "artifact:shot.png" ]
      in
      let open Yojson.Safe.Util in
      let item = List.nth (filed |> to_list) 0 in
      Alcotest.(check string) "adopted as a binary artifact"
        "artifact_binary" (item |> member "kind" |> to_string);
      Alcotest.(check int) "with its byte count"
        (String.length png_bytes) (item |> member "bytes" |> to_int);
      Alcotest.(check string) "with its format" "png"
        (item |> member "format" |> to_string);
      let body_path = item |> member "body" |> to_string in
      Alcotest.(check string) "the body is filed under the request"
        "evidence/vrf-binary-test/0.bin" body_path;
      let masc_dir = CU.masc_dir_from_base_path ~base_path in
      let filed_bytes = Fs_compat.load_file (Filename.concat masc_dir body_path) in
      Alcotest.(check string) "the filed bytes are the read bytes" png_bytes filed_bytes;
      let unfiled =
        VS.snapshot_submitted_evidence_json
          ?artifact_read
          ~base_path
          ~worker:"endpoint-worker"
          [ "artifact:shot.png" ]
      in
      let open Yojson.Safe.Util in
      let bare = List.nth (unfiled |> to_list) 0 in
      Alcotest.(check bool) "without a request id there is no body field"
        false (bare |> member "body" != `Null))

let test_checkout_relative_artifact_is_not_guessed () =
  with_temp_dir (fun base_path ->
    let config = W.default_config base_path in
    ignore (W.init config ~agent_name:(Some "checkout-worker"));
    ensure_keeper_meta config "checkout-worker";
    ensure_producer_playground config "checkout-worker";
    let root =
      Keeper_sandbox_config.host_root_abs_of_agent
        ~base_path:(VS.project_root_of_base_path config.base_path)
        ~agent_name:"checkout-worker"
    in
    let write relative contents =
      let path = Filename.concat root relative in
      Fs_compat.mkdir_p (Filename.dirname path);
      Out_channel.with_open_text path (fun channel ->
        Out_channel.output_string channel contents)
    in
    (* Two checkouts of the same repository, one under [repos/] and one at the
       top level, each holding the same relative path with different content.
       This is the polisher layout that produced task-785. *)
    write "repos/masc/lib/proof.ml" "content from repos checkout";
    write "masc/lib/proof.ml" "content from top-level checkout";
    let item reference =
      VS.snapshot_submitted_evidence_json
        ~base_path:config.base_path
        ~worker:"checkout-worker"
        [ reference ]
      |> Yojson.Safe.Util.to_list
      |> List.hd
    in
    let open Yojson.Safe.Util in
    let checkout_relative = item "artifact:lib/proof.ml" in
    Alcotest.(check string)
      "a checkout-relative path resolves to no file and says so"
      "artifact_unreadable"
      (checkout_relative |> member "kind" |> to_string);
    Alcotest.(check string)
      "the judge is told which reference missed"
      "artifact:lib/proof.ml"
      (checkout_relative |> member "reference" |> to_string);
    Alcotest.(check string)
      "and why"
      (VS.evidence_read_failure_code VS.Evidence_missing)
      (checkout_relative |> member "reason" |> member "code" |> to_string);
    (* The path the submitter can write instead still reads, and it reads the
       checkout it names rather than whichever one sorted first. *)
    let named = item "artifact:masc/lib/proof.ml" in
    Alcotest.(check string)
      "a root-relative path reads"
      "artifact"
      (named |> member "kind" |> to_string);
    Alcotest.(check string)
      "and reads the checkout it named"
      "content from top-level checkout"
      (named |> member "content" |> to_string))
;;

(* task-808 was refused for a missing artifact and approved four minutes later
   with the file still absent. The answer has to be the same both times, and
   after 2026-08-29 the judge is the one giving it: an unreadable artifact no
   longer stops the review before an evaluator runs. What this pins is that
   the judge is handed enough to answer with — which reference failed and why
   — rather than a bundle that silently omits it. *)
let test_unreadable_artifacts_reach_the_judge () =
  let items =
    [ VS.Evidence_note "narrative evidence"
    ; VS.Evidence_artifact
        { reference = "artifact:proof.txt"
        ; content = "read from disk"
        ; bytes = 14
        ; truncated = false
        }
    ; VS.Evidence_artifact_unreadable
        { reference = "artifact:missing.txt"; reason = VS.Evidence_missing }
    ; VS.Evidence_artifact_unreadable
        { reference = "artifact:locked.txt"
        ; reason = VS.Evidence_read_error "Unix.Unix_error(EACCES, open, _)"
        }
    ]
  in
  let transported =
    VS.submitted_evidence_access_transport_to_yojson
      (VS.Evidence_available
         { request =
             { id = "vrf-001"
             ; task_id = "task-001"
             ; worker = "producer"
             ; created_at = 0.0
             }
         ; items
         })
  in
  let open Yojson.Safe.Util in
  let carried =
    transported
    |> member "items"
    |> to_list
    |> List.filter_map (fun item ->
      match item |> member "kind" |> to_string with
      | "artifact_unreadable" ->
        Some
          ( item |> member "reference" |> to_string
          , item |> member "reason" |> to_string )
      | _ -> None)
  in
  Alcotest.(check (list (pair string string)))
    "every unreadable artifact reaches the judge with its reference and reason"
    [ "artifact:missing.txt", VS.evidence_read_failure_code VS.Evidence_missing
    ; ( "artifact:locked.txt"
      , VS.evidence_read_failure_code
          (VS.Evidence_read_error "Unix.Unix_error(EACCES, open, _)") )
    ]
    carried
;;

(* The request-to-question mapping. The request supplies the completion
   material; a stop never reaches this mapping, so a request shaped for one is
   a missing field rather than a question. *)
let request_with ~output =
  { V.id = "vrf-question"
  ; task_id = "task-question"
  ; output
  ; criteria = [ "the cancel record is written from the produced status" ]
  ; worker = "worker-1"
  ; created_at = 1234.5
  }
;;

let completion_output =
  `Assoc [ "required_artifacts", `List [ `String "note:done" ] ]
;;

(* An output with no completion material. The operator reads a stop from its
   Board post, not from this mapping, so nothing in a record stands in for
   [required_artifacts]. *)
let output_without_completion_material =
  `Assoc [ "task_title", `String "the upstream schema landed instead" ]
;;

let test_the_request_asks_the_completion_question () =
  match
    CA.For_testing.verdict_question_of_request
      (request_with ~output:completion_output)
  with
  | Error e -> Alcotest.fail ("completion question must be built: " ^ e)
  | Ok
      { Masc.Task.Anti_rationalization.completion_contract
      ; required_evidence
      ; evidence_posture
      ; few_shot_block
      } ->
    (* This function maps an intent to a question and reads no store. The
       calibration block and the evidence posture are filled at the review
       site, where the snapshot and the ledger are opened; a value here would
       mean this mapping had grown a disk read. *)
    Alcotest.(check string)
      "the calibration block is not filled by the mapping" "" few_shot_block;
    Alcotest.(check bool)
      "the posture is the same placeholder, filled at the review site"
      true
      (evidence_posture = Masc.Task.Anti_rationalization.Note_only);
    Alcotest.(check (option (list string)))
      "the contract carries the request criteria"
      (Some [ "the cancel record is written from the produced status" ])
      completion_contract;
    Alcotest.(check (list string))
      "the required evidence is the request's required_artifacts"
      [ "note:done" ] required_evidence
;;

(* A record without the completion material is a missing field, not a
   question that happens to fit. *)
let test_a_record_without_completion_material_is_no_question () =
  let expect_error label output =
    match CA.For_testing.verdict_question_of_request (request_with ~output) with
    | Ok _ -> Alcotest.fail (label ^ " must not build a question")
    | Error _ -> ()
  in
  expect_error "an output without completion material" output_without_completion_material;
  expect_error "a non-object output" (`String "the upstream schema landed instead")
;;

let () =
  Alcotest.run "Verification" [
    "verification_evidence wire", [
      Alcotest.test_case "roundtrip" `Quick test_verification_evidence_roundtrip;
      Alcotest.test_case "spliced fields are the encoded object" `Quick
        test_verification_evidence_fields_are_the_encoded_object;
      Alcotest.test_case "decode requires both keys" `Quick
        test_verification_evidence_decode_requires_both_keys;
    ];
    "verdict question", [
      Alcotest.test_case "the request asks the completion question" `Quick
        test_the_request_asks_the_completion_question;
      Alcotest.test_case "a record without completion material is no question" `Quick
        test_a_record_without_completion_material_is_no_question;
    ];
    "criterion", [
      Alcotest.test_case "roundtrip" `Quick test_criterion_roundtrip;
      Alcotest.test_case "of_yojson errors" `Quick test_criterion_of_yojson_errors;
      Alcotest.test_case "request parser is strict" `Quick
        test_request_of_yojson_is_strict;
    ];
    "completion_authority", [
      Alcotest.test_case "system LLM helpers keep typed facts" `Quick
        test_system_llm_authority_helpers_are_typed;
      Alcotest.test_case "system LLM retry disposition is typed" `Quick
        test_system_llm_retry_disposition_is_typed;
      Alcotest.test_case "a cancel claim is routed to the operator" `Quick
        test_cancel_claim_is_routed_to_the_operator;
      Alcotest.test_case "scan scope limits a submission to its own verification" `Quick
        test_scan_scope_limits_a_submission_to_its_own_verification;
      Alcotest.test_case "system LLM notes keep metadata only" `Quick
        test_system_llm_review_notes_are_metadata_only;
      Alcotest.test_case "unreadable evidence uses structured current contract" `Quick
        test_unreadable_evidence_uses_structured_current_contract;
      Alcotest.test_case "invalid reference rejects hidden payload" `Quick
        test_invalid_reference_snapshot_rejects_hidden_payload;
      Alcotest.test_case "system LLM rejection reaches producer queue" `Quick
        test_system_llm_rejection_is_durably_delivered_to_producer_keeper;
      Alcotest.test_case "system LLM rejection uses registry producer binding" `Quick
        test_system_llm_rejection_prefers_registered_producer_binding;
      Alcotest.test_case "system LLM rejection does not derive unregistered Keeper" `Quick
        test_system_llm_rejection_does_not_derive_unregistered_keeper;
      Alcotest.test_case "system LLM commits without Keeper verifier" `Quick
        test_system_llm_agent_commits_without_a_keeper_verifier;
      Alcotest.test_case "system LLM invalid contract remains pending" `Quick
        test_system_llm_agent_defers_invalid_contract_without_rejecting_task;
      Alcotest.test_case "system LLM uses persisted request contract" `Quick
        test_system_llm_agent_uses_persisted_request_contract_snapshot;
      Alcotest.test_case "rejected verdict audit keeps reason" `Quick
        test_rejected_verdict_audit_preserves_reason;
      Alcotest.test_case "verdict audit names the judging runtime" `Quick
        test_verdict_audit_names_the_judging_runtime;
      Alcotest.test_case "judgements share the fixed authority actor" `Quick
        test_judgements_share_fixed_authority_actor;
    ];
    "workspace_boundary", [
      Alcotest.test_case "raw submit notifies once" `Quick
        test_raw_workspace_submission_notifies_once;
    ];
    "id_generation", [
      Alcotest.test_case "vrf- prefix" `Quick test_generate_id_prefix;
      Alcotest.test_case "10k ids collision-free" `Quick test_generate_id_no_collisions;
    ];
    "notifications", [
      Alcotest.test_case "verdict keeps typed authority" `Quick
        test_verdict_event_preserves_typed_authority;
      Alcotest.test_case "rejected verdict keeps wire type" `Quick
        test_rejected_verdict_event_preserves_wire_type;
      Alcotest.test_case "stalled projection names forward paths" `Quick
        test_stalled_projection_names_forward_paths;
      Alcotest.test_case "stalled metadata keeps typed authority" `Quick
        test_stalled_metadata_preserves_typed_authority;
      Alcotest.test_case "the same stall is posted once" `Quick
        test_the_same_stall_is_posted_once;
      Alcotest.test_case "a different stall still reaches the board" `Quick
        test_a_different_stall_still_reaches_the_board;
    ];
    "storage", [
      Alcotest.test_case "create and load" `Quick test_create_and_load;
      Alcotest.test_case "create rejects blank criterion" `Quick
        test_create_rejects_blank_criterion_before_write;
      Alcotest.test_case "delete request (idempotent)" `Quick test_delete_request;
      Alcotest.test_case "list requests" `Quick test_list_requests;
      Alcotest.test_case "list requests missing dir stays quiet" `Quick
        test_list_requests_missing_dir_stays_quiet;
      Alcotest.test_case "verifications dir resolves active store" `Quick
        test_verifications_dir_resolves_active_store;
      Alcotest.test_case "request path uses current store" `Quick
        test_request_path_uses_current_store;
      Alcotest.test_case "list requests isolates bad entry with metric" `Quick
        test_list_requests_isolates_bad_entry_with_metric;
      Alcotest.test_case "list requests reports every unreadable entry" `Quick
        test_list_requests_reports_every_unreadable_entry;
      Alcotest.test_case "list requests rereads current content" `Quick
        test_list_requests_rereads_current_request_content;
      Alcotest.test_case "submitted evidence authority-scoped and contained" `Quick
        test_submitted_evidence_inspection_is_authority_scoped_and_contained;
      Alcotest.test_case "submit snapshot resolves Docker relative refs" `Quick
        test_submit_snapshot_resolves_docker_relative_artifact_and_explicit_note;
      Alcotest.test_case "submit snapshot is immutable and cwd-independent" `Quick
        test_submit_snapshot_survives_mutation_deletion_and_authority_cwd;
      Alcotest.test_case "submit snapshot rejects traversal and symlink escape" `Quick
        test_submit_snapshot_rejects_relative_traversal_and_symlink_escape;
      Alcotest.test_case "submit snapshot rejects bare and absolute refs" `Quick
        test_submit_snapshot_rejects_bare_and_absolute_references;
      Alcotest.test_case "submitted evidence rejects cross playground" `Quick
        test_submitted_evidence_inspection_rejects_cross_playground_path;
      Alcotest.test_case "submitted evidence bounded UTF-8" `Quick
        test_submitted_evidence_inspection_is_bounded_and_utf8_safe;
      Alcotest.test_case "transport projection omits truncated prefix" `Quick
        test_transport_projection_omits_truncated_prefix;
      Alcotest.test_case "transport projection bounds the evidence total" `Quick
        test_transport_projection_bounds_the_evidence_total;
      Alcotest.test_case "transport projection keeps the most items it can" `Quick
        test_transport_projection_keeps_the_most_items_it_can;
      Alcotest.test_case "truncated snapshot items names over-cap artifacts"
        `Quick
        test_truncated_snapshot_items_names_only_truncated_artifacts;
      Alcotest.test_case "submitted evidence rejects unknown artifact field" `Quick
        test_submitted_evidence_rejects_unknown_artifact_field;
      Alcotest.test_case "submitted evidence rejects malformed UTF-8" `Quick
        test_submitted_evidence_rejects_malformed_utf8;
      Alcotest.test_case "submitted evidence rejects symlink and FIFO" `Quick
        test_submitted_evidence_rejects_symlink_escape_and_fifo;
      Alcotest.test_case "submitted evidence race remains typed" `Quick
        test_changed_during_read_maps_to_typed_unreadable_reason;
      Alcotest.test_case "submitted evidence requires exact assignment identity" `Quick
        test_submitted_evidence_requires_exact_task_assignment_identity;
      Alcotest.test_case "keeper task projection has no evidence or verdict action" `Quick
        test_keeper_task_projection_never_exposes_snapshot_or_verdict_action;
      Alcotest.test_case "an unreadable artifact reaches the judge, not a defer" `Quick
        test_unreadable_artifacts_reach_the_judge;
      Alcotest.test_case "a checkout-relative artifact path is not guessed" `Quick
        test_checkout_relative_artifact_is_not_guessed;
      Alcotest.test_case "an injected artifact read answers the snapshot" `Quick
        test_injected_artifact_read_answers_the_snapshot;
      Alcotest.test_case "the size pre-check uses the injected reader" `Quick
        test_artifact_reference_size_uses_the_injected_reader;
      Alcotest.test_case "the reader routes by where the tree lives" `Quick
        test_reader_routes_by_where_the_tree_lives;
      Alcotest.test_case "an injected reader answers under the text line" `Quick
        test_an_injected_reader_answers_under_the_text_line;
      Alcotest.test_case "a binary payload is adopted and filed" `Quick
        test_a_binary_payload_is_adopted_and_filed;
    ];
  ]
