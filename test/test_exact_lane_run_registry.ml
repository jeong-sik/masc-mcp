open Alcotest
module R = Masc.Exact_lane_run_registry

let remove_if_exists path =
  try Sys.remove path with
  | Sys_error _ -> ()
;;

let mark_completed_exn t ~run_id ~outcome ~elapsed_s ~output =
  match R.mark_completed t ~run_id ~outcome ~elapsed_s ~selected_slot:None ~output with
  | Ok () -> ()
  | Error error ->
    failf
      "exact-lane completion failed: %s"
      (R.completion_error_to_string error)
;;

let mark_completed_with_selected_slot_exn
      t
      ~run_id
      ~outcome
      ~elapsed_s
      ~selected_slot
      ~output
  =
  match
    R.mark_completed
      t
      ~run_id
      ~outcome
      ~elapsed_s
      ~selected_slot
      ~output
  with
  | Ok () -> ()
  | Error error ->
    failf
      "exact-lane completion with slot failed: %s"
      (R.completion_error_to_string error)
;;

let test_round_trip_preserves_exact_evidence () =
  let path = Filename.temp_file "exact-lane-runs-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  R.register_running
    registry
    ~run_id:"run-1"
    ~lane:R.Librarian
    ~actor:"keeper-a"
    ~started_at:10.0
    ~input:(R.Exact_input (`Assoc [ "message_count", `Int 4 ]));
  mark_completed_with_selected_slot_exn
    registry
    ~run_id:"run-1"
    ~outcome:R.Succeeded
    ~elapsed_s:0.5
    ~selected_slot:(Some "librarian-primary")
    ~output:(`Assoc [ "fact_count", `Int 3 ]);
  let original = R.get registry ~run_id:"run-1" |> Option.get |> R.run_to_yojson in
  let replayed = R.replay path in
  let restored = R.get replayed ~run_id:"run-1" |> Option.get |> R.run_to_yojson in
  check string "round trip" (Yojson.Safe.to_string original) (Yojson.Safe.to_string restored);
  let replayed_again = R.replay path in
  check string "second restart keeps the same original evidence"
    (Yojson.Safe.to_string original)
    (R.get replayed_again ~run_id:"run-1" |> Option.get |> R.run_to_yojson
     |> Yojson.Safe.to_string);
  (match R.get replayed ~run_id:"run-1" |> Option.get with
   | { status = R.Completed { selected_slot = Some selected_slot; _ }; _ } ->
     check string "selected slot" "librarian-primary" selected_slot
   | _ -> fail "selected slot did not survive durable replay");
  remove_if_exists path
;;

let test_replay_selects_latest_payloads_across_blank_rows () =
  let path = Filename.temp_file "exact-lane-latest-" ".jsonl" in
  let registry = R.create ~path () in
  let register text =
    R.register_running registry ~run_id:"same-id" ~lane:R.Librarian
      ~actor:"keeper-a" ~started_at:10.0
      ~input:(R.Exact_input (`Assoc [ "prompt", `String text ]))
  in
  register "old input";
  mark_completed_exn registry ~run_id:"same-id" ~outcome:R.Succeeded
    ~elapsed_s:0.5 ~output:(`String "old completion");
  register "새 입력";
  mark_completed_exn registry ~run_id:"same-id"
    ~outcome:(R.Failed { code = "cancelled"; detail = "operator cancelled" })
    ~elapsed_s:0.8 ~output:(`Assoc [ "result", `String "취소된 실행의 원문" ]);
  let expected = R.get registry ~run_id:"same-id" |> Option.get |> R.run_to_yojson in
  let rows = Fs_compat.load_file path |> String.split_on_char '\n' in
  Fs_compat.save_file path ("\n \n" ^ String.concat "\n\n \n" rows);
  for _ = 1 to 2 do
    let replayed = R.replay path in
    check string "latest registration and completion remain paired"
      (Yojson.Safe.to_string expected)
      (R.get replayed ~run_id:"same-id" |> Option.get |> R.run_to_yojson
       |> Yojson.Safe.to_string)
  done;
  let retained =
    Fs_compat.load_file path |> String.split_on_char '\n'
    |> List.filter (fun line -> String.trim line <> "")
  in
  check int "only the retained register and complete survive" 2 (List.length retained);
  remove_if_exists path
;;

let test_completion_without_slot_receipt_writes_explicit_null () =
  let path = Filename.temp_file "exact-lane-null-slot-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  R.register_running
    registry
    ~run_id:"run-no-receipt"
    ~lane:R.Board_attention
    ~actor:"keeper-a"
    ~started_at:10.0
    ~input:(R.Exact_input `Null);
  mark_completed_exn
    registry
    ~run_id:"run-no-receipt"
    ~outcome:R.Succeeded
    ~elapsed_s:0.5
    ~output:`Null;
  let lines = Fs_compat.load_file path |> String.split_on_char '\n' in
  let completion_event = Yojson.Safe.from_string (List.nth lines 1) in
  (match completion_event with
   | `Assoc fields ->
     (match List.assoc_opt "completion" fields with
      | Some (`Assoc completion_fields) ->
        check bool "None is explicit durable evidence" true
          (List.assoc_opt "selected_slot" completion_fields = Some `Null)
      | _ -> fail "completion event must carry an object payload")
   | _ -> fail "completion event must be an object");
  remove_if_exists path
;;

let test_missing_selected_slot_completion_is_not_replayed_as_success () =
  let path = Filename.temp_file "exact-lane-legacy-slot-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  R.register_running
    registry
    ~run_id:"legacy-run"
    ~lane:R.Board_attention
    ~actor:"keeper-a"
    ~started_at:10.0
    ~input:(R.Exact_input `Null);
  mark_completed_exn
    registry
    ~run_id:"legacy-run"
    ~outcome:R.Succeeded
    ~elapsed_s:0.5
    ~output:`Null;
  let lines = Fs_compat.load_file path |> String.split_on_char '\n' in
  let completion_event =
    match Yojson.Safe.from_string (List.nth lines 1) with
    | `Assoc fields ->
      `Assoc
        (List.map
           (fun (name, value) ->
              if String.equal name "completion"
              then (
                match value with
                | `Assoc completion_fields ->
                  name, `Assoc (List.remove_assoc "selected_slot" completion_fields)
                | _ -> fail "completion event must carry an object payload")
              else name, value)
           fields)
    | _ -> fail "completion event must be an object"
  in
  Fs_compat.save_file
    path
    (String.concat
       "\n"
       [ List.nth lines 0; Yojson.Safe.to_string completion_event; "" ]);
  let replayed = R.replay path in
  check
    (option string)
    "pre-v4 completion is rejected and its registration is restart-failed"
    (Some "failed")
    (R.get replayed ~run_id:"legacy-run" |> Option.map (fun run -> R.status_label run.R.status));
  remove_if_exists path
;;

(* #29277. A hard cut leaves rows carrying a field no current decoder reads.
   Such a row only leaves the log through compaction, and [replay] declines to
   compact while one is present — so the store keeps it, and the retention
   bound stops applying to that store. Live evidence on 2026-08-23: 2 000 rows
   in [exact-lane-runs-v4.jsonl] surviving every boot, 36 MB serving zero
   readable runs. This pins that the deployment cut is the way out. *)
let test_hard_cut_artifact_does_not_poison_compaction_forever () =
  let path = Filename.temp_file "exact-lane-hard-cut-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  R.register_running
    registry
    ~run_id:"live-run"
    ~lane:R.Board_attention
    ~actor:"keeper-a"
    ~started_at:10.0
    ~input:(R.Exact_input (`Assoc [ "candidate_id", `String "retained-candidate" ]));
  mark_completed_exn
    registry
    ~run_id:"live-run"
    ~outcome:R.Succeeded
    ~elapsed_s:0.5
    ~output:(`Assoc [ "decision", `String "retained judgment" ]);
  let expected = R.get registry ~run_id:"live-run" |> Option.get |> R.run_to_yojson in
  let live_lines =
    Fs_compat.load_file path
    |> String.split_on_char '\n'
    |> List.filter (fun line -> not (String.equal line ""))
  in
  (* A registration written before the field was cut, same shape the live
     store held. *)
  let artifact =
    match Yojson.Safe.from_string (List.nth live_lines 0) with
    | `Assoc fields ->
      `Assoc
        (List.map
           (fun (name, value) ->
              if String.equal name "id"
              then name, `String "hard-cut-run"
              else if String.equal name "registration"
              then (
                match value with
                | `Assoc registration ->
                  name, `Assoc (("subject_id", `String "s-1") :: registration)
                | _ -> fail "registration event must carry an object payload")
              else name, value)
           fields)
    | _ -> fail "registration event must be an object"
  in
  Fs_compat.save_file
    path
    (String.concat "\n" (live_lines @ [ Yojson.Safe.to_string artifact; "" ]));
  let replayed = R.replay path in
  check
    (option string)
    "the readable run still replays"
    (Some "succeeded")
    (R.get replayed ~run_id:"live-run"
     |> Option.map (fun run -> R.status_label run.R.status));
  let after_replay : Run_registry_core.cut_report =
    R.cut_replay_log ~execute:false path
  in
  check int "replay left the unreadable row on disk" 1 after_replay.malformed_lines;
  let cut : Run_registry_core.cut_report = R.cut_replay_log ~execute:true path in
  check bool "the cut rewrote the store" true cut.rewritten;
  check int "the cut dropped the unreadable row" 1 cut.malformed_lines;
  let after_cut : Run_registry_core.cut_report =
    R.cut_replay_log ~execute:false path
  in
  check int "nothing unreadable is left" 0 after_cut.malformed_lines;
  check int "the readable run survived the cut" 1 after_cut.retained_entries;
  check string "cut preserves the original payload beside an unreadable row"
    (Yojson.Safe.to_string expected)
    (R.replay path |> R.get ~run_id:"live-run" |> Option.get |> R.run_to_yojson
     |> Yojson.Safe.to_string);
  check
    (option string)
    "and still replays"
    (Some "succeeded")
    (R.replay path
     |> R.get ~run_id:"live-run"
     |> Option.map (fun run -> R.status_label run.R.status));
  remove_if_exists path
;;

(* The unterminated-tail guard is the one the cut keeps: a partial read must
   not become a truncating rewrite. *)
let test_cut_refuses_a_store_with_an_unterminated_tail () =
  let path = Filename.temp_file "exact-lane-torn-tail-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  R.register_running
    registry
    ~run_id:"live-run"
    ~lane:R.Board_attention
    ~actor:"keeper-a"
    ~started_at:10.0
    ~input:(R.Exact_input `Null);
  mark_completed_exn
    registry
    ~run_id:"live-run"
    ~outcome:R.Succeeded
    ~elapsed_s:0.5
    ~output:`Null;
  let content = Fs_compat.load_file path in
  Fs_compat.save_file path (content ^ "{\"event\":\"reg");
  let cut : Run_registry_core.cut_report = R.cut_replay_log ~execute:true path in
  check bool "the cut left the store alone" false cut.rewritten;
  (* A caller that only reads [rewritten] cannot tell "nothing needed cutting"
     from "the cut declined". [reached_end] is what separates them, and the
     dry run reports it too — otherwise a deploy step would call this a
     success. *)
  check bool "and says why" false cut.reached_end;
  check
    bool
    "the dry run predicts the refusal"
    false
    (R.cut_replay_log ~execute:false path).reached_end;
  check
    string
    "the bytes are untouched"
    (content ^ "{\"event\":\"reg")
    (Fs_compat.load_file path);
  remove_if_exists path
;;

let test_blank_selected_slot_is_rejected_before_write () =
  let registry = R.create () in
  R.register_running
    registry
    ~run_id:"blank-slot"
    ~lane:R.Librarian
    ~actor:"keeper-a"
    ~started_at:10.0
    ~input:(R.Exact_input `Null);
  let result =
    R.mark_completed
      registry
      ~run_id:"blank-slot"
      ~outcome:R.Succeeded
      ~elapsed_s:0.5
      ~selected_slot:(Some " \t")
      ~output:`Null
  in
  check bool "blank selected slot is a typed writer error" true
    (match result with
     | Error R.Invalid_selected_slot -> true
     | Error R.Unknown_run | Error (R.Persistence_failed _) | Ok () -> false);
  check string "invalid completion leaves the registered run running" "running"
    (R.get registry ~run_id:"blank-slot" |> Option.get |> fun run -> R.status_label run.R.status)
;;

let test_running_shape_has_no_invented_completion () =
  let registry = R.create () in
  R.register_running
    registry
    ~run_id:"run-live"
    ~lane:R.Board_attention
    ~actor:"keeper-a"
    ~started_at:20.0
    ~input:(R.Exact_input `Null);
  let run = R.get registry ~run_id:"run-live" |> Option.get in
  check string "status" "running" (R.status_label run.status);
  match R.run_to_yojson run with
  | `Assoc fields ->
    check bool "no elapsed" false (List.mem_assoc "elapsed_s" fields);
    check bool "no output" false (List.mem_assoc "output" fields)
  | _ -> fail "run serializer must emit an object"
;;

let test_replay_settles_running_as_server_restart_failure () =
  let path = Filename.temp_file "exact-lane-restart-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  R.register_running registry ~run_id:"interrupted-run" ~lane:R.Librarian
    ~actor:"keeper-a" ~started_at:1.0 ~input:(R.Exact_input (`String "older run"));
  mark_completed_exn registry ~run_id:"interrupted-run" ~outcome:R.Succeeded
    ~elapsed_s:0.5 ~output:(`String "completion preceding the latest registration");
  R.register_running
    registry
    ~run_id:"interrupted-run"
    ~lane:R.Librarian
    ~actor:"keeper-a"
    ~started_at:(Time_compat.now () -. 2.0)
    ~input:(R.Exact_input (`Assoc [ "message_count", `Int 4 ]));
  let replayed = R.replay path in
  (match R.get replayed ~run_id:"interrupted-run" with
   | Some
       { status =
           R.Completed
             { outcome = R.Failed { code; detail }
             ; elapsed_s
             ; output
             ; selected_slot
             }
       ; _
       } ->
     check string "typed restart code" "server_restarted" code;
     check string
       "operator detail"
       "exact-output fibers do not survive server restart"
       detail;
     check bool "elapsed time is retained" true (elapsed_s >= 2.0);
     check (option string) "no slot receipt is invented" None selected_slot;
     check
       string
       "durable output names the interruption"
       "server_restarted"
       (Yojson.Safe.Util.member "reason" output |> Yojson.Safe.Util.to_string)
   | Some run ->
     failf "replayed run stayed non-terminal: %s" (R.status_label run.status)
   | None -> fail "replayed running exact lane disappeared");
  let replayed_again = R.replay path in
  let second = R.get replayed_again ~run_id:"interrupted-run" |> Option.get in
  check bool "latest registration input survives both restarts" true
    (second.input = R.Exact_input (`Assoc [ "message_count", `Int 4 ]));
  (match second.status with
   | R.Completed { output; _ } ->
     check string "restart completion body survives the second compaction"
       "server_restarted"
       (Yojson.Safe.Util.member "reason" output |> Yojson.Safe.Util.to_string)
   | _ -> fail "restart must remain terminal");
  check
    (option string)
    "the synthesized terminal event survives another replay"
    (Some "failed")
    (R.get replayed_again ~run_id:"interrupted-run"
     |> Option.map (fun run -> R.status_label run.R.status));
  remove_if_exists path
;;

let test_current_storage_generation () =
  check string "current store file" "exact-lane-runs-v5.jsonl" R.storage_filename
;;

(* The registration decoder is exact-field, so removing a field from it makes
   every row written with that field unreadable, and the store then never
   compacts again (#29598 did this to v4: 2,000 of 4,090 live rows skipped on
   every boot). A removed field has to ride on the store version. This test
   holds the two together: the fixture below is a full v5 registration row, so
   a decoder that stops accepting one of its keys fails here, and the fix is
   to change the fixture and [storage_filename] in the same commit. *)
let v5_registration_row =
  {|{"event":"register","id":"exact-board-attention-pin","started_at":30.0,"registration":{"lane":"board_attention_exact","actor":"keeper-a","input":{"kind":"exact","payload":{"candidate_id":"c"}}}}|}

(* The same row as v4 wrote it: [subject_id] inside the registration. *)
let v4_registration_row =
  {|{"event":"register","id":"exact-board-attention-pin","started_at":30.0,"registration":{"lane":"board_attention_exact","subject_id":"s","actor":"keeper-a","input":{"kind":"exact","payload":{"candidate_id":"c"}}}}|}

let test_store_version_pins_the_registration_shape () =
  (* Reads the decoder's verdict on the row, not only whether an ID survives
     replay. A registration that decodes is restart-failed into a terminal row,
     while a refused row is absent. [cut_replay_log] reports what the same
     decoder read and counts what it refused. *)
  let malformed_lines row =
    let path = Filename.temp_file "exact-lane-shape-" ".jsonl" in
    Fs_compat.save_file path (row ^ "\n");
    let report = R.cut_replay_log ~execute:false path in
    remove_if_exists path;
    report.Run_registry_core.lines_read, report.Run_registry_core.malformed_lines
  in
  check string "the row shape below belongs to this store version"
    "exact-lane-runs-v5.jsonl" R.storage_filename;
  check (pair int int) "a v5 registration row is read and accepted"
    (1, 0) (malformed_lines v5_registration_row);
  check (pair int int) "the field v4 carried and v5 removed is rejected, not ignored"
    (1, 1) (malformed_lines v4_registration_row)
;;

(* The retained-run bound exists to serve the internal-agents monitor, which
   pages backwards through this store with a cursor. A bound below that route's
   maximum page size would let the operator's "older" request walk off the end
   of the store, so the relation is pinned here rather than left to the comment
   that derives it: raising exact_lane_run_page_max without revisiting the
   retention fails this. *)
let monitor_pages_retained = 10

let test_retention_is_derived_from_the_monitor_page_size () =
  let page_max = Server_routes_http_routes_dashboard.exact_lane_run_page_max in
  check bool "retention holds whole pages, not a fraction of one" true
    (R.max_completed_retained >= page_max);
  check int "retention is the page maximum times the pages the monitor keeps"
    (page_max * monitor_pages_retained)
    R.max_completed_retained
;;

(* The bound is applied, not merely declared. In-memory registry so writing
   past it stays cheap. *)
let test_completed_runs_are_bounded () =
  let registry = R.create () in
  let total = R.max_completed_retained + 8 in
  for index = 1 to total do
    let run_id = Printf.sprintf "run-%05d" index in
    R.register_running
      registry
      ~run_id
      ~lane:R.Librarian
      ~actor:"keeper-a"
      ~started_at:(float_of_int index)
      ~input:(R.Exact_input (`Assoc [ "index", `Int index ]));
    mark_completed_exn
      registry
      ~run_id
      ~outcome:R.Succeeded
      ~elapsed_s:0.1
      ~output:(`Assoc [ "index", `Int index ])
  done;
  check int "completed runs are bounded"
    R.max_completed_retained
    (List.length (R.list_runs registry));
  let has run_id =
    List.exists (fun (run : R.run) -> String.equal run.R.run_id run_id)
      (R.list_runs registry)
  in
  check bool "the newest completed run is retained" true
    (has (Printf.sprintf "run-%05d" total));
  check bool "the oldest completed run is evicted" false (has "run-00001")
;;

(* Lane audit W8: the retention bound is per lane. Under the old global
   bound the busiest lane (librarian, every few turns per keeper) evicted
   the quietest lane's entire history. The three Board-attention runs here
   are OLDER than every librarian run and must survive a librarian overflow. *)
let test_a_busy_lane_cannot_evict_a_quiet_lanes_history () =
  let registry = R.create () in
  let record ~run_id ~lane ~started_at =
    R.register_running
      registry
      ~run_id
      ~lane
      ~actor:"keeper-a"
      ~started_at
      ~input:(R.Exact_input (`Assoc []));
    mark_completed_exn
      registry
      ~run_id
      ~outcome:R.Succeeded
      ~elapsed_s:0.1
      ~output:(`Assoc [])
  in
  for index = 1 to 3 do
    record
      ~run_id:(Printf.sprintf "board-%02d" index)
      ~lane:R.Board_attention
      ~started_at:(float_of_int index)
  done;
  for index = 1 to R.max_completed_retained + 8 do
    record
      ~run_id:(Printf.sprintf "librarian-%05d" index)
      ~lane:R.Librarian
      ~started_at:(float_of_int (100 + index))
  done;
  let runs = R.list_runs registry in
  let count lane =
    List.length (List.filter (fun (run : R.run) -> run.R.lane = lane) runs)
  in
  check int "the quiet lane's whole history survives" 3 (count R.Board_attention);
  check int "the busy lane is bounded to its own quota"
    R.max_completed_retained
    (count R.Librarian)
;;

let test_exact_history_is_not_pruned_across_lanes () =
  let path = Filename.temp_file "exact-lane-runs-all-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  let lanes = Array.of_list R.all_lanes in
  List.init 80 Fun.id
  |> List.iter (fun index ->
    let run_id = Printf.sprintf "run-%02d" index in
    let lane =
      lanes.(index mod Array.length lanes)
    in
    R.register_running
      registry
      ~run_id
      ~lane
      ~actor:"keeper-a"
      ~started_at:(float_of_int index)
      ~input:(R.Exact_input (`Assoc [ "index", `Int index ]));
    mark_completed_exn
      registry
      ~run_id
      ~outcome:R.Succeeded
      ~elapsed_s:0.1
      ~output:(`Assoc [ "index", `Int index ]));
  let replayed = R.replay path in
  check int "all exact runs survive replay" 80 (List.length (R.list_runs replayed));
  check
    (list string)
    "every registered lane survives replay"
    (R.all_lanes |> List.map R.lane_key |> List.sort String.compare)
    (R.list_runs replayed
     |> List.map (fun (run : R.run) -> R.lane_key run.lane)
     |> List.sort_uniq String.compare);
  let permissions = (Unix.stat path).Unix.st_perm land 0o777 in
  check int "durable registry is private" 0o600 permissions;
  remove_if_exists path
;;

let test_all_lanes_matches_the_independent_constructor_oracle () =
  let expected =
    [ R.Librarian
    ; R.Hitl_auto_judge
    ; R.Board_attention
    ]
  in
  check
    (list string)
    "all_lanes is the complete ordered constructor enumeration"
    (List.map R.lane_key expected)
    (List.map R.lane_key R.all_lanes)
;;

let test_failed_durable_registration_is_not_published_in_memory () =
  let directory = Filename.temp_dir "exact-lane-runs-dir-" "" in
  let registry = R.create ~path:directory () in
  let failed =
    try
      R.register_running
        registry
        ~run_id:"not-published"
        ~lane:R.Librarian
        ~actor:"keeper-a"
        ~started_at:1.0
        ~input:(R.Exact_input `Null);
      false
    with
    | Sys_error _ | Unix.Unix_error _ -> true
  in
  check bool "directory cannot be used as durable JSONL" true failed;
  check int "failed registration absent" 0 (List.length (R.list_runs registry));
  Unix.rmdir directory
;;

let test_failed_durable_completion_is_explicitly_visible () =
  let path = Filename.temp_file "exact-lane-completion-failure-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  R.register_running
    registry
    ~run_id:"completion-not-published"
    ~lane:R.Librarian
    ~actor:"keeper-a"
    ~started_at:1.0
    ~input:(R.Exact_input `Null);
  Sys.remove path;
  Unix.mkdir path 0o700;
  let completion =
    R.mark_completed
      registry
      ~run_id:"completion-not-published"
      ~outcome:(R.Failed { code = "model_error"; detail = "typed failure detail" })
      ~elapsed_s:0.1
      ~selected_slot:None
      ~output:(`String "must-not-publish")
  in
  (match completion with
   | Error (R.Persistence_failed failure) ->
     check bool "failure retains durable detail" true
       (String.trim failure.detail <> "")
   | Error R.Unknown_run -> fail "registered run became unknown"
   | Error R.Invalid_selected_slot -> fail "explicit None slot became invalid"
   | Ok () -> fail "directory unexpectedly received durable completion");
  let run = R.get registry ~run_id:"completion-not-published" |> Option.get in
  check bool "failed completion is not reported as running" true
    (not (String.equal "running" (R.status_label run.status)));
  (match run.status with
   | R.Completion_persistence_failed
       { intended_outcome = R.Failed { code; detail }
       ; output = `String output
       ; failure
       ; _
       } ->
     check string "intended output remains observable" "must-not-publish" output;
     check string "intended failure code remains observable" "model_error" code;
     check string "intended failure detail remains observable" "typed failure detail" detail;
     check bool "persistence failure remains explicit" true
       (String.trim failure.detail <> "")
   | _ -> fail "expected explicit completion persistence failure");
  (match R.run_to_yojson run with
   | `Assoc fields ->
     check bool "serialized persistence error" true
       (List.mem_assoc "persistence_error" fields);
     check bool "serialized persistence state" true
       (List.mem_assoc "persistence_state" fields);
     check (option string) "serialized intended failure code" (Some "model_error")
       (Option.bind
          (List.assoc_opt "intended_code" fields)
          (function `String value -> Some value | _ -> None));
     check (option string) "serialized intended failure detail" (Some "typed failure detail")
       (Option.bind
          (List.assoc_opt "intended_detail" fields)
          (function `String value -> Some value | _ -> None))
   | _ -> fail "run serializer must emit an object");
  Unix.rmdir path
;;

let test_observation_reads_do_not_wait_for_durable_writer () =
  let path = Filename.temp_file "exact-lane-read-projection-" ".jsonl" in
  let registry = R.create ~path () in
  let ready_read, ready_write = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 ->
    Unix.close ready_read;
    (try
       let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0 in
       Unix.lockf fd Unix.F_LOCK 0;
       ignore (Unix.write_substring ready_write "x" 0 1 : int);
       Unix.sleepf 0.5;
       Unix.close fd;
       Unix._exit 0
     with
     | _ -> Unix._exit 2)
  | child ->
    Unix.close ready_write;
    let ready = Bytes.create 1 in
    ignore (Unix.read ready_read ready 0 1 : int);
    Unix.close ready_read;
    Fun.protect
      ~finally:(fun () ->
        let rec wait_child () =
          try Unix.waitpid [] child with
          | Unix.Unix_error (Unix.EINTR, _, _) -> wait_child ()
        in
        (match wait_child () with
         | _, Unix.WEXITED 0 -> ()
         | _, status ->
           failf
             "durable-lock child failed: %s"
             (match status with
              | Unix.WEXITED code -> Printf.sprintf "exit %d" code
              | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
              | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal));
        remove_if_exists path;
        remove_if_exists (Fs_compat.private_jsonl_lock_path path))
      (fun () ->
         Eio_main.run @@ fun env ->
         let clock = Eio.Stdenv.clock env in
         Eio.Switch.run @@ fun sw ->
         let started, set_started = Eio.Promise.create () in
         Eio.Fiber.fork ~sw (fun () ->
           Eio.Promise.resolve set_started ();
           R.register_running
             registry
             ~run_id:"writer-blocked-on-durable-lock"
             ~lane:R.Board_attention
             ~actor:"keeper-a"
             ~started_at:1.0
             ~input:(R.Exact_input `Null));
         Eio.Promise.await started;
         Eio.Time.sleep clock 0.05;
         let read_started_at = Eio.Time.now clock in
         let visible = R.list_runs registry in
         let read_elapsed_s = Eio.Time.now clock -. read_started_at in
         check int "pre-commit projection remains unchanged" 0 (List.length visible);
         check bool "Atomic projection read does not wait for durable writer" true
           (read_elapsed_s < 0.2))
;;

(* Paging exists because listing everything serialized 5,908 runs to 246 MB on
   every load. The properties that make a page trustworthy are that it is a
   total order (so a boundary cannot lose a run) and that a summary carries no
   payload (so the size that forced paging cannot creep back). *)
let test_pages_are_a_total_order_over_equal_timestamps () =
  let registry = R.create () in
  List.iter
    (fun run_id ->
       R.register_running
         registry
         ~run_id
         ~lane:R.Librarian
         ~actor:"keeper-a"
         ~started_at:10.0
         ~input:(R.Exact_input (`Assoc [ "n", `Int 1 ])))
    [ "run-a"; "run-b"; "run-c"; "run-d" ];
  let first = R.recent_runs registry ~limit:2 ~before:None in
  check int "page size honoured" 2 (List.length first.runs);
  check int "total counts every retained run, not the page" 4 first.total;
  check bool "more remains" true first.has_more;
  let last = List.nth first.runs 1 in
  let second =
    R.recent_runs registry ~limit:2 ~before:(Some (last.R.started_at, last.R.run_id))
  in
  check int "second page size" 2 (List.length second.runs);
  check bool "no more after the last page" false second.has_more;
  let ids page = List.map (fun (run : R.run) -> run.R.run_id) page in
  let seen = ids first.runs @ ids second.runs in
  check int "every run appears exactly once across pages" 4 (List.length (List.sort_uniq String.compare seen));
  check
    (list string)
    "identical started_at is ordered by run_id, newest first"
    [ "run-d"; "run-c"; "run-b"; "run-a" ]
    seen
;;

let test_summary_carries_no_payload () =
  let registry = R.create () in
  R.register_running
    registry
    ~run_id:"run-1"
    ~lane:R.Librarian
    ~actor:"keeper-a"
    ~started_at:10.0
    ~input:(R.Exact_input (`Assoc [ "conversation_history", `String "…megabytes…" ]));
  mark_completed_exn
    registry
    ~run_id:"run-1"
    ~outcome:R.Succeeded
    ~elapsed_s:0.5
    ~output:(`Assoc [ "fact_count", `Int 3 ]);
  let run = R.get registry ~run_id:"run-1" |> Option.get in
  let field name json =
    match json with
    | `Assoc fields -> List.assoc_opt name fields
    | _ -> None
  in
  let summary = R.run_summary_to_yojson run in
  let detail = R.run_to_yojson run in
  check bool "summary omits input" true (Option.is_none (field "input" summary));
  check bool "summary omits output" true (Option.is_none (field "output" summary));
  check bool "summary does not invent a subject" true (field "subject_id" summary = Some `Null);
  check bool "detail keeps input" true (Option.is_some (field "input" detail));
  check bool "detail keeps output" true (Option.is_some (field "output" detail));
  check bool "summary still identifies the run" true (Option.is_some (field "run_id" summary))
;;

(* The projection dropping the payload was never the question -- it already
   did. What held 498 MB of live heap on this fleet was the store underneath
   it keeping every retained row whole, to serve a detail view that reads one
   at a time (measured 2026-09-05).

   This weighs the store rather than reading a field, because a field can be
   `Null while the bytes are still reachable from somewhere else. *)
let test_the_store_does_not_hold_the_payloads_it_retains () =
  let path = Filename.temp_file "exact-lane-weight-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  let payload_bytes = 200_000 in
  let runs = 40 in
  for i = 1 to runs do
    let run_id = Printf.sprintf "run-%d" i in
    R.register_running
      registry
      ~run_id
      ~lane:R.Librarian
      ~actor:"keeper-a"
      ~started_at:(float_of_int i)
      ~input:(R.Exact_input (`Assoc [ "prompt", `String (String.make payload_bytes 'A') ]));
    mark_completed_exn
      registry
      ~run_id
      ~outcome:R.Succeeded
      ~elapsed_s:1.0
      ~output:(`Assoc [ "reply", `String (String.make payload_bytes 'B') ])
  done;
  Gc.full_major ();
  let held = Obj.reachable_words (Obj.repr registry) * (Sys.word_size / 8) in
  let written = runs * payload_bytes * 2 in
  check bool
    (Printf.sprintf
       "the store holds %d bytes for %d bytes of payload"
       held
       written)
    true
    (held < written / 10);
  (* And the payloads are still there to be read, one at a time. *)
  check bool "a detail read still gets the whole input" true
    (match (R.get registry ~run_id:"run-7" |> Option.get).R.input with
     | R.Exact_input (`Assoc [ "prompt", `String s ]) -> String.length s = payload_bytes
     | _ -> false);
  for _ = 1 to 2 do
    let replayed = R.replay path in
    Gc.full_major ();
    let held = Obj.reachable_words (Obj.repr replayed) * (Sys.word_size / 8) in
    check bool "replay retains metadata without retaining the full payloads" true
      (held < written / 10);
    let detail = R.get replayed ~run_id:"run-7" |> Option.get in
    check bool "full input remains readable after compaction" true
      (detail.input = R.Exact_input (`Assoc [ "prompt", `String (String.make payload_bytes 'A') ]));
    check bool "full output remains readable after compaction" true
      (match detail.status with
       | R.Completed { output; _ } ->
         output = `Assoc [ "reply", `String (String.make payload_bytes 'B') ]
       | _ -> false)
  done;
  remove_if_exists path
;;

let test_projected_runs_omit_payload_in_memory () =
  let path = Filename.temp_file "exact-lane-proj-" ".jsonl" in
  remove_if_exists path;
  let registry = R.create ~path () in
  R.register_running
    registry
    ~run_id:"run-large"
    ~lane:R.Librarian
    ~actor:"keeper-a"
    ~started_at:10.0
    ~input:(R.Exact_input (`Assoc [ "prompt", `String (String.make 1000 'A') ]));
  (* Verify an active Running run has stripped input in projection *)
  let running_listed = List.hd (R.list_runs registry) in
  check bool "running listed run input is stripped" true
    (match running_listed.R.input with R.Exact_input `Null -> true | _ -> false);
  check bool "running get preserves full input" true
    (match (R.get registry ~run_id:"run-large" |> Option.get).R.input with
     | R.Exact_input (`Assoc [ "prompt", `String s ]) -> String.length s = 1000
     | _ -> false);
  mark_completed_exn
    registry
    ~run_id:"run-large"
    ~outcome:R.Succeeded
    ~elapsed_s:0.5
    ~output:(`Assoc [ "completion", `String (String.make 1000 'B') ]);
  let listed_run = List.hd (R.list_runs registry) in
  check bool "listed run input is stripped" true
    (match listed_run.R.input with R.Exact_input `Null -> true | _ -> false);
  check bool "listed run output is stripped" true
    (match listed_run.R.status with R.Completed { output = `Null; _ } -> true | _ -> false);
  let paged_run = List.hd (R.recent_runs registry ~limit:1 ~before:None).runs in
  check bool "paged run input is stripped" true
    (match paged_run.R.input with R.Exact_input `Null -> true | _ -> false);
  check bool "paged run output is stripped" true
    (match paged_run.R.status with R.Completed { output = `Null; _ } -> true | _ -> false);
  (* Non-existent run fast-rejects to None *)
  check bool "non-existent run returns None" true
    (Option.is_none (R.get registry ~run_id:"run-nonexistent"));
  let detail_run = R.get registry ~run_id:"run-large" |> Option.get in
  check bool "get run detail preserves full input from disk" true
    (match detail_run.R.input with
     | R.Exact_input (`Assoc [ "prompt", `String s ]) -> String.length s = 1000
     | _ -> false);
  check bool "get run detail preserves full output from disk" true
    (match detail_run.R.status with
     | R.Completed { output = `Assoc [ "completion", `String s ]; _ } -> String.length s = 1000
     | _ -> false);
  remove_if_exists path
;;

let () =
  run
    "exact_lane_run_registry"
    [ ( "registry"
      , [ test_case "durable exact evidence" `Quick test_round_trip_preserves_exact_evidence
        ; test_case "latest payloads survive blank rows and repeated replay" `Quick
            test_replay_selects_latest_payloads_across_blank_rows
        ; test_case "missing receipt is explicit null" `Quick
            test_completion_without_slot_receipt_writes_explicit_null
        ; test_case "pre-v4 completion is not replayed as success" `Quick
            test_missing_selected_slot_completion_is_not_replayed_as_success
        ; test_case "blank selected slot is rejected before write" `Quick
            test_blank_selected_slot_is_rejected_before_write
        ; test_case "hard-cut artifact does not poison compaction forever" `Quick
            test_hard_cut_artifact_does_not_poison_compaction_forever
        ; test_case "cut refuses a store with an unterminated tail" `Quick
            test_cut_refuses_a_store_with_an_unterminated_tail
        ; test_case "running shape" `Quick test_running_shape_has_no_invented_completion
        ; test_case "restart settles running lane" `Quick
            test_replay_settles_running_as_server_restart_failure
        ; test_case "current storage generation" `Quick test_current_storage_generation
        ; test_case "store version pins the registration shape" `Quick
            test_store_version_pins_the_registration_shape
        ; test_case "exact history is not cross-lane pruned" `Quick
            test_exact_history_is_not_pruned_across_lanes
        ; Alcotest.test_case
            "a busy lane cannot evict a quiet lane's history"
            `Quick
            test_a_busy_lane_cannot_evict_a_quiet_lanes_history
        ; test_case "all lanes matches independent constructor oracle" `Quick
            test_all_lanes_matches_the_independent_constructor_oracle
        ; test_case "retention is derived from the monitor page size" `Quick
            test_retention_is_derived_from_the_monitor_page_size
        ; test_case "completed runs are bounded" `Quick
            test_completed_runs_are_bounded
        ; test_case "failed durable registration is not published" `Quick
            test_failed_durable_registration_is_not_published_in_memory
        ; test_case "failed durable completion is explicitly visible" `Quick
            test_failed_durable_completion_is_explicitly_visible
        ; test_case "observation reads do not wait for durable writer" `Quick
            test_observation_reads_do_not_wait_for_durable_writer
        ; test_case "pages are a total order over equal timestamps" `Quick
            test_pages_are_a_total_order_over_equal_timestamps
        ; test_case "summary carries no payload" `Quick test_summary_carries_no_payload
        ; test_case "projected runs omit payload in memory" `Quick
            test_projected_runs_omit_payload_in_memory
        ; test_case "the store does not hold the payloads it retains" `Quick
            test_the_store_does_not_hold_the_payloads_it_retains
        ] )
    ]
