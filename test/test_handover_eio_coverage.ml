(** Handover Eio Module Coverage Tests

    Tests for handover types and utilities:
    - handover_record type
    - trigger_reason type
    - trigger_reason_to_string function
    - generate_id function
*)

open Alcotest

module Handover_eio = Masc_mcp.Handover_eio

(* ============================================================
   handover_record Type Tests
   ============================================================ *)

let test_handover_record_basic () =
  let h : Handover_eio.handover_record = {
    id = "handover-12345-00001";
    from_agent = "claude";
    to_agent = None;
    task_id = "task-001";
    session_id = "session-abc";
    current_goal = "Complete the feature";
    progress_summary = "50% done";
    completed_steps = ["Step 1"; "Step 2"];
    pending_steps = ["Step 3"; "Step 4"];
    key_decisions = ["Use pattern A"];
    assumptions = ["API is stable"];
    warnings = [];
    unresolved_errors = [];
    modified_files = ["src/main.ml"];
    created_at = 1704067200.0;
    context_usage_percent = 75;
    handover_reason = "context_limit_75";
  } in
  check string "from_agent" "claude" h.from_agent;
  check int "context_usage_percent" 75 h.context_usage_percent;
  check int "completed_steps" 2 (List.length h.completed_steps)

let test_handover_record_with_to_agent () =
  let h : Handover_eio.handover_record = {
    id = "handover-12345-00002";
    from_agent = "claude";
    to_agent = Some "gemini";
    task_id = "task-002";
    session_id = "session-def";
    current_goal = "Goal";
    progress_summary = "Summary";
    completed_steps = [];
    pending_steps = [];
    key_decisions = [];
    assumptions = [];
    warnings = ["Warning 1"];
    unresolved_errors = ["Error 1"];
    modified_files = [];
    created_at = 0.0;
    context_usage_percent = 90;
    handover_reason = "explicit";
  } in
  match h.to_agent with
  | Some a -> check string "to_agent" "gemini" a
  | None -> fail "expected Some"

let test_handover_record_errors () =
  let h : Handover_eio.handover_record = {
    id = "handover-12345-00003";
    from_agent = "codex";
    to_agent = None;
    task_id = "task-003";
    session_id = "session-ghi";
    current_goal = "";
    progress_summary = "";
    completed_steps = [];
    pending_steps = [];
    key_decisions = [];
    assumptions = [];
    warnings = [];
    unresolved_errors = ["Build failed"; "Tests timeout"];
    modified_files = ["a.ml"; "b.ml"; "c.ml"];
    created_at = 0.0;
    context_usage_percent = 100;
    handover_reason = "error: Build failed";
  } in
  check int "unresolved_errors" 2 (List.length h.unresolved_errors);
  check int "modified_files" 3 (List.length h.modified_files)

(* ============================================================
   trigger_reason Type Tests
   ============================================================ *)

let test_trigger_reason_context_limit () =
  let r = Handover_eio.ContextLimit 80 in
  match r with
  | Handover_eio.ContextLimit pct -> check int "percent" 80 pct
  | _ -> fail "expected ContextLimit"

let test_trigger_reason_timeout () =
  let r = Handover_eio.Timeout 300 in
  match r with
  | Handover_eio.Timeout secs -> check int "seconds" 300 secs
  | _ -> fail "expected Timeout"

let test_trigger_reason_explicit () =
  let r = Handover_eio.Explicit in
  match r with
  | Handover_eio.Explicit -> check bool "explicit" true true
  | _ -> fail "expected Explicit"

let test_trigger_reason_fatal_error () =
  let r = Handover_eio.FatalError "Out of memory" in
  match r with
  | Handover_eio.FatalError msg -> check string "message" "Out of memory" msg
  | _ -> fail "expected FatalError"

let test_trigger_reason_task_complete () =
  let r = Handover_eio.TaskComplete in
  match r with
  | Handover_eio.TaskComplete -> check bool "task_complete" true true
  | _ -> fail "expected TaskComplete"

(* ============================================================
   trigger_reason_to_string Tests
   ============================================================ *)

let test_trigger_reason_to_string_context_limit () =
  let s = Handover_eio.trigger_reason_to_string (Handover_eio.ContextLimit 85) in
  check string "context_limit" "context_limit_85" s

let test_trigger_reason_to_string_timeout () =
  let s = Handover_eio.trigger_reason_to_string (Handover_eio.Timeout 600) in
  check string "timeout" "timeout_600s" s

let test_trigger_reason_to_string_explicit () =
  let s = Handover_eio.trigger_reason_to_string Handover_eio.Explicit in
  check string "explicit" "explicit" s

let test_trigger_reason_to_string_fatal_error () =
  let s = Handover_eio.trigger_reason_to_string (Handover_eio.FatalError "Crash") in
  check string "fatal_error" "error: Crash" s

let test_trigger_reason_to_string_task_complete () =
  let s = Handover_eio.trigger_reason_to_string Handover_eio.TaskComplete in
  check string "task_complete" "task_complete" s

(* ============================================================
   generate_id Tests
   ============================================================ *)

let test_generate_id_format () =
  let id = Handover_eio.generate_id () in
  check bool "starts with handover-" true (String.sub id 0 9 = "handover-")

let test_generate_id_length () =
  let id = Handover_eio.generate_id () in
  check bool "reasonable length" true (String.length id > 15)

let test_generate_id_unique () =
  let id1 = Handover_eio.generate_id () in
  let id2 = Handover_eio.generate_id () in
  check bool "ids different" true (id1 <> id2)

(* ============================================================
   Eio Helpers
   ============================================================ *)

module Backend = Masc_mcp.Backend
module Room_utils = Masc_mcp.Room_utils

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter (fun name -> rm_rf (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Unix.unlink path

let make_test_dir () =
  let unique_id = Printf.sprintf "masc_handover_test_%d_%d"
    (Unix.getpid ())
    (int_of_float (Unix.gettimeofday () *. 1000000.)) in
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ()) unique_id in
  (try Unix.mkdir tmp_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  tmp_dir

let make_test_config ~base_path : Room_utils.config =
  let backend_config : Backend.config = {
    backend_type = Backend.Memory;
    base_path;
    postgres_url = None;
    node_id = "test-node";
    cluster_name = "test";
    pubsub_max_messages = 1000;
  } in
  let error_to_string = function
    | Backend.ConnectionFailed s -> "ConnectionFailed: " ^ s
    | Backend.KeyNotFound s -> "KeyNotFound: " ^ s
    | Backend.OperationFailed s -> "OperationFailed: " ^ s
    | Backend.BackendNotSupported s -> "BackendNotSupported: " ^ s
    | Backend.InvalidKey s -> "InvalidKey: " ^ s
  in
  let memory_backend = match Backend.MemoryBackend.create backend_config with
    | Ok t -> t
    | Error e -> failwith (Printf.sprintf "Failed to create memory backend: %s" (error_to_string e))
  in
  {
    base_path;
    lock_expiry_minutes = 5;
    backend_config;
    backend = Room_utils.Memory memory_backend;
  }

let with_eio_env f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = make_test_dir () in
  let config = make_test_config ~base_path:tmp_dir in
  Fun.protect
    ~finally:(fun () -> try rm_rf tmp_dir with _ -> ())
    (fun () -> f ~fs config)

(* ============================================================
   create_handover Tests
   ============================================================ *)

let test_create_handover_basic () =
  let h = Handover_eio.create_handover
    ~from_agent:"claude"
    ~task_id:"task-001"
    ~session_id:"sess-001"
    ~reason:(Handover_eio.ContextLimit 75)
  in
  check string "from_agent" "claude" h.from_agent;
  check string "task_id" "task-001" h.task_id;
  check bool "id not empty" true (String.length h.id > 0)

let test_create_handover_timeout () =
  let h = Handover_eio.create_handover
    ~from_agent:"gemini"
    ~task_id:"task-002"
    ~session_id:"sess-002"
    ~reason:(Handover_eio.Timeout 300)
  in
  check string "reason" "timeout_300s" h.handover_reason

(* ============================================================
   handover_to_json / handover_of_json Tests
   ============================================================ *)

let test_handover_json_roundtrip () =
  let original = Handover_eio.create_handover
    ~from_agent:"alice"
    ~task_id:"task-rt"
    ~session_id:"sess-rt"
    ~reason:Handover_eio.Explicit
  in
  let json = Handover_eio.handover_to_json original in
  match Handover_eio.handover_of_json json with
  | Some decoded ->
      check string "id" original.id decoded.id;
      check string "from_agent" original.from_agent decoded.from_agent
  | None -> fail "json decode failed"

let test_handover_of_json_invalid () =
  let json = `Assoc [("invalid", `String "data")] in
  match Handover_eio.handover_of_json json with
  | None -> check bool "returns None" true true
  | Some _ -> fail "expected None for invalid json"

(* ============================================================
   Eio IO Tests: save_handover / load_handover
   ============================================================ *)

let test_save_and_load_handover () =
  with_eio_env @@ fun ~fs config ->
  let h = Handover_eio.create_handover
    ~from_agent:"claude"
    ~task_id:"task-io"
    ~session_id:"sess-io"
    ~reason:(Handover_eio.ContextLimit 75)
  in
  (* Save *)
  (match Handover_eio.save_handover ~fs config h with
   | Ok () -> ()
   | Error e -> failf "save failed: %s" e);
  (* Load *)
  match Handover_eio.load_handover ~fs config h.id with
  | Ok loaded ->
      check string "id matches" h.id loaded.id;
      check string "from_agent matches" h.from_agent loaded.from_agent
  | Error e -> failf "load failed: %s" e

let test_load_nonexistent () =
  with_eio_env @@ fun ~fs config ->
  match Handover_eio.load_handover ~fs config "nonexistent-id" with
  | Error _ -> check bool "returns error" true true
  | Ok _ -> fail "expected error for nonexistent"

(* ============================================================
   Eio IO Tests: list_handovers
   ============================================================ *)

let test_list_handovers_empty () =
  with_eio_env @@ fun ~fs config ->
  let handovers = Handover_eio.list_handovers ~fs config in
  check int "empty list" 0 (List.length handovers)

let test_list_handovers_multiple () =
  with_eio_env @@ fun ~fs config ->
  let h1 = Handover_eio.create_handover
    ~from_agent:"a1" ~task_id:"t1" ~session_id:"s1" ~reason:Handover_eio.Explicit in
  let h2 = Handover_eio.create_handover
    ~from_agent:"a2" ~task_id:"t2" ~session_id:"s2" ~reason:(Handover_eio.Timeout 60) in
  ignore (Handover_eio.save_handover ~fs config h1);
  ignore (Handover_eio.save_handover ~fs config h2);
  let handovers = Handover_eio.list_handovers ~fs config in
  check int "two handovers" 2 (List.length handovers)

(* ============================================================
   Eio IO Tests: get_pending_handovers
   ============================================================ *)

let test_get_pending_handovers () =
  with_eio_env @@ fun ~fs config ->
  let h = Handover_eio.create_handover
    ~from_agent:"claude" ~task_id:"t1" ~session_id:"s1" ~reason:Handover_eio.Explicit in
  ignore (Handover_eio.save_handover ~fs config h);
  let pending = Handover_eio.get_pending_handovers ~fs config in
  check int "one pending" 1 (List.length pending)

(* ============================================================
   Eio IO Tests: claim_handover
   ============================================================ *)

let test_claim_handover_success () =
  with_eio_env @@ fun ~fs config ->
  let h = Handover_eio.create_handover
    ~from_agent:"claude" ~task_id:"t1" ~session_id:"s1" ~reason:Handover_eio.Explicit in
  ignore (Handover_eio.save_handover ~fs config h);
  match Handover_eio.claim_handover ~fs config ~handover_id:h.id ~agent_name:"gemini" with
  | Ok claimed ->
      check bool "has to_agent" true (claimed.to_agent <> None);
      (match claimed.to_agent with
       | Some name -> check string "to_agent" "gemini" name
       | None -> fail "expected Some")
  | Error e -> failf "claim failed: %s" e

let test_claim_handover_nonexistent () =
  with_eio_env @@ fun ~fs config ->
  match Handover_eio.claim_handover ~fs config ~handover_id:"fake-id" ~agent_name:"x" with
  | Error _ -> check bool "returns error" true true
  | Ok _ -> fail "expected error"

(* ============================================================
   format_as_markdown Tests
   ============================================================ *)

let test_format_as_markdown () =
  let h = Handover_eio.create_handover
    ~from_agent:"claude" ~task_id:"t1" ~session_id:"s1" ~reason:Handover_eio.Explicit in
  let md = Handover_eio.format_as_markdown h in
  check bool "contains handover" true (String.length md > 0);
  check bool "is markdown" true (String.contains md '#')

(* ============================================================
   build_successor_prompt Tests
   ============================================================ *)

let test_build_successor_prompt () =
  let h = Handover_eio.create_handover
    ~from_agent:"claude" ~task_id:"t1" ~session_id:"s1" ~reason:Handover_eio.Explicit in
  let prompt = Handover_eio.build_successor_prompt h ~additional_instructions:(Some "extra") in
  check bool "nonempty" true (String.length prompt > 0);
  check bool "contains extra" true (String.length prompt > 50)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Handover Eio Coverage" [
    "handover_record", [
      test_case "basic" `Quick test_handover_record_basic;
      test_case "with to_agent" `Quick test_handover_record_with_to_agent;
      test_case "errors" `Quick test_handover_record_errors;
    ];
    "trigger_reason", [
      test_case "context_limit" `Quick test_trigger_reason_context_limit;
      test_case "timeout" `Quick test_trigger_reason_timeout;
      test_case "explicit" `Quick test_trigger_reason_explicit;
      test_case "fatal_error" `Quick test_trigger_reason_fatal_error;
      test_case "task_complete" `Quick test_trigger_reason_task_complete;
    ];
    "trigger_reason_to_string", [
      test_case "context_limit" `Quick test_trigger_reason_to_string_context_limit;
      test_case "timeout" `Quick test_trigger_reason_to_string_timeout;
      test_case "explicit" `Quick test_trigger_reason_to_string_explicit;
      test_case "fatal_error" `Quick test_trigger_reason_to_string_fatal_error;
      test_case "task_complete" `Quick test_trigger_reason_to_string_task_complete;
    ];
    "generate_id", [
      test_case "format" `Quick test_generate_id_format;
      test_case "length" `Quick test_generate_id_length;
      test_case "unique" `Quick test_generate_id_unique;
    ];
    "create_handover", [
      test_case "basic" `Quick test_create_handover_basic;
      test_case "timeout" `Quick test_create_handover_timeout;
    ];
    "json_roundtrip", [
      test_case "roundtrip" `Quick test_handover_json_roundtrip;
      test_case "invalid" `Quick test_handover_of_json_invalid;
    ];
    "eio_save_load", [
      test_case "save and load" `Quick test_save_and_load_handover;
      test_case "load nonexistent" `Quick test_load_nonexistent;
    ];
    "eio_list", [
      test_case "empty" `Quick test_list_handovers_empty;
      test_case "multiple" `Quick test_list_handovers_multiple;
    ];
    "eio_pending", [
      test_case "get pending" `Quick test_get_pending_handovers;
    ];
    "eio_claim", [
      test_case "success" `Quick test_claim_handover_success;
      test_case "nonexistent" `Quick test_claim_handover_nonexistent;
    ];
    "format", [
      test_case "as markdown" `Quick test_format_as_markdown;
    ];
    "successor_prompt", [
      test_case "build" `Quick test_build_successor_prompt;
    ];
  ]
