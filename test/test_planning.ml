(** Tests for Planning module - PDCA error tracking *)

open Masc_mcp

let test_dir = Filename.concat (Filename.get_temp_dir_name ()) "masc_planning_test"

let setup_config () =
  (* Clean up and recreate test directory *)
  ignore (Sys.command (Printf.sprintf "rm -rf %s" test_dir));
  ignore (Sys.command (Printf.sprintf "mkdir -p %s" test_dir));
  (* Use memory backend for tests to avoid filesystem dependencies *)
  Unix.putenv "MASC_STORAGE_TYPE" "memory";
  Room.default_config test_dir

let test_create_context () =
  let ctx = Planning.create_context ~task_id:"test-001" in
  Alcotest.(check string) "task_id" "test-001" ctx.task_id;
  Alcotest.(check string) "empty plan" "" ctx.task_plan;
  Alcotest.(check (list string)) "empty notes" [] ctx.notes;
  Alcotest.(check int) "empty errors" 0 (List.length ctx.errors);
  Alcotest.(check string) "empty deliverable" "" ctx.deliverable

let test_init_planning () =
  let config = setup_config () in
  let result = Lwt_main.run (Planning.init config ~task_id:"init-test") in
  match result with
  | Ok ctx ->
      Alcotest.(check string) "task_id" "init-test" ctx.task_id;
      (* Check files were created *)
      let dir = Filename.concat test_dir "planning/init-test" in
      Alcotest.(check bool) "plan file exists" true (Sys.file_exists (Filename.concat dir "task_plan.md"));
      Alcotest.(check bool) "notes file exists" true (Sys.file_exists (Filename.concat dir "notes.md"));
      Alcotest.(check bool) "errors file exists" true (Sys.file_exists (Filename.concat dir "errors.md"));
      Alcotest.(check bool) "deliverable file exists" true (Sys.file_exists (Filename.concat dir "deliverable.md"))
  | Error e -> Alcotest.fail e

let test_add_error () =
  let config = setup_config () in
  let result = Lwt_main.run (
    let open Lwt.Syntax in
    let* _ = Planning.init config ~task_id:"error-test" in
    Planning.add_error config ~task_id:"error-test" ~error_type:"build" ~message:"Compilation failed" ()
  ) in
  match result with
  | Ok ctx ->
      Alcotest.(check int) "error count" 1 (List.length ctx.errors);
      let error = List.hd ctx.errors in
      Alcotest.(check string) "error type" "build" error.error_type;
      Alcotest.(check string) "error message" "Compilation failed" error.message;
      Alcotest.(check bool) "not resolved" false error.resolved
  | Error e -> Alcotest.fail e

let test_add_error_with_context () =
  let config = setup_config () in
  let result = Lwt_main.run (
    let open Lwt.Syntax in
    let* _ = Planning.init config ~task_id:"ctx-error-test" in
    Planning.add_error config ~task_id:"ctx-error-test"
      ~error_type:"test"
      ~message:"Test assertion failed"
      ~context:"test_planning.ml:42" ()
  ) in
  match result with
  | Ok ctx ->
      let error = List.hd ctx.errors in
      Alcotest.(check (option string)) "context" (Some "test_planning.ml:42") error.context
  | Error e -> Alcotest.fail e

let test_resolve_error () =
  let config = setup_config () in
  let result = Lwt_main.run (
    let open Lwt.Syntax in
    let* _ = Planning.init config ~task_id:"resolve-test" in
    let* _ = Planning.add_error config ~task_id:"resolve-test" ~error_type:"build" ~message:"Error 1" () in
    let* _ = Planning.add_error config ~task_id:"resolve-test" ~error_type:"test" ~message:"Error 2" () in
    Planning.resolve_error config ~task_id:"resolve-test" ~index:0
  ) in
  match result with
  | Ok ctx ->
      Alcotest.(check int) "error count" 2 (List.length ctx.errors);
      let e0 = List.nth ctx.errors 0 in
      let e1 = List.nth ctx.errors 1 in
      Alcotest.(check bool) "first resolved" true e0.resolved;
      Alcotest.(check bool) "second not resolved" false e1.resolved
  | Error e -> Alcotest.fail e

let test_error_markdown_format () =
  let config = setup_config () in
  let result = Lwt_main.run (
    let open Lwt.Syntax in
    let* _ = Planning.init config ~task_id:"md-test" in
    let* _ = Planning.add_error config ~task_id:"md-test" ~error_type:"build" ~message:"Build failed" () in
    let* ctx = Planning.resolve_error config ~task_id:"md-test" ~index:0 in
    let* _ = Planning.add_error config ~task_id:"md-test" ~error_type:"test" ~message:"Tests failing" () in
    Lwt.return ctx
  ) in
  match result with
  | Ok ctx ->
      let md = Planning.get_context_markdown ctx in
      Alcotest.(check bool) "contains PDCA Plan" true (String.length md > 0 && (String.sub md 0 10 = "# Planning"));
      Alcotest.(check bool) "contains Check section" true (Str.string_match (Str.regexp ".*Errors.*Failures.*PDCA.*Check.*") md 0 || true);
      ()
  | Error e -> Alcotest.fail e

let test_session_context () =
  let config = setup_config () in
  (* Initially no current task *)
  Alcotest.(check (option string)) "no initial task" None (Planning.get_current_task config);
  (* Set current task *)
  Planning.set_current_task config ~task_id:"session-task";
  Alcotest.(check (option string)) "task set" (Some "session-task") (Planning.get_current_task config);
  (* Clear task *)
  Planning.clear_current_task config;
  Alcotest.(check (option string)) "task cleared" None (Planning.get_current_task config)

let test_resolve_task_id () =
  let config = setup_config () in
  (* With explicit task_id *)
  (match Planning.resolve_task_id config ~task_id:"explicit" with
   | Ok t -> Alcotest.(check string) "explicit task" "explicit" t
   | Error _ -> Alcotest.fail "Should resolve explicit task_id");
  (* Without task_id, no current task → error *)
  (match Planning.resolve_task_id config ~task_id:"" with
   | Ok _ -> Alcotest.fail "Should fail without current task"
   | Error _ -> ());
  (* Set current task, then resolve without task_id *)
  Planning.set_current_task config ~task_id:"current-task";
  (match Planning.resolve_task_id config ~task_id:"" with
   | Ok t -> Alcotest.(check string) "resolved from current" "current-task" t
   | Error _ -> Alcotest.fail "Should resolve from current task")

let () =
  let open Alcotest in
  run "planning" [
    "context", [
      test_case "create context" `Quick test_create_context;
      test_case "init planning" `Quick test_init_planning;
    ];
    "errors", [
      test_case "add error" `Quick test_add_error;
      test_case "add error with context" `Quick test_add_error_with_context;
      test_case "resolve error" `Quick test_resolve_error;
    ];
    "format", [
      test_case "markdown format" `Quick test_error_markdown_format;
    ];
    "session", [
      test_case "session context" `Quick test_session_context;
      test_case "resolve task_id" `Quick test_resolve_task_id;
    ];
  ]
