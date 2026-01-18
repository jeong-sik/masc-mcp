(** test_planning_eio.ml - Tests for Planning_eio module (OCaml 5.x Pure Sync) *)

open Alcotest
open Masc_mcp

let temp_dir = ref ""

let setup () =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "planning_eio_test_%d" (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir dir 0o755;
  temp_dir := dir

let teardown () =
  (* Clean up temp directory *)
  if !temp_dir <> "" then begin
    let cmd = Printf.sprintf "rm -rf %s" (Filename.quote !temp_dir) in
    ignore (Sys.command cmd)
  end

let make_config () : Room.config =
  Room.default_config !temp_dir

(* ===== Type Tests ===== *)

let test_create_context () =
  let ctx = Planning_eio.create_context ~task_id:"test-task-123" in
  check string "task_id" "test-task-123" ctx.task_id;
  check string "task_plan empty" "" ctx.task_plan;
  check int "notes empty" 0 (List.length ctx.notes);
  check int "errors empty" 0 (List.length ctx.errors);
  check string "deliverable empty" "" ctx.deliverable;
  check bool "created_at not empty" true (String.length ctx.created_at > 0);
  check bool "updated_at not empty" true (String.length ctx.updated_at > 0)

let test_json_serialization () =
  let ctx = Planning_eio.create_context ~task_id:"json-test" in
  let ctx = { ctx with
    task_plan = "Test plan";
    notes = ["Note 1"; "Note 2"];
    deliverable = "Test deliverable";
  } in
  let json = Planning_eio.planning_context_to_yojson ctx in
  match Planning_eio.planning_context_of_yojson json with
  | Ok restored ->
      check string "task_id" ctx.task_id restored.task_id;
      check string "task_plan" ctx.task_plan restored.task_plan;
      check int "notes count" (List.length ctx.notes) (List.length restored.notes);
      check string "deliverable" ctx.deliverable restored.deliverable
  | Error e ->
      fail (Printf.sprintf "JSON deserialization failed: %s" e)

let test_error_entry_serialization () =
  let entry : Planning_eio.error_entry = {
    timestamp = "2025-01-01T00:00:00Z";
    error_type = "build";
    message = "Test error";
    context = Some "test_file.ml";
    resolved = false;
  } in
  let json = Planning_eio.error_entry_to_yojson entry in
  match Planning_eio.error_entry_of_yojson json with
  | Ok restored ->
      check string "timestamp" entry.timestamp restored.timestamp;
      check string "error_type" entry.error_type restored.error_type;
      check string "message" entry.message restored.message;
      check (option string) "context" entry.context restored.context;
      check bool "resolved" entry.resolved restored.resolved
  | Error e ->
      fail (Printf.sprintf "Error entry deserialization failed: %s" e)

(* ===== File Operations Tests ===== *)

let test_init () =
  let config = make_config () in
  match Planning_eio.init config ~task_id:"init-test" with
  | Ok ctx ->
      check string "task_id" "init-test" ctx.task_id;
      (* Check files were created *)
      let dir = Filename.concat !temp_dir "planning/init-test" in
      check bool "task_plan.md exists" true (Sys.file_exists (Filename.concat dir "task_plan.md"));
      check bool "notes.md exists" true (Sys.file_exists (Filename.concat dir "notes.md"));
      check bool "errors.md exists" true (Sys.file_exists (Filename.concat dir "errors.md"));
      check bool "deliverable.md exists" true (Sys.file_exists (Filename.concat dir "deliverable.md"));
      check bool "context.json exists" true (Sys.file_exists (Filename.concat dir "context.json"))
  | Error e ->
      fail (Printf.sprintf "Init failed: %s" e)

let test_load () =
  let config = make_config () in
  (* Initialize first *)
  (match Planning_eio.init config ~task_id:"load-test" with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "Init for load test failed: %s" e));
  (* Then load *)
  match Planning_eio.load config ~task_id:"load-test" with
  | Ok ctx ->
      check string "task_id" "load-test" ctx.task_id
  | Error e ->
      fail (Printf.sprintf "Load failed: %s" e)

let test_load_nonexistent () =
  let config = make_config () in
  match Planning_eio.load config ~task_id:"nonexistent-task" with
  | Ok _ ->
      fail "Expected error for nonexistent task"
  | Error _ ->
      (* Expected behavior *)
      ()

let test_update_plan () =
  let config = make_config () in
  (* Initialize first *)
  (match Planning_eio.init config ~task_id:"update-plan-test" with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "Init failed: %s" e));
  (* Update plan *)
  let content = "# My Task Plan\n\n1. Step 1\n2. Step 2\n3. Step 3" in
  match Planning_eio.update_plan config ~task_id:"update-plan-test" ~content with
  | Ok ctx ->
      check string "task_plan updated" content ctx.task_plan;
      (* Verify file *)
      let plan_path = Filename.concat !temp_dir "planning/update-plan-test/task_plan.md" in
      let file_content =
        let ic = open_in plan_path in
        Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
          really_input_string ic (in_channel_length ic))
      in
      check string "file content" content file_content
  | Error e ->
      fail (Printf.sprintf "Update plan failed: %s" e)

let test_add_note () =
  let config = make_config () in
  (* Initialize first *)
  (match Planning_eio.init config ~task_id:"add-note-test" with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "Init failed: %s" e));
  (* Add notes *)
  (match Planning_eio.add_note config ~task_id:"add-note-test" ~note:"First observation" with
   | Ok ctx ->
       check int "notes count 1" 1 (List.length ctx.notes)
   | Error e ->
       fail (Printf.sprintf "Add note 1 failed: %s" e));
  match Planning_eio.add_note config ~task_id:"add-note-test" ~note:"Second observation" with
  | Ok ctx ->
      check int "notes count 2" 2 (List.length ctx.notes);
      check string "first note" "First observation" (List.nth ctx.notes 0);
      check string "second note" "Second observation" (List.nth ctx.notes 1)
  | Error e ->
      fail (Printf.sprintf "Add note 2 failed: %s" e)

let test_set_deliverable () =
  let config = make_config () in
  (* Initialize first *)
  (match Planning_eio.init config ~task_id:"deliverable-test" with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "Init failed: %s" e));
  (* Set deliverable *)
  let content = "# Final Deliverable\n\nTask completed successfully!" in
  match Planning_eio.set_deliverable config ~task_id:"deliverable-test" ~content with
  | Ok ctx ->
      check string "deliverable" content ctx.deliverable
  | Error e ->
      fail (Printf.sprintf "Set deliverable failed: %s" e)

(* ===== Error Tracking Tests ===== *)

let test_add_error () =
  let config = make_config () in
  (* Initialize first *)
  (match Planning_eio.init config ~task_id:"error-test" with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "Init failed: %s" e));
  (* Add error *)
  match Planning_eio.add_error config ~task_id:"error-test"
    ~error_type:"build" ~message:"Compilation failed" ~context:"lib/main.ml" () with
  | Ok ctx ->
      check int "errors count" 1 (List.length ctx.errors);
      let err = List.hd ctx.errors in
      check string "error_type" "build" err.error_type;
      check string "message" "Compilation failed" err.message;
      check (option string) "context" (Some "lib/main.ml") err.context;
      check bool "not resolved" false err.resolved
  | Error e ->
      fail (Printf.sprintf "Add error failed: %s" e)

let test_resolve_error () =
  let config = make_config () in
  (* Initialize and add error *)
  (match Planning_eio.init config ~task_id:"resolve-test" with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "Init failed: %s" e));
  (match Planning_eio.add_error config ~task_id:"resolve-test"
    ~error_type:"test" ~message:"Test failed" () with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "Add error failed: %s" e));
  (* Resolve error *)
  match Planning_eio.resolve_error config ~task_id:"resolve-test" ~index:0 with
  | Ok ctx ->
      check int "errors count" 1 (List.length ctx.errors);
      let err = List.hd ctx.errors in
      check bool "resolved" true err.resolved
  | Error e ->
      fail (Printf.sprintf "Resolve error failed: %s" e)

let test_resolve_invalid_index () =
  let config = make_config () in
  (* Initialize and add error *)
  (match Planning_eio.init config ~task_id:"invalid-resolve-test" with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "Init failed: %s" e));
  (* Try to resolve nonexistent error *)
  match Planning_eio.resolve_error config ~task_id:"invalid-resolve-test" ~index:0 with
  | Ok _ ->
      fail "Expected error for invalid index"
  | Error _ ->
      (* Expected behavior *)
      ()

(* ===== Session Context Tests ===== *)

let test_session_context () =
  let config = make_config () in
  (* Initially no current task *)
  check (option string) "no current task" None (Planning_eio.get_current_task config);
  (* Set current task *)
  Planning_eio.set_current_task config ~task_id:"session-test-task";
  check (option string) "current task set" (Some "session-test-task") (Planning_eio.get_current_task config);
  (* Clear current task *)
  Planning_eio.clear_current_task config;
  check (option string) "current task cleared" None (Planning_eio.get_current_task config)

let test_resolve_task_id () =
  let config = make_config () in
  (* With explicit task_id *)
  (match Planning_eio.resolve_task_id config ~task_id:"explicit-task" with
   | Ok id -> check string "explicit task_id" "explicit-task" id
   | Error e -> fail (Printf.sprintf "Resolve explicit task_id failed: %s" e));
  (* Without task_id, no current task *)
  (match Planning_eio.resolve_task_id config ~task_id:"" with
   | Ok _ -> fail "Expected error when no current task"
   | Error _ -> ());
  (* Set current task, then resolve empty *)
  Planning_eio.set_current_task config ~task_id:"current-fallback";
  match Planning_eio.resolve_task_id config ~task_id:"" with
  | Ok id -> check string "fallback to current" "current-fallback" id
  | Error e -> fail (Printf.sprintf "Fallback resolution failed: %s" e)

(* ===== Display Helper Tests ===== *)

let test_get_context_markdown () =
  let ctx : Planning_eio.planning_context = {
    task_id = "md-test";
    task_plan = "# My Plan\n\nDo something";
    notes = ["Note 1"; "Note 2"];
    errors = [
      { timestamp = "2025-01-01T00:00:00Z"; error_type = "build"; message = "Failed"; context = None; resolved = false };
      { timestamp = "2025-01-01T01:00:00Z"; error_type = "test"; message = "Passed"; context = Some "test.ml"; resolved = true };
    ];
    deliverable = "Done!";
    created_at = "2025-01-01T00:00:00Z";
    updated_at = "2025-01-01T02:00:00Z";
  } in
  let md = Planning_eio.get_context_markdown ctx in
  let contains substr = try ignore (Str.search_forward (Str.regexp_string substr) md 0); true with Not_found -> false in
  check bool "contains task_id" true (contains "md-test");
  check bool "contains plan" true (contains "My Plan");
  check bool "contains notes" true (contains "Note 1");
  check bool "contains deliverable" true (contains "Done!")

(* ===== Test Suite ===== *)

let () =
  setup ();
  at_exit teardown;
  run "Planning_eio" [
    "types", [
      test_case "create_context" `Quick test_create_context;
      test_case "json_serialization" `Quick test_json_serialization;
      test_case "error_entry_serialization" `Quick test_error_entry_serialization;
    ];
    "file_operations", [
      test_case "init" `Quick test_init;
      test_case "load" `Quick test_load;
      test_case "load_nonexistent" `Quick test_load_nonexistent;
      test_case "update_plan" `Quick test_update_plan;
      test_case "add_note" `Quick test_add_note;
      test_case "set_deliverable" `Quick test_set_deliverable;
    ];
    "error_tracking", [
      test_case "add_error" `Quick test_add_error;
      test_case "resolve_error" `Quick test_resolve_error;
      test_case "resolve_invalid_index" `Quick test_resolve_invalid_index;
    ];
    "session_context", [
      test_case "session_context" `Quick test_session_context;
      test_case "resolve_task_id" `Quick test_resolve_task_id;
    ];
    "display", [
      test_case "get_context_markdown" `Quick test_get_context_markdown;
    ];
  ]
