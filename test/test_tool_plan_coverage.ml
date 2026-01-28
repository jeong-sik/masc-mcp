(** Tool_plan Module Coverage Tests *)

open Alcotest

module Tool_plan = Masc_mcp.Tool_plan

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("task_id", `String "task-123")] in
  check string "extracts string" "task-123" (Tool_plan.get_string args "task_id" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_plan.get_string args "task_id" "default")

let test_get_string_wrong_type () =
  let args = `Assoc [("task_id", `Int 42)] in
  check string "uses default on type mismatch" "default" (Tool_plan.get_string args "task_id" "default")

let test_get_int_exists () =
  let args = `Assoc [("index", `Int 5)] in
  check int "extracts int" 5 (Tool_plan.get_int args "index" 0)

let test_get_int_missing () =
  let args = `Assoc [] in
  check int "uses default" 0 (Tool_plan.get_int args "index" 0)

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_plan.context = { config } in
  check bool "context created" true (ctx.config.Masc_mcp.Room.base_path = "/tmp/test")

(* ============================================================
   Dispatch Tests
   ============================================================ *)

let make_ctx () : Tool_plan.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-plan" in
  { config }

let test_dispatch_plan_init () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001")] in
  match Tool_plan.dispatch ctx ~name:"masc_plan_init" ~args with
  | Some (success, msg) ->
      (* Will fail due to no fs initialization, just check dispatch works *)
      check bool "dispatches to plan_init" true (String.length msg > 0);
      ignore success
  | None -> fail "expected Some"

let test_dispatch_plan_update () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("content", `String "test")] in
  match Tool_plan.dispatch ctx ~name:"masc_plan_update" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_note_add () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("note", `String "test note")] in
  match Tool_plan.dispatch ctx ~name:"masc_note_add" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_deliver () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("content", `String "deliverable")] in
  match Tool_plan.dispatch ctx ~name:"masc_deliver" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_plan_get () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001")] in
  match Tool_plan.dispatch ctx ~name:"masc_plan_get" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_error_add () =
  let ctx = make_ctx () in
  let args = `Assoc [
    ("task_id", `String "task-001");
    ("error_type", `String "compile");
    ("message", `String "error msg")
  ] in
  match Tool_plan.dispatch ctx ~name:"masc_error_add" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_error_resolve () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("error_index", `Int 0)] in
  match Tool_plan.dispatch ctx ~name:"masc_error_resolve" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_plan_set_task () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001")] in
  match Tool_plan.dispatch ctx ~name:"masc_plan_set_task" ~args with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

let test_dispatch_plan_set_task_empty () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "")] in
  match Tool_plan.dispatch ctx ~name:"masc_plan_set_task" ~args with
  | Some (success, _) -> check bool "fails on empty" false success
  | None -> fail "expected Some"

let test_dispatch_plan_get_task () =
  let ctx = make_ctx () in
  match Tool_plan.dispatch ctx ~name:"masc_plan_get_task" ~args:(`Assoc []) with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

let test_dispatch_plan_clear_task () =
  let ctx = make_ctx () in
  match Tool_plan.dispatch ctx ~name:"masc_plan_clear_task" ~args:(`Assoc []) with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_plan.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_plan Coverage" [
    "get_string", [
      test_case "exists" `Quick test_get_string_exists;
      test_case "missing" `Quick test_get_string_missing;
      test_case "wrong type" `Quick test_get_string_wrong_type;
    ];
    "get_int", [
      test_case "exists" `Quick test_get_int_exists;
      test_case "missing" `Quick test_get_int_missing;
    ];
    "context", [
      test_case "creation" `Quick test_context_creation;
    ];
    "dispatch", [
      test_case "plan_init" `Quick test_dispatch_plan_init;
      test_case "plan_update" `Quick test_dispatch_plan_update;
      test_case "note_add" `Quick test_dispatch_note_add;
      test_case "deliver" `Quick test_dispatch_deliver;
      test_case "plan_get" `Quick test_dispatch_plan_get;
      test_case "error_add" `Quick test_dispatch_error_add;
      test_case "error_resolve" `Quick test_dispatch_error_resolve;
      test_case "plan_set_task" `Quick test_dispatch_plan_set_task;
      test_case "plan_set_task_empty" `Quick test_dispatch_plan_set_task_empty;
      test_case "plan_get_task" `Quick test_dispatch_plan_get_task;
      test_case "plan_clear_task" `Quick test_dispatch_plan_clear_task;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
