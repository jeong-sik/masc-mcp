(** Tool_run Module Coverage Tests *)

open Alcotest

module Tool_run = Masc_mcp.Tool_run

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("task_id", `String "run-123")] in
  check string "extracts string" "run-123" (Tool_run.get_string args "task_id" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_run.get_string args "task_id" "default")

let test_get_string_opt_exists () =
  let args = `Assoc [("agent_name", `String "claude")] in
  check (option string) "extracts option" (Some "claude") (Tool_run.get_string_opt args "agent_name")

let test_get_string_opt_missing () =
  let args = `Assoc [] in
  check (option string) "returns None" None (Tool_run.get_string_opt args "agent_name")

let test_get_string_opt_empty () =
  let args = `Assoc [("agent_name", `String "")] in
  check (option string) "empty is None" None (Tool_run.get_string_opt args "agent_name")

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_run.context = { config } in
  check bool "context created" true (ctx.config.Masc_mcp.Room.base_path = "/tmp/test")

(* ============================================================
   Dispatch Tests
   ============================================================ *)

let make_ctx () : Tool_run.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-run" in
  { config }

let test_dispatch_run_init () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001")] in
  match Tool_run.dispatch ctx ~name:"masc_run_init" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_run_plan () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("plan", `String "test plan")] in
  match Tool_run.dispatch ctx ~name:"masc_run_plan" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_run_log () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("note", `String "log entry")] in
  match Tool_run.dispatch ctx ~name:"masc_run_log" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_run_deliverable () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("deliverable", `String "output")] in
  match Tool_run.dispatch ctx ~name:"masc_run_deliverable" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_run_get () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001")] in
  match Tool_run.dispatch ctx ~name:"masc_run_get" ~args with
  | Some (_, msg) -> check bool "has message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_run_list () =
  let ctx = make_ctx () in
  match Tool_run.dispatch ctx ~name:"masc_run_list" ~args:(`Assoc []) with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_run.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_run Coverage" [
    "get_string", [
      test_case "exists" `Quick test_get_string_exists;
      test_case "missing" `Quick test_get_string_missing;
    ];
    "get_string_opt", [
      test_case "exists" `Quick test_get_string_opt_exists;
      test_case "missing" `Quick test_get_string_opt_missing;
      test_case "empty" `Quick test_get_string_opt_empty;
    ];
    "context", [
      test_case "creation" `Quick test_context_creation;
    ];
    "dispatch", [
      test_case "run_init" `Quick test_dispatch_run_init;
      test_case "run_plan" `Quick test_dispatch_run_plan;
      test_case "run_log" `Quick test_dispatch_run_log;
      test_case "run_deliverable" `Quick test_dispatch_run_deliverable;
      test_case "run_get" `Quick test_dispatch_run_get;
      test_case "run_list" `Quick test_dispatch_run_list;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
