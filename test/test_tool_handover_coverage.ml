(** Tool_handover Module Coverage Tests *)

open Alcotest

module Tool_handover = Masc_mcp.Tool_handover

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("task_id", `String "task-001")] in
  check string "extracts string" "task-001" (Tool_handover.get_string args "task_id" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_handover.get_string args "task_id" "default")

let test_get_string_opt_exists () =
  let args = `Assoc [("additional_instructions", `String "Be careful")] in
  check (option string) "extracts option" (Some "Be careful") (Tool_handover.get_string_opt args "additional_instructions")

let test_get_string_opt_missing () =
  let args = `Assoc [] in
  check (option string) "returns None" None (Tool_handover.get_string_opt args "additional_instructions")

let test_get_int_exists () =
  let args = `Assoc [("context_pct", `Int 85)] in
  check int "extracts int" 85 (Tool_handover.get_int args "context_pct" 0)

let test_get_int_missing () =
  let args = `Assoc [] in
  check int "uses default" 0 (Tool_handover.get_int args "context_pct" 0)

let test_get_bool_exists () =
  let args = `Assoc [("pending_only", `Bool true)] in
  check bool "extracts bool" true (Tool_handover.get_bool args "pending_only" false)

let test_get_bool_missing () =
  let args = `Assoc [] in
  check bool "uses default" false (Tool_handover.get_bool args "pending_only" false)

let test_get_string_list_exists () =
  let args = `Assoc [("completed_steps", `List [`String "step1"; `String "step2"])] in
  check (list string) "extracts list" ["step1"; "step2"] (Tool_handover.get_string_list args "completed_steps")

let test_get_string_list_missing () =
  let args = `Assoc [] in
  check (list string) "returns empty" [] (Tool_handover.get_string_list args "completed_steps")

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_handover.context = {
    config;
    agent_name = "test-agent";
    fs = None;
    proc_mgr = None;
    sw = Obj.magic ();  (* Cannot create real switch without Eio runtime *)
  } in
  check string "agent_name" "test-agent" ctx.agent_name

(* ============================================================
   Dispatch Tests - No fs available
   ============================================================ *)

let make_ctx () : Tool_handover.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-handover" in
  {
    config;
    agent_name = "test-agent";
    fs = None;
    proc_mgr = None;
    sw = Obj.magic ();
  }

let test_dispatch_handover_create_no_fs () =
  let ctx = make_ctx () in
  let args = `Assoc [
    ("task_id", `String "task-001");
    ("session_id", `String "session-001");
    ("reason", `String "explicit");
    ("goal", `String "Complete the task")
  ] in
  match Tool_handover.dispatch ctx ~name:"masc_handover_create" ~args with
  | Some (success, msg) ->
      check bool "fails without fs" false success;
      check bool "mentions fs" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_handover_list_no_fs () =
  let ctx = make_ctx () in
  match Tool_handover.dispatch ctx ~name:"masc_handover_list" ~args:(`Assoc []) with
  | Some (success, _) -> check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_handover_claim_no_fs () =
  let ctx = make_ctx () in
  let args = `Assoc [("handover_id", `String "handover-001")] in
  match Tool_handover.dispatch ctx ~name:"masc_handover_claim" ~args with
  | Some (success, _) -> check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_handover_claim_and_spawn_no_fs () =
  let ctx = make_ctx () in
  let args = `Assoc [("handover_id", `String "handover-001")] in
  match Tool_handover.dispatch ctx ~name:"masc_handover_claim_and_spawn" ~args with
  | Some (success, _) -> check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_handover_get_no_fs () =
  let ctx = make_ctx () in
  let args = `Assoc [("handover_id", `String "handover-001")] in
  match Tool_handover.dispatch ctx ~name:"masc_handover_get" ~args with
  | Some (success, _) -> check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_handover.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_handover Coverage" [
    "get_string", [
      test_case "exists" `Quick test_get_string_exists;
      test_case "missing" `Quick test_get_string_missing;
    ];
    "get_string_opt", [
      test_case "exists" `Quick test_get_string_opt_exists;
      test_case "missing" `Quick test_get_string_opt_missing;
    ];
    "get_int", [
      test_case "exists" `Quick test_get_int_exists;
      test_case "missing" `Quick test_get_int_missing;
    ];
    "get_bool", [
      test_case "exists" `Quick test_get_bool_exists;
      test_case "missing" `Quick test_get_bool_missing;
    ];
    "get_string_list", [
      test_case "exists" `Quick test_get_string_list_exists;
      test_case "missing" `Quick test_get_string_list_missing;
    ];
    "context", [
      test_case "creation" `Quick test_context_creation;
    ];
    "dispatch", [
      test_case "handover_create_no_fs" `Quick test_dispatch_handover_create_no_fs;
      test_case "handover_list_no_fs" `Quick test_dispatch_handover_list_no_fs;
      test_case "handover_claim_no_fs" `Quick test_dispatch_handover_claim_no_fs;
      test_case "handover_claim_and_spawn_no_fs" `Quick test_dispatch_handover_claim_and_spawn_no_fs;
      test_case "handover_get_no_fs" `Quick test_dispatch_handover_get_no_fs;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
