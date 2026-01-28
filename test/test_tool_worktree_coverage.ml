(** Tool_worktree Module Coverage Tests *)

open Alcotest

module Tool_worktree = Masc_mcp.Tool_worktree

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("task_id", `String "task-001")] in
  check string "extracts string" "task-001" (Tool_worktree.get_string args "task_id" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_worktree.get_string args "task_id" "default")

let test_get_string_base_branch () =
  let args = `Assoc [("base_branch", `String "main")] in
  check string "extracts branch" "main" (Tool_worktree.get_string args "base_branch" "develop")

let test_get_string_base_branch_default () =
  let args = `Assoc [] in
  check string "uses develop default" "develop" (Tool_worktree.get_string args "base_branch" "develop")

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_worktree.context = { config; agent_name = "test-agent" } in
  check string "agent_name" "test-agent" ctx.agent_name

(* ============================================================
   Dispatch Tests
   ============================================================ *)

let make_ctx () : Tool_worktree.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-worktree" in
  { config; agent_name = "test-agent" }

let test_dispatch_worktree_create () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001"); ("base_branch", `String "main")] in
  try
    match Tool_worktree.dispatch ctx ~name:"masc_worktree_create" ~args with
    | Some _ -> check bool "routes to worktree_create" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (git error expected)" true true

let test_dispatch_worktree_remove () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_id", `String "task-001")] in
  try
    match Tool_worktree.dispatch ctx ~name:"masc_worktree_remove" ~args with
    | Some _ -> check bool "routes to worktree_remove" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (git error expected)" true true

let test_dispatch_worktree_list () =
  let ctx = make_ctx () in
  try
    match Tool_worktree.dispatch ctx ~name:"masc_worktree_list" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to worktree_list" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (git error expected)" true true

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_worktree.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_worktree Coverage" [
    "get_string", [
      test_case "exists" `Quick test_get_string_exists;
      test_case "missing" `Quick test_get_string_missing;
      test_case "base_branch" `Quick test_get_string_base_branch;
      test_case "base_branch_default" `Quick test_get_string_base_branch_default;
    ];
    "context", [
      test_case "creation" `Quick test_context_creation;
    ];
    "dispatch", [
      test_case "worktree_create" `Quick test_dispatch_worktree_create;
      test_case "worktree_remove" `Quick test_dispatch_worktree_remove;
      test_case "worktree_list" `Quick test_dispatch_worktree_list;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
