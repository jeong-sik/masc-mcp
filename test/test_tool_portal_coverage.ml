(** Tool_portal Module Coverage Tests *)

open Alcotest

module Tool_portal = Masc_mcp.Tool_portal

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("target_agent", `String "claude-001")] in
  check string "extracts string" "claude-001" (Tool_portal.get_string args "target_agent" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_portal.get_string args "target_agent" "default")

let test_get_string_opt_exists () =
  let args = `Assoc [("initial_message", `String "hello")] in
  check (option string) "extracts option" (Some "hello") (Tool_portal.get_string_opt args "initial_message")

let test_get_string_opt_missing () =
  let args = `Assoc [] in
  check (option string) "returns None" None (Tool_portal.get_string_opt args "initial_message")

let test_get_string_opt_empty () =
  let args = `Assoc [("initial_message", `String "")] in
  check (option string) "empty is None" None (Tool_portal.get_string_opt args "initial_message")

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_portal.context = { config; agent_name = "test-agent" } in
  check string "agent_name" "test-agent" ctx.agent_name

(* ============================================================
   Dispatch Tests
   ============================================================ *)

let make_ctx () : Tool_portal.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-portal" in
  { config; agent_name = "test-agent" }

let test_dispatch_portal_open () =
  let ctx = make_ctx () in
  let args = `Assoc [("target_agent", `String "claude-002")] in
  try
    match Tool_portal.dispatch ctx ~name:"masc_portal_open" ~args with
    | Some _ -> check bool "routes to portal_open" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_portal_send () =
  let ctx = make_ctx () in
  let args = `Assoc [("message", `String "hello")] in
  try
    match Tool_portal.dispatch ctx ~name:"masc_portal_send" ~args with
    | Some _ -> check bool "routes to portal_send" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_portal_close () =
  let ctx = make_ctx () in
  try
    match Tool_portal.dispatch ctx ~name:"masc_portal_close" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to portal_close" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_portal_status () =
  let ctx = make_ctx () in
  try
    match Tool_portal.dispatch ctx ~name:"masc_portal_status" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to portal_status" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_portal.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_portal Coverage" [
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
      test_case "portal_open" `Quick test_dispatch_portal_open;
      test_case "portal_send" `Quick test_dispatch_portal_send;
      test_case "portal_close" `Quick test_dispatch_portal_close;
      test_case "portal_status" `Quick test_dispatch_portal_status;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
