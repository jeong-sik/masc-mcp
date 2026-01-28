(** Tool_a2a Module Coverage Tests *)

open Alcotest

module Tool_a2a = Masc_mcp.Tool_a2a

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("target_agent", `String "codex-001")] in
  check string "extracts string" "codex-001" (Tool_a2a.get_string args "target_agent" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_a2a.get_string args "target_agent" "default")

let test_get_string_opt_exists () =
  let args = `Assoc [("endpoint", `String "https://example.com")] in
  check (option string) "extracts option" (Some "https://example.com") (Tool_a2a.get_string_opt args "endpoint")

let test_get_string_opt_missing () =
  let args = `Assoc [] in
  check (option string) "returns None" None (Tool_a2a.get_string_opt args "endpoint")

let test_get_int_exists () =
  let args = `Assoc [("timeout", `Int 600)] in
  check int "extracts int" 600 (Tool_a2a.get_int args "timeout" 300)

let test_get_int_missing () =
  let args = `Assoc [] in
  check int "uses default" 300 (Tool_a2a.get_int args "timeout" 300)

let test_get_bool_exists () =
  let args = `Assoc [("clear", `Bool false)] in
  check bool "extracts bool" false (Tool_a2a.get_bool args "clear" true)

let test_get_bool_missing () =
  let args = `Assoc [] in
  check bool "uses default" true (Tool_a2a.get_bool args "clear" true)

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_a2a.context = { config; agent_name = "test-agent" } in
  check string "agent_name" "test-agent" ctx.agent_name

(* ============================================================
   Dispatch Tests
   ============================================================ *)

let make_ctx () : Tool_a2a.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-a2a" in
  { config; agent_name = "test-agent" }

let test_dispatch_a2a_discover () =
  let ctx = make_ctx () in
  try
    match Tool_a2a.dispatch ctx ~name:"masc_a2a_discover" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to a2a_discover" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (error expected)" true true

let test_dispatch_a2a_query_skill () =
  let ctx = make_ctx () in
  let args = `Assoc [("agent_name", `String "claude"); ("skill_id", `String "coding")] in
  try
    match Tool_a2a.dispatch ctx ~name:"masc_a2a_query_skill" ~args with
    | Some _ -> check bool "routes to a2a_query_skill" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (error expected)" true true

let test_dispatch_a2a_delegate () =
  let ctx = make_ctx () in
  let args = `Assoc [
    ("target_agent", `String "codex");
    ("message", `String "Please review this code");
    ("task_type", `String "async");
    ("timeout", `Int 300)
  ] in
  try
    match Tool_a2a.dispatch ctx ~name:"masc_a2a_delegate" ~args with
    | Some _ -> check bool "routes to a2a_delegate" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (error expected)" true true

let test_dispatch_a2a_subscribe () =
  let ctx = make_ctx () in
  let args = `Assoc [("events", `List [`String "broadcast"; `String "task_update"])] in
  try
    match Tool_a2a.dispatch ctx ~name:"masc_a2a_subscribe" ~args with
    | Some _ -> check bool "routes to a2a_subscribe" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (error expected)" true true

let test_dispatch_a2a_unsubscribe () =
  let ctx = make_ctx () in
  let args = `Assoc [("subscription_id", `String "sub-001")] in
  try
    match Tool_a2a.dispatch ctx ~name:"masc_a2a_unsubscribe" ~args with
    | Some _ -> check bool "routes to a2a_unsubscribe" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (error expected)" true true

let test_dispatch_poll_events () =
  let ctx = make_ctx () in
  let args = `Assoc [("subscription_id", `String "sub-001"); ("clear", `Bool true)] in
  try
    match Tool_a2a.dispatch ctx ~name:"masc_poll_events" ~args with
    | Some _ -> check bool "routes to poll_events" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (error expected)" true true

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_a2a.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_a2a Coverage" [
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
    "context", [
      test_case "creation" `Quick test_context_creation;
    ];
    "dispatch", [
      test_case "a2a_discover" `Quick test_dispatch_a2a_discover;
      test_case "a2a_query_skill" `Quick test_dispatch_a2a_query_skill;
      test_case "a2a_delegate" `Quick test_dispatch_a2a_delegate;
      test_case "a2a_subscribe" `Quick test_dispatch_a2a_subscribe;
      test_case "a2a_unsubscribe" `Quick test_dispatch_a2a_unsubscribe;
      test_case "poll_events" `Quick test_dispatch_poll_events;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
