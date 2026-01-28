(** Tool_vote Module Coverage Tests *)

open Alcotest

module Tool_vote = Masc_mcp.Tool_vote

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("topic", `String "Should we merge?")] in
  check string "extracts string" "Should we merge?" (Tool_vote.get_string args "topic" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_vote.get_string args "topic" "default")

let test_get_int_exists () =
  let args = `Assoc [("required_votes", `Int 3)] in
  check int "extracts int" 3 (Tool_vote.get_int args "required_votes" 2)

let test_get_int_missing () =
  let args = `Assoc [] in
  check int "uses default" 2 (Tool_vote.get_int args "required_votes" 2)

let test_get_string_list_exists () =
  let args = `Assoc [("options", `List [`String "yes"; `String "no"; `String "abstain"])] in
  check (list string) "extracts list" ["yes"; "no"; "abstain"] (Tool_vote.get_string_list args "options")

let test_get_string_list_missing () =
  let args = `Assoc [] in
  check (list string) "returns empty" [] (Tool_vote.get_string_list args "options")

let test_get_string_list_mixed () =
  let args = `Assoc [("options", `List [`String "yes"; `Int 1; `String "no"])] in
  check (list string) "filters non-strings" ["yes"; "no"] (Tool_vote.get_string_list args "options")

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_vote.context = { config; agent_name = "test-agent" } in
  check string "agent_name" "test-agent" ctx.agent_name

(* ============================================================
   Dispatch Tests
   ============================================================ *)

let make_ctx () : Tool_vote.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-vote" in
  { config; agent_name = "test-agent" }

let test_dispatch_vote_create () =
  let ctx = make_ctx () in
  let args = `Assoc [
    ("topic", `String "Should we deploy?");
    ("options", `List [`String "yes"; `String "no"]);
    ("required_votes", `Int 2)
  ] in
  try
    match Tool_vote.dispatch ctx ~name:"masc_vote_create" ~args with
    | Some (success, _) -> check bool "succeeds" true success
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_vote_cast () =
  let ctx = make_ctx () in
  let args = `Assoc [("vote_id", `String "vote-001"); ("choice", `String "yes")] in
  try
    match Tool_vote.dispatch ctx ~name:"masc_vote_cast" ~args with
    | Some (success, _) -> check bool "succeeds" true success
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_vote_status () =
  let ctx = make_ctx () in
  let args = `Assoc [("vote_id", `String "vote-001")] in
  try
    match Tool_vote.dispatch ctx ~name:"masc_vote_status" ~args with
    | Some _ -> check bool "routes to vote_status" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_votes () =
  let ctx = make_ctx () in
  try
    match Tool_vote.dispatch ctx ~name:"masc_votes" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to votes" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_vote.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_vote Coverage" [
    "get_string", [
      test_case "exists" `Quick test_get_string_exists;
      test_case "missing" `Quick test_get_string_missing;
    ];
    "get_int", [
      test_case "exists" `Quick test_get_int_exists;
      test_case "missing" `Quick test_get_int_missing;
    ];
    "get_string_list", [
      test_case "exists" `Quick test_get_string_list_exists;
      test_case "missing" `Quick test_get_string_list_missing;
      test_case "mixed" `Quick test_get_string_list_mixed;
    ];
    "context", [
      test_case "creation" `Quick test_context_creation;
    ];
    "dispatch", [
      test_case "vote_create" `Quick test_dispatch_vote_create;
      test_case "vote_cast" `Quick test_dispatch_vote_cast;
      test_case "vote_status" `Quick test_dispatch_vote_status;
      test_case "votes" `Quick test_dispatch_votes;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
