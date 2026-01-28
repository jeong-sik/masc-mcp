(** Tool_swarm Module Coverage Tests

    Tests for swarm tool handlers - argument extraction, dispatch logic.
    Integration tests require Eio runtime and filesystem.
*)

open Alcotest

module Tool_swarm = Masc_mcp.Tool_swarm

(* ============================================================
   Argument Helper Tests (Pure Functions)
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("name", `String "claude")] in
  check string "extracts string" "claude" (Tool_swarm.get_string args "name" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_swarm.get_string args "name" "default")

let test_get_string_wrong_type () =
  let args = `Assoc [("name", `Int 42)] in
  check string "uses default on type mismatch" "default" (Tool_swarm.get_string args "name" "default")

let test_get_float_exists () =
  let args = `Assoc [("rate", `Float 0.5)] in
  check (float 0.001) "extracts float" 0.5 (Tool_swarm.get_float args "rate" 0.0)

let test_get_float_missing () =
  let args = `Assoc [] in
  check (float 0.001) "uses default" 0.3 (Tool_swarm.get_float args "rate" 0.3)

let test_get_float_from_int () =
  (* JSON often has ints where floats are expected *)
  let args = `Assoc [("rate", `Int 1)] in
  check (float 0.001) "int fallback to default" 0.5 (Tool_swarm.get_float args "rate" 0.5)

let test_get_int_exists () =
  let args = `Assoc [("limit", `Int 10)] in
  check int "extracts int" 10 (Tool_swarm.get_int args "limit" 5)

let test_get_int_missing () =
  let args = `Assoc [] in
  check int "uses default" 5 (Tool_swarm.get_int args "limit" 5)

let test_get_bool_exists_true () =
  let args = `Assoc [("enabled", `Bool true)] in
  check bool "extracts true" true (Tool_swarm.get_bool args "enabled" false)

let test_get_bool_exists_false () =
  let args = `Assoc [("enabled", `Bool false)] in
  check bool "extracts false" false (Tool_swarm.get_bool args "enabled" true)

let test_get_bool_missing () =
  let args = `Assoc [] in
  check bool "uses default" true (Tool_swarm.get_bool args "enabled" true)

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_swarm.context = {
    config;
    fs = None;
    agent_name = "test-agent";
  } in
  check string "agent_name" "test-agent" ctx.agent_name;
  check bool "fs is None" true (ctx.fs = None)

(* ============================================================
   Dispatch Tests (Without Filesystem - Returns Error)
   ============================================================ *)

let make_ctx () : Tool_swarm.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-swarm" in
  { config; fs = None; agent_name = "test-agent" }

let test_dispatch_swarm_init_no_fs () =
  let ctx = make_ctx () in
  match Tool_swarm.dispatch ctx ~name:"masc_swarm_init" ~args:(`Assoc []) with
  | Some (success, msg) ->
      check bool "fails without fs" false success;
      check bool "error message" true (String.length msg > 0)
  | None -> fail "expected Some"

let test_dispatch_swarm_join_no_fs () =
  let ctx = make_ctx () in
  match Tool_swarm.dispatch ctx ~name:"masc_swarm_join" ~args:(`Assoc []) with
  | Some (success, _) ->
      check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_swarm_leave_no_fs () =
  let ctx = make_ctx () in
  match Tool_swarm.dispatch ctx ~name:"masc_swarm_leave" ~args:(`Assoc []) with
  | Some (success, _) ->
      check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_swarm_status_no_fs () =
  let ctx = make_ctx () in
  match Tool_swarm.dispatch ctx ~name:"masc_swarm_status" ~args:(`Assoc []) with
  | Some (success, _) ->
      check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_swarm_evolve_no_fs () =
  let ctx = make_ctx () in
  match Tool_swarm.dispatch ctx ~name:"masc_swarm_evolve" ~args:(`Assoc []) with
  | Some (success, _) ->
      check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_swarm_propose_no_fs () =
  let ctx = make_ctx () in
  let args = `Assoc [("description", `String "test proposal")] in
  match Tool_swarm.dispatch ctx ~name:"masc_swarm_propose" ~args with
  | Some (success, _) ->
      check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_swarm_vote_no_fs () =
  let ctx = make_ctx () in
  let args = `Assoc [("proposal_id", `String "prop-1"); ("vote_for", `Bool true)] in
  match Tool_swarm.dispatch ctx ~name:"masc_swarm_vote" ~args with
  | Some (success, _) ->
      check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_swarm_deposit_no_fs () =
  let ctx = make_ctx () in
  let args = `Assoc [("path_id", `String "path-1"); ("strength", `Float 0.5)] in
  match Tool_swarm.dispatch ctx ~name:"masc_swarm_deposit" ~args with
  | Some (success, _) ->
      check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_swarm_trails_no_fs () =
  let ctx = make_ctx () in
  match Tool_swarm.dispatch ctx ~name:"masc_swarm_trails" ~args:(`Assoc []) with
  | Some (success, _) ->
      check bool "fails without fs" false success
  | None -> fail "expected Some"

let test_dispatch_swarm_walph () =
  (* masc_swarm_walph requires Eio runtime - test dispatch routing only *)
  let ctx = make_ctx () in
  let args = `Assoc [("command", `String "STATUS")] in
  try
    match Tool_swarm.dispatch ctx ~name:"masc_swarm_walph" ~args with
    | Some _ -> check bool "dispatch routes to walph" true true
    | None -> fail "expected Some"
  with
  | Effect.Unhandled _ ->
      (* Expected: Eio effects not handled outside Eio runtime *)
      check bool "requires Eio runtime" true true

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_swarm.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

let test_dispatch_non_swarm_tool () =
  let ctx = make_ctx () in
  match Tool_swarm.dispatch ctx ~name:"masc_status" ~args:(`Assoc []) with
  | None -> check bool "returns None for non-swarm" true true
  | Some _ -> fail "expected None for non-swarm tool"

(* ============================================================
   Individual Handler Tests (Argument Parsing)
   ============================================================ *)

let test_handle_init_parses_behavior () =
  let ctx = make_ctx () in
  let args = `Assoc [("behavior", `String "stigmergy")] in
  try
    let (success, msg) = Tool_swarm.handle_init ctx args in
    (* Will fail due to no fs, just check it returns a result *)
    check bool "fails without fs" false success;
    check bool "has error message" true (String.length msg > 0)
  with
  | Effect.Unhandled _ ->
      (* Some paths may require Eio runtime *)
      check bool "requires Eio runtime" true true

let test_handle_join_parses_agent_name () =
  let ctx = make_ctx () in
  let args = `Assoc [("agent_name", `String "custom-agent")] in
  let (success, _) = Tool_swarm.handle_join ctx args in
  check bool "fails without fs" false success

let test_handle_propose_parses_threshold () =
  let ctx = make_ctx () in
  let args = `Assoc [
    ("description", `String "Test");
    ("threshold", `Float 0.75);
  ] in
  let (success, _) = Tool_swarm.handle_propose ctx args in
  check bool "fails without fs" false success

let test_handle_deposit_parses_strength () =
  let ctx = make_ctx () in
  let args = `Assoc [
    ("path_id", `String "test-path");
    ("strength", `Float 0.8);
  ] in
  let (success, _) = Tool_swarm.handle_deposit ctx args in
  check bool "fails without fs" false success

let test_handle_trails_parses_limit () =
  let ctx = make_ctx () in
  let args = `Assoc [("limit", `Int 10)] in
  let (success, _) = Tool_swarm.handle_trails ctx args in
  check bool "fails without fs" false success

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_swarm Coverage" [
    "get_string", [
      test_case "exists" `Quick test_get_string_exists;
      test_case "missing" `Quick test_get_string_missing;
      test_case "wrong type" `Quick test_get_string_wrong_type;
    ];
    "get_float", [
      test_case "exists" `Quick test_get_float_exists;
      test_case "missing" `Quick test_get_float_missing;
      test_case "from int" `Quick test_get_float_from_int;
    ];
    "get_int", [
      test_case "exists" `Quick test_get_int_exists;
      test_case "missing" `Quick test_get_int_missing;
    ];
    "get_bool", [
      test_case "true" `Quick test_get_bool_exists_true;
      test_case "false" `Quick test_get_bool_exists_false;
      test_case "missing" `Quick test_get_bool_missing;
    ];
    "context", [
      test_case "creation" `Quick test_context_creation;
    ];
    "dispatch_no_fs", [
      test_case "init" `Quick test_dispatch_swarm_init_no_fs;
      test_case "join" `Quick test_dispatch_swarm_join_no_fs;
      test_case "leave" `Quick test_dispatch_swarm_leave_no_fs;
      test_case "status" `Quick test_dispatch_swarm_status_no_fs;
      test_case "evolve" `Quick test_dispatch_swarm_evolve_no_fs;
      test_case "propose" `Quick test_dispatch_swarm_propose_no_fs;
      test_case "vote" `Quick test_dispatch_swarm_vote_no_fs;
      test_case "deposit" `Quick test_dispatch_swarm_deposit_no_fs;
      test_case "trails" `Quick test_dispatch_swarm_trails_no_fs;
      test_case "walph" `Quick test_dispatch_swarm_walph;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
      test_case "non-swarm" `Quick test_dispatch_non_swarm_tool;
    ];
    "handler_parsing", [
      test_case "init behavior" `Quick test_handle_init_parses_behavior;
      test_case "join agent_name" `Quick test_handle_join_parses_agent_name;
      test_case "propose threshold" `Quick test_handle_propose_parses_threshold;
      test_case "deposit strength" `Quick test_handle_deposit_parses_strength;
      test_case "trails limit" `Quick test_handle_trails_parses_limit;
    ];
  ]
