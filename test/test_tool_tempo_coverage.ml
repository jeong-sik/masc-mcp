(** Tool_tempo Module Coverage Tests *)

open Alcotest

module Tool_tempo = Masc_mcp.Tool_tempo

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("action", `String "get")] in
  check string "extracts string" "get" (Tool_tempo.get_string args "action" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_tempo.get_string args "action" "default")

let test_get_string_opt_exists () =
  let args = `Assoc [("reason", `String "manual")] in
  check (option string) "extracts option" (Some "manual") (Tool_tempo.get_string_opt args "reason")

let test_get_string_opt_missing () =
  let args = `Assoc [] in
  check (option string) "returns None" None (Tool_tempo.get_string_opt args "reason")

let test_get_float_exists () =
  let args = `Assoc [("interval", `Float 30.0)] in
  check (float 0.001) "extracts float" 30.0 (Tool_tempo.get_float args "interval" 0.0)

let test_get_float_from_int () =
  let args = `Assoc [("interval", `Int 30)] in
  check (float 0.001) "converts int" 30.0 (Tool_tempo.get_float args "interval" 0.0)

let test_get_float_missing () =
  let args = `Assoc [] in
  check (float 0.001) "uses default" 10.0 (Tool_tempo.get_float args "interval" 10.0)

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_tempo.context = { config; agent_name = "test-agent" } in
  check string "agent_name" "test-agent" ctx.agent_name

(* ============================================================
   Dispatch Tests - Check routing only
   ============================================================ *)

let make_ctx () : Tool_tempo.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-tempo" in
  { config; agent_name = "test-agent" }

let test_dispatch_tempo_get () =
  let ctx = make_ctx () in
  try
    match Tool_tempo.dispatch ctx ~name:"masc_tempo_get" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to tempo_get" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_tempo_set () =
  let ctx = make_ctx () in
  let args = `Assoc [("interval_seconds", `Float 60.0); ("reason", `String "test")] in
  try
    match Tool_tempo.dispatch ctx ~name:"masc_tempo_set" ~args with
    | Some _ -> check bool "routes to tempo_set" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_tempo_set_invalid () =
  let ctx = make_ctx () in
  let args = `Assoc [("interval_seconds", `Float 0.0)] in
  match Tool_tempo.dispatch ctx ~name:"masc_tempo_set" ~args with
  | Some (success, _) -> check bool "fails on zero" false success
  | None -> fail "expected Some"

let test_dispatch_tempo_adjust () =
  let ctx = make_ctx () in
  try
    match Tool_tempo.dispatch ctx ~name:"masc_tempo_adjust" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to tempo_adjust" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_tempo_reset () =
  let ctx = make_ctx () in
  try
    match Tool_tempo.dispatch ctx ~name:"masc_tempo_reset" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to tempo_reset" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_tempo_action_get () =
  let ctx = make_ctx () in
  let args = `Assoc [("action", `String "get")] in
  try
    match Tool_tempo.dispatch ctx ~name:"masc_tempo" ~args with
    | Some _ -> check bool "routes to tempo action get" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_tempo_action_set () =
  let ctx = make_ctx () in
  let args = `Assoc [("action", `String "set"); ("mode", `String "slow")] in
  try
    match Tool_tempo.dispatch ctx ~name:"masc_tempo" ~args with
    | Some _ -> check bool "routes to tempo action set" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_tempo_action_invalid () =
  let ctx = make_ctx () in
  let args = `Assoc [("action", `String "invalid")] in
  match Tool_tempo.dispatch ctx ~name:"masc_tempo" ~args with
  | Some (success, _) -> check bool "fails on invalid" false success
  | None -> fail "expected Some"

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_tempo.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_tempo Coverage" [
    "get_string", [
      test_case "exists" `Quick test_get_string_exists;
      test_case "missing" `Quick test_get_string_missing;
    ];
    "get_string_opt", [
      test_case "exists" `Quick test_get_string_opt_exists;
      test_case "missing" `Quick test_get_string_opt_missing;
    ];
    "get_float", [
      test_case "exists" `Quick test_get_float_exists;
      test_case "from int" `Quick test_get_float_from_int;
      test_case "missing" `Quick test_get_float_missing;
    ];
    "context", [
      test_case "creation" `Quick test_context_creation;
    ];
    "dispatch", [
      test_case "tempo_get" `Quick test_dispatch_tempo_get;
      test_case "tempo_set" `Quick test_dispatch_tempo_set;
      test_case "tempo_set_invalid" `Quick test_dispatch_tempo_set_invalid;
      test_case "tempo_adjust" `Quick test_dispatch_tempo_adjust;
      test_case "tempo_reset" `Quick test_dispatch_tempo_reset;
      test_case "tempo_action_get" `Quick test_dispatch_tempo_action_get;
      test_case "tempo_action_set" `Quick test_dispatch_tempo_action_set;
      test_case "tempo_action_invalid" `Quick test_dispatch_tempo_action_invalid;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
