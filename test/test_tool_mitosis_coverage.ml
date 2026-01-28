(** Tool_mitosis Module Coverage Tests *)

open Alcotest

module Tool_mitosis = Masc_mcp.Tool_mitosis

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("summary", `String "test summary")] in
  check string "extracts string" "test summary" (Tool_mitosis.get_string args "summary" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_mitosis.get_string args "summary" "default")

let test_get_float_exists () =
  let args = `Assoc [("context_ratio", `Float 0.75)] in
  check (float 0.001) "extracts float" 0.75 (Tool_mitosis.get_float args "context_ratio" 0.0)

let test_get_float_from_int () =
  let args = `Assoc [("context_ratio", `Int 1)] in
  check (float 0.001) "converts int" 1.0 (Tool_mitosis.get_float args "context_ratio" 0.0)

let test_get_float_missing () =
  let args = `Assoc [] in
  check (float 0.001) "uses default" 0.5 (Tool_mitosis.get_float args "context_ratio" 0.5)

let test_get_bool_exists_true () =
  let args = `Assoc [("task_done", `Bool true)] in
  check bool "extracts true" true (Tool_mitosis.get_bool args "task_done" false)

let test_get_bool_exists_false () =
  let args = `Assoc [("task_done", `Bool false)] in
  check bool "extracts false" false (Tool_mitosis.get_bool args "task_done" true)

let test_get_bool_missing () =
  let args = `Assoc [] in
  check bool "uses default" true (Tool_mitosis.get_bool args "task_done" true)

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_mitosis.context = { config } in
  check bool "context created" true (ctx.config.Masc_mcp.Room.base_path = "/tmp/test")

(* ============================================================
   Dispatch Tests
   ============================================================ *)

let make_ctx () : Tool_mitosis.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-mitosis" in
  { config }

let test_dispatch_mitosis_status () =
  let ctx = make_ctx () in
  match Tool_mitosis.dispatch ctx ~name:"masc_mitosis_status" ~args:(`Assoc []) with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

let test_dispatch_mitosis_all () =
  let ctx = make_ctx () in
  match Tool_mitosis.dispatch ctx ~name:"masc_mitosis_all" ~args:(`Assoc []) with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

let test_dispatch_mitosis_pool () =
  let ctx = make_ctx () in
  match Tool_mitosis.dispatch ctx ~name:"masc_mitosis_pool" ~args:(`Assoc []) with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

let test_dispatch_mitosis_check () =
  let ctx = make_ctx () in
  let args = `Assoc [("context_ratio", `Float 0.5)] in
  match Tool_mitosis.dispatch ctx ~name:"masc_mitosis_check" ~args with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

let test_dispatch_mitosis_record () =
  let ctx = make_ctx () in
  let args = `Assoc [("task_done", `Bool true); ("tool_called", `Bool true)] in
  match Tool_mitosis.dispatch ctx ~name:"masc_mitosis_record" ~args with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

let test_dispatch_mitosis_prepare () =
  let ctx = make_ctx () in
  let args = `Assoc [("full_context", `String "test context")] in
  match Tool_mitosis.dispatch ctx ~name:"masc_mitosis_prepare" ~args with
  | Some (success, _) -> check bool "succeeds" true success
  | None -> fail "expected Some"

(* Note: mitosis_divide is not tested here as it involves spawning
   which requires external processes *)

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_mitosis.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_mitosis Coverage" [
    "get_string", [
      test_case "exists" `Quick test_get_string_exists;
      test_case "missing" `Quick test_get_string_missing;
    ];
    "get_float", [
      test_case "exists" `Quick test_get_float_exists;
      test_case "from int" `Quick test_get_float_from_int;
      test_case "missing" `Quick test_get_float_missing;
    ];
    "get_bool", [
      test_case "true" `Quick test_get_bool_exists_true;
      test_case "false" `Quick test_get_bool_exists_false;
      test_case "missing" `Quick test_get_bool_missing;
    ];
    "context", [
      test_case "creation" `Quick test_context_creation;
    ];
    "dispatch", [
      test_case "mitosis_status" `Quick test_dispatch_mitosis_status;
      test_case "mitosis_all" `Quick test_dispatch_mitosis_all;
      test_case "mitosis_pool" `Quick test_dispatch_mitosis_pool;
      test_case "mitosis_check" `Quick test_dispatch_mitosis_check;
      test_case "mitosis_record" `Quick test_dispatch_mitosis_record;
      test_case "mitosis_prepare" `Quick test_dispatch_mitosis_prepare;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
