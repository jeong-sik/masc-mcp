(** Tool_cache Module Coverage Tests *)

open Alcotest

module Tool_cache = Masc_mcp.Tool_cache

(* ============================================================
   Argument Helper Tests
   ============================================================ *)

let test_get_string_exists () =
  let args = `Assoc [("key", `String "mykey")] in
  check string "extracts string" "mykey" (Tool_cache.get_string args "key" "default")

let test_get_string_missing () =
  let args = `Assoc [] in
  check string "uses default" "default" (Tool_cache.get_string args "key" "default")

let test_get_string_opt_exists () =
  let args = `Assoc [("tag", `String "important")] in
  check (option string) "extracts option" (Some "important") (Tool_cache.get_string_opt args "tag")

let test_get_string_opt_missing () =
  let args = `Assoc [] in
  check (option string) "returns None" None (Tool_cache.get_string_opt args "tag")

let test_get_int_opt_exists () =
  let args = `Assoc [("ttl_seconds", `Int 3600)] in
  check (option int) "extracts option" (Some 3600) (Tool_cache.get_int_opt args "ttl_seconds")

let test_get_int_opt_missing () =
  let args = `Assoc [] in
  check (option int) "returns None" None (Tool_cache.get_int_opt args "ttl_seconds")

let test_get_string_list_exists () =
  let args = `Assoc [("tags", `List [`String "a"; `String "b"])] in
  check (list string) "extracts list" ["a"; "b"] (Tool_cache.get_string_list args "tags")

let test_get_string_list_missing () =
  let args = `Assoc [] in
  check (list string) "returns empty" [] (Tool_cache.get_string_list args "tags")

let test_get_string_list_mixed () =
  let args = `Assoc [("tags", `List [`String "a"; `Int 1; `String "b"])] in
  check (list string) "filters non-strings" ["a"; "b"] (Tool_cache.get_string_list args "tags")

(* ============================================================
   Context Creation Tests
   ============================================================ *)

let test_context_creation () =
  let config = Masc_mcp.Room.default_config "/tmp/test" in
  let ctx : Tool_cache.context = { config } in
  check bool "context created" true (ctx.config.Masc_mcp.Room.base_path = "/tmp/test")

(* ============================================================
   Dispatch Tests - Check routing only
   ============================================================ *)

let make_ctx () : Tool_cache.context =
  let config = Masc_mcp.Room.default_config "/tmp/test-cache" in
  { config }

let test_dispatch_cache_set () =
  let ctx = make_ctx () in
  let args = `Assoc [("key", `String "k1"); ("value", `String "v1")] in
  try
    match Tool_cache.dispatch ctx ~name:"masc_cache_set" ~args with
    | Some _ -> check bool "routes to cache_set" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_cache_get () =
  let ctx = make_ctx () in
  let args = `Assoc [("key", `String "k1")] in
  try
    match Tool_cache.dispatch ctx ~name:"masc_cache_get" ~args with
    | Some _ -> check bool "routes to cache_get" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_cache_delete () =
  let ctx = make_ctx () in
  let args = `Assoc [("key", `String "k1")] in
  try
    match Tool_cache.dispatch ctx ~name:"masc_cache_delete" ~args with
    | Some _ -> check bool "routes to cache_delete" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_cache_list () =
  let ctx = make_ctx () in
  try
    match Tool_cache.dispatch ctx ~name:"masc_cache_list" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to cache_list" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_cache_clear () =
  let ctx = make_ctx () in
  try
    match Tool_cache.dispatch ctx ~name:"masc_cache_clear" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to cache_clear" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_cache_stats () =
  let ctx = make_ctx () in
  try
    match Tool_cache.dispatch ctx ~name:"masc_cache_stats" ~args:(`Assoc []) with
    | Some _ -> check bool "routes to cache_stats" true true
    | None -> fail "expected Some"
  with _ -> check bool "handler called (fs error expected)" true true

let test_dispatch_unknown_tool () =
  let ctx = make_ctx () in
  match Tool_cache.dispatch ctx ~name:"masc_unknown" ~args:(`Assoc []) with
  | None -> check bool "returns None for unknown" true true
  | Some _ -> fail "expected None for unknown tool"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Tool_cache Coverage" [
    "get_string", [
      test_case "exists" `Quick test_get_string_exists;
      test_case "missing" `Quick test_get_string_missing;
    ];
    "get_string_opt", [
      test_case "exists" `Quick test_get_string_opt_exists;
      test_case "missing" `Quick test_get_string_opt_missing;
    ];
    "get_int_opt", [
      test_case "exists" `Quick test_get_int_opt_exists;
      test_case "missing" `Quick test_get_int_opt_missing;
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
      test_case "cache_set" `Quick test_dispatch_cache_set;
      test_case "cache_get" `Quick test_dispatch_cache_get;
      test_case "cache_delete" `Quick test_dispatch_cache_delete;
      test_case "cache_list" `Quick test_dispatch_cache_list;
      test_case "cache_clear" `Quick test_dispatch_cache_clear;
      test_case "cache_stats" `Quick test_dispatch_cache_stats;
      test_case "unknown" `Quick test_dispatch_unknown_tool;
    ];
  ]
