(** Mode Module Coverage Tests - Category-based Tool Filtering *)

open Alcotest

module Mode = Masc_mcp.Mode

(* ============================================================
   Category Tests
   ============================================================ *)

let test_category_to_string () =
  check string "core" "core" (Mode.category_to_string Mode.Core);
  check string "comm" "comm" (Mode.category_to_string Mode.Comm);
  check string "portal" "portal" (Mode.category_to_string Mode.Portal);
  check string "worktree" "worktree" (Mode.category_to_string Mode.Worktree);
  check string "health" "health" (Mode.category_to_string Mode.Health);
  check string "discovery" "discovery" (Mode.category_to_string Mode.Discovery);
  check string "voting" "voting" (Mode.category_to_string Mode.Voting);
  check string "interrupt" "interrupt" (Mode.category_to_string Mode.Interrupt);
  check string "cost" "cost" (Mode.category_to_string Mode.Cost);
  check string "auth" "auth" (Mode.category_to_string Mode.Auth);
  check string "ratelimit" "ratelimit" (Mode.category_to_string Mode.RateLimit);
  check string "encryption" "encryption" (Mode.category_to_string Mode.Encryption)

let test_category_of_string_valid () =
  check (option bool) "core" (Some true) (Option.map (fun c -> c = Mode.Core) (Mode.category_of_string "core"));
  check (option bool) "comm" (Some true) (Option.map (fun c -> c = Mode.Comm) (Mode.category_of_string "comm"));
  check (option bool) "portal" (Some true) (Option.map (fun c -> c = Mode.Portal) (Mode.category_of_string "portal"));
  check (option bool) "worktree" (Some true) (Option.map (fun c -> c = Mode.Worktree) (Mode.category_of_string "worktree"));
  check (option bool) "health" (Some true) (Option.map (fun c -> c = Mode.Health) (Mode.category_of_string "health"));
  check (option bool) "discovery" (Some true) (Option.map (fun c -> c = Mode.Discovery) (Mode.category_of_string "discovery"));
  check (option bool) "voting" (Some true) (Option.map (fun c -> c = Mode.Voting) (Mode.category_of_string "voting"));
  check (option bool) "interrupt" (Some true) (Option.map (fun c -> c = Mode.Interrupt) (Mode.category_of_string "interrupt"));
  check (option bool) "cost" (Some true) (Option.map (fun c -> c = Mode.Cost) (Mode.category_of_string "cost"));
  check (option bool) "auth" (Some true) (Option.map (fun c -> c = Mode.Auth) (Mode.category_of_string "auth"));
  check (option bool) "ratelimit" (Some true) (Option.map (fun c -> c = Mode.RateLimit) (Mode.category_of_string "ratelimit"));
  check (option bool) "encryption" (Some true) (Option.map (fun c -> c = Mode.Encryption) (Mode.category_of_string "encryption"))

let test_category_of_string_invalid () =
  check (option bool) "invalid" None (Option.map (fun _ -> true) (Mode.category_of_string "invalid"));
  check (option bool) "empty" None (Option.map (fun _ -> true) (Mode.category_of_string ""));
  check (option bool) "unknown" None (Option.map (fun _ -> true) (Mode.category_of_string "foobar"))

let test_category_roundtrip () =
  List.iter (fun cat ->
    let s = Mode.category_to_string cat in
    match Mode.category_of_string s with
    | Some cat' -> check bool ("roundtrip " ^ s) true (cat = cat')
    | None -> fail ("Failed to parse category: " ^ s)
  ) Mode.all_categories

(* ============================================================
   Mode Tests
   ============================================================ *)

let test_mode_to_string () =
  check string "minimal" "minimal" (Mode.mode_to_string Mode.Minimal);
  check string "standard" "standard" (Mode.mode_to_string Mode.Standard);
  check string "parallel" "parallel" (Mode.mode_to_string Mode.Parallel);
  check string "full" "full" (Mode.mode_to_string Mode.Full);
  check string "solo" "solo" (Mode.mode_to_string Mode.Solo);
  check string "custom" "custom" (Mode.mode_to_string Mode.Custom)

let test_mode_of_string_valid () =
  check (option bool) "minimal" (Some true) (Option.map (fun m -> m = Mode.Minimal) (Mode.mode_of_string "minimal"));
  check (option bool) "standard" (Some true) (Option.map (fun m -> m = Mode.Standard) (Mode.mode_of_string "standard"));
  check (option bool) "parallel" (Some true) (Option.map (fun m -> m = Mode.Parallel) (Mode.mode_of_string "parallel"));
  check (option bool) "full" (Some true) (Option.map (fun m -> m = Mode.Full) (Mode.mode_of_string "full"));
  check (option bool) "solo" (Some true) (Option.map (fun m -> m = Mode.Solo) (Mode.mode_of_string "solo"));
  check (option bool) "custom" (Some true) (Option.map (fun m -> m = Mode.Custom) (Mode.mode_of_string "custom"))

let test_mode_of_string_invalid () =
  check (option bool) "invalid" None (Option.map (fun _ -> true) (Mode.mode_of_string "invalid"));
  check (option bool) "empty" None (Option.map (fun _ -> true) (Mode.mode_of_string ""))

let test_mode_roundtrip () =
  let modes = [Mode.Minimal; Mode.Standard; Mode.Parallel; Mode.Full; Mode.Solo; Mode.Custom] in
  List.iter (fun mode ->
    let s = Mode.mode_to_string mode in
    match Mode.mode_of_string s with
    | Some mode' -> check bool ("roundtrip " ^ s) true (mode = mode')
    | None -> fail ("Failed to parse mode: " ^ s)
  ) modes

(* ============================================================
   All Categories Tests
   ============================================================ *)

let test_all_categories_count () =
  check int "12 categories" 12 (List.length Mode.all_categories)

let test_all_categories_unique () =
  let rec has_duplicates = function
    | [] -> false
    | x :: xs -> List.mem x xs || has_duplicates xs
  in
  check bool "no duplicates" false (has_duplicates Mode.all_categories)

(* ============================================================
   Categories for Mode Tests
   ============================================================ *)

let test_categories_minimal () =
  let cats = Mode.categories_for_mode Mode.Minimal in
  check bool "has core" true (List.mem Mode.Core cats);
  check bool "has health" true (List.mem Mode.Health cats);
  check int "count" 2 (List.length cats)

let test_categories_standard () =
  let cats = Mode.categories_for_mode Mode.Standard in
  check bool "has core" true (List.mem Mode.Core cats);
  check bool "has comm" true (List.mem Mode.Comm cats);
  check bool "has worktree" true (List.mem Mode.Worktree cats);
  check bool "has health" true (List.mem Mode.Health cats);
  check int "count" 4 (List.length cats)

let test_categories_parallel () =
  let cats = Mode.categories_for_mode Mode.Parallel in
  check bool "has core" true (List.mem Mode.Core cats);
  check bool "has comm" true (List.mem Mode.Comm cats);
  check bool "has portal" true (List.mem Mode.Portal cats);
  check bool "has worktree" true (List.mem Mode.Worktree cats);
  check bool "has health" true (List.mem Mode.Health cats);
  check bool "has discovery" true (List.mem Mode.Discovery cats);
  check bool "has voting" true (List.mem Mode.Voting cats);
  check bool "has interrupt" true (List.mem Mode.Interrupt cats);
  check int "count" 8 (List.length cats)

let test_categories_full () =
  let cats = Mode.categories_for_mode Mode.Full in
  check int "all categories" 12 (List.length cats)

let test_categories_solo () =
  let cats = Mode.categories_for_mode Mode.Solo in
  check bool "has core" true (List.mem Mode.Core cats);
  check bool "has worktree" true (List.mem Mode.Worktree cats);
  check int "count" 2 (List.length cats)

let test_categories_custom () =
  let cats = Mode.categories_for_mode Mode.Custom in
  check int "empty" 0 (List.length cats)

(* ============================================================
   Tool Category Tests
   ============================================================ *)

let test_tool_category_core () =
  check bool "join" true (Mode.tool_category "masc_join" = Mode.Core);
  check bool "status" true (Mode.tool_category "masc_status" = Mode.Core);
  check bool "claim" true (Mode.tool_category "masc_claim" = Mode.Core);
  check bool "done" true (Mode.tool_category "masc_done" = Mode.Core);
  check bool "tasks" true (Mode.tool_category "masc_tasks" = Mode.Core);
  check bool "add_task" true (Mode.tool_category "masc_add_task" = Mode.Core)

let test_tool_category_comm () =
  check bool "broadcast" true (Mode.tool_category "masc_broadcast" = Mode.Comm);
  check bool "messages" true (Mode.tool_category "masc_messages" = Mode.Comm)

let test_tool_category_portal () =
  check bool "portal_open" true (Mode.tool_category "masc_portal_open" = Mode.Portal);
  check bool "portal_send" true (Mode.tool_category "masc_portal_send" = Mode.Portal);
  check bool "portal_close" true (Mode.tool_category "masc_portal_close" = Mode.Portal)

let test_tool_category_worktree () =
  check bool "worktree_create" true (Mode.tool_category "masc_worktree_create" = Mode.Worktree);
  check bool "worktree_list" true (Mode.tool_category "masc_worktree_list" = Mode.Worktree)

let test_tool_category_health () =
  check bool "heartbeat" true (Mode.tool_category "masc_heartbeat" = Mode.Health);
  check bool "gc" true (Mode.tool_category "masc_gc" = Mode.Health);
  check bool "cache_set" true (Mode.tool_category "masc_cache_set" = Mode.Health);
  check bool "tempo" true (Mode.tool_category "masc_tempo" = Mode.Health)

let test_tool_category_discovery () =
  check bool "register_capabilities" true (Mode.tool_category "masc_register_capabilities" = Mode.Discovery);
  check bool "find_by_capability" true (Mode.tool_category "masc_find_by_capability" = Mode.Discovery)

let test_tool_category_voting () =
  check bool "vote_create" true (Mode.tool_category "masc_vote_create" = Mode.Voting);
  check bool "vote_cast" true (Mode.tool_category "masc_vote_cast" = Mode.Voting)

let test_tool_category_interrupt () =
  check bool "interrupt" true (Mode.tool_category "masc_interrupt" = Mode.Interrupt);
  check bool "approve" true (Mode.tool_category "masc_approve" = Mode.Interrupt)

let test_tool_category_cost () =
  check bool "cost_log" true (Mode.tool_category "masc_cost_log" = Mode.Cost);
  check bool "cost_report" true (Mode.tool_category "masc_cost_report" = Mode.Cost)

let test_tool_category_auth () =
  check bool "auth_enable" true (Mode.tool_category "masc_auth_enable" = Mode.Auth);
  check bool "auth_status" true (Mode.tool_category "masc_auth_status" = Mode.Auth)

let test_tool_category_ratelimit () =
  check bool "rate_limit_status" true (Mode.tool_category "masc_rate_limit_status" = Mode.RateLimit);
  check bool "rate_limit_config" true (Mode.tool_category "masc_rate_limit_config" = Mode.RateLimit)

let test_tool_category_encryption () =
  check bool "encryption_enable" true (Mode.tool_category "masc_encryption_enable" = Mode.Encryption);
  check bool "generate_key" true (Mode.tool_category "masc_generate_key" = Mode.Encryption)

let test_tool_category_unknown () =
  (* Unknown tools default to Core *)
  check bool "unknown defaults to core" true (Mode.tool_category "unknown_tool" = Mode.Core)

(* ============================================================
   Is Tool Enabled Tests
   ============================================================ *)

let test_is_tool_enabled_minimal () =
  let cats = Mode.categories_for_mode Mode.Minimal in
  check bool "join enabled" true (Mode.is_tool_enabled cats "masc_join");
  check bool "heartbeat enabled" true (Mode.is_tool_enabled cats "masc_heartbeat");
  check bool "broadcast disabled" false (Mode.is_tool_enabled cats "masc_broadcast");
  check bool "worktree_create disabled" false (Mode.is_tool_enabled cats "masc_worktree_create")

let test_is_tool_enabled_standard () =
  let cats = Mode.categories_for_mode Mode.Standard in
  check bool "join enabled" true (Mode.is_tool_enabled cats "masc_join");
  check bool "broadcast enabled" true (Mode.is_tool_enabled cats "masc_broadcast");
  check bool "worktree_create enabled" true (Mode.is_tool_enabled cats "masc_worktree_create");
  check bool "portal_open disabled" false (Mode.is_tool_enabled cats "masc_portal_open")

let test_is_tool_enabled_full () =
  let cats = Mode.categories_for_mode Mode.Full in
  check bool "join enabled" true (Mode.is_tool_enabled cats "masc_join");
  check bool "portal_open enabled" true (Mode.is_tool_enabled cats "masc_portal_open");
  check bool "encryption_enable enabled" true (Mode.is_tool_enabled cats "masc_encryption_enable")

(* ============================================================
   Description Tests
   ============================================================ *)

let test_mode_description () =
  let modes = [Mode.Minimal; Mode.Standard; Mode.Parallel; Mode.Full; Mode.Solo; Mode.Custom] in
  List.iter (fun mode ->
    let desc = Mode.mode_description mode in
    check bool "non-empty" true (String.length desc > 0)
  ) modes

let test_category_description () =
  List.iter (fun cat ->
    let desc = Mode.category_description cat in
    check bool "non-empty" true (String.length desc > 0)
  ) Mode.all_categories

(* ============================================================
   JSON Serialization Tests
   ============================================================ *)

let test_categories_to_json () =
  let cats = [Mode.Core; Mode.Comm] in
  match Mode.categories_to_json cats with
  | `List items ->
    check int "count" 2 (List.length items);
    (match items with
     | [`String "core"; `String "comm"] -> ()
     | _ -> fail "unexpected json")
  | _ -> fail "expected list"

let test_categories_to_json_empty () =
  let cats = [] in
  match Mode.categories_to_json cats with
  | `List items -> check int "empty" 0 (List.length items)
  | _ -> fail "expected list"

let test_categories_of_json () =
  let json = `List [`String "core"; `String "health"] in
  let cats = Mode.categories_of_json json in
  check int "count" 2 (List.length cats);
  check bool "has core" true (List.mem Mode.Core cats);
  check bool "has health" true (List.mem Mode.Health cats)

let test_categories_of_json_invalid () =
  let json = `String "not a list" in
  let cats = Mode.categories_of_json json in
  check int "empty on invalid" 0 (List.length cats)

let test_categories_of_json_filter_invalid () =
  let json = `List [`String "core"; `String "invalid"; `String "health"] in
  let cats = Mode.categories_of_json json in
  check int "filters invalid" 2 (List.length cats)

let test_categories_json_roundtrip () =
  let cats = [Mode.Core; Mode.Comm; Mode.Portal] in
  let json = Mode.categories_to_json cats in
  let cats' = Mode.categories_of_json json in
  check int "same length" (List.length cats) (List.length cats');
  List.iter (fun c -> check bool "contains" true (List.mem c cats')) cats

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Mode Coverage" [
    "category", [
      test_case "to_string" `Quick test_category_to_string;
      test_case "of_string valid" `Quick test_category_of_string_valid;
      test_case "of_string invalid" `Quick test_category_of_string_invalid;
      test_case "roundtrip" `Quick test_category_roundtrip;
    ];
    "mode", [
      test_case "to_string" `Quick test_mode_to_string;
      test_case "of_string valid" `Quick test_mode_of_string_valid;
      test_case "of_string invalid" `Quick test_mode_of_string_invalid;
      test_case "roundtrip" `Quick test_mode_roundtrip;
    ];
    "all_categories", [
      test_case "count" `Quick test_all_categories_count;
      test_case "unique" `Quick test_all_categories_unique;
    ];
    "categories_for_mode", [
      test_case "minimal" `Quick test_categories_minimal;
      test_case "standard" `Quick test_categories_standard;
      test_case "parallel" `Quick test_categories_parallel;
      test_case "full" `Quick test_categories_full;
      test_case "solo" `Quick test_categories_solo;
      test_case "custom" `Quick test_categories_custom;
    ];
    "tool_category", [
      test_case "core tools" `Quick test_tool_category_core;
      test_case "comm tools" `Quick test_tool_category_comm;
      test_case "portal tools" `Quick test_tool_category_portal;
      test_case "worktree tools" `Quick test_tool_category_worktree;
      test_case "health tools" `Quick test_tool_category_health;
      test_case "discovery tools" `Quick test_tool_category_discovery;
      test_case "voting tools" `Quick test_tool_category_voting;
      test_case "interrupt tools" `Quick test_tool_category_interrupt;
      test_case "cost tools" `Quick test_tool_category_cost;
      test_case "auth tools" `Quick test_tool_category_auth;
      test_case "ratelimit tools" `Quick test_tool_category_ratelimit;
      test_case "encryption tools" `Quick test_tool_category_encryption;
      test_case "unknown defaults to core" `Quick test_tool_category_unknown;
    ];
    "is_tool_enabled", [
      test_case "minimal mode" `Quick test_is_tool_enabled_minimal;
      test_case "standard mode" `Quick test_is_tool_enabled_standard;
      test_case "full mode" `Quick test_is_tool_enabled_full;
    ];
    "descriptions", [
      test_case "mode description" `Quick test_mode_description;
      test_case "category description" `Quick test_category_description;
    ];
    "json", [
      test_case "categories_to_json" `Quick test_categories_to_json;
      test_case "categories_to_json empty" `Quick test_categories_to_json_empty;
      test_case "categories_of_json" `Quick test_categories_of_json;
      test_case "categories_of_json invalid" `Quick test_categories_of_json_invalid;
      test_case "categories_of_json filter" `Quick test_categories_of_json_filter_invalid;
      test_case "roundtrip" `Quick test_categories_json_roundtrip;
    ];
  ]
