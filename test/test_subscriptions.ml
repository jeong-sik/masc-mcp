(** Tests for Resource Subscriptions - MCP 2025-11-25 Spec *)

open Alcotest

module Subscriptions = Masc_mcp.Subscriptions
module Encryption = Masc_mcp.Encryption

(* Initialize RNG for crypto *)
let () = Encryption.initialize ()

(* Helper: check if s2 is substring of s1 *)
let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

(** Resource type conversion tests *)
let test_resource_type_roundtrip () =
  let types = [
    Subscriptions.Tasks;
    Subscriptions.Agents;
    Subscriptions.Messages;
    Subscriptions.Votes;
    Subscriptions.Custom "custom_resource";
  ] in
  List.iter (fun t ->
    let s = Subscriptions.resource_type_to_string t in
    let t' = Subscriptions.resource_type_of_string s in
    check bool ("roundtrip: " ^ s) true (t = t')
  ) types

let test_change_type_to_string () =
  check string "created" "created" (Subscriptions.change_type_to_string Subscriptions.Created);
  check string "updated" "updated" (Subscriptions.change_type_to_string Subscriptions.Updated);
  check string "deleted" "deleted" (Subscriptions.change_type_to_string Subscriptions.Deleted)

(** Subscription tests *)
let test_subscribe () =
  let sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"claude"
    ~resource:Subscriptions.Tasks
    () in
  check bool "has id" true (String.length sub.id > 0);
  check string "subscriber" "claude" sub.subscriber;
  check bool "resource is Tasks" true (sub.resource = Subscriptions.Tasks);
  check (option string) "no filter" None sub.filter

let test_subscribe_with_filter () =
  let sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"gemini"
    ~resource:Subscriptions.Tasks
    ~filter:"task-001"
    () in
  check (option string) "has filter" (Some "task-001") sub.filter

let test_unsubscribe () =
  let sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"codex"
    ~resource:Subscriptions.Messages
    () in
  let id = sub.id in
  let result = Subscriptions.SubscriptionStore.unsubscribe id in
  check bool "unsubscribe success" true result;
  let found = Subscriptions.SubscriptionStore.get id in
  check bool "subscription removed" true (Option.is_none found)

let test_unsubscribe_not_found () =
  let result = Subscriptions.SubscriptionStore.unsubscribe "nonexistent_sub" in
  check bool "unsubscribe fails for unknown" false result

let test_get_subscription () =
  let sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"agent1"
    ~resource:Subscriptions.Agents
    () in
  let found = Subscriptions.SubscriptionStore.get sub.id in
  check bool "found" true (Option.is_some found);
  check string "same id" sub.id (Option.get found).id

(** Matching tests *)
let test_find_matching_no_filter () =
  let _sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"watcher"
    ~resource:Subscriptions.Tasks
    () in
  let matches = Subscriptions.SubscriptionStore.find_matching
    ~resource:Subscriptions.Tasks
    ~resource_id:"any-task" in
  check bool "finds match" true (List.length matches > 0)

let test_find_matching_with_filter () =
  let _sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"specific_watcher"
    ~resource:Subscriptions.Tasks
    ~filter:"task-specific"
    () in
  let matches = Subscriptions.SubscriptionStore.find_matching
    ~resource:Subscriptions.Tasks
    ~resource_id:"task-specific" in
  check bool "finds specific match" true (List.exists (fun (s : Subscriptions.subscription) -> s.filter = Some "task-specific") matches)

let test_find_matching_wildcard () =
  let _sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"wildcard_watcher"
    ~resource:Subscriptions.Messages
    ~filter:"*"
    () in
  let matches = Subscriptions.SubscriptionStore.find_matching
    ~resource:Subscriptions.Messages
    ~resource_id:"any-message" in
  check bool "wildcard matches" true (List.exists (fun (s : Subscriptions.subscription) -> s.filter = Some "*") matches)

let test_find_matching_wrong_resource () =
  let sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"tasks_only"
    ~resource:Subscriptions.Tasks
    () in
  let matches = Subscriptions.SubscriptionStore.find_matching
    ~resource:Subscriptions.Votes
    ~resource_id:"vote-001" in
  check bool "no match for different resource" false (List.exists (fun (s : Subscriptions.subscription) -> s.id = sub.id) matches)

(** Notification tests *)
let test_notify_change () =
  let _sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"notified_agent"
    ~resource:Subscriptions.Tasks
    () in
  let count = Subscriptions.notify_change
    ~resource:Subscriptions.Tasks
    ~change:Subscriptions.Created
    ~resource_id:"new-task"
    ~data:(`Assoc [("title", `String "Test Task")]) in
  check bool "at least one notified" true (count > 0)

let test_pop_notifications () =
  let sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"poll_agent"
    ~resource:Subscriptions.Agents
    () in
  let _ = Subscriptions.notify_change
    ~resource:Subscriptions.Agents
    ~change:Subscriptions.Updated
    ~resource_id:"agent-001"
    ~data:(`Assoc [("status", `String "active")]) in
  let notifs = Subscriptions.SubscriptionStore.pop_notifications sub.id in
  check bool "has notifications" true (List.length notifs > 0);
  (* Second pop should be empty *)
  let notifs2 = Subscriptions.SubscriptionStore.pop_notifications sub.id in
  check int "notifications consumed" 0 (List.length notifs2)

(** Tool handler tests *)
let test_tool_subscribe () =
  let args = `Assoc [
    ("action", `String "subscribe");
    ("subscriber", `String "tool_agent");
    ("resource", `String "tasks");
  ] in
  let (success, result) = Subscriptions.handle_subscription_tool args in
  check bool "subscribe success" true success;
  check bool "result has id" true (contains result "id")

let test_tool_unsubscribe () =
  let sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"unsub_agent"
    ~resource:Subscriptions.Tasks
    () in
  let args = `Assoc [
    ("action", `String "unsubscribe");
    ("subscription_id", `String sub.id);
  ] in
  let (success, _) = Subscriptions.handle_subscription_tool args in
  check bool "unsubscribe success" true success

let test_tool_list () =
  let _ = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"list_agent"
    ~resource:Subscriptions.Tasks
    () in
  let args = `Assoc [("action", `String "list")] in
  let (success, result) = Subscriptions.handle_subscription_tool args in
  check bool "list success" true success;
  check bool "has count" true (contains result "count")

let test_tool_list_by_subscriber () =
  let _ = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"specific_list_agent"
    ~resource:Subscriptions.Tasks
    () in
  let args = `Assoc [
    ("action", `String "list");
    ("subscriber", `String "specific_list_agent");
  ] in
  let (success, result) = Subscriptions.handle_subscription_tool args in
  check bool "list by subscriber success" true success;
  check bool "has subscriptions" true (contains result "subscriptions")

let test_tool_poll () =
  let sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"poll_tool_agent"
    ~resource:Subscriptions.Tasks
    () in
  let args = `Assoc [
    ("action", `String "poll");
    ("subscription_id", `String sub.id);
  ] in
  let (success, result) = Subscriptions.handle_subscription_tool args in
  check bool "poll success" true success;
  check bool "has notifications array" true (contains result "notifications")

let test_tool_invalid_action () =
  let args = `Assoc [("action", `String "invalid")] in
  let (success, _) = Subscriptions.handle_subscription_tool args in
  check bool "invalid action fails" false success

let test_tool_missing_action () =
  let args = `Assoc [] in
  let (success, _) = Subscriptions.handle_subscription_tool args in
  check bool "missing action fails" false success

(** JSON serialization *)
let test_subscription_to_json () =
  let sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"json_agent"
    ~resource:Subscriptions.Tasks
    () in
  let json = Subscriptions.subscription_to_json sub in
  match json with
  | `Assoc fields ->
    check bool "has id" true (List.mem_assoc "id" fields);
    check bool "has subscriber" true (List.mem_assoc "subscriber" fields);
    check bool "has resource" true (List.mem_assoc "resource" fields)
  | _ -> fail "Expected Assoc"

let test_notification_to_json () =
  let notif : Subscriptions.notification = {
    subscription_id = "sub-123";
    resource = Subscriptions.Tasks;
    change = Subscriptions.Created;
    resource_id = "task-456";
    data = `Assoc [("title", `String "Test")];
    timestamp = Unix.gettimeofday ();
  } in
  let json = Subscriptions.notification_to_json notif in
  match json with
  | `Assoc fields ->
    check bool "has subscription_id" true (List.mem_assoc "subscription_id" fields);
    check bool "has change" true (List.mem_assoc "change" fields);
    check bool "has data" true (List.mem_assoc "data" fields)
  | _ -> fail "Expected Assoc"

(** Hook function tests *)
let test_notify_task_change () =
  let _sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"task_hook_agent"
    ~resource:Subscriptions.Tasks
    () in
  (* Should not raise *)
  Subscriptions.notify_task_change
    ~change:Subscriptions.Created
    ~task_id:"hook-task"
    ~data:(`Assoc []);
  check bool "task hook executed" true true

let test_notify_agent_change () =
  let _sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"agent_hook_agent"
    ~resource:Subscriptions.Agents
    () in
  Subscriptions.notify_agent_change
    ~change:Subscriptions.Updated
    ~agent_name:"agent-x"
    ~data:(`Assoc []);
  check bool "agent hook executed" true true

let test_notify_message () =
  let _sub = Subscriptions.SubscriptionStore.subscribe
    ~subscriber:"msg_hook_agent"
    ~resource:Subscriptions.Messages
    () in
  Subscriptions.notify_message
    ~message_id:"msg-001"
    ~data:(`Assoc []);
  check bool "message hook executed" true true

(** Test suites *)
let type_tests = [
  "resource type roundtrip", `Quick, test_resource_type_roundtrip;
  "change type to string", `Quick, test_change_type_to_string;
]

let subscription_tests = [
  "subscribe", `Quick, test_subscribe;
  "subscribe with filter", `Quick, test_subscribe_with_filter;
  "unsubscribe", `Quick, test_unsubscribe;
  "unsubscribe not found", `Quick, test_unsubscribe_not_found;
  "get subscription", `Quick, test_get_subscription;
]

let matching_tests = [
  "find matching no filter", `Quick, test_find_matching_no_filter;
  "find matching with filter", `Quick, test_find_matching_with_filter;
  "find matching wildcard", `Quick, test_find_matching_wildcard;
  "find matching wrong resource", `Quick, test_find_matching_wrong_resource;
]

let notification_tests = [
  "notify change", `Quick, test_notify_change;
  "pop notifications", `Quick, test_pop_notifications;
]

let tool_tests = [
  "subscribe action", `Quick, test_tool_subscribe;
  "unsubscribe action", `Quick, test_tool_unsubscribe;
  "list action", `Quick, test_tool_list;
  "list by subscriber", `Quick, test_tool_list_by_subscriber;
  "poll action", `Quick, test_tool_poll;
  "invalid action", `Quick, test_tool_invalid_action;
  "missing action", `Quick, test_tool_missing_action;
]

let json_tests = [
  "subscription_to_json", `Quick, test_subscription_to_json;
  "notification_to_json", `Quick, test_notification_to_json;
]

let hook_tests = [
  "notify_task_change", `Quick, test_notify_task_change;
  "notify_agent_change", `Quick, test_notify_agent_change;
  "notify_message", `Quick, test_notify_message;
]

let () =
  run "Subscriptions" [
    "types", type_tests;
    "subscription", subscription_tests;
    "matching", matching_tests;
    "notification", notification_tests;
    "tool", tool_tests;
    "json", json_tests;
    "hooks", hook_tests;
  ]
