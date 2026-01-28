(** Subscriptions Module Coverage Tests

    Tests for MCP Resource Subscriptions:
    - resource_type: Tasks, Agents, Messages, Votes, Custom
    - resource_type_to_string, resource_type_of_string: conversions
    - change_type: Created, Updated, Deleted
    - change_type_to_string: conversion
    - subscription record type
    - notification record type
*)

open Alcotest

module Subscriptions = Masc_mcp.Subscriptions

(* ============================================================
   resource_type Tests
   ============================================================ *)

let test_resource_type_tasks () =
  check string "tasks" "tasks"
    (Subscriptions.resource_type_to_string Subscriptions.Tasks)

let test_resource_type_agents () =
  check string "agents" "agents"
    (Subscriptions.resource_type_to_string Subscriptions.Agents)

let test_resource_type_messages () =
  check string "messages" "messages"
    (Subscriptions.resource_type_to_string Subscriptions.Messages)

let test_resource_type_votes () =
  check string "votes" "votes"
    (Subscriptions.resource_type_to_string Subscriptions.Votes)

let test_resource_type_custom () =
  check string "custom" "my_resource"
    (Subscriptions.resource_type_to_string (Subscriptions.Custom "my_resource"))

(* ============================================================
   resource_type_of_string Tests
   ============================================================ *)

let test_resource_type_of_tasks () =
  match Subscriptions.resource_type_of_string "tasks" with
  | Subscriptions.Tasks -> ()
  | _ -> fail "expected Tasks"

let test_resource_type_of_agents () =
  match Subscriptions.resource_type_of_string "agents" with
  | Subscriptions.Agents -> ()
  | _ -> fail "expected Agents"

let test_resource_type_of_messages () =
  match Subscriptions.resource_type_of_string "messages" with
  | Subscriptions.Messages -> ()
  | _ -> fail "expected Messages"

let test_resource_type_of_votes () =
  match Subscriptions.resource_type_of_string "votes" with
  | Subscriptions.Votes -> ()
  | _ -> fail "expected Votes"

let test_resource_type_of_custom () =
  match Subscriptions.resource_type_of_string "unknown" with
  | Subscriptions.Custom s -> check string "custom value" "unknown" s
  | _ -> fail "expected Custom"

let test_resource_type_roundtrip_tasks () =
  let rt = Subscriptions.Tasks in
  let s = Subscriptions.resource_type_to_string rt in
  let rt' = Subscriptions.resource_type_of_string s in
  check bool "roundtrip" true (rt = rt')

let test_resource_type_roundtrip_custom () =
  let rt = Subscriptions.Custom "my_type" in
  let s = Subscriptions.resource_type_to_string rt in
  let rt' = Subscriptions.resource_type_of_string s in
  match rt' with
  | Subscriptions.Custom "my_type" -> ()
  | _ -> fail "custom roundtrip failed"

(* ============================================================
   change_type Tests
   ============================================================ *)

let test_change_type_created () =
  check string "created" "created"
    (Subscriptions.change_type_to_string Subscriptions.Created)

let test_change_type_updated () =
  check string "updated" "updated"
    (Subscriptions.change_type_to_string Subscriptions.Updated)

let test_change_type_deleted () =
  check string "deleted" "deleted"
    (Subscriptions.change_type_to_string Subscriptions.Deleted)

(* ============================================================
   subscription Record Tests
   ============================================================ *)

let test_subscription_record () =
  let sub : Subscriptions.subscription = {
    id = "sub-001";
    subscriber = "claude";
    resource = Subscriptions.Tasks;
    filter = Some "task-123";
    created_at = 1000.0;
  } in
  check string "id" "sub-001" sub.id;
  check string "subscriber" "claude" sub.subscriber;
  check bool "resource is Tasks" true (sub.resource = Subscriptions.Tasks);
  check (option string) "filter" (Some "task-123") sub.filter;
  check bool "created_at" true (sub.created_at = 1000.0)

let test_subscription_no_filter () =
  let sub : Subscriptions.subscription = {
    id = "sub-002";
    subscriber = "gemini";
    resource = Subscriptions.Agents;
    filter = None;
    created_at = 2000.0;
  } in
  check (option string) "no filter" None sub.filter

let test_subscription_custom_resource () =
  let sub : Subscriptions.subscription = {
    id = "sub-003";
    subscriber = "codex";
    resource = Subscriptions.Custom "events";
    filter = None;
    created_at = 3000.0;
  } in
  match sub.resource with
  | Subscriptions.Custom "events" -> ()
  | _ -> fail "expected Custom events"

(* ============================================================
   notification Record Tests
   ============================================================ *)

let test_notification_record () =
  let notif : Subscriptions.notification = {
    subscription_id = "sub-001";
    resource = Subscriptions.Tasks;
    change = Subscriptions.Created;
    resource_id = "task-123";
    data = `Assoc [("status", `String "pending")];
    timestamp = 1234567890.0;
  } in
  check string "subscription_id" "sub-001" notif.subscription_id;
  check bool "resource is Tasks" true (notif.resource = Subscriptions.Tasks);
  check bool "change is Created" true (notif.change = Subscriptions.Created);
  check string "resource_id" "task-123" notif.resource_id;
  check bool "timestamp" true (notif.timestamp = 1234567890.0)

let test_notification_updated () =
  let notif : Subscriptions.notification = {
    subscription_id = "sub-002";
    resource = Subscriptions.Agents;
    change = Subscriptions.Updated;
    resource_id = "agent-001";
    data = `Null;
    timestamp = 0.0;
  } in
  check bool "change is Updated" true (notif.change = Subscriptions.Updated)

let test_notification_deleted () =
  let notif : Subscriptions.notification = {
    subscription_id = "sub-003";
    resource = Subscriptions.Messages;
    change = Subscriptions.Deleted;
    resource_id = "msg-001";
    data = `Null;
    timestamp = 0.0;
  } in
  check bool "change is Deleted" true (notif.change = Subscriptions.Deleted)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Subscriptions Coverage" [
    "resource_type_to_string", [
      test_case "tasks" `Quick test_resource_type_tasks;
      test_case "agents" `Quick test_resource_type_agents;
      test_case "messages" `Quick test_resource_type_messages;
      test_case "votes" `Quick test_resource_type_votes;
      test_case "custom" `Quick test_resource_type_custom;
    ];
    "resource_type_of_string", [
      test_case "tasks" `Quick test_resource_type_of_tasks;
      test_case "agents" `Quick test_resource_type_of_agents;
      test_case "messages" `Quick test_resource_type_of_messages;
      test_case "votes" `Quick test_resource_type_of_votes;
      test_case "custom" `Quick test_resource_type_of_custom;
      test_case "roundtrip tasks" `Quick test_resource_type_roundtrip_tasks;
      test_case "roundtrip custom" `Quick test_resource_type_roundtrip_custom;
    ];
    "change_type", [
      test_case "created" `Quick test_change_type_created;
      test_case "updated" `Quick test_change_type_updated;
      test_case "deleted" `Quick test_change_type_deleted;
    ];
    "subscription", [
      test_case "record" `Quick test_subscription_record;
      test_case "no filter" `Quick test_subscription_no_filter;
      test_case "custom resource" `Quick test_subscription_custom_resource;
    ];
    "notification", [
      test_case "record" `Quick test_notification_record;
      test_case "updated" `Quick test_notification_updated;
      test_case "deleted" `Quick test_notification_deleted;
    ];
  ]
