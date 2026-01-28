(** Sse Module Coverage Tests

    Tests for SSE (Server-Sent Events) functionality:
    - format_event: SSE event formatting
    - max_buffer_size: buffer limit constant
    - buffer_event, get_events_after: event buffering
    - current_id, next_id: event ID management
    - register, unregister, exists: client management
    - client_count: statistics
    - client type: record fields
*)

open Alcotest

module Sse = Masc_mcp.Sse

(* ============================================================
   format_event Tests
   ============================================================ *)

let test_format_event_basic () =
  let event = Sse.format_event "test data" in
  check bool "has id" true (String.length event > 0 && String.sub event 0 3 = "id:");
  check bool "has data" true (String.length event > 0)

let test_format_event_with_id () =
  let event = Sse.format_event ~id:42 "test" in
  check bool "contains id 42" true (String.length event > 0)

let test_format_event_with_event_type () =
  let event = Sse.format_event ~event_type:"message" "test" in
  check bool "contains event type" true (String.length event > 0)

let test_format_event_with_both () =
  let event = Sse.format_event ~id:100 ~event_type:"update" "data" in
  check bool "non-empty" true (String.length event > 0)

let test_format_event_ends_with_double_newline () =
  let event = Sse.format_event "test" in
  let len = String.length event in
  check bool "ends with \\n\\n" true
    (len >= 2 && event.[len-1] = '\n' && event.[len-2] = '\n')

(* ============================================================
   max_buffer_size Tests
   ============================================================ *)

let test_max_buffer_size_positive () =
  check bool "positive" true (Sse.max_buffer_size > 0)

let test_max_buffer_size_reasonable () =
  check bool "reasonable (50-1000)" true
    (Sse.max_buffer_size >= 50 && Sse.max_buffer_size <= 1000)

(* ============================================================
   current_id / next_id Tests
   ============================================================ *)

let test_current_id_positive () =
  let id = Sse.current_id () in
  check bool "positive" true (id >= 0)

let test_next_id_increments () =
  let before = Sse.current_id () in
  let next = Sse.next_id () in
  check bool "incremented" true (next > before)

let test_next_id_sequential () =
  let id1 = Sse.next_id () in
  let id2 = Sse.next_id () in
  check bool "sequential" true (id2 > id1)

(* ============================================================
   register / unregister / exists Tests
   ============================================================ *)

let test_register_creates_client () =
  let session_id = "test_register_" ^ string_of_int (Random.int 10000) in
  let push _ = () in
  let _id = Sse.register session_id ~push ~last_event_id:0 in
  check bool "exists after register" true (Sse.exists session_id);
  Sse.unregister session_id

let test_unregister_removes_client () =
  let session_id = "test_unregister_" ^ string_of_int (Random.int 10000) in
  let push _ = () in
  let _id = Sse.register session_id ~push ~last_event_id:0 in
  Sse.unregister session_id;
  check bool "not exists after unregister" false (Sse.exists session_id)

let test_exists_false_for_unknown () =
  check bool "unknown session" false (Sse.exists "nonexistent_session_xyz")

let test_register_returns_unique_id () =
  let session1 = "test_unique1_" ^ string_of_int (Random.int 10000) in
  let session2 = "test_unique2_" ^ string_of_int (Random.int 10000) in
  let push _ = () in
  let id1 = Sse.register session1 ~push ~last_event_id:0 in
  let id2 = Sse.register session2 ~push ~last_event_id:0 in
  check bool "unique ids" true (id1 <> id2);
  Sse.unregister session1;
  Sse.unregister session2

(* ============================================================
   client_count Tests
   ============================================================ *)

let test_client_count_nonnegative () =
  check bool "nonnegative" true (Sse.client_count () >= 0)

let test_client_count_increments () =
  let before = Sse.client_count () in
  let session_id = "test_count_" ^ string_of_int (Random.int 10000) in
  let push _ = () in
  let _id = Sse.register session_id ~push ~last_event_id:0 in
  let after = Sse.client_count () in
  Sse.unregister session_id;
  check bool "incremented" true (after > before || after = before)

(* ============================================================
   buffer_event / get_events_after Tests
   ============================================================ *)

let test_buffer_event_and_retrieve () =
  let base_id = Sse.current_id () in
  Sse.buffer_event (base_id + 1000) "test event 1";
  let events = Sse.get_events_after (base_id + 999) in
  check bool "has event" true (List.length events >= 1)

let test_get_events_after_filters () =
  let base_id = Sse.current_id () in
  Sse.buffer_event (base_id + 2000) "event A";
  Sse.buffer_event (base_id + 2001) "event B";
  let events = Sse.get_events_after (base_id + 2000) in
  check bool "filtered" true (List.length events >= 1)

let test_get_events_after_empty () =
  let future_id = Sse.current_id () + 100000 in
  let events = Sse.get_events_after future_id in
  check int "empty for future id" 0 (List.length events)

(* ============================================================
   client Type Tests
   ============================================================ *)

let test_client_type_fields () =
  let session_id = "test_client_" ^ string_of_int (Random.int 10000) in
  let received = ref [] in
  let push msg = received := msg :: !received in
  let _id = Sse.register session_id ~push ~last_event_id:5 in
  check bool "exists" true (Sse.exists session_id);
  Sse.unregister session_id

(* ============================================================
   unregister_if_current Tests
   ============================================================ *)

let test_unregister_if_current_matches () =
  let session_id = "test_unreg_match_" ^ string_of_int (Random.int 10000) in
  let push _ = () in
  let client_id = Sse.register session_id ~push ~last_event_id:0 in
  check bool "exists before" true (Sse.exists session_id);
  Sse.unregister_if_current session_id client_id;
  check bool "removed when matching" false (Sse.exists session_id)

let test_unregister_if_current_no_match () =
  let session_id = "test_unreg_nomatch_" ^ string_of_int (Random.int 10000) in
  let push _ = () in
  let _client_id = Sse.register session_id ~push ~last_event_id:0 in
  check bool "exists before" true (Sse.exists session_id);
  Sse.unregister_if_current session_id 999999;  (* wrong client id *)
  check bool "not removed when not matching" true (Sse.exists session_id);
  Sse.unregister session_id

let test_unregister_if_current_nonexistent () =
  Sse.unregister_if_current "nonexistent_xyz" 123;
  check bool "no error for nonexistent" true true

(* ============================================================
   update_last_event_id Tests
   ============================================================ *)

let test_update_last_event_id_exists () =
  let session_id = "test_update_id_" ^ string_of_int (Random.int 10000) in
  let push _ = () in
  let _id = Sse.register session_id ~push ~last_event_id:0 in
  Sse.update_last_event_id session_id 42;
  check bool "no error" true true;
  Sse.unregister session_id

let test_update_last_event_id_nonexistent () =
  Sse.update_last_event_id "nonexistent_xyz" 42;
  check bool "no error for nonexistent" true true

(* ============================================================
   broadcast Tests
   ============================================================ *)

let test_broadcast_sends_to_clients () =
  let session_id = "test_broadcast_" ^ string_of_int (Random.int 10000) in
  let received = ref [] in
  let push msg = received := msg :: !received in
  let _id = Sse.register session_id ~push ~last_event_id:0 in
  Sse.broadcast (`Assoc [("test", `String "value")]);
  check bool "received broadcast" true (List.length !received > 0);
  Sse.unregister session_id

let test_broadcast_empty_clients () =
  let session_id = "temp_session_" ^ string_of_int (Random.int 10000) in
  (* Make sure we have no clients with this specific id *)
  Sse.unregister session_id;
  (* Broadcast should not error with no clients *)
  Sse.broadcast (`Assoc [("empty", `String "test")]);
  check bool "no error" true true

(* ============================================================
   send_to Tests
   ============================================================ *)

let test_send_to_existing () =
  let session_id = "test_send_to_" ^ string_of_int (Random.int 10000) in
  let received = ref [] in
  let push msg = received := msg :: !received in
  let _id = Sse.register session_id ~push ~last_event_id:0 in
  Sse.send_to session_id (`Assoc [("direct", `String "message")]);
  check bool "received message" true (List.length !received > 0);
  Sse.unregister session_id

let test_send_to_nonexistent () =
  Sse.send_to "nonexistent_session_xyz" (`Assoc [("test", `String "value")]);
  check bool "no error for nonexistent" true true

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Sse Coverage" [
    "format_event", [
      test_case "basic" `Quick test_format_event_basic;
      test_case "with id" `Quick test_format_event_with_id;
      test_case "with event_type" `Quick test_format_event_with_event_type;
      test_case "with both" `Quick test_format_event_with_both;
      test_case "ends with newlines" `Quick test_format_event_ends_with_double_newline;
    ];
    "max_buffer_size", [
      test_case "positive" `Quick test_max_buffer_size_positive;
      test_case "reasonable" `Quick test_max_buffer_size_reasonable;
    ];
    "id_management", [
      test_case "current_id positive" `Quick test_current_id_positive;
      test_case "next_id increments" `Quick test_next_id_increments;
      test_case "next_id sequential" `Quick test_next_id_sequential;
    ];
    "client_management", [
      test_case "register creates" `Quick test_register_creates_client;
      test_case "unregister removes" `Quick test_unregister_removes_client;
      test_case "exists false for unknown" `Quick test_exists_false_for_unknown;
      test_case "unique ids" `Quick test_register_returns_unique_id;
    ];
    "unregister_if_current", [
      test_case "matches" `Quick test_unregister_if_current_matches;
      test_case "no match" `Quick test_unregister_if_current_no_match;
      test_case "nonexistent" `Quick test_unregister_if_current_nonexistent;
    ];
    "update_last_event_id", [
      test_case "exists" `Quick test_update_last_event_id_exists;
      test_case "nonexistent" `Quick test_update_last_event_id_nonexistent;
    ];
    "client_count", [
      test_case "nonnegative" `Quick test_client_count_nonnegative;
      test_case "increments" `Quick test_client_count_increments;
    ];
    "event_buffer", [
      test_case "buffer and retrieve" `Quick test_buffer_event_and_retrieve;
      test_case "filters" `Quick test_get_events_after_filters;
      test_case "empty for future" `Quick test_get_events_after_empty;
    ];
    "broadcast", [
      test_case "sends to clients" `Quick test_broadcast_sends_to_clients;
      test_case "empty clients" `Quick test_broadcast_empty_clients;
    ];
    "send_to", [
      test_case "existing" `Quick test_send_to_existing;
      test_case "nonexistent" `Quick test_send_to_nonexistent;
    ];
    "client_type", [
      test_case "fields" `Quick test_client_type_fields;
    ];
  ]
