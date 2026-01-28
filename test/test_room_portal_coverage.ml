(** Room Portal Module Coverage Tests

    Tests for A2A protocol utilities:
    - gen_a2a_task_id function
*)

open Alcotest

module Room_portal = Masc_mcp.Room_portal

(* ============================================================
   gen_a2a_task_id Tests
   ============================================================ *)

let test_gen_a2a_task_id_format () =
  let id = Room_portal.gen_a2a_task_id () in
  check bool "starts with a2a-" true (String.sub id 0 4 = "a2a-")

let test_gen_a2a_task_id_length () =
  let id = Room_portal.gen_a2a_task_id () in
  (* Format: a2a-YYYYMMDDHHMMSS-XXXX = 4 + 14 + 1 + 4 = 23 chars *)
  check bool "reasonable length" true (String.length id >= 20)

let test_gen_a2a_task_id_unique () =
  let id1 = Room_portal.gen_a2a_task_id () in
  (* Small delay to ensure different random suffix *)
  Unix.sleepf 0.001;
  let id2 = Room_portal.gen_a2a_task_id () in
  (* Random suffix should make them different *)
  check bool "ids likely different" true (id1 <> id2)

let test_gen_a2a_task_id_has_timestamp () =
  let id = Room_portal.gen_a2a_task_id () in
  (* Should have numeric timestamp portion after "a2a-" *)
  let timestamp_part = String.sub id 4 14 in
  check bool "timestamp is numeric" true
    (String.for_all (fun c -> c >= '0' && c <= '9') timestamp_part)

let test_gen_a2a_task_id_multiple () =
  let ids = List.init 5 (fun _ -> Room_portal.gen_a2a_task_id ()) in
  (* All should start with a2a- *)
  let all_valid = List.for_all (fun id ->
    String.length id >= 4 && String.sub id 0 4 = "a2a-"
  ) ids in
  check bool "all valid format" true all_valid

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Room Portal Coverage" [
    "gen_a2a_task_id", [
      test_case "format" `Quick test_gen_a2a_task_id_format;
      test_case "length" `Quick test_gen_a2a_task_id_length;
      test_case "unique" `Quick test_gen_a2a_task_id_unique;
      test_case "has timestamp" `Quick test_gen_a2a_task_id_has_timestamp;
      test_case "multiple" `Quick test_gen_a2a_task_id_multiple;
    ];
  ]
