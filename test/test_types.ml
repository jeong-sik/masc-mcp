(** Tests for Types module *)

open Masc_mcp.Types

let test_agent_status_roundtrip () =
  let statuses = [Active; Busy; Listening; Inactive] in
  List.iter (fun status ->
    let json = agent_status_to_yojson status in
    match agent_status_of_yojson json with
    | Ok parsed -> Alcotest.(check string) "roundtrip" (show_agent_status status) (show_agent_status parsed)
    | Error e -> Alcotest.fail e
  ) statuses

let test_task_status_todo () =
  let status = Todo in
  let json = task_status_to_yojson status in
  match task_status_of_yojson json with
  | Ok Todo -> ()  (* Pattern match to verify it's Todo *)
  | Ok _ -> Alcotest.fail "expected Todo"
  | Error e -> Alcotest.fail e

let test_task_status_claimed () =
  let status = Claimed { assignee = "claude"; claimed_at = "2024-01-01T00:00:00Z" } in
  let json = task_status_to_yojson status in
  match task_status_of_yojson json with
  | Ok (Claimed { assignee; _ }) -> Alcotest.(check string) "assignee" "claude" assignee
  | Ok _ -> Alcotest.fail "wrong variant"
  | Error e -> Alcotest.fail e

let test_task_status_done () =
  let status = Done { assignee = "gemini"; completed_at = "2024-01-01T00:00:00Z"; notes = Some "test" } in
  let json = task_status_to_yojson status in
  match task_status_of_yojson json with
  | Ok (Done { notes = Some n; _ }) -> Alcotest.(check string) "notes" "test" n
  | Ok _ -> Alcotest.fail "wrong variant or missing notes"
  | Error e -> Alcotest.fail e

let test_message_roundtrip () =
  let msg = {
    seq = 1;
    from_agent = "claude";
    msg_type = "broadcast";
    content = "Hello @gemini!";
    mention = Some "gemini";
    timestamp = "2024-01-01T00:00:00Z";
  } in
  let json = message_to_yojson msg in
  match message_of_yojson json with
  | Ok parsed ->
      Alcotest.(check int) "seq" 1 parsed.seq;
      Alcotest.(check string) "from" "claude" parsed.from_agent;
      Alcotest.(check (option string)) "mention" (Some "gemini") parsed.mention
  | Error e -> Alcotest.fail e

let () =
  Alcotest.run "Types" [
    "agent_status", [
      Alcotest.test_case "roundtrip" `Quick test_agent_status_roundtrip;
    ];
    "task_status", [
      Alcotest.test_case "todo" `Quick test_task_status_todo;
      Alcotest.test_case "claimed" `Quick test_task_status_claimed;
      Alcotest.test_case "done" `Quick test_task_status_done;
    ];
    "message", [
      Alcotest.test_case "roundtrip" `Quick test_message_roundtrip;
    ];
  ]
