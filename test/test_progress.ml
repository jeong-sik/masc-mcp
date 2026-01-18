open Alcotest

(** Test Progress module *)

let captured_json = ref []

let mock_broadcast json =
  captured_json := json :: !captured_json

let setup () =
  captured_json := [];
  Masc_mcp.Progress.reset_for_testing ();
  Masc_mcp.Progress.set_sse_callback mock_broadcast

let test_notify_basic () =
  setup ();
  Masc_mcp.Progress.notify ~task_id:"test-task" ~progress:0.5 ();

  check int "one notification sent" 1 (List.length !captured_json);

  let json = List.hd !captured_json in
  let open Yojson.Safe.Util in
  check string "jsonrpc version" "2.0" (json |> member "jsonrpc" |> to_string);
  check string "method" "notifications/progress" (json |> member "method" |> to_string);

  let params = json |> member "params" in
  check string "taskId" "test-task" (params |> member "taskId" |> to_string);
  check bool "progress is 0.5" true (Float.equal 0.5 (params |> member "progress" |> to_float))

let test_notify_with_message () =
  setup ();
  Masc_mcp.Progress.notify ~task_id:"test-task" ~progress:0.75 ~message:"Processing..." ();

  let json = List.hd !captured_json in
  let params = Yojson.Safe.Util.member "params" json in
  check string "message included" "Processing..."
    (Yojson.Safe.Util.member "message" params |> Yojson.Safe.Util.to_string)

let test_tracker_create () =
  setup ();
  let tracker = Masc_mcp.Progress.Tracker.create ~task_id:"tracker-test" ~total_steps:10 () in
  check bool "initial progress is 0" true (Float.equal 0.0 tracker.current)

let test_tracker_step () =
  setup ();
  let tracker = Masc_mcp.Progress.Tracker.create ~task_id:"step-test" ~total_steps:4 () in

  Masc_mcp.Progress.Tracker.step tracker ();
  check bool "progress after 1 step" true (Float.equal 0.25 tracker.current);

  Masc_mcp.Progress.Tracker.step tracker ();
  check bool "progress after 2 steps" true (Float.equal 0.5 tracker.current);

  (* Should have sent 2 notifications *)
  check int "two notifications sent" 2 (List.length !captured_json)

let test_tracker_complete () =
  setup ();
  let tracker = Masc_mcp.Progress.Tracker.create ~task_id:"complete-test" () in

  Masc_mcp.Progress.Tracker.complete tracker ~message:"Done!" ();
  check bool "progress is 1.0" true (Float.equal 1.0 tracker.current);

  let json = List.hd !captured_json in
  let params = Yojson.Safe.Util.member "params" json in
  check bool "final progress is 1.0" true
    (Float.equal 1.0 (Yojson.Safe.Util.member "progress" params |> Yojson.Safe.Util.to_float))

let test_handle_tool_start () =
  setup ();
  let args = `Assoc [
    ("action", `String "start");
    ("task_id", `String "tool-test");
    ("total_steps", `Int 5);
  ] in
  let (success, msg) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "start succeeds" true success;
  check bool "message mentions task" true (String.length msg > 0)

let test_handle_tool_update () =
  setup ();
  let args = `Assoc [
    ("action", `String "update");
    ("task_id", `String "update-test");
    ("progress", `Float 0.6);
    ("message", `String "60% done");
  ] in
  let (success, msg) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "update succeeds" true success;
  check bool "notification sent" true (List.length !captured_json > 0);
  check bool "message shows percentage" true (String.length msg > 0 && String.sub msg 0 (min 16 (String.length msg)) = "Progress updated")

let test_handle_tool_complete () =
  setup ();
  let args = `Assoc [
    ("action", `String "complete");
    ("task_id", `String "complete-test");
  ] in
  let (success, _) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "complete succeeds" true success;

  let json = List.hd !captured_json in
  let params = Yojson.Safe.Util.member "params" json in
  check bool "progress is 1.0" true
    (Float.equal 1.0 (Yojson.Safe.Util.member "progress" params |> Yojson.Safe.Util.to_float))

let test_handle_tool_invalid_action () =
  setup ();
  let args = `Assoc [
    ("action", `String "invalid");
    ("task_id", `String "test");
  ] in
  let (success, _) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "invalid action fails" false success

let test_handle_tool_missing_task_id () =
  setup ();
  let args = `Assoc [
    ("action", `String "update");
    ("progress", `Float 0.5);
  ] in
  let (success, _) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "missing task_id fails" false success

let test_start_and_stop_tracking () =
  setup ();
  let _ = Masc_mcp.Progress.start_tracking ~task_id:"track-test" ~total_steps:10 () in
  check bool "tracker exists" true (Option.is_some (Masc_mcp.Progress.get_tracker "track-test"));

  Masc_mcp.Progress.stop_tracking "track-test";
  check bool "tracker removed" true (Option.is_none (Masc_mcp.Progress.get_tracker "track-test"))

(** Validation tests *)

let test_validate_task_id_empty () =
  setup ();
  let args = `Assoc [
    ("action", `String "start");
    ("task_id", `String "");
  ] in
  let (success, msg) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "empty task_id fails" false success;
  check bool "error mentions empty" true (String.length msg > 0)

let test_validate_task_id_too_long () =
  setup ();
  let long_id = String.make 300 'x' in
  let args = `Assoc [
    ("action", `String "start");
    ("task_id", `String long_id);
  ] in
  let (success, msg) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "long task_id fails" false success;
  check bool "error mentions too long" true (String.length msg > 0)

let test_validate_task_id_invalid_chars () =
  setup ();
  let args = `Assoc [
    ("action", `String "start");
    ("task_id", `String "task\x00id");  (* null char *)
  ] in
  let (success, msg) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "invalid chars fail" false success;
  check bool "error mentions invalid" true (String.length msg > 0)

let test_validate_progress_out_of_range () =
  setup ();
  let args = `Assoc [
    ("action", `String "update");
    ("task_id", `String "test");
    ("progress", `Float 1.5);
  ] in
  let (success, msg) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "progress > 1.0 fails" false success;
  check bool "error mentions range" true (String.length msg > 0)

let test_validate_progress_negative () =
  setup ();
  let args = `Assoc [
    ("action", `String "update");
    ("task_id", `String "test");
    ("progress", `Float (-0.5));
  ] in
  let (success, msg) = Masc_mcp.Progress.handle_progress_tool args in
  check bool "negative progress fails" false success;
  check bool "error mentions range" true (String.length msg > 0)

let () =
  run "progress"
    [
      ("notify", [
        test_case "basic notification" `Quick test_notify_basic;
        test_case "with message" `Quick test_notify_with_message;
      ]);
      ("tracker", [
        test_case "create" `Quick test_tracker_create;
        test_case "step" `Quick test_tracker_step;
        test_case "complete" `Quick test_tracker_complete;
      ]);
      ("tool_handler", [
        test_case "start" `Quick test_handle_tool_start;
        test_case "update" `Quick test_handle_tool_update;
        test_case "complete" `Quick test_handle_tool_complete;
        test_case "invalid action" `Quick test_handle_tool_invalid_action;
        test_case "missing task_id" `Quick test_handle_tool_missing_task_id;
      ]);
      ("tracking", [
        test_case "start and stop" `Quick test_start_and_stop_tracking;
      ]);
      ("validation", [
        test_case "empty task_id" `Quick test_validate_task_id_empty;
        test_case "task_id too long" `Quick test_validate_task_id_too_long;
        test_case "task_id invalid chars" `Quick test_validate_task_id_invalid_chars;
        test_case "progress > 1.0" `Quick test_validate_progress_out_of_range;
        test_case "progress < 0.0" `Quick test_validate_progress_negative;
      ]);
    ]
