open Alcotest

let test_unregister_if_current () =
  let open Masc_mcp.Sse in
  let session_id = "test_session" in
  let noop _ = () in

  let id1 = register session_id ~push:noop ~last_event_id:0 in
  check bool "registered" true (exists session_id);

  (* Re-register same session_id (simulates reconnect) *)
  let id2 = register session_id ~push:noop ~last_event_id:0 in
  check bool "still registered" true (exists session_id);

  (* Old connection cleanup must not unregister the new connection *)
  unregister_if_current session_id id1;
  check bool "new connection survives old cleanup" true (exists session_id);

  (* Current connection cleanup should unregister *)
  unregister_if_current session_id id2;
  check bool "unregistered" false (exists session_id);
  ()

let () =
  run "sse"
    [
      ("unregister_if_current", [test_case "guards reconnect" `Quick test_unregister_if_current]);
    ]

