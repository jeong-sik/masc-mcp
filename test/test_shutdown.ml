open Alcotest

(** Test Shutdown module *)

let setup () =
  Masc_mcp.Shutdown.reset_for_testing ()

let test_initial_state () =
  setup ();
  check bool "not shutting down initially" false (Masc_mcp.Shutdown.is_shutting_down ());
  check int "no active requests initially" 0 (Masc_mcp.Shutdown.active_count ())

let test_incr_decr_requests () =
  setup ();
  Masc_mcp.Shutdown.incr_requests ();
  check int "1 active request after incr" 1 (Masc_mcp.Shutdown.active_count ());

  Masc_mcp.Shutdown.incr_requests ();
  check int "2 active requests after second incr" 2 (Masc_mcp.Shutdown.active_count ());

  Masc_mcp.Shutdown.decr_requests ();
  check int "1 active request after decr" 1 (Masc_mcp.Shutdown.active_count ());

  Masc_mcp.Shutdown.decr_requests ();
  check int "0 active requests after second decr" 0 (Masc_mcp.Shutdown.active_count ())

let test_request_shutdown () =
  setup ();
  check bool "not shutting down before request" false (Masc_mcp.Shutdown.is_shutting_down ());

  Masc_mcp.Shutdown.request_shutdown ();
  check bool "shutting down after request" true (Masc_mcp.Shutdown.is_shutting_down ())

let test_request_shutdown_idempotent () =
  setup ();
  Masc_mcp.Shutdown.request_shutdown ();
  Masc_mcp.Shutdown.request_shutdown ();  (* calling twice should be safe *)
  check bool "still shutting down" true (Masc_mcp.Shutdown.is_shutting_down ())

let test_reset () =
  setup ();
  Masc_mcp.Shutdown.incr_requests ();
  Masc_mcp.Shutdown.incr_requests ();
  check int "2 active requests" 2 (Masc_mcp.Shutdown.active_count ());

  Masc_mcp.Shutdown.reset_for_testing ();
  check int "0 active requests after reset" 0 (Masc_mcp.Shutdown.active_count ())

let test_wait_for_pending_immediate () =
  Lwt_main.run (
    let open Lwt.Syntax in
    let () = setup () in
    (* No active requests - should return immediately *)
    let start = Unix.gettimeofday () in
    let* () = Masc_mcp.Shutdown.wait_for_pending ~timeout_sec:5.0 in
    let elapsed = Unix.gettimeofday () -. start in
    check bool "returns immediately with no requests" true (elapsed < 0.5);
    Lwt.return_unit
  )

let test_wait_for_pending_with_requests () =
  Lwt_main.run (
    let open Lwt.Syntax in
    let () = setup () in
    (* Add request and decrement after short delay *)
    Masc_mcp.Shutdown.incr_requests ();
    let _ = Lwt.async (fun () ->
      let* () = Lwt_unix.sleep 0.2 in
      Masc_mcp.Shutdown.decr_requests ();
      Lwt.return_unit
    ) in
    let start = Unix.gettimeofday () in
    let* () = Masc_mcp.Shutdown.wait_for_pending ~timeout_sec:5.0 in
    let elapsed = Unix.gettimeofday () -. start in
    check bool "waits for request to complete" true (elapsed >= 0.2 && elapsed < 1.0);
    Lwt.return_unit
  )

let test_wait_for_pending_timeout () =
  Lwt_main.run (
    let open Lwt.Syntax in
    let () = setup () in
    (* Add request but don't decrement - should timeout *)
    Masc_mcp.Shutdown.incr_requests ();
    let start = Unix.gettimeofday () in
    let* () = Masc_mcp.Shutdown.wait_for_pending ~timeout_sec:0.3 in
    let elapsed = Unix.gettimeofday () -. start in
    check bool "times out after specified duration" true (elapsed >= 0.3 && elapsed < 1.0);
    check int "request still active after timeout" 1 (Masc_mcp.Shutdown.active_count ());
    Lwt.return_unit
  )

let () =
  run "shutdown"
    [
      ("state", [
        test_case "initial state" `Quick test_initial_state;
        test_case "request shutdown" `Quick test_request_shutdown;
        test_case "shutdown idempotent" `Quick test_request_shutdown_idempotent;
        test_case "reset" `Quick test_reset;
      ]);
      ("requests", [
        test_case "incr/decr" `Quick test_incr_decr_requests;
      ]);
      ("wait", [
        test_case "immediate completion" `Quick test_wait_for_pending_immediate;
        test_case "wait for requests" `Slow test_wait_for_pending_with_requests;
        test_case "timeout" `Slow test_wait_for_pending_timeout;
      ]);
    ]
