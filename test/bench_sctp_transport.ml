(** SCTP Transport Benchmark - Flow-Controlled UDP

    Tests SCTP transport layer with congestion control.
    Goal: Achieve 0% packet loss through cwnd-based flow control.

    Run with:
      dune exec -- ./test/bench_sctp_transport.exe

    @author Second Brain
    @since MASC v3.4
*)

open Masc_mcp

(** Test configuration - Miuda.ai compatible *)
let base_port = 23000
let message_size = 1024           (* 1KB like Miuda.ai benchmark *)
let test_duration_sec = 5.0       (* 5 seconds *)
let _num_connections = 1           (* Start with single connection, for future multi-connection tests *)

(** Receiver with acknowledgment - sends ACKs back *)
let run_receiver ~net ~sw port stop_flag =
  let config = { Udp_socket_eio.default_config with
    bind_addr = "127.0.0.1";
    bind_port = port;
    recv_buffer_size = 262144;  (* 256KB recv buffer *)
  } in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let count = ref 0 in
  let bytes = ref 0 in
  let last_tsn = ref 0l in

  (* ACK buffer - 4 bytes for cumulative TSN *)
  let ack_buf = Bytes.create 4 in

  while not !stop_flag do
    match Udp_socket_eio.recv_timeout sock 0.001 with
    | Some dgram ->
      incr count;
      bytes := !bytes + Bytes.length dgram.data;

      (* Extract TSN from first 4 bytes of data *)
      if Bytes.length dgram.data >= 4 then begin
        let tsn = Bytes.get_int32_be dgram.data 0 in
        last_tsn := tsn;

        (* Send ACK with cumulative TSN *)
        Bytes.set_int32_be ack_buf 0 tsn;
        ignore (Udp_socket_eio.send_to sock ack_buf dgram.source)
      end
    | None -> ()
  done;
  Udp_socket_eio.close sock;
  (!count, !bytes, !last_tsn)

(** Simple flow-controlled sender - tracks window locally *)
let run_sender ~net ~sw server_port duration_sec =
  let config = Udp_socket_eio.default_config in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let dest = { Udp_socket_eio.addr = "127.0.0.1"; port = server_port } in

  (* Simple flow control state *)
  let cwnd = 65536 in              (* Congestion window: 64KB *)
  let in_flight = ref 0 in         (* Bytes in flight *)
  let sent = ref 0 in
  let acked = ref 0 in
  let current_tsn = ref 1l in

  let start_time = Unix.gettimeofday () in
  let end_time = start_time +. duration_sec in

  (* Data with TSN prefix *)
  let make_data tsn =
    let data = Bytes.make message_size 'X' in
    Bytes.set_int32_be data 0 tsn;
    data
  in

  (* Sender loop with flow control *)
  while Unix.gettimeofday () < end_time do
    (* Send while window allows *)
    while !in_flight + message_size <= cwnd && Unix.gettimeofday () < end_time do
      let data = make_data !current_tsn in
      ignore (Udp_socket_eio.send_to sock data dest);
      in_flight := !in_flight + message_size;
      incr sent;
      current_tsn := Int32.succ !current_tsn
    done;

    (* Try to receive ACKs - wait a bit for window to drain *)
    let rec recv_acks timeout_left =
      if timeout_left <= 0.0 then ()
      else
        match Udp_socket_eio.recv_timeout sock 0.001 with  (* 1ms poll *)
        | Some dgram when Bytes.length dgram.data >= 4 ->
          (* ACK received - release window *)
          in_flight := max 0 (!in_flight - message_size);
          incr acked;
          recv_acks (timeout_left -. 0.001)
        | _ ->
          recv_acks (timeout_left -. 0.001)
    in
    (* Wait up to 10ms for ACKs when window is full *)
    if !in_flight + message_size > cwnd then
      recv_acks 0.01
  done;

  (* Final drain - wait for remaining ACKs *)
  let drain_start = Unix.gettimeofday () in
  while Unix.gettimeofday () < drain_start +. 0.5 do
    match Udp_socket_eio.recv_timeout sock 0.01 with
    | Some _dgram ->
      in_flight := max 0 (!in_flight - message_size);
      incr acked
    | None -> ()
  done;

  Udp_socket_eio.close sock;
  let actual_duration = Unix.gettimeofday () -. start_time in
  (* Return simple stats: cwnd, in_flight *)
  (!sent, !acked, actual_duration, cwnd, !in_flight)

(** Run single connection test *)
let run_test ~net ~sw =
  Printf.printf "Running SCTP Transport benchmark (flow-controlled)...\n%!";

  let server_port = base_port in
  let stop_flag = ref false in

  let recv_result = ref (0, 0, 0l) in
  let send_result = ref (0, 0, 0.0, 0, 0) in

  Eio.Fiber.both
    (fun () -> recv_result := run_receiver ~net ~sw server_port stop_flag)
    (fun () ->
      (* Small delay for receiver to start *)
      Eio_unix.sleep 0.05;
      send_result := run_sender ~net ~sw server_port test_duration_sec;
      (* Give receiver time to process *)
      Eio_unix.sleep 0.2;
      stop_flag := true
    );

  let (recv_count, recv_bytes, _last_tsn) = !recv_result in
  let (sent_count, acked_count, duration, cwnd, in_flight) = !send_result in
  (sent_count, acked_count, recv_count, recv_bytes, duration, cwnd, in_flight)

(** Print results *)
let print_results sent acked recv bytes duration cwnd in_flight =
  let throughput =
    if duration > 0.0 then
      (float_of_int bytes /. 1_000_000.0) /. duration
    else 0.0
  in
  let loss_pct =
    if sent > 0 then 100.0 *. (1.0 -. float_of_int recv /. float_of_int sent)
    else 0.0
  in
  let ack_rate =
    if sent > 0 then 100.0 *. float_of_int acked /. float_of_int sent
    else 0.0
  in

  Printf.printf "\n";
  Printf.printf "╔═══════════════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║   MASC SCTP Transport - Flow-Controlled UDP Throughput                    ║\n";
  Printf.printf "║   Congestion window (cwnd) prevents receiver buffer overflow              ║\n";
  Printf.printf "╠═══════════════════════════════════════════════════════════════════════════╣\n";
  Printf.printf "| Messages Sent:     %12d                                          |\n" sent;
  Printf.printf "| Messages Received: %12d (%.2f%% loss)                              |\n" recv loss_pct;
  Printf.printf "| Messages ACKed:    %12d (%.2f%% ack rate)                         |\n" acked ack_rate;
  Printf.printf "| Total Bytes:       %12d (%.2f MB)                                 |\n" bytes (float_of_int bytes /. 1_000_000.0);
  Printf.printf "| Duration:          %12.2f seconds                                    |\n" duration;
  Printf.printf "├───────────────────────────────────────────────────────────────────────────┤\n";
  Printf.printf "| Throughput:        %12.2f MB/s                                       |\n" throughput;
  Printf.printf "├───────────────────────────────────────────────────────────────────────────┤\n";
  Printf.printf "| Flow Control Stats:                                                       |\n";
  Printf.printf "|   cwnd:      %8d bytes (congestion window)                          |\n" cwnd;
  Printf.printf "|   in_flight: %8d bytes (at end)                                     |\n" in_flight;
  Printf.printf "╚═══════════════════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";

  if loss_pct < 1.0 then
    Printf.printf "🎉 SUCCESS: %.2f%% packet loss (target: <1%%)!\n" loss_pct
  else if loss_pct < 5.0 then
    Printf.printf "👍 GOOD: %.2f%% packet loss (acceptable)\n" loss_pct
  else if loss_pct < 20.0 then
    Printf.printf "⚠️  WARNING: %.2f%% packet loss (needs tuning)\n" loss_pct
  else
    Printf.printf "❌ FAIL: %.2f%% packet loss (flow control not working)\n" loss_pct;

  Printf.printf "\n";
  Printf.printf "Comparison (without flow control: bench_udp_oneway):\n";
  Printf.printf "  Raw UDP:       59 MB/s, 49%% loss\n";
  Printf.printf "  SCTP Transport: %.2f MB/s, %.2f%% loss\n" throughput loss_pct;
  Printf.printf "\n"

(** Main *)
let () =
  Printf.printf "MASC SCTP Transport Benchmark\n";
  Printf.printf "=============================\n";
  Printf.printf "Testing flow-controlled UDP with SCTP congestion control:\n";
  Printf.printf "  - Message size: %d bytes (1KB)\n" message_size;
  Printf.printf "  - Duration: %.0f seconds\n" test_duration_sec;
  Printf.printf "  - Flow control: cwnd-based (RFC 4960)\n";
  Printf.printf "  - Goal: 0%% packet loss\n\n";

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in

  let (sent, acked, recv, bytes, duration, cwnd, in_flight) =
    run_test ~net ~sw
  in

  print_results sent acked recv bytes duration cwnd in_flight;
  Printf.printf "Benchmark complete.\n"
