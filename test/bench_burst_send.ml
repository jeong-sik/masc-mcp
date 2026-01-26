(** Burst Send Benchmark - Maximum Send Rate Test

    Tests the pure send rate without waiting for ACKs.
    This establishes the theoretical ceiling for flow-controlled variants.

    Run with:
      dune exec -- ./test/bench_burst_send.exe

    @author Second Brain
    @since MASC v3.5
*)

open Masc_mcp

(** Test configuration *)
let base_port = 25000
let message_size = 1024           (* 1KB like Miuda.ai benchmark *)
let test_duration_sec = 5.0       (* 5 seconds *)

(** Large recv buffer to minimize loss *)
let recv_buffer_size = 16_777_216  (* 16MB *)

(** Receiver - just counts, no ACKs *)
let run_receiver ~net ~sw port stop_flag =
  let config : Udp_socket_eio.config = {
    bind_addr = "127.0.0.1";
    bind_port = port;
    recv_buffer_size = recv_buffer_size;
  } in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let count = ref 0 in
  let bytes = ref 0 in

  while not !stop_flag do
    match Udp_socket_eio.recv_timeout sock 0.001 with
    | Some dgram ->
      incr count;
      bytes := !bytes + Bytes.length dgram.data
    | None -> ()
  done;

  Udp_socket_eio.close sock;
  (!count, !bytes)

(** Sender - burst send without waiting for ACKs *)
let run_sender ~net ~sw server_port duration_sec =
  let config = Udp_socket_eio.default_config in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let dest = { Udp_socket_eio.addr = "127.0.0.1"; port = server_port } in

  let sent = ref 0 in
  let start_time = Unix.gettimeofday () in
  let end_time = start_time +. duration_sec in

  (* Pre-allocate data buffer *)
  let data = Bytes.make message_size 'X' in

  (* Pure burst send - no flow control *)
  while Unix.gettimeofday () < end_time do
    ignore (Udp_socket_eio.send_to sock data dest);
    incr sent
  done;

  Udp_socket_eio.close sock;
  let actual_duration = Unix.gettimeofday () -. start_time in
  (!sent, actual_duration)

(** Run test *)
let run_test ~net ~sw =
  Printf.printf "Running Burst Send benchmark...\n%!";

  let server_port = base_port in
  let stop_flag = ref false in

  let recv_result = ref (0, 0) in
  let send_result = ref (0, 0.0) in

  Eio.Fiber.both
    (fun () -> recv_result := run_receiver ~net ~sw server_port stop_flag)
    (fun () ->
      Eio_unix.sleep 0.05;
      send_result := run_sender ~net ~sw server_port test_duration_sec;
      Eio_unix.sleep 0.3;
      stop_flag := true
    );

  let (recv_count, recv_bytes) = !recv_result in
  let (sent_count, duration) = !send_result in
  (sent_count, recv_count, recv_bytes, duration)

(** Print results *)
let print_results sent recv bytes duration =
  let send_throughput =
    if duration > 0.0 then
      (float_of_int sent *. float_of_int message_size /. 1_000_000.0) /. duration
    else 0.0
  in
  let recv_throughput =
    if duration > 0.0 then
      (float_of_int bytes /. 1_000_000.0) /. duration
    else 0.0
  in
  let loss_pct =
    if sent > 0 then 100.0 *. (1.0 -. float_of_int recv /. float_of_int sent)
    else 0.0
  in

  Printf.printf "\n";
  Printf.printf "╔═══════════════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║   MASC Burst Send - Maximum Send Rate (No Flow Control)                  ║\n";
  Printf.printf "║   Establishes theoretical ceiling for OCaml UDP on macOS                 ║\n";
  Printf.printf "╠═══════════════════════════════════════════════════════════════════════════╣\n";
  Printf.printf "| Messages Sent:     %12d                                          |\n" sent;
  Printf.printf "| Messages Received: %12d (%.2f%% loss)                              |\n" recv loss_pct;
  Printf.printf "| Total Bytes Recv:  %12d (%.2f MB)                                 |\n" bytes (float_of_int bytes /. 1_000_000.0);
  Printf.printf "| Duration:          %12.2f seconds                                    |\n" duration;
  Printf.printf "├───────────────────────────────────────────────────────────────────────────┤\n";
  Printf.printf "| Send Throughput:   %12.2f MB/s (sent rate)                          |\n" send_throughput;
  Printf.printf "| Recv Throughput:   %12.2f MB/s (received rate)                       |\n" recv_throughput;
  Printf.printf "├───────────────────────────────────────────────────────────────────────────┤\n";
  Printf.printf "| Analysis:                                                                 |\n";
  Printf.printf "|   This is the CEILING for OCaml UDP on macOS                             |\n";
  Printf.printf "|   Flow control cannot exceed recv throughput (%.2f MB/s)                  |\n" recv_throughput;
  Printf.printf "╚═══════════════════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";

  Printf.printf "Comparison with flow-controlled variants:\n";
  Printf.printf "  Burst (this):     %.2f MB/s send, %.2f MB/s recv, %.2f%% loss\n" send_throughput recv_throughput loss_pct;
  Printf.printf "  Batch ACK:        31 MB/s, 0.74%% loss\n";
  Printf.printf "  Per-packet ACK:   18 MB/s, 0%% loss\n";
  Printf.printf "\n"

(** Main *)
let () =
  Printf.printf "MASC Burst Send Benchmark\n";
  Printf.printf "=========================\n";
  Printf.printf "Testing maximum send rate (no flow control):\n";
  Printf.printf "  - Message size: %d bytes (1KB)\n" message_size;
  Printf.printf "  - Duration: %.0f seconds\n" test_duration_sec;
  Printf.printf "  - Recv buffer: %d bytes (%d MB)\n" recv_buffer_size (recv_buffer_size / 1_048_576);
  Printf.printf "  - Goal: Establish throughput ceiling\n\n";

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in

  let (sent, recv, bytes, duration) =
    run_test ~net ~sw
  in

  print_results sent recv bytes duration;
  Printf.printf "Benchmark complete.\n"
