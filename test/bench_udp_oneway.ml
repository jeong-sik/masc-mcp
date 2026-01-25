(** One-Way UDP Throughput Benchmark - Miuda.ai Compatible

    Matches the methodology from https://miuda.ai/blog/webrtc-datachannel-benchmark/
    - 1KB message size
    - One-way transfer (send only, no echo)
    - 10 second duration
    - Multiple connections (configurable)

    Run with:
      dune exec -- ./test/bench_udp_oneway.exe

    @author Second Brain
    @since MASC v3.3
*)

open Masc_mcp

(** Test configuration - matching Miuda.ai *)
let base_port = 22000
let message_size = 1024           (* 1KB like Miuda.ai benchmark *)
let test_duration_sec = 10.0      (* 10 seconds like Miuda.ai *)
let num_connections = 10          (* 10 connections like Miuda.ai *)

(** Receiver fiber - counts received packets (optimized: no timeout polling) *)
let run_receiver ~net ~sw port stop_flag =
  let config : Udp_socket_eio.config = {
    bind_addr = "127.0.0.1";
    bind_port = port;
    recv_buffer_size = 262144;  (* 256KB recv buffer *)
  } in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let count = ref 0 in
  let bytes = ref 0 in
  (* Use short timeout for tight polling instead of 0.1s *)
  while not !stop_flag do
    match Udp_socket_eio.recv_timeout sock 0.001 with  (* 1ms timeout *)
    | Some dgram ->
      incr count;
      bytes := !bytes + Bytes.length dgram.data
    | None -> ()
  done;
  Udp_socket_eio.close sock;
  (!count, !bytes)

(** Sender fiber - sends as fast as possible *)
let run_sender ~net ~sw server_port duration_sec =
  let config = Udp_socket_eio.default_config in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let dest = { Udp_socket_eio.addr = "127.0.0.1"; port = server_port } in
  let data = Bytes.make message_size 'X' in

  let sent = ref 0 in
  let start_time = Unix.gettimeofday () in
  let end_time = start_time +. duration_sec in

  (* Send as fast as possible - no waiting for response *)
  while Unix.gettimeofday () < end_time do
    ignore (Udp_socket_eio.send_to sock data dest);
    incr sent
  done;

  Udp_socket_eio.close sock;
  let actual_duration = Unix.gettimeofday () -. start_time in
  (!sent, actual_duration)

(** Run single connection pair *)
let run_connection_pair ~net ~sw port_offset =
  let server_port = base_port + port_offset in
  let stop_flag = ref false in

  let recv_result = ref (0, 0) in
  let send_result = ref (0, 0.0) in

  Eio.Fiber.both
    (fun () -> recv_result := run_receiver ~net ~sw server_port stop_flag)
    (fun () ->
      (* Small delay for receiver to start *)
      Eio_unix.sleep 0.05;
      send_result := run_sender ~net ~sw server_port test_duration_sec;
      (* Give receiver time to process remaining packets *)
      Eio_unix.sleep 0.1;
      stop_flag := true
    );

  let (recv_count, recv_bytes) = !recv_result in
  let (sent_count, duration) = !send_result in
  (sent_count, recv_count, recv_bytes, duration)

(** Run all connections in parallel *)
let run_parallel_connections ~net ~sw =
  Printf.printf "Running %d parallel connections for %.0f seconds each...\n%!" num_connections test_duration_sec;

  let results = Array.make num_connections (0, 0, 0, 0.0) in

  (* Run all connections in parallel using Fiber.all *)
  let fibers = List.init num_connections (fun i () ->
    results.(i) <- run_connection_pair ~net ~sw i
  ) in

  Eio.Fiber.all fibers;

  (* Aggregate results *)
  let total_sent = Array.fold_left (fun acc (s, _, _, _) -> acc + s) 0 results in
  let total_recv = Array.fold_left (fun acc (_, r, _, _) -> acc + r) 0 results in
  let total_bytes = Array.fold_left (fun acc (_, _, b, _) -> acc + b) 0 results in
  let max_duration = Array.fold_left (fun acc (_, _, _, d) -> max acc d) 0.0 results in

  (total_sent, total_recv, total_bytes, max_duration, results)

(** Print results *)
let print_results total_sent total_recv total_bytes duration =
  let throughput =
    if duration > 0.0 then
      (float_of_int total_bytes /. 1_000_000.0) /. duration
    else 0.0
  in
  let loss_pct =
    if total_sent > 0 then 100.0 *. (1.0 -. float_of_int total_recv /. float_of_int total_sent)
    else 0.0
  in
  let msgs_per_sec = float_of_int total_recv /. duration in

  Printf.printf "\n";
  Printf.printf "╔═══════════════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║   MASC WebRTC - One-Way UDP Throughput (Miuda.ai Compatible)             ║\n";
  Printf.printf "║   Methodology: %d connections, %d bytes/msg, %.0fs duration              ║\n" num_connections message_size test_duration_sec;
  Printf.printf "╠═══════════════════════════════════════════════════════════════════════════╣\n";
  Printf.printf "| Messages Sent:     %12d                                          |\n" total_sent;
  Printf.printf "| Messages Received: %12d (%.2f%% loss)                              |\n" total_recv loss_pct;
  Printf.printf "| Total Bytes:       %12d (%.2f MB)                                 |\n" total_bytes (float_of_int total_bytes /. 1_000_000.0);
  Printf.printf "| Duration:          %12.2f seconds                                    |\n" duration;
  Printf.printf "├───────────────────────────────────────────────────────────────────────────┤\n";
  Printf.printf "| Throughput:        %12.2f MB/s                                       |\n" throughput;
  Printf.printf "| Messages/sec:      %12.0f                                            |\n" msgs_per_sec;
  Printf.printf "╚═══════════════════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";

  Printf.printf "Comparison (Miuda.ai benchmark, 1KB, 10 connections):\n";
  Printf.printf "  Rust (webrtc-rs): 213 MB/s, ~218k msg/s\n";
  Printf.printf "  Go (Pion):        178 MB/s, ~182k msg/s\n";
  Printf.printf "  OCaml (MASC):     %.2f MB/s, ~%.0fk msg/s  " throughput (msgs_per_sec /. 1000.0);

  if throughput >= 213.0 then
    Printf.printf "🏆 FASTER than Rust!\n"
  else if throughput >= 178.0 then
    Printf.printf "🎉 Competitive with Go!\n"
  else if throughput >= 100.0 then
    Printf.printf "👍 Good performance!\n"
  else if throughput >= 50.0 then
    Printf.printf "📈 Decent (better than Python)\n"
  else
    Printf.printf "🔧 Needs optimization\n";

  Printf.printf "\n"

(** Main *)
let () =
  Printf.printf "MASC WebRTC - One-Way UDP Throughput Benchmark\n";
  Printf.printf "==============================================\n";
  Printf.printf "Matching Miuda.ai methodology:\n";
  Printf.printf "  - Message size: %d bytes (1KB)\n" message_size;
  Printf.printf "  - Connections: %d (parallel)\n" num_connections;
  Printf.printf "  - Duration: %.0f seconds\n" test_duration_sec;
  Printf.printf "  - Direction: One-way (send only, no echo)\n\n";

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in

  let (total_sent, total_recv, total_bytes, duration, _) =
    run_parallel_connections ~net ~sw
  in

  print_results total_sent total_recv total_bytes duration;
  Printf.printf "Benchmark complete.\n"
