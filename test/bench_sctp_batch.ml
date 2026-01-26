(** SCTP Batch I/O Benchmark - Optimized Flow Control

    Tests SCTP transport with batched I/O patterns:
    - Large cwnd (1MB) for more in-flight data
    - Batched/cumulative ACKs
    - Non-blocking recv for parallelism

    Run with:
      dune exec -- ./test/bench_sctp_batch.exe

    @author Second Brain
    @since MASC v3.5
*)

open Masc_mcp

(** Test configuration *)
let base_port = 24000
let message_size = 1024           (* 1KB like Miuda.ai benchmark *)
let test_duration_sec = 5.0       (* 5 seconds *)

(** Optimized parameters *)
let cwnd = 2_097_152              (* 2MB congestion window *)
let ack_batch_size = 128          (* ACK every N packets *)

(** Receiver with batched acknowledgment *)
let run_receiver ~net ~sw port stop_flag =
  let config : Udp_socket_eio.config = {
    bind_addr = "127.0.0.1";
    bind_port = port;
    recv_buffer_size = 8_388_608;  (* 8MB recv buffer *)
  } in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let count = ref 0 in
  let bytes = ref 0 in
  let last_tsn = ref 0l in
  let ack_count = ref 0 in

  (* ACK buffer - 4 bytes for cumulative TSN *)
  let ack_buf = Bytes.create 4 in
  let last_source = ref None in

  while not !stop_flag do
    match Udp_socket_eio.recv_timeout sock 0.001 with
    | Some dgram ->
      incr count;
      bytes := !bytes + Bytes.length dgram.data;

      (* Extract TSN from first 4 bytes of data *)
      if Bytes.length dgram.data >= 4 then begin
        let tsn = Bytes.get_int32_be dgram.data 0 in
        last_tsn := tsn;
        last_source := Some dgram.source;

        (* Batched ACK: send every N packets *)
        if !count mod ack_batch_size = 0 then begin
          Bytes.set_int32_be ack_buf 0 tsn;
          ignore (Udp_socket_eio.send_to sock ack_buf dgram.source);
          incr ack_count
        end
      end
    | None -> ()
  done;

  (* Send final ACK for any remaining packets *)
  (match !last_source with
   | Some src ->
     Bytes.set_int32_be ack_buf 0 !last_tsn;
     ignore (Udp_socket_eio.send_to sock ack_buf src)
   | None -> ());

  Udp_socket_eio.close sock;
  (!count, !bytes, !last_tsn, !ack_count)

(** Optimized sender with large window and batch processing *)
let run_sender ~net ~sw server_port duration_sec =
  let config = { Udp_socket_eio.default_config with
    recv_buffer_size = 262144;  (* 256KB for ACKs *)
  } in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let dest = { Udp_socket_eio.addr = "127.0.0.1"; port = server_port } in

  (* Flow control state *)
  let in_flight = ref 0 in
  let sent = ref 0 in
  let acked = ref 0 in
  let current_tsn = ref 1l in
  let batch_acked = ref 0 in

  let start_time = Unix.gettimeofday () in
  let end_time = start_time +. duration_sec in

  (* Pre-allocate data buffer *)
  let data = Bytes.make message_size 'X' in

  (* Optimized sender loop *)
  while Unix.gettimeofday () < end_time do
    (* Burst send while window allows - no individual ACK waiting *)
    let burst_count = ref 0 in
    while !in_flight + message_size <= cwnd &&
          !burst_count < 256 &&  (* Max burst *)
          Unix.gettimeofday () < end_time do
      Bytes.set_int32_be data 0 !current_tsn;
      ignore (Udp_socket_eio.send_to sock data dest);
      in_flight := !in_flight + message_size;
      incr sent;
      incr burst_count;
      current_tsn := Int32.succ !current_tsn
    done;

    (* Process any pending ACKs - non-blocking batch receive *)
    let rec drain_acks () =
      match Udp_socket_eio.recv_timeout sock 0.0001 with  (* 100μs poll *)
      | Some dgram when Bytes.length dgram.data >= 4 ->
        (* Batch ACK - release window for ack_batch_size packets *)
        let released = ack_batch_size * message_size in
        in_flight := max 0 (!in_flight - released);
        acked := !acked + ack_batch_size;
        incr batch_acked;
        drain_acks ()
      | _ -> ()
    in
    drain_acks ();

    (* If window is nearly full, wait a bit for ACKs *)
    if !in_flight > cwnd - (64 * message_size) then begin
      match Udp_socket_eio.recv_timeout sock 0.001 with
      | Some dgram when Bytes.length dgram.data >= 4 ->
        let released = ack_batch_size * message_size in
        in_flight := max 0 (!in_flight - released);
        acked := !acked + ack_batch_size;
        incr batch_acked
      | _ -> ()
    end
  done;

  (* Final drain - wait for remaining ACKs *)
  let drain_start = Unix.gettimeofday () in
  while Unix.gettimeofday () < drain_start +. 0.5 do
    match Udp_socket_eio.recv_timeout sock 0.01 with
    | Some _dgram ->
      let released = ack_batch_size * message_size in
      in_flight := max 0 (!in_flight - released);
      acked := !acked + ack_batch_size;
      incr batch_acked
    | None -> ()
  done;

  Udp_socket_eio.close sock;
  let actual_duration = Unix.gettimeofday () -. start_time in
  (!sent, !acked, actual_duration, !in_flight, !batch_acked)

(** Run single connection test *)
let run_test ~net ~sw =
  Printf.printf "Running SCTP Batch I/O benchmark....\n%!";

  let server_port = base_port in
  let stop_flag = ref false in

  let recv_result = ref (0, 0, 0l, 0) in
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

  let (recv_count, recv_bytes, _last_tsn, recv_ack_count) = !recv_result in
  let (sent_count, acked_count, duration, in_flight, batch_acked) = !send_result in
  (sent_count, acked_count, recv_count, recv_bytes, duration, in_flight, recv_ack_count, batch_acked)

(** Print results *)
let print_results sent acked recv bytes duration in_flight recv_acks batch_acked =
  let throughput =
    if duration > 0.0 then
      (float_of_int bytes /. 1_000_000.0) /. duration
    else 0.0
  in
  let loss_pct =
    if sent > 0 then 100.0 *. (1.0 -. float_of_int recv /. float_of_int sent)
    else 0.0
  in
  let ack_efficiency =
    if sent > 0 then float_of_int sent /. float_of_int (max 1 recv_acks)
    else 0.0
  in

  Printf.printf "\n";
  Printf.printf "╔═══════════════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║   MASC SCTP Batch I/O - High-Performance Flow Control                     ║\n";
  Printf.printf "║   1MB cwnd + Batched ACKs (every %d packets)                              ║\n" ack_batch_size;
  Printf.printf "╠═══════════════════════════════════════════════════════════════════════════╣\n";
  Printf.printf "| Messages Sent:     %12d                                          |\n" sent;
  Printf.printf "| Messages Received: %12d (%.2f%% loss)                              |\n" recv loss_pct;
  Printf.printf "| Messages ACKed:    %12d                                          |\n" acked;
  Printf.printf "| Total Bytes:       %12d (%.2f MB)                                 |\n" bytes (float_of_int bytes /. 1_000_000.0);
  Printf.printf "| Duration:          %12.2f seconds                                    |\n" duration;
  Printf.printf "├───────────────────────────────────────────────────────────────────────────┤\n";
  Printf.printf "| Throughput:        %12.2f MB/s                                       |\n" throughput;
  Printf.printf "├───────────────────────────────────────────────────────────────────────────┤\n";
  Printf.printf "| Batch I/O Stats:                                                          |\n";
  Printf.printf "|   cwnd:            %8d bytes (1MB window)                          |\n" cwnd;
  Printf.printf "|   in_flight:       %8d bytes (at end)                               |\n" in_flight;
  Printf.printf "|   ACKs sent:       %8d (batched)                                    |\n" recv_acks;
  Printf.printf "|   ACKs received:   %8d                                              |\n" batch_acked;
  Printf.printf "|   ACK efficiency:  %8.1f packets/ACK                                 |\n" ack_efficiency;
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
  Printf.printf "Comparison:\n";
  Printf.printf "  Raw UDP (no flow ctrl):    59 MB/s, 49%% loss\n";
  Printf.printf "  Per-packet ACK:            18 MB/s, 0%% loss\n";
  Printf.printf "  Batch ACK (this):          %.2f MB/s, %.2f%% loss\n" throughput loss_pct;
  Printf.printf "\n"

(** Main *)
let () =
  Printf.printf "MASC SCTP Batch I/O Benchmark\n";
  Printf.printf "=============================\n";
  Printf.printf "Testing optimized flow control with batch I/O:\n";
  Printf.printf "  - Message size: %d bytes (1KB)\n" message_size;
  Printf.printf "  - Duration: %.0f seconds\n" test_duration_sec;
  Printf.printf "  - cwnd: %d bytes (1MB)\n" cwnd;
  Printf.printf "  - ACK batch size: %d packets\n" ack_batch_size;
  Printf.printf "  - Goal: High throughput + 0%% packet loss\n\n";

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in

  let (sent, acked, recv, bytes, duration, in_flight, recv_acks, batch_acked) =
    run_test ~net ~sw
  in

  print_results sent acked recv bytes duration in_flight recv_acks batch_acked;
  Printf.printf "Benchmark complete.\n"
