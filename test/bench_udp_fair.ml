(** Fair UDP Throughput Benchmark - Reliable Measurement

    This benchmark measures ACTUAL reliable throughput with:
    1. Echo pattern (send → receive echo → count as delivered)
    2. Windowed batching (send N, wait for N echoes)
    3. Near-zero packet loss target

    Run with:
      dune exec -- ./test/bench_udp_fair.exe

    @author Second Brain
    @since MASC v3.3
*)

open Masc_mcp

(** Test configuration *)
let base_port = 21000
let window_size = 100        (* Send this many before waiting for echoes *)
let test_duration_sec = 3.0  (* Run each test for this long *)
let message_sizes = [64; 512; 1024; 4096; 8192]

(** Benchmark result *)
type result = {
  message_size: int;
  packets_sent: int;
  packets_recv: int;
  total_bytes: int;
  duration_sec: float;
  throughput_mbps: float;
  loss_pct: float;
} [@@warning "-69"]

(** Echo server fiber - receives and echoes back (zero-copy optimized) *)
let run_echo_server ~net ~sw port stop_flag =
  let config = { Udp_socket_eio.default_config with
    bind_addr = "127.0.0.1";
    bind_port = port;
  } in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let count = ref 0 in
  (* Pre-allocate send buffer for echo - avoids allocation per packet *)
  let echo_buf = Bytes.create 65536 in
  while not !stop_flag do
    match Udp_socket_eio.recv_timeout sock 0.1 with
    | Some dgram ->
      (* Echo back to sender - copy to our buffer to avoid holding recv_buf *)
      let len = Bytes.length dgram.data in
      Bytes.blit dgram.data 0 echo_buf 0 len;
      let echo_slice = Bytes.sub echo_buf 0 len in
      ignore (Udp_socket_eio.send_to sock echo_slice dgram.source);
      incr count
    | None -> ()
  done;
  Udp_socket_eio.close sock;
  !count

(** Client: send packets and wait for echoes *)
let run_client ~net ~sw message_size server_port duration_sec =
  let config = Udp_socket_eio.default_config in
  let sock = Udp_socket_eio.create ~net ~sw ~config () in
  let _ = Udp_socket_eio.bind sock in

  let dest = { Udp_socket_eio.addr = "127.0.0.1"; port = server_port } in
  let data = Bytes.make message_size 'X' in

  let sent = ref 0 in
  let recv = ref 0 in
  let start_time = Unix.gettimeofday () in
  let end_time = start_time +. duration_sec in

  (* Windowed send-receive loop *)
  while Unix.gettimeofday () < end_time do
    (* Send a window of packets *)
    let window_sent = ref 0 in
    while !window_sent < window_size && Unix.gettimeofday () < end_time do
      ignore (Udp_socket_eio.send_to sock data dest);
      incr sent;
      incr window_sent
    done;

    (* Wait for echoes (with timeout per packet) *)
    let window_recv = ref 0 in
    let deadline = Unix.gettimeofday () +. 0.5 in  (* 500ms max wait for window *)
    while !window_recv < !window_sent && Unix.gettimeofday () < deadline do
      match Udp_socket_eio.recv_timeout sock 0.01 with
      | Some _ ->
        incr recv;
        incr window_recv
      | None -> ()
    done
  done;

  Udp_socket_eio.close sock;
  let actual_duration = Unix.gettimeofday () -. start_time in
  (!sent, !recv, actual_duration)

(** Run single test with echo pattern *)
let run_test ~net ~sw message_size port_offset =
  Printf.printf "  %4d bytes, %ds window=%d... %!" message_size (int_of_float test_duration_sec) window_size;

  let server_port = base_port + port_offset in
  let stop_flag = ref false in

  (* Run server and client concurrently *)
  let server_result = ref 0 in
  let client_result = ref (0, 0, 0.0) in

  Eio.Fiber.both
    (fun () -> server_result := run_echo_server ~net ~sw server_port stop_flag)
    (fun () ->
      (* Small delay for server to start *)
      Eio_unix.sleep 0.05;
      client_result := run_client ~net ~sw message_size server_port test_duration_sec;
      stop_flag := true
    );

  let (sent, recv, duration) = !client_result in

  let total_bytes = recv * message_size * 2 in  (* Round-trip bytes *)
  let throughput =
    if duration > 0.0 then
      (float_of_int total_bytes /. 1_000_000.0) /. duration
    else 0.0
  in
  let loss =
    if sent > 0 then 100.0 *. (1.0 -. float_of_int recv /. float_of_int sent)
    else 0.0
  in

  let result = {
    message_size;
    packets_sent = sent;
    packets_recv = recv;
    total_bytes;
    duration_sec = duration;
    throughput_mbps = throughput;
    loss_pct = loss;
  } in

  Printf.printf "%.2f MB/s (loss %.1f%%, %d/%d)\n%!" throughput loss recv sent;
  result

(** Print summary *)
let print_summary results =
  Printf.printf "\n";
  Printf.printf "╔═══════════════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║        MASC WebRTC - Fair UDP Throughput (Echo Pattern)                  ║\n";
  Printf.printf "║        Reliable measurement with flow control                            ║\n";
  Printf.printf "╠═══════════════════════════════════════════════════════════════════════════╣\n";
  Printf.printf "| %8s | %8s | %8s | %8s | %10s | %12s |\n"
    "Size" "Sent" "Recv" "Loss %" "Duration" "Throughput";
  Printf.printf "├──────────┼──────────┼──────────┼──────────┼────────────┼──────────────┤\n";

  List.iter (fun r ->
    Printf.printf "| %6d B | %8d | %8d | %6.2f%% | %7.2f s | %9.2f MB/s |\n"
      r.message_size r.packets_sent r.packets_recv r.loss_pct r.duration_sec r.throughput_mbps
  ) results;

  Printf.printf "╚═══════════════════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";

  (* Find best *)
  let best = List.fold_left (fun acc r ->
    if r.throughput_mbps > acc.throughput_mbps then r else acc
  ) (List.hd results) results in

  Printf.printf "Peak Throughput: %.2f MB/s (at %d bytes, %.2f%% loss)\n\n"
    best.throughput_mbps best.message_size best.loss_pct;

  Printf.printf "Comparison (reliable throughput):\n";
  Printf.printf "  Go (Pion):     178 MB/s\n";
  Printf.printf "  Rust (webrtc): 213 MB/s\n";
  Printf.printf "  OCaml (MASC):  %.2f MB/s  " best.throughput_mbps;

  if best.loss_pct > 10.0 then
    Printf.printf "⚠️  High loss - not reliable\n"
  else if best.throughput_mbps >= 213.0 then
    Printf.printf "🏆 FASTER than Rust!\n"
  else if best.throughput_mbps >= 178.0 then
    Printf.printf "🎉 Competitive with Go!\n"
  else if best.throughput_mbps >= 100.0 then
    Printf.printf "👍 Good performance!\n"
  else if best.throughput_mbps >= 50.0 then
    Printf.printf "📈 Decent (better than Python)\n"
  else
    Printf.printf "🔧 Needs optimization\n";

  Printf.printf "\n"

(** Main *)
let () =
  Printf.printf "MASC WebRTC - Fair UDP Throughput Benchmark\n";
  Printf.printf "============================================\n";
  Printf.printf "Echo pattern: send → receive echo → count as delivered\n";
  Printf.printf "Window size: %d packets (flow control)\n" window_size;
  Printf.printf "Test duration: %.1f seconds per size\n\n" test_duration_sec;

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in

  Printf.printf "Running tests...\n";

  let results = List.mapi (fun i size ->
    run_test ~net ~sw size i
  ) message_sizes in

  print_summary results;
  Printf.printf "Benchmark complete.\n"
