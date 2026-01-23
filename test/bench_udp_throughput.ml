(** UDP Throughput Benchmark - Real Network I/O

    This benchmark measures ACTUAL UDP throughput on localhost.
    Simple approach: send N packets, count received, measure time.

    Run with:
      dune exec -- ./test/bench_udp_throughput.exe

    @author Second Brain
    @since MASC v3.3
*)

open Masc_mcp

(** Test configuration *)
let base_port = 20000
let packets_per_test = 10000
let message_sizes = [64; 512; 1024; 4096; 8192]  (* Max 8192 to avoid MTU issues *)

(** Benchmark result *)
type result = {
  message_size: int;
  packets_sent: int;
  packets_recv: int;
  total_bytes: int;
  duration_ms: float;
  throughput_mbps: float;
  packet_loss_pct: float;
} [@@warning "-69"]

(** Run single-direction throughput test *)
let run_test ~net ~sw message_size num_packets port_offset =
  Printf.printf "  %5d bytes x %d packets... %!" message_size num_packets;

  let recv_port = base_port + port_offset in

  (* Create receiver socket first *)
  let recv_config = { Udp_socket_eio.default_config with
    bind_addr = "127.0.0.1";
    bind_port = recv_port;
  } in
  let receiver = Udp_socket_eio.create ~net ~sw ~config:recv_config () in
  let _recv_ep = Udp_socket_eio.bind receiver in

  (* Create sender socket *)
  let sender_config = Udp_socket_eio.default_config in
  let sender = Udp_socket_eio.create ~net ~sw ~config:sender_config () in
  let _sender_ep = Udp_socket_eio.bind sender in

  let dest = { Udp_socket_eio.addr = "127.0.0.1"; port = recv_port } in
  let data = Bytes.make message_size 'X' in

  (* Send all packets first *)
  let send_start = Mtime_clock.elapsed () in
  for _ = 1 to num_packets do
    ignore (Udp_socket_eio.send_to sender data dest)
  done;
  let send_end = Mtime_clock.elapsed () in

  (* Longer delay for larger messages to let kernel buffers drain *)
  let delay = 0.01 +. (float_of_int message_size /. 100000.0) in
  Eio_unix.sleep delay;

  (* Receive with timeout - try harder for larger messages *)
  let recv_count = ref 0 in
  let recv_bytes = ref 0 in
  let no_recv_count = ref 0 in
  let max_no_recv = 5 in  (* Give up after 5 empty polls *)

  while !no_recv_count < max_no_recv do
    match Udp_socket_eio.recv_timeout receiver 0.02 with
    | Some dgram ->
      incr recv_count;
      recv_bytes := !recv_bytes + Bytes.length dgram.Udp_socket_eio.data;
      no_recv_count := 0  (* Reset counter on successful recv *)
    | None ->
      incr no_recv_count
  done;

  let recv_end = Mtime_clock.elapsed () in

  (* Calculate duration (send time only for throughput) *)
  let send_duration_ns = Mtime.Span.(to_uint64_ns (abs_diff send_end send_start)) in
  let send_duration_ms = Int64.to_float send_duration_ns /. 1_000_000.0 in

  let total_duration_ns = Mtime.Span.(to_uint64_ns (abs_diff recv_end send_start)) in
  let _total_duration_ms = Int64.to_float total_duration_ns /. 1_000_000.0 in

  (* Throughput = bytes sent / time *)
  let bytes_sent = num_packets * message_size in
  let throughput_mbps =
    if send_duration_ms > 0.0 then
      (float_of_int bytes_sent /. 1_000_000.0) /. (send_duration_ms /. 1000.0)
    else 0.0
  in

  let packet_loss =
    if num_packets > 0 then
      100.0 *. (1.0 -. (float_of_int !recv_count /. float_of_int num_packets))
    else 0.0
  in

  (* Cleanup *)
  Udp_socket_eio.close sender;
  Udp_socket_eio.close receiver;

  let result = {
    message_size;
    packets_sent = num_packets;
    packets_recv = !recv_count;
    total_bytes = bytes_sent;
    duration_ms = send_duration_ms;
    throughput_mbps;
    packet_loss_pct = packet_loss;
  } in

  Printf.printf "%.2f MB/s (recv %d/%d)\n%!" throughput_mbps !recv_count num_packets;
  result

(** Print summary *)
let print_summary results =
  Printf.printf "\n";
  Printf.printf "╔═══════════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║            MASC WebRTC - Real UDP Throughput Results                  ║\n";
  Printf.printf "╠═══════════════════════════════════════════════════════════════════════╣\n";
  Printf.printf "| %10s | %10s | %10s | %8s | %14s |\n"
    "Size" "Sent" "Recv" "Loss %" "Throughput";
  Printf.printf "├────────────┼────────────┼────────────┼──────────┼────────────────┤\n";

  List.iter (fun r ->
    Printf.printf "| %7d B  | %10d | %10d | %6.2f%% | %10.2f MB/s |\n"
      r.message_size r.packets_sent r.packets_recv r.packet_loss_pct r.throughput_mbps
  ) results;

  Printf.printf "╚═══════════════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";

  (* Find best *)
  let best = List.fold_left (fun acc r ->
    if r.throughput_mbps > acc.throughput_mbps then r else acc
  ) (List.hd results) results in

  Printf.printf "Peak Throughput: %.2f MB/s (at %d bytes)\n\n" best.throughput_mbps best.message_size;

  Printf.printf "Comparison with other implementations:\n";
  Printf.printf "  Go (Pion):     178 MB/s\n";
  Printf.printf "  Rust (webrtc): 213 MB/s\n";
  Printf.printf "  OCaml (MASC):  %.2f MB/s  " best.throughput_mbps;

  if best.throughput_mbps >= 213.0 then
    Printf.printf "🏆 FASTER than Rust!\n"
  else if best.throughput_mbps >= 178.0 then
    Printf.printf "🎉 Competitive with Go!\n"
  else if best.throughput_mbps >= 100.0 then
    Printf.printf "👍 Good progress!\n"
  else if best.throughput_mbps >= 50.0 then
    Printf.printf "📈 Better than Python\n"
  else
    Printf.printf "🔧 Needs optimization\n";

  Printf.printf "\n"

(** Main *)
let () =
  Printf.printf "MASC WebRTC - UDP Throughput Benchmark\n";
  Printf.printf "======================================\n";
  Printf.printf "Measures REAL network throughput on localhost.\n";
  Printf.printf "Packets per test: %d\n\n" packets_per_test;

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in

  Printf.printf "Running tests...\n";

  let results = List.mapi (fun i size ->
    run_test ~net ~sw size packets_per_test i
  ) message_sizes in

  print_summary results;
  Printf.printf "Benchmark complete.\n"
