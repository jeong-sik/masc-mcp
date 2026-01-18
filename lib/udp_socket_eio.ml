(** OCaml 5.x Eio-based UDP Socket for WebRTC

    Pure OCaml 5.x implementation showcasing:
    - Direct-style async (no Lwt monads!)
    - Eio.Net for real networking
    - Fibers for lightweight concurrency
    - Implicit effect handlers via Eio runtime
*)

(** {1 Types} *)

type config = {
  bind_addr: string;
  bind_port: int;
  recv_buffer_size: int;
  send_buffer_size: int;
}

type state =
  | Unbound
  | Bound
  | Connected
  | Closed

type endpoint = {
  addr: string;
  port: int;
}

type datagram = {
  data: bytes;
  source: endpoint;
  timestamp: float;
}

type stats = {
  packets_sent: int;
  packets_recv: int;
  bytes_sent: int;
  bytes_recv: int;
  errors: int;
}

(** Internal socket representation - using existential type for socket *)
type socket_wrapper = Socket : 'a Eio.Net.datagram_socket -> socket_wrapper

type t = {
  mutable state: state;
  mutable socket: socket_wrapper option;
  mutable local_ep: endpoint option;
  mutable remote_ep: endpoint option;
  mutable stats: stats;
  config: config;
  net: Eio_unix.Net.t;
  sw: Eio.Switch.t;
  recv_buf: Cstruct.t;
  send_buf: Cstruct.t;  (* Reusable send buffer - avoids allocation per send *)
}

(** {1 Configuration} *)

let default_config = {
  bind_addr = "0.0.0.0";
  bind_port = 0;
  recv_buffer_size = 65536;
  send_buffer_size = 65536;
}

(** {1 Helper Functions} *)

let endpoint_to_sockaddr ep =
  (* Parse IP address using ipaddr library *)
  let ip = match Ipaddr.of_string ep.addr with
    | Ok (Ipaddr.V4 v4) -> Eio.Net.Ipaddr.of_raw (Ipaddr.V4.to_octets v4)
    | Ok (Ipaddr.V6 v6) -> Eio.Net.Ipaddr.of_raw (Ipaddr.V6.to_octets v6)
    | Error _ -> Eio.Net.Ipaddr.V4.loopback
  in
  `Udp (ip, ep.port)

let sockaddr_to_endpoint sockaddr =
  match sockaddr with
  | `Udp (addr, port) ->
    let ip_str = Eio.Net.Ipaddr.pp Format.str_formatter addr;
      Format.flush_str_formatter ()
    in
    { addr = ip_str; port }
  | _ -> { addr = "0.0.0.0"; port = 0 }

(** {1 Socket Lifecycle} *)

let create ~net ~sw ?(config = default_config) () =
  let recv_buf = Cstruct.create config.recv_buffer_size in
  let send_buf = Cstruct.create config.send_buffer_size in  (* Reusable send buffer *)
  {
    state = Unbound;
    socket = None;
    local_ep = None;
    remote_ep = None;
    stats = {
      packets_sent = 0;
      packets_recv = 0;
      bytes_sent = 0;
      bytes_recv = 0;
      errors = 0;
    };
    config;
    net;
    sw;
    recv_buf;
    send_buf;
  }

let bind t =
  if t.state <> Unbound then
    failwith "Socket already bound or closed";

  (* Create UDP socket and bind - use config's bind_addr *)
  let bind_ip = match Ipaddr.of_string t.config.bind_addr with
    | Ok (Ipaddr.V4 v4) -> Eio.Net.Ipaddr.of_raw (Ipaddr.V4.to_octets v4)
    | Ok (Ipaddr.V6 v6) -> Eio.Net.Ipaddr.of_raw (Ipaddr.V6.to_octets v6)
    | Error _ -> Eio.Net.Ipaddr.V4.any  (* fallback to 0.0.0.0 *)
  in
  let listening_addr = `Udp (bind_ip, t.config.bind_port) in
  let socket = Eio.Net.datagram_socket ~sw:t.sw t.net listening_addr in

  t.socket <- Some (Socket socket);
  t.state <- Bound;

  (* Return config's address/port
     Note: For port=0 (auto-assign), caller should use a fixed port in tests
     as Eio doesn't easily expose the auto-assigned port *)
  let local_ep = { addr = t.config.bind_addr; port = t.config.bind_port } in
  t.local_ep <- Some local_ep;
  local_ep

let connect t remote =
  if t.state = Closed then
    failwith "Socket closed";
  if t.state = Unbound then
    ignore (bind t);

  t.remote_ep <- Some remote;
  t.state <- Connected

let close t =
  (* In Eio, sockets are managed by the switch - just mark as closed *)
  t.socket <- None;
  t.state <- Closed

(** {1 Data Transfer} *)

let send_to t data dest =
  match t.socket with
  | None -> failwith "Socket not bound"
  | Some (Socket sock) ->
    let dest_addr = endpoint_to_sockaddr dest in
    let len = Bytes.length data in
    (* Zero-copy optimization: reuse pre-allocated send buffer *)
    Cstruct.blit_from_bytes data 0 t.send_buf 0 len;
    let slice = Cstruct.sub t.send_buf 0 len in
    Eio.Net.send sock ~dst:dest_addr [slice];
    t.stats <- { t.stats with
      packets_sent = t.stats.packets_sent + 1;
      bytes_sent = t.stats.bytes_sent + len;
    };
    len

let send t data =
  match t.remote_ep with
  | None -> invalid_arg "Socket not connected - use send_to instead"
  | Some dest -> send_to t data dest

let recv t =
  match t.socket with
  | None -> failwith "Socket not bound"
  | Some (Socket sock) ->
    let src, len = Eio.Net.recv sock t.recv_buf in
    let data = Bytes.create len in
    Cstruct.blit_to_bytes t.recv_buf 0 data 0 len;
    let source = sockaddr_to_endpoint src in
    t.stats <- { t.stats with
      packets_recv = t.stats.packets_recv + 1;
      bytes_recv = t.stats.bytes_recv + len;
    };
    { data; source; timestamp = Unix.gettimeofday () }

(** Zero-copy receive - returns Cstruct slice directly from recv_buf.
    WARNING: Data is only valid until next recv call!
    Use for high-performance scenarios where you process data immediately. *)
let recv_zerocopy t =
  match t.socket with
  | None -> failwith "Socket not bound"
  | Some (Socket sock) ->
    let src, len = Eio.Net.recv sock t.recv_buf in
    let source = sockaddr_to_endpoint src in
    t.stats <- { t.stats with
      packets_recv = t.stats.packets_recv + 1;
      bytes_recv = t.stats.bytes_recv + len;
    };
    (Cstruct.sub t.recv_buf 0 len, source)

let recv_timeout t timeout_sec =
  match t.socket with
  | None -> failwith "Socket not bound"
  | Some (Socket sock) ->
    (* Use Fiber.first for timeout - one fiber receives, one sleeps *)
    let result = ref None in
    begin
      try
        Eio.Fiber.first
          (fun () ->
            let src, len = Eio.Net.recv sock t.recv_buf in
            let data = Bytes.create len in
            Cstruct.blit_to_bytes t.recv_buf 0 data 0 len;
            let source = sockaddr_to_endpoint src in
            t.stats <- { t.stats with
              packets_recv = t.stats.packets_recv + 1;
              bytes_recv = t.stats.bytes_recv + len;
            };
            result := Some { data; source; timestamp = Unix.gettimeofday () })
          (fun () ->
            Eio_unix.sleep timeout_sec)
      with _ -> ()
    end;
    !result

(** {1 Concurrent Operations} *)

let recv_loop t handler =
  (* Direct-style loop - Eio handles the scheduling *)
  while t.state <> Closed do
    try
      let dgram = recv t in
      handler dgram
    with
    | exn ->
      t.stats <- { t.stats with errors = t.stats.errors + 1 };
      if t.state <> Closed then
        Eio.traceln "UDP recv error: %s" (Printexc.to_string exn)
  done

(** {1 State and Statistics} *)

let get_state t = t.state

let local_endpoint t = t.local_ep

let remote_endpoint t = t.remote_ep

let get_stats t = t.stats

let stats_to_json stats =
  `Assoc [
    ("packetsSent", `Int stats.packets_sent);
    ("packetsRecv", `Int stats.packets_recv);
    ("bytesSent", `Int stats.bytes_sent);
    ("bytesRecv", `Int stats.bytes_recv);
    ("errors", `Int stats.errors);
  ]

(** {1 STUN/ICE Integration} *)

let stun_request t server timeout_sec =
  (* Generate STUN binding request *)
  let request = Stun.create_binding_request () in
  match Stun.encode request with
  | Error e -> Error e
  | Ok request_bytes ->
      (* Send request *)
      let _ = send_to t request_bytes server in

      (* Wait for response with timeout using Fiber.first *)
      let result = ref (Error "Timeout") in
      begin
        try
          Eio.Fiber.first
            (fun () ->
              let dgram = recv t in
              if dgram.source.addr = server.addr && dgram.source.port = server.port then
                result := Ok dgram.data
              else
                result := Error "Response from unexpected source")
            (fun () ->
              Eio_unix.sleep timeout_sec;
              result := Error "STUN request timeout")
        with exn ->
          result := Error (Printexc.to_string exn)
      end;
      !result

(** {1 Pretty Printing} *)

let string_of_state = function
  | Unbound -> "Unbound"
  | Bound -> "Bound"
  | Connected -> "Connected"
  | Closed -> "Closed"

let string_of_endpoint ep =
  Printf.sprintf "%s:%d" ep.addr ep.port

let pp_datagram fmt dgram =
  Format.fprintf fmt "Datagram { from=%s, size=%d, time=%.3f }"
    (string_of_endpoint dgram.source)
    (Bytes.length dgram.data)
    dgram.timestamp

let pp_stats fmt stats =
  Format.fprintf fmt "Stats { sent=%d/%dB, recv=%d/%dB, errors=%d }"
    stats.packets_sent stats.bytes_sent
    stats.packets_recv stats.bytes_recv
    stats.errors
