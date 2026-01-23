(** TURN relay data path example (ICE-style).

    Usage:
      TURN_URL=turns:turn.localhost:5349?transport=udp \
      TURN_USERNAME=... \
      TURN_CREDENTIAL=... \
      TURN_TLS_CA=/path/to/rootCA.pem \
      TURN_PEER_HOST=host.docker.internal \
      TURN_PEER_PORT=7001 \
      TURN_MESSAGES=5 \
      dune exec examples/turn_relay_data_example.exe
*)

let env_opt name =
  match Sys.getenv_opt name with
  | Some value ->
      let trimmed = String.trim value in
      if trimmed = "" then None else Some trimmed
  | None -> None

let env_required name =
  match env_opt name with
  | Some value -> value
  | None ->
      Printf.eprintf "Missing %s\n" name;
      exit 1

let env_required_credential () =
  match env_opt "TURN_CREDENTIAL" with
  | Some value -> value
  | None ->
      (match env_opt "TURN_PASSWORD" with
       | Some value -> value
       | None ->
           Printf.eprintf "Missing TURN_CREDENTIAL (or TURN_PASSWORD)\n";
           exit 1)

let env_int ?(default=0) name =
  match env_opt name with
  | Some value -> (match int_of_string_opt value with Some v -> v | None -> default)
  | None -> default

let string_starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let parse_query_param query key =
  let parts = String.split_on_char '&' query in
  let prefix = key ^ "=" in
  List.find_map (fun part ->
    if string_starts_with ~prefix part then
      Some (String.sub part (String.length prefix) (String.length part - String.length prefix))
    else
      None
  ) parts

let parse_host_port ~default_port host_port =
  match String.rindex_opt host_port ':' with
  | Some idx ->
      let host = String.sub host_port 0 idx in
      let port_str = String.sub host_port (idx + 1) (String.length host_port - idx - 1) in
      (match int_of_string_opt port_str with
       | Some port when host <> "" -> Some (host, port)
       | _ -> None)
  | None ->
      if host_port = "" then None else Some (host_port, default_port)

let parse_turn_url url =
  let url = String.trim url in
  let scheme, rest =
    if string_starts_with ~prefix:"turns:" url then
      ("turns", String.sub url 6 (String.length url - 6))
    else if string_starts_with ~prefix:"turn:" url then
      ("turn", String.sub url 5 (String.length url - 5))
    else
      ("", "")
  in
  if scheme = "" then
    None
  else
    let host_port, query =
      match String.split_on_char '?' rest with
      | [hp; q] -> (hp, Some q)
      | [hp] -> (hp, None)
      | _ -> ("", None)
    in
    let default_port = if scheme = "turns" then 5349 else 3478 in
    match parse_host_port ~default_port host_port with
    | None -> None
    | Some (host, port) ->
        let transport = Option.bind query (fun q -> parse_query_param q "transport") in
        let server_transport =
          match scheme, transport with
          | "turns", _ -> Webrtc.Turn.Tls
          | "turn", Some "tcp" -> Webrtc.Turn.Tcp
          | "turn", _ -> Webrtc.Turn.Udp
          | _ -> Webrtc.Turn.Udp
        in
        let relay_transport =
          match transport with
          | Some "tcp" -> Webrtc.Turn.TCP
          | _ -> Webrtc.Turn.UDP
        in
        Some (scheme, host, port, server_transport, relay_transport)

let resolve_ipv4 host =
  try
    let he = Unix.gethostbyname host in
    if Array.length he.Unix.h_addr_list = 0 then None
    else Some (Unix.string_of_inet_addr he.Unix.h_addr_list.(0))
  with _ ->
    None


let start_udp_echo ~net ~sw port =
  let bind_addr = `Udp (Eio.Net.Ipaddr.V4.any, port) in
  let sock = Eio.Net.datagram_socket ~sw net bind_addr in
  Eio.Fiber.fork ~sw (fun () ->
    let buf = Cstruct.create 2048 in
    while true do
      let (addr, len) = Eio.Net.recv sock buf in
      let payload = Cstruct.sub buf 0 len in
      Eio.Net.send sock ~dst:addr [payload]
    done
  );
  ()

let () =
  Mirage_crypto_rng_unix.use_default ();
  let debug = env_opt "TURN_DEBUG" <> None in
  let url = env_required "TURN_URL" in
  let username = env_required "TURN_USERNAME" in
  let password = env_required_credential () in
  let peer_host = Option.value ~default:"host.docker.internal" (env_opt "TURN_PEER_HOST") in
  let peer_port = env_int ~default:7001 "TURN_PEER_PORT" in
  let messages = env_int ~default:1 "TURN_MESSAGES" in
  let payload_base = Option.value ~default:"turn-echo" (env_opt "TURN_PAYLOAD") in
  let local_echo = env_int ~default:1 "TURN_LOCAL_ECHO" <> 0 in
  let recv_timeout_s = float_of_int (env_int ~default:5 "TURN_RECV_TIMEOUT") in
  let use_channel = env_int ~default:1 "TURN_USE_CHANNEL" <> 0 in
  match parse_turn_url url with
  | None ->
      Printf.eprintf "Invalid TURN_URL. Example: TURN_URL=turns:turn.localhost:5349?transport=udp\n";
      exit 1
  | Some (scheme, host, port, server_transport, relay_transport) ->
      let tls =
        match scheme with
        | "turns" ->
            let ca_file = env_opt "TURN_TLS_CA" in
            if ca_file = None then begin
              Printf.eprintf "Missing TURN_TLS_CA for turns: URL\n";
              exit 1
            end;
            Some Webrtc.Turn.{
              ca_file;
              cert_file = env_opt "TURN_TLS_CERT";
              key_file = env_opt "TURN_TLS_KEY";
            }
        | _ -> None
      in
      let config = Webrtc.Turn.{ default_config with
        server_host = host;
        server_port = port;
        username;
        password;
        realm = "";
        transport = relay_transport;
        server_transport;
        tls;
      } in
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let net = Eio.Stdenv.net env in
      if local_echo then
        start_udp_echo ~net ~sw peer_port;
      let peer_ip =
        match resolve_ipv4 peer_host with
        | Some ip -> ip
        | None ->
            Printf.eprintf "Failed to resolve TURN_PEER_HOST=%s\n" peer_host;
            exit 1
      in
      let client = Webrtc.Turn.create config in
      let rtts = ref [] in
      let result =
        try
          Webrtc.Turn.run_with_eio ~net ~sw client (fun () ->
          let recv_with_timeout () =
            match Eio.Time.with_timeout (Eio.Stdenv.clock env) recv_timeout_s (fun () ->
              Ok (Webrtc.Turn.recv_data client)
            ) with
            | Ok data -> `Data data
            | Error `Timeout -> `Timeout
          in
          match Webrtc.Turn.allocate client with
          | Error msg -> Error msg
          | Ok _ ->
              if debug then
                Printf.printf "CreatePermission %s:%d\n" peer_ip peer_port;
              (match Webrtc.Turn.create_permission client (peer_ip, peer_port) with
               | Ok () -> ()
               | Error msg -> raise (Failure ("CreatePermission failed: " ^ msg)));
              if use_channel then
                (if debug then
                   Printf.printf "ChannelBind 0x4000 %s:%d\n" peer_ip peer_port;
                 match Webrtc.Turn.channel_bind client 0x4000 (peer_ip, peer_port) with
                 | Ok () -> ()
                 | Error msg -> raise (Failure ("ChannelBind failed: " ^ msg)));
              let rec loop idx =
                if idx > messages then Ok ()
                else
                  let payload = Bytes.of_string (Printf.sprintf "%s-%d" payload_base idx) in
                  let start = Unix.gettimeofday () in
                  if debug then
                    Printf.printf "SendData #%d (%d bytes)\n" idx (Bytes.length payload);
                  (match Webrtc.Turn.send_data client (peer_ip, peer_port) payload with
                   | Ok () -> ()
                   | Error msg -> raise (Failure ("SendData failed: " ^ msg)));
                  match recv_with_timeout () with
                  | `Data (Some ((_from_ip, _from_port), data)) ->
                      let rtt = Unix.gettimeofday () -. start in
                      rtts := rtt :: !rtts;
                      if data = payload then
                        loop (idx + 1)
                      else
                        Error "Echo payload mismatch"
                  | `Data None -> Error "No data received"
                  | `Timeout -> Error "Timeout waiting for relay data"
              in
              let res = loop 1 in
              Webrtc.Turn.close client;
              res
        )
        with
        | Failure msg -> Error msg
        | Eio.Cancel.Cancelled _ -> Error "Timeout waiting for relay data"
      in
      match result with
      | Ok () ->
          let count = List.length !rtts in
          let total = List.fold_left ( +. ) 0.0 !rtts in
          let avg = if count = 0 then 0.0 else total /. float_of_int count in
          let max_rtt = List.fold_left max 0.0 !rtts in
          Printf.printf "Relay data OK: %d messages, avg=%.3fms max=%.3fms\n"
            count (avg *. 1000.) (max_rtt *. 1000.)
      | Error msg ->
          Printf.eprintf "Relay data failed: %s\n" msg;
          exit 1
