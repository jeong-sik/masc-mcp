(** TURN TLS allocation example.

    Usage:
      TURN_URL=turns:localhost:5349?transport=udp \
      TURN_USERNAME=... \
      TURN_CREDENTIAL=... \
      TURN_TLS_CA=/path/to/ca.crt \
      ./turn_tls_example.exe

    Notes:
      - For self-signed certs, set TURN_TLS_CA to the cert used by coturn.
      - Use a DNS host like "localhost" for turns: URLs.
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

let () =
  Mirage_crypto_rng_unix.use_default ();
  let url = env_required "TURN_URL" in
  match parse_turn_url url with
  | None ->
      Printf.eprintf "Invalid TURN_URL. Example: TURN_URL=turns:localhost:5349?transport=udp\n";
      exit 1
  | Some (scheme, host, port, server_transport, relay_transport) ->
      let username = env_required "TURN_USERNAME" in
      let password = env_required_credential () in
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
      let client = Webrtc.Turn.create config in
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let net = Eio.Stdenv.net env in
      match Webrtc.Turn.run_with_eio ~net ~sw client (fun () ->
        match Webrtc.Turn.allocate client with
        | Ok relay ->
            Webrtc.Turn.close client;
            Ok relay
        | Error msg -> Error msg
      ) with
      | Ok (ip, relay_port) ->
        Printf.printf "Relay: %s:%d (%s)\n" ip relay_port
          (match relay_transport with Webrtc.Turn.UDP -> "UDP" | Webrtc.Turn.TCP -> "TCP")
      | Error msg ->
        Printf.eprintf "TURN allocate failed: %s\n" msg;
        exit 1
