(** TURN relay candidate example.

    Usage:
      TURN_URL=turn:host:3478?transport=udp \
      TURN_USERNAME=... \
      TURN_CREDENTIAL=... \
      TURN_TLS_CA=/path/to/turn.crt \
      ./turn_relay_example.exe
*)

open Masc_mcp

let env_opt name =
  match Sys.getenv_opt name with
  | Some value ->
      let trimmed = String.trim value in
      if trimmed = "" then None else Some trimmed
  | None -> None

let () =
  Mirage_crypto_rng_unix.use_default ();
  match env_opt "TURN_URL" with
  | None ->
      Printf.eprintf "Missing TURN_URL. Example: TURN_URL=turn:host:3478?transport=udp\n";
      exit 1
  | Some url ->
      let is_turns = String.length url >= 6 && String.sub url 0 6 = "turns:" in
      let credential =
        match env_opt "TURN_CREDENTIAL" with
        | Some _ as value -> value
        | None -> env_opt "TURN_PASSWORD"
      in
      let turn_tls =
        if is_turns then
          match env_opt "TURN_TLS_CA" with
          | None ->
              Printf.eprintf "Missing TURN_TLS_CA for turns: URL\n";
              exit 1
          | Some ca_file ->
              Some Ice.{
                ca_file = Some ca_file;
                cert_file = env_opt "TURN_TLS_CERT";
                key_file = env_opt "TURN_TLS_KEY";
              }
        else
          None
      in
      let ice_server = Ice.{
        urls = [url];
        username = env_opt "TURN_USERNAME";
        credential;
        turn_tls;
      } in
      let config = Ice.{ default_config with ice_servers = [ice_server] } in
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let net = Eio.Stdenv.net env in
      let agent = Ice_eio.create ~config () in
      let candidates = Ice_eio.gather_candidates ~net ~sw agent in
      let relay_candidates = List.filter (fun c -> c.Ice.cand_type = Ice.Relay) candidates in
      Printf.printf "Total candidates: %d\n" (List.length candidates);
      if relay_candidates = [] then
        Printf.printf "No relay candidates found. Check TURN credentials and network.\n"
      else
        List.iter (fun c ->
          Printf.printf "Relay: %s:%d (%s)\n"
            c.Ice.address c.Ice.port (Ice.string_of_transport c.Ice.transport)
        ) relay_candidates;
      Ice_eio.close agent
