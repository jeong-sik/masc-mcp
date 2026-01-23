(** MASC gRPC-Eio Server - gzip compression enabled

    This is a standalone gRPC server using grpc-eio-next (OCaml 5.x Effects).
    Runs on port 9936 alongside the main MASC server (8935/9935).

    {2 Usage}

    {v
    # Start gRPC-Eio server
    dune exec bin/masc_grpc_eio.exe -- --path /path/to/.masc

    # With custom port
    dune exec bin/masc_grpc_eio.exe -- --path /path/to/.masc --port 9936

    # Test with grpcurl (gzip enabled)
    grpcurl -plaintext -H "grpc-accept-encoding: gzip" \
      localhost:9936 masc.v1.MASCService/GetStatus
    v}

    {2 Features}

    - Native gzip compression (fixes 406 error!)
    - Eio-based direct-style concurrency
    - Codec negotiation via grpc-accept-encoding header
    - Logging interceptor for debugging

    @see <bin/main.ml> Main MASC server (HTTP/SSE + gRPC-Lwt)
*)

let () =
  (* Parse command line arguments *)
  let masc_path = ref "" in
  let port = ref 9936 in
  let host = ref "127.0.0.1" in

  let spec = [
    ("--path", Arg.Set_string masc_path, " Path to .masc directory (required)");
    ("--port", Arg.Set_int port, " gRPC port (default: 9936)");
    ("--host", Arg.Set_string host, " Bind address (default: 127.0.0.1)");
  ] in

  let usage = "MASC gRPC-Eio Server (gzip enabled)\n\nUsage: masc_grpc_eio --path /path/to/.masc [options]" in
  Arg.parse spec (fun _ -> ()) usage;

  if !masc_path = "" then begin
    Printf.eprintf "Error: --path is required\n";
    Printf.eprintf "Example: masc_grpc_eio --path ~/me/.masc\n";
    exit 1
  end;

  (* Expand ~ to home directory *)
  let masc_path =
    if String.length !masc_path > 0 && !masc_path.[0] = '~' then
      Filename.concat (Sys.getenv "HOME") (String.sub !masc_path 1 (String.length !masc_path - 1))
    else !masc_path
  in

  (* Verify .masc directory exists *)
  if not (Sys.file_exists masc_path) then begin
    Printf.eprintf "Error: MASC directory not found: %s\n" masc_path;
    Printf.eprintf "Initialize with: mkdir -p %s && touch %s/config.json\n" masc_path masc_path;
    exit 1
  end;

  (* Create Room config using default_config helper *)
  let room_config = Masc_mcp.Room.default_config masc_path in

  (* Create gRPC-Eio config *)
  let grpc_config : Masc_mcp.Transport_grpc_next.config = {
    host = !host;
    port = !port;
    codecs = [Grpc_core.Codec.gzip (); Grpc_core.Codec.identity];
    max_message_size = 4 * 1024 * 1024;
  } in

  Printf.printf "╔════════════════════════════════════════════════════════╗\n";
  Printf.printf "║           MASC gRPC-Eio Server (gzip enabled)          ║\n";
  Printf.printf "╠════════════════════════════════════════════════════════╣\n";
  Printf.printf "║  Path: %-48s ║\n" masc_path;
  Printf.printf "║  Port: %-48d ║\n" !port;
  Printf.printf "║  Host: %-48s ║\n" !host;
  Printf.printf "║  Codecs: gzip, identity                                ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";

  (* Run server *)
  Masc_mcp.Transport_grpc_next.run grpc_config room_config
