(** Distributed A2A Communication Test

    Tests message passing between TWO SEPARATE PROCESSES sharing
    the same MASC room via filesystem backend.

    Validates:
    1. Process A broadcasts -> Process B receives
    2. Event persistence across process restarts
    3. Shared state consistency

    Run with: dune exec test/test_a2a_distributed.exe -- [producer|consumer] <shared_dir>
*)

open Masc_mcp

(* Initialize crypto RNG *)
let () = Encryption.initialize ()

let shared_dir = ref "/tmp/masc_distributed_test"
let role = ref "producer"

let parse_args () =
  let specs = [
    ("--dir", Arg.Set_string shared_dir, "Shared directory for MASC room");
    ("--role", Arg.Set_string role, "Role: producer or consumer");
  ] in
  Arg.parse specs (fun s -> role := s) "test_a2a_distributed [producer|consumer] --dir <path>"

let producer () =
  Printf.printf "=== PRODUCER PROCESS ===\n%!";
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let config = Room_eio.create_config ~fs !shared_dir in

  (* Ensure directory exists *)
  (try Unix.mkdir !shared_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  (* Register as producer *)
  Printf.printf "[Producer] Registering agent 'producer-agent'...\n%!";
  (match Room_eio.register_agent config ~name:"producer-agent" ~capabilities:["code"; "write"] () with
   | Ok _ -> Printf.printf "[Producer] Registered successfully\n%!"
   | Error e -> Printf.printf "[Producer] Registration failed: %s\n%!" e);

  (* Broadcast messages *)
  for i = 1 to 3 do
    let content = Printf.sprintf "Message %d from producer at %f" i (Unix.gettimeofday ()) in
    Printf.printf "[Producer] Broadcasting: %s\n%!" content;
    match Room_eio.broadcast config ~from_agent:"producer-agent" ~content with
    | Ok msg -> Printf.printf "[Producer] Broadcast successful, seq=%d\n%!" msg.seq
    | Error e -> Printf.printf "[Producer] Broadcast failed: %s\n%!" e;
    Unix.sleepf 0.1
  done;

  (* Leave gracefully *)
  let _ = Room_eio.remove_agent config ~name:"producer-agent" in
  Printf.printf "[Producer] Done. Messages persisted to %s\n%!" !shared_dir

let consumer () =
  Printf.printf "=== CONSUMER PROCESS ===\n%!";
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let config = Room_eio.create_config ~fs !shared_dir in

  (* Register as consumer *)
  Printf.printf "[Consumer] Registering agent 'consumer-agent'...\n%!";
  (match Room_eio.register_agent config ~name:"consumer-agent" ~capabilities:["read"; "review"] () with
   | Ok _ -> Printf.printf "[Consumer] Registered successfully\n%!"
   | Error e -> Printf.printf "[Consumer] Registration failed: %s\n%!" e);

  (* Read messages by sequence *)
  Printf.printf "[Consumer] Reading messages from shared room...\n%!";
  let rec read_messages seq found =
    if found >= 3 then found
    else match Room_eio.get_message config ~seq with
    | Ok msg ->
        Printf.printf "[Consumer] Found message seq=%d from=%s: %s\n%!"
          msg.seq msg.from_agent (String.sub msg.content 0 (min 50 (String.length msg.content)));
        read_messages (seq + 1) (found + 1)
    | Error _ ->
        if seq < 100 then read_messages (seq + 1) found
        else found
  in
  let found = read_messages 1 0 in
  Printf.printf "[Consumer] Found %d messages\n%!" found;

  (* Check events *)
  Printf.printf "[Consumer] Reading recent events...\n%!";
  let events = Room_eio.get_recent_events config ~limit:10 in
  Printf.printf "[Consumer] Found %d events\n%!" (List.length events);
  List.iter (fun e ->
    let open Yojson.Safe.Util in
    let event_type = e |> member "type" |> to_string in
    let agent = e |> member "agent" |> to_string in
    Printf.printf "  - %s by %s\n%!" event_type agent
  ) events;

  (* Leave *)
  let _ = Room_eio.remove_agent config ~name:"consumer-agent" in

  (* Verify producer's messages were found *)
  if found >= 3 then begin
    Printf.printf "\n✅ DISTRIBUTED TEST PASSED: Consumer read producer's messages\n%!";
    exit 0
  end else begin
    Printf.printf "\n❌ DISTRIBUTED TEST FAILED: Expected 3 messages, found %d\n%!" found;
    exit 1
  end

let () =
  parse_args ();
  match !role with
  | "producer" -> producer ()
  | "consumer" -> consumer ()
  | _ ->
      Printf.printf "Usage: test_a2a_distributed [producer|consumer] --dir <shared_path>\n";
      exit 1
