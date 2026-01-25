(* Simple test for atomic_update *)

open Masc_mcp

(* Initialize crypto RNG *)
let () = Encryption.initialize ()

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let test_dir = "/tmp/masc_test_debug" in
  
  (* Ensure clean dir *)
  (try Unix.mkdir test_dir 0o755 with _ -> ());
  (try Unix.mkdir (test_dir ^ "/.masc") 0o755 with _ -> ());
  
  let config = Room_eio.create_config ~fs test_dir in
  
  (* Test 1: Basic atomic_update *)
  Printf.printf "Test 1: Basic atomic_update_state\n%!";
  
  (* Register an agent which uses atomic_update_state *)
  (match Room_eio.register_agent config ~name:"test-agent" ~capabilities:[] () with
  | Ok _ -> Printf.printf "  ✓ Agent registered\n%!"
  | Error e -> Printf.printf "  ✗ Agent registration failed: %s\n%!" e);
  
  (* Check state *)
  (match Room_eio.read_state config with
  | Ok state -> 
      Printf.printf "  ✓ State read OK: %d active agents\n%!" (List.length state.active_agents)
  | Error e -> 
      Printf.printf "  ✗ State read failed: %s\n%!" e);
  
  (* Test 2: Broadcast which uses atomic_update_state *)
  Printf.printf "\nTest 2: Broadcast with atomic_update\n%!";
  (match Room_eio.broadcast config ~from_agent:"test-agent" ~content:"Hello!" with
  | Ok msg -> Printf.printf "  ✓ Broadcast OK: seq=%d\n%!" msg.seq
  | Error e -> Printf.printf "  ✗ Broadcast failed: %s\n%!" e);
  
  Printf.printf "\n✅ Single-process test complete\n%!"
