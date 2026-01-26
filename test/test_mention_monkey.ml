(** Monkey/Fuzz testing for Mention module *)

open Masc_mcp

(** Random string generator *)
let random_string len =
  let chars = "abcdefghijklmnopqrstuvwxyz0123456789_-@" in
  String.init len (fun _ -> chars.[Random.int (String.length chars)])

(** Generate random mention-like strings *)
let random_mention () =
  let patterns = [
    (* Valid patterns *)
    (fun () -> Printf.sprintf "@%s" (random_string (1 + Random.int 10)));
    (fun () -> Printf.sprintf "@@%s" (random_string (1 + Random.int 10)));
    (fun () -> Printf.sprintf "@%s-%s-%s"
      (random_string (2 + Random.int 5))
      (random_string (2 + Random.int 5))
      (random_string (2 + Random.int 5)));
    (* Edge cases *)
    (fun () -> "@");
    (fun () -> "@@");
    (fun () -> "@-");
    (fun () -> "@_");
    (fun () -> "@ ");
    (fun () -> "@\n");
    (fun () -> "@\t");
    (fun () -> String.make (1 + Random.int 1000) '@');
    (fun () -> "@" ^ String.make (Random.int 500) 'a');
    (fun () -> random_string (Random.int 100));
    (* Unicode attempts *)
    (fun () -> "@한글");
    (fun () -> "@émoji");
    (fun () -> "@🚀");
    (* Injection attempts *)
    (fun () -> "@agent; rm -rf /");
    (fun () -> "@agent\x00null");
    (fun () -> "@agent<script>");
  ] in
  let gen = List.nth patterns (Random.int (List.length patterns)) in
  gen ()

(** Test that parse never crashes *)
let test_no_crash () =
  let iterations = 10000 in
  let start = Unix.gettimeofday () in
  for _ = 1 to iterations do
    let input = random_mention () in
    let _ = Mention.parse input in
    let _ = Mention.extract input in
    ()
  done;
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "✅ %d iterations in %.3fs (%.0f ops/sec)\n"
    iterations elapsed (float_of_int iterations /. elapsed)

(** Test that extract result is consistent with parse *)
let test_consistency () =
  let failures = ref 0 in
  for _ = 1 to 1000 do
    let input = random_mention () in
    let mode = Mention.parse input in
    let extract = Mention.extract input in
    let consistent = match mode, extract with
      | Mention.None, None -> true
      | Mention.Stateless s, Some e -> s = e
      | Mention.Stateful s, Some e -> s = e
      | Mention.Broadcast s, Some e -> s = e
      | _ -> false
    in
    if not consistent then begin
      Printf.printf "❌ Inconsistent: input=%S mode=%s extract=%s\n"
        input (Mention.mode_to_string mode)
        (match extract with Some s -> s | None -> "None");
      incr failures
    end
  done;
  if !failures = 0 then
    Printf.printf "✅ 1000 consistency checks passed\n"
  else
    Printf.printf "❌ %d consistency failures\n" !failures

(** Test very long inputs *)
let test_long_inputs () =
  let lengths = [100; 1000; 10000; 100000] in
  List.iter (fun len ->
    let input = "@" ^ String.make len 'a' in
    let start = Unix.gettimeofday () in
    let _ = Mention.parse input in
    let elapsed = Unix.gettimeofday () -. start in
    if elapsed > 0.1 then
      Printf.printf "⚠️  Length %d took %.3fs (slow!)\n" len elapsed
    else
      Printf.printf "✅ Length %d: %.4fs\n" len elapsed
  ) lengths

(** Test concurrent access (if applicable) *)
let test_rapid_fire () =
  let count = 50000 in
  let start = Unix.gettimeofday () in
  for i = 1 to count do
    let input = Printf.sprintf "@agent-%d-test" i in
    let _ = Mention.parse input in
    ()
  done;
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "✅ Rapid fire: %d parses in %.3fs (%.0f ops/sec)\n"
    count elapsed (float_of_int count /. elapsed)

let () =
  Random.self_init ();
  Printf.printf "\n🐒 Mention Monkey Test\n";
  Printf.printf "========================\n\n";

  Printf.printf "1. Crash resistance (10,000 random inputs):\n";
  test_no_crash ();

  Printf.printf "\n2. Parse/Extract consistency:\n";
  test_consistency ();

  Printf.printf "\n3. Long input performance:\n";
  test_long_inputs ();

  Printf.printf "\n4. Rapid fire (50,000 sequential):\n";
  test_rapid_fire ();

  Printf.printf "\n🎉 Monkey test complete!\n"
