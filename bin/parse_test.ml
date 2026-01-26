open Masc_mcp

let () =
  let tests = [
    (* Basic patterns *)
    "@test-gentle-gecko STATEFUL";
    "@ollama-gentle-gecko test";
    "@ollama test";
    "@@gemini broadcast";
    (* Edge cases *)
    "@ollama-test";              (* 2-part: should be Stateful *)
    "@claude-code-v2";           (* 3-part with number *)
    "@gemini-swift-fox123";      (* number at end *)
    "@ alone";                   (* just @ *)
    "@@ broadcast";              (* @@ without agent *)
    "no mention here";           (* no @ at all *)
    "@_underscore";              (* underscore agent *)
  ] in
  Printf.printf "%-35s   %-30s   %s\n" "Input" "Mode" "Extract";
  Printf.printf "%s\n" (String.make 80 '-');
  List.iter (fun msg ->
    let mode = Mention.parse msg in
    let ext = match Mention.extract msg with Some s -> s | None -> "None" in
    Printf.printf "%-35s → %-30s | ext=%s\n" msg (Mention.mode_to_string mode) ext
  ) tests
