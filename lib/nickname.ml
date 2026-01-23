(** Nickname generator for MASC agents - Docker-style adjective+animal *)

(* Adjectives - positive, memorable, easy to pronounce *)
let adjectives = [|
  "swift"; "brave"; "calm"; "eager"; "fierce";
  "gentle"; "happy"; "jolly"; "keen"; "lucky";
  "merry"; "noble"; "proud"; "quick"; "witty";
  "bold"; "cool"; "deft"; "fair"; "grand";
  "hale"; "jade"; "kind"; "lean"; "neat";
  "pale"; "rare"; "sage"; "tame"; "warm";
|]

(* Animals - recognizable, memorable *)
let animals = [|
  "fox"; "bear"; "wolf"; "hawk"; "lion";
  "tiger"; "eagle"; "otter"; "panda"; "koala";
  "raven"; "falcon"; "badger"; "beaver"; "whale";
  "shark"; "crane"; "heron"; "moose"; "viper";
  "cobra"; "gecko"; "lemur"; "llama"; "manta";
  "orca"; "rhino"; "sloth"; "tapir"; "zebra";
|]

(* Initialize random seed once *)
let () = Random.self_init ()

(** Generate a short random suffix (4 hex chars) for uniqueness *)
let random_suffix () =
  Printf.sprintf "%04x" (Random.int 0xFFFF)

(** Generate a unique nickname for an agent type.
    Format: {agent_type}-{adjective}-{animal}
    Example: claude-swift-fox, gemini-brave-tiger *)
let generate agent_type =
  let adj = adjectives.(Random.int (Array.length adjectives)) in
  let animal = animals.(Random.int (Array.length animals)) in
  Printf.sprintf "%s-%s-%s" agent_type adj animal

(** Generate with suffix for guaranteed uniqueness.
    Format: {agent_type}-{adjective}-{animal}-{hex4}
    Example: claude-swift-fox-a3b2 *)
let generate_unique agent_type =
  let base = generate agent_type in
  Printf.sprintf "%s-%s" base (random_suffix ())

(** Check if a name looks like a generated nickname.
    Returns true for patterns like "claude-swift-fox" *)
let is_generated_nickname name =
  let parts = String.split_on_char '-' name in
  List.length parts >= 3

(** Extract agent_type from a generated nickname.
    "claude-swift-fox" -> Some "claude"
    "claude" -> Some "claude" (legacy) *)
let extract_agent_type name =
  let parts = String.split_on_char '-' name in
  match parts with
  | agent_type :: _ -> Some agent_type
  | [] -> None
