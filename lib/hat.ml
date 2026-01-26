(** Hat System - Role-based Persona for MASC Agents

    Inspired by ralph-orchestrator's Hat System.
    Each hat represents a specialized persona with distinct behaviors.

    Usage:
    - Agents can wear different hats for different tasks
    - Walph can rotate hats during iterations
    - Broadcast format: @agent:hat (e.g., @claude:builder)

    @see https://github.com/mikeyobrien/ralph-orchestrator
*)

(** {1 Types} *)

(** Available hat types *)
type t =
  | Builder     (** Code writing, implementation *)
  | Reviewer    (** Code review, quality checks *)
  | Researcher  (** Investigation, exploration *)
  | Tester      (** Test writing, coverage *)
  | Architect   (** Design, planning *)
  | Debugger    (** Bug hunting, fixing *)
  | Documenter  (** Documentation, comments *)
  | Custom of string  (** User-defined hat *)

(** Hat configuration *)
type config = {
  name: string;
  instructions: string;
  triggers: string list;      (** Events this hat responds to *)
  emits: string list;         (** Events this hat can emit *)
  backend: string option;     (** Preferred LLM backend *)
}

(** Agent with hat *)
type hatted_agent = {
  agent_name: string;
  current_hat: t;
  hat_history: (t * float) list;  (** (hat, timestamp) *)
}

(** {1 Serialization} *)

let to_string = function
  | Builder -> "builder"
  | Reviewer -> "reviewer"
  | Researcher -> "researcher"
  | Tester -> "tester"
  | Architect -> "architect"
  | Debugger -> "debugger"
  | Documenter -> "documenter"
  | Custom s -> s

let of_string = function
  | "builder" | "build" | "impl" -> Builder
  | "reviewer" | "review" -> Reviewer
  | "researcher" | "research" | "explore" -> Researcher
  | "tester" | "test" -> Tester
  | "architect" | "arch" | "design" -> Architect
  | "debugger" | "debug" | "fix" -> Debugger
  | "documenter" | "docs" | "doc" -> Documenter
  | s -> Custom s

let to_emoji = function
  | Builder -> "🔨"
  | Reviewer -> "🔍"
  | Researcher -> "🔬"
  | Tester -> "🧪"
  | Architect -> "📐"
  | Debugger -> "🐛"
  | Documenter -> "📝"
  | Custom _ -> "🎭"

(** {1 Default Configurations} *)

let default_config hat =
  let name = to_string hat in
  match hat with
  | Builder -> {
      name;
      instructions = "Focus on implementation. Write clean, working code. Follow existing patterns.";
      triggers = ["implement"; "build"; "create"; "add"];
      emits = ["code_written"; "needs_review"];
      backend = None;
    }
  | Reviewer -> {
      name;
      instructions = "Review code critically. Check for bugs, security issues, and style. Be constructive.";
      triggers = ["review"; "check"; "needs_review"];
      emits = ["approved"; "changes_requested"; "needs_fix"];
      backend = None;
    }
  | Researcher -> {
      name;
      instructions = "Investigate thoroughly. Gather information. Summarize findings clearly.";
      triggers = ["research"; "explore"; "investigate"; "find"];
      emits = ["findings"; "recommendation"];
      backend = Some "gemini";  (* Good for search/exploration *)
    }
  | Tester -> {
      name;
      instructions = "Write comprehensive tests. Aim for edge cases. Ensure coverage.";
      triggers = ["test"; "coverage"; "verify"];
      emits = ["tests_written"; "coverage_report"];
      backend = None;
    }
  | Architect -> {
      name;
      instructions = "Design solutions. Consider trade-offs. Document decisions.";
      triggers = ["design"; "plan"; "architect"];
      emits = ["design_doc"; "decision"];
      backend = Some "claude-cli";  (* Good for reasoning *)
    }
  | Debugger -> {
      name;
      instructions = "Find root cause. Fix bugs systematically. Add regression tests.";
      triggers = ["debug"; "fix"; "error"; "bug"];
      emits = ["bug_fixed"; "needs_test"];
      backend = None;
    }
  | Documenter -> {
      name;
      instructions = "Write clear documentation. Add examples. Keep it concise.";
      triggers = ["document"; "explain"; "docs"];
      emits = ["docs_written"];
      backend = None;
    }
  | Custom s -> {
      name = s;
      instructions = Printf.sprintf "Custom hat: %s" s;
      triggers = [s];
      emits = [s ^ "_done"];
      backend = None;
    }

(** {1 Parsing} *)

(** Parse "@agent:hat" format from message
    @return Some (agent, hat) or None *)
let parse_hatted_mention msg =
  let re = Str.regexp "@\\([a-zA-Z0-9_-]+\\):\\([a-zA-Z0-9_-]+\\)" in
  try
    let _ = Str.search_forward re msg 0 in
    let agent = Str.matched_group 1 msg in
    let hat_str = Str.matched_group 2 msg in
    Some (agent, of_string hat_str)
  with Not_found -> None

(** Extract all hatted mentions from a message *)
let extract_hatted_mentions msg =
  let re = Str.regexp "@\\([a-zA-Z0-9_-]+\\):\\([a-zA-Z0-9_-]+\\)" in
  let rec find_all start acc =
    try
      let _ = Str.search_forward re msg start in
      let agent = Str.matched_group 1 msg in
      let hat_str = Str.matched_group 2 msg in
      let end_pos = Str.match_end () in
      find_all end_pos ((agent, of_string hat_str) :: acc)
    with Not_found -> List.rev acc
  in
  find_all 0 []

(** {1 Hat Registry} *)

(** In-memory hat registry for agents *)
let agent_hats : (string, hatted_agent) Hashtbl.t = Hashtbl.create 16

(** Get or create hatted agent entry *)
let get_agent agent_name =
  match Hashtbl.find_opt agent_hats agent_name with
  | Some a -> a
  | None ->
      let a = { agent_name; current_hat = Builder; hat_history = [] } in
      Hashtbl.replace agent_hats agent_name a;
      a

(** Change agent's hat *)
let wear ~agent_name hat =
  let agent = get_agent agent_name in
  let now = Unix.gettimeofday () in
  let updated = {
    agent with
    current_hat = hat;
    hat_history = (hat, now) :: agent.hat_history;
  } in
  Hashtbl.replace agent_hats agent_name updated;
  Printf.sprintf "%s %s now wearing %s hat"
    (to_emoji hat) agent_name (to_string hat)

(** Get agent's current hat *)
let current ~agent_name =
  let agent = get_agent agent_name in
  agent.current_hat

(** Format agent with hat for display *)
let format_agent agent_name =
  let agent = get_agent agent_name in
  Printf.sprintf "%s %s:%s"
    (to_emoji agent.current_hat)
    agent_name
    (to_string agent.current_hat)

(** {1 Hat Rotation for Walph} *)

(** Preset hat rotations *)
type rotation =
  | Single of t                     (** One hat throughout *)
  | Alternate of t * t              (** Alternate between two *)
  | Sequence of t list              (** Fixed sequence, repeat *)
  | TDD                             (** Tester → Builder → Tester... *)
  | ReviewLoop                      (** Builder → Reviewer → Builder... *)

let rotation_to_string = function
  | Single h -> Printf.sprintf "single(%s)" (to_string h)
  | Alternate (a, b) -> Printf.sprintf "alt(%s,%s)" (to_string a) (to_string b)
  | Sequence hats -> Printf.sprintf "seq(%s)" (String.concat "," (List.map to_string hats))
  | TDD -> "tdd"
  | ReviewLoop -> "review-loop"

(** Get next hat in rotation *)
let next_in_rotation rotation current_idx =
  match rotation with
  | Single h -> (h, 0)
  | Alternate (a, b) ->
      if current_idx mod 2 = 0 then (a, current_idx + 1)
      else (b, current_idx + 1)
  | Sequence hats ->
      let len = List.length hats in
      if len = 0 then (Builder, 0)
      else
        let idx = current_idx mod len in
        (List.nth hats idx, current_idx + 1)
  | TDD ->
      if current_idx mod 2 = 0 then (Tester, current_idx + 1)
      else (Builder, current_idx + 1)
  | ReviewLoop ->
      if current_idx mod 2 = 0 then (Builder, current_idx + 1)
      else (Reviewer, current_idx + 1)

(** {1 JSON Serialization} *)

let to_json hat =
  `Assoc [
    ("hat", `String (to_string hat));
    ("emoji", `String (to_emoji hat));
  ]

let config_to_json config =
  `Assoc [
    ("name", `String config.name);
    ("instructions", `String config.instructions);
    ("triggers", `List (List.map (fun s -> `String s) config.triggers));
    ("emits", `List (List.map (fun s -> `String s) config.emits));
    ("backend", match config.backend with None -> `Null | Some b -> `String b);
  ]

let hatted_agent_to_json agent =
  `Assoc [
    ("agent_name", `String agent.agent_name);
    ("current_hat", to_json agent.current_hat);
    ("hat_changes", `Int (List.length agent.hat_history));
  ]

(** List all hatted agents *)
let list_all () =
  Hashtbl.fold (fun _ agent acc -> agent :: acc) agent_hats []
