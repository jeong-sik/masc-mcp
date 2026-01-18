(** MASC Handover - Cellular Agent DNA Transfer

    Implements the "last will and testament" pattern for agent succession.
    When an agent dies (context limit, timeout, crash), it leaves behind
    structured state for the next agent to inherit.

    Inspired by:
    - Stanford Generative Agents (Memory Stream + Reflection)
    - MemGPT (Self-managed memory tiers)
    - Erlang "Let It Crash" (Supervisor + State recovery)
    - A-MEM (Zettelkasten linking)
*)

open Lwt.Syntax

(** Handover record - the "DNA" passed to next agent *)
type handover_record = {
  id: string;
  from_agent: string;
  to_agent: string option;  (* None = any agent can pick up *)
  task_id: string;
  session_id: string;

  (* Core state *)
  current_goal: string;
  progress_summary: string;
  completed_steps: string list;
  pending_steps: string list;

  (* Thinking context - the "implicit knowledge" problem *)
  key_decisions: string list;      (* Why we chose X over Y *)
  assumptions: string list;        (* What we're assuming is true *)
  warnings: string list;           (* Watch out for these *)

  (* Error state from PDCA *)
  unresolved_errors: string list;

  (* Files and resources *)
  modified_files: string list;
  locked_files: string list;

  (* Metadata *)
  created_at: float;
  context_usage_percent: int;  (* How full was context when handover triggered *)
  handover_reason: string;     (* "context_limit" | "timeout" | "explicit" | "error" *)
}

(** Handover trigger reasons *)
type trigger_reason =
  | ContextLimit of int   (* percentage *)
  | Timeout of int        (* seconds elapsed *)
  | Explicit              (* user/agent requested *)
  | FatalError of string  (* fatal error *)
  | TaskComplete          (* normal completion *)

let trigger_reason_to_string = function
  | ContextLimit pct -> Printf.sprintf "context_limit_%d" pct
  | Timeout secs -> Printf.sprintf "timeout_%ds" secs
  | Explicit -> "explicit"
  | FatalError msg -> Printf.sprintf "error: %s" msg
  | TaskComplete -> "task_complete"

(** Generate unique handover ID *)
let generate_id () =
  let timestamp = Unix.gettimeofday () in
  let random = Random.int 100000 in
  Printf.sprintf "handover-%d-%05d" (int_of_float (timestamp *. 1000.)) random

(** Create empty handover record *)
let create_handover ~from_agent ~task_id ~session_id ~reason : handover_record =
  {
    id = generate_id ();
    from_agent;
    to_agent = None;
    task_id;
    session_id;
    current_goal = "";
    progress_summary = "";
    completed_steps = [];
    pending_steps = [];
    key_decisions = [];
    assumptions = [];
    warnings = [];
    unresolved_errors = [];
    modified_files = [];
    locked_files = [];
    created_at = Unix.gettimeofday ();
    context_usage_percent = 0;
    handover_reason = trigger_reason_to_string reason;
  }

(** Handover to JSON *)
let handover_to_json (h : handover_record) : Yojson.Safe.t =
  `Assoc [
    ("id", `String h.id);
    ("from_agent", `String h.from_agent);
    ("to_agent", match h.to_agent with Some a -> `String a | None -> `Null);
    ("task_id", `String h.task_id);
    ("session_id", `String h.session_id);
    ("current_goal", `String h.current_goal);
    ("progress_summary", `String h.progress_summary);
    ("completed_steps", `List (List.map (fun s -> `String s) h.completed_steps));
    ("pending_steps", `List (List.map (fun s -> `String s) h.pending_steps));
    ("key_decisions", `List (List.map (fun s -> `String s) h.key_decisions));
    ("assumptions", `List (List.map (fun s -> `String s) h.assumptions));
    ("warnings", `List (List.map (fun s -> `String s) h.warnings));
    ("unresolved_errors", `List (List.map (fun s -> `String s) h.unresolved_errors));
    ("modified_files", `List (List.map (fun s -> `String s) h.modified_files));
    ("locked_files", `List (List.map (fun s -> `String s) h.locked_files));
    ("created_at", `Float h.created_at);
    ("context_usage_percent", `Int h.context_usage_percent);
    ("handover_reason", `String h.handover_reason);
  ]

(** JSON to handover *)
let handover_of_json (json : Yojson.Safe.t) : handover_record option =
  let open Yojson.Safe.Util in
  try
    let str key = json |> member key |> to_string in
    let str_opt key = json |> member key |> to_string_option in
    let str_list key = json |> member key |> to_list |> List.map to_string in
    let int_val key = json |> member key |> to_int in
    let float_val key = json |> member key |> to_float in
    Some {
      id = str "id";
      from_agent = str "from_agent";
      to_agent = str_opt "to_agent";
      task_id = str "task_id";
      session_id = str "session_id";
      current_goal = str "current_goal";
      progress_summary = str "progress_summary";
      completed_steps = str_list "completed_steps";
      pending_steps = str_list "pending_steps";
      key_decisions = str_list "key_decisions";
      assumptions = str_list "assumptions";
      warnings = str_list "warnings";
      unresolved_errors = str_list "unresolved_errors";
      modified_files = str_list "modified_files";
      locked_files = str_list "locked_files";
      created_at = float_val "created_at";
      context_usage_percent = int_val "context_usage_percent";
      handover_reason = str "handover_reason";
    }
  with _ -> None

(** Storage paths *)
let handover_dir (config : Room_utils.config) =
  Filename.concat config.base_path ".masc/handovers"

let handover_path config handover_id =
  Filename.concat (handover_dir config) (handover_id ^ ".json")

let pending_handovers_path config =
  Filename.concat (handover_dir config) "pending.json"

(** Ensure directory exists *)
let ensure_dir path =
  if not (Sys.file_exists path) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" path))

(** Save handover to filesystem *)
let save_handover config (h : handover_record) : (unit, string) result Lwt.t =
  let dir = handover_dir config in
  ensure_dir dir;
  let path = handover_path config h.id in
  let json = handover_to_json h in
  try
    let oc = open_out path in
    output_string oc (Yojson.Safe.pretty_to_string json);
    close_out oc;
    Lwt.return (Ok ())
  with exn ->
    Lwt.return (Error (Printf.sprintf "Failed to save handover: %s" (Printexc.to_string exn)))

(** Load handover from filesystem *)
let load_handover config handover_id : (handover_record, string) result Lwt.t =
  let path = handover_path config handover_id in
  if not (Sys.file_exists path) then
    Lwt.return (Error (Printf.sprintf "Handover not found: %s" handover_id))
  else
    try
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      match handover_of_json json with
      | Some h -> Lwt.return (Ok h)
      | None -> Lwt.return (Error "Failed to parse handover JSON")
    with exn ->
      Lwt.return (Error (Printf.sprintf "Failed to load handover: %s" (Printexc.to_string exn)))

(** List all handovers *)
let list_handovers config : handover_record list Lwt.t =
  let dir = handover_dir config in
  if not (Sys.file_exists dir) then Lwt.return []
  else
    let files = Sys.readdir dir |> Array.to_list in
    let json_files = List.filter (fun f ->
      Filename.check_suffix f ".json" && f <> "pending.json"
    ) files in
    let* handovers = Lwt_list.filter_map_s (fun f ->
      let id = Filename.chop_suffix f ".json" in
      let+ result = load_handover config id in
      match result with Ok h -> Some h | Error _ -> None
    ) json_files in
    (* Sort by created_at descending *)
    Lwt.return (List.sort (fun a b -> compare b.created_at a.created_at) handovers)

(** Get pending handovers (not yet picked up) *)
let get_pending_handovers config : handover_record list Lwt.t =
  let* all = list_handovers config in
  Lwt.return (List.filter (fun h -> h.to_agent = None) all)

(** Claim a handover (agent picks it up) *)
let claim_handover config ~handover_id ~agent_name : (handover_record, string) result Lwt.t =
  let* result = load_handover config handover_id in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok h ->
    if Option.is_some h.to_agent then
      Lwt.return (Error (Printf.sprintf "Handover already claimed by %s" (Option.get h.to_agent)))
    else
      let h' = { h with to_agent = Some agent_name } in
      let* save_result = save_handover config h' in
      match save_result with
      | Error e -> Lwt.return (Error e)
      | Ok () -> Lwt.return (Ok h')

(** Format handover as markdown for agent consumption *)
let format_as_markdown (h : handover_record) : string =
  let buf = Buffer.create 2048 in
  let add s = Buffer.add_string buf s; Buffer.add_char buf '\n' in

  add "# 🧬 Handover DNA";
  add "";
  add (Printf.sprintf "**From**: %s → **To**: %s" h.from_agent
    (Option.value h.to_agent ~default:"(unclaimed)"));
  add (Printf.sprintf "**Task**: %s | **Reason**: %s" h.task_id h.handover_reason);
  add "";

  add "## 🎯 Current Goal";
  add h.current_goal;
  add "";

  add "## 📊 Progress";
  add h.progress_summary;
  add "";

  if h.completed_steps <> [] then begin
    add "### ✅ Completed";
    List.iter (fun s -> add ("- " ^ s)) h.completed_steps;
    add ""
  end;

  if h.pending_steps <> [] then begin
    add "### ⏳ Pending";
    List.iter (fun s -> add ("- " ^ s)) h.pending_steps;
    add ""
  end;

  if h.key_decisions <> [] then begin
    add "## 🧠 Key Decisions (Why)";
    List.iter (fun s -> add ("- " ^ s)) h.key_decisions;
    add ""
  end;

  if h.assumptions <> [] then begin
    add "## 💭 Assumptions";
    List.iter (fun s -> add ("- " ^ s)) h.assumptions;
    add ""
  end;

  if h.warnings <> [] then begin
    add "## ⚠️ Warnings";
    List.iter (fun s -> add ("- " ^ s)) h.warnings;
    add ""
  end;

  if h.unresolved_errors <> [] then begin
    add "## ❌ Unresolved Errors";
    List.iter (fun s -> add ("- " ^ s)) h.unresolved_errors;
    add ""
  end;

  if h.modified_files <> [] then begin
    add "## 📁 Modified Files";
    List.iter (fun s -> add ("- " ^ s)) h.modified_files;
    add ""
  end;

  Buffer.contents buf

(** Create handover from current planning context *)
let create_from_planning config ~from_agent ~task_id ~session_id ~reason
    ~goal ~progress ~completed ~pending ~decisions ~assumptions ~warnings
    ~errors ~files ~context_pct : (handover_record, string) result Lwt.t =
  let h = {
    (create_handover ~from_agent ~task_id ~session_id ~reason) with
    current_goal = goal;
    progress_summary = progress;
    completed_steps = completed;
    pending_steps = pending;
    key_decisions = decisions;
    assumptions = assumptions;
    warnings = warnings;
    unresolved_errors = errors;
    modified_files = files;
    locked_files =
      Room.get_all_active_locks config
      |> List.filter (fun (lock : Types.file_lock) -> lock.locked_by = from_agent)
      |> List.map (fun (lock : Types.file_lock) -> lock.file_path);
    context_usage_percent = context_pct;
  } in
  let* result = save_handover config h in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok () -> Lwt.return (Ok h)

(** Build a prompt for the successor agent from handover DNA *)
let build_successor_prompt (h : handover_record) ~additional_instructions : string =
  let dna = format_as_markdown h in
  let instructions = match additional_instructions with
    | Some i -> Printf.sprintf "\n\n## Additional Instructions\n%s" i
    | None -> ""
  in
  Printf.sprintf {|You are continuing work from a previous agent session.

%s%s

## Your Mission
1. Review the DNA above to understand the context
2. Continue from the pending steps
3. Use MASC tools (masc_done, masc_broadcast) to coordinate
4. When finished or hitting limits, create your own handover with masc_handover_create

Begin work now.|} dna instructions

(** Claim a handover and spawn the successor agent *)
let claim_and_spawn config ~handover_id ~agent_name ?additional_instructions ?timeout_seconds ()
    : (Spawn.spawn_result, string) result Lwt.t =
  (* First, claim the handover *)
  let* claim_result = claim_handover config ~handover_id ~agent_name in
  match claim_result with
  | Error e -> Lwt.return (Error e)
  | Ok h ->
      (* Build prompt from DNA *)
      let prompt = build_successor_prompt h ~additional_instructions in
      (* Spawn the agent *)
      let result = Spawn.spawn
        ~agent_name
        ~prompt
        ?timeout_seconds
        ~working_dir:config.base_path
        ()
      in
      Lwt.return (Ok result)
