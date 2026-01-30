(** MASC Handover - Cellular Agent DNA Transfer (Eio Native)

    Implements the "last will and testament" pattern for agent succession.
    When an agent dies (context limit, timeout, crash), it leaves behind
    structured state for the next agent to inherit.
*)


(** Handover record - the "DNA" passed to next agent *)
type handover_record = {
  id: string;
  from_agent: string;
  to_agent: string option;
  task_id: string;
  session_id: string;

  (* Core state *)
  current_goal: string;
  progress_summary: string;
  completed_steps: string list;
  pending_steps: string list;

  (* Thinking context *)
  key_decisions: string list;
  assumptions: string list;
  warnings: string list;

  (* Error state from PDCA *)
  unresolved_errors: string list;

  (* Files and resources *)
  modified_files: string list;

  (* Metadata *)
  created_at: float;
  context_usage_percent: int;
  handover_reason: string;
}

(** Handover trigger reasons *)
type trigger_reason =
  | ContextLimit of int
  | Timeout of int
  | Explicit
  | FatalError of string
  | TaskComplete

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
      created_at = float_val "created_at";
      context_usage_percent = int_val "context_usage_percent";
      handover_reason = str "handover_reason";
    }
  with Yojson.Safe.Util.Type_error _ | Yojson.Json_error _ -> None

(** Storage paths *)
let handover_dir_path (config : Room_utils.config) =
  let path = Filename.concat config.base_path ".masc/handovers" in
  path

let handover_file_path config handover_id =
  Filename.concat (handover_dir_path config) (handover_id ^ ".json")

(** Ensure directory exists using Eio *)
let ensure_dir fs dir_path =
  let path = Eio.Path.(fs / dir_path) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path

(** Save handover to filesystem *)
let save_handover ~fs config (h : handover_record) : (unit, string) result =
  let dir = handover_dir_path config in
  ensure_dir fs dir;
  let file = handover_file_path config h.id in
  let json = handover_to_json h in
  try
    let path = Eio.Path.(fs / file) in
    Eio.Path.save ~create:(`Or_truncate 0o600) path (Yojson.Safe.pretty_to_string json);
    Ok ()
  with exn ->
    Error (Printf.sprintf "Failed to save handover: %s" (Printexc.to_string exn))

(** Load handover from filesystem *)
let load_handover ~fs config handover_id : (handover_record, string) result =
  let file = handover_file_path config handover_id in
  let path = Eio.Path.(fs / file) in
  try
    let content = Eio.Path.load path in
    let json = Yojson.Safe.from_string content in
    match handover_of_json json with
    | Some h -> Ok h
    | None -> Error "Failed to parse handover JSON"
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
      Error (Printf.sprintf "Handover not found: %s" handover_id)
  | exn ->
      Error (Printf.sprintf "Failed to load handover: %s" (Printexc.to_string exn))

(** List all handovers *)
let list_handovers ~fs config : handover_record list =
  let dir = handover_dir_path config in
  let path = Eio.Path.(fs / dir) in
  try
    let files = Eio.Path.read_dir path in
    let json_files = List.filter (fun f ->
      Filename.check_suffix f ".json" && f <> "pending.json"
    ) files in
    let handovers = List.filter_map (fun f ->
      let id = Filename.chop_suffix f ".json" in
      match load_handover ~fs config id with Ok h -> Some h | Error _ -> None
    ) json_files in
    List.sort (fun a b -> compare b.created_at a.created_at) handovers
  with Eio.Io _ | Yojson.Json_error _ -> []

(** Get pending handovers *)
let get_pending_handovers ~fs config : handover_record list =
  let all = list_handovers ~fs config in
  List.filter (fun h -> h.to_agent = None) all

(** Claim a handover *)
let claim_handover ~fs config ~handover_id ~agent_name : (handover_record, string) result =
  match load_handover ~fs config handover_id with
  | Error e -> Error e
  | Ok h ->
    if Option.is_some h.to_agent then
      Error (Printf.sprintf "Handover already claimed by %s" (Option.get h.to_agent))
    else
      let h' = { h with to_agent = Some agent_name } in
      match save_handover ~fs config h' with
      | Error e -> Error e
      | Ok () -> Ok h'

(** Format handover as markdown *)
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

(** Build a prompt for the successor agent *)
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
let claim_and_spawn ~sw ~fs ~proc_mgr config ~handover_id ~agent_name ?additional_instructions ?timeout_seconds () 
    : (Spawn_eio.spawn_result, string) result =
  match claim_handover ~fs config ~handover_id ~agent_name with
  | Error e -> Error e
  | Ok h ->
      let prompt = build_successor_prompt h ~additional_instructions in
      let result = Spawn_eio.spawn
        ~sw
        ~proc_mgr
        ~agent_name
        ~prompt
        ?timeout_seconds
        ~working_dir:config.base_path
        ()
      in
      Ok result
