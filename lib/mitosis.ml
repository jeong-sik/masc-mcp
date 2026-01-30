(** MASC Mitosis - Cell Division Pattern for Infinite Agent Lifecycle

    Inspired by cellular biology:
    - Mitosis: Agent division before context overflow
    - Apoptosis: Graceful death after task completion
    - Stem Cells: Reserve agents ready for instant handoff

    Key insight: Don't wait for 80% - divide proactively and continuously *)

(** Lifecycle state of an agent cell *)
type cell_state =
  | Stem        (* Reserve, ready to differentiate *)
  | Active      (* Currently working *)
  | Prepared    (* Ready for handoff, DNA extracted - NEW! *)
  | Dividing    (* In the process of handoff *)
  | Apoptotic   (* Gracefully shutting down *)

(** Mitosis phase - 2-phase approach *)
type mitosis_phase =
  | Idle        (* Normal operation *)
  | ReadyForHandoff of string  (* DNA already extracted, waiting for handoff threshold *)

(** Agent cell with lifecycle metadata *)
type cell = {
  id: string;
  generation: int;          (* How many divisions from origin *)
  state: cell_state;
  phase: mitosis_phase;     (* Current mitosis phase - NEW! *)
  born_at: float;
  last_activity: float;
  context_dna: string option;  (* Compressed context from parent *)
  prepared_dna: string option; (* DNA extracted at prepare phase - NEW! *)
  prepare_context_len: int;    (* Context length when DNA was extracted - for delta calc *)
  task_count: int;
  tool_call_count: int;
}

(** Stem Cell Pool - reserve agents ready for instant activation *)
type stem_pool = {
  cells: cell list;
  max_size: int;
  warm_up_count: int;  (* How many to keep warm *)
}

(** Mitosis trigger conditions *)
type mitosis_trigger =
  | Time_based of float         (* Every N seconds *)
  | Task_count of int           (* Every N tasks *)
  | Tool_calls of int           (* Every N tool calls *)
  | Context_threshold of float  (* At N% context usage *)
  | Complexity_spike            (* When task complexity increases *)

(** Mitosis configuration *)
type mitosis_config = {
  triggers: mitosis_trigger list;
  stem_pool_size: int;
  max_generation: int;         (* Prevent infinite division *)
  dna_compression_ratio: float;
  apoptosis_delay: float;      (* Grace period before death *)
  (* 2-Phase thresholds *)
  prepare_threshold: float;    (* When to extract DNA and prepare (default: 0.5) *)
  handoff_threshold: float;    (* When to actually handoff (default: 0.8) *)
  (* Delta quality controls - BALTHASAR feedback *)
  min_context_for_delta: int;  (* Skip delta if context < this (short session exception) *)
  min_delta_len: int;          (* Skip delta if delta < this (quality threshold) *)
}

(** Default mitosis configuration - 2-phase approach *)
let default_config = {
  triggers = [
    Time_based 300.0;        (* Every 5 minutes *)
    Task_count 10;           (* Every 10 tasks *)
    Tool_calls 20;           (* Every 20 tool calls *)
  ];
  stem_pool_size = 2;
  max_generation = 10;       (* Reduced from 100 per MAGI feedback *)
  dna_compression_ratio = 0.1;
  apoptosis_delay = 5.0;
  (* 2-Phase thresholds *)
  prepare_threshold = 0.5;   (* 50%: Extract DNA, warm up stem cell *)
  handoff_threshold = 0.8;   (* 80%: Actually transfer work (same as relay) *)
  (* Delta quality controls *)
  min_context_for_delta = 1000;  (* < 1000 chars = short session, skip delta *)
  min_delta_len = 100;           (* < 100 chars delta = noise, skip *)
}

(** Create a new stem cell *)
let create_stem_cell ~generation =
  let id = Printf.sprintf "cell-%d-%d" generation (int_of_float (Unix.gettimeofday () *. 1000.0) mod 10000) in
  {
    id;
    generation;
    state = Stem;
    phase = Idle;
    born_at = Unix.gettimeofday ();
    last_activity = Unix.gettimeofday ();
    context_dna = None;
    prepared_dna = None;
    prepare_context_len = 0;
    task_count = 0;
    tool_call_count = 0;
  }

(** Initialize stem cell pool *)
let init_pool ~config =
  let cells = List.init config.stem_pool_size (fun i ->
    create_stem_cell ~generation:i
  ) in
  {
    cells;
    max_size = config.stem_pool_size;
    warm_up_count = min 2 config.stem_pool_size;
  }

(** Check if non-context triggers are met *)
let check_non_context_triggers ~config ~cell =
  let now = Unix.gettimeofday () in
  let age = now -. cell.born_at in
  List.exists (function
    | Time_based interval -> age >= interval
    | Task_count n -> cell.task_count >= n
    | Tool_calls n -> cell.tool_call_count >= n
    | Context_threshold _ -> false  (* Handled by 2-phase thresholds *)
    | Complexity_spike -> false
  ) config.triggers

(** Phase 1: Should we PREPARE for division? (extract DNA, warm up) *)
let should_prepare ~config ~cell ~context_ratio =
  (* Only prepare if not already prepared *)
  match cell.phase with
  | ReadyForHandoff _ -> false  (* Already prepared *)
  | Idle ->
    context_ratio >= config.prepare_threshold ||
    check_non_context_triggers ~config ~cell

(** Phase 2: Should we actually HANDOFF? *)
let should_handoff ~config ~cell ~context_ratio =
  (* Only handoff if prepared OR if we hit handoff threshold directly *)
  match cell.phase with
  | ReadyForHandoff _ -> context_ratio >= config.handoff_threshold
  | Idle ->
    (* Emergency: hit handoff threshold without prepare phase *)
    context_ratio >= config.handoff_threshold

(** Legacy: Check if any trigger condition is met (for backward compat) *)
let should_divide ~config ~cell ~context_ratio =
  should_handoff ~config ~cell ~context_ratio

(** Safe substring extraction - never throws, returns empty on invalid range *)
let safe_sub s start len =
  let s_len = String.length s in
  if start < 0 || len < 0 || start >= s_len then ""
  else
    let actual_len = min len (s_len - start) in
    if actual_len <= 0 then ""
    else String.sub s start actual_len

(** Compress context into DNA for transfer *)
let compress_to_dna ~ratio ~context =
  (* Simple compression: take first N% of context *)
  let len = String.length context in
  let target_len = int_of_float (float_of_int len *. ratio) in
  if target_len >= len then context
  else safe_sub context 0 target_len

(** Extract DNA from dying cell for child *)
let extract_dna ~config ~parent_cell ~full_context =
  let compressed = compress_to_dna ~ratio:config.dna_compression_ratio ~context:full_context in
  let header = Printf.sprintf
    "[Generation %d | Parent: %s | Tasks: %d | Born: %.0f]\n\n"
    parent_cell.generation
    parent_cell.id
    parent_cell.task_count
    parent_cell.born_at
  in
  header ^ compressed

(** Extract delta context (changes since DNA was prepared)
    Quality controls:
    - Skip if full_context < min_context_for_delta (short session)
    - Skip if delta < min_delta_len (noise threshold) *)
let extract_delta ~config ~full_context ~since_len =
  let current_len = String.length full_context in
  (* Short session exception: skip delta for very short sessions *)
  if current_len < config.min_context_for_delta then begin
    Printf.printf "[MITOSIS/DELTA] Short session (%d < %d chars), skipping delta\n%!"
      current_len config.min_context_for_delta;
    ""
  end
  else if since_len >= current_len then
    ""  (* No new content *)
  else
    let raw_delta = safe_sub full_context since_len (current_len - since_len) in
    let compressed = compress_to_dna ~ratio:config.dna_compression_ratio ~context:raw_delta in
    (* Quality threshold: skip if delta is too short (noise) *)
    if String.length compressed < config.min_delta_len then begin
      Printf.printf "[MITOSIS/DELTA] Delta too short (%d < %d chars), treating as noise\n%!"
        (String.length compressed) config.min_delta_len;
      ""
    end
    else
      compressed

(** String Set for O(log n) lookup instead of O(n) List.mem *)
module StringSet = Set.Make(String)

(** Simple line-based deduplication for merge - O(n log n) instead of O(n²) *)
let deduplicate_lines ~base ~delta =
  let base_lines = String.split_on_char '\n' base |> List.map String.trim in
  (* Build a Set for O(log n) membership test *)
  let base_set = List.fold_left (fun acc line ->
    if String.length line > 10 then (* Only track meaningful lines *)
      StringSet.add line acc
    else acc
  ) StringSet.empty base_lines in
  let delta_lines = String.split_on_char '\n' delta in
  let unique_lines = List.filter (fun line ->
    let trimmed = String.trim line in
    (* Keep if: short line OR not in base *)
    String.length trimmed <= 10 || not (StringSet.mem trimmed base_set)
  ) delta_lines in
  String.concat "\n" unique_lines

(** Merge prepared DNA with delta from 50%->80% window
    Enhanced strategy:
    - Skip if delta is empty
    - Deduplicate overlapping content
    - Add clear section marker *)
let merge_dna_with_delta ~prepared_dna ~delta =
  if String.length delta = 0 then
    prepared_dna
  else
    (* Deduplicate: remove lines from delta that already exist in prepared_dna *)
    let deduped_delta = deduplicate_lines ~base:prepared_dna ~delta in
    let deduped_len = String.length deduped_delta in
    let original_len = String.length delta in
    if deduped_len < original_len then
      Printf.printf "[MITOSIS/MERGE] Deduplication: %d → %d chars (-%d%% overlap)\n%!"
        original_len deduped_len ((original_len - deduped_len) * 100 / original_len);
    if String.length (String.trim deduped_delta) = 0 then
      prepared_dna  (* All delta was duplicate *)
    else
      Printf.sprintf "%s\n\n## Recent Updates (Delta)\n\n%s" prepared_dna deduped_delta

(** Phase 1: Prepare for division - extract DNA but don't handoff yet *)
let prepare_for_division ~config ~cell ~full_context =
  let dna = extract_dna ~config ~parent_cell:cell ~full_context in
  let context_len = String.length full_context in
  let prepared_cell = { cell with
    state = Prepared;
    phase = ReadyForHandoff dna;
    prepared_dna = Some dna;
    prepare_context_len = context_len;  (* Track for delta calculation *)
  } in
  Printf.printf "[MITOSIS/PREPARE] Cell %s (gen %d) prepared at %.0f%% - DNA extracted (%d chars), waiting for handoff threshold\n%!"
    cell.id cell.generation (config.prepare_threshold *. 100.0) context_len;
  prepared_cell

(** Activate a stem cell with DNA from parent *)
let activate_stem ~pool ~dna =
  match List.find_opt (fun c -> c.state = Stem) pool.cells with
  | None ->
    (* No stem cells available - create emergency cell *)
    let emergency = create_stem_cell ~generation:999 in
    let activated = { emergency with
      state = Active;
      phase = Idle;  (* Fresh start *)
      context_dna = Some dna;
      prepared_dna = None;
      last_activity = Unix.gettimeofday ();
    } in
    (activated, pool)
  | Some stem ->
    let activated = { stem with
      state = Active;
      phase = Idle;  (* Fresh start *)
      context_dna = Some dna;
      prepared_dna = None;
      last_activity = Unix.gettimeofday ();
    } in
    let remaining = List.filter (fun c -> c.id <> stem.id) pool.cells in
    (activated, { pool with cells = remaining })

(** Trigger apoptosis on a cell *)
let begin_apoptosis cell =
  { cell with state = Apoptotic }

(** Complete apoptosis - cell is now dead *)
let complete_apoptosis _cell =
  (* Return any final state/logs *)
  `Dead

(** Perform mitosis: parent divides into child *)
let perform_mitosis ~config ~pool ~parent ~full_context =
  (* 1. Build DNA: merge prepared DNA with delta if available, else extract fresh *)
  let dna = match parent.phase with
    | ReadyForHandoff prepared_dna ->
        (* Delta merge: prepared DNA (50%) + changes since then (50%->80%) *)
        let delta = extract_delta ~config ~full_context ~since_len:parent.prepare_context_len in
        let merged = merge_dna_with_delta ~prepared_dna ~delta in
        Printf.printf "[MITOSIS/DELTA] Merged DNA: prepared=%d chars + delta=%d chars\n%!"
          (String.length prepared_dna) (String.length delta);
        merged
    | Idle -> extract_dna ~config ~parent_cell:parent ~full_context
  in

  (* 2. Parent begins apoptosis *)
  let dying_parent = begin_apoptosis parent in

  (* 3. Activate a stem cell with the DNA *)
  let (child, new_pool) = activate_stem ~pool ~dna in
  let child = { child with generation = parent.generation + 1 } in

  (* 4. Replenish stem pool *)
  let new_stem = create_stem_cell ~generation:(child.generation + 1) in
  let replenished_pool = { new_pool with
    cells = new_stem :: new_pool.cells
  } in

  (child, dying_parent, replenished_pool, dna)

(** Build handoff prompt for the new cell *)
let build_mitosis_prompt ~child ~dna =
  Printf.sprintf
    "🧬 **MITOSIS HANDOFF** (Generation %d)\n\n\
     You are a new agent cell, born from division.\n\
     Your parent completed their lifecycle and passed on their DNA (compressed context).\n\n\
     **IMPORTANT**: Continue seamlessly. The user should not notice the transition.\n\n\
     ## Inherited DNA\n\n\
     %s\n\n\
     ---\n\
     **Instructions**:\n\
     1. Parse the inherited DNA above\n\
     2. Continue the current task\n\
     3. You will divide again when triggers are met\n\
     4. This is normal - embrace the lifecycle\n"
    child.generation
    dna

(** Execute the full mitosis cycle *)
let execute_mitosis ~config ~pool ~parent ~full_context ~spawn_fn =
  let (child, dying_parent, new_pool, dna) =
    perform_mitosis ~config ~pool ~parent ~full_context in

  (* Build the handoff prompt *)
  let prompt = build_mitosis_prompt ~child ~dna in

  (* Spawn the new agent *)
  let spawn_result = spawn_fn ~prompt in

  (* Complete apoptosis of parent *)
  let _ = complete_apoptosis dying_parent in

  Printf.printf "[MITOSIS] Cell %s (gen %d) → Cell %s (gen %d)\n%!"
    parent.id parent.generation child.id child.generation;

  (spawn_result, child, new_pool)

(** Update cell activity counters *)
let record_activity ~cell ~task_done ~tool_called =
  let task_count = if task_done then cell.task_count + 1 else cell.task_count in
  let tool_call_count = if tool_called then cell.tool_call_count + 1 else cell.tool_call_count in
  { cell with
    task_count;
    tool_call_count;
    last_activity = Unix.gettimeofday ();
  }

(** 2-Phase mitosis result *)
type mitosis_check_result =
  | NoAction                              (* Nothing to do *)
  | Prepared of cell                      (* Phase 1: Cell prepared, DNA extracted *)
  | Handoff of Spawn.spawn_result * cell * stem_pool  (* Phase 2: Handoff executed *)

(** Auto-mitosis check - 2-phase approach *)
let auto_mitosis_check_2phase ~config ~pool ~cell ~context_ratio ~full_context ~spawn_fn =
  (* Phase 2: Check if we should handoff (higher priority) *)
  if should_handoff ~config ~cell ~context_ratio then begin
    Printf.printf "[MITOSIS/HANDOFF] Threshold %.0f%% reached for cell %s (gen %d), executing handoff...\n%!"
      (config.handoff_threshold *. 100.0) cell.id cell.generation;
    let (spawn_result, child, new_pool) =
      execute_mitosis ~config ~pool ~parent:cell ~full_context ~spawn_fn in
    Handoff (spawn_result, child, new_pool)
  end
  (* Phase 1: Check if we should prepare *)
  else if should_prepare ~config ~cell ~context_ratio then begin
    let prepared_cell = prepare_for_division ~config ~cell ~full_context in
    Prepared prepared_cell
  end
  else
    NoAction

(** Legacy auto-mitosis check - for backward compat *)
let auto_mitosis_check ~config ~pool ~cell ~context_ratio ~full_context ~spawn_fn =
  if should_divide ~config ~cell ~context_ratio then begin
    Printf.printf "[MITOSIS/AUTO] Trigger met for cell %s (gen %d), dividing...\n%!"
      cell.id cell.generation;
    Some (execute_mitosis ~config ~pool ~parent:cell ~full_context ~spawn_fn)
  end
  else
    None

(** Cell state to string *)
let state_to_string = function
  | Stem -> "stem"
  | Active -> "active"
  | Prepared -> "prepared"
  | Dividing -> "dividing"
  | Apoptotic -> "apoptotic"

(** Mitosis phase to string *)
let phase_to_string = function
  | Idle -> "idle"
  | ReadyForHandoff _ -> "ready_for_handoff"

(** Cell to JSON *)
let cell_to_json cell =
  `Assoc [
    ("id", `String cell.id);
    ("generation", `Int cell.generation);
    ("state", `String (state_to_string cell.state));
    ("phase", `String (phase_to_string cell.phase));
    ("born_at", `Float cell.born_at);
    ("last_activity", `Float cell.last_activity);
    ("context_dna", match cell.context_dna with Some d -> `String d | None -> `Null);
    ("prepared_dna", match cell.prepared_dna with Some d -> `String d | None -> `Null);
    ("prepare_context_len", `Int cell.prepare_context_len);
    ("task_count", `Int cell.task_count);
    ("tool_call_count", `Int cell.tool_call_count);
  ]

(** Pool to JSON *)
let pool_to_json pool =
  `Assoc [
    ("cells", `List (List.map cell_to_json pool.cells));
    ("max_size", `Int pool.max_size);
    ("warm_up_count", `Int pool.warm_up_count);
    ("stem_count", `Int (List.length (List.filter (fun c -> c.state = Stem) pool.cells)));
  ]

(** Trigger to JSON *)
let trigger_to_json = function
  | Time_based f -> `Assoc [("type", `String "time_based"); ("interval_seconds", `Float f)]
  | Task_count n -> `Assoc [("type", `String "task_count"); ("count", `Int n)]
  | Tool_calls n -> `Assoc [("type", `String "tool_calls"); ("count", `Int n)]
  | Context_threshold f -> `Assoc [("type", `String "context_threshold"); ("threshold", `Float f)]
  | Complexity_spike -> `Assoc [("type", `String "complexity_spike")]

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("triggers", `List (List.map trigger_to_json config.triggers));
    ("stem_pool_size", `Int config.stem_pool_size);
    ("max_generation", `Int config.max_generation);
    ("dna_compression_ratio", `Float config.dna_compression_ratio);
    ("apoptosis_delay", `Float config.apoptosis_delay);
    (* 2-Phase thresholds *)
    ("prepare_threshold", `Float config.prepare_threshold);
    ("handoff_threshold", `Float config.handoff_threshold);
  ]

(** Write mitosis status to file for hook consumption.
    Hook reads this file to warn Claude about context pressure. *)
let write_status ~base_path ~cell ~config =
  let masc_dir = Filename.concat base_path ".masc" in
  let status_file = Filename.concat masc_dir "mitosis-status.json" in
  (* Rough estimate: assume 100 tool calls ≈ 80% context *)
  let tool_calls = cell.tool_call_count in
  let estimated_ratio = Float.min 1.0 (Float.of_int tool_calls /. 125.0) in
  let status =
    if estimated_ratio >= config.handoff_threshold then "critical"
    else if estimated_ratio >= config.prepare_threshold then "warning"
    else "healthy" in
  let json = `Assoc [
    ("tool_calls", `Int tool_calls);
    ("task_count", `Int cell.task_count);
    ("estimated_ratio", `Float estimated_ratio);
    ("status", `String status);
    ("generation", `Int cell.generation);
    ("phase", `String (phase_to_string cell.phase));
    ("prepare_threshold", `Float config.prepare_threshold);
    ("handoff_threshold", `Float config.handoff_threshold);
    ("updated_at", `Float (Unix.gettimeofday ()));
  ] in
  (* Ensure .masc directory exists *)
  if Sys.file_exists masc_dir && Sys.is_directory masc_dir then begin
    let oc = open_out status_file in
    output_string oc (Yojson.Safe.pretty_to_string json ^ "\n");
    close_out oc
  end

(** Write mitosis status to both local file AND backend (for cross-machine collaboration).
    - Local file: Hook reads this to warn Claude about context pressure
    - Backend (PostgreSQL): Other agents on different machines can see context pressure
    Key format: mitosis:{node_id} *)
let write_status_with_backend ~room_config ~cell ~config =
  let open Room_utils in
  let base_path = room_config.base_path in
  let node_id = room_config.backend_config.Backend.node_id in
  let cluster_name = room_config.backend_config.Backend.cluster_name in

  (* Calculate status *)
  let tool_calls = cell.tool_call_count in
  let estimated_ratio = Float.min 1.0 (Float.of_int tool_calls /. 125.0) in
  let status =
    if estimated_ratio >= config.handoff_threshold then "critical"
    else if estimated_ratio >= config.prepare_threshold then "warning"
    else "healthy" in

  (* JSON with node_id for multi-agent distinction *)
  let json = `Assoc [
    ("node_id", `String node_id);
    ("cluster_name", `String cluster_name);
    ("tool_calls", `Int tool_calls);
    ("task_count", `Int cell.task_count);
    ("estimated_ratio", `Float estimated_ratio);
    ("status", `String status);
    ("generation", `Int cell.generation);
    ("phase", `String (phase_to_string cell.phase));
    ("prepare_threshold", `Float config.prepare_threshold);
    ("handoff_threshold", `Float config.handoff_threshold);
    ("updated_at", `Float (Unix.gettimeofday ()));
  ] in
  let json_str = Yojson.Safe.to_string json in

  (* 1. Write to local file (for hook consumption) *)
  let masc_dir = Filename.concat base_path ".masc" in
  let status_file = Filename.concat masc_dir "mitosis-status.json" in
  if Sys.file_exists masc_dir && Sys.is_directory masc_dir then begin
    let oc = open_out status_file in
    output_string oc (Yojson.Safe.pretty_to_string json ^ "\n");
    close_out oc
  end;

  (* 2. Write to backend (for cross-machine collaboration) *)
  let key = Printf.sprintf "mitosis:%s" node_id in
  ignore (backend_set room_config ~key ~value:json_str)

(** Get all mitosis statuses from backend (for monitoring other agents) *)
let get_all_statuses ~room_config =
  let open Room_utils in
  match backend_get_all room_config ~prefix:"mitosis:" with
  | Ok pairs ->
      List.filter_map (fun (_key, value) ->
        try
          let json = Yojson.Safe.from_string value in
          let node_id = Yojson.Safe.Util.(json |> member "node_id" |> to_string) in
          let status = Yojson.Safe.Util.(json |> member "status" |> to_string) in
          let ratio = Yojson.Safe.Util.(json |> member "estimated_ratio" |> to_float) in
          Some (node_id, status, ratio)
        with Yojson.Safe.Util.Type_error _ -> None
      ) pairs
  | Error _ -> []
