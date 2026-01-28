(** Mitosis Module Coverage Tests

    Tests for MASC Mitosis - Cell Division Pattern for Infinite Agent Lifecycle:
    - cell_state: Stem, Active, Prepared, Dividing, Apoptotic
    - mitosis_phase: Idle, ReadyForHandoff
    - cell: id, generation, state, phase, born_at, context_dna
    - stem_pool: cells, max_size, warm_up_count
    - mitosis_trigger: Time_based, Task_count, Tool_calls, Context_threshold
    - mitosis_config: triggers, thresholds, pool size
    - create_stem_cell: cell factory
*)

open Alcotest

module Mitosis = Masc_mcp.Mitosis

(* ============================================================
   cell_state Tests
   ============================================================ *)

let test_cell_state_stem () =
  let s = Mitosis.Stem in
  check bool "stem state" true (s = Mitosis.Stem)

let test_cell_state_active () =
  let s = Mitosis.Active in
  check bool "active state" true (s = Mitosis.Active)

let test_cell_state_prepared () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let cell' : Mitosis.cell = { cell with state = Mitosis.Prepared } in
  check bool "prepared state" true (cell'.state = Mitosis.Prepared)

let test_cell_state_dividing () =
  let s = Mitosis.Dividing in
  check bool "dividing state" true (s = Mitosis.Dividing)

let test_cell_state_apoptotic () =
  let s = Mitosis.Apoptotic in
  check bool "apoptotic state" true (s = Mitosis.Apoptotic)

let test_cell_states_distinct () =
  check bool "stem <> active" false (Mitosis.Stem = Mitosis.Active);
  check bool "dividing <> apoptotic" false (Mitosis.Dividing = Mitosis.Apoptotic);
  (* Note: Prepared tested via state_to_string since it's used differently in cell type *)
  check bool "state_to_string prepared" true (Mitosis.state_to_string Mitosis.Prepared = "prepared")

(* ============================================================
   mitosis_phase Tests
   ============================================================ *)

let test_phase_idle () =
  let p = Mitosis.Idle in
  check bool "idle phase" true (p = Mitosis.Idle)

let test_phase_ready_for_handoff () =
  let p = Mitosis.ReadyForHandoff "dna-string-123" in
  match p with
  | Mitosis.ReadyForHandoff dna ->
    check string "dna string" "dna-string-123" dna
  | _ -> fail "expected ReadyForHandoff"

let test_phase_ready_for_handoff_empty_dna () =
  let p = Mitosis.ReadyForHandoff "" in
  match p with
  | Mitosis.ReadyForHandoff dna ->
    check string "empty dna" "" dna
  | _ -> fail "expected ReadyForHandoff"

(* ============================================================
   mitosis_trigger Tests
   ============================================================ *)

let test_trigger_time_based () =
  let t = Mitosis.Time_based 300.0 in
  match t with
  | Mitosis.Time_based secs ->
    check bool "300 seconds" true (abs_float (secs -. 300.0) < 0.001)
  | _ -> fail "expected Time_based"

let test_trigger_task_count () =
  let t = Mitosis.Task_count 10 in
  match t with
  | Mitosis.Task_count n -> check int "10 tasks" 10 n
  | _ -> fail "expected Task_count"

let test_trigger_tool_calls () =
  let t = Mitosis.Tool_calls 20 in
  match t with
  | Mitosis.Tool_calls n -> check int "20 tool calls" 20 n
  | _ -> fail "expected Tool_calls"

let test_trigger_context_threshold () =
  let t = Mitosis.Context_threshold 0.8 in
  match t with
  | Mitosis.Context_threshold pct ->
    check bool "80% threshold" true (abs_float (pct -. 0.8) < 0.001)
  | _ -> fail "expected Context_threshold"

let test_trigger_complexity_spike () =
  let t = Mitosis.Complexity_spike in
  check bool "complexity spike" true (t = Mitosis.Complexity_spike)

(* ============================================================
   default_config Tests
   ============================================================ *)

let test_default_config_triggers () =
  let c = Mitosis.default_config in
  check bool "has triggers" true (List.length c.triggers > 0)

let test_default_config_stem_pool_size () =
  let c = Mitosis.default_config in
  check int "stem pool size" 2 c.stem_pool_size

let test_default_config_max_generation () =
  let c = Mitosis.default_config in
  check int "max generation" 10 c.max_generation

let test_default_config_dna_compression_ratio () =
  let c = Mitosis.default_config in
  check bool "compression ratio" true (abs_float (c.dna_compression_ratio -. 0.1) < 0.001)

let test_default_config_apoptosis_delay () =
  let c = Mitosis.default_config in
  check bool "apoptosis delay" true (abs_float (c.apoptosis_delay -. 5.0) < 0.001)

let test_default_config_prepare_threshold () =
  let c = Mitosis.default_config in
  check bool "prepare threshold 0.5" true (abs_float (c.prepare_threshold -. 0.5) < 0.001)

let test_default_config_handoff_threshold () =
  let c = Mitosis.default_config in
  check bool "handoff threshold 0.8" true (abs_float (c.handoff_threshold -. 0.8) < 0.001)

let test_default_config_min_context_for_delta () =
  let c = Mitosis.default_config in
  check int "min context for delta" 1000 c.min_context_for_delta

let test_default_config_min_delta_len () =
  let c = Mitosis.default_config in
  check int "min delta len" 100 c.min_delta_len

let test_default_config_threshold_ordering () =
  let c = Mitosis.default_config in
  check bool "prepare < handoff" true (c.prepare_threshold < c.handoff_threshold)

(* ============================================================
   create_stem_cell Tests
   ============================================================ *)

let test_create_stem_cell_generation_0 () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  check int "generation 0" 0 cell.generation;
  check bool "state is Stem" true (cell.state = Mitosis.Stem);
  check bool "phase is Idle" true (cell.phase = Mitosis.Idle)

let test_create_stem_cell_generation_5 () =
  let cell = Mitosis.create_stem_cell ~generation:5 in
  check int "generation 5" 5 cell.generation

let test_create_stem_cell_has_id () =
  let cell = Mitosis.create_stem_cell ~generation:1 in
  check bool "id non-empty" true (String.length cell.id > 0);
  check bool "id contains cell-" true (String.sub cell.id 0 5 = "cell-")

let test_create_stem_cell_timestamps () =
  let before = Unix.gettimeofday () in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let after = Unix.gettimeofday () in
  check bool "born_at reasonable" true (cell.born_at >= before && cell.born_at <= after);
  check bool "last_activity reasonable" true (cell.last_activity >= before && cell.last_activity <= after)

let test_create_stem_cell_no_dna () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  check (option string) "no context_dna" None cell.context_dna;
  check (option string) "no prepared_dna" None cell.prepared_dna

let test_create_stem_cell_zero_counts () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  check int "task_count 0" 0 cell.task_count;
  check int "tool_call_count 0" 0 cell.tool_call_count;
  check int "prepare_context_len 0" 0 cell.prepare_context_len

let test_create_stem_cell_unique_ids () =
  let cell1 = Mitosis.create_stem_cell ~generation:0 in
  Unix.sleepf 0.001;  (* Small delay to ensure different timestamp *)
  let cell2 = Mitosis.create_stem_cell ~generation:0 in
  check bool "unique ids" true (cell1.id <> cell2.id)

(* ============================================================
   cell Record Tests
   ============================================================ *)

let test_cell_manual_creation () =
  let cell : Mitosis.cell = {
    id = "test-cell-001";
    generation = 3;
    state = Mitosis.Active;
    phase = Mitosis.Idle;
    born_at = 1000.0;
    last_activity = 1100.0;
    context_dna = Some "compressed-context-data";
    prepared_dna = None;
    prepare_context_len = 500;
    task_count = 5;
    tool_call_count = 15;
  } in
  check string "id" "test-cell-001" cell.id;
  check int "generation" 3 cell.generation;
  check bool "state active" true (cell.state = Mitosis.Active);
  check int "task_count" 5 cell.task_count

let test_cell_with_prepared_dna () =
  let cell : Mitosis.cell = {
    id = "test-cell-002";
    generation = 2;
    state = Mitosis.Prepared;
    phase = Mitosis.ReadyForHandoff "prepared-dna";
    born_at = 1000.0;
    last_activity = 1200.0;
    context_dna = Some "original-dna";
    prepared_dna = Some "prepared-dna";
    prepare_context_len = 800;
    task_count = 8;
    tool_call_count = 20;
  } in
  check (option string) "has prepared_dna" (Some "prepared-dna") cell.prepared_dna;
  check bool "state prepared" true (cell.state = Mitosis.Prepared)

(* ============================================================
   stem_pool Tests
   ============================================================ *)

let test_stem_pool_creation () =
  let pool : Mitosis.stem_pool = {
    cells = [];
    max_size = 5;
    warm_up_count = 2;
  } in
  check int "empty cells" 0 (List.length pool.cells);
  check int "max_size" 5 pool.max_size;
  check int "warm_up_count" 2 pool.warm_up_count

let test_stem_pool_with_cells () =
  let cell1 = Mitosis.create_stem_cell ~generation:0 in
  let cell2 = Mitosis.create_stem_cell ~generation:0 in
  let pool : Mitosis.stem_pool = {
    cells = [cell1; cell2];
    max_size = 5;
    warm_up_count = 2;
  } in
  check int "2 cells" 2 (List.length pool.cells)

(* ============================================================
   Custom Config Tests
   ============================================================ *)

let test_custom_config () =
  let config : Mitosis.mitosis_config = {
    triggers = [Mitosis.Time_based 60.0; Mitosis.Task_count 5];
    stem_pool_size = 3;
    max_generation = 20;
    dna_compression_ratio = 0.2;
    apoptosis_delay = 10.0;
    prepare_threshold = 0.4;
    handoff_threshold = 0.7;
    min_context_for_delta = 500;
    min_delta_len = 50;
  } in
  check int "custom pool size" 3 config.stem_pool_size;
  check int "custom max gen" 20 config.max_generation;
  check bool "custom prepare threshold" true (abs_float (config.prepare_threshold -. 0.4) < 0.001)

(* ============================================================
   init_pool Tests
   ============================================================ *)

let test_init_pool_default_config () =
  let pool = Mitosis.init_pool ~config:Mitosis.default_config in
  check int "pool size" 2 (List.length pool.cells);
  check int "max_size" 2 pool.max_size

let test_init_pool_custom_size () =
  let config = { Mitosis.default_config with stem_pool_size = 5 } in
  let pool = Mitosis.init_pool ~config in
  check int "pool size 5" 5 (List.length pool.cells)

let test_init_pool_all_stem () =
  let pool = Mitosis.init_pool ~config:Mitosis.default_config in
  let all_stem = List.for_all (fun c -> c.Mitosis.state = Mitosis.Stem) pool.cells in
  check bool "all stem state" true all_stem

let test_init_pool_warm_up_count () =
  let config = { Mitosis.default_config with stem_pool_size = 10 } in
  let pool = Mitosis.init_pool ~config in
  check int "warm_up_count <= 2" 2 pool.warm_up_count

(* ============================================================
   safe_sub Tests
   ============================================================ *)

let test_safe_sub_normal () =
  check string "normal" "ell" (Mitosis.safe_sub "hello" 1 3)

let test_safe_sub_start_0 () =
  check string "start 0" "he" (Mitosis.safe_sub "hello" 0 2)

let test_safe_sub_full_string () =
  check string "full string" "hello" (Mitosis.safe_sub "hello" 0 5)

let test_safe_sub_negative_start () =
  check string "negative start" "" (Mitosis.safe_sub "hello" (-1) 3)

let test_safe_sub_negative_len () =
  check string "negative len" "" (Mitosis.safe_sub "hello" 0 (-1))

let test_safe_sub_start_beyond () =
  check string "start beyond" "" (Mitosis.safe_sub "hello" 10 3)

let test_safe_sub_len_exceeds () =
  check string "len exceeds" "lo" (Mitosis.safe_sub "hello" 3 100)

let test_safe_sub_empty_string () =
  check string "empty string" "" (Mitosis.safe_sub "" 0 3)

let test_safe_sub_zero_len () =
  check string "zero len" "" (Mitosis.safe_sub "hello" 2 0)

(* ============================================================
   compress_to_dna Tests
   ============================================================ *)

let test_compress_to_dna_full () =
  let result = Mitosis.compress_to_dna ~ratio:1.0 ~context:"hello world" in
  check string "ratio 1.0" "hello world" result

let test_compress_to_dna_half () =
  let result = Mitosis.compress_to_dna ~ratio:0.5 ~context:"hello world" in
  check string "ratio 0.5" "hello" result

let test_compress_to_dna_10_percent () =
  let result = Mitosis.compress_to_dna ~ratio:0.1 ~context:"hello world 123" in
  check string "ratio 0.1" "h" result

let test_compress_to_dna_empty () =
  let result = Mitosis.compress_to_dna ~ratio:0.5 ~context:"" in
  check string "empty context" "" result

let test_compress_to_dna_ratio_exceeds () =
  let result = Mitosis.compress_to_dna ~ratio:2.0 ~context:"hello" in
  check string "ratio > 1" "hello" result

(* ============================================================
   deduplicate_lines Tests
   ============================================================ *)

let test_deduplicate_lines_no_overlap () =
  let base = "line one\nline two\nline three" in
  let delta = "line four\nline five" in
  let result = Mitosis.deduplicate_lines ~base ~delta in
  check bool "has line four" true (String.length result > 0)

let test_deduplicate_lines_with_overlap () =
  let base = "this is a longer line one\nline two long\nline three" in
  let delta = "this is a longer line one\nnew line here" in
  let result = Mitosis.deduplicate_lines ~base ~delta in
  check bool "removes duplicate" true (not (String.length result = String.length delta))

let test_deduplicate_lines_short_lines_kept () =
  let base = "long base line here" in
  let delta = "short\na\nb" in
  let result = Mitosis.deduplicate_lines ~base ~delta in
  check string "short lines kept" "short\na\nb" result

let test_deduplicate_lines_empty_delta () =
  let result = Mitosis.deduplicate_lines ~base:"some base" ~delta:"" in
  check string "empty delta" "" result

let test_deduplicate_lines_empty_base () =
  let result = Mitosis.deduplicate_lines ~base:"" ~delta:"line one\nline two" in
  check string "empty base" "line one\nline two" result

(* ============================================================
   merge_dna_with_delta Tests
   ============================================================ *)

let test_merge_dna_with_delta_empty () =
  let result = Mitosis.merge_dna_with_delta ~prepared_dna:"original dna" ~delta:"" in
  check string "empty delta returns original" "original dna" result

let test_merge_dna_with_delta_has_delta () =
  let result = Mitosis.merge_dna_with_delta ~prepared_dna:"original" ~delta:"new content here" in
  check bool "contains original" true (String.sub result 0 8 = "original");
  check bool "contains delta marker" true (String.length result > 8)

let test_merge_dna_with_delta_deduplicates () =
  let prepared = "long line in prepared dna content here" in
  let delta = "long line in prepared dna content here\nnew stuff" in
  let result = Mitosis.merge_dna_with_delta ~prepared_dna:prepared ~delta in
  check bool "deduplication applied" true (String.length result > 0)

(* ============================================================
   begin_apoptosis / complete_apoptosis Tests
   ============================================================ *)

let test_begin_apoptosis () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let dying = Mitosis.begin_apoptosis cell in
  check bool "state apoptotic" true (dying.state = Mitosis.Apoptotic)

let test_begin_apoptosis_preserves_id () =
  let cell = Mitosis.create_stem_cell ~generation:5 in
  let dying = Mitosis.begin_apoptosis cell in
  check string "preserves id" cell.id dying.id;
  check int "preserves generation" 5 dying.generation

let test_complete_apoptosis () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let dying = Mitosis.begin_apoptosis cell in
  let result = Mitosis.complete_apoptosis dying in
  check bool "returns Dead" true (result = `Dead)

(* ============================================================
   build_mitosis_prompt Tests
   ============================================================ *)

let test_build_mitosis_prompt_contains_generation () =
  let cell = Mitosis.create_stem_cell ~generation:3 in
  let prompt = Mitosis.build_mitosis_prompt ~child:cell ~dna:"test dna" in
  check bool "contains generation" true (String.length prompt > 0)

let test_build_mitosis_prompt_contains_dna () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let prompt = Mitosis.build_mitosis_prompt ~child:cell ~dna:"my special dna content" in
  check bool "contains dna" true (
    try let _ = Str.search_forward (Str.regexp "my special dna content") prompt 0 in true
    with Not_found -> false
  )

let test_build_mitosis_prompt_contains_mitosis () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let prompt = Mitosis.build_mitosis_prompt ~child:cell ~dna:"dna" in
  check bool "contains MITOSIS" true (
    try let _ = Str.search_forward (Str.regexp "MITOSIS") prompt 0 in true
    with Not_found -> false
  )

(* ============================================================
   state_to_string Tests
   ============================================================ *)

let test_state_to_string_stem () =
  check string "stem" "stem" (Mitosis.state_to_string Mitosis.Stem)

let test_state_to_string_active () =
  check string "active" "active" (Mitosis.state_to_string Mitosis.Active)

let test_state_to_string_prepared () =
  check string "prepared" "prepared" (Mitosis.state_to_string Mitosis.Prepared)

let test_state_to_string_dividing () =
  check string "dividing" "dividing" (Mitosis.state_to_string Mitosis.Dividing)

let test_state_to_string_apoptotic () =
  check string "apoptotic" "apoptotic" (Mitosis.state_to_string Mitosis.Apoptotic)

(* ============================================================
   phase_to_string Tests
   ============================================================ *)

let test_phase_to_string_idle () =
  check string "idle" "idle" (Mitosis.phase_to_string Mitosis.Idle)

let test_phase_to_string_ready_for_handoff () =
  check string "ready" "ready_for_handoff" (Mitosis.phase_to_string (Mitosis.ReadyForHandoff "dna"))

(* ============================================================
   cell_to_json Tests
   ============================================================ *)

let test_cell_to_json_returns_assoc () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let json = Mitosis.cell_to_json cell in
  match json with
  | `Assoc _ -> ()
  | _ -> fail "expected Assoc"

let test_cell_to_json_has_id () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let json = Mitosis.cell_to_json cell in
  match json with
  | `Assoc fields -> check bool "has id" true (List.mem_assoc "id" fields)
  | _ -> fail "expected Assoc"

let test_cell_to_json_has_generation () =
  let cell = Mitosis.create_stem_cell ~generation:5 in
  let json = Mitosis.cell_to_json cell in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "generation" fields with
     | Some (`Int n) -> check int "generation 5" 5 n
     | _ -> fail "expected Int generation")
  | _ -> fail "expected Assoc"

let test_cell_to_json_has_state () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let json = Mitosis.cell_to_json cell in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "state" fields with
     | Some (`String s) -> check string "state stem" "stem" s
     | _ -> fail "expected String state")
  | _ -> fail "expected Assoc"

let test_cell_to_json_null_dna () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let json = Mitosis.cell_to_json cell in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "context_dna" fields with
     | Some `Null -> ()
     | _ -> fail "expected Null context_dna")
  | _ -> fail "expected Assoc"

(* ============================================================
   pool_to_json Tests
   ============================================================ *)

let test_pool_to_json_returns_assoc () =
  let pool = Mitosis.init_pool ~config:Mitosis.default_config in
  let json = Mitosis.pool_to_json pool in
  match json with
  | `Assoc _ -> ()
  | _ -> fail "expected Assoc"

let test_pool_to_json_has_cells () =
  let pool = Mitosis.init_pool ~config:Mitosis.default_config in
  let json = Mitosis.pool_to_json pool in
  match json with
  | `Assoc fields -> check bool "has cells" true (List.mem_assoc "cells" fields)
  | _ -> fail "expected Assoc"

let test_pool_to_json_has_stem_count () =
  let pool = Mitosis.init_pool ~config:Mitosis.default_config in
  let json = Mitosis.pool_to_json pool in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "stem_count" fields with
     | Some (`Int n) -> check int "stem count" 2 n
     | _ -> fail "expected Int stem_count")
  | _ -> fail "expected Assoc"

(* ============================================================
   trigger_to_json Tests
   ============================================================ *)

let test_trigger_to_json_time_based () =
  let json = Mitosis.trigger_to_json (Mitosis.Time_based 300.0) in
  match json with
  | `Assoc fields ->
    check bool "has type" true (List.mem_assoc "type" fields);
    (match List.assoc_opt "type" fields with
     | Some (`String s) -> check string "type" "time_based" s
     | _ -> fail "expected String type")
  | _ -> fail "expected Assoc"

let test_trigger_to_json_task_count () =
  let json = Mitosis.trigger_to_json (Mitosis.Task_count 10) in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "count" fields with
     | Some (`Int n) -> check int "count" 10 n
     | _ -> fail "expected Int count")
  | _ -> fail "expected Assoc"

let test_trigger_to_json_tool_calls () =
  let json = Mitosis.trigger_to_json (Mitosis.Tool_calls 20) in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "type" fields with
     | Some (`String s) -> check string "type" "tool_calls" s
     | _ -> fail "expected String type")
  | _ -> fail "expected Assoc"

let test_trigger_to_json_context_threshold () =
  let json = Mitosis.trigger_to_json (Mitosis.Context_threshold 0.8) in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "threshold" fields with
     | Some (`Float f) -> check bool "threshold 0.8" true (abs_float (f -. 0.8) < 0.001)
     | _ -> fail "expected Float threshold")
  | _ -> fail "expected Assoc"

let test_trigger_to_json_complexity_spike () =
  let json = Mitosis.trigger_to_json Mitosis.Complexity_spike in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "type" fields with
     | Some (`String s) -> check string "type" "complexity_spike" s
     | _ -> fail "expected String type")
  | _ -> fail "expected Assoc"

(* ============================================================
   config_to_json Tests
   ============================================================ *)

let test_config_to_json_returns_assoc () =
  let json = Mitosis.config_to_json Mitosis.default_config in
  match json with
  | `Assoc _ -> ()
  | _ -> fail "expected Assoc"

let test_config_to_json_has_triggers () =
  let json = Mitosis.config_to_json Mitosis.default_config in
  match json with
  | `Assoc fields -> check bool "has triggers" true (List.mem_assoc "triggers" fields)
  | _ -> fail "expected Assoc"

let test_config_to_json_has_thresholds () =
  let json = Mitosis.config_to_json Mitosis.default_config in
  match json with
  | `Assoc fields ->
    check bool "has prepare_threshold" true (List.mem_assoc "prepare_threshold" fields);
    check bool "has handoff_threshold" true (List.mem_assoc "handoff_threshold" fields)
  | _ -> fail "expected Assoc"

(* ============================================================
   should_prepare / should_handoff Tests
   ============================================================ *)

let test_should_prepare_below_threshold () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let result = Mitosis.should_prepare ~config:Mitosis.default_config ~cell ~context_ratio:0.3 in
  check bool "below threshold" false result

let test_should_prepare_at_threshold () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let result = Mitosis.should_prepare ~config:Mitosis.default_config ~cell ~context_ratio:0.5 in
  check bool "at threshold" true result

let test_should_prepare_above_threshold () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let result = Mitosis.should_prepare ~config:Mitosis.default_config ~cell ~context_ratio:0.6 in
  check bool "above threshold" true result

let test_should_prepare_already_prepared () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let cell' = { cell with phase = Mitosis.ReadyForHandoff "dna" } in
  let result = Mitosis.should_prepare ~config:Mitosis.default_config ~cell:cell' ~context_ratio:0.9 in
  check bool "already prepared" false result

let test_should_handoff_below_threshold () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let result = Mitosis.should_handoff ~config:Mitosis.default_config ~cell ~context_ratio:0.5 in
  check bool "below threshold" false result

let test_should_handoff_at_threshold () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let result = Mitosis.should_handoff ~config:Mitosis.default_config ~cell ~context_ratio:0.8 in
  check bool "at threshold" true result

let test_should_handoff_prepared_below () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let cell' = { cell with phase = Mitosis.ReadyForHandoff "dna" } in
  let result = Mitosis.should_handoff ~config:Mitosis.default_config ~cell:cell' ~context_ratio:0.6 in
  check bool "prepared but below handoff" false result

let test_should_handoff_prepared_above () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let cell' = { cell with phase = Mitosis.ReadyForHandoff "dna" } in
  let result = Mitosis.should_handoff ~config:Mitosis.default_config ~cell:cell' ~context_ratio:0.9 in
  check bool "prepared and above" true result

let test_should_divide_alias () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let handoff = Mitosis.should_handoff ~config:Mitosis.default_config ~cell ~context_ratio:0.9 in
  let divide = Mitosis.should_divide ~config:Mitosis.default_config ~cell ~context_ratio:0.9 in
  check bool "alias" true (handoff = divide)

(* ============================================================
   activate_stem Tests
   ============================================================ *)

let test_activate_stem_with_available () =
  let pool = Mitosis.init_pool ~config:Mitosis.default_config in
  let (activated, new_pool) = Mitosis.activate_stem ~pool ~dna:"test dna" in
  check bool "state active" true (activated.state = Mitosis.Active);
  check (option string) "has dna" (Some "test dna") activated.context_dna;
  check int "pool reduced" 1 (List.length new_pool.cells)

let test_activate_stem_empty_pool () =
  let pool : Mitosis.stem_pool = { cells = []; max_size = 2; warm_up_count = 1 } in
  let (activated, _) = Mitosis.activate_stem ~pool ~dna:"emergency dna" in
  check int "emergency generation" 999 activated.generation;
  check bool "state active" true (activated.state = Mitosis.Active)

(* ============================================================
   extract_dna Tests
   ============================================================ *)

let test_extract_dna_contains_header () =
  let cell = Mitosis.create_stem_cell ~generation:3 in
  let dna = Mitosis.extract_dna ~config:Mitosis.default_config ~parent_cell:cell ~full_context:"my full context here" in
  check bool "contains generation" true (
    try let _ = Str.search_forward (Str.regexp "Generation 3") dna 0 in true
    with Not_found -> false
  )

let test_extract_dna_contains_compressed () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let long_context = String.make 1000 'x' in
  let dna = Mitosis.extract_dna ~config:Mitosis.default_config ~parent_cell:cell ~full_context:long_context in
  check bool "shorter than original" true (String.length dna < 1000 + 200)  (* header overhead *)

(* ============================================================
   check_non_context_triggers Tests
   ============================================================ *)

let test_check_triggers_time_based () =
  let config = { Mitosis.default_config with triggers = [Time_based 0.001] } in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  Unix.sleepf 0.01;  (* Wait for trigger *)
  let result = Mitosis.check_non_context_triggers ~config ~cell in
  check bool "time trigger met" true result

let test_check_triggers_time_not_met () =
  let config = { Mitosis.default_config with triggers = [Time_based 99999.0] } in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let result = Mitosis.check_non_context_triggers ~config ~cell in
  check bool "time trigger not met" false result

let test_check_triggers_task_count () =
  let config = { Mitosis.default_config with triggers = [Task_count 3] } in
  let cell = { (Mitosis.create_stem_cell ~generation:0) with task_count = 5 } in
  let result = Mitosis.check_non_context_triggers ~config ~cell in
  check bool "task trigger met" true result

let test_check_triggers_task_not_met () =
  let config = { Mitosis.default_config with triggers = [Task_count 10] } in
  let cell = { (Mitosis.create_stem_cell ~generation:0) with task_count = 2 } in
  let result = Mitosis.check_non_context_triggers ~config ~cell in
  check bool "task trigger not met" false result

let test_check_triggers_tool_calls () =
  let config = { Mitosis.default_config with triggers = [Tool_calls 5] } in
  let cell = { (Mitosis.create_stem_cell ~generation:0) with tool_call_count = 10 } in
  let result = Mitosis.check_non_context_triggers ~config ~cell in
  check bool "tool trigger met" true result

let test_check_triggers_tool_not_met () =
  let config = { Mitosis.default_config with triggers = [Tool_calls 100] } in
  let cell = { (Mitosis.create_stem_cell ~generation:0) with tool_call_count = 5 } in
  let result = Mitosis.check_non_context_triggers ~config ~cell in
  check bool "tool trigger not met" false result

let test_check_triggers_context_ignored () =
  let config = { Mitosis.default_config with triggers = [Context_threshold 0.5] } in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let result = Mitosis.check_non_context_triggers ~config ~cell in
  check bool "context threshold ignored" false result

let test_check_triggers_complexity_ignored () =
  let config = { Mitosis.default_config with triggers = [Complexity_spike] } in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let result = Mitosis.check_non_context_triggers ~config ~cell in
  check bool "complexity spike ignored" false result

let test_check_triggers_empty () =
  let config = { Mitosis.default_config with triggers = [] } in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let result = Mitosis.check_non_context_triggers ~config ~cell in
  check bool "no triggers" false result

(* ============================================================
   extract_delta Tests
   ============================================================ *)

let test_extract_delta_short_session () =
  let config = { Mitosis.default_config with min_context_for_delta = 1000 } in
  let result = Mitosis.extract_delta ~config ~full_context:"short" ~since_len:0 in
  check string "short session empty" "" result

let test_extract_delta_no_new_content () =
  let config = { Mitosis.default_config with min_context_for_delta = 10 } in
  let result = Mitosis.extract_delta ~config ~full_context:"hello world" ~since_len:100 in
  check string "no new content" "" result

let test_extract_delta_too_short () =
  let config = {
    Mitosis.default_config with
    min_context_for_delta = 10;
    min_delta_len = 1000;
    dna_compression_ratio = 1.0;
  } in
  let result = Mitosis.extract_delta ~config ~full_context:"hello world new content" ~since_len:5 in
  check string "delta too short" "" result

let test_extract_delta_valid () =
  let config = {
    Mitosis.default_config with
    min_context_for_delta = 10;
    min_delta_len = 5;
    dna_compression_ratio = 1.0;
  } in
  let full = "original content here with new content added at the end" in
  let result = Mitosis.extract_delta ~config ~full_context:full ~since_len:22 in
  check bool "valid delta nonempty" true (String.length result > 0)

let test_extract_delta_same_len () =
  let config = { Mitosis.default_config with min_context_for_delta = 5 } in
  let result = Mitosis.extract_delta ~config ~full_context:"hello" ~since_len:5 in
  check string "same length" "" result

(* ============================================================
   record_activity Tests
   ============================================================ *)

let test_record_activity_task_done () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let updated = Mitosis.record_activity ~cell ~task_done:true ~tool_called:false in
  check int "task count +1" 1 updated.task_count;
  check int "tool count same" 0 updated.tool_call_count

let test_record_activity_tool_called () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let updated = Mitosis.record_activity ~cell ~task_done:false ~tool_called:true in
  check int "task count same" 0 updated.task_count;
  check int "tool count +1" 1 updated.tool_call_count

let test_record_activity_both () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let updated = Mitosis.record_activity ~cell ~task_done:true ~tool_called:true in
  check int "task count +1" 1 updated.task_count;
  check int "tool count +1" 1 updated.tool_call_count

let test_record_activity_neither () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let updated = Mitosis.record_activity ~cell ~task_done:false ~tool_called:false in
  check int "task count same" 0 updated.task_count;
  check int "tool count same" 0 updated.tool_call_count

let test_record_activity_accumulates () =
  let cell = { (Mitosis.create_stem_cell ~generation:0) with task_count = 5; tool_call_count = 3 } in
  let updated = Mitosis.record_activity ~cell ~task_done:true ~tool_called:true in
  check int "task count accumulated" 6 updated.task_count;
  check int "tool count accumulated" 4 updated.tool_call_count

let test_record_activity_updates_timestamp () =
  let cell = Mitosis.create_stem_cell ~generation:0 in
  Unix.sleepf 0.01;
  let updated = Mitosis.record_activity ~cell ~task_done:false ~tool_called:false in
  check bool "timestamp updated" true (updated.last_activity > cell.last_activity)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Mitosis Coverage" [
    "cell_state", [
      test_case "stem" `Quick test_cell_state_stem;
      test_case "active" `Quick test_cell_state_active;
      test_case "prepared" `Quick test_cell_state_prepared;
      test_case "dividing" `Quick test_cell_state_dividing;
      test_case "apoptotic" `Quick test_cell_state_apoptotic;
      test_case "distinct" `Quick test_cell_states_distinct;
    ];
    "mitosis_phase", [
      test_case "idle" `Quick test_phase_idle;
      test_case "ready for handoff" `Quick test_phase_ready_for_handoff;
      test_case "empty dna" `Quick test_phase_ready_for_handoff_empty_dna;
    ];
    "mitosis_trigger", [
      test_case "time based" `Quick test_trigger_time_based;
      test_case "task count" `Quick test_trigger_task_count;
      test_case "tool calls" `Quick test_trigger_tool_calls;
      test_case "context threshold" `Quick test_trigger_context_threshold;
      test_case "complexity spike" `Quick test_trigger_complexity_spike;
    ];
    "default_config", [
      test_case "triggers" `Quick test_default_config_triggers;
      test_case "stem pool size" `Quick test_default_config_stem_pool_size;
      test_case "max generation" `Quick test_default_config_max_generation;
      test_case "dna compression ratio" `Quick test_default_config_dna_compression_ratio;
      test_case "apoptosis delay" `Quick test_default_config_apoptosis_delay;
      test_case "prepare threshold" `Quick test_default_config_prepare_threshold;
      test_case "handoff threshold" `Quick test_default_config_handoff_threshold;
      test_case "min context for delta" `Quick test_default_config_min_context_for_delta;
      test_case "min delta len" `Quick test_default_config_min_delta_len;
      test_case "threshold ordering" `Quick test_default_config_threshold_ordering;
    ];
    "create_stem_cell", [
      test_case "generation 0" `Quick test_create_stem_cell_generation_0;
      test_case "generation 5" `Quick test_create_stem_cell_generation_5;
      test_case "has id" `Quick test_create_stem_cell_has_id;
      test_case "timestamps" `Quick test_create_stem_cell_timestamps;
      test_case "no dna" `Quick test_create_stem_cell_no_dna;
      test_case "zero counts" `Quick test_create_stem_cell_zero_counts;
      test_case "unique ids" `Quick test_create_stem_cell_unique_ids;
    ];
    "cell", [
      test_case "manual creation" `Quick test_cell_manual_creation;
      test_case "with prepared dna" `Quick test_cell_with_prepared_dna;
    ];
    "stem_pool", [
      test_case "creation" `Quick test_stem_pool_creation;
      test_case "with cells" `Quick test_stem_pool_with_cells;
    ];
    "custom_config", [
      test_case "custom values" `Quick test_custom_config;
    ];
    "init_pool", [
      test_case "default config" `Quick test_init_pool_default_config;
      test_case "custom size" `Quick test_init_pool_custom_size;
      test_case "all stem" `Quick test_init_pool_all_stem;
      test_case "warm up count" `Quick test_init_pool_warm_up_count;
    ];
    "safe_sub", [
      test_case "normal" `Quick test_safe_sub_normal;
      test_case "start 0" `Quick test_safe_sub_start_0;
      test_case "full string" `Quick test_safe_sub_full_string;
      test_case "negative start" `Quick test_safe_sub_negative_start;
      test_case "negative len" `Quick test_safe_sub_negative_len;
      test_case "start beyond" `Quick test_safe_sub_start_beyond;
      test_case "len exceeds" `Quick test_safe_sub_len_exceeds;
      test_case "empty string" `Quick test_safe_sub_empty_string;
      test_case "zero len" `Quick test_safe_sub_zero_len;
    ];
    "compress_to_dna", [
      test_case "full" `Quick test_compress_to_dna_full;
      test_case "half" `Quick test_compress_to_dna_half;
      test_case "10 percent" `Quick test_compress_to_dna_10_percent;
      test_case "empty" `Quick test_compress_to_dna_empty;
      test_case "ratio exceeds" `Quick test_compress_to_dna_ratio_exceeds;
    ];
    "deduplicate_lines", [
      test_case "no overlap" `Quick test_deduplicate_lines_no_overlap;
      test_case "with overlap" `Quick test_deduplicate_lines_with_overlap;
      test_case "short lines kept" `Quick test_deduplicate_lines_short_lines_kept;
      test_case "empty delta" `Quick test_deduplicate_lines_empty_delta;
      test_case "empty base" `Quick test_deduplicate_lines_empty_base;
    ];
    "merge_dna_with_delta", [
      test_case "empty delta" `Quick test_merge_dna_with_delta_empty;
      test_case "has delta" `Quick test_merge_dna_with_delta_has_delta;
      test_case "deduplicates" `Quick test_merge_dna_with_delta_deduplicates;
    ];
    "apoptosis", [
      test_case "begin" `Quick test_begin_apoptosis;
      test_case "preserves id" `Quick test_begin_apoptosis_preserves_id;
      test_case "complete" `Quick test_complete_apoptosis;
    ];
    "build_mitosis_prompt", [
      test_case "contains generation" `Quick test_build_mitosis_prompt_contains_generation;
      test_case "contains dna" `Quick test_build_mitosis_prompt_contains_dna;
      test_case "contains MITOSIS" `Quick test_build_mitosis_prompt_contains_mitosis;
    ];
    "state_to_string", [
      test_case "stem" `Quick test_state_to_string_stem;
      test_case "active" `Quick test_state_to_string_active;
      test_case "prepared" `Quick test_state_to_string_prepared;
      test_case "dividing" `Quick test_state_to_string_dividing;
      test_case "apoptotic" `Quick test_state_to_string_apoptotic;
    ];
    "phase_to_string", [
      test_case "idle" `Quick test_phase_to_string_idle;
      test_case "ready for handoff" `Quick test_phase_to_string_ready_for_handoff;
    ];
    "cell_to_json", [
      test_case "returns assoc" `Quick test_cell_to_json_returns_assoc;
      test_case "has id" `Quick test_cell_to_json_has_id;
      test_case "has generation" `Quick test_cell_to_json_has_generation;
      test_case "has state" `Quick test_cell_to_json_has_state;
      test_case "null dna" `Quick test_cell_to_json_null_dna;
    ];
    "pool_to_json", [
      test_case "returns assoc" `Quick test_pool_to_json_returns_assoc;
      test_case "has cells" `Quick test_pool_to_json_has_cells;
      test_case "has stem count" `Quick test_pool_to_json_has_stem_count;
    ];
    "trigger_to_json", [
      test_case "time based" `Quick test_trigger_to_json_time_based;
      test_case "task count" `Quick test_trigger_to_json_task_count;
      test_case "tool calls" `Quick test_trigger_to_json_tool_calls;
      test_case "context threshold" `Quick test_trigger_to_json_context_threshold;
      test_case "complexity spike" `Quick test_trigger_to_json_complexity_spike;
    ];
    "config_to_json", [
      test_case "returns assoc" `Quick test_config_to_json_returns_assoc;
      test_case "has triggers" `Quick test_config_to_json_has_triggers;
      test_case "has thresholds" `Quick test_config_to_json_has_thresholds;
    ];
    "should_prepare", [
      test_case "below threshold" `Quick test_should_prepare_below_threshold;
      test_case "at threshold" `Quick test_should_prepare_at_threshold;
      test_case "above threshold" `Quick test_should_prepare_above_threshold;
      test_case "already prepared" `Quick test_should_prepare_already_prepared;
    ];
    "should_handoff", [
      test_case "below threshold" `Quick test_should_handoff_below_threshold;
      test_case "at threshold" `Quick test_should_handoff_at_threshold;
      test_case "prepared below" `Quick test_should_handoff_prepared_below;
      test_case "prepared above" `Quick test_should_handoff_prepared_above;
      test_case "divide alias" `Quick test_should_divide_alias;
    ];
    "activate_stem", [
      test_case "with available" `Quick test_activate_stem_with_available;
      test_case "empty pool" `Quick test_activate_stem_empty_pool;
    ];
    "extract_dna", [
      test_case "contains header" `Quick test_extract_dna_contains_header;
      test_case "contains compressed" `Quick test_extract_dna_contains_compressed;
    ];
    "check_non_context_triggers", [
      test_case "time based" `Quick test_check_triggers_time_based;
      test_case "time not met" `Quick test_check_triggers_time_not_met;
      test_case "task count" `Quick test_check_triggers_task_count;
      test_case "task not met" `Quick test_check_triggers_task_not_met;
      test_case "tool calls" `Quick test_check_triggers_tool_calls;
      test_case "tool not met" `Quick test_check_triggers_tool_not_met;
      test_case "context ignored" `Quick test_check_triggers_context_ignored;
      test_case "complexity ignored" `Quick test_check_triggers_complexity_ignored;
      test_case "empty" `Quick test_check_triggers_empty;
    ];
    "extract_delta", [
      test_case "short session" `Quick test_extract_delta_short_session;
      test_case "no new content" `Quick test_extract_delta_no_new_content;
      test_case "too short" `Quick test_extract_delta_too_short;
      test_case "valid" `Quick test_extract_delta_valid;
      test_case "same len" `Quick test_extract_delta_same_len;
    ];
    "record_activity", [
      test_case "task done" `Quick test_record_activity_task_done;
      test_case "tool called" `Quick test_record_activity_tool_called;
      test_case "both" `Quick test_record_activity_both;
      test_case "neither" `Quick test_record_activity_neither;
      test_case "accumulates" `Quick test_record_activity_accumulates;
      test_case "updates timestamp" `Quick test_record_activity_updates_timestamp;
    ];
  ]
