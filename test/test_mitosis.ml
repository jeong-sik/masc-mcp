open Alcotest
open Masc_mcp

(** Test Mitosis module - 2-Phase Cell Division Pattern *)

(* ===== safe_sub tests ===== *)

let test_safe_sub_normal () =
  let result = Mitosis.safe_sub "hello world" 0 5 in
  check string "normal substring" "hello" result

let test_safe_sub_negative_start () =
  let result = Mitosis.safe_sub "hello" (-1) 3 in
  check string "negative start returns empty" "" result

let test_safe_sub_negative_len () =
  let result = Mitosis.safe_sub "hello" 0 (-1) in
  check string "negative len returns empty" "" result

let test_safe_sub_start_beyond_len () =
  let result = Mitosis.safe_sub "hello" 10 3 in
  check string "start beyond length returns empty" "" result

let test_safe_sub_len_exceeds () =
  let result = Mitosis.safe_sub "hello" 3 100 in
  check string "len exceeds returns rest" "lo" result

let test_safe_sub_empty_string () =
  let result = Mitosis.safe_sub "" 0 5 in
  check string "empty string returns empty" "" result

(* ===== deduplicate_lines tests ===== *)

let test_dedup_no_overlap () =
  let base = "line1\nline2\nline3" in
  let delta = "line4\nline5" in
  let result = Mitosis.deduplicate_lines ~base ~delta in
  check string "no overlap keeps all" "line4\nline5" result

let test_dedup_full_overlap () =
  let base = "this is a longer line\nanother long line here" in
  let delta = "this is a longer line\nanother long line here" in
  let result = Mitosis.deduplicate_lines ~base ~delta in
  (* Should be empty since both lines are duplicates *)
  check string "full overlap removes all" "" result

let test_dedup_partial_overlap () =
  let base = "existing long line here\nkeep this one" in
  let delta = "existing long line here\nnew unique line" in
  let result = Mitosis.deduplicate_lines ~base ~delta in
  (* First line filtered out, only "new unique line" remains *)
  check string "partial overlap removes duplicates" "new unique line" result

let test_dedup_short_lines_kept () =
  let base = "short\nthis is a long line" in
  let delta = "short\nnew" in
  let result = Mitosis.deduplicate_lines ~base ~delta in
  (* Short lines (<=10 chars) are always kept *)
  check string "short lines always kept" "short\nnew" result

(* ===== compress_to_dna tests ===== *)

let test_compress_ratio () =
  let context = String.make 1000 'a' in
  let result = Mitosis.compress_to_dna ~ratio:0.1 ~context in
  check int "10% compression" 100 (String.length result)

let test_compress_ratio_over_100 () =
  let context = "short" in
  let result = Mitosis.compress_to_dna ~ratio:2.0 ~context in
  check string "ratio > 1 returns original" "short" result

(* ===== 2-Phase mitosis tests ===== *)

let test_should_prepare_at_50 () =
  let config = Mitosis.default_config in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let cell = { cell with state = Mitosis.Active; phase = Mitosis.Idle } in
  let result = Mitosis.should_prepare ~config ~cell ~context_ratio:0.5 in
  check bool "should prepare at 50%" true result

let test_should_not_prepare_below_threshold () =
  let config = Mitosis.default_config in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let cell = { cell with state = Mitosis.Active; phase = Mitosis.Idle } in
  let result = Mitosis.should_prepare ~config ~cell ~context_ratio:0.3 in
  check bool "should not prepare at 30%" false result

let test_should_not_prepare_when_already_prepared () =
  let config = Mitosis.default_config in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let cell = { cell with
    state = Mitosis.Prepared;
    phase = Mitosis.ReadyForHandoff "some dna"
  } in
  let result = Mitosis.should_prepare ~config ~cell ~context_ratio:0.6 in
  check bool "should not re-prepare" false result

let test_should_handoff_at_80 () =
  let config = Mitosis.default_config in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let cell = { cell with
    state = Mitosis.Prepared;
    phase = Mitosis.ReadyForHandoff "dna"
  } in
  let result = Mitosis.should_handoff ~config ~cell ~context_ratio:0.8 in
  check bool "should handoff at 80%" true result

let test_should_not_handoff_at_60 () =
  let config = Mitosis.default_config in
  let cell = Mitosis.create_stem_cell ~generation:0 in
  let cell = { cell with
    state = Mitosis.Prepared;
    phase = Mitosis.ReadyForHandoff "dna"
  } in
  let result = Mitosis.should_handoff ~config ~cell ~context_ratio:0.6 in
  check bool "should not handoff at 60%" false result

(* ===== extract_delta tests ===== *)

let test_extract_delta_short_session () =
  let config = { Mitosis.default_config with min_context_for_delta = 1000 } in
  let result = Mitosis.extract_delta ~config ~full_context:"short" ~since_len:0 in
  check string "short session returns empty" "" result

let test_extract_delta_no_new_content () =
  let config = Mitosis.default_config in
  let context = String.make 2000 'a' in
  let result = Mitosis.extract_delta ~config ~full_context:context ~since_len:2000 in
  check string "no new content returns empty" "" result

let test_extract_delta_noise_filter () =
  let config = { Mitosis.default_config with
    min_context_for_delta = 100;
    min_delta_len = 100;
    dna_compression_ratio = 1.0  (* No compression for test *)
  } in
  let context = String.make 200 'a' ^ "tiny" in
  let result = Mitosis.extract_delta ~config ~full_context:context ~since_len:200 in
  check string "tiny delta filtered as noise" "" result

(* ===== cell state tests ===== *)

let test_cell_state_to_string () =
  check string "stem" "stem" (Mitosis.state_to_string Mitosis.Stem);
  check string "active" "active" (Mitosis.state_to_string Mitosis.Active);
  check string "prepared" "prepared" (Mitosis.state_to_string Mitosis.Prepared);
  check string "dividing" "dividing" (Mitosis.state_to_string Mitosis.Dividing);
  check string "apoptotic" "apoptotic" (Mitosis.state_to_string Mitosis.Apoptotic)

let test_phase_to_string () =
  check string "idle" "idle" (Mitosis.phase_to_string Mitosis.Idle);
  check string "ready" "ready_for_handoff"
    (Mitosis.phase_to_string (Mitosis.ReadyForHandoff "dna"))

(* ===== Test suites ===== *)

let safe_sub_tests = [
  "normal", `Quick, test_safe_sub_normal;
  "negative start", `Quick, test_safe_sub_negative_start;
  "negative len", `Quick, test_safe_sub_negative_len;
  "start beyond len", `Quick, test_safe_sub_start_beyond_len;
  "len exceeds", `Quick, test_safe_sub_len_exceeds;
  "empty string", `Quick, test_safe_sub_empty_string;
]

let dedup_tests = [
  "no overlap", `Quick, test_dedup_no_overlap;
  "full overlap", `Quick, test_dedup_full_overlap;
  "partial overlap", `Quick, test_dedup_partial_overlap;
  "short lines kept", `Quick, test_dedup_short_lines_kept;
]

let compress_tests = [
  "ratio 10%", `Quick, test_compress_ratio;
  "ratio > 100%", `Quick, test_compress_ratio_over_100;
]

let two_phase_tests = [
  "prepare at 50%", `Quick, test_should_prepare_at_50;
  "no prepare below threshold", `Quick, test_should_not_prepare_below_threshold;
  "no re-prepare", `Quick, test_should_not_prepare_when_already_prepared;
  "handoff at 80%", `Quick, test_should_handoff_at_80;
  "no handoff at 60%", `Quick, test_should_not_handoff_at_60;
]

let delta_tests = [
  "short session", `Quick, test_extract_delta_short_session;
  "no new content", `Quick, test_extract_delta_no_new_content;
  "noise filter", `Quick, test_extract_delta_noise_filter;
]

let state_tests = [
  "cell state to string", `Quick, test_cell_state_to_string;
  "phase to string", `Quick, test_phase_to_string;
]

let () =
  run "Mitosis" [
    "safe_sub", safe_sub_tests;
    "deduplicate", dedup_tests;
    "compress", compress_tests;
    "2-phase", two_phase_tests;
    "delta", delta_tests;
    "state", state_tests;
  ]
