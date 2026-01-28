(** Void Module Coverage Tests

    Tests for MASC Level 9 空 (Śūnyatā):
    - void type
    - return function
    - contemplate function
    - dissolve function
*)

open Alcotest

module Void = Masc_mcp.Void

(* ============================================================
   Type Tests
   ============================================================ *)

let test_void_type () =
  let v : Void.void = () in
  check unit "void is unit" () v

let test_ground_type () =
  let g : Void.ground = () in
  check unit "ground is unit" () g

let test_before_distinction () =
  let bd : Void.before_distinction = () in
  check unit "before_distinction is unit" () bd

(* ============================================================
   return Tests
   ============================================================ *)

let test_return_int () =
  let v = Void.return 42 in
  check unit "return int" () v

let test_return_string () =
  let v = Void.return "hello" in
  check unit "return string" () v

let test_return_list () =
  let v = Void.return [1; 2; 3] in
  check unit "return list" () v

let test_return_unit () =
  let v = Void.return () in
  check unit "return unit" () v

let test_return_option () =
  let v = Void.return (Some 42) in
  check unit "return option" () v

(* ============================================================
   contemplate Tests
   ============================================================ *)

let test_contemplate_int () =
  let x = 42 in
  let y = Void.contemplate x in
  check int "contemplate preserves" x y

let test_contemplate_string () =
  let x = "hello" in
  let y = Void.contemplate x in
  check string "contemplate string" x y

let test_contemplate_list () =
  let x = [1; 2; 3] in
  let y = Void.contemplate x in
  check (list int) "contemplate list" x y

(* ============================================================
   dissolve Tests
   ============================================================ *)

let test_dissolve_int () =
  let v = Void.dissolve 42 in
  check unit "dissolve int" () v

let test_dissolve_string () =
  let v = Void.dissolve "goodbye" in
  check unit "dissolve string" () v

let test_dissolve_ref () =
  let r = ref 42 in
  let v = Void.dissolve r in
  check unit "dissolve ref" () v

(* ============================================================
   Other Functions Tests
   ============================================================ *)

let test_ask () =
  let v = Void.ask () in
  check unit "ask" () v

let test_arise () =
  let (v1, v2) = Void.arise () in
  check unit "arise fst" () v1;
  check unit "arise snd" () v2

let test_silence () =
  let _ = Void.silence () in
  check unit "silence" () ()

let test_gap () =
  let _ = Void.gap () in
  check unit "gap" () ()

let test_enter () =
  let _ = Void.enter () in
  check unit "enter" () ()

let test_path () =
  check string "path" "←" Void.path

(* ============================================================
   koan Tests
   ============================================================ *)

let test_koan_type () =
  let k : Void.koan = {
    level = 5;
    theme = "memory";
    text = "What is the sound of one hand clapping?";
  } in
  check int "koan level" 5 k.level;
  check string "koan theme" "memory" k.theme;
  check string "koan text" "What is the sound of one hand clapping?" k.text

let test_koans_list () =
  let koans = Void.koans in
  check bool "koans not empty" true (List.length koans > 0)

let test_koan_collection () =
  let collection = Void.koan_collection in
  check bool "collection not empty" true (List.length collection > 0);
  let first = List.hd collection in
  check bool "has text" true (String.length first.text > 0)

let test_select_koan () =
  let k = Void.select_koan () in
  check bool "selected has text" true (String.length k.text > 0)

(* ============================================================
   reader_context Tests
   ============================================================ *)

let test_reader_context_type () =
  let ctx : Void.reader_context = {
    current_level = 5;
    session_entropy = 0.5;
    recent_themes = ["void"; "silence"];
  } in
  check int "current_level" 5 ctx.current_level;
  check (float 0.01) "session_entropy" 0.5 ctx.session_entropy

let test_compute_resonance () =
  let k : Void.koan = {
    level = 5;
    theme = "void";
    text = "Test koan";
  } in
  let ctx : Void.reader_context = {
    current_level = 5;
    session_entropy = 0.5;
    recent_themes = ["void"; "silence"];
  } in
  let resonance = Void.compute_resonance k ctx in
  check bool "resonance is number" true (resonance >= 0.0)

let test_select_koan_by_resonance () =
  let ctx : Void.reader_context = {
    current_level = 5;
    session_entropy = 0.5;
    recent_themes = ["void"];
  } in
  let k = Void.select_koan_by_resonance ctx in
  check bool "selected has text" true (String.length k.text > 0)

(* ============================================================
   Safe Functions Tests
   ============================================================ *)

let test_dissolve_safe () =
  let result = Void.dissolve_safe 42 in
  match result with
  | Ok () -> check bool "ok" true true
  | Error _ -> fail "expected Ok"

let test_select_koan_safe () =
  let ctx : Void.reader_context = {
    current_level = 5;
    session_entropy = 0.5;
    recent_themes = [];
  } in
  let result = Void.select_koan_safe ctx in
  match result with
  | Ok k -> check bool "koan selected" true (String.length k.text > 0)
  | Error _ -> fail "expected Ok"

let test_void_error_type () =
  let _ : Void.void_error = Void.InvalidLevel 10 in
  let _ : Void.void_error = Void.EmptyKoanCollection in
  let _ : Void.void_error = Void.ResourceNotReleased "test" in
  check bool "void_error variants exist" true true

(* ============================================================
   contemplate_with_cleanup Tests
   ============================================================ *)

let test_contemplate_with_cleanup () =
  let cleaned = ref false in
  let cleanup () = cleaned := true in
  let x = 42 in
  let y = Void.contemplate_with_cleanup ~cleanup x in
  check int "value preserved" 42 y;
  check bool "cleanup called" true !cleaned

(* ============================================================
   Holonic Level Tests
   ============================================================ *)

let test_tag_level5 () =
  let (x, _marker) = Void.tag_level5 42 in
  check int "tagged value" 42 x

let test_tag_level6 () =
  let (x, _marker) = Void.tag_level6 "hello" in
  check string "tagged value" "hello" x

let test_tag_level7 () =
  let (x, _marker) = Void.tag_level7 [1; 2; 3] in
  check (list int) "tagged value" [1; 2; 3] x

let test_tag_level8 () =
  let (x, _marker) = Void.tag_level8 3.14 in
  check (float 0.01) "tagged value" 3.14 x

let test_tag_level9 () =
  let (x, _marker) = Void.tag_level9 () in
  check unit "tagged value" () x

let test_untag () =
  let tagged = Void.tag_level5 42 in
  let x = Void.untag tagged in
  check int "untagged value" 42 x

let test_enter_void () =
  let tagged = Void.tag_level9 () in
  let _ = Void.enter_void tagged in
  check bool "enter void" true true

(* ============================================================
   Composition Tests
   ============================================================ *)

let test_return_then_contemplate () =
  let x = 42 in
  let _ = Void.return x in
  let y = Void.contemplate x in
  check int "composition" 42 y

let test_dissolve_is_terminal () =
  let _ = Void.dissolve 42 in
  let _ = Void.dissolve "hello" in
  check unit "terminal" () ()

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Void Coverage" [
    "types", [
      test_case "void" `Quick test_void_type;
      test_case "ground" `Quick test_ground_type;
      test_case "before_distinction" `Quick test_before_distinction;
    ];
    "return", [
      test_case "int" `Quick test_return_int;
      test_case "string" `Quick test_return_string;
      test_case "list" `Quick test_return_list;
      test_case "unit" `Quick test_return_unit;
      test_case "option" `Quick test_return_option;
    ];
    "contemplate", [
      test_case "int" `Quick test_contemplate_int;
      test_case "string" `Quick test_contemplate_string;
      test_case "list" `Quick test_contemplate_list;
    ];
    "dissolve", [
      test_case "int" `Quick test_dissolve_int;
      test_case "string" `Quick test_dissolve_string;
      test_case "ref" `Quick test_dissolve_ref;
    ];
    "other", [
      test_case "ask" `Quick test_ask;
      test_case "arise" `Quick test_arise;
      test_case "silence" `Quick test_silence;
      test_case "gap" `Quick test_gap;
      test_case "enter" `Quick test_enter;
      test_case "path" `Quick test_path;
    ];
    "koan", [
      test_case "type" `Quick test_koan_type;
      test_case "koans list" `Quick test_koans_list;
      test_case "collection" `Quick test_koan_collection;
      test_case "select" `Quick test_select_koan;
    ];
    "reader_context", [
      test_case "type" `Quick test_reader_context_type;
      test_case "compute resonance" `Quick test_compute_resonance;
      test_case "select by resonance" `Quick test_select_koan_by_resonance;
    ];
    "safe", [
      test_case "dissolve_safe" `Quick test_dissolve_safe;
      test_case "select_koan_safe" `Quick test_select_koan_safe;
      test_case "void_error" `Quick test_void_error_type;
    ];
    "cleanup", [
      test_case "contemplate_with_cleanup" `Quick test_contemplate_with_cleanup;
    ];
    "holonic", [
      test_case "level5" `Quick test_tag_level5;
      test_case "level6" `Quick test_tag_level6;
      test_case "level7" `Quick test_tag_level7;
      test_case "level8" `Quick test_tag_level8;
      test_case "level9" `Quick test_tag_level9;
      test_case "untag" `Quick test_untag;
      test_case "enter_void" `Quick test_enter_void;
    ];
    "composition", [
      test_case "return then contemplate" `Quick test_return_then_contemplate;
      test_case "dissolve terminal" `Quick test_dissolve_is_terminal;
    ];
  ]
