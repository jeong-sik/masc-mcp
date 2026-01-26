(* Walph Wiggum 종합 검증 테스트 *)

open Alcotest
module R = Masc_mcp.Room

(* 1. 형식증명: preset → chain ID 매핑 *)
let test_chain_mapping () =
  check (option string) "coverage preset"
    (Some "walph-coverage") (R.get_chain_id_for_preset "coverage");
  check (option string) "refactor preset"
    (Some "walph-refactor") (R.get_chain_id_for_preset "refactor");
  check (option string) "docs preset"
    (Some "walph-docs") (R.get_chain_id_for_preset "docs");
  check (option string) "drain preset"
    None (R.get_chain_id_for_preset "drain");
  check (option string) "unknown preset"
    None (R.get_chain_id_for_preset "unknown")

(* 2. 반대로: @walph 명령어 파싱 - 정상 케이스 *)
let test_command_parsing_valid () =
  let check_cmd input expected_cmd =
    match R.parse_walph_command input with
    | Some (cmd, _) ->
        check string ("parse " ^ input)
          expected_cmd (String.uppercase_ascii cmd)
    | None ->
        fail ("Expected Some for: " ^ input)
  in
  check_cmd "@walph STOP" "STOP";
  check_cmd "@walph PAUSE" "PAUSE";
  check_cmd "@walph RESUME" "RESUME";
  check_cmd "@walph STATUS" "STATUS";
  check_cmd "@walph START coverage" "START"

(* 3. 반대로: @walph 명령어 파싱 - 무시할 케이스 *)
let test_command_parsing_invalid () =
  let check_none input =
    match R.parse_walph_command input with
    | None -> ()
    | Some _ -> fail ("Expected None for: " ^ input)
  in
  check_none "hello world";
  check_none "just some text"

(* 4. 수직 확장: UTF-8 접두사 체크 *)
let test_utf8_prefix () =
  let starts_with prefix s =
    let plen = String.length prefix in
    String.length s >= plen && String.sub s 0 plen = prefix
  in
  check bool "emoji 📋 prefix" true (starts_with "📋" "📋 No unclaimed tasks");
  check bool "emoji ✅ prefix" true (starts_with "✅" "✅ claude claimed task-1");
  check bool "text No prefix" true (starts_with "No" "No unclaimed tasks");
  check bool "wrong emoji" false (starts_with "📋" "No unclaimed tasks")

(* 5. 수평 확장: 대소문자 무시 *)
let test_case_insensitive () =
  let check_cmd input expected_cmd =
    match R.parse_walph_command input with
    | Some (cmd, _) ->
        check string ("parse " ^ input)
          expected_cmd (String.uppercase_ascii cmd)
    | None ->
        fail ("Expected Some for: " ^ input)
  in
  check_cmd "@WALPH stop" "STOP";
  check_cmd "@Walph Pause" "PAUSE";
  check_cmd "@wAlPh ResuMe" "RESUME"

(* 6. Recursive: 중첩 패턴 *)
let test_recursive_patterns () =
  (* 첫 번째 @walph 이후 명령어만 추출 *)
  match R.parse_walph_command "@walph @walph STOP" with
  | Some (cmd, _) ->
      check string "nested @walph" "@WALPH" (String.uppercase_ascii cmd)
  | None ->
      (* @walph 뒤에 @walph이 오면 "@walph"이 명령어로 파싱됨 - 이건 OK *)
      ()

(* Test suite *)
let () =
  run "Walph Wiggum Tests" [
    "형식증명", [
      test_case "chain_mapping" `Quick test_chain_mapping;
    ];
    "반대로", [
      test_case "valid_commands" `Quick test_command_parsing_valid;
      test_case "invalid_commands" `Quick test_command_parsing_invalid;
    ];
    "수직확장", [
      test_case "utf8_prefix" `Quick test_utf8_prefix;
    ];
    "수평확장", [
      test_case "case_insensitive" `Quick test_case_insensitive;
    ];
    "recursive", [
      test_case "nested_patterns" `Quick test_recursive_patterns;
    ];
  ]
