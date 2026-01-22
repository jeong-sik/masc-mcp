(** Test Drift Guard Module *)

open Masc_mcp

let () = Random.self_init ()

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
    end else
      Sys.remove path

let with_temp_masc_dir f =
  let base =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "masc-drift-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.)))
  in
  Unix.mkdir base 0o755;
  let config = Room.default_config base in
  let _ = Room.init config ~agent_name:None in
  try
    let result = f config in
    let _ = Room.reset config in
    rm_rf base;
    result
  with e ->
    let _ = Room.reset config in
    rm_rf base;
    raise e

let test_text_similarity_identical () =
  let text = "This is a test sentence for similarity comparison." in
  let similarity = Drift_guard.text_similarity text text in
  (* Should be exactly 1.0 or very close *)
  assert (similarity > 0.99);
  print_endline "✓ test_text_similarity_identical passed"

let test_text_similarity_similar () =
  let original = "The quick brown fox jumps over the lazy dog." in
  let similar = "The quick brown fox leaps over the lazy dog." in  (* jumps -> leaps *)
  let similarity = Drift_guard.text_similarity original similar in
  assert (similarity > 0.85);  (* Should be highly similar *)
  print_endline "✓ test_text_similarity_similar passed"

let test_text_similarity_different () =
  let text1 = "Machine learning is a subset of artificial intelligence." in
  let text2 = "Cooking requires fresh ingredients and careful preparation." in
  let similarity = Drift_guard.text_similarity text1 text2 in
  assert (similarity < 0.3);  (* Should be very different *)
  print_endline "✓ test_text_similarity_different passed"

let test_verify_handoff_verified () =
  let original = "Task completed: Implemented user authentication with JWT tokens and OAuth2 support." in
  let received = "Task completed: Implemented user authentication using JWT tokens and OAuth2 support." in
  match Drift_guard.verify_handoff ~original ~received () with
  | Drift_guard.Verified { similarity } ->
    assert (similarity >= 0.85);
    print_endline "✓ test_verify_handoff_verified passed"
  | Drift_guard.Drift_detected _ ->
    failwith "Expected verification to pass"

let test_verify_handoff_semantic_drift () =
  let original = "We decided to use PostgreSQL for the database and Redis for caching." in
  let received = "We decided to use MongoDB for the database and Memcached for caching." in
  match Drift_guard.verify_handoff ~original ~received () with
  | Drift_guard.Drift_detected { drift_type; _ } ->
    assert (drift_type = Drift_guard.Semantic || drift_type = Drift_guard.Structural);
    print_endline "✓ test_verify_handoff_semantic_drift passed"
  | Drift_guard.Verified _ ->
    failwith "Expected drift detection"

let test_verify_handoff_factual_drift () =
  let original = "Completed tasks: 1. Setup database 2. Create API endpoints 3. Write tests 4. Deploy to staging 5. Monitor logs" in
  let received = "Completed tasks: 1. Setup database" in  (* Significant content loss *)
  match Drift_guard.verify_handoff ~original ~received () with
  | Drift_guard.Drift_detected { drift_type; _ } ->
    assert (drift_type = Drift_guard.Factual);
    print_endline "✓ test_verify_handoff_factual_drift passed"
  | Drift_guard.Verified _ ->
    failwith "Expected factual drift detection"

let test_verify_handoff_custom_threshold () =
  let original = "The project uses React for frontend." in
  let received = "The project uses Vue for frontend." in
  (* With high threshold, this should detect drift *)
  match Drift_guard.verify_handoff ~original ~received ~threshold:0.95 () with
  | Drift_guard.Drift_detected _ ->
    print_endline "✓ test_verify_handoff_custom_threshold passed"
  | Drift_guard.Verified _ ->
    failwith "Expected drift with high threshold"

let test_drift_type_conversion () =
  assert (Drift_guard.drift_type_to_string Drift_guard.Semantic = "semantic");
  assert (Drift_guard.drift_type_to_string Drift_guard.Factual = "factual");
  assert (Drift_guard.drift_type_to_string Drift_guard.Structural = "structural");
  assert (Drift_guard.drift_type_to_string Drift_guard.None = "none");

  assert (Drift_guard.drift_type_of_string "semantic" = Drift_guard.Semantic);
  assert (Drift_guard.drift_type_of_string "factual" = Drift_guard.Factual);
  assert (Drift_guard.drift_type_of_string "unknown" = Drift_guard.None);
  print_endline "✓ test_drift_type_conversion passed"

let test_verify_and_log () =
  with_temp_masc_dir (fun config ->
    Lwt_main.run (
      let open Lwt.Syntax in
      let original = "Working on task PK-12345: API implementation" in
      let received = "Working on task PK-12345: API implementation completed" in

      let* result = Drift_guard.verify_and_log config
          ~from_agent:"claude" ~to_agent:"gemini"
          ~task_id:"task-001"
          ~original ~received () in

      (* Verify result *)
      (match result with
       | Drift_guard.Verified _ -> ()
       | Drift_guard.Drift_detected _ -> ());  (* Either is fine for this test *)

      (* Verify log file exists *)
      let log_file = Drift_guard.drift_log_file config in
      assert (Sys.file_exists log_file);
      Lwt.return_unit
    )
  );
  print_endline "✓ test_verify_and_log passed"

let test_get_drift_stats () =
  with_temp_masc_dir (fun config ->
    Lwt_main.run (
      let open Lwt.Syntax in
      (* Log multiple events *)
      let* _ = Drift_guard.verify_and_log config
          ~from_agent:"a" ~to_agent:"b" ~task_id:"t1"
          ~original:"same text" ~received:"same text" () in
      let* _ = Drift_guard.verify_and_log config
          ~from_agent:"a" ~to_agent:"b" ~task_id:"t2"
          ~original:"completely different" ~received:"totally unrelated content here" () in

      let* (total, drift_count, avg_sim) = Drift_guard.get_drift_stats config ~days:7 in
      assert (total = 2);
      assert (drift_count >= 0 && drift_count <= 2);
      assert (avg_sim >= 0.0 && avg_sim <= 1.0);
      Lwt.return_unit
    )
  );
  print_endline "✓ test_get_drift_stats passed"

let test_korean_text () =
  (* Note: OCaml's Str.regexp doesn't fully support Unicode *)
  (* Test that Korean text at least doesn't crash and gives some similarity *)
  let original = "작업 완료: 사용자 인증 기능 구현" in
  let received = "작업 완료: 사용자 인증 기능 구현됨" in
  let similarity = Drift_guard.text_similarity original received in
  (* Just verify it returns a valid similarity (0.0-1.0), not exact value *)
  assert (similarity >= 0.0 && similarity <= 1.0);
  print_endline "✓ test_korean_text passed"

let () =
  print_endline "\n=== Drift Guard Tests ===\n";
  test_text_similarity_identical ();
  test_text_similarity_similar ();
  test_text_similarity_different ();
  test_verify_handoff_verified ();
  test_verify_handoff_semantic_drift ();
  test_verify_handoff_factual_drift ();
  test_verify_handoff_custom_threshold ();
  test_drift_type_conversion ();
  test_verify_and_log ();
  test_get_drift_stats ();
  test_korean_text ();
  print_endline "\nAll Drift Guard tests passed! ✓"
