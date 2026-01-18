(** Test Tempo Module *)

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
      (Printf.sprintf "masc-tempo-%d-%06d" (Unix.getpid ()) (Random.int 1_000_000))
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

(* Test 1: Get tempo returns default state *)
let test_get_tempo_default () =
  with_temp_masc_dir (fun config ->
    let state = Tempo.get_tempo config in
    assert (state.current_interval_s = 300.0);  (* default 5 minutes *)
    assert (state.reason = "default");
    Printf.printf "  ✓ Default tempo is 300s (5 minutes)\n"
  )

(* Test 2: Set tempo manually *)
let test_set_tempo () =
  with_temp_masc_dir (fun config ->
    let state = Tempo.set_tempo config ~interval_s:120.0 ~reason:"test adjustment" in
    assert (state.current_interval_s = 120.0);
    assert (state.reason = "test adjustment");
    assert (state.last_adjusted > 0.0);
    Printf.printf "  ✓ Set tempo to 120s works\n"
  )

(* Test 3: Set tempo clamps to minimum *)
let test_set_tempo_min_clamp () =
  with_temp_masc_dir (fun config ->
    let state = Tempo.set_tempo config ~interval_s:10.0 ~reason:"too fast" in
    assert (state.current_interval_s = 60.0);  (* clamped to min *)
    Printf.printf "  ✓ Tempo clamped to minimum 60s\n"
  )

(* Test 4: Set tempo clamps to maximum *)
let test_set_tempo_max_clamp () =
  with_temp_masc_dir (fun config ->
    let state = Tempo.set_tempo config ~interval_s:3600.0 ~reason:"too slow" in
    assert (state.current_interval_s = 600.0);  (* clamped to max *)
    Printf.printf "  ✓ Tempo clamped to maximum 600s\n"
  )

(* Test 5: Reset tempo *)
let test_reset_tempo () =
  with_temp_masc_dir (fun config ->
    (* Set a custom tempo *)
    let _ = Tempo.set_tempo config ~interval_s:120.0 ~reason:"custom" in
    (* Reset it *)
    let state = Tempo.reset_tempo config in
    assert (state.current_interval_s = 300.0);
    assert (state.reason = "reset to default");
    Printf.printf "  ✓ Reset tempo restores 300s default\n"
  )

(* Test 6: Tempo persists across loads *)
let test_tempo_persistence () =
  with_temp_masc_dir (fun config ->
    (* Set tempo *)
    let _ = Tempo.set_tempo config ~interval_s:180.0 ~reason:"persistent test" in
    (* Load again *)
    let state = Tempo.get_tempo config in
    assert (state.current_interval_s = 180.0);
    assert (state.reason = "persistent test");
    Printf.printf "  ✓ Tempo persists across loads\n"
  )

(* Test 7: Format state output *)
let test_format_state () =
  let state = {
    Tempo.current_interval_s = 60.0;
    last_adjusted = Unix.gettimeofday ();
    reason = "urgent task";
  } in
  let output = Tempo.format_state state in
  (* Check output contains "Tempo" and has reasonable length *)
  let contains_tempo =
    try let _ = Str.search_forward (Str.regexp "Tempo") output 0 in true
    with Not_found -> false
  in
  assert contains_tempo;
  assert (String.length output > 20);
  Printf.printf "  ✓ Format state produces readable output: %s\n" output

(* Test 8: Adjust tempo with no tasks -> slow *)
let test_adjust_tempo_idle () =
  with_temp_masc_dir (fun config ->
    (* No tasks added, should be idle/slow *)
    Lwt_main.run (
      let open Lwt.Syntax in
      let* state = Tempo.adjust_tempo config in
      assert (state.current_interval_s = 600.0);  (* max for idle *)
      assert (String.length state.reason > 0);
      Printf.printf "  ✓ Idle state = 600s (10 minutes)\n";
      Lwt.return_unit
    )
  )

(* Test 9: Adjust tempo with urgent task -> fast *)
let test_adjust_tempo_urgent () =
  with_temp_masc_dir (fun config ->
    (* Add an urgent task (priority 1) *)
    let _ = Room.add_task config
      ~title:"Urgent task"
      ~description:""
      ~priority:1 in
    Lwt_main.run (
      let open Lwt.Syntax in
      let* state = Tempo.adjust_tempo config in
      assert (state.current_interval_s = 60.0);  (* min for urgent *)
      Printf.printf "  ✓ Urgent task = 60s (1 minute)\n";
      Lwt.return_unit
    )
  )

(* Test 10: Adjust tempo with normal task -> default *)
let test_adjust_tempo_normal () =
  with_temp_masc_dir (fun config ->
    (* Add a normal task (priority 3) *)
    let _ = Room.add_task config
      ~title:"Normal task"
      ~description:""
      ~priority:3 in
    Lwt_main.run (
      let open Lwt.Syntax in
      let* state = Tempo.adjust_tempo config in
      assert (state.current_interval_s = 300.0);  (* default for normal *)
      Printf.printf "  ✓ Normal task = 300s (5 minutes)\n";
      Lwt.return_unit
    )
  )

let () =
  Printf.printf "Testing Tempo module...\n";
  test_get_tempo_default ();
  test_set_tempo ();
  test_set_tempo_min_clamp ();
  test_set_tempo_max_clamp ();
  test_reset_tempo ();
  test_tempo_persistence ();
  test_format_state ();
  test_adjust_tempo_idle ();
  test_adjust_tempo_urgent ();
  test_adjust_tempo_normal ();
  Printf.printf "✅ All 10 tempo tests passed!\n"
