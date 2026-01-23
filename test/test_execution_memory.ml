(** Tests for Execution Memory *)

open Masc_mcp
open Alcotest

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
      (Printf.sprintf "masc-run-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.)))
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

(* Test: Init run *)
let test_init_run () =
  with_temp_masc_dir (fun config ->
    let result = Lwt_main.run (
      Execution_memory.init_run config ~task_id:"task-001" ~agent_name:"claude"
    ) in
    match result with
    | Error e -> fail e
    | Ok run ->
        check string "task_id" "task-001" run.task_id;
        check string "agent_name" "claude" run.agent_name;
        check string "status" "running" (Execution_memory.status_to_string run.status);
        check (list string) "notes" [] run.notes;
        check (option string) "deliverable" None run.deliverable)

(* Test: Set plan *)
let test_set_plan () =
  with_temp_masc_dir (fun config ->
    let _ = Lwt_main.run (
      Execution_memory.init_run config ~task_id:"task-002" ~agent_name:"gemini"
    ) in
    let plan = "1. Research\n2. Implement\n3. Test" in
    let result = Lwt_main.run (
      Execution_memory.set_plan config ~task_id:"task-002" ~plan
    ) in
    match result with
    | Error e -> fail e
    | Ok run ->
        check string "plan" plan run.plan)

(* Test: Add notes *)
let test_add_notes () =
  with_temp_masc_dir (fun config ->
    let _ = Lwt_main.run (
      Execution_memory.init_run config ~task_id:"task-003" ~agent_name:"codex"
    ) in
    let _ = Lwt_main.run (
      Execution_memory.add_note config ~task_id:"task-003" ~note:"Started work"
    ) in
    let result = Lwt_main.run (
      Execution_memory.add_note config ~task_id:"task-003" ~note:"Made progress"
    ) in
    match result with
    | Error e -> fail e
    | Ok run ->
        check int "notes count" 2 (List.length run.notes);
        (* Notes should contain timestamps *)
        check bool "has timestamp" true (
          List.for_all (fun n -> String.length n > 10) run.notes))

(* Test: Set deliverable *)
let test_set_deliverable () =
  with_temp_masc_dir (fun config ->
    let _ = Lwt_main.run (
      Execution_memory.init_run config ~task_id:"task-004" ~agent_name:"claude"
    ) in
    let deliverable = "Created PR #123" in
    let result = Lwt_main.run (
      Execution_memory.set_deliverable config ~task_id:"task-004" ~deliverable
    ) in
    match result with
    | Error e -> fail e
    | Ok run ->
        check (option string) "deliverable" (Some deliverable) run.deliverable;
        check string "status" "completed" (Execution_memory.status_to_string run.status))

(* Test: List runs *)
let test_list_runs () =
  with_temp_masc_dir (fun config ->
    let _ = Lwt_main.run (
      Execution_memory.init_run config ~task_id:"task-a" ~agent_name:"claude"
    ) in
    let _ = Lwt_main.run (
      Execution_memory.init_run config ~task_id:"task-b" ~agent_name:"gemini"
    ) in
    let runs = Lwt_main.run (Execution_memory.list_runs config) in
    check int "run count" 2 (List.length runs))

(* Test: Format as markdown *)
let test_format_markdown () =
  with_temp_masc_dir (fun config ->
    let _ = Lwt_main.run (
      Execution_memory.init_run config ~task_id:"task-md" ~agent_name:"claude"
    ) in
    let _ = Lwt_main.run (
      Execution_memory.set_plan config ~task_id:"task-md" ~plan:"Test plan"
    ) in
    let _ = Lwt_main.run (
      Execution_memory.add_note config ~task_id:"task-md" ~note:"Note 1"
    ) in
    let result = Lwt_main.run (Execution_memory.load_run config "task-md") in
    match result with
    | Error e -> fail e
    | Ok run ->
        let md = Execution_memory.format_as_markdown run in
        check bool "has header" true (String.length md > 0);
        check bool "contains task" true (
          try let _ = Str.search_forward (Str.regexp_string "task-md") md 0 in true
          with Not_found -> false);
        check bool "contains plan" true (
          try let _ = Str.search_forward (Str.regexp_string "Test plan") md 0 in true
          with Not_found -> false))

(* Test: Status transitions *)
let test_status_transitions () =
  check string "running" "running" (Execution_memory.status_to_string Execution_memory.Running);
  check string "completed" "completed" (Execution_memory.status_to_string Execution_memory.Completed);
  check string "paused" "paused" (Execution_memory.status_to_string Execution_memory.Paused);
  check string "failed" "failed: oops" (Execution_memory.status_to_string (Execution_memory.Failed "oops"));

  (* Round-trip *)
  let rt status =
    Execution_memory.status_of_string (Execution_memory.status_to_string status)
  in
  check string "rt running" "running" (Execution_memory.status_to_string (rt Execution_memory.Running));
  check string "rt completed" "completed" (Execution_memory.status_to_string (rt Execution_memory.Completed))

let () =
  run "Execution Memory" [
    "init", [
      test_case "init run" `Quick test_init_run;
    ];
    "plan", [
      test_case "set plan" `Quick test_set_plan;
    ];
    "notes", [
      test_case "add notes" `Quick test_add_notes;
    ];
    "deliverable", [
      test_case "set deliverable" `Quick test_set_deliverable;
    ];
    "list", [
      test_case "list runs" `Quick test_list_runs;
    ];
    "format", [
      test_case "format markdown" `Quick test_format_markdown;
    ];
    "status", [
      test_case "status transitions" `Quick test_status_transitions;
    ];
  ]
