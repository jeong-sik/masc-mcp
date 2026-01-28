(** Tests for Checkpoint_fs filesystem backend *)

module Checkpoint_fs = Masc_mcp.Checkpoint_fs
open Masc_mcp.Checkpoint_types
open Alcotest

let () = Random.init 42

let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

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
      (Printf.sprintf "masc-checkpoint-fs-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.)))
  in
  Unix.mkdir base 0o755;
  let masc_dir = Filename.concat base ".masc" in
  try
    let result = f masc_dir in
    rm_rf base;
    result
  with e ->
    rm_rf base;
    raise e

let test_save_and_resume () =
  with_temp_masc_dir (fun masc_dir ->
    let task_id = "task-001" in
    let result =
      Lwt_main.run
        (Checkpoint_fs.save_checkpoint ~masc_dir ~task_id ~step:1
           ~action:"init" ~agent:"codex" ~state:"{}" ())
    in
    match result with
    | Error e -> fail e
    | Ok checkpoint_id ->
        check bool "id prefix" true (starts_with ~prefix:"cp-task-001-" checkpoint_id);
        let resume =
          Lwt_main.run (Checkpoint_fs.get_last_checkpoint ~masc_dir ~task_id)
        in
        match resume with
        | Error e -> fail e
        | Ok None -> fail "expected resume checkpoint"
        | Ok (Some json) ->
            let open Yojson.Safe.Util in
            check string "task id" checkpoint_id (json |> member "id" |> to_string);
            check int "step" 1 (json |> member "step" |> to_int);
            check string "action" "init" (json |> member "action" |> to_string);
            check string "agent" "codex" (json |> member "agent" |> to_string);
            check string "status" "completed" (json |> member "status" |> to_string);
            check string "interrupt_message" "" (json |> member "interrupt_message" |> to_string))

let test_interrupt_pending_and_approve () =
  with_temp_masc_dir (fun masc_dir ->
    let task_id = "task-002" in
    let interrupt =
      Lwt_main.run
        (Checkpoint_fs.save_checkpoint ~masc_dir ~task_id ~step:1
           ~action:"danger" ~agent:"codex" ~state:"{}"
           ~status:Interrupted ~interrupt_message:"really?" ())
    in
    match interrupt with
    | Error e -> fail e
    | Ok _checkpoint_id ->
        let pending =
          Lwt_main.run (Checkpoint_fs.get_pending_interrupts ~masc_dir ~timeout_minutes:30 ())
        in
        (match pending with
        | Error e -> fail e
        | Ok json ->
            let open Yojson.Safe.Util in
            let records = json |> member "records" |> to_list in
            check int "pending count" 1 (List.length records));
        let approved =
          Lwt_main.run (Checkpoint_fs.update_checkpoint_status ~masc_dir ~task_id ~new_status:Completed ())
        in
        (match approved with
        | Ok _ -> ()
        | Error e -> fail e);
        let pending_after =
          Lwt_main.run (Checkpoint_fs.get_pending_interrupts ~masc_dir ~timeout_minutes:30 ())
        in
        match pending_after with
        | Error e -> fail e
        | Ok json ->
            let open Yojson.Safe.Util in
            let records = json |> member "records" |> to_list in
            check int "pending after approve" 0 (List.length records))

let test_auto_reject_timeout () =
  with_temp_masc_dir (fun masc_dir ->
    let task_id = "task-003" in
    let created_at = Unix.gettimeofday () -. 3600.0 in
    let interrupt =
      Lwt_main.run
        (Checkpoint_fs.save_checkpoint ~masc_dir ~task_id ~step:1
           ~action:"timeout" ~agent:"codex" ~state:"{}"
           ~status:Interrupted ~interrupt_message:"wait" ~created_at ())
    in
    match interrupt with
    | Error e -> fail e
    | Ok _ ->
        let pending =
          Lwt_main.run (Checkpoint_fs.get_pending_interrupts ~masc_dir ~timeout_minutes:30 ())
        in
        match pending with
        | Error e -> fail e
        | Ok json ->
            let open Yojson.Safe.Util in
            let records = json |> member "records" |> to_list in
            check int "auto rejected pending count" 0 (List.length records))

let () =
  run "Checkpoint_fs" [
    "save_resume", [
      test_case "save + resume" `Quick test_save_and_resume;
    ];
    "interrupt", [
      test_case "pending + approve" `Quick test_interrupt_pending_and_approve;
      test_case "auto reject timeout" `Quick test_auto_reject_timeout;
    ];
  ]
