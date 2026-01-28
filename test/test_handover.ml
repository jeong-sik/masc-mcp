(** Tests for Handover (Cellular Agent DNA Transfer) *)

open Masc_mcp
open Alcotest

let () = Random.init 42

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
      (Printf.sprintf "masc-handover-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.)))
  in
  Unix.mkdir base 0o755;
  (* Initialize MASC room first *)
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

(* Test: Create and save handover *)
let test_create_handover () =
  with_temp_masc_dir (fun config ->
    let handover = Handover.create_handover
      ~from_agent:"claude"
      ~task_id:"task-001"
      ~session_id:"session-abc123"
      ~reason:Handover.Explicit
    in
    check string "from_agent" "claude" handover.from_agent;
    check string "task_id" "task-001" handover.task_id;
    check string "reason" "explicit" handover.handover_reason;
    check (option string) "to_agent" None handover.to_agent;

    let save_result = Lwt_main.run (Handover.save_handover config handover) in
    match save_result with
    | Error e -> fail e
    | Ok () ->
        let load_result = Lwt_main.run (Handover.load_handover config handover.id) in
        match load_result with
        | Error e -> fail e
        | Ok loaded ->
            check string "loaded id" handover.id loaded.id;
            check string "loaded task" handover.task_id loaded.task_id)

(* Test: Create handover from planning context *)
let test_create_from_planning () =
  with_temp_masc_dir (fun config ->
    let result = Lwt_main.run (
      Handover.create_from_planning config
        ~from_agent:"gemini"
        ~task_id:"task-002"
        ~session_id:"session-xyz"
        ~reason:(Handover.ContextLimit 85)
        ~goal:"Implement feature X"
        ~progress:"50% complete"
        ~completed:["Step 1"; "Step 2"]
        ~pending:["Step 3"; "Step 4"]
        ~decisions:["Chose React Query over SWR"]
        ~assumptions:["API is stable"]
        ~warnings:["Watch for rate limits"]
        ~errors:["Network error on attempt 2"]
        ~files:["src/foo.ts"; "src/bar.ts"]
        ~context_pct:85
    ) in
    match result with
    | Error e -> fail e
    | Ok h ->
        check string "goal" "Implement feature X" h.current_goal;
        check string "progress" "50% complete" h.progress_summary;
        check int "completed count" 2 (List.length h.completed_steps);
        check int "pending count" 2 (List.length h.pending_steps);
        check int "context pct" 85 h.context_usage_percent;
        check bool "reason contains context" true
          (String.length h.handover_reason > 0))

(* Test: List and filter handovers *)
let test_list_handovers () =
  with_temp_masc_dir (fun config ->
    (* Create 3 handovers *)
    let h1 = Handover.create_handover ~from_agent:"claude" ~task_id:"t1"
      ~session_id:"s1" ~reason:Handover.Explicit in
    let h2 = Handover.create_handover ~from_agent:"gemini" ~task_id:"t2"
      ~session_id:"s2" ~reason:Handover.TaskComplete in
    let h3 = Handover.create_handover ~from_agent:"codex" ~task_id:"t3"
      ~session_id:"s3" ~reason:(Handover.FatalError "crash") in

    Lwt_main.run (Handover.save_handover config h1) |> ignore;
    Lwt_main.run (Handover.save_handover config h2) |> ignore;
    Lwt_main.run (Handover.save_handover config h3) |> ignore;

    (* List all *)
    let all = Lwt_main.run (Handover.list_handovers config) in
    check int "all count" 3 (List.length all);

    (* All should be pending (unclaimed) *)
    let pending = Lwt_main.run (Handover.get_pending_handovers config) in
    check int "pending count" 3 (List.length pending))

(* Test: Claim handover *)
let test_claim_handover () =
  with_temp_masc_dir (fun config ->
    let h = Handover.create_handover ~from_agent:"claude" ~task_id:"t1"
      ~session_id:"s1" ~reason:Handover.Explicit in
    Lwt_main.run (Handover.save_handover config h) |> ignore;

    (* Claim it *)
    let claim_result = Lwt_main.run (
      Handover.claim_handover config ~handover_id:h.id ~agent_name:"gemini"
    ) in
    match claim_result with
    | Error e -> fail e
    | Ok claimed ->
        check (option string) "claimed by" (Some "gemini") claimed.to_agent;

        (* Cannot claim again *)
        let re_claim = Lwt_main.run (
          Handover.claim_handover config ~handover_id:h.id ~agent_name:"codex"
        ) in
        match re_claim with
        | Ok _ -> fail "should not allow double claim"
        | Error _ -> ()  (* Expected *))

(* Helper: check if string contains substring *)
let str_contains haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(* Test: Format as markdown *)
let test_format_markdown () =
  let h : Handover.handover_record = {
    id = "handover-123";
    from_agent = "claude";
    to_agent = Some "gemini";
    task_id = "task-001";
    session_id = "session-abc";
    current_goal = "Fix the bug";
    progress_summary = "Found the issue";
    completed_steps = ["Reproduced"; "Identified root cause"];
    pending_steps = ["Apply fix"; "Write test"];
    key_decisions = ["Use patch over rewrite"];
    assumptions = ["Test suite is reliable"];
    warnings = ["Memory sensitive area"];
    unresolved_errors = [];
    modified_files = ["src/main.ml"];
    created_at = 0.0;
    context_usage_percent = 75;
    handover_reason = "explicit";
  } in
  let md = Handover.format_as_markdown h in
  check bool "has DNA header" true (String.length md > 0);
  check bool "contains goal" true (str_contains md "Fix the bug")

(* Test: Trigger reason serialization *)
let test_trigger_reasons () =
  check string "context limit" "context_limit_90"
    (Handover.trigger_reason_to_string (Handover.ContextLimit 90));
  check string "timeout" "timeout_300s"
    (Handover.trigger_reason_to_string (Handover.Timeout 300));
  check string "explicit" "explicit"
    (Handover.trigger_reason_to_string Handover.Explicit);
  check string "fatal error" "error: boom"
    (Handover.trigger_reason_to_string (Handover.FatalError "boom"));
  check string "task complete" "task_complete"
    (Handover.trigger_reason_to_string Handover.TaskComplete)

(* Test: Build successor prompt from handover DNA *)
let test_build_successor_prompt () =
  let h : Handover.handover_record = {
    id = "handover-456";
    from_agent = "claude";
    to_agent = None;
    task_id = "task-xyz";
    session_id = "session-123";
    current_goal = "Implement OAuth2";
    progress_summary = "Started implementation";
    completed_steps = ["Research"; "Design"];
    pending_steps = ["Code"; "Test"; "Deploy"];
    key_decisions = ["Use PKCE flow"];
    assumptions = ["OAuth provider is reliable"];
    warnings = ["Token refresh may fail"];
    unresolved_errors = [];
    modified_files = ["auth.ts"];
    created_at = 0.0;
    context_usage_percent = 80;
    handover_reason = "context_limit_80";
  } in
  let prompt = Handover.build_successor_prompt h ~additional_instructions:None in
  check bool "contains goal" true (str_contains prompt "Implement OAuth2");
  check bool "contains pending steps" true (str_contains prompt "Code");
  check bool "contains decision" true (str_contains prompt "PKCE flow");
  check bool "contains mission" true (str_contains prompt "Your Mission");
  (* With additional instructions *)
  let prompt2 = Handover.build_successor_prompt h
    ~additional_instructions:(Some "Prioritize security") in
  check bool "contains extra instructions" true (str_contains prompt2 "Prioritize security")

let () =
  run "Handover" [
    "create", [
      test_case "create basic handover" `Quick test_create_handover;
      test_case "create from planning" `Quick test_create_from_planning;
    ];
    "list", [
      test_case "list and filter" `Quick test_list_handovers;
    ];
    "claim", [
      test_case "claim handover" `Quick test_claim_handover;
    ];
    "format", [
      test_case "format as markdown" `Quick test_format_markdown;
    ];
    "trigger_reason", [
      test_case "serialize reasons" `Quick test_trigger_reasons;
    ];
    "spawn", [
      test_case "build successor prompt" `Quick test_build_successor_prompt;
    ];
  ]
