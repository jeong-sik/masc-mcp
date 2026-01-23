(** masc_checkpoint - MASC Checkpoint Saver with Interrupt Support

    Save task checkpoint to Neo4j for workflow persistence.
    LangGraph pattern: state checkpointing for resumable workflows.

    Usage:
      # Create checkpoint
      masc-checkpoint --task-id task-019 --step 1 --action "스키마 설계" --agent claude

      # Interrupt (wait for approval)
      masc-checkpoint --task-id task-019 --step 2 --action "DB 삭제" --agent claude \
        --interrupt "정말 삭제하시겠습니까?"

      # Approve interrupted checkpoint
      masc-checkpoint --task-id task-019 --approve

      # Reject interrupted checkpoint
      masc-checkpoint --task-id task-019 --reject --reason "취소됨"

      # Resume from last checkpoint
      masc-checkpoint --task-id task-019 --resume

    Environment:
      NEO4J_URI - Connection URI
      NEO4J_PASSWORD - Password for neo4j user
      CLAUDE_SESSION_ID - Auto-detect session ID
      MASC_CHECKPOINT_BACKEND - neo4j|filesystem|auto (default: auto)
      MASC_DIR - Filesystem backend base dir for state (default: .masc)
*)

open Lwt.Syntax

(* Import from library - 코드 중복 제거 *)
open Masc_mcp.Checkpoint_types

(** Filesystem checkpoint store (fallback when Neo4j unavailable) *)
module Checkpoint_fs = Masc_mcp.Checkpoint_fs

(** Generate checkpoint ID with UUID suffix for collision avoidance *)
let make_checkpoint_id ~task_id ~step =
  let timestamp = int_of_float (Unix.time ()) in
  let random_suffix = Random.int 10000 in  (* 충돌 방지 *)
  Printf.sprintf "cp-%s-%d-%d-%04d" task_id step timestamp random_suffix

(** Initialize random seed *)
let () = Random.self_init ()

(** Select checkpoint backend.
    - env MASC_CHECKPOINT_BACKEND=neo4j|filesystem|auto
    - default: auto (prefer neo4j if configured, fallback to filesystem) *)
type checkpoint_backend =
  | Auto
  | Neo4j
  | FileSystem

let backend_from_env () =
  match Sys.getenv_opt "MASC_CHECKPOINT_BACKEND" with
  | None -> Auto
  | Some raw ->
      (match String.lowercase_ascii (String.trim raw) with
       | "neo4j" -> Neo4j
       | "filesystem" | "fs" -> FileSystem
       | "auto" | "" -> Auto
       | _ -> Auto)

let neo4j_configured () =
  match Sys.getenv_opt "NEO4J_URI" with
  | Some uri when String.trim uri <> "" -> true
  | _ -> false

let is_connect_error msg =
  let prefix = "Connect failed:" in
  let plen = String.length prefix in
  String.length msg >= plen && String.sub msg 0 plen = prefix

(** Save checkpoint to Neo4j with status support *)
let save_checkpoint ~task_id ~step ~action ~agent
    ?(state="{}") ?(total_steps=0) ?(session_id="")
    ?(status=Completed) ?(interrupt_message="") () =

  (* 입력 검증 - Critical Fix #2 *)
  if not (is_valid_task_id task_id) then
    Lwt.return (Error "Invalid task_id: must be alphanumeric with dashes/underscores, max 100 chars")
  else if not (is_valid_step step) then
    Lwt.return (Error "Invalid step: must be positive integer")
  else if not (is_valid_json_state state) then
    Lwt.return (Error "Invalid state: must be valid JSON")
  else begin
    let checkpoint_id = make_checkpoint_id ~task_id ~step in
    let status_str = status_to_string status in

    (* Connect to Neo4j *)
    let* conn_result = Neo4j_bolt.Bolt.connect () in
    match conn_result with
    | Error e ->
        let msg = Printf.sprintf "Connect failed: %s" (Neo4j_bolt.Bolt.error_to_string e) in
        Lwt.return (Error msg)
    | Ok conn ->
        (* Lwt.finalize로 연결 누수 방지 - Critical Fix #4 *)
        Lwt.finalize (fun () ->
      (* 1. Create Checkpoint node with status *)
      let cypher = {|
        CREATE (c:Checkpoint {
          id: $id,
          task_id: $task_id,
          step: $step,
          total_steps: $total_steps,
          action: $action,
          state: $state,
          agent: $agent,
          session_id: $session_id,
          status: $status,
          interrupt_message: $interrupt_message,
          date: date($date),
          ts: datetime()
        })
      |} in
      let params = `Assoc [
        ("id", `String checkpoint_id);
        ("task_id", `String task_id);
        ("step", `Int step);
        ("total_steps", `Int total_steps);
        ("action", `String action);
        ("state", `String state);
        ("agent", `String agent);
        ("session_id", `String session_id);
        ("status", `String status_str);
        ("interrupt_message", `String interrupt_message);
        ("date", `String (today ()));
      ] in
      let* _ = Neo4j_bolt.Bolt.query conn ~cypher ~params () in

      (* 2. Link to MascTask (create if not exists) *)
      let cypher = {|
        MERGE (t:MascTask {id: $task_id})
        WITH t
        MATCH (c:Checkpoint {id: $checkpoint_id})
        MERGE (t)-[:HAS_STEP]->(c)
      |} in
      let params = `Assoc [
        ("task_id", `String task_id);
        ("checkpoint_id", `String checkpoint_id);
      ] in
      let* _ = Neo4j_bolt.Bolt.query conn ~cypher ~params () in

      (* 3. Link to Agent (create if not exists) *)
      let cypher = {|
        MERGE (a:Agent {name: $agent})
        WITH a
        MATCH (c:Checkpoint {id: $checkpoint_id})
        MERGE (a)-[:EXECUTED]->(c)
      |} in
      let params = `Assoc [
        ("agent", `String agent);
        ("checkpoint_id", `String checkpoint_id);
      ] in
      let* _ = Neo4j_bolt.Bolt.query conn ~cypher ~params () in

      (* 4. Link to previous checkpoint (NEXT relationship) *)
      let* () =
        if step > 1 then begin
          let cypher = {|
            MATCH (prev:Checkpoint {task_id: $task_id})
            WHERE prev.step = $prev_step
            MATCH (curr:Checkpoint {id: $checkpoint_id})
            MERGE (prev)-[:NEXT]->(curr)
          |} in
          let params = `Assoc [
            ("task_id", `String task_id);
            ("prev_step", `Int (step - 1));
            ("checkpoint_id", `String checkpoint_id);
          ] in
          let* _ = Neo4j_bolt.Bolt.query conn ~cypher ~params () in
          Lwt.return_unit
        end else Lwt.return_unit
      in

      (* 5. Link to Session if provided *)
      let* () =
        if session_id <> "" then begin
          let cypher = {|
            MATCH (s:Session {session_id: $session_id})
            MATCH (c:Checkpoint {id: $checkpoint_id})
            MERGE (s)-[:HAS_CHECKPOINT]->(c)
          |} in
          let params = `Assoc [
            ("session_id", `String session_id);
            ("checkpoint_id", `String checkpoint_id);
          ] in
          let* _ = Neo4j_bolt.Bolt.query conn ~cypher ~params () in
          Lwt.return_unit
        end else Lwt.return_unit
      in

      Lwt.return (Ok checkpoint_id)
        ) (fun () -> Neo4j_bolt.Bolt.close conn)  (* finalize: 항상 연결 닫기 *)
  end  (* else begin 닫기 *)

(** Time Travel: Revert to a specific checkpoint step
    Returns the state from that checkpoint for replay *)
let revert_to_checkpoint ~task_id ~target_step =
  let* conn_result = Neo4j_bolt.Bolt.connect () in
  match conn_result with
  | Error e ->
      let msg = Printf.sprintf "Connect failed: %s" (Neo4j_bolt.Bolt.error_to_string e) in
      Lwt.return (Error msg)
  | Ok conn ->
      Lwt.finalize (fun () ->
        (* 1. Get target checkpoint state *)
        let cypher = {|
          MATCH (c:Checkpoint {task_id: $task_id, step: $step})
          RETURN c.id as id, c.step as step, c.action as action,
                 c.state as state, c.agent as agent, c.status as status
        |} in
        let params = `Assoc [
          ("task_id", `String task_id);
          ("step", `Int target_step);
        ] in
        let* result = Neo4j_bolt.Bolt.query conn ~cypher ~params () in

        match result with
        | Ok (`Assoc [("records", `List records)]) when List.length records > 0 ->
            (* 2. Mark checkpoints after target_step as reverted *)
            let cypher_revert = {|
              MATCH (c:Checkpoint {task_id: $task_id})
              WHERE c.step > $step
              SET c.status = 'reverted',
                  c.reverted_at = datetime()
              RETURN count(c) as reverted_count
            |} in
            let* _ = Neo4j_bolt.Bolt.query conn ~cypher:cypher_revert ~params () in

            (* Return the target checkpoint state *)
            (match List.hd records with
            | `List [`List values] when List.length values >= 6 ->
                let result_json = `Assoc [
                  ("id", List.nth values 0);
                  ("step", List.nth values 1);
                  ("action", List.nth values 2);
                  ("state", List.nth values 3);
                  ("agent", List.nth values 4);
                  ("status", List.nth values 5);
                  ("time_traveled", `Bool true);
                ] in
                Lwt.return (Ok result_json)
            | _ -> Lwt.return (Error "Invalid checkpoint format"))
        | Ok _ -> Lwt.return (Error (Printf.sprintf "Checkpoint step %d not found" target_step))
        | Error e -> Lwt.return (Error (Neo4j_bolt.Bolt.error_to_string e))
      ) (fun () -> Neo4j_bolt.Bolt.close conn)

(** State Edit: Update state during approve (for human-in-the-loop modifications) *)
let approve_with_state_edit ~task_id ~new_state =
  if not (is_valid_json_state new_state) then
    Lwt.return (Error "Invalid state: must be valid JSON")
  else begin
    let* conn_result = Neo4j_bolt.Bolt.connect () in
    match conn_result with
    | Error e ->
        let msg = Printf.sprintf "Connect failed: %s" (Neo4j_bolt.Bolt.error_to_string e) in
        Lwt.return (Error msg)
    | Ok conn ->
        Lwt.finalize (fun () ->
          let cypher = {|
            MATCH (c:Checkpoint {task_id: $task_id, status: 'interrupted'})
            SET c.status = 'completed',
                c.state = $new_state,
                c.state_edited = true,
                c.resolved_at = datetime()
            RETURN c.id as id, c.step as step, c.state as state
          |} in
          let params = `Assoc [
            ("task_id", `String task_id);
            ("new_state", `String new_state);
          ] in
          let* result = Neo4j_bolt.Bolt.query conn ~cypher ~params () in

          match result with
          | Ok (`Assoc [("records", `List records)]) when List.length records > 0 ->
              Lwt.return (Ok "Approved with state edit")
          | Ok _ ->
              Lwt.return (Error "No interrupted checkpoint found")
          | Error e ->
              Lwt.return (Error (Neo4j_bolt.Bolt.error_to_string e))
        ) (fun () -> Neo4j_bolt.Bolt.close conn)
  end

(** Update checkpoint status (for approve/reject) *)
let update_checkpoint_status ~task_id ~new_status ?(reason="") () =
  (* Critical Fix #3: State transition validation *)
  (* approve/reject는 Interrupted → Completed/Rejected 전이만 허용 *)
  if not (can_transition ~from:Interrupted ~to_:new_status) then
    Lwt.return (Error (Printf.sprintf "Invalid state transition: interrupted → %s"
      (status_to_string new_status)))
  else begin
    let* conn_result = Neo4j_bolt.Bolt.connect () in
    match conn_result with
    | Error e ->
        let msg = Printf.sprintf "Connect failed: %s" (Neo4j_bolt.Bolt.error_to_string e) in
        Lwt.return (Error msg)
    | Ok conn ->
        (* Lwt.finalize로 연결 누수 방지 *)
        Lwt.finalize (fun () ->
          let cypher = {|
            MATCH (c:Checkpoint {task_id: $task_id, status: 'interrupted'})
            SET c.status = $new_status,
                c.resolved_at = datetime(),
                c.reject_reason = $reason
            RETURN c.id as id, c.step as step, c.action as action
          |} in
          let params = `Assoc [
            ("task_id", `String task_id);
            ("new_status", `String (status_to_string new_status));
            ("reason", `String reason);
          ] in
          let* result = Neo4j_bolt.Bolt.query conn ~cypher ~params () in

          match result with
          | Ok (`Assoc [("records", `List records)]) when List.length records > 0 ->
              Lwt.return (Ok "Updated")
          | Ok _ ->
              Lwt.return (Error "No interrupted checkpoint found")
          | Error e ->
              Lwt.return (Error (Neo4j_bolt.Bolt.error_to_string e))
        ) (fun () -> Neo4j_bolt.Bolt.close conn)
  end

(** Get last checkpoint for resume *)
let get_last_checkpoint ~task_id =
  let* conn_result = Neo4j_bolt.Bolt.connect () in
  match conn_result with
  | Error e ->
      let msg = Printf.sprintf "Connect failed: %s" (Neo4j_bolt.Bolt.error_to_string e) in
      Lwt.return (Error msg)
  | Ok conn ->
      (* Lwt.finalize로 연결 누수 방지 *)
      Lwt.finalize (fun () ->
        let cypher = {|
          MATCH (t:MascTask {id: $task_id})-[:HAS_STEP]->(c:Checkpoint)
          RETURN c.id as id, c.step as step, c.action as action,
                 c.state as state, c.agent as agent, c.status as status,
                 c.interrupt_message as interrupt_message
          ORDER BY c.step DESC
          LIMIT 1
        |} in
        let params = `Assoc [("task_id", `String task_id)] in
        let* result = Neo4j_bolt.Bolt.query conn ~cypher ~params () in

        match result with
        | Ok (`Assoc [("records", `List records)]) when List.length records > 0 ->
            (match List.hd records with
            | `List [`List values] when List.length values >= 7 ->
                let result_json = `Assoc [
                  ("id", List.nth values 0);
                  ("step", List.nth values 1);
                  ("action", List.nth values 2);
                  ("state", List.nth values 3);
                  ("agent", List.nth values 4);
                  ("status", List.nth values 5);
                  ("interrupt_message", List.nth values 6);
                ] in
                Lwt.return (Ok (Some result_json))
            | _ -> Lwt.return (Ok None))
        | Ok _ -> Lwt.return (Ok None)
        | Error e -> Lwt.return (Error (Neo4j_bolt.Bolt.error_to_string e))
      ) (fun () -> Neo4j_bolt.Bolt.close conn)

(** Debug flag - set via MASC_DEBUG=1 environment variable *)
let debug_enabled = try Sys.getenv "MASC_DEBUG" = "1" with Not_found -> false

(** Auto-reject timed out interrupts (uses existing connection) *)
let auto_reject_timed_out_with_conn conn ~timeout_minutes =
  (* Use parameterized query for better security habits *)
  let cypher = {|
    MATCH (c:Checkpoint {status: 'interrupted'})
    WHERE c.ts < datetime() - duration('PT' + toString($timeout) + 'M')
    SET c.status = 'rejected',
        c.resolved_at = datetime(),
        c.reject_reason = 'Timeout: no response in ' + toString($timeout) + ' minutes'
    RETURN count(c) as rejected_count
  |} in
  let params = `Assoc [("timeout", `Int timeout_minutes)] in
  let* result = Neo4j_bolt.Bolt.query conn ~cypher ~params () in

  (* Parse count from result - debug logs only when MASC_DEBUG=1 *)
  match result with
  | Ok (`Assoc [("records", `List records)]) when List.length records > 0 ->
      (match List.hd records with
      | `List [`List [`Int count]] -> Lwt.return count
      | unexpected ->
          if debug_enabled then
            Printf.eprintf "[DEBUG] Unexpected record format: %s\n"
              (Yojson.Safe.to_string unexpected);
          Lwt.return 0)
  | Ok unexpected ->
      if debug_enabled then
        Printf.eprintf "[DEBUG] Unexpected result format: %s\n"
          (Yojson.Safe.to_string unexpected);
      Lwt.return 0
  | Error e ->
      if debug_enabled then
        Printf.eprintf "[DEBUG] Auto-reject query failed: %s\n"
          (Neo4j_bolt.Bolt.error_to_string e);
      Lwt.return 0

(** Get pending interrupts (with auto-cleanup of timed out ones)
    Single connection used for both operations (MAGI review fix) *)
let get_pending_interrupts ?(timeout_minutes=30) () =
  let* conn_result = Neo4j_bolt.Bolt.connect () in
  match conn_result with
  | Error e ->
      let msg = Printf.sprintf "Connect failed: %s" (Neo4j_bolt.Bolt.error_to_string e) in
      Lwt.return (Error msg)
  | Ok conn ->
      (* Lwt.finalize로 연결 누수 방지 *)
      Lwt.finalize (fun () ->
        (* First, auto-reject timed out interrupts (reusing connection) *)
        let* rejected_count = auto_reject_timed_out_with_conn conn ~timeout_minutes in
        if rejected_count > 0 then
          Printf.eprintf "⏰ Auto-rejected %d timed out interrupt(s)\n" rejected_count;

        (* Then fetch remaining pending interrupts *)
        let cypher = {|
          MATCH (c:Checkpoint {status: 'interrupted'})
          RETURN c.id as id, c.task_id as task_id, c.step as step,
                 c.action as action, c.interrupt_message as message,
                 toString(c.ts) as ts
          ORDER BY c.ts DESC
        |} in
        let* result = Neo4j_bolt.Bolt.query conn ~cypher ~params:(`Assoc []) () in

        match result with
        | Ok json -> Lwt.return (Ok json)
        | Error e -> Lwt.return (Error (Neo4j_bolt.Bolt.error_to_string e))
      ) (fun () -> Neo4j_bolt.Bolt.close conn)

(** Branch from an existing checkpoint - creates new execution path *)
let branch_from_checkpoint ~task_id ~source_step ~branch_name ~agent =
  if not (is_valid_task_id task_id) then
    Lwt.return (Error "Invalid task_id")
  else if branch_name = "" then
    Lwt.return (Error "Branch name is required")
  else begin
    let* conn_result = Neo4j_bolt.Bolt.connect () in
    match conn_result with
    | Error e ->
        let msg = Printf.sprintf "Connect failed: %s" (Neo4j_bolt.Bolt.error_to_string e) in
        Lwt.return (Error msg)
    | Ok conn ->
        Lwt.finalize (fun () ->
          (* 1. Find source checkpoint and get its state *)
          let find_cypher = {|
            MATCH (t:MascTask {id: $task_id})-[:HAS_STEP]->(c:Checkpoint {step: $step})
            RETURN c.id as id, c.state as state, c.action as action
          |} in
          let find_params = `Assoc [
            ("task_id", `String task_id);
            ("step", `Int source_step);
          ] in
          let* find_result = Neo4j_bolt.Bolt.query conn ~cypher:find_cypher ~params:find_params () in

          match find_result with
          | Error e ->
              Lwt.return (Error (Neo4j_bolt.Bolt.error_to_string e))
          | Ok (`Assoc [("records", `List records)]) when List.length records > 0 ->
              let source_id, source_state, source_action =
                match List.hd records with
                | `List [`List values] when List.length values >= 3 ->
                    let id = match List.nth values 0 with `String s -> s | _ -> "" in
                    let state = match List.nth values 1 with `String s -> s | _ -> "{}" in
                    let action = match List.nth values 2 with `String s -> s | _ -> "" in
                    (id, state, action)
                | _ -> ("", "{}", "")
              in

              if source_id = "" then
                Lwt.return (Error (Printf.sprintf "Checkpoint at step %d not found" source_step))
              else begin
                (* 2. Mark source checkpoint as branched *)
                let mark_cypher = {|
                  MATCH (c:Checkpoint {id: $source_id})
                  SET c.status = 'branched',
                      c.branched_at = datetime()
                  RETURN c.id
                |} in
                let mark_params = `Assoc [("source_id", `String source_id)] in
                let* _ = Neo4j_bolt.Bolt.query conn ~cypher:mark_cypher ~params:mark_params () in

                (* 3. Create new branch checkpoint *)
                let new_id = make_checkpoint_id ~task_id ~step:(source_step + 1) in
                let branch_cypher = {|
                  MATCH (t:MascTask {id: $task_id})
                  MATCH (parent:Checkpoint {id: $parent_id})
                  CREATE (c:Checkpoint {
                    id: $id,
                    task_id: $task_id,
                    step: $step,
                    action: $action,
                    state: $state,
                    agent: $agent,
                    status: 'pending',
                    parent_checkpoint_id: $parent_id,
                    branch_name: $branch_name,
                    ts: datetime()
                  })
                  CREATE (t)-[:HAS_STEP]->(c)
                  CREATE (parent)-[:BRANCHED_TO {branch_name: $branch_name, ts: datetime()}]->(c)
                  RETURN c.id as new_id
                |} in
                let branch_params = `Assoc [
                  ("id", `String new_id);
                  ("task_id", `String task_id);
                  ("step", `Int (source_step + 1));
                  ("action", `String (Printf.sprintf "[Branch: %s] from %s" branch_name source_action));
                  ("state", `String source_state);
                  ("agent", `String agent);
                  ("parent_id", `String source_id);
                  ("branch_name", `String branch_name);
                ] in
                let* branch_result = Neo4j_bolt.Bolt.query conn ~cypher:branch_cypher ~params:branch_params () in

                match branch_result with
                | Ok _ ->
                    let result_json = `Assoc [
                      ("branch_id", `String new_id);
                      ("parent_id", `String source_id);
                      ("branch_name", `String branch_name);
                      ("step", `Int (source_step + 1));
                      ("state", `String source_state);
                    ] in
                    Lwt.return (Ok result_json)
                | Error e ->
                    Lwt.return (Error (Neo4j_bolt.Bolt.error_to_string e))
              end
          | Ok _ ->
              Lwt.return (Error (Printf.sprintf "Checkpoint at step %d not found" source_step))
        ) (fun () -> Neo4j_bolt.Bolt.close conn)
  end

(** Parse command line arguments *)
let parse_args () =
  let default_masc_dir =
    match Sys.getenv_opt "MASC_DIR" with
    | Some d when String.trim d <> "" -> d
    | _ -> ".masc"
  in
  let masc_dir = ref default_masc_dir in
  let task_id = ref "" in
  let step = ref 0 in
  let action = ref "" in
  let agent = ref "" in
  let state = ref "{}" in
  let total_steps = ref 0 in
  let session_id = ref "" in
  let mode = ref "create" in
  let interrupt_msg = ref "" in
  let reason = ref "" in
  let timeout = ref 30 in  (* Default 30 minutes *)
  let revert_step = ref 0 in  (* Time Travel target step *)
  let edit_state = ref "" in  (* State Edit: new state JSON *)
  let branch_name = ref "" in  (* Checkpoint Branching *)
  let branch_step = ref 0 in   (* Branch source step *)

  let status_str = ref "" in

  let specs = [
    ("--masc-dir", Arg.Set_string masc_dir, "Path to .masc directory (default: $MASC_DIR or .masc)");
    ("--task-id", Arg.Set_string task_id, "Task ID (e.g., task-019)");
    ("--step", Arg.Set_int step, "Step number (1-based)");
    ("--action", Arg.Set_string action, "Action description");
    ("--agent", Arg.Set_string agent, "Agent name (e.g., claude)");
    ("--state", Arg.Set_string state, "JSON state (optional)");
    ("--total-steps", Arg.Set_int total_steps, "Total steps (optional)");
    ("--session-id", Arg.Set_string session_id, "Session ID (optional)");
    ("--status", Arg.Set_string status_str, "Initial status (pending, in_progress, completed)");
    ("--resume", Arg.Unit (fun () -> mode := "resume"), "Get last checkpoint for resume");
    ("--interrupt", Arg.String (fun msg -> mode := "interrupt"; interrupt_msg := msg), "Interrupt with message (wait for approval)");
    ("--approve", Arg.Unit (fun () -> mode := "approve"), "Approve interrupted checkpoint");
    ("--reject", Arg.Unit (fun () -> mode := "reject"), "Reject interrupted checkpoint");
    ("--reason", Arg.Set_string reason, "Rejection reason");
    ("--pending", Arg.Unit (fun () -> mode := "pending"), "List pending interrupts");
    ("--timeout", Arg.Set_int timeout, "Timeout in minutes for auto-reject (default: 30)");
    (* LangGraph 핵심 기능 추가 *)
    ("--revert-to", Arg.Int (fun s -> mode := "revert"; revert_step := s), "Time Travel: Revert to step N");
    ("--edit-state", Arg.Set_string edit_state, "State Edit: New JSON state for approve");
    (* Checkpoint Branching *)
    ("--branch", Arg.Int (fun s -> mode := "branch"; branch_step := s), "Branch from step N");
    ("--branch-name", Arg.Set_string branch_name, "Name for the new branch");
  ] in

  Arg.parse specs (fun _ -> ()) "masc-checkpoint: Save/Resume MASC checkpoints with interrupt support";

  (!mode, !masc_dir, !task_id, !step, !action, !agent, !state, !total_steps, !session_id, !interrupt_msg, !reason, !status_str, !timeout, !revert_step, !edit_state, !branch_name, !branch_step)

(** Main entry point *)
let () =
  let (mode, masc_dir, task_id, step, action, agent, state, total_steps, session_id_arg, interrupt_msg, reason, status_str, timeout, revert_step, edit_state, branch_name, branch_step) = parse_args () in

  (* Auto-detect session_id: CLI arg > CLAUDE_SESSION_ID env var *)
  let session_id =
    if session_id_arg <> "" then session_id_arg
    else try Sys.getenv "CLAUDE_SESSION_ID" with Not_found -> ""
  in

  let backend = backend_from_env () in

  let run_with_backend neo4j fs =
    match backend with
    | FileSystem -> fs ()
    | Neo4j -> neo4j ()
    | Auto ->
        if neo4j_configured () then
          let* res = neo4j () in
          (match res with
           | Error msg when is_connect_error msg -> fs ()
           | _ -> Lwt.return res)
        else
          fs ()
  in

  Lwt_main.run begin
    match mode with
    (* Time Travel: Revert to previous checkpoint step *)
    | "revert" ->
        if task_id = "" then begin
          prerr_endline "Error: --task-id is required for revert";
          exit 1
        end;
        let* result =
          run_with_backend
            (fun () -> revert_to_checkpoint ~task_id ~target_step:revert_step)
            (fun () -> Checkpoint_fs.revert_to_checkpoint ~masc_dir ~task_id ~target_step:revert_step)
        in
        (match result with
        | Ok json ->
            Printf.printf "⏪ Time Travel to step %d:\n" revert_step;
            print_endline (Yojson.Safe.pretty_to_string json)
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            exit 1);
        Lwt.return_unit

    | "resume" ->
        if task_id = "" then begin
          prerr_endline "Error: --task-id is required for resume";
          exit 1
        end;
        let* result =
          run_with_backend
            (fun () -> get_last_checkpoint ~task_id)
            (fun () -> Checkpoint_fs.get_last_checkpoint ~masc_dir ~task_id)
        in
        (match result with
        | Ok (Some json) ->
            print_endline (Yojson.Safe.pretty_to_string json)
        | Ok None ->
            print_endline "null"
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            exit 1);
        Lwt.return_unit

    | "interrupt" ->
        if task_id = "" || step = 0 || action = "" || agent = "" then begin
          prerr_endline "Error: --task-id, --step, --action, --agent are required for interrupt";
          exit 1
        end;
        let* result =
          run_with_backend
            (fun () ->
              save_checkpoint ~task_id ~step ~action ~agent ~state ~total_steps ~session_id
                ~status:Interrupted ~interrupt_message:interrupt_msg ())
            (fun () ->
              Checkpoint_fs.save_checkpoint ~masc_dir ~task_id ~step ~action ~agent ~state
                ~total_steps ~session_id ~status:Interrupted ~interrupt_message:interrupt_msg ())
        in
        (match result with
        | Ok checkpoint_id ->
            Printf.printf "⏸️ Interrupted: %s\n" checkpoint_id;
            Printf.printf "   Message: %s\n" interrupt_msg;
            Printf.printf "   Use --approve or --reject to continue\n"
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            exit 1);
        Lwt.return_unit

    | "approve" ->
        if task_id = "" then begin
          prerr_endline "Error: --task-id is required for approve";
          exit 1
        end;
        (* State Edit: if --edit-state provided, use approve_with_state_edit *)
        let* result =
          if edit_state <> "" then
            run_with_backend
              (fun () -> approve_with_state_edit ~task_id ~new_state:edit_state)
              (fun () -> Checkpoint_fs.approve_with_state_edit ~masc_dir ~task_id ~new_state:edit_state)
          else
            run_with_backend
              (fun () -> update_checkpoint_status ~task_id ~new_status:Completed ())
              (fun () -> Checkpoint_fs.update_checkpoint_status ~masc_dir ~task_id ~new_status:Completed ())
        in
        (match result with
        | Ok _ ->
            if edit_state <> "" then
              Printf.printf "✅ Approved with state edit: %s\n" task_id
            else
              Printf.printf "✅ Approved: %s\n" task_id
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            exit 1);
        Lwt.return_unit

    | "reject" ->
        if task_id = "" then begin
          prerr_endline "Error: --task-id is required for reject";
          exit 1
        end;
        let* result =
          run_with_backend
            (fun () -> update_checkpoint_status ~task_id ~new_status:Rejected ~reason ())
            (fun () -> Checkpoint_fs.update_checkpoint_status ~masc_dir ~task_id ~new_status:Rejected ~reason ())
        in
        (match result with
        | Ok _ ->
            Printf.printf "❌ Rejected: %s\n" task_id;
            if reason <> "" then Printf.printf "   Reason: %s\n" reason
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            exit 1);
        Lwt.return_unit

    | "pending" ->
        let* result =
          run_with_backend
            (fun () -> get_pending_interrupts ~timeout_minutes:timeout ())
            (fun () -> Checkpoint_fs.get_pending_interrupts ~masc_dir ~timeout_minutes:timeout ())
        in
        (match result with
        | Ok json ->
            print_endline (Yojson.Safe.pretty_to_string json)
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            exit 1);
        Lwt.return_unit

    | "branch" ->
        if task_id = "" then begin
          prerr_endline "Error: --task-id is required for branch";
          exit 1
        end;
        if branch_step = 0 then begin
          prerr_endline "Error: --branch N requires step number";
          exit 1
        end;
        if branch_name = "" then begin
          prerr_endline "Error: --branch-name is required";
          exit 1
        end;
        let agent_name = if agent = "" then "unknown" else agent in
        let* result =
          run_with_backend
            (fun () -> branch_from_checkpoint ~task_id ~source_step:branch_step ~branch_name ~agent:agent_name)
            (fun () -> Checkpoint_fs.branch_from_checkpoint ~masc_dir ~task_id ~source_step:branch_step ~branch_name ~agent:agent_name)
        in
        (match result with
        | Ok json ->
            Printf.printf "🌿 Branch created:\n";
            print_endline (Yojson.Safe.pretty_to_string json)
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            exit 1);
        Lwt.return_unit

    | "create" | _ ->
        if task_id = "" then begin
          prerr_endline "Error: --task-id is required";
          exit 1
        end;
        if step = 0 || action = "" || agent = "" then begin
          prerr_endline "Error: --step, --action, --agent are required for create";
          exit 1
        end;
        let status = if status_str = "" then Completed
                     else match status_of_string status_str with
                          | Some s -> s
                          | None -> Printf.eprintf "Warning: Unknown status '%s', using 'completed'\n" status_str; Completed
        in
        let* result =
          run_with_backend
            (fun () -> save_checkpoint ~task_id ~step ~action ~agent ~state ~total_steps ~session_id ~status ())
            (fun () -> Checkpoint_fs.save_checkpoint ~masc_dir ~task_id ~step ~action ~agent ~state ~total_steps ~session_id ~status ())
        in
        (match result with
        | Ok checkpoint_id ->
            Printf.printf "✅ Created: %s\n" checkpoint_id
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            exit 1);
        Lwt.return_unit
  end
