(* Boxed tool execution: result projection followed by a feature scenario
   through the Gate and the production Execute dispatch selector. *)

open Alcotest
module Stage = Masc.Keeper_tool_execute_observe
module Target = Masc.Keeper_sandbox_shell_ir_target
module Gate = Masc.Keeper_gate

let observe_run = Keeper_types_profile_sandbox.Observe

let boxed () =
  Target.Boxed { target = Masc_exec.Sandbox_target.host (); run = observe_run }
;;

let process_result status =
  { Masc_exec.Exec_dispatch.status; stdout = "out"; stderr = "err" }
;;

let result status = Ok (process_result status)

let observation_label = function
  | Gate.Observed_result { run; result } ->
    Printf.sprintf "result:%s %s"
      (Keeper_types_profile_sandbox.observation_run_to_string run)
      result.stdout
  | Gate.Observed_refused { status = Unix.WEXITED code; stderr } ->
    Printf.sprintf "refused exit=%d stderr=%s" code stderr
  | Gate.Observed_refused { status = Unix.WSIGNALED signal; stderr } ->
    Printf.sprintf "refused signal=%d stderr=%s" signal stderr
  | Gate.Observed_refused { status = Unix.WSTOPPED signal; stderr } ->
    Printf.sprintf "refused stopped=%d stderr=%s" signal stderr
  | Gate.Observation_unavailable reason -> "unavailable " ^ reason
;;

let observation = testable (fun fmt o -> Format.pp_print_string fmt (observation_label o)) ( = )

(* Exit 0 is clean, and the result is kept for the caller: the gate that
   allows on it must not run the call again. *)
let test_a_clean_run_is_kept_for_the_caller () =
  let stage = Stage.create ~route:boxed ~dispatch:(fun _ -> result (Unix.WEXITED 0)) in
  check observation "clean, and it says which box"
    (Gate.Observed_result { run = observe_run; result = process_result (Unix.WEXITED 0) })
    (Stage.observe stage ());
  (* The box the route named is the box the gate is told about: a guest_local
     route answers guest_local, so the audit row can say so. *)
  let local =
    Stage.create
      ~route:(fun () ->
        Target.Boxed
          { target = Masc_exec.Sandbox_target.host ()
          ; run = Keeper_types_profile_sandbox.Guest_local
          })
      ~dispatch:(fun _ -> result (Unix.WEXITED 0))
  in
  check observation "guest_local travels"
    (Gate.Observed_result
       { run = Keeper_types_profile_sandbox.Guest_local
       ; result = process_result (Unix.WEXITED 0)
       })
    (Stage.observe local ());
  match Stage.outcome stage with
  | Some (Gate.Observed_result { result; _ }) ->
    check string "the run's output" "out" result.stdout
  | _ -> fail "a clean run left nothing for the caller"
;;

(* Other Observe statuses are refused with the stderr the box wrote,
   and nothing is kept: there is no result to return without the judge. *)
let test_a_non_zero_run_is_refused_with_its_stderr () =
  let stage = Stage.create ~route:boxed ~dispatch:(fun _ -> result (Unix.WEXITED 2)) in
  check observation "refused"
    (Gate.Observed_refused { status = Unix.WEXITED 2; stderr = "err" })
    (Stage.observe stage ());
  (* The refusal is readable afterwards, so the deferred receipt can tell the
     keeper what the box refused. *)
  check (option observation) "the outcome is remembered"
    (Some (Gate.Observed_refused { status = Unix.WEXITED 2; stderr = "err" }))
    (Stage.outcome stage);
  let signalled = Stage.create ~route:boxed ~dispatch:(fun _ -> result (Unix.WSIGNALED 15)) in
  check observation "a signal is refused too"
    (Gate.Observed_refused { status = Unix.WSIGNALED 15; stderr = "err" })
    (Stage.observe signalled ())
;;

(* No box means no dispatch: the reason travels, the fake dispatch is never
   reached. *)
let test_no_box_is_unavailable_without_dispatching () =
  let stage =
    Stage.create
      ~route:(fun () -> Target.No_box "docker_observe_unsupported: no shim")
      ~dispatch:(fun _ -> fail "dispatched with no box")
  in
  check observation "unavailable, in the route's words"
    (Gate.Observation_unavailable "docker_observe_unsupported: no shim")
    (Stage.observe stage ())
;;

(* A dispatch the typed gate refused before anything ran is unavailable
   under the gate's own closed tag, never read as clean or refused. *)
let test_a_refused_dispatch_is_unavailable_under_its_tag () =
  let stage =
    Stage.create
      ~route:boxed
      ~dispatch:(fun _ -> Error (Keeper_tooling.Execute_shell_ir.Gate_reject "x"))
  in
  check observation "gate_reject" (Gate.Observation_unavailable "gate_reject") (Stage.observe stage ());
  let path =
    Stage.create
      ~route:boxed
      ~dispatch:(fun _ -> Error (Keeper_tooling.Execute_shell_ir.Path_reject "y"))
  in
  check observation "path_reject" (Gate.Observation_unavailable "path_reject") (Stage.observe path ())
;;

(* Gate -> boxed dispatch -> the Execute owner's authorised-dispatch path.
   The injected route runs a real subprocess on a temporary host tree. This
   exercises settlement and replay selection, not Linux box enforcement:
   the failing child writes first and exits afterwards, just as a guest-local
   script can write before a denied network operation. *)
let with_execution_workspace f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Fs_compat.set_fs (Eio.Stdenv.fs env);
  Masc_test_deps.init_eio_clock env;
  Process_eio.init
    ~cwd_default:(Eio.Stdenv.cwd env)
    ~proc_mgr:(Eio.Stdenv.process_mgr env)
    ~clock:(Eio.Stdenv.clock env);
  let base_path = Filename.temp_file "guest-local-settlement-" "" in
  Unix.unlink base_path;
  Unix.mkdir base_path 0o700;
  let rec remove path =
    match Unix.lstat path with
    | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
    | _ -> Unix.unlink path
  in
  Eio.Switch.on_release sw (fun () ->
    Process_eio.reset_for_testing ();
    Masc.Keeper_approval_queue.For_testing.reset_runtime_state ();
    remove base_path);
  (match Masc.Keeper_approval_queue.install_persistence ~base_path with
   | Ok _ -> ()
   | Error error -> fail (Masc.Keeper_approval_queue.install_error_to_string error));
  let config = Masc.Workspace.default_config base_path in
  (match Masc.Keeper_gate_mode.set config ~actor:"test" Masc.Keeper_gate_mode.Auto_judge with
   | Ok _ -> ()
   | Error error -> fail error);
  f base_path
;;

let execute_through_gate
      ?(observed_secret_files = fun () -> [])
      ?(prepare_secret_files = fun () -> Ok [])
      ~run ~script base_path =
  let dispatch_count = ref 0 in
  let dispatch target =
    incr dispatch_count;
    let bin =
      match Masc_exec.Exec_program.of_string "sh" with
      | Ok bin -> bin
      | Error _ -> fail "sh is not an executable name"
    in
    let ir =
      Keeper_tooling.Execute_shell_ir.simple_bin
        ~cwd_raw:base_path ~sandbox:target bin [ "-c"; script ]
    in
    Keeper_tooling.Execute_shell_ir.dispatch ~workdir:base_path ~sandbox:target ir
  in
  let target = Masc_exec.Sandbox_target.host () in
  let stage = Stage.create ~route:(fun () -> Target.Boxed { target; run }) ~dispatch in
  let request : Gate.request =
    { keeper_name = "settlement"
    ; operation = "tool_execute"
    ; input = `Assoc [ "input", `Assoc [ "argv", `List [ `String "sh"; `String "-c"; `String script ] ] ]
    ; call_summary = None
    ; base_path
    ; causal_context = None
    ; task_id = None
    ; continuation_channel = None
    ; sandbox_profile = Some Keeper_types_profile_sandbox.Remote_ssh
    }
  in
  let decision = Gate.decide ~keeper_always_allow:false ~observe:(Stage.observe stage) request in
  let chunks = ref [] in
  let result =
    match decision with
    | Gate.Allow { source; _ } ->
      (match
         Masc.Keeper_tool_execute_runtime.For_testing.secret_files_for_source
           ~source ~observed:observed_secret_files ~prepare:prepare_secret_files
       with
       | Ok _ -> ()
       | Error error -> fail ("identity preparation replaced the execution result: " ^ error));
      (match
         Stage.dispatch_authorized ~source
           ~on_output_chunk:(fun chunk -> chunks := chunk :: !chunks)
           ~dispatch:(fun () -> dispatch target)
       with
       | Ok result -> Some result
       | Error _ -> fail "the real Execute dispatcher rejected the authorised request")
    | Gate.Deferred _ -> None
    | Gate.Unavailable _ -> fail "Gate persistence was unavailable"
  in
  let pending =
    match Masc.Keeper_approval_queue.pending_count_for_keeper_in_workspace
            ~base_path ~keeper_name:"settlement" with
    | Ok count -> count
    | Error error -> fail (Masc.Keeper_approval_queue.storage_error_to_string error)
  in
  decision, result, !dispatch_count, pending, List.rev !chunks
;;

let test_guest_local_failure_returns_the_execution_once () =
  with_execution_workspace @@ fun base_path ->
  let prepared = ref 0 in
  let observed = ref 0 in
  let _, result, count, pending, chunks =
    execute_through_gate ~run:Keeper_types_profile_sandbox.Guest_local
      ~observed_secret_files:(fun () -> incr observed; [ "bound-snapshot/hosts.yml" ])
      ~prepare_secret_files:(fun () -> incr prepared; Error "github_app_token_refresh_failed")
      ~script:"printf x >> append.txt; printf 'partial output\\n'; printf 'network refused\\n' >&2; exit 23"
      base_path
  in
  check int "the full command was dispatched once" 1 count;
  check int "no approval can replay the command" 0 pending;
  check int "the executed guest's identity is read once" 1 !observed;
  check int "a later token refresh cannot overwrite the result" 0 !prepared;
  let ic = open_in_bin (Filename.concat base_path "append.txt") in
  let appended = Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic)) in
  check string "the completed prefix appended once" "x" appended;
  match result with
  | None -> fail "guest-local failure was deferred instead of returned"
  | Some result ->
    check bool "the real exit status survives" true (result.status = Unix.WEXITED 23);
    check string "stdout survives" "partial output\n" result.stdout;
    check string "stderr survives" "network refused\n" result.stderr;
    check bool "Execute streams the same result without a second dispatch" true
      (chunks = [ `Stdout result.stdout; `Stderr result.stderr ])
;;

let test_successful_boxed_results_are_not_reexecuted () =
  List.iter
    (fun run ->
      with_execution_workspace @@ fun base_path ->
      let _, result, count, pending, _ =
        execute_through_gate ~run ~script:"printf 'read result\\n'" base_path
      in
      check int "one dispatch" 1 count;
      check int "no approval" 0 pending;
      match result with
      | Some result ->
        check bool "exit zero" true (result.status = Unix.WEXITED 0);
        check string "original output" "read result\n" result.stdout
      | None -> fail "successful boxed run was deferred")
    [ Keeper_types_profile_sandbox.Observe; Keeper_types_profile_sandbox.Guest_local ]
;;

let test_observe_failure_still_reaches_the_judge () =
  with_execution_workspace @@ fun base_path ->
  let decision, result, count, pending, chunks =
    execute_through_gate ~run:Keeper_types_profile_sandbox.Observe
      ~script:"printf 'write refused\\n' >&2; exit 23" base_path
  in
  check int "one observation dispatch" 1 count;
  check int "the Judge still has the request" 1 pending;
  check bool "no final result before judgment" true (Option.is_none result);
  check bool "refused observation is not streamed as a final result" true (chunks = []);
  match decision with
  | Gate.Deferred { approval_id; _ } ->
    (match Masc.Keeper_approval_queue.get_pending_entry_for_workspace ~base_path ~id:approval_id with
     | Ok (Some { observation = Some refusal; _ }) ->
       check string "Judge reads the refusal" "write refused\n" refusal.observed_stderr
     | _ -> fail "the pending approval lost the Observe refusal")
  | Gate.Allow _ | Gate.Unavailable _ -> fail "Observe failure did not reach the Judge"
;;

let test_manual_approval_does_not_prepare_identity () =
  with_execution_workspace @@ fun base_path ->
  (match
     Masc.Keeper_gate_mode.set (Masc.Workspace.default_config base_path)
       ~actor:"test" Masc.Keeper_gate_mode.Manual
   with
   | Ok _ -> ()
   | Error error -> fail error);
  let _, result, count, pending, _ =
    execute_through_gate ~run:Keeper_types_profile_sandbox.Guest_local
      ~observed_secret_files:(fun () -> fail "manual approval read an execution identity")
      ~prepare_secret_files:(fun () -> fail "manual approval refreshed an identity")
      ~script:"printf x >> append.txt" base_path
  in
  check int "not dispatched before approval" 0 count;
  check int "the original approval remains pending" 1 pending;
  check bool "no fabricated execution result" true (Option.is_none result)
;;

let () =
  run
    "keeper_tool_execute_observe"
    [ ( "stage"
      , [ test_case "a clean run is kept for the caller" `Quick test_a_clean_run_is_kept_for_the_caller
        ; test_case "a non-zero run is refused with its stderr" `Quick
            test_a_non_zero_run_is_refused_with_its_stderr
        ; test_case "no box is unavailable without dispatching" `Quick
            test_no_box_is_unavailable_without_dispatching
        ; test_case "a refused dispatch is unavailable under its tag" `Quick
            test_a_refused_dispatch_is_unavailable_under_its_tag
        ] )
    ; ( "execution-settlement"
      , [ test_case "guest-local failure returns its partial execution once" `Quick
            test_guest_local_failure_returns_the_execution_once
        ; test_case "successful boxed results are not reexecuted" `Quick
            test_successful_boxed_results_are_not_reexecuted
        ; test_case "Observe failure still reaches the Judge" `Quick
            test_observe_failure_still_reaches_the_judge
        ; test_case "manual approval does not prepare identity" `Quick
            test_manual_approval_does_not_prepare_identity
        ] )
    ]
;;
