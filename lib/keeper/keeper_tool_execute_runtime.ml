open Keeper_types
open Keeper_meta_contract
open Keeper_types_profile
open Keeper_tool_shared_runtime

let elapsed_duration_ms ~start_time ~end_time =
  let elapsed_ms = (end_time -. start_time) *. 1000. in
  match classify_float elapsed_ms with
  | FP_nan | FP_infinite -> 0
  | _ when elapsed_ms <= 0. -> 0
  | _ when elapsed_ms < 1. -> 1
  | _ -> int_of_float elapsed_ms

let model_execute_location_fields ~config ~meta ~args ~cwd =
  let execution_location =
    Keeper_sandbox_repo_path.execution_location_json ~config ~meta ~args ~cwd
  in
  let response_cwd =
    match execution_location with
    | `Assoc fields ->
      (match List.assoc_opt "cwd" fields with
       | Some cwd -> cwd
       | None -> `Null)
    | _ -> `Null
  in
  [ "cwd", response_cwd; "execution_location", execution_location ]

let model_execute_cwd_resolution_error ~config ~meta ~args ~cwd error =
  let code = Keeper_tool_execute_path.execute_cwd_resolution_error_code error in
  let private_message =
    Keeper_tool_execute_path.execute_cwd_resolution_error_private_message error
  in
  Log.Keeper.warn
    ~keeper_name:meta.name
    "execute cwd resolution rejected code=%s detail=%s"
    code
    private_message;
  let fields =
    [ "typed", `Bool true; "code", `String code ]
    @ model_execute_location_fields ~config ~meta ~args ~cwd
  in
  let message =
    match meta.sandbox_profile with
    | Docker ->
      Keeper_tool_execute_path.execute_cwd_resolution_error_public_message error
    | _ -> private_message
  in
  Keeper_tool_execution.failure
    ~class_:Tool_result.Policy_rejection
    ~effect_disposition:Tool_result.Proven_pre_effect
    (error_json ~fields message)

let sandbox_target_label = function
  | Masc_exec.Sandbox_target.Host -> "host"
  | Masc_exec.Sandbox_target.Docker { image; _ } -> "docker:" ^ image
  | Masc_exec.Sandbox_target.Micro_vm { image; _ } -> "microvm:" ^ image
  | Masc_exec.Sandbox_target.Ssh { endpoint; _ } -> "ssh:" ^ endpoint.host
  | Masc_exec.Sandbox_target.Delegated _ -> "delegated"
;;

(* The Gate operation name this runtime submits under. Shared with the replay
   path so the two cannot drift apart into an unsupported-replay repair. *)
let gate_operation = Keeper_gate.tool_execute_gate_operation

let execute_gate_input ~input ~cwd ~sandbox_profile ~sandbox_target =
  `Assoc
    [ "schema", `String "masc.keeper_gate.request.v1"
    ; "input", input
    ; "cwd", `String cwd
    ; "sandbox_profile", `String sandbox_profile
    ; "sandbox_target", `String sandbox_target
    ]
;;

(* Inverse of [execute_gate_input]. The producer owns the argument schema and
   the effect encoding, so it owns the inversion — replay only decides whether
   to spend the grant.

   Unlike the write path, nothing is reconstructed: the approved tool
   arguments were stored verbatim under [input], because the Gate request
   wraps them with execution context rather than re-encoding them. The
   surrounding [cwd]/[sandbox_*] fields describe where the approval was
   granted; the handler re-derives them from the current turn, and a
   divergence there fails the canonical-input match rather than silently
   executing somewhere else. *)
let replay_args_of_gate_input input =
  match input with
  | `Assoc fields ->
    (match List.assoc_opt "input" fields with
     | Some args -> Ok args
     | None -> Error "approved Gate input has no input")
  | _ -> Error "approved Gate input is not an object"
;;

let execute_secret_redaction
      ~additional_secret_files
      ~base_path
      ~keeper_name
  =
  Keeper_secret_redaction.snapshot_with_additional_secret_files
    ~redact_identity_scalars:
      (Runtime_params.get Runtime_settings.keeper_chat_redact_identity_scalars)
    ~additional_secret_files
    ~base_path
    ~keeper_name

let redact_execute_text redaction text =
  Keeper_secret_redaction.redact_text redaction text

let redact_execute_output redaction ~stdout ~stderr =
  let stdout = redact_execute_text redaction stdout in
  let stderr = redact_execute_text redaction stderr in
  let output =
    if String.equal stderr "" then stdout else stdout ^ stderr
  in
  stdout, stderr, output

let composable_output_fields ~base_path ~stdout ~stderr ~output =
  if String.length output <= Tool_bridge.default_externalize_threshold_bytes
  then Ok [ "output", `String output ]
  else
    try
      let store = Tool_blob_store.create ~base_path in
      let store_stream bytes =
        Tool_blob_store.put_durable store ~bytes ~mime:"text/plain"
        |> Tool_output.normalized_artifact_ref_to_json
      in
      Ok
        [ "output_artifact", store_stream output
        ; "stdout_artifact", store_stream stdout
        ; "stderr_artifact", store_stream stderr
        ]
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> Error (Printexc.to_string exn)

module For_testing = struct
  (* Test seam: when set, [handle_tool_execute_typed] routes its dispatch
     through this override instead of the real shell dispatch. The override
     returns a controlled [Execute_shell_ir.dispatch_error] so tests can
     drive each rejected-dispatch branch through the real production wiring
     (stream start -> dispatch -> stream end) without spawning a process. *)
  let dispatch_override
      : (unit ->
        ( Masc_exec.Exec_dispatch.dispatch_result
        , Keeper_tooling.Execute_shell_ir.dispatch_error )
        result)
        option ref
    = ref None

  let model_execute_location_fields = model_execute_location_fields
  let redact_execute_output_with_additional_secret_files
        ~additional_secret_files
        ~base_path
        ~keeper_name
        ~stdout
        ~stderr
    =
    let redaction =
      execute_secret_redaction
        ~additional_secret_files
        ~base_path
        ~keeper_name
    in
    redact_execute_output redaction ~stdout ~stderr

  let redact_execute_output ~base_path ~keeper_name ~stdout ~stderr =
    redact_execute_output_with_additional_secret_files
      ~additional_secret_files:[]
      ~base_path
      ~keeper_name
      ~stdout
      ~stderr

end

(* Typed Execute input projections extracted to
   [Keeper_tool_execute_input] (godfile decomp). *)
let assoc_upsert = Keeper_tool_execute_input.assoc_upsert
let typed_input_command_text = Keeper_tool_execute_input.typed_input_command_text
let typed_input_timeout_sec = Keeper_tool_execute_input.typed_input_timeout_sec
let typed_input_timeout_budget = Keeper_tool_execute_input.typed_input_timeout_budget
let typed_validation_error_text = Keeper_tool_execute_input.typed_validation_error_text

(* Backend target helpers for typed Shell IR dispatch. *)
let guest_sandbox_target = Keeper_sandbox_shell_ir_target.guest_target

type dispatch_bundle =
  { sandbox : Masc_exec.Sandbox_target.t
  ; sandbox_profile : Keeper_types_profile_sandbox.sandbox_profile
  ; fields : (string * Yojson.Safe.t) list
  ; base_host_env : string array option
  ; github_secret_files : unit -> (string list, string) result
  ; observe_route : unit -> Keeper_sandbox_shell_ir_target.observe_route
    (* Where this call can run boxed before the judge is asked (RFC-0422).
       Lazy: resolving it may acquire the guest. *)
  ; cleanup : unit -> unit
  }

let input_with_cwd cwd (input : Keeper_tool_execute_typed_input.execute_input) =
  { input with Keeper_tool_execute_typed_input.cwd = Some cwd }

let handle_tool_execute_typed
      ~(turn_sandbox_factory : Keeper_sandbox_factory.t option)
      ~(config : Workspace.config)
      ~(meta : keeper_meta)
      ?continuation_channel
      ?gate_context
      ?gate_grant
      ~shell_ir_rewrite
      ~(args : Yojson.Safe.t)
      ()
  =
  match
    Keeper_tool_execute_path.resolve_tool_execute_cwd_typed
      ~config
      ~meta
      (* Keep all keepers on the shared execute-worktree root. The exact
         external-effect Gate is evaluated after the concrete cwd and sandbox
         target have both been resolved. *)
      ~write_enabled:true
      ~args
  with
    | Error e ->
      let cwd =
        Keeper_tool_execute_path.requested_tool_execute_cwd
          ~config
          ~meta
          ~write_enabled:true
          ~args
      in
      model_execute_cwd_resolution_error ~config ~meta ~args ~cwd e
    | Ok cwd ->
        let model_location_fields =
          model_execute_location_fields ~config ~meta ~args ~cwd
      in
      let typed_args = assoc_upsert "cwd" (`String cwd) args in
      match Keeper_tool_execute_typed_input.of_json typed_args with
      | Error e ->
        Keeper_tool_execution.failure
          ~class_:Tool_result.Policy_rejection
          ~effect_disposition:Tool_result.Proven_pre_effect
          (error_json
             ~fields:
               ([ "typed", `Bool true ] @ model_location_fields)
             e)
      | Ok input ->
        (match Keeper_tool_execute_typed_input.validate input with
         | Error e ->
           let fields =
             [ "typed", `Bool true ] @ model_location_fields
           in
           Keeper_tool_execution.failure
             ~class_:Tool_result.Policy_rejection
             ~effect_disposition:Tool_result.Proven_pre_effect
             (error_json ~fields (typed_validation_error_text e))
         | Ok () ->
        let cmd = typed_input_command_text input in
        let timeout_budget = typed_input_timeout_budget input in
        let timeout_sec = typed_input_timeout_sec input in
        let input = input_with_cwd cwd input in
        let sandbox_profile, _ =
          Keeper_sandbox_runner.effective_sandbox_profile ~meta
        in
        let resolved_dispatch =
          match Keeper_sandbox_factory.resolve_opt turn_sandbox_factory ~cwd with
          | No_factory ->
            (match sandbox_profile with
             | Remote_ssh -> Ok `Remote_ssh
             | Docker | Micro_vm ->
               Error
                 (Keeper_sandbox_shell_ir_target.target_error
                    "typed Shell IR guest dispatch requires a turn sandbox factory (no factory provided)"))
          | Remote_ssh_profile ->
            if sandbox_profile = Remote_ssh
            then Ok `Remote_ssh
            else
              Error
                (Keeper_sandbox_shell_ir_target.profile_contract_mismatch
                   ~expected:sandbox_profile
                   ~actual:Remote_ssh)
          | Runtime binding ->
            guest_sandbox_target
              ~binding
              ~meta
              ~cwd
              ~timeout_sec
              ~base_path:config.base_path
              ()
            |> Result.map (fun dispatch -> `Guest dispatch)
        in
        let dispatch_sandbox =
          match resolved_dispatch with
          | Error _ as error -> error
          | Ok `Remote_ssh ->
            (* Host identity projection is deliberately absent: the remote
               shim synthesizes its own minimal environment. *)
            (match
               Keeper_sandbox_shell_ir_target.ssh_target
                 ~base_path:config.base_path
                 ~meta
                 ~timeout_sec
                 ()
             with
             | Error error -> Error error
             | Ok dispatch ->
               let endpoint_fields =
                 match dispatch.target with
                 | Masc_exec.Sandbox_target.Ssh { endpoint; _ } ->
                   [ "remote_endpoint", `String endpoint.name
                   ; "remote_host", `String endpoint.host
                   ]
                 | Host | Docker _ | Micro_vm _ | Delegated _ -> []
               in
               Ok
                 { sandbox = dispatch.target
                 ; sandbox_profile = Remote_ssh
                 ; fields =
                     [ "requested_sandbox", `String "remote_ssh"
                     ; "via", `String "remote_ssh"
                     ; "sandbox_profile", `String "remote_ssh"
                     ]
                     @ endpoint_fields
                 ; base_host_env = None
                 ; github_secret_files = (fun () -> Ok [])
                 ; observe_route = dispatch.observe_route
                 ; cleanup = Fun.id
                 })
          (* The factory result is the sole route authority. Its frozen guest
             profile selects both the concrete target and the Gate label, so
             a later meta snapshot cannot split execution from observability. *)
          | Ok (`Guest (dispatch : Keeper_sandbox_shell_ir_target.guest_dispatch)) ->
            let label =
              Keeper_types_profile_sandbox.sandbox_profile_to_string
                dispatch.sandbox_profile
            in
            Ok
              { sandbox = dispatch.target
              ; sandbox_profile = dispatch.sandbox_profile
              ; fields =
                  [ "requested_sandbox", `String label
                  ; "via", `String label
                  ; "sandbox_profile", `String label
                  ]
              ; base_host_env = None
              ; github_secret_files =
                  (fun () ->
                     Keeper_turn_sandbox_runtime.prepare_github_identity_secret_files
                       ~timeout_sec
                       dispatch.runtime)
              ; observe_route = dispatch.observe_route
              ; cleanup = Fun.id
              }
        in
        (match dispatch_sandbox with
         | Error ({ message; fields; class_ } : Keeper_sandbox_shell_ir_target.target_error) ->
           Keeper_tool_execution.failure
             ~class_
             ~effect_disposition:Tool_result.Proven_pre_effect
             (error_json
                ~fields:
                  ([ "typed", `Bool true; "cmd", `String cmd ]
                   @ model_location_fields
                   @ fields)
                message)
         | Ok dispatch_bundle ->
        Fun.protect ~finally:dispatch_bundle.cleanup (fun () ->
        let dispatch_sandbox = dispatch_bundle.sandbox in
        let sandbox_extra_fields = dispatch_bundle.fields in
        let base_host_env = dispatch_bundle.base_host_env in
        let dispatched_model_location_fields =
          (* [Host] is unreachable on this lane: every profile a keeper may
             declare builds a guest or SSH target, and the builder that made
             a host one went with the [Local] profile. The arm stays because
             [Sandbox_target.t] is the general execution type and other
             callers do run on this host; it answers with the same fields the
             other targets do rather than describing a keeper that cannot
             exist. *)
          match dispatch_sandbox with
          | Masc_exec.Sandbox_target.Host
          | Docker _ | Micro_vm _ | Ssh _ -> model_location_fields
          | Masc_exec.Sandbox_target.Delegated _ ->
            (* Unreachable from a profile-built target today (no profile
               builds one); a delegated stage is labelled where the
               delegation is minted, not here. *)
            model_location_fields
        in
        (* Lower the validated typed input exactly once. The resulting Shell IR
           is the neutral dispatch representation; it carries no product or
           inferred authorization semantics. *)
        (* RFC tools-as-shell-commands: the one conversion point.  Stages
           whose program is the bare reserved word [masc] become delegated
           tool calls before dispatch.  Every caller supplies a surface, and
           a lane with no turn to look a tool up in supplies the one that
           refuses ([Keeper_shell_tool_command.refuse_reserved_command]).
           There is no absent case: routing lives in the IR's sandbox field,
           so a line that skipped the rewrite is not refused, it runs as a
           host program of that name (#32730).  A refusal reads as a typed
           refusal with its own code; both refusals travel the same failure
           shape below. *)
        let shell_ir_error =
          match
            Keeper_tool_execute_typed_input.to_shell_ir
              ~sandbox:dispatch_sandbox
              input
          with
          | Error e ->
            Error (typed_validation_error_text e, "typed_validation_failed")
          | Ok ir -> (
            match shell_ir_rewrite ir with
            | Ok rewritten -> Ok rewritten
            | Error message -> Error (message, "shell_tool_command_rejected"))
        in
        match shell_ir_error with
        | Error (text, code) ->
          let fields =
            [ "typed", `Bool true; "cmd", `String cmd; "code", `String code ]
            @ dispatched_model_location_fields
          in
          Keeper_tool_execution.failure
            ~class_:Tool_result.Policy_rejection
            ~effect_disposition:Tool_result.Proven_pre_effect
            (error_json ~fields text)
        | Ok ir ->
        let cmd_for_log =
          Exec_policy.sanitize_command_for_log_of_ir ~fallback_cmd:cmd ir
          |> Exec_policy.truncate_for_log
        in
        let message_for_log s =
          String.map
            (function
              | '\n' | '\r' | '\t' -> ' '
              | c -> c)
            s
          |> Exec_policy.truncate_for_log
        in
        let typed_context_fields =
          [ "typed", `Bool true; "cmd", `String cmd_for_log ]
          @ dispatched_model_location_fields
        in
        let typed_error_json
              ?(class_ = Tool_result.Runtime_failure)
              ?(extra_fields = [])
              msg
          =
          Keeper_tool_execution.failure
            ~class_
            ~effect_disposition:Tool_result.Proven_pre_effect
            (error_json
               ~fields:(typed_context_fields @ extra_fields)
               msg)
        in
        let sandbox_profile_label =
          Keeper_types_profile_sandbox.sandbox_profile_to_string
            dispatch_bundle.sandbox_profile
        in
        let typed_args = assoc_upsert "cwd" (`String cwd) typed_args in
        let gate_input =
          execute_gate_input
            ~input:typed_args
            ~cwd
            ~sandbox_profile:sandbox_profile_label
            ~sandbox_target:(sandbox_target_label dispatch_sandbox)
        in
        let gate_request : Keeper_gate.request =
          { keeper_name = meta.name
          ; operation = gate_operation
          ; input = gate_input
          ; call_summary = Keeper_tool_execute_input.typed_input_call_summary input
          ; base_path = config.base_path
          ; causal_context = Option.map (fun current -> current ()) gate_context
          ; task_id = Option.map Keeper_id.Task_id.to_string meta.current_task_id
          ; continuation_channel
          ; sandbox_profile = Some dispatch_bundle.sandbox_profile
          }
        in
        (* RFC-0422: the box the gate may ask for, after every cheaper
           authority has declined. The same IR, the same cwd and budget, a
           target whose runner asks the shim for [Observe]; no output
           streaming, because until the gate has read the answer this run is
           not yet the call's result. The IR is rewritten with the box's
           target because execution reads the target from the IR, not from
           [~sandbox] — handing the effect-built IR to the box unchanged ran
           the real call and reported its exit as an observation. *)
        let observation =
          Keeper_tool_execute_observe.create
            ~route:dispatch_bundle.observe_route
            ~dispatch:(fun sandbox ->
              Keeper_tooling.Execute_shell_ir.dispatch
                ~workdir:cwd
                ~sandbox
                ~timeout_sec
                ?base_host_env
                (Masc_exec.Shell_ir.with_sandbox sandbox ir))
        in
        let gate_decision =
          Keeper_gate.decide
            ?cycle_grant:gate_grant
            ~observe:(Keeper_tool_execute_observe.observe observation)
            (* NDT-OK: this typed, caller-owned policy input is consumed only
               at the external-effect authorization boundary. *)
            ~keeper_always_allow:(Option.value ~default:false meta.always_allow)
            gate_request
        in
        (match
           gate_decision
         with
         | Keeper_gate.Deferred { approval_id; reason; audit_receipts } ->
           (* RFC-0422 §3.3: a keeper whose request the box refused is told
              what was refused, in the same bytes the judge reads, so it can
              choose an observation-only path instead of waiting. *)
           let observation_fields =
             match Keeper_tool_execute_observe.outcome observation with
             | Some (Keeper_gate.Observed_refused { status; stderr }) ->
               [ ( "observation"
                 , Keeper_approval_queue_rules_types.observed_refusal_to_yojson
                     (Keeper_gate.observed_refusal ~status ~stderr) )
               ]
             | Some (Keeper_gate.Observed_result _ | Keeper_gate.Observation_unavailable _)
             | None -> []
           in
           Keeper_gate_deferred_payload.create
             ~operation:gate_operation
             ~approval_id
             ~reason
             ~audit_receipts
             ~context:(`Assoc (typed_context_fields @ observation_fields))
             ()
           |> Keeper_gate_deferred_payload.to_execution
         | Keeper_gate.Unavailable reason ->
           typed_error_json
             ~class_:Tool_result.Dependency_unavailable
             ~extra_fields:
               [ "error", `String "gate_unavailable"
               ; "gate_reason"
               , `String (Keeper_gate.unavailable_reason_to_string reason)
               ]
             "External effect was not executed because the Gate could not durably record its decision state. This Keeper remains active and may continue other work."
         | Keeper_gate.Allow authorization ->
          Log.Keeper.info
            ~keeper_name:meta.name
            "external effect authorized operation=tool_execute source=%s"
            (Keeper_gate.authorization_source_to_string authorization.source);
          let authorized result =
            Keeper_tool_execution.with_gate_authorization authorization result
          in
          (match dispatch_bundle.github_secret_files () with
           | Error err ->
             authorized
               (typed_error_json
                  ~extra_fields:[ "error", `String "github_identity_snapshot_unavailable" ]
                  ("GitHub identity snapshot unavailable: " ^ err))
           | Ok github_secret_files ->
          let output_redaction =
            execute_secret_redaction
              ~additional_secret_files:github_secret_files
              ~base_path:config.base_path
              ~keeper_name:meta.name
          in
          (* NDT-OK: wall clock is used only for elapsed telemetry, never for
             dispatch branching or policy decisions. *)
          let t0 = Unix.gettimeofday () in
          let task_id =
            Option.map Keeper_id.Task_id.to_string meta.current_task_id
          in
          let stdout_stream_redaction =
            Keeper_secret_redaction.create_stream_state output_redaction
          in
          let stderr_stream_redaction =
            Keeper_secret_redaction.create_stream_state output_redaction
          in
          (* Execute output always streams. The MASC_STREAM_EXECUTE_OUTPUT
             kill switch was read here on every tool execution — an env
             effect inside the dispatch path — while nothing in the
             repository, shell config, or deployment scripts ever set it,
             and its parse accepted every value except the exact string
             "false" (RFC-0371 B7). *)
          (try
             Keeper_keepalive_signal.record_execute_stream_start
               ~keeper_name:meta.name
               ~task_id
           with
            | Eio.Cancel.Cancelled _ as e -> raise e
            | exn ->
              Log.Dashboard.warn
                "execute stream start callback failed keeper=%s: %s"
                meta.name
                (Printexc.to_string exn));
          let record_stream_chunk stream data =
            if not (String.equal data "")
            then (
              try
                Keeper_keepalive_signal.record_execute_stream_chunk
                  ~keeper_name:meta.name
                  ~stream
                  data
              with
              | Eio.Cancel.Cancelled _ as e -> raise e
              | exn ->
                Log.Dashboard.warn
                  "execute stream chunk callback failed keeper=%s: %s"
                  meta.name
                  (Printexc.to_string exn))
          in
          (* The line-buffered redactor is boundary-safe when a credential is
             split across process chunks and scans every byte once. *)
          let on_output_chunk chunk =
            let stream, state, data =
              match chunk with
              | `Stdout s -> `Stdout, stdout_stream_redaction, s
              | `Stderr s -> `Stderr, stderr_stream_redaction, s
            in
            let data = Keeper_secret_redaction.redact_stream_chunk state data in
            record_stream_chunk stream data
          in
          (* RFC execute-subset-dispositions step 1.  A script that arrived
             inside [argv:["sh";"-c";...]] is invisible to everything else on
             this path, so it is counted here and nowhere else.  Recognition
             and classification only -- the dispatch below is unchanged, and
             what runs is exactly what ran before this line existed. *)
          (* [lowered] is what the step-1 line was missing. The finding says
             what was hidden inside the argv; it does not say whether step 4
             then put the call under the boundary, because the tap reads the
             input and the lowering happens after it. Measured on 27 live
             records: 9 [representable], and no way to tell how many of them
             the gate actually took.

             It is a property of the call rather than of the stage, so every
             stage has to answer: lowering rewrites one costume, and a
             multi-stage call whose second stage is still [bash -c] has not
             left its shell behind because its first stage did.  Reading only
             the [Simple] arm and calling the other two lowered said the
             opposite.

             Since #32662 no multi-stage call reaches here: [Argv] and
             [Script] both lower to one [Simple]. The stage traversal is
             {!Keeper_tooling.Shell_costume.ir_keeps_a_shell}'s answer for
             the whole [Shell_ir.t] type, and only its own tests still take
             it. Kept because the predicate is about the type, not about
             which caller happens to be the last one standing. *)
          let lowered = not (Keeper_tooling.Shell_costume.ir_keeps_a_shell ir) in
          let costume_findings =
            Keeper_tool_execute_typed_input.hidden_script_findings
              ~sandbox:dispatch_sandbox
              input
          in
          List.iter
            (fun (shell, finding) ->
               Log.Keeper.info
                 "shell_costume keeper=%s shell=%s finding=%s lowered=%b cmd=%s"
                 meta.name
                 shell
                 (Keeper_tooling.Shell_costume.finding_tag finding)
                 lowered
                 cmd_for_log)
            costume_findings;
          (* Tell without refusing.  Two thirds of live escapes are [;], which
             the IR omits on purpose, and none of them is refused: an
             argv-shaped costume arrives as one opaque program, so the gate has
             nothing to object to and the call runs.  Refusing them would break
             work that runs today; saying nothing gives the writer no reason to
             stop.  So the answer also says what the call should have been.

             It goes in the payload, not in [metadata].  A completed result's
             model-visible text is the serialized [data]
             ([Tool_result.message]); [_meta] is a separate field, and every
             read of it in agent_core discards it -- [agent_tools.ml] answers
             [Ok { content; _meta = _ }] at the point a tool result becomes
             conversation.  Metadata reaches dispatch observers and an MCP wire
             client; it does not reach the keeper this sentence is written for.
             A field beside the others leaves [ok], the status and the streams
             alone, so a call that worked still reads as a call that worked. *)
          let costume_advice =
            List.filter_map
              (fun (shell, finding) ->
                 match finding with
                 | Keeper_tooling.Shell_costume.Outside_the_subset reason ->
                   Some
                     (`Assoc
                         [ "shell", `String shell
                         ; ( "finding"
                           , `String
                               (Keeper_tooling.Shell_costume.finding_tag finding) )
                         ; ( "should_have_been"
                           , `String
                               (Keeper_tooling.Subset_rewrite.to_string
                                  (Keeper_tooling.Subset_rewrite.of_reason reason))
                           )
                         ])
                 | Keeper_tooling.Shell_costume.Representable
                 | Keeper_tooling.Shell_costume.Refused_by_policy _
                 | Keeper_tooling.Shell_costume.Unparsable _ -> None)
              costume_findings
          in
          let escaped_shell_fields =
            match costume_advice with
            | [] -> []
            | advice -> [ "escaped_shell", `List advice ]
          in
          let dispatch_unboxed () =
            Keeper_tooling.Execute_shell_ir.dispatch
              ~workdir:cwd
              ~sandbox:dispatch_sandbox
              ~timeout_sec
              ?base_host_env
              ~on_output_chunk
              ir
          in
          let dispatch () =
            match !For_testing.dispatch_override with
            | Some override -> override ()
            | None ->
              Keeper_tool_execute_observe.dispatch_authorized
                ~source:authorization.source
                ~on_output_chunk
                ~dispatch:dispatch_unboxed
          in
          let dispatch_result =
            match dispatch_sandbox with
            | Masc_exec.Sandbox_target.Host ->
              Keeper_external_resource_lease.with_lease
                (Keeper_external_resource_lease.Host_cwd cwd)
                dispatch
            | Docker _ | Micro_vm _ | Ssh _ | Delegated _ -> dispatch ()
          in
          match dispatch_result with
          | Error (Keeper_tooling.Execute_shell_ir.Gate_reject diagnostic) ->
            (* RFC-0208 P1: gate denial audit line. *)
            Log.Keeper.warn
              "shell_ir gate_reject keeper=%s cmd=%s diagnostic=%s"
              meta.name
              cmd_for_log
              (message_for_log diagnostic);
            (try
               Keeper_keepalive_signal.record_execute_stream_end
                 ~keeper_name:meta.name
                 ~task_id
                 ~status:(`Assoc [ "rejected", `String "gate_reject" ])
             with
              | Eio.Cancel.Cancelled _ as e -> raise e
              | exn ->
                Log.Dashboard.warn
                  "execute stream end callback failed keeper=%s: %s"
                  meta.name
                  (Printexc.to_string exn));
            authorized (typed_error_json diagnostic)
          | Error (Keeper_tooling.Execute_shell_ir.Cannot_parse reason) ->
            let reason_tag = Keeper_tooling.Execute_shell_ir.parse_reason_tag reason in
            (* Parity with gate_reject/path_reject, which have always carried
               their diagnostic.  These two could not until the typed gate
               could produce them. *)
            Log.Keeper.warn
              "shell_ir cannot_parse keeper=%s cmd=%s reason=%s"
              meta.name
              cmd_for_log
              reason_tag;
            (try
               Keeper_keepalive_signal.record_execute_stream_end
                 ~keeper_name:meta.name
                 ~task_id
                 ~status:
                   (`Assoc
                      [ "rejected", `String "cannot_parse"; "reason", `String reason_tag ])
             with
              | Eio.Cancel.Cancelled _ as e -> raise e
              | exn ->
                Log.Dashboard.warn
                  "execute stream end callback failed keeper=%s: %s"
                  meta.name
                  (Printexc.to_string exn));
            authorized
              (typed_error_json (Printf.sprintf "Cannot parse command: %s" reason_tag))
          | Error (Keeper_tooling.Execute_shell_ir.Too_complex reason) ->
            let reason_tag = Keeper_tooling.Execute_shell_ir.too_complex_reason_tag reason in
            (* Parity with gate_reject/path_reject, which have always carried
               their diagnostic.  These two could not until the typed gate
               could produce them. *)
            Log.Keeper.warn
              "shell_ir too_complex keeper=%s cmd=%s reason=%s"
              meta.name
              cmd_for_log
              reason_tag;
            (try
               Keeper_keepalive_signal.record_execute_stream_end
                 ~keeper_name:meta.name
                 ~task_id
                 ~status:
                   (`Assoc
                      [ "rejected", `String "too_complex"; "reason", `String reason_tag ])
             with
              | Eio.Cancel.Cancelled _ as e -> raise e
              | exn ->
                Log.Dashboard.warn
                  "execute stream end callback failed keeper=%s: %s"
                  meta.name
                  (Printexc.to_string exn));
            authorized
              (typed_error_json
                 (Printf.sprintf
                    "Command too complex: %s. %s."
                    reason_tag
                    (Keeper_tooling.Subset_rewrite.to_string
                       (Keeper_tooling.Subset_rewrite.of_reason reason))))
          | Error (Keeper_tooling.Execute_shell_ir.Path_reject e) ->
            (* RFC-0208 P1: path-policy denial audit line. *)
            Log.Keeper.warn
              "shell_ir path_reject keeper=%s cmd=%s reason=%s"
              meta.name
              cmd_for_log
              (message_for_log e);
            (try
               Keeper_keepalive_signal.record_execute_stream_end
                 ~keeper_name:meta.name
                 ~task_id
                 ~status:(`Assoc [ "rejected", `String "path_reject" ])
             with
              | Eio.Cancel.Cancelled _ as e -> raise e
              | exn ->
                Log.Dashboard.warn
                  "execute stream end callback failed keeper=%s: %s"
                  meta.name
                  (Printexc.to_string exn));
            authorized
              (typed_error_json
                 ~extra_fields:[ "blocked_cmd", `String cmd_for_log ]
                 e)
          | Ok result ->
            let elapsed_ms =
              (* NDT-OK: second wall-clock read closes the elapsed telemetry
                 span recorded immediately below. *)
              elapsed_duration_ms ~start_time:t0 ~end_time:(Unix.gettimeofday ())
            in
            Log.Keeper.info
              "shell_ir dispatch keeper=%s sandbox=%s status=%s elapsed_ms=%d"
              meta.name
              sandbox_profile_label
              (Keeper_sandbox_exec_failure.status_label result.status)
              elapsed_ms;
            let stdout, stderr, output =
              redact_execute_output output_redaction
                ~stdout:result.stdout
                ~stderr:result.stderr
            in
            (* One reading of the status, so the values it decides cannot
               disagree, and a test can ask what a status means without
               starting a child ([Keeper_tool_execute_exit_report]). *)
            let exit_report =
              Keeper_tool_execute_exit_report.of_status
                ~status:result.status
                ~stderr
                ~timeout_budget
            in
            let status_json = exit_report.Keeper_tool_execute_exit_report.status in
            (* The line-buffered redactor holds a partial trailing line, so the
               stream is flushed here before the end marker. *)
            record_stream_chunk
              `Stdout
              (Keeper_secret_redaction.redact_stream_finish stdout_stream_redaction);
            record_stream_chunk
              `Stderr
              (Keeper_secret_redaction.redact_stream_finish stderr_stream_redaction);
            (try
               Keeper_keepalive_signal.record_execute_stream_end
                 ~keeper_name:meta.name
                 ~task_id
                 ~status:status_json
             with
             | Eio.Cancel.Cancelled _ as e -> raise e
             | exn ->
               Log.Dashboard.warn
                 "execute stream end callback failed keeper=%s: %s"
                 meta.name
                 (Printexc.to_string exn));
            (try
               Keeper_keepalive_signal.record_execute_output
                 ~keeper_name:meta.name
                 ~task_id
                 ~stdout
                 ~stderr
                 ~status:status_json
                 ~streamed:true
             with
             | Eio.Cancel.Cancelled _ as e -> raise e
             | exn ->
               Log.Dashboard.warn
                 "execute output callback failed keeper=%s: %s"
                 meta.name
                 (Printexc.to_string exn));
            let succeeded = exit_report.Keeper_tool_execute_exit_report.ok in
            let failure_error_fields =
              exit_report.Keeper_tool_execute_exit_report.error_fields
            in
            let output_fields =
              if succeeded
              then
                composable_output_fields
                  ~base_path:config.base_path
                  ~stdout
                  ~stderr
                  ~output
              else Ok [ "output", `String output ]
            in
            (match output_fields with
             | Error detail ->
               Log.Keeper.warn
                 ~keeper_name:meta.name
                 "execute output artifact persistence failed after process completion: %s"
                 detail;
               authorized
                 (Keeper_tool_execution.failure
                    ~effect_disposition:Tool_result.Proven_post_effect
                    (error_json
                       ~fields:
                         ([ "typed", `Bool true
                          ; "code", `String "execute_output_externalization_failed"
                          ; "status", status_json
                          ; "execution_time_ms", `Int elapsed_ms
                          ]
                          @ dispatched_model_location_fields)
                       "Execute completed, but its oversized output artifact could not be persisted."))
             | Ok output_fields ->
               let timeout_fields =
                 exit_report.Keeper_tool_execute_exit_report.timeout_fields
               in
               let payload =
                 `Assoc
                   ([ "ok", `Bool succeeded
                    ; "status", status_json
                    ]
                    @ escaped_shell_fields
                    @ timeout_fields
                    @ output_fields
                    @ [ "typed", `Bool true
                      ; "execution_time_ms", `Int elapsed_ms
                      ]
                    @ failure_error_fields
                    @ sandbox_extra_fields
                    @ dispatched_model_location_fields)
               in
               (* A process that ran and exited nonzero (or died to a
                  signal) is an observed tool result the model reads and
                  reacts to — the payload carries ok:false, the exit
                  status, and stderr. Routing it through the failure
                  disposition marked the whole turn
                  Terminal_effect_failed (sticky), so a keeper probing a
                  missing path with `ls` died mid-mission — four turn
                  deaths across the E0 campaign and a pilot Keeper
                  (masc#28983). Only infra failures — sandbox dispatch,
                  secret projection, output/manifest persistence — keep
                  the failure disposition. *)
               authorized
                 (match
                    Tool_bridge.attach_artifact_manifest
                      ~base_path:config.base_path
                      (let answered =
                         Tool_result.make_ok
                           ~tool_name:"tool_execute"
                           ~start_time:t0
                           ~data:payload
                           ()
                       in
                       answered)
                  with
                  | Ok result -> Keeper_tool_execution.of_tool_result result
                  | Error _ ->
                    Keeper_tool_execution.failure
                      ~effect_disposition:Tool_result.Proven_post_effect
                      (error_json
                         ~fields:
                           ([ "typed", `Bool true
                            ; "code", `String "execute_result_manifest_failed"
                            ; "status", status_json
                            ; "execution_time_ms", `Int elapsed_ms
                            ]
                            @ dispatched_model_location_fields)
                         "Execute completed, but its result manifest could not be persisted.")))
        )))))

let handle_tool_execute_with_outcome
      ~(turn_sandbox_factory : Keeper_sandbox_factory.t option)
      ~(config : Workspace.config)
      ~(meta : keeper_meta)
      ?continuation_channel
      ?gate_context
      ?gate_grant
      ~shell_ir_rewrite
      ~(args : Yojson.Safe.t)
      ()
  =
  (* No key pre-check: [Keeper_tool_execute_typed_input.of_json] is the one
     admission door, and its errors name the field that was wrong. A second
     key list here is a copy that diverges — #29813 advertised [script] in
     the schema while this function still refused it, so every
     schema-conformant script call bounced before the parser could read it. *)
  handle_tool_execute_typed
    ~turn_sandbox_factory
    ~config
    ~meta
    ?continuation_channel
    ?gate_context
    ?gate_grant
    ~shell_ir_rewrite
    ~args
    ()
;;

let handle_tool_execute
      ~turn_sandbox_factory
      ~config
      ~meta
      ?continuation_channel
      ?gate_context
      ?gate_grant
      ~shell_ir_rewrite
      ~args
      ()
  =
  (handle_tool_execute_with_outcome
     ~turn_sandbox_factory
     ~config
     ~meta
     ?continuation_channel
     ?gate_context
     ?gate_grant
     ~shell_ir_rewrite
     ~args
     ()).raw_output
;;
