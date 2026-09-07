(** See .mli for the contract. *)

type t =
  { route : unit -> Keeper_sandbox_shell_ir_target.observe_route
  ; dispatch :
      Masc_exec.Sandbox_target.t
      -> ( Masc_exec.Exec_dispatch.dispatch_result
         , Keeper_tooling.Execute_shell_ir.dispatch_error )
         result
  ; outcome : Keeper_gate.observation option ref
  }

let create ~route ~dispatch = { route; dispatch; outcome = ref None }

(* The typed gate's refusals, in the closed tags it already exports, so the
   gate log names the same reason the real dispatch would have logged. *)
let unavailable_tag = function
  | Keeper_tooling.Execute_shell_ir.Gate_reject _ -> "gate_reject"
  | Keeper_tooling.Execute_shell_ir.Cannot_parse reason ->
    "cannot_parse:" ^ Keeper_tooling.Execute_shell_ir.parse_reason_tag reason
  | Keeper_tooling.Execute_shell_ir.Too_complex reason ->
    "too_complex:" ^ Keeper_tooling.Execute_shell_ir.too_complex_reason_tag reason
  | Keeper_tooling.Execute_shell_ir.Path_reject _ -> "path_reject"
;;

let observe t () : Keeper_gate.observation =
  let outcome : Keeper_gate.observation =
    match t.route () with
    | Keeper_sandbox_shell_ir_target.No_box reason ->
      Keeper_gate.Observation_unavailable reason
    | Keeper_sandbox_shell_ir_target.Boxed { target = sandbox; run } ->
      (match t.dispatch sandbox with
       | Ok result ->
         (match run, result.Masc_exec.Exec_dispatch.status with
          | Keeper_types_profile_sandbox.Guest_local, _
          | Keeper_types_profile_sandbox.Observe, Unix.WEXITED 0 ->
            Keeper_gate.Observed_result { run; result }
          | Keeper_types_profile_sandbox.Observe, status ->
            Keeper_gate.Observed_refused { status; stderr = result.stderr })
       | Error error -> Keeper_gate.Observation_unavailable (unavailable_tag error))
  in
  t.outcome := Some outcome;
  outcome
;;

let outcome t = !(t.outcome)

let dispatch_authorized ~source ~on_output_chunk ~dispatch =
  match source with
  | Keeper_gate.Observed_in_box { result; run = _ } ->
    if not (String.equal result.stdout "")
    then on_output_chunk (`Stdout result.stdout);
    if not (String.equal result.stderr "")
    then on_output_chunk (`Stderr result.stderr);
    Ok result
  | Keeper_gate.One_shot_resolution _
  | Keeper_gate.Exact_always_rule _
  | Keeper_gate.Keeper_always_allow
  | Keeper_gate.Workspace_always_allow
  | Keeper_gate.Readonly_sandbox -> dispatch ()
;;
