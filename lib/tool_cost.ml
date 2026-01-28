(** Tool_cost - Cost tracking and reporting handlers *)

type context = {
  agent_name: string;
}

(* Helper functions *)
let get_string args key default =
  match args with
  | `Assoc l ->
      (match List.assoc_opt key l with
       | Some (`String s) -> s
       | _ -> default)
  | _ -> default

let get_int args key default =
  match args with
  | `Assoc l ->
      (match List.assoc_opt key l with
       | Some (`Int i) -> i
       | _ -> default)
  | _ -> default

let get_float args key default =
  match args with
  | `Assoc l ->
      (match List.assoc_opt key l with
       | Some (`Float f) -> f
       | Some (`Int i) -> float_of_int i
       | _ -> default)
  | _ -> default

(* Safe exec helper - runs CLI command and returns result *)
let safe_exec args =
  try
    let argv = Array.of_list args in
    let cmd = argv.(0) in
    let ic = Unix.open_process_args_in cmd argv in
    let output = In_channel.input_all ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> (true, output)
    | _ -> (false, output)
  with e ->
    (false, Printf.sprintf "❌ Command failed: %s" (Printexc.to_string e))

(* Handle masc_cost_log *)
let handle_cost_log ctx args =
  let model = get_string args "model" "unknown" in
  let input_tokens = get_int args "input_tokens" 0 in
  let output_tokens = get_int args "output_tokens" 0 in
  let cost_usd = get_float args "cost_usd" 0.0 in
  let task_id = get_string args "task_id" "" in
  let base_args = ["masc-cost"; "--log"; "--agent"; ctx.agent_name; "--model"; model;
                   "--input-tokens"; string_of_int input_tokens;
                   "--output-tokens"; string_of_int output_tokens;
                   "--cost"; Printf.sprintf "%.4f" cost_usd] in
  let cli_args = if task_id = "" then base_args else base_args @ ["--task"; task_id] in
  safe_exec cli_args

(* Handle masc_cost_report *)
let handle_cost_report _ctx args =
  let period = get_string args "period" "daily" in
  let agent = get_string args "agent" "" in
  let task_id = get_string args "task_id" "" in
  let base_args = ["masc-cost"; "--report"; "--period"; period; "--json"] in
  let cli_args = base_args
                 |> (fun a -> if agent = "" then a else a @ ["--agent"; agent])
                 |> (fun a -> if task_id = "" then a else a @ ["--task"; task_id]) in
  safe_exec cli_args

(* Dispatch handler *)
let dispatch ctx ~name ~args =
  match name with
  | "masc_cost_log" -> Some (handle_cost_log ctx args)
  | "masc_cost_report" -> Some (handle_cost_report ctx args)
  | _ -> None
