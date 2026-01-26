(** Bounded Autonomy - Constrained multi-agent execution loops

    Provides formal guarantees:
    - Termination: Always terminates via hard_max_iterations
    - Safety: Post-check prevents silent constraint violations
    - Soundness: Typed comparisons with explicit error handling

    Designed based on MAGI review (Gemini + Qwen3 formal verification).
*)

open Spawn_eio

(** Comparison operators for goal conditions *)
type comparison =
  | Eq of Yojson.Safe.t
  | Neq of Yojson.Safe.t
  | Lt of float
  | Lte of float
  | Gt of float
  | Gte of float
  | Between of float * float
  | In of Yojson.Safe.t list

(** Goal condition with JSONPath-like path *)
type goal = {
  path: string;
  condition: comparison;
}

(** Constraint specification *)
type constraints = {
  max_turns: int option;
  max_tokens: int option;
  max_cost_usd: float option;
  max_time_seconds: float option;
  token_buffer: int;           (** Buffer for predictive checking *)
  hard_max_iterations: int;    (** Absolute failsafe limit *)
}

(** Default constraints - safe defaults *)
let default_constraints = {
  max_turns = Some 10;
  max_tokens = Some 100000;
  max_cost_usd = Some 1.0;
  max_time_seconds = Some 300.0;
  token_buffer = 5000;
  hard_max_iterations = 100;
}

(** Bounded execution state *)
type bounded_state = {
  mutable turns: int;
  mutable tokens_in: int;
  mutable tokens_out: int;
  mutable cost_usd: float;
  start_time: float;
  constraints: constraints;
}

(** Create new bounded state *)
let create_state constraints =
  {
    turns = 0;
    tokens_in = 0;
    tokens_out = 0;
    cost_usd = 0.0;
    start_time = Unix.gettimeofday ();
    constraints;
  }

(** Check single constraint *)
let check_single name current limit =
  match limit with
  | None -> None
  | Some max when current >= max ->
      Some (Printf.sprintf "%s exceeded: %d >= %d" name current max)
  | _ -> None

let check_single_float name current limit =
  match limit with
  | None -> None
  | Some max when current >= max ->
      Some (Printf.sprintf "%s exceeded: %.2f >= %.2f" name current max)
  | _ -> None

(** Check all constraints - returns first violation or None *)
let check_constraints state =
  let elapsed = Unix.gettimeofday () -. state.start_time in
  let total_tokens = state.tokens_in + state.tokens_out in
  let checks = [
    check_single "turns" state.turns state.constraints.max_turns;
    check_single "tokens" total_tokens state.constraints.max_tokens;
    check_single_float "cost_usd" state.cost_usd state.constraints.max_cost_usd;
    check_single_float "time_seconds" elapsed state.constraints.max_time_seconds;
  ] in
  List.find_map Fun.id checks

(** Check constraints with buffer (predictive) *)
let check_constraints_with_buffer state =
  let avg_tokens_per_turn =
    if state.turns > 0 then
      (state.tokens_in + state.tokens_out) / state.turns
    else
      state.constraints.token_buffer
  in
  let predicted_total =
    state.tokens_in + state.tokens_out + avg_tokens_per_turn
  in
  match state.constraints.max_tokens with
  | Some max when predicted_total > max ->
      Some (Printf.sprintf "Approaching token limit: %d + ~%d > %d"
        (state.tokens_in + state.tokens_out) avg_tokens_per_turn max)
  | _ -> check_constraints state

(** Simple path resolution - supports "$.field" and "$.field.subfield" *)
let resolve_path json path =
  let path =
    if String.starts_with ~prefix:"$." path then
      String.sub path 2 (String.length path - 2)
    else path
  in
  let parts = String.split_on_char '.' path in
  let rec walk json = function
    | [] -> Some json
    | part :: rest ->
        match json with
        | `Assoc fields ->
            (match List.assoc_opt part fields with
             | Some v -> walk v rest
             | None -> None)
        | _ -> None
  in
  walk json parts

(** Extract float from JSON value *)
let json_to_float = function
  | `Int i -> Some (float_of_int i)
  | `Float f -> Some f
  | `String s -> (try Some (float_of_string s) with _ -> None)
  | _ -> None

(** Check goal condition against result *)
let check_goal result goal =
  match resolve_path result goal.path with
  | None -> false  (* Path not found = goal not met *)
  | Some value ->
      match goal.condition with
      | Eq expected -> value = expected
      | Neq expected -> value <> expected
      | Lt threshold ->
          (match json_to_float value with
           | Some v -> v < threshold
           | None -> false)
      | Lte threshold ->
          (match json_to_float value with
           | Some v -> v <= threshold
           | None -> false)
      | Gt threshold ->
          (match json_to_float value with
           | Some v -> v > threshold
           | None -> false)
      | Gte threshold ->
          (match json_to_float value with
           | Some v -> v >= threshold
           | None -> false)
      | Between (lo, hi) ->
          (match json_to_float value with
           | Some v -> v >= lo && v <= hi
           | None -> false)
      | In values -> List.mem value values

(** Execution history entry *)
type history_entry = {
  turn: int;
  agent: string;
  tokens_in: int;
  tokens_out: int;
  cost_usd: float;
  elapsed_ms: int;
  goal_met: bool;
}

(** Bounded run result *)
type bounded_result = {
  status: [ `Goal_reached | `Constraint_exceeded | `Error ];
  reason: string;
  final_output: string option;
  stats: bounded_state;
  history: history_entry list;
  warning: string option;
}

(** Update state from spawn result *)
let update_state state result =
  state.turns <- state.turns + 1;
  state.tokens_in <- state.tokens_in +
    (Option.value result.input_tokens ~default:0);
  state.tokens_out <- state.tokens_out +
    (Option.value result.output_tokens ~default:0);
  state.cost_usd <- state.cost_usd +.
    (Option.value result.cost_usd ~default:0.0)

(** Main bounded execution loop *)
let bounded_run ~constraints ~goal ~agents ~prompt ~spawn_fn =
  (* Pre-check: empty agents *)
  if List.length agents = 0 then
    {
      status = `Error;
      reason = "No agents available";
      final_output = None;
      stats = create_state constraints;
      history = [];
      warning = None;
    }
  else
    let state = create_state constraints in
    let history = ref [] in

    let rec loop () =
      (* 1. Hard limit check (failsafe) *)
      if state.turns >= constraints.hard_max_iterations then
        {
          status = `Constraint_exceeded;
          reason = Printf.sprintf "Hard iteration limit reached (%d)"
            constraints.hard_max_iterations;
          final_output = None;
          stats = state;
          history = List.rev !history;
          warning = None;
        }
      else
      (* 2. Predictive constraint check *)
      match check_constraints_with_buffer state with
      | Some reason ->
          {
            status = `Constraint_exceeded;
            reason;
            final_output = None;
            stats = state;
            history = List.rev !history;
            warning = None;
          }
      | None ->
          (* 3. Select next agent (round-robin) *)
          let agent_idx = state.turns mod (List.length agents) in
          let agent = List.nth agents agent_idx in

          (* 4. Execute agent with exception handling *)
          let result =
            try Ok (spawn_fn agent prompt)
            with e -> Error (Printexc.to_string e)
          in

          match result with
          | Error msg ->
              {
                status = `Error;
                reason = Printf.sprintf "Agent execution failed: %s" msg;
                final_output = None;
                stats = state;
                history = List.rev !history;
                warning = None;
              }
          | Ok spawn_result ->
              (* 5. Update state AFTER execution *)
              update_state state spawn_result;

              (* 6. Parse output as JSON for goal check *)
              let output_json =
                try Yojson.Safe.from_string spawn_result.output
                with _ -> `Assoc [("raw", `String spawn_result.output)]
              in

              let goal_met = check_goal output_json goal in

              (* 7. Record history *)
              let entry = {
                turn = state.turns;
                agent;
                tokens_in = Option.value spawn_result.input_tokens ~default:0;
                tokens_out = Option.value spawn_result.output_tokens ~default:0;
                cost_usd = Option.value spawn_result.cost_usd ~default:0.0;
                elapsed_ms = spawn_result.elapsed_ms;
                goal_met;
              } in
              history := entry :: !history;

              (* 8. Post-check: did we exceed constraints? *)
              let warning = check_constraints state in

              if goal_met then
                {
                  status = `Goal_reached;
                  reason = Printf.sprintf "Goal met: %s" goal.path;
                  final_output = Some spawn_result.output;
                  stats = state;
                  history = List.rev !history;
                  warning;
                }
              else if Option.is_some warning then
                (* Exceeded but return partial result *)
                {
                  status = `Constraint_exceeded;
                  reason = Option.get warning;
                  final_output = Some spawn_result.output;
                  stats = state;
                  history = List.rev !history;
                  warning;
                }
              else
                loop ()
    in
    loop ()

(** Convert bounded_result to JSON *)
let result_to_json result =
  let status_str = match result.status with
    | `Goal_reached -> "goal_reached"
    | `Constraint_exceeded -> "constraint_exceeded"
    | `Error -> "error"
  in
  let history_json = List.map (fun e ->
    `Assoc [
      ("turn", `Int e.turn);
      ("agent", `String e.agent);
      ("tokens_in", `Int e.tokens_in);
      ("tokens_out", `Int e.tokens_out);
      ("cost_usd", `Float e.cost_usd);
      ("elapsed_ms", `Int e.elapsed_ms);
      ("goal_met", `Bool e.goal_met);
    ]
  ) result.history in
  `Assoc [
    ("status", `String status_str);
    ("reason", `String result.reason);
    ("final_output", match result.final_output with
      | Some s -> `String s
      | None -> `Null);
    ("stats", `Assoc [
      ("turns", `Int result.stats.turns);
      ("tokens_in", `Int result.stats.tokens_in);
      ("tokens_out", `Int result.stats.tokens_out);
      ("tokens_total", `Int (result.stats.tokens_in + result.stats.tokens_out));
      ("cost_usd", `Float result.stats.cost_usd);
      ("elapsed_seconds", `Float (Unix.gettimeofday () -. result.stats.start_time));
    ]);
    ("history", `List history_json);
    ("warning", match result.warning with
      | Some w -> `String w
      | None -> `Null);
  ]

(** Parse constraints from JSON *)
let constraints_of_json json =
  let open Yojson.Safe.Util in
  let get_int_opt key =
    try Some (json |> member key |> to_int)
    with _ -> None
  in
  let get_float_opt key =
    try Some (json |> member key |> to_float)
    with _ -> None
  in
  {
    max_turns = get_int_opt "max_turns";
    max_tokens = get_int_opt "max_tokens";
    max_cost_usd = get_float_opt "max_cost_usd";
    max_time_seconds = get_float_opt "max_time_seconds";
    token_buffer =
      (try json |> member "token_buffer" |> to_int
       with _ -> default_constraints.token_buffer);
    hard_max_iterations =
      (try json |> member "hard_max_iterations" |> to_int
       with _ -> default_constraints.hard_max_iterations);
  }

(** Parse goal from JSON *)
let goal_of_json json =
  let open Yojson.Safe.Util in
  let path = json |> member "path" |> to_string in
  let cond = json |> member "condition" in
  let condition =
    if member "eq" cond <> `Null then
      Eq (member "eq" cond)
    else if member "neq" cond <> `Null then
      Neq (member "neq" cond)
    else if member "lt" cond <> `Null then
      Lt (member "lt" cond |> to_float)
    else if member "lte" cond <> `Null then
      Lte (member "lte" cond |> to_float)
    else if member "gt" cond <> `Null then
      Gt (member "gt" cond |> to_float)
    else if member "gte" cond <> `Null then
      Gte (member "gte" cond |> to_float)
    else if member "between" cond <> `Null then
      let arr = member "between" cond |> to_list in
      Between (List.nth arr 0 |> to_float, List.nth arr 1 |> to_float)
    else if member "in" cond <> `Null then
      In (member "in" cond |> to_list)
    else
      Eq (`Bool true)  (* Default: look for truthy value *)
  in
  { path; condition }
