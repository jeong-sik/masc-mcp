(** MASC Cost Tracking CLI
    Track token usage and costs per agent/task *)

open Printf

(** Cost entry type *)
type cost_entry = {
  agent: string;
  task_id: string option;
  model: string;
  input_tokens: int;
  output_tokens: int;
  cost_usd: float;
  timestamp: string;
}

(** Get MASC root directory *)
let get_masc_root () =
  let me_root = try Sys.getenv "ME_ROOT" with Not_found ->
    Filename.concat (Sys.getenv "HOME") "me"
  in
  Filename.concat me_root ".masc"

(** Get costs file path *)
let costs_file () = Filename.concat (get_masc_root ()) "costs.jsonl"

(** Get current ISO timestamp *)
let now_iso () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(** Parse ISO date to Unix time (simplified) *)
let parse_iso_date s =
  try
    Scanf.sscanf s "%d-%d-%dT%d:%d:%dZ" (fun y m d h min sec ->
      let tm = {
        Unix.tm_sec = sec; tm_min = min; tm_hour = h;
        tm_mday = d; tm_mon = m - 1; tm_year = y - 1900;
        tm_wday = 0; tm_yday = 0; tm_isdst = false
      } in
      fst (Unix.mktime tm))
  with _ -> 0.0

(** Ensure MASC directory exists *)
let ensure_masc_dir () =
  let dir = get_masc_root () in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755

(** Log a cost entry *)
let log_cost ~agent ~task_id ~model ~input_tokens ~output_tokens ~cost_usd =
  ensure_masc_dir ();
  let entry = sprintf
    {|{"agent":"%s","task_id":%s,"model":"%s","input_tokens":%d,"output_tokens":%d,"cost_usd":%.4f,"timestamp":"%s"}|}
    agent
    (match task_id with Some t -> sprintf {|"%s"|} t | None -> "null")
    model input_tokens output_tokens cost_usd (now_iso ())
  in
  let oc = open_out_gen [Open_append; Open_creat] 0o644 (costs_file ()) in
  output_string oc (entry ^ "\n");
  close_out oc;
  printf "✅ Cost logged: %s → $%.4f\n" agent cost_usd

(** Parse a JSON line into cost entry *)
let parse_cost_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    Some {
      agent = json |> member "agent" |> to_string;
      task_id = json |> member "task_id" |> to_string_option;
      model = json |> member "model" |> to_string;
      input_tokens = json |> member "input_tokens" |> to_int;
      output_tokens = json |> member "output_tokens" |> to_int;
      cost_usd = json |> member "cost_usd" |> to_float;
      timestamp = json |> member "timestamp" |> to_string;
    }
  with _ -> None

(** Read all cost entries *)
let read_costs () =
  let file = costs_file () in
  if not (Sys.file_exists file) then []
  else begin
    let ic = open_in file in
    let rec read_lines acc =
      match input_line ic with
      | line ->
          let acc' = match parse_cost_line line with
            | Some e -> e :: acc
            | None -> acc
          in
          read_lines acc'
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    read_lines []
  end

(** Filter entries by time period *)
let filter_by_period period entries =
  let now = Unix.gettimeofday () in
  let cutoff = match period with
    | "hourly" -> now -. 3600.0
    | "daily" -> now -. 86400.0
    | "weekly" -> now -. 604800.0
    | "monthly" -> now -. 2592000.0
    | "all" -> 0.0
    | _ -> now -. 86400.0  (* default: daily *)
  in
  List.filter (fun e -> parse_iso_date e.timestamp >= cutoff) entries

(** Filter entries by agent *)
let filter_by_agent agent entries =
  if agent = "" then entries
  else List.filter (fun e -> e.agent = agent) entries

(** Filter entries by task *)
let filter_by_task task_id entries =
  if task_id = "" then entries
  else List.filter (fun e -> e.task_id = Some task_id) entries

(** Aggregate costs by agent *)
let aggregate_by_agent entries =
  let tbl = Hashtbl.create 8 in
  List.iter (fun e ->
    let (tokens, cost, _model) =
      try Hashtbl.find tbl e.agent
      with Not_found -> (0, 0.0, e.model)
    in
    Hashtbl.replace tbl e.agent
      (tokens + e.input_tokens + e.output_tokens, cost +. e.cost_usd, e.model)
  ) entries;
  Hashtbl.fold (fun agent (tokens, cost, model) acc ->
    (agent, tokens, cost, model) :: acc
  ) tbl []

(** Print cost report *)
let print_report ~agent ~task_id ~period =
  let entries = read_costs () in
  let filtered = entries
    |> filter_by_period period
    |> filter_by_agent agent
    |> filter_by_task task_id
  in

  if List.length filtered = 0 then begin
    printf "📊 No cost data for period: %s\n" period;
    exit 0
  end;

  let total_tokens = List.fold_left (fun acc e ->
    acc + e.input_tokens + e.output_tokens) 0 filtered in
  let total_cost = List.fold_left (fun acc e -> acc +. e.cost_usd) 0.0 filtered in
  let by_agent = aggregate_by_agent filtered in

  printf "\n";
  printf "╔══════════════════════════════════════════════╗\n";
  printf "║  💰 MASC Cost Report (%s)              \n" period;
  printf "╠══════════════════════════════════════════════╣\n";
  printf "║  Period: %s                              \n" period;
  printf "║  Entries: %d                              \n" (List.length filtered);
  printf "╠══════════════════════════════════════════════╣\n";

  (* By agent breakdown *)
  printf "║  📊 By Agent:                              \n";
  List.iter (fun (agent, tokens, cost, _model) ->
    let pct = if total_cost > 0.0 then cost /. total_cost *. 100.0 else 0.0 in
    printf "║    %-10s %8d tokens  $%7.2f (%4.1f%%)\n"
      agent tokens cost pct
  ) (List.sort (fun (_, _, a, _) (_, _, b, _) -> compare b a) by_agent);

  printf "╠══════════════════════════════════════════════╣\n";
  printf "║  📈 TOTAL: %d tokens, $%.2f            \n" total_tokens total_cost;
  printf "╚══════════════════════════════════════════════╝\n"

(** Print JSON report *)
let print_json_report ~agent ~task_id ~period =
  let entries = read_costs () in
  let filtered = entries
    |> filter_by_period period
    |> filter_by_agent agent
    |> filter_by_task task_id
  in
  let by_agent = aggregate_by_agent filtered in
  let total_cost = List.fold_left (fun acc e -> acc +. e.cost_usd) 0.0 filtered in

  let agents_json = String.concat ", " (List.map (fun (agent, tokens, cost, model) ->
    sprintf {|{"agent":"%s","tokens":%d,"cost_usd":%.4f,"model":"%s"}|} agent tokens cost model
  ) by_agent) in

  printf {|{"period":"%s","total_entries":%d,"total_cost_usd":%.4f,"by_agent":[%s]}|}
    period (List.length filtered) total_cost agents_json;
  print_newline ()

(** Parse command line arguments *)
type action = Log | Report

let () =
  let action = ref Report in
  let agent = ref "" in
  let task_id = ref "" in
  let model = ref "unknown" in
  let input_tokens = ref 0 in
  let output_tokens = ref 0 in
  let cost_usd = ref 0.0 in
  let period = ref "daily" in
  let json_output = ref false in

  let specs = [
    ("--log", Arg.Unit (fun () -> action := Log), "Log a cost entry");
    ("--report", Arg.Unit (fun () -> action := Report), "Show cost report");
    ("--agent", Arg.Set_string agent, "Agent name (claude, gemini, codex)");
    ("--task", Arg.Set_string task_id, "Task ID");
    ("--model", Arg.Set_string model, "Model name");
    ("--input-tokens", Arg.Set_int input_tokens, "Input tokens");
    ("--output-tokens", Arg.Set_int output_tokens, "Output tokens");
    ("--tokens", Arg.Int (fun t -> input_tokens := t), "Total tokens (shorthand)");
    ("--cost", Arg.Set_float cost_usd, "Cost in USD");
    ("--period", Arg.Set_string period, "Report period: hourly|daily|weekly|monthly|all");
    ("--json", Arg.Set json_output, "Output as JSON");
  ] in

  Arg.parse specs (fun _ -> ()) "masc-cost: Track multi-agent costs\n";

  match !action with
  | Log ->
      if !agent = "" then begin
        prerr_endline "Error: --agent is required for --log";
        exit 1
      end;
      let task = if !task_id = "" then None else Some !task_id in
      log_cost
        ~agent:!agent
        ~task_id:task
        ~model:!model
        ~input_tokens:!input_tokens
        ~output_tokens:!output_tokens
        ~cost_usd:!cost_usd

  | Report ->
      let task = !task_id in
      if !json_output then
        print_json_report ~agent:!agent ~task_id:task ~period:!period
      else
        print_report ~agent:!agent ~task_id:task ~period:!period
