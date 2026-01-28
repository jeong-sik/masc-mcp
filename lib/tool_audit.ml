(** Tool_audit - Audit query and statistics handlers *)

type context = {
  config: Room.config;
}

type audit_event = {
  timestamp: float;
  agent: string;
  event_type: string;
  success: bool;
  detail: string option;
}

(* Helper functions *)
let get_string args key default =
  match args with
  | `Assoc l ->
      (match List.assoc_opt key l with
       | Some (`String s) -> s
       | _ -> default)
  | _ -> default

let get_string_opt args key =
  match args with
  | `Assoc l ->
      (match List.assoc_opt key l with
       | Some (`String s) when s <> "" -> Some s
       | _ -> None)
  | _ -> None

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

(* Audit log path *)
let audit_log_path (config : Room.config) =
  Filename.concat (Room_utils.masc_dir config) "audit.log"

(* Convert audit event to JSON *)
let audit_event_to_json (e : audit_event) : Yojson.Safe.t =
  `Assoc [
    ("timestamp", `Float e.timestamp);
    ("agent", `String e.agent);
    ("event_type", `String e.event_type);
    ("success", `Bool e.success);
    ("detail", match e.detail with Some d -> `String d | None -> `Null);
  ]

(* Read audit events since given timestamp *)
let read_audit_events (config : Room.config) ~since : audit_event list =
  let path = audit_log_path config in
  if not (Sys.file_exists path) then []
  else
    let content = In_channel.with_open_text path In_channel.input_all in
    let lines = String.split_on_char '\n' content |> List.filter (fun s -> String.trim s <> "") in
    List.filter_map (fun line ->
      try
        let json = Yojson.Safe.from_string line in
        let open Yojson.Safe.Util in
        let timestamp = json |> member "timestamp" |> to_float in
        if timestamp < since then None
        else
          let agent = json |> member "agent" |> to_string in
          let event_type = json |> member "event_type" |> to_string in
          let success = json |> member "success" |> to_bool in
          let detail = json |> member "detail" |> to_string_option in
          Some { timestamp; agent; event_type; success; detail }
      with _ -> None
    ) lines

(* Handle masc_audit_query *)
let handle_audit_query ctx args =
  let agent_filter = get_string_opt args "agent" in
  let event_type = get_string args "event_type" "all" in
  let limit = get_int args "limit" 50 in
  let since_hours = get_float args "since_hours" 24.0 in
  let since = Unix.gettimeofday () -. (since_hours *. 3600.0) in
  let events = read_audit_events ctx.config ~since in
  let filtered =
    events
    |> List.filter (fun e ->
        match agent_filter with
        | Some a -> e.agent = a
        | None -> true)
    |> List.filter (fun e ->
        if event_type = "all" then true
        else e.event_type = event_type)
  in
  let limited =
    let rec take n xs =
      match xs with
      | [] -> []
      | _ when n <= 0 -> []
      | x :: rest -> x :: take (n - 1) rest
    in
    take limit filtered
  in
  let json = `Assoc [
    ("count", `Int (List.length limited));
    ("events", `List (List.map audit_event_to_json limited));
  ] in
  (true, Yojson.Safe.pretty_to_string json)

(* Handle masc_audit_stats *)
let handle_audit_stats ctx args =
  let agent_filter = get_string_opt args "agent" in
  let since = Unix.gettimeofday () -. (24.0 *. 3600.0) in
  let events = read_audit_events ctx.config ~since in
  let agents =
    let from_events = List.map (fun e -> e.agent) events in
    let from_metrics = Metrics_store_eio.get_all_agents ctx.config in
    let combined = from_events @ from_metrics in
    List.sort_uniq String.compare combined
  in
  let agents = match agent_filter with
    | Some a -> [a]
    | None -> agents
  in
  let stats_for agent_id =
    let agent_events = List.filter (fun e -> e.agent = agent_id) events in
    let count_type t =
      List.fold_left (fun acc e -> if e.event_type = t then acc + 1 else acc) 0 agent_events
    in
    let auth_success = count_type "auth_success" in
    let auth_failure = count_type "auth_failure" in
    let anomaly = count_type "anomaly_detected" in
    let violations = count_type "security_violation" in
    let tool_calls = count_type "tool_call" in
    let auth_total = auth_success + auth_failure in
    let auth_rate =
      if auth_total = 0 then `Null
      else `Float (float_of_int auth_success /. float_of_int auth_total)
    in
    let task_rate =
      match Metrics_store_eio.calculate_agent_metrics ctx.config ~agent_id ~days:7 with
      | Some m -> `Float m.Metrics_store_eio.task_completion_rate
      | None -> `Null
    in
    `Assoc [
      ("agent_id", `String agent_id);
      ("auth_success", `Int auth_success);
      ("auth_failure", `Int auth_failure);
      ("auth_success_rate", auth_rate);
      ("anomaly_count", `Int anomaly);
      ("security_violations", `Int violations);
      ("tool_calls", `Int tool_calls);
      ("task_completion_rate", task_rate);
    ]
  in
  let json = `Assoc [
    ("count", `Int (List.length agents));
    ("agents", `List (List.map stats_for agents));
  ] in
  (true, Yojson.Safe.pretty_to_string json)

(* Dispatch handler *)
let dispatch ctx ~name ~args =
  match name with
  | "masc_audit_query" -> Some (handle_audit_query ctx args)
  | "masc_audit_stats" -> Some (handle_audit_stats ctx args)
  | _ -> None
