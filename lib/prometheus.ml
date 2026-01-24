(** Prometheus-Compatible Metrics for masc-mcp

    Provides lightweight metrics collection and Prometheus text format export.

    Usage:
    {[
      let () = Prometheus.inc_counter "masc_tasks_total" ~labels:[("status", "completed")]
      let () = Prometheus.set_gauge "masc_active_agents" 5.0
      let text = Prometheus.to_prometheus_text ()
    ]}

    @since 0.4.0
*)

(** {1 Metric Types} *)

type label = string * string

type metric_type =
  | Counter
  | Gauge
  | Histogram

type metric = {
  name: string;
  help: string;
  metric_type: metric_type;
  mutable value: float;
  labels: label list;
}

(** {1 Global Metrics Store} *)

let metrics : (string, metric) Hashtbl.t = Hashtbl.create 64
let metrics_mutex = Mutex.create ()

let with_lock f =
  Mutex.lock metrics_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock metrics_mutex) f

(** {1 Metric Registration} *)

let register_counter ~name ~help ?(labels=[]) () =
  with_lock (fun () ->
    let key = name ^ (String.concat "" (List.map (fun (k, v) -> k ^ v) labels)) in
    if not (Hashtbl.mem metrics key) then
      Hashtbl.add metrics key { name; help; metric_type = Counter; value = 0.0; labels }
  )

let register_gauge ~name ~help ?(labels=[]) () =
  with_lock (fun () ->
    let key = name ^ (String.concat "" (List.map (fun (k, v) -> k ^ v) labels)) in
    if not (Hashtbl.mem metrics key) then
      Hashtbl.add metrics key { name; help; metric_type = Gauge; value = 0.0; labels }
  )

(** {1 Metric Updates} *)

let inc_counter name ?(labels=[]) ?(delta=1.0) () =
  let key = name ^ (String.concat "" (List.map (fun (k, v) -> k ^ v) labels)) in
  with_lock (fun () ->
    match Hashtbl.find_opt metrics key with
    | Some m -> m.value <- m.value +. delta
    | None ->
        Hashtbl.add metrics key {
          name;
          help = name;
          metric_type = Counter;
          value = delta;
          labels;
        }
  )

let set_gauge name ?(labels=[]) value =
  let key = name ^ (String.concat "" (List.map (fun (k, v) -> k ^ v) labels)) in
  with_lock (fun () ->
    match Hashtbl.find_opt metrics key with
    | Some m -> m.value <- value
    | None ->
        Hashtbl.add metrics key {
          name;
          help = name;
          metric_type = Gauge;
          value;
          labels;
        }
  )

let inc_gauge name ?(labels=[]) ?(delta=1.0) () =
  let key = name ^ (String.concat "" (List.map (fun (k, v) -> k ^ v) labels)) in
  with_lock (fun () ->
    match Hashtbl.find_opt metrics key with
    | Some m -> m.value <- m.value +. delta
    | None ->
        Hashtbl.add metrics key {
          name;
          help = name;
          metric_type = Gauge;
          value = delta;
          labels;
        }
  )

let dec_gauge name ?(labels=[]) ?(delta=1.0) () =
  inc_gauge name ~labels ~delta:(-.delta) ()

(** {1 Built-in Metrics} *)

let init () =
  register_counter ~name:"masc_mcp_requests_total"
    ~help:"Total MCP requests received" ();
  register_counter ~name:"masc_tasks_total"
    ~help:"Total tasks processed" ();
  register_counter ~name:"masc_errors_total"
    ~help:"Total errors" ();
  register_gauge ~name:"masc_active_agents"
    ~help:"Currently active agents" ();
  register_gauge ~name:"masc_pending_tasks"
    ~help:"Tasks waiting to be claimed" ();
  register_gauge ~name:"masc_uptime_seconds"
    ~help:"Server uptime in seconds" ()

let start_time = Unix.gettimeofday ()

let update_uptime () =
  set_gauge "masc_uptime_seconds" (Unix.gettimeofday () -. start_time)

(** {1 Prometheus Export} *)

let type_to_string = function
  | Counter -> "counter"
  | Gauge -> "gauge"
  | Histogram -> "histogram"

let labels_to_string = function
  | [] -> ""
  | labels ->
      let pairs = List.map (fun (k, v) ->
        Printf.sprintf "%s=\"%s\"" k (String.escaped v)
      ) labels in
      "{" ^ String.concat "," pairs ^ "}"

let to_prometheus_text () =
  update_uptime ();
  let buf = Buffer.create 1024 in
  let by_name = Hashtbl.create 32 in
  with_lock (fun () ->
    Hashtbl.iter (fun _ m ->
      let existing = Hashtbl.find_opt by_name m.name |> Option.value ~default:[] in
      Hashtbl.replace by_name m.name (m :: existing)
    ) metrics
  );
  Hashtbl.iter (fun name ms ->
    match ms with
    | [] -> ()
    | m :: _ ->
        Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" name m.help);
        Buffer.add_string buf (Printf.sprintf "# TYPE %s %s\n" name (type_to_string m.metric_type));
        List.iter (fun metric ->
          Buffer.add_string buf (Printf.sprintf "%s%s %g\n"
            metric.name
            (labels_to_string metric.labels)
            metric.value)
        ) ms
  ) by_name;
  Buffer.contents buf

(** {1 Convenience Functions} *)

let record_request () =
  inc_counter "masc_mcp_requests_total" ()

let record_task_completed () =
  inc_counter "masc_tasks_total" ~labels:[("status", "completed")] ()

let record_task_failed () =
  inc_counter "masc_tasks_total" ~labels:[("status", "failed")] ()

let record_error ?(error_type="unknown") () =
  inc_counter "masc_errors_total" ~labels:[("type", error_type)] ()

let set_active_agents count =
  set_gauge "masc_active_agents" (float_of_int count)

let set_pending_tasks count =
  set_gauge "masc_pending_tasks" (float_of_int count)

(** Initialize on module load *)
let () = init ()
