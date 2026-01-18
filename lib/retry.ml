(** Retry Module - Exponential backoff, Circuit Breaker, Timeout for MASC *)

(* ============================================ *)
(* Retry Policy Configuration                   *)
(* ============================================ *)

type retry_policy = {
  max_attempts: int;
  initial_delay_ms: int;
  max_delay_ms: int;
  backoff_multiplier: float;
  jitter: bool;
}

let default_policy = {
  max_attempts = 3;
  initial_delay_ms = 100;
  max_delay_ms = 10000;
  backoff_multiplier = 2.0;
  jitter = true;
}

let aggressive_policy = {
  max_attempts = 5;
  initial_delay_ms = 50;
  max_delay_ms = 5000;
  backoff_multiplier = 1.5;
  jitter = true;
}

let conservative_policy = {
  max_attempts = 2;
  initial_delay_ms = 500;
  max_delay_ms = 30000;
  backoff_multiplier = 3.0;
  jitter = false;
}

(* ============================================ *)
(* Retry Result Type                            *)
(* ============================================ *)

type 'a retry_result =
  | Success of 'a
  | Exhausted of { attempts: int; last_error: string }
  | CircuitOpen
  | TimedOut of { timeout_ms: int }

(* ============================================ *)
(* Thread-safe Statistics                       *)
(* ============================================ *)

type retry_stats = {
  mutable total_attempts: int;
  mutable successful_retries: int;
  mutable failed_retries: int;
  mutable circuit_breaker_trips: int;
  mutable timeouts: int;
  mutex: Mutex.t;
}

let global_stats = {
  total_attempts = 0;
  successful_retries = 0;
  failed_retries = 0;
  circuit_breaker_trips = 0;
  timeouts = 0;
  mutex = Mutex.create ();
}

(* Thread-safe stats helpers *)
let with_stats_lock f =
  Mutex.lock global_stats.mutex;
  let result = try f () with e -> Mutex.unlock global_stats.mutex; raise e in
  Mutex.unlock global_stats.mutex;
  result

let stats_inc_total () =
  with_stats_lock (fun () -> global_stats.total_attempts <- global_stats.total_attempts + 1)

let stats_inc_successful () =
  with_stats_lock (fun () -> global_stats.successful_retries <- global_stats.successful_retries + 1)

let stats_inc_failed () =
  with_stats_lock (fun () -> global_stats.failed_retries <- global_stats.failed_retries + 1)

let stats_inc_trips () =
  with_stats_lock (fun () -> global_stats.circuit_breaker_trips <- global_stats.circuit_breaker_trips + 1)

let stats_inc_timeouts () =
  with_stats_lock (fun () -> global_stats.timeouts <- global_stats.timeouts + 1)

(* ============================================ *)
(* Circuit Breaker                              *)
(* ============================================ *)

type circuit_state =
  | Closed      (* Normal operation *)
  | Open        (* Failing, reject requests *)
  | HalfOpen    (* Testing if service recovered *)

type circuit_breaker = {
  name: string;
  failure_threshold: int;
  success_threshold: int;
  timeout_ms: int;
  mutable state: circuit_state;
  mutable failure_count: int;
  mutable success_count: int;
  mutable last_failure_time: float;
  mutable probe_in_progress: bool;  (* Prevents thundering herd in HalfOpen *)
  mutex: Mutex.t;
}

let create_circuit_breaker
    ?(failure_threshold=5)
    ?(success_threshold=2)
    ?(timeout_ms=30000)
    ~name
    () =
  {
    name;
    failure_threshold;
    success_threshold;
    timeout_ms;
    state = Closed;
    failure_count = 0;
    success_count = 0;
    last_failure_time = 0.0;
    probe_in_progress = false;
    mutex = Mutex.create ();
  }

(* Thread-safe circuit breaker helper *)
let with_cb_lock cb f =
  Mutex.lock cb.mutex;
  let result = try f () with e -> Mutex.unlock cb.mutex; raise e in
  Mutex.unlock cb.mutex;
  result

(* Default circuit breakers for different operation types *)
let file_ops_breaker = create_circuit_breaker ~name:"file_ops" ~failure_threshold:3 ()
let external_cmd_breaker = create_circuit_breaker ~name:"external_cmd" ~failure_threshold:5 ()
let broadcast_breaker = create_circuit_breaker ~name:"broadcast" ~failure_threshold:10 ()

(* Check if circuit breaker allows request *)
let circuit_allows (cb : circuit_breaker) : bool =
  with_cb_lock cb (fun () ->
    match cb.state with
    | Closed -> true
    | Open ->
        let now = Unix.gettimeofday () in
        let elapsed_ms = (now -. cb.last_failure_time) *. 1000.0 in
        if elapsed_ms >= float_of_int cb.timeout_ms then begin
          cb.state <- HalfOpen;
          cb.success_count <- 0;
          cb.probe_in_progress <- true;  (* First request becomes the probe *)
          true
        end else
          false
    | HalfOpen ->
        (* Only allow one probe at a time to prevent thundering herd *)
        if cb.probe_in_progress then
          false  (* Another probe is already testing the circuit *)
        else begin
          cb.probe_in_progress <- true;
          true
        end
  )

(* Record success *)
let circuit_record_success (cb : circuit_breaker) =
  with_cb_lock cb (fun () ->
    match cb.state with
    | Closed ->
        cb.failure_count <- 0
    | HalfOpen ->
        cb.probe_in_progress <- false;  (* Allow next probe *)
        cb.success_count <- cb.success_count + 1;
        if cb.success_count >= cb.success_threshold then begin
          cb.state <- Closed;
          cb.failure_count <- 0;
          cb.success_count <- 0
        end
    | Open -> ()
  )

(* Record failure *)
let circuit_record_failure (cb : circuit_breaker) =
  with_cb_lock cb (fun () ->
    cb.last_failure_time <- Unix.gettimeofday ();
    match cb.state with
    | Closed ->
        cb.failure_count <- cb.failure_count + 1;
        if cb.failure_count >= cb.failure_threshold then begin
          cb.state <- Open;
          stats_inc_trips ()
        end
    | HalfOpen ->
        cb.probe_in_progress <- false;  (* Reset probe on failure *)
        cb.state <- Open;
        cb.success_count <- 0;
        stats_inc_trips ()
    | Open -> ()
  )

(* Exception-safe probe execution with guaranteed cleanup.
   Uses Lwt.finalize to ensure probe_in_progress is reset even on exceptions. *)
let with_circuit_probe (cb : circuit_breaker) (f : unit -> 'a Lwt.t) : ('a, string) result Lwt.t =
  let open Lwt.Syntax in
  let allowed = circuit_allows cb in
  if not allowed then
    Lwt.return (Error "Circuit breaker open")
  else
    Lwt.finalize
      (fun () ->
        Lwt.catch
          (fun () ->
            let* result = f () in
            circuit_record_success cb;
            Lwt.return (Ok result))
          (fun exn ->
            circuit_record_failure cb;
            Lwt.return (Error (Printexc.to_string exn))))
      (fun () ->
        (* Failsafe: ensure probe is released even if record functions failed *)
        with_cb_lock cb (fun () ->
          if cb.state = HalfOpen then
            cb.probe_in_progress <- false);
        Lwt.return_unit)

(* Get circuit breaker status as JSON *)
let circuit_status (cb : circuit_breaker) : Yojson.Safe.t =
  with_cb_lock cb (fun () ->
    let state_str = match cb.state with
      | Closed -> "closed"
      | Open -> "open"
      | HalfOpen -> "half_open"
    in
    `Assoc [
      ("name", `String cb.name);
      ("state", `String state_str);
      ("failure_count", `Int cb.failure_count);
      ("failure_threshold", `Int cb.failure_threshold);
      ("success_count", `Int cb.success_count);
      ("success_threshold", `Int cb.success_threshold);
    ]
  )

(* Reset circuit breaker *)
let reset_circuit_breaker (cb : circuit_breaker) =
  with_cb_lock cb (fun () ->
    cb.state <- Closed;
    cb.failure_count <- 0;
    cb.success_count <- 0;
    cb.last_failure_time <- 0.0;
    cb.probe_in_progress <- false
  )

(* ============================================ *)
(* Delay Calculation                            *)
(* ============================================ *)

let calculate_delay policy attempt =
  let base_delay = float_of_int policy.initial_delay_ms in
  let multiplied = base_delay *. (policy.backoff_multiplier ** float_of_int (attempt - 1)) in
  let capped = min multiplied (float_of_int policy.max_delay_ms) in
  let delay =
    if policy.jitter then
      let jitter_factor = 0.75 +. (Random.float 0.5) in
      capped *. jitter_factor
    else
      capped
  in
  int_of_float delay

(* ============================================ *)
(* Timeout Wrapper                              *)
(* ============================================ *)

let with_timeout ~timeout_ms (task : unit -> 'a Lwt.t) : 'a option Lwt.t =
  let open Lwt.Syntax in
  let timeout_sec = float_of_int timeout_ms /. 1000.0 in
  let timeout_promise =
    let* () = Lwt_unix.sleep timeout_sec in
    Lwt.return_none
  in
  let task_promise =
    let* result = task () in
    Lwt.return_some result
  in
  Lwt.pick [timeout_promise; task_promise]

(* ============================================ *)
(* Async Retry with Policy                      *)
(* ============================================ *)

let with_retry_lwt
    ?(policy=default_policy)
    ?(circuit_breaker=None)
    ?(timeout_ms=None)
    ~op_name
    f =
  let open Lwt.Syntax in

  let rec attempt n last_error =
    (* Check circuit breaker *)
    let cb_allows = match circuit_breaker with
      | None -> true
      | Some cb -> circuit_allows cb
    in
    if not cb_allows then begin
      Log.Retry.warn "%s: circuit breaker OPEN, rejecting" op_name;
      Lwt.return CircuitOpen
    end
    else if n > policy.max_attempts then begin
      stats_inc_failed ();
      Lwt.return (Exhausted { attempts = n - 1; last_error })
    end
    else begin
      stats_inc_total ();

      (* Wrap with timeout if specified *)
      let task_with_timeout =
        match timeout_ms with
        | None ->
            Lwt.catch
              (fun () -> let* r = f () in Lwt.return (Some (Ok r)))
              (fun exn -> Lwt.return (Some (Error (Printexc.to_string exn))))
        | Some ms ->
            let* result_opt = with_timeout ~timeout_ms:ms (fun () ->
              Lwt.catch
                (fun () -> let* r = f () in Lwt.return (Ok r))
                (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
            ) in
            match result_opt with
            | None ->
                stats_inc_timeouts ();
                Lwt.return None
            | Some r -> Lwt.return (Some r)
      in

      let* result = task_with_timeout in
      match result with
      | None ->
          (* Timeout *)
          (match circuit_breaker with Some cb -> circuit_record_failure cb | None -> ());
          let ms = Option.value timeout_ms ~default:0 in
          if n < policy.max_attempts then begin
            let delay = calculate_delay policy n in
            Log.Retry.warn "%s: attempt %d/%d TIMEOUT (%dms), retrying in %dms"
              op_name n policy.max_attempts ms delay;
            let* () = Lwt_unix.sleep (float_of_int delay /. 1000.0) in
            attempt (n + 1) (Printf.sprintf "Timeout after %dms" ms)
          end else
            Lwt.return (TimedOut { timeout_ms = ms })
      | Some (Ok value) ->
          (match circuit_breaker with Some cb -> circuit_record_success cb | None -> ());
          if n > 1 then stats_inc_successful ();
          Lwt.return (Success value)
      | Some (Error err) ->
          (match circuit_breaker with Some cb -> circuit_record_failure cb | None -> ());
          if n < policy.max_attempts then begin
            let delay = calculate_delay policy n in
            Log.Retry.warn "%s: attempt %d/%d failed, retrying in %dms: %s"
              op_name n policy.max_attempts delay err;
            let* () = Lwt_unix.sleep (float_of_int delay /. 1000.0) in
            attempt (n + 1) err
          end else
            attempt (n + 1) err
    end
  in
  attempt 1 ""

(* ============================================ *)
(* Sync Retry (for non-Lwt code)                *)
(* ============================================ *)

let with_retry_sync ?(policy=default_policy) ~op_name f =
  let rec attempt n last_error =
    if n > policy.max_attempts then begin
      stats_inc_failed ();
      Exhausted { attempts = n - 1; last_error }
    end
    else begin
      stats_inc_total ();
      try
        let result = f () in
        if n > 1 then stats_inc_successful ();
        Success result
      with exn ->
        let err = Printexc.to_string exn in
        if n < policy.max_attempts then begin
          let delay = calculate_delay policy n in
          Log.Retry.warn "%s: attempt %d/%d failed, retrying in %dms: %s"
            op_name n policy.max_attempts delay err;
          Unix.sleepf (float_of_int delay /. 1000.0);
          attempt (n + 1) err
        end else
          attempt (n + 1) err
    end
  in
  attempt 1 ""

(* ============================================ *)
(* Statistics                                   *)
(* ============================================ *)

let get_stats () : Yojson.Safe.t =
  with_stats_lock (fun () ->
    `Assoc [
      ("total_attempts", `Int global_stats.total_attempts);
      ("successful_retries", `Int global_stats.successful_retries);
      ("failed_retries", `Int global_stats.failed_retries);
      ("circuit_breaker_trips", `Int global_stats.circuit_breaker_trips);
      ("timeouts", `Int global_stats.timeouts);
      ("circuit_breakers", `Assoc [
        ("file_ops", circuit_status file_ops_breaker);
        ("external_cmd", circuit_status external_cmd_breaker);
        ("broadcast", circuit_status broadcast_breaker);
      ]);
    ]
  )

let reset_stats () =
  with_stats_lock (fun () ->
    global_stats.total_attempts <- 0;
    global_stats.successful_retries <- 0;
    global_stats.failed_retries <- 0;
    global_stats.circuit_breaker_trips <- 0;
    global_stats.timeouts <- 0
  )

let reset_all_circuit_breakers () =
  reset_circuit_breaker file_ops_breaker;
  reset_circuit_breaker external_cmd_breaker;
  reset_circuit_breaker broadcast_breaker

(* ============================================ *)
(* Idempotency Store (Thread-safe)              *)
(* ============================================ *)

let idempotency_store : (string, float) Hashtbl.t = Hashtbl.create 1000
let idempotency_mutex = Mutex.create ()

let with_idempotency_lock f =
  Mutex.lock idempotency_mutex;
  let result = try f () with e -> Mutex.unlock idempotency_mutex; raise e in
  Mutex.unlock idempotency_mutex;
  result

let generate_idempotency_key () =
  let now = Unix.gettimeofday () in
  let rand = Random.int 1000000 in
  Printf.sprintf "%f-%06d" now rand

let check_idempotency ~key ~ttl_seconds =
  with_idempotency_lock (fun () ->
    match Hashtbl.find_opt idempotency_store key with
    | None -> `NotSeen
    | Some timestamp ->
        let now = Unix.gettimeofday () in
        if now -. timestamp < ttl_seconds then
          `AlreadyExecuted
        else begin
          Hashtbl.remove idempotency_store key;
          `Expired
        end
  )

let record_idempotency ~key =
  with_idempotency_lock (fun () ->
    Hashtbl.replace idempotency_store key (Unix.gettimeofday ())
  )

let cleanup_idempotency ~max_age_seconds =
  with_idempotency_lock (fun () ->
    let now = Unix.gettimeofday () in
    let to_remove = Hashtbl.fold (fun k v acc ->
      if now -. v > max_age_seconds then k :: acc else acc
    ) idempotency_store [] in
    List.iter (Hashtbl.remove idempotency_store) to_remove;
    List.length to_remove
  )

(* ============================================ *)
(* Idempotency Auto-Cleanup Scheduler           *)
(* ============================================ *)

type cleanup_scheduler = {
  mutable running: bool;
  mutable interval_seconds: float;
  mutable max_age_seconds: float;
  mutex: Mutex.t;
}

let scheduler_instance : cleanup_scheduler option ref = ref None

let create_scheduler ?(interval_seconds=60.0) ?(max_age_seconds=300.0) () =
  match !scheduler_instance with
  | Some s -> s
  | None ->
      let s = {
        running = false;
        interval_seconds;
        max_age_seconds;
        mutex = Mutex.create ();
      } in
      scheduler_instance := Some s;
      s

let start_scheduler ?(interval_seconds=60.0) ?(max_age_seconds=300.0) () =
  let open Lwt.Syntax in
  let scheduler = create_scheduler ~interval_seconds ~max_age_seconds () in
  Mutex.lock scheduler.mutex;
  if scheduler.running then begin
    Mutex.unlock scheduler.mutex;
    ()
  end else begin
    scheduler.running <- true;
    scheduler.interval_seconds <- interval_seconds;
    scheduler.max_age_seconds <- max_age_seconds;
    Mutex.unlock scheduler.mutex;
    let rec loop () =
      if not scheduler.running then
        Lwt.return_unit
      else
        let* () = Lwt_unix.sleep scheduler.interval_seconds in
        if scheduler.running then begin
          let removed = cleanup_idempotency ~max_age_seconds:scheduler.max_age_seconds in
          if removed > 0 then
            Log.Retry.debug "IdempotencyScheduler cleaned up %d expired keys" removed;
          loop ()
        end else
          Lwt.return_unit
    in
    Lwt.async (fun () ->
      Lwt.catch loop (fun exn ->
        Log.Retry.error "IdempotencyScheduler crashed: %s" (Printexc.to_string exn);
        Lwt.return_unit))
  end

let stop_scheduler () =
  match !scheduler_instance with
  | None -> ()
  | Some scheduler ->
      Mutex.lock scheduler.mutex;
      scheduler.running <- false;
      Mutex.unlock scheduler.mutex

let scheduler_status () : Yojson.Safe.t =
  match !scheduler_instance with
  | None -> `Assoc [("running", `Bool false); ("configured", `Bool false)]
  | Some scheduler ->
      Mutex.lock scheduler.mutex;
      let status = `Assoc [
        ("running", `Bool scheduler.running);
        ("configured", `Bool true);
        ("interval_seconds", `Float scheduler.interval_seconds);
        ("max_age_seconds", `Float scheduler.max_age_seconds);
        ("store_size", `Int (Hashtbl.length idempotency_store));
      ] in
      Mutex.unlock scheduler.mutex;
      status
