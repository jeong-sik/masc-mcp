(** Rate Limiting for masc-mcp

    Provides token bucket rate limiting per client/agent.

    Configuration via environment:
    - MASC_RATE_LIMIT: requests per second (default: 20)
    - MASC_RATE_BURST: burst capacity (default: 50)

    @since 0.4.0
*)

(** {1 Token Bucket Algorithm} *)

type bucket = {
  mutable tokens: float;
  mutable last_update: float;
}

type t = {
  rate: float;
  burst: int;
  buckets: (string, bucket) Hashtbl.t;
  mutex: Mutex.t;
}

(** {1 Configuration} *)

let default_rate = 20.0  (* Higher default for multi-agent coordination *)
let default_burst = 50

let rate_from_env () =
  Sys.getenv_opt "MASC_RATE_LIMIT"
  |> Option.map float_of_string
  |> Option.value ~default:default_rate

let burst_from_env () =
  Sys.getenv_opt "MASC_RATE_BURST"
  |> Option.map int_of_string
  |> Option.value ~default:default_burst

(** {1 Limiter Creation} *)

let create ?(rate=default_rate) ?(burst=default_burst) () =
  {
    rate;
    burst;
    buckets = Hashtbl.create 256;
    mutex = Mutex.create ();
  }

let create_from_env () =
  create ~rate:(rate_from_env ()) ~burst:(burst_from_env ()) ()

(** {1 Rate Checking} *)

let with_lock limiter f =
  Mutex.lock limiter.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock limiter.mutex) f

let check limiter ~key =
  with_lock limiter (fun () ->
    let now = Unix.gettimeofday () in
    let bucket = match Hashtbl.find_opt limiter.buckets key with
      | Some b -> b
      | None ->
          let b = { tokens = float_of_int limiter.burst; last_update = now } in
          Hashtbl.add limiter.buckets key b;
          b
    in
    let elapsed = now -. bucket.last_update in
    let new_tokens = bucket.tokens +. (elapsed *. limiter.rate) in
    bucket.tokens <- min (float_of_int limiter.burst) new_tokens;
    bucket.last_update <- now;

    if bucket.tokens >= 1.0 then begin
      bucket.tokens <- bucket.tokens -. 1.0;
      true
    end else
      false
  )

let remaining limiter ~key =
  with_lock limiter (fun () ->
    match Hashtbl.find_opt limiter.buckets key with
    | Some b -> int_of_float b.tokens
    | None -> limiter.burst
  )

(** {1 Cleanup} *)

let cleanup limiter ~older_than_seconds =
  with_lock limiter (fun () ->
    let now = Unix.gettimeofday () in
    let threshold = now -. float_of_int older_than_seconds in
    let to_remove = Hashtbl.fold (fun key bucket acc ->
      if bucket.last_update <= threshold then key :: acc
      else acc
    ) limiter.buckets [] in
    List.iter (Hashtbl.remove limiter.buckets) to_remove;
    List.length to_remove
  )

(** {1 Global Instance} *)

let global = lazy (create_from_env ())

let check_global ~key =
  check (Lazy.force global) ~key

let remaining_global ~key =
  remaining (Lazy.force global) ~key

(** {1 HTTP Helpers} *)

let headers limiter ~key =
  let remaining = remaining limiter ~key in
  [
    ("X-RateLimit-Limit", string_of_int limiter.burst);
    ("X-RateLimit-Remaining", string_of_int remaining);
  ]

let too_many_requests_body () =
  {|{"error":"Too Many Requests","message":"Rate limit exceeded"}|}
