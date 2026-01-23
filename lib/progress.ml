(** Progress Notifications - MCP 2025-11-25 MAY requirement

    Send real-time progress updates for long-running tasks.
    Uses JSON-RPC 2.0 notifications/progress method.
*)

(** Progress update record *)
type progress = {
  task_id: string;
  progress: float;  (** 0.0 ~ 1.0 *)
  message: string option;
  estimated_remaining: float option;  (** seconds *)
}

(** Validation error types *)
type validation_error =
  | TaskIdEmpty
  | TaskIdTooLong of int  (** max 256 chars *)
  | TaskIdInvalidChars
  | ProgressOutOfRange of float

(** Validate task_id: non-empty, max 256 chars, printable ASCII only *)
let validate_task_id s =
  let len = String.length s in
  if len = 0 then Error TaskIdEmpty
  else if len > 256 then Error (TaskIdTooLong len)
  else
    let is_printable_ascii c =
      let code = Char.code c in
      code >= 32 && code <= 126
    in
    if String.for_all is_printable_ascii s then Ok s
    else Error TaskIdInvalidChars

(** Validate progress: 0.0 ~ 1.0 *)
let validate_progress p =
  if p < 0.0 || p > 1.0 then Error (ProgressOutOfRange p)
  else Ok p

(** Convert validation error to message *)
let validation_error_to_string = function
  | TaskIdEmpty -> "task_id cannot be empty"
  | TaskIdTooLong n -> Printf.sprintf "task_id too long (%d chars, max 256)" n
  | TaskIdInvalidChars -> "task_id contains invalid characters (ASCII 32-126 only)"
  | ProgressOutOfRange p -> Printf.sprintf "progress out of range: %.2f (must be 0.0-1.0)" p

(** Convert progress to JSON-RPC notification *)
let progress_to_jsonrpc p =
  let params = [
    ("taskId", `String p.task_id);
    ("progress", `Float p.progress);
  ] in
  let params = match p.message with
    | Some m -> ("message", `String m) :: params
    | None -> params
  in
  let params = match p.estimated_remaining with
    | Some r -> ("estimatedRemaining", `Float r) :: params
    | None -> params
  in
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "notifications/progress");
    ("params", `Assoc params);
  ]

(** Progress tracker for a single task.
    NOTE: Tracker instances are NOT thread-safe. Each tracker should be
    used from a single async context. The State.trackers Hashtbl is protected,
    but individual Tracker.t instances are not. *)
module Tracker = struct
  type t = {
    task_id: string;
    mutable current: float;
    total_steps: int;  (* immutable - set at creation *)
    mutable completed_steps: int;
    start_time: float;
  }

  (** Forward reference for notify - breaks circular dependency.
      See wire-up comment below for INVARIANT. *)
  let notify_ref : (task_id:string -> progress:float -> ?message:string -> ?estimated_remaining:float -> unit -> unit) ref =
    ref (fun ~task_id ~progress ?message:_ ?estimated_remaining:_ () ->
      (* WARNING: If you see this, notify_ref was never wired up! *)
      Printf.eprintf "[Progress] BUG: notify_ref not wired up! task=%s progress=%.2f\n%!" task_id progress
    )

  let create ~task_id ?(total_steps=100) () = {
    task_id;
    current = 0.0;
    total_steps;
    completed_steps = 0;
    start_time = Unix.gettimeofday ();
  }

  (** Update progress directly (0.0 ~ 1.0) *)
  let update t ~progress ?message () =
    t.current <- Float.min 1.0 (Float.max 0.0 progress);
    let elapsed = Unix.gettimeofday () -. t.start_time in
    let estimated_remaining =
      (* Only estimate when progress > 10% to avoid wild initial estimates *)
      if t.current > 0.1 then
        Some ((elapsed /. t.current) *. (1.0 -. t.current))
      else
        None
    in
    !notify_ref ~task_id:t.task_id ~progress:t.current ?message ?estimated_remaining ()

  (** Increment step count.
      WARNING: Logs if called more times than total_steps (indicates caller bug). *)
  let step t ?message () =
    t.completed_steps <- t.completed_steps + 1;
    if t.completed_steps > t.total_steps then
      Printf.eprintf "[Progress] WARNING: step called %d times but total_steps is %d (task=%s)\n%!"
        t.completed_steps t.total_steps t.task_id;
    let progress = Float.of_int t.completed_steps /. Float.of_int t.total_steps in
    update t ~progress ?message ()

  (** Mark as complete *)
  let complete t ?message () =
    update t ~progress:1.0 ?message ()
end

(** Module-level state - encapsulated for clarity and testability *)
module State = struct
  type t = {
    mutable sse_broadcast: Yojson.Safe.t -> unit;
    trackers: (string, Tracker.t) Hashtbl.t;
    mutex: Mutex.t;
  }

  let create () = {
    sse_broadcast = (fun _ ->
      (* Default no-op. Unlike notify_ref, this is expected during init.
         set_sse_callback should be called before any progress notifications. *)
      ()
    );
    trackers = Hashtbl.create 32;
    mutex = Mutex.create ();
  }

  (** Module singleton - initialized once *)
  let global = create ()

  (** Thread-safe operation wrapper *)
  let with_lock f =
    Mutex.lock global.mutex;
    let result = try f () with e -> Mutex.unlock global.mutex; raise e in
    Mutex.unlock global.mutex;
    result

  (** Reset for testing only *)
  let reset () =
    with_lock (fun () ->
      global.sse_broadcast <- (fun _ -> ());
      Hashtbl.clear global.trackers
    )
end

(** Set SSE broadcast callback *)
let set_sse_callback callback =
  State.with_lock (fun () ->
    State.global.sse_broadcast <- callback
  )

(** Send progress notification via SSE *)
let notify ~task_id ~progress ?message ?estimated_remaining () =
  let p = { task_id; progress; message; estimated_remaining } in
  let json = progress_to_jsonrpc p in
  (* Read callback under lock, call outside to avoid deadlock *)
  let callback = State.with_lock (fun () -> State.global.sse_broadcast) in
  try callback json
  with e ->
    Printf.eprintf "[Progress] SSE broadcast failed: %s\n%!" (Printexc.to_string e)

(** Wire up the forward reference
    INVARIANT: This assignment MUST happen exactly once, after notify is defined.
    If omitted, Tracker.update/step/complete will log BUG warnings to stderr.
    This pattern breaks circular dependency: Tracker needs notify, State needs Tracker.
    Alternative: recursive modules or functors (more complex). *)
let () = Tracker.notify_ref := notify

(** Start tracking a task *)
let start_tracking ~task_id ?total_steps () =
  let tracker = Tracker.create ~task_id ?total_steps () in
  State.with_lock (fun () ->
    Hashtbl.replace State.global.trackers task_id tracker
  );
  tracker

(** Get tracker for a task *)
let get_tracker task_id =
  State.with_lock (fun () ->
    Hashtbl.find_opt State.global.trackers task_id
  )

(** Stop tracking a task *)
let stop_tracking task_id =
  State.with_lock (fun () ->
    Hashtbl.remove State.global.trackers task_id
  )

(** MCP tool handler for progress - with input validation *)
let handle_progress_tool arguments =
  let open Yojson.Safe.Util in
  (* Catch Type_error specifically - thrown by member/to_* on missing or wrong type *)
  let get_string key = try Some (arguments |> member key |> to_string) with Type_error _ -> None in
  let get_float key = try Some (arguments |> member key |> to_float) with Type_error _ -> None in
  let get_int key = try Some (arguments |> member key |> to_int) with Type_error _ -> None in

  (* Validate and get task_id *)
  let validated_task_id () =
    match get_string "task_id" with
    | None -> Error "task_id required"
    | Some raw ->
      match validate_task_id raw with
      | Ok id -> Ok id
      | Error e -> Error (validation_error_to_string e)
  in

  match get_string "action" with
  | Some "start" ->
    (match validated_task_id () with
     | Ok task_id ->
       let total_steps = Option.value ~default:100 (get_int "total_steps") in
       let _ = start_tracking ~task_id ~total_steps () in
       (true, Printf.sprintf "Started tracking task: %s" task_id)
     | Error msg -> (false, msg))

  | Some "update" ->
    (match validated_task_id (), get_float "progress" with
     | Ok task_id, Some progress ->
       (match validate_progress progress with
        | Ok progress ->
          let message = get_string "message" in
          notify ~task_id ~progress ?message ();
          (true, Printf.sprintf "Progress updated: %s → %.0f%%" task_id (progress *. 100.0))
        | Error e -> (false, validation_error_to_string e))
     | Error msg, _ -> (false, msg)
     | _, None -> (false, "progress required"))

  | Some "step" ->
    (match validated_task_id () with
     | Ok task_id ->
       (match get_tracker task_id with
        | Some tracker ->
          let message = get_string "message" in
          Tracker.step tracker ?message ();
          (true, Printf.sprintf "Step completed: %s (%.0f%%)" task_id (tracker.Tracker.current *. 100.0))
        | None -> (false, Printf.sprintf "No tracker for task: %s" task_id))
     | Error msg -> (false, msg))

  | Some "complete" ->
    (match validated_task_id () with
     | Ok task_id ->
       let message = get_string "message" in
       notify ~task_id ~progress:1.0 ?message ();
       stop_tracking task_id;
       (true, Printf.sprintf "Task completed: %s" task_id)
     | Error msg -> (false, msg))

  | Some "stop" ->
    (match validated_task_id () with
     | Ok task_id ->
       stop_tracking task_id;
       (true, Printf.sprintf "Stopped tracking: %s" task_id)
     | Error msg -> (false, msg))

  | Some other -> (false, Printf.sprintf "Unknown action: %s" other)
  | None -> (false, "action required: start, update, step, complete, stop")

(** Reset state - TESTING ONLY, not exposed in mli *)
let reset_for_testing () = State.reset ()
