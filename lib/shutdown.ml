(** Graceful Shutdown Module - MCP 2025-11-25 SHOULD requirement *)

(** Module-level state - encapsulated for clarity and testability *)
module State = struct
  type t = {
    mutable shutdown_requested: bool;
    mutable shutdown_promise: unit Lwt.t;
    mutable shutdown_resolver: unit Lwt.u;
    mutable active_requests: int;
    (** Sync mutex for incr/decr_requests (called from any thread).
        Used with with_lock for quick atomic operations. *)
    mutex: Mutex.t;
    (** Lwt_condition for event-driven waiting (replaces 0.1s polling).
        Signaled by decr_requests when active_requests reaches 0. *)
    requests_done: unit Lwt_condition.t;
    (** Lwt mutex required by Lwt_condition.wait.
        Separate from sync mutex because Lwt operations are cooperative. *)
    lwt_mutex: Lwt_mutex.t;
  }

  let create () =
    let promise, resolver = Lwt.wait () in
    {
      shutdown_requested = false;
      shutdown_promise = promise;
      shutdown_resolver = resolver;
      active_requests = 0;
      mutex = Mutex.create ();
      requests_done = Lwt_condition.create ();
      lwt_mutex = Lwt_mutex.create ();
    }

  (** Module singleton - initialized once *)
  let global = create ()

  (** Thread-safe operation wrapper (for synchronous operations) *)
  let with_lock f =
    Mutex.lock global.mutex;
    let result = try f () with e -> Mutex.unlock global.mutex; raise e in
    Mutex.unlock global.mutex;
    result

  (** Reset for testing only
      WARNING: Never call this in production code.
      Calling during active shutdown will corrupt state. *)
  let reset () =
    with_lock (fun () ->
      global.shutdown_requested <- false;
      let promise, resolver = Lwt.wait () in
      global.shutdown_promise <- promise;
      global.shutdown_resolver <- resolver;
      global.active_requests <- 0
    )
end

let is_shutting_down () =
  State.with_lock (fun () -> State.global.shutdown_requested)

let incr_requests () =
  State.with_lock (fun () ->
    State.global.active_requests <- State.global.active_requests + 1
  )

let decr_requests () =
  let should_signal =
    State.with_lock (fun () ->
      State.global.active_requests <- State.global.active_requests - 1;
      if State.global.active_requests < 0 then
        Printf.eprintf "[Shutdown] WARNING: active_requests went negative (%d) - mismatched incr/decr!\n%!"
          State.global.active_requests;
      State.global.active_requests = 0
    )
  in
  (* Signal condition if all requests are done - outside mutex to avoid deadlock *)
  if should_signal then
    Lwt_condition.broadcast State.global.requests_done ()

let active_count () =
  State.with_lock (fun () ->
    State.global.active_requests
  )

let request_shutdown () =
  let should_wakeup =
    State.with_lock (fun () ->
      if not State.global.shutdown_requested then begin
        State.global.shutdown_requested <- true;
        true
      end else false
    )
  in
  if should_wakeup then begin
    Printf.eprintf "\n[Shutdown] Shutdown signal received, gracefully stopping...\n%!";
    Lwt.wakeup_later State.global.shutdown_resolver ()
  end

let wait_for_shutdown () =
  State.with_lock (fun () -> State.global.shutdown_promise)

(** Wait for pending requests with timeout - event-driven using Lwt_condition

    NOTE: There's an inherent race between sync Mutex (for active_requests)
    and Lwt_mutex (for condition wait). We mitigate this by:
    1. Adding a 100ms safety re-check timeout
    2. Always re-checking condition after wakeup
    This bounds worst-case latency to 100ms if signal is missed. *)
let wait_for_pending ~timeout_sec =
  let open Lwt.Syntax in
  (* Check immediately if no active requests *)
  if active_count () = 0 then Lwt.return_unit
  else begin
    let deadline = Unix.gettimeofday () +. timeout_sec in
    let rec loop () =
      if active_count () = 0 then Lwt.return_unit
      else begin
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0.0 then Lwt.return_unit  (* Timeout *)
        else begin
          (* Wait for signal OR 100ms safety re-check, whichever comes first *)
          let safety_recheck = Float.min 0.1 remaining in
          let* () = Lwt.pick [
            Lwt_mutex.with_lock State.global.lwt_mutex (fun () ->
              Lwt_condition.wait ~mutex:State.global.lwt_mutex State.global.requests_done
            );
            Lwt_unix.sleep safety_recheck;
          ] in
          loop ()
        end
      end
    in
    let* () = loop () in
    let remaining = active_count () in
    if remaining > 0 then
      Printf.eprintf "[Shutdown] Timeout waiting for %d pending requests\n%!" remaining;
    Lwt.return_unit
  end

(** Reset state - TESTING ONLY
    WARNING: Never call this in production code.
    Calling during active shutdown will corrupt state. *)
let reset_for_testing () = State.reset ()
