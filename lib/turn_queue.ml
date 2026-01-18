(** Turn Queue - Sequential Voice Output with Priority

    Core constraint: "병렬 수집 → 순차 출력"
    Multiple agents can request turns, but output is serialized.

    Architecture:
    - Agents enqueue turn requests (non-blocking)
    - Queue processes one turn at a time (blocking TTS)
    - Priority ordering: lower number = higher priority
    - Timestamp as tiebreaker for same priority
*)

(** {1 Types} *)

(** Turn request status *)
type turn_status =
  | Queued      (** Waiting in queue *)
  | Speaking    (** Currently being processed *)
  | Completed   (** Successfully finished *)
  | Failed of string  (** Failed with error *)
[@@deriving show, eq]

(** Turn request *)
type turn_request = {
  id: string;               (** Unique turn ID *)
  agent_id: string;         (** Agent requesting the turn *)
  message: string;          (** Message to speak *)
  voice: string;            (** Voice to use (e.g., "Sarah", "Roger") *)
  priority: int;            (** Lower = higher priority (default: 1) *)
  timestamp: float;         (** Request time (Unix timestamp) *)
  mutable status: turn_status;  (** Current status *)
  mutable duration_ms: int option;  (** Actual duration after completion *)
}

(** Queue state *)
type t = {
  mutable queue: turn_request list;  (** Pending requests (sorted) *)
  mutable current: turn_request option;  (** Currently speaking *)
  mutable is_processing: bool;  (** Processing loop active *)
  mutable total_processed: int;  (** Stats: total completed *)
  mutable total_failed: int;     (** Stats: total failed *)
}

(** {1 Constants} *)

let default_priority = 1
let default_voice = "Sarah"

(** {1 String Conversions} *)

let string_of_status = function
  | Queued -> "queued"
  | Speaking -> "speaking"
  | Completed -> "completed"
  | Failed msg -> Printf.sprintf "failed(%s)" msg

(** {1 Turn Request Creation} *)

(** Generate unique turn ID *)
let generate_turn_id () =
  let t = Unix.gettimeofday () in
  let ms = int_of_float (t *. 1000.) mod 1000000 in
  Printf.sprintf "turn_%d_%d" ms (Random.int 10000)

(** Create a new turn request *)
let create_turn ~agent_id ~message ?(voice=default_voice) ?(priority=default_priority) () =
  {
    id = generate_turn_id ();
    agent_id;
    message;
    voice;
    priority;
    timestamp = Unix.gettimeofday ();
    status = Queued;
    duration_ms = None;
  }

(** {1 Queue Creation and Management} *)

(** Create empty queue *)
let create () : t =
  Random.self_init ();
  {
    queue = [];
    current = None;
    is_processing = false;
    total_processed = 0;
    total_failed = 0;
  }

(** Sort queue by priority, then timestamp *)
let sort_queue queue =
  List.sort (fun a b ->
    let pri_cmp = compare a.priority b.priority in
    if pri_cmp <> 0 then pri_cmp
    else compare a.timestamp b.timestamp
  ) queue

(** Enqueue a turn request *)
let enqueue t ~agent_id ~message ?voice ?priority () =
  let turn = create_turn ~agent_id ~message ?voice ?priority () in
  t.queue <- sort_queue (turn :: t.queue);
  turn

(** Dequeue next turn (removes from queue) *)
let dequeue t : turn_request option =
  match t.queue with
  | [] -> None
  | turn :: rest ->
    t.queue <- rest;
    t.current <- Some turn;
    turn.status <- Speaking;
    Some turn

(** Peek at next turn without removing *)
let peek t : turn_request option =
  match t.queue with
  | [] -> None
  | turn :: _ -> Some turn

(** Get queue length *)
let length t = List.length t.queue

(** Check if queue is empty *)
let is_empty t = t.queue = []

(** Check if currently processing *)
let is_processing t = t.is_processing

(** Get current speaker *)
let current_speaker t = t.current

(** {1 Turn Completion} *)

(** Mark current turn as completed *)
let complete_turn t ~duration_ms =
  match t.current with
  | None -> ()
  | Some turn ->
    turn.status <- Completed;
    turn.duration_ms <- Some duration_ms;
    t.current <- None;
    t.total_processed <- t.total_processed + 1

(** Mark current turn as failed *)
let fail_turn t ~error =
  match t.current with
  | None -> ()
  | Some turn ->
    turn.status <- Failed error;
    t.current <- None;
    t.total_failed <- t.total_failed + 1

(** {1 Queue Operations} *)

(** Clear all pending turns *)
let clear t =
  t.queue <- [];
  t.current <- None

(** Remove turns from specific agent *)
let remove_agent_turns t ~agent_id =
  t.queue <- List.filter (fun turn -> turn.agent_id <> agent_id) t.queue

(** Get all turns from specific agent *)
let get_agent_turns t ~agent_id =
  List.filter (fun turn -> turn.agent_id = agent_id) t.queue

(** Get all queued turns *)
let get_all_turns t = t.queue

(** {1 Priority Management} *)

(** Agent priority presets (MAGI agents) *)
let agent_priority = function
  | "claude" | "balthasar" -> 1   (* Mirror - moderate priority *)
  | "codex" | "melchior" -> 2     (* Scientist - high priority for technical *)
  | "gemini" | "casper" -> 2      (* Strategist *)
  | _ -> 3                        (* Default *)

(** Enqueue with auto-priority based on agent *)
let enqueue_auto_priority t ~agent_id ~message ?voice () =
  let priority = agent_priority agent_id in
  enqueue t ~agent_id ~message ?voice ~priority ()

(** {1 Processing Loop} *)

(** Process queue loop - calls handler for each turn
    Returns when queue is empty or stop is requested *)
let rec process_loop t ~handler ~stop_signal =
  let open Lwt.Syntax in
  if !stop_signal || (is_empty t && t.current = None) then begin
    t.is_processing <- false;
    Lwt.return_unit
  end else begin
    t.is_processing <- true;
    match dequeue t with
    | None ->
      (* Queue empty, wait a bit and check again *)
      let* () = Lwt_unix.sleep 0.1 in
      process_loop t ~handler ~stop_signal
    | Some turn ->
      (* Process this turn *)
      let start_time = Unix.gettimeofday () in
      let* result =
        Lwt.catch
          (fun () ->
            let+ () = handler turn in
            Ok ())
          (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
      in
      let duration_ms = int_of_float ((Unix.gettimeofday () -. start_time) *. 1000.) in
      (match result with
       | Ok () -> complete_turn t ~duration_ms
       | Error msg -> fail_turn t ~error:msg);
      (* Continue processing *)
      process_loop t ~handler ~stop_signal
  end

(** Start processing loop in background *)
let start_processing t ~handler =
  let stop_signal = ref false in
  let _task = Lwt.async (fun () -> process_loop t ~handler ~stop_signal) in
  stop_signal  (* Return stop signal for control *)

(** {1 Status and JSON} *)

(** Get queue status as JSON *)
let status_json t =
  let turn_to_json turn =
    `Assoc [
      ("id", `String turn.id);
      ("agent_id", `String turn.agent_id);
      ("message", `String (String.sub turn.message 0 (min 50 (String.length turn.message))));
      ("voice", `String turn.voice);
      ("priority", `Int turn.priority);
      ("status", `String (string_of_status turn.status));
    ]
  in
  `Assoc [
    ("queue_length", `Int (length t));
    ("is_processing", `Bool t.is_processing);
    ("current_speaker", match t.current with
      | None -> `Null
      | Some turn -> `String turn.agent_id);
    ("total_processed", `Int t.total_processed);
    ("total_failed", `Int t.total_failed);
    ("pending", `List (List.map turn_to_json t.queue));
  ]

(** {1 Pretty Printing} *)

let pp_turn fmt turn =
  Format.fprintf fmt "Turn(%s, agent=%s, priority=%d, status=%s)"
    turn.id turn.agent_id turn.priority (string_of_status turn.status)

let pp fmt t =
  Format.fprintf fmt "TurnQueue(len=%d, processing=%b, processed=%d, failed=%d)"
    (length t) t.is_processing t.total_processed t.total_failed
