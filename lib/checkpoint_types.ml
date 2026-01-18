(** Checkpoint types and pure functions for testability
    Extracted from bin/masc_checkpoint.ml for unit testing *)

(** Checkpoint status types *)
type checkpoint_status =
  | Pending
  | InProgress
  | Interrupted
  | Completed
  | Rejected
  | Reverted  (* Time Travel: checkpoint was superseded by revert *)
  | Branched  (* Checkpoint Branching: this checkpoint spawned a new branch *)
[@@deriving show, eq]

(** Parse status from string *)
let status_of_string = function
  | "pending" -> Some Pending
  | "in_progress" -> Some InProgress
  | "interrupted" -> Some Interrupted
  | "completed" -> Some Completed
  | "rejected" -> Some Rejected
  | "reverted" -> Some Reverted
  | "branched" -> Some Branched
  | _ -> None

(** Convert status to string *)
let status_to_string = function
  | Pending -> "pending"
  | InProgress -> "in_progress"
  | Interrupted -> "interrupted"
  | Completed -> "completed"
  | Rejected -> "rejected"
  | Reverted -> "reverted"
  | Branched -> "branched"

(** Generate checkpoint ID from components *)
let make_checkpoint_id ~task_id ~step ~timestamp =
  Printf.sprintf "cp-%s-%d-%d" task_id step timestamp

(** Generate checkpoint ID with current time *)
let make_checkpoint_id_now ~task_id ~step =
  let timestamp = int_of_float (Unix.time ()) in
  make_checkpoint_id ~task_id ~step ~timestamp

(** Format date from Unix time *)
let format_date unix_time =
  let tm = Unix.localtime unix_time in
  Printf.sprintf "%04d-%02d-%02d" (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday

(** Get today's date as string *)
let today () = format_date (Unix.time ())

(** Validate task_id format (non-empty, alphanumeric with dashes) *)
let is_valid_task_id task_id =
  task_id <> "" &&
  String.length task_id <= 100 &&
  String.for_all (fun c ->
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    (c >= '0' && c <= '9') ||
    c = '-' || c = '_'
  ) task_id

(** Validate step number (positive) *)
let is_valid_step step = step > 0

(** Status transition validation (state machine) *)
let can_transition ~from ~to_ =
  match from, to_ with
  | Pending, InProgress -> true
  | InProgress, Interrupted -> true
  | InProgress, Completed -> true
  | Interrupted, Completed -> true  (* approved *)
  | Interrupted, Rejected -> true   (* rejected *)
  | _ -> false

(** Check if status is terminal (no more transitions) *)
let is_terminal = function
  | Completed | Rejected | Reverted -> true
  | _ -> false

(** Check if status requires user action *)
let requires_user_action = function
  | Interrupted -> true
  | _ -> false

(** Timeout calculation helper *)
let is_timed_out ~created_at ~timeout_minutes =
  let now = Unix.time () in
  let timeout_seconds = float_of_int (timeout_minutes * 60) in
  now -. created_at > timeout_seconds

(** Parse JSON safely *)
let parse_json_string s =
  try Some (Yojson.Safe.from_string s)
  with _ -> None

(** Validate JSON state string *)
let is_valid_json_state state =
  match parse_json_string state with
  | Some _ -> true
  | None -> false

(** Checkpoint record type for return values *)
type checkpoint_info = {
  id: string;
  task_id: string;
  step: int;
  action: string;
  agent: string;
  status: checkpoint_status;
  interrupt_message: string option;
  created_at: float option;
}

(** Create checkpoint info from components *)
let make_checkpoint_info ~id ~task_id ~step ~action ~agent ~status ?interrupt_message ?created_at () =
  { id; task_id; step; action; agent; status; interrupt_message; created_at }
