(** MASC Response - Standardized API Response Envelope

    Provides consistent response format across all MCP tools:
    - success: bool flag for quick status check
    - data: actual response payload
    - message: human-readable summary
    - errors: detailed error list with recovery hints

    MAGI Recommendation: API/UX Designer #1 (CRITICAL - Inconsistent formats)

    Usage:
    {[
      Response.ok ~message:"Task claimed" (`Assoc [("task_id", `String "task-001")])
      |> Response.to_json
      |> Yojson.Safe.pretty_to_string
    ]}
*)

(** Error severity levels *)
type severity =
  | Fatal    (* Operation completely failed *)
  | Warning  (* Partial success or recoverable issue *)
  | Info     (* Informational, no action needed *)

(** Detailed error information with recovery hints *)
type error_detail = {
  code: string;           (* Machine-readable error code *)
  severity: severity;
  message: string;        (* Human-readable description *)
  recovery_hints: string list;  (* Actionable suggestions *)
}

(** Standard tool response *)
type t = {
  success: bool;
  data: Yojson.Safe.t;
  message: string;
  errors: error_detail list;
  timestamp: float;
}

(** Severity to string *)
let severity_to_string = function
  | Fatal -> "fatal"
  | Warning -> "warning"
  | Info -> "info"

(** Severity from string - returns Result for safety *)
let severity_of_string = function
  | "fatal" -> Ok Fatal
  | "warning" -> Ok Warning
  | "info" -> Ok Info
  | s -> Error (Printf.sprintf "Unknown severity: %s" s)

(** Severity from string with default (for backwards compatibility) *)
let severity_of_string_default ?(default=Info) s =
  match severity_of_string s with
  | Ok sev -> sev
  | Error _ -> default

(** Error detail to JSON *)
let error_to_json (e : error_detail) : Yojson.Safe.t =
  `Assoc [
    ("code", `String e.code);
    ("severity", `String (severity_to_string e.severity));
    ("message", `String e.message);
    ("recovery_hints", `List (List.map (fun h -> `String h) e.recovery_hints));
  ]

(** Response to JSON *)
let to_json (r : t) : Yojson.Safe.t =
  `Assoc [
    ("success", `Bool r.success);
    ("data", r.data);
    ("message", `String r.message);
    ("errors", `List (List.map error_to_json r.errors));
    ("timestamp", `Float r.timestamp);
  ]

(** Response to pretty JSON string *)
let to_string (r : t) : string =
  Yojson.Safe.pretty_to_string (to_json r)

(** Create a success response *)
let ok ?(message="OK") (data : Yojson.Safe.t) : t =
  {
    success = true;
    data;
    message;
    errors = [];
    timestamp = Unix.gettimeofday ();
  }

(** Create a success response with warnings *)
let ok_with_warnings ?(message="OK") ~warnings (data : Yojson.Safe.t) : t =
  {
    success = true;
    data;
    message;
    errors = warnings;
    timestamp = Unix.gettimeofday ();
  }

(** Create an error response *)
let error ?(data=`Null) ~code ~message ?(hints=[]) () : t =
  {
    success = false;
    data;
    message;
    errors = [{
      code;
      severity = Fatal;
      message;
      recovery_hints = hints;
    }];
    timestamp = Unix.gettimeofday ();
  }

(** Create an error response with multiple errors *)
let errors ?(data=`Null) ~message (error_list : error_detail list) : t =
  {
    success = false;
    data;
    message;
    errors = error_list;
    timestamp = Unix.gettimeofday ();
  }

(** Helper: Create an error detail *)
let make_error ~code ?(severity=Fatal) ~message ?(hints=[]) () : error_detail =
  { code; severity; message; recovery_hints = hints }

(** Helper: Create a warning detail *)
let make_warning ~code ~message ?(hints=[]) () : error_detail =
  { code; severity = Warning; message; recovery_hints = hints }

(** Helper: Create an info detail *)
let make_info ~code ~message () : error_detail =
  { code; severity = Info; message; recovery_hints = [] }

(* ========================================
   Common Error Types with Recovery Hints
   ======================================== *)

(** Validation error with hints *)
let validation_error ~field ~reason : t =
  error
    ~code:"VALIDATION_ERROR"
    ~message:(Printf.sprintf "Validation failed for '%s': %s" field reason)
    ~hints:[
      Printf.sprintf "Check the format of '%s'" field;
      "Refer to the API documentation for valid values";
    ]
    ()

(** Not found error with hints *)
let not_found ~resource ~id : t =
  error
    ~code:"NOT_FOUND"
    ~message:(Printf.sprintf "%s '%s' not found" resource id)
    ~hints:[
      Printf.sprintf "Verify the %s ID is correct" (String.lowercase_ascii resource);
      Printf.sprintf "Use list_%ss to see available items" (String.lowercase_ascii resource);
    ]
    ()

(** Already exists error *)
let already_exists ~resource ~id : t =
  error
    ~code:"ALREADY_EXISTS"
    ~message:(Printf.sprintf "%s '%s' already exists" resource id)
    ~hints:[
      Printf.sprintf "Use a different ID for the new %s" (String.lowercase_ascii resource);
      Printf.sprintf "Or update the existing %s instead" (String.lowercase_ascii resource);
    ]
    ()

(** Permission denied error *)
let permission_denied ~action ~resource : t =
  error
    ~code:"PERMISSION_DENIED"
    ~message:(Printf.sprintf "Permission denied: cannot %s %s" action resource)
    ~hints:[
      "Check if you have the required permissions";
    ]
    ()

(** Conflict error (e.g., concurrent modification) *)
let conflict ~resource ~reason : t =
  error
    ~code:"CONFLICT"
    ~message:(Printf.sprintf "Conflict on %s: %s" resource reason)
    ~hints:[
      "Refresh and try the operation again";
      "Check if another agent is modifying the same resource";
    ]
    ()

(** Timeout error *)
let timeout ~operation : t =
  error
    ~code:"TIMEOUT"
    ~message:(Printf.sprintf "Operation '%s' timed out" operation)
    ~hints:[
      "Try the operation again";
      "The operation may take longer than expected";
      "Consider breaking the operation into smaller parts";
    ]
    ()

(* ========================================
   Drift-Specific Responses (MAGI #2)
   ======================================== *)

(** Drift detection result with recovery hints
    @param threshold The similarity threshold used for detection (pass from caller) *)
let drift_detected ~similarity ~drift_type ~threshold ~details : t =
  let hints = match drift_type with
    | "factual" -> [
        "Re-request context from source agent";
        "Check for content truncation during handoff";
        "Verify the original context is still available";
      ]
    | "semantic" -> [
        "Verify task understanding with source agent";
        Printf.sprintf "Consider lowering the threshold (current: %.2f)" threshold;
        "Review the transformation applied to context";
      ]
    | "structural" -> [
        "Check for format conversion issues";
        "Verify both sides use compatible data formats";
      ]
    | _ -> [
        "Review the handoff process";
        "Check for network or encoding issues";
      ]
  in
  {
    success = false;
    data = `Assoc [
      ("similarity", `Float similarity);
      ("drift_type", `String drift_type);
      ("threshold", `Float threshold);
    ];
    message = Printf.sprintf "Context drift detected (%.0f%% similarity)" (similarity *. 100.0);
    errors = [{
      code = "DRIFT_DETECTED";
      severity = Warning;
      message = details;
      recovery_hints = hints;
    }];
    timestamp = Unix.gettimeofday ();
  }

(** Handoff verified successfully *)
let handoff_verified ~similarity : t =
  ok
    ~message:(Printf.sprintf "Context verified (%.0f%% similarity)" (similarity *. 100.0))
    (`Assoc [
      ("similarity", `Float similarity);
      ("verified", `Bool true);
    ])

(* ========================================
   Task-Specific Responses
   ======================================== *)

(** Task claimed *)
let task_claimed ~task_id ~agent : t =
  ok
    ~message:(Printf.sprintf "Task '%s' claimed by '%s'" task_id agent)
    (`Assoc [
      ("task_id", `String task_id);
      ("claimed_by", `String agent);
      ("status", `String "in_progress");
    ])

(** Task already claimed *)
let task_already_claimed ~task_id ~claimed_by : t =
  error
    ~code:"TASK_CLAIMED"
    ~message:(Printf.sprintf "Task '%s' already claimed" task_id)
    ~data:(`Assoc [
      ("task_id", `String task_id);
      ("claimed_by", `String claimed_by);
    ])
    ~hints:[
      Printf.sprintf "Coordinate with '%s' who owns this task" claimed_by;
      "Use masc_claim_next to get the next available task";
      "Or create a new task with masc_add_task";
    ]
    ()

(** Task completed *)
let task_completed ~task_id ~agent ~notes : t =
  ok
    ~message:(Printf.sprintf "Task '%s' completed" task_id)
    (`Assoc [
      ("task_id", `String task_id);
      ("completed_by", `String agent);
      ("notes", `String notes);
      ("status", `String "completed");
    ])
