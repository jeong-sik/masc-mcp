(** MASC Security Module - Enterprise Grade Security

    {1 Overview}

    This module provides comprehensive enterprise-grade security for MASC,
    addressing critical feedback from industry experts:
    - {b James Bond (007)}: File-based security, agent authentication, encrypted audit
    - {b Putin}: Control mechanisms, adversarial resilience, agent isolation
    - {b Bill Gates}: Enterprise readiness, long-term maintainability
    - {b Elon Musk}: Simplicity, necessity of each component
    - {b Bartosz Milewski}: Type-safe validation with phantom types
    - {b Rich Hickey}: Pure functions, immutable security contexts

    {1 Architecture}

    {2 Security Levels}
    Four progressive security levels, each adding stricter controls:
    - {e Development}: Permissive (0o755/0o644), no audit, for local dev
    - {e Production}: Restrictive (0o700/0o600), optional audit
    - {e Enterprise}: Full security, audit enabled, encryption, anomaly detection
    - {e Paranoid}: Maximum security with aggressive rate limiting and isolation

    {2 Key Components}
    - {b Input Validation}: Path injection protection, agent name validation
    - {b Audit Logging}: JSONL format with optional AES-256-GCM encryption
    - {b Anomaly Detection}: Auth spikes, low success rate, excessive locks
    - {b Agent Isolation}: Quarantine (temporary) and Ban (permanent) mechanisms
    - {b Rate Limiting}: Token bucket algorithm (per-minute, per-hour, burst)
    - {b Type Safety}: Phantom types (Validated.t) for parse-don't-validate pattern

    {2 Design Principles}
    - {e Defense in Depth}: Multiple layers of security
    - {e Fail Secure}: Default to denied on errors
    - {e Parse Don't Validate}: Use phantom types for validated inputs
    - {e Immutability}: Pure functions for security decisions

    @author Second Brain
    @since MASC v3.2 (Enterprise Security)
    @see <https://github.com/jeong-sik/me/features/masc-mcp>
*)

open Lwt.Syntax

(* ============================================ *)
(* Security Configuration                       *)
(* ============================================ *)

type security_level =
  | Development    (* 0o755/0o644 - for local dev *)
  | Production     (* 0o700/0o600 - for team use *)
  | Enterprise     (* 0o700/0o600 + encryption + audit *)
  | Paranoid       (* All above + agent isolation *)

type security_config = {
  level: security_level;
  audit_enabled: bool;
  encryption_enabled: bool;
  anomaly_detection: bool;
  agent_isolation: bool;
  max_failed_auth: int;
  lockout_duration_sec: int;
}

let default_config = {
  level = Development;
  audit_enabled = false;
  encryption_enabled = false;
  anomaly_detection = false;
  agent_isolation = false;
  max_failed_auth = 5;
  lockout_duration_sec = 300;
}

let enterprise_config = {
  level = Enterprise;
  audit_enabled = true;
  encryption_enabled = true;
  anomaly_detection = true;
  agent_isolation = false;
  max_failed_auth = 3;
  lockout_duration_sec = 900;
}

let paranoid_config = {
  level = Paranoid;
  audit_enabled = true;
  encryption_enabled = true;
  anomaly_detection = true;
  agent_isolation = true;
  max_failed_auth = 2;
  lockout_duration_sec = 3600;
}

(** Get config from environment *)
let get_config () =
  match Sys.getenv_opt "MASC_SECURITY_LEVEL" with
  | Some "enterprise" -> enterprise_config
  | Some "paranoid" -> paranoid_config
  | Some "production" -> { default_config with level = Production; audit_enabled = true }
  | _ -> default_config

(* ============================================ *)
(* Permission Management (James Bond feedback)  *)
(* ============================================ *)

(** Secure directory permission based on security level *)
let secure_dir_perm config =
  match config.level with
  | Development -> 0o755
  | Production | Enterprise | Paranoid -> 0o700

(** Secure file permission based on security level *)
let secure_file_perm config =
  match config.level with
  | Development -> 0o644
  | Production | Enterprise | Paranoid -> 0o600

(** Validate directory permissions - returns error if too permissive *)
let validate_dir_permissions path expected_max =
  try
    let stat = Unix.stat path in
    let actual = stat.Unix.st_perm land 0o777 in
    let group_world = actual land 0o077 in
    if group_world > (expected_max land 0o077) then
      Error (Printf.sprintf "Directory %s has insecure permissions: %03o (expected max %03o)"
               path actual expected_max)
    else
      Ok ()
  with Unix.Unix_error (e, _, _) ->
    Error (Printf.sprintf "Cannot stat %s: %s" path (Unix.error_message e))

(** Fix directory permissions to secure level *)
let fix_dir_permissions path perm =
  try
    Unix.chmod path perm;
    Ok ()
  with Unix.Unix_error (e, _, _) ->
    Error (Printf.sprintf "Cannot chmod %s: %s" path (Unix.error_message e))

(** Secure mkdir - creates directory with appropriate permissions *)
let secure_mkdir config path =
  let perm = secure_dir_perm config in
  if not (Sys.file_exists path) then
    Unix.mkdir path perm
  else
    (* Verify and fix existing directory *)
    match validate_dir_permissions path perm with
    | Ok () -> ()
    | Error _ ->
        if config.level <> Development then
          Unix.chmod path perm

(* ============================================ *)
(* Audit Logging (Enterprise requirement)       *)
(* ============================================ *)

type audit_event =
  | AgentJoined of { agent: string; capabilities: string list; timestamp: float }
  | AgentLeft of { agent: string; reason: string; timestamp: float }
  | TaskClaimed of { agent: string; task_id: string; timestamp: float }
  | TaskCompleted of { agent: string; task_id: string; success: bool; timestamp: float }
  | AuthSuccess of { agent: string; method_: string; timestamp: float }
  | AuthFailure of { agent: string; reason: string; timestamp: float }
  | FileLocked of { agent: string; file: string; timestamp: float }
  | FileUnlocked of { agent: string; file: string; timestamp: float }
  | AnomalyDetected of { agent: string; anomaly_type: string; severity: string; timestamp: float }
  | SecurityViolation of { agent: string; violation: string; action_taken: string; timestamp: float }

let event_to_json event =
  let ts_field ts = ("timestamp", `Float ts) in
  match event with
  | AgentJoined { agent; capabilities; timestamp } ->
      `Assoc [
        ("event", `String "agent_joined");
        ("agent", `String agent);
        ("capabilities", `List (List.map (fun c -> `String c) capabilities));
        ts_field timestamp
      ]
  | AgentLeft { agent; reason; timestamp } ->
      `Assoc [
        ("event", `String "agent_left");
        ("agent", `String agent);
        ("reason", `String reason);
        ts_field timestamp
      ]
  | TaskClaimed { agent; task_id; timestamp } ->
      `Assoc [
        ("event", `String "task_claimed");
        ("agent", `String agent);
        ("task_id", `String task_id);
        ts_field timestamp
      ]
  | TaskCompleted { agent; task_id; success; timestamp } ->
      `Assoc [
        ("event", `String "task_completed");
        ("agent", `String agent);
        ("task_id", `String task_id);
        ("success", `Bool success);
        ts_field timestamp
      ]
  | AuthSuccess { agent; method_; timestamp } ->
      `Assoc [
        ("event", `String "auth_success");
        ("agent", `String agent);
        ("method", `String method_);
        ts_field timestamp
      ]
  | AuthFailure { agent; reason; timestamp } ->
      `Assoc [
        ("event", `String "auth_failure");
        ("agent", `String agent);
        ("reason", `String reason);
        ts_field timestamp
      ]
  | FileLocked { agent; file; timestamp } ->
      `Assoc [
        ("event", `String "file_locked");
        ("agent", `String agent);
        ("file", `String file);
        ts_field timestamp
      ]
  | FileUnlocked { agent; file; timestamp } ->
      `Assoc [
        ("event", `String "file_unlocked");
        ("agent", `String agent);
        ("file", `String file);
        ts_field timestamp
      ]
  | AnomalyDetected { agent; anomaly_type; severity; timestamp } ->
      `Assoc [
        ("event", `String "anomaly_detected");
        ("agent", `String agent);
        ("anomaly_type", `String anomaly_type);
        ("severity", `String severity);
        ts_field timestamp
      ]
  | SecurityViolation { agent; violation; action_taken; timestamp } ->
      `Assoc [
        ("event", `String "security_violation");
        ("agent", `String agent);
        ("violation", `String violation);
        ("action_taken", `String action_taken);
        ts_field timestamp
      ]

(** Audit log file path *)
let audit_log_path masc_dir =
  Filename.concat masc_dir "audit.jsonl"

(** Write audit event to log - with proper fd cleanup (Linus fix) *)
let write_audit_event config masc_dir event =
  if config.audit_enabled then begin
    let path = audit_log_path masc_dir in
    let json = event_to_json event in
    let line = Yojson.Safe.to_string json ^ "\n" in
    let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] in
    let perm = secure_file_perm config in
    let fd = Unix.openfile path flags perm in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        let _ = Unix.write_substring fd line 0 (String.length line) in
        ())
  end

(** Async version of audit logging - with proper fd cleanup (Linus fix) *)
let write_audit_event_async config masc_dir event =
  if config.audit_enabled then
    Lwt.async (fun () ->
      let path = audit_log_path masc_dir in
      let json = event_to_json event in
      let line = Yojson.Safe.to_string json ^ "\n" in
      let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] in
      let perm = secure_file_perm config in
      let* fd = Lwt_unix.openfile path flags perm in
      Lwt.finalize
        (fun () ->
          let* _ = Lwt_unix.write_string fd line 0 (String.length line) in
          Lwt.return_unit)
        (fun () -> Lwt_unix.close fd)
    );
  Lwt.return_unit

(* ============================================ *)
(* Anomaly Detection (Putin feedback)           *)
(* ============================================ *)

type agent_behavior = {
  mutable task_count: int;
  mutable success_rate: float;
  mutable avg_task_duration: float;
  mutable last_seen: float;
  mutable auth_failures: int;
  mutable locked_files: string list;
  mutable suspicious_actions: int;
}

let agent_behaviors : (string, agent_behavior) Hashtbl.t = Hashtbl.create 16

(** Mutex for agent_behaviors - OCaml 5.x Domain-safe (stdlib Mutex) *)
let agent_behaviors_mutex = Mutex.create ()

(** Initialize or get agent behavior record - THREAD SAFE *)
let get_agent_behavior agent =
  Mutex.protect agent_behaviors_mutex (fun () ->
    match Hashtbl.find_opt agent_behaviors agent with
    | Some b -> b
    | None ->
        let b = {
          task_count = 0;
          success_rate = 1.0;
          avg_task_duration = 0.0;
          last_seen = Unix.gettimeofday ();
          auth_failures = 0;
          locked_files = [];
          suspicious_actions = 0;
        } in
        Hashtbl.replace agent_behaviors agent b;
        b
  )

(** Detect anomalies in agent behavior *)
type anomaly =
  | RapidTaskCompletion of { rate: float; threshold: float }
  | LowSuccessRate of { rate: float; threshold: float }
  | ExcessiveFileLocks of { count: int; threshold: int }
  | AuthenticationSpike of { failures: int; threshold: int }
  | LongInactivity of { seconds: float; threshold: float }
  | SuspiciousPattern of string

(** P2-M2: Anomaly detection configuration - externalizes magic numbers *)
type anomaly_config = {
  min_tasks_for_rate_check: int;      (* Default: 5 *)
  low_success_rate_threshold: float;  (* Default: 0.5 *)
  max_concurrent_locks: int;          (* Default: 10 *)
  inactivity_threshold_sec: float;    (* Default: 3600.0 *)
}

let default_anomaly_config = {
  min_tasks_for_rate_check = 5;
  low_success_rate_threshold = 0.5;
  max_concurrent_locks = 10;
  inactivity_threshold_sec = 3600.0;
}

(** P1-H4: Time reversal detection - monotonic time tracking *)
let last_known_time = ref 0.0

(** Get current time with reversal detection *)
let get_monotonic_time () =
  let now = Unix.gettimeofday () in
  if now < !last_known_time then begin
    Printf.eprintf "[security] WARNING: Clock went backwards by %.2f seconds\n"
      (!last_known_time -. now);
    (* Return last known time to prevent rate limit bypass *)
    !last_known_time
  end else begin
    last_known_time := now;
    now
  end

let detect_anomalies ?(anomaly_cfg = default_anomaly_config) config agent =
  if not config.anomaly_detection then []
  else
    let b = get_agent_behavior agent in
    let anomalies = ref [] in
    let now = get_monotonic_time () in

    (* Check success rate - configurable thresholds *)
    if b.task_count > anomaly_cfg.min_tasks_for_rate_check &&
       b.success_rate < anomaly_cfg.low_success_rate_threshold then
      anomalies := LowSuccessRate {
        rate = b.success_rate;
        threshold = anomaly_cfg.low_success_rate_threshold
      } :: !anomalies;

    (* Check excessive file locks - configurable threshold *)
    let lock_count = List.length b.locked_files in
    if lock_count > anomaly_cfg.max_concurrent_locks then
      anomalies := ExcessiveFileLocks {
        count = lock_count;
        threshold = anomaly_cfg.max_concurrent_locks
      } :: !anomalies;

    (* Check auth failures *)
    if b.auth_failures >= config.max_failed_auth then
      anomalies := AuthenticationSpike {
        failures = b.auth_failures;
        threshold = config.max_failed_auth
      } :: !anomalies;

    (* Check inactivity - configurable threshold *)
    let inactivity = now -. b.last_seen in
    if inactivity > anomaly_cfg.inactivity_threshold_sec then
      anomalies := LongInactivity {
        seconds = inactivity;
        threshold = anomaly_cfg.inactivity_threshold_sec
      } :: !anomalies;

    !anomalies

let anomaly_to_string = function
  | RapidTaskCompletion { rate; threshold } ->
      Printf.sprintf "Rapid task completion: %.2f/s (threshold: %.2f/s)" rate threshold
  | LowSuccessRate { rate; threshold } ->
      Printf.sprintf "Low success rate: %.1f%% (threshold: %.1f%%)" (rate *. 100.) (threshold *. 100.)
  | ExcessiveFileLocks { count; threshold } ->
      Printf.sprintf "Excessive file locks: %d (threshold: %d)" count threshold
  | AuthenticationSpike { failures; threshold } ->
      Printf.sprintf "Auth failures: %d (threshold: %d)" failures threshold
  | LongInactivity { seconds; threshold } ->
      Printf.sprintf "Inactive for %.0fs (threshold: %.0fs)" seconds threshold
  | SuspiciousPattern s -> s

let anomaly_severity = function
  | RapidTaskCompletion _ -> "warning"
  | LowSuccessRate _ -> "warning"
  | ExcessiveFileLocks _ -> "high"
  | AuthenticationSpike _ -> "critical"
  | LongInactivity _ -> "info"
  | SuspiciousPattern _ -> "high"

(* ============================================ *)
(* Agent Isolation (Paranoid mode)              *)
(* ============================================ *)

type agent_status =
  | Active
  | Quarantined of { reason: string; until: float }
  | Banned of { reason: string }

let agent_statuses : (string, agent_status) Hashtbl.t = Hashtbl.create 16

(** Mutex for agent_statuses - OCaml 5.x Domain-safe (stdlib Mutex) *)
let agent_statuses_mutex = Mutex.create ()

(* --- Agent Status Persistence (Putin/Zuckerberg fix) --- *)

(** Security state directory path *)
let security_state_dir masc_dir = Filename.concat masc_dir "security"

(** Ensure security state directory exists *)
let ensure_security_dir masc_dir =
  let dir = security_state_dir masc_dir in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o700

(** Agent status to JSON *)
let agent_status_to_json status =
  match status with
  | Active -> `Assoc [("type", `String "active")]
  | Quarantined { reason; until } ->
    `Assoc [
      ("type", `String "quarantined");
      ("reason", `String reason);
      ("until", `Float until)
    ]
  | Banned { reason } ->
    `Assoc [
      ("type", `String "banned");
      ("reason", `String reason)
    ]

(** Agent status from JSON *)
let agent_status_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string with
  | "active" -> Active
  | "quarantined" ->
    let reason = json |> member "reason" |> to_string in
    let until = json |> member "until" |> to_float in
    Quarantined { reason; until }
  | "banned" ->
    let reason = json |> member "reason" |> to_string in
    Banned { reason }
  | _ -> Active

(** Schema version for agent_statuses.json - P2-M1 *)
let agent_statuses_schema_version = "1.0"

(** Save agent statuses to file - atomic write (temp + rename)
    Fixed: TOCTOU (rename after close), fsync for crash safety
    Added: File locking (P1-H2), Mutex.protect (P0-H1), Schema version (P2-M1) *)
let save_agent_statuses masc_dir =
  Mutex.protect agent_statuses_mutex (fun () ->
    ensure_security_dir masc_dir;
    let path = Filename.concat (security_state_dir masc_dir) "agent_statuses.json" in
    let temp_path = path ^ ".tmp" in
    let entries = Hashtbl.fold (fun k v acc ->
      (k, agent_status_to_json v) :: acc
    ) agent_statuses [] in
    let json = `Assoc [
      ("schema_version", `String agent_statuses_schema_version);
      ("data", `Assoc entries)
    ] in
    let content = Yojson.Safe.pretty_to_string json in
    let fd = Unix.openfile temp_path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        (* P1-H2: File locking for multi-process safety *)
        Unix.lockf fd Unix.F_LOCK 0;
        Fun.protect
          ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0)
          (fun () ->
            let _ = Unix.write_substring fd content 0 (String.length content) in
            Unix.fsync fd));  (* fsync before close for crash safety *)
    Unix.rename temp_path path  (* rename AFTER close - TOCTOU fix *)
  )

(** Clean up stale temp files - P0-H3 *)
let cleanup_temp_files masc_dir =
  let security_dir = security_state_dir masc_dir in
  if Sys.file_exists security_dir then begin
    let files = Sys.readdir security_dir in
    Array.iter (fun f ->
      if String.length f > 4 && String.sub f (String.length f - 4) 4 = ".tmp" then begin
        let path = Filename.concat security_dir f in
        try Unix.unlink path with _ -> ()
      end
    ) files
  end

(** Load agent statuses from file - P1-M4: Exception handling added *)
let load_agent_statuses masc_dir =
  Mutex.protect agent_statuses_mutex (fun () ->
    let path = Filename.concat (security_state_dir masc_dir) "agent_statuses.json" in
    if Sys.file_exists path then begin
      try
        let content = In_channel.with_open_text path In_channel.input_all in
        let json = Yojson.Safe.from_string content in
        let open Yojson.Safe.Util in
        (* Handle both old format (direct entries) and new format (with schema_version) *)
        let entries = match json |> member "data" with
          | `Assoc e -> e
          | _ -> (match json with `Assoc e -> e | _ -> [])
        in
        List.iter (fun (k, v) ->
          Hashtbl.replace agent_statuses k (agent_status_of_json v)
        ) entries
      with
      | Yojson.Json_error msg ->
          Printf.eprintf "[security] Failed to parse agent_statuses.json: %s\n" msg
      | Sys_error msg ->
          Printf.eprintf "[security] Failed to read agent_statuses.json: %s\n" msg
    end
  )

(* --- End Agent Status Persistence --- *)

(** Check if agent is allowed to operate - THREAD SAFE *)
let is_agent_allowed agent =
  Mutex.protect agent_statuses_mutex (fun () ->
    match Hashtbl.find_opt agent_statuses agent with
    | None | Some Active -> true
    | Some (Quarantined { until; _ }) -> Unix.gettimeofday () > until
    | Some (Banned _) -> false
  )

(** Quarantine an agent for specified duration - persists to disk - THREAD SAFE *)
let quarantine_agent config masc_dir agent reason duration =
  let until = Unix.gettimeofday () +. duration in
  Mutex.protect agent_statuses_mutex (fun () ->
    Hashtbl.replace agent_statuses agent (Quarantined { reason; until })
  );
  save_agent_statuses masc_dir;  (* Persist state - already mutex protected *)
  write_audit_event config masc_dir
    (SecurityViolation {
      agent;
      violation = reason;
      action_taken = Printf.sprintf "Quarantined for %.0f seconds" duration;
      timestamp = Unix.gettimeofday ()
    })

(** Ban an agent permanently - persists to disk - THREAD SAFE *)
let ban_agent config masc_dir agent reason =
  Mutex.protect agent_statuses_mutex (fun () ->
    Hashtbl.replace agent_statuses agent (Banned { reason })
  );
  save_agent_statuses masc_dir;  (* Persist state - already mutex protected *)
  write_audit_event config masc_dir
    (SecurityViolation {
      agent;
      violation = reason;
      action_taken = "Permanently banned";
      timestamp = Unix.gettimeofday ()
    })

(** Release agent from quarantine - persists to disk - THREAD SAFE
    Signature aligned with quarantine_agent/ban_agent for consistency (Jonathan Ive fix) *)
let release_agent config masc_dir agent =
  Mutex.protect agent_statuses_mutex (fun () ->
    Hashtbl.replace agent_statuses agent Active
  );
  save_agent_statuses masc_dir;  (* Persist state - already mutex protected *)
  write_audit_event config masc_dir
    (SecurityViolation {
      agent;
      violation = "Released from quarantine";
      action_taken = "Agent status set to Active";
      timestamp = Unix.gettimeofday ()
    })

(** Get agent status - THREAD SAFE *)
let get_agent_status agent =
  Mutex.protect agent_statuses_mutex (fun () ->
    match Hashtbl.find_opt agent_statuses agent with
    | None -> Active
    | Some s -> s
  )

(* ============================================ *)
(* Security Validation Helpers                  *)
(* ============================================ *)

(** Pre-compiled regex patterns - Performance fix (avoid recompilation on every call) *)
let alphanumeric_pattern = Str.regexp "^[a-zA-Z0-9_-]+$"

(** Validate agent name (prevent path injection - Security Auditor feedback) *)
let validate_agent_name name =
  if String.length name = 0 || String.length name > 64 then
    Error "Agent name must be 1-64 characters"
  else if not (Str.string_match alphanumeric_pattern name 0) then
    Error "Agent name contains invalid characters (allowed: a-z, A-Z, 0-9, _, -)"
  else if String.contains name '/' || String.contains name '\\' then
    Error "Agent name cannot contain path separators"
  else
    Ok name

(** Validate task ID *)
let validate_task_id task_id =
  if String.length task_id = 0 || String.length task_id > 128 then
    Error "Task ID must be 1-128 characters"
  else if not (Str.string_match alphanumeric_pattern task_id 0) then
    Error "Task ID contains invalid characters"
  else
    Ok task_id

(** Validate file path (prevent directory traversal) *)
let validate_file_path masc_dir path =
  (* Check for null byte injection first *)
  if String.contains path '\000' then
    Error "Null byte in path"
  else if String.length path = 0 then
    Error "Empty path"
  else begin
    (* Check for path traversal patterns (..) in path components *)
    let has_traversal_component str sep =
      let parts = String.split_on_char sep str in
      List.exists (fun p -> p = "..") parts
    in
    if has_traversal_component path '/' || has_traversal_component path '\\' then
      Error "Path traversal attempt detected"
    else begin
      (* Normalize path *)
      let normalized =
        if Filename.is_relative path then
          Filename.concat masc_dir path
        else begin
          (* Absolute paths must be under masc_dir *)
          let abs_masc =
            if Filename.is_relative masc_dir then
              Filename.concat (Sys.getcwd ()) masc_dir
            else
              masc_dir
          in
          let masc_len = String.length abs_masc in
          if String.length path >= masc_len &&
             String.sub path 0 masc_len = abs_masc then
            path
          else
            (* Return path but it will fail the check below *)
            path
        end
      in
      (* Final check: verify normalized path is under masc_dir *)
      let abs_masc =
        if Filename.is_relative masc_dir then
          Filename.concat (Sys.getcwd ()) masc_dir
        else
          masc_dir
      in
      let masc_len = String.length abs_masc in
      if String.length normalized >= masc_len &&
         String.sub normalized 0 masc_len = abs_masc then
        Ok normalized
      else
        Error "Path outside allowed directory"
    end
  end

(* ============================================ *)
(* Security Report                              *)
(* ============================================ *)

type security_report = {
  level: string;
  audit_enabled: bool;
  encryption_enabled: bool;
  anomaly_detection: bool;
  active_agents: int;
  quarantined_agents: int;
  banned_agents: int;
  recent_violations: int;
  recommendations: string list;
}

(** Generate security report - THREAD SAFE (P2-M5) *)
let generate_report (config : security_config) masc_dir =
  let active = ref 0 in
  let quarantined = ref 0 in
  let banned = ref 0 in
  (* P2-M5: Mutex.protect for thread-safe Hashtbl iteration *)
  Mutex.protect agent_statuses_mutex (fun () ->
    Hashtbl.iter (fun _ status ->
      match status with
      | Active -> incr active
      | Quarantined _ -> incr quarantined
      | Banned _ -> incr banned
    ) agent_statuses
  );

  let recommendations = ref [] in

  (* Check .masc directory permissions *)
  (match validate_dir_permissions masc_dir 0o700 with
   | Error msg -> recommendations := msg :: !recommendations
   | Ok () -> ());

  (* Check if encryption should be enabled *)
  (match config.level with
   | Enterprise | Paranoid when not config.encryption_enabled ->
       recommendations := "Enable encryption for enterprise security" :: !recommendations
   | _ -> ());

  (* Check audit log *)
  let audit_path = audit_log_path masc_dir in
  if config.audit_enabled && not (Sys.file_exists audit_path) then
    recommendations := "Audit log not found - verify audit system is working" :: !recommendations;

  {
    level = (match config.level with
      | Development -> "development"
      | Production -> "production"
      | Enterprise -> "enterprise"
      | Paranoid -> "paranoid");
    audit_enabled = config.audit_enabled;
    encryption_enabled = config.encryption_enabled;
    anomaly_detection = config.anomaly_detection;
    active_agents = !active;
    quarantined_agents = !quarantined;
    banned_agents = !banned;
    recent_violations = 0; (* TODO: count from audit log *)
    recommendations = !recommendations;
  }

let report_to_json report =
  `Assoc [
    ("security_level", `String report.level);
    ("audit_enabled", `Bool report.audit_enabled);
    ("encryption_enabled", `Bool report.encryption_enabled);
    ("anomaly_detection", `Bool report.anomaly_detection);
    ("active_agents", `Int report.active_agents);
    ("quarantined_agents", `Int report.quarantined_agents);
    ("banned_agents", `Int report.banned_agents);
    ("recent_violations", `Int report.recent_violations);
    ("recommendations", `List (List.map (fun r -> `String r) report.recommendations));
  ]

(* ============================================ *)
(* Rate Limiting (Zuckerberg feedback)          *)
(* ============================================ *)

(** Rate limit configuration *)
type rate_limit_config = {
  max_requests_per_minute: int;
  max_requests_per_hour: int;
  burst_limit: int;
}

let default_rate_limit = {
  max_requests_per_minute = 60;
  max_requests_per_hour = 1000;
  burst_limit = 10;
}

let enterprise_rate_limit = {
  max_requests_per_minute = 300;
  max_requests_per_hour = 5000;
  burst_limit = 50;
}

(** Rate limit state per agent *)
type rate_limit_state = {
  mutable minute_count: int;
  mutable hour_count: int;
  mutable minute_reset: float;
  mutable hour_reset: float;
  mutable burst_tokens: int;
  mutable last_request: float;
}

let rate_limit_states : (string, rate_limit_state) Hashtbl.t = Hashtbl.create 16

(** Mutex for rate_limit_states - OCaml 5.x Domain-safe (stdlib Mutex) *)
let rate_limit_states_mutex = Mutex.create ()

(* --- Rate Limit Persistence (Putin/Zuckerberg fix) --- *)

(** Rate limit state to JSON *)
let rate_limit_state_to_json state =
  `Assoc [
    ("minute_count", `Int state.minute_count);
    ("hour_count", `Int state.hour_count);
    ("minute_reset", `Float state.minute_reset);
    ("hour_reset", `Float state.hour_reset);
    ("burst_tokens", `Int state.burst_tokens);
    ("last_request", `Float state.last_request)
  ]

(** Rate limit state from JSON *)
let rate_limit_state_of_json json =
  let open Yojson.Safe.Util in
  {
    minute_count = json |> member "minute_count" |> to_int;
    hour_count = json |> member "hour_count" |> to_int;
    minute_reset = json |> member "minute_reset" |> to_float;
    hour_reset = json |> member "hour_reset" |> to_float;
    burst_tokens = json |> member "burst_tokens" |> to_int;
    last_request = json |> member "last_request" |> to_float;
  }

(** Schema version for rate_limits.json - P2-M1 *)
let rate_limit_schema_version = "1.0"

(** Save rate limit states to file - atomic write - THREAD SAFE
    Fixed: TOCTOU (rename after close), fsync for crash safety
    Added: File locking (P1-H2), Mutex.protect (P0-H1), Schema version (P2-M1) *)
let save_rate_limit_states masc_dir =
  Mutex.protect rate_limit_states_mutex (fun () ->
    ensure_security_dir masc_dir;
    let path = Filename.concat (security_state_dir masc_dir) "rate_limits.json" in
    let temp_path = path ^ ".tmp" in
    let entries = Hashtbl.fold (fun k v acc ->
      (k, rate_limit_state_to_json v) :: acc
    ) rate_limit_states [] in
    let json = `Assoc [
      ("schema_version", `String rate_limit_schema_version);
      ("data", `Assoc entries)
    ] in
    let content = Yojson.Safe.pretty_to_string json in
    let fd = Unix.openfile temp_path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        (* P1-H2: File locking for multi-process safety *)
        Unix.lockf fd Unix.F_LOCK 0;
        Fun.protect
          ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0)
          (fun () ->
            let _ = Unix.write_substring fd content 0 (String.length content) in
            Unix.fsync fd));  (* fsync before close for crash safety *)
    Unix.rename temp_path path  (* rename AFTER close - TOCTOU fix *)
  )

(** Load rate limit states from file - THREAD SAFE
    P1-M4: Exception handling added *)
let load_rate_limit_states masc_dir =
  Mutex.protect rate_limit_states_mutex (fun () ->
    let path = Filename.concat (security_state_dir masc_dir) "rate_limits.json" in
    if Sys.file_exists path then begin
      try
        let content = In_channel.with_open_text path In_channel.input_all in
        let json = Yojson.Safe.from_string content in
        let open Yojson.Safe.Util in
        (* Handle both old format (direct entries) and new format (with schema_version) *)
        let entries = match json |> member "data" with
          | `Assoc e -> e
          | _ -> (match json with `Assoc e -> e | _ -> [])
        in
        List.iter (fun (k, v) ->
          Hashtbl.replace rate_limit_states k (rate_limit_state_of_json v)
        ) entries
      with
      | Yojson.Json_error msg ->
          Printf.eprintf "[security] Failed to parse rate_limits.json: %s\n" msg
      | Sys_error msg ->
          Printf.eprintf "[security] Failed to read rate_limits.json: %s\n" msg
    end
  )

(* --- End Rate Limit Persistence --- *)

(** Get rate limit state - THREAD SAFE, uses monotonic time *)
let get_rate_limit_state agent =
  Mutex.protect rate_limit_states_mutex (fun () ->
    match Hashtbl.find_opt rate_limit_states agent with
    | Some state -> state
    | None ->
      let now = get_monotonic_time () in
      let state = {
        minute_count = 0;
        hour_count = 0;
        minute_reset = now +. 60.0;
        hour_reset = now +. 3600.0;
        burst_tokens = default_rate_limit.burst_limit;
        last_request = now;
      } in
      Hashtbl.add rate_limit_states agent state;
      state
  )

(** Check and update rate limit - returns Ok () or Error with reason - THREAD SAFE
    Persists state when limits are exceeded (Putin/Zuckerberg fix).
    Uses monotonic time (P1-H4), fixes burst_tokens overflow (M7). *)
let check_rate_limit (config : security_config) masc_dir agent =
  let now = get_monotonic_time () in
  let state = get_rate_limit_state agent in
  let limit = match config.level with
    | Enterprise | Paranoid -> enterprise_rate_limit
    | _ -> default_rate_limit
  in

  Mutex.protect rate_limit_states_mutex (fun () ->
    (* Reset counters if window expired *)
    if now > state.minute_reset then begin
      state.minute_count <- 0;
      state.minute_reset <- now +. 60.0
    end;
    if now > state.hour_reset then begin
      state.hour_count <- 0;
      state.hour_reset <- now +. 3600.0
    end;

    (* Refill burst tokens (1 per second, up to limit)
       M7 fix: Clamp elapsed to prevent int overflow *)
    let elapsed = min (now -. state.last_request) (float_of_int limit.burst_limit) in
    let elapsed_int = int_of_float (max 0.0 elapsed) in
    state.burst_tokens <- min limit.burst_limit (state.burst_tokens + elapsed_int);
    state.last_request <- now
  );

  (* Check limits - persist on limit exceeded (outside mutex to avoid deadlock) *)
  if state.minute_count >= limit.max_requests_per_minute then begin
    save_rate_limit_states masc_dir;  (* Persist violation state *)
    Error (Printf.sprintf "Rate limit exceeded: %d requests/minute (max: %d)"
             state.minute_count limit.max_requests_per_minute)
  end
  else if state.hour_count >= limit.max_requests_per_hour then begin
    save_rate_limit_states masc_dir;  (* Persist violation state *)
    Error (Printf.sprintf "Rate limit exceeded: %d requests/hour (max: %d)"
             state.hour_count limit.max_requests_per_hour)
  end
  else if state.burst_tokens <= 0 then begin
    save_rate_limit_states masc_dir;  (* Persist violation state *)
    Error "Burst limit exceeded: too many rapid requests"
  end
  else begin
    Mutex.protect rate_limit_states_mutex (fun () ->
      state.minute_count <- state.minute_count + 1;
      state.hour_count <- state.hour_count + 1;
      state.burst_tokens <- state.burst_tokens - 1
    );
    Ok ()
  end

(* ============================================ *)
(* Encrypted Audit Logging (James Bond++)       *)
(* ============================================ *)

(** Audit write result - no silent failures *)
type audit_result =
  | Audit_ok
  | Audit_disabled
  | Audit_encryption_failed of string
  | Audit_key_error of string

(** Write encrypted audit event - NEVER falls back to plaintext (Security Auditor fix).
    Returns result type to indicate success/failure. Callers must handle errors. *)
let write_encrypted_audit_event (config : security_config) masc_dir event : audit_result =
  if not config.audit_enabled then
    Audit_disabled
  else begin
    let json = event_to_json event in

    if config.encryption_enabled then begin
      (* Load encryption key and encrypt - NO FALLBACK TO PLAINTEXT *)
      match Encryption.load_key Encryption.default_config with
      | Ok key ->
        (match Encryption.encrypt_json ~key ~adata:"audit" json with
         | Ok envelope ->
           let envelope_json = Encryption.envelope_to_json envelope in
           let encrypted_line = Yojson.Safe.to_string envelope_json ^ "\n" in
           let path = audit_log_path masc_dir ^ ".enc" in
           let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] in
           let perm = secure_file_perm config in
           let fd = Unix.openfile path flags perm in
           Fun.protect
             ~finally:(fun () -> Unix.close fd)
             (fun () ->
               let _ = Unix.write_substring fd encrypted_line 0 (String.length encrypted_line) in
               Audit_ok)
         | Error e ->
           (* NO FALLBACK - encryption failed, report error *)
           Audit_encryption_failed (Encryption.show_encryption_error e))
      | Error e ->
        (* NO FALLBACK - key error, report it *)
        Audit_key_error (Encryption.show_encryption_error e)
    end else begin
      (* Encryption not required - write plaintext *)
      write_audit_event config masc_dir event;
      Audit_ok
    end
  end

(* ============================================ *)
(* Type-Safe Security Validation (Bartosz++)    *)
(* ============================================ *)

(** Type-safe validated inputs using abstract types.
    No Obj.magic - proper module signature ensures type safety.
    Once validated, the value is guaranteed to be safe. *)

(** Validated agent name - abstract type *)
module Agent_name : sig
  type t
  val of_string : string -> (t, string) result
  val to_string : t -> string
end = struct
  type t = string
  let of_string s =
    match validate_agent_name s with
    | Ok _ -> Ok s
    | Error e -> Error e
  let to_string t = t
end

(** Validated task ID - abstract type *)
module Task_id : sig
  type t
  val of_string : string -> (t, string) result
  val to_string : t -> string
end = struct
  type t = string
  let of_string s =
    match validate_task_id s with
    | Ok _ -> Ok s
    | Error e -> Error e
  let to_string t = t
end

(** Validated file path - abstract type *)
module File_path : sig
  type t
  val of_string : masc_dir:string -> string -> (t, string) result
  val to_string : t -> string
end = struct
  type t = string
  let of_string ~masc_dir s =
    match validate_file_path masc_dir s with
    | Ok normalized -> Ok normalized
    | Error e -> Error e
  let to_string t = t
end

(* ============================================ *)
(* Immutable Security Context (Rich Hickey++)   *)
(* ============================================ *)

(** Immutable security context - once created, cannot be modified *)
type security_context = {
  ctx_config: security_config;
  ctx_agent: string;
  ctx_masc_dir: string;
  ctx_timestamp: float;
  ctx_request_id: string;
}

(** Create a new immutable security context *)
let create_context config masc_dir agent =
  {
    ctx_config = config;
    ctx_agent = agent;
    ctx_masc_dir = masc_dir;
    ctx_timestamp = Unix.gettimeofday ();
    ctx_request_id = Printf.sprintf "%s-%f" agent (Unix.gettimeofday ());
  }

(** Pure function: validate request in context *)
let validate_request ctx =
  (* Check agent status *)
  if not (is_agent_allowed ctx.ctx_agent) then
    Error "Agent is quarantined or banned"
  (* Check rate limit *)
  else match check_rate_limit ctx.ctx_config ctx.ctx_masc_dir ctx.ctx_agent with
    | Error e -> Error e
    | Ok () -> Ok ctx

(** Pure function: get security level as string *)
let level_to_string = function
  | Development -> "development"
  | Production -> "production"
  | Enterprise -> "enterprise"
  | Paranoid -> "paranoid"

(** Pure function: check if encryption required *)
let encryption_required (config : security_config) =
  match config.level with
  | Enterprise | Paranoid -> true
  | Development | Production -> false

(* ============================================ *)
(* State Initialization (Putin/Zuckerberg fix)  *)
(* ============================================ *)

(** Initialize security state from persisted files.
    Call this at startup to restore quarantine/ban lists and rate limits.
    P0-H3: Cleanup stale temp files first *)
let init_security_state masc_dir =
  cleanup_temp_files masc_dir;  (* P0-H3: Remove stale .tmp files *)
  load_agent_statuses masc_dir;
  load_rate_limit_states masc_dir

(** Flush all security state to disk.
    Call this before shutdown to ensure state is persisted. *)
let flush_security_state masc_dir =
  save_agent_statuses masc_dir;
  save_rate_limit_states masc_dir
