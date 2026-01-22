(** Enterprise Security Integration Tests

    Validates enterprise-grade security requirements:
    - James Bond: File security, authentication, intelligence protection
    - Putin: Control mechanisms, adversarial resilience, agent isolation
    - Bill Gates: Maintainability, 10-year enterprise readiness
    - Elon Musk: Simplicity, necessity of each component

    @author Second Brain
    @since MASC v3.2
*)

open Masc_mcp.Security

(* ============================================ *)
(* Test Intent Classification                   *)
(* ============================================ *)

type security_intent =
  | SecurityValidation   (* Core security mechanisms *)
  | AuditCompliance      (* Audit logging requirements *)
  | AnomalyDetection     (* Behavioral anomaly detection *)
  | AgentIsolation       (* Quarantine and ban mechanisms *)
  | InputValidation      (* Path injection, input sanitization *)
  | EnterpriseReadiness  (* Overall enterprise requirements *)

let intent_to_string = function
  | SecurityValidation -> "Security Validation"
  | AuditCompliance -> "Audit Compliance"
  | AnomalyDetection -> "Anomaly Detection"
  | AgentIsolation -> "Agent Isolation"
  | InputValidation -> "Input Validation"
  | EnterpriseReadiness -> "Enterprise Readiness"

(* ============================================ *)
(* Test Utilities                               *)
(* ============================================ *)

let temp_masc_dir () =
  (* Use PID + timestamp for deterministic unique dir *)
  let dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "masc_security_test_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.))) in
  Unix.mkdir dir 0o700;
  dir

let cleanup_dir dir =
  let rec rm path =
    if Sys.is_directory path then begin
      Array.iter (fun f -> rm (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Unix.unlink path
  in
  try rm dir with _ -> ()

let with_temp_masc f =
  let dir = temp_masc_dir () in
  try
    let result = f dir in
    cleanup_dir dir;
    result
  with e ->
    cleanup_dir dir;
    raise e

(* ============================================ *)
(* Permission Tests (James Bond)                *)
(* ============================================ *)

let test_secure_dir_permissions () =
  Printf.printf "[TEST] Secure directory permissions (James Bond)...\n%!";

  (* Development mode allows 0o755 *)
  let dev_config = { default_config with level = Development } in
  assert (secure_dir_perm dev_config = 0o755);

  (* Production mode requires 0o700 *)
  let prod_config = { default_config with level = Production } in
  assert (secure_dir_perm prod_config = 0o700);

  (* Enterprise mode requires 0o700 *)
  assert (secure_dir_perm enterprise_config = 0o700);

  Printf.printf "[PASS] Directory permissions scale with security level\n%!"

let test_permission_validation () =
  Printf.printf "[TEST] Permission validation...\n%!";
  with_temp_masc (fun dir ->
    (* Create a world-readable directory *)
    let insecure_dir = Filename.concat dir "insecure" in
    Unix.mkdir insecure_dir 0o755;

    (* Validation should fail for enterprise *)
    let result = validate_dir_permissions insecure_dir 0o700 in
    assert (Result.is_error result);

    (* Fix permissions *)
    let _ = fix_dir_permissions insecure_dir 0o700 in
    let result2 = validate_dir_permissions insecure_dir 0o700 in
    assert (Result.is_ok result2);

    Printf.printf "[PASS] Permission validation detects and fixes insecure dirs\n%!"
  )

(* ============================================ *)
(* Audit Logging Tests (Enterprise Compliance)  *)
(* ============================================ *)

let test_audit_logging () =
  Printf.printf "[TEST] Audit logging (Enterprise)...\n%!";
  with_temp_masc (fun dir ->
    let config = { enterprise_config with audit_enabled = true } in

    (* Log various events *)
    write_audit_event config dir
      (AgentJoined { agent = "claude"; capabilities = ["code"; "review"]; timestamp = Unix.gettimeofday () });

    write_audit_event config dir
      (AuthSuccess { agent = "claude"; method_ = "token"; timestamp = Unix.gettimeofday () });

    write_audit_event config dir
      (TaskClaimed { agent = "claude"; task_id = "task-001"; timestamp = Unix.gettimeofday () });

    (* Verify audit log exists and has content *)
    let audit_path = audit_log_path dir in
    assert (Sys.file_exists audit_path);

    let content = In_channel.with_open_text audit_path In_channel.input_all in
    let lines = String.split_on_char '\n' content |> List.filter (fun s -> String.length s > 0) in
    assert (List.length lines >= 3);

    (* Verify each line is valid JSON *)
    List.iter (fun line ->
      let _ = Yojson.Safe.from_string line in ()
    ) lines;

    Printf.printf "[PASS] Audit logging creates valid JSONL entries\n%!"
  )

let test_audit_disabled () =
  Printf.printf "[TEST] Audit disabled in development...\n%!";
  with_temp_masc (fun dir ->
    let config = { default_config with audit_enabled = false } in

    (* Log event with audit disabled *)
    write_audit_event config dir
      (AgentJoined { agent = "test"; capabilities = []; timestamp = Unix.gettimeofday () });

    (* Audit log should not exist *)
    let audit_path = audit_log_path dir in
    assert (not (Sys.file_exists audit_path));

    Printf.printf "[PASS] Audit logging respects disabled setting\n%!"
  )

(* ============================================ *)
(* Anomaly Detection Tests (Putin)              *)
(* ============================================ *)

let test_anomaly_auth_failures () =
  Printf.printf "[TEST] Anomaly detection - auth failures (Putin)...\n%!";

  let config = enterprise_config in
  let agent = "suspicious_agent" in

  (* Simulate auth failures *)
  let behavior = get_agent_behavior agent in
  behavior.auth_failures <- config.max_failed_auth;

  let anomalies = detect_anomalies config agent in
  assert (List.exists (function AuthenticationSpike _ -> true | _ -> false) anomalies);

  Printf.printf "[PASS] Auth failure spike detected as anomaly\n%!"

let test_anomaly_low_success_rate () =
  Printf.printf "[TEST] Anomaly detection - low success rate...\n%!";

  let config = enterprise_config in
  let agent = "failing_agent" in

  let behavior = get_agent_behavior agent in
  behavior.task_count <- 10;
  behavior.success_rate <- 0.2;

  let anomalies = detect_anomalies config agent in
  assert (List.exists (function LowSuccessRate _ -> true | _ -> false) anomalies);

  Printf.printf "[PASS] Low success rate detected as anomaly\n%!"

(* ============================================ *)
(* Agent Isolation Tests (Putin)                *)
(* ============================================ *)

let test_agent_quarantine () =
  Printf.printf "[TEST] Agent quarantine (Putin)...\n%!";
  with_temp_masc (fun dir ->
    let config = { paranoid_config with audit_enabled = false } in (* Disable audit for test *)
    let agent = "compromised_agent" in

    (* Agent should be allowed initially *)
    assert (is_agent_allowed agent);

    (* Quarantine the agent *)
    quarantine_agent config dir agent "Suspicious behavior" 60.0;

    (* Agent should be blocked *)
    assert (not (is_agent_allowed agent));

    (* Check status *)
    (match get_agent_status agent with
     | Quarantined { reason; _ } ->
         assert (reason = "Suspicious behavior")
     | _ -> failwith "Expected Quarantined status");

    (* Release the agent *)
    release_agent config dir agent;
    assert (is_agent_allowed agent);

    Printf.printf "[PASS] Agent quarantine and release works correctly\n%!"
  )

let test_agent_ban () =
  Printf.printf "[TEST] Agent permanent ban (Putin)...\n%!";
  with_temp_masc (fun dir ->
    let config = { paranoid_config with audit_enabled = false } in
    let agent = "malicious_agent" in

    (* Ban the agent *)
    ban_agent config dir agent "Confirmed malicious activity";

    (* Agent should be permanently blocked *)
    assert (not (is_agent_allowed agent));

    (* Verify status *)
    (match get_agent_status agent with
     | Banned { reason } ->
         assert (String.length reason > 0)
     | _ -> failwith "Expected Banned status");

    Printf.printf "[PASS] Agent permanent ban works correctly\n%!"
  )

let test_state_persistence () =
  Printf.printf "[TEST] Security state persistence (Putin/Zuckerberg)...\n%!";
  with_temp_masc (fun dir ->
    let config = { paranoid_config with audit_enabled = false } in
    (* Use a unique agent name to avoid collisions with other tests - PID + timestamp *)
    let agent = Printf.sprintf "persistent_agent_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.)) in

    (* Quarantine an agent *)
    quarantine_agent config dir agent "Test quarantine" 3600.0;

    (* Verify state is persisted to file *)
    let status_file = Filename.concat (Filename.concat dir "security") "agent_statuses.json" in
    assert (Sys.file_exists status_file);

    (* Verify the file contains our agent *)
    let content = In_channel.with_open_text status_file In_channel.input_all in
    assert (String.length content > 0);
    (* Simple substring check using Str *)
    let contains haystack needle =
      try let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in true
      with Not_found -> false
    in
    assert (contains content agent);
    assert (contains content "quarantined");

    Printf.printf "[PASS] Security state persists to disk correctly\n%!"
  )

(* ============================================ *)
(* Input Validation Tests (Security Auditor)    *)
(* ============================================ *)

let test_agent_name_validation () =
  Printf.printf "[TEST] Agent name validation (path injection)...\n%!";

  (* Valid names *)
  assert (Result.is_ok (validate_agent_name "claude"));
  assert (Result.is_ok (validate_agent_name "gemini-pro"));
  assert (Result.is_ok (validate_agent_name "agent_123"));

  (* Invalid: path traversal *)
  assert (Result.is_error (validate_agent_name "../../../etc/passwd"));
  assert (Result.is_error (validate_agent_name "agent/subdir"));
  assert (Result.is_error (validate_agent_name "agent\\subdir"));

  (* Invalid: empty or too long *)
  assert (Result.is_error (validate_agent_name ""));
  assert (Result.is_error (validate_agent_name (String.make 100 'a')));

  (* Invalid: special characters *)
  assert (Result.is_error (validate_agent_name "agent;rm -rf"));
  assert (Result.is_error (validate_agent_name "agent$(whoami)"));

  Printf.printf "[PASS] Agent name validation blocks injection attacks\n%!"

let test_task_id_validation () =
  Printf.printf "[TEST] Task ID validation...\n%!";

  assert (Result.is_ok (validate_task_id "task-001"));
  assert (Result.is_ok (validate_task_id "PK-12345"));

  assert (Result.is_error (validate_task_id ""));
  assert (Result.is_error (validate_task_id "../task"));

  Printf.printf "[PASS] Task ID validation works correctly\n%!"

let test_file_path_validation () =
  Printf.printf "[TEST] File path validation (directory traversal)...\n%!";
  with_temp_masc (fun dir ->
    (* Valid paths within masc_dir *)
    assert (Result.is_ok (validate_file_path dir "tasks.json"));
    assert (Result.is_ok (validate_file_path dir "agents/claude.json"));

    (* Invalid: path traversal *)
    assert (Result.is_error (validate_file_path dir "../../../etc/passwd"));
    assert (Result.is_error (validate_file_path dir "..\\..\\windows\\system32"));

    (* Invalid: null byte injection *)
    assert (Result.is_error (validate_file_path dir "file.txt\x00.exe"));

    Printf.printf "[PASS] File path validation blocks traversal attacks\n%!"
  )

(* ============================================ *)
(* Enterprise Readiness Tests (Bill Gates)      *)
(* ============================================ *)

let test_security_report () =
  Printf.printf "[TEST] Security report generation (Bill Gates)...\n%!";
  with_temp_masc (fun dir ->
    let config = enterprise_config in
    let report = generate_report config dir in

    assert (report.level = "enterprise");
    assert (report.audit_enabled = true);
    assert (report.encryption_enabled = true);

    (* Report should be convertible to JSON *)
    let json = report_to_json report in
    let _ = Yojson.Safe.to_string json in

    Printf.printf "[PASS] Security report provides enterprise visibility\n%!"
  )

let test_security_level_progression () =
  Printf.printf "[TEST] Security level progression...\n%!";

  (* Development: permissive *)
  let dev = default_config in
  assert (dev.level = Development);
  assert (not dev.audit_enabled);

  (* Production: audit + permissions *)
  let prod = { default_config with level = Production; audit_enabled = true } in
  assert (prod.audit_enabled);

  (* Enterprise: full security *)
  assert (enterprise_config.audit_enabled);
  assert (enterprise_config.encryption_enabled);
  assert (enterprise_config.anomaly_detection);

  (* Paranoid: maximum security *)
  assert (paranoid_config.agent_isolation);
  assert (paranoid_config.max_failed_auth < enterprise_config.max_failed_auth);

  Printf.printf "[PASS] Security levels provide progressive protection\n%!"

(* ============================================ *)
(* Test Registry                                *)
(* ============================================ *)

type test_entry = {
  name: string;
  intent: security_intent;
  run: unit -> unit;
}

let all_tests = [
  (* Security Validation *)
  { name = "secure_dir_permissions"; intent = SecurityValidation; run = test_secure_dir_permissions };
  { name = "permission_validation"; intent = SecurityValidation; run = test_permission_validation };

  (* Audit Compliance *)
  { name = "audit_logging"; intent = AuditCompliance; run = test_audit_logging };
  { name = "audit_disabled"; intent = AuditCompliance; run = test_audit_disabled };

  (* Anomaly Detection *)
  { name = "anomaly_auth_failures"; intent = AnomalyDetection; run = test_anomaly_auth_failures };
  { name = "anomaly_low_success_rate"; intent = AnomalyDetection; run = test_anomaly_low_success_rate };

  (* Agent Isolation *)
  { name = "agent_quarantine"; intent = AgentIsolation; run = test_agent_quarantine };
  { name = "agent_ban"; intent = AgentIsolation; run = test_agent_ban };
  { name = "state_persistence"; intent = AgentIsolation; run = test_state_persistence };

  (* Input Validation *)
  { name = "agent_name_validation"; intent = InputValidation; run = test_agent_name_validation };
  { name = "task_id_validation"; intent = InputValidation; run = test_task_id_validation };
  { name = "file_path_validation"; intent = InputValidation; run = test_file_path_validation };

  (* Enterprise Readiness *)
  { name = "security_report"; intent = EnterpriseReadiness; run = test_security_report };
  { name = "security_level_progression"; intent = EnterpriseReadiness; run = test_security_level_progression };
]

(* ============================================ *)
(* Main Runner                                  *)
(* ============================================ *)

let () =
  Random.self_init ();

  Printf.printf "\n";
  Printf.printf "╔═══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║       MASC Enterprise Security Test Suite                     ║\n";
  Printf.printf "║                                                               ║\n";
  Printf.printf "║  Addressing feedback from:                                    ║\n";
  Printf.printf "║  - James Bond (007): File security, authentication           ║\n";
  Printf.printf "║  - Putin: Control mechanisms, adversarial resilience         ║\n";
  Printf.printf "║  - Bill Gates: Enterprise readiness, maintainability         ║\n";
  Printf.printf "║  - Security Auditor: Path injection, input validation        ║\n";
  Printf.printf "╚═══════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";

  let passed = ref 0 in
  let failed = ref 0 in

  (* Group tests by intent *)
  let intents = [SecurityValidation; AuditCompliance; AnomalyDetection; AgentIsolation; InputValidation; EnterpriseReadiness] in

  List.iter (fun intent ->
    Printf.printf "\n=== %s ===\n\n" (intent_to_string intent);
    let tests_for_intent = List.filter (fun t -> t.intent = intent) all_tests in
    List.iter (fun test ->
      try
        test.run ();
        incr passed
      with e ->
        Printf.printf "[FAIL] %s: %s\n" test.name (Printexc.to_string e);
        incr failed
    ) tests_for_intent
  ) intents;

  Printf.printf "\n";
  Printf.printf "════════════════════════════════════════════════════════════════\n";
  Printf.printf "Results: %d passed, %d failed (total: %d)\n" !passed !failed (!passed + !failed);
  Printf.printf "════════════════════════════════════════════════════════════════\n";

  if !failed > 0 then exit 1
