(** Auth Module Tests *)

open Alcotest
module Auth = Masc_mcp.Auth
module Types = Masc_mcp.Types

(* Setup a temp directory for testing *)
let setup_test_room () =
  (* Use PID + timestamp for deterministic unique dir *)
  let unique_id = Printf.sprintf "masc-auth-test-%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.)) in
  let tmp = Filename.concat (Filename.get_temp_dir_name ()) unique_id in
  Unix.mkdir tmp 0o755;
  let masc_dir = Filename.concat tmp ".masc" in
  Unix.mkdir masc_dir 0o755;
  tmp

let cleanup_test_room dir =
  (* Simple recursive delete *)
  let rec rm_rf path =
    if Sys.is_directory path then begin
      Array.iter (fun f -> rm_rf (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Sys.remove path
  in
  try rm_rf dir with _ -> ()

(* ============================================ *)
(* Token generation tests                       *)
(* ============================================ *)

let test_token_generation () =
  let token = Auth.generate_token () in
  check int "token length is 64 hex chars" 64 (String.length token);
  check bool "token is hex" true
    (String.for_all (fun c ->
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
    ) token)

let test_sha256_hash () =
  let hash1 = Auth.sha256_hash "hello" in
  let hash2 = Auth.sha256_hash "hello" in
  let hash3 = Auth.sha256_hash "world" in
  check string "same input same hash" hash1 hash2;
  check bool "different input different hash" true (hash1 <> hash3);
  check int "hash length is 64 chars" 64 (String.length hash1)

(* ============================================ *)
(* Auth config tests                            *)
(* ============================================ *)

let test_default_auth_config () =
  let cfg = Types.default_auth_config in
  check bool "auth disabled by default" false cfg.enabled;
  check bool "token not required by default" false cfg.require_token;
  check int "24hr expiry by default" 24 cfg.token_expiry_hours

let test_save_load_auth_config () =
  let dir = setup_test_room () in
  let cfg = { Types.default_auth_config with enabled = true; require_token = true } in
  Auth.save_auth_config dir cfg;
  let loaded = Auth.load_auth_config dir in
  check bool "enabled persisted" true loaded.enabled;
  check bool "require_token persisted" true loaded.require_token;
  cleanup_test_room dir

(* ============================================ *)
(* Credential management tests                  *)
(* ============================================ *)

let test_create_credential () =
  let dir = setup_test_room () in
  let result = Auth.create_token dir ~agent_name:"claude" ~role:Types.Worker in
  cleanup_test_room dir;
  match result with
  | Ok (raw_token, cred) ->
      check string "agent_name matches" "claude" cred.agent_name;
      check bool "role is Worker" true (cred.role = Types.Worker);
      check int "raw token is 64 chars" 64 (String.length raw_token);
      check int "stored token is 64 chars" 64 (String.length cred.token)
  | Error _ ->
      fail "create_token should succeed"

let test_verify_token () =
  let dir = setup_test_room () in
  let create_result = Auth.create_token dir ~agent_name:"claude" ~role:Types.Admin in
  let verify_result = match create_result with
    | Ok (raw_token, _) -> Auth.verify_token dir ~agent_name:"claude" ~token:raw_token
    | Error e -> Error e
  in
  cleanup_test_room dir;
  match create_result, verify_result with
  | Ok _, Ok cred ->
      check string "verified agent matches" "claude" cred.agent_name;
      check bool "verified role matches" true (cred.role = Types.Admin)
  | Ok _, Error e ->
      fail (Printf.sprintf "verify_token should succeed: %s" (Types.masc_error_to_string e))
  | Error _, _ ->
      fail "create_token should succeed"

let test_verify_wrong_token () =
  let dir = setup_test_room () in
  let create_result = Auth.create_token dir ~agent_name:"claude" ~role:Types.Worker in
  let verify_result = match create_result with
    | Ok _ -> Auth.verify_token dir ~agent_name:"claude" ~token:"wrongtoken"
    | Error e -> Error e
  in
  cleanup_test_room dir;
  match create_result, verify_result with
  | Ok _, Ok _ ->
      fail "verify_token should fail with wrong token"
  | Ok _, Error (Types.InvalidToken _) ->
      (* Expected *)
      ()
  | Ok _, Error e ->
      fail (Printf.sprintf "wrong error type: %s" (Types.masc_error_to_string e))
  | Error _, _ ->
      fail "create_token should succeed"

let test_list_credentials () =
  let dir = setup_test_room () in
  let _ = Auth.create_token dir ~agent_name:"claude" ~role:Types.Admin in
  let _ = Auth.create_token dir ~agent_name:"gemini" ~role:Types.Worker in
  let _ = Auth.create_token dir ~agent_name:"codex" ~role:Types.Reader in
  let creds = Auth.list_credentials dir in
  cleanup_test_room dir;
  check int "3 credentials" 3 (List.length creds)

let test_delete_credential () =
  let dir = setup_test_room () in
  let _ = Auth.create_token dir ~agent_name:"claude" ~role:Types.Worker in
  Auth.delete_credential dir "claude";
  let creds = Auth.list_credentials dir in
  cleanup_test_room dir;
  check int "0 credentials after delete" 0 (List.length creds)

(* ============================================ *)
(* Permission tests                             *)
(* ============================================ *)

let test_reader_permissions () =
  check bool "reader can read" true
    (Types.has_permission Types.Reader Types.CanReadState);
  check bool "reader cannot claim" false
    (Types.has_permission Types.Reader Types.CanClaimTask);
  check bool "reader cannot init" false
    (Types.has_permission Types.Reader Types.CanInit)

let test_worker_permissions () =
  check bool "worker can read" true
    (Types.has_permission Types.Worker Types.CanReadState);
  check bool "worker can claim" true
    (Types.has_permission Types.Worker Types.CanClaimTask);
  check bool "worker can lock" true
    (Types.has_permission Types.Worker Types.CanLockFile);
  check bool "worker cannot init" false
    (Types.has_permission Types.Worker Types.CanInit)

let test_admin_permissions () =
  check bool "admin can read" true
    (Types.has_permission Types.Admin Types.CanReadState);
  check bool "admin can init" true
    (Types.has_permission Types.Admin Types.CanInit);
  check bool "admin can reset" true
    (Types.has_permission Types.Admin Types.CanReset);
  check bool "admin can approve" true
    (Types.has_permission Types.Admin Types.CanApprove)

(* ============================================ *)
(* Authorization tests                          *)
(* ============================================ *)

let test_auth_disabled_allows_all () =
  let dir = setup_test_room () in
  (* Auth disabled by default *)
  let result = Auth.check_permission dir ~agent_name:"anyone" ~token:None ~permission:Types.CanInit in
  cleanup_test_room dir;
  match result with
  | Ok () -> ()
  | Error _ -> fail "should allow when auth disabled"

let test_auth_enabled_requires_token () =
  let dir = setup_test_room () in
  let _ = Auth.enable_auth dir ~require_token:true in
  let result = Auth.check_permission dir ~agent_name:"claude" ~token:None ~permission:Types.CanClaimTask in
  cleanup_test_room dir;
  match result with
  | Ok () -> fail "should require token"
  | Error (Types.Unauthorized _) -> ()
  | Error _ -> fail "wrong error type"

let test_auth_enabled_with_valid_token () =
  let dir = setup_test_room () in
  let _ = Auth.enable_auth dir ~require_token:true in
  let create_result = Auth.create_token dir ~agent_name:"claude" ~role:Types.Worker in
  let check_result = match create_result with
    | Ok (raw_token, _) ->
        Auth.check_permission dir ~agent_name:"claude" ~token:(Some raw_token) ~permission:Types.CanClaimTask
    | Error e -> Error e
  in
  cleanup_test_room dir;
  match check_result with
  | Ok () -> ()
  | Error e -> fail (Types.masc_error_to_string e)

let test_permission_denied_for_reader () =
  let dir = setup_test_room () in
  let _ = Auth.enable_auth dir ~require_token:true in
  let create_result = Auth.create_token dir ~agent_name:"reader_agent" ~role:Types.Reader in
  let check_result = match create_result with
    | Ok (raw_token, _) ->
        Auth.check_permission dir ~agent_name:"reader_agent" ~token:(Some raw_token) ~permission:Types.CanClaimTask
    | Error e -> Error e
  in
  cleanup_test_room dir;
  match check_result with
  | Ok () -> fail "reader should not claim"
  | Error (Types.Forbidden _) -> ()
  | Error e -> fail (Printf.sprintf "wrong error: %s" (Types.masc_error_to_string e))

(* ============================================ *)
(* Enable/disable tests                         *)
(* ============================================ *)

let test_enable_disable_auth () =
  let dir = setup_test_room () in
  let initially_disabled = not (Auth.is_auth_enabled dir) in
  let _ = Auth.enable_auth dir ~require_token:false in
  let enabled = Auth.is_auth_enabled dir in
  Auth.disable_auth dir;
  let disabled_again = not (Auth.is_auth_enabled dir) in
  cleanup_test_room dir;
  check bool "auth disabled initially" true initially_disabled;
  check bool "auth enabled after enable" true enabled;
  check bool "auth disabled after disable" true disabled_again

(* ============================================ *)
(* Test suite                                   *)
(* ============================================ *)

let () =
  Random.init 42;
  run "Auth" [
    "token_generation", [
      test_case "generate token" `Quick test_token_generation;
      test_case "sha256 hash" `Quick test_sha256_hash;
    ];
    "config", [
      test_case "default config" `Quick test_default_auth_config;
      test_case "save/load config" `Quick test_save_load_auth_config;
    ];
    "credentials", [
      test_case "create credential" `Quick test_create_credential;
      test_case "verify token" `Quick test_verify_token;
      test_case "verify wrong token" `Quick test_verify_wrong_token;
      test_case "list credentials" `Quick test_list_credentials;
      test_case "delete credential" `Quick test_delete_credential;
    ];
    "permissions", [
      test_case "reader permissions" `Quick test_reader_permissions;
      test_case "worker permissions" `Quick test_worker_permissions;
      test_case "admin permissions" `Quick test_admin_permissions;
    ];
    "authorization", [
      test_case "auth disabled allows all" `Quick test_auth_disabled_allows_all;
      test_case "auth enabled requires token" `Quick test_auth_enabled_requires_token;
      test_case "auth enabled with valid token" `Quick test_auth_enabled_with_valid_token;
      test_case "permission denied for reader" `Quick test_permission_denied_for_reader;
    ];
    "enable_disable", [
      test_case "enable/disable auth" `Quick test_enable_disable_auth;
    ];
  ]
