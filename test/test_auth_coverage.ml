(** Auth Module Coverage Tests

    Tests for MASC Authentication & Authorization:
    - generate_token: random token generation
    - sha256_hash: cryptographic hashing
    - auth_dir, agents_dir, room_secret_file, auth_config_file: path helpers
*)

open Alcotest

module Auth = Masc_mcp.Auth
module Types = Masc_mcp.Types

(* ============================================================
   generate_token Tests
   ============================================================ *)

let test_generate_token_nonempty () =
  let token = Auth.generate_token () in
  check bool "nonempty" true (String.length token > 0)

let test_generate_token_length () =
  let token = Auth.generate_token () in
  (* 32 bytes -> 64 hex chars *)
  check int "64 chars" 64 (String.length token)

let test_generate_token_hex () =
  let token = Auth.generate_token () in
  let is_hex = String.for_all (fun c ->
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
  ) token in
  check bool "all hex" true is_hex

let test_generate_token_unique () =
  let t1 = Auth.generate_token () in
  let t2 = Auth.generate_token () in
  check bool "unique" true (t1 <> t2)

(* ============================================================
   sha256_hash Tests
   ============================================================ *)

let test_sha256_hash_nonempty () =
  let hash = Auth.sha256_hash "test" in
  check bool "nonempty" true (String.length hash > 0)

let test_sha256_hash_length () =
  let hash = Auth.sha256_hash "test" in
  (* SHA256 produces 64 hex chars *)
  check int "64 chars" 64 (String.length hash)

let test_sha256_hash_hex () =
  let hash = Auth.sha256_hash "test" in
  let is_hex = String.for_all (fun c ->
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
  ) hash in
  check bool "all hex" true is_hex

let test_sha256_hash_deterministic () =
  let h1 = Auth.sha256_hash "hello" in
  let h2 = Auth.sha256_hash "hello" in
  check string "same hash" h1 h2

let test_sha256_hash_different_inputs () =
  let h1 = Auth.sha256_hash "hello" in
  let h2 = Auth.sha256_hash "world" in
  check bool "different hashes" true (h1 <> h2)

let test_sha256_hash_empty () =
  let hash = Auth.sha256_hash "" in
  check int "64 chars for empty" 64 (String.length hash)

(* ============================================================
   Path Helper Tests
   ============================================================ *)

let test_auth_dir_nonempty () =
  let path = Auth.auth_dir "/tmp/test" in
  check bool "nonempty" true (String.length path > 0)

let test_auth_dir_contains_masc () =
  let path = Auth.auth_dir "/tmp/test" in
  check bool "contains .masc" true
    (try
       let _ = Str.search_forward (Str.regexp "\\.masc") path 0 in
       true
     with Not_found -> false)

let test_auth_dir_contains_auth () =
  let path = Auth.auth_dir "/tmp/test" in
  check bool "contains auth" true
    (try
       let _ = Str.search_forward (Str.regexp "auth") path 0 in
       true
     with Not_found -> false)

let test_agents_dir_nonempty () =
  let path = Auth.agents_dir "/tmp/test" in
  check bool "nonempty" true (String.length path > 0)

let test_agents_dir_contains_agents () =
  let path = Auth.agents_dir "/tmp/test" in
  check bool "contains agents" true
    (try
       let _ = Str.search_forward (Str.regexp "agents") path 0 in
       true
     with Not_found -> false)

let test_room_secret_file_nonempty () =
  let path = Auth.room_secret_file "/tmp/test" in
  check bool "nonempty" true (String.length path > 0)

let test_room_secret_file_contains_hash () =
  let path = Auth.room_secret_file "/tmp/test" in
  check bool "contains .hash" true
    (try
       let _ = Str.search_forward (Str.regexp "\\.hash") path 0 in
       true
     with Not_found -> false)

let test_auth_config_file_nonempty () =
  let path = Auth.auth_config_file "/tmp/test" in
  check bool "nonempty" true (String.length path > 0)

let test_auth_config_file_json () =
  let path = Auth.auth_config_file "/tmp/test" in
  check bool "ends with .json" true
    (String.length path >= 5 &&
     String.sub path (String.length path - 5) 5 = ".json")

(* ============================================================
   permission_for_tool Tests
   ============================================================ *)

let test_permission_for_tool_init () =
  match Auth.permission_for_tool "masc_init" with
  | Some Types.CanInit -> ()
  | _ -> fail "expected CanInit"

let test_permission_for_tool_reset () =
  match Auth.permission_for_tool "masc_reset" with
  | Some Types.CanReset -> ()
  | _ -> fail "expected CanReset"

let test_permission_for_tool_join () =
  match Auth.permission_for_tool "masc_join" with
  | Some Types.CanJoin -> ()
  | _ -> fail "expected CanJoin"

let test_permission_for_tool_leave () =
  match Auth.permission_for_tool "masc_leave" with
  | Some Types.CanLeave -> ()
  | _ -> fail "expected CanLeave"

let test_permission_for_tool_status () =
  match Auth.permission_for_tool "masc_status" with
  | Some Types.CanReadState -> ()
  | _ -> fail "expected CanReadState"

let test_permission_for_tool_who () =
  match Auth.permission_for_tool "masc_who" with
  | Some Types.CanReadState -> ()
  | _ -> fail "expected CanReadState"

let test_permission_for_tool_tasks () =
  match Auth.permission_for_tool "masc_tasks" with
  | Some Types.CanReadState -> ()
  | _ -> fail "expected CanReadState"

let test_permission_for_tool_add_task () =
  match Auth.permission_for_tool "masc_add_task" with
  | Some Types.CanAddTask -> ()
  | _ -> fail "expected CanAddTask"

let test_permission_for_tool_claim () =
  match Auth.permission_for_tool "masc_claim" with
  | Some Types.CanClaimTask -> ()
  | _ -> fail "expected CanClaimTask"

let test_permission_for_tool_claim_next () =
  match Auth.permission_for_tool "masc_claim_next" with
  | Some Types.CanClaimTask -> ()
  | _ -> fail "expected CanClaimTask"

let test_permission_for_tool_done () =
  match Auth.permission_for_tool "masc_done" with
  | Some Types.CanCompleteTask -> ()
  | _ -> fail "expected CanCompleteTask"

let test_permission_for_tool_broadcast () =
  match Auth.permission_for_tool "masc_broadcast" with
  | Some Types.CanBroadcast -> ()
  | _ -> fail "expected CanBroadcast"

let test_permission_for_tool_lock () =
  match Auth.permission_for_tool "masc_lock" with
  | Some Types.CanLockFile -> ()
  | _ -> fail "expected CanLockFile"

let test_permission_for_tool_unlock () =
  match Auth.permission_for_tool "masc_unlock" with
  | Some Types.CanUnlockFile -> ()
  | _ -> fail "expected CanUnlockFile"

let test_permission_for_tool_portal_open () =
  match Auth.permission_for_tool "masc_portal_open" with
  | Some Types.CanOpenPortal -> ()
  | _ -> fail "expected CanOpenPortal"

let test_permission_for_tool_portal_send () =
  match Auth.permission_for_tool "masc_portal_send" with
  | Some Types.CanSendPortal -> ()
  | _ -> fail "expected CanSendPortal"

let test_permission_for_tool_worktree_create () =
  match Auth.permission_for_tool "masc_worktree_create" with
  | Some Types.CanCreateWorktree -> ()
  | _ -> fail "expected CanCreateWorktree"

let test_permission_for_tool_worktree_remove () =
  match Auth.permission_for_tool "masc_worktree_remove" with
  | Some Types.CanRemoveWorktree -> ()
  | _ -> fail "expected CanRemoveWorktree"

let test_permission_for_tool_vote_create () =
  match Auth.permission_for_tool "masc_vote_create" with
  | Some Types.CanVote -> ()
  | _ -> fail "expected CanVote"

let test_permission_for_tool_interrupt () =
  match Auth.permission_for_tool "masc_interrupt" with
  | Some Types.CanInterrupt -> ()
  | _ -> fail "expected CanInterrupt"

let test_permission_for_tool_approve () =
  match Auth.permission_for_tool "masc_approve" with
  | Some Types.CanApprove -> ()
  | _ -> fail "expected CanApprove"

let test_permission_for_tool_auth_enable () =
  match Auth.permission_for_tool "masc_auth_enable" with
  | Some Types.CanInit -> ()  (* Admin only *)
  | _ -> fail "expected CanInit"

let test_permission_for_tool_auth_status () =
  match Auth.permission_for_tool "masc_auth_status" with
  | Some Types.CanReadState -> ()
  | _ -> fail "expected CanReadState"

let test_permission_for_tool_unknown () =
  match Auth.permission_for_tool "unknown_tool_xyz" with
  | None -> ()
  | Some _ -> fail "expected None for unknown tool"

let test_permission_for_tool_empty () =
  match Auth.permission_for_tool "" with
  | None -> ()
  | Some _ -> fail "expected None for empty string"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Auth Coverage" [
    "generate_token", [
      test_case "nonempty" `Quick test_generate_token_nonempty;
      test_case "length" `Quick test_generate_token_length;
      test_case "hex" `Quick test_generate_token_hex;
      test_case "unique" `Quick test_generate_token_unique;
    ];
    "sha256_hash", [
      test_case "nonempty" `Quick test_sha256_hash_nonempty;
      test_case "length" `Quick test_sha256_hash_length;
      test_case "hex" `Quick test_sha256_hash_hex;
      test_case "deterministic" `Quick test_sha256_hash_deterministic;
      test_case "different inputs" `Quick test_sha256_hash_different_inputs;
      test_case "empty input" `Quick test_sha256_hash_empty;
    ];
    "auth_dir", [
      test_case "nonempty" `Quick test_auth_dir_nonempty;
      test_case "contains masc" `Quick test_auth_dir_contains_masc;
      test_case "contains auth" `Quick test_auth_dir_contains_auth;
    ];
    "agents_dir", [
      test_case "nonempty" `Quick test_agents_dir_nonempty;
      test_case "contains agents" `Quick test_agents_dir_contains_agents;
    ];
    "room_secret_file", [
      test_case "nonempty" `Quick test_room_secret_file_nonempty;
      test_case "contains hash" `Quick test_room_secret_file_contains_hash;
    ];
    "auth_config_file", [
      test_case "nonempty" `Quick test_auth_config_file_nonempty;
      test_case "json extension" `Quick test_auth_config_file_json;
    ];
    "permission_for_tool", [
      test_case "init" `Quick test_permission_for_tool_init;
      test_case "reset" `Quick test_permission_for_tool_reset;
      test_case "join" `Quick test_permission_for_tool_join;
      test_case "leave" `Quick test_permission_for_tool_leave;
      test_case "status" `Quick test_permission_for_tool_status;
      test_case "who" `Quick test_permission_for_tool_who;
      test_case "tasks" `Quick test_permission_for_tool_tasks;
      test_case "add_task" `Quick test_permission_for_tool_add_task;
      test_case "claim" `Quick test_permission_for_tool_claim;
      test_case "claim_next" `Quick test_permission_for_tool_claim_next;
      test_case "done" `Quick test_permission_for_tool_done;
      test_case "broadcast" `Quick test_permission_for_tool_broadcast;
      test_case "lock" `Quick test_permission_for_tool_lock;
      test_case "unlock" `Quick test_permission_for_tool_unlock;
      test_case "portal_open" `Quick test_permission_for_tool_portal_open;
      test_case "portal_send" `Quick test_permission_for_tool_portal_send;
      test_case "worktree_create" `Quick test_permission_for_tool_worktree_create;
      test_case "worktree_remove" `Quick test_permission_for_tool_worktree_remove;
      test_case "vote_create" `Quick test_permission_for_tool_vote_create;
      test_case "interrupt" `Quick test_permission_for_tool_interrupt;
      test_case "approve" `Quick test_permission_for_tool_approve;
      test_case "auth_enable" `Quick test_permission_for_tool_auth_enable;
      test_case "auth_status" `Quick test_permission_for_tool_auth_status;
      test_case "unknown" `Quick test_permission_for_tool_unknown;
      test_case "empty" `Quick test_permission_for_tool_empty;
    ];
  ]
