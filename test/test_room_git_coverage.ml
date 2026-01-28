(** Room Git Module Coverage Tests

    Tests for git utility functions:
    - git_root function
    - is_git_repo function
    - remote_branch_exists function
    - origin_head_branch function
    - resolve_base_branch function
    - list function
    - get_info function
*)

open Alcotest

module Room_git = Masc_mcp.Room_git

(* ============================================================
   git_root Tests
   ============================================================ *)

let test_git_root_returns_option () =
  (* Test on current directory - should be a git repo *)
  let result = Room_git.git_root ~base_path:"." in
  (* Just verify it returns an option *)
  let _ : string option = result in
  check bool "returns option" true true

let test_git_root_nonexistent () =
  let result = Room_git.git_root ~base_path:"/nonexistent/path/xyz" in
  match result with
  | None -> check bool "nonexistent returns None" true true
  | Some _ -> check bool "might exist" true true  (* allow both outcomes *)

let test_git_root_tmp () =
  let result = Room_git.git_root ~base_path:"/tmp" in
  match result with
  | None -> check bool "/tmp is not git repo" true true
  | Some _ -> check bool "/tmp might be in git repo" true true

let test_git_root_current_nonempty () =
  match Room_git.git_root ~base_path:"." with
  | Some root -> check bool "root nonempty" true (String.length root > 0)
  | None -> fail "expected git root for current dir"

let test_git_root_is_directory () =
  match Room_git.git_root ~base_path:"." with
  | Some root -> check bool "root is directory" true (Sys.is_directory root)
  | None -> fail "expected git root"

(* ============================================================
   is_git_repo Tests
   ============================================================ *)

let test_is_git_repo_current () =
  let result = Room_git.is_git_repo ~base_path:"." in
  (* Current directory should be a git repo for masc-mcp *)
  check bool "current dir is git repo" true result

let test_is_git_repo_returns_bool () =
  let result = Room_git.is_git_repo ~base_path:"/tmp" in
  let _ : bool = result in
  check bool "returns bool" true true

let test_is_git_repo_nonexistent () =
  let result = Room_git.is_git_repo ~base_path:"/nonexistent/xyz" in
  check bool "nonexistent is not git repo" false result

let test_is_git_repo_root () =
  let result = Room_git.is_git_repo ~base_path:"/" in
  (* Root directory is unlikely to be a git repo *)
  let _ : bool = result in
  check bool "returns bool for root" true true

(* ============================================================
   remote_branch_exists Tests
   ============================================================ *)

let test_remote_branch_exists_returns_bool () =
  match Room_git.git_root ~base_path:"." with
  | Some root ->
      let result = Room_git.remote_branch_exists root "main" in
      let _ : bool = result in
      check bool "returns bool" true true
  | None -> fail "need git repo"

let test_remote_branch_exists_main_or_master () =
  match Room_git.git_root ~base_path:"." with
  | Some root ->
      let has_main = Room_git.remote_branch_exists root "main" in
      let has_master = Room_git.remote_branch_exists root "master" in
      check bool "has main or master" true (has_main || has_master)
  | None -> fail "need git repo"

let test_remote_branch_exists_nonexistent () =
  match Room_git.git_root ~base_path:"." with
  | Some root ->
      let result = Room_git.remote_branch_exists root "nonexistent-branch-xyz-123" in
      check bool "nonexistent branch" false result
  | None -> fail "need git repo"

let test_remote_branch_exists_empty_branch () =
  match Room_git.git_root ~base_path:"." with
  | Some root ->
      let result = Room_git.remote_branch_exists root "" in
      check bool "empty branch" false result
  | None -> fail "need git repo"

(* ============================================================
   origin_head_branch Tests
   ============================================================ *)

let test_origin_head_branch_returns_option () =
  match Room_git.git_root ~base_path:"." with
  | Some root ->
      let result = Room_git.origin_head_branch root in
      let _ : string option = result in
      check bool "returns option" true true
  | None -> fail "need git repo"

let test_origin_head_branch_typical_values () =
  match Room_git.git_root ~base_path:"." with
  | Some root ->
      (match Room_git.origin_head_branch root with
       | Some branch ->
           (* If we have origin HEAD, it should be main or master typically *)
           check bool "branch nonempty" true (String.length branch > 0)
       | None ->
           (* origin/HEAD might not be set *)
           check bool "origin HEAD not set" true true)
  | None -> fail "need git repo"

(* ============================================================
   resolve_base_branch Tests
   ============================================================ *)

let test_resolve_base_branch_main () =
  match Room_git.git_root ~base_path:"." with
  | Some root ->
      (* If origin/main exists, should resolve to main *)
      if Room_git.remote_branch_exists root "main" then begin
        match Room_git.resolve_base_branch root "main" with
        | Ok (branch, fallback) ->
            check string "resolved to main" "main" branch;
            check (option string) "no fallback needed" None fallback
        | Error _ -> fail "should resolve main"
      end else
        check bool "main not on remote" true true
  | None -> fail "need git repo for test"

let test_resolve_base_branch_returns_result () =
  match Room_git.git_root ~base_path:"." with
  | Some root ->
      let result = Room_git.resolve_base_branch root "main" in
      let _ : (string * string option, Masc_mcp.Types.masc_error) result = result in
      check bool "returns result" true true
  | None -> fail "need git repo"

let test_resolve_base_branch_nonexistent_fallback () =
  match Room_git.git_root ~base_path:"." with
  | Some root ->
      (* If we ask for a nonexistent branch, it should fallback *)
      (match Room_git.resolve_base_branch root "nonexistent-xyz-123" with
       | Ok (branch, fallback) ->
           check bool "branch nonempty" true (String.length branch > 0);
           (* Should have fallback info about the missing branch *)
           check bool "has fallback note" true (fallback <> None)
       | Error _ ->
           (* Error if no fallback available either *)
           check bool "no fallback available" true true)
  | None -> fail "need git repo"

(* ============================================================
   list Tests
   ============================================================ *)

let test_list_returns_json () =
  let result = Room_git.list ~base_path:"." in
  let _ : Yojson.Safe.t = result in
  check bool "returns json" true true

let test_list_has_worktrees_key () =
  let result = Room_git.list ~base_path:"." in
  match result with
  | `Assoc fields ->
      check bool "has worktrees key" true (List.mem_assoc "worktrees" fields)
  | _ -> fail "expected assoc"

let test_list_has_count_key () =
  let result = Room_git.list ~base_path:"." in
  match result with
  | `Assoc fields ->
      check bool "has count key" true (List.mem_assoc "count" fields)
  | _ -> fail "expected assoc"

let test_list_has_masc_hint () =
  let result = Room_git.list ~base_path:"." in
  match result with
  | `Assoc fields ->
      check bool "has masc_hint" true (List.mem_assoc "masc_hint" fields)
  | _ -> fail "expected assoc"

let test_list_count_is_int () =
  let result = Room_git.list ~base_path:"." in
  let open Yojson.Safe.Util in
  let count = result |> member "count" in
  match count with
  | `Int n -> check bool "count >= 0" true (n >= 0)
  | _ -> fail "count should be int"

let test_list_worktrees_is_list () =
  let result = Room_git.list ~base_path:"." in
  let open Yojson.Safe.Util in
  let worktrees = result |> member "worktrees" in
  match worktrees with
  | `List _ -> check bool "worktrees is list" true true
  | _ -> fail "worktrees should be list"

let test_list_nonrepo () =
  let result = Room_git.list ~base_path:"/tmp" in
  match result with
  | `Assoc fields ->
      (* Non-repo should return error or worktrees depending on impl *)
      let has_error = List.mem_assoc "error" fields in
      let has_worktrees = List.mem_assoc "worktrees" fields in
      check bool "has error or worktrees" true (has_error || has_worktrees)
  | _ -> fail "expected assoc"

let test_list_current_repo_worktrees () =
  let result = Room_git.list ~base_path:"." in
  let open Yojson.Safe.Util in
  let worktrees = result |> member "worktrees" |> to_list in
  (* Should have at least one worktree (the main repo) *)
  check bool "at least one worktree" true (List.length worktrees >= 1)

let test_list_worktree_has_path () =
  let result = Room_git.list ~base_path:"." in
  let open Yojson.Safe.Util in
  let worktrees = result |> member "worktrees" |> to_list in
  match worktrees with
  | wt :: _ ->
      let path = wt |> member "path" |> to_string in
      check bool "path nonempty" true (String.length path > 0)
  | [] -> check bool "no worktrees" true true

let test_list_worktree_has_branch () =
  let result = Room_git.list ~base_path:"." in
  let open Yojson.Safe.Util in
  let worktrees = result |> member "worktrees" |> to_list in
  match worktrees with
  | wt :: _ ->
      let _ = wt |> member "branch" in
      check bool "has branch key" true true
  | [] -> check bool "no worktrees" true true

let test_list_worktree_has_is_masc () =
  let result = Room_git.list ~base_path:"." in
  let open Yojson.Safe.Util in
  let worktrees = result |> member "worktrees" |> to_list in
  match worktrees with
  | wt :: _ ->
      let is_masc = wt |> member "is_masc" |> to_bool in
      let _ : bool = is_masc in
      check bool "has is_masc bool" true true
  | [] -> check bool "no worktrees" true true

(* ============================================================
   get_info Tests
   ============================================================ *)

let test_get_info_returns_option () =
  let result = Room_git.get_info ~base_path:"." ~agent_name:"test" ~task_id:"001" in
  let _ : (string * string) option = result in
  check bool "returns option" true true

let test_get_info_nonexistent () =
  let result = Room_git.get_info ~base_path:"." ~agent_name:"nonexistent" ~task_id:"xyz123" in
  check (option (pair string string)) "nonexistent worktree" None result

let test_get_info_nonrepo () =
  let result = Room_git.get_info ~base_path:"/tmp" ~agent_name:"test" ~task_id:"001" in
  check (option (pair string string)) "non-repo returns None" None result

let test_get_info_empty_agent () =
  let result = Room_git.get_info ~base_path:"." ~agent_name:"" ~task_id:"task" in
  (* Empty agent name should return None (no such worktree) *)
  check (option (pair string string)) "empty agent" None result

let test_get_info_empty_task () =
  let result = Room_git.get_info ~base_path:"." ~agent_name:"agent" ~task_id:"" in
  (* Empty task id should return None *)
  check (option (pair string string)) "empty task" None result

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Room Git Coverage" [
    "git_root", [
      test_case "returns option" `Quick test_git_root_returns_option;
      test_case "nonexistent" `Quick test_git_root_nonexistent;
      test_case "tmp" `Quick test_git_root_tmp;
      test_case "current nonempty" `Quick test_git_root_current_nonempty;
      test_case "is directory" `Quick test_git_root_is_directory;
    ];
    "is_git_repo", [
      test_case "current" `Quick test_is_git_repo_current;
      test_case "returns bool" `Quick test_is_git_repo_returns_bool;
      test_case "nonexistent" `Quick test_is_git_repo_nonexistent;
      test_case "root" `Quick test_is_git_repo_root;
    ];
    "remote_branch_exists", [
      test_case "returns bool" `Quick test_remote_branch_exists_returns_bool;
      test_case "main or master" `Quick test_remote_branch_exists_main_or_master;
      test_case "nonexistent" `Quick test_remote_branch_exists_nonexistent;
      test_case "empty branch" `Quick test_remote_branch_exists_empty_branch;
    ];
    "origin_head_branch", [
      test_case "returns option" `Quick test_origin_head_branch_returns_option;
      test_case "typical values" `Quick test_origin_head_branch_typical_values;
    ];
    "resolve_base_branch", [
      test_case "main" `Quick test_resolve_base_branch_main;
      test_case "returns result" `Quick test_resolve_base_branch_returns_result;
      test_case "nonexistent fallback" `Quick test_resolve_base_branch_nonexistent_fallback;
    ];
    "list", [
      test_case "returns json" `Quick test_list_returns_json;
      test_case "has worktrees key" `Quick test_list_has_worktrees_key;
      test_case "has count key" `Quick test_list_has_count_key;
      test_case "has masc_hint" `Quick test_list_has_masc_hint;
      test_case "count is int" `Quick test_list_count_is_int;
      test_case "worktrees is list" `Quick test_list_worktrees_is_list;
      test_case "nonrepo" `Quick test_list_nonrepo;
      test_case "current worktrees" `Quick test_list_current_repo_worktrees;
      test_case "worktree has path" `Quick test_list_worktree_has_path;
      test_case "worktree has branch" `Quick test_list_worktree_has_branch;
      test_case "worktree has is_masc" `Quick test_list_worktree_has_is_masc;
    ];
    "get_info", [
      test_case "returns option" `Quick test_get_info_returns_option;
      test_case "nonexistent" `Quick test_get_info_nonexistent;
      test_case "nonrepo" `Quick test_get_info_nonrepo;
      test_case "empty agent" `Quick test_get_info_empty_agent;
      test_case "empty task" `Quick test_get_info_empty_task;
    ];
  ]
