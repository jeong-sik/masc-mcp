(** Worktree Detection Tests *)

open Alcotest
open Masc_mcp

(* ============================================ *)
(* Git Root Detection Tests                     *)
(* ============================================ *)

let test_parse_gitdir_worktree () =
  (* Standard worktree format *)
  let line = "gitdir: /Users/dancer/me/.git/worktrees/masc-agent-meta" in
  let result = Room_utils.parse_gitdir_to_main_root line in
  check (option string) "parses worktree path" (Some "/Users/dancer/me") result

let test_parse_gitdir_invalid () =
  let line = "invalid format" in
  let result = Room_utils.parse_gitdir_to_main_root line in
  check (option string) "returns None for invalid" None result

let test_parse_gitdir_not_worktree () =
  (* Non-worktree gitdir (shouldn't happen in practice) *)
  let line = "gitdir: /some/other/path" in
  let result = Room_utils.parse_gitdir_to_main_root line in
  check (option string) "returns None for non-worktree" None result

let test_find_git_root_main_repo () =
  (* Test with actual main repo path *)
  let path = "/Users/dancer/me" in
  if Sys.file_exists path then begin
    let result = Room_utils.find_git_root path in
    check (option string) "finds main repo root" (Some "/Users/dancer/me") result
  end

let test_find_git_root_worktree () =
  (* Test with actual worktree path *)
  let path = "/Users/dancer/me/.worktrees/masc-agent-meta" in
  if Sys.file_exists path then begin
    let result = Room_utils.find_git_root path in
    (* Worktree should resolve to main repo *)
    check (option string) "worktree resolves to main repo" (Some "/Users/dancer/me") result
  end

let test_find_git_root_subdir () =
  (* Test from a subdirectory *)
  let path = "/Users/dancer/me/.worktrees/masc-agent-meta/features/masc-mcp" in
  if Sys.file_exists path then begin
    let result = Room_utils.find_git_root path in
    check (option string) "subdir resolves to main repo" (Some "/Users/dancer/me") result
  end

let test_resolve_masc_base_path () =
  (* Test the final resolve function *)
  let worktree_path = "/Users/dancer/me/.worktrees/masc-agent-meta" in
  if Sys.file_exists worktree_path then begin
    let result = Room_utils.resolve_masc_base_path worktree_path in
    check string "worktree base resolves to main repo" "/Users/dancer/me" result
  end

(* ============================================ *)
(* Test Suite                                   *)
(* ============================================ *)

let () =
  run "Worktree Detection" [
    "parse_gitdir", [
      test_case "parses worktree gitdir" `Quick test_parse_gitdir_worktree;
      test_case "handles invalid format" `Quick test_parse_gitdir_invalid;
      test_case "handles non-worktree" `Quick test_parse_gitdir_not_worktree;
    ];
    "find_git_root", [
      test_case "main repo" `Quick test_find_git_root_main_repo;
      test_case "worktree" `Quick test_find_git_root_worktree;
      test_case "subdir of worktree" `Quick test_find_git_root_subdir;
    ];
    "resolve_base_path", [
      test_case "worktree resolves to main" `Quick test_resolve_masc_base_path;
    ];
  ]
