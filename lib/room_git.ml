(** MASC Git Worktree Operations

    Provides Git worktree isolation for multi-agent parallel work.
    Separated from room.ml for better modularity.
*)

open Types

(* ============================================ *)
(* Git Repository Utilities                     *)
(* ============================================ *)

(** Get git root directory *)
let git_root ~base_path =
  let cmd = Printf.sprintf "cd %s && git rev-parse --show-toplevel 2>/dev/null" base_path in
  let ic = Unix.open_process_in cmd in
  let result = try Some (input_line ic) with End_of_file -> None in
  let _ = Unix.close_process_in ic in
  result

(** Check if directory is a git repository *)
let is_git_repo ~base_path =
  match git_root ~base_path with
  | Some _ -> true
  | None -> false

let remote_branch_exists root branch =
  let cmd = Printf.sprintf
    "cd %s && git show-ref --verify --quiet refs/remotes/origin/%s" root branch
  in
  Sys.command cmd = 0

let origin_head_branch root =
  let cmd = Printf.sprintf
    "cd %s && git symbolic-ref -q refs/remotes/origin/HEAD 2>/dev/null" root
  in
  let ic = Unix.open_process_in cmd in
  let line = try Some (input_line ic) with End_of_file -> None in
  let _ = Unix.close_process_in ic in
  match line with
  | None -> None
  | Some refname ->
      (match List.rev (String.split_on_char '/' refname) with
      | branch :: _ -> Some branch
      | [] -> None)

let resolve_base_branch root base_branch =
  if remote_branch_exists root base_branch then
    Ok (base_branch, None)
  else
    let candidates =
      match origin_head_branch root with
      | Some head -> [head; "main"; "master"]
      | None -> ["main"; "master"]
    in
    match List.find_opt (remote_branch_exists root) candidates with
    | Some fallback -> Ok (fallback, Some base_branch)
    | None -> Error (IoError (Printf.sprintf
        "Base branch origin/%s not found and no fallback branch detected." base_branch))

(* ============================================ *)
(* Worktree Operations                          *)
(* ============================================ *)

(** Create worktree for agent

    @param base_path Project root path
    @param agent_name Agent creating the worktree (e.g., "claude")
    @param task_id Task or feature ID (e.g., "PK-12345")
    @param base_branch Branch to base off (e.g., "develop", "main")
    @return Result with success message or error
*)
let create ~base_path ~agent_name ~task_id ~base_branch : string masc_result =
  if not (is_git_repo ~base_path) then
    Error (IoError "Not a git repository. MASC v2 requires .git directory for worktree isolation.")
  else begin
    match git_root ~base_path with
    | None -> Error (IoError "Cannot determine git root")
    | Some root ->
        let worktree_name = Printf.sprintf "%s-%s" agent_name task_id in
        let worktree_path = Filename.concat root (Filename.concat ".worktrees" worktree_name) in
        let branch_name = Printf.sprintf "%s/%s" agent_name task_id in

        (* Create .worktrees directory if not exists *)
        let worktrees_dir = Filename.concat root ".worktrees" in
        if not (Sys.file_exists worktrees_dir) then
          Unix.mkdir worktrees_dir 0o755;

        (* Check if worktree already exists *)
        if Sys.file_exists worktree_path then
          Ok (Printf.sprintf "✅ Worktree already exists:\n  Path: %s\n  Branch: %s\n\nNext: cd %s"
              worktree_path branch_name worktree_path)
        else begin
          (* Fetch origin first *)
          let fetch_cmd = Printf.sprintf "cd %s && git fetch origin 2>&1" root in
          let _ = Sys.command fetch_cmd in
          match resolve_base_branch root base_branch with
          | Error e -> Error e
          | Ok (resolved_base, fallback_from) ->
              let note = match fallback_from with
                | None -> ""
                | Some missing ->
                    Printf.sprintf "\n  Note: origin/%s not found; used origin/%s" missing resolved_base
              in
              (* Create worktree with new branch from base *)
              let cmd = Printf.sprintf
                "cd %s && git worktree add %s -b %s origin/%s 2>&1"
                root worktree_path branch_name resolved_base in
              let exit_code = Sys.command cmd in

              if exit_code = 0 then begin
                Ok (Printf.sprintf "✅ Worktree created:\n  Path: %s\n  Branch: %s%s\n\nNext: cd %s && work && gh pr create --draft"
                    worktree_path branch_name note worktree_path)
              end
              else
                Error (IoError (Printf.sprintf "Failed to create worktree from origin/%s." resolved_base))
        end
  end

(** Remove worktree after work is merged

    @param base_path Project root path
    @param agent_name Agent that owns the worktree
    @param task_id Task ID used when creating
    @return Result with success message or error
*)
let remove ~base_path ~agent_name ~task_id : string masc_result =
  match git_root ~base_path with
  | None -> Error (IoError "Cannot determine git root")
  | Some root ->
      let worktree_name = Printf.sprintf "%s-%s" agent_name task_id in
      let worktree_path = Filename.concat root (Filename.concat ".worktrees" worktree_name) in
      let branch_name = Printf.sprintf "%s/%s" agent_name task_id in

      if not (Sys.file_exists worktree_path) then
        Error (IoError (Printf.sprintf "Worktree not found: %s" worktree_path))
      else begin
        (* Remove worktree *)
        let remove_cmd = Printf.sprintf "cd %s && git worktree remove %s 2>&1" root worktree_path in
        let exit_code = Sys.command remove_cmd in

        if exit_code = 0 then begin
          (* Try to delete the branch (may fail if not merged, which is ok) *)
          let branch_cmd = Printf.sprintf "cd %s && git branch -d %s 2>&1" root branch_name in
          let _ = Sys.command branch_cmd in

          (* Prune stale worktrees *)
          let prune_cmd = Printf.sprintf "cd %s && git worktree prune 2>&1" root in
          let _ = Sys.command prune_cmd in

          Ok (Printf.sprintf "✅ Worktree removed: %s\n   Branch: %s" worktree_path branch_name)
        end
        else
          Error (IoError "Failed to remove worktree. It may have uncommitted changes.")
      end

(** List all worktrees in the repository *)
let list ~base_path =
  match git_root ~base_path with
  | None -> `Assoc [("error", `String "Not a git repository")]
  | Some root ->
      let cmd = Printf.sprintf "cd %s && git worktree list --porcelain 2>/dev/null" root in
      let ic = Unix.open_process_in cmd in
      let rec read_lines acc =
        try
          let line = input_line ic in
          read_lines (line :: acc)
        with End_of_file -> List.rev acc
      in
      let lines = read_lines [] in
      let _ = Unix.close_process_in ic in

      (* Parse porcelain output into worktree info *)
      let rec parse_worktrees lines current acc =
        match lines with
        | [] ->
            if current <> [] then List.rev (List.rev current :: acc)
            else List.rev acc
        | "" :: rest ->
            if current <> [] then parse_worktrees rest [] (List.rev current :: acc)
            else parse_worktrees rest [] acc
        | line :: rest ->
            parse_worktrees rest (line :: current) acc
      in
      let worktree_blocks = parse_worktrees lines [] [] in

      let parse_block block =
        let path = ref "" in
        let branch = ref "" in
        List.iter (fun line ->
          if String.length line > 9 && String.sub line 0 9 = "worktree " then
            path := String.sub line 9 (String.length line - 9)
          else if String.length line > 7 && String.sub line 0 7 = "branch " then
            branch := String.sub line 7 (String.length line - 7)
        ) block;
        if !path <> "" then
          Some (`Assoc [
            ("path", `String !path);
            ("branch", `String !branch);
            (* Check if path contains .worktrees - stdlib compatible *)
            ("is_masc", `Bool (try
              let _ = Str.search_forward (Str.regexp_string ".worktrees") !path 0 in true
            with Not_found -> false));
          ])
        else None
      in

      let worktrees = List.filter_map parse_block worktree_blocks in
      `Assoc [
        ("worktrees", `List worktrees);
        ("count", `Int (List.length worktrees));
        ("masc_hint", `String "Use masc_worktree_create to add a new worktree for your task");
      ]

(** Get worktree info for a specific agent/task *)
let get_info ~base_path ~agent_name ~task_id =
  match git_root ~base_path with
  | None -> None
  | Some root ->
      let worktree_name = Printf.sprintf "%s-%s" agent_name task_id in
      let worktree_path = Filename.concat root (Filename.concat ".worktrees" worktree_name) in
      let branch_name = Printf.sprintf "%s/%s" agent_name task_id in
      if Sys.file_exists worktree_path then
        Some (worktree_path, branch_name)
      else
        None
