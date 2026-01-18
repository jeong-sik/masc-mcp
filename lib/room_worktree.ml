(** Room Worktree - Git Worktree Integration for Agent Isolation

    MASC v2 feature: Each agent works in isolated git worktrees
    to prevent file conflicts during parallel work.

    Extracted from room.ml for modularity.
*)

open Types
open Room_utils

(** Get git root directory - delegates to Room_git *)
let git_root config =
  Room_git.git_root ~base_path:config.base_path

(** Check if directory is a git repository - delegates to Room_git *)
let is_git_repo config =
  Room_git.is_git_repo ~base_path:config.base_path

(** Create worktree for agent - Result version *)
let worktree_create_r config ~agent_name ~task_id ~base_branch : string masc_result =
  if not (is_initialized config) then
    Error NotInitialized
  else if not (is_git_repo config) then
    Error (IoError "Not a git repository. MASC v2 requires .git directory for worktree isolation.")
  else match validate_agent_name_r agent_name, validate_task_id_r task_id with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok _, Ok _ -> begin
    match git_root config with
    | None -> Error (IoError "Cannot determine git root")
    | Some root ->
        let worktree_name = Printf.sprintf "%s-%s" agent_name task_id in
        let worktree_path = Filename.concat root (Filename.concat ".worktrees" worktree_name) in
        let branch_name = Printf.sprintf "%s/%s" agent_name task_id in
        let update_agent_current_task () =
          let agent_file = Filename.concat (agents_dir config) (safe_filename agent_name ^ ".json") in
          if Sys.file_exists agent_file then begin
            let content = In_channel.with_open_text agent_file In_channel.input_all in
            match Yojson.Safe.from_string content |> agent_of_yojson with
            | Ok agent ->
                let updated_agent = { agent with current_task = Some worktree_name } in
                let oc = open_out agent_file in
                output_string oc (Yojson.Safe.pretty_to_string (agent_to_yojson updated_agent));
                close_out oc
            | Error _ -> ()
          end
        in

        (* Create .worktrees directory if not exists *)
        let worktrees_dir = Filename.concat root ".worktrees" in
        if not (Sys.file_exists worktrees_dir) then
          Unix.mkdir worktrees_dir 0o755;

        (* Check if worktree already exists *)
        if Sys.file_exists worktree_path then begin
          update_agent_current_task ();
          Ok (Printf.sprintf "✅ Worktree already exists:\n  Path: %s\n  Branch: %s\n\nNext: cd %s"
              worktree_path branch_name worktree_path)
        end else begin
          (* Fetch origin first *)
          let fetch_cmd = Printf.sprintf "cd %s && git fetch origin 2>&1" root in
          let _ = Sys.command fetch_cmd in

          match Room_git.resolve_base_branch root base_branch with
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
                (* Update agent's current_worktree in state *)
                update_agent_current_task ();

                (* Log event *)
                let event = Printf.sprintf
                  "{\"type\":\"worktree_create\",\"agent\":\"%s\",\"branch\":\"%s\",\"path\":\"%s\",\"ts\":\"%s\"}"
                  agent_name branch_name worktree_path (now_iso ()) in
                log_event config event;

                Ok (Printf.sprintf "✅ Worktree created:\n  Path: %s\n  Branch: %s%s\n\nNext: cd %s && work && gh pr create --draft"
                    worktree_path branch_name note worktree_path)
              end
              else
                Error (IoError (Printf.sprintf "Failed to create worktree from origin/%s." resolved_base))
        end
  end

(** Create worktree - backward compatible
    @deprecated Use worktree_create_r for type-safe error handling *)
let worktree_create config ~agent_name ~task_id ~base_branch =
  match worktree_create_r config ~agent_name ~task_id ~base_branch with
  | Ok msg -> msg
  | Error e -> masc_error_to_string e

(** Remove worktree - Result version *)
let worktree_remove_r config ~agent_name ~task_id : string masc_result =
  if not (is_initialized config) then
    Error NotInitialized
  else match validate_agent_name_r agent_name, validate_task_id_r task_id with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok _, Ok _ -> begin
    match git_root config with
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

            (* Log event *)
            let event = Printf.sprintf
              "{\"type\":\"worktree_remove\",\"agent\":\"%s\",\"branch\":\"%s\",\"ts\":\"%s\"}"
              agent_name branch_name (now_iso ()) in
            log_event config event;

            Ok (Printf.sprintf "✅ Worktree removed: %s\n   Branch: %s" worktree_path branch_name)
          end
          else
            Error (IoError "Failed to remove worktree. It may have uncommitted changes.")
        end
  end

(** Remove worktree - backward compatible
    @deprecated Use worktree_remove_r for type-safe error handling *)
let worktree_remove config ~agent_name ~task_id =
  match worktree_remove_r config ~agent_name ~task_id with
  | Ok msg -> msg
  | Error e -> masc_error_to_string e

(** List all worktrees *)
let worktree_list config =
  if not (is_initialized config) then
    `Assoc [("error", `String "MASC not initialized")]
  else begin
    match git_root config with
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
              ("is_masc", `Bool (String.length !path > 11 &&
                try String.sub !path (String.length !path - 11) 11 = ".worktrees/"
                with _ -> false));
            ])
          else None
        in

        let worktrees = List.filter_map parse_block worktree_blocks in
        `Assoc [
          ("worktrees", `List worktrees);
          ("count", `Int (List.length worktrees));
          ("masc_hint", `String "Use masc_worktree_create to add a new worktree for your task");
        ]
  end
