(** Worktree tools - Git worktree management for task isolation *)

(* Argument helpers *)
let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

(* Context required by worktree tools *)
type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

(* Individual handlers *)
let handle_worktree_create ctx args =
  let task_id = get_string args "task_id" "" in
  let base_branch = get_string args "base_branch" "develop" in
  match Room.worktree_create_r ctx.config ~agent_name:ctx.agent_name ~task_id ~base_branch with
  | Ok msg -> (true, msg)
  | Error e -> (false, Types.masc_error_to_string e)

let handle_worktree_remove ctx args =
  let task_id = get_string args "task_id" "" in
  match Room.worktree_remove_r ctx.config ~agent_name:ctx.agent_name ~task_id with
  | Ok msg -> (true, msg)
  | Error e -> (false, Types.masc_error_to_string e)

let handle_worktree_list ctx _args =
  let json = Room.worktree_list ctx.config in
  (true, Yojson.Safe.pretty_to_string json)

(* Dispatch function - returns None if tool not handled *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_worktree_create" -> Some (handle_worktree_create ctx args)
  | "masc_worktree_remove" -> Some (handle_worktree_remove ctx args)
  | "masc_worktree_list" -> Some (handle_worktree_list ctx args)
  | _ -> None
