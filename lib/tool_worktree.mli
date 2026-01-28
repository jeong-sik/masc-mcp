(** Worktree tools - Git worktree management for task isolation *)

val get_string : Yojson.Safe.t -> string -> string -> string

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

val handle_worktree_create : context -> Yojson.Safe.t -> result
val handle_worktree_remove : context -> Yojson.Safe.t -> result
val handle_worktree_list : context -> Yojson.Safe.t -> result

val dispatch : context -> name:string -> args:Yojson.Safe.t -> result option
