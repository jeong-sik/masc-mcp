(** MASC Mode System - Category-based tool filtering

    Inspired by Serena MCP's switch_modes pattern.
    Reduces token usage by ~50% in minimal mode.
*)

(** Tool categories *)
type category =
  | Core        (* init, join, status, tasks, claim, done... *)
  | Comm        (* broadcast, messages, lock, unlock, listen, who, reset *)
  | Portal      (* portal_open, portal_send, portal_close, portal_status *)
  | Worktree    (* worktree_create, worktree_remove, worktree_list *)
  | Health      (* heartbeat, cleanup_zombies, gc, agents *)
  | Discovery   (* register_capabilities, find_by_capability *)
  | Voting      (* vote_create, vote_cast, vote_status, votes *)
  | Interrupt   (* interrupt, approve, reject, pending_interrupts, branch *)
  | Cost        (* cost_log, cost_report *)
  | Auth        (* auth_enable, auth_disable, auth_status, auth_create_token... *)
  | RateLimit   (* rate_limit_status, rate_limit_config *)
  | Encryption  (* encryption_status, encryption_enable, encryption_disable, generate_key *)

(** Mode presets *)
type mode =
  | Minimal   (* core, health *)
  | Standard  (* core, comm, worktree, health *)
  | Parallel  (* heavy multi-agent: core+comm+portal+worktree+health+discovery+voting+interrupt *)
  | Full      (* all categories *)
  | Solo      (* core, worktree - for single-agent work *)
  | Custom    (* user-defined categories *)

(** Category to string conversion *)
let category_to_string = function
  | Core -> "core"
  | Comm -> "comm"
  | Portal -> "portal"
  | Worktree -> "worktree"
  | Health -> "health"
  | Discovery -> "discovery"
  | Voting -> "voting"
  | Interrupt -> "interrupt"
  | Cost -> "cost"
  | Auth -> "auth"
  | RateLimit -> "ratelimit"
  | Encryption -> "encryption"

(** String to category conversion *)
let category_of_string = function
  | "core" -> Some Core
  | "comm" -> Some Comm
  | "portal" -> Some Portal
  | "worktree" -> Some Worktree
  | "health" -> Some Health
  | "discovery" -> Some Discovery
  | "voting" -> Some Voting
  | "interrupt" -> Some Interrupt
  | "cost" -> Some Cost
  | "auth" -> Some Auth
  | "ratelimit" -> Some RateLimit
  | "encryption" -> Some Encryption
  | _ -> None

(** Mode to string conversion *)
let mode_to_string = function
  | Minimal -> "minimal"
  | Standard -> "standard"
  | Parallel -> "parallel"
  | Full -> "full"
  | Solo -> "solo"
  | Custom -> "custom"

(** String to mode conversion *)
let mode_of_string = function
  | "minimal" -> Some Minimal
  | "standard" -> Some Standard
  | "parallel" -> Some Parallel
  | "full" -> Some Full
  | "solo" -> Some Solo
  | "custom" -> Some Custom
  | _ -> None

(** All categories *)
let all_categories = [
  Core; Comm; Portal; Worktree; Health; Discovery;
  Voting; Interrupt; Cost; Auth; RateLimit; Encryption
]

(** Categories for each mode preset *)
let categories_for_mode = function
  | Minimal -> [Core; Health]
  | Standard -> [Core; Comm; Worktree; Health]
  | Parallel -> [Core; Comm; Portal; Worktree; Health; Discovery; Voting; Interrupt]
  | Full -> all_categories
  | Solo -> [Core; Worktree]
  | Custom -> [] (* Will be loaded from config *)

(** Tool name to category mapping *)
let tool_category tool_name =
  match tool_name with
  (* Core tools *)
  | "masc_set_room" | "masc_init" | "masc_join" | "masc_leave"
  | "masc_status" | "masc_add_task" | "masc_claim" | "masc_done"
  | "masc_tasks" | "masc_archive_view" | "masc_claim_next"
  | "masc_update_priority" | "masc_transition" | "masc_release"
  | "masc_run_init" | "masc_run_plan" | "masc_run_log"
  | "masc_run_deliverable" | "masc_run_get" | "masc_run_list"
  | "masc_task_history" -> Core

  (* Communication tools *)
  | "masc_broadcast" | "masc_messages" | "masc_lock" | "masc_unlock"
  | "masc_listen" | "masc_who" | "masc_reset"
  | "masc_subscription" | "masc_progress" -> Comm

  (* Portal A2A tools *)
  | "masc_portal_open" | "masc_portal_send" | "masc_portal_close"
  | "masc_portal_status" -> Portal

  (* Git worktree tools *)
  | "masc_worktree_create" | "masc_worktree_remove"
  | "masc_worktree_list" -> Worktree

  (* Health & maintenance tools *)
  | "masc_heartbeat" | "masc_cleanup_zombies" | "masc_gc"
  | "masc_agents"
  | "masc_cache_set" | "masc_cache_get" | "masc_cache_delete"
  | "masc_cache_list" | "masc_cache_clear" | "masc_cache_stats"
  | "masc_tempo" | "masc_tempo_get" | "masc_tempo_set"
  | "masc_tempo_adjust" | "masc_tempo_reset"
  | "masc_mcp_session" | "masc_cancellation"
  | "masc_relay_status" | "masc_relay_checkpoint"
  | "masc_relay_now" | "masc_relay_smart_check"
  | "masc_mitosis_status" | "masc_mitosis_all" | "masc_mitosis_pool"
  | "masc_mitosis_divide" | "masc_mitosis_check" | "masc_mitosis_record"
  | "masc_mitosis_prepare" | "masc_memento_mori"
  | "masc_verify_handoff" -> Health

  (* Agent discovery tools *)
  | "masc_register_capabilities" | "masc_find_by_capability"
  | "masc_agent_update"
  | "masc_get_metrics" | "masc_agent_fitness" | "masc_select_agent"
  | "masc_collaboration_graph" | "masc_consolidate_learning" -> Discovery

  (* Voting/consensus tools *)
  | "masc_vote_create" | "masc_vote_cast" | "masc_vote_status"
  | "masc_votes" -> Voting

  (* Interrupt/checkpoint tools *)
  | "masc_interrupt" | "masc_approve" | "masc_reject"
  | "masc_pending_interrupts" | "masc_branch" -> Interrupt

  (* Cost tracking tools *)
  | "masc_cost_log" | "masc_cost_report" -> Cost

  (* Authentication tools *)
  | "masc_auth_enable" | "masc_auth_disable" | "masc_auth_status"
  | "masc_auth_create_token" | "masc_auth_refresh" | "masc_auth_revoke"
  | "masc_auth_list"
  | "masc_audit_query" | "masc_audit_stats" | "masc_governance_set" -> Auth

  (* Rate limiting tools *)
  | "masc_rate_limit_status" | "masc_rate_limit_config" -> RateLimit

  (* Encryption tools *)
  | "masc_encryption_status" | "masc_encryption_enable"
  | "masc_encryption_disable" | "masc_generate_key" -> Encryption

  (* Mode management tools - always available *)
  | "masc_switch_mode" | "masc_get_config" -> Core

  (* Unknown tools default to Core *)
  | _ -> Core

(** Check if a tool is enabled for given categories *)
let is_tool_enabled enabled_categories tool_name =
  let cat = tool_category tool_name in
  List.mem cat enabled_categories

(** Mode descriptions for help text *)
let mode_description = function
  | Minimal -> "Core task management + health checks only"
  | Standard -> "Core + communication + worktree + health"
  | Parallel -> "Multi-agent parallel mode: comm + portal + discovery + voting + interrupt"
  | Full -> "All features enabled"
  | Solo -> "Single-agent mode: core + worktree, no multi-agent features"
  | Custom -> "Custom category selection"

(** Category descriptions *)
let category_description = function
  | Core -> "Task management: init, join, status, tasks, claim, done"
  | Comm -> "Communication: broadcast, messages, lock, unlock, listen"
  | Portal -> "A2A direct messaging: portal_open, portal_send"
  | Worktree -> "Git worktrees: worktree_create, worktree_list"
  | Health -> "Maintenance: heartbeat, cache, tempo, relay/mitosis, gc"
  | Discovery -> "Agent discovery & metrics: register_capabilities, fitness, collaboration"
  | Voting -> "Consensus: vote_create, vote_cast, votes"
  | Interrupt -> "Checkpoints: interrupt, approve, reject, branch"
  | Cost -> "Cost tracking: cost_log, cost_report"
  | Auth -> "Authentication & governance: auth_enable, audit_query"
  | RateLimit -> "Rate limits: rate_limit_status, rate_limit_config"
  | Encryption -> "Data protection: encryption_enable, generate_key"

(** JSON serialization *)
let categories_to_json cats =
  `List (List.map (fun c -> `String (category_to_string c)) cats)

let categories_of_json json =
  match json with
  | `List items ->
    List.filter_map (fun item ->
      match item with
      | `String s -> category_of_string s
      | _ -> None
    ) items
  | _ -> []
