(** MASC Authentication & Authorization Module *)

open Types

(* ============================================ *)
(* Crypto utilities                             *)
(* ============================================ *)

(** Generate a cryptographically random token (hex string) *)
let generate_token () =
  let bytes = Bytes.create 32 in
  for i = 0 to 31 do
    Bytes.set bytes i (Char.chr (Random.int 256))
  done;
  let hex = Buffer.create 64 in
  Bytes.iter (fun c -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c))) bytes;
  Buffer.contents hex

(** SHA256 hash of a string using Digestif *)
let sha256_hash input =
  Digestif.SHA256.(digest_string input |> to_hex)

(* ============================================ *)
(* Auth directory management                    *)
(* ============================================ *)

let auth_dir config = Filename.concat config ".masc/auth"
let agents_dir config = Filename.concat (auth_dir config) "agents"
let room_secret_file config = Filename.concat (auth_dir config) "room_secret.hash"
let auth_config_file config = Filename.concat (auth_dir config) "config.json"

(** Ensure auth directories exist *)
let ensure_auth_dirs config =
  let auth = auth_dir config in
  let agents = agents_dir config in
  if not (Sys.file_exists auth) then Unix.mkdir auth 0o700;
  if not (Sys.file_exists agents) then Unix.mkdir agents 0o700

(* ============================================ *)
(* Auth config management                       *)
(* ============================================ *)

(** Load auth config *)
let load_auth_config config : auth_config =
  let file = auth_config_file config in
  if Sys.file_exists file then
    try
      let content = In_channel.with_open_text file In_channel.input_all in
      let json = Yojson.Safe.from_string content in
      match auth_config_of_yojson json with
      | Ok cfg -> cfg
      | Error _ -> default_auth_config
    with _ -> default_auth_config
  else
    default_auth_config

(** Save auth config *)
let save_auth_config config (auth_cfg : auth_config) =
  ensure_auth_dirs config;
  let file = auth_config_file config in
  let json = auth_config_to_yojson auth_cfg in
  Out_channel.with_open_text file (fun oc ->
    output_string oc (Yojson.Safe.pretty_to_string json)
  )

(* ============================================ *)
(* Credential management                        *)
(* ============================================ *)

(** Get credential file path for an agent *)
let credential_file config agent_name =
  Filename.concat (agents_dir config) (agent_name ^ ".json")

(** Load agent credential *)
let load_credential config agent_name : agent_credential option =
  let file = credential_file config agent_name in
  if Sys.file_exists file then
    try
      let content = In_channel.with_open_text file In_channel.input_all in
      let json = Yojson.Safe.from_string content in
      match agent_credential_of_yojson json with
      | Ok cred -> Some cred
      | Error _ -> None
    with _ -> None
  else
    None

(** Save agent credential *)
let save_credential config (cred : agent_credential) =
  ensure_auth_dirs config;
  let file = credential_file config cred.agent_name in
  let json = agent_credential_to_yojson cred in
  Out_channel.with_open_text file (fun oc ->
    output_string oc (Yojson.Safe.pretty_to_string json)
  )

(** Delete agent credential *)
let delete_credential config agent_name =
  let file = credential_file config agent_name in
  if Sys.file_exists file then Sys.remove file

(** List all credentials *)
let list_credentials config : agent_credential list =
  let dir = agents_dir config in
  if Sys.file_exists dir then
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".json")
    |> List.filter_map (fun f ->
        let name = Filename.chop_suffix f ".json" in
        load_credential config name
      )
  else
    []

(* ============================================ *)
(* Token operations                             *)
(* ============================================ *)

(** Create a new token for an agent *)
let create_token config ~agent_name ~role : (string * agent_credential, masc_error) result =
  let auth_cfg = load_auth_config config in
  let raw_token = generate_token () in
  let token_hash = sha256_hash raw_token in
  let now = now_iso () in
  let expires_at =
    if auth_cfg.token_expiry_hours > 0 then
      let expiry = Unix.gettimeofday () +. (float_of_int auth_cfg.token_expiry_hours *. 3600.0) in
      let tm = Unix.gmtime expiry in
      Some (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
        (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
    else
      None
  in
  let cred = {
    agent_name;
    token = token_hash;
    role;
    created_at = now;
    expires_at;
  } in
  save_credential config cred;
  Ok (raw_token, cred)  (* Return raw token to user, store hash *)

(** Verify a token *)
let verify_token config ~agent_name ~token : (agent_credential, masc_error) result =
  match load_credential config agent_name with
  | None -> Error (Unauthorized ("No credential found for " ^ agent_name))
  | Some cred ->
      let token_hash = sha256_hash token in
      if cred.token <> token_hash then
        Error (InvalidToken "Token mismatch")
      else
        (* Check expiry *)
        match cred.expires_at with
        | None -> Ok cred
        | Some exp_str ->
            (* Simple ISO string comparison works for UTC *)
            let now = now_iso () in
            if now > exp_str then
              Error (TokenExpired agent_name)
            else
              Ok cred

(** Refresh a token (generate new one, update credential) *)
let refresh_token config ~agent_name ~old_token : (string * agent_credential, masc_error) result =
  match verify_token config ~agent_name ~token:old_token with
  | Error (TokenExpired _) ->
      (* Allow refresh even if expired *)
      (match load_credential config agent_name with
       | None -> Error (Unauthorized ("No credential found for " ^ agent_name))
       | Some old_cred -> create_token config ~agent_name ~role:old_cred.role)
  | Error e -> Error e
  | Ok old_cred -> create_token config ~agent_name ~role:old_cred.role

(* ============================================ *)
(* Authorization                                *)
(* ============================================ *)

(** Check if agent has permission for an action *)
let check_permission config ~agent_name ~token ~permission : (unit, masc_error) result =
  let auth_cfg = load_auth_config config in
  if not auth_cfg.enabled then
    (* Auth disabled - allow everything *)
    Ok ()
  else if not auth_cfg.require_token then
    (* Token not required - use default role *)
    if has_permission auth_cfg.default_role permission then
      Ok ()
    else
      Error (Forbidden { agent = agent_name; action = show_permission permission })
  else
    (* Token required - verify and check role *)
    match token with
    | None -> Error (Unauthorized "Token required")
    | Some t ->
        match verify_token config ~agent_name ~token:t with
        | Error e -> Error e
        | Ok cred ->
            if has_permission cred.role permission then
              Ok ()
            else
              Error (Forbidden { agent = agent_name; action = show_permission permission })

(** Map MCP tool name to required permission *)
let permission_for_tool = function
  | "masc_init" -> Some CanInit
  | "masc_reset" -> Some CanReset
  | "masc_join" -> Some CanJoin
  | "masc_leave" -> Some CanLeave
  | "masc_status" | "masc_who" | "masc_tasks" | "masc_messages"
  | "masc_agents" | "masc_portal_status" | "masc_pending_interrupts"
  | "masc_votes" | "masc_vote_status" | "masc_worktree_list"
  | "masc_cost_report" | "masc_task_history" -> Some CanReadState
  | "masc_add_task" -> Some CanAddTask
  | "masc_claim" | "masc_claim_next" -> Some CanClaimTask
  | "masc_done" | "masc_update_priority" | "masc_transition" | "masc_release" -> Some CanCompleteTask
  | "masc_broadcast" | "masc_listen" | "masc_heartbeat"
  | "masc_register_capabilities" | "masc_find_by_capability"
  | "masc_agent_update" -> Some CanBroadcast
  | "masc_lock" -> Some CanLockFile
  | "masc_unlock" -> Some CanUnlockFile
  | "masc_portal_open" | "masc_portal_close" -> Some CanOpenPortal
  | "masc_portal_send" -> Some CanSendPortal
  | "masc_worktree_create" -> Some CanCreateWorktree
  | "masc_worktree_remove" -> Some CanRemoveWorktree
  | "masc_vote_create" | "masc_vote_cast" -> Some CanVote
  | "masc_interrupt" | "masc_branch" -> Some CanInterrupt
  | "masc_approve" | "masc_reject" -> Some CanApprove
  | "masc_cost_log" | "masc_cleanup_zombies" -> Some CanBroadcast (* Worker level *)
  (* Auth tools - special handling *)
  | "masc_auth_enable" | "masc_auth_disable" | "masc_auth_create_token"
  | "masc_auth_revoke" -> Some CanInit  (* Admin only *)
  | "masc_auth_status" | "masc_auth_refresh" -> Some CanReadState
  | _ -> None

(** Check permission for a tool call *)
let authorize_tool config ~agent_name ~token ~tool_name : (unit, masc_error) result =
  match permission_for_tool tool_name with
  | None -> Ok ()  (* Unknown tool - allow (fail-open for extensibility) *)
  | Some perm -> check_permission config ~agent_name ~token ~permission:perm

(* ============================================ *)
(* Room secret (for room-level auth)            *)
(* ============================================ *)

(** Initialize room secret *)
let init_room_secret config : string =
  ensure_auth_dirs config;
  let secret = generate_token () in
  let hash = sha256_hash secret in
  Out_channel.with_open_text (room_secret_file config) (fun oc ->
    output_string oc hash
  );
  (* Update auth config with hash *)
  let cfg = load_auth_config config in
  save_auth_config config { cfg with room_secret_hash = Some hash };
  secret  (* Return raw secret to show user once *)

(** Verify room secret *)
let verify_room_secret config secret : bool =
  let hash = sha256_hash secret in
  let file = room_secret_file config in
  if Sys.file_exists file then
    let stored_hash = String.trim (In_channel.with_open_text file In_channel.input_all) in
    hash = stored_hash
  else
    false

(* ============================================ *)
(* High-level auth operations                   *)
(* ============================================ *)

(** Enable authentication for a room *)
let enable_auth config ~require_token : string =
  let secret = init_room_secret config in
  let cfg = load_auth_config config in
  save_auth_config config { cfg with enabled = true; require_token };
  secret

(** Disable authentication *)
let disable_auth config =
  let cfg = load_auth_config config in
  save_auth_config config { cfg with enabled = false }

(** Check if auth is enabled *)
let is_auth_enabled config : bool =
  let cfg = load_auth_config config in
  cfg.enabled
