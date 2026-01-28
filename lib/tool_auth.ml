(** Auth tools - Authentication and authorization *)

let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

let get_bool args key default =
  match Yojson.Safe.Util.member key args with
  | `Bool b -> b
  | _ -> default

type context = {
  config: Room.config;
  agent_name: string;
}

type result = bool * string

let handle_auth_enable ctx args =
  let require_token = get_bool args "require_token" false in
  let secret = Auth.enable_auth ctx.config.base_path ~require_token in
  let msg = Printf.sprintf {|🔐 **Authentication Enabled**

Room Secret (SAVE THIS - shown only once):
`%s`

Share this secret securely with authorized agents.
Require token for actions: %b

Use `masc_auth_create_token` to create agent tokens.
|} secret require_token in
  (true, msg)

let handle_auth_disable ctx _args =
  Auth.disable_auth ctx.config.base_path;
  (true, "🔓 Authentication disabled. All agents can perform any action.")

let handle_auth_status ctx _args =
  let cfg = Auth.load_auth_config ctx.config.base_path in
  let status = if cfg.enabled then "✅ Enabled" else "❌ Disabled" in
  let require = if cfg.require_token then "Yes" else "No (optional)" in
  let default = Types.agent_role_to_string cfg.default_role in
  let msg = Printf.sprintf {|🔐 **Authentication Status**

Status: %s
Require Token: %s
Default Role: %s
Token Expiry: %d hours
|} status require default cfg.token_expiry_hours in
  (true, msg)

let handle_auth_create_token ctx args =
  let role_str = get_string args "role" "worker" in
  let role = match Types.agent_role_of_string role_str with
    | Ok r -> r
    | Error _ -> Types.Worker
  in
  match Auth.create_token ctx.config.base_path ~agent_name:ctx.agent_name ~role with
  | Ok (raw_token, cred) ->
      let expires = match cred.expires_at with
        | Some exp -> exp
        | None -> "never"
      in
      let msg = Printf.sprintf {|🔑 **Token Created for %s**

Token (SAVE THIS - shown only once):
`%s`

Role: %s
Expires: %s

Pass this token in requests to authenticate.
|} ctx.agent_name raw_token (Types.agent_role_to_string role) expires in
      (true, msg)
  | Error e ->
      (false, Types.masc_error_to_string e)

let handle_auth_refresh ctx args =
  let token = get_string args "token" "" in
  match Auth.refresh_token ctx.config.base_path ~agent_name:ctx.agent_name ~old_token:token with
  | Ok (new_token, cred) ->
      let expires = match cred.expires_at with
        | Some exp -> exp
        | None -> "never"
      in
      let msg = Printf.sprintf {|🔄 **Token Refreshed for %s**

New Token:
`%s`

Expires: %s
|} ctx.agent_name new_token expires in
      (true, msg)
  | Error e ->
      (false, Types.masc_error_to_string e)

let handle_auth_revoke ctx _args =
  Auth.delete_credential ctx.config.base_path ctx.agent_name;
  (true, Printf.sprintf "🗑️ Token revoked for %s" ctx.agent_name)

let handle_auth_list ctx _args =
  let creds = Auth.list_credentials ctx.config.base_path in
  if creds = [] then
    (true, "No agent credentials found.")
  else begin
    let buf = Buffer.create 512 in
    Buffer.add_string buf "👥 **Agent Credentials**\n";
    Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
    List.iter (fun (c : Types.agent_credential) ->
      let expires = match c.expires_at with Some exp -> exp | None -> "never" in
      Buffer.add_string buf (Printf.sprintf "  • %s (%s) - expires: %s\n"
        c.agent_name (Types.agent_role_to_string c.role) expires)
    ) creds;
    (true, Buffer.contents buf)
  end

let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_auth_enable" -> Some (handle_auth_enable ctx args)
  | "masc_auth_disable" -> Some (handle_auth_disable ctx args)
  | "masc_auth_status" -> Some (handle_auth_status ctx args)
  | "masc_auth_create_token" -> Some (handle_auth_create_token ctx args)
  | "masc_auth_refresh" -> Some (handle_auth_refresh ctx args)
  | "masc_auth_revoke" -> Some (handle_auth_revoke ctx args)
  | "masc_auth_list" -> Some (handle_auth_list ctx args)
  | _ -> None
