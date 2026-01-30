(** Auto-Responder Daemon - Automatic @mention response via spawn

    When a broadcast message contains @mention, automatically spawn
    the mentioned agent to respond.

    Enable with: MASC_AUTO_RESPOND=true

    Chain limit: Max 3 responses per minute to prevent infinite loops
*)

(** Auto-respond mode *)
type mode = Disabled | Spawn | Llm

(** Check auto-respond mode *)
let get_mode () =
  match Sys.getenv_opt "MASC_AUTO_RESPOND" with
  | Some "true" | Some "1" | Some "yes" | Some "spawn" -> Spawn
  | Some "llm" | Some "fast" -> Llm
  | _ -> Disabled

(** Check if auto-respond is enabled *)
let is_enabled () = get_mode () <> Disabled

(** Activity log file - human readable, shared across all modes *)
let activity_log_file () =
  match Sys.getenv_opt "ME_ROOT" with
  | Some root -> root ^ "/logs/auto-responder.log"
  | None -> "/tmp/auto-responder.log"

(** Debug logging to file *)
let debug_log msg =
  let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 "/tmp/auto_debug.log" in
  Printf.fprintf oc "[%f] %s\n%!" (Unix.gettimeofday ()) msg;
  close_out oc

(** Activity logging - human readable format *)
let activity_log ~mode ~from_agent ~mention ~status ~detail =
  let log_file = activity_log_file () in
  let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
  let time = Unix.localtime (Unix.gettimeofday ()) in
  let timestamp = Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec
  in
  let mode_str = match mode with Disabled -> "OFF" | Spawn -> "SPAWN" | Llm -> "LLM" in
  Printf.fprintf oc "[%s] [%s] %s → @%s | %s | %s\n%!"
    timestamp mode_str from_agent mention status detail;
  close_out oc

(** Chain limit: track recent responses to prevent infinite loops *)
let recent_responses : (string, float) Hashtbl.t = Hashtbl.create 16
let chain_limit = 3  (* max responses per agent type per minute *)
let chain_window = 60.0  (* seconds *)

(** Check if we should throttle this response *)
let should_throttle ~agent_type =
  let now = Unix.gettimeofday () in
  let key = agent_type in
  (* Clean old entries *)
  Hashtbl.filter_map_inplace (fun _ ts ->
    if now -. ts < chain_window then Some ts else None
  ) recent_responses;
  (* Count recent responses for this agent type *)
  let count = Hashtbl.fold (fun k _ acc ->
    if String.length k >= String.length agent_type &&
       String.sub k 0 (String.length agent_type) = agent_type
    then acc + 1 else acc
  ) recent_responses 0 in
  if count >= chain_limit then begin
    debug_log (Printf.sprintf "THROTTLE: %s has %d responses in last %.0fs" agent_type count chain_window);
    true
  end else begin
    Hashtbl.add recent_responses (Printf.sprintf "%s-%f" key now) now;
    false
  end

(** LLM-MCP endpoint (default: 8932) *)
let llm_mcp_url () =
  match Sys.getenv_opt "LLM_MCP_URL" with
  | Some url -> url
  | None -> "http://127.0.0.1:8932/mcp"

(** Re-export from Mention module for convenience *)
let spawnable_agents = Mention.spawnable_agents
let agent_type_of_mention = Mention.agent_type_of_mention
let is_spawnable = Mention.is_spawnable

(** Build shell command for spawning an agent *)
let build_spawn_command ~agent_type ~prompt ~working_dir =
  let escaped_prompt = Filename.quote prompt in
  let base_cmd = match agent_type with
    | "claude" -> Printf.sprintf "echo %s | claude -p --allowedTools 'mcp__masc__*'" escaped_prompt
    | "gemini" -> Printf.sprintf "echo %s | gemini --yolo" escaped_prompt
    | "codex" -> Printf.sprintf "echo %s | codex exec" escaped_prompt
    | "ollama" -> Printf.sprintf "echo %s | ollama run qwen3:30b" escaped_prompt
    | "glm" ->
        (* GLM has no standalone CLI - use temp file to avoid shell escaping issues *)
        let llm_url = llm_mcp_url () in
        let json_escaped =
          prompt
          |> String.split_on_char '"' |> String.concat {|\"|}
          |> String.split_on_char '\n' |> String.concat {|\\n|}
        in
        Printf.sprintf "echo '{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"glm\",\"arguments\":{\"prompt\":\"%s\"}}}' > /tmp/glm_spawn_$$.json && curl -s '%s' -H 'Content-Type: application/json' -d @/tmp/glm_spawn_$$.json && rm -f /tmp/glm_spawn_$$.json" json_escaped llm_url
    | _ -> Printf.sprintf "echo %s | %s" escaped_prompt agent_type
  in
  Printf.sprintf "cd %s && timeout 120 %s" (Filename.quote working_dir) base_cmd

(** Build prompt for auto-response *)
let build_response_prompt ~from_agent ~content ~mention =
  Printf.sprintf {|You received a mention in the MASC room from %s.

Message: "%s"

Quick response protocol:
1. Call mcp__masc__masc_join(agent_name="%s")
   → Read the response to get your assigned nickname (e.g., "gemini-rare-beaver")
2. Call mcp__masc__masc_broadcast using YOUR ASSIGNED NICKNAME from step 1:
   mcp__masc__masc_broadcast(agent_name="<your-assigned-nickname>", message="[your concise response]")
   IMPORTANT: Do NOT use "%s" - use the full nickname from the join response!
3. Call mcp__masc__masc_leave()

Respond in 1-2 sentences. Be helpful and concise.|}
    from_agent content mention mention

(** Spawn agent in background (non-blocking) - Heavy mode *)
let spawn_in_background ~agent_type ~prompt ~working_dir =
  let cmd = build_spawn_command ~agent_type ~prompt ~working_dir in
  let log_file = Printf.sprintf "/tmp/auto_respond_%s_%d.log"
    agent_type (int_of_float (Unix.gettimeofday () *. 1000.) mod 10000)
  in
  (* Write command to file for debugging *)
  let debug_file = "/tmp/auto_spawn_cmd.txt" in
  let oc = open_out debug_file in
  Printf.fprintf oc "CMD: %s\nLOG: %s\n" cmd log_file;
  close_out oc;
  (* Run in background with nohup - use script file to avoid quoting issues *)
  let script_file = "/tmp/auto_spawn_script.sh" in
  let sc = open_out script_file in
  Printf.fprintf sc "#!/bin/bash\n%s\n" cmd;
  close_out sc;
  ignore (Unix.chmod script_file 0o755);
  let bg_cmd = Printf.sprintf "nohup %s > %s 2>&1 &" script_file log_file in
  debug_log (Printf.sprintf "SPAWN_CMD: %s" bg_cmd);
  let ret = Unix.system bg_cmd in
  debug_log (Printf.sprintf "SPAWN_RET: %s" (match ret with Unix.WEXITED n -> string_of_int n | _ -> "signal"))

(** Call llm-mcp directly (fast mode) - Returns LLM response or error *)
let call_llm_mcp_sync ~agent_type ~prompt =
  let url = llm_mcp_url () in
  let tool_name = match agent_type with
    | "gemini" -> "gemini"
    | "claude" -> "claude-cli"
    | "codex" -> "codex"
    | "glm" -> "glm"
    | _ -> "ollama"  (* ollama is the fallback for unknown types *)
  in
  (* Escape prompt for JSON - replace quotes and newlines *)
  let escaped_prompt =
    prompt
    |> String.split_on_char '"' |> String.concat {|\"|}
    |> String.split_on_char '\n' |> String.concat {|\n|}
  in
  let json_body = Printf.sprintf
    {|{"jsonrpc":"2.0","method":"tools/call","id":1,"params":{"name":"%s","arguments":{"prompt":"%s","response_format":"compact"}}}|}
    tool_name escaped_prompt
  in
  (* Use temp file with unique name to avoid race conditions between threads *)
  let tmp_file = Printf.sprintf "/tmp/llm_call_%d_%d.json"
    (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.) mod 1000000) in
  let oc = open_out tmp_file in
  output_string oc json_body;
  close_out oc;
  let cmd = Printf.sprintf
    "curl -s '%s' -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' -d @%s 2>/dev/null | jq -r '.result.content[0].text // .error.message // \"no response\"' 2>/dev/null | head -c 500"
    url tmp_file
  in
  let ic = Unix.open_process_in cmd in
  let response = try input_line ic with End_of_file -> "no response" in
  ignore (Unix.close_process_in ic);
  Safe_ops.remove_file_logged ~context:"auto_responder" tmp_file;
  response

(** MASC HTTP helper - call MASC tool via HTTP (using temp file to avoid escaping issues) *)
let masc_call ~tool_name ~args =
  let masc_url = Printf.sprintf "http://127.0.0.1:%d/mcp"
    (match Sys.getenv_opt "MASC_HTTP_PORT" with Some p -> int_of_string p | None -> 8935)
  in
  let body = Printf.sprintf
    {|{"jsonrpc":"2.0","method":"tools/call","id":1,"params":{"name":"%s","arguments":%s}}|}
    tool_name args
  in
  (* Use temp file to avoid shell escaping issues *)
  let tmp_file = Printf.sprintf "/tmp/masc_call_%d_%d.json"
    (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000000.) mod 1000000) in
  let oc = open_out tmp_file in
  output_string oc body;
  close_out oc;
  let cmd = Printf.sprintf
    "curl -s '%s' -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' -d @%s 2>/dev/null"
    masc_url tmp_file
  in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 256 in
  (try while true do Buffer.add_string buf (input_line ic); Buffer.add_char buf '\n' done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  Safe_ops.remove_file_logged ~context:"auto_responder" tmp_file;
  Buffer.contents buf

(** Extract nickname from MASC join response *)
let extract_nickname response =
  (* Look for "Nickname: xxx" pattern *)
  let lines = String.split_on_char '\n' response in
  let rec find = function
    | [] -> None
    | line :: rest ->
        if String.length line > 12 && String.sub line 0 10 = "  Nickname:" then
          Some (String.trim (String.sub line 10 (String.length line - 10)))
        else find rest
  in
  find lines

(** Call llm-mcp and broadcast response (fast mode, background)
    New approach: Auto-responder handles MASC join/broadcast/leave directly *)
let call_llm_and_broadcast ~agent_type ~prompt ~mention ~base_path =
  ignore base_path;
  (* Step 1: Get simple answer from LLM (prompt is the original message content) *)
  let response = call_llm_mcp_sync ~agent_type ~prompt in
  debug_log (Printf.sprintf "LLM_RESPONSE: %s" (if String.length response > 100 then String.sub response 0 100 ^ "..." else response));

  if response <> "" && response <> "no response" then begin
    (* Step 2: Join MASC to get assigned nickname *)
    let join_args = Printf.sprintf {|{"agent_name":"%s","capabilities":["llm-auto-responder"]}|} agent_type in
    let join_resp = masc_call ~tool_name:"masc_join" ~args:join_args in
    debug_log (Printf.sprintf "MASC_JOIN: %s" (if String.length join_resp > 200 then String.sub join_resp 0 200 ^ "..." else join_resp));

    match extract_nickname join_resp with
    | None ->
        debug_log "MASC_JOIN_FAILED: Could not extract nickname";
        Printf.eprintf "[Auto-Responder/LLM] Failed to join MASC\n%!"
    | Some nickname ->
        debug_log (Printf.sprintf "MASC_NICKNAME: %s" nickname);

        (* Step 3: Broadcast response with proper nickname *)
        let message = Printf.sprintf "@%s %s" mention (String.escaped response) in
        let broadcast_args = Printf.sprintf {|{"agent_name":"%s","message":"%s"}|} nickname message in
        let _ = masc_call ~tool_name:"masc_broadcast" ~args:broadcast_args in
        debug_log "MASC_BROADCAST: sent";

        (* Step 4: Leave MASC *)
        let leave_args = Printf.sprintf {|{"agent_name":"%s"}|} nickname in
        let _ = masc_call ~tool_name:"masc_leave" ~args:leave_args in
        debug_log "MASC_LEAVE: done";

        let short_resp = if String.length response > 50 then String.sub response 0 50 ^ "..." else response in
        Printf.eprintf "[Auto-Responder/LLM] %s: %s\n%!" nickname short_resp
  end else
    Printf.eprintf "[Auto-Responder/LLM] LLM returned empty response\n%!"

(** Run LLM call in background process (Thread.create doesn't work well with Eio)
    Creates a shell script that does: LLM call -> MASC join -> broadcast -> leave *)
let llm_call_in_background ~agent_type ~prompt ~mention ~base_path =
  ignore base_path;
  let llm_url = llm_mcp_url () in
  let masc_port = match Sys.getenv_opt "MASC_HTTP_PORT" with Some p -> p | None -> "8935" in
  let masc_url = Printf.sprintf "http://127.0.0.1:%s/mcp" masc_port in
  let escaped_prompt =
    prompt
    |> String.split_on_char '"' |> String.concat {|\\\"|}
    |> String.split_on_char '\n' |> String.concat " "
    |> String.split_on_char '\'' |> String.concat {|'\\''|}
  in
  (* MASC requires Accept header with both application/json and text/event-stream *)
  let accept_header = "-H 'Accept: application/json, text/event-stream'" in
  let script = Printf.sprintf {|#!/bin/bash
# LLM call in background for %s
set -e

# Step 1: Call LLM
RESPONSE=$(curl -s '%s' -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","method":"tools/call","id":1,"params":{"name":"%s","arguments":{"prompt":"%s","response_format":"compact"}}}' \
  | jq -r '.result.content[0].text // "no response"' | head -c 300)

echo "[LLM] Response: $RESPONSE" >> /tmp/auto_debug.log

if [ "$RESPONSE" = "no response" ] || [ -z "$RESPONSE" ]; then
  echo "[LLM] Empty response, skipping MASC" >> /tmp/auto_debug.log
  exit 0
fi

# Step 2: Join MASC (with Accept header for SSE support)
JOIN_RESP=$(curl -s '%s' -H 'Content-Type: application/json' %s \
  -d '{"jsonrpc":"2.0","method":"tools/call","id":1,"params":{"name":"masc_join","arguments":{"agent_name":"%s","capabilities":["llm-auto-responder"]}}}')

# macOS compatible: use sed instead of grep -oP
NICKNAME=$(echo "$JOIN_RESP" | sed -n 's/.*Nickname: \([a-z]*-[a-z]*-[a-z]*\).*/\1/p' | head -1)
echo "[MASC] Nickname: $NICKNAME" >> /tmp/auto_debug.log

if [ -z "$NICKNAME" ]; then
  echo "[MASC] Failed to join, no nickname" >> /tmp/auto_debug.log
  exit 1
fi

# Step 3: Broadcast (escape response for JSON)
ESCAPED_RESP=$(echo "$RESPONSE" | sed 's/"/\\"/g' | tr '\n' ' ')
curl -s '%s' -H 'Content-Type: application/json' %s \
  -d "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"masc_broadcast\",\"arguments\":{\"agent_name\":\"$NICKNAME\",\"message\":\"@%s $ESCAPED_RESP\"}}}" > /dev/null

echo "[MASC] Broadcast sent" >> /tmp/auto_debug.log

# Step 4: Leave
curl -s '%s' -H 'Content-Type: application/json' %s \
  -d "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":1,\"params\":{\"name\":\"masc_leave\",\"arguments\":{\"agent_name\":\"$NICKNAME\"}}}" > /dev/null

echo "[MASC] Left room" >> /tmp/auto_debug.log
|} agent_type llm_url agent_type escaped_prompt masc_url accept_header agent_type masc_url accept_header mention masc_url accept_header
  in
  let script_file = Printf.sprintf "/tmp/llm_bg_%d_%d.sh"
    (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.) mod 10000) in
  let log_file = Printf.sprintf "/tmp/llm_bg_%s_%d.log"
    agent_type (int_of_float (Unix.gettimeofday () *. 1000.) mod 10000) in
  let oc = open_out script_file in
  output_string oc script;
  close_out oc;
  ignore (Unix.chmod script_file 0o755);
  let bg_cmd = Printf.sprintf "nohup %s > %s 2>&1 &" script_file log_file in
  debug_log (Printf.sprintf "LLM_BG_CMD: %s" bg_cmd);
  let ret = Unix.system bg_cmd in
  debug_log (Printf.sprintf "LLM_BG_RET: %s" (match ret with Unix.WEXITED n -> string_of_int n | _ -> "signal"))

(** Maybe spawn a response agent if mention detected and enabled

    Returns:
    - Some task_id if spawn was triggered
    - None if no spawn needed
*)
let maybe_respond ~base_path ~from_agent ~content ~mention =
  let mode = get_mode () in
  let mode_str = match mode with Disabled -> "Disabled" | Spawn -> "Spawn" | Llm -> "Llm" in
  debug_log (Printf.sprintf "CALLED: from=%s mention=%s mode=%s enabled=%b"
    from_agent (match mention with Some m -> m | None -> "NONE") mode_str (is_enabled ()));
  match mention with
  | None ->
      debug_log "EXIT: No mention";
      None
  | Some _m when not (is_enabled ()) ->
      let env_val = match Sys.getenv_opt "MASC_AUTO_RESPOND" with Some v -> v | None -> "not set" in
      debug_log (Printf.sprintf "EXIT: Disabled (env=%s)" env_val);
      Printf.eprintf "[Auto-Responder] Disabled (MASC_AUTO_RESPOND=%s)\n%!" env_val;
      None
  | Some m ->
      let from_base = agent_type_of_mention from_agent in
      let mention_base = agent_type_of_mention m in
      debug_log (Printf.sprintf "CHECK: from_base=%s mention_base=%s spawnable=%b" from_base mention_base (is_spawnable m));
      (* Prevent infinite loop: don't respond to same agent type *)
      if from_base = mention_base then begin
        debug_log "EXIT: Self-mention";
        Printf.eprintf "[Auto-Responder] Skip self-mention @%s from %s\n%!" m from_agent;
        None
      end
      else if not (is_spawnable m) then begin
        debug_log "EXIT: Not spawnable";
        activity_log ~mode ~from_agent ~mention:m ~status:"SKIP" ~detail:"Not spawnable agent type";
        Printf.eprintf "[Auto-Responder] @%s not spawnable\n%!" m;
        None
      end
      else if should_throttle ~agent_type:mention_base then begin
        debug_log "EXIT: Throttled";
        activity_log ~mode ~from_agent ~mention:m ~status:"THROTTLE" ~detail:(Printf.sprintf "Max %d responses per %.0fs" chain_limit chain_window);
        Printf.eprintf "[Auto-Responder] Throttled @%s (chain limit)\n%!" m;
        None
      end
      else begin
        let prompt = build_response_prompt ~from_agent ~content ~mention:m in
        let task_id = Printf.sprintf "auto-respond-%s-%d" mention_base
          (int_of_float (Unix.gettimeofday () *. 1000.) mod 10000) in
        debug_log (Printf.sprintf "DISPATCH: mode=%s task_id=%s" mode_str task_id);
        (* Mode-based dispatch *)
        (match mode with
        | Llm ->
            debug_log "ACTION: LLM call";
            activity_log ~mode ~from_agent ~mention:m ~status:"LLM" ~detail:task_id;
            Printf.eprintf "[Auto-Responder/LLM] Fast-calling %s for @%s\n%!" mention_base m;
            (* LLM mode: run in background process (Thread.create doesn't work well with Eio) *)
            llm_call_in_background ~agent_type:mention_base ~prompt:content ~mention:from_agent ~base_path
        | Spawn ->
            (* glm has no CLI - use LLM mode approach (auto-responder handles MASC) *)
            if mention_base = "glm" then begin
              debug_log "ACTION: LLM call (glm has no CLI)";
              activity_log ~mode ~from_agent ~mention:m ~status:"LLM-GLM" ~detail:task_id;
              Printf.eprintf "[Auto-Responder/LLM-GLM] Fast-calling glm for @%s\n%!" m;
              (* Run in background process *)
              llm_call_in_background ~agent_type:"glm" ~prompt:content ~mention:from_agent ~base_path
            end else begin
              debug_log "ACTION: Spawn";
              activity_log ~mode ~from_agent ~mention:m ~status:"SPAWN" ~detail:task_id;
              Printf.eprintf "[Auto-Responder/Spawn] Spawning %s for @%s from %s\n%!" mention_base m from_agent;
              spawn_in_background ~agent_type:mention_base ~prompt ~working_dir:base_path
            end
        | Disabled ->
            debug_log "ACTION: Mode disabled (should not reach here)");
        Some task_id
      end
