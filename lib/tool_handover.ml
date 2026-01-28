(** Handover tools - Cellular agent handover DNA *)

(* Argument helpers *)
let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

let get_string_opt args key =
  match Yojson.Safe.Util.member key args with
  | `String s when s <> "" -> Some s
  | _ -> None

let get_int args key default =
  match Yojson.Safe.Util.member key args with
  | `Int n -> n
  | _ -> default

let _get_int_opt args key =
  match Yojson.Safe.Util.member key args with
  | `Int n -> Some n
  | _ -> None

let get_bool args key default =
  match Yojson.Safe.Util.member key args with
  | `Bool b -> b
  | _ -> default

let get_string_list args key =
  match Yojson.Safe.Util.member key args with
  | `List items ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

(* Context required by handover tools - needs Eio filesystem *)
type context = {
  config: Room.config;
  agent_name: string;
  fs: Eio.Fs.dir_ty Eio.Path.t option;
  proc_mgr: Eio_unix.Process.mgr_ty Eio.Resource.t option;
  sw: Eio.Switch.t;
}

type result = bool * string

(* Individual handlers *)
let handle_handover_create ctx args =
  let task_id = get_string args "task_id" "" in
  let session_id = get_string args "session_id" "" in
  let reason_str = get_string args "reason" "explicit" in
  let reason = match reason_str with
    | "context_limit" -> Handover_eio.ContextLimit (get_int args "context_pct" 80)
    | "timeout" -> Handover_eio.Timeout 300
    | "error" -> Handover_eio.FatalError "Unknown error"
    | "complete" -> Handover_eio.TaskComplete
    | _ -> Handover_eio.Explicit
  in
  let h = {
    (Handover_eio.create_handover ~from_agent:ctx.agent_name ~task_id ~session_id ~reason) with
    current_goal = get_string args "goal" "";
    progress_summary = get_string args "progress" "";
    completed_steps = get_string_list args "completed_steps";
    pending_steps = get_string_list args "pending_steps";
    key_decisions = get_string_list args "decisions";
    assumptions = get_string_list args "assumptions";
    warnings = get_string_list args "warnings";
    unresolved_errors = get_string_list args "errors";
    modified_files = get_string_list args "files";
    context_usage_percent = get_int args "context_pct" 0;
  } in
  match ctx.fs with
  | Some fs ->
      (match Handover_eio.save_handover ~fs ctx.config h with
       | Ok () -> (true, Printf.sprintf "✅ Handover DNA created: %s" h.id)
       | Error e -> (false, Printf.sprintf "❌ Failed to save handover: %s" e))
  | None -> (false, "❌ Filesystem not available")

let handle_handover_list ctx args =
  let pending_only = get_bool args "pending_only" false in
  match ctx.fs with
  | Some fs ->
      let handovers =
        if pending_only then Handover_eio.get_pending_handovers ~fs ctx.config
        else Handover_eio.list_handovers ~fs ctx.config
      in
      let json = `List (List.map Handover_eio.handover_to_json handovers) in
      (true, Yojson.Safe.pretty_to_string json)
  | None -> (false, "❌ Filesystem not available")

let handle_handover_claim ctx args =
  let handover_id = get_string args "handover_id" "" in
  match ctx.fs with
  | Some fs ->
      (match Handover_eio.claim_handover ~fs ctx.config ~handover_id ~agent_name:ctx.agent_name with
       | Ok h -> (true, Printf.sprintf "✅ Handover %s claimed by %s" h.id ctx.agent_name)
       | Error e -> (false, Printf.sprintf "❌ Failed to claim handover: %s" e))
  | None -> (false, "❌ Filesystem not available")

let handle_handover_claim_and_spawn ctx args =
  let handover_id = get_string args "handover_id" "" in
  let additional_instructions = get_string_opt args "additional_instructions" in
  let timeout_seconds = _get_int_opt args "timeout_seconds" in
  match ctx.fs, ctx.proc_mgr with
  | Some fs, Some pm ->
      (match Handover_eio.claim_and_spawn ~sw:ctx.sw ~fs ~proc_mgr:pm ctx.config
               ~handover_id ~agent_name:ctx.agent_name ?additional_instructions ?timeout_seconds () with
       | Ok result -> (true, Spawn_eio.result_to_human_string result)
       | Error e -> (false, Printf.sprintf "❌ Failed to claim/spawn: %s" e))
  | None, _ -> (false, "❌ Filesystem not available")
  | _, None -> (false, "❌ Process manager not available in this environment")

let handle_handover_get ctx args =
  let handover_id = get_string args "handover_id" "" in
  match ctx.fs with
  | Some fs ->
      (match Handover_eio.load_handover ~fs ctx.config handover_id with
       | Ok h -> (true, Handover_eio.format_as_markdown h)
       | Error e -> (false, Printf.sprintf "❌ Failed to load handover: %s" e))
  | None -> (false, "❌ Filesystem not available")

(* Dispatch function - returns None if tool not handled *)
let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_handover_create" -> Some (handle_handover_create ctx args)
  | "masc_handover_list" -> Some (handle_handover_list ctx args)
  | "masc_handover_claim" -> Some (handle_handover_claim ctx args)
  | "masc_handover_claim_and_spawn" -> Some (handle_handover_claim_and_spawn ctx args)
  | "masc_handover_get" -> Some (handle_handover_get ctx args)
  | _ -> None
