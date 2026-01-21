(** Session Management - Track connected agents and rate limiting *)

open Types

(** Session info stored in registry *)
type session = {
  agent_name: string;
  connected_at: float;     (* Unix timestamp *)
  mutable last_activity: float;
  mutable is_listening: bool;
  mutable message_queue: Yojson.Safe.t list;  (* Pending messages *)
}

(** Rate limit tracking per category

    OCaml 5.4 Atomic Fields: burst_used and last_burst_reset use [@atomic]
    for lock-free concurrent access. This enables safe updates from multiple
    domains without explicit locks, using compare-and-set semantics.

    Access pattern:
    - Read:  Atomic.Loc.get [%atomic.loc tracker.burst_used]
    - Write: Atomic.Loc.set [%atomic.loc tracker.burst_used] value
    - CAS:   Atomic.Loc.compare_and_set [%atomic.loc tracker.field] old new
    - Incr:  Atomic.Loc.fetch_and_add [%atomic.loc tracker.burst_used] 1
*)
type rate_tracker = {
  mutable general_timestamps: float list;
  mutable broadcast_timestamps: float list;
  mutable task_ops_timestamps: float list;
  mutable burst_used: int [@atomic];  (* Atomic: concurrent burst tracking *)
  mutable last_burst_reset: float [@atomic];  (* Atomic: timestamp for reset *)
}

(** Session registry - manages all connected agents *)
type registry = {
  mutable sessions: (string, session) Hashtbl.t;
  mutable rate_trackers: (string, rate_tracker) Hashtbl.t;
  config: rate_limit_config;
  lock: Eio.Mutex.t;
}

(** Create new registry *)
let create ?(config = default_rate_limit) () = {
  sessions = Hashtbl.create 16;
  rate_trackers = Hashtbl.create 16;
  config;
  lock = Eio.Mutex.create ();
}

(** Run a critical section with mutex protection. *)
let with_lock registry f =
  Eio.Mutex.lock registry.lock;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock registry.lock) f

(** Register a new session *)
let register registry ~agent_name =
  with_lock registry (fun () ->
    let now = Unix.gettimeofday () in
    let session = {
      agent_name;
      connected_at = now;
      last_activity = now;
      is_listening = false;
      message_queue = [];
    } in
    Hashtbl.replace registry.sessions agent_name session;
    Log.Session.info "Session registered: %s (total: %d)"
      agent_name (Hashtbl.length registry.sessions);
    session
  )

(** Unregister session *)
let unregister registry ~agent_name =
  with_lock registry (fun () ->
    Hashtbl.remove registry.sessions agent_name;
    Log.Session.info "Session unregistered: %s (total: %d)"
      agent_name (Hashtbl.length registry.sessions)
  )

(** Update activity timestamp *)
let update_activity registry ~agent_name ?(is_listening = None) () =
  with_lock registry (fun () ->
    match Hashtbl.find_opt registry.sessions agent_name with
    | Some session ->
        session.last_activity <- Unix.gettimeofday ();
        (match is_listening with
         | Some v -> session.is_listening <- v
         | None -> ())
    | None -> ()
  )

(** Create empty rate tracker *)
let create_tracker () = {
  general_timestamps = [];
  broadcast_timestamps = [];
  task_ops_timestamps = [];
  burst_used = 0;  (* Initial value for atomic field *)
  last_burst_reset = Unix.gettimeofday ();  (* Initial value for atomic field *)
}

(** Atomic helpers for rate tracker - OCaml 5.4+ *)
let get_burst_used tracker =
  Atomic.Loc.get [%atomic.loc tracker.burst_used]

let set_burst_used tracker v =
  Atomic.Loc.set [%atomic.loc tracker.burst_used] v

let incr_burst_used tracker =
  ignore (Atomic.Loc.fetch_and_add [%atomic.loc tracker.burst_used] 1)

let get_last_burst_reset tracker =
  Atomic.Loc.get [%atomic.loc tracker.last_burst_reset]

let set_last_burst_reset tracker v =
  Atomic.Loc.set [%atomic.loc tracker.last_burst_reset] v

(** Get timestamps for category *)
let get_timestamps tracker = function
  | GeneralLimit -> tracker.general_timestamps
  | BroadcastLimit -> tracker.broadcast_timestamps
  | TaskOpsLimit -> tracker.task_ops_timestamps

(** Set timestamps for category *)
let set_timestamps tracker category ts =
  match category with
  | GeneralLimit -> tracker.general_timestamps <- ts
  | BroadcastLimit -> tracker.broadcast_timestamps <- ts
  | TaskOpsLimit -> tracker.task_ops_timestamps <- ts

(** Enhanced rate limit check with category and role *)
let check_rate_limit_ex registry ~agent_name ~category ~role =
  with_lock registry (fun () ->
    let now = Unix.gettimeofday () in
    let one_minute_ago = now -. 60.0 in

    (* Get or create tracker *)
    let tracker =
      match Hashtbl.find_opt registry.rate_trackers agent_name with
      | Some t -> t
      | None ->
          let t = create_tracker () in
          Hashtbl.replace registry.rate_trackers agent_name t;
          t
    in

    (* Reset burst if a minute has passed - atomic compare-and-set *)
    let last_reset = get_last_burst_reset tracker in
    if now -. last_reset > 60.0 then begin
      set_burst_used tracker 0;
      set_last_burst_reset tracker now
    end;

    (* Filter to last minute only *)
    let timestamps = get_timestamps tracker category in
    let recent = List.filter (fun t -> t > one_minute_ago) timestamps in
    set_timestamps tracker category recent;

    (* Compute effective limit based on role *)
    let base_limit = effective_limit registry.config ~role ~category in
    let limit =
      if List.mem agent_name registry.config.priority_agents
      then int_of_float (float_of_int base_limit *. 1.5)
      else base_limit
    in

    let current = List.length recent in

    if current >= limit then begin
      (* Check if burst is available - using atomic read/increment *)
      let burst = get_burst_used tracker in
      if burst < registry.config.burst_allowed then begin
        incr_burst_used tracker;  (* Atomic increment *)
        set_timestamps tracker category (now :: recent);
        (true, 0)  (* Burst allowed *)
      end else begin
        let oldest = List.fold_left min now recent in
        let wait = int_of_float (oldest +. 60.0 -. now) in
        (false, max 1 wait)
      end
    end else begin
      set_timestamps tracker category (now :: recent);
      (true, 0)
    end
  )

(** Check rate limit - simple wrapper using defaults *)
let check_rate_limit registry ~agent_name =
  check_rate_limit_ex registry ~agent_name ~category:GeneralLimit ~role:Worker

(** Get rate limit status for an agent *)
let get_rate_limit_status registry ~agent_name ~role =
  with_lock registry (fun () ->
    let now = Unix.gettimeofday () in
    let one_minute_ago = now -. 60.0 in

    let tracker =
      match Hashtbl.find_opt registry.rate_trackers agent_name with
      | Some t -> t
      | None -> create_tracker ()
    in

    let status_for_category category =
      let timestamps = get_timestamps tracker category in
      let recent = List.filter (fun t -> t > one_minute_ago) timestamps in
      let limit = effective_limit registry.config ~role ~category in
      let current = List.length recent in
      `Assoc [
        ("category", `String (show_rate_limit_category category));
        ("current", `Int current);
        ("limit", `Int limit);
        ("remaining", `Int (max 0 (limit - current)));
      ]
    in

    let burst = get_burst_used tracker in  (* Atomic read *)
    `Assoc [
      ("agent", `String agent_name);
      ("role", `String (agent_role_to_string role));
      ("burst_remaining", `Int (registry.config.burst_allowed - burst));
      ("categories", `List [
        status_for_category GeneralLimit;
        status_for_category BroadcastLimit;
        status_for_category TaskOpsLimit;
      ]);
    ]
  )

(** Push message to session queue *)
let push_message registry ~from_agent ~content ~mention =
  with_lock registry (fun () ->
    let notification = `Assoc [
      ("type", `String "masc/message");
      ("from", `String from_agent);
      ("content", `String content);
      ("mention", match mention with Some m -> `String m | None -> `Null);
      ("timestamp", `String (now_iso ()));
    ] in

    let targets = ref [] in
    Hashtbl.iter (fun name session ->
      (* Don't send to self *)
      if name <> from_agent then begin
        (* Check mention filter *)
        let should_send = match mention with
          | None -> true  (* Broadcast *)
          | Some m -> m = name  (* Direct mention *)
        in
        if should_send then begin
          session.message_queue <- session.message_queue @ [notification];
          targets := name :: !targets
        end
      end
    ) registry.sessions;

    if !targets <> [] then
      Log.Session.debug "Pushed to: %s" (String.concat ", " !targets);

    !targets
  )

(** Pop message from queue (for listen) *)
let pop_message registry ~agent_name =
  with_lock registry (fun () ->
    match Hashtbl.find_opt registry.sessions agent_name with
    | Some session ->
        (match session.message_queue with
         | msg :: rest ->
             session.message_queue <- rest;
             Some msg
         | [] -> None)
    | None -> None
  )

(** Wait for message (blocking with timeout) *)
let wait_for_message registry ~agent_name ~timeout =
  let start_time = Unix.gettimeofday () in
  let check_interval = 2.0 in

  (* Ensure session exists *)
  (match Hashtbl.find_opt registry.sessions agent_name with
   | Some _ -> ()
   | None -> ignore (register registry ~agent_name));

  update_activity registry ~agent_name ~is_listening:(Some true) ();

  let rec wait_loop () =
    let elapsed = Unix.gettimeofday () -. start_time in
    if elapsed >= timeout then
      None
    else begin
      match pop_message registry ~agent_name with
      | Some msg ->
          Some msg
      | None ->
          Unix.sleepf check_interval;
          wait_loop ()
    end
  in

  let result =
    try wait_loop () with _ -> None
  in
  update_activity registry ~agent_name ~is_listening:(Some false) ();
  result

(** Get inactive agents (idle > threshold seconds) *)
let get_inactive_agents registry ~threshold =
  let now = Unix.gettimeofday () in
  Hashtbl.fold (fun name session acc ->
    if now -. session.last_activity > threshold then
      name :: acc
    else
      acc
  ) registry.sessions []

(** Get all agent statuses *)
let get_agent_statuses registry =
  let now = Unix.gettimeofday () in
  Hashtbl.fold (fun name session acc ->
    let idle_secs = int_of_float (now -. session.last_activity) in
    let status_icon =
      if session.is_listening then "🎧"
      else if idle_secs > 60 then "💤"
      else "🔨"
    in
    let status = `Assoc [
      ("name", `String name);
      ("listening", `Bool session.is_listening);
      ("idle_seconds", `Int idle_secs);
      ("status", `String status_icon);
    ] in
    status :: acc
  ) registry.sessions []

(** Format status for display *)
let status_string registry =
  let statuses = get_agent_statuses registry in
  if statuses = [] then
    "📡 No agents connected."
  else begin
    let buf = Buffer.create 256 in
    Buffer.add_string buf (Printf.sprintf "📡 Connected agents (%d):\n" (List.length statuses));
    Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";

    List.iter (fun status ->
      let open Yojson.Safe.Util in
      let name = status |> member "name" |> to_string in
      let icon = status |> member "status" |> to_string in
      let idle = status |> member "idle_seconds" |> to_int in
      let listening = status |> member "listening" |> to_bool in
      let idle_info = if idle > 30 then Printf.sprintf "(idle %ds)" idle else "" in
      let listen_info = if listening then "리스닝중" else "" in
      Buffer.add_string buf (Printf.sprintf "  %s %s %s %s\n" icon name listen_info idle_info)
    ) statuses;

    Buffer.add_string buf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
    Buffer.add_string buf "🎧=리스닝 🔨=작업중 💤=졸고있음(60s+)";

    (* Check inactive agents *)
    let inactive = get_inactive_agents registry ~threshold:Resilience.default_warning_threshold in
    if inactive <> [] then begin
      Buffer.add_string buf "\n\n⚠️ **INACTIVE AGENTS**: ";
      Buffer.add_string buf (String.concat ", " inactive);
      Buffer.add_string buf "\n   @mention으로 깨워주세요!"
    end;

    Buffer.contents buf
  end

(** Connected agent names *)
let connected_agents registry =
  Hashtbl.fold (fun name _ acc -> name :: acc) registry.sessions []

(** Restore sessions from disk (call on server startup) *)
let restore_from_disk registry ~agents_path =
  if Sys.file_exists agents_path && Sys.is_directory agents_path then begin
    let now = Unix.gettimeofday () in
    let restored = ref 0 in
    Sys.readdir agents_path |> Array.iter (fun name ->
      if Filename.check_suffix name ".json" then begin
        let agent_name = Filename.chop_suffix name ".json" in
        let session = {
          agent_name;
          connected_at = now;  (* Treat as just connected *)
          last_activity = now;
          is_listening = false;
          message_queue = [];
        } in
        Hashtbl.replace registry.sessions agent_name session;
        incr restored
      end
    );
    if !restored > 0 then
      Log.Session.info "Restored %d session(s) from disk" !restored
  end

(* ============================================ *)
(* MCP 2025-11-25 Spec: X-MCP-Session-ID        *)
(* ============================================ *)

(** MCP Session ID store - separate from agent sessions *)
module McpSessionStore = struct
  type mcp_session = {
    id: string;
    created_at: float;
    mutable last_activity: float;
    mutable agent_name: string option;
    mutable metadata: (string * string) list;
    mutable request_count: int;
  }

  let sessions : (string, mcp_session) Hashtbl.t = Hashtbl.create 64
  let max_age = ref 3600.0  (* 1 hour default *)

  (** Generate MCP session ID *)
  let generate_id () : string =
    let bytes = Mirage_crypto_rng.generate 16 in
    let buf = Buffer.create 32 in
    for i = 0 to String.length bytes - 1 do
      Buffer.add_string buf (Printf.sprintf "%02x" (Char.code (String.get bytes i)))
    done;
    Printf.sprintf "mcp_%s" (Buffer.contents buf)

  (** Create new MCP session *)
  let create ?agent_name () : mcp_session =
    let now = Unix.gettimeofday () in
    let session = {
      id = generate_id ();
      created_at = now;
      last_activity = now;
      agent_name;
      metadata = [];
      request_count = 0;
    } in
    Hashtbl.add sessions session.id session;
    session

  (** Get MCP session by ID *)
  let get (session_id : string) : mcp_session option =
    match Hashtbl.find_opt sessions session_id with
    | None -> None
    | Some session ->
      session.last_activity <- Unix.gettimeofday ();
      session.request_count <- session.request_count + 1;
      Some session

  (** Cleanup stale MCP sessions *)
  let cleanup_stale () : int =
    let now = Unix.gettimeofday () in
    let stale = Hashtbl.fold (fun id session acc ->
      if now -. session.last_activity > !max_age then id :: acc else acc
    ) sessions [] in
    List.iter (Hashtbl.remove sessions) stale;
    List.length stale

  (** Convert MCP session to JSON *)
  let to_json (s : mcp_session) : Yojson.Safe.t =
    `Assoc [
      ("id", `String s.id);
      ("created_at", `Float s.created_at);
      ("last_activity", `Float s.last_activity);
      ("agent_name", match s.agent_name with None -> `Null | Some n -> `String n);
      ("request_count", `Int s.request_count);
      ("metadata", `Assoc (List.map (fun (k, v) -> (k, `String v)) s.metadata));
    ]

  (** List all MCP sessions *)
  let list_all () : mcp_session list =
    Hashtbl.fold (fun _ s acc -> s :: acc) sessions []

  (** Remove session *)
  let remove (id : string) : bool =
    if Hashtbl.mem sessions id then begin
      Hashtbl.remove sessions id;
      true
    end else false
end

(** Extract MCP Session ID from HTTP headers (supports both naming conventions) *)
let extract_mcp_session_id (headers : Cohttp.Header.t) : string option =
  match Cohttp.Header.get headers "Mcp-Session-Id" with
  | Some _ as result -> result
  | None -> Cohttp.Header.get headers "X-MCP-Session-ID"

(** Get or create MCP session from headers *)
let get_or_create_mcp_session (headers : Cohttp.Header.t) : McpSessionStore.mcp_session =
  match extract_mcp_session_id headers with
  | Some id ->
    (match McpSessionStore.get id with
     | Some session -> session
     | None -> McpSessionStore.create ())
  | None ->
    McpSessionStore.create ()

(** Add MCP session ID to response headers *)
let add_mcp_session_header (headers : Cohttp.Header.t) (session : McpSessionStore.mcp_session) : Cohttp.Header.t =
  Cohttp.Header.add headers "Mcp-Session-Id" session.id

(** MCP tool handler for session management *)
let handle_mcp_session_tool (arguments : Yojson.Safe.t) : (bool * string) =
  let get_string key =
    match Yojson.Safe.Util.member key arguments with
    | `String s -> Some s
    | _ -> None
  in
  match get_string "action" with
  | Some "get" ->
    (match get_string "session_id" with
     | Some id ->
       (match McpSessionStore.get id with
        | Some s -> (true, Yojson.Safe.pretty_to_string (McpSessionStore.to_json s))
        | None -> (false, Printf.sprintf "MCP session '%s' not found" id))
     | None -> (false, "session_id required"))
  | Some "create" ->
    let agent_name = get_string "agent_name" in
    let s = McpSessionStore.create ?agent_name () in
    (true, Yojson.Safe.pretty_to_string (McpSessionStore.to_json s))
  | Some "list" ->
    let sessions = McpSessionStore.list_all () in
    let json = `Assoc [
      ("count", `Int (List.length sessions));
      ("sessions", `List (List.map McpSessionStore.to_json sessions));
    ] in
    (true, Yojson.Safe.pretty_to_string json)
  | Some "cleanup" ->
    let removed = McpSessionStore.cleanup_stale () in
    (true, Printf.sprintf "Removed %d stale MCP sessions" removed)
  | Some "remove" ->
    (match get_string "session_id" with
     | Some id ->
       if McpSessionStore.remove id then (true, "Session removed")
       else (false, "Session not found")
     | None -> (false, "session_id required"))
  | Some other -> (false, Printf.sprintf "Unknown action: %s" other)
  | None -> (false, "action required: get, create, list, cleanup, remove")
