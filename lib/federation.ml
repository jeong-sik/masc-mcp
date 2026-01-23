(** Federation Module - Level 3 Cross-Organization Collaboration

    Implements the A2A-inspired federation protocol for:
    - Organization discovery (local and remote)
    - Secure handshake with challenge-response
    - Task delegation with trust management
    - Shared state coordination

    @see HOLONIC-ARCHITECTURE.md Level 3: 단체 (Corps / Federation)
    @see https://arxiv.org/html/2501.06322v1 (Google A2A Protocol)

    Quality Improvements (2026-01-10):
    - Thread-safe with Mutex
    - Result-based error handling for I/O
    - Input validation for all IDs
    - Path traversal prevention
*)

open Types

(** Re-export config type from Room_utils *)
type config = Room_utils.config

(* ============================================ *)
(* Federation Types (synced with main branch)   *)
(* ============================================ *)

(** Trust level for federation members *)
type trust_level =
  | Trusted
  | Verified
  | Pending
  | Untrusted
[@@deriving yojson, show, eq]

(** Member status *)
type member_status =
  | Active
  | Inactive
  | Suspended
[@@deriving yojson, show, eq]

(** Organization in the federation *)
type organization = {
  id: string;
  name: string;
  endpoint: string option;
  public_key: string option;
  trust_level: trust_level;
  joined_at: string option;
  rooms: string list;
} [@@deriving yojson, show, eq]

(** Federation member *)
type federation_member = {
  member_id: string;
  organization: organization;
  capabilities: string list;
  active: bool;
  trust_level: trust_level;
  status: member_status;
  joined_at: string;
} [@@deriving yojson, show, eq]

(** Shared state entry *)
type shared_state_entry = {
  key: string;
  value: string;
  version: int;
  updated_by: string;
  updated_at: string;
} [@@deriving yojson, show, eq]

(** Federation configuration *)
type federation_config = {
  id: string;
  name: string;
  local_org: organization;
  members: federation_member list;
  shared_state: shared_state_entry list;
  created_at: string;
  protocol_version: string;
} [@@deriving yojson, show, eq]

(** Handshake challenge for joining federation *)
type handshake_challenge = {
  challenge_id: string;
  from_org: organization;
  nonce: string;
  created_at: string;
  expires_at: string;
} [@@deriving yojson, show, eq]

(** Handshake response *)
type handshake_response = {
  challenge_id: string;
  nonce: string;
  signature: string;
  responder_org: organization;
} [@@deriving yojson, show, eq]

(** Delegation request *)
type delegation_request = {
  id: string;
  from_org: string;
  to_org: string;
  task: task;  (* Types.task *)
  priority: int;
  timeout_seconds: int option;
  created_at: string;
  status: string;
  result: string option;
} [@@deriving show]

(** Manual JSON conversion for delegation_request (task has custom serialization) *)
let delegation_request_to_yojson r =
  `Assoc [
    ("id", `String r.id);
    ("from_org", `String r.from_org);
    ("to_org", `String r.to_org);
    ("task", task_to_yojson r.task);
    ("priority", `Int r.priority);
    ("timeout_seconds", match r.timeout_seconds with Some t -> `Int t | None -> `Null);
    ("created_at", `String r.created_at);
    ("status", `String r.status);
    ("result", match r.result with Some s -> `String s | None -> `Null);
  ]

let delegation_request_of_yojson json =
  let open Yojson.Safe.Util in
  try
    Ok {
      id = json |> member "id" |> to_string;
      from_org = json |> member "from_org" |> to_string;
      to_org = json |> member "to_org" |> to_string;
      task = (match json |> member "task" |> task_of_yojson with Ok t -> t | Error e -> failwith e);
      priority = json |> member "priority" |> to_int;
      timeout_seconds = json |> member "timeout_seconds" |> to_int_option;
      created_at = json |> member "created_at" |> to_string;
      status = json |> member "status" |> to_string;
      result = json |> member "result" |> to_string_option;
    }
  with e -> Error (Printexc.to_string e)

(** Federation event - variant type with inline records *)
type federation_event =
  | HandshakeSuccess of { org_id: string; timestamp: string }
  | OrgJoined of { org_id: string; timestamp: string }
  | OrgLeft of { org_id: string; reason: string; timestamp: string }
  | TaskDelegated of { task_id: string; from_org: string; to_org: string; task: string; timestamp: string }
  | TaskCompleted of { task_id: string; result: string; timestamp: string }
  | TrustUpdated of { org_id: string; old_level: trust_level; new_level: trust_level; timestamp: string }
  | ConfigUpdated of { timestamp: string }
[@@deriving yojson, show, eq]

(** Trust too low error details *)
type trust_too_low_error = {
  org_id: string;
  required: trust_level;
  actual: trust_level;
} [@@deriving yojson, show, eq]

(** Delegation failed error details *)
type delegation_failed_error = {
  task_id: string;
  reason: string;
} [@@deriving yojson, show, eq]

(** Federation error *)
type federation_error =
  | InvalidChallenge
  | ExpiredChallenge
  | InvalidSignature
  | OrgNotFound of string
  | ConfigError of string
  | HandshakeError of string
  | TrustTooLow of trust_too_low_error
  | DelegationFailed of delegation_failed_error
  | FederationNotInitialized
[@@deriving yojson, show, eq]

(** Default trust threshold for delegation *)
let default_trust_threshold : trust_level = Verified

(** Helper: Create local organization *)
let make_local_org ~id ~name ?(capabilities = []) () : organization = {
  id;
  name;
  endpoint = None;
  public_key = None;
  trust_level = Trusted;  (* Local org is fully trusted *)
  joined_at = None;
  rooms = capabilities;  (* Use capabilities as rooms for local org *)
}

(** Helper: Create federation member from organization *)
let make_federation_member ~(org : organization) ~now : federation_member = {
  member_id = org.id;
  organization = org;
  capabilities = org.rooms;
  active = true;
  trust_level = org.trust_level;
  status = Active;
  joined_at = now;
}

(* ============================================ *)
(* Thread Safety: Mutex for Global State       *)
(* ============================================ *)

(** Mutex for thread-safe state access *)
let state_mutex = Mutex.create ()

(** Execute function with mutex held *)
let with_lock f =
  Mutex.lock state_mutex;
  match f () with
  | result -> Mutex.unlock state_mutex; result
  | exception e -> Mutex.unlock state_mutex; raise e

(* ============================================ *)
(* Input Validation                            *)
(* ============================================ *)

(** Validate organization/task ID format *)
let validate_id (id : string) (field_name : string) : (string, string) result =
  if String.length id = 0 then
    Error (Printf.sprintf "%s cannot be empty" field_name)
  else if String.length id > 256 then
    Error (Printf.sprintf "%s too long (max 256 chars)" field_name)
  else if String.contains id '/' || String.contains id '\\' then
    Error (Printf.sprintf "%s cannot contain path separators" field_name)
  else if String.contains id '\000' then
    Error (Printf.sprintf "%s cannot contain null bytes" field_name)
  else
    Ok id

(** Validate endpoint URL *)
let validate_endpoint (url : string) : (string, string) result =
  if String.length url = 0 then
    Error "Endpoint cannot be empty"
  else if not (String.sub url 0 (min 8 (String.length url)) = "https://" ||
               String.sub url 0 (min 7 (String.length url)) = "http://") then
    Error "Endpoint must be a valid URL (http:// or https://)"
  else
    Ok url

(* ============================================ *)
(* Safe File I/O with Result Types             *)
(* ============================================ *)

(** Safe file write with proper error handling *)
let safe_write_file (path : string) (content : string) : (unit, string) result =
  try
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then
      Unix.mkdir dir 0o700;
    let tmp_path = path ^ ".tmp" in
    let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o600 tmp_path in
    output_string oc content;
    close_out oc;
    (* Atomic rename for consistency *)
    Sys.rename tmp_path path;
    Ok ()
  with
  | Sys_error msg -> Error (Printf.sprintf "File write error: %s" msg)
  | Unix.Unix_error (err, _, _) -> Error (Printf.sprintf "Unix error: %s" (Unix.error_message err))

(** Safe file read with proper error handling *)
let safe_read_file (path : string) : (string, string) result =
  try
    if not (Sys.file_exists path) then
      Error (Printf.sprintf "File not found: %s" path)
    else
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Ok content
  with
  | Sys_error msg -> Error (Printf.sprintf "File read error: %s" msg)

(** Safe file append with proper error handling *)
let safe_append_file (path : string) (content : string) : (unit, string) result =
  try
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then
      Unix.mkdir dir 0o700;
    let oc = open_out_gen [Open_append; Open_creat] 0o600 path in
    output_string oc content;
    close_out oc;
    Ok ()
  with
  | Sys_error msg -> Error (Printf.sprintf "File append error: %s" msg)
  | Unix.Unix_error (err, _, _) -> Error (Printf.sprintf "Unix error: %s" (Unix.error_message err))

(* ============================================ *)
(* Path Safety                                 *)
(* ============================================ *)

(** Validate path is within base directory (prevent path traversal) *)
let validate_path (base_path : string) (target_path : string) : (string, string) result =
  let normalized_base =
    try Unix.realpath base_path
    with Unix.Unix_error _ -> base_path
  in
  let normalized_target =
    try Unix.realpath (Filename.dirname target_path) ^ "/" ^ Filename.basename target_path
    with Unix.Unix_error _ -> target_path
  in
  if String.length normalized_target >= String.length normalized_base &&
     String.sub normalized_target 0 (String.length normalized_base) = normalized_base then
    Ok target_path
  else
    Error "Path traversal detected: target is outside base directory"

(** Federation state - stored in .masc/federation/ *)
type federation_state = {
  mutable fed_config: federation_config option;
  mutable pending_handshakes: handshake_challenge list;
  mutable pending_delegations: delegation_request list;
  mutable event_log: federation_event list;
}

(** Global federation state *)
let state : federation_state = {
  fed_config = None;
  pending_handshakes = [];
  pending_delegations = [];
  event_log = [];
}

(** Federation directory path - with path validation *)
let federation_dir (config : config) : (string, string) result =
  let dir = Filename.concat config.base_path "federation" in
  validate_path config.base_path dir

(** Federation config file path - with path validation *)
let config_file (config : config) : (string, string) result =
  match federation_dir config with
  | Error e -> Error e
  | Ok dir ->
    let path = Filename.concat dir "federation.json" in
    validate_path config.base_path path

(** Events log file path - with path validation *)
let events_file (config : config) : (string, string) result =
  match federation_dir config with
  | Error e -> Error e
  | Ok dir ->
    let path = Filename.concat dir "events.jsonl" in
    validate_path config.base_path path

(** Members file path - with path validation *)
let members_file (config : config) : (string, string) result =
  match federation_dir config with
  | Error e -> Error e
  | Ok dir ->
    let path = Filename.concat dir "members.json" in
    validate_path config.base_path path

(** Get current ISO8601 timestamp *)
let now_iso8601 () : string =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(** Generate random nonce for handshake *)
let generate_nonce () : string =
  let bytes = Mirage_crypto_rng.generate 32 in
  let buf = Buffer.create 64 in
  for i = 0 to String.length bytes - 1 do
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code (String.get bytes i)))
  done;
  Buffer.contents buf

(** Generate unique ID *)
let generate_id () : string =
  let bytes = Mirage_crypto_rng.generate 16 in
  let buf = Buffer.create 32 in
  for i = 0 to String.length bytes - 1 do
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code (String.get bytes i)))
  done;
  let hex = Buffer.contents buf in
  Printf.sprintf "%s-%s-%s-%s-%s"
    (String.sub hex 0 8)
    (String.sub hex 8 4)
    (String.sub hex 12 4)
    (String.sub hex 16 4)
    (String.sub hex 20 12)

(** Ensure federation directory exists - returns Result *)
let ensure_federation_dir (config : config) : (unit, string) result =
  match federation_dir config with
  | Error e -> Error e
  | Ok dir ->
    try
      if not (Sys.file_exists dir) then
        Unix.mkdir dir 0o700;
      Ok ()
    with
    | Unix.Unix_error (err, _, _) -> Error (Printf.sprintf "Failed to create directory: %s" (Unix.error_message err))

(** Log a federation event - best effort, ignores errors *)
let log_event (config : config) (event : federation_event) : unit =
  match ensure_federation_dir config with
  | Error _ -> ()  (* Silently ignore directory creation failures *)
  | Ok () ->
    state.event_log <- event :: state.event_log;
    match events_file config with
    | Error _ -> ()  (* Silently ignore path errors *)
    | Ok path ->
      ignore (safe_append_file path (Yojson.Safe.to_string (federation_event_to_yojson event) ^ "\n"))

(** Helper: Save config to disk - thread-safe *)
let save_config (config : config) (fed_config : federation_config) : (unit, string) result =
  match config_file config with
  | Error e -> Error e
  | Ok path ->
    let json = federation_config_to_yojson fed_config in
    safe_write_file path (Yojson.Safe.pretty_to_string json)

(** Initialize federation with local organization

    @param config Room configuration
    @param org_id Organization identifier
    @param org_name Human-readable organization name
    @return Initialized federation configuration or error
*)
let initialize (config : config) ~org_id ~org_name : (federation_config, string) result =
  (* Validate inputs *)
  match validate_id org_id "org_id" with
  | Error e -> Error e
  | Ok validated_org_id ->
    match validate_id org_name "org_name" with
    | Error e -> Error e
    | Ok validated_org_name ->
      (* Ensure directory exists *)
      match ensure_federation_dir config with
      | Error e -> Error e
      | Ok () ->
        let now = now_iso8601 () in
        let local_org = make_local_org ~id:validated_org_id ~name:validated_org_name () in
        let fed_config : federation_config = {
          id = generate_id ();
          name = validated_org_name ^ " Federation";
          local_org;
          members = [];
          shared_state = [];
          created_at = now;
          protocol_version = "1.0";
        } in
        (* Thread-safe state update *)
        with_lock (fun () ->
          state.fed_config <- Some fed_config
        );
        (* Save to disk *)
        match save_config config fed_config with
        | Error e -> Error e
        | Ok () -> Ok fed_config

(** Load federation config from disk *)
let load (config : config) : (federation_config option, string) result =
  match config_file config with
  | Error e -> Error e
  | Ok path ->
    match safe_read_file path with
    | Error _ -> Ok None  (* File not found is OK *)
    | Ok content ->
      match Yojson.Safe.from_string content |> federation_config_of_yojson with
      | Ok fed_config ->
        with_lock (fun () ->
          state.fed_config <- Some fed_config
        );
        Ok (Some fed_config)
      | Error msg -> Error (Printf.sprintf "Failed to parse config: %s" msg)

(** Get current federation config *)
let get_config () : federation_config option =
  state.fed_config

(** Create handshake challenge for incoming organization

    @param from_org Organization requesting to join
    @return Handshake challenge to send back
*)
let create_challenge (from_org : organization) : handshake_challenge =
  let now = Unix.gettimeofday () in
  let timeout = 3600.0 in  (* 1 hour timeout *)
  let challenge : handshake_challenge = {
    challenge_id = generate_id ();
    from_org;
    nonce = generate_nonce ();
    created_at = string_of_float now;
    expires_at = string_of_float (now +. timeout);
  } in
  (* Thread-safe state update *)
  with_lock (fun () ->
    state.pending_handshakes <- challenge :: state.pending_handshakes
  );
  challenge

(** Verify handshake response

    In production, this would:
    1. Check signature against public key
    2. Verify nonce matches challenge
    3. Check expiry

    For now, accept all responses (trust-on-first-use)
*)
let verify_response (config : config) (response : handshake_response) : (federation_member, federation_error) result =
  (* Thread-safe challenge lookup and removal *)
  let challenge_opt = with_lock (fun () ->
    match List.find_opt (fun (c : handshake_challenge) -> c.challenge_id = response.challenge_id) state.pending_handshakes with
    | None -> None
    | Some challenge ->
      state.pending_handshakes <- List.filter (fun (c : handshake_challenge) -> c.challenge_id <> response.challenge_id) state.pending_handshakes;
      Some challenge
  ) in
  match challenge_opt with
  | None -> Error (HandshakeError "Challenge not found or expired")
  | Some challenge ->
    (* Create member *)
    let now = now_iso8601 () in
    let member = make_federation_member ~org:challenge.from_org ~now in
    let member = { member with status = Active } in
    (* Log event *)
    log_event config (HandshakeSuccess { org_id = challenge.from_org.id; timestamp = now });
    Ok member

(** Add organization to federation

    @param config Room configuration
    @param org Organization to add
    @return Updated member or error
*)
let add_member (config : config) (org : organization) : (federation_member, federation_error) result =
  (* Validate org_id *)
  match validate_id org.id "org_id" with
  | Error e -> Error (HandshakeError e)
  | Ok _ ->
    (* Thread-safe state update *)
    let result = with_lock (fun () ->
      match state.fed_config with
      | None -> Error FederationNotInitialized
      | Some fed_config ->
        if List.exists (fun m -> m.organization.id = org.id) fed_config.members then
          Error (HandshakeError "Organization already a member")
        else begin
          let now = now_iso8601 () in
          let member = make_federation_member ~org ~now in
          let member = { member with status = Active } in
          let updated = { fed_config with members = member :: fed_config.members } in
          state.fed_config <- Some updated;
          Ok (updated, member, now)
        end
    ) in
    match result with
    | Error e -> Error e
    | Ok (updated, member, now) ->
      (* Save outside lock *)
      begin match save_config config updated with
      | Error e -> Error (HandshakeError e)
      | Ok () ->
        log_event config (OrgJoined { org_id = org.id; timestamp = now });
        Ok member
      end

(** Remove organization from federation *)
let remove_member (config : config) ~org_id ~reason : (unit, federation_error) result =
  (* Validate org_id *)
  match validate_id org_id "org_id" with
  | Error e -> Error (HandshakeError e)
  | Ok _ ->
    (* Thread-safe state update *)
    let result = with_lock (fun () ->
      match state.fed_config with
      | None -> Error FederationNotInitialized
      | Some fed_config ->
        if not (List.exists (fun m -> m.organization.id = org_id) fed_config.members) then
          Error (OrgNotFound org_id)
        else begin
          let updated = { fed_config with
            members = List.filter (fun m -> m.organization.id <> org_id) fed_config.members
          } in
          state.fed_config <- Some updated;
          Ok updated
        end
    ) in
    match result with
    | Error e -> Error e
    | Ok updated ->
      (* Save outside lock *)
      begin match save_config config updated with
      | Error e -> Error (HandshakeError e)
      | Ok () ->
        let now = now_iso8601 () in
        log_event config (OrgLeft { org_id; reason; timestamp = now });
        Ok ()
      end

(** Find member by organization ID *)
let find_member ~org_id : federation_member option =
  match state.fed_config with
  | None -> None
  | Some fed_config ->
    List.find_opt (fun m -> m.organization.id = org_id) fed_config.members

(** Convert trust level to integer for comparison (higher = more trusted) *)
let trust_level_to_int = function
  | Trusted -> 4
  | Verified -> 3
  | Pending -> 2
  | Untrusted -> 1

(** Check if member meets minimum trust level for delegation *)
let can_delegate_to ~(member : federation_member) ~min_trust : bool =
  member.active &&
  member.status = Active &&
  trust_level_to_int member.trust_level >= trust_level_to_int min_trust

(** Convert integer back to trust level *)
let int_to_trust_level = function
  | n when n >= 4 -> Trusted
  | 3 -> Verified
  | 2 -> Pending
  | _ -> Untrusted

(** Update trust level based on success/failure of delegation *)
let update_trust ~(member : federation_member) ~success : federation_member =
  let current = trust_level_to_int member.trust_level in
  let delta = if success then 1 else -1 in
  let new_level = int_to_trust_level (max 1 (min 4 (current + delta))) in
  { member with trust_level = new_level }

(** Check if delegation is allowed to target organization *)
let can_delegate ~to_org_id ?(min_trust = default_trust_threshold) () : bool =
  match find_member ~org_id:to_org_id with
  | None -> false
  | Some member -> can_delegate_to ~member ~min_trust

(** Create delegation request

    @param config Room configuration
    @param to_org Target organization ID
    @param task Task to delegate
    @param priority 1-5 (1 = highest)
    @param timeout_seconds Maximum time for task completion
    @return Delegation request or error
*)
let create_delegation (config : config) ~to_org ~(task : task) ~priority ~timeout_seconds
    : (delegation_request, federation_error) result =
  (* Validate to_org *)
  match validate_id to_org "to_org" with
  | Error e -> Error (HandshakeError e)
  | Ok _ ->
    (* Thread-safe read and update *)
    let result = with_lock (fun () ->
      match state.fed_config with
      | None -> Error FederationNotInitialized
      | Some fed_config ->
        match find_member ~org_id:to_org with
        | None -> Error (OrgNotFound to_org)
        | Some member ->
          if not (can_delegate_to ~member ~min_trust:default_trust_threshold) then
            Error (TrustTooLow {
              org_id = to_org;
              required = default_trust_threshold;
              actual = member.trust_level
            })
          else begin
            let now = now_iso8601 () in
            let request : delegation_request = {
              id = generate_id ();
              from_org = fed_config.local_org.id;
              to_org;
              task;
              priority;
              timeout_seconds;
              created_at = now;
              status = "pending";
              result = None;
            } in
            state.pending_delegations <- request :: state.pending_delegations;
            Ok (request, now)
          end
    ) in
    match result with
    | Error e -> Error e
    | Ok (request, now) ->
      log_event config (TaskDelegated {
        task_id = request.id;
        from_org = request.from_org;
        to_org = request.to_org;
        task = task.id;
        timestamp = now
      });
      Ok request

(** Update delegation status *)
let update_delegation ~request_id ~status ~result : (delegation_request, federation_error) result =
  (* Thread-safe update *)
  with_lock (fun () ->
    match List.find_opt (fun (r : delegation_request) -> r.id = request_id) state.pending_delegations with
    | None -> Error (DelegationFailed { task_id = request_id; reason = "Delegation not found" })
    | Some request ->
      let updated = { request with status; result } in
      state.pending_delegations <- List.map (fun (r : delegation_request) ->
        if r.id = request_id then updated else r
      ) state.pending_delegations;
      Ok updated
  )

(** Update trust based on delegation outcome *)
let update_member_trust (config : config) ~org_id ~success : (federation_member, federation_error) result =
  (* Validate org_id *)
  match validate_id org_id "org_id" with
  | Error e -> Error (HandshakeError e)
  | Ok _ ->
    (* Thread-safe state update *)
    let result = with_lock (fun () ->
      match state.fed_config with
      | None -> Error FederationNotInitialized
      | Some fed_config ->
        match find_member ~org_id with
        | None -> Error (OrgNotFound org_id)
        | Some member ->
          let old_level = member.trust_level in
          let updated_member = update_trust ~member ~success in
          let updated_config = { fed_config with
            members = List.map (fun m ->
              if m.organization.id = org_id then updated_member else m
            ) fed_config.members
          } in
          state.fed_config <- Some updated_config;
          Ok (updated_config, old_level, updated_member)
    ) in
    match result with
    | Error e -> Error e
    | Ok (updated_config, old_level, updated_member) ->
      (* Save outside lock *)
      begin match save_config config updated_config with
      | Error e -> Error (HandshakeError e)
      | Ok () ->
        let now = now_iso8601 () in
        log_event config (TrustUpdated {
          org_id;
          old_level;
          new_level = updated_member.trust_level;
          timestamp = now
        });
        Ok updated_member
      end

(** Get shared state entry *)
let get_shared_state ~key : shared_state_entry option =
  with_lock (fun () ->
    match state.fed_config with
    | None -> None
    | Some fed_config ->
      List.find_opt (fun e -> e.key = key) fed_config.shared_state
  )

(** Set shared state entry (optimistic concurrency) *)
let set_shared_state (config : config) ~key ~value ~expected_version
    : (shared_state_entry, federation_error) result =
  (* Validate key *)
  match validate_id key "key" with
  | Error e -> Error (HandshakeError e)
  | Ok _ ->
    (* Thread-safe optimistic concurrency *)
    let result = with_lock (fun () ->
      match state.fed_config with
      | None -> Error FederationNotInitialized
      | Some fed_config ->
        let existing = List.find_opt (fun e -> e.key = key) fed_config.shared_state in
        let current_version = match existing with
          | Some e -> e.version
          | None -> 0
        in
        if expected_version <> current_version then
          Error (DelegationFailed {
            task_id = key;
            reason = Printf.sprintf "Version mismatch: expected %d, got %d" expected_version current_version
          })
        else begin
          let now = now_iso8601 () in
          let entry : shared_state_entry = {
            key;
            value;
            version = current_version + 1;
            updated_by = fed_config.local_org.id;
            updated_at = now;
          } in
          let updated = { fed_config with
            shared_state = entry :: List.filter (fun e -> e.key <> key) fed_config.shared_state
          } in
          state.fed_config <- Some updated;
          Ok (updated, entry)
        end
    ) in
    match result with
    | Error e -> Error e
    | Ok (updated, entry) ->
      (* Save outside lock *)
      begin match save_config config updated with
      | Error e -> Error (HandshakeError e)
      | Ok () -> Ok entry
      end

(** List all members with their status *)
let list_members () : federation_member list =
  match state.fed_config with
  | None -> []
  | Some fed_config -> fed_config.members

(** Get federation status summary *)
let status () : Yojson.Safe.t =
  match state.fed_config with
  | None ->
    `Assoc [
      ("initialized", `Bool false);
      ("message", `String "Federation not initialized. Use initialize() first.");
    ]
  | Some fed_config ->
    let active_members = List.filter (fun (m : federation_member) -> m.status = Active) fed_config.members in
    let suspended_members = List.filter (fun (m : federation_member) -> m.status = Suspended) fed_config.members in
    `Assoc [
      ("initialized", `Bool true);
      ("federation_id", `String fed_config.id);
      ("federation_name", `String fed_config.name);
      ("local_org", `Assoc [
        ("id", `String fed_config.local_org.id);
        ("name", `String fed_config.local_org.name);
      ]);
      ("protocol_version", `String fed_config.protocol_version);
      ("created_at", `String fed_config.created_at);
      ("members", `Assoc [
        ("total", `Int (List.length fed_config.members));
        ("active", `Int (List.length active_members));
        ("suspended", `Int (List.length suspended_members));
      ]);
      ("pending_handshakes", `Int (List.length state.pending_handshakes));
      ("pending_delegations", `Int (List.length state.pending_delegations));
      ("event_log_size", `Int (List.length state.event_log));
      ("shared_state_keys", `Int (List.length fed_config.shared_state));
    ]

(** Discover remote organizations (placeholder for async HTTP)

    In production, this would:
    1. Fetch /.well-known/agent-card.json from endpoint
    2. Parse organization info
    3. Initiate handshake

    For now, returns placeholder for MCP clients to handle.
*)
let discover_remote ~endpoint : Yojson.Safe.t =
  `Assoc [
    ("type", `String "remote_discovery");
    ("endpoint", `String endpoint);
    ("well_known_url", `String (endpoint ^ "/.well-known/agent-card.json"));
    ("note", `String "Use HTTP client to fetch agent card from well_known_url");
    ("next_steps", `List [
      `String "1. Fetch agent card from well_known_url";
      `String "2. Parse organization info";
      `String "3. Call create_challenge() with organization";
      `String "4. Send challenge to remote endpoint";
      `String "5. Verify response with verify_response()";
    ]);
  ]

(* ============================================ *)
(* Cross-Room Communication                     *)
(* ============================================ *)

(** List rooms in a federated organization

    Returns room information for the specified organization.
    For local org, reads from disk. For remote orgs, returns
    placeholder for HTTP client to fetch.
*)
let list_org_rooms ~org_id : Yojson.Safe.t =
  match state.fed_config with
  | None ->
    `Assoc [
      ("success", `Bool false);
      ("error", `String "Federation not initialized");
    ]
  | Some fed_config ->
    if fed_config.local_org.id = org_id then
      (* Local org - return actual room info *)
      `Assoc [
        ("success", `Bool true);
        ("org_id", `String org_id);
        ("rooms", `List (List.map (fun r -> `String r) fed_config.local_org.rooms));
        ("source", `String "local");
      ]
    else
      (* Remote org - return placeholder for HTTP client *)
      match find_member ~org_id with
      | None ->
        `Assoc [
          ("success", `Bool false);
          ("error", `String (Printf.sprintf "Organization %s not found in federation" org_id));
        ]
      | Some member ->
        let endpoint = Option.value member.organization.endpoint ~default:"unknown" in
        `Assoc [
          ("success", `Bool true);
          ("org_id", `String org_id);
          ("type", `String "remote_room_list");
          ("endpoint", `String endpoint);
          ("rooms_url", `String (endpoint ^ "/api/rooms"));
          ("note", `String "Use HTTP client to fetch rooms from rooms_url");
        ]

(** Cross-room message type *)
type cross_room_message = {
  id: string;
  from_org: string;
  from_room: string;
  to_org: string;
  to_room: string;
  content: string;
  message_type: string;  (* "request" | "response" | "broadcast" *)
  created_at: string;
  metadata: (string * string) list;
}

(** Send message to a room in another organization

    For remote orgs, returns routing info for HTTP client.
    Trust level is checked before allowing cross-room messaging.
*)
let send_cross_room_message (config : config)
    ~from_room ~to_org ~to_room ~content ~message_type
    : (Yojson.Safe.t, federation_error) result =
  match state.fed_config with
  | None -> Error FederationNotInitialized
  | Some fed_config ->
    match find_member ~org_id:to_org with
    | None -> Error (OrgNotFound to_org)
    | Some member ->
      if not (can_delegate_to ~member ~min_trust:default_trust_threshold) then
        Error (TrustTooLow {
          org_id = to_org;
          required = default_trust_threshold;
          actual = member.trust_level
        })
      else begin
        let now = now_iso8601 () in
        let msg : cross_room_message = {
          id = generate_id ();
          from_org = fed_config.local_org.id;
          from_room;
          to_org;
          to_room;
          content;
          message_type;
          created_at = now;
          metadata = [];
        } in
        (* Log the event *)
        log_event config (TaskDelegated {
          task_id = msg.id;
          from_org = msg.from_org;
          to_org = msg.to_org;
          task = "cross_room_message";
          timestamp = now;
        });
        let endpoint = Option.value member.organization.endpoint ~default:"unknown" in
        Ok (`Assoc [
          ("success", `Bool true);
          ("message_id", `String msg.id);
          ("from_org", `String msg.from_org);
          ("from_room", `String msg.from_room);
          ("to_org", `String msg.to_org);
          ("to_room", `String msg.to_room);
          ("type", `String "cross_room_message");
          ("endpoint", `String endpoint);
          ("delivery_url", `String (endpoint ^ "/api/rooms/" ^ to_room ^ "/messages"));
          ("note", `String "Use HTTP client to POST message to delivery_url");
        ])
      end

(** Subscribe to events from a remote room

    Returns subscription info for SSE client setup.
*)
let subscribe_remote_room ~org_id ~room_id : (Yojson.Safe.t, federation_error) result =
  match state.fed_config with
  | None -> Error FederationNotInitialized
  | Some _fed_config ->
    match find_member ~org_id with
    | None -> Error (OrgNotFound org_id)
    | Some member ->
      if not (can_delegate_to ~member ~min_trust:default_trust_threshold) then
        Error (TrustTooLow {
          org_id;
          required = default_trust_threshold;
          actual = member.trust_level
        })
      else begin
        let endpoint = Option.value member.organization.endpoint ~default:"unknown" in
        Ok (`Assoc [
          ("success", `Bool true);
          ("subscription_type", `String "remote_room_events");
          ("org_id", `String org_id);
          ("room_id", `String room_id);
          ("endpoint", `String endpoint);
          ("sse_url", `String (endpoint ^ "/api/rooms/" ^ room_id ^ "/events"));
          ("note", `String "Connect SSE client to sse_url to receive events");
          ("event_types", `List [
            `String "task_created";
            `String "task_claimed";
            `String "task_completed";
            `String "message_broadcast";
            `String "agent_joined";
            `String "agent_left";
          ]);
        ])
      end
