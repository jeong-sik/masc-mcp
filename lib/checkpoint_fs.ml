(** Filesystem-backed checkpoint store for masc-checkpoint.

    This is a local fallback when Neo4j is unavailable and is also useful for
    durable, resumable workflows without external dependencies.
*)

open Lwt.Syntax

open Checkpoint_types

let () = Random.self_init ()

let schema_version = 1

let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let ends_with ~suffix s =
  let slen = String.length suffix in
  let len = String.length s in
  len >= slen && String.sub s (len - slen) slen = suffix

let list_hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let rec mkdir_p path =
  if path = "" || path = Filename.dirname path && Sys.file_exists path then ()
  else if Sys.file_exists path then ()
  else begin
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let read_file_opt path =
  if Sys.file_exists path then
    try Some (In_channel.with_open_text path In_channel.input_all)
    with _ -> None
  else
    None

let write_file_atomic path content =
  mkdir_p (Filename.dirname path);
  let tmp_path =
    Printf.sprintf "%s.tmp.%d.%06d" path (Unix.getpid ()) (Random.int 1_000_000)
  in
  Out_channel.with_open_text tmp_path (fun oc ->
    Out_channel.output_string oc content;
    Out_channel.flush oc
  );
  Unix.rename tmp_path path

let read_json_opt path =
  match read_file_opt path with
  | None -> None
  | Some content ->
      (try
         if String.trim content = "" then Some (`Assoc [])
         else Some (Yojson.Safe.from_string content)
       with _ -> None)

let write_json path json =
  write_file_atomic path (Yojson.Safe.pretty_to_string json)

let checkpoints_root masc_dir =
  Filename.concat masc_dir "state/checkpoints"

let task_root masc_dir task_id =
  Filename.concat (checkpoints_root masc_dir) task_id

let meta_path masc_dir task_id =
  Filename.concat (task_root masc_dir task_id) "meta.json"

let checkpoint_path masc_dir ~task_id ~checkpoint_id =
  Filename.concat (task_root masc_dir task_id) (checkpoint_id ^ ".json")

type checkpoint_record = {
  id: string;
  task_id: string;
  step: int;
  total_steps: int;
  action: string;
  state: string;
  agent: string;
  session_id: string;
  status: checkpoint_status;
  interrupt_message: string;
  created_at: float;
  resolved_at: float option;
  reject_reason: string option;
  parent_checkpoint_id: string option;
  branch_name: string option;
  state_edited: bool;
}

let json_string_member json key ~default =
  match Yojson.Safe.Util.member key json with
  | `String s -> s
  | _ -> default

let json_int_member json key ~default =
  match Yojson.Safe.Util.member key json with
  | `Int n -> n
  | _ -> default

let json_float_member_opt json key =
  match Yojson.Safe.Util.member key json with
  | `Float f -> Some f
  | `Int n -> Some (float_of_int n)
  | _ -> None

let json_bool_member json key ~default =
  match Yojson.Safe.Util.member key json with
  | `Bool b -> b
  | _ -> default

let json_string_member_opt json key =
  match Yojson.Safe.Util.member key json with
  | `String s when String.trim s <> "" -> Some s
  | _ -> None

let checkpoint_record_of_json json : (checkpoint_record, string) result =
  let task_id = json_string_member json "task_id" ~default:"" in
  let id = json_string_member json "id" ~default:"" in
  let step = json_int_member json "step" ~default:0 in
  let total_steps = json_int_member json "total_steps" ~default:0 in
  let action = json_string_member json "action" ~default:"" in
  let state = json_string_member json "state" ~default:"{}" in
  let agent = json_string_member json "agent" ~default:"" in
  let session_id = json_string_member json "session_id" ~default:"" in
  let status_str = json_string_member json "status" ~default:"" in
  let interrupt_message = json_string_member json "interrupt_message" ~default:"" in
  let created_at = Option.value (json_float_member_opt json "created_at") ~default:0.0 in
  let resolved_at = json_float_member_opt json "resolved_at" in
  let reject_reason = json_string_member_opt json "reject_reason" in
  let parent_checkpoint_id = json_string_member_opt json "parent_checkpoint_id" in
  let branch_name = json_string_member_opt json "branch_name" in
  let state_edited = json_bool_member json "state_edited" ~default:false in
  match status_of_string status_str with
  | None -> Error (Printf.sprintf "Invalid checkpoint status: %s" status_str)
  | Some status ->
      Ok {
        id;
        task_id;
        step;
        total_steps;
        action;
        state;
        agent;
        session_id;
        status;
        interrupt_message;
        created_at;
        resolved_at;
        reject_reason;
        parent_checkpoint_id;
        branch_name;
        state_edited;
      }

let checkpoint_json_of_record (c : checkpoint_record) : Yojson.Safe.t =
  `Assoc [
    ("schema_version", `Int schema_version);
    ("id", `String c.id);
    ("task_id", `String c.task_id);
    ("step", `Int c.step);
    ("total_steps", `Int c.total_steps);
    ("action", `String c.action);
    ("state", `String c.state);
    ("agent", `String c.agent);
    ("session_id", `String c.session_id);
    ("status", `String (status_to_string c.status));
    ("interrupt_message", `String c.interrupt_message);
    ("created_at", `Float c.created_at);
    ("resolved_at", match c.resolved_at with Some f -> `Float f | None -> `Null);
    ("reject_reason", match c.reject_reason with Some s -> `String s | None -> `Null);
    ("parent_checkpoint_id", match c.parent_checkpoint_id with Some s -> `String s | None -> `Null);
    ("branch_name", match c.branch_name with Some s -> `String s | None -> `Null);
    ("state_edited", `Bool c.state_edited);
  ]

let list_checkpoint_files masc_dir task_id =
  let dir = task_root masc_dir task_id in
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun name ->
      starts_with ~prefix:"cp-" name && ends_with ~suffix:".json" name
    )
    |> List.map (fun name -> Filename.concat dir name)
  else
    []

let load_task_checkpoints masc_dir task_id =
  let files = list_checkpoint_files masc_dir task_id in
  let rec loop acc = function
    | [] -> Ok acc
    | path :: rest ->
        (match read_json_opt path with
         | None -> loop acc rest
         | Some json ->
             (match checkpoint_record_of_json json with
              | Ok record -> loop (record :: acc) rest
              | Error _ -> loop acc rest))
  in
  loop [] files

let write_meta masc_dir ~task_id ~checkpoint_id ~step =
  let meta = `Assoc [
    ("schema_version", `Int schema_version);
    ("task_id", `String task_id);
    ("last_checkpoint_id", `String checkpoint_id);
    ("last_step", `Int step);
    ("updated_at", `Float (Unix.gettimeofday ()));
  ] in
  write_json (meta_path masc_dir task_id) meta

let read_meta_last_checkpoint_id masc_dir task_id =
  match read_json_opt (meta_path masc_dir task_id) with
  | Some json ->
      let id = json_string_member json "last_checkpoint_id" ~default:"" in
      if String.trim id = "" then None else Some id
  | None -> None

let checkpoint_to_resume_json (c : checkpoint_record) : Yojson.Safe.t =
  `Assoc [
    ("id", `String c.id);
    ("step", `Int c.step);
    ("action", `String c.action);
    ("state", `String c.state);
    ("agent", `String c.agent);
    ("status", `String (status_to_string c.status));
    ("interrupt_message", `String c.interrupt_message);
  ]

let save_checkpoint
    ~masc_dir
    ~task_id
    ~step
    ~action
    ~agent
    ?(state="{}")
    ?(total_steps=0)
    ?(session_id="")
    ?(status=Completed)
    ?(interrupt_message="")
    ?created_at
    () =
  if not (is_valid_task_id task_id) then
    Lwt.return (Error "Invalid task_id")
  else if not (is_valid_step step) then
    Lwt.return (Error "Invalid step")
  else if not (is_valid_json_state state) then
    Lwt.return (Error "Invalid state: must be valid JSON")
  else begin
    let checkpoint_id =
      let timestamp = int_of_float (Unix.time ()) in
      let random_suffix = Random.int 10000 in
      Printf.sprintf "cp-%s-%d-%d-%04d" task_id step timestamp random_suffix
    in
    let created_at = Option.value created_at ~default:(Unix.gettimeofday ()) in
    let record = {
      id = checkpoint_id;
      task_id;
      step;
      total_steps;
      action;
      state;
      agent;
      session_id;
      status;
      interrupt_message;
      created_at;
      resolved_at = None;
      reject_reason = None;
      parent_checkpoint_id = None;
      branch_name = None;
      state_edited = false;
    } in
    let path = checkpoint_path masc_dir ~task_id ~checkpoint_id in
    (try
       write_json path (checkpoint_json_of_record record);
       write_meta masc_dir ~task_id ~checkpoint_id ~step;
       Lwt.return (Ok checkpoint_id)
     with exn ->
       Lwt.return (Error (Printexc.to_string exn)))
  end

let find_latest_interrupted checkpoints =
  checkpoints
  |> List.filter (fun c -> equal_checkpoint_status c.status Interrupted)
  |> List.sort (fun a b ->
    let by_created = compare b.created_at a.created_at in
    if by_created <> 0 then by_created else compare b.step a.step
  )
  |> list_hd_opt

let update_checkpoint_file masc_dir (c : checkpoint_record) =
  let path = checkpoint_path masc_dir ~task_id:c.task_id ~checkpoint_id:c.id in
  write_json path (checkpoint_json_of_record c)

let update_checkpoint_status ~masc_dir ~task_id ~new_status ?(reason="") () =
  let* checkpoints =
    match load_task_checkpoints masc_dir task_id with
    | Ok list -> Lwt.return list
    | Error _ -> Lwt.return []
  in
  match find_latest_interrupted checkpoints with
  | None -> Lwt.return (Error "No interrupted checkpoint found")
  | Some target ->
      if not (can_transition ~from:Interrupted ~to_:new_status) then
        Lwt.return (Error "Invalid state transition")
      else begin
        let now = Unix.gettimeofday () in
        let updated = {
          target with
          status = new_status;
          resolved_at = Some now;
          reject_reason = if reason = "" then target.reject_reason else Some reason;
        } in
        (try
           update_checkpoint_file masc_dir updated;
           Lwt.return (Ok "Updated")
         with exn ->
           Lwt.return (Error (Printexc.to_string exn)))
      end

let approve_with_state_edit ~masc_dir ~task_id ~new_state =
  if not (is_valid_json_state new_state) then
    Lwt.return (Error "Invalid state: must be valid JSON")
  else begin
    let* checkpoints =
      match load_task_checkpoints masc_dir task_id with
      | Ok list -> Lwt.return list
      | Error _ -> Lwt.return []
    in
    match find_latest_interrupted checkpoints with
    | None -> Lwt.return (Error "No interrupted checkpoint found")
    | Some target ->
        if not (can_transition ~from:Interrupted ~to_:Completed) then
          Lwt.return (Error "Invalid state transition")
        else begin
          let now = Unix.gettimeofday () in
          let updated = {
            target with
            status = Completed;
            state = new_state;
            state_edited = true;
            resolved_at = Some now;
          } in
          (try
             update_checkpoint_file masc_dir updated;
             Lwt.return (Ok "Approved with state edit")
           with exn ->
             Lwt.return (Error (Printexc.to_string exn)))
        end
  end

let get_last_checkpoint ~masc_dir ~task_id =
  let read_by_id checkpoint_id =
    let path = checkpoint_path masc_dir ~task_id ~checkpoint_id in
    match read_json_opt path with
    | None -> Ok None
    | Some json ->
        (match checkpoint_record_of_json json with
         | Ok record -> Ok (Some (checkpoint_to_resume_json record))
         | Error _ -> Ok None)
  in
  match read_meta_last_checkpoint_id masc_dir task_id with
  | Some checkpoint_id ->
      Lwt.return (read_by_id checkpoint_id)
  | None ->
      let checkpoints =
        match load_task_checkpoints masc_dir task_id with
        | Ok list -> list
        | Error _ -> []
      in
      let latest =
        checkpoints
        |> List.sort (fun a b ->
          let by_step = compare b.step a.step in
          if by_step <> 0 then by_step else compare b.created_at a.created_at
        )
        |> list_hd_opt
      in
      Lwt.return (Ok (Option.map checkpoint_to_resume_json latest))

let auto_reject_timed_out ~timeout_minutes (c : checkpoint_record) =
  if equal_checkpoint_status c.status Interrupted &&
     is_timed_out ~created_at:c.created_at ~timeout_minutes
  then
    let reason =
      Printf.sprintf "Timeout: no response in %d minutes" timeout_minutes
    in
    Some { c with status = Rejected; resolved_at = Some (Unix.gettimeofday ()); reject_reason = Some reason }
  else
    None

let list_task_ids masc_dir =
  let root = checkpoints_root masc_dir in
  if Sys.file_exists root && Sys.is_directory root then
    Sys.readdir root
    |> Array.to_list
    |> List.filter (fun name ->
      let path = Filename.concat root name in
      Sys.is_directory path
    )
  else
    []

let iso8601_of_unix_seconds seconds =
  let tm = Unix.gmtime seconds in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let make_records values_list =
  `Assoc [
    ("records",
     `List (List.map (fun values -> `List [`List values]) values_list))
  ]

let get_pending_interrupts ~masc_dir ?(timeout_minutes=30) () =
  let task_ids = list_task_ids masc_dir in
  let rec gather_pending acc = function
    | [] -> acc
    | task_id :: rest ->
        let checkpoints =
          match load_task_checkpoints masc_dir task_id with
          | Ok list -> list
          | Error _ -> []
        in
        (* Auto-reject timed out interrupts (update list + persist) *)
        let checkpoints =
          List.map (fun c ->
            match auto_reject_timed_out ~timeout_minutes c with
            | None -> c
            | Some updated ->
                (try update_checkpoint_file masc_dir updated with _ -> ());
                updated
          ) checkpoints
        in
        let pending =
          checkpoints
          |> List.filter (fun c -> equal_checkpoint_status c.status Interrupted)
          |> List.sort (fun a b -> compare b.created_at a.created_at)
          |> List.map (fun c ->
            [
              `String c.id;
              `String c.task_id;
              `Int c.step;
              `String c.action;
              `String c.interrupt_message;
              `String (iso8601_of_unix_seconds c.created_at);
            ])
        in
        gather_pending (pending @ acc) rest
  in
  let records = gather_pending [] task_ids in
  Lwt.return (Ok (make_records records))

let revert_to_checkpoint ~masc_dir ~task_id ~target_step =
  let checkpoints =
    match load_task_checkpoints masc_dir task_id with
    | Ok list -> list
    | Error _ -> []
  in
  let target = List.find_opt (fun c -> c.step = target_step) checkpoints in
  match target with
  | None -> Lwt.return (Error (Printf.sprintf "Checkpoint step %d not found" target_step))
  | Some t ->
      let now = Unix.gettimeofday () in
      List.iter (fun c ->
        if c.step > target_step then
          let updated = { c with status = Reverted; resolved_at = Some now } in
          try update_checkpoint_file masc_dir updated with _ -> ()
      ) checkpoints;
      let json =
        `Assoc [
          ("id", `String t.id);
          ("step", `Int t.step);
          ("action", `String t.action);
          ("state", `String t.state);
          ("agent", `String t.agent);
          ("status", `String (status_to_string t.status));
          ("time_traveled", `Bool true);
        ]
      in
      Lwt.return (Ok json)

let branch_from_checkpoint ~masc_dir ~task_id ~source_step ~branch_name ~agent =
  if not (is_valid_task_id task_id) then
    Lwt.return (Error "Invalid task_id")
  else if branch_name = "" then
    Lwt.return (Error "Branch name is required")
  else begin
    let checkpoints =
      match load_task_checkpoints masc_dir task_id with
      | Ok list -> list
      | Error _ -> []
    in
    match List.find_opt (fun c -> c.step = source_step) checkpoints with
    | None -> Lwt.return (Error (Printf.sprintf "Checkpoint at step %d not found" source_step))
    | Some parent ->
        let now = Unix.gettimeofday () in
        let parent_updated = { parent with status = Branched; resolved_at = Some now } in
        (try update_checkpoint_file masc_dir parent_updated with _ -> ());
        let new_id =
          let timestamp = int_of_float (Unix.time ()) in
          let random_suffix = Random.int 10000 in
          Printf.sprintf "cp-%s-%d-%d-%04d" task_id (source_step + 1) timestamp random_suffix
        in
        let record = {
          id = new_id;
          task_id;
          step = source_step + 1;
          total_steps = 0;
          action = Printf.sprintf "[Branch: %s] from %s" branch_name parent.action;
          state = parent.state;
          agent;
          session_id = parent.session_id;
          status = Pending;
          interrupt_message = "";
          created_at = now;
          resolved_at = None;
          reject_reason = None;
          parent_checkpoint_id = Some parent.id;
          branch_name = Some branch_name;
          state_edited = false;
        } in
        (try
           write_json (checkpoint_path masc_dir ~task_id ~checkpoint_id:new_id) (checkpoint_json_of_record record);
           write_meta masc_dir ~task_id ~checkpoint_id:new_id ~step:record.step;
           let json = `Assoc [
             ("branch_id", `String new_id);
             ("parent_id", `String parent.id);
             ("branch_name", `String branch_name);
             ("step", `Int record.step);
             ("state", `String record.state);
           ] in
           Lwt.return (Ok json)
         with exn ->
           Lwt.return (Error (Printexc.to_string exn)))
  end
