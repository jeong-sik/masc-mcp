(** Tool_lock - Resource locking handlers *)

type context = {
  config: Room.config;
  agent_name: string;
}

(** Helper: get string from args *)
let get_string args key default =
  match Yojson.Safe.Util.member key args with
  | `String s -> s
  | _ -> default

(** Helper: result to response *)
let result_to_response = function
  | Ok msg -> (true, msg)
  | Error e -> (false, Types.masc_error_to_string e)

(** Helper: extract lock owner from JSON value *)
let lock_owner_of_value value =
  try
    let open Yojson.Safe.Util in
    match Yojson.Safe.from_string value |> member "owner" with
    | `String s -> Some s
    | _ -> None
  with _ -> None

(** Handle masc_lock *)
let handle_lock ctx args =
  let file = get_string args "file" "" in
  let resource =
    if file <> "" then file else get_string args "resource" ""
  in
  let result =
    let ( let* ) = Result.bind in
    let* resource = Room_utils.validate_file_path_r resource in
    let ttl_seconds = ctx.config.lock_expiry_minutes * 60 in
    match Room_utils.backend_acquire_lock ctx.config ~key:resource ~ttl_seconds ~owner:ctx.agent_name with
    | Ok true ->
        let now = Unix.gettimeofday () in
        let payload = `Assoc [
          ("resource", `String resource);
          ("owner", `String ctx.agent_name);
          ("acquired_at", `Float now);
          ("expires_at", `Float (now +. float_of_int ttl_seconds));
        ] in
        Ok (Yojson.Safe.pretty_to_string payload)
    | Ok false ->
        let owner =
          match Room_utils.backend_get ctx.config ~key:("locks:" ^ resource) with
          | Ok (Some v) -> lock_owner_of_value v
          | _ -> None
        in
        let by = Option.value owner ~default:"unknown" in
        Error (Types.FileLocked { file = resource; by })
    | Error msg ->
        Error (Types.IoError (Backend.show_error msg))
  in
  result_to_response result

(** Handle masc_unlock *)
let handle_unlock ctx args =
  let file = get_string args "file" "" in
  let resource =
    if file <> "" then file else get_string args "resource" ""
  in
  let result =
    let ( let* ) = Result.bind in
    let* resource = Room_utils.validate_file_path_r resource in
    match Room_utils.backend_release_lock ctx.config ~key:resource ~owner:ctx.agent_name with
    | Ok true ->
        Ok (Printf.sprintf "🔓 Unlocked: %s" resource)
    | Ok false ->
        let owner =
          match Room_utils.backend_get ctx.config ~key:("locks:" ^ resource) with
          | Ok (Some v) -> lock_owner_of_value v
          | _ -> None
        in
        (match owner with
         | Some by -> Error (Types.FileLocked { file = resource; by })
         | None -> Error (Types.FileNotLocked resource))
    | Error msg ->
        Error (Types.IoError (Backend.show_error msg))
  in
  result_to_response result

(** Dispatch handler. Returns Some (success, result) if handled, None otherwise *)
let dispatch ctx ~name ~args =
  match name with
  | "masc_lock" -> Some (handle_lock ctx args)
  | "masc_unlock" -> Some (handle_unlock ctx args)
  | _ -> None
