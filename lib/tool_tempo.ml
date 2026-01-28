(** Tempo Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    5 tools: tempo, tempo_get, tempo_set, tempo_adjust, tempo_reset
*)

(** Tool handler context *)
type context = {
  config: Room.config;
  agent_name: string;
}

(** Tool result type *)
type result = bool * string

(** {1 Argument Helpers} *)

let get_string args key default =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) -> s
       | _ -> default)
  | _ -> default

let get_string_opt args key =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) when s <> "" -> Some s
       | _ -> None)
  | _ -> None

let get_float args key default =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Float f) -> f
       | Some (`Int i) -> Float.of_int i
       | _ -> default)
  | _ -> default

(** {1 Individual Handlers} *)

let handle_tempo_get ctx _args : result =
  let state = Tempo.get_tempo ctx.config in
  (true, Yojson.Safe.pretty_to_string (Tempo.state_to_json state))

let handle_tempo_set ctx args : result =
  let interval = get_float args "interval_seconds" 0.0 in
  let reason = get_string args "reason" "manual" in
  if interval <= 0.0 then
    (false, "❌ interval_seconds must be > 0")
  else
    let state = Tempo.set_tempo ctx.config ~interval_s:interval ~reason in
    (true, Yojson.Safe.pretty_to_string (Tempo.state_to_json state))

let handle_tempo_adjust ctx _args : result =
  let state = Tempo.adjust_tempo ctx.config in
  (true, Yojson.Safe.pretty_to_string (Tempo.state_to_json state))

let handle_tempo_reset ctx _args : result =
  let state = Tempo.reset_tempo ctx.config in
  (true, Yojson.Safe.pretty_to_string (Tempo.state_to_json state))

let handle_tempo ctx args : result =
  let action = get_string args "action" "get" in
  match action with
  | "get" ->
      let json = Room.get_tempo ctx.config in
      (true, Yojson.Safe.pretty_to_string json)
  | "set" ->
      let mode = get_string args "mode" "normal" in
      let reason = get_string_opt args "reason" in
      (true, Room.set_tempo ctx.config ~mode ~reason ~agent_name:ctx.agent_name)
  | _ ->
      (false, "❌ Unknown action. Use 'get' or 'set'")

(** {1 Dispatcher} *)

let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_tempo_get" -> Some (handle_tempo_get ctx args)
  | "masc_tempo_set" -> Some (handle_tempo_set ctx args)
  | "masc_tempo_adjust" -> Some (handle_tempo_adjust ctx args)
  | "masc_tempo_reset" -> Some (handle_tempo_reset ctx args)
  | "masc_tempo" -> Some (handle_tempo ctx args)
  | _ -> None
