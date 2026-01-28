(** Tool_walph - Walph loop control handlers *)

type ('a, 'b) context = {
  config: Room.config;
  agent_name: string;
  net: 'a Eio.Net.t;
  clock: 'b Eio.Time.clock;
}

(* Helper functions *)
let get_string args key default =
  match args with
  | `Assoc l ->
      (match List.assoc_opt key l with
       | Some (`String s) -> s
       | _ -> default)
  | _ -> default

let get_string_opt args key =
  match args with
  | `Assoc l ->
      (match List.assoc_opt key l with
       | Some (`String s) when s <> "" -> Some s
       | _ -> None)
  | _ -> None

let get_int args key default =
  match args with
  | `Assoc l ->
      (match List.assoc_opt key l with
       | Some (`Int i) -> i
       | _ -> default)
  | _ -> default

(* Handle masc_walph_loop *)
let handle_walph_loop ctx args =
  let preset = get_string args "preset" "drain" in
  let max_iterations = get_int args "max_iterations" 10 in
  let target = get_string_opt args "target" in
  (true, Room_walph_eio.walph_loop ctx.config ~net:ctx.net ~clock:ctx.clock ~agent_name:ctx.agent_name ~preset ~max_iterations ?target ())

(* Handle masc_walph_control *)
let handle_walph_control ctx args =
  let command = get_string args "command" "STATUS" in
  let target_agent = get_string_opt args "target_agent" in
  (true, Room_walph_eio.walph_control ctx.config ~from_agent:ctx.agent_name ~command ~args:"" ~target_agent ())

(* Handle masc_walph_natural *)
let handle_walph_natural ctx args =
  let message = get_string args "message" "" in
  if message = "" then
    (true, "❌ message is required for natural language control")
  else begin
    (* Phase 1: Heuristic-based intent classification (fast, no network) *)
    let msg_lower = String.lowercase_ascii message in
    let contains s = try let _ = Str.search_forward (Str.regexp_string s) msg_lower 0 in true with Not_found -> false in

    let intent =
      if contains "stop" || contains "정지" || contains "그만" || contains "멈춰" then
        `Stop
      else if contains "pause" || contains "일시" || contains "잠깐" then
        `Pause
      else if contains "resume" || contains "재개" || contains "계속" || contains "다시" then
        `Resume
      else if contains "status" || contains "상태" || contains "뭐해" || contains "진행" then
        `Status
      else if contains "start" || contains "시작" || contains "커버리지" || contains "coverage" then
        `Start_coverage
      else if contains "refactor" || contains "리팩" || contains "lint" then
        `Start_refactor
      else if contains "docs" || contains "문서" || contains "doc" then
        `Start_docs
      else if contains "drain" || contains "태스크" || contains "task" then
        `Start_drain
      else
        `Ignore
    in

    match intent with
    | `Ignore ->
        (true, "ℹ️ Message not recognized as Walph command. Try: start, stop, pause, resume, status")
    | `Stop ->
        (true, Room_walph_eio.walph_control ctx.config ~from_agent:ctx.agent_name ~command:"STOP" ~args:"" ())
    | `Pause ->
        (true, Room_walph_eio.walph_control ctx.config ~from_agent:ctx.agent_name ~command:"PAUSE" ~args:"" ())
    | `Resume ->
        (true, Room_walph_eio.walph_control ctx.config ~from_agent:ctx.agent_name ~command:"RESUME" ~args:"" ())
    | `Status ->
        (true, Room_walph_eio.walph_control ctx.config ~from_agent:ctx.agent_name ~command:"STATUS" ~args:"" ())
    | `Start_coverage ->
        (true, Room_walph_eio.walph_loop ctx.config ~net:ctx.net ~clock:ctx.clock ~agent_name:ctx.agent_name ~preset:"coverage" ~max_iterations:10 ())
    | `Start_refactor ->
        (true, Room_walph_eio.walph_loop ctx.config ~net:ctx.net ~clock:ctx.clock ~agent_name:ctx.agent_name ~preset:"refactor" ~max_iterations:10 ())
    | `Start_docs ->
        (true, Room_walph_eio.walph_loop ctx.config ~net:ctx.net ~clock:ctx.clock ~agent_name:ctx.agent_name ~preset:"docs" ~max_iterations:10 ())
    | `Start_drain ->
        (true, Room_walph_eio.walph_loop ctx.config ~net:ctx.net ~clock:ctx.clock ~agent_name:ctx.agent_name ~preset:"drain" ~max_iterations:10 ())
  end

(* Dispatch handler *)
let dispatch ctx ~name ~args =
  match name with
  | "masc_walph_loop" -> Some (handle_walph_loop ctx args)
  | "masc_walph_control" -> Some (handle_walph_control ctx args)
  | "masc_walph_natural" -> Some (handle_walph_natural ctx args)
  | _ -> None
