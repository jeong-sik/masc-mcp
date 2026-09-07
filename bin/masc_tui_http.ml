(** TUI HTTP client — Dashboard API wrapper over Masc_http_client. *)

let report_err prefix msg = Printf.sprintf "(%s: %s)" prefix msg

(* A request, a mailbox wait or a loop gap at least this long is written to
   the TUI log with two clocks (RFC-0429 §3.0): elapsed time and process CPU
   time. A request that took ten seconds of elapsed and none of CPU was
   waiting on something. This is measurement, not a fix: the stall it exists
   to place has been placed since -- #33772 timed it as the main loop's own
   iteration cost, paid once per step of a reply -- and the fix, which is
   render and I/O sharing one domain, is not here.

   This threshold was written believing the cadence's routine polls would
   stay under it and out of the log. They do not. Measured over the live
   TUI logs on this host, 2026-09-07: 43 of the 72 lines are the three
   requests that ride every tick -- /gate/keepers 29 (median 2972 ms),
   /keepers/turns 12 (median 1298 ms), /keepers/tool-approvals 2. Which is
   the same finding from the other side: the loop is what slows a request,
   and a poll issued by that loop is slowed by it too. Raising the number
   would only hide the majority case, so it stays and the premise is
   written down as measured rather than as assumed.

   Elapsed is [Mtime_clock.elapsed_ns], which no NTP step moves. The stall
   being chased is around ten seconds, and a wall clock corrected by that
   much would either invent it or hide it — the one thing this log must not
   do. [Masc_tui_esc_interrupt] takes the same clock for the same reason. *)
let slow_report_ns = 1_000_000_000L
let ms_of_ns ns = Int64.to_float ns /. 1e6

let timed ~verb ~path (run : unit -> (int * string, string) result) =
  let started_ns = Mtime_clock.elapsed_ns () and started_cpu = Sys.time () in
  let result = run () in
  let elapsed_ns = Int64.sub (Mtime_clock.elapsed_ns ()) started_ns in
  if Int64.compare elapsed_ns slow_report_ns >= 0 then
    Log.Transport.info "http %s %s took %.0f ms elapsed, %.0f ms cpu: %s" verb
      path (ms_of_ns elapsed_ns)
      ((Sys.time () -. started_cpu) *. 1000.)
      (match result with
       | Ok (status, _) -> Printf.sprintf "status %d" status
       | Error detail -> detail);
  result
let default_timeout_sec = 10.0
let request_timeout_sec () = default_timeout_sec
let keeper_chat_timeout_sec = 180.0

(* A preset restore captures the live state into an autosave, then rewrites
   the override table, every keeper TOML and runtime.toml. The read timeout
   would give up while the server is still writing, and the operator would
   lose the only report that says what landed. *)
let preset_restore_timeout_sec = 120.0

(* One name for the send target. The buffered send and the streaming send are
   two ways of reading the same turn, not two endpoints, and a contract test
   pins that this literal appears once so they cannot drift apart. *)
let keeper_chat_stream_path = "/api/v1/keepers/chat/stream"
let mcp_path = "/mcp"
let observer_stream_path = "/mcp?sse_kind=observer"
let keeper_turn_interrupt_path = "/api/v1/keepers/turn/interrupt"
let keeper_tool_approval_path = "/api/v1/keepers/tool-approval"
let fusion_runs_path = "/api/v1/dashboard/fusion-runs"
let runtime_probe_path = "/api/v1/dashboard/runtime-probe"

let trim_nonempty = String_util.trim_nonempty

let first_nonempty_env names =
  List.find_map
    (fun name -> Option.bind (Env_config_core.raw_value_opt name) trim_nonempty)
    names

let sanitize_header_value value =
  value
  |> String.map (function
       | '\r' | '\n' -> ' '
       | c -> c)
  |> String.trim

let default_agent_name = Masc_tui_credential.agent_name

(* One name for the bearer the write routes require, so the header builder and
   the surfaces that report its absence cannot disagree about whether this
   process holds one.

   The bearer is discovered once, at boot, from the workspace the TUI was told
   to open -- not per request. This module is never handed a base path, and
   resolving one on its own would let the client present the credential of a
   workspace other than the one on screen. *)
let operator_token_cell = ref None

(* Carry out [Masc_tui_credential.plan]. The environment wins so a single run
   can be pointed at a different credential; otherwise the bearer comes from the
   workspace, and a workspace that demands one but holds none gets one minted.

   Minting grants nothing this process did not already have: the credential
   store is a directory under the workspace, so anything that can read the
   bearer masc login wrote can equally write another. The trust boundary is
   filesystem access to the workspace, not possession of the token. What it
   does remove is the operator's obligation to carry a secret from shell to
   shell, which is where every refusal in this file started.

   Admin because that is the role [masc login] issues for this agent, and the
   keeper lifecycle routes the TUI already offers require it -- minting a
   narrower role would leave working surfaces failing. *)
let install_operator_token ~base_path ~host ~port =
  let cfg = Auth.load_auth_config base_path in
  (* The auth directory, not the config file: a missing config reads as the
     default, so its absence proves nothing about whether a workspace is here.
     The directory holds the credential store, so a workspace a server has ever
     served has one.

     Read before anything else in startup can create it. Other startup steps do
     make directories under .masc for a base path that names nothing -- a
     mistyped flag gets an empty .masc/keepers -- and a check that ran after
     one of those had made .masc/auth would read its own footprint as evidence
     of a workspace. *)
  let workspace_initialized = Sys.file_exists (Auth.auth_dir base_path) in
  let outcome =
    match
      Masc_tui_credential.plan
        ~env_token:(first_nonempty_env [ Masc_tui_credential.token_env_var ])
        ~workspace_token:
          (Auth_login.read_persisted_token ~base_path
             ~agent_name:default_agent_name)
        ~workspace_requires_token:(cfg.enabled && cfg.require_token)
        ~workspace_initialized
    with
    | Masc_tui_credential.Use token ->
        operator_token_cell := Some token;
        Masc_tui_credential.Held
    | Masc_tui_credential.Go_without ->
        operator_token_cell := None;
        Masc_tui_credential.Not_required
    | Masc_tui_credential.No_workspace ->
        operator_token_cell := None;
        Masc_tui_credential.Unavailable Masc_tui_credential.no_workspace_detail
    | Masc_tui_credential.Mint -> (
        match
          Auth_login.mint ~base_path ~host ~port
            ~agent_name:default_agent_name ~role:Masc_domain.Admin
            ~token_env_var:Masc_tui_credential.token_env_var
            ~token_lifetime:
              (Auth_login.Expires_in_hours
                 Masc_tui_credential.self_mint_expiry_hours)
            ()
        with
        | Ok report ->
            operator_token_cell := Some report.bearer_token;
            Masc_tui_credential.Minted
        | Error err ->
            operator_token_cell := None;
            Masc_tui_credential.Unavailable
              (Masc_domain.masc_error_to_string err))
  in
  outcome

let operator_token () = !operator_token_cell
let operator_token_present () = Option.is_some (operator_token ())

let auth_headers () =
  let agent_header = [ ("X-MASC-Agent", default_agent_name) ] in
  match operator_token () with
  | Some token ->
      ("Authorization", "Bearer " ^ sanitize_header_value token) :: agent_header
  | None -> agent_header

let json_headers headers =
  ("Content-Type", "application/json") :: headers

let host_for_url host =
  if String.contains host ':' && not (String.starts_with ~prefix:"[" host) then
    "[" ^ host ^ "]"
  else host

let url_of ~(host : string) ~(port : int) ~(path : string) =
  Printf.sprintf "http://%s:%d%s" (host_for_url host) port path

let percent_encode_path_segment value = Uri.pct_encode value

(* A query value keeps the characters a path segment may not, and loses the
   two that end a value. A file path carries slashes, so encoding it as a path
   segment would spell them out and the server would not find the file. *)
let percent_encode_query_value value =
  Uri.pct_encode ~component:`Query_value value

let request_clock () = Eio_context.get_clock_opt ()

(** Send an HTTP GET request and return the structured status/body pair. *)
let http_get ~(host : string) ~(port : int) ~(path : string) :
    (int * string, string) result =
  let url = url_of ~host ~port ~path in
  timed ~verb:"GET" ~path @@ fun () ->
  match
    Masc_http_client.get_sync ?clock:(request_clock ())
      ~timeout_sec:(request_timeout_sec ()) ~url ~headers:(auth_headers ()) ()
  with
  | Ok (status, body) -> Ok (status, body)
  | Error e -> Error (report_err "GET failed" e)

(** Fetch an arbitrary external URL's body for web link previews. Unlike the
    dashboard helpers above this sends NO masc auth header -- the URL is a
    third-party site, so leaking the operator's token there would be a real
    credential exposure. Only http(s) is followed, and only a 2xx response
    yields a body. Response size is capped by {!Masc_http_client} (8 MB). *)
let fetch_link_preview_body ~(url : string) : (string, string) result =
  if not (String.starts_with ~prefix:"http://" url
          || String.starts_with ~prefix:"https://" url)
  then Error "link preview: unsupported url scheme"
  else
    let headers =
      [ ("User-Agent", "masc-tui/link-preview (+https://github.com/jeong-sik/masc)")
      ; ("Accept", "text/html,application/xhtml+xml") ]
    in
    match
      Masc_http_client.get_response_sync ?clock:(request_clock ())
        ~timeout_sec:(request_timeout_sec ()) ~url ~headers ()
    with
    | Error e -> Error (report_err "link preview GET failed" e)
    | Ok { Masc_http_client.status; body; _ } ->
        if status >= 200 && status < 300 then Ok body
        else Error (Printf.sprintf "link preview: HTTP %d" status)

(** Send an HTTP POST request with a JSON body and return the structured status/body pair. *)
let http_post_with_timeout ~timeout_sec ~headers ~(host : string) ~(port : int)
    ~(path : string) ~(body : string) : (int * string, string) result =
  let url = url_of ~host ~port ~path in
  timed ~verb:"POST" ~path @@ fun () ->
  match
    Masc_http_client.post_sync ?clock:(request_clock ())
      ~timeout_sec ~url ~headers:(json_headers headers) ~body ()
  with
  | Ok (status, body) -> Ok (status, body)
  | Error e -> Error (report_err "POST failed" e)

let http_post ~headers ~(host : string) ~(port : int) ~(path : string)
    ~(body : string) : (int * string, string) result =
  http_post_with_timeout ~timeout_sec:(request_timeout_sec ()) ~headers ~host
    ~port ~path ~body

(* A refusal is about this client's credential, not about the surface that
   asked for the data. Every surface used to paste the server's auth JSON into
   the terminal -- "HTTP 401: {\"error\":\"[AuthError] Invalid token..." -- which
   names neither what is wrong nor what clears it. Answered here because this
   is the one place that knows what was presented. Other statuses keep the
   server's own words: those are about the request, and the surface is right to
   show them. *)
let decode_json ~allow_empty ~status_code ~body =
  match status_code with
  | 401 | 403 ->
      Error
        (Masc_tui_credential.refusal
           ~credential_sent:(operator_token_present ()))
  | _ ->
      Masc.Tui_decode.decode_json_response_body ~allow_empty ~status_code ~body

(** GET a JSON response from a dashboard endpoint. *)
let get_json ~(host : string) ~(port : int) ~(path : string) : (Yojson.Safe.t, string) result =
  match http_get ~host ~port ~path with
  | Error e -> Error e
  | Ok (status_code, body) -> decode_json ~allow_empty:false ~status_code ~body

(** POST a JSON body and parse the JSON response. *)
let post_json_with_timeout ~timeout_sec ~(host : string) ~(port : int)
    ~(path : string) ~(body : string) : (Yojson.Safe.t, string) result =
  match
    http_post_with_timeout ~timeout_sec ~headers:(auth_headers ()) ~host ~port
      ~path ~body
  with
  | Error e -> Error e
  | Ok (status_code, body) -> decode_json ~allow_empty:true ~status_code ~body

let post_json ~(host : string) ~(port : int) ~(path : string) ~(body : string) : (Yojson.Safe.t, string) result =
  match http_post ~headers:(auth_headers ()) ~host ~port ~path ~body with
  | Error e -> Error e
  | Ok (status_code, body) -> decode_json ~allow_empty:true ~status_code ~body

let post_keeper_chat ~(host : string) ~(port : int)
    (request : Masc_tui_keeper_chat_projection.request) :
    ( Masc_tui_keeper_chat_projection.response
    , Masc_tui_keeper_chat_projection.error )
    result =
  let url = url_of ~host ~port ~path:keeper_chat_stream_path in
  let headers =
    json_headers
      (("Accept", "text/event-stream") :: auth_headers ())
  in
  (* Whole body, no live view, nothing held to resume after. *)
  let body =
    Masc_tui_keeper_chat_projection.request_body
      ~since_seq:Masc.Keeper_chat_event_log.Whole_turn request
  in
  match
    Masc_http_client.post_sync ?clock:(request_clock ())
      ~timeout_sec:keeper_chat_timeout_sec ~url ~headers ~body ()
  with
  | Error detail ->
      Error (Masc_tui_keeper_chat_projection.Transport_error detail)
  | Ok (status, response_body)
    when not (Masc.Tui_decode.is_success_http_status status) ->
      Error
        (Masc_tui_keeper_chat_projection.Http_error
           { status; body = response_body })
  | Ok (_, response_body) ->
      Masc_tui_keeper_chat_projection.decode_response_with_provenance ~request
        response_body
      |> Result.map_error (fun error ->
             Masc_tui_keeper_chat_projection.Protocol_error error)

(** Send a keeper chat turn and hand each response chunk to [on_chunk] as it
    arrives, so the caller can draw the turn while it runs.

    The turn's outcome still comes from the strict decode over the complete
    body, which the streaming read returns as well. So this returns exactly
    what {!post_keeper_chat} would have returned for the same stream, and a
    defect in whatever [on_chunk] drives cannot change it.

    [keeper_chat_timeout_sec] is the silence bound here rather than a total
    cap. That is strictly more room than the buffered send had: a turn that
    keeps emitting is no longer cut off at all, and one that goes quiet is
    still bounded by the same number. *)
(* [since_seq] is the whole turn on the first POST and the log's resume
   position on a re-POST after the stream was cut, so the server replays only
   what the pane missed before switching to live frames. *)
let post_keeper_chat_streaming ~clock ~(host : string) ~(port : int)
    ~(on_chunk : string -> unit)
    ~(since_seq : Masc.Keeper_chat_event_log.replay_position)
    (request : Masc_tui_keeper_chat_projection.request) :
    ( Masc_tui_keeper_chat_projection.response
    , Masc_tui_keeper_chat_projection.error )
    result =
  let url = url_of ~host ~port ~path:keeper_chat_stream_path in
  let headers =
    json_headers (("Accept", "text/event-stream") :: auth_headers ())
  in
  let body =
    Masc_tui_keeper_chat_projection.request_body ~since_seq request
  in
  match
    Masc_http_client.post_stream ~clock
      ~idle_timeout_sec:keeper_chat_timeout_sec ~url ~headers ~body ~on_chunk ()
  with
  | Error detail ->
      Error (Masc_tui_keeper_chat_projection.Transport_error detail)
  | Ok (Masc_http_client.Pool.Buffered { status; body; _ }) ->
      Error (Masc_tui_keeper_chat_projection.Http_error { status; body })
  | Ok (Masc_http_client.Pool.Streamed { response; _ }) ->
      Masc_tui_keeper_chat_projection.decode_response_with_provenance ~request
        response.Masc_http_client.Pool.body
      |> Result.map_error (fun error ->
             Masc_tui_keeper_chat_projection.Protocol_error error)

(** Fetch the operator evidence bundle for one awaiting-verification task
    ([GET /api/v1/verification/evidence]). A task outside
    awaiting_verification is a 400 whose body names why; that text becomes
    the error the pane draws. *)
let fetch_verification_evidence ~(host : string) ~(port : int)
    ~(task_id : string) :
    (Masc.Tui_decode.verification_evidence, string) result =
  let path =
    Printf.sprintf "/api/v1/verification/evidence?task_id=%s"
      (percent_encode_path_segment task_id)
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "evidence returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json -> Masc.Tui_decode.decode_verification_evidence json
      | exception Yojson.Json_error detail ->
          Error ("evidence was not JSON: " ^ detail))

(** Fetch one goal's merged event timeline
    ([GET /api/v1/dashboard/goals/detail]). Only the [timeline] (and the
    queue-state detail behind a [`Null] timeline) is decoded; the rest of the
    detail payload stays server-side until a surface needs it. *)
let fetch_goal_timeline ~(host : string) ~(port : int) ~(goal_id : string) :
    (Masc.Tui_decode.goal_timeline, string) result =
  let path =
    Printf.sprintf "/api/v1/dashboard/goals/detail?goal_id=%s"
      (percent_encode_path_segment goal_id)
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "goal detail returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json -> Masc.Tui_decode.decode_goal_detail_timeline json
      | exception Yojson.Json_error detail ->
          Error ("goal detail was not JSON: " ^ detail))

(** Fetch one task's event history
    ([GET /api/v1/dashboard/tasks/history]). *)
let fetch_task_history ~(host : string) ~(port : int) ~(task_id : string) :
    (Masc.Tui_decode.task_history_event list, string) result =
  let path =
    Printf.sprintf "/api/v1/dashboard/tasks/history?task_id=%s&limit=50"
      (percent_encode_path_segment task_id)
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "task history returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json -> Masc.Tui_decode.decode_task_history json
      | exception Yojson.Json_error detail ->
          Error ("task history was not JSON: " ^ detail))

(** Fetch a keeper's durable tool-call log
    ([GET /api/v1/keepers/:name/tool-calls]). *)
let fetch_keeper_calls ~(host : string) ~(port : int) ~(keeper_name : string)
    ~(limit : int) : (Masc.Tui_decode.keeper_calls_snapshot, string) result =
  let path =
    Printf.sprintf "/api/v1/keepers/%s/tool-calls?limit=%d"
      (percent_encode_path_segment keeper_name)
      (max 1 limit)
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "tool calls returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json ->
          Masc.Tui_decode.decode_keeper_calls_snapshot
            ~requested_keeper:keeper_name json
      | exception Yojson.Json_error detail ->
          Error ("tool calls were not JSON: " ^ detail))

(** Fetch the files a keeper wrote
    ([GET /api/v1/keepers/:name/file-changes]).

    The window is time rather than a row count. A count could not say what it
    had covered -- the server scans a multiple of it in fleet rows, so a
    keeper that made no more calls and a scan that stopped short arrive
    looking the same. The server bounds the window at what the read costs and
    states in its answer the window it actually covered. *)
(** One directory level of the workspace ([/api/v1/workspace/children];
    an empty [path] asks [/workspace/tree] for the root). *)
(* The workspace routes resolve [?keeper=] to that keeper's playground root
   (resolve_workspace_base), which is how a Changes row's clone-relative
   address ("repos/<id>/<path>") becomes readable here. No keeper means the
   project workspace, as before. *)
let keeper_query_suffix = function
  | None -> ""
  | Some keeper -> "&keeper=" ^ percent_encode_query_value keeper

(* The other workspace axis: [?repo_id=] resolves to one of the project's
   registered repositories (resolve_workspace_base), the address a
   Repositories row carries. *)
let repo_query_suffix = function
  | None -> ""
  | Some repo_id -> "&repo_id=" ^ percent_encode_query_value repo_id

let fetch_workspace_entries ?keeper ?repo ~(host : string) ~(port : int)
    ~(path : string) () :
    (Masc.Tui_decode.workspace_tree_node list, string) result =
  (* One route for every level, the root included: [tree?depth=0] walks the
     whole workspace and returned nested files under a 200-node cap, so the
     root pane showed ".ci/hardening-baseline.json" beside ".ci". The server
     answers a bare list, so ask for its maximum and let the pane read a full
     page as truncated. *)
  let route =
    Printf.sprintf "/api/v1/workspace/children?path=%s&limit=%d"
      (percent_encode_query_value path)
      Server_routes_http_routes_workspace.max_tree_node_limit
    ^ keeper_query_suffix keeper
    ^ repo_query_suffix repo
  in
  match http_get ~host ~port ~path:route with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "workspace entries returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error detail ->
          Error ("workspace entries were not JSON: " ^ detail)
      | json -> Masc.Tui_decode.decode_workspace_tree json)

(** The whole file at [path] ([/api/v1/workspace/file]). *)
let fetch_workspace_file ?keeper ?repo ~(host : string) ~(port : int)
    ~(path : string) () : (string, string) result =
  let route =
    Printf.sprintf "/api/v1/workspace/file?path=%s%s%s"
      (percent_encode_query_value path)
      (keeper_query_suffix keeper)
      (repo_query_suffix repo)
  in
  match http_get ~host ~port ~path:route with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "workspace file returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error detail ->
          Error ("workspace file was not JSON: " ^ detail)
      | json -> Masc.Tui_decode.decode_workspace_file json)

(** The file's commit history ([/api/v1/git/log]), most recent first. *)
let fetch_git_log ?keeper ?repo ~(host : string) ~(port : int)
    ~(path : string) ~(limit : int) () :
    (Masc.Tui_decode.git_log_row list, string) result =
  let route =
    Printf.sprintf "/api/v1/git/log?path=%s&limit=%d%s%s"
      (percent_encode_query_value path)
      limit
      (keeper_query_suffix keeper)
      (repo_query_suffix repo)
  in
  match http_get ~host ~port ~path:route with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "git log returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error detail ->
          Error ("git log was not JSON: " ^ detail)
      | json -> Masc.Tui_decode.decode_git_log json)

(** Who last touched each run of lines in the file ([/api/v1/git/blame]).
    Scoped by the same keeper / repo axes the tree and the log use, so the
    answer describes the checkout the file was read from. *)
let fetch_git_blame ?keeper ?repo ~(host : string) ~(port : int)
    ~(path : string) () :
    (Masc.Tui_decode.blame_block list, string) result =
  let route =
    Printf.sprintf "/api/v1/git/blame?path=%s%s%s"
      (percent_encode_query_value path)
      (keeper_query_suffix keeper)
      (repo_query_suffix repo)
  in
  match http_get ~host ~port ~path:route with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "git blame returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error detail ->
          Error ("git blame was not JSON: " ^ detail)
      | json -> Masc.Tui_decode.decode_git_blame json)

(** Durable Keeper writes over one repository file. Unlike the removed IDE
    region store, this reads the tool-call log projection and therefore keeps
    working after the producing turn exits. *)
(* [repo_id] is a required labeled option rather than [?repo_id]: this
   signature has no positional argument at all, so an optional here can never
   be erased (warning 16). *)
let fetch_ide_file_activity ~(host : string) ~(port : int)
    ~(repo_id : string option) ~(file_path : string) :
    (Masc.Tui_decode.file_activity_snapshot, string) result =
  let route =
    "/api/v1/ide/file-activity?file_path="
    ^ percent_encode_query_value file_path
    ^ Option.fold ~none:"" ~some:(fun repo_id ->
        "&repo_id=" ^ percent_encode_query_value repo_id)
        repo_id
  in
  match http_get ~host ~port ~path:route with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "file activity returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | `Assoc fields -> (
          match List.assoc_opt "ok" fields, List.assoc_opt "data" fields with
          | Some (`Bool true), Some data ->
            Masc.Tui_decode.decode_file_activity_snapshot data
          | Some (`Bool false), _ -> (
              match List.assoc_opt "error" fields with
              | Some (`String detail) -> Error detail
              | Some _ | None -> Error "file activity rejected")
          | (Some _ | None), _ -> Error "unexpected file activity envelope")
      | _ -> Error "unexpected file activity envelope"
      | exception Yojson.Json_error detail ->
        Error ("file activity was not JSON: " ^ detail))

(** Ask the language server about a name on a line
    ([GET /api/v1/lsp/question]). [question] is the route's own word
    (definition / hover); positions are 1-based both ways. *)
let fetch_lsp_question ?keeper ?repo ~(host : string) ~(port : int)
    ~(path : string) ~(line : int) ~(symbol : string) ~(question : string) ()
    : (Masc.Tui_decode.lsp_answer, string) result =
  let route =
    Printf.sprintf "/api/v1/lsp/question?question=%s&path=%s&line=%d&symbol=%s%s%s"
      (percent_encode_query_value question)
      (percent_encode_query_value path)
      line
      (percent_encode_query_value symbol)
      (keeper_query_suffix keeper)
      (repo_query_suffix repo)
  in
  match http_get ~host ~port ~path:route with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    -> (
      (* The route's refusals are JSON with the reason in "error"; hand the
         reason itself to the pane rather than a status line. *)
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error _ ->
          Error (Printf.sprintf "lsp question returned %d: %s" status body)
      | `Assoc fields -> (
          match List.assoc_opt "error" fields with
          | Some (`String e) -> Error e
          | Some _ | None ->
              Error (Printf.sprintf "lsp question returned %d: %s" status body))
      | _ ->
          Error (Printf.sprintf "lsp question returned %d: %s" status body))
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error detail ->
          Error ("lsp question was not JSON: " ^ detail)
      | json -> Masc.Tui_decode.decode_lsp_answer json)

let fetch_keeper_file_changes ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(window_hours : float) :
    (Masc.Tui_decode.file_change_snapshot, string) result =
  let path =
    Printf.sprintf "/api/v1/keepers/%s/file-changes?window_hours=%g"
      (percent_encode_path_segment keeper_name)
      (Float.max 0.0 window_hours)
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "file changes returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json -> Masc.Tui_decode.decode_file_change_snapshot json
      | exception Yojson.Json_error detail ->
          Error ("file changes were not JSON: " ^ detail))

(** Open the MCP session the observer feed is registered under.

    The transport registers an SSE observer only for a session it has seen
    [initialize]; the id of the new session comes back in the
    [Mcp-Session-Id] response header. *)
let open_mcp_session ~(host : string) ~(port : int) ~(client_version : string)
    : (string, string) result =
  let url = url_of ~host ~port ~path:mcp_path in
  let headers =
    json_headers
      (("Accept", "application/json, text/event-stream") :: auth_headers ())
  in
  let body = Masc_tui_observer.initialize_request_body ~client_version in
  match
    Masc_http_client.post_response_sync ?clock:(request_clock ())
      ~timeout_sec:(request_timeout_sec ()) ~url ~headers ~body ()
  with
  | Error detail -> Error (report_err "MCP initialize failed" detail)
  | Ok { Masc_http_client.status; body; _ }
    when not (Masc.Tui_decode.is_success_http_status status) ->
      Error (Printf.sprintf "MCP initialize returned %d: %s" status body)
  | Ok { Masc_http_client.headers; _ } ->
      Masc_tui_observer.session_id_of_headers headers

(** Read the runtime's event feed until it ends.

    Blocks on the calling fiber for the life of the stream and hands every
    body chunk to [on_chunk] as it arrives. The silence bound is the one
    the keeper chat stream uses: a feed from a runtime with keepers turning
    that says nothing for that long has gone quiet, and the caller reopens
    it on its own schedule. [Ok ()] is the server closing the stream; a
    refusal and a transport failure both come back as [Error]. *)
let observe_runtime_events ~clock ~(host : string) ~(port : int)
    ~(session_id : string) ~(on_chunk : string -> unit) : (unit, string) result
    =
  let url = url_of ~host ~port ~path:observer_stream_path in
  let headers =
    ("Accept", "text/event-stream")
    :: ("Mcp-Session-Id", sanitize_header_value session_id)
    :: auth_headers ()
  in
  match
    Masc_http_client.get_stream ~clock ~idle_timeout_sec:keeper_chat_timeout_sec
      ~url ~headers ~on_chunk ()
  with
  | Error detail -> Error (report_err "observer stream failed" detail)
  | Ok (Masc_http_client.Pool.Buffered { status; body; _ }) ->
      Error (Printf.sprintf "observer stream refused with %d: %s" status body)
  | Ok (Masc_http_client.Pool.Streamed _) -> Ok ()

(** One MCP [tools/call] under an existing session.

    The task tools ([masc_add_task], [masc_transition]) have no REST route;
    this is how the dashboard calls them and now how the TUI does. The
    session is the one the observer feed opened, or one the caller opened
    for this call; the server keeps sessions across requests. *)
let call_mcp_tool ~(host : string) ~(port : int) ~(session_id : string)
    ~(request_id : string) ~(tool : string)
    ~(arguments : (string * Yojson.Safe.t) list) :
    (Masc_tui_mcp.outcome, string) result =
  let headers =
    json_headers
      (("Accept", "application/json, text/event-stream")
      :: ("Mcp-Session-Id", sanitize_header_value session_id)
      :: auth_headers ())
  in
  let body = Masc_tui_mcp.request_body ~request_id ~tool ~arguments in
  match http_post ~headers ~host ~port ~path:mcp_path ~body with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "tools/call returned %d: %s" status body)
  | Ok (_, body) -> Masc_tui_mcp.outcome_of_body ~request_id body

(** Fetch a keeper's durable chat transcript.

    The pane's scrollback used to be session-local while the server kept the
    transcript all along. *)
let fetch_keeper_chat_history ~(host : string) ~(port : int)
    ~(keeper_name : string) :
    (Masc_tui_keeper_chat_history.decoded, string) result =
  let path =
    Printf.sprintf "/api/v1/keepers/%s/chat/history"
      (percent_encode_path_segment keeper_name)
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status) ->
      Error (Printf.sprintf "chat history returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json -> Masc_tui_keeper_chat_history.rows_of_json json
      | exception Yojson.Json_error detail ->
          Error ("chat history was not JSON: " ^ detail))

let fetch_keeper_memory_journal ~(host : string) ~(port : int)
    ~(keeper_name : string) :
    (Masc_tui_keeper_chat_history.decoded, string) result =
  let path =
    Printf.sprintf "/api/v1/keepers/%s/memory-journal?limit=20"
      (percent_encode_path_segment keeper_name)
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status) ->
      Error (Printf.sprintf "memory journal returned %d: %s" status body)
  | Ok (_, body) ->
      (match Yojson.Safe.from_string body with
       | json -> Masc_tui_keeper_chat_history.memory_rows_of_json json
       | exception Yojson.Json_error detail ->
           Error ("memory journal was not JSON: " ^ detail))

(** Fetch one actual Librarian input only when the operator asks from Config.
    The run listing deliberately omits payloads; first resolve the newest
    Librarian run id, then open that one Admin-only detail record. This keeps a
    normal prompt-list refresh from downloading the potentially large bounded
    conversation input of every retained run. *)
let fetch_latest_librarian_input ~(host : string) ~(port : int) :
    (string list, string) result =
  let get_json ~label path =
    match http_get ~host ~port ~path with
    | Error detail -> Error (label ^ " request failed: " ^ detail)
    | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status) ->
        Error (Printf.sprintf "%s returned %d: %s" label status body)
    | Ok (_, body) ->
        (match Yojson.Safe.from_string body with
         | json -> Ok json
         | exception Yojson.Json_error detail ->
             Error (label ^ " was not JSON: " ^ detail))
  in
  let open Result.Syntax in
  let* listing =
    get_json
      ~label:"exact lane run listing"
      "/api/v1/dashboard/exact-lane-runs?limit=1&lane=librarian_exact"
  in
  let* page = Masc.Tui_decode.decode_librarian_run_page listing in
  let* run_id =
    page.lrp_run_id
    |> Option.to_result ~none:"no retained Librarian exact run"
  in
  let* detail =
    get_json
      ~label:"Librarian exact run detail"
      ("/api/v1/dashboard/exact-lane-runs/" ^ percent_encode_path_segment run_id)
  in
  Masc.Tui_decode.decode_librarian_actual_input ~run_id detail

(* The server filters before it pages. A quiet Verifier lane must not be hidden
   behind a busy Librarian lane's bounded window, and the TUI must not impose a
   second arbitrary search cap over the durable registry. *)
let lane_run_list_limit = 50

(* One run's payloads can be megabytes of conversation history (a 136 MB
   [conversation_history] field is why the listing drops payloads); beyond a
   bound the TUI declines to parse rather than hanging the frame on a body it
   would truncate anyway. *)
let lane_run_detail_max_body_bytes = 4 * 1024 * 1024

(** Recent run summaries of one standalone lane, newest first. *)
let fetch_lane_runs ~(host : string) ~(port : int) ~(lane : string) :
    (Masc.Tui_decode.lane_run_summary list, string) result =
  let open Result.Syntax in
  let path =
    Printf.sprintf
      "/api/v1/dashboard/exact-lane-runs?limit=%d&lane=%s"
      lane_run_list_limit
      (percent_encode_path_segment lane)
  in
  let* listing = get_json ~host ~port ~path in
  let* page = Masc.Tui_decode.decode_lane_run_page ~lane listing in
  Ok page.lrpg_runs

(** The full record of one standalone-lane run, including exact prompt/output
    or Verifier request/verdict/tool evidence. *)
let fetch_lane_run_detail ~(host : string) ~(port : int) ~(run_id : string) :
    (Masc.Tui_decode.lane_run_detail, string) result =
  match
    http_get ~host ~port
      ~path:
        ("/api/v1/dashboard/exact-lane-runs/"
         ^ percent_encode_path_segment run_id)
  with
  | Error detail -> Error ("lane run detail request failed: " ^ detail)
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status) ->
      Error (Printf.sprintf "lane run detail returned %d: %s" status body)
  | Ok (_, body) ->
      if String.length body > lane_run_detail_max_body_bytes then
        Error
          (Printf.sprintf
             "lane run record is %d bytes; the TUI does not render a payload \
              above %d bytes"
             (String.length body) lane_run_detail_max_body_bytes)
      else
        (match Yojson.Safe.from_string body with
         | json -> Masc.Tui_decode.decode_lane_run_detail json
         | exception Yojson.Json_error detail ->
             Error ("lane run detail was not JSON: " ^ detail))

(** Fetch one page of chat rows older than [before].

    [before] absent asks for the newest window, which is what the transcript
    fetch already returns; the pane passes a cursor, so it is required here.
    Defined before {!fetch_keeper_context_inspector}, which reads the answer
    to a turn through it. *)
(** One page of a turn's journal
    ([GET /api/v1/keepers/:name/chat/events?operation_id=&since_seq=&limit=],
    RFC-0412 §3.2). The page is checked against the operation it was asked
    for; every failure is typed ({!Masc_tui_keeper_chat_log.events_error}) so
    the caller decides by code, not by reading the server's sentence. *)
let fetch_keeper_chat_events ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(operation_id : string)
    ~(since_seq : Masc.Keeper_chat_event_log.replay_position) ~(limit : int) :
    ( Masc_tui_keeper_chat_log.events_page
    , Masc_tui_keeper_chat_log.events_error )
    result =
  (* The whole journal is the parameter's absence; a held seq is the seq. *)
  let since_seq_query =
    match Masc.Keeper_chat_event_log.replay_position_to_wire since_seq with
    | None -> ""
    | Some seq -> Printf.sprintf "&since_seq=%d" seq
  in
  let path =
    Printf.sprintf "/api/v1/keepers/%s/chat/events?operation_id=%s%s&limit=%d"
      (percent_encode_path_segment keeper_name)
      (percent_encode_query_value operation_id)
      since_seq_query limit
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error (Masc_tui_keeper_chat_log.Events_transport detail)
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status) ->
      Error
        (Masc_tui_keeper_chat_log.decode_events_error ~status
           ~credential_sent:(operator_token_present ()) body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json -> (
          match Masc_tui_keeper_chat_log.decode_events_page json with
          | Error detail -> Error (Masc_tui_keeper_chat_log.Events_undecodable detail)
          | Ok page
            when not
                   (String.equal page.Masc_tui_keeper_chat_log.operation_id
                      operation_id) ->
              Error
                (Masc_tui_keeper_chat_log.Events_undecodable
                   (Printf.sprintf "page is for operation %s, asked for %s"
                      page.Masc_tui_keeper_chat_log.operation_id operation_id))
          | Ok page -> Ok page)
      | exception Yojson.Json_error detail ->
          Error
            (Masc_tui_keeper_chat_log.Events_undecodable
               ("events body was not JSON: " ^ detail)))

let fetch_keeper_chat_history_page ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(before : float) :
    (Masc_tui_keeper_chat_history.page, string) result =
  let path =
    (* %.17g rather than %h: both round-trip through float_of_string, but the
       hex form carries a '+' in its exponent, which a query string reads as a
       space. 17 significant digits is the shortest width that is exact for
       every double. *)
    Printf.sprintf "/api/v1/keepers/%s/chat/history/page?before=%.17g"
      (percent_encode_path_segment keeper_name)
      before
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status) ->
      Error (Printf.sprintf "chat history page returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json -> Masc_tui_keeper_chat_history.page_of_json json
      | exception Yojson.Json_error detail ->
          Error ("chat history page was not JSON: " ^ detail))

(** Fetch one completed turn and the immutable provider-input snapshot joined
    by that turn's exact [turn_ref]. A failure on either side stays visible;
    no mutable latest-prompt value is allowed to fill another turn. *)
let fetch_keeper_context_inspector ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(turn_back : int)
    : Masc_tui_context_inspector.reading =
  let fetch ~label ~path ~decode =
    match http_get ~host ~port ~path with
    | Error detail -> Error (label ^ " request failed: " ^ detail)
    | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status) ->
        Error (Printf.sprintf "%s returned %d: %s" label status body)
    | Ok (_, body) ->
        (match Yojson.Safe.from_string body with
         | json -> decode json
         | exception Yojson.Json_error detail ->
             Error (label ^ " was not JSON: " ^ detail))
  in
  let encoded = percent_encode_path_segment keeper_name in
  let turn =
    fetch ~label:"turn-records"
      ~path:(Printf.sprintf "/api/v1/keepers/%s/turn-records?limit=50" encoded)
      ~decode:Masc_tui_context_inspector.decode_turn_records
  in
  (* Which row the exact provider input is read for. Stepping back names the
     row itself; the newest reading keeps the attributed fallback, because a
     keeper mid-turn has not written the snapshot for the row it is on and
     the newest row that did is still the honest newest answer. *)
  let viewing_record selection =
    if turn_back <= 0 then
      Option.map
        (fun (attributed : Masc_tui_context_inspector.attributed_turn) ->
           attributed.record)
        selection.Masc_tui_context_inspector.attributed
    else
      List.nth_opt selection.Masc_tui_context_inspector.rows turn_back
  in
  let provider_input =
    match turn with
    | Error detail -> Error ("provider-input turn unavailable: " ^ detail)
    | Ok selection -> (
      match viewing_record selection with
      | None ->
          Error "provider-input unavailable: no turn on this page recorded an exact input composition"
      | Some record ->
          let turn_ref = Ids.Turn_ref.to_string record.Turn_record.turn_ref in
          fetch ~label:"provider-input"
            ~path:
              (Printf.sprintf
                 "/api/v1/keepers/%s/provider-input?turn_ref=%s"
                 encoded
                 (percent_encode_query_value turn_ref))
            ~decode:
              (Masc_tui_context_inspector.decode_provider_input
                 ~expected_keeper:keeper_name
                 ~expected_turn_ref:record.Turn_record.turn_ref))
  in
  (* The answer that came back: the newest transcript page, joined to the
     row by the turn_ref the chat rows carry. Rows this join cannot reach
     say so; they never borrow another turn's answer. *)
  let response =
    match turn with
    | Error detail -> Error ("response unavailable: " ^ detail)
    | Ok selection -> (
      match viewing_record selection with
      | None -> Error "response unavailable: no row on this page to name"
      | Some record ->
          let key = Ids.Turn_ref.to_string record.Turn_record.turn_ref in
          (match fetch_keeper_chat_history_page ~host ~port ~keeper_name
                   ~before:(Unix.gettimeofday ()) with
            | Error detail -> Error ("response unavailable: " ^ detail)
            | Ok page ->
                let parts =
                  List.filter_map
                    (fun (row : Masc_tui_keeper_chat_history.row) ->
                       if
                         not
                           (String.equal
                              (Option.value ~default:"" row.turn_id)
                              key)
                       then None
                       else
                         match row.kind with
                         | Masc_tui_keeper_chat_history.Said_by_keeper
                         | Masc_tui_keeper_chat_history.Autonomous_reply ->
                             if row.text = "" then None
                             else
                               Some
                                 (Masc_tui_context_inspector.Reply_text
                                    row.text)
                         | Masc_tui_keeper_chat_history.Tool_calls block ->
                             Some
                               (Masc_tui_context_inspector.Tool_steps
                                  (Masc_tui_keeper_chat_history.tool_rows
                                     block))
                         | Masc_tui_keeper_chat_history.Reasoning lines ->
                             Some
                               (Masc_tui_context_inspector.Reasoning_lines
                                  lines)
                         | _ -> None)
                    page.Masc_tui_keeper_chat_history.decoded.rows
                in
                Ok
                  { Masc_tui_context_inspector.parts
                  ; outside_newest_page = parts = []
                  }))
  in
  { Masc_tui_context_inspector.turn; provider_input; response }

(** What the server did with one answer to a held tool call.

    [settled] reports whether a wait was actually released. False means the
    call had already timed out or been answered. [remembered] reports whether
    the server kept the answer for the identical retried call: a late answer
    that still descends from a question the operator was shown is not wasted,
    and the pane should say so rather than only "too late". *)
type tool_approval_answer =
  { settled : bool
  ; remembered : bool
  }

let post_keeper_tool_approval ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(tool_call_id : string) ~(allow : bool) :
    (tool_approval_answer, string) result =
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [ ("name", `String keeper_name)
         ; ("tool_call_id", `String tool_call_id)
         ; ("decision", `String (if allow then "approve" else "deny"))
         ])
  in
  match post_json ~host ~port ~path:keeper_tool_approval_path ~body with
  | Error detail -> Error detail
  | Ok json -> (
      match json with
      | `Assoc fields -> (
          match
            ( List.assoc_opt "settled" fields
            , List.assoc_opt "remembered" fields )
          with
          | Some (`Bool settled), Some (`Bool remembered) ->
              Ok { settled; remembered }
          | _ -> Error "approval response has no settled/remembered flags")
      | _ -> Error "approval response was not a JSON object")

let post_keeper_turn_interrupt ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(request_id : string) :
    (Masc_tui_interrupt_signal.interrupt_signal, string) result =
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [ ("name", `String keeper_name)
         ; ("request_id", `String request_id)
         ])
  in
  match
    post_json ~host ~port ~path:keeper_turn_interrupt_path ~body
  with
  | Error detail -> Error detail
  | Ok json ->
    Masc_tui_interrupt_signal.decode_interrupt_signal
      ~expected_request_id:request_id json

let fetch_keeper_chat_operation ~(host : string) ~(port : int)
    (request : Masc_tui_keeper_chat_projection.request) :
    ( Masc_tui_keeper_chat_projection.operation_reconciliation
    , Masc_tui_keeper_chat_projection.error )
    result =
  let path =
    Printf.sprintf "/api/v1/keepers/%s/chat/operations/%s"
      (percent_encode_path_segment request.keeper_name)
      (percent_encode_path_segment request.request_id)
  in
  match http_get ~host ~port ~path with
  | Error detail ->
      Error (Masc_tui_keeper_chat_projection.Transport_error detail)
  | Ok (status, response_body)
    when not (Masc.Tui_decode.is_success_http_status status) ->
      Error
        (Masc_tui_keeper_chat_projection.Http_error
           { status; body = response_body })
  | Ok (_, response_body) ->
      (match Yojson.Safe.from_string response_body with
       | json ->
           Masc_tui_keeper_chat_projection.decode_operation_reconciliation
             ~request json
           |> Result.map_error (fun error ->
                  Masc_tui_keeper_chat_projection.protocol_error error)
       | exception Yojson.Json_error detail ->
           Error
             (Masc_tui_keeper_chat_projection.protocol_error
                (Masc_tui_keeper_chat_projection.Malformed_event
                   ("Keeper chat operation response is invalid JSON: "
                  ^ detail))))

(** Fetch the live keeper roster from [GET /api/v1/gate/keepers].

    The Keepers surface needs one fact the durable metadata on disk cannot
    give it: whether a keepalive fiber is running each keeper. This route is
    [masc_keeper_list], the same reading the channel connectors use, and it
    answers in a few kilobytes — the operator snapshot carries the same fact
    inside a payload 150 times larger.

    The status is returned rather than folded into an error string: this route
    requires an operator token, and "no token" is a different thing for the
    surface to say than "the read failed". *)
let fetch_keeper_runtimes ~(host : string) ~(port : int) :
    (int * string, string) result =
  http_get ~host ~port ~path:"/api/v1/gate/keepers?detailed=true"

(** POST a keeper lifecycle action ([boot] / [shutdown]).

    Returns the HTTP status alongside the body: a paused owner refuses [boot]
    with 409, and the caller routes that into the resume-then-boot recovery
    rather than reporting it as a failure. Collapsing the status into an error
    string would make that decision a substring match. *)
let post_keeper_lifecycle ~(host : string) ~(port : int) ~(keeper_name : string)
    ~(action : string) : (int * string, string) result =
  let path =
    Printf.sprintf "/api/v1/keepers/%s/%s"
      (percent_encode_path_segment keeper_name)
      (percent_encode_path_segment action)
  in
  http_post ~headers:(auth_headers ()) ~host ~port ~path
    ~body:Masc_tui_keeper_control.lifecycle_body

(** POST a keeper directive ([pause] / [resume] / [wakeup]). *)
let post_keeper_directive ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(action : string)
    ~(operator_operation_id : string) : (int * string, string) result =
  let path =
    Printf.sprintf "/api/v1/keepers/%s/directive"
      (percent_encode_path_segment keeper_name)
  in
  let body =
    Masc_tui_keeper_control.directive_body ~operator_operation_id action
  in
  http_post ~headers:(auth_headers ()) ~host ~port ~path ~body

(** Fetch /api/v1/dashboard/briefing (Mission / Overview snapshot). *)
let fetch_dashboard_briefing ~(host : string) ~(port : int) : (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/briefing"

(** Fetch /api/v1/dashboard/transport-health (delivery-path summary). *)
let fetch_transport_health ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/transport-health"

(** Fetch the actor-scoped operator summary that owns pending confirmations. *)
let fetch_operator_snapshot ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:"/api/v1/operator?view=summary&include_messages=0&include_keepers=0"

(** GET /api/v1/runtime/resolved — runtimes and keeper assignments. *)
let fetch_runtime_resolved ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/runtime/resolved"

(** GET /api/v1/dashboard/clients — everyone attached to this workspace:
    directory agents, state-backed sessions, runtime fibers. *)
let fetch_clients ~(host : string) ~(port : int) : (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/clients"

(** GET /api/v1/dashboard/runtime-probe — cached provider metadata
    reachability. [force] schedules a background refresh past the route's
    recent-value window; the returned [refresh_state] remains authoritative. *)
let fetch_runtime_probe ~(host : string) ~(port : int) ~(force : bool) :
    (Yojson.Safe.t, string) result =
  let path = if force then runtime_probe_path ^ "?force=1" else runtime_probe_path in
  get_json ~host ~port ~path

type runtime_config_commit_receipt = Masc_tui_runtime_config_receipt.t
type runtime_assignment_write_result =
  | Runtime_assignment_committed of runtime_config_commit_receipt
  | Runtime_assignment_unchanged

let decode_runtime_config_commit_receipt = Masc_tui_runtime_config_receipt.decode
let runtime_config_commit_receipt_summary = Masc_tui_runtime_config_receipt.summary

(** POST /api/v1/runtime/config/assignment — point a keeper at a runtime.
    [runtime_id = None] clears the explicit assignment back to the default. *)
let post_runtime_assignment ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(runtime_id : string option)
    ~(expected_assignment_revision : Yojson.Safe.t) :
    (runtime_assignment_write_result, string) result =
  let body =
    Yojson.Safe.to_string
      (`Assoc
         (("keeper_name", `String keeper_name)
          ::
          (match runtime_id with
           | Some id -> [ ("runtime_id", `String id) ]
           | None -> [])
          @ [ "expected_assignment_revision", expected_assignment_revision ]))
  in
  match
    post_json ~host ~port ~path:"/api/v1/runtime/config/assignment" ~body
  with
  | Error detail -> Error detail
  | Ok (`Assoc fields as json) ->
    (match List.assoc_opt "applied" fields with
     | Some (`Bool false) ->
       Masc_tui_keeper_config.decode_unchanged_runtime_assignment_response json
       |> Result.map (fun _revision -> Runtime_assignment_unchanged)
     | Some _ | None ->
       decode_runtime_config_commit_receipt json
       |> Result.map (fun receipt -> Runtime_assignment_committed receipt))
  | Ok _ -> Error "runtime assignment response must be an object"

(** POST /api/v1/runtime/config/routing — set one lane's candidate order.

    The server owns the write: it previews the resulting runtime.toml, refuses
    an id the catalog does not know, and only then persists. This sends the
    whole list because the endpoint's contract is the lane's order, not a
    delta — a caller that sent one id would be declaring the lane has one
    candidate. A success is a commit receipt ([{ok = true; state =
    "committed"; ...}]) — decoded here so a 2xx body of any other shape is an
    error rather than a guessed success, matching [tool_envelope_outcome]. *)
let set_runtime_lane_slots ~(host : string) ~(port : int) ~(lane : string)
      ~(runtime_ids : string list) : (unit, string) result =
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [ "lane", `String lane
         ; "runtime_ids", `List (List.map (fun id -> `String id) runtime_ids)
         ])
  in
  match
    post_json ~host ~port ~path:"/api/v1/runtime/config/routing" ~body
  with
  | Error detail -> Error detail
  | Ok json ->
    decode_runtime_config_commit_receipt json
    |> Result.map (fun (_receipt : runtime_config_commit_receipt) -> ())
;;

(** GET /api/v1/keepers/tool-approvals — the tool calls keepers are holding. *)
let fetch_keeper_tool_approvals ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/keepers/tool-approvals"

(** GET /api/v1/keepers/turns — which keepers are mid-turn right now. The
    running-turn slot lives in the server's Keeper Owner, not in the durable
    meta the local keeper list is read from, so the badge has to ask. *)
let fetch_keeper_turns ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/keepers/turns"

(** POST /api/v1/keepers/<name>/identity-switch — turn one attached service
    on or off for this keeper without touching the consent. *)
let post_identity_switch ~(host : string) ~(port : int) ~(keeper_name : string)
    ~(provider_id : string) ~(enabled : bool) : (unit, string) result =
  let body =
    Yojson.Safe.to_string
      (`Assoc [ ("provider", `String provider_id); ("enabled", `Bool enabled) ])
  in
  match
    post_json ~host ~port
      ~path:
        (Printf.sprintf "/api/v1/keepers/%s/identity-switch" keeper_name)
      ~body
  with
  | Error detail -> Error detail
  | Ok json -> (
      match json with
      | `Assoc fields -> (
          match List.assoc_opt "ok" fields with
          | Some (`Bool true) -> Ok ()
          | Some _ | None -> Error "identity switch response does not say ok")
      | _ -> Error "identity switch response was not a JSON object")

(** GET /api/v1/dashboard/gate — the durable Gate: pending approvals that
    survive nobody watching, plus both lane modes. *)
let fetch_dashboard_gate ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/gate"

let expect_ok_true ~(what : string) json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "ok" fields with
      | Some (`Bool true) -> Ok ()
      | Some _ | None -> Error (what ^ " response does not say ok"))
  | _ -> Error (what ^ " response was not a JSON object")

(** POST /api/v1/dashboard/gate/resolve — decide one durable Gate approval. *)
(* [reason] is a required-labeled option rather than [?reason]: nothing
   follows it, so an optional argument here is unerasable (warning 16). *)
let post_dashboard_gate_resolve ~(host : string) ~(port : int)
    ~(approval_id : string) ~(approve : bool) ~(reason : string option) :
    (unit, string) result =
  let body =
    Yojson.Safe.to_string
      (`Assoc
         ([ ("id", `String approval_id)
          ; ("decision", `String (if approve then "approve" else "reject"))
          ]
         @ match reason with
           | None -> []
           | Some reason -> [ "reason", `String reason ]))
  in
  match post_json ~host ~port ~path:"/api/v1/dashboard/gate/resolve" ~body with
  | Error detail -> Error detail
  | Ok json -> expect_ok_true ~what:"gate resolve" json

(** POST /api/v1/dashboard/gate/retry — explicitly rearm a blocked Auto
    Judge only with the complete row identity just observed. The server checks
    every field again, so this never turns a refresh race into a retry of a
    different external effect. *)
let post_dashboard_gate_retry ~(host : string) ~(port : int)
    ~(request : Yojson.Safe.t) : (unit, string) result =
  let body = Yojson.Safe.to_string request in
  match post_json ~host ~port ~path:"/api/v1/dashboard/gate/retry" ~body with
  | Error detail -> Error detail
  | Ok json -> expect_ok_true ~what:"gate retry" json

(** POST /api/v1/dashboard/gate/external-mode — set the external-services
    lane. Its own switch: the workspace lane never opens this one. *)
let post_dashboard_gate_external_mode ~(host : string) ~(port : int)
    ~(mode : string) : (unit, string) result =
  let body = Yojson.Safe.to_string (`Assoc [ ("mode", `String mode) ]) in
  match
    post_json ~host ~port ~path:"/api/v1/dashboard/gate/external-mode" ~body
  with
  | Error detail -> Error detail
  | Ok json -> expect_ok_true ~what:"gate external mode" json

(** GET /api/v1/dashboard/gate/keeper-settings — durable per-keeper Gate
    settings: which Keepers were held stricter than the workspace, and which
    judge each is put to first. *)
let fetch_keeper_gate_settings ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/gate/keeper-settings"

(** GET /api/v1/runtime/params — the Runtime_params registry: every knob this
    build registered, what it is set to, and what it would be with nobody
    overriding it. *)
let fetch_runtime_params ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/runtime/params"

(** Set one typed Runtime_params override.  [value] stays JSON all the way to
    the server; the registry owns type and bounds validation. *)
let post_runtime_param_set ~(host : string) ~(port : int) ~(key : string)
    ~(value : Yojson.Safe.t) : (Yojson.Safe.t, string) result =
  let body =
    Yojson.Safe.to_string
      (`Assoc [ "param_key", `String key; "value", value ])
  in
  post_json ~host ~port ~path:"/api/v1/runtime/params/set" ~body

(** Clear one Runtime_params override, returning it to the registered default. *)
let post_runtime_param_clear ~(host : string) ~(port : int) ~(key : string) :
    (Yojson.Safe.t, string) result =
  let body =
    Yojson.Safe.to_string (`Assoc [ "param_key", `String key ])
  in
  post_json ~host ~port ~path:"/api/v1/runtime/params/clear" ~body

(** GET /api/v1/keepers/tool-approval-mode — per-keeper gate stances. *)
let fetch_keeper_tool_approval_modes ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/keepers/tool-approval-mode"

(** POST /api/v1/keepers/tool-approval-mode — set one keeper's gate stance. *)
let post_keeper_tool_approval_mode ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(mode : string) : (unit, string) result =
  let body =
    Yojson.Safe.to_string
      (`Assoc [ ("name", `String keeper_name); ("mode", `String mode) ])
  in
  match
    post_json ~host ~port ~path:"/api/v1/keepers/tool-approval-mode" ~body
  with
  | Error detail -> Error detail
  | Ok _ -> Ok ()

(** POST /api/v1/operator/confirm to approve/deny a pending confirmation. *)
let operator_confirm_body ~(token : string)
    ~(decision : Masc_tui_operator_projection.approval_decision) =
  let decision = Masc_tui_operator_projection.approval_decision_wire decision in
  Yojson.Safe.to_string
    (`Assoc [ ("confirm_token", `String token); ("decision", `String decision) ])

let post_operator_confirm ~(host : string) ~(port : int) ~(token : string)
    ~(decision : Masc_tui_operator_projection.approval_decision) :
    (Masc_tui_operator_projection.confirm_outcome, string) result =
  let body = operator_confirm_body ~token ~decision in
  match post_json ~host ~port ~path:"/api/v1/operator/confirm" ~body with
  | Error _ as error -> error
  | Ok json ->
      Masc_tui_operator_projection.decode_confirm_response
        ~expected_token:token ~expected_decision:decision json

(** POST /api/v1/keepers/ask-answer.

    The answers come from [Masc_tui_ask_projection.readiness], which builds
    them in the ask's question order from choice ids the row itself carried.
    The store settles on first write, so a second submission gets back the
    answer that won rather than a bare rejection: the caller reads
    [state] and [answers] out of the body to say what was chosen. *)
let post_keeper_ask_answer ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(ask_id : string) ~(answers : Yojson.Safe.t)
    ~(actor_id : string option) ~(session_id : string option) :
    (Yojson.Safe.t, string) result =
  let payload =
    match
      Masc_tui_ask_projection.request_body ~answers ~actor_id ~session_id
    with
    | `Assoc fields ->
        `Assoc (("name", `String keeper_name) :: ("ask_id", `String ask_id) :: fields)
    | other -> other
  in
  post_json ~host ~port ~path:"/api/v1/keepers/ask-answer"
    ~body:(Yojson.Safe.to_string payload)

(** Fetch /api/v1/board (post list) in the operator-selected server order,
    optionally narrowed to one hearth.

    The narrowing is the server's, not a filter over what arrived: the listing
    is paged, and a client-side filter over one page of a board where 71% of
    posts sit in a single hearth would show three rows and call it the
    hearth. *)
let fetch_board ~(host : string) ~(port : int) ~(sort_by : string)
    ~(hearth : string option) : (Yojson.Safe.t, string) result =
  let narrowing =
    match hearth with
    | None -> ""
    | Some hearth -> "&hearth=" ^ percent_encode_query_value hearth
  in
  get_json ~host ~port
    ~path:("/api/v1/board?sort_by=" ^ sort_by ^ narrowing)

(** Fetch /api/v1/board/hearths: every sub-board and how many posts sit in it,
    counted over the whole board rather than over one listing page. The pane
    cycles through these, and a vocabulary read off a page cannot offer a
    hearth whose posts all fall outside it. *)
let fetch_board_hearths ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/board/hearths"

(** POST /api/v1/tools/masc_board_post. The draft follows the commit-message
    shape -- first line is the title, the rest is the body -- and the server
    stamps the author from the agent header, so the payload carries text
    only. The response is the tools envelope [{ok, message}]; interpreting it
    stays with the caller. *)
let post_board_new ~(host : string) ~(port : int) ~(title : string)
    ~(body : string) ?hearth () : (Yojson.Safe.t, string) result =
  let hearth_field =
    match hearth with
    | Some h when String.trim h <> "" -> [ ("hearth", `String (String.trim h)) ]
    | _ -> []
  in
  let payload =
    `Assoc
      ([ ("title", `String title); ("body", `String body) ]
      @ hearth_field)
  in
  post_json ~host ~port ~path:"/api/v1/tools/masc_board_post"
    ~body:(Yojson.Safe.to_string payload)

(** POST /api/v1/tools/masc_goal_transition. The action travels as the tool's
    own wire word via [Goal_phase.Public_action.to_string] rather than a
    local literal, so the TUI and the tool cannot disagree about what
    "drop" means. The server owns the phase rules; an invalid transition is
    its rejection to return, not the TUI's to pre-guess. *)
let post_goal_transition ~(host : string) ~(port : int) ~(goal_id : string)
    ~(action : Goal_phase.Public_action.t)
    ~(note : string option) : (Yojson.Safe.t, string) result =
  let payload =
    `Assoc
      ([ ("goal_id", `String goal_id)
       ; ("action", `String (Goal_phase.Public_action.to_string action))
       ]
      @
      match note with
      | Some text -> [ ("note", `String text) ]
      | None -> [])
  in
  post_json ~host ~port ~path:"/api/v1/tools/masc_goal_transition"
    ~body:(Yojson.Safe.to_string payload)

(** POST /api/v1/tools/masc_board_vote. [up] rides as a bool rather than a
    string so no direction word exists here to drift from the tool's. *)
let post_board_vote ~(host : string) ~(port : int) ~(post_id : string)
    ~(up : bool) : (Yojson.Safe.t, string) result =
  let payload =
    `Assoc
      [ ("post_id", `String post_id)
      ; ("direction", `String (if up then "up" else "down"))
      ]
  in
  post_json ~host ~port ~path:"/api/v1/tools/masc_board_vote"
    ~body:(Yojson.Safe.to_string payload)

(** POST /api/v1/tools/masc_board_comment. The author is stamped by the
    route from the agent header, exactly as for a new post. *)
let post_board_comment ~(host : string) ~(port : int) ~(post_id : string)
    ~(content : string) : (Yojson.Safe.t, string) result =
  let payload =
    `Assoc [ ("post_id", `String post_id); ("content", `String content) ]
  in
  post_json ~host ~port ~path:"/api/v1/tools/masc_board_comment"
    ~body:(Yojson.Safe.to_string payload)

(** Fetch /api/v1/board/<postId> (post detail + comments). *)
let fetch_board_post ~(host : string) ~(port : int) ~(post_id : string) : (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/board/%s?format=flat"
         (percent_encode_path_segment post_id))

(** Fetch /api/v1/dashboard/scheduled-automation (schedule list projection).
    The server sorts active-first by due time and caps rows at its own limit,
    so the path alone is the whole request. *)
let fetch_schedules ~(host : string) ~(port : int) : (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/scheduled-automation"

(** Fetch one schedule's exact projection, which carries its wake history. The
    aggregate above sends one wake per row; this route reads the same ledger
    for a single [schedule_id] and is not on the aggregate's shared cache. *)
let fetch_schedule_detail ~(host : string) ~(port : int) ~(schedule_id : string) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/dashboard/scheduled-automation?schedule_id=%s"
         (percent_encode_query_value schedule_id))
(** Fetch the schedules aimed at one target. The fleet page caps at its own
    limit with active rows first, so a Keeper whose schedules are terminal or
    further down does not appear on it; this asks for that Keeper's own page. *)
let fetch_schedules_for_target ~(host : string) ~(port : int)
    ~(payload_target : string) : (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      (Printf.sprintf
         "/api/v1/dashboard/scheduled-automation?payload_target=%s"
         (percent_encode_query_value payload_target))

(** Create or atomically modify a schedule from the JSON form the TUI handed
    to [$EDITOR]. The server validates each against its canonical Tool schema;
    update requires [schedule_id] and refuses running or terminal rows. *)
let post_schedule_create_json ~(host : string) ~(port : int) ~(body_json : string) =
  post_json ~host ~port ~path:"/api/v1/tools/masc_schedule_create"
    ~body:body_json

let post_schedule_update ~(host : string) ~(port : int) ~(body_json : string) =
  post_json ~host ~port ~path:"/api/v1/tools/masc_schedule_update"
    ~body:body_json

(** POST /api/v1/tools/masc_schedule_cancel. The payload is the tool's own
    argument contract, so validation is the tool's, not duplicated here.
    [cancelled_by_kind] is omitted: the tool defaults it to human operator,
    which is what a terminal operator is. The reason is a fixed audit phrase --
    the arm display already named which schedule the second press cancels. *)
let post_schedule_cancel ~(host : string) ~(port : int) ~(schedule_id : string)
    : (Yojson.Safe.t, string) result =
  let payload =
    `Assoc
      [ ("schedule_id", `String schedule_id)
      ; ("cancelled_by_id", `String default_agent_name)
      ; ("reason", `String "cancelled from the TUI")
      ]
  in
  post_json ~host ~port ~path:"/api/v1/tools/masc_schedule_cancel"
    ~body:(Yojson.Safe.to_string payload)

(** POST /api/v1/tools/masc_schedule_create. The payload is the tool's own
    argument contract; the kind-specific timing fields arrive already
    assembled by the caller (the form's typed spec builds them), and time
    syntax, cron text, and timezone spellings stay the tool's to validate.
    The requester rides as this process, a human operator's terminal. *)
let post_schedule_create ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(message : string)
    ~(timing_fields : (string * Yojson.Safe.t) list) :
    (Yojson.Safe.t, string) result =
  let payload =
    `Assoc
      ([ ("keeper_name", `String keeper_name)
       ; ("message", `String message)
       ; ("requested_by_id", `String default_agent_name)
       ; ("requested_by_kind", `String "human_operator")
       ; ("scheduled_by_id", `String default_agent_name)
       ; ("scheduled_by_kind", `String "human_operator")
       ; ("source", `String "operator_request")
       ]
      @ timing_fields)
  in
  post_json ~host ~port ~path:"/api/v1/tools/masc_schedule_create"
    ~body:(Yojson.Safe.to_string payload)

(** POST /api/v1/verification/verdict — the operator's verdict on a task
    awaiting verification. The route demands a reason with a reject and takes
    none with an approve, so the variant carries it only where it rides. The
    route wants a token-bound admin credential — the one this process mints
    at startup. *)
let post_verification_verdict ~(host : string) ~(port : int)
    ~(task_id : string) ~(verdict : [ `Approve | `Reject of string ]) :
    (Yojson.Safe.t, string) result =
  let fields =
    match verdict with
    | `Approve ->
        [ ("task_id", `String task_id); ("verdict", `String "approve") ]
    | `Reject reason ->
        [ ("task_id", `String task_id)
        ; ("verdict", `String "reject")
        ; ("reason", `String reason)
        ]
  in
  post_json ~host ~port ~path:"/api/v1/verification/verdict"
    ~body:(Yojson.Safe.to_string (`Assoc fields))

(* The operator's own verdict on a Harness row, joined to the machine's by
   the notes hash. The verdict is what the human holds — not agreement with
   the machine — so the caller resolves y/n against the row before posting. *)
let post_harness_label ~(host : string) ~(port : int) ~(notes_hash : string)
    ~(verdict : [ `Approve | `Reject ]) ~(reason : string) :
    (Yojson.Safe.t, string) result =
  post_json ~host ~port ~path:"/api/v1/dashboard/harness-label"
    ~body:
      (Yojson.Safe.to_string
         (`Assoc
           [ ("notes_hash", `String notes_hash)
           ; ( "verdict"
             , `String
                 (match verdict with `Approve -> "approve" | `Reject -> "reject")
             )
           ; ("reason", `String reason)
           ]))

(** POST /api/v1/keepers/:name/config — a partial settings patch. The body is
    exactly the fields the operator left in $EDITOR; a field absent from the
    body is absent from the patch, so the editor round-trip cannot blank a
    setting it never showed. Validation is the route's (it re-uses
    masc_keeper_up's arg parsing), not duplicated here. *)
type keeper_config_post_error =
  | Keeper_config_transport_error of string
  | Keeper_config_revision_conflict of Yojson.Safe.t
  | Keeper_config_reconciliation_required of Yojson.Safe.t
  | Keeper_config_runtime_sync_failed of Yojson.Safe.t
  | Keeper_config_http_error of
      { status : int
      ; body : string
      }

let keeper_config_post_error_to_string = function
  | Keeper_config_transport_error detail -> detail
  | Keeper_config_revision_conflict _ ->
    "keeper config revision conflict; authoritative reload required"
  | Keeper_config_reconciliation_required _ ->
    "keeper config reconciliation required; authoritative reload required"
  | Keeper_config_runtime_sync_failed _ ->
    "keeper config applied but runtime sync failed; authoritative reload required"
  | Keeper_config_http_error { status; body } ->
    Printf.sprintf "keeper config returned %d: %s" status body

let post_keeper_config ~(host : string) ~(port : int) ~(keeper_name : string)
    ~(patch_json : string) : (Yojson.Safe.t, keeper_config_post_error) result =
  let path =
    Printf.sprintf "/api/v1/keepers/%s/config"
      (percent_encode_path_segment keeper_name)
  in
  match http_post ~headers:(auth_headers ()) ~host ~port ~path ~body:patch_json with
  | Error detail -> Error (Keeper_config_transport_error detail)
  | Ok (status, body) when Masc.Tui_decode.is_success_http_status status ->
    (match Yojson.Safe.from_string body with
     | json -> Ok json
     | exception Yojson.Json_error detail ->
       Error (Keeper_config_http_error { status; body = detail }))
  | Ok (status, body) ->
    let parsed =
      match Yojson.Safe.from_string body with
      | json -> Some json
      | exception Yojson.Json_error _ -> None
    in
    let code =
      match parsed with
      | Some json ->
        (match Json_util.assoc_member_opt "error" json with
         | Some error ->
           (match Json_util.assoc_member_opt "code" error with
            | Some (`String code) -> Some code
            | Some _ | None -> None)
         | None -> None)
      | None -> None
    in
    let config_application_indeterminate =
      (* Read the state discriminator, not the whole object shape: a
         structural compare flips to false the moment the server adds a
         field beside [state], which is the write-outcome-unknown moment
         the operator most needs reported. *)
      match parsed with
      | Some json ->
        (match Json_util.assoc_member_opt "config_application" json with
         | Some app -> Json_util.get_string app "state" = Some "indeterminate"
         | None -> false)
      | None -> false
    in
    (match status, code, parsed with
     | 409, Some code, Some json
       when String.equal
              code
              Masc.Keeper_turn_up_update.config_revision_conflict_code ->
       Error (Keeper_config_revision_conflict json)
     | ( 503
       , Some
           ( "keeper_manifest_reconciliation_required"
           | "keeper_config_composite_reconciliation_required" )
       , Some json )
       when config_application_indeterminate ->
       Error (Keeper_config_reconciliation_required json)
     | 503, Some "keeper_runtime_sync_failed", Some json
       when Json_util.assoc_member_opt "config_applied" json = Some (`Bool true) ->
       Error (Keeper_config_runtime_sync_failed json)
     | _ -> Error (Keeper_config_http_error { status; body }))

(** POST /api/v1/keepers/:name/up — masc_keeper_up's own create-or-update
    contract. The keeper name in the path is the row the operator launched
    from; the body carries the rest of the declaration. *)
let post_keeper_up ~(host : string) ~(port : int) ~(keeper_name : string)
    ~(declaration_json : string) : (Yojson.Safe.t, string) result =
  post_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/keepers/%s/up"
         (percent_encode_path_segment keeper_name))
    ~body:declaration_json

(** Register a repository. Same permission boundary as the dashboard's add
    dialog: the route wants CanAdmin, and [post_json] carries the token. *)
let post_repository_add ~(host : string) ~(port : int)
    ~(declaration_json : string) : (Yojson.Safe.t, string) result =
  post_json ~host ~port ~path:"/api/v1/repositories" ~body:declaration_json

(** Fetch /api/v1/dashboard/logs. The server caps [limit] at 3000; the TUI asks
    for a screenful's worth of history rather than the whole ring. [level] is
    the route's minimum-level floor in its own lowercase spelling; absent, the
    server serves everything (its default floor is debug), and an invalid
    spelling is the route's 400 to give, not this client's to pre-judge. *)
let fetch_dashboard_logs ~(host : string) ~(port : int) ?level ~(limit : int)
    () : (Yojson.Safe.t, string) result =
  let level_query =
    match level with None -> "" | Some level -> "&level=" ^ level
  in
  get_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/dashboard/logs?limit=%d%s"
         (max 1 (min 3000 limit))
         level_query)

(** Fetch /api/v1/dashboard/tools, optionally including one Keeper's exact
    effective turn surface beside the global registered catalog. *)
let fetch_dashboard_tools ~(host : string) ~(port : int) ?keeper () :
    (Yojson.Safe.t, string) result =
  let path =
    match keeper with
    | None -> "/api/v1/dashboard/tools"
    | Some keeper_name ->
        "/api/v1/dashboard/tools?keeper="
        ^ percent_encode_query_value keeper_name
  in
  get_json ~host ~port ~path

(** Fetch /api/v1/gate/connectors. *)
let fetch_connectors ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/gate/connectors"

let fetch_connector_names ~(host : string) ~(port : int) ~(connector : string)
    ~(kind : string) ?(offset = 0) ?after_id ?(limit = 500) ?(ids = []) () :
    (Yojson.Safe.t, string) result =
  let exact =
    match ids with
    | [] -> ""
    | ids ->
      "&ids=" ^ percent_encode_query_value (String.concat "," ids)
  in
  let cursor =
    match after_id with
    | None -> ""
    | Some value -> "&after_id=" ^ percent_encode_query_value value
  in
  get_json ~host ~port
    ~path:
      (Printf.sprintf
         "/api/v1/gate/connector/names?name=%s&scope=%s&offset=%d&limit=%d%s%s"
         (percent_encode_query_value connector)
         (percent_encode_query_value kind) offset limit exact cursor)

(** Fetch the workspace skills catalog (/api/v1/skills): per-skill usage
    rows and execution-plan flows for the Tools screen tracking views. *)
let fetch_skills_catalog ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/skills"

(** Fetch the current composite lane snapshot for every registered Keeper. *)
let fetch_keeper_lanes ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/keepers/composite"

(** Fetch the read-only standalone-lane admission and observation matrix. *)
let fetch_standalone_lanes ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/standalone-lanes"

(** Fetch /api/v1/repositories. *)
let fetch_repositories ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/repositories"

(** Fetch the Git working-tree changes for one registered repository. *)
let fetch_repository_changes ~(host : string) ~(port : int)
    ~(repository_id : string) : (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      ("/api/v1/repositories/"
       ^ percent_encode_path_segment repository_id
       ^ "/changes")

(** Fetch /api/v1/dashboard/keeper-memory-health: one fleet snapshot with a
    row per keeper, including keepers with a config but no snapshot — the
    starvation rows this endpoint exists to expose. *)
let fetch_keeper_memory_health ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/keeper-memory-health"

(** Fetch what one keeper remembers, fact by fact
    ([GET /api/v1/keepers/:name/memory-facts]): the ordinary and
    source-bound stores, each answered independently. *)
let fetch_keeper_memory_facts ~(host : string) ~(port : int)
    ~(keeper_name : string) : (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      ("/api/v1/keepers/"
       ^ percent_encode_path_segment keeper_name
       ^ "/memory-facts")

(** Fetch Git working-tree changes for the current project workspace. *)
let fetch_project_changes ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/git/status"

(** Fetch /api/v1/dashboard/harness-health. No window is passed: the surface
    shows what the harness decided recently, and a window is a question an
    operator asks in the dashboard rather than a default. *)
let fetch_harness_health ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/harness-health"

(** Fetch the retained Fusion run registry list. *)
let fetch_fusion_runs ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:fusion_runs_path

(** Fetch one run joined to its exact typed-origin Board evidence. *)
let fetch_fusion_detail ~(host : string) ~(port : int) ~(run_id : string) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:(fusion_runs_path ^ "/" ^ percent_encode_path_segment run_id)

(** Fetch /api/v1/verification/requests. [limit] bounds the page; the surface
    lists what is waiting rather than the whole history. *)
let fetch_verification_requests ~(host : string) ~(port : int) ~(limit : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:(Printf.sprintf "/api/v1/verification/requests?limit=%d" (max 1 limit))

(** Fetch /api/v1/dashboard/planning (goals + rollup + task backlog). *)
let fetch_dashboard_planning ~(host : string) ~(port : int) : (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/dashboard/planning"

(** Fetch /health for the server's own identity: version, the commit its
    binary was built from, and the paths it resolved. The probe shape carries
    all three, so this does not pay for [full=1]. *)
let fetch_server_identity ~(host : string) ~(port : int) : (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/health"

(** Fetch the operator fleet reading from /health?full=1.

    The operator snapshot does not carry it: [keeper_fleet_safety] is assembled
    in lib/server from a scan the operator projection has no path to, and the
    dependency runs server -> operator, not back. So this reads the health
    surface the dashboard already reads for the same facts.

    [full=1] is the only shape that carries the section. It is a wider payload
    than the fleet reading alone, which is why the caller polls it on the fleet
    view rather than on every tick. *)
let fetch_fleet_safety ~(host : string) ~(port : int) : (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/health?full=1"

(** GET /api/v1/keepers/:name/config — name, instructions, effective_config,
    sources. Read here for the detail pane's Instructions tab. *)
let fetch_keeper_config_snapshot ~(host : string) ~(port : int)
    ~(keeper_name : string) : (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/keepers/%s/config"
         (percent_encode_path_segment keeper_name))

(** GET /api/v1/gate/keeper-status?name=... — the selected Keeper's detailed
    status, including the server-observed [sandbox_live] projection. *)
let fetch_keeper_status_snapshot ~(host : string) ~(port : int)
    ~(keeper_name : string) : (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/gate/keeper-status?name=%s"
         (percent_encode_query_value keeper_name))

let fetch_keeper_sandbox_logs ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(tail : int) : (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/gate/keeper-sandbox-logs?name=%s&tail=%d"
         (percent_encode_query_value keeper_name) tail)

(** GET /api/v1/keepers/:name/github-identity — the keeper's GitHub CLI
    identity observation (config dir, projected token env, stored and
    effective auth). *)
let fetch_keeper_github_identity ~(host : string) ~(port : int)
    ~(keeper_name : string) : (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/keepers/%s/github-identity"
         (percent_encode_path_segment keeper_name))

(** GET /api/v1/keepers/oauth/attached-tools — every declared service and
    what it currently offers this Keeper. *)
let fetch_attached_tools ~(host : string) ~(port : int) ~(keeper_name : string) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/keepers/oauth/attached-tools?keeper=%s"
         (percent_encode_path_segment keeper_name))

(** POST /api/v1/keepers/:name/identity-refresh — ask an attached service
    again what tools it has. *)
let post_keeper_identity_refresh ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(provider_id : string) :
    (Yojson.Safe.t, string) result =
  post_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/keepers/%s/identity-refresh"
         (percent_encode_path_segment keeper_name))
    ~body:(Yojson.Safe.to_string (`Assoc [ "provider", `String provider_id ]))

(** POST /api/v1/keepers/:name/oauth-login — begin attaching this Keeper to
    [provider_id]. Answers with the URL the operator has to open; nothing is
    written to the Keeper until the browser comes back. *)
let post_keeper_oauth_login ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(provider_id : string) :
    (Yojson.Safe.t, string) result =
  post_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/keepers/%s/oauth-login"
         (percent_encode_path_segment keeper_name))
    ~body:
      (Yojson.Safe.to_string (`Assoc [ "provider", `String provider_id ]))

(** POST /api/v1/keepers/oauth/client — record an app the operator made.

    Not keeper-scoped, because the client belongs to the install: the path
    says so and the server stores it under the provider's client group. *)
let post_keeper_oauth_client ~(host : string) ~(port : int)
    ~(provider_id : string) ~(client_id : string) ~(client_secret : string)
    ~(scopes : string) : (Yojson.Safe.t, string) result =
  post_json ~host ~port ~path:"/api/v1/keepers/oauth/client"
    ~body:
      (Yojson.Safe.to_string
         (`Assoc
           [ "provider", `String provider_id
           ; "client_id", `String client_id
           ; "client_secret", `String client_secret
           ; "scopes", `String scopes
           ]))

(** GET /api/v1/runtime/config/raw — runtime.toml's path and text as the
    server reads them. *)
let fetch_runtime_config_raw ~(host : string) ~(port : int) :
    (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/runtime/config/raw"

(** POST /api/v1/runtime/config/raw/preview — validate edited text without
    writing it. The body names the one field the route reads. *)
let post_runtime_config_preview ~(host : string) ~(port : int)
    ~(source_text : string) : (Yojson.Safe.t, string) result =
  post_json ~host ~port ~path:"/api/v1/runtime/config/raw/preview"
    ~body:(Yojson.Safe.to_string (`Assoc [ ("source_text", `String source_text) ]))

(** POST /api/v1/runtime/config/raw — write the edited text. Callers go
    through the preview first; this route also validates, so a race still
    fails closed. *)
let post_runtime_config_raw ~(host : string) ~(port : int)
    ~(source_text : string) : (runtime_config_commit_receipt, string) result =
  match
    post_json ~host ~port ~path:"/api/v1/runtime/config/raw"
      ~body:(Yojson.Safe.to_string (`Assoc [ ("source_text", `String source_text) ]))
  with
  | Error _ as error -> error
  | Ok json -> decode_runtime_config_commit_receipt json

type skill_editor_loaded =
  { sel_reference : Skill_reference.t
  ; sel_source_text : string
  ; sel_access : string
  ; sel_snapshot_revision : string
  }

type skill_editor_save_status =
  | Skill_unchanged
  | Skill_saved_and_published
  | Skill_saved_but_unpublished of string

type skill_editor_save_receipt =
  { ses_status : skill_editor_save_status
  ; ses_reference : Skill_reference.t
  ; ses_snapshot_revision : string option
  }

let skill_editor_body reference source_text =
  `Assoc
    ([ "reference", Skill_reference.to_yojson reference ]
     @
     match source_text with
     | None -> []
     | Some text -> [ "source_text", `String text ])
  |> Yojson.Safe.to_string
;;

let decode_skill_editor_loaded = function
  | `Assoc fields ->
    (match
       List.assoc_opt "reference" fields,
       List.assoc_opt "source_text" fields,
       List.assoc_opt "access" fields,
       List.assoc_opt "snapshot_revision" fields
     with
     | Some reference_json, Some (`String source_text), Some (`String access),
       Some (`String snapshot_revision) ->
       (match Skill_reference.of_yojson reference_json with
        | Ok reference ->
          Ok
            { sel_reference = reference
            ; sel_source_text = source_text
            ; sel_access = access
            ; sel_snapshot_revision = snapshot_revision
            }
        | Error _ -> Error "Skill editor returned an invalid exact reference")
     | _ -> Error "Skill editor read response is incomplete")
  | _ -> Error "Skill editor read response must be an object"
;;

let post_skill_editor_read ~host ~port reference =
  match
    post_json
      ~host
      ~port
      ~path:"/api/v1/skills/editor/read"
      ~body:(skill_editor_body reference None)
  with
  | Error _ as error -> error
  | Ok json -> decode_skill_editor_loaded json
;;

let post_skill_editor_preview ~host ~port ~reference ~source_text =
  post_json
    ~host
    ~port
    ~path:"/api/v1/skills/editor/preview"
    ~body:(skill_editor_body reference (Some source_text))
;;

let decode_skill_editor_save_receipt = function
  | `Assoc fields ->
    let snapshot_revision =
      match List.assoc_opt "snapshot_revision" fields with
      | Some (`String value) -> Some value
      | Some _ | None -> None
    in
    let reason =
      match List.assoc_opt "reason" fields with
      | Some (`String value) -> value
      | Some _ | None -> "publication did not complete"
    in
    let status =
      match List.assoc_opt "status" fields with
      | Some (`String "unchanged") -> Ok Skill_unchanged
      | Some (`String "saved_and_published") -> Ok Skill_saved_and_published
      | Some (`String "saved_but_unpublished") ->
        Ok (Skill_saved_but_unpublished reason)
      | Some (`String unknown) -> Error ("unknown Skill save status: " ^ unknown)
      | Some _ | None -> Error "Skill save status is missing"
    in
    let reference =
      match List.assoc_opt "preview" fields with
      | Some (`Assoc preview_fields) ->
        (match List.assoc_opt "reference" preview_fields with
         | Some json -> Skill_reference.of_yojson json
         | None -> Error (Skill_reference.Missing_field
                            { object_name = "preview"; field = "reference" }))
      | Some _ | None ->
        Error
          (Skill_reference.Missing_field
             { object_name = "Skill save response"; field = "preview" })
    in
    (match status, reference with
     | Ok ses_status, Ok ses_reference ->
       Ok { ses_status; ses_reference; ses_snapshot_revision = snapshot_revision }
     | Error detail, _ -> Error detail
     | _, Error _ -> Error "Skill save response reference is invalid")
  | _ -> Error "Skill save response must be an object"
;;

let post_skill_editor_save ~host ~port ~reference ~source_text =
  match
    post_json
      ~host
      ~port
      ~path:"/api/v1/skills/editor/save"
      ~body:(skill_editor_body reference (Some source_text))
  with
  | Error _ as error -> error
  | Ok json -> decode_skill_editor_save_receipt json
;;

let fetch_skill_editor_sources ~host ~port =
  match get_json ~host ~port ~path:"/api/v1/skills/editor/sources" with
  | Error _ as error -> error
  | Ok (`Assoc fields) ->
    (match List.assoc_opt "sources" fields with
     | Some (`List values) ->
       values
       |> List.fold_left
            (fun acc value ->
               match acc, value with
               | Error _ as error, _ -> error
               | Ok sources, `Assoc fields ->
                 (match List.assoc_opt "source_id" fields with
                  | Some (`String source_id) -> Ok (source_id :: sources)
                  | _ -> Error "Skill source entry is incomplete")
               | Ok _, _ -> Error "Skill source entry must be an object")
            (Ok [])
       |> Result.map List.rev
     | _ -> Error "Skill editor sources response is incomplete")
  | Ok _ -> Error "Skill editor sources response must be an object"
;;

let post_skill_editor_create ~host ~port ~source_id ~package_id ~source_text =
  post_json
    ~host
    ~port
    ~path:"/api/v1/skills/editor/create"
    ~body:
      (Yojson.Safe.to_string
         (`Assoc
           [ "source_id", `String source_id
           ; "package_id", `String package_id
           ; "source_text", `String source_text
           ]))
;;

let post_skill_evidence ~host ~port reference =
  post_json
    ~host
    ~port
    ~path:"/api/v1/skills/evidence"
    ~body:(skill_editor_body reference None)
;;

let fetch_async_request_observation ~host ~port =
  get_json ~host ~port ~path:"/api/v1/async-requests"
;;

(** GET /api/v1/prompts — every prompt the registry serves, with the file
    value, any override, and what is currently effective. *)
let fetch_prompts ~(host : string) ~(port : int) : (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/prompts"

(** POST /api/v1/prompts — body {key, action, value}. [action] is ["set"] or
    ["clear"]; the server persists the override and answers what it did. *)
let post_prompt_override ~(host : string) ~(port : int) ~(key : string)
    ~(value : string) : (Yojson.Safe.t, string) result =
  post_json ~host ~port ~path:"/api/v1/prompts"
    ~body:
      (Yojson.Safe.to_string
         (`Assoc
            [ ("key", `String key)
            ; ("action", `String "set")
            ; ("value", `String value)
            ]))

let post_prompt_clear ~(host : string) ~(port : int) ~(key : string)
    : (Yojson.Safe.t, string) result =
  post_json ~host ~port ~path:"/api/v1/prompts"
    ~body:
      (Yojson.Safe.to_string
         (`Assoc [ ("key", `String key); ("action", `String "clear") ]))

(** GET /api/v1/presets — the prompt presets under <base>/.masc/presets, as
    manifests, plus the directories whose manifest did not read. *)
let fetch_presets ~(host : string) ~(port : int) : (Yojson.Safe.t, string) result =
  get_json ~host ~port ~path:"/api/v1/presets"

(** GET /api/v1/presets/show — everything the named preset would change: the
    override text, the keeper instructions, the assignments and the lanes.
    The listing counts and names; this is what the preset actually holds. *)
let fetch_preset_detail ~(host : string) ~(port : int) ~(name : string)
  : (Yojson.Safe.t, string) result
  =
  get_json ~host ~port
    ~path:("/api/v1/presets/show?name=" ^ Uri.pct_encode ~component:`Query_value name)

(** POST /api/v1/presets — body {name, description}: snapshot the live
    prompt overrides, keeper instructions and runtime routing under [name]. *)
let post_preset_save ~(host : string) ~(port : int) ~(name : string)
    ~(description : string) : (Yojson.Safe.t, string) result =
  post_json ~host ~port ~path:"/api/v1/presets"
    ~body:
      (Yojson.Safe.to_string
         (`Assoc [ ("name", `String name); ("description", `String description) ]))

(** POST /api/v1/presets/restore — body {name}: the server autosaves the live
    state, applies the preset surface by surface, and answers a report. *)
(* A restore is a write, so a transport failure is not a refusal: the server
   may have finished it. The two are separated here because only this layer
   knows which one happened, and the operator's next move differs — a refusal
   is retried, an unknown outcome is read off the preset list first. *)
let post_preset_restore ~(host : string) ~(port : int) ~(name : string)
    : (Yojson.Safe.t, [ `Refused of string | `Unknown_outcome of string ]) result =
  match
    http_post_with_timeout ~timeout_sec:preset_restore_timeout_sec
      ~headers:(auth_headers ()) ~host ~port ~path:"/api/v1/presets/restore"
      ~body:(Yojson.Safe.to_string (`Assoc [ ("name", `String name) ]))
  with
  | Error transport -> Error (`Unknown_outcome transport)
  | Ok (status_code, body) -> (
    match decode_json ~allow_empty:true ~status_code ~body with
    | Ok json -> Ok json
    | Error message -> Error (`Refused message))

(** POST /api/v1/gate/connector/bind?name= — body {channel_id, keeper_name}. *)
let post_connector_bind ~(host : string) ~(port : int) ~(connector : string)
    ~(body_json : string) : (Yojson.Safe.t, string) result =
  post_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/gate/connector/bind?name=%s"
         (percent_encode_path_segment connector))
    ~body:body_json

(** POST /api/v1/gate/connector/unbind?name= — body {channel_id}. *)
let post_connector_unbind ~(host : string) ~(port : int) ~(connector : string)
    ~(body_json : string) : (Yojson.Safe.t, string) result =
  post_json ~host ~port
    ~path:
      (Printf.sprintf "/api/v1/gate/connector/unbind?name=%s"
         (percent_encode_path_segment connector))
    ~body:body_json

(** One [resources/list] over the MCP endpoint, on an open session. *)
let call_mcp_resources_list ~(host : string) ~(port : int)
    ~(session_id : string) ~(request_id : string) :
    (Masc_tui_mcp.resource list, string) result =
  let headers =
    json_headers
      (("Accept", "application/json, text/event-stream")
      :: ("Mcp-Session-Id", sanitize_header_value session_id)
      :: auth_headers ())
  in
  let body = Masc_tui_mcp.resources_list_request_body ~request_id in
  match http_post ~headers ~host ~port ~path:mcp_path ~body with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "resources/list returned %d: %s" status body)
  | Ok (_, body) -> Masc_tui_mcp.resources_of_body ~request_id body

(** One [resources/read] over the MCP endpoint, on an open session. *)
let call_mcp_resources_read ~(host : string) ~(port : int)
    ~(session_id : string) ~(request_id : string) ~(uri : string) :
    (Masc_tui_mcp.resource_content list, string) result =
  let headers =
    json_headers
      (("Accept", "application/json, text/event-stream")
      :: ("Mcp-Session-Id", sanitize_header_value session_id)
      :: auth_headers ())
  in
  let body = Masc_tui_mcp.resources_read_request_body ~request_id ~uri in
  match http_post ~headers ~host ~port ~path:mcp_path ~body with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "resources/read returned %d: %s" status body)
  | Ok (_, body) -> Masc_tui_mcp.resource_contents_of_body ~request_id body

(** POST /api/v1/keepers/:name/github-login — the device-flow login as the
    server streams it: gh's own (redacted) output, then an error or the
    final identity observation. Every chunk reaches [on_chunk] as it
    arrives; the return says only how the stream ended. *)
let post_keeper_github_login_streaming ~clock ~(host : string) ~(port : int)
    ~(keeper_name : string) ~(on_chunk : string -> unit) :
    (unit, string) result =
  let url =
    url_of ~host ~port
      ~path:
        (Printf.sprintf "/api/v1/keepers/%s/github-login"
           (percent_encode_path_segment keeper_name))
  in
  let headers =
    json_headers (("Accept", "text/event-stream") :: auth_headers ())
  in
  match
    Masc_http_client.post_stream ~clock ~idle_timeout_sec:900.0 ~url ~headers
      ~body:"{}" ~on_chunk ()
  with
  | Error detail -> Error detail
  | Ok (Masc_http_client.Pool.Buffered { status; body; _ }) ->
      Error (Printf.sprintf "github-login returned %d: %s" status body)
  | Ok (Masc_http_client.Pool.Streamed _) -> Ok ()

(** Fetch what the working tree holds for one file ([GET /api/v1/git/diff]).

    The other half of the diff story. A file change says what a keeper tried
    to write and may carry producer-recorded ranges for that completed call.
    This says what is in the tree now, with per-row numbers git computed.

    [keeper] names whose playground the path is read under, and the path is
    relative to that playground -- the same address the Changes surface
    already shows. Without a keeper the server reads the project checkout. *)
let fetch_git_diff ?repo ~(host : string) ~(port : int)
    ~(keeper : string option) ~(path : string) ~(base_ref : string) () :
    (Masc.Tui_decode.git_diff, string) result =
  let query =
    [ Some (Printf.sprintf "path=%s" (percent_encode_query_value path))
    ; Some (Printf.sprintf "base_ref=%s" (percent_encode_query_value base_ref))
    ; Option.map
        (fun name ->
          Printf.sprintf "keeper=%s" (percent_encode_query_value name))
        keeper
    ; Option.map
        (fun repo_id ->
          Printf.sprintf "repo_id=%s" (percent_encode_query_value repo_id))
        repo
    ]
    |> List.filter_map Fun.id
    |> String.concat "&"
  in
  let request_path = "/api/v1/git/diff?" ^ query in
  match http_get ~host ~port ~path:request_path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status)
    ->
      Error (Printf.sprintf "git diff returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json -> Masc.Tui_decode.decode_git_diff json
      | exception Yojson.Json_error detail ->
          Error ("git diff was not JSON: " ^ detail))

(** The questions one keeper is waiting on the operator for
    ([GET /api/v1/keepers/asks]).

    Open questions only. The rows carry choice ids next to labels and
    {!submit_keeper_ask_answer} takes ids back, so nothing on this side ever
    matches a choice by its wording. *)
let fetch_keeper_asks ?keeper_name ~(host : string) ~(port : int) () :
    (Masc.Tui_decode.asks_snapshot, string) result =
  (* No keeper named means the whole fleet. An operator opening this surface
     does not know which Keeper is stuck yet, and asking them to pick a name
     first is asking them to guess. *)
  let path =
    match keeper_name with
    | None -> "/api/v1/keepers/asks"
    | Some name ->
        Printf.sprintf "/api/v1/keepers/asks?name=%s" (percent_encode_query_value name)
  in
  match http_get ~host ~port ~path with
  | Error detail -> Error detail
  | Ok (status, body) when not (Masc.Tui_decode.is_success_http_status status) ->
      Error (Printf.sprintf "asks returned %d: %s" status body)
  | Ok (_, body) -> (
      match Yojson.Safe.from_string body with
      | json -> Masc.Tui_decode.decode_asks_snapshot json
      | exception Yojson.Json_error detail -> Error ("asks were not JSON: " ^ detail))

(** Answer one question of one ask ([POST /api/v1/keepers/ask-answer]).

    A [409] is not a transport failure: another surface answered first. The
    body carries what landed, and the caller surfaces that rather than
    retrying — resubmitting would only lose again, and the operator needs to
    see the decision that stands. *)
let submit_keeper_ask_answer ~(host : string) ~(port : int) ~(keeper_name : string)
    ~(ask_id : string) ~(question_id : string) ~(choice_ids : string list) :
    (unit, string) result =
  let body =
    Yojson.Safe.to_string
      (`Assoc
        [
          ("name", `String keeper_name);
          ("ask_id", `String ask_id);
          ( "answers",
            `List
              [
                `Assoc
                  [
                    ("question_id", `String question_id);
                    ( "response",
                      `Assoc
                        [
                          ("kind", `String "chose");
                          ( "choice_ids",
                            `List (List.map (fun id -> `String id) choice_ids) );
                        ] );
                  ];
              ] );
        ])
  in
  match
    http_post ~headers:(auth_headers ()) ~host ~port ~path:"/api/v1/keepers/ask-answer" ~body
  with
  | Error detail -> Error detail
  | Ok (status, response_body) when Masc.Tui_decode.is_success_http_status status ->
      let (_ : string) = response_body in
      Ok ()
  | Ok (409, response_body) ->
      Error (Printf.sprintf "another surface answered first: %s" response_body)
  | Ok (status, response_body) ->
      Error (Printf.sprintf "answer returned %d: %s" status response_body)
