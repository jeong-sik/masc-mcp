open Alcotest
open Masc

let shell_quote value =
  "'" ^ String.concat "'\"'\"'" (String.split_on_char '\'' value) ^ "'"
;;

let auth_subscription =
  {|{"loggedIn":true,"authMethod":"claude.ai","subscriptionType":"team","apiProvider":"firstParty"}|}
;;

let assistant =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-fixture-1","message":{"role":"assistant","model":"claude-fixture","content":[{"type":"text","text":"MASC_CLAUDE_OK"}]}}|}
;;

let empty_assistant =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-empty-1","message":{"role":"assistant","model":"claude-fixture","content":[]}}|}
;;

let native_tool_assistant =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-native-1","message":{"role":"assistant","model":"claude-fixture","content":[{"type":"tool_use","id":"native-call-1","name":"Read","input":{"file_path":"/tmp/fixture"}}]}}|}
;;

let native_tool_result =
  {|{"type":"user","session_id":"__SESSION__","uuid":"user-native-1","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"native-call-1","content":"fixture"}]}}|}
;;

let tool_progress =
  {|{"type":"tool_progress","session_id":"__SESSION__","uuid":"tool-progress-1","tool_use_id":"native-call-1","tool_name":"Bash","elapsed_time_seconds":240}|}
;;

let result =
  {|{"type":"result","subtype":"success","is_error":false,"session_id":"__SESSION__","uuid":"turn-fixture-1","result":"MASC_CLAUDE_OK","api_error_status":null}|}
;;

let result_with_usage =
  {|{"type":"result","subtype":"success","is_error":false,"session_id":"__SESSION__","uuid":"turn-usage-1","result":"MASC_CLAUDE_OK","api_error_status":null,"usage":{"input_tokens":123456,"output_tokens":789,"cache_read_input_tokens":42}}|}
;;

(* Three assistant frames of one turn. The first two share a message id,
   the shape parallel tool calls produce, so their usage must count once. *)
let assistant_with_usage_a =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-usage-a1","message":{"id":"msg-usage-a","role":"assistant","model":"claude-fixture","content":[{"type":"text","text":"first"}],"usage":{"input_tokens":100,"output_tokens":10}}}|}
;;

let assistant_with_usage_a_sibling =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-usage-a2","message":{"id":"msg-usage-a","role":"assistant","model":"claude-fixture","content":[{"type":"text","text":"first again"}],"usage":{"input_tokens":100,"output_tokens":10}}}|}
;;

let assistant_with_usage_b =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-usage-b1","message":{"id":"msg-usage-b","role":"assistant","model":"claude-fixture","content":[{"type":"text","text":"second"}],"usage":{"input_tokens":200,"output_tokens":20,"cache_read_input_tokens":5}}}|}
;;

(* A frame the CLI emits without a message id cannot be matched to a
   sibling, so it counts on its own. *)
let assistant_with_usage_no_id =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-usage-c1","message":{"role":"assistant","model":"claude-fixture","content":[{"type":"text","text":"third"}],"usage":{"input_tokens":1,"output_tokens":1}}}|}
;;

let result_with_partial_usage =
  {|{"type":"result","subtype":"success","is_error":false,"session_id":"__SESSION__","uuid":"turn-usage-2","result":"MASC_CLAUDE_OK","api_error_status":null,"usage":{"output_tokens":789}}|}
;;

type fixture_step =
  | Emit of string
  | Emit_and_read of string
  | Emit_and_expect_request_id of string * string
  | Emit_after_closing_input of string
  | Expect_user_message_contains of string
    (* Assert against the user message the CLI actually received on stdin. The
       fixture exits 97 when the needle is absent, so a turn that loses a block
       between the caller and the wire fails the test instead of passing on a
       reply the fixture would have emitted anyway. *)
  | Pause of float

let fixture_script
      ?(auth_json = auth_subscription)
      ?(before_initialize_response = [])
      steps
  =
  let path = Filename.temp_file "masc-claude-code-" ".sh" in
  let output = open_out_bin path in
  let write_step = function
    | Emit line -> output_string output ("emit " ^ shell_quote line ^ "\n")
    | Emit_and_read line ->
      output_string output ("emit " ^ shell_quote line ^ "\n");
      output_string output "IFS= read -r ignored_response\n"
    | Emit_and_expect_request_id (line, request_id) ->
      output_string output ("emit " ^ shell_quote line ^ "\n");
      output_string output "IFS= read -r control_response\n";
      output_string output
        (Printf.sprintf
           "printf '%%s' \"$control_response\" | grep -F '\"request_id\":\"%s\"' >/dev/null || exit 96\n"
           request_id)
    | Emit_after_closing_input line ->
      output_string output "exec 0<&-\n";
      output_string output ("emit " ^ shell_quote line ^ "\n")
    | Expect_user_message_contains needle ->
      output_string output
        (Printf.sprintf
           "printf '%%s' \"$user_message\" | grep -F %s >/dev/null || exit 97\n"
           (shell_quote needle))
    | Pause seconds ->
      output_string output (Printf.sprintf "sleep %.3f\n" seconds)
  in
  output_string output "#!/bin/sh\n";
  output_string output "set -eu\n";
  List.iter
    (fun name ->
       output_string output
         (Printf.sprintf "[ \"${%s+x}\" != x ] || exit 90\n" name))
    [ "ANTHROPIC_API_KEY"
    ; "ANTHROPIC_AUTH_TOKEN"
    ; "ANTHROPIC_API_URL"
    ; "ANTHROPIC_BASE_URL"
    ; "ANTHROPIC_BEDROCK_BASE_URL"
    ; "ANTHROPIC_VERTEX_BASE_URL"
    ; "ANTHROPIC_VERTEX_PROJECT_ID"
    ; "CLAUDE_CODE_OAUTH_TOKEN"
    ; "CLAUDE_CODE_SKIP_BEDROCK_AUTH"
    ; "CLAUDE_CODE_SKIP_VERTEX_AUTH"
    ; "CLAUDE_CODE_USE_BEDROCK"
    ; "CLAUDE_CODE_USE_VERTEX"
    ; "CLOUD_ML_REGION"
    ; "MASC_CLAUDE_SECRET_CANARY"
    ];
  output_string output "[ -n \"${HOME-}\" ] || exit 91\n";
  output_string output "[ -n \"${USER-}\" ] || exit 97\n";
  output_string output
    "[ \"${CLAUDE_CODE_ENTRYPOINT-}\" = masc ] || exit 92\n";
  output_string output
    "[ \"${CLAUDE_AGENT_SDK_VERSION-}\" = masc-ocaml ] || exit 93\n";
  output_string output "if [ \"${1-}\" = auth ]; then\n";
  output_string output
    ("  printf '%s\\n' " ^ shell_quote auth_json ^ "\n");
  output_string output "  exit 0\n";
  output_string output "fi\n";
  output_string output "session=''\n";
  output_string output "for arg in \"$@\"; do\n";
  output_string output "  case \"$arg\" in\n";
  output_string output "    --session-id=*) session=${arg#--session-id=} ;;\n";
  output_string output "    --resume=*) session=${arg#--resume=} ;;\n";
  output_string output "  esac\n";
  output_string output "done\n";
  output_string output "[ -n \"$session\" ] || exit 94\n";
  output_string output "emit() { printf '%s\\n' \"$1\" | sed \"s/__SESSION__/$session/g\"; }\n";
  output_string output "IFS= read -r initialize\n";
  output_string output
    "request_id=$(printf '%s' \"$initialize\" | sed -n 's/.*\"request_id\":\"\\([^\"]*\\)\".*/\\1/p')\n";
  output_string output "[ -n \"$request_id\" ] || exit 95\n";
  List.iter write_step before_initialize_response;
  output_string output
    "printf '{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"%s\",\"response\":{}}}\\n' \"$request_id\"\n";
  output_string output "IFS= read -r user_message\n";
  List.iter write_step steps;
  output_string output "while IFS= read -r ignored; do :; done\n";
  close_out output;
  Unix.chmod path 0o700;
  path
;;

let with_fixture ?auth_json ?before_initialize_response steps f =
  let path = fixture_script ?auth_json ?before_initialize_response steps in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)
;;

(* Two kinds of deadline live in this suite and they want opposite things.

   A deadline a test asserts *fires* wants to be short: the fixture pauses
   past it on purpose and the timeout is the result. 0.05 s is right there.

   A deadline a test needs to *not* fire has to cover starting the fixture as
   well as the pause it is compared against. Spawning the shell alone measured
   p50 12 ms with a 409 ms tail under load on this repo's machine, so 0.05 s
   expired before the script reached its first [read] and the case failed
   about half the time -- which is not what any of these are asserting.

   Growing the pauses alongside was tried and made it worse: the suite got
   slower, which lengthened the very spawn tail being worked around, and the
   failure moved to whichever neighbour happened to be unlucky. So the pauses
   stay as they are and only the windows that must survive a process start
   carry this name.

   The default stays 2 s. Raising it was tried and inverted the suite: the
   default also feeds the cases that assert a deadline *does* fire, and a
   5 s default swallowed the 0.2 s pause those wait through, so
   [test_no_deadline_keeps_initialize_bounded] failed 12 times out of 12. The
   two kinds cannot share one number. *)
let window_outlasting_process_start_s = 5.0

let run_fixture ?(dynamic_tools = []) ?session_mode ?(timeout_s = 2.0)
    ?admission_timeout_s ?(no_turn_deadline = false) ?on_session_ready_delay_s
    ?on_turn_started_delay_s ?on_stream_event ?(images = []) path =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    let config =
      { (Runtime_claude_code.default_config ~cwd:"/tmp") with
        cli_path = path
      ; admission_timeout_s = Option.value admission_timeout_s ~default:timeout_s
      ; timeout_s = if no_turn_deadline then None else Some timeout_s
      }
    in
    let on_session_ready =
      Option.map
        (fun delay_s ~session_id:_ ->
           Eio.Time.sleep clock delay_s;
           Ok ())
        on_session_ready_delay_s
    in
    let on_turn_started =
      Option.map
        (fun delay_s ~session_id:_ ~turn_id:_ ->
           Eio.Time.sleep clock delay_s;
           Ok ())
        on_turn_started_delay_s
    in
    Runtime_claude_code.run_turn
      ~mgr:(Eio.Stdenv.process_mgr env)
      ~clock
      ~cwd:Eio.Path.(Eio.Stdenv.fs env / "/tmp")
      ~dynamic_tools
      ?session_mode
      ?on_session_ready
      ?on_turn_started
      ?on_stream_event
      config
      ~prompt:"Return the fixture marker"
      ~images)
;;

let test_validation_is_process_free () =
  let config =
    { (Runtime_claude_code.default_config ~cwd:"/tmp") with cli_path = "" }
  in
  match Runtime_claude_code.validate_turn config ~prompt:"fixture" ~images:[] with
  | Error (Runtime_claude_code.Invalid_config "cli_path must not be empty") -> ()
  | Error error -> fail (Runtime_claude_code.error_to_string error)
  | Ok () -> fail "invalid Claude Code config passed admission"
;;

let test_subscription_turn_and_env_scrub () =
  List.iter
    (fun name -> Unix.putenv name "hostile-fixture-value")
    [ "ANTHROPIC_API_KEY"
    ; "ANTHROPIC_AUTH_TOKEN"
    ; "ANTHROPIC_API_URL"
    ; "ANTHROPIC_BASE_URL"
    ; "ANTHROPIC_BEDROCK_BASE_URL"
    ; "ANTHROPIC_VERTEX_BASE_URL"
    ; "ANTHROPIC_VERTEX_PROJECT_ID"
    ; "CLAUDE_CODE_OAUTH_TOKEN"
    ; "CLAUDE_CODE_SKIP_BEDROCK_AUTH"
    ; "CLAUDE_CODE_SKIP_VERTEX_AUTH"
    ; "CLAUDE_CODE_USE_BEDROCK"
    ; "CLAUDE_CODE_USE_VERTEX"
    ; "CLOUD_ML_REGION"
    ; "MASC_CLAUDE_SECRET_CANARY"
    ];
  with_fixture [ Emit assistant; Emit result ] (fun path ->
    match run_fixture path with
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok turn ->
      check string "text" "MASC_CLAUDE_OK" turn.text;
      check string "turn" "turn-fixture-1" turn.turn_id;
      check string "model" "claude-fixture" turn.model;
      check string "subscription" "team" turn.subscription.subscription_type;
      check bool "new session" false turn.resumed;
      check bool "no usage block yields none" true (Option.is_none turn.usage))
;;

let test_progress_resets_stream_idle_timeout () =
  with_fixture
    [ Emit assistant; Pause 0.4; Emit assistant; Pause 0.4; Emit result ]
    (fun path ->
       match run_fixture ~timeout_s:window_outlasting_process_start_s path with
       | Ok turn ->
         check string "progressing turn completes" "MASC_CLAUDE_OK" turn.text
       | Error error -> fail (Runtime_claude_code.error_to_string error))
;;

let test_stream_idle_timeout_is_typed () =
  with_fixture [ Pause 0.2; Emit assistant; Emit result ] (fun path ->
    match run_fixture ~timeout_s:0.05 path with
    | Error (Runtime_claude_code.Timeout seconds) ->
      check (float 0.001) "exact idle timeout" 0.05 seconds
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "silent Claude stream ignored its idle timeout")
;;

let test_no_deadline_keeps_initialize_bounded () =
  with_fixture
    ~before_initialize_response:[ Pause 0.2 ]
    [ Emit assistant; Emit result ]
    (fun path ->
       match
         run_fixture
           ~admission_timeout_s:0.05
           ~no_turn_deadline:true
           path
       with
       | Error (Runtime_claude_code.Timeout seconds) ->
         check (float 0.001) "admission timeout" 0.05 seconds
       | Error error -> fail (Runtime_claude_code.error_to_string error)
       | Ok _ -> fail "an unbounded turn disabled Claude initialization bounds")
;;

(* The image has to reach the CLI's stdin, not merely be accepted by the caller.
   The fixture greps the line it read and exits 97 when the block is absent, so
   a projection that quietly drops the image fails here instead of returning the
   reply the fixture would have emitted regardless. *)
let test_image_reaches_the_cli_user_message () =
  let image =
    { Runtime_claude_code.media_type = "image/png"
    ; base64_data = Base64.encode_string "\x89PNG\r\n\x1a\nfixture"
    }
  in
  with_fixture
    [ Expect_user_message_contains {|"type":"image"|}
    ; Expect_user_message_contains {|"media_type":"image/png"|}
    ; Emit assistant
    ; Emit_after_closing_input result
    ]
    (fun path ->
      match run_fixture ~images:[ image ] path with
      | Ok _ -> ()
      | Error error ->
        failwith
          ("image turn failed: " ^ Runtime_claude_code.error_to_string error))
;;

(* A media type the block cannot carry is rejected before the CLI is spawned,
   so the caller learns which image is wrong instead of reading a provider 400
   attributed to the model several seconds later. *)
let test_unsupported_image_media_type_is_rejected () =
  let config = Runtime_claude_code.default_config ~cwd:"/tmp" in
  let image =
    { Runtime_claude_code.media_type = "image/tiff"; base64_data = "AAAA" }
  in
  match Runtime_claude_code.validate_turn config ~prompt:"x" ~images:[ image ] with
  | Ok () -> failwith "an unsupported media type must not validate"
  | Error (Runtime_claude_code.Invalid_config detail) ->
    if not (String_util.contains_substring detail "image/tiff")
    then failwith ("error should name the rejected media type: " ^ detail)
  | Error other ->
    failwith
      ("expected Invalid_config, got " ^ Runtime_claude_code.error_to_string other)
;;

let test_no_deadline_starts_after_user_message () =
  with_fixture
    [ Pause 0.2; Emit assistant; Emit result ]
    (fun path ->
       match
         run_fixture
           ~admission_timeout_s:window_outlasting_process_start_s
           ~no_turn_deadline:true
           path
       with
       | Ok turn -> check string "unbounded result" "MASC_CLAUDE_OK" turn.text
       | Error error -> fail (Runtime_claude_code.error_to_string error))
;;

let test_no_deadline_keeps_turn_started_callback_bounded () =
  with_fixture [ Emit assistant; Emit result ] (fun path ->
    match
      run_fixture
        ~admission_timeout_s:0.05
        ~no_turn_deadline:true
        ~on_turn_started_delay_s:0.2
        path
    with
    | Error (Runtime_claude_code.Timeout seconds) ->
      check (float 0.001) "host callback timeout" 0.05 seconds
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "no-deadline turn made the host state callback unbounded")
;;

let test_state_callback_timeout_is_typed () =
  with_fixture [] (fun path ->
    match
      run_fixture
        ~timeout_s:0.05
        ~on_session_ready_delay_s:0.2
        path
    with
    | Error (Runtime_claude_code.Timeout seconds) ->
      check (float 0.001) "exact callback timeout" 0.05 seconds
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "blocking Claude callback ignored its timeout")
;;

let test_callback_timeout_origin_is_preserved_without_deadline () =
  with_fixture [] (fun path ->
    check_raises
      "callback-origin timeout escapes unchanged"
      Eio.Time.Timeout
      (fun () ->
         Eio_main.run (fun env ->
           let config =
             { (Runtime_claude_code.default_config ~cwd:"/tmp") with
               cli_path = path
             ; timeout_s = None
             }
           in
           Runtime_claude_code.run_turn
             ~mgr:(Eio.Stdenv.process_mgr env)
             ~clock:(Eio.Stdenv.clock env)
             ~cwd:Eio.Path.(Eio.Stdenv.fs env / "/tmp")
             ~on_session_ready:(fun ~session_id:_ -> raise Eio.Time.Timeout)
             config
             ~prompt:"fixture"
             ~images:[]
           |> ignore)))
;;

(* [dynamic_tool_bytes] measures what the tool declarations add to a request.
   The request-side check #27427 needs is a comparison against a declared
   window, and a size that silently ignores part of what it sends is what made
   the old system_and_user_bytes report 6,971 bytes for a turn the provider
   refused as full. Each field the request carries has to be in the sum. *)
let test_dynamic_tool_bytes_counts_every_field () =
  let tool =
    { Runtime_claude_code.name = "ab"
    ; description = "cde"
    ; input_schema = `Assoc [ "f", `String "g" ]
    ; call =
        (fun ~call_id:_ _ ->
          { Runtime_claude_code.success = true; content = ""; abort_turn = None })
    }
  in
  let schema_bytes = String.length (Yojson.Safe.to_string tool.Runtime_claude_code.input_schema) in
  check int "single tool sums name, description and schema"
    (2 + 3 + schema_bytes)
    (Runtime_claude_code.dynamic_tool_bytes [ tool ]);
  check int "empty list is zero" 0 (Runtime_claude_code.dynamic_tool_bytes []);
  check int "two tools sum"
    (2 * (2 + 3 + schema_bytes))
    (Runtime_claude_code.dynamic_tool_bytes [ tool; tool ])
;;

(* The keeper side of this runtime hardcoded [usage = None] while the
   antigravity runtime fills the same slot from its CLI stream, which is why
   official-client turns carry no input_tokens (#28023). Reading it here is what
   lets a turn record the size it actually sent. *)
let test_result_usage_is_carried () =
  with_fixture [ Emit assistant; Emit result_with_usage ] (fun path ->
    match run_fixture path with
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok turn ->
      (match turn.usage with
       | None -> fail "usage block was dropped"
       | Some usage ->
         (* turn_usage keeps the CLI's exclusive wire counts as-is; the
            inclusive normalization happens in the keeper's api_usage
            mapping. The frame's cache_read 42 must survive the parse and
            its absent cache_creation reads as 0. *)
         check int "input tokens" 123456 usage.input_tokens;
         check int "output tokens" 789 usage.output_tokens;
         check int "cache read carried" 42 usage.cache_read_input_tokens;
         check int "absent cache creation is 0" 0 usage.cache_creation_input_tokens))
;;

(* A usage block the CLI shapes differently must not fail the turn: the text is
   the turn's product and the counts are an observation of it. An absent value
   after this lands means the CLI sent nothing usable, which is the measurement
   #27427 needs. *)
let test_partial_result_usage_does_not_fail_the_turn () =
  with_fixture [ Emit assistant; Emit result_with_partial_usage ] (fun path ->
    match run_fixture path with
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok turn ->
      check string "text survives" "MASC_CLAUDE_OK" turn.text;
      check bool "partial usage yields none" true (Option.is_none turn.usage))
;;

let test_non_subscription_auth_is_rejected () =
  let auth_json =
    {|{"loggedIn":true,"authMethod":"apiKey","subscriptionType":"api","apiProvider":"firstParty"}|}
  in
  with_fixture ~auth_json [] (fun path ->
    match run_fixture path with
    | Error (Runtime_claude_code.Subscription_required _) -> ()
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "API-key Claude auth was admitted as subscription")
;;

let test_missing_cli_is_not_reported_as_logout () =
  let outcome =
    Eio_main.run (fun env ->
      let config =
        { (Runtime_claude_code.default_config ~cwd:"/tmp") with
          cli_path = "/definitely/missing/masc-claude-fixture"
        }
      in
      Runtime_claude_code.probe_subscription
        ~mgr:(Eio.Stdenv.process_mgr env)
        ~clock:(Eio.Stdenv.clock env)
        ~cwd:Eio.Path.(Eio.Stdenv.fs env / "/tmp")
        config)
  in
  match outcome with
  | Error (Runtime_claude_code.Spawn_failed _) -> ()
  | Error error -> fail (Runtime_claude_code.error_to_string error)
  | Ok _ -> fail "missing Claude CLI was reported as a valid login"
;;

let rate_limit_rejected =
  {|{"type":"rate_limit_event","session_id":"__SESSION__","uuid":"limit-1","rate_limit_info":{"status":"rejected","rateLimitType":"seven_day","resetsAt":1786356000,"overageStatus":"rejected","overageDisabledReason":"org_level_disabled_until"}}|}
;;

let quota_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-quota-1","result":"not inspected","api_error_status":429,"terminal_reason":"api_error"}|}
;;

let quota_result_with_success_flag =
  {|{"type":"result","subtype":"success","is_error":false,"session_id":"__SESSION__","uuid":"turn-quota-flag-1","result":"must not settle","api_error_status":null}|}
;;

let prompt_too_long_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-400-1","result":"Prompt is too long · the request is ~250000 tokens (limit 200000)","api_error_status":400}|}
;;

let prompt_too_long_statusless_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-statusless-overflow-1","result":"Prompt is too long"}|}
;;

let statusless_overflow_prose_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-statusless-generic-1","result":"gateway diagnostic mentioned Prompt is too long for an unrelated rejection"}|}
;;

let generic_400_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-400-generic","result":"request rejected; diagnostic mentioned Prompt is too long without the provider prefix","api_error_status":400}|}
;;

let prompt_too_long_typed_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-413-1","result":"Input is too long for requested model","api_error_status":413,"terminal_reason":"prompt_too_long"}|}
;;

let non_overflow_reason_with_prefix_prose_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-400-misprose","result":"Prompt is too long · gateway relayed the sentence for an unrelated rejection","api_error_status":400,"terminal_reason":"api_error"}|}
;;

(* 2026-08-14: three live resumed-session overflows arrived as
   [subtype=success, is_error=true] with no [terminal_reason] and no
   [api_error_status] — only the sentence. The frame shape the 400-bound
   fallback missed. *)
let statusless_prompt_too_long_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-nostatus-1","result":"Prompt is too long"}|}
;;

let statusless_input_too_long_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-nostatus-2","result":"Input is too long for requested model"}|}
;;

let statusless_generic_error_result =
  {|{"type":"result","subtype":"success","is_error":true,"session_id":"__SESSION__","uuid":"turn-nostatus-3","result":"stream closed before completion"}|}
;;

(* A live keeper reported Claude's explicit "prompt is too long" terminal as a
   generic 400, so the provider-bound history shrinker never saw the typed
   ContextOverflow it consumes. This fixture carries no [terminal_reason], so
   it exercises the sentence-table fallback: the provider sentence alone is
   the admission evidence, any status; non-prefix prose stays generic below. *)
let test_terminal_prompt_too_long_is_typed () =
  with_fixture [ Emit prompt_too_long_result ] (fun path ->
    match run_fixture path with
    | Error
        (Runtime_claude_code.Context_window_exceeded
          { message; tool_effect_attempted = false; response_emitted = false }) ->
      check
        bool
        "provider reason survives into the typed failure"
        true
        (Astring.String.is_infix ~affix:"Prompt is too long" message);
      check
        bool
        "status code is still reported"
        true
        (Astring.String.is_infix ~affix:"api_status=400" message)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "an is_error terminal was reported as completion")
;;

let test_statusless_canonical_prompt_too_long_is_typed () =
  with_fixture [ Emit prompt_too_long_statusless_result ] (fun path ->
    match run_fixture path with
    | Error
        (Runtime_claude_code.Context_window_exceeded
          { message; tool_effect_attempted = false; response_emitted = false }) ->
      check
        bool
        "canonical provider verdict survives into the typed failure"
        true
        (Astring.String.is_infix ~affix:"Prompt is too long" message);
      check
        bool
        "missing status remains explicit"
        true
        (Astring.String.is_infix ~affix:"api_status=unknown" message)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "a canonical statusless overflow was reported as completion")
;;

let test_statusless_overflow_prose_remains_turn_failed () =
  with_fixture [ Emit statusless_overflow_prose_result ] (fun path ->
    match run_fixture path with
    | Error (Runtime_claude_code.Turn_failed_with_observation { detail; _ }) ->
      check
        bool
        "unrelated statusless rejection keeps its diagnostic"
        true
        (Astring.String.is_infix ~affix:"unrelated rejection" detail)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "unrelated statusless prose was reported as completion")
;;

let test_unrelated_400_remains_turn_failed () =
  with_fixture [ Emit generic_400_result ] (fun path ->
    match run_fixture path with
    | Error (Runtime_claude_code.Turn_failed_with_observation { detail; _ }) ->
      check
        bool
        "generic rejection reason survives"
        true
        (Astring.String.is_infix ~affix:"diagnostic mentioned" detail)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "an unrelated 400 terminal was reported as completion")
;;

(* The 2026-08-14 live shape: is_error with the overflow sentence but no
   [terminal_reason] and no [api_error_status]. The sentence table admits it
   without a status requirement. *)
let test_statusless_prompt_too_long_is_typed () =
  with_fixture [ Emit statusless_prompt_too_long_result ] (fun path ->
    match run_fixture path with
    | Error
        (Runtime_claude_code.Context_window_exceeded
          { message; tool_effect_attempted = false; response_emitted = false }) ->
      check
        bool
        "provider sentence survives into the typed failure"
        true
        (Astring.String.is_infix ~affix:"Prompt is too long" message)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "a statusless overflow terminal was reported as completion")
;;

let test_statusless_input_too_long_is_typed () =
  with_fixture [ Emit statusless_input_too_long_result ] (fun path ->
    match run_fixture path with
    | Error
        (Runtime_claude_code.Context_window_exceeded
          { message; tool_effect_attempted = false; response_emitted = false }) ->
      check
        bool
        "provider sentence survives into the typed failure"
        true
        (Astring.String.is_infix ~affix:"Input is too long" message)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "a statusless overflow terminal was reported as completion")
;;

let test_statusless_generic_error_remains_turn_failed () =
  with_fixture [ Emit statusless_generic_error_result ] (fun path ->
    match run_fixture path with
    | Error (Runtime_claude_code.Turn_failed_with_observation { detail; _ }) ->
      check
        bool
        "generic rejection reason survives"
        true
        (Astring.String.is_infix ~affix:"stream closed" detail)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "a statusless generic error was reported as completion")
;;

(* CLI 2.1.228+ states the loop outcome as [terminal_reason]; a live forced
   overflow emitted "prompt_too_long" while the sentence and status took the
   413 shape. The typed verdict must classify without the prefix-400 shape. *)
let test_terminal_reason_prompt_too_long_is_typed () =
  with_fixture [ Emit prompt_too_long_typed_result ] (fun path ->
    match run_fixture path with
    | Error
        (Runtime_claude_code.Context_window_exceeded
          { message; tool_effect_attempted = false; response_emitted = false }) ->
      check
        bool
        "provider sentence survives into the typed failure"
        true
        (Astring.String.is_infix ~affix:"Input is too long" message)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "a typed overflow terminal was reported as completion")
;;

(* When the frame carries a verdict it is authoritative in both directions: a
   non-overflow reason stays generic even when the prose repeats the exact
   overflow prefix. *)
let test_non_overflow_reason_beats_prefix_prose () =
  with_fixture [ Emit non_overflow_reason_with_prefix_prose_result ] (fun path ->
    match run_fixture path with
    | Error (Runtime_claude_code.Turn_failed_with_observation { detail; _ }) ->
      check
        bool
        "generic rejection keeps the provider sentence"
        true
        (Astring.String.is_infix ~affix:"gateway relayed" detail)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "a non-overflow terminal was reported as completion")
;;

let test_quota_is_structurally_classified () =
  with_fixture [ Emit rate_limit_rejected; Emit quota_result ] (fun path ->
    match run_fixture path with
    | Error
        (Runtime_claude_code.Quota_blocked
          { api_error_status = Some 429
          ; rate_limit = Some { status = Runtime_claude_code.Rejected; _ }
          ; tool_effect_attempted = false
          ; response_emitted = false
          }) -> ()
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "typed quota rejection was reported as completion")
;;

let test_quota_after_native_tool_records_effect () =
  with_fixture
    [ Emit native_tool_assistant
    ; Emit native_tool_result
    ; Emit rate_limit_rejected
    ; Emit quota_result
    ]
    (fun path ->
      match run_fixture path with
      | Error
          (Runtime_claude_code.Quota_blocked
            { tool_effect_attempted = true; response_emitted = false; _ }) ->
        ()
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok _ -> fail "post-native-tool quota rejection was reported as completion")
;;

let test_rejected_rate_limit_overrides_success_flag () =
  with_fixture [ Emit rate_limit_rejected; Emit quota_result_with_success_flag ] (fun path ->
    match run_fixture path with
    | Error
        (Runtime_claude_code.Quota_blocked
          { api_error_status = None
          ; rate_limit = Some { status = Runtime_claude_code.Rejected; _ }
          ; tool_effect_attempted = false
          ; response_emitted = false
          }) -> ()
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "typed quota rejection was overridden by is_error=false")
;;

let mcp_initialize =
  {|{"type":"control_request","request_id":"mcp-init-1","request":{"subtype":"mcp_message","server_name":"masc","message":{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"claude-code-fixture","version":"1"}}}}}|}
;;

let mcp_list =
  {|{"type":"control_request","request_id":"mcp-list-1","request":{"subtype":"mcp_message","server_name":"masc","message":{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}}}|}
;;

let mcp_initialized_notification =
  {|{"type":"control_request","request_id":"mcp-notify-1","request":{"subtype":"mcp_message","server_name":"masc","message":{"jsonrpc":"2.0","method":"notifications/initialized","params":{}}}}|}
;;

let mcp_list_after_notification =
  {|{"type":"control_request","request_id":"mcp-list-after-notification","request":{"subtype":"mcp_message","server_name":"masc","message":{"jsonrpc":"2.0","id":3,"method":"tools/list","params":{}}}}|}
;;

let mcp_call =
  {|{"type":"control_request","request_id":"mcp-call-1","request":{"subtype":"mcp_message","server_name":"masc","message":{"jsonrpc":"2.0","id":"call-1","method":"tools/call","params":{"name":"masc_probe","arguments":{"marker":"from-claude"}}}}}|}
;;

let probe_tool call_count : Runtime_claude_code.dynamic_tool =
  { name = "masc_probe"
  ; description = "Return a fixture marker"
  ; input_schema = `Assoc [ "type", `String "object" ]
  ; call =
      (fun ~call_id:_ _ ->
        incr call_count;
        { success = true; content = "MASC_TOOL_RESULT"; abort_turn = None })
  }
;;

(* Producer contract measured in Claude Code 2.1.263: Yct emits the
   optional envelope boolean from isApiErrorMessage, outside message. *)
let api_error_assistant =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-api-error-1","is_api_error_message":true,"message":{"role":"assistant","model":"<synthetic>","content":[{"type":"text","text":"You've hit your limit"}]}}|}
;;

let api_error_with_native_tool =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-api-error-native-1","is_api_error_message":true,"message":{"role":"assistant","model":"<synthetic>","content":[{"type":"text","text":"You've hit your limit"},{"type":"tool_use","id":"native-call-1","name":"Read","input":{}}]}}|}
;;

let api_error_with_mcp_tool =
  {|{"type":"assistant","session_id":"__SESSION__","uuid":"assistant-api-error-mcp-1","is_api_error_message":true,"message":{"role":"assistant","model":"<synthetic>","content":[{"type":"text","text":"You've hit your limit"},{"type":"tool_use","id":"mcp-observation-1","name":"mcp__masc__masc_probe","input":{}}]}}|}
;;

let check_quota_observation ~tool_effect_attempted ~response_emitted = function
  | Error
      (Runtime_claude_code.Quota_blocked
         { api_error_status = Some 429
         ; rate_limit = Some { status = Rejected; resets_at = Some 1786356000; _ }
         ; tool_effect_attempted = actual_effect
         ; response_emitted = actual_response
         }) ->
    check bool "tool effect" tool_effect_attempted actual_effect;
    check bool "response emitted" response_emitted actual_response
  | Error error -> fail (Runtime_claude_code.error_to_string error)
  | Ok _ -> fail "quota rejection completed the turn"
;;

let test_api_diagnostic_quota_keeps_failover_safe () =
  let events = ref [] in
  with_fixture
    [ Emit api_error_assistant; Emit rate_limit_rejected; Emit quota_result ]
    (fun path ->
       run_fixture ~on_stream_event:(fun event -> events := event :: !events) path
       |> check_quota_observation ~tool_effect_attempted:false ~response_emitted:false;
       check int "diagnostic starts no response stream" 0 (List.length !events))
;;

let test_api_diagnostic_preserves_prior_text () =
  let events = ref [] in
  with_fixture
    [ Emit assistant
    ; Emit api_error_assistant
    ; Emit rate_limit_rejected
    ; Emit quota_result
    ]
    (fun path ->
       run_fixture ~on_stream_event:(fun event -> events := event :: !events) path
       |> check_quota_observation ~tool_effect_attempted:false ~response_emitted:true;
       match List.rev !events with
       | [ Turn_started { model = "claude-fixture"; _ }; Text_delta "MASC_CLAUDE_OK" ] ->
         ()
       | _ -> fail "diagnostic changed the real response stream")
;;

let test_api_diagnostic_preserves_native_effects () =
  List.iter
    (fun (frames, expected_stream_starts) ->
       let events = ref [] in
       with_fixture
         (frames @ [ Emit rate_limit_rejected; Emit quota_result ])
         (fun path ->
            run_fixture ~on_stream_event:(fun event -> events := event :: !events) path
            |> check_quota_observation ~tool_effect_attempted:true ~response_emitted:false;
            let starts =
              List.filter
                (function
                  | Runtime_claude_code.Turn_started _ -> true
                  | _ -> false)
                !events
            in
            check
              int
              "only real assistant starts stream"
              expected_stream_starts
              (List.length starts);
            let native_events =
              List.filter
                (function
                  | Runtime_claude_code.Native_tool_started _ | Native_tool_finished _ ->
                    true
                  | Turn_started _ -> false
                  | Text_delta _
                  | Dynamic_tool_started _
                  | Dynamic_tool_finished _
                  | Turn_finished _ -> fail "native-only turn emitted response content")
                (List.rev !events)
            in
            match native_events with
            | [ Native_tool_started
                  { identity = Some (Runtime_native_tools.Call_id "native-call-1"); _ }
              ; Native_tool_finished
                  { identity = Some (Runtime_native_tools.Call_id "native-call-1"); _ }
              ] -> ()
            | _ -> fail "diagnostic lost native tool start or completion"))
    [ [ Emit native_tool_assistant; Emit native_tool_result; Emit api_error_assistant ], 1
    ; [ Emit api_error_with_native_tool; Emit native_tool_result ], 0
    ]
;;

let test_api_diagnostic_preserves_mcp_effects () =
  List.iter
    (fun (diagnostic, expected_wrapper) ->
       let calls = ref 0 in
       let events = ref [] in
       with_fixture
         [ Emit_and_read mcp_initialize
         ; Emit mcp_initialized_notification
         ; Emit_and_read mcp_call
         ; Emit diagnostic
         ; Emit rate_limit_rejected
         ; Emit quota_result
         ]
         (fun path ->
            run_fixture
              ~dynamic_tools:[ probe_tool calls ]
              ~on_stream_event:(fun event -> events := event :: !events)
              path
            |> check_quota_observation ~tool_effect_attempted:true ~response_emitted:false;
            check int "MCP effect ran exactly once" 1 !calls;
            match List.rev !events with
            | [ Dynamic_tool_started _; Dynamic_tool_finished _ ] ->
              check bool "no wrapper observation expected" false expected_wrapper
            | [ Dynamic_tool_started _
              ; Dynamic_tool_finished _
              ; Native_tool_started { origin = Runtime_native_tools.Mcp_wrapper; _ }
              ] -> check bool "wrapper observation retained" true expected_wrapper
            | _ -> fail "diagnostic changed MCP observation or emitted text"))
    [ api_error_assistant, false; api_error_with_mcp_tool, true ]
;;

let test_api_diagnostic_overflow_keeps_retry_safe () =
  let events = ref [] in
  with_fixture [ Emit api_error_assistant; Emit prompt_too_long_result ] (fun path ->
    match run_fixture ~on_stream_event:(fun event -> events := event :: !events) path with
    | Error
        (Runtime_claude_code.Context_window_exceeded
           { message; tool_effect_attempted = false; response_emitted = false }) ->
      check int "no diagnostic stream to close" 0 (List.length !events);
      check
        bool
        "terminal detail retained"
        true
        (String.ends_with
           ~suffix:"Prompt is too long · the request is ~250000 tokens (limit 200000)"
           message)
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "diagnostic overflow completed")
;;

let test_api_diagnostic_preserves_terminal_error_detail () =
  with_fixture [ Emit api_error_assistant; Emit generic_400_result ] (fun path ->
    match run_fixture path with
    | Error
        (Runtime_claude_code.Turn_failed_with_observation
           { detail; tool_effect_attempted = false; response_emitted = false }) ->
      check
        string
        "terminal detail retained"
        "terminal subtype=success api_status=400 reason=none: request rejected; \
         diagnostic mentioned Prompt is too long without the provider prefix"
        detail
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "diagnostic generic failure completed")
;;

let assistant_api_flag flag =
  match Yojson.Safe.from_string api_error_assistant with
  | `Assoc fields ->
    let fields = List.remove_assoc "is_api_error_message" fields in
    Yojson.Safe.to_string
      (`Assoc
          (Option.fold
             ~none:fields
             ~some:(fun value -> ("is_api_error_message", value) :: fields)
             flag))
  | _ -> fail "invalid assistant fixture"
;;

let test_real_identical_prose_is_still_response () =
  List.iter
    (fun flag ->
       let events = ref [] in
       with_fixture
         [ Emit (assistant_api_flag flag); Emit rate_limit_rejected; Emit quota_result ]
         (fun path ->
            run_fixture ~on_stream_event:(fun event -> events := event :: !events) path
            |> check_quota_observation ~tool_effect_attempted:false ~response_emitted:true;
            match List.rev !events with
            | [ Turn_started _; Text_delta "You've hit your limit" ] -> ()
            | _ -> fail "ordinary assistant prose was hidden"))
    [ None; Some (`Bool false) ]
;;

let test_malformed_api_error_flag_fails_closed () =
  List.iter
    (fun value ->
       let events = ref [] in
       with_fixture
         [ Emit (assistant_api_flag (Some value)); Emit result ]
         (fun path ->
            match
              run_fixture ~on_stream_event:(fun event -> events := event :: !events) path
            with
            | Error
                (Runtime_claude_code.Protocol_error
                   { stage = "assistant message"; detail }) ->
              check
                string
                "typed field failure"
                "field \"is_api_error_message\" must be a boolean"
                detail;
              check int "malformed frame emits nothing" 0 (List.length !events)
            | Error error -> fail (Runtime_claude_code.error_to_string error)
            | Ok _ -> fail "malformed diagnostic flag admitted"))
    [ `Null; `String "true"; `Int 1; `Assoc []; `List [] ]
;;

let test_valid_response_after_api_diagnostic () =
  let events = ref [] in
  with_fixture [ Emit api_error_assistant; Emit assistant; Emit result ] (fun path ->
    match run_fixture ~on_stream_event:(fun event -> events := event :: !events) path with
    | Ok turn ->
      check string "measured model" "claude-fixture" turn.model;
      check string "real response" "MASC_CLAUDE_OK" turn.text;
      (match List.rev !events with
       | [ Turn_started { turn_id = "assistant-fixture-1"; model = "claude-fixture" }
         ; Text_delta "MASC_CLAUDE_OK"
         ; Turn_finished { text = "MASC_CLAUDE_OK" }
         ] -> ()
       | _ -> fail "diagnostic started or polluted response stream")
    | Error error -> fail (Runtime_claude_code.error_to_string error))
;;

let test_api_diagnostic_does_not_replace_measured_model_or_text () =
  let empty_result =
    {|{"type":"result","subtype":"success","is_error":false,"session_id":"__SESSION__","uuid":"turn-after-diagnostic","result":""}|}
  in
  with_fixture
    [ Emit assistant; Emit api_error_assistant; Emit empty_result ]
    (fun path ->
       match run_fixture path with
       | Ok turn ->
         check string "measured model survives diagnostic" "claude-fixture" turn.model;
         check string "fallback contains only real text" "MASC_CLAUDE_OK" turn.text
       | Error error -> fail (Runtime_claude_code.error_to_string error))
;;

let test_tool_call_before_turn_admission_is_rejected () =
  let call_count = ref 0 in
  with_fixture
    ~before_initialize_response:
      [ Emit_and_read mcp_initialize
      ; Emit mcp_initialized_notification
      ; Emit_and_read mcp_call
      ]
    []
    (fun path ->
      match run_fixture ~dynamic_tools:[ probe_tool call_count ] path with
      | Error (Runtime_claude_code.Protocol_error { stage = "MCP tools/call"; _ }) ->
        check int "tool effect count" 0 !call_count
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok _ -> fail "tool call ran before durable turn admission")
;;

let test_post_effect_response_failure_requires_recovery () =
  let call_count = ref 0 in
  with_fixture
    [ Emit_and_read mcp_initialize
    ; Emit_and_read mcp_initialized_notification
    ; Emit_after_closing_input mcp_call
    ]
    (fun path ->
      match run_fixture ~dynamic_tools:[ probe_tool call_count ] path with
      | Error
          (Runtime_claude_code.Turn_transport_interrupted
            { stage = "control success response"
            ; tool_effect_attempted = true
            ; _
            }) -> check int "tool effect count" 1 !call_count
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok _ -> fail "post-effect response failure completed the turn")
;;

let test_context_overflow_after_tool_records_effect () =
  let call_count = ref 0 in
  with_fixture
    [ Emit_and_read mcp_initialize
    ; Emit mcp_initialized_notification
    ; Emit_and_read mcp_list
    ; Emit_and_read mcp_call
    ; Emit prompt_too_long_result
    ]
    (fun path ->
      match run_fixture ~dynamic_tools:[ probe_tool call_count ] path with
      | Error
          (Runtime_claude_code.Context_window_exceeded
            { tool_effect_attempted = true; response_emitted = false; _ }) ->
        check int "tool effect count" 1 !call_count
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok _ -> fail "post-effect context overflow completed the turn")
;;

let test_context_overflow_after_response_records_activity () =
  with_fixture [ Emit assistant; Emit prompt_too_long_result ] (fun path ->
    match run_fixture path with
    | Error
        (Runtime_claude_code.Context_window_exceeded
          { tool_effect_attempted = false; response_emitted = true; _ }) ->
      ()
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "post-response context overflow completed the turn")
;;

let test_context_overflow_after_empty_assistant_remains_retry_safe () =
  let events = ref [] in
  with_fixture [ Emit empty_assistant; Emit prompt_too_long_result ] (fun path ->
    match
      run_fixture
        ~on_stream_event:(fun event -> events := event :: !events)
        path
    with
    | Error
        (Runtime_claude_code.Context_window_exceeded
          { tool_effect_attempted = false; response_emitted = false; _ }) ->
      (match List.rev !events with
       | [ Turn_started
             { turn_id = "assistant-empty-1"; model = "claude-fixture" }
         ; Turn_finished { text = "" }
         ] ->
         ()
       | _ -> fail "retry-safe overflow left the first stream unbalanced")
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "empty assistant frame made context overflow complete")
;;

let test_dynamic_tool_abort_stops_the_provider_loop () =
  let tool : Runtime_claude_code.dynamic_tool =
    { name = "masc_probe"
    ; description = "Abort a repeated provider loop"
    ; input_schema = `Assoc [ "type", `String "object" ]
    ; call =
        (fun ~call_id:_ _ ->
          { success = false
          ; content = "same deterministic failure"
          ; abort_turn =
              Some
                (Repeated_tool_call
                   { tool_name = "masc_probe"; repeated_count = 3 })
          })
    }
  in
  with_fixture
    [ Emit_and_read mcp_initialize
    ; Emit mcp_initialized_notification
    ; Emit_and_read mcp_list
    ; Emit_and_read mcp_call
    ]
    (fun path ->
      match run_fixture ~dynamic_tools:[ tool ] path with
      | Error
          (Runtime_claude_code.Stopped_by_host
            { stop = Repeated_tool_call { tool_name; repeated_count }; _ }) ->
        check string "tool" "masc_probe" tool_name;
        check int "repeat count" 3 repeated_count
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok _ -> fail "dynamic tool abort did not stop the Claude turn")
;;

(* A live keeper, 2026-09-02: every host-stopped turn recorded output_tokens = 0
   because the stop was built with usage = None. The result frame never
   arrives after a host stop, so the sum of the assistant frames seen so far
   is the turn's measurement, deduplicated by message id. *)
let test_host_stop_carries_the_assistant_usage_sum () =
  let tool : Runtime_claude_code.dynamic_tool =
    { name = "masc_probe"
    ; description = "Abort a repeated provider loop"
    ; input_schema = `Assoc [ "type", `String "object" ]
    ; call =
        (fun ~call_id:_ _ ->
          { success = false
          ; content = "same deterministic failure"
          ; abort_turn =
              Some
                (Repeated_tool_call
                   { tool_name = "masc_probe"; repeated_count = 3 })
          })
    }
  in
  with_fixture
    [ Emit assistant_with_usage_a
    ; Emit assistant_with_usage_a_sibling
    ; Emit assistant_with_usage_b
    ; Emit assistant_with_usage_no_id
    ; Emit_and_read mcp_initialize
    ; Emit mcp_initialized_notification
    ; Emit_and_read mcp_list
    ; Emit_and_read mcp_call
    ]
    (fun path ->
      match run_fixture ~dynamic_tools:[ tool ] path with
      | Error (Runtime_claude_code.Stopped_by_host { usage = Some usage; _ }) ->
        check int "input tokens summed, sibling counted once, id-less counted" 301
          usage.input_tokens;
        check int "output tokens summed" 31 usage.output_tokens;
        check int "cache read carried" 5 usage.cache_read_input_tokens;
        check int "absent cache creation is 0" 0 usage.cache_creation_input_tokens
      | Error (Runtime_claude_code.Stopped_by_host { usage = None; _ }) ->
        fail "host stop dropped the assistant usage"
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok _ -> fail "dynamic tool abort did not stop the Claude turn")
;;

let test_dynamic_tool_callback () =
  let observed_call_id = ref None in
  let observed_input = ref `Null in
  let tool : Runtime_claude_code.dynamic_tool =
    { name = "masc_probe"
    ; description = "Return a fixture marker"
    ; input_schema =
        `Assoc
          [ "type", `String "object"
          ; "properties", `Assoc [ "marker", `Assoc [ "type", `String "string" ] ]
          ]
    ; call =
        (fun ~call_id input ->
          observed_call_id := Some call_id;
          observed_input := input;
          { success = true; content = "MASC_TOOL_RESULT"; abort_turn = None })
    }
  in
  with_fixture
    [ Emit_and_read mcp_initialize
    ; Emit mcp_initialized_notification
    ; Emit_and_read mcp_list
    ; Emit_and_read mcp_call
    ; Emit assistant
    ; Emit result
    ]
    (fun path ->
      match run_fixture ~dynamic_tools:[ tool ] path with
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok turn ->
        check int "measured calls" 1 turn.dynamic_tool_calls;
        check (option string) "call id" (Some "call-1") !observed_call_id;
        check string
          "arguments"
          {|{"marker":"from-claude"}|}
          (Yojson.Safe.to_string !observed_input))
;;

let test_stream_events_preserve_text_and_tool_identity () =
  let events = ref [] in
  let tool : Runtime_claude_code.dynamic_tool =
    { name = "masc_probe"
    ; description = "Return a fixture marker"
    ; input_schema = `Assoc [ "type", `String "object" ]
    ; call =
        (fun ~call_id:_ _ ->
          { success = true; content = "MASC_TOOL_RESULT"; abort_turn = None })
    }
  in
  with_fixture
    [ Emit_and_read mcp_initialize
    ; Emit mcp_initialized_notification
    ; Emit_and_read mcp_list
    ; Emit assistant
    ; Emit_and_read mcp_call
    ; Emit result
    ]
    (fun path ->
      match
        run_fixture
          ~dynamic_tools:[ tool ]
          ~on_stream_event:(fun event -> events := event :: !events)
          path
      with
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok _ ->
        match List.rev !events with
        | [ Turn_started { turn_id = "assistant-fixture-1"; model = "claude-fixture" }
          ; Text_delta "MASC_CLAUDE_OK"
          ; Dynamic_tool_started
              { call_id = "call-1"
              ; tool_name = "masc_probe"
              ; arguments
              }
          ; Dynamic_tool_finished { call_id = "call-1" }
          ; Turn_finished { text = "MASC_CLAUDE_OK" }
          ] ->
          check string
            "exact tool arguments"
            {|{"marker":"from-claude"}|}
            (Yojson.Safe.to_string arguments)
        | _ -> fail "Claude stream events did not preserve wire order and identity")
;;

let test_stream_events_preserve_native_tool_origin () =
  let events = ref [] in
  with_fixture
    [ Emit native_tool_assistant; Emit native_tool_result; Emit assistant; Emit result ]
    (fun path ->
      match
        run_fixture
          ~timeout_s:window_outlasting_process_start_s
          ~on_stream_event:(fun event -> events := event :: !events)
          path
      with
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok turn ->
        check int "no MASC dynamic calls" 0 turn.dynamic_tool_calls;
        match List.rev !events with
        | [ Turn_started { turn_id = "assistant-native-1"; model = "claude-fixture" }
          ; Native_tool_started
              { identity = Some (Runtime_native_tools.Call_id "native-call-1")
              ; tool_name = Some "Read"
              ; origin = Runtime_native_tools.Built_in
              }
          ; Native_tool_finished
              { identity = Some (Runtime_native_tools.Call_id "native-call-1")
              ; tool_name = Some "Read"
              ; origin = Runtime_native_tools.Built_in
              }
          ; Text_delta "MASC_CLAUDE_OK"
          ; Turn_finished { text = "MASC_CLAUDE_OK" }
          ] -> ()
        | _ -> fail "Claude native tool activity was not kept distinct from MASC tools")
;;

(* This test used to assert the opposite: that an MCP notification draws no
   control_response, on the reasoning that a JSON-RPC notification carries no id
   and so needs no reply. That conflates two layers. The reply rule that binds
   here belongs to the control channel, not to the MCP payload: the client wraps
   every MCP message in a control_request carrying a [request_id] and blocks
   until that id comes back.

   The old fixture could not see the difference because it moved on without
   waiting — a fake client that never blocks cannot observe that the real one
   does. Measured against Claude Code 2.1.226 with every other variable held:
   with the notification unanswered the turn stops after
   notifications/initialized and never reaches a result (90s, 4 events); with it
   answered the same turn runs tools/list -> init -> result in 5.2s. On the live
   fleet that stall was every claude_code keeper turn — 458 attempts, 0
   completions over 8 days (#28060).

   Emit_and_expect_request_id makes the fixture wait and exit 96 unless the
   response carries the id, so this now fails if the ack is dropped again. *)
let test_mcp_notification_is_acknowledged_on_the_control_channel () =
  with_fixture
    [ Emit_and_read mcp_initialize
    ; Emit_and_expect_request_id (mcp_initialized_notification, "mcp-notify-1")
    ; Emit_and_expect_request_id
        (mcp_list_after_notification, "mcp-list-after-notification")
    ; Emit assistant
    ; Emit result
    ]
    (fun path ->
      match run_fixture path with
      | Error error -> fail (Runtime_claude_code.error_to_string error)
      | Ok turn -> check string "text" "MASC_CLAUDE_OK" turn.text)
;;

let test_shared_mcp_bridge_owns_exact_dispatch () =
  let tool_specs () =
    [ `Assoc
        [ "name", `String "masc_probe"
        ; "description", `String "fixture"
        ; "inputSchema", `Assoc [ "type", `String "object" ]
        ]
    ]
  in
  let call_tool ~name ~call_id:_ ~arguments:_ =
    if String.equal name "missing"
    then None
    else failf "unexpected tool dispatch reached the callback: %s" name
  in
  let session = Runtime_official_client_mcp.create_session () in
  let dispatch json =
    Runtime_official_client_mcp.handle_message
      ~session
      ~server_name:"masc"
      ~tool_call_policy:Runtime_official_client_mcp.Allow_tool_calls
      ~tool_specs
      ~call_tool
      json
    |> Result.get_ok
  in
  let callback_count = ref 0 in
  let guarded_call_tool ~name:_ ~call_id:_ ~arguments:_ =
    incr callback_count;
    Some { Runtime_official_client_mcp.success = true; content = "unexpected" }
  in
  let fresh_session = Runtime_official_client_mcp.create_session () in
  let rejects_phase label message =
    match
      Runtime_official_client_mcp.handle_message
        ~session:fresh_session
        ~server_name:"masc"
        ~tool_call_policy:Runtime_official_client_mcp.Allow_tool_calls
        ~tool_specs
        ~call_tool:guarded_call_tool
        message
    with
    | Error { stage; _ } when String.starts_with ~prefix:"MCP " stage -> ()
    | Error _ -> failf "%s had the wrong lifecycle error stage" label
    | Ok _ -> failf "%s bypassed the MCP lifecycle" label
  in
  rejects_phase
    "list before initialize"
    (`Assoc
       [ "jsonrpc", `String "2.0"
       ; "id", `Int 90
       ; "method", `String "tools/list"
       ; "params", `Assoc []
       ]);
  rejects_phase
    "call before initialize"
    (`Assoc
       [ "jsonrpc", `String "2.0"
       ; "id", `Int 91
       ; "method", `String "tools/call"
       ; ( "params"
         , `Assoc
             [ "name", `String "masc_probe"
             ; "arguments", `Assoc []
             ] )
       ]);
  check int "pre-initialize callback count" 0 !callback_count;
  let initialized =
    Runtime_official_client_mcp.handle_message
      ~session:fresh_session
      ~server_name:"masc"
      ~tool_call_policy:Runtime_official_client_mcp.Allow_tool_calls
      ~tool_specs
      ~call_tool:guarded_call_tool
      (`Assoc
         [ "jsonrpc", `String "2.0"
         ; "id", `Int 92
         ; "method", `String "initialize"
         ; ( "params"
           , `Assoc
               [ "protocolVersion", `String "2025-11-25"
               ; "capabilities", `Assoc []
               ; ( "clientInfo"
                 , `Assoc
                     [ "name", `String "official-client-test"
                     ; "version", `String "1"
                     ] )
               ] )
         ])
    |> Result.get_ok
  in
  check bool "initialize has response" true (Option.is_some initialized.response);
  rejects_phase
    "call before initialized notification"
    (`Assoc
       [ "jsonrpc", `String "2.0"
       ; "id", `Int 93
       ; "method", `String "tools/call"
       ; ( "params"
         , `Assoc
             [ "name", `String "masc_probe"
             ; "arguments", `Assoc []
             ] )
       ]);
  check int "pre-ready callback count" 0 !callback_count;
  let initialize =
    dispatch
      (`Assoc
         [ "jsonrpc", `String "2.0"
         ; "id", `Int 0
         ; "method", `String "initialize"
         ; ( "params"
           , `Assoc
               [ "protocolVersion", `String "2025-11-25"
               ; "capabilities", `Assoc []
               ; ( "clientInfo"
                 , `Assoc
                     [ "name", `String "official-client-test"
                     ; "version", `String "1"
                     ] )
               ] )
         ])
  in
  check bool "initialize has response" true (Option.is_some initialize.response);
  let notification =
    dispatch
      (`Assoc
         [ "jsonrpc", `String "2.0"
         ; "method", `String "notifications/initialized"
         ; "params", `Assoc []
         ])
  in
  check (option string) "notification has no response" None
    (Option.map Yojson.Safe.to_string notification.response);
  let list =
    dispatch
      (`Assoc
         [ "jsonrpc", `String "2.0"
         ; "id", `Int 1
         ; "method", `String "tools/list"
         ; "params", `Assoc []
         ])
  in
  check bool "list has response" true (Option.is_some list.response);
  check bool "list did not call a tool" false list.tool_called;
  let unknown =
    dispatch
      (`Assoc
         [ "jsonrpc", `String "2.0"
         ; "id", `String "call-unknown"
         ; "method", `String "tools/call"
         ; ( "params"
           , `Assoc
               [ "name", `String "missing"
               ; "arguments", `Assoc []
               ] )
         ])
  in
  check bool "unknown tool did not execute" false unknown.tool_called;
  let open Yojson.Safe.Util in
  check int
    "unknown tool JSON-RPC code"
    (-32602)
    (unknown.response |> Option.get |> member "error" |> member "code" |> to_int);
  (match
     Runtime_official_client_mcp.handle_message
       ~session
       ~server_name:"masc"
       ~tool_call_policy:Runtime_official_client_mcp.Allow_tool_calls
       ~tool_specs
       ~call_tool
       (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "id", `Bool true
          ; "method", `String "tools/list"
          ])
   with
   | Error { stage = "MCP message"; _ } -> ()
   | Error _ -> fail "invalid request id had the wrong error stage"
   | Ok _ -> fail "boolean JSON-RPC request id was admitted");
  let rejects label message =
    match
      Runtime_official_client_mcp.handle_message
        ~session
        ~server_name:"masc"
        ~tool_call_policy:Runtime_official_client_mcp.Allow_tool_calls
        ~tool_specs
        ~call_tool
        message
    with
    | Error { stage = "MCP message"; _ } -> ()
    | Error _ -> failf "%s had the wrong error stage" label
    | Ok _ -> failf "%s was admitted" label
  in
  let rejects_raw label raw_message =
    rejects label (Yojson.Safe.from_string raw_message)
  in
  rejects
    "explicit null notification id"
    (`Assoc
       [ "jsonrpc", `String "2.0"
       ; "id", `Null
       ; "method", `String "notifications/initialized"
       ; "params", `Assoc []
       ]);
  rejects
    "scalar params"
    (`Assoc
       [ "jsonrpc", `String "2.0"
       ; "id", `Int 2
       ; "method", `String "tools/list"
       ; "params", `Null
       ]);
  rejects_raw
    "duplicate method"
    {|{"jsonrpc":"2.0","id":3,"method":"tools/list","method":"tools/call","params":{}}|};
  rejects_raw
    "nested duplicate argument"
    {|{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"missing","arguments":{"score":1,"score":2}}}|};
  rejects
    "unknown top-level field"
    (`Assoc
       [ "jsonrpc", `String "2.0"
       ; "id", `Int 5
       ; "method", `String "tools/list"
       ; "params", `Assoc []
       ; "fallback", `Bool true
       ]);
  rejects_raw
    "non-finite nested value"
    {|{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"missing","arguments":{"score":NaN}}}|}
;;

let test_shared_mcp_protocol_is_negotiated () =
  let tool_specs () = [] in
  let call_tool ~name:_ ~call_id:_ ~arguments:_ = None in
  let initialize protocol_version =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", `Int 1
      ; "method", `String "initialize"
      ; ( "params"
        , `Assoc
            [ "protocolVersion", `String protocol_version
            ; "capabilities", `Assoc []
            ; ( "clientInfo"
              , `Assoc
                  [ "name", `String "official-client-test"
                  ; "version", `String "1"
                  ] )
            ] )
      ]
  in
  let open Yojson.Safe.Util in
  let handle message =
    Runtime_official_client_mcp.handle_message
      ~session:(Runtime_official_client_mcp.create_session ())
      ~server_name:"masc"
      ~tool_call_policy:Runtime_official_client_mcp.Allow_tool_calls
      ~tool_specs
      ~call_tool
      message
  in
  match
    handle (initialize "2025-11-25")
  with
  | Error error -> fail (error.stage ^ ": " ^ error.detail)
  | Ok dispatch ->
    check string
      "measured Antigravity protocol"
      "2025-11-25"
      (dispatch.response
       |> Option.get
       |> member "result"
       |> member "protocolVersion"
       |> to_string);
    check string
      "MCP server version uses the canonical build owner"
      Runtime_build_version.current
      (dispatch.response
       |> Option.get
       |> member "result"
       |> member "serverInfo"
       |> member "version"
       |> to_string);
    (match
       handle (initialize "2099-01-01")
     with
     | Error { stage = "MCP initialize"; _ } -> ()
     | Error _ -> fail "unsupported protocol version had the wrong stage"
     | Ok _ -> fail "unsupported protocol version was silently normalized");
    (match
       handle
         (`Assoc
            [ "jsonrpc", `String "2.0"
            ; "id", `Int 2
            ; "method", `String "initialize"
            ; ( "params"
              , `Assoc
                  [ "capabilities", `Assoc []
                  ; ( "clientInfo"
                    , `Assoc
                        [ "name", `String "official-client-test"
                        ; "version", `String "1"
                        ] )
                  ] )
            ])
     with
     | Error { stage = "MCP initialize"; _ } -> ()
     | Error _ -> fail "missing protocol version had the wrong stage"
     | Ok _ -> fail "missing protocol version inherited a hidden default")
;;

let test_malformed_json_fails_closed () =
  with_fixture [ Emit "not-json" ] (fun path ->
    match run_fixture path with
    | Error (Runtime_claude_code.Protocol_error _) -> ()
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "malformed stream JSON was admitted")
;;

let test_duplicate_keys_fail_closed () =
  let duplicate =
    {|{"type":"assistant","type":"result","session_id":"__SESSION__","uuid":"duplicate-1"}|}
  in
  with_fixture [ Emit duplicate ] (fun path ->
    match run_fixture path with
    | Error (Runtime_claude_code.Protocol_error _) -> ()
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "duplicate stream JSON key was admitted")
;;

let test_unsupported_control_request_fails_closed () =
  let request =
    {|{"type":"control_request","request_id":"permission-1","request":{"subtype":"can_use_tool","tool_name":"Bash","input":{}}}|}
  in
  with_fixture [ Emit request ] (fun path ->
    match run_fixture path with
    | Error (Runtime_claude_code.Unsupported_control_request "can_use_tool") -> ()
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "unsupported permission request was admitted")
;;

let test_unknown_stream_type_fails_closed () =
  let unknown =
    {|{"type":"future_magic","session_id":"__SESSION__","uuid":"future-1"}|}
  in
  with_fixture [ Emit unknown ] (fun path ->
    match run_fixture ~timeout_s:window_outlasting_process_start_s path with
    | Error (Runtime_claude_code.Protocol_error _) -> ()
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok _ -> fail "unknown stream type was silently ignored")
;;

let test_tool_progress_keeps_stream_open () =
  with_fixture [ Emit tool_progress; Emit assistant; Emit result ] (fun path ->
    match run_fixture ~timeout_s:window_outlasting_process_start_s path with
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok turn -> check string "text" "MASC_CLAUDE_OK" turn.text)
;;

let test_dynamic_tool_tokenizer_chars_are_validated () =
  let tool : Runtime_claude_code.dynamic_tool =
    { name = "bad,tool"
    ; description = "invalid fixture"
    ; input_schema = `Assoc []
    ; call =
        (fun ~call_id:_ _ ->
          { success = true; content = "unused"; abort_turn = None })
    }
  in
  let config = Runtime_claude_code.default_config ~cwd:"/tmp" in
  match
    Runtime_claude_code.validate_turn
      ~dynamic_tools:[ tool ]
      config
      ~prompt:"fixture"
      ~images:[]
  with
  | Error (Runtime_claude_code.Invalid_config _) -> ()
  | Error error -> fail (Runtime_claude_code.error_to_string error)
  | Ok () -> fail "allowedTools delimiter was admitted in a tool name"
;;

let test_resume_preserves_session_identity () =
  with_fixture [ Emit assistant; Emit result ] (fun path ->
    match
      run_fixture
        ~session_mode:
          (Runtime_claude_code.Resume { session_id = "fixture-session-resume" })
        path
    with
    | Error error -> fail (Runtime_claude_code.error_to_string error)
    | Ok turn ->
      check string "session" "fixture-session-resume" turn.session_id;
      check bool "resumed" true turn.resumed)
;;

let test_live_subscription () =
  if Sys.getenv_opt "MASC_CLAUDE_CODE_LIVE" <> Some "1"
  then Alcotest.skip ()
  else
    let outcome =
      Eio_main.run (fun env ->
        let config =
          { (Runtime_claude_code.default_config ~cwd:"/tmp") with timeout_s = Some 60.0 }
        in
        Runtime_claude_code.run_turn
          ~mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env)
          ~cwd:Eio.Path.(Eio.Stdenv.fs env / "/tmp")
          config
          ~prompt:"Reply with exactly MASC_CLAUDE_LIVE_OK"
          ~images:[])
    in
    match outcome with
    | Ok turn -> check string "live response" "MASC_CLAUDE_LIVE_OK" turn.text
    | Error error -> fail (Runtime_claude_code.error_to_string error)
;;


(* RFC-0390: the argv is the contract with the installed CLI. These pin how
   [config.native] selects [--tools] and what [--allowedTools] pre-approves. *)
let flag_value argv flag =
  let rec walk = function
    | name :: value :: _ when String.equal name flag -> Some value
    | _ :: rest -> walk rest
    | [] -> None
  in
  walk argv
;;

let native_argv ~native ~dynamic_tools =
  let config =
    { (Runtime_claude_code.default_config ~cwd:"/tmp") with native }
  in
  match
    Runtime_claude_code.command
      config
      ~dynamic_tools
      ~reasoning_effort:None
      ~session_mode:Runtime_claude_code.Start
      ~session_id:"11111111-1111-4111-8111-111111111111"
  with
  | Ok argv -> argv
  | Error error -> fail (Runtime_claude_code.error_to_string error)
;;

let stub_dynamic_tool =
  { Runtime_claude_code.name = "masc_status"
  ; description = "fixture"
  ; input_schema = `Assoc []
  ; call =
      (fun ~call_id:_ _ ->
        { Runtime_claude_code.success = true
        ; content = "{}"
        ; abort_turn = None
        })
  }
;;

(* The settings layers are part of the same argv contract: empty renders the
   historical bare [--setting-sources=] so nothing loads, and a declared list
   renders comma-joined in declaration order. *)
let test_setting_sources_render_in_argv () =
  let argv sources =
    let config =
      { (Runtime_claude_code.default_config ~cwd:"/tmp") with
        setting_sources = sources
      }
    in
    match
      Runtime_claude_code.command
        config
        ~dynamic_tools:[]
        ~reasoning_effort:None
        ~session_mode:Runtime_claude_code.Start
        ~session_id:"11111111-1111-4111-8111-111111111111"
    with
    | Ok argv -> argv
    | Error error -> fail (Runtime_claude_code.error_to_string error)
  in
  check bool "default keeps the bare no-layer token" true
    (List.mem "--setting-sources=" (argv []));
  check bool "declared layers render comma-joined in order" true
    (List.mem
       "--setting-sources=project,user"
       (argv
          [ Runtime_native_tools.Settings_project
          ; Runtime_native_tools.Settings_user
          ]));
  check bool "a declared list drops the bare token" true
    (not
       (List.mem
          "--setting-sources="
          (argv [ Runtime_native_tools.Settings_local ])))
;;

(* Passing [--system-prompt] replaces the CLI's built-in prompt, so the flag's
   absence is what selects that prompt. claude 2.1.260 reads any string as the
   prompt — "" included — which is why an unset prompt has to drop the flag
   rather than send an empty value. Its --help states the same coupling:
   "Only applies with the default system prompt (ignored with
   --system-prompt)". *)
let test_system_prompt_flag_is_omitted_when_unset () =
  let argv system_prompt =
    let config =
      { (Runtime_claude_code.default_config ~cwd:"/tmp") with system_prompt }
    in
    match
      Runtime_claude_code.command
        config
        ~dynamic_tools:[]
        ~reasoning_effort:None
        ~session_mode:Runtime_claude_code.Start
        ~session_id:"11111111-1111-4111-8111-111111111111"
    with
    | Ok argv -> argv
    | Error error -> fail (Runtime_claude_code.error_to_string error)
  in
  check bool "an unset prompt sends no --system-prompt" false
    (List.mem "--system-prompt" (argv None));
  check (option string) "so the flag carries no value" None
    (flag_value (argv None) "--system-prompt");
  check bool "a declared prompt sends the flag" true
    (List.mem "--system-prompt" (argv (Some "keeper instructions")));
  check (option string) "with its text verbatim" (Some "keeper instructions")
    (flag_value (argv (Some "keeper instructions")) "--system-prompt")
;;

let test_native_posture_selects_tools_flag () =
  let argv posture = native_argv ~native:posture ~dynamic_tools:[] in
  check (option string) "none disables the built-in set" (Some "")
    (flag_value (argv Runtime_native_tools.Native_none) "--tools");
  check (option string) "read allowlists the read set" (Some "Read,Glob,Grep")
    (flag_value (argv Runtime_native_tools.Native_read) "--tools");
  check (option string) "full enables the default set" (Some "default")
    (flag_value (argv Runtime_native_tools.Native_full) "--tools")
;;

let test_native_read_preapproves_read_tools () =
  check (option string)
    "read with no MCP tools still pre-approves its read set"
    (Some "Read,Glob,Grep")
    (flag_value
       (native_argv ~native:Runtime_native_tools.Native_read ~dynamic_tools:[])
       "--allowedTools");
  check (option string)
    "read merges native names ahead of MCP names"
    (Some "Read,Glob,Grep,mcp__masc__masc_status")
    (flag_value
       (native_argv
          ~native:Runtime_native_tools.Native_read
          ~dynamic_tools:[ stub_dynamic_tool ])
       "--allowedTools");
  check (option string)
    "none with no MCP tools passes no allowlist"
    None
    (flag_value
       (native_argv ~native:Runtime_native_tools.Native_none ~dynamic_tools:[])
       "--allowedTools")
;;

let () =
  run
    "runtime_claude_code"
    [ ( "native posture argv (RFC-0390)"
      , [ test_case
            "posture selects --tools"
            `Quick
            test_native_posture_selects_tools_flag
        ; Alcotest.test_case
            "setting sources render in argv"
            `Quick
            test_setting_sources_render_in_argv
        ; test_case
            "an unset system prompt omits the flag"
            `Quick
            test_system_prompt_flag_is_omitted_when_unset
        ; test_case
            "read pre-approves its read set"
            `Quick
            test_native_read_preapproves_read_tools
        ] )
    ; ( "admission"
      , [ test_case "validation is process-free" `Quick test_validation_is_process_free
        ; test_case
            "subscription auth and env scrub"
            `Quick
            test_subscription_turn_and_env_scrub
        ; test_case
            "progress resets stream idle timeout"
            `Quick
            test_progress_resets_stream_idle_timeout
        ; test_case
            "stream idle timeout is typed"
            `Quick
            test_stream_idle_timeout_is_typed
        ; test_case
            "no deadline keeps initialize bounded"
            `Quick
            test_no_deadline_keeps_initialize_bounded
        ; test_case
            "no deadline starts after user message"
            `Quick
            test_no_deadline_starts_after_user_message
        ; test_case
            "no deadline keeps turn-started callback bounded"
            `Quick
            test_no_deadline_keeps_turn_started_callback_bounded
        ; test_case
            "state callback timeout is typed"
            `Quick
            test_state_callback_timeout_is_typed
        ; test_case
            "callback timeout origin is preserved without deadline"
            `Quick
            test_callback_timeout_origin_is_preserved_without_deadline
        ; test_case
            "non-subscription rejected"
            `Quick
            test_non_subscription_auth_is_rejected
        ; test_case
            "missing CLI differs from logout"
            `Quick
            test_missing_cli_is_not_reported_as_logout
        ] )
    ; ( "terminal"
      , [ test_case
            "terminal prompt too long is typed"
            `Quick
            test_terminal_prompt_too_long_is_typed
        ; test_case
            "terminal_reason prompt_too_long is typed"
            `Quick
            test_terminal_reason_prompt_too_long_is_typed
        ; test_case
            "statusless canonical prompt too long is typed"
            `Quick
            test_statusless_canonical_prompt_too_long_is_typed
        ; test_case
            "statusless overflow prose remains turn failed"
            `Quick
            test_statusless_overflow_prose_remains_turn_failed
        ; test_case
            "non-overflow terminal_reason beats prefix prose"
            `Quick
            test_non_overflow_reason_beats_prefix_prose
        ; test_case
            "unrelated 400 remains turn failed"
            `Quick
            test_unrelated_400_remains_turn_failed
        ; test_case
            "statusless prompt too long is typed"
            `Quick
            test_statusless_prompt_too_long_is_typed
        ; test_case
            "statusless input too long is typed"
            `Quick
            test_statusless_input_too_long_is_typed
        ; test_case
            "statusless generic error remains turn failed"
            `Quick
            test_statusless_generic_error_remains_turn_failed
        ; test_case
            "quota is structurally classified"
            `Quick
            test_quota_is_structurally_classified
        ; test_case "quota after native tool records effect" `Quick
            test_quota_after_native_tool_records_effect
        ; test_case
            "rejected rate limit overrides success flag"
            `Quick
            test_rejected_rate_limit_overrides_success_flag
        ; test_case "malformed JSON fails closed" `Quick test_malformed_json_fails_closed
        ; test_case "duplicate keys fail closed" `Quick test_duplicate_keys_fail_closed
        ; test_case "result usage is carried" `Quick test_result_usage_is_carried
        ; test_case
            "dynamic tool bytes counts every field"
            `Quick
            test_dynamic_tool_bytes_counts_every_field
        ; test_case
            "partial usage does not fail the turn"
            `Quick
            test_partial_result_usage_does_not_fail_the_turn
        ] )
    ; ( "API diagnostic origin"
      , [ test_case
            "quota remains failover safe"
            `Quick
            test_api_diagnostic_quota_keeps_failover_safe
        ; test_case
            "prior text remains emitted"
            `Quick
            test_api_diagnostic_preserves_prior_text
        ; test_case
            "prior and mixed native effects survive"
            `Quick
            test_api_diagnostic_preserves_native_effects
        ; test_case
            "prior and mixed MCP effects survive"
            `Quick
            test_api_diagnostic_preserves_mcp_effects
        ; test_case
            "overflow remains retry safe"
            `Quick
            test_api_diagnostic_overflow_keeps_retry_safe
        ; test_case
            "terminal error detail survives"
            `Quick
            test_api_diagnostic_preserves_terminal_error_detail
        ; test_case
            "identical ordinary prose remains response"
            `Quick
            test_real_identical_prose_is_still_response
        ; test_case
            "malformed flag fails closed"
            `Quick
            test_malformed_api_error_flag_fails_closed
        ; test_case
            "valid response follows diagnostic"
            `Quick
            test_valid_response_after_api_diagnostic
        ; test_case
            "diagnostic preserves measured model and text"
            `Quick
            test_api_diagnostic_does_not_replace_measured_model_or_text
        ] )
    ; ( "mcp"
      , [ test_case "dynamic tool callback" `Quick test_dynamic_tool_callback
        ; test_case
            "stream preserves text and tool identity"
            `Quick
            test_stream_events_preserve_text_and_tool_identity
        ; test_case
            "stream preserves native tool origin"
            `Quick
            test_stream_events_preserve_native_tool_origin
        ; test_case
            "pre-admission tool call is rejected"
            `Quick
            test_tool_call_before_turn_admission_is_rejected
        ; test_case
            "post-effect response failure requires recovery"
            `Quick
            test_post_effect_response_failure_requires_recovery
        ; test_case
            "context overflow after tool records effect"
            `Quick
            test_context_overflow_after_tool_records_effect
        ; test_case
            "context overflow after response records activity"
            `Quick
            test_context_overflow_after_response_records_activity
        ; test_case
            "empty assistant keeps context overflow retry safe"
            `Quick
            test_context_overflow_after_empty_assistant_remains_retry_safe
        ; test_case
            "dynamic tool abort stops provider loop"
            `Quick
            test_dynamic_tool_abort_stops_the_provider_loop
        ; test_case "host stop carries the assistant usage sum" `Quick
            test_host_stop_carries_the_assistant_usage_sum
        ; test_case
            "shared bridge owns exact dispatch"
            `Quick
            test_shared_mcp_bridge_owns_exact_dispatch
        ; test_case
            "shared protocol is negotiated"
            `Quick
            test_shared_mcp_protocol_is_negotiated
        ; test_case
            "notification is acknowledged on the control channel"
            `Quick
            test_mcp_notification_is_acknowledged_on_the_control_channel
        ; test_case
            "unsupported control request fails closed"
            `Quick
            test_unsupported_control_request_fails_closed
        ; test_case
            "allowedTools tokenizer chars validated"
            `Quick
            test_dynamic_tool_tokenizer_chars_are_validated
        ] )
    ; ( "hard-cut"
      , [ test_case
            "tool progress keeps stream open"
            `Quick
            test_tool_progress_keeps_stream_open
        ; test_case
            "unknown stream type fails closed"
            `Quick
            test_unknown_stream_type_fails_closed
        ] )
    ; ( "images"
      , [ test_case "image reaches the CLI user message" `Quick
            test_image_reaches_the_cli_user_message
        ; test_case "unsupported media type is rejected" `Quick
            test_unsupported_image_media_type_is_rejected
        ] )
    ; "session", [ test_case "resume identity" `Quick test_resume_preserves_session_identity ]
    ; "live", [ test_case "subscription turn" `Slow test_live_subscription ]
    ]
;;
