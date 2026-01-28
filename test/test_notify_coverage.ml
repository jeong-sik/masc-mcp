(** Notify Module Coverage Tests

    Tests for macOS Notification system:
    - event type: Mention, Interrupt, PortalMessage, TaskCompleted, Custom
    - focus_payload record type
    - sanitize_token: shell-safe identifier sanitization
    - token_value: optional token extraction
    - is_truthy: boolean string parsing
    - escape_shell: shell string escaping
    - render_focus_template: template substitution
*)

open Alcotest

module Notify = Masc_mcp.Notify

(* ============================================================
   sanitize_token Tests
   ============================================================ *)

let test_sanitize_token_alphanumeric () =
  check string "alphanumeric" "abc123" (Notify.sanitize_token "abc123")

let test_sanitize_token_with_dash () =
  check string "with dash" "test-value" (Notify.sanitize_token "test-value")

let test_sanitize_token_with_underscore () =
  check string "with underscore" "test_value" (Notify.sanitize_token "test_value")

let test_sanitize_token_with_dot () =
  check string "with dot" "test.value" (Notify.sanitize_token "test.value")

let test_sanitize_token_removes_special () =
  check string "removes special" "testvalue" (Notify.sanitize_token "test@value!")

let test_sanitize_token_removes_spaces () =
  check string "removes spaces" "testvalue" (Notify.sanitize_token "test value")

let test_sanitize_token_empty () =
  check string "empty" "" (Notify.sanitize_token "")

let test_sanitize_token_all_special () =
  check string "all special" "" (Notify.sanitize_token "!@#$%^&*()")

let test_sanitize_token_mixed_case () =
  check string "mixed case" "TestValue" (Notify.sanitize_token "TestValue")

(* ============================================================
   token_value Tests
   ============================================================ *)

let test_token_value_some () =
  check string "some" "test" (Notify.token_value (Some "test"))

let test_token_value_none () =
  check string "none" "" (Notify.token_value None)

let test_token_value_sanitizes () =
  check string "sanitizes" "testvalue" (Notify.token_value (Some "test@value"))

let test_token_value_empty_some () =
  check string "empty some" "" (Notify.token_value (Some ""))

(* ============================================================
   is_truthy Tests
   ============================================================ *)

let test_is_truthy_1 () =
  check bool "1" true (Notify.is_truthy "1")

let test_is_truthy_true () =
  check bool "true" true (Notify.is_truthy "true")

let test_is_truthy_yes () =
  check bool "yes" true (Notify.is_truthy "yes")

let test_is_truthy_on () =
  check bool "on" true (Notify.is_truthy "on")

let test_is_truthy_y () =
  check bool "y" true (Notify.is_truthy "y")

let test_is_truthy_TRUE () =
  check bool "TRUE" true (Notify.is_truthy "TRUE")

let test_is_truthy_Yes () =
  check bool "Yes" true (Notify.is_truthy "Yes")

let test_is_truthy_0 () =
  check bool "0" false (Notify.is_truthy "0")

let test_is_truthy_false () =
  check bool "false" false (Notify.is_truthy "false")

let test_is_truthy_no () =
  check bool "no" false (Notify.is_truthy "no")

let test_is_truthy_empty () =
  check bool "empty" false (Notify.is_truthy "")

let test_is_truthy_whitespace () =
  check bool "with whitespace" true (Notify.is_truthy "  true  ")

(* ============================================================
   escape_shell Tests
   ============================================================ *)

let test_escape_shell_plain () =
  check string "plain" "test" (Notify.escape_shell "test")

let test_escape_shell_single_quote () =
  check string "single quote" "it'\\''s" (Notify.escape_shell "it's")

let test_escape_shell_newline () =
  check string "newline" "line1 line2" (Notify.escape_shell "line1\nline2")

let test_escape_shell_empty () =
  check string "empty" "" (Notify.escape_shell "")

let test_escape_shell_multiple_quotes () =
  check string "multiple quotes" "a'\\''b'\\''c" (Notify.escape_shell "a'b'c")

let test_escape_shell_special_chars () =
  (* Most special chars are kept as-is *)
  check string "special chars" "$PATH" (Notify.escape_shell "$PATH")

(* ============================================================
   render_focus_template Tests
   ============================================================ *)

let test_render_focus_template_target () =
  let payload : Notify.focus_payload = {
    target_agent = Some "claude";
    from_agent = None;
    task_id = None;
  } in
  let result = Notify.render_focus_template "focus {{target}}" payload in
  check string "target" "focus claude" result

let test_render_focus_template_from () =
  let payload : Notify.focus_payload = {
    target_agent = None;
    from_agent = Some "gemini";
    task_id = None;
  } in
  let result = Notify.render_focus_template "from {{from}}" payload in
  check string "from" "from gemini" result

let test_render_focus_template_task () =
  let payload : Notify.focus_payload = {
    target_agent = None;
    from_agent = None;
    task_id = Some "task-001";
  } in
  let result = Notify.render_focus_template "task {{task}}" payload in
  check string "task" "task task-001" result

let test_render_focus_template_all () =
  let payload : Notify.focus_payload = {
    target_agent = Some "claude";
    from_agent = Some "gemini";
    task_id = Some "task-001";
  } in
  let result = Notify.render_focus_template "{{target}} {{from}} {{task}}" payload in
  check string "all" "claude gemini task-001" result

let test_render_focus_template_none () =
  let payload : Notify.focus_payload = {
    target_agent = None;
    from_agent = None;
    task_id = None;
  } in
  let result = Notify.render_focus_template "empty: {{target}}{{from}}{{task}}" payload in
  check string "none" "empty: " result

let test_render_focus_template_no_placeholders () =
  let payload : Notify.focus_payload = {
    target_agent = Some "claude";
    from_agent = None;
    task_id = None;
  } in
  let result = Notify.render_focus_template "static text" payload in
  check string "no placeholders" "static text" result

let test_render_focus_template_sanitizes () =
  let payload : Notify.focus_payload = {
    target_agent = Some "claude@test";
    from_agent = None;
    task_id = None;
  } in
  let result = Notify.render_focus_template "agent: {{target}}" payload in
  check string "sanitizes" "agent: claudetest" result

(* ============================================================
   escape_applescript Tests
   ============================================================ *)

let test_escape_applescript_plain () =
  check string "plain" "test" (Notify.escape_applescript "test")

let test_escape_applescript_double_quote () =
  check string "double quote" "he said \\\"hello\\\"" (Notify.escape_applescript "he said \"hello\"")

let test_escape_applescript_backslash () =
  check string "backslash" "path\\\\to\\\\file" (Notify.escape_applescript "path\\to\\file")

let test_escape_applescript_newline () =
  check string "newline" "line1 line2" (Notify.escape_applescript "line1\nline2")

let test_escape_applescript_empty () =
  check string "empty" "" (Notify.escape_applescript "")

let test_escape_applescript_mixed () =
  check string "mixed" "say \\\"hi\\\" and \\\\" (Notify.escape_applescript "say \"hi\" and \\")

(* ============================================================
   agent_emoji Tests
   ============================================================ *)

let test_agent_emoji_claude () =
  check string "claude" "🟣" (Notify.agent_emoji "claude")

let test_agent_emoji_gemini () =
  check string "gemini" "🔵" (Notify.agent_emoji "gemini")

let test_agent_emoji_codex () =
  check string "codex" "🟢" (Notify.agent_emoji "codex")

let test_agent_emoji_system () =
  check string "system" "⚙️" (Notify.agent_emoji "system")

let test_agent_emoji_unknown () =
  check string "unknown" "🤖" (Notify.agent_emoji "unknown-agent")

let test_agent_emoji_empty () =
  check string "empty" "🤖" (Notify.agent_emoji "")

(* ============================================================
   event Type Tests
   ============================================================ *)

let test_event_mention () =
  let e : Notify.event = Mention {
    from_agent = "gemini";
    target_agent = Some "claude";
    message = "hello";
  } in
  match e with
  | Notify.Mention { from_agent; target_agent; message } ->
    check string "from_agent" "gemini" from_agent;
    check (option string) "target_agent" (Some "claude") target_agent;
    check string "message" "hello" message
  | _ -> fail "expected Mention"

let test_event_interrupt () =
  let e : Notify.event = Interrupt { agent = "claude"; action = "stop" } in
  match e with
  | Notify.Interrupt { agent; action } ->
    check string "agent" "claude" agent;
    check string "action" "stop" action
  | _ -> fail "expected Interrupt"

let test_event_portal_message () =
  let e : Notify.event = PortalMessage {
    from_agent = "codex";
    target_agent = None;
    message = "data";
  } in
  match e with
  | Notify.PortalMessage { from_agent; target_agent; message } ->
    check string "from_agent" "codex" from_agent;
    check (option string) "target_agent" None target_agent;
    check string "message" "data" message
  | _ -> fail "expected PortalMessage"

let test_event_task_completed () =
  let e : Notify.event = TaskCompleted { agent = "claude"; task_id = "task-001" } in
  match e with
  | Notify.TaskCompleted { agent; task_id } ->
    check string "agent" "claude" agent;
    check string "task_id" "task-001" task_id
  | _ -> fail "expected TaskCompleted"

let test_event_custom () =
  let e : Notify.event = Custom {
    title = "Title";
    subtitle = "Subtitle";
    message = "Message";
  } in
  match e with
  | Notify.Custom { title; subtitle; message } ->
    check string "title" "Title" title;
    check string "subtitle" "Subtitle" subtitle;
    check string "message" "Message" message
  | _ -> fail "expected Custom"

(* ============================================================
   focus_payload Record Tests
   ============================================================ *)

let test_focus_payload_all_some () =
  let p : Notify.focus_payload = {
    target_agent = Some "claude";
    from_agent = Some "gemini";
    task_id = Some "task-001";
  } in
  check (option string) "target" (Some "claude") p.target_agent;
  check (option string) "from" (Some "gemini") p.from_agent;
  check (option string) "task" (Some "task-001") p.task_id

let test_focus_payload_all_none () =
  let p : Notify.focus_payload = {
    target_agent = None;
    from_agent = None;
    task_id = None;
  } in
  check (option string) "target" None p.target_agent;
  check (option string) "from" None p.from_agent;
  check (option string) "task" None p.task_id

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Notify Coverage" [
    "sanitize_token", [
      test_case "alphanumeric" `Quick test_sanitize_token_alphanumeric;
      test_case "with dash" `Quick test_sanitize_token_with_dash;
      test_case "with underscore" `Quick test_sanitize_token_with_underscore;
      test_case "with dot" `Quick test_sanitize_token_with_dot;
      test_case "removes special" `Quick test_sanitize_token_removes_special;
      test_case "removes spaces" `Quick test_sanitize_token_removes_spaces;
      test_case "empty" `Quick test_sanitize_token_empty;
      test_case "all special" `Quick test_sanitize_token_all_special;
      test_case "mixed case" `Quick test_sanitize_token_mixed_case;
    ];
    "token_value", [
      test_case "some" `Quick test_token_value_some;
      test_case "none" `Quick test_token_value_none;
      test_case "sanitizes" `Quick test_token_value_sanitizes;
      test_case "empty some" `Quick test_token_value_empty_some;
    ];
    "is_truthy", [
      test_case "1" `Quick test_is_truthy_1;
      test_case "true" `Quick test_is_truthy_true;
      test_case "yes" `Quick test_is_truthy_yes;
      test_case "on" `Quick test_is_truthy_on;
      test_case "y" `Quick test_is_truthy_y;
      test_case "TRUE" `Quick test_is_truthy_TRUE;
      test_case "Yes" `Quick test_is_truthy_Yes;
      test_case "0" `Quick test_is_truthy_0;
      test_case "false" `Quick test_is_truthy_false;
      test_case "no" `Quick test_is_truthy_no;
      test_case "empty" `Quick test_is_truthy_empty;
      test_case "whitespace" `Quick test_is_truthy_whitespace;
    ];
    "escape_shell", [
      test_case "plain" `Quick test_escape_shell_plain;
      test_case "single quote" `Quick test_escape_shell_single_quote;
      test_case "newline" `Quick test_escape_shell_newline;
      test_case "empty" `Quick test_escape_shell_empty;
      test_case "multiple quotes" `Quick test_escape_shell_multiple_quotes;
      test_case "special chars" `Quick test_escape_shell_special_chars;
    ];
    "render_focus_template", [
      test_case "target" `Quick test_render_focus_template_target;
      test_case "from" `Quick test_render_focus_template_from;
      test_case "task" `Quick test_render_focus_template_task;
      test_case "all" `Quick test_render_focus_template_all;
      test_case "none" `Quick test_render_focus_template_none;
      test_case "no placeholders" `Quick test_render_focus_template_no_placeholders;
      test_case "sanitizes" `Quick test_render_focus_template_sanitizes;
    ];
    "escape_applescript", [
      test_case "plain" `Quick test_escape_applescript_plain;
      test_case "double quote" `Quick test_escape_applescript_double_quote;
      test_case "backslash" `Quick test_escape_applescript_backslash;
      test_case "newline" `Quick test_escape_applescript_newline;
      test_case "empty" `Quick test_escape_applescript_empty;
      test_case "mixed" `Quick test_escape_applescript_mixed;
    ];
    "agent_emoji", [
      test_case "claude" `Quick test_agent_emoji_claude;
      test_case "gemini" `Quick test_agent_emoji_gemini;
      test_case "codex" `Quick test_agent_emoji_codex;
      test_case "system" `Quick test_agent_emoji_system;
      test_case "unknown" `Quick test_agent_emoji_unknown;
      test_case "empty" `Quick test_agent_emoji_empty;
    ];
    "event", [
      test_case "mention" `Quick test_event_mention;
      test_case "interrupt" `Quick test_event_interrupt;
      test_case "portal message" `Quick test_event_portal_message;
      test_case "task completed" `Quick test_event_task_completed;
      test_case "custom" `Quick test_event_custom;
    ];
    "focus_payload", [
      test_case "all some" `Quick test_focus_payload_all_some;
      test_case "all none" `Quick test_focus_payload_all_none;
    ];
  ]
