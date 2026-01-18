(** Session_eio Tests - Eio-native session management *)

open Masc_mcp.Session_eio

let test_create_registry () =
  let registry = create () in
  Alcotest.(check int) "no sessions" 0 (List.length (connected_agents registry))

let test_register_session () =
  Eio_main.run @@ fun _ ->
  let registry = create () in
  let session = register registry ~agent_name:"claude" in
  Alcotest.(check string) "agent name" "claude" session.agent_name;
  Alcotest.(check int) "one session" 1 (List.length (connected_agents registry))

let test_unregister_session () =
  Eio_main.run @@ fun _ ->
  let registry = create () in
  let _ = register registry ~agent_name:"claude" in
  unregister registry ~agent_name:"claude";
  Alcotest.(check int) "no sessions" 0 (List.length (connected_agents registry))

let test_update_activity () =
  Eio_main.run @@ fun _ ->
  let registry = create () in
  let session = register registry ~agent_name:"claude" in
  let initial_time = session.last_activity in
  Unix.sleepf 0.1;
  update_activity registry ~agent_name:"claude" ();
  Alcotest.(check bool) "activity updated" true (session.last_activity > initial_time)

let test_find_opt () =
  Eio_main.run @@ fun _ ->
  let registry = create () in
  let _ = register registry ~agent_name:"claude" in
  Alcotest.(check bool) "found" true (Option.is_some (find_opt registry "claude"));
  Alcotest.(check bool) "not found" true (Option.is_none (find_opt registry "gemini"))

let test_rate_limit () =
  let registry = create () in
  let (allowed, wait) = check_rate_limit registry ~agent_name:"claude" in
  Alcotest.(check bool) "first request allowed" true allowed;
  Alcotest.(check int) "no wait" 0 wait

let test_rate_limit_burst () =
  let config = { Masc_mcp.Types.default_rate_limit with per_minute = 2; burst_allowed = 1 } in
  let registry = create ~config () in
  (* First two requests *)
  let _ = check_rate_limit registry ~agent_name:"claude" in
  let _ = check_rate_limit registry ~agent_name:"claude" in
  (* Third uses burst *)
  let (allowed, _) = check_rate_limit registry ~agent_name:"claude" in
  Alcotest.(check bool) "burst allowed" true allowed;
  (* Fourth should be denied *)
  let (denied, wait) = check_rate_limit registry ~agent_name:"claude" in
  Alcotest.(check bool) "fourth denied" false denied;
  Alcotest.(check bool) "has wait time" true (wait > 0)

let test_push_pop_message () =
  Eio_main.run @@ fun _ ->
  let registry = create () in
  let _ = register registry ~agent_name:"claude" in
  let _ = register registry ~agent_name:"gemini" in
  let targets = push_message registry ~from_agent:"claude" ~content:"Hello" ~mention:None in
  Alcotest.(check int) "one target" 1 (List.length targets);
  Alcotest.(check bool) "gemini got message" true (List.mem "gemini" targets);
  (* Pop message *)
  let msg = pop_message registry ~agent_name:"gemini" in
  Alcotest.(check bool) "message received" true (Option.is_some msg)

let test_push_with_mention () =
  Eio_main.run @@ fun _ ->
  let registry = create () in
  let _ = register registry ~agent_name:"claude" in
  let _ = register registry ~agent_name:"gemini" in
  let _ = register registry ~agent_name:"codex" in
  let targets = push_message registry ~from_agent:"claude" ~content:"@gemini hello" ~mention:(Some "gemini") in
  Alcotest.(check int) "only gemini" 1 (List.length targets);
  Alcotest.(check bool) "codex no message" true (Option.is_none (pop_message registry ~agent_name:"codex"))

let test_wait_for_message_timeout () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let registry = create () in
  let _ = register registry ~agent_name:"claude" in
  let start = Unix.gettimeofday () in
  (* timeout of 3s with 2s check interval means it will timeout after ~2-4s *)
  let msg = wait_for_message ~clock registry ~agent_name:"claude" ~timeout:3.0 in
  let elapsed = Unix.gettimeofday () -. start in
  Alcotest.(check bool) "no message" true (Option.is_none msg);
  (* Should take 2-4 seconds due to check interval *)
  Alcotest.(check bool) "waited ~3s" true (elapsed >= 2.0 && elapsed < 5.0)

let test_inactive_agents () =
  Eio_main.run @@ fun _ ->
  let registry = create () in
  let session = register registry ~agent_name:"claude" in
  session.last_activity <- Unix.gettimeofday () -. 200.0;  (* 200s ago *)
  let inactive = get_inactive_agents registry ~threshold:120.0 in
  Alcotest.(check int) "one inactive" 1 (List.length inactive);
  Alcotest.(check bool) "claude inactive" true (List.mem "claude" inactive)

let string_contains s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

let test_status_string () =
  Eio_main.run @@ fun _ ->
  let registry = create () in
  let _ = register registry ~agent_name:"claude" in
  let status = status_string registry in
  Alcotest.(check bool) "contains claude" true (string_contains status "claude");
  Alcotest.(check bool) "connected agents header" true (String.length status > 0)

let test_restore_from_disk () =
  Eio_main.run @@ fun _ ->
  let registry = create () in
  (* Create temp dir with agent files *)
  let temp_dir = Filename.temp_dir "masc_test_" "" in
  let agent_file = Filename.concat temp_dir "test_agent.json" in
  let oc = open_out agent_file in
  output_string oc "{}";
  close_out oc;
  restore_from_disk registry ~agents_path:temp_dir;
  let agents = connected_agents registry in
  Alcotest.(check bool) "agent restored" true (List.mem "test_agent" agents);
  (* Cleanup *)
  Sys.remove agent_file;
  Unix.rmdir temp_dir

let () = Mirage_crypto_rng_unix.use_default ()

let test_mcp_session_create () =
  let session = McpSessionStore.create ~agent_name:"claude" () in
  Alcotest.(check bool) "has id" true (String.length session.McpSessionStore.id > 0);
  Alcotest.(check bool) "starts with mcp_" true (String.sub session.McpSessionStore.id 0 4 = "mcp_");
  Alcotest.(check bool) "has agent" true (session.McpSessionStore.agent_name = Some "claude")

let test_mcp_session_get () =
  let session = McpSessionStore.create () in
  let retrieved = McpSessionStore.get session.McpSessionStore.id in
  Alcotest.(check bool) "found" true (Option.is_some retrieved);
  Alcotest.(check bool) "same id" true (Option.map (fun (s : McpSessionStore.mcp_session) -> s.id) retrieved = Some session.id)

let test_mcp_session_cleanup () =
  let _ = McpSessionStore.create () in
  let removed = McpSessionStore.cleanup_stale () in
  (* New session shouldn't be stale *)
  Alcotest.(check int) "no stale" 0 removed

let tests = [
  "registry", [
    Alcotest.test_case "create" `Quick test_create_registry;
    Alcotest.test_case "register" `Quick test_register_session;
    Alcotest.test_case "unregister" `Quick test_unregister_session;
    Alcotest.test_case "update activity" `Quick test_update_activity;
    Alcotest.test_case "find_opt" `Quick test_find_opt;
  ];
  "rate_limit", [
    Alcotest.test_case "basic" `Quick test_rate_limit;
    Alcotest.test_case "burst" `Quick test_rate_limit_burst;
  ];
  "message", [
    Alcotest.test_case "push/pop" `Quick test_push_pop_message;
    Alcotest.test_case "mention" `Quick test_push_with_mention;
    Alcotest.test_case "wait timeout" `Slow test_wait_for_message_timeout;
  ];
  "status", [
    Alcotest.test_case "inactive agents" `Quick test_inactive_agents;
    Alcotest.test_case "status string" `Quick test_status_string;
    Alcotest.test_case "restore from disk" `Quick test_restore_from_disk;
  ];
  "mcp_session", [
    Alcotest.test_case "create" `Quick test_mcp_session_create;
    Alcotest.test_case "get" `Quick test_mcp_session_get;
    Alcotest.test_case "cleanup" `Quick test_mcp_session_cleanup;
  ];
]

let () = Alcotest.run "Session_eio" tests
