(** Turn Queue Tests *)

open Alcotest

let turn_status_testable =
  testable Masc_mcp.Turn_queue.pp_turn_status Masc_mcp.Turn_queue.equal_turn_status

(* === Queue Creation Tests === *)

let test_create_queue () =
  let q = Masc_mcp.Turn_queue.create () in
  check bool "empty queue" true (Masc_mcp.Turn_queue.is_empty q);
  check int "length is 0" 0 (Masc_mcp.Turn_queue.length q);
  check bool "not processing" false (Masc_mcp.Turn_queue.is_processing q)

let test_queue_constants () =
  check int "default priority" 1 Masc_mcp.Turn_queue.default_priority;
  check string "default voice" "Sarah" Masc_mcp.Turn_queue.default_voice

(* === Enqueue Tests === *)

let test_enqueue_single () =
  let q = Masc_mcp.Turn_queue.create () in
  let turn = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"Hello" () in
  check string "agent_id" "claude" turn.agent_id;
  check string "message" "Hello" turn.message;
  check string "voice" "Sarah" turn.voice;
  check int "priority" 1 turn.priority;
  check turn_status_testable "status" Masc_mcp.Turn_queue.Queued turn.status;
  check int "queue length" 1 (Masc_mcp.Turn_queue.length q)

let test_enqueue_with_options () =
  let q = Masc_mcp.Turn_queue.create () in
  let turn = Masc_mcp.Turn_queue.enqueue q 
    ~agent_id:"gemini" 
    ~message:"Test" 
    ~voice:"Roger" 
    ~priority:2 
    () 
  in
  check string "voice" "Roger" turn.voice;
  check int "priority" 2 turn.priority

let test_enqueue_multiple () =
  let q = Masc_mcp.Turn_queue.create () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"First" () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"gemini" ~message:"Second" () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"codex" ~message:"Third" () in
  check int "queue length" 3 (Masc_mcp.Turn_queue.length q)

(* === Priority Tests === *)

let test_priority_ordering () =
  let q = Masc_mcp.Turn_queue.create () in
  (* Add in reverse priority order *)
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"low" ~message:"Low" ~priority:3 () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"high" ~message:"High" ~priority:1 () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"mid" ~message:"Mid" ~priority:2 () in
  (* Dequeue should return highest priority (lowest number) first *)
  let first = Masc_mcp.Turn_queue.dequeue q in
  check (option string) "first is high" (Some "high") (Option.map (fun t -> t.Masc_mcp.Turn_queue.agent_id) first);
  let second = Masc_mcp.Turn_queue.dequeue q in
  check (option string) "second is mid" (Some "mid") (Option.map (fun t -> t.Masc_mcp.Turn_queue.agent_id) second);
  let third = Masc_mcp.Turn_queue.dequeue q in
  check (option string) "third is low" (Some "low") (Option.map (fun t -> t.Masc_mcp.Turn_queue.agent_id) third)

let test_auto_priority () =
  let q = Masc_mcp.Turn_queue.create () in
  let claude_turn = Masc_mcp.Turn_queue.enqueue_auto_priority q ~agent_id:"claude" ~message:"Claude" () in
  let codex_turn = Masc_mcp.Turn_queue.enqueue_auto_priority q ~agent_id:"codex" ~message:"Codex" () in
  let other_turn = Masc_mcp.Turn_queue.enqueue_auto_priority q ~agent_id:"other" ~message:"Other" () in
  check int "claude priority" 1 claude_turn.priority;
  check int "codex priority" 2 codex_turn.priority;
  check int "other priority" 3 other_turn.priority

(* === Dequeue Tests === *)

let test_dequeue_empty () =
  let q = Masc_mcp.Turn_queue.create () in
  check (option string) "dequeue empty" None 
    (Option.map (fun t -> t.Masc_mcp.Turn_queue.agent_id) (Masc_mcp.Turn_queue.dequeue q))

let test_dequeue_sets_speaking () =
  let q = Masc_mcp.Turn_queue.create () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"Test" () in
  let turn = Masc_mcp.Turn_queue.dequeue q in
  match turn with
  | None -> fail "should have turn"
  | Some t -> check turn_status_testable "status" Masc_mcp.Turn_queue.Speaking t.status

let test_peek () =
  let q = Masc_mcp.Turn_queue.create () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"Test" () in
  let peek1 = Masc_mcp.Turn_queue.peek q in
  let peek2 = Masc_mcp.Turn_queue.peek q in
  check (option string) "peek returns same" 
    (Option.map (fun t -> t.Masc_mcp.Turn_queue.id) peek1)
    (Option.map (fun t -> t.Masc_mcp.Turn_queue.id) peek2);
  check int "length unchanged" 1 (Masc_mcp.Turn_queue.length q)

(* === Turn Completion Tests === *)

let test_complete_turn () =
  let q = Masc_mcp.Turn_queue.create () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"Test" () in
  let turn = Masc_mcp.Turn_queue.dequeue q in
  Masc_mcp.Turn_queue.complete_turn q ~duration_ms:1500;
  match turn with
  | None -> fail "should have turn"
  | Some t ->
    check turn_status_testable "status completed" Masc_mcp.Turn_queue.Completed t.status;
    check (option int) "duration set" (Some 1500) t.duration_ms

let test_fail_turn () =
  let q = Masc_mcp.Turn_queue.create () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"Test" () in
  let turn = Masc_mcp.Turn_queue.dequeue q in
  Masc_mcp.Turn_queue.fail_turn q ~error:"TTS error";
  match turn with
  | None -> fail "should have turn"
  | Some t ->
    check turn_status_testable "status failed" (Masc_mcp.Turn_queue.Failed "TTS error") t.status

(* === Queue Management Tests === *)

let test_clear () =
  let q = Masc_mcp.Turn_queue.create () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"1" () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"gemini" ~message:"2" () in
  Masc_mcp.Turn_queue.clear q;
  check bool "empty after clear" true (Masc_mcp.Turn_queue.is_empty q)

let test_remove_agent_turns () =
  let q = Masc_mcp.Turn_queue.create () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"1" () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"gemini" ~message:"2" () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"3" () in
  Masc_mcp.Turn_queue.remove_agent_turns q ~agent_id:"claude";
  check int "only gemini left" 1 (Masc_mcp.Turn_queue.length q);
  let remaining = Masc_mcp.Turn_queue.get_all_turns q in
  check string "remaining is gemini" "gemini" (List.hd remaining).agent_id

let test_get_agent_turns () =
  let q = Masc_mcp.Turn_queue.create () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"1" () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"gemini" ~message:"2" () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"3" () in
  let claude_turns = Masc_mcp.Turn_queue.get_agent_turns q ~agent_id:"claude" in
  check int "claude has 2 turns" 2 (List.length claude_turns)

(* === JSON Status Tests === *)

let test_status_json () =
  let q = Masc_mcp.Turn_queue.create () in
  let _ = Masc_mcp.Turn_queue.enqueue q ~agent_id:"claude" ~message:"Test" () in
  let json = Masc_mcp.Turn_queue.status_json q in
  match json with
  | `Assoc fields ->
    check int "queue_length" 1 (
      match List.assoc "queue_length" fields with
      | `Int n -> n
      | _ -> -1
    );
    check bool "is_processing" false (
      match List.assoc "is_processing" fields with
      | `Bool b -> b
      | _ -> true
    )
  | _ -> fail "should be object"

(* === Test Runner === *)

let () =
  run "Turn Queue" [
    "create", [
      test_case "create empty queue" `Quick test_create_queue;
      test_case "constants" `Quick test_queue_constants;
    ];
    "enqueue", [
      test_case "single turn" `Quick test_enqueue_single;
      test_case "with options" `Quick test_enqueue_with_options;
      test_case "multiple turns" `Quick test_enqueue_multiple;
    ];
    "priority", [
      test_case "ordering" `Quick test_priority_ordering;
      test_case "auto priority" `Quick test_auto_priority;
    ];
    "dequeue", [
      test_case "empty queue" `Quick test_dequeue_empty;
      test_case "sets speaking" `Quick test_dequeue_sets_speaking;
      test_case "peek" `Quick test_peek;
    ];
    "completion", [
      test_case "complete turn" `Quick test_complete_turn;
      test_case "fail turn" `Quick test_fail_turn;
    ];
    "management", [
      test_case "clear" `Quick test_clear;
      test_case "remove agent turns" `Quick test_remove_agent_turns;
      test_case "get agent turns" `Quick test_get_agent_turns;
    ];
    "json", [
      test_case "status json" `Quick test_status_json;
    ];
  ]
