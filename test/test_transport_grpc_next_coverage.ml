(** Transport gRPC Next Module Coverage Tests

    Tests for gRPC transport layer pure functions:
    - config type and constructors
    - service_status type
    - get_binding transport binding
    - make_error_response helper
    - protobuf encode/decode functions
    - planning_context_of_json parser
*)

open Alcotest

module Transport_grpc_next = Masc_mcp.Transport_grpc_next
module Transport = Masc_mcp.Transport
module Pb = Masc_mcp.Masc_pb.Masc.V1

(* ============================================================
   config Type Tests
   ============================================================ *)

let test_config_type () =
  let cfg : Transport_grpc_next.config = {
    host = "localhost";
    port = 9999;
    tls = false;
    cert_file = None;
    key_file = None;
  } in
  check string "host" "localhost" cfg.host;
  check int "port" 9999 cfg.port;
  check bool "tls" false cfg.tls

let test_config_with_tls () =
  let cfg : Transport_grpc_next.config = {
    host = "0.0.0.0";
    port = 443;
    tls = true;
    cert_file = Some "/path/to/cert.pem";
    key_file = Some "/path/to/key.pem";
  } in
  check bool "tls enabled" true cfg.tls;
  match cfg.cert_file, cfg.key_file with
  | Some c, Some k ->
    check bool "has cert" true (String.length c > 0);
    check bool "has key" true (String.length k > 0)
  | _ -> fail "expected Some"

(* ============================================================
   default_config Tests
   ============================================================ *)

let test_default_config_host () =
  check string "default host" "127.0.0.1" Transport_grpc_next.default_config.host

let test_default_config_port () =
  check int "default port" 9935 Transport_grpc_next.default_config.port

let test_default_config_tls () =
  check bool "default tls" false Transport_grpc_next.default_config.tls

let test_default_config_cert_file () =
  match Transport_grpc_next.default_config.cert_file with
  | None -> check bool "no cert" true true
  | Some _ -> fail "expected None"

let test_default_config_key_file () =
  match Transport_grpc_next.default_config.key_file with
  | None -> check bool "no key" true true
  | Some _ -> fail "expected None"

(* ============================================================
   tls_config Tests
   ============================================================ *)

let test_tls_config () =
  let cfg = Transport_grpc_next.tls_config
    ~host:"grpc.example.com"
    ~port:8443
    ~cert_file:"/etc/ssl/cert.pem"
    ~key_file:"/etc/ssl/key.pem" in
  check string "host" "grpc.example.com" cfg.host;
  check int "port" 8443 cfg.port;
  check bool "tls" true cfg.tls;
  match cfg.cert_file, cfg.key_file with
  | Some c, Some k ->
    check string "cert" "/etc/ssl/cert.pem" c;
    check string "key" "/etc/ssl/key.pem" k
  | _ -> fail "expected Some"

(* ============================================================
   service_status Type Tests
   ============================================================ *)

let test_check_dependencies () =
  match Transport_grpc_next.check_dependencies () with
  | Transport_grpc_next.Available -> check bool "available" true true
  | _ -> fail "expected Available"

let test_service_status_not_installed () =
  let _ : Transport_grpc_next.service_status = Transport_grpc_next.NotInstalled in
  check bool "NotInstalled exists" true true

let test_service_status_running () =
  let status : Transport_grpc_next.service_status =
    Transport_grpc_next.Running { host = "localhost"; port = 9935 } in
  match status with
  | Transport_grpc_next.Running r ->
    check string "host" "localhost" r.host;
    check int "port" 9935 r.port
  | _ -> fail "expected Running"

(* ============================================================
   get_binding Tests
   ============================================================ *)

let test_get_binding_default () =
  let binding = Transport_grpc_next.get_binding ~host:"127.0.0.1" ~port:9935 in
  check bool "protocol is Grpc" true (binding.protocol = Transport.Grpc);
  check string "url" "grpc://127.0.0.1:9935" binding.url;
  check (list (pair string string)) "options" [] binding.options

let test_get_binding_custom () =
  let binding = Transport_grpc_next.get_binding ~host:"example.com" ~port:443 in
  check string "url" "grpc://example.com:443" binding.url

let test_get_binding_ipv6 () =
  let binding = Transport_grpc_next.get_binding ~host:"::1" ~port:9935 in
  check string "url" "grpc://::1:9935" binding.url

(* ============================================================
   make_error_response Tests
   ============================================================ *)

let test_make_error_response_basic () =
  let response = Transport_grpc_next.make_error_response "Test error" in
  check bool "contains success false" true (String.length response > 0)

let test_make_error_response_content () =
  let response = Transport_grpc_next.make_error_response "Invalid request" in
  check bool "has success:false" true (String.sub response 0 20 = "{\"success\":false,\"me")

(* ============================================================
   Protobuf Encode Response Tests
   ============================================================ *)

let test_encode_status_response () =
  let msg = Pb.StatusResponse.make
    ~room_path:"/tmp/room"
    ~project_name:"test-project"
    ~message_count:42
    ()
  in
  let encoded = Transport_grpc_next.encode_status_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_join_response () =
  let msg = Pb.JoinResponse.make ~success:true ~message:"Joined" () in
  let encoded = Transport_grpc_next.encode_join_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_leave_response () =
  let msg = Pb.LeaveResponse.make ~success:true () in
  let encoded = Transport_grpc_next.encode_leave_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_add_task_response () =
  let msg = Pb.AddTaskResponse.make ~task_id:"task-001" ~success:true () in
  let encoded = Transport_grpc_next.encode_add_task_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_claim_task_response () =
  let msg = Pb.ClaimTaskResponse.make ~success:true ~message:"Claimed" () in
  let encoded = Transport_grpc_next.encode_claim_task_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_done_task_response () =
  let msg = Pb.DoneTaskResponse.make ~success:true ~message:"Done" () in
  let encoded = Transport_grpc_next.encode_done_task_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_cancel_task_response () =
  let msg = Pb.CancelTaskResponse.make ~success:true ~message:"Cancelled" () in
  let encoded = Transport_grpc_next.encode_cancel_task_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_broadcast_response () =
  let msg = Pb.BroadcastResponse.make ~success:true ~seq:42 () in
  let encoded = Transport_grpc_next.encode_broadcast_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_get_messages_response () =
  let messages = [
    Pb.Message.make ~id:"1" ~from:"agent1" ~content:"hello" ~timestamp:"2024-01-01" ~seq:1 ();
  ] in
  let msg = Pb.GetMessagesResponse.make ~messages () in
  let encoded = Transport_grpc_next.encode_get_messages_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_create_vote_response () =
  let msg = Pb.CreateVoteResponse.make ~vote_id:"vote-001" ~success:true () in
  let encoded = Transport_grpc_next.encode_create_vote_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_cast_vote_response () =
  let msg = Pb.CastVoteResponse.make ~success:true ~message:"Vote cast" () in
  let encoded = Transport_grpc_next.encode_cast_vote_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

(* ============================================================
   Protobuf Encode Planning Response Tests
   ============================================================ *)

let test_encode_init_plan_response () =
  let context = Pb.PlanningContext.make ~task_id:"task-001" () in
  let msg = Pb.InitPlanResponse.make ~success:true ~context () in
  let encoded = Transport_grpc_next.encode_init_plan_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_update_plan_response () =
  let context = Pb.PlanningContext.make ~task_id:"task-001" ~task_plan:"plan" () in
  let msg = Pb.UpdatePlanResponse.make ~success:true ~context () in
  let encoded = Transport_grpc_next.encode_update_plan_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_add_note_response () =
  let context = Pb.PlanningContext.make ~task_id:"task-001" ~notes:["note1"] () in
  let msg = Pb.AddNoteResponse.make ~success:true ~context () in
  let encoded = Transport_grpc_next.encode_add_note_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_get_plan_response () =
  let context = Pb.PlanningContext.make ~task_id:"task-001" () in
  let msg = Pb.GetPlanResponse.make ~success:true ~context ~markdown:"# Plan" () in
  let encoded = Transport_grpc_next.encode_get_plan_response msg in
  check bool "encoded non-empty" true (String.length encoded > 0)

(* ============================================================
   Protobuf Decode Request Tests
   ============================================================ *)

let test_decode_join_request_valid () =
  let request = Pb.JoinRequest.make ~agent_name:"claude" ~capabilities:["code"] () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.JoinRequest.to_proto request) in
  match Transport_grpc_next.decode_join_request encoded with
  | Some decoded -> check string "agent_name" "claude" decoded.agent_name
  | None -> fail "expected Some"

let test_decode_join_request_invalid () =
  match Transport_grpc_next.decode_join_request "invalid" with
  | None -> check bool "returns None" true true
  | Some _ -> fail "expected None"

let test_decode_leave_request_valid () =
  (* LeaveRequest.t = string (single field optimization) *)
  let request = Pb.LeaveRequest.make ~agent_name:"claude" () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.LeaveRequest.to_proto request) in
  match Transport_grpc_next.decode_leave_request encoded with
  | Some decoded -> check string "agent_name" "claude" decoded
  | None -> fail "expected Some"

let test_decode_add_task_request_valid () =
  let request = Pb.AddTaskRequest.make ~title:"Test" ~description:"Desc" ~priority:1 () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.AddTaskRequest.to_proto request) in
  match Transport_grpc_next.decode_add_task_request encoded with
  | Some decoded -> check string "title" "Test" decoded.title
  | None -> fail "expected Some"

let test_decode_claim_task_request_valid () =
  let request = Pb.ClaimTaskRequest.make ~agent_name:"claude" ~task_id:"task-001" () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.ClaimTaskRequest.to_proto request) in
  match Transport_grpc_next.decode_claim_task_request encoded with
  | Some decoded -> check string "task_id" "task-001" decoded.task_id
  | None -> fail "expected Some"

let test_decode_done_task_request_valid () =
  let request = Pb.DoneTaskRequest.make ~agent_name:"claude" ~task_id:"task-001" ~notes:"Done" () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.DoneTaskRequest.to_proto request) in
  match Transport_grpc_next.decode_done_task_request encoded with
  | Some decoded -> check string "notes" "Done" decoded.notes
  | None -> fail "expected Some"

let test_decode_cancel_task_request_valid () =
  let request = Pb.CancelTaskRequest.make ~agent_name:"claude" ~task_id:"task-001" ~reason:"Test" () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.CancelTaskRequest.to_proto request) in
  match Transport_grpc_next.decode_cancel_task_request encoded with
  | Some decoded -> check string "reason" "Test" decoded.reason
  | None -> fail "expected Some"

let test_decode_broadcast_request_valid () =
  let request = Pb.BroadcastRequest.make ~agent_name:"claude" ~message:"Hello" () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.BroadcastRequest.to_proto request) in
  match Transport_grpc_next.decode_broadcast_request encoded with
  | Some decoded -> check string "message" "Hello" decoded.message
  | None -> fail "expected Some"

let test_decode_get_messages_request_valid () =
  let request = Pb.GetMessagesRequest.make ~since_seq:10 ~limit:20 () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.GetMessagesRequest.to_proto request) in
  match Transport_grpc_next.decode_get_messages_request encoded with
  | Some decoded ->
    check int "since_seq" 10 decoded.since_seq;
    check int "limit" 20 decoded.limit
  | None -> fail "expected Some"

let test_decode_create_vote_request_valid () =
  let request = Pb.CreateVoteRequest.make ~proposer:"claude" ~topic:"Topic" ~options:["A"; "B"] ~required_votes:2 () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.CreateVoteRequest.to_proto request) in
  match Transport_grpc_next.decode_create_vote_request encoded with
  | Some decoded -> check string "topic" "Topic" decoded.topic
  | None -> fail "expected Some"

let test_decode_cast_vote_request_valid () =
  let request = Pb.CastVoteRequest.make ~agent_name:"claude" ~vote_id:"v1" ~choice:"A" () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.CastVoteRequest.to_proto request) in
  match Transport_grpc_next.decode_cast_vote_request encoded with
  | Some decoded -> check string "choice" "A" decoded.choice
  | None -> fail "expected Some"

(* ============================================================
   Protobuf Decode Planning Request Tests
   ============================================================ *)

let test_decode_init_plan_request_valid () =
  let task_id = "task-001" in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.InitPlanRequest.to_proto task_id) in
  match Transport_grpc_next.decode_init_plan_request encoded with
  | Some decoded -> check string "task_id" "task-001" decoded
  | None -> fail "expected Some"

let test_decode_update_plan_request_valid () =
  let request = Pb.UpdatePlanRequest.make ~task_id:"task-001" ~content:"Content" () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.UpdatePlanRequest.to_proto request) in
  match Transport_grpc_next.decode_update_plan_request encoded with
  | Some decoded -> check string "content" "Content" decoded.content
  | None -> fail "expected Some"

let test_decode_add_note_request_valid () =
  let request = Pb.AddNoteRequest.make ~task_id:"task-001" ~note:"Note" () in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.AddNoteRequest.to_proto request) in
  match Transport_grpc_next.decode_add_note_request encoded with
  | Some decoded -> check string "note" "Note" decoded.note
  | None -> fail "expected Some"

let test_decode_get_plan_request_valid () =
  let task_id = "task-001" in
  let encoded = Ocaml_protoc_plugin.Writer.contents (Pb.GetPlanRequest.to_proto task_id) in
  match Transport_grpc_next.decode_get_plan_request encoded with
  | Some decoded -> check string "task_id" "task-001" decoded
  | None -> fail "expected Some"

(* ============================================================
   Request Encoder Tests (for Client)
   ============================================================ *)

let test_encode_claim_task_request () =
  let request = Pb.ClaimTaskRequest.make ~agent_name:"claude" ~task_id:"task-001" () in
  let encoded = Transport_grpc_next.encode_claim_task_request request in
  check bool "encoded non-empty" true (String.length encoded > 0)

let test_encode_broadcast_request () =
  let request = Pb.BroadcastRequest.make ~agent_name:"claude" ~message:"Hello" () in
  let encoded = Transport_grpc_next.encode_broadcast_request request in
  check bool "encoded non-empty" true (String.length encoded > 0)

(* ============================================================
   planning_context_of_json Tests
   ============================================================ *)

let test_planning_context_of_json_full () =
  let json = `Assoc [
    ("task_plan", `String "Plan content");
    ("notes", `List [`String "note1"; `String "note2"]);
    ("deliverable", `String "Deliverable");
    ("created_at", `String "2024-01-01T00:00:00Z");
    ("updated_at", `String "2024-01-01T01:00:00Z");
  ] in
  let context = Transport_grpc_next.planning_context_of_json "task-001" json in
  check string "task_id" "task-001" context.task_id;
  check string "task_plan" "Plan content" context.task_plan;
  check (list string) "notes" ["note1"; "note2"] context.notes

let test_planning_context_of_json_empty () =
  let json = `Assoc [] in
  let context = Transport_grpc_next.planning_context_of_json "task-001" json in
  check string "task_id" "task-001" context.task_id;
  check string "task_plan default" "" context.task_plan

let test_planning_context_of_json_not_assoc () =
  let json = `String "not an object" in
  let context = Transport_grpc_next.planning_context_of_json "task-001" json in
  check string "task_id" "task-001" context.task_id

let test_planning_context_of_json_invalid_notes () =
  let json = `Assoc [("notes", `String "not a list")] in
  let context = Transport_grpc_next.planning_context_of_json "task-001" json in
  check (list string) "notes default" [] context.notes

let test_planning_context_of_json_mixed_notes () =
  let json = `Assoc [("notes", `List [`String "valid"; `Int 123; `String "also"])] in
  let context = Transport_grpc_next.planning_context_of_json "task-001" json in
  check (list string) "filters non-strings" ["valid"; "also"] context.notes

(* ============================================================
   Roundtrip Tests
   ============================================================ *)

let test_roundtrip_claim_task () =
  let original = Pb.ClaimTaskRequest.make ~agent_name:"claude" ~task_id:"task-123" () in
  let encoded = Transport_grpc_next.encode_claim_task_request original in
  match Transport_grpc_next.decode_claim_task_request encoded with
  | Some decoded ->
    check string "agent_name roundtrip" "claude" decoded.agent_name;
    check string "task_id roundtrip" "task-123" decoded.task_id
  | None -> fail "roundtrip failed"

let test_roundtrip_broadcast () =
  let original = Pb.BroadcastRequest.make ~agent_name:"gemini" ~message:"Test" () in
  let encoded = Transport_grpc_next.encode_broadcast_request original in
  match Transport_grpc_next.decode_broadcast_request encoded with
  | Some decoded ->
    check string "agent_name roundtrip" "gemini" decoded.agent_name;
    check string "message roundtrip" "Test" decoded.message
  | None -> fail "roundtrip failed"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Transport gRPC Next Coverage" [
    "config", [
      test_case "basic" `Quick test_config_type;
      test_case "with tls" `Quick test_config_with_tls;
    ];
    "default_config", [
      test_case "host" `Quick test_default_config_host;
      test_case "port" `Quick test_default_config_port;
      test_case "tls" `Quick test_default_config_tls;
      test_case "cert_file" `Quick test_default_config_cert_file;
      test_case "key_file" `Quick test_default_config_key_file;
    ];
    "tls_config", [
      test_case "create" `Quick test_tls_config;
    ];
    "service_status", [
      test_case "check_dependencies" `Quick test_check_dependencies;
      test_case "not_installed" `Quick test_service_status_not_installed;
      test_case "running" `Quick test_service_status_running;
    ];
    "get_binding", [
      test_case "default" `Quick test_get_binding_default;
      test_case "custom" `Quick test_get_binding_custom;
      test_case "ipv6" `Quick test_get_binding_ipv6;
    ];
    "make_error_response", [
      test_case "basic" `Quick test_make_error_response_basic;
      test_case "content" `Quick test_make_error_response_content;
    ];
    "encode_responses", [
      test_case "status" `Quick test_encode_status_response;
      test_case "join" `Quick test_encode_join_response;
      test_case "leave" `Quick test_encode_leave_response;
      test_case "add_task" `Quick test_encode_add_task_response;
      test_case "claim_task" `Quick test_encode_claim_task_response;
      test_case "done_task" `Quick test_encode_done_task_response;
      test_case "cancel_task" `Quick test_encode_cancel_task_response;
      test_case "broadcast" `Quick test_encode_broadcast_response;
      test_case "get_messages" `Quick test_encode_get_messages_response;
      test_case "create_vote" `Quick test_encode_create_vote_response;
      test_case "cast_vote" `Quick test_encode_cast_vote_response;
    ];
    "encode_planning_responses", [
      test_case "init_plan" `Quick test_encode_init_plan_response;
      test_case "update_plan" `Quick test_encode_update_plan_response;
      test_case "add_note" `Quick test_encode_add_note_response;
      test_case "get_plan" `Quick test_encode_get_plan_response;
    ];
    "decode_requests", [
      test_case "join_valid" `Quick test_decode_join_request_valid;
      test_case "join_invalid" `Quick test_decode_join_request_invalid;
      test_case "leave_valid" `Quick test_decode_leave_request_valid;
      test_case "add_task_valid" `Quick test_decode_add_task_request_valid;
      test_case "claim_task_valid" `Quick test_decode_claim_task_request_valid;
      test_case "done_task_valid" `Quick test_decode_done_task_request_valid;
      test_case "cancel_task_valid" `Quick test_decode_cancel_task_request_valid;
      test_case "broadcast_valid" `Quick test_decode_broadcast_request_valid;
      test_case "get_messages_valid" `Quick test_decode_get_messages_request_valid;
      test_case "create_vote_valid" `Quick test_decode_create_vote_request_valid;
      test_case "cast_vote_valid" `Quick test_decode_cast_vote_request_valid;
    ];
    "decode_planning_requests", [
      test_case "init_plan_valid" `Quick test_decode_init_plan_request_valid;
      test_case "update_plan_valid" `Quick test_decode_update_plan_request_valid;
      test_case "add_note_valid" `Quick test_decode_add_note_request_valid;
      test_case "get_plan_valid" `Quick test_decode_get_plan_request_valid;
    ];
    "encode_requests_client", [
      test_case "claim_task" `Quick test_encode_claim_task_request;
      test_case "broadcast" `Quick test_encode_broadcast_request;
    ];
    "planning_context_of_json", [
      test_case "full" `Quick test_planning_context_of_json_full;
      test_case "empty" `Quick test_planning_context_of_json_empty;
      test_case "not_assoc" `Quick test_planning_context_of_json_not_assoc;
      test_case "invalid_notes" `Quick test_planning_context_of_json_invalid_notes;
      test_case "mixed_notes" `Quick test_planning_context_of_json_mixed_notes;
    ];
    "roundtrip", [
      test_case "claim_task" `Quick test_roundtrip_claim_task;
      test_case "broadcast" `Quick test_roundtrip_broadcast;
    ];
  ]
