(** gRPC Transport Layer (Eio) - Protocol Buffers binding for MASC MCP

    This module provides gRPC transport support using Eio direct-style async
    for Multi-Agent Streaming Coordination (MASC). Built on top of grpc-eio-next
    which supports h2 >= 0.13 with TLS.

    {2 Architecture}

    The gRPC transport layer consists of:
    - Protobuf serialization/deserialization using [ocaml-protoc-plugin]
    - Service handlers that delegate to {!Room} module
    - H2-based TCP server with Eio async I/O
    - Client module for outbound RPC calls

    {2 Differences from legacy transport_grpc}

    - Uses Eio direct-style concurrency
    - Supports TLS via Tls_eio and Flow_handler
    - Compatible with h2 >= 0.13 (httpun-eio compatible)
    - Uses grpc-eio library from lib/grpc-eio-next

    @see <https://github.com/ocaml-multicore/eio> Eio documentation
*)

module Pb = Masc_pb.Masc.V1

(** {1 Configuration} *)

(** gRPC server configuration.

    @param host Bind address (default: 127.0.0.1)
    @param port Listen port (default: 9935, different from HTTP 8935)
    @param tls Enable TLS encryption
    @param cert_file Path to TLS certificate file (PEM format)
    @param key_file Path to TLS private key file (PEM format) *)
type config = {
  host: string;
  port: int;
  tls: bool;
  cert_file: string option;
  key_file: string option;
}

(** Default configuration for local development. *)
let default_config = {
  host = "127.0.0.1";
  port = 9935;
  tls = false;
  cert_file = None;
  key_file = None;
}

(** Create TLS-enabled configuration.

    @param host Bind address
    @param port Listen port
    @param cert_file Path to certificate file
    @param key_file Path to private key file
    @return Configuration with TLS enabled *)
let tls_config ~host ~port ~cert_file ~key_file = {
  host;
  port;
  tls = true;
  cert_file = Some cert_file;
  key_file = Some key_file;
}

(** Service availability status. *)
type service_status =
  | NotInstalled  (** gRPC dependencies not installed *)
  | Available     (** Ready to start *)
  | Running of { host: string; port: int }  (** Server is running *)

(** Check if gRPC dependencies are available.
    @return [Available] if all required libraries are installed *)
let check_dependencies () =
  Available

(** Get transport binding info for MCP Agent Card.
    @param host Server host
    @param port Server port
    @return Transport binding with gRPC protocol *)
let get_binding ~host ~port : Transport.binding =
  {
    Transport.protocol = Transport.Grpc;
    url = Printf.sprintf "grpc://%s:%d" host port;
    options = [];
  }

(** {1 Protobuf Serialization}

    Encode/decode functions for Protocol Buffer messages.
    Uses [ocaml-protoc-plugin] for type-safe serialization. *)

(** {2 Response Encoders} *)

let encode_status_response (msg : Pb.StatusResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.StatusResponse.to_proto msg)

let encode_join_response (msg : Pb.JoinResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.JoinResponse.to_proto msg)

let encode_leave_response (msg : Pb.LeaveResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.LeaveResponse.to_proto msg)

let encode_add_task_response (msg : Pb.AddTaskResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.AddTaskResponse.to_proto msg)

let encode_claim_task_response (msg : Pb.ClaimTaskResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.ClaimTaskResponse.to_proto msg)

let encode_done_task_response (msg : Pb.DoneTaskResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.DoneTaskResponse.to_proto msg)

let encode_cancel_task_response (msg : Pb.CancelTaskResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.CancelTaskResponse.to_proto msg)

let encode_broadcast_response (msg : Pb.BroadcastResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.BroadcastResponse.to_proto msg)

let encode_get_messages_response (msg : Pb.GetMessagesResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.GetMessagesResponse.to_proto msg)

let encode_create_vote_response (msg : Pb.CreateVoteResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.CreateVoteResponse.to_proto msg)

let encode_cast_vote_response (msg : Pb.CastVoteResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.CastVoteResponse.to_proto msg)

(** {2 Planning Response Encoders} *)

let encode_init_plan_response (msg : Pb.InitPlanResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.InitPlanResponse.to_proto msg)

let encode_update_plan_response (msg : Pb.UpdatePlanResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.UpdatePlanResponse.to_proto msg)

let encode_add_note_response (msg : Pb.AddNoteResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.AddNoteResponse.to_proto msg)

let encode_get_plan_response (msg : Pb.GetPlanResponse.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.GetPlanResponse.to_proto msg)

(** {2 Request Decoders}

    All decoders return [Option] - [None] on parse failure. *)

let decode_join_request (data : string) : Pb.JoinRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.JoinRequest.from_proto reader)

let decode_leave_request (data : string) : Pb.LeaveRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.LeaveRequest.from_proto reader)

let decode_add_task_request (data : string) : Pb.AddTaskRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.AddTaskRequest.from_proto reader)

let decode_claim_task_request (data : string) : Pb.ClaimTaskRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.ClaimTaskRequest.from_proto reader)

let decode_done_task_request (data : string) : Pb.DoneTaskRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.DoneTaskRequest.from_proto reader)

let decode_cancel_task_request (data : string) : Pb.CancelTaskRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.CancelTaskRequest.from_proto reader)

let decode_broadcast_request (data : string) : Pb.BroadcastRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.BroadcastRequest.from_proto reader)

let decode_get_messages_request (data : string) : Pb.GetMessagesRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.GetMessagesRequest.from_proto reader)

let decode_create_vote_request (data : string) : Pb.CreateVoteRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.CreateVoteRequest.from_proto reader)

let decode_cast_vote_request (data : string) : Pb.CastVoteRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.CastVoteRequest.from_proto reader)

(** {2 Planning Request Decoders} *)

let decode_init_plan_request (data : string) : Pb.InitPlanRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.InitPlanRequest.from_proto reader)

let decode_update_plan_request (data : string) : Pb.UpdatePlanRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.UpdatePlanRequest.from_proto reader)

let decode_add_note_request (data : string) : Pb.AddNoteRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.AddNoteRequest.from_proto reader)

let decode_get_plan_request (data : string) : Pb.GetPlanRequest.t option =
  let reader = Ocaml_protoc_plugin.Reader.create data in
  Result.to_option (Pb.GetPlanRequest.from_proto reader)

(** {2 Request Encoders (for Client)} *)

let encode_claim_task_request (msg : Pb.ClaimTaskRequest.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.ClaimTaskRequest.to_proto msg)

let encode_broadcast_request (msg : Pb.BroadcastRequest.t) : string =
  Ocaml_protoc_plugin.Writer.contents (Pb.BroadcastRequest.to_proto msg)

(** {1 RPC Handlers (Eio)}

    Each handler follows the pattern:
    1. Decode request (return error response on failure)
    2. Call Room API
    3. Encode response with success/failure status *)

(** Helper: Create error response *)
let make_error_response msg =
  Printf.sprintf "{\"success\":false,\"message\":\"%s\"}" msg

(** GetStatus handler - returns room status with agent/task counts. *)
let get_status_handler (room_config: Room.config) (request_data: string) : string =
  let _ = request_data in  (* Empty request *)
  let message_count =
    let msgs_path = Filename.concat room_config.base_path "messages" in
    if Sys.file_exists msgs_path && Sys.is_directory msgs_path then
      Array.length (Sys.readdir msgs_path)
    else
      0
  in
  let response = Pb.StatusResponse.make
    ~room_path:room_config.base_path
    ~project_name:room_config.backend_config.cluster_name
    ~message_count
    ()
  in
  encode_status_response response

(** Join handler - register agent in room. *)
let join_handler (room_config: Room.config) (data: string) : string =
  match decode_join_request data with
  | None -> make_error_response "Invalid JoinRequest"
  | Some req ->
    let result = Room.join room_config ~agent_name:req.agent_name ~capabilities:req.capabilities () in
    let response = Pb.JoinResponse.make ~success:true ~message:result () in
    encode_join_response response

(** Leave handler - acknowledge agent departure. *)
let leave_handler (_room_config: Room.config) (data: string) : string =
  match decode_leave_request data with
  | None -> make_error_response "Invalid LeaveRequest"
  | Some _req ->
    let response = Pb.LeaveResponse.make ~success:true () in
    encode_leave_response response

(** AddTask handler - create new task on quest board. *)
let add_task_handler (room_config: Room.config) (data: string) : string =
  match decode_add_task_request data with
  | None -> make_error_response "Invalid AddTaskRequest"
  | Some req ->
    let result = Room.add_task room_config ~title:req.title ~description:req.description ~priority:req.priority in
    let success = not (String.length result > 0 && result.[0] = '\xe2') in
    let response = Pb.AddTaskResponse.make ~task_id:result ~success () in
    encode_add_task_response response

(** ClaimTask handler - assign task to agent. *)
let claim_task_handler (room_config: Room.config) (data: string) : string =
  match decode_claim_task_request data with
  | None -> make_error_response "Invalid ClaimTaskRequest"
  | Some req ->
    let result = Room.claim_task_r room_config ~agent_name:req.agent_name ~task_id:req.task_id in
    let success, message = match result with
      | Ok msg -> (true, msg)
      | Error e -> (false, Types.masc_error_to_string e)
    in
    let response = Pb.ClaimTaskResponse.make ~success ~message () in
    encode_claim_task_response response

(** DoneTask handler - mark task as completed. *)
let done_task_handler (room_config: Room.config) (data: string) : string =
  match decode_done_task_request data with
  | None -> make_error_response "Invalid DoneTaskRequest"
  | Some req ->
    let result = Room.complete_task_r room_config ~agent_name:req.agent_name ~task_id:req.task_id ~notes:req.notes in
    let success, message = match result with
      | Ok msg -> (true, msg)
      | Error e -> (false, Types.masc_error_to_string e)
    in
    let response = Pb.DoneTaskResponse.make ~success ~message () in
    encode_done_task_response response

(** CancelTask handler - cancel a task. *)
let cancel_task_handler (room_config: Room.config) (data: string) : string =
  match decode_cancel_task_request data with
  | None -> make_error_response "Invalid CancelTaskRequest"
  | Some req ->
    let result = Room.cancel_task_r room_config ~agent_name:req.agent_name ~task_id:req.task_id ~reason:req.reason in
    let success, message = match result with
      | Ok msg -> (true, msg)
      | Error e -> (false, Types.masc_error_to_string e)
    in
    let response = Pb.CancelTaskResponse.make ~success ~message () in
    encode_cancel_task_response response

(** Broadcast handler - send message to all agents. *)
let broadcast_handler (room_config: Room.config) (data: string) : string =
  match decode_broadcast_request data with
  | None -> make_error_response "Invalid BroadcastRequest"
  | Some req ->
    let _msg = Room.broadcast room_config ~from_agent:req.agent_name ~content:req.message in
    let msgs_path = Filename.concat room_config.base_path "messages" in
    let seq =
      if Sys.file_exists msgs_path && Sys.is_directory msgs_path then
        Array.length (Sys.readdir msgs_path)
      else 0
    in
    let response = Pb.BroadcastResponse.make ~success:true ~seq () in
    encode_broadcast_response response

(** GetMessages handler - retrieve recent messages. *)
let get_messages_handler (room_config: Room.config) (data: string) : string =
  match decode_get_messages_request data with
  | None -> make_error_response "Invalid GetMessagesRequest"
  | Some req ->
    let msgs_path = Filename.concat room_config.base_path "messages" in
    let messages =
      if Sys.file_exists msgs_path && Sys.is_directory msgs_path then begin
        let files = Sys.readdir msgs_path |> Array.to_list |> List.sort compare |> List.rev in
        let count = ref 0 in
        List.filter_map (fun name ->
          if !count >= req.limit then None
          else begin
            let path = Filename.concat msgs_path name in
            try
              let json = Yojson.Safe.from_file path in
              match Types.message_of_yojson json with
              | Ok msg when msg.seq > req.since_seq ->
                incr count;
                Some (Pb.Message.make
                  ~id:(string_of_int msg.seq)
                  ~from:msg.from_agent
                  ~content:msg.content
                  ~timestamp:msg.timestamp
                  ~seq:msg.seq
                  ())
              | _ -> None
            with _ -> None
          end
        ) files
      end else []
    in
    let response = Pb.GetMessagesResponse.make ~messages () in
    encode_get_messages_response response

(** CreateVote handler - start a new vote. *)
let create_vote_handler (room_config: Room.config) (data: string) : string =
  match decode_create_vote_request data with
  | None -> make_error_response "Invalid CreateVoteRequest"
  | Some req ->
    let result = Room.vote_create room_config
      ~proposer:req.proposer
      ~topic:req.topic
      ~options:req.options
      ~required_votes:req.required_votes
    in
    let success = not (String.length result > 0 && result.[0] = '\xe2') in
    let response = Pb.CreateVoteResponse.make ~vote_id:result ~success () in
    encode_create_vote_response response

(** CastVote handler - cast a vote. *)
let cast_vote_handler (room_config: Room.config) (data: string) : string =
  match decode_cast_vote_request data with
  | None -> make_error_response "Invalid CastVoteRequest"
  | Some req ->
    let result = Room.vote_cast room_config
      ~agent_name:req.agent_name
      ~vote_id:req.vote_id
      ~choice:req.choice
    in
    let success = not (String.length result > 0 && result.[0] = '\xe2') in
    let response = Pb.CastVoteResponse.make ~success ~message:result () in
    encode_cast_vote_response response

(** {1 Planning Handlers} *)

let plans_dir config = Filename.concat config.Room.base_path "plans"

let ensure_plans_dir config =
  let dir = plans_dir config in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755

let plan_path config task_id =
  Filename.concat (plans_dir config) (task_id ^ ".json")

let read_plan config task_id =
  let path = plan_path config task_id in
  if Sys.file_exists path then
    try Some (Yojson.Safe.from_file path) with _ -> None
  else None

let write_plan config task_id json =
  ensure_plans_dir config;
  Yojson.Safe.to_file (plan_path config task_id) json

let planning_context_of_json task_id json =
  let get_str key default = match json with
    | `Assoc fields -> (match List.assoc_opt key fields with
        | Some (`String s) -> s | _ -> default)
    | _ -> default
  in
  let get_list key = match json with
    | `Assoc fields -> (match List.assoc_opt key fields with
        | Some (`List items) -> List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> [])
    | _ -> []
  in
  Pb.PlanningContext.make
    ~task_id
    ~task_plan:(get_str "task_plan" "")
    ~notes:(get_list "notes")
    ~deliverable:(get_str "deliverable" "")
    ~created_at:(get_str "created_at" "")
    ~updated_at:(get_str "updated_at" "")
    ()

(** InitPlan handler *)
let init_plan_handler (room_config: Room.config) (data: string) : string =
  match decode_init_plan_request data with
  | None -> make_error_response "Invalid InitPlanRequest"
  | Some task_id ->
    let now = Types.now_iso () in
    let json = `Assoc [
      ("task_plan", `String "");
      ("notes", `List []);
      ("deliverable", `String "");
      ("created_at", `String now);
      ("updated_at", `String now);
    ] in
    write_plan room_config task_id json;
    let context = planning_context_of_json task_id json in
    let response = Pb.InitPlanResponse.make ~success:true ~context () in
    encode_init_plan_response response

(** UpdatePlan handler *)
let update_plan_handler (room_config: Room.config) (data: string) : string =
  match decode_update_plan_request data with
  | None -> make_error_response "Invalid UpdatePlanRequest"
  | Some req ->
    let existing = read_plan room_config req.task_id |> Option.value ~default:(`Assoc []) in
    let now = Types.now_iso () in
    let json = match existing with
      | `Assoc fields ->
        let updated = List.map (fun (k, v) ->
          if k = "task_plan" then (k, `String req.content)
          else if k = "updated_at" then (k, `String now)
          else (k, v)
        ) fields in
        let has_plan = List.exists (fun (k, _) -> k = "task_plan") updated in
        if has_plan then `Assoc updated
        else `Assoc (("task_plan", `String req.content) :: ("updated_at", `String now) :: fields)
      | _ -> `Assoc [("task_plan", `String req.content); ("updated_at", `String now)]
    in
    write_plan room_config req.task_id json;
    let context = planning_context_of_json req.task_id json in
    let response = Pb.UpdatePlanResponse.make ~success:true ~context () in
    encode_update_plan_response response

(** AddNote handler *)
let add_note_handler (room_config: Room.config) (data: string) : string =
  match decode_add_note_request data with
  | None -> make_error_response "Invalid AddNoteRequest"
  | Some req ->
    let existing = read_plan room_config req.task_id |> Option.value ~default:(`Assoc []) in
    let now = Types.now_iso () in
    let json = match existing with
      | `Assoc fields ->
        let notes = match List.assoc_opt "notes" fields with
          | Some (`List items) -> items @ [`String req.note]
          | _ -> [`String req.note]
        in
        let updated = List.filter (fun (k, _) -> k <> "notes" && k <> "updated_at") fields in
        `Assoc (("notes", `List notes) :: ("updated_at", `String now) :: updated)
      | _ -> `Assoc [("notes", `List [`String req.note]); ("updated_at", `String now)]
    in
    write_plan room_config req.task_id json;
    let context = planning_context_of_json req.task_id json in
    let response = Pb.AddNoteResponse.make ~success:true ~context () in
    encode_add_note_response response

(** GetPlan handler *)
let get_plan_handler (room_config: Room.config) (data: string) : string =
  match decode_get_plan_request data with
  | None -> make_error_response "Invalid GetPlanRequest"
  | Some task_id ->
    match read_plan room_config task_id with
    | None ->
      let response = Pb.GetPlanResponse.make ~success:false ~markdown:"" () in
      encode_get_plan_response response
    | Some json ->
      let context = planning_context_of_json task_id json in
      let markdown = Buffer.create 256 in
      Buffer.add_string markdown (Printf.sprintf "# Plan: %s\n\n" task_id);
      Buffer.add_string markdown "## Task Plan\n";
      Buffer.add_string markdown (context.task_plan ^ "\n\n");
      Buffer.add_string markdown "## Notes\n";
      List.iter (fun note ->
        Buffer.add_string markdown (Printf.sprintf "- %s\n" note)
      ) context.notes;
      Buffer.add_string markdown "\n## Deliverable\n";
      Buffer.add_string markdown (context.deliverable ^ "\n");
      let response = Pb.GetPlanResponse.make
        ~success:true
        ~context
        ~markdown:(Buffer.contents markdown)
        ()
      in
      encode_get_plan_response response

(** {1 Service Registration (Eio)} *)

(** Create MASC gRPC service using grpc-eio.

    @param room_config Room configuration to pass to handlers
    @return Configured Grpc_eio.Service.t *)
let create_service (room_config: Room.config) : Grpc_eio.Service.t =
  Grpc_eio.Service.create "masc.v1.MASCService"
  |> Grpc_eio.Service.add_unary "GetStatus" (get_status_handler room_config)
  |> Grpc_eio.Service.add_unary "Join" (join_handler room_config)
  |> Grpc_eio.Service.add_unary "Leave" (leave_handler room_config)
  |> Grpc_eio.Service.add_unary "AddTask" (add_task_handler room_config)
  |> Grpc_eio.Service.add_unary "ClaimTask" (claim_task_handler room_config)
  |> Grpc_eio.Service.add_unary "DoneTask" (done_task_handler room_config)
  |> Grpc_eio.Service.add_unary "CancelTask" (cancel_task_handler room_config)
  |> Grpc_eio.Service.add_unary "Broadcast" (broadcast_handler room_config)
  |> Grpc_eio.Service.add_unary "GetMessages" (get_messages_handler room_config)
  |> Grpc_eio.Service.add_unary "CreateVote" (create_vote_handler room_config)
  |> Grpc_eio.Service.add_unary "CastVote" (cast_vote_handler room_config)
  |> Grpc_eio.Service.add_unary "InitPlan" (init_plan_handler room_config)
  |> Grpc_eio.Service.add_unary "UpdatePlan" (update_plan_handler room_config)
  |> Grpc_eio.Service.add_unary "AddNote" (add_note_handler room_config)
  |> Grpc_eio.Service.add_unary "GetPlan" (get_plan_handler room_config)

(** {1 Server (Eio)} *)

(** Server state *)
type server = {
  grpc_server: Grpc_eio.Server.t;
  config: config;
  mutable running: bool;
}

(** Start gRPC server using Eio.

    @param sw Eio switch for fiber management
    @param env Eio environment
    @param config Server configuration
    @param room_config Room configuration
    @return Server instance *)
let start_server ~sw ~env config room_config : server =
  Eio.traceln "🔌 Starting gRPC server (Eio) on %s:%d" config.host config.port;

  let service = create_service room_config in

  (* Build server config *)
  let grpc_config : Grpc_eio.Server.config = {
    host = config.host;
    port = config.port;
    codecs = [Grpc_core.Codec.gzip (); Grpc_core.Codec.identity];
    max_message_size = 4 * 1024 * 1024;
    default_timeout = Some Grpc_core.Timeout.default;
    tls = (match config.tls, config.cert_file, config.key_file with
      | true, Some cert, Some key ->
        Some { Grpc_eio.Tls_config.cert_file = cert; key_file = key; ca_file = None; client_auth = NoClientCert }
      | _ -> None);
  } in

  let grpc_server = Grpc_eio.Server.create ~config:grpc_config ()
    |> Grpc_eio.Server.add_service service
  in

  Eio.traceln "✅ gRPC service registered: /masc.v1.MASCService";
  Eio.traceln "   Core: GetStatus, Join, Leave";
  Eio.traceln "   Tasks: AddTask, ClaimTask, DoneTask, CancelTask";
  Eio.traceln "   Messaging: Broadcast, GetMessages";
  Eio.traceln "   Voting: CreateVote, CastVote";
  Eio.traceln "   Planning: InitPlan, UpdatePlan, AddNote, GetPlan";

  (* Start server in background fiber *)
  Eio.Fiber.fork ~sw (fun () ->
    Grpc_eio.Server.serve ~sw ~env grpc_server
  );

  { grpc_server; config; running = true }

(** Stop gRPC server. *)
let stop_server server =
  if server.running then begin
    Eio.traceln "🛑 Stopping gRPC server...";
    Grpc_eio.Server.shutdown server.grpc_server;
    server.running <- false
  end

(** Check if server is running. *)
let is_running server = server.running

(** {1 Client (Eio)} *)

module Client = struct
  (** Client connection state.
      Note: sw and env must be provided for each call since Eio requires them. *)
  type t = {
    config: config;
    client: Grpc_eio.Client.t;
  }

  (** Connect to gRPC server.

      @param sw Eio switch
      @param env Eio environment
      @param config Server configuration
      @return Connected client *)
  let connect ~sw ~env config : t =
    Eio.traceln "🔌 gRPC client connecting to %s:%d" config.host config.port;
    let target = Printf.sprintf "http://%s:%d" config.host config.port in
    let client = Grpc_eio.Client.connect ~sw ~env target in
    { config; client }

  (** Close client connection. *)
  let close client =
    Grpc_eio.Client.close client.client;
    Eio.traceln "🔌 gRPC client disconnected"

  (** Call a unary RPC method.

      @param sw Eio switch for this call
      @param env Eio environment
      @param client Connected client
      @param service Service name
      @param method_ Method name
      @param request Serialized protobuf request
      @return Result with serialized response or gRPC status *)
  let call_unary ~sw ~env client ~service ~method_ ~request =
    Grpc_eio.Client.call_unary ~sw ~env client.client ~service ~method_ ~request

  (** Claim a task via gRPC.

      @param sw Eio switch
      @param env Eio environment
      @param client Connected client
      @param agent_name Agent claiming the task
      @param task_id Task to claim
      @return Result with response or error *)
  let claim_task ~sw ~env client ~agent_name ~task_id =
    let request = Pb.ClaimTaskRequest.make ~agent_name ~task_id () in
    let encoded = encode_claim_task_request request in
    call_unary ~sw ~env client ~service:"masc.v1.MASCService" ~method_:"ClaimTask" ~request:encoded

  (** Broadcast a message via gRPC.

      @param sw Eio switch
      @param env Eio environment
      @param client Connected client
      @param agent_name Sender agent
      @param message Message content
      @return Result with response or error *)
  let broadcast ~sw ~env client ~agent_name ~message =
    let request = Pb.BroadcastRequest.make ~agent_name ~message () in
    let encoded = encode_broadcast_request request in
    call_unary ~sw ~env client ~service:"masc.v1.MASCService" ~method_:"Broadcast" ~request:encoded
end
