(** MASC MCP - Main Library Entry Point *)

module Log = Log
module Types = Types
module Room_utils = Room_utils
module Room = Room
module Session = Session
module Tools = Tools
module Mcp_server = Mcp_server
module Mcp_session = Mcp_session
module Sse = Sse
module Checkpoint_types = Checkpoint_types
module Checkpoint_fs = Checkpoint_fs
module Auth = Auth
module Retry = Retry
module Backend = Backend
module Mcp_protocol = Mcp_protocol
module Orchestrator = Orchestrator
module Spawn = Spawn
module Agent_card = Agent_card
module Planning = Planning
module Transport = Transport
(* module Transport_grpc = Transport_grpc *)  (** Disabled: h2 version conflict with httpun-eio (Lwt-based) *)
module Transport_grpc_next = Transport_grpc_next  (** gRPC-Eio with TLS + gzip support *)
module Masc_pb = Masc_pb  (** Generated protobuf types *)
module Encryption = Encryption

(* Enterprise Security - James Bond, Putin, Bill Gates feedback *)
module Security = Security

(* Phase 6: A2A MCP Tools *)
module A2a_tools = A2a_tools

(* Phase 7: MCP 2025-11-25 Spec Compliance *)
module Cancellation = Cancellation
module Subscriptions = Subscriptions
module Progress = Progress
module Shutdown = Shutdown

(* Phase 8: Mitosis - Cell Division for Infinite Lifecycle *)
module Mitosis = Mitosis

(* Phase 8b: Cellular Agent - Handover DNA Transfer *)
module Handover = Handover

(* Phase 9: Execution Memory *)
module Execution_memory = Execution_memory

(* Phase 11: Internal Caching *)
module Cache = Cache

(* Phase 12: Cluster Tempo Control *)
module Tempo = Tempo

(* Phase 13: Visual Dashboard *)
module Dashboard = Dashboard

(* Level 2: Organization - Holonic Architecture *)
module Metrics_store = Metrics_store
module Fitness = Fitness
module Hebbian = Hebbian
module Drift_guard = Drift_guard
module Telemetry = Telemetry

(* Level 2 MAGI Improvements *)
module Level2_config = Level2_config
module Metrics_cache = Metrics_cache
module Validation = Validation
module Response = Response

(* P1: Effect System (OCaml 5.3+) - Type-safe side effect tracking *)
module Effects = Effects

(* Voice Bridge for MASC-Voice Integration *)
module Voice_bridge = Voice_bridge

(* Turn Queue for Sequential Voice Output *)
module Turn_queue = Turn_queue

(* Voice Session Manager for Multi-Agent Sessions *)
module Voice_session_manager = Voice_session_manager

(* Voice Conference for Multi-Agent Voice Orchestration *)
module Voice_conference = Voice_conference

(* Voice Stream for WebSocket Audio Delivery *)
module Voice_stream = Voice_stream

(* WebRTC DataChannel for P2P Audio *)
module Webrtc_datachannel = Webrtc_datachannel

(* Level 3: Federation *)
module Federation = Federation

(* Level 4: Swarm - Emergent Collective Intelligence *)
module Level4_config = Level4_config
module Swarm = Swarm
module Swarm_behaviors = Swarm_behaviors

(* Level 5: Institution - Persistent Collective Memory *)
module Institution = Institution

(* Level 6: Mind - Meta-Cognition and Self-Awareness *)
module Mind = Mind

(* Level 7: Noosphere - Collective Intelligence *)
module Noosphere = Noosphere

(* Level 8: Meta - System of Systems *)
module Meta = Meta

(* Level 9: Void - The Groundless Ground (空) *)
module Void = Void

(* Pure OCaml WebRTC Protocol Stack *)
module Stun = Stun            (** RFC 5389 - Session Traversal Utilities for NAT *)
module Sdp = Sdp              (** RFC 8866 - Session Description Protocol *)
module Ice = Ice              (** RFC 8445 - Interactive Connectivity Establishment *)
module Dtls = Dtls            (** RFC 6347 - Datagram Transport Layer Security *)
module Dtls_crypto = Dtls_crypto  (** RFC 5288 - AES-GCM for DTLS *)
module Sctp = Sctp            (** RFC 4960 - Stream Control Transmission Protocol *)
module Sctp_transport = Sctp_transport  (** SCTP Transport with Flow Control *)
module Datachannel = Datachannel  (** RFC 8831/8832 - WebRTC Data Channels *)

(* OCaml 5.x Eio Direct-Style Modules *)
module Backend_eio = Backend_eio  (** Eio-native file storage backend *)
module Cache_eio = Cache_eio      (** Eio-native caching (pure sync) *)
module Hebbian_eio = Hebbian_eio  (** Eio-native Hebbian learning (pure sync) *)
module Http_server_eio = Http_server_eio  (** Eio-native HTTP server *)
module Mcp_server_eio = Mcp_server_eio    (** Eio-native MCP server *)
module Metrics_store_eio = Metrics_store_eio  (** Eio-native metrics (pure sync) *)
module Planning_eio = Planning_eio        (** Eio-native planning (pure sync) *)
module Room_eio = Room_eio        (** Eio-native room operations *)
module Session_eio = Session_eio  (** Eio-native session management *)
module Udp_socket_eio = Udp_socket_eio  (** Eio-native UDP sockets *)
module Ice_eio = Ice_eio  (** Eio-native ICE with Mutex protection *)
module Sctp_eio = Sctp_eio  (** Eio-native SCTP with Mutex protection *)
module Datachannel_eio = Datachannel_eio  (** Eio-native DataChannel with SCTP integration *)
(* TODO: These Eio modules need to be created:
   - Dtls_eio (Eio-native DTLS with Mutex protection)
   - Webrtc_stack_eio (Unified WebRTC stack) *)
