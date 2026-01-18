[@@@warning "-32"]  (* Suppress unused value warnings for exported API *)

(** RFC 8445 ICE - Eio-Native Implementation with Thread-Safe State

    This module provides an Eio-native ICE agent with:
    - Eio.Mutex for thread-safe state access
    - Direct-style async (no Lwt monads)
    - Integration with Stun module for candidate gathering

    Example usage:
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let net = Eio.Stdenv.net env in

      (* Create agent *)
      let agent = Ice_eio.create () in

      (* Register callbacks *)
      Ice_eio.on_candidate agent (fun c ->
        Printf.printf "New candidate: %s:%d\n" c.address c.port
      );

      (* Gather local candidates *)
      let candidates = Ice_eio.gather_candidates ~net ~sw agent in

      (* Add remote candidates (from SDP) *)
      List.iter (Ice_eio.add_remote_candidate agent) remote_candidates;

      (* Form pairs and run checks *)
      let _pairs = Ice_eio.form_pairs agent in
      match Ice_eio.run_checks ~net ~sw agent with
      | Ok pair -> Printf.printf "Connected via %s:%d\n" pair.local.address pair.local.port
      | Error e -> Printf.eprintf "ICE failed: %s\n" e
    ]}

    @author Second Brain
    @since MASC v3.2
*)

(** {1 Types} *)

(** Thread-safe ICE agent *)
type t

(** {1 Creation} *)

(** Create a new thread-safe ICE agent.
    @param config Optional ICE configuration (default: Google STUN server)
*)
val create : ?config:Ice.config -> unit -> t

(** {1 State Access (Thread-Safe)} *)

(** Get current connection state *)
val get_state : t -> Ice.connection_state

(** Set connection state *)
val set_state : t -> Ice.connection_state -> unit

(** Get gathering state *)
val get_gathering_state : t -> Ice.gathering_state

(** Get local candidates *)
val get_local_candidates : t -> Ice.candidate list

(** Get remote candidates *)
val get_remote_candidates : t -> Ice.candidate list

(** Get all candidate pairs *)
val get_pairs : t -> Ice.candidate_pair list

(** Get nominated pair (after successful negotiation) *)
val get_nominated_pair : t -> Ice.candidate_pair option

(** Get local ICE credentials (ufrag, pwd) *)
val get_local_credentials : t -> string * string

(** {1 State Mutation (Thread-Safe)} *)

(** Set remote ICE credentials from SDP *)
val set_remote_credentials : t -> ufrag:string -> pwd:string -> unit

(** Add a remote candidate from SDP *)
val add_remote_candidate : t -> Ice.candidate -> unit

(** Close the agent and release resources *)
val close : t -> unit

(** Restart ICE (generate new credentials, clear candidates) *)
val restart : t -> unit

(** Set connection state (for testing/simulation) *)
val set_state : t -> Ice.connection_state -> unit

(** {1 Candidate Gathering (Eio-Native)} *)

(** Gather local candidates using STUN.

    This queries configured STUN servers to discover server-reflexive
    candidates and enumerates local interfaces for host candidates.

    @param net Eio network capability
    @param sw Eio switch for resource management
    @return List of gathered local candidates
*)
val gather_candidates :
  net:Eio_unix.Net.t ->
  sw:Eio.Switch.t ->
  t ->
  Ice.candidate list

(** {1 Connectivity Checks (Eio-Native)} *)

(** Form candidate pairs from local and remote candidates.
    Must be called after adding remote candidates.
    @return Sorted list of candidate pairs (by priority)
*)
val form_pairs : t -> Ice.candidate_pair list

(** Perform connectivity check on a single pair.
    @param net Eio network capability
    @param sw Eio switch
    @return Updated pair with success/failure state
*)
val check_pair :
  net:Eio_unix.Net.t ->
  sw:Eio.Switch.t ->
  t ->
  Ice.candidate_pair ->
  (Ice.candidate_pair, string) result

(** Run connectivity checks on all pairs.
    Implements the ICE algorithm to find a working path.
    @param net Eio network capability
    @param sw Eio switch
    @return Nominated pair or error
*)
val run_checks :
  net:Eio_unix.Net.t ->
  sw:Eio.Switch.t ->
  t ->
  (Ice.candidate_pair, string) result

(** {1 Full Negotiation} *)

(** Full ICE negotiation: gather + check.

    This is the main entry point for ICE negotiation.
    Caller should add remote candidates before calling this.

    @param net Eio network capability
    @param sw Eio switch
    @return Nominated candidate pair or error
*)
val negotiate :
  net:Eio_unix.Net.t ->
  sw:Eio.Switch.t ->
  t ->
  (Ice.candidate_pair, string) result

(** {1 Event Callbacks} *)

(** Register callback for new local candidates *)
val on_candidate : t -> (Ice.candidate -> unit) -> unit

(** Register callback for state changes *)
val on_state_change : t -> (Ice.connection_state -> unit) -> unit

(** {1 Debug/Status} *)

(** Get agent status as JSON *)
val status_json : t -> Yojson.Safe.t

(** Pretty print agent status *)
val pp : Format.formatter -> t -> unit
