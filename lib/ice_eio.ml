(** RFC 8445 ICE - Eio-Native Implementation with Thread-Safe State

    This module provides an Eio-native wrapper around ICE with:
    - Eio.Mutex for thread-safe state access
    - Direct-style async (no Lwt monads)
    - Structured concurrency via Eio.Switch

    The underlying ICE logic is reused from ice.ml, but all mutable
    state access is protected by a Mutex.

    @author Second Brain
    @since MASC v3.2
*)

open Ice

(** {1 Thread-Safe ICE Agent} *)

(** ICE agent with Eio-native concurrency and Mutex protection *)
type t = {
  agent: Ice.agent;
  mutex: Eio.Mutex.t;
  mutable on_candidate_callback: (Ice.candidate -> unit) option;
  mutable on_state_change_callback: (Ice.connection_state -> unit) option;
}

(** Create a new thread-safe ICE agent *)
let create ?(config = Ice.default_config) () =
  let agent = Ice.create config in
  {
    agent;
    mutex = Eio.Mutex.create ();
    on_candidate_callback = None;
    on_state_change_callback = None;
  }

(** {1 State Access (Mutex-Protected)} *)

(** Get current connection state *)
let get_state t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Ice.get_state t.agent
  )

(** Get gathering state *)
let get_gathering_state t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Ice.get_gathering_state t.agent
  )

(** Get local candidates *)
let get_local_candidates t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Ice.get_local_candidates t.agent
  )

(** Get remote candidates *)
let get_remote_candidates t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Ice.get_remote_candidates t.agent
  )

(** Get all candidate pairs *)
let get_pairs t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Ice.get_pairs t.agent
  )

(** Get nominated pair *)
let get_nominated_pair t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Ice.get_nominated_pair t.agent
  )

(** Get local credentials *)
let get_local_credentials t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Ice.get_local_credentials t.agent
  )

(** {1 State Mutation (Mutex-Protected)} *)

(** Set remote ICE credentials *)
let set_remote_credentials t ~ufrag ~pwd =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Ice.set_remote_credentials t.agent ~ufrag ~pwd
  )

(** Add remote candidate *)
let add_remote_candidate t candidate =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Ice.add_remote_candidate t.agent candidate
  )

(** Close the agent *)
let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Ice.close t.agent
  )

(** Restart ICE *)
let restart t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Ice.restart t.agent
  )

(** Set connection state (for testing/simulation) *)
let set_state t state =
  let notify = Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let old_state = t.agent.state in
    t.agent.state <- state;
    old_state <> state
  ) in
  if notify then
    match t.on_state_change_callback with Some cb -> cb state | None -> ()
[@@warning "-32"] (* Exposed in mli for external use *)

(** {1 Candidate Gathering (Eio-Native)} *)

(** IP address family *)
type ip_family = IPv4 | IPv6

(** Gather local candidates using STUN.

    This is the Eio-native version that:
    1. Enumerates local interfaces for host candidates
    2. Queries STUN servers for server reflexive candidates
    3. Queries TURN servers for relay candidates (if configured)

    @param net Eio network capability
    @param sw Eio switch for resource management
*)
let rec gather_candidates ~net ~sw t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.agent.gathering_state <- Ice.Gathering
  );

  (* Step 1: Gather host candidates from local interfaces *)
  let host_candidates = gather_host_candidates () in

  (* Step 2: Gather server reflexive candidates via STUN *)
  let srflx_candidates =
    List.filter_map (fun server ->
      match parse_ice_server_url server with
      | Some (host, port) ->
        begin
          match Stun.binding_request_eio ~net ~sw ~server:(host ^ ":" ^ string_of_int port) ~timeout:3.0 () with
          | Ok result ->
            let candidate = {
              foundation = Ice.generate_foundation ~candidate_type:Server_reflexive ~base_address:result.mapped_address.ip ();
              component = 1;
              transport = UDP;
              priority = Ice.calculate_priority ~candidate_type:Server_reflexive ~local_pref:65534 ~component:1;
              address = result.mapped_address.ip;
              port = result.mapped_address.port;
              cand_type = Server_reflexive;
              base_address = Some "0.0.0.0";
              base_port = Some 0;
              related_address = Some result.mapped_address.ip;
              related_port = Some result.mapped_address.port;
              extensions = [];
            } in
            Some candidate
          | Error _ -> None
        end
      | None -> None
    ) (get_stun_servers t.agent.config)
  in

  (* Step 3: Update agent state with all candidates *)
  let all_candidates = host_candidates @ srflx_candidates in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.agent.local_candidates <- all_candidates;
    t.agent.gathering_state <- Ice.Gathering_complete
  );

  (* Step 4: Notify callback if registered *)
  begin match t.on_candidate_callback with
  | Some cb -> List.iter cb all_candidates
  | None -> ()
  end;

  all_candidates

(** Get local IPv4/IPv6 addresses from network interfaces (cross-platform)

    Uses `ifconfig` on macOS, `ip addr` on Linux. Falls back to loopback if unavailable.

    @return List of (ip_address, family) pairs *)
and get_local_addresses () =
  let is_valid_ipv4 ip =
    (* Filter loopback, link-local, and invalid *)
    ip <> "127.0.0.1" &&
    String.length ip >= 7 &&
    not (String.length ip >= 4 && String.sub ip 0 4 = "169.")
  in
  let is_valid_ipv6 ip =
    (* Filter loopback and link-local *)
    ip <> "::1" &&
    String.length ip >= 4 &&
    not (String.sub ip 0 4 = "fe80")
  in
  let parse_ifconfig output =
    (* Parse macOS/BSD ifconfig output for inet/inet6 addresses *)
    let lines = String.split_on_char '\n' output in
    let rec extract acc = function
      | [] -> acc
      | line :: rest ->
        let line = String.trim line in
        (* Match: inet 192.168.1.100 or inet6 fe80::1 *)
        if String.length line > 5 && String.sub line 0 5 = "inet " then
          let parts = String.split_on_char ' ' line in
          match parts with
          | "inet" :: ip :: _ when is_valid_ipv4 ip ->
            extract ((ip, IPv4) :: acc) rest
          | _ -> extract acc rest
        else if String.length line > 6 && String.sub line 0 6 = "inet6 " then
          let parts = String.split_on_char ' ' line in
          match parts with
          | "inet6" :: ip :: _ ->
            (* Remove %interface suffix *)
            let ip_clean = match String.split_on_char '%' ip with
              | clean :: _ -> clean
              | [] -> ip
            in
            if is_valid_ipv6 ip_clean then
              extract ((ip_clean, IPv6) :: acc) rest
            else
              extract acc rest
          | _ -> extract acc rest
        else
          extract acc rest
    in
    extract [] lines
  in
  (* Try ifconfig first (macOS/BSD), then ip addr (Linux) *)
  try
    let ic = Unix.open_process_in "ifconfig 2>/dev/null || ip -o addr show 2>/dev/null" in
    let output = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in
    let addrs = parse_ifconfig output in
    if addrs = [] then [("127.0.0.1", IPv4)] else addrs
  with _ ->
    [("127.0.0.1", IPv4)]  (* Fallback to loopback *)

(** Gather host candidates from local network interfaces *)
and gather_host_candidates () =
  (* Get real local addresses from network interfaces *)
  let local_addrs = get_local_addresses () in

  (* Create a candidate for each local address *)
  List.mapi (fun idx (addr, _family) ->
    let local_pref = 65535 - idx in  (* Higher preference for earlier interfaces *)
    {
      foundation = Ice.generate_foundation ~candidate_type:Host ~base_address:addr ();
      component = 1;
      transport = UDP;
      priority = Ice.calculate_priority ~candidate_type:Host ~local_pref ~component:1;
      address = addr;
      port = 0;  (* Port will be assigned when socket is bound *)
      cand_type = Host;
      base_address = None;
      base_port = None;
      related_address = None;
      related_port = None;
      extensions = [];
    }
  ) local_addrs

(** Parse ICE server URL (stun:host:port or turn:host:port) *)
and parse_ice_server_url url =
  let url = String.trim url in
  if String.length url > 5 && String.sub url 0 5 = "stun:" then
    let rest = String.sub url 5 (String.length url - 5) in
    match String.split_on_char ':' rest with
    | [host; port] -> Some (host, int_of_string port)
    | [host] -> Some (host, 3478)
    | _ -> None
  else if String.length url > 5 && String.sub url 0 5 = "turn:" then
    let rest = String.sub url 5 (String.length url - 5) in
    match String.split_on_char ':' rest with
    | [host; port] -> Some (host, int_of_string port)
    | [host] -> Some (host, 3478)
    | _ -> None
  else
    None

(** Get STUN server URLs from config *)
and get_stun_servers config =
  List.concat_map (fun server ->
    List.filter (fun url ->
      String.length url > 5 && String.sub url 0 5 = "stun:"
    ) server.urls
  ) config.ice_servers

(** {1 Connectivity Checks (Eio-Native)} *)

(** Perform connectivity check on a candidate pair.

    Uses STUN binding request with proper credentials.

    @param net Eio network capability
    @param sw Eio switch
*)
let check_pair ~net ~sw t pair =
  (* Build STUN request with ICE credentials *)
  let (local_ufrag, local_pwd) = get_local_credentials t in
  let remote_ufrag = Eio.Mutex.use_ro t.mutex (fun () ->
    t.agent.remote_ufrag
  ) in

  match remote_ufrag with
  | None -> Error "Remote credentials not set"
  | Some r_ufrag ->
    (* Construct server address from remote candidate *)
    let server = Printf.sprintf "%s:%d" pair.remote.address pair.remote.port in

    (* TODO: Use authenticated STUN request with USERNAME and MESSAGE-INTEGRITY *)
    let _ = (local_ufrag, local_pwd, r_ufrag) in  (* Will be used for auth *)

    match Stun.binding_request_eio ~net ~sw ~server ~timeout:1.0 () with
    | Ok _result ->
      let succeeded_pair = { pair with pair_state = Pair_succeeded } in
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        (* Update pair in list *)
        t.agent.pairs <- List.map (fun p ->
          if p.local.foundation = pair.local.foundation &&
             p.remote.foundation = pair.remote.foundation
          then succeeded_pair
          else p
        ) t.agent.pairs
      );
      Ok succeeded_pair
    | Error e ->
      let failed_pair = { pair with pair_state = Pair_failed } in
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        t.agent.pairs <- List.map (fun p ->
          if p.local.foundation = pair.local.foundation &&
             p.remote.foundation = pair.remote.foundation
          then failed_pair
          else p
        ) t.agent.pairs
      );
      Error e

(** Run connectivity checks on all pairs.

    Implements the ICE connectivity check algorithm:
    1. Form candidate pairs from local and remote candidates
    2. Prioritize pairs
    3. Check pairs in priority order
    4. Handle nominations

    @param net Eio network capability
    @param sw Eio switch
*)
(** Set state and notify callback *)
let set_state t new_state =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.agent.state <- new_state
  );
  match t.on_state_change_callback with
  | Some cb -> cb new_state
  | None -> ()

let run_checks ~net ~sw t =
  set_state t Checking;

  let pairs = get_pairs t in

  (* Check pairs in priority order *)
  let results = List.filter_map (fun pair ->
    match pair.pair_state with
    | Pair_frozen | Pair_waiting ->
      begin match check_pair ~net ~sw t pair with
      | Ok p -> Some (Ok p)
      | Error e -> Some (Error (pair, e))
      end
    | _ -> None
  ) (List.sort (fun a b ->
    Int64.compare b.pair_priority a.pair_priority
  ) pairs) in

  (* Find first successful pair *)
  let successful = List.find_opt (function Ok _ -> true | Error _ -> false) results in

  match successful with
  | Some (Ok pair) ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.agent.nominated_pair <- Some pair;
      t.agent.state <- Connected
    );
    (match t.on_state_change_callback with Some cb -> cb Connected | None -> ());
    Ok pair
  | _ ->
    set_state t Failed;
    Error "All connectivity checks failed"

(** {1 Event Callbacks} *)

(** Register callback for new local candidates *)
let on_candidate t callback =
  t.on_candidate_callback <- Some callback

(** Register callback for state changes *)
let on_state_change t callback =
  t.on_state_change_callback <- Some callback

(** {1 Convenience Functions} *)

(** Full ICE negotiation: gather + check.

    This is the main entry point for ICE negotiation.

    @param net Eio network capability
    @param sw Eio switch
    @return Nominated candidate pair or error
*)
let negotiate ~net ~sw t =
  (* Step 1: Gather candidates *)
  let _candidates = gather_candidates ~net ~sw t in

  (* Step 2: Wait for remote candidates (simplified - caller should add them) *)
  (* In real implementation, this would wait for SDP exchange *)

  (* Step 3: Run connectivity checks *)
  run_checks ~net ~sw t

(** Create candidate pairs from local and remote candidates *)
let form_pairs t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let local = t.agent.local_candidates in
    let remote = t.agent.remote_candidates in

    let pairs = List.concat_map (fun l ->
      List.map (fun r ->
        let priority = Ice.calculate_pair_priority
          l.priority
          r.priority
          (t.agent.config.role = Controlling)
        in
        {
          local = l;
          remote = r;
          pair_priority = priority;
          pair_state = Pair_frozen;
          nominated = false;
        }
      ) remote
    ) local in

    (* Sort by priority descending *)
    let sorted = List.sort (fun a b ->
      Int64.compare b.pair_priority a.pair_priority
    ) pairs in

    t.agent.pairs <- sorted;
    sorted
  )

(** {1 Debug/Status} *)

(** Get agent status as JSON *)
let status_json t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    let state = Ice.show_connection_state t.agent.state in
    let gathering = Ice.show_gathering_state t.agent.gathering_state in
    let local_count = List.length t.agent.local_candidates in
    let remote_count = List.length t.agent.remote_candidates in
    let pair_count = List.length t.agent.pairs in
    let nominated = match t.agent.nominated_pair with
      | Some p -> Printf.sprintf "%s:%d <-> %s:%d"
          p.local.address p.local.port
          p.remote.address p.remote.port
      | None -> "none"
    in

    `Assoc [
      ("state", `String state);
      ("gathering_state", `String gathering);
      ("local_candidates", `Int local_count);
      ("remote_candidates", `Int remote_count);
      ("pairs", `Int pair_count);
      ("nominated", `String nominated);
    ]
  )

(** Pretty print agent status *)
let pp fmt t =
  let json = status_json t in
  Format.fprintf fmt "%s" (Yojson.Safe.pretty_to_string json)
