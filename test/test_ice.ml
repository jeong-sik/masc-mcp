(** Tests for RFC 8445 ICE module *)

open Alcotest
open Masc_mcp.Ice

(** {1 Test Utilities} *)

let candidate_type_testable = testable (fun fmt ct -> Format.fprintf fmt "%s" (string_of_candidate_type ct)) (=)

(** {1 Candidate Tests} *)

let test_parse_host_candidate () =
  let line = "1 1 UDP 2130706431 192.168.1.100 54321 typ host" in
  match parse_candidate line with
  | Error e -> fail e
  | Ok c ->
    check string "foundation" "1" c.foundation;
    check int "component" 1 c.component;
    check bool "transport is UDP" true (c.transport = UDP);
    check int "priority" 2130706431 c.priority;
    check string "address" "192.168.1.100" c.address;
    check int "port" 54321 c.port;
    check candidate_type_testable "type" Host c.cand_type

let test_parse_srflx_candidate () =
  let line = "2 1 UDP 1694498815 203.0.113.50 12345 typ srflx raddr 192.168.1.100 rport 54321" in
  match parse_candidate line with
  | Error e -> fail e
  | Ok c ->
    check candidate_type_testable "type" Server_reflexive c.cand_type;
    check (option string) "raddr" (Some "192.168.1.100") c.related_address;
    check (option int) "rport" (Some 54321) c.related_port

let test_parse_relay_candidate () =
  let line = "3 1 UDP 16777215 198.51.100.10 9999 typ relay raddr 203.0.113.50 rport 12345" in
  match parse_candidate line with
  | Error e -> fail e
  | Ok c ->
    check candidate_type_testable "type" Relay c.cand_type

let test_candidate_to_string () =
  let c = {
    foundation = "test";
    component = 1;
    transport = UDP;
    priority = 100;
    address = "10.0.0.1";
    port = 5000;
    cand_type = Host;
    base_address = None;
    base_port = None;
    related_address = None;
    related_port = None;
    extensions = [];
  } in
  let encoded = candidate_to_string c in
  check bool "starts with foundation" true (String.sub encoded 0 4 = "test");
  check bool "contains UDP" true (String.length encoded > 10 && String.contains encoded 'U')

let test_candidate_roundtrip () =
  let original = {
    foundation = "abc123";
    component = 1;
    transport = UDP;
    priority = 2130706431;
    address = "192.168.1.50";
    port = 12345;
    cand_type = Host;
    base_address = None;
    base_port = None;
    related_address = None;
    related_port = None;
    extensions = [];
  } in
  let encoded = candidate_to_string original in
  match parse_candidate encoded with
  | Error e -> fail e
  | Ok parsed ->
    check string "foundation matches" original.foundation parsed.foundation;
    check int "port matches" original.port parsed.port;
    check string "address matches" original.address parsed.address

(** {1 Priority Tests} *)

let test_calculate_priority () =
  let p = calculate_priority ~candidate_type:Host ~local_pref:65535 ~component:1 in
  (* RFC 8445: priority = (2^24)*(type preference) + (2^8)*(local preference) + (256 - component ID) *)
  (* Host type preference is 126, so: 126*2^24 + 65535*2^8 + 255 = 2130706431 *)
  check int "priority formula" 2130706431 p

let test_priority_ordering () =
  let host_prio = calculate_priority ~candidate_type:Host ~local_pref:65535 ~component:1 in
  let srflx_prio = calculate_priority ~candidate_type:Server_reflexive ~local_pref:65535 ~component:1 in
  let relay_prio = calculate_priority ~candidate_type:Relay ~local_pref:65535 ~component:1 in
  check bool "host > srflx" true (host_prio > srflx_prio);
  check bool "srflx > relay" true (srflx_prio > relay_prio)

(** {1 Agent Tests} *)

let test_create_agent () =
  let agent = create default_config in
  check bool "not gathering" true (get_gathering_state agent = Gathering_new);
  check bool "no local candidates" true (List.length (get_local_candidates agent) = 0)

let test_agent_config () =
  let config = {
    default_config with
    role = Controlled;
    ice_lite = true;
  } in
  let agent = create config in
  check bool "new state" true (get_state agent = New);
  check bool "ice lite config" true config.ice_lite

(** {1 Pair Tests} *)

let test_pair_priority () =
  (* Pair priority formula: 2^32 * MIN(G,D) + 2 * MAX(G,D) + (G > D ? 1 : 0) *)
  let prio = calculate_pair_priority 100 50 true in
  check bool "priority > 0" true (Int64.compare prio 0L > 0)

(** {1 State Tests} *)

let test_initial_state () =
  let agent = create default_config in
  check bool "new state" true (get_state agent = New)

let test_string_of_connection_state () =
  check string "new" "new" (string_of_connection_state New);
  check string "completed" "completed" (string_of_connection_state Completed);
  check string "failed" "failed" (string_of_connection_state Failed)

let test_string_of_candidate_type () =
  check string "host" "host" (string_of_candidate_type Host);
  check string "srflx" "srflx" (string_of_candidate_type Server_reflexive);
  check string "prflx" "prflx" (string_of_candidate_type Peer_reflexive);
  check string "relay" "relay" (string_of_candidate_type Relay)

(** {1 Config Tests} *)

let test_default_config () =
  check bool "controlling by default" true (default_config.role = Controlling);
  check bool "not lite" false default_config.ice_lite;
  check int "default check interval" 50 default_config.check_interval_ms

(** {1 Status JSON Tests} *)

let test_status_json () =
  let agent = create default_config in
  let status = status_json agent in
  let open Yojson.Safe.Util in
  check string "state" "new" (status |> member "state" |> to_string);
  check bool "isGathering" false (status |> member "isGathering" |> to_bool)

(** {1 Test Suites} *)

let candidate_tests = [
  "parse_host_candidate", `Quick, test_parse_host_candidate;
  "parse_srflx_candidate", `Quick, test_parse_srflx_candidate;
  "parse_relay_candidate", `Quick, test_parse_relay_candidate;
  "candidate_to_string", `Quick, test_candidate_to_string;
  "candidate_roundtrip", `Quick, test_candidate_roundtrip;
]

let priority_tests = [
  "calculate_priority", `Quick, test_calculate_priority;
  "priority_ordering", `Quick, test_priority_ordering;
]

let agent_tests = [
  "create_agent", `Quick, test_create_agent;
  "agent_config", `Quick, test_agent_config;
]

let pair_tests = [
  "pair_priority", `Quick, test_pair_priority;
]

let state_tests = [
  "initial_state", `Quick, test_initial_state;
  "string_of_connection_state", `Quick, test_string_of_connection_state;
  "string_of_candidate_type", `Quick, test_string_of_candidate_type;
]

let config_tests = [
  "default_config", `Quick, test_default_config;
]

let status_tests = [
  "status_json", `Quick, test_status_json;
]

let () =
  Alcotest.run "ICE (RFC 8445)" [
    ("candidate", candidate_tests);
    ("priority", priority_tests);
    ("agent", agent_tests);
    ("pair", pair_tests);
    ("state", state_tests);
    ("config", config_tests);
    ("status", status_tests);
  ]
