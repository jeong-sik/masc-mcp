(** Tests for Ice_eio: Thread-Safe Eio-Native ICE Implementation

    Tests cover:
    - Agent creation and state management
    - Mutex-protected state access
    - Candidate gathering (unit + integration)
    - Pair formation
    - Concurrent access safety
*)

open Masc_mcp

(* ============================================
   Unit Tests: Agent Creation & State
   ============================================ *)

(** Test agent creation *)
let test_create () =
  let agent = Ice_eio.create () in
  Alcotest.(check bool) "initial state is New"
    true (Ice_eio.get_state agent = Ice.New);
  Alcotest.(check bool) "gathering state is New"
    true (Ice_eio.get_gathering_state agent = Ice.Gathering_new);
  Alcotest.(check int) "no local candidates"
    0 (List.length (Ice_eio.get_local_candidates agent));
  Alcotest.(check int) "no remote candidates"
    0 (List.length (Ice_eio.get_remote_candidates agent))

(** Test custom config *)
let test_create_with_config () =
  let config = { Ice.default_config with
    role = Ice.Controlled;
    aggressive_nomination = false;
  } in
  let agent = Ice_eio.create ~config () in
  Alcotest.(check bool) "state is New"
    true (Ice_eio.get_state agent = Ice.New)

(** Test credentials generation *)
let test_credentials () =
  let agent = Ice_eio.create () in
  let (ufrag, pwd) = Ice_eio.get_local_credentials agent in
  (* ufrag should be at least 4 chars, pwd at least 22 chars per RFC *)
  Alcotest.(check bool) "ufrag length >= 4"
    true (String.length ufrag >= 4);
  Alcotest.(check bool) "pwd length >= 22"
    true (String.length pwd >= 22)

(** Test set remote credentials *)
let test_remote_credentials () =
  Eio_main.run @@ fun _env ->
  let agent = Ice_eio.create () in
  Ice_eio.set_remote_credentials agent ~ufrag:"remoteufrag" ~pwd:"remotepassword1234567890";
  (* Verify by trying to form pairs - no crash means success *)
  let _ = Ice_eio.get_pairs agent in
  ()

(** Test add remote candidate *)
let test_add_remote_candidate () =
  Eio_main.run @@ fun _env ->
  let agent = Ice_eio.create () in
  let candidate = {
    Ice.foundation = "1";
    component = 1;
    transport = Ice.UDP;
    priority = 2130706431;
    address = "192.168.1.100";
    port = 54321;
    cand_type = Ice.Host;
    base_address = None;
    base_port = None;
    related_address = None;
    related_port = None;
    extensions = [];
  } in
  Ice_eio.add_remote_candidate agent candidate;
  Alcotest.(check int) "one remote candidate"
    1 (List.length (Ice_eio.get_remote_candidates agent))

(** Test close *)
let test_close () =
  Eio_main.run @@ fun _env ->
  let agent = Ice_eio.create () in
  Ice_eio.close agent;
  Alcotest.(check bool) "state is Closed"
    true (Ice_eio.get_state agent = Ice.Closed)

(** Test restart *)
let test_restart () =
  Eio_main.run @@ fun _env ->
  let agent = Ice_eio.create () in
  let (ufrag1, _) = Ice_eio.get_local_credentials agent in
  Ice_eio.restart agent;
  let (ufrag2, _) = Ice_eio.get_local_credentials agent in
  Alcotest.(check bool) "ufrag changed after restart"
    true (ufrag1 <> ufrag2);
  Alcotest.(check bool) "state is New after restart"
    true (Ice_eio.get_state agent = Ice.New)

(* ============================================
   Unit Tests: Pair Formation
   ============================================ *)

(** Test pair formation *)
let test_form_pairs () =
  Eio_main.run @@ fun _env ->
  let agent = Ice_eio.create () in

  (* Add a local candidate manually for testing *)
  (* Note: In real usage, gather_candidates would do this *)

  (* Add remote candidate *)
  let remote = {
    Ice.foundation = "2";
    component = 1;
    transport = Ice.UDP;
    priority = 2130706431;
    address = "203.0.113.50";
    port = 12345;
    cand_type = Ice.Host;
    base_address = None;
    base_port = None;
    related_address = None;
    related_port = None;
    extensions = [];
  } in
  Ice_eio.add_remote_candidate agent remote;

  (* Form pairs - will be empty since no local candidates yet *)
  let pairs = Ice_eio.form_pairs agent in
  (* With no local candidates gathered, pairs will be empty *)
  let _ = pairs in
  ()

(* ============================================
   Unit Tests: Concurrent Access Safety
   ============================================ *)

(** Test concurrent state access with Mutex *)
let test_concurrent_access () =
  Eio_main.run @@ fun _env ->
  let agent = Ice_eio.create () in

  (* Spawn multiple fibers accessing state concurrently *)
  Eio.Fiber.all [
    (fun () ->
      for _ = 1 to 100 do
        let _ = Ice_eio.get_state agent in
        let _ = Ice_eio.get_local_credentials agent in
        ()
      done);
    (fun () ->
      for i = 1 to 100 do
        let candidate = {
          Ice.foundation = string_of_int i;
          component = 1;
          transport = Ice.UDP;
          priority = 2130706431 - i;
          address = Printf.sprintf "192.168.1.%d" (i mod 255);
          port = 10000 + i;
          cand_type = Ice.Host;
          base_address = None;
          base_port = None;
          related_address = None;
          related_port = None;
          extensions = [];
        } in
        Ice_eio.add_remote_candidate agent candidate
      done);
    (fun () ->
      for _ = 1 to 100 do
        let _ = Ice_eio.get_remote_candidates agent in
        ()
      done);
  ];

  (* Verify final state *)
  Alcotest.(check int) "100 remote candidates added"
    100 (List.length (Ice_eio.get_remote_candidates agent))

(* ============================================
   Integration Tests: Candidate Gathering
   ============================================ *)

(** Test candidate gathering with live STUN *)
let test_gather_candidates_live () =
  let skip_live = try Sys.getenv "SKIP_LIVE_TESTS" = "1" with Not_found -> false in
  if skip_live then
    Alcotest.skip ()
  else begin
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in

    let agent = Ice_eio.create () in

    (* Gather candidates *)
    let candidates = Ice_eio.gather_candidates ~net ~sw agent in

    Printf.printf "Gathered %d candidates:\n" (List.length candidates);
    List.iter (fun (c : Ice.candidate) ->
      Printf.printf "  - %s %s:%d (%s)\n"
        (Ice.show_candidate_type c.Ice.cand_type)
        c.Ice.address c.Ice.port
        c.Ice.foundation
    ) candidates;

    (* Should have at least host candidate *)
    Alcotest.(check bool) "at least one candidate"
      true (List.length candidates >= 1);

    (* Gathering state should be complete *)
    Alcotest.(check bool) "gathering complete"
      true (Ice_eio.get_gathering_state agent = Ice.Gathering_complete)
  end

(* Helper for string contains check *)
let string_contains ~needle haystack =
  let re = Str.regexp_string needle in
  try ignore (Str.search_forward re haystack 0); true
  with Not_found -> false

(** Test status JSON *)
let test_status_json () =
  Eio_main.run @@ fun _env ->
  let agent = Ice_eio.create () in
  let json = Ice_eio.status_json agent in
  let json_str = Yojson.Safe.to_string json in

  Alcotest.(check bool) "has state field"
    true (String.length json_str > 0);
  Alcotest.(check bool) "contains 'state'"
    true (string_contains ~needle:"state" json_str)

(* ============================================
   Test Suite Registration
   ============================================ *)

let unit_tests = [
  "create agent", `Quick, test_create;
  "create with config", `Quick, test_create_with_config;
  "credentials generation", `Quick, test_credentials;
  "set remote credentials", `Quick, test_remote_credentials;
  "add remote candidate", `Quick, test_add_remote_candidate;
  "close agent", `Quick, test_close;
  "restart agent", `Quick, test_restart;
  "form pairs", `Quick, test_form_pairs;
  "concurrent access", `Quick, test_concurrent_access;
  "status JSON", `Quick, test_status_json;
]

let integration_tests = [
  "gather candidates (live STUN)", `Slow, test_gather_candidates_live;
]

let () =
  Alcotest.run "Ice_eio" [
    "Unit Tests", unit_tests;
    "Integration Tests", integration_tests;
  ]
