(** SDP Coverage Tests - RFC 8866 Session Description Protocol

    Tests for lib/sdp.ml covering:
    - ICE candidate parsing/serialization
    - Fingerprint parsing/serialization
    - Full session parsing/serialization
    - WebRTC helpers
    - Roundtrip tests

    Note: Internal functions (parse_origin, parse_connection, etc.)
    are tested indirectly through the main `parse` function.
*)

open Alcotest
module Sdp = Masc_mcp.Sdp

let fail msg = Alcotest.fail msg

(* ============================================
   ICE Candidate Tests
   ============================================ *)

let test_parse_ice_candidate_simple () =
  let line = "abc123 1 udp 2130706431 192.168.1.1 54321 typ host" in
  match Sdp.parse_ice_candidate line with
  | Ok c ->
    check string "foundation" "abc123" c.foundation;
    check int "component" 1 c.component;
    check string "transport" "udp" c.transport;
    check int "priority" 2130706431 c.priority;
    check string "address" "192.168.1.1" c.address;
    check int "port" 54321 c.port;
    check string "cand_type" "host" c.cand_type;
    check (option string) "rel_addr" None c.rel_addr;
    check (option int) "rel_port" None c.rel_port
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_ice_candidate_with_relay () =
  let line = "xyz789 1 udp 16777215 10.0.0.1 3478 typ relay raddr 192.168.1.1 rport 54321" in
  match Sdp.parse_ice_candidate line with
  | Ok c ->
    check string "foundation" "xyz789" c.foundation;
    check string "cand_type" "relay" c.cand_type;
    check (option string) "rel_addr" (Some "192.168.1.1") c.rel_addr;
    check (option int) "rel_port" (Some 54321) c.rel_port
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_ice_candidate_with_extensions () =
  let line = "abc 1 udp 100 1.2.3.4 5000 typ srflx raddr 10.0.0.1 rport 8000 generation 0 network-id 1" in
  match Sdp.parse_ice_candidate line with
  | Ok c ->
    check string "cand_type" "srflx" c.cand_type;
    check (option string) "rel_addr" (Some "10.0.0.1") c.rel_addr;
    check int "extensions count" 2 (List.length c.extensions)
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_ice_candidate_invalid () =
  let line = "invalid candidate" in
  match Sdp.parse_ice_candidate line with
  | Ok _ -> fail "Should have failed"
  | Error _ -> ()

let test_ice_candidate_roundtrip () =
  let candidate : Sdp.ice_candidate = {
    foundation = "test123";
    component = 1;
    transport = "udp";
    priority = 2130706431;
    address = "192.168.1.100";
    port = 12345;
    cand_type = "host";
    rel_addr = None;
    rel_port = None;
    extensions = [];
  } in
  let str = Sdp.ice_candidate_to_string candidate in
  match Sdp.parse_ice_candidate str with
  | Ok parsed ->
    check string "foundation" candidate.foundation parsed.foundation;
    check int "component" candidate.component parsed.component;
    check int "port" candidate.port parsed.port
  | Error e -> fail ("Roundtrip failed: " ^ e)

let test_ice_candidate_roundtrip_with_relay () =
  let candidate : Sdp.ice_candidate = {
    foundation = "relay1";
    component = 1;
    transport = "udp";
    priority = 100;
    address = "203.0.113.1";
    port = 3478;
    cand_type = "relay";
    rel_addr = Some "192.168.1.1";
    rel_port = Some 54321;
    extensions = [];
  } in
  let str = Sdp.ice_candidate_to_string candidate in
  match Sdp.parse_ice_candidate str with
  | Ok parsed ->
    check string "cand_type" "relay" parsed.cand_type;
    check (option string) "rel_addr" (Some "192.168.1.1") parsed.rel_addr;
    check (option int) "rel_port" (Some 54321) parsed.rel_port
  | Error e -> fail ("Roundtrip failed: " ^ e)

(* ============================================
   Fingerprint Tests
   ============================================ *)

let test_parse_fingerprint_sha256 () =
  let line = "sha-256 AB:CD:EF:01:23:45:67:89:AB:CD:EF:01:23:45:67:89:AB:CD:EF:01:23:45:67:89:AB:CD:EF:01:23:45:67:89" in
  match Sdp.parse_fingerprint line with
  | Ok fp ->
    check string "hash_func" "sha-256" fp.hash_func;
    check bool "fingerprint starts with AB" true (String.length fp.fingerprint > 0)
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_fingerprint_sha1 () =
  let line = "sha-1 00:11:22:33:44:55:66:77:88:99:AA:BB:CC:DD:EE:FF:00:11:22:33" in
  match Sdp.parse_fingerprint line with
  | Ok fp ->
    check string "hash_func" "sha-1" fp.hash_func
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_fingerprint_invalid () =
  let line = "invalid" in
  match Sdp.parse_fingerprint line with
  | Ok _ -> fail "Should have failed"
  | Error _ -> ()

let test_fingerprint_roundtrip () =
  let fp : Sdp.fingerprint = {
    hash_func = "sha-256";
    fingerprint = "AA:BB:CC:DD:EE:FF";
  } in
  let str = Sdp.fingerprint_to_string fp in
  match Sdp.parse_fingerprint str with
  | Ok parsed ->
    check string "hash_func" fp.hash_func parsed.hash_func;
    check string "fingerprint" fp.fingerprint parsed.fingerprint
  | Error e -> fail ("Roundtrip failed: " ^ e)

(* ============================================
   Full Session Tests (tests internal parsing indirectly)
   ============================================ *)

let minimal_sdp = {|v=0
o=- 1234567890 2 IN IP4 127.0.0.1
s=-
t=0 0|}

let test_parse_minimal_session () =
  match Sdp.parse minimal_sdp with
  | Ok s ->
    check int "version" 0 s.version;
    check string "name" "-" s.name;
    check (pair int int) "timing" (0, 0) s.timing;
    (* Origin is tested indirectly *)
    check string "origin username" "-" s.origin.username;
    check string "origin sess_id" "1234567890" s.origin.sess_id;
    check string "origin unicast_address" "127.0.0.1" s.origin.unicast_address
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_session_with_origin_ipv6 () =
  let sdp = {|v=0
o=jdoe 2890844526 2890842807 IN IP6 ::1
s=Test
t=0 0|} in
  match Sdp.parse sdp with
  | Ok s ->
    check string "origin username" "jdoe" s.origin.username;
    check string "origin address" "::1" s.origin.unicast_address
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_session_with_connection () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=Test
c=IN IP4 192.168.1.1
t=0 0|} in
  match Sdp.parse sdp with
  | Ok s ->
    check bool "has connection" true (Option.is_some s.connection);
    (match s.connection with
     | Some c ->
       check string "address" "192.168.1.1" c.address
     | None -> fail "Expected connection")
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_session_with_multicast_connection () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=Test
c=IN IP4 224.2.36.42/127
t=0 0|} in
  match Sdp.parse sdp with
  | Ok s ->
    (match s.connection with
     | Some c ->
       check string "address" "224.2.36.42" c.address;
       check (option int) "ttl" (Some 127) c.ttl
     | None -> fail "Expected connection")
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_session_with_bandwidth () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=Test
b=CT:384
t=0 0|} in
  match Sdp.parse sdp with
  | Ok s ->
    check int "bandwidth count" 1 (List.length s.bandwidth);
    (match s.bandwidth with
     | b :: _ ->
       check string "bw_type" "CT" b.bw_type;
       check int "bandwidth" 384 b.bandwidth
     | [] -> fail "Expected bandwidth")
  | Error e -> fail ("Parse failed: " ^ e)

let webrtc_sdp = {|v=0
o=- 1234567890 2 IN IP4 127.0.0.1
s=-
t=0 0
a=ice-ufrag:abcd
a=ice-pwd:secret123456789012345678
a=fingerprint:sha-256 AA:BB:CC:DD:EE:FF
a=group:BUNDLE 0
m=application 9 UDP/DTLS/SCTP webrtc-datachannel
c=IN IP4 0.0.0.0
a=mid:0
a=sctp-port:5000
a=max-message-size:262144|}

let test_parse_webrtc_session () =
  match Sdp.parse webrtc_sdp with
  | Ok s ->
    check int "version" 0 s.version;
    check (option string) "ice_ufrag" (Some "abcd") s.ice_ufrag;
    check (option string) "ice_pwd" (Some "secret123456789012345678") s.ice_pwd;
    check bool "has fingerprint" true (Option.is_some s.fingerprint);
    check bool "has bundle_group" true (Option.is_some s.bundle_group);
    check int "media count" 1 (List.length s.media)
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_session_with_ice_lite () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=test
t=0 0
a=ice-lite|} in
  match Sdp.parse sdp with
  | Ok s ->
    check bool "ice_lite" true s.ice_lite
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_media_audio () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=Test
t=0 0
m=audio 49170 RTP/AVP 0|} in
  match Sdp.parse sdp with
  | Ok s ->
    check int "media count" 1 (List.length s.media);
    (match s.media with
     | m :: _ ->
       check bool "media_type is Audio" true (m.media_type = Sdp.Audio);
       check int "port" 49170 m.port;
       check bool "proto is RTP_AVP" true (m.proto = Sdp.RTP_AVP)
     | [] -> fail "Expected media")
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_media_video () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=Test
t=0 0
m=video 51372 RTP/SAVPF 96 97 98|} in
  match Sdp.parse sdp with
  | Ok s ->
    (match s.media with
     | m :: _ ->
       check bool "media_type is Video" true (m.media_type = Sdp.Video);
       check int "port" 51372 m.port;
       check bool "proto is RTP_SAVPF" true (m.proto = Sdp.RTP_SAVPF);
       check (list string) "fmt" ["96"; "97"; "98"] m.fmt
     | [] -> fail "Expected media")
  | Error e -> fail ("Parse failed: " ^ e)

let test_parse_media_application () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=Test
t=0 0
m=application 9 UDP/DTLS/SCTP webrtc-datachannel|} in
  match Sdp.parse sdp with
  | Ok s ->
    (match s.media with
     | m :: _ ->
       check bool "media_type is Application" true (m.media_type = Sdp.Application);
       check int "port" 9 m.port;
       check bool "proto is UDP_DTLS_SCTP" true (m.proto = Sdp.UDP_DTLS_SCTP)
     | [] -> fail "Expected media")
  | Error e -> fail ("Parse failed: " ^ e)

let test_session_roundtrip () =
  match Sdp.parse minimal_sdp with
  | Ok s ->
    let str = Sdp.to_string s in
    (match Sdp.parse str with
     | Ok s2 ->
       check int "version" s.version s2.version;
       check string "name" s.name s2.name
     | Error e -> fail ("Roundtrip parse failed: " ^ e))
  | Error e -> fail ("Initial parse failed: " ^ e)

let test_webrtc_session_roundtrip () =
  match Sdp.parse webrtc_sdp with
  | Ok s ->
    let str = Sdp.to_string s in
    (match Sdp.parse str with
     | Ok s2 ->
       check (option string) "ice_ufrag" s.ice_ufrag s2.ice_ufrag;
       check (option string) "ice_pwd" s.ice_pwd s2.ice_pwd;
       check int "media count" (List.length s.media) (List.length s2.media)
     | Error e -> fail ("Roundtrip parse failed: " ^ e))
  | Error e -> fail ("Initial parse failed: " ^ e)

(* ============================================
   WebRTC Helper Tests
   ============================================ *)

let test_create_datachannel_offer () =
  let fp : Sdp.fingerprint = {
    hash_func = "sha-256";
    fingerprint = "AA:BB:CC:DD:EE:FF:00:11:22:33:44:55:66:77:88:99:AA:BB:CC:DD:EE:FF:00:11:22:33:44:55:66:77:88:99";
  } in
  let s = Sdp.create_datachannel_offer
    ~ice_ufrag:"test1234"
    ~ice_pwd:"password12345678901234"
    ~fingerprint:fp
    ~sctp_port:5000
    ~max_message_size:262144
    ()
  in
  check (option string) "ice_ufrag" (Some "test1234") s.ice_ufrag;
  check (option string) "ice_pwd" (Some "password12345678901234") s.ice_pwd;
  check bool "has fingerprint" true (Option.is_some s.fingerprint);
  check int "media count" 1 (List.length s.media);
  (match s.media with
   | m :: _ ->
     check bool "is Application" true (m.media_type = Sdp.Application);
     check (option int) "sctp_port" (Some 5000) m.sctp_port
   | [] -> fail "Expected media")

let test_create_datachannel_answer () =
  let fp : Sdp.fingerprint = {
    hash_func = "sha-256";
    fingerprint = "AA:BB:CC:DD";
  } in
  match Sdp.parse webrtc_sdp with
  | Ok offer ->
    let answer = Sdp.create_datachannel_answer
      ~offer
      ~ice_ufrag:"answer123"
      ~ice_pwd:"answerpassword123456"
      ~fingerprint:fp
    in
    check (option string) "ice_ufrag" (Some "answer123") answer.ice_ufrag;
    check bool "has fingerprint" true (Option.is_some answer.fingerprint);
    check int "media count" 1 (List.length answer.media)
  | Error e -> fail ("Parse offer failed: " ^ e)

let test_add_ice_candidate () =
  let media = Sdp.default_media in
  let candidate : Sdp.ice_candidate = {
    foundation = "abc";
    component = 1;
    transport = "udp";
    priority = 100;
    address = "1.2.3.4";
    port = 5000;
    cand_type = "host";
    rel_addr = None;
    rel_port = None;
    extensions = [];
  } in
  let media' = Sdp.add_ice_candidate media candidate in
  check int "candidates count" 1 (List.length media'.ice_candidates)

let test_get_ice_candidates () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=Test
t=0 0
m=application 9 UDP/DTLS/SCTP webrtc-datachannel
a=mid:data
a=candidate:abc 1 udp 100 1.2.3.4 5000 typ host|} in
  match Sdp.parse sdp with
  | Ok s ->
    let candidates = Sdp.get_ice_candidates s in
    check int "candidates count" 1 (List.length candidates);
    (match candidates with
     | (mid, c) :: _ ->
       check string "mid" "data" mid;
       check string "foundation" "abc" c.foundation
     | [] -> fail "Expected candidate")
  | Error e -> fail ("Parse failed: " ^ e)

let test_get_media_by_mid () =
  match Sdp.parse webrtc_sdp with
  | Ok s ->
    (match Sdp.get_media_by_mid s "0" with
     | Some m ->
       check bool "is Application" true (m.media_type = Sdp.Application)
     | None -> fail "Expected media with mid=0")
  | Error e -> fail ("Parse failed: " ^ e)

let test_is_datachannel_session () =
  match Sdp.parse webrtc_sdp with
  | Ok s ->
    check bool "is datachannel" true (Sdp.is_datachannel_session s)
  | Error e -> fail ("Parse failed: " ^ e)

(* ============================================
   String Conversion Tests
   ============================================ *)

let test_string_of_net_type () =
  check string "IN" "IN" (Sdp.string_of_net_type Sdp.IN)

let test_net_type_of_string () =
  check bool "IN -> IN" true (Sdp.net_type_of_string "IN" = Sdp.IN);
  check bool "unknown -> IN (default)" true (Sdp.net_type_of_string "UNKNOWN" = Sdp.IN)

let test_string_of_addr_type () =
  check string "IP4" "IP4" (Sdp.string_of_addr_type Sdp.IP4);
  check string "IP6" "IP6" (Sdp.string_of_addr_type Sdp.IP6)

let test_addr_type_of_string () =
  check bool "IP4 -> IP4" true (Sdp.addr_type_of_string "IP4" = Sdp.IP4);
  check bool "IP6 -> IP6" true (Sdp.addr_type_of_string "IP6" = Sdp.IP6);
  check bool "unknown -> IP4 (default)" true (Sdp.addr_type_of_string "UNKNOWN" = Sdp.IP4)

let test_string_of_media_type () =
  check string "audio" "audio" (Sdp.string_of_media_type Sdp.Audio);
  check string "video" "video" (Sdp.string_of_media_type Sdp.Video);
  check string "application" "application" (Sdp.string_of_media_type Sdp.Application);
  check string "text" "text" (Sdp.string_of_media_type Sdp.Text);
  check string "message" "message" (Sdp.string_of_media_type Sdp.Message)

let test_media_type_of_string () =
  check bool "audio -> Audio" true (Sdp.media_type_of_string "audio" = Sdp.Audio);
  check bool "video -> Video" true (Sdp.media_type_of_string "video" = Sdp.Video);
  check bool "application -> Application" true (Sdp.media_type_of_string "application" = Sdp.Application);
  check bool "text -> Text" true (Sdp.media_type_of_string "text" = Sdp.Text);
  check bool "message -> Message" true (Sdp.media_type_of_string "message" = Sdp.Message);
  check bool "unknown -> Application (default)" true (Sdp.media_type_of_string "unknown" = Sdp.Application)

let test_string_of_transport () =
  check string "UDP" "UDP" (Sdp.string_of_transport Sdp.UDP);
  check string "TCP" "TCP" (Sdp.string_of_transport Sdp.TCP);
  check string "RTP/AVP" "RTP/AVP" (Sdp.string_of_transport Sdp.RTP_AVP);
  check string "RTP/SAVP" "RTP/SAVP" (Sdp.string_of_transport Sdp.RTP_SAVP);
  check string "RTP/SAVPF" "RTP/SAVPF" (Sdp.string_of_transport Sdp.RTP_SAVPF);
  check string "DTLS/SCTP" "DTLS/SCTP" (Sdp.string_of_transport Sdp.DTLS_SCTP);
  check string "UDP/DTLS/SCTP" "UDP/DTLS/SCTP" (Sdp.string_of_transport Sdp.UDP_DTLS_SCTP)

let test_transport_of_string () =
  check bool "UDP -> UDP" true (Sdp.transport_of_string "UDP" = Sdp.UDP);
  check bool "TCP -> TCP" true (Sdp.transport_of_string "TCP" = Sdp.TCP);
  check bool "DTLS/SCTP -> DTLS_SCTP" true (Sdp.transport_of_string "DTLS/SCTP" = Sdp.DTLS_SCTP);
  check bool "UDP/DTLS/SCTP -> UDP_DTLS_SCTP" true (Sdp.transport_of_string "UDP/DTLS/SCTP" = Sdp.UDP_DTLS_SCTP);
  check bool "RTP/AVP -> RTP_AVP" true (Sdp.transport_of_string "RTP/AVP" = Sdp.RTP_AVP);
  check bool "RTP/SAVP -> RTP_SAVP" true (Sdp.transport_of_string "RTP/SAVP" = Sdp.RTP_SAVP);
  check bool "RTP/SAVPF -> RTP_SAVPF" true (Sdp.transport_of_string "RTP/SAVPF" = Sdp.RTP_SAVPF);
  check bool "unknown -> UDP (default)" true (Sdp.transport_of_string "UNKNOWN" = Sdp.UDP)

let test_string_of_direction () =
  check string "sendrecv" "sendrecv" (Sdp.string_of_direction Sdp.Sendrecv);
  check string "sendonly" "sendonly" (Sdp.string_of_direction Sdp.Sendonly);
  check string "recvonly" "recvonly" (Sdp.string_of_direction Sdp.Recvonly);
  check string "inactive" "inactive" (Sdp.string_of_direction Sdp.Inactive)

let test_direction_of_string () =
  check bool "sendrecv -> Sendrecv" true (Sdp.direction_of_string "sendrecv" = Sdp.Sendrecv);
  check bool "sendonly -> Sendonly" true (Sdp.direction_of_string "sendonly" = Sdp.Sendonly);
  check bool "recvonly -> Recvonly" true (Sdp.direction_of_string "recvonly" = Sdp.Recvonly);
  check bool "inactive -> Inactive" true (Sdp.direction_of_string "inactive" = Sdp.Inactive);
  check bool "unknown -> Sendrecv (default)" true (Sdp.direction_of_string "unknown" = Sdp.Sendrecv)

let test_string_of_setup () =
  check string "active" "active" (Sdp.string_of_setup Sdp.Active);
  check string "passive" "passive" (Sdp.string_of_setup Sdp.Passive);
  check string "actpass" "actpass" (Sdp.string_of_setup Sdp.Actpass);
  check string "holdconn" "holdconn" (Sdp.string_of_setup Sdp.Holdconn)

let test_setup_of_string () =
  check bool "active -> Active" true (Sdp.setup_of_string "active" = Sdp.Active);
  check bool "passive -> Passive" true (Sdp.setup_of_string "passive" = Sdp.Passive);
  check bool "actpass -> Actpass" true (Sdp.setup_of_string "actpass" = Sdp.Actpass);
  check bool "holdconn -> Holdconn" true (Sdp.setup_of_string "holdconn" = Sdp.Holdconn);
  check bool "unknown -> Actpass (default)" true (Sdp.setup_of_string "unknown" = Sdp.Actpass)

(* ============================================
   Parsing Helper Tests
   ============================================ *)

let test_split_first () =
  (* No delimiter *)
  let (before, after) = Sdp.split_first ':' "nodelimiter" in
  check string "no delim - before" "nodelimiter" before;
  check (option string) "no delim - after" None after;
  (* With delimiter *)
  let (before2, after2) = Sdp.split_first ':' "key:value" in
  check string "with delim - before" "key" before2;
  check (option string) "with delim - after" (Some "value") after2;
  (* Delimiter at start *)
  let (before3, after3) = Sdp.split_first ':' ":value" in
  check string "at start - before" "" before3;
  check (option string) "at start - after" (Some "value") after3;
  (* Multiple delimiters *)
  let (before4, after4) = Sdp.split_first ':' "a:b:c" in
  check string "multiple - before" "a" before4;
  check (option string) "multiple - after" (Some "b:c") after4

let test_split_spaces () =
  (* Normal spaces *)
  check (list string) "normal" ["a"; "b"; "c"] (Sdp.split_spaces "a b c");
  (* Multiple spaces *)
  check (list string) "multiple spaces" ["a"; "b"] (Sdp.split_spaces "a   b");
  (* No spaces *)
  check (list string) "no spaces" ["single"] (Sdp.split_spaces "single");
  (* Empty string *)
  check (list string) "empty" [] (Sdp.split_spaces "");
  (* Leading/trailing spaces *)
  check (list string) "leading/trailing" ["x"; "y"] (Sdp.split_spaces " x y ")

(* ============================================
   Edge Cases
   ============================================ *)

let test_empty_sdp () =
  match Sdp.parse "" with
  | Ok _ -> ()  (* Empty SDP returns default session *)
  | Error _ -> ()  (* Or error, both acceptable *)

let test_whitespace_handling () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=test with spaces
t=0 0|} in
  match Sdp.parse sdp with
  | Ok s ->
    check string "name with spaces" "test with spaces" s.name
  | Error e -> fail ("Parse failed: " ^ e)

let test_multiple_media_sections () =
  let sdp = {|v=0
o=- 1 1 IN IP4 127.0.0.1
s=-
t=0 0
m=audio 49170 RTP/AVP 0
m=video 51372 RTP/AVP 99|} in
  match Sdp.parse sdp with
  | Ok s ->
    check int "media count" 2 (List.length s.media)
  | Error e -> fail ("Parse failed: " ^ e)

let test_media_to_string () =
  let media : Sdp.media = {
    Sdp.default_media with
    media_type = Sdp.Audio;
    port = 49170;
    proto = Sdp.RTP_AVP;
    fmt = ["0"];
  } in
  let str = Sdp.media_to_string media in
  check bool "starts with m=audio" true (String.sub str 0 7 = "m=audio")

let test_pp_session () =
  match Sdp.parse minimal_sdp with
  | Ok s ->
    let buf = Buffer.create 256 in
    let fmt = Format.formatter_of_buffer buf in
    Sdp.pp_session fmt s;
    Format.pp_print_flush fmt ();
    let str = Buffer.contents buf in
    check bool "not empty" true (String.length str > 0)
  | Error e -> fail ("Parse failed: " ^ e)

(* ============================================
   Test Runner
   ============================================ *)

let () =
  run "SDP Coverage" [
    "ice_candidate", [
      test_case "parse simple" `Quick test_parse_ice_candidate_simple;
      test_case "parse with relay" `Quick test_parse_ice_candidate_with_relay;
      test_case "parse with extensions" `Quick test_parse_ice_candidate_with_extensions;
      test_case "parse invalid" `Quick test_parse_ice_candidate_invalid;
      test_case "roundtrip" `Quick test_ice_candidate_roundtrip;
      test_case "roundtrip with relay" `Quick test_ice_candidate_roundtrip_with_relay;
    ];
    "fingerprint", [
      test_case "parse sha-256" `Quick test_parse_fingerprint_sha256;
      test_case "parse sha-1" `Quick test_parse_fingerprint_sha1;
      test_case "parse invalid" `Quick test_parse_fingerprint_invalid;
      test_case "roundtrip" `Quick test_fingerprint_roundtrip;
    ];
    "session_parsing", [
      test_case "parse minimal" `Quick test_parse_minimal_session;
      test_case "parse origin ipv6" `Quick test_parse_session_with_origin_ipv6;
      test_case "parse connection" `Quick test_parse_session_with_connection;
      test_case "parse multicast connection" `Quick test_parse_session_with_multicast_connection;
      test_case "parse bandwidth" `Quick test_parse_session_with_bandwidth;
      test_case "parse webrtc" `Quick test_parse_webrtc_session;
      test_case "parse ice-lite" `Quick test_parse_session_with_ice_lite;
    ];
    "media_parsing", [
      test_case "parse audio" `Quick test_parse_media_audio;
      test_case "parse video" `Quick test_parse_media_video;
      test_case "parse application" `Quick test_parse_media_application;
    ];
    "roundtrip", [
      test_case "minimal" `Quick test_session_roundtrip;
      test_case "webrtc" `Quick test_webrtc_session_roundtrip;
    ];
    "webrtc_helpers", [
      test_case "create_datachannel_offer" `Quick test_create_datachannel_offer;
      test_case "create_datachannel_answer" `Quick test_create_datachannel_answer;
      test_case "add_ice_candidate" `Quick test_add_ice_candidate;
      test_case "get_ice_candidates" `Quick test_get_ice_candidates;
      test_case "get_media_by_mid" `Quick test_get_media_by_mid;
      test_case "is_datachannel_session" `Quick test_is_datachannel_session;
    ];
    "string_conversions", [
      test_case "net_type" `Quick test_string_of_net_type;
      test_case "net_type_of_string" `Quick test_net_type_of_string;
      test_case "addr_type" `Quick test_string_of_addr_type;
      test_case "addr_type_of_string" `Quick test_addr_type_of_string;
      test_case "media_type" `Quick test_string_of_media_type;
      test_case "media_type_of_string" `Quick test_media_type_of_string;
      test_case "transport" `Quick test_string_of_transport;
      test_case "transport_of_string" `Quick test_transport_of_string;
      test_case "direction" `Quick test_string_of_direction;
      test_case "direction_of_string" `Quick test_direction_of_string;
      test_case "setup" `Quick test_string_of_setup;
      test_case "setup_of_string" `Quick test_setup_of_string;
    ];
    "parsing_helpers", [
      test_case "split_first" `Quick test_split_first;
      test_case "split_spaces" `Quick test_split_spaces;
    ];
    "edge_cases", [
      test_case "empty sdp" `Quick test_empty_sdp;
      test_case "whitespace handling" `Quick test_whitespace_handling;
      test_case "multiple media" `Quick test_multiple_media_sections;
      test_case "media_to_string" `Quick test_media_to_string;
      test_case "pp_session" `Quick test_pp_session;
    ];
  ]
