(** Federation Module Coverage Tests

    Tests for MASC Federation - Level 3 Cross-Organization Collaboration:
    - trust_level: Trusted, Verified, Pending, Untrusted
    - member_status: Active, Inactive, Suspended
    - organization: id, name, endpoint, public_key, trust_level
    - federation_member: member_id, organization, capabilities, status
    - shared_state_entry: key, value, version, updated_by
    - federation_config: id, name, local_org, members, shared_state
    - handshake_challenge / handshake_response: secure joining
    - PPX-derived JSON serialization
*)

open Alcotest

module Federation = Masc_mcp.Federation

(* ============================================================
   trust_level Tests
   ============================================================ *)

let test_trust_level_trusted () =
  let t = Federation.Trusted in
  let json = Federation.trust_level_to_yojson t in
  match Federation.trust_level_of_yojson json with
  | Ok t' -> check bool "trusted" true (Federation.equal_trust_level t t')
  | Error e -> fail e

let test_trust_level_verified () =
  let t = Federation.Verified in
  let json = Federation.trust_level_to_yojson t in
  match Federation.trust_level_of_yojson json with
  | Ok t' -> check bool "verified" true (Federation.equal_trust_level t t')
  | Error e -> fail e

let test_trust_level_pending () =
  let t = Federation.Pending in
  let json = Federation.trust_level_to_yojson t in
  match Federation.trust_level_of_yojson json with
  | Ok t' -> check bool "pending" true (Federation.equal_trust_level t t')
  | Error e -> fail e

let test_trust_level_untrusted () =
  let t = Federation.Untrusted in
  let json = Federation.trust_level_to_yojson t in
  match Federation.trust_level_of_yojson json with
  | Ok t' -> check bool "untrusted" true (Federation.equal_trust_level t t')
  | Error e -> fail e

let test_trust_level_show () =
  check bool "show trusted" true (String.length (Federation.show_trust_level Federation.Trusted) > 0);
  check bool "show untrusted" true (String.length (Federation.show_trust_level Federation.Untrusted) > 0)

(* ============================================================
   member_status Tests
   ============================================================ *)

let test_member_status_active () =
  let s = Federation.Active in
  let json = Federation.member_status_to_yojson s in
  match Federation.member_status_of_yojson json with
  | Ok s' -> check bool "active" true (Federation.equal_member_status s s')
  | Error e -> fail e

let test_member_status_inactive () =
  let s = Federation.Inactive in
  let json = Federation.member_status_to_yojson s in
  match Federation.member_status_of_yojson json with
  | Ok s' -> check bool "inactive" true (Federation.equal_member_status s s')
  | Error e -> fail e

let test_member_status_suspended () =
  let s = Federation.Suspended in
  let json = Federation.member_status_to_yojson s in
  match Federation.member_status_of_yojson json with
  | Ok s' -> check bool "suspended" true (Federation.equal_member_status s s')
  | Error e -> fail e

let test_member_status_show () =
  check bool "show active" true (String.length (Federation.show_member_status Federation.Active) > 0)

(* ============================================================
   organization Tests
   ============================================================ *)

let make_org () : Federation.organization = {
  id = "org-123";
  name = "Test Org";
  endpoint = Some "https://example.com/api";
  public_key = Some "pk_test_key";
  trust_level = Federation.Verified;
  joined_at = Some "2026-01-01T00:00:00Z";
  rooms = ["room1"; "room2"];
}

let test_organization_json_roundtrip () =
  let org = make_org () in
  let json = Federation.organization_to_yojson org in
  match Federation.organization_of_yojson json with
  | Ok org' ->
    check string "id" org.id org'.id;
    check string "name" org.name org'.name;
    check (option string) "endpoint" org.endpoint org'.endpoint;
    check (option string) "public_key" org.public_key org'.public_key;
    check bool "trust_level" true (Federation.equal_trust_level org.trust_level org'.trust_level);
    check int "rooms count" (List.length org.rooms) (List.length org'.rooms)
  | Error e -> fail e

let test_organization_no_endpoint () =
  let org : Federation.organization = {
    id = "org-456";
    name = "Local Org";
    endpoint = None;
    public_key = None;
    trust_level = Federation.Trusted;
    joined_at = None;
    rooms = [];
  } in
  let json = Federation.organization_to_yojson org in
  match Federation.organization_of_yojson json with
  | Ok org' ->
    check (option string) "no endpoint" None org'.endpoint;
    check (option string) "no public_key" None org'.public_key;
    check int "empty rooms" 0 (List.length org'.rooms)
  | Error e -> fail e

let test_organization_equal () =
  let org1 = make_org () in
  let org2 = make_org () in
  check bool "equal orgs" true (Federation.equal_organization org1 org2)

let test_organization_not_equal () =
  let org1 = make_org () in
  let org2 : Federation.organization = { (make_org ()) with id = "different" } in
  check bool "not equal" false (Federation.equal_organization org1 org2)

let test_organization_show () =
  let org = make_org () in
  let s = Federation.show_organization org in
  check bool "show contains id" true (String.length s > 0)

(* ============================================================
   federation_member Tests
   ============================================================ *)

let make_member () : Federation.federation_member = {
  member_id = "member-001";
  organization = make_org ();
  capabilities = ["read"; "write"; "admin"];
  active = true;
  trust_level = Federation.Verified;
  status = Federation.Active;
  joined_at = "2026-01-01T00:00:00Z";
}

let test_federation_member_json_roundtrip () =
  let m = make_member () in
  let json = Federation.federation_member_to_yojson m in
  match Federation.federation_member_of_yojson json with
  | Ok m' ->
    check string "member_id" m.member_id m'.member_id;
    check bool "active" m.active m'.active;
    check int "capabilities" (List.length m.capabilities) (List.length m'.capabilities);
    check string "joined_at" m.joined_at m'.joined_at
  | Error e -> fail e

let test_federation_member_inactive () =
  let m : Federation.federation_member = {
    (make_member ()) with
    active = false;
    status = Federation.Inactive;
  } in
  let json = Federation.federation_member_to_yojson m in
  match Federation.federation_member_of_yojson json with
  | Ok m' ->
    check bool "inactive" false m'.active;
    check bool "status inactive" true (Federation.equal_member_status Federation.Inactive m'.status)
  | Error e -> fail e

let test_federation_member_equal () =
  let m1 = make_member () in
  let m2 = make_member () in
  check bool "equal members" true (Federation.equal_federation_member m1 m2)

let test_federation_member_show () =
  let m = make_member () in
  let s = Federation.show_federation_member m in
  check bool "show non-empty" true (String.length s > 0)

(* ============================================================
   shared_state_entry Tests
   ============================================================ *)

let make_state_entry () : Federation.shared_state_entry = {
  key = "config:version";
  value = "1.0.0";
  version = 5;
  updated_by = "org-123";
  updated_at = "2026-01-15T12:00:00Z";
}

let test_shared_state_entry_json_roundtrip () =
  let e = make_state_entry () in
  let json = Federation.shared_state_entry_to_yojson e in
  match Federation.shared_state_entry_of_yojson json with
  | Ok e' ->
    check string "key" e.key e'.key;
    check string "value" e.value e'.value;
    check int "version" e.version e'.version;
    check string "updated_by" e.updated_by e'.updated_by;
    check string "updated_at" e.updated_at e'.updated_at
  | Error err -> fail err

let test_shared_state_entry_equal () =
  let e1 = make_state_entry () in
  let e2 = make_state_entry () in
  check bool "equal entries" true (Federation.equal_shared_state_entry e1 e2)

let test_shared_state_entry_version_increment () =
  let e1 = make_state_entry () in
  let e2 : Federation.shared_state_entry = { e1 with version = e1.version + 1 } in
  check int "incremented version" (e1.version + 1) e2.version;
  check bool "not equal" false (Federation.equal_shared_state_entry e1 e2)

(* ============================================================
   federation_config Tests
   ============================================================ *)

let make_federation_config () : Federation.federation_config = {
  id = "fed-001";
  name = "Test Federation";
  local_org = make_org ();
  members = [make_member ()];
  shared_state = [make_state_entry ()];
  created_at = "2026-01-01T00:00:00Z";
  protocol_version = "1.0";
}

let test_federation_config_json_roundtrip () =
  let c = make_federation_config () in
  let json = Federation.federation_config_to_yojson c in
  match Federation.federation_config_of_yojson json with
  | Ok c' ->
    check string "id" c.id c'.id;
    check string "name" c.name c'.name;
    check string "protocol_version" c.protocol_version c'.protocol_version;
    check int "members count" (List.length c.members) (List.length c'.members);
    check int "shared_state count" (List.length c.shared_state) (List.length c'.shared_state)
  | Error e -> fail e

let test_federation_config_empty_members () =
  let c : Federation.federation_config = {
    (make_federation_config ()) with
    members = [];
    shared_state = [];
  } in
  let json = Federation.federation_config_to_yojson c in
  match Federation.federation_config_of_yojson json with
  | Ok c' ->
    check int "no members" 0 (List.length c'.members);
    check int "no state" 0 (List.length c'.shared_state)
  | Error e -> fail e

let test_federation_config_equal () =
  let c1 = make_federation_config () in
  let c2 = make_federation_config () in
  check bool "equal configs" true (Federation.equal_federation_config c1 c2)

(* ============================================================
   handshake_challenge Tests
   ============================================================ *)

let make_challenge () : Federation.handshake_challenge = {
  challenge_id = "challenge-xyz";
  from_org = make_org ();
  nonce = "random_nonce_12345";
  created_at = "2026-01-20T10:00:00Z";
  expires_at = "2026-01-20T10:05:00Z";
}

let test_handshake_challenge_json_roundtrip () =
  let c = make_challenge () in
  let json = Federation.handshake_challenge_to_yojson c in
  match Federation.handshake_challenge_of_yojson json with
  | Ok c' ->
    check string "challenge_id" c.challenge_id c'.challenge_id;
    check string "nonce" c.nonce c'.nonce;
    check string "created_at" c.created_at c'.created_at;
    check string "expires_at" c.expires_at c'.expires_at
  | Error e -> fail e

let test_handshake_challenge_equal () =
  let c1 = make_challenge () in
  let c2 = make_challenge () in
  check bool "equal challenges" true (Federation.equal_handshake_challenge c1 c2)

let test_handshake_challenge_show () =
  let c = make_challenge () in
  let s = Federation.show_handshake_challenge c in
  check bool "show non-empty" true (String.length s > 0)

(* ============================================================
   handshake_response Tests
   ============================================================ *)

let make_response () : Federation.handshake_response = {
  challenge_id = "challenge-xyz";
  nonce = "random_nonce_12345";
  signature = "sig_abc123";
  responder_org = make_org ();
}

let test_handshake_response_json_roundtrip () =
  let r = make_response () in
  let json = Federation.handshake_response_to_yojson r in
  match Federation.handshake_response_of_yojson json with
  | Ok r' ->
    check string "challenge_id" r.challenge_id r'.challenge_id;
    check string "nonce" r.nonce r'.nonce;
    check string "signature" r.signature r'.signature
  | Error e -> fail e

let test_handshake_response_equal () =
  let r1 = make_response () in
  let r2 = make_response () in
  check bool "equal responses" true (Federation.equal_handshake_response r1 r2)

let test_handshake_response_show () =
  let r = make_response () in
  let s = Federation.show_handshake_response r in
  check bool "show non-empty" true (String.length s > 0)

(* ============================================================
   validate_id Tests
   ============================================================ *)

let test_validate_id_valid () =
  match Federation.validate_id "org-123" "org_id" with
  | Ok id -> check string "valid id" "org-123" id
  | Error _ -> fail "expected Ok"

let test_validate_id_empty () =
  match Federation.validate_id "" "org_id" with
  | Error msg -> check bool "contains empty" true (String.length msg > 0)
  | Ok _ -> fail "expected Error"

let test_validate_id_too_long () =
  let long_id = String.make 257 'x' in
  match Federation.validate_id long_id "org_id" with
  | Error msg -> check bool "contains too long" true (String.length msg > 0)
  | Ok _ -> fail "expected Error"

let test_validate_id_with_slash () =
  match Federation.validate_id "org/123" "org_id" with
  | Error msg -> check bool "contains separator" true (String.length msg > 0)
  | Ok _ -> fail "expected Error"

let test_validate_id_with_backslash () =
  match Federation.validate_id "org\\123" "org_id" with
  | Error msg -> check bool "contains backslash" true (String.length msg > 0)
  | Ok _ -> fail "expected Error"

let test_validate_id_with_null () =
  match Federation.validate_id "org\000123" "org_id" with
  | Error msg -> check bool "contains null" true (String.length msg > 0)
  | Ok _ -> fail "expected Error"

let test_validate_id_max_length () =
  let max_id = String.make 256 'a' in
  match Federation.validate_id max_id "org_id" with
  | Ok id -> check int "max length" 256 (String.length id)
  | Error _ -> fail "expected Ok for max length"

(* ============================================================
   validate_endpoint Tests
   ============================================================ *)

let test_validate_endpoint_https () =
  match Federation.validate_endpoint "https://example.com/api" with
  | Ok url -> check bool "starts with https" true (String.sub url 0 8 = "https://")
  | Error _ -> fail "expected Ok"

let test_validate_endpoint_http () =
  match Federation.validate_endpoint "http://localhost:8080" with
  | Ok url -> check bool "starts with http" true (String.sub url 0 7 = "http://")
  | Error _ -> fail "expected Ok"

let test_validate_endpoint_empty () =
  match Federation.validate_endpoint "" with
  | Error msg -> check bool "contains empty" true (String.length msg > 0)
  | Ok _ -> fail "expected Error"

let test_validate_endpoint_invalid () =
  match Federation.validate_endpoint "ftp://example.com" with
  | Error msg -> check bool "contains valid URL" true (String.length msg > 0)
  | Ok _ -> fail "expected Error for ftp"

let test_validate_endpoint_no_protocol () =
  match Federation.validate_endpoint "example.com/api" with
  | Error msg -> check bool "contains valid URL" true (String.length msg > 0)
  | Ok _ -> fail "expected Error for no protocol"

(* ============================================================
   trust_level_to_int / int_to_trust_level Tests
   ============================================================ *)

let test_trust_level_to_int_trusted () =
  check int "Trusted=4" 4 (Federation.trust_level_to_int Federation.Trusted)

let test_trust_level_to_int_verified () =
  check int "Verified=3" 3 (Federation.trust_level_to_int Federation.Verified)

let test_trust_level_to_int_pending () =
  check int "Pending=2" 2 (Federation.trust_level_to_int Federation.Pending)

let test_trust_level_to_int_untrusted () =
  check int "Untrusted=1" 1 (Federation.trust_level_to_int Federation.Untrusted)

let test_int_to_trust_level_4 () =
  check bool "4=Trusted" true
    (Federation.equal_trust_level Federation.Trusted (Federation.int_to_trust_level 4))

let test_int_to_trust_level_3 () =
  check bool "3=Verified" true
    (Federation.equal_trust_level Federation.Verified (Federation.int_to_trust_level 3))

let test_int_to_trust_level_2 () =
  check bool "2=Pending" true
    (Federation.equal_trust_level Federation.Pending (Federation.int_to_trust_level 2))

let test_int_to_trust_level_1 () =
  check bool "1=Untrusted" true
    (Federation.equal_trust_level Federation.Untrusted (Federation.int_to_trust_level 1))

let test_int_to_trust_level_0 () =
  check bool "0=Untrusted" true
    (Federation.equal_trust_level Federation.Untrusted (Federation.int_to_trust_level 0))

let test_int_to_trust_level_5 () =
  check bool "5=Trusted" true
    (Federation.equal_trust_level Federation.Trusted (Federation.int_to_trust_level 5))

let test_int_to_trust_level_negative () =
  check bool "-1=Untrusted" true
    (Federation.equal_trust_level Federation.Untrusted (Federation.int_to_trust_level (-1)))

let test_trust_level_roundtrip () =
  List.iter (fun level ->
    let i = Federation.trust_level_to_int level in
    let level' = Federation.int_to_trust_level i in
    check bool "roundtrip" true (Federation.equal_trust_level level level')
  ) [Federation.Trusted; Federation.Verified; Federation.Pending; Federation.Untrusted]

(* ============================================================
   can_delegate_to Tests
   ============================================================ *)

let make_active_member trust_level : Federation.federation_member = {
  member_id = "member-001";
  organization = make_org ();
  capabilities = ["task"; "broadcast"];
  active = true;
  trust_level;
  status = Federation.Active;
  joined_at = "2026-01-01T00:00:00Z";
}

let test_can_delegate_to_trusted_verified () =
  let member = make_active_member Federation.Trusted in
  check bool "Trusted >= Verified" true
    (Federation.can_delegate_to ~member ~min_trust:Federation.Verified)

let test_can_delegate_to_verified_verified () =
  let member = make_active_member Federation.Verified in
  check bool "Verified >= Verified" true
    (Federation.can_delegate_to ~member ~min_trust:Federation.Verified)

let test_can_delegate_to_pending_verified () =
  let member = make_active_member Federation.Pending in
  check bool "Pending < Verified" false
    (Federation.can_delegate_to ~member ~min_trust:Federation.Verified)

let test_can_delegate_to_untrusted_verified () =
  let member = make_active_member Federation.Untrusted in
  check bool "Untrusted < Verified" false
    (Federation.can_delegate_to ~member ~min_trust:Federation.Verified)

let test_can_delegate_to_inactive_member () =
  let member = { (make_active_member Federation.Trusted) with active = false } in
  check bool "inactive member" false
    (Federation.can_delegate_to ~member ~min_trust:Federation.Untrusted)

let test_can_delegate_to_suspended_member () =
  let member = { (make_active_member Federation.Trusted) with status = Federation.Suspended } in
  check bool "suspended member" false
    (Federation.can_delegate_to ~member ~min_trust:Federation.Untrusted)

(* ============================================================
   update_trust Tests
   ============================================================ *)

let test_update_trust_success_verified () =
  let member = make_active_member Federation.Verified in
  let updated = Federation.update_trust ~member ~success:true in
  check bool "Verified+1=Trusted" true
    (Federation.equal_trust_level Federation.Trusted updated.trust_level)

let test_update_trust_success_trusted () =
  let member = make_active_member Federation.Trusted in
  let updated = Federation.update_trust ~member ~success:true in
  check bool "Trusted stays Trusted" true
    (Federation.equal_trust_level Federation.Trusted updated.trust_level)

let test_update_trust_failure_verified () =
  let member = make_active_member Federation.Verified in
  let updated = Federation.update_trust ~member ~success:false in
  check bool "Verified-1=Pending" true
    (Federation.equal_trust_level Federation.Pending updated.trust_level)

let test_update_trust_failure_untrusted () =
  let member = make_active_member Federation.Untrusted in
  let updated = Federation.update_trust ~member ~success:false in
  check bool "Untrusted stays Untrusted" true
    (Federation.equal_trust_level Federation.Untrusted updated.trust_level)

let test_update_trust_success_pending () =
  let member = make_active_member Federation.Pending in
  let updated = Federation.update_trust ~member ~success:true in
  check bool "Pending+1=Verified" true
    (Federation.equal_trust_level Federation.Verified updated.trust_level)

let test_update_trust_failure_pending () =
  let member = make_active_member Federation.Pending in
  let updated = Federation.update_trust ~member ~success:false in
  check bool "Pending-1=Untrusted" true
    (Federation.equal_trust_level Federation.Untrusted updated.trust_level)

(* ============================================================
   make_local_org Tests
   ============================================================ *)

let test_make_local_org_basic () =
  let org = Federation.make_local_org ~id:"local-001" ~name:"Local Org" () in
  check string "id" "local-001" org.id;
  check string "name" "Local Org" org.name;
  check bool "trust=Trusted" true (Federation.equal_trust_level Federation.Trusted org.trust_level);
  check (option string) "endpoint" None org.endpoint;
  check (option string) "public_key" None org.public_key

let test_make_local_org_with_capabilities () =
  let org = Federation.make_local_org ~id:"local-002" ~name:"Local Org"
    ~capabilities:["room1"; "room2"] () in
  check int "rooms count" 2 (List.length org.rooms);
  check bool "has room1" true (List.mem "room1" org.rooms)

let test_make_local_org_empty_capabilities () =
  let org = Federation.make_local_org ~id:"local-003" ~name:"Local Org" () in
  check int "empty rooms" 0 (List.length org.rooms)

(* ============================================================
   make_federation_member Tests
   ============================================================ *)

let test_make_federation_member_basic () =
  let org = make_org () in
  let now = "2026-01-20T12:00:00Z" in
  let member = Federation.make_federation_member ~org ~now in
  check string "member_id" org.id member.member_id;
  check string "joined_at" now member.joined_at;
  check bool "active" true member.active;
  check bool "status=Active" true (Federation.equal_member_status Federation.Active member.status)

let test_make_federation_member_capabilities () =
  let org : Federation.organization = {
    (make_org ()) with rooms = ["cap1"; "cap2"; "cap3"]
  } in
  let member = Federation.make_federation_member ~org ~now:"2026-01-20T12:00:00Z" in
  check int "capabilities count" 3 (List.length member.capabilities)

let test_make_federation_member_trust_level () =
  let org : Federation.organization = { (make_org ()) with trust_level = Federation.Pending } in
  let member = Federation.make_federation_member ~org ~now:"2026-01-20T12:00:00Z" in
  check bool "inherits trust" true
    (Federation.equal_trust_level Federation.Pending member.trust_level)

(* ============================================================
   delegation_request JSON Tests
   ============================================================ *)

let make_task () : Masc_mcp.Types.task = {
  id = "task-001";
  title = "Test Task";
  description = "A test task";
  task_status = Masc_mcp.Types.Todo;
  priority = 1;
  files = [];
  created_at = "2026-01-01T00:00:00Z";
  worktree = None;
}

let make_delegation_request () : Federation.delegation_request = {
  id = "del-001";
  from_org = "org-local";
  to_org = "org-remote";
  task = make_task ();
  priority = 2;
  timeout_seconds = Some 3600;
  created_at = "2026-01-20T12:00:00Z";
  status = "pending";
  result = None;
}

let test_delegation_request_to_json () =
  let r = make_delegation_request () in
  let json = Federation.delegation_request_to_yojson r in
  match json with
  | `Assoc fields ->
    check bool "has id" true (List.mem_assoc "id" fields);
    check bool "has from_org" true (List.mem_assoc "from_org" fields);
    check bool "has to_org" true (List.mem_assoc "to_org" fields);
    check bool "has task" true (List.mem_assoc "task" fields);
    check bool "has priority" true (List.mem_assoc "priority" fields);
    check bool "has status" true (List.mem_assoc "status" fields)
  | _ -> fail "expected Assoc"

let test_delegation_request_roundtrip () =
  let r = make_delegation_request () in
  let json = Federation.delegation_request_to_yojson r in
  match Federation.delegation_request_of_yojson json with
  | Ok r' ->
    check string "id" r.id r'.id;
    check string "from_org" r.from_org r'.from_org;
    check string "to_org" r.to_org r'.to_org;
    check int "priority" r.priority r'.priority;
    check string "status" r.status r'.status
  | Error e -> fail e

let test_delegation_request_no_timeout () =
  let r = { (make_delegation_request ()) with timeout_seconds = None } in
  let json = Federation.delegation_request_to_yojson r in
  match Federation.delegation_request_of_yojson json with
  | Ok r' -> check (option int) "no timeout" None r'.timeout_seconds
  | Error e -> fail e

let test_delegation_request_with_result () =
  let r = { (make_delegation_request ()) with result = Some "completed successfully" } in
  let json = Federation.delegation_request_to_yojson r in
  match Federation.delegation_request_of_yojson json with
  | Ok r' -> check (option string) "result" (Some "completed successfully") r'.result
  | Error e -> fail e

(* ============================================================
   federation_event JSON Tests
   ============================================================ *)

let test_federation_event_handshake_success () =
  let e = Federation.HandshakeSuccess { org_id = "org-001"; timestamp = "2026-01-20T12:00:00Z" } in
  let json = Federation.federation_event_to_yojson e in
  match Federation.federation_event_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_event e e')
  | Error err -> fail err

let test_federation_event_org_joined () =
  let e = Federation.OrgJoined { org_id = "org-002"; timestamp = "2026-01-20T12:00:00Z" } in
  let json = Federation.federation_event_to_yojson e in
  match Federation.federation_event_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_event e e')
  | Error err -> fail err

let test_federation_event_org_left () =
  let e = Federation.OrgLeft { org_id = "org-003"; reason = "voluntary"; timestamp = "2026-01-20T12:00:00Z" } in
  let json = Federation.federation_event_to_yojson e in
  match Federation.federation_event_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_event e e')
  | Error err -> fail err

let test_federation_event_task_delegated () =
  let e = Federation.TaskDelegated {
    task_id = "task-001";
    from_org = "org-local";
    to_org = "org-remote";
    task = "test task";
    timestamp = "2026-01-20T12:00:00Z"
  } in
  let json = Federation.federation_event_to_yojson e in
  match Federation.federation_event_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_event e e')
  | Error err -> fail err

let test_federation_event_task_completed () =
  let e = Federation.TaskCompleted { task_id = "task-001"; result = "success"; timestamp = "2026-01-20T12:00:00Z" } in
  let json = Federation.federation_event_to_yojson e in
  match Federation.federation_event_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_event e e')
  | Error err -> fail err

let test_federation_event_trust_updated () =
  let e = Federation.TrustUpdated {
    org_id = "org-001";
    old_level = Federation.Pending;
    new_level = Federation.Verified;
    timestamp = "2026-01-20T12:00:00Z"
  } in
  let json = Federation.federation_event_to_yojson e in
  match Federation.federation_event_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_event e e')
  | Error err -> fail err

let test_federation_event_config_updated () =
  let e = Federation.ConfigUpdated { timestamp = "2026-01-20T12:00:00Z" } in
  let json = Federation.federation_event_to_yojson e in
  match Federation.federation_event_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_event e e')
  | Error err -> fail err

let test_federation_event_show () =
  let e = Federation.HandshakeSuccess { org_id = "org-001"; timestamp = "2026-01-20T12:00:00Z" } in
  check bool "show non-empty" true (String.length (Federation.show_federation_event e) > 0)

(* ============================================================
   federation_error JSON Tests
   ============================================================ *)

let test_federation_error_invalid_challenge () =
  let e = Federation.InvalidChallenge in
  let json = Federation.federation_error_to_yojson e in
  match Federation.federation_error_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_error e e')
  | Error err -> fail err

let test_federation_error_expired_challenge () =
  let e = Federation.ExpiredChallenge in
  let json = Federation.federation_error_to_yojson e in
  match Federation.federation_error_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_error e e')
  | Error err -> fail err

let test_federation_error_invalid_signature () =
  let e = Federation.InvalidSignature in
  let json = Federation.federation_error_to_yojson e in
  match Federation.federation_error_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_error e e')
  | Error err -> fail err

let test_federation_error_org_not_found () =
  let e = Federation.OrgNotFound "org-xyz" in
  let json = Federation.federation_error_to_yojson e in
  match Federation.federation_error_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_error e e')
  | Error err -> fail err

let test_federation_error_config_error () =
  let e = Federation.ConfigError "config issue" in
  let json = Federation.federation_error_to_yojson e in
  match Federation.federation_error_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_error e e')
  | Error err -> fail err

let test_federation_error_trust_too_low () =
  let e = Federation.TrustTooLow {
    org_id = "org-001";
    required = Federation.Verified;
    actual = Federation.Pending
  } in
  let json = Federation.federation_error_to_yojson e in
  match Federation.federation_error_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_error e e')
  | Error err -> fail err

let test_federation_error_delegation_failed () =
  let e = Federation.DelegationFailed { task_id = "task-001"; reason = "timeout" } in
  let json = Federation.federation_error_to_yojson e in
  match Federation.federation_error_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_error e e')
  | Error err -> fail err

let test_federation_error_not_initialized () =
  let e = Federation.FederationNotInitialized in
  let json = Federation.federation_error_to_yojson e in
  match Federation.federation_error_of_yojson json with
  | Ok e' -> check bool "equal" true (Federation.equal_federation_error e e')
  | Error err -> fail err

let test_federation_error_show () =
  let e = Federation.OrgNotFound "org-xyz" in
  check bool "show non-empty" true (String.length (Federation.show_federation_error e) > 0)

(* ============================================================
   discover_remote Tests
   ============================================================ *)

let test_discover_remote_structure () =
  let result = Federation.discover_remote ~endpoint:"https://example.com" in
  match result with
  | `Assoc fields ->
    check bool "has type" true (List.mem_assoc "type" fields);
    check bool "has endpoint" true (List.mem_assoc "endpoint" fields);
    check bool "has well_known_url" true (List.mem_assoc "well_known_url" fields);
    check bool "has next_steps" true (List.mem_assoc "next_steps" fields)
  | _ -> fail "expected Assoc"

let test_discover_remote_well_known_url () =
  let result = Federation.discover_remote ~endpoint:"https://example.com" in
  match result with
  | `Assoc fields ->
    (match List.assoc_opt "well_known_url" fields with
     | Some (`String url) ->
       check bool "contains agent-card" true
         (String.length url > 0 &&
          try let _ = Str.search_forward (Str.regexp_string "agent-card") url 0 in true
          with Not_found -> false)
     | _ -> fail "expected string")
  | _ -> fail "expected Assoc"

let test_discover_remote_type () =
  let result = Federation.discover_remote ~endpoint:"https://remote.org" in
  match result with
  | `Assoc fields ->
    (match List.assoc_opt "type" fields with
     | Some (`String t) -> check string "type" "remote_discovery" t
     | _ -> fail "expected string")
  | _ -> fail "expected Assoc"

(* ============================================================
   default_trust_threshold Tests
   ============================================================ *)

let test_default_trust_threshold () =
  check bool "default is Verified" true
    (Federation.equal_trust_level Federation.Verified Federation.default_trust_threshold)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Federation Coverage" [
    "trust_level", [
      test_case "trusted" `Quick test_trust_level_trusted;
      test_case "verified" `Quick test_trust_level_verified;
      test_case "pending" `Quick test_trust_level_pending;
      test_case "untrusted" `Quick test_trust_level_untrusted;
      test_case "show" `Quick test_trust_level_show;
    ];
    "member_status", [
      test_case "active" `Quick test_member_status_active;
      test_case "inactive" `Quick test_member_status_inactive;
      test_case "suspended" `Quick test_member_status_suspended;
      test_case "show" `Quick test_member_status_show;
    ];
    "organization", [
      test_case "json roundtrip" `Quick test_organization_json_roundtrip;
      test_case "no endpoint" `Quick test_organization_no_endpoint;
      test_case "equal" `Quick test_organization_equal;
      test_case "not equal" `Quick test_organization_not_equal;
      test_case "show" `Quick test_organization_show;
    ];
    "federation_member", [
      test_case "json roundtrip" `Quick test_federation_member_json_roundtrip;
      test_case "inactive" `Quick test_federation_member_inactive;
      test_case "equal" `Quick test_federation_member_equal;
      test_case "show" `Quick test_federation_member_show;
    ];
    "shared_state_entry", [
      test_case "json roundtrip" `Quick test_shared_state_entry_json_roundtrip;
      test_case "equal" `Quick test_shared_state_entry_equal;
      test_case "version increment" `Quick test_shared_state_entry_version_increment;
    ];
    "federation_config", [
      test_case "json roundtrip" `Quick test_federation_config_json_roundtrip;
      test_case "empty members" `Quick test_federation_config_empty_members;
      test_case "equal" `Quick test_federation_config_equal;
    ];
    "handshake_challenge", [
      test_case "json roundtrip" `Quick test_handshake_challenge_json_roundtrip;
      test_case "equal" `Quick test_handshake_challenge_equal;
      test_case "show" `Quick test_handshake_challenge_show;
    ];
    "handshake_response", [
      test_case "json roundtrip" `Quick test_handshake_response_json_roundtrip;
      test_case "equal" `Quick test_handshake_response_equal;
      test_case "show" `Quick test_handshake_response_show;
    ];
    "validate_id", [
      test_case "valid" `Quick test_validate_id_valid;
      test_case "empty" `Quick test_validate_id_empty;
      test_case "too long" `Quick test_validate_id_too_long;
      test_case "with slash" `Quick test_validate_id_with_slash;
      test_case "with backslash" `Quick test_validate_id_with_backslash;
      test_case "with null" `Quick test_validate_id_with_null;
      test_case "max length" `Quick test_validate_id_max_length;
    ];
    "validate_endpoint", [
      test_case "https" `Quick test_validate_endpoint_https;
      test_case "http" `Quick test_validate_endpoint_http;
      test_case "empty" `Quick test_validate_endpoint_empty;
      test_case "invalid" `Quick test_validate_endpoint_invalid;
      test_case "no protocol" `Quick test_validate_endpoint_no_protocol;
    ];
    "trust_level_conversion", [
      test_case "to_int trusted" `Quick test_trust_level_to_int_trusted;
      test_case "to_int verified" `Quick test_trust_level_to_int_verified;
      test_case "to_int pending" `Quick test_trust_level_to_int_pending;
      test_case "to_int untrusted" `Quick test_trust_level_to_int_untrusted;
      test_case "int_to 4" `Quick test_int_to_trust_level_4;
      test_case "int_to 3" `Quick test_int_to_trust_level_3;
      test_case "int_to 2" `Quick test_int_to_trust_level_2;
      test_case "int_to 1" `Quick test_int_to_trust_level_1;
      test_case "int_to 0" `Quick test_int_to_trust_level_0;
      test_case "int_to 5" `Quick test_int_to_trust_level_5;
      test_case "int_to negative" `Quick test_int_to_trust_level_negative;
      test_case "roundtrip" `Quick test_trust_level_roundtrip;
    ];
    "can_delegate_to", [
      test_case "trusted >= verified" `Quick test_can_delegate_to_trusted_verified;
      test_case "verified >= verified" `Quick test_can_delegate_to_verified_verified;
      test_case "pending < verified" `Quick test_can_delegate_to_pending_verified;
      test_case "untrusted < verified" `Quick test_can_delegate_to_untrusted_verified;
      test_case "inactive member" `Quick test_can_delegate_to_inactive_member;
      test_case "suspended member" `Quick test_can_delegate_to_suspended_member;
    ];
    "update_trust", [
      test_case "success verified" `Quick test_update_trust_success_verified;
      test_case "success trusted" `Quick test_update_trust_success_trusted;
      test_case "failure verified" `Quick test_update_trust_failure_verified;
      test_case "failure untrusted" `Quick test_update_trust_failure_untrusted;
      test_case "success pending" `Quick test_update_trust_success_pending;
      test_case "failure pending" `Quick test_update_trust_failure_pending;
    ];
    "make_local_org", [
      test_case "basic" `Quick test_make_local_org_basic;
      test_case "with capabilities" `Quick test_make_local_org_with_capabilities;
      test_case "empty capabilities" `Quick test_make_local_org_empty_capabilities;
    ];
    "make_federation_member", [
      test_case "basic" `Quick test_make_federation_member_basic;
      test_case "capabilities" `Quick test_make_federation_member_capabilities;
      test_case "trust level" `Quick test_make_federation_member_trust_level;
    ];
    "delegation_request", [
      test_case "to json" `Quick test_delegation_request_to_json;
      test_case "roundtrip" `Quick test_delegation_request_roundtrip;
      test_case "no timeout" `Quick test_delegation_request_no_timeout;
      test_case "with result" `Quick test_delegation_request_with_result;
    ];
    "federation_event", [
      test_case "handshake success" `Quick test_federation_event_handshake_success;
      test_case "org joined" `Quick test_federation_event_org_joined;
      test_case "org left" `Quick test_federation_event_org_left;
      test_case "task delegated" `Quick test_federation_event_task_delegated;
      test_case "task completed" `Quick test_federation_event_task_completed;
      test_case "trust updated" `Quick test_federation_event_trust_updated;
      test_case "config updated" `Quick test_federation_event_config_updated;
      test_case "show" `Quick test_federation_event_show;
    ];
    "federation_error", [
      test_case "invalid challenge" `Quick test_federation_error_invalid_challenge;
      test_case "expired challenge" `Quick test_federation_error_expired_challenge;
      test_case "invalid signature" `Quick test_federation_error_invalid_signature;
      test_case "org not found" `Quick test_federation_error_org_not_found;
      test_case "config error" `Quick test_federation_error_config_error;
      test_case "trust too low" `Quick test_federation_error_trust_too_low;
      test_case "delegation failed" `Quick test_federation_error_delegation_failed;
      test_case "not initialized" `Quick test_federation_error_not_initialized;
      test_case "show" `Quick test_federation_error_show;
    ];
    "discover_remote", [
      test_case "structure" `Quick test_discover_remote_structure;
      test_case "well known url" `Quick test_discover_remote_well_known_url;
      test_case "type" `Quick test_discover_remote_type;
    ];
    "default_trust_threshold", [
      test_case "is verified" `Quick test_default_trust_threshold;
    ];
  ]
