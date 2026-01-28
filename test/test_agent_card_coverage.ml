(** Agent Card Coverage Tests - A2A Protocol Compatible Agent Metadata *)

open Alcotest

module Agent_card = Masc_mcp.Agent_card

(* ============================================================
   Provider Tests
   ============================================================ *)

let test_provider_json_roundtrip () =
  let p : Agent_card.provider = {
    organization = "Test Org";
    url = Some "https://example.com";
  } in
  let json = Agent_card.provider_to_yojson p in
  match Agent_card.provider_of_yojson json with
  | Ok p' ->
    check string "organization" p.organization p'.organization;
    check (option string) "url" p.url p'.url
  | Error e -> fail ("Provider roundtrip failed: " ^ e)

let test_provider_without_url () =
  let p : Agent_card.provider = {
    organization = "Minimal Org";
    url = None;
  } in
  let json = Agent_card.provider_to_yojson p in
  match Agent_card.provider_of_yojson json with
  | Ok p' ->
    check string "organization" "Minimal Org" p'.organization;
    check (option string) "url" None p'.url
  | Error e -> fail ("Provider roundtrip failed: " ^ e)

let test_provider_show () =
  let p : Agent_card.provider = {
    organization = "Show Test";
    url = Some "https://show.test";
  } in
  let s = Agent_card.show_provider p in
  check bool "contains organization" true (String.length s > 0);
  check bool "contains Show Test" true (Str.string_match (Str.regexp ".*Show Test.*") s 0)

let test_provider_eq () =
  let p1 : Agent_card.provider = { organization = "A"; url = Some "x" } in
  let p2 : Agent_card.provider = { organization = "A"; url = Some "x" } in
  let p3 : Agent_card.provider = { organization = "B"; url = Some "x" } in
  check bool "equal same" true (Agent_card.equal_provider p1 p2);
  check bool "not equal diff org" false (Agent_card.equal_provider p1 p3)

(* ============================================================
   Skill Tests
   ============================================================ *)

let test_skill_json_roundtrip () =
  let s : Agent_card.skill = {
    id = "test-skill";
    name = "Test Skill";
    description = Some "A test skill";
    input_modes = ["text"; "data"];
    output_modes = ["text"; "file"];
  } in
  let json = Agent_card.skill_to_yojson s in
  match Agent_card.skill_of_yojson json with
  | Ok s' ->
    check string "id" s.id s'.id;
    check string "name" s.name s'.name;
    check (option string) "description" s.description s'.description;
    check (list string) "input_modes" s.input_modes s'.input_modes;
    check (list string) "output_modes" s.output_modes s'.output_modes
  | Error e -> fail ("Skill roundtrip failed: " ^ e)

let test_skill_without_description () =
  let s : Agent_card.skill = {
    id = "minimal";
    name = "Minimal";
    description = None;
    input_modes = [];
    output_modes = [];
  } in
  let json = Agent_card.skill_to_yojson s in
  match Agent_card.skill_of_yojson json with
  | Ok s' ->
    check string "id" "minimal" s'.id;
    check (option string) "description" None s'.description
  | Error e -> fail ("Skill roundtrip failed: " ^ e)

let test_skill_show () =
  let s : Agent_card.skill = {
    id = "show-test";
    name = "Show Test Skill";
    description = Some "Test";
    input_modes = ["text"];
    output_modes = ["text"];
  } in
  let str = Agent_card.show_skill s in
  check bool "non-empty" true (String.length str > 0)

let test_skill_eq () =
  let s1 : Agent_card.skill = {
    id = "a"; name = "A"; description = None; input_modes = []; output_modes = []
  } in
  let s2 : Agent_card.skill = {
    id = "a"; name = "A"; description = None; input_modes = []; output_modes = []
  } in
  let s3 : Agent_card.skill = {
    id = "b"; name = "B"; description = None; input_modes = []; output_modes = []
  } in
  check bool "equal same" true (Agent_card.equal_skill s1 s2);
  check bool "not equal diff" false (Agent_card.equal_skill s1 s3)

(* ============================================================
   Binding Tests
   ============================================================ *)

let test_binding_json_roundtrip () =
  let b : Agent_card.binding = {
    protocol = "json-rpc";
    url = "http://localhost:8935/mcp";
  } in
  let json = Agent_card.binding_to_yojson b in
  match Agent_card.binding_of_yojson json with
  | Ok b' ->
    check string "protocol" b.protocol b'.protocol;
    check string "url" b.url b'.url
  | Error e -> fail ("Binding roundtrip failed: " ^ e)

let test_binding_grpc () =
  let b : Agent_card.binding = {
    protocol = "grpc";
    url = "grpc://127.0.0.1:9935";
  } in
  let json = Agent_card.binding_to_yojson b in
  match Agent_card.binding_of_yojson json with
  | Ok b' ->
    check string "protocol" "grpc" b'.protocol;
    check bool "url starts with grpc" true (String.sub b'.url 0 4 = "grpc")
  | Error e -> fail ("Binding roundtrip failed: " ^ e)

let test_binding_show () =
  let b : Agent_card.binding = { protocol = "sse"; url = "http://test/sse" } in
  let s = Agent_card.show_binding b in
  check bool "non-empty" true (String.length s > 0)

let test_binding_eq () =
  let b1 : Agent_card.binding = { protocol = "x"; url = "y" } in
  let b2 : Agent_card.binding = { protocol = "x"; url = "y" } in
  let b3 : Agent_card.binding = { protocol = "z"; url = "y" } in
  check bool "equal same" true (Agent_card.equal_binding b1 b2);
  check bool "not equal diff" false (Agent_card.equal_binding b1 b3)

(* ============================================================
   Security Scheme Tests
   ============================================================ *)

let test_security_scheme_bearer () =
  let s : Agent_card.security_scheme = {
    scheme_type = "bearer";
    bearer_format = Some "JWT";
    api_key_name = None;
    api_key_in = None;
  } in
  let json = Agent_card.security_scheme_to_yojson s in
  match Agent_card.security_scheme_of_yojson json with
  | Ok s' ->
    check string "scheme_type" "bearer" s'.scheme_type;
    check (option string) "bearer_format" (Some "JWT") s'.bearer_format
  | Error e -> fail ("Security scheme roundtrip failed: " ^ e)

let test_security_scheme_apikey () =
  let s : Agent_card.security_scheme = {
    scheme_type = "apiKey";
    bearer_format = None;
    api_key_name = Some "X-API-Key";
    api_key_in = Some "header";
  } in
  let json = Agent_card.security_scheme_to_yojson s in
  match Agent_card.security_scheme_of_yojson json with
  | Ok s' ->
    check string "scheme_type" "apiKey" s'.scheme_type;
    check (option string) "api_key_name" (Some "X-API-Key") s'.api_key_name;
    check (option string) "api_key_in" (Some "header") s'.api_key_in
  | Error e -> fail ("Security scheme roundtrip failed: " ^ e)

let test_security_scheme_none () =
  let s : Agent_card.security_scheme = {
    scheme_type = "none";
    bearer_format = None;
    api_key_name = None;
    api_key_in = None;
  } in
  let json = Agent_card.security_scheme_to_yojson s in
  match Agent_card.security_scheme_of_yojson json with
  | Ok s' -> check string "scheme_type" "none" s'.scheme_type
  | Error e -> fail ("Security scheme roundtrip failed: " ^ e)

let test_security_scheme_show () =
  let s : Agent_card.security_scheme = {
    scheme_type = "bearer"; bearer_format = Some "MASC"; api_key_name = None; api_key_in = None
  } in
  let str = Agent_card.show_security_scheme s in
  check bool "non-empty" true (String.length str > 0)

let test_security_scheme_eq () =
  let s1 : Agent_card.security_scheme = {
    scheme_type = "none"; bearer_format = None; api_key_name = None; api_key_in = None
  } in
  let s2 : Agent_card.security_scheme = {
    scheme_type = "none"; bearer_format = None; api_key_name = None; api_key_in = None
  } in
  check bool "equal same" true (Agent_card.equal_security_scheme s1 s2)

(* ============================================================
   Agent Card Tests
   ============================================================ *)

let test_generate_default () =
  let card = Agent_card.generate_default () in
  check string "name" "MASC-MCP" card.name;
  check string "version" "2.0.0" card.version;
  check bool "has description" true (Option.is_some card.description);
  check bool "has provider" true (Option.is_some card.provider);
  check bool "has capabilities" true (List.length card.capabilities > 0);
  check bool "has skills" true (List.length card.skills > 0);
  check bool "has bindings" true (List.length card.bindings > 0)

let test_generate_with_custom_port () =
  let card = Agent_card.generate_default ~port:9000 () in
  let has_9000_binding = List.exists (fun (b : Agent_card.binding) ->
    Str.string_match (Str.regexp ".*9000.*") b.url 0
  ) card.bindings in
  check bool "binding has custom port" true has_9000_binding

let test_generate_with_custom_host () =
  let card = Agent_card.generate_default ~host:"0.0.0.0" () in
  let has_custom_host = List.exists (fun (b : Agent_card.binding) ->
    Str.string_match (Str.regexp ".*0\\.0\\.0\\.0.*") b.url 0
  ) card.bindings in
  check bool "binding has custom host" true has_custom_host

let test_agent_card_to_json () =
  let card = Agent_card.generate_default () in
  let json = Agent_card.to_json card in
  match json with
  | `Assoc fields ->
    check bool "has name" true (List.mem_assoc "name" fields);
    check bool "has version" true (List.mem_assoc "version" fields);
    check bool "has capabilities" true (List.mem_assoc "capabilities" fields);
    check bool "has skills" true (List.mem_assoc "skills" fields);
    check bool "has bindings" true (List.mem_assoc "bindings" fields);
    check bool "has createdAt" true (List.mem_assoc "createdAt" fields);
    check bool "has updatedAt" true (List.mem_assoc "updatedAt" fields)
  | _ -> fail "Expected JSON object"

let test_agent_card_json_roundtrip () =
  let card = Agent_card.generate_default () in
  let json = Agent_card.to_json card in
  match Agent_card.from_json json with
  | Ok card' ->
    check string "name" card.name card'.name;
    check string "version" card.version card'.version;
    check (list string) "capabilities" card.capabilities card'.capabilities;
    check int "skills count" (List.length card.skills) (List.length card'.skills);
    check int "bindings count" (List.length card.bindings) (List.length card'.bindings)
  | Error e -> fail ("Agent card roundtrip failed: " ^ e)

let test_from_json_invalid () =
  let json = `String "not an object" in
  match Agent_card.from_json json with
  | Ok _ -> fail "Should fail on invalid JSON"
  | Error _ -> ()

let test_from_json_missing_fields () =
  let json = `Assoc [("name", `String "Test")] in
  match Agent_card.from_json json with
  | Ok _ -> fail "Should fail on missing required fields"
  | Error _ -> ()

let test_with_bindings () =
  let card = Agent_card.generate_default () in
  let new_bindings : Agent_card.binding list = [
    { protocol = "custom"; url = "custom://test" }
  ] in
  let card' = Agent_card.with_bindings card new_bindings in
  check int "bindings count" 1 (List.length card'.bindings);
  check string "binding protocol" "custom" (List.hd card'.bindings).protocol

let test_with_extension () =
  let card = Agent_card.generate_default () in
  let card' = Agent_card.with_extension card "custom_ext" (`String "value") in
  let has_custom = List.exists (fun (k, _) -> k = "custom_ext") card'.extensions in
  check bool "has custom extension" true has_custom

let test_with_extension_overwrite () =
  let card = Agent_card.generate_default () in
  let card' = Agent_card.with_extension card "masc" (`String "overwritten") in
  (* Should overwrite the default masc extension *)
  let masc_ext = List.find_opt (fun (k, _) -> k = "masc") card'.extensions in
  match masc_ext with
  | Some (_, `String "overwritten") -> ()
  | Some (_, _) -> fail "masc extension should be overwritten"
  | None -> fail "masc extension should exist"

let test_agent_card_show () =
  let card = Agent_card.generate_default () in
  let s = Agent_card.show_agent_card card in
  check bool "non-empty" true (String.length s > 0)

let test_agent_card_eq () =
  let card1 = Agent_card.generate_default () in
  let card2 = Agent_card.generate_default () in
  (* Note: timestamps might differ, so we can't compare directly *)
  check bool "equal name" true (card1.name = card2.name);
  check bool "equal version" true (card1.version = card2.version)

(* ============================================================
   MASC Skills Tests
   ============================================================ *)

let test_masc_skills_not_empty () =
  check bool "has skills" true (List.length Agent_card.masc_skills > 0)

let test_masc_skills_have_ids () =
  let all_have_ids = List.for_all (fun (s : Agent_card.skill) ->
    String.length s.id > 0
  ) Agent_card.masc_skills in
  check bool "all have ids" true all_have_ids

let test_masc_skills_unique_ids () =
  let ids = List.map (fun (s : Agent_card.skill) -> s.id) Agent_card.masc_skills in
  let unique_ids = List.sort_uniq String.compare ids in
  check int "unique ids" (List.length ids) (List.length unique_ids)

let test_masc_skills_have_names () =
  let all_have_names = List.for_all (fun (s : Agent_card.skill) ->
    String.length s.name > 0
  ) Agent_card.masc_skills in
  check bool "all have names" true all_have_names

let test_masc_skills_expected_count () =
  (* Should have 8 MASC skills defined *)
  check int "skill count" 8 (List.length Agent_card.masc_skills)

(* ============================================================
   Now ISO8601 Tests
   ============================================================ *)

let test_now_iso8601_format () =
  let ts = Agent_card.now_iso8601 () in
  (* Should match pattern YYYY-MM-DDTHH:MM:SSZ *)
  let regex = Str.regexp "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z" in
  check bool "valid ISO8601 format" true (Str.string_match regex ts 0)

let test_now_iso8601_length () =
  let ts = Agent_card.now_iso8601 () in
  check int "timestamp length" 20 (String.length ts)

(* ============================================================
   Edge Cases
   ============================================================ *)

let test_empty_capabilities () =
  let card = Agent_card.generate_default () in
  let minimal_card = { card with capabilities = [] } in
  let json = Agent_card.to_json minimal_card in
  match Agent_card.from_json json with
  | Ok card' -> check (list string) "empty capabilities" [] card'.capabilities
  | Error e -> fail ("Empty capabilities roundtrip failed: " ^ e)

let test_empty_skills () =
  let card = Agent_card.generate_default () in
  let minimal_card = { card with skills = [] } in
  let json = Agent_card.to_json minimal_card in
  match Agent_card.from_json json with
  | Ok card' -> check int "empty skills" 0 (List.length card'.skills)
  | Error e -> fail ("Empty skills roundtrip failed: " ^ e)

let test_empty_bindings () =
  let card = Agent_card.generate_default () in
  let minimal_card = { card with bindings = [] } in
  let json = Agent_card.to_json minimal_card in
  match Agent_card.from_json json with
  | Ok card' -> check int "empty bindings" 0 (List.length card'.bindings)
  | Error e -> fail ("Empty bindings roundtrip failed: " ^ e)

let test_empty_extensions () =
  let card = Agent_card.generate_default () in
  let minimal_card = { card with extensions = [] } in
  let json = Agent_card.to_json minimal_card in
  match Agent_card.from_json json with
  | Ok card' -> check int "empty extensions" 0 (List.length card'.extensions)
  | Error e -> fail ("Empty extensions roundtrip failed: " ^ e)

let test_signature_none () =
  let card = Agent_card.generate_default () in
  check (option string) "no signature by default" None card.signature

let test_signature_set () =
  let card = Agent_card.generate_default () in
  let signed_card = { card with signature = Some "sig123" } in
  let json = Agent_card.to_json signed_card in
  match Agent_card.from_json json with
  | Ok card' -> check (option string) "signature preserved" (Some "sig123") card'.signature
  | Error e -> fail ("Signed card roundtrip failed: " ^ e)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Agent Card Coverage" [
    "provider", [
      test_case "json roundtrip" `Quick test_provider_json_roundtrip;
      test_case "without url" `Quick test_provider_without_url;
      test_case "show" `Quick test_provider_show;
      test_case "eq" `Quick test_provider_eq;
    ];
    "skill", [
      test_case "json roundtrip" `Quick test_skill_json_roundtrip;
      test_case "without description" `Quick test_skill_without_description;
      test_case "show" `Quick test_skill_show;
      test_case "eq" `Quick test_skill_eq;
    ];
    "binding", [
      test_case "json roundtrip" `Quick test_binding_json_roundtrip;
      test_case "grpc binding" `Quick test_binding_grpc;
      test_case "show" `Quick test_binding_show;
      test_case "eq" `Quick test_binding_eq;
    ];
    "security_scheme", [
      test_case "bearer" `Quick test_security_scheme_bearer;
      test_case "apikey" `Quick test_security_scheme_apikey;
      test_case "none" `Quick test_security_scheme_none;
      test_case "show" `Quick test_security_scheme_show;
      test_case "eq" `Quick test_security_scheme_eq;
    ];
    "agent_card", [
      test_case "generate default" `Quick test_generate_default;
      test_case "custom port" `Quick test_generate_with_custom_port;
      test_case "custom host" `Quick test_generate_with_custom_host;
      test_case "to_json" `Quick test_agent_card_to_json;
      test_case "json roundtrip" `Quick test_agent_card_json_roundtrip;
      test_case "from_json invalid" `Quick test_from_json_invalid;
      test_case "from_json missing fields" `Quick test_from_json_missing_fields;
      test_case "with_bindings" `Quick test_with_bindings;
      test_case "with_extension" `Quick test_with_extension;
      test_case "with_extension overwrite" `Quick test_with_extension_overwrite;
      test_case "show" `Quick test_agent_card_show;
      test_case "eq" `Quick test_agent_card_eq;
    ];
    "masc_skills", [
      test_case "not empty" `Quick test_masc_skills_not_empty;
      test_case "have ids" `Quick test_masc_skills_have_ids;
      test_case "unique ids" `Quick test_masc_skills_unique_ids;
      test_case "have names" `Quick test_masc_skills_have_names;
      test_case "expected count" `Quick test_masc_skills_expected_count;
    ];
    "now_iso8601", [
      test_case "format" `Quick test_now_iso8601_format;
      test_case "length" `Quick test_now_iso8601_length;
    ];
    "edge_cases", [
      test_case "empty capabilities" `Quick test_empty_capabilities;
      test_case "empty skills" `Quick test_empty_skills;
      test_case "empty bindings" `Quick test_empty_bindings;
      test_case "empty extensions" `Quick test_empty_extensions;
      test_case "signature none" `Quick test_signature_none;
      test_case "signature set" `Quick test_signature_set;
    ];
  ]
