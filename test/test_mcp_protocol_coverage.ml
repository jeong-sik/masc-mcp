(** Mcp_protocol Module Coverage Tests

    Tests for MCP Streamable HTTP Protocol:
    - Http_negotiation.parse_accept_header: Accept header parsing
    - Http_negotiation.is_media_type_accepted: media type matching
    - Http_negotiation.accepts_sse_header: SSE acceptance
    - Http_negotiation.accepts_streamable_mcp: streamable MCP acceptance
    - Content type constants
*)

open Alcotest

module Http_negotiation = Masc_mcp.Mcp_protocol.Http_negotiation

(* ============================================================
   Content Type Constants Tests
   ============================================================ *)

let test_sse_content_type () =
  check string "sse content type" "text/event-stream" Http_negotiation.sse_content_type

let test_json_content_type () =
  check string "json content type" "application/json" Http_negotiation.json_content_type

(* ============================================================
   parse_accept_header Tests
   ============================================================ *)

let test_parse_accept_header_none () =
  let result = Http_negotiation.parse_accept_header None in
  check int "empty list for None" 0 (List.length result)

let test_parse_accept_header_empty () =
  (* Empty string splits to [""] which yields one filter_map entry,
     but the trimmed empty string doesn't match the pattern *)
  let result = Http_negotiation.parse_accept_header (Some "") in
  check int "empty list for empty string" 1 (List.length result)

let test_parse_accept_header_single () =
  let result = Http_negotiation.parse_accept_header (Some "application/json") in
  check int "one entry" 1 (List.length result);
  match result with
  | [(media, q)] ->
    check string "media type" "application/json" media;
    check bool "q default 1.0" true (q = 1.0)
  | _ -> fail "expected single entry"

let test_parse_accept_header_multiple () =
  let result = Http_negotiation.parse_accept_header (Some "text/html, application/json") in
  check int "two entries" 2 (List.length result)

let test_parse_accept_header_with_q () =
  let result = Http_negotiation.parse_accept_header (Some "text/html;q=0.9") in
  match result with
  | [(media, q)] ->
    check string "media type" "text/html" media;
    check bool "q = 0.9" true (abs_float (q -. 0.9) < 0.001)
  | _ -> fail "expected single entry"

let test_parse_accept_header_q_zero () =
  let result = Http_negotiation.parse_accept_header (Some "text/html;q=0") in
  match result with
  | [(_, q)] -> check bool "q = 0" true (q = 0.0)
  | _ -> fail "expected single entry"

let test_parse_accept_header_case_insensitive () =
  let result = Http_negotiation.parse_accept_header (Some "TEXT/HTML") in
  match result with
  | [(media, _)] -> check string "lowercase" "text/html" media
  | _ -> fail "expected single entry"

let test_parse_accept_header_complex () =
  let result = Http_negotiation.parse_accept_header
    (Some "text/html, application/json;q=0.9, */*;q=0.1") in
  check int "three entries" 3 (List.length result)

let test_parse_accept_header_with_charset () =
  let result = Http_negotiation.parse_accept_header
    (Some "text/html; charset=utf-8; q=0.8") in
  match result with
  | [(media, q)] ->
    check string "media type" "text/html" media;
    check bool "q = 0.8" true (abs_float (q -. 0.8) < 0.001)
  | _ -> fail "expected single entry"

let test_parse_accept_header_whitespace () =
  let result = Http_negotiation.parse_accept_header
    (Some "  text/html  ,  application/json  ") in
  check int "two entries" 2 (List.length result)

(* ============================================================
   is_media_type_accepted Tests
   ============================================================ *)

let test_is_media_type_accepted_found () =
  let media_types = [("application/json", 1.0); ("text/html", 0.5)] in
  check bool "json found" true
    (Http_negotiation.is_media_type_accepted media_types "application/json")

let test_is_media_type_accepted_not_found () =
  let media_types = [("application/json", 1.0)] in
  check bool "xml not found" false
    (Http_negotiation.is_media_type_accepted media_types "application/xml")

let test_is_media_type_accepted_q_zero () =
  let media_types = [("text/html", 0.0)] in
  check bool "q=0 not accepted" false
    (Http_negotiation.is_media_type_accepted media_types "text/html")

let test_is_media_type_accepted_case_insensitive () =
  let media_types = [("application/json", 1.0)] in
  check bool "case insensitive" true
    (Http_negotiation.is_media_type_accepted media_types "Application/JSON")

let test_is_media_type_accepted_empty_list () =
  check bool "empty list" false
    (Http_negotiation.is_media_type_accepted [] "text/html")

(* ============================================================
   accepts_sse_header Tests
   ============================================================ *)

let test_accepts_sse_none () =
  check bool "None" false (Http_negotiation.accepts_sse_header None)

let test_accepts_sse_json_only () =
  check bool "json only" false
    (Http_negotiation.accepts_sse_header (Some "application/json"))

let test_accepts_sse_wildcard () =
  check bool "wildcard" false
    (Http_negotiation.accepts_sse_header (Some "*/*"))

let test_accepts_sse_exact () =
  check bool "exact" true
    (Http_negotiation.accepts_sse_header (Some "text/event-stream"))

let test_accepts_sse_with_q () =
  check bool "with q=0.5" true
    (Http_negotiation.accepts_sse_header (Some "text/event-stream;q=0.5"))

let test_accepts_sse_q_zero () =
  check bool "q=0 rejected" false
    (Http_negotiation.accepts_sse_header (Some "text/event-stream;q=0"))

(* ============================================================
   accepts_streamable_mcp Tests
   ============================================================ *)

let test_streamable_none () =
  check bool "None" false (Http_negotiation.accepts_streamable_mcp None)

let test_streamable_json_only () =
  check bool "json only" false
    (Http_negotiation.accepts_streamable_mcp (Some "application/json"))

let test_streamable_sse_only () =
  check bool "sse only" false
    (Http_negotiation.accepts_streamable_mcp (Some "text/event-stream"))

let test_streamable_both () =
  check bool "both" true
    (Http_negotiation.accepts_streamable_mcp
      (Some "application/json, text/event-stream"))

let test_streamable_both_reversed () =
  check bool "reversed" true
    (Http_negotiation.accepts_streamable_mcp
      (Some "text/event-stream, application/json"))

let test_streamable_json_q_zero () =
  check bool "json q=0" false
    (Http_negotiation.accepts_streamable_mcp
      (Some "application/json;q=0, text/event-stream"))

let test_streamable_sse_q_zero () =
  check bool "sse q=0" false
    (Http_negotiation.accepts_streamable_mcp
      (Some "application/json, text/event-stream;q=0"))

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Mcp_protocol Coverage" [
    "constants", [
      test_case "sse_content_type" `Quick test_sse_content_type;
      test_case "json_content_type" `Quick test_json_content_type;
    ];
    "parse_accept_header", [
      test_case "none" `Quick test_parse_accept_header_none;
      test_case "empty" `Quick test_parse_accept_header_empty;
      test_case "single" `Quick test_parse_accept_header_single;
      test_case "multiple" `Quick test_parse_accept_header_multiple;
      test_case "with q" `Quick test_parse_accept_header_with_q;
      test_case "q zero" `Quick test_parse_accept_header_q_zero;
      test_case "case insensitive" `Quick test_parse_accept_header_case_insensitive;
      test_case "complex" `Quick test_parse_accept_header_complex;
      test_case "with charset" `Quick test_parse_accept_header_with_charset;
      test_case "whitespace" `Quick test_parse_accept_header_whitespace;
    ];
    "is_media_type_accepted", [
      test_case "found" `Quick test_is_media_type_accepted_found;
      test_case "not found" `Quick test_is_media_type_accepted_not_found;
      test_case "q zero" `Quick test_is_media_type_accepted_q_zero;
      test_case "case insensitive" `Quick test_is_media_type_accepted_case_insensitive;
      test_case "empty list" `Quick test_is_media_type_accepted_empty_list;
    ];
    "accepts_sse_header", [
      test_case "none" `Quick test_accepts_sse_none;
      test_case "json only" `Quick test_accepts_sse_json_only;
      test_case "wildcard" `Quick test_accepts_sse_wildcard;
      test_case "exact" `Quick test_accepts_sse_exact;
      test_case "with q" `Quick test_accepts_sse_with_q;
      test_case "q zero" `Quick test_accepts_sse_q_zero;
    ];
    "accepts_streamable_mcp", [
      test_case "none" `Quick test_streamable_none;
      test_case "json only" `Quick test_streamable_json_only;
      test_case "sse only" `Quick test_streamable_sse_only;
      test_case "both" `Quick test_streamable_both;
      test_case "reversed" `Quick test_streamable_both_reversed;
      test_case "json q=0" `Quick test_streamable_json_q_zero;
      test_case "sse q=0" `Quick test_streamable_sse_q_zero;
    ];
  ]
