open Alcotest

let test_accepts_sse_header () =
  let open Masc_mcp.Mcp_protocol.Http_negotiation in
  check bool "missing accept" false (accepts_sse_header None);
  check bool "application/json" false (accepts_sse_header (Some "application/json"));
  check bool "wildcard only" false (accepts_sse_header (Some "*/*"));
  check bool "event-stream exact" true (accepts_sse_header (Some "text/event-stream"));
  check bool "event-stream with params" true (accepts_sse_header (Some "text/event-stream; charset=utf-8"));
  check bool "event-stream not first" true (accepts_sse_header (Some "application/json, text/event-stream"));
  check bool "event-stream q=0" false (accepts_sse_header (Some "text/event-stream;q=0"));
  check bool "case-insensitive" true (accepts_sse_header (Some "Text/Event-Stream"));
  ()

let test_accepts_streamable_mcp () =
  let open Masc_mcp.Mcp_protocol.Http_negotiation in
  check bool "missing accept" false (accepts_streamable_mcp None);
  check bool "json only" false (accepts_streamable_mcp (Some "application/json"));
  check bool "sse only" false (accepts_streamable_mcp (Some "text/event-stream"));
  check bool "wildcard only" false (accepts_streamable_mcp (Some "*/*"));
  check bool "json + sse" true (accepts_streamable_mcp (Some "application/json, text/event-stream"));
  check bool "json + sse reversed" true (accepts_streamable_mcp (Some "text/event-stream, application/json"));
  check bool "sse q=0" false (accepts_streamable_mcp (Some "application/json, text/event-stream;q=0"));
  check bool "case-insensitive" true (accepts_streamable_mcp (Some "Application/Json, Text/Event-Stream"));
  ()

let () =
  run "http_negotiation"
    [
      ("accepts_sse_header", [test_case "parses Accept" `Quick test_accepts_sse_header]);
      ("accepts_streamable_mcp", [test_case "requires json+sse" `Quick test_accepts_streamable_mcp]);
    ]
