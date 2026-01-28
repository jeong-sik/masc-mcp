(** LLM Client Coverage Tests

    Tests for pure functions in llm_client_eio.ml:
    - model_to_string
    - build_request
    - parse_response
*)

open Alcotest

module Llm = Masc_mcp.Llm_client_eio

(* ============================================================
   model_to_string Tests
   ============================================================ *)

let test_model_to_string_gemini () =
  check string "gemini" "gemini" (Llm.model_to_string Llm.Gemini)

let test_model_to_string_claude () =
  check string "claude" "claude-cli" (Llm.model_to_string Llm.Claude)

let test_model_to_string_codex () =
  check string "codex" "codex" (Llm.model_to_string Llm.Codex)

let test_model_to_string_ollama () =
  check string "ollama" "ollama:llama3" (Llm.model_to_string (Llm.Ollama "llama3"))

let test_model_to_string_ollama_custom () =
  check string "ollama custom" "ollama:qwen2.5:7b" (Llm.model_to_string (Llm.Ollama "qwen2.5:7b"))

(* ============================================================
   build_request Tests
   ============================================================ *)

let test_build_request_gemini () =
  let req = Llm.build_request ~model:Llm.Gemini ~prompt:"Hello" in
  check bool "has jsonrpc" true (
    try let _ = Str.search_forward (Str.regexp "jsonrpc") req 0 in true
    with Not_found -> false);
  check bool "has gemini" true (
    try let _ = Str.search_forward (Str.regexp "\"gemini\"") req 0 in true
    with Not_found -> false);
  check bool "has prompt" true (
    try let _ = Str.search_forward (Str.regexp "Hello") req 0 in true
    with Not_found -> false)

let test_build_request_claude () =
  let req = Llm.build_request ~model:Llm.Claude ~prompt:"Test" in
  check bool "has claude-cli" true (
    try let _ = Str.search_forward (Str.regexp "claude-cli") req 0 in true
    with Not_found -> false)

let test_build_request_codex () =
  let req = Llm.build_request ~model:Llm.Codex ~prompt:"Code" in
  check bool "has codex" true (
    try let _ = Str.search_forward (Str.regexp "\"codex\"") req 0 in true
    with Not_found -> false)

let test_build_request_ollama () =
  let req = Llm.build_request ~model:(Llm.Ollama "mistral") ~prompt:"Local" in
  check bool "has ollama:mistral" true (
    try let _ = Str.search_forward (Str.regexp "ollama:mistral") req 0 in true
    with Not_found -> false)

let test_build_request_special_chars () =
  let req = Llm.build_request ~model:Llm.Gemini ~prompt:"Hello \"world\" \n test" in
  check bool "valid json" true (
    try let _ = Yojson.Safe.from_string req in true
    with _ -> false)

let test_build_request_empty_prompt () =
  let req = Llm.build_request ~model:Llm.Gemini ~prompt:"" in
  check bool "valid json" true (
    try let _ = Yojson.Safe.from_string req in true
    with _ -> false)

let test_build_request_has_tools_call () =
  let req = Llm.build_request ~model:Llm.Gemini ~prompt:"test" in
  check bool "has tools/call" true (
    try let _ = Str.search_forward (Str.regexp "tools/call") req 0 in true
    with Not_found -> false)

let test_build_request_has_response_format () =
  let req = Llm.build_request ~model:Llm.Gemini ~prompt:"test" in
  check bool "has response_format" true (
    try let _ = Str.search_forward (Str.regexp "response_format") req 0 in true
    with Not_found -> false)

(* ============================================================
   parse_response Tests
   ============================================================ *)

let test_parse_response_success () =
  let json = {|{"result": {"content": "Hello!", "model": "gemini-1.5"}}|} in
  match Llm.parse_response json with
  | Ok resp ->
      check string "content" "Hello!" resp.content;
      check string "model" "gemini-1.5" resp.model
  | Error _ -> fail "expected success"

let test_parse_response_string_result () =
  let json = {|{"result": "Simple response"}|} in
  match Llm.parse_response json with
  | Ok resp -> check string "content" "Simple response" resp.content
  | Error _ -> fail "expected success"

let test_parse_response_text_array () =
  let json = {|{"result": {"content": [{"text": "Array text"}]}}|} in
  match Llm.parse_response json with
  | Ok resp -> check string "content" "Array text" resp.content
  | Error _ -> fail "expected success"

let test_parse_response_error () =
  let json = {|{"error": {"message": "Rate limit exceeded"}}|} in
  match Llm.parse_response json with
  | Error (Llm.ServerError (500, msg)) ->
      check bool "has rate limit" true (
        try let _ = Str.search_forward (Str.regexp "Rate limit") msg 0 in true
        with Not_found -> false)
  | _ -> fail "expected ServerError"

let test_parse_response_invalid_json () =
  let json = "not valid json {{{" in
  match Llm.parse_response json with
  | Error (Llm.ParseError _) -> ()
  | _ -> fail "expected ParseError"

let test_parse_response_no_result () =
  let json = {|{"something": "else"}|} in
  match Llm.parse_response json with
  | Error (Llm.ParseError _) -> ()
  | _ -> fail "expected ParseError"

let test_parse_response_empty_content () =
  let json = {|{"result": {"content": ""}}|} in
  match Llm.parse_response json with
  | Ok resp -> check string "empty content" "" resp.content
  | Error _ -> fail "expected success"

let test_parse_response_missing_model () =
  let json = {|{"result": {"content": "test"}}|} in
  match Llm.parse_response json with
  | Ok resp -> check string "unknown model" "unknown" resp.model
  | Error _ -> fail "expected success"

let test_parse_response_tokens_none () =
  let json = {|{"result": {"content": "test"}}|} in
  match Llm.parse_response json with
  | Ok resp -> check (option int) "no tokens" None resp.tokens_used
  | Error _ -> fail "expected success"

(* ============================================================
   Type Tests
   ============================================================ *)

let test_response_type () =
  let resp : Llm.response = { content = "test"; model = "gemini"; tokens_used = Some 100 } in
  check string "content" "test" resp.content;
  check string "model" "gemini" resp.model;
  check (option int) "tokens" (Some 100) resp.tokens_used

let test_error_types () =
  let _ : Llm.error = Llm.ConnectionError "test" in
  let _ : Llm.error = Llm.ParseError "test" in
  let _ : Llm.error = Llm.ServerError (500, "test") in
  let _ : Llm.error = Llm.Timeout in
  check bool "error types exist" true true

let test_model_types () =
  let _ : Llm.model = Llm.Gemini in
  let _ : Llm.model = Llm.Claude in
  let _ : Llm.model = Llm.Codex in
  let _ : Llm.model = Llm.Ollama "test" in
  check bool "model types exist" true true

(* ============================================================
   Config Tests
   ============================================================ *)

let test_default_host () =
  check string "default host" "127.0.0.1" Llm.default_host

let test_default_port () =
  check int "default port" 8932 Llm.default_port

let test_default_timeout () =
  check bool "timeout positive" true (Llm.default_timeout_sec > 0.0)

(* ============================================================
   Mock HTTP Tests (call_with_http)
   ============================================================ *)

(** Mock HTTP that returns success response *)
let mock_http_success : Llm.http_post_fn = fun ~host:_ ~port:_ ~path:_ ~body:_ ->
  Ok {|{"result": {"content": "Mocked response", "model": "mock-model"}}|}

(** Mock HTTP that returns error response *)
let mock_http_error : Llm.http_post_fn = fun ~host:_ ~port:_ ~path:_ ~body:_ ->
  Ok {|{"error": {"message": "Mock error"}}|}

(** Mock HTTP that returns connection error *)
let mock_http_connection_error : Llm.http_post_fn = fun ~host:_ ~port:_ ~path:_ ~body:_ ->
  Error (Llm.ConnectionError "Mock connection failed")

(** Mock HTTP that captures request for verification *)
let mock_http_capture captured : Llm.http_post_fn = fun ~host ~port ~path ~body ->
  captured := Some (host, port, path, body);
  Ok {|{"result": {"content": "captured"}}|}

let test_call_with_http_success () =
  match Llm.call_with_http ~http_post:mock_http_success ~prompt:"Hello" () with
  | Ok resp ->
      check string "content" "Mocked response" resp.content;
      check string "model" "mock-model" resp.model
  | Error _ -> fail "expected success"

let test_call_with_http_error () =
  match Llm.call_with_http ~http_post:mock_http_error ~prompt:"Hello" () with
  | Error (Llm.ServerError (500, msg)) ->
      check bool "has mock error" true (
        try let _ = Str.search_forward (Str.regexp "Mock error") msg 0 in true
        with Not_found -> false)
  | _ -> fail "expected ServerError"

let test_call_with_http_connection_error () =
  match Llm.call_with_http ~http_post:mock_http_connection_error ~prompt:"Hello" () with
  | Error (Llm.ConnectionError msg) ->
      check bool "has connection failed" true (
        try let _ = Str.search_forward (Str.regexp "connection failed") msg 0 in true
        with Not_found -> false)
  | _ -> fail "expected ConnectionError"

let test_call_with_http_uses_correct_path () =
  let captured = ref None in
  let _ = Llm.call_with_http ~http_post:(mock_http_capture captured) ~prompt:"test" () in
  match !captured with
  | Some (_, _, path, _) -> check string "path" "/mcp" path
  | None -> fail "request not captured"

let test_call_with_http_uses_correct_host () =
  let captured = ref None in
  let _ = Llm.call_with_http ~http_post:(mock_http_capture captured) ~host:"custom.host" ~prompt:"test" () in
  match !captured with
  | Some (host, _, _, _) -> check string "host" "custom.host" host
  | None -> fail "request not captured"

let test_call_with_http_uses_correct_port () =
  let captured = ref None in
  let _ = Llm.call_with_http ~http_post:(mock_http_capture captured) ~port:9999 ~prompt:"test" () in
  match !captured with
  | Some (_, port, _, _) -> check int "port" 9999 port
  | None -> fail "request not captured"

let test_call_with_http_body_has_prompt () =
  let captured = ref None in
  let _ = Llm.call_with_http ~http_post:(mock_http_capture captured) ~prompt:"MyUniquePrompt" () in
  match !captured with
  | Some (_, _, _, body) ->
      check bool "body has prompt" true (
        try let _ = Str.search_forward (Str.regexp "MyUniquePrompt") body 0 in true
        with Not_found -> false)
  | None -> fail "request not captured"

let test_call_with_http_body_has_model () =
  let captured = ref None in
  let _ = Llm.call_with_http ~http_post:(mock_http_capture captured) ~model:Llm.Claude ~prompt:"test" () in
  match !captured with
  | Some (_, _, _, body) ->
      check bool "body has claude-cli" true (
        try let _ = Str.search_forward (Str.regexp "claude-cli") body 0 in true
        with Not_found -> false)
  | None -> fail "request not captured"

let test_call_with_http_default_model () =
  let captured = ref None in
  let _ = Llm.call_with_http ~http_post:(mock_http_capture captured) ~prompt:"test" () in
  match !captured with
  | Some (_, _, _, body) ->
      check bool "default is gemini" true (
        try let _ = Str.search_forward (Str.regexp "\"gemini\"") body 0 in true
        with Not_found -> false)
  | None -> fail "request not captured"

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "LLM Client Coverage" [
    "model_to_string", [
      test_case "gemini" `Quick test_model_to_string_gemini;
      test_case "claude" `Quick test_model_to_string_claude;
      test_case "codex" `Quick test_model_to_string_codex;
      test_case "ollama" `Quick test_model_to_string_ollama;
      test_case "ollama custom" `Quick test_model_to_string_ollama_custom;
    ];
    "build_request", [
      test_case "gemini" `Quick test_build_request_gemini;
      test_case "claude" `Quick test_build_request_claude;
      test_case "codex" `Quick test_build_request_codex;
      test_case "ollama" `Quick test_build_request_ollama;
      test_case "special chars" `Quick test_build_request_special_chars;
      test_case "empty prompt" `Quick test_build_request_empty_prompt;
      test_case "has tools/call" `Quick test_build_request_has_tools_call;
      test_case "has response_format" `Quick test_build_request_has_response_format;
    ];
    "parse_response", [
      test_case "success" `Quick test_parse_response_success;
      test_case "string result" `Quick test_parse_response_string_result;
      test_case "text array" `Quick test_parse_response_text_array;
      test_case "error" `Quick test_parse_response_error;
      test_case "invalid json" `Quick test_parse_response_invalid_json;
      test_case "no result" `Quick test_parse_response_no_result;
      test_case "empty content" `Quick test_parse_response_empty_content;
      test_case "missing model" `Quick test_parse_response_missing_model;
      test_case "tokens none" `Quick test_parse_response_tokens_none;
    ];
    "types", [
      test_case "response" `Quick test_response_type;
      test_case "error" `Quick test_error_types;
      test_case "model" `Quick test_model_types;
    ];
    "config", [
      test_case "default host" `Quick test_default_host;
      test_case "default port" `Quick test_default_port;
      test_case "default timeout" `Quick test_default_timeout;
    ];
    "call_with_http", [
      test_case "success" `Quick test_call_with_http_success;
      test_case "error" `Quick test_call_with_http_error;
      test_case "connection error" `Quick test_call_with_http_connection_error;
      test_case "correct path" `Quick test_call_with_http_uses_correct_path;
      test_case "correct host" `Quick test_call_with_http_uses_correct_host;
      test_case "correct port" `Quick test_call_with_http_uses_correct_port;
      test_case "body has prompt" `Quick test_call_with_http_body_has_prompt;
      test_case "body has model" `Quick test_call_with_http_body_has_model;
      test_case "default model" `Quick test_call_with_http_default_model;
    ];
  ]
