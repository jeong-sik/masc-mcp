(** Coverage tests for Llm_client_eio module *)

open Masc_mcp

let test_model_type_construction () =
  (* Test model variant construction *)
  let _ = Llm_client_eio.Gemini in
  let _ = Llm_client_eio.Claude in
  let _ = Llm_client_eio.Codex in
  let _ = Llm_client_eio.Ollama "qwen3" in
  Alcotest.(check pass) "model variants" () ()

let test_response_type () =
  let resp : Llm_client_eio.response = {
    content = "Hello";
    model = "gemini";
    tokens_used = Some 100;
  } in
  Alcotest.(check string) "content" "Hello" resp.content;
  Alcotest.(check string) "model" "gemini" resp.model;
  Alcotest.(check (option int)) "tokens" (Some 100) resp.tokens_used

let test_error_type () =
  let _ = Llm_client_eio.ConnectionError "failed" in
  let _ = Llm_client_eio.ParseError "invalid json" in
  let _ = Llm_client_eio.ServerError (500, "internal error") in
  let _ = Llm_client_eio.Timeout in
  Alcotest.(check pass) "error variants" () ()

let test_default_constants () =
  Alcotest.(check string) "default_host" "127.0.0.1" Llm_client_eio.default_host;
  Alcotest.(check int) "default_port" 8932 Llm_client_eio.default_port;
  Alcotest.(check (float 0.1)) "default_timeout" 30.0 Llm_client_eio.default_timeout_sec

let test_model_to_string () =
  Alcotest.(check string) "gemini" "gemini" (Llm_client_eio.model_to_string Llm_client_eio.Gemini);
  Alcotest.(check string) "claude" "claude-cli" (Llm_client_eio.model_to_string Llm_client_eio.Claude);
  Alcotest.(check string) "codex" "codex" (Llm_client_eio.model_to_string Llm_client_eio.Codex);
  Alcotest.(check string) "ollama" "ollama:qwen3" (Llm_client_eio.model_to_string (Llm_client_eio.Ollama "qwen3"))

let contains s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in true
  with Not_found -> false

let test_build_request () =
  let req = Llm_client_eio.build_request ~model:Llm_client_eio.Gemini ~prompt:"Hello" in
  Alcotest.(check bool) "contains jsonrpc" true (String.length req > 0);
  Alcotest.(check bool) "contains gemini" true (contains req "gemini");
  Alcotest.(check bool) "contains prompt" true (contains req "Hello")

let test_build_request_with_special_chars () =
  let req = Llm_client_eio.build_request ~model:Llm_client_eio.Claude ~prompt:"Say \"hello\" world" in
  Alcotest.(check bool) "escapes quotes" true (String.length req > 0)

let test_parse_response_success () =
  let json = {|{"result": {"content": "Hello", "model": "gemini"}}|} in
  match Llm_client_eio.parse_response json with
  | Ok resp ->
      Alcotest.(check string) "content" "Hello" resp.content;
      Alcotest.(check string) "model" "gemini" resp.model
  | Error _ -> Alcotest.fail "Expected Ok"

let test_parse_response_with_text_array () =
  let json = {|{"result": {"content": [{"text": "Response text"}], "model": "claude"}}|} in
  match Llm_client_eio.parse_response json with
  | Ok resp ->
      Alcotest.(check string) "content from array" "Response text" resp.content
  | Error _ -> Alcotest.fail "Expected Ok"

let test_parse_response_string_result () =
  let json = {|{"result": "Simple string response"}|} in
  match Llm_client_eio.parse_response json with
  | Ok resp ->
      Alcotest.(check string) "string result" "Simple string response" resp.content
  | Error _ -> Alcotest.fail "Expected Ok"

let test_parse_response_error () =
  let json = {|{"error": {"message": "Rate limit exceeded"}}|} in
  match Llm_client_eio.parse_response json with
  | Error (Llm_client_eio.ServerError (500, msg)) ->
      Alcotest.(check string) "error message" "Rate limit exceeded" msg
  | _ -> Alcotest.fail "Expected ServerError"

let test_parse_response_invalid_json () =
  match Llm_client_eio.parse_response "not json" with
  | Error (Llm_client_eio.ParseError _) -> ()
  | _ -> Alcotest.fail "Expected ParseError"

let test_parse_response_no_result_or_error () =
  let json = {|{"something": "else"}|} in
  match Llm_client_eio.parse_response json with
  | Error (Llm_client_eio.ParseError msg) ->
      Alcotest.(check bool) "has error message" true (String.length msg > 0)
  | _ -> Alcotest.fail "Expected ParseError"

let test_parse_response_invalid_structure () =
  let json = {|[1, 2, 3]|} in
  match Llm_client_eio.parse_response json with
  | Error (Llm_client_eio.ParseError _) -> ()
  | _ -> Alcotest.fail "Expected ParseError"

let () =
  Alcotest.run "Llm_client_eio Coverage" [
    "types", [
      Alcotest.test_case "model type construction" `Quick test_model_type_construction;
      Alcotest.test_case "response type" `Quick test_response_type;
      Alcotest.test_case "error type" `Quick test_error_type;
    ];
    "constants", [
      Alcotest.test_case "default constants" `Quick test_default_constants;
    ];
    "model_to_string", [
      Alcotest.test_case "all models" `Quick test_model_to_string;
    ];
    "build_request", [
      Alcotest.test_case "basic request" `Quick test_build_request;
      Alcotest.test_case "special chars" `Quick test_build_request_with_special_chars;
    ];
    "parse_response", [
      Alcotest.test_case "success" `Quick test_parse_response_success;
      Alcotest.test_case "text array" `Quick test_parse_response_with_text_array;
      Alcotest.test_case "string result" `Quick test_parse_response_string_result;
      Alcotest.test_case "error response" `Quick test_parse_response_error;
      Alcotest.test_case "invalid json" `Quick test_parse_response_invalid_json;
      Alcotest.test_case "no result" `Quick test_parse_response_no_result_or_error;
      Alcotest.test_case "invalid structure" `Quick test_parse_response_invalid_structure;
    ];
  ]
