(** LLM Client for Eio - HTTP client for llm-mcp server

    Provides simple interface to call LLM models via llm-mcp.
    Used for Walph intent classification and other LLM tasks.

    @see https://github.com/jeong-sik/llm-mcp for server details
*)

(* Eio modules are referenced with full paths for clarity *)

(** {1 Types} *)

type model =
  | Gemini      (** Fast, good for classification *)
  | Claude      (** Balanced, good for reasoning *)
  | Codex       (** Code-focused *)
  | Ollama of string  (** Local model *)

type response = {
  content: string;
  model: string;
  tokens_used: int option;
}

type error =
  | ConnectionError of string
  | ParseError of string
  | ServerError of int * string
  | Timeout

(** {1 Configuration} *)

let default_host = "127.0.0.1"
let default_port = 8932
let default_timeout_sec = 30.0

(** {1 Internal Helpers} *)

let model_to_string = function
  | Gemini -> "gemini"
  | Claude -> "claude-cli"
  | Codex -> "codex"
  | Ollama name -> Printf.sprintf "ollama:%s" name

(** Build MCP JSON-RPC request for LLM call *)
let build_request ~model ~prompt =
  let model_str = model_to_string model in
  Printf.sprintf {|{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "%s",
    "arguments": {
      "prompt": %s,
      "response_format": "compact"
    }
  }
}|} model_str (Yojson.Safe.to_string (`String prompt))

(** Parse LLM response from MCP JSON-RPC response *)
let parse_response json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "result" fields with
         | Some (`Assoc result_fields) ->
             let content = match List.assoc_opt "content" result_fields with
               | Some (`String s) -> s
               | Some (`List [`Assoc text_fields]) ->
                   (match List.assoc_opt "text" text_fields with
                    | Some (`String s) -> s
                    | _ -> "")
               | _ -> ""
             in
             let model = match List.assoc_opt "model" result_fields with
               | Some (`String s) -> s
               | _ -> "unknown"
             in
             Ok { content; model; tokens_used = None }
         | Some (`String s) -> Ok { content = s; model = "unknown"; tokens_used = None }
         | _ ->
             (match List.assoc_opt "error" fields with
              | Some (`Assoc err) ->
                  let msg = match List.assoc_opt "message" err with
                    | Some (`String s) -> s
                    | _ -> "Unknown error"
                  in
                  Error (ServerError (500, msg))
              | _ -> Error (ParseError "No result or error in response")))
    | _ -> Error (ParseError "Invalid JSON structure")
  with
  | Yojson.Json_error msg -> Error (ParseError msg)
  | _ -> Error (ParseError "Unknown parse error")

(** {1 Public API} *)

(** Call LLM model with prompt (Eio-native, non-blocking)

    @param net Eio network capability
    @param model LLM model to use (default: Gemini for speed)
    @param host llm-mcp server host (default: 127.0.0.1)
    @param port llm-mcp server port (default: 8932)
    @param prompt The prompt to send
    @return Response or error *)
let call ~net ?(model=Gemini) ?(host=default_host) ?(port=default_port) ~prompt () =
  let request_body = build_request ~model ~prompt in
  try
    (* Connect to llm-mcp server using Eio.Net *)
    Eio.Net.with_tcp_connect ~host ~service:(string_of_int port) net @@ fun flow ->
    (* Send HTTP request *)
    let request = Printf.sprintf
      "POST /mcp HTTP/1.1\r\n\
       Host: %s:%d\r\n\
       Content-Type: application/json\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n\
       %s"
      host port (String.length request_body) request_body
    in
    Eio.Flow.copy_string request flow;
    Eio.Flow.shutdown flow `Send;

    (* Read response *)
    let buf = Buffer.create 4096 in
    let rec read_all () =
      let chunk = Cstruct.create 4096 in
      match Eio.Flow.single_read flow chunk with
      | n ->
          Buffer.add_string buf (Cstruct.to_string ~len:n chunk);
          read_all ()
      | exception End_of_file -> ()
    in
    read_all ();

    (* Parse HTTP response *)
    let response_str = Buffer.contents buf in
    let body_start =
      try
        let idx = Str.search_forward (Str.regexp "\r\n\r\n") response_str 0 in
        idx + 4
      with Not_found -> 0
    in
    let body = String.sub response_str body_start (String.length response_str - body_start) in
    parse_response body
  with
  | Unix.Unix_error (err, _, _) ->
      Error (ConnectionError (Unix.error_message err))
  | exn ->
      Error (ConnectionError (Printexc.to_string exn))

(** Convenience function for quick classification tasks *)
let classify ~net ~prompt () =
  call ~net ~model:Gemini ~prompt ()

(** {1 Walph Intent Classification} *)

(** Walph command intent *)
type walph_intent =
  | Start of { preset: string; target: string option }
  | Stop
  | Pause
  | Resume
  | Status
  | Ignore

(** Classify natural language message into Walph intent

    @param net Eio network capability
    @param message Natural language message to classify
    @return Classified intent with confidence *)
let classify_walph_intent ~net ~message () =
  let prompt = Printf.sprintf {|You are a command classifier for Walph automation system.

Classify this message into exactly one intent:
- START: 작업 시작 요청 (커버리지, 리팩토링, 문서화, drain 등)
- STOP: 정지/종료/그만 요청
- PAUSE: 일시정지/멈춰 요청
- RESUME: 재개/계속 요청
- STATUS: 상태 조회/뭐해 요청
- IGNORE: Walph 관련 아님

Message: "%s"

Output JSON only (no markdown):
{"intent": "START|STOP|PAUSE|RESUME|STATUS|IGNORE", "preset": "drain|coverage|refactor|docs|null", "target": "file/path or null", "confidence": 0.0-1.0}|} message
  in
  match call ~net ~model:Gemini ~prompt () with
  | Error e -> Error e
  | Ok response ->
      try
        (* Extract JSON from response, handling potential markdown code blocks *)
        let json_str =
          let content = String.trim response.content in
          if String.length content > 0 && content.[0] = '{' then content
          else
            (* Try to extract JSON from markdown code block *)
            let re = Str.regexp {|```json?\n?\(.*\)\n?```|} in
            if Str.string_match re content 0 then
              Str.matched_group 1 content
            else content
        in
        let json = Yojson.Safe.from_string json_str in
        match json with
        | `Assoc fields ->
            let intent_str = match List.assoc_opt "intent" fields with
              | Some (`String s) -> String.uppercase_ascii s
              | _ -> "IGNORE"
            in
            let preset = match List.assoc_opt "preset" fields with
              | Some (`String s) when s <> "null" -> s
              | _ -> "drain"
            in
            let target = match List.assoc_opt "target" fields with
              | Some (`String s) when s <> "null" -> Some s
              | _ -> None
            in
            let confidence = match List.assoc_opt "confidence" fields with
              | Some (`Float f) -> f
              | Some (`Int i) -> float_of_int i
              | _ -> 0.5
            in
            let intent = match intent_str with
              | "START" -> Start { preset; target }
              | "STOP" -> Stop
              | "PAUSE" -> Pause
              | "RESUME" -> Resume
              | "STATUS" -> Status
              | _ -> Ignore
            in
            (* Only return intent if confidence is high enough *)
            if confidence >= 0.7 then Ok intent
            else Ok Ignore
        | _ -> Ok Ignore
      with _ -> Ok Ignore

(** {1 Chain Orchestration} *)

(** Build MCP JSON-RPC request for chain.orchestrate
    @param goal The goal description for orchestration
    @param timeout Timeout in seconds (default: 120)
    @param max_replans Maximum re-planning attempts (default: 2) *)
let build_chain_request ~goal ?(timeout=120) ?(max_replans=2) () =
  Printf.sprintf {|{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "chain.orchestrate",
    "arguments": {
      "goal": %s,
      "timeout": %d,
      "max_replans": %d,
      "trace": false,
      "verify_on_complete": true
    }
  }
}|} (Yojson.Safe.to_string (`String goal)) timeout max_replans

(** Call chain.orchestrate on llm-mcp server
    @param net Eio network capability
    @param goal Goal description for orchestration
    @param host llm-mcp server host (default: 127.0.0.1)
    @param port llm-mcp server port (default: 8932)
    @param timeout_sec Timeout in seconds (default: 120 for long chains)
    @return Response content or error *)
let call_chain ~net ~goal ?(host=default_host) ?(port=default_port) ?(timeout_sec=120.0) () =
  let request_body = build_chain_request ~goal ~timeout:(int_of_float timeout_sec) () in
  try
    Eio.Net.with_tcp_connect ~host ~service:(string_of_int port) net @@ fun flow ->
    let request = Printf.sprintf
      "POST /mcp HTTP/1.1\r\n\
       Host: %s:%d\r\n\
       Content-Type: application/json\r\n\
       Accept: application/json, text/event-stream\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n\
       %s"
      host port (String.length request_body) request_body
    in
    Eio.Flow.copy_string request flow;
    Eio.Flow.shutdown flow `Send;

    (* Read response with timeout *)
    let buf = Buffer.create 16384 in
    let deadline = Unix.gettimeofday () +. timeout_sec in
    let rec read_all () =
      if Unix.gettimeofday () > deadline then
        ()  (* Timeout *)
      else
        let chunk = Cstruct.create 4096 in
        match Eio.Flow.single_read flow chunk with
        | n ->
            Buffer.add_string buf (Cstruct.to_string ~len:n chunk);
            read_all ()
        | exception End_of_file -> ()
    in
    read_all ();

    (* Parse SSE response - extract last data line *)
    let response_str = Buffer.contents buf in
    let lines = String.split_on_char '\n' response_str in
    let data_lines = List.filter_map (fun line ->
      let line = String.trim line in
      if String.length line > 6 && String.sub line 0 6 = "data: " then
        Some (String.sub line 6 (String.length line - 6))
      else None
    ) lines in
    match List.rev data_lines with
    | last_data :: _ ->
        (try
          let json = Yojson.Safe.from_string last_data in
          match json with
          | `Assoc fields ->
              (match List.assoc_opt "result" fields with
               | Some (`Assoc result_fields) ->
                   (match List.assoc_opt "content" result_fields with
                    | Some (`List [`Assoc text_fields]) ->
                        (match List.assoc_opt "text" text_fields with
                         | Some (`String s) -> Ok s
                         | _ -> Ok last_data)
                    | Some (`String s) -> Ok s
                    | _ -> Ok last_data)
               | _ ->
                   (match List.assoc_opt "error" fields with
                    | Some (`Assoc err) ->
                        let msg = match List.assoc_opt "message" err with
                          | Some (`String s) -> s
                          | _ -> "Unknown chain error"
                        in
                        Error (ServerError (500, msg))
                    | _ -> Ok last_data))
          | _ -> Ok last_data
        with _ -> Ok last_data)
    | [] -> Error (ParseError "No data in response")
  with
  | Unix.Unix_error (err, _, _) ->
      Error (ConnectionError (Unix.error_message err))
  | exn ->
      Error (ConnectionError (Printexc.to_string exn))
