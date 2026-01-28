(** Transport Module Coverage Tests

    Tests for MASC Transport Layer - Protocol Bindings Abstraction:
    - request: id, method_name, params, headers
    - response: id, success, result, error
    - error: code, message, data
    - protocol: JsonRpc, Rest, Grpc, Sse
    - binding: protocol, url, options
    - ErrorCodes: standard JSON-RPC 2.0 error codes
    - make_error / make_success: response factories
    - JsonRpc module: parse_request, serialize_response
*)

open Alcotest

module Transport = Masc_mcp.Transport

(* ============================================================
   protocol Tests
   ============================================================ *)

let test_protocol_jsonrpc () =
  let p = Transport.JsonRpc in
  check string "jsonrpc string" "json-rpc" (Transport.protocol_to_string p)

let test_protocol_rest () =
  let p = Transport.Rest in
  check string "rest string" "rest" (Transport.protocol_to_string p)

let test_protocol_grpc () =
  let p = Transport.Grpc in
  check string "grpc string" "grpc" (Transport.protocol_to_string p)

let test_protocol_sse () =
  let p = Transport.Sse in
  check string "sse string" "sse" (Transport.protocol_to_string p)

let test_protocol_of_string_jsonrpc () =
  check (option bool) "json-rpc" (Some true)
    (Option.map (fun p -> p = Transport.JsonRpc) (Transport.protocol_of_string "json-rpc"));
  check (option bool) "jsonrpc" (Some true)
    (Option.map (fun p -> p = Transport.JsonRpc) (Transport.protocol_of_string "jsonrpc"))

let test_protocol_of_string_rest () =
  check (option bool) "rest" (Some true)
    (Option.map (fun p -> p = Transport.Rest) (Transport.protocol_of_string "rest"))

let test_protocol_of_string_grpc () =
  check (option bool) "grpc" (Some true)
    (Option.map (fun p -> p = Transport.Grpc) (Transport.protocol_of_string "grpc"))

let test_protocol_of_string_sse () =
  check (option bool) "sse" (Some true)
    (Option.map (fun p -> p = Transport.Sse) (Transport.protocol_of_string "sse"))

let test_protocol_of_string_invalid () =
  check (option string) "invalid" None
    (Option.map Transport.protocol_to_string (Transport.protocol_of_string "invalid"))

let test_protocol_roundtrip () =
  let protocols = [Transport.JsonRpc; Transport.Rest; Transport.Grpc; Transport.Sse] in
  List.iter (fun p ->
    let s = Transport.protocol_to_string p in
    match Transport.protocol_of_string s with
    | Some p' -> check bool (Printf.sprintf "%s roundtrip" s) true (p = p')
    | None -> fail (Printf.sprintf "roundtrip failed for %s" s)
  ) protocols

(* ============================================================
   ErrorCodes Tests
   ============================================================ *)

let test_error_codes_parse () =
  check int "parse_error" (-32700) Transport.ErrorCodes.parse_error

let test_error_codes_invalid_request () =
  check int "invalid_request" (-32600) Transport.ErrorCodes.invalid_request

let test_error_codes_method_not_found () =
  check int "method_not_found" (-32601) Transport.ErrorCodes.method_not_found

let test_error_codes_invalid_params () =
  check int "invalid_params" (-32602) Transport.ErrorCodes.invalid_params

let test_error_codes_internal () =
  check int "internal_error" (-32603) Transport.ErrorCodes.internal_error

let test_error_codes_server () =
  check int "server_error" (-32000) Transport.ErrorCodes.server_error

let test_error_codes_not_initialized () =
  check int "not_initialized" (-32001) Transport.ErrorCodes.not_initialized

let test_error_codes_task_not_found () =
  check int "task_not_found" (-32002) Transport.ErrorCodes.task_not_found

let test_error_codes_permission_denied () =
  check int "permission_denied" (-32003) Transport.ErrorCodes.permission_denied

let test_error_codes_all_negative () =
  let codes = [
    Transport.ErrorCodes.parse_error;
    Transport.ErrorCodes.invalid_request;
    Transport.ErrorCodes.method_not_found;
    Transport.ErrorCodes.invalid_params;
    Transport.ErrorCodes.internal_error;
    Transport.ErrorCodes.server_error;
    Transport.ErrorCodes.not_initialized;
    Transport.ErrorCodes.task_not_found;
    Transport.ErrorCodes.permission_denied;
  ] in
  List.iter (fun c ->
    check bool (Printf.sprintf "code %d negative" c) true (c < 0)
  ) codes

(* ============================================================
   make_error Tests
   ============================================================ *)

let test_make_error_basic () =
  let resp = Transport.make_error ~code:(-32600) ~message:"Invalid request" () in
  check bool "not success" false resp.success;
  check (option string) "no result" None (Option.map Yojson.Safe.to_string resp.result);
  match resp.error with
  | Some e ->
    check int "error code" (-32600) e.code;
    check string "error message" "Invalid request" e.message
  | None -> fail "expected error"

let test_make_error_with_id () =
  let resp = Transport.make_error ~id:"req-123" ~code:(-32601) ~message:"Not found" () in
  check (option string) "has id" (Some "req-123") resp.id

let test_make_error_with_data () =
  let data = `Assoc [("detail", `String "extra info")] in
  let resp = Transport.make_error ~code:(-32000) ~message:"Server error" ~data:(Some data) () in
  match resp.error with
  | Some e ->
    (match e.data with
     | Some d ->
       let open Yojson.Safe.Util in
       let detail = d |> member "detail" |> to_string in
       check string "error data" "extra info" detail
     | None -> fail "expected error data")
  | None -> fail "expected error"

let test_make_error_no_data () =
  let resp = Transport.make_error ~code:(-32700) ~message:"Parse error" () in
  match resp.error with
  | Some e -> check (option string) "no data" None (Option.map Yojson.Safe.to_string e.data)
  | None -> fail "expected error"

(* ============================================================
   make_success Tests
   ============================================================ *)

let test_make_success_basic () =
  let result = `Assoc [("status", `String "ok")] in
  let resp = Transport.make_success ~result () in
  check bool "success" true resp.success;
  check (option string) "no error" None (Option.map (fun e -> e.Transport.message) resp.error);
  match resp.result with
  | Some r ->
    let open Yojson.Safe.Util in
    let status = r |> member "status" |> to_string in
    check string "result status" "ok" status
  | None -> fail "expected result"

let test_make_success_with_id () =
  let result = `Int 42 in
  let resp = Transport.make_success ~id:"req-456" ~result () in
  check (option string) "has id" (Some "req-456") resp.id;
  check bool "success" true resp.success

let test_make_success_complex_result () =
  let result = `Assoc [
    ("items", `List [`String "a"; `String "b"; `String "c"]);
    ("count", `Int 3);
  ] in
  let resp = Transport.make_success ~result () in
  match resp.result with
  | Some r ->
    let open Yojson.Safe.Util in
    let count = r |> member "count" |> to_int in
    check int "result count" 3 count
  | None -> fail "expected result"

(* ============================================================
   request Tests
   ============================================================ *)

let test_request_creation () =
  let req : Transport.request = {
    id = Some "req-001";
    method_name = "masc_status";
    params = `Assoc [];
    headers = [("Content-Type", "application/json")];
  } in
  check (option string) "id" (Some "req-001") req.id;
  check string "method_name" "masc_status" req.method_name;
  check int "headers count" 1 (List.length req.headers)

let test_request_no_id () =
  let req : Transport.request = {
    id = None;
    method_name = "notification";
    params = `Null;
    headers = [];
  } in
  check (option string) "no id" None req.id

let test_request_with_params () =
  let req : Transport.request = {
    id = Some "req-002";
    method_name = "masc_join";
    params = `Assoc [("agent_name", `String "claude")];
    headers = [];
  } in
  let open Yojson.Safe.Util in
  let agent = req.params |> member "agent_name" |> to_string in
  check string "param value" "claude" agent

(* ============================================================
   response Tests
   ============================================================ *)

let test_response_success () =
  let resp : Transport.response = {
    id = Some "req-003";
    success = true;
    result = Some (`String "done");
    error = None;
  } in
  check bool "success" true resp.success;
  check (option string) "no error" None (Option.map (fun e -> e.Transport.message) resp.error)

let test_response_error () =
  let resp : Transport.response = {
    id = Some "req-004";
    success = false;
    result = None;
    error = Some { code = -32601; message = "Method not found"; data = None };
  } in
  check bool "not success" false resp.success;
  match resp.error with
  | Some e -> check string "error message" "Method not found" e.message
  | None -> fail "expected error"

(* ============================================================
   error Tests
   ============================================================ *)

let test_error_creation () =
  let e : Transport.error = {
    code = -32700;
    message = "Parse error";
    data = None;
  } in
  check int "code" (-32700) e.code;
  check string "message" "Parse error" e.message

let test_error_with_data () =
  let e : Transport.error = {
    code = -32000;
    message = "Server error";
    data = Some (`Assoc [("stack", `String "traceback...")]);
  } in
  match e.data with
  | Some d ->
    let open Yojson.Safe.Util in
    let stack = d |> member "stack" |> to_string in
    check bool "has stack" true (String.length stack > 0)
  | None -> fail "expected data"

(* ============================================================
   binding Tests
   ============================================================ *)

let test_binding_creation () =
  let b : Transport.binding = {
    protocol = Transport.JsonRpc;
    url = "http://localhost:8931";
    options = [("timeout", "30")];
  } in
  check bool "protocol jsonrpc" true (b.protocol = Transport.JsonRpc);
  check string "url" "http://localhost:8931" b.url;
  check int "options count" 1 (List.length b.options)

let test_binding_rest () =
  let b : Transport.binding = {
    protocol = Transport.Rest;
    url = "https://api.example.com/v1";
    options = [];
  } in
  check bool "protocol rest" true (b.protocol = Transport.Rest)

let test_binding_grpc () =
  let b : Transport.binding = {
    protocol = Transport.Grpc;
    url = "grpc://localhost:50051";
    options = [("tls", "true")];
  } in
  check bool "protocol grpc" true (b.protocol = Transport.Grpc)

(* ============================================================
   JsonRpc Module Tests
   ============================================================ *)

let test_jsonrpc_version () =
  check string "version" "2.0" Transport.JsonRpc.version

let test_jsonrpc_parse_request_valid () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `String "1");
    ("method", `String "test_method");
    ("params", `Assoc [("key", `String "value")]);
  ] in
  match Transport.JsonRpc.parse_request json with
  | Ok req ->
    check (option string) "id" (Some "1") req.id;
    check string "method" "test_method" req.method_name
  | Error e -> fail e

let test_jsonrpc_parse_request_no_id () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "notification");
    ("params", `Null);
  ] in
  match Transport.JsonRpc.parse_request json with
  | Ok req ->
    check (option string) "no id" None req.id;
    check string "method" "notification" req.method_name
  | Error e -> fail e

let test_jsonrpc_parse_request_wrong_version () =
  let json = `Assoc [
    ("jsonrpc", `String "1.0");
    ("method", `String "test");
    ("params", `Null);
  ] in
  match Transport.JsonRpc.parse_request json with
  | Ok _ -> fail "should reject wrong version"
  | Error e -> check bool "error contains version" true (String.length e > 0)

let test_jsonrpc_parse_request_missing_method () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("params", `Null);
  ] in
  match Transport.JsonRpc.parse_request json with
  | Ok _ -> fail "should reject missing method"
  | Error _ -> ()

let test_jsonrpc_serialize_response_success () =
  let resp = Transport.make_success ~id:"req-1" ~result:(`String "done") () in
  let json = Transport.JsonRpc.serialize_response resp in
  let json_str = Yojson.Safe.to_string json in
  check bool "has jsonrpc" true
    (try let _ = Str.search_forward (Str.regexp "2.0") json_str 0 in true
     with Not_found -> false);
  check bool "has result" true
    (try let _ = Str.search_forward (Str.regexp "result") json_str 0 in true
     with Not_found -> false)

let test_jsonrpc_serialize_response_error () =
  let resp = Transport.make_error ~id:"req-2" ~code:(-32600) ~message:"Invalid" () in
  let json = Transport.JsonRpc.serialize_response resp in
  let json_str = Yojson.Safe.to_string json in
  check bool "has error" true
    (try let _ = Str.search_forward (Str.regexp "error") json_str 0 in true
     with Not_found -> false)

let test_jsonrpc_serialize_response_no_id () =
  let resp = Transport.make_success ~result:`Null () in
  let json = Transport.JsonRpc.serialize_response resp in
  let json_str = Yojson.Safe.to_string json in
  check bool "has null id" true
    (try let _ = Str.search_forward (Str.regexp "null") json_str 0 in true
     with Not_found -> false)

let test_jsonrpc_make_request_basic () =
  let json = Transport.JsonRpc.make_request ~method_name:"masc_join" ~params:(`Assoc []) () in
  let json_str = Yojson.Safe.to_string json in
  check bool "has method" true
    (try let _ = Str.search_forward (Str.regexp "masc_join") json_str 0 in true
     with Not_found -> false)

let test_jsonrpc_make_request_with_id () =
  let json = Transport.JsonRpc.make_request ~id:"req-99" ~method_name:"test" ~params:`Null () in
  let json_str = Yojson.Safe.to_string json in
  check bool "has id" true
    (try let _ = Str.search_forward (Str.regexp "req-99") json_str 0 in true
     with Not_found -> false)

(* ============================================================
   Rest Module Tests
   ============================================================ *)

let test_rest_method_to_string_get () =
  check string "GET" "GET" (Transport.Rest.method_to_string Transport.Rest.GET)

let test_rest_method_to_string_post () =
  check string "POST" "POST" (Transport.Rest.method_to_string Transport.Rest.POST)

let test_rest_method_to_string_put () =
  check string "PUT" "PUT" (Transport.Rest.method_to_string Transport.Rest.PUT)

let test_rest_method_to_string_delete () =
  check string "DELETE" "DELETE" (Transport.Rest.method_to_string Transport.Rest.DELETE)

let test_rest_method_to_string_patch () =
  check string "PATCH" "PATCH" (Transport.Rest.method_to_string Transport.Rest.PATCH)

let test_rest_tool_to_endpoint_status () =
  let (m, path) = Transport.Rest.tool_to_endpoint "masc_status" in
  check string "method" "GET" (Transport.Rest.method_to_string m);
  check string "path" "/api/v1/status" path

let test_rest_tool_to_endpoint_tasks () =
  let (m, path) = Transport.Rest.tool_to_endpoint "masc_tasks" in
  check string "method" "GET" (Transport.Rest.method_to_string m);
  check string "path" "/api/v1/tasks" path

let test_rest_tool_to_endpoint_add_task () =
  let (m, path) = Transport.Rest.tool_to_endpoint "masc_add_task" in
  check string "method" "POST" (Transport.Rest.method_to_string m);
  check string "path" "/api/v1/tasks" path

let test_rest_tool_to_endpoint_claim () =
  let (m, path) = Transport.Rest.tool_to_endpoint "masc_claim" in
  check string "method" "POST" (Transport.Rest.method_to_string m);
  check bool "has task_id" true
    (try let _ = Str.search_forward (Str.regexp "task_id") path 0 in true
     with Not_found -> false)

let test_rest_tool_to_endpoint_join () =
  let (m, path) = Transport.Rest.tool_to_endpoint "masc_join" in
  check string "method" "POST" (Transport.Rest.method_to_string m);
  check string "path" "/api/v1/agents" path

let test_rest_tool_to_endpoint_leave () =
  let (m, path) = Transport.Rest.tool_to_endpoint "masc_leave" in
  check string "method" "DELETE" (Transport.Rest.method_to_string m);
  check bool "has agent_name" true
    (try let _ = Str.search_forward (Str.regexp "agent_name") path 0 in true
     with Not_found -> false)

let test_rest_tool_to_endpoint_broadcast () =
  let (m, path) = Transport.Rest.tool_to_endpoint "masc_broadcast" in
  check string "method" "POST" (Transport.Rest.method_to_string m);
  check string "path" "/api/v1/messages" path

let test_rest_tool_to_endpoint_agent_card () =
  let (m, path) = Transport.Rest.tool_to_endpoint "masc_agent_card" in
  check string "method" "GET" (Transport.Rest.method_to_string m);
  check string "path" "/.well-known/agent-card.json" path

let test_rest_tool_to_endpoint_unknown () =
  let (m, path) = Transport.Rest.tool_to_endpoint "unknown_tool" in
  check string "method" "POST" (Transport.Rest.method_to_string m);
  check bool "has unknown" true
    (try let _ = Str.search_forward (Str.regexp "unknown_tool") path 0 in true
     with Not_found -> false)

let test_rest_parse_request_status () =
  let req = Transport.Rest.parse_request ~http_method:"GET" ~path:"/api/v1/status" ~query_params:[] ~body:"" in
  check string "method_name" "masc_status" req.method_name

let test_rest_parse_request_tasks () =
  let req = Transport.Rest.parse_request ~http_method:"GET" ~path:"/api/v1/tasks" ~query_params:[] ~body:"" in
  check string "method_name" "masc_tasks" req.method_name

let test_rest_parse_request_agents () =
  let req = Transport.Rest.parse_request ~http_method:"GET" ~path:"/api/v1/agents" ~query_params:[] ~body:"" in
  check string "method_name" "masc_who" req.method_name

let test_rest_parse_request_agent_card () =
  let req = Transport.Rest.parse_request ~http_method:"GET" ~path:"/.well-known/agent-card.json" ~query_params:[] ~body:"" in
  check string "method_name" "masc_agent_card" req.method_name

let test_rest_parse_request_tool () =
  let req = Transport.Rest.parse_request ~http_method:"POST" ~path:"/api/v1/tools/custom_tool" ~query_params:[] ~body:"" in
  check string "method_name" "custom_tool" req.method_name

let test_rest_parse_request_unknown () =
  let req = Transport.Rest.parse_request ~http_method:"POST" ~path:"/some/random/path" ~query_params:[] ~body:"" in
  check string "method_name" "unknown" req.method_name

let test_rest_parse_request_with_body () =
  let req = Transport.Rest.parse_request ~http_method:"POST" ~path:"/api/v1/status" ~query_params:[] ~body:"{\"key\":\"value\"}" in
  check bool "has params" true (req.params <> `Null)

let test_rest_parse_request_root () =
  let req = Transport.Rest.parse_request ~http_method:"GET" ~path:"/" ~query_params:[] ~body:"" in
  check string "method_name" "masc_status" req.method_name

let test_rest_generate_openapi_paths () =
  let paths = Transport.Rest.generate_openapi_paths () in
  let json_str = Yojson.Safe.to_string paths in
  check bool "has status" true
    (try let _ = Str.search_forward (Str.regexp "masc_status") json_str 0 in true
     with Not_found -> false);
  check bool "has join" true
    (try let _ = Str.search_forward (Str.regexp "masc_join") json_str 0 in true
     with Not_found -> false)

(* ============================================================
   get_bindings Tests
   ============================================================ *)

let test_get_bindings_nonempty () =
  let bindings = Transport.get_bindings ~host:"localhost" ~port:8931 in
  check bool "nonempty" true (List.length bindings > 0)

let test_get_bindings_has_sse () =
  let bindings = Transport.get_bindings ~host:"localhost" ~port:8931 in
  check bool "has sse" true
    (List.exists (fun b -> b.Transport.protocol = Transport.Sse) bindings)

let test_get_bindings_has_jsonrpc () =
  let bindings = Transport.get_bindings ~host:"localhost" ~port:8931 in
  check bool "has jsonrpc" true
    (List.exists (fun b -> b.Transport.protocol = Transport.JsonRpc) bindings)

let test_get_bindings_has_rest () =
  let bindings = Transport.get_bindings ~host:"localhost" ~port:8931 in
  check bool "has rest" true
    (List.exists (fun b -> b.Transport.protocol = Transport.Rest) bindings)

let test_get_bindings_url_contains_host () =
  let bindings = Transport.get_bindings ~host:"127.0.0.1" ~port:9000 in
  check bool "contains host" true
    (List.exists (fun b ->
      try let _ = Str.search_forward (Str.regexp "127.0.0.1") b.Transport.url 0 in true
      with Not_found -> false
    ) bindings)

let test_get_bindings_url_contains_port () =
  let bindings = Transport.get_bindings ~host:"localhost" ~port:9999 in
  check bool "contains port" true
    (List.exists (fun b ->
      try let _ = Str.search_forward (Str.regexp "9999") b.Transport.url 0 in true
      with Not_found -> false
    ) bindings)

(* ============================================================
   bindings_to_json Tests
   ============================================================ *)

let test_bindings_to_json_nonempty () =
  let bindings = Transport.get_bindings ~host:"localhost" ~port:8931 in
  let json = Transport.bindings_to_json bindings in
  let json_str = Yojson.Safe.to_string json in
  check bool "nonempty" true (String.length json_str > 2)

let test_bindings_to_json_has_protocol () =
  let bindings = Transport.get_bindings ~host:"localhost" ~port:8931 in
  let json = Transport.bindings_to_json bindings in
  let json_str = Yojson.Safe.to_string json in
  check bool "has protocol" true
    (try let _ = Str.search_forward (Str.regexp "protocol") json_str 0 in true
     with Not_found -> false)

let test_bindings_to_json_has_url () =
  let bindings = Transport.get_bindings ~host:"localhost" ~port:8931 in
  let json = Transport.bindings_to_json bindings in
  let json_str = Yojson.Safe.to_string json in
  check bool "has url" true
    (try let _ = Str.search_forward (Str.regexp "url") json_str 0 in true
     with Not_found -> false)

(* ============================================================
   generate_request_id Tests
   ============================================================ *)

let test_generate_request_id_nonempty () =
  let id = Transport.generate_request_id () in
  check bool "nonempty" true (String.length id > 0)

let test_generate_request_id_has_prefix () =
  let id = Transport.generate_request_id () in
  check bool "has req- prefix" true
    (String.length id > 4 && String.sub id 0 4 = "req-")

let test_generate_request_id_unique () =
  let id1 = Transport.generate_request_id () in
  let id2 = Transport.generate_request_id () in
  check bool "unique" true (id1 <> id2)

(* ============================================================
   parallel_requests Tests
   ============================================================ *)

let test_parallel_requests_empty () =
  let handler _ = Transport.make_success ~result:`Null () in
  let results = Transport.parallel_requests ~requests:[] ~handler in
  check int "empty" 0 (List.length results)

let test_parallel_requests_single () =
  let req : Transport.request = { id = Some "1"; method_name = "test"; params = `Null; headers = [] } in
  let handler _ = Transport.make_success ~result:(`String "done") () in
  let results = Transport.parallel_requests ~requests:[req] ~handler in
  check int "single" 1 (List.length results)

let test_parallel_requests_multiple () =
  let req1 : Transport.request = { id = Some "1"; method_name = "test1"; params = `Null; headers = [] } in
  let req2 : Transport.request = { id = Some "2"; method_name = "test2"; params = `Null; headers = [] } in
  let handler _ = Transport.make_success ~result:`Null () in
  let results = Transport.parallel_requests ~requests:[req1; req2] ~handler in
  check int "multiple" 2 (List.length results)

(* ============================================================
   batch_requests Tests
   ============================================================ *)

let test_batch_requests_empty () =
  let handler _ = Transport.make_success ~result:`Null () in
  let results = Transport.batch_requests ~concurrency:2 ~requests:[] ~handler in
  check int "empty" 0 (List.length results)

let test_batch_requests_less_than_concurrency () =
  let req : Transport.request = { id = Some "1"; method_name = "test"; params = `Null; headers = [] } in
  let handler _ = Transport.make_success ~result:`Null () in
  let results = Transport.batch_requests ~concurrency:5 ~requests:[req] ~handler in
  check int "single" 1 (List.length results)

let test_batch_requests_more_than_concurrency () =
  let reqs = List.init 5 (fun i ->
    { Transport.id = Some (string_of_int i); method_name = "test"; params = `Null; headers = [] }
  ) in
  let handler _ = Transport.make_success ~result:`Null () in
  let results = Transport.batch_requests ~concurrency:2 ~requests:reqs ~handler in
  check int "five" 5 (List.length results)

(* ============================================================
   Stats Module Tests
   ============================================================ *)

let test_stats_record_request_success () =
  Transport.Stats.reset ();
  Transport.Stats.record_request ~success:true ~latency_ms:10;
  let stats = Transport.Stats.get_stats () in
  let json_str = Yojson.Safe.to_string stats in
  check bool "has total" true
    (try let _ = Str.search_forward (Str.regexp "total_requests") json_str 0 in true
     with Not_found -> false)

let test_stats_record_request_failure () =
  Transport.Stats.reset ();
  Transport.Stats.record_request ~success:false ~latency_ms:5;
  let stats = Transport.Stats.get_stats () in
  let json_str = Yojson.Safe.to_string stats in
  check bool "has failed" true
    (try let _ = Str.search_forward (Str.regexp "failed_requests") json_str 0 in true
     with Not_found -> false)

let test_stats_get_stats_returns_json () =
  Transport.Stats.reset ();
  let stats = Transport.Stats.get_stats () in
  check bool "is assoc" true
    (match stats with `Assoc _ -> true | _ -> false)

let test_stats_reset () =
  Transport.Stats.record_request ~success:true ~latency_ms:100;
  Transport.Stats.reset ();
  let stats = Transport.Stats.get_stats () in
  let json_str = Yojson.Safe.to_string stats in
  check bool "contains 0" true
    (try let _ = Str.search_forward (Str.regexp "\"total_requests\":0") json_str 0 in true
     with Not_found -> false)

let test_stats_avg_latency () =
  Transport.Stats.reset ();
  Transport.Stats.record_request ~success:true ~latency_ms:100;
  Transport.Stats.record_request ~success:true ~latency_ms:200;
  let stats = Transport.Stats.get_stats () in
  let json_str = Yojson.Safe.to_string stats in
  check bool "has avg_latency" true
    (try let _ = Str.search_forward (Str.regexp "avg_latency_ms") json_str 0 in true
     with Not_found -> false)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Transport Coverage" [
    "protocol.to_string", [
      test_case "jsonrpc" `Quick test_protocol_jsonrpc;
      test_case "rest" `Quick test_protocol_rest;
      test_case "grpc" `Quick test_protocol_grpc;
      test_case "sse" `Quick test_protocol_sse;
    ];
    "protocol.of_string", [
      test_case "jsonrpc" `Quick test_protocol_of_string_jsonrpc;
      test_case "rest" `Quick test_protocol_of_string_rest;
      test_case "grpc" `Quick test_protocol_of_string_grpc;
      test_case "sse" `Quick test_protocol_of_string_sse;
      test_case "invalid" `Quick test_protocol_of_string_invalid;
      test_case "roundtrip" `Quick test_protocol_roundtrip;
    ];
    "error_codes", [
      test_case "parse" `Quick test_error_codes_parse;
      test_case "invalid_request" `Quick test_error_codes_invalid_request;
      test_case "method_not_found" `Quick test_error_codes_method_not_found;
      test_case "invalid_params" `Quick test_error_codes_invalid_params;
      test_case "internal" `Quick test_error_codes_internal;
      test_case "server" `Quick test_error_codes_server;
      test_case "not_initialized" `Quick test_error_codes_not_initialized;
      test_case "task_not_found" `Quick test_error_codes_task_not_found;
      test_case "permission_denied" `Quick test_error_codes_permission_denied;
      test_case "all negative" `Quick test_error_codes_all_negative;
    ];
    "make_error", [
      test_case "basic" `Quick test_make_error_basic;
      test_case "with id" `Quick test_make_error_with_id;
      test_case "with data" `Quick test_make_error_with_data;
      test_case "no data" `Quick test_make_error_no_data;
    ];
    "make_success", [
      test_case "basic" `Quick test_make_success_basic;
      test_case "with id" `Quick test_make_success_with_id;
      test_case "complex result" `Quick test_make_success_complex_result;
    ];
    "request", [
      test_case "creation" `Quick test_request_creation;
      test_case "no id" `Quick test_request_no_id;
      test_case "with params" `Quick test_request_with_params;
    ];
    "response", [
      test_case "success" `Quick test_response_success;
      test_case "error" `Quick test_response_error;
    ];
    "error", [
      test_case "creation" `Quick test_error_creation;
      test_case "with data" `Quick test_error_with_data;
    ];
    "binding", [
      test_case "creation" `Quick test_binding_creation;
      test_case "rest" `Quick test_binding_rest;
      test_case "grpc" `Quick test_binding_grpc;
    ];
    "jsonrpc", [
      test_case "version" `Quick test_jsonrpc_version;
      test_case "parse valid" `Quick test_jsonrpc_parse_request_valid;
      test_case "parse no id" `Quick test_jsonrpc_parse_request_no_id;
      test_case "wrong version" `Quick test_jsonrpc_parse_request_wrong_version;
      test_case "missing method" `Quick test_jsonrpc_parse_request_missing_method;
      test_case "serialize success" `Quick test_jsonrpc_serialize_response_success;
      test_case "serialize error" `Quick test_jsonrpc_serialize_response_error;
      test_case "serialize no id" `Quick test_jsonrpc_serialize_response_no_id;
      test_case "make request basic" `Quick test_jsonrpc_make_request_basic;
      test_case "make request with id" `Quick test_jsonrpc_make_request_with_id;
    ];
    "rest.method_to_string", [
      test_case "GET" `Quick test_rest_method_to_string_get;
      test_case "POST" `Quick test_rest_method_to_string_post;
      test_case "PUT" `Quick test_rest_method_to_string_put;
      test_case "DELETE" `Quick test_rest_method_to_string_delete;
      test_case "PATCH" `Quick test_rest_method_to_string_patch;
    ];
    "rest.tool_to_endpoint", [
      test_case "status" `Quick test_rest_tool_to_endpoint_status;
      test_case "tasks" `Quick test_rest_tool_to_endpoint_tasks;
      test_case "add_task" `Quick test_rest_tool_to_endpoint_add_task;
      test_case "claim" `Quick test_rest_tool_to_endpoint_claim;
      test_case "join" `Quick test_rest_tool_to_endpoint_join;
      test_case "leave" `Quick test_rest_tool_to_endpoint_leave;
      test_case "broadcast" `Quick test_rest_tool_to_endpoint_broadcast;
      test_case "agent_card" `Quick test_rest_tool_to_endpoint_agent_card;
      test_case "unknown" `Quick test_rest_tool_to_endpoint_unknown;
    ];
    "rest.parse_request", [
      test_case "status" `Quick test_rest_parse_request_status;
      test_case "tasks" `Quick test_rest_parse_request_tasks;
      test_case "agents" `Quick test_rest_parse_request_agents;
      test_case "agent_card" `Quick test_rest_parse_request_agent_card;
      test_case "tool" `Quick test_rest_parse_request_tool;
      test_case "unknown" `Quick test_rest_parse_request_unknown;
      test_case "with body" `Quick test_rest_parse_request_with_body;
      test_case "root" `Quick test_rest_parse_request_root;
    ];
    "rest.generate_openapi_paths", [
      test_case "paths" `Quick test_rest_generate_openapi_paths;
    ];
    "get_bindings", [
      test_case "nonempty" `Quick test_get_bindings_nonempty;
      test_case "has sse" `Quick test_get_bindings_has_sse;
      test_case "has jsonrpc" `Quick test_get_bindings_has_jsonrpc;
      test_case "has rest" `Quick test_get_bindings_has_rest;
      test_case "url contains host" `Quick test_get_bindings_url_contains_host;
      test_case "url contains port" `Quick test_get_bindings_url_contains_port;
    ];
    "bindings_to_json", [
      test_case "nonempty" `Quick test_bindings_to_json_nonempty;
      test_case "has protocol" `Quick test_bindings_to_json_has_protocol;
      test_case "has url" `Quick test_bindings_to_json_has_url;
    ];
    "generate_request_id", [
      test_case "nonempty" `Quick test_generate_request_id_nonempty;
      test_case "has prefix" `Quick test_generate_request_id_has_prefix;
      test_case "unique" `Quick test_generate_request_id_unique;
    ];
    "parallel_requests", [
      test_case "empty" `Quick test_parallel_requests_empty;
      test_case "single" `Quick test_parallel_requests_single;
      test_case "multiple" `Quick test_parallel_requests_multiple;
    ];
    "batch_requests", [
      test_case "empty" `Quick test_batch_requests_empty;
      test_case "less than concurrency" `Quick test_batch_requests_less_than_concurrency;
      test_case "more than concurrency" `Quick test_batch_requests_more_than_concurrency;
    ];
    "stats", [
      test_case "record success" `Quick test_stats_record_request_success;
      test_case "record failure" `Quick test_stats_record_request_failure;
      test_case "get_stats returns json" `Quick test_stats_get_stats_returns_json;
      test_case "reset" `Quick test_stats_reset;
      test_case "avg latency" `Quick test_stats_avg_latency;
    ];
  ]
