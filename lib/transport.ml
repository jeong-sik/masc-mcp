(** Transport Layer - Protocol Bindings Abstraction

    Provides a unified interface for multiple transport protocols:
    - JSON-RPC 2.0 (MCP standard)
    - REST (OpenAPI compatible)
    - gRPC (Phase 4)
    - SSE (Server-Sent Events for streaming)

    Based on A2A Protocol bindings specification.
*)

(** Request/Response types *)
type request = {
  id: string option;          (* Request ID for correlation *)
  method_name: string;        (* Method/tool name *)
  params: Yojson.Safe.t;      (* Parameters as JSON *)
  headers: (string * string) list;  (* Transport headers *)
}

type response = {
  id: string option;
  success: bool;
  result: Yojson.Safe.t option;
  error: error option;
}

and error = {
  code: int;
  message: string;
  data: Yojson.Safe.t option;
}

(** Protocol type *)
type protocol =
  | JsonRpc
  | Rest
  | Grpc
  | Sse

let protocol_to_string = function
  | JsonRpc -> "json-rpc"
  | Rest -> "rest"
  | Grpc -> "grpc"
  | Sse -> "sse"

let protocol_of_string = function
  | "json-rpc" | "jsonrpc" -> Some JsonRpc
  | "rest" -> Some Rest
  | "grpc" -> Some Grpc
  | "sse" -> Some Sse
  | _ -> None

(** Transport binding configuration *)
type binding = {
  protocol: protocol;
  url: string;
  options: (string * string) list;
}

(** Standard JSON-RPC 2.0 error codes *)
module ErrorCodes = struct
  let parse_error = -32700
  let invalid_request = -32600
  let method_not_found = -32601
  let invalid_params = -32602
  let internal_error = -32603
  (* Server errors: -32000 to -32099 *)
  let server_error = -32000
  let not_initialized = -32001
  let task_not_found = -32002
  let permission_denied = -32003
end

(** Create error response *)
let make_error ?id ?(data=None) ~code ~message () : response =
  { id; success = false; result = None; error = Some { code; message; data } }

(** Create success response *)
let make_success ?id ~result () : response =
  { id; success = true; result = Some result; error = None }

(** JSON-RPC 2.0 serialization *)
module JsonRpc = struct
  let version = "2.0"

  (** Parse JSON-RPC request *)
  let parse_request (json : Yojson.Safe.t) : (request, string) result =
    let open Yojson.Safe.Util in
    try
      let jsonrpc = json |> member "jsonrpc" |> to_string in
      if jsonrpc <> version then
        Error (Printf.sprintf "Invalid JSON-RPC version: %s" jsonrpc)
      else
        let id = json |> member "id" |> to_string_option in
        let method_name = json |> member "method" |> to_string in
        let params = json |> member "params" in
        Ok { id; method_name; params; headers = [] }
    with e -> Error (Printexc.to_string e)

  (** Serialize JSON-RPC response *)
  let serialize_response (resp : response) : Yojson.Safe.t =
    let base = [("jsonrpc", `String version)] in
    let with_id = match resp.id with
      | Some id -> base @ [("id", `String id)]
      | None -> base @ [("id", `Null)]
    in
    if resp.success then
      `Assoc (with_id @ [("result", match resp.result with Some r -> r | None -> `Null)])
    else
      let error_obj = match resp.error with
        | Some e ->
            let base_err = [("code", `Int e.code); ("message", `String e.message)] in
            let with_data = match e.data with
              | Some d -> base_err @ [("data", d)]
              | None -> base_err
            in
            `Assoc with_data
        | None -> `Assoc [("code", `Int ErrorCodes.internal_error); ("message", `String "Unknown error")]
      in
      `Assoc (with_id @ [("error", error_obj)])

  (** Create JSON-RPC request *)
  let make_request ?id ~method_name ~params () : Yojson.Safe.t =
    let base = [
      ("jsonrpc", `String version);
      ("method", `String method_name);
      ("params", params);
    ] in
    let with_id = match id with
      | Some i -> base @ [("id", `String i)]
      | None -> base
    in
    `Assoc with_id
end

(** REST API helpers *)
module Rest = struct
  (** HTTP method type *)
  type http_method = GET | POST | PUT | DELETE | PATCH

  let method_to_string = function
    | GET -> "GET"
    | POST -> "POST"
    | PUT -> "PUT"
    | DELETE -> "DELETE"
    | PATCH -> "PATCH"

  (** Map MASC tools to REST endpoints *)
  let tool_to_endpoint = function
    (* Task operations *)
    | "masc_status" -> (GET, "/api/v1/status")
    | "masc_tasks" -> (GET, "/api/v1/tasks")
    | "masc_add_task" -> (POST, "/api/v1/tasks")
    | "masc_claim" -> (POST, "/api/v1/tasks/{task_id}/claim")
    | "masc_done" -> (POST, "/api/v1/tasks/{task_id}/done")
    | "masc_cancel_task" -> (POST, "/api/v1/tasks/{task_id}/cancel")
    (* Agent operations *)
    | "masc_join" -> (POST, "/api/v1/agents")
    | "masc_leave" -> (DELETE, "/api/v1/agents/{agent_name}")
    | "masc_who" -> (GET, "/api/v1/agents")
    | "masc_agents" -> (GET, "/api/v1/agents/detailed")
    (* Messaging *)
    | "masc_broadcast" -> (POST, "/api/v1/messages")
    | "masc_messages" -> (GET, "/api/v1/messages")
    (* Locking *)
    | "masc_lock" -> (POST, "/api/v1/locks")
    | "masc_unlock" -> (DELETE, "/api/v1/locks/{file_path}")
    (* Voting *)
    | "masc_vote_create" -> (POST, "/api/v1/votes")
    | "masc_vote_cast" -> (POST, "/api/v1/votes/{vote_id}/cast")
    | "masc_vote_status" -> (GET, "/api/v1/votes/{vote_id}")
    | "masc_votes" -> (GET, "/api/v1/votes")
    (* Planning *)
    | "masc_plan_init" -> (POST, "/api/v1/planning/{task_id}")
    | "masc_plan_update" -> (PUT, "/api/v1/planning/{task_id}/plan")
    | "masc_note_add" -> (POST, "/api/v1/planning/{task_id}/notes")
    | "masc_deliver" -> (PUT, "/api/v1/planning/{task_id}/deliverable")
    | "masc_plan_get" -> (GET, "/api/v1/planning/{task_id}")
    (* Agent Card *)
    | "masc_agent_card" -> (GET, "/.well-known/agent-card.json")
    (* Worktree *)
    | "masc_worktree_create" -> (POST, "/api/v1/worktrees")
    | "masc_worktree_remove" -> (DELETE, "/api/v1/worktrees/{task_id}")
    | "masc_worktree_list" -> (GET, "/api/v1/worktrees")
    (* Default *)
    | tool -> (POST, Printf.sprintf "/api/v1/tools/%s" tool)

  (** Parse REST request to internal request *)
  let parse_request ~http_method ~path ~query_params ~body : request =
    let method_name =
      (* Try to reverse map from path to tool name *)
      match http_method, path with
      | "GET", "/" | "GET", "/api/v1/status" -> "masc_status"
      | "GET", "/api/v1/tasks" -> "masc_tasks"
      | "GET", "/api/v1/agents" -> "masc_who"
      | "GET", "/.well-known/agent-card.json" -> "masc_agent_card"
      | _, p when String.length p > 14 && String.sub p 0 14 = "/api/v1/tools/" ->
          String.sub p 14 (String.length p - 14)
      | _ -> "unknown"
    in
    let params = match body with
      | "" -> `Assoc query_params
      | s -> (try Yojson.Safe.from_string s with _ -> `Assoc query_params)
    in
    { id = None; method_name; params; headers = [] }

  (** Generate OpenAPI-style endpoint documentation *)
  let generate_openapi_paths () : Yojson.Safe.t =
    let tools = [
      "masc_status"; "masc_tasks"; "masc_add_task"; "masc_claim"; "masc_done";
      "masc_cancel_task"; "masc_join"; "masc_leave"; "masc_who"; "masc_agents";
      "masc_broadcast"; "masc_messages"; "masc_lock"; "masc_unlock";
      "masc_vote_create"; "masc_vote_cast"; "masc_vote_status"; "masc_votes";
      "masc_plan_init"; "masc_plan_update"; "masc_note_add"; "masc_deliver"; "masc_plan_get";
      "masc_agent_card"; "masc_worktree_create"; "masc_worktree_remove"; "masc_worktree_list";
    ] in
    let paths = List.map (fun tool ->
      let (http_method, path) = tool_to_endpoint tool in
      let method_str = String.lowercase_ascii (method_to_string http_method) in
      (path, `Assoc [
        (method_str, `Assoc [
          ("operationId", `String tool);
          ("summary", `String (Printf.sprintf "MASC %s operation" tool));
          ("tags", `List [`String "MASC"]);
        ])
      ])
    ) tools in
    `Assoc paths
end

(** Get available bindings for current MASC instance *)
let get_bindings ~host ~port : binding list =
  let base_url = Printf.sprintf "http://%s:%d" host port in
  [
    { protocol = Sse; url = Printf.sprintf "%s/sse" base_url; options = [] };
    { protocol = JsonRpc; url = Printf.sprintf "%s/mcp" base_url; options = [] };
    { protocol = Rest; url = Printf.sprintf "%s/api/v1" base_url; options = [] };
  ]

(** Bindings to JSON (for Agent Card) *)
let bindings_to_json (bindings : binding list) : Yojson.Safe.t =
  `List (List.map (fun b ->
    `Assoc [
      ("protocol", `String (protocol_to_string b.protocol));
      ("url", `String b.url);
    ]
  ) bindings)

(* ===== OCaml 5.x Parallel Processing ===== *)

(** Atomic request counter for unique IDs *)
let request_counter = Atomic.make 0

(** Generate unique request ID (thread-safe) *)
let generate_request_id () =
  let count = Atomic.fetch_and_add request_counter 1 in
  Printf.sprintf "req-%d-%d" (Unix.getpid ()) count

(** Parallel request execution using Domains (OCaml 5.x)
    Execute multiple requests in parallel and collect results *)
let parallel_requests ~(requests : request list) ~(handler : request -> response) : response list =
  match requests with
  | [] -> []
  | [single] -> [handler single]  (* Single request, no parallelism needed *)
  | _ ->
      (* Use Domain.spawn for true parallelism *)
      let domains = List.map (fun req ->
        Domain.spawn (fun () -> handler req)
      ) requests in
      List.map Domain.join domains

(** Batch request processing with concurrency limit *)
let batch_requests ~concurrency ~requests ~handler =
  let rec process acc remaining =
    match remaining with
    | [] -> List.rev acc
    | _ ->
        let (batch, rest) =
          let rec take n lst acc =
            if n = 0 then (List.rev acc, lst)
            else match lst with
              | [] -> (List.rev acc, [])
              | x :: xs -> take (n-1) xs (x :: acc)
          in
          take concurrency remaining []
        in
        let results = parallel_requests ~requests:batch ~handler in
        process (List.rev_append results acc) rest
  in
  process [] requests

(** Atomic statistics for monitoring *)
module Stats = struct
  let total_requests = Atomic.make 0
  let successful_requests = Atomic.make 0
  let failed_requests = Atomic.make 0
  let total_latency_ms = Atomic.make 0

  let record_request ~success ~latency_ms =
    Atomic.incr total_requests;
    if success then Atomic.incr successful_requests
    else Atomic.incr failed_requests;
    Atomic.fetch_and_add total_latency_ms latency_ms |> ignore

  let get_stats () =
    let total = Atomic.get total_requests in
    let success = Atomic.get successful_requests in
    let failed = Atomic.get failed_requests in
    let latency = Atomic.get total_latency_ms in
    `Assoc [
      ("total_requests", `Int total);
      ("successful_requests", `Int success);
      ("failed_requests", `Int failed);
      ("avg_latency_ms", `Int (if total > 0 then latency / total else 0));
    ]

  let reset () =
    Atomic.set total_requests 0;
    Atomic.set successful_requests 0;
    Atomic.set failed_requests 0;
    Atomic.set total_latency_ms 0
end
