(** Agent Card - A2A Protocol Compatible Agent Metadata

    Implements the A2A Agent Card specification for standardized agent discovery.
    Provides `/.well-known/agent-card.json` endpoint support.

    @see https://github.com/google/A2A for A2A specification
*)

(** Provider information for the agent *)
type provider = {
  organization: string;
  url: string option;
} [@@deriving yojson, show, eq]

(** Skill definition for agent capabilities *)
type skill = {
  id: string;
  name: string;
  description: string option;
  input_modes: string list;   (** e.g., ["text", "file", "data"] *)
  output_modes: string list;  (** e.g., ["text", "file", "artifact"] *)
} [@@deriving yojson, show, eq]

(** Protocol binding for agent communication *)
type binding = {
  protocol: string;  (** "json-rpc" | "grpc" | "rest" | "sse" *)
  url: string;
} [@@deriving yojson, show, eq]

(** Security scheme for authentication *)
type security_scheme = {
  scheme_type: string;  (** "bearer" | "apiKey" | "oauth2" | "none" *)
  bearer_format: string option;
  api_key_name: string option;
  api_key_in: string option;  (** "header" | "query" *)
} [@@deriving yojson, show, eq]

(** Agent Card - A2A compliant agent metadata *)
type agent_card = {
  name: string;
  version: string;
  description: string option;
  provider: provider option;
  capabilities: string list;
  skills: skill list;
  bindings: binding list;
  security_schemes: (string * security_scheme) list;
  default_input_modes: string list;
  default_output_modes: string list;
  extensions: (string * Yojson.Safe.t) list;
  signature: string option;  (** Optional cryptographic signature *)
  created_at: string;
  updated_at: string;
} [@@deriving show, eq]

(** Convert agent_card to JSON (A2A spec format) *)
let to_json (card : agent_card) : Yojson.Safe.t =
  let optional_string key = function
    | None -> []
    | Some v -> [(key, `String v)]
  in
  let optional_obj key = function
    | None -> []
    | Some v -> [(key, provider_to_yojson v)]
  in
  `Assoc ([
    ("name", `String card.name);
    ("version", `String card.version);
  ] @ optional_string "description" card.description
    @ optional_obj "provider" card.provider
    @ [
    ("capabilities", `List (List.map (fun s -> `String s) card.capabilities));
    ("skills", `List (List.map skill_to_yojson card.skills));
    ("bindings", `List (List.map binding_to_yojson card.bindings));
    ("securitySchemes", `Assoc (List.map (fun (k, v) -> (k, security_scheme_to_yojson v)) card.security_schemes));
    ("defaultInputModes", `List (List.map (fun s -> `String s) card.default_input_modes));
    ("defaultOutputModes", `List (List.map (fun s -> `String s) card.default_output_modes));
  ] @ (if card.extensions = [] then [] else [
    ("extensions", `Assoc card.extensions)
  ]) @ optional_string "signature" card.signature
    @ [
    ("createdAt", `String card.created_at);
    ("updatedAt", `String card.updated_at);
  ])

(** Parse agent_card from JSON *)
let from_json (json : Yojson.Safe.t) : (agent_card, string) result =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let version = json |> member "version" |> to_string in
    let description = json |> member "description" |> to_string_option in

    let provider =
      match json |> member "provider" with
      | `Null -> None
      | p ->
        match provider_of_yojson p with
        | Ok v -> Some v
        | Error _ -> None
    in

    let capabilities =
      json |> member "capabilities"
      |> to_list
      |> List.map to_string
    in

    let skills =
      json |> member "skills"
      |> to_list
      |> List.filter_map (fun s ->
        match skill_of_yojson s with
        | Ok v -> Some v
        | Error _ -> None)
    in

    let bindings =
      json |> member "bindings"
      |> to_list
      |> List.filter_map (fun b ->
        match binding_of_yojson b with
        | Ok v -> Some v
        | Error _ -> None)
    in

    let security_schemes =
      match json |> member "securitySchemes" with
      | `Assoc pairs ->
        List.filter_map (fun (k, v) ->
          match security_scheme_of_yojson v with
          | Ok scheme -> Some (k, scheme)
          | Error _ -> None) pairs
      | _ -> []
    in

    let default_input_modes =
      json |> member "defaultInputModes"
      |> to_list
      |> List.map to_string
    in

    let default_output_modes =
      json |> member "defaultOutputModes"
      |> to_list
      |> List.map to_string
    in

    let extensions =
      match json |> member "extensions" with
      | `Assoc pairs -> pairs
      | _ -> []
    in

    let signature = json |> member "signature" |> to_string_option in
    let created_at = json |> member "createdAt" |> to_string in
    let updated_at = json |> member "updatedAt" |> to_string in

    Ok {
      name;
      version;
      description;
      provider;
      capabilities;
      skills;
      bindings;
      security_schemes;
      default_input_modes;
      default_output_modes;
      extensions;
      signature;
      created_at;
      updated_at;
    }
  with
  | e -> Error (Printf.sprintf "Failed to parse agent card: %s" (Printexc.to_string e))

(** Get current ISO8601 timestamp *)
let now_iso8601 () : string =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(** MASC skill definitions *)
let masc_skills : skill list = [
  {
    id = "task-management";
    name = "Task Management";
    description = Some "Create, claim, and complete tasks on the quest board";
    input_modes = ["text"];
    output_modes = ["text"; "data"];
  };
  {
    id = "agent-coordination";
    name = "Agent Coordination";
    description = Some "Join room, broadcast messages, coordinate with other agents";
    input_modes = ["text"];
    output_modes = ["text"; "stream"];
  };
  {
    id = "file-locking";
    name = "File Locking";
    description = Some "Lock and unlock files to prevent concurrent edits";
    input_modes = ["text"];
    output_modes = ["text"];
  };
  {
    id = "git-worktree";
    name = "Git Worktree Management";
    description = Some "Create isolated git worktrees for parallel development";
    input_modes = ["text"];
    output_modes = ["text"; "file"];
  };
  {
    id = "voting";
    name = "Multi-Agent Voting";
    description = Some "Create votes and reach consensus among agents";
    input_modes = ["text"];
    output_modes = ["text"; "data"];
  };
  {
    id = "cost-tracking";
    name = "Cost Tracking";
    description = Some "Log and report token usage and API costs";
    input_modes = ["data"];
    output_modes = ["text"; "data"];
  };
  {
    id = "human-in-loop";
    name = "Human-in-the-Loop";
    description = Some "Interrupt workflows for user approval on sensitive actions";
    input_modes = ["text"];
    output_modes = ["text"];
  };
  {
    id = "portal-a2a";
    name = "A2A Portal Communication";
    description = Some "Direct agent-to-agent private communication channels";
    input_modes = ["text"];
    output_modes = ["text"; "stream"];
  };
]

(** Generate default MASC agent card *)
let generate_default ?(port=8935) ?(host="127.0.0.1") () : agent_card =
  let timestamp = now_iso8601 () in
  let base_url = Printf.sprintf "http://%s:%d" host port in
  {
    name = "MASC-MCP";
    version = "2.0.0";
    description = Some "Multi-Agent Streaming Coordination - A2A compatible agent coordination system";
    provider = Some {
      organization = "Second Brain";
      url = Some "https://github.com/jeong-sik/me";
    };
    capabilities = [
      "streaming";
      "push-notifications";
      "state-management";
      "multi-agent";
      "git-worktree";
      "voting";
      "cost-tracking";
      "human-in-loop";
      "encryption";
      "rate-limiting";
    ];
    skills = masc_skills;
    bindings = [
      { protocol = "sse"; url = Printf.sprintf "%s/sse" base_url };
      { protocol = "json-rpc"; url = Printf.sprintf "%s/mcp" base_url };
      { protocol = "rest"; url = Printf.sprintf "%s/api/v1" base_url };
      { protocol = "grpc"; url = Printf.sprintf "grpc://%s:%d" host (port + 1000) };  (* gRPC on port+1000 *)
    ];
    security_schemes = [
      ("bearer", {
        scheme_type = "bearer";
        bearer_format = Some "MASC Token";
        api_key_name = None;
        api_key_in = None;
      });
      ("none", {
        scheme_type = "none";
        bearer_format = None;
        api_key_name = None;
        api_key_in = None;
      });
    ];
    default_input_modes = ["text"; "data"];
    default_output_modes = ["text"; "data"; "stream"];
    extensions = [
      ("masc", `Assoc [
        ("roomPath", `String ".masc");
        ("storageTypes", `List [`String "file"; `String "postgres"]);
        ("features", `Assoc [
          ("voting", `Bool true);
          ("worktree", `Bool true);
          ("costTracking", `Bool true);
          ("encryption", `Bool true);
          ("rateLimiting", `Bool true);
          ("zombieGC", `Bool true);
          ("branchExecution", `Bool true);
        ]);
      ]);
    ];
    signature = None;
    created_at = timestamp;
    updated_at = timestamp;
  }

(** Update agent card with new bindings based on runtime config *)
let with_bindings (card : agent_card) (bindings : binding list) : agent_card =
  let timestamp = now_iso8601 () in
  { card with bindings; updated_at = timestamp }

(** Add extension data to agent card *)
let with_extension (card : agent_card) (key : string) (value : Yojson.Safe.t) : agent_card =
  let timestamp = now_iso8601 () in
  let extensions =
    List.filter (fun (k, _) -> k <> key) card.extensions
    @ [(key, value)]
  in
  { card with extensions; updated_at = timestamp }
