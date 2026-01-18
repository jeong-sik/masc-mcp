(** Redis Common Module - Shared utilities for Redis backends *)

(* Key construction helpers *)
let make_namespaced_key ~namespace key =
  Printf.sprintf "%s:%s" namespace key

let make_lock_key key = "lock:" ^ key

let make_pubsub_key channel = "pubsub:" ^ channel

(* Owner value format for distributed locks *)
let make_owner_value owner =
  Printf.sprintf "%s:%f" owner (Unix.gettimeofday ())

let verify_owner ~owner ~value =
  let owner_prefix = owner ^ ":" in
  String.length value >= String.length owner_prefix &&
  String.sub value 0 (String.length owner_prefix) = owner_prefix

(* Strip namespace prefix from key *)
let strip_namespace_prefix ~namespace key =
  let prefix_len = String.length namespace + 1 in
  if String.length key > prefix_len then
    String.sub key prefix_len (String.length key - prefix_len)
  else key

(* Message JSON for pub/sub *)
let make_message_json message =
  Yojson.Safe.to_string (`Assoc [
    ("message", `String message);
    ("timestamp", `Float (Unix.gettimeofday ()));
  ])

let parse_message_json msg_json =
  try
    let msg = Yojson.Safe.from_string msg_json in
    let open Yojson.Safe.Util in
    Some (msg |> member "message" |> to_string)
  with _ -> None

(* Generic release_lock logic - takes get and delete functions as parameters *)
let release_lock_generic ~get ~delete ~key ~owner =
  let lock_key = make_lock_key key in
  match get lock_key with
  | Error e -> Error e
  | Ok None -> Ok false  (* Lock doesn't exist *)
  | Ok (Some value) ->
      if verify_owner ~owner ~value then
        delete lock_key
      else
        Ok false  (* Not the owner *)

(* Generic get_all logic - takes list_keys and get functions as parameters *)
let get_all_generic ~list_keys ~get ~prefix =
  match list_keys prefix with
  | Error e -> Error e
  | Ok keys ->
      let pairs = List.filter_map (fun k ->
        match get k with
        | Ok (Some v) -> Some (k, v)
        | _ -> None
      ) keys in
      Ok pairs

(* Lock value construction and verification *)
let make_lock_value owner =
  Printf.sprintf "%s:%f" owner (Unix.gettimeofday ())

let verify_lock_owner value owner =
  let owner_prefix = owner ^ ":" in
  String.length value >= String.length owner_prefix &&
  String.sub value 0 (String.length owner_prefix) = owner_prefix

(* ============================================ *)
(* Upstash REST API JSON Response Parsers      *)
(* ============================================ *)

(** Parse GET response: {"result": "value"} or {"result": null} *)
let parse_get_response body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let result = json |> member "result" in
    match result with
    | `Null -> Ok None
    | `String s -> Ok (Some s)
    | _ -> Ok None
  with _ -> Ok None

(** Parse DEL response: {"result": 1} (deleted) or {"result": 0} (not found) *)
let parse_del_response body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let result = json |> member "result" |> to_int in
    Ok (result > 0)
  with _ -> Ok false

(** Parse EXISTS response: {"result": 1} (exists) or {"result": 0} (not found) *)
let parse_exists_response body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "result" |> to_int > 0
  with _ -> false

(** Parse KEYS response: {"result": ["key1", "key2", ...]} *)
let parse_keys_response body ~namespace =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let keys = json |> member "result" |> to_list |> List.map to_string in
    let stripped = List.map (strip_namespace_prefix ~namespace) keys in
    Ok stripped
  with _ -> Ok []

(** Parse SETNX response: {"result": 1} (set) or {"result": 0} (already exists) *)
let parse_setnx_response body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    Ok (json |> member "result" |> to_int = 1)
  with _ -> Ok false

(** Parse SET NX EX response: {"result": "OK"} (acquired) or {"result": null} (failed) *)
let parse_set_nx_ex_response body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let result = json |> member "result" in
    (* "OK" means lock acquired, null means already locked *)
    Ok (result <> `Null && result <> `String "")
  with _ -> Ok false

(** Parse EXPIRE response: {"result": 1} (success) or {"result": 0} (key not found) *)
let parse_expire_response body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    Ok (json |> member "result" |> to_int = 1)
  with _ -> Ok false

(** Parse LPUSH response: {"result": queue_length} *)
let parse_lpush_response body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let queue_len = json |> member "result" |> to_int in
    Ok (min queue_len 1)  (* Return 1 as pseudo-subscriber count *)
  with _ -> Ok 0

(** Parse PING response: "PONG" or {"result": "PONG"} *)
let parse_ping_response body =
  String.length body > 0 && (
    String.sub body 0 1 <> "{" ||
    try
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      json |> member "result" |> to_string = "PONG"
    with _ -> false)
