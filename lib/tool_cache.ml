(** Cache Tool Handlers

    Extracted from mcp_server_eio.ml for testability.
    6 tools: cache_set, cache_get, cache_delete, cache_list, cache_clear, cache_stats
*)

(** Tool handler context *)
type context = {
  config: Room.config;
}

(** Tool result type *)
type result = bool * string

(** {1 Argument Helpers} *)

let get_string args key default =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) -> s
       | _ -> default)
  | _ -> default

let get_string_opt args key =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) when s <> "" -> Some s
       | _ -> None)
  | _ -> None

let get_int_opt args key =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Int i) -> Some i
       | _ -> None)
  | _ -> None

let get_string_list args key =
  match args with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`List lst) ->
           List.filter_map (function `String s -> Some s | _ -> None) lst
       | _ -> [])
  | _ -> []

(** {1 Individual Handlers} *)

let handle_cache_set ctx args : result =
  let key = get_string args "key" "" in
  let value = get_string args "value" "" in
  let ttl_seconds = get_int_opt args "ttl_seconds" in
  let tags = get_string_list args "tags" in
  match Cache_eio.set ctx.config ~key ~value ?ttl_seconds ~tags () with
  | Ok entry ->
      (true, Yojson.Safe.pretty_to_string (Cache_eio.entry_to_json entry))
  | Error e ->
      (false, Printf.sprintf "❌ Cache set failed: %s" e)

let handle_cache_get ctx args : result =
  let key = get_string args "key" "" in
  match Cache_eio.get ctx.config ~key with
  | Ok (Some entry) ->
      (true, Yojson.Safe.pretty_to_string (`Assoc [
        ("hit", `Bool true);
        ("entry", Cache_eio.entry_to_json entry);
      ]))
  | Ok None ->
      (true, Yojson.Safe.pretty_to_string (`Assoc [
        ("hit", `Bool false);
        ("key", `String key);
      ]))
  | Error e ->
      (false, Printf.sprintf "❌ Cache get failed: %s" e)

let handle_cache_delete ctx args : result =
  let key = get_string args "key" "" in
  match Cache_eio.delete ctx.config ~key with
  | Ok removed ->
      let json = `Assoc [
        ("removed", `Bool removed);
        ("key", `String key);
      ] in
      (true, Yojson.Safe.pretty_to_string json)
  | Error e ->
      (false, Printf.sprintf "❌ Cache delete failed: %s" e)

let handle_cache_list ctx args : result =
  let tag = get_string_opt args "tag" in
  let entries = Cache_eio.list ctx.config ?tag () in
  let json = `Assoc [
    ("count", `Int (List.length entries));
    ("entries", `List (List.map Cache_eio.entry_to_json entries));
  ] in
  (true, Yojson.Safe.pretty_to_string json)

let handle_cache_clear ctx _args : result =
  match Cache_eio.clear ctx.config with
  | Ok count ->
      (true, Printf.sprintf "Cleared %d cache entries" count)
  | Error e ->
      (false, Printf.sprintf "❌ Cache clear failed: %s" e)

let handle_cache_stats ctx _args : result =
  match Cache_eio.stats ctx.config with
  | Ok (total, expired, size_bytes) ->
      let json = `Assoc [
        ("total_entries", `Int total);
        ("expired_entries", `Int expired);
        ("size_bytes", `Float size_bytes);
        ("size_kb", `Float (size_bytes /. 1024.0));
      ] in
      (true, Yojson.Safe.pretty_to_string json)
  | Error e ->
      (false, Printf.sprintf "❌ Cache stats failed: %s" e)

(** {1 Dispatcher} *)

let dispatch ctx ~name ~args : result option =
  match name with
  | "masc_cache_set" -> Some (handle_cache_set ctx args)
  | "masc_cache_get" -> Some (handle_cache_get ctx args)
  | "masc_cache_delete" -> Some (handle_cache_delete ctx args)
  | "masc_cache_list" -> Some (handle_cache_list ctx args)
  | "masc_cache_clear" -> Some (handle_cache_clear ctx args)
  | "masc_cache_stats" -> Some (handle_cache_stats ctx args)
  | _ -> None
