(** MASC Cache - Shared Context Store (Eio Native)

    Pure synchronous cache operations.
    Compatible with Eio direct-style concurrency.

    에이전트 간 컨텍스트 공유 및 캐싱:
    - 파일 콘텐츠 해시
    - API 응답 (JIRA, GitHub)
    - 임베딩 결과
    - 코드베이스 요약

    Storage: .masc/cache/
*)

(** Cache entry *)
type cache_entry = {
  key: string;
  value: string;
  created_at: float;
  expires_at: float option;  (* None = no expiry *)
  tags: string list;
}

(** Get cache directory *)
let cache_dir (config : Room_utils.config) =
  Filename.concat config.base_path ".masc/cache"

(** Ensure cache directory exists *)
let ensure_cache_dir config =
  let dir = cache_dir config in
  let masc_dir = Filename.concat config.base_path ".masc" in
  if not (Sys.file_exists masc_dir) then
    Unix.mkdir masc_dir 0o755;
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755

(** Sanitize key for filename *)
let sanitize_key key =
  (* Replace unsafe chars with underscore, limit length *)
  let safe = Str.global_replace (Str.regexp "[^a-zA-Z0-9_-]") "_" key in
  if String.length safe > 64 then
    String.sub safe 0 64
  else safe

(** Get cache file path *)
let cache_file config key =
  Filename.concat (cache_dir config) (sanitize_key key ^ ".json")

(** Entry to JSON *)
let entry_to_json (entry : cache_entry) : Yojson.Safe.t =
  `Assoc [
    ("key", `String entry.key);
    ("value", `String entry.value);
    ("created_at", `Float entry.created_at);
    ("expires_at", match entry.expires_at with
      | Some t -> `Float t
      | None -> `Null);
    ("tags", `List (List.map (fun t -> `String t) entry.tags));
  ]

(** Entry from JSON *)
let entry_of_json (json : Yojson.Safe.t) : cache_entry option =
  let open Yojson.Safe.Util in
  try
    let key = json |> member "key" |> to_string in
    let value = json |> member "value" |> to_string in
    let created_at = json |> member "created_at" |> to_float in
    let expires_at = match json |> member "expires_at" with
      | `Null -> None
      | `Float f -> Some f
      | _ -> None
    in
    let tags = json |> member "tags" |> to_list |> List.map to_string in
    Some { key; value; created_at; expires_at; tags }
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Printf.eprintf "[cache] JSON type error in entry_of_json: %s\n%!" msg;
    None
  | e ->
    Printf.eprintf "[cache] Unexpected error in entry_of_json: %s\n%!" (Printexc.to_string e);
    None

(** Check if entry is expired *)
let is_expired entry =
  match entry.expires_at with
  | None -> false
  | Some exp -> Unix.gettimeofday () > exp

(** Set cache entry - synchronous *)
let set config ~key ~value ?(ttl_seconds : int option) ?(tags : string list = []) ()
    : (cache_entry, string) result =
  ensure_cache_dir config;
  let now = Unix.gettimeofday () in
  let expires_at = Option.map (fun ttl -> now +. float_of_int ttl) ttl_seconds in
  let entry = { key; value; created_at = now; expires_at; tags } in
  let path = cache_file config key in
  let json = entry_to_json entry in
  let content = Yojson.Safe.pretty_to_string json in
  try
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    Ok entry
  with e ->
    Error (Printexc.to_string e)

(** Get cache entry - synchronous *)
let get config ~key : (cache_entry option, string) result =
  let path = cache_file config key in
  if not (Sys.file_exists path) then
    Ok None
  else
    try
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      match entry_of_json json with
      | Some entry ->
        if is_expired entry then begin
          (* Auto-delete expired entries *)
          Sys.remove path;
          Ok None
        end else
          Ok (Some entry)
      | None -> Ok None
    with e ->
      Error (Printexc.to_string e)

(** Delete cache entry - synchronous *)
let delete config ~key : (bool, string) result =
  let path = cache_file config key in
  if not (Sys.file_exists path) then
    Ok false
  else
    try
      Sys.remove path;
      Ok true
    with e ->
      Error (Printexc.to_string e)

(** List all cache entries - synchronous *)
let list config ?(tag : string option) () : cache_entry list =
  let dir = cache_dir config in
  if not (Sys.file_exists dir) then
    []
  else
    let entries = Sys.readdir dir |> Array.to_list in
    List.filter_map (fun filename ->
      if not (Filename.check_suffix filename ".json") then
        None
      else
        let path = Filename.concat dir filename in
        match Safe_ops.read_file_safe path with
        | Error _ -> None  (* File read error - skip this entry *)
        | Ok content ->
          match Safe_ops.parse_json_safe ~context:"cache_get_all" content with
          | Error msg ->
            Printf.eprintf "[cache] %s\n%!" msg;
            None
          | Ok json ->
            match entry_of_json json with
            | Some entry ->
              if is_expired entry then begin
                Safe_ops.remove_file_logged ~context:"cache_expire" path;
                None
              end else begin
                match tag with
                | None -> Some entry
                | Some t ->
                  if List.mem t entry.tags then Some entry
                  else None
              end
            | None -> None
    ) entries

(** Clear all cache entries - synchronous *)
let clear config : (int, string) result =
  let dir = cache_dir config in
  if not (Sys.file_exists dir) then
    Ok 0
  else
    try
      let entries = Sys.readdir dir |> Array.to_list in
      let count = List.fold_left (fun acc filename ->
        if Filename.check_suffix filename ".json" then begin
          let path = Filename.concat dir filename in
          Safe_ops.remove_file_logged ~context:"cache_clear" path;
          acc + 1
        end else acc
      ) 0 entries in
      Ok count
    with e ->
      Error (Printexc.to_string e)

(** Get cache statistics - synchronous *)
let stats config : (int * int * float, string) result =
  (* Returns: (total_entries, expired_entries, total_size_bytes) *)
  let dir = cache_dir config in
  if not (Sys.file_exists dir) then
    Ok (0, 0, 0.0)
  else
    try
      let entries = Sys.readdir dir |> Array.to_list in
      let total, expired, size = List.fold_left (fun (t, e, s) filename ->
        if Filename.check_suffix filename ".json" then
          let path = Filename.concat dir filename in
          let file_size = (Unix.stat path).st_size in
          let is_exp =
            match Safe_ops.read_file_safe path with
            | Error _ -> false
            | Ok content ->
              match Safe_ops.parse_json_safe ~context:"cache_stats" content with
              | Error _ -> false
              | Ok json ->
                match entry_of_json json with
                | Some entry -> is_expired entry
                | None -> false
          in
          (t + 1, e + (if is_exp then 1 else 0), s +. float_of_int file_size)
        else (t, e, s)
      ) (0, 0, 0.0) entries in
      Ok (total, expired, size)
    with e ->
      Error (Printexc.to_string e)

(** Format stats for display *)
let format_stats (total, expired, size) =
  Printf.sprintf "Cache: %d entries (%d expired), %.1f KB"
    total expired (size /. 1024.0)
