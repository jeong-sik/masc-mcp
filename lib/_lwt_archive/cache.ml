(** MASC Cache - Shared Context Store

    에이전트 간 컨텍스트 공유 및 캐싱:
    - 파일 콘텐츠 해시
    - API 응답 (JIRA, GitHub)
    - 임베딩 결과
    - 코드베이스 요약

    Storage: .masc/cache/
*)

open Lwt.Syntax

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
  with _ -> None

(** Check if entry is expired *)
let is_expired entry =
  match entry.expires_at with
  | None -> false
  | Some exp -> Unix.gettimeofday () > exp

(** Set cache entry *)
let set config ~key ~value ?(ttl_seconds : int option) ?(tags : string list = []) ()
    : (cache_entry, string) result Lwt.t =
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
    Lwt.return (Ok entry)
  with e ->
    Lwt.return (Error (Printexc.to_string e))

(** Get cache entry *)
let get config ~key : (cache_entry option, string) result Lwt.t =
  let path = cache_file config key in
  if not (Sys.file_exists path) then
    Lwt.return (Ok None)
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
          Lwt.return (Ok None)
        end else
          Lwt.return (Ok (Some entry))
      | None -> Lwt.return (Ok None)
    with e ->
      Lwt.return (Error (Printexc.to_string e))

(** Delete cache entry *)
let delete config ~key : (bool, string) result Lwt.t =
  let path = cache_file config key in
  if not (Sys.file_exists path) then
    Lwt.return (Ok false)
  else
    try
      Sys.remove path;
      Lwt.return (Ok true)
    with e ->
      Lwt.return (Error (Printexc.to_string e))

(** List all cache entries *)
let list config ?(tag : string option) () : cache_entry list Lwt.t =
  let dir = cache_dir config in
  if not (Sys.file_exists dir) then
    Lwt.return []
  else
    let entries = Sys.readdir dir |> Array.to_list in
    let* results = Lwt_list.filter_map_s (fun filename ->
      if not (Filename.check_suffix filename ".json") then
        Lwt.return None
      else
        let path = Filename.concat dir filename in
        try
          let ic = open_in path in
          let content = really_input_string ic (in_channel_length ic) in
          close_in ic;
          let json = Yojson.Safe.from_string content in
          match entry_of_json json with
          | Some entry ->
            if is_expired entry then begin
              Sys.remove path;
              Lwt.return None
            end else begin
              match tag with
              | None -> Lwt.return (Some entry)
              | Some t ->
                if List.mem t entry.tags then Lwt.return (Some entry)
                else Lwt.return None
            end
          | None -> Lwt.return None
        with _ -> Lwt.return None
    ) entries in
    Lwt.return results

(** Clear all cache entries *)
let clear config : (int, string) result Lwt.t =
  let dir = cache_dir config in
  if not (Sys.file_exists dir) then
    Lwt.return (Ok 0)
  else
    try
      let entries = Sys.readdir dir |> Array.to_list in
      let count = List.fold_left (fun acc filename ->
        if Filename.check_suffix filename ".json" then begin
          let path = Filename.concat dir filename in
          Sys.remove path;
          acc + 1
        end else acc
      ) 0 entries in
      Lwt.return (Ok count)
    with e ->
      Lwt.return (Error (Printexc.to_string e))

(** Get cache stats *)
let stats config : (int * int * float) Lwt.t =
  (* Returns: (total_entries, total_bytes, oldest_timestamp) *)
  let dir = cache_dir config in
  if not (Sys.file_exists dir) then
    Lwt.return (0, 0, 0.0)
  else
    let entries = Sys.readdir dir |> Array.to_list in
    let count, bytes, oldest = List.fold_left (fun (c, b, o) filename ->
      if Filename.check_suffix filename ".json" then begin
        let path = Filename.concat dir filename in
        let stat = Unix.stat path in
        let size = stat.Unix.st_size in
        let mtime = stat.Unix.st_mtime in
        let oldest' = if o = 0.0 || mtime < o then mtime else o in
        (c + 1, b + size, oldest')
      end else (c, b, o)
    ) (0, 0, 0.0) entries in
    Lwt.return (count, bytes, oldest)

(** Format stats as string *)
let format_stats (count, bytes, oldest) =
  let size_str =
    if bytes < 1024 then Printf.sprintf "%d B" bytes
    else if bytes < 1024 * 1024 then Printf.sprintf "%.1f KB" (float_of_int bytes /. 1024.0)
    else Printf.sprintf "%.1f MB" (float_of_int bytes /. 1024.0 /. 1024.0)
  in
  let age_str =
    if oldest = 0.0 then "N/A"
    else
      let age = Unix.gettimeofday () -. oldest in
      if age < 60.0 then Printf.sprintf "%.0fs ago" age
      else if age < 3600.0 then Printf.sprintf "%.1fm ago" (age /. 60.0)
      else if age < 86400.0 then Printf.sprintf "%.1fh ago" (age /. 3600.0)
      else Printf.sprintf "%.1fd ago" (age /. 86400.0)
  in
  Printf.sprintf "📦 Cache: %d entries, %s, oldest: %s" count size_str age_str
