(** MASC Metrics Cache - Performance Optimization

    Caches agent metrics to avoid repeated file reads:
    - TTL-based expiration (default 5 minutes)
    - Thread-safe with Lwt
    - Reduces I/O for fitness calculations

    Performance improvement:
    - Before: Every fitness calculation reads all JSONL files
    - After: Cache hit returns in O(1), cache miss reads once
*)

open Lwt.Syntax

(** Cache entry with timestamp *)
type cache_entry = {
  metrics: Metrics_store.agent_metrics;
  cached_at: float;
}

(** In-memory cache *)
let cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 16

(** Cache TTL in seconds - now externalized via Level2_config *)
let cache_ttl = ref (Level2_config.Metrics_cache.ttl_seconds ())

(** MAGI Recommendation: Hit/miss counters for observability *)
let cache_hits = ref 0
let cache_misses = ref 0

(** Set cache TTL *)
let set_ttl ttl = cache_ttl := ttl

(** Get cache TTL *)
let get_ttl () = !cache_ttl

(** Reload TTL from config (for runtime updates) *)
let reload_ttl () =
  cache_ttl := Level2_config.Metrics_cache.ttl_seconds ()

(** Generate cache key *)
let cache_key ~agent_id ~days =
  Printf.sprintf "%s:%d" agent_id days

(** Clear entire cache *)
let clear () =
  Hashtbl.clear cache

(** Clear cache for specific agent *)
let invalidate ~agent_id =
  let keys_to_remove = Hashtbl.fold (fun k _ acc ->
    if String.length k > String.length agent_id &&
       String.sub k 0 (String.length agent_id) = agent_id then
      k :: acc
    else acc
  ) cache [] in
  List.iter (Hashtbl.remove cache) keys_to_remove

(** Get cached metrics or fetch from store *)
let get_metrics config ~agent_id ~days : Metrics_store.agent_metrics option Lwt.t =
  let key = cache_key ~agent_id ~days in
  let now = Unix.gettimeofday () in

  match Hashtbl.find_opt cache key with
  | Some entry when now -. entry.cached_at < !cache_ttl ->
    (* Cache hit - return cached value *)
    incr cache_hits;
    Lwt.return (Some entry.metrics)
  | _ ->
    (* Cache miss or expired - fetch from store *)
    incr cache_misses;
    let* metrics_opt = Metrics_store.calculate_agent_metrics config ~agent_id ~days in
    (match metrics_opt with
    | Some metrics ->
      Hashtbl.replace cache key { metrics; cached_at = now };
      Lwt.return (Some metrics)
    | None ->
      Lwt.return None)

(** Get metrics for multiple agents (batch operation) *)
let get_metrics_batch config ~agent_ids ~days : (string * Metrics_store.agent_metrics option) list Lwt.t =
  Lwt_list.map_s (fun agent_id ->
    let* metrics = get_metrics config ~agent_id ~days in
    Lwt.return (agent_id, metrics)
  ) agent_ids

(** Cache statistics *)
type cache_stats = {
  size: int;
  hits: int;
  misses: int;
  hit_rate: float;
  oldest_entry_age: float option;
}

(** Get cache statistics *)
let get_stats () : cache_stats =
  let now = Unix.gettimeofday () in
  let oldest = Hashtbl.fold (fun _ entry acc ->
    let age = now -. entry.cached_at in
    match acc with
    | None -> Some age
    | Some old_age -> Some (max old_age age)
  ) cache None in
  let total = !cache_hits + !cache_misses in
  let hit_rate = if total = 0 then 0.0
    else float_of_int !cache_hits /. float_of_int total in
  {
    size = Hashtbl.length cache;
    hits = !cache_hits;
    misses = !cache_misses;
    hit_rate;
    oldest_entry_age = oldest;
  }

(** Reset statistics counters *)
let reset_stats () =
  cache_hits := 0;
  cache_misses := 0

(** Prune expired entries *)
let prune () =
  let now = Unix.gettimeofday () in
  let expired = Hashtbl.fold (fun k entry acc ->
    if now -. entry.cached_at >= !cache_ttl then k :: acc
    else acc
  ) cache [] in
  List.iter (Hashtbl.remove cache) expired;
  List.length expired
