(** MASC Drift Guard - Context Integrity Verification

    Verifies that handoff context is preserved accurately:
    - Detects semantic drift between original and received context
    - Flags potential information loss or distortion
    - Logs drift events for analysis

    Research basis:
    - Drift detection in LLM chains
    - Context compression verification

    Threshold: similarity < 0.85 → Drift detected
*)

open Lwt.Syntax

(** Config type alias *)
type config = Room_utils.config

(** Types of drift *)
type drift_type =
  | Semantic     (* Meaning changed *)
  | Factual      (* Facts omitted or altered *)
  | Structural   (* Format/structure changed *)
  | None         (* No drift detected *)

(** Verification result *)
type verification_result =
  | Verified of { similarity: float }
  | Drift_detected of {
      similarity: float;
      drift_type: drift_type;
      details: string;
    }

(** Drift event for logging *)
type drift_event = {
  id: string;
  timestamp: float;
  from_agent: string;
  to_agent: string;
  task_id: string;
  original_length: int;
  received_length: int;
  similarity: float;
  drift_type: drift_type;
  verified: bool;
}

(** Default similarity threshold - now externalized via Level2_config *)
let default_threshold () = Level2_config.Drift_guard.default_threshold ()

(** Get drift log file path *)
let drift_log_file (config : config) =
  Filename.concat config.Room_utils.base_path ".masc/drift_log.jsonl"

(** P1 Fix: Token cache to avoid re-tokenizing same text *)
let token_cache : (string, string list) Hashtbl.t = Hashtbl.create 64
let token_cache_max_size () = Level2_config.Token_cache.max_size ()

(** Tokenize text for comparison *)
let tokenize text =
  text
  |> String.lowercase_ascii
  |> Str.global_replace (Str.regexp "[^a-z0-9가-힣]+") " "
  |> String.split_on_char ' '
  |> List.filter (fun s -> String.length s > 0)

(** Tokenize with caching *)
let tokenize_cached text =
  (* Use first 32 chars of MD5 as cache key *)
  let hash = Digest.string text |> Digest.to_hex in
  match Hashtbl.find_opt token_cache hash with
  | Some tokens -> tokens
  | None ->
    let tokens = tokenize text in
    (* Limit cache size to prevent memory bloat *)
    if Hashtbl.length token_cache < token_cache_max_size () then
      Hashtbl.add token_cache hash tokens;
    tokens

(** Clear token cache *)
let clear_token_cache () =
  Hashtbl.clear token_cache

(** Calculate Jaccard similarity between two token sets.
    @return float in [0.0, 1.0]
    Invariant: |intersection| ≤ |union|, so result ≤ 1.0
    Edge case: both empty → 1.0 (identical) *)
let jaccard_similarity tokens1 tokens2 =
  let set1 = List.sort_uniq String.compare tokens1 in
  let set2 = List.sort_uniq String.compare tokens2 in
  let intersection = List.filter (fun t -> List.mem t set2) set1 in
  let union_size = List.length set1 + List.length set2 - List.length intersection in
  if union_size = 0 then 1.0
  else float_of_int (List.length intersection) /. float_of_int union_size

(** P1 Fix: O(n) term frequency using Hashtbl instead of O(n²) List.assoc *)
let count_terms_fast tokens =
  let tbl = Hashtbl.create (List.length tokens) in
  List.iter (fun t ->
    let count = Hashtbl.find_opt tbl t |> Option.value ~default:0 in
    Hashtbl.replace tbl t (count + 1)
  ) tokens;
  tbl

(** Calculate cosine similarity using term frequency.
    @return float in [0.0, 1.0]
    Invariant: By Cauchy-Schwarz, dot(v1,v2) ≤ |v1|×|v2|
    Edge case: zero magnitude → 0.0 (no similarity) *)
let cosine_similarity tokens1 tokens2 =
  (* Build term frequency maps using O(n) Hashtbl *)
  let tf1 = count_terms_fast tokens1 in
  let tf2 = count_terms_fast tokens2 in

  (* Get all unique terms *)
  let all_terms = Hashtbl.create 32 in
  Hashtbl.iter (fun k _ -> Hashtbl.replace all_terms k ()) tf1;
  Hashtbl.iter (fun k _ -> Hashtbl.replace all_terms k ()) tf2;

  (* Calculate dot product and magnitudes *)
  let dot_product = ref 0.0 in
  Hashtbl.iter (fun term _ ->
    let v1 = Hashtbl.find_opt tf1 term |> Option.value ~default:0 |> float_of_int in
    let v2 = Hashtbl.find_opt tf2 term |> Option.value ~default:0 |> float_of_int in
    dot_product := !dot_product +. (v1 *. v2)
  ) all_terms;

  let mag1 = sqrt (Hashtbl.fold (fun _ c acc ->
    acc +. (float_of_int c ** 2.0)
  ) tf1 0.0) in

  let mag2 = sqrt (Hashtbl.fold (fun _ c acc ->
    acc +. (float_of_int c ** 2.0)
  ) tf2 0.0) in

  if mag1 = 0.0 || mag2 = 0.0 then 0.0
  else !dot_product /. (mag1 *. mag2)

(** Calculate text similarity (combined metric) *)
let text_similarity original received =
  (* P1 Fix: Use cached tokenization for repeated comparisons *)
  let tokens1 = tokenize_cached original in
  let tokens2 = tokenize_cached received in

  (* Use both Jaccard and Cosine, weighted average *)
  let jaccard = jaccard_similarity tokens1 tokens2 in
  let cosine = cosine_similarity tokens1 tokens2 in

  (* Weighted average: configurable via MASC_DRIFT_JACCARD_WEIGHT / MASC_DRIFT_COSINE_WEIGHT
     Invariant: jaccard ∈ [0,1], cosine ∈ [0,1] → combined ∈ [0,1] *)
  let w = Level2_config.Drift_guard.weights () in
  let combined = (jaccard *. w.jaccard) +. (cosine *. w.cosine) in
  (* P0-3: Validate final similarity to prevent NaN/Inf *)
  Validation.Safe_float.validate combined ~name:"text_similarity"

(** Detect drift type based on characteristics *)
let detect_drift_type ~original ~received ~similarity =
  let orig_len = String.length original in
  let recv_len = String.length received in
  let length_ratio = float_of_int recv_len /. float_of_int (max 1 orig_len) in

  if similarity >= 0.95 then
    None
  else if length_ratio < 0.5 then
    (* Significant content loss *)
    Factual
  else if length_ratio > 2.0 then
    (* Significant content addition - potential hallucination *)
    Semantic
  else if similarity < 0.7 then
    (* Major semantic change *)
    Semantic
  else
    (* Moderate changes - likely structural *)
    Structural

(** Verify handoff context *)
let verify_handoff ~original ~received ?threshold ()
    : verification_result =
  let threshold = Option.value threshold ~default:(default_threshold ()) in
  let similarity = text_similarity original received in

  if similarity >= threshold then
    Verified { similarity }
  else
    let drift_type = detect_drift_type ~original ~received ~similarity in
    let details = match drift_type with
      | Factual -> "Significant content loss detected"
      | Semantic -> "Meaning may have changed substantially"
      | Structural -> "Format/structure modified"
      | None -> "Minor variations within tolerance"
    in
    Drift_detected { similarity; drift_type; details }

(** Drift type to string *)
let drift_type_to_string = function
  | Semantic -> "semantic"
  | Factual -> "factual"
  | Structural -> "structural"
  | None -> "none"

(** Drift type from string *)
let drift_type_of_string = function
  | "semantic" -> Semantic
  | "factual" -> Factual
  | "structural" -> Structural
  | _ -> None

(** {1 Drift Auto-Correction (R1 Research)} *)

(** Correction result *)
type correction_result =
  | Corrected of { context: string; method_used: string }
  | Needs_clarification of { question: string }
  | Use_original of { reason: string }

(** Correct semantic drift by re-summarizing *)
let correct_semantic_drift ~original ~received : correction_result =
  (* Strategy: Use original as authoritative source *)
  let original_len = String.length original in
  let received_len = String.length received in
  if original_len > received_len * 2 then
    (* Received is too short, might have lost info *)
    Use_original { reason = "Received context significantly shorter than original" }
  else if received_len > original_len * 2 then
    (* Received expanded too much, possible hallucination *)
    Corrected {
      context = original;
      method_used = "reverted_to_original_due_to_expansion"
    }
  else
    (* Similar length but different meaning - need clarification *)
    Needs_clarification {
      question = Printf.sprintf
        "Context drift detected. Original: %d chars, Received: %d chars. Which version is authoritative?"
        original_len received_len
    }

(** Request clarification for factual drift *)
let request_clarification ~original ~received : correction_result =
  let orig_preview = if String.length original > 100
    then String.sub original 0 100 ^ "..."
    else original in
  let recv_preview = if String.length received > 100
    then String.sub received 0 100 ^ "..."
    else received in
  Needs_clarification {
    question = Printf.sprintf
      "Factual drift detected.\nOriginal: %s\nReceived: %s\nPlease confirm the correct version."
      orig_preview recv_preview
  }

(** Reformat structural drift *)
let reformat_context ~original ~received:_ : correction_result =
  (* For structural drift, original format is preferred *)
  Corrected {
    context = original;
    method_used = "preserved_original_structure"
  }

(** Handle drift with auto-correction (R1: Error Propagation Guard) *)
let handle_drift ~original ~received (result : verification_result)
    : (string, correction_result) result =
  match result with
  | Verified { similarity = _ } ->
    Ok received  (* No drift, use received context *)
  | Drift_detected { drift_type; similarity; _ } ->
    Log.warn ~ctx:"drift_guard"
      "Drift detected (%.2f similarity, type: %s), attempting correction"
      similarity (drift_type_to_string drift_type);
    let correction = match drift_type with
      | Semantic -> correct_semantic_drift ~original ~received
      | Factual -> request_clarification ~original ~received
      | Structural -> reformat_context ~original ~received
      | None -> Corrected { context = received; method_used = "minor_drift_accepted" }
    in
    match correction with
    | Corrected { context; method_used } ->
      Log.info ~ctx:"drift_guard" "Drift corrected using: %s" method_used;
      Ok context
    | _ ->
      Error correction

(** Verify and auto-correct in one step *)
let verify_and_correct ~original ~received ?threshold ()
    : (string, correction_result) result =
  let result = verify_handoff ~original ~received ?threshold () in
  handle_drift ~original ~received result

(** Generate drift event ID *)
let generate_id () =
  let timestamp = Unix.gettimeofday () in
  let random = Random.int 100000 in
  Printf.sprintf "drift-%d-%05d" (int_of_float (timestamp *. 1000.)) random

(** Log drift event *)
let log_drift config ~from_agent ~to_agent ~task_id ~original ~received ~result
    : unit Lwt.t =
  let similarity, drift_type, verified = match result with
    | Verified { similarity } -> (similarity, None, true)
    | Drift_detected { similarity; drift_type; _ } -> (similarity, drift_type, false)
  in
  let event = {
    id = generate_id ();
    timestamp = Unix.gettimeofday ();
    from_agent;
    to_agent;
    task_id;
    original_length = String.length original;
    received_length = String.length received;
    similarity;
    drift_type;
    verified;
  } in
  let json = `Assoc [
    ("id", `String event.id);
    ("timestamp", `Float event.timestamp);
    ("from_agent", `String event.from_agent);
    ("to_agent", `String event.to_agent);
    ("task_id", `String event.task_id);
    ("original_length", `Int event.original_length);
    ("received_length", `Int event.received_length);
    ("similarity", `Float event.similarity);
    ("drift_type", `String (drift_type_to_string event.drift_type));
    ("verified", `Bool event.verified);
  ] in
  let line = Yojson.Safe.to_string json ^ "\n" in
  let file = drift_log_file config in
  (* Security: 0o600 - only owner can read/write drift log *)
  Lwt_io.with_file
    ~flags:[Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT]
    ~perm:0o600
    ~mode:Lwt_io.Output
    file
    (fun oc -> Lwt_io.write oc line)

(** Verify and log (combined operation) *)
let verify_and_log config ~from_agent ~to_agent ~task_id ~original ~received
    ?threshold () : verification_result Lwt.t =
  let result = verify_handoff ~original ~received ?threshold () in
  let* () = log_drift config ~from_agent ~to_agent ~task_id ~original ~received ~result in
  Lwt.return result

(** Get recent drift events *)
let get_recent_drifts config ~days : drift_event list Lwt.t =
  let file = drift_log_file config in
  if not (Sys.file_exists file) then
    Lwt.return []
  else
    let* content = Lwt_io.with_file ~mode:Lwt_io.Input file Lwt_io.read in
    let now = Unix.gettimeofday () in
    let cutoff = now -. (float_of_int days *. 86400.0) in
    let lines = String.split_on_char '\n' content
      |> List.filter (fun s -> String.trim s <> "") in
    let events = List.filter_map (fun line ->
      try
        let open Yojson.Safe.Util in
        let json = Yojson.Safe.from_string line in
        let timestamp = json |> member "timestamp" |> to_float in
        if timestamp < cutoff then None
        else Some {
          id = json |> member "id" |> to_string;
          timestamp;
          from_agent = json |> member "from_agent" |> to_string;
          to_agent = json |> member "to_agent" |> to_string;
          task_id = json |> member "task_id" |> to_string;
          original_length = json |> member "original_length" |> to_int;
          received_length = json |> member "received_length" |> to_int;
          similarity = json |> member "similarity" |> to_float;
          drift_type = json |> member "drift_type" |> to_string |> drift_type_of_string;
          verified = json |> member "verified" |> to_bool;
        }
      with exn ->
        let preview = if String.length line > 50 then String.sub line 0 50 ^ "..." else line in
        Printf.eprintf "[drift_guard] Failed to parse drift event: %s (line: %s)\n"
          (Printexc.to_string exn) preview;
        None
    ) lines in
    Lwt.return events

(** Get drift statistics *)
let get_drift_stats config ~days : (int * int * float) Lwt.t =
  let* events = get_recent_drifts config ~days in
  let total = List.length events in
  let drifts = List.filter (fun e -> not e.verified) events in
  let drift_count = List.length drifts in
  let avg_similarity = if total = 0 then 1.0
    else (List.fold_left (fun acc e -> acc +. e.similarity) 0.0 events) /. float_of_int total
  in
  Lwt.return (total, drift_count, avg_similarity)
