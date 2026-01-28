(** Miscellaneous Module Coverage Tests

    Tests for smaller utility modules:
    - Compression_dict: zstd compression for small messages
    - Log: logging utilities
    - Config: configuration handling
    - Env_config: environment configuration
*)

open Alcotest

module Compression_dict = Masc_mcp.Compression_dict
module Log = Masc_mcp.Log
module Config = Masc_mcp.Config
module Env_config = Masc_mcp.Env_config
module Mode = Masc_mcp.Mode

(* ============================================================
   Compression_dict Tests
   ============================================================ *)

let test_min_dict_size () =
  check int "min_dict_size" 32 Compression_dict.min_dict_size

let test_max_dict_size () =
  check int "max_dict_size" 2048 Compression_dict.max_dict_size

let test_should_use_dict_below_threshold () =
  check bool "size 0" false (Compression_dict.should_use_dict 0);
  check bool "size 10" false (Compression_dict.should_use_dict 10);
  check bool "size 31" false (Compression_dict.should_use_dict 31)

let test_should_use_dict_at_threshold () =
  check bool "size 32" true (Compression_dict.should_use_dict 32)

let test_should_use_dict_above_threshold () =
  check bool "size 100" true (Compression_dict.should_use_dict 100);
  check bool "size 1000" true (Compression_dict.should_use_dict 1000)

let test_get_dict_empty () =
  check string "empty dict" "" (Compression_dict.get_dict ())

let test_has_dict_false () =
  check bool "no dict" false (Compression_dict.has_dict ())

let test_compress_small_data () =
  let data = "hi" in
  let compressed, used_dict, did_compress = Compression_dict.compress data in
  check string "unchanged data" data compressed;
  check bool "no dict used" false used_dict;
  check bool "no compression" false did_compress

let test_compress_larger_data () =
  (* Create compressible data (repeated pattern) *)
  let data = String.make 1000 'a' in
  let compressed, used_dict, did_compress = Compression_dict.compress data in
  check bool "no dict used" false used_dict;
  (* Repeated data should compress well *)
  if did_compress then
    check bool "compressed smaller" true (String.length compressed < String.length data)
  else
    (* If not compressed, at least the data is unchanged *)
    check string "unchanged" data compressed

let test_compress_with_level () =
  let data = String.make 500 'x' in
  let c1, _, _ = Compression_dict.compress ~level:1 data in
  let c9, _, _ = Compression_dict.compress ~level:9 data in
  (* Higher compression level should generally produce smaller or equal output *)
  check bool "level 9 <= level 1" true (String.length c9 <= String.length c1 + 10)

let test_decompress_roundtrip () =
  let data = String.make 100 'z' ^ String.make 100 'y' in
  let compressed, _, did_compress = Compression_dict.compress data in
  if did_compress then begin
    let decompressed = Compression_dict.decompress ~orig_size:(String.length data) ~used_dict:false compressed in
    check string "roundtrip" data decompressed
  end else
    (* Data wasn't compressed, nothing to decompress *)
    ()

let test_decompress_with_orig_size () =
  let data = "This is a test string that we will compress and it needs to be long enough" in
  let orig_size = String.length data in
  let compressed, _, did_compress = Compression_dict.compress data in
  if did_compress then begin
    let decompressed = Compression_dict.decompress ~orig_size ~used_dict:false compressed in
    check int "correct size" orig_size (String.length decompressed)
  end else
    ()

let test_encoding_constants () =
  check string "encoding_with_dict" "zstd-dict" Compression_dict.encoding_with_dict;
  check string "encoding_standard" "zstd" Compression_dict.encoding_standard

let test_version_info () =
  check string "version" "6.0.0" Compression_dict.version;
  check bool "version_string non-empty" true (String.length Compression_dict.version_string > 0)

(* ============================================================
   Log Tests
   ============================================================ *)

let test_log_level_to_string () =
  check string "debug" "DEBUG" (Log.level_to_string Log.Debug);
  check string "info" "INFO" (Log.level_to_string Log.Info);
  check string "warn" "WARN" (Log.level_to_string Log.Warn);
  check string "error" "ERROR" (Log.level_to_string Log.Error)

let test_log_level_of_string () =
  check bool "debug" true (Log.level_of_string "DEBUG" = Log.Debug);
  check bool "info" true (Log.level_of_string "INFO" = Log.Info);
  check bool "warn" true (Log.level_of_string "WARN" = Log.Warn);
  check bool "warning" true (Log.level_of_string "WARNING" = Log.Warn);
  check bool "error" true (Log.level_of_string "ERROR" = Log.Error);
  check bool "unknown" true (Log.level_of_string "unknown" = Log.Info) (* default *)

let test_log_level_of_string_lowercase () =
  check bool "debug lower" true (Log.level_of_string "debug" = Log.Debug);
  check bool "info lower" true (Log.level_of_string "info" = Log.Info);
  check bool "warn lower" true (Log.level_of_string "warn" = Log.Warn)

let test_log_level_to_int () =
  check bool "debug < info" true (Log.level_to_int Log.Debug < Log.level_to_int Log.Info);
  check bool "info < warn" true (Log.level_to_int Log.Info < Log.level_to_int Log.Warn);
  check bool "warn < error" true (Log.level_to_int Log.Warn < Log.level_to_int Log.Error)

let test_log_should_log () =
  Log.set_level Log.Info;
  check bool "info logged at info" true (Log.should_log Log.Info);
  check bool "warn logged at info" true (Log.should_log Log.Warn);
  check bool "error logged at info" true (Log.should_log Log.Error);
  check bool "debug not logged at info" false (Log.should_log Log.Debug)

let test_log_set_level () =
  Log.set_level Log.Error;
  check bool "debug not logged" false (Log.should_log Log.Debug);
  check bool "info not logged" false (Log.should_log Log.Info);
  check bool "warn not logged" false (Log.should_log Log.Warn);
  check bool "error logged" true (Log.should_log Log.Error);
  Log.set_level Log.Info (* Reset to default *)

let test_log_set_level_from_string () =
  Log.set_level_from_string "debug";
  check bool "debug logged" true (Log.should_log Log.Debug);
  Log.set_level_from_string "error";
  check bool "debug not logged" false (Log.should_log Log.Debug);
  Log.set_level Log.Info (* Reset to default *)

let test_log_timestamp () =
  let ts = Log.timestamp () in
  (* Format: YYYY-MM-DD HH:MM:SS *)
  check bool "length >= 19" true (String.length ts >= 19);
  check bool "contains dash" true (String.contains ts '-');
  check bool "contains colon" true (String.contains ts ':')

(* ============================================================
   Config Tests
   ============================================================ *)

let test_config_default () =
  let c = Config.default in
  check bool "mode is Standard" true (c.mode = Mode.Standard)

let test_config_to_json () =
  let c = Config.default in
  let json = Config.to_json c in
  match json with
  | `Assoc fields ->
    check bool "has mode" true (List.mem_assoc "mode" fields);
    check bool "has enabled_categories" true (List.mem_assoc "enabled_categories" fields)
  | _ -> fail "expected object"

let test_config_of_json () =
  let json = `Assoc [
    ("mode", `String "standard");
  ] in
  let c = Config.of_json json in
  check bool "mode is Standard" true (c.mode = Mode.Standard)

let test_config_of_json_custom () =
  let json = `Assoc [
    ("mode", `String "custom");
    ("enabled_categories", `List [`String "room"; `String "task"]);
  ] in
  let c = Config.of_json json in
  check bool "mode is Custom" true (c.mode = Mode.Custom)

let test_config_of_json_invalid () =
  let json = `String "invalid" in
  let c = Config.of_json json in
  (* Should return default on invalid input *)
  check bool "mode is Standard" true (c.mode = Mode.Standard)

let test_config_filename () =
  check string "config filename" "config.json" Config.config_filename

(* ============================================================
   Env_config Tests
   ============================================================ *)

let test_env_zombie_threshold () =
  let threshold = Env_config.Zombie.threshold_seconds in
  check bool "positive threshold" true (threshold > 0.0)

let test_env_zombie_cleanup_interval () =
  let interval = Env_config.Zombie.cleanup_interval_seconds in
  check bool "positive interval" true (interval > 0.0)

let test_env_lock_timeout () =
  let timeout = Env_config.Lock.timeout_seconds in
  check bool "positive timeout" true (timeout > 0.0)

let test_env_lock_expiry_warning () =
  let warning = Env_config.Lock.expiry_warning_seconds in
  check bool "positive warning" true (warning > 0.0)

let test_env_session_max_age () =
  let max_age = Env_config.Session.max_age_seconds in
  check bool "positive max_age" true (max_age > 0.0)

let test_env_session_rate_limit () =
  let window = Env_config.Session.rate_limit_window_seconds in
  check bool "positive window" true (window > 0.0)

let test_env_tempo_min () =
  let min = Env_config.Tempo.min_interval_seconds in
  check bool "positive min" true (min > 0.0)

let test_env_tempo_max () =
  let max = Env_config.Tempo.max_interval_seconds in
  check bool "positive max" true (max > 0.0)

let test_env_tempo_default () =
  let default = Env_config.Tempo.default_interval_seconds in
  check bool "positive default" true (default > 0.0)

let test_env_tempo_ordering () =
  let min = Env_config.Tempo.min_interval_seconds in
  let max = Env_config.Tempo.max_interval_seconds in
  check bool "min <= max" true (min <= max)

let test_env_orchestrator_interval () =
  let interval = Env_config.Orchestrator.check_interval_seconds in
  check bool "positive interval" true (interval > 0.0)

let test_env_orchestrator_agent_name () =
  let name = Env_config.Orchestrator.agent_name in
  check bool "non-empty name" true (String.length name > 0)

let test_env_mitosis_interval () =
  let interval = Env_config.Mitosis.trigger_interval_seconds in
  check bool "positive interval" true (interval > 0.0)

let test_env_federation_timeout () =
  let timeout = Env_config.Federation.timeout_seconds in
  check bool "positive timeout" true (timeout > 0.0)

let test_env_cancellation_max_age () =
  let max_age = Env_config.Cancellation.token_max_age_seconds in
  check bool "positive max_age" true (max_age > 0.0)

let test_env_get_string () =
  let v = Env_config.get_string ~default:"fallback" "NONEXISTENT_VAR_12345" in
  check string "fallback value" "fallback" v

let test_env_get_int () =
  let v = Env_config.get_int ~default:42 "NONEXISTENT_VAR_12345" in
  check int "fallback value" 42 v

let test_env_get_float () =
  let v = Env_config.get_float ~default:3.14 "NONEXISTENT_VAR_12345" in
  check bool "fallback value" true (abs_float (v -. 3.14) < 0.001)

let test_env_get_bool () =
  let v = Env_config.get_bool ~default:true "NONEXISTENT_VAR_12345" in
  check bool "fallback value" true v

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Misc Coverage" [
    "compression_dict.thresholds", [
      test_case "min_dict_size" `Quick test_min_dict_size;
      test_case "max_dict_size" `Quick test_max_dict_size;
      test_case "should_use_dict below" `Quick test_should_use_dict_below_threshold;
      test_case "should_use_dict at" `Quick test_should_use_dict_at_threshold;
      test_case "should_use_dict above" `Quick test_should_use_dict_above_threshold;
    ];
    "compression_dict.dict", [
      test_case "get_dict empty" `Quick test_get_dict_empty;
      test_case "has_dict false" `Quick test_has_dict_false;
    ];
    "compression_dict.compress", [
      test_case "small data" `Quick test_compress_small_data;
      test_case "larger data" `Quick test_compress_larger_data;
      test_case "with level" `Quick test_compress_with_level;
    ];
    "compression_dict.decompress", [
      test_case "roundtrip" `Quick test_decompress_roundtrip;
      test_case "with orig_size" `Quick test_decompress_with_orig_size;
    ];
    "compression_dict.constants", [
      test_case "encoding constants" `Quick test_encoding_constants;
      test_case "version info" `Quick test_version_info;
    ];
    "log.level", [
      test_case "to_string" `Quick test_log_level_to_string;
      test_case "of_string" `Quick test_log_level_of_string;
      test_case "of_string lowercase" `Quick test_log_level_of_string_lowercase;
      test_case "to_int ordering" `Quick test_log_level_to_int;
    ];
    "log.logging", [
      test_case "should_log" `Quick test_log_should_log;
      test_case "set_level" `Quick test_log_set_level;
      test_case "set_level_from_string" `Quick test_log_set_level_from_string;
      test_case "timestamp" `Quick test_log_timestamp;
    ];
    "config", [
      test_case "default" `Quick test_config_default;
      test_case "to_json" `Quick test_config_to_json;
      test_case "of_json" `Quick test_config_of_json;
      test_case "of_json custom" `Quick test_config_of_json_custom;
      test_case "of_json invalid" `Quick test_config_of_json_invalid;
      test_case "filename" `Quick test_config_filename;
    ];
    "env_config.zombie", [
      test_case "threshold" `Quick test_env_zombie_threshold;
      test_case "cleanup_interval" `Quick test_env_zombie_cleanup_interval;
    ];
    "env_config.lock", [
      test_case "timeout" `Quick test_env_lock_timeout;
      test_case "expiry_warning" `Quick test_env_lock_expiry_warning;
    ];
    "env_config.session", [
      test_case "max_age" `Quick test_env_session_max_age;
      test_case "rate_limit" `Quick test_env_session_rate_limit;
    ];
    "env_config.tempo", [
      test_case "min" `Quick test_env_tempo_min;
      test_case "max" `Quick test_env_tempo_max;
      test_case "default" `Quick test_env_tempo_default;
      test_case "ordering" `Quick test_env_tempo_ordering;
    ];
    "env_config.orchestrator", [
      test_case "interval" `Quick test_env_orchestrator_interval;
      test_case "agent_name" `Quick test_env_orchestrator_agent_name;
    ];
    "env_config.mitosis", [
      test_case "interval" `Quick test_env_mitosis_interval;
    ];
    "env_config.federation", [
      test_case "timeout" `Quick test_env_federation_timeout;
    ];
    "env_config.cancellation", [
      test_case "max_age" `Quick test_env_cancellation_max_age;
    ];
    "env_config.helpers", [
      test_case "get_string" `Quick test_env_get_string;
      test_case "get_int" `Quick test_env_get_int;
      test_case "get_float" `Quick test_env_get_float;
      test_case "get_bool" `Quick test_env_get_bool;
    ];
  ]
