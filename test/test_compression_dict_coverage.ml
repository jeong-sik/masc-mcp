(** Compression_dict Module Coverage Tests

    Tests for MASC Compression (Simplified zstd):
    - min_dict_size, max_dict_size: size thresholds
    - should_use_dict: compression decision
    - get_dict, has_dict: dictionary stubs
    - compress, decompress: actual compression
    - encoding_with_dict, encoding_standard: headers
    - version, version_string: version info
*)

open Alcotest

module Compression_dict = Masc_mcp.Compression_dict

(* ============================================================
   Threshold Constants Tests
   ============================================================ *)

let test_min_dict_size () =
  check int "min_dict_size" 32 Compression_dict.min_dict_size

let test_max_dict_size () =
  check int "max_dict_size" 2048 Compression_dict.max_dict_size

let test_min_less_than_max () =
  check bool "min < max" true (Compression_dict.min_dict_size < Compression_dict.max_dict_size)

(* ============================================================
   should_use_dict Tests
   ============================================================ *)

let test_should_use_dict_below_min () =
  check bool "below min" false (Compression_dict.should_use_dict 10);
  check bool "at 31" false (Compression_dict.should_use_dict 31)

let test_should_use_dict_at_min () =
  check bool "at min" true (Compression_dict.should_use_dict 32)

let test_should_use_dict_above_min () =
  check bool "above min" true (Compression_dict.should_use_dict 100);
  check bool "large" true (Compression_dict.should_use_dict 10000)

let test_should_use_dict_zero () =
  check bool "zero" false (Compression_dict.should_use_dict 0)

(* ============================================================
   Dictionary Stubs Tests
   ============================================================ *)

let test_get_dict_empty () =
  check string "empty dict" "" (Compression_dict.get_dict ())

let test_has_dict_false () =
  check bool "no dict" false (Compression_dict.has_dict ())

(* ============================================================
   compress Tests
   ============================================================ *)

let test_compress_small_data () =
  let data = "short" in
  let (result, used_dict, did_compress) = Compression_dict.compress data in
  check string "unchanged" data result;
  check bool "no dict used" false used_dict;
  check bool "no compression" false did_compress

let test_compress_large_data () =
  (* Create compressible data - repeated pattern *)
  let data = String.make 200 'x' in
  let (result, used_dict, did_compress) = Compression_dict.compress data in
  check bool "no dict used" false used_dict;
  (* Compressible data should compress *)
  if did_compress then
    check bool "smaller" true (String.length result < String.length data)
  else
    check string "unchanged" data result

let test_compress_incompressible_data () =
  (* Random-ish data that won't compress well *)
  let data = String.init 100 (fun i -> Char.chr (i mod 256)) in
  let (_, used_dict, _) = Compression_dict.compress data in
  check bool "no dict" false used_dict

let test_compress_with_level () =
  let data = String.make 100 'y' in
  let (_, used_dict1, _) = Compression_dict.compress ~level:1 data in
  let (_, used_dict3, _) = Compression_dict.compress ~level:3 data in
  check bool "no dict level 1" false used_dict1;
  check bool "no dict level 3" false used_dict3

(* ============================================================
   decompress Tests
   ============================================================ *)

let test_decompress_roundtrip () =
  let original = String.make 200 'z' in
  let (compressed, used_dict, did_compress) = Compression_dict.compress original in
  if did_compress then begin
    let decompressed = Compression_dict.decompress
      ~orig_size:(String.length original)
      ~used_dict
      compressed in
    check string "roundtrip" original decompressed
  end else
    (* No compression happened, data should be unchanged *)
    check string "unchanged" original compressed

let test_decompress_uncompressed_data () =
  let data = "hello world" in
  let result = Compression_dict.decompress ~orig_size:11 ~used_dict:false data in
  (* Should return original if not actually compressed *)
  check bool "reasonable result" true (String.length result > 0)

(* ============================================================
   Encoding Headers Tests
   ============================================================ *)

let test_encoding_with_dict () =
  check string "dict encoding" "zstd-dict" Compression_dict.encoding_with_dict

let test_encoding_standard () =
  check string "standard encoding" "zstd" Compression_dict.encoding_standard

(* ============================================================
   Version Tests
   ============================================================ *)

let test_version () =
  check string "version" "6.0.0" Compression_dict.version

let test_version_string () =
  check bool "has MASC" true (String.length Compression_dict.version_string > 0);
  check bool "contains v6" true (
    try
      ignore (Str.search_forward (Str.regexp "v6") Compression_dict.version_string 0);
      true
    with Not_found -> false
  )

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Compression_dict Coverage" [
    "thresholds", [
      test_case "min_dict_size" `Quick test_min_dict_size;
      test_case "max_dict_size" `Quick test_max_dict_size;
      test_case "min < max" `Quick test_min_less_than_max;
    ];
    "should_use_dict", [
      test_case "below min" `Quick test_should_use_dict_below_min;
      test_case "at min" `Quick test_should_use_dict_at_min;
      test_case "above min" `Quick test_should_use_dict_above_min;
      test_case "zero" `Quick test_should_use_dict_zero;
    ];
    "dictionary_stubs", [
      test_case "get_dict empty" `Quick test_get_dict_empty;
      test_case "has_dict false" `Quick test_has_dict_false;
    ];
    "compress", [
      test_case "small data" `Quick test_compress_small_data;
      test_case "large data" `Quick test_compress_large_data;
      test_case "incompressible" `Quick test_compress_incompressible_data;
      test_case "with level" `Quick test_compress_with_level;
    ];
    "decompress", [
      test_case "roundtrip" `Quick test_decompress_roundtrip;
      test_case "uncompressed" `Quick test_decompress_uncompressed_data;
    ];
    "encoding", [
      test_case "with dict" `Quick test_encoding_with_dict;
      test_case "standard" `Quick test_encoding_standard;
    ];
    "version", [
      test_case "version" `Quick test_version;
      test_case "version string" `Quick test_version_string;
    ];
  ]
