(** Test suite for Compact Protocol v4 Compression

    Tests compression functionality across MASC-MCP modules:
    - Backend_eio.Compression (storage layer)
    - Datachannel.Compression (WebRTC layer)
    - Http_server_eio.Compression (HTTP layer - tested separately)

    Expected Results:
    - 60-70% compression ratio for text/JSON data
    - Roundtrip integrity preservation
    - Transparent decompression via ZSTD header detection
*)

(* ===== Backend_eio Compression Tests ===== *)

module BackendCompression = Masc_mcp.Backend_eio.Compression

let test_backend_compress_skip_small () =
  let small = "tiny" in  (* <32 bytes, below min_dict_size *)
  let (result, _used_dict, did_compress) = BackendCompression.compress small in
  Alcotest.(check bool) "small data not compressed" false did_compress;
  Alcotest.(check string) "data unchanged" small result

let test_backend_compress_large () =
  let large = String.make 1000 'x' in  (* Highly compressible *)
  let (result, _used_dict, did_compress) = BackendCompression.compress large in
  Alcotest.(check bool) "large data compressed" true did_compress;
  Alcotest.(check bool) "smaller than original" true (String.length result < 1000)

let test_backend_roundtrip () =
  let original = String.init 500 (fun i -> Char.chr (65 + (i mod 26))) in
  let compressed = BackendCompression.compress_with_header original in
  let decompressed = BackendCompression.decompress_auto compressed in
  Alcotest.(check string) "roundtrip preserves data" original decompressed

let test_backend_header_format () =
  let data = String.make 512 'Z' in
  let compressed = BackendCompression.compress_with_header data in
  (* Check magic header (9-byte format: ZSTD\x00 or ZSTDD + 4-byte size) *)
  let magic4 = String.sub compressed 0 4 in
  Alcotest.(check string) "ZSTD magic prefix" "ZSTD" magic4;
  (* Fifth byte: \x00 for standard, 'D' for dictionary *)
  let is_dict = compressed.[4] = 'D' in
  let magic5 = if is_dict then "ZSTDD" else "ZSTD\x00" in
  Alcotest.(check string) "5-byte magic" magic5 (String.sub compressed 0 5);
  (* Check original size stored in bytes 5-8 (big endian) *)
  let orig_size =
    (Char.code compressed.[5] lsl 24) lor
    (Char.code compressed.[6] lsl 16) lor
    (Char.code compressed.[7] lsl 8) lor
    Char.code compressed.[8]
  in
  Alcotest.(check int) "original size in header" 512 orig_size

let test_backend_non_compressed_passthrough () =
  let plain = "This is plain text without ZSTD header" in
  let result = BackendCompression.decompress_auto plain in
  Alcotest.(check string) "non-compressed unchanged" plain result

let test_backend_json_compression () =
  (* Need larger JSON (>256 bytes) to trigger compression *)
  let json_parts = List.init 10 (fun i ->
    Printf.sprintf {|{"id":%d,"type":"agent_response","status":"ok","timestamp":%d}|} i (1234567890 + i)
  ) in
  let json = "[" ^ String.concat "," json_parts ^ "]" in
  let len = String.length json in
  if len > 256 then begin
    let compressed = BackendCompression.compress_with_header json in
    let ratio = (float_of_int (String.length compressed)) /. (float_of_int len) in
    Alcotest.(check bool) "JSON compresses <85%" true (ratio < 0.85);
    let decompressed = BackendCompression.decompress_auto compressed in
    Alcotest.(check string) "JSON roundtrip" json decompressed
  end else
    Alcotest.(check pass) "JSON too short for compression" () ()

let backend_tests = [
  "skip small data", `Quick, test_backend_compress_skip_small;
  "compress large data", `Quick, test_backend_compress_large;
  "roundtrip", `Quick, test_backend_roundtrip;
  "ZSTD header format", `Quick, test_backend_header_format;
  "non-compressed passthrough", `Quick, test_backend_non_compressed_passthrough;
  "JSON compression ratio", `Quick, test_backend_json_compression;
]

(* ===== DataChannel Compression Tests ===== *)

module DataChannelCompression = Masc_mcp.Datachannel.Compression

let test_dc_compress_skip_small () =
  let small = Bytes.of_string "tiny" in
  let (result, compressed) = DataChannelCompression.compress small in
  Alcotest.(check bool) "small data not compressed" false compressed;
  Alcotest.(check bytes) "data unchanged" small result

let test_dc_compress_large () =
  let large = Bytes.make 500 'x' in  (* Highly compressible *)
  let (result, compressed) = DataChannelCompression.compress large in
  Alcotest.(check bool) "large data compressed" true compressed;
  Alcotest.(check bool) "smaller than original" true (Bytes.length result < 500)

let test_dc_roundtrip () =
  let original = Bytes.init 300 (fun i -> Char.chr (65 + (i mod 26))) in
  let compressed = DataChannelCompression.compress_with_header original in
  let decompressed = DataChannelCompression.decompress_auto compressed in
  Alcotest.(check bytes) "roundtrip preserves data" original decompressed

let test_dc_header_format () =
  let data = Bytes.make 256 'Z' in
  let compressed = DataChannelCompression.compress_with_header data in
  (* Check ZSTD magic header *)
  let magic = Bytes.sub_string compressed 0 4 in
  Alcotest.(check string) "ZSTD magic" "ZSTD" magic;
  (* Check original size stored in bytes 4-7 (big endian) *)
  let orig_size =
    (Char.code (Bytes.get compressed 4) lsl 24) lor
    (Char.code (Bytes.get compressed 5) lsl 16) lor
    (Char.code (Bytes.get compressed 6) lsl 8) lor
    Char.code (Bytes.get compressed 7)
  in
  Alcotest.(check int) "original size in header" 256 orig_size

let test_dc_non_compressed_passthrough () =
  let plain = Bytes.of_string "Plain bytes without ZSTD header" in
  let result = DataChannelCompression.decompress_auto plain in
  Alcotest.(check bytes) "non-compressed unchanged" plain result

let test_dc_binary_roundtrip () =
  (* Test with binary data including null bytes *)
  let binary = Bytes.init 200 (fun i -> Char.chr (i mod 256)) in
  let compressed = DataChannelCompression.compress_with_header binary in
  let decompressed = DataChannelCompression.decompress_auto compressed in
  Alcotest.(check bytes) "binary roundtrip" binary decompressed

let datachannel_tests = [
  "skip small data", `Quick, test_dc_compress_skip_small;
  "compress large data", `Quick, test_dc_compress_large;
  "roundtrip", `Quick, test_dc_roundtrip;
  "ZSTD header format", `Quick, test_dc_header_format;
  "non-compressed passthrough", `Quick, test_dc_non_compressed_passthrough;
  "binary roundtrip", `Quick, test_dc_binary_roundtrip;
]

(* ===== Compression Threshold Tests ===== *)

let test_threshold_backend () =
  (* Now uses dictionary compression with lower threshold *)
  Alcotest.(check int) "backend min_size" 32 BackendCompression.min_size

let test_threshold_datachannel () =
  Alcotest.(check int) "datachannel min_size" 128 DataChannelCompression.min_size

let test_default_level () =
  Alcotest.(check int) "backend level" 3 BackendCompression.default_level;
  Alcotest.(check int) "datachannel level" 3 DataChannelCompression.default_level

let threshold_tests = [
  "backend min_size", `Quick, test_threshold_backend;
  "datachannel min_size", `Quick, test_threshold_datachannel;
  "default compression level", `Quick, test_default_level;
]

(* ===== Compression Ratio Benchmarks ===== *)

let test_compression_ratio_text () =
  let text = String.concat "" (List.init 20 (fun _ ->
    "The quick brown fox jumps over the lazy dog. "
  )) in
  let compressed = BackendCompression.compress_with_header text in
  let ratio = (float_of_int (String.length compressed)) /. (float_of_int (String.length text)) in
  (* Expect at least 50% compression for repetitive text *)
  Alcotest.(check bool) "text compresses >50%" true (ratio < 0.5)

let test_compression_ratio_json () =
  let json = String.concat "," (List.init 50 (fun i ->
    Printf.sprintf {|{"id":%d,"name":"item_%d","value":%d}|} i i (i * 100)
  )) in
  let full_json = "[" ^ json ^ "]" in
  let compressed = BackendCompression.compress_with_header full_json in
  let ratio = (float_of_int (String.length compressed)) /. (float_of_int (String.length full_json)) in
  (* Expect at least 60% compression for JSON *)
  Alcotest.(check bool) "JSON compresses >60%" true (ratio < 0.4)

let test_incompressible_data () =
  (* Already-compressed data won't compress further *)
  let seed = "ZSTD header already present means this won't compress" in
  let already_compressed = BackendCompression.compress_with_header (String.make 500 'x') in
  (* Try to compress the already-compressed data *)
  let (result, _used_dict, did_compress) = BackendCompression.compress already_compressed in
  (* Already-compressed data should NOT compress further (or compress poorly) *)
  if did_compress then begin
    let ratio = (float_of_int (String.length result)) /. (float_of_int (String.length already_compressed)) in
    (* If it did compress, it should be marginal - allow up to 95% *)
    Alcotest.(check bool) "pre-compressed data doesn't shrink much" true (ratio > 0.9 || String.length already_compressed < 32)
  end else begin
    ignore seed;
    Alcotest.(check pass) "pre-compressed data not compressed further" () ()
  end

let ratio_tests = [
  "text compression ratio", `Quick, test_compression_ratio_text;
  "JSON compression ratio", `Quick, test_compression_ratio_json;
  "incompressible data", `Quick, test_incompressible_data;
]

(* ===== Test Entry Point ===== *)

let () =
  Alcotest.run "Compression" [
    "Backend_eio", backend_tests;
    "DataChannel", datachannel_tests;
    "Thresholds", threshold_tests;
    "Compression Ratios", ratio_tests;
  ]
