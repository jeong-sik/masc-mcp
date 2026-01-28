(** Backend Eio Module Coverage Tests

    Tests for backend compression utilities:
    - Compression.min_size constant
    - Compression.default_level constant
    - Compression.magic constant
    - Compression.magic_dict constant
*)

open Alcotest

module Backend_eio = Masc_mcp.Backend_eio

(* ============================================================
   Compression Constants Tests
   ============================================================ *)

let test_compression_min_size () =
  check int "min_size" 32 Backend_eio.Compression.min_size

let test_compression_default_level () =
  check int "default_level" 3 Backend_eio.Compression.default_level

let test_compression_magic () =
  check string "magic" "ZSTD" Backend_eio.Compression.magic

let test_compression_magic_dict () =
  check string "magic_dict" "ZSTDD" Backend_eio.Compression.magic_dict

let test_compression_magic_lengths () =
  check int "magic length" 4 (String.length Backend_eio.Compression.magic);
  check int "magic_dict length" 5 (String.length Backend_eio.Compression.magic_dict)

(* ============================================================
   Test Runners
   ============================================================ *)

let () =
  run "Backend Eio Coverage" [
    "compression_constants", [
      test_case "min_size" `Quick test_compression_min_size;
      test_case "default_level" `Quick test_compression_default_level;
      test_case "magic" `Quick test_compression_magic;
      test_case "magic_dict" `Quick test_compression_magic_dict;
      test_case "magic lengths" `Quick test_compression_magic_lengths;
    ];
  ]
