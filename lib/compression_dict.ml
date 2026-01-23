(** Compression for Small Messages - Simplified

    Plain zstd compression without external dictionary dependency.
    Maintains backward-compatible API for existing MASC code.

    @since 2026-01-23 v6.0 simplified (removed compact-protocol dependency)
*)

(** {1 Size Thresholds} *)

(** Minimum message size for compression *)
let min_dict_size = 32

(** Maximum message size for dictionary compression (not used in simplified version) *)
let max_dict_size = 2048

(** Should compress this message size? *)
let should_use_dict (size : int) : bool =
  size >= min_dict_size

(** {1 Dictionary Stubs} *)

(** Get dictionary - returns empty (no dictionary in simplified version) *)
let get_dict () : string = ""

(** Check if dictionary is available - always false in simplified version *)
let has_dict () : bool = false

(** {1 Compression} *)

(** Compress data if beneficial

    @param data The data to compress
    @return (compressed_data, used_dict, did_compress)
    Note: used_dict is always false in simplified version *)
let compress ?(level = 3) (data : string) : string * bool * bool =
  let orig_size = String.length data in
  if orig_size < min_dict_size then
    (data, false, false)
  else
    try
      let compressed = Zstd.compress ~level data in
      if String.length compressed < orig_size then
        (compressed, false, true)
      else
        (data, false, false)
    with _ -> (data, false, false)

(** Decompress data

    @param data The compressed data
    @param orig_size Original size of uncompressed data
    @param used_dict Ignored in simplified version
    @return Decompressed data *)
let decompress ~orig_size ~(used_dict : bool) (data : string) : string =
  let _ = used_dict in  (* Ignored - no dictionary *)
  try
    Zstd.decompress orig_size data
  with _ -> data

(** {1 Content-Encoding Headers} *)

(** Content-Encoding header value for dictionary compression *)
let encoding_with_dict = "zstd-dict"

(** Content-Encoding header value for standard compression *)
let encoding_standard = "zstd"

(** {1 Version Info} *)

let version = "6.0.0"
let version_string = "MASC Compression v6.0 (simplified)"
