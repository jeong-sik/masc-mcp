(** Compression Dictionary for Small Messages - v5 Unified

    This module now delegates to lib/compact-protocol (Compact Protocol v5).
    Maintains backward-compatible API for existing MASC code.

    @see lib/compact-protocol/compact.ml for implementation
    @since 2026-01-13 v5.0 unified
*)

(** Module alias for convenience *)
module CP = Compact_protocol.Compact

(** {1 Dictionary Access - Delegates to Compact_protocol} *)

(** Get dictionary for compression *)
let get_dict () : string = CP.get_embedded_dict ()

(** Check if dictionary is available *)
let has_dict () : bool = CP.has_embedded_dict ()

(** {1 Size Thresholds} *)

(** Minimum message size for dictionary compression *)
let min_dict_size = CP.min_compress_size

(** Maximum message size for dictionary compression *)
let max_dict_size = CP.max_dict_size

(** Should use dictionary for this message size? *)
let should_use_dict = CP.should_use_dict

(** {1 Compression - Wrapper around Compact_protocol} *)

(** Compress with dictionary if beneficial

    @param data The data to compress
    @return (compressed_data, used_dict, did_compress) *)
let compress ?(level = 3) (data : string) : string * bool * bool =
  let orig_size = String.length data in
  let compressed, used_dict = CP.compress ~level data in
  let did_compress = String.length compressed < orig_size in
  (compressed, used_dict, did_compress)

(** Decompress with optional dictionary

    @param data The compressed data
    @param orig_size Original size of uncompressed data
    @param used_dict Whether dictionary was used for compression
    @return Decompressed data *)
let decompress ~orig_size ~(used_dict : bool) (data : string) : string =
  let dict = if used_dict then Some (get_dict ()) else None in
  try
    match dict with
    | Some d -> Zstd.decompress orig_size ~dict:d data
    | None -> Zstd.decompress orig_size data
  with _ ->
    (* Fallback: try Compact Protocol auto-detect path *)
    CP.decompress data

(** {1 Content-Encoding Headers} *)

(** Content-Encoding header value for dictionary compression *)
let encoding_with_dict = "zstd-dict"

(** Content-Encoding header value for standard compression *)
let encoding_standard = "zstd"

(** {1 Version Info} *)

(** Protocol version - now unified with Compact_protocol *)
let version = CP.version
let version_string = CP.version_string
