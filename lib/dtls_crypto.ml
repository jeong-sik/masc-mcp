(** DTLS Crypto - Real AES-GCM Encryption for DTLS 1.2

    Implements RFC 5288 (AES-GCM Cipher Suites for TLS)
    Used by WebRTC for secure data transmission.

    Cipher Suite: TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256

    @author Second Brain
    @since MASC v3.0
*)

(** {1 Types} *)

(** Cipher suite identifiers (RFC 5246) *)
type cipher_suite =
  | TLS_NULL_WITH_NULL_NULL               (* 0x0000 - no encryption *)
  | TLS_RSA_WITH_AES_128_GCM_SHA256       (* 0x009C *)
  | TLS_RSA_WITH_AES_256_GCM_SHA384       (* 0x009D *)
  | TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 (* 0xC02F - WebRTC default *)
  | TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384 (* 0xC030 *)

(** Key material for encryption/decryption *)
type key_material = {
  client_write_key: Cstruct.t;       (** 16 bytes for AES-128, 32 for AES-256 *)
  server_write_key: Cstruct.t;
  client_write_iv: Cstruct.t;        (** 4 bytes implicit IV *)
  server_write_iv: Cstruct.t;
}

(** Security parameters derived from handshake *)
type security_params = {
  cipher_suite: cipher_suite;
  master_secret: Cstruct.t;          (** 48 bytes *)
  client_random: Cstruct.t;          (** 32 bytes *)
  server_random: Cstruct.t;          (** 32 bytes *)
  key_material: key_material option;
}

(** Encryption context for a connection *)
type t = {
  mutable params: security_params option;
  is_client: bool;  (* Not mutable - set once at creation *)
  mutable read_seq: int64;
  mutable write_seq: int64;
}

(** {1 Constants} *)

let aes_128_key_size = 16
let aes_256_key_size = 32
let gcm_iv_size = 4        (* Implicit IV from key expansion *)
let gcm_nonce_explicit = 8 (* Explicit nonce in each record *)
let gcm_tag_size = 16      (* Authentication tag *)
let master_secret_size = 48
let random_size = 32

(** {1 Cipher Suite Utilities} *)

let cipher_suite_to_int = function
  | TLS_NULL_WITH_NULL_NULL -> 0x0000
  | TLS_RSA_WITH_AES_128_GCM_SHA256 -> 0x009C
  | TLS_RSA_WITH_AES_256_GCM_SHA384 -> 0x009D
  | TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 -> 0xC02F
  | TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384 -> 0xC030

let cipher_suite_of_int = function
  | 0x0000 -> Some TLS_NULL_WITH_NULL_NULL
  | 0x009C -> Some TLS_RSA_WITH_AES_128_GCM_SHA256
  | 0x009D -> Some TLS_RSA_WITH_AES_256_GCM_SHA384
  | 0xC02F -> Some TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
  | 0xC030 -> Some TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384
  | _ -> None

let string_of_cipher_suite = function
  | TLS_NULL_WITH_NULL_NULL -> "TLS_NULL_WITH_NULL_NULL"
  | TLS_RSA_WITH_AES_128_GCM_SHA256 -> "TLS_RSA_WITH_AES_128_GCM_SHA256"
  | TLS_RSA_WITH_AES_256_GCM_SHA384 -> "TLS_RSA_WITH_AES_256_GCM_SHA384"
  | TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 -> "TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256"
  | TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384 -> "TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384"

let key_size_of_cipher_suite = function
  | TLS_NULL_WITH_NULL_NULL -> 0
  | TLS_RSA_WITH_AES_128_GCM_SHA256
  | TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 -> aes_128_key_size
  | TLS_RSA_WITH_AES_256_GCM_SHA384
  | TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384 -> aes_256_key_size

(** {1 PRF - Pseudo-Random Function (RFC 5246 Section 5)} *)

(** TLS 1.2 PRF using SHA-256 *)
let prf_sha256 ~secret ~label ~seed ~length =
  let module H = Digestif.SHA256 in
  let hmac key data =
    H.hmac_string ~key:(Cstruct.to_string key) (Cstruct.to_string data)
    |> H.to_raw_string
    |> Cstruct.of_string
  in
  let label_seed = Cstruct.concat [Cstruct.of_string label; seed] in

  (* P_hash(secret, seed) = HMAC(secret, A(1) + seed) + HMAC(secret, A(2) + seed) + ... *)
  let rec p_hash acc a remaining =
    if remaining <= 0 then
      Cstruct.sub (Cstruct.concat (List.rev acc)) 0 length
    else
      let a_next = hmac secret a in
      let output = hmac secret (Cstruct.concat [a_next; label_seed]) in
      p_hash (output :: acc) a_next (remaining - Cstruct.length output)
  in
  let a1 = hmac secret label_seed in
  p_hash [] a1 length

(** {1 Key Derivation (RFC 5246 Section 6.3)} *)

(** Derive key material from master secret *)
let derive_key_material ~master_secret ~server_random ~client_random ~cipher_suite =
  let key_size = key_size_of_cipher_suite cipher_suite in
  if key_size = 0 then
    (* Null cipher - no keys needed *)
    {
      client_write_key = Cstruct.empty;
      server_write_key = Cstruct.empty;
      client_write_iv = Cstruct.empty;
      server_write_iv = Cstruct.empty;
    }
  else
    (* key_block = PRF(master_secret, "key expansion", server_random + client_random) *)
    let seed = Cstruct.concat [server_random; client_random] in
    let key_block_length = 2 * key_size + 2 * gcm_iv_size in
    let key_block = prf_sha256
      ~secret:master_secret
      ~label:"key expansion"
      ~seed
      ~length:key_block_length
    in
    let offset = ref 0 in
    let take n =
      let result = Cstruct.sub key_block !offset n in
      offset := !offset + n;
      result
    in
    {
      client_write_key = take key_size;
      server_write_key = take key_size;
      client_write_iv = take gcm_iv_size;
      server_write_iv = take gcm_iv_size;
    }

(** Derive master secret from pre-master secret (RFC 5246 Section 8.1) *)
let derive_master_secret ~pre_master_secret ~client_random ~server_random =
  let seed = Cstruct.concat [client_random; server_random] in
  prf_sha256
    ~secret:pre_master_secret
    ~label:"master secret"
    ~seed
    ~length:master_secret_size

(** {1 AES-GCM Encryption (RFC 5116, RFC 5288)} *)

(** Build nonce for AES-GCM
    nonce = implicit_iv (4 bytes) || explicit_nonce (8 bytes) *)
let build_nonce ~implicit_iv ~explicit_nonce =
  Cstruct.concat [implicit_iv; explicit_nonce]

(** Build Additional Authenticated Data (AAD) for DTLS
    AAD = epoch (2) || sequence_number (6) || type (1) || version (2) || length (2) *)
let build_aad ~epoch ~seq_num ~content_type ~version_major ~version_minor ~length =
  let aad = Cstruct.create 13 in
  Cstruct.BE.set_uint16 aad 0 epoch;
  (* 6-byte sequence number *)
  Cstruct.BE.set_uint16 aad 2 (Int64.to_int (Int64.shift_right seq_num 32));
  Cstruct.BE.set_uint32 aad 4 (Int64.to_int32 seq_num);
  Cstruct.set_uint8 aad 8 content_type;
  Cstruct.set_uint8 aad 9 version_major;
  Cstruct.set_uint8 aad 10 version_minor;
  Cstruct.BE.set_uint16 aad 11 length;
  aad

(** Encrypt data using AES-128-GCM
    Note: mirage-crypto uses string type for all data *)
let aes_gcm_encrypt ~key ~nonce ~aad ~plaintext =
  let key_str = Cstruct.to_string key in
  let nonce_str = Cstruct.to_string nonce in
  let aad_str = Cstruct.to_string aad in
  let plaintext_str = Cstruct.to_string plaintext in
  let gcm_key = Mirage_crypto.AES.GCM.of_secret key_str in
  let ciphertext_str = Mirage_crypto.AES.GCM.authenticate_encrypt ~key:gcm_key ~nonce:nonce_str ~adata:aad_str plaintext_str in
  Cstruct.of_string ciphertext_str

(** Decrypt data using AES-128-GCM *)
let aes_gcm_decrypt ~key ~nonce ~aad ~ciphertext =
  let key_str = Cstruct.to_string key in
  let nonce_str = Cstruct.to_string nonce in
  let aad_str = Cstruct.to_string aad in
  let ciphertext_str = Cstruct.to_string ciphertext in
  let gcm_key = Mirage_crypto.AES.GCM.of_secret key_str in
  match Mirage_crypto.AES.GCM.authenticate_decrypt ~key:gcm_key ~nonce:nonce_str ~adata:aad_str ciphertext_str with
  | Some plaintext_str -> Some (Cstruct.of_string plaintext_str)
  | None -> None

(** {1 Crypto Context Management} *)

(** Create new crypto context *)
let create ~is_client =
  Mirage_crypto_rng_unix.use_default ();
  {
    params = None;
    is_client;
    read_seq = 0L;
    write_seq = 0L;
  }

(** Set security parameters after handshake *)
let set_params ctx ~cipher_suite ~master_secret ~client_random ~server_random =
  let key_material = derive_key_material
    ~master_secret ~server_random ~client_random ~cipher_suite
  in
  ctx.params <- Some {
    cipher_suite;
    master_secret;
    client_random;
    server_random;
    key_material = Some key_material;
  }

(** Reset sequence numbers (after key change) *)
let reset_sequence ctx =
  ctx.read_seq <- 0L;
  ctx.write_seq <- 0L

(** {1 Record Encryption/Decryption} *)

(** Encrypt a DTLS record
    Returns: explicit_nonce || ciphertext || tag *)
let encrypt_record ctx ~epoch ~seq_num ~content_type ~version_major ~version_minor ~plaintext =
  match ctx.params with
  | None ->
    (* No encryption - return plaintext *)
    Ok plaintext
  | Some params ->
    match params.key_material with
    | None -> Error "Key material not derived"
    | Some km ->
      let cipher_suite = params.cipher_suite in
      if cipher_suite = TLS_NULL_WITH_NULL_NULL then
        Ok plaintext
      else
        (* Get write key and IV based on role *)
        let write_key, write_iv =
          if ctx.is_client then
            km.client_write_key, km.client_write_iv
          else
            km.server_write_key, km.server_write_iv
        in

        (* Generate explicit nonce (8 bytes random) *)
        let explicit_nonce = Cstruct.of_string (Mirage_crypto_rng.generate gcm_nonce_explicit) in

        (* Build full nonce *)
        let nonce = build_nonce ~implicit_iv:write_iv ~explicit_nonce in

        (* Build AAD - use passed seq_num to match record header *)
        let aad = build_aad
          ~epoch
          ~seq_num
          ~content_type
          ~version_major
          ~version_minor
          ~length:(Cstruct.length plaintext)
        in

        (* Encrypt *)
        let ciphertext_with_tag = aes_gcm_encrypt ~key:write_key ~nonce ~aad ~plaintext in

        (* Increment sequence number *)
        ctx.write_seq <- Int64.add ctx.write_seq 1L;

        (* Return: explicit_nonce || ciphertext || tag *)
        Ok (Cstruct.concat [explicit_nonce; ciphertext_with_tag])

(** Decrypt a DTLS record
    Input: explicit_nonce || ciphertext || tag *)
let decrypt_record ctx ~epoch ~seq_num ~content_type ~version_major ~version_minor ~ciphertext =
  match ctx.params with
  | None ->
    (* No encryption - return as-is *)
    Ok ciphertext
  | Some params ->
    match params.key_material with
    | None -> Error "Key material not derived"
    | Some km ->
      let cipher_suite = params.cipher_suite in
      if cipher_suite = TLS_NULL_WITH_NULL_NULL then
        Ok ciphertext
      else if Cstruct.length ciphertext < gcm_nonce_explicit + gcm_tag_size then
        Error "Ciphertext too short"
      else
        (* Get read key and IV based on role *)
        let read_key, read_iv =
          if ctx.is_client then
            km.server_write_key, km.server_write_iv
          else
            km.client_write_key, km.client_write_iv
        in

        (* Extract explicit nonce *)
        let explicit_nonce = Cstruct.sub ciphertext 0 gcm_nonce_explicit in
        let encrypted_data = Cstruct.sub ciphertext gcm_nonce_explicit
          (Cstruct.length ciphertext - gcm_nonce_explicit)
        in

        (* Build full nonce *)
        let nonce = build_nonce ~implicit_iv:read_iv ~explicit_nonce in

        (* Calculate plaintext length for AAD *)
        let plaintext_length =
          Cstruct.length encrypted_data - gcm_tag_size
        in

        (* Build AAD *)
        let aad = build_aad
          ~epoch
          ~seq_num
          ~content_type
          ~version_major
          ~version_minor
          ~length:plaintext_length
        in

        (* Decrypt and verify *)
        match aes_gcm_decrypt ~key:read_key ~nonce ~aad ~ciphertext:encrypted_data with
        | Some plaintext -> Ok plaintext
        | None -> Error "Decryption failed - authentication error"

(** {1 Utilities} *)

(** Generate random bytes *)
let random_bytes n =
  Cstruct.of_string (Mirage_crypto_rng.generate n)

(** Generate client/server random (32 bytes with timestamp) *)
let generate_random () =
  let buf = Cstruct.create random_size in
  let timestamp = Int32.of_float (Unix.gettimeofday ()) in
  Cstruct.BE.set_uint32 buf 0 timestamp;
  let random_part = Cstruct.of_string (Mirage_crypto_rng.generate (random_size - 4)) in
  Cstruct.blit random_part 0 buf 4 (random_size - 4);
  buf

(** Check if encryption is enabled *)
let is_encrypted ctx =
  match ctx.params with
  | None -> false
  | Some params -> params.cipher_suite <> TLS_NULL_WITH_NULL_NULL

(** Get current cipher suite *)
let get_cipher_suite ctx =
  match ctx.params with
  | None -> None
  | Some params -> Some params.cipher_suite

(** {1 Pretty Printing} *)

let pp_cipher_suite fmt cs =
  Format.fprintf fmt "%s" (string_of_cipher_suite cs)

let pp fmt ctx =
  Format.fprintf fmt "DtlsCrypto(encrypted=%b, write_seq=%Ld, read_seq=%Ld)"
    (is_encrypted ctx) ctx.write_seq ctx.read_seq
