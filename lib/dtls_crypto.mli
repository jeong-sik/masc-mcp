[@@@warning "-32"]  (* Suppress unused value warnings for exported API *)

(** DTLS Crypto - Real AES-GCM Encryption for DTLS 1.2

    Implements RFC 5288 (AES-GCM Cipher Suites for TLS)
    Used by WebRTC for secure data transmission.
*)

(** {1 Types} *)

(** Cipher suite identifiers *)
type cipher_suite =
  | TLS_NULL_WITH_NULL_NULL
  | TLS_RSA_WITH_AES_128_GCM_SHA256
  | TLS_RSA_WITH_AES_256_GCM_SHA384
  | TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256  (** WebRTC default *)
  | TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384

(** Key material for encryption/decryption *)
type key_material = {
  client_write_key: Cstruct.t;
  server_write_key: Cstruct.t;
  client_write_iv: Cstruct.t;
  server_write_iv: Cstruct.t;
}

(** Security parameters *)
type security_params = {
  cipher_suite: cipher_suite;
  master_secret: Cstruct.t;
  client_random: Cstruct.t;
  server_random: Cstruct.t;
  key_material: key_material option;
}

(** Encryption context *)
type t

(** {1 Constants} *)

val aes_128_key_size : int
val aes_256_key_size : int
val gcm_iv_size : int
val gcm_nonce_explicit : int
val gcm_tag_size : int
val master_secret_size : int
val random_size : int

(** {1 Cipher Suite Utilities} *)

val cipher_suite_to_int : cipher_suite -> int
val cipher_suite_of_int : int -> cipher_suite option
val string_of_cipher_suite : cipher_suite -> string
val key_size_of_cipher_suite : cipher_suite -> int

(** {1 Key Derivation} *)

val prf_sha256 :
  secret:Cstruct.t ->
  label:string ->
  seed:Cstruct.t ->
  length:int ->
  Cstruct.t
(** TLS 1.2 PRF using SHA-256 *)

val derive_master_secret :
  pre_master_secret:Cstruct.t ->
  client_random:Cstruct.t ->
  server_random:Cstruct.t ->
  Cstruct.t
(** Derive 48-byte master secret from pre-master secret *)

val derive_key_material :
  master_secret:Cstruct.t ->
  server_random:Cstruct.t ->
  client_random:Cstruct.t ->
  cipher_suite:cipher_suite ->
  key_material
(** Derive encryption keys from master secret *)

val prf_sha256 :
  secret:Cstruct.t ->
  label:string ->
  seed:Cstruct.t ->
  length:int ->
  Cstruct.t
(** TLS 1.2 PRF with SHA256 - used for Finished verify data *)

(** {1 Context Management} *)

val create : is_client:bool -> t
(** Create new crypto context *)

val set_params :
  t ->
  cipher_suite:cipher_suite ->
  master_secret:Cstruct.t ->
  client_random:Cstruct.t ->
  server_random:Cstruct.t ->
  unit
(** Set security parameters after handshake *)

val reset_sequence : t -> unit
(** Reset sequence numbers (after key change) *)

(** {1 Record Encryption/Decryption} *)

val encrypt_record :
  t ->
  epoch:int ->
  seq_num:int64 ->
  content_type:int ->
  version_major:int ->
  version_minor:int ->
  plaintext:Cstruct.t ->
  (Cstruct.t, string) result
(** Encrypt a DTLS record. Returns: explicit_nonce || ciphertext || tag
    @param seq_num Sequence number to use in AAD (must match record header) *)

val decrypt_record :
  t ->
  epoch:int ->
  seq_num:int64 ->
  content_type:int ->
  version_major:int ->
  version_minor:int ->
  ciphertext:Cstruct.t ->
  (Cstruct.t, string) result
(** Decrypt a DTLS record *)

(** {1 Utilities} *)

val random_bytes : int -> Cstruct.t
(** Generate random bytes *)

val generate_random : unit -> Cstruct.t
(** Generate 32-byte client/server random *)

val is_encrypted : t -> bool
(** Check if encryption is enabled *)

val get_cipher_suite : t -> cipher_suite option
(** Get current cipher suite *)

(** {1 Pretty Printing} *)

val pp_cipher_suite : Format.formatter -> cipher_suite -> unit
val pp : Format.formatter -> t -> unit
