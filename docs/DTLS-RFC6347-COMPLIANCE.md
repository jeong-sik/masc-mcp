# DTLS RFC 6347 Coverage (Code Inspection)

**Date**: 2026-01-12
**Reviewer**: code inspection
**Project**: ocaml-webrtc DTLS Implementation

---

## Executive Summary

이 문서는 코드 레벨 점검 결과입니다. 실제 동작/상호운용 검증은 포함하지 않습니다.

### Verification Logs (Attempted, Failed)

- 2026-01-17: 테스트 빌드 중 `grpc-eio` 라이브러리 미존재로 실패
  - `/Users/dancer/me/logs/webrtc-rfc-full-117-tests-20260117-150058.log`
  - `/Users/dancer/me/logs/webrtc-rfc-full-117-tests-ocaml-webrtc-20260117-150300.log`
  - 당시 worktree/설치본 기준이며, 현재 리포지토리는 `grpc-direct` 사용

## File Structure

| File | Lines | Purpose |
|------|-------|---------|
| `lib/webrtc/dtls.ml` | 851 | Full protocol with Effect handlers |
| `lib/dtls.ml` | 555 | Record layer, replay protection |
| `lib/dtls_crypto.ml` | 368 | AES-GCM encryption, key derivation |
| **Total** | **1,774** | |

---

## RFC 6347 Section-by-Section Analysis

### Section 3: DTLS Design Rationale

| Requirement | Status | Notes |
|-------------|--------|-------|
| Handle packet loss | Partial | Message seq tracking, no timeout retransmit |
| Handle reordering | Implemented | Sliding window replay detection |
| Handle fragmentation | Partial | Structures defined, reassembly incomplete |

### Section 4.1: Record Layer

| Requirement | Status | Notes |
|-------------|--------|-------|
| Record header (13 bytes) | Implemented | content_type, version, epoch, seq, length |
| Epoch field (2 bytes) | Implemented | Tracked in `t.epoch` |
| Sequence number (6 bytes) | Implemented | `read_seq_num`, `write_seq_num` |
| Anti-replay (Section 4.1.2.6) | Implemented | Sliding window in dtls.ml |
| MAC calculation with seq | Implemented | AAD includes epoch+seq |

### Section 4.2: Handshake Protocol

| Requirement | Status | Notes |
|-------------|--------|-------|
| Message sequence numbers | Implemented | `message_seq`, `next_receive_seq` |
| Retransmission timer | Not implemented | No timeout/retransmit logic |
| Fragment offset/length | Partial | Header parsing done, reassembly incomplete |
| HelloVerifyRequest (DoS) | Implemented | Cookie support implemented |
| Handshake message types | Implemented | All 11 types defined |

### Section 4.2.1: ClientHello

| Requirement | Status | Notes |
|-------------|--------|-------|
| client_random (32 bytes) | Implemented | Generated and stored |
| session_id support | Partial | Parsed, not used for resumption |
| cipher_suites list | Implemented | 4 suites defined |
| compression_methods | Implemented | NULL compression only |
| cookie field | Implemented | From HelloVerifyRequest |

### Section 4.2.4-4.2.6: Key Exchange

| Requirement | Status | Notes |
|-------------|--------|-------|
| ServerHello parsing | Implemented | server_random, cipher extracted |
| Certificate parsing | Placeholder | Just stores raw bytes |
| ServerKeyExchange | Not implemented | No ECDHE params parsing |
| ClientKeyExchange | Placeholder | Sends 65 random bytes |
| ECDHE curve operations | Not implemented | No P-256/X25519 |
| PreMasterSecret derivation | Not implemented | Never computed from ECDHE |

### Section 4.2.8: Finished Message

| Requirement | Status | Notes |
|-------------|--------|-------|
| verify_data computation | Placeholder | Uses random bytes |
| PRF("client finished") | Implemented | `prf_sha256` implemented |
| Hash of handshake messages | Partial | Messages stored, not hashed |
| Finished verification | Not implemented | Just sets state to Established |

### Section 5: Security Considerations

| Requirement | Status | Notes |
|-------------|--------|-------|
| Replay protection | Implemented | Sliding window |
| Certificate chain validation | Not implemented | No X.509 parsing |
| CRL/OCSP checking | Not implemented | Not implemented |
| Cipher suite negotiation | Implemented | Downgrade protection |

---

## Crypto Implementation (RFC 5288)

### AES-GCM (dtls_crypto.ml)

| Feature | Status | Notes |
|---------|--------|-------|
| AES-128-GCM | Implemented | Using mirage-crypto |
| AES-256-GCM | Implemented | Using mirage-crypto |
| 12-byte GCM nonce | Implemented | 4 implicit + 8 explicit |
| 16-byte auth tag | Implemented | GCM tag appended |
| AAD construction | Implemented | epoch + seq + type + version + length |

### Key Derivation

| Feature | Status | Notes |
|---------|--------|-------|
| PRF-SHA256 | Implemented | TLS 1.2 PRF |
| Master secret (48 bytes) | Implemented | From pre-master |
| Key material expansion | Implemented | client/server keys + IVs |
| Key size selection | Implemented | Based on cipher suite |

---

## Missing Critical Features

### 1. ECDHE Key Exchange (Priority: HIGH)

**Current**: `ClientKeyExchange` sends 65 random bytes
**Required**:
- Generate ephemeral EC key pair (P-256 or X25519)
- Parse server's EC public key from ServerKeyExchange
- Compute shared secret via ECDH
- Derive premaster_secret from shared secret

**Effort**: ~200 LOC
**Dependencies**: mirage-crypto-ec

### 2. Certificate Validation (Priority: HIGH)

**Current**: `handle_certificate` stores raw bytes
**Required**:
- Parse X.509 certificate chain
- Verify signature chain to trusted root
- Check validity period (notBefore, notAfter)
- Verify hostname/SAN matching

**Effort**: ~300 LOC
**Dependencies**: x509, pem

### 3. Finished Verification (Priority: HIGH)

**Current**: `handle_finished` just sets state
**Required**:
- Hash all handshake messages (SHA-256)
- Compute: `verify_data = PRF(master_secret, "client finished", Hash(handshake_messages))[0..11]`
- Compare with received verify_data

**Effort**: ~50 LOC
**Dependencies**: None (PRF already implemented)

### 4. Server-Side Handshake (Priority: MEDIUM)

**Current**: Only client-side implemented
**Required**:
- Handle ClientHello, send HelloVerifyRequest
- Send ServerHello, Certificate, ServerKeyExchange, ServerHelloDone
- Receive ClientKeyExchange, ChangeCipherSpec, Finished
- Send ChangeCipherSpec, Finished

**Effort**: ~400 LOC
**Dependencies**: None (reuse existing structures)

### 5. Retransmission Timer (Priority: MEDIUM)

**Current**: No retry logic
**Required** (RFC 6347 Section 4.2.4):
- Initial timeout: 1 second
- Exponential backoff: 2, 4, 8, ... seconds
- Max retransmits: 6 (configurable)

**Effort**: ~100 LOC
**Dependencies**: Eio timer

---

## Test Coverage

| Test Suite | Cases | Coverage |
|------------|-------|----------|
| test_dtls_crypto.ml | 11 | Cipher, context, key derivation, encrypt/decrypt |
| test_stun.ml | 8 | STUN parsing (related) |
| test_sctp.ml | 7 | SCTP transport (related) |

**Missing Tests**:
- Handshake state machine tests
- Fragment reassembly tests
- Replay attack prevention tests
- Certificate validation tests
- Interoperability tests (vs OpenSSL, BoringSSL)

---

## Roadmap to 100%

### Phase 1: Core Crypto (Current → 60%)
- [ ] Implement ECDHE key exchange with P-256
- [ ] Implement Finished verification
- Estimated: 2 days

### Phase 2: Certificate Handling (60% → 80%)
- [ ] X.509 parsing with x509 library
- [ ] Chain validation
- [ ] Hostname verification
- Estimated: 3 days

### Phase 3: Server-Side (80% → 95%)
- [ ] Complete server handshake flow
- [ ] Symmetric with client implementation
- Estimated: 3 days

### Phase 4: Reliability (95% → 100%)
- [ ] Retransmission timer
- [ ] Fragment reassembly
- [ ] Comprehensive test suite
- [ ] Browser interop testing
- Estimated: 2 days

**Total Estimated Effort**: 10 days

---

## References

- [RFC 6347](https://tools.ietf.org/html/rfc6347) - DTLS 1.2
- [RFC 5288](https://tools.ietf.org/html/rfc5288) - AES-GCM Cipher Suites
- [RFC 5246](https://tools.ietf.org/html/rfc5246) - TLS 1.2 (base spec)
- [RFC 8446](https://tools.ietf.org/html/rfc8446) - TLS 1.3 (future migration)
