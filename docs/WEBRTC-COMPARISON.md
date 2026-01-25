# WebRTC Implementation Comparison

**Last Updated**: 2026-01-25  
**Purpose**: Record comparable, reproducible benchmark results.

## Verification

- Measured results are recorded below with raw logs.
- When adding results, include: date, OS, hardware, commit hash, command, and raw logs.

### Attempted Runs (Failed)

| Date | Host | OS | Commit | Status | Notes | Logs |
|------|------|----|--------|--------|-------|------|
| 2026-01-17 | maladies.local | Darwin | unknown | failed | `grpc-eio` library not found during test build | `/Users/dancer/me/logs/webrtc-rfc-full-117-tests-20260117-150058.log` |
| 2026-01-17 | maladies.local | Darwin | unknown | failed | `grpc-eio` library not found during test build | `/Users/dancer/me/logs/webrtc-rfc-full-117-tests-ocaml-webrtc-20260117-150300.log` |

Note: 위 로그는 당시 worktree/설치본이 `grpc-eio` 의존이었던 상태에서 발생한 실패 기록입니다. 현재 리포지토리는 `grpc-direct`를 사용합니다.

## Environment (Measured Runs)

- Date: 2026-01-25
- Host: maladies.local
- OS: Darwin
- Repo: masc-mcp (branch `docs-nohype-masc-mcp-0125`)
- Commit: `f962c55` (same code as measurement runs)

## How to Run (MASC)

```bash
cd ~/me/features/masc-mcp

# UDP echo benchmark (fair throughput)
dune exec -- ./test/bench_udp_fair.exe

# UDP one-way throughput (Miuda.ai compatible)
dune exec -- ./test/bench_udp_oneway.exe

# UDP raw throughput (send-focused)
dune exec -- ./test/bench_udp_throughput.exe

# SCTP transport (flow-controlled)
dune exec -- ./test/bench_sctp_transport.exe

# SCTP batch I/O (batched ACK)
dune exec -- ./test/bench_sctp_batch.exe

# Burst send ceiling (no flow control)
dune exec -- ./test/bench_burst_send.exe
```

## Results (Measured)

### UDP

| Date | Benchmark | Result | Loss | Notes | Logs |
|------|-----------|--------|------|-------|------|
| 2026-01-25 | UDP fair echo (8KB peak) | 82.67 MB/s | 0.00% | window=100, duration=3s/size | `/Users/dancer/me/logs/masc-webrtc-bench-udp-fair-20260125-211841.log` |
| 2026-01-25 | UDP one-way (1KB, 10 conns) | 8.51 MB/s | 49.47% | duration=10s | `/Users/dancer/me/logs/masc-webrtc-bench-udp-oneway-20260125-211952.log` |
| 2026-01-25 | UDP send throughput (8KB peak) | 418.27 MB/s | 99.05% | send-focused; very high loss | `/Users/dancer/me/logs/masc-webrtc-bench-udp-throughput-20260125-212011.log` |
| 2026-01-25 | UDP burst send (1KB) | 8.29 MB/s send, 4.30 MB/s recv | 48.17% | no flow control | `/Users/dancer/me/logs/masc-webrtc-bench-burst-send-20260125-212048.log` |

### SCTP

| Date | Benchmark | Result | Loss | Notes | Logs |
|------|-----------|--------|------|-------|------|
| 2026-01-25 | SCTP transport (1KB) | 4.20 MB/s | 0.00% | cwnd=64KB, duration=5s | `/Users/dancer/me/logs/masc-webrtc-bench-sctp-transport-20260125-212022.log` |
| 2026-01-25 | SCTP batch (1KB) | 5.98 MB/s | 3.71% | cwnd=2MB, ACK batch=128 | `/Users/dancer/me/logs/masc-webrtc-bench-sctp-batch-20260125-212034.log` |

## TODO (Pending)

- ocaml-webrtc interop smoke
  - TURN relay smoke: needs `TURN_SERVER`, `TURN_USERNAME`, `TURN_PASSWORD`, optional `TURN_TLS_CA`
  - Browser interop: needs public IP, `WEBRTC_CERT_PEM`, `WEBRTC_KEY_PEM`, and browser peer
  - Tracking doc: ocaml-webrtc `docs/INTEROP-STATUS-2026-01-25.md`
