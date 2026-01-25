# WebRTC Implementation Comparison

**Last Updated**: 2026-01-25  
**Purpose**: Record comparable, reproducible benchmark results.

## Verification

- No successful benchmark results recorded in this document yet.
- When adding results, include: date, OS, hardware, commit hash, command, and raw logs.

### Attempted Runs (Failed)

| Date | Host | OS | Commit | Status | Notes | Logs |
|------|------|----|--------|--------|-------|------|
| 2026-01-17 | maladies.local | Darwin | unknown | failed | `grpc-eio` library not found during test build | `/Users/dancer/me/logs/webrtc-rfc-full-117-tests-20260117-150058.log` |
| 2026-01-17 | maladies.local | Darwin | unknown | failed | `grpc-eio` library not found during test build | `/Users/dancer/me/logs/webrtc-rfc-full-117-tests-ocaml-webrtc-20260117-150300.log` |

Note: 위 로그는 당시 worktree/설치본이 `grpc-eio` 의존이었던 상태에서 발생한 실패 기록입니다. 현재 리포지토리는 `grpc-direct`를 사용합니다.

## How to Run (MASC)

```bash
cd ~/me/features/masc-mcp

# UDP echo benchmark
dune exec -- ./test/bench_udp_fair.exe

# UDP flood benchmark
dune exec -- ./test/bench_udp_throughput.exe

# STUN connectivity
dune exec -- ./test/bench_network_real.exe

# In-memory protocol benchmark
dune exec -- ./test/bench_webrtc_stack.exe
```

## Results (Measured)

| Date | Host | Commit | Benchmark | Result | Notes |
|------|------|--------|-----------|--------|-------|
| - | - | - | - | - | - |

## Comparisons (Measured)

| Date | Project | Version | Benchmark | Result | Notes |
|------|---------|---------|-----------|--------|-------|
| - | - | - | - | - | - |
