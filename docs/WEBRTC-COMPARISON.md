# WebRTC Implementation Comparison (Template)

**Last Updated**: 2026-01-25  
**Purpose**: Record comparable, reproducible benchmark results.

## Verification

- No verified results recorded in this document yet.
- When adding results, include: date, OS, hardware, commit hash, command, and raw logs.

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
