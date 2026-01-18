# MASC Mitosis - Cell Division for Infinite Agent Lifecycle

## Overview

Mitosis는 세포 분열에서 영감을 받은 에이전트 컨텍스트 관리 패턴입니다.
컨텍스트 오버플로우 전에 **proactive하게** 새 세션으로 핸드오프합니다.

```
Traditional: 100% → CRASH → Lost context
Mitosis:      50% → Prepare → 80% → Handoff → Seamless continuation
```

## 2-Phase Division Pattern

### Phase 1: Prepare (50% threshold)
- DNA 추출 (현재 컨텍스트 압축)
- 상태: `Idle` → `ReadyForHandoff`
- 작업 계속 가능

### Phase 2: Handoff (80% threshold)
- 새 에이전트에게 DNA 전달
- 현재 에이전트 graceful shutdown
- 상태: `Prepared` → `Dividing` → `Apoptotic`

```
Timeline:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  0%      50%              80%              100%
   │       │                │                 │
   │  ┌────▼────┐      ┌────▼────┐           │
   │  │ PREPARE │      │ HANDOFF │           │
   │  │ DNA추출 │      │ 새세션  │           │
   │  └─────────┘      └─────────┘           │
   │                                          │
  idle    ready_for_handoff    dividing    (avoided)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## API Reference

### MCP Tools

#### `masc_mitosis_check`
컨텍스트 비율 기반 분열 상태 확인

```json
{
  "name": "masc_mitosis_check",
  "arguments": {
    "context_ratio": 0.5
  }
}
```

**Response:**
```json
{
  "should_divide": false,
  "should_prepare": true,
  "should_handoff": false,
  "current_phase": "idle",
  "prepare_threshold": 0.5,
  "handoff_threshold": 0.8
}
```

#### `masc_mitosis_prepare`
DNA 추출 및 핸드오프 준비

```json
{
  "name": "masc_mitosis_prepare",
  "arguments": {
    "context_ratio": 0.5,
    "full_context": "...",
    "since_len": 0
  }
}
```

**Response:**
```json
{
  "prepared": true,
  "phase": "ready_for_handoff",
  "dna": "compressed context...",
  "dna_length": 195
}
```

#### `masc_mitosis_handoff`
새 에이전트로 핸드오프 실행

```json
{
  "name": "masc_mitosis_handoff",
  "arguments": {
    "context_ratio": 0.8,
    "target_agent": "claude"
  }
}
```

## Configuration

```ocaml
type mitosis_config = {
  prepare_threshold: float;      (* 0.5 - DNA 준비 시점 *)
  handoff_threshold: float;      (* 0.8 - 핸드오프 시점 *)
  min_context_for_delta: int;    (* 1000 - 최소 컨텍스트 길이 *)
  min_delta_len: int;            (* 100 - 노이즈 필터 *)
  dna_compression_ratio: float;  (* 0.3 - 압축률 *)
}
```

## Quality Controls

### 1. Short Session Exception
- `min_context_for_delta = 1000`
- 짧은 세션은 DNA 추출 스킵

### 2. Delta Noise Filter
- `min_delta_len = 100`
- 너무 작은 변경사항 필터링

### 3. Line-based Deduplication
- `deduplicate_lines` 함수
- 중복 라인 제거 (O(n log n) StringSet)

## Cell States

| State | Description |
|-------|-------------|
| `Stem` | 대기 상태, 활성화 준비 |
| `Active` | 작업 중 |
| `Prepared` | DNA 추출 완료, 핸드오프 대기 |
| `Dividing` | 핸드오프 진행 중 |
| `Apoptotic` | Graceful shutdown |

## Usage Example

```bash
# 1. Check at 50%
curl -X POST http://127.0.0.1:8935/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call",
       "params":{"name":"masc_mitosis_check",
                 "arguments":{"context_ratio":0.5}}}'

# 2. Prepare DNA
curl -X POST http://127.0.0.1:8935/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call",
       "params":{"name":"masc_mitosis_prepare",
                 "arguments":{"context_ratio":0.5,
                              "full_context":"...",
                              "since_len":0}}}'

# 3. Check at 80% → Handoff
curl -X POST http://127.0.0.1:8935/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call",
       "params":{"name":"masc_mitosis_check",
                 "arguments":{"context_ratio":0.8}}}'
```

## Test Coverage

```
test_mitosis.ml: 22 tests
├── safe_sub: 6 edge cases
├── deduplicate_lines: 4 scenarios
├── compress_to_dna: 2 ratio tests
├── 2-phase mitosis: 5 lifecycle tests
├── extract_delta: 3 quality filters
└── state/phase: 2 conversions
```

## Design Decisions

### Why 2-Phase?
- **Problem**: 단일 시점 핸드오프는 작업 중단 발생
- **Solution**: 50%에서 미리 준비, 80%에서 실행
- **Benefit**: 30% 버퍼 동안 계속 작업 가능

### Why StringSet for Deduplication?
- **Before**: `List.mem` O(n) → O(n²) total
- **After**: `StringSet.mem` O(log n) → O(n log n) total
- **Benefit**: 대용량 컨텍스트에서 성능 보장

### Why safe_sub?
- **Problem**: `String.sub` raises exception on invalid range
- **Solution**: `safe_sub` returns empty string
- **Benefit**: 예외 없는 안전한 문자열 처리

## Future Work

- [ ] Handoff 실행 구현 (`masc_mitosis_handoff`)
- [ ] 멀티 에이전트 릴레이
- [ ] DNA 압축 알고리즘 개선
- [ ] 메트릭스 수집 (분열 횟수, 성공률)
