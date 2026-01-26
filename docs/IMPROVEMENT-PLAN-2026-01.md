# MASC Improvement Plan (2026-01)

## Goal

멀티 에이전트 협업의 안정성/재현성을 높이는 개선안을 정리합니다.

## Verified (real-world)

- 기록 없음. 실행 로그(run_id/날짜/환경/데이터셋)를 함께 적는다.

## Comparisons (measured)

- 기록 없음. 비교 조건/지표/로그를 함께 적는다.

## Planned (hypotheses)

---

## Priority Matrix

| 우선순위 | 개선 항목 | 해결하는 실패 | 기대 효과(가설) | 난이도 |
|---------|----------|--------------|----------|--------|
| **P1** | Schema Validation | 메시지 조정 실패 | 감소 | Medium |
| **P1** | Checkpointing | 장기 작업 실패 복구 | High | Medium |
| **P2** | Judge Agent Pattern | 검증 실패 | 개선 | Low |
| **P2** | Lamport Timestamps | 메시지 순서 보장 | Medium | Low |
| **P3** | Adaptive Orchestration | 스케일링 오버헤드 | Medium | High |
| **P3** | Bloom Filter Discovery | 에이전트 매칭 속도 | 개선 | Medium |

---

## P1: Critical Improvements

### 1.1 Schema-Based Message Validation

**문제**: 에이전트 간 메시지 형식 불일치로 조정 실패 발생

**현재 MASC**:
```ocaml
(* masc_broadcast는 자유 형식 메시지 허용 *)
let broadcast ~agent_name ~message = ...
```

**개선안**:
```ocaml
(* schema/message_types.ml *)
type task_message = {
  action: [`Claim | `Done | `Query | `Respond];
  task_id: string;
  payload: Yojson.Safe.t;
  timestamp: float;
}

type broadcast_message =
  | Task of task_message
  | Status of status_message
  | Error of error_message

(* 컴파일 타임에 메시지 형식 검증 *)
let broadcast ~agent_name ~(message: broadcast_message) = ...
```

**구현 위치**: `features/masc-mcp/lib/core/message_types.ml`

**예상 LOC**: ~200 lines

---

### 1.2 Automatic Checkpointing

**문제**: 긴 작업 중 실패 시 전체 재시작 필요

**현재 MASC**:
```
masc_run_init → masc_run_plan → masc_run_log → masc_run_deliverable
                     ↑ 수동 저장만 가능
```

**개선안**:
```
masc_checkpoint_config(task_id, interval=5) → 5단계마다 자동 스냅샷
masc_checkpoint_save(task_id, step, state)  → 수동 체크포인트
masc_checkpoint_restore(task_id, step)      → 특정 단계로 복원
masc_checkpoint_list(task_id)               → 체크포인트 목록
```

**LangGraph와 차별화**:
- LangGraph: 매 단계 저장
- MASC: 설정 가능한 간격 + 핸드오버와 통합

**구현 위치**: `features/masc-mcp/lib/core/checkpoint.ml`

**데이터 구조**:
```json
{
  "task_id": "task-001",
  "checkpoints": [
    {"step": 5, "timestamp": 1768281517, "state_hash": "abc123", "size_bytes": 2048},
    {"step": 10, "timestamp": 1768281600, "state_hash": "def456", "size_bytes": 3072}
  ],
  "config": {"interval": 5, "max_checkpoints": 10}
}
```

**예상 LOC**: ~400 lines

---

## P2: High-Value Improvements

### 2.1 Judge Agent Pattern

**문제**: 에이전트 자가 검증은 검증 실패를 유발할 수 있음

**개선안**:
```ocaml
(* 새 도구 추가 *)
masc_judge_register(agent_name, criteria)   (* Judge 에이전트 등록 *)
masc_judge_evaluate(task_id, output)        (* 평가 요청 *)
masc_judge_verdict(task_id)                 (* 판정 조회 *)
```

**워크플로우**:
```
Worker Agent          Judge Agent
    │                     │
    ├──[작업 완료]────────►│
    │                     │
    │                     ├──[독립 평가]
    │                     │
    │◄──[PASS/FAIL]───────┤
    │                     │
    └──[FAIL시 재작업]─────►
```

**구현**: Judge Agent는 별도 프롬프트/컨텍스트로 격리

**예상 LOC**: ~250 lines

---

### 2.2 Lamport Timestamps for Message Ordering

**문제**: SSE 비동기 환경에서 메시지 순서 보장 약함

**현재**: Unix timestamp만 사용 (동시 메시지 순서 불명확)

**개선안**:
```ocaml
type lamport_clock = {
  logical_time: int;
  agent_id: string;
  wall_time: float;
}

(* 메시지 전송 시 *)
let send_message ~agent ~message =
  let clock = increment_clock agent.clock in
  let frame = { message; clock } in
  sse_emit frame
```

**이점**:
- 인과 관계 추적 가능
- 분산 디버깅 용이
- 충돌 해결 규칙 명확화

**예상 LOC**: ~100 lines

---

## P3: Strategic Improvements

### 3.1 Adaptive Orchestration

**문제**: 정적 조직 구조는 복잡성 증가 시 조정 오버헤드

**배경**: 작업에 따라 조직 구조 동적 변경

**개선안**:
```ocaml
type orchestration_mode =
  | Flat          (* 모든 에이전트 동등 *)
  | Hierarchical  (* Manager + Workers *)
  | Consensus     (* MAGI 투표 방식 *)
  | Swarm         (* 자율 협업 *)

masc_orchestration_set(mode)
masc_orchestration_auto()  (* 작업 복잡도에 따라 자동 선택 *)
```

**예상 LOC**: ~500 lines (큰 변경)

---

### 3.2 Bloom Filter for Capability Matching

**문제**: 에이전트 수 증가 시 능력 매칭 O(n) 검색

**개선안**:
```ocaml
(* 현재 *)
let find_agents_with_capability cap =
  List.filter (fun a -> List.mem cap a.capabilities) all_agents
  (* O(n * m) where n=agents, m=capabilities *)

(* 개선: Bloom filter *)
let capability_bloom = BloomFilter.create ~size:1024 ~hash_count:3
let find_agents_with_capability cap =
  if BloomFilter.may_contain capability_bloom cap then
    (* 정확한 검색은 후보에만 *)
    ...
  (* O(1) 초기 필터링 *)
```

**예상 LOC**: ~150 lines

---

## Implementation Roadmap

### Phase 1: Foundation (Week 1-2)
- [ ] Schema-based message types 정의
- [ ] Lamport timestamp 도입
- [ ] 기존 테스트 통과 확인

### Phase 2: Reliability (Week 3-4)
- [ ] Checkpointing 시스템 구현
- [ ] Judge agent pattern 추가
- [ ] 에러 복구 시나리오 테스트

### Phase 3: Scale (Week 5-6)
- [ ] Bloom filter 능력 매칭
- [ ] Adaptive orchestration (선택적)
- [ ] 성능 벤치마크

---

## Measurement Plan

| 메트릭 | 기준 | 측정 방법 |
|--------|------|----------|
| 메시지 형식 오류 | 미측정 | Schema 검증 로그 |
| 작업 실패 복구 시간 | 미측정 | 복구 로그 |
| 검증 결과 | 미측정 | Judge 판정 기록 |
| 에이전트 매칭 속도 | 미측정 | 벤치마크 |

---

## Risk Assessment

| 리스크 | 영향 | 완화 전략 |
|--------|------|----------|
| Schema 변경으로 기존 클라이언트 호환성 | High | 버전 필드 + 마이그레이션 기간 |
| Checkpoint 오버헤드 | Medium | 설정 가능한 간격 |
| Judge Agent 추가 비용 | Medium | 선택적 활성화 |

---

## References (ideas)

- MASC v2 Design Doc - features/masc-mcp/docs/MASC-V2-DESIGN.md

---

*Updated: 2026-01-25*
