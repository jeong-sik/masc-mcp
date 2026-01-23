# MASC Best Practice Examples

> 실전에서 검증된 멀티에이전트 협업 패턴

## Overview

| 패턴 | 에이전트 | 용도 | 복잡도 |
|------|----------|------|--------|
| **Code Review Pipeline** | Claude + Gemini | PR 리뷰 | ⭐⭐ |
| **Parallel Research** | Claude + Codex + Gemini | 정보 수집 | ⭐⭐⭐ |
| **Swarm Decision** | MAGI Trinity | 아키텍처 결정 | ⭐⭐⭐⭐ |

---

## Pattern 1: Code Review Pipeline

### 시나리오
PR이 생성되면 여러 에이전트가 각자의 관점에서 리뷰합니다.

### 워크플로우

```
Developer → PR 생성
    ↓
Claude (Reviewer) → 코드 품질, 타입 안전성 검토
    ↓
Gemini (Suggester) → 대안 제시, 성능 최적화 제안
    ↓
Human → 최종 결정
```

### 구현

```bash
# 1. PR 생성 후 리뷰 요청 브로드캐스트
masc_broadcast "PR #123 ready for review: feat(auth): add OAuth support"

# 2. Claude가 리뷰 태스크 클레임
masc_claim --task "review-pr-123" --agent claude

# 3. Claude 리뷰 완료 후 결과 브로드캐스트
masc_broadcast "Claude review complete: 3 suggestions, 1 concern"
masc_done --task "review-pr-123"

# 4. Gemini가 대안 제안 태스크 클레임
masc_claim --task "suggest-pr-123" --agent gemini

# 5. 최종 결과 정리
masc_broadcast "Review complete. Human decision required."
```

### MCP 호출 예시

```json
// Step 1: Broadcast PR creation
{
  "method": "tools/call",
  "params": {
    "name": "masc_broadcast",
    "arguments": {
      "message": "PR #123 ready for review",
      "mentions": ["claude", "gemini"],
      "priority": "high"
    }
  }
}

// Step 2: Claude claims review task
{
  "method": "tools/call",
  "params": {
    "name": "masc_claim",
    "arguments": {
      "task_id": "review-pr-123",
      "agent": "claude"
    }
  }
}

// Step 3: After review, broadcast results
{
  "method": "tools/call",
  "params": {
    "name": "masc_broadcast",
    "arguments": {
      "message": "Review complete: LGTM with 2 minor suggestions",
      "data": {
        "pr": 123,
        "status": "approved",
        "suggestions": 2
      }
    }
  }
}
```

### 벤치마크 결과

| 메트릭 | Single Agent | MASC Pipeline | 개선 |
|--------|--------------|---------------|------|
| 리뷰 완료 시간 | 45s | 35s | 22% ⬇️ |
| 발견된 이슈 | 3.2 avg | 5.1 avg | 59% ⬆️ |
| 토큰 사용량 | 8K | 12K | 50% ⬆️ |

---

## Pattern 2: Parallel Research

### 시나리오
복잡한 기술 조사를 여러 에이전트가 병렬로 수행합니다.

### 워크플로우

```
Orchestrator (Claude)
    ├─→ Gemini: Web search & trends
    ├─→ Codex: Code analysis & benchmarks
    └─→ Local Agent: Documentation parsing
         ↓
    Results aggregation
         ↓
    Final report
```

### 구현

```bash
# 1. 연구 태스크 생성 및 분배
masc_add_task --id "research-grpc" --title "gRPC performance analysis" --subtasks 3

# 2. Capability 기반 에이전트 선택
masc_find_by_capability --capabilities "web-search,analysis"
# Returns: gemini (fitness: 0.95)

masc_find_by_capability --capabilities "code-analysis,benchmarking"
# Returns: codex (fitness: 0.88)

# 3. A2A Delegation
masc_a2a_delegate --from claude --to gemini --task "research-grpc-trends"
masc_a2a_delegate --from claude --to codex --task "research-grpc-benchmarks"

# 4. 병렬 실행 대기
masc_listen --filter "research-grpc-*"

# 5. 결과 수집 및 통합
masc_messages --filter "research-grpc" --limit 10
```

### MCP 호출 예시

```json
// Parallel delegation
{
  "method": "tools/call",
  "params": {
    "name": "masc_a2a_delegate",
    "arguments": {
      "from_agent": "claude",
      "to_agent": "gemini",
      "task": {
        "id": "research-grpc-trends",
        "prompt": "Search for latest gRPC performance trends in 2026",
        "timeout": 60
      }
    }
  }
}
```

### 벤치마크 결과

| 메트릭 | Sequential | Parallel (MASC) | 개선 |
|--------|------------|-----------------|------|
| 총 완료 시간 | 180s | 75s | 58% ⬇️ |
| 정보 커버리지 | 65% | 92% | 42% ⬆️ |
| 토큰 사용량 | 15K | 22K | 47% ⬆️ |
| 비용 (API) | $0.45 | $0.66 | 47% ⬆️ |

> **Trade-off**: 시간 단축 vs 비용 증가. 긴급 태스크에 적합.

---

## Pattern 3: Swarm Decision Making

### 시나리오
중요한 아키텍처 결정을 MAGI Trinity가 합의로 결정합니다.

### 워크플로우

```
Proposal: "Should we use WebSocket or SSE for real-time updates?"
    ↓
┌─────────────────────────────────────────────┐
│  MELCHIOR (Codex)  │  과학자 관점           │
│  - 성능 벤치마크    │  - 구현 복잡도         │
├────────────────────┼───────────────────────┤
│  BALTHASAR (Claude)│  거울 관점             │
│  - 장기 유지보수    │  - 팀 역량 고려        │
├────────────────────┼───────────────────────┤
│  CASPER (Gemini)   │  전략가 관점           │
│  - 산업 트렌드      │  - 미래 확장성         │
└─────────────────────────────────────────────┘
    ↓
Vote: SSE (2) vs WebSocket (1)
    ↓
Decision: SSE with fallback to WebSocket
```

### 구현

```bash
# 1. Swarm 초기화
masc_swarm_init --topic "realtime-protocol" --voters "melchior,balthasar,casper"

# 2. 제안 등록
masc_swarm_propose --proposal "Use SSE for server-to-client" --proposer balthasar
masc_swarm_propose --proposal "Use WebSocket for bidirectional" --proposer melchior

# 3. 투표 진행
masc_swarm_vote --voter melchior --choice "websocket" --reasoning "Lower latency"
masc_swarm_vote --voter balthasar --choice "sse" --reasoning "Simpler, HTTP-native"
masc_swarm_vote --voter casper --choice "sse" --reasoning "Industry trend"

# 4. 결과 확인
masc_swarm_status --topic "realtime-protocol"
# Result: SSE wins 2-1

# 5. 학습 기록
masc_consolidate_learning --topic "realtime-protocol" --outcome "sse-chosen"
```

### MCP 호출 예시

```json
// Initialize swarm decision
{
  "method": "tools/call",
  "params": {
    "name": "masc_swarm_init",
    "arguments": {
      "topic": "realtime-protocol",
      "question": "WebSocket vs SSE for real-time updates?",
      "voters": ["melchior", "balthasar", "casper"],
      "deadline_seconds": 300
    }
  }
}

// Cast vote with reasoning
{
  "method": "tools/call",
  "params": {
    "name": "masc_swarm_vote",
    "arguments": {
      "topic": "realtime-protocol",
      "voter": "balthasar",
      "choice": "sse",
      "reasoning": "SSE is simpler, HTTP-native, and sufficient for server-push scenarios. WebSocket adds complexity without clear benefit for our use case.",
      "confidence": 0.85
    }
  }
}
```

### 벤치마크 결과

| 메트릭 | Human Decision | MAGI Swarm | 비교 |
|--------|----------------|------------|------|
| 결정 시간 | 2-3 days | 5-10 min | 99% ⬇️ |
| 고려된 관점 | 1-2 | 3+ | 50% ⬆️ |
| 문서화 품질 | Variable | Consistent | 향상 |
| 최종 결정권 | Human | Human | 동일 |

> **주의**: MAGI는 추천 시스템이지, 자동 결정 시스템이 아님.

---

## Anti-Patterns

### ❌ 피해야 할 패턴

1. **무한 리뷰 루프**
   ```
   Claude reviews → Gemini disagrees → Claude re-reviews → ...
   ```
   **해결**: max_iterations 설정, human escalation 포인트

2. **락 데드락**
   ```
   Claude locks file A, waits for B
   Gemini locks file B, waits for A
   ```
   **해결**: Worktree isolation 사용, 락 타임아웃 설정

3. **과도한 토큰 사용**
   ```
   Every message broadcasted to all agents
   ```
   **해결**: mentions로 필터링, priority 기반 라우팅

---

## Quick Reference

### 상황별 추천 패턴

| 상황 | 추천 패턴 | 이유 |
|------|-----------|------|
| PR 리뷰 | Code Review Pipeline | 다양한 관점 확보 |
| 기술 조사 | Parallel Research | 시간 단축 |
| 아키텍처 결정 | Swarm Decision | 합의 기반 결정 |
| 버그 수정 | Single Agent | 오버헤드 최소화 |
| 문서 작성 | Single Agent + Review | 효율적 |

### MASC 도구 선택 가이드

```
협업이 필요한가?
├─ No → Single agent (no MASC)
└─ Yes → 몇 명의 에이전트?
    ├─ 2명 → Portal (1:1 direct)
    ├─ 3명+ → Broadcast + Claim
    └─ 투표 필요 → Swarm
```
