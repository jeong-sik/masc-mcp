# Cellular Agent Pattern

**Status**: Phase 8 (Implemented)
**Module**: `lib/handover.ml`

## Overview

Cellular Agent는 **Spawn → Work → Preserve (DNA) → Die** 사이클을 따르는 ephemeral 에이전트 패턴입니다.

에이전트가 죽을 때 (context limit, timeout, crash) 다음 에이전트에게 구조화된 상태를 전달하는 **"last will and testament"** 패턴을 구현합니다.

## Prior Art & Inspiration

| Source | Concept | MASC Adaptation |
|--------|---------|-----------------|
| **Stanford Generative Agents** | Memory Stream + Reflection | Handover = compressed memory |
| **MemGPT** | Self-managed memory tiers | Context % triggers handover |
| **Erlang "Let It Crash"** | Supervisor + State recovery | MASC = Supervisor |
| **A-MEM** | Zettelkasten linking | Task/Session relationships |

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    MASC Supervisor                          │
│  ┌─────────┐    ┌─────────┐    ┌─────────┐                 │
│  │ Agent A │───▶│ Handover│───▶│ Agent B │                 │
│  │  (dies) │    │  (DNA)  │    │ (spawns)│                 │
│  └─────────┘    └─────────┘    └─────────┘                 │
│       │              │              │                       │
│       ▼              ▼              ▼                       │
│   context=85%    .masc/handovers/   claim + resume          │
└─────────────────────────────────────────────────────────────┘
```

## Handover Record (DNA)

```ocaml
type handover_record = {
  id: string;
  from_agent: string;
  to_agent: string option;  (* None = any agent can pick up *)
  task_id: string;
  session_id: string;

  (* Core state *)
  current_goal: string;
  progress_summary: string;
  completed_steps: string list;
  pending_steps: string list;

  (* Thinking context - the "implicit knowledge" problem *)
  key_decisions: string list;
  assumptions: string list;
  warnings: string list;
  unresolved_errors: string list;

  (* File state *)
  modified_files: string list;
  locked_files: string list;

  (* Meta *)
  created_at: float;
  context_usage_percent: int;
  handover_reason: string;
}
```

## Trigger Reasons

| Reason | Description | Example |
|--------|-------------|---------|
| `ContextLimit(pct)` | Context window 임계값 도달 | `context_limit_85` |
| `Timeout(secs)` | 작업 시간 초과 | `timeout_300s` |
| `Explicit` | 사용자/에이전트가 명시적 요청 | `explicit` |
| `FatalError(msg)` | 복구 불가능한 에러 | `error: API rate limit` |
| `TaskComplete` | 작업 정상 완료 | `task_complete` |

## MCP Tools

### `masc_handover_create`

Planning context에서 handover 생성.

```json
{
  "task_id": "task-001",
  "session_id": "session-xyz",
  "reason": "context_limit",
  "context_pct": 85,
  "goal": "PK-32008 LocalStorage SSR 버그 수정",
  "progress": "원인 파악 완료",
  "completed": ["버그 재현", "원인 분석"],
  "pending": ["수정", "테스트", "PR"],
  "decisions": ["SSR-safe 패턴 사용"],
  "assumptions": ["Next.js 14 환경"],
  "warnings": ["hydration mismatch 주의"],
  "errors": [],
  "files": ["src/hooks/useLocalStorage.ts"]
}
```

### `masc_handover_list`

대기 중인 handover 목록 조회.

```json
{
  "pending_only": true
}
```

### `masc_handover_claim`

Handover를 claim (다른 에이전트가 가져가지 못하도록).

```json
{
  "handover_id": "handover-abc123",
  "agent_name": "gemini"
}
```

### `masc_handover_get`

Handover 상세 조회 (마크다운 형태).

```json
{
  "handover_id": "handover-abc123"
}
```

### `masc_handover_claim_and_spawn`

Handover claim + successor agent 자동 spawn.
DNA 컨텍스트가 새 에이전트의 프롬프트로 전달됨.

```json
{
  "handover_id": "handover-abc123",
  "agent_name": "gemini",
  "additional_instructions": "Prioritize security",
  "timeout_seconds": 600
}
```

**Workflow**:
1. Handover claim (다른 에이전트가 가져갈 수 없게)
2. DNA → 마크다운 프롬프트 변환
3. Successor agent spawn (claude/gemini/codex/ollama)
4. 결과 반환

## Example: Full Lifecycle

### 1. Agent A가 context limit 도달

```
Claude (85% context) → masc_handover_create
```

### 2. Handover DNA 생성됨

```markdown
# 🧬 Agent Handover DNA

## Meta
- **ID**: handover-abc123
- **From**: claude → (unclaimed)
- **Task**: task-001
- **Reason**: context_limit_85

## 🎯 Current Goal
PK-32008 LocalStorage SSR 버그 수정

## ✅ Completed
- 버그 재현 확인
- SSR 환경에서 window 객체 접근 문제 확인

## ⏳ Pending
- localStorage 접근을 useEffect로 이동
- 테스트 작성
- PR 생성

## 🧠 Key Decisions
- SSR-safe 패턴으로 typeof window !== 'undefined' 사용
```

### 3. Agent B가 claim

```
Gemini → masc_handover_claim(handover_id, "gemini")
Gemini → masc_handover_get(handover_id)  # DNA 읽기
Gemini → Resume work from pending_steps
```

### 4. Agent B가 작업 완료

```
Gemini → masc_handover_create(reason=TaskComplete)
         또는 masc_done(task_id)
```

## Storage

```
.masc/
└── handovers/
    ├── handover-abc123.json
    ├── handover-def456.json
    └── pending.json  (index)
```

## Design Decisions

### Why JSON over Binary?

- 사람이 읽을 수 있음 (디버깅 용이)
- 다른 도구/스크립트와 통합 쉬움
- Git으로 추적 가능

### Why Markdown Output?

- LLM이 자연스럽게 파싱
- 사람도 바로 읽을 수 있음
- 기존 MCP 응답 형식과 일관성

### Why Explicit Claim?

- Race condition 방지
- 누가 작업 중인지 명확
- 중복 작업 방지

## Future Work

- [ ] Redis backend 지원 (분산 환경)
- [ ] Handover chain 추적 (A → B → C)
- [ ] Auto-spawn on claim (Orchestrator 연동)
- [ ] Context compression (요약 전략)
- [ ] Handover quality metrics

## Related

- `MASC-V2-DESIGN.md` - Overall MASC architecture
- `INTERRUPT-DESIGN.md` - Human-in-the-loop patterns
- `lib/orchestrator.ml` - Self-sustaining loop
- `lib/planning.ml` - PDCA cycle integration
