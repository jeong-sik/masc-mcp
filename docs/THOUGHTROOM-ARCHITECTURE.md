# ThoughtRoom Architecture

Status: concept document (not validated). For implemented behavior, see `docs/SPEC.md`.
Verification: none recorded (2026-01-25)
Scope: design proposal; examples are illustrative.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1.0 | 2026-01-02 | Initial design (brainstorm output) |

---

## 1. Core Philosophy

### 1.1 왜 ThoughtRoom인가?

**단일 에이전트의 한계** (문헌 기반 가설):
- 자기 생성물 평가 시 확증편향 가능성
- 자기 합리화 편향 가능성
- Self-refine loops may have diminishing returns (hypothesis)

**ThoughtRoom 설계 원칙(가설)**:
```
만든 역할 ≠ 검증 역할 ≠ 판단 역할
```

### 1.2 핵심 원칙

| 원칙 | 설명 | 구현 |
|------|------|------|
| **Intentional Diversity** | 의도적으로 다른 관점 배치 | MAGI Trinity |
| **Structured Debate** | 구조화된 토론 프로토콜 | MASC voting/portal |
| **Shared Memory** | 집단 기억 공유 | HippoRAG + Neo4j |
| **Consensus Extraction** | 합의 도출 메커니즘 | 2/3 majority rule |

---

## 2. Auto-Decomposition Pipeline

Motivation (hypothesis): 작업 분할의 어려움을 AI가 보조한다.

### 2.1 Pipeline 구조

```
┌─────────────────────────────────────────────────────────────────┐
│                   AUTO-DECOMPOSITION PIPELINE                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────┐      ┌─────────────┐      ┌─────────────┐    │
│  │  DECOMPOSER │──────│  VALIDATOR  │──────│  EXECUTOR   │    │
│  │  (Gemini)   │      │  (Claude)   │      │  (Codex)    │    │
│  │  분해     │      │  검증    │      │  실행     │    │
│  └─────────────┘      └─────────────┘      └─────────────┘    │
│        │                    │                    │             │
│        ▼                    ▼                    ▼             │
│  ┌─────────────┐      ┌─────────────┐      ┌─────────────┐    │
│  │ Task Graph  │──────│ Validated   │──────│  Results    │    │
│  │ (DAG)       │      │ Graph       │      │  Merged     │    │
│  └─────────────┘      └─────────────┘      └─────────────┘    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 분해 전략 (Decomposition Strategies)

| Strategy | When to Use | Example |
|----------|-------------|---------|
| **File-based** | 파일 독립적 수정 | 100개 파일 마이그레이션 |
| **Layer-based** | 레이어별 분리 가능 | API → Service → DB |
| **Role-based** | 전문성 필요 | Test작성 vs 구현 vs 리뷰 |
| **Phase-based** | 순차 의존 | Design → Implement → Test |

### 2.3 Decomposer Agent 프롬프트

```markdown
## Task Decomposition Request

You are CASPER (Gemini 3), the Strategist.

Given this task:
{task_description}

Analyze and decompose into subtasks that can be:
1. Executed in parallel (no dependencies)
2. Assigned to different specialists
3. Verified independently

Output format:
```json
{
  "strategy": "file-based|layer-based|role-based|phase-based",
  "subtasks": [
    {
      "id": "task-001",
      "description": "...",
      "dependencies": [],  // empty = can run parallel
      "specialist": "codex|claude|gemini",
      "estimated_complexity": "low|medium|high"
    }
  ],
  "critical_path": ["task-001", "task-003", "task-005"]
}
```

### 2.4 Validator Agent 체크리스트

```markdown
## Validation Checklist

BALTHASAR (Claude Opus) validates decomposition:

□ No circular dependencies
□ All files accounted for
□ No overlapping responsibilities
□ Critical path is correct
□ Complexity estimates realistic
□ Rollback strategy defined
```

---

## 3. Code Review Pipeline

Principle (hypothesis): creation and review roles are separated.

### 3.1 Pipeline 구조

```
┌───────────────────────────────────────────────────────────────────┐
│                    CODE REVIEW PIPELINE                           │
├───────────────────────────────────────────────────────────────────┤
│                                                                   │
│  Phase 1: CREATION                                                │
│  ┌─────────────┐                                                 │
│  │  MELCHIOR   │──── Creates code ────►  Draft PR                │
│  │  (Codex)    │                                                 │
│  └─────────────┘                                                 │
│        │                                                          │
│        ▼ (MASC broadcast: "Code ready for review")               │
│                                                                   │
│  Phase 2: MULTI-CRITIC REVIEW                                     │
│  ┌─────────────┐   ┌─────────────┐   ┌─────────────┐            │
│  │  Critic A   │   │  Critic B   │   │  Critic C   │            │
│  │  (Type      │   │  (Security  │   │  (Perf      │            │
│  │   Safety)   │   │   Focus)    │   │   Focus)    │            │
│  └──────┬──────┘   └──────┬──────┘   └──────┬──────┘            │
│         │                 │                 │                    │
│         └─────────────────┼─────────────────┘                    │
│                           ▼                                       │
│  Phase 3: SYNTHESIS                                               │
│  ┌─────────────┐                                                 │
│  │  BALTHASAR  │──── Synthesizes critiques ────►  Review Report │
│  │  (Judge)    │                                                 │
│  └─────────────┘                                                 │
│        │                                                          │
│        ▼                                                          │
│  Phase 4: RESOLUTION                                              │
│  ┌─────────────┐                                                 │
│  │  MELCHIOR   │──── Applies fixes ────►  Updated PR            │
│  │  (Codex)    │                                                 │
│  └─────────────┘                                                 │
│                                                                   │
└───────────────────────────────────────────────────────────────────┘
```

### 3.2 Multi-Critic 역할 분배

| Critic | Focus | Checks |
|--------|-------|--------|
| **Type Safety** | 타입 시스템 | `any` 금지, Parse Don't Validate |
| **Security** | 보안 취약점 | OWASP Top 10, injection |
| **Performance** | 성능 | N+1 queries, unnecessary re-renders |
| **Architecture** | 설계 | SOLID, coupling, cohesion |
| **Maintainability** | 유지보수 | 복잡도, 중복, 명명규칙 |

### 3.3 Confidence-based Filtering

```typescript
interface ReviewComment {
  file: string;
  line: number;
  severity: 'error' | 'warning' | 'suggestion';
  confidence: number;  // 0.0 - 1.0
  message: string;
  autoFixable: boolean;
}

// Only surface high-confidence issues
const surfacedComments = comments.filter(c =>
  c.confidence >= 0.8 || c.severity === 'error'
);
```

---

## 4. Memory Integration

### 4.1 HippoRAG Layer (Semantic Memory)

```
┌─────────────────────────────────────────────────────────────────┐
│                        HIPPORAG LAYER                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  User Query: "지난주 리펙토링할 때 뭐 했더라?"                    │
│                                                                 │
│  ┌─────────────┐                                                │
│  │   DG Layer  │  ──► Sparse Retrieval (BM25)                  │
│  │ (Dentate    │  ──► Dense Retrieval (BGE-M3)                 │
│  │  Gyrus)     │  ──► Graph Neighbors (Neo4j PPR)              │
│  └──────┬──────┘                                                │
│         │                                                        │
│         ▼                                                        │
│  ┌─────────────┐                                                │
│  │   CA3 Layer │  ──► Pattern Completion                       │
│  │ (Hippocampus│  ──► Cross-Reference DG outputs               │
│  │  CA3)       │                                                │
│  └──────┬──────┘                                                │
│         │                                                        │
│         ▼                                                        │
│  ┌─────────────┐                                                │
│  │   CA1 Layer │  ──► Synthesize final response                │
│  │ (Output     │  ──► Coherent narrative                       │
│  │  Integration│                                                │
│  └─────────────┘                                                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 4.2 Neo4j Layer (Knowledge Graph)

```cypher
// ThoughtRoom Session Structure
(:ThoughtRoom {id: $room_id})
  -[:HAS_AGENT]->(:Agent {name: 'MELCHIOR', type: 'codex'})
  -[:HAS_AGENT]->(:Agent {name: 'BALTHASAR', type: 'claude'})
  -[:HAS_AGENT]->(:Agent {name: 'CASPER', type: 'gemini'})

// Task Decomposition Graph
(:Task {id: $task_id, status: 'decomposed'})
  -[:SUBTASK]->(:Subtask {id: 'task-001', assignee: 'codex'})
  -[:SUBTASK]->(:Subtask {id: 'task-002', assignee: 'claude'})
  -[:DEPENDS_ON {type: 'sequential'}]->(:Subtask {id: 'task-003'})

// Review Trail
(:PullRequest {number: 123})
  -[:REVIEWED_BY]->(:Review {critic: 'type-safety', confidence: 0.95})
  -[:REVIEWED_BY]->(:Review {critic: 'security', confidence: 0.87})
  -[:SYNTHESIZED_BY]->(:ReviewSummary {judge: 'BALTHASAR'})

// Learning from Reviews
(:Pattern {name: 'any-type-usage'})
  -[:DETECTED_IN]->(:Review)
  -[:APPLIED_TO]->(:PullRequest)
```

### 4.3 Cross-Agent Memory Sharing

```
┌─────────────────────────────────────────────────────────────────┐
│                    SHARED MEMORY PROTOCOL                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Agent A writes:                                                │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ masc_broadcast(                                         │   │
│  │   message="Found pattern: any type in UserService.ts",  │   │
│  │   memory_write=true  // Persist to shared memory        │   │
│  │ )                                                       │   │
│  └─────────────────────────────────────────────────────────┘   │
│                         │                                       │
│                         ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                   SHARED MEMORY                         │   │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────┐          │   │
│  │  │  Qdrant   │  │   Neo4j   │  │ PostgreSQL │          │   │
│  │  │ (Vector)  │  │  (Graph)  │  │  (Session) │          │   │
│  │  └───────────┘  └───────────┘  └───────────┘          │   │
│  └─────────────────────────────────────────────────────────┘   │
│                         │                                       │
│                         ▼                                       │
│  Agent B reads:                                                 │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ memories = hippo_search(                                │   │
│  │   query="any type patterns in codebase",                │   │
│  │   include_recent_broadcasts=true                        │   │
│  │ )                                                       │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 5. MASC Protocol Extensions

### 5.1 ThoughtRoom-specific Tools

| Tool | Purpose | Parameters |
|------|---------|------------|
| `thoughtroom_create` | 새 ThoughtRoom 세션 생성 | `topic`, `agents[]` |
| `thoughtroom_decompose` | 작업 자동 분해 요청 | `task`, `strategy` |
| `thoughtroom_review` | 코드 리뷰 파이프라인 시작 | `pr_number`, `critics[]` |
| `thoughtroom_consensus` | 합의 도출 요청 | `options[]`, `threshold` |
| `thoughtroom_memory` | 공유 메모리 조회/저장 | `action`, `query` |

### 5.2 Agent Registration

```ocaml
(* ThoughtRoom agent capabilities *)
type agent_capability =
  | Decomposition    (* Can decompose tasks *)
  | CodeGeneration   (* Can write code *)
  | CodeReview       (* Can review code *)
  | TypeAnalysis     (* Type system expertise *)
  | SecurityAudit    (* Security focus *)
  | PerformanceAudit (* Performance focus *)
  | JudgeSynthesis   (* Can synthesize opinions *)
  | WebSearch        (* Can search web *)
  | MemoryAccess     (* Can read/write shared memory *)

(* MAGI Trinity default capabilities *)
let melchior_capabilities =
  [CodeGeneration; PerformanceAudit]

let balthasar_capabilities =
  [CodeReview; TypeAnalysis; JudgeSynthesis; MemoryAccess]

let casper_capabilities =
  [Decomposition; WebSearch; SecurityAudit]
```

---

## 6. Example Workflows

### 6.1 Large-Scale Migration

```
User: "CRA에서 Vite로 마이그레이션 해줘 (100+ 파일)"

ThoughtRoom:
1. CASPER (Decomposer):
   - Strategy: file-based
   - 100 files → 20 batches of 5
   - Dependencies: config files first, then components

2. BALTHASAR (Validator):
   - OK: No circular dependencies
   - OK: Config → Components → Tests order correct
   - Note: Batch 7 has cross-dependency, needs sequential

3. Parallel Execution:
   - MELCHIOR: Batches 1-5 (configs)
   - CASPER: Batches 6-10 (components A-M)
   - BALTHASAR: Batches 11-15 (components N-Z)

4. Review Pipeline:
   - Each batch reviewed by non-author
   - Confidence filter: only surface 0.8+ issues
```

### 6.2 Complex Feature Implementation

```
User: "사용자 인증 시스템 구현해줘"

ThoughtRoom:
1. CASPER (Decomposer):
   - Strategy: layer-based
   - Task graph:
     ├── API Layer (routes, middleware)
     ├── Service Layer (auth logic)
     ├── Data Layer (user model)
     └── Test Layer (unit + integration)

2. BALTHASAR (Validator):
   - OK: Separation of concerns correct
   - Note: JWT refresh logic missing from task graph
   - Added: Token refresh subtask

3. Implementation:
   - MELCHIOR: API + Service layers
   - Review: BALTHASAR (type safety) + CASPER (security)
   - Fix: MELCHIOR applies security findings

4. Memory:
   - Pattern learned: "JWT auth requires refresh token handling"
   - Stored in Neo4j: (:Pattern)-[:LEARNED_FROM]->(:ThoughtRoom)
```

---

## 7. Implementation Roadmap (Proposal)

Note: 아래 항목은 계획 초안이며 완료 여부는 이 문서에서 검증하지 않음.

### Phase 1: Foundation (Assumed Baseline; Verification Needed)
- [ ] MASC Protocol v2 (OCaml)
- [ ] Basic agent coordination
- [ ] Worktree isolation (primary isolation strategy)
- [ ] ThoughtRoom design document

### Phase 2: Auto-Decomposition
- [ ] Decomposer Agent prompt engineering
- [ ] Validator Agent checklist
- [ ] Task graph data structure (DAG)
- [ ] Parallel execution orchestration

### Phase 3: Code Review Pipeline
- [ ] Multi-critic role definitions
- [ ] Confidence-based filtering
- [ ] Review synthesis (Judge)
- [ ] Auto-fix integration

### Phase 4: Memory Integration
- [ ] HippoRAG integration hooks
- [ ] Neo4j ThoughtRoom schema
- [ ] Cross-agent memory sharing protocol
- [ ] Pattern learning from reviews

### Phase 5: Production
- [ ] Performance optimization
- [ ] Error recovery
- [ ] Metrics & observability
- [ ] User documentation

---

## 8. Target Metrics (Hypotheses)

Targets below are hypotheses and require measurement.

| Metric | Target (hypothesis) | Measurement |
|--------|--------------------|-------------|
| **Confirmation Bias Reduction** | 50% fewer self-approved issues | Pre/post review defect rate |
| **Task Completion Speed** | 2x for parallelizable tasks | Time to completion |
| **Code Quality** | 30% fewer production bugs | Bug count after deployment |
| **Knowledge Retention** | 80% pattern recall | Pattern reuse rate |
| **Agent Utilization** | >70% parallel efficiency | Active time / total time |

---

## 9. References

- [MAR: Multi-Agent Reflexion](https://arxiv.org/abs/2402.00678) - Confirmation bias in single-agent
- [MultiAgentBench](https://arxiv.org/abs/2404.00000) - Graph topology collaboration
- [MALMM](https://arxiv.org/abs/2403.00000) - Shared memory multi-agent
- [HippoRAG Paper](https://arxiv.org/abs/2405.00000) - Hippocampal indexing for RAG
- [MASC Protocol v2](./MASC-V2-DESIGN.md) - Foundation coordination layer

---
