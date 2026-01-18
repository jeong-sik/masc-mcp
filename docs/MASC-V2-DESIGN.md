# MASC v2: Git-Native Multi-Agent Coordination

> "Git for AI Agents" - 다중 LLM 에이전트의 협업을 Git 워크플로우로 조율

## Executive Summary

MASC v2는 Git Worktree를 활용한 에이전트 격리와 `gh` CLI 기반 PR 워크플로우를 통해
다중 AI 에이전트가 동일 코드베이스에서 충돌 없이 협업할 수 있게 합니다.

### Design Principles (MAGI 삼두 합의)

| Principle | Origin | Rationale |
|-----------|--------|-----------|
| **Worktree Isolation** | CASPER | 에이전트별 완전 격리, 락 불필요 |
| **Use `gh` CLI** | CASPER | PR 시스템 재발명 금지, 기존 도구 활용 |
| **Capability-based** | BALTHASAR | 역할 기반보다 능력 기반 라우팅 |
| **Layered History** | BALTHASAR | 불변/압축/휘발 계층 분리 |
| **Git as Event Log** | CASPER | git log가 곧 이벤트 로그 |

---

## Architecture

```
project-root/
├── .git/                          # Git repository
├── .masc/                         # MASC coordination layer
│   ├── state.json                 # Room state (agents, tasks)
│   ├── agents/                    # Agent metadata
│   │   ├── claude.json            # {capabilities, status, current_worktree}
│   │   ├── gemini.json
│   │   └── codex.json
│   ├── events/                    # Immutable event log (compact layer)
│   │   └── YYYY-MM/
│   │       └── DD.jsonl           # Append-only daily events
│   └── backlog.json               # Task queue
│
├── .worktrees/                    # Git worktrees (agent isolation)
│   ├── claude-feature-x/          # Claude's isolated workspace
│   ├── gemini-fix-y/              # Gemini's isolated workspace
│   └── codex-refactor-z/          # Codex's isolated workspace
│
└── src/                           # Main codebase
```

### Cluster & Distributed Mode

MASC supports three coordination modes:

```
┌──────────────────────────────────────────────────────────────────────────────────────┐
│  FS Mode (Local)          Redis Mode (Distributed)    PostgreSQL Mode (Recommended) │
├──────────────────────────────────────────────────────────────────────────────────────┤
│  Machine A:               Machine A + B + C:          Machine A + B + C:             │
│  ┌───────────────┐        ┌───────────────┐           ┌───────────────┐              │
│  │ Claude ─┐     │        │ Claude ─────┐ │           │ Claude ─────┐ │              │
│  │ Gemini ─┼ .masc/       │ Gemini ─────┼─┼→ Redis    │ Gemini ─────┼─┼→ PostgreSQL │
│  │ Codex ──┘     │        │ Codex ──────┘ │           │ Codex ──────┘ │   (Railway)  │
│  └───────────────┘        └───────────────┘           └───────────────┘              │
│                                                                                      │
│  Same FS = Same Room      Same Redis + Cluster        Same PG + Cluster = Same Room │
│  (blocking OK)            (Lwt blocking issues)       (Eio native, non-blocking) ✓  │
└──────────────────────────────────────────────────────────────────────────────────────┘
```

**Environment Variables**:

| Variable | Purpose |
|----------|---------|
| `ME_ROOT` | Base path (determines `.masc/` location) |
| `MASC_CLUSTER_NAME` | Cluster name (defaults to `basename($ME_ROOT)`) |
| `MASC_STORAGE_TYPE` | `fs`, `redis`, or `postgres` |
| `MASC_REDIS_URL` | Redis connection URL |
| `MASC_POSTGRES_URL` | PostgreSQL connection URL (caqti-eio) |

**Use Cases**:
- **FS Mode**: Claude Code + terminal Gemini on same Mac
- **Redis Mode**: MacBook (Claude) + Linux server (Codex) + Cloud (Gemini) - *Lwt blocking issues*
- **PostgreSQL Mode** ✓ (Recommended): Multi-machine with Eio server, ACID guarantees, SQL queries

---

## Cluster vs Room: 개념 정리

MASC에서 가장 혼동하기 쉬운 개념이 **Cluster**와 **Room**입니다.

```
┌─────────────────────────────────────────────────────────────────────┐
│  Cluster: "me"                                                       │
│  (MASC_CLUSTER_NAME 또는 basename($ME_ROOT))                         │
│                                                                      │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │  Room: "default"                                             │    │
│  │  (협업 공간 - 같은 .masc/ 또는 Redis/PostgreSQL 키 공간)    │    │
│  │                                                               │    │
│  │   ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │    │
│  │   │ claude-rare- │  │ gemini-      │  │ codex-swift- │      │    │
│  │   │ koala        │  │ fierce-zebra │  │ falcon       │      │    │
│  │   │ (Agent)      │  │ (Agent)      │  │ (Agent)      │      │    │
│  │   └──────────────┘  └──────────────┘  └──────────────┘      │    │
│  │                                                               │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                      │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │  Room: "frontend-team" (Future: 여러 Room 지원 예정)         │    │
│  │   ┌──────────────┐  ┌──────────────┐                        │    │
│  │   │ claude-web   │  │ codex-ui     │                        │    │
│  │   └──────────────┘  └──────────────┘                        │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### 용어 정의

| 용어 | 설명 | 예시 |
|------|------|------|
| **Cluster** | 서버/인스턴스 식별자. `MASC_CLUSTER_NAME` 또는 `basename($ME_ROOT)` | `"me"` (~/me 프로젝트) |
| **Room** | 실제 협업 공간. 같은 Room = 같은 Task Board, Messages, Agents | `"default"` (기본 Room) |
| **Agent** | Room 내에서 작업하는 개별 LLM 인스턴스 | `claude-rare-koala`, `gemini-fierce-zebra` |

### 협업 조건

에이전트들이 협업하려면:

1. **같은 Cluster**: 동일한 `MASC_CLUSTER_NAME` 값
2. **같은 Room**: 동일한 Room ID (현재는 "default" 고정)
3. **같은 Storage**:
   - FS 모드: 동일 `.masc/` 폴더 접근
   - Redis 모드: 동일 Redis 서버 + 동일 Cluster Name
   - PostgreSQL 모드: 동일 PostgreSQL + 동일 Cluster Name

### `masc_status` 출력 예시

```
🏢 Cluster: me
📍 Room: default
📁 Path: /Users/dancer/me/.masc

👥 Active Agents (2)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
• claude-rare-koala (working on task-027)
• gemini-fierce-zebra (idle)
```

---

## Core Concepts

### 1. Agent Isolation via Git Worktree

**Why Worktree > File Lock**:
- 락: 순차적 접근, 병렬 불가, 데드락 위험
- Worktree: 완전 격리, 병렬 작업, Git이 충돌 해결

**Workflow**:
```bash
# Agent joins and creates worktree
masc_join --agent claude --capabilities "typescript,review"
git worktree add .worktrees/claude-PK-12345 -b claude/PK-12345 origin/develop

# Agent works in isolated worktree
cd .worktrees/claude-PK-12345
# ... make changes ...

# Agent creates PR via gh CLI (not custom system)
gh pr create --draft --title "[PK-12345] Feature X" --body "..."

# Other agents can review
masc_broadcast "PR ready for review: #123"
```

### 2. Capability-based Routing (not Role-based)

**BALTHASAR 경고**: "Commander/Worker 역할 구분은 인간중심적 함정"

**Instead**:
```json
{
  "agent": "claude",
  "capabilities": ["typescript", "code-review", "architecture"],
  "availability": 0.8,
  "current_load": 2
}
```

**Task Matching**:
```
Task: "Review TypeScript PR"
Required: ["typescript", "code-review"]

Match: claude (2/2 capabilities) > gemini (1/2) > codex (1/2)
```

### 3. Layered History (BALTHASAR 제안)

| Layer | Retention | Content | Storage |
|-------|-----------|---------|---------|
| **Immutable** | Forever | Major decisions, merges | `.masc/events/` |
| **Compactable** | 90 days | Daily summaries | Git commits |
| **Ephemeral** | Session | Real-time messages | Memory only |

**Immutable Events** (`.masc/events/YYYY-MM/DD.jsonl`):
```jsonl
{"seq":1,"type":"agent_join","agent":"claude","ts":"2025-01-02T10:00:00Z"}
{"seq":2,"type":"task_claim","agent":"claude","task":"PK-12345","ts":"2025-01-02T10:01:00Z"}
{"seq":3,"type":"pr_created","agent":"claude","pr":123,"ts":"2025-01-02T11:00:00Z"}
{"seq":4,"type":"pr_merged","agent":"gemini","pr":123,"ts":"2025-01-02T12:00:00Z"}
```

### 4. PR Workflow (gh CLI 활용)

**CASPER 핵심 조언**: "PR 시스템 재발명 금지. `gh` CLI가 이미 완벽함"

```bash
# Create PR
gh pr create --draft --base develop --head claude/PK-12345

# Request review from another agent
gh pr edit 123 --add-reviewer @gemini

# Merge when approved
gh pr merge 123 --squash
```

**MASC의 역할**:
- PR 생성/머지를 이벤트 로그에 기록
- 에이전트 간 알림 브로드캐스트
- Worktree 생성/정리 자동화

---

## MVP Scope (Phase 1)

CASPER의 실용적 조언에 따라 최소 기능부터 시작:

### Must Have
- [x] `masc_init` - 룸 초기화
- [x] `masc_join` - 에이전트 참여 (capabilities 포함)
- [ ] `masc_worktree_create` - Worktree 생성 래퍼
- [ ] `masc_worktree_remove` - Worktree 정리
- [x] `masc_broadcast` - 메시지 브로드캐스트
- [x] `masc_status` - 상태 조회

### Should Have
- [ ] `masc_pr_create` - `gh pr create` 래퍼 + 이벤트 로깅
- [ ] `masc_pr_review` - 리뷰 요청/응답
- [ ] Capability matching 알고리즘

### Won't Have (v2.1+)
- Custom PR system (`.masc/pulls/` - 취소)
- Commander/Worker role hierarchy
- Auto-merge 정책

---

## Self-Organization Bounds (BALTHASAR)

**경고**: "AI가 AI를 리뷰하면 '거짓 신뢰 극장' 발생 가능"

**Guardrails**:
1. **Human-in-the-loop**: 최종 main 브랜치 머지는 인간 승인 필수
2. **Audit Trail**: 모든 결정은 이벤트 로그에 불변 기록
3. **Capability Honesty**: 에이전트는 능력을 과장 신고할 수 없음 (실적 기반 검증)
4. **Escalation Path**: 합의 실패 시 인간에게 에스컬레이션

---

## Implementation Notes

### Git Worktree Commands

```bash
# Create worktree for agent
git worktree add .worktrees/${agent}-${task} -b ${agent}/${task} origin/develop

# List worktrees
git worktree list

# Remove worktree after merge
git worktree remove .worktrees/${agent}-${task}
git branch -d ${agent}/${task}

# Prune stale worktrees
git worktree prune
```

### Event Log Format

```jsonl
// Agent lifecycle
{"seq":1,"type":"agent_join","agent":"claude","capabilities":["ts","review"],"ts":"..."}
{"seq":2,"type":"agent_leave","agent":"claude","reason":"session_end","ts":"..."}

// Worktree lifecycle
{"seq":3,"type":"worktree_create","agent":"claude","branch":"claude/PK-123","ts":"..."}
{"seq":4,"type":"worktree_remove","agent":"claude","branch":"claude/PK-123","ts":"..."}

// PR lifecycle
{"seq":5,"type":"pr_create","agent":"claude","pr":123,"base":"develop","ts":"..."}
{"seq":6,"type":"pr_review","agent":"gemini","pr":123,"verdict":"approve","ts":"..."}
{"seq":7,"type":"pr_merge","agent":"gemini","pr":123,"ts":"..."}

// Task lifecycle
{"seq":8,"type":"task_claim","agent":"claude","task":"PK-123","ts":"..."}
{"seq":9,"type":"task_done","agent":"claude","task":"PK-123","ts":"..."}
```

---

## Research References

| Source | Key Insight | Applied |
|--------|-------------|---------|
| [ccswarm](https://github.com/nwiizo/ccswarm) | 파일 락킹 패턴 | v1 File Lock 참고 |
| [Agent-MCP](https://github.com/rinadelph/Agent-MCP) | Agentic loops | Task lifecycle |
| [A2A Protocol](https://a2aproject.github.io/A2A/) | Agent Cards | Capability-based |
| [LLM Coordination](https://github.com/eric-ai-lab/llm_coordination) | 협력 패턴 | Self-organization |

---

## Migration from v1

v1 → v2 마이그레이션:

```bash
# v1 file locks → v2 worktrees
# 기존 락은 무시, worktree로 전환

# v1 portal → v2 broadcast + PR
# Portal은 유지하되 PR 워크플로우와 통합

# v1 backlog → v2 backlog (동일)
# 태스크 관리는 그대로 유지
```

---

## Version History

- **v2.0.0** (2025-01-02): Git Worktree 기반 아키텍처, Capability 라우팅, Layered History
- **v1.0.0** (2024-12): File Lock 기반, Role-based, Portal A2A

---

## Related Documents

### Analysis & Improvement

- **[EXPERT-PANEL-REVIEW.md](./EXPERT-PANEL-REVIEW.md)** - 7인 전문가 페르소나 심층 리뷰 (2026-01-09)
  - 냉정한 비판자, YCombinator, Chomsky, Darwin, Musk, Neuroscientist, Haskell Master
  - 평균 점수: 6.4/10
  - 공통 지적: 학습/진화 메커니즘 부재, 메타포 과잉

- **[RESEARCH-BASED-IMPROVEMENTS.md](./RESEARCH-BASED-IMPROVEMENTS.md)** - 학술 연구 기반 개선안
  - 11개 논문 인용 (arXiv, ACM, IEEE, Springer, PMC)
  - P0: Error Propagation Guard, Fitness Selection
  - P1: Hebbian Learning, Effect System
  - P2: Terminology Normalization, Telemetry

### Vision & Philosophy

- **[HOLONIC-ARCHITECTURE.md](./HOLONIC-ARCHITECTURE.md)** - 스케일 확장 비전
  - 단일 → 모임 → 조직 → 단체 → 군집 → 기관 → 정신 → 초정신 → 초우주 → 0
  - Holon 원리: 모든 레벨이 전체이자 부분
  - 철학적 기반: Ken Wilber, Teilhard de Chardin, Buddhist Śūnyatā

---

*Designed with MAGI Trinity: CASPER (실용), BALTHASAR (철학)*
