# MASC MCP - Ideas & Roadmap

## Current Status (2026-01-09)
- 200+ tests passing (11 new: dashboard tests)
- OCaml native binary
- Redis distributed backend
- Pause/Resume, File Lock, Task Board, Worktree support
- claude/gemini/codex CLI spawn
- Cellular Agent Handover + Auto-spawn
- Internal Caching (Shared Context Store)
- Cluster Tempo Control (Adaptive Orchestration)
- **NEW**: Visual Dashboard (Terminal Status Viewer)

---

## Recently Implemented

### Phase 13: Visual Dashboard ✅
**Implemented**: 2026-01-09

터미널 기반 MASC 상태 대시보드. `watch -n 1` 명령어와 함께 사용하여 실시간 모니터링 가능.

**MCP Tool**:
- `masc_dashboard` - 전체 또는 컴팩트 대시보드 생성

**CLI**:
```bash
masc-mcp dashboard          # Full dashboard
masc-mcp dashboard --compact # Single-line summary
watch -n 1 masc-mcp dashboard  # Real-time monitoring
```

**Shows**:
- Active Agents (with zombie detection)
- Task Board (by priority/status)
- File Locks
- Recent Broadcasts (last 5)
- Tempo Status
- Active Worktrees

**Implementation**: `lib/dashboard.ml` (11 tests)

---

### Phase 12: Cluster Tempo Control ✅
**Implemented**: 2026-01-09

Orchestrator check interval 동적 조정. 태스크 긴급도에 따라 체크 주기 자동 조절.

**MCP Tools** (4개):
- `masc_tempo_get` - 현재 템포 상태 조회
- `masc_tempo_set` - 수동 템포 설정
- `masc_tempo_adjust` - 태스크 기반 자동 조정
- `masc_tempo_reset` - 기본값으로 리셋

**Tempo Levels**:
| 상황 | 체크 간격 | 설명 |
|------|-----------|------|
| Urgent (priority 1-2) | 60s | 긴급 태스크 있음 |
| Normal (priority 3) | 300s | 일반 태스크 |
| Idle (no tasks) | 600s | 할 일 없음 |

**Storage**: `.masc/tempo.json`

---

### Phase 8.1: Auto-spawn on Claim ✅
**Implemented**: 2026-01-09
**Docs**: `docs/CELLULAR-AGENT.md`

Handover claim 시 자동으로 successor agent spawn.

**New MCP Tool**:
- `masc_handover_claim_and_spawn` - Claim + Spawn in one step

**Implementation**:
- `handover.ml`: `build_successor_prompt` + `claim_and_spawn`
- DNA → 마크다운 프롬프트 → agent CLI spawn

---

### Phase 8: Cellular Agent Handover ✅
**Implemented**: 2026-01-09
**Docs**: `docs/CELLULAR-AGENT.md`

Ephemeral agent pattern: **Spawn → Work → Preserve (DNA) → Die**

에이전트가 context limit, timeout, crash로 죽을 때 다음 에이전트에게 구조화된 상태를 전달.

**MCP Tools**:
- `masc_handover_create` - DNA 생성
- `masc_handover_list` - 대기 중인 handover 조회
- `masc_handover_claim` - handover claim
- `masc_handover_get` - DNA 읽기 (마크다운)

**Inspiration**: Stanford Generative Agents, MemGPT, Erlang Supervisor, A-MEM

---

## Ideas Backlog

### 1. Token Usage Tracking ✅
**Priority**: High → **Implemented**: 2026-01-09

`spawn_result`에 토큰 추적 필드 추가:
- `input_tokens`, `output_tokens`: 입출력 토큰
- `cache_creation_tokens`, `cache_read_tokens`: 캐시 토큰
- `cost_usd`: 비용

Claude CLI `--output-format json` 사용, JSON 파싱으로 자동 추출.

---

### 2. Internal Caching (Shared Context Store) ✅
**Priority**: Medium → **Implemented**: 2026-01-09

에이전트 간 컨텍스트 공유 및 캐싱.

**MCP Tools** (6개):
- `masc_cache_set` - 값 저장 (TTL, 태그 지원)
- `masc_cache_get` - 값 조회
- `masc_cache_delete` - 값 삭제
- `masc_cache_list` - 전체/태그별 목록
- `masc_cache_clear` - 전체 삭제
- `masc_cache_stats` - 통계 (항목 수, 용량, 가장 오래된 항목)

**Features**:
- TTL (Time To Live) 지원 - 자동 만료
- 태그 기반 그룹핑 및 필터링
- 자동 만료 정리

**Location**: `.masc/cache/`

---

### 3. Execution Memory ✅
**Priority**: High → **Implemented**: 2026-01-09

MCP 도구 6개로 작업 실행 추적:
- `masc_run_init`, `masc_run_plan`, `masc_run_log`
- `masc_run_deliverable`, `masc_run_get`, `masc_run_list`

저장소: `.masc/runs/{task_id}/meta.json`

---

### 4. Cluster Tempo Control ✅
**Priority**: Low → **Implemented**: 2026-01-09

Orchestrator check interval 동적 조정.

**MCP Tools**: `masc_tempo_get`, `masc_tempo_set`, `masc_tempo_adjust`, `masc_tempo_reset`
**Storage**: `.masc/tempo.json`

---

### 5. Visual Dashboard ✅
**Priority**: Medium → **Implemented**: 2026-01-09

터미널 기반 상태 대시보드 구현. TUI 라이브러리 없이 순수 ASCII 출력.

**MCP Tool**: `masc_dashboard`
**CLI**: `masc-mcp dashboard [--compact]`

**Shows**:
- Active Agents (zombie detection 포함)
- Task Board (priority/status별)
- File Locks
- Recent Broadcasts
- Tempo Status
- Active Worktrees

---

### 6. Binary Distribution
**Priority**: High
**Description**: 쉬운 설치 지원

```bash
# Goal
brew install masc
# or
npm install -g @masc/cli
```

**Removes**: opam/dune 의존성

---

### 7. Worktree Diff Broadcast
**Priority**: Medium
**Description**: 워크트리 간 변경 사항 요약 공유

```
📢 [system] A 에이전트가 auth.ts 수정 중 (+50, -10)
```

**Solves**: Context fragmentation between worktrees

---

## MAGI Evaluation Summary (2026-01-07)

**CASPER (Gemini)**:
> "Agent Framework가 아닌, Agent OS 커널(Kernel)에 가깝다"

**Positioning**:
- ❌ "Agent Framework"
- ✅ "Git Worktree Coordination for AI Agents"

**Unique Selling Points**:
1. Git Worktree 기반 물리적 격리
2. CLI-First Orchestration
3. MCP Native Coordination

**Target Users**: 시니어 엔지니어, DevOps, AI IDE 빌더

---

## Contributing

Ideas welcome! Open an issue or PR.
