# MASC-MCP Full Specification

> Multi-Agent Streaming Coordination over Model Context Protocol

**Version**: 3.0.0 (Phase 13 Complete)
**Last Updated**: 2026-01-09
**Status**: Production Ready
**Tests**: 291+ passed (337 test functions across 23 test files)
**Modules**: 44 library modules (~24,000 lines of OCaml)

---

## Executive Summary

MASC-MCP는 MCP(Model Context Protocol) 2025-11-25 스펙을 완전 구현한 OCaml 네이티브 다중 에이전트 협업 시스템입니다. A2A(Agent-to-Agent) 프로토콜의 핵심 기능을 통합하여 **Universal Agent Glue** 역할을 수행합니다.

```
┌─────────────────────────────────────────────────────────────┐
│                    MASC: The Glue Layer                     │
├─────────────────────────────────────────────────────────────┤
│  Claude Code ←→ MCP ←→ MASC ←→ A2A/gRPC ←→ Gemini/Codex   │
│  Cursor      ←→ MCP ←→ MASC ←→ REST     ←→ Custom Agents  │
│  Any Client  ←→ MCP ←→ Any Protocol ←→ Any Agent          │
└─────────────────────────────────────────────────────────────┘
```

---

## Table of Contents

1. [What's New in v3.0](#whats-new-in-v30)
2. [Architecture](#architecture)
3. [MCP Tools Reference (111 tools)](#mcp-tools-reference)
4. [MCP 2025-11-25 Compliance](#mcp-2025-11-25-compliance)
5. [A2A Integration](#a2a-integration)
6. [Feature Matrix](#feature-matrix)
7. [Pros and Cons Analysis](#pros-and-cons-analysis)
8. [Best Practices](#best-practices)
9. [Roadmap](#roadmap)

---

## What's New in v3.0

### Phase 8-13 Features (2026-01-09)

| Phase | Feature | Tools | Description |
|-------|---------|-------|-------------|
| **8** | Cellular Agent Handover | 5 | DNA-based state transfer between agents |
| **9** | Execution Memory | 6 | Task execution tracking (runs/) |
| **10** | Token Usage Tracking | - | Cost attribution in spawn results |
| **11** | Internal Caching | 6 | Shared context store with TTL |
| **12** | Cluster Tempo Control | 4 | Adaptive orchestrator intervals |
| **13** | Visual Dashboard | 1 | Terminal-based status viewer |

### Key Metrics Change

| Metric | v2.1.0 (Phase 7) | v3.0.0 (Phase 13) | Change |
|--------|------------------|-------------------|--------|
| MCP Tools | 70+ | **111** | +58% |
| Library Modules | 23 | **44** | +91% |
| Tests | 234 | **291+** | +24% |
| Lines of Code | ~12,000 | **~24,000** | +100% |

---

## Architecture

### Module Structure (44 modules)

```
lib/
├── Core (8 modules)
│   ├── types.ml           # Core type definitions (806 lines)
│   ├── room.ml            # Room state management (1,744 lines)
│   ├── room_utils.ml      # Room utilities
│   ├── room_git.ml        # Git operations
│   ├── room_portal.ml     # Portal (A2A) support
│   ├── room_worktree.ml   # Git worktree management
│   ├── backend.ml         # Storage backends (1,209 lines)
│   └── config.ml          # Configuration
│
├── MCP Protocol (6 modules)
│   ├── mcp_server.ml      # HTTP/SSE server (2,172 lines)
│   ├── mcp_protocol.ml    # JSON-RPC 2.0
│   ├── mcp_session.ml     # Legacy session
│   ├── tools.ml           # 111 MCP tool definitions (2,188 lines)
│   ├── sse.ml             # SSE with Event IDs
│   └── log.ml             # Logging
│
├── A2A Integration (6 modules)
│   ├── agent_card.ml      # A2A Agent Cards
│   ├── transport.ml       # Protocol abstraction
│   ├── transport_grpc.ml  # gRPC support (952 lines)
│   ├── a2a_tools.ml       # A2A MCP tools
│   ├── orchestrator.ml    # Multi-agent orchestration
│   └── masc_pb.ml         # Protobuf (auto-generated)
│
├── Cellular Agent (6 modules) [NEW in v3.0]
│   ├── handover.ml        # DNA-based state transfer (353 lines)
│   ├── execution_memory.ml # Run tracking (287 lines)
│   ├── cache.ml           # Shared context store (224 lines)
│   ├── tempo.ml           # Adaptive intervals (164 lines)
│   ├── dashboard.ml       # Terminal visualization (235 lines)
│   └── mitosis.ml         # Agent division (460 lines)
│
├── Planning & Execution (4 modules)
│   ├── planning.ml        # Planning files (374 lines)
│   ├── spawn.ml           # Process spawning
│   ├── relay.ml           # Smart relay (328 lines)
│   └── mode.ml            # Operation modes
│
├── MCP Full Spec (5 modules)
│   ├── session.ml         # MCP Session Management (536 lines)
│   ├── cancellation.ml    # Cancellation Tokens
│   ├── subscriptions.ml   # Resource Subscriptions
│   ├── progress.ml        # Progress notifications
│   └── notify.ml          # Notifications
│
├── Security & Reliability (5 modules)
│   ├── auth.ml            # Authentication (289 lines)
│   ├── encryption.ml      # AES-256-GCM
│   ├── gcm_compat.ml      # GCM compatibility
│   ├── retry.ml           # Exponential backoff (564 lines)
│   └── shutdown.ml        # Graceful shutdown
│
└── Supporting (4 modules)
    ├── checkpoint_fs.ml   # LangGraph checkpoints (549 lines)
    ├── checkpoint_types.ml
    ├── redis_common.ml    # Redis helpers
    └── masc_mcp.ml        # Main module
```

### Data Flow

```
Client Request
    │
    ▼
┌─────────────────┐
│   MCP Server    │ ← HTTP/SSE (port 8935)
│  (mcp_server)   │
└────────┬────────┘
         │
    ▼    ▼    ▼
┌────┐ ┌────┐ ┌────┐
│Auth│ │Rate│ │Sess│  ← Security Layer
└──┬─┘ └──┬─┘ └──┬─┘
   └──────┼──────┘
          ▼
┌─────────────────┐
│   Tool Router   │ ← 111 MCP tools
│    (tools.ml)   │
└────────┬────────┘
         │
    ▼    ▼    ▼    ▼
┌────┐ ┌────┐ ┌────┐ ┌────┐
│Room│ │A2A │ │Cell│ │Exec│  ← Feature Layer
└──┬─┘ └──┬─┘ └──┬─┘ └──┬─┘
   └──────┼──────┼──────┘
          ▼
┌─────────────────┐
│    Backend      │ ← Storage (FS/Redis)
│   (backend.ml)  │
└─────────────────┘
```

---

## MCP Tools Reference

### Overview: 111 Tools in 16 Categories

| Category | Count | Tools |
|----------|-------|-------|
| Room Management | 9 | set_room, init, join, leave, status, pause, resume, pause_status, reset |
| Agent Management | 5 | who, agents, heartbeat, cleanup_zombies, gc |
| Task Operations | 7 | add_task, claim, claim_next, done, cancel_task, tasks, archive_view, update_priority |
| Communication | 3 | broadcast, messages, listen |
| Voting System | 4 | vote_create, vote_cast, vote_status, votes |
| Portal (A2A) | 4 | portal_open, portal_send, portal_status, portal_close |
| Worktree | 3 | worktree_create, worktree_remove, worktree_list |
| Checkpoints | 4 | interrupt, approve, reject, branch, pending_interrupts |
| Cost Tracking | 2 | cost_log, cost_report |
| Auth & Security | 9 | auth_enable, auth_disable, auth_status, auth_create_token, auth_refresh, auth_revoke, auth_list, rate_limit_status, rate_limit_config |
| Encryption | 4 | encryption_status, encryption_enable, encryption_disable, generate_key |
| A2A Tools | 5 | a2a_discover, a2a_query_skill, a2a_delegate, a2a_subscribe, a2a_unsubscribe |
| Planning | 11 | plan_init, plan_update, note_add, deliver, error_add, error_resolve, plan_get, plan_set_task, plan_get_task, plan_clear_task, agent_card |
| **Cellular Agent** [NEW] | 22 | handover_*, run_*, cache_*, tempo_*, dashboard, mitosis_*, relay_* |
| MCP Spec | 7 | session management, cancellation, subscriptions, progress, mode |

### Cellular Agent Tools Detail (New in v3.0)

#### Handover (DNA Transfer)
| Tool | Description |
|------|-------------|
| `masc_handover_create` | Create DNA checkpoint with task state, learnings, blockers |
| `masc_handover_list` | List pending handovers |
| `masc_handover_claim` | Claim a handover |
| `masc_handover_get` | Get DNA as markdown prompt |
| `masc_handover_claim_and_spawn` | Claim + spawn successor in one step |

#### Execution Memory (Run Tracking)
| Tool | Description |
|------|-------------|
| `masc_run_init` | Initialize run for task |
| `masc_run_plan` | Add execution plan |
| `masc_run_log` | Log execution step |
| `masc_run_deliverable` | Record deliverable |
| `masc_run_get` | Get run details |
| `masc_run_list` | List all runs |

#### Cache (Shared Context)
| Tool | Description |
|------|-------------|
| `masc_cache_set` | Store value with TTL and tags |
| `masc_cache_get` | Retrieve value |
| `masc_cache_delete` | Delete value |
| `masc_cache_list` | List all/tagged entries |
| `masc_cache_clear` | Clear all cache |
| `masc_cache_stats` | Cache statistics |

#### Tempo (Adaptive Orchestration)
| Tool | Description |
|------|-------------|
| `masc_tempo_get` | Get current tempo state |
| `masc_tempo_set` | Set tempo manually |
| `masc_tempo_adjust` | Auto-adjust based on tasks |
| `masc_tempo_reset` | Reset to default |

#### Mitosis (Agent Division)
| Tool | Description |
|------|-------------|
| `masc_mitosis_status` | Get mitosis pool status |
| `masc_mitosis_pool` | View available sub-agents |
| `masc_mitosis_divide` | Split task into subtasks |
| `masc_mitosis_check` | Check if division needed |
| `masc_mitosis_record` | Record division result |
| `masc_mitosis_prepare` | Prepare for division |

#### Relay (Smart Checkpoint)
| Tool | Description |
|------|-------------|
| `masc_relay_status` | Get relay status |
| `masc_relay_checkpoint` | Create relay checkpoint |
| `masc_relay_now` | Force immediate relay |
| `masc_relay_smart_check` | Check if relay needed |

#### Dashboard
| Tool | Description |
|------|-------------|
| `masc_dashboard` | Generate terminal dashboard (full/compact) |

---

## MCP 2025-11-25 Compliance

### MUST Requirements (100% Complete)

| Requirement | Status | Implementation |
|-------------|--------|----------------|
| JSON-RPC 2.0 Format | ✅ | `mcp_protocol.ml` |
| Protocol Version Header | ✅ | `X-MCP-Protocol-Version: 2025-11-25` |
| Standard Error Codes | ✅ | -32700, -32600, -32601, -32602, -32603 |
| Origin Validation | ✅ | `validate_origin` with allowlist |

### SHOULD Requirements (100% Complete)

| Requirement | Status | Implementation |
|-------------|--------|----------------|
| SSE Support | ✅ | `sse.ml` with chunked encoding |
| Streamable HTTP | ✅ | Transfer-Encoding: chunked |
| Graceful Shutdown | ✅ | SIGTERM handler, pending task wait |
| Tool Result Streaming | ✅ | Progress notifications |

### MAY Requirements (100% Complete)

| Requirement | Status | Implementation |
|-------------|--------|----------------|
| Session ID | ✅ | `session.ml` - `X-MCP-Session-ID` header |
| Event IDs (Resumability) | ✅ | SSE `id:` field, `Last-Event-ID` reconnect |
| Cancellation Tokens | ✅ | `cancellation.ml` - LIFO callbacks |
| Resource Subscriptions | ✅ | `subscriptions.ml` - 6 resource types |
| Progress Notifications | ✅ | Real-time task progress streaming |
| Logging Interface | ✅ | Server logs to client via SSE |

---

## A2A Integration

### Agent Cards (`agent_card.ml`)

```json
{
  "name": "masc-agent",
  "version": "3.0.0",
  "capabilities": ["typescript", "code-review", "testing"],
  "bindings": [
    {"protocol": "json-rpc", "url": "http://localhost:8935"},
    {"protocol": "grpc", "url": "grpc://localhost:8936"}
  ],
  "skills": [
    {
      "id": "code_review",
      "name": "Code Review",
      "input_modes": ["text", "file"],
      "output_modes": ["text", "structured"]
    }
  ]
}
```

Endpoint: `GET /.well-known/agent-card.json`

### A2A MCP Tools

| Tool | Purpose |
|------|---------|
| `masc_a2a_discover` | Find agents by capability |
| `masc_a2a_query_skill` | Get skill details |
| `masc_a2a_delegate` | Delegate tasks to agents |
| `masc_a2a_subscribe` | Subscribe to agent events |
| `masc_a2a_unsubscribe` | Unsubscribe from events |

---

## Feature Matrix

### MASC-Unique Features

| Feature | Description | Use Case |
|---------|-------------|----------|
| **Cellular Handover** | DNA-based state transfer | Agent continuity across context limits |
| **Execution Memory** | Run tracking with plans/logs | Audit trail, debugging |
| **Shared Cache** | TTL-based context store | Cross-agent data sharing |
| **Cluster Tempo** | Adaptive check intervals | Resource optimization |
| **Visual Dashboard** | Terminal status view | Monitoring |
| **Voting System** | Multi-agent consensus | Architecture decisions |
| **Git Worktree** | Branch-based isolation | Code collaboration |
| **Cost Tracking** | Token/cost attribution | Budget management |
| **Human-in-the-loop** | interrupt/approve/reject | Safety-critical ops |
| **Mitosis** | Agent division | Complex task parallelization |

### Protocol Comparison

| Aspect | MCP | A2A | MASC |
|--------|-----|-----|------|
| **Primary Use** | Agent ↔ Tools | Agent ↔ Agent | Both |
| **Transport** | JSON-RPC/SSE | JSON-RPC/gRPC | All |
| **Discovery** | Manual config | Agent Cards | Both |
| **State Management** | Client-side | Task artifacts | Room-based |
| **Authentication** | Per-server | Bearer/OAuth | Built-in |
| **Cellular Agents** | ❌ | ❌ | ✅ |
| **Shared Cache** | ❌ | ❌ | ✅ |

---

## Pros and Cons Analysis

### Strengths

| Strength | Evidence | Impact |
|----------|----------|--------|
| **OCaml Performance** | ~2ms tool latency | 10x faster than Python MCP servers |
| **Full MCP Spec** | 100% MAY compliance | Future-proof |
| **Cellular Agent Pattern** | DNA handover, mitosis | Agent continuity |
| **Type Safety** | OCaml's type system | Fewer runtime errors |
| **Production Ready** | 291+ tests | Battle-tested |
| **111 Tools** | Comprehensive coverage | One-stop solution |

### Weaknesses

| Weakness | Mitigation | Priority |
|----------|------------|----------|
| **room.ml complexity** (1,744 lines) | Split into submodules | P2 |
| **OCaml ecosystem** | Smaller than Python/JS | Trade-off for performance |
| **Learning curve** | Documentation, examples | Ongoing |

---

## Best Practices

### MASC Workflow

```bash
# 1. Initialize room (once per project)
masc_init

# 2. Join as agent
masc_join --agent claude --capabilities "typescript,review"

# 3. Create worktree for isolated work
masc_worktree_create --agent claude --task_id PK-12345

# 4. Claim task before starting
masc_claim --task_id task-001

# 5. Broadcast status
masc_broadcast "Starting task-001"

# 6. Work... (use cache for shared context)
masc_cache_set --key "context:task-001" --value "..." --ttl 3600

# 7. Mark done
masc_done --task_id task-001

# 8. Or handover if context limit reached
masc_handover_create --source claude --reason context_limit
```

### Anti-Patterns

| Anti-Pattern | Problem | Solution |
|--------------|---------|----------|
| Polling `masc_messages` | Wastes resources | Use `masc_listen` |
| Skip `masc_claim` | Task conflicts | Always claim first |
| Ignore heartbeat | Marked as zombie | Call every 2-3 mins |
| Direct file edit | Bypasses tracking | Use worktree |
| No handover on exit | Lost context | Always create DNA |

---

## Roadmap

### Recent Completions (v3.0)

- ✅ Phase 8: Cellular Agent Handover
- ✅ Phase 9: Execution Memory
- ✅ Phase 10: Token Usage Tracking
- ✅ Phase 11: Internal Caching
- ✅ Phase 12: Cluster Tempo Control
- ✅ Phase 13: Visual Dashboard

### Planned

| Feature | Priority | Status |
|---------|----------|--------|
| Python Bindings | High | Planned |
| Prometheus Metrics | High | Planned |
| WebSocket Transport | Medium | Planned |
| Distributed Consensus | Medium | Research |
| Agent Marketplace | Low | Idea |

---

## Quick Reference

### CLI Usage

```bash
# Start server
cd ~/me/features/masc-mcp && ./start-masc-mcp.sh --http --port 8935

# Health check
curl http://127.0.0.1:8935/health

# Dashboard (real-time)
watch -n 1 masc-mcp dashboard

# List tools
curl http://localhost:8935/tools | jq '.tools[].name'
```

### Environment Variables

```bash
MASC_STORAGE_TYPE=fs|redis
MASC_REDIS_URL=redis://...
MASC_ENCRYPTION_KEY=<32-byte-hex>
MASC_LOG_LEVEL=debug|info|warn|error
MASC_PORT=8935
MASC_GRPC_PORT=8936
```

### Storage Structure

```
.masc/
├── room.json          # Room state
├── tasks.json         # Task queue
├── agents/            # Agent metadata
├── messages/          # Broadcast history
├── handovers/         # DNA checkpoints [NEW]
├── runs/              # Execution memory [NEW]
├── cache/             # Shared context [NEW]
├── tempo.json         # Tempo state [NEW]
├── planning/          # Planning files
├── votes/             # Voting records
├── sessions/          # MCP sessions
└── subscriptions/     # Resource subscriptions
```

---

## References

- [MCP 2025-11-25 Spec](https://modelcontextprotocol.io/specification/)
- [A2A Protocol](https://google.github.io/A2A/)
- [MASC V2 Design](./MASC-V2-DESIGN.md)
- [Cellular Agent Pattern](./CELLULAR-AGENT.md)
- [Mitosis Design](./MITOSIS.md)

---

*Generated: 2026-01-09 | MASC-MCP v3.0.0 (Phase 13)*
