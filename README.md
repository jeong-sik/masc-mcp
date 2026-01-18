# MASC MCP

[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/jeong-sik/masc-mcp)
[![OCaml](https://img.shields.io/badge/OCaml-5.x-orange.svg)](https://ocaml.org/)
[![MCP](https://img.shields.io/badge/MCP-2025--11--25-blue.svg)](https://spec.modelcontextprotocol.io/)
[![Status](https://img.shields.io/badge/status-Production%20Ready-green.svg)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Tests](https://img.shields.io/badge/Tests-385%20passed-green.svg)]()

**M**ulti-**A**gent **S**treaming **C**oordination - MCP Server

A native OCaml implementation for coordinating multiple AI agents (Claude, Gemini, Codex) working together on the same codebase.

## 5-Minute Quick Start

```bash
# 1. Start server (auto-builds if needed)
./start-masc-mcp.sh --http --port 8935

# 2. Verify it's running
curl http://127.0.0.1:8935/health
# {"status":"ok","server":"masc-mcp"...}

# 3. Add to Claude Code config (~/.claude/config.json)
{
  "mcpServers": {
    "masc": { "type": "http", "url": "http://127.0.0.1:8935/mcp" }
  }
}

# 4. Use in Claude Code
masc_join(agent_name: "claude")
masc_add_task(title: "My first task")
masc_claim(agent_name: "claude", task_id: "task-001")
masc_done(agent_name: "claude", task_id: "task-001")
```

**Prerequisites**: OCaml 5.x + opam + dune 3.x (see [Full Installation](#installation) below)

---

## Why MASC?

When multiple AI agents work on the same project, they need:
- **Task coordination** - Who's working on what?
- **Isolation** - Prevent simultaneous edits (via Git worktree)
- **Communication** - Share progress and findings
- **Human oversight** - Approve dangerous operations

MASC provides all of this through the [Model Context Protocol](https://modelcontextprotocol.io/).

## Features

| Feature | Description |
|---------|-------------|
| **Task Board** | Claim tasks, track progress, priority queue |
| **Worktree Isolation** | Each agent gets isolated git worktree |
| **Broadcast** | @mention agents, real-time SSE notifications |
| **Portal (A2A)** | Direct agent-to-agent communication |
| **Human-in-the-loop** | Interrupt pattern for dangerous operations |
| **Cost Tracking** | Monitor token usage across agents |
| **Zombie Cleanup** | Auto-remove stale agents |
| **Encryption** | AES-256-GCM protection for sensitive data |
| **Mode System** | Toggle feature categories to reduce token usage |

## Mode System (Token Optimization)

MASC provides **Serena-style mode switching** to reduce token usage by enabling only the categories you need.

### Available Modes

| Mode | Tools | Token Reduction | Use Case |
|------|-------|-----------------|----------|
| `full` | 58 | - | All features |
| `standard` | 28 | **52%** | Default: core + comm + worktree + health |
| `minimal` | 18 | **69%** | Solo work: core + health only |
| `solo` | 17 | **71%** | Single-agent: core + worktree |

### Quick Usage

```bash
# Switch to minimal mode (fastest, least tokens)
masc_switch_mode(mode: "minimal")

# Switch to full mode (all features)
masc_switch_mode(mode: "full")

# Custom: enable specific categories
masc_switch_mode(mode: "custom", categories: ["core", "worktree", "portal"])

# Check current configuration
masc_get_config()
```

### Categories

| Category | Tools | Description |
|----------|-------|-------------|
| `core` | 12 | Task board, status, claim, done |
| `comm` | 7 | Broadcast, messages, listen |
| `portal` | 4 | Agent-to-agent direct communication |
| `worktree` | 3 | Git worktree isolation |
| `health` | 4 | Heartbeat, zombies, GC, agents |
| `discovery` | 2 | Find agents by capability |
| `voting` | 4 | Multi-agent consensus |
| `interrupt` | 5 | Human-in-the-loop approvals |
| `cost` | 2 | Token usage tracking |
| `auth` | 7 | Token-based authentication |
| `ratelimit` | 2 | Request throttling |
| `encryption` | 4 | AES-256-GCM data protection |

See [docs/MODE-SYSTEM.md](docs/MODE-SYSTEM.md) for detailed documentation.

## Quick Start

### Prerequisites

- OCaml 5.x with opam
- dune 3.x

### Installation

```bash
git clone https://github.com/jeong-sik/masc-mcp.git
cd masc-mcp

# Install dependencies
opam install . --deps-only

# Build
dune build

# Test
dune test
# 291+ tests passed

# Skip network-dependent tests (STUN/UDP bind)
MASC_SKIP_NET_TESTS=1 dune test
```

### Running the Server

```bash
# HTTP mode (default, SSE)
./start-masc-mcp.sh --port 8935

# Stdio mode (legacy)
./start-masc-mcp.sh --stdio

# Check health
curl http://127.0.0.1:8935/health
```

### Room & Cluster Architecture

MASC uses a **Room** concept to coordinate agents. A Room is defined by its base path (`.masc/` directory location).

```
┌─────────────────────────────────────────────────────────────────┐
│  Room = Base Path + .masc/ folder                               │
│  Cluster = Named group of agents sharing the same Room          │
└─────────────────────────────────────────────────────────────────┘
```

#### How Clustering Works

| Mode | Room Determined By | Use Case |
|------|-------------------|----------|
| **FS (Local)** | `--path` argument or `$ME_ROOT` | Single machine |
| **Redis (Distributed)** | `MASC_CLUSTER_NAME` + Redis URL | Multi-machine (Lwt server) |
| **PostgreSQL (Distributed)** | `MASC_CLUSTER_NAME` + Postgres URL | Multi-machine (Eio server, recommended) |

```bash
# FS Mode: agents on same machine share ~/me/.masc/
./start-masc-mcp.sh --path ~/me --port 8935

# Redis Mode: agents on different machines share "myproject" cluster
export MASC_CLUSTER_NAME=myproject
export MASC_REDIS_URL=redis://...
./start-masc-mcp.sh --port 8935
```

#### Cluster Naming

When `MASC_CLUSTER_NAME` is not set, it defaults to `basename($ME_ROOT)`:
- `ME_ROOT=/Users/alice/me` → cluster `"me"`
- `ME_ROOT=/Users/bob/workspace/kidsnote` → cluster `"kidsnote"`

### Storage Backend Configuration

MASC supports multiple storage backends for distributed coordination:

#### Default: File-Based (SQLite-like)

No configuration needed. State stored in `.masc/` directory.

```bash
# Verify local storage
ls -la .masc/
```

#### Redis (Distributed Multi-Machine)

Enable Redis for multi-machine agent coordination (Claude on Mac, Gemini on Linux):

```bash
# Environment variables
export MASC_STORAGE_TYPE=redis
export MASC_REDIS_URL="redis://[user:password@]host:port"
```

**URL Scheme Detection**:
| Scheme | Backend | Use Case |
|--------|---------|----------|
| `redis://` | RedisNative (TCP) | Direct connection, fastest |
| `rediss://` | RedisNative (TLS) | Secure connection |
| `https://` | REST API | Upstash, serverless Redis |

**Example: Railway Redis**:
```bash
# ~/.zshenv
export MASC_STORAGE_TYPE=redis
export MASC_REDIS_URL="redis://default:xxx@trolley.proxy.rlwy.net:24277"

# Restart server
./start-masc-mcp.sh --http --port 8935

# Verify Redis connection
curl http://127.0.0.1:8935/health
# {"status":"ok","backend":"redis",...}
```

**Features with Redis**:
- Atomic distributed locks (`SET NX EX`)
- Real-time message synchronization
- Cross-machine task coordination
- Automatic key namespacing (`masc:*`)

#### PostgreSQL (Recommended for Eio Server)

Enable PostgreSQL for stable, non-blocking distributed coordination. **Recommended for Eio-native server** (`masc-mcp-eio`) as it uses caqti-eio for true async database access.

```bash
# Environment variables
export MASC_STORAGE_TYPE=postgres
export MASC_POSTGRES_URL="postgres://[user:password@]host:port/database"
# Alternative: DATABASE_URL or RAILWAY_PG_URL are also detected
```

**Example: Railway PostgreSQL**:
```bash
# ~/.zshenv
export MASC_STORAGE_TYPE=postgres
export MASC_POSTGRES_URL="$RAILWAY_PG_URL"  # or set directly

# Start Eio server (recommended for PostgreSQL)
./start-masc-mcp.sh --http --port 8935 --eio

# Verify PostgreSQL connection
curl http://127.0.0.1:8935/health
# {"status":"ok","server":"masc-mcp-eio","backend":"postgres",...}
```

**Required Table** (auto-created or manual):
```sql
CREATE TABLE IF NOT EXISTS masc_kv (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    expires_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_masc_kv_expires ON masc_kv(expires_at);
```

**Why PostgreSQL over Redis for Eio?**:
| Feature | Redis (Lwt) | PostgreSQL (Eio) |
|---------|-------------|------------------|
| Async Model | Lwt monadic | Eio direct-style |
| Blocking Risk | Possible (Lwt_eio bridge) | None (native caqti-eio) |
| Connection Pool | Manual | Built-in (caqti pool) |
| Transactions | Limited | Full ACID |

### macOS Notifications (Click Focus)

MASC sends macOS notifications for mentions, portal messages, interrupts, and task completion.
If `terminal-notifier` is installed, you can attach a click action to focus the right terminal window.

Environment options:

- `MASC_NOTIFY_FOCUS_CMD`: custom command template (tokens: `{{target}}`, `{{from}}`, `{{task}}`)
- `MASC_NOTIFY_FOCUS_APP`: app name to activate (e.g., `Alacritty`, `Terminal`)
- `MASC_TMUX_SESSION`: tmux session name (selects window `session:{{target}}`)
- `MASC_NOTIFY_FOCUS_ON_OSASCRIPT`: set to `1` to auto-focus when only osascript is available

Example:

```bash
export MASC_NOTIFY_FOCUS_APP="Alacritty"
export MASC_TMUX_SESSION="masc"
# If terminal-notifier is missing, auto-focus immediately after osascript alert
export MASC_NOTIFY_FOCUS_ON_OSASCRIPT=1
# Optional: override with a custom command
# export MASC_NOTIFY_FOCUS_CMD="open -a 'Alacritty'; tmux select-window -t masc:{{target}}"
```

### Environment Variables Reference

| Variable | Default | Description |
|----------|---------|-------------|
| **Core** | | |
| `ME_ROOT` | - | Base path for `.masc/` directory |
| `MASC_BASE_PATH` | `$ME_ROOT` | Override base path |
| `MASC_CLUSTER_NAME` | `basename($ME_ROOT)` | Cluster name for Redis mode |
| **Storage** | | |
| `MASC_STORAGE_TYPE` | `fs` | `fs`, `redis`, or `postgres` |
| `MASC_REDIS_URL` | - | Redis connection URL |
| `MASC_POSTGRES_URL` | - | PostgreSQL connection URL (also: `DATABASE_URL`, `RAILWAY_PG_URL`) |
| **Security** | | |
| `MASC_ENCRYPTION_KEY` | - | AES-256-GCM key (base64) |
| **Notifications** | | |
| `MASC_NOTIFY_FOCUS_APP` | - | App to focus on notification click |
| `MASC_NOTIFY_FOCUS_CMD` | - | Custom focus command |
| `MASC_TMUX_SESSION` | - | tmux session for window selection |
| `MASC_NOTIFY_FOCUS_ON_OSASCRIPT` | `0` | Auto-focus with osascript |
| **Testing** | | |
| `MASC_SKIP_NET_TESTS` | `0` | Skip STUN/UDP tests |

### MCP Configuration

Add to `~/.claude/config.json`:

```json
{
  "mcpServers": {
    "masc": {
      "type": "http",
      "url": "http://127.0.0.1:8935/mcp"
    }
  }
}
```

## Usage Examples

### Multi-Agent Collaboration

```bash
# Claude initializes room and creates task
masc_init(agent_name: "claude")
masc_add_task(title: "Review PR #1234", priority: 1)

# Claude claims and starts working
masc_claim(agent_name: "claude", task_id: "task-001")
masc_broadcast(agent_name: "claude", message: "Starting PR review")

# Ask Gemini for help with @mention
masc_broadcast(agent_name: "claude", message: "@gemini check for security issues")

# Gemini joins and responds
masc_join(agent_name: "gemini")
masc_broadcast(agent_name: "gemini", message: "@claude SQL injection at line 42")

# Claude completes task
masc_done(agent_name: "claude", task_id: "task-001", notes: "Fixed security issue")
```

### Worktree Isolation (No Conflicts!)

```bash
# Each agent gets isolated workspace
masc_worktree_create(agent_name: "claude", task_id: "feature-A")
# Creates: .worktrees/claude-feature-A/

masc_worktree_create(agent_name: "gemini", task_id: "feature-B")
# Creates: .worktrees/gemini-feature-B/

# Work independently, then cleanup
masc_worktree_remove(agent_name: "claude", task_id: "feature-A")
```

### TURN Relay Example (ICE)

```bash
TURN_URL=turn:host:3478?transport=udp \
TURN_USERNAME=... \
TURN_CREDENTIAL=... \
dune exec examples/turn_relay_example.exe
```

For `turns:` URLs, set `TURN_TLS_CA` (and optionally `TURN_TLS_CERT`/`TURN_TLS_KEY`).
Credentials: prefer `TURN_CREDENTIAL`. `TURN_PASSWORD` is accepted as an alias in examples.
System CA examples: macOS `/etc/ssl/cert.pem`, Debian/Ubuntu `/etc/ssl/certs/ca-certificates.crt`.

### TURN TLS Example (Allocation)

```bash
TURN_URL=turns:localhost:5349?transport=udp \
TURN_USERNAME=... \
TURN_CREDENTIAL=... \
TURN_TLS_CA=/path/to/turn.crt \
dune exec examples/turn_tls_example.exe
```

Optional: set `TURN_TLS_CERT` and `TURN_TLS_KEY` for mTLS.

### TURN Relay Data Example (E2E)

```bash
TURN_URL=turns:turn.localhost:5349?transport=udp \
TURN_USERNAME=... \
TURN_CREDENTIAL=... \
TURN_TLS_CA=/path/to/rootCA.pem \
TURN_PEER_HOST=host.docker.internal \
TURN_PEER_PORT=7001 \
TURN_MESSAGES=5 \
dune exec examples/turn_relay_data_example.exe
```

Defaults: `TURN_PEER_HOST=host.docker.internal`, `TURN_PEER_PORT=7001`, `TURN_MESSAGES=1`.
Set `TURN_LOCAL_ECHO=0` if you run an external UDP echo server.
Set `TURN_USE_CHANNEL=0` to force Send Indication (no ChannelBind).
If `host.docker.internal` doesn't resolve on your host, set `TURN_PEER_HOST` to your LAN IP.

### TURN Production Checklist (Self-Host)

Use this only if you want a dedicated TURN endpoint for validation/regression tests. The library does not require a TURN server to be always-on.

1. Provision a public VM and point a DNS A record (e.g. `turn.example.com`) at the VM IP.
2. Install coturn and set `listening-ip`, `relay-ip`, `external-ip`, `realm`, `user`, and a relay port range.
3. Open firewall: `3478/tcp+udp`, `5349/tcp+udp`, and the relay port range.
4. Issue TLS cert via ACME DNS-01 (e.g. Cloudflare) and install it to coturn with a reload command.
5. Validate with `turn_tls_example.exe` and `turn_relay_data_example.exe` using `TURN_TLS_CA`.

### Human Approval for Dangerous Operations

```bash
# Agent detects dangerous operation
masc_interrupt(
  agent_name: "claude",
  task_id: "cleanup-job",
  step: 3,
  action: "DELETE FROM users WHERE inactive=true",
  message: "Delete 1,247 records from production?"
)

# User reviews and approves/rejects
masc_approve(task_id: "cleanup-job")
# or
masc_reject(task_id: "cleanup-job", reason: "Backup first")
```

## Best Practices & Benchmarks

### Collaboration Patterns

| 패턴 | 에이전트 | 용도 | 문서 |
|------|----------|------|------|
| **Code Review Pipeline** | Claude + Gemini | PR 다각도 리뷰 | [예제](examples/BEST-PRACTICES.md#pattern-1-code-review-pipeline) |
| **Parallel Research** | 3+ agents | 병렬 정보 수집 | [예제](examples/BEST-PRACTICES.md#pattern-2-parallel-research) |
| **Swarm Decision** | MAGI Trinity | 아키텍처 결정 | [예제](examples/BEST-PRACTICES.md#pattern-3-swarm-decision-making) |

### Performance Benchmarks

```bash
# Run benchmarks
./benchmarks/quick-bench.sh
```

| Operation | Latency (avg) | Status |
|-----------|---------------|--------|
| `masc_status` | 33ms | 🟢 |
| `masc_broadcast` | 33ms | 🟢 |
| `masc_a2a_discover` | 29ms | 🟢 |

> 📊 자세한 내용: [examples/BEST-PRACTICES.md](examples/BEST-PRACTICES.md)

---

## MCP Tools (111)

### Core (22)
| Tool | Description |
|------|-------------|
| `masc_init` | Initialize room |
| `masc_join` / `masc_leave` | Agent lifecycle |
| `masc_add_task` | Create task |
| `masc_claim` / `masc_done` | Task workflow |
| `masc_claim_next` | Auto-claim highest priority |
| `masc_broadcast` | Send message |
| `masc_portal_*` | Direct A2A communication |
| `masc_worktree_*` | Git worktree management |

### Checkpoint (5) - Human-in-the-loop
| Tool | Description |
|------|-------------|
| `masc_interrupt` | Pause for approval |
| `masc_approve` / `masc_reject` | User decision |
| `masc_pending_interrupts` | List waiting |
| `masc_branch` | Fork execution path |

### Cost (2)
| Tool | Description |
|------|-------------|
| `masc_cost_log` | Log token usage |
| `masc_cost_report` | Generate report |

### Auth (7) - Role-Based Access Control
| Tool | Description |
|------|-------------|
| `masc_auth_enable` | Enable authentication (returns room secret) |
| `masc_auth_disable` | Disable authentication |
| `masc_auth_status` | Check auth status |
| `masc_auth_create_token` | Create token for agent |
| `masc_auth_refresh` | Refresh expired token |
| `masc_auth_revoke` | Revoke agent token |
| `masc_auth_list` | List all credentials |

**Roles**: `reader` (read-only) → `worker` (claim/lock/broadcast) → `admin` (full access)

### Rate Limiting (2)
| Tool | Description |
|------|-------------|
| `masc_rate_limit_status` | Check your rate limit status |
| `masc_rate_limit_config` | View rate limit configuration |

**Categories**: General (10/min), Broadcast (15/min), TaskOps (30/min), FileLock (20/min)

**Role Multipliers**: Reader (0.5x), Worker (1.0x), Admin (2.0x)

**Burst**: 5 extra requests per minute when over limit

### Encryption (4) - Data Protection
| Tool | Description |
|------|-------------|
| `masc_encryption_status` | Check encryption status |
| `masc_encryption_enable` | Enable AES-256-GCM encryption |
| `masc_encryption_disable` | Disable encryption |
| `masc_generate_key` | Generate new 256-bit key |

**Algorithm**: AES-256-GCM (AEAD - authenticated encryption)

**Key Sources**: Environment variable (`MASC_ENCRYPTION_KEY`) or file

## Architecture

```
.masc/                    # Room state (file-based)
├── agents/               # Agent registry
├── tasks/                # Task backlog
├── locks/                # File locks
├── messages/             # Broadcast history
├── portals/              # A2A connections
├── checkpoints/          # Interrupt state
└── auth/                 # Authentication (NEW)
    ├── config.json       # Auth settings
    ├── room_secret.hash  # Room secret (SHA256)
    └── agents/           # Agent tokens
        ├── claude.json
        └── gemini.json

lib/
├── types.ml              # Domain types (ppx_deriving)
├── room.ml               # Core logic (Result types)
├── auth.ml               # Auth module
├── encryption.ml         # AES-256-GCM encryption
├── session.ml            # Session management + rate limiting
├── mcp_server.ml         # JSON-RPC + SSE
└── tools.ml              # MCP tool schemas
```

## Type Safety

MASC uses OCaml's type system for compile-time safety:

```ocaml
(* ADT for errors - no stringly-typed errors *)
type masc_error =
  | NotInitialized
  | TaskNotFound of string
  | TaskAlreadyClaimed of { task_id: string; by: string }
  | FileLocked of { file: string; by: string }
  | InvalidAgentName of string
  | InvalidFilePath of string
  ...

(* Result type for all operations *)
type 'a masc_result = ('a, masc_error) result

(* Validation at the edge *)
let claim_task_r config ~agent_name ~task_id =
  match validate_agent_name_r agent_name, validate_task_id_r task_id with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok _, Ok _ -> (* proceed with validated inputs *)
```

## Security

### Network Security

⚠️ **Important**: By default, MASC binds to `0.0.0.0` (all interfaces).

| Environment | Risk Level | Recommendation |
|-------------|------------|----------------|
| Home/Office Wi-Fi | 🟢 Low | NAT protection, firewall |
| Public Wi-Fi (café) | 🔴 High | Stop server or use firewall |
| Internet-exposed | 🔴 Critical | Never expose directly |

**Mitigation Options**:

```bash
# Option 1: macOS firewall (check status)
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate

# Option 2: Stop server when not needed
pkill -f "masc-mcp.*main.exe"

# Option 3: Use Unix socket (requires code change - see Roadmap)
```

**What's exposed if accessed**:
- Agent status and task list (no secrets)
- Broadcast messages
- File lock state
- **NOT exposed**: passwords, API keys, file contents

### Application Security

- **XSS Prevention**: HTML entities escaped in messages
- **Path Traversal**: Blocked (`..`, absolute paths)
- **Input Validation**: All inputs validated with ADT errors
- **File Locking**: Atomic operations with `O_EXCL`

## MCP Protocol Support

Implements [MCP Spec 2025-11-25](https://spec.modelcontextprotocol.io/):

- `GET /mcp` - SSE stream for server notifications
- `POST /mcp` - JSON-RPC requests
- `DELETE /mcp` - Session termination
- `MCP-Protocol-Version` header negotiation
- `Mcp-Session-Id` for session tracking

## Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create feature branch (`git checkout -b feature/amazing`)
3. Write tests for new functionality
4. Ensure all tests pass (`dune test`)
5. Submit PR

## Roadmap

- [x] Authentication/Authorization (P1 - Complete)
- [x] Rate limiting (P2 - Complete)
- [x] Encrypted storage (P3 - Complete, AES-256-GCM)
- [ ] Metrics/Monitoring (P4)
- [ ] Retry/Recovery logic (P5)
- [x] Distributed mode (Redis backend) (P6 - Complete, dual-protocol)
- [ ] Room-to-Room portals
- [ ] Localhost-only binding option (Unix socket or 127.0.0.1)

## License

MIT License - See [LICENSE](LICENSE) for details.

## Acknowledgments

- Inspired by [LangGraph's Interrupt Pattern](https://langchain-ai.github.io/langgraph/concepts/human_in_the_loop/)
- Built for the [Claude Code](https://claude.ai) ecosystem
- MCP specification by [Anthropic](https://anthropic.com)
