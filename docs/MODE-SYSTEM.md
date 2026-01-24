# MASC Mode System

> Serena-style tool filtering for token optimization

## Overview

MASC MCP provides 140+ tools across 12 categories. Most agents don't need all of them. The **Mode System** lets you enable only the categories you use, reducing token consumption by narrowing the enabled tool surface.

## Why Mode System?

| Problem | Solution |
|---------|----------|
| Full tool list is heavy | Filter to reduce token usage |
| Single agent doesn't need voting | Use `solo` mode |
| Quick task doesn't need encryption | Use `minimal` mode |
| CI/CD automation | Use `standard` mode |

## Modes

### Preset Modes

| Mode | Categories | Tools | Tokens (approx) |
|------|------------|-------|-----------------|
| **full** | All 12 | dynamic | varies |
| **parallel** | core, comm, portal, worktree, health, discovery, voting, interrupt | dynamic | varies |
| **standard** | core, comm, worktree, health | dynamic | varies |
| **minimal** | core, health | dynamic | varies |
| **solo** | core, worktree | dynamic | varies |

### Mode Selection Guide

```
┌─────────────────────────────────────────────────────────────┐
│ How many agents are working?                                │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Single Agent ─────────┬───── Need worktree? ─── Yes ──► solo
│                        │                                    │
│                        └───── Just tasks? ────── Yes ──► minimal
│                                                             │
│  Multiple Agents ──────┬───── Heavy parallel? ─── Yes ──► parallel
│                        │                                    │
│                        ├───── Basic collab? ──── Yes ──► standard
│                        │                                    │
│                        └───── Full features? ─── Yes ──► full
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Categories

### Core (12 tools) - Always recommended

Essential task management. Included in all modes.

```
masc_set_room, masc_init, masc_join, masc_leave, masc_status,
masc_add_task, masc_claim, masc_done, masc_tasks, masc_archive_view,
masc_claim_next, masc_update_priority
```

### Communication (dynamic)

Real-time messaging between agents.

```
masc_broadcast, masc_messages, masc_lock, masc_unlock, masc_listen, masc_who, masc_reset
```

### Portal (4 tools)

Direct agent-to-agent private channels.

```
masc_portal_open, masc_portal_send, masc_portal_close, masc_portal_status
```

### Worktree (3 tools)

Git worktree isolation for parallel work.

```
masc_worktree_create, masc_worktree_remove, masc_worktree_list
```

### Health (4 tools)

Agent lifecycle and cleanup.

```
masc_heartbeat, masc_cleanup_zombies, masc_gc, masc_agents
```

### Discovery (2 tools)

Find agents by capability.

```
masc_register_capabilities, masc_find_by_capability
```

### Voting (4 tools)

Multi-agent consensus.

```
masc_vote_create, masc_vote_cast, masc_vote_status, masc_votes
```

### Interrupt (5 tools)

Human-in-the-loop pattern.

```
masc_interrupt, masc_approve, masc_reject, masc_pending_interrupts, masc_branch
```

### Cost (2 tools)

Token usage tracking.

```
masc_cost_log, masc_cost_report
```

### Auth (7 tools)

Token-based authentication.

```
masc_auth_enable, masc_auth_disable, masc_auth_status,
masc_auth_create_token, masc_auth_refresh, masc_auth_revoke, masc_auth_list
```

### RateLimit (2 tools)

Request throttling.

```
masc_rate_limit_status, masc_rate_limit_config
```

### Encryption (4 tools)

AES-256-GCM data protection.

```
masc_encryption_status, masc_encryption_enable, masc_encryption_disable, masc_generate_key
```

## Usage

### Switch Mode

```json
// Switch to preset mode
{
  "name": "masc_switch_mode",
  "arguments": {
    "mode": "minimal"
  }
}

// Custom: specific categories
{
  "name": "masc_switch_mode",
  "arguments": {
    "mode": "custom",
    "categories": ["core", "worktree", "portal"]
  }
}
```

### Get Current Config

```json
{
  "name": "masc_get_config",
  "arguments": {}
}
```

**Response:**

```json
{
  "mode": "minimal",
  "mode_description": "Core task management + health checks only",
  "enabled_categories": ["core", "health"],
  "disabled_categories": ["comm", "portal", "worktree", "discovery", "voting", "interrupt", "cost", "auth", "ratelimit", "encryption"],
  "enabled_tool_count": 103,
  "available_modes": [...]
}
```

> Tool counts are computed at runtime and will change as tools evolve. Use `masc_get_config` for the current count.

## Configuration Persistence

Mode configuration is saved to `.masc/config.json`:

```json
{
  "mode": "standard",
  "enabled_categories": ["core", "comm", "worktree", "health"]
}
```

The configuration persists across server restarts.

## Best Practices

### 1. Start Minimal, Add as Needed

```bash
# Start with minimal
masc_switch_mode(mode: "minimal")

# Need worktree? Switch to solo
masc_switch_mode(mode: "solo")

# Need multi-agent? Switch to standard or parallel
masc_switch_mode(mode: "standard")

# Heavy parallel work? Switch to parallel
masc_switch_mode(mode: "parallel")
```

### 2. Use Custom for CI/CD

```bash
# Only task management and worktree for automation
masc_switch_mode(mode: "custom", categories: ["core", "worktree"])
```

### 3. Full Mode for Complex Collaboration

```bash
# When you need voting, encryption, or other advanced features
masc_switch_mode(mode: "full")
```

## Mode Tools Reference

### masc_switch_mode

Switch to a preset mode or enable custom categories.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `mode` | string | Yes | One of: `minimal`, `standard`, `full`, `solo`, `custom` |
| `categories` | array | No | For `custom` mode: list of category names |

**Valid Categories:**

`core`, `comm`, `portal`, `worktree`, `health`, `discovery`, `voting`, `interrupt`, `cost`, `auth`, `ratelimit`, `encryption`

### masc_get_config

Get current mode configuration.

**Parameters:** None

**Response Fields:**

| Field | Type | Description |
|-------|------|-------------|
| `mode` | string | Current mode name |
| `mode_description` | string | Human-readable description |
| `enabled_categories` | array | List of enabled category names |
| `disabled_categories` | array | List of disabled category names |
| `enabled_tool_count` | number | Approximate tool count |
| `available_modes` | array | All available preset modes |

## Troubleshooting

### "Invalid category name" Error

Check spelling. Valid names are lowercase:
- `core`, `comm`, `portal`, `worktree`, `health`
- `discovery`, `voting`, `interrupt`, `cost`
- `auth`, `ratelimit`, `encryption`

### Mode Not Persisting

Ensure `.masc/` directory exists and is writable:

```bash
ls -la .masc/
# Should see config.json after first mode switch
```

### Tools Not Filtered

1. Check current config with `masc_get_config`
2. Verify server was restarted after config change (for MCP clients that cache)
