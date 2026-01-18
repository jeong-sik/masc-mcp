# MASC Glossary

**Version**: 1.0.0
**Date**: 2026-01-10
**Status**: Official

## Introduction

This glossary defines the official terminology for the MASC (Multi-Agent Streaming Coordination) project. Terminology normalization is essential for:

1. **Consistency**: Unified vocabulary across documentation, code, and communication
2. **Clarity**: Reducing confusion from multiple terms describing the same concept
3. **Onboarding**: Faster learning curve for new contributors
4. **Searchability**: Single term per concept improves documentation discovery

### Normalization Principles

- **One Concept, One Term**: Each concept has exactly one official term
- **Descriptive over Metaphorical**: Prefer clear descriptions over biological/scientific metaphors
- **Backward Compatibility**: Deprecated terms remain functional with deprecation warnings

---

## Official Terminology

| Concept | Official Term | Deprecated Terms | Description |
|---------|---------------|------------------|-------------|
| Context transfer | **Handoff** | relay, handover, mitosis | The process of transferring execution context from one agent to another |
| Compressed context | **Capsule** | DNA, summary, context blob | A compressed representation of agent context for efficient transfer |
| Agent lifecycle | **Lifecycle** | cell state, cell lifecycle | The stages an agent goes through from creation to termination |
| Work unit | **Task** | job, work, quest | A discrete unit of work assigned to an agent |
| Agent workspace | **Worktree** | workspace, sandbox | Isolated git worktree for parallel agent work |
| Inter-agent message | **Broadcast** | announce, notify | A message sent to all agents in the room |
| Direct communication | **Portal** | tunnel, channel | A direct communication link between two specific agents |
| Resource protection | **Lock** | mutex, guard | Exclusive access to a shared resource |
| Agent pool | **Stem Pool** | reserve, pool | Pre-warmed agents ready for immediate handoff |
| Context ratio | **Usage** | context_pct, fill_rate | Percentage of context window consumed |

---

## Term Definitions

### Handoff

The process of transferring execution context from one agent (source) to another (target). A handoff occurs when:

- Context usage exceeds threshold (typically 80%)
- Agent completes its assigned task
- Explicit user request
- Error recovery

**Usage in code**:
```ocaml
(* Correct *)
let handoff ~source ~target ~capsule = ...

(* Deprecated *)
let relay ~from ~to_ ~dna = ...  (* Use handoff instead *)
```

### Capsule

A compressed representation of agent context containing:

- Current goal and progress
- Completed and pending steps
- Key decisions and rationale
- Warnings and assumptions
- Modified files list

**Why "Capsule" over "DNA"**:
- More intuitive metaphor (container vs. genetic code)
- Clearer purpose (packaging vs. replication)
- Avoids overloaded biological terminology

### Lifecycle

The stages an agent goes through:

```
Created -> Active -> Prepared -> Handoff -> Terminated
             |                      ^
             +---- (error) ---------+
```

| Stage | Description |
|-------|-------------|
| Created | Agent initialized, not yet working |
| Active | Agent executing tasks |
| Prepared | Capsule extracted, ready for handoff |
| Handoff | Context being transferred |
| Terminated | Agent gracefully shut down |

### Task

A discrete unit of work with:

- Unique identifier (e.g., `task-001`)
- Title and description
- Priority (1-5, where 1 is highest)
- Status: `pending`, `in_progress`, `completed`, `cancelled`
- Assigned agent (if claimed)

### Worktree

An isolated git worktree created for agent work:

```bash
# Location pattern
.worktrees/{agent}-{task}/

# Example
.worktrees/claude-task-001/
```

Benefits:
- No merge conflicts between agents
- Clean git history per task
- Easy cleanup after PR merge

---

## Migration Notes

### For Documentation Authors

When writing new documentation:

1. Use only official terms from the table above
2. If referencing old documentation, add a note: "(formerly known as X)"
3. Update existing docs when making substantial edits

### For Code Contributors

The codebase is transitioning to normalized terminology:

```ocaml
(* Type aliases for backward compatibility *)
type handoff = relay  [@@deprecated "Use handoff"]
type capsule = dna    [@@deprecated "Use capsule"]
```

**Migration commands** (when ready):
```bash
# Rename relay_ to handoff_
sed -i 's/relay_/handoff_/g' lib/*.ml

# Rename dna_ to capsule_
sed -i 's/dna_/capsule_/g' lib/*.ml
```

### For API Users

MCP tool names are stable but parameter names may change:

| Current | Future | Notes |
|---------|--------|-------|
| `masc_relay_*` | `masc_handoff_*` | Planned for v3.0 |
| `dna` param | `capsule` param | Planned for v3.0 |

---

## Deprecated Terms Reference

For readers encountering deprecated terms in older documentation:

| Deprecated Term | Official Term | Context |
|-----------------|---------------|---------|
| relay | Handoff | From early designs based on "relay race" metaphor |
| handover | Handoff | British English variant, unified to American English |
| mitosis | Handoff | Biological metaphor from "cell division" design |
| DNA | Capsule | Biological metaphor for compressed context |
| summary | Capsule | Informal term, now formalized as Capsule |
| cell state | Lifecycle | From cellular biology metaphor |
| job | Task | Generic term, unified to Task |
| work | Task | Generic term, unified to Task |
| quest | Task | RPG-style term from early designs |

---

## Rationale for Key Decisions

### Why "Handoff" over "Relay"?

1. **Industry standard**: "Handoff" is widely used in distributed systems
2. **Clearer semantics**: Implies transfer of responsibility, not just forwarding
3. **Single word**: Easier to use in compound terms (handoff_rate vs relay_rate)

### Why "Capsule" over "DNA"?

1. **Intuitive metaphor**: A capsule contains and protects contents
2. **No biological baggage**: DNA implies replication, mutation, genetics
3. **Practical naming**: `capsule.ml` clearer than `dna.ml`

### Why "Task" over "Job" or "Work"?

1. **Specificity**: Task implies discrete, completable units
2. **Common in project management**: Aligns with JIRA, Trello, etc.
3. **Existing usage**: Already used in `masc_add_task`, `masc_tasks`

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-10 | Initial glossary with P2 terminology normalization |

---

## References

- [MASC V2 Design](./MASC-V2-DESIGN.md) - Architecture overview
- [Research-Based Improvements](./RESEARCH-BASED-IMPROVEMENTS.md) - Source of normalization decisions
- [SPEC.md](./SPEC.md) - Full specification
