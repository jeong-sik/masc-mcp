# Figma MCP Codebase Audit Report
**Date:** 2026-01-23
**Target:** `/Users/dancer/me/.worktrees/figma-mcp-audit-0123`
**Agent:** gemini-kind-viper

## 1. Critical Issues & Redundancy

### 1.1. Triplicated Neo4j Connection Logic
Three different modules manage Neo4j connections, causing potential connection leaks and state inconsistency.
- **`lib/clients/neo4j_client.py`**: Functional client with "Second Brain" specific caching.
- **`lib/core/database/neo4j_client.py`**: Singleton `GraphDatabase` driver wrapper (thread-safe).
- **`lib/database/connection_pool.py`**: Pool manager that *also* handles Neo4j (plus Qdrant/OpenAI).

**Improvement:** Consolidate to `lib/core/database/neo4j_client.py` as the single low-level driver. `lib/clients` should consume this driver, not create new connections. `connection_pool.py` should likely be deprecated or refactored to use the Core driver.

### 1.2. Duplicated UCB/MCTS Algorithms
`Ouroboros` (Self-evolution) re-implements UCB1 instead of reusing the `mcts` library.
- **`lib/ouroboros/ucb.py`**: Re-implementation with JSON/OS dependencies.
- **`lib/mcts/ucb1.py`**: Cleaner, dataclass-based implementation.
- **`lib/ouroboros/agent_bandit.py`**: Uses the local `ucb.py`.

**Improvement:** Delete `lib/ouroboros/ucb.py`. Update `lib/ouroboros` to import `UCB1Selector` from `lib/mcts/ucb1.py`.

### 1.3. Fragmented Database Architecture
- **Postgres**:
  - `lib/clients/postgres_client.py`: "Unified Railway PostgreSQL connection".
  - `lib/database/pg_writer.py` & `pg_cache.py`: Another set of Postgres tools.
  - `lib/ocaml/pg_client`: OCaml implementation.
- **Ambiguity**: It is unclear which Python client is the "System of Record".

**Improvement:** Designate `lib/core/database` as the source of truth for all DB drivers. Move Postgres logic there.

## 2. Incompleteness & Empty Artifacts

### 2.1. Empty Tool Directories
The following directories exist but contain no code:
- `tools/slack-cli/`
- `tools/slack-term/`
- `lib/ocaml/qdrant_client/` (appears empty in file listing)

**Improvement:** Remove these directories if they are abandoned placeholders, or add a `README.md` explaining their roadmap status.

### 2.2. Unused Design Patterns
- **`lib/core/patterns/saga.py`**: `Saga` class is defined but never used.
- **`lib/core/patterns/state_machine.py`**: `StateMachine` is defined but never used.

**Improvement:** Delete unused files to reduce cognitive load. YAGNI (You Ain't Gonna Need It).

## 3. Operational & Security Risks

### 3.1. Hardcoded Paths
- **`lib/ocaml/credits_check/lib/credits.ml`**: Hardcodes `HOME/me` path logic.
  ```ocaml
  let me_root = try Sys.getenv "ME_ROOT" with Not_found -> Sys.getenv "HOME" ^ "/me"
  ```
  If the repository is cloned elsewhere (e.g., CI/CD), this breaks.

**Improvement:** Use relative paths from the executable location or strictly require `ME_ROOT` env var.

### 3.2. Mutable State in Git
- **`lib/state/user_twin.json`**: Contains runtime stats (`interaction_stats`) and last sync timestamps.
- **Risk**: Committing runtime state to Git causes merge conflicts and "dirty" worktrees.

**Improvement:** Move mutable state to `data/state/` (which should be `.gitignore`d). Keep `lib/state` for read-only templates only (rename to `*.template.json`).

## 4. Code Quality & Typing

### 4.1. Excessive `Any` in Core
- `lib/core/types` uses `Any` extensively (65 occurrences), defeating the purpose of a type system.
- Functional constructs like `Yoneda` and `Coyoneda` seem over-engineered for a Python MCP agent and are likely unused.

**Improvement:** Remove unused functional programming categories. Add proper Generics (`TypeVar`) to `Either` and `Option` if kept.

## 5. Summary of Recommended Actions
1. **Consolidate DB Clients**: Merge `lib/clients`, `lib/database` -> `lib/core/database`.
2. **De-duplicate Algorithms**: `lib/ouroboros` should use `lib/mcts`.
3. **Clean Up**: Delete empty `tools/*` and unused `lib/core/patterns/*`.
4. **Fix State**: Move `lib/state/*.json` to `data/`.
5. **Standardize Paths**: Fix OCaml hardcoded paths.
