# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.6.0] - 2026-01-30

### Changed
- **Protocol Standardisation**: Updated client logic to always request standard JSON (Verbose) from llm-mcp.

### Removed
- **Manual Locking**: Removed `masc_lock` and `masc_unlock` tools.
- **Legacy API**: Removed `/api/v1/locks` REST endpoints.

## [2.5.0] - 2026-01-29

### Added
- **Cellular Mitosis Protocol**: 2-phase context handoff system.
- **Relay System**: Seamless context compression and agent replacement.

## [2.3.1] - 2026-01-28

### Fixed
- `room_enter` 이동 시 이전 방에서 에이전트 중복 제거
- `masc_room_enter`가 호출자 닉네임을 유지하도록 보정

### Changed
- `masc_status`에 클러스터명 표시 보강
- 멀티룸 테스트에 에이전트 이동 케이스 추가

## [2.3.0] - 2026-01-28

### Changed
- **Major Refactoring**: Extracted 124 handlers from God Function into 26 Tool_* modules
- `mcp_server_eio.ml` reduced from ~3,400 to ~1,580 lines (-54%)
- Dispatch chain pattern for clean tool routing

### Added
- 26 new Tool_* modules:
  - `Tool_task`: Core task operations (add, claim, done, transition)
  - `Tool_room`: Room management (status, init, reset)
  - `Tool_control`: Flow control (pause, resume, switch_mode)
  - `Tool_agent`: Agent operations (select, fitness, collaboration)
  - `Tool_a2a`: Agent-to-agent communication
  - `Tool_walph`: WALPH loop integration
  - And 20 more specialized modules
- 21 new test files for Tool_* coverage
- `test_dispatch_chain_evidence.ml` for routing verification

### Technical
- Dispatch chain returns `result option` (`None` = try next module)
- 21 stateful handlers remain in God Function (require state/registry/sw)
- All 189 tests pass

## [2.2.1] - 2026-01-19

### Added
- Dashboard now reports lock count in full and compact views.

### Changed
- Resolve base path consistently when restoring sessions and logging startup.
- Align version strings across CLI, health response, and package metadata.

## [2.1.0] - 2026-01-19

### Added
- Read-only GraphQL endpoint (`POST /graphql`)
- Relay-style connections for tasks, agents, and messages
- GraphQL examples and real-world use cases in README
- GraphQL API Alcotest coverage

## [0.1.1] - 2026-01-18

### Fixed
- Server crashes on unhandled exceptions in request handlers
- Server crashes on port binding errors (EADDRINUSE, EACCES)
- Added comprehensive error logging with method, path, and backtrace

## [0.1.0] - 2026-01-18

### Added
- Initial release
- Multi-Agent Streaming Coordination (MASC) MCP Server
- Core tools: task management, file locking, broadcasting
- Portal A2A workflow for direct agent communication
- Git worktree isolation for parallel work
- Redis distributed mode for multi-machine coordination
- Compact Protocol v5 for agent communication (64%+ token savings)
- Human-in-the-loop interrupt workflow
- Voting system for multi-agent consensus
- Cellular/Mitosis agent lifecycle management
- Swarm intelligence patterns (flocking, foraging, stigmergy)
- Multi-room support
- Cost tracking and rate limiting

### Technical
- OCaml 5.x native implementation
- MCP 2025-11-25 spec compliance
- 385 tests passing
- HTTP/SSE mode with Redis backend support
