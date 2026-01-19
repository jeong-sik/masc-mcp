# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.2.1] - 2026-01-19

### Added
- Dashboard now reports lock count in full and compact views.

### Changed
- Resolve base path consistently when restoring sessions and logging startup.
- Align version strings across CLI, health response, and package metadata.

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
