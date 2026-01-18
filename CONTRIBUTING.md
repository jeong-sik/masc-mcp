# Contributing to MASC MCP

Thank you for your interest in contributing to MASC (Multi-Agent Streaming Coordination)!

## Quick Start

```bash
# 1. Clone and setup
git clone https://github.com/your-org/masc-mcp.git
cd masc-mcp

# 2. Install OCaml dependencies
opam install . --deps-only

# 3. Build
dune build

# 4. Run tests
dune test
```

## Development Guidelines

### Code Style

- **OCaml 5.x** - Use latest language features
- **Type Safety** - Leverage `ppx_deriving` for JSON serialization
- **Pure Functions** - Extract testable logic from IO code
- **Error Handling** - Use `Result.t` over exceptions

### Project Structure

```
lib/
├── types.ml           # Domain types (state machines)
├── room.ml            # Core collaboration logic
├── checkpoint_types.ml # Checkpoint pure functions
└── mcp_server.ml      # JSON-RPC 2.0 server

bin/
├── main.ml            # MCP server entry point
└── masc_checkpoint.ml # Human-in-the-loop CLI

test/
├── test_room.ml       # Room tests (74)
├── test_types.ml      # Types tests (5)
├── test_checkpoint.ml # Checkpoint tests (13)
└── test_mcp_server.ml # MCP tests (3)
```

### Testing

- **Minimum Coverage**: 80%+ (currently 95 tests)
- **Test Types**: Unit tests with Alcotest
- **Pure Functions**: Tested without mocking
- **IO Functions**: Integration tested with real Neo4j (optional)

```bash
# Run all tests
dune test

# Run specific test file
dune exec test/test_checkpoint.exe
```

### Commit Messages

Use conventional commits in **Korean**:

```
feat: 새로운 기능 추가
fix: 버그 수정
refactor: 코드 리팩토링
test: 테스트 추가/수정
docs: 문서 수정
chore: 빌드/설정 변경
```

Example:
```
feat(checkpoint): 자동 타임아웃 거절 기능 추가

- 30분 초과 interrupt 자동 rejected 처리
- --timeout 옵션으로 조절 가능
```

## Pull Request Process

1. **Fork** the repository
2. **Create branch**: `feature/description` or `fix/description`
3. **Write tests** for new functionality
4. **Run tests**: `dune test` must pass
5. **Create PR** with description
6. **Wait for review**

## Architecture Decisions

### Why OCaml?

- **Type Safety**: Compile-time guarantees prevent runtime errors
- **Performance**: Native binaries, no runtime overhead
- **Maintainability**: Strong typing catches refactoring issues

### Why File-based State?

- **Simplicity**: No database dependency for core features
- **Atomicity**: File system provides atomic writes
- **Debugging**: Human-readable JSON state files

### Why Neo4j for Checkpoints?

- **Graph Relationships**: Task → Checkpoint → Agent relationships
- **Temporal Queries**: "Show interrupted checkpoints older than X"
- **Visualization**: Neo4j Browser for debugging

## Reporting Issues

When reporting issues, please include:

1. **MASC MCP version** (`dune describe | grep version`)
2. **OCaml version** (`ocaml --version`)
3. **OS and version**
4. **Steps to reproduce**
5. **Expected vs actual behavior**
6. **Error messages/logs**

## Feature Requests

Open an issue with:

1. **Problem**: What problem does this solve?
2. **Proposal**: How should it work?
3. **Alternatives**: What alternatives did you consider?
4. **Impact**: Who benefits from this?

## Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Help newcomers learn

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Questions? Open an issue or reach out to maintainers.
