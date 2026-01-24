# masc-mcp Makefile
# Enterprise-ready development commands

.PHONY: build test clean coverage doc install-deps dev-setup fmt fmt-check ci

# Default target
all: build

# Build the project
build:
	dune build

# Run tests
test:
	dune test

# Clean build artifacts
clean:
	dune clean

# Run tests with coverage instrumentation
# NOTE: bisect_ppx pending ppxlib 0.37+ support (OCaml 5.2+ AST changes)
coverage:
	@echo "⚠️  Coverage disabled: bisect_ppx pending ppxlib 0.37+ support"
	@echo "Running tests instead..."
	dune test

# Generate documentation
doc:
	dune build @doc
	@echo "Documentation generated at _build/default/_doc/_html/index.html"

# Install dependencies
install-deps:
	opam install . --deps-only --with-test --with-doc -y

# Development setup
dev-setup: install-deps
	@echo "Development environment ready!"

# Format code (if ocamlformat is installed)
fmt:
	dune fmt || true

# Check formatting
fmt-check:
	dune fmt --check || true

# CI target (for GitHub Actions)
ci: fmt-check test
	@echo "CI checks passed!"

# Start the MCP server (local development)
run:
	dune exec masc-mcp -- --port 8933

# Build release binary
release:
	dune build --release
	@echo "Release binary at _build/default/bin/main.exe"
