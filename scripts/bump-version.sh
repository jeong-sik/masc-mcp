#!/bin/bash
# Usage: ./scripts/bump-version.sh 2.2.3

set -e

NEW_VERSION="${1:-}"
if [ -z "$NEW_VERSION" ]; then
  echo "Usage: $0 <new-version>"
  echo "Example: $0 2.2.3"
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

echo "🔄 Bumping version to $NEW_VERSION"

# 1. Update dune-project
sed -i '' "s/(version [^)]*)/(version $NEW_VERSION)/" "$ROOT_DIR/dune-project"
echo "✅ dune-project"

# 2. Update bin/main_eio.ml (health + CLI)
sed -i '' "s/\"version\":\"[0-9.]*\"/\"version\":\"$NEW_VERSION\"/g" "$ROOT_DIR/bin/main_eio.ml"
sed -i '' "s/~version:\"[0-9.]*\"/~version:\"$NEW_VERSION\"/g" "$ROOT_DIR/bin/main_eio.ml"
echo "✅ bin/main_eio.ml"

# 3. Update bin/main.ml (CLI)
sed -i '' "s/~version:\"[0-9.]*\"/~version:\"$NEW_VERSION\"/g" "$ROOT_DIR/bin/main.ml"
echo "✅ bin/main.ml"

# 4. Update lib/mcp_server.ml
sed -i '' "s/\"version\", \`String \"[0-9.]*\"/\"version\", \`String \"$NEW_VERSION\"/" "$ROOT_DIR/lib/mcp_server.ml"
echo "✅ lib/mcp_server.ml"

# 5. Update lib/http_server_eio.ml
sed -i '' "s/\"version\":\"[0-9.]*\"/\"version\":\"$NEW_VERSION\"/g" "$ROOT_DIR/lib/http_server_eio.ml"
echo "✅ lib/http_server_eio.ml"

# 6. Update opam file if exists
if [ -f "$ROOT_DIR/masc_mcp.opam" ]; then
  sed -i '' "s/^version: .*/version: \"$NEW_VERSION\"/" "$ROOT_DIR/masc_mcp.opam"
  echo "✅ masc_mcp.opam"
fi

echo ""
echo "📦 Version bumped to $NEW_VERSION"
echo "   Run: dune build && git add -A && git commit -m 'chore: bump to $NEW_VERSION'"
