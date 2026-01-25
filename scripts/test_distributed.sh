#!/bin/bash
# Distributed A2A Test - Two-process communication
# Tests that messages persist and are readable across separate processes

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
SHARED_DIR="/tmp/masc_distributed_test_$$"

echo "=============================================="
echo "MASC A2A Distributed Test (2-Process)"
echo "=============================================="
echo "Shared directory: $SHARED_DIR"
echo ""

# Clean up
rm -rf "$SHARED_DIR"
mkdir -p "$SHARED_DIR"

# Build
echo "[Build] Building test executable..."
cd "$PROJECT_DIR"
dune build test/test_a2a_distributed.exe

# Run producer first
echo ""
echo "[Step 1/2] Running PRODUCER process..."
dune exec test/test_a2a_distributed.exe -- producer --dir "$SHARED_DIR"

echo ""
echo "[Step 2/2] Running CONSUMER process (separate process)..."
dune exec test/test_a2a_distributed.exe -- consumer --dir "$SHARED_DIR"

# Cleanup
echo ""
echo "[Cleanup] Removing test directory..."
rm -rf "$SHARED_DIR"

echo ""
echo "=============================================="
echo "✅ Distributed test completed successfully!"
echo "=============================================="
