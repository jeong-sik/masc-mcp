#!/bin/bash
# True Concurrent Distributed Test
# Multiple processes write SIMULTANEOUSLY - the real test!

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
SHARED_DIR="/tmp/masc_concurrent_test_$$"
NUM_WORKERS=${1:-4}
MSGS_PER_WORKER=${2:-10}

echo "=============================================="
echo "MASC A2A TRUE CONCURRENT TEST"
echo "=============================================="
echo "Workers: $NUM_WORKERS"
echo "Messages per worker: $MSGS_PER_WORKER"
echo "Total expected messages: $((NUM_WORKERS * MSGS_PER_WORKER))"
echo "Shared directory: $SHARED_DIR"
echo ""

# Clean up
rm -rf "$SHARED_DIR"
mkdir -p "$SHARED_DIR"

# Build
echo "[Build] Building test executable..."
cd "$PROJECT_DIR"
dune build test/test_concurrent_distributed.exe

# Direct path to built binary (avoids dune exec lock)
BINARY="$PROJECT_DIR/_build/default/test/test_concurrent_distributed.exe"

# Launch all workers SIMULTANEOUSLY
echo ""
echo "[Concurrent] Launching $NUM_WORKERS workers in parallel..."
PIDS=""
for i in $(seq 1 $NUM_WORKERS); do
    "$BINARY" \
        --id "w$i" \
        --dir "$SHARED_DIR" \
        --msgs "$MSGS_PER_WORKER" &
    PIDS="$PIDS $!"
done

# Wait for ALL workers to complete
echo "[Concurrent] Waiting for all workers..."
FAILED=0
for pid in $PIDS; do
    if ! wait $pid; then
        FAILED=1
    fi
done

if [ $FAILED -ne 0 ]; then
    echo "❌ Some workers failed"
    exit 1
fi

# Verify results
echo ""
echo "[Verify] Checking for collisions and data integrity..."
"$BINARY" --id verify --dir "$SHARED_DIR"
VERIFY_RESULT=$?

# Cleanup
echo ""
echo "[Cleanup] Removing test directory..."
rm -rf "$SHARED_DIR"

if [ $VERIFY_RESULT -eq 0 ]; then
    echo ""
    echo "=============================================="
    echo "✅ TRUE CONCURRENT TEST PASSED!"
    echo "   $NUM_WORKERS workers × $MSGS_PER_WORKER msgs = $((NUM_WORKERS * MSGS_PER_WORKER)) total"
    echo "   No seq collisions under parallel writes"
    echo "=============================================="
    exit 0
else
    echo ""
    echo "=============================================="
    echo "❌ TRUE CONCURRENT TEST FAILED"
    echo "=============================================="
    exit 1
fi
