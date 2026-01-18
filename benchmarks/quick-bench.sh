#!/bin/bash
# MASC Quick Benchmark
# Measures latency of common operations

set -e

MASC_URL="${MASC_URL:-http://127.0.0.1:8935/mcp}"

echo "=== MASC Quick Benchmark ==="
echo "URL: $MASC_URL"
echo ""

# Helper: measure call latency
measure() {
    local name="$1"
    local tool="$2"
    local args="$3"

    local total=0
    local iterations=5

    for i in $(seq 1 $iterations); do
        local start=$(python3 -c 'import time; print(int(time.time() * 1000))')

        curl -s -X POST "$MASC_URL" \
            -H "Content-Type: application/json" \
            -H "Accept: application/json, text/event-stream" \
            --max-time 5 \
            -d "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"$tool\",\"arguments\":$args}}" > /dev/null 2>&1

        local end=$(python3 -c 'import time; print(int(time.time() * 1000))')
        local duration=$((end - start))
        total=$((total + duration))
    done

    local avg=$((total / iterations))
    printf "%-25s %4dms (avg of %d)\n" "$name" "$avg" "$iterations"
}

echo "Operation                 Latency"
echo "─────────────────────────────────"

# Core operations
measure "masc_status" "masc_status" "{}"
measure "masc_agents" "masc_agents" "{}"
measure "masc_messages (5)" "masc_messages" "{\"limit\":5}"
measure "masc_broadcast" "masc_broadcast" "{\"message\":\"bench\",\"priority\":\"low\"}"
measure "masc_tasks" "masc_tasks" "{}"

echo ""
echo "─────────────────────────────────"

# Lock operations
measure "masc_lock" "masc_lock" "{\"file\":\"bench_test.txt\"}"
measure "masc_unlock" "masc_unlock" "{\"file\":\"bench_test.txt\"}"

echo ""
echo "─────────────────────────────────"

# A2A operations
measure "masc_a2a_discover" "masc_a2a_discover" "{}"
measure "masc_find_by_capability" "masc_find_by_capability" "{\"capabilities\":[\"code-review\"]}"

echo ""
echo "=== Benchmark Complete ==="
