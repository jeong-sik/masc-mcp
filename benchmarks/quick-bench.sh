#!/bin/bash
# MASC Quick Benchmark
# Measures latency of common operations

set -e

MASC_URL="${MASC_URL:-http://127.0.0.1:8935/mcp}"
MASC_AGENT="${MASC_AGENT:-bench}"
MASC_TOKEN="${MASC_TOKEN:-}"

AUTH_HEADER=()
if [ -n "$MASC_TOKEN" ]; then
    AUTH_HEADER=(-H "Authorization: Bearer $MASC_TOKEN")
fi

echo "=== MASC Quick Benchmark ==="
echo "URL: $MASC_URL"
echo "Agent: $MASC_AGENT"
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
            "${AUTH_HEADER[@]}" \
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
measure "masc_status" "masc_status" "{\"agent_name\":\"$MASC_AGENT\"}"
measure "masc_agents" "masc_agents" "{\"agent_name\":\"$MASC_AGENT\"}"
measure "masc_messages (5)" "masc_messages" "{\"agent_name\":\"$MASC_AGENT\",\"limit\":5}"
measure "masc_broadcast" "masc_broadcast" "{\"agent_name\":\"$MASC_AGENT\",\"message\":\"bench\",\"priority\":\"low\"}"
measure "masc_tasks" "masc_tasks" "{\"agent_name\":\"$MASC_AGENT\"}"

echo ""
echo "─────────────────────────────────"

# Lock operations
measure "masc_lock" "masc_lock" "{\"agent_name\":\"$MASC_AGENT\",\"file\":\"bench_test.txt\"}"
measure "masc_unlock" "masc_unlock" "{\"agent_name\":\"$MASC_AGENT\",\"file\":\"bench_test.txt\"}"

echo ""
echo "─────────────────────────────────"

# A2A operations
measure "masc_a2a_discover" "masc_a2a_discover" "{\"agent_name\":\"$MASC_AGENT\"}"
measure "masc_find_by_capability" "masc_find_by_capability" "{\"agent_name\":\"$MASC_AGENT\",\"capabilities\":[\"code-review\"]}"

echo ""
echo "=== Benchmark Complete ==="
