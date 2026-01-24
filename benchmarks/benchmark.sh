#!/bin/bash
# MASC Benchmark Framework
# Usage: ./benchmark.sh [pattern] [iterations]

set -e

MASC_URL="${MASC_URL:-http://127.0.0.1:8935/mcp}"
PATTERN="${1:-all}"
ITERATIONS="${2:-3}"
RESULTS_DIR="$(dirname "$0")/results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

mkdir -p "$RESULTS_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[BENCH]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Helper: Call MASC tool and measure time
call_masc() {
    local tool="$1"
    local args="$2"
    local start=$(date +%s%N)

    local response=$(curl -s -X POST "$MASC_URL" \
        -H "Content-Type: application/json" \
        -H "Accept: application/json, text/event-stream" \
        -d "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"$tool\",\"arguments\":$args}}" 2>/dev/null)

    local end=$(date +%s%N)
    local duration=$(( (end - start) / 1000000 ))  # ms

    echo "$duration"
}

# Benchmark 1: Coordination Latency
bench_coordination() {
    log "Running: Coordination Latency Benchmark"
    local results=()

    for i in $(seq 1 $ITERATIONS); do
        # Measure status call
        local status_time=$(call_masc "masc_status" "{}")

        # Measure broadcast
        local broadcast_time=$(call_masc "masc_broadcast" "{\"message\":\"bench_$i\",\"priority\":\"low\"}")

        # Measure messages retrieval
        local messages_time=$(call_masc "masc_messages" "{\"limit\":5}")

        results+=("$status_time,$broadcast_time,$messages_time")
        log "  Iteration $i: status=${status_time}ms, broadcast=${broadcast_time}ms, messages=${messages_time}ms"
    done

    # Calculate averages
    local sum_status=0 sum_broadcast=0 sum_messages=0
    for r in "${results[@]}"; do
        IFS=',' read -r s b m <<< "$r"
        sum_status=$((sum_status + s))
        sum_broadcast=$((sum_broadcast + b))
        sum_messages=$((sum_messages + m))
    done

    local avg_status=$((sum_status / ITERATIONS))
    local avg_broadcast=$((sum_broadcast / ITERATIONS))
    local avg_messages=$((sum_messages / ITERATIONS))

    echo "coordination,$avg_status,$avg_broadcast,$avg_messages" >> "$RESULTS_DIR/results_$TIMESTAMP.csv"
    log "  Average: status=${avg_status}ms, broadcast=${avg_broadcast}ms, messages=${avg_messages}ms"
}

# Benchmark 2: Task Lifecycle
bench_task_lifecycle() {
    log "Running: Task Lifecycle Benchmark"
    local task_id="bench_task_$TIMESTAMP"

    local start=$(date +%s%N)

    # Create task
    call_masc "masc_add_task" "{\"id\":\"$task_id\",\"title\":\"Benchmark task\"}" > /dev/null

    # Claim task
    call_masc "masc_claim" "{\"task_id\":\"$task_id\"}" > /dev/null

    # Complete task
    call_masc "masc_done" "{\"task_id\":\"$task_id\"}" > /dev/null

    local end=$(date +%s%N)
    local total=$((  (end - start) / 1000000 ))

    echo "task_lifecycle,$total" >> "$RESULTS_DIR/results_$TIMESTAMP.csv"
    log "  Total lifecycle: ${total}ms"
}

# Benchmark 3: Lock Contention
bench_lock_contention() {
    log "Running: Lock Contention Benchmark"
    local test_file="bench_lock_$TIMESTAMP.txt"
    local results=()

    for i in $(seq 1 $ITERATIONS); do
        # Lock
        local lock_time=$(call_masc "masc_lock" "{\"agent_name\":\"bench\",\"file\":\"$test_file\"}")

        # Unlock
        local unlock_time=$(call_masc "masc_unlock" "{\"agent_name\":\"bench\",\"file\":\"$test_file\"}")

        results+=("$lock_time,$unlock_time")
        log "  Iteration $i: lock=${lock_time}ms, unlock=${unlock_time}ms"
    done

    # Calculate averages
    local sum_lock=0 sum_unlock=0
    for r in "${results[@]}"; do
        IFS=',' read -r l u <<< "$r"
        sum_lock=$((sum_lock + l))
        sum_unlock=$((sum_unlock + u))
    done

    local avg_lock=$((sum_lock / ITERATIONS))
    local avg_unlock=$((sum_unlock / ITERATIONS))

    echo "lock_contention,$avg_lock,$avg_unlock" >> "$RESULTS_DIR/results_$TIMESTAMP.csv"
    log "  Average: lock=${avg_lock}ms, unlock=${avg_unlock}ms"
}

# Benchmark 4: A2A Communication
bench_a2a() {
    log "Running: A2A Communication Benchmark"

    # Discover agents
    local discover_time=$(call_masc "masc_a2a_discover" "{}")

    # Query skill
    local query_time=$(call_masc "masc_a2a_query_skill" "{\"skill\":\"code-review\"}")

    echo "a2a,$discover_time,$query_time" >> "$RESULTS_DIR/results_$TIMESTAMP.csv"
    log "  discover=${discover_time}ms, query_skill=${query_time}ms"
}

# Benchmark 5: Swarm Operations
bench_swarm() {
    log "Running: Swarm Operations Benchmark"
    local topic="bench_swarm_$TIMESTAMP"

    # Init swarm
    local init_time=$(call_masc "masc_swarm_init" "{\"topic\":\"$topic\"}")

    # Propose
    local propose_time=$(call_masc "masc_swarm_propose" "{\"topic\":\"$topic\",\"proposal\":\"test\"}")

    # Vote
    local vote_time=$(call_masc "masc_swarm_vote" "{\"topic\":\"$topic\",\"choice\":\"test\",\"voter\":\"benchmark\"}")

    # Status
    local status_time=$(call_masc "masc_swarm_status" "{\"topic\":\"$topic\"}")

    echo "swarm,$init_time,$propose_time,$vote_time,$status_time" >> "$RESULTS_DIR/results_$TIMESTAMP.csv"
    log "  init=${init_time}ms, propose=${propose_time}ms, vote=${vote_time}ms, status=${status_time}ms"
}

# Main
main() {
    log "MASC Benchmark Suite"
    log "URL: $MASC_URL"
    log "Pattern: $PATTERN"
    log "Iterations: $ITERATIONS"
    log "Results: $RESULTS_DIR/results_$TIMESTAMP.csv"
    echo ""

    # Check MASC is running
    if ! curl -s "$MASC_URL" > /dev/null 2>&1; then
        error "MASC server not reachable at $MASC_URL"
        exit 1
    fi

    # Initialize results file
    echo "benchmark,metric1,metric2,metric3,metric4" > "$RESULTS_DIR/results_$TIMESTAMP.csv"

    case "$PATTERN" in
        all)
            bench_coordination
            bench_task_lifecycle
            bench_lock_contention
            bench_a2a
            bench_swarm
            ;;
        coordination)
            bench_coordination
            ;;
        task)
            bench_task_lifecycle
            ;;
        lock)
            bench_lock_contention
            ;;
        a2a)
            bench_a2a
            ;;
        swarm)
            bench_swarm
            ;;
        *)
            error "Unknown pattern: $PATTERN"
            echo "Available: coordination, task, lock, a2a, swarm, all"
            exit 1
            ;;
    esac

    echo ""
    log "Benchmark complete! Results saved to $RESULTS_DIR/results_$TIMESTAMP.csv"

    # Print summary
    echo ""
    echo "=== Summary ==="
    cat "$RESULTS_DIR/results_$TIMESTAMP.csv" | column -t -s ','
}

main "$@"
