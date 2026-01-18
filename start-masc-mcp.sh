#!/bin/bash
# MASC MCP Server (OCaml) - Start Script (HTTP/SSE default)
# Usage: ./start-masc-mcp.sh [--stdio] [--http] [--eio] [--port PORT] [--base-path PATH]

set -e

# Optional: load OPAM environment if available (must never be fatal for MCP startup)
if command -v opam >/dev/null 2>&1; then
    eval "$(opam env 2>/dev/null)" >/dev/null 2>/dev/null || true
fi

# Redis backend: auto-use RAILWAY_REDIS_URL if MASC_REDIS_URL not set
export MASC_REDIS_URL="${MASC_REDIS_URL:-$RAILWAY_REDIS_URL}"

# Room/namespace: derive from ME_ROOT directory name if not set
# e.g., ME_ROOT=/Users/dancer/me → cluster "me"
# e.g., ME_ROOT=/Users/dancer/workspace/kidsnote → cluster "kidsnote"
if [ -z "$MASC_CLUSTER_NAME" ] && [ -n "$ME_ROOT" ]; then
    export MASC_CLUSTER_NAME="$(basename "$ME_ROOT")"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Resolve executable path (prefer workspace build dir)
# Lwt-based server (main.exe) - default
WORKSPACE_EXE="$SCRIPT_DIR/../_build/default/masc-mcp/bin/main.exe"
LOCAL_EXE="$SCRIPT_DIR/_build/default/bin/main.exe"
# Eio-based server (main_eio.exe) - for PostgresNative backend
WORKSPACE_EIO_EXE="$SCRIPT_DIR/../_build/default/masc-mcp/bin/main_eio.exe"
LOCAL_EIO_EXE="$SCRIPT_DIR/_build/default/bin/main_eio.exe"
MASC_EXE=""
MASC_EIO_EXE=""

if [ -x "$WORKSPACE_EXE" ]; then
    MASC_EXE="$WORKSPACE_EXE"
elif [ -x "$LOCAL_EXE" ]; then
    MASC_EXE="$LOCAL_EXE"
fi

if [ -x "$WORKSPACE_EIO_EXE" ]; then
    MASC_EIO_EXE="$WORKSPACE_EIO_EXE"
elif [ -x "$LOCAL_EIO_EXE" ]; then
    MASC_EIO_EXE="$LOCAL_EIO_EXE"
fi

# Build if needed (requires dune on PATH)
if [ -z "$MASC_EXE" ]; then
    echo "Building MASC MCP server (bin/main.exe only)..." >&2
    if ! command -v dune >/dev/null 2>&1; then
        echo "Error: dune not found. Install dune or build masc-mcp binary first." >&2
        exit 1
    fi
    dune build ./bin/main.exe 1>&2

    if [ -x "$WORKSPACE_EXE" ]; then
        MASC_EXE="$WORKSPACE_EXE"
    elif [ -x "$LOCAL_EXE" ]; then
        MASC_EXE="$LOCAL_EXE"
    else
        echo "Error: build succeeded but masc-mcp executable not found." >&2
        exit 1
    fi
fi

# Rebuild if sources are newer than the executable (avoids stale binary runs)
if [ -n "$MASC_EXE" ] && command -v dune >/dev/null 2>&1; then
    if find "$SCRIPT_DIR/bin" "$SCRIPT_DIR/lib" \
        -type f \( -name '*.ml' -o -name '*.mli' -o -name 'dune' \) \
        -newer "$MASC_EXE" 2>/dev/null | head -n 1 | grep -q .; then
        echo "Rebuilding MASC MCP server (stale executable detected)..." >&2
        dune build ./bin/main.exe 1>&2

        if [ -x "$WORKSPACE_EXE" ]; then
            MASC_EXE="$WORKSPACE_EXE"
        elif [ -x "$LOCAL_EXE" ]; then
            MASC_EXE="$LOCAL_EXE"
        fi
    fi
fi

# Default arguments
PORT="${MASC_MCP_PORT:-8935}"
HTTP_MODE="${MASC_MCP_HTTP:-true}"
BASE_PATH="${MASC_BASE_PATH:-$ME_ROOT}"
EIO_MODE="false"

# Auto-detect: Use Eio server if PostgreSQL backend is configured
if [ "$MASC_STORAGE_TYPE" = "postgres" ]; then
    EIO_MODE="true"
fi

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --http)
            HTTP_MODE="true"
            shift
            ;;
        --stdio)
            HTTP_MODE="false"
            shift
            ;;
        --eio)
            EIO_MODE="true"
            shift
            ;;
        --lwt)
            EIO_MODE="false"
            shift
            ;;
        --port)
            PORT="$2"
            shift 2
            ;;
        --base-path)
            BASE_PATH="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            echo "Usage: $0 [--stdio] [--http] [--eio] [--lwt] [--port PORT] [--base-path PATH]" >&2
            exit 1
            ;;
    esac
done

# Select executable based on EIO_MODE
SELECTED_EXE="$MASC_EXE"
RUNTIME_NAME="Lwt"

if [ "$EIO_MODE" = "true" ]; then
    RUNTIME_NAME="Eio"
    if [ -z "$MASC_EIO_EXE" ]; then
        echo "Building MASC MCP server (Eio mode)..." >&2
        if ! command -v dune >/dev/null 2>&1; then
            echo "Error: dune not found. Cannot build Eio server." >&2
            exit 1
        fi
        dune build ./bin/main_eio.exe 1>&2
        if [ -x "$WORKSPACE_EIO_EXE" ]; then
            MASC_EIO_EXE="$WORKSPACE_EIO_EXE"
        elif [ -x "$LOCAL_EIO_EXE" ]; then
            MASC_EIO_EXE="$LOCAL_EIO_EXE"
        else
            echo "Error: Failed to build Eio server (main_eio.exe)." >&2
            exit 1
        fi
    fi
    SELECTED_EXE="$MASC_EIO_EXE"
fi

# Eio server has different CLI format and is HTTP-only
if [ "$EIO_MODE" = "true" ]; then
    echo "Starting MASC MCP server (HTTP mode, $RUNTIME_NAME)..." >&2
    echo "  Port: $PORT" >&2
    echo "  Base path: $BASE_PATH" >&2
    echo "  MCP endpoint: http://127.0.0.1:$PORT/mcp" >&2
    echo "  SSE: use Accept: text/event-stream on /mcp" >&2
    exec "$SELECTED_EXE" --port="$PORT" --base-path="$BASE_PATH"
elif [ "$HTTP_MODE" = "true" ]; then
    echo "Starting MASC MCP server (HTTP mode, $RUNTIME_NAME)..." >&2
    echo "  Port: $PORT" >&2
    echo "  Base path: $BASE_PATH" >&2
    echo "  MCP endpoint: http://127.0.0.1:$PORT/mcp" >&2
    echo "  SSE: use Accept: text/event-stream on /mcp" >&2
    exec "$SELECTED_EXE" --http --port "$PORT" --path "$BASE_PATH"
else
    echo "Starting MASC MCP server (stdio mode, $RUNTIME_NAME)..." >&2
    echo "  Base path: $BASE_PATH" >&2
    exec "$SELECTED_EXE" --stdio --path "$BASE_PATH"
fi
