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

# Resolve executable path
# Priority: 1. Release binary  2. Workspace build  3. Local build  4. Auto-download
RELEASE_BINARY="$SCRIPT_DIR/masc-mcp-macos-arm64"
WORKSPACE_EXE="$SCRIPT_DIR/../_build/default/masc-mcp/bin/main.exe"
LOCAL_EXE="$SCRIPT_DIR/_build/default/bin/main.exe"
# Eio-based server (main_eio.exe) - for PostgresNative backend
WORKSPACE_EIO_EXE="$SCRIPT_DIR/../_build/default/masc-mcp/bin/main_eio.exe"
LOCAL_EIO_EXE="$SCRIPT_DIR/_build/default/bin/main_eio.exe"
MASC_EXE=""
MASC_EIO_EXE=""

# 1. Pre-downloaded release binary (fastest, no build needed)
if [ -x "$RELEASE_BINARY" ]; then
    MASC_EXE="$RELEASE_BINARY"
# 2. Workspace build
elif [ -x "$WORKSPACE_EXE" ]; then
    MASC_EXE="$WORKSPACE_EXE"
# 3. Local build
elif [ -x "$LOCAL_EXE" ]; then
    MASC_EXE="$LOCAL_EXE"
fi

if [ -x "$WORKSPACE_EIO_EXE" ]; then
    MASC_EIO_EXE="$WORKSPACE_EIO_EXE"
elif [ -x "$LOCAL_EIO_EXE" ]; then
    MASC_EIO_EXE="$LOCAL_EIO_EXE"
fi

# 4. Auto-download from GitHub releases if nothing found
if [ -z "$MASC_EXE" ]; then
    echo "No binary found. Downloading from GitHub releases..." >&2
    RELEASE_URL="https://github.com/jeong-sik/masc-mcp/releases/latest/download/masc-mcp-macos-arm64"
    if curl -fsSL -o "$RELEASE_BINARY" "$RELEASE_URL" 2>/dev/null; then
        chmod +x "$RELEASE_BINARY"
        MASC_EXE="$RELEASE_BINARY"
        echo "Downloaded: $RELEASE_BINARY" >&2
    else
        # Fallback: build from source
        echo "Download failed. Building from source..." >&2
        if ! command -v dune >/dev/null 2>&1; then
            echo "Error: dune not found. Install dune or download binary manually." >&2
            exit 1
        fi
        dune build ./bin/main.exe 1>&2
        if [ -x "$LOCAL_EXE" ]; then
            MASC_EXE="$LOCAL_EXE"
        else
            echo "Error: build failed." >&2
            exit 1
        fi
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
