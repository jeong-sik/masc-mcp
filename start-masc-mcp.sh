#!/bin/bash
# MASC MCP Server (OCaml) - Start Script (HTTP/SSE default)
# Usage: ./start-masc-mcp.sh [--stdio] [--http] [--eio] [--lwt] [--port PORT] [--base-path PATH|--path PATH]
# Note: Eio is the default runtime; --lwt exits with an error.

set -e

# Optional: load OPAM environment if available (must never be fatal for MCP startup)
if command -v opam >/dev/null 2>&1; then
    eval "$(opam env 2>/dev/null)" >/dev/null 2>/dev/null || true
fi

# Storage backends are opt-in. Use MASC_STORAGE_TYPE + MASC_REDIS_URL/MASC_POSTGRES_URL explicitly.

# Room/namespace: derive from ME_ROOT directory name if not set
# e.g., ME_ROOT=/Users/dancer/me → cluster "me"
# e.g., ME_ROOT=/Users/dancer/workspace/kidsnote → cluster "kidsnote"
if [ -z "$MASC_CLUSTER_NAME" ] && [ -n "$ME_ROOT" ]; then
    export MASC_CLUSTER_NAME="$(basename "$ME_ROOT")"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Resolve executable path
# Priority: 1. Release binary  2. Workspace build  3. Local build  4. Installed  5. Auto-download
RELEASE_BINARY="$SCRIPT_DIR/masc-mcp-macos-arm64"
WORKSPACE_EXE="$SCRIPT_DIR/../_build/default/masc-mcp/bin/main.exe"
LOCAL_EXE="$SCRIPT_DIR/_build/default/bin/main.exe"
INSTALLED_EXE="$(command -v masc-mcp || true)"
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
# 4. System-installed
elif [ -n "$INSTALLED_EXE" ]; then
    MASC_EXE="$INSTALLED_EXE"
fi

if [ -x "$WORKSPACE_EIO_EXE" ]; then
    MASC_EIO_EXE="$WORKSPACE_EIO_EXE"
elif [ -x "$LOCAL_EIO_EXE" ]; then
    MASC_EIO_EXE="$LOCAL_EIO_EXE"
fi

# 5. Build Eio version if not found (Lwt deprecated, download disabled)
if [ -z "$MASC_EIO_EXE" ]; then
    echo "Building MASC MCP server from source..." >&2
    if ! command -v dune >/dev/null 2>&1; then
        echo "Error: dune not found. Install dune first." >&2
        exit 1
    fi
    dune build --root "$SCRIPT_DIR" bin/main_eio.exe 1>&2
    if [ -x "$LOCAL_EIO_EXE" ]; then
        MASC_EIO_EXE="$LOCAL_EIO_EXE"
    else
        echo "Error: build failed." >&2
        exit 1
    fi
fi

# Rebuild Eio version if sources are newer than the executable (avoids stale binary runs)
# NOTE: Lwt version (main.exe) is deprecated - Eio is now the default
if [ -n "$MASC_EIO_EXE" ] && command -v dune >/dev/null 2>&1; then
    if find "$SCRIPT_DIR/bin" "$SCRIPT_DIR/lib" \
        -type f \( -name '*.ml' -o -name '*.mli' -o -name 'dune' \) \
        -newer "$MASC_EIO_EXE" 2>/dev/null | head -n 1 | grep -q .; then
        echo "Rebuilding MASC MCP server (stale executable detected)..." >&2
        dune build --root "$SCRIPT_DIR" bin/main_eio.exe 1>&2

        if [ -x "$WORKSPACE_EIO_EXE" ]; then
            MASC_EIO_EXE="$WORKSPACE_EIO_EXE"
        elif [ -x "$LOCAL_EIO_EXE" ]; then
            MASC_EIO_EXE="$LOCAL_EIO_EXE"
        fi
    fi
fi

# Default arguments
PORT="${MASC_MCP_PORT:-8935}"
HTTP_MODE="${MASC_MCP_HTTP:-true}"
BASE_PATH="${MASC_BASE_PATH:-${ME_ROOT:-$(pwd -P)}}"
# NOTE: Eio is now the default runtime (Lwt deprecated since 2026-01)
EIO_MODE="true"

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
            echo "Error: Lwt runtime is deprecated since 2026-01." >&2
            echo "Please use Eio (default). Lwt support has been removed." >&2
            exit 1
            ;;
        --port)
            PORT="$2"
            shift 2
            ;;
        --base-path|--path)
            BASE_PATH="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            echo "Usage: $0 [--stdio] [--http] [--eio] [--lwt] [--port PORT] [--base-path PATH|--path PATH]" >&2
            echo "Note: Eio is the default runtime; --lwt exits with an error." >&2
            exit 1
            ;;
    esac
done

resolve_base_path() {
    local path="$1"

    if [ -f "$path/.git" ]; then
        local gitdir
        gitdir="$(sed -n 's/^gitdir: //p' "$path/.git")"
        if [ -n "$gitdir" ]; then
            case "$gitdir" in
                */.git/worktrees/*)
                    echo "${gitdir%/.git/worktrees/*}"
                    return
                    ;;
                */.git)
                    echo "${gitdir%/.git}"
                    return
                    ;;
            esac
        fi
    fi

    if [ -d "$path/.git" ]; then
        echo "$path"
        return
    fi

    if command -v git >/dev/null 2>&1; then
        local git_root
        git_root="$(git -C "$path" rev-parse --show-toplevel 2>/dev/null || true)"
        if [ -n "$git_root" ]; then
            echo "$git_root"
            return
        fi
    fi

    echo "$path"
}

RESOLVED_BASE_PATH="$(resolve_base_path "$BASE_PATH")"

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
        dune build --root "$SCRIPT_DIR" bin/main_eio.exe 1>&2
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
    echo "  Base path: $RESOLVED_BASE_PATH" >&2
    if [ "$RESOLVED_BASE_PATH" != "$BASE_PATH" ]; then
        echo "  Base path (input): $BASE_PATH" >&2
    fi
    echo "  MASC dir: $RESOLVED_BASE_PATH/.masc" >&2
    echo "  MCP endpoint: http://127.0.0.1:$PORT/mcp" >&2
    echo "  SSE: use Accept: text/event-stream on /mcp" >&2
    exec "$SELECTED_EXE" --port="$PORT" --base-path="$BASE_PATH"
elif [ "$HTTP_MODE" = "true" ]; then
    echo "Starting MASC MCP server (HTTP mode, $RUNTIME_NAME)..." >&2
    echo "  Port: $PORT" >&2
    echo "  Base path: $RESOLVED_BASE_PATH" >&2
    if [ "$RESOLVED_BASE_PATH" != "$BASE_PATH" ]; then
        echo "  Base path (input): $BASE_PATH" >&2
    fi
    echo "  MASC dir: $RESOLVED_BASE_PATH/.masc" >&2
    echo "  MCP endpoint: http://127.0.0.1:$PORT/mcp" >&2
    echo "  SSE: use Accept: text/event-stream on /mcp" >&2
    exec "$SELECTED_EXE" --http --port "$PORT" --path "$BASE_PATH"
else
    echo "Starting MASC MCP server (stdio mode, $RUNTIME_NAME)..." >&2
    echo "  Base path: $RESOLVED_BASE_PATH" >&2
    if [ "$RESOLVED_BASE_PATH" != "$BASE_PATH" ]; then
        echo "  Base path (input): $BASE_PATH" >&2
    fi
    echo "  MASC dir: $RESOLVED_BASE_PATH/.masc" >&2
    exec "$SELECTED_EXE" --stdio --path "$BASE_PATH"
fi
