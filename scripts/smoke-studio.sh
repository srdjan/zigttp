#!/bin/bash
# Smoke test for the opt-in browser proof workbench (zttp studio).
#
# studio is compiled out of the default build (build.zig enable_studio=false),
# so this script builds with -Dstudio and then drives studio end to end. It is
# kept out of scripts/smoke-v1.sh so the default fast path passes on a clean
# checkout without the opt-in feature.
#
# Usage: bash scripts/smoke-studio.sh
#   ZTTP=/path/to/zttp  to skip the build and use an existing -Dstudio binary.

set -euo pipefail

ZIG="${ZIG:-zig}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZTTP="${ZTTP:-$REPO_ROOT/zig-out/bin/zttp}"

STUDIO_PORT=$((RANDOM % 1000 + 5000))
TMP_DIR=$(mktemp -d -t zttp-smoke-studio-XXXXXX)
APP_NAME="smoke-studio-app"
APP_DIR="$TMP_DIR/$APP_NAME"

cleanup() {
    pkill -f "zttp.*--port $STUDIO_PORT" 2>/dev/null || true
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

step() { printf '\n[smoke-studio] %s\n' "$*"; }
fail() { printf '\n[smoke-studio] FAIL: %s\n' "$*" >&2; exit 1; }

# Poll Studio state until proof analysis reaches a ready payload with verdict.
# Studio serves state.json before analysis completes; that early
# {"status":"checking",...} response is not enough.
# Usage: body=$(wait_for_studio_ready URL ATTEMPTS) || fail "..."
wait_for_studio_ready() {
    local url="$1" attempts="$2" body
    for _ in $(seq 1 "$attempts"); do
        if body=$(/usr/bin/curl -sf "$url" 2>/dev/null); then
            if printf '%s' "$body" | grep -q '"status":"ready"' &&
               printf '%s' "$body" | grep -q '"verdict"'; then
                printf '%s' "$body"
                return 0
            fi
        fi
        sleep 0.2
    done
    [ -n "${body:-}" ] && printf '%s' "$body"
    return 1
}

stop_bg() {
    local pid="$1" pattern="$2"
    kill "$pid" 2>/dev/null || true
    pkill -f "$pattern" 2>/dev/null || true
    wait 2>/dev/null || true
}

step "build zttp with -Dstudio"
(cd "$REPO_ROOT" && $ZIG build -Dstudio) || fail "zig build -Dstudio"
[ -x "$ZTTP" ] || fail "missing $ZTTP after build"

step "init $APP_NAME under $TMP_DIR"
(cd "$TMP_DIR" && "$ZTTP" init "$APP_NAME" >/dev/null) || fail "init"
cd "$APP_DIR"

step "studio launches on :$STUDIO_PORT and serves /_zttp/studio/state.json"
"$ZTTP" studio --port "$STUDIO_PORT" >/dev/null 2>&1 &
STUDIO_PID=$!
# Studio re-enters the developer CLI with `serve --studio --watch --prove`;
# wait for proof analysis to finish, not just for the HTTP endpoint to exist.
state_url="http://127.0.0.1:$STUDIO_PORT/_zttp/studio/state.json"
state_body=$(wait_for_studio_ready "$state_url" 75) || fail "studio state.json never reached ready verdict state: $state_body"
stop_bg "$STUDIO_PID" "zttp.*--port $STUDIO_PORT"

step "PASS: studio init -> launch -> ready verdict"
exit 0
