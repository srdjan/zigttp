#!/bin/bash
# E2E test for handler panic isolation (B1).
# Verifies that a panicking handler returns HTTP 500 and the server survives to
# serve subsequent requests. Uses ZIGTTP_DEBUG_PANIC_PATH to inject a panic on
# a specific request path via the setjmp recovery hook in callHandlerGuarded.
#
# Usage: bash scripts/test-panic-isolation.sh

set -euo pipefail

ZIG="${ZIG:-zig}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZIGTTP="${ZIGTTP:-$REPO_ROOT/zig-out/bin/zigttp}"
PORT=$((RANDOM % 1000 + 14000))

TMP_DIR=$(mktemp -d -t zigttp-panic-XXXXXX)
APP_DIR="$TMP_DIR/panic-app"
SRV_PID=""

cleanup() {
    [ -n "$SRV_PID" ] && kill "$SRV_PID" 2>/dev/null || true
    [ -n "$SRV_PID" ] && wait "$SRV_PID" 2>/dev/null || true
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

step() { printf '\n[panic-isolation] %s\n' "$*"; }
fail() { printf '\n[panic-isolation] FAIL: %s\n' "$*" >&2; exit 1; }

step "build $ZIGTTP"
(cd "$REPO_ROOT" && $ZIG build) || fail "zig build failed"
[ -x "$ZIGTTP" ] || fail "missing $ZIGTTP after build"

step "init test app"
(cd "$TMP_DIR" && "$ZIGTTP" init "panic-app" >/dev/null) || fail "init failed"
[ -f "$APP_DIR/src/handler.ts" ] || fail "handler.ts not created by init"

step "start server on :$PORT with ZIGTTP_DEBUG_PANIC_PATH=/crash"
ZIGTTP_DEBUG_PANIC_PATH=/crash "$ZIGTTP" dev "$APP_DIR/src/handler.ts" -p "$PORT" \
    >/dev/null 2>&1 &
SRV_PID=$!

# Wait up to 5s for the server to accept connections.
ready=0
for i in $(seq 1 25); do
    if /usr/bin/curl -sf "http://127.0.0.1:$PORT/_health" >/dev/null 2>&1; then
        ready=1
        break
    fi
    sleep 0.2
done
[ "$ready" = "1" ] || fail "server did not become ready on :$PORT within 5s"

step "send panicking request GET /crash - expect HTTP 500"
status=$(/usr/bin/curl -s -o /dev/null -w "%{http_code}" \
    --max-time 5 "http://127.0.0.1:$PORT/crash")
[ "$status" = "500" ] || fail "expected 500 from /crash, got $status"

step "verify server process survived the panic"
kill -0 "$SRV_PID" 2>/dev/null || fail "server process exited after handler panic"

step "verify subsequent request succeeds (slot rebuilt after quarantine)"
ok_status=$(/usr/bin/curl -s -o /dev/null -w "%{http_code}" \
    --max-time 5 "http://127.0.0.1:$PORT/")
[ "$ok_status" = "200" ] || fail "expected 200 on recovery request, got $ok_status"

step "PASS: handler panic isolated, worker survived and recovered"
exit 0
