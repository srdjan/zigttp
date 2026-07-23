#!/bin/bash
# E2E test for handler panic isolation (B1).
# Verifies that a panicking handler returns HTTP 500 and the server survives to
# serve subsequent requests. Uses the internal --_debug-panic-path test hook to
# trigger a real panic on a specific request path inside callHandlerGuarded.
#
# Usage: bash scripts/test-panic-isolation.sh [--skip-build] [--zttp PATH]

set -euo pipefail

SKIP_BUILD=0
ZIG="${ZIG:-zig}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZTTP="${ZTTP:-$REPO_ROOT/zig-out/bin/zttp}"
PORT=$((RANDOM % 1000 + 14000))

while [ "$#" -gt 0 ]; do
    case "$1" in
        --skip-build)
            SKIP_BUILD=1
            shift
            ;;
        --zttp)
            [ "$#" -ge 2 ] || { printf '[panic-isolation] FAIL: --zttp requires a path\n' >&2; exit 1; }
            ZTTP="$2"
            shift 2
            ;;
        *)
            printf '[panic-isolation] FAIL: unknown argument: %s\n' "$1" >&2
            exit 1
            ;;
    esac
done

TMP_DIR=$(mktemp -d -t zttp-panic-XXXXXX)
APP_DIR="$TMP_DIR/panic-app"
SERVER_LOG="$TMP_DIR/server.log"
SRV_PID=""

cleanup() {
    if [ -n "$SRV_PID" ]; then
        kill "$SRV_PID" 2>/dev/null || true
        sleep 0.2
        kill -9 "$SRV_PID" 2>/dev/null || true
        wait "$SRV_PID" 2>/dev/null || true
    fi
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

step() { printf '\n[panic-isolation] %s\n' "$*"; }
fail() {
    printf '\n[panic-isolation] FAIL: %s\n' "$*" >&2
    if [ -f "$SERVER_LOG" ]; then
        printf '\n[panic-isolation] server log:\n' >&2
        sed -n '1,120p' "$SERVER_LOG" >&2
    fi
    exit 1
}

if [ "$SKIP_BUILD" = "1" ]; then
    step "use existing $ZTTP"
else
    step "build $ZTTP"
    (cd "$REPO_ROOT" && $ZIG build) || fail "zig build failed"
fi
[ -x "$ZTTP" ] || fail "missing $ZTTP after build"

step "init test app"
(cd "$TMP_DIR" && "$ZTTP" init "panic-app" >/dev/null) || fail "init failed"
[ -f "$APP_DIR/src/handler.ts" ] || fail "handler.ts not created by init"

cat > "$APP_DIR/src/handler.ts" <<'HANDLER'
import type { Spec } from "zttp:types";

type Guardrails = Spec<
    | "deterministic"
    | "no_secret_leakage"
    | "injection_safe"
>;

function handler(req: Request): Response & Guardrails {
    if (req.method === "GET" && req.path === "/crash") {
        return Response.text("panic injection route");
    }
    if (req.method === "GET" && req.path === "/ok") {
        return Response.text("ok");
    }
    return Response.text("not found", { status: 404 });
}
HANDLER

step "start server on :$PORT with panic injection path /crash"
"$ZTTP" serve "$APP_DIR/src/handler.ts" -p "$PORT" --_debug-panic-path /crash \
    >"$SERVER_LOG" 2>&1 &
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
    --max-time 5 "http://127.0.0.1:$PORT/ok")
[ "$ok_status" = "200" ] || fail "expected 200 on recovery request, got $ok_status"

step "PASS: handler panic isolated, worker survived and recovered"
exit 0
