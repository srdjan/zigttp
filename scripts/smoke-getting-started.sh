#!/bin/bash
# Smoke test for docs/getting-started.md.
# Runs the guide path in a temp dir:
#   build CLI -> init api -> dev -> test -> deploy.
#
# Usage: bash scripts/smoke-getting-started.sh

set -euo pipefail

ZIG="${ZIG:-zig}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZIGTTP="${ZIGTTP:-$REPO_ROOT/zig-out/bin/zigttp}"

DEV_PORT=$((RANDOM % 1000 + 5600))
DEPLOY_PORT=$((DEV_PORT + 1))

# Positional template (portable): GNU mktemp replaces the trailing X's; BSD
# mktemp does too. Avoid `-t PREFIX`, where BSD leaves the X's literal.
TMP_DIR=$(mktemp -d "${TMPDIR:-/tmp}/zigttp-getting-started.XXXXXX")
APP_NAME="my-app"
APP_DIR="$TMP_DIR/$APP_NAME"
HTMX_DIR="$TMP_DIR/htmx-app"

cleanup() {
    pkill -f "zigttp.*--port $DEV_PORT" 2>/dev/null || true
    pkill -f "$APP_DIR/.zigttp/deploy/$APP_NAME" 2>/dev/null || true
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

step() { printf '\n[getting-started] %s\n' "$*"; }
fail() { printf '\n[getting-started] FAIL: %s\n' "$*" >&2; exit 1; }

wait_for_http() {
    local url="$1" attempts="$2" body
    for _ in $(seq 1 "$attempts"); do
        if body=$(/usr/bin/curl -sf "$url" 2>/dev/null); then
            printf '%s' "$body"
            return 0
        fi
        sleep 0.2
    done
    return 1
}

stop_bg() {
    {
        local pid="$1" pattern="$2"
        kill "$pid" || true
        pkill -f "$pattern" || true
        wait "$pid" || true
    } 2>/dev/null
}

step "build zigttp"
(cd "$REPO_ROOT" && "$ZIG" build) || fail "zig build"
[ -x "$ZIGTTP" ] || fail "missing $ZIGTTP after build"
[ -x "$REPO_ROOT/zig-out/bin/zigttp-runtime" ] || fail "missing zigttp-runtime after build"

step "default help lists only the five core commands"
help_out="$("$ZIGTTP" --help)"
for verb in init dev test expert deploy; do
    printf '%s' "$help_out" | grep -q "zigttp $verb" || fail "core command missing from help: $verb"
done
for hidden in check serve compile proofs; do
    printf '%s' "$help_out" | grep -q "zigttp $hidden " && fail "advanced command leaked into default help: $hidden"
done

step "init api project"
(cd "$TMP_DIR" && "$ZIGTTP" init "$APP_NAME" --template api >/dev/null) || fail "init api"
[ -f "$APP_DIR/zigttp.json" ] || fail "missing zigttp.json"
[ -f "$APP_DIR/src/handler.ts" ] || fail "missing src/handler.ts"
[ -f "$APP_DIR/tests/handler.test.jsonl" ] || fail "missing tests/handler.test.jsonl"

cd "$APP_DIR"

step "dev serves /health"
"$ZIGTTP" dev --port "$DEV_PORT" --no-tour >/tmp/zigttp-getting-started-dev.log 2>&1 &
DEV_PID=$!
health_body=$(wait_for_http "http://127.0.0.1:$DEV_PORT/health" 75) || {
    cat /tmp/zigttp-getting-started-dev.log >&2 || true
    fail "dev did not serve /health"
}
printf '%s' "$health_body" | grep -q '"ok":true' || fail "unexpected /health body: $health_body"
stop_bg "$DEV_PID" "zigttp.*--port $DEV_PORT"

step "test"
"$ZIGTTP" test >/dev/null || fail "test"

step "deploy and run artifact"
"$ZIGTTP" deploy >/dev/null 2>&1 || fail "deploy"
[ -x "$APP_DIR/.zigttp/deploy/$APP_NAME" ] || fail "missing deploy artifact"
[ -s "$APP_DIR/.zigttp/proofs.jsonl" ] || fail "missing proof ledger"
"$APP_DIR/.zigttp/deploy/$APP_NAME" -p "$DEPLOY_PORT" >/dev/null 2>&1 &
DEPLOY_PID=$!
deploy_body=$(wait_for_http "http://127.0.0.1:$DEPLOY_PORT/health" 25) || fail "deploy artifact did not serve /health"
printf '%s' "$deploy_body" | grep -q '"ok":true' || fail "unexpected deploy artifact body: $deploy_body"
stop_bg "$DEPLOY_PID" "$APP_DIR/.zigttp/deploy/$APP_NAME"

step "htmx scaffold uses tsx"
(cd "$TMP_DIR" && "$ZIGTTP" init htmx-app --template htmx >/dev/null) || fail "init htmx"
[ -f "$HTMX_DIR/src/handler.tsx" ] || fail "htmx missing src/handler.tsx"
[ ! -f "$HTMX_DIR/src/handler.ts" ] || fail "htmx should not write src/handler.ts"
(cd "$HTMX_DIR" && "$ZIGTTP" test >/dev/null) || fail "htmx test"

step "PASS: getting started guide commands work"
