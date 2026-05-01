#!/bin/bash
# End-to-end smoke test for the v1 user flow:
#   init -> doctor -> check -> studio -> build -> run -> deploy --local -> run.
# Exits non-zero on first failure, with a one-line diagnostic.
#
# Usage: bash scripts/smoke-v1.sh

set -euo pipefail

ZIG="${ZIG:-zig}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZIGTTP="${ZIGTTP:-$REPO_ROOT/zig-out/bin/zigttp}"

# Random ports in the ephemeral range; reduces flake when smoke runs concurrently.
STUDIO_PORT=$((RANDOM % 1000 + 5000))
BUILD_PORT=$((STUDIO_PORT + 1))
DEPLOY_PORT=$((STUDIO_PORT + 2))

TMP_DIR=$(mktemp -d -t zigttp-smoke-XXXXXX)
APP_NAME="smoke-app"
APP_DIR="$TMP_DIR/$APP_NAME"

cleanup() {
    pkill -f "$APP_DIR/.zigttp/build/$APP_NAME" 2>/dev/null || true
    pkill -f "$APP_DIR/.zigttp/deploy/$APP_NAME" 2>/dev/null || true
    pkill -f "zigttp.*--port $STUDIO_PORT" 2>/dev/null || true
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

step() { printf '\n[smoke-v1] %s\n' "$*"; }
fail() { printf '\n[smoke-v1] FAIL: %s\n' "$*" >&2; exit 1; }

# Poll URL until reachable (HTTP 2xx) or attempts exhausted, then return its body.
# Usage: body=$(wait_for_http URL ATTEMPTS FAIL_MSG)
wait_for_http() {
    local url="$1" attempts="$2" fail_msg="$3"
    for _ in $(seq 1 "$attempts"); do
        if /usr/bin/curl -sf "$url" >/dev/null 2>&1; then
            /usr/bin/curl -sf "$url"
            return 0
        fi
        sleep 0.2
    done
    fail "$fail_msg"
}

# Stop a backgrounded zigttp process: kill by PID, sweep stragglers by pattern, wait.
# Usage: stop_bg PID PATTERN
stop_bg() {
    local pid="$1" pattern="$2"
    kill "$pid" 2>/dev/null || true
    pkill -f "$pattern" 2>/dev/null || true
    wait 2>/dev/null || true
}

step "build $ZIGTTP and zigttp-runtime"
(cd "$REPO_ROOT" && $ZIG build) || fail "zig build"
[ -x "$ZIGTTP" ] || fail "missing $ZIGTTP after build"

step "init $APP_NAME under $TMP_DIR"
(cd "$TMP_DIR" && "$ZIGTTP" init "$APP_NAME" >/dev/null) || fail "init"
[ -f "$APP_DIR/zigttp.json" ] || fail "no zigttp.json after init"
[ -f "$APP_DIR/src/handler.ts" ] || fail "no src/handler.ts after init"
[ -f "$APP_DIR/tests/handler.test.jsonl" ] || fail "no tests/handler.test.jsonl after init"
[ -f "$APP_DIR/README.md" ] || fail "no README.md after init"

cd "$APP_DIR"

step "doctor"
"$ZIGTTP" doctor >/dev/null || fail "doctor exited non-zero"

step "check"
"$ZIGTTP" check >/dev/null || fail "check exited non-zero"

step "studio launches on :$STUDIO_PORT and serves /_zigttp/studio/state.json"
"$ZIGTTP" studio --port "$STUDIO_PORT" >/dev/null 2>&1 &
STUDIO_PID=$!
# Studio spawns a child via zigttp-runtime; the dashboard becomes available
# after pool prewarm completes (~10s budget at 0.2s/attempt).
state_url="http://127.0.0.1:$STUDIO_PORT/_zigttp/studio/state.json"
state_body=$(wait_for_http "$state_url" 50 "studio /_zigttp/studio/state.json never became reachable")
echo "$state_body" | grep -q '"verdict"' || fail "studio state.json missing verdict field: $state_body"
stop_bg "$STUDIO_PID" "zigttp.*--port $STUDIO_PORT"

step "build emits .zigttp/build/$APP_NAME"
"$ZIGTTP" build >/dev/null 2>&1 || fail "build exited non-zero"
[ -x "$APP_DIR/.zigttp/build/$APP_NAME" ] || fail "build artifact missing or not executable"

step "run build artifact on :$BUILD_PORT and curl /"
"$APP_DIR/.zigttp/build/$APP_NAME" -p "$BUILD_PORT" >/dev/null 2>&1 &
BUILD_PID=$!
build_body=$(wait_for_http "http://127.0.0.1:$BUILD_PORT/" 25 "build artifact did not respond on /")
[ -n "$build_body" ] || fail "build artifact returned empty body on /"
stop_bg "$BUILD_PID" "$APP_DIR/.zigttp/build/$APP_NAME"

step "deploy --local emits .zigttp/deploy/$APP_NAME and appends ledger row"
"$ZIGTTP" deploy --local >/dev/null 2>&1 || fail "deploy --local exited non-zero"
[ -x "$APP_DIR/.zigttp/deploy/$APP_NAME" ] || fail "deploy artifact missing or not executable"
[ -s "$APP_DIR/.zigttp/proofs.jsonl" ] || fail "proofs.jsonl missing or empty after deploy --local"
grep -q '"kind":"deploy"' "$APP_DIR/.zigttp/proofs.jsonl" || fail "no kind=deploy row in proofs.jsonl"

step "run deploy artifact on :$DEPLOY_PORT and curl /"
"$APP_DIR/.zigttp/deploy/$APP_NAME" -p "$DEPLOY_PORT" >/dev/null 2>&1 &
DEPLOY_PID=$!
deploy_body=$(wait_for_http "http://127.0.0.1:$DEPLOY_PORT/" 25 "deploy artifact did not respond on /")
[ -n "$deploy_body" ] || fail "deploy artifact returned empty body on /"
stop_bg "$DEPLOY_PID" "$APP_DIR/.zigttp/deploy/$APP_NAME"

step "PASS: init -> doctor -> check -> studio -> build -> deploy --local"
exit 0
