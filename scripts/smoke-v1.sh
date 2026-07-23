#!/bin/bash
# End-to-end smoke test for the v1 user flow:
#   init -> doctor -> check -> build -> run -> deploy -> run.
# Exits non-zero on first failure, with a one-line diagnostic.
#
# studio is compiled out of the default build, so it is exercised separately
# by scripts/smoke-studio.sh (which builds with -Dstudio); keep this fast path
# free of any opt-in feature so it passes on a clean checkout.
#
# Usage: bash scripts/smoke-v1.sh

set -euo pipefail

ZIG="${ZIG:-zig}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZTTP="${ZTTP:-$REPO_ROOT/zig-out/bin/zttp}"

# Random ports in the ephemeral range; reduces flake when smoke runs concurrently.
BASE_PORT=$((RANDOM % 1000 + 5000))
BUILD_PORT=$((BASE_PORT + 1))
DEPLOY_PORT=$((BASE_PORT + 2))

TMP_DIR=$(mktemp -d -t zttp-smoke-XXXXXX)
APP_NAME="smoke-app"
APP_DIR="$TMP_DIR/$APP_NAME"
BROKEN_APP_DIR="$TMP_DIR/broken-app"
RUNTIME_BIN="$REPO_ROOT/zig-out/bin/zttp-runtime"
RUNTIME_BAK="$RUNTIME_BIN.smoke-bak"

cleanup() {
    pkill -f "$APP_DIR/.zttp/build/$APP_NAME" 2>/dev/null || true
    pkill -f "$APP_DIR/.zttp/deploy/$APP_NAME" 2>/dev/null || true
    # Restore runtime if the missing-runtime negative test was interrupted.
    [ -e "$RUNTIME_BAK" ] && mv "$RUNTIME_BAK" "$RUNTIME_BIN" 2>/dev/null
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

step() { printf '\n[smoke-v1] %s\n' "$*"; }
fail() { printf '\n[smoke-v1] FAIL: %s\n' "$*" >&2; exit 1; }

# Poll URL until reachable (HTTP 2xx) or attempts exhausted, printing the
# response body to stdout. Returns non-zero on exhaustion so callers can
# chain `|| fail "..."` - bash command substitution does not propagate
# `set -e`, so we cannot rely on an inner `fail` to abort the parent.
# Usage: body=$(wait_for_http URL ATTEMPTS) || fail "..."
wait_for_http() {
    local url="$1" attempts="$2"
    for _ in $(seq 1 "$attempts"); do
        if body=$(/usr/bin/curl -sf "$url" 2>/dev/null); then
            printf '%s' "$body"
            return 0
        fi
        sleep 0.2
    done
    return 1
}

# Stop a backgrounded zttp process: kill by PID, sweep stragglers by pattern, wait.
# Usage: stop_bg PID PATTERN
stop_bg() {
    local pid="$1" pattern="$2"
    kill "$pid" 2>/dev/null || true
    pkill -f "$pattern" 2>/dev/null || true
    wait 2>/dev/null || true
}

step "build $ZTTP and zttp-runtime"
(cd "$REPO_ROOT" && $ZIG build) || fail "zig build"
[ -x "$ZTTP" ] || fail "missing $ZTTP after build"

step "negative: init rejects path-like project names"
if invalid_out=$(cd "$TMP_DIR" && "$ZTTP" init "../bad-app" 2>&1); then
    fail "init accepted path-like project name"
fi
printf '%s' "$invalid_out" | grep -q "Invalid project name" || fail "init rejection did not explain invalid project name: $invalid_out"

step "negative: build outside a project prints a project diagnostic"
mkdir -p "$TMP_DIR/no-project"
if missing_out=$(cd "$TMP_DIR/no-project" && "$ZTTP" build 2>&1); then
    fail "build succeeded without zttp.json"
fi
printf '%s' "$missing_out" | grep -q "No zttp.json found" || fail "missing-project diagnostic did not mention zttp.json: $missing_out"

step "negative: broken handler build fails without emitting an artifact"
(cd "$TMP_DIR" && "$ZTTP" init "broken-app" >/dev/null 2>&1) || fail "init broken-app"
printf 'function handler(req) {\n' > "$BROKEN_APP_DIR/src/handler.ts"
if broken_out=$(cd "$BROKEN_APP_DIR" && "$ZTTP" build 2>&1); then
    fail "build succeeded for syntactically broken handler"
fi
[ ! -e "$BROKEN_APP_DIR/.zttp/build/broken-app" ] || fail "broken handler emitted a build artifact"
printf '%s' "$broken_out" | grep -Eq "Compilation failed|Parse|error" || fail "broken-handler diagnostic was not actionable: $broken_out"

step "init $APP_NAME under $TMP_DIR"
(cd "$TMP_DIR" && "$ZTTP" init "$APP_NAME" >/dev/null) || fail "init"
[ -f "$APP_DIR/zttp.json" ] || fail "no zttp.json after init"
[ -f "$APP_DIR/src/handler.ts" ] || fail "no src/handler.ts after init"
[ -f "$APP_DIR/tests/handler.test.jsonl" ] || fail "no tests/handler.test.jsonl after init"
[ -f "$APP_DIR/README.md" ] || fail "no README.md after init"

cd "$APP_DIR"

step "doctor"
"$ZTTP" doctor >/dev/null || fail "doctor exited non-zero"

step "check"
"$ZTTP" check >/dev/null || fail "check exited non-zero"

step "build emits .zttp/build/$APP_NAME"
"$ZTTP" build >/dev/null 2>&1 || fail "build exited non-zero"
[ -x "$APP_DIR/.zttp/build/$APP_NAME" ] || fail "build artifact missing or not executable"

step "run build artifact on :$BUILD_PORT and curl /"
"$APP_DIR/.zttp/build/$APP_NAME" -p "$BUILD_PORT" >/dev/null 2>&1 &
BUILD_PID=$!
build_body=$(wait_for_http "http://127.0.0.1:$BUILD_PORT/" 25) || fail "build artifact did not respond on /"
[ -n "$build_body" ] || fail "build artifact returned empty body on /"
stop_bg "$BUILD_PID" "$APP_DIR/.zttp/build/$APP_NAME"

step "negative: deploy without zttp-runtime emits an install hint"
mv "$RUNTIME_BIN" "$RUNTIME_BAK"
missing_runtime_status=0
missing_runtime_out=$("$ZTTP" deploy 2>&1) || missing_runtime_status=$?
mv "$RUNTIME_BAK" "$RUNTIME_BIN"
[ "$missing_runtime_status" -ne 0 ] || fail "deploy succeeded with zttp-runtime missing"
printf '%s' "$missing_runtime_out" | grep -q "zttp-runtime template not found" || fail "missing-runtime diagnostic was not actionable: $missing_runtime_out"
[ ! -e "$APP_DIR/.zttp/deploy/$APP_NAME" ] || fail "deploy artifact was created despite missing runtime"

step "deploy emits .zttp/deploy/$APP_NAME and appends ledger row"
"$ZTTP" deploy >/dev/null 2>&1 || fail "deploy exited non-zero"
[ -x "$APP_DIR/.zttp/deploy/$APP_NAME" ] || fail "deploy artifact missing or not executable"
[ -s "$APP_DIR/.zttp/proofs.jsonl" ] || fail "proofs.jsonl missing or empty after deploy"
grep -q '"kind":"deploy"' "$APP_DIR/.zttp/proofs.jsonl" || fail "no kind=deploy row in proofs.jsonl"

step "run deploy artifact on :$DEPLOY_PORT and curl /"
"$APP_DIR/.zttp/deploy/$APP_NAME" -p "$DEPLOY_PORT" >/dev/null 2>&1 &
DEPLOY_PID=$!
deploy_body=$(wait_for_http "http://127.0.0.1:$DEPLOY_PORT/" 25) || fail "deploy artifact did not respond on /"
[ -n "$deploy_body" ] || fail "deploy artifact returned empty body on /"
stop_bg "$DEPLOY_PID" "$APP_DIR/.zttp/deploy/$APP_NAME"

step "PASS: init -> doctor -> check -> build -> deploy"
exit 0
