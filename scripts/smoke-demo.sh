#!/bin/bash
# End-to-end smoke for the first-run Proof Theater:
#   demo -> baseline state -> introduce bug -> witness -> repair -> deploy receipt.

set -euo pipefail

ZIG="${ZIG:-zig}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZIGTTP="${ZIGTTP:-$REPO_ROOT/zig-out/bin/zigttp}"

PORT=$((RANDOM % 1000 + 6200))
TMP_DIR=$(mktemp -d -t zigttp-demo-smoke-XXXXXX)
APP_DIR="$TMP_DIR/proof-demo"
LOG_FILE="$TMP_DIR/demo.log"
PID=""

cleanup() {
    if [ -n "$PID" ]; then
        kill "$PID" 2>/dev/null || true
        wait "$PID" 2>/dev/null || true
    fi
    pkill -f "zigttp.*--port $PORT" 2>/dev/null || true
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

fail() {
    printf '\n[smoke-demo] FAIL: %s\n' "$*" >&2
    [ -f "$LOG_FILE" ] && cat "$LOG_FILE" >&2
    exit 1
}

wait_step() {
    local step="$1" body=""
    for _ in $(seq 1 120); do
        if body=$(/usr/bin/curl -sf "http://127.0.0.1:$PORT/_zigttp/studio/demo/state.json" 2>/dev/null); then
            if printf '%s' "$body" | grep -q "\"step\":\"$step\""; then
                printf '%s' "$body"
                return 0
            fi
        fi
        sleep 0.25
        if ! kill -0 "$PID" 2>/dev/null; then
            return 1
        fi
    done
    printf '%s' "$body" >&2
    return 1
}

post_action() {
    local action="$1"
    /usr/bin/curl -sf \
        -X POST \
        -H 'content-type: application/json' \
        --data "{\"action\":\"$action\"}" \
        "http://127.0.0.1:$PORT/_zigttp/studio/demo/action" >/dev/null
}

(cd "$REPO_ROOT" && "$ZIG" build) || fail "zig build"
[ -x "$ZIGTTP" ] || fail "missing $ZIGTTP"

"$ZIGTTP" demo --no-open --port "$PORT" --out "$APP_DIR" >"$LOG_FILE" 2>&1 &
PID=$!

wait_step baseline >/dev/null || fail "baseline state not reached"

post_action introduce_bug || fail "introduce_bug action failed"
witness_body=$(wait_step witness) || fail "witness state not reached"
printf '%s' "$witness_body" | grep -q 'SECRET_KEY' || fail "witness state did not mention SECRET_KEY"

post_action repair_bug || fail "repair_bug action failed"
wait_step repaired >/dev/null || fail "repaired state not reached"

post_action deploy || fail "deploy action failed"
deployed_body=$(wait_step deployed) || fail "deployed state not reached"
printf '%s' "$deployed_body" | grep -q 'proofs.jsonl' || fail "deployed state did not include ledger receipt"
[ -s "$APP_DIR/.zigttp/proofs.jsonl" ] || fail "proof ledger row missing"
[ -x "$APP_DIR/.zigttp/deploy/proof-demo" ] || fail "local deploy artifact missing"

printf '[smoke-demo] ok on port %s\n' "$PORT"
