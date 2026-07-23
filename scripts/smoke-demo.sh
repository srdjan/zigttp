#!/bin/bash
# Noninteractive smoke for the release Proof Passport demo:
#   scripted demo -> witness -> repair -> deploy -> offline passport export.

set -euo pipefail

ZIG="${ZIG:-zig}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZTTP="${ZTTP:-$REPO_ROOT/zig-out/bin/zttp}"

TMP_DIR=$(mktemp -d -t zttp-demo-smoke-XXXXXX)
export ZTTP_SESSIONS_DIR="$TMP_DIR/sessions"
APP_DIR="proof-demo"
PASSPORT_DIR="proof-demo/passport"
LOG_FILE="$TMP_DIR/demo.log"

cleanup() {
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

fail() {
    printf '\n[smoke-demo] FAIL: %s\n' "$*" >&2
    [ -f "$LOG_FILE" ] && cat "$LOG_FILE" >&2
    exit 1
}

(cd "$REPO_ROOT" && "$ZIG" build) || fail "zig build"
[ -x "$ZTTP" ] || fail "missing $ZTTP"

(cd "$TMP_DIR" && "$ZTTP" demo --scripted --out "$APP_DIR" --export "$PASSPORT_DIR" >"$LOG_FILE" 2>&1) || fail "scripted demo failed"

APP_ABS="$TMP_DIR/$APP_DIR"
PASSPORT_ABS="$TMP_DIR/$PASSPORT_DIR"

[ -f "$PASSPORT_ABS/passport.json" ] || fail "passport.json missing"
[ -f "$PASSPORT_ABS/events.jsonl" ] || fail "events.jsonl missing"
[ -f "$PASSPORT_ABS/session-meta.json" ] || fail "session-meta.json missing"
[ -f "$PASSPORT_ABS/index.html" ] || fail "index.html missing"
[ -f "$PASSPORT_ABS/verify.txt" ] || fail "verify.txt missing"

grep -q '"kind":"zttp-proof-passport"' "$PASSPORT_ABS/passport.json" || fail "passport kind missing"
grep -q '"step":"deployed"' "$PASSPORT_ABS/passport.json" || fail "passport final step is not deployed"
grep -q '"contractHash":"' "$PASSPORT_ABS/passport.json" || fail "passport missing contract hash"
grep -q '"policyHash":"' "$PASSPORT_ABS/passport.json" || fail "passport missing policy hash"
grep -q '"kind":"verified_patch"' "$PASSPORT_ABS/events.jsonl" || fail "events missing verified patch"
grep -q 'zttp proofs show HEAD' "$PASSPORT_ABS/verify.txt" || fail "verify commands missing proof ledger check"
grep -q 'zttp Proof Passport' "$PASSPORT_ABS/index.html" || fail "html export missing title"

[ -s "$APP_ABS/.zttp/proofs.jsonl" ] || fail "proof ledger row missing"
[ -x "$APP_ABS/.zttp/deploy/proof-demo" ] || fail "local deploy artifact missing"

printf '[smoke-demo] ok: passport exported to %s\n' "$PASSPORT_ABS"
