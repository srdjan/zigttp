#!/usr/bin/env bash
# Prepare a clean autoloop session that the Witness Theater demo tape
# resumes into. Idempotent: wipes prior session state for this cwd,
# resets the demo handler to its leaking baseline, runs the autoloop
# until it has emitted at least one verified_patch with witness data,
# and removes any prior witness-regressions.jsonl so `m` always writes a
# fresh artifact during the recording.
#
# Run from the repo root:
#   ./scripts/witness-theater-demo-prep.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

DEMO_DIR="$REPO_ROOT/temp/witness-demo"
HANDLER_PATH="$DEMO_DIR/handler.ts"
REGRESSIONS_PATH="$DEMO_DIR/witness-regressions.jsonl"

mkdir -p "$DEMO_DIR"
cp -f "$REPO_ROOT/examples/autoloop/handler.ts" "$HANDLER_PATH"
rm -f "$REGRESSIONS_PATH"

# Drop any prior sessions for this cwd so `--resume` lands on the run we
# are about to produce. The session root is keyed by a sha256 of the
# real cwd path; matching on that hash keeps us from clobbering
# unrelated sessions.
SESSION_ROOT="${ZIGTTP_SESSION_ROOT:-$HOME/.zigttp/sessions}"
CWD_HASH="$(printf '%s' "$(pwd -P)" | shasum -a 256 | cut -d' ' -f1)"
SESSION_BUCKET="$SESSION_ROOT/$CWD_HASH"
if [[ -d "$SESSION_BUCKET" ]]; then
    rm -rf "$SESSION_BUCKET"
fi

echo "Building..."
zig build >/dev/null

echo "Running autoloop..."
./zig-out/bin/zigts expert \
    --handler "$HANDLER_PATH" \
    --goal no_secret_leakage \
    --max-iters 2 \
    >/tmp/witness-theater-prep.log 2>&1 || true

# Reset handler so the file the demo opens reads as the original leak.
# The autoloop mutated it during convergence; the persisted patch
# events remain intact.
cp -f "$REPO_ROOT/examples/autoloop/handler.ts" "$HANDLER_PATH"

# Confirm a session landed with the expected witness data so the tape
# does not record an empty pane.
SESSION_FILE="$(ls -t "$SESSION_BUCKET"/*/events.jsonl 2>/dev/null | head -n1 || true)"
if [[ -z "$SESSION_FILE" ]]; then
    echo "error: no session events file found under $SESSION_BUCKET" >&2
    echo "autoloop log:" >&2
    cat /tmp/witness-theater-prep.log >&2
    exit 1
fi

if ! grep -q '"witnesses_new":\[' "$SESSION_FILE"; then
    echo "error: session at $SESSION_FILE has no witness data" >&2
    exit 1
fi

echo "Demo session ready. Run 'vhs docs/witness-theater-demo.tape' to record."
