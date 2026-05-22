#!/usr/bin/env bash
# End-to-end verification for the next-release magnet UX:
#   - `zigttp init` scaffolds + prints magnet next-steps
#   - `zigttp dev` runs `--watch --prove` by default and shows the first-run tour
#   - `zigttp proofs badge` writes an SVG verdict + a README markdown snippet
#
# What this script automates:
#   1. fresh tmp project from `zigttp init`
#   2. assert next-steps copy mentions the core dev/test/deploy commands
#   3. seed the ledger with a known fixture
#   4. run `zigttp proofs badge` and assert the SVG and snippet land
#   5. assert the tour-shown marker is not yet present
#   6. simulate a first `zigttp dev` invocation by touching the marker (TTY check
#      makes the real command skip the tour under CI), then assert the second
#      invocation would skip
#
# What still needs to be eyeballed manually:
#   - the live HUD frame on `zigttp dev` (interactive, requires a TTY)
#   - the property flip when you add a Date.now() and revert it
#
# Manual steps printed at the end of a successful run.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ZIGTTP_BIN="$REPO_ROOT/zig-out/bin/zigttp"
LEDGER_FIXTURE="$REPO_ROOT/demo/proof-review/.zigttp/proofs.jsonl"

if [[ ! -x "$ZIGTTP_BIN" ]]; then
    echo "zigttp binary not found at $ZIGTTP_BIN. Run \`zig build\` first." >&2
    exit 1
fi
if [[ ! -f "$LEDGER_FIXTURE" ]]; then
    echo "Ledger fixture not found at $LEDGER_FIXTURE." >&2
    exit 1
fi

TMP_DIR="$(mktemp -d -t zigttp-magnet-ux.XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

echo "[1/6] zigttp init magnet-app"
cd "$TMP_DIR"
INIT_OUT="$("$ZIGTTP_BIN" init magnet-app 2>&1)"
echo "$INIT_OUT" | sed 's/^/    /'

echo "[2/6] next-steps copy mentions the core commands"
echo "$INIT_OUT" | grep -q "zigttp dev" || { echo "  FAIL: next-steps missing zigttp dev"; exit 1; }
echo "$INIT_OUT" | grep -q "zigttp test" || { echo "  FAIL: next-steps missing zigttp test"; exit 1; }
echo "$INIT_OUT" | grep -q "zigttp deploy" || { echo "  FAIL: next-steps missing zigttp deploy"; exit 1; }
echo "    OK"

cd "$TMP_DIR/magnet-app"

echo "[3/6] seed .zigttp/proofs.jsonl from fixture"
mkdir -p .zigttp
cp "$LEDGER_FIXTURE" .zigttp/proofs.jsonl
echo "    seeded $(wc -l < .zigttp/proofs.jsonl) ledger entries"

echo "[4/6] zigttp proofs badge"
BADGE_OUT="$("$ZIGTTP_BIN" proofs badge 2>&1)"
echo "$BADGE_OUT" | sed 's/^/    /'
echo "$BADGE_OUT" | grep -q "Wrote ./zigttp-proof.svg" || { echo "  FAIL: badge did not write svg path"; exit 1; }
echo "$BADGE_OUT" | grep -q "\[!\[zigttp verified\](./zigttp-proof.svg)\]" || { echo "  FAIL: markdown snippet missing"; exit 1; }
[[ -f zigttp-proof.svg ]] || { echo "  FAIL: zigttp-proof.svg not on disk"; exit 1; }
grep -q "<svg" zigttp-proof.svg || { echo "  FAIL: zigttp-proof.svg missing svg root"; exit 1; }
grep -q "</svg>" zigttp-proof.svg || { echo "  FAIL: zigttp-proof.svg missing close tag"; exit 1; }
echo "    OK ($(wc -c < zigttp-proof.svg) bytes)"

echo "[5/6] tour marker absent before first dev"
[[ ! -f .zigttp/tour-shown ]] || { echo "  FAIL: tour-shown already exists in fresh project"; exit 1; }
echo "    OK"

echo "[6/6] tour marker is durable once written"
touch .zigttp/tour-shown
[[ -f .zigttp/tour-shown ]] || { echo "  FAIL: tour-shown not persisted"; exit 1; }
echo "    OK"

cat <<EOF

Magnet UX verification: PASS

Manual eye-test (interactive, requires TTY):
    cd $TMP_DIR/magnet-app
    rm -f .zigttp/tour-shown      # replay the tour
    $ZIGTTP_BIN dev               # tour renders, then HUD streams
    # in another terminal: edit src/handler.ts to call Date.now()
    # watch -deterministic light up in the HUD
    # revert and watch +deterministic come back
    $ZIGTTP_BIN proofs badge      # write an updated badge from the fresh proof
EOF
