#!/bin/bash
# Run the pull-request proof gate and stage its output for CI:
#   - proof-gate.md   the sticky PR comment
#   - $GITHUB_OUTPUT  verdict=safe|breaking|error (from the gate's exit code)
#
# Usage: scripts/proof-gate.sh <base-ref> <head-ref>
# Exit code mirrors `zigttp proofs gate`: 0 safe, 1 breaking, 2 error. The
# workflow runs this with continue-on-error and enforces the verdict in a
# separate step, so the comment is always posted first.
#
# For a machine-readable report, run `zigttp proofs gate --format json`.

set -uo pipefail

BASE="${1:-origin/main}"
HEAD="${2:-HEAD}"

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZIGTTP="${ZIGTTP:-$REPO_ROOT/zig-out/bin/zigttp}"

# Make the base ref available to `git show <base>:<path>` and the diff.
git fetch --no-tags origin "${BASE#origin/}" >/dev/null 2>&1 || true

# CI runners hold no persistent attest identity, so do not sign ledger rows.
"$ZIGTTP" proofs gate --base "$BASE" --head "$HEAD" --format md --out proof-gate.md --no-sign
GATE_EXIT=$?

case "$GATE_EXIT" in
    0) VERDICT=safe ;;
    1) VERDICT=breaking ;;
    *) VERDICT=error ;;
esac

if [ -n "${GITHUB_OUTPUT:-}" ]; then
    echo "verdict=$VERDICT" >> "$GITHUB_OUTPUT"
fi

exit "$GATE_EXIT"
