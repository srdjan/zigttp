#!/usr/bin/env bash
# scripts/update-expert-goldens.sh
#
# Regenerate the byte-exact golden fixtures that `zig build test-expert-golden`
# checks the `zigts` direct-command JSON/text contract against. Run this ONLY
# after a deliberate contract change (see docs/zigts-expert-contract.md); it
# overwrites the fixtures in place with the current binary's output, so review
# the diff before committing.
#
# Each command here mirrors an addExpertGolden/addExpertMetaGolden entry in
# build.zig. The exit-code-only checks (addExpertExitCheck) do not pin stdout
# and need no fixture, so they are not regenerated here.
#
# Usage (from anywhere; the script cd's to the repo root):
#   bash scripts/update-expert-goldens.sh

set -euo pipefail

cd "$(dirname "$0")/.."

FIXTURES="packages/tools/tests/fixtures/expert"
ZIGTS="./zig-out/bin/zigts"

echo ">> building zigts"
zig build

# Redirect stdout into each fixture. The verify-paths *missing* commands exit 1
# by contract, so guard those with `|| true` - we want their stdout regardless.
echo ">> regenerating fixtures under $FIXTURES"

"$ZIGTS" meta --json                                        > "$FIXTURES/meta.golden.json"
"$ZIGTS" verify-paths "$FIXTURES/clean_handler.ts" --json   > "$FIXTURES/verify_paths_clean.golden.json"
"$ZIGTS" verify-paths "$FIXTURES/missing.ts" --json         > "$FIXTURES/verify_paths_missing.golden.json" || true
"$ZIGTS" describe-rule ZTS303 --json                        > "$FIXTURES/describe_rule_ZTS303.golden.json"
"$ZIGTS" canonicalize "$FIXTURES/canonicalize_mixed.ts" --json \
                                                            > "$FIXTURES/canonicalize_mixed.golden.json"
"$ZIGTS" canonicalize "$FIXTURES/canonicalize_mixed.ts" --json --simulate \
                                                            > "$FIXTURES/canonicalize_mixed_simulate.golden.json"
"$ZIGTS" verify-paths "$FIXTURES/clean_handler.ts"          > "$FIXTURES/verify_paths_clean_text.golden.txt"
"$ZIGTS" verify-paths "$FIXTURES/missing.ts"                > "$FIXTURES/verify_paths_missing_text.golden.txt" || true

echo ">> done. Review the diff, then run: zig build test-expert-golden"
