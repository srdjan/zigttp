#!/usr/bin/env bash
# scripts/verify.sh
#
# One-command local gate that mirrors the `test` job in
# .github/workflows/ci.yml step-for-step, in order, one process per step.
#
# Sequential by design: the build graph may run the pool-heavy `test` and
# `test-zruntime` roots in parallel and reintroduce the macOS teardown TRAP
# that build.zig (see the comment above the test step) warns about. We invoke
# them as separate processes here rather than adding a `verify` build step.
#
# The format gate is a separate CI job (ci.yml: Check formatting); it is run
# LAST here so a local `bash scripts/verify.sh` still catches formatting drift
# without gating the test suite on it. Note: the repo is fmt-clean only under
# the pinned toolchain (0.16.0, matching CI's ZIG_VERSION); running under a
# nightly `zig` may report spurious drift, so keeping fmt last means the full
# correctness suite always runs first regardless.
#
# Usage (from anywhere; the script cd's to the repo root):
#   bash scripts/verify.sh

set -euo pipefail

cd "$(dirname "$0")/.."

step() {
  printf '\n========================================\n'
  printf '>> %s\n' "$1"
  printf '========================================\n'
}

step "zig build test  (aggregate unit suite)"
if [ "$(uname -s)" = "Darwin" ]; then
  zig build test -j1
else
  zig build test
fi

step "zig build test-zruntime  (standalone runtime root)"
zig build test-zruntime

step "zig build test-docs-drift test-doc-links  (docs drift and link gates)"
zig build test-docs-drift test-doc-links

step "zig build -Doptimize=ReleaseFast  (release binaries)"
zig build -Doptimize=ReleaseFast

step "zig build smoke-v1  (v1 user-flow smoke)"
zig build smoke-v1

step "zig build test-panic-isolation  (handler panic isolation E2E)"
zig build test-panic-isolation

step "zig build test-cli -Dstudio  (studio workbench unit tests)"
# studio.zig only compiles under -Dstudio, so its unit tests (incl. the
# TRADE_TABLE drift gate) run in no default target. Build the CLI test suite
# with studio enabled so they are actually exercised.
zig build test-cli -Dstudio

step "bash scripts/test-examples.sh  (example handler tests)"
bash scripts/test-examples.sh

step "bash scripts/test-install-archive-safety.sh  (installer archive path safety)"
bash scripts/test-install-archive-safety.sh

step "policy hash unchanged  (ci.yml: Assert policy hash unchanged)"
if [ ! -f policy-hash.txt ]; then
  echo "error: policy-hash.txt is missing - the policy hash baseline must be committed" >&2
  echo "Run: ./zig-out/bin/zts describe-rule --hash > policy-hash.txt" >&2
  exit 1
fi
EXPECTED=$(cat policy-hash.txt)
ACTUAL=$(./zig-out/bin/zts describe-rule --hash)
if [ "$ACTUAL" != "$EXPECTED" ]; then
  echo "error: policy hash mismatch - rules changed without updating policy-hash.txt" >&2
  echo "Expected: $EXPECTED" >&2
  echo "Actual:   $ACTUAL" >&2
  echo "Run: ./zig-out/bin/zts describe-rule --hash > policy-hash.txt" >&2
  exit 1
fi
echo "policy hash OK: $ACTUAL"

step "bash scripts/check-semantics-spec.sh  (strict semantics spec gate)"
bash scripts/check-semantics-spec.sh

step "verify expert subsystem  (ci.yml: Verify expert subsystem)"
if ! command -v jq >/dev/null 2>&1; then
  echo "error: jq is required for the expert-subsystem check (matches ci.yml)" >&2
  exit 1
fi
META=$(./zig-out/bin/zts meta --json)
echo "$META" | jq -e '.rule_count >= 25' >/dev/null
echo "$META" | jq -e '.policy_hash | length == 64' >/dev/null
echo "expert subsystem OK"

step "zig fmt --check build.zig packages/  (ci.yml: Check formatting)"
zig fmt --check build.zig packages/

printf '\n========================================\n'
printf '>> verify.sh: all CI test-job steps passed\n'
printf '========================================\n'
