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
# The format gate is a separate CI job; run it alongside this script:
#   zig fmt --check build.zig packages/
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
zig build test

step "zig build test-zruntime  (standalone runtime root)"
zig build test-zruntime

step "zig build test-doc-links  (docs link gate)"
zig build test-doc-links

step "zig build -Doptimize=ReleaseFast  (release binaries)"
zig build -Doptimize=ReleaseFast

step "zig build smoke-v1  (v1 user-flow smoke)"
zig build smoke-v1

step "zig build test-panic-isolation  (handler panic isolation E2E)"
zig build test-panic-isolation

step "bash scripts/test-examples.sh  (example handler tests)"
bash scripts/test-examples.sh

step "bash scripts/test-install-archive-safety.sh  (installer archive path safety)"
bash scripts/test-install-archive-safety.sh

step "policy hash unchanged  (ci.yml: Assert policy hash unchanged)"
if [ ! -f policy-hash.txt ]; then
  echo "error: policy-hash.txt is missing - the policy hash baseline must be committed" >&2
  echo "Run: ./zig-out/bin/zigts describe-rule --hash > policy-hash.txt" >&2
  exit 1
fi
EXPECTED=$(cat policy-hash.txt)
ACTUAL=$(./zig-out/bin/zigts describe-rule --hash)
if [ "$ACTUAL" != "$EXPECTED" ]; then
  echo "error: policy hash mismatch - rules changed without updating policy-hash.txt" >&2
  echo "Expected: $EXPECTED" >&2
  echo "Actual:   $ACTUAL" >&2
  echo "Run: ./zig-out/bin/zigts describe-rule --hash > policy-hash.txt" >&2
  exit 1
fi
echo "policy hash OK: $ACTUAL"

step "semantics spec gate  (spec-check all mechanisms + spec.ts drift)"
# spec-check exits non-zero on any divergence, including the differential corpus
# (mechanism 4) running the registry against the real compiler and the SMT
# equivalence check (mechanism 5). Mechanism 5 needs the z3 binary; where it is
# absent the SMT obligations are skipped and the structural mechanisms still gate
# (so CI without z3 stays green). --audit additionally runs the faithful-model
# exclusion audit (opt-in because its f64-associativity refutation is slow), so
# CI machine-checks the soundness boundary even though interactive spec-check
# stays fast. Keep the summary visible so SMT/audit availability and skipped or
# inconclusive obligations are not hidden in local or CI logs.
./zig-out/bin/zigts spec-check --audit
# The exclusion audit only checks the soundness boundary when z3 actually ran
# EVERY refutation. Plain `spec-check --audit` is deliberately lenient (a z3-absent
# skip or an inconclusive timeout is non-fatal, so a future genuinely un-refutable
# law cannot wedge the command). This gate is stricter: re-read the machine-
# readable summary and REQUIRE a complete audit, so a silent skip or timeout
# cannot pass here with false confidence. An explicit ZIGTTP_Z3 opt-out is honored.
AUDIT_JSON=$(./zig-out/bin/zigts spec-check --audit --json || true)
z3_explicitly_disabled() {
  [ "${ZIGTTP_Z3+set}" = set ] || return 1
  case "$(printf '%s' "$ZIGTTP_Z3" | tr '[:upper:]' '[:lower:]')" in
    '' | off | none | 0 | disable | disabled) return 0 ;;
    *) return 1 ;;
  esac
}
if printf '%s' "$AUDIT_JSON" | jq -e '.audit.available == false' >/dev/null 2>&1; then
  if z3_explicitly_disabled; then
    echo "NOTE: ZIGTTP_Z3 disables z3 -> semantics exclusion audit SKIPPED (soundness boundary unchecked, explicit opt-out)"
  else
    echo "error: z3 not found, so the semantics exclusion audit (the verify.sh soundness-boundary gate) did not run. Install z3 or set ZIGTTP_Z3=off to skip it explicitly." >&2
    exit 1
  fi
else
  printf '%s' "$AUDIT_JSON" | jq -e '.audit.refuted == .audit.total and .audit.inconclusive == 0' >/dev/null 2>&1 ||
    {
      echo "error: semantics exclusion audit incomplete (not all excluded laws refuted, or an inconclusive timeout): the soundness boundary was not fully machine-checked." >&2
      exit 1
    }
fi
# The committed readable spec must match the registry it is generated from.
./zig-out/bin/zigts spec-render --check docs/spec/semantics.spec.ts
echo "semantics spec gate OK"

step "verify expert subsystem  (ci.yml: Verify expert subsystem)"
if ! command -v jq >/dev/null 2>&1; then
  echo "error: jq is required for the expert-subsystem check (matches ci.yml)" >&2
  exit 1
fi
META=$(./zig-out/bin/zigts meta --json)
echo "$META" | jq -e '.rule_count >= 25' >/dev/null
echo "$META" | jq -e '.policy_hash | length == 64' >/dev/null
echo "expert subsystem OK"

printf '\n========================================\n'
printf '>> verify.sh: all CI test-job steps passed\n'
printf '========================================\n'
