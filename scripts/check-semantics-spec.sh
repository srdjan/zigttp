#!/usr/bin/env bash
# Strict semantics/spec gate for release readiness.

set -euo pipefail

cd "$(dirname "$0")/.."

if ! command -v jq >/dev/null 2>&1; then
  echo "error: jq is required for the semantics spec gate" >&2
  exit 1
fi

if [ ! -x ./zig-out/bin/zts ]; then
  echo "error: ./zig-out/bin/zts is missing; run 'zig build -Doptimize=ReleaseFast' first" >&2
  exit 1
fi

# spec-check exits non-zero on any structural, differential, or SMT divergence.
# --audit additionally runs the faithful-model exclusion audit so the release
# gate machine-checks the current soundness boundary.
./zig-out/bin/zts spec-check --audit

AUDIT_JSON=$(./zig-out/bin/zts spec-check --audit --json || true)

z3_explicitly_disabled() {
  [ "${ZTTP_Z3+set}" = set ] || return 1
  case "$(printf '%s' "$ZTTP_Z3" | tr '[:upper:]' '[:lower:]')" in
    '' | off | none | 0 | disable | disabled) return 0 ;;
    *) return 1 ;;
  esac
}

if printf '%s' "$AUDIT_JSON" | jq -e '.audit.available == false' >/dev/null 2>&1; then
  if z3_explicitly_disabled; then
    echo "NOTE: ZTTP_Z3 disables z3 -> semantics exclusion audit SKIPPED (soundness boundary unchecked, explicit opt-out)"
  else
    echo "error: z3 not found, so the semantics exclusion audit did not run. Install z3 or set ZTTP_Z3=off to skip it explicitly." >&2
    exit 1
  fi
else
  printf '%s' "$AUDIT_JSON" | jq -e '.audit.refuted == .audit.total and .audit.inconclusive == 0' >/dev/null 2>&1 ||
    {
      echo "error: semantics exclusion audit incomplete (not all excluded laws refuted, or an inconclusive timeout)." >&2
      exit 1
    }
fi

if printf '%s' "$AUDIT_JSON" | jq -e '.smt.available == false' >/dev/null 2>&1; then
  if z3_explicitly_disabled; then
    echo "NOTE: ZTTP_Z3 disables z3 -> SMT equivalence check SKIPPED (soundness boundary unchecked, explicit opt-out)"
  else
    echo "error: z3 not found, so the SMT equivalence check did not run. Install z3 or set ZTTP_Z3=off to skip it explicitly." >&2
    exit 1
  fi
else
  printf '%s' "$AUDIT_JSON" | jq -e '.smt.available == true and .smt.proved == .smt.total and .smt.unproven == 0' >/dev/null 2>&1 ||
    {
      echo "error: SMT equivalence check incomplete or unavailable." >&2
      exit 1
    }
fi

./zig-out/bin/zts spec-render --check docs/spec/semantics.spec.ts
echo "semantics spec gate OK"
