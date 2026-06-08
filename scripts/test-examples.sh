#!/bin/bash
# Run all example handler tests.
# Exits with code 1 if any test suite fails.
#
# Usage: bash scripts/test-examples.sh

set -e

ZIG="${ZIG:-zig}"
ZIGTTP="${ZIGTTP:-zig-out/bin/zigttp}"
PASS=0
FAIL=0

# Build once up front, then invoke the produced binary directly. Going through
# `zig build run --` per suite would re-walk the build graph for every suite.
echo "Building runtime..."
$ZIG build
echo ""

run_tests() {
    local handler=$1
    local tests=$2
    local name
    name=$(echo "$handler" | sed 's|examples/||')

    local output
    output=$("$ZIGTTP" serve "$handler" --test "$tests" 2>&1) || true

    local results
    results=$(echo "$output" | grep "^Results:" || echo "Results: ? passed, ? failed, ? total")
    local passed
    passed=$(echo "$results" | grep -o '[0-9]* passed' | grep -o '[0-9]*')
    local failed
    failed=$(echo "$results" | grep -o '[0-9]* failed' | grep -o '[0-9]*')

    if [ "${failed:-1}" = "0" ]; then
        echo "  PASS  $name ($passed tests)"
        PASS=$((PASS + 1))
    else
        echo "  FAIL  $name ($failed failed)"
        echo "$output" | grep "FAIL" | head -5 | sed 's/^/        /'
        FAIL=$((FAIL + 1))
    fi
}

# Assert a handler type-checks clean (exit 0, no errors/warnings) under the
# analyzer. Used for examples whose value is the static proof, not runtime I/O.
check_types() {
    local handler=$1
    local name
    name=$(echo "$handler" | sed 's|examples/||')

    if "$ZIGTTP" check "$handler" --types >/dev/null 2>&1; then
        echo "  PASS  $name (check --types)"
        PASS=$((PASS + 1))
    else
        echo "  FAIL  $name (check --types)"
        "$ZIGTTP" check "$handler" --types 2>&1 | grep -iE "error|warning" | head -5 | sed 's/^/        /'
        FAIL=$((FAIL + 1))
    fi
}

echo "Example Handler Tests"
echo "====================="
echo ""

# handler/
run_tests "examples/handler/handler-full.tsx" "examples/handler/handler.test.jsonl"
run_tests "examples/handler/handler.ts"       "examples/handler/handler-ts.test.jsonl"
run_tests "examples/handler/handler.tsx"       "examples/handler/handler-tsx.test.jsonl"
run_tests "examples/handler/sugar.ts"          "examples/handler/sugar.test.jsonl"
run_tests "examples/handler/feature-probes.ts" "examples/handler/feature-probes.test.jsonl"

# jsx/
run_tests "examples/jsx/jsx-simple.tsx"    "examples/jsx/jsx-simple.test.jsonl"
run_tests "examples/jsx/jsx-component.tsx" "examples/jsx/jsx-component.test.jsonl"
run_tests "examples/jsx/jsx-ssr.tsx"       "examples/jsx/jsx-ssr.test.jsonl"

# modules/
run_tests "examples/modules/modules.ts"      "examples/modules/modules.test.jsonl"
run_tests "examples/modules/modules_all.ts"  "examples/modules/modules_all.test.jsonl"

# fetch/
run_tests "examples/fetch/weather-forecasts.ts" "examples/fetch/weather-forecasts.test.jsonl"
run_tests "examples/fetch/weather-app.ts"       "examples/fetch/weather-app.test.jsonl"
run_tests "examples/fetch/webhook.ts"           "examples/fetch/webhook.test.jsonl"

# routing/
run_tests "examples/routing/router.ts"         "examples/routing/router.test.jsonl"
run_tests "examples/routing/guard-compose.ts"  "examples/routing/guard-compose.test.jsonl"
run_tests "examples/routing/match-handler.ts"  "examples/routing/match-handler.test.jsonl"

# patterns/ - every pattern example must type-check clean; the
# request-dependent handlers additionally get behavioral suites.
for h in examples/patterns/*.ts; do check_types "$h"; done
run_tests "examples/patterns/validate-external.ts"         "examples/patterns/validate-external.test.jsonl"
run_tests "examples/patterns/discriminated-union-match.ts" "examples/patterns/discriminated-union-match.test.jsonl"
run_tests "examples/patterns/derive-types.ts"              "examples/patterns/derive-types.test.jsonl"

# `zigttp check` writes a zigttp.d.ts typings stub into the cwd; drop it.
rm -f zigttp.d.ts

echo ""
echo "====================="
echo "Suites: $((PASS + FAIL)) total, $PASS passed, $FAIL failed"

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
