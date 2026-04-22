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
# `zig build run --` per suite would re-walk the build graph 14 times per CI run.
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

echo "Example Handler Tests"
echo "====================="
echo ""

# handler/
run_tests "examples/handler/handler-full.tsx" "examples/handler/handler.test.jsonl"
run_tests "examples/handler/handler.ts"       "examples/handler/handler-ts.test.jsonl"
run_tests "examples/handler/handler.tsx"       "examples/handler/handler-tsx.test.jsonl"
run_tests "examples/handler/sugar.ts"          "examples/handler/sugar.test.jsonl"

# jsx/
run_tests "examples/jsx/jsx-simple.tsx"    "examples/jsx/jsx-simple.test.jsonl"
run_tests "examples/jsx/jsx-component.tsx" "examples/jsx/jsx-component.test.jsonl"
run_tests "examples/jsx/jsx-ssr.tsx"       "examples/jsx/jsx-ssr.test.jsonl"

# modules/
run_tests "examples/modules/modules.ts"      "examples/modules/modules.test.jsonl"
run_tests "examples/modules/modules_all.ts"  "examples/modules/modules_all.test.jsonl"

# routing/
run_tests "examples/routing/router.ts"         "examples/routing/router.test.jsonl"
run_tests "examples/routing/guard-compose.ts"  "examples/routing/guard-compose.test.jsonl"
run_tests "examples/routing/match-handler.ts"  "examples/routing/match-handler.test.jsonl"

# htmx-todo/
run_tests "examples/htmx-todo/handlers.tsx"    "examples/htmx-todo/handlers.test.jsonl"

# shopping-cart/
run_tests "examples/shopping-cart/shopping-cart.tsx" "examples/shopping-cart/shopping-cart.test.jsonl"

# url-shortener/ — re-enabled after value.zig toConditionBool fix. The prior
# "fixture drift" note was misdiagnosed: the handler uses `if (pub)` on the
# result of `routerMatch`, and the old truthiness path rejected objects at
# runtime, producing silent empty 200s that looked like fixture drift.
run_tests "examples/url-shortener/shortener.ts" "examples/url-shortener/shortener.test.jsonl"

echo ""
echo "====================="
echo "Suites: $((PASS + FAIL)) total, $PASS passed, $FAIL failed"

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
