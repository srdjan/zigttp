#!/bin/bash
# Compile-time handler verification tests
# Run from the project root: bash tests/verify/run_tests.sh

set -e

ZIG="${ZIG:-zig}"
PASS=0
FAIL=0
TESTS=0

check_pass() {
    local file=$1
    local desc=$2
    TESTS=$((TESTS + 1))
    if $ZIG build -Dhandler="$file" -Dverify 2>&1 | grep -q "Verification passed"; then
        echo "  PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $desc (expected pass)"
        FAIL=$((FAIL + 1))
    fi
}

check_fail() {
    local file=$1
    local desc=$2
    local expected_msg=$3
    TESTS=$((TESTS + 1))
    local output
    output=$($ZIG build -Dhandler="$file" -Dverify 2>&1 || true)
    if echo "$output" | grep -q "Verification failed"; then
        if [ -n "$expected_msg" ]; then
            if echo "$output" | grep -q "$expected_msg"; then
                echo "  PASS: $desc"
                PASS=$((PASS + 1))
            else
                echo "  FAIL: $desc (missing expected message: $expected_msg)"
                FAIL=$((FAIL + 1))
            fi
        else
            echo "  PASS: $desc"
            PASS=$((PASS + 1))
        fi
    else
        echo "  FAIL: $desc (expected failure)"
        FAIL=$((FAIL + 1))
    fi
}

check_warn() {
    local file=$1
    local desc=$2
    local expected_msg=$3
    TESTS=$((TESTS + 1))
    local output
    output=$($ZIG build -Dhandler="$file" -Dverify 2>&1 || true)
    if echo "$output" | grep -q "Verification passed" && echo "$output" | grep -q "$expected_msg"; then
        echo "  PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $desc (expected warning: $expected_msg)"
        FAIL=$((FAIL + 1))
    fi
}

echo "Handler Verification Tests"
echo "=========================="
echo ""

echo "Check 1: Exhaustive Return Analysis"
check_pass "tests/verify/correct_handler.js" "handler with trailing return passes"
check_pass "tests/verify/correct_if_else.js" "if/else both returning passes"
check_pass "examples/handler.jsx" "real-world JSX handler passes"
check_pass "examples/handler.ts" "TypeScript handler passes"
check_fail "tests/verify/missing_return_else.js" "missing else detected" "not all code paths return"
check_fail "tests/verify/missing_return_switch.js" "switch without default detected" "without default case"
echo ""

echo "Check 3: Unreachable Code"
check_warn "tests/verify/unreachable_code.js" "unreachable code after return" "unreachable code"
echo ""

echo "=========================="
echo "Results: $PASS/$TESTS passed, $FAIL failed"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
