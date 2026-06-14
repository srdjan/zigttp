#!/usr/bin/env bash
# Handler verification diagnostics exercised through the current zigttp CLI.
# Run from the project root: bash tests/verify/run_tests.sh

set -u

ZIG="${ZIG:-zig}"
ZIGTTP="${ZIGTTP:-./zig-out/bin/zigttp}"
CHECK_TIMEOUT_SECONDS="${CHECK_TIMEOUT_SECONDS:-30}"
PASS=0
FAIL=0
TESTS=0

if [ ! -x "$ZIGTTP" ]; then
    "$ZIG" build >/dev/null
fi

run_check() {
    perl -e 'my $timeout = shift @ARGV; alarm $timeout; exec @ARGV' \
        "$CHECK_TIMEOUT_SECONDS" "$ZIGTTP" check "$1" 2>&1
}

check_pass() {
    local file=$1
    local desc=$2
    TESTS=$((TESTS + 1))
    local output
    output=$(run_check "$file")
    local status=$?
    if [ "$status" -eq 0 ]; then
        echo "  PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $desc (expected pass)"
        printf '%s\n' "$output" | sed 's/^/        /'
        FAIL=$((FAIL + 1))
    fi
}

check_fail() {
    local file=$1
    local desc=$2
    local expected_msg=$3
    TESTS=$((TESTS + 1))
    local output
    output=$(run_check "$file")
    local status=$?
    if [ "$status" -ne 0 ]; then
        if [ -n "$expected_msg" ]; then
            if printf '%s\n' "$output" | grep -Fq "$expected_msg"; then
                echo "  PASS: $desc"
                PASS=$((PASS + 1))
            else
                echo "  FAIL: $desc (missing expected message: $expected_msg)"
                printf '%s\n' "$output" | sed 's/^/        /'
                FAIL=$((FAIL + 1))
            fi
        else
            echo "  PASS: $desc"
            PASS=$((PASS + 1))
        fi
    else
        echo "  FAIL: $desc (expected failure)"
        printf '%s\n' "$output" | sed 's/^/        /'
        FAIL=$((FAIL + 1))
    fi
}

check_warn() {
    local file=$1
    local desc=$2
    local expected_msg=$3
    TESTS=$((TESTS + 1))
    local output
    output=$(run_check "$file")
    local status=$?
    if printf '%s\n' "$output" | grep -Fq "$expected_msg" &&
        printf '%s\n' "$output" | grep -Fq "0 errors"; then
        echo "  PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $desc (expected warning: $expected_msg)"
        printf '%s\n' "$output" | sed 's/^/        /'
        FAIL=$((FAIL + 1))
    fi
}

echo "Handler Verification Tests"
echo "=========================="
echo ""

echo "Check 1: Exhaustive Return Analysis"
check_pass "tests/verify/correct_handler.ts" "handler with trailing return passes"
check_pass "tests/verify/correct_if_else.ts" "if/else both returning passes"
check_pass "examples/handler/spec-guardrails.ts" "declared guardrails example passes"
check_pass "examples/handler/handler.ts" "TypeScript handler passes"
check_fail "tests/verify/missing_return_else.ts" "missing else detected" "not all code paths return"
check_fail "tests/verify/non_exhaustive_match.ts" "non-exhaustive match detected" "match expression must be exhaustive"
echo ""

echo "Check 2: Result Checking"
check_fail "tests/verify/unchecked_result.ts" "unchecked result.value detected" "result.value accessed without checking"
check_pass "tests/verify/checked_result.ts" "result.ok checked before .value"
echo ""

echo "Check 3: Unreachable Code"
check_warn "tests/verify/unreachable_code.ts" "unreachable code after return" "unreachable code"
echo ""

echo "Check 4: Dead Variables"
check_warn "tests/verify/unused_variable.ts" "unused variable detected" "declared variable is never used"
echo ""

echo "Check 6: Optional Value Checking"
check_fail "tests/verify/unchecked_optional.ts" "unchecked optional use detected" "optional value used without checking"
check_pass "tests/verify/optional_narrowed_truthiness.ts" "if (val) narrows optional"
check_pass "tests/verify/optional_narrowed_negated.ts" "if (!val) return narrows optional"
check_pass "tests/verify/optional_narrowed_undefined.ts" "if (val !== undefined) narrows optional"
check_pass "tests/verify/optional_nullish_coalesce.ts" "env() ?? default resolves optionality"
check_fail "tests/verify/optional_object_access.ts" "property access on optional detected" "property access on optional value"
echo ""

echo "=========================="
echo "Results: $PASS/$TESTS passed, $FAIL failed"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
