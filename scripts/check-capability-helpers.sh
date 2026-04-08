#!/bin/bash

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

echo "Capability Helper Audit"
echo "======================="

fail=0

check_pattern() {
    local description="$1"
    local pattern="$2"
    local matches

    matches="$(git ls-files -z 'zigts/modules/*.zig' | xargs -0 rg -n -H --color never "$pattern" || true)"
    if [ -n "$matches" ]; then
        echo ""
        echo "FAIL: $description"
        echo "$matches"
        fail=1
    fi
}

check_pattern "direct runtime policy access in virtual modules" 'capability_policy\.'
check_pattern "direct environment reads in virtual modules" 'std\.c\.getenv'
check_pattern "direct filesystem reads in virtual modules" 'file_io\.readFile'
check_pattern "direct SQLite opens in virtual modules" 'Db\.openReadWriteCreate'
check_pattern "direct clock access in virtual modules" 'compat\.realtimeNow(Ms|Ns)'
check_pattern "direct stderr writes in virtual modules" 'std\.c\.write\('
check_pattern "direct HMAC creation in virtual modules" 'HmacSha256\.create'
check_pattern "direct SHA-256 hashing in virtual modules" 'Sha256\.init'
check_pattern "direct callback/sql store state access in virtual modules" 'ctx\.getModuleState\((IoCallbacks|DurableCallbacks|SqlStore)'

if [ "$fail" -ne 0 ]; then
    echo ""
    echo "Use shared checked helpers in zigts/module_binding.zig instead of direct sensitive operations."
    exit 1
fi

echo "PASS: no direct sensitive operations found in zigts/modules"
