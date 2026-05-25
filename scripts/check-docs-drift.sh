#!/usr/bin/env bash
set -euo pipefail

fail() {
  printf 'docs drift: %s\n' "$1" >&2
  exit 1
}

scope_file="docs/releases/v0.1.0-scope.md"
registry_file="packages/zigts/src/builtin_modules.zig"

[[ -f "$scope_file" ]] || fail "missing $scope_file"
[[ -f "$registry_file" ]] || fail "missing $registry_file"

module_count=$(
  awk '
    /pub const builtin_governance_entries/ { in_entries = 1 }
    in_entries && /\.specifier = "zigttp:/ { count += 1 }
    in_entries && /^};/ { print count; exit }
  ' "$registry_file"
)

[[ -n "$module_count" ]] || fail "could not count builtin governance entries"

scope_count=$(
  sed -n 's/^\*\*Virtual modules\.\*\* \([0-9][0-9]*\) modules .*/\1/p' "$scope_file"
)

[[ "$scope_count" == "$module_count" ]] ||
  fail "$scope_file says $scope_count modules, registry has $module_count"

if grep -R "packages/runtime/src/generated/embedded_handler.zig" docs README.md CHANGELOG.md SECURITY.md RELEASE_CHECKLIST.md >/dev/null; then
  fail "docs reference obsolete packages/runtime/src/generated/embedded_handler.zig"
fi

if grep -R "src/generated/embedded_handler.zig" docs README.md CHANGELOG.md SECURITY.md RELEASE_CHECKLIST.md >/dev/null; then
  fail "docs reference obsolete src/generated/embedded_handler.zig"
fi

if grep -R "docs/capabilities.md" docs README.md CHANGELOG.md SECURITY.md RELEASE_CHECKLIST.md >/dev/null; then
  fail "docs reference obsolete docs/capabilities.md"
fi

if grep -n "std.net" docs/internals/architecture.md >/dev/null; then
  fail "architecture docs still describe the HTTP server as std.net"
fi

if grep -n "zero external dependencies" docs/internals/architecture.md >/dev/null; then
  fail "architecture docs still claim zero external dependencies"
fi

if grep -n "threaded and evented I/O paths" docs/performance.md >/dev/null; then
  fail "performance docs still claim evented request path support"
fi

printf 'docs drift: OK (%s builtin virtual modules)\n' "$module_count"
