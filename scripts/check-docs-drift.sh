#!/usr/bin/env bash
set -euo pipefail

fail() {
  printf 'docs drift: %s\n' "$1" >&2
  exit 1
}

modules_doc="docs/virtual-modules/README.md"
registry_file="packages/zigts/src/builtin_modules.zig"

[[ -f "$modules_doc" ]] || fail "missing $modules_doc"
[[ -f "$registry_file" ]] || fail "missing $registry_file"

module_count=$(
  awk '
    /pub const builtin_governance_entries/ { in_entries = 1 }
    in_entries && /\.specifier = "zigttp:/ { count += 1 }
    in_entries && /^};/ { print count; exit }
  ' "$registry_file"
)

[[ -n "$module_count" ]] || fail "could not count builtin governance entries"

doc_count=$(
  awk '
    /^## Module Catalog/ { in_catalog = 1; next }
    /^## / && in_catalog { exit }
    in_catalog && /^\| `zigttp:/ { count += 1 }
    END { print count + 0 }
  ' "$modules_doc"
)

[[ "$doc_count" == "$module_count" ]] ||
  fail "$modules_doc lists $doc_count modules, registry has $module_count"

while IFS= read -r specifier; do
  if ! awk -v specifier="$specifier" '
    /^## Module Catalog/ { in_catalog = 1; next }
    /^## / && in_catalog { exit }
    in_catalog && index($0, "| `" specifier "` |") > 0 { found = 1 }
    END { exit(found ? 0 : 1) }
  ' "$modules_doc"; then
    fail "$modules_doc is missing $specifier"
  fi
done < <(
  awk '
    /pub const builtin_governance_entries/ { in_entries = 1 }
    in_entries && /\.specifier = "zigttp:/ {
      line = $0
      sub(/^.*\.specifier = "/, "", line)
      sub(/".*$/, "", line)
      print line
    }
    in_entries && /^};/ { exit }
  ' "$registry_file"
)

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
