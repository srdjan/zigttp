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

doc_cell_for() {
  local specifier="$1"
  local column="$2"
  awk -F '|' -v specifier="$specifier" -v column="$column" '
    /^## Module Catalog/ { in_catalog = 1; next }
    /^## / && in_catalog { exit }
    in_catalog && index($2, "`" specifier "`") > 0 {
      cell = $column
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", cell)
      print cell
      found = 1
      exit
    }
    END { exit(found ? 0 : 1) }
  ' "$modules_doc"
}

canonical_csv() {
  tr ',' '\n' |
    sed 's/`//g; s/^[[:space:]]*//; s/[[:space:]]*$//' |
    sed '/^$/d; /^none$/d' |
    LC_ALL=C sort |
    paste -sd ',' -
}

spec_field() {
  local spec_file="$1"
  local field="$2"
  awk -v field="$field" '
    index($0, "\"" field "\"") > 0 {
      line = $0
      sub(/^.*:[[:space:]]*"/, "", line)
      sub(/".*$/, "", line)
      print line
      exit
    }
  ' "$spec_file"
}

spec_array_values() {
  local spec_file="$1"
  local field="$2"
  awk -v field="$field" '
    function update_depth(line, tmp) {
      tmp = line
      depth += gsub(/\[/, "", tmp)
      tmp = line
      depth -= gsub(/\]/, "", tmp)
    }
    {
      if (!in_array) {
        if (index($0, "\"" field "\"") == 0) next
        in_array = 1
        update_depth($0)
        line = $0
        sub(/^.*\[/, "", line)
      } else {
        line = $0
        update_depth($0)
      }

      while (match(line, /"[^"]+"/)) {
        value = substr(line, RSTART + 1, RLENGTH - 2)
        if (value != field) print value
        line = substr(line, RSTART + RLENGTH)
      }

      if (depth <= 0) exit
    }
  ' "$spec_file"
}

spec_export_names() {
  local spec_file="$1"
  awk '
    function update_depth(line, tmp) {
      tmp = line
      depth += gsub(/\[/, "", tmp)
      tmp = line
      depth -= gsub(/\]/, "", tmp)
    }
    /"exports"[[:space:]]*:/ {
      in_exports = 1
      update_depth($0)
      next
    }
    in_exports {
      if (match($0, /"name"[[:space:]]*:[[:space:]]*"[^"]+"/)) {
        line = $0
        sub(/^.*"name"[[:space:]]*:[[:space:]]*"/, "", line)
        sub(/".*$/, "", line)
        print line
      }
      update_depth($0)
      if (depth <= 0) exit
    }
  ' "$spec_file"
}

compare_doc_list() {
  local specifier="$1"
  local label="$2"
  local expected="$3"
  local actual="$4"
  if [[ "$expected" != "$actual" ]]; then
    fail "$modules_doc has $label mismatch for $specifier (expected: ${expected:-none}; actual: ${actual:-none})"
  fi
}

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

spec_count=0
while IFS= read -r -d '' spec_file; do
  spec_count=$((spec_count + 1))
  specifier="$(spec_field "$spec_file" "specifier")"
  [[ -n "$specifier" ]] || fail "could not read specifier from $spec_file"

  doc_exports_cell="$(doc_cell_for "$specifier" 3)" ||
    fail "$modules_doc is missing $specifier"
  doc_caps_cell="$(doc_cell_for "$specifier" 4)" ||
    fail "$modules_doc is missing capabilities for $specifier"

  expected_exports="$(spec_export_names "$spec_file" | canonical_csv)"
  actual_exports="$(printf '%s\n' "$doc_exports_cell" | canonical_csv)"
  compare_doc_list "$specifier" "exports" "$expected_exports" "$actual_exports"

  expected_caps="$(spec_array_values "$spec_file" "requiredCapabilities" | canonical_csv)"
  actual_caps="$(printf '%s\n' "$doc_caps_cell" | canonical_csv)"
  compare_doc_list "$specifier" "capabilities" "$expected_caps" "$actual_caps"
done < <(git ls-files -z 'packages/modules/module-specs/*.json')

[[ "$spec_count" == "$module_count" ]] ||
  fail "module specs have $spec_count modules, registry has $module_count"

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
