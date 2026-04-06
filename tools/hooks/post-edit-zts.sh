#!/usr/bin/env bash
set -euo pipefail

# PostToolUse hook: advisory analysis after Edit/Write on handler files.
# Runs zigts edit-simulate on the written file and reports new violations.
# Non-gating: always exits 0.

payload="$(cat)"

file_path="$(printf '%s' "$payload" | jq -r '.tool_input.file_path // empty')"

# Skip non-handler files
[ -z "$file_path" ] && exit 0
case "$file_path" in
  *.ts|*.tsx|*.js|*.jsx) ;;
  *) exit 0 ;;
esac

# Skip if file does not exist
[ -f "$file_path" ] || exit 0

# Find zigts binary
if command -v zigts &>/dev/null; then
  ZIGTS="zigts"
elif [ -x "./zig-out/bin/zigts" ]; then
  ZIGTS="./zig-out/bin/zigts"
else
  exit 0
fi

# Run edit-simulate (advisory)
result="$(timeout 30 "$ZIGTS" edit-simulate "$file_path" 2>/dev/null)" || exit 0

# Check for violations
total="$(printf '%s' "$result" | jq -r '.summary.total // 0' 2>/dev/null)" || total=0

if [ "$total" -gt 0 ]; then
  new_count="$(printf '%s' "$result" | jq -r '.summary.new // 0' 2>/dev/null)" || new_count=0

  message="$(printf '%s' "$result" | jq -r '
    "Post-edit validation (advisory): " +
    (.summary.total|tostring) + " violation(s), " +
    (.summary.new|tostring) + " new. " +
    ([.violations[:5][] | .code + " line " + (.line|tostring) + ": " + .message] | join("; "))
  ' 2>/dev/null)" || message="Post-edit: $total violation(s) found"

  jq -n \
    --arg msg "$message" \
    '{
      hookSpecificOutput: {
        hookEventName: "PostToolUse",
        additionalContext: $msg
      }
    }'
fi

exit 0
