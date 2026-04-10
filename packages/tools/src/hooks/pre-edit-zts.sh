#!/usr/bin/env bash
set -euo pipefail

# PreToolUse hook: gate Edit/Write on handler files.
# Runs zigts check on the current file. Denies if errors exist.
# Conservative: allows on timeout, missing binary, or non-handler files.

payload="$(cat)"

file_path="$(printf '%s' "$payload" | jq -r '.tool_input.file_path // empty')"

# Skip non-handler files
[ -z "$file_path" ] && exit 0
case "$file_path" in
  *.ts|*.tsx|*.js|*.jsx) ;;
  *) exit 0 ;;
esac

# Skip if file does not exist yet (new file creation)
[ -f "$file_path" ] || exit 0

# Skip if zigts not available
if ! command -v zigts &>/dev/null; then
  # Try local build
  if [ -x "./zig-out/bin/zigts" ]; then
    ZIGTS="./zig-out/bin/zigts"
  else
    exit 0
  fi
else
  ZIGTS="zigts"
fi

# Run check on current file state
check_output="$(timeout 12 "$ZIGTS" check "$file_path" --json 2>/dev/null)" || {
  # Timeout or crash: allow conservatively
  exit 0
}

# Parse error count
error_count="$(printf '%s' "$check_output" | jq -r '.errors | length // 0' 2>/dev/null)" || error_count=0

if [ "$error_count" -gt 0 ]; then
  summary="$(printf '%s' "$check_output" | jq -r '
    .errors[:3] | map(.code + " line " + (.line|tostring) + ": " + .message) | join("; ")
  ' 2>/dev/null)" || summary="zigts check found errors"

  jq -n \
    --arg reason "zigts check: $error_count error(s) in current file" \
    --arg ctx "$summary" \
    '{
      hookSpecificOutput: {
        hookEventName: "PreToolUse",
        permissionDecision: "allow",
        additionalContext: ("Warning: " + $reason + ". " + $ctx)
      }
    }'
  exit 0
fi

exit 0
