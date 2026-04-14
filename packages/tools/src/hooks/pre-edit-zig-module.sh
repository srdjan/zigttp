#!/usr/bin/env bash
set -euo pipefail

payload="$(cat)"
file_path="$(printf '%s' "$payload" | jq -r '.tool_input.file_path // empty')"

[ -z "$file_path" ] && exit 0
case "$file_path" in
  packages/zigts/src/modules/*.zig|packages/zigts/module-specs/*.json) ;;
  *) exit 0 ;;
esac

[ -f "$file_path" ] || exit 0

if command -v zigts &>/dev/null; then
  ZIGTS="zigts"
elif [ -x "./zig-out/bin/zigts" ]; then
  ZIGTS="./zig-out/bin/zigts"
else
  exit 0
fi

result="$(timeout 15 "$ZIGTS" expert verify-modules "$file_path" --json 2>/dev/null)" || exit 0

violation_count="$(printf '%s' "$result" | jq -r '.violations | length // 0' 2>/dev/null)" || violation_count=0

if [ "$violation_count" -gt 0 ]; then
  summary="$(printf '%s' "$result" | jq -r '
    .violations[:3] | map(.code + " " + .severity + ": " + .message) | join("; ")
  ' 2>/dev/null)" || summary="virtual module validation found issues"

  jq -n \
    --arg ctx "$summary" \
    '{
      hookSpecificOutput: {
        hookEventName: "PreToolUse",
        permissionDecision: "allow",
        additionalContext: ("Virtual module advisory: " + $ctx)
      }
    }'
fi

exit 0
