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

result="$(timeout 30 "$ZIGTS" expert verify-modules "$file_path" --json 2>/dev/null)" || exit 0
violation_count="$(printf '%s' "$result" | jq -r '.violations | length // 0' 2>/dev/null)" || violation_count=0

if [ "$violation_count" -gt 0 ]; then
  message="$(printf '%s' "$result" | jq -r '
    "Virtual module post-edit validation: " +
    (.violations|length|tostring) + " issue(s). " +
    ([.violations[:5][] | .code + " " + .severity + ": " + .message] | join("; "))
  ' 2>/dev/null)" || message="Virtual module post-edit validation found issues"

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
