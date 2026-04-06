#!/usr/bin/env bash
set -euo pipefail

# SessionStart hook: export policy metadata as environment variables.
# Requires CLAUDE_ENV_FILE to be set by Claude Code.

[ -z "${CLAUDE_ENV_FILE:-}" ] && exit 0

if command -v zigts &>/dev/null; then
  ZIGTS="zigts"
elif [ -x "./zig-out/bin/zigts" ]; then
  ZIGTS="./zig-out/bin/zigts"
else
  exit 0
fi

meta="$("$ZIGTS" expert meta --json 2>/dev/null)" || exit 0

printf '%s' "$meta" | jq -r '"ZIGTS_POLICY_VERSION=" + .policy_version' >> "$CLAUDE_ENV_FILE"
printf '%s' "$meta" | jq -r '"ZIGTS_POLICY_HASH=" + .policy_hash' >> "$CLAUDE_ENV_FILE"
