#!/usr/bin/env bash
# Assert the deployed runtime carries no expert-agent / model-provider surface.
#
# The pi package (the `zigttp expert` agent) embeds the Anthropic/OpenAI HTTP
# client host strings. Those markers must appear ONLY in the developer `zigttp`
# binary - never in the deployable `zigttp-runtime` template, and never in the
# pi-free `zigts` analyzer binary. This turns "pi is not in the runtime" from an
# incidental build property into an enforced invariant: a future edit that wires
# pi_app into runtime_main (or back into zigts) fails this check.
#
# Usage: check-runtime-purity.sh <dev-zigttp-bin> <runtime-bin> <zigts-bin>
#
# `grep -acF` scans the binary directly (no `strings`/binutils dependency, so
# this runs on minimal CI images) and reads the whole file. `|| true` absorbs
# grep's exit 1 on zero matches under `set -e`.
set -eu

dev_bin="$1"
runtime_bin="$2"
zigts_bin="$3"

# Markers that exist only in pi's provider HTTP clients.
markers=("api.anthropic.com" "api.openai.com" "anthropic-version")

contains() {
  grep -acF "$2" "$1" || true
}

fail=0

# 1. The shipped/analyzer binaries must be clean.
for bin in "$runtime_bin" "$zigts_bin"; do
  for m in "${markers[@]}"; do
    if [ "$(contains "$bin" "$m")" -gt 0 ]; then
      echo "error: '$(basename "$bin")' contains agent/provider marker '$m' - pi linked where it must not be" >&2
      fail=1
    fi
  done
done

# 2. Sanity: the dev binary MUST contain at least one marker, else the check is
#    vacuous (e.g. every provider host was renamed and the markers went stale).
#    Requiring only one keeps the guard meaningful if a provider is dropped.
present=0
for m in "${markers[@]}"; do
  if [ "$(contains "$dev_bin" "$m")" -gt 0 ]; then
    present=$((present + 1))
  fi
done
if [ "$present" -eq 0 ]; then
  echo "error: no agent/provider markers found in dev binary '$(basename "$dev_bin")' - purity check is vacuous; update markers" >&2
  fail=1
fi

if [ "$fail" -eq 0 ]; then
  echo "runtime purity OK: no agent/provider surface in $(basename "$runtime_bin") or $(basename "$zigts_bin")"
fi
exit "$fail"
