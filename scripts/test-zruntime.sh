#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

mkdir -p zig-out
zig test --dep zts -Mroot=src/zruntime.zig -Mzts=zts/root.zig "$@"

# zig test creates libroot.a and libroot.a.o in CWD - move to zig-out/
for f in libroot.a libroot.a.o; do
    [ -f "$f" ] && mv "$f" zig-out/
done
