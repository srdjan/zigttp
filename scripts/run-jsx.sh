#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

zig build -Doptimize=ReleaseFast -Dhandler=examples/handler.jsx &&
./zig-out/bin/zigttp-server -p 3000
