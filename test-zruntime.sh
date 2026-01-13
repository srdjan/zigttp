#!/usr/bin/env bash
set -euo pipefail

exec zig test --dep zts -Mroot=src/zruntime.zig -Mzts=zts/root.zig "$@"
