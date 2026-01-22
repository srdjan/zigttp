#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BENCH_BIN="$ROOT_DIR/zig-out/bin/zigttp-bench"

if [[ ! -x "$BENCH_BIN" ]]; then
  echo "building zigttp-bench (ReleaseFast)..."
  (cd "$ROOT_DIR" && zig build -Doptimize=ReleaseFast)
fi

run_case() {
  local script="$1"
  local fn="$2"
  local iters="$3"
  local warmup_rounds="$4"
  local warmup_iters="$5"

  echo "== $(basename "$script")::$fn =="
  "$BENCH_BIN" --script "$script" \
    --bench \
    --bench-fn "$fn" \
    --iterations "$iters" \
    --warmup "$warmup_rounds" \
    --warmup-iters "$warmup_iters"
  echo ""
}

run_case "$ROOT_DIR/benchmarks/jit/arith.js" run 200000 120 200
run_case "$ROOT_DIR/benchmarks/jit/math.js" runInt 200000 120 200
run_case "$ROOT_DIR/benchmarks/jit/math.js" runMixed 200000 120 200
run_case "$ROOT_DIR/benchmarks/jit/branch.js" run 200000 120 200
run_case "$ROOT_DIR/benchmarks/jit/object.js" run 100000 120 200
run_case "$ROOT_DIR/benchmarks/jit/pic.js" runMono 150000 120 200
run_case "$ROOT_DIR/benchmarks/jit/pic.js" runPoly 150000 120 200
if [[ "${JIT_SKIP_CALL:-0}" != "1" ]]; then
  run_case "$ROOT_DIR/benchmarks/jit/call.js" runFunc 50000 120 200
  run_case "$ROOT_DIR/benchmarks/jit/call.js" runMethod 50000 120 200
else
  echo "== call.js skipped (JIT_SKIP_CALL=1) =="
  echo ""
fi
