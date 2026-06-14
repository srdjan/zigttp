#!/bin/bash
set -euo pipefail

# Usage:
#   bench-diff.sh --baseline <baseline.json> --bench <bench-exe> [--runs N]
#
# Runs the benchmark binary $BENCH_RUNS times (default 5) and compares the
# per-benchmark best-of-N ops_per_sec against the baseline. Microbench variance
# frequently exceeds 3% on a single run, so single-run mode is too flaky for
# CI gating.

BENCH_RUNS="${BENCH_RUNS:-5}"
REGRESSION_PCT="${BENCH_REGRESSION_PCT:-8.0}"
GEOMEAN_PCT="${BENCH_GEOMEAN_PCT:-3.0}"

usage() {
  echo "usage: $0 --baseline <baseline.json> --bench <bench-exe> [--runs N]" >&2
  exit 2
}

baseline_json=""
bench_exe=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --baseline) baseline_json="$2"; shift 2 ;;
    --bench)    bench_exe="$2";     shift 2 ;;
    --runs)     BENCH_RUNS="$2";    shift 2 ;;
    *)          usage ;;
  esac
done

if [[ -z "$baseline_json" || ! -f "$baseline_json" ]]; then
  echo "baseline benchmark file not found: $baseline_json" >&2
  exit 1
fi
if [[ -z "$bench_exe" || ! -x "$bench_exe" ]]; then
  echo "bench binary not executable: $bench_exe" >&2
  exit 1
fi

tmpdir="$(mktemp -d)"
cleanup() { [[ -d "$tmpdir" ]] && rm -rf "$tmpdir"; }
trap cleanup EXIT INT TERM

for i in $(seq 1 "$BENCH_RUNS"); do
  "$bench_exe" --json --quiet > "$tmpdir/run-$i.json"
done
current_json="$tmpdir/current.json"
python3 - "$current_json" "$tmpdir" "$BENCH_RUNS" <<'PY'
import json, sys, os
out_path, tmpdir, n = sys.argv[1], sys.argv[2], int(sys.argv[3])
runs = []
for i in range(1, n + 1):
    with open(os.path.join(tmpdir, f"run-{i}.json")) as fh:
        runs.append(json.load(fh))
# Take per-bench best (max ops_per_sec) across runs.
best = {b["name"]: dict(b) for b in runs[0]["benchmarks"]}
for run in runs[1:]:
    for b in run["benchmarks"]:
        prev = best.get(b["name"])
        if prev is None or b.get("ops_per_sec", 0) > prev.get("ops_per_sec", 0):
            best[b["name"]] = dict(b)
merged = dict(runs[0])
merged["benchmarks"] = [best[name] for name in best]
with open(out_path, "w") as fh:
    json.dump(merged, fh)
PY

python3 - "$current_json" "$baseline_json" "$REGRESSION_PCT" "$GEOMEAN_PCT" <<'PY'
import json
import math
import sys

# Benches whose per-run time is dominated by timer resolution or JIT warmup variance.
# They stay in the JSON report, but are excluded from gate checks entirely.
SKIP_REGRESSION_CHECK = {"forOfLoop", "httpHandler", "httpHandlerHeavy"}

current_path, baseline_path = sys.argv[1], sys.argv[2]
regression_limit = float(sys.argv[3])
geomean_limit = float(sys.argv[4])

with open(current_path, "r", encoding="utf-8") as fh:
    current = json.load(fh)
with open(baseline_path, "r", encoding="utf-8") as fh:
    baseline = json.load(fh)

def extract_ops(doc, label):
    benches = {}
    for bench in doc.get("benchmarks", []):
        name = bench.get("name")
        if not name:
            raise SystemExit(f"{label}: benchmark entry missing name")
        if not bench.get("success", True):
            raise SystemExit(f"{label}: benchmark {name} failed ({bench.get('error')})")
        ops = bench.get("ops_per_sec")
        if not isinstance(ops, (int, float)):
            raise SystemExit(f"{label}: benchmark {name} missing ops_per_sec")
        benches[name] = float(ops)
    if not benches:
        raise SystemExit(f"{label}: no benchmark entries found")
    return benches

current_ops = extract_ops(current, "current")
baseline_ops = extract_ops(baseline, "baseline")

missing = sorted(set(baseline_ops) - set(current_ops))
if missing:
    raise SystemExit("current: missing baseline benchmarks: " + ", ".join(missing))

ratios = []
hard_failures = []
for name in sorted(baseline_ops):
    base = baseline_ops[name]
    if base <= 0:
        raise SystemExit(f"baseline: benchmark {name} has non-positive ops_per_sec")
    ratio = current_ops[name] / base
    if name in SKIP_REGRESSION_CHECK:
        continue
    ratios.append(ratio)
    regression_pct = max(0.0, (1.0 - ratio) * 100.0)
    if regression_pct > regression_limit:
        hard_failures.append((name, regression_pct, current_ops[name], base))

if hard_failures:
    for name, regression_pct, current_value, baseline_value in hard_failures:
        print(
            f"benchmark regression >{regression_limit:g}%: {name} current={current_value:.0f} baseline={baseline_value:.0f} regression={regression_pct:.2f}%",
            file=sys.stderr,
        )
    raise SystemExit(1)

if not ratios:
    raise SystemExit("no benchmarks left after applying regression-check skip list")

geomean = math.exp(sum(math.log(r) for r in ratios) / len(ratios))
geomean_regression_pct = max(0.0, (1.0 - geomean) * 100.0)
if geomean_regression_pct > geomean_limit:
    print(
        f"geomean regression >{geomean_limit:g}%: geomean={geomean:.5f} regression={geomean_regression_pct:.2f}%",
        file=sys.stderr,
    )
    raise SystemExit(1)

print(
    f"bench-check ok: compared {len(ratios)} benchmarks, geomean={geomean:.5f}, regression={geomean_regression_pct:.2f}%"
)
PY
