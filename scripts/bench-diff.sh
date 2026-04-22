#!/bin/bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <current.json> <baseline.json>" >&2
  exit 2
fi

current_json="$1"
baseline_json="$2"

if [[ ! -f "$current_json" ]]; then
  echo "current benchmark file not found: $current_json" >&2
  exit 1
fi

if [[ ! -f "$baseline_json" ]]; then
  echo "baseline benchmark file not found: $baseline_json" >&2
  exit 1
fi

python3 - "$current_json" "$baseline_json" <<'PY'
import json
import math
import sys

current_path, baseline_path = sys.argv[1], sys.argv[2]

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
    ratios.append(ratio)
    regression_pct = max(0.0, (1.0 - ratio) * 100.0)
    if regression_pct > 3.0:
        hard_failures.append((name, regression_pct, current_ops[name], base))

if hard_failures:
    for name, regression_pct, current_value, baseline_value in hard_failures:
        print(
            f"benchmark regression >3%: {name} current={current_value:.0f} baseline={baseline_value:.0f} regression={regression_pct:.2f}%",
            file=sys.stderr,
        )
    raise SystemExit(1)

geomean = math.exp(sum(math.log(r) for r in ratios) / len(ratios))
geomean_regression_pct = max(0.0, (1.0 - geomean) * 100.0)
if geomean_regression_pct > 1.0:
    print(
        f"geomean regression >1%: geomean={geomean:.5f} regression={geomean_regression_pct:.2f}%",
        file=sys.stderr,
    )
    raise SystemExit(1)

print(
    f"bench-check ok: compared {len(ratios)} benchmarks, geomean={geomean:.5f}, regression={geomean_regression_pct:.2f}%"
)
PY
