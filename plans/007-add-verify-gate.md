# Plan 007: Add a one-command verify gate and correct the `zig build test` doc claim

> **Executor instructions**: Follow this plan step by step. Run every verification command and confirm the expected result before moving on. Touch only the files listed as in scope. If any STOP condition occurs, stop and report; do not improvise around it. When done, update the status row for this plan in `plans/README.md`, unless a reviewer says they maintain the index.
>
> **Drift check, run first**: `git diff --name-only 560239d -- build.zig .github/workflows/ci.yml CLAUDE.md scripts/`
> Empty (or only new files under `scripts/`) means no drift. If `build.zig`, `ci.yml`, or `CLAUDE.md` appears, re-open it and compare against the Current state excerpts before editing.

## Status

- **Priority**: P1 (prerequisite/DX: floats up — closes the local/CI signal gap)
- **Effort**: S
- **Risk**: LOW
- **Depends on**: none. Best landed alongside 001 (both restore trustworthy gate signal).
- **Category**: dx
- **Planned at**: commit `560239d` and 2026-06-20

## Why this matters

There is no single local command that reproduces CI, and the documented one (`zig build test`) covers less than CI runs. A contributor or automated executor can get a green `zig build test` locally and still fail CI on panic-isolation, smoke, doc-links, the standalone zruntime root, examples, or the policy-hash assert. That is the "tests pass locally but CI is red" anti-pattern. This plan adds a sequential `verify` script mirroring CI exactly and corrects the doc so the gate signal is trustworthy.

## Current state

`build.zig:656-674` — the `test` step depends on ~20 sub-suites but, by design (`build.zig:689-692`), does **not** include the standalone `test-zruntime` root, and does not include `smoke-v1`, `test-panic-isolation`, the ReleaseFast build, `test-doc-links`, or `scripts/test-examples.sh`:

```zig
const test_step = b.step("test", "Run unit tests");
test_step.dependOn(&run_unit_tests.step);
... // many sub-suites
test_step.dependOn(&run_server_tests.step);
```

`build.zig:689-692` documents why zruntime is kept out of the aggregate:

```
// main.zig already imports zruntime.zig in the aggregate runtime test root.
// Keep test-zruntime as a focused standalone target, but do not run the same
// pool-heavy tests twice inside zig build test; parallel duplicate roots
// have produced intermittent libc/JIT/arena teardown TRAPs on macOS.
```

`.github/workflows/ci.yml` runs these as **separate sequential** steps:

```yaml
- run: zig build test
- run: zig build test-zruntime
- run: zig build test-doc-links
- run: zig build -Doptimize=ReleaseFast
- run: zig build smoke-v1
- run: zig build test-panic-isolation
- run: bash scripts/test-examples.sh
- name: Assert policy hash unchanged   # (separate step)
```

`CLAUDE.md:126` overstates the aggregate:

```
zig build test                     # All tests (runtime + CLI + engine + zruntime)
```

It is not "all tests" and the standalone zruntime root is not in it.

## Commands you will need

| Purpose | Command | Expected on success |
|---|---|---|
| Aggregate suite | `zig build test` | build summary success |
| New verify script | `bash scripts/verify.sh` | every sub-step prints success; script exits 0 |
| Format gate | `zig fmt --check build.zig packages/` | no output |

## Scope

**In scope:**

- `scripts/verify.sh` (new) — runs the CI build/test steps sequentially.
- `CLAUDE.md` — correct the `zig build test` description and point at `verify`.

**Out of scope:**

- `build.zig` — do **not** add a `verify` build step that `dependOn`s both `test` and `test-zruntime`; the build graph may run those pool-heavy roots in parallel and reintroduce the macOS teardown TRAP the existing comment warns about. Sequential shell invocation is the safe shape.
- `.github/workflows/ci.yml` — leave CI as-is. (Optionally a later plan can make CI call `scripts/verify.sh`; not here, to keep this reversible.)
- Any test logic or source under `packages/`.

## Git/workflow guidance

- Branch: work on `main`.
- Commit style: Conventional Commits, e.g. `chore(dx): add scripts/verify.sh mirroring CI and fix test doc`.
- Do not push or open a PR unless the operator asks.

## Steps

### Step 1: Write `scripts/verify.sh`

Create a `bash` script with `set -euo pipefail` that runs, sequentially (one process each, mirroring `ci.yml` order):

```sh
zig build test
zig build test-zruntime
zig build test-doc-links
zig build -Doptimize=ReleaseFast
zig build smoke-v1
zig build test-panic-isolation
bash scripts/test-examples.sh
```

Use the exact step names present in `build.zig`/`ci.yml` (verify each `zig build <step>` exists by checking `build.zig` `b.step("...")` calls). Echo a banner before each. Make it executable (`chmod +x scripts/verify.sh`). Do not add the policy-hash assert unless `ci.yml` shows the exact command; if it is a non-trivial inline step, reference it in a comment as a CI-only check rather than guessing.

**Verify**: `bash scripts/verify.sh` -> runs each step sequentially and exits 0. (Allow the documented expected failure-path text from ratchet/panic-isolation tests; the final per-step summaries must be successful.)

### Step 2: Correct `CLAUDE.md`

Change the `zig build test` line (`CLAUDE.md:126`) so it no longer claims "All tests ... + zruntime". State that `zig build test` is the bulk unit suite (excludes the standalone zruntime root, smoke, panic-isolation, examples) and that `bash scripts/verify.sh` is the full local gate mirroring CI. Keep the surrounding command list intact.

**Verify**: `git diff CLAUDE.md` shows only the corrected description and the new `verify` line.

## Test plan

- No new unit tests; the deliverable is the script and the doc correction.
- Regression check: `bash scripts/verify.sh` reproduces a CI-equivalent local run and exits 0 on a clean tree.

## Done criteria

All must hold:

- [ ] `scripts/verify.sh` exists, is executable, and runs the CI step set sequentially.
- [ ] `bash scripts/verify.sh` exits 0 on the current tree.
- [ ] `CLAUDE.md` no longer claims `zig build test` is "all tests"; it points to `scripts/verify.sh`.
- [ ] No `build.zig` or `ci.yml` changes.
- [ ] `plans/README.md` status row updated.

## STOP conditions

Stop and report if:

- Any `zig build <step>` referenced in `ci.yml` does not exist as a `build.zig` step (names drifted) — list the mismatch.
- `scripts/test-examples.sh` does not exist or needs arguments not shown in `ci.yml`.
- `bash scripts/verify.sh` fails a step that is green when run on its own (indicates an environment/ordering issue, not a code defect) — report which step and the output.
- The policy-hash CI step requires a command you cannot reproduce safely.

## Maintenance notes

- If CI steps change, update `scripts/verify.sh` in the same change; a future follow-up could make `ci.yml` call the script so the two cannot drift.
- This plan deliberately avoids a `build.zig` `verify` step to dodge the macOS parallel-duplicate-root TRAP; do not "simplify" it into one without solving that.
