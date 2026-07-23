# Restore Zig Format Baseline

Planned at commit: `f2c637d` (`refactor(runtime): group self-extract payload params into PayloadInput`)
Planned on: 2026-06-20

## Drift Check

Before editing, verify the in-scope files have not changed since this plan was written:

```sh
git diff --name-only f2c637d -- packages/runtime/src/zruntime.zig .github/workflows/ci.yml CONTRIBUTING.md
```

Expected output for direct execution from the planned commit is empty. If any path appears, re-open that file and confirm the finding still applies before proceeding.

## Status

- Priority: P1
- Effort: S
- Risk: LOW
- Confidence: HIGH
- Category: DX / CI hygiene
- Depends on: none

## Why

The repository's documented and CI-enforced format gate currently fails on a clean checkout. This blocks trustworthy CI signal and makes later source changes harder to review.

## Current State

- `.github/workflows/ci.yml` runs `zig fmt --check build.zig packages/` in CI.
- `CONTRIBUTING.md` instructs contributors to run `zig fmt` before every commit.
- On commit `f2c637d`, this command fails:

```sh
zig fmt --check build.zig packages/
```

Observed output:

```text
packages/runtime/src/zruntime.zig
```

Other gates were checked during the advisory pass and passed:

```sh
zig build test-docs-drift test-doc-links --summary all
zig build test-zts --summary all
zig build test-zruntime test-server test-modules test-module-governance test-capability-audit test-sdk --summary all
zig build test-cli test-expert test-expert-app test-cassette test-expert-golden test-proof-review --summary all
zig build test-panic-isolation --summary all
zig build test --summary all
```

## Scope

In scope:

- `packages/runtime/src/zruntime.zig`

Out of scope:

- Any semantic runtime change.
- Formatting unrelated files unless the drift check shows the baseline has changed and the executor re-scopes intentionally.

## Steps

1. Run:

   ```sh
   zig fmt packages/runtime/src/zruntime.zig
   ```

2. Inspect the diff:

   ```sh
   git diff -- packages/runtime/src/zruntime.zig
   ```

   Confirm the diff is formatting-only.

3. Verify the format gate:

   ```sh
   zig fmt --check build.zig packages/
   ```

4. Run focused runtime checks:

   ```sh
   zig build test-zruntime test-server --summary all
   ```

5. Run the aggregate gate:

   ```sh
   zig build test --summary all
   git diff --check
   ```

## Done Criteria

- `zig fmt --check build.zig packages/` passes.
- `zig build test-zruntime test-server --summary all` passes.
- `zig build test --summary all` passes.
- `git diff --check` passes.
- Diff is limited to formatting in `packages/runtime/src/zruntime.zig`.

## STOP Conditions

- `zig fmt` changes files outside `packages/runtime/src/zruntime.zig`.
- The diff includes semantic edits.
- Runtime tests fail after a formatting-only change.
