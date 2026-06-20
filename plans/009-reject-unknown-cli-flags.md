# Plan 009: Reject unknown flags on machine-facing analyzer commands

> **Executor instructions**: Follow this plan step by step. Run every verification command and confirm the expected result before moving on. Touch only the files listed as in scope. If any STOP condition occurs, stop and report; do not improvise around it. When done, update the status row for this plan in `plans/README.md`, unless a reviewer says they maintain the index.
>
> **Drift check, run first**: `git diff --name-only 560239d -- packages/tools/src/zigts_cli.zig packages/tools/src/expert.zig packages/tools/src/describe_rule.zig packages/tools/src/search_rules.zig packages/runtime/src/dev_cli.zig`
> Empty means no drift. If any path appears, re-open it and compare against the Current state excerpts before editing.

## Status

- **Priority**: P2
- **Effort**: S
- **Risk**: LOW-MED
- **Depends on**: none
- **Category**: dx
- **Planned at**: commit `560239d` and 2026-06-20

## Why this matters

Several machine-facing commands silently drop unrecognized flags. `zigttp modules --josn` (typo) exits 0 and prints the default human-readable text instead of JSON, with no error. These commands exist for tool/CI/agent integration, where a silently-ignored flag is a wrong-output failure that is hard to notice. Sibling commands (`restrictions`, `check`) already reject unknown flags and get a clean error message via the shared dispatch, so the behavior is inconsistent.

## Current state

Silent-ignore commands — their arg loops match only known flags and drop the rest:

- `packages/tools/src/zigts_cli.zig:360-364` — `runFeaturesCommand`: `for (argv) |arg| { if (eql(arg,"--json")) json_mode = true; }`.
- `packages/tools/src/zigts_cli.zig:441-445` — `runModulesCommand`: same pattern.
- `packages/tools/src/expert.zig:62-71` — `runMeta`: `hasHelpFlag` + `hasFlag(argv,"--json")` only.
- `packages/tools/src/describe_rule.zig:16-27` — a typo'd `-` flag falls through (the `else if (!startsWith("-"))` guard excludes it, so it is ignored).
- `packages/tools/src/search_rules.zig:14-23` — same fall-through.

Strict siblings to match:

- `packages/tools/src/zigts_cli.zig:421` — `runRestrictionsCommand` ends its loop with `return error.InvalidArgument;`.
- `packages/tools/src/zigts_cli.zig:205` — `runCheckCommand` does the same.

Shared error surfacing that makes the strict path clean:

- `packages/runtime/src/dev_cli.zig:351-373` — maps `error.InvalidArgument`/`MissingArgument`/`UnknownOption` from `zigts_cli.run` to `"zigttp <cmd>: invalid arguments (...). Run \`zigttp <cmd> --help\`..."` and exit 1.

## Commands you will need

| Purpose | Command | Expected on success |
|---|---|---|
| CLI tests | `zig build test-cli` | build summary success, 0 failed |
| Engine/tools tests | `zig build test-zigts` | success |
| Manual: typo flag rejected | build, then `zigttp modules --josn; echo $?` | non-zero exit + invalid-arguments message |
| Manual: valid flag works | `zigttp modules --json` | JSON output, exit 0 |
| Format gate | `zig fmt --check build.zig packages/` | no output |
| Aggregate | `zig build test` | success |

## Scope

**In scope:**

- `packages/tools/src/zigts_cli.zig` — `runFeaturesCommand`, `runModulesCommand` arg loops.
- `packages/tools/src/expert.zig` — `runMeta` arg handling.
- `packages/tools/src/describe_rule.zig` and `packages/tools/src/search_rules.zig` — reject unknown `-`-prefixed flags while preserving the single positional argument each accepts.

**Out of scope:**

- Changing accepted flags, output formats, or the positional-argument semantics of `describe-rule`/`search`.
- Refactoring the broader CLI dispatch beyond adding reject branches (a shared helper is optional, see Step 2).
- `restrictions`/`check` (already correct).

## Git/workflow guidance

- Branch: work on `main`.
- Commit style: Conventional Commits, e.g. `fix(cli): reject unknown flags on features/modules/meta/describe-rule/search`.
- Do not push or open a PR unless the operator asks.

## Steps

### Step 1: Add reject branches

In each of the five loops, add a final branch that returns `error.InvalidArgument` for an unrecognized argument, matching `runRestrictionsCommand` (`zigts_cli.zig:421`). For `describe-rule` and `search`, keep accepting the single positional (the rule name / keyword) — only reject tokens that start with `-` and are not a known flag.

**Verify**: `zig build test-zigts test-cli` -> success.

### Step 2 (optional): Factor a shared helper

If it reduces duplication cleanly, add a tiny `rejectUnknownFlag(arg) !void` helper and call it from all five sites so the behavior cannot drift again. Keep it minimal; do not restructure dispatch.

### Step 3: Confirm error surfacing

Build the `zigttp` binary and confirm the reject path produces the clean `dev_cli.zig:351-373` message (not a raw Zig error trace).

**Verify**: `zigttp modules --josn; echo $?` -> prints the invalid-arguments message and a non-zero exit; `zigttp modules --json` -> JSON, exit 0; `zigttp describe-rule ZTS001` (or a valid rule) -> still works.

### Step 4: Tests

Add CLI tests asserting that each affected command returns `error.InvalidArgument` (or the dispatch maps it to a non-zero exit) for an unknown flag, and still succeeds for its valid flags/positional. Model after existing `zigts_cli`/`dev_cli` arg-validation tests (search for `InvalidArgument` in the test blocks).

**Verify**: `zig build test-cli test-zigts` -> success; `zig fmt --check build.zig packages/` -> no output.

## Test plan

- New tests: unknown-flag rejection for `features`, `modules`, `meta`, `describe-rule`, `search`; valid-flag/positional acceptance preserved.
- Regression: the new tests fail on the planned-at implementation (silent ignore) and pass after.
- Final: `zig build test` -> success.

## Done criteria

All must hold:

- [ ] The five commands reject unknown flags with `error.InvalidArgument`, surfaced as the clean dispatch message + non-zero exit.
- [ ] Valid flags and the `describe-rule`/`search` positional still work.
- [ ] `zig build test-cli test-zigts` exit 0; `zig build test` exit 0; `zig fmt --check` clean.
- [ ] New tests cover reject + accept for each command.
- [ ] No files outside the in-scope list are modified.
- [ ] `plans/README.md` status row updated.

## STOP conditions

Stop and report if:

- An existing test, example, script, or `scripts/*.sh` passes a stray/unknown flag to one of these commands and would now break (report it; decide whether the caller or the plan is wrong).
- Rejecting unknown flags would break a documented machine-integration contract in `docs/internals/zigts-expert-contract.md` (re-check that doc first).
- The positional-argument detection for `describe-rule`/`search` cannot cleanly distinguish a positional from an unknown flag.

## Maintenance notes

- The optional shared helper (Step 2) is the durable fix against re-drift; a reviewer should prefer it for any new command.
- Keep parity with `restrictions`/`check` as the reference behavior.
