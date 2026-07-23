# Plan 008: Correct `zttp:io` return-type metadata for `parallel` and `race`

> **Executor instructions**: Follow this plan step by step. Run every verification command and confirm the expected result before moving on. Touch only the files listed as in scope. If any STOP condition occurs, stop and report; do not improvise around it. When done, update the status row for this plan in `plans/README.md`, unless a reviewer says they maintain the index.
>
> **Drift check, run first**: `git diff --name-only 560239d -- packages/zts/src/modules/workflow/io.zig packages/modules/module-specs/workflow/io.json packages/zts/src/builtin_modules.zig`
> Empty means no drift. If any path appears, re-open it and compare against the Current state excerpts before editing.

## Status

- **Priority**: P2
- **Effort**: S
- **Risk**: LOW
- **Depends on**: none
- **Category**: correctness (type metadata / observability)
- **Planned at**: commit `560239d` and 2026-06-20

## Why this matters

The `zttp:io` exports `parallel` and `race` declare their return kind as `.string`, but `parallel` returns a JS array and `race` returns a Response object (or `undefined`). This wrong metadata flows into the type checker, the bool checker, the generated `.d.ts` signatures, the `modules --json` surface, and the expert agent's view of the module — every consumer is told these return strings. A handler doing `const r = parallel([...]); r[0].json()` is type-checked as if `r` were a string. The binding-vs-spec audit (ZVM009) cannot catch it because the JSON spec mirrors the same wrong value.

## Current state

`packages/zts/src/modules/workflow/io.zig:31-40`:

```zig
pub const binding = mb.ModuleBinding{
    .specifier = "zttp:io",
    .name = "io",
    .required_capabilities = &.{.runtime_callback},
    .stateful = true,
    .exports = &.{
        .{ .name = "parallel", .func = parallelNative, .arg_count = 1, .effect = .write, .returns = .string, .param_types = &.{}, .return_labels = .{ .external = true } },
        .{ .name = "race", .func = raceNative, .arg_count = 1, .effect = .write, .returns = .string, .param_types = &.{}, .return_labels = .{ .external = true } },
    },
};
```

Actual returns contradict `.string`:

- `io.zig:261` — `parallel` returns `result_arr.toValue()` (a JS array; docstring `io.zig:8` says `-> Array<T>`).
- `io.zig:367,375` — `race` returns a Response object from the build-response path, or `JSValue.undefined_val`.

The JSON spec mirrors the wrong value, `packages/modules/module-specs/workflow/io.json`:

```json
"exports": [
  { "name": "parallel", "effect": "write", "returns": "string" },
  { "name": "race",     "effect": "write", "returns": "string" }
]
```

`ReturnKind` is defined at `packages/zts/src/module_binding.zig:1167`; its `jsTypeName` map (`:1190-1195`) includes `.string -> "string"`, `.object -> "object"`, `.unknown -> "unknown"`, `.optional_string`, `.optional_object`. The analogous outbound module declares `.returns = .object` for both `fetch` and `fetchWithRetry` (`packages/modules/src/net/fetch.zig:35,48`) — that is the correct precedent to follow. The registry-assertion test pattern lives near `packages/zts/src/builtin_modules.zig:202-229`.

## Commands you will need

| Purpose | Command | Expected on success |
|---|---|---|
| Engine tests | `zig build test-zts` | build summary success |
| Module governance / binding-vs-spec audit | `zig build test-module-governance` | success |
| Module tests | `zig build test-modules` | success |
| Inspect machine surface | `zig build run -- modules --json` (or run the built `zttp modules --json`) | `parallel`/`race` show `"object"` |
| Format gate | `zig fmt --check build.zig packages/` | no output |
| Aggregate | `zig build test` | success |

## Scope

**In scope:**

- `packages/zts/src/modules/workflow/io.zig` — change both `.returns` values.
- `packages/modules/module-specs/workflow/io.json` — change both `"returns"` values to match (required, or ZVM009 fails).
- A registry assertion test (in `builtin_modules.zig` near `:202-229`, or in `io.zig`) pinning the corrected return kinds so the two copies cannot silently drift back.

**Out of scope:**

- Changing `parallel`/`race` runtime behavior or their `param_types`/`effect`/`return_labels` (note `param_types = &.{}` is a separate, smaller gap — leave it unless trivially correct and covered by a test).
- Adding a new `ReturnKind` variant. Use the closest existing kind.
- Any other module's metadata.

## Git/workflow guidance

- Branch: work on `main`.
- Commit style: Conventional Commits, e.g. `fix(modules): correct zttp:io parallel/race return metadata`.
- Do not push or open a PR unless the operator asks.

## Steps

### Step 1: Pick the correct `ReturnKind`

Read `module_binding.zig:1167-1195`. Use `.object` for both `parallel` (array is an object in JS) and `race` (Response object), matching `fetch.zig`'s `.object`. If a more specific kind exists that the type checker maps usefully (e.g. an array/response kind), prefer it only if a consumer benefits and a test covers it; otherwise `.object`.

### Step 2: Update the binding and the spec together

Set `.returns = .object` for both exports in `io.zig:37-38`, and `"returns": "object"` for both in `io.json`. They must change together so the ZVM009 binding-vs-spec audit stays consistent.

**Verify**: `zig build test-module-governance` -> success (audit agrees).

### Step 3: Add a registry assertion test

Add a test asserting `binding.exports[..].returns == .object` for `parallel` and `race` (model after `builtin_modules.zig:202-229`). This pins the metadata against future drift.

**Verify**: `zig build test-zts` -> success; the new test fails if either `.returns` is reverted to `.string`.

### Step 4: Confirm the machine surface

**Verify**: build and run `zttp modules --json`; confirm `parallel` and `race` now report `"object"`. Run `zig fmt --check build.zig packages/` -> no output, and `zig build test` -> success.

## Test plan

- New registry assertion test (above) is the regression guard; it must fail on the planned-at `.string` and pass after.
- Re-run any example handler that imports `zttp:io` (search `examples/` for `zttp:io`) to confirm corrected metadata does not surface a spurious type error; if it does, that is a real previously-masked diagnostic — report it (do not weaken the fix).

## Done criteria

All must hold:

- [ ] `io.zig` and `io.json` both declare `object` for `parallel` and `race`.
- [ ] `zig build test-zts test-module-governance test-modules` all exit 0.
- [ ] A registry test pins the corrected return kinds.
- [ ] `zttp modules --json` shows `"object"` for both.
- [ ] `zig build test` exits 0; `zig fmt --check` clean.
- [ ] `plans/README.md` status row updated.

## STOP conditions

Stop and report if:

- Current-state excerpts do not match live code.
- Correcting the metadata surfaces type errors in an existing example/handler that the project intends to keep working (report the diagnostic; it indicates the old `.string` was masking a real mismatch).
- No suitable `ReturnKind` exists and the choice requires a new enum variant (escalate as a design decision).

## Maintenance notes

- The same wrong-value-in-both-copies pattern can recur for any module; the new registry test is the guard. A reviewer should require such a test for any new module export's return kind.
- `param_types = &.{}` on these exports is a related, deferred gap (the callback-array argument is untyped); note it but do not expand this plan.
