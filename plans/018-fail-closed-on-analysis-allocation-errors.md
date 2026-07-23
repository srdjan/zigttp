# Plan 018: Fail closed when compiler analysis cannot allocate

> **Executor instructions**: Follow this plan phase by phase. Run every verification command and red-prove each new allocator-failure test. Do not turn intentional optimization fallbacks into fatal compile errors. Update `plans/README.md` when complete.
>
> **Drift check, run first**: `git diff --name-only b00eae29 -- packages/zts/src/pipeline.zig packages/zts/src/bool_checker.zig packages/zts/src/type_checker.zig packages/zts/src/strict_checker.zig packages/zts/src/handler_verifier.zig packages/zts/src/flow_checker.zig`

## Status

- **Priority**: P0
- **Effort**: L
- **Risk**: MED
- **Depends on**: none
- **Category**: compiler soundness / fail-closed behavior
- **Planned at**: `b00eae29` on 2026-07-10

## Why this matters

The compiler's proof contract depends on diagnostics and analysis facts being complete. Several proof walkers swallow allocation failures when appending diagnostics or updating flow/type maps, then their public `!u32` methods count whatever partial diagnostics survived. The pipeline can report zero errors and emit a clean proof result even though a load-bearing finding or narrowing/taint fact was dropped.

Allocation pressure should produce `error.OutOfMemory`, never a stronger proof.

## Current evidence

- `packages/zts/src/pipeline.zig:137-190,223-255` trusts the error counts returned by all five analysis passes.
- `packages/zts/src/type_checker.zig:146-154,1835-1837` exposes an error-returning `check` but drops diagnostic append failure; load-bearing maps also use `catch {}` at lines such as 257, 276-292, and 1687.
- `packages/zts/src/strict_checker.zig:151-164,197-199` has the same partial-diagnostic pattern.
- `packages/zts/src/handler_verifier.zig:356-390,1439-1441` can omit a verifier error and return a lower count.
- `packages/zts/src/flow_checker.zig:396-409,1892-1919` can drop taint diagnostics and witness data.
- `packages/zts/src/bool_checker.zig:216-225,1671-1673` can drop state-isolation/type diagnostics.

Intentional best-effort fallbacks exist elsewhere, such as static handler fast-path/JIT analysis falling back to the interpreter. Those are not proof claims and are outside this plan.

## Scope

In scope:

- bool, type, strict, handler-verifier, and flow-checker state required for diagnostics/properties;
- pipeline propagation of analysis allocation failure;
- `std.testing.FailingAllocator` regression coverage for each pass and one pipeline-level assertion.

Out of scope:

- JIT compilation/cache allocation fallbacks;
- optional static route fast paths that safely fall back to bytecode execution;
- redesigning `TypePool`'s `null_type_idx` sentinel globally;
- merging the independent analysis walkers.

## Steps

### 1. Inventory proof-relevant swallowed failures

For each in-scope pass, classify every `catch {}`, `catch return`, and sentinel fallback as either:

- proof-relevant state/diagnostic: must set failure and eventually return an error;
- message/witness enrichment only: may degrade detail if the core diagnostic remains present;
- intentional non-proof optimization fallback: leave unchanged and document why.

Record the inventory in code comments near the checker state, not as a new broad architecture document.

### 2. Add a sticky analysis-failure state

Use the smallest explicit mechanism that fits the existing void-returning walkers, such as a sticky `allocation_failed: bool`. Every proof-relevant failed append/put/dupe sets it. Traversal may short-circuit once set, but the public `check`/`verify` method must return `error.OutOfMemory` before exposing counts or properties.

Where practical and reviewable, convert a local helper from `void` to `!void` instead of adding another swallowed catch. Keep the public API error union already present.

Ensure owned diagnostic strings are freed when the diagnostic append itself fails; do not trade soundness for leaks.

### 3. Propagate through the pipeline

`pipeline.resolve` and `pipeline.check` already use `try`; preserve that path and add explicit tests that no `ResolvedModule`/`CheckedModule` is produced after an in-scope pass reports allocation failure.

Audit contract/proof construction call sites to ensure they do not catch `OutOfMemory` and substitute empty diagnostics/properties.

### 4. Red-prove failure injection

Add deterministic failing-allocator tests that hit at least:

- diagnostic append in each pass;
- a type/narrowing map update;
- a flow label/defended-path update;
- the pipeline-level call around one known-invalid handler.

Each test must prove that the pre-fix code can return a clean/partial result at that fail index and the fixed code returns `error.OutOfMemory`. Avoid brittle global allocation ordinals where a small targeted allocator/capability can isolate the desired append.

## Verification

```sh
zig fmt --check build.zig packages/
zig build test-zts test-precompile test-zts-cli test-expert
bash scripts/verify.sh
git diff --check
git status --short
```

## Done criteria

- [x] No proof-relevant allocation failure is silently converted to missing facts or diagnostics in the five scoped passes.
- [x] Public pass APIs return `error.OutOfMemory` on sticky failure.
- [x] The pipeline cannot emit a clean result after forced analysis failure.
- [x] Intentional optimization fallbacks remain non-fatal and are distinguished in review.
- [x] Red-proven allocator-failure tests cover every pass.
- [x] All verification commands pass.

## STOP conditions

- Correctness requires redesigning the global `TypePool` sentinel contract; split that into a separately approved plan rather than silently expanding this one.
- A proposed change makes safe interpreter/JIT fallback allocation failures fatal without proof that the path affects a proof claim.
- A failing-allocator test cannot reliably target a proof-relevant operation; add an injectable allocator boundary rather than asserting a fragile ordinal.
- A focused gate fails twice after reasonable correction.
