# Correct Sparse Array Callback Semantics

Planned at commit: `f2c637d` (`refactor(runtime): group self-extract payload params into PayloadInput`)
Planned on: 2026-06-20

## Drift Check

Before editing, verify the in-scope files have not changed since this plan was written:

```sh
git diff --name-only f2c637d -- packages/zts/src/builtins/array.zig packages/zts/src/object.zig
```

Expected output for direct execution from the planned commit is empty. If either path appears, re-open the callback implementations and storage helpers before editing.

## Status

- Priority: P2
- Effort: M
- Risk: MEDIUM
- Confidence: MEDIUM-HIGH
- Category: JavaScript semantics
- Depends on: `plans/001-restore-format-baseline.md`

## Why

Several `Array.prototype` callback methods currently invoke callbacks for in-length sparse holes by substituting `undefined`. For methods such as `map`, `filter`, `reduce`, `forEach`, `every`, and `some`, JavaScript semantics require hole-aware behavior. This is a user-visible engine correctness issue.

## Current State

Evidence from `packages/zts/src/builtins/array.zig`:

- `arrayMap` loops `0..len`, reads `obj.getIndex(i) orelse undefined`, invokes the callback, and `arrayPush`es at lines 652-668.
- `arrayFilter` does the same at lines 671-689.
- `arrayReduce` uses index `0` as the implicit accumulator even if it is absent, then calls the callback for every later index at lines 692-720.
- `arrayForEach`, `arrayEvery`, and `arraySome` call callbacks for every index from `0..len` at lines 723-770.
- `arrayFind` and `arrayFindIndex` also use every index at lines 773-805. Re-check current intended spec behavior for these before changing them.

Evidence from `packages/zts/src/object.zig`:

- `getIndex` returns null when an element slot contains `undefined` at lines 2255-2275.
- Length and allocated backing are explicitly decoupled for sparse arrays at lines 2278-2282.

The executor must be careful not to confuse a hole with an explicitly stored `undefined` value if the runtime can represent that distinction.

## Scope

In scope:

- `packages/zts/src/builtins/array.zig`
- `packages/zts/src/object.zig` only if a presence helper is needed.
- Regression tests for sparse arrays.

Out of scope:

- Reworking dense array storage.
- Changing non-callback array methods unless a helper requires a minimal shared adjustment.
- Broad ECMAScript conformance expansion.

## Steps

1. Add or reuse an explicit array-element presence check.

   If `getIndex` cannot distinguish holes from explicit `undefined`, add a small helper with documented behavior rather than overloading `getIndex` further.

2. Apply per-method sparse semantics:

   - `map`: invoke callback only for present elements; preserve output length and holes.
   - `filter`: invoke callback only for present elements; compact kept values.
   - `reduce`: without an initial value, start at the first present element; throw on an empty all-hole array.
   - `forEach`, `every`, `some`: skip holes.
   - `find` and `findIndex`: verify the project's intended/current JS subset behavior before changing; do not update these merely by analogy.

3. Add focused tests. Good cases:

   - callback count for a sparse array with length greater than populated slots.
   - `map` preserves length without manufacturing a dense `undefined` element.
   - `filter` compacts present values only.
   - `reduce` with a leading hole uses the first present value as the accumulator.
   - explicit `undefined` remains visitable if the object model supports it.

4. Run:

   ```sh
   zig build test-zts --summary all
   zig build test --summary all
   git diff --check
   ```

## Done Criteria

- Sparse holes are skipped for callback methods that require present elements.
- Explicit `undefined` is not accidentally treated as a hole if the representation can distinguish it.
- Regression tests cover both sparse holes and normal dense arrays.
- Engine and aggregate tests pass.

## STOP Conditions

- The object model cannot represent hole vs explicit `undefined` without a storage change larger than this plan.
- Current parser syntax cannot create sparse arrays and direct object construction would be too invasive for a focused test.
- Changing `find` or `findIndex` requires a policy decision about this engine's JavaScript subset.
