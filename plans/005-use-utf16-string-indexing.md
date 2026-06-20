# Use UTF-16 String Indexing

Planned at commit: `f2c637d` (`refactor(runtime): group self-extract payload params into PayloadInput`)
Planned on: 2026-06-20

## Drift Check

Before editing, verify the in-scope files have not changed since this plan was written:

```sh
git diff --name-only f2c637d -- packages/zigts/src/builtins/string_builtins.zig packages/zigts/src/string.zig
```

Expected output for direct execution from the planned commit is empty. If either path appears, re-open the string helpers and method implementations before editing.

## Status

- Priority: P2
- Effort: M
- Risk: MEDIUM
- Confidence: HIGH for `length`/`slice`/`substring`, MEDIUM for lone-surrogate `charAt`
- Category: JavaScript semantics
- Depends on: `plans/001-restore-format-baseline.md`

## Why

JavaScript string indexes and `.length` are based on UTF-16 code units, not UTF-8 bytes. Current string methods already contain a UTF-16 helper for `charCodeAt`, but `length`, `charAt`, `slice`, and `substring` still use byte indexes. Non-ASCII handlers can observe wrong lengths or invalid byte slices.

## Current State

Evidence from `packages/zigts/src/builtins/string_builtins.zig`:

- `utf16CodeUnitAt` exists and powers `charCodeAt` at lines 14-34 and 71-89.
- `stringLength` returns `data.len` bytes, or rope `total_len`, at lines 36-47.
- `stringCharAt` slices `data[idx .. idx + 1]` at lines 50-69.
- `stringSlice` computes defaults and clamps against `data.len`, then slices bytes at lines 218-288.
- `stringSubstring` also clamps and slices by bytes at lines 290-362.

Evidence from `packages/zigts/src/string.zig`:

- `JSString.length()` is documented as bytes at lines 44-46.
- `codepointLength()` exists, but JavaScript needs UTF-16 code-unit length, not Unicode scalar count, at lines 62-68.

## Scope

In scope:

- `packages/zigts/src/builtins/string_builtins.zig`
- `packages/zigts/src/string.zig` only if a shared helper belongs there.
- Regression tests for non-ASCII strings.

Out of scope:

- Changing the internal string storage format.
- Full Unicode normalization or locale behavior.
- Reworking `indexOf`/`includes` unless a helper exposes an obviously safe small fix.

## Steps

1. Add small helpers for UTF-16 code-unit operations:

   - count UTF-16 units in UTF-8 data.
   - map a UTF-16 code-unit index/range to byte offsets without splitting UTF-8 codepoints.
   - keep an ASCII fast path.

2. Update:

   - `stringLength` to return UTF-16 code-unit length.
   - `stringSlice` and `stringSubstring` to clamp by UTF-16 code units and slice at valid UTF-8 byte boundaries.
   - `stringCharAt` to use UTF-16 indexing.

3. Handle astral `charAt` explicitly.

   JavaScript can return a one-code-unit string containing a lone surrogate. If the current UTF-8 string representation cannot represent that safely, stop and record the design choice instead of returning invalid UTF-8 or a silent replacement character.

4. Add tests:

   - `"é".length` is `1`, not `2`.
   - `"😀".length` is `2`.
   - `charCodeAt` remains correct for BMP and astral strings.
   - `slice`/`substring` do not cut through a UTF-8 sequence.
   - `charAt` behavior for astral input is either correct or explicitly covered by a documented limitation.

5. Run:

   ```sh
   zig build test-zigts --summary all
   zig build test --summary all
   git diff --check
   ```

## Done Criteria

- UTF-16 code-unit indexing is used consistently for the scoped string methods.
- Non-ASCII regression tests fail on the planned-at implementation and pass after the change.
- No invalid UTF-8 strings are produced.
- Engine and aggregate tests pass.

## STOP Conditions

- Correct `charAt` requires representing lone UTF-16 surrogates and the current string model cannot do that safely.
- A fix would require changing string storage or GC layout.
- Existing docs intentionally define this subset as byte-indexed strings.
