# Plan 012: Make malformed bytecode-cache tag bytes a catchable error, not a panic

> **Executor instructions**: Follow this plan step by step. Run every verification command and confirm the expected result before moving on. Touch only the files listed as in scope. If any STOP condition occurs, stop and report; do not improvise around it. When done, update the status row for this plan in `plans/README.md`, unless a reviewer says they maintain the index.
>
> **Drift check, run first**: `git diff --name-only a4a731bd -- packages/zts/src/bytecode_cache.zig packages/runtime/src/runtime_pool.zig`
> Empty means no drift. If any path appears, re-open it and compare against the Current state excerpts before editing.

## Status

- **Priority**: P1
- **Effort**: S/M
- **Risk**: LOW
- **Depends on**: none
- **Category**: bug (availability / robustness)
- **Planned at**: commit `a4a731bd` and 2026-07-02

## Why this matters

`bytecode_cache.zig`'s deserializer decodes on-disk/embedded tag bytes directly into exhaustive enums via raw `@enumFromInt`, with no range check. In Debug/ReleaseSafe builds this panics; in ReleaseFast it is undefined behavior. The embedded/self-extracting-binary fast path in `runtime_pool.zig` (`loadHandlerCached`) calls this deserializer with a bare `try` and no fallback, so any corrupted or version-skewed embedded bytecode payload (bit rot, a self-extracting binary trailer built by a different `zts` version than the one running it) crashes every request on that binary instead of failing gracefully. The sibling dev-cache path four lines below already handles exactly this failure class: it wraps the same deserialize call in a `catch` that logs, disables the cache, and falls back to recompiling from source. This plan brings the enum decode itself to a catchable error and gives the embedded path the same graceful-degradation the dev path already has.

## Current state

- `packages/zts/src/bytecode_cache.zig:32-38` — `ConstantTag` (exhaustive `enum(u8)`, values 0-4).
- `packages/zts/src/bytecode_cache.zig:41-47` — `SpecialCode` (exhaustive `enum(u8)`, values 0-4).
- `packages/zts/src/bytecode.zig:564-568` — `PatternType` (exhaustive `enum(u2)`, values 0-2).
- Raw decode sites in `bytecode_cache.zig` (verify exact line numbers first — they may have shifted):
  - `:247` — `const tag: ConstantTag = @enumFromInt(tag_byte);` inside `deserializeConstant`.
  - `:283` — `const code: SpecialCode = @enumFromInt(code_byte);`.
  - `:386` — `pattern.pattern_type = @enumFromInt(try reader.readByte());`.
  - `:389` — `pattern.url_atom = @enumFromInt(url_atom_raw);` — check the declared type of `url_atom` before including this site. If it resolves to a non-exhaustive enum (has a trailing `_,` variant, as `object.ClassId` does elsewhere in this codebase), `@enumFromInt` on it cannot panic and this site is out of scope; only include it if its enum is exhaustive.
- `packages/zts/src/bytecode_cache.zig:217-221` — existing `DeserializeError` set: `error{ EndOfStream, OutOfMemory, IncompleteRead }`.
- `packages/runtime/src/runtime_pool.zig:920-936` — `loadHandlerCached`'s embedded fast path: `try rt.loadFromCachedBytecode(entry_bytecode);` (line 934), no catch.
- `packages/runtime/src/runtime_pool.zig:948-963` — the dev-cache sibling path's existing fallback pattern to model the fix after:
  ```zig
  if (self.cache.getRaw(key)) |cached_data| {
      rt.loadFromCachedBytecode(cached_data) catch |err| {
          std.log.warn(
              "bytecode cache deserialize failed for {s}: {}; recompiling from source",
              .{ self.handler_filename, err },
          );
          self.cache_disabled.store(true, .monotonic);
          self.cache.clear();
          slow_path_recovered = true;
      };
      if (!slow_path_recovered) return;
  }
  ```

## Commands you will need

| Purpose | Command | Expected on success |
|---|---|---|
| Build | `zig build` | exit 0 |
| zts tests (bytecode_cache) | `zig build test-zts` | success |
| ZRuntime tests (runtime_pool) | `zig build test-zruntime` | success |
| Format gate | `zig fmt --check build.zig packages/` | no output |
| Full local gate | `bash scripts/verify.sh` | exit 0 |

## Scope

**In scope, the only files/directories to modify:**

- `packages/zts/src/bytecode_cache.zig` — replace the raw `@enumFromInt` at the confirmed exhaustive-enum sites with a bounds-checked decode that returns a new `DeserializeError` member (e.g. `error.InvalidTag`) instead of panicking/UB on an out-of-range byte.
- `packages/runtime/src/runtime_pool.zig` — give the embedded-bytecode fast path in `loadHandlerCached` (around line 934) a `catch` fallback. Exact recovery behavior needs a decision: the embedded path has no "source to recompile from" the way the dev path does (there is no `self.handler_code` guarantee at deploy time) — see Step 2 before assuming the dev path's exact recovery shape transfers directly.

**Out of scope:**

- The other `@enumFromInt` sites already found sound in this file (`736`, `1284`, `1360`, `1429`, `1539` — these decode `bytecode.Opcode` or `object.Atom` from values the *compiler itself* just wrote in the same process, or from a non-exhaustive enum; do not touch them without new evidence they are unsafe).
- `self_extract.zig`'s trailer-level parsing (bounds checks, overflow-safe field reads) — already sound, not this plan's concern. This plan is about the bytecode *content* the trailer points to, not the trailer format itself.
- Any change to the on-wire serialization format (tag byte values, ordering) — this is a decode-robustness fix, not a format change.

## Git/workflow guidance

- Branch: work on `main`.
- Commit style: Conventional Commits, e.g. `fix(zts): make malformed bytecode-cache tag bytes a catchable error`.
- Do not push or open a PR unless the operator asks.

## Steps

### Step 1: Add a bounds-checked enum decode helper

Add a small helper in `bytecode_cache.zig`, e.g. `fn decodeTag(comptime E: type, raw: u8) !E`, that uses `std.meta.intToEnum(E, raw)` (which already returns `error.InvalidEnumTag` for out-of-range values on exhaustive enums) instead of `@enumFromInt`. Map `std.meta.intToEnum`'s error to a `DeserializeError` member (add `InvalidTag` to the `DeserializeError` set at `:217-221`, or reuse `std.meta.IntToEnumError` directly if `DeserializeError` can absorb it cleanly — check how `SerializeError`/`DeserializeError` are declared and composed elsewhere in this file first, and match that pattern rather than inventing a new one). Apply it at the three (or four, per the `url_atom` check above) confirmed exhaustive-enum decode sites.

**Verify**: `zig build test-zts` -> success. Add a test that feeds an out-of-range tag byte into `deserializeConstant` (or the lowest-level function you changed) and asserts it returns an error rather than panicking — model after the existing `test "validateCache detects corruption"` (`bytecode_cache.zig:1011`).

### Step 2: Decide and implement the embedded-path fallback

The dev-cache path recovers by recompiling from source (`self.handler_code`). The embedded/self-extracting-binary path (`loadHandlerCached`, `runtime_pool.zig:920-936`) may not have handler source available at all when running from a deployed artifact with only precompiled bytecode. Before writing the fallback:

- Check whether `self.handler_code` (or an equivalent source string) is actually populated in the embedded-bytecode configuration. If yes, mirror the dev path's `catch` exactly (log, disable cache if applicable, recompile).
- If source is not available in this configuration, the only safe fallback is to fail the request/handler-load cleanly (return a typed error up to the caller, which should already have error handling above `loadHandlerCached` — check `runtime_pool.zig` callers) rather than crash the process. Confirm the caller surfaces this as a clean 5xx/startup-failure rather than an unhandled panic, and do not invent a recompile path that does not have the inputs to succeed.

Implement whichever of the two applies, matching this codebase's existing logging style (`std.log.warn`/`std.log.err` with the pattern seen at `runtime_pool.zig:953-956`).

**Verify**: `zig build test-zruntime` -> success. Add a test exercising `loadHandlerCached`'s embedded path with a deliberately corrupted `entry_bytecode` payload (flip a tag byte in a valid serialized sample) and assert it returns a clean error instead of panicking.

## Test plan

- New test in `bytecode_cache.zig`: malformed tag byte at each of the three (or four) decode sites returns an error, not a panic. Use `std.testing.expectError`.
- New test in `runtime_pool.zig` (or wherever `loadHandlerCached`'s embedded path is most directly testable): corrupted embedded bytecode is handled per the Step 2 decision, proven to fail (panic) on the pre-fix code path — temporarily revert Step 1 locally to confirm the red-proof, then reapply.
- Final: `bash scripts/verify.sh` -> exit 0.

## Done criteria

All must hold:

- [ ] All confirmed exhaustive-enum `@enumFromInt` decode sites in `bytecode_cache.zig`'s deserialization path return a catchable error on out-of-range input instead of panicking/UB.
- [ ] The embedded-bytecode fast path in `runtime_pool.zig` no longer takes down the process on a malformed/corrupted payload; it fails the specific handler load/request cleanly (or recompiles, if source is genuinely available — see Step 2).
- [ ] `zig build test-zts test-zruntime` exit 0.
- [ ] `bash scripts/verify.sh` exit 0.
- [ ] New tests prove both the decode-level error return and the embedded-path fallback, with the panic/crash reproduced pre-fix.
- [ ] No files outside the in-scope list are modified.
- [ ] `plans/README.md` status row updated.

## STOP conditions

Stop and report if:

- Current-state excerpts do not match the live code (re-check line numbers first).
- `url_atom`'s type turns out to be exhaustive and there's a reason (found during Step 1) that its safe range differs from the other three sites in a way this plan's uniform helper can't express — report the shape rather than special-casing silently.
- The embedded path genuinely has no recovery option (no source, and the caller cannot cleanly fail this one handler without crashing the whole server) — this would mean the availability gap is structural, not a decode-layer fix, and needs a design decision before continuing.
- A step's verification fails twice after reasonable local correction.

## Maintenance notes

- If a future format version adds new tag values, the bounds-checked decode helper means an old binary reading a newer cache format now fails cleanly instead of panicking — worth calling out in any future format-versioning work as already covered by this fix.
- Reviewers should check that the `DeserializeError` addition doesn't silently widen what `SerializeError`/other call sites are expected to handle in ways that break exhaustiveness elsewhere (grep for `catch` sites on functions whose error set this touches).
