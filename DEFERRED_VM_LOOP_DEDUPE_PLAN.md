# Deferred VM Loop Dedupe Plan

## Status

Deferred. Do not start this work until the production FaaS table stakes, engine
facade refactor, and measurement gates in [Improvement Plan](IMPROVEMENT_PLAN.md)
are green on `main`.

## Problem

Opcode semantics are implemented across three execution tiers:

- interpreter dispatch in `packages/zigts/src/interpreter.zig`
- baseline JIT lowering in `packages/zigts/src/jit/baseline.zig`
- optimized JIT lowering in `packages/zigts/src/jit/optimized.zig`

That duplication can let supported-subset behavior diverge between tiers. The
risk is real, but it is not a production FaaS table-stakes item. It should wait
until the runtime boundary is stable enough that this refactor does not compete
with deadline, shutdown, readiness, logging, panic-isolation, memory-cap, and
engine-facade work.

## Scope

- Keep public handler behavior unchanged.
- Keep JIT dispatch direct. Do not introduce runtime vtables or function-pointer
  dispatch for opcode semantics.
- Preserve the deliberately excluded JS features listed in the improvement plan.
- Migrate one opcode family at a time: arithmetic first, then comparison,
  property access, and calls.

## Plan

1. Add an opcode parity harness under the existing `test-zigts` flow. Drive the
   same corpus through interpreter, baseline JIT, and optimized JIT, then assert
   identical results and diagnostics for mixed-type arithmetic, comparison,
   property access, calls, and failure paths.
2. Extract pure semantic helpers for arithmetic, comparison, and overflow. Keep
   the helpers small enough for the compiler and JIT lowering paths to inline.
3. Add a comptime opcode-semantics table that generates direct tier-specific
   code. The table is the source of truth; each tier still emits direct code.
4. Factor polymorphic-inline-cache handling into a shared helper consulted by the
   interpreter and JIT. The current JIT helper-call path should remain available
   until parity and perf gates pass.
5. Roll out by opcode family. After each family, run parity tests and benchmarks
   before touching the next family.

## Verification

- `zig build test-zigts`
- opcode parity harness green across interpreter, baseline JIT, and optimized JIT
- `zig build bench`
- no regression against the binary-size and performance receipt gates from the
  improvement plan

## Exit Criteria

- One semantic source feeds all three tiers for the migrated opcode families.
- Tier-specific code remains direct-dispatch and benchmark-neutral.
- Any unsupported subset behavior stays rejected at the same boundary as before.
- Rollback remains simple: one opcode family can return to separate tier code
  without reverting unrelated families.
