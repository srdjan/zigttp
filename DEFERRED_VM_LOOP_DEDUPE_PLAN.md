# Deferred VM Loop Dedupe Plan

## Status

The semantic-dedupe refactor is **deferred**. Step 0 below (extending the opcode
parity harness) was the one test-only exception cleared to start ahead of the
gates; it has **landed** (2026-06-07, uncommitted) - see the note under Step 0,
including a fault-representation divergence it surfaced.

Do not start Steps 1-4 until every precondition gate is green on `main`. Status
as of this refresh (2026-06-07), verified against source - re-check at execution
time:

- [x] Phase 0: GC correctness fixes + `test-zigts`/`test-server`/bench gates
      wired (commit `adaf6e0`).
- [ ] Track A1: engine facade is strict - `packages/runtime/src/engine_facade.zig`
      is the only runtime entry into the engine, `packages/zigts/src/runtime_contract.zig`
      exposes stable public types, and the direct reaches into engine internals
      from `zruntime.zig` are gone. (Today only `engine_adapter.zig` exists.)
- [ ] Track B1: production table stakes land and their tests go green - the
      `error.SkipZigTest` placeholders in `packages/runtime/src/server_test.zig`
      for per-request deadline, graceful shutdown, `/_health`, `/_readiness`,
      panic isolation, and the memory cap are replaced by passing tests.
- [ ] Track B2 + measurement gates: binary-size CI gate (fail on >2% drift) and
      perf receipts that actually measure req/s, cold-start, and RSS (not just
      the latency corpus), with `bench-check` CI-blocking.

See [Improvement Plan](IMPROVEMENT_PLAN.md) for the tracks these gates belong to.

## Problem

Opcode semantics are implemented across three execution tiers:

- interpreter dispatch in `packages/zigts/src/interpreter.zig` (`dispatch` at
  `:211`, a Zig switch lowered to computed-goto)
- baseline JIT lowering in `packages/zigts/src/jit/baseline.zig` (`compileOpcode`
  switch at `:1259`, emits native code per opcode)
- optimized JIT lowering in `packages/zigts/src/jit/optimized.zig` (loop-focused
  unboxed fast paths; no full opcode switch)

That duplication can let supported-subset behavior diverge between tiers. The
risk is real - the parity gate already caught a baseline-JIT i32-overflow
wraparound that the interpreter handled correctly - but it is not a production
FaaS table-stakes item. It waits until the runtime boundary is stable enough that
this refactor does not compete with deadline, shutdown, readiness, logging,
panic-isolation, memory-cap, and engine-facade work.

### Duplication taxonomy (current state)

- **Already shared (leave alone):** comparison primitives (`interpreter/cmp.zig`),
  the PIC struct and its lookup/update policy (`interpreter/ic.zig`), value
  representation (`value.zig`), and the structural opcode table
  (`bytecode.zig` `getOpcodeInfo`).
- **Logic-duplicated (the real targets):** the `context.zig` JIT helpers
  (`jitAdd` at `:1368`, `jitSub` `:1396`, `jitMul` `:1410`, `jitDiv` `:1439`,
  `jitMod` `:1448`, `jitLooseEquals` `:1543`, `jitCompare*` `:1558+`)
  re-implement the semantics that `interpreter/arith.zig`, `interpreter/cmp.zig`,
  and `interpreter/util.zig` already express as pure functions, instead of
  delegating to them.
- **Inlined per tier (parity-only for now):** integer overflow->float handling,
  NaN-box tag guards, and `toInt32`-based bitwise/shift logic are open-coded in
  interpreter dispatch and again in baseline codegen, and a third unboxed form in
  the optimized loop body.

## What already exists (reuse, do not rebuild)

- **Parity harness:** `packages/zigts/src/tests/opcode_parity.zig` (anchored in
  `root.zig:234`). 15 cases, three tiers, deterministic tier-forcing,
  `sameValue` NaN-box-aware comparison.
- **Tier-forcing API:** `interpreter/jit_policy.zig` +
  `bytecode.CompilationTier`. Force interpreted with maxed thresholds; force
  baseline with `.eager` + threshold 1; force optimized by warming type feedback
  then calling `jit_compile.tryCompileOptimized`.
- **Opcode table:** `bytecode.zig` `Opcode` enum (`:37`) + `OpcodeInfo` /
  `getOpcodeInfo` (`:234`) - the structural source of truth all tiers consume.
- **Pure interpreter helpers:** `interpreter/cmp.zig`, `interpreter/arith.zig`,
  `interpreter/util.zig`.
- **Shared PIC:** `interpreter/ic.zig` `PolymorphicInlineCache` (`:58`), plus the
  `jitGetFieldIC` helper-call path the baseline JIT already uses.

## Scope

- Keep public handler behavior unchanged.
- Keep JIT dispatch direct. Do not introduce runtime vtables or function-pointer
  dispatch for opcode semantics.
- Preserve the deliberately excluded JS features listed in the improvement plan.
- Migrate one opcode family at a time: arithmetic first, then comparison,
  property access, and calls.
- The optimized tier is loop-focused and has no full opcode switch; its coverage
  for the property-access and calls families is conditional, and monomorphic call
  inlining in the optimized tier is itself deferred (see `IMPROVEMENT_PLAN.md`
  Deferred Work). Treat calls as the riskiest, last family.

## Plan

### Step 0 - Extend the parity harness (LANDED 2026-06-07, test-only)

Extend `packages/zigts/src/tests/opcode_parity.zig` beyond arithmetic /
comparison / overflow to cover property access (`get_field`/`put_field` and the
`_ic` variants), calls (`call`/`call_method`), and failure/diagnostic paths,
asserting identical results **and** identical diagnostics across interpreter,
baseline JIT, and optimized JIT. Reuse the existing tier-forcing and `sameValue`
machinery. Property-access and call cases need a constructed object/closure in
the corpus, so extend the `Case` builder accordingly (the current corpus is
restricted to numeric/boolean results for a documented soundness reason - widen
it carefully). This tightens the net before any refactor touches the tiers and
is safe to land independently of the deferred gates.

**Landed.** The harness now also covers property access (`get_put_field` and
`get_put_field_ic`, written-then-read-back via the `.length` atom), calls (a
runtime-built closure invoked through `push_const`/`call` - the callee lives in
`constants[0]` because `make_closure`/`make_function` would bail baseline), and
two failure paths (`add` of two `undefined`s -> `TypeError`; `call` on a
non-callable -> `NotCallable`), each across interpreter, baseline JIT, and
optimized-when-reached. `zig build test-zigts` green (1218 pass, 1 skip).
Test-only; no engine source changed.

**Finding (the standalone bug — boundary half now fixed).** The tiers represent
runtime faults differently. The interpreter returns an `error` from `run()` and
aborts; the JIT signals a fault by setting `ctx.exception` and returning the
`exception_val` sentinel (`jitCall` at `interpreter/jit_intrinsics.zig:32-35`,
the `jitAdd/jitSub/...` family in `context.zig`), and it does **not** poll
`ctx.exception` in straight-line code, so it pushes the sentinel and keeps
executing subsequent opcodes.

*Boundary fix (landed).* Both `cc.execute` boundaries in `interpreter/frame.zig`
(`run` and `callBytecodeFunction`) now reconcile the two representations: after
the compiled call they check `ctx.hasException()` and return
`error.NativeFunctionError` — the same Zig error the interpreter fallback
returns — instead of handing back a post-fault value. This makes the
cross-frame path converge: `doCall`'s `try` now aborts the enclosing frame on a
JIT-callee fault exactly as it does for an interpreted callee, and top-level JIT
faults route through the same `error.HandlerError` → 500 path the interpreter
already uses (verified no regression: `test-zruntime` green). Guarded by
`opcode_parity.zig` → "a fault makes run() return an error on every tier, not a
post-fault value" (a `fault-op; push_i8 99; ret` body: pre-fix the JIT returned
`99`).

*Residual (still for this refactor).* The boundary fix does not stop the
**single innermost compiled function** from running its own opcodes between the
fault point and its `ret` — the compiled native code has already executed them
before returning. Side-effecting tail opcodes (a `cacheSet`/`fetch`/`log` with
operands independent of the poisoned stack) can still fire on the JIT tier and
not the interpreter. Eliminating that needs a per-fallible-call exception check
emitted inside `jit/baseline.zig` and `jit/optimized.zig` (x86 + arm64) that
bails to a shared sentinel-returning epilogue — perf-sensitive, and exactly the
N-way emitter duplication this dedupe exists to collapse, so it belongs here.

### Step 1 - Collapse the logic-duplicated JIT helpers (deferred)

Make the `context.zig` JIT helpers (`jitAdd`/`jitSub`/`jitMul`/`jitDiv`/`jitMod`,
`jitLooseEquals`, `jitCompare*`) delegate to the existing pure helpers in
`interpreter/arith.zig`, `interpreter/cmp.zig`, and `interpreter/util.zig` instead
of re-implementing the semantics. Keep the helpers small enough to inline on the
JIT helper-call path. This removes the highest-value duplication first with the
lowest blast radius, because the baseline JIT already calls these helpers for
cold paths.

### Step 2 - Promote the opcode table to a semantic source (deferred)

Extend the existing `bytecode.zig` `OpcodeInfo` table with a per-opcode semantic
descriptor for the migrated families. The table is the source of truth; each tier
still emits direct code - interpreter dispatch, baseline native codegen, and the
optimized unboxed loop body - generated from or checked against the descriptor.
Do not add a parallel table.

### Step 3 - Bring the inline PIC codegen under one policy (deferred)

The PIC struct and lookup policy in `interpreter/ic.zig` are already shared, and
the baseline JIT already consults them via the `jitGetFieldIC` helper-call path.
Bring the baseline inline-probe codegen (`emitGetFieldIC` at `baseline.zig:4311`)
under parity coverage so it cannot drift from the shared policy. The helper-call
path stays available until parity and perf gates pass.

### Step 4 - Roll out by opcode family (deferred)

Arithmetic, then comparison, then property access, then calls. After each family:
run the parity harness and the benchmarks before touching the next family. Calls
last, because the optimized tier's coverage there is partial and call inlining is
separately deferred.

## Verification

- `zig build test-zigts` (includes the parity harness).
- Opcode parity harness green across interpreter, baseline JIT, and optimized JIT
  for every migrated family, including diagnostics parity.
- `zig build bench` and `zig build bench-check`: no regression against the
  binary-size and performance-receipt gates from the improvement plan.
- Run tests per-target, not the combined step (the combined step can stall in the
  test-runner IPC).

## Exit Criteria

- One semantic source feeds all three tiers for the migrated opcode families.
- Tier-specific code remains direct-dispatch and benchmark-neutral.
- Any unsupported subset behavior stays rejected at the same boundary as before.
- Rollback remains simple: one opcode family can return to separate tier code
  without reverting unrelated families.
