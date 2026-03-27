# zigttp Performance TODO

> Canonical performance backlog and benchmark gate for hot-path work.
> Code changes land in this repo. Comparative benchmark source of truth lives in `../zigttp-bench`.
> Seeded from repo analysis on 2026-03-27.

## Hot-Path Code Analysis

### Interpreter Dispatch and Property Access
- [ ] **JIT PIC coverage is narrower than interpreter PIC state** — `zts/interpreter.zig:259-337`, `zts/jit/baseline.zig:82-91`.
  - Current state: interpreter PIC tracks 8 entries with `last_hit`; baseline JIT probes only 4 entries via `PIC_CHECK_COUNT`.
  - Risk: moderately polymorphic shapes can miss the generated fast path earlier than the runtime cache design suggests.
  - Follow-up: benchmark 4 vs 8 entry probe depth, or make probe depth adaptive by code size budget.

### JIT Tiering and Call Path
- [ ] **Whole-function type specialization is still missing** — `zts/type_feedback.zig:156-168`, `zts/interpreter.zig:405-415`, `zts/jit/root.zig:1-28`, `zts/jit/optimized.zig:1-13`.
  - Current state: type feedback tracks value sites and call sites, but not function-entry argument profiles.
  - Current optimized tier is loop-scoped; there is no compiler path for function-wide argument guards and unboxed locals.
  - Priority: P0.
- [ ] **Generic call overhead still dominates non-inlined hot call sites** — `zts/interpreter.zig:3760-3825`, `zts/jit/baseline.zig:3277-3575`.
  - Current state: monomorphic guarded calls and inlining exist, but broader user-call traffic still falls back to generic helper-heavy paths.
  - Target benchmarks: `functionCalls`, JIT `call.js`.

### String Path
- [ ] **Rope hashing still flattens concat ropes** — `zts/string.zig:330-344`.
  - Current state: repeated concat is O(1), but `RopeNode.getHash()` flattens the rope to compute hash.
  - Risk: handlers that build strings and then use them as property keys / map keys / cache keys still pay flatten cost.
- [ ] **String access helpers eagerly flatten ropes on content reads** — `zts/builtins/helpers.zig:133-167`.
  - Current state: flattening is cached, but hot builtin paths can still force flattening earlier than necessary.
  - Follow-up: preserve rope form through more builtin operations or add piecewise implementations where it pays off.
- [ ] **`concat_n` in JIT still routes through a helper** — `zts/jit/baseline.zig:3932-3952`.
  - Current state: correctness is good and helper already does single-allocation concat, but the JIT side still pays call overhead.
  - Target benchmarks: `stringBuild`, `httpHandler`, `httpHandlerHeavy`.

### Builtins and Numeric Paths
- [ ] **Builtin fast dispatch is narrow compared to the remaining benchmark gap** — `zts/object.zig:783-809`, `zts/interpreter.zig`, `zts/builtins/number.zig:340-430`.
  - Current state: hot builtin IDs exist for `Math.*`, string methods, and `parseInt`, but non-trivial builtin traffic still goes through generic dispatch/marshaling.
  - Target benchmarks: `mathOps`, `parseInt`, `functionCalls`.

### Runtime / Pool / Server Hot Path
- [ ] **Pool contention path must stay regression-free while JIT work lands** — `zts/pool.zig:222-291`, `src/zruntime.zig:4300-4365`.
  - Current state: `free_hint`, spin-first retries, and jittered backoff already keep the request path lean.
  - Constraint: any perf work touching runtime scheduling, pool reuse, or request setup needs explicit HTTP and cold-start regression checks.

### Architecture Asymmetry
- [ ] **Some JIT fast paths remain ARM64-biased** — `zts/jit/baseline.zig:4983-5095`.
  - Current state: range-iterator `for...of` fast path is ARM64-focused; x86-64 still falls back to helper code there.
  - Follow-up: review x86-64 parity for every helper-backed hot path before declaring JIT perf work complete.

## Priority Backlog

## P0
- [ ] **3.2: JIT whole-function type specialization, SMI-first** — `zts/type_feedback.zig`, `zts/interpreter.zig`, `zts/jit/root.zig`, new `zts/jit/whole_function.zig`.
  - Goal: add a new optimized-tier compiler path for monomorphic integer-heavy functions, not a rewrite of baseline JIT.
  - Scope: functions whose bodies stay within a supported integer-safe subset and whose formal arguments are monomorphic `.smi`.
  - Supported v1 subset: local loads/stores, integer constants, integer arithmetic, bitwise ops, integer comparisons, simple branches, `ret`, and trivial stack movement around them.
  - Explicitly out of scope in v1: closures/upvalues, generators, async, generic calls, property specialization, spread/module ops, exception paths, mixed-type bodies.
  - Compiler strategy:
    - add function-entry argument feedback,
    - emit one entry guard per specialized argument,
    - unbox args once in the prologue,
    - keep locals unboxed for the compiled function body,
    - rebox on return,
    - deopt on type mismatch or overflow through existing `jitDeoptimize`.
  - Tiering:
    - keep `CompilationTier.optimized` as the public tier label,
    - in `tryCompileOptimized`, attempt whole-function specialization first,
    - on ineligible/unsupported, fall back to the existing loop-optimized compiler,
    - on optimized deopt, demote back to baseline as today.
  - Done when:
    - eligible leaf and branchy integer functions compile through the new path,
    - type mismatch and overflow deopt correctly,
    - unsupported functions transparently stay on existing paths,
    - correctness holds across interpreter, baseline JIT, and optimized JIT.

- [ ] **Fix JIT crash on 9+ if-else branches** — `../zigttp-bench/docs/pending_zigttp_fixes.md`, `../zigttp-bench/CLAUDE.md:125-142`.
  - Problem: functions with 9+ branches crash after warmup under JIT, blocking realistic control-flow and `dynamicProps` parity.
  - Likely area: large control-flow lowering, label patching, or register allocation pressure in JIT codegen.
  - Done when:
    - the documented reproducer no longer crashes,
    - `dynamicProps` can be restored to the full branch count in `../zigttp-bench`,
    - dedicated JIT and filtered microbench runs stay stable for repeated runs.

## P1
- [ ] **String hot-path follow-up after rope introduction** — `zts/string.zig`, `zts/builtins/helpers.zig`, `zts/jit/baseline.zig`.
  - Problem: `stringBuild` and `stringOps` are still well behind Deno in the isolated benchmark suite.
  - Focus:
    - incremental rope hashing instead of flatten-for-hash,
    - fewer eager flatten points in builtin/string helpers,
    - review whether `concat_2` / `concat_n` can avoid helper overhead in hot JIT cases,
    - validate rope depth heuristics against real microbench workloads.
  - Done when: median improvement is visible on `stringBuild` and `stringOps` without regressing `httpHandler*`.

- [ ] **Builtin and call dispatch overhead reduction** — `zts/object.zig`, `zts/interpreter.zig`, `zts/builtins/number.zig`, `zts/jit/baseline.zig`.
  - Problem: `functionCalls`, `mathOps`, and `parseInt` still show a large throughput gap despite targeted fast paths.
  - Focus:
    - widen direct builtin dispatch coverage,
    - add a tighter radix-10 `parseInt` path,
    - reduce argument marshaling overhead for hot leaf calls,
    - let whole-function specialization pick up integer-heavy helper-free leaf functions.
  - Done when: those three benchmarks improve together rather than one-off synthetic wins.

- [ ] **Property access and dynamic access follow-up** — `zts/interpreter.zig`, `zts/jit/baseline.zig`, `zts/object.zig`, `../zigttp-bench/zigttp-work-items.md`.
  - Problem: property-heavy benchmarks are better than before, but dynamic object access is still constrained by missing `obj[key]` support and JIT/polymorphism limits.
  - Focus:
    - evaluate widening PIC probe depth or making it adaptive,
    - add bracket notation support for object dynamic access,
    - review megamorphic fallback behavior and cost,
    - keep `monoProperty` / `monoPropertyWrite` as regression sentinels.
  - Done when: `dynamicProps` becomes idiomatic again and polymorphic property access does not regress monomorphic fast paths.

## P2
- [ ] **Dev-mode type-directed codegen parity** — `docs/performance.md:146-157`, `tools/precompile.zig:1056-1165`, `zts/parser/codegen.zig`.
  - Problem: type-directed specialized opcodes are wired for precompiled handlers, but dev mode still uses more generic bytecode.
  - Focus: thread BoolChecker-derived type maps through the dev-mode codegen path so local `zig build run` behavior is closer to precompiled behavior.
  - Done when: numeric-heavy dev-mode scripts emit the same specialized opcode family where semantics allow.

- [ ] **x86-64 JIT fast-path parity** — `zts/jit/baseline.zig`.
  - Problem: some hot paths still have ARM64-only inline fast paths with x86-64 helper fallbacks.
  - Focus: parity-review helper-backed JIT paths and implement x86-64 equivalents when code size and maintenance cost are justified.
  - Done when: architecture-specific perf gaps are explained by hardware, not missing compiler paths.

- [ ] **Cold-start follow-up: Linux static target / JIT cross-compilation unblock** — `README.md:615-619`, `docs/performance.md:76-86`.
  - Problem: the planned Linux cold-start target remains blocked on JIT cross-compilation issues.
  - Scope: keep separate from throughput hot-path work.
  - Done when: ReleaseFast Linux targets with JIT support can be built and benchmarked as a first-class production path.

## Benchmark-Driven Work Still Pending from `../zigttp-bench`

- [ ] **Reconcile current isolated benchmark numbers with historical “optimization complete” claims** — `../zigttp-bench/docs/optimization-complete.md`, `../zigttp-bench/CLAUDE.md:148-170`.
  - Current stance: treat `optimization-complete.md` as historical context, not proof that no further tuning is needed.
  - Use current isolated-process numbers as the backlog baseline.

- [ ] **Keep the remaining benchmark backlog visible even where partial fixes exist** — `../zigttp-bench/zigttp-work-items.md`.
  - Carry forward:
    - string concatenation perf,
    - builtin dispatch overhead,
    - dynamic property access / bracket notation,
    - long if-else chain stability.
  - Do not remove an item from this file until the benchmark workaround is removed or the benchmark gap is explicitly accepted.

## Verification / Benchmark Gates

### Repo Checks
- [ ] `zig build test-zts`
- [ ] `zig build test`

### Dedicated JIT Runs
- [ ] `cd ../zigttp-bench && ./zigttp-benchmarks/jit/run.sh`

### Filtered Microbench Runs
- [ ] `cd ../zigttp-bench && deno run -A bench.ts micro zigttp --filter=arithmetic,functionCalls,mathOps,stringBuild,stringOps,propertyAccess,dynamicProps,parseInt`

### Safety Checks
- [ ] `cd ../zigttp-bench && deno run -A bench.ts http zigttp --http-mode=floor --connections=1`
- [ ] `cd ../zigttp-bench && deno run -A bench.ts cold zigttp --iterations=20`

### Acceptance Rules for New Perf Work
- [ ] Every new perf item should name:
  - affected hot path,
  - intended benchmark subset,
  - explicit “done when” criteria,
  - non-regression checks required before closing.
- [ ] Hot-path JIT work should not be called done without at least 3 repeated benchmark runs in `../zigttp-bench`.
- [ ] If a change improves one synthetic benchmark but regresses HTTP floor or cold start beyond noise, keep the item open and document the trade-off here.
