# zigttp Codebase Audit - Pass 2 (2026-02-07)

Date: 2026-02-07
Scope: `zts/` engine layer and `src/` server layer
Predecessor: `docs/codebase-audit-2026-02.md` (Pass 1)

## Executive Summary

Pass 1 identified and fixed critical security/correctness issues (C1, H1-H3) and applied Phase 3-4 improvements. This second pass surfaces six new findings not covered in the prior audit, validates the status of remaining open items from Pass 1, and produces a unified implementation plan covering all outstanding work.

The most significant new finding is **N1**: rope string comparison uses function-local static mutable state, making it thread-unsafe and silently truncating deep ropes. This is a correctness bug that can produce wrong equality results for concatenated strings exceeding 64 leaves.

## Prior Audit Status Summary

### Completed (Pass 1)

| ID | Finding | Severity | Resolution |
|----|---------|----------|------------|
| C1 | Unsafe builtins object extraction (`getObject` accepted any pointer) | Critical | `getObject` now requires `isObject()` (`zts/builtins.zig`) |
| H1 | Static file serving broken on threaded backend | High | Real sync static response path implemented (`src/server.zig`) |
| H2 | Response status uses unchecked integer casts | High | Centralized `parseHttpStatusOrDefault` with clamping (`zts/http.zig`) |
| H3 | JSON `writeInt` overflows on i32 min value (-2147483648) | High | Widened to i64 before negation (`zts/http.zig`) |
| M5 | Symlink escape in static file serving | Medium | Canonical-path validation added (`src/server.zig`) |
| -- | Interpreter test memory leak | -- | Fixed setup in `zts/interpreter.zig` |
| -- | Stripper test noise | -- | Suppressible logging added (`zts/stripper.zig`) |
| -- | Object teardown arena/double-destroy | -- | Hardened (`zts/object.zig`) |

### Still Open (Pass 1)

| ID | Finding | Severity | Status | Notes |
|----|---------|----------|--------|-------|
| M1 | JIT PIC fast path checks 4 entries vs interpreter's 8 | Medium | Open | See unified plan Phase 2 |
| M2 | Duplicated sync/evented request handling code paths | Medium | Open | Evented path is dead code; sync path is sole active path |
| M3 | Hidden-class migration incomplete (two systems active) | Medium | Open | Index-based pool + legacy pointer-based classes coexist |
| M4 | Thread-local runtime cache disabled | Medium | Open | `acquireWithCache`/`releaseWithCache` bypass TLS and delegate directly to pool |
| L1 | Comment/implementation drift on PIC sizing | Low | Open | Comments say "4 entries" while `PIC_ENTRIES = 8` |
| L2 | Documentation drift on backend behavior | Low | Open | |

## New Findings

### N1. Rope string comparison is thread-unsafe and silently truncates (HIGH)

**Files**: `zts/value.zig:666-691`

`collectLeaves()` uses a nested struct with `var` (function-local static mutable) fields to accumulate rope tree leaves:

```zig
const LeafArray = struct {
    var leaves: [max_leaves]*const JSString = undefined;  // static mutable
    var count: usize = 0;                                 // static mutable
    // ...
};
```

Two problems:

1. **Thread safety**: In Zig, `var` declarations inside a struct nested within a function are effectively static mutable state - they persist across calls and are shared across threads. If multiple threads compare rope strings concurrently (possible during GC marking, concurrent request handling, or any future multi-threaded path), they will corrupt each other's leaf buffers. This is undefined behavior.

2. **Silent truncation**: The `add()` function silently drops leaves when `count >= max_leaves` (64). A rope built from 65+ concatenations will produce incorrect equality results because the comparison will not examine all leaf data.

**Severity justification**: Hybrid arena mode means GC rarely runs in production, reducing the thread-safety risk. However, the silent truncation bug is reachable in single-threaded mode whenever user code builds strings through heavy concatenation (e.g., building HTML or JSON responses via repeated `+`). Incorrect string equality is a correctness violation.

**Fix**: Replace the static mutable buffer with a stack-allocated `std.BoundedArray` passed as a parameter, or take an allocator for dynamic collection. The 64-leaf limit should either produce an error (fallback to full string materialization) or be removed entirely via allocator-backed collection.

### N2. GC sweep leaks memory when heap_ptr is null (MEDIUM)

**Files**: `zts/gc.zig:235-241`

In `processUnmarkedWord()`, when an unmarked (garbage) object needs to be freed:

```zig
if (heap_ptr) |h| {
    h.freeRaw(obj);
} else {
    std.log.warn("GC sweep: cannot free object without heap reference (memory leak)", .{});
}
```

When `heap_ptr` is null, the object is removed from tracking (`object_index`, `objects`) and subtracted from `allocated`, but its actual memory is never freed. This is a programming error path - `majorGC` should always have a heap reference - but if it occurs, objects leak permanently with only a log warning.

**Severity justification**: Hybrid mode disables GC in production, so this path is unreachable in normal FaaS operation. Risk is elevated if GC is re-enabled for development, testing, or future long-running server modes.

**Fix**: Assert that `heap_ptr` is non-null at GC init time (store the heap reference in the GC struct during initialization so it cannot be null during sweep). Alternatively, convert the null case to `@panic()` or `unreachable` in debug builds to surface the programming error immediately.

### N3. Incomplete builtin implementations (MEDIUM)

Multiple features have TODO stubs that silently produce incorrect or no-op results when user code exercises them:

| Location | Feature | Behavior When Hit |
|----------|---------|-------------------|
| `zts/parser/codegen.zig:1591` | Rest element in object destructuring (`const {a, ...rest} = obj`) | Empty block - rest variable is never assigned |
| `zts/parser/codegen.zig:1664` | Rest element in array destructuring (`const [a, ...rest] = arr`) | Empty block - rest variable is never assigned |
| `zts/builtins.zig:2085` | `Array.prototype.sort(compareFn)` with custom comparator | Comparator ignored, default string sort used |
| `zts/builtins.zig:797` | `new Map(iterable)` constructor from iterable | Iterable argument silently ignored, empty map returned |
| `zts/builtins.zig:1001` | `new Set(iterable)` constructor from iterable | Iterable argument silently ignored, empty set returned |
| `zts/builtins.zig:3559, 3566, 3576` | `Result.map()`, `Result.mapErr()`, `Result.match()` | Returns self/inner value without applying callback |
| `zts/builtins.zig:3767` | `new WeakMap(iterable)` constructor from iterable | Iterable argument silently ignored |
| `zts/builtins.zig:3888` | `new WeakSet(iterable)` constructor from iterable | Iterable argument silently ignored |
| `zts/interpreter.zig:2273` | Tail call optimization | Falls through to regular call (safe but stack-consuming) |
| `zts/interpreter.zig:4419` | String/object coercion in abstract equality | Returns false instead of coercing |

**Severity justification**: Each individual TODO is low-to-medium severity. The aggregate risk is medium because FaaS handlers that use any of these patterns will get silently wrong results with no error message. The rest element destructuring and custom sort comparator are the most likely to be hit in typical handler code.

**Fix**: Prioritize by usage likelihood. Rest element destructuring and custom sort comparators are common JavaScript patterns. Map/Set iterable construction is moderately common. Result methods are zigttp-specific and can be deferred. For each, either implement the feature or emit a clear unsupported-feature error at parse/compile time (matching the existing feature detection pattern).

### N4. Dead code inventory - approximately 5,585 lines removable (LOW)

| Component | File(s) | Lines | Evidence |
|-----------|---------|-------|----------|
| Optimized JIT tier | `zts/jit/optimized.zig` | 2,084 | Tier promotion disabled; confirmed dead in memory notes and audit Pass 1 |
| AIR modules | `zts/air/*.zig` (7 files) | 3,501 | Exploratory/unused; no integration with compilation pipeline |
| Thread-local cache functions | `zts/pool.zig:310-356` | ~50 | Functions defined but bypassed; `acquireWithCache` delegates directly to `pool.acquire()` |

Total: approximately 5,635 lines. Removing these reduces compilation time (the JIT and AIR modules are substantial), binary size, and cognitive load during code review.

**Note**: The AIR line count (3,501) is higher than the plan estimate (2,500) based on actual `wc -l` measurement.

### N5. Test coverage gaps (LOW-MEDIUM)

Areas with no test coverage that would expose known issues:

| Gap | Would Expose | Suggested Test |
|-----|-------------|----------------|
| Deep rope tree equality | N1 (silent truncation) | Build rope with 100+ concat operations, compare for equality |
| GC sweep with null heap_ptr | N2 (memory leak) | Call majorGC without heap reference in debug build |
| Rest element destructuring | N3 (silent no-op) | Destructure with rest element, verify rest variable contents |
| Custom sort comparator | N3 (comparator ignored) | Sort array with reverse comparator, verify order |
| Map/Set constructor from array | N3 (iterable ignored) | `new Map([[1,2]])`, verify map has entry |
| Property access at binary search boundary | Boundary correctness | Object with exactly 8 properties, verify lookup correctness |
| HTTP parser malformed input | Robustness | Send truncated headers, oversized headers, null bytes |
| Concurrent string operations | Thread safety | Multiple threads comparing/hashing same rope strings |

### N6. Tail call optimization TODO in interpreter (LOW)

**File**: `zts/interpreter.zig:2273`

The `tail_call` opcode falls through to a regular call:

```zig
.tail_call => {
    const argc: u8 = self.pc[0];
    self.pc += 1;
    // TODO: Implement proper tail call optimization
    try self.doCall(argc, false);
    return .handled;
},
```

For FaaS handlers using recursive patterns (recursive JSON tree walking, recursive list processing), this means each recursive call consumes a stack frame. The `MAX_STATE_DEPTH = 1024` limit at `interpreter.zig:848` is enforced and returns `error.CallStackOverflow`, so this is safe (no actual stack overflow/crash), but it limits recursion depth.

**Severity justification**: Low. FaaS handlers typically process bounded request data. The 1024 frame limit is generous for most use cases. TCO would be a performance/capability enhancement, not a correctness fix.

## Unified Implementation Plan

This plan covers all remaining work from both Pass 1 and Pass 2, ordered by impact.

### Phase 1: Correctness Fixes (N1, N2)

| Task | Files | Description | Effort | Verification |
|------|-------|-------------|--------|--------------|
| Fix rope `collectLeaves` thread safety and truncation | `zts/value.zig:666-705` | Replace static mutable `LeafArray` with stack-allocated bounded array passed as parameter. Handle overflow by falling back to full string materialization and byte-by-byte comparison. | 0.5 day | `zig build test-zts`; add test for rope with 100+ leaves |
| Harden GC sweep `heap_ptr` requirement | `zts/gc.zig:235-241` | Store heap reference in GC struct at init. Assert non-null during sweep. Convert null branch to `unreachable` in debug. | 0.25 day | `zig build test-zts`; verify assertion fires in debug mode |

### Phase 2: Performance (M1, M4)

| Task | Files | Description | Effort | Verification |
|------|-------|-------------|--------|--------------|
| Harmonize JIT PIC depth with interpreter | `zts/jit/baseline.zig:90`, `zts/interpreter.zig:259-266` | Either increase `PIC_CHECK_COUNT` to 8 (matching `PIC_ENTRIES`) or benchmark to find optimal check count and update both constant and comments. Fix comment drift (L1): lines 260-261 and 265-266 say "4" but constant is 8. | 1-2 days | `zig build test-zts`; `zig build bench` for regression |
| Decide: re-enable or remove TLS cache | `zts/pool.zig:305-356` | Option A: Remove dead TLS cache code (~50 lines). Option B: Make `in_use` atomic and re-enable with stress test validation. Recommendation: remove (Option A) since FaaS model is single-request-per-instance. | 0.5-1 day | `zig build test-zts`; stress test under concurrent load |

### Phase 3: Code Quality (M2, M3, N4, L1, L2)

| Task | Files | Description | Effort | Verification |
|------|-------|-------------|--------|--------------|
| Remove dead evented I/O code path | `src/server.zig` | `useEventedBackend()` always returns false. Remove the evented code path entirely (M2) to eliminate duplicated request handling. Keep only the sync/threaded path. | 1-2 days | `zig build test` |
| Complete hidden-class migration | `zts/object.zig`, `zts/context.zig` | Remove legacy pointer-based `HiddenClass` initialization/deinitialization. Migrate all references to index-based `HiddenClassPool`. | 3-5 days | `zig build test-zts`; property access microbenchmark |
| Remove dead code | `zts/jit/optimized.zig` (2,084 lines), `zts/air/*.zig` (3,501 lines), `zts/pool.zig:310-356` (~50 lines) | Delete files/functions that are confirmed dead. Update `build.zig` and module imports. | 0.5-1 day | `zig build test` compiles and passes |
| Fix documentation drift | `docs/`, `zts/interpreter.zig:259-266` | Update PIC comments to match `PIC_ENTRIES = 8`. Update architecture/performance docs to reflect that evented I/O is removed and sync is the sole path. | 0.5 day | Manual review |

### Phase 4: Feature Completeness (N3, selected items)

Prioritized by likelihood of use in FaaS handlers:

| Task | Files | Description | Effort | Verification |
|------|-------|-------------|--------|--------------|
| Implement rest element destructuring | `zts/parser/codegen.zig:1590-1593, 1663-1666` | Object rest: create new object with remaining properties. Array rest: slice remaining elements into new array. | 1-2 days | `zig build test-zts`; add destructuring tests |
| Implement `Array.sort` custom comparator | `zts/builtins.zig:2084-2087` | Invoke user-provided comparison function during sort. Requires callback infrastructure (function invocation from native builtins). | 1 day | `zig build test-zts`; sort with reverse comparator |
| Implement Map/Set/WeakMap/WeakSet iterable constructors | `zts/builtins.zig:797, 1001, 3767, 3888` | Iterate over argument array and call set/add for each entry. | 1-2 days | `zig build test-zts`; construct from array literal |
| Implement Result callback methods | `zts/builtins.zig:3559-3578` | `map`, `mapErr`, `match` need to invoke user callbacks. Lower priority - zigttp-specific API, can be deferred. | 1 day | `zig build test-zts` |

### Phase 5: Test Hardening (N5)

| Task | Files | Description | Effort | Verification |
|------|-------|-------------|--------|--------------|
| Add rope tree stress tests | `zts/value.zig` | Build ropes exceeding former 64-leaf limit, verify equality. Test asymmetric ropes (deep left, shallow right). | 0.5 day | `zig build test-zts` |
| Add property access boundary tests | `zts/object.zig` | Objects with exactly 7, 8, 9 properties to exercise linear/binary search boundary (`BINARY_SEARCH_THRESHOLD = 8`). | 0.5 day | `zig build test-zts` |
| Add HTTP parser edge case tests | `src/server.zig` | Truncated headers, oversized headers, null bytes in header values, missing Content-Length with body. | 1 day | `zig build test` |
| Add feature-gap regression tests | `zts/builtins.zig`, `zts/parser/codegen.zig` | Tests for each N3 item to prevent silent regression if stubs are accidentally removed. | 0.5 day | `zig build test-zts` |

## Verification Commands (all phases)

```bash
zig build test           # All src/ tests
zig build test-zts       # Engine tests
zig build test-zruntime  # Runtime tests
zig build bench          # Performance regression check
```

## Constraints

All changes must preserve:

- Handler function signature (`function handler(req) { return Response... }`)
- Response helper API (`Response.json`, `Response.text`, `Response.html`, `Response.redirect`)
- CLI flags (`-p`, `-h`, `-e`, `-m`, `-n`, `--cors`, `--static`)
- Compatibility with `examples/` directory
- Cold start performance targets
- No conflict with OOP removal plan (`docs/oop-removal-plan.md`)
- No conflict with TS-to-Zig transpilation roadmap (`docs/ts-to-zig-transpile.md`)
