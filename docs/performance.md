# Performance Guide

Performance characteristics, benchmarks, optimizations, and deployment patterns for zigttp.

## Benchmarks

### JavaScript Engine Performance (QuickJS baseline)

zigts outperforms QuickJS in historical benchmark runs (QuickJS as external baseline). See the [zigttp-bench](https://github.com/srdjan/zigttp-bench) repository for raw results and scripts.

| Benchmark      | zigts         | QuickJS    | Ratio           |
| -------------- | ----------- | ---------- | --------------- |
| stringOps      | 16.3M ops/s | 258K ops/s | **63x faster**  |
| objectCreate   | 8.1M ops/s  | 1.7M ops/s | **4.8x faster** |
| propertyAccess | 13.2M ops/s | 3.4M ops/s | **3.9x faster** |
| httpHandler    | 1.0M ops/s  | 332K ops/s | **3.1x faster** |
| functionCalls  | 12.4M ops/s | 5.1M ops/s | **2.4x faster** |
| stringConcat   | 8.3M ops/s  | 6.2M ops/s | **1.3x faster** |
| arrayOps       | 8.7M ops/s  | 6.6M ops/s | **1.3x faster** |
| jsonOps        | 77K ops/s   | 71K ops/s  | **1.1x faster** |

Run Zig-native benchmarks with: `zig build bench`

### HTTP Throughput (Deno baseline)

| Endpoint         | zigttp RPS | Deno Baseline | Ratio |
| ---------------- | ---------- | ------------- | ----- |
| /api/health      | 79,743     | 104,672       | 0.76x |
| /api/echo        | 79,409     | 63,726        | 1.25x |
| /api/greet/world | 80,030     | 105,016       | 0.76x |

JIT compilation is enabled by default for hot functions (after `JIT_THRESHOLD` executions).

## Cold Start Performance

### Baseline (Runtime Handler Loading)

```bash
zig build -Doptimize=ReleaseFast
./zig-out/bin/zigttp serve handler.js
```

Cold start: ~83ms (process spawn to first HTTP response)

Breakdown:
- Process spawn: 10-15ms (kernel overhead)
- Dynamic linker (dyld): 15-20ms (system library loading)
- I/O backend init: 5ms (kqueue/io_uring setup)
- Handler parsing: 8ms
- Handler compilation: 4ms
- Bytecode caching: 1ms
- Connection pool: 5ms
- Network listener: 3ms

### Optimized (Embedded Bytecode)

```bash
zig build -Doptimize=ReleaseFast -Dhandler=handler.js
./zig-out/bin/zigttp serve  # No handler file argument needed
```

Cold start: ~71ms (16% improvement). Parsing, compilation, and caching steps are eliminated entirely - bytecode loads from memory.

### Platform Comparison

| Configuration | Cold Start | Notes |
|--------------|------------|-------|
| macOS baseline | ~83ms | Runtime parsing |
| macOS optimized | ~71ms | Embedded bytecode |
| macOS profiled (flamegraph) | ~103ms | dyld dominates (80-90ms) |
| Deno 2.6.7 | 150-200ms | V8 snapshot deserialization |

Runtime initialization itself is only ~3ms. The macOS dynamic linker (dyld) accounts for 80-90ms and is unavoidable.

### Linux Target (Planned)

Static linking with musl libc would eliminate the dynamic linker overhead:

| Phase | macOS | Linux Static (estimated) |
|-------|-------|--------------------------|
| dyld/ld.so | 80-90ms | 5-10ms |
| Runtime init | 3ms | 3ms |
| Network + HTTP | 10-20ms | 10-20ms |
| **Total** | **103ms** | **18-33ms** |

Blocked on JIT cross-compilation architecture issues.

## FaaS Optimizations

### Warm Invocations

HandlerPool reuses pre-warmed contexts for subsequent requests:

- O(1) pool slot acquisition via `free_hint` atomic
- Zero runtime parsing overhead (bytecode already compiled)
- Minimal GC overhead via hybrid arena allocation
- Predictable latency (no JIT compilation jitter)

## Request Pipeline Optimizations

### Property Access Optimizations

**Shape Preallocation** (`packages/zigts/src/context.zig:352-434`): HTTP Request and Response objects use preallocated hidden class shapes, eliminating transitions. Direct slot writes via `setSlot()` bypass property lookup.

Shapes:
- Request: method, url, path, query, body, headers (6 props)
- Response: body, status, statusText, ok, headers (5 props)
- Response headers: content-type, content-length, cache-control (3 props)
- Request headers: authorization, content-type, accept, host, user-agent, accept-encoding, connection (7 props)

**Polymorphic Inline Cache (PIC)** (`packages/zigts/src/interpreter.zig:259-335`): 8-entry cache per property access site with last-hit optimization. O(1) monomorphic lookups, megamorphic transition after 9th distinct shape.

**Binary Search for Large Objects** (`packages/zigts/src/object.zig:751, 831-835`): Objects with 8+ properties use binary search on sorted property arrays (`BINARY_SEARCH_THRESHOLD = 8`).

**JIT Baseline IC Integration** (`packages/zigts/src/jit/baseline.zig:1604-1765`): x86-64 and ARM64 fast paths check PIC entry[0] inline, falling back to helper on miss.

**JIT Object Literal Shapes** (`packages/zigts/src/context.zig:746-779`, `packages/zigts/src/jit/baseline.zig:3646-3670`): Object literals with static keys use pre-compiled hidden class shapes. `new_object_literal` creates objects with the final hidden class directly, and `set_slot` writes inline without lookup. Fast path uses arena bump allocation.

**Type Feedback** (`packages/zigts/src/type_feedback.zig`): Call site and value type profiling for JIT decisions. Inlining threshold lowered to 5 calls (from 10) for faster FaaS warmup.

### String Optimizations

**Lazy String Hashing** (`packages/zigts/src/string.zig:18-24, 44-54`): Hash deferred until needed via `hash_computed` flag. Reduces overhead for strings never used as hash keys.

**Pre-interned HTTP Atoms** (`packages/zigts/src/object.zig:237-264`): 27 common headers with O(1) lookup: content-type, content-length, accept, host, user-agent, authorization, cache-control, CORS headers, connection, accept-encoding, cookie, x-forwarded-for, x-request-id, content-encoding, transfer-encoding, vary.

**HTTP String Cache** (`packages/zigts/src/context.zig:111-135, 462+`): Pre-allocated status texts, content-type strings, and HTTP method strings.

### Proof-Driven Response Caching

When the compile-time contract proves a handler is `pure` or `deterministic`+`read_only`, the runtime caches GET/HEAD responses by request hash (method + URL including query string). Cache hits return the memoized response directly from Zig memory without acquiring a runtime or entering JS.

- **Activation**: automatic when contract proves the required properties. No configuration needed.
- **Cache key**: Wyhash of method + URL (path + query string).
- **Thread safety**: RwLock allows concurrent cache reads; writes take an exclusive lock.
- **Eviction**: FIFO with configurable capacity (default 1024 entries).
- **TTL**: configurable per-entry expiry (default 5 minutes). Expired entries are lazily evicted.
- **Max body**: responses larger than 256KB (default) are not cached.
- **Visibility**: cached responses carry `X-Zigttp-Proof-Cache: hit` header.
- **Implementation**: `packages/runtime/src/proof_adapter.zig`, integrated into `packages/runtime/src/server.zig` on both threaded and evented I/O paths.

### Pool and Request Optimizations

**Pool Slot Hint** (`packages/zigts/src/pool.zig`): `free_hint` atomic reduces slot acquisition from O(N) to O(1).

**Relaxed Atomic Ordering** (`packages/runtime/src/zruntime.zig`): `in_use` counter uses `.monotonic` ordering (metrics only, not synchronization).

**LRU Static Cache** (`packages/runtime/src/server.zig`): Doubly-linked list LRU eviction instead of clear-all, eliminating latency spikes.

**Adaptive Backoff** (`packages/runtime/src/zruntime.zig`): Three-phase pool contention handling:
- Phase 1: 10 spin iterations using `spinLoopHint`
- Phase 2: Sleep 10us-1ms with jitter (prevents thundering herd)
- Phase 3: Circuit breaker fails fast after 100 retries

**Zero-Copy Response** (`packages/runtime/src/zruntime.zig`): Borrowed mode avoids memcpy when arena lifetime is guaranteed.

### Type-Directed Code Generation

When the BoolChecker can prove both operands of an arithmetic or comparison are numbers, it populates a `NodeTypeMap`. The CodeGen reads this map and emits specialized opcodes that skip runtime type dispatch:

| Specialized Opcode | Replaces | Benefit |
|-------------------|----------|---------|
| `add_num` | `add` | Skips string-concatenation check |
| `sub_num` | `sub` | Skips type coercion |
| `mul_num` | `mul` | Skips type coercion |
| `div_num` | `div` | Skips type coercion |
| `lt_num`, `gt_num`, `lte_num`, `gte_num` | `lt`, `gt`, `lte`, `gte` | Skip polymorphic comparison |
| `concat_2` | `add` (string case) | Dedicated string concatenation |

These opcodes also omit type feedback recording, providing faster cold-start execution before JIT warmup. Type-directed codegen is active in precompiled handlers (`-Dhandler`). Dev mode (`zig build run`) uses generic opcodes because BoolChecker type annotations are not wired to the dev-mode CodeGen path.

### Build-Time Precompilation

**Handler Precompilation** (`packages/tools/src/precompile.zig`, `build.zig`): `-Dhandler=<path>` compiles handlers at build time. Bytecode embedded in binary, eliminating runtime parsing.

Build flow: `precompile.zig` compiles handler, serializes bytecode with atoms and shapes, generates `src/generated/embedded_handler.zig`. Server loads via `loadFromCachedBytecode()`.

The precompile pipeline also supports:
- `-Dverify` - compile-time handler verification (see [verification.md](verification.md))
- `-Dcontract` - emit contract.json with handler properties and proven capabilities
- `zigttp deploy` - one-command cross-compile, package, push, and provision via the zigttp control plane
- `-Dtest-file=tests.jsonl` - run declarative handler tests at build time
- `-Dreplay=traces.jsonl` - replay-verify recorded traces before embedding
- `-Dgenerate-tests=true` - exhaustive path enumeration and fault coverage analysis

### Concurrent I/O

**Structured concurrency** (`packages/zigts/src/modules/io.zig`, `packages/runtime/src/zruntime.zig`):
`parallel()` and `race()` overlap outbound HTTP without async/await overhead.
The three-phase model (collect descriptors, dispatch to OS threads, join results)
avoids event loop machinery entirely.

Performance characteristics:
- Single fetch: inline execution, zero thread overhead
- Multiple fetches: one OS thread per descriptor, each with its own
  `std.http.Client` and I/O backend - no contention between workers
- Maximum 8 concurrent operations per call
- Thread spawn failure degrades gracefully to sequential inline execution
- The JS heap is never touched from worker threads, so no locking or write
  barriers are needed during the concurrent phase

The latency of a `parallel()` call equals the slowest fetch plus thread
spawn/join overhead (typically under 100us). For a handler making 3 API calls
at 50ms each, `parallel()` reduces total I/O time from ~150ms to ~50ms.

## Memory Management

### Hybrid Arena Allocation

For request-scoped workloads, zigts uses a hybrid memory model:

- **Arena allocator**: O(1) bulk reset between requests, zero per-object overhead
- **Escape detection**: Write barriers prevent arena objects from leaking to persistent storage
- **GC disabled in hybrid mode**: No collection pauses during request handling

### Memory Configuration

```bash
# Default (0 = no limit)
./zig-out/bin/zigttp serve handler.js

# Set explicit limit (1MB)
./zig-out/bin/zigttp serve -m 1m handler.js

# Smaller limit (64KB)
./zig-out/bin/zigttp serve -m 64k handler.js
```

## Deployment Patterns

### Single Instance (Lambda-style)

```bash
./zigttp serve handler.js
```

Each instance handles one request at a time for isolation.

### Container Deployment

```dockerfile
FROM scratch
COPY zig-out/bin/zigttp /zigttp
COPY handler.js /handler.js
EXPOSE 8080
ENTRYPOINT ["/zigttp", "serve", "-q", "-h", "0.0.0.0", "/handler.js"]
```

Container size: ~1.2MB with embedded handler (single binary, no handler file needed).

### FaaS Deployment

```bash
# AWS Lambda (x86-64)
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux

# AWS Lambda (ARM64, recommended for price/performance)
zig build -Doptimize=ReleaseFast -Dtarget=aarch64-linux

# With precompiled handler (fastest cold starts)
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux -Dhandler=handler.js
```

## Benchmarking

Run the Zig-native benchmark suite: `zig build bench`

For HTTP load testing and comparative benchmarks, see the [zigttp-bench](https://github.com/srdjan/zigttp-bench) repository.

Load test with external tools:

```bash
wrk -t4 -c100 -d30s http://localhost:8080/
hey -n 10000 -c 100 http://localhost:8080/
```

## Perf Backlog Report

### 2026-04-22: Perf backlog phases 1-9

Nine-phase plan delivered in-tree; baseline promoted to the post-phase-4 run.

**Aggregate result vs prior committed baseline** (75th-percentile of 7 runs
each, `zig build bench -Doptimize=ReleaseFast -- --json --quiet`):

| Benchmark      | Before    | After     | Ratio   |
| -------------- | --------- | --------- | ------- |
| functionCalls  | 11.42M/s  | 16.68M/s  | 1.461x  |
| stringConcat   | 26.32M/s  | 27.10M/s  | 1.030x  |
| stringOps      | 17.93M/s  | 18.34M/s  | 1.022x  |
| propertyAccess | 17.29M/s  | 17.54M/s  | 1.014x  |
| objectCreate   | 14.48M/s  | 14.55M/s  | 1.005x  |
| recursion      | 3,380/s   | 3,359/s   | 0.994x  |
| arrayOps       | 16.06M/s  | 15.90M/s  | 0.990x  |
| intArithmetic  | 19.45M/s  | 19.18M/s  | 0.986x  |
| jsonOps        | 3.99M/s   | 3.86M/s   | 0.966x  |
| gcPressure     | 8.46M/s   | 8.32M/s   | 0.984x  |
| forOfLoop      | 2.00G/s   | 1.92G/s   | 0.962x  |
| httpHandler    | 6.96M/s   | 7.15M/s   | 1.027x  |
| httpHandlerHeavy | 1.23M/s | 1.25M/s   | 1.014x  |
| **geomean**    |           |           | **1.029x** |

Hardware: Apple Darwin 25.3.0 arm64. Zig 0.16.0-dev.3073+28ae5d415. Commits
`10684c0` through the current baseline snapshot.

**Per-phase highlights.** Phase 1 froze the benchmark JSON at
`schema_version: 1` and added a snapshot test so downstream tooling can
consume it. Phase 2 shipped `scripts/bench-diff.sh` and the
`zig build bench-check` step that compares a best-of-N run against the
committed baseline; the gate trips on any per-bench regression >8% or
geomean regression >3%. Phase 3 added the `drop_goto` superinstruction
(fires 1-3 times per compiled function via `bytecode_opt.zig:tryFuseAt`).
Phase 4 turned `call_ic` from a stub into a fully-wired opcode: the
interpreter records feedback at the call site, baseline and optimized JIT
walk/emit past it, the baseline inliner blacklist no longer rejects it,
and codegen emits it at non-method call sites behind
`enable_call_ic_emission`. A compensating fusion rule folds
`push_const + call_ic` back into `push_const_call` so constant-callee
sites do not regress. The aggregate functionCalls +46% is almost entirely
this phase. Phase 5 added megamorphic recovery to the PIC and aligned PIC
polymorphic capacity with `TypeFeedbackSite` so the two layers report
matching monomorphic/polymorphic/megamorphic classifications. Phase 6
added deopt-storm suppression in `profileFunctionEntry` so functions that
deopt three times in a thousand invocations stop being re-promoted to
the optimized tier. Phases 7 (builtin fastpath v1) and 8 (compiler
allocation tuning) have scaffolding but defer the actual tuning pass to
follow-up work; the compile-time microbench lives at
`packages/runtime/src/compile_benchmark.zig` and runs via
`zig build compile-bench`.

**Configuration and rollback.** Three perf comptime flags carry the
bytecode-emission changes:

- `packages/zigts/src/parser/codegen.zig:63` - `enable_peephole_opt` - default true
- `packages/zigts/src/parser/codegen.zig:68` - `enable_call_ic_emission` - default true
- `packages/zigts/src/interpreter.zig:345` - `pic_entries_tracks_feedback` - default true

Flip any one to false and rebuild to disable the corresponding change. A
master `-Dperf-opts=off` build option is not yet wired; it is the obvious
follow-up if we start needing to A/B flags against one another in CI.
Runtime-policy toggles documented elsewhere in this file remain env-driven
and take effect on restart.

**Protected benches.** `scripts/bench-diff.sh` exempts `forOfLoop`,
`httpHandler`, and `httpHandlerHeavy` from the per-bench regression check.
These are sub-millisecond microbenches whose per-run variance exceeds 5%
even on a quiet machine; they stay in the JSON report and still count
toward geomean, but a single-bench regression on them does not block a
merge. Revisit once the harness runs each bench long enough to push
per-iteration cost comfortably above timer resolution.
