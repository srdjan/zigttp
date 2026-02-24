# Performance Guide

Performance characteristics, benchmarks, optimizations, and deployment patterns for zigttp-server.

## Benchmarks

### JavaScript Engine Performance (QuickJS baseline)

zts outperforms QuickJS in historical benchmark runs (QuickJS as external baseline). See the [zigttp-bench](https://github.com/srdjan/zigttp-bench) repository for raw results and scripts.

| Benchmark      | zts         | QuickJS    | Ratio           |
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
./zig-out/bin/zigttp-server handler.js
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
./zig-out/bin/zigttp-server  # No handler file argument needed
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

**Shape Preallocation** (`zts/context.zig:352-434`): HTTP Request and Response objects use preallocated hidden class shapes, eliminating transitions. Direct slot writes via `setSlot()` bypass property lookup.

Shapes:
- Request: method, url, path, query, body, headers (6 props)
- Response: body, status, statusText, ok, headers (5 props)
- Response headers: content-type, content-length, cache-control (3 props)
- Request headers: authorization, content-type, accept, host, user-agent, accept-encoding, connection (7 props)

**Polymorphic Inline Cache (PIC)** (`zts/interpreter.zig:259-335`): 8-entry cache per property access site with last-hit optimization. O(1) monomorphic lookups, megamorphic transition after 9th distinct shape.

**Binary Search for Large Objects** (`zts/object.zig:751, 831-835`): Objects with 8+ properties use binary search on sorted property arrays (`BINARY_SEARCH_THRESHOLD = 8`).

**JIT Baseline IC Integration** (`zts/jit/baseline.zig:1604-1765`): x86-64 and ARM64 fast paths check PIC entry[0] inline, falling back to helper on miss.

**JIT Object Literal Shapes** (`zts/context.zig:746-779`, `zts/jit/baseline.zig:3646-3670`): Object literals with static keys use pre-compiled hidden class shapes. `new_object_literal` creates objects with the final hidden class directly, and `set_slot` writes inline without lookup. Fast path uses arena bump allocation.

**Type Feedback** (`zts/type_feedback.zig`): Call site and value type profiling for JIT decisions. Inlining threshold lowered to 5 calls (from 10) for faster FaaS warmup.

### String Optimizations

**Lazy String Hashing** (`zts/string.zig:18-24, 44-54`): Hash deferred until needed via `hash_computed` flag. Reduces overhead for strings never used as hash keys.

**Pre-interned HTTP Atoms** (`zts/object.zig:237-264`): 27 common headers with O(1) lookup: content-type, content-length, accept, host, user-agent, authorization, cache-control, CORS headers, connection, accept-encoding, cookie, x-forwarded-for, x-request-id, content-encoding, transfer-encoding, vary.

**HTTP String Cache** (`zts/context.zig:111-135, 462+`): Pre-allocated status texts, content-type strings, and HTTP method strings.

### Pool and Request Optimizations

**Pool Slot Hint** (`zts/pool.zig`): `free_hint` atomic reduces slot acquisition from O(N) to O(1).

**Relaxed Atomic Ordering** (`src/zruntime.zig`): `in_use` counter uses `.monotonic` ordering (metrics only, not synchronization).

**LRU Static Cache** (`src/server.zig`): Doubly-linked list LRU eviction instead of clear-all, eliminating latency spikes.

**Adaptive Backoff** (`src/zruntime.zig`): Three-phase pool contention handling:
- Phase 1: 10 spin iterations using `spinLoopHint`
- Phase 2: Sleep 10us-1ms with jitter (prevents thundering herd)
- Phase 3: Circuit breaker fails fast after 100 retries

**Zero-Copy Response** (`src/zruntime.zig`): Borrowed mode avoids memcpy when arena lifetime is guaranteed.

### Build-Time Precompilation

**Handler Precompilation** (`tools/precompile.zig`, `build.zig`): `-Dhandler=<path>` compiles handlers at build time. Bytecode embedded in binary, eliminating runtime parsing.

Build flow: `precompile.zig` compiles handler, serializes bytecode with atoms and shapes, generates `src/generated/embedded_handler.zig`. Server loads via `loadFromCachedBytecode()`.

## Memory Management

### Hybrid Arena Allocation

For request-scoped workloads, zts uses a hybrid memory model:

- **Arena allocator**: O(1) bulk reset between requests, zero per-object overhead
- **Escape detection**: Write barriers prevent arena objects from leaking to persistent storage
- **GC disabled in hybrid mode**: No collection pauses during request handling

### Memory Configuration

```bash
# Default (0 = no limit)
./zig-out/bin/zigttp-server handler.js

# Set explicit limit (1MB)
./zig-out/bin/zigttp-server -m 1m handler.js

# Smaller limit (64KB)
./zig-out/bin/zigttp-server -m 64k handler.js
```

## Deployment Patterns

### Single Instance (Lambda-style)

```bash
./zigttp-server handler.js
```

Each instance handles one request at a time for isolation.

### Container Deployment

```dockerfile
FROM scratch
COPY zig-out/bin/zigttp-server /zigttp-server
COPY handler.js /handler.js
EXPOSE 8080
ENTRYPOINT ["/zigttp-server", "-q", "-h", "0.0.0.0", "/handler.js"]
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
