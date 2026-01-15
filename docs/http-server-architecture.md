# HTTP Server Architecture for zts (Performance-FaaS)

This document maps the proposed high-performance architecture to the existing codebase and defines a concrete implementation plan. It focuses on sub-1ms latency for simple handlers, high throughput, and <5ms cold starts, while keeping allocation patterns predictable.

## Goals (Measured)

- Simple handler p50 latency <1ms (warm, no I/O).
- Cold start <5ms (init + first request without parsing JS on the critical path where possible).
- High throughput under concurrency (steady p99 without GC spikes).
- Predictable memory usage with bounded per-request allocations.

## Baseline (Current Code)

- `src/server.zig`: evented IO via `std.Io` on non-Apple targets; threaded IO on Apple platforms. Keep-alive loop with per-connection arena reset. HTTP/1.1 parser uses buffered line reads + batch string storage. Static file cache with LRU and `sendFileAll` streaming for large files.
- `src/zruntime.zig`: lock-free runtime pool (`zts.LockFreePool`) with prewarm, adaptive backoff on acquire, and per-request GC reset. Borrowed-response path is already wired in the server. Hybrid allocation is enabled for request-scoped memory.
- `zts/bytecode_cache.zig`: supports atom-aware serialization/deserialization. `Runtime.loadFromCachedBytecode()` exists, but `HandlerPool` still parses on every runtime init (cache not wired into pool yet).
- `zts/*`: GC, parser, intern pool, and baseline JIT (Phase 11) are present.

## JIT Implementation (Current State)

- **Tiering**: Bytecode functions start in `.interpreted`, move to `.baseline_candidate` at `JIT_THRESHOLD = 100` calls, and compile to `.baseline` if supported. `.optimized` is defined but not implemented.
- **Compiler**: Baseline JIT eliminates interpreter dispatch only (no type specialization). Unsupported opcodes return `UnsupportedOpcode` and the function remains interpreted.
- **Architectures**: x86-64 (SysV ABI) and AArch64 (Apple ABI) emitters. Other architectures are compile errors for JIT.
- **Executable memory**: Per-context `CodeAllocator` (lazy init). macOS uses `MAP_JIT` + `pthread_jit_write_protect_np` for W^X compliance; Linux uses `mprotect` RW↔RX.
- **Semantics**: Compiled code calls `Context` helpers (`jitAdd`, `jitGetField`, `jitCall`, etc.) for full JS semantics. No inline caches in JIT yet.
- **Profiling hooks**: Execution count drives compilation. Back-edge counting (`LOOP_THRESHOLD = 1000`) is tracked but not used for compilation. PIC hit/miss counters are interpreter-only.
- **Controls**: JIT can be disabled with `ZTS_DISABLE_JIT`. Tests can disable via `ZTS_DISABLE_JIT_TESTS` / `disableJitForTests()` (multi-threaded stability caution).

### Baseline JIT Supported Opcodes (compileOpcode switch)

Any opcode not listed below triggers `UnsupportedOpcode` and the function stays interpreted.

| Category | Opcodes |
| --- | --- |
| Stack/constants | `nop`, `push_const`, `push_0`, `push_1`, `push_2`, `push_3`, `push_i8`, `push_null`, `push_undefined`, `push_true`, `push_false`, `dup`, `dup2`, `drop`, `swap`, `rot3` |
| Locals | `get_loc`, `get_loc_0`, `get_loc_1`, `get_loc_2`, `get_loc_3`, `put_loc`, `put_loc_0`, `put_loc_1`, `put_loc_2`, `put_loc_3` |
| Arithmetic | `add`, `sub`, `mul`, `div`, `mod`, `pow`, `neg`, `inc`, `dec` |
| Comparisons | `lt`, `lte`, `gt`, `gte`, `eq`, `neq`, `strict_eq`, `strict_neq` |
| Control flow | `goto`, `if_true`, `if_false`, `loop`, `ret`, `ret_undefined` |
| Objects/fields | `new_object`, `new_array`, `get_field`, `get_field_ic`, `put_field`, `put_field_keep`, `put_field_ic`, `get_elem`, `put_elem`, `get_global`, `put_global`, `get_field_call` |
| Calls | `call`, `call_method`, `tail_call`, `push_const_call` |
| Bitwise/type/logical | `bit_and`, `bit_or`, `bit_xor`, `bit_not`, `shl`, `shr`, `ushr`, `typeof`, `not` |
| Iteration | `for_of_next`, `for_of_next_put_loc` |
| Super/inline ops | `get_loc_add`, `get_loc_get_loc_add`, `if_false_goto`, `add_mod`, `sub_mod`, `mul_mod`, `mod_const`, `mod_const_i8`, `add_const_i8`, `sub_const_i8`, `mul_const_i8`, `lt_const_i8`, `le_const_i8`, `shr_1`, `mul_2` |

### Coverage Gaps (still interpreted)

The following opcodes are present in `zts/bytecode.zig` but not compiled by the baseline JIT today:

- **Stack/fast-path**: `push_i16`, `halt`, `get_length`.
- **Delete/define**: `delete_field`, `delete_elem`, `define_global`.
- **Functions/closures**: `make_function`, `get_upvalue`, `put_upvalue`, `close_upvalue`, `make_closure`.
- **Spread & dynamic call**: `array_spread`, `call_spread`, `call_ic`.
- **Type & modules**: `instanceof`, `import_module`, `import_name`, `import_default`, `export_name`, `export_default`.
- **Async**: `await_val`, `make_async`.

Impact: any function using these opcodes will remain interpreted (even after hitting the JIT threshold), so hot paths should avoid them until baseline coverage expands.

### Performance Counters and Benchmark Expectations

- **Instrumentation**: `zts/perf.zig` provides `CompileStats` and `RuntimeStats`, gated by compile-time flags (`enable_perf_stats`, `enable_opcode_profiling`). Use this for compile-time cost breakdown and runtime instruction/call counts.
- **JIT microbenchmarks**: `benchmarks/jit/` contains focused scripts with a runner (`benchmarks/jit/run.sh`) and recorded baselines (see `benchmarks/jit/baseline-2026-01-13*.txt`). These are machine-specific; compare relative deltas, not absolute numbers.
- **Expected behavior**:
  - Cold start: JIT rarely triggers (threshold 100 calls). Favor bytecode cache for latency.
  - Warm path: tight arithmetic, simple branches, property access, and call-heavy loops should benefit once baseline JIT compiles.
  - Mixed features (closures, modules, async, spread): stay interpreted until baseline coverage expands.

### Common JS Patterns -> Likely Gaps

These patterns *may* emit unsupported opcodes and therefore stay interpreted under the current baseline JIT:

| JS pattern | Likely opcode(s) |
| --- | --- |
| Large integer literal or tight loops with wide immediates | `push_i16` |
| `.length` fast path on arrays/strings/ranges | `get_length` |
| `delete obj.prop` / `delete obj[key]` | `delete_field`, `delete_elem` |
| `let f = function() {}` / `() => {}` / nested functions | `make_function` |
| Closures capturing outer locals | `make_closure`, `get_upvalue`, `put_upvalue`, `close_upvalue` |
| `arr = [...other]` or `[...a, ...b]` | `array_spread` |
| `fn(...args)` / `obj.method(...args)` | `call_spread` |
| Heavily polymorphic call sites (inline call cache) | `call_ic` |
| `value instanceof Ctor` | `instanceof` |
| `async function` / `await` | `make_async`, `await_val` |
| `import` / `export` in modules | `import_module`, `import_name`, `import_default`, `export_name`, `export_default` |

Guidance: For hot FaaS paths, keep handlers simple (no closures/async/spread) and prefer straight-line arithmetic and property access until baseline JIT coverage expands.

### JIT-Friendly Handler Style Guide (FaaS Hot Path)

- Keep handlers **flat**: avoid nested functions and closures for the hot path.
- Avoid `async/await` in latency-sensitive handlers; prefer sync logic or move async work behind feature flags.
- Prefer direct calls (`fn(x)`) over spread (`fn(...args)`).
- Avoid `delete` and `instanceof` in tight loops.
- Use simple loops and straight-line math; avoid large immediate constants that force `push_i16`.
- Prefer property access with fixed keys over highly dynamic access patterns.
- Keep object shapes stable (avoid adding/removing fields after creation).

## Target Architecture

### 1) I/O + Concurrency

- Use evented IO where supported; keep threaded IO as fallback.
- Reactor/worker model with per-core runtime pools to minimize cross-thread contention.
- Prefer per-thread pools and queues; avoid global locks in hot path.

Implementation mapping:
- `src/server.zig`: backend selection by platform already present; add per-thread runtime pool shard and per-reactor `Io.Group`.
- `src/zruntime.zig`: add `HandlerPoolShard` (one per core) with local lock-free queues.

### 2) HTTP Parsing (Zero-Copy, SIMD-aware)

- Use a ring buffer per connection; parse headers and URL as slices into the buffer.
- Only copy the request body if it must outlive the buffer.
- Replace per-line read with a scanner for `\r\n\r\n` using SIMD when available.

Implementation mapping:
- `src/server.zig`: replace `BufferedReader.readLine()` with an incremental parser that uses a fixed ring buffer.
- `zts/http.zig`: reuse or adapt utilities for header parsing if present.

### 3) Runtime Pool + JS Isolation

- Pool size per core, no sharing across cores unless pool is exhausted.
- Borrowed response bodies are sent before resetting runtime to avoid extra copies.
- Prepare for true bytecode cache hits: serialized bytecode + atoms.

Implementation mapping:
- `src/zruntime.zig`: shard pool by core; borrowed-response path is already the server default; wire bytecode cache into `HandlerPool`.
- `zts/bytecode_cache.zig`: atom-aware fast deserialization exists; integrate into runtime initialization.

### 4) Allocation Strategy (Predictable)

- Per-request arena for temporary strings, headers, and JSON parsing.
- Fixed buffer allocator for request parsing and response headers.
- Hybrid allocator for JS runtime with arena + persistent allocator.

Implementation mapping:
- `src/server.zig`: per-request arena already present; add fixed buffer allocator for headers and response templates.
- `src/zruntime.zig`: keep hybrid allocator; add explicit reset hooks for request boundaries.

### 5) Static File Serving

- Small file cache with LRU (existing) + `sendfile` for large files.
- Cache metadata separately for validation; send `ETag` and `Last-Modified`.

Implementation mapping:
- `src/server.zig`: add ETag/Last-Modified and conditional 304 response; prefer `sendfile` when supported.

### 6) GC Tuning (Request Lifecycle)

- Run minor GC based on per-request allocation watermark.
- Track last request size to bias thresholds (already partially implemented).

Implementation mapping:
- `src/zruntime.zig`: expose a request-local GC hint; update thresholds based on request size and runtime memory pressure.
- `zts/gc.zig`: optional API to update nursery thresholds dynamically.

### 7) JIT Strategy (FaaS-Aware)

- Keep baseline JIT as a warm-path accelerator; avoid JIT compilation on cold start unless handler is long-lived.
- Use bytecode cache for cold start, then allow JIT promotion on hot paths (post-first request).
- Expand supported opcode set and use PIC feedback for a future optimized tier.

Implementation mapping:
- `zts/interpreter.zig`: decide whether to allow JIT promotion on first request (configurable).
- `zts/jit/baseline.zig`: expand opcode coverage; add lightweight guards for common hot paths.
- `src/zruntime.zig`: expose JIT policy knobs (disable, threshold override, warmup).

## Concrete Code Patterns

### Comptime lowercase table (fast header normalization)

```zig
const LowerTable = comptime blk: {
    var t: [256]u8 = undefined;
    for (&t, 0..) |*b, i| {
        b.* = if (i >= 'A' and i <= 'Z') @intCast(i + 32) else @intCast(i);
    }
    break :blk t;
};

inline fn lowerByte(b: u8) u8 {
    return LowerTable[b];
}
```

### SIMD header terminator scan

```zig
fn findHeaderEnd(buf: []const u8) ?usize {
    const Vec = @Vector(16, u8);
    const cr = @as(Vec, @splat('\r'));
    const lf = @as(Vec, @splat('\n'));
    var i: usize = 0;
    while (i + 16 <= buf.len) : (i += 16) {
        const chunk = @as(*const Vec, @ptrCast(buf.ptr + i)).*;
        if (@reduce(.Or, chunk == cr) or @reduce(.Or, chunk == lf)) {
            return std.mem.indexOf(u8, buf[i..], "\r\n\r\n") orelse null;
        }
    }
    return std.mem.indexOf(u8, buf[i..], "\r\n\r\n") orelse null;
}
```

### Per-request arena + borrowed response

```zig
var arena = std.heap.ArenaAllocator.init(parent_alloc);
const req_alloc = arena.allocator();
const req = try parseRequest(req_alloc, ...);

var handle = try pool.executeHandlerBorrowed(req);
defer handle.deinit();
try sendResponse(stream, &handle.response);
```

### Lock-free pool shard (concept)

```zig
const Shard = struct {
    pool: HandlerPool,
    // local queue for ready runtimes
};
```

## Patch Plan (Proposed)

1) **HTTP parser overhaul (server)**
   - Add ring-buffer parser; remove per-line string copies.
   - Replace lowercase copy with comptime table.
   - Add SIMD scan for header terminator.
   - Files: `src/server.zig`.

2) **Borrowed response default**
   - Already in place: server uses `executeHandlerBorrowed` and releases after send.
   - Audit edge cases (error paths) to ensure runtime reset happens after response send.
   - Files: `src/server.zig`, `src/zruntime.zig`.

3) **Pool sharding + fast acquire**
   - Create one `HandlerPool` per core; select by connection/thread.
   - Keep global overflow pool if shard is exhausted.
   - Files: `src/server.zig`, `src/zruntime.zig`.

4) **Bytecode cache true hit path**
   - Atom-aware serialization/deserialization exists; wire it into `HandlerPool`.
   - Use `Runtime.loadFromCachedBytecode()` on cache hit.
   - Files: `src/zruntime.zig`, `zts/bytecode_cache.zig`.

5) **Static file improvements**
   - ETag/Last-Modified; handle 304.
   - Prefer `sendfile` for large files.
   - Files: `src/server.zig`.

6) **GC tuning hooks**
   - Add API to set per-request GC thresholds.
   - Adjust nursery size based on request body length.
   - Files: `zts/gc.zig`, `src/zruntime.zig`.

7) **JIT policy + coverage**
   - Add runtime config for JIT enable/threshold (override `JIT_THRESHOLD` per deployment).
   - Defer JIT promotion during cold start; enable after N requests or warmup.
   - Expand baseline opcode coverage and consider PIC-driven optimized tier.
   - Files: `zts/interpreter.zig`, `zts/jit/baseline.zig`, `src/zruntime.zig`.

## Trade-Offs (FaaS-Oriented)

- Aggressive pooling reduces latency but increases resident memory; use lazy growth plus small prewarm.
- Borrowed response bodies eliminate copies but require strict send-before-reset discipline.
- Evented IO yields best tail latency but needs careful backpressure and buffer sizing.

## Implementation Status

The following optimizations from this plan have been implemented:

### Completed

1. **Bytecode Cache Wiring** - `HandlerPool.loadHandlerCached()` now checks bytecode cache first, deserializes with atom remapping on hit, and caches parsed bytecode on miss. Thread-safe with mutex protection.

2. **HTTP Parser Optimizations**:
   - Comptime lowercase table (`LowerTable`) for O(1) per-byte header normalization
   - SIMD header terminator scan utility (`findHeaderEnd`) using `@Vector(16, u8)`
   - Ring buffer integration deferred pending benchmark results

3. **JIT Policy Controls** - `JitPolicy` enum (disabled/lazy/eager) with configurable threshold:
   - Environment variables: `ZTS_JIT_POLICY`, `ZTS_JIT_THRESHOLD`
   - Exposed in `RuntimeConfig` for per-pool configuration
   - Default: lazy mode with threshold=100

4. **Static File ETag/304** - `serveStaticFile()` now:
   - Computes ETag from mtime nanoseconds and file size
   - Checks `If-None-Match` header and returns 304 Not Modified
   - Adds `ETag` header to all 200 responses

5. **GC Tuning Hooks** - New APIs in `zts/gc.zig` and `src/zruntime.zig`:
   - `hintRequestSize(body_len)` - adjusts major GC threshold for large requests
   - `resetRequestHint()` - restores default thresholds
   - `collectIfAbove(watermark)` - conditional minor GC
   - `getNurseryUsage()`, `setMajorGCThreshold()`, `getGCStats()`

### Deferred

- **Pool Sharding** - Per-core runtime pools require significant refactor; current lock-free pool with adaptive backoff performs well under moderate concurrency

## Next Steps

- Benchmark current optimizations under load
- Measure cold start improvement from bytecode cache
- Consider pool sharding if contention becomes measurable bottleneck
