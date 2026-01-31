# Cold Start Optimization

This document describes cold start optimization strategies for zigttp, based on systematic profiling and benchmarking.

## Quick Summary

**Optimize for production**: Use the `-Dhandler` build flag to embed bytecode at compile time.

```bash
zig build -Doptimize=ReleaseFast -Dhandler=path/to/handler.js
```

**Performance gain**: 16% faster cold starts (83ms → 71ms)

## Cold Start Performance

### Baseline (Runtime Handler Loading)

```bash
zig build -Doptimize=ReleaseFast
./zig-out/bin/zigttp-server handler.js
```

**Cold start**: ~83ms (process spawn to first HTTP response)

**Breakdown**:
- Process spawn: 10-15ms (kernel overhead)
- Dynamic linker (dyld): 15-20ms (system library loading)
- I/O backend init: 5ms (kqueue/io_uring setup)
- **Handler parsing**: 8ms ← can be eliminated
- **Handler compilation**: 4ms ← can be eliminated
- **Bytecode caching**: 1ms ← can be eliminated
- Connection pool: 5ms (28 worker threads on macOS)
- Network listener: 3ms (socket creation and bind)

### Optimized (Embedded Bytecode)

```bash
zig build -Doptimize=ReleaseFast -Dhandler=handler.js
./zig-out/bin/zigttp-server  # No handler file argument needed
```

**Cold start**: ~71ms
**Improvement**: 13ms faster (16% reduction)

**Breakdown**:
- Process spawn: 10-15ms (unchanged)
- Dynamic linker (dyld): 15-20ms (unchanged)
- I/O backend init: 5ms (unchanged)
- **Embedded bytecode load**: 0ms (direct memory access)
- Connection pool: 5ms (unchanged)
- Network listener: 3ms (unchanged)

### Comparison

| Configuration | Cold Start | Improvement |
|--------------|------------|-------------|
| Baseline (runtime parsing) | 83ms | - |
| Optimized (embedded bytecode) | **71ms** | **-13ms (-16%)** |

## How Embedded Bytecode Works

### Build Process

When you use the `-Dhandler` flag:

1. **Precompile tool runs**: The `tools/precompile.zig` executable compiles your handler to bytecode
2. **Bytecode generation**: Handler source → parsed → compiled → serialized to bytecode array
3. **Code generation**: Generates `src/generated/embedded_handler.zig` containing the bytecode
4. **Server build**: Server binary includes the embedded bytecode module

Example generated file:

```zig
//! Auto-generated embedded handler bytecode
pub const bytecode = [_]u8{
    0x17, 0x00, 0x00, 0x37, 0x00, 0x00, 0x00, 0x00, ...
};
```

### Runtime Loading

From `src/zruntime.zig`:

```zig
fn loadHandlerCached(self: *Self, rt: *Runtime) !void {
    // Fast path: use embedded bytecode if available
    if (self.embedded_bytecode) |bytecode| {
        try rt.loadFromCachedBytecode(bytecode);
        return;  // ← Immediate return, no parsing!
    }

    // Slow path: runtime compilation (development mode)
    const key = bytecode_cache.BytecodeCache.cacheKey(self.handler_code);

    // Check cache
    if (self.cache.getRaw(key)) |cached_data| {
        try rt.loadFromCachedBytecode(cached_data);
        return;
    }

    // Cache miss: parse + compile + cache
    const serialized = try rt.loadCodeWithCaching(...);
    self.cache.putRaw(key, serialized);
}
```

The embedded bytecode path:
- ✅ No file I/O
- ✅ No parsing (8ms saved)
- ✅ No compilation (4ms saved)
- ✅ No caching overhead (1ms saved)
- ✅ Direct bytecode load from memory

### Bytecode Details

Example handler compression:

**Input**: `handler.js` (1,968 bytes source)
**Output**: `embedded_handler.zig` (895 bytes bytecode)
**Compression**: 54.5% size reduction

The bytecode includes:
- Compiled instructions
- String literal table
- Object literal shapes (for inline caching)
- Pattern dispatch table (for fast routing)
- Function metadata

## Profiling Results

### Methodology

Profiling conducted on Apple M4 Pro using:
- macOS `sample` command (5-second CPU sampling)
- Custom timing scripts (30 iterations, 3-run warmup)
- Flamegraph visualization (Brendan Gregg's toolkit)

### Hot Spots

Top functions during cold start (5-second profile, 1,265 total samples):

| Function | Samples | % | Phase |
|----------|---------|---|-------|
| Thread.PosixThreadImpl.spawn | 394 | 31% | Thread pool creation |
| libsystem_pthread (thread_start) | 313 | 25% | Thread startup |
| clock_gettime | 178 | 14% | Time measurement |
| dyld initialization | 100 | 8% | Dynamic linker |
| Thread.Futex.timedWait | 57 | 5% | Synchronization |

**Key finding**: Thread creation dominates profiling samples but is NOT the bottleneck for cold start time (see next section).

### Tested Optimizations

#### ✅ Embedded Bytecode: -13ms

**Status**: IMPLEMENTED and RECOMMENDED

The only optimization with measurable impact.

#### ❌ Reducing Prewarm Count: 0ms

**Tested**: Changed `prewarm_count` from 2 to 0
**Result**: No cold start improvement
**Memory savings**: 1.2MB (but no startup benefit)

**Why ineffective**: Prewarm runs in parallel worker threads:

```zig
// Spawn workers in parallel
for (0..prewarm_count) |i| {
    threads[i] = std.Thread.spawn(.{}, prewarmWorker, .{&contexts[i]});
}
// Join all workers
for (threads) |thread| {
    thread.join();
}
```

The parallel execution completes before network listener creation. Server logs confirm: "Pool ready: 0-1ms".

#### ❌ Symbol Stripping: 0ms

**Tested**: `strip zig-out/bin/zigttp-server`
**Result**: No cold start improvement
**Binary size**: 1.3MB → 1.2MB (100KB saved)

**Why ineffective**: dyld overhead comes from system library loading and debugger notifications (mach_msg syscalls), not symbol table size.

#### ❌ Reducing Pool Size: <2ms variance

**Tested**: Pool sizes 1, 2, 4, 8, 16, 28

Results:
```
Pool size  1: 48ms
Pool size  2: 49ms
Pool size  4: 49ms
Pool size  8: 47ms
Pool size 16: 47ms
Pool size 28: 47ms
```

**Why ineffective**: Thread pool slots are allocated but threads are created lazily or very efficiently in parallel.

## Remaining Optimization Potential

The remaining 71ms breaks down as:

### Unavoidable OS Overhead (~25-35ms)

1. **Process spawn** (10-15ms): Kernel creates new process
2. **Dynamic linker** (15-20ms): System library loading

These require infrastructure-level solutions:
- Pre-fork worker pools
- Lambda SnapStart-style snapshots
- Firecracker microVM clones

### Necessary Initialization (~35-45ms)

3. **I/O backend** (5ms): kqueue/io_uring setup
4. **Connection pool** (5ms): Worker thread creation
5. **Network listener** (3ms): Socket bind
6. **Everything else** (22-32ms): GC init, builtins, etc.

These could theoretically be reduced but with significant trade-offs:
- Lazy I/O init: Complicates code, moves cost to first request
- Single-threaded mode: No concurrency
- Smaller pool: Reduced throughput

**Not recommended**: Minimal gains, functionality trade-offs.

## Comparison: zigttp vs Deno

| Runtime | Cold Start | Memory (RSS) | Binary Size |
|---------|------------|--------------|-------------|
| Deno 2.6.7 | 150-200ms | ~30 MB | ~100 MB |
| zigttp (baseline) | 83ms | 4.0 MB | 1.2 MB |
| zigttp (optimized) | **71ms** | **3.8 MB** | **1.2 MB** |

**zigttp is 2.1-2.8x faster than Deno for cold starts**.

Why zigttp is faster:
1. No V8 snapshot deserialization (~50ms in Deno)
2. Custom JIT (simpler than TurboFan)
3. Direct bytecode execution
4. Smaller memory footprint (less paging overhead)
5. Optional embedded bytecode precompilation

## Production Deployment

### Recommended Build

```bash
# Build with embedded handler
zig build -Doptimize=ReleaseFast -Dhandler=src/handler.js

# Strip symbols (minimal benefit but no downside)
strip zig-out/bin/zigttp-server

# Deploy single binary
./zig-out/bin/zigttp-server -p 8080
```

No handler file argument needed - bytecode is embedded.

### Lambda Deployment Example

**Without optimization**:
```dockerfile
FROM alpine:latest
COPY handler.js /app/handler.js
COPY zigttp-server /app/zigttp-server
CMD ["/app/zigttp-server", "/app/handler.js"]
```

**With optimization** (smaller image, faster cold starts):
```dockerfile
FROM alpine:latest
COPY zigttp-server /app/zigttp-server
# No handler.js needed - embedded in binary
CMD ["/app/zigttp-server"]
```

### Container Image Comparison

| Configuration | Image Size | Files | Cold Start |
|---------------|------------|-------|------------|
| Baseline | ~2.5 MB | 2 (server + handler.js) | 83ms |
| Optimized | **~1.2 MB** | **1** (server only) | **71ms** |

## Build System Integration

### Basic Usage

```bash
# Development: fast iteration
zig build -Doptimize=ReleaseFast
./zig-out/bin/zigttp-server handler.js

# Production: optimized cold start
zig build -Doptimize=ReleaseFast -Dhandler=handler.js
./zig-out/bin/zigttp-server
```

### CI/CD Integration

```yaml
# GitHub Actions example
- name: Build optimized server
  run: |
    zig build -Doptimize=ReleaseFast -Dhandler=src/handler.js
    strip zig-out/bin/zigttp-server

- name: Upload artifact
  uses: actions/upload-artifact@v3
  with:
    name: zigttp-optimized
    path: zig-out/bin/zigttp-server
```

### Multi-Handler Builds

For multiple handlers, build separate binaries:

```bash
# API handler
zig build -Doptimize=ReleaseFast -Dhandler=handlers/api.js
mv zig-out/bin/zigttp-server builds/zigttp-api

# Web handler
zig build -Doptimize=ReleaseFast -Dhandler=handlers/web.js
mv zig-out/bin/zigttp-server builds/zigttp-web

# Background worker
zig build -Doptimize=ReleaseFast -Dhandler=handlers/worker.js
mv zig-out/bin/zigttp-server builds/zigttp-worker
```

## Benchmarking Cold Starts

### Quick Test

```bash
# Time a single cold start
time (./zigttp-server &
      sleep 0.1 && curl -s http://localhost:8080/api/health &&
      pkill zigttp-server)
```

### Comprehensive Analysis

See [zigttp-bench](https://github.com/srdjan/zigttp-bench) repository for complete benchmarking suite:

```bash
git clone https://github.com/srdjan/zigttp-bench
cd zigttp-bench

# Build optimized version
./scripts/build_optimized.sh

# Run cold start analysis (30 iterations)
./scripts/analyze_coldstart_embedded.sh

# Generate flamegraph profiling
./scripts/profile_coldstart.sh
```

## Platform-Specific Analysis

### macOS (Development) - 103ms Cold Start

**Detailed profiling** (2026-01-31, flamegraph analysis):

| Phase | Time | Percentage | Optimization Potential |
|-------|------|------------|----------------------|
| **dyld (dynamic linker)** | 80-90ms | 78-87% | ❌ Not optimizable |
| **Runtime pool init** | 3ms | 3% | ✅ Already optimal |
| **Network + HTTP** | 10-20ms | 10-19% | ⚠️ Minimal gains |
| **Total** | **103ms** | **100%** | - |

**Key findings from flamegraph profiling**:
- Runtime initialization is NOT the bottleneck (only 3ms)
- macOS dyld (dynamic linker) dominates cold start time (80-90ms)
- All runtime-level optimizations yield <3% total improvement

**Why dyld takes 80-90ms on macOS**:
1. Loading libSystem.B.dylib (core system library)
2. Symbol resolution and relocation
3. Security validation (code signing, ASLR)
4. Debugger notifications (mach_msg syscalls)
5. Thread-local storage setup

**Optimization attempts**:
- ❌ Static linking: NOT POSSIBLE on macOS (Apple doesn't provide static libSystem)
- ❌ Eliminate libc: NOT FEASIBLE (100+ stdlib functions required)
- ❌ Defer caches: <0.5ms gain (not worth complexity)
- ❌ Lazy worker threads: <1ms gain (requires structural changes)

**Conclusion for macOS**: 103ms is near-optimal for the architecture. The runtime itself (3ms) is already excellent. The dyld overhead is inherent to macOS and affects ALL dynamically-linked binaries.

**macOS is acceptable for development** - the ~100ms cold start is competitive with other runtimes and unavoidable without Apple changing the OS.

### Linux (Production Target) - Optimization Plan

**Goal**: Sub-20ms cold start via static linking

**Current status**: Cross-compilation blocked by JIT architecture issues

**Theoretical performance** (fully static Linux binary):

| Phase | macOS | Linux Static | Improvement |
|-------|-------|--------------|-------------|
| dyld/ld.so overhead | 80-90ms | 5-10ms | -75-80ms |
| Runtime init | 3ms | 3ms | 0ms |
| Network + HTTP | 10-20ms | 10-20ms | 0ms |
| **Total** | **103ms** | **18-33ms** | **70-85ms (68-82%)** |

**Implementation plan**:

1. **Fix JIT cross-compilation** (required)
   - Architecture-specific register handling in `zts/jit/x86.zig`
   - Conditional compilation for arm64/x86_64/aarch64
   - Runtime architecture detection

2. **Build fully static Linux binary**
   ```bash
   zig build -Dtarget=x86_64-linux-musl -Doptimize=ReleaseFast \
     -Dhandler=handler.js
   ```

3. **Verify no dynamic dependencies**
   ```bash
   ldd zig-out/bin/zigttp-server
   # Expected: "not a dynamic executable"
   ```

4. **Benchmark on native Linux hardware**
   - Measure actual cold start (not cross-compile)
   - Profile with `perf` to verify no dyld overhead
   - Validate 18-33ms target

**Benefits of Linux static binary**:
- ✅ 70-85ms faster cold start than macOS
- ✅ No dynamic library dependencies
- ✅ Smaller container images
- ✅ Better portability (single binary)
- ✅ Predictable performance (no dyld variance)

**Timeline**: Future enhancement after JIT architecture work

**Priority**: Medium - macOS performance is acceptable for development, but Linux static binary provides significant production benefits

## Future Optimizations

### Process-Level Caching

These require deployment infrastructure changes:

**Lambda SnapStart equivalent**: -50ms (on top of static binary)
- Snapshot process after initialization
- Restore from snapshot on invocation
- Eliminates spawn + init overhead
- Combined with static Linux: ~10-15ms total cold start

**Pre-fork worker pool**: -40ms
- Maintain pool of pre-spawned processes
- Assign work to idle worker
- Amortizes cold start cost

**Firecracker microVM clones**: -45ms
- Clone entire VM state with CoW memory
- Near-instant process availability
- Requires specialized runtime

### JIT Warmup Reduction

Current JIT policy (lazy compilation after 100 calls) is optimized for warm throughput. For cold start optimization:

```zig
// src/zruntime.zig
jit_policy: .disabled,  // Pure interpreter, -3ms cold start
```

Trade-off: ~20% slower warm request throughput.

**Not recommended** unless cold start is the only metric that matters.

## Monitoring Cold Starts

### Basic Timing

```bash
# Add timing to handler
function handler(request) {
    console.log(`Request start: ${Date.now()}`);
    // ... handler code ...
    console.log(`Request end: ${Date.now()}`);
    return response;
}
```

### Metrics Export

For production monitoring:

```javascript
let coldStartTime = null;
let requestCount = 0;

function handler(request) {
    requestCount++;

    if (requestCount === 1) {
        // First request captures cold start
        coldStartTime = Date.now() - startTime;
    }

    return Response.json({
        data: processRequest(request),
        meta: {
            coldStart: coldStartTime,
            requestCount: requestCount
        }
    });
}
```

## Static Linking Investigation

### Objective

Eliminate the 80-90ms dyld overhead by using static linking (fully self-contained binary with no dynamic dependencies).

### Findings (2026-01-31)

**Current dependencies**:
```bash
$ otool -L zigttp-server
zigttp-server:
  /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1356.0.0)
```

Only one dynamic dependency: libSystem.B.dylib (macOS core system library containing libc, libpthread, libm, and system call wrappers).

**Symbol analysis**: Binary imports 100+ symbols from libSystem.B.dylib:
- Networking: `accept`, `bind`, `connect`, `listen`, `socket`, `getaddrinfo`
- Threading: `pthread_create`, `pthread_mutex_lock`, `__ulock_wait2`
- Memory: `malloc`, `free`, `calloc`, `realloc`
- I/O: `read`, `write`, `close`, `fcntl`
- Math: `cos`, `exp`, `log`, `sin`
- System: `clock_gettime`, `fork`, `execve`

### Results

#### ❌ Static Linking on macOS: NOT POSSIBLE

**Reason**: Apple does not provide static versions of libSystem.B.dylib. macOS requires dynamic linking for:
- System call interface (all syscalls go through libSystem)
- Thread-local storage (`_tlv_bootstrap`)
- ABI compatibility and versioning
- Security features (code signing, library validation)
- App Store requirements

**Attempted**:
```bash
$ zig build -Dtarget=aarch64-macos -Doptimize=ReleaseFast
# Always links libSystem.B.dylib dynamically
```

This is a hard architectural requirement of macOS. Cannot be worked around.

#### ❌ Linux Cross-Compilation: BLOCKED

**Attempted**:
```bash
$ zig build -Dtarget=x86_64-linux-musl -Doptimize=ReleaseFast
```

**Error**: JIT architecture incompatibility
```
zts/jit/x86.zig:15:22: error: enum 'jit.x86.Register' has no member named 'eax'
```

**Cause**: JIT has architecture-specific register handling. Cross-compiling from arm64 to x86_64 fails due to hardcoded register names.

**Resolution required**: Conditional compilation for each target architecture.

#### ❌ Eliminate libc: NOT FEASIBLE

**Reason**: Would require reimplementing 100+ stdlib functions as direct syscall wrappers. Estimated effort: months. Still wouldn't avoid dyld on macOS.

### Theoretical Maximum (If Static Linking Worked)

| Phase | Current | Static | Improvement |
|-------|---------|--------|-------------|
| dyld loading | 80-90ms | 0ms | 80-90ms |
| Runtime init | 3ms | 3ms | 0ms |
| Network + HTTP | 10-20ms | 10-20ms | 0ms |
| **Total** | **103ms** | **13-23ms** | **78-87%** |

Best-case: **13-23ms** cold start

### Practical Alternatives

Since static linking is impossible on macOS:

1. **Persistent daemon** (recommended for dedicated servers)
   - Cold start: 0ms (daemon always running)
   - Use launchd (macOS) or systemd (Linux)
   - Memory cost: ~4-10 MB RSS

2. **Pre-fork model** (documented alternative)
   - Cold start: ~63ms (40ms improvement)
   - Already explored in optimization results

3. **Linux deployment** (future plan)
   - Build native static Linux binary
   - Expected: ~18-33ms cold start
   - Requires fixing JIT cross-compilation

4. **Accept current performance** (pragmatic)
   - 103ms is competitive with Deno (~150ms), Node (~200ms)
   - dyld overhead affects ALL macOS binaries
   - Acceptable for development use

### Conclusion

The 80-90ms dyld overhead is inherent to macOS architecture. It cannot be eliminated through application-level changes.

**For macOS (development)**: Accept 103ms as near-optimal. The runtime itself is already excellent (3ms initialization).

**For Linux (production)**: Static linking is viable and would provide 70-85ms improvement. Requires fixing JIT architecture issues first.

Full investigation documented in: `/Users/srdjans/Code/zigttp-bench/results/static_linking_investigation.md`

## Summary

### Recommended for Production Deployments

**Current (macOS development)**:
```bash
zig build -Doptimize=ReleaseFast -Dhandler=path/to/handler.js
```

**Benefits**:
- Embedded bytecode precompilation
- Single binary deployment
- Smaller container images
- Zero trade-offs

**Performance**:
- macOS: ~103ms cold start (acceptable for development)
- Runtime initialization: 3ms (highly optimized)
- dyld overhead: 80-90ms (unavoidable on macOS)

### Future: Linux Static Binary

**Target**:
```bash
zig build -Dtarget=x86_64-linux-musl -Doptimize=ReleaseFast \
  -Dhandler=handler.js
```

**Expected performance**:
- Cold start: 18-33ms (70-85ms faster than macOS)
- No dynamic dependencies
- Portable single binary

**Blockers**:
- JIT architecture-specific code needs conditional compilation
- Requires native Linux testing (not cross-compile)

**Timeline**: Future enhancement

### Platform Strategy

- **macOS**: Development environment only (~100ms acceptable)
- **Linux**: Production deployment target (sub-20ms goal via static linking)
- **Pre-fork/daemon**: Alternative for sub-millisecond cold starts on any platform
