# Performance Guide

This document covers performance characteristics, benchmarks, optimizations, and deployment patterns for zigttp-server.

## Benchmarks

### JavaScript Engine Performance (QuickJS baseline)

zts outperforms QuickJS in our historical benchmark runs (QuickJS is used only as an external baseline). See `benchmarks/*.json` for raw results.

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

Run benchmarks with: `./zig-out/bin/zigttp-bench`

### HTTP Throughput (Deno baseline)

| Endpoint         | zigttp RPS | Deno Baseline | Ratio |
| ---------------- | ---------- | ------------- | ----- |
| /api/health      | 79,743     | 104,672       | 0.76x |
| /api/echo        | 79,409     | 63,726        | 1.25x |
| /api/greet/world | 80,030     | 105,016       | 0.76x |

Note: JIT compilation is enabled by default for hot functions (after `JIT_THRESHOLD` executions). Optional instrumentation (perf.zig) and parallel compiler (compiler.zig) modules exist in `zts/` but are not exported.

## FaaS Optimizations

### Cold Start Performance

- **Binary initialization**: < 1ms to initialize runtime and load handler
- **Handler loading**: Typically < 5ms for JavaScript parsing and bytecode compilation
- **Build-time precompilation**: `-Dhandler=<path>` embeds bytecode directly, eliminating all runtime parsing
- **No JIT warm-up**: Bytecode interpreter provides predictable performance from first request
- **Memory footprint**: 256KB default JS heap (configurable per function)
- **Deployment size**: ~500KB binary, zero runtime dependencies

### Warm Invocations

HandlerPool reuses pre-warmed contexts for subsequent requests:

- O(1) pool slot acquisition via `free_hint` atomic
- Zero runtime parsing overhead (bytecode already compiled)
- Minimal GC overhead via hybrid arena allocation
- Predictable latency (no JIT compilation required)

## Request Pipeline Optimizations

### Property Access Optimizations

#### Shape Preallocation

**Location**: `zts/context.zig:284-346`

HTTP Request and Response objects use preallocated hidden class shapes, eliminating hidden class transitions. Direct slot writes via `setSlot()` bypass property lookup entirely.

**Shapes**:
- Request shape: method, url, body, headers (4 props)
- Response shape: body, status, statusText, ok, headers (5 props)
- Response headers shape: content-type, content-length, cache-control (3 props)
- Request headers shape: authorization, content-type, accept (3 props)

#### Polymorphic Inline Cache (PIC)

**Location**: `zts/interpreter.zig:214-272`

8-entry cache per property access site with last-hit optimization for O(1) monomorphic lookups. Megamorphic transition after 9th distinct shape.

**Benefits**:
- Monomorphic sites: O(1) property access
- Polymorphic sites (2-8 shapes): O(n) linear scan, typically 2-3 shapes
- Megamorphic sites: Falls back to hash table lookup

#### Binary Search for Large Objects

**Location**: `zts/object.zig:751, 831-835`

Objects with 8+ properties use binary search on sorted property arrays.

- Threshold: `BINARY_SEARCH_THRESHOLD = 8`
- Complexity: O(log n) vs O(n) linear scan
- Applied automatically for large objects

#### JIT Baseline IC Integration

**Location**: `zts/jit/baseline.zig:1604-1765`

x86-64 and ARM64 JIT fast paths check PIC entry[0] inline, falling back to helper only on cache miss.

**Fast path**:
1. Load PIC entry[0] shape
2. Compare with object's hidden class
3. If match: load property from cached offset
4. If miss: call IC helper

#### JIT Object Literal Shapes

**Location**: `zts/context.zig:746-779`, `zts/jit/baseline.zig:3646-3670`

Object literals with static keys use pre-compiled hidden class shapes. The `new_object_literal` opcode creates objects with the final hidden class directly (no transitions), and `set_slot` writes property values to inline slots without lookup overhead.

Fast path uses arena bump allocation with minimal slot initialization (`createWithArenaFast`). Supports both x86-64 and ARM64.

#### Type Feedback

**Location**: `zts/type_feedback.zig`

Call site and value type profiling for JIT optimization decisions. Early exits when sites become megamorphic avoid redundant type classification. Inlining threshold lowered to 5 calls (from 10) for faster FaaS warmup.

### String Optimizations

#### Lazy String Hashing

**Location**: `zts/string.zig:18-24, 44-54`

Hash computation deferred until actually needed. Both `JSString` and `SliceString` use a `hash_computed` flag to track state; `getHash()`/`getHashConst()` compute on first access. Reduces overhead for strings never used as hash keys.

**Benefits**:
- No hash computation for strings only used for comparison
- Hash computed once and cached
- Reduces allocation overhead by ~15% in typical workloads

#### Pre-interned HTTP Atoms

**Location**: `zts/object.zig:237-264`

27 common headers with O(1) lookup:

**Basic**: content-type, content-length, accept, host, user-agent, authorization

**Caching**: cache-control, if-modified-since, if-none-match, etag, last-modified, expires, pragma

**CORS**: origin, access-control-allow-origin, access-control-allow-methods, access-control-allow-headers, access-control-allow-credentials, access-control-max-age

**Other**: connection, accept-encoding, cookie, x-forwarded-for, x-request-id, content-encoding, transfer-encoding, vary

#### HTTP String Cache

**Location**: `zts/context.zig:96-110, 349-400`

Pre-allocated status texts (OK, Created, Not Found, etc.) and content-type strings (application/json, text/plain, text/html).

**Benefits**:
- Zero allocation for common status texts
- Zero allocation for common content types
- Reduces per-request allocations

### Pool and Request Optimizations

#### Pool Slot Hint

**Location**: `zts/pool.zig`

`free_hint` atomic reduces slot acquisition from O(N) linear scan to O(1) in the common case.

**Algorithm**:
1. Check `free_hint` slot first
2. If free, acquire atomically
3. If busy, fall back to linear scan
4. Update `free_hint` on release

#### Relaxed Atomic Ordering

**Location**: `src/zruntime.zig`

The `in_use` counter uses `.monotonic` ordering since it's only for metrics/limits, not synchronization.

**Benefits**:
- Reduced memory barrier overhead
- Faster pool acquisition/release
- No correctness impact (metrics are approximate)

#### LRU Static Cache

**Location**: `src/server.zig`

Static file cache uses doubly-linked list LRU eviction instead of clear-all, eliminating latency spikes.

**Configuration**:
- `static_cache_max_bytes`: Maximum total cache size (default: 2MB)
- `static_cache_max_file_size`: Maximum individual file size (default: 128KB)

**Benefits**:
- No periodic latency spikes from cache clears
- Efficient memory usage
- Predictable eviction behavior

#### Adaptive Backoff

**Location**: `src/zruntime.zig`

Three-phase backoff for pool contention:

**Phase 1**: 10 spin iterations using `spinLoopHint`
- Low-latency busy-wait for short contention

**Phase 2**: Sleep 10us-1ms with jitter
- Prevents thundering herd
- Exponential backoff with randomization

**Phase 3**: Circuit breaker fails fast after 100 retries
- Returns error instead of infinite wait
- Allows graceful degradation

#### Zero-Copy Response

**Location**: `src/zruntime.zig`

Borrowed mode for both body and headers avoids memcpy when arena lifetime is guaranteed.

**Conditions**:
- Response body allocated in request arena
- Headers allocated in request arena
- Arena lifetime extends beyond HTTP write

**Benefits**:
- Zero memcpy overhead for response body
- Zero memcpy overhead for response headers
- Reduced memory allocation

### Build-Time Precompilation

**Location**: `tools/precompile.zig`, `build.zig`

The `-Dhandler=<path>` build option compiles JavaScript handlers at build time. Bytecode is embedded directly into the binary, eliminating runtime parsing entirely. This provides the fastest possible cold start for production deployments.

Build flow:
1. `precompile.zig` uses full zts engine to compile handler
2. Bytecode serialized with atoms and shapes
3. Generated `src/generated/embedded_handler.zig` included in build
4. Server loads bytecode directly via `loadFromCachedBytecode()`

**Benefits**:
- Zero parsing overhead on cold start
- Predictable cold start latency (< 1ms)
- Reduced binary size (no parser in production build when using embedded handler)
- Optimal for FaaS deployments

## Memory Management

### Hybrid Arena Allocation

For request-scoped workloads, zts uses a hybrid memory model:

**Arena allocator**: O(1) bulk reset between requests, zero per-object overhead

**Escape detection**: Write barriers prevent arena objects from leaking into persistent storage

**GC disabled in hybrid mode**: No collection pauses during request handling

**Benefits**:
- Predictable memory usage per request
- No GC pauses during request processing
- O(1) cleanup between requests
- Eliminates latency spikes in FaaS environments

### Memory Configuration

```bash
# Default (256KB) - typical API handlers
./zig-out/bin/zigttp-server handler.js

# Larger (1MB) - complex processing, large JSON
./zig-out/bin/zigttp-server -m 1m handler.js

# Smaller (64KB) - minimal functions
./zig-out/bin/zigttp-server -m 64k handler.js
```

### Memory Tuning Guidelines

**Small handlers (< 64KB)**:
- Simple JSON responses
- Minimal object creation
- No large string operations

**Medium handlers (256KB, default)**:
- Typical REST API handlers
- Moderate JSON processing
- Template rendering

**Large handlers (1MB+)**:
- Complex data transformations
- Large JSON documents
- Extensive string operations

## Handler Optimization Tips

### Reuse Objects Across Requests

```javascript
// GOOD: Reuse objects across requests
let responseTemplate = { status: "ok" };

function handler(request) {
    responseTemplate.timestamp = Date.now();
    return Response.json(responseTemplate);
}

// AVOID: Creating large objects per request
function handler(request) {
    // This creates garbage every request
    let bigArray = [];
    for (let i = 0; i < 10000; i++) {
        bigArray.push({ index: i });
    }
    return Response.json(bigArray);
}
```

### Pre-compute Constants

```javascript
// GOOD: Pre-compute at load time
const routes = {
    "/api/health": handleHealth,
    "/api/users": handleUsers,
    "/api/posts": handlePosts,
};

function handler(request) {
    const handler = routes[request.url];
    if (handler) {
        return handler(request);
    }
    return Response.text("Not Found", { status: 404 });
}

// AVOID: Computing on every request
function handler(request) {
    if (request.url === "/api/health") {
        return handleHealth(request);
    }
    // Repeated string comparisons
}
```

### Minimize String Concatenation

```javascript
// GOOD: Build arrays and join
function buildHtml(items) {
    const parts = ["<ul>"];
    for (let i = 0; i < items.length; i++) {
        parts.push("<li>" + items[i] + "</li>");
    }
    parts.push("</ul>");
    return parts.join("");
}

// AVOID: Repeated concatenation
function buildHtml(items) {
    let html = "<ul>";
    for (let i = 0; i < items.length; i++) {
        html = html + "<li>" + items[i] + "</li>"; // Creates new string each iteration
    }
    html = html + "</ul>";
    return html;
}
```

## Deployment Patterns

### Single Instance (Lambda-style)

```bash
./zigttp-server handler.js
```

Each instance handles one request at a time for isolation. Ideal for AWS Lambda, Azure Functions, Google Cloud Functions.

**Characteristics**:
- Single-threaded
- One request per instance
- Pre-warmed handler pool for subsequent requests
- Minimal memory footprint

### Multiple Instances Behind Load Balancer

For high-throughput scenarios, deploy multiple instances. The small binary size (~500KB) and instant cold starts make horizontal scaling efficient.

```bash
# Instance 1
./zigttp-server -p 8080 handler.js

# Instance 2
./zigttp-server -p 8081 handler.js

# Instance 3
./zigttp-server -p 8082 handler.js
```

**Load balancer configuration**: Round-robin or least-connections to distribute requests.

### Container Deployment

```dockerfile
FROM scratch
COPY zig-out/bin/zigttp-server /zigttp-server
COPY handler.js /handler.js
EXPOSE 8080
ENTRYPOINT ["/zigttp-server", "-q", "-h", "0.0.0.0", "/handler.js"]
```

**Benefits**:
- Minimal container size (~500KB)
- No runtime dependencies
- Fast container startup
- Efficient resource usage

### FaaS Deployment (AWS Lambda)

Build for target platform:

```bash
# AWS Lambda (x86-64)
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux

# AWS Lambda (ARM64)
zig build -Doptimize=ReleaseFast -Dtarget=aarch64-linux

# With precompiled handler (fastest cold starts)
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux -Dhandler=handler.js
```

Package as Lambda deployment:

```bash
zip function.zip bootstrap handler.js
aws lambda create-function --function-name my-function \
  --zip-file fileb://function.zip --runtime provided.al2 \
  --handler handler.handler --role arn:aws:iam::...
```

**Lambda-specific optimizations**:
- Use precompiled handlers for fastest cold starts
- Set memory to match handler requirements (256MB-1GB typical)
- Enable ARM64 (Graviton2) for better price/performance
- Use provisioned concurrency for latency-sensitive workloads

### Edge Deployment (Cloudflare Workers - experimental)

Build with wasm32 target:

```bash
zig build -Doptimize=ReleaseFast -Dtarget=wasm32-wasi
```

Note: WebAssembly support is experimental.

## Production Configuration

### Server Options

CLI options for the standalone server:

```bash
zigttp-server -p 8080 -h 127.0.0.1 -n 8 --cors --static ./public handler.js
```

Advanced options are available through `ServerConfig` when embedding `Server` directly in Zig:

```zig
const config = ServerConfig{
    .pool_wait_timeout_ms = 5,
    .pool_metrics_every = 1000,
    .static_cache_max_bytes = 2 * 1024 * 1024,
    .static_cache_max_file_size = 128 * 1024,
};
```

### Pool Metrics

When `pool_metrics_every` is set, logs include a line like:

```
Pool metrics: in_use=2/8 exhausted=0 avg_wait_us=3 max_wait_us=20 avg_exec_us=120 max_exec_us=500
```

Fields:
- `in_use`: Current/maximum pool size
- `exhausted`: Number of times pool was fully utilized
- `avg_wait_us`/`max_wait_us`: Pool acquisition time (microseconds)
- `avg_exec_us`/`max_exec_us`: Handler execution time (microseconds)

Use these metrics to tune pool size and identify performance bottlenecks.

## Performance Monitoring

### Key Metrics

1. **Cold start latency**: Time from binary start to first request handled
2. **Handler execution time**: Time spent in JavaScript handler
3. **Pool acquisition time**: Time waiting for available handler context
4. **Memory usage**: JS heap usage and GC frequency
5. **Request throughput**: Requests per second under load

### Benchmarking

Run the built-in benchmark suite:

```bash
./zig-out/bin/zigttp-bench
```

Load test with external tools:

```bash
# Apache Bench
ab -n 10000 -c 100 http://localhost:8080/

# wrk
wrk -t4 -c100 -d30s http://localhost:8080/

# hey
hey -n 10000 -c 100 http://localhost:8080/
```

### Profiling

For detailed performance analysis, use system profiling tools:

```bash
# Linux perf
perf record -g ./zigttp-server handler.js
perf report

# macOS Instruments
instruments -t "Time Profiler" ./zigttp-server handler.js

# Valgrind (memory profiling)
valgrind --tool=massif ./zigttp-server handler.js
```
