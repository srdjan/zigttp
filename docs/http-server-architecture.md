# HTTP Server Architecture for zts (Performance-FaaS)

This document maps the proposed high-performance architecture to the existing codebase and defines a concrete implementation plan. It focuses on sub-1ms latency for simple handlers, high throughput, and <5ms cold starts, while keeping allocation patterns predictable.

## Goals (Measured)

- Simple handler p50 latency <1ms (warm, no I/O).
- Cold start <5ms (init + first request without parsing JS on the critical path where possible).
- High throughput under concurrency (steady p99 without GC spikes).
- Predictable memory usage with bounded per-request allocations.

## Baseline (Current Code)

- `src/server.zig`: evented IO with `std.Io`, per-connection arena reset; HTTP/1.1 parser uses line reads; static cache implemented with LRU.
- `src/zruntime.zig`: lock-free runtime pool backed by `zts.LockFreePool`, hybrid allocation, bytecode cache placeholder.
- `zts/*`: GC, parser, bytecode cache, intern pool already present.

## Target Architecture

### 1) I/O + Concurrency

- Use evented IO where supported; keep threaded IO as fallback.
- Reactor/worker model with per-core runtime pools to minimize cross-thread contention.
- Prefer per-thread pools and queues; avoid global locks in hot path.

Implementation mapping:
- `src/server.zig`: add backend selection by platform, add per-thread runtime pool shard, and use `Io.Group` per reactor.
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
- `src/zruntime.zig`: shard pool by core; add `executeHandlerBorrowed` as default in server; implement bytecode cache with atoms.
- `zts/bytecode_cache.zig`: expose a fast deserialization path that bypasses parser.

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
   - Switch server to `executeHandlerBorrowed` and delay runtime reset.
   - Ensure response send happens before release.
   - Files: `src/server.zig`, `src/zruntime.zig`.

3) **Pool sharding + fast acquire**
   - Create one `HandlerPool` per core; select by connection/thread.
   - Keep global overflow pool if shard is exhausted.
   - Files: `src/server.zig`, `src/zruntime.zig`.

4) **Bytecode cache true hit path**
   - Serialize bytecode + atoms at compile time; cache per handler hash.
   - On cold start, load bytecode without parsing.
   - Files: `zts/bytecode_cache.zig`, `src/zruntime.zig`.

5) **Static file improvements**
   - ETag/Last-Modified; handle 304.
   - Prefer `sendfile` for large files.
   - Files: `src/server.zig`.

6) **GC tuning hooks**
   - Add API to set per-request GC thresholds.
   - Adjust nursery size based on request body length.
   - Files: `zts/gc.zig`, `src/zruntime.zig`.

## Trade-Offs (FaaS-Oriented)

- Aggressive pooling reduces latency but increases resident memory; use lazy growth plus small prewarm.
- Borrowed response bodies eliminate copies but require strict send-before-reset discipline.
- Evented IO yields best tail latency but needs careful backpressure and buffer sizing.

## Next Steps

- Decide on parser scope (incremental vs full rewrite).
- Pick core-sharding strategy (per-thread vs explicit CPU affinity).
- Prioritize bytecode cache atom serialization (biggest cold-start win).
