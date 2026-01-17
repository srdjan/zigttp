# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

zigttp-server is a **serverless JavaScript runtime** for FaaS (Function-as-a-Service) use cases, powered by **zts** - a pure Zig JavaScript engine. Target deployments: AWS Lambda, Azure Functions, Cloudflare Workers, edge computing.

**Design goals**: Instant cold starts, small deployment package, request isolation, zero external dependencies.

**Trade-offs for FaaS optimization**: Single-threaded sequential processing (one request per instance), compile-time configuration, ES5 JavaScript subset (with some ES6).

## Build Commands

Requires Zig 0.16.0+ (nightly).

```bash
# Build
zig build                           # Debug build
zig build -Doptimize=ReleaseFast   # Optimized build

# Run
zig build run -- -e "function handler(req) { return Response.json({ok:true}); }"
zig build run -- examples/handler.js -p 3000
zig build run -- examples/handler.ts       # TypeScript handler
zig build run -- examples/handler.tsx      # TSX handler

# Test
zig build test                      # All src/ tests
zig build test-zts                  # zts engine tests only
zig build test-zruntime             # Native Zig runtime tests only

# Run a single test by name
zig build test 2>&1 | head -50      # See available tests
zig build test -- --test-filter "runtime init"  # Filter by name

# Benchmark
zig build bench                     # Build and run benchmarks (src/benchmark.zig)
```

## Architecture

### Two-Layer Design

**Server layer (src/)**: HTTP listener, CLI, request routing, static file serving. Entry point is `main.zig`, HTTP handling in `server.zig`, JS runtime management in `zruntime.zig`.

**Engine layer (zts/)**: Pure Zig JavaScript engine with two-pass compilation (parse to IR, then bytecode). Key components:
- Parser (`zts/parser/`): Pratt parser, tokenizer, IR, bytecode codegen, scope tracking
- VM (`interpreter.zig`): Stack-based bytecode interpreter with JIT baseline compiler
- Values (`value.zig`, `object.zig`): NaN-boxing, hidden classes, inline caching
- Memory (`gc.zig`, `heap.zig`, `arena.zig`, `pool.zig`): Generational GC, arena allocator, lock-free pooling
- TypeScript (`stripper.zig`, `comptime.zig`): Type stripping and compile-time evaluation

### Request Flow

1. Server accepts HTTP connection
2. HandlerPool acquires an isolated runtime (LockFreePool-backed)
3. Request parsed and converted to JS Request object
4. Handler function invoked with Request
5. JS Response extracted and sent as HTTP response
6. Runtime released back to pool

### Key Patterns

**Handler Pool**: Pre-allocates JS contexts for per-request isolation using `LockFreePool`. Critical for FaaS warm instance reuse.

**Result Type**: Functional error handling throughout - `Result(T)` union with `ok`/`err` variants. Maps JS exceptions to Zig errors.

**NaN-Boxing**: 64-bit tagged values storing integers, floats, and pointers in a single word.

**Hidden Classes**: V8-style hidden class transitions for inline caching. Enables fast property access.

**Hybrid Arena Allocation**: Request-scoped arena with O(1) bulk reset. Write barriers detect arena escape. GC disabled in hybrid mode.

### Performance Optimizations

The request pipeline includes several optimizations for low-latency FaaS workloads:

#### Property Access Optimizations

**Shape Preallocation** (`zts/context.zig:284-346`): HTTP Request and Response objects use preallocated hidden class shapes, eliminating hidden class transitions. Direct slot writes via `setSlot()` bypass property lookup entirely.
- Request shape: method, url, body, headers (4 props)
- Response shape: body, status, statusText, ok, headers (5 props)
- Response headers shape: content-type, content-length, cache-control (3 props)
- Request headers shape: authorization, content-type, accept (3 props)

**Polymorphic Inline Cache (PIC)** (`zts/interpreter.zig:214-272`): 8-entry cache per property access site with last-hit optimization for O(1) monomorphic lookups. Megamorphic transition after 9th distinct shape.

**Binary Search for Large Objects** (`zts/object.zig:751, 831-835`): Objects with 8+ properties use binary search on sorted property arrays. Threshold: `BINARY_SEARCH_THRESHOLD = 8`.

**JIT Baseline IC Integration** (`zts/baseline.zig:1604-1765`): x86-64 and ARM64 JIT fast paths check PIC entry[0] inline, falling back to helper only on cache miss.

#### String Optimizations

**Lazy String Hashing** (`zts/string.zig:18-24, 44-54`): Hash computation deferred until actually needed. `hash_computed` flag tracks state; `getHash()`/`getHashConst()` compute on first access. Reduces overhead for strings never used as hash keys.

**Pre-interned HTTP Atoms** (`zts/object.zig:237-264`): 27 common headers with O(1) lookup:
- Basic: content-type, content-length, accept, host, user-agent, authorization
- Caching: cache-control, if-modified-since, if-none-match, etag, last-modified, expires, pragma
- CORS: origin, access-control-allow-origin, access-control-allow-methods, access-control-allow-headers, access-control-allow-credentials, access-control-max-age
- Other: connection, accept-encoding, cookie, x-forwarded-for, x-request-id, content-encoding, transfer-encoding, vary

**HTTP String Cache** (`zts/context.zig:96-110, 349-400`): Pre-allocated status texts (OK, Created, Not Found, etc.) and content-type strings (application/json, text/plain, text/html).

#### Pool and Request Optimizations

**Pool Slot Hint** (`zts/pool.zig`): `free_hint` atomic reduces slot acquisition from O(N) linear scan to O(1) in the common case.

**Relaxed Atomic Ordering** (`src/zruntime.zig`): The `in_use` counter uses `.monotonic` ordering since it's only for metrics/limits, not synchronization.

**LRU Static Cache** (`src/server.zig`): Static file cache uses doubly-linked list LRU eviction instead of clear-all, eliminating latency spikes.

**Adaptive Backoff** (`src/zruntime.zig`): Three-phase backoff for pool contention:
- Phase 1: 10 spin iterations using `spinLoopHint`
- Phase 2: Sleep 10us-1ms with jitter (prevents thundering herd)
- Phase 3: Circuit breaker fails fast after 100 retries

**Zero-Copy Response** (`src/zruntime.zig`): Borrowed mode for both body and headers avoids memcpy when arena lifetime is guaranteed.

## TypeScript/TSX Support

zts includes a native TypeScript/TSX stripper that removes type annotations at load time. Use `.ts` or `.tsx` files directly.

### Compile-Time Evaluation

The `comptime()` function evaluates expressions at load time:

```typescript
const x = comptime(1 + 2 * 3);                 // -> const x = 7;
const upper = comptime("hello".toUpperCase()); // -> const upper = "HELLO";
const etag = comptime(hash("content-v1"));     // -> const etag = "a1b2c3d4";
```

**Supported**: Literals, arithmetic, bitwise, comparison, logical, ternary, Math, string methods, `parseInt`, `parseFloat`, `JSON.parse`, `hash`.

**Disallowed**: Variables, `Date.now()`, `Math.random()`, closures, assignments.

See [docs/typescript-comptime-spec.md](docs/typescript-comptime-spec.md) for full specification.

## JavaScript Runtime

**Supported**: ES5 + `for...of` (arrays), typed arrays, `**` operator, `globalThis`, string methods (replaceAll, trimStart/End), Math extensions.

**Limitations**: Strict mode only, no `with`, no array holes, no `new Number()`/`new String()`, only `Date.now()` from Date API.

**Response helpers**:
- `Response.json(data, init?)`
- `Response.text(text, init?)`
- `Response.html(html, init?)`
- `Response.redirect(url, status?)`

## CLI Options

```
-p, --port <PORT>      Port (default: 8080)
-h, --host <HOST>      Host (default: 127.0.0.1)
-e, --eval <CODE>      Inline JavaScript code
-m, --memory <SIZE>    JS runtime memory limit (default: 0 = no limit)
-n, --pool <N>         Runtime pool size (default: 8)
--cors                 Enable CORS headers
--static <DIR>         Serve static files
```

## JSX Support

JSX/TSX is parsed directly by the zts parser when JSX mode is enabled (based on `.jsx`/`.tsx` file extensions). No separate transformer.

### JSX Runtime (in zts/http.zig)

- `h(tag, props, ...children)` - Creates virtual DOM nodes
- `renderToString(node)` - Renders to HTML string
- `Fragment` - For grouping without wrapper element

## Testing Guidelines

Tests live alongside code using Zig `test "..."` blocks (no separate test directory). Run the relevant `zig build test*` step after changes.

## Code Quality Patterns

**Memory Safety**: All allocations use `errdefer` for cleanup on failure paths. Header strings are duplicated to avoid use-after-free from GC.

**Safe Optional Handling**: Pool and listener access uses `orelse` pattern instead of `?` unwrap.

**Path Traversal Prevention**: `isPathSafe()` in server.zig validates static file paths.

## Zig Idioms

For Zig-specific patterns (allocators, error unions, comptime, data-oriented design), consult `.codex/skills/zig-expert/SKILL.md`.
