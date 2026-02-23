# zigttp Architecture

This document describes the architecture of zigttp-server, a serverless JavaScript runtime powered by the zts JavaScript engine.

## Design Philosophy

**Goal**: Instant cold starts, small deployment package, request isolation, zero external dependencies.

**Trade-offs for FaaS**: Single-threaded sequential processing (one request per instance), compile-time configuration, ES5 JavaScript subset with select ES6 features.

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     zigttp-server (Zig)                       │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │ HTTP Server │──│ HandlerPool │──│  Builtins/HTTP     │  │
│  │  (std.net)  │  │  (contexts) │  │  (Response, h())   │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
├─────────────────────────────────────────────────────────────┤
│                    zts (Pure Zig)                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │   Parser    │──│  Bytecode   │──│  Generational GC    │  │
│  │             │  │     VM      │  │ (Nursery + Tenured) │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Two-Layer Design

### Server Layer (src/)

HTTP listener, CLI, request routing, static file serving.

**Key Components**:
- `main.zig` - Entry point, CLI argument parsing
- `server.zig` - HTTP request/response handling, static file cache
- `zruntime.zig` - HandlerPool management, JS context lifecycle

### Engine Layer (zts/)

Pure Zig JavaScript engine with two-pass compilation (parse to IR, then bytecode).

**Key Components**:

#### Parser (`zts/parser/`)
- `parse.zig` - Pratt parser for JavaScript/TypeScript
- `tokenizer.zig` - Lexical analysis
- `codegen.zig` - Bytecode generation from IR
- `ir.zig` - Intermediate representation

#### VM and Runtime
- `interpreter.zig` - Stack-based bytecode interpreter with JIT baseline compiler
- `value.zig` - NaN-boxing value representation
- `object.zig` - Hidden classes, object system, property access
- `builtins.zig` - Built-in JavaScript functions and APIs
- `http.zig` - HTTP runtime (Request, Response) and JSX runtime (h, renderToString)

#### Memory Management
- `gc.zig` - Generational GC (nursery + tenured spaces)
- `heap.zig` - Size-class segregated allocator
- `arena.zig` - Request-scoped arena allocator
- `pool.zig` - Lock-free runtime pooling

#### Language Extensions
- `stripper.zig` - TypeScript/TSX type annotation stripper
- `comptime.zig` - Compile-time expression evaluator

#### JIT Compilation
- `jit/baseline.zig` - Baseline JIT compiler for x86-64 and ARM64
- `type_feedback.zig` - Call site and value type profiling

## Request Flow

1. Server accepts HTTP connection (`server.zig`)
2. HandlerPool acquires an isolated runtime from LockFreePool (`zruntime.zig`)
3. Request parsed and converted to JS Request object
4. Handler function invoked with Request
5. JS Response extracted and sent as HTTP response
6. Runtime released back to pool

## Key Patterns

### Handler Pool

Pre-allocates JS contexts for per-request isolation using `LockFreePool`. Critical for FaaS warm instance reuse.

**Configuration**:
- Default pool size: `2 * cpu_count` (minimum 8)
- Configurable via `-n` flag or `ServerConfig.pool_size`
- Each context is pre-warmed and reusable

### Result Type

Functional error handling throughout using `Result(T)` union with `ok`/`err` variants. Maps JS exceptions to Zig errors.

```zig
const Result = union(enum) {
    ok: T,
    err: Error,
};
```

### NaN-Boxing

64-bit tagged values storing integers, floats, and pointers in a single word. Enables efficient value representation without heap allocation for primitives.

**Layout**:
- Integers: 48-bit signed int in low bits, tag in high bits
- Floats: IEEE 754 double precision (NaN values used for tagging)
- Pointers: 48-bit pointer with tag in high bits

### Hidden Classes

V8-style hidden class transitions for inline caching. Enables fast property access through predictable object shapes.

**Shape Preallocation**: HTTP Request and Response objects use preallocated hidden class shapes (`zts/context.zig:352-434`), eliminating hidden class transitions. Direct slot writes via `setSlot()` bypass property lookup entirely.

### Hybrid Arena Allocation

Request-scoped arena with O(1) bulk reset. Write barriers detect arena escape. GC disabled in hybrid mode.

**Benefits**:
- No per-object deallocation overhead
- Predictable memory usage per request
- Zero GC pauses during request handling
- Write barriers prevent arena objects from leaking to persistent storage

## Runtime Model

zts uses a **generational garbage collector** with:

1. **NaN-boxing** for efficient value representation (64-bit tagged values)
2. **Hidden classes** for inline caching (V8-style optimization)
3. **LockFreePool-backed handler pool** for request isolation in FaaS environments
4. **Hybrid arena allocation** for request-scoped memory with O(1) reset

The Result<T> pattern throughout makes error handling explicit and prevents silent failures.

## Project Structure

```
zigttp-server/
├── build.zig              # Zig build configuration
├── zts/                   # Pure Zig JavaScript engine
│   ├── parser/            # Two-pass parser with IR
│   │   ├── parse.zig      # Main parser (Pratt parser)
│   │   ├── tokenizer.zig  # Tokenizer
│   │   ├── codegen.zig    # Bytecode generation
│   │   └── ir.zig         # Intermediate representation
│   ├── interpreter.zig    # Stack-based bytecode VM
│   ├── value.zig          # NaN-boxing value representation
│   ├── object.zig         # Hidden classes, object system
│   ├── gc.zig             # Generational GC (nursery + tenured)
│   ├── heap.zig           # Size-class segregated allocator
│   ├── arena.zig          # Request-scoped arena allocator
│   ├── http.zig           # HTTP/JSX runtime for SSR
│   ├── pool.zig           # Lock-free runtime pooling
│   ├── builtins.zig       # Built-in JavaScript functions
│   ├── stripper.zig       # TypeScript/TSX type stripper
│   ├── comptime.zig       # Compile-time expression evaluator
│   ├── jit/
│   │   └── baseline.zig   # Baseline JIT compiler (x86-64, ARM64)
│   └── type_feedback.zig  # Call site profiling
├── src/
│   ├── main.zig           # CLI entry point
│   ├── zruntime.zig       # HandlerPool, JS context management
│   ├── server.zig         # HTTP server implementation
├── tools/
│   └── precompile.zig     # Build-time bytecode compiler
└── examples/
    ├── handler.jsx        # Example JSX handler
    ├── htmx-todo/         # HTMX Todo app example
    └── jsx-ssr.jsx        # Full SSR example
```

## Memory Safety

All allocations use `errdefer` for cleanup on failure paths. Header strings are duplicated to avoid use-after-free from GC.

**Safe Optional Handling**: Pool and listener access uses `orelse` pattern instead of `?` unwrap.

**Path Traversal Prevention**: `isPathSafe()` in server.zig validates static file paths.

## Performance Architecture

### Property Access Optimizations

**Polymorphic Inline Cache (PIC)** (`zts/interpreter.zig:259-335`): 8-entry cache per property access site with last-hit optimization for O(1) monomorphic lookups. Megamorphic transition after 9th distinct shape.

**Binary Search for Large Objects** (`zts/object.zig:751, 831-835`): Objects with 8+ properties use binary search on sorted property arrays. Threshold: `BINARY_SEARCH_THRESHOLD = 8`.

**JIT Baseline IC Integration** (`zts/jit/baseline.zig:1604-1765`): x86-64 and ARM64 JIT fast paths check PIC entry[0] inline, falling back to helper only on cache miss.

**JIT Object Literal Shapes** (`zts/context.zig:746-779`, `zts/jit/baseline.zig:3646-3670`): Object literals with static keys use pre-compiled hidden class shapes. The `new_object_literal` opcode creates objects with the final hidden class directly (no transitions), and `set_slot` writes property values to inline slots without lookup overhead.

### String Optimizations

**Lazy String Hashing** (`zts/string.zig:18-24, 44-54`): Hash computation deferred until needed. `hash_computed` flag tracks state; `getHash()`/`getHashConst()` compute on first access.

**Pre-interned HTTP Atoms** (`zts/object.zig:237-264`): 27 common headers with O(1) lookup (content-type, content-length, accept, host, user-agent, authorization, cache-control, CORS headers, etc.).

**HTTP String Cache** (`zts/context.zig:111-135, 462+`): Pre-allocated status texts (OK, Created, Not Found, etc.), content-type strings (application/json, text/plain, text/html), and HTTP method strings (GET, POST, PUT, etc.).

### Pool and Request Optimizations

**Pool Slot Hint** (`zts/pool.zig`): `free_hint` atomic reduces slot acquisition from O(N) linear scan to O(1) in the common case.

**Relaxed Atomic Ordering** (`src/zruntime.zig`): The `in_use` counter uses `.monotonic` ordering since it's only for metrics/limits, not synchronization.

**LRU Static Cache** (`src/server.zig`): Static file cache uses doubly-linked list LRU eviction instead of clear-all, eliminating latency spikes.

**Adaptive Backoff** (`src/zruntime.zig`): Three-phase backoff for pool contention:
- Phase 1: 10 spin iterations using `spinLoopHint`
- Phase 2: Sleep 10us-1ms with jitter (prevents thundering herd)
- Phase 3: Circuit breaker fails fast after 100 retries

**Zero-Copy Response** (`src/zruntime.zig`): Borrowed mode for both body and headers avoids memcpy when arena lifetime is guaranteed.

### Build-Time Precompilation

**Handler Precompilation** (`tools/precompile.zig`, `build.zig`): The `-Dhandler=<path>` build option compiles JavaScript handlers at build time. Bytecode is embedded directly into the binary, eliminating runtime parsing entirely.

Build flow: `precompile.zig` uses full zts engine to compile, serialize bytecode with atoms and shapes, and generate `src/generated/embedded_handler.zig`. The server loads this bytecode directly via `loadFromCachedBytecode()`.

## Deployment Patterns

### Single Instance (Lambda-style)

```bash
./zigttp-server handler.js
```

Each instance handles one request at a time for isolation.

### Multiple Instances Behind Load Balancer

For high-throughput scenarios, deploy multiple instances. The small binary size (~500KB) and instant cold starts make horizontal scaling efficient.

### Container Deployment

```dockerfile
FROM scratch
COPY zig-out/bin/zigttp-server /zigttp-server
COPY handler.js /handler.js
EXPOSE 8080
ENTRYPOINT ["/zigttp-server", "-q", "-h", "0.0.0.0", "/handler.js"]
```

### FaaS Deployment

Build for target platform:

```bash
# AWS Lambda (x86-64)
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux

# AWS Lambda (ARM64)
zig build -Doptimize=ReleaseFast -Dtarget=aarch64-linux

# With precompiled handler (fastest)
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux -Dhandler=handler.js
```

Package as Lambda deployment:

```bash
zip function.zip bootstrap handler.js
aws lambda create-function --function-name my-function \
  --zip-file fileb://function.zip --runtime provided.al2 \
  --handler handler.handler --role arn:aws:iam::...
```
