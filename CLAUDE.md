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

# Test
zig build test                      # All src/ tests
zig build test-zts             # zts engine tests only
zig build test-zruntime             # Native Zig runtime tests only

# Benchmark
./zig-out/bin/zigttp-bench          # Run benchmarks with mquickjs comparison
```

## Architecture

### Core Components

- **main.zig** - CLI argument parsing, creates ServerConfig, starts server
- **server.zig** - HTTP listener, request parsing, connection handling, static file serving, JSX detection
- **zruntime.zig** - Native Zig runtime: RuntimePool, JS context management, Request/Response conversion, JSX runtime (uses zts)
- **bindings.zig** - Native API implementations (console, fetch, Deno namespace, timers)
- **event_loop.zig** - Async operation management, microtask queue, Promise resolution
- **jsx.zig** - JSX-to-JavaScript transformer for SSR

### zts Engine (Pure Zig)

- **zts/root.zig** - Module entry point, re-exports main types
- **zts/parser.zig** - Tokenizer + direct bytecode emission (no AST)
- **zts/bytecode.zig** - Opcode definitions and FunctionBytecode struct
- **zts/interpreter.zig** - Stack-based bytecode VM
- **zts/value.zig** - NaN-boxing value representation
- **zts/object.zig** - Hidden classes, Atom interning, inline caching
- **zts/string.zig** - JSString and StringTable for string interning
- **zts/context.zig** - JS execution context (globals, stack, atoms)
- **zts/gc.zig** - Generational GC (nursery + tenured)
- **zts/heap.zig** - Size-class segregated allocator
- **zts/arena.zig** - Request-scoped arena allocator with O(1) reset
- **zts/pool.zig** - Lock-free runtime pooling
- **zts/builtins.zig** - Built-in JavaScript functions

### Request Flow

1. Server accepts HTTP connection
2. RuntimePool.acquire() gets an isolated JS context
3. Request parsed and converted to JS Request object
4. Handler function invoked with Request
5. JS Response extracted and sent as HTTP response
6. Runtime released back to pool

### Key Patterns

**Runtime Pool**: Pre-allocates JS contexts for per-request isolation. Mutex-protected acquire/release. Critical for FaaS warm instance reuse.

**Result Type**: Functional error handling throughout - `Result(T)` union with `ok`/`err` variants. Maps JS exceptions to Zig errors.

**NaN-Boxing**: zts uses NaN-boxing for efficient 64-bit tagged values. Allows storing integers, floats, and pointers in a single 64-bit word.

**Hidden Classes**: V8-style hidden class transitions for inline caching. Enables fast property access.

**Thread-Local Context**: Native callbacks access event loop via `threadlocal var current_loop`.

### Memory Model

**Generational GC**: Nursery (young generation) and tenured (old generation) heaps. Each runtime instance has isolated memory. Size-class segregated allocator for efficient small object allocation.

**Hybrid Arena Allocation**: For FaaS workloads, zts supports a hybrid memory model where request-scoped objects are allocated from an arena:

- `arena.zig` provides O(1) bulk reset between requests
- `HybridAllocator` wraps arena with fallback to GC allocator
- Objects track their allocation source via `is_arena` flag in ObjectFlags
- `arena_ptr` field on JSObject enables arena-aware overflow slot allocation
- Write barriers in `setProperty` detect arena escape (storing arena object into persistent object)
- GC is disabled when `hybrid_mode` is true on the GC struct

This eliminates per-object allocation overhead and GC pauses during request handling while preventing use-after-free through escape detection.

## JavaScript Runtime

**Supported**: ES5 + `for...of` (arrays), typed arrays, `**` operator, `globalThis`, string methods (replaceAll, trimStart/End), Math extensions.

**Limitations**: Strict mode only, no `with`, no array holes, no `new Number()`/`new String()`, only `Date.now()` from Date API.

**Response helpers** available in handlers:
- `Response.json(data, init?)`
- `Response.text(text, init?)`
- `Response.html(html, init?)`
- `Response.redirect(url, status?)`

## CLI Options

```
-p, --port <PORT>      Port (default: 8080)
-h, --host <HOST>      Host (default: 127.0.0.1)
-e, --eval <CODE>      Inline JavaScript code
-m, --memory <SIZE>    JS runtime memory (default: 512k)
-n, --pool <N>         Runtime pool size (default: 8)
--cors                 Enable CORS headers
--static <DIR>         Serve static files
```

## JSX Transformer

**jsx.zig** provides a native JSX-to-JavaScript transformer for SSR (Server-Side Rendering).

### Architecture

- Single-pass tokenizer/transformer (no AST)
- Outputs `h(tag, props, ...children)` calls (hyperscript pattern)
- ES5-compatible output (var, no arrow functions)

### Transform Flow

1. Server.init() detects `.jsx` extension
2. jsx.transform() processes source code
3. JSX elements → h() calls, JS code → pass-through
4. Transformed code loaded into RuntimePool

### JSX Runtime (in zruntime.zig)

- `h(tag, props, ...children)` - Creates virtual DOM nodes
- `renderToString(node)` - Renders to HTML string
- `Fragment` - For grouping without wrapper element
- Void elements (br, img, input, etc.) rendered as self-closing

### Key Implementation Details

- Lowercase tags = string literals (`'div'`)
- Uppercase tags = component references (`Card`)
- Null terminator appended to transformed code for C API compatibility
- Brace depth tracking for expressions like `{items.map(...)}`

## Security Features

**Path Traversal Prevention**: `isPathSafe()` in server.zig validates static file paths - blocks `..`, absolute paths, and Windows drive letters.

**SSRF Prevention**: `isUrlAllowed()` in bindings.zig validates fetch URLs - blocks `file://`, localhost, 127.0.0.1, private IP ranges (10.x, 192.168.x, 172.16-31.x).

**Deno API Sandboxing**: All file operations (readTextFile, writeTextFile, remove, mkdir, stat, readDir) validate paths with `isFilePathSafe()`.

## Code Quality Patterns

**Memory Safety**: All allocations use `errdefer` for cleanup on failure paths. Header strings are duplicated to avoid use-after-free from GC.

**Buffered I/O**: `BufferedReader` in server.zig reduces syscalls by ~90% compared to byte-by-byte reading.

**Safe Optional Handling**: Pool and listener access uses `orelse` pattern instead of `?` unwrap to handle uninitialized state gracefully.
