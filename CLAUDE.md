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

# Benchmark
zig build bench                     # Build and run benchmarks
```

## Architecture

### Core Components (src/)

- **main.zig** - CLI argument parsing, creates ServerConfig, starts server
- **server.zig** - HTTP listener, request parsing, connection handling, static file serving
- **zruntime.zig** - Native Zig runtime: HandlerPool, JS context management, Request/Response conversion

### zts Engine (Pure Zig)

The JS engine uses a two-pass architecture: parse to IR, then generate bytecode.

**Parser (zts/parser/):**
- **root.zig** - Parser module entry point
- **parse.zig** - Pratt parser, builds IR
- **tokenizer.zig** - Lexical analysis
- **ir.zig** - Intermediate representation
- **codegen.zig** - IR to bytecode emission
- **scope.zig** - Scope and variable tracking

**Runtime (zts/):**
- **root.zig** - Module entry point, re-exports main types
- **interpreter.zig** - Stack-based bytecode VM
- **bytecode.zig** - Opcode definitions and FunctionBytecode struct
- **value.zig** - NaN-boxing value representation
- **object.zig** - Hidden classes, Atom interning, inline caching
- **string.zig** - JSString and StringTable for string interning
- **context.zig** - JS execution context (globals, stack, atoms)
- **builtins.zig** - Built-in JavaScript functions
- **http.zig** - Request/Response classes, h() hyperscript, renderToString() for SSR

**Memory Management (zts/):**
- **gc.zig** - Generational GC (nursery + tenured)
- **heap.zig** - Size-class segregated allocator
- **arena.zig** - Request-scoped arena allocator with O(1) reset
- **pool.zig** - Lock-free runtime pooling

**TypeScript Support (zts/):**
- **stripper.zig** - TypeScript/TSX type annotation removal
- **comptime.zig** - Compile-time expression evaluator for `comptime()` calls

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

**NaN-Boxing**: zts uses NaN-boxing for efficient 64-bit tagged values. Allows storing integers, floats, and pointers in a single 64-bit word.

**Hidden Classes**: V8-style hidden class transitions for inline caching. Enables fast property access.

**Thread-Local Runtime**: Native callbacks can access the current runtime via thread-local storage when needed.

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

## TypeScript/TSX Support

zts includes a native TypeScript/TSX stripper that removes type annotations at load time. Use `.ts` or `.tsx` files directly without a separate build step.

### Compile-Time Evaluation

The `comptime()` function evaluates expressions at load time and replaces them with literal values:

```typescript
const x = comptime(1 + 2 * 3);                 // -> const x = 7;
const upper = comptime("hello".toUpperCase()); // -> const upper = "HELLO";
const pi = comptime(Math.PI);                  // -> const pi = 3.141592653589793;
const etag = comptime(hash("content-v1"));     // -> const etag = "a1b2c3d4";
```

**Supported**: Literals, arithmetic, bitwise, comparison, logical operators, ternary, Math constants/functions, string methods, `parseInt`, `parseFloat`, `JSON.parse`, `hash`.

**Disallowed**: Variables, `Date.now()`, `Math.random()`, closures, assignments.

See [docs/typescript-comptime-spec.md](docs/typescript-comptime-spec.md) for full specification.

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
-m, --memory <SIZE>    JS runtime memory (default: 256k)
-n, --pool <N>         Runtime pool size (default: 8)
--cors                 Enable CORS headers
--static <DIR>         Serve static files
```

## JSX Support

JSX/TSX is parsed directly by the zts parser when JSX mode is enabled (based on `.jsx`/`.tsx` file extensions in `zruntime.zig`). There is no separate transformer file.

### Flow

1. `Runtime.loadCode()` enables JSX mode for `.jsx`/`.tsx`
2. Parser produces JSX nodes
3. Codegen emits `h(tag, props, ...children)` calls

### JSX Runtime (in zts/http.zig)

- `h(tag, props, ...children)` - Creates virtual DOM nodes
- `renderToString(node)` - Renders to HTML string
- `Fragment` - For grouping without wrapper element
- Void elements (br, img, input, etc.) rendered as self-closing

## Security Features

**Path Traversal Prevention**: `isPathSafe()` in server.zig validates static file paths - blocks `..`, absolute paths, and Windows drive letters.

## Code Quality Patterns

**Memory Safety**: All allocations use `errdefer` for cleanup on failure paths. Header strings are duplicated to avoid use-after-free from GC.

**Buffered I/O**: `BufferedReader` in server.zig reduces syscalls compared to byte-by-byte reading.

**Safe Optional Handling**: Pool and listener access uses `orelse` pattern instead of `?` unwrap to handle uninitialized state gracefully.
