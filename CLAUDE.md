# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

zigttp-server is a lightweight HTTP server written in Zig that embeds MicroQuickJS (a minimal JavaScript engine) to execute JavaScript request handlers. Design goals: ~10KB RAM footprint, instant cold starts, zero external dependencies.

**Trade-offs for simplicity**: Single-threaded sequential processing, compile-time configuration, ES5 JavaScript subset (with some ES6).

## Build Commands

```bash
# Build
zig build                           # Debug build
zig build -Doptimize=ReleaseFast   # Optimized build

# Run
zig build run -- -e "function handler(req) { return Response.json({ok:true}); }"
zig build run -- examples/handler.js -p 3000

# Test
zig build test
```

## Architecture

### Core Components

- **main.zig** - CLI argument parsing, creates ServerConfig, starts server
- **server.zig** - HTTP listener, request parsing, connection handling, static file serving, JSX detection
- **runtime.zig** - JS runtime wrapper, RuntimePool for request isolation, Request/Response conversion, JSX runtime
- **mquickjs.zig** - Low-level C bindings with type-safe Zig wrapper around mquickjs API
- **bindings.zig** - Native API implementations (console, fetch, Deno namespace, timers)
- **event_loop.zig** - Async operation management, microtask queue, Promise resolution
- **jsx.zig** - JSX-to-JavaScript transformer for SSR

### Request Flow

1. Server accepts HTTP connection
2. RuntimePool.acquire() gets an isolated JS context
3. Request parsed and converted to JS Request object
4. Handler function invoked with Request
5. JS Response extracted and sent as HTTP response
6. Runtime released back to pool

### Key Patterns

**Runtime Pool**: Pre-allocates JS contexts for per-request isolation. Mutex-protected acquire/release.

**Result Type**: Functional error handling throughout - `Result(T)` union with `ok`/`err` variants. Maps JS exceptions to Zig errors.

**GC Reference Guards**: MicroQuickJS uses compacting GC where object addresses change after allocations. Use `pushGCRef()` to protect values across JS allocations:
```zig
var guard = try mq.pushGCRef(ctx);
defer guard.deinit();
```

**Thread-Local Context**: Native C callbacks access event loop via `threadlocal var current_loop`.

### Memory Model

Fixed buffer architecture - user specifies memory limit (default 512KB), Zig allocates buffer upfront, mquickjs uses it as heap. Each runtime instance has its own buffer.

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

### JSX Runtime (in runtime.zig)

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
