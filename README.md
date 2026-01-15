# zigttp - Serverless JavaScript Runtime using Zig and zts

> **Note**: This project is experimental and under active development.

A serverless JavaScript runtime for FaaS (Function-as-a-Service) use cases,
powered by **zts** - a pure Zig JavaScript engine. Designed for AWS Lambda,
Azure Functions, Cloudflare Workers, and edge computing deployments.

## Features

- **Instant cold starts**: No JIT warm-up by default, predictable startup times
- **Small deployment package**: Pure Zig, zero external dependencies
- **Request isolation**: LockFreePool-backed handler pool with pre-warmed
  contexts
- **Functional API**: Response helpers similar to Deno/Fetch API
- **Safe by default**: Strict mode JavaScript, sandboxed execution
- **TypeScript/TSX**: Native type stripping with compile-time evaluation

## Use Cases

- AWS Lambda / Azure Functions / Cloudflare Workers style deployments
- Edge computing with JavaScript handlers
- Lightweight HTTP function handlers
- Multi-tenant request processing

## Quick Start

### 1. Build

```bash
zig build -Doptimize=ReleaseFast
```

### 2. Run

```bash
# Inline handler
./zig-out/bin/zigttp-server -e "function handler(r) { return Response.json({hello:'world'}) }"

# Or with a handler file
./zig-out/bin/zigttp-server examples/handler.js

# Test it
curl http://localhost:8080/
```

## Usage

```
zigttp-server [options] <handler.js>
zigttp-server -e "<inline-code>"

Options:
  -p, --port <PORT>     Port to listen on (default: 8080)
  -h, --host <HOST>     Host to bind to (default: 127.0.0.1)
  -e, --eval <CODE>     Evaluate inline JavaScript handler
  -m, --memory <SIZE>   JS runtime memory limit (default: 0 = no limit)
  -n, --pool <N>        Runtime pool size (default: 8)
  -q, --quiet           Disable request logging
  --cors                Enable CORS headers
  --static <DIR>        Serve static files from directory
  --help                Show help message
```

## Handler API

Your handler must define a `handler` function that receives a request object and
returns a Response.

### Request Object

```javascript
{
    method: string,     // "GET", "POST", etc.
    url: string,        // URL path (e.g., "/api/users")
    headers: object,    // HTTP headers
    body: string|null   // Request body (for POST/PUT)
}
```

### Response Helpers

```javascript
// JSON response (sets Content-Type automatically)
Response.json(data, init?)

// Text response  
Response.text(text, init?)

// HTML response
Response.html(html, init?)

// Redirect response (default status: 302)
Response.redirect(url, status?)
```

### Example Handler

```javascript
function handler(request) {
    // Simple routing
    if (request.url === "/") {
        return Response.html("<h1>Hello World</h1>");
    }

    if (request.url === "/api/echo") {
        return Response.json({
            method: request.method,
            url: request.url,
            body: request.body,
        });
    }

    if (request.method === "POST" && request.url === "/api/data") {
        const data = JSON.parse(request.body);
        return Response.json({ received: data, ok: true });
    }

    // 404 fallback
    return Response.text("Not Found", { status: 404 });
}
```

## Advanced Server Configuration (Zig API)

If you embed `Server` directly in Zig, these `ServerConfig` fields tune the new
performance features:

```zig
const config = ServerConfig{
    .pool_wait_timeout_ms = 5,
    .pool_metrics_every = 1000,
    .static_cache_max_bytes = 2 * 1024 * 1024,
    .static_cache_max_file_size = 128 * 1024,
};
```

When `pool_metrics_every` is set, logs include a line like:

```
Pool metrics: in_use=2/8 exhausted=0 avg_wait_us=3 max_wait_us=20 avg_exec_us=120 max_exec_us=500
```

Fields are the current in-use count, pool size, exhausted acquisitions, and
average/max wait and execution time (microseconds).

## JSX Support

zigttp-server parses JSX directly (no separate transform step). Use `.jsx` files
to write handlers with JSX syntax.

### Basic JSX

```jsx
// examples/jsx-simple.jsx
function handler(request) {
    const page = <div class="hello">Hello JSX!</div>;
    return Response.html(renderToString(page));
}
```

### Components

```jsx
function Card(props) {
    return (
        <div class="card">
            <h2>{props.title}</h2>
            <div>{props.children}</div>
        </div>
    );
}

function handler(request) {
    const page = <Card title="Welcome">Hello from JSX!</Card>;
    return Response.html(renderToString(page));
}
```

### JSX Runtime API

- **`h(tag, props, ...children)`** - Create virtual DOM node (used internally by
  JSX codegen)
- **`renderToString(node)`** - Render virtual DOM to HTML string
- **`Fragment`** - Fragment component for grouping without wrapper element

### JSX Features

| Feature       | Example              | Output               |
| ------------- | -------------------- | -------------------- |
| Elements      | `<div>text</div>`    | `<div>text</div>`    |
| Attributes    | `<div class="foo">`  | `<div class="foo">`  |
| Expressions   | `<div>{value}</div>` | `<div>...</div>`     |
| Components    | `<Card title="x"/>`  | Calls Card function  |
| Fragments     | `<>a</>`             | `a` (no wrapper)     |
| Self-closing  | `<br/>`              | `<br />`             |
| Boolean attrs | `<input disabled/>`  | `<input disabled />` |

### Full SSR Example

```jsx
// examples/jsx-ssr.jsx
function Layout(props) {
    return (
        <html>
            <head>
                <title>{props.title}</title>
            </head>
            <body>
                <h1>{props.title}</h1>
                {props.children}
            </body>
        </html>
    );
}

function handler(request) {
    const page = (
        <Layout title="My App">
            <p>Method: {request.method}</p>
        </Layout>
    );
    return Response.html(renderToString(page));
}
```

## TypeScript Support

zts includes a native TypeScript/TSX stripper that removes type annotations at
load time. Use `.ts` or `.tsx` files directly without a separate build step.

### Basic Usage

```typescript
// handler.ts
interface Request {
    method: string;
    path: string;
    headers: Record<string, string>;
    body: string | null;
}

function handler(request: Request): Response {
    const data: { message: string } = { message: "Hello TypeScript!" };
    return Response.json(data);
}
```

### Compile-Time Evaluation

The `comptime()` function evaluates expressions at load time and replaces them
with literal values:

```typescript
// Arithmetic
const x = comptime(1 + 2 * 3); // -> const x = 7;

// String operations
const upper = comptime("hello".toUpperCase()); // -> const upper = "HELLO";

// Math functions
const pi = comptime(Math.PI); // -> const pi = 3.141592653589793;
const max = comptime(Math.max(1, 5, 3)); // -> const max = 5;

// Hash function (FNV-1a)
const etag = comptime(hash("content-v1")); // -> const etag = "a1b2c3d4";

// JSON parsing
const cfg = comptime(JSON.parse('{"a":1}')); // -> const cfg = ({a:1});

// TSX works too
const el = <div>{comptime(1 + 2)}</div>; // -> <div>{3}</div>
```

### Supported comptime Operations

| Category   | Operations                                                          |
| ---------- | ------------------------------------------------------------------- |
| Literals   | number, string, boolean, null, undefined, NaN, Infinity             |
| Arithmetic | `+ - * / % **`                                                      |
| Bitwise    | `\| & ^ << >> >>>`                                                  |
| Comparison | `== != === !== < <= > >=`                                           |
| Logical    | `&& \|\| ??`                                                        |
| Ternary    | `cond ? a : b`                                                      |
| Math       | PI, E, floor, ceil, round, sqrt, sin, cos, min, max, etc.           |
| String     | length, toUpperCase, toLowerCase, trim, slice, split, replace, etc. |
| Built-in   | parseInt, parseFloat, JSON.parse, hash                              |

Disallowed: variables, Date.now(), Math.random(), closures, assignments.

See [docs/typescript-comptime-spec.md](docs/typescript-comptime-spec.md) for the
full specification.

## JavaScript Subset

zts implements ES5 with some ES6+ extensions. Key limitations:

- **Strict mode only**: No `with`; `var` is not supported (use `let`/`const`)
- **No array holes**: `[1,,3]` is a syntax error
- **No direct eval**: Only global eval `(1, eval)('code')`
- **No value boxing**: No `new Number(1)`, `new String('x')`
- **Limited Date**: Only `Date.now()` is available

Supported ES6+ features:

- `for...of` (arrays only)
- Typed arrays
- `\u{hex}` in strings
- `Math.imul`, `Math.clz32`, `Math.fround`, `Math.trunc`
- Exponentiation operator (`**`)
- `String.prototype.codePointAt`, `replaceAll`, `trimStart`, `trimEnd`
- `globalThis`

## Architecture

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

### Runtime Model

zts uses a **generational garbage collector** with:

1. NaN-boxing for efficient value representation (64-bit tagged values)
2. Hidden classes for inline caching (V8-style optimization)
3. LockFreePool-backed handler pool for request isolation in FaaS environments
4. Hybrid arena allocation for request-scoped memory with O(1) reset

The Result<T> pattern throughout makes error handling explicit and prevents
silent failures.

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
│   └── comptime.zig       # Compile-time expression evaluator
├── src/
│   ├── main.zig           # CLI entry point
│   ├── zruntime.zig       # HandlerPool, JS context management
│   ├── server.zig         # HTTP server implementation
└── examples/
    ├── handler.jsx        # Example JSX handler
    ├── htmx-todo/         # HTMX Todo app example
    └── jsx-ssr.jsx        # Full SSR example
```

## Building from Source

### Prerequisites

- Zig 0.16.0 or later (nightly)

### Build Commands

```bash
# Debug build
zig build

# Release build (optimized)
zig build -Doptimize=ReleaseFast

# Run tests
zig build test              # Main runtime tests
zig build test-zts          # JS engine tests
zig build test-zruntime     # Native Zig runtime tests

# Run directly
zig build run -- -e "function handler(r) { return Response.json({ok:true}) }"
```

## Extending with Native Functions

Add custom native functions callable from JavaScript by implementing the
`NativeFn` signature in `zts/object.zig`:

```zig
// In a custom module:

fn myNativeFunction(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    // Your implementation
    return zts.JSValue.fromInt(42);
}

// Register it via context.setGlobal()
```

See `zts/builtins.zig` for examples of core JS APIs and native function wiring.

## Performance

### Benchmarks (QuickJS baseline)

zts outperforms QuickJS in our historical benchmark runs (QuickJS is used only
as an external baseline). See `benchmarks/*.json` for raw results.

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

Note: Optional instrumentation (perf), parallel compiler, and JIT modules exist
in `zts/` but are not enabled by default.

### FaaS Optimizations

- **Cold start**: < 1ms to initialize runtime and load handler
- **Warm invocations**: HandlerPool reuses pre-warmed contexts
- **Memory**: 256KB default JS heap (configurable per function)
- **Deployment size**: ~500KB binary, zero runtime dependencies

### Request Pipeline Optimizations

The server includes several optimizations for low-latency request handling:

- **O(1) pool slot acquisition**: `free_hint` atomic tracks likely-free slots, avoiding linear scan
- **Pre-interned HTTP atoms**: Common headers (content-type, host, user-agent, etc.) use predefined atoms for O(1) lookup
- **LRU static cache**: Doubly-linked list eviction instead of clear-all eliminates latency spikes
- **Adaptive backoff**: Three-phase contention handling (spin, sleep with jitter, circuit breaker)
- **Zero-copy response**: Borrowed mode for body and headers avoids memcpy
- **Relaxed atomics**: Metrics-only counters use `.monotonic` ordering

### Hybrid Arena Allocation

For request-scoped workloads, zts uses a hybrid memory model:

- **Arena allocator**: O(1) bulk reset between requests, zero per-object
  overhead
- **Escape detection**: Write barriers prevent arena objects from leaking into
  persistent storage
- **GC disabled in hybrid mode**: No collection pauses during request handling

This design eliminates GC latency spikes in FaaS environments while maintaining
memory safety.

### Deployment Patterns

```bash
# Single instance (Lambda-style)
./zigttp-server handler.js

# Multiple instances behind load balancer
# Each instance handles one request at a time for isolation
```

For high-throughput scenarios, deploy multiple instances. The small binary size
and instant cold starts make horizontal scaling efficient.

## License

MIT licensed.

## Credits

- **zts** - Pure Zig JavaScript engine (part of this project)
- [Zig](https://ziglang.org/) programming language
