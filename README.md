# zigttp - Serverless JavaScript Runtime using Zig and zts

> **Note**: This project is experimental and under active development.

A serverless JavaScript runtime for FaaS (Function-as-a-Service) use cases, powered by **zts** - a pure Zig JavaScript engine. Designed for AWS Lambda, Azure Functions, Cloudflare Workers, and edge computing deployments.

## Features

- **Instant cold starts**: No JIT warm-up, predictable startup times
- **Small deployment package**: Pure Zig, zero external dependencies
- **Request isolation**: RuntimePool with pre-warmed contexts
- **Functional API**: Response helpers similar to Deno/Fetch API
- **Safe by default**: Strict mode JavaScript, sandboxed execution

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
  -m, --memory <SIZE>   JS runtime memory limit (default: 256k)
  -q, --quiet           Disable request logging
  --help                Show help message
```

## Handler API

Your handler must define a `handler` function that receives a request object and returns a Response.

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
// Basic response
new Response(body, { status: 200, headers: {} })

// JSON response (sets Content-Type automatically)
Response.json(data, init?)

// Text response  
Response.text(text, init?)

// HTML response
Response.html(html, init?)
```

### Example Handler

```javascript
function handler(request) {
    // Simple routing
    if (request.url === '/') {
        return Response.html('<h1>Hello World</h1>');
    }

    if (request.url === '/api/echo') {
        return Response.json({
            method: request.method,
            url: request.url,
            body: request.body
        });
    }

    if (request.method === 'POST' && request.url === '/api/data') {
        var data = JSON.parse(request.body);
        return Response.json({ received: data, ok: true });
    }

    // 404 fallback
    return new Response('Not Found', { status: 404 });
}
```

## JSX Support

zigttp-server includes a native JSX transformer for server-side rendering. Use `.jsx` files to write handlers with JSX syntax.

### Basic JSX

```jsx
// examples/jsx-simple.jsx
function handler(request) {
    var page = <div class="hello">Hello JSX!</div>;
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
    var page = <Card title="Welcome">Hello from JSX!</Card>;
    return Response.html(renderToString(page));
}
```

### JSX Runtime API

- **`h(tag, props, ...children)`** - Create virtual DOM node (used internally by transformer)
- **`renderToString(node)`** - Render virtual DOM to HTML string
- **`Fragment`** - Fragment component for grouping without wrapper element

### JSX Features

| Feature | Example | Output |
|---------|---------|--------|
| Elements | `<div>text</div>` | `<div>text</div>` |
| Attributes | `<div class="foo">` | `<div class="foo">` |
| Expressions | `<div>{value}</div>` | `<div>...</div>` |
| Components | `<Card title="x"/>` | Calls Card function |
| Fragments | `<>a</>` | `a` (no wrapper) |
| Self-closing | `<br/>` | `<br />` |
| Boolean attrs | `<input disabled/>` | `<input disabled />` |

### Full SSR Example

```jsx
// examples/jsx-ssr.jsx
function Layout(props) {
    return (
        <html>
            <head><title>{props.title}</title></head>
            <body>
                <h1>{props.title}</h1>
                {props.children}
            </body>
        </html>
    );
}

function handler(request) {
    var page = (
        <Layout title="My App">
            <p>Method: {request.method}</p>
        </Layout>
    );
    return Response.html(renderToString(page));
}
```

## JavaScript Subset

zts implements ES5 with some ES6+ extensions. Key limitations:

- **Strict mode only**: No `with`, globals must be declared with `var`
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
│  │ HTTP Server │──│ RuntimePool │──│  Native Bindings    │  │
│  │  (std.net)  │  │  (contexts) │  │ (console, Response) │  │
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
3. RuntimePool for request isolation in FaaS environments

The Result<T> pattern throughout makes error handling explicit and prevents silent failures.

## Project Structure

```
zigttp-server/
├── build.zig              # Zig build configuration
├── zts/              # Pure Zig JavaScript engine
│   ├── parser.zig         # Tokenizer + bytecode compiler
│   ├── interpreter.zig    # Stack-based VM
│   ├── value.zig          # NaN-boxing value representation
│   ├── object.zig         # Hidden classes, object system
│   ├── gc.zig             # Generational GC
│   └── heap.zig           # Size-class allocator
├── mquickjs/              # Legacy C engine (benchmarks only)
├── src/
│   ├── main.zig           # CLI entry point
│   ├── runtime.zig        # RuntimePool, JS context management
│   ├── server.zig         # HTTP server implementation
│   ├── bindings.zig       # Native APIs (console, fetch, Deno)
│   └── jsx.zig            # JSX transformer
└── examples/
    ├── handler.js         # Example JavaScript handler
    ├── handler.jsx        # Same example using JSX
    ├── htmx-todo/         # HTMX Todo app example
    └── jsx-ssr.jsx        # Full SSR example
```

## Building from Source

### Prerequisites

- Zig 0.15.0 or later

### Build Commands

```bash
# Debug build
zig build

# Release build (optimized)
zig build -Doptimize=ReleaseFast

# Run tests
zig build test

# Run directly
zig build run -- -e "function handler(r) { return Response.json({ok:true}) }"
```

## Extending with Native Functions

Add custom C functions callable from JavaScript:

```zig
// In runtime.zig, add to installBindings():

fn myNativeFunction(ctx: *mq.JSContext, this: mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.C) mq.JSValue {
    // Your implementation
    return mq.fromInt(42);
}

// Register it:
const my_fn = mq.newCFunction(ctx, myNativeFunction, "myFunction", 0);
_ = mq.setPropertyStr(ctx, global, "myFunction", my_fn.ok);
```

## Performance for FaaS

- **Cold start**: < 1ms to initialize runtime and load handler
- **Warm invocations**: RuntimePool reuses pre-warmed contexts
- **Memory**: 256KB default JS heap (configurable per function)
- **Deployment size**: ~500KB binary, zero runtime dependencies

### Deployment Patterns

```bash
# Single instance (Lambda-style)
./zigttp-server handler.js

# Multiple instances behind load balancer
# Each instance handles one request at a time for isolation
```

For high-throughput scenarios, deploy multiple instances. The small binary size and instant cold starts make horizontal scaling efficient.

## License

MIT licensed.

## Credits

- **zts** - Pure Zig JavaScript engine (part of this project)
- [Zig](https://ziglang.org/) programming language
- [MicroQuickJS](https://github.com/bellard/mquickjs) by Fabrice Bellard (legacy benchmarking only)
