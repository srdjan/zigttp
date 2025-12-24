# mqjs-server

> **Note**: This project is experimental and under active development.

A lightweight HTTP server written in Zig that embeds [MicroQuickJS](https://github.com/bellard/mquickjs) (mquickjs) for JavaScript request handlers.


## Features

- **Minimal footprint**: mquickjs runs JavaScript in as low as 10KB RAM
- **Fast startup**: No JIT compilation, instant cold starts
- **Zero dependencies**: Just Zig and the mquickjs C sources
- **Functional API**: Response helpers similar to Deno/Fetch API
- **Safe by default**: Strict mode JavaScript, no eval of local variables

## Quick Start

### 1. Build

```bash
zig build -Doptimize=ReleaseFast
```

### 2. Run

```bash
# Inline handler
./zig-out/bin/mqjs-server -e "function handler(r) { return Response.json({hello:'world'}) }"

# Or with a handler file
./zig-out/bin/mqjs-server examples/handler.js

# Test it
curl http://localhost:8080/
```

## Usage

```
mqjs-server [options] <handler.js>
mqjs-server -e "<inline-code>"

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

mqjs-server includes a native JSX transformer for server-side rendering. Use `.jsx` files to write handlers with JSX syntax.

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

mquickjs implements ES5 with some ES6+ extensions. Key limitations:

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
│                     mqjs-server (Zig)                       │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │ HTTP Server │──│   Runtime   │──│  Native Bindings    │  │
│  │  (std.net)  │  │  Wrapper    │  │ (console, Response) │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
├─────────────────────────────────────────────────────────────┤
│                    mquickjs (C)                             │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │   Parser    │──│  Bytecode   │──│  Tracing GC         │  │
│  │             │  │     VM      │  │  (Compacting)       │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Memory Model

mquickjs uses a **compacting garbage collector** which means:

1. Object addresses can change after any JS allocation
2. The Zig bindings use `GCRef` guards for stable references
3. String slices returned from JS are only valid until the next allocation

This is why the bindings use a Result type pattern - to make error handling explicit and avoid dangling pointers.

## Project Structure

```
mqjs-server/
├── build.zig              # Zig build configuration
├── mquickjs/              # Vendored mquickjs sources (from bellard/mquickjs)
├── src/
│   ├── main.zig           # CLI entry point
│   ├── mquickjs.zig       # Low-level C bindings
│   ├── runtime.zig        # High-level JS runtime wrapper
│   ├── server.zig         # HTTP server implementation
│   └── jsx.zig            # JSX transformer
└── examples/
    ├── handler.js         # Example JavaScript handler
    ├── handler.jsx        # Same example using JSX
    ├── htmx-todo/         # HTMX Todo app example
    ├── jsx-simple.jsx     # Simple JSX example
    ├── jsx-component.jsx  # Component JSX example
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

## Performance Notes

- **Startup**: < 1ms to initialize runtime and load handler
- **Memory**: 256KB default JS heap (configurable)
- **Requests**: Single-threaded, sequential processing
- **Compute**: Comparable to QuickJS (~50% of V8 for compute-bound)

For high-throughput scenarios, consider running multiple instances behind a load balancer.

## License

This Zig server host is MIT licensed.
mquickjs is MIT licensed (Copyright Fabrice Bellard & Charlie Gordon).

## Credits

- [MicroQuickJS](https://github.com/bellard/mquickjs) by Fabrice Bellard
- [Zig](https://ziglang.org/) programming language
