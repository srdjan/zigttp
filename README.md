# zigttp - Serverless JavaScript Runtime

> **Note**: THIS PROJECT IS EXPERIMENTAL AND UNDER ACTIVE DEVELOPMENT.

A high-performance serverless JavaScript runtime for FaaS deployments, powered by **zts** - a pure Zig JavaScript engine. Designed for AWS Lambda, Azure Functions, Cloudflare Workers, and edge computing.

## Why zigttp?

### Langugage Goals: Light Functional TypeScript subset
- Support 'light functional programming style' with the strict subset of TypeScript
- make JSX first class citizen in zts.
- Native TypeScript "comptime" support, a-la Zig
- Use Zig asynchronous I/O behind the scenes 

### Runtime Goals:
- Experimient with AOT compilation for TypeScript/JavaScript handlers in zigttp, while preserving the JIT/interpreter for strictly dynamic runtime scenarios.
- **Fast cold starts**: 3ms runtime init, ~100ms total on macOS (2-3x faster than Deno)
- **Future Linux target**: Sub-20ms cold starts via static linking (planned)
- **Small footprint**: 1.2MB binary, 4MB memory, zero runtime dependencies
- Request isolation via pre-warmed handler pool

### Perfomance

<img src="docs/bench.jpg" alt="Performance benchmark" width="90%">

## Quick Start

Build with Zig 0.16.0 or later:

```bash
zig build -Doptimize=ReleaseFast

# Run with inline handler
./zig-out/bin/zigttp-server -e "function handler(r) { return Response.json({hello:'world'}) }"

# Or with a handler file
./zig-out/bin/zigttp-server examples/handler.ts
```

Test it:

```bash
curl http://localhost:8080/
```

## Handler Example

```jsx
function HomePage() {
    return (
        <html>
            <head>
                <title>Hello World</title>
            </head>
            <body>
                <h1>Hello World</h1>
                <p>Welcome to zigttp-server!</p>
            </body>
        </html>
    );
}

function handler(request) {
    if (request.url === "/") {
        return Response.html(renderToString(<HomePage />));
    }

    if (request.url === "/api/echo") {
        return Response.json({
            method: request.method,
            url: request.url,
            body: request.body,
        });
    }

    return Response.text("Not Found", { status: 404 });
}
```

## HTMX Example

zigttp includes native support for HTMX attributes in JSX:

```jsx
function TodoForm() {
    return (
        <form
            hx-post="/todos"
            hx-target="#todo-list"
            hx-swap="beforeend">
            <input type="text" name="text" required />
            <button type="submit">Add Todo</button>
        </form>
    );
}

function handler(request) {
    if (request.url === "/" && request.method === "GET") {
        return Response.html(renderToString(<TodoForm />));
    }

    if (request.url === "/todos" && request.method === "POST") {
        // Parse form data, create todo item
        const todoHtml = renderToString(
            <div class="todo-item">New todo item</div>
        );
        return Response.html(todoHtml);
    }

    return Response.text("Not Found", { status: 404 });
}
```

See [examples/htmx-todo/](examples/htmx-todo/) for a complete HTMX application.

## Virtual Modules

zigttp provides native virtual modules via `import { ... } from "zigttp:*"` syntax. These run as native Zig code with zero JS interpretation overhead.

### Available Modules

| Module | Exports | Description |
|--------|---------|-------------|
| `zigttp:env` | `env` | Environment variable access |
| `zigttp:crypto` | `sha256`, `hmacSha256`, `base64Encode`, `base64Decode` | Cryptographic functions |
| `zigttp:router` | `routerMatch` | Pattern-matching HTTP router |
| `zigttp:auth` | `parseBearer`, `jwtVerify`, `jwtSign`, `verifyWebhookSignature`, `timingSafeEqual` | JWT auth and webhook verification |
| `zigttp:validate` | `schemaCompile`, `validateJson`, `validateObject`, `coerceJson`, `schemaDrop` | JSON Schema validation |
| `zigttp:cache` | `cacheGet`, `cacheSet`, `cacheDelete`, `cacheIncr`, `cacheStats` | In-memory key-value cache with TTL and LRU |

### Auth Example

```typescript
import { parseBearer, jwtVerify, jwtSign } from "zigttp:auth";

function handler(req: Request): Response {
    const token = parseBearer(req.headers["authorization"]);
    if (!token) return Response.json({ error: "unauthorized" }, { status: 401 });

    const result = jwtVerify(token, "my-secret");
    if (!result.ok) return Response.json({ error: result.error }, { status: 401 });

    return Response.json({ user: result.value });
}
```

### Validation Example

```typescript
import { schemaCompile, validateJson } from "zigttp:validate";

schemaCompile("user", JSON.stringify({
    type: "object",
    required: ["name", "email"],
    properties: {
        name: { type: "string", minLength: 1, maxLength: 100 },
        email: { type: "string", minLength: 5 },
        age: { type: "integer", minimum: 0, maximum: 200 }
    }
}));

function handler(req: Request): Response {
    if (req.method === "POST") {
        const result = validateJson("user", req.body);
        if (!result.ok) return Response.json({ errors: result.errors }, { status: 400 });
        return Response.json({ user: result.value }, { status: 201 });
    }
    return Response.json({ ok: true });
}
```

### Cache Example

```typescript
import { cacheGet, cacheSet, cacheStats } from "zigttp:cache";

function handler(req: Request): Response {
    const cached = cacheGet("api", req.url);
    if (cached) return Response.json(JSON.parse(cached));

    const data = { message: "computed", path: req.url };
    cacheSet("api", req.url, JSON.stringify(data), 60); // TTL: 60 seconds

    return Response.json(data);
}
```

See [examples/modules_all.ts](examples/modules_all.ts) for an integration example using all modules together.

## CLI Options

```bash
zigttp-server [options] <handler.js>

Options:
  -p, --port <PORT>     Port (default: 8080)
  -h, --host <HOST>     Host (default: 127.0.0.1)
  -e, --eval <CODE>     Inline JavaScript handler
  -m, --memory <SIZE>   JS runtime memory limit (default: 256k)
  -n, --pool <N>        Runtime pool size (default: auto)
  --cors                Enable CORS headers
  --static <DIR>        Serve static files
  --outbound-http       Enable native outbound bridge (httpRequest)
  --outbound-host <H>   Restrict outbound bridge to exact host H
  --outbound-timeout-ms Connect timeout for outbound bridge (ms)
  --outbound-max-response <SIZE>
```

## Key Features

**Performance**: NaN-boxing, hidden classes with inline caching, polymorphic inline cache (PIC), generational GC, hybrid arena allocation for request-scoped workloads.

**HTTP/FaaS Optimizations**: Shape preallocation for Request/Response objects, pre-interned HTTP atoms, HTTP string caching, LockFreePool handler isolation, zero-copy response mode.

**Language Support**: ES5 + select ES6 features (for...of, typed arrays, exponentiation), native TypeScript/TSX stripping, compile-time evaluation with `comptime()`, direct JSX parsing.

**JIT Compilation**: Baseline JIT for x86-64 and ARM64, inline cache integration, object literal shapes, type feedback, adaptive compilation.

**Virtual Modules**: Native `zigttp:auth` (JWT/HS256, webhook signatures), `zigttp:validate` (JSON Schema), `zigttp:cache` (TTL/LRU key-value store), plus `zigttp:env`, `zigttp:crypto`, `zigttp:router`.

**Developer Experience**: Fetch-like Response API (Response.json, Response.text, Response.html), console methods (log, error, warn, info, debug), static file serving with LRU cache, CORS support, pool metrics.

## Native Outbound Bridge

When enabled with `--outbound-http`, handlers can call:

```javascript
const raw = httpRequest(JSON.stringify({
  url: "http://127.0.0.1:8787/v1/ops?view=state",
  method: "GET",
  headers: { Authorization: "Bearer ..." }
}));
const resp = JSON.parse(raw);
```

`httpRequest` returns JSON with either `{ ok: true, status, reason, body, content_type? }` or `{ ok: false, error, details }`.
Use `--outbound-host` to restrict egress to a single host.

## Build-Time Precompilation

For production deployments, precompile handlers at build time to eliminate all runtime parsing and achieve the fastest cold starts:

```bash
# Development build (runtime handler loading)
zig build -Doptimize=ReleaseFast

# Production build (embedded bytecode, 16% faster cold starts)
zig build -Doptimize=ReleaseFast -Dhandler=examples/handler.ts
```

**Cold Start Performance**:

| Platform | Cold Start | Runtime Init | Status |
|----------|------------|--------------|--------|
| **macOS** (development) | ~103ms | 3ms | ✅ Current |
| **Linux** (production) | ~18-33ms (planned) | 3ms | 🚧 Future |

**macOS Performance** (development environment):
- Total cold start: ~103ms (2-3x faster than Deno)
- Runtime initialization: 3ms (highly optimized)
- dyld overhead: 80-90ms (unavoidable on macOS, affects all binaries)
- Competitive for development, acceptable for local testing

**Linux Target** (future production optimization):
- Static linking with musl libc
- Expected cold start: 18-33ms (70-85ms faster than macOS)
- Zero dynamic dependencies
- Requires fixing JIT cross-compilation issues

**Embedded Bytecode Optimization** (recommended for all platforms):
```bash
zig build -Doptimize=ReleaseFast -Dhandler=path/to/handler.js
```

Benefits:
- Eliminates runtime parsing and compilation
- Smaller container images (single binary)
- Reduced memory footprint
- Zero trade-offs

**Platform Strategy**:
- **macOS**: Development only (~100ms is acceptable)
- **Linux**: Production target (sub-10ms goal via static linking)
- **Pre-fork/daemon**: Alternative for sub-millisecond response times

See [docs/cold-start-optimization.md](docs/cold-start-optimization.md) for detailed profiling analysis and static linking investigation.

## Documentation

- [User Guide](docs/user-guide.md) - Complete handler API reference, routing patterns, examples
- [Architecture](docs/architecture.md) - System design, runtime model, project structure
- [JSX Guide](docs/jsx-guide.md) - JSX/TSX usage and server-side rendering
- [Performance](docs/performance.md) - Benchmarks, optimizations, deployment patterns
- [API Reference](docs/api-reference.md) - Advanced configuration, extending with native functions
- [TypeScript comptime Spec](docs/typescript-comptime-spec.md) - Compile-time evaluation reference

## JavaScript Subset

zts implements ES5 with select ES6+ extensions:

**Supported**: Strict mode, let/const, for...of (arrays), typed arrays, exponentiation operator, Math extensions, modern string methods (replaceAll, trimStart/End), globalThis, `range(end)` / `range(start, end)` / `range(start, end, step)`.

**Not Supported**: Arrow functions, template literals, destructuring, spread operator, classes, async/await, Promises.

See [User Guide](docs/user-guide.md#javascript-subset-reference) for full details.

## License

MIT licensed.

## Credits

- **zts** - Pure Zig JavaScript engine (part of this project)
- [Zig](https://ziglang.org/) programming language
- Codex & Claude
