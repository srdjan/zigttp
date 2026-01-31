# zigttp - Serverless JavaScript Runtime

> **Note**: THIS PROJECT IS EXPERIMENTAL AND UNDER ACTIVE DEVELOPMENT.

A high-performance serverless JavaScript runtime for FaaS deployments, powered by **zts** - a pure Zig JavaScript engine. Designed for AWS Lambda, Azure Functions, Cloudflare Workers, and edge computing.

## Why zigttp?

- Sub-millisecond cold starts with zero JIT warmup
- Single static binary, zero runtime dependencies
- Small deployment package (~500KB)
- Request isolation via pre-warmed handler pool
- Native TypeScript/TSX support with compile-time evaluation
- Direct JSX parsing for server-side rendering

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

```javascript
function handler(request) {
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

    return Response.text("Not Found", { status: 404 });
}
```

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
```

## Key Features

**Performance**: NaN-boxing, hidden classes with inline caching, polymorphic inline cache (PIC), generational GC, hybrid arena allocation for request-scoped workloads.

**HTTP/FaaS Optimizations**: Shape preallocation for Request/Response objects, pre-interned HTTP atoms, HTTP string caching, LockFreePool handler isolation, zero-copy response mode.

**Language Support**: ES5 + select ES6 features (for...of, typed arrays, exponentiation), native TypeScript/TSX stripping, compile-time evaluation with `comptime()`, direct JSX parsing.

**JIT Compilation**: Baseline JIT for x86-64 and ARM64, inline cache integration, object literal shapes, type feedback, adaptive compilation.

**Developer Experience**: Fetch-like Response API (Response.json, Response.text, Response.html), static file serving with LRU cache, CORS support, pool metrics.

## Build-Time Precompilation

For production deployments, precompile handlers at build time to eliminate all runtime parsing:

```bash
zig build -Doptimize=ReleaseFast -Dhandler=examples/handler.ts
```

This embeds bytecode directly in the binary for the fastest possible cold starts.

## Documentation

- [User Guide](docs/user-guide.md) - Complete handler API reference, routing patterns, examples
- [Architecture](docs/architecture.md) - System design, runtime model, project structure
- [JSX Guide](docs/jsx-guide.md) - JSX/TSX usage and server-side rendering
- [Performance](docs/performance.md) - Benchmarks, optimizations, deployment patterns
- [API Reference](docs/api-reference.md) - Advanced configuration, extending with native functions
- [TypeScript comptime Spec](docs/typescript-comptime-spec.md) - Compile-time evaluation reference

## JavaScript Subset

zts implements ES5 with select ES6+ extensions:

**Supported**: Strict mode, let/const, for...of (arrays), typed arrays, exponentiation operator, Math extensions, modern string methods (replaceAll, trimStart/End), globalThis.

**Not Supported**: Arrow functions, template literals, destructuring, spread operator, classes, async/await, Promises.

See [User Guide](docs/user-guide.md#javascript-subset-reference) for full details.

## Benchmarks

zts outperforms QuickJS in our benchmark suite:

| Benchmark      | zts         | QuickJS    | Improvement |
| -------------- | ----------- | ---------- | ----------- |
| stringOps      | 16.3M ops/s | 258K ops/s | 63x faster  |
| objectCreate   | 8.1M ops/s  | 1.7M ops/s | 4.8x faster |
| propertyAccess | 13.2M ops/s | 3.4M ops/s | 3.9x faster |
| httpHandler    | 1.0M ops/s  | 332K ops/s | 3.1x faster |

See [Performance](docs/performance.md) for detailed analysis.

## License

MIT licensed.

## Credits

- **zts** - Pure Zig JavaScript engine (part of this project)
- [Zig](https://ziglang.org/) programming language
