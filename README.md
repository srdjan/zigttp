<p align="center">
  <img src="docs/zigttp-logo.jpg" alt="zigttp" width="600">
  <p align="center"><a href="https://zigttp.timok.deno.net/">🌐 Web Site</a></p>
</p>

A JavaScript runtime built from scratch in Zig for serverless workloads. One binary, no dependencies, instant cold starts.

Where Node.js and Deno optimize for generality, zigttp optimizes for a single use case: running a request handler as fast as possible, then getting out of the way. It ships a pure-Zig JS engine (zts) with a JIT compiler, NaN-boxed values, and hidden classes - but skips everything a FaaS handler doesn't need (event loop, Promises, `require`).

[![status: experimental](https://github.com/GIScience/badges/raw/master/status/experimental.svg)](https://github.com/GIScience/badges#experimental)
> Not Ready (yet)! Still under active development.

### What makes it different

**Opinionated language subset.** TypeScript with the footguns removed. No classes, no `this`, no `var`, no `while` loops - just functions, `let`/`const`, arrow functions, destructuring, `for...of`, and `match` expressions. Unsupported features fail at parse time with a suggested alternative, not at runtime with a cryptic stack trace.

**JSX as a first-class primitive.** The parser handles JSX directly - no Babel, no build step. Write TSX handlers that return server-rendered HTML.

**Compile-time verification.** `-Dverify` proves your handler correct at build time: every code path returns a Response, Result values are checked before access, no unreachable code. This works because zigttp's JS subset has no back-edges and no exceptions - the IR tree IS the control flow graph. `break` and `continue` within `for-of` are forward jumps only and don't compromise this property. See [verification docs](docs/verification.md).

**Boolean enforcement.** Truthy/falsy coercion is rejected in conditions and logical operators - always, not behind a flag. The BoolChecker performs progressive type inference for virtual-module return types, `match` expressions, optional returns like `env()`/`cacheGet()`, and Result-shaped values like `jwtVerify(...).ok`. Values the static checker cannot prove are caught by runtime VM assertions. See [boolean enforcement docs](docs/sound-mode.md).

**Compile-time evaluation.** `comptime()` folds expressions at load time, modeled after Zig's comptime. Hash a version string, uppercase a constant, precompute a config value - all before the handler runs.

**Automatic runtime sandboxing.** Every precompiled handler is sandboxed by default. The compiler extracts a contract of what the handler does (env vars, outbound hosts, cache namespaces) and derives a least-privilege policy that restricts runtime access to exactly the proven values. No configuration required. `-Dcontract` additionally emits a `contract.json` manifest. `-Dpolicy=policy.json` overrides auto-derived sandboxing with an explicit policy. Non-literal arguments honestly report `"dynamic": true`.

**Full TypeScript type checking.** Beyond stripping type annotations, the compiler checks them. Variable types, function argument types, return types, property access on records, and virtual module function signatures are all validated at build time. Interface declarations with all-function members are treated as nominal types to prevent structural forgery.

**Structured concurrent I/O.** `parallel()` and `race()` from `zigttp:io` overlap outbound HTTP without async/await or Promises. Handler code stays synchronous and linear; concurrency happens in the I/O layer using OS threads. Three API calls at 50ms each complete in ~50ms total.

**Proven deployment manifests.** `-Ddeploy=aws` generates platform-specific deployment configurations (AWS SAM templates) directly from compiler-proven contracts. Env vars become parameters, routes become API events, egress hosts become tags. Proof levels (complete/partial/none) signal what was statically verified vs what needs manual review.

**Native modules over JS polyfills.** Common FaaS needs (JWT auth, JSON Schema validation, caching, crypto) are implemented in Zig and exposed as `zigttp:*` virtual modules with zero interpretation overhead.

### Numbers

3ms runtime init. 1.2MB binary. 4MB memory baseline. Pre-warmed handler pool with per-request isolation. See [performance docs](docs/performance.md) for cold start breakdowns and deployment patterns.

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
| `zigttp:io` | `parallel`, `race` | Structured concurrent I/O (overlaps fetchSync calls using OS threads) |

### Auth Example

```typescript
import { parseBearer, jwtVerify, jwtSign } from "zigttp:auth";

function handler(req: Request): Response {
    const token = parseBearer(req.headers["authorization"]);
    if (token === undefined) return Response.json({ error: "unauthorized" }, { status: 401 });

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
    if (cached !== undefined) return Response.json(JSON.parse(cached));

    const data = { message: "computed", path: req.url };
    cacheSet("api", req.url, JSON.stringify(data), 60); // TTL: 60 seconds

    return Response.json(data);
}
```

### Concurrent I/O Example

```typescript
import { parallel, race } from "zigttp:io";

function handler(req: Request): Response {
    // Three API calls in ~50ms instead of ~150ms
    const [user, orders, inventory] = parallel([
        () => fetchSync("https://users.internal/api/v1/123"),
        () => fetchSync("https://orders.internal/api/v1?user=123"),
        () => fetchSync("https://inventory.internal/api/v1/789")
    ]);

    return Response.json({
        user: user.json(),
        orders: orders.json(),
        inventory: inventory.json()
    });
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
  -m, --memory <SIZE>   JS runtime memory limit (default: 0 = no limit)
  -n, --pool <N>        Runtime pool size (default: auto)
  --cors                Enable CORS headers
  --static <DIR>        Serve static files
  --outbound-http       Enable native outbound bridge (fetchSync/httpRequest)
  --outbound-host <H>   Restrict outbound bridge to exact host H
  --outbound-timeout-ms Connect timeout for outbound bridge (ms)
  --outbound-max-response <SIZE>
```

## Key Features

**Performance**: NaN-boxing, index-based hidden classes with SoA layout and O(1) transition lookups, polymorphic inline cache (PIC), generational GC, hybrid arena allocation for request-scoped workloads.

**HTTP/FaaS Optimizations**: Shape preallocation for Request/Response objects, pre-interned HTTP atoms, HTTP string caching, LockFreePool handler isolation, zero-copy response mode.

**Compile-Time Analysis**: Handler verification (`-Dverify`) proves correctness at build time. Contract extraction and auto-sandboxing restrict runtime capabilities to proven values. Boolean enforcement rejects truthy/falsy coercion. Full TypeScript type checking validates annotations against virtual module signatures.

**Structured Concurrency**: `parallel()` and `race()` overlap outbound HTTP using OS threads. No async/await, no event loop - handler code stays synchronous and linear.

**Deployment Pipeline**: Contract manifests (`-Dcontract`), proven deployment manifests (`-Ddeploy=aws`), and auto-derived runtime sandboxing form a pipeline from source analysis to platform-specific deployment configuration.

**Language Support**: ES5 + select ES6 features (for...of, typed arrays, exponentiation), native TypeScript/TSX stripping with type checking, compile-time evaluation with `comptime()`, direct JSX parsing.

**JIT Compilation**: Baseline JIT for x86-64 and ARM64, inline cache integration, object literal shapes, type feedback, adaptive compilation.

**Virtual Modules**: Native `zigttp:auth` (JWT/HS256, webhook signatures), `zigttp:validate` (JSON Schema), `zigttp:cache` (TTL/LRU key-value store), `zigttp:io` (structured concurrent I/O), plus `zigttp:env`, `zigttp:crypto`, `zigttp:router`.

**Developer Experience**: Fetch-like HTTP surface (`Response.*`, `Response(body, init?)`, `Request(url, init?)`, `Headers(init?)`, `request.text()`, `request.json()`, `headers.get()`, `fetchSync()`), console methods (log, error, warn, info, debug), static file serving with LRU cache, CORS support, pool metrics.

## Native Outbound Bridge

When enabled with `--outbound-http`, handlers can call the higher-level `fetchSync()` helper:

```javascript
const resp = fetchSync("http://127.0.0.1:8787/v1/ops?view=state", {
  method: "GET",
  headers: { Authorization: "Bearer ..." }
});

const data = resp.json();
```

`fetchSync()` returns a response-shaped object with `status`, `ok`, `headers.get(name)`, `text()`, and `json()`.

Current helper semantics:
- `headers.get(name)` is case-insensitive and returns the last observed value for that header name, or `null`.
- `Headers(init?)`, `Request(url, init?)`, and `Response(body, init?)` are available as factory-style HTTP types. `new` is not supported by the parser, so call them as plain functions.
- `Headers` instances support `get(name)`, `set(name, value)`, `append(name, value)`, `has(name)`, and `delete(name)`.
- `text()` returns the raw body string, or `""` when no body is present.
- `json()` returns parsed JSON, or `undefined` when the body is empty or invalid JSON.
- Body readers are single-use. Once `text()` or `json()` is called on a request/response object, subsequent body reads throw. Use `request.body` if you need the raw body string without consuming it.
- Validation, allowlist, network, timeout, and size-limit failures do not throw into handler code; `fetchSync()` returns a `599` response with a JSON body containing `error` and `details`.

The lower-level `httpRequest(jsonString)` bridge remains available:

```javascript
const raw = httpRequest(JSON.stringify({
  url: "http://127.0.0.1:8787/v1/ops?view=state",
  method: "GET",
  headers: { Authorization: "Bearer ..." }
}));
```

`httpRequest` returns JSON with either `{ ok: true, status, reason, body, content_type? }` or `{ ok: false, error, details }`.
Use `--outbound-host` to restrict egress to a single host.

## Compile-Time Toolchain

zigttp's compile-time toolchain goes beyond precompilation. It verifies correctness, checks types, extracts a capability contract, auto-derives a runtime sandbox, and can generate platform-specific deployment configs - all at build time.

```bash
# Development build (runtime handler loading)
zig build -Doptimize=ReleaseFast

# Production build (embedded bytecode, auto-sandboxed, 16% faster cold starts)
zig build -Doptimize=ReleaseFast -Dhandler=examples/handler.ts

# Verify handler correctness at compile time
zig build -Dhandler=handler.ts -Dverify

# Emit contract manifest (what the handler is allowed to do)
zig build -Dhandler=handler.ts -Dcontract

# Override auto-derived sandbox with an explicit capability policy
zig build -Dhandler=handler.ts -Dpolicy=policy.json

# Generate proven AWS SAM deployment manifest
zig build -Dhandler=handler.ts -Ddeploy=aws

# Combine all passes
zig build -Doptimize=ReleaseFast -Dhandler=handler.ts -Dverify -Dcontract -Ddeploy=aws
```

### Handler Verification (`-Dverify`)

The verifier statically proves three properties of your handler at compile time:

1. **Every code path returns a Response.** Missing `else` branches, `switch` cases without `default`, and paths that fall through without returning are all caught.
2. **Result values are checked before access.** Calls like `jwtVerify` and `validateJson` return Result objects. The verifier ensures `.ok` is checked before `.value` is accessed.
3. **No unreachable code.** Statements after an unconditional return produce a warning.

This works because zigttp's JS subset eliminates most non-trivial control flow - no `while`, no `try/catch`, no exceptions. `break` and `continue` are allowed within `for-of` (forward jumps only). The IR tree is the control flow graph. Verification is a recursive tree walk, not a fixpoint dataflow analysis.

```
$ zig build -Dhandler=handler.ts -Dverify

verify error: not all code paths return a Response
  --> handler.ts:2:17
   |
  2 | function handler(req) {
   |                 ^
   = help: ensure every branch (if/else, switch/default) ends with a return statement
```

See [docs/verification.md](docs/verification.md) for the full specification.

### Contract Manifest and Auto-Sandboxing

Every precompilation automatically extracts a contract from the handler's IR. The contract describes what your handler does before it runs and is used to derive the runtime sandbox. Add `-Dcontract` to also emit it as `contract.json`. It extracts:

- **Virtual modules** imported and which functions are used
- **Environment variables** accessed via `env("NAME")` - literal names are enumerated, dynamic access is flagged
- **Outbound hosts** called via `fetchSync("https://...")` - hosts are extracted from URL literals
- **Cache namespaces** used by `cacheGet`/`cacheSet`/etc.
- **Verification results** (when combined with `-Dverify`)
- **Route patterns** (when combined with `-Daot`)

```json
{
  "version": 1,
  "modules": ["zigttp:auth", "zigttp:cache"],
  "functions": {
    "zigttp:auth": ["jwtVerify", "parseBearer"],
    "zigttp:cache": ["cacheGet", "cacheSet"]
  },
  "env": { "literal": ["JWT_SECRET"], "dynamic": false },
  "egress": { "hosts": ["api.example.com"], "dynamic": false },
  "cache": { "namespaces": ["sessions"], "dynamic": false }
}
```

The `"dynamic": false` fields are the key signal. They mean "we can enumerate every value statically." When a handler uses a variable instead of a string literal (`env(someVar)` instead of `env("JWT_SECRET")`), the contract honestly reports `"dynamic": true`.

**Auto-sandboxing**: The contract is used to derive a `RuntimePolicy` embedded in the binary. Sections with `dynamic: false` are restricted to exactly the proven literals. Sections with `dynamic: true` remain unrestricted. The build reports what was proven:

```
Sandbox: complete (all access statically proven)
  env: restricted to [JWT_SECRET] (1 proven, no dynamic access)
  egress: restricted to [api.example.com] (1 proven, no dynamic access)
  cache: restricted to [sessions] (1 proven, no dynamic access)
```

### Explicit Policy Override (`-Dpolicy`)

To override auto-derived sandboxing with a stricter or different policy, pass an explicit policy file. The policy is validated against the contract at build time and enforced at runtime. Local file-import handlers are covered: capability usage is aggregated across the module graph before validation.

```json
{
  "env": { "allow": ["JWT_SECRET"] },
  "egress": { "allow_hosts": ["api.example.com"] },
  "cache": { "allow_namespaces": ["sessions"] }
}
```

Omit a section to leave that capability unrestricted. If a section is present, dynamic access in that category is rejected because zigttp cannot fully enumerate it.

### Proven Deployment Manifests (`-Ddeploy`)

Generate platform-specific deployment configurations from compiler-proven contracts:

```bash
zig build -Dhandler=handler.ts -Ddeploy=aws
```

Currently supported targets:
- **aws**: Generates AWS SAM `template.json` with proven env vars as parameters, routes as HttpApi events, egress hosts as tags, and proof level metadata.

Proof levels: `complete` (all checks pass, no dynamic flags), `partial` (some verification but dynamic access detected), `none` (no verification ran). A deploy report shows PROVEN vs NEEDS MANUAL REVIEW sections.

### Precompiled Bytecode

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

See [Performance](docs/performance.md) for detailed profiling analysis and deployment patterns.

## Documentation

- [User Guide](docs/user-guide.md) - Complete handler API reference, routing patterns, examples
- [Verification](docs/verification.md) - Compile-time handler verification: checks, diagnostics, examples
- [Boolean Enforcement](docs/sound-mode.md) - Strict boolean enforcement: type inference, narrowing, diagnostics
- [Architecture](docs/architecture.md) - System design, runtime model, concurrency, project structure
- [JSX Guide](docs/jsx-guide.md) - JSX/TSX usage and server-side rendering
- [TypeScript](docs/typescript.md) - Type stripping, compile-time evaluation
- [Performance](docs/performance.md) - Benchmarks, cold starts, optimizations, concurrent I/O
- [Feature Detection](docs/feature-detection.md) - Unsupported feature detection matrix
- [API Reference](docs/api-reference.md) - Zig embedding API, extending with native functions

## JavaScript Subset

zts implements ES5 with select ES6+ extensions:

**Supported**: Strict mode, let/const, arrow functions, template literals, destructuring, spread operator, for...of (arrays) with `break`/`continue`, optional chaining, nullish coalescing, typed arrays, exponentiation operator, compound assignments (`+=`, `-=`, `*=`, `/=`, `%=`, `**=`, bitwise), pipe operator (`|>`), array higher-order methods (`.map()`, `.filter()`, `.reduce()`, `.find()`, `.findIndex()`, `.some()`, `.every()`, `.forEach()`), `Object.keys()` / `.values()` / `.entries()`, Math extensions, modern string methods (replaceAll, trimStart/End), globalThis, `range()`, `match` expression (pattern matching).

**Not Supported**: Classes, async/await, Promises, `var`, `while`/`do-while` loops, `this`, `new`, `try/catch`, `null`, regular expressions, labeled `break`/`continue`, `as`/`satisfies` type assertions. All unsupported features are detected at parse time with helpful error messages suggesting alternatives. Use `undefined` as the sole absent-value sentinel.

See [User Guide](docs/user-guide.md#javascript-subset-reference) for full details.

## License

MIT licensed.

## Credits

- **zts** - Pure Zig JavaScript engine (part of this project)
- [Zig](https://ziglang.org/) programming language
- Codex & Claude
