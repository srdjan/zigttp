<p align="center">
  <img src="docs/zigttp-logo.jpg" alt="zigttp" width="600">
  <p align="center"><a href="https://zigttp.timok.deno.net/">🌐 Web Site</a></p>
</p>

A JavaScript runtime built from scratch in Zig for serverless workloads. One binary, no dependencies, instant cold starts.

Where Node.js and Deno optimize for generality, zigttp optimizes for a single use case: running a request handler as fast as possible, then getting out of the way. It ships a pure-Zig JS engine (zigts) with a JIT compiler, NaN-boxed values, and hidden classes - but skips everything a FaaS handler doesn't need (event loop, Promises, `require`).

Validated release target: Zig `0.16.0-dev.3073+28ae5d415`. The compiler/analyzer CLI is `zigts`.

### What makes it different

**Opinionated language subset.** TypeScript with the footguns removed. No classes, no `this`, no `var`, no `while` loops, no `switch` - just functions, `let`/`const`, arrow functions, destructuring, `for...of`, `match` expressions, and `assert` statements. Unsupported features fail at parse time with a suggested alternative, not at runtime with a cryptic stack trace. Wait for it, there is a payoff for this :) 

**JSX as a first-class primitive.** The parser handles JSX directly - no Babel, no build step. Write TSX handlers that return server-rendered HTML.

**Compile-time verification.** `-Dverify` proves your handler correct at build time: every code path returns a Response, Result values are checked before access, no unreachable code. This works because zigttp's JS subset has no back-edges and no exceptions - the IR tree IS the control flow graph. `break` and `continue` within `for-of` are forward jumps only and don't compromise this property. See [verification docs](docs/verification.md).

**Sound mode.** The compiler uses type inference to catch bugs across all operators at compile time, not just in boolean contexts. Arithmetic on non-numeric types is rejected (`"hello" - 1`, `true * 5`, `env("X") / 2`). Mixed-type `+` is an error - use template literals. Tautological comparisons (`typeof x === "number"` when x is provably number) emit warnings. In boolean contexts, types with unambiguous falsy states are accepted (`if (count)`, `if (name)`), while objects and functions are rejected (always truthy). Values the static checker cannot prove are caught by runtime VM assertions. When types are statically proven, the compiler emits specialized opcodes (`add_num`, `lt_num`, etc.) that skip runtime type dispatch. See [sound mode docs](docs/sound-mode.md).

**Compile-time evaluation.** `comptime()` folds expressions at load time, modeled after Zig's comptime. Hash a version string, uppercase a constant, precompute a config value - all before the handler runs.

**Automatic runtime sandboxing.** Every precompiled handler is sandboxed by default. The compiler extracts a contract of what the handler does (env vars, outbound hosts, cache namespaces, SQL query names) and derives a least-privilege policy that restricts runtime access to exactly the proven values. No configuration required. `-Dcontract` additionally emits a `contract.json` manifest. `-Dpolicy=policy.json` overrides auto-derived sandboxing with an explicit policy. Non-literal arguments honestly report `"dynamic": true`. Self-extracting binaries parse the embedded contract at startup: proven env vars are validated (missing vars fail fast instead of causing a 500 on first request), proven routes reject non-matching requests at the HTTP layer before entering JS, and proven handler properties are logged for operator visibility.

**Handler effect classification.** Every virtual module function carries a compile-time effect annotation (read, write, or none). The compiler aggregates these to prove handler-level properties: pure, read-only, stateless, retry-safe, deterministic, idempotent (safe for at-least-once delivery), injection-safe (no unvalidated input in sinks), and state-isolated (no cross-request data leakage). The I/O depth bound (max virtual module calls per request) enables compile-time Lambda timeout derivation. These properties flow into deployment manifests, OWASP compliance mapping, and the build report - no annotations or configuration needed. Handlers proven `deterministic` and `read_only` also have their GET/HEAD responses cached at runtime and served from Zig memory without entering JS. The `X-Zigttp-Proof-Cache: hit` response header confirms a cache hit.

**Full TypeScript type checking.** The compiler checks type annotations, not just strips them. Variable types, function argument types, return types, property access on records, and virtual module function signatures are validated at build time. Generic type aliases (`type Result<T> = { ok: boolean; value: T }`) are instantiated when used in annotations. Object literals are structurally matched against declared interface and type alias return types. Optional types from virtual modules (`env()`, `cacheGet()`, `parseBearer()`) are narrowed in if-guards: `if (x)`, `if (!x) return`, and `if (x !== undefined)` all narrow nullable bindings to their inner type. Discriminated unions narrow in if-conditions: `if (r.kind === "err") { return; }` narrows `r` to the remaining member afterward. Type guard functions (`x is T`) narrow in both if-branches and after `assert` statements. `distinct type UserId = string` creates nominal types that prevent cross-type assignment while unwrapping for operations. `readonly` fields reject assignment at compile time. Template literal types (`` `/api/${string}` ``) validate string patterns at build time. `const` bindings preserve literal types; `: Type` annotations validate assignability without widening. `let` bindings widen literals so reassignment works. Interface declarations with all-function members are nominal to prevent structural forgery.

**Structured concurrent I/O.** `parallel()` and `race()` from `zigttp:io` overlap outbound HTTP without async/await or Promises. Handler code stays synchronous and linear; concurrency happens in the I/O layer using OS threads. Three API calls at 50ms each complete in ~50ms total.

**Proven deployment manifests.** `-Ddeploy=aws` generates platform-specific deployment configurations (AWS SAM templates) directly from compiler-proven contracts. Env vars become parameters, routes become API events, egress hosts become tags and conditional VPC SecurityGroup resources. The deploy report includes OWASP Top 10 compliance mapping, cost estimation from proven I/O depth, and handler properties. Proof levels (complete/partial/none) signal what was statically verified vs what needs manual review.

**Deterministic replay.** Record every I/O boundary during handler execution with `--trace`, then replay against a new handler version with `--replay` or `-Dreplay` at build time. Because virtual modules are the only I/O boundary, recording their inputs and outputs captures all external state - handlers become deterministic pure functions of (Request, VirtualModuleResponses).

**Durable execution.** `--durable <dir>` enables crash recovery and long-running workflows via write-ahead oplog. Wrap work in `run(key, fn)` from `zigttp:durable` with an idempotency key; each I/O call is persisted before returning to the handler. On crash recovery, recorded results are replayed without touching the network. Completed runs are deduplicated by key. Durable runs can suspend with `sleep(ms)`, `sleepUntil(unixMs)`, or `waitSignal(name)` and resume when the timer fires or a signal arrives via `signal(key, name, payload)`. A background scheduler polls for ready timers and signals using the same replay-safe recovery path.

**Guard composition.** `guard()` from `zigttp:compose` combined with the pipe operator (`|>`) composes handlers with pre/post guards at compile time. The parser desugars `guard(auth) |> guard(log) |> handler |> guard(cors)` into a single flat function with sequential if-checks - zero runtime overhead.

**Behavioral contract.** `-Dcontract` enumerates every execution path through the handler and embeds them in contract.json as structured `behaviors`. Each path records the route, branching conditions (which I/O calls succeed or fail), the I/O sequence, and the resulting HTTP status. The restricted JS subset has no back-edges and no exceptions, so path enumeration is finite and exhaustive. Comparing two behavioral contracts shows which paths were preserved, which changed response codes, which were removed, and which are new.

**Proven evolution.** `-Dprove=contract.json:traces.jsonl` compares two handler versions by diffing their contracts (surface and behavior) and replaying recorded traces. The upgrade verifier produces a four-value verdict: `safe`, `safe_with_additions`, `breaking`, or `needs_review`. It factors in behavioral path changes, property regressions with severity (critical/warning/info), and trace coverage gaps. Output: `proof.json`, `proof-report.txt`, and `upgrade-manifest.json`. The standalone `zigts prove old.json new.json` CLI compares contracts without rebuilding (exit 0 for safe, 1 for breaking, 2 for needs_review).

**Contract-driven mock server.** `zigts mock tests.jsonl --port 3001` serves mock HTTP responses from PathGenerator test cases. Frontend teams get a mock API provably consistent with the handler contract.

**Native modules over JS polyfills.** Common FaaS needs (JWT auth, JSON Schema validation, caching, crypto) are implemented in Zig and exposed as `zigttp:*` virtual modules with zero interpretation overhead.

### Numbers

3ms runtime init. 1.2MB binary. 4MB memory baseline. Pre-warmed handler pool with per-request isolation. See [performance docs](docs/performance.md) for cold start breakdowns and deployment patterns.

## Install

Pre-built binaries for macOS and Linux (x86_64, aarch64):

```bash
curl -fsSL https://raw.githubusercontent.com/srdjan/zigttp/main/install.sh | sh
```

Or download a tarball from [GitHub Releases](https://github.com/srdjan/zigttp/releases).

To build from source (requires Zig `0.16.0-dev.3073+28ae5d415`):

```bash
git clone https://github.com/srdjan/zigttp.git && cd zigttp
zig build -Doptimize=ReleaseFast
```

## Quick Start

```bash

# Run with inline handler
./zig-out/bin/zigttp serve -e "function handler(r) { return Response.json({hello:'world'}) }"

# Or with a handler file
./zig-out/bin/zigttp serve examples/handler/handler.ts
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
                <p>Welcome to zigttp!</p>
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
| `zigttp:validate` | `schemaCompile`, `validateJson`, `validateObject`, `coerceJson`, `schemaDrop` | JSON Schema validation with format validators (email, uuid, iso-date, iso-datetime) |
| `zigttp:decode` | `decodeJson`, `decodeForm`, `decodeQuery` | Schema-backed typed ingress for JSON, URL-encoded forms, and query strings |
| `zigttp:cache` | `cacheGet`, `cacheSet`, `cacheDelete`, `cacheIncr`, `cacheStats` | In-memory key-value cache with TTL and LRU |
| `zigttp:sql` | `sql`, `sqlOne`, `sqlMany`, `sqlExec` | Registered SQLite queries with build-time schema validation |
| `zigttp:service` | `serviceCall` | Named internal service-to-service calls backed by `system.json` |
| `zigttp:io` | `parallel`, `race` | Structured concurrent I/O (overlaps fetchSync calls using OS threads) |
| `zigttp:compose` | `guard`, `pipe` | Compile-time handler composition via pipe operator |
| `zigttp:durable` | `run`, `step`, `stepWithTimeout`, `sleep`, `sleepUntil`, `waitSignal`, `signal`, `signalAt` | Durable execution with crash recovery, timers, signals, and timeout-aware steps |

Each export carries an effect annotation used for handler property classification. Read-effect functions: all of env, crypto, router, auth, validate, plus `cacheGet`/`cacheStats`, `sql`/`sqlOne`/`sqlMany`. Write-effect functions: `cacheSet`/`cacheDelete`/`cacheIncr`, `sqlExec`, `serviceCall`, `parallel`/`race`, and all durable functions. `guard` has no runtime effect.

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
import { schemaCompile } from "zigttp:validate";
import { decodeJson } from "zigttp:decode";

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
        const result = decodeJson("user", req.body ?? "{}");
        if (!result.ok) return Response.json({ errors: result.errors }, { status: 400 });
        return Response.json({ user: result.value }, { status: 201 });
    }
    return Response.json({ ok: true });
}
```

Use `decodeJson`, `decodeForm`, and `decodeQuery` as the default request-ingress helpers when the payload shape is schema-backed. `validateJson` and `coerceJson` remain available for lower-level validation flows.

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

See [examples/modules/modules_all.ts](examples/modules/modules_all.ts) for an integration example using all modules together.

### SQL Example

```typescript
import { sql, sqlMany, sqlExec } from "zigttp:sql";

sql("listTodos", "SELECT id, title, done FROM todos ORDER BY id ASC");
sql("createTodo", "INSERT INTO todos (title, done) VALUES (:title, 0)");

function handler(req: Request): Response {
    if (req.method === "GET") {
        return Response.json({ items: sqlMany("listTodos") });
    }

    const body = JSON.parse(req.body);
    return Response.json(sqlExec("createTodo", { title: body.title }), { status: 201 });
}
```

Build-time validation requires a schema snapshot:

```bash
zig build -Dhandler=examples/sql/sql-crud.ts -Dsql-schema=examples/sql/schema.sql
```

## CLI Options

```bash
zigttp serve [options] <handler.js>

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
  --trace <FILE>        Record handler I/O traces to JSONL file
  --replay <FILE>       Replay recorded traces and verify handler output
  --sqlite <FILE>       SQLite database path for zigttp:sql
  --durable <DIR>       Enable durable execution with write-ahead oplog
  --system <FILE>       System registry for zigttp:service
  --no-env-check        Skip startup env var validation (development use)
```

### zigts CLI (compiler and analyzer)

Standalone analysis and compilation without starting a server.

```bash
zigts check [handler.ts] [options]    # Verify handler, show proof card
zigts compile [--system path] <handler.ts> <out.zig>  # Compile to embedded bytecode
zigts prove <old.json> <new.json>     # Compare contracts (exit 0=safe, 1=breaking)
zigts mock <tests.jsonl> [--port N]   # Mock server from test cases
zigts link <system.json>              # Cross-handler contract linking
zigts features [--json]               # List allowed/blocked language features
zigts modules [--json]                # List virtual modules and exports
zigts init                            # Install skill files and hooks for Claude Code
zigts expert meta [--json]            # Policy metadata (version, hash, rule count)
zigts expert verify-paths <f>... [--json]  # Full analysis on files
```

#### Structured JSON output

`zigts check --json handler.ts` writes machine-readable diagnostics to stdout. Add `--system system.json` when the handler uses `serviceCall()` and you want compile-time typing for internal service responses and request-shape validation.

```json
{
  "success": false,
  "diagnostics": [{
    "code": "ZTS001",
    "severity": "error",
    "message": "'try/catch' is not supported",
    "file": "handler.ts",
    "line": 23,
    "column": 3,
    "suggestion": "use Result types for error handling"
  }]
}
```

On success, the output includes a proof summary: env vars, outbound hosts, virtual modules, and handler properties.

Code ranges: ZTS0xx (parser), ZTS1xx (sound mode), ZTS2xx (type checker), ZTS3xx (handler verifier).

## Key Features

**Performance**: Type-prefix NaN-boxing (single-instruction type checks), index-based hidden classes with SoA layout and O(1) transition lookups, polymorphic inline cache (PIC), generational GC, hybrid arena allocation for request-scoped workloads.

**HTTP/FaaS Optimizations**: Shape preallocation for Request/Response objects, pre-interned HTTP atoms, HTTP string caching, LockFreePool handler isolation, zero-copy response mode.

**Compile-Time Analysis**: Handler verification (`-Dverify`) proves correctness at build time. Contract extraction with behavioral paths and auto-sandboxing restrict runtime capabilities to proven values. Proven properties also control runtime behavior: deterministic+read_only handlers have their responses cached and served without JS execution. `zigttp:sql` queries are prepared against a build-time schema snapshot via `-Dsql-schema=...`. Sound mode rejects non-numeric arithmetic, mixed-type `+`, and tautological comparisons at compile time, and emits type-specialized opcodes when types are proven. TypeScript type checking validates annotations, narrows optionals through if-guards, and structurally matches object literals against declared types.

**Structured Concurrency**: `parallel()` and `race()` overlap outbound HTTP using OS threads. No async/await, no event loop - handler code stays synchronous and linear.

**Deployment Pipeline**: Contract manifests with behavioral paths (`-Dcontract`), proven deployment manifests (`-Ddeploy=aws`), auto-derived runtime sandboxing, deterministic replay (`--trace`/`--replay`/`-Dreplay`), proven evolution with upgrade verdicts (`-Dprove`), and durable execution (`--durable`) form a pipeline from source analysis to production deployment with crash recovery.

**Language Support**: ES5 + select ES6 features (for...of with break/continue, typed arrays, exponentiation, pipe operator, compound assignments), native TypeScript/TSX stripping with type checking, compile-time evaluation with `comptime()`, direct JSX parsing, `match` expression, `assert` statement, `distinct type`, `readonly` fields, template literal types, type guards (`x is T`).

**JIT Compilation**: Baseline JIT for x86-64 and ARM64, inline cache integration, object literal shapes, type feedback, adaptive compilation.

**Virtual Modules**: Native `zigttp:auth` (JWT/HS256, webhook signatures), `zigttp:validate` (JSON Schema registry), `zigttp:decode` (typed request ingress), `zigttp:cache` (TTL/LRU key-value store), `zigttp:service` (named internal service calls), `zigttp:io` (structured concurrent I/O), `zigttp:compose` (guard composition), `zigttp:durable` (crash recovery, timers, signals), plus `zigttp:env`, `zigttp:crypto`, `zigttp:router`.

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

## Internal Service Calls

For internal zigttp-to-zigttp calls, prefer `serviceCall()` from `zigttp:service` over hard-coded internal URLs:

```typescript
import { serviceCall } from "zigttp:service";

function handler(req: Request): Response {
  const user = serviceCall("users", "GET /api/users/:id", {
    params: { id: "123" },
  });

  if (user.status !== 200) {
    return Response.json({ error: "user service unavailable" }, { status: 502 });
  }

  return Response.json(user.json());
}
```

`serviceCall(serviceName, "METHOD /path", init?)` resolves through `system.json`, lowers to the existing outbound bridge, and gives `zigts link` a first-class internal edge to verify.

With `zigts check --system <FILE>` or `zigts compile --system <FILE>`, literal `serviceCall()` sites also become payload-aware at compile time:

- `status` narrows to the target route's proven status codes
- `.json()` returns the target route's proven JSON type when there is a single compatible response schema
- if different status codes produce different schemas, narrow on `resp.status` before calling `.json()`
- required path/query/header/body inputs are validated against the target route contract during type checking

`init` supports:

- `params` for `:path` placeholders
- `query` for query string keys
- `headers` for request headers
- `body` for string request bodies

Run handlers that use `zigttp:service` with `--system <FILE>`:

```bash
zigttp serve --system examples/system/system.json examples/system/gateway.ts
```

`system.json` now requires a stable `name` for each handler:

```json
{
  "version": 1,
  "handlers": [
    { "name": "gateway", "path": "examples/system/gateway.ts", "baseUrl": "https://gateway.internal" },
    { "name": "users", "path": "examples/system/users.ts", "baseUrl": "https://users.internal" }
  ]
}
```

`zigts link <system.json>` uses those names and routes to prove internal service composition. Raw `fetchSync("https://users.internal/...")` still works, but named service calls produce stronger linking and clearer diagnostics.

The linker now reports payload proof separately from route proof. `proofLevel` keeps its current meaning, while `system-contract.json` and `system-report.txt` add explicit payload fields:

- `payloadProven`
- `payloadCompatible`
- `payloadDetail`

## Compile-Time Toolchain

zigttp's compile-time toolchain goes beyond precompilation. It verifies correctness, checks types, extracts a capability contract, auto-derives a runtime sandbox, and can generate platform-specific deployment configs - all at build time.

```bash
# Development build (runtime handler loading)
zig build -Doptimize=ReleaseFast

# Production build (embedded bytecode, auto-sandboxed, 16% faster cold starts)
zig build -Doptimize=ReleaseFast -Dhandler=examples/handler/handler.ts

# Verify handler correctness at compile time
zig build -Dhandler=handler.ts -Dverify

# Emit contract manifest (what the handler is allowed to do)
zig build -Dhandler=handler.ts -Dcontract

# Validate zigttp:sql queries against a schema snapshot
zig build -Dhandler=examples/sql/sql-crud.ts -Dsql-schema=examples/sql/schema.sql

# Override auto-derived sandbox with an explicit capability policy
zig build -Dhandler=handler.ts -Dpolicy=policy.json

# Generate proven AWS SAM deployment manifest
zig build -Dhandler=handler.ts -Ddeploy=aws

# Replay-verify handler against recorded traces before embedding
zig build -Dhandler=handler.ts -Dreplay=traces.jsonl

# Compare handler versions (equivalent, additive, or breaking)
zig build -Dhandler=handler.ts -Dprove=old-contract.json:traces.jsonl

# Combine all passes
zig build -Doptimize=ReleaseFast -Dhandler=handler.ts -Dverify -Dcontract -Ddeploy=aws

# External enrichment flags (optional, for code generator integration)
zig build -Dhandler=handler.ts -Dmanifest=governance-manifest.json
zig build -Dhandler=handler.ts -Dexpect-properties=properties.json
zig build -Dhandler=handler.ts -Ddata-labels=data-labels.json
zig build -Dhandler=handler.ts -Dfault-severity=fault-severity.json
zig build -Dhandler=handler.ts -Dreport=json
```

### Handler Verification (`-Dverify`)

The verifier statically proves six properties of your handler at compile time:

1. **Every code path returns a Response.** Missing `else` branches and paths that fall through without returning are caught.
2. **Result values are checked before access.** Calls like `jwtVerify`, `decodeJson`, and `decodeQuery` return Result objects. The verifier ensures `.ok` is checked before `.value` is accessed.
3. **No unreachable code.** Statements after an unconditional return produce a warning.
4. **No unused variables.** Declared variables that are never referenced produce a warning. Suppress with an underscore prefix (`_unused`).
5. **Match expressions have default arms.** A `match` without a default arm produces a warning.
6. **Optional values are checked before use.** Values from `env()`, `cacheGet()`, `parseBearer()`, and `routerMatch()` must be narrowed via `if (val)`, `val !== undefined`, `val ?? default`, or reassignment before use in expressions.

This works because zigttp's JS subset eliminates most non-trivial control flow - no `while`, no `switch`, no `try/catch`, no exceptions. `break` and `continue` are allowed within `for-of` (forward jumps only). The IR tree is the control flow graph. Verification is a recursive tree walk, not a fixpoint dataflow analysis.

```
$ zig build -Dhandler=handler.ts -Dverify

verify error: not all code paths return a Response
  --> handler.ts:2:17
   |
  2 | function handler(req) {
   |                 ^
   = help: ensure every branch (if/else) ends with a return statement
```

See [docs/verification.md](docs/verification.md) for the full specification.

### Contract Manifest and Auto-Sandboxing

Every precompilation automatically extracts a contract from the handler's IR. The contract describes what your handler does before it runs and is used to derive the runtime sandbox. Add `-Dcontract` to also emit it as `contract.json`. It extracts:

- **Virtual modules** imported and which functions are used
- **Environment variables** accessed via `env("NAME")` - literal names are enumerated, dynamic access is flagged
- **Outbound hosts** called via `fetchSync("https://...")` - hosts are extracted from URL literals
- **Internal service calls** made via `serviceCall("name", "METHOD /path", init)` - service names, route signatures, and statically proven params/query/header/body keys are captured
- **System-linked payload facts** for named internal edges - target response statuses, JSON payload proof, and payload-proof gaps are reported in system-level output
- **Cache namespaces** used by `cacheGet`/`cacheSet`/etc.
- **SQL queries** registered with `sql("name", "...")` - names, statement kinds, and touched tables are captured after schema validation
- **API surface** from proven routes: method/path, path/query/header params, JSON request bodies, response variants, and bearer auth metadata
- **Handler properties** derived from effect classification of virtual module functions (pure, read_only, stateless, retry_safe, deterministic)
- **Behavioral paths** - every execution path through the handler with route, branching conditions, I/O sequence, and response status (exhaustive when the path count stays below 1024)
- **Verification results** (when combined with `-Dverify`)
- **Route patterns** (when combined with `-Daot`)

```json
{
  "version": 11,
  "modules": ["zigttp:auth", "zigttp:cache"],
  "functions": {
    "zigttp:auth": ["jwtVerify", "parseBearer"],
    "zigttp:cache": ["cacheGet", "cacheSet"]
  },
  "env": { "literal": ["JWT_SECRET"], "dynamic": false },
  "egress": { "hosts": ["api.example.com"], "dynamic": false },
  "cache": { "namespaces": ["sessions"], "dynamic": false },
  "sql": {
    "backend": "sqlite",
    "queries": [
      { "name": "listTodos", "operation": "select", "tables": ["todos"] }
    ],
    "dynamic": false
  },
  "properties": {
    "pure": false,
    "readOnly": false,
    "stateless": false,
    "retrySafe": false,
    "deterministic": true,
    "hasEgress": true
  },
  "behaviors": [
    {
      "method": "GET",
      "pattern": "/users/:id",
      "status": 200,
      "ioDepth": 2,
      "failurePath": false,
      "conditions": [
        {"kind": "io_ok", "module": "auth", "func": "jwtVerify"},
        {"kind": "io_ok", "module": "cache", "func": "cacheGet"}
      ],
      "ioSequence": [
        {"module": "auth", "func": "jwtVerify"},
        {"module": "cache", "func": "cacheGet"}
      ]
    },
    {
      "method": "GET",
      "pattern": "/users/:id",
      "status": 401,
      "ioDepth": 1,
      "failurePath": true,
      "conditions": [
        {"kind": "io_fail", "module": "auth", "func": "jwtVerify"}
      ],
      "ioSequence": [
        {"module": "auth", "func": "jwtVerify"}
      ]
    }
  ],
  "behaviorsExhaustive": true
}
```

The `"dynamic": false` fields are the key signal. They mean "we can enumerate every value statically." When a handler uses a variable instead of a string literal (`env(someVar)` instead of `env("JWT_SECRET")`), the contract honestly reports `"dynamic": true`.

### OpenAPI and TypeScript SDK

The same proven route facts can also be emitted as OpenAPI and as a generated TypeScript client:

```bash
zig build -Dhandler=examples/routing/api-surface.ts -Dcontract -Dopenapi -Dsdk=ts
```

This writes three sibling artifacts in `src/generated/`:

- `contract.json`
- `openapi.json`
- `client.ts`

The current API emitters include facts the compiler can prove without guessing:

- route method and path
- path, query, and header params reached through literal access
- proven JSON request bodies from `validateJson(...)`, `coerceJson(...)`, and `decodeJson(...)`
- proven form request bodies from `decodeForm(...)`
- typed query params from `decodeQuery(...)`
- proven response variants, including multiple status codes when statically visible
- bearer auth metadata
- `x-zigttp-*` hints whenever part of the surface stays dynamic

The generated SDK only exposes typed helpers for routes it can prove end to end. Everything else remains available through `requestRaw()` and is listed in `skippedOperations`.
A fully proven route can be consumed like this:

```ts
import { createClient } from "./src/generated/client";

const api = createClient({ baseUrl: "https://api.example.com" });

const result = await api.postProfilesId({
    params: { id: "user_123" },
    query: { verbose: true },
    body: { displayName: "Ada" },
    headers: { "x-client-id": "cli-42" },
});

console.log(result.data.displayName);
```

**Auto-sandboxing**: The contract is used to derive a `RuntimePolicy` embedded in the binary. Sections with `dynamic: false` are restricted to exactly the proven literals. Sections with `dynamic: true` remain unrestricted. The build reports what was proven:

```
Sandbox: complete (all access statically proven)
  env: restricted to [JWT_SECRET] (1 proven, no dynamic access)
  egress: restricted to [api.example.com] (1 proven, no dynamic access)
  cache: restricted to [sessions] (1 proven, no dynamic access)
  sql: restricted to [listTodos] (1 proven, no dynamic access)
Handler Properties:
  ---    pure            handler is a deterministic function of the request
  ---    read_only       no state mutations via virtual modules
  ---    stateless       independent of mutable state
  ---    retry_safe      safe for Lambda auto-retry on timeout
  PROVEN deterministic   no Date.now() or Math.random()
```

### Explicit Policy Override (`-Dpolicy`)

To override auto-derived sandboxing with a stricter or different policy, pass an explicit policy file. The policy is validated against the contract at build time and enforced at runtime. Local file-import handlers are covered: capability usage is aggregated across the module graph before validation.

```json
{
  "env": { "allow": ["JWT_SECRET"] },
  "egress": { "allow_hosts": ["api.example.com"] },
  "cache": { "allow_namespaces": ["sessions"] },
  "sql": { "allow_queries": ["listTodos"] }
}
```

Omit a section to leave that capability unrestricted. If a section is present, dynamic access in that category is rejected because zigttp cannot fully enumerate it.

### Proven Deployment Manifests (`-Ddeploy`)

Generate platform-specific deployment configurations from compiler-proven contracts:

```bash
zig build -Dhandler=handler.ts -Ddeploy=aws
```

Currently supported targets:
- **aws**: Generates AWS SAM `template.json` with proven env vars as parameters, routes as HttpApi events, egress hosts as tags, handler effect properties (zigttp:retrySafe, zigttp:readOnly), and proof level metadata.

Proof levels: `complete` (all checks pass, no dynamic flags), `partial` (some verification but dynamic access detected), `none` (no verification ran). A deploy report shows PROVEN vs NEEDS MANUAL REVIEW sections.

### Deterministic Replay (`--trace` / `--replay` / `-Dreplay`)

zigttp's restricted JS subset (no async, no exceptions, no side-effecting builtins) makes handlers deterministic pure functions of their request and virtual module responses. The replay system exploits this property.

**Record** traces during normal operation:

```bash
zigttp serve handler.ts --trace traces.jsonl
```

Every virtual module call, `fetchSync` response, `Date.now()` timestamp, and `Math.random()` value is recorded alongside the request and response.

**Replay** traces against a modified handler to detect regressions:

```bash
zigttp serve --replay traces.jsonl handler-v2.ts
```

Reports identical, status-changed, and body-changed results with structured diffs.

**Build-time replay** fails the build if regressions are detected:

```bash
zig build -Dhandler=handler-v2.ts -Dreplay=traces.jsonl
```

### Durable Execution (`--durable`)

Enable crash recovery with a write-ahead oplog:

```bash
zigttp serve handler.ts --durable ./oplogs
```

Handlers opt into durability via the `zigttp:durable` virtual module:

```typescript
import { run, step, stepWithTimeout, sleep, waitSignal, signal } from "zigttp:durable";

function handler(req: Request): Response {
    // Deliver a signal to a waiting run
    if (req.url === "/approve") {
        signal("order:42", "approved", { by: "admin" });
        return Response.json({ ok: true });
    }

    // Durable workflow with steps, timers, and signals
    return run("order:42", () => {
        const order = step("create", () =>
            fetchSync("https://api.internal/orders", { method: "POST", body: "{}" }));
        sleep(5000);
        const approval = waitSignal("approved");
        const confirmed = step("confirm", () =>
            fetchSync("https://api.internal/orders/42/confirm", {
                method: "POST", body: JSON.stringify(approval)
            }));
        return Response.json(confirmed.json());
    });
}
```

`run(key, fn)` wraps a unit of work with an idempotency key. Each `step(name, fn)` persists its result to the oplog before returning to the handler. `sleep(ms)` and `sleepUntil(unixMs)` suspend the run until a timer fires. `waitSignal(name)` suspends until a signal arrives via `signal(key, name, payload)` or `signalAt(key, name, unixMs, payload)`. Pending runs return `202 Accepted` with a JSON body describing the wait. On crash recovery, recorded results are replayed without touching the network. A background scheduler polls for ready timers and signals. Completed runs are deduplicated by key. `stepWithTimeout(name, timeoutMs, fn)` executes a step with a deadline - returns `{ ok: true, value }` on completion or `{ ok: false, error: "timeout" }` if the deadline is exceeded.

### Proven Evolution (`-Dprove`)

Compare two handler versions and classify the upgrade:

```bash
zig build -Dhandler=handler-v2.ts -Dprove=old-contract.json:traces.jsonl
```

Two diff levels run against the old and new contracts. The surface diff compares I/O capabilities (env vars, egress hosts, cache namespaces, SQL query names, routes). The behavioral diff compares every execution path, matching by route and branching conditions, then classifying each as preserved, response-changed, removed, or added.

Property regressions carry severity. Losing `retry_safe` or `injection_safe` is critical. Losing `deterministic` or `idempotent` is a warning. Losing `pure` is informational.

The upgrade verdict combines these signals:

- **safe**: Identical behavior, no property regressions
- **safe_with_additions**: New paths or capabilities added, existing behavior preserved
- **breaking**: Paths removed, responses changed, or critical property lost
- **needs_review**: Structurally OK but warning-level regressions or significant coverage gaps

Output: `proof.json` (machine-readable certificate), `proof-report.txt` (human-readable), and `upgrade-manifest.json` (verdict with full breakdown).

### Guard Composition (`zigttp:compose`)

Compose handlers with pre/post guards using the pipe operator:

```typescript
import { guard } from "zigttp:compose";

const withAuth = guard((req: Request): Response | undefined => {
    if (req.headers["authorization"] === undefined)
        return Response.json({ error: "unauthorized" }, { status: 401 });
    return undefined;
});

const withCors = guard((res: Response): Response | undefined => {
    return Response.json(res.body, {
        status: res.status,
        headers: { "access-control-allow-origin": "*" }
    });
});

const handler = withAuth |> mainHandler |> withCors;
```

The parser desugars the pipe chain into a single flat function with sequential if-checks at compile time. Pre-guards receive the request and short-circuit on non-undefined return. Post-guards receive the response and can replace it. Exactly one non-guard handler is required.

### External Enrichment Flags

Five optional build flags accept external JSON files for cross-referencing against compiler-proven contracts. These work with any code generator or hand-written files - no specific tooling required.

- **`-Dmanifest=<path>`** - Cross-references a declared manifest (routes, SQL tables, env vars) against the handler contract. Errors on declared items missing from code, warns on undeclared items found in code. Emits `manifest-alignment.json`.
- **`-Dexpect-properties=<path>`** - Verifies handler-derived properties (state_isolated, injection_safe, read_only, etc.) match external expectations. Build fails on mismatches.
- **`-Ddata-labels=<path>`** - Merges externally declared data sensitivity labels (secret, credential, etc.) with the flow checker's heuristic labels. Violations of declared labels become build errors.
- **`-Dfault-severity=<path>`** - Overrides fault severity classification at the route level. A route declared "critical" elevates all failable calls within it to critical severity for fault coverage diagnostics.
- **`-Dreport=json`** - Emits a structured JSON build report aggregating verification, properties, fault coverage, flow analysis, manifest alignment, and property expectations into `report.json`.

All flags are optional and additive. Without them, zigttp works identically to before.

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

## Nightly Revalidation

When updating to a new Zig nightly, run:

```bash
zig version
zig build
zig build test
zig build test-zigts
zig build test-zruntime
bash scripts/test-examples.sh
ZTS_RUN_STRESS_TESTS=1 zig build test-zruntime
ZTS_RUN_FLAKY_NIGHTLY_TESTS=1 zig build test -- --test-filter "parseRequest rejects long header"
```

Then update the pinned version in this README and `docs/user-guide.md`.

## Documentation

- [User Guide](docs/user-guide.md) - Complete handler API reference, routing patterns, examples
- [Verification](docs/verification.md) - Compile-time handler verification: checks, diagnostics, examples
- [Sound Mode](docs/sound-mode.md) - Type-directed analysis: arithmetic safety, `+` safety, tautology detection, truthiness, type-specialized codegen
- [Architecture](docs/architecture.md) - System design, runtime model, concurrency, project structure
- [JSX Guide](docs/jsx-guide.md) - JSX/TSX usage and server-side rendering
- [TypeScript](docs/typescript.md) - Type stripping, compile-time evaluation
- [Performance](docs/performance.md) - Benchmarks, cold starts, optimizations, concurrent I/O
- [Feature Detection](docs/feature-detection.md) - Unsupported feature detection matrix
- [API Reference](docs/api-reference.md) - Zig embedding API, extending with native functions

## JavaScript Subset

zigts implements ES5 with select ES6+ extensions:

**Supported**: Strict mode, let/const, arrow functions, template literals, destructuring, spread operator, for...of (arrays) with `break`/`continue`, optional chaining, nullish coalescing, typed arrays, exponentiation operator, compound assignments (`+=`, `-=`, `*=`, `/=`, `%=`, `**=`, bitwise), pipe operator (`|>`), array higher-order methods (`.map()`, `.filter()`, `.reduce()`, `.find()`, `.findIndex()`, `.some()`, `.every()`, `.forEach()`), `Object.keys()` / `.values()` / `.entries()`, Math extensions, modern string methods (replaceAll, trimStart/End), globalThis, `range()`, `match` expression (pattern matching).

**Not Supported**: Classes, async/await, Promises, `var`, `while`/`do-while` loops, `this`, `new`, `try/catch`, `null`, regular expressions, labeled `break`/`continue`, `as`/`satisfies` type assertions. All unsupported features are detected at parse time with helpful error messages suggesting alternatives. Use `undefined` as the sole absent-value sentinel.

See [User Guide](docs/user-guide.md#javascript-subset-reference) for full details.

## License

MIT licensed.

## Credits

- **zigts** - Pure Zig JavaScript engine (part of this project)
- [Zig](https://ziglang.org/) programming language
- Codex & Claude
