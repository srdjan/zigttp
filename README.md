<p align="center">
  <img src="docs/zigttp-logo.jpg" alt="zigttp" width="600">
  <p align="center"><a href="https://zigttp.timok.deno.net/">🌐 Web Site</a></p>
</p>

A JavaScript runtime built from scratch in Zig for serverless workloads. One binary, no dependencies, instant cold starts.

Where Node.js and Deno optimize for generality, zigttp optimizes for a single use case: running a request handler as fast as possible, then getting out of the way. It ships a pure-Zig JS engine (zts) with a JIT compiler, NaN-boxed values, and hidden classes - but skips everything a FaaS handler doesn't need (event loop, Promises, `require`).

[![status: experimental](https://github.com/GIScience/badges/raw/master/status/experimental.svg)](https://github.com/GIScience/badges#experimental)
> Not Ready (yet)! Still under active development.

### What makes it different

**Opinionated language subset.** TypeScript with the footguns removed. No classes, no `this`, no `var`, no `while` loops - just functions, `let`/`const`, arrow functions, destructuring, `for...of`, and `match` expressions. Unsupported features fail at parse time with a suggested alternative, not at runtime with a cryptic stack trace. Wait for it, there is a payoff for this :) 

**JSX as a first-class primitive.** The parser handles JSX directly - no Babel, no build step. Write TSX handlers that return server-rendered HTML.

**Compile-time verification.** `-Dverify` proves your handler correct at build time: every code path returns a Response, Result values are checked before access, no unreachable code. This works because zigttp's JS subset has no back-edges and no exceptions - the IR tree IS the control flow graph. `break` and `continue` within `for-of` are forward jumps only and don't compromise this property. See [verification docs](docs/verification.md).

**Sound mode.** The compiler uses type inference to catch bugs across all operators at compile time, not just in boolean contexts. Arithmetic on non-numeric types is rejected (`"hello" - 1`, `true * 5`, `env("X") / 2`). Mixed-type `+` is an error - use template literals. Tautological comparisons (`typeof x === "number"` when x is provably number) emit warnings. In boolean contexts, types with unambiguous falsy states are accepted (`if (count)`, `if (name)`), while objects and functions are rejected (always truthy). Values the static checker cannot prove are caught by runtime VM assertions. When types are statically proven, the compiler emits specialized opcodes (`add_num`, `lt_num`, etc.) that skip runtime type dispatch. See [sound mode docs](docs/sound-mode.md).

**Compile-time evaluation.** `comptime()` folds expressions at load time, modeled after Zig's comptime. Hash a version string, uppercase a constant, precompute a config value - all before the handler runs.

**Automatic runtime sandboxing.** Every precompiled handler is sandboxed by default. The compiler extracts a contract of what the handler does (env vars, outbound hosts, cache namespaces, SQL query names) and derives a least-privilege policy that restricts runtime access to exactly the proven values. No configuration required. `-Dcontract` additionally emits a `contract.json` manifest. `-Dpolicy=policy.json` overrides auto-derived sandboxing with an explicit policy. Non-literal arguments honestly report `"dynamic": true`.

**Handler effect classification.** Every virtual module function carries a compile-time effect annotation (read, write, or none). The compiler aggregates these to prove handler-level properties: pure (no external calls), read-only (no state mutations), stateless (no mutable state dependency), retry-safe (safe for Lambda auto-retry), and deterministic (no Date.now/Math.random). These flow into deployment manifests and the build report - no annotations or configuration needed.

**Full TypeScript type checking.** Beyond stripping type annotations, the compiler checks them. Variable types, function argument types, return types, property access on records, and virtual module function signatures are all validated at build time. Interface declarations with all-function members are treated as nominal types to prevent structural forgery.

**Structured concurrent I/O.** `parallel()` and `race()` from `zigttp:io` overlap outbound HTTP without async/await or Promises. Handler code stays synchronous and linear; concurrency happens in the I/O layer using OS threads. Three API calls at 50ms each complete in ~50ms total.

**Proven deployment manifests.** `-Ddeploy=aws` generates platform-specific deployment configurations (AWS SAM templates) directly from compiler-proven contracts. Env vars become parameters, routes become API events, egress hosts become tags. Proof levels (complete/partial/none) signal what was statically verified vs what needs manual review.

**Deterministic replay.** Record every I/O boundary during handler execution with `--trace`, then replay against a new handler version with `--replay` or `-Dreplay` at build time. Because virtual modules are the only I/O boundary, recording their inputs and outputs captures all external state - handlers become deterministic pure functions of (Request, VirtualModuleResponses).

**Durable execution.** `--durable <dir>` enables crash recovery and long-running workflows via write-ahead oplog. Wrap work in `run(key, fn)` from `zigttp:durable` with an idempotency key; each I/O call is persisted before returning to the handler. On crash recovery, recorded results are replayed without touching the network. Completed runs are deduplicated by key. Durable runs can suspend with `sleep(ms)`, `sleepUntil(unixMs)`, or `waitSignal(name)` and resume when the timer fires or a signal arrives via `signal(key, name, payload)`. A background scheduler polls for ready timers and signals using the same replay-safe recovery path.

**Guard composition.** `guard()` from `zigttp:compose` combined with the pipe operator (`|>`) composes handlers with pre/post guards at compile time. The parser desugars `guard(auth) |> guard(log) |> handler |> guard(cors)` into a single flat function with sequential if-checks - zero runtime overhead.

**Proven evolution.** `-Dprove=contract.json:traces.jsonl` compares two handler versions by diffing their contracts and replaying recorded traces. The result is classified as equivalent, additive, or breaking with a machine-readable proof certificate.

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
| `zigttp:sql` | `sql`, `sqlOne`, `sqlMany`, `sqlExec` | Registered SQLite queries with build-time schema validation |
| `zigttp:io` | `parallel`, `race` | Structured concurrent I/O (overlaps fetchSync calls using OS threads) |
| `zigttp:compose` | `guard` | Compile-time handler composition via pipe operator |
| `zigttp:durable` | `run`, `step`, `sleep`, `sleepUntil`, `waitSignal`, `signal`, `signalAt` | Durable execution with crash recovery, timers, and signals |

Each export carries an effect annotation used for handler property classification. Read-effect functions: all of env, crypto, router, auth, validate, plus `cacheGet`/`cacheStats`, `sql`/`sqlOne`/`sqlMany`. Write-effect functions: `cacheSet`/`cacheDelete`/`cacheIncr`, `sqlExec`, `parallel`/`race`, and all durable functions. `guard` has no runtime effect.

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
zig build -Dhandler=examples/sql-crud.ts -Dsql-schema=examples/sql/schema.sql
```

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
  --trace <FILE>        Record handler I/O traces to JSONL file
  --replay <FILE>       Replay recorded traces and verify handler output
  --sqlite <FILE>       SQLite database path for zigttp:sql
  --durable <DIR>       Enable durable execution with write-ahead oplog
```

## Key Features

**Performance**: NaN-boxing, index-based hidden classes with SoA layout and O(1) transition lookups, polymorphic inline cache (PIC), generational GC, hybrid arena allocation for request-scoped workloads.

**HTTP/FaaS Optimizations**: Shape preallocation for Request/Response objects, pre-interned HTTP atoms, HTTP string caching, LockFreePool handler isolation, zero-copy response mode.

**Compile-Time Analysis**: Handler verification (`-Dverify`) proves correctness at build time. Contract extraction and auto-sandboxing restrict runtime capabilities to proven values. `zigttp:sql` queries are prepared against a build-time schema snapshot via `-Dsql-schema=...`. Sound mode rejects non-numeric arithmetic, mixed-type `+`, and tautological comparisons at compile time, and emits type-specialized opcodes when types are proven. Full TypeScript type checking validates annotations against virtual module signatures.

**Structured Concurrency**: `parallel()` and `race()` overlap outbound HTTP using OS threads. No async/await, no event loop - handler code stays synchronous and linear.

**Deployment Pipeline**: Contract manifests (`-Dcontract`), proven deployment manifests (`-Ddeploy=aws`), auto-derived runtime sandboxing, deterministic replay (`--trace`/`--replay`/`-Dreplay`), proven evolution (`-Dprove`), and durable execution (`--durable`) form a pipeline from source analysis to production deployment with crash recovery.

**Language Support**: ES5 + select ES6 features (for...of with break/continue, typed arrays, exponentiation, pipe operator, compound assignments), native TypeScript/TSX stripping with type checking, compile-time evaluation with `comptime()`, direct JSX parsing, `match` expression.

**JIT Compilation**: Baseline JIT for x86-64 and ARM64, inline cache integration, object literal shapes, type feedback, adaptive compilation.

**Virtual Modules**: Native `zigttp:auth` (JWT/HS256, webhook signatures), `zigttp:validate` (JSON Schema), `zigttp:cache` (TTL/LRU key-value store), `zigttp:io` (structured concurrent I/O), `zigttp:compose` (guard composition), `zigttp:durable` (crash recovery, timers, signals), plus `zigttp:env`, `zigttp:crypto`, `zigttp:router`.

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

# Validate zigttp:sql queries against a schema snapshot
zig build -Dhandler=examples/sql-crud.ts -Dsql-schema=examples/sql/schema.sql

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
```

### Handler Verification (`-Dverify`)

The verifier statically proves six properties of your handler at compile time:

1. **Every code path returns a Response.** Missing `else` branches, `switch` cases without `default`, and paths that fall through without returning are all caught.
2. **Result values are checked before access.** Calls like `jwtVerify` and `validateJson` return Result objects. The verifier ensures `.ok` is checked before `.value` is accessed.
3. **No unreachable code.** Statements after an unconditional return produce a warning.
4. **No unused variables.** Declared variables that are never referenced produce a warning. Suppress with an underscore prefix (`_unused`).
5. **Match expressions have default arms.** A `match` without a default arm produces a warning.
6. **Optional values are checked before use.** Values from `env()`, `cacheGet()`, `parseBearer()`, and `routerMatch()` must be narrowed via `if (val)`, `val !== undefined`, `val ?? default`, or reassignment before use in expressions.

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
- **SQL queries** registered with `sql("name", "...")` - names, statement kinds, and touched tables are captured after schema validation
- **Handler properties** derived from effect classification of virtual module functions (pure, read_only, stateless, retry_safe, deterministic)
- **Verification results** (when combined with `-Dverify`)
- **Route patterns** (when combined with `-Daot`)

```json
{
  "version": 6,
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
  }
}
```

The `"dynamic": false` fields are the key signal. They mean "we can enumerate every value statically." When a handler uses a variable instead of a string literal (`env(someVar)` instead of `env("JWT_SECRET")`), the contract honestly reports `"dynamic": true`.

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
zigttp-server handler.ts --trace traces.jsonl
```

Every virtual module call, `fetchSync` response, `Date.now()` timestamp, and `Math.random()` value is recorded alongside the request and response.

**Replay** traces against a modified handler to detect regressions:

```bash
zigttp-server --replay traces.jsonl handler-v2.ts
```

Reports identical, status-changed, and body-changed results with structured diffs.

**Build-time replay** fails the build if regressions are detected:

```bash
zig build -Dhandler=handler-v2.ts -Dreplay=traces.jsonl
```

### Durable Execution (`--durable`)

Enable crash recovery with a write-ahead oplog:

```bash
zigttp-server handler.ts --durable ./oplogs
```

Handlers opt into durability via the `zigttp:durable` virtual module:

```typescript
import { run, step, sleep, waitSignal, signal } from "zigttp:durable";

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

`run(key, fn)` wraps a unit of work with an idempotency key. Each `step(name, fn)` persists its result to the oplog before returning to the handler. `sleep(ms)` and `sleepUntil(unixMs)` suspend the run until a timer fires. `waitSignal(name)` suspends until a signal arrives via `signal(key, name, payload)` or `signalAt(key, name, unixMs, payload)`. Pending runs return `202 Accepted` with a JSON body describing the wait. On crash recovery, recorded results are replayed without touching the network. A background scheduler polls for ready timers and signals. Completed runs are deduplicated by key.

### Proven Evolution (`-Dprove`)

Compare two handler versions and classify the upgrade:

```bash
zig build -Dhandler=handler-v2.ts -Dprove=old-contract.json:traces.jsonl
```

The system diffs the old and new contracts (env vars, egress hosts, cache namespaces, SQL query names, routes) and replays recorded traces against the new handler. Results are classified as:

- **equivalent**: Same contract, same responses for all recorded traces
- **additive**: New capabilities added (new env vars, new routes) but all existing behavior preserved
- **breaking**: Existing behavior changed or capabilities removed

Output is a machine-readable proof certificate and a human-readable report.

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
- [Sound Mode](docs/sound-mode.md) - Type-directed analysis: arithmetic safety, `+` safety, tautology detection, truthiness, type-specialized codegen
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
