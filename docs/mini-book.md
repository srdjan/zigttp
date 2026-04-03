# The zigttp Book

A guide to the serverless JavaScript runtime that proves your code correct before it ever runs.

---

## Table of Contents

1. [What Is zigttp?](#1-what-is-zigttp)
2. [Writing Handlers](#2-writing-handlers)
3. [Virtual Modules](#3-virtual-modules---native-power-zero-overhead)
4. [The zts Compiler](#4-the-zts-compiler)
5. [Compile-Time Verification](#5-compile-time-verification)
6. [Contracts, Sandboxing, and Deployment](#6-contracts-sandboxing-and-deployment)
7. [Testing and Replay](#7-testing-and-replay)
8. [Durable Execution](#8-durable-execution)
9. [Guard Composition](#9-guard-composition)
10. [Performance](#10-performance)
11. [Quick Reference](#11-quick-reference)

---

## 1. What Is zigttp?

Every serverless platform makes the same promise: write a function, deploy it, let the platform handle the rest. In practice, that promise comes with baggage. Node.js cold starts measure in hundreds of milliseconds. A "hello world" Lambda ships tens of megabytes of runtime. Your handler pulls in npm packages that pull in more packages, and nobody can tell you exactly what the function does at deploy time - what env vars it reads, what hosts it calls, whether every code path actually returns a response.

zigttp is a JavaScript runtime built from scratch in Zig for Function-as-a-Service workloads. Rather than running arbitrary JavaScript, zigttp restricts the language to an analyzable subset and uses those restrictions to prove things about your code at compile time.

Giving up features that make static analysis intractable - classes, exceptions, while loops, async/await, Promises - buys something no general-purpose runtime can offer. The compiler can walk every execution path through your handler, prove that each one returns a Response, and extract exactly which environment variables your code reads, which external hosts it contacts, and which cache namespaces it touches. From the source code alone, it generates a least-privilege sandbox, a deployment manifest, and a test suite.

### Design Philosophy

zigttp treats restrictions as features. Every language feature that was removed was removed because its absence enables a concrete capability:

No `class` or `this` means objects have predictable shapes that the hidden class system can optimize aggressively. No `while` or `do-while` means there are no back-edges in the control flow graph, which means the IR tree IS the control flow graph - no separate CFG construction needed. No `try/catch` means error handling is always explicit via Result types, and the verifier can prove you checked every result. No `null` means `undefined` is the single absent-value sentinel, simplifying every optional check. No `var` means bindings are block-scoped by default, making scope analysis trivial.

The result is a runtime where the compiler knows more about your handler than you do.

### Hello World

```bash
zig build run -- -e "function handler(req) { return Response.json({ok: true}); }"
```

Or as a file:

```typescript
// handler.ts
function handler(req: Request): Response {
    return Response.json({ message: "Hello from zigttp" });
}
```

```bash
zig build run -- handler.ts -p 3000
```

That is the entire surface area. One function, one request in, one response out. Everything else builds on top of this contract.

---

## 2. Writing Handlers

A zigttp handler is a function named `handler` that takes a `Request` and returns a `Response`. There are no middleware stacks, no app objects, no lifecycle hooks. The function is the unit of deployment.

### The Request Object

Every incoming HTTP request is converted into a `Request` object with these properties:

```typescript
function handler(req: Request): Response {
    const method = req.method;     // "GET", "POST", "PUT", "DELETE", etc.
    const url    = req.url;        // "/api/users/123?active=true"
    const path   = req.path;       // "/api/users/123"
    const query  = req.query;      // "active=true"
    const body   = req.body;       // request body as string (or undefined)
    const headers = req.headers;   // header object with .get(), .has()
    // ...
}
```

The Request shape is preallocated at startup with a hidden class that has slots for all six properties. This means accessing `req.method` is a direct slot read - no property lookup, no hash table probe, no hidden class transition.

### Response Helpers

zigttp provides four response constructors that cover the common cases:

```typescript
// JSON response (sets Content-Type: application/json)
Response.json({ users: [1, 2, 3] })
Response.json({ error: "not found" }, { status: 404 })

// Plain text
Response.text("OK")
Response.text("Bad Request", { status: 400 })

// HTML
Response.html("<h1>Hello</h1>")

// Redirect
Response.redirect("/login")
Response.redirect("/new-location", 301)
```

Each helper returns a `Response` object with `body`, `status`, `statusText`, `ok`, and `headers` properties - again, preallocated with a hidden class for direct slot access.

### TypeScript Support

zigttp includes a native TypeScript stripper written in Zig. There is no dependency on `tsc`, no `node_modules`, no build step. You write `.ts` or `.tsx` files and the runtime strips type annotations at load time, preserving line numbers for accurate error reporting.

```typescript
type RequestData = {
    name: string;
    count: number;
};

interface ResponseData {
    message: string;
    timestamp: number;
}

function processData(data: RequestData): ResponseData {
    return {
        message: "Hello, " + data.name,
        timestamp: Date.now()
    };
}

function handler(req: Request): Response {
    const data: RequestData = { name: "World", count: 42 };
    const result: ResponseData = processData(data);
    return Response.json(result);
}
```

Beyond stripping, the zts type checker validates annotations against actual usage. If you annotate a parameter as `string` and pass a number, the compiler catches it. Virtual module return types are fully typed - `jwtVerify` returns `{ok: boolean, value?: object, error?: string}`, and the type checker knows this. Generic type aliases are fully supported: `type Result<T> = { ok: boolean; value: T; error: string }` can be used in annotations like `const auth: Result<object>`, and the checker instantiates the body with the provided type arguments.

### JSX and Server-Side Rendering

JSX is a first-class syntax in zts. The parser handles `.jsx` and `.tsx` files directly - no Babel, no React, no virtual DOM library. JSX compiles to `h()` calls, and `renderToString()` produces HTML:

```jsx
function HomePage() {
    return (
        <html>
            <head><title>zigttp</title></head>
            <body>
                <h1>zigttp</h1>
                <p>A tiny JavaScript runtime for HTTP handlers.</p>
            </body>
        </html>
    );
}

function handler(request) {
    if (request.url === "/" && request.method === "GET") {
        return Response.html(renderToString(<HomePage />));
    }
    return Response.json({ error: "Not Found" }, { status: 404 });
}
```

Components are plain functions. Props are plain objects. Children are passed as additional arguments. No component lifecycle, no state management, no re-rendering.

### Compile-Time Evaluation

The `comptime()` function evaluates expressions at load time and replaces them with their result:

```typescript
const x = comptime(1 + 2 * 3);                 // becomes: const x = 7;
const upper = comptime("hello".toUpperCase());  // becomes: const upper = "HELLO";
const etag = comptime(hash("content-v1"));      // becomes: const etag = "a1b2c3d4";
```

This is useful for configuration constants, precomputed hashes, and any value that should not be recomputed on every request.

---

## 3. Virtual Modules - Native Power, Zero Overhead

Most serverless handlers need the same handful of capabilities: read an environment variable, verify a JWT, validate a JSON body, cache a result. In a Node.js Lambda, each of these is an npm package with its own dependency tree, its own cold start cost, and its own surface area for supply chain attacks.

zigttp replaces all of this with virtual modules - native Zig implementations exposed through ES module syntax. When you write `import { jwtVerify } from "zigttp:auth"`, the import resolves to a native function that executes directly in Zig. There is no JavaScript interpretation overhead for these calls. There is no npm. There is no `node_modules` directory.

Each virtual module export carries metadata that the compiler uses for verification, contract extraction, and effect classification. This metadata is the foundation for everything the compiler can prove about your handler.

### zigttp:env

```typescript
import { env } from "zigttp:env";

const apiKey = env("API_KEY");           // string | undefined
const port = env("PORT") ?? "8080";      // string (narrowed via ??)
```

`env()` returns the value of an environment variable, or `undefined` if it is not set. The compiler tracks every literal env var name it sees and includes them in the handler contract. When the contract shows `dynamic: false` for env vars, the runtime sandbox restricts the handler to exactly those variables.

### zigttp:crypto

```typescript
import { sha256, hmacSha256, base64Encode, base64Decode } from "zigttp:crypto";

const hash = sha256("hello world");
const mac = hmacSha256("message", "secret-key");
const encoded = base64Encode("binary data");
const decoded = base64Decode(encoded);
```

Stateless cryptographic primitives. SHA-256 hashing, HMAC-SHA256 for message authentication, and base64 encoding/decoding. All implemented in Zig with no allocations on the hot path.

### zigttp:router

```typescript
import { routerMatch } from "zigttp:router";

const routes = {
    "GET /":           getHome,
    "GET /health":     getHealth,
    "GET /users/:id":  getUser,
    "POST /echo":      postEcho,
};

function handler(req) {
    const match = routerMatch(routes, req);
    if (match) {
        req.params = match.params;   // { id: "123" }
        return match.handler(req);
    }
    return Response.json({ error: "Not Found" }, { status: 404 });
}
```

Pattern-matching HTTP router with path parameter extraction. Route patterns are `"METHOD /path/:param"` strings. The router returns `{ handler, params }` on match, or `undefined` on miss. The compiler extracts all route patterns into the contract for deployment manifest generation.

### zigttp:auth

```typescript
import { parseBearer, jwtVerify, jwtSign, verifyWebhookSignature } from "zigttp:auth";

// Extract token from "Bearer <token>" header
const token = parseBearer(req.headers.get("authorization"));  // string | undefined

// Verify JWT (HS256)
const result = jwtVerify(token, env("JWT_SECRET") ?? "secret");
if (!result.ok) {
    return Response.json({ error: result.error }, { status: 401 });
}
const claims = result.value;  // decoded payload

// Sign a JWT
const jwt = jwtSign(JSON.stringify({ sub: "user-123" }), "secret");

// Verify webhook signature (HMAC-SHA256)
const valid = verifyWebhookSignature(body, signature, secret);
```

`jwtVerify` returns a Result type: `{ ok: boolean, value?: object, error?: string }`. The verifier enforces that you check `.ok` before accessing `.value`. The effect classifier marks `jwtVerify` as a read operation and its failure as `critical` severity - meaning the fault coverage checker will warn if a failed JWT verification leads to a 200 response.

### zigttp:validate

```typescript
import { schemaCompile, validateJson, validateObject, coerceJson } from "zigttp:validate";

// Compile a JSON Schema (typically at module scope, runs once)
schemaCompile("user", JSON.stringify({
    type: "object",
    required: ["name", "age"],
    properties: {
        name: { type: "string", minLength: 1, maxLength: 100 },
        age:  { type: "integer", minimum: 0, maximum: 200 }
    }
}));

// Validate a JSON string against a compiled schema
const result = validateJson("user", req.body);
if (!result.ok) {
    return Response.json({ errors: result.errors }, { status: 400 });
}
const user = result.value;  // parsed and validated object

// Validate an already-parsed object
const objResult = validateObject("user", someObject);

// Coerce JSON to a typed form
const coerced = coerceJson("user", '{"name":"Alice","age":"30"}');
```

Schemas are compiled once and stored in a per-runtime SchemaRegistry. The registry persists across requests within a pool slot, so schema compilation is a one-time cost. Supported JSON Schema keywords: `type`, `required`, `properties`, `minLength`, `maxLength`, `minimum`, `maximum`, `enum`, `items`.

### zigttp:decode

```typescript
import { decodeJson, decodeForm, decodeQuery } from "zigttp:decode";

// Preferred schema-backed ingress helpers
const json = decodeJson("user", req.body ?? "{}");
const form = decodeForm("user", req.body ?? "");
const query = decodeQuery("user.filters", req.query ?? {});
```

These helpers reuse the same compiled schemas as `zigttp:validate`, but make
request ingress the primary typed surface: JSON bodies, URL-encoded forms, and
query strings all come back as validated Result values.

### zigttp:cache

```typescript
import { cacheGet, cacheSet, cacheDelete, cacheIncr, cacheStats } from "zigttp:cache";

// Namespace-isolated key-value cache
cacheSet("sessions", "abc123", JSON.stringify({ user: "alice" }), 300);  // TTL in seconds
const session = cacheGet("sessions", "abc123");  // string | undefined

// Atomic increment (returns new value)
const count = cacheIncr("counters", "page-views");  // number

// Delete a key
cacheDelete("sessions", "abc123");

// Cache statistics
const stats = cacheStats();  // { hits, misses, size, evictions }
```

In-memory key-value cache with namespace isolation, LRU eviction, and lazy TTL expiration. The cache persists across requests within the same pool slot, making it effective for warm instance reuse. The compiler extracts namespace strings into the contract, and the sandbox can restrict cache access to proven namespaces.

### zigttp:sql

```typescript
import { sqlQuery, sqlExecute } from "zigttp:sql";

// Read query (returns array of row objects)
const users = sqlQuery("SELECT * FROM users WHERE active = ?", [1]);

// Write operation (returns { changes, lastInsertRowid })
const result = sqlExecute("INSERT INTO users (name, email) VALUES (?, ?)", ["Alice", "alice@example.com"]);
```

SQLite queries with optional build-time schema validation. When a `-Dsql-schema` build option points to a schema snapshot, the compiler validates that query column references match the actual schema. Named query allowlisting restricts which SQL statements the handler can execute.

### zigttp:io

```typescript
import { parallel, race } from "zigttp:io";

// Execute three API calls concurrently using OS threads
const [user, orders, inventory] = parallel([
    () => fetchSync("https://users.internal/api/v1/123"),
    () => fetchSync("https://orders.internal/api/v1?user=123"),
    () => fetchSync("https://inventory.internal/api/v1/789")
]);

// Return the first successful result
const fastest = race([
    () => fetchSync("https://primary.api/data"),
    () => fetchSync("https://fallback.api/data")
]);
```

Structured concurrent I/O. `parallel()` executes `fetchSync` calls on OS threads and returns results in declaration order. `race()` returns the first successful response. These are the only concurrency primitives in zigttp - there are no Promises, no async/await, no event loop. Concurrency is explicit and structured.

### zigttp:compose

```typescript
import { guard } from "zigttp:compose";

const preflight = (req: Request): Response | undefined => {
    if (req.method === "OPTIONS") {
        return Response.text("", { status: 204, headers: { "Access-Control-Allow-Origin": "*" } });
    }
};

const requireAuth = (req: Request): Response | undefined => {
    const token = parseBearer(req.headers.get("authorization"));
    if (!token) return Response.json({ error: "unauthorized" }, { status: 401 });
};

const handler = guard(preflight)
    |> guard(requireAuth)
    |> routeHandler;
```

Compile-time handler composition. The parser sees the pipe chain, collects the `guard()` calls, and desugars the entire chain into a single flat function with sequential if-checks. Pre-guards receive the request and short-circuit on non-undefined return. Post-guards (after the main handler) receive the response and can replace it. This is a pure compile-time macro - zero runtime overhead, zero middleware dispatch.

### zigttp:durable

```typescript
import { run, step, sleep, waitSignal, signal } from "zigttp:durable";

function handler(req: Request): Response {
    const result = run("order-" + orderId, () => {
        const payment = step("charge", () => {
            return fetchSync("https://payments.api/charge", { method: "POST", body: chargeBody });
        });

        sleep(5000);  // suspends, returns 202, resumes on next poll

        const confirmation = step("confirm", () => {
            return fetchSync("https://orders.api/confirm", { method: "POST", body: confirmBody });
        });

        return { payment, confirmation };
    });

    return Response.json(result);
}
```

Durable execution with crash recovery, timers, and inter-handler signaling. `run()` wraps idempotent work keyed by a string. `step()` persists sub-results to a write-ahead oplog. If the process crashes mid-execution, recovery replays the oplog on restart - completed steps return their recorded results (zero real I/O), and execution resumes from where it left off. `sleep()` and `waitSignal()` suspend the handler and return a 202 response; a background scheduler polls and resumes when the condition is met.

### Effect Classification

Every virtual module export carries an effect annotation: `read`, `write`, or `none`. The contract builder aggregates these across all imported functions to derive handler-level properties. A handler that only calls `env()` and `cacheGet()` is classified as `read_only`. A handler that calls `cacheSet()` but wraps it in a durable `step()` is classified as `retry_safe`. A handler that makes no virtual module calls at all is `pure`.

These classifications flow into deployment manifests as machine-readable tags, into sandbox policies as access restrictions, and into operational dashboards as safety guarantees.

---

## 4. The zts Compiler

zts is the JavaScript engine at the heart of zigttp. It is written entirely in Zig with no external dependencies - no libc (except for SQLite), no LLVM, no runtime library. Everything from the tokenizer to the JIT compiler is purpose-built for the zigttp use case.

### Two-Pass Compilation

The compiler works in two phases. The first pass tokenizes the source and runs a Pratt parser that produces an Intermediate Representation (IR) - a tree of typed nodes representing expressions, statements, scopes, and bindings. The second pass walks the IR and emits bytecode for the stack-based VM.

This two-pass design is deliberate. The IR tree is the artifact that all static analysis operates on: the verifier walks it to check exhaustive returns, the contract builder walks it to extract capabilities, the path generator walks it to enumerate execution paths, the bool checker walks it for type-directed analysis. Because zigttp's JS subset has no back-edges (no while loops, no goto), the IR tree IS the control flow graph. Every compile-time analysis is a tree walk instead of a graph algorithm.

### TypeScript Stripping

The TypeScript stripper is a separate pass that runs before parsing. It removes type annotations, interface declarations, type aliases, and generics while preserving source positions for accurate error reporting. Generic type aliases (`type Result<T> = { ok: boolean; value: T }`) are recorded with their parameter names; the type checker instantiates them when used in annotations (`Result<string>` resolves to the concrete record type). The stripper also enforces zigttp-specific TypeScript restrictions: the `any` type is a compile-time error (use `unknown` with narrowing instead), and type assertions (`as`, `satisfies`) are rejected (use type guards instead).

### Sound Mode

Sound mode is zigttp's type-directed analysis layer. The BoolChecker walks the IR tree, infers expression types from context, and applies safety rules:

**Type-directed truthiness.** In boolean contexts (if conditions, logical operators), only types with unambiguous falsy values are accepted. Booleans, numbers, strings, and optionals work naturally. Objects and functions are rejected because they are always truthy - an `if (obj)` check is dead code that the programmer probably did not intend. Undefined emits a warning because it is always false.

**Arithmetic type safety.** Arithmetic operators (`-`, `*`, `/`, `%`, `**`) require numeric operands. Writing `"hello" - 1` is a compile-time error, not a runtime NaN. The `+` operator rejects mixed types: `count + " items"` is an error because the programmer should use a template literal instead.

**Tautological comparison detection.** If the compiler can prove that `typeof x === "string"` is always true (because `x` is provably a string), it emits a warning. Same for `x === undefined` when `x` is provably non-optional.

**Type-specialized codegen.** When the BoolChecker proves both operands of an arithmetic or comparison are numbers, it populates a `NodeTypeMap`. The code generator emits specialized opcodes (`add_num`, `sub_num`, `lt_num`, etc.) that skip runtime type dispatch entirely. These opcodes provide faster cold-start execution because they do not need JIT warmup to reach peak performance.

### Progressive Type Inference

zts infers types progressively from virtual module return signatures, match expression arms, optional narrowing, and result property access:

```typescript
const val = env("KEY");        // inferred: optional_string (string | undefined)
if (val) {
    // val is narrowed to string here
    const upper = val.toUpperCase();  // valid
}

const result = jwtVerify(token, secret);
if (result.ok) {                      // result.ok inferred as boolean
    const claims = result.value;      // valid only in this branch
}

const name = env("NAME") ?? "default";  // inferred: string (resolved via ??)
```

### NaN-Boxing

Every JavaScript value in zts is a 64-bit word using type-prefix NaN-boxing. Each type owns a unique upper 16-bit prefix: pointers use 0xFFFC, integers use 0xFFFD, special values (true, false, undefined) use 0xFFFE, and extern references use 0xFFFF. Raw f64 doubles are stored inline whenever the prefix is below 0xFFFC. Checking whether a value is, say, an integer is a single shift-and-compare: `(raw >> 48) == 0xFFFD`. No tag bits stolen from pointers, no boxing allocations, no union discriminants.

### Hidden Classes and Inline Caching

zts implements V8-style hidden classes for objects. When you create `{ name: "Alice", age: 30 }`, the runtime constructs a hidden class chain: empty -> +name -> +name+age. Objects with the same property sequence share the same hidden class. Property access sites are backed by an 8-entry polymorphic inline cache (PIC) that maps hidden class to property slot offset. Monomorphic sites (one shape seen) resolve in O(1) via the last-hit optimization. Objects with 8 or more properties fall back to binary search on sorted property arrays.

For HTTP objects, this goes further. Request and Response shapes are preallocated at startup with known property layouts. Creating a Request does not trigger any hidden class transitions - the object is born with its final shape, and property values are written directly to inline slots via `setSlot()`.

### JIT Baseline Compiler

zts includes a baseline JIT compiler for x86-64 and ARM64. The JIT watches call sites via type feedback counters and compiles hot functions to native code after a configurable threshold (default: 100 calls in lazy mode, 25 in eager mode). The JIT generates fast paths for inline cache hits, type-proven arithmetic, and property access patterns observed during interpretation. When a fast path misses, execution falls back to a helper that updates the cache and returns to native code.

For FaaS workloads, the JIT threshold is intentionally lower than a general-purpose engine (the inlining threshold is 5 calls instead of 10) to account for shorter handler lifetimes. The JIT can also be disabled entirely (`-Djit=disabled`) for maximum cold start predictability.

---

## 5. Compile-Time Verification

The `-Dverify` build flag enables the handler verifier, which statically proves six properties of your handler at compile time. If any check fails, the build fails with a precise error message pointing to the problematic code path.

### Check 1: Exhaustive Response Returns

The verifier walks every code path through your handler function and proves that each one ends with a `return` statement that produces a `Response`. A handler with an if/else where the else branch falls through without returning is a build error, not a runtime 500.

```typescript
// BUILD ERROR: not all code paths return a Response
function handler(req: Request): Response {
    if (req.method === "GET") {
        return Response.json({ ok: true });
    }
    // falls through - what happens for POST?
}
```

### Check 2: Result Values Checked Before Access

Virtual module functions like `jwtVerify`, `decodeJson`, and `decodeQuery` return Result types with an `.ok` property. The verifier tracks these bindings and ensures you check `.ok` before accessing `.value` or `.error`:

```typescript
// BUILD ERROR: result used without checking .ok
const result = jwtVerify(token, secret);
return Response.json(result.value);  // what if verification failed?
```

### Check 3: No Unreachable Code

Statements after an unconditional return are flagged:

```typescript
function handler(req: Request): Response {
    return Response.json({ ok: true });
    const x = 42;  // BUILD ERROR: unreachable code after return
}
```

### Check 4: Unused Variables

Declared variables that are never referenced produce warnings. Prefix with underscore to suppress:

```typescript
const _unused = env("DEBUG");  // OK, underscore suppresses warning
const forgotten = 42;          // WARNING: unused variable 'forgotten'
```

### Check 5: Non-Exhaustive Match

Match expressions (zigttp's pattern matching) must have a default arm:

```typescript
const result = match (status) {
    200 => "ok",
    404 => "not found",
    // BUILD ERROR: match expression needs a default arm
};
```

### Check 6: Optional Values Checked Before Use

Functions like `env()`, `cacheGet()`, `parseBearer()`, and `routerMatch()` return optional values (the value or `undefined`). The verifier tracks these bindings through the control flow tree and recognizes five narrowing patterns:

```typescript
const token = parseBearer(header);

// Pattern 1: truthiness guard
if (token) { /* token is string here */ }

// Pattern 2: negated early return
if (!token) return Response.json({ error: "no token" }, { status: 401 });
// token is string after this point

// Pattern 3: explicit undefined check
if (token !== undefined) { /* token is string here */ }

// Pattern 4: nullish coalescing
const safe = token ?? "default";  // safe is string

// Pattern 5: reassignment
let value = env("KEY");
value = value ?? "fallback";  // value is string after reassignment
```

### Why This Works

These checks are possible because of zigttp's restricted JavaScript subset. Without `while` loops, there are no back-edges in the control flow. Without `try/catch`, there are no invisible exception paths. Without `break` to labels or `continue` across function boundaries, control flow is strictly tree-shaped. The verifier does not need to build a separate control flow graph, compute dominance frontiers, or iterate to a fixed point. It walks the IR tree once, top to bottom, and every path it encounters is a real path the handler can take.

---

## 6. Contracts, Sandboxing, and Deployment

### Contract Extraction

Every time you precompile a handler with `-Dhandler=<path>`, the compiler automatically extracts a contract - a complete manifest of everything the handler does. The `-Dcontract` flag additionally writes this to `contract.json`.

The contract captures:

- **Virtual module imports**: which modules and functions the handler uses
- **Environment variables**: every literal `env("NAME")` call, with `dynamic: true` if any non-literal argument is detected
- **Outbound hosts**: every literal URL in `fetchSync()` calls
- **Cache namespaces**: every literal namespace in cache operations
- **Route patterns**: every route string passed to `routerMatch`
- **SQL queries**: every SQL statement with operation type and table references
- **Durable usage**: step names, timer usage, signal names

Each section has a `dynamic` flag. When `dynamic: false`, the compiler has enumerated every possible value. When `dynamic: true`, at least one call site uses a computed argument that the compiler cannot resolve statically. This is an honest signal, not a failure - it tells operators exactly where manual review is needed.

### Handler Properties

The contract builder computes handler-level properties from effect classification:

| Property | Meaning | Derived From |
|----------|---------|-------------|
| `pure` | No virtual module calls, no egress | Zero I/O of any kind |
| `read_only` | Only read-classified functions | env, cacheGet, parseBearer, jwtVerify, routerMatch |
| `stateless` | Read-only and no cache reads | No cacheGet calls |
| `retry_safe` | Safe to retry on failure | Read-only, or all writes inside durable steps |
| `deterministic` | Same input always produces same output | No Date.now() or Math.random() |
| `has_egress` | Makes outbound HTTP calls | Uses fetchSync |
| `idempotent` | Safe for at-least-once delivery | Deterministic AND retry-safe |
| `max_io_depth` | Maximum I/O calls per request | PathGenerator path analysis |
| `injection_safe` | No unvalidated input in sinks | FlowChecker label tracking |
| `state_isolated` | No cross-request data leakage | Verifier Check 7 (module-scope mutation) |

These properties flow into deployment manifests as machine-readable tags, OpenAPI specs as `x-zigttp-properties` extensions, OWASP Top 10 compliance mapping, cost estimation, and operational tooling as safety guarantees.

### Runtime Sandboxing

When a handler is precompiled, the compiler automatically generates a `RuntimePolicy` from the contract. The policy restricts the handler to exactly the capabilities the compiler can prove it needs:

- If the contract lists env vars `["API_KEY", "DB_URL"]` with `dynamic: false`, the sandbox blocks access to any other environment variable
- If the contract lists egress hosts `["api.stripe.com"]` with `dynamic: false`, the sandbox blocks outbound HTTP to any other host
- If a section has `dynamic: true`, that section remains permissive

The policy is embedded as a constant in the generated binary. There is no configuration file to drift out of sync, no runtime lookup, no way for the handler to bypass its sandbox. An explicit `--policy` file can override the auto-derived policy when needed.

### Proven Deployment Manifests

The `-Ddeploy=aws` build option generates a complete AWS SAM deployment template from the compiler-proven contract. The system extracts platform-agnostic `ProvenFacts` from the contract and renders them into target-specific configuration:

```bash
zig build -Dhandler=handler.ts -Ddeploy=aws
```

This produces `src/generated/deploy/template.json` (AWS SAM template) and `src/generated/deploy/deploy-report.txt`. The template includes:

- Proven environment variables as CloudFormation parameters
- Route patterns as HttpApi events
- Egress hosts as tags and conditional VPC SecurityGroup (activated via `VpcSubnetIds` parameter)
- Handler properties as metadata tags (retrySafe, readOnly, injectionSafe, idempotent, stateIsolated, maxIoDepth)
- Proof level (complete/partial/none) as a top-level field

The deploy report includes: PROVEN/NEEDS MANUAL REVIEW sections, FLOW ANALYSIS (secret/credential leakage, input validation), FAULT COVERAGE, HANDLER PROPERTIES, OWASP TOP 10 COVERAGE (A01-A07 mapped to proven properties), VPC EGRESS (when egress hosts exist), and COST ESTIMATE (AWS Lambda pricing derived from proven I/O depth).

Two standalone tools support the deployment workflow:
- `zts prove old.json new.json` compares two contracts and classifies the change (exit 0 for safe, 1 for breaking)
- `zts mock tests.jsonl --port 3001` serves mock HTTP responses from PathGenerator test cases

### Fault Coverage

The `-Dgenerate-tests=true` flag enables exhaustive path enumeration and fault coverage analysis. The `PathGenerator` walks the handler IR, forking at every branch point and I/O success/failure boundary, producing test cases for each execution path. The `FaultCoverageChecker` then analyzes these paths against failure severity annotations on each virtual module function.

`jwtVerify` returning `{ok: false}` is annotated as `critical` severity. If any execution path leads from a critical I/O failure to a 2xx response, the fault coverage checker warns. This catches the most common class of serverless bugs - a handler that returns 200 even when authentication failed because the developer forgot a guard clause. Cache misses returning 200 are correctly classified as graceful degradation and produce no warning.

### Rate Limit Extraction

When a handler uses guard composition (`zigttp:compose`) and calls `cacheIncr`, the compiler detects the rate limiting pattern and extracts it into the contract. The `rateLimiting` section in `contract.json` captures the cache namespace and whether it is static or dynamic. This enables API gateways and documentation to automatically include rate limit metadata without manual configuration.

---

## 7. Testing and Replay

### The Determinism Property

zigttp handlers have a property that almost no other serverless runtime can claim: they are deterministic pure functions of their inputs. Virtual modules are the ONLY I/O boundary - there is no filesystem access, no network access outside of `fetchSync`, no shared mutable state, no non-deterministic builtins (except `Date.now()` and `Math.random()`, which are intercepted). If you record the request and every virtual module response, you can replay the handler and get the same output every time.

### Trace Recording

The `--trace traces.jsonl` flag records every request processed by the handler:

```bash
zig build run -- handler.ts --trace traces.jsonl
```

Each request produces a sequence of JSONL lines:

```json
{"type":"request","method":"GET","url":"/api/users","headers":{"authorization":"Bearer eyJ..."}}
{"type":"io","seq":0,"module":"auth","fn":"parseBearer","args":["Bearer eyJ..."],"result":"eyJ..."}
{"type":"io","seq":1,"module":"auth","fn":"jwtVerify","args":["eyJ...","secret"],"result":{"ok":true,"value":{"sub":"123"}}}
{"type":"response","status":200,"body":"{\"user\":{\"sub\":\"123\"}}"}
{"type":"meta","duration_us":142,"handler":"handler.ts","pool_slot":3,"io_count":2}
```

This trace captures the complete execution context. The request, every virtual module call with its arguments and return value, and the final response.

### Replay Verification

The `--replay` flag replays recorded traces against a handler and compares outputs:

```bash
zig build run -- handler.ts --replay traces.jsonl
```

During replay, virtual module calls return the recorded values instead of executing real logic. The runner compares the handler's actual response (status and body) against the recorded response. This catches regressions: if you refactor a handler and the output changes for any recorded scenario, replay tells you exactly which trace diverged and how.

Build-time replay via `-Dreplay=traces.jsonl` integrates this into the precompile pipeline. The build fails if any trace produces a different response than recorded, giving you regression protection as part of the compile step.

### Declarative Handler Testing

Because handlers are pure functions of (Request, VirtualModuleResponses), testing them requires no mocking frameworks, no test servers, no infrastructure. You declare inputs and expected outputs in JSONL:

```json
{"type":"test","name":"authenticated user gets profile"}
{"type":"request","method":"GET","url":"/api/profile","headers":{"authorization":"Bearer valid-token"}}
{"type":"io","seq":0,"module":"auth","fn":"parseBearer","args":["Bearer valid-token"],"result":"valid-token"}
{"type":"io","seq":1,"module":"auth","fn":"jwtVerify","args":["valid-token","secret"],"result":{"ok":true,"value":{"sub":"user-123"}}}
{"type":"expect","status":200,"bodyContains":"user-123"}
```

```json
{"type":"test","name":"missing auth returns 401"}
{"type":"request","method":"GET","url":"/api/profile"}
{"type":"expect","status":401}
```

Run tests at runtime with `--test tests.jsonl` or at build time with `-Dtest-file=tests.jsonl`. Each test registers replay stubs for the declared I/O calls, executes the handler with the declared request, and compares the actual response against the assertions. Exit code 1 on any failure.

### Exhaustive Path Generation

The path generator produces test cases automatically. It walks the handler's IR tree, forking at every branch point and I/O boundary, and emits a test for each distinct execution path. Combined with fault coverage analysis, this gives you a complete picture of your handler's behavior without writing a single test by hand.

---

## 8. Durable Execution

FaaS platforms kill instances without warning. If your handler makes three API calls and the instance dies after the second, you lose work. Retry logic is brittle. Idempotency keys help but do not solve the underlying problem: the handler has no memory of what it already did.

zigttp's durable execution mode solves this with a write-ahead oplog. Enable it with `--durable <dir>`:

```bash
zig build run -- handler.ts --durable ./oplogs -p 3000
```

### How It Works

When a request arrives, the runtime creates an oplog file and writes the request to it. As the handler executes, every virtual module call's result is written to the oplog before being returned to the handler. When the handler completes, a "complete" marker is written and the oplog is deleted.

If the process crashes mid-execution, the oplog survives on disk. On restart, the runtime scans the oplog directory for incomplete files, re-executes the handlers, and replays recorded I/O from the oplog. Completed steps return their recorded results with zero real I/O. When the oplog is exhausted, execution transitions to live mode for any remaining calls.

### Durable Steps

The `step()` function persists sub-results:

```typescript
import { run, step } from "zigttp:durable";

const result = run("process-order-42", () => {
    // If this step already ran (recovered from crash), returns recorded result
    const payment = step("charge-card", () => {
        return fetchSync("https://payments.api/charge", {
            method: "POST",
            body: JSON.stringify({ amount: 4999, currency: "usd" })
        });
    });

    // Same for this step
    const confirmation = step("send-confirmation", () => {
        return fetchSync("https://email.api/send", {
            method: "POST",
            body: JSON.stringify({ to: "user@example.com", template: "order-confirm" })
        });
    });

    return { payment: payment.status, confirmation: confirmation.status };
});
```

### Timers and Signals

`sleep(ms)` suspends the handler and returns a `202 Accepted` response. A background scheduler polls incomplete oplogs and resumes execution when the timer fires:

```typescript
import { run, step, sleep, waitSignal, signal } from "zigttp:durable";

// Long-running workflow
const result = run("approval-flow-" + requestId, () => {
    step("request-approval", () => {
        return fetchSync("https://approvals.api/request", { method: "POST", body: approvalBody });
    });

    // Suspend until approval signal arrives
    const approval = waitSignal("approved");

    step("process-approval", () => {
        return fetchSync("https://orders.api/fulfill", { method: "POST", body: approval.payload });
    });
});

// Another handler sends the signal
signal("approval-flow-" + requestId, "approved", JSON.stringify({ approved: true }));
```

`signalAt(key, name, unixMs, payload?)` schedules a signal for a future timestamp. The `DurableStore` persists signals to the filesystem and the `DurableScheduler` delivers them when due.

---

## 9. Guard Composition

Most web frameworks implement middleware as a runtime dispatch chain - each middleware function calls `next()` to pass control to the next one. This works but has costs: every middleware hop is a function call with its own stack frame, error handling is implicit, and the framework cannot optimize the chain because it does not know its shape until runtime.

zigttp's guard composition is a compile-time alternative. You declare a chain using the pipe operator and `guard()`, and the parser desugars it into a single flat function:

```typescript
import { guard } from "zigttp:compose";
import { parseBearer, jwtVerify } from "zigttp:auth";
import { routerMatch } from "zigttp:router";
import { env } from "zigttp:env";

// Pre-guard: handle CORS preflight
const preflight = (req: Request): Response | undefined => {
    if (req.method === "OPTIONS") {
        return Response.text("", {
            status: 204,
            headers: {
                "Access-Control-Allow-Origin": "*",
                "Access-Control-Allow-Methods": "GET, POST",
                "Access-Control-Allow-Headers": "Authorization, Content-Type",
            }
        });
    }
};

// Pre-guard: require authentication
const requireAuth = (req: Request): Response | undefined => {
    const token = parseBearer(req.headers.get("authorization"));
    if (!token) return Response.json({ error: "unauthorized" }, { status: 401 });
    const result = jwtVerify(token, env("JWT_SECRET") ?? "secret");
    if (!result.ok) return Response.json({ error: result.error }, { status: 403 });
};

// Main handler
function routeHandler(req: Request): Response {
    const found = routerMatch(routes, req);
    if (found) {
        req.params = found.params;
        return found.handler(req);
    }
    return Response.json({ error: "Not Found" }, { status: 404 });
}

// Composed: preflight -> auth -> routes
const handler = guard(preflight)
    |> guard(requireAuth)
    |> routeHandler;
```

The parser transforms this into roughly:

```typescript
function handler(req) {
    const _g0 = preflight(req);
    if (_g0 !== undefined) return _g0;
    const _g1 = requireAuth(req);
    if (_g1 !== undefined) return _g1;
    return routeHandler(req);
}
```

This desugared form uses standard IR nodes, which means the verifier, contract extractor, and all other compile-time analyses handle it transparently. The guard chain is not a framework feature - it is a language feature that the compiler understands.

Rules: exactly one non-guard handler is required in the chain. `guard()` must be imported from `zigttp:compose`. Pre-guards receive the request and return `Response | undefined` (undefined means "continue"). Post-guards (placed after the handler) receive the response and can replace it.

---

## 10. Performance

zigttp is optimized for the FaaS performance profile: cold start latency matters more than sustained throughput. Every architectural decision - from NaN-boxing to shape preallocation to the JIT threshold - is tuned for handlers that might process a single request before the instance is recycled.

### Cold Start: Precompiled Bytecode

The single largest cold start optimization is handler precompilation. With `-Dhandler=handler.ts`, the build embeds compiled bytecode directly into the binary:

```bash
zig build -Dhandler=handler.ts -Doptimize=ReleaseFast
```

The resulting binary contains no JavaScript source. On startup, the server calls `loadFromCachedBytecode()` and begins executing immediately - no tokenizer, no parser, no code generation. Benchmarks show this eliminates roughly 16% of cold start time compared to runtime parsing.

### Request Pipeline Optimizations

Each request flows through a pipeline optimized at every stage:

**Shape preallocation.** Request and Response objects are created with their final hidden class shapes. No hidden class transitions during request handling. Property writes go directly to inline slots.

**Pre-interned HTTP atoms.** 27 common HTTP header names (content-type, authorization, cache-control, etc.) are pre-interned as atoms. Looking up `req.headers["authorization"]` is an atom comparison, not a string comparison.

**HTTP string cache.** Status texts ("OK", "Not Found"), content-type strings ("application/json", "text/html"), and HTTP method strings ("GET", "POST") are pre-allocated and reused across requests.

**Zero-copy response.** When the arena lifetime covers the entire response write, the server uses borrowed mode for both body and headers - no memcpy from JS heap to response buffer.

### Memory Management

**Hybrid arena allocation.** Request-scoped values use a bump allocator that resets in O(1) between requests. No GC pressure for ephemeral objects. Write barriers detect if a request-scoped value escapes to a long-lived object, promoting it to the GC heap.

**Generational GC.** Long-lived objects (schemas, cached data) use a two-generation collector. The nursery is a 64KB bump space; survivors promote to the tenured heap after two collections. The tenured heap uses mark-sweep with SIMD-accelerated sweeping.

**Lock-free handler pool.** Pre-allocated handler contexts use `LockFreePool` with an atomic `free_hint` that turns O(N) slot scanning into O(1) in the common case. Three-phase adaptive backoff handles contention: spin (10 iterations), sleep with jitter (10us-1ms), and circuit breaker (fail fast after 100 retries).

### Benchmarks

zts benchmarks at 63x faster than QuickJS on string operations and delivers cold starts in the 71-83ms range. The JIT compiler narrows the gap with general-purpose engines on compute-heavy workloads while maintaining predictable cold start behavior.

For detailed benchmark methodology and results, see the separate `zigttp-bench` repository.

---

## 11. Quick Reference

### CLI Options

| Flag | Default | Description |
|------|---------|-------------|
| `-p, --port` | 8080 | HTTP port |
| `-h, --host` | 127.0.0.1 | Bind address |
| `-e, --eval` | - | Inline JavaScript |
| `-m, --memory` | 0 (no limit) | JS runtime memory limit (supports k/m/g suffixes) |
| `-n, --pool` | auto (2 x CPU) | Runtime pool size |
| `-q, --quiet` | false | Disable request logging |
| `--cors` | false | Enable CORS headers |
| `--static` | - | Serve static files from directory |
| `--outbound-http` | false | Enable fetchSync |
| `--outbound-host` | - | Restrict egress to single host |
| `--outbound-timeout-ms` | - | Outbound request timeout |
| `--outbound-max-response` | - | Max outbound response bytes |
| `--sqlite` | - | SQLite database path |
| `--trace` | - | Record traces to JSONL file |
| `--replay` | - | Replay traces and verify output |
| `--test` | - | Run declarative handler tests |
| `--durable` | - | Enable durable execution with oplog directory |

### Build Options

| Flag | Description |
|------|-------------|
| `-Doptimize=ReleaseFast` | Optimized build |
| `-Dhandler=<path>` | Precompile handler into binary |
| `-Dverify` | Enable compile-time verification |
| `-Dcontract` | Emit contract.json |
| `-Ddeploy=aws` | Generate deployment manifest |
| `-Dreplay=<traces>` | Replay-verify before embedding |
| `-Dtest-file=<tests>` | Run handler tests at build time |
| `-Dgenerate-tests=true` | Generate exhaustive test cases |
| `-Dsql-schema=<path>` | SQLite schema for query validation |
| `-Dpolicy=<path>` | Capability policy JSON override |
| `-Dopenapi` | Generate OpenAPI manifest |
| `-Dsdk=ts` | Generate TypeScript SDK client |
| `-Dprove=<spec>` | Prove upgrade safety |
| `-Daot` | Enable native AOT generation |

### Standalone Tools

| Command | Description |
|---------|-------------|
| `zts prove old.json new.json [dir/]` | Compare two contracts, classify as equivalent/additive/breaking |
| `zts mock tests.jsonl [--port PORT]` | Serve mock HTTP responses from generated tests |
| `zig build bench` | Run performance benchmarks |

### Virtual Module Exports

| Module | Exports | Effect |
|--------|---------|--------|
| `zigttp:env` | `env` | read |
| `zigttp:crypto` | `sha256`, `hmacSha256`, `base64Encode`, `base64Decode` | none |
| `zigttp:router` | `routerMatch` | read |
| `zigttp:auth` | `parseBearer`, `jwtVerify`, `jwtSign`, `verifyWebhookSignature`, `timingSafeEqual` | read |
| `zigttp:validate` | `schemaCompile`, `validateJson`, `validateObject`, `coerceJson`, `schemaDrop` | read/write |
| `zigttp:decode` | `decodeJson`, `decodeForm`, `decodeQuery` | read |
| `zigttp:cache` | `cacheGet`, `cacheSet`, `cacheDelete`, `cacheIncr`, `cacheStats` | read/write |
| `zigttp:sql` | `sqlQuery`, `sqlExecute` | read/write |
| `zigttp:io` | `parallel`, `race` | read |
| `zigttp:compose` | `guard` | none |
| `zigttp:durable` | `run`, `step`, `sleep`, `sleepUntil`, `waitSignal`, `signal`, `signalAt` | write |

### Supported JavaScript Features

ES5 core plus: arrow functions, `let`/`const`, template literals, destructuring (object and array), spread operator, `for...of` (arrays), optional chaining (`?.`), nullish coalescing (`??`), typed arrays, exponentiation (`**`), `globalThis`, `match` expression, pipe operator (`|>`), compound assignments (`+=`, `-=`, etc.), `range()`, array HOFs (`.map()`, `.filter()`, `.reduce()`, `.find()`, `.findIndex()`, `.some()`, `.every()`, `.forEach()`), `Object.keys()`, `Object.values()`, `Object.entries()`, `break`/`continue` in `for-of`.

### Intentionally Unsupported

| Feature | Reason | Alternative |
|---------|--------|-------------|
| `class` | Prevents shape analysis | Plain objects and functions |
| `this` | Implicit binding is error-prone | Explicit parameters |
| `while`/`do-while` | Creates back-edges in CFG | `for...of` with finite collection |
| `try`/`catch`/`throw` | Invisible error paths | Result types |
| `async`/`await`/`Promise` | Event loop complexity | `fetchSync`, `parallel`, `race` |
| `var` | Function-scoped hoisting | `let`/`const` |
| `null` | Two absent values is one too many | `undefined` |
| `==` | Type coercion bugs | `===` |
| `++`/`--` | Mutability footgun | `+= 1` |
| Regular expressions | Backtracking DoS risk | String methods |
| `new` | Constructor complexity | Factory functions |
| `import()` dynamic | Breaks static analysis | Static imports only |

---

*zigttp: write a function, prove it correct, deploy it with confidence.*
