# zigttp-server User Guide

A serverless JavaScript runtime for FaaS deployments (AWS Lambda, Azure
Functions, Cloudflare Workers), powered by Zig and zts.

---

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Command Line Reference](#command-line-reference)
4. [Handler API](#handler-api)
5. [Request Object](#request-object)
6. [Response Object](#response-object)
7. [Routing Patterns](#routing-patterns)
8. [Working with JSON](#working-with-json)
9. [Error Handling](#error-handling)
10. [Virtual Modules](#virtual-modules)
11. [JavaScript Subset Reference](#javascript-subset-reference)
12. [TypeScript Support](#typescript-support)
13. [Complete Examples](#complete-examples)
14. [Performance Tuning](#performance-tuning-for-faas)
15. [Compile-Time Verification](#compile-time-verification)
16. [Contract Manifest](#contract-manifest)
17. [Runtime Sandboxing](#runtime-sandboxing)
18. [Troubleshooting](#troubleshooting)

---

## Installation

### Prerequisites

- **Zig 0.16.0+** (nightly): Download from
  [ziglang.org](https://ziglang.org/download/)

### Build

```bash
# Clone the repository
git clone https://github.com/your-org/zigttp
cd zigttp

# Build release version (optimized for deployment)
zig build -Doptimize=ReleaseFast

# Or debug version
zig build

# Verify installation
./zig-out/bin/zigttp-server --help
```

### Deployment Package

The resulting binary (~500KB) has zero runtime dependencies and can be deployed
directly to FaaS platforms or container environments.

---

## Quick Start

### Hello World (Inline)

```bash
./zig-out/bin/zigttp-server -e "function handler(req) { return Response.text('Hello World!') }"
```

Test it:

```bash
curl http://localhost:8080/
# Output: Hello World!
```

### Hello World (File)

Create `hello.js`:

```javascript
function handler(request) {
    return Response.text("Hello World!");
}
```

Run:

```bash
./zig-out/bin/zigttp-server hello.js
```

### JSON API

```bash
./zig-out/bin/zigttp-server -e "function handler(req) { return Response.json({message: 'Hello', url: req.url}) }"
```

Test:

```bash
curl http://localhost:8080/api/test
# Output: {"message":"Hello","url":"/api/test"}
```

---

## Command Line Reference

```
zigttp-server [OPTIONS] <handler.js>
zigttp-server -e "<inline-code>"

OPTIONS:
  -p, --port <PORT>     Port to listen on
                        Default: 8080
                        Example: -p 3000

  -h, --host <HOST>     Host/IP to bind to
                        Default: 127.0.0.1
                        Example: -h 0.0.0.0 (all interfaces)

  -e, --eval <CODE>     Inline JavaScript handler code
                        Example: -e "function handler(r) { return Response.json({ok:true}) }"

  -m, --memory <SIZE>   JavaScript runtime memory limit
                        Default: 0 (no limit)
                        Supports: k/kb, m/mb, g/gb suffixes
                        Example: -m 512k, -m 1m

  -n, --pool <N>        Runtime pool size
                        Default: auto (2 * cpu count, min 8)

  -q, --quiet           Disable request logging
                        Useful for production/benchmarks

  --trace <FILE>        Record handler I/O traces to JSONL
                        Useful for replay and verification

  --replay <FILE>       Replay recorded traces instead of serving traffic

  --durable <DIR>       Enable durable run/step oplogs in a directory
                        Required for zigttp:durable

  --help                Show help message
```

### Examples

```bash
# Custom port
./zig-out/bin/zigttp-server -p 3000 handler.js

# Bind to all interfaces (accessible from network)
./zig-out/bin/zigttp-server -h 0.0.0.0 handler.js

# Increased memory for complex handlers
./zig-out/bin/zigttp-server -m 1m handler.js

# Quiet mode with custom port
./zig-out/bin/zigttp-server -q -p 8000 handler.js

# Record traces for replay
./zig-out/bin/zigttp-server --trace traces.jsonl handler.js

# Durable execution with persisted oplogs
./zig-out/bin/zigttp-server --durable .zigttp-durable handler.js

# Inline with all options
./zig-out/bin/zigttp-server -p 3000 -m 512k -e "function handler(r) { return Response.json({ok:true}) }"
```

---

## Handler API

Every handler file must define a `handler` function:

```javascript
function handler(request) {
    // Process request
    // Return a Response
    return Response.text("OK");
}
```

The function receives a `request` object and must return a `Response`.

---

## Request Object

The request object contains all information about the incoming HTTP request:

```javascript
{
    method: string,      // HTTP method: "GET", "POST", "PUT", "DELETE", etc.
    url: string,         // Full URL including query string
    path: string,        // URL path: "/api/users", "/", "/search"
    query: object,       // Parsed query parameters
    headers: object,     // HTTP headers as key-value pairs, plus headers.get(name)
    body: string | null  // Request body (for POST, PUT, PATCH) or null
}
```

### Accessing Request Properties

```javascript
function handler(request) {
    // Method
    console.log(request.method); // "GET", "POST", etc.

    // Full URL (including query string)
    console.log(request.url); // "/api/users?id=1"

    // Path (without query string)
    console.log(request.path); // "/api/users"

    // Parsed query parameters
    console.log(request.query.id); // 1

    // Headers
    console.log(request.headers.get("Content-Type")); // "application/json"
    console.log(request.headers.get("Authorization")); // "Bearer xxx"

    // Body (may be null for GET requests)
    if (request.body) {
        console.log(request.body); // Raw body string
    }
    console.log(request.text()); // Raw body string or ""
    // request.json() reads the same body stream, so call either text() or json()
    console.log(request.json()); // Parsed JSON or undefined

    return Response.text("OK");
}
```

Current helper semantics:
- `request.headers.get(name)` is case-insensitive and returns the last observed value for that header name, or `null`.
- `request.text()` returns the raw body string, or `""` when no body is present.
- `request.json()` returns parsed JSON, or `undefined` when the body is empty or invalid JSON.
- `request.text()` and `request.json()` are single-use body readers. After either one runs, further body reads throw. Use `request.body` if you need the raw string without consuming it.

### Common Header Access

```javascript
function handler(request) {
    let contentType = request.headers.get("Content-Type") || "";
    let auth = request.headers.get("Authorization") || "";
    let userAgent = request.headers.get("User-Agent") || "";
    let accept = request.headers.get("Accept") || "";

    return Response.json({
        contentType: contentType,
        hasAuth: auth.length > 0,
        userAgent: userAgent,
    });
}
```

---

## Response Object

Factory-style HTTP types are also available:

```javascript
const headers = Headers({ "Content-Type": "application/json" });
headers.append("X-Trace", "abc123");

const request = Request("/items?id=1", {
    method: "POST",
    headers: headers,
    body: "{\"ok\":true}",
});

const response = Response("Created", {
    status: 201,
    headers: { "X-Reply": "ok" },
});
```

`new` is not supported by zigttp's parser, so `Headers`, `Request`, and `Response`
are called as plain factory functions.

### Response Helpers

#### `Response.text(body, init?)`

Create a basic response with optional configuration. `Response(body, init?)`
creates the same response-shaped object for local composition, while
`Response.text/json/html` remain the primary direct-return helpers.

```javascript
// Simple text response
Response.text("Hello World");

// With status code
Response.text("Not Found", { status: 404 });

// With headers
Response.text("OK", {
    status: 200,
    headers: {
        "Content-Type": "text/plain",
        "X-Custom-Header": "value",
    },
});

// Empty response
Response.text("", { status: 204 });
```

#### `Response.json(data, init?)`

Create a JSON response. Automatically sets `Content-Type: application/json`.

```javascript
// Object
Response.json({ message: "Hello", count: 42 });

// Array
Response.json([1, 2, 3, 4, 5]);

// With status
Response.json({ error: "Not found" }, { status: 404 });

// With additional headers
Response.json({ data: "value" }, {
    status: 201,
    headers: { "X-Request-Id": "12345" },
});
```

#### `Response.text(text, init?)`

Create a plain text response. Sets `Content-Type: text/plain`.

```javascript
Response.text("Hello World");
Response.text("Error occurred", { status: 500 });
```

#### `Response.html(html, init?)`

Create an HTML response. Sets `Content-Type: text/html`.

```javascript
// Simple JSX component
const page = <h1>Hello World</h1>;
Response.html(renderToString(page));

// Full HTML document
const doc = (
    <html>
        <head><title>My Page</title></head>
        <body>Page content</body>
    </html>
);
Response.html(renderToString(doc));
```

**Note**: For HTML responses, prefer using JSX/TSX with `renderToString()` rather than string concatenation. See [JSX Guide](jsx-guide.md) for complete documentation.

### HTTP Status Codes

Common status codes:

| Code | Meaning               | Usage                   |
| ---- | --------------------- | ----------------------- |
| 200  | OK                    | Successful request      |
| 201  | Created               | Resource created (POST) |
| 204  | No Content            | Success with no body    |
| 301  | Moved Permanently     | Redirect                |
| 302  | Found                 | Temporary redirect      |
| 400  | Bad Request           | Invalid input           |
| 401  | Unauthorized          | Authentication required |
| 403  | Forbidden             | Access denied           |
| 404  | Not Found             | Resource doesn't exist  |
| 405  | Method Not Allowed    | Wrong HTTP method       |
| 500  | Internal Server Error | Server error            |

---

## Routing Patterns

### Simple Path Matching

```javascript
function handler(request) {
    let path = request.url;
    let method = request.method;

    // Exact match
    if (path === "/") {
        return Response.text("Home page");
    }

    if (path === "/about") {
        return Response.text("About page");
    }

    if (path === "/api/health") {
        return Response.json({ status: "ok" });
    }

    return Response.text("Not Found", { status: 404 });
}
```

### Method-Based Routing

```javascript
function handler(request) {
    let path = request.url;
    let method = request.method;

    if (path === "/api/users") {
        if (method === "GET") {
            return getUsers();
        }
        if (method === "POST") {
            return createUser(request);
        }
        return Response.text("Method Not Allowed", { status: 405 });
    }

    return Response.text("Not Found", { status: 404 });
}

function getUsers() {
    return Response.json([
        { id: 1, name: "Alice" },
        { id: 2, name: "Bob" },
    ]);
}

function createUser(request) {
    let data = JSON.parse(request.body);
    return Response.json({ id: 3, name: data.name }, { status: 201 });
}
```

### Path Parameters (Manual Extraction)

```javascript
function handler(request) {
    let path = request.url;

    // Match /api/users/:id
    if (path.indexOf("/api/users/") === 0) {
        let id = path.substring("/api/users/".length);
        return getUserById(id);
    }

    // Match /api/posts/:id/comments
    if (path.indexOf("/api/posts/") === 0 && path.indexOf("/comments") > 0) {
        let parts = path.split("/");
        let postId = parts[3]; // ['', 'api', 'posts', 'id', 'comments']
        return getComments(postId);
    }

    return Response.text("Not Found", { status: 404 });
}

function getUserById(id) {
    return Response.json({ id: id, name: "User " + id });
}

function getComments(postId) {
    return Response.json({ postId: postId, comments: [] });
}
```

### Prefix Matching

```javascript
function handler(request) {
    let path = request.url;

    // All /api/* routes
    if (path.indexOf("/api/") === 0) {
        return handleApi(request);
    }

    // All /admin/* routes
    if (path.indexOf("/admin/") === 0) {
        return handleAdmin(request);
    }

    // Static pages
    return handleStatic(request);
}

function handleApi(request) {
    let subpath = request.url.substring(4); // Remove '/api'
    return Response.json({ api: true, subpath: subpath });
}

function handleAdmin(request) {
    // Check auth header
    if (!request.headers["Authorization"]) {
        return Response.text("Unauthorized", { status: 401 });
    }
    return Response.json({ admin: true });
}

function handleStatic(request) {
    return Response.html(renderToString(<h1>Welcome</h1>));
}
```

### Router Helper Function

```javascript
// Simple router implementation
function createRouter() {
    let routes = [];

    return {
        get: function (path, handler) {
            routes.push({ method: "GET", path: path, handler: handler });
        },
        post: function (path, handler) {
            routes.push({ method: "POST", path: path, handler: handler });
        },
        put: function (path, handler) {
            routes.push({ method: "PUT", path: path, handler: handler });
        },
        delete: function (path, handler) {
            routes.push({ method: "DELETE", path: path, handler: handler });
        },
        handle: function (request) {
            for (let i = 0; i < routes.length; i++) {
                let route = routes[i];
                if (
                    route.method === request.method &&
                    route.path === request.url
                ) {
                    return route.handler(request);
                }
            }
            return Response.text("Not Found", { status: 404 });
        },
    };
}

// Usage
let router = createRouter();

router.get("/", function (req) {
    return Response.html(renderToString(<h1>Home</h1>));
});

router.get("/api/users", function (req) {
    return Response.json([{ id: 1, name: "Alice" }]);
});

router.post("/api/users", function (req) {
    let data = JSON.parse(req.body);
    return Response.json(data, { status: 201 });
});

function handler(request) {
    return router.handle(request);
}
```

### Match Expression Routing

The `match` expression provides declarative pattern matching for request dispatch. Each arm tests a pattern against the discriminant and returns a single expression.

```typescript
function handler(req: Request): Response {
    return match (req) {
        when { method: "GET", path: "/health" }:
            Response.json({ ok: true })
        when { method: "GET", path: "/version" }:
            Response.json({ version: "1.0.0" })
        when { method: "POST", path: "/echo" }:
            Response.json(req.body)
        default:
            Response.text("Not Found", { status: 404 })
    };
}
```

Match is an expression - it always produces a value. You can assign it to a variable, return it, or pass it as an argument.

**Pattern types:**
- **Object patterns**: `when { key: "value" }` - tests properties of the discriminant with strict equality
- **Literal patterns**: `when "GET"` - tests the discriminant directly
- **Wildcard**: `when _` - matches anything (equivalent to `default`)
- **default**: catch-all arm

Arms are tested top-to-bottom. The first matching arm's expression is returned. If no arm matches and there is no default, the result is `undefined`.

The `-Dverify` flag will warn about match expressions without a default arm, since they may not produce a value. The `-Dcontract` flag extracts route patterns from match arms with `method`/`path` properties.

---

## Working with JSON

### Parsing JSON Request Body

```javascript
function handler(request) {
    if (request.method !== "POST") {
        return Response.text("Method Not Allowed", { status: 405 });
    }

    // Check content type
    let contentType = request.headers["Content-Type"] || "";
    if (contentType.indexOf("application/json") === -1) {
        return Response.json(
            { error: "Content-Type must be application/json" },
            { status: 400 },
        );
    }

    // Check for body
    if (!request.body) {
        return Response.json({ error: "Request body is required" }, {
            status: 400,
        });
    }

    // Parse JSON
    try {
        let data = JSON.parse(request.body);
        return Response.json({ received: data, ok: true });
    } catch (e) {
        return Response.json({ error: "Invalid JSON: " + e.message }, {
            status: 400,
        });
    }
}
```

### Building JSON Responses

```javascript
function handler(request) {
    // Simple object
    let user = {
        id: 1,
        name: "Alice",
        email: "alice@example.com",
        active: true,
    };

    // Nested objects
    let response = {
        user: user,
        metadata: {
            timestamp: Date.now(),
            version: "1.0",
        },
    };

    // Arrays
    let list = {
        items: [
            { id: 1, name: "Item 1" },
            { id: 2, name: "Item 2" },
        ],
        total: 2,
    };

    return Response.json(response);
}
```

### JSON Validation Helper

```javascript
function validateJson(body, requiredFields) {
    if (!body) {
        return { valid: false, error: "Body is required" };
    }

    try {
        let data = JSON.parse(body);

        for (let i = 0; i < requiredFields.length; i++) {
            let field = requiredFields[i];
            if (data[field] === undefined) {
                return { valid: false, error: "Missing field: " + field };
            }
        }

        return { valid: true, data: data };
    } catch (e) {
        return { valid: false, error: "Invalid JSON" };
    }
}

function handler(request) {
    if (request.url === "/api/users" && request.method === "POST") {
        let result = validateJson(request.body, ["name", "email"]);

        if (!result.valid) {
            return Response.json({ error: result.error }, { status: 400 });
        }

        // Use result.data
        return Response.json({
            id: 1,
            name: result.data.name,
            email: result.data.email,
        }, { status: 201 });
    }

    return Response.text("Not Found", { status: 404 });
}
```

---

## Error Handling

### Try-Catch Pattern

```javascript
function handler(request) {
    try {
        return processRequest(request);
    } catch (e) {
        console.error("Error:", e.message);
        return Response.json({
            error: "Internal Server Error",
            message: e.message,
        }, { status: 500 });
    }
}

function processRequest(request) {
    if (request.url === "/api/risky") {
        // This might throw
        let data = JSON.parse(request.body);
        return Response.json(data);
    }
    return Response.text("OK");
}
```

### Error Response Helper

```javascript
function errorResponse(status, message, details) {
    return Response.json({
        error: true,
        status: status,
        message: message,
        details: details || null,
        timestamp: Date.now(),
    }, { status: status });
}

function handler(request) {
    if (!request.headers["Authorization"]) {
        return errorResponse(401, "Authentication required");
    }

    if (request.method === "POST" && !request.body) {
        return errorResponse(400, "Request body is required");
    }

    try {
        let data = JSON.parse(request.body);
        if (!data.name) {
            return errorResponse(400, "Validation failed", {
                field: "name",
                reason: "required",
            });
        }
        return Response.json({ ok: true });
    } catch (e) {
        return errorResponse(400, "Invalid JSON");
    }
}
```

---

## Virtual Modules

zigttp provides native virtual modules that run as compiled Zig code - no JS
interpretation overhead. Import them using ES6 import syntax:

```typescript
import { functionName } from "zigttp:module";
```

### zigttp:env

```typescript
import { env } from "zigttp:env";

const apiKey = env("API_KEY");        // string or undefined
const appName = env("APP_NAME");
```

### zigttp:crypto

```typescript
import { sha256, hmacSha256, base64Encode, base64Decode } from "zigttp:crypto";

const hash = sha256("hello");                   // hex-encoded SHA-256
const mac = hmacSha256("secret", "data");       // hex-encoded HMAC-SHA256
const encoded = base64Encode("hello");          // base64 string
const decoded = base64Decode(encoded);          // original string
```

### zigttp:router

```typescript
import { routerMatch } from "zigttp:router";

const routes = {
    "GET /":          function(req) { return Response.text("home"); },
    "GET /users/:id": function(req) { return Response.json({ id: req.params.id }); },
    "POST /users":    function(req) { return Response.json({ created: true }, { status: 201 }); },
};

function handler(req) {
    const match = routerMatch(routes, req);
    if (match) {
        req.params = match.params;
        return match.handler(req);
    }
    return Response.json({ error: "Not Found" }, { status: 404 });
}
```

### zigttp:auth

Authentication utilities with HS256 JWT support and webhook signature
verification.

```typescript
import { parseBearer, jwtVerify, jwtSign, verifyWebhookSignature, timingSafeEqual } from "zigttp:auth";
```

**parseBearer(header)** - Extract token from "Bearer \<token\>" header. Returns
string or undefined.

```typescript
const token = parseBearer(req.headers["authorization"]);
// "Bearer eyJ..." -> "eyJ..."
// "Basic ..." -> null
```

**jwtVerify(token, secret)** - Verify HS256 JWT. Checks `exp` and `nbf` claims
automatically. Returns `{ ok: true, value: claims }` or
`{ ok: false, error: message }`.

```typescript
const result = jwtVerify(token, "my-secret-key");
if (result.ok) {
    const userId = result.value.sub;
} else {
    // result.error: "invalid signature", "token expired", "token not yet valid"
}
```

**jwtSign(claims_json, secret)** - Create HS256 JWT from JSON claims string.
Returns the signed token string.

```typescript
const token = jwtSign(JSON.stringify({
    sub: "user-123",
    iat: Date.now() / 1000,
    exp: Date.now() / 1000 + 3600  // 1 hour
}), "my-secret-key");
```

**verifyWebhookSignature(payload, secret, signature)** - HMAC-SHA256 webhook
verification. Handles `sha256=` prefix (GitHub/Stripe style). Constant-time
comparison. Returns boolean.

```typescript
const valid = verifyWebhookSignature(req.body, "webhook-secret", req.headers["x-hub-signature-256"]);
```

**timingSafeEqual(a, b)** - Constant-time string comparison. Returns boolean.

```typescript
const match = timingSafeEqual(providedToken, expectedToken);
```

### zigttp:validate

JSON Schema validation with a compiled schema registry. Schemas persist across
requests within the same runtime pool slot.

```typescript
import { schemaCompile, validateJson, validateObject, coerceJson, schemaDrop } from "zigttp:validate";
```

**schemaCompile(name, schema_json)** - Compile and register a JSON Schema.
Returns boolean.

Supported schema keywords: `type`, `required`, `properties`, `minLength`,
`maxLength`, `minimum`, `maximum`, `enum`, `items`.

Supported types: `string`, `number`, `integer`, `boolean`, `array`, `object`,
`null`.

```typescript
schemaCompile("user", JSON.stringify({
    type: "object",
    required: ["name", "email"],
    properties: {
        name: { type: "string", minLength: 1, maxLength: 100 },
        email: { type: "string", minLength: 5 },
        age: { type: "integer", minimum: 0, maximum: 200 },
        role: { type: "string", enum: ["admin", "user", "guest"] }
    }
}));
```

**validateJson(name, json_string)** - Parse JSON and validate against a
compiled schema. Returns `{ ok: true, value: parsed }` or
`{ ok: false, errors: [...] }`.

```typescript
const result = validateJson("user", req.body);
if (!result.ok) {
    // result.errors is an array of { path, message }
    return Response.json({ errors: result.errors }, { status: 400 });
}
// result.value is the parsed object
```

**validateObject(name, value)** - Validate an existing JS value (no parsing
step).

**coerceJson(name, json_string)** - Parse JSON with type coercion before
validation. Coerces string-to-number, string-to-boolean, and number-to-string
when the schema expects a different type.

**schemaDrop(name)** - Remove a compiled schema. Returns boolean.

### zigttp:cache

In-memory key-value cache with namespace isolation, LRU eviction, and lazy TTL
expiry. Cache persists across requests within the same runtime pool slot.

```typescript
import { cacheGet, cacheSet, cacheDelete, cacheIncr, cacheStats } from "zigttp:cache";
```

**cacheGet(namespace, key)** - Retrieve a cached value. Returns string or
undefined. Expired entries are removed on access.

```typescript
const cached = cacheGet("sessions", sessionId);
```

**cacheSet(namespace, key, value, ttl?)** - Store a value. Optional TTL in
seconds (omit or 0 for no expiry). Evicts LRU entries if over capacity (default:
10,000 entries or 16MB).

```typescript
cacheSet("sessions", sessionId, JSON.stringify(userData), 3600); // 1 hour TTL
cacheSet("config", "settings", jsonString);                       // no expiry
```

**cacheDelete(namespace, key)** - Remove a cached entry. Returns boolean.

**cacheIncr(namespace, key, delta?, ttl?)** - Atomic increment. Creates entry
at 0 if missing. Default delta is 1. Returns the new value as a number.

```typescript
const count = cacheIncr("counters", "requests");       // +1
const score = cacheIncr("game", "score", 10);          // +10
const neg = cacheIncr("counters", "errors", -1);       // -1
```

**cacheStats(namespace?)** - Get cache statistics. Returns
`{ hits, misses, entries, bytes }`. Pass a namespace for per-namespace stats,
or omit for aggregate.

```typescript
const stats = cacheStats();           // aggregate
const nsStats = cacheStats("api");    // per-namespace
```

### zigttp:durable

Crash-safe, replay-safe execution for handlers that need an idempotency key.
Requires `--durable <DIR>` at runtime.

```typescript
import {
  run,
  step,
  sleep,
  sleepUntil,
  waitSignal,
  signal,
  signalAt,
} from "zigttp:durable";
```

**run(key, fn)** - Start or resume a durable execution keyed by `key`. If a
completed run already exists for that key, zigttp returns the recorded
`Response` without re-running the callback.

**step(name, fn)** - Memoize a named subcomputation inside `run()`. If the step
already completed in a previous attempt, zigttp returns the recorded result and
skips the callback body.

**sleep(ms)** / **sleepUntil(unixMs)** - Suspend a durable run until the timer
is due. Pending runs return a framework-owned `202 Accepted` response with a
JSON body describing the outstanding timer.

**waitSignal(name)** - Suspend a durable run until a named signal arrives. When
the signal is delivered, the recorded JSON payload is returned to the handler.

**signal(key, name, payload?)** - Queue an immediate signal for an incomplete
durable run. Returns `false` if `key` does not identify an incomplete run.

**signalAt(key, name, atUnixMs, payload?)** - Queue a scheduled signal that
becomes available at or after `atUnixMs`. Delivery uses the same replay-safe
path as `signal()`.

```typescript
import { run, step } from "zigttp:durable";

function handler(req) {
    const key = req.headers.get("idempotency-key") ?? req.path;

    return run(key, function() {
        const auth = step("auth", function() {
            return req.headers.get("authorization") === "secret";
        });
        if (!auth) {
            return Response.json({ error: "unauthorized" }, { status: 401 });
        }

        const charge = step("charge", function() {
            return fetchSync("https://payments.internal/charge").json();
        });

        return Response.json({ ok: true, charge: charge });
    });
}
```

Current constraints:

- `run()` callbacks must return a `Response`
- `step()` must be called inside `run()`
- `sleep()`, `sleepUntil()`, and `waitSignal()` must be called inside `run()`
- `sleep*()` and `waitSignal()` are not supported inside `step()`
- Nested `run()` and nested `step()` are not supported in v1
- `step()` results and signal payloads must be JSON-serializable
- Pending durable waits return `202 Accepted` with a JSON body of the form
  `{"pending":true,"durableKey":"...","wait":{...}}`

### zigttp:io

Structured concurrent I/O without async/await. Handler code stays synchronous
and linear; concurrency happens in the I/O layer. Requires outbound HTTP to be
enabled at build time.

```typescript
import { parallel, race } from "zigttp:io";
```

**parallel(thunks)** - Execute an array of zero-arg functions concurrently.
Each thunk typically wraps a `fetchSync` call. All HTTP requests fire in
parallel using OS threads; results are returned in declaration order regardless
of completion order.

```typescript
// Three API calls in ~50ms instead of ~150ms
const [user, orders, inventory] = parallel([
  () => fetchSync("https://users.internal/api/v1/123"),
  () => fetchSync("https://orders.internal/api/v1?user=123"),
  () => fetchSync("https://inventory.internal/api/v1/789")
]);

// Each element is a standard Response object
if (user.ok) {
  const userData = user.json();
}
```

Thunks can use computed URLs:

```typescript
const [profile, history] = parallel([
  () => fetchSync(`https://api.internal/users/${userId}`),
  () => fetchSync(`https://api.internal/history?user=${userId}&limit=10`)
]);
```

**race(thunks)** - Execute an array of zero-arg functions concurrently, return
the result of whichever completes first. Useful for failover patterns.

```typescript
const fastest = race([
  () => fetchSync("https://primary.example.com/data"),
  () => fetchSync("https://fallback.example.com/data")
]);
```

Limits: at most 8 concurrent operations per `parallel`/`race` call.

All existing guarantees are preserved:

- **Verification** (`-Dverify`): `parallel` returns a fixed-size array;
  every code path after it is linear and verifiable.
- **Contracts** (`-Dcontract`): egress hosts inside thunks are extracted.
  `parallel` is a structured grouping of effects the contract already tracks.
- **Sound mode** (`-Dsound`): return types inferred as `object` (array of
  Response objects).
- **Policies**: each constituent `fetchSync` is individually checked against
  `--outbound-host` allowlists and capability policies.
- **Determinism**: same inputs produce same output ordering. Results always
  match declaration order.

---

## JavaScript Subset Reference

zts implements ES5 with some ES6+ extensions. Here's what's available:

### Supported Features

```javascript
// Variables
let x = 1; // ✓ let keyword
const y = 2; // ✓ const keyword
var z = 3; // ✓ var keyword (function scoped)

// Functions
function foo() {} // ✓ Function declarations
let bar = function () {}; // ✓ Function expressions
let arrow = () => {}; // ✗ NOT supported

// Objects and Arrays
let obj = { a: 1, b: 2 }; // ✓ Object literals
let arr = [1, 2, 3]; // ✓ Array literals

// Loops
for (let i = 0; i < 10; i++) {} // ✓ for loop
while (condition) {} // ✓ while loop
for (let item of array) {} // ✓ for...of (arrays only)
for (let key in obj) {} // ✓ for...in (own properties only)

// Built-in Objects
JSON.parse(), JSON.stringify(); // ✓ JSON
Math.floor(), Math.random(); // ✓ Math
Array.isArray(), [].push(); // ✓ Array methods
"".split(), "".indexOf(); // ✓ String methods
Date.now(); // ✓ Date (limited)

// ES6+ Extensions
Math.imul(), Math.clz32(); // ✓ Additional Math
Math.trunc(), Math.log2(); // ✓
"".trimStart(), "".trimEnd(); // ✓ String methods
"".codePointAt(); // ✓
2 ** 10; // ✓ Exponentiation

// Match Expression
let result = match (x) {   // ✓ Pattern matching
    when "a": 1,
    when { key: "val" }: 2,
    when _: 0               // wildcard
};
```

### NOT Supported

```javascript
// ES6+ Syntax
let, const                   // Use 'let' instead
() => {}                     // Use 'function() {}' instead
`template ${literals}`       // Use string concatenation
{ ...spread }                // Use Object.assign or manual copy
class Foo {}                 // Use constructor functions
import/export                // Not in handler context
async/await                  // Synchronous only
Promise                      // Not available

// Other Limitations
eval('local code')           // Only global eval: (1, eval)('code')
new Number(1)                // No value boxing
[1, , 3]                     // No array holes
```

### Strict Mode

zts always runs in strict mode:

```javascript
// These are errors:
x = 1; // Error: must use 'let x = 1'
with (obj) {} // Error: 'with' not allowed
delete x; // Error: cannot delete letiables
```

---

## TypeScript Support

zts includes a native TypeScript/TSX stripper that removes type annotations at
load time. Use `.ts` or `.tsx` files directly without a separate build step.

### How It Works

The TypeScript stripper performs a single-pass transformation:

1. Removes type annotations (`: Type`, `as Type`)
2. Removes interface and type declarations
3. Removes generics (`<T>`)
4. Preserves all runtime code unchanged
5. Optionally evaluates `comptime()` expressions

### Basic TypeScript Handler

```typescript
// handler.ts
interface Request {
    method: string;
    path: string;
    headers: Record<string, string>;
    body: string | null;
}

interface User {
    id: number;
    name: string;
    email: string;
}

function handler(request: Request): Response {
    const users: User[] = [
        { id: 1, name: "Alice", email: "alice@example.com" },
        { id: 2, name: "Bob", email: "bob@example.com" },
    ];

    if (request.url === "/api/users") {
        return Response.json(users);
    }

    return Response.json({ error: "Not found" }, { status: 404 });
}
```

After stripping, this becomes valid ES5 JavaScript with all type annotations
removed.

### TSX for Server-Side Rendering

Combine TypeScript types with JSX syntax:

```tsx
// handler.tsx
interface PageProps {
    title: string;
    children: any;
}

function Layout(props: PageProps) {
    return (
        <html>
            <head>
                <title>{props.title}</title>
            </head>
            <body>{props.children}</body>
        </html>
    );
}

function handler(request: Request): Response {
    const page = (
        <Layout title="My App">
            <h1>Welcome</h1>
            <p>Path: {request.url}</p>
        </Layout>
    );
    return Response.html(renderToString(page));
}
```

### Compile-Time Evaluation with comptime()

The `comptime()` function evaluates expressions at load time and replaces them
with literal values. This is useful for:

- Pre-computing constants
- Embedding build metadata
- Generating hash-based ETags
- Parsing JSON configuration

#### Basic Usage

```typescript
// Arithmetic - computed at load time
const x = comptime(1 + 2 * 3); // -> const x = 7;
const bits = comptime(1 << 10); // -> const bits = 1024;

// String operations
const upper = comptime("hello".toUpperCase()); // -> const upper = "HELLO";
const parts = comptime("a,b,c".split(",")); // -> const parts = ["a","b","c"];

// Math constants and functions
const pi = comptime(Math.PI); // -> const pi = 3.141592653589793;
const max = comptime(Math.max(1, 5, 3)); // -> const max = 5;
const root = comptime(Math.sqrt(2)); // -> const root = 1.4142135623730951;

// Objects and arrays
const config = comptime({ timeout: 30 }); // -> const config = ({timeout:30});
const arr = comptime([1, 2, 3]); // -> const arr = [1,2,3];
```

#### Hash Function

Generate deterministic hashes for cache keys or ETags:

```typescript
// FNV-1a hash returns 8-character hex string
const etag = comptime(hash("content-v1")); // -> const etag = "a1b2c3d4";

function handler(request: Request): Response {
    return Response.text("Content", {
        headers: { "ETag": etag },
    });
}
```

#### JSON Parsing

Parse JSON at compile time:

```typescript
const config = comptime(JSON.parse('{"debug":false,"maxItems":100}'));
// -> const config = ({debug:false,maxItems:100});
```

#### Method Chaining

String method chaining works in comptime:

```typescript
const cleaned = comptime("  Hello World  ".trim().toUpperCase());
// -> const cleaned = "HELLO WORLD";

const slug = comptime("My Blog Post".toLowerCase().replace(" ", "-"));
// -> const slug = "my-blog-post";
```

#### comptime in TSX

Expressions inside JSX are also evaluated:

```tsx
const el = (
    <div class={comptime("container-" + hash("v1"))}>
        {comptime(Math.PI.toFixed(2))}
    </div>
);
// -> <div class="container-a1b2c3d4">3.14</div>
```

### Supported comptime Operations

#### Literals

- Numbers: `42`, `3.14`, `-1`, `0xFF`
- Strings: `"hello"`, `'world'`
- Booleans: `true`, `false`
- Special: `null`, `undefined`, `NaN`, `Infinity`

#### Operators

| Type       | Operators                 |
| ---------- | ------------------------- |
| Unary      | `+ - ! ~`                 |
| Arithmetic | `+ - * / % **`            |
| Bitwise    | `\| & ^ << >> >>>`        |
| Comparison | `== != === !== < <= > >=` |
| Logical    | `&& \|\| ??`              |
| Ternary    | `cond ? a : b`            |
| Pipe       | `a \|> f` (desugars to `f(a)`) |
| Compound   | `+= -= *= /= %= **= &= \|= ^= <<= >>= >>>=` |

#### Math Constants

- `Math.PI`, `Math.E`, `Math.LN2`, `Math.LN10`
- `Math.LOG2E`, `Math.LOG10E`, `Math.SQRT2`, `Math.SQRT1_2`

#### Math Functions

- `abs`, `floor`, `ceil`, `round`, `trunc`
- `sqrt`, `cbrt`, `pow`, `exp`, `log`, `log2`, `log10`
- `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`
- `min`, `max`, `sign`, `hypot`
- `clz32`, `imul`, `fround`

#### String Properties and Methods

- `.length`
- `toUpperCase()`, `toLowerCase()`
- `trim()`, `trimStart()`, `trimEnd()`
- `slice(start, end?)`, `substring(start, end?)`
- `includes(search)`, `startsWith(search)`, `endsWith(search)`
- `indexOf(search)`, `charAt(index)`
- `split(delimiter)`, `repeat(count)`
- `replace(search, replacement)`, `replaceAll(search, replacement)`
- `padStart(length, padStr?)`, `padEnd(length, padStr?)`

#### Array Properties

- `.length`

#### Built-in Functions

- `parseInt(str, radix?)`, `parseFloat(str)`
- `JSON.parse(str)` - parses JSON string to comptime value
- `hash(str)` - FNV-1a hash, returns 8-char hex string

### comptime Errors

Certain operations are not allowed in comptime and will produce errors:

```typescript
// These will fail at load time:
comptime(foo + 1); // Error: unknown identifier 'foo'
comptime(Date.now()); // Error: Date.now() not allowed (non-deterministic)
comptime(Math.random()); // Error: Math.random() not allowed (non-deterministic)
comptime(() => 1); // Error: function literals not allowed
comptime(x = 1); // Error: assignments not allowed
```

Error messages include line and column information for debugging.

### Performance Benefits

1. **Zero-cost types**: Type annotations are stripped with no runtime overhead
2. **Pre-computed values**: `comptime()` shifts computation from runtime to load
   time
3. **Smaller output**: Type declarations don't appear in the stripped output
4. **Single-pass**: Stripping happens in one pass, no AST construction

---

## Complete Examples

### REST API Server

```javascript
// In-memory data store
let users = [
    { id: 1, name: "Alice", email: "alice@example.com" },
    { id: 2, name: "Bob", email: "bob@example.com" },
];
let nextId = 3;

function handler(request) {
    let path = request.url;
    let method = request.method;

    // GET /api/users - List all users
    if (path === "/api/users" && method === "GET") {
        return Response.json(users);
    }

    // POST /api/users - Create user
    if (path === "/api/users" && method === "POST") {
        try {
            let data = JSON.parse(request.body);
            if (!data.name || !data.email) {
                return Response.json({ error: "name and email required" }, {
                    status: 400,
                });
            }
            let user = { id: nextId++, name: data.name, email: data.email };
            users.push(user);
            return Response.json(user, { status: 201 });
        } catch (e) {
            return Response.json({ error: "Invalid JSON" }, { status: 400 });
        }
    }

    // GET /api/users/:id - Get single user
    if (path.indexOf("/api/users/") === 0 && method === "GET") {
        let id = parseInt(path.substring("/api/users/".length), 10);
        let user = findUser(id);
        if (!user) {
            return Response.json({ error: "User not found" }, { status: 404 });
        }
        return Response.json(user);
    }

    // PUT /api/users/:id - Update user
    if (path.indexOf("/api/users/") === 0 && method === "PUT") {
        let id = parseInt(path.substring("/api/users/".length), 10);
        let user = findUser(id);
        if (!user) {
            return Response.json({ error: "User not found" }, { status: 404 });
        }
        try {
            let data = JSON.parse(request.body);
            if (data.name) user.name = data.name;
            if (data.email) user.email = data.email;
            return Response.json(user);
        } catch (e) {
            return Response.json({ error: "Invalid JSON" }, { status: 400 });
        }
    }

    // DELETE /api/users/:id - Delete user
    if (path.indexOf("/api/users/") === 0 && method === "DELETE") {
        let id = parseInt(path.substring("/api/users/".length), 10);
        let index = findUserIndex(id);
        if (index === -1) {
            return Response.json({ error: "User not found" }, { status: 404 });
        }
        users.splice(index, 1);
        return Response.text("", { status: 204 });
    }

    return Response.json({ error: "Not Found" }, { status: 404 });
}

function findUser(id) {
    for (let i = 0; i < users.length; i++) {
        if (users[i].id === id) return users[i];
    }
    return null;
}

function findUserIndex(id) {
    for (let i = 0; i < users.length; i++) {
        if (users[i].id === id) return i;
    }
    return -1;
}
```

### HTML Web Application

```jsx
function Layout(props) {
    return (
        <html lang="en">
            <head>
                <meta charset="UTF-8" />
                <meta name="viewport" content="width=device-width, initial-scale=1.0" />
                <title>{props.title} | My Site</title>
                <style>{`
                    body {
                        font-family: -apple-system, "San Francisco", "Roboto", "Segoe UI", sans-serif;
                        max-width: 800px;
                        margin: 0 auto;
                        padding: 20px;
                    }
                    nav { margin-bottom: 20px; }
                    nav a { margin-right: 15px; }
                    .success { color: green; }
                    .error { color: red; }
                `}</style>
            </head>
            <body>
                <nav>
                    <a href="/">Home</a>
                    <a href="/about">About</a>
                    <a href="/contact">Contact</a>
                </nav>
                <main>{props.children}</main>
            </body>
        </html>
    );
}

function HomePage() {
    return (
        <Layout title="Home">
            <h1>Welcome to My Site</h1>
            <p>This is a simple web application powered by zigttp-server.</p>
            <p>Built with Zig and zts for serverless deployments.</p>
        </Layout>
    );
}

function AboutPage() {
    return (
        <Layout title="About">
            <h1>About</h1>
            <p>zigttp-server is a serverless JavaScript runtime powered by zts.</p>
            <h2>Features</h2>
            <ul>
                <li>Instant cold starts</li>
                <li>Zero dependencies</li>
                <li>ES5 JavaScript with select ES6+ features</li>
            </ul>
        </Layout>
    );
}

function ContactPage() {
    return (
        <Layout title="Contact">
            <h1>Contact Us</h1>
            <form method="POST" action="/contact">
                <p>
                    <label>Name:<br /><input type="text" name="name" required /></label>
                </p>
                <p>
                    <label>Email:<br /><input type="email" name="email" required /></label>
                </p>
                <p>
                    <label>Message:<br /><textarea name="message" rows="5" required></textarea></label>
                </p>
                <p><button type="submit">Send Message</button></p>
            </form>
        </Layout>
    );
}

function ThankYouPage() {
    return (
        <Layout title="Thank You">
            <h1>Thank You!</h1>
            <p class="success">Your message has been received.</p>
            <p><a href="/">Return to Home</a></p>
        </Layout>
    );
}

function NotFoundPage() {
    return (
        <Layout title="Not Found">
            <h1>404 - Page Not Found</h1>
            <p>The page you requested does not exist.</p>
            <p><a href="/">Return to Home</a></p>
        </Layout>
    );
}

function handler(request) {
    const path = request.url;
    const method = request.method;

    if (path === "/" && method === "GET") {
        return Response.html(renderToString(<HomePage />));
    }

    if (path === "/about" && method === "GET") {
        return Response.html(renderToString(<AboutPage />));
    }

    if (path === "/contact" && method === "GET") {
        return Response.html(renderToString(<ContactPage />));
    }

    if (path === "/contact" && method === "POST") {
        // Log form submission (simplified)
        console.log("Contact form submitted:", request.body);
        return Response.html(renderToString(<ThankYouPage />));
    }

    return Response.html(renderToString(<NotFoundPage />), { status: 404 });
}
```

### Health Check / Metrics Endpoint

```javascript
let startTime = Date.now();
let requestCount = 0;

function handler(request) {
    requestCount++;

    if (request.url === "/health") {
        return Response.json({
            status: "healthy",
            timestamp: Date.now(),
        });
    }

    if (request.url === "/metrics") {
        let uptime = Date.now() - startTime;
        return Response.json({
            uptime_ms: uptime,
            uptime_seconds: Math.floor(uptime / 1000),
            total_requests: requestCount,
            runtime: "zts",
        });
    }

    if (request.url === "/ready") {
        // Readiness check - could include dependency checks
        return Response.json({ ready: true });
    }

    return Response.json({
        message: "Hello",
        request_number: requestCount,
    });
}
```

---

## Performance Tuning for FaaS

### Benchmarks

zts outperforms QuickJS in our historical benchmark runs (QuickJS is used only
as an external baseline). See `benchmarks/*.json` for raw results.

| Operation      | zts         | QuickJS    | Improvement |
| -------------- | ----------- | ---------- | ----------- |
| stringOps      | 16.3M ops/s | 258K ops/s | 63x faster  |
| objectCreate   | 8.1M ops/s  | 1.7M ops/s | 4.8x faster |
| propertyAccess | 13.2M ops/s | 3.4M ops/s | 3.9x faster |
| httpHandler    | 1.0M ops/s  | 332K ops/s | 3.1x faster |
| functionCalls  | 12.4M ops/s | 5.1M ops/s | 2.4x faster |

Run benchmarks: `./zig-out/bin/zigttp-bench`

Note: Optional instrumentation (perf), parallel compiler, and JIT modules exist
in `zts/` but are not enabled by default.

### Memory Configuration

```bash
# Default (256KB) - typical API handlers
./zig-out/bin/zigttp-server handler.js

# Larger (1MB) - complex processing, large JSON
./zig-out/bin/zigttp-server -m 1m handler.js

# Smaller (64KB) - minimal functions
./zig-out/bin/zigttp-server -m 64k handler.js
```

### Cold Start Optimization

zigttp-server is optimized for FaaS cold starts:

- Binary initialization: < 1ms
- Handler loading: typically < 5ms
- No JIT warm-up required by default

### Hybrid Arena Allocation

For request-scoped workloads, zts uses a hybrid memory model that eliminates GC
latency spikes:

- **Arena allocator**: All request-scoped objects are allocated from a
  contiguous memory region
- **O(1) reset**: Between requests, the arena is reset in constant time (no
  per-object deallocation)
- **No GC pauses**: Garbage collection is disabled during request handling
- **Escape detection**: Write barriers prevent arena objects from leaking into
  persistent storage

This design is ideal for FaaS environments where predictable latency matters
more than throughput.

### Optimize Handler Code

```javascript
// GOOD: Reuse objects across requests
let responseTemplate = { status: "ok" };

function handler(request) {
    responseTemplate.timestamp = Date.now();
    return Response.json(responseTemplate);
}

// AVOID: Creating large objects per request
function handler(request) {
    // This creates garbage every request
    let bigArray = [];
    for (let i = 0; i < 10000; i++) {
        bigArray.push({ index: i });
    }
    return Response.json(bigArray);
}
```

### Production Deployment

#### Server Options

CLI options for the standalone server:

```bash
zigttp-server -p 8080 -h 127.0.0.1 -n 8 --cors --static ./public handler.js
```

Advanced options are available through `ServerConfig` when embedding `Server`
directly in Zig:

```zig
const config = ServerConfig{
    .pool_wait_timeout_ms = 5,
    .pool_metrics_every = 1000,
    .static_cache_max_bytes = 2 * 1024 * 1024,
    .static_cache_max_file_size = 128 * 1024,
};
```

#### Standalone Server

```bash
# Quiet mode, bind to all interfaces
./zig-out/bin/zigttp-server -q -h 0.0.0.0 -p 8080 handler.js
```

#### Docker Container

```dockerfile
FROM scratch
COPY zig-out/bin/zigttp-server /zigttp-server
COPY handler.js /handler.js
EXPOSE 8080
ENTRYPOINT ["/zigttp-server", "-q", "-h", "0.0.0.0", "/handler.js"]
```

#### AWS Lambda (Custom Runtime)

```bash
# Build for Lambda
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux

# Package as Lambda deployment
zip function.zip bootstrap handler.js
aws lambda create-function --function-name my-function \
  --zip-file fileb://function.zip --runtime provided.al2 \
  --handler handler.handler --role arn:aws:iam::...
```

#### Cloudflare Workers (via Wasm)

Build with wasm32 target for edge deployment (experimental).

---

## Compile-Time Verification

zigttp can statically prove your handler is correct at build time. Add `-Dverify` to any build command:

```bash
zig build -Dhandler=handler.ts -Dverify
```

The verifier checks six properties:

1. **Exhaustive returns** - every code path through the handler returns a Response
2. **Result safety** - Result values from `jwtVerify`, `validateJson`, etc. have `.ok` checked before `.value` is accessed
3. **Unreachable code** - statements after unconditional returns are flagged (warning)
4. **Unused variables** - declared variables that are never referenced (warning, suppress with `_` prefix)
5. **Non-exhaustive match** - match expressions without a default arm (warning)
6. **Optional safety** - optional values from `env()`, `cacheGet()`, `parseBearer()`, and `routerMatch()` must be narrowed before use

This is possible because zigttp's JS subset bans most non-trivial control flow (`while`, `try/catch`). `break` and `continue` are allowed within `for-of` (forward jumps only). The IR tree is the control flow graph.

Example diagnostics:

```
verify error: not all code paths return a Response
  --> handler.ts:2:17
   |
  2 | function handler(req) {
   |                 ^
   = help: ensure every branch (if/else, switch/default) ends with a return statement
```

```
verify error: optional value used without checking for undefined
  --> handler.ts:6:14
   |
  6 |         app: appName,
   |              ^
   = help: check before use: if (val !== undefined) { ... }
           or provide a default: val ?? "fallback"
```

Optional values are narrowed by `if (val)`, `if (!val) return`, `val !== undefined`, `val ?? default`, or reassignment. Optional chaining (`val?.prop`) is safe.

See [verification.md](verification.md) for the full specification, recognized patterns, and test fixtures.

## Contract Manifest

Every precompilation automatically extracts a contract from the handler's IR,
describing what the handler does. Add `-Dcontract` to also emit the contract as
a `contract.json` file:

```bash
zig build -Dhandler=handler.ts -Dcontract
```

The contract extracts from the handler's IR:

- Which `zigttp:*` virtual modules are imported and which functions are used
- Literal env var names from `env("NAME")` calls
- Outbound hosts from `fetchSync("https://...")` URL arguments
- Cache namespace strings from `cacheGet`/`cacheSet`/etc.
- Durable run keys, whether durable keys are dynamic, and literal `step()` names
- Verification results (when combined with `-Dverify`)

Non-literal arguments (e.g., `env(someVariable)`) set `"dynamic": true` as an
honest signal that static analysis cannot enumerate all values.

```bash
# Combine verification and contract
zig build -Dhandler=handler.ts -Dverify -Dcontract
```

The contract is written to `src/generated/contract.json` alongside the embedded
bytecode.

## OpenAPI Manifest

Add `-Dopenapi` to emit a compiler-derived `openapi.json` alongside the
embedded bytecode:

```bash
zig build -Dhandler=handler.ts -Dopenapi
```

The current emitter only includes facts the compiler can prove:

- `schemaCompile("name", JSON.stringify({...}))` schemas become component schemas
- `validateJson("name", ...)` and `coerceJson("name", ...)` become request schema refs
- `parseBearer()` / `jwtVerify()` enable bearer auth metadata
- `routerMatch()` route tables with literal `"METHOD /path"` keys become OpenAPI paths

Dynamic schemas or routes are preserved as `x-zigttp-*` hints instead of guessed
OpenAPI operations. The manifest is written to `src/generated/openapi.json`.

## Runtime Sandboxing

Every precompiled handler is automatically sandboxed based on its contract. No
configuration required. The compiler derives a `RuntimePolicy` from the contract
and embeds it in the generated code.

### How It Works

The contract records whether each capability section (env, egress, cache) uses
only literal string arguments or includes dynamic (computed) access:

- **Static access** (`dynamic: false`): the compiler proved all calls use string
  literals. The sandbox restricts to exactly those values. Any runtime access to
  an unlisted value throws a `CapabilityPolicyError`.
- **Dynamic access** (`dynamic: true`): some calls use computed arguments. That
  section remains unrestricted because the compiler cannot enumerate all possible
  values.

The build prints a sandbox report:

```
Sandbox: complete (all access statically proven)
  env: restricted to [API_KEY, DATABASE_URL] (2 proven, no dynamic access)
  egress: restricted to [api.stripe.com] (1 proven, no dynamic access)
  cache: restricted to [sessions] (1 proven, no dynamic access)
```

Or for partial proof:

```
Sandbox derived from contract:
  env: restricted to [API_KEY] (1 proven, no dynamic access)
  egress: unrestricted (dynamic access detected)
  cache: restricted to [] (none proven, no dynamic access)
```

### Explicit Policy Override

Add `-Dpolicy=policy.json` to override auto-derived sandboxing with an explicit
least-privilege capability policy:

```bash
zig build -Dhandler=handler.ts -Dpolicy=policy.json
```

```json
{
  "env": { "allow": ["JWT_SECRET"] },
  "egress": { "allow_hosts": ["api.example.com"] },
  "cache": { "allow_namespaces": ["sessions"] }
}
```

Explicit policy rules:

- Omit a section to leave that capability unrestricted.
- If a section is present, only the listed literals are allowed.
- Dynamic access in a restricted category fails the build because zigttp cannot
  enumerate it soundly.
- Local file imports are aggregated before validation, so helper modules count
  toward the same policy.

### Non-Precompiled Handlers

Handlers run via `zig build run --` (dev mode) are not sandboxed. Sandboxing
requires precompilation (`-Dhandler=...`) because contract extraction runs as
part of the compile pipeline.

---

## Troubleshooting

### Common Errors

**"No handler specified"**

```bash
# Wrong:
./zig-out/bin/zigttp-server

# Right:
./zig-out/bin/zigttp-server handler.js
# or
./zig-out/bin/zigttp-server -e "function handler(r) { return Response.text('OK') }"
```

**"No 'handler' function defined"**

```javascript
// Wrong: missing handler function
console.log("Hello");

// Right: must define handler
function handler(request) {
    return Response.text("Hello");
}
```

**"SyntaxError" in handler**

```javascript
// Wrong: ES6 syntax not supported
const x = 1;
let y = 2;
const fn = () => x + y;

// Right: ES5 syntax
let x = 1;
let y = 2;
let fn = function () {
    return x + y;
};
```

**JSON parse errors**

```javascript
// Always wrap JSON.parse in try-catch
function handler(request) {
    try {
        let data = JSON.parse(request.body);
        return Response.json(data);
    } catch (e) {
        return Response.json({ error: "Invalid JSON" }, { status: 400 });
    }
}
```

### Debugging

```javascript
// Console methods for debugging
// console.log(value)   - stdout
// console.error(value) - stderr
// console.warn(value)  - stderr
// console.info(value)  - stdout
// console.debug(value) - stdout

function handler(request) {
    console.log("Method:", request.method);
    console.log("Path:", request.url);
    console.log("Headers:", JSON.stringify(request.headers));
    console.debug("Body:", request.body);

    return Response.text("OK");
}
```

### Memory Issues

If you see out-of-memory errors:

1. Increase memory limit: `-m 512k` or `-m 1m`
2. Reduce object creation in hot paths
3. Avoid storing large amounts of data in letiables

---

## Quick Reference Card

```
┌─────────────────────────────────────────────────────────────────┐
│                    zigttp-server Quick Reference                  │
├─────────────────────────────────────────────────────────────────┤
│ START SERVER                                                    │
│   zigttp-server handler.js                                        │
│   zigttp-server -p 3000 -e "function handler(r) {...}"           │
├─────────────────────────────────────────────────────────────────┤
│ REQUEST OBJECT                                                  │
│   request.method   → "GET", "POST", "PUT", "DELETE"            │
│   request.url     → "/api/users"                              │
│   request.headers  → { "Content-Type": "..." }                 │
│   request.body     → "..." or null                             │
├─────────────────────────────────────────────────────────────────┤
│ RESPONSE HELPERS                                                │
│   Response.json({ data })        → application/json            │
│   Response.text("string")        → text/plain                  │
│   Response.html("<html>")        → text/html                   │
│   Response.text(body, { status: 404, headers: {} })             │
├─────────────────────────────────────────────────────────────────┤
│ COMMON PATTERNS                                                 │
│   let data = JSON.parse(request.body);                         │
│   return Response.json({ error: "msg" }, { status: 400 });     │
│   if (request.url.indexOf('/api/') === 0) { ... }             │
├─────────────────────────────────────────────────────────────────┤
│ REMEMBER                                                        │
│   • Use 'let' not 'let/const'                                  │
│   • Use 'function(){}' not arrow functions                     │
│   • Always try-catch JSON.parse                                │
│   • Handler must return a Response                             │
└─────────────────────────────────────────────────────────────────┘
```
