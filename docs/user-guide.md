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
10. [JavaScript Subset Reference](#javascript-subset-reference)
11. [TypeScript Support](#typescript-support)
12. [Complete Examples](#complete-examples)
13. [Performance Tuning](#performance-tuning)
14. [Troubleshooting](#troubleshooting)

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
                        Default: 256k
                        Supports: k/kb, m/mb, g/gb suffixes
                        Example: -m 512k, -m 1m

  -n, --pool <N>        Runtime pool size
                        Default: auto (2 * cpu count, min 8)

  -q, --quiet           Disable request logging
                        Useful for production/benchmarks

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
    path: string,        // URL path: "/api/users", "/", "/search"
    headers: object,     // HTTP headers as key-value pairs
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

    // Query string (empty string if none)
    console.log(request.query); // "id=1"

    // Headers
    console.log(request.headers["Content-Type"]); // "application/json"
    console.log(request.headers["Authorization"]); // "Bearer xxx"

    // Body (may be null for GET requests)
    if (request.body) {
        console.log(request.body); // Raw body string
    }

    return Response.text("OK");
}
```

### Common Header Access

```javascript
function handler(request) {
    let contentType = request.headers["Content-Type"] || "";
    let auth = request.headers["Authorization"] || "";
    let userAgent = request.headers["User-Agent"] || "";
    let accept = request.headers["Accept"] || "";

    return Response.json({
        contentType: contentType,
        hasAuth: auth.length > 0,
        userAgent: userAgent,
    });
}
```

---

## Response Object

### Response Helpers

#### `Response.text(body, init?)`

Create a basic response with optional configuration. (`new Response(...)` is not
supported.)

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
