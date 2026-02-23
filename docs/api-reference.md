# API Reference

This document covers advanced server configuration and extending zigttp with native functions.

## Handler API

Every handler file must define a `handler` function:

```javascript
function handler(request) {
    // Process request
    // Return a Response
    return Response.text("OK");
}
```

### Request Object

The request object contains all information about the incoming HTTP request:

```javascript
{
    method: string,      // "GET", "POST", etc.
    url: string,        // Full URL path (e.g., "/api/users?id=1")
    path: string,       // URL path without query string (e.g., "/api/users")
    query: string,      // Query string (e.g., "id=1"), empty string if none
    headers: object,    // HTTP headers
    body: string|null   // Request body (for POST/PUT)
}
```

See [User Guide](user-guide.md#request-object) for detailed request handling examples.

### Response Helpers

```javascript
// JSON response (sets Content-Type automatically)
Response.json(data, init?)

// Text response
Response.text(text, init?)

// HTML response
Response.html(html, init?)

// Redirect response (default status: 302)
Response.redirect(url, status?)
```

**Response init object**:

```javascript
{
    status: number,        // HTTP status code (default: 200)
    headers: object,       // Additional headers
}
```

**Example**:

```javascript
Response.json({ error: "Not found" }, {
    status: 404,
    headers: { "X-Request-Id": "12345" }
});
```

## CLI Options

```bash
zigttp-server [options] <handler.js>
zigttp-server -e "<inline-code>"

Options:
  -p, --port <PORT>     Port to listen on (default: 8080)
  -h, --host <HOST>     Host to bind to (default: 127.0.0.1)
  -e, --eval <CODE>     Evaluate inline JavaScript handler
  -m, --memory <SIZE>   JS runtime memory limit (default: 256k)
                        Supports: k/kb, m/mb, g/gb suffixes
  -n, --pool <N>        Runtime pool size (default: auto = 2 * cpu, min 8)
  -q, --quiet           Disable request logging
  --cors                Enable CORS headers
  --static <DIR>        Serve static files from directory
  --outbound-http       Enable native outbound bridge (httpRequest)
  --outbound-host <H>   Restrict outbound bridge to exact host H
  --outbound-timeout-ms Connect timeout for outbound bridge (ms)
  --outbound-max-response <SIZE>
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

# Enable CORS
./zig-out/bin/zigttp-server --cors handler.js

# Serve static files
./zig-out/bin/zigttp-server --static ./public handler.js
```

## Virtual Modules

zigttp provides native virtual modules imported via `import { ... } from "zigttp:*"` syntax. These execute as native Zig code with zero JS interpretation overhead.

### Available Modules

| Module | Exports | Description |
|--------|---------|-------------|
| `zigttp:env` | `env` | Environment variable access |
| `zigttp:crypto` | `sha256`, `hmacSha256`, `base64Encode`, `base64Decode` | Cryptographic functions |
| `zigttp:router` | `routerMatch` | Pattern-matching HTTP router |
| `zigttp:auth` | `parseBearer`, `jwtVerify`, `jwtSign`, `verifyWebhookSignature`, `timingSafeEqual` | JWT auth and webhook verification |
| `zigttp:validate` | `schemaCompile`, `validateJson`, `validateObject`, `coerceJson`, `schemaDrop` | JSON Schema validation |
| `zigttp:cache` | `cacheGet`, `cacheSet`, `cacheDelete`, `cacheIncr`, `cacheStats` | In-memory key-value cache with TTL and LRU |

### zigttp:auth

**`parseBearer(headerValue)`** - Extracts the token from a `Bearer <token>` authorization header. Returns the token string or `null` if the header is missing or malformed.

**`jwtVerify(token, secret)`** - Verifies an HS256 JWT. Returns `{ok: true, value: <claims>}` on success or `{ok: false, error: "<reason>"}` on failure. Automatically checks `exp` and `nbf` claims when present.

**`jwtSign(claimsJson, secret)`** - Signs a JSON claims string as an HS256 JWT. Returns the signed token string.

**`verifyWebhookSignature(payload, secret, signature)`** - Verifies an HMAC-SHA256 webhook signature. The signature may optionally have a `sha256=` prefix. Returns `true` or `false`.

**`timingSafeEqual(a, b)`** - Constant-time string comparison. Returns `true` if strings are identical, `false` otherwise.

### zigttp:validate

**`schemaCompile(name, schemaJson)`** - Compiles a JSON Schema string into an internal representation and registers it under the given name. Supported keywords: `type`, `required`, `properties`, `minLength`, `maxLength`, `minimum`, `maximum`, `enum`, `items`. Returns `true` on success.

**`validateJson(name, jsonString)`** - Parses a JSON string and validates it against a compiled schema. Returns `{ok: true, value: <parsed>}` or `{ok: false, errors: [{path, message}]}`.

**`validateObject(name, object)`** - Validates an existing JS object against a compiled schema. Same return shape as `validateJson`.

**`coerceJson(name, jsonString)`** - Like `validateJson` but applies type coercion before validation (string-to-number, string-to-boolean, number-to-string).

**`schemaDrop(name)`** - Removes a compiled schema from the registry. Returns `true` if the schema existed.

### zigttp:cache

**`cacheGet(namespace, key)`** - Retrieves a cached value. Returns the string value or `null` if not found or expired.

**`cacheSet(namespace, key, value, ttl?)`** - Stores a string value with optional TTL in seconds. LRU eviction occurs when limits are exceeded (default: 10,000 entries, 16MB). Returns `true`.

**`cacheDelete(namespace, key)`** - Removes an entry from the cache. Returns `true` if the entry existed.

**`cacheIncr(namespace, key, delta?, ttl?)`** - Atomically increments a numeric counter. Creates the key with value 0 if it does not exist. Default delta is 1. Returns the new value as a number.

**`cacheStats(namespace?)`** - Returns cache statistics: `{hits, misses, entries, bytes}`. If namespace is omitted, returns aggregate stats.

See [User Guide](user-guide.md#virtual-modules) for complete usage examples.

## Console Methods

```javascript
console.log(value)    // Print to stdout
console.error(value)  // Print to stderr
console.warn(value)   // Print to stderr
console.info(value)   // Print to stdout
console.debug(value)  // Print to stdout
```

### Native outbound bridge

```javascript
const raw = httpRequest(JSON.stringify({
  url: "http://127.0.0.1:8787/v1/ops?view=state",
  method: "GET",
  headers: { Authorization: "Bearer ..." }
}));
const resp = JSON.parse(raw);
```

`httpRequest` returns JSON:
- success: `{ ok: true, status, reason, body, content_type? }`
- failure: `{ ok: false, error, details }`

## Advanced Server Configuration (Zig API)

If you embed `Server` directly in Zig code, these `ServerConfig` fields tune performance features:

```zig
const std = @import("std");
const Server = @import("server.zig").Server;
const ServerConfig = @import("server.zig").ServerConfig;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const config = ServerConfig{
        // Pool configuration
        .pool_size = 16,                        // Handler pool size (default: auto)
        .pool_wait_timeout_ms = 5,              // Max wait for available handler (ms)
        .pool_metrics_every = 1000,             // Log metrics every N requests (0 = disabled)

        // Static file cache configuration
        .static_cache_max_bytes = 2 * 1024 * 1024,    // Max total cache size (default: 2MB)
        .static_cache_max_file_size = 128 * 1024,      // Max individual file size (default: 128KB)

        // Memory configuration
        .js_memory_limit = 512 * 1024,          // JS runtime memory limit (default: 256KB)

        // Network configuration
        .port = 8080,
        .host = "127.0.0.1",

        // Logging
        .quiet = false,                         // Disable request logging

        // Features
        .cors = false,                          // Enable CORS headers
        .static_dir = null,                     // Static file directory
    };

    var server = try Server.init(allocator, config);
    defer server.deinit();

    try server.loadHandler("handler.js");
    try server.listen();
}
```

### Configuration Fields

#### Pool Configuration

**`pool_size`** (default: auto = 2 * cpu_count, min 8)
- Number of pre-allocated handler contexts
- Higher values support more concurrent requests
- Memory usage scales linearly with pool size

**`pool_wait_timeout_ms`** (default: 5)
- Maximum time to wait for available handler (milliseconds)
- Prevents indefinite blocking on pool exhaustion
- Returns 503 Service Unavailable if timeout exceeded

**`pool_metrics_every`** (default: 0 = disabled)
- Log pool metrics every N requests
- Useful for monitoring pool utilization
- Output format: `Pool metrics: in_use=2/8 exhausted=0 avg_wait_us=3 max_wait_us=20`

#### Static File Cache

**`static_cache_max_bytes`** (default: 2MB)
- Maximum total size of static file cache
- LRU eviction when limit exceeded
- Set to 0 to disable caching

**`static_cache_max_file_size`** (default: 128KB)
- Maximum individual file size to cache
- Files larger than this are served directly from disk
- Prevents cache pollution from large assets

#### Memory Configuration

**`js_memory_limit`** (default: 256KB)
- Per-context JavaScript heap size
- Includes all JS objects, strings, arrays
- Affects GC frequency and memory usage

#### Network Configuration

**`port`** (default: 8080)
- TCP port to listen on

**`host`** (default: "127.0.0.1")
- Host/IP to bind to
- Use "0.0.0.0" to bind to all interfaces

#### Features

**`quiet`** (default: false)
- Disable request logging
- Useful for production/benchmarks

**`cors`** (default: false)
- Enable CORS headers on all responses
- Adds Access-Control-Allow-Origin: *

**`static_dir`** (default: null)
- Directory for static file serving
- Files served from this directory before handler

### Pool Metrics

When `pool_metrics_every` is set, logs include:

```
Pool metrics: in_use=2/8 exhausted=0 avg_wait_us=3 max_wait_us=20 avg_exec_us=120 max_exec_us=500
```

**Fields**:
- `in_use`: Current in-use count / total pool size
- `exhausted`: Number of times pool was fully utilized
- `avg_wait_us`/`max_wait_us`: Pool acquisition time (microseconds)
- `avg_exec_us`/`max_exec_us`: Handler execution time (microseconds)

## Extending with Native Functions

You can add custom native functions callable from JavaScript by implementing the `NativeFn` signature in `zts/object.zig`:

### Native Function Signature

```zig
pub const NativeFn = *const fn(ctx: *Context, this: JSValue, args: []const JSValue) Error!JSValue;
```

**Parameters**:
- `ctx`: JavaScript execution context
- `this`: The `this` value for the function call
- `args`: Array of argument values

**Returns**: `JSValue` or error

### Example: Custom Math Function

```zig
const zts = @import("zts");

fn mySquare(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    if (args.len < 1) {
        return zts.JSValue.fromInt(0);
    }

    const num = try args[0].toNumber(ctx);
    const result = num * num;

    return zts.JSValue.fromFloat(result);
}

pub fn registerCustomFunctions(ctx: *zts.Context) !void {
    const fn_value = try ctx.createNativeFunction("square", mySquare);
    try ctx.setGlobal("square", fn_value);
}
```

**Usage in JavaScript**:

```javascript
function handler(request) {
    const x = square(5);  // Returns 25
    return Response.json({ result: x });
}
```

### Example: String Manipulation

```zig
fn reverseString(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    if (args.len < 1) {
        return zts.JSValue.fromString(ctx, "");
    }

    const str = try args[0].toString(ctx);
    const allocator = ctx.allocator;

    // Allocate reversed string
    const reversed = try allocator.alloc(u8, str.len);
    errdefer allocator.free(reversed);

    var i: usize = 0;
    while (i < str.len) : (i += 1) {
        reversed[i] = str[str.len - 1 - i];
    }

    return try zts.JSValue.fromString(ctx, reversed);
}
```

### Example: Async I/O (File Read)

```zig
fn readFile(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    if (args.len < 1) {
        return error.InvalidArgument;
    }

    const path = try args[0].toString(ctx);
    const allocator = ctx.allocator;

    // Read file contents
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const contents = try file.readToEndAlloc(allocator, 1024 * 1024); // Max 1MB
    errdefer allocator.free(contents);

    return try zts.JSValue.fromString(ctx, contents);
}
```

### Value Conversion

#### From JavaScript to Zig

```zig
// Numbers
const num: f64 = try value.toNumber(ctx);
const int: i32 = try value.toInt32(ctx);

// Strings
const str: []const u8 = try value.toString(ctx);

// Booleans
const bool_val: bool = try value.toBoolean(ctx);

// Objects
const obj: *zts.Object = try value.toObject(ctx);

// Arrays
if (value.isArray(ctx)) {
    const len = try value.getProperty(ctx, "length");
    // ... iterate
}
```

#### From Zig to JavaScript

```zig
// Numbers
const num = zts.JSValue.fromInt(42);
const float = zts.JSValue.fromFloat(3.14);

// Strings
const str = try zts.JSValue.fromString(ctx, "hello");

// Booleans
const true_val = zts.JSValue.fromBool(true);

// null/undefined
const null_val = zts.JSValue.null();
const undef_val = zts.JSValue.undefined();

// Objects
const obj = try ctx.createObject();
try obj.setProperty(ctx, "key", zts.JSValue.fromInt(123));

// Arrays
const arr = try ctx.createArray();
try arr.setProperty(ctx, "0", zts.JSValue.fromInt(1));
try arr.setProperty(ctx, "1", zts.JSValue.fromInt(2));
```

### Error Handling

```zig
fn mayFailFunction(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    if (args.len < 1) {
        // Throw JavaScript error
        return ctx.throwError("Missing required argument");
    }

    const value = try args[0].toNumber(ctx);

    if (value < 0) {
        return ctx.throwError("Value must be non-negative");
    }

    return zts.JSValue.fromFloat(@sqrt(value));
}
```

### Registering Multiple Functions

```zig
pub fn registerMathExtensions(ctx: *zts.Context) !void {
    const functions = .{
        .{ "square", mySquare },
        .{ "cube", myCube },
        .{ "factorial", myFactorial },
    };

    inline for (functions) |func| {
        const fn_value = try ctx.createNativeFunction(func[0], func[1]);
        try ctx.setGlobal(func[0], fn_value);
    }
}
```

### Creating Native Objects

```zig
fn createPoint(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    const x = if (args.len > 0) try args[0].toNumber(ctx) else 0;
    const y = if (args.len > 1) try args[1].toNumber(ctx) else 0;

    const obj = try ctx.createObject();
    try obj.setProperty(ctx, "x", zts.JSValue.fromFloat(x));
    try obj.setProperty(ctx, "y", zts.JSValue.fromFloat(y));

    return obj.toValue();
}
```

**Usage**:

```javascript
const p = createPoint(10, 20);
console.log(p.x, p.y);  // 10 20
```

### Best Practices

1. **Always use errdefer**: Clean up resources on error paths

```zig
fn allocateResource(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    const buffer = try allocator.alloc(u8, 1024);
    errdefer allocator.free(buffer);  // Clean up on error

    // ... use buffer

    return result;
}
```

2. **Validate arguments**: Check argument count and types

```zig
fn safeFunction(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    if (args.len < 2) {
        return ctx.throwError("Requires 2 arguments");
    }

    if (!args[0].isNumber()) {
        return ctx.throwError("First argument must be a number");
    }

    // ... safe to use args
}
```

3. **Use appropriate allocators**: Request-scoped allocations should use arena allocator

```zig
fn createTempData(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    // Use context allocator for request-scoped data
    const temp = try ctx.allocator.alloc(u8, 100);
    // No manual free needed - arena cleans up after request

    return result;
}
```

4. **Handle null and undefined**: Check for null/undefined before conversion

```zig
fn safeToString(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    if (args.len < 1 or args[0].isNullOrUndefined()) {
        return zts.JSValue.fromString(ctx, "");
    }

    const str = try args[0].toString(ctx);
    return zts.JSValue.fromString(ctx, str);
}
```

## Examples

See `zts/builtins.zig` for comprehensive examples of native function implementations, including:

- Array methods (push, pop, shift, unshift, slice, splice)
- String methods (split, replace, slice, substring)
- Object methods (keys, values, assign)
- Math functions (floor, ceil, round, sqrt, sin, cos)
- JSON parsing and stringification
- Console methods (log, error, warn, info, debug)

## Build Configuration

### Build-Time Handler Precompilation

Compile handlers at build time for fastest cold starts:

```bash
zig build -Doptimize=ReleaseFast -Dhandler=examples/handler.ts
```

This embeds bytecode directly in the binary, eliminating all runtime parsing.

### Cross-Compilation

Build for different targets:

```bash
# Linux x86-64
zig build -Dtarget=x86_64-linux

# Linux ARM64
zig build -Dtarget=aarch64-linux

# macOS x86-64
zig build -Dtarget=x86_64-macos

# macOS ARM64 (Apple Silicon)
zig build -Dtarget=aarch64-macos
```

### Custom Build Script

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "my-server",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    exe.addIncludePath(.{ .path = "zts" });
    exe.linkLibC();

    b.installArtifact(exe);
}
```
