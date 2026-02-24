# API Reference: Zig Embedding & Advanced Configuration

This document covers advanced server configuration and extending zigttp with native Zig functions. For handler JavaScript API, CLI options, and virtual modules, see [User Guide](user-guide.md).

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

**`pool_size`** (default: auto = 2 * cpu_count, min 8) - Number of pre-allocated handler contexts. Memory scales linearly with pool size.

**`pool_wait_timeout_ms`** (default: 5) - Maximum time to wait for available handler (milliseconds). Returns 503 if timeout exceeded.

**`pool_metrics_every`** (default: 0 = disabled) - Log pool metrics every N requests. Output: `Pool metrics: in_use=2/8 exhausted=0 avg_wait_us=3 max_wait_us=20 avg_exec_us=120 max_exec_us=500`

#### Static File Cache

**`static_cache_max_bytes`** (default: 2MB) - Maximum total cache size. LRU eviction when exceeded.

**`static_cache_max_file_size`** (default: 128KB) - Files larger than this are served directly from disk.

## Extending with Native Functions

Add custom native functions callable from JavaScript by implementing the `NativeFn` signature:

### Native Function Signature

```zig
pub const NativeFn = *const fn(ctx: *Context, this: JSValue, args: []const JSValue) Error!JSValue;
```

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

Usage in JavaScript:

```javascript
function handler(request) {
    const x = square(5);  // Returns 25
    return Response.json({ result: x });
}
```

### Value Conversion

#### From JavaScript to Zig

```zig
const num: f64 = try value.toNumber(ctx);
const int: i32 = try value.toInt32(ctx);
const str: []const u8 = try value.toString(ctx);
const bool_val: bool = try value.toBoolean(ctx);
const obj: *zts.Object = try value.toObject(ctx);
```

#### From Zig to JavaScript

```zig
const num = zts.JSValue.fromInt(42);
const float = zts.JSValue.fromFloat(3.14);
const str = try zts.JSValue.fromString(ctx, "hello");
const true_val = zts.JSValue.fromBool(true);
const null_val = zts.JSValue.null();
const undef_val = zts.JSValue.undefined();

const obj = try ctx.createObject();
try obj.setProperty(ctx, "key", zts.JSValue.fromInt(123));
```

### Error Handling

```zig
fn mayFailFunction(ctx: *zts.Context, this: zts.JSValue, args: []const zts.JSValue) !zts.JSValue {
    if (args.len < 1) {
        return ctx.throwError("Missing required argument");
    }

    const value = try args[0].toNumber(ctx);

    if (value < 0) {
        return ctx.throwError("Value must be non-negative");
    }

    return zts.JSValue.fromFloat(@sqrt(value));
}
```

### Best Practices

1. **Always use errdefer**: Clean up resources on error paths
2. **Validate arguments**: Check argument count and types before conversion
3. **Use appropriate allocators**: Request-scoped allocations should use the context's arena allocator (no manual free needed)
4. **Handle null and undefined**: Check `isNullOrUndefined()` before conversion

### Build-Time Handler Precompilation

Compile handlers at build time for fastest cold starts:

```bash
zig build -Doptimize=ReleaseFast -Dhandler=examples/handler.ts
```

This embeds bytecode directly in the binary, eliminating all runtime parsing.
