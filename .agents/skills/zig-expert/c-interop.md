# Zig C Interoperability Reference

Zig provides seamless C interop with zero overhead. This reference covers FFI patterns, @cImport, and ABI compatibility.

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [@cImport and Header Translation](#cimport-and-header-translation)
3. [Calling C from Zig](#calling-c-from-zig)
4. [Exposing Zig to C](#exposing-zig-to-c)
5. [Memory and ABI](#memory-and-abi)
6. [Common Patterns](#common-patterns)

---

## Core Concepts

### Why Zig Excels at C Interop

- **No preprocessor gap** - @cImport translates C headers directly
- **ABI compatible** - Zig structs can match C layout exactly
- **Pointer types** - Zig's `[*c]T` represents C pointers faithfully
- **Null handling** - Optional pointers (`?*T`) map to nullable C pointers
- **No runtime** - Zig has no hidden runtime that conflicts with C

### C Pointer Types

| Zig Type | C Equivalent | Notes |
|----------|--------------|-------|
| `*T` | `T*` (non-null) | Zig guarantees non-null |
| `?*T` | `T*` (nullable) | Explicitly nullable |
| `[*]T` | `T*` (array) | Unknown length pointer |
| `[*c]T` | `T*` (C ptr) | C ABI compatible, nullable |
| `[]T` | N/A | Zig slice (ptr + len) |

---

## @cImport and Header Translation

### Basic Usage

```zig
const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("stdlib.h");
});

pub fn main() void {
    _ = c.printf("Hello from Zig!\n");
    
    const ptr = c.malloc(100);
    defer c.free(ptr);
}
```

### With Defines

```zig
const c = @cImport({
    @cDefine("_GNU_SOURCE", {});
    @cDefine("NDEBUG", "1");
    @cInclude("features.h");
    @cInclude("string.h");
});
```

### System Libraries

```zig
// OpenSSL
const ssl = @cImport({
    @cInclude("openssl/ssl.h");
    @cInclude("openssl/err.h");
});

// SQLite
const sqlite = @cImport({
    @cInclude("sqlite3.h");
});

// POSIX
const posix = @cImport({
    @cDefine("_POSIX_C_SOURCE", "200809L");
    @cInclude("unistd.h");
    @cInclude("sys/socket.h");
});
```

### Build System Integration

```zig
// build.zig
pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "myapp",
        .root_source_file = b.path("src/main.zig"),
    });
    
    // Link C library
    exe.linkSystemLibrary("ssl");
    exe.linkSystemLibrary("crypto");
    
    // Add include path
    exe.addIncludePath(b.path("vendor/include"));
    
    // Link static library
    exe.addObjectFile(b.path("vendor/lib/libfoo.a"));
    
    b.installArtifact(exe);
}
```

---

## Calling C from Zig

### Simple Function Calls

```zig
const c = @cImport(@cInclude("math.h"));

fn compute() f64 {
    const x: f64 = 3.14;
    return c.sin(x) + c.cos(x);
}
```

### Handling C Strings

```zig
const c = @cImport(@cInclude("string.h"));

fn zigStringToC(zig_str: []const u8) [*c]const u8 {
    // Zig slices are NOT null-terminated
    // For C, need null-terminated string
    return zig_str.ptr; // Only if you KNOW it's null-terminated
}

fn cStringToZig(c_str: [*c]const u8) []const u8 {
    if (c_str == null) return "";
    return std.mem.span(c_str); // Find null terminator
}

// Safe pattern: allocate null-terminated copy
fn toNullTerminated(allocator: std.mem.Allocator, str: []const u8) ![:0]u8 {
    return try allocator.dupeZ(u8, str);
}
```

### Handling C Arrays

```zig
const c = @cImport(@cInclude("stdlib.h"));

fn processArray() void {
    // C returns pointer, we know the length
    const ptr: [*c]c_int = c.calloc(100, @sizeOf(c_int));
    if (ptr == null) return;
    defer c.free(ptr);
    
    // Convert to Zig slice for safe access
    const slice: []c_int = ptr[0..100];
    
    for (slice) |*item| {
        item.* = 42;
    }
}
```

### Callbacks

```zig
const c = @cImport(@cInclude("stdlib.h"));

// C-compatible callback signature
fn compareInts(a: ?*const anyopaque, b: ?*const anyopaque) callconv(.C) c_int {
    const ia: *const c_int = @ptrCast(@alignCast(a));
    const ib: *const c_int = @ptrCast(@alignCast(b));
    return ia.* - ib.*;
}

fn sortArray(arr: []c_int) void {
    c.qsort(
        arr.ptr,
        arr.len,
        @sizeOf(c_int),
        compareInts,
    );
}
```

---

## Exposing Zig to C

### Export Functions

```zig
// Callable from C as: int32_t add(int32_t a, int32_t b);
export fn add(a: i32, b: i32) i32 {
    return a + b;
}

// With explicit symbol name
export fn zig_process_data(ptr: [*c]u8, len: usize) callconv(.C) c_int {
    const slice = ptr[0..len];
    // ... process
    return 0;
}
```

### Export Structs

```zig
// C-compatible struct layout
const CPoint = extern struct {
    x: f64,
    y: f64,
};

export fn create_point(x: f64, y: f64) CPoint {
    return .{ .x = x, .y = y };
}

export fn point_distance(p: *const CPoint) f64 {
    return @sqrt(p.x * p.x + p.y * p.y);
}
```

### Generate C Header

```zig
// build.zig
pub fn build(b: *std.Build) void {
    const lib = b.addStaticLibrary(.{
        .name = "mylib",
        .root_source_file = b.path("src/lib.zig"),
    });
    
    // Generate C header
    lib.installHeader(b.path("include/mylib.h"), "mylib.h");
    
    b.installArtifact(lib);
}
```

---

## Memory and ABI

### Struct Layout

```zig
// Zig-native struct (may have padding, reordering)
const ZigStruct = struct {
    a: u8,
    b: u64,
    c: u8,
};

// C-compatible struct (exact C layout)
const CStruct = extern struct {
    a: u8,
    // 7 bytes padding
    b: u64,
    c: u8,
    // 7 bytes padding
};

// Packed struct (no padding, may be slow)
const PackedStruct = packed struct {
    a: u8,
    b: u64,
    c: u8,
};

comptime {
    @compileLog(@sizeOf(ZigStruct));    // May vary
    @compileLog(@sizeOf(CStruct));      // 24 (matches C)
    @compileLog(@sizeOf(PackedStruct)); // 10
}
```

### Handling void* (anyopaque)

```zig
// C: void* userdata pattern
const Callback = *const fn (?*anyopaque) callconv(.C) void;

fn registerCallback(cb: Callback, userdata: ?*anyopaque) void {
    // Store for later invocation
    stored_cb = cb;
    stored_data = userdata;
}

// Usage with typed data
const MyData = struct {
    value: i32,
};

fn myCallback(raw: ?*anyopaque) callconv(.C) void {
    const data: *MyData = @ptrCast(@alignCast(raw));
    std.debug.print("Value: {}\n", .{data.value});
}

var my_data = MyData{ .value = 42 };
registerCallback(myCallback, &my_data);
```

### Allocator Bridge

```zig
// Wrap Zig allocator for C usage
var global_allocator: std.mem.Allocator = undefined;

export fn zig_malloc(size: usize) ?*anyopaque {
    const slice = global_allocator.alloc(u8, size) catch return null;
    return slice.ptr;
}

export fn zig_free(ptr: ?*anyopaque) void {
    if (ptr) |p| {
        // Need to track size somehow, or use different approach
        // This is simplified - real impl needs size tracking
    }
}

// Better: provide sized alloc/free
export fn zig_alloc(size: usize) ?[*]u8 {
    const slice = global_allocator.alloc(u8, size) catch return null;
    return slice.ptr;
}

export fn zig_free_sized(ptr: [*]u8, size: usize) void {
    global_allocator.free(ptr[0..size]);
}
```

---

## Common Patterns

### Wrapping C Library

```zig
const c = @cImport(@cInclude("sqlite3.h"));

const Database = struct {
    handle: *c.sqlite3,
    
    pub fn open(path: [:0]const u8) !Database {
        var db: ?*c.sqlite3 = null;
        const rc = c.sqlite3_open(path.ptr, &db);
        if (rc != c.SQLITE_OK) {
            if (db) |d| c.sqlite3_close(d);
            return error.OpenFailed;
        }
        return .{ .handle = db.? };
    }
    
    pub fn close(self: *Database) void {
        _ = c.sqlite3_close(self.handle);
    }
    
    pub fn exec(self: *Database, sql: [:0]const u8) !void {
        var err_msg: [*c]u8 = null;
        const rc = c.sqlite3_exec(self.handle, sql.ptr, null, null, &err_msg);
        if (rc != c.SQLITE_OK) {
            defer c.sqlite3_free(err_msg);
            std.log.err("SQL error: {s}", .{std.mem.span(err_msg)});
            return error.ExecFailed;
        }
    }
};
```

### Error Code Translation

```zig
const c = @cImport(@cInclude("errno.h"));

const PosixError = error{
    PermissionDenied,
    FileNotFound,
    IoError,
    Unknown,
};

fn translateErrno() PosixError {
    return switch (c.errno) {
        c.EACCES => error.PermissionDenied,
        c.ENOENT => error.FileNotFound,
        c.EIO => error.IoError,
        else => error.Unknown,
    };
}

fn posixRead(fd: c_int, buf: []u8) PosixError!usize {
    const result = c.read(fd, buf.ptr, buf.len);
    if (result < 0) return translateErrno();
    return @intCast(result);
}
```

### Opaque Type Wrapper

```zig
// For C types we don't want to expose internals
const CHandle = opaque {};

extern fn c_create_handle() ?*CHandle;
extern fn c_destroy_handle(?*CHandle) void;
extern fn c_handle_do_thing(*CHandle, c_int) c_int;

const Handle = struct {
    ptr: *CHandle,
    
    pub fn init() !Handle {
        const ptr = c_create_handle() orelse return error.CreateFailed;
        return .{ .ptr = ptr };
    }
    
    pub fn deinit(self: *Handle) void {
        c_destroy_handle(self.ptr);
    }
    
    pub fn doThing(self: *Handle, value: i32) !i32 {
        const result = c_handle_do_thing(self.ptr, value);
        if (result < 0) return error.OperationFailed;
        return result;
    }
};
```

### Variadic Functions

```zig
const c = @cImport(@cInclude("stdio.h"));

fn printFormatted(comptime fmt: [*:0]const u8, args: anytype) void {
    // Zig doesn't directly support C varargs
    // Use @call with .auto for specific arities
    
    // For printf specifically, use std.fmt instead:
    // std.debug.print(fmt, args);
    
    // Or build format string and call C:
    _ = c.printf(fmt); // No varargs from Zig
}

// Better approach: format in Zig, pass to C
fn logToC(comptime fmt: []const u8, args: anytype) void {
    var buf: [1024]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, fmt, args) catch return;
    _ = c.puts(msg.ptr);
}
```

### Thread Safety with C

```zig
const c = @cImport({
    @cInclude("pthread.h");
});

const Mutex = struct {
    inner: c.pthread_mutex_t,
    
    pub fn init() Mutex {
        var m: c.pthread_mutex_t = undefined;
        _ = c.pthread_mutex_init(&m, null);
        return .{ .inner = m };
    }
    
    pub fn deinit(self: *Mutex) void {
        _ = c.pthread_mutex_destroy(&self.inner);
    }
    
    pub fn lock(self: *Mutex) void {
        _ = c.pthread_mutex_lock(&self.inner);
    }
    
    pub fn unlock(self: *Mutex) void {
        _ = c.pthread_mutex_unlock(&self.inner);
    }
};
```

---

## Best Practices

### Do

- Use `extern struct` for C-compatible data
- Prefer `[:0]const u8` (sentinel-terminated) for strings passed to C
- Wrap C resources with Zig types that handle cleanup
- Translate C error codes to Zig errors at boundaries
- Use `@ptrCast` and `@alignCast` explicitly for void* conversions

### Don't

- Pass Zig slices directly to C (they're not pointers)
- Assume C strings are valid UTF-8
- Ignore C function return codes
- Mix Zig allocator and C malloc/free for same memory
- Use `packed struct` unless specifically needed (performance penalty)
