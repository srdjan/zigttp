---
name: zig-expert
description: "Write idiomatic Zig code following the Zen of Zig philosophy. Use for systems programming with Zig (.zig files), covering manual memory management with allocators, error unions and explicit error handling, compile-time programming (comptime), data-oriented design, and the new async/Io model. Applies functional programming parallels (Result types, ADTs, explicit effects) for C-free systems development."
---

# Idiomatic Zig Programming

## Workflow Decision Tree

1. **Declaring a binding?** → Use `const` unless mutation required
2. **Function needs memory?** → Accept `Allocator` parameter, never global alloc
3. **Function can fail?** → Return error union `!T`, use `try` to propagate
4. **Handling an error?** → Use `catch` with explicit handler or `try` to propagate
5. **Need cleanup on exit?** → Use `defer` immediately after acquisition
6. **Cleanup only on error?** → Use `errdefer` for conditional cleanup
7. **Need generic code?** → Use `comptime` type parameters
8. **Compile-time known value?** → Use `comptime` to evaluate at build time
9. **Calling C code?** → Use `@cImport` for seamless FFI
10. **Need async I/O?** → Pass `Io` interface, use `io.async()` and `future.await()`
11. **Optimizing hot path?** → Consider data-oriented design (SoA vs AoS)

## Essential Patterns

### Error Unions

```zig
const FileError = error{ NotFound, PermissionDenied, InvalidPath };

fn readConfig(path: []const u8) FileError!Config {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return switch (err) {
            error.FileNotFound => error.NotFound,
            error.AccessDenied => error.PermissionDenied,
            else => error.InvalidPath,
        };
    };
    defer file.close();
    // ... parse config
    return config;
}

// Propagate with try
pub fn main() !void {
    const config = try readConfig("app.conf");
    // ...
}

// Handle explicitly with catch
pub fn mainSafe() void {
    const config = readConfig("app.conf") catch |err| {
        std.debug.print("Failed: {}\n", .{err});
        return;
    };
    // ...
}
```

### Allocator Pattern

```zig
const std = @import("std");

fn processData(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var result = try allocator.alloc(u8, input.len * 2);
    errdefer allocator.free(result);
    // ... process into result
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    const data = try processData(allocator, "input");
    defer allocator.free(data);
}
```

### Tagged Unions

```zig
const PaymentState = union(enum) {
    pending: void,
    processing: struct { transaction_id: []const u8 },
    completed: Receipt,
    failed: PaymentError,
    
    // Methods on the union
    pub fn describe(self: PaymentState) []const u8 {
        return switch (self) {
            .pending => "Waiting for payment",
            .processing => |p| p.transaction_id,
            .completed => |r| r.summary,
            .failed => |e| e.message,
        };
    }
};

// Exhaustive switch (compiler enforces all cases)
fn handlePayment(state: PaymentState) void {
    switch (state) {
        .pending => startProcessing(),
        .processing => |p| pollStatus(p.transaction_id),
        .completed => |receipt| sendConfirmation(receipt),
        .failed => |err| notifyFailure(err),
    }
}
```

### Compile-Time Programming

```zig
// comptime function for generics
fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}

// Compile-time computed constants
const LOOKUP_TABLE = blk: {
    var table: [256]u8 = undefined;
    for (&table, 0..) |*entry, i| {
        entry.* = @intCast((i * 7) % 256);
    }
    break :blk table;
};

// Generic container
fn ArrayList(comptime T: type) type {
    return struct {
        items: []T,
        allocator: std.mem.Allocator,
        
        const Self = @This();
        
        pub fn init(allocator: std.mem.Allocator) Self {
            return .{ .items = &[_]T{}, .allocator = allocator };
        }
        
        pub fn append(self: *Self, item: T) !void {
            // ...
        }
    };
}
```

### Resource Management with defer

```zig
fn processFile(allocator: std.mem.Allocator, path: []const u8) !void {
    // Open file
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    
    const buffer = try allocator.alloc(u8, 4096);
    defer allocator.free(buffer);
    
    var result = try allocator.alloc(u8, 1024);
    errdefer allocator.free(result);
    
    // If we reach here successfully, caller owns result
    // ...
}
```

## Build-Verify Loop

1. Write code using the patterns above
2. Run `zig build` — fix any compile errors (type mismatches, missing errdefer)
3. Run `zig build test -- --test-filter "name"` — verify correctness
4. Check GPA leak detection: `gpa.deinit()` returns `.ok` when no leaks
5. Only proceed when tests pass and no leaks detected

```zig
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
defer std.debug.assert(gpa.deinit() == .ok);
```

## Quick Reference

```zig
const std = @import("std");

// Optionals (?T)
var maybe_value: ?u32 = null;
const unwrapped = maybe_value orelse 0;
const ptr = maybe_value orelse return error.Missing;

// Slices
const slice: []const u8 = "hello";
const arr: [5]u8 = .{ 1, 2, 3, 4, 5 };
for (slice, 0..) |byte, index| { }

// Switch (exhaustive)
switch (tagged_union) {
    .variant => |captured| doSomething(captured),
    else => {},
}
```

## Detailed References

- **[references/idioms.md](references/idioms.md)** - Data-oriented design, memory patterns, testing
- **[references/async-io.md](references/async-io.md)** - New async/Io model, futures, cancellation
- **[references/c-interop.md](references/c-interop.md)** - C FFI, @cImport, ABI compatibility

## Forbidden Patterns

| ❌ Never | ✅ Instead |
|----------|-----------|
| Global allocator / hidden malloc | Pass `Allocator` explicitly |
| Exceptions / panic for errors | Return error union `!T` |
| Null pointers without type | Use optional `?*T` |
| Preprocessor macros | Use `comptime` and inline functions |
| C-style strings in Zig code | Use slices `[]const u8` |
| Ignoring errors silently | Handle with `catch` or propagate with `try` |
| `var` when `const` works | Default to `const`, mutate only when necessary |
| Hidden control flow | Make all branches explicit |
| OOP inheritance hierarchies | Use composition and tagged unions |
