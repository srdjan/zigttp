---
name: zig-expert
description: Write idiomatic Zig code following the Zen of Zig philosophy. Use for systems programming with Zig (.zig files), covering manual memory management with allocators, error unions and explicit error handling, compile-time programming (comptime), data-oriented design, and the new async/Io model. Applies functional programming parallels (Result types, ADTs, explicit effects) for C-free systems development.
---

# Idiomatic Zig Programming

Expert guidance for writing idiomatic Zig code that embodies the Zen of Zig: explicit intent, no hidden control flow, and compile-time over runtime.

> Prefer Zig 0.16.0 idioms: `std.Io` with explicit `io`, `main(init: std.process.Init|Minimal)` for args/env, destructuring, labeled switch, `@branchHint` on hot paths, `.empty` for std containers, `DebugAllocator` in debug builds, and explicit backing integers for packed types.

## Zen of Zig

| Principle | Implication |
|-----------|-------------|
| Communicate intent precisely | APIs should make ownership, failure, and allocation obvious |
| Edge cases matter | Handle invalid states explicitly rather than hoping they never happen |
| Favor reading over writing | Prefer straightforward code over abstraction tricks |
| Compile errors > runtime crashes | Push invariants into types and comptime checks |
| Runtime crashes > silent bugs | Fail loudly rather than corrupting state |
| Memory is a resource | Accept allocators explicitly; define ownership clearly |
| No hidden control flow | No exceptions, GC, implicit allocations, or magical globals |

## Workflow Decision Tree

1. **Need mutation?** Use `var`; otherwise use `const`.
2. **Need allocation?** Accept `std.mem.Allocator`; never hide allocation behind globals.
3. **Can fail?** Return `!T` and propagate with `try`.
4. **Need cleanup?** Place `defer` or `errdefer` immediately after acquisition.
5. **Need OS interaction?** Pass `std.Io` explicitly; keep side effects at the boundary.
6. **Need environment variables or args?** Read them in `main(init)` and pass explicit config downward.
7. **Need generic code?** Use `comptime` parameters and exhaustive `switch`.
8. **Need C interop?** `@cImport` still works, but prefer build-system-managed translate-c for new code when practical.
9. **Need hot-path tuning?** Measure first, then use layout/data changes and targeted `@branchHint`.

## Essential Patterns

### Error Unions

```zig
const std = @import("std");

const FileError = error{ NotFound, PermissionDenied, InvalidPath };

fn readConfig(io: std.Io, path: []const u8) FileError!Config {
    const file = std.Io.Dir.cwd().openFile(io, path, .{}) catch |err| {
        return switch (err) {
            error.FileNotFound => error.NotFound,
            error.AccessDenied => error.PermissionDenied,
            else => error.InvalidPath,
        };
    };
    defer file.close(io);

    // ... parse config
    return config;
}
```

### Allocator Ownership

```zig
const std = @import("std");

fn processData(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    const result = try allocator.alloc(u8, input.len * 2);
    errdefer allocator.free(result);

    // ... write into result

    return result;
}

pub fn main() !void {
    var debug_alloc: std.heap.DebugAllocator(.{}) = .init;
    defer _ = debug_alloc.deinit();
    const allocator = debug_alloc.allocator();

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

    pub fn describe(self: PaymentState) []const u8 {
        return switch (self) {
            .pending => "Waiting for payment",
            .processing => |p| p.transaction_id,
            .completed => |r| r.summary,
            .failed => |e| e.message,
        };
    }
};
```

### Resource Management with `std.Io`

```zig
const std = @import("std");

fn processFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8) !void {
    const file = try std.Io.Dir.cwd().openFile(io, path, .{});
    defer file.close(io);

    const buffer = try allocator.alloc(u8, 4096);
    defer allocator.free(buffer);

    // ... read/write using file.reader(io, ...) or file.writer(io, ...)
}
```

## Quick Reference

```zig
const std = @import("std");

// const by default
const answer: u32 = 42;
var counter: u32 = 0;

// optionals and error unions
const maybe_value: ?u32 = null;
const value = maybe_value orelse 0;

fn canFail() !u32 {
    return error.SomeError;
}
const n = try canFail();

// destructuring (0.16+)
const sum, const overflow = @addWithOverflow(a, b);

// std containers prefer .empty
var list: std.ArrayListUnmanaged(u8) = .empty;
var map: std.AutoHashMapUnmanaged(u32, u32) = .empty;

// type info uses quoted identifiers
switch (@typeInfo(T)) {
    .@"struct" => |s| _ = s,
    .@"int" => |i| _ = i,
    else => {},
}

// std.Io at the boundary
pub fn main(init: std.process.Init.Minimal) !void {
    var debug_alloc: std.heap.DebugAllocator(.{}) = .init;
    defer _ = debug_alloc.deinit();
    const allocator = debug_alloc.allocator();

    const io = std.testing.io; // use a real Io backend in non-test code
    _ = io;
    _ = allocator;
    _ = init;
}
```

## Zig 0.16.0 Notes

### `std.Io` Is the Default

- Prefer `std.Io` APIs for filesystem, networking, time, process, and synchronization boundaries.
- `Io.Threaded` is the stable default implementation.
- `Io.Evented` remains experimental; do not assume feature parity, especially around networking.

### Args and Environment Belong to `main(init)`

- Read environment variables and CLI args in `main(init: std.process.Init|Minimal)`.
- Pass explicit config structs, individual values, or `*const std.process.Environ.Map` to lower layers.
- Avoid new code that reaches for process-global environment state when explicit plumbing is possible.

```zig
const std = @import("std");

pub fn main(init: std.process.Init.Minimal) !void {
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena.deinit();

    const port = init.environ.getPosix("PORT") orelse "8080";
    var args = init.args.iterate();
    _ = args.next(); // argv0

    try run(port);
}
```

### New Type Construction Builtins

`@Type` has been replaced by specific builtins:

- `@EnumLiteral()`
- `@Int(...)`
- `@Tuple(...)`
- `@Pointer(...)`
- `@Fn(...)`
- `@Struct(...)`
- `@Union(...)`
- `@Enum(...)`

There is no `@Array` builtin in 0.16.0; use normal array syntax. There is no `@Opaque`; write `opaque {}` directly.

### Packed Types Need Explicit Backing Integers

```zig
const Flags = packed struct(u8) {
    writable: bool = true,
    enumerable: bool = true,
    _reserved: u6 = 0,
};
```

Use the same explicitness for packed unions when representation matters.

### Allocator Defaults

- `GeneralPurposeAllocator` is gone.
- Prefer `DebugAllocator` in debug-heavy development workflows.
- Prefer `std.heap.smp_allocator` or another deliberate allocator choice in release/runtime code.
- `ArenaAllocator` is thread-safe in 0.16.0, but still use it for lifetime grouping, not as a universal default.

### Small but Useful 0.16 Changes

- Prefer `.empty` over old `init()` patterns for unmanaged std containers.
- Prefer `std.mem.find*` names over old `indexOf*` names in new code.
- `@trap()` is preferable to relying on `unreachable` when you want a defined abort in release builds.

## Forbidden Patterns

| ❌ Never | ✅ Instead |
|----------|-----------|
| Global allocator / hidden malloc | Pass `Allocator` explicitly |
| Hidden environment or args lookups deep in the stack | Read in `main(init)` and pass config explicitly |
| `@Type(...)` or made-up `@StructType`/`@IntType` names | Use `@Struct`, `@Int`, `@Enum`, `@Pointer`, etc. |
| `GeneralPurposeAllocator` | Use `DebugAllocator`, `smp_allocator`, arena, or another deliberate choice |
| `std.fs`-style examples without `io` in 0.16-only code | Use `std.Io` APIs with explicit `io` |
| `packed struct` or `packed union` without backing int when layout matters | Spell the backing integer explicitly |
| Blindly enabling `Io.Evented` in production guidance | Treat it as experimental until validated |
| `var` when `const` works | Default to `const` |
| Ignoring errors silently | Handle with `catch` or propagate with `try` |
| OOP inheritance hierarchies | Use composition and tagged unions |

## Detailed References

- **[references/idioms.md](references/idioms.md)** - Data-oriented design, memory patterns, testing
- **[references/async-io.md](references/async-io.md)** - New async/Io model, futures, cancellation
- **[references/c-interop.md](references/c-interop.md)** - C FFI, build-system integration, ABI compatibility
