# Zig Idioms Reference

Detailed patterns for data-oriented design, memory management, and testing in idiomatic Zig.

## Table of Contents

1. [Data-Oriented Design](#data-oriented-design)
2. [Memory Patterns](#memory-patterns)
3. [Error Handling Patterns](#error-handling-patterns)
4. [Compile-Time Patterns](#compile-time-patterns)
5. [Testing Patterns](#testing-patterns)
6. [Common Tasks](#common-tasks)

---

## Data-Oriented Design

Data-oriented design (DOD) structures programs around data layout and access patterns for optimal performance. Zig's low-level control makes DOD natural.

### Array of Structs vs Struct of Arrays

```zig
// ❌ Array of Structs (AoS) - poor cache locality for partial access
const EntityAoS = struct {
    position: Vec3,
    velocity: Vec3,
    health: f32,
    name: [32]u8,
};
var entities_aos: [1000]EntityAoS = undefined;

// ✅ Struct of Arrays (SoA) - excellent cache locality
const EntitiesSoA = struct {
    positions: [1000]Vec3,
    velocities: [1000]Vec3,
    healths: [1000]f32,
    names: [1000][32]u8,
};
var entities_soa: EntitiesSoA = undefined;

// Update all positions - touches only position data
fn updatePositions(entities: *EntitiesSoA, dt: f32) void {
    for (&entities.positions, entities.velocities) |*pos, vel| {
        pos.* = pos.*.add(vel.scale(dt));
    }
}
```

### Packing Data for Cache Efficiency

```zig
// ❌ Wasteful - bool padded to 4+ bytes per entity
const CreatureWasteful = struct {
    kind: enum { Elf, Orc },
    is_alive: bool,
    // 3+ bytes padding
};

// ✅ Packed - encode state in single enum
const CreatureStatus = enum(u8) {
    alive_elf,
    dead_elf,
    alive_orc,
    dead_orc,
    
    pub fn isAlive(self: CreatureStatus) bool {
        return self == .alive_elf or self == .alive_orc;
    }
    
    pub fn kind(self: CreatureStatus) enum { elf, orc } {
        return switch (self) {
            .alive_elf, .dead_elf => .elf,
            .alive_orc, .dead_orc => .orc,
        };
    }
};
```

### Separate Hot and Cold Data

```zig
// Hot data - accessed every frame
const EntityHot = struct {
    position: Vec3,
    velocity: Vec3,
    flags: u32,
};

// Cold data - accessed rarely
const EntityCold = struct {
    name: [64]u8,
    creation_time: i64,
    metadata: []const u8,
};

// Store separately
const World = struct {
    hot: []EntityHot,      // dense array, great cache use
    cold: []EntityCold,    // separate allocation
    allocator: std.mem.Allocator,
};
```

### Index-Based References

```zig
// ❌ Pointer-based - cache unfriendly, complex lifetime
const NodePtr = struct {
    data: i32,
    next: ?*NodePtr,
};

// ✅ Index-based - cache friendly, simple lifetime
const NodeIndex = u32;
const INVALID_INDEX: NodeIndex = std.math.maxInt(NodeIndex);

const NodePool = struct {
    data: []i32,
    next: []NodeIndex,
    allocator: std.mem.Allocator,
    
    pub fn get(self: *const NodePool, idx: NodeIndex) i32 {
        return self.data[idx];
    }
    
    pub fn getNext(self: *const NodePool, idx: NodeIndex) ?NodeIndex {
        const next = self.next[idx];
        return if (next == INVALID_INDEX) null else next;
    }
};
```

---

## Memory Patterns

### Arena Allocator for Batch Operations

```zig
// Arena: fast bump allocation, single free at end
fn processRequest(parent_allocator: std.mem.Allocator, request: Request) !Response {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit(); // frees ALL arena allocations
    const allocator = arena.allocator();
    
    // All allocations use arena - no individual frees needed
    const parsed = try parseJson(allocator, request.body);
    const validated = try validate(allocator, parsed);
    const enriched = try enrich(allocator, validated);
    
    // Only final result needs to survive
    return try Response.clone(parent_allocator, enriched);
}
```

### Fixed Buffer Allocator for Stack Allocation

```zig
fn formatMessage(comptime max_len: usize, fmt: []const u8, args: anytype) []const u8 {
    var buf: [max_len]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    
    return std.fmt.allocPrint(fba.allocator(), fmt, args) catch |_| {
        return "[message too long]";
    };
}

// Usage - no heap allocation
const msg = formatMessage(256, "User {} logged in from {}", .{ user_id, ip });
```

### Pool Allocator for Same-Size Objects

```zig
const EntityPool = struct {
    pool: std.heap.MemoryPool(Entity),
    
    pub fn init(allocator: std.mem.Allocator) EntityPool {
        return .{ .pool = std.heap.MemoryPool(Entity).init(allocator) };
    }
    
    pub fn create(self: *EntityPool) !*Entity {
        return try self.pool.create();
    }
    
    pub fn destroy(self: *EntityPool, entity: *Entity) void {
        self.pool.destroy(entity);
    }
    
    pub fn deinit(self: *EntityPool) void {
        self.pool.deinit();
    }
};
```

### Ownership Transfer Pattern

```zig
// Caller owns returned memory
fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    
    const stat = try file.stat();
    const buffer = try allocator.alloc(u8, stat.size);
    errdefer allocator.free(buffer); // only on error
    
    const bytes_read = try file.readAll(buffer);
    if (bytes_read != stat.size) {
        return error.IncompleteRead;
    }
    
    return buffer; // ownership transfers to caller
}

// Caller must free
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    const content = try readFile(allocator, "data.txt");
    defer allocator.free(content); // caller's responsibility
}
```

---

## Error Handling Patterns

### Error Set Composition

```zig
// Domain-specific error sets
const ParseError = error{ InvalidSyntax, UnexpectedToken, EndOfInput };
const IoError = error{ NotFound, PermissionDenied, BrokenPipe };

// Combined error set for high-level function
const LoadConfigError = ParseError || IoError || error{InvalidConfig};

fn loadConfig(path: []const u8) LoadConfigError!Config {
    const content = try readFile(path); // IoError
    const parsed = try parse(content);   // ParseError
    return try validate(parsed);          // error.InvalidConfig
}
```

### Error Transformation (Mapping)

```zig
const AppError = error{
    ConfigError,
    DatabaseError,
    NetworkError,
};

fn initialize() AppError!void {
    loadConfig() catch |err| {
        std.log.err("Config failed: {}", .{err});
        return error.ConfigError;
    };
    
    connectDb() catch |err| {
        std.log.err("DB failed: {}", .{err});
        return error.DatabaseError;
    };
}
```

### Optional Chaining

```zig
const User = struct {
    profile: ?*Profile,
};

const Profile = struct {
    address: ?*Address,
};

const Address = struct {
    city: []const u8,
};

fn getUserCity(user: ?*User) ?[]const u8 {
    const u = user orelse return null;
    const profile = u.profile orelse return null;
    const address = profile.address orelse return null;
    return address.city;
}
```

---

## Compile-Time Patterns

### Type-Safe Builder

```zig
fn HttpRequest(comptime has_body: bool) type {
    return struct {
        method: []const u8,
        path: []const u8,
        body: if (has_body) []const u8 else void,
        
        const Self = @This();
        
        pub fn withBody(self: Self, body: []const u8) HttpRequest(true) {
            return .{
                .method = self.method,
                .path = self.path,
                .body = body,
            };
        }
    };
}

fn get(path: []const u8) HttpRequest(false) {
    return .{ .method = "GET", .path = path, .body = {} };
}

// Usage
const req = get("/api/users").withBody("{}");
```

### Compile-Time Validation

```zig
fn Port(comptime value: u16) type {
    if (value == 0) @compileError("Port cannot be 0");
    if (value < 1024) @compileError("Use privileged ports explicitly");
    
    return struct {
        pub const port = value;
    };
}

const ServerPort = Port(8080); // OK
// const BadPort = Port(0);    // compile error
```

### Inline Switch for Dispatch

```zig
const Operation = enum { add, sub, mul, div };

fn compute(comptime op: Operation, a: i32, b: i32) i32 {
    return switch (op) {
        .add => a + b,
        .sub => a - b,
        .mul => a * b,
        .div => @divTrunc(a, b),
    };
}

// At compile time, only the relevant branch exists
const result = compute(.add, 5, 3); // compiles to just: 5 + 3
```

### Reflection for Serialization

```zig
fn toJson(comptime T: type, value: T, writer: anytype) !void {
    const info = @typeInfo(T);
    
    switch (info) {
        .Struct => |s| {
            try writer.writeAll("{");
            inline for (s.fields, 0..) |field, i| {
                if (i > 0) try writer.writeAll(",");
                try writer.print("\"{s}\":", .{field.name});
                try toJson(field.type, @field(value, field.name), writer);
            }
            try writer.writeAll("}");
        },
        .Int => try writer.print("{}", .{value}),
        .Pointer => |p| if (p.child == u8) {
            try writer.print("\"{s}\"", .{value});
        },
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    }
}
```

---

## Testing Patterns

### Test Allocator for Leak Detection

```zig
test "no memory leaks" {
    const allocator = std.testing.allocator; // fails test on leak
    
    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();
    
    try list.appendSlice("hello");
    try std.testing.expectEqualStrings("hello", list.items);
}
```

### Table-Driven Tests

```zig
test "parser handles all cases" {
    const cases = .{
        .{ "123", 123 },
        .{ "-456", -456 },
        .{ "0", 0 },
        .{ "+789", 789 },
    };
    
    inline for (cases) |case| {
        const input, const expected = case;
        const result = try parseInt(input);
        try std.testing.expectEqual(expected, result);
    }
}
```

### Fuzz Testing

```zig
test "fuzz parser" {
    const allocator = std.testing.allocator;
    
    // Generate random inputs
    var prng = std.Random.DefaultPrng.init(0);
    const random = prng.random();
    
    for (0..1000) |_| {
        var buf: [100]u8 = undefined;
        const len = random.intRangeAtMost(usize, 1, 100);
        random.bytes(buf[0..len]);
        
        // Should not crash on any input
        _ = parse(allocator, buf[0..len]) catch continue;
    }
}
```

---

## Common Tasks

### File I/O

```zig
// Read entire file
fn readAll(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, std.math.maxInt(usize));
}

// Write file
fn writeAll(path: []const u8, data: []const u8) !void {
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();
    try file.writeAll(data);
}

// Read lines iterator
fn readLines(allocator: std.mem.Allocator, path: []const u8) !LineIterator {
    const file = try std.fs.cwd().openFile(path, .{});
    return .{
        .file = file,
        .reader = file.reader(),
        .allocator = allocator,
    };
}
```

### JSON Parsing

```zig
const std = @import("std");

const Config = struct {
    host: []const u8,
    port: u16,
    debug: bool = false,
};

fn parseConfig(allocator: std.mem.Allocator, json: []const u8) !Config {
    const parsed = try std.json.parseFromSlice(Config, allocator, json, .{});
    defer parsed.deinit();
    return parsed.value;
}

fn serializeConfig(allocator: std.mem.Allocator, config: Config) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    try std.json.stringify(config, .{}, list.writer());
    return list.toOwnedSlice();
}
```

### HTTP Client (Basic)

```zig
fn httpGet(allocator: std.mem.Allocator, url: []const u8) ![]u8 {
    var client = std.http.Client{ .allocator = allocator };
    defer client.deinit();
    
    var response = std.ArrayList(u8).init(allocator);
    const result = try client.fetch(.{
        .location = .{ .url = url },
        .response_storage = .{ .dynamic = &response },
    });
    
    if (result.status != .ok) {
        return error.HttpError;
    }
    
    return response.toOwnedSlice();
}
```

### Command Line Arguments

```zig
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    
    if (args.len < 2) {
        std.debug.print("Usage: {s} <filename>\n", .{args[0]});
        return error.InvalidArgs;
    }
    
    const filename = args[1];
    // ...
}
```

### Logging

```zig
const std = @import("std");

// Scoped logger
const log = std.log.scoped(.my_module);

fn doWork() !void {
    log.info("Starting work", .{});
    
    const result = performOperation() catch |err| {
        log.err("Operation failed: {}", .{err});
        return err;
    };
    
    log.debug("Result: {}", .{result});
}
```
