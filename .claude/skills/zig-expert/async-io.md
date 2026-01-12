# Zig Async I/O Reference

The new async/Io model in Zig (2025+) provides colorblind async - single code works in both sync and async contexts.

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [The Io Interface](#the-io-interface)
3. [Futures and Await](#futures-and-await)
4. [Cancellation](#cancellation)
5. [Practical Patterns](#practical-patterns)
6. [FP Parallels](#fp-parallels)

---

## Core Concepts

### Colorblind Async

Traditional async has "function coloring" - async functions can only call async functions, creating viral spread. Zig defeats this by:

1. **Io as parameter** - Functions receive `Io` interface, not marked async
2. **Same code, different behavior** - Sync or async based on `Io` implementation
3. **No viral calling convention** - Regular functions can use async operations
4. **Library reuse** - One library works for both sync and async callers

```zig
// This function works in BOTH sync and async contexts
fn saveFile(io: std.Io, data: []const u8, path: []const u8) !void {
    const file = try std.Io.Dir.cwd().createFile(io, path, .{});
    defer file.close(io);
    try file.writeAll(io, data);
}
```

### Io Interface (Effect Handler Parallel)

The `Io` interface is similar to effect handlers in FP - it's a capability passed in that determines how I/O operations execute:

| Io Implementation | Behavior |
|-------------------|----------|
| Blocking Io | Traditional synchronous I/O |
| Event Loop Io | Non-blocking with epoll/kqueue/io_uring |
| Thread Pool Io | Operations dispatched to worker threads |
| Test Io | Mocked I/O for testing |

```zig
// FP parallel: Reader monad / effect handler
// The Io parameter is dependency injection for I/O effects
fn processRequest(io: std.Io, request: Request) !Response {
    // All I/O goes through io - behavior determined by caller
    const data = try fetchData(io, request.url);
    const result = process(data);
    try saveResult(io, result);
    return result;
}
```

---

## The Io Interface

### Passing Io Through Call Stack

```zig
const std = @import("std");
const Io = std.Io;

// Top-level receives Io from runtime
pub fn main(io: Io) !void {
    try runServer(io);
}

// Functions pass Io down
fn runServer(io: Io) !void {
    const listener = try Io.TcpListener.bind(io, .{ .port = 8080 });
    defer listener.close(io);
    
    while (true) {
        const conn = try listener.accept(io);
        try handleConnection(io, conn);
    }
}

fn handleConnection(io: Io, conn: Io.TcpStream) !void {
    defer conn.close(io);
    const request = try readRequest(io, conn);
    const response = try processRequest(io, request);
    try conn.writeAll(io, response);
}
```

### File Operations with Io

```zig
fn copyFile(io: Io, src: []const u8, dst: []const u8) !void {
    const src_file = try Io.Dir.cwd().openFile(io, src, .{});
    defer src_file.close(io);
    
    const dst_file = try Io.Dir.cwd().createFile(io, dst, .{});
    defer dst_file.close(io);
    
    var buf: [4096]u8 = undefined;
    while (true) {
        const n = try src_file.read(io, &buf);
        if (n == 0) break;
        try dst_file.writeAll(io, buf[0..n]);
    }
}
```

---

## Futures and Await

### Launching Concurrent Operations

```zig
fn saveData(io: Io, data: []const u8) !void {
    // Launch two concurrent file writes
    var file_a = io.async(saveToFile, .{ io, data, "backup_a.dat" });
    var file_b = io.async(saveToFile, .{ io, data, "backup_b.dat" });
    
    // Wait for both to complete
    try file_a.await(io);
    try file_b.await(io);
    
    // Both files written
    const stdout: Io.File = .stdout();
    try stdout.writeAll(io, "Save complete\n");
}

fn saveToFile(io: Io, data: []const u8, path: []const u8) !void {
    const file = try Io.Dir.cwd().createFile(io, path, .{});
    defer file.close(io);
    try file.writeAll(io, data);
}
```

### Future Type

```zig
// io.async returns a Future
const Future = struct {
    // Wait for completion, get result
    pub fn await(self: *Future, io: Io) !ResultType {
        // ...
    }
    
    // Cancel the operation
    pub fn cancel(self: *Future, io: Io) !void {
        // ...
    }
};
```

### Parallel Map Pattern

```zig
fn fetchAllUrls(io: Io, allocator: std.mem.Allocator, urls: []const []const u8) ![][]u8 {
    var futures = try allocator.alloc(@TypeOf(io.async(fetchUrl, .{ io, "" })), urls.len);
    defer allocator.free(futures);
    
    // Launch all requests concurrently
    for (urls, 0..) |url, i| {
        futures[i] = io.async(fetchUrl, .{ io, url });
    }
    
    // Collect results
    var results = try allocator.alloc([]u8, urls.len);
    errdefer {
        for (results) |r| allocator.free(r);
        allocator.free(results);
    }
    
    for (futures, 0..) |*future, i| {
        results[i] = try future.await(io);
    }
    
    return results;
}
```

---

## Cancellation

### Defer Cancel Pattern

```zig
fn withTimeout(io: Io, timeout_ns: u64) !Response {
    // Launch the main operation
    var operation = io.async(performOperation, .{io});
    
    // Ensure cancellation on any exit path
    defer operation.cancel(io) catch {};
    
    // Launch timeout
    var timer = io.async(sleep, .{ io, timeout_ns });
    defer timer.cancel(io) catch {};
    
    // Race: first to complete wins
    // (simplified - real implementation would use select/race)
    return try operation.await(io);
}
```

### Graceful Shutdown

```zig
fn runServer(io: Io, shutdown_signal: *std.atomic.Value(bool)) !void {
    const listener = try Io.TcpListener.bind(io, .{ .port = 8080 });
    defer listener.close(io);
    
    var active_connections = std.ArrayList(*Future).init(allocator);
    defer {
        // Cancel all active connections on shutdown
        for (active_connections.items) |conn| {
            conn.cancel(io) catch {};
        }
        active_connections.deinit();
    }
    
    while (!shutdown_signal.load(.acquire)) {
        const conn = listener.accept(io) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        
        const future = io.async(handleConnection, .{ io, conn });
        try active_connections.append(future);
    }
}
```

---

## Practical Patterns

### TCP Echo Server

```zig
const std = @import("std");
const Io = std.Io;

pub fn main(io: Io) !void {
    const listener = try Io.TcpListener.bind(io, .{
        .address = .{ .ipv4 = .{ 0, 0, 0, 0 } },
        .port = 8080,
    });
    defer listener.close(io);
    
    std.log.info("Listening on :8080", .{});
    
    while (true) {
        const conn = try listener.accept(io);
        _ = io.async(handleClient, .{ io, conn });
    }
}

fn handleClient(io: Io, conn: Io.TcpStream) void {
    defer conn.close(io);
    
    var buf: [1024]u8 = undefined;
    while (true) {
        const n = conn.read(io, &buf) catch return;
        if (n == 0) return;
        conn.writeAll(io, buf[0..n]) catch return;
    }
}
```

### HTTP Request Handler

```zig
fn handleHttpRequest(io: Io, conn: Io.TcpStream, allocator: std.mem.Allocator) !void {
    defer conn.close(io);
    
    // Read request
    var buf: [8192]u8 = undefined;
    const n = try conn.read(io, &buf);
    const request = try parseHttpRequest(buf[0..n]);
    
    // Process based on path
    const response = switch (request.path) {
        "/" => "HTTP/1.1 200 OK\r\n\r\nHello!",
        "/api/data" => try handleApiRequest(io, allocator, request),
        else => "HTTP/1.1 404 Not Found\r\n\r\nNot Found",
    };
    
    try conn.writeAll(io, response);
}
```

### Concurrent Database Queries

```zig
fn getUserDashboard(io: Io, db: *Database, user_id: UserId) !Dashboard {
    // Launch concurrent queries
    var profile_future = io.async(db.getProfile, .{ io, user_id });
    var orders_future = io.async(db.getRecentOrders, .{ io, user_id });
    var notifications_future = io.async(db.getNotifications, .{ io, user_id });
    
    // Await all results
    const profile = try profile_future.await(io);
    const orders = try orders_future.await(io);
    const notifications = try notifications_future.await(io);
    
    return Dashboard{
        .profile = profile,
        .orders = orders,
        .notifications = notifications,
    };
}
```

---

## FP Parallels

### Io as Reader Monad

The `Io` parameter pattern mirrors the Reader monad - computation that depends on an environment:

```zig
// Zig: Io parameter
fn fetchData(io: Io, url: []const u8) ![]u8 { ... }

// Haskell parallel: Reader IO
// fetchData :: String -> Reader IO ByteString

// TypeScript parallel: Reader pattern
// const fetchData = (url: string): Reader<IO, Uint8Array> => ...
```

### Future as Task/Effect

Zig's `Future` is similar to Task/Effect in FP - a description of async computation:

```zig
// Zig: Future from io.async
var future = io.async(compute, .{args});
const result = try future.await(io);

// FP parallel: Task that runs when awaited
// val task: Task[Result] = Task.async(compute(args))
// val result = task.runSync
```

### Structured Concurrency

The defer-cancel pattern enforces structured concurrency - child operations don't outlive parents:

```zig
fn withChildren(io: Io) !void {
    var child1 = io.async(work1, .{io});
    defer child1.cancel(io) catch {};
    
    var child2 = io.async(work2, .{io});
    defer child2.cancel(io) catch {};
    
    // Children guaranteed to complete or cancel
    // before this function returns
    try child1.await(io);
    try child2.await(io);
}
```

### No Function Coloring

Unlike Rust/JS/Python async, Zig functions don't need `async` markers:

```zig
// All these are regular functions
fn syncOrAsync(io: Io) !void { ... }
fn callsAsync(io: Io) !void {
    try syncOrAsync(io);  // no await keyword needed
}

// The Io implementation determines sync vs async behavior
// - BlockingIo: synchronous execution
// - EventLoopIo: asynchronous with event loop
```

This is the key insight: async is not a property of functions, but of the runtime environment (Io). Libraries write code once; callers choose sync or async by providing different Io implementations.
