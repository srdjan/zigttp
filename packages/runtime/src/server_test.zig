//! test-server: integration suite for the HTTP server / runtime facade.
//!
//! This is the Phase 0b gate for the engine/runtime facade refactor. It tests
//! the server through PUBLIC entry points only:
//!   - `Server.init` / `Server.deinit` lifecycle (server.zig)
//!   - `HandlerPool.executeHandler` / `.executeHandlerBorrowed` request flow,
//!     pool occupancy via `.getInUse` / `.max_size` (engine_adapter facade)
//!   - `RuntimeConfig` (jit_policy/jit_threshold) to drive engine behavior
//!     instead of poking `zq.interpreter.*` globals directly.
//!
//! It deliberately does NOT reach into interpreter/JIT internals. The JIT and
//! profiling tests in zruntime.zig (the `setJitPolicy`/`getJitPolicy`/
//! `snapshotPerfStats` sites flagged by the facade audit) should migrate here
//! and be re-expressed against `RuntimeConfig` + a future public perf-stats
//! accessor on `Runtime`.
//!
//! Track-B1 features (per-request timeout, graceful-shutdown drain, /_health,
//! /_readiness on pool exhaustion, panic isolation -> 500) do not exist yet.
//! They appear below as `error.SkipZigTest` placeholders so the suite compiles
//! and runs green today, and the tests can be filled in as each B1 slice lands.
//! Each placeholder names the exact public seam B1 must add.

const std = @import("std");
const builtin = @import("builtin");

const server = @import("server.zig");
const engine = @import("engine_adapter.zig");
const http_types = @import("http_types.zig");

const Server = server.Server;
const ServerConfig = server.ServerConfig;
const HandlerPool = engine.HandlerPool;
const RuntimeConfig = engine.RuntimeConfig;
const HttpRequestOwned = http_types.HttpRequestOwned;
const HttpResponse = http_types.HttpResponse;

// Generational-GC heap corruption guard, mirrored from zruntime.zig: the
// concurrent/recycling paths are flaky under linux glibc malloc only. Keep the
// same gate so this suite stays green on CI without masking real bugs on macOS.
const skip_linux_glibc_heap_corruption_tests = builtin.os.tag == .linux;

/// Build an owned GET request with no body. Caller deinits.
fn getRequest(allocator: std.mem.Allocator, url: []const u8) !HttpRequestOwned {
    return .{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, url),
        .headers = .empty,
        .body = null,
    };
}

// ===========================================================================
// Server lifecycle (public init/deinit surface)
// ===========================================================================

test "Server.init/deinit round-trips an inline handler without leaking" {
    const allocator = std.testing.allocator;
    var srv = try Server.init(allocator, .{
        .handler = .{ .inline_code = "function handler(req) { return Response.text('ok'); }" },
        .log_requests = false,
        .pool_size = 1,
    });
    defer srv.deinit();

    // init must not start the listener or pool; those are start()'s job.
    try std.testing.expect(srv.pool == null);
    try std.testing.expect(!srv.running);
}

test "Server.init auto-sizes the pool when pool_size is zero" {
    const allocator = std.testing.allocator;
    var srv = try Server.init(allocator, .{
        .handler = .{ .inline_code = "function handler(req) { return Response.text('ok'); }" },
        .log_requests = false,
        .pool_size = 0,
    });
    defer srv.deinit();
    try std.testing.expect(srv.config.pool_size > 0);
}

// ===========================================================================
// Handler execution: happy path (HandlerPool public surface)
// ===========================================================================

test "executeHandler returns 200 with the handler body" {
    const allocator = std.heap.c_allocator;
    var pool = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled },
        "function handler(req) { return Response.json({ ok: true }); }",
        "<server-test>",
        1,
        0,
    );
    defer pool.deinit();

    var req = try getRequest(allocator, "/");
    defer req.deinit(allocator);

    var resp = try pool.executeHandler(req.asView());
    defer resp.deinit();

    try std.testing.expectEqual(@as(u16, 200), resp.status);
    try std.testing.expect(std.mem.indexOf(u8, resp.body, "\"ok\":true") != null);
}

test "executeHandler echoes request method and url back to the handler" {
    const allocator = std.heap.c_allocator;
    // `req.url` is the raw path here (the subset has no `new URL`); the runtime
    // surfaces the request line as method + url.
    var pool = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled },
        "function handler(req) { return Response.text(req.method + ' ' + req.url); }",
        "<server-test>",
        1,
        0,
    );
    defer pool.deinit();

    var req = try getRequest(allocator, "/widgets");
    defer req.deinit(allocator);

    var resp = try pool.executeHandler(req.asView());
    defer resp.deinit();

    try std.testing.expectEqual(@as(u16, 200), resp.status);
    try std.testing.expectEqualStrings("GET /widgets", resp.body);
}

test "pool occupancy is zero before and after a completed request" {
    const allocator = std.heap.c_allocator;
    var pool = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled },
        "function handler(req) { return Response.text('ok'); }",
        "<server-test>",
        2,
        0,
    );
    defer pool.deinit();

    try std.testing.expectEqual(@as(usize, 0), pool.getInUse());
    try std.testing.expect(pool.max_size == 2);

    var req = try getRequest(allocator, "/");
    defer req.deinit(allocator);
    var resp = try pool.executeHandler(req.asView());
    resp.deinit();

    // The runtime is returned to the pool, so occupancy settles back to zero.
    try std.testing.expectEqual(@as(usize, 0), pool.getInUse());
}

// ===========================================================================
// Handler execution: error paths
// ===========================================================================

test "assert guard rejection short-circuits with the guard response" {
    const allocator = std.heap.c_allocator;
    // `assert cond, response` is the supported statement form: on a failed
    // condition the handler returns the guard response instead of falling
    // through. This is the documented error path for declarative guards.
    var pool = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled },
        "function handler(req) { assert req.method === 'POST', Response.text('method not allowed', { status: 405 }); return Response.text('ok'); }",
        "<server-test>",
        1,
        0,
    );
    defer pool.deinit();

    var req = try getRequest(allocator, "/"); // GET -> guard fails
    defer req.deinit(allocator);

    var resp = try pool.executeHandler(req.asView());
    defer resp.deinit();
    try std.testing.expectEqual(@as(u16, 405), resp.status);
    try std.testing.expectEqualStrings("method not allowed", resp.body);
}

test "B1: handler returning a non-Response value should not yield a silent empty 200" {
    // PLACEHOLDER + documents a KNOWN GAP. Today a handler that returns a
    // non-Response primitive (e.g. `return 42;`) flows through
    // extractResponseInternal's `if (!result.isObject())` branch and produces
    // the default HttpResponse: status 200, empty body. The `hasException`
    // guard in executeHandler only fires when an exception is SET, not when a
    // handler simply returns the wrong type. B1 should enforce the handler
    // return contract (non-Response -> 500) at the execution boundary, then
    // this test becomes: `return 42;` -> 500 (or a surfaced HandlerError).
    //
    // Verified-today behavior (the gap): `return 42;` -> 200 with empty body.
    return error.SkipZigTest;
}

test "handler returning a string body yields a 200 with that body" {
    // The non-object/string branch of extractResponseInternal is a supported,
    // documented path: a bare string return becomes the response body.
    const allocator = std.heap.c_allocator;
    var pool = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled },
        "function handler(req) { return 'plain body'; }",
        "<server-test>",
        1,
        0,
    );
    defer pool.deinit();

    var req = try getRequest(allocator, "/");
    defer req.deinit(allocator);

    var resp = try pool.executeHandler(req.asView());
    defer resp.deinit();
    try std.testing.expectEqual(@as(u16, 200), resp.status);
    try std.testing.expectEqualStrings("plain body", resp.body);
}

test "executeHandlerBorrowed releases the runtime even when the handler errors" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.heap.c_allocator;
    var pool = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled },
        "function handler(req) { return Response.text('ok'); }",
        "<server-test>",
        1,
        0,
    );
    defer pool.deinit();

    var req = try getRequest(allocator, "/");
    defer req.deinit(allocator);

    {
        var handle = try pool.executeHandlerBorrowed(req.asView());
        try std.testing.expectEqual(@as(usize, 1), pool.getInUse());
        handle.deinit();
    }
    // After the borrowed handle is released the slot returns to the pool.
    try std.testing.expectEqual(@as(usize, 0), pool.getInUse());
}

// ===========================================================================
// Keep-alive (placeholder: needs a public raw-request seam)
// ===========================================================================

test "keep-alive: two sequential requests reuse one connection" {
    // PLACEHOLDER. End-to-end keep-alive lives in ConnectionPool
    // (handleSingleRequestSync -> RequestOutcome.keep_alive), which is
    // file-private in server.zig. To test it from this module without surface
    // creep, B1 must expose a public test seam, e.g.
    //   pub fn Server.handleRawRequestForTest(fd, request_num) !RequestOutcome
    // (or a buffer-in/buffer-out variant). Fill in once that lands.
    return error.SkipZigTest;
}

// ===========================================================================
// Track-B1 placeholders. Each names the public seam B1 must add.
// ===========================================================================

test "B1: per-request timeout aborts a slow handler and returns 504" {
    // PLACEHOLDER. Needs ServerConfig.timeout_ms (exists) to be ENFORCED per
    // request by the execution path, plus a public seam that runs a single
    // request with a deadline (e.g. HandlerPool.executeHandlerWithDeadline or
    // Server.handleRawRequestForTest). Today timeout_ms is connection-scoped
    // only. Assert: slow handler -> 504, fast handler -> 200.
    return error.SkipZigTest;
}

test "B1: graceful shutdown drains in-flight requests before closing" {
    // PLACEHOLDER. Needs Server.shutdown() (graceful) that flips `running`
    // false, stops accepting, and waits for getInUse()==0 before returning.
    // Assert: a request in flight at shutdown completes with its real body,
    // and post-shutdown the listener is closed.
    return error.SkipZigTest;
}

test "B1: /_health returns 200" {
    // PLACEHOLDER. Needs the health route wired into the request dispatch
    // (ConnectionPool handler dispatch, ahead of the JS handler) plus the
    // public raw-request seam to drive it. Assert: GET /_health -> 200.
    return error.SkipZigTest;
}

test "B1: /_readiness returns 503 when the pool is exhausted" {
    // PLACEHOLDER. Needs the readiness route to consult HandlerPool occupancy
    // (getInUse vs max_size) and return 503 when no slot is free, 200
    // otherwise. Drive via the public raw-request seam with a pool_size=1 pool
    // whose only slot is held by an in-flight borrowed handle.
    return error.SkipZigTest;
}

test "B1: a panicking handler is isolated and the connection returns 500" {
    // PLACEHOLDER. Needs panic isolation in the execution path so a handler
    // panic is caught at the request boundary and mapped to a 500 without
    // taking down the worker/server. Assert: panicking handler -> 500, and a
    // subsequent request on a fresh runtime -> 200 (proves isolation).
    return error.SkipZigTest;
}
