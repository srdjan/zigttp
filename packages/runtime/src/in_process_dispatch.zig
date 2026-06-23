//! In-process handler registry for multi-handler workflows.
//!
//! The orchestrator handler dispatches to co-located sub-handlers BY NAME
//! without HTTP, reusing `HandlerPool.executeHandlerBorrowed` for full
//! per-call isolation: a separate pooled runtime with its own GC/arena,
//! arena reset on release, and setjmp panic isolation. A sub-handler panic
//! quarantines only its own pool slot and surfaces an error to the
//! orchestrator (a different Runtime instance), never corrupting it.
//!
//! This is the substrate the `zigttp:workflow` combinators build on. Route
//! (href) based resolution for HATEOAS link-following is layered on top
//! separately; this module is the name-keyed core.

const std = @import("std");
const builtin = @import("builtin");
const runtime_pool = @import("runtime_pool.zig");
const runtime_config = @import("runtime_config.zig");
const http_types = @import("http_types.zig");

const HandlerPool = runtime_pool.HandlerPool;
const ResponseHandle = HandlerPool.ResponseHandle;
const RuntimeConfig = runtime_config.RuntimeConfig;
const HttpRequestView = http_types.HttpRequestView;

/// One co-located sub-handler: a name and its own isolated HandlerPool. The
/// pool borrows `handler_code` for its lifetime, so it is freed after the pool.
pub const Target = struct {
    name: []const u8,
    handler_code: []const u8,
    pool: HandlerPool,

    fn deinit(self: *Target, allocator: std.mem.Allocator) void {
        self.pool.deinit();
        allocator.free(self.handler_code);
        allocator.free(self.name);
    }
};

/// A set of co-located handlers addressable by name, each with its own pool.
/// Built from a system manifest (name -> source path) at startup.
pub const SystemRuntime = struct {
    allocator: std.mem.Allocator,
    targets: std.ArrayListUnmanaged(Target) = .empty,

    pub fn init(allocator: std.mem.Allocator) SystemRuntime {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *SystemRuntime) void {
        for (self.targets.items) |*t| t.deinit(self.allocator);
        self.targets.deinit(self.allocator);
    }

    /// Load a sub-handler from source into its own pool, addressable by `name`.
    pub fn addHandler(
        self: *SystemRuntime,
        name: []const u8,
        source: []const u8,
        entry: []const u8,
        config: RuntimeConfig,
        pool_size: usize,
    ) !void {
        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);
        const code_owned = try self.allocator.dupe(u8, source);
        errdefer self.allocator.free(code_owned);

        var pool = try HandlerPool.init(self.allocator, config, code_owned, entry, pool_size, 0);
        errdefer pool.deinit();

        try self.targets.append(self.allocator, .{
            .name = name_owned,
            .handler_code = code_owned,
            .pool = pool,
        });
    }

    pub fn find(self: *SystemRuntime, name: []const u8) ?*Target {
        for (self.targets.items) |*t| {
            if (std.mem.eql(u8, t.name, name)) return t;
        }
        return null;
    }

    /// Dispatch a request to a co-located sub-handler by name, in-process.
    /// The returned handle borrows the sub-runtime's response bytes; the caller
    /// must copy anything it needs before calling `handle.deinit()`.
    pub fn dispatch(self: *SystemRuntime, name: []const u8, request: HttpRequestView) !ResponseHandle {
        const target = self.find(name) orelse return error.UnknownHandler;
        return target.pool.executeHandlerBorrowed(request);
    }
};

const skip_linux_glibc_heap_corruption_tests = builtin.os.tag == .linux;

test "SystemRuntime dispatches a request to a named co-located handler in-process" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.heap.c_allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();

    try sys.addHandler(
        "payments",
        "function handler(req) { return Response.json({ from: 'payments' }); }",
        "<payments>",
        .{ .jit_policy = .disabled },
        1,
    );
    try sys.addHandler(
        "orders",
        "function handler(req) { return Response.json({ from: 'orders' }); }",
        "<orders>",
        .{ .jit_policy = .disabled },
        1,
    );

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{
        .method = "GET",
        .url = "/",
        .path = "/",
        .headers = headers,
        .body = null,
    };

    var handle = try sys.dispatch("payments", view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 200), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "payments") != null);

    // An unknown sub-handler is a clear error, not a silent default.
    try std.testing.expectError(error.UnknownHandler, sys.dispatch("nope", view));
}
