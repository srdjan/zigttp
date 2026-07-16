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
const zq = @import("zigts");
const runtime_pool = @import("runtime_pool.zig");
const runtime_config = @import("runtime_config.zig");
const http_types = @import("http_types.zig");

const HandlerPool = runtime_pool.HandlerPool;
const ResponseHandle = HandlerPool.ResponseHandle;
const RuntimeConfig = runtime_config.RuntimeConfig;
const HttpRequestView = http_types.HttpRequestView;

/// One co-located sub-handler: a name and its own isolated HandlerPool. The
/// pool borrows `handler_code`, `filename`, and policy slices from `contract`
/// for its lifetime, so they are freed after the pool. The registration owns
/// all four so callers may free the source/path they passed in right after.
pub const Target = struct {
    name: []const u8,
    filename: []const u8,
    handler_code: []const u8,
    contract: zq.HandlerContract,
    pool: HandlerPool,

    fn deinit(self: *Target, allocator: std.mem.Allocator) void {
        self.pool.deinit();
        self.contract.deinit(allocator);
        allocator.free(self.handler_code);
        allocator.free(self.filename);
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
        var contract = try zq.pipeline.extractContract(self.allocator, source, entry, .{
            .strict = false,
            .version = zq.version.string,
            .read_file = zq.file_io.readFileForModuleGraph,
        });
        errdefer contract.deinit(self.allocator);

        var target_config = config;
        target_config.dev_capability_policy = zq.handler_policy.contractToRuntimePolicy(&contract);

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);
        const code_owned = try self.allocator.dupe(u8, source);
        errdefer self.allocator.free(code_owned);
        const filename_owned = try self.allocator.dupe(u8, entry);
        errdefer self.allocator.free(filename_owned);

        var pool = try HandlerPool.init(self.allocator, target_config, code_owned, filename_owned, pool_size, 0);
        errdefer pool.deinit();

        try self.targets.append(self.allocator, .{
            .name = name_owned,
            .filename = filename_owned,
            .handler_code = code_owned,
            .contract = contract,
            .pool = pool,
        });
    }

    /// Build a registry from a `system.json` manifest: parse it, load each
    /// handler's source from its `path`, and register it under its `name` in
    /// its own isolated pool. `base_config` seeds every sub-handler's config
    /// with `system_registry` cleared so sub-handlers are leaf handlers (no
    /// nested in-process orchestration in this phase). Caller owns the result
    /// and must `deinit()` it.
    pub fn buildFromSystemConfig(
        allocator: std.mem.Allocator,
        system_json_path: []const u8,
        base_config: RuntimeConfig,
        pool_size: usize,
    ) !SystemRuntime {
        const system_json = try zq.file_io.readFile(allocator, system_json_path, 1024 * 1024);
        defer allocator.free(system_json);

        var config = try zq.system_linker.parseSystemConfig(allocator, system_json);
        defer config.deinit(allocator);

        var sub_config = base_config;
        sub_config.system_registry = null;

        var sys = SystemRuntime.init(allocator);
        errdefer sys.deinit();

        const system_dir = std.fs.path.dirname(system_json_path) orelse ".";
        for (config.handlers) |entry| {
            // A workflow-enabled `--system` bundle is local and fail-fast: every
            // handler with a `path` must be readable before the server starts.
            // Path resolution mirrors the proof tooling (cwd first, then
            // manifest directory) so runtime and `zigts link` agree.
            const entry_path = try resolveSystemHandlerPath(allocator, system_dir, entry.path);
            defer allocator.free(entry_path);

            const source = try zq.file_io.readFile(allocator, entry_path, 10 * 1024 * 1024);
            defer allocator.free(source);
            try sys.addHandler(entry.name, source, entry_path, sub_config, pool_size);
        }
        return sys;
    }

    pub fn find(self: *SystemRuntime, name: []const u8) ?*Target {
        for (self.targets.items) |*t| {
            if (std.mem.eql(u8, t.name, name)) return t;
        }
        return null;
    }

    /// Resolve a request path to a co-located handler by the "/<name>" mount
    /// convention: the target whose name, taken as a leading path segment, is
    /// the longest prefix of `path`. This is how `workflow.follow` dispatches a
    /// resolved affordance href without a separate route table: a bundle handler
    /// named `payments` owns `/payments` and `/payments/...`. Returns null when
    /// no handler mounts a prefix of `path` (a 599 UnknownRoute at the call site).
    pub fn findByRoute(self: *SystemRuntime, path: []const u8) ?*Target {
        var best: ?*Target = null;
        var best_len: usize = 0;
        for (self.targets.items) |*t| {
            if (pathMountsName(path, t.name) and t.name.len > best_len) {
                best = t;
                best_len = t.name.len;
            }
        }
        return best;
    }

    /// Dispatch a request to a co-located sub-handler by name, in-process.
    /// The returned handle borrows the sub-runtime's response bytes; the caller
    /// must copy anything it needs before calling `handle.deinit()`.
    pub fn dispatch(self: *SystemRuntime, name: []const u8, request: HttpRequestView) !ResponseHandle {
        const target = self.find(name) orelse return error.UnknownHandler;
        return target.pool.executeHandlerBorrowed(request);
    }
};

fn resolveSystemHandlerPath(
    allocator: std.mem.Allocator,
    system_dir: []const u8,
    entry_path: []const u8,
) ![]const u8 {
    if (std.fs.path.isAbsolute(entry_path)) return try allocator.dupe(u8, entry_path);
    if (zq.file_io.fileExists(allocator, entry_path)) return try allocator.dupe(u8, entry_path);
    return std.fs.path.resolve(allocator, &.{ system_dir, entry_path });
}

/// True when `path` is mounted under handler `name`: `path` is exactly
/// `/<name>` or begins with `/<name>/`. Avoids matching `/payments` against a
/// handler named `pay`.
fn pathMountsName(path: []const u8, name: []const u8) bool {
    if (path.len == 0 or path[0] != '/') return false;
    const rest = path[1..];
    if (!std.mem.startsWith(u8, rest, name)) return false;
    const after = rest[name.len..];
    return after.len == 0 or after[0] == '/';
}

test "SystemRuntime dispatches a request to a named co-located handler in-process" {
    const allocator = std.testing.allocator;

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

// The remaining tests exercise the full `zigttp:workflow.call` path end to end:
// an orchestrator handler (its own HandlerPool, with `config.system_registry`
// pointing at a SystemRuntime) imports `zigttp:workflow` and dispatches to a
// co-located sub-handler in-process via `workflowCallCallback`.

test "workflow.call dispatches to a co-located sub-handler and copies out its response" {
    const allocator = std.testing.allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "greeter",
        "function handler(req) { return Response.json({ hello: req.method, at: req.url }); }",
        "<greeter>",
        .{ .jit_policy = .disabled },
        1,
    );

    // The orchestrator returns the sub-handler's Response verbatim; asserting on
    // it proves the borrowed bytes were copied out before `handle.deinit()`.
    const orchestrator_src =
        \\import { call } from "zigttp:workflow";
        \\function handler(req) {
        \\  return call("greeter", { method: "POST", path: "/hi" });
        \\}
    ;
    var orch = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled, .system_registry = @ptrCast(&sys) },
        orchestrator_src,
        "<orchestrator>",
        1,
        0,
    );
    defer orch.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{
        .method = "GET",
        .url = "/",
        .path = "/",
        .headers = headers,
        .body = null,
    };

    var handle = try orch.executeHandlerBorrowed(view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 200), handle.response.status);
    // The sub-handler saw the method/path the orchestrator passed in `init`.
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "\"hello\":\"POST\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "/hi") != null);
}

test "workflow.call to an unknown handler fails soft as a 599 error response" {
    const allocator = std.testing.allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "greeter",
        "function handler(req) { return Response.json({ ok: true }); }",
        "<greeter>",
        .{ .jit_policy = .disabled },
        1,
    );

    // A missing target must not crash the orchestrator: the dispatch error is
    // turned into a 599 error Response (same catch arm `error.HandlerPanicked`
    // takes, so this also covers panic isolation surfacing).
    const orchestrator_src =
        \\import { call } from "zigttp:workflow";
        \\function handler(req) {
        \\  return call("nope", { path: "/x" });
        \\}
    ;
    var orch = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled, .system_registry = @ptrCast(&sys) },
        orchestrator_src,
        "<orchestrator>",
        1,
        0,
    );
    defer orch.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{
        .method = "GET",
        .url = "/",
        .path = "/",
        .headers = headers,
        .body = null,
    };

    var handle = try orch.executeHandlerBorrowed(view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 599), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "UnknownHandler") != null);
}

test "pathMountsName matches exact and sub-paths, not bare name prefixes" {
    try std.testing.expect(pathMountsName("/payments", "payments"));
    try std.testing.expect(pathMountsName("/payments/charge", "payments"));
    // A handler named `pay` must not capture `/payments`.
    try std.testing.expect(!pathMountsName("/payments", "pay"));
    try std.testing.expect(!pathMountsName("/pay", "payments"));
    // Missing leading slash or root path never mounts.
    try std.testing.expect(!pathMountsName("payments", "payments"));
    try std.testing.expect(!pathMountsName("/", "payments"));
}

test "workflow.follow routes a resolved affordance href to a co-located handler by mount" {
    const allocator = std.testing.allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "payments",
        "function handler(req) { return Response.json({ from: 'payments', at: req.url }); }",
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

    // The orchestrator builds a resource() and follows its `pay` affordance;
    // follow resolves href "/payments/charge" -> the "payments" mount, in-process.
    const orchestrator_src =
        \\import { follow } from "zigttp:workflow";
        \\function handler(req) {
        \\  const r = resource({ id: 1 }, {
        \\    pay: { href: "/payments/charge", method: "POST" },
        \\  });
        \\  return follow(r, "pay");
        \\}
    ;
    var orch = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled, .system_registry = @ptrCast(&sys) },
        orchestrator_src,
        "<orchestrator>",
        1,
        0,
    );
    defer orch.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{ .method = "GET", .url = "/", .path = "/", .headers = headers, .body = null };

    var handle = try orch.executeHandlerBorrowed(view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 200), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "\"from\":\"payments\"") != null);
    // Dispatched with the affordance's href as the request path.
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "/payments/charge") != null);
}

test "workflow.follow routes hrefs by path while preserving parsed query" {
    const allocator = std.testing.allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "greet",
        "function handler(req) { return Response.json({ path: req.path, url: req.url, lang: req.query.lang }); }",
        "<greet>",
        .{ .jit_policy = .disabled },
        1,
    );

    const orchestrator_src =
        \\import { follow } from "zigttp:workflow";
        \\function handler(req) {
        \\  const r = resource({ id: 1 }, {
        \\    hello: { href: "/greet?lang=en#ignored", method: "GET" },
        \\  });
        \\  return follow(r, "hello");
        \\}
    ;
    var orch = try HandlerPool.init(allocator, .{ .jit_policy = .disabled, .system_registry = @ptrCast(&sys) }, orchestrator_src, "<orchestrator>", 1, 0);
    defer orch.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{ .method = "GET", .url = "/", .path = "/", .headers = headers, .body = null };

    var handle = try orch.executeHandlerBorrowed(view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 200), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "\"path\":\"/greet\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "\"url\":\"/greet?lang=en\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "\"lang\":\"en\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "ignored") == null);
}

test "workflow.follow to an unmounted href fails soft as a 599" {
    const allocator = std.testing.allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "payments",
        "function handler(req) { return Response.json({ ok: true }); }",
        "<payments>",
        .{ .jit_policy = .disabled },
        1,
    );

    const orchestrator_src =
        \\import { follow } from "zigttp:workflow";
        \\function handler(req) {
        \\  const r = resource({ id: 1 }, { gone: { href: "/nope/x", method: "GET" } });
        \\  return follow(r, "gone");
        \\}
    ;
    var orch = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled, .system_registry = @ptrCast(&sys) },
        orchestrator_src,
        "<orchestrator>",
        1,
        0,
    );
    defer orch.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{ .method = "GET", .url = "/", .path = "/", .headers = headers, .body = null };

    var handle = try orch.executeHandlerBorrowed(view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 599), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "UnknownRoute") != null);
}

test "workflow.follow on a missing affordance rel fails soft as a 599" {
    const allocator = std.testing.allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "payments",
        "function handler(req) { return Response.json({ ok: true }); }",
        "<payments>",
        .{ .jit_policy = .disabled },
        1,
    );

    const orchestrator_src =
        \\import { follow } from "zigttp:workflow";
        \\function handler(req) {
        \\  const r = resource({ id: 1 }, { pay: { href: "/payments/x", method: "POST" } });
        \\  return follow(r, "nonexistent");
        \\}
    ;
    var orch = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled, .system_registry = @ptrCast(&sys) },
        orchestrator_src,
        "<orchestrator>",
        1,
        0,
    );
    defer orch.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{ .method = "GET", .url = "/", .path = "/", .headers = headers, .body = null };

    var handle = try orch.executeHandlerBorrowed(view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 599), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "NoSuchAffordance") != null);
}

test "workflow.follow substitutes {param} from init.params before dispatch" {
    const allocator = std.testing.allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "orders",
        "function handler(req) { return Response.json({ from: 'orders', at: req.url }); }",
        "<orders>",
        .{ .jit_policy = .disabled },
        1,
    );

    // A templated affordance href; follow fills {id} from init.params, so the
    // orders handler receives the substituted path (not a literal "/orders/{id}").
    const orchestrator_src =
        \\import { follow } from "zigttp:workflow";
        \\function handler(req) {
        \\  const r = resource({ id: 1 }, { item: { href: "/orders/{id}", method: "GET" } });
        \\  return follow(r, "item", { params: { id: "42" } });
        \\}
    ;
    var orch = try HandlerPool.init(allocator, .{ .jit_policy = .disabled, .system_registry = @ptrCast(&sys) }, orchestrator_src, "<orchestrator>", 1, 0);
    defer orch.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{ .method = "GET", .url = "/", .path = "/", .headers = headers, .body = null };

    var handle = try orch.executeHandlerBorrowed(view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 200), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "/orders/42") != null);
    // The literal placeholder must NOT survive to the target.
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "{id}") == null);
}

test "workflow.follow percent-encodes templated params before dispatch" {
    const allocator = std.testing.allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "orders",
        "function handler(req) { return Response.json({ at: req.url, path: req.path }); }",
        "<orders>",
        .{ .jit_policy = .disabled },
        1,
    );

    const orchestrator_src =
        \\import { follow } from "zigttp:workflow";
        \\function handler(req) {
        \\  const r = resource({ id: 1 }, { item: { href: "/orders/{id}", method: "GET" } });
        \\  return follow(r, "item", { params: { id: "42/refund?admin=1#frag" } });
        \\}
    ;
    var orch = try HandlerPool.init(allocator, .{ .jit_policy = .disabled, .system_registry = @ptrCast(&sys) }, orchestrator_src, "<orchestrator>", 1, 0);
    defer orch.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{ .method = "GET", .url = "/", .path = "/", .headers = headers, .body = null };

    var handle = try orch.executeHandlerBorrowed(view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 200), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "/orders/42%2Frefund%3Fadmin%3D1%23frag") != null);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "/orders/42/refund") == null);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "admin=1") == null);
}

test "workflow.follow on a templated href with no params fails soft as a 599" {
    const allocator = std.testing.allocator;

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "orders",
        "function handler(req) { return Response.json({ ok: true }); }",
        "<orders>",
        .{ .jit_policy = .disabled },
        1,
    );

    const orchestrator_src =
        \\import { follow } from "zigttp:workflow";
        \\function handler(req) {
        \\  const r = resource({ id: 1 }, { item: { href: "/orders/{id}", method: "GET" } });
        \\  return follow(r, "item");
        \\}
    ;
    var orch = try HandlerPool.init(allocator, .{ .jit_policy = .disabled, .system_registry = @ptrCast(&sys) }, orchestrator_src, "<orchestrator>", 1, 0);
    defer orch.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{ .method = "GET", .url = "/", .path = "/", .headers = headers, .body = null };

    var handle = try orch.executeHandlerBorrowed(view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 599), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "MissingTemplateParam") != null);
}

test "buildFromSystemConfig fails when a handler source file is unreadable" {
    const allocator = std.testing.allocator;

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(io, &dir_buf);
    const dir = dir_buf[0..dir_len];

    const ok_path = try std.fmt.allocPrint(allocator, "{s}/ok.ts", .{dir});
    defer allocator.free(ok_path);
    try zq.file_io.writeFile(allocator, ok_path, "function handler(req) { return Response.json({ ok: true }); }");

    // Manifest references the valid handler plus one whose source file is absent.
    const manifest = try std.fmt.allocPrint(allocator,
        \\{{ "version": 1, "handlers": [
        \\  {{ "name": "ok", "path": "{s}/ok.ts", "baseUrl": "https://ok.internal" }},
        \\  {{ "name": "gone", "path": "{s}/missing.ts", "baseUrl": "https://gone.internal" }}
        \\] }}
    , .{ dir, dir });
    defer allocator.free(manifest);
    const system_path = try std.fmt.allocPrint(allocator, "{s}/system.json", .{dir});
    defer allocator.free(system_path);
    try zq.file_io.writeFile(allocator, system_path, manifest);

    try std.testing.expectError(
        error.FileNotFound,
        SystemRuntime.buildFromSystemConfig(allocator, system_path, .{ .jit_policy = .disabled }, 1),
    );
}

test "buildFromSystemConfig resolves handler paths relative to the manifest" {
    const allocator = std.testing.allocator;

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(io, &dir_buf);
    const dir = dir_buf[0..dir_len];

    const handler_path = try std.fmt.allocPrint(allocator, "{s}/rel.ts", .{dir});
    defer allocator.free(handler_path);
    try zq.file_io.writeFile(allocator, handler_path, "function handler(req) { return Response.json({ ok: true }); }");

    const manifest = try std.fmt.allocPrint(allocator,
        \\{{ "version": 1, "handlers": [
        \\  {{ "name": "rel", "path": "rel.ts", "baseUrl": "https://rel.internal" }}
        \\] }}
    , .{});
    defer allocator.free(manifest);
    const system_path = try std.fmt.allocPrint(allocator, "{s}/system.json", .{dir});
    defer allocator.free(system_path);
    try zq.file_io.writeFile(allocator, system_path, manifest);

    var sys = try SystemRuntime.buildFromSystemConfig(allocator, system_path, .{ .jit_policy = .disabled }, 1);
    defer sys.deinit();

    try std.testing.expectEqual(@as(usize, 1), sys.targets.items.len);
    try std.testing.expect(sys.find("rel") != null);
}

test "buildFromSystemConfig applies each target's contract-derived egress policy" {
    const allocator = std.testing.allocator;

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(io, &dir_buf);
    const dir = dir_buf[0..dir_len];

    const entry_path = try std.fmt.allocPrint(allocator, "{s}/entry.ts", .{dir});
    defer allocator.free(entry_path);
    try zq.file_io.writeFile(
        allocator,
        entry_path,
        "function handler(req) { return fetchSync('http://localhost:1'); }",
    );

    const target_path = try std.fmt.allocPrint(allocator, "{s}/target.ts", .{dir});
    defer allocator.free(target_path);
    try zq.file_io.writeFile(
        allocator,
        target_path,
        "function handler(req) { const request = fetchSync; return request('http://localhost:1'); }",
    );

    const manifest = try std.fmt.allocPrint(allocator,
        \\{{ "version": 1, "handlers": [
        \\  {{ "name": "entry", "path": "{s}", "baseUrl": "https://entry.internal" }},
        \\  {{ "name": "target", "path": "{s}", "baseUrl": "https://target.internal" }}
        \\] }}
    , .{ entry_path, target_path });
    defer allocator.free(manifest);
    const system_path = try std.fmt.allocPrint(allocator, "{s}/system.json", .{dir});
    defer allocator.free(system_path);
    try zq.file_io.writeFile(allocator, system_path, manifest);

    var sys = try SystemRuntime.buildFromSystemConfig(
        allocator,
        system_path,
        .{
            .jit_policy = .disabled,
            .outbound_http_enabled = true,
            .dev_capability_policy = .{
                .egress = .{ .enabled = true, .values = &[_][]const u8{"localhost"} },
            },
        },
        1,
    );
    defer sys.deinit();

    var headers: std.ArrayListUnmanaged(http_types.HttpHeader) = .empty;
    defer headers.deinit(allocator);
    const view = HttpRequestView{
        .method = "GET",
        .url = "/",
        .path = "/",
        .headers = headers,
        .body = null,
    };

    var handle = try sys.dispatch("target", view);
    defer handle.deinit();
    try std.testing.expectEqual(@as(u16, 599), handle.response.status);
    try std.testing.expect(std.mem.indexOf(u8, handle.response.body, "HostNotAllowed") != null);
}

test "buildFromSystemConfig includes imported capabilities in each target policy" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(io, &dir_buf);
    const dir = dir_buf[0..dir_len];

    const dependency_path = try std.fmt.allocPrint(allocator, "{s}/dependency.ts", .{dir});
    defer allocator.free(dependency_path);
    try zq.file_io.writeFile(
        allocator,
        dependency_path,
        "export function request() { return fetchSync('http://localhost:1'); }",
    );

    const handler_path = try std.fmt.allocPrint(allocator, "{s}/entry.ts", .{dir});
    defer allocator.free(handler_path);
    try zq.file_io.writeFile(
        allocator,
        handler_path,
        "import { request } from './dependency.ts'; function handler(req) { return request(req, undefined); }",
    );

    const manifest = try std.fmt.allocPrint(allocator,
        \\{{ "version": 1, "handlers": [
        \\  {{ "name": "entry", "path": "{s}", "baseUrl": "https://entry.internal" }}
        \\] }}
    , .{handler_path});
    defer allocator.free(manifest);
    const system_path = try std.fmt.allocPrint(allocator, "{s}/system.json", .{dir});
    defer allocator.free(system_path);
    try zq.file_io.writeFile(allocator, system_path, manifest);

    var sys = try SystemRuntime.buildFromSystemConfig(
        allocator,
        system_path,
        .{
            .jit_policy = .disabled,
            .outbound_http_enabled = true,
            .dev_capability_policy = .{
                .egress = .{ .enabled = true, .values = &[_][]const u8{"denied.example"} },
            },
        },
        1,
    );
    defer sys.deinit();

    const target = sys.find("entry") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 1), target.contract.egress.hosts.items.len);
    try std.testing.expectEqualStrings("localhost", target.contract.egress.hosts.items[0]);
}

test "buildFromSystemConfig rejects a target whose contract cannot compile" {
    const allocator = std.testing.allocator;

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(io, &dir_buf);
    const dir = dir_buf[0..dir_len];

    const handler_path = try std.fmt.allocPrint(allocator, "{s}/broken.ts", .{dir});
    defer allocator.free(handler_path);
    try zq.file_io.writeFile(allocator, handler_path, "function handler(");

    const manifest = try std.fmt.allocPrint(allocator,
        \\{{ "version": 1, "handlers": [
        \\  {{ "name": "broken", "path": "{s}", "baseUrl": "https://broken.internal" }}
        \\] }}
    , .{handler_path});
    defer allocator.free(manifest);
    const system_path = try std.fmt.allocPrint(allocator, "{s}/system.json", .{dir});
    defer allocator.free(system_path);
    try zq.file_io.writeFile(allocator, system_path, manifest);

    try std.testing.expectError(
        error.UnexpectedToken,
        SystemRuntime.buildFromSystemConfig(allocator, system_path, .{ .jit_policy = .disabled }, 1),
    );
}
