//! In-process edge runtime for routing one listener to many handler pools.

const std = @import("std");
const zq = @import("zts");
const compat = @import("zts").compat;
const zruntime = @import("zruntime.zig");
const http_parser = @import("http_parser.zig");
const http_types = @import("http_types.zig");
const contract_runtime = @import("contract_runtime.zig");
const response_mod = @import("server_response.zig");
const io_mod = @import("server_io.zig");
const runtime_natives = @import("runtime_natives.zig");

const Io = std.Io;
const net = std.Io.net;
const HandlerPool = zruntime.HandlerPool;
const RuntimeConfig = zruntime.RuntimeConfig;
const HttpHeader = http_types.HttpHeader;
const HttpRequestView = http_types.HttpRequestView;
const HttpResponse = http_types.HttpResponse;
const QueryParam = http_types.QueryParam;

const readFilePosix = zq.file_io.readFile;

pub const LoadPolicy = enum {
    round_robin,
    least_busy,

    fn parse(value: []const u8) !LoadPolicy {
        if (std.mem.eql(u8, value, "round_robin") or std.mem.eql(u8, value, "round-robin")) return .round_robin;
        if (std.mem.eql(u8, value, "least_busy") or std.mem.eql(u8, value, "least-busy")) return .least_busy;
        return error.InvalidEdgeConfig;
    }
};

pub const ListenerConfig = struct {
    host: []const u8,
    port: u16,
    protocol: Protocol = .http,

    pub const Protocol = enum {
        http,
        https,
    };
};

pub const HandlerConfig = struct {
    name: []const u8,
    entry: []const u8,
    pool_size: usize = 0,
    pool_wait_timeout_ms: u32 = 5000,
    lifecycle: ?contract_runtime.PoolingPolicy = null,
    runtime_config: RuntimeConfig = .{},
};

pub const TargetConfig = struct {
    handler: []const u8,
    weight: u32 = 1,
};

pub const RouteConfig = struct {
    host: []const u8 = "*",
    method: []const u8 = "*",
    path_prefix: []const u8 = "/",
    targets: []TargetConfig,
    policy: LoadPolicy = .least_busy,
};

pub const EdgeConfig = struct {
    listener: ListenerConfig,
    handlers: []HandlerConfig,
    routes: []RouteConfig,
    max_body_size: usize = 1024 * 1024,
    max_headers: usize = 64,
    timeout_ms: u32 = 30_000,

    pub fn deinit(self: *EdgeConfig, allocator: std.mem.Allocator) void {
        allocator.free(self.listener.host);
        for (self.handlers) |*handler| {
            allocator.free(handler.name);
            allocator.free(handler.entry);
            if (handler.runtime_config.outbound_allow_host) |host| allocator.free(host);
            if (handler.runtime_config.sqlite_path) |path| allocator.free(path);
            if (handler.runtime_config.system_config_path) |path| allocator.free(path);
            if (handler.runtime_config.durable_oplog_dir) |path| allocator.free(path);
        }
        allocator.free(self.handlers);
        for (self.routes) |*route| {
            allocator.free(route.host);
            allocator.free(route.method);
            allocator.free(route.path_prefix);
            for (route.targets) |target| allocator.free(target.handler);
            allocator.free(route.targets);
        }
        allocator.free(self.routes);
    }
};

pub fn loadConfig(allocator: std.mem.Allocator, path: []const u8) !EdgeConfig {
    const bytes = try readFilePosix(allocator, path, 1024 * 1024);
    defer allocator.free(bytes);
    const root_dir = std.fs.path.dirname(path) orelse ".";
    return parseConfig(allocator, bytes, root_dir);
}

pub fn parseConfig(allocator: std.mem.Allocator, bytes: []const u8, root_dir: []const u8) !EdgeConfig {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, bytes, .{});
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidEdgeConfig;
    const obj = parsed.value.object;
    const listener = try parseListener(allocator, obj.get("listener"));
    const handlers = try parseHandlers(allocator, obj.get("handlers"), root_dir);
    errdefer {
        var tmp = EdgeConfig{
            .listener = listener,
            .handlers = handlers,
            .routes = &.{},
        };
        tmp.deinit(allocator);
    }
    const timeout_ms = try parseU32Field(obj, "timeoutMs", 30_000);
    for (handlers) |*handler| {
        if (handler.runtime_config.request_timeout_ms == 0) {
            handler.runtime_config.request_timeout_ms = timeout_ms;
        }
    }

    const routes = try parseRoutes(allocator, obj.get("routes"));
    errdefer {
        for (routes) |*route| {
            allocator.free(route.host);
            allocator.free(route.method);
            allocator.free(route.path_prefix);
            for (route.targets) |target| allocator.free(target.handler);
            allocator.free(route.targets);
        }
        allocator.free(routes);
    }

    return .{
        .listener = listener,
        .handlers = handlers,
        .routes = routes,
        .max_body_size = try parseUsizeField(obj, "maxBodySize", 1024 * 1024),
        .max_headers = try parseUsizeField(obj, "maxHeaders", 64),
        .timeout_ms = timeout_ms,
    };
}

pub const EdgeServer = struct {
    allocator: std.mem.Allocator,
    config: EdgeConfig,
    io_backend: Io.Threaded,
    listener: ?net.Server = null,
    targets: []TargetRuntime = &.{},
    routes: []RouteRuntime = &.{},
    running: bool = false,
    request_count: std.atomic.Value(u64) = std.atomic.Value(u64).init(0),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, config: EdgeConfig) !Self {
        var io_backend = Io.Threaded.init(allocator, .{ .environ = .empty });
        errdefer io_backend.deinit();

        var targets = try allocator.alloc(TargetRuntime, config.handlers.len);
        errdefer allocator.free(targets);
        var target_count: usize = 0;
        errdefer {
            for (targets[0..target_count]) |*target| target.deinit(allocator);
        }

        for (config.handlers) |handler_cfg| {
            targets[target_count] = try TargetRuntime.init(allocator, handler_cfg);
            target_count += 1;
        }

        const routes = try buildRouteTable(allocator, config.routes, targets);
        errdefer {
            for (routes) |*route| route.deinit(allocator);
            allocator.free(routes);
        }

        return .{
            .allocator = allocator,
            .config = config,
            .io_backend = io_backend,
            .targets = targets,
            .routes = routes,
        };
    }

    pub fn deinit(self: *Self) void {
        self.running = false;
        if (self.listener) |*listener| listener.deinit(self.io_backend.io());
        for (self.routes) |*route| route.deinit(self.allocator);
        self.allocator.free(self.routes);
        for (self.targets) |*target| target.deinit(self.allocator);
        self.allocator.free(self.targets);
        self.io_backend.deinit();
        self.config.deinit(self.allocator);
    }

    pub fn run(self: *Self) !void {
        try self.start();
        try self.acceptLoop();
    }

    pub fn start(self: *Self) !void {
        if (self.config.listener.protocol == .https) return error.TlsTerminationNotImplemented;

        const io = self.io_backend.io();
        const address = try net.IpAddress.parseIp4(self.config.listener.host, self.config.listener.port);
        self.listener = try address.listen(io, .{ .reuse_address = true });
        self.running = true;
        std.log.info("Edge listening on http://{s}:{d}", .{ self.config.listener.host, self.config.listener.port });
        std.log.info("   Handlers: {d}", .{self.targets.len});
        std.log.info("   Routes: {d}", .{self.routes.len});
    }

    fn acceptLoop(self: *Self) !void {
        const io = self.io_backend.io();
        var listener = self.listener orelse return error.NotStarted;
        while (self.running) {
            const stream = listener.accept(io) catch |err| {
                if (err == error.ConnectionAborted) continue;
                return err;
            };
            const fd = stream.socket.handle;
            const thread = std.Thread.spawn(.{}, connectionThread, .{ self, fd }) catch {
                std.Io.Threaded.closeFd(fd);
                continue;
            };
            thread.detach();
        }
    }

    fn connectionThread(self: *Self, fd: std.posix.fd_t) void {
        defer std.Io.Threaded.closeFd(fd);
        std.posix.setsockopt(fd, std.posix.IPPROTO.TCP, std.posix.TCP.NODELAY, &std.mem.toBytes(@as(c_int, 1))) catch {};

        // Bound slow clients so a partial-header connection cannot hold this
        // thread open indefinitely (the read path drops it on WouldBlock).
        const recv_timeout = std.posix.timeval{
            .sec = @intCast(self.config.timeout_ms / 1000),
            .usec = @intCast((self.config.timeout_ms % 1000) * 1000),
        };
        std.posix.setsockopt(fd, std.posix.SOL.SOCKET, std.posix.SO.RCVTIMEO, std.mem.asBytes(&recv_timeout)) catch {};
        std.posix.setsockopt(fd, std.posix.SOL.SOCKET, std.posix.SO.SNDTIMEO, std.mem.asBytes(&recv_timeout)) catch {};

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        const request_data = readRequestData(fd, allocator, self.config.max_body_size, self.config.timeout_ms) catch |err| {
            if (err == error.UnsupportedTransferEncoding) sendError(fd, 501, "Not Implemented") catch {};
            if (err == error.FileTooBig) sendError(fd, 413, "Payload Too Large") catch {};
            if (err == error.RequestTimeout or err == error.WouldBlock) sendError(fd, 408, "Request Timeout") catch {};
            return;
        };
        var parsed = parseRequest(allocator, request_data, self.config.max_headers, self.config.max_body_size) catch |err| {
            if (err == error.UnsupportedTransferEncoding) {
                sendError(fd, 501, "Not Implemented") catch {};
            } else {
                sendError(fd, 400, "Bad Request") catch {};
            }
            return;
        };
        defer parsed.deinit(allocator);

        if (std.mem.eql(u8, parsed.request.path, "/healthz")) {
            sendStatic(fd, 200, "OK", "ok\n") catch {};
            return;
        }
        if (std.mem.eql(u8, parsed.request.path, "/readyz")) {
            sendStatic(fd, 200, "OK", "ready\n") catch {};
            return;
        }

        const route = self.matchRoute(parsed.request.method, requestHost(parsed.request.headers.items), parsed.request.path) orelse {
            sendError(fd, 404, "Not Found") catch {};
            return;
        };
        const target = route.selectTarget();
        var handle = target.pool.executeHandlerBorrowed(parsed.request) catch |err| {
            const status = statusForHandlerError(err);
            const message = messageForHandlerError(status);
            sendError(fd, status, message) catch {};
            return;
        };
        defer handle.deinit();

        handle.response.putHeaderBorrowed("X-Zttp-Edge-Target", target.name) catch {};
        sendResponse(fd, &handle.response) catch {};
        _ = self.request_count.fetchAdd(1, .monotonic);
    }

    pub fn matchRoute(self: *Self, method: []const u8, host: []const u8, path: []const u8) ?*RouteRuntime {
        return selectRoute(self.routes, method, host, path);
    }
};

fn statusForHandlerError(err: anyerror) u16 {
    return if (err == error.PoolExhausted)
        503
    else if (err == error.RequestTimeout)
        504
    else
        500;
}

fn messageForHandlerError(status: u16) []const u8 {
    return switch (status) {
        503 => "Service Unavailable",
        504 => "Gateway Timeout",
        else => "Internal Server Error",
    };
}

const TargetRuntime = struct {
    name: []const u8,
    handler_code: []const u8,
    entry: []const u8,
    pool: HandlerPool,

    fn init(allocator: std.mem.Allocator, cfg: HandlerConfig) !TargetRuntime {
        const handler_code = try readFilePosix(allocator, cfg.entry, 10 * 1024 * 1024);
        errdefer allocator.free(handler_code);

        const pool_size = if (cfg.pool_size == 0) defaultPoolSize() else cfg.pool_size;
        var pool = try HandlerPool.initWithEmbeddedAndDeps(
            allocator,
            cfg.runtime_config,
            handler_code,
            cfg.entry,
            pool_size,
            cfg.pool_wait_timeout_ms,
            null,
            null,
        );
        errdefer pool.deinit();
        if (cfg.lifecycle) |policy| pool.setPoolingPolicy(policy);

        return .{
            .name = cfg.name,
            .handler_code = handler_code,
            .entry = cfg.entry,
            .pool = pool,
        };
    }

    fn deinit(self: *TargetRuntime, allocator: std.mem.Allocator) void {
        self.pool.deinit();
        allocator.free(self.handler_code);
    }
};

const WeightedTarget = struct {
    target: *TargetRuntime,
    weight: u32,
};

const RouteRuntime = struct {
    host: []const u8,
    method: []const u8,
    path_prefix: []const u8,
    policy: LoadPolicy,
    targets: []WeightedTarget,
    cursor: std.atomic.Value(usize),

    fn deinit(self: *RouteRuntime, allocator: std.mem.Allocator) void {
        allocator.free(self.targets);
    }

    fn selectTarget(self: *RouteRuntime) *TargetRuntime {
        if (self.targets.len == 1) return self.targets[0].target;
        return switch (self.policy) {
            .least_busy => self.selectLeastBusy(),
            .round_robin => self.selectRoundRobin(),
        };
    }

    fn selectRoundRobin(self: *RouteRuntime) *TargetRuntime {
        const total = totalWeight(self.targets);
        const next = self.cursor.fetchAdd(1, .acq_rel) % total;
        var offset: usize = 0;
        for (self.targets) |candidate| {
            offset += @max(candidate.weight, 1);
            if (next < offset) return candidate.target;
        }
        return self.targets[0].target;
    }

    fn selectLeastBusy(self: *RouteRuntime) *TargetRuntime {
        var best = self.targets[0].target;
        var best_load = best.pool.getInUse();
        for (self.targets[1..]) |candidate| {
            const load = candidate.target.pool.getInUse();
            if (load < best_load) {
                best = candidate.target;
                best_load = load;
            }
        }
        return best;
    }
};

fn buildRouteTable(allocator: std.mem.Allocator, configs: []RouteConfig, targets: []TargetRuntime) ![]RouteRuntime {
    var routes = try allocator.alloc(RouteRuntime, configs.len);
    errdefer allocator.free(routes);
    var count: usize = 0;
    errdefer {
        for (routes[0..count]) |*route| route.deinit(allocator);
    }

    for (configs) |cfg| {
        var weighted = try allocator.alloc(WeightedTarget, cfg.targets.len);
        errdefer allocator.free(weighted);
        for (cfg.targets, 0..) |target_cfg, i| {
            weighted[i] = .{
                .target = findTarget(targets, target_cfg.handler) orelse return error.UnknownEdgeTarget,
                .weight = @max(target_cfg.weight, 1),
            };
        }
        routes[count] = .{
            .host = cfg.host,
            .method = cfg.method,
            .path_prefix = cfg.path_prefix,
            .policy = cfg.policy,
            .targets = weighted,
            .cursor = std.atomic.Value(usize).init(0),
        };
        count += 1;
    }
    sortRoutes(routes);
    return routes;
}

fn selectRoute(routes: []RouteRuntime, method: []const u8, host: []const u8, path: []const u8) ?*RouteRuntime {
    for (routes) |*route| {
        if (!matchesMethod(route.method, method)) continue;
        if (!matchesHost(route.host, host)) continue;
        if (!matchesPathPrefix(route.path_prefix, path)) continue;
        return route;
    }
    return null;
}

fn sortRoutes(routes: []RouteRuntime) void {
    std.mem.sort(RouteRuntime, routes, {}, struct {
        fn lessThan(_: void, a: RouteRuntime, b: RouteRuntime) bool {
            if (a.host.len != b.host.len) return a.host.len > b.host.len;
            if (a.path_prefix.len != b.path_prefix.len) return a.path_prefix.len > b.path_prefix.len;
            if (!std.mem.eql(u8, a.method, "*") and std.mem.eql(u8, b.method, "*")) return true;
            return false;
        }
    }.lessThan);
}

fn findTarget(targets: []TargetRuntime, name: []const u8) ?*TargetRuntime {
    for (targets) |*target| {
        if (std.mem.eql(u8, target.name, name)) return target;
    }
    return null;
}

fn matchesMethod(route_method: []const u8, method: []const u8) bool {
    return std.mem.eql(u8, route_method, "*") or std.ascii.eqlIgnoreCase(route_method, method);
}

fn matchesHost(route_host: []const u8, host: []const u8) bool {
    return std.mem.eql(u8, route_host, "*") or std.ascii.eqlIgnoreCase(route_host, host);
}

fn matchesPathPrefix(prefix: []const u8, path: []const u8) bool {
    if (!std.mem.startsWith(u8, path, prefix)) return false;
    if (std.mem.eql(u8, prefix, "/")) return true;
    if (path.len == prefix.len) return true;
    return prefix[prefix.len - 1] == '/' or path[prefix.len] == '/';
}

fn totalWeight(targets: []WeightedTarget) usize {
    var total: usize = 0;
    for (targets) |target| total += @max(target.weight, 1);
    return total;
}

fn parseListener(allocator: std.mem.Allocator, value_opt: ?std.json.Value) !ListenerConfig {
    const value = value_opt orelse return .{
        .host = try allocator.dupe(u8, "127.0.0.1"),
        .port = 8443,
    };
    if (value != .object) return error.InvalidEdgeConfig;
    const obj = value.object;
    const protocol_str = try stringField(obj, "protocol", "http");
    const protocol: ListenerConfig.Protocol = if (std.mem.eql(u8, protocol_str, "http"))
        .http
    else if (std.mem.eql(u8, protocol_str, "https"))
        .https
    else
        return error.InvalidEdgeConfig;
    return .{
        .host = try dupStringField(allocator, obj, "host", "127.0.0.1"),
        .port = try parseU16Field(obj, "port", 8443),
        .protocol = protocol,
    };
}

fn parseHandlers(allocator: std.mem.Allocator, value_opt: ?std.json.Value, root_dir: []const u8) ![]HandlerConfig {
    const value = value_opt orelse return error.InvalidEdgeConfig;
    if (value != .array) return error.InvalidEdgeConfig;
    const arr = value.array.items;
    if (arr.len == 0) return error.InvalidEdgeConfig;

    var handlers = try allocator.alloc(HandlerConfig, arr.len);
    errdefer allocator.free(handlers);
    var count: usize = 0;
    errdefer {
        for (handlers[0..count]) |*handler| {
            allocator.free(handler.name);
            allocator.free(handler.entry);
            if (handler.runtime_config.outbound_allow_host) |host| allocator.free(host);
            if (handler.runtime_config.system_config_path) |path| allocator.free(path);
        }
    }

    for (arr) |item| {
        if (item != .object) return error.InvalidEdgeConfig;
        const obj = item.object;

        // Resolve entry inside a block so entry_raw is freed exactly once (by
        // the defer) whether we break out or error. A loop-wide errdefer plus a
        // later explicit free double-frees entry_raw when a subsequent `try`
        // fails; each owned value below gets its own errdefer so a mid-iteration
        // error frees only what this (not-yet-committed) iteration allocated.
        const entry = blk: {
            const entry_raw = try dupStringField(allocator, obj, "entry", "");
            defer allocator.free(entry_raw);
            if (entry_raw.len == 0) return error.InvalidEdgeConfig;
            break :blk if (std.fs.path.isAbsolute(entry_raw))
                try allocator.dupe(u8, entry_raw)
            else
                try std.fs.path.resolve(allocator, &.{ root_dir, entry_raw });
        };
        errdefer allocator.free(entry);

        var runtime_config = RuntimeConfig{};
        runtime_config.outbound_http_enabled = try parseBoolField(obj, "outboundHttp", false);
        runtime_config.outbound_allow_host = try dupOptionalStringField(allocator, obj, "outboundHost");
        errdefer if (runtime_config.outbound_allow_host) |h| allocator.free(h);
        if (runtime_config.outbound_allow_host != null) runtime_config.outbound_http_enabled = true;
        runtime_config.outbound_timeout_ms = try parseU32Field(obj, "outboundTimeoutMs", 10_000);
        runtime_config.system_config_path = try dupOptionalResolvedPath(allocator, obj, "system", root_dir);
        errdefer if (runtime_config.system_config_path) |p| allocator.free(p);

        const name = try dupStringField(allocator, obj, "name", "");
        errdefer allocator.free(name);
        if (name.len == 0) return error.InvalidEdgeConfig;

        handlers[count] = .{
            .name = name,
            .entry = entry,
            .pool_size = try parseUsizeField(obj, "pool", 0),
            .pool_wait_timeout_ms = try parseU32Field(obj, "poolWaitTimeoutMs", 5000),
            .lifecycle = try parseOptionalLifecycle(obj.get("lifecycle")),
            .runtime_config = runtime_config,
        };
        count += 1;
    }
    return handlers;
}

fn parseRoutes(allocator: std.mem.Allocator, value_opt: ?std.json.Value) ![]RouteConfig {
    const value = value_opt orelse return error.InvalidEdgeConfig;
    if (value != .array) return error.InvalidEdgeConfig;
    const arr = value.array.items;
    if (arr.len == 0) return error.InvalidEdgeConfig;
    var routes = try allocator.alloc(RouteConfig, arr.len);
    errdefer allocator.free(routes);
    var count: usize = 0;
    errdefer {
        for (routes[0..count]) |*route| {
            allocator.free(route.host);
            allocator.free(route.method);
            allocator.free(route.path_prefix);
            for (route.targets) |target| allocator.free(target.handler);
            allocator.free(route.targets);
        }
    }

    for (arr) |item| {
        if (item != .object) return error.InvalidEdgeConfig;
        const obj = item.object;
        routes[count] = .{
            .host = try dupStringField(allocator, obj, "host", "*"),
            .method = try dupStringField(allocator, obj, "method", "*"),
            .path_prefix = try dupStringField(allocator, obj, "pathPrefix", "/"),
            .targets = try parseTargets(allocator, obj),
            .policy = try LoadPolicy.parse(try stringField(obj, "policy", "least_busy")),
        };
        count += 1;
    }
    return routes;
}

fn parseTargets(allocator: std.mem.Allocator, obj: std.json.ObjectMap) ![]TargetConfig {
    if (obj.get("target")) |single| {
        if (single != .string) return error.InvalidEdgeConfig;
        const targets = try allocator.alloc(TargetConfig, 1);
        targets[0] = .{ .handler = try allocator.dupe(u8, single.string), .weight = 1 };
        return targets;
    }

    const value = obj.get("targets") orelse return error.InvalidEdgeConfig;
    if (value != .array) return error.InvalidEdgeConfig;
    const arr = value.array.items;
    if (arr.len == 0) return error.InvalidEdgeConfig;
    var targets = try allocator.alloc(TargetConfig, arr.len);
    errdefer allocator.free(targets);
    var count: usize = 0;
    errdefer {
        for (targets[0..count]) |target| allocator.free(target.handler);
    }

    for (arr) |item| {
        if (item == .string) {
            targets[count] = .{ .handler = try allocator.dupe(u8, item.string), .weight = 1 };
        } else if (item == .object) {
            targets[count] = .{
                .handler = try dupStringField(allocator, item.object, "handler", ""),
                .weight = try parseU32Field(item.object, "weight", 1),
            };
            if (targets[count].handler.len == 0) return error.InvalidEdgeConfig;
        } else {
            return error.InvalidEdgeConfig;
        }
        count += 1;
    }
    return targets;
}

fn parseOptionalLifecycle(value_opt: ?std.json.Value) !?contract_runtime.PoolingPolicy {
    const value = value_opt orelse return null;
    if (value != .string) return error.InvalidEdgeConfig;
    if (std.mem.eql(u8, value.string, "ephemeral")) return .ephemeral;
    if (std.mem.eql(u8, value.string, "bounded")) return .reuse_bounded_by_count;
    if (std.mem.eql(u8, value.string, "ttl")) return .reuse_bounded_by_ttl;
    if (std.mem.eql(u8, value.string, "reuse")) return .reuse_unbounded;
    return error.InvalidEdgeConfig;
}

fn stringField(obj: std.json.ObjectMap, key: []const u8, default: []const u8) ![]const u8 {
    const value = obj.get(key) orelse return default;
    if (value != .string) return error.InvalidEdgeConfig;
    return value.string;
}

fn dupStringField(allocator: std.mem.Allocator, obj: std.json.ObjectMap, key: []const u8, default: []const u8) ![]u8 {
    return allocator.dupe(u8, try stringField(obj, key, default));
}

fn dupOptionalStringField(allocator: std.mem.Allocator, obj: std.json.ObjectMap, key: []const u8) !?[]u8 {
    const value = obj.get(key) orelse return null;
    if (value != .string) return error.InvalidEdgeConfig;
    return try allocator.dupe(u8, value.string);
}

fn dupOptionalResolvedPath(allocator: std.mem.Allocator, obj: std.json.ObjectMap, key: []const u8, root_dir: []const u8) !?[]u8 {
    const raw = try dupOptionalStringField(allocator, obj, key) orelse return null;
    defer allocator.free(raw);
    if (std.fs.path.isAbsolute(raw)) return try allocator.dupe(u8, raw);
    return try std.fs.path.resolve(allocator, &.{ root_dir, raw });
}

fn parseBoolField(obj: std.json.ObjectMap, key: []const u8, default: bool) !bool {
    const value = obj.get(key) orelse return default;
    if (value != .bool) return error.InvalidEdgeConfig;
    return value.bool;
}

fn parseU16Field(obj: std.json.ObjectMap, key: []const u8, default: u16) !u16 {
    const value = try parseU64Field(obj, key, default);
    if (value > std.math.maxInt(u16)) return error.InvalidEdgeConfig;
    return @intCast(value);
}

fn parseU32Field(obj: std.json.ObjectMap, key: []const u8, default: u32) !u32 {
    const value = try parseU64Field(obj, key, default);
    if (value > std.math.maxInt(u32)) return error.InvalidEdgeConfig;
    return @intCast(value);
}

fn parseUsizeField(obj: std.json.ObjectMap, key: []const u8, default: usize) !usize {
    const value = try parseU64Field(obj, key, default);
    if (value > std.math.maxInt(usize)) return error.InvalidEdgeConfig;
    return @intCast(value);
}

fn parseU64Field(obj: std.json.ObjectMap, key: []const u8, default: u64) !u64 {
    const value = obj.get(key) orelse return default;
    if (value != .integer) return error.InvalidEdgeConfig;
    if (value.integer < 0) return error.InvalidEdgeConfig;
    return @intCast(value.integer);
}

const ParsedRequest = struct {
    request: HttpRequestView,
    query_storage: ?[]QueryParam = null,
    query_decoded_storage: ?[]u8 = null,
    body_owned: bool = false,

    fn deinit(self: *ParsedRequest, allocator: std.mem.Allocator) void {
        if (self.body_owned) {
            if (self.request.body) |body| allocator.free(body);
        }
        if (self.query_storage) |storage| allocator.free(storage);
        if (self.query_decoded_storage) |storage| allocator.free(storage);
        self.request.headers.deinit(allocator);
    }
};

fn parseRequest(allocator: std.mem.Allocator, data: []const u8, max_headers: usize, max_body_size: usize) !ParsedRequest {
    const header_end = http_parser.findHeaderEnd(data) orelse return error.InvalidRequest;
    const header_section = data[0..header_end];
    var lines = std.mem.splitSequence(u8, header_section, "\r\n");
    const request_line = lines.next() orelse return error.InvalidRequest;
    const parsed_line = try http_parser.parseRequestLineBorrowed(request_line, http_parser.DEFAULT_MAX_URL_LENGTH);

    var headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
    errdefer headers.deinit(allocator);
    var slots = http_parser.FastHeaderSlots{};
    var header_count: usize = 0;
    while (lines.next()) |line| {
        if (line.len == 0) break;
        if (header_count >= max_headers) return error.TooManyHeaders;
        const header = http_parser.splitHeaderLine(line) orelse continue;
        try headers.append(allocator, .{ .key = header.key, .value = header.value });
        if (std.ascii.eqlIgnoreCase(header.key, "content-length")) {
            const parsed_len = try http_parser.parseContentLengthValue(header.value);
            if (slots.content_length) |existing| {
                if (existing != parsed_len) return error.DuplicateContentLength;
            } else {
                slots.content_length = parsed_len;
            }
        }
        header_count += 1;
    }
    slots.transfer_encoding = try http_parser.parseTransferEncoding(data[0..header_end]);
    const body_start = header_end + 4;
    var body_owned = false;
    const body = if (slots.transfer_encoding == .chunked) blk: {
        if (slots.content_length != null) return error.InvalidRequest;
        const decoded = try http_parser.decodeChunkedBody(allocator, data[body_start..], max_body_size);
        if (decoded.len == 0) {
            allocator.free(decoded);
            break :blk null;
        }
        body_owned = true;
        break :blk decoded;
    } else if (slots.content_length) |len| blk: {
        // Bound the body to Content-Length. The read loop over-reads (it stops
        // once it has *at least* total_needed bytes, and pipelined/extra client
        // bytes land in the same buffer), so handing data[body_start..] verbatim
        // injects trailing bytes into the handler body. Mirror server.zig.
        if (len > max_body_size) return error.FileTooBig;
        if (len == 0) break :blk null;
        const end = body_start + len;
        if (end > data.len) return error.IncompleteBody;
        break :blk data[body_start..end];
    } else null;
    const query = try http_parser.parseQueryString(allocator, parsed_line.query_string, http_parser.DEFAULT_MAX_QUERY_LENGTH);

    return .{
        .request = .{
            .method = parsed_line.method,
            .url = parsed_line.url,
            .path = parsed_line.path,
            .query_params = query.params,
            .headers = headers,
            .body = body,
        },
        .query_storage = query.storage,
        .query_decoded_storage = query.decoded_storage,
        .body_owned = body_owned,
    };
}

fn readRequestData(fd: std.posix.fd_t, allocator: std.mem.Allocator, max_body_size: usize, timeout_ms: u32) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);
    var header_end: ?usize = null;
    var total_needed: ?usize = null;
    var is_chunked = false;
    var chunked_state: http_parser.ChunkedBodyParseState = .{};
    const max_chunked_encoded_body_bytes = http_parser.maxChunkedEncodedBodyBytes(max_body_size);
    var tmp: [8192]u8 = undefined;
    var read_timer = compat.Timer.start() catch null;
    const read_budget_ns: u64 = @as(u64, timeout_ms) * std.time.ns_per_ms;

    while (true) {
        if (read_budget_ns > 0) {
            if (read_timer) |*timer| {
                if (timer.read() > read_budget_ns) return error.RequestTimeout;
            }
        }
        const n = try std.posix.read(fd, &tmp);
        if (n == 0) return error.EndOfStream;
        try buf.appendSlice(allocator, tmp[0..n]);
        if (header_end == null) {
            if (http_parser.findHeaderEnd(buf.items)) |end| {
                header_end = end;
                const content_len_opt = try http_parser.parseContentLength(buf.items[0..end]);
                const transfer_encoding = try http_parser.parseTransferEncoding(buf.items[0..end]);
                is_chunked = transfer_encoding == .chunked;
                if (is_chunked and content_len_opt != null) return error.InvalidRequest;
                const content_len = content_len_opt orelse 0;
                if (content_len > max_body_size) return error.FileTooBig;
                if (is_chunked) {
                    const body = buf.items[end + 4 ..];
                    if (try http_parser.chunkedBodyConsumedResumable(body, max_body_size, &chunked_state)) |encoded_len| {
                        if (encoded_len > max_chunked_encoded_body_bytes) return error.FileTooBig;
                        total_needed = end + 4 + encoded_len;
                    } else if (body.len > max_chunked_encoded_body_bytes) {
                        return error.FileTooBig;
                    }
                } else {
                    total_needed = end + 4 + content_len;
                }
            } else if (buf.items.len > 32 * 1024) {
                return error.InvalidRequest;
            }
        } else if (is_chunked and total_needed == null) {
            const body_start = header_end.? + 4;
            const body = buf.items[body_start..];
            if (try http_parser.chunkedBodyConsumedResumable(body, max_body_size, &chunked_state)) |encoded_len| {
                if (encoded_len > max_chunked_encoded_body_bytes) return error.FileTooBig;
                total_needed = body_start + encoded_len;
            } else if (body.len > max_chunked_encoded_body_bytes) {
                return error.FileTooBig;
            }
        }
        if (total_needed) |needed| {
            if (buf.items.len >= needed) break;
        }
    }
    return buf.toOwnedSlice(allocator);
}

fn requestHost(headers: []const HttpHeader) []const u8 {
    for (headers) |header| {
        if (std.ascii.eqlIgnoreCase(header.key, "host")) return header.value;
    }
    return "";
}

fn sendResponse(fd: std.posix.fd_t, response: *HttpResponse) !void {
    var header_buf: [8192]u8 = undefined;
    var pos: usize = 0;
    const status_line = try std.fmt.bufPrint(header_buf[pos..], "HTTP/1.1 {d} {s}\r\n", .{ response.status, statusText(response.status) });
    pos += status_line.len;
    for (response.headers.items) |header| {
        if (isHopByHopHeader(header.key) or response_mod.isFramingHeader(header.key)) continue;
        const line = try std.fmt.bufPrint(header_buf[pos..], "{s}: {s}\r\n", .{ header.key, header.value });
        pos += line.len;
    }
    const content_len = try std.fmt.bufPrint(header_buf[pos..], "Content-Length: {d}\r\nConnection: close\r\n\r\n", .{response.body.len});
    pos += content_len.len;

    if (response.body.len > 0) {
        var iovecs: [2]std.posix.iovec_const = .{
            .{ .base = header_buf[0..pos].ptr, .len = pos },
            .{ .base = response.body.ptr, .len = response.body.len },
        };
        try io_mod.writevAllFd(fd, &iovecs);
    } else {
        try io_mod.writeAllFd(fd, header_buf[0..pos]);
    }
}

fn sendStatic(fd: std.posix.fd_t, status: u16, reason: []const u8, body: []const u8) !void {
    var buf: [512]u8 = undefined;
    const response = try std.fmt.bufPrint(&buf, "HTTP/1.1 {d} {s}\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {d}\r\nConnection: close\r\n\r\n{s}", .{ status, reason, body.len, body });
    try io_mod.writeAllFd(fd, response);
}

fn sendError(fd: std.posix.fd_t, status: u16, message: []const u8) !void {
    try sendStatic(fd, status, statusText(status), message);
}

fn isHopByHopHeader(key: []const u8) bool {
    return std.ascii.eqlIgnoreCase(key, "connection") or
        std.ascii.eqlIgnoreCase(key, "keep-alive") or
        std.ascii.eqlIgnoreCase(key, "proxy-authenticate") or
        std.ascii.eqlIgnoreCase(key, "proxy-authorization") or
        std.ascii.eqlIgnoreCase(key, "te") or
        std.ascii.eqlIgnoreCase(key, "trailer") or
        std.ascii.eqlIgnoreCase(key, "transfer-encoding") or
        std.ascii.eqlIgnoreCase(key, "upgrade");
}

fn statusText(status: u16) []const u8 {
    return runtime_natives.statusTextFor(status);
}

fn defaultPoolSize() usize {
    const cpu_count = std.Thread.getCpuCount() catch 4;
    return std.math.clamp(cpu_count * 4, 8, 128);
}

test "edge config parses routes and targets" {
    const json =
        \\{
        \\  "listener": {"host":"127.0.0.1","port":9443,"protocol":"http"},
        \\  "timeoutMs": 1234,
        \\  "handlers": [
        \\    {"name":"api","entry":"src/api.ts","pool":2},
        \\    {"name":"web","entry":"src/web.ts"}
        \\  ],
        \\  "routes": [
        \\    {"host":"example.test","method":"GET","pathPrefix":"/api","target":"api"},
        \\    {"pathPrefix":"/","targets":[{"handler":"web","weight":2}]}
        \\  ]
        \\}
    ;
    var config = try parseConfig(std.testing.allocator, json, "/tmp/app");
    defer config.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(u16, 9443), config.listener.port);
    try std.testing.expectEqual(@as(usize, 2), config.handlers.len);
    try std.testing.expectEqualStrings("/tmp/app/src/api.ts", config.handlers[0].entry);
    try std.testing.expectEqual(@as(u32, 1234), config.timeout_ms);
    try std.testing.expectEqual(@as(u32, 1234), config.handlers[0].runtime_config.request_timeout_ms);
    try std.testing.expectEqual(@as(usize, 2), config.routes.len);
    try std.testing.expectEqualStrings("api", config.routes[0].targets[0].handler);
}

test "edge handler timeout maps to gateway timeout" {
    try std.testing.expectEqual(@as(u16, 503), statusForHandlerError(error.PoolExhausted));
    try std.testing.expectEqual(@as(u16, 504), statusForHandlerError(error.RequestTimeout));
    try std.testing.expectEqual(@as(u16, 500), statusForHandlerError(error.HandlerPanic));
    try std.testing.expectEqualStrings("Gateway Timeout", messageForHandlerError(504));
    try std.testing.expectEqualStrings("Gateway Timeout", statusText(504));
    try std.testing.expectEqualStrings("Not Implemented", statusText(501));
    try std.testing.expectEqualStrings("Method Not Allowed", statusText(405));
    try std.testing.expectEqualStrings("Too Many Requests", statusText(429));
    try std.testing.expectEqualStrings("Request Header Fields Too Large", statusText(431));
}

test "route selection prefers specific host and longest prefix" {
    const dummy_pool: HandlerPool = undefined;
    var targets = [_]TargetRuntime{
        .{ .name = "root", .handler_code = "", .entry = "", .pool = dummy_pool },
        .{ .name = "api", .handler_code = "", .entry = "", .pool = dummy_pool },
    };
    var root_targets = [_]WeightedTarget{.{ .target = &targets[0], .weight = 1 }};
    var api_targets = [_]WeightedTarget{.{ .target = &targets[1], .weight = 1 }};
    var routes = [_]RouteRuntime{
        .{
            .host = "*",
            .method = "*",
            .path_prefix = "/",
            .policy = .least_busy,
            .targets = &root_targets,
            .cursor = std.atomic.Value(usize).init(0),
        },
        .{
            .host = "example.test",
            .method = "GET",
            .path_prefix = "/api",
            .policy = .least_busy,
            .targets = &api_targets,
            .cursor = std.atomic.Value(usize).init(0),
        },
    };
    sortRoutes(&routes);
    const route = selectRoute(&routes, "GET", "example.test", "/api/users").?;
    try std.testing.expectEqualStrings("api", route.targets[0].target.name);
}

test "path prefix does not match partial segment" {
    try std.testing.expect(matchesPathPrefix("/api", "/api/users"));
    try std.testing.expect(matchesPathPrefix("/api", "/api"));
    try std.testing.expect(!matchesPathPrefix("/api", "/apix"));
}

test "edge parseRequest decodes chunked request body" {
    const allocator = std.testing.allocator;
    const data =
        "POST /api HTTP/1.1\r\n" ++
        "Host: example.test\r\n" ++
        "Transfer-Encoding: chunked\r\n" ++
        "\r\n" ++
        "5\r\nhello\r\n6\r\n world\r\n0\r\n\r\n";

    var parsed = try parseRequest(allocator, data, 64, 1024);
    defer parsed.deinit(allocator);
    try std.testing.expect(parsed.request.body != null);
    try std.testing.expectEqualStrings("hello world", parsed.request.body.?);
}

test "edge parseRequest rejects unsupported transfer-encoding" {
    const allocator = std.testing.allocator;
    const data =
        "POST /api HTTP/1.1\r\n" ++
        "Host: example.test\r\n" ++
        "Transfer-Encoding: gzip\r\n" ++
        "\r\n" ++
        "hello";

    try std.testing.expectError(error.UnsupportedTransferEncoding, parseRequest(allocator, data, 64, 1024));
}

test "edge readRequestData rejects unsupported transfer-encoding" {
    const allocator = std.testing.allocator;
    const fds = try io_mod.createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);
    defer std.Io.Threaded.closeFd(fds[1]);

    try io_mod.writeAllFd(
        fds[1],
        "POST /api HTTP/1.1\r\nHost: example.test\r\nTransfer-Encoding: gzip\r\n\r\n" ++
            "GET /next HTTP/1.1\r\nHost: example.test\r\n\r\n",
    );

    try std.testing.expectError(
        error.UnsupportedTransferEncoding,
        readRequestData(fds[0], allocator, 1024, 30_000),
    );
}

test "edge readRequestData rejects oversized encoded chunked body" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const max_body_size: usize = 64;

    var request: std.ArrayList(u8) = .empty;
    defer request.deinit(std.testing.allocator);
    try request.appendSlice(
        std.testing.allocator,
        "POST /api HTTP/1.1\r\nHost: example.test\r\nTransfer-Encoding: chunked\r\n\r\n",
    );

    // Each one-byte decoded chunk carries a large extension. The decoded
    // payload stays within max_body_size while its encoded representation
    // exceeds the separate wire-size cap.
    var chunk_index: usize = 0;
    while (chunk_index < max_body_size) : (chunk_index += 1) {
        try request.appendSlice(std.testing.allocator, "1;");
        try request.appendNTimes(std.testing.allocator, 'x', 1100);
        try request.appendSlice(std.testing.allocator, "\r\na\r\n");
    }
    try request.appendSlice(std.testing.allocator, "0\r\n\r\n");
    const body_start = http_parser.findHeaderEnd(request.items).? + 4;
    const encoded_cap = http_parser.maxChunkedEncodedBodyBytes(max_body_size);
    try std.testing.expect(request.items.len - body_start > encoded_cap);

    const fds = try io_mod.createUnixSocketPair();
    var read_fd_open = true;
    defer if (read_fd_open) std.Io.Threaded.closeFd(fds[0]);
    var write_fd_open = true;
    errdefer if (write_fd_open) std.Io.Threaded.closeFd(fds[1]);

    const WriteCtx = struct {
        fd: std.posix.fd_t,
        bytes: []const u8,
    };
    const writer_thread = try std.Thread.spawn(.{}, struct {
        fn run(ctx: WriteCtx) void {
            io_mod.writeAllFd(ctx.fd, ctx.bytes) catch {};
            std.Io.Threaded.closeFd(ctx.fd);
        }
    }.run, .{WriteCtx{ .fd = fds[1], .bytes = request.items }});
    write_fd_open = false;

    const result = readRequestData(fds[0], allocator, max_body_size, 30_000);
    std.Io.Threaded.closeFd(fds[0]);
    read_fd_open = false;
    writer_thread.join();

    try std.testing.expectError(error.FileTooBig, result);
}

test "edge sendResponse drops handler supplied framing headers" {
    const allocator = std.testing.allocator;
    var response = HttpResponse.init(allocator);
    defer response.deinit();

    response.status = 200;
    response.body = "ok";
    try response.putHeader("X-Test", "one");
    try response.putHeader("Content-Length", "999");
    try response.putHeader("Connection", "keep-alive");
    try response.putHeader("Transfer-Encoding", "chunked");

    const fds = try io_mod.createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);
    defer std.Io.Threaded.closeFd(fds[1]);

    try sendResponse(fds[0], &response);
    _ = std.c.shutdown(fds[0], 1);

    var buf: [512]u8 = undefined;
    var total: usize = 0;
    while (total < buf.len) {
        const n = try std.posix.read(fds[1], buf[total..]);
        if (n == 0) break;
        total += n;
    }
    const out = buf[0..total];

    try std.testing.expect(std.mem.indexOf(u8, out, "X-Test: one\r\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Content-Length: 999") == null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Transfer-Encoding: chunked") == null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Connection: keep-alive") == null);
    try std.testing.expect(std.mem.indexOf(u8, out, "Content-Length: 2\r\nConnection: close\r\n\r\nok") != null);
}
