const std = @import("std");
const http = @import("http.zig");
const types = @import("types.zig");
const state = @import("state.zig");
const io_util = @import("io_util.zig");
const printer_mod = @import("printer.zig");

const Printer = printer_mod.Printer;
const Progress = printer_mod.Progress;

pub const Readiness = enum { running, failed, timed_out };

pub const ServiceStatus = enum { pending, running, failed };

// Poll cadence for waitForReady. Kept constant in this wave; Wave 5 may
// surface it as a flag alongside rate-limited progress output.
const poll_interval_ms: u32 = 2000;
pub const default_deadline_seconds: u32 = 120;

pub const ProviderContext = struct {
    name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const types.EnvVar,
    project_id: []const u8,
    plan_id: []const u8,
    registry_credential_id: ?[]const u8,
    state_entry: ?*const state.Entry,
};

// FetchResponse mirrors http.Response but only exposes what execute actually
// reads. The body is owned by the caller-provided allocator so executeWith
// can free it uniformly whether the body came from the real HTTP fetch or a
// test stub.
pub const FetchResponse = struct {
    status: u16,
    body: []u8,
};

pub fn executePutReal(
    allocator: std.mem.Allocator,
    url: []const u8,
    api_token: []const u8,
    body: []const u8,
) !FetchResponse {
    const response = try http.requestJson(allocator, .PUT, url, api_token, body);
    defer response.deinit(allocator);
    return .{
        .status = response.status,
        .body = try allocator.dupe(u8, response.body),
    };
}

pub fn execute(
    allocator: std.mem.Allocator,
    ctx: *const ProviderContext,
    api_token: []const u8,
    progress: ?*Progress,
) !types.DeployResult {
    return executeWith(allocator, ctx, api_token, progress, executePutReal);
}

pub fn executeWith(
    allocator: std.mem.Allocator,
    ctx: *const ProviderContext,
    api_token: []const u8,
    progress: ?*Progress,
    fetch_put: anytype,
) !types.DeployResult {
    if (progress) |p| p.force("  Calling provider...");

    const url = try std.fmt.allocPrint(allocator, "https://api.northflank.com/v1/projects/{s}/services/deployment", .{ctx.project_id});
    defer allocator.free(url);
    const body = try putBodyJson(allocator, ctx);
    defer allocator.free(body);

    const response = try fetch_put(allocator, url, api_token, body);
    defer allocator.free(response.body);
    if (response.status == 401 or response.status == 403) return error.ProviderUnauthorized;
    if (response.status != 200 and response.status != 201) return error.NorthflankDeployFailed;

    return .{
        .provider = .northflank,
        .name = ctx.name,
        .service_id = try parseStringField(allocator, response.body, "id"),
        .deployment_id = null,
        .url = try parseOptionalStringField(allocator, response.body, "ports.0.dns"),
        .status = try allocator.dupe(u8, if (ctx.state_entry != null) "updated" else "created"),
        .image_digest_ref = try allocator.dupe(u8, ctx.image_ref),
    };
}

fn serviceStatusUrl(allocator: std.mem.Allocator, project_id: []const u8, service_id: []const u8) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "https://api.northflank.com/v1/projects/{s}/services/{s}",
        .{ project_id, service_id },
    );
}

pub fn fetchStatusReal(
    allocator: std.mem.Allocator,
    url: []const u8,
    api_token: []const u8,
) !ServiceStatus {
    const response = try http.requestJson(allocator, .GET, url, api_token, null);
    defer response.deinit(allocator);
    if (response.status != 200) return error.NorthflankStatusFailed;
    return parseServiceStatus(allocator, response.body);
}

fn parseServiceStatus(allocator: std.mem.Allocator, body: []const u8) !ServiceStatus {
    const raw = (try parseOptionalStringField(allocator, body, "status")) orelse return .pending;
    defer allocator.free(raw);
    const mapping = [_]struct { name: []const u8, status: ServiceStatus }{
        .{ .name = "running", .status = .running },
        .{ .name = "completed", .status = .running },
        .{ .name = "healthy", .status = .running },
        .{ .name = "failed", .status = .failed },
        .{ .name = "errored", .status = .failed },
        .{ .name = "crashloop", .status = .failed },
    };
    for (mapping) |m| {
        if (std.ascii.eqlIgnoreCase(raw, m.name)) return m.status;
    }
    return .pending;
}

// RealSleeper carries a borrowed std.Io handle so a single threaded-IO backend
// can serve every poll tick. The lifetime is owned by waitForReadyDefault.
pub const RealSleeper = struct {
    io: std.Io,

    pub fn sleep(self: RealSleeper, ms: u32) void {
        std.Io.sleep(self.io, .fromMilliseconds(ms), .awake) catch {};
    }
};

pub fn waitForReadyDefault(
    allocator: std.mem.Allocator,
    ctx: *const ProviderContext,
    service_id: []const u8,
    api_token: []const u8,
    deadline_seconds: u32,
    printer: Printer,
) !Readiness {
    var io_backend = io_util.threadedIo(allocator);
    defer io_backend.deinit();
    const sleeper = RealSleeper{ .io = io_backend.io() };
    return waitForReady(
        allocator,
        ctx,
        service_id,
        api_token,
        deadline_seconds,
        fetchStatusReal,
        sleeper,
        printer,
    );
}

pub fn waitForReady(
    allocator: std.mem.Allocator,
    ctx: *const ProviderContext,
    service_id: []const u8,
    api_token: []const u8,
    deadline_seconds: u32,
    fetch_status: anytype,
    sleeper: anytype,
    printer: Printer,
) !Readiness {
    const url = try serviceStatusUrl(allocator, ctx.project_id, service_id);
    defer allocator.free(url);

    printer.warn("Waiting for service to start...");

    const deadline_ms: u32 = deadline_seconds * 1000;
    var elapsed_ms: u32 = 0;

    while (true) {
        const status = try fetch_status(allocator, url, api_token);
        switch (status) {
            .running => return .running,
            .failed => return .failed,
            .pending => {},
        }
        if (elapsed_ms + poll_interval_ms > deadline_ms) return .timed_out;
        sleeper.sleep(poll_interval_ms);
        elapsed_ms += poll_interval_ms;
    }
}

fn putBodyJson(allocator: std.mem.Allocator, ctx: *const ProviderContext) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("name");
    try json.write(ctx.name);
    try json.objectField("billing");
    try json.beginObject();
    try json.objectField("deploymentPlan");
    try json.write(ctx.plan_id);
    try json.endObject();
    try json.objectField("deployment");
    try json.beginObject();
    try json.objectField("instances");
    try json.write(@as(u32, 1));
    try json.objectField("type");
    try json.write("deployment");
    try json.objectField("external");
    try json.beginObject();
    try json.objectField("imagePath");
    try json.write(ctx.image_ref);
    if (ctx.registry_credential_id) |value| {
        try json.objectField("credentials");
        try json.write(value);
    }
    try json.endObject();
    try json.endObject();
    try json.objectField("ports");
    try json.beginArray();
    try json.beginObject();
    try json.objectField("name");
    try json.write("http");
    try json.objectField("internalPort");
    try json.write(@as(u32, 3000));
    try json.objectField("public");
    try json.write(true);
    try json.objectField("protocol");
    try json.write("HTTP");
    try json.endObject();
    try json.endArray();
    try json.objectField("runtimeEnvironment");
    try json.beginObject();
    for (ctx.env_vars) |env_var| {
        try json.objectField(env_var.key);
        try json.write(env_var.value);
    }
    try json.endObject();
    try json.endObject();
    return aw.toOwnedSlice();
}

fn parseStringField(allocator: std.mem.Allocator, body: []const u8, key: []const u8) ![]u8 {
    return (try parseOptionalStringField(allocator, body, key)) orelse error.InvalidNorthflankResponse;
}

fn parseOptionalStringField(allocator: std.mem.Allocator, body: []const u8, path: []const u8) !?[]u8 {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, body, .{});
    defer parsed.deinit();
    var current = parsed.value;
    var parts = std.mem.splitScalar(u8, path, '.');
    while (parts.next()) |part| {
        if (current == .object) {
            current = current.object.get(part) orelse return null;
            continue;
        }
        if (current == .array) {
            const idx = std.fmt.parseUnsigned(usize, part, 10) catch return null;
            if (idx >= current.array.items.len) return null;
            current = current.array.items[idx];
            continue;
        }
        return null;
    }
    if (current != .string) return null;
    return try allocator.dupe(u8, current.string);
}

// Test stubs in this module use module-static counters reset at each test entry;
// Zig runs tests serially so the pattern is race-free.
const StubSleeper = struct {
    pub fn sleep(_: StubSleeper, ms: u32) void {
        sleep_total_ms += ms;
    }
};
var sleep_total_ms: u32 = 0;
var sleep_calls: usize = 0;
const CountingSleeper = struct {
    pub fn sleep(_: CountingSleeper, _: u32) void {
        sleep_calls += 1;
    }
};

test "waitForReady returns running after pending -> pending -> running" {
    const TestCtx = struct {
        var fetch_calls: usize = 0;

        fn fetchStatus(_: std.mem.Allocator, _: []const u8, _: []const u8) !ServiceStatus {
            fetch_calls += 1;
            if (fetch_calls < 3) return .pending;
            return .running;
        }
    };
    TestCtx.fetch_calls = 0;
    sleep_total_ms = 0;

    const ctx = ProviderContext{
        .name = "demo",
        .region = "us-central",
        .image_ref = "img",
        .env_vars = &.{},
        .project_id = "proj-1",
        .plan_id = "plan",
        .registry_credential_id = null,
        .state_entry = null,
    };

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    const outcome = try waitForReady(
        std.testing.allocator,
        &ctx,
        "srv-1",
        "tok",
        default_deadline_seconds,
        TestCtx.fetchStatus,
        StubSleeper{},
        buffered.printer(),
    );
    try std.testing.expectEqual(Readiness.running, outcome);
    try std.testing.expectEqual(@as(usize, 3), TestCtx.fetch_calls);
    try std.testing.expect(sleep_total_ms <= default_deadline_seconds * 1000);
    try std.testing.expectEqual(@as(u32, 2 * poll_interval_ms), sleep_total_ms);
    try std.testing.expectEqualStrings("Waiting for service to start...\n", buffered.stderrSlice());
}

test "waitForReady returns failed on terminal failure" {
    const TestCtx = struct {
        var fetch_calls: usize = 0;

        fn fetchStatus(_: std.mem.Allocator, _: []const u8, _: []const u8) !ServiceStatus {
            fetch_calls += 1;
            return .failed;
        }
    };
    TestCtx.fetch_calls = 0;
    sleep_calls = 0;

    const ctx = ProviderContext{
        .name = "demo",
        .region = "us-central",
        .image_ref = "img",
        .env_vars = &.{},
        .project_id = "proj-1",
        .plan_id = "plan",
        .registry_credential_id = null,
        .state_entry = null,
    };

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    const outcome = try waitForReady(
        std.testing.allocator,
        &ctx,
        "srv-1",
        "tok",
        default_deadline_seconds,
        TestCtx.fetchStatus,
        CountingSleeper{},
        buffered.printer(),
    );
    try std.testing.expectEqual(Readiness.failed, outcome);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.fetch_calls);
    try std.testing.expectEqual(@as(usize, 0), sleep_calls);
}

test "waitForReady returns timed_out when deadline elapses" {
    const TestCtx = struct {
        var fetch_calls: usize = 0;

        fn fetchStatus(_: std.mem.Allocator, _: []const u8, _: []const u8) !ServiceStatus {
            fetch_calls += 1;
            return .pending;
        }
    };
    TestCtx.fetch_calls = 0;
    sleep_total_ms = 0;

    const ctx = ProviderContext{
        .name = "demo",
        .region = "us-central",
        .image_ref = "img",
        .env_vars = &.{},
        .project_id = "proj-1",
        .plan_id = "plan",
        .registry_credential_id = null,
        .state_entry = null,
    };

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    const short_deadline: u32 = 10;
    const outcome = try waitForReady(
        std.testing.allocator,
        &ctx,
        "srv-1",
        "tok",
        short_deadline,
        TestCtx.fetchStatus,
        StubSleeper{},
        buffered.printer(),
    );
    try std.testing.expectEqual(Readiness.timed_out, outcome);
    try std.testing.expectEqual(@as(usize, 5), TestCtx.fetch_calls);
    try std.testing.expectEqual(@as(u32, 4 * poll_interval_ms), sleep_total_ms);
    try std.testing.expect(sleep_total_ms <= short_deadline * 1000);
}

test "parseServiceStatus recognises running and failure states" {
    try std.testing.expectEqual(ServiceStatus.running, try parseServiceStatus(std.testing.allocator, "{\"status\":\"running\"}"));
    try std.testing.expectEqual(ServiceStatus.failed, try parseServiceStatus(std.testing.allocator, "{\"status\":\"failed\"}"));
    try std.testing.expectEqual(ServiceStatus.pending, try parseServiceStatus(std.testing.allocator, "{\"status\":\"deploying\"}"));
    try std.testing.expectEqual(ServiceStatus.pending, try parseServiceStatus(std.testing.allocator, "{}"));
}

test "execute emits 'Calling provider...' progress line" {
    // Stub fetch returns a minimal Northflank response so executeWith can
    // parse the service id. The stub also lets us assert execute never hits
    // the network during tests.
    const TestCtx = struct {
        var fetch_calls: usize = 0;

        fn fetchPut(
            allocator: std.mem.Allocator,
            _: []const u8,
            _: []const u8,
            _: []const u8,
        ) !FetchResponse {
            fetch_calls += 1;
            return .{
                .status = 200,
                .body = try allocator.dupe(u8, "{\"id\":\"srv-1\"}"),
            };
        }
    };
    TestCtx.fetch_calls = 0;

    const ctx = ProviderContext{
        .name = "demo",
        .region = "us-central",
        .image_ref = "img",
        .env_vars = &.{},
        .project_id = "proj-1",
        .plan_id = "plan",
        .registry_credential_id = null,
        .state_entry = null,
    };

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();
    var progress = printer_mod.Progress{ .printer = buffered.printer() };

    const result = try executeWith(std.testing.allocator, &ctx, "tok", &progress, TestCtx.fetchPut);
    defer {
        std.testing.allocator.free(result.service_id);
        std.testing.allocator.free(result.status);
        std.testing.allocator.free(result.image_digest_ref);
    }

    try std.testing.expectEqual(@as(usize, 1), TestCtx.fetch_calls);
    try std.testing.expectEqualStrings("srv-1", result.service_id);
    try std.testing.expectEqualStrings("  Calling provider...\n", buffered.stderrSlice());
}

test "putBodyJson serializes deployment request" {
    const env_vars = [_]types.EnvVar{.{ .key = "PORT", .value = "3000" }};
    const body = try putBodyJson(std.testing.allocator, &.{
        .name = "demo",
        .region = "us-east",
        .image_ref = "ghcr.io/acme/demo@sha256:abc",
        .env_vars = &env_vars,
        .project_id = "project-1",
        .plan_id = "nf-compute-20",
        .registry_credential_id = null,
        .state_entry = null,
    });
    defer std.testing.allocator.free(body);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"imagePath\":\"ghcr.io/acme/demo@sha256:abc\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"PORT\":\"3000\"") != null);
}
