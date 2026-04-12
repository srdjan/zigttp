const std = @import("std");
const http = @import("http.zig");
const plan = @import("plan.zig");
const state = @import("state.zig");

pub const ProviderContext = struct {
    name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const plan.EnvVar,
    project_id: []const u8,
    plan_id: []const u8,
    registry_credential_id: ?[]const u8,
    state_entry: ?*const state.Entry,
};

pub fn execute(
    allocator: std.mem.Allocator,
    ctx: *const ProviderContext,
    api_token: []const u8,
) !plan.DeployResult {
    const url = try std.fmt.allocPrint(allocator, "https://api.northflank.com/v1/projects/{s}/services/deployment", .{ctx.project_id});
    defer allocator.free(url);
    const body = try putBodyJson(allocator, ctx);
    defer allocator.free(body);

    const response = try http.requestJson(allocator, .PUT, url, api_token, body);
    defer response.deinit(allocator);
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

test "putBodyJson serializes deployment request" {
    const env_vars = [_]plan.EnvVar{.{ .key = "PORT", .value = "3000" }};
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
