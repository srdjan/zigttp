const std = @import("std");
const http = @import("http.zig");
const plan = @import("plan.zig");
const state = @import("state.zig");

pub const ProviderContext = struct {
    name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const plan.EnvVar,
    scope_id: []const u8,
    plan_id: []const u8,
    registry_credential_id: ?[]const u8,
    state_entry: ?*const state.Entry,
};

pub fn buildPlan(
    allocator: std.mem.Allocator,
    ctx: *const ProviderContext,
) !plan.ProviderPlan {
    const action: plan.ProviderAction = if (ctx.state_entry != null) .update else .create;
    const requests = try allocator.alloc(plan.HttpRequestPreview, if (action == .create) 2 else 3);
    if (action == .create) {
        requests[0] = .{
            .method = try allocator.dupe(u8, "POST"),
            .url = try allocator.dupe(u8, "https://api.render.com/v1/services"),
            .body_json = try createBodyJson(allocator, ctx),
        };
        requests[1] = .{
            .method = try allocator.dupe(u8, "PUT"),
            .url = try std.fmt.allocPrint(allocator, "https://api.render.com/v1/services/<service-id>/env-vars", .{}),
            .body_json = try envVarsJson(allocator, ctx.env_vars),
        };
    } else {
        const service_id = ctx.state_entry.?.service_id;
        requests[0] = .{
            .method = try allocator.dupe(u8, "PATCH"),
            .url = try std.fmt.allocPrint(allocator, "https://api.render.com/v1/services/{s}", .{service_id}),
            .body_json = try updateBodyJson(allocator, ctx),
        };
        requests[1] = .{
            .method = try allocator.dupe(u8, "PUT"),
            .url = try std.fmt.allocPrint(allocator, "https://api.render.com/v1/services/{s}/env-vars", .{service_id}),
            .body_json = try envVarsJson(allocator, ctx.env_vars),
        };
        requests[2] = .{
            .method = try allocator.dupe(u8, "POST"),
            .url = try std.fmt.allocPrint(allocator, "https://api.render.com/v1/services/{s}/deploys", .{service_id}),
            .body_json = try std.fmt.allocPrint(allocator, "{{\"imageUrl\":\"{s}\"}}", .{ctx.image_ref}),
        };
    }
    return .{ .action = action, .requests = requests };
}

pub fn execute(
    allocator: std.mem.Allocator,
    ctx: *const ProviderContext,
    api_key: []const u8,
) !plan.DeployResult {
    const action = if (ctx.state_entry != null) plan.ProviderAction.update else plan.ProviderAction.create;
    const service_id = if (action == .create)
        try createService(allocator, ctx, api_key)
    else
        try allocator.dupe(u8, ctx.state_entry.?.service_id);
    defer if (action == .create) allocator.free(service_id);

    if (action == .update) {
        try patchService(allocator, service_id, ctx, api_key);
    }
    try replaceEnvVars(allocator, service_id, ctx.env_vars, api_key);
    const deploy_id = if (action == .update) try triggerDeploy(allocator, service_id, ctx.image_ref, api_key) else null;

    const service_url = try fetchServiceUrl(allocator, service_id, api_key);
    return .{
        .provider = .render,
        .name = ctx.name,
        .service_id = try allocator.dupe(u8, service_id),
        .deployment_id = deploy_id,
        .url = service_url,
        .status = try allocator.dupe(u8, if (action == .create) "created" else "updated"),
        .image_digest_ref = try allocator.dupe(u8, ctx.image_ref),
    };
}

fn createService(allocator: std.mem.Allocator, ctx: *const ProviderContext, api_key: []const u8) ![]u8 {
    const body = try createBodyJson(allocator, ctx);
    defer allocator.free(body);
    const response = try http.requestJson(allocator, .POST, "https://api.render.com/v1/services", api_key, body);
    defer response.deinit(allocator);
    if (response.status != 201 and response.status != 200) return error.RenderCreateFailed;
    return parseRenderServiceId(allocator, response.body);
}

fn patchService(allocator: std.mem.Allocator, service_id: []const u8, ctx: *const ProviderContext, api_key: []const u8) !void {
    const body = try updateBodyJson(allocator, ctx);
    defer allocator.free(body);
    const url = try std.fmt.allocPrint(allocator, "https://api.render.com/v1/services/{s}", .{service_id});
    defer allocator.free(url);
    const response = try http.requestJson(allocator, .PATCH, url, api_key, body);
    defer response.deinit(allocator);
    if (response.status != 200) return error.RenderUpdateFailed;
}

fn replaceEnvVars(allocator: std.mem.Allocator, service_id: []const u8, env_vars: []const plan.EnvVar, api_key: []const u8) !void {
    const url = try std.fmt.allocPrint(allocator, "https://api.render.com/v1/services/{s}/env-vars", .{service_id});
    defer allocator.free(url);
    const body = try envVarsJson(allocator, env_vars);
    defer allocator.free(body);
    const response = try http.requestJson(allocator, .PUT, url, api_key, body);
    defer response.deinit(allocator);
    if (response.status != 200) return error.RenderEnvUpdateFailed;
}

fn triggerDeploy(allocator: std.mem.Allocator, service_id: []const u8, image_ref: []const u8, api_key: []const u8) !?[]const u8 {
    const url = try std.fmt.allocPrint(allocator, "https://api.render.com/v1/services/{s}/deploys", .{service_id});
    defer allocator.free(url);
    const body = try std.fmt.allocPrint(allocator, "{{\"imageUrl\":\"{s}\"}}", .{image_ref});
    defer allocator.free(body);
    const response = try http.requestJson(allocator, .POST, url, api_key, body);
    defer response.deinit(allocator);
    if (response.status != 200 and response.status != 201 and response.status != 202) return error.RenderDeployFailed;
    return parseOptionalStringField(allocator, response.body, "id");
}

fn fetchServiceUrl(allocator: std.mem.Allocator, service_id: []const u8, api_key: []const u8) !?[]const u8 {
    const url = try std.fmt.allocPrint(allocator, "https://api.render.com/v1/services/{s}", .{service_id});
    defer allocator.free(url);
    const response = try http.requestJson(allocator, .GET, url, api_key, null);
    defer response.deinit(allocator);
    if (response.status != 200) return null;
    return parseOptionalStringField(allocator, response.body, "serviceDetails.url");
}

fn createBodyJson(allocator: std.mem.Allocator, ctx: *const ProviderContext) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "{{\"type\":\"web_service\",\"name\":\"{s}\",\"ownerId\":\"{s}\",\"image\":{{\"ownerId\":\"{s}\",\"imagePath\":\"{s}\"{s}}},\"serviceDetails\":{{\"runtime\":\"image\",\"plan\":\"{s}\",\"numInstances\":1,\"region\":\"{s}\"}}}}",
        .{
            ctx.name,
            ctx.scope_id,
            ctx.scope_id,
            ctx.image_ref,
            if (ctx.registry_credential_id) |id|
                try std.fmt.allocPrint(allocator, ",\"registryCredentialId\":\"{s}\"", .{id})
            else
                "",
            ctx.plan_id,
            ctx.region,
        },
    );
}

fn updateBodyJson(allocator: std.mem.Allocator, ctx: *const ProviderContext) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "{{\"image\":{{\"ownerId\":\"{s}\",\"imagePath\":\"{s}\"{s}}},\"serviceDetails\":{{\"runtime\":\"image\",\"plan\":\"{s}\"}}}}",
        .{
            ctx.scope_id,
            ctx.image_ref,
            if (ctx.registry_credential_id) |id|
                try std.fmt.allocPrint(allocator, ",\"registryCredentialId\":\"{s}\"", .{id})
            else
                "",
            ctx.plan_id,
        },
    );
}

fn envVarsJson(allocator: std.mem.Allocator, env_vars: []const plan.EnvVar) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginArray();
    for (env_vars) |env_var| {
        try json.beginObject();
        try json.objectField("key");
        try json.write(env_var.key);
        try json.objectField("value");
        try json.write(env_var.value);
        try json.endObject();
    }
    try json.endArray();
    return aw.toOwnedSlice();
}

fn parseRenderServiceId(allocator: std.mem.Allocator, body: []const u8) ![]u8 {
    return (try parseOptionalStringField(allocator, body, "service.id")) orelse
        (try parseOptionalStringField(allocator, body, "id")) orelse
        error.RenderCreateFailed;
}

fn parseOptionalStringField(allocator: std.mem.Allocator, body: []const u8, path: []const u8) !?[]u8 {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, body, .{});
    defer parsed.deinit();
    const value = try lookupPath(parsed.value, path);
    if (value == null or value.? != .string) return null;
    return try allocator.dupe(u8, value.?.string);
}

fn lookupPath(root: std.json.Value, path: []const u8) !?std.json.Value {
    var current = root;
    var parts = std.mem.splitScalar(u8, path, '.');
    while (parts.next()) |part| {
        if (current != .object) return null;
        current = current.object.get(part) orelse return null;
    }
    return current;
}

test "render plan builds create request" {
    const env_vars = [_]plan.EnvVar{.{ .key = "PORT", .value = "3000" }};
    const provider_plan = try buildPlan(std.testing.allocator, &.{
        .name = "demo",
        .region = "oregon",
        .image_ref = "ghcr.io/acme/demo@sha256:abc",
        .env_vars = &env_vars,
        .scope_id = "owner-1",
        .plan_id = "starter",
        .registry_credential_id = null,
        .state_entry = null,
    });
    defer {
        for (provider_plan.requests) |req| {
            std.testing.allocator.free(req.method);
            std.testing.allocator.free(req.url);
            if (req.body_json) |body| std.testing.allocator.free(body);
        }
        std.testing.allocator.free(provider_plan.requests);
    }
    try std.testing.expectEqual(plan.ProviderAction.create, provider_plan.action);
}
