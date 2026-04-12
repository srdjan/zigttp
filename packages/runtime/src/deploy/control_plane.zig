const std = @import("std");
const http = @import("http.zig");
const plan = @import("plan.zig");

pub const default_base_url = "https://api.zigttp.dev";

pub const DeploySession = struct {
    provider: plan.Provider,
    registry_host: []u8,
    namespace: []u8,
    registry_username: []u8,
    registry_password: []u8,
    scope_id: []u8,
    plan_id: []u8,
    provider_api_token: []u8,
    registry_credential_id: ?[]u8,
    region: []u8,
    url_hint: ?[]u8,

    pub fn deinit(self: *DeploySession, allocator: std.mem.Allocator) void {
        allocator.free(self.registry_host);
        allocator.free(self.namespace);
        allocator.free(self.registry_username);
        allocator.free(self.registry_password);
        allocator.free(self.scope_id);
        allocator.free(self.plan_id);
        allocator.free(self.provider_api_token);
        if (self.registry_credential_id) |value| allocator.free(value);
        allocator.free(self.region);
        if (self.url_hint) |value| allocator.free(value);
    }
};

pub const LoginChallenge = struct {
    device_code: []u8,
    verification_url: []u8,
    interval_seconds: u32,
    expires_in_seconds: u32,

    pub fn deinit(self: *LoginChallenge, allocator: std.mem.Allocator) void {
        allocator.free(self.device_code);
        allocator.free(self.verification_url);
    }
};

pub const LoginResult = struct {
    token: []u8,
    email: ?[]u8,

    pub fn deinit(self: *LoginResult, allocator: std.mem.Allocator) void {
        allocator.free(self.token);
        if (self.email) |value| allocator.free(value);
    }
};

pub const PollStatus = enum {
    pending,
    done,
    expired,
    denied,
};

pub const PollOutcome = union(PollStatus) {
    pending: void,
    done: LoginResult,
    expired: void,
    denied: void,
};

pub fn baseUrl(allocator: std.mem.Allocator) ![]u8 {
    const raw = std.c.getenv("ZIGTTP_CONTROL_PLANE_URL");
    if (raw == null) return allocator.dupe(u8, default_base_url);
    return allocator.dupe(u8, std.mem.sliceTo(raw.?, 0));
}

pub fn fetchDeploySession(
    allocator: std.mem.Allocator,
    token: []const u8,
    project_name: []const u8,
) !DeploySession {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const url = try std.fmt.allocPrint(allocator, "{s}/v1/deploy/session", .{base});
    defer allocator.free(url);

    const body = try buildSessionBody(allocator, project_name);
    defer allocator.free(body);

    var response = try http.requestJson(allocator, .POST, url, token, body);
    defer response.deinit(allocator);

    if (response.status == 401 or response.status == 403) return error.NotSignedIn;
    if (response.status >= 400) return error.ControlPlaneError;

    return try parseSession(allocator, response.body);
}

pub fn startLogin(allocator: std.mem.Allocator) !LoginChallenge {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const url = try std.fmt.allocPrint(allocator, "{s}/v1/auth/device/start", .{base});
    defer allocator.free(url);

    var response = try http.requestJson(allocator, .POST, url, null, "{}");
    defer response.deinit(allocator);

    if (response.status >= 400) return error.ControlPlaneError;
    return try parseChallenge(allocator, response.body);
}

pub fn pollLogin(
    allocator: std.mem.Allocator,
    device_code: []const u8,
) !PollOutcome {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const url = try std.fmt.allocPrint(allocator, "{s}/v1/auth/device/poll", .{base});
    defer allocator.free(url);

    const body = try std.fmt.allocPrint(allocator, "{{\"deviceCode\":\"{s}\"}}", .{device_code});
    defer allocator.free(body);

    var response = try http.requestJson(allocator, .POST, url, null, body);
    defer response.deinit(allocator);

    if (response.status == 202) return .{ .pending = {} };
    if (response.status == 410) return .{ .expired = {} };
    if (response.status == 403) return .{ .denied = {} };
    if (response.status >= 400) return error.ControlPlaneError;

    const result = try parseLoginResult(allocator, response.body);
    return .{ .done = result };
}

fn buildSessionBody(allocator: std.mem.Allocator, project_name: []const u8) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("projectName");
    try json.write(project_name);
    try json.endObject();
    return aw.toOwnedSlice();
}

fn parseSession(allocator: std.mem.Allocator, body: []const u8) !DeploySession {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidSessionResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidSessionResponse;
    const root = parsed.value.object;

    const provider_str = requireString(root, "provider") orelse return error.InvalidSessionResponse;
    const provider = plan.Provider.fromString(provider_str) orelse return error.InvalidSessionResponse;

    const registry_host = try dupeField(allocator, root, "registryHost");
    errdefer allocator.free(registry_host);
    const namespace = try dupeField(allocator, root, "namespace");
    errdefer allocator.free(namespace);
    const registry_username = try dupeField(allocator, root, "registryUsername");
    errdefer allocator.free(registry_username);
    const registry_password = try dupeField(allocator, root, "registryPassword");
    errdefer allocator.free(registry_password);
    const scope_id = try dupeField(allocator, root, "scopeId");
    errdefer allocator.free(scope_id);
    const plan_id = try dupeField(allocator, root, "planId");
    errdefer allocator.free(plan_id);
    const provider_api_token = try dupeField(allocator, root, "providerApiToken");
    errdefer allocator.free(provider_api_token);
    const region = try dupeField(allocator, root, "region");
    errdefer allocator.free(region);

    var registry_credential_id: ?[]u8 = null;
    if (root.get("registryCredentialId")) |value| {
        if (value == .string) registry_credential_id = try allocator.dupe(u8, value.string);
    }
    errdefer if (registry_credential_id) |value| allocator.free(value);

    var url_hint: ?[]u8 = null;
    errdefer if (url_hint) |value| allocator.free(value);
    if (root.get("urlHint")) |value| {
        if (value == .string) url_hint = try allocator.dupe(u8, value.string);
    }

    return .{
        .provider = provider,
        .registry_host = registry_host,
        .namespace = namespace,
        .registry_username = registry_username,
        .registry_password = registry_password,
        .scope_id = scope_id,
        .plan_id = plan_id,
        .provider_api_token = provider_api_token,
        .registry_credential_id = registry_credential_id,
        .region = region,
        .url_hint = url_hint,
    };
}

fn parseChallenge(allocator: std.mem.Allocator, body: []const u8) !LoginChallenge {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidChallengeResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidChallengeResponse;
    const root = parsed.value.object;

    const device_code = try dupeField(allocator, root, "deviceCode");
    errdefer allocator.free(device_code);
    const verification_url = try dupeField(allocator, root, "verificationUrl");
    errdefer allocator.free(verification_url);

    const interval: u32 = if (root.get("intervalSeconds")) |value| switch (value) {
        .integer => |i| if (i < 0) 5 else @intCast(i),
        else => 5,
    } else 5;
    const expires: u32 = if (root.get("expiresInSeconds")) |value| switch (value) {
        .integer => |i| if (i < 0) 600 else @intCast(i),
        else => 600,
    } else 600;

    return .{
        .device_code = device_code,
        .verification_url = verification_url,
        .interval_seconds = interval,
        .expires_in_seconds = expires,
    };
}

fn parseLoginResult(allocator: std.mem.Allocator, body: []const u8) !LoginResult {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidLoginResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidLoginResponse;
    const root = parsed.value.object;

    const token = try dupeField(allocator, root, "token");
    errdefer allocator.free(token);

    var email: ?[]u8 = null;
    if (root.get("email")) |value| {
        if (value == .string) email = try allocator.dupe(u8, value.string);
    }
    return .{ .token = token, .email = email };
}

fn dupeField(allocator: std.mem.Allocator, obj: std.json.ObjectMap, key: []const u8) ![]u8 {
    const value = obj.get(key) orelse return error.MissingField;
    if (value != .string) return error.InvalidFieldType;
    return allocator.dupe(u8, value.string);
}

fn requireString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    if (value != .string) return null;
    return value.string;
}

test "baseUrl falls back to default" {
    const allocator = std.testing.allocator;
    _ = std.c.unsetenv("ZIGTTP_CONTROL_PLANE_URL");
    const url = try baseUrl(allocator);
    defer allocator.free(url);
    try std.testing.expectEqualStrings(default_base_url, url);
}

test "baseUrl honors override" {
    const allocator = std.testing.allocator;
    _ = std.c.setenv("ZIGTTP_CONTROL_PLANE_URL", "http://localhost:9999", 1);
    defer _ = std.c.unsetenv("ZIGTTP_CONTROL_PLANE_URL");
    const url = try baseUrl(allocator);
    defer allocator.free(url);
    try std.testing.expectEqualStrings("http://localhost:9999", url);
}

test "parseSession extracts full session" {
    const allocator = std.testing.allocator;
    const body =
        \\{"provider":"northflank","registryHost":"registry.zigttp.dev","namespace":"u-42","registryUsername":"tok","registryPassword":"pw","scopeId":"proj-1","planId":"plan-nf-basic","providerApiToken":"nf-token","registryCredentialId":"cred-1","region":"us-east","urlHint":"https://demo-u42.zigttp.app"}
    ;
    var session = try parseSession(allocator, body);
    defer session.deinit(allocator);

    try std.testing.expectEqual(plan.Provider.northflank, session.provider);
    try std.testing.expectEqualStrings("registry.zigttp.dev", session.registry_host);
    try std.testing.expectEqualStrings("proj-1", session.scope_id);
    try std.testing.expectEqualStrings("us-east", session.region);
    try std.testing.expect(session.registry_credential_id != null);
    try std.testing.expectEqualStrings("cred-1", session.registry_credential_id.?);
}

test "parseChallenge accepts minimal device flow response" {
    const allocator = std.testing.allocator;
    const body =
        \\{"deviceCode":"ABCD-1234","verificationUrl":"https://zigttp.dev/cli/authorize?code=ABCD","intervalSeconds":3,"expiresInSeconds":300}
    ;
    var challenge = try parseChallenge(allocator, body);
    defer challenge.deinit(allocator);
    try std.testing.expectEqualStrings("ABCD-1234", challenge.device_code);
    try std.testing.expectEqual(@as(u32, 3), challenge.interval_seconds);
}

test "parseLoginResult captures token and email" {
    const allocator = std.testing.allocator;
    const body =
        \\{"token":"t-abc","email":"me@example.com"}
    ;
    var result = try parseLoginResult(allocator, body);
    defer result.deinit(allocator);
    try std.testing.expectEqualStrings("t-abc", result.token);
    try std.testing.expectEqualStrings("me@example.com", result.email.?);
}
