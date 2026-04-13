const std = @import("std");
const http = @import("http.zig");
const types = @import("types.zig");
const json_util = @import("json_util.zig");

pub const default_base_url = "https://api.zigttp.dev";
pub const default_region = "us-central";

pub const DeploySession = struct {
    provider: types.Provider,
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
    // User auth token owned by the session, used by the refresh path to
    // re-fetch without plumbing credentials through every call site. Empty
    // until fetchDeploySession populates it.
    token: []u8,
    // Unix seconds. null means the control plane did not advertise expiry.
    expires_at: ?i64,

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
        if (self.token.len > 0) allocator.free(self.token);
    }
};

// swapRefreshedSession asserts the control plane has not reassigned the
// registry identity for this deploy. The OCI image, registry_ref, and provider
// payload are all derived from namespace/registry_host at the start of the
// run; if those change mid-deploy the already-built image would push to the
// wrong location. On mismatch the fresh session is freed and the caller gets
// a clear error to surface; on match the old session is freed in place.
pub fn swapRefreshedSession(
    allocator: std.mem.Allocator,
    old: *DeploySession,
    fresh: DeploySession,
) !void {
    if (!std.mem.eql(u8, fresh.namespace, old.namespace) or
        !std.mem.eql(u8, fresh.registry_host, old.registry_host))
    {
        var f = fresh;
        f.deinit(allocator);
        return error.SessionIdentityChanged;
    }
    old.deinit(allocator);
    old.* = fresh;
}

pub fn defaultNowSec() i64 {
    var ts: std.posix.timespec = undefined;
    switch (std.posix.errno(std.posix.system.clock_gettime(.REALTIME, &ts))) {
        .SUCCESS => return @intCast(ts.sec),
        else => return 0,
    }
}

// sessionRefreshIfExpiring re-fetches the session if it is within skew_seconds
// of expiry. Returns true when the session was refreshed in place. The
// production variant delegates to sessionRefreshIfExpiringFns with the real
// fetchDeploySession seam so tests can inject a stub.
pub fn sessionRefreshIfExpiring(
    allocator: std.mem.Allocator,
    session: *DeploySession,
    project_name: []const u8,
    region: []const u8,
    skew_seconds: i64,
    now_fn: *const fn () i64,
) !bool {
    return sessionRefreshIfExpiringFns(
        allocator,
        session,
        project_name,
        region,
        skew_seconds,
        now_fn,
        fetchDeploySession,
    );
}

pub fn sessionRefreshIfExpiringFns(
    allocator: std.mem.Allocator,
    session: *DeploySession,
    project_name: []const u8,
    region: []const u8,
    skew_seconds: i64,
    now_fn: *const fn () i64,
    fetch_session: anytype,
) !bool {
    const expires_at = session.expires_at orelse return false;
    const now = now_fn();
    if (expires_at - now > skew_seconds) return false;
    const fresh = try fetch_session(allocator, session.token, project_name, region);
    try swapRefreshedSession(allocator, session, fresh);
    return true;
}

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

pub const TokenIdentity = struct {
    email: ?[]u8,

    pub fn deinit(self: *TokenIdentity, allocator: std.mem.Allocator) void {
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
    region: []const u8,
) !DeploySession {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const url = try std.fmt.allocPrint(allocator, "{s}/v1/deploy/session", .{base});
    defer allocator.free(url);

    const body = try buildSessionBody(allocator, project_name, region);
    defer allocator.free(body);

    var response = try http.requestJson(allocator, .POST, url, token, body);
    defer response.deinit(allocator);

    if (response.status == 401 or response.status == 403) return error.NotSignedIn;
    if (response.status >= 400) return error.ControlPlaneError;

    var session = try parseSession(allocator, response.body);
    errdefer session.deinit(allocator);
    session.token = try allocator.dupe(u8, token);
    return session;
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

pub fn validateToken(
    allocator: std.mem.Allocator,
    token: []const u8,
) !TokenIdentity {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const url = try std.fmt.allocPrint(allocator, "{s}/v1/auth/token/verify", .{base});
    defer allocator.free(url);

    var response = try http.requestJson(allocator, .POST, url, token, "{}");
    defer response.deinit(allocator);

    if (response.status == 401 or response.status == 403) return error.InvalidToken;
    if (response.status >= 400) return error.ControlPlaneError;
    return try parseTokenIdentity(allocator, response.body);
}

fn buildSessionBody(allocator: std.mem.Allocator, project_name: []const u8, region: []const u8) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("projectName");
    try json.write(project_name);
    try json.objectField("region");
    try json.write(region);
    try json.endObject();
    return aw.toOwnedSlice();
}

fn parseSession(allocator: std.mem.Allocator, body: []const u8) !DeploySession {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidSessionResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidSessionResponse;
    const root = parsed.value.object;

    const provider_str = json_util.getString(root, "provider") orelse return error.InvalidSessionResponse;
    const provider = types.Provider.fromString(provider_str) orelse return error.InvalidSessionResponse;

    // Collapse the required-field errdefer ladder via OwnedSlices. Every
    // tracked slice is freed on error; on success we call `owned.release()`
    // to drop the tracking list and transfer ownership to the session.
    var owned = json_util.OwnedSlices.init(allocator);
    errdefer owned.freeAll();

    const registry_host = try owned.dupeRequired(root, "registryHost");
    const namespace = try owned.dupeRequired(root, "namespace");
    const registry_username = try owned.dupeRequired(root, "registryUsername");
    const registry_password = try owned.dupeRequired(root, "registryPassword");
    const scope_id = try owned.dupeRequired(root, "scopeId");
    const plan_id = try owned.dupeRequired(root, "planId");
    const provider_api_token = try owned.dupeRequired(root, "providerApiToken");
    const region = try owned.dupeRequired(root, "region");

    const registry_credential_id = try json_util.dupeOptional(allocator, root, "registryCredentialId");
    errdefer if (registry_credential_id) |value| allocator.free(value);

    const url_hint = try json_util.dupeOptional(allocator, root, "urlHint");
    errdefer if (url_hint) |value| allocator.free(value);

    // parseSession leaves token as the empty literal; fetchDeploySession
    // overwrites it with an owned copy. DeploySession.deinit skips freeing
    // when token.len == 0.
    const token: []u8 = &.{};

    const expires_at = json_util.getI64(root, "expiresAt");

    owned.release();

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
        .token = token,
        .expires_at = expires_at,
    };
}

fn parseChallenge(allocator: std.mem.Allocator, body: []const u8) !LoginChallenge {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidChallengeResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidChallengeResponse;
    const root = parsed.value.object;

    const device_code = try json_util.dupeRequired(allocator, root, "deviceCode");
    errdefer allocator.free(device_code);
    const verification_url = try json_util.dupeRequired(allocator, root, "verificationUrl");
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

    const token = try json_util.dupeRequired(allocator, root, "token");
    errdefer allocator.free(token);

    const email = try json_util.dupeOptional(allocator, root, "email");
    return .{ .token = token, .email = email };
}

fn parseTokenIdentity(allocator: std.mem.Allocator, body: []const u8) !TokenIdentity {
    if (body.len == 0) return .{ .email = null };

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidLoginResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidLoginResponse;
    const root = parsed.value.object;

    return .{
        .email = try json_util.dupeOptional(allocator, root, "email"),
    };
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

test "buildSessionBody includes default region preference" {
    const allocator = std.testing.allocator;
    const body = try buildSessionBody(allocator, "demo", default_region);
    defer allocator.free(body);

    try std.testing.expect(std.mem.indexOf(u8, body, "\"projectName\":\"demo\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"region\":\"us-central\"") != null);
}

test "buildSessionBody honors explicit region" {
    const allocator = std.testing.allocator;
    const body = try buildSessionBody(allocator, "demo", "eu-west");
    defer allocator.free(body);

    try std.testing.expect(std.mem.indexOf(u8, body, "\"region\":\"eu-west\"") != null);
}

test "parseSession extracts full session" {
    const allocator = std.testing.allocator;
    const body =
        \\{"provider":"northflank","registryHost":"registry.zigttp.dev","namespace":"u-42","registryUsername":"tok","registryPassword":"pw","scopeId":"proj-1","planId":"plan-nf-basic","providerApiToken":"nf-token","registryCredentialId":"cred-1","region":"us-east","urlHint":"https://demo-u42.zigttp.app","expiresAt":1234567890}
    ;
    var session = try parseSession(allocator, body);
    defer session.deinit(allocator);

    try std.testing.expectEqual(types.Provider.northflank, session.provider);
    try std.testing.expectEqualStrings("registry.zigttp.dev", session.registry_host);
    try std.testing.expectEqualStrings("proj-1", session.scope_id);
    try std.testing.expectEqualStrings("us-east", session.region);
    try std.testing.expect(session.registry_credential_id != null);
    try std.testing.expectEqualStrings("cred-1", session.registry_credential_id.?);
    try std.testing.expect(session.expires_at != null);
    try std.testing.expectEqual(@as(i64, 1234567890), session.expires_at.?);
}

test "parseSession tolerates missing expiresAt" {
    const allocator = std.testing.allocator;
    const body =
        \\{"provider":"northflank","registryHost":"registry.zigttp.dev","namespace":"u-42","registryUsername":"tok","registryPassword":"pw","scopeId":"proj-1","planId":"plan-nf-basic","providerApiToken":"nf-token","region":"us-east"}
    ;
    var session = try parseSession(allocator, body);
    defer session.deinit(allocator);
    try std.testing.expect(session.expires_at == null);
}

test "sessionRefreshIfExpiring returns false when no expiry set" {
    const allocator = std.testing.allocator;
    const TestCtx = struct {
        var fetch_calls: usize = 0;
        fn fetchSession(_: std.mem.Allocator, _: []const u8, _: []const u8, _: []const u8) !DeploySession {
            fetch_calls += 1;
            return error.ShouldNotBeCalled;
        }
        fn now() i64 {
            return 1000;
        }
    };
    TestCtx.fetch_calls = 0;

    var session = try makeStubSession(allocator, null);
    defer session.deinit(allocator);

    const refreshed = try sessionRefreshIfExpiringFns(
        allocator,
        &session,
        "demo",
        "us-east",
        60,
        TestCtx.now,
        TestCtx.fetchSession,
    );
    try std.testing.expect(!refreshed);
    try std.testing.expectEqual(@as(usize, 0), TestCtx.fetch_calls);
}

test "sessionRefreshIfExpiring returns false when far from expiry" {
    const allocator = std.testing.allocator;
    const TestCtx = struct {
        var fetch_calls: usize = 0;
        fn fetchSession(_: std.mem.Allocator, _: []const u8, _: []const u8, _: []const u8) !DeploySession {
            fetch_calls += 1;
            return error.ShouldNotBeCalled;
        }
        fn now() i64 {
            return 1000;
        }
    };
    TestCtx.fetch_calls = 0;

    var session = try makeStubSession(allocator, 1000 + 3600);
    defer session.deinit(allocator);

    const refreshed = try sessionRefreshIfExpiringFns(
        allocator,
        &session,
        "demo",
        "us-east",
        60,
        TestCtx.now,
        TestCtx.fetchSession,
    );
    try std.testing.expect(!refreshed);
    try std.testing.expectEqual(@as(usize, 0), TestCtx.fetch_calls);
}

test "sessionRefreshIfExpiring returns true when within skew" {
    const allocator = std.testing.allocator;
    const TestCtx = struct {
        var fetch_calls: usize = 0;
        var received_token: []const u8 = "";
        fn fetchSession(alloc: std.mem.Allocator, token: []const u8, _: []const u8, _: []const u8) !DeploySession {
            fetch_calls += 1;
            received_token = token;
            var fresh = try makeStubSession(alloc, 1000 + 7200);
            alloc.free(fresh.token);
            fresh.token = try alloc.dupe(u8, "rotated-token");
            return fresh;
        }
        fn now() i64 {
            return 1000;
        }
    };
    TestCtx.fetch_calls = 0;
    TestCtx.received_token = "";

    var session = try makeStubSession(allocator, 1000 + 30);
    allocator.free(session.token);
    session.token = try allocator.dupe(u8, "initial-token");
    defer session.deinit(allocator);

    const refreshed = try sessionRefreshIfExpiringFns(
        allocator,
        &session,
        "demo",
        "us-east",
        60,
        TestCtx.now,
        TestCtx.fetchSession,
    );
    try std.testing.expect(refreshed);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.fetch_calls);
    try std.testing.expectEqualStrings("initial-token", TestCtx.received_token);
    try std.testing.expectEqualStrings("rotated-token", session.token);
    try std.testing.expectEqual(@as(i64, 1000 + 7200), session.expires_at.?);
}

fn makeStubSession(allocator: std.mem.Allocator, expires_at: ?i64) !DeploySession {
    return .{
        .provider = .northflank,
        .registry_host = try allocator.dupe(u8, "registry.zigttp.dev"),
        .namespace = try allocator.dupe(u8, "u-42"),
        .registry_username = try allocator.dupe(u8, "tok"),
        .registry_password = try allocator.dupe(u8, "pw"),
        .scope_id = try allocator.dupe(u8, "proj-1"),
        .plan_id = try allocator.dupe(u8, "plan-basic"),
        .provider_api_token = try allocator.dupe(u8, "nf-token"),
        .registry_credential_id = null,
        .region = try allocator.dupe(u8, default_region),
        .url_hint = null,
        .token = try allocator.dupe(u8, "stub-token"),
        .expires_at = expires_at,
    };
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

test "parseTokenIdentity captures optional email" {
    var identity = try parseTokenIdentity(std.testing.allocator, "{\"email\":\"me@example.com\"}");
    defer identity.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("me@example.com", identity.email.?);
}

test "parseTokenIdentity accepts empty object" {
    var identity = try parseTokenIdentity(std.testing.allocator, "{}");
    defer identity.deinit(std.testing.allocator);
    try std.testing.expect(identity.email == null);
}
