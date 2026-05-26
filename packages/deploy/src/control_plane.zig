const std = @import("std");
const http = @import("http.zig");
const types = @import("types.zig");
const json_util = @import("json_util.zig");

pub const default_base_url = "https://api.zigttp.dev";
pub const default_region = "us-central";

pub const SessionRequest = struct {
    project_name: []const u8,
    region: []const u8,
    contract_json: []const u8,
    contract_sha256: []const u8,
    plan_id: ?[]const u8 = null,
};

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
    token: []u8,
    expires_at: ?i64,
    request_body: []u8,
    grant_ids: []const []u8,

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
        if (self.request_body.len > 0) allocator.free(self.request_body);
        for (self.grant_ids) |grant_id| allocator.free(grant_id);
        allocator.free(self.grant_ids);
    }
};

pub const DeployPlanRequired = struct {
    plan_id: []u8,
    review_url: ?[]u8,
    baseline_sha: ?[]u8,
    proposed_sha: ?[]u8,
    expires_at: ?i64,
    diff_reasons: []const []u8,
    covered_reasons: []const []u8,
    uncovered_reasons: []const []u8,

    pub fn deinit(self: *DeployPlanRequired, allocator: std.mem.Allocator) void {
        allocator.free(self.plan_id);
        if (self.review_url) |value| allocator.free(value);
        if (self.baseline_sha) |value| allocator.free(value);
        if (self.proposed_sha) |value| allocator.free(value);
        freeStringList(allocator, self.diff_reasons);
        freeStringList(allocator, self.covered_reasons);
        freeStringList(allocator, self.uncovered_reasons);
    }
};

pub const FetchDeploySessionResult = union(enum) {
    approved: DeploySession,
    plan_required: DeployPlanRequired,
};

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

pub fn sessionRefreshIfExpiring(
    allocator: std.mem.Allocator,
    session: *DeploySession,
    skew_seconds: i64,
    now_fn: *const fn () i64,
) !bool {
    return sessionRefreshIfExpiringFns(
        allocator,
        session,
        skew_seconds,
        now_fn,
        fetchApprovedDeploySession,
    );
}

pub fn sessionRefreshIfExpiringFns(
    allocator: std.mem.Allocator,
    session: *DeploySession,
    skew_seconds: i64,
    now_fn: *const fn () i64,
    fetch_session: anytype,
) !bool {
    const expires_at = session.expires_at orelse return false;
    const now = now_fn();
    if (expires_at - now > skew_seconds) return false;
    const fresh = try fetch_session(allocator, session.token, session.request_body);
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

pub const ReviewAction = enum {
    approve,
    reject,
};

pub const ReviewMode = enum {
    once,
    grant,
};

pub const ReviewResult = struct {
    status: []u8,
    grants_created: usize,

    pub fn deinit(self: *ReviewResult, allocator: std.mem.Allocator) void {
        allocator.free(self.status);
    }
};

pub const DeployPlanStatus = struct {
    id: []u8,
    project_name: []u8,
    status: []u8,
    baseline_sha: ?[]u8,
    proposed_sha: []u8,
    diff_reasons: []const []u8,
    created_at: []u8,
    expires_at: []u8,
    decided_at: ?[]u8,
    decided_by_token_id: ?[]u8,
    consumed_at: ?[]u8,

    pub fn deinit(self: *DeployPlanStatus, allocator: std.mem.Allocator) void {
        allocator.free(self.id);
        allocator.free(self.project_name);
        allocator.free(self.status);
        if (self.baseline_sha) |value| allocator.free(value);
        allocator.free(self.proposed_sha);
        freeStringList(allocator, self.diff_reasons);
        allocator.free(self.created_at);
        allocator.free(self.expires_at);
        if (self.decided_at) |value| allocator.free(value);
        if (self.decided_by_token_id) |value| allocator.free(value);
        if (self.consumed_at) |value| allocator.free(value);
    }
};

pub const CapabilityGrant = struct {
    id: []u8,
    project_name: []u8,
    source_plan_id: []u8,
    created_at: []u8,
    expires_at: ?[]u8,
    revoked_at: ?[]u8,
    status: []u8,
    reasons: []const []u8,

    pub fn deinit(self: *CapabilityGrant, allocator: std.mem.Allocator) void {
        allocator.free(self.id);
        allocator.free(self.project_name);
        allocator.free(self.source_plan_id);
        allocator.free(self.created_at);
        if (self.expires_at) |value| allocator.free(value);
        if (self.revoked_at) |value| allocator.free(value);
        allocator.free(self.status);
        freeStringList(allocator, self.reasons);
    }
};

pub const CapabilityGrantList = struct {
    items: []CapabilityGrant,

    pub fn deinit(self: *CapabilityGrantList, allocator: std.mem.Allocator) void {
        for (self.items) |*item| item.deinit(allocator);
        allocator.free(self.items);
    }
};

pub fn baseUrl(allocator: std.mem.Allocator) ![]u8 {
    const raw = std.c.getenv("ZIGTTP_CONTROL_PLANE_URL");
    if (raw == null) return allocator.dupe(u8, default_base_url);
    return allocator.dupe(u8, std.mem.sliceTo(raw.?, 0));
}

pub fn fetchDeploySession(
    allocator: std.mem.Allocator,
    token: []const u8,
    request: SessionRequest,
) !FetchDeploySessionResult {
    const body = try buildSessionBody(
        allocator,
        request.project_name,
        request.region,
        request.contract_json,
        request.contract_sha256,
        request.plan_id,
    );
    errdefer allocator.free(body);
    return fetchDeploySessionFromBody(allocator, token, body);
}

pub fn fetchApprovedDeploySession(
    allocator: std.mem.Allocator,
    token: []const u8,
    request_body: []const u8,
) !DeploySession {
    const result = try fetchDeploySessionFromBody(
        allocator,
        token,
        try allocator.dupe(u8, request_body),
    );
    return switch (result) {
        .approved => |session| session,
        .plan_required => |plan_value| {
            var plan = plan_value;
            defer plan.deinit(allocator);
            return error.ControlPlaneReviewRequired;
        },
    };
}

fn fetchDeploySessionFromBody(
    allocator: std.mem.Allocator,
    token: []const u8,
    request_body: []u8,
) !FetchDeploySessionResult {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const url = try std.fmt.allocPrint(allocator, "{s}/v1/deploy/session", .{base});
    defer allocator.free(url);

    var response = try http.requestJson(allocator, .POST, url, token, request_body);
    defer response.deinit(allocator);

    if (response.status == 401 or response.status == 403) return error.NotSignedIn;
    if (response.status == 202) {
        var plan = try parsePlanRequired(allocator, response.body);
        errdefer plan.deinit(allocator);
        allocator.free(request_body);
        return .{ .plan_required = plan };
    }
    if (response.status >= 400) return error.ControlPlaneError;

    var session = try parseSession(allocator, response.body);
    errdefer session.deinit(allocator);
    session.token = try allocator.dupe(u8, token);
    session.request_body = request_body;
    return .{ .approved = session };
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

pub fn reviewPlan(
    allocator: std.mem.Allocator,
    token: []const u8,
    plan_id: []const u8,
    action: ReviewAction,
    mode: ReviewMode,
) !ReviewResult {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const action_segment = switch (action) {
        .approve => "approve",
        .reject => "reject",
    };
    const url = try std.fmt.allocPrint(
        allocator,
        "{s}/v1/deploy/plans/{s}/{s}",
        .{ base, plan_id, action_segment },
    );
    defer allocator.free(url);

    const body = switch (action) {
        .approve => switch (mode) {
            .once => "{\"mode\":\"once\"}",
            .grant => "{\"mode\":\"grant\"}",
        },
        .reject => "{}",
    };

    var response = try http.requestJson(allocator, .POST, url, token, body);
    defer response.deinit(allocator);

    if (response.status == 401 or response.status == 403) return error.NotSignedIn;
    if (response.status == 404) return error.PlanNotFound;
    if (response.status == 409) return error.PlanConflict;
    if (response.status == 410) return error.PlanExpired;
    if (response.status >= 400) return error.ControlPlaneError;

    return try parseReviewResult(allocator, response.body);
}

pub fn fetchDeployPlan(
    allocator: std.mem.Allocator,
    token: []const u8,
    plan_id: []const u8,
) !DeployPlanStatus {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const url = try std.fmt.allocPrint(
        allocator,
        "{s}/v1/deploy/plans/{s}",
        .{ base, plan_id },
    );
    defer allocator.free(url);

    var response = try http.requestJson(allocator, .GET, url, token, null);
    defer response.deinit(allocator);

    if (response.status == 401 or response.status == 403) return error.NotSignedIn;
    if (response.status == 404) return error.PlanNotFound;
    if (response.status >= 400) return error.ControlPlaneError;

    return try parseDeployPlanStatus(allocator, response.body);
}

pub fn fetchCapabilityGrants(
    allocator: std.mem.Allocator,
    token: []const u8,
    project_name: ?[]const u8,
) !CapabilityGrantList {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const url = if (project_name) |project|
        try std.fmt.allocPrint(
            allocator,
            "{s}/v1/projects/{s}/grants",
            .{ base, project },
        )
    else
        try std.fmt.allocPrint(allocator, "{s}/v1/grants", .{base});
    defer allocator.free(url);

    var response = try http.requestJson(allocator, .GET, url, token, null);
    defer response.deinit(allocator);

    if (response.status == 401 or response.status == 403) return error.NotSignedIn;
    if (response.status >= 400) return error.ControlPlaneError;

    return try parseCapabilityGrantList(allocator, response.body);
}

pub fn revokeCapabilityGrant(
    allocator: std.mem.Allocator,
    token: []const u8,
    grant_id: []const u8,
) !CapabilityGrant {
    const base = try baseUrl(allocator);
    defer allocator.free(base);
    const url = try std.fmt.allocPrint(
        allocator,
        "{s}/v1/grants/{s}/revoke",
        .{ base, grant_id },
    );
    defer allocator.free(url);

    var response = try http.requestJson(allocator, .POST, url, token, "{}");
    defer response.deinit(allocator);

    if (response.status == 401 or response.status == 403) return error.NotSignedIn;
    if (response.status == 404) return error.GrantNotFound;
    if (response.status >= 400) return error.ControlPlaneError;

    return try parseRevokedGrant(allocator, response.body);
}

fn buildSessionBody(
    allocator: std.mem.Allocator,
    project_name: []const u8,
    region: []const u8,
    contract_json: []const u8,
    contract_sha256: []const u8,
    plan_id: ?[]const u8,
) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("projectName");
    try json.write(project_name);
    try json.objectField("region");
    try json.write(region);
    try json.objectField("contract");
    try aw.writer.writeAll(contract_json);
    try json.objectField("contractSha256");
    try json.write(contract_sha256);
    if (plan_id) |value| {
        try json.objectField("planId");
        try json.write(value);
    }
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

    const grant_ids = try dupStringArrayFromField(allocator, root, "grantIds");
    errdefer freeStringList(allocator, grant_ids);

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
        .request_body = &.{},
        .grant_ids = grant_ids,
    };
}

fn parsePlanRequired(allocator: std.mem.Allocator, body: []const u8) !DeployPlanRequired {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidSessionResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidSessionResponse;
    const root = parsed.value.object;

    const plan_id = try json_util.dupeRequired(allocator, root, "planId");
    errdefer allocator.free(plan_id);
    const review_url = try json_util.dupeOptional(allocator, root, "reviewUrl");
    errdefer if (review_url) |value| allocator.free(value);
    const baseline_sha = try json_util.dupeOptional(allocator, root, "baselineSha");
    errdefer if (baseline_sha) |value| allocator.free(value);
    const proposed_sha = try json_util.dupeOptional(allocator, root, "proposedSha");
    errdefer if (proposed_sha) |value| allocator.free(value);
    const diff_reasons = try dupNestedStringArray(allocator, root, "diff", "reasons");
    errdefer freeStringList(allocator, diff_reasons);
    const covered_reasons = try dupNestedStringArray(allocator, root, "grantCoverage", "coveredReasons");
    errdefer freeStringList(allocator, covered_reasons);
    const uncovered_reasons = try dupNestedStringArray(allocator, root, "grantCoverage", "uncoveredReasons");
    errdefer freeStringList(allocator, uncovered_reasons);

    return .{
        .plan_id = plan_id,
        .review_url = review_url,
        .baseline_sha = baseline_sha,
        .proposed_sha = proposed_sha,
        .expires_at = json_util.getI64(root, "expiresAt"),
        .diff_reasons = diff_reasons,
        .covered_reasons = covered_reasons,
        .uncovered_reasons = uncovered_reasons,
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

fn parseReviewResult(allocator: std.mem.Allocator, body: []const u8) !ReviewResult {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidSessionResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidSessionResponse;
    const root = parsed.value.object;

    const plan_obj = if (root.get("plan")) |value|
        switch (value) {
            .object => value.object,
            else => return error.InvalidSessionResponse,
        }
    else
        root;

    const status = try json_util.dupeRequired(allocator, plan_obj, "status");
    errdefer allocator.free(status);

    const grants_created = if (root.get("grants")) |value|
        switch (value) {
            .array => value.array.items.len,
            else => return error.InvalidSessionResponse,
        }
    else
        0;

    return .{
        .status = status,
        .grants_created = grants_created,
    };
}

fn parseDeployPlanStatus(allocator: std.mem.Allocator, body: []const u8) !DeployPlanStatus {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidSessionResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidSessionResponse;
    const root = parsed.value.object;

    const id = try json_util.dupeRequired(allocator, root, "id");
    errdefer allocator.free(id);
    const project_name = try json_util.dupeRequired(allocator, root, "projectName");
    errdefer allocator.free(project_name);
    const status = try json_util.dupeRequired(allocator, root, "status");
    errdefer allocator.free(status);
    const baseline_sha = try json_util.dupeOptional(allocator, root, "baselineSha");
    errdefer if (baseline_sha) |value| allocator.free(value);
    const proposed_sha = try json_util.dupeRequired(allocator, root, "proposedSha");
    errdefer allocator.free(proposed_sha);
    const diff_reasons = try dupNestedStringArray(allocator, root, "diff", "reasons");
    errdefer freeStringList(allocator, diff_reasons);
    const created_at = try json_util.dupeRequired(allocator, root, "createdAt");
    errdefer allocator.free(created_at);
    const expires_at = try json_util.dupeRequired(allocator, root, "expiresAt");
    errdefer allocator.free(expires_at);
    const decided_at = try json_util.dupeOptional(allocator, root, "decidedAt");
    errdefer if (decided_at) |value| allocator.free(value);
    const decided_by_token_id = try json_util.dupeOptional(allocator, root, "decidedByTokenId");
    errdefer if (decided_by_token_id) |value| allocator.free(value);
    const consumed_at = try json_util.dupeOptional(allocator, root, "consumedAt");
    errdefer if (consumed_at) |value| allocator.free(value);

    return .{
        .id = id,
        .project_name = project_name,
        .status = status,
        .baseline_sha = baseline_sha,
        .proposed_sha = proposed_sha,
        .diff_reasons = diff_reasons,
        .created_at = created_at,
        .expires_at = expires_at,
        .decided_at = decided_at,
        .decided_by_token_id = decided_by_token_id,
        .consumed_at = consumed_at,
    };
}

fn parseCapabilityGrantList(allocator: std.mem.Allocator, body: []const u8) !CapabilityGrantList {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidSessionResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidSessionResponse;
    const root = parsed.value.object;
    const grants_value = root.get("grants") orelse return error.InvalidSessionResponse;
    if (grants_value != .array) return error.InvalidSessionResponse;

    const items = try allocator.alloc(CapabilityGrant, grants_value.array.items.len);
    var built: usize = 0;
    errdefer {
        for (items[0..built]) |*item| item.deinit(allocator);
        allocator.free(items);
    }

    for (grants_value.array.items, 0..) |item, idx| {
        items[idx] = try parseCapabilityGrantValue(allocator, item);
        built += 1;
    }

    return .{ .items = items };
}

fn parseRevokedGrant(allocator: std.mem.Allocator, body: []const u8) !CapabilityGrant {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return error.InvalidSessionResponse;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidSessionResponse;
    const root = parsed.value.object;
    const grant = root.get("grant") orelse return error.InvalidSessionResponse;
    return parseCapabilityGrantValue(allocator, grant);
}

fn parseCapabilityGrantValue(allocator: std.mem.Allocator, value: std.json.Value) !CapabilityGrant {
    if (value != .object) return error.InvalidSessionResponse;
    const obj = value.object;

    const id = try json_util.dupeRequired(allocator, obj, "id");
    errdefer allocator.free(id);
    const project_name = try json_util.dupeRequired(allocator, obj, "projectName");
    errdefer allocator.free(project_name);
    const source_plan_id = try json_util.dupeRequired(allocator, obj, "sourcePlanId");
    errdefer allocator.free(source_plan_id);
    const created_at = try json_util.dupeRequired(allocator, obj, "createdAt");
    errdefer allocator.free(created_at);
    const expires_at = try json_util.dupeOptional(allocator, obj, "expiresAt");
    errdefer if (expires_at) |slice| allocator.free(slice);
    const revoked_at = try json_util.dupeOptional(allocator, obj, "revokedAt");
    errdefer if (revoked_at) |slice| allocator.free(slice);
    const status = try json_util.dupeRequired(allocator, obj, "status");
    errdefer allocator.free(status);
    const reasons = try dupStringArrayFromField(allocator, obj, "reasons");
    errdefer freeStringList(allocator, reasons);

    return .{
        .id = id,
        .project_name = project_name,
        .source_plan_id = source_plan_id,
        .created_at = created_at,
        .expires_at = expires_at,
        .revoked_at = revoked_at,
        .status = status,
        .reasons = reasons,
    };
}

fn dupStringArrayFromField(
    allocator: std.mem.Allocator,
    obj: std.json.ObjectMap,
    key: []const u8,
) ![]const []u8 {
    const value = obj.get(key) orelse return try allocator.alloc([]u8, 0);
    if (value != .array) return error.InvalidSessionResponse;
    return try dupStringArray(allocator, value.array.items);
}

fn dupNestedStringArray(
    allocator: std.mem.Allocator,
    obj: std.json.ObjectMap,
    parent_key: []const u8,
    child_key: []const u8,
) ![]const []u8 {
    const parent = obj.get(parent_key) orelse return try allocator.alloc([]u8, 0);
    if (parent != .object) return try allocator.alloc([]u8, 0);
    const child = parent.object.get(child_key) orelse return try allocator.alloc([]u8, 0);
    if (child != .array) return error.InvalidSessionResponse;
    return try dupStringArray(allocator, child.array.items);
}

fn dupStringArray(allocator: std.mem.Allocator, items: []const std.json.Value) ![]const []u8 {
    const result = try allocator.alloc([]u8, items.len);
    errdefer {
        for (result[0..]) |slice| allocator.free(slice);
        allocator.free(result);
    }

    for (items, 0..) |item, idx| {
        if (item != .string) return error.InvalidSessionResponse;
        result[idx] = try allocator.dupe(u8, item.string);
    }
    return result;
}

fn freeStringList(allocator: std.mem.Allocator, values: []const []u8) void {
    for (values) |value| allocator.free(value);
    allocator.free(values);
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

test "buildSessionBody includes contract payload and region" {
    const allocator = std.testing.allocator;
    const body = try buildSessionBody(
        allocator,
        "demo",
        default_region,
        "{\"routes\":[]}",
        "abc123",
        null,
    );
    defer allocator.free(body);

    try std.testing.expect(std.mem.indexOf(u8, body, "\"projectName\":\"demo\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"region\":\"us-central\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"contract\":{\"routes\":[]}") != null);
    try std.testing.expect(std.mem.indexOf(u8, body, "\"contractSha256\":\"abc123\"") != null);
}

test "buildSessionBody includes optional planId" {
    const allocator = std.testing.allocator;
    const body = try buildSessionBody(
        allocator,
        "demo",
        "eu-west",
        "{\"routes\":[]}",
        "abc123",
        "plan-1",
    );
    defer allocator.free(body);

    try std.testing.expect(std.mem.indexOf(u8, body, "\"planId\":\"plan-1\"") != null);
}

test "parseSession extracts full session" {
    const allocator = std.testing.allocator;
    const body =
        \\{"provider":"northflank","registryHost":"registry.zigttp.dev","namespace":"u-42","registryUsername":"tok","registryPassword":"pw","scopeId":"proj-1","planId":"plan-nf-basic","providerApiToken":"nf-token","registryCredentialId":"cred-1","region":"us-east","urlHint":"https://demo-u42.zigttp.app","expiresAt":1234567890,"grantIds":["grant-1","grant-2"]}
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
    try std.testing.expectEqual(@as(usize, 2), session.grant_ids.len);
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

test "parsePlanRequired extracts review details" {
    const allocator = std.testing.allocator;
    const body =
        \\{"status":"plan_required","planId":"plan-1","reviewUrl":"https://control/deploy/plans/plan-1","baselineSha":"base","proposedSha":"next","expiresAt":1234,"diff":{"reasons":["new env read: SECRET_KEY"]},"grantCoverage":{"coveredReasons":["new env read: DATABASE_URL"],"uncoveredReasons":["new effect: write"]}}
    ;
    var plan = try parsePlanRequired(allocator, body);
    defer plan.deinit(allocator);

    try std.testing.expectEqualStrings("plan-1", plan.plan_id);
    try std.testing.expectEqualStrings("https://control/deploy/plans/plan-1", plan.review_url.?);
    try std.testing.expectEqual(@as(usize, 1), plan.diff_reasons.len);
    try std.testing.expectEqual(@as(usize, 1), plan.covered_reasons.len);
    try std.testing.expectEqual(@as(usize, 1), plan.uncovered_reasons.len);
}

test "parseDeployPlanStatus extracts plan details" {
    var plan = try parseDeployPlanStatus(
        std.testing.allocator,
        "{\"id\":\"plan-1\",\"projectName\":\"demo\",\"proposedSha\":\"next\",\"baselineSha\":\"base\",\"status\":\"pending\",\"createdAt\":\"2026-04-14T12:00:00.000Z\",\"expiresAt\":\"2026-04-14T12:30:00.000Z\",\"diff\":{\"reasons\":[\"new env read: SECRET_KEY\",\"new effect: write\"]}}",
    );
    defer plan.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("plan-1", plan.id);
    try std.testing.expectEqualStrings("demo", plan.project_name);
    try std.testing.expectEqualStrings("pending", plan.status);
    try std.testing.expectEqualStrings("base", plan.baseline_sha.?);
    try std.testing.expectEqualStrings("next", plan.proposed_sha);
    try std.testing.expectEqual(@as(usize, 2), plan.diff_reasons.len);
    try std.testing.expectEqualStrings("2026-04-14T12:00:00.000Z", plan.created_at);
    try std.testing.expect(plan.decided_at == null);
}

test "parseCapabilityGrantList extracts grants" {
    var grants = try parseCapabilityGrantList(
        std.testing.allocator,
        "{\"grants\":[{\"id\":\"grant-1\",\"projectName\":\"demo\",\"sourcePlanId\":\"plan-1\",\"createdAt\":\"2026-04-14T12:00:00.000Z\",\"expiresAt\":\"2026-04-15T12:00:00.000Z\",\"status\":\"active\",\"reasons\":[\"new env read: SECRET_KEY\"]}]}",
    );
    defer grants.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 1), grants.items.len);
    try std.testing.expectEqualStrings("grant-1", grants.items[0].id);
    try std.testing.expectEqualStrings("demo", grants.items[0].project_name);
    try std.testing.expectEqualStrings("active", grants.items[0].status);
    try std.testing.expectEqualStrings("2026-04-15T12:00:00.000Z", grants.items[0].expires_at.?);
    try std.testing.expectEqual(@as(usize, 1), grants.items[0].reasons.len);
}

test "parseRevokedGrant accepts wrapper payload" {
    var grant = try parseRevokedGrant(
        std.testing.allocator,
        "{\"grant\":{\"id\":\"grant-1\",\"projectName\":\"demo\",\"sourcePlanId\":\"plan-1\",\"createdAt\":\"2026-04-14T12:00:00.000Z\",\"revokedAt\":\"2026-04-14T13:00:00.000Z\",\"status\":\"revoked\",\"reasons\":[\"new effect: write\"]}}",
    );
    defer grant.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("revoked", grant.status);
    try std.testing.expectEqualStrings("2026-04-14T13:00:00.000Z", grant.revoked_at.?);
}

test "sessionRefreshIfExpiring returns false when no expiry set" {
    const allocator = std.testing.allocator;
    const TestCtx = struct {
        var fetch_calls: usize = 0;
        fn fetchSession(_: std.mem.Allocator, _: []const u8, _: []const u8) !DeploySession {
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
        fn fetchSession(_: std.mem.Allocator, _: []const u8, _: []const u8) !DeploySession {
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
        var received_body: []const u8 = "";
        fn fetchSession(alloc: std.mem.Allocator, token: []const u8, request_body: []const u8) !DeploySession {
            fetch_calls += 1;
            received_token = token;
            received_body = request_body;
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
    TestCtx.received_body = "";

    var session = try makeStubSession(allocator, 1000 + 30);
    allocator.free(session.token);
    session.token = try allocator.dupe(u8, "initial-token");
    defer session.deinit(allocator);

    const refreshed = try sessionRefreshIfExpiringFns(
        allocator,
        &session,
        60,
        TestCtx.now,
        TestCtx.fetchSession,
    );
    try std.testing.expect(refreshed);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.fetch_calls);
    try std.testing.expectEqualStrings("initial-token", TestCtx.received_token);
    try std.testing.expectEqualStrings("{\"projectName\":\"demo\"}", TestCtx.received_body);
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
        .request_body = try allocator.dupe(u8, "{\"projectName\":\"demo\"}"),
        .grant_ids = try allocator.alloc([]u8, 0),
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

test "parseReviewResult accepts nested plan payload" {
    var result = try parseReviewResult(
        std.testing.allocator,
        "{\"plan\":{\"status\":\"approved\"},\"grants\":[{},{}]}",
    );
    defer result.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("approved", result.status);
    try std.testing.expectEqual(@as(usize, 2), result.grants_created);
}

test "parseReviewResult accepts flat plan payload" {
    var result = try parseReviewResult(
        std.testing.allocator,
        "{\"status\":\"rejected\"}",
    );
    defer result.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("rejected", result.status);
    try std.testing.expectEqual(@as(usize, 0), result.grants_created);
}
