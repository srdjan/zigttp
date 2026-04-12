const std = @import("std");
const redact = @import("redact.zig");

pub const Provider = enum {
    render,
    northflank,

    pub fn fromString(raw: []const u8) ?Provider {
        if (std.mem.eql(u8, raw, "render")) return .render;
        if (std.mem.eql(u8, raw, "northflank")) return .northflank;
        return null;
    }

    pub fn toString(self: Provider) []const u8 {
        return @tagName(self);
    }
};

pub const Arch = enum {
    amd64,
    arm64,

    pub fn fromString(raw: []const u8) ?Arch {
        if (std.mem.eql(u8, raw, "amd64")) return .amd64;
        if (std.mem.eql(u8, raw, "arm64")) return .arm64;
        return null;
    }

    pub fn toString(self: Arch) []const u8 {
        return @tagName(self);
    }

    pub fn targetTriple(self: Arch) []const u8 {
        return switch (self) {
            .amd64 => "x86_64-linux-musl",
            .arm64 => "aarch64-linux-musl",
        };
    }

    pub fn ociArchitecture(self: Arch) []const u8 {
        return switch (self) {
            .amd64 => "amd64",
            .arm64 => "arm64",
        };
    }
};

pub const EnvVar = struct {
    key: []const u8,
    value: []const u8,
};

pub const ProofRoute = struct {
    pattern: []const u8,
    is_prefix: bool,
};

pub const ProofSummary = struct {
    handler_name: []const u8,
    handler_path: []const u8,
    proof_level: []const u8,
    env_vars: []const []const u8,
    env_proven: bool,
    egress_hosts: []const []const u8,
    egress_proven: bool,
    cache_namespaces: []const []const u8,
    cache_proven: bool,
    checks_passed: []const []const u8,
    routes: []const ProofRoute,
    retry_safe: bool,
    read_only: bool,
    injection_safe: bool,
    idempotent: bool,
    state_isolated: bool,
    no_secret_leakage: bool,
    no_credential_leakage: bool,
    input_validated: bool,
    pii_contained: bool,
    results_safe: bool,
    fault_covered: bool,
    max_io_depth: ?u32 = null,
    rate_limit_namespace: ?[]const u8 = null,
};

pub const HttpRequestPreview = struct {
    method: []const u8,
    url: []const u8,
    body_json: ?[]const u8 = null,
};

pub const ProviderAction = enum {
    create,
    update,
    replace_requires_confirm,

    pub fn toString(self: ProviderAction) []const u8 {
        return @tagName(self);
    }
};

pub const ArtifactPlan = struct {
    build_command: []const u8,
    target_triple: []const u8,
    binary_path: []const u8,
    binary_sha256: []const u8,
    image_digest_ref: []const u8,
    config_digest: []const u8,
    layer_digest: []const u8,
    manifest_digest: []const u8,
    layer_size: usize,
    manifest_size: usize,
};

pub const ProviderPlan = struct {
    action: ProviderAction,
    requests: []const HttpRequestPreview,
};

pub const DeployPlan = struct {
    provider: Provider,
    name: []const u8,
    handler_path: []const u8,
    region: ?[]const u8,
    registry: []const u8,
    arch: Arch,
    dry_run: bool,
    json: bool,
    confirm: bool,
    env_vars: []const EnvVar,
    artifact: ArtifactPlan,
    registry_requests: []const HttpRequestPreview,
    provider_plan: ProviderPlan,
    warning: ?[]const u8 = null,
    proof: ?ProofSummary = null,
    proof_report: ?[]const u8 = null,
};

pub const DeployResult = struct {
    provider: Provider,
    name: []const u8,
    service_id: []const u8,
    deployment_id: ?[]const u8,
    url: ?[]const u8,
    status: []const u8,
    image_digest_ref: []const u8,
};

pub fn writePlanJson(writer: *std.Io.Writer, plan: *const DeployPlan) !void {
    var json: std.json.Stringify = .{ .writer = writer };
    try json.beginObject();
    try json.objectField("provider");
    try json.write(plan.provider.toString());
    try json.objectField("name");
    try json.write(plan.name);
    try json.objectField("handler");
    try json.write(plan.handler_path);
    if (plan.region) |region| {
        try json.objectField("region");
        try json.write(region);
    }
    try json.objectField("registry");
    try json.write(plan.registry);
    try json.objectField("arch");
    try json.write(plan.arch.toString());
    try json.objectField("dryRun");
    try json.write(plan.dry_run);
    try json.objectField("confirm");
    try json.write(plan.confirm);

    try json.objectField("env");
    try json.beginArray();
    for (plan.env_vars) |env_var| {
        try json.beginObject();
        try json.objectField("key");
        try json.write(env_var.key);
        try json.objectField("value");
        try json.write(redact.redactSecret(env_var.value));
        try json.endObject();
    }
    try json.endArray();

    try json.objectField("artifact");
    try json.beginObject();
    try json.objectField("buildCommand");
    try json.write(plan.artifact.build_command);
    try json.objectField("targetTriple");
    try json.write(plan.artifact.target_triple);
    try json.objectField("binaryPath");
    try json.write(plan.artifact.binary_path);
    try json.objectField("binarySha256");
    try json.write(plan.artifact.binary_sha256);
    try json.objectField("imageDigestRef");
    try json.write(plan.artifact.image_digest_ref);
    try json.objectField("configDigest");
    try json.write(plan.artifact.config_digest);
    try json.objectField("layerDigest");
    try json.write(plan.artifact.layer_digest);
    try json.objectField("manifestDigest");
    try json.write(plan.artifact.manifest_digest);
    try json.objectField("layerSize");
    try json.write(plan.artifact.layer_size);
    try json.objectField("manifestSize");
    try json.write(plan.artifact.manifest_size);
    try json.endObject();

    try json.objectField("registryRequests");
    try writeRequestPreviews(&json, plan.registry_requests);
    try json.objectField("providerPlan");
    try json.beginObject();
    try json.objectField("action");
    try json.write(plan.provider_plan.action.toString());
    try json.objectField("requests");
    try writeRequestPreviews(&json, plan.provider_plan.requests);
    if (plan.warning) |warning| {
        try json.objectField("warning");
        try json.write(warning);
    }
    try json.endObject();
    if (plan.proof) |proof| {
        try json.objectField("proof");
        try writeProofSummary(&json, &proof);
    }
    if (plan.proof_report) |report| {
        try json.objectField("proofReport");
        try json.write(report);
    }
    try json.endObject();
}

fn writeRequestPreviews(json: *std.json.Stringify, requests: []const HttpRequestPreview) !void {
    try json.beginArray();
    for (requests) |request| {
        try json.beginObject();
        try json.objectField("method");
        try json.write(request.method);
        try json.objectField("url");
        try json.write(request.url);
        if (request.body_json) |body| {
            try json.objectField("body");
            try json.write(body);
        }
        try json.endObject();
    }
    try json.endArray();
}

fn writeProofSummary(json: *std.json.Stringify, proof: *const ProofSummary) !void {
    try json.beginObject();
    try json.objectField("handlerName");
    try json.write(proof.handler_name);
    try json.objectField("handlerPath");
    try json.write(proof.handler_path);
    try json.objectField("proofLevel");
    try json.write(proof.proof_level);
    try json.objectField("envProven");
    try json.write(proof.env_proven);
    try json.objectField("envVars");
    try writeStringArray(json, proof.env_vars);
    try json.objectField("egressProven");
    try json.write(proof.egress_proven);
    try json.objectField("egressHosts");
    try writeStringArray(json, proof.egress_hosts);
    try json.objectField("cacheProven");
    try json.write(proof.cache_proven);
    try json.objectField("cacheNamespaces");
    try writeStringArray(json, proof.cache_namespaces);
    try json.objectField("checksPassed");
    try writeStringArray(json, proof.checks_passed);
    try json.objectField("routes");
    try json.beginArray();
    for (proof.routes) |route| {
        try json.beginObject();
        try json.objectField("pattern");
        try json.write(route.pattern);
        try json.objectField("isPrefix");
        try json.write(route.is_prefix);
        try json.endObject();
    }
    try json.endArray();
    try json.objectField("retrySafe");
    try json.write(proof.retry_safe);
    try json.objectField("readOnly");
    try json.write(proof.read_only);
    try json.objectField("injectionSafe");
    try json.write(proof.injection_safe);
    try json.objectField("idempotent");
    try json.write(proof.idempotent);
    try json.objectField("stateIsolated");
    try json.write(proof.state_isolated);
    try json.objectField("noSecretLeakage");
    try json.write(proof.no_secret_leakage);
    try json.objectField("noCredentialLeakage");
    try json.write(proof.no_credential_leakage);
    try json.objectField("inputValidated");
    try json.write(proof.input_validated);
    try json.objectField("piiContained");
    try json.write(proof.pii_contained);
    try json.objectField("resultsSafe");
    try json.write(proof.results_safe);
    try json.objectField("faultCovered");
    try json.write(proof.fault_covered);
    if (proof.max_io_depth) |depth| {
        try json.objectField("maxIoDepth");
        try json.write(depth);
    }
    if (proof.rate_limit_namespace) |ns| {
        try json.objectField("rateLimitNamespace");
        try json.write(ns);
    }
    try json.endObject();
}

fn writeStringArray(json: *std.json.Stringify, values: []const []const u8) !void {
    try json.beginArray();
    for (values) |value| try json.write(value);
    try json.endArray();
}

pub fn mergeManagedEnvVars(
    allocator: std.mem.Allocator,
    managed_keys: []const []const u8,
    remote: []const EnvVar,
    requested: []const EnvVar,
) ![]EnvVar {
    var out = std.ArrayList(EnvVar).empty;
    errdefer {
        for (out.items) |item| {
            allocator.free(item.key);
            allocator.free(item.value);
        }
        out.deinit(allocator);
    }

    for (remote) |entry| {
        if (keyContains(managed_keys, entry.key)) continue;
        try out.append(allocator, .{
            .key = try allocator.dupe(u8, entry.key),
            .value = try allocator.dupe(u8, entry.value),
        });
    }
    for (requested) |entry| {
        try out.append(allocator, .{
            .key = try allocator.dupe(u8, entry.key),
            .value = try allocator.dupe(u8, entry.value),
        });
    }

    return out.toOwnedSlice(allocator);
}

fn keyContains(keys: []const []const u8, key: []const u8) bool {
    for (keys) |candidate| {
        if (std.mem.eql(u8, candidate, key)) return true;
    }
    return false;
}

test "mergeManagedEnvVars preserves unmanaged remote keys and overwrites managed" {
    const managed = [_][]const u8{"FOO"};
    const remote = [_]EnvVar{
        .{ .key = "FOO", .value = "old" },
        .{ .key = "BAR", .value = "keep" },
    };
    const requested = [_]EnvVar{
        .{ .key = "FOO", .value = "new" },
    };

    const merged = try mergeManagedEnvVars(std.testing.allocator, &managed, &remote, &requested);
    defer {
        for (merged) |item| {
            std.testing.allocator.free(item.key);
            std.testing.allocator.free(item.value);
        }
        std.testing.allocator.free(merged);
    }

    try std.testing.expectEqual(@as(usize, 2), merged.len);
    try std.testing.expectEqualStrings("BAR", merged[0].key);
    try std.testing.expectEqualStrings("keep", merged[0].value);
    try std.testing.expectEqualStrings("FOO", merged[1].key);
    try std.testing.expectEqualStrings("new", merged[1].value);
}

test "mergeManagedEnvVars drops no keys when no managed list matches" {
    const managed = [_][]const u8{};
    const remote = [_]EnvVar{.{ .key = "KEEP", .value = "yes" }};
    const requested = [_]EnvVar{.{ .key = "NEW", .value = "val" }};

    const merged = try mergeManagedEnvVars(std.testing.allocator, &managed, &remote, &requested);
    defer {
        for (merged) |item| {
            std.testing.allocator.free(item.key);
            std.testing.allocator.free(item.value);
        }
        std.testing.allocator.free(merged);
    }

    try std.testing.expectEqual(@as(usize, 2), merged.len);
}

test "writePlanJson redacts env values and serializes reconcile action" {
    const env_vars = [_]EnvVar{.{ .key = "SECRET", .value = "hunter2" }};
    const deploy_plan = DeployPlan{
        .provider = .render,
        .name = "demo",
        .handler_path = "src/handler.ts",
        .region = "oregon",
        .registry = "ghcr.io/acme/demo",
        .arch = .amd64,
        .dry_run = true,
        .json = true,
        .confirm = false,
        .env_vars = &env_vars,
        .artifact = .{
            .build_command = "zig build -Dhandler=src/handler.ts",
            .target_triple = "x86_64-linux-musl",
            .binary_path = "/tmp/zigttp",
            .binary_sha256 = "sha256:abc",
            .image_digest_ref = "ghcr.io/acme/demo@sha256:def",
            .config_digest = "sha256:c",
            .layer_digest = "sha256:l",
            .manifest_digest = "sha256:m",
            .layer_size = 100,
            .manifest_size = 200,
        },
        .registry_requests = &.{},
        .provider_plan = .{ .action = .replace_requires_confirm, .requests = &.{} },
    };

    var aw: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer aw.deinit();
    try writePlanJson(&aw.writer, &deploy_plan);
    const bytes = aw.writer.buffered();

    try std.testing.expect(std.mem.indexOf(u8, bytes, "\"replace_requires_confirm\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, bytes, "\"SECRET\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, bytes, "hunter2") == null);
    try std.testing.expect(std.mem.indexOf(u8, bytes, "<redacted>") != null);
}
