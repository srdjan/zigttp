const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const config_mod = @import("deploy/config.zig");
const plan_mod = @import("deploy/plan.zig");
const builder = @import("deploy/builder.zig");
const state = @import("deploy/state.zig");
const northflank_adapter = @import("deploy/northflank_adapter.zig");
const oci_image = @import("deploy/oci/image.zig");
const oci_config = @import("deploy/oci/config.zig");
const registry = @import("deploy/oci/registry.zig");
const auth = @import("deploy/auth.zig");
const control_plane = @import("deploy/control_plane.zig");
const autodetect = @import("deploy/autodetect.zig");
const first_run = @import("deploy/first_run.zig");

const precompile = zigts_cli.precompile;
const deploy_manifest = zigts_cli.deploy_manifest;

const Reconciliation = struct {
    action: plan_mod.ProviderAction,
    warning: ?[]u8 = null,
};

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    try config_mod.parse(argv);

    var credentials = try first_run.ensureSignedIn(allocator);
    defer credentials.deinit(allocator);

    const handler_path = try autodetect.detectHandler(allocator);
    defer allocator.free(handler_path);

    const service_name = try autodetect.detectName(allocator);
    defer allocator.free(service_name);

    var session = try control_plane.fetchDeploySession(allocator, credentials.token, service_name);
    defer session.deinit(allocator);

    const image_repo = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ session.namespace, service_name });
    defer allocator.free(image_repo);

    const env_vars = try config_mod.loadEnvFile(allocator, ".env");
    defer {
        for (env_vars) |item| {
            allocator.free(item.key);
            allocator.free(item.value);
        }
        allocator.free(env_vars);
    }

    stdoutLine("  Handler:    ", handler_path);
    stdoutLine("  Name:       ", service_name);

    const source = try zigts.file_io.readFile(allocator, handler_path, 10 * 1024 * 1024);
    defer allocator.free(source);
    var compiled = try precompile.compileHandler(
        allocator,
        source,
        handler_path,
        false,
        true,
        true,
        null,
        null,
        false,
        null,
    );
    defer compiled.deinit(allocator);
    if (compiled.verify_failed) {
        if (compiled.violations_summary) |summary| {
            _ = std.c.write(std.c.STDERR_FILENO, summary.ptr, summary.len);
        }
        return error.VerificationFailed;
    }
    stdoutLine("  Verifying.. ok", "");

    var proven_extract: ?struct {
        facts: deploy_manifest.ProvenFacts,
        checks_buf: [][]const u8,
        routes_buf: []deploy_manifest.ProvenRoute,
    } = null;
    defer if (proven_extract) |extract| {
        allocator.free(extract.checks_buf);
        allocator.free(extract.routes_buf);
    };

    if (compiled.contract) |*contract| {
        const extract = try deploy_manifest.extractProvenFacts(allocator, contract);
        proven_extract = .{
            .facts = extract.facts,
            .checks_buf = extract.checks_buf,
            .routes_buf = extract.routes_buf,
        };
    }

    const arch: plan_mod.Arch = .amd64;
    var build_result = try builder.buildLinuxArtifact(allocator, handler_path, arch.targetTriple());
    defer build_result.deinit(allocator);
    stdoutLine("  Building... ok", "");

    const image_labels = if (proven_extract) |*extract|
        try buildProofLabels(allocator, &extract.facts)
    else
        try allocator.alloc(oci_config.Label, 0);
    defer freeLabels(allocator, image_labels);

    var image = try oci_image.buildImage(allocator, session.registry_host, image_repo, arch, build_result.binary_bytes, image_labels);
    defer image.deinit(allocator);
    stdoutLine("  Packaging.. ok", "");

    var registry_ref = try registry.makeRegistryRef(allocator, session.registry_host, image_repo);
    defer registry_ref.deinit(allocator);

    var store = try state.load(allocator);
    defer store.deinit(allocator);
    const previous = store.get(session.provider, service_name);

    const reconciliation = try evaluateReconciliation(
        allocator,
        previous,
        session.scope_id,
        session.region,
        session.plan_id,
        env_vars,
    );
    defer if (reconciliation.warning) |warning| allocator.free(warning);
    if (reconciliation.action == .replace_requires_confirm) {
        if (reconciliation.warning) |warning| stderrLine(warning);
        return error.DeployDrift;
    }

    try registry.push(allocator, &registry_ref, &image, session.registry_username, session.registry_password);
    stdoutLine("  Uploading.. ok", "");

    const provider_ctx = northflank_adapter.ProviderContext{
        .name = service_name,
        .region = session.region,
        .image_ref = image.image_digest_ref,
        .env_vars = env_vars,
        .project_id = session.scope_id,
        .plan_id = session.plan_id,
        .registry_credential_id = session.registry_credential_id,
        .state_entry = previous,
    };
    const result = try northflank_adapter.execute(allocator, &provider_ctx, session.provider_api_token);
    defer {
        allocator.free(result.service_id);
        if (result.deployment_id) |value| allocator.free(value);
        if (result.url) |value| allocator.free(value);
        allocator.free(result.status);
        allocator.free(result.image_digest_ref);
    }
    stdoutLine("  Deploying.. ok", "");

    try store.put(allocator, .{
        .provider = session.provider,
        .name = try allocator.dupe(u8, service_name),
        .scope_id = try allocator.dupe(u8, session.scope_id),
        .service_id = try allocator.dupe(u8, result.service_id),
        .region = try allocator.dupe(u8, session.region),
        .plan_id = try allocator.dupe(u8, session.plan_id),
        .url = if (result.url) |value| try allocator.dupe(u8, value) else null,
        .last_image_digest = try allocator.dupe(u8, image.image_digest_ref),
        .managed_env_keys = try managedEnvKeys(allocator, env_vars),
    });
    try state.save(allocator, &store);

    const public_url = result.url orelse session.url_hint;
    if (public_url) |url| stdoutLine("  ", url);
}

fn stdoutLine(prefix: []const u8, value: []const u8) void {
    _ = std.c.write(std.c.STDOUT_FILENO, prefix.ptr, prefix.len);
    if (value.len > 0) _ = std.c.write(std.c.STDOUT_FILENO, value.ptr, value.len);
    _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
}

fn stderrLine(line: []const u8) void {
    _ = std.c.write(std.c.STDERR_FILENO, line.ptr, line.len);
    _ = std.c.write(std.c.STDERR_FILENO, "\n", 1);
}

fn managedEnvKeys(allocator: std.mem.Allocator, env_vars: []const plan_mod.EnvVar) ![]const []const u8 {
    const keys = try allocator.alloc([]const u8, env_vars.len);
    errdefer allocator.free(keys);
    for (env_vars, 0..) |env_var, idx| keys[idx] = try allocator.dupe(u8, env_var.key);
    return keys;
}

fn buildProofLabels(
    allocator: std.mem.Allocator,
    facts: *const deploy_manifest.ProvenFacts,
) ![]oci_config.Label {
    var labels = std.ArrayList(oci_config.Label).empty;
    errdefer {
        for (labels.items) |label| {
            allocator.free(label.key);
            allocator.free(label.value);
        }
        labels.deinit(allocator);
    }

    try appendLabel(allocator, &labels, "zigttp.proof-level", facts.proof_level.toString());
    try appendBoolLabel(allocator, &labels, "zigttp.retry-safe", facts.retry_safe);
    try appendBoolLabel(allocator, &labels, "zigttp.read-only", facts.read_only);
    try appendBoolLabel(allocator, &labels, "zigttp.injection-safe", facts.injection_safe);
    try appendBoolLabel(allocator, &labels, "zigttp.idempotent", facts.idempotent);
    try appendBoolLabel(allocator, &labels, "zigttp.state-isolated", facts.state_isolated);
    try appendBoolLabel(allocator, &labels, "zigttp.results-safe", facts.results_safe);
    try appendBoolLabel(allocator, &labels, "zigttp.fault-covered", facts.fault_covered);
    try appendBoolLabel(allocator, &labels, "zigttp.env-proven", facts.env_proven);
    try appendBoolLabel(allocator, &labels, "zigttp.egress-proven", facts.egress_proven);
    try appendBoolLabel(allocator, &labels, "zigttp.cache-proven", facts.cache_proven);
    if (facts.env_vars.len > 0) try appendJoinedLabel(allocator, &labels, "zigttp.env-vars", facts.env_vars);
    if (facts.egress_hosts.len > 0) try appendJoinedLabel(allocator, &labels, "zigttp.egress-hosts", facts.egress_hosts);
    if (facts.cache_namespaces.len > 0) try appendJoinedLabel(allocator, &labels, "zigttp.cache-namespaces", facts.cache_namespaces);
    if (facts.routes.len > 0) {
        var route_strings = std.ArrayList([]const u8).empty;
        defer route_strings.deinit(allocator);
        for (facts.routes) |route| {
            try route_strings.append(allocator, try std.fmt.allocPrint(allocator, "{s}{s}", .{
                route.pattern,
                if (route.is_prefix) "*" else "",
            }));
        }
        defer for (route_strings.items) |value| allocator.free(value);
        try appendJoinedLabel(allocator, &labels, "zigttp.routes", route_strings.items);
    }
    if (facts.max_io_depth) |depth| {
        try appendLabelOwned(allocator, &labels, "zigttp.max-io-depth", try std.fmt.allocPrint(allocator, "{d}", .{depth}));
    }
    return labels.toOwnedSlice(allocator);
}

fn appendBoolLabel(
    allocator: std.mem.Allocator,
    labels: *std.ArrayList(oci_config.Label),
    key: []const u8,
    value: bool,
) !void {
    try appendLabel(allocator, labels, key, if (value) "true" else "false");
}

fn appendJoinedLabel(
    allocator: std.mem.Allocator,
    labels: *std.ArrayList(oci_config.Label),
    key: []const u8,
    values: []const []const u8,
) !void {
    const joined = try std.mem.join(allocator, ",", values);
    try appendLabelOwned(allocator, labels, key, joined);
}

fn appendLabel(
    allocator: std.mem.Allocator,
    labels: *std.ArrayList(oci_config.Label),
    key: []const u8,
    value: []const u8,
) !void {
    try labels.append(allocator, .{
        .key = try allocator.dupe(u8, key),
        .value = try allocator.dupe(u8, value),
    });
}

fn appendLabelOwned(
    allocator: std.mem.Allocator,
    labels: *std.ArrayList(oci_config.Label),
    key: []const u8,
    value: []u8,
) !void {
    try labels.append(allocator, .{
        .key = try allocator.dupe(u8, key),
        .value = value,
    });
}

fn freeLabels(allocator: std.mem.Allocator, labels: []const oci_config.Label) void {
    for (labels) |label| {
        allocator.free(label.key);
        allocator.free(label.value);
    }
    allocator.free(labels);
}

fn evaluateReconciliation(
    allocator: std.mem.Allocator,
    previous: ?*const state.Entry,
    scope_id: []const u8,
    region: []const u8,
    plan_id: []const u8,
    env_vars: []const plan_mod.EnvVar,
) !Reconciliation {
    const entry = previous orelse return .{ .action = .create };

    var reasons = std.ArrayList([]const u8).empty;
    defer {
        for (reasons.items) |reason| allocator.free(reason);
        reasons.deinit(allocator);
    }

    if (!std.mem.eql(u8, entry.scope_id, scope_id)) {
        try reasons.append(allocator, try std.fmt.allocPrint(allocator, "scope changed from {s} to {s}", .{ entry.scope_id, scope_id }));
    }
    if (entry.region) |previous_region| {
        if (!std.mem.eql(u8, previous_region, region)) {
            try reasons.append(allocator, try std.fmt.allocPrint(allocator, "region changed from {s} to {s}", .{ previous_region, region }));
        }
    }
    if (entry.plan_id) |previous_plan_id| {
        if (!std.mem.eql(u8, previous_plan_id, plan_id)) {
            try reasons.append(allocator, try std.fmt.allocPrint(allocator, "plan changed from {s} to {s}", .{ previous_plan_id, plan_id }));
        }
    }
    if (try removedManagedEnvKeysWarning(allocator, entry.managed_env_keys, env_vars)) |warning| {
        try reasons.append(allocator, warning);
    }

    if (reasons.items.len == 0) return .{ .action = .update };

    const joined = try std.mem.join(allocator, "; ", reasons.items);
    defer allocator.free(joined);
    return .{
        .action = .replace_requires_confirm,
        .warning = try std.fmt.allocPrint(allocator, "This service was created with different settings ({s}). Run `zigttp logout` and contact support.", .{joined}),
    };
}

fn removedManagedEnvKeysWarning(
    allocator: std.mem.Allocator,
    previous_keys: []const []const u8,
    env_vars: []const plan_mod.EnvVar,
) !?[]u8 {
    var removed = std.ArrayList([]const u8).empty;
    defer {
        for (removed.items) |key| allocator.free(key);
        removed.deinit(allocator);
    }

    for (previous_keys) |key| {
        if (!envVarsContainKey(env_vars, key)) {
            try removed.append(allocator, try allocator.dupe(u8, key));
        }
    }
    if (removed.items.len == 0) return null;

    const joined = try std.mem.join(allocator, ", ", removed.items);
    defer allocator.free(joined);
    return try std.fmt.allocPrint(allocator, "managed env keys would be removed: {s}", .{joined});
}

fn envVarsContainKey(env_vars: []const plan_mod.EnvVar, key: []const u8) bool {
    for (env_vars) |env_var| {
        if (std.mem.eql(u8, env_var.key, key)) return true;
    }
    return false;
}

test "evaluate reconciliation flags state drift" {
    const env_vars = [_]plan_mod.EnvVar{
        .{ .key = "KEEP", .value = "1" },
    };
    const managed = try std.testing.allocator.alloc([]const u8, 2);
    managed[0] = try std.testing.allocator.dupe(u8, "KEEP");
    managed[1] = try std.testing.allocator.dupe(u8, "DROP");
    var entry = state.Entry{
        .provider = .northflank,
        .name = try std.testing.allocator.dupe(u8, "demo"),
        .scope_id = try std.testing.allocator.dupe(u8, "owner-1"),
        .service_id = try std.testing.allocator.dupe(u8, "srv-1"),
        .region = try std.testing.allocator.dupe(u8, "us-east"),
        .plan_id = try std.testing.allocator.dupe(u8, "starter"),
        .url = null,
        .last_image_digest = null,
        .managed_env_keys = managed,
    };
    defer entry.deinit(std.testing.allocator);

    const reconciliation = try evaluateReconciliation(std.testing.allocator, &entry, "owner-2", "eu-west", "pro", &env_vars);
    defer if (reconciliation.warning) |warning| std.testing.allocator.free(warning);

    try std.testing.expectEqual(plan_mod.ProviderAction.replace_requires_confirm, reconciliation.action);
    try std.testing.expect(reconciliation.warning != null);
    try std.testing.expect(std.mem.indexOf(u8, reconciliation.warning.?, "scope changed") != null);
    try std.testing.expect(std.mem.indexOf(u8, reconciliation.warning.?, "zigttp logout") != null);
}

test "evaluate reconciliation stays update when state matches" {
    const env_vars = [_]plan_mod.EnvVar{
        .{ .key = "KEEP", .value = "1" },
    };
    const managed = try std.testing.allocator.alloc([]const u8, 1);
    managed[0] = try std.testing.allocator.dupe(u8, "KEEP");
    var entry = state.Entry{
        .provider = .northflank,
        .name = try std.testing.allocator.dupe(u8, "demo"),
        .scope_id = try std.testing.allocator.dupe(u8, "owner-1"),
        .service_id = try std.testing.allocator.dupe(u8, "srv-1"),
        .region = try std.testing.allocator.dupe(u8, "us-east"),
        .plan_id = try std.testing.allocator.dupe(u8, "starter"),
        .url = null,
        .last_image_digest = null,
        .managed_env_keys = managed,
    };
    defer entry.deinit(std.testing.allocator);

    const reconciliation = try evaluateReconciliation(std.testing.allocator, &entry, "owner-1", "us-east", "starter", &env_vars);
    defer if (reconciliation.warning) |warning| std.testing.allocator.free(warning);

    try std.testing.expectEqual(plan_mod.ProviderAction.update, reconciliation.action);
    try std.testing.expect(reconciliation.warning == null);
}

pub fn logout(allocator: std.mem.Allocator) !void {
    try auth.clear(allocator);
    const msg = "Signed out.\n";
    _ = std.c.write(std.c.STDOUT_FILENO, msg.ptr, msg.len);
}
