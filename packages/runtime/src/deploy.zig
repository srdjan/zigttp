const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const config_mod = @import("deploy/config.zig");
const plan_mod = @import("deploy/plan.zig");
const builder = @import("deploy/builder.zig");
const state = @import("deploy/state.zig");
const render_adapter = @import("deploy/render_adapter.zig");
const northflank_adapter = @import("deploy/northflank_adapter.zig");
const oci_image = @import("deploy/oci/image.zig");
const oci_config = @import("deploy/oci/config.zig");
const registry = @import("deploy/oci/registry.zig");

const precompile = zigts_cli.precompile;
const deploy_manifest = zigts_cli.deploy_manifest;
const Reconciliation = struct {
    action: plan_mod.ProviderAction,
    warning: ?[]u8 = null,
};

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var config = config_mod.parse(allocator, argv) catch |err| switch (err) {
        error.HelpRequested => return error.HelpRequested,
        else => return err,
    };
    defer config.deinit(allocator);

    const registry_value = config.registry orelse return error.MissingRegistry;
    const env_vars = try config_mod.loadEnvFile(allocator, config.env_file);
    defer {
        for (env_vars) |item| {
            allocator.free(item.key);
            allocator.free(item.value);
        }
        allocator.free(env_vars);
    }

    const source = try zigts.file_io.readFile(allocator, config.handler_path, 10 * 1024 * 1024);
    defer allocator.free(source);
    var compiled = try precompile.compileHandler(
        allocator,
        source,
        config.handler_path,
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

    var proven_extract: ?struct {
        facts: deploy_manifest.ProvenFacts,
        checks_buf: [][]const u8,
        routes_buf: []deploy_manifest.ProvenRoute,
    } = null;
    defer if (proven_extract) |extract| {
        allocator.free(extract.checks_buf);
        allocator.free(extract.routes_buf);
    };

    var proof_report: ?[]u8 = null;
    defer if (proof_report) |report| allocator.free(report);
    var proof_summary: ?plan_mod.ProofSummary = null;

    if (compiled.contract) |*contract| {
        const extract = try deploy_manifest.extractProvenFacts(allocator, contract);
        proven_extract = .{
            .facts = extract.facts,
            .checks_buf = extract.checks_buf,
            .routes_buf = extract.routes_buf,
        };
        proof_summary = toProofSummary(&proven_extract.?.facts);
        proof_report = try renderProofReport(allocator, &proven_extract.?.facts, config.provider.toString());
    }

    const tag = if (config.tag) |value|
        try allocator.dupe(u8, value)
    else
        try defaultTag(allocator, compiled.bytecode);
    defer allocator.free(tag);

    var build_result = try builder.buildLinuxArtifact(allocator, config.handler_path, config.arch.targetTriple());
    defer build_result.deinit(allocator);

    const image_labels = if (proven_extract) |*extract|
        try buildProofLabels(allocator, &extract.facts)
    else
        try allocator.alloc(oci_config.Label, 0);
    defer freeLabels(allocator, image_labels);

    var image = try oci_image.buildImage(allocator, registry_value, tag, config.arch, build_result.binary_bytes, image_labels);
    defer image.deinit(allocator);

    var registry_ref = try registry.parseRegistryRef(allocator, registry_value);
    defer registry_ref.deinit(allocator);
    const registry_requests = try registry.buildRequestPreviews(allocator, &registry_ref, &image);
    defer freeRequestPreviews(allocator, registry_requests);

    var store = try state.load(allocator);
    defer store.deinit(allocator);
    const previous = store.get(config.provider, config.name);

    const region = config.region orelse switch (config.provider) {
        .render => "oregon",
        .northflank => "us-east",
    };

    const scope_id = switch (config.provider) {
        .render => try readEnv(allocator, "RENDER_WORKSPACE_ID"),
        .northflank => try readEnv(allocator, "NORTHFLANK_PROJECT_ID"),
    };
    defer allocator.free(scope_id);
    const plan_id = switch (config.provider) {
        .render => try readEnv(allocator, "RENDER_PLAN"),
        .northflank => try readEnv(allocator, "NORTHFLANK_PLAN_ID"),
    };
    defer allocator.free(plan_id);

    const reconciliation = try evaluateReconciliation(allocator, previous, scope_id, region, plan_id, env_vars);
    defer if (reconciliation.warning) |warning| allocator.free(warning);

    var provider_plan = switch (config.provider) {
        .render => try buildRenderPlan(allocator, config.name, region, image.image_digest_ref, env_vars, scope_id, plan_id, previous),
        .northflank => try buildNorthflankPlan(allocator, config.name, region, image.image_digest_ref, env_vars, scope_id, plan_id, previous),
    };
    defer freeRequestPreviews(allocator, provider_plan.requests);
    provider_plan.action = reconciliation.action;

    const deploy_plan = plan_mod.DeployPlan{
        .provider = config.provider,
        .name = config.name,
        .handler_path = config.handler_path,
        .region = region,
        .registry = registry_value,
        .arch = config.arch,
        .dry_run = config.dry_run,
        .json = config.json,
        .confirm = config.confirm,
        .env_vars = env_vars,
        .artifact = .{
            .build_command = build_result.build_command,
            .target_triple = config.arch.targetTriple(),
            .binary_path = build_result.binary_path,
            .binary_sha256 = build_result.binary_sha256,
            .image_tag_ref = image.image_tag_ref,
            .image_digest_ref = image.image_digest_ref,
            .config_digest = image.config_blob.digest,
            .layer_digest = image.layer_blob.digest,
            .manifest_digest = image.manifest_blob.digest,
            .layer_size = image.layer_blob.gzip_bytes.len,
            .manifest_size = image.manifest_blob.bytes.len,
        },
        .registry_requests = registry_requests,
        .provider_plan = provider_plan,
        .warning = reconciliation.warning,
        .proof = proof_summary,
        .proof_report = proof_report,
    };

    if (config.json) {
        emitPlanJson(allocator, &deploy_plan) catch |err| return err;
        if (config.dry_run) return;
    } else if (config.dry_run) {
        std.debug.print("Provider: {s}\n", .{config.provider.toString()});
        std.debug.print("Image:    {s}\n", .{image.image_digest_ref});
        std.debug.print("Build:    {s}\n", .{build_result.build_command});
        std.debug.print("Action:   {s}\n", .{provider_plan.action.toString()});
        if (reconciliation.warning) |warning| {
            std.debug.print("Warning:  {s}\n", .{warning});
        }
        if (proof_report) |report| {
            std.debug.print("\n{s}\n", .{report});
        }
        return;
    }

    if (reconciliation.action == .replace_requires_confirm and !config.confirm) {
        if (reconciliation.warning) |warning| {
            std.debug.print("{s}\n", .{warning});
        } else {
            std.debug.print("This deploy requires --confirm.\n", .{});
        }
        return error.ConfirmRequired;
    }

    const registry_user = try requireEnvOwned(allocator, "ZIGTTP_REGISTRY_USER");
    defer allocator.free(registry_user);
    const registry_token = try requireEnvOwned(allocator, "ZIGTTP_REGISTRY_TOKEN");
    defer allocator.free(registry_token);
    try registry.push(allocator, &registry_ref, &image, registry_user, registry_token);

    const result = switch (config.provider) {
        .render => try executeRender(allocator, config.name, region, image.image_digest_ref, env_vars, previous),
        .northflank => try executeNorthflank(allocator, config.name, region, image.image_digest_ref, env_vars, previous),
    };
    defer {
        allocator.free(result.service_id);
        if (result.deployment_id) |value| allocator.free(value);
        if (result.url) |value| allocator.free(value);
        allocator.free(result.status);
        allocator.free(result.image_digest_ref);
    }

    try store.put(allocator, .{
        .provider = config.provider,
        .name = try allocator.dupe(u8, config.name),
        .scope_id = try allocator.dupe(u8, switch (config.provider) {
            .render => try readEnv(allocator, "RENDER_WORKSPACE_ID"),
            .northflank => try readEnv(allocator, "NORTHFLANK_PROJECT_ID"),
        }),
        .service_id = try allocator.dupe(u8, result.service_id),
        .region = try allocator.dupe(u8, region),
        .plan_id = try allocator.dupe(u8, switch (config.provider) {
            .render => try readEnv(allocator, "RENDER_PLAN"),
            .northflank => try readEnv(allocator, "NORTHFLANK_PLAN_ID"),
        }),
        .url = if (result.url) |value| try allocator.dupe(u8, value) else null,
        .last_image_digest = try allocator.dupe(u8, image.image_digest_ref),
        .managed_env_keys = try managedEnvKeys(allocator, env_vars),
    });
    try state.save(allocator, &store);

    if (config.json) {
        try emitResultJson(allocator, &result);
        return;
    }

    std.debug.print("Deployed {s} via {s}\n", .{ config.name, config.provider.toString() });
    std.debug.print("Service:  {s}\n", .{result.service_id});
    if (result.url) |url| std.debug.print("URL:      {s}\n", .{url});
}

fn buildRenderPlan(
    allocator: std.mem.Allocator,
    name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const plan_mod.EnvVar,
    scope_id: []const u8,
    plan_id: []const u8,
    previous: ?*const state.Entry,
) !plan_mod.ProviderPlan {
    const registry_credential_id = optionalEnvOwned(allocator, "RENDER_REGISTRY_CREDENTIAL_ID") catch null;
    defer if (registry_credential_id) |value| allocator.free(value);
    return render_adapter.buildPlan(allocator, &.{
        .name = name,
        .region = region,
        .image_ref = image_ref,
        .env_vars = env_vars,
        .scope_id = scope_id,
        .plan_id = plan_id,
        .registry_credential_id = registry_credential_id,
        .state_entry = previous,
    });
}

fn buildNorthflankPlan(
    allocator: std.mem.Allocator,
    name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const plan_mod.EnvVar,
    project_id: []const u8,
    plan_id: []const u8,
    previous: ?*const state.Entry,
) !plan_mod.ProviderPlan {
    const registry_credential_id = optionalEnvOwned(allocator, "NORTHFLANK_REGISTRY_CREDENTIAL_ID") catch null;
    defer if (registry_credential_id) |value| allocator.free(value);
    return northflank_adapter.buildPlan(allocator, &.{
        .name = name,
        .region = region,
        .image_ref = image_ref,
        .env_vars = env_vars,
        .project_id = project_id,
        .plan_id = plan_id,
        .registry_credential_id = registry_credential_id,
        .state_entry = previous,
    });
}

fn executeRender(
    allocator: std.mem.Allocator,
    name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const plan_mod.EnvVar,
    previous: ?*const state.Entry,
) !plan_mod.DeployResult {
    const scope_id = try requireEnvOwned(allocator, "RENDER_WORKSPACE_ID");
    defer allocator.free(scope_id);
    const plan_id = try requireEnvOwned(allocator, "RENDER_PLAN");
    defer allocator.free(plan_id);
    const api_key = try requireEnvOwned(allocator, "RENDER_API_KEY");
    defer allocator.free(api_key);
    const registry_credential_id = optionalEnvOwned(allocator, "RENDER_REGISTRY_CREDENTIAL_ID") catch null;
    defer if (registry_credential_id) |value| allocator.free(value);
    return render_adapter.execute(allocator, &.{
        .name = name,
        .region = region,
        .image_ref = image_ref,
        .env_vars = env_vars,
        .scope_id = scope_id,
        .plan_id = plan_id,
        .registry_credential_id = registry_credential_id,
        .state_entry = previous,
    }, api_key);
}

fn executeNorthflank(
    allocator: std.mem.Allocator,
    name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const plan_mod.EnvVar,
    previous: ?*const state.Entry,
) !plan_mod.DeployResult {
    const project_id = try requireEnvOwned(allocator, "NORTHFLANK_PROJECT_ID");
    defer allocator.free(project_id);
    const plan_id = try requireEnvOwned(allocator, "NORTHFLANK_PLAN_ID");
    defer allocator.free(plan_id);
    const api_token = try requireEnvOwned(allocator, "NORTHFLANK_API_TOKEN");
    defer allocator.free(api_token);
    const registry_credential_id = optionalEnvOwned(allocator, "NORTHFLANK_REGISTRY_CREDENTIAL_ID") catch null;
    defer if (registry_credential_id) |value| allocator.free(value);
    return northflank_adapter.execute(allocator, &.{
        .name = name,
        .region = region,
        .image_ref = image_ref,
        .env_vars = env_vars,
        .project_id = project_id,
        .plan_id = plan_id,
        .registry_credential_id = registry_credential_id,
        .state_entry = previous,
    }, api_token);
}

fn managedEnvKeys(allocator: std.mem.Allocator, env_vars: []const plan_mod.EnvVar) ![]const []const u8 {
    const keys = try allocator.alloc([]const u8, env_vars.len);
    errdefer allocator.free(keys);
    for (env_vars, 0..) |env_var, idx| keys[idx] = try allocator.dupe(u8, env_var.key);
    return keys;
}

fn toProofSummary(facts: *const deploy_manifest.ProvenFacts) plan_mod.ProofSummary {
    return .{
        .handler_name = facts.handler_name,
        .handler_path = facts.handler_path,
        .proof_level = facts.proof_level.toString(),
        .env_vars = facts.env_vars,
        .env_proven = facts.env_proven,
        .egress_hosts = facts.egress_hosts,
        .egress_proven = facts.egress_proven,
        .cache_namespaces = facts.cache_namespaces,
        .cache_proven = facts.cache_proven,
        .checks_passed = facts.checks_passed,
        .routes = @ptrCast(facts.routes),
        .retry_safe = facts.retry_safe,
        .read_only = facts.read_only,
        .injection_safe = facts.injection_safe,
        .idempotent = facts.idempotent,
        .state_isolated = facts.state_isolated,
        .no_secret_leakage = facts.no_secret_leakage,
        .no_credential_leakage = facts.no_credential_leakage,
        .input_validated = facts.input_validated,
        .pii_contained = facts.pii_contained,
        .results_safe = facts.results_safe,
        .fault_covered = facts.fault_covered,
        .max_io_depth = facts.max_io_depth,
        .rate_limit_namespace = facts.rate_limit_namespace,
    };
}

fn renderProofReport(
    allocator: std.mem.Allocator,
    facts: *const deploy_manifest.ProvenFacts,
    provider: []const u8,
) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try deploy_manifest.writeDeployReport(&aw.writer, facts, provider);
    return aw.toOwnedSlice();
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
        .warning = try std.fmt.allocPrint(allocator, "Changes require --confirm: {s}", .{joined}),
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

fn freeRequestPreviews(allocator: std.mem.Allocator, requests: []const plan_mod.HttpRequestPreview) void {
    for (requests) |request| {
        allocator.free(request.method);
        allocator.free(request.url);
        if (request.body_json) |body| allocator.free(body);
    }
    allocator.free(requests);
}

fn defaultTag(allocator: std.mem.Allocator, bytecode: []const u8) ![]u8 {
    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytecode, &digest, .{});
    const hex = std.fmt.bytesToHex(digest, .lower);
    return std.fmt.allocPrint(allocator, "sha256-{s}", .{hex[0..12]});
}

fn requireEnvOwned(allocator: std.mem.Allocator, name: []const u8) ![]u8 {
    return (try optionalEnvOwned(allocator, name)) orelse error.MissingEnvironmentVariable;
}

fn optionalEnvOwned(allocator: std.mem.Allocator, name: []const u8) !?[]u8 {
    const name_z = try allocator.dupeZ(u8, name);
    defer allocator.free(name_z);
    const raw = std.c.getenv(name_z) orelse return null;
    return try allocator.dupe(u8, std.mem.sliceTo(raw, 0));
}

fn readEnv(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    const value = try requireEnvOwned(allocator, name);
    return value;
}

fn writeResultJson(writer: *std.Io.Writer, result: *const plan_mod.DeployResult) !void {
    var json: std.json.Stringify = .{ .writer = writer };
    try json.beginObject();
    try json.objectField("provider");
    try json.write(result.provider.toString());
    try json.objectField("name");
    try json.write(result.name);
    try json.objectField("serviceId");
    try json.write(result.service_id);
    if (result.deployment_id) |value| {
        try json.objectField("deploymentId");
        try json.write(value);
    }
    if (result.url) |value| {
        try json.objectField("url");
        try json.write(value);
    }
    try json.objectField("status");
    try json.write(result.status);
    try json.objectField("image");
    try json.write(result.image_digest_ref);
    try json.endObject();
}

fn emitPlanJson(allocator: std.mem.Allocator, deploy_plan: *const plan_mod.DeployPlan) !void {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try plan_mod.writePlanJson(&aw.writer, deploy_plan);
    try aw.writer.writeByte('\n');
    const bytes = try aw.toOwnedSlice();
    defer allocator.free(bytes);
    _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
}

fn emitResultJson(allocator: std.mem.Allocator, result: *const plan_mod.DeployResult) !void {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try writeResultJson(&aw.writer, result);
    try aw.writer.writeByte('\n');
    const bytes = try aw.toOwnedSlice();
    defer allocator.free(bytes);
    _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
}

test "evaluate reconciliation requests confirm for state drift" {
    const env_vars = [_]plan_mod.EnvVar{
        .{ .key = "KEEP", .value = "1" },
    };
    const managed = try std.testing.allocator.alloc([]const u8, 2);
    managed[0] = try std.testing.allocator.dupe(u8, "KEEP");
    managed[1] = try std.testing.allocator.dupe(u8, "DROP");
    var entry = state.Entry{
        .provider = .render,
        .name = try std.testing.allocator.dupe(u8, "demo"),
        .scope_id = try std.testing.allocator.dupe(u8, "owner-1"),
        .service_id = try std.testing.allocator.dupe(u8, "srv-1"),
        .region = try std.testing.allocator.dupe(u8, "oregon"),
        .plan_id = try std.testing.allocator.dupe(u8, "starter"),
        .url = null,
        .last_image_digest = null,
        .managed_env_keys = managed,
    };
    defer entry.deinit(std.testing.allocator);

    const reconciliation = try evaluateReconciliation(std.testing.allocator, &entry, "owner-2", "frankfurt", "pro", &env_vars);
    defer if (reconciliation.warning) |warning| std.testing.allocator.free(warning);

    try std.testing.expectEqual(plan_mod.ProviderAction.replace_requires_confirm, reconciliation.action);
    try std.testing.expect(reconciliation.warning != null);
    try std.testing.expect(std.mem.indexOf(u8, reconciliation.warning.?, "scope changed") != null);
    try std.testing.expect(std.mem.indexOf(u8, reconciliation.warning.?, "managed env keys would be removed") != null);
}

test "evaluate reconciliation stays update when state matches" {
    const env_vars = [_]plan_mod.EnvVar{
        .{ .key = "KEEP", .value = "1" },
    };
    const managed = try std.testing.allocator.alloc([]const u8, 1);
    managed[0] = try std.testing.allocator.dupe(u8, "KEEP");
    var entry = state.Entry{
        .provider = .render,
        .name = try std.testing.allocator.dupe(u8, "demo"),
        .scope_id = try std.testing.allocator.dupe(u8, "owner-1"),
        .service_id = try std.testing.allocator.dupe(u8, "srv-1"),
        .region = try std.testing.allocator.dupe(u8, "oregon"),
        .plan_id = try std.testing.allocator.dupe(u8, "starter"),
        .url = null,
        .last_image_digest = null,
        .managed_env_keys = managed,
    };
    defer entry.deinit(std.testing.allocator);

    const reconciliation = try evaluateReconciliation(std.testing.allocator, &entry, "owner-1", "oregon", "starter", &env_vars);
    defer if (reconciliation.warning) |warning| std.testing.allocator.free(warning);

    try std.testing.expectEqual(plan_mod.ProviderAction.update, reconciliation.action);
    try std.testing.expect(reconciliation.warning == null);
}
