const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const config_mod = @import("deploy/config.zig");
const types_mod = @import("deploy/types.zig");
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
const io_util = @import("deploy/io_util.zig");
const printer_mod = @import("deploy/printer.zig");

const Printer = printer_mod.Printer;

const precompile = zigts_cli.precompile;
const deploy_manifest = zigts_cli.deploy_manifest;

const Reconciliation = struct {
    action: types_mod.ProviderAction,
    warning: ?[]u8 = null,
};

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const options = try config_mod.parse(argv);

    var stdout_buf: [256]u8 = undefined;
    var stderr_buf: [256]u8 = undefined;
    var stdout_writer = printer_mod.FdWriter.init(std.c.STDOUT_FILENO, stdout_buf[0..]);
    var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
    const printer = Printer{
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };
    var progress = printer_mod.Progress{ .printer = printer };

    const handler_path = try autodetect.detectHandler(allocator);
    defer allocator.free(handler_path);

    const service_name = try autodetect.detectName(allocator);
    defer allocator.free(service_name);

    // When a second provider lands, the lookup will need to iterate providers.
    var store = try state.load(allocator);
    defer store.deinit(allocator);
    const previous = store.get(.northflank, service_name);

    const effective_region: []const u8 = options.region orelse blk: {
        if (previous) |entry| {
            if (entry.region) |region| break :blk region;
        }
        break :blk control_plane.default_region;
    };

    var session = try fetchDeploySessionWithAuthRetry(allocator, service_name, effective_region, printer);
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

    printer.line("  Handler:    ", handler_path);
    printer.line("  Name:       ", service_name);

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
            printer.write(summary);
        }
        return error.VerificationFailed;
    }
    printer.line("  Verifying.. ok", "");

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

    const arch: types_mod.Arch = .amd64;
    var build_result = try builder.buildLinuxArtifact(allocator, handler_path, arch.targetTriple());
    defer build_result.deinit(allocator);
    printer.line("  Building... ok", "");

    const image_labels = if (proven_extract) |*extract|
        try buildProofLabels(allocator, &extract.facts)
    else
        try allocator.alloc(oci_config.Label, 0);
    defer freeLabels(allocator, image_labels);

    var image = try oci_image.buildImage(allocator, session.registry_host, image_repo, arch, build_result.binary_bytes, image_labels);
    defer image.deinit(allocator);
    printer.line("  Packaging.. ok", "");

    var registry_ref = try registry.makeRegistryRef(allocator, session.registry_host, image_repo);
    defer registry_ref.deinit(allocator);

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
        if (reconciliation.warning) |warning| printer.warn(warning);
        if (!options.confirm) return error.DeployDrift;
    }

    try pushWithAuthRetry(allocator, &registry_ref, &image, &session, service_name, effective_region, &progress);
    printer.line("  Uploading.. ok", "");
    printer.line("  Digest:     ", image.image_digest_ref);

    const result = try executeWithAuthRetry(
        allocator,
        &session,
        service_name,
        effective_region,
        image.image_digest_ref,
        env_vars,
        previous,
        &progress,
    );
    defer {
        allocator.free(result.service_id);
        if (result.deployment_id) |value| allocator.free(value);
        if (result.url) |value| allocator.free(value);
        allocator.free(result.status);
        allocator.free(result.image_digest_ref);
    }
    printer.line("  Deploying.. ok", "");

    var readiness_error: ?anyerror = null;
    if (options.wait) {
        const readiness = try waitForReadyWithAuthRetry(
            allocator,
            &session,
            service_name,
            effective_region,
            image.image_digest_ref,
            env_vars,
            previous,
            result.service_id,
            northflank_adapter.default_deadline_seconds,
            printer,
        );
        switch (readiness) {
            .running => printer.line("  Running.", ""),
            .failed => {
                printer.warn("Service failed to start. See your provider dashboard for logs.");
                readiness_error = error.ServiceDidNotStart;
            },
            .timed_out => {
                printer.warn("Timed out waiting for service to become ready after 120s. The deploy was accepted; check the URL in a moment.");
                readiness_error = error.ServiceReadyTimeout;
            },
        }
    }

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
    if (public_url) |url| printer.line("  ", url);

    if (readiness_error) |err| return err;
}

pub fn login(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const options = try config_mod.parseLogin(argv);

    var stdout_buf: [256]u8 = undefined;
    var stderr_buf: [256]u8 = undefined;
    var stdout_writer = printer_mod.FdWriter.init(std.c.STDOUT_FILENO, stdout_buf[0..]);
    var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
    const printer = Printer{
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };

    const mode: first_run.LoginMode = if (options.token_stdin)
        .token_stdin
    else if (options.device)
        .device
    else
        .prompt;

    var credentials = try first_run.login(allocator, printer, mode);
    credentials.deinit(allocator);
}

fn fetchDeploySessionWithAuthRetry(
    allocator: std.mem.Allocator,
    service_name: []const u8,
    region: []const u8,
    printer: Printer,
) !control_plane.DeploySession {
    return fetchDeploySessionWithAuthRetryFns(
        allocator,
        service_name,
        region,
        printer,
        first_run.ensureSignedIn,
        control_plane.fetchDeploySession,
        auth.clear,
    );
}

fn fetchDeploySessionWithAuthRetryFns(
    allocator: std.mem.Allocator,
    service_name: []const u8,
    region: []const u8,
    printer: Printer,
    ensure_signed_in: anytype,
    fetch_session: anytype,
    clear_auth: anytype,
) !control_plane.DeploySession {
    var credentials = try ensure_signed_in(allocator, printer);
    defer credentials.deinit(allocator);

    return fetch_session(allocator, credentials.token, service_name, region) catch |err| switch (err) {
        error.NotSignedIn => {
            _ = try clear_auth(allocator);

            var refreshed_credentials = try ensure_signed_in(allocator, printer);
            defer refreshed_credentials.deinit(allocator);

            return fetch_session(allocator, refreshed_credentials.token, service_name, region);
        },
        else => return err,
    };
}

const refresh_skew_seconds: i64 = 120;

fn pushWithAuthRetry(
    allocator: std.mem.Allocator,
    registry_ref: *registry.RegistryRef,
    image: *oci_image.OciImage,
    session: *control_plane.DeploySession,
    project_name: []const u8,
    region: []const u8,
    progress: *printer_mod.Progress,
) !void {
    return pushWithAuthRetryFns(
        allocator,
        registry_ref,
        image,
        session,
        project_name,
        region,
        progress,
        registry.push,
        control_plane.fetchDeploySession,
        control_plane.defaultNowSec,
    );
}

fn pushWithAuthRetryFns(
    allocator: std.mem.Allocator,
    registry_ref: *registry.RegistryRef,
    image: *oci_image.OciImage,
    session: *control_plane.DeploySession,
    project_name: []const u8,
    region: []const u8,
    progress: *printer_mod.Progress,
    push_fn: anytype,
    fetch_session: anytype,
    now_fn: *const fn () i64,
) !void {
    _ = try control_plane.sessionRefreshIfExpiringFns(
        allocator,
        session,
        project_name,
        region,
        refresh_skew_seconds,
        now_fn,
        fetch_session,
    );

    push_fn(allocator, registry_ref, image, session.registry_username, session.registry_password, progress) catch |err| switch (err) {
        error.RegistryUnauthorized => {
            const fresh = try fetch_session(allocator, session.token, project_name, region);
            try control_plane.swapRefreshedSession(allocator, session, fresh);
            return push_fn(allocator, registry_ref, image, session.registry_username, session.registry_password, progress);
        },
        else => return err,
    };
}

fn executeWithAuthRetry(
    allocator: std.mem.Allocator,
    session: *control_plane.DeploySession,
    project_name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const types_mod.EnvVar,
    state_entry: ?*const state.Entry,
    progress: *printer_mod.Progress,
) !types_mod.DeployResult {
    return executeWithAuthRetryFns(
        allocator,
        session,
        project_name,
        region,
        image_ref,
        env_vars,
        state_entry,
        progress,
        northflank_adapter.execute,
        control_plane.fetchDeploySession,
        control_plane.defaultNowSec,
    );
}

fn executeWithAuthRetryFns(
    allocator: std.mem.Allocator,
    session: *control_plane.DeploySession,
    project_name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const types_mod.EnvVar,
    state_entry: ?*const state.Entry,
    progress: *printer_mod.Progress,
    execute_fn: anytype,
    fetch_session: anytype,
    now_fn: *const fn () i64,
) !types_mod.DeployResult {
    _ = try control_plane.sessionRefreshIfExpiringFns(
        allocator,
        session,
        project_name,
        region,
        refresh_skew_seconds,
        now_fn,
        fetch_session,
    );

    var ctx = buildProviderContext(session, project_name, image_ref, env_vars, state_entry);
    return execute_fn(allocator, &ctx, session.provider_api_token, progress) catch |err| switch (err) {
        error.ProviderUnauthorized => {
            const fresh = try fetch_session(allocator, session.token, project_name, region);
            try control_plane.swapRefreshedSession(allocator, session, fresh);
            ctx = buildProviderContext(session, project_name, image_ref, env_vars, state_entry);
            return execute_fn(allocator, &ctx, session.provider_api_token, progress);
        },
        else => return err,
    };
}

fn waitForReadyWithAuthRetry(
    allocator: std.mem.Allocator,
    session: *control_plane.DeploySession,
    project_name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const types_mod.EnvVar,
    state_entry: ?*const state.Entry,
    service_id: []const u8,
    deadline_seconds: u32,
    printer: Printer,
) !northflank_adapter.Readiness {
    var io_backend = io_util.threadedIo(allocator);
    defer io_backend.deinit();
    const sleeper = northflank_adapter.RealSleeper{ .io = io_backend.io() };
    return waitForReadyWithAuthRetryFns(
        allocator,
        session,
        project_name,
        region,
        image_ref,
        env_vars,
        state_entry,
        service_id,
        deadline_seconds,
        printer,
        northflank_adapter.waitForReady,
        northflank_adapter.fetchStatusReal,
        sleeper,
        control_plane.fetchDeploySession,
        control_plane.defaultNowSec,
    );
}

fn waitForReadyWithAuthRetryFns(
    allocator: std.mem.Allocator,
    session: *control_plane.DeploySession,
    project_name: []const u8,
    region: []const u8,
    image_ref: []const u8,
    env_vars: []const types_mod.EnvVar,
    state_entry: ?*const state.Entry,
    service_id: []const u8,
    deadline_seconds: u32,
    printer: Printer,
    wait_fn: anytype,
    fetch_status: anytype,
    sleeper: anytype,
    fetch_session: anytype,
    now_fn: *const fn () i64,
) !northflank_adapter.Readiness {
    _ = try control_plane.sessionRefreshIfExpiringFns(
        allocator,
        session,
        project_name,
        region,
        refresh_skew_seconds,
        now_fn,
        fetch_session,
    );

    var ctx = buildProviderContext(session, project_name, image_ref, env_vars, state_entry);
    return wait_fn(
        allocator,
        &ctx,
        service_id,
        session.provider_api_token,
        deadline_seconds,
        fetch_status,
        sleeper,
        printer,
    ) catch |err| switch (err) {
        error.ProviderUnauthorized => {
            const fresh = try fetch_session(allocator, session.token, project_name, region);
            try control_plane.swapRefreshedSession(allocator, session, fresh);
            ctx = buildProviderContext(session, project_name, image_ref, env_vars, state_entry);
            return wait_fn(
                allocator,
                &ctx,
                service_id,
                session.provider_api_token,
                deadline_seconds,
                fetch_status,
                sleeper,
                printer,
            );
        },
        else => return err,
    };
}

fn buildProviderContext(
    session: *const control_plane.DeploySession,
    name: []const u8,
    image_ref: []const u8,
    env_vars: []const types_mod.EnvVar,
    state_entry: ?*const state.Entry,
) northflank_adapter.ProviderContext {
    return .{
        .name = name,
        .region = session.region,
        .image_ref = image_ref,
        .env_vars = env_vars,
        .project_id = session.scope_id,
        .plan_id = session.plan_id,
        .registry_credential_id = session.registry_credential_id,
        .state_entry = state_entry,
    };
}

fn managedEnvKeys(allocator: std.mem.Allocator, env_vars: []const types_mod.EnvVar) ![]const []const u8 {
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
    // Label carries env var names only; values live in the provider runtime, never the image.
    if (facts.env_vars.len > 0) try appendJsonArrayLabel(allocator, &labels, "zigttp.env-vars", facts.env_vars);
    if (facts.egress_hosts.len > 0) try appendJsonArrayLabel(allocator, &labels, "zigttp.egress-hosts", facts.egress_hosts);
    if (facts.cache_namespaces.len > 0) try appendJsonArrayLabel(allocator, &labels, "zigttp.cache-namespaces", facts.cache_namespaces);
    if (facts.routes.len > 0) {
        var route_strings = std.ArrayList([]const u8).empty;
        defer {
            for (route_strings.items) |value| allocator.free(value);
            route_strings.deinit(allocator);
        }
        for (facts.routes) |route| {
            try route_strings.append(allocator, try std.fmt.allocPrint(allocator, "{s}{s}", .{
                route.pattern,
                if (route.is_prefix) "*" else "",
            }));
        }
        try appendJsonArrayLabel(allocator, &labels, "zigttp.routes", route_strings.items);
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

fn appendJsonArrayLabel(
    allocator: std.mem.Allocator,
    labels: *std.ArrayList(oci_config.Label),
    key: []const u8,
    values: []const []const u8,
) !void {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginArray();
    for (values) |value| try json.write(value);
    try json.endArray();
    const encoded = try allocator.dupe(u8, aw.writer.buffered());
    try appendLabelOwned(allocator, labels, key, encoded);
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
    env_vars: []const types_mod.EnvVar,
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
        .warning = try std.fmt.allocPrint(allocator, "This service was created with different settings ({s}). Re-run `zigttp deploy --confirm` to continue.", .{joined}),
    };
}

fn removedManagedEnvKeysWarning(
    allocator: std.mem.Allocator,
    previous_keys: []const []const u8,
    env_vars: []const types_mod.EnvVar,
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

fn envVarsContainKey(env_vars: []const types_mod.EnvVar, key: []const u8) bool {
    for (env_vars) |env_var| {
        if (std.mem.eql(u8, env_var.key, key)) return true;
    }
    return false;
}

test "evaluate reconciliation flags state drift" {
    const env_vars = [_]types_mod.EnvVar{
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

    try std.testing.expectEqual(types_mod.ProviderAction.replace_requires_confirm, reconciliation.action);
    try std.testing.expect(reconciliation.warning != null);
    try std.testing.expect(std.mem.indexOf(u8, reconciliation.warning.?, "scope changed") != null);
    try std.testing.expect(std.mem.indexOf(u8, reconciliation.warning.?, "--confirm") != null);
}

test "evaluate reconciliation stays update when state matches" {
    const env_vars = [_]types_mod.EnvVar{
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

    try std.testing.expectEqual(types_mod.ProviderAction.update, reconciliation.action);
    try std.testing.expect(reconciliation.warning == null);
}

// Test stubs in this file rely on module-static counters reset at each test
// entry; Zig runs tests serially so the pattern is race-free.

test "fetchDeploySessionWithAuthRetry retries after rejected saved token" {
    const TestCtx = struct {
        var ensure_calls: usize = 0;
        var fetch_calls: usize = 0;
        var clear_calls: usize = 0;
        var received_region: []const u8 = "";

        fn ensureSignedIn(allocator: std.mem.Allocator, _: Printer) !auth.Credentials {
            ensure_calls += 1;
            return .{
                .token = try allocator.dupe(u8, if (ensure_calls == 1) "expired-token" else "fresh-token"),
                .email = null,
            };
        }

        fn fetchSession(
            allocator: std.mem.Allocator,
            token: []const u8,
            service_name: []const u8,
            region: []const u8,
        ) !control_plane.DeploySession {
            fetch_calls += 1;
            received_region = region;
            try std.testing.expectEqualStrings("demo", service_name);
            if (std.mem.eql(u8, token, "expired-token")) return error.NotSignedIn;
            try std.testing.expectEqualStrings("fresh-token", token);
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
                .region = try allocator.dupe(u8, control_plane.default_region),
                .url_hint = null,
                .token = try allocator.dupe(u8, token),
                .expires_at = null,
            };
        }

        fn clearAuth(_: std.mem.Allocator) !bool {
            clear_calls += 1;
            return true;
        }
    };

    TestCtx.ensure_calls = 0;
    TestCtx.fetch_calls = 0;
    TestCtx.clear_calls = 0;
    TestCtx.received_region = "";

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    var session = try fetchDeploySessionWithAuthRetryFns(
        std.testing.allocator,
        "demo",
        "eu-west",
        buffered.printer(),
        TestCtx.ensureSignedIn,
        TestCtx.fetchSession,
        TestCtx.clearAuth,
    );
    defer session.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 2), TestCtx.ensure_calls);
    try std.testing.expectEqual(@as(usize, 2), TestCtx.fetch_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.clear_calls);
    try std.testing.expectEqualStrings("eu-west", TestCtx.received_region);
    try std.testing.expectEqualStrings(control_plane.default_region, session.region);
    try std.testing.expectEqualStrings("fresh-token", session.token);
}

test "fetchDeploySessionWithAuthRetry does not clear auth for other fetch errors" {
    const TestCtx = struct {
        var ensure_calls: usize = 0;
        var fetch_calls: usize = 0;
        var clear_calls: usize = 0;

        fn ensureSignedIn(allocator: std.mem.Allocator, _: Printer) !auth.Credentials {
            ensure_calls += 1;
            return .{
                .token = try allocator.dupe(u8, "token"),
                .email = null,
            };
        }

        fn fetchSession(_: std.mem.Allocator, _: []const u8, _: []const u8, _: []const u8) !control_plane.DeploySession {
            fetch_calls += 1;
            return error.ControlPlaneError;
        }

        fn clearAuth(_: std.mem.Allocator) !bool {
            clear_calls += 1;
            return true;
        }
    };

    TestCtx.ensure_calls = 0;
    TestCtx.fetch_calls = 0;
    TestCtx.clear_calls = 0;

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    try std.testing.expectError(
        error.ControlPlaneError,
        fetchDeploySessionWithAuthRetryFns(
            std.testing.allocator,
            "demo",
            control_plane.default_region,
            buffered.printer(),
            TestCtx.ensureSignedIn,
            TestCtx.fetchSession,
            TestCtx.clearAuth,
        ),
    );

    try std.testing.expectEqual(@as(usize, 1), TestCtx.ensure_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.fetch_calls);
    try std.testing.expectEqual(@as(usize, 0), TestCtx.clear_calls);
}

fn freshSessionForTest(allocator: std.mem.Allocator, token: []const u8) !control_plane.DeploySession {
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
        .region = try allocator.dupe(u8, control_plane.default_region),
        .url_hint = null,
        .token = try allocator.dupe(u8, token),
        .expires_at = null,
    };
}

test "pushWithAuthRetry refreshes on RegistryUnauthorized" {
    const TestCtx = struct {
        var push_calls: usize = 0;
        var fetch_calls: usize = 0;

        fn pushStub(
            _: std.mem.Allocator,
            _: *const registry.RegistryRef,
            _: *const oci_image.OciImage,
            _: []const u8,
            _: []const u8,
            _: ?*printer_mod.Progress,
        ) !void {
            push_calls += 1;
            if (push_calls == 1) return error.RegistryUnauthorized;
        }

        fn fetchSession(
            allocator: std.mem.Allocator,
            _: []const u8,
            _: []const u8,
            _: []const u8,
        ) !control_plane.DeploySession {
            fetch_calls += 1;
            return freshSessionForTest(allocator, "rotated-token");
        }

        fn nowStub() i64 {
            return 1000;
        }
    };
    TestCtx.push_calls = 0;
    TestCtx.fetch_calls = 0;

    var session = try freshSessionForTest(std.testing.allocator, "initial-token");
    defer session.deinit(std.testing.allocator);

    var registry_ref = try registry.makeRegistryRef(std.testing.allocator, "registry.zigttp.dev", "u-42/demo");
    defer registry_ref.deinit(std.testing.allocator);

    var image: oci_image.OciImage = undefined;
    // pushStub never touches image fields; zero-init is fine.
    @memset(std.mem.asBytes(&image), 0);

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();
    var progress = printer_mod.Progress{ .printer = buffered.printer() };

    try pushWithAuthRetryFns(
        std.testing.allocator,
        &registry_ref,
        &image,
        &session,
        "demo",
        "us-east",
        &progress,
        TestCtx.pushStub,
        TestCtx.fetchSession,
        TestCtx.nowStub,
    );

    try std.testing.expectEqual(@as(usize, 2), TestCtx.push_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.fetch_calls);
    try std.testing.expectEqualStrings("rotated-token", session.token);
}

test "executeWithAuthRetry refreshes on ProviderUnauthorized" {
    const TestCtx = struct {
        var execute_calls: usize = 0;
        var fetch_calls: usize = 0;
        var saw_refreshed_ctx = false;

        fn executeStub(
            allocator: std.mem.Allocator,
            ctx: *const northflank_adapter.ProviderContext,
            api_token: []const u8,
            _: ?*printer_mod.Progress,
        ) !types_mod.DeployResult {
            execute_calls += 1;
            if (execute_calls == 1) return error.ProviderUnauthorized;
            try std.testing.expectEqualStrings("proj-2", ctx.project_id);
            try std.testing.expectEqualStrings("plan-rotated", ctx.plan_id);
            try std.testing.expectEqualStrings("eu-west", ctx.region);
            try std.testing.expectEqualStrings("nf-token-rotated", api_token);
            saw_refreshed_ctx = true;
            return .{
                .provider = .northflank,
                .name = ctx.name,
                .service_id = try allocator.dupe(u8, "srv-1"),
                .deployment_id = null,
                .url = null,
                .status = try allocator.dupe(u8, "created"),
                .image_digest_ref = try allocator.dupe(u8, ctx.image_ref),
            };
        }

        fn fetchSession(
            allocator: std.mem.Allocator,
            _: []const u8,
            _: []const u8,
            _: []const u8,
        ) !control_plane.DeploySession {
            fetch_calls += 1;
            var fresh = try freshSessionForTest(allocator, "rotated-token");
            allocator.free(fresh.scope_id);
            fresh.scope_id = try allocator.dupe(u8, "proj-2");
            allocator.free(fresh.plan_id);
            fresh.plan_id = try allocator.dupe(u8, "plan-rotated");
            allocator.free(fresh.region);
            fresh.region = try allocator.dupe(u8, "eu-west");
            allocator.free(fresh.provider_api_token);
            fresh.provider_api_token = try allocator.dupe(u8, "nf-token-rotated");
            return fresh;
        }

        fn nowStub() i64 {
            return 1000;
        }
    };
    TestCtx.execute_calls = 0;
    TestCtx.fetch_calls = 0;
    TestCtx.saw_refreshed_ctx = false;

    var session = try freshSessionForTest(std.testing.allocator, "initial-token");
    defer session.deinit(std.testing.allocator);

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();
    var progress = printer_mod.Progress{ .printer = buffered.printer() };

    const result = try executeWithAuthRetryFns(
        std.testing.allocator,
        &session,
        "demo",
        "us-east",
        "img",
        &.{},
        null,
        &progress,
        TestCtx.executeStub,
        TestCtx.fetchSession,
        TestCtx.nowStub,
    );
    defer {
        std.testing.allocator.free(result.service_id);
        std.testing.allocator.free(result.status);
        std.testing.allocator.free(result.image_digest_ref);
    }

    try std.testing.expectEqual(@as(usize, 2), TestCtx.execute_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.fetch_calls);
    try std.testing.expectEqualStrings("rotated-token", session.token);
    try std.testing.expect(TestCtx.saw_refreshed_ctx);
}

test "waitForReadyWithAuthRetry refreshes on ProviderUnauthorized" {
    const TestCtx = struct {
        var wait_calls: usize = 0;
        var fetch_calls: usize = 0;
        var saw_refreshed_ctx = false;

        const NoopSleeper = struct {
            pub fn sleep(_: NoopSleeper, _: u32) void {}
        };

        fn waitStub(
            allocator: std.mem.Allocator,
            ctx: *const northflank_adapter.ProviderContext,
            service_id: []const u8,
            api_token: []const u8,
            deadline_seconds: u32,
            fetch_status: anytype,
            sleeper: NoopSleeper,
            printer: Printer,
        ) !northflank_adapter.Readiness {
            _ = service_id;
            _ = deadline_seconds;
            _ = sleeper;
            _ = printer;
            wait_calls += 1;
            const status = try fetch_status(allocator, ctx.project_id, api_token);
            return switch (status) {
                .running => blk: {
                    try std.testing.expectEqualStrings("proj-2", ctx.project_id);
                    try std.testing.expectEqualStrings("plan-rotated", ctx.plan_id);
                    try std.testing.expectEqualStrings("eu-west", ctx.region);
                    try std.testing.expectEqualStrings("nf-token-rotated", api_token);
                    saw_refreshed_ctx = true;
                    break :blk .running;
                },
                .pending => .timed_out,
                .failed => .failed,
            };
        }

        fn fetchStatus(_: std.mem.Allocator, project_id: []const u8, api_token: []const u8) !northflank_adapter.ServiceStatus {
            if (std.mem.eql(u8, project_id, "proj-1")) {
                try std.testing.expectEqualStrings("nf-token", api_token);
                return error.ProviderUnauthorized;
            }
            try std.testing.expectEqualStrings("proj-2", project_id);
            try std.testing.expectEqualStrings("nf-token-rotated", api_token);
            return .running;
        }

        fn fetchSession(
            allocator: std.mem.Allocator,
            _: []const u8,
            _: []const u8,
            _: []const u8,
        ) !control_plane.DeploySession {
            fetch_calls += 1;
            var fresh = try freshSessionForTest(allocator, "rotated-token");
            allocator.free(fresh.scope_id);
            fresh.scope_id = try allocator.dupe(u8, "proj-2");
            allocator.free(fresh.plan_id);
            fresh.plan_id = try allocator.dupe(u8, "plan-rotated");
            allocator.free(fresh.region);
            fresh.region = try allocator.dupe(u8, "eu-west");
            allocator.free(fresh.provider_api_token);
            fresh.provider_api_token = try allocator.dupe(u8, "nf-token-rotated");
            return fresh;
        }

        fn nowStub() i64 {
            return 1000;
        }
    };
    TestCtx.wait_calls = 0;
    TestCtx.fetch_calls = 0;
    TestCtx.saw_refreshed_ctx = false;

    var session = try freshSessionForTest(std.testing.allocator, "initial-token");
    defer session.deinit(std.testing.allocator);

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    const readiness = try waitForReadyWithAuthRetryFns(
        std.testing.allocator,
        &session,
        "demo",
        "us-east",
        "img",
        &.{},
        null,
        "srv-1",
        northflank_adapter.default_deadline_seconds,
        buffered.printer(),
        TestCtx.waitStub,
        TestCtx.fetchStatus,
        TestCtx.NoopSleeper{},
        TestCtx.fetchSession,
        TestCtx.nowStub,
    );

    try std.testing.expectEqual(northflank_adapter.Readiness.running, readiness);
    try std.testing.expectEqual(@as(usize, 2), TestCtx.wait_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.fetch_calls);
    try std.testing.expectEqualStrings("rotated-token", session.token);
    try std.testing.expect(TestCtx.saw_refreshed_ctx);
}

pub fn logout(allocator: std.mem.Allocator) !void {
    const had_creds = try auth.clear(allocator);
    const msg = if (had_creds) "Signed out.\n" else "Already signed out.\n";
    _ = std.c.write(std.c.STDOUT_FILENO, msg.ptr, msg.len);
}
