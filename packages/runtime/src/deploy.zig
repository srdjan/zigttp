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

    const contract = compiled.contract orelse return error.ContractUnavailable;
    const contract_json = try canonicalContractJson(allocator, &contract);
    defer allocator.free(contract_json);
    const contract_sha256 = try sha256Hex(allocator, contract_json);
    defer allocator.free(contract_sha256);

    const session_outcome = try fetchDeploySessionWithAuthRetry(
        allocator,
        service_name,
        effective_region,
        contract_json,
        contract_sha256,
        printer,
    );
    var session = switch (session_outcome) {
        .approved => |session| session,
        .plan_required => |plan_value| {
            var plan = plan_value;
            defer plan.deinit(allocator);
            try printDeployPlanRequired(allocator, printer, &plan);
            return error.ControlPlaneReviewRequired;
        },
    };
    defer session.deinit(allocator);

    if (session.grant_ids.len > 0) {
        const joined = try std.mem.join(allocator, ", ", session.grant_ids);
        defer allocator.free(joined);
        printer.line("  Grants:     ", joined);
    }

    const image_repo = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ session.namespace, service_name });
    defer allocator.free(image_repo);

    var proven_extract: ?struct {
        facts: deploy_manifest.ProvenFacts,
        checks_buf: [][]const u8,
        routes_buf: []deploy_manifest.ProvenRoute,
    } = null;
    defer if (proven_extract) |extract| {
        allocator.free(extract.checks_buf);
        allocator.free(extract.routes_buf);
    };

    if (compiled.contract) |*compiled_contract| {
        const extract = try deploy_manifest.extractProvenFacts(allocator, compiled_contract);
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

pub fn review(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const options = try config_mod.parseReview(argv);

    var stdout_buf: [256]u8 = undefined;
    var stderr_buf: [256]u8 = undefined;
    var stdout_writer = printer_mod.FdWriter.init(std.c.STDOUT_FILENO, stdout_buf[0..]);
    var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
    const printer = Printer{
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };

    if (options.action) |action| {
        var result = try reviewPlanWithAuthRetry(
            allocator,
            options.plan_id.?,
            switch (action) {
                .approve => .approve,
                .reject => .reject,
            },
            if (options.grant) .grant else .once,
            printer,
        );
        defer result.deinit(allocator);

        const action_text = switch (action) {
            .approve => if (options.grant) "approved and granted" else "approved",
            .reject => "rejected",
        };
        const line = try std.fmt.allocPrint(
            allocator,
            "plan {s} {s}",
            .{ options.plan_id.?, action_text },
        );
        defer allocator.free(line);
        printer.line("  Review:     ", line);
        if (result.grants_created > 0) {
            const grant_line = try std.fmt.allocPrint(
                allocator,
                "{d}",
                .{result.grants_created},
            );
            defer allocator.free(grant_line);
            printer.line("  Grants:     ", grant_line);
        }
        return;
    }

    var plan = try fetchDeployPlanWithAuthRetry(allocator, options.plan_id.?, printer);
    defer plan.deinit(allocator);
    try printReviewStatus(allocator, printer, &plan);
}

pub fn grants(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const options = try config_mod.parseGrants(argv);

    var stdout_buf: [256]u8 = undefined;
    var stderr_buf: [256]u8 = undefined;
    var stdout_writer = printer_mod.FdWriter.init(std.c.STDOUT_FILENO, stdout_buf[0..]);
    var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
    const printer = Printer{
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };

    var grants_list = try fetchCapabilityGrantsWithAuthRetry(allocator, options.project_name, printer);
    defer grants_list.deinit(allocator);
    try printCapabilityGrants(allocator, printer, &grants_list);
}

pub fn revokeGrant(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const options = try config_mod.parseRevokeGrant(argv);

    var stdout_buf: [256]u8 = undefined;
    var stderr_buf: [256]u8 = undefined;
    var stdout_writer = printer_mod.FdWriter.init(std.c.STDOUT_FILENO, stdout_buf[0..]);
    var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
    const printer = Printer{
        .stdout = &stdout_writer.interface,
        .stderr = &stderr_writer.interface,
    };

    var grant = try revokeCapabilityGrantWithAuthRetry(allocator, options.grant_id.?, printer);
    defer grant.deinit(allocator);
    printer.line("  Grant:      ", grant.id);
    printer.line("  Project:    ", grant.project_name);
    printer.line("  Status:     ", grant.status);
}

fn fetchDeploySessionWithAuthRetry(
    allocator: std.mem.Allocator,
    service_name: []const u8,
    region: []const u8,
    contract_json: []const u8,
    contract_sha256: []const u8,
    printer: Printer,
) !control_plane.FetchDeploySessionResult {
    return fetchDeploySessionWithAuthRetryFns(
        allocator,
        service_name,
        region,
        contract_json,
        contract_sha256,
        printer,
        first_run.ensureSignedIn,
        control_plane.fetchDeploySession,
        auth.clear,
    );
}

fn reviewPlanWithAuthRetry(
    allocator: std.mem.Allocator,
    plan_id: []const u8,
    action: control_plane.ReviewAction,
    mode: control_plane.ReviewMode,
    printer: Printer,
) !control_plane.ReviewResult {
    return reviewPlanWithAuthRetryFns(
        allocator,
        plan_id,
        action,
        mode,
        printer,
        first_run.ensureSignedIn,
        control_plane.reviewPlan,
        auth.clear,
    );
}

fn fetchDeployPlanWithAuthRetry(
    allocator: std.mem.Allocator,
    plan_id: []const u8,
    printer: Printer,
) !control_plane.DeployPlanStatus {
    return fetchDeployPlanWithAuthRetryFns(
        allocator,
        plan_id,
        printer,
        first_run.ensureSignedIn,
        control_plane.fetchDeployPlan,
        auth.clear,
    );
}

fn fetchCapabilityGrantsWithAuthRetry(
    allocator: std.mem.Allocator,
    project_name: ?[]const u8,
    printer: Printer,
) !control_plane.CapabilityGrantList {
    return fetchCapabilityGrantsWithAuthRetryFns(
        allocator,
        project_name,
        printer,
        first_run.ensureSignedIn,
        control_plane.fetchCapabilityGrants,
        auth.clear,
    );
}

fn revokeCapabilityGrantWithAuthRetry(
    allocator: std.mem.Allocator,
    grant_id: []const u8,
    printer: Printer,
) !control_plane.CapabilityGrant {
    return revokeCapabilityGrantWithAuthRetryFns(
        allocator,
        grant_id,
        printer,
        first_run.ensureSignedIn,
        control_plane.revokeCapabilityGrant,
        auth.clear,
    );
}

fn reviewPlanWithAuthRetryFns(
    allocator: std.mem.Allocator,
    plan_id: []const u8,
    action: control_plane.ReviewAction,
    mode: control_plane.ReviewMode,
    printer: Printer,
    ensure_signed_in: anytype,
    review_plan: anytype,
    clear_auth: anytype,
) !control_plane.ReviewResult {
    var credentials = try ensure_signed_in(allocator, printer);
    defer credentials.deinit(allocator);

    return review_plan(allocator, credentials.token, plan_id, action, mode) catch |err| switch (err) {
        error.NotSignedIn => {
            _ = try clear_auth(allocator);

            var refreshed_credentials = try ensure_signed_in(allocator, printer);
            defer refreshed_credentials.deinit(allocator);

            return review_plan(allocator, refreshed_credentials.token, plan_id, action, mode);
        },
        else => return err,
    };
}

fn fetchDeployPlanWithAuthRetryFns(
    allocator: std.mem.Allocator,
    plan_id: []const u8,
    printer: Printer,
    ensure_signed_in: anytype,
    fetch_plan: anytype,
    clear_auth: anytype,
) !control_plane.DeployPlanStatus {
    var credentials = try ensure_signed_in(allocator, printer);
    defer credentials.deinit(allocator);

    return fetch_plan(allocator, credentials.token, plan_id) catch |err| switch (err) {
        error.NotSignedIn => {
            _ = try clear_auth(allocator);

            var refreshed_credentials = try ensure_signed_in(allocator, printer);
            defer refreshed_credentials.deinit(allocator);

            return fetch_plan(allocator, refreshed_credentials.token, plan_id);
        },
        else => return err,
    };
}

fn fetchCapabilityGrantsWithAuthRetryFns(
    allocator: std.mem.Allocator,
    project_name: ?[]const u8,
    printer: Printer,
    ensure_signed_in: anytype,
    fetch_grants: anytype,
    clear_auth: anytype,
) !control_plane.CapabilityGrantList {
    var credentials = try ensure_signed_in(allocator, printer);
    defer credentials.deinit(allocator);

    return fetch_grants(allocator, credentials.token, project_name) catch |err| switch (err) {
        error.NotSignedIn => {
            _ = try clear_auth(allocator);

            var refreshed_credentials = try ensure_signed_in(allocator, printer);
            defer refreshed_credentials.deinit(allocator);

            return fetch_grants(allocator, refreshed_credentials.token, project_name);
        },
        else => return err,
    };
}

fn revokeCapabilityGrantWithAuthRetryFns(
    allocator: std.mem.Allocator,
    grant_id: []const u8,
    printer: Printer,
    ensure_signed_in: anytype,
    revoke_grant: anytype,
    clear_auth: anytype,
) !control_plane.CapabilityGrant {
    var credentials = try ensure_signed_in(allocator, printer);
    defer credentials.deinit(allocator);

    return revoke_grant(allocator, credentials.token, grant_id) catch |err| switch (err) {
        error.NotSignedIn => {
            _ = try clear_auth(allocator);

            var refreshed_credentials = try ensure_signed_in(allocator, printer);
            defer refreshed_credentials.deinit(allocator);

            return revoke_grant(allocator, refreshed_credentials.token, grant_id);
        },
        else => return err,
    };
}

fn fetchDeploySessionWithAuthRetryFns(
    allocator: std.mem.Allocator,
    service_name: []const u8,
    region: []const u8,
    contract_json: []const u8,
    contract_sha256: []const u8,
    printer: Printer,
    ensure_signed_in: anytype,
    fetch_session: anytype,
    clear_auth: anytype,
) !control_plane.FetchDeploySessionResult {
    var credentials = try ensure_signed_in(allocator, printer);
    defer credentials.deinit(allocator);

    return fetch_session(allocator, credentials.token, .{
        .project_name = service_name,
        .region = region,
        .contract_json = contract_json,
        .contract_sha256 = contract_sha256,
    }) catch |err| switch (err) {
        error.NotSignedIn => {
            _ = try clear_auth(allocator);

            var refreshed_credentials = try ensure_signed_in(allocator, printer);
            defer refreshed_credentials.deinit(allocator);

            return fetch_session(allocator, refreshed_credentials.token, .{
                .project_name = service_name,
                .region = region,
                .contract_json = contract_json,
                .contract_sha256 = contract_sha256,
            });
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
        control_plane.fetchApprovedDeploySession,
        control_plane.defaultNowSec,
    );
}

fn pushWithAuthRetryFns(
    allocator: std.mem.Allocator,
    registry_ref: *registry.RegistryRef,
    image: *oci_image.OciImage,
    session: *control_plane.DeploySession,
    _: []const u8,
    _: []const u8,
    progress: *printer_mod.Progress,
    push_fn: anytype,
    fetch_session: anytype,
    now_fn: *const fn () i64,
) !void {
    _ = try control_plane.sessionRefreshIfExpiringFns(
        allocator,
        session,
        refresh_skew_seconds,
        now_fn,
        fetch_session,
    );

    push_fn(allocator, registry_ref, image, session.registry_username, session.registry_password, progress) catch |err| switch (err) {
        error.RegistryUnauthorized => {
            const fresh = try fetch_session(allocator, session.token, session.request_body);
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
        control_plane.fetchApprovedDeploySession,
        control_plane.defaultNowSec,
    );
}

fn executeWithAuthRetryFns(
    allocator: std.mem.Allocator,
    session: *control_plane.DeploySession,
    project_name: []const u8,
    _: []const u8,
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
        refresh_skew_seconds,
        now_fn,
        fetch_session,
    );

    var ctx = buildProviderContext(session, project_name, image_ref, env_vars, state_entry);
    return execute_fn(allocator, &ctx, session.provider_api_token, progress) catch |err| switch (err) {
        error.ProviderUnauthorized => {
            const fresh = try fetch_session(allocator, session.token, session.request_body);
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
        control_plane.fetchApprovedDeploySession,
        control_plane.defaultNowSec,
    );
}

fn waitForReadyWithAuthRetryFns(
    allocator: std.mem.Allocator,
    session: *control_plane.DeploySession,
    project_name: []const u8,
    _: []const u8,
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
            const fresh = try fetch_session(allocator, session.token, session.request_body);
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

fn canonicalContractJson(
    allocator: std.mem.Allocator,
    contract: *const zigts.HandlerContract,
) ![]u8 {
    var raw_writer: std.Io.Writer.Allocating = .init(allocator);
    defer raw_writer.deinit();
    try zigts.writeContractJson(contract, &raw_writer.writer);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, raw_writer.writer.buffered(), .{}) catch {
        return error.InvalidContract;
    };
    defer parsed.deinit();

    var out: std.Io.Writer.Allocating = .init(allocator);
    defer out.deinit();
    try writeCanonicalJsonValue(allocator, parsed.value, &out.writer);
    return out.toOwnedSlice();
}

fn writeCanonicalJsonValue(
    allocator: std.mem.Allocator,
    value: std.json.Value,
    writer: anytype,
) !void {
    switch (value) {
        .object => |obj| {
            const Entry = struct {
                key: []const u8,
                value: *const std.json.Value,
            };
            var entries = try allocator.alloc(Entry, obj.count());
            defer allocator.free(entries);

            var it = obj.iterator();
            var idx: usize = 0;
            while (it.next()) |entry| : (idx += 1) {
                entries[idx] = .{
                    .key = entry.key_ptr.*,
                    .value = entry.value_ptr,
                };
            }
            std.mem.sort(Entry, entries, {}, struct {
                fn lessThan(_: void, a: Entry, b: Entry) bool {
                    return std.mem.lessThan(u8, a.key, b.key);
                }
            }.lessThan);

            try writer.writeByte('{');
            for (entries, 0..) |entry, i| {
                if (i > 0) try writer.writeByte(',');
                try zigts.handler_contract.writeJsonString(writer, entry.key);
                try writer.writeByte(':');
                try writeCanonicalJsonValue(allocator, entry.value.*, writer);
            }
            try writer.writeByte('}');
        },
        .array => |array| {
            try writer.writeByte('[');
            for (array.items, 0..) |item, i| {
                if (i > 0) try writer.writeByte(',');
                try writeCanonicalJsonValue(allocator, item, writer);
            }
            try writer.writeByte(']');
        },
        .null => try writer.writeAll("null"),
        .bool => |b| try writer.writeAll(if (b) "true" else "false"),
        .integer => |i| try writer.print("{d}", .{i}),
        .float => |f| try writer.print("{d}", .{f}),
        .string => |s| try zigts.handler_contract.writeJsonString(writer, s),
        else => return error.UnsupportedJsonValue,
    }
}

fn sha256Hex(allocator: std.mem.Allocator, value: []const u8) ![]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(value);
    const digest = hasher.finalResult();
    return try allocator.dupe(u8, &std.fmt.bytesToHex(digest, .lower));
}

fn printDeployPlanRequired(
    allocator: std.mem.Allocator,
    printer: Printer,
    plan: *const control_plane.DeployPlanRequired,
) !void {
    printer.warn("Capability review required before this deploy can continue.");
    printer.line("  Plan ID:    ", plan.plan_id);
    if (plan.review_url) |review_url| {
        printer.line("  Review:     ", review_url);
    }

    for (plan.covered_reasons) |reason| {
        const line = try std.fmt.allocPrint(allocator, "  already granted: {s}", .{reason});
        defer allocator.free(line);
        printer.warn(line);
    }
    const reasons = if (plan.uncovered_reasons.len > 0) plan.uncovered_reasons else plan.diff_reasons;
    for (reasons) |reason| {
        const line = try std.fmt.allocPrint(allocator, "  needs review:    {s}", .{reason});
        defer allocator.free(line);
        printer.warn(line);
    }
    printer.warn("Approve the plan, then re-run `zigttp deploy`.");
}

fn printReviewStatus(
    _: std.mem.Allocator,
    printer: Printer,
    plan: *const control_plane.DeployPlanStatus,
) !void {
    printer.line("  Plan ID:    ", plan.id);
    printer.line("  Project:    ", plan.project_name);
    printer.line("  Status:     ", plan.status);
    if (plan.baseline_sha) |baseline_sha| {
        printer.line("  Baseline:   ", baseline_sha);
    }
    printer.line("  Proposed:   ", plan.proposed_sha);
    printer.line("  Created:    ", plan.created_at);
    printer.line("  Expires:    ", plan.expires_at);
    if (plan.decided_at) |decided_at| {
        printer.line("  Decided:    ", decided_at);
    }
    if (plan.decided_by_token_id) |token_id| {
        printer.line("  By Token:   ", token_id);
    }
    if (plan.consumed_at) |consumed_at| {
        printer.line("  Consumed:   ", consumed_at);
    }

    for (plan.diff_reasons) |reason| {
        printer.line("  Risk:       ", reason);
    }
}

fn printCapabilityGrants(
    allocator: std.mem.Allocator,
    printer: Printer,
    grants_list: *const control_plane.CapabilityGrantList,
) !void {
    if (grants_list.items.len == 0) {
        printer.line("  Grants:     ", "none");
        return;
    }

    const count = try std.fmt.allocPrint(allocator, "{d}", .{grants_list.items.len});
    defer allocator.free(count);
    printer.line("  Grants:     ", count);
    for (grants_list.items) |grant| {
        printer.line("  ID:         ", grant.id);
        printer.line("  Project:    ", grant.project_name);
        printer.line("  Status:     ", grant.status);
        if (grant.expires_at) |expires_at| {
            printer.line("  Expires:    ", expires_at);
        }
        if (grant.revoked_at) |revoked_at| {
            printer.line("  Revoked:    ", revoked_at);
        }
        const scope = if (grant.reasons.len > 0)
            try std.mem.join(allocator, ", ", grant.reasons)
        else
            try allocator.dupe(u8, "grant scope");
        defer allocator.free(scope);
        printer.line("  Scope:      ", scope);
        printer.line("", "");
    }
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
            request: control_plane.SessionRequest,
        ) !control_plane.FetchDeploySessionResult {
            fetch_calls += 1;
            received_region = request.region;
            try std.testing.expectEqualStrings("demo", request.project_name);
            try std.testing.expectEqualStrings("{\"routes\":[]}", request.contract_json);
            try std.testing.expectEqualStrings("sha", request.contract_sha256);
            if (std.mem.eql(u8, token, "expired-token")) return error.NotSignedIn;
            try std.testing.expectEqualStrings("fresh-token", token);
            return .{ .approved = .{
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
                .request_body = try allocator.dupe(u8, "{\"session\":true}"),
                .grant_ids = try allocator.alloc([]u8, 0),
            } };
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

    const outcome = try fetchDeploySessionWithAuthRetryFns(
        std.testing.allocator,
        "demo",
        "eu-west",
        "{\"routes\":[]}",
        "sha",
        buffered.printer(),
        TestCtx.ensureSignedIn,
        TestCtx.fetchSession,
        TestCtx.clearAuth,
    );
    var session = switch (outcome) {
        .approved => |session| session,
        else => return error.TestUnexpectedResult,
    };
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

        fn fetchSession(_: std.mem.Allocator, _: []const u8, _: control_plane.SessionRequest) !control_plane.FetchDeploySessionResult {
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
            "{\"routes\":[]}",
            "sha",
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

test "fetchDeploySessionWithAuthRetry returns plan_required without clearing auth" {
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

        fn fetchSession(
            allocator: std.mem.Allocator,
            _: []const u8,
            _: control_plane.SessionRequest,
        ) !control_plane.FetchDeploySessionResult {
            fetch_calls += 1;
            return .{ .plan_required = .{
                .plan_id = try allocator.dupe(u8, "plan-1"),
                .review_url = try allocator.dupe(u8, "https://control/deploy/plans/plan-1"),
                .baseline_sha = null,
                .proposed_sha = null,
                .expires_at = null,
                .diff_reasons = try allocator.dupe([]u8, &.{try allocator.dupe(u8, "new env read: SECRET_KEY")}),
                .covered_reasons = try allocator.alloc([]u8, 0),
                .uncovered_reasons = try allocator.dupe([]u8, &.{try allocator.dupe(u8, "new env read: SECRET_KEY")}),
            } };
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

    const result = try fetchDeploySessionWithAuthRetryFns(
        std.testing.allocator,
        "demo",
        control_plane.default_region,
        "{\"routes\":[]}",
        "sha",
        buffered.printer(),
        TestCtx.ensureSignedIn,
        TestCtx.fetchSession,
        TestCtx.clearAuth,
    );
    switch (result) {
        .plan_required => |*plan| {
            defer plan.deinit(std.testing.allocator);
            try std.testing.expectEqualStrings("plan-1", plan.plan_id);
        },
        else => return error.TestExpectedPlanRequired,
    }

    try std.testing.expectEqual(@as(usize, 1), TestCtx.ensure_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.fetch_calls);
    try std.testing.expectEqual(@as(usize, 0), TestCtx.clear_calls);
}

test "reviewPlanWithAuthRetry retries after rejected saved token" {
    const TestCtx = struct {
        var ensure_calls: usize = 0;
        var review_calls: usize = 0;
        var clear_calls: usize = 0;

        fn ensureSignedIn(allocator: std.mem.Allocator, _: Printer) !auth.Credentials {
            ensure_calls += 1;
            return .{
                .token = try allocator.dupe(u8, if (ensure_calls == 1) "expired-token" else "fresh-token"),
                .email = null,
            };
        }

        fn reviewPlan(
            allocator: std.mem.Allocator,
            token: []const u8,
            plan_id: []const u8,
            action: control_plane.ReviewAction,
            mode: control_plane.ReviewMode,
        ) !control_plane.ReviewResult {
            review_calls += 1;
            try std.testing.expectEqualStrings("plan-1", plan_id);
            try std.testing.expect(action == .approve);
            try std.testing.expect(mode == .grant);
            if (std.mem.eql(u8, token, "expired-token")) return error.NotSignedIn;
            try std.testing.expectEqualStrings("fresh-token", token);
            return .{
                .status = try allocator.dupe(u8, "approved"),
                .grants_created = 1,
            };
        }

        fn clearAuth(_: std.mem.Allocator) !bool {
            clear_calls += 1;
            return true;
        }
    };

    TestCtx.ensure_calls = 0;
    TestCtx.review_calls = 0;
    TestCtx.clear_calls = 0;

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    var result = try reviewPlanWithAuthRetryFns(
        std.testing.allocator,
        "plan-1",
        .approve,
        .grant,
        buffered.printer(),
        TestCtx.ensureSignedIn,
        TestCtx.reviewPlan,
        TestCtx.clearAuth,
    );
    defer result.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 2), TestCtx.ensure_calls);
    try std.testing.expectEqual(@as(usize, 2), TestCtx.review_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.clear_calls);
    try std.testing.expectEqualStrings("approved", result.status);
    try std.testing.expectEqual(@as(usize, 1), result.grants_created);
}

test "reviewPlanWithAuthRetry does not clear auth for plan conflicts" {
    const TestCtx = struct {
        var ensure_calls: usize = 0;
        var review_calls: usize = 0;
        var clear_calls: usize = 0;

        fn ensureSignedIn(allocator: std.mem.Allocator, _: Printer) !auth.Credentials {
            ensure_calls += 1;
            return .{
                .token = try allocator.dupe(u8, "token"),
                .email = null,
            };
        }

        fn reviewPlan(
            _: std.mem.Allocator,
            _: []const u8,
            _: []const u8,
            _: control_plane.ReviewAction,
            _: control_plane.ReviewMode,
        ) !control_plane.ReviewResult {
            review_calls += 1;
            return error.PlanConflict;
        }

        fn clearAuth(_: std.mem.Allocator) !bool {
            clear_calls += 1;
            return true;
        }
    };

    TestCtx.ensure_calls = 0;
    TestCtx.review_calls = 0;
    TestCtx.clear_calls = 0;

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    try std.testing.expectError(
        error.PlanConflict,
        reviewPlanWithAuthRetryFns(
            std.testing.allocator,
            "plan-1",
            .reject,
            .once,
            buffered.printer(),
            TestCtx.ensureSignedIn,
            TestCtx.reviewPlan,
            TestCtx.clearAuth,
        ),
    );

    try std.testing.expectEqual(@as(usize, 1), TestCtx.ensure_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.review_calls);
    try std.testing.expectEqual(@as(usize, 0), TestCtx.clear_calls);
}

test "fetchDeployPlanWithAuthRetry retries after rejected saved token" {
    const TestCtx = struct {
        var ensure_calls: usize = 0;
        var fetch_calls: usize = 0;
        var clear_calls: usize = 0;

        fn ensureSignedIn(allocator: std.mem.Allocator, _: Printer) !auth.Credentials {
            ensure_calls += 1;
            return .{
                .token = try allocator.dupe(u8, if (ensure_calls == 1) "expired-token" else "fresh-token"),
                .email = null,
            };
        }

        fn fetchPlan(
            allocator: std.mem.Allocator,
            token: []const u8,
            plan_id: []const u8,
        ) !control_plane.DeployPlanStatus {
            fetch_calls += 1;
            try std.testing.expectEqualStrings("plan-1", plan_id);
            if (std.mem.eql(u8, token, "expired-token")) return error.NotSignedIn;
            try std.testing.expectEqualStrings("fresh-token", token);
            return .{
                .id = try allocator.dupe(u8, "plan-1"),
                .project_name = try allocator.dupe(u8, "demo"),
                .status = try allocator.dupe(u8, "pending"),
                .baseline_sha = try allocator.dupe(u8, "base"),
                .proposed_sha = try allocator.dupe(u8, "next"),
                .diff_reasons = try allocator.dupe([]u8, &.{try allocator.dupe(u8, "new env read: SECRET_KEY")}),
                .created_at = try allocator.dupe(u8, "2026-04-14T12:00:00.000Z"),
                .expires_at = try allocator.dupe(u8, "2026-04-14T12:30:00.000Z"),
                .decided_at = null,
                .decided_by_token_id = null,
                .consumed_at = null,
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

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    var plan = try fetchDeployPlanWithAuthRetryFns(
        std.testing.allocator,
        "plan-1",
        buffered.printer(),
        TestCtx.ensureSignedIn,
        TestCtx.fetchPlan,
        TestCtx.clearAuth,
    );
    defer plan.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 2), TestCtx.ensure_calls);
    try std.testing.expectEqual(@as(usize, 2), TestCtx.fetch_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.clear_calls);
    try std.testing.expectEqualStrings("pending", plan.status);
}

test "fetchCapabilityGrantsWithAuthRetry retries after rejected saved token" {
    const TestCtx = struct {
        var ensure_calls: usize = 0;
        var fetch_calls: usize = 0;
        var clear_calls: usize = 0;

        fn ensureSignedIn(allocator: std.mem.Allocator, _: Printer) !auth.Credentials {
            ensure_calls += 1;
            return .{
                .token = try allocator.dupe(u8, if (ensure_calls == 1) "expired-token" else "fresh-token"),
                .email = null,
            };
        }

        fn fetchGrants(
            allocator: std.mem.Allocator,
            token: []const u8,
            project_name: ?[]const u8,
        ) !control_plane.CapabilityGrantList {
            fetch_calls += 1;
            try std.testing.expect(project_name != null);
            try std.testing.expectEqualStrings("demo", project_name.?);
            if (std.mem.eql(u8, token, "expired-token")) return error.NotSignedIn;
            try std.testing.expectEqualStrings("fresh-token", token);

            const items = try allocator.alloc(control_plane.CapabilityGrant, 1);
            items[0] = .{
                .id = try allocator.dupe(u8, "grant-1"),
                .project_name = try allocator.dupe(u8, "demo"),
                .source_plan_id = try allocator.dupe(u8, "plan-1"),
                .created_at = try allocator.dupe(u8, "2026-04-14T12:00:00.000Z"),
                .expires_at = null,
                .revoked_at = null,
                .status = try allocator.dupe(u8, "active"),
                .reasons = try allocator.dupe([]u8, &.{try allocator.dupe(u8, "new env read: SECRET_KEY")}),
            };
            return .{ .items = items };
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

    var grants_list = try fetchCapabilityGrantsWithAuthRetryFns(
        std.testing.allocator,
        "demo",
        buffered.printer(),
        TestCtx.ensureSignedIn,
        TestCtx.fetchGrants,
        TestCtx.clearAuth,
    );
    defer grants_list.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 2), TestCtx.ensure_calls);
    try std.testing.expectEqual(@as(usize, 2), TestCtx.fetch_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.clear_calls);
    try std.testing.expectEqual(@as(usize, 1), grants_list.items.len);
}

test "revokeCapabilityGrantWithAuthRetry retries after rejected saved token" {
    const TestCtx = struct {
        var ensure_calls: usize = 0;
        var revoke_calls: usize = 0;
        var clear_calls: usize = 0;

        fn ensureSignedIn(allocator: std.mem.Allocator, _: Printer) !auth.Credentials {
            ensure_calls += 1;
            return .{
                .token = try allocator.dupe(u8, if (ensure_calls == 1) "expired-token" else "fresh-token"),
                .email = null,
            };
        }

        fn revokeGrant(
            allocator: std.mem.Allocator,
            token: []const u8,
            grant_id: []const u8,
        ) !control_plane.CapabilityGrant {
            revoke_calls += 1;
            try std.testing.expectEqualStrings("grant-1", grant_id);
            if (std.mem.eql(u8, token, "expired-token")) return error.NotSignedIn;
            try std.testing.expectEqualStrings("fresh-token", token);
            return .{
                .id = try allocator.dupe(u8, "grant-1"),
                .project_name = try allocator.dupe(u8, "demo"),
                .source_plan_id = try allocator.dupe(u8, "plan-1"),
                .created_at = try allocator.dupe(u8, "2026-04-14T12:00:00.000Z"),
                .expires_at = null,
                .revoked_at = try allocator.dupe(u8, "2026-04-14T13:00:00.000Z"),
                .status = try allocator.dupe(u8, "revoked"),
                .reasons = try allocator.dupe([]u8, &.{try allocator.dupe(u8, "new effect: write")}),
            };
        }

        fn clearAuth(_: std.mem.Allocator) !bool {
            clear_calls += 1;
            return true;
        }
    };

    TestCtx.ensure_calls = 0;
    TestCtx.revoke_calls = 0;
    TestCtx.clear_calls = 0;

    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    var grant = try revokeCapabilityGrantWithAuthRetryFns(
        std.testing.allocator,
        "grant-1",
        buffered.printer(),
        TestCtx.ensureSignedIn,
        TestCtx.revokeGrant,
        TestCtx.clearAuth,
    );
    defer grant.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 2), TestCtx.ensure_calls);
    try std.testing.expectEqual(@as(usize, 2), TestCtx.revoke_calls);
    try std.testing.expectEqual(@as(usize, 1), TestCtx.clear_calls);
    try std.testing.expectEqualStrings("revoked", grant.status);
}

test "printDeployPlanRequired shows review summary" {
    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    var plan = control_plane.DeployPlanRequired{
        .plan_id = try std.testing.allocator.dupe(u8, "plan-1"),
        .review_url = try std.testing.allocator.dupe(u8, "https://control/deploy/plans/plan-1"),
        .baseline_sha = null,
        .proposed_sha = null,
        .expires_at = null,
        .diff_reasons = try std.testing.allocator.dupe([]u8, &.{try std.testing.allocator.dupe(u8, "new env read: SECRET_KEY")}),
        .covered_reasons = try std.testing.allocator.dupe([]u8, &.{try std.testing.allocator.dupe(u8, "new env read: DATABASE_URL")}),
        .uncovered_reasons = try std.testing.allocator.dupe([]u8, &.{try std.testing.allocator.dupe(u8, "new effect: write")}),
    };
    defer plan.deinit(std.testing.allocator);

    try printDeployPlanRequired(std.testing.allocator, buffered.printer(), &plan);

    try std.testing.expect(std.mem.indexOf(u8, buffered.stdoutSlice(), "Plan ID:") != null);
    try std.testing.expect(std.mem.indexOf(u8, buffered.stderrSlice(), "already granted") != null);
    try std.testing.expect(std.mem.indexOf(u8, buffered.stderrSlice(), "needs review") != null);
}

test "printReviewStatus shows plan details" {
    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    var plan = control_plane.DeployPlanStatus{
        .id = try std.testing.allocator.dupe(u8, "plan-1"),
        .project_name = try std.testing.allocator.dupe(u8, "demo"),
        .status = try std.testing.allocator.dupe(u8, "approved"),
        .baseline_sha = try std.testing.allocator.dupe(u8, "base"),
        .proposed_sha = try std.testing.allocator.dupe(u8, "next"),
        .diff_reasons = try std.testing.allocator.dupe([]u8, &.{
            try std.testing.allocator.dupe(u8, "new env read: SECRET_KEY"),
            try std.testing.allocator.dupe(u8, "new effect: write"),
        }),
        .created_at = try std.testing.allocator.dupe(u8, "2026-04-14T12:00:00.000Z"),
        .expires_at = try std.testing.allocator.dupe(u8, "2026-04-14T12:30:00.000Z"),
        .decided_at = try std.testing.allocator.dupe(u8, "2026-04-14T12:05:00.000Z"),
        .decided_by_token_id = try std.testing.allocator.dupe(u8, "token-1"),
        .consumed_at = null,
    };
    defer plan.deinit(std.testing.allocator);

    try printReviewStatus(std.testing.allocator, buffered.printer(), &plan);

    try std.testing.expect(std.mem.indexOf(u8, buffered.stdoutSlice(), "Plan ID:") != null);
    try std.testing.expect(std.mem.indexOf(u8, buffered.stdoutSlice(), "Project:") != null);
    try std.testing.expect(std.mem.indexOf(u8, buffered.stdoutSlice(), "Status:") != null);
    try std.testing.expect(std.mem.indexOf(u8, buffered.stdoutSlice(), "Risk:       new env read: SECRET_KEY") != null);
    try std.testing.expectEqualStrings("", buffered.stderrSlice());
}

test "printCapabilityGrants shows grant details" {
    var buffered = printer_mod.BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    const items = try std.testing.allocator.alloc(control_plane.CapabilityGrant, 1);
    items[0] = .{
        .id = try std.testing.allocator.dupe(u8, "grant-1"),
        .project_name = try std.testing.allocator.dupe(u8, "demo"),
        .source_plan_id = try std.testing.allocator.dupe(u8, "plan-1"),
        .created_at = try std.testing.allocator.dupe(u8, "2026-04-14T12:00:00.000Z"),
        .expires_at = try std.testing.allocator.dupe(u8, "2026-04-15T12:00:00.000Z"),
        .revoked_at = null,
        .status = try std.testing.allocator.dupe(u8, "active"),
        .reasons = try std.testing.allocator.dupe([]u8, &.{
            try std.testing.allocator.dupe(u8, "new env read: SECRET_KEY"),
            try std.testing.allocator.dupe(u8, "new effect: write"),
        }),
    };
    var grants_list = control_plane.CapabilityGrantList{ .items = items };
    defer grants_list.deinit(std.testing.allocator);

    try printCapabilityGrants(std.testing.allocator, buffered.printer(), &grants_list);

    try std.testing.expect(std.mem.indexOf(u8, buffered.stdoutSlice(), "Grants:") != null);
    try std.testing.expect(std.mem.indexOf(u8, buffered.stdoutSlice(), "grant-1") != null);
    try std.testing.expect(std.mem.indexOf(u8, buffered.stdoutSlice(), "Scope:      new env read: SECRET_KEY, new effect: write") != null);
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
        .request_body = try allocator.dupe(u8, "{\"projectName\":\"demo\",\"contract\":{}}"),
        .grant_ids = try allocator.alloc([]u8, 0),
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
