const std = @import("std");
const zigts = @import("zigts");

const session_events = @import("session/events.zig");
const session_id_mod = @import("session/session_id.zig");
const session_paths = @import("session/paths.zig");
const proof_enrichment = @import("proof_enrichment.zig");
const ui_payload = @import("ui_payload.zig");

const marker_relpath = ".zigttp/proof-passport-session";
const dir_marker_relpath = ".zigttp/proof-passport-session-dir";

pub const Step = enum {
    baseline,
    witness,
    repaired,
    deployed,
};

pub const SessionInfo = struct {
    session_id: []u8,
    session_dir: []u8,
    events_path: []u8,
    meta_path: []u8,
    expert_command: []u8,

    pub fn deinit(self: *SessionInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.session_id);
        allocator.free(self.session_dir);
        allocator.free(self.events_path);
        allocator.free(self.meta_path);
        allocator.free(self.expert_command);
        self.* = undefined;
    }
};

pub const AppendOptions = struct {
    workspace_root: []const u8,
    handler_path: []const u8,
    step: Step,
    before: ?[]const u8 = null,
    after: ?[]const u8 = null,
    deploy_artifact: ?[]const u8 = null,
};

pub fn ensureSession(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
) !SessionInfo {
    const session_id = try readOrCreateSessionId(allocator, workspace_root);
    errdefer allocator.free(session_id);

    const session_dir = try readOrCreateSessionDir(allocator, workspace_root, session_id);
    errdefer allocator.free(session_dir);

    const realpath = try workspaceRealpath(allocator, workspace_root);
    defer allocator.free(realpath);
    try session_paths.writeWorkspacePointer(allocator, session_dir, realpath);

    const events_path = try std.fs.path.join(allocator, &.{ session_dir, "events.jsonl" });
    errdefer allocator.free(events_path);
    const meta_path = try std.fs.path.join(allocator, &.{ session_dir, "meta.json" });
    errdefer allocator.free(meta_path);

    if (!zigts.file_io.fileExists(allocator, meta_path)) {
        const policy_hash = zigts.rule_registry.policyHash();
        try session_events.writeMeta(allocator, meta_path, .{
            .session_id = session_id,
            .workspace_realpath = realpath,
            .created_at_unix_ms = nowUnixMs(),
            .policy_hash = policy_hash[0..],
        });
    }

    const expert_command = try buildExpertCommand(allocator, realpath, session_id);
    errdefer allocator.free(expert_command);

    return .{
        .session_id = session_id,
        .session_dir = session_dir,
        .events_path = events_path,
        .meta_path = meta_path,
        .expert_command = expert_command,
    };
}

pub fn appendStep(
    allocator: std.mem.Allocator,
    options: AppendOptions,
) !SessionInfo {
    var info = try ensureSession(allocator, options.workspace_root);
    errdefer info.deinit(allocator);

    switch (options.step) {
        .baseline => try appendBaseline(allocator, info.events_path),
        .witness => try appendWitness(allocator, info.events_path, options.handler_path),
        .repaired => try appendVerifiedPatch(allocator, info.events_path, options),
        .deployed => try appendDeployed(allocator, info.events_path, options.deploy_artifact),
    }
    return info;
}

pub fn resetToBaseline(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
) !SessionInfo {
    var info = try ensureSession(allocator, workspace_root);
    errdefer info.deinit(allocator);
    try zigts.file_io.writeFile(allocator, info.events_path, "");
    try appendBaseline(allocator, info.events_path);
    return info;
}

fn appendBaseline(
    allocator: std.mem.Allocator,
    events_path: []const u8,
) !void {
    try session_events.appendEvent(allocator, events_path, .{
        .system_note = "Proof Passport baseline: Studio loaded a green demo workspace with declared specs for injection_safe and no_secret_leakage.",
    });
}

fn appendWitness(
    allocator: std.mem.Allocator,
    events_path: []const u8,
    handler_path: []const u8,
) !void {
    const body = try std.fmt.allocPrint(
        allocator,
        "Proof Passport witness: unsafe SECRET_KEY flow in {s}. GET /status moves env(\"SECRET_KEY\") into Response.json; no_secret_leakage is broken until the status payload is repaired.",
        .{handler_path},
    );
    defer allocator.free(body);

    const payload_text = try allocator.dupe(u8, body);
    errdefer allocator.free(payload_text);
    var payload: ui_payload.UiPayload = .{ .plain_text = payload_text };
    defer payload.deinit(allocator);

    try session_events.appendEvent(allocator, events_path, .{ .diagnostic_box = .{
        .llm_text = body,
        .ui_payload = payload,
    } });
}

fn appendVerifiedPatch(
    allocator: std.mem.Allocator,
    events_path: []const u8,
    options: AppendOptions,
) !void {
    const before = options.before orelse return error.MissingBeforeSource;
    const after = options.after orelse return error.MissingAfterSource;
    const policy_hash = zigts.rule_registry.policyHash();

    var patch = try proof_enrichment.buildVerifiedPatchPayload(allocator, .{
        .workspace_root = options.workspace_root,
        .file = options.handler_path,
        .before = before,
        .after = after,
        .policy_hash = policy_hash[0..],
        .applied_at_unix_ms = nowUnixMs(),
        .post_apply_ok = true,
        .post_apply_summary = "demo repair removed the SECRET_KEY response flow",
        .goal_context = &.{ "no_secret_leakage", "injection_safe" },
    });
    defer patch.deinit(allocator);

    try session_events.appendEvent(allocator, events_path, .{ .verified_patch = .{
        .llm_text = "Verified patch: repaired src/handler.tsx and restored no_secret_leakage.",
        .ui_payload = .{ .verified_patch = patch },
    } });
}

fn appendDeployed(
    allocator: std.mem.Allocator,
    events_path: []const u8,
    deploy_artifact: ?[]const u8,
) !void {
    const artifact = deploy_artifact orelse ".zigttp/deploy/<service>";
    const body = try std.fmt.allocPrint(
        allocator,
        "Proof Passport deploy receipt: ledger .zigttp/proofs.jsonl and local artifact {s} are present.",
        .{artifact},
    );
    defer allocator.free(body);
    try session_events.appendEvent(allocator, events_path, .{ .system_note = body });
}

fn readOrCreateSessionId(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
) ![]u8 {
    const marker_path = try std.fs.path.join(allocator, &.{ workspace_root, marker_relpath });
    defer allocator.free(marker_path);

    if (zigts.file_io.readFile(allocator, marker_path, 4096)) |raw| {
        defer allocator.free(raw);
        const trimmed = std.mem.trim(u8, raw, " \t\r\n");
        if (trimmed.len > 0) return try allocator.dupe(u8, trimmed);
    } else |_| {}

    const id = try session_id_mod.generate(allocator);
    errdefer allocator.free(id);
    const line = try std.fmt.allocPrint(allocator, "{s}\n", .{id});
    defer allocator.free(line);
    try zigts.file_io.writeFile(allocator, marker_path, line);
    return id;
}

fn readOrCreateSessionDir(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    session_id: []const u8,
) ![]u8 {
    const marker_path = try std.fs.path.join(allocator, &.{ workspace_root, dir_marker_relpath });
    defer allocator.free(marker_path);

    if (zigts.file_io.readFile(allocator, marker_path, 4096)) |raw| {
        defer allocator.free(raw);
        const trimmed = std.mem.trim(u8, raw, " \t\r\n");
        if (trimmed.len > 0) return try allocator.dupe(u8, trimmed);
    } else |_| {}

    const session_dir = try session_paths.sessionDirForWorkspace(allocator, workspace_root, session_id);
    errdefer allocator.free(session_dir);
    const line = try std.fmt.allocPrint(allocator, "{s}\n", .{session_dir});
    defer allocator.free(line);
    try zigts.file_io.writeFile(allocator, marker_path, line);
    return session_dir;
}

fn workspaceRealpath(allocator: std.mem.Allocator, workspace_root: []const u8) ![:0]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    return try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io_backend.io(), workspace_root, allocator);
}

fn buildExpertCommand(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    session_id: []const u8,
) ![]u8 {
    const quoted_root = try shellQuote(allocator, workspace_root);
    defer allocator.free(quoted_root);
    const quoted_id = try shellQuote(allocator, session_id);
    defer allocator.free(quoted_id);
    return try std.fmt.allocPrint(allocator, "cd {s} && zigttp expert --session-id {s}", .{ quoted_root, quoted_id });
}

fn shellQuote(allocator: std.mem.Allocator, value: []const u8) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.append(allocator, '\'');
    for (value) |c| {
        if (c == '\'') {
            try out.appendSlice(allocator, "'\\''");
        } else {
            try out.append(allocator, c);
        }
    }
    try out.append(allocator, '\'');
    return out.toOwnedSlice(allocator);
}

fn nowUnixMs() i64 {
    var ts: std.posix.timespec = undefined;
    _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
    return @as(i64, ts.sec) * 1000 + @divTrunc(@as(i64, ts.nsec), 1_000_000);
}

const testing = std.testing;

test "demo passport expert command shell-quotes workspace and session" {
    const allocator = testing.allocator;
    const command = try buildExpertCommand(allocator, "/tmp/proof demo", "abc'123");
    defer allocator.free(command);
    try testing.expectEqualStrings("cd '/tmp/proof demo' && zigttp expert --session-id 'abc'\\''123'", command);
}
