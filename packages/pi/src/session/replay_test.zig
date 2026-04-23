//! Gating end-to-end tests for session persistence + resume + replay-safety.
//!
//! The critical invariant: after resuming a session whose workspace has been
//! mutated on disk, the reconstructed `assistant_tool_use` that encoded the
//! original `apply_edit` must NOT re-apply — replay mode suppresses all
//! filesystem writes during the first turn after resume.

const std = @import("std");
const zigts = @import("zigts");

const agent = @import("../agent.zig");
const registry_mod = @import("../registry/registry.zig");
const turn = @import("../turn.zig");
const loop = @import("../loop.zig");
const transcript_mod = @import("../transcript.zig");
const persister = @import("persister.zig");

const testing = std.testing;

const clean_handler =
    "function handler(req: Request): Response { return Response.json({ok: true}); }";

// ---------------------------------------------------------------------------
// Scaffolding
// ---------------------------------------------------------------------------

const IsolatedTmp = @import("../test_support/tmp.zig").IsolatedTmp;
const EnvOverride = @import("../test_support/env.zig").EnvOverride;
const cwdPathAlloc = @import("../test_support/cwd.zig").cwdPathAlloc;

fn initTmp(allocator: std.mem.Allocator) !IsolatedTmp {
    return IsolatedTmp.init(allocator, "replay");
}

/// Test client that canned-returns a single apply-edit reply whose content
/// is `clean_handler` targeting `handler.ts` at the workspace root.
const EditClient = struct {
    call_count: usize = 0,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const @import("../transcript.zig").Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!loop.ModelCallResult {
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
        const self: *EditClient = @ptrCast(@alignCast(ctx));
        self.call_count += 1;
        return .{ .reply = .{ .response = .{ .edit = .{
            .file = "handler.ts",
            .content = clean_handler,
            .before = null,
        } } } };
    }

    fn asModelClient(self: *EditClient) loop.ModelClient {
        return .{ .context = self, .request_fn = requestFn };
    }
};

fn buildSession(
    allocator: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    resume_latest: bool,
) !agent.AgentSession {
    // Tests unset ANTHROPIC_API_KEY to keep the session stub-backed; the
    // EditClient is fed directly to `runOneTurnWithClient`, so the
    // backend vtable is never exercised.
    return try agent.initFromEnvWithSessionConfig(allocator, registry, .{
        .resume_latest = resume_latest,
    });
}

/// Bespoke one-turn runner that uses an explicit ModelClient (our EditClient)
/// and replicates `agent.runOneTurn`'s persistence+replay bookkeeping.
fn runOneTurnWithClient(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    registry: *const registry_mod.Registry,
    client: loop.ModelClient,
    user_text: []const u8,
    workspace_root: []const u8,
) ![]u8 {
    const replay = session.replay_next_turn;
    session.replay_next_turn = false;

    _ = try loop.runTurnWith(
        allocator,
        client,
        registry,
        &session.transcript,
        user_text,
        .{ .replay_mode = replay, .workspace_root = workspace_root },
    );
    const tr = &session.transcript;

    if (session.events_path) |path| {
        const entries = tr.entries.items;
        while (session.last_persisted_len < entries.len) : (session.last_persisted_len += 1) {
            try persister.appendEntry(allocator, path, &entries[session.last_persisted_len], session.persist_opts);
        }
    }

    return transcript_mod.renderRichEntryToOwned(allocator, tr.at(tr.len() - 1));
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "replay-safety: resumed session over mutated workspace does not rewrite files" {
    const allocator = testing.allocator;

    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    // Isolate the sessions root under the tmp dir.
    const sessions_dir = try std.fs.path.join(allocator, &.{ tmp.abs_path, "sessions" });
    defer allocator.free(sessions_dir);
    var env_override = try EnvOverride.set(allocator, "ZIGTTP_SESSIONS_DIR", sessions_dir);
    defer env_override.restore(allocator);

    // Force the stub backend so live-client state never gets built.
    var api_override = try EnvOverride.unset(allocator, "ANTHROPIC_API_KEY");
    defer api_override.restore(allocator);

    // Workspace lives beside the sessions dir so the test stays inside the
    // isolated scratch tree after chdir.
    const ws_dir_raw = try std.fs.path.join(allocator, &.{ tmp.abs_path, "ws" });
    defer allocator.free(ws_dir_raw);
    {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), ws_dir_raw);
    }

    const saved_cwd = try cwdPathAlloc(allocator);
    defer allocator.free(saved_cwd);
    try std.Io.Threaded.chdir(ws_dir_raw);
    defer std.Io.Threaded.chdir(saved_cwd) catch {};

    // macOS symlinks /tmp -> /private/tmp; the loop resolves the workspace
    // via realpath and any edit path against that, so pin the comparable
    // handler path off the realpath'd cwd.
    const ws_dir = try cwdPathAlloc(allocator);
    defer allocator.free(ws_dir);

    const handler_path = try std.fs.path.join(allocator, &.{ ws_dir, "handler.ts" });
    defer allocator.free(handler_path);

    var registry: registry_mod.Registry = .{};
    defer registry.deinit(allocator);

    var client: EditClient = .{};

    // --- First session: edit lands, events.jsonl gets entries. ---
    var first_session_id: []u8 = undefined;
    {
        var session = try buildSession(allocator, &registry, false);
        defer session.deinit(allocator);

        const rendered = try runOneTurnWithClient(
            allocator,
            &session,
            &registry,
            client.asModelClient(),
            "add an ok response",
            ws_dir,
        );
        allocator.free(rendered);

        // The edit was applied.
        const written = try zigts.file_io.readFile(allocator, handler_path, 1024 * 1024);
        defer allocator.free(written);
        try testing.expectEqualStrings(clean_handler, written);

        // events.jsonl has non-empty content.
        const events_path = session.events_path.?;
        const raw = try zigts.file_io.readFile(allocator, events_path, 1024 * 1024);
        defer allocator.free(raw);
        try testing.expect(raw.len > 0);

        first_session_id = try allocator.dupe(u8, session.session_id.?);
    }
    defer allocator.free(first_session_id);

    // --- Mutate the file on disk so we can detect any replay write. ---
    const mutated_bytes = "MUTATED\n";
    try zigts.file_io.writeFile(allocator, handler_path, mutated_bytes);

    // --- Resume the session and run one turn. The first turn after resume
    //     runs in replay_mode and must NOT touch handler.ts. ---
    {
        var session = try buildSession(allocator, &registry, true);
        defer session.deinit(allocator);

        try testing.expectEqualStrings(first_session_id, session.session_id.?);
        try testing.expect(session.replay_next_turn);

        const rendered = try runOneTurnWithClient(
            allocator,
            &session,
            &registry,
            client.asModelClient(),
            "retry",
            ws_dir,
        );
        allocator.free(rendered);

        // replay_next_turn must have cleared after the first turn.
        try testing.expect(!session.replay_next_turn);

        // CRITICAL: handler.ts still equals the mutated bytes.
        const after = try zigts.file_io.readFile(allocator, handler_path, 1024 * 1024);
        defer allocator.free(after);
        try testing.expectEqualStrings(mutated_bytes, after);
    }
}

test "replay-safety: /new starts a fresh session_id and fresh events.jsonl" {
    const allocator = testing.allocator;

    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const sessions_dir = try std.fs.path.join(allocator, &.{ tmp.abs_path, "sessions" });
    defer allocator.free(sessions_dir);
    var env_override = try EnvOverride.set(allocator, "ZIGTTP_SESSIONS_DIR", sessions_dir);
    defer env_override.restore(allocator);

    var api_override = try EnvOverride.unset(allocator, "ANTHROPIC_API_KEY");
    defer api_override.restore(allocator);

    const ws_dir_raw = try std.fs.path.join(allocator, &.{ tmp.abs_path, "ws" });
    defer allocator.free(ws_dir_raw);
    {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), ws_dir_raw);
    }

    const saved_cwd = try cwdPathAlloc(allocator);
    defer allocator.free(saved_cwd);
    try std.Io.Threaded.chdir(ws_dir_raw);
    defer std.Io.Threaded.chdir(saved_cwd) catch {};

    // macOS symlinks /tmp -> /private/tmp; the loop resolves the workspace
    // via realpath and any edit path against that, so pin the comparable
    // handler path off the realpath'd cwd.
    const ws_dir = try cwdPathAlloc(allocator);
    defer allocator.free(ws_dir);

    var registry: registry_mod.Registry = .{};
    defer registry.deinit(allocator);

    var client: EditClient = .{};

    var first_id: []u8 = undefined;
    {
        var session = try buildSession(allocator, &registry, false);
        defer session.deinit(allocator);

        const rendered = try runOneTurnWithClient(
            allocator,
            &session,
            &registry,
            client.asModelClient(),
            "first intent",
            ws_dir,
        );
        allocator.free(rendered);

        const raw = try zigts.file_io.readFile(allocator, session.events_path.?, 1024 * 1024);
        defer allocator.free(raw);
        try testing.expect(raw.len > 0);

        first_id = try allocator.dupe(u8, session.session_id.?);
    }
    defer allocator.free(first_id);

    // /new is equivalent to rebuilding with no resume and no explicit id.
    var fresh = try buildSession(allocator, &registry, false);
    defer fresh.deinit(allocator);

    try testing.expect(!std.mem.eql(u8, first_id, fresh.session_id.?));

    // Fresh events.jsonl does not exist yet (no turn has run).
    try testing.expect(!zigts.file_io.fileExists(allocator, fresh.events_path.?));
}
