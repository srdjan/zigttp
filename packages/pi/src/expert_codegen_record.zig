//! expert_codegen_record - live baseline recorder for the codegen eval.
//!
//! Recording spends real tokens, so everything here is gated behind
//! `ZIGTTP_CODEGEN_RECORD=1` and a live ANTHROPIC_API_KEY; without both, every
//! test skips and the default `zig build test-expert-app` never touches the
//! network. The recorder drives real expert turns (full persona + tool
//! registry) through the live Anthropic client, which tees each roundtrip's SSE
//! body to a per-case cassette. Those cassettes are then committed and replayed
//! deterministically (free) by the offline eval. Recorded once, replayed
//! forever.

const std = @import("std");
const zigts = @import("zigts");
const anthropic = @import("providers/anthropic/client.zig");
const cassette_client = @import("providers/cassette_client.zig");
const transcript_mod = @import("transcript.zig");
const loop = @import("loop.zig");
const app = @import("app.zig");
const agent = @import("agent.zig");
const codegen = @import("expert_codegen_eval.zig");
const IsolatedTmp = @import("test_support/tmp.zig").IsolatedTmp;
const cwdPathAlloc = @import("test_support/cwd.zig").cwdPathAlloc;

const testing = std.testing;

/// Replays a recorded multi-roundtrip session: each model request is served the
/// next committed cassette step (step_0, step_1, ...), parsed through the same
/// assembler the live client uses. The transcript is ignored - the recorded
/// responses are authoritative - so the loop re-executes the recorded tool calls
/// and re-vetoes the recorded edit deterministically and offline.
const CassetteSequenceClient = struct {
    steps: []const []const u8,
    index: usize = 0,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        tr: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!loop.ModelCallResult {
        const self: *CassetteSequenceClient = @ptrCast(@alignCast(ctx));
        _ = tr;
        _ = extra_user_text;
        if (self.index >= self.steps.len) return error.CassetteSequenceExhausted;
        const cassette = try cassette_client.loadCassetteFromBytes(arena, self.steps[self.index], null);
        self.index += 1;
        return try cassette_client.replay(arena, cassette);
    }

    pub fn asClient(self: *CassetteSequenceClient) loop.ModelClient {
        return .{ .context = self, .request_fn = requestFn };
    }
};

/// Read step_0.jsonl, step_1.jsonl, ... from an absolute case directory until a
/// step is missing. Returns an empty slice when the case has no cassette yet.
fn readCaseSteps(allocator: std.mem.Allocator, dir_abs: []const u8) ![][]u8 {
    var list: std.ArrayList([]u8) = .empty;
    errdefer {
        for (list.items) |s| allocator.free(s);
        list.deinit(allocator);
    }
    var i: usize = 0;
    while (true) : (i += 1) {
        const path = try std.fmt.allocPrint(allocator, "{s}/step_{d}.jsonl", .{ dir_abs, i });
        defer allocator.free(path);
        const bytes = zigts.file_io.readFile(allocator, path, 4 * 1024 * 1024) catch break;
        try list.append(allocator, bytes);
    }
    return list.toOwnedSlice(allocator);
}

/// Where committed cassettes live, relative to the repo root (the cwd when the
/// recorder runs via `zig build`).
pub const cassette_root = "packages/pi/src/providers/testdata/codegen";

/// Borrowed env-var read (no allocation), mirroring agent.zig's `envVar`:
/// std.process env helpers are not the 0.16 path; std.c.getenv is.
fn envValue(name_z: [:0]const u8) ?[]const u8 {
    const raw = std.c.getenv(name_z) orelse return null;
    const v = std.mem.sliceTo(raw, 0);
    return if (v.len == 0) null else v;
}

fn recordingRequested() bool {
    const flag = envValue("ZIGTTP_CODEGEN_RECORD") orelse return false;
    return std.mem.eql(u8, flag, "1");
}

// Smoke test: prove the live record-tee writes a cassette that replays to the
// same reply, with a single cheap call, before spending tokens on the full
// corpus. Skipped unless ZIGTTP_CODEGEN_RECORD=1 and a key is present.
test "record-tee captures a faithful anthropic cassette (live, gated)" {
    const allocator = testing.allocator;
    if (!recordingRequested()) return error.SkipZigTest;
    const key = envValue("ANTHROPIC_API_KEY") orelse return error.SkipZigTest;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var client = anthropic.Client.init(.{
        .api_key = key,
        .system_prompt = "You are a terse assistant. Reply with exactly one word.",
    });
    client.enableRecording(cassette_root, "_smoke");

    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(allocator);
    try tr.append(allocator, .{ .user_text = "Say OK." });

    const live = client.sendTurn(a, &tr, null) catch |err| {
        std.debug.print("[codegen-smoke] sendTurn failed: {s}\n", .{@errorName(err)});
        return err;
    };

    // Replay the just-written cassette and confirm it round-trips to a reply.
    const path = cassette_root ++ "/_smoke/step_0.jsonl";
    const cassette = try cassette_client.loadCassetteFromPath(a, path);
    const replayed = try cassette_client.replay(a, cassette);

    const live_kind = std.meta.activeTag(live.reply.response);
    const replay_kind = std.meta.activeTag(replayed.reply.response);
    try testing.expectEqual(live_kind, replay_kind);
    std.debug.print("[codegen-smoke] live and replay agree: {s}\n", .{@tagName(replay_kind)});
}

const RecordCase = struct {
    name: []const u8,
    prompt: []const u8,
    seed_files: []const codegen.SeedFile = &.{},
};

// Representative first-baseline tasks. These elicit realistic multi-roundtrip
// agent behaviour (explore then edit), so each records as step_0/step_1/...
const record_corpus = [_]RecordCase{
    .{
        .name = "health",
        .prompt = "Create a handler in handler.ts that responds to GET /health with " ++
            "Response.json({ ok: true }). Keep it minimal and deterministic.",
    },
    .{
        .name = "validate-body",
        .prompt = "Create a handler in handler.ts that decodes the JSON request body with " ++
            "zigttp:validate against a schema named \"item\" requiring a string field \"name\", " ++
            "returns the validated data on success, and returns a 400 with the errors on failure.",
    },
    .{
        .name = "jwt-auth",
        .prompt = "Create a handler in handler.ts that requires a bearer JWT using zigttp:auth " ++
            "with the secret from env JWT_SECRET, returns 401 when the token is missing or invalid, " ++
            "and otherwise returns the verified claims as JSON. Never use a fallback secret.",
    },
};

// Record the real expert agent against the corpus and report the live baseline.
// Gated: ZIGTTP_CODEGEN_RECORD=1 + a live key. Each case runs in its own tmp
// workspace with cwd switched to it, so the agent's tools and the edit veto
// resolve the same files; cassettes are written to an absolute repo path so the
// chdir does not misplace them. ZIGTTP_CODEGEN_LIMIT caps the case count for a
// cheap small-scale validation before the full run.
test "record codegen baseline corpus (live, gated)" {
    if (!recordingRequested()) return error.SkipZigTest;
    if (envValue("ANTHROPIC_API_KEY") == null) return error.SkipZigTest;
    // A live recording driver, not a memory-correctness test: use an arena over
    // the page allocator so the strict test allocator's leak check does not flag
    // the live HTTP/TLS stack (which the deterministic tests never exercise).
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var registry = try app.buildRegistry(allocator);
    defer registry.deinit(allocator);
    var session = try agent.initFromEnvWithSessionConfig(allocator, &registry, .{
        .no_session = true,
        .no_context_files = true,
    });
    defer session.deinit(allocator);
    if (session.authKind() != .anthropic_api_key) return error.SkipZigTest;

    const repo_root = try cwdPathAlloc(allocator);
    defer allocator.free(repo_root);
    const out_dir = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ repo_root, cassette_root });
    defer allocator.free(out_dir);

    var limit: usize = record_corpus.len;
    if (envValue("ZIGTTP_CODEGEN_LIMIT")) |lim| {
        limit = std.fmt.parseInt(usize, lim, 10) catch limit;
    }

    var first_draft_passes: usize = 0;
    var greens: usize = 0;
    var total: usize = 0;
    for (record_corpus, 0..) |rc, i| {
        if (i >= limit) break;
        total += 1;

        var tmp = try IsolatedTmp.init(allocator, "codegen-record");
        defer tmp.cleanup(allocator);
        for (rc.seed_files) |sf| try tmp.writeFile(allocator, sf.path, sf.bytes);

        const saved_cwd = try cwdPathAlloc(allocator);
        defer allocator.free(saved_cwd);
        try std.Io.Threaded.chdir(tmp.abs_path);
        defer std.Io.Threaded.chdir(saved_cwd) catch {};

        session.backend.anthropic.enableRecording(out_dir, rc.name);

        var tr: transcript_mod.Transcript = .{};
        defer tr.deinit(allocator);
        const result = loop.runTurnWith(allocator, session.modelClient(), &registry, &tr, rc.prompt, .{
            .workspace_root = ".",
            .max_attempts = loop.interactive_max_attempts,
            .approval_fn = loop.ApprovalFn.fromFn(loop.autoApprove),
            .replay_mode = false,
            .turn_timeout_ms = 0,
        }) catch |err| {
            std.debug.print("[codegen-record] {s}: turn failed: {s}\n", .{ rc.name, @errorName(err) });
            return err;
        };
        if (result.first_draft_veto_pass) first_draft_passes += 1;
        if (result.applied_edit) greens += 1;
        std.debug.print(
            "[codegen-record] {s}: first_draft_pass={} applied={} compiler_authored={} roundtrips={d} retries={d} tools={d} steps={d}\n",
            .{
                rc.name,
                result.first_draft_veto_pass,
                result.applied_edit,
                result.compiler_authored_apply,
                result.roundtrips,
                result.veto_retry_count,
                result.tool_call_count,
                session.backend.anthropic.record_step,
            },
        );
    }
    std.debug.print(
        "[codegen-record] BASELINE first-draft pass: {d}/{d}; reached-green: {d}/{d}\n",
        .{ first_draft_passes, total, greens, total },
    );
}

// Offline ratchet: replay every committed cassette through the real veto and
// require it still passes on the first draft. Runs in normal CI (no network, no
// key): it reproduces the recorded baseline deterministically and fails if a
// compiler/policy change would make a previously-clean recorded edit regress.
// Uses an arena over the page allocator (the replay executes the full tool +
// veto stack; this is a fidelity check, not a leak test).
test "codegen baseline replays at the committed first-draft pass rate" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const repo_root = try cwdPathAlloc(a);
    const codegen_dir = try std.fmt.allocPrint(a, "{s}/{s}", .{ repo_root, cassette_root });

    var registry = try app.buildRegistry(a);
    defer registry.deinit(a);

    var with_cassettes: usize = 0;
    var passes: usize = 0;
    for (record_corpus) |rc| {
        const dir_abs = try std.fmt.allocPrint(a, "{s}/{s}", .{ codegen_dir, rc.name });
        // Read the cassette steps from the repo (absolute) BEFORE chdir.
        const steps = readCaseSteps(a, dir_abs) catch &.{};
        if (steps.len == 0) continue; // case not recorded yet
        with_cassettes += 1;

        var tmp = try IsolatedTmp.init(a, "codegen-replay");
        defer tmp.cleanup(a);
        for (rc.seed_files) |sf| try tmp.writeFile(a, sf.path, sf.bytes);

        const saved_cwd = try cwdPathAlloc(a);
        try std.Io.Threaded.chdir(tmp.abs_path);
        defer std.Io.Threaded.chdir(saved_cwd) catch {};

        var client: CassetteSequenceClient = .{ .steps = steps };
        var tr: transcript_mod.Transcript = .{};
        const result = try loop.runTurnWith(a, client.asClient(), &registry, &tr, rc.prompt, .{
            .workspace_root = ".",
            .max_attempts = loop.interactive_max_attempts,
            .approval_fn = loop.ApprovalFn.fromFn(loop.autoApprove),
            .replay_mode = false,
            .turn_timeout_ms = 0,
        });
        if (result.first_draft_veto_pass) passes += 1;
    }

    // Ratchet: at least one committed cassette, and every one of them still
    // passes veto on the first draft.
    try testing.expect(with_cassettes > 0);
    try testing.expectEqual(with_cassettes, passes);
}
