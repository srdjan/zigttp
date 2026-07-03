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

/// Delete a case's cassette directory (`<out_dir_abs>/<name>`) via its absolute
/// parent, so it works regardless of the current working directory. Best-effort.
fn removeCaseDir(allocator: std.mem.Allocator, out_dir_abs: []const u8, name: []const u8) void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    var parent = std.Io.Dir.openDirAbsolute(io, out_dir_abs, .{}) catch return;
    defer parent.close(io);
    parent.deleteTree(io, name) catch {};
}

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
    /// The recorded first-draft outcome, locked in after recording. The offline
    /// ratchet asserts replay reproduces exactly this, so a case the agent
    /// currently fails is a valid, pinned corpus entry (it feeds the gap
    /// histogram) - not a broken test.
    expect_first_draft_pass: bool = true,
};

// The corpus spans common tasks the agent handles cleanly and harder ones that
// probe known gap areas (user-input egress, websocket events, durable
// workflows). Each elicits realistic multi-roundtrip behaviour (explore then
// edit) and records as step_0/step_1/...
const record_corpus = [_]RecordCase{
    .{
        .name = "health",
        .prompt = "Create a handler in handler.ts that responds to GET /health with " ++
            "Response.json({ ok: true }). Keep it minimal and deterministic.",
        .expect_first_draft_pass = true,
    },
    .{
        .name = "validate-body",
        .prompt = "Create a handler in handler.ts that decodes the JSON request body with " ++
            "zigttp:validate against a schema named \"item\" requiring a string field \"name\", " ++
            "returns the validated data on success, and returns a 400 with the errors on failure.",
        .expect_first_draft_pass = true,
    },
    .{
        .name = "jwt-auth",
        .prompt = "Create a handler in handler.ts that requires a bearer JWT using zigttp:auth " ++
            "with the secret from env JWT_SECRET, returns 401 when the token is missing or invalid, " ++
            "and otherwise returns the verified claims as JSON. Never use a fallback secret.",
        // Was ZTS401 (credential in response); closed by the strict-mode traps
        // teaching (return only non-sensitive fields).
        .expect_first_draft_pass = true,
    },
    .{
        .name = "weather-egress",
        .prompt = "Create a handler in handler.ts that reads a `city` query parameter and " ++
            "fetches the current weather for that city from https://api.open-meteo.com/v1/forecast " ++
            "using zigttp:fetch, returning the JSON response.",
        // Was ZTS602 (never converged); closed by the literal-URL + init-query
        // egress teaching.
        .expect_first_draft_pass = true,
    },
    .{
        .name = "websocket-echo",
        .prompt = "Create a WebSocket echo handler in handler.ts using zigttp:websocket that " ++
            "echoes every received message back to the sending client.",
        .expect_first_draft_pass = true,
    },
    .{
        .name = "durable-order",
        .prompt = "Create a durable handler in handler.ts using zigttp:durable that runs a " ++
            "two-step order workflow: a `reserve` step then a `charge` step, via run() and step().",
        // Was ZTS042/narrowing death-spiral (never converged); closed by the
        // "use untyped values directly, never narrow with as/guards" teaching.
        .expect_first_draft_pass = true,
    },
    .{
        .name = "workflow-queued-call",
        .prompt = "Create a durable workflow handler in handler.ts using zigttp:durable and " ++
            "zigttp:workflow. It should read the Idempotency-Key header, enter run(key), " ++
            "and dispatch a greet child handler with workflow.call at durable depth 0.",
        .expect_first_draft_pass = true,
    },
    .{
        .name = "workflow-nested-dispatch-avoidance",
        .prompt = "Create a durable order workflow in handler.ts. Reserve inventory with a " ++
            "durable step, then dispatch a notify child handler with workflow.call after the " ++
            "step completes. Keep the child dispatch outside the step callback.",
        .expect_first_draft_pass = true,
    },
    .{
        .name = "workflow-saga-compensation",
        .prompt = "Create a handler in handler.ts using zigttp:workflow saga() for reserve, " ++
            "charge, and ship steps. Include compensate functions for every non-last static " ++
            "saga step so the saga compensation proof can pass.",
        .expect_first_draft_pass = true,
    },
    .{
        .name = "workflow-wait-signal",
        .prompt = "Create a durable approval workflow in handler.ts using waitSignal and " ++
            "signal. The /wait path should park a run using the Idempotency-Key header, and " ++
            "the /signal path should resume the same key with an approved payload.",
        .expect_first_draft_pass = true,
    },
    .{
        .name = "sql-users",
        .prompt = "Create a handler in handler.ts that returns all users (id and name) from " ++
            "the sqlite database using zigttp:sql. The users table has columns id (integer) " ++
            "and name (text).",
        // Seeds the project SQL schema the veto discovers via zigttp.json's
        // `sqlite` key (resolved relative to the workspace).
        .seed_files = &.{
            .{ .path = "zigttp.json", .bytes = "{\n  \"sqlite\": \"schema.sql\"\n}\n" },
            .{ .path = "schema.sql", .bytes = "CREATE TABLE users (\n  id INTEGER PRIMARY KEY,\n  name TEXT NOT NULL\n);\n" },
        },
        // Recorded with the best model (Sonnet): writes correct SQL, self-checks
        // cleanly, and first-draft-passes. Previously it failed because the
        // property analysis reported read_only as PROVEN for a SELECT and the
        // agent declared it (then ZTS501 rejected it); the classifier now gates
        // declarable read_only on write-effect imports, so the agent is no
        // longer told to declare a property the import forbids.
        .expect_first_draft_pass = true,
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
    // The codegen corpus measures the QUALITY users actually get, and users run
    // the best models - so the corpus defaults to a strong model regardless of
    // the product's default_model (which may be a cheaper model). ZIGTTP_CODEGEN
    // _MODEL overrides it (e.g. Haiku) for cheap harness testing. Env strings
    // live for the process, so the borrowed slice is safe for the session.
    const corpus_model = envValue("ZIGTTP_CODEGEN_MODEL") orelse "claude-sonnet-4-6";
    var session = try agent.initFromEnvWithSessionConfig(allocator, &registry, .{
        .no_session = true,
        .no_context_files = true,
        .model = corpus_model,
    });
    defer session.deinit(allocator);
    if (session.authKind() != .anthropic_api_key) return error.SkipZigTest;
    // ZIGTTP_CODEGEN_ONLY=<name> records just one case, leaving the others'
    // committed cassettes untouched.
    const only_case = envValue("ZIGTTP_CODEGEN_ONLY");

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
        if (only_case) |only| {
            if (!std.mem.eql(u8, only, rc.name)) continue;
        }
        total += 1;

        var tmp = try IsolatedTmp.init(allocator, "codegen-record");
        defer tmp.cleanup(allocator);
        for (rc.seed_files) |sf| try tmp.writeFile(allocator, sf.path, sf.bytes);

        // Clear any prior cassette for this case so a shorter new recording (or
        // a transient mid-turn failure) cannot leave stale trailing steps that
        // would corrupt replay.
        removeCaseDir(allocator, out_dir, rc.name);

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
            // A transient live error (e.g. a network ReadFailed) on one case
            // must not abort the whole corpus run; skip and keep recording the
            // rest. Remove the partial cassette so replay never reads a
            // truncated step sequence.
            std.debug.print("[codegen-record] {s}: turn failed: {s} (skipped)\n", .{ rc.name, @errorName(err) });
            removeCaseDir(allocator, out_dir, rc.name);
            continue;
        };
        if (result.first_draft_veto_pass) first_draft_passes += 1;
        if (result.applied_edit) greens += 1;
        const fail_code = codegen.firstZtsCode(&tr) orelse "-";
        std.debug.print(
            "[codegen-record] {s}: first_draft_pass={} applied={} compiler_authored={} roundtrips={d} retries={d} tools={d} steps={d} fail={s}\n",
            .{
                rc.name,
                result.first_draft_veto_pass,
                result.applied_edit,
                result.compiler_authored_apply,
                result.roundtrips,
                result.veto_retry_count,
                result.tool_call_count,
                session.backend.anthropic.record_step,
                fail_code,
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
        if (steps.len == 0) {
            std.debug.print("[codegen-replay] {s}: missing committed cassette in {s}\n", .{ rc.name, dir_abs });
            return error.MissingCodegenCassette;
        }
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
        // Ratchet: replay must reproduce the recorded first-draft outcome
        // exactly. A regression flips a recorded pass to fail (or vice versa).
        if (result.first_draft_veto_pass != rc.expect_first_draft_pass) {
            std.debug.print(
                "[codegen-replay] {s}: expected first_draft_pass={} got {} (code {s})\n",
                .{ rc.name, rc.expect_first_draft_pass, result.first_draft_veto_pass, codegen.firstZtsCode(&tr) orelse "-" },
            );
            return error.CassetteRatchetMismatch;
        }
        // Gap histogram: the first rule each non-passing case tripped, ranking
        // which teaching gap to close next.
        if (!rc.expect_first_draft_pass) {
            std.debug.print("[codegen-gap] {s}: {s} (green={})\n", .{
                rc.name,
                codegen.firstZtsCode(&tr) orelse "?",
                result.applied_edit,
            });
        }
        passes += 1;
    }

    try testing.expect(with_cassettes > 0);
    try testing.expectEqual(with_cassettes, passes);
}
