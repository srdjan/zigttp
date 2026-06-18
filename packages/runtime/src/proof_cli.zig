//! `zigttp proof` command group — the Proof Flight Recorder's replay surface.
//!
//! A capsule (see `capsule.zig`) bundles a handler's recorded request traces
//! with the manifest that pins the handler/contract/policy hashes. `proof
//! replay <capsule>` loads that capsule, replays every recorded request
//! against the *current* handler source, and reports whether the responses
//! still match — turning "did this edit change behavior on a request I
//! actually exercised?" into a one-command regression gate.
//!
//! Replay reuses `replay_runner.replayOne` (the same engine `--replay` and
//! the witness harness use): each recorded trace carries its I/O results, so
//! the handler runs deterministically against replay stubs with no live
//! effects. A capsule whose `schemaVersion` or `policyHash` no longer matches
//! the running binary fails closed — the recorded behavior was proven under a
//! different rule set, so silently replaying it would be misleading.
//!
//! Distinct from `proofs_cli.zig` (the `zigttp proofs` ledger viewer): that
//! renders `.zigttp/proofs.jsonl` receipts; this replays capsule traces.

const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const capsule = @import("capsule.zig");
const replay_runner = @import("replay_runner.zig");
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;

const trace = zigts.trace;
const file_io = zigts.file_io;
const rule_registry = zigts.rule_registry;
const precompile = zigts_cli.precompile;
const handler_contract = zigts.handler_contract;
const Sha256 = std.crypto.hash.sha2.Sha256;

const max_handler_bytes = 4 * 1024 * 1024;
const max_trace_bytes = 16 * 1024 * 1024;

pub const Error = error{
    UsageError,
    CapsuleNotFound,
    SchemaVersionMismatch,
    PolicyMismatch,
    HandlerNotFound,
    ReplayRegression,
    EmptyCapsule,
};

/// True for errors whose message the command already explained on stderr, so
/// the dispatcher exits 1 without bubbling a raw Zig trace. Mirrors
/// `proofs_cli.isExpectedUserError`.
pub fn isExpectedUserError(err: anyerror) bool {
    return switch (err) {
        Error.UsageError,
        Error.CapsuleNotFound,
        Error.SchemaVersionMismatch,
        Error.PolicyMismatch,
        Error.HandlerNotFound,
        Error.ReplayRegression,
        Error.EmptyCapsule,
        => true,
        else => false,
    };
}

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len == 0) {
        printHelp();
        return Error.UsageError;
    }
    const sub = argv[0];
    if (std.mem.eql(u8, sub, "help") or std.mem.eql(u8, sub, "--help")) {
        printHelp();
        return;
    }
    if (std.mem.eql(u8, sub, "replay")) {
        return runReplay(allocator, argv[1..]);
    }
    std.debug.print("zigttp proof: unknown subcommand '{s}'\n\n", .{sub});
    printHelp();
    return Error.UsageError;
}

/// Outcome of replaying one capsule, decoupled from CLI output so tests can
/// assert on it directly.
pub const ReplayReport = struct {
    total: u32 = 0,
    matched: u32 = 0,
    regressed: u32 = 0,

    pub fn clean(self: ReplayReport) bool {
        // A capsule that reproduced zero requests verifies nothing; treating
        // `regressed == 0` as clean would let a breaking edit pass the replay
        // gate with exit 0. Fail closed unless at least one request actually ran.
        return self.total > 0 and self.regressed == 0;
    }
};

fn runReplay(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var allow_version_mismatch = false;
    var name: ?[]const u8 = null;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--allow-version-mismatch")) {
            allow_version_mismatch = true;
            continue;
        }
        if (std.mem.startsWith(u8, arg, "-")) {
            std.debug.print("zigttp proof replay: unknown flag '{s}'\n", .{arg});
            return Error.UsageError;
        }
        if (name == null) {
            name = arg;
        } else {
            std.debug.print("zigttp proof replay: expected a single capsule name\n", .{});
            return Error.UsageError;
        }
    }
    const capsule_name = name orelse {
        std.debug.print("Usage: zigttp proof replay <capsule> [--allow-version-mismatch]\n", .{});
        return Error.UsageError;
    };

    const report = try replayCapsule(allocator, capsule_name, allow_version_mismatch);

    if (report.total == 0) {
        // A capsule that captured no requests verifies nothing. Fail closed
        // rather than printing a vacuous "0/0 clean" pass, which would let a
        // breaking edit slip through the replay gate with exit 0.
        std.debug.print(
            "capsule '{s}': recorded no requests; nothing to verify (re-record with traffic).\n",
            .{capsule_name},
        );
        return Error.EmptyCapsule;
    }

    std.debug.print(
        "capsule '{s}': {d}/{d} requests reproduced\n",
        .{ capsule_name, report.matched, report.total },
    );
    if (!report.clean()) {
        std.debug.print(
            "  {d} regression(s) — the current handler no longer matches the recorded behavior.\n",
            .{report.regressed},
        );
        return Error.ReplayRegression;
    }
    std.debug.print("  capsule replay clean.\n", .{});
}

/// Replay every recorded request in `<capsule_name>` against the current
/// handler. Fails closed on a schema or policy mismatch (unless
/// `allow_version_mismatch`). Returns the tally; the caller decides exit code.
pub fn replayCapsule(
    allocator: std.mem.Allocator,
    capsule_name: []const u8,
    allow_version_mismatch: bool,
) !ReplayReport {
    const dir = try capsule.capsuleDirAlloc(allocator, capsule_name);
    defer allocator.free(dir);

    const manifest_path = try capsule.manifestPathAlloc(allocator, capsule_name);
    defer allocator.free(manifest_path);

    const manifest_json = file_io.readFile(allocator, manifest_path, max_trace_bytes) catch {
        std.debug.print("zigttp proof replay: no capsule '{s}' at {s}\n", .{ capsule_name, manifest_path });
        return Error.CapsuleNotFound;
    };
    defer allocator.free(manifest_json);

    var loaded = capsule.parse(allocator, manifest_json, .{ .allow_version_mismatch = allow_version_mismatch }) catch |err| {
        if (err == error.SchemaVersionMismatch) {
            std.debug.print(
                "zigttp proof replay: capsule '{s}' was recorded under capsule schema v?, this binary speaks v{d}.\n" ++
                    "The recorded behavior was proven under a different format; pass --allow-version-mismatch to replay anyway.\n",
                .{ capsule_name, capsule.schema_version },
            );
            return Error.SchemaVersionMismatch;
        }
        std.debug.print("zigttp proof replay: capsule '{s}' manifest is malformed: {}\n", .{ capsule_name, err });
        return Error.CapsuleNotFound;
    };
    defer loaded.deinit();
    const manifest = loaded.manifest;

    // Fail closed on policy drift: the recorded behavior was proven under the
    // rule set named by `policyHash`; a different rule set may classify the
    // same handler differently, so replaying silently would mislead.
    const current_policy = rule_registry.policyHash();
    if (!allow_version_mismatch and !std.mem.eql(u8, manifest.policy_hash, &current_policy)) {
        std.debug.print(
            "zigttp proof replay: capsule '{s}' was recorded under policy {s}, this binary is {s}.\n" ++
                "Pass --allow-version-mismatch to replay against the current rule set anyway.\n",
            .{ capsule_name, manifest.policy_hash, current_policy[0..] },
        );
        return Error.PolicyMismatch;
    }

    const handler_source = file_io.readFile(allocator, manifest.handler_path, max_handler_bytes) catch {
        std.debug.print(
            "zigttp proof replay: capsule '{s}' references handler '{s}', which is not readable from here.\n",
            .{ capsule_name, manifest.handler_path },
        );
        return Error.HandlerNotFound;
    };
    defer allocator.free(handler_source);

    return replayTraceFiles(allocator, dir, manifest.trace_files, handler_source, manifest.handler_path);
}

/// Replay every group in every trace file against `handler_source`. Split out
/// so tests can drive it with an explicit handler + trace set, no manifest.
pub fn replayTraceFiles(
    allocator: std.mem.Allocator,
    capsule_dir: []const u8,
    trace_files: []const []const u8,
    handler_source: []const u8,
    handler_filename: []const u8,
) !ReplayReport {
    var report: ReplayReport = .{};

    // `replay_file_path` non-null is the sentinel that makes Runtime install
    // replay stubs for virtual modules instead of real implementations.
    const config = RuntimeConfig{
        .replay_file_path = "proof-capsule",
        .enforce_arena_escape = false,
    };

    for (trace_files) |rel| {
        const path = try std.fs.path.join(allocator, &.{ capsule_dir, rel });
        defer allocator.free(path);

        // The trace source must outlive the parsed groups: `parseTraceFile`
        // returns zero-copy slices that point into `source`, so freeing it
        // early would dangle every request/response field. Keep it alive for
        // the whole replay loop, then free.
        const source = file_io.readFile(allocator, path, max_trace_bytes) catch |err| {
            std.debug.print("zigttp proof replay: cannot read trace '{s}': {}\n", .{ path, err });
            return Error.CapsuleNotFound;
        };
        defer allocator.free(source);

        const groups = try trace.parseTraceFile(allocator, source);
        defer {
            for (groups) |g| allocator.free(g.io_calls);
            allocator.free(groups);
        }

        for (groups) |group| {
            report.total += 1;
            var result = replay_runner.replayOne(
                allocator,
                config,
                handler_source,
                handler_filename,
                &group,
            ) catch {
                report.regressed += 1;
                continue;
            };
            defer result.deinit(allocator);
            if (result.match) {
                report.matched += 1;
            } else {
                report.regressed += 1;
            }
        }
    }

    return report;
}

// -- Expert-loop probe ------------------------------------------------------

/// Capsule-replay probe for the expert loop (`pi_app.capsule_probe` seam).
/// Replays the workspace's `default` capsule against just-applied content and
/// returns the tally. Silent zero-tally when there is no capsule (the common
/// case) so the expert loop pays nothing until a developer records one.
///
/// Replays against `after_bytes` directly (not the on-disk handler), so it
/// reflects the edit the agent just applied. Best-effort: any failure yields a
/// zero tally rather than an error, matching the seam's non-gating contract.
pub fn replayActiveCapsule(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    handler_path: []const u8,
    after_bytes: []const u8,
) !ReplayReport {
    const capsule_name = "default";
    const manifest_rel = try capsule.manifestPathAlloc(allocator, capsule_name);
    defer allocator.free(manifest_rel);
    const manifest_path = try std.fs.path.join(allocator, &.{ workspace_root, manifest_rel });
    defer allocator.free(manifest_path);

    const manifest_json = file_io.readFile(allocator, manifest_path, max_trace_bytes) catch {
        // No capsule recorded for this workspace — nothing to check.
        return .{};
    };
    defer allocator.free(manifest_json);

    var loaded = capsule.parse(allocator, manifest_json, .{}) catch return .{};
    defer loaded.deinit();

    const capsule_dir = try std.fs.path.join(allocator, &.{ workspace_root, capsule.capsules_root, capsule_name });
    defer allocator.free(capsule_dir);

    const report = replayTraceFiles(
        allocator,
        capsule_dir,
        loaded.manifest.trace_files,
        after_bytes,
        handler_path,
    ) catch return .{};

    return report;
}

// -- Recording --------------------------------------------------------------

/// Relative path (within the capsule dir) of the trace file `dev
/// --record-proof` records a session into. A single rolling file keeps the
/// capsule simple; re-recording overwrites it via the live `--trace` writer.
pub const session_trace_rel = "traces/session.jsonl";

/// Absolute path the recorder should pass to the runtime's `--trace` flag so
/// live request capture lands inside the capsule. Creates the capsule and its
/// `traces/` subdir. Caller frees.
pub fn sessionTracePathAlloc(allocator: std.mem.Allocator, capsule_name: []const u8) ![]u8 {
    try capsule.ensureCapsuleDir(allocator, capsule_name);
    const traces_dir = try std.fs.path.join(allocator, &.{ capsule.capsules_root, capsule_name, "traces" });
    defer allocator.free(traces_dir);
    try capsule.mkdirIfAbsent(allocator, traces_dir);
    return std.fs.path.join(allocator, &.{ capsule.capsules_root, capsule_name, session_trace_rel });
}

/// Compile `handler_path`, derive the proof facts, and write the capsule
/// manifest. Called by `dev --record-proof` once the recording session ends,
/// so the capsule's pinned hashes describe the handler the traces ran
/// against. Best-effort: a compile failure leaves the recorded traces in
/// place without a manifest and is reported, never fatal to `dev`.
pub fn writeManifest(
    allocator: std.mem.Allocator,
    capsule_name: []const u8,
    handler_path: []const u8,
    system_path: ?[]const u8,
) !void {
    var check = try precompile.runCheckOnly(allocator, handler_path, null, false, system_path);
    defer check.deinit(allocator);

    const contract = if (check.contract) |*c| c else return Error.HandlerNotFound;

    const handler_src = try file_io.readFile(allocator, handler_path, max_handler_bytes);
    defer allocator.free(handler_src);

    var handler_hash: [64]u8 = undefined;
    capsule.hashHex(handler_src, &handler_hash);

    // Contract hash over the serialized contract JSON.
    var contract_json: std.ArrayList(u8) = .empty;
    defer contract_json.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &contract_json);
    try handler_contract.writeContractJson(contract, &aw.writer);
    contract_json = aw.toArrayList();
    var contract_hash: [64]u8 = undefined;
    capsule.hashHex(contract_json.items, &contract_hash);

    const policy = rule_registry.policyHash();

    // Routes: prefer the api routes (carry method), fall back to plain routes.
    var routes: std.ArrayList(capsule.Route) = .empty;
    defer routes.deinit(allocator);
    for (contract.api.routes.items) |r| {
        try routes.append(allocator, .{ .method = r.method, .path = r.path });
    }
    if (routes.items.len == 0) {
        for (contract.routes.items) |r| {
            try routes.append(allocator, .{ .method = "GET", .path = r.pattern });
        }
    }

    // Proven specs from the classified properties.
    var spec_buf: [zigts.handler_contract.HandlerProperties.max_proven_specs]?[]const u8 = undefined;
    var proven: std.ArrayList([]const u8) = .empty;
    defer proven.deinit(allocator);
    if (contract.properties) |props| {
        const n = props.provenSpecNames(&spec_buf);
        for (spec_buf[0..n]) |name_opt| {
            if (name_opt) |name| try proven.append(allocator, name);
        }
    }

    // Declared specs and imported modules (effects) are owned by the contract.
    var declared: std.ArrayList([]const u8) = .empty;
    defer declared.deinit(allocator);
    for (contract.declared_specs.items) |s| try declared.append(allocator, s);

    var effects: std.ArrayList([]const u8) = .empty;
    defer effects.deinit(allocator);
    for (contract.modules.items) |m| try effects.append(allocator, m);

    const manifest: capsule.Manifest = .{
        .name = capsule_name,
        .handler_path = handler_path,
        .handler_hash = &handler_hash,
        .contract_hash = &contract_hash,
        .zigttp_version = zigts.version.string,
        .policy_hash = &policy,
        .proven_specs = proven.items,
        .declared_specs = declared.items,
        .routes = routes.items,
        .effects = effects.items,
        .trace_files = &.{session_trace_rel},
    };

    const json = try manifest.toJsonAlloc(allocator);
    defer allocator.free(json);
    const manifest_path = try capsule.manifestPathAlloc(allocator, capsule_name);
    defer allocator.free(manifest_path);
    try file_io.writeFile(allocator, manifest_path, json);
}

fn printHelp() void {
    const help =
        \\zigttp proof — replay a recorded proof capsule against the current handler
        \\
        \\Usage:
        \\  zigttp proof replay <capsule> [--allow-version-mismatch]
        \\      Replay every recorded request in the named capsule
        \\      (.zigttp/capsules/<capsule>/) against the current handler and
        \\      report whether the responses still match. Exits 1 on any
        \\      regression. Fails closed when the capsule's schema or policy
        \\      hash no longer matches this binary; --allow-version-mismatch
        \\      overrides that gate.
        \\
        \\A capsule is recorded by `zigttp dev --record-proof` or
        \\authored from recorded `--trace` output. The capsule format lives in
        \\packages/runtime/src/capsule.zig.
        \\
    ;
    std.debug.print("{s}", .{help});
}

// -- Tests ------------------------------------------------------------------

const testing = std.testing;

// A trace recorded from `handler(req) -> Response.json({ ok: true })`: one
// GET / request, no I/O, a 200 with that body.
const sample_trace =
    \\{"type":"request","method":"GET","url":"/","headers":{},"body":null}
    \\{"type":"response","status":200,"headers":{},"body":"{\"ok\":true}"}
    \\{"type":"meta","duration_us":1,"handler":"handler.ts","pool_slot":0,"io_count":0}
;

const matching_handler =
    \\function handler(req) {
    \\  return Response.json({ ok: true });
    \\}
;

const regressed_handler =
    \\function handler(req) {
    \\  return Response.json({ ok: false });
    \\}
;

fn writeCapsuleWithTrace(tmp: *std.testing.TmpDir, name: []const u8) ![]u8 {
    // Returns the absolute capsule dir path (caller frees). Writes the
    // manifest + one trace file under a tmp cwd.
    try capsule.ensureCapsuleDir(testing.allocator, name);
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    _ = tmp;

    const trace_rel = "traces/001.jsonl";
    const trace_path = try std.fs.path.join(testing.allocator, &.{ capsule.capsules_root, name, trace_rel });
    defer testing.allocator.free(trace_path);
    // Ensure traces/ subdir exists.
    const traces_dir = try std.fs.path.join(testing.allocator, &.{ capsule.capsules_root, name, "traces" });
    defer testing.allocator.free(traces_dir);
    try capsule.ensureCapsuleDir(testing.allocator, name); // root
    {
        const dz = try testing.allocator.dupeZ(u8, traces_dir);
        defer testing.allocator.free(dz);
        switch (std.posix.errno(std.posix.system.mkdir(dz, 0o755))) {
            .SUCCESS, .EXIST => {},
            else => return error.MakeDirFailed,
        }
    }
    try file_io.writeFile(testing.allocator, trace_path, sample_trace);

    _ = &path_buf;
    return std.fs.path.join(testing.allocator, &.{ capsule.capsules_root, name });
}

test "replayTraceFiles reports a clean match for an unchanged handler" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const dir = try writeCapsuleWithTrace(&tmp, "checkout");
    defer testing.allocator.free(dir);

    // Replay through an arena over the leak-checked allocator — the same
    // pattern the replay-runner's own roundtrip test uses; the runtime's
    // teardown frees through this allocator, so a page-protected allocator
    // would fault on deinit.
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const report = try replayTraceFiles(
        arena.allocator(),
        dir,
        &.{"traces/001.jsonl"},
        matching_handler,
        "handler.ts",
    );
    try testing.expectEqual(@as(u32, 1), report.total);
    try testing.expectEqual(@as(u32, 1), report.matched);
    try testing.expectEqual(@as(u32, 0), report.regressed);
    try testing.expect(report.clean());
}

test "replayTraceFiles flags a regression when the response changed" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const dir = try writeCapsuleWithTrace(&tmp, "checkout");
    defer testing.allocator.free(dir);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const report = try replayTraceFiles(
        arena.allocator(),
        dir,
        &.{"traces/001.jsonl"},
        regressed_handler,
        "handler.ts",
    );
    try testing.expectEqual(@as(u32, 1), report.total);
    try testing.expectEqual(@as(u32, 0), report.matched);
    try testing.expectEqual(@as(u32, 1), report.regressed);
    try testing.expect(!report.clean());
}

test "ReplayReport with zero recorded requests is not clean" {
    // Regression: a capsule that captured no traffic (empty trace file) yields
    // total=0, regressed=0. clean() must NOT treat that as a pass, or `zigttp
    // proof replay` would print "0/0 reproduced ... clean" and exit 0 after a
    // breaking edit, defeating the fail-closed replay gate.
    const empty = ReplayReport{ .total = 0, .matched = 0, .regressed = 0 };
    try testing.expect(!empty.clean());
    // A real reproduction with no regressions is still clean.
    const ok = ReplayReport{ .total = 3, .matched = 3, .regressed = 0 };
    try testing.expect(ok.clean());
}

test "replayCapsule fails closed on a policy mismatch" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const dir = try writeCapsuleWithTrace(&tmp, "checkout");
    defer testing.allocator.free(dir);
    try file_io.writeFile(testing.allocator, "handler.ts", matching_handler);

    // Manifest with a deliberately wrong policy hash.
    const manifest: capsule.Manifest = .{
        .name = "checkout",
        .handler_path = "handler.ts",
        .handler_hash = "a" ** 64,
        .contract_hash = "b" ** 64,
        .zigttp_version = "test",
        .policy_hash = "0" ** 64,
        .trace_files = &.{"traces/001.jsonl"},
    };
    const json = try manifest.toJsonAlloc(testing.allocator);
    defer testing.allocator.free(json);
    const manifest_path = try capsule.manifestPathAlloc(testing.allocator, "checkout");
    defer testing.allocator.free(manifest_path);
    try file_io.writeFile(testing.allocator, manifest_path, json);

    // The policy gate trips before any replay, so the allocator never reaches
    // the runtime here; testing.allocator is fine.
    try testing.expectError(Error.PolicyMismatch, replayCapsule(testing.allocator, "checkout", false));

    // --allow-version-mismatch overrides the gate and replays clean. The
    // override reaches the runtime, so use an arena (see replayTraceFiles).
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const report = try replayCapsule(arena.allocator(), "checkout", true);
    try testing.expectEqual(@as(u32, 1), report.matched);
    try testing.expect(report.clean());
}

test "replayCapsule reports a missing capsule" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try testing.expectError(Error.CapsuleNotFound, replayCapsule(testing.allocator, "nope", false));
}
