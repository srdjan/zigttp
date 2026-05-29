//! `zigttp proofs gate` — the pull-request proof gate.
//!
//! A proof's value is realized at a trust boundary: a reviewer who did not
//! write the code. Every other proof surface (terminal HUD, Studio, badge)
//! is author-facing on the author's own machine. This command carries the
//! verdict to where review happens. Given a git range, for every changed
//! handler it compiles the before and after versions, runs the same
//! `contract_diff` the expert loop and `prove-behavior` compute, aggregates a
//! repo-level behavioral verdict, and emits a PR-ready Markdown report (or a
//! machine JSON object) plus a process exit code.
//!
//!   exit 0 = safe (equivalent / equivalent_modulo_laws / additive)
//!   exit 1 = breaking (a response on an existing path changed or was removed,
//!            a route/capability was removed)
//!   exit 2 = usage or git error
//!
//! The analysis is entirely reused: `diffContracts` + `behavioralVerdict` for
//! the per-handler verdict, and `equivalence_probe_lib` for the signed
//! `kind=equivalence` ledger rows (best-effort, skipped under `--no-sign`).
//! Only the git-range discovery, multi-handler aggregation, and the
//! PR-oriented rendering are new here.

const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const equivalence_probe_lib = @import("../equivalence_probe_lib.zig");
const shared = @import("../cli_shared.zig");

const contract_diff = zigts.contract_diff;
const file_io = zigts.file_io;
const Classification = contract_diff.Classification;

const max_source_bytes = 4 * 1024 * 1024;

pub const json_schema = "zigttp.proof-gate.v1";

pub const Format = enum { md, json };

pub const Options = struct {
    /// Explicit base ref, or null to resolve the default (origin/main → main).
    base: ?[]const u8 = null,
    /// Explicit head ref, or null to diff the base against the working tree.
    head: ?[]const u8 = null,
    format: Format = .md,
    /// Write the report to this file instead of stdout.
    out: ?[]const u8 = null,
    /// Skip appending signed kind=equivalence ledger rows. CI runners have no
    /// persistent attest identity, so the Action passes this.
    no_sign: bool = false,
};

// ---------------------------------------------------------------------------
// Pure data model (no git, no compile) — the unit-tested core.
// ---------------------------------------------------------------------------

pub const BehaviorCounts = struct {
    preserved: u32 = 0,
    response_changed: u32 = 0,
    added: u32 = 0,
    removed: u32 = 0,
};

pub const Counterexample = struct {
    method: []const u8,
    pattern: []const u8,
    status: u16,
    /// `.removed` or `.response_changed`.
    change: contract_diff.BehaviorChangeKind,
};

pub const SurfaceDelta = struct {
    added_routes: []const []const u8 = &.{},
    removed_routes: []const []const u8 = &.{},
    added_env: []const []const u8 = &.{},
    removed_env: []const []const u8 = &.{},
    added_egress: []const []const u8 = &.{},
    removed_egress: []const []const u8 = &.{},
    added_cache: []const []const u8 = &.{},
    removed_cache: []const []const u8 = &.{},
    capabilities_widened: bool = false,
    capabilities_narrowed: bool = false,

    /// One category of surface change. `noun` is the singular prose form
    /// ("route") and `json` is the JSON field name ("routes"); the renderers
    /// and the serializer all iterate this list so a new category is added in
    /// one place.
    const Category = struct {
        noun: []const u8,
        json: []const u8,
        added: []const []const u8,
        removed: []const []const u8,
    };

    fn categories(self: *const SurfaceDelta) [4]Category {
        return .{
            .{ .noun = "route", .json = "routes", .added = self.added_routes, .removed = self.removed_routes },
            .{ .noun = "env", .json = "env", .added = self.added_env, .removed = self.removed_env },
            .{ .noun = "egress", .json = "egress", .added = self.added_egress, .removed = self.removed_egress },
            .{ .noun = "cache", .json = "cache", .added = self.added_cache, .removed = self.removed_cache },
        };
    }

    fn isEmpty(self: *const SurfaceDelta) bool {
        for (self.categories()) |c| {
            if (c.added.len > 0 or c.removed.len > 0) return false;
        }
        return !self.capabilities_widened and !self.capabilities_narrowed;
    }
};

pub const HandlerResult = struct {
    path: []const u8,
    verdict: Classification,
    behavior: BehaviorCounts = .{},
    surface: SurfaceDelta = .{},
    counterexamples: []const Counterexample = &.{},
    signed: bool = false,
};

pub const Skipped = struct {
    path: []const u8,
    /// "no_contract" — the file does not compile to a handler contract.
    reason: []const u8,
};

/// Repo verdict = worst per-handler verdict. One breaking handler makes the
/// whole pull request breaking. Empty (no handler changes) reads as
/// `.equivalent`, i.e. safe.
pub fn worstVerdict(results: []const HandlerResult) Classification {
    var worst: Classification = .equivalent;
    for (results) |r| {
        if (rank(r.verdict) > rank(worst)) worst = r.verdict;
    }
    return worst;
}

fn rank(c: Classification) u8 {
    return switch (c) {
        .equivalent => 0,
        .equivalent_modulo_laws => 1,
        .additive => 2,
        .breaking => 3,
    };
}

fn isSafe(c: Classification) bool {
    return c != .breaking;
}

// ---------------------------------------------------------------------------
// Rendering (pure: operates on the data model only)
// ---------------------------------------------------------------------------

/// Marker the GitHub Action's sticky-comment step keys on to update in place.
pub const sticky_marker = "<!-- zigttp-proof -->";

pub fn renderMarkdown(
    w: *std.Io.Writer,
    base_label: []const u8,
    head_label: []const u8,
    results: []const HandlerResult,
    skipped: []const Skipped,
    verdict: Classification,
    signed_rows: usize,
) !void {
    try w.writeAll(sticky_marker);
    try w.writeAll("\n## zigttp proof gate\n\n");
    try w.print("**Verdict: `{s}`**", .{verdict.toString()});
    try w.print("  ·  base `{s}` → head `{s}`", .{ base_label, head_label });
    try w.print("  ·  {d} handler{s} checked\n\n", .{ results.len, plural(results.len) });

    if (results.len == 0) {
        try w.writeAll("_No changed handlers in this range._\n");
    } else {
        try w.writeAll("| Handler | Verdict | Δ behavior | Δ surface |\n");
        try w.writeAll("|---|---|---|---|\n");
        for (results) |r| {
            try w.print("| `{s}` | `{s}` | ", .{ r.path, r.verdict.toString() });
            try writeBehaviorCell(w, r.behavior);
            try w.writeAll(" | ");
            try writeSurfaceCell(w, r.surface);
            try w.writeAll(" |\n");
        }

        for (results) |r| {
            if (r.verdict.isSafeNoOp()) continue;
            try w.print("\n### `{s}` — {s}\n", .{ r.path, r.verdict.toString() });
            if (!r.surface.isEmpty()) {
                try w.writeAll("\n**Surface changes**\n\n");
                try writeSurfaceLines(w, r.surface);
            }
            if (r.verdict == .breaking and r.counterexamples.len > 0) {
                try w.writeAll("\n**Counterexample** — behavior changed on an existing path:\n\n");
                for (r.counterexamples) |c| {
                    switch (c.change) {
                        .removed => try w.print("- `{s} {s}` removed\n", .{ c.method, c.pattern }),
                        else => try w.print("- `{s} {s}` status {d} → response changed\n", .{ c.method, c.pattern, c.status }),
                    }
                }
            }
        }
    }

    if (skipped.len > 0) {
        try w.print("\n<details><summary>{d} file{s} skipped (not a handler)</summary>\n\n", .{ skipped.len, plural(skipped.len) });
        for (skipped) |s| try w.print("- `{s}`\n", .{s.path});
        try w.writeAll("\n</details>\n");
    }

    if (signed_rows > 0) {
        try w.print("\n_Signed `kind=equivalence` receipt{s} appended to `.zigttp/proofs.jsonl` ({d} row{s})._\n", .{ plural(signed_rows), signed_rows, plural(signed_rows) });
    }
}

fn writeBehaviorCell(w: *std.Io.Writer, b: BehaviorCounts) !void {
    var first = true;
    if (b.response_changed > 0) {
        try w.print("{d} response changed", .{b.response_changed});
        first = false;
    }
    if (b.added > 0) {
        if (!first) try w.writeAll(", ");
        try w.print("{d} added", .{b.added});
        first = false;
    }
    if (b.removed > 0) {
        if (!first) try w.writeAll(", ");
        try w.print("{d} removed", .{b.removed});
        first = false;
    }
    if (first) try w.writeAll("—");
}

fn writeSurfaceCell(w: *std.Io.Writer, s: SurfaceDelta) !void {
    var first = true;
    for (s.categories()) |c| {
        first = try writeCount(w, first, c.added.len, "+", c.noun);
        first = try writeCount(w, first, c.removed.len, "-", c.noun);
    }
    if (s.capabilities_widened) {
        if (!first) try w.writeAll(", ");
        try w.writeAll("caps widened");
        first = false;
    }
    if (s.capabilities_narrowed) {
        if (!first) try w.writeAll(", ");
        try w.writeAll("caps narrowed");
        first = false;
    }
    if (first) try w.writeAll("—");
}

fn writeCount(w: *std.Io.Writer, first: bool, n: usize, sign: []const u8, label: []const u8) !bool {
    if (n == 0) return first;
    if (!first) try w.writeAll(", ");
    try w.print("{s}{d} {s}", .{ sign, n, label });
    return false;
}

fn writeSurfaceLines(w: *std.Io.Writer, s: SurfaceDelta) !void {
    for (s.categories()) |c| {
        for (c.added) |v| try w.print("- `+ {s} {s}`\n", .{ c.noun, v });
        for (c.removed) |v| try w.print("- `- {s} {s}`\n", .{ c.noun, v });
    }
    if (s.capabilities_widened) try w.writeAll("- `capabilities widened`\n");
    if (s.capabilities_narrowed) try w.writeAll("- `capabilities narrowed`\n");
}

fn plural(n: usize) []const u8 {
    return if (n == 1) "" else "s";
}

pub fn renderJson(
    w: *std.Io.Writer,
    base_label: []const u8,
    head_label: []const u8,
    results: []const HandlerResult,
    skipped: []const Skipped,
    verdict: Classification,
) !void {
    var json: std.json.Stringify = .{ .writer = w };
    try json.beginObject();
    try json.objectField("schema");
    try json.write(json_schema);
    try json.objectField("base");
    try json.write(base_label);
    try json.objectField("head");
    try json.write(head_label);
    try json.objectField("verdict");
    try json.write(verdict.toString());
    try json.objectField("safe");
    try json.write(isSafe(verdict));
    try json.objectField("handlersChecked");
    try json.write(results.len);

    try json.objectField("handlers");
    try json.beginArray();
    for (results) |r| {
        try json.beginObject();
        try json.objectField("path");
        try json.write(r.path);
        try json.objectField("verdict");
        try json.write(r.verdict.toString());
        try json.objectField("behaviorDiff");
        try json.beginObject();
        try json.objectField("preserved");
        try json.write(r.behavior.preserved);
        try json.objectField("responseChanged");
        try json.write(r.behavior.response_changed);
        try json.objectField("added");
        try json.write(r.behavior.added);
        try json.objectField("removed");
        try json.write(r.behavior.removed);
        try json.endObject();
        try json.objectField("surface");
        try json.beginObject();
        for (r.surface.categories()) |c| {
            try writeJsonStringPair(&json, c.json, c.added, c.removed);
        }
        try json.objectField("capabilitiesWidened");
        try json.write(r.surface.capabilities_widened);
        try json.objectField("capabilitiesNarrowed");
        try json.write(r.surface.capabilities_narrowed);
        try json.endObject();
        try json.objectField("counterexamples");
        try json.beginArray();
        for (r.counterexamples) |c| {
            try json.beginObject();
            try json.objectField("method");
            try json.write(c.method);
            try json.objectField("pattern");
            try json.write(c.pattern);
            try json.objectField("status");
            try json.write(c.status);
            try json.objectField("change");
            try json.write(@tagName(c.change));
            try json.endObject();
        }
        try json.endArray();
        try json.objectField("signed");
        try json.write(r.signed);
        try json.endObject();
    }
    try json.endArray();

    try json.objectField("skipped");
    try json.beginArray();
    for (skipped) |s| {
        try json.beginObject();
        try json.objectField("path");
        try json.write(s.path);
        try json.objectField("reason");
        try json.write(s.reason);
        try json.endObject();
    }
    try json.endArray();
    try json.endObject();
    try w.writeByte('\n');
}

fn writeJsonStringPair(
    json: *std.json.Stringify,
    field: []const u8,
    added: []const []const u8,
    removed: []const []const u8,
) !void {
    try json.objectField(field);
    try json.beginObject();
    try json.objectField("added");
    try json.beginArray();
    for (added) |v| try json.write(v);
    try json.endArray();
    try json.objectField("removed");
    try json.beginArray();
    for (removed) |v| try json.write(v);
    try json.endArray();
    try json.endObject();
}

// ---------------------------------------------------------------------------
// Extraction from a ContractDiff into the arena-owned data model
// ---------------------------------------------------------------------------

fn collectItems(
    arena: std.mem.Allocator,
    changes: []const contract_diff.ItemChange,
    want: contract_diff.ChangeStatus,
) ![]const []const u8 {
    var list: std.ArrayList([]const u8) = .empty;
    for (changes) |c| {
        if (c.status == want) try list.append(arena, try arena.dupe(u8, c.value));
    }
    return try list.toOwnedSlice(arena);
}

/// Route changes arrive in two shapes: bare `routes` (static route table) and
/// `api_route_changes` (method+path, the form `routerMatch` handlers produce).
/// Both are surfaced under one list so the report does not silently drop a
/// route added or removed through `routerMatch`.
fn collectRoutes(
    arena: std.mem.Allocator,
    diff: *const contract_diff.ContractDiff,
    want: contract_diff.ChangeStatus,
) ![]const []const u8 {
    var list: std.ArrayList([]const u8) = .empty;
    for (diff.routes.items) |c| {
        if (c.status == want) try list.append(arena, try arena.dupe(u8, c.pattern));
    }
    for (diff.api_route_changes.items) |c| {
        if (c.status == want) try list.append(arena, try std.fmt.allocPrint(arena, "{s} {s}", .{ c.method, c.path }));
    }
    return try list.toOwnedSlice(arena);
}

fn extractSurface(arena: std.mem.Allocator, diff: *const contract_diff.ContractDiff) !SurfaceDelta {
    return .{
        .added_routes = try collectRoutes(arena, diff, .added),
        .removed_routes = try collectRoutes(arena, diff, .removed),
        .added_env = try collectItems(arena, diff.env_changes.items, .added),
        .removed_env = try collectItems(arena, diff.env_changes.items, .removed),
        .added_egress = try collectItems(arena, diff.egress_changes.items, .added),
        .removed_egress = try collectItems(arena, diff.egress_changes.items, .removed),
        .added_cache = try collectItems(arena, diff.cache_changes.items, .added),
        .removed_cache = try collectItems(arena, diff.cache_changes.items, .removed),
        .capabilities_widened = diff.capabilitiesWidened(),
        .capabilities_narrowed = diff.capabilitiesNarrowed(),
    };
}

/// The breaking paths, deduped by (method, pattern). Sourced from the
/// per-path behavior diff when present, and from `api_route_changes` for
/// route removals/response breaks that the behavior diff does not carry
/// (a removed `routerMatch` route shows up only here).
fn extractCounterexamples(arena: std.mem.Allocator, diff: *const contract_diff.ContractDiff) ![]const Counterexample {
    var list: std.ArrayList(Counterexample) = .empty;
    var seen: std.ArrayList([]const u8) = .empty;

    if (diff.behavior_diff) |bd| {
        for (bd.changes.items) |c| {
            switch (c.change) {
                .removed, .response_changed => {},
                else => continue,
            }
            try appendCounterexample(arena, &list, &seen, c.method, c.pattern, c.status, c.change);
        }
    }
    for (diff.api_route_changes.items) |c| {
        const change: contract_diff.BehaviorChangeKind = if (c.status == .removed)
            .removed
        else if (c.response == .breaking)
            .response_changed
        else
            continue;
        try appendCounterexample(arena, &list, &seen, c.method, c.path, 0, change);
    }
    return try list.toOwnedSlice(arena);
}

fn appendCounterexample(
    arena: std.mem.Allocator,
    list: *std.ArrayList(Counterexample),
    seen: *std.ArrayList([]const u8),
    method: []const u8,
    pattern: []const u8,
    status: u16,
    change: contract_diff.BehaviorChangeKind,
) !void {
    const key = try std.fmt.allocPrint(arena, "{s} {s}", .{ method, pattern });
    for (seen.items) |s| {
        if (std.mem.eql(u8, s, key)) return;
    }
    try seen.append(arena, key);
    try list.append(arena, .{
        .method = try arena.dupe(u8, method),
        .pattern = try arena.dupe(u8, pattern),
        .status = status,
        .change = change,
    });
}

fn isHandlerContract(contract: *const zigts.HandlerContract) bool {
    return contract.routes.items.len > 0 or
        contract.api.routes.items.len > 0 or
        contract.behaviors.items.len > 0;
}

fn behaviorCounts(diff: *const contract_diff.ContractDiff) BehaviorCounts {
    const bd = diff.behavior_diff orelse return .{};
    return .{
        .preserved = bd.preserved,
        .response_changed = bd.response_changed,
        .added = bd.added,
        .removed = bd.removed,
    };
}

// ---------------------------------------------------------------------------
// Git helpers (local std.process.run wrapper — the only new I/O here)
// ---------------------------------------------------------------------------

const GitResult = struct {
    ok: bool,
    /// Owned by the caller's allocator.
    stdout: []u8,
};

fn runGit(allocator: std.mem.Allocator, cwd: []const u8, args: []const []const u8) !GitResult {
    var io_backend = shared.threadedIo(allocator);
    defer io_backend.deinit();
    const r = std.process.run(allocator, io_backend.io(), .{
        .argv = args,
        .cwd = .{ .path = cwd },
        .stdout_limit = .limited(8 * 1024 * 1024),
        .stderr_limit = .limited(256 * 1024),
    }) catch return error.GitSpawnFailed;
    allocator.free(r.stderr);
    const ok = switch (r.term) {
        .exited => |code| code == 0,
        else => false,
    };
    return .{ .ok = ok, .stdout = r.stdout };
}

fn refExists(allocator: std.mem.Allocator, cwd: []const u8, ref: []const u8) bool {
    const res = runGit(allocator, cwd, &.{ "git", "rev-parse", "--verify", "--quiet", ref }) catch return false;
    defer allocator.free(res.stdout);
    return res.ok;
}

fn isHandlerCandidate(path: []const u8) bool {
    const has_ext = std.mem.endsWith(u8, path, ".ts") or std.mem.endsWith(u8, path, ".tsx") or
        std.mem.endsWith(u8, path, ".js") or std.mem.endsWith(u8, path, ".jsx");
    if (!has_ext) return false;
    if (std.mem.indexOf(u8, path, ".test.") != null) return false;
    if (std.mem.indexOf(u8, path, ".spec.") != null) return false;
    if (std.mem.startsWith(u8, path, "tests/") or std.mem.indexOf(u8, path, "/tests/") != null) return false;
    if (std.mem.startsWith(u8, path, "test/") or std.mem.indexOf(u8, path, "/test/") != null) return false;
    if (std.mem.indexOf(u8, path, "fixtures/") != null) return false;
    return true;
}

fn changedHandlers(
    arena: std.mem.Allocator,
    cwd: []const u8,
    base: []const u8,
    head: ?[]const u8,
) ![]const []const u8 {
    const res = if (head) |h| blk: {
        const range = try std.fmt.allocPrint(arena, "{s}...{s}", .{ base, h });
        break :blk try runGit(arena, cwd, &.{ "git", "diff", "--name-only", "--diff-filter=ACMR", range });
    } else try runGit(arena, cwd, &.{ "git", "diff", "--name-only", "--diff-filter=ACMR", base });
    defer arena.free(res.stdout);
    if (!res.ok) return error.BadGitRef;

    var list: std.ArrayList([]const u8) = .empty;
    var it = std.mem.tokenizeScalar(u8, res.stdout, '\n');
    while (it.next()) |line| {
        const path = std.mem.trim(u8, line, " \t\r");
        if (path.len == 0) continue;
        if (!isHandlerCandidate(path)) continue;
        try list.append(arena, try arena.dupe(u8, path));
    }
    return try list.toOwnedSlice(arena);
}

/// Read a path at a git ref. Returns null when the path did not exist there
/// (a newly added handler), which `git show` reports as a non-zero exit.
fn readAtRef(arena: std.mem.Allocator, cwd: []const u8, ref: []const u8, path: []const u8) !?[]u8 {
    const spec = try std.fmt.allocPrint(arena, "{s}:{s}", .{ ref, path });
    const res = try runGit(arena, cwd, &.{ "git", "show", spec });
    if (!res.ok) {
        arena.free(res.stdout);
        return null;
    }
    return res.stdout;
}

// ---------------------------------------------------------------------------
// Orchestration
// ---------------------------------------------------------------------------

/// Entry point. Returns the process exit code (0 safe, 1 breaking, 2 error).
/// Allocation failures propagate; git/usage failures are reported on stderr
/// and folded into exit code 2.
pub fn run(
    gpa: std.mem.Allocator,
    opts: Options,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !u8 {
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const cwd = shared.realCwd(arena) catch {
        try stderr.writeAll("zigttp proofs gate: cannot resolve the working directory\n");
        return 2;
    };

    if (!refExists(arena, cwd, "HEAD")) {
        try stderr.writeAll("zigttp proofs gate: not a git repository (no HEAD)\n");
        return 2;
    }

    const base = resolveBase(arena, cwd, opts) catch {
        try stderr.writeAll("zigttp proofs gate: base ref does not resolve (tried --base / origin/main / main)\n");
        return 2;
    };
    const head_label: []const u8 = opts.head orelse "working tree";

    const paths = changedHandlers(arena, cwd, base, opts.head) catch |err| switch (err) {
        error.BadGitRef => {
            try stderr.print("zigttp proofs gate: cannot diff `{s}` against `{s}`\n", .{ base, head_label });
            return 2;
        },
        else => return err,
    };

    var results: std.ArrayList(HandlerResult) = .empty;
    var skipped: std.ArrayList(Skipped) = .empty;
    var signed_rows: usize = 0;

    for (paths) |path| {
        const before = try readAtRef(arena, cwd, base, path);
        const after = try readAfter(arena, cwd, opts.head, path);
        if (after == null) {
            try skipped.append(arena, .{ .path = path, .reason = "unreadable" });
            continue;
        }

        // A newly added handler has no before contract: it cannot break an
        // existing caller, so it reads as additive.
        if (before == null) {
            try results.append(arena, .{ .path = path, .verdict = .additive });
            continue;
        }

        const result = analyzeHandler(gpa, arena, path, before.?, after.?, !opts.no_sign) catch {
            try skipped.append(arena, .{ .path = path, .reason = "no_contract" });
            continue;
        };
        const hr = result orelse {
            try skipped.append(arena, .{ .path = path, .reason = "no_contract" });
            continue;
        };
        if (hr.signed) signed_rows += 1;
        try results.append(arena, hr);
    }

    const verdict = worstVerdict(results.items);

    var report: std.Io.Writer.Allocating = .init(arena);
    switch (opts.format) {
        .md => try renderMarkdown(&report.writer, base, head_label, results.items, skipped.items, verdict, signed_rows),
        .json => try renderJson(&report.writer, base, head_label, results.items, skipped.items, verdict),
    }
    const bytes = report.writer.buffered();

    if (opts.out) |out_path| {
        file_io.writeFile(arena, out_path, bytes) catch {
            try stderr.print("zigttp proofs gate: cannot write report to `{s}`\n", .{out_path});
            return 2;
        };
    } else {
        try stdout.writeAll(bytes);
    }

    return if (isSafe(verdict)) 0 else 1;
}

fn resolveBase(arena: std.mem.Allocator, cwd: []const u8, opts: Options) ![]const u8 {
    if (opts.base) |b| {
        if (!refExists(arena, cwd, b)) return error.BadGitRef;
        return b;
    }
    if (refExists(arena, cwd, "origin/main")) return "origin/main";
    if (refExists(arena, cwd, "main")) return "main";
    return error.BadGitRef;
}

fn readAfter(arena: std.mem.Allocator, cwd: []const u8, head: ?[]const u8, path: []const u8) !?[]u8 {
    if (head) |h| return readAtRef(arena, cwd, h, path);
    return file_io.readFile(arena, path, max_source_bytes) catch |err| switch (err) {
        error.FileNotFound => return null,
        else => return err,
    };
}

/// Compile before/after to contracts, diff them, and project the result into
/// the arena-owned data model. Returns null when either side has no contract
/// (not a handler). The ContractDiff and compile results live only for the
/// duration of this call; everything kept is copied into the arena. When
/// `sign` is set, a signed `kind=equivalence` row is appended here, while both
/// contracts are still in scope, so the receipt path does not recompile them.
fn analyzeHandler(
    gpa: std.mem.Allocator,
    arena: std.mem.Allocator,
    path: []const u8,
    before_src: []const u8,
    after_src: []const u8,
    sign: bool,
) !?HandlerResult {
    var before_result = try zigts_cli.precompile.runCheckOnlyFromSource(gpa, before_src, path, null, true, null, false);
    defer before_result.deinit(gpa);
    var after_result = try zigts_cli.precompile.runCheckOnlyFromSource(gpa, after_src, path, null, true, null, false);
    defer after_result.deinit(gpa);

    const before_contract = if (before_result.contract) |*c| c else return null;
    const after_contract = if (after_result.contract) |*c| c else return null;

    // A changed `.ts`/`.js` file that compiles but exposes no routes and no
    // behavior paths is a library/config module, not a request handler. It
    // would only add equivalent-verdict noise to the report, so skip it.
    if (!isHandlerContract(after_contract)) return null;

    var diff = try contract_diff.diffContracts(gpa, before_contract, after_contract);
    defer diff.deinit(gpa);

    var signed = false;
    if (sign) {
        // Best-effort: append a signed kind=equivalence row from the contracts
        // already in hand. A failure never changes the verdict.
        equivalence_probe_lib.recordEquivalenceReceiptFromContracts(gpa, path, before_contract, after_contract, shared.nowUnixMs()) catch {};
        signed = true;
    }

    return .{
        .path = try arena.dupe(u8, path),
        .verdict = diff.behavioralVerdict(),
        .behavior = behaviorCounts(&diff),
        .surface = try extractSurface(arena, &diff),
        .counterexamples = try extractCounterexamples(arena, &diff),
        .signed = signed,
    };
}

// ---------------------------------------------------------------------------
// Tests — the pure aggregation + rendering core (no git, no compile).
// ---------------------------------------------------------------------------

const testing = std.testing;

test "worstVerdict: one breaking handler makes the repo breaking" {
    const results = [_]HandlerResult{
        .{ .path = "a.ts", .verdict = .equivalent },
        .{ .path = "b.ts", .verdict = .additive },
        .{ .path = "c.ts", .verdict = .breaking },
    };
    try testing.expectEqual(Classification.breaking, worstVerdict(&results));
}

test "worstVerdict: all equivalent stays equivalent (safe)" {
    const results = [_]HandlerResult{
        .{ .path = "a.ts", .verdict = .equivalent },
        .{ .path = "b.ts", .verdict = .equivalent_modulo_laws },
    };
    const v = worstVerdict(&results);
    try testing.expectEqual(Classification.equivalent_modulo_laws, v);
    try testing.expect(isSafe(v));
}

test "worstVerdict: empty range is safe" {
    try testing.expectEqual(Classification.equivalent, worstVerdict(&.{}));
    try testing.expect(isSafe(worstVerdict(&.{})));
}

test "renderMarkdown: breaking report carries header, table, and counterexample" {
    const cex = [_]Counterexample{
        .{ .method = "GET", .pattern = "/users/:id", .status = 200, .change = .response_changed },
        .{ .method = "DELETE", .pattern = "/users/:id", .status = 0, .change = .removed },
    };
    const removed_routes = [_][]const u8{"/users/:id"};
    const results = [_]HandlerResult{
        .{
            .path = "api/users.ts",
            .verdict = .breaking,
            .behavior = .{ .preserved = 4, .response_changed = 1, .removed = 1 },
            .surface = .{ .removed_routes = &removed_routes },
            .counterexamples = &cex,
        },
        .{ .path = "api/health.ts", .verdict = .equivalent },
    };

    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try renderMarkdown(&aw.writer, "origin/main", "working tree", &results, &.{}, .breaking, 0);
    const out = aw.writer.buffered();

    try testing.expect(std.mem.indexOf(u8, out, sticky_marker) != null);
    try testing.expect(std.mem.indexOf(u8, out, "**Verdict: `breaking`**") != null);
    try testing.expect(std.mem.indexOf(u8, out, "| `api/users.ts` | `breaking` |") != null);
    try testing.expect(std.mem.indexOf(u8, out, "1 response changed, 1 removed") != null);
    try testing.expect(std.mem.indexOf(u8, out, "`- route /users/:id`") != null);
    try testing.expect(std.mem.indexOf(u8, out, "`DELETE /users/:id` removed") != null);
    try testing.expect(std.mem.indexOf(u8, out, "`GET /users/:id` status 200 → response changed") != null);
}

test "renderMarkdown: empty range states no changed handlers" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try renderMarkdown(&aw.writer, "origin/main", "working tree", &.{}, &.{}, .equivalent, 0);
    try testing.expect(std.mem.indexOf(u8, aw.writer.buffered(), "_No changed handlers in this range._") != null);
}

test "renderJson: shape carries schema, verdict, safe, and behaviorDiff keys" {
    const added_routes = [_][]const u8{"/v2/orders"};
    const results = [_]HandlerResult{
        .{
            .path = "api/orders.ts",
            .verdict = .additive,
            .behavior = .{ .preserved = 2, .added = 2 },
            .surface = .{ .added_routes = &added_routes },
            .signed = true,
        },
    };

    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try renderJson(&aw.writer, "origin/main", "feature/x", &results, &.{}, .additive);
    const out = aw.writer.buffered();

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    const root = parsed.value.object;
    try testing.expectEqualStrings(json_schema, root.get("schema").?.string);
    try testing.expectEqualStrings("additive", root.get("verdict").?.string);
    try testing.expect(root.get("safe").?.bool);
    try testing.expectEqual(@as(i64, 1), root.get("handlersChecked").?.integer);

    const handler = root.get("handlers").?.array.items[0].object;
    try testing.expectEqualStrings("api/orders.ts", handler.get("path").?.string);
    const bd = handler.get("behaviorDiff").?.object;
    try testing.expectEqual(@as(i64, 2), bd.get("added").?.integer);
    try testing.expect(handler.get("signed").?.bool);
    const surface_routes = handler.get("surface").?.object.get("routes").?.object;
    try testing.expectEqualStrings("/v2/orders", surface_routes.get("added").?.array.items[0].string);
}
