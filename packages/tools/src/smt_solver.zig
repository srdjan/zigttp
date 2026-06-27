//! smt_solver.zig - the native, I/O half of spec-check mechanism 5.
//!
//! semantics_smt.zig (in the zigts package) is pure: it turns an equivalence
//! obligation into an SMT-LIB2 query string. This file runs that query through
//! the `z3` binary and returns a verdict. It lives in the tools layer, never the
//! zigts package, so `std.process.Child` / file I/O stay out of the freestanding
//! and wasm analyzer builds. The driver (`semantics_check.runSmt`) takes `solve`
//! as an injected function pointer; the standalone keyless callers and any build
//! without z3 simply pass null and the SMT mechanism is skipped.
//!
//! z3 is resolved to an ABSOLUTE path so argv[0] resolution never depends on the
//! child's PATH (the empty-environ-breaks-PATH-lookup trap that otherwise makes
//! tool invocation flaky). The child still inherits the parent environment; z3's
//! sat/unsat verdict on these tiny ground queries does not depend on it. Any
//! spawn / IO / parse failure maps to `.unknown`, which the driver records as an
//! *unproven* obligation (not a counterexample): a broken or missing solver can
//! never produce a false "equivalent", and an environmental failure does not
//! masquerade as a value-equivalence counterexample.
//!
//! Resolution order, with ZIGTTP_Z3 authoritative so SMT can be turned off:
//!   - ZIGTTP_Z3 = off / none / 0 / disable / "" -> SMT disabled (resolve null).
//!   - ZIGTTP_Z3 = <path>  -> that path if executable, else null (NO PATH
//!     fallback: an explicit choice that fails is honored, not silently masked).
//!   - ZIGTTP_Z3 unset     -> search PATH, then a few well-known locations.
//! This gives a real opt-out: `ZIGTTP_Z3=off` skips the mechanism even on a host
//! that has z3 installed (scrubbing PATH alone does not, because of the
//! well-known fallbacks).

const std = @import("std");
const zigts = @import("zigts");

const Verdict = zigts.semantics_smt.Verdict;
const file_io = zigts.file_io;

/// Well-known absolute locations checked only when ZIGTTP_Z3 is unset.
const candidates = [_][]const u8{
    "/opt/homebrew/bin/z3",
    "/usr/local/bin/z3",
    "/usr/bin/z3",
    "/opt/local/bin/z3",
};

fn isExecutable(alloc: std.mem.Allocator, path: []const u8) bool {
    const z = alloc.dupeZ(u8, path) catch return false;
    defer alloc.free(z);
    return std.c.access(z.ptr, std.c.X_OK) == 0;
}

/// True if an explicit ZIGTTP_Z3 value means "disable the SMT mechanism".
fn isDisableSentinel(v: []const u8) bool {
    return v.len == 0 or
        std.ascii.eqlIgnoreCase(v, "off") or
        std.ascii.eqlIgnoreCase(v, "none") or
        std.ascii.eqlIgnoreCase(v, "0") or
        std.ascii.eqlIgnoreCase(v, "disable") or
        std.ascii.eqlIgnoreCase(v, "disabled");
}

const ResolverEnv = struct {
    z3: ?[]const u8,
    path: ?[]const u8,
};

fn envSlice(name: [:0]const u8) ?[]const u8 {
    if (std.c.getenv(name.ptr)) |raw| return std.mem.span(raw);
    return null;
}

/// Resolve an absolute path to the z3 binary, or null if none is found / SMT is
/// explicitly disabled. The returned slice is owned by the caller.
pub fn resolveZ3(alloc: std.mem.Allocator) ?[]const u8 {
    return resolveZ3From(alloc, .{
        .z3 = envSlice("ZIGTTP_Z3"),
        .path = envSlice("PATH"),
    }, &candidates);
}

fn resolveZ3From(alloc: std.mem.Allocator, env: ResolverEnv, fallback_candidates: []const []const u8) ?[]const u8 {
    // ZIGTTP_Z3 is authoritative when set: it both pins the binary and provides
    // the opt-out, and it does NOT fall through to PATH/candidates.
    if (env.z3) |p| {
        if (isDisableSentinel(p)) return null;
        return if (isExecutable(alloc, p)) (alloc.dupe(u8, p) catch null) else null;
    }
    if (env.path) |path| {
        var it = std.mem.tokenizeScalar(u8, path, ':');
        while (it.next()) |dir| {
            // Skip non-absolute PATH entries (a stray '.' or relative dir).
            if (!std.fs.path.isAbsolute(dir)) continue;
            const cand = std.fmt.allocPrint(alloc, "{s}/z3", .{dir}) catch continue;
            if (isExecutable(alloc, cand)) return cand;
            alloc.free(cand);
        }
    }
    for (fallback_candidates) |c| {
        if (isExecutable(alloc, c)) return alloc.dupe(u8, c) catch null;
    }
    return null;
}

/// Whether a usable z3 is present. Used by the CLI to report "skipped" cleanly.
pub fn available(alloc: std.mem.Allocator) bool {
    const p = resolveZ3(alloc) orelse return false;
    alloc.free(p);
    return true;
}

fn tmpQueryPath(alloc: std.mem.Allocator) ![]u8 {
    const dir = if (std.c.getenv("TMPDIR")) |raw| std.mem.span(raw) else "/tmp";
    const trimmed = std.mem.trimEnd(u8, dir, "/");
    return std.fmt.allocPrint(alloc, "{s}/zigttp-smt-{d}.smt2", .{ trimmed, std.c.getpid() });
}

/// SolveFn-compatible: run `query` through z3 and return the verdict. The query
/// goes via a temp file because `std.process.run` ignores stdin. A spawn/IO
/// failure (or z3 emitting no usable verdict) maps to `.solver_error`, distinct
/// from a genuine `.unknown` timeout - the audit must not treat a broken solver
/// as a benign inconclusive.
pub fn solve(query: []const u8, alloc: std.mem.Allocator) Verdict {
    return solveImpl(query, alloc) catch .solver_error;
}

fn solveImpl(query: []const u8, alloc: std.mem.Allocator) !Verdict {
    const z3 = resolveZ3(alloc) orelse return .solver_error;
    defer alloc.free(z3);

    const tmp = try tmpQueryPath(alloc);
    defer alloc.free(tmp);
    try file_io.writeFile(alloc, tmp, query);
    defer {
        const tz = alloc.dupeZ(u8, tmp) catch null;
        if (tz) |z| {
            _ = std.c.unlink(z.ptr);
            alloc.free(z);
        }
    }

    var io_backend = std.Io.Threaded.init(alloc, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const res = try std.process.run(alloc, io, .{
        .argv = &.{ z3, "-smt2", tmp },
    });
    defer alloc.free(res.stdout);
    defer alloc.free(res.stderr);

    return zigts.semantics_smt.parseVerdict(res.stdout);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "disable sentinel recognizes the opt-out spellings" {
    try std.testing.expect(isDisableSentinel(""));
    try std.testing.expect(isDisableSentinel("off"));
    try std.testing.expect(isDisableSentinel("OFF"));
    try std.testing.expect(isDisableSentinel("none"));
    try std.testing.expect(isDisableSentinel("0"));
    try std.testing.expect(isDisableSentinel("disable"));
    try std.testing.expect(!isDisableSentinel("/opt/homebrew/bin/z3"));
    try std.testing.expect(!isDisableSentinel("z3"));
}

test "resolveZ3From honors disable and explicit invalid path without fallback" {
    const a = std.testing.allocator;
    const fallbacks = &[_][]const u8{"/opt/homebrew/bin/z3"};

    try std.testing.expect(resolveZ3From(a, .{ .z3 = "off", .path = "/bin" }, fallbacks) == null);
    try std.testing.expect(resolveZ3From(a, .{ .z3 = "/definitely/not/z3", .path = "/bin" }, fallbacks) == null);
}

test "resolveZ3From searches absolute PATH entries" {
    const a = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "z3", .data = "#!/bin/sh\n" });
    const dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", a);
    defer a.free(dir);
    const z3_path = try std.fs.path.join(a, &.{ dir, "z3" });
    defer a.free(z3_path);
    const z3_path_z = try a.dupeZ(u8, z3_path);
    defer a.free(z3_path_z);
    try std.testing.expectEqual(@as(c_int, 0), std.c.chmod(z3_path_z.ptr, 0o755));

    const found = resolveZ3From(a, .{ .z3 = null, .path = dir }, &[_][]const u8{}) orelse {
        try std.testing.expect(false);
        return;
    };
    defer a.free(found);
    try std.testing.expectEqualStrings(z3_path, found);
}

test "resolveZ3 and available agree" {
    const a = std.testing.allocator;
    const found = resolveZ3(a);
    if (found) |p| {
        defer a.free(p);
        try std.testing.expect(available(a));
        try std.testing.expect(std.fs.path.isAbsolute(p));
    } else {
        try std.testing.expect(!available(a));
    }
}

test "solve proves a true equivalence when z3 is present" {
    const a = std.testing.allocator;
    if (!available(a)) return error.SkipZigTest;
    // (set-logic ALL)(declare-const x Int)(assert (not (= (* x 2) (+ x x))))(check-sat)
    const lhs = [_]zigts.semantics.Term{ .{ .child = 0 }, .{ .child = 0 }, .{ .binop = .add } };
    const rhs = [_]zigts.semantics.Term{ .{ .child = 0 }, .{ .child = 0 }, .{ .binop = .add } };
    const q = try zigts.semantics_smt.encodeEquivalence(a, &lhs, &rhs, false);
    defer a.free(q);
    try std.testing.expectEqual(Verdict.equivalent, solve(q, a));
}

test "solve refutes a faithful-model non-law when z3 is present" {
    // Exercises the REFUTE direction through real z3 + the faithful encoder, so a
    // drift that made the encoding accidentally always-equal (unsat) is caught
    // here, not only in scripts/verify.sh. add_commutative is the fast refutation
    // (string concat counterexample).
    const a = std.testing.allocator;
    if (!available(a)) return error.SkipZigTest;
    const lhs = [_]zigts.semantics.Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .add } };
    const rhs = [_]zigts.semantics.Term{ .{ .child = 1 }, .{ .child = 0 }, .{ .binop = .add } };
    const q = try zigts.semantics_audit.encodeRefutation(a, &lhs, &rhs);
    defer a.free(q);
    try std.testing.expectEqual(Verdict.counterexample, solve(q, a));
}
