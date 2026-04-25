//! Behavior Path Canonicalization
//!
//! Rewrites a `BehaviorPath.io_sequence` into a normal form using the
//! algebraic laws declared on each virtual-module `FunctionBinding`. Two
//! behavior paths are considered *observationally equivalent modulo laws*
//! iff their canonicalized io_sequences are identical. This is the mechanism
//! behind `zigts prove --equivalent` (wired up in a later step).
//!
//! ## Soundness invariant
//!
//! Every rewrite rule is unconditionally sound: the before- and after-
//! sequences produce the same observable effect *regardless of surrounding
//! context*. Conditional rules (commutation, reordering across writes,
//! context-dependent folding) are explicitly deferred until side-condition
//! discharge exists.
//!
//! The canonicalizer *never* changes `response_status`, `route_method`,
//! `route_pattern`, or `conditions`. These are path identity; if a rewrite
//! would perturb them, that rewrite is buggy and the assertion fires.
//!
//! ## Rules (priority order)
//!
//! 1. **Adjacent pure dedup** - if `seq[i]` and `seq[i+1]` are the same
//!    `(module, func)`, the function has the `.pure` law, and both calls
//!    have matching non-null `arg_signature`, drop `seq[i+1]`. Justified
//!    by `f(args) = f(args)`: the second call is a no-op if the first's
//!    result is reused. We do not require dataflow proof that the result
//!    *is* reused - for pure functions, executing twice vs once is
//!    observationally equivalent.
//!
//! 2. **Idempotent-write collapse** - if `seq[i]` and `seq[i+1]` are the
//!    same `(module, func)`, the function has the `.idempotent_call` law,
//!    and both calls have matching non-null `arg_signature`, drop
//!    `seq[i+1]`. Justified by `f(args); f(args) = f(args)`. The only
//!    rule that applies to write-effect functions.
//!
//! ## Deferred rules
//!
//! - **Inverse collapse** (`g(f(x)) = x`): requires SSA / dataflow tracking
//!   to prove that the second call's argument is the first call's output.
//!   Without it, collapsing a `base64Encode` followed by a `base64Decode`
//!   is unsound because the two calls could operate on unrelated values.
//!
//! - **Absorbing fold** (`f(absorbing_arg) = residue`): requires enough
//!   control-flow analysis to prune downstream statements the residue
//!   makes dead. Ripping out the call alone is not equivalent because
//!   subsequent statements may observe the residue.
//!
//! Both deferred rules remain valid on `FunctionBinding`s; the canonicalizer
//! simply does not apply them yet. The empirical law-synthesis harness
//! (`laws_synth.zig`, later step) validates laws independently of which
//! rules the canonicalizer currently consumes, so seeded `.inverse_of` and
//! `.absorbing` laws are not wasted - they are load-bearing for the
//! follow-up work.

const std = @import("std");
const mb = @import("module_binding.zig");
const handler_contract = @import("handler_contract.zig");
const builtin_modules = @import("builtin_modules.zig");

const PathIoCall = handler_contract.PathIoCall;
const PathCondition = handler_contract.PathCondition;
const BehaviorPath = handler_contract.BehaviorPath;
const FunctionBinding = mb.FunctionBinding;
const Law = mb.Law;
const LawKind = mb.LawKind;

// ---------------------------------------------------------------------------
// Law registry
// ---------------------------------------------------------------------------

/// Result of looking up a `(module, func)` pair in the law registry. Both
/// `module_name` and `binding` point at comptime data with program lifetime,
/// so they are safe to embed in long-lived records without ownership tracking.
pub const LookupResult = struct {
    module_name: []const u8,
    binding: *const FunctionBinding,
};

/// Thin wrapper around `builtin_modules.findFunction` that validates the
/// short module name matches. Exists as a named seam so tests can inject
/// custom function lookup without reaching into `builtin_modules`.
pub const LawRegistry = struct {
    lookup_fn: *const fn (module: []const u8, func: []const u8) ?LookupResult,

    pub const default: LawRegistry = .{ .lookup_fn = defaultLookup };

    pub fn lookup(self: LawRegistry, module: []const u8, func: []const u8) ?LookupResult {
        return self.lookup_fn(module, func);
    }

    pub fn hasLaw(self: LawRegistry, module: []const u8, func: []const u8, kind: LawKind) bool {
        const hit = self.lookup(module, func) orelse return false;
        for (hit.binding.laws) |law| {
            if (std.meta.activeTag(law) == kind) return true;
        }
        return false;
    }
};

fn defaultLookup(module: []const u8, func: []const u8) ?LookupResult {
    for (&builtin_modules.all) |*binding| {
        if (!std.mem.eql(u8, binding.name, module) and !std.mem.eql(u8, binding.specifier, module)) continue;
        for (binding.exports) |*candidate| {
            if (std.mem.eql(u8, candidate.name, func)) {
                return .{ .module_name = binding.name, .binding = candidate };
            }
        }
    }
    return null;
}

// ---------------------------------------------------------------------------
// Signature equality
// ---------------------------------------------------------------------------

/// Two calls have *equal* arg signatures iff both are non-null and byte-equal.
/// A null signature (the path generator could not capture args) disables all
/// rewrites on that call.
fn signaturesEqual(a: ?[]const u8, b: ?[]const u8) bool {
    const sa = a orelse return false;
    const sb = b orelse return false;
    return std.mem.eql(u8, sa, sb);
}

/// True iff two adjacent calls are the same virtual-module function with
/// matching argument signatures.
fn isAdjacentDuplicate(a: PathIoCall, b: PathIoCall) bool {
    if (!std.mem.eql(u8, a.module, b.module)) return false;
    if (!std.mem.eql(u8, a.func, b.func)) return false;
    return signaturesEqual(a.arg_signature, b.arg_signature);
}

// ---------------------------------------------------------------------------
// Canonicalization
// ---------------------------------------------------------------------------

/// Identity of a single law that fired during canonicalization. Strings
/// point into the `FunctionBinding` literals declared at comptime, so they
/// are valid for the lifetime of the program and do not require ownership
/// tracking.
pub const FiredLawId = struct {
    module: []const u8,
    func: []const u8,
    kind: LawKind,

    pub fn eql(a: FiredLawId, b: FiredLawId) bool {
        return a.kind == b.kind and
            std.mem.eql(u8, a.module, b.module) and
            std.mem.eql(u8, a.func, b.func);
    }
};

/// Per-rule rewrite counts. Useful for surfacing to users and for the
/// law-synthesis harness to detect no-progress bugs.
pub const CanonicalStats = struct {
    pure_dedup: u32 = 0,
    idempotent_collapse: u32 = 0,

    pub fn total(self: CanonicalStats) u32 {
        return self.pure_dedup + self.idempotent_collapse;
    }
};

/// Rewrite `path.io_sequence` into canonical form. Returns a fully-owned
/// clone of the input path with the rewritten sequence; the caller is
/// responsible for `deinit`. The input is never mutated.
///
/// `stats_out` receives per-rule rewrite counts when provided.
/// `fired_out` receives one `FiredLawId` per applied rewrite when provided,
/// in the order the rewrites fired. Callers that want a deduped set should
/// collapse the list themselves.
pub fn canonicalize(
    allocator: std.mem.Allocator,
    path: *const BehaviorPath,
    registry: LawRegistry,
    stats_out: ?*CanonicalStats,
    fired_out: ?*std.ArrayList(FiredLawId),
) !BehaviorPath {
    var out = try path.dupeOwned(allocator);
    errdefer out.deinit(allocator);

    var stats: CanonicalStats = .{};
    // Fixpoint loop: each pass rewrites at most one adjacent duplicate,
    // exits on the first pass that makes no progress. Cap at seq.len
    // iterations as a belt-and-braces guard against rule oscillation.
    const max_passes: usize = out.io_sequence.items.len + 1;
    var pass: usize = 0;
    while (pass < max_passes) : (pass += 1) {
        const before = out.io_sequence.items.len;
        try runPass(allocator, &out.io_sequence, registry, &stats, fired_out);
        if (out.io_sequence.items.len == before) break;
    }

    std.debug.assert(path.response_status == out.response_status);
    std.debug.assert(path.io_depth >= out.io_depth);
    std.debug.assert(std.mem.eql(u8, path.route_method, out.route_method));
    std.debug.assert(std.mem.eql(u8, path.route_pattern, out.route_pattern));
    std.debug.assert(path.is_failure_path == out.is_failure_path);
    std.debug.assert(path.conditions.items.len == out.conditions.items.len);

    out.io_depth = @intCast(out.io_sequence.items.len);

    if (stats_out) |s| s.* = stats;
    return out;
}

/// One rewriting pass over the sequence. Applies the first matching rule
/// to the first adjacent duplicate pair found, records the fired law, and
/// returns. The outer loop re-runs until the sequence stabilizes.
fn runPass(
    allocator: std.mem.Allocator,
    seq: *std.ArrayList(PathIoCall),
    registry: LawRegistry,
    stats: *CanonicalStats,
    fired_out: ?*std.ArrayList(FiredLawId),
) !void {
    if (seq.items.len < 2) return;

    var i: usize = 0;
    while (i + 1 < seq.items.len) : (i += 1) {
        const a = seq.items[i];
        const b = seq.items[i + 1];

        if (!isAdjacentDuplicate(a, b)) continue;

        // Priority order: .pure before .idempotent_call. Both rewrites
        // drop seq[i+1]; the difference is which rule receives credit.
        const hit = registry.lookup(a.module, a.func) orelse continue;
        const kind = firstApplicableLaw(hit.binding) orelse continue;

        dropAt(allocator, seq, i + 1);
        switch (kind) {
            .pure => stats.pure_dedup += 1,
            .idempotent_call => stats.idempotent_collapse += 1,
            else => unreachable,
        }
        if (fired_out) |list| {
            try list.append(allocator, .{
                .module = hit.module_name,
                .func = hit.binding.name,
                .kind = kind,
            });
        }
        return;
    }
}

/// Return the highest-priority law kind on `binding` that the canonicalizer
/// currently consumes (`.pure` > `.idempotent_call`). Returns null when no
/// consumable law is present - `.inverse_of` and `.absorbing` are declared
/// but not yet exploited by the rewriter.
fn firstApplicableLaw(binding: *const FunctionBinding) ?LawKind {
    var has_idempotent = false;
    for (binding.laws) |law| {
        switch (law) {
            .pure => return .pure,
            .idempotent_call => has_idempotent = true,
            else => {},
        }
    }
    return if (has_idempotent) .idempotent_call else null;
}

fn dropAt(allocator: std.mem.Allocator, seq: *std.ArrayList(PathIoCall), index: usize) void {
    var removed = seq.orderedRemove(index);
    removed.deinit(allocator);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

/// Build a PathIoCall with owned strings for use in tests.
fn makeCall(
    allocator: std.mem.Allocator,
    module: []const u8,
    func: []const u8,
    args: ?[]const u8,
) !PathIoCall {
    return .{
        .module = try allocator.dupe(u8, module),
        .func = try allocator.dupe(u8, func),
        .arg_signature = if (args) |a| try allocator.dupe(u8, a) else null,
    };
}

fn makePath(
    allocator: std.mem.Allocator,
    io: []const PathIoCall,
    status: u16,
) !BehaviorPath {
    var seq: std.ArrayList(PathIoCall) = .empty;
    errdefer {
        for (seq.items) |*c| @constCast(c).deinit(allocator);
        seq.deinit(allocator);
    }
    for (io) |c| try seq.append(allocator, c);

    return .{
        .route_method = try allocator.dupe(u8, "GET"),
        .route_pattern = try allocator.dupe(u8, "/test"),
        .conditions = .empty,
        .io_sequence = seq,
        .response_status = status,
        .io_depth = @intCast(io.len),
        .is_failure_path = false,
    };
}

test "canonicalize drops adjacent duplicate pure calls" {
    const a = testing.allocator;

    const calls = [_]PathIoCall{
        try makeCall(a, "env", "env", "lit:DATABASE_URL"),
        try makeCall(a, "env", "env", "lit:DATABASE_URL"),
        try makeCall(a, "env", "env", "lit:REDIS_URL"),
    };
    var in = try makePath(a, &calls, 200);
    defer in.deinit(a);

    var stats: CanonicalStats = .{};
    var out = try canonicalize(a, &in, .default, &stats, null);
    defer out.deinit(a);

    try testing.expectEqual(@as(usize, 2), out.io_sequence.items.len);
    try testing.expectEqual(@as(u32, 1), stats.pure_dedup);
    try testing.expectEqualStrings("env", out.io_sequence.items[0].func);
    try testing.expectEqualStrings("lit:DATABASE_URL", out.io_sequence.items[0].arg_signature.?);
    try testing.expectEqualStrings("lit:REDIS_URL", out.io_sequence.items[1].arg_signature.?);
}

test "canonicalize does not drop pure calls with different args" {
    const a = testing.allocator;

    const calls = [_]PathIoCall{
        try makeCall(a, "env", "env", "lit:A"),
        try makeCall(a, "env", "env", "lit:B"),
    };
    var in = try makePath(a, &calls, 200);
    defer in.deinit(a);

    var out = try canonicalize(a, &in, .default, null, null);
    defer out.deinit(a);

    try testing.expectEqual(@as(usize, 2), out.io_sequence.items.len);
}

test "canonicalize requires non-null arg signature for rewrite" {
    const a = testing.allocator;

    const calls = [_]PathIoCall{
        try makeCall(a, "env", "env", null),
        try makeCall(a, "env", "env", null),
    };
    var in = try makePath(a, &calls, 200);
    defer in.deinit(a);

    var out = try canonicalize(a, &in, .default, null, null);
    defer out.deinit(a);

    // Null signatures never match; both calls preserved.
    try testing.expectEqual(@as(usize, 2), out.io_sequence.items.len);
}

test "canonicalize collapses adjacent idempotent cacheSet" {
    const a = testing.allocator;

    const calls = [_]PathIoCall{
        try makeCall(a, "cache", "cacheSet", "lit:sessions|lit:k|lit:v|?"),
        try makeCall(a, "cache", "cacheSet", "lit:sessions|lit:k|lit:v|?"),
    };
    var in = try makePath(a, &calls, 200);
    defer in.deinit(a);

    var stats: CanonicalStats = .{};
    var out = try canonicalize(a, &in, .default, &stats, null);
    defer out.deinit(a);

    try testing.expectEqual(@as(usize, 1), out.io_sequence.items.len);
    try testing.expectEqual(@as(u32, 1), stats.idempotent_collapse);
}

test "canonicalize does not collapse cacheSet with different keys" {
    const a = testing.allocator;

    const calls = [_]PathIoCall{
        try makeCall(a, "cache", "cacheSet", "lit:sessions|lit:a|lit:v|?"),
        try makeCall(a, "cache", "cacheSet", "lit:sessions|lit:b|lit:v|?"),
    };
    var in = try makePath(a, &calls, 200);
    defer in.deinit(a);

    var out = try canonicalize(a, &in, .default, null, null);
    defer out.deinit(a);

    try testing.expectEqual(@as(usize, 2), out.io_sequence.items.len);
}

test "canonicalize does not touch unrelated calls between duplicates" {
    const a = testing.allocator;

    // env, sha256, env - the two envs are NOT adjacent, so no rewrite.
    // This documents that MVP canonicalization is adjacency-only;
    // non-adjacent dedup across commuting ops requires `.commutes_with`.
    const calls = [_]PathIoCall{
        try makeCall(a, "env", "env", "lit:X"),
        try makeCall(a, "crypto", "sha256", "lit:Y"),
        try makeCall(a, "env", "env", "lit:X"),
    };
    var in = try makePath(a, &calls, 200);
    defer in.deinit(a);

    var out = try canonicalize(a, &in, .default, null, null);
    defer out.deinit(a);

    try testing.expectEqual(@as(usize, 3), out.io_sequence.items.len);
}

test "canonicalize is a no-op on functions without laws" {
    const a = testing.allocator;

    // cacheIncr has no law (it's genuinely stateful accumulation).
    const calls = [_]PathIoCall{
        try makeCall(a, "cache", "cacheIncr", "lit:counters|lit:hits|?|?"),
        try makeCall(a, "cache", "cacheIncr", "lit:counters|lit:hits|?|?"),
    };
    var in = try makePath(a, &calls, 200);
    defer in.deinit(a);

    var out = try canonicalize(a, &in, .default, null, null);
    defer out.deinit(a);

    try testing.expectEqual(@as(usize, 2), out.io_sequence.items.len);
}

test "canonicalize preserves route, status, and condition count" {
    const a = testing.allocator;

    const calls = [_]PathIoCall{
        try makeCall(a, "env", "env", "lit:KEY"),
        try makeCall(a, "env", "env", "lit:KEY"),
    };
    var seq: std.ArrayList(PathIoCall) = .empty;
    for (calls) |c| try seq.append(a, c);
    errdefer {
        for (seq.items) |*c| @constCast(c).deinit(a);
        seq.deinit(a);
    }

    var conditions: std.ArrayList(PathCondition) = .empty;
    try conditions.append(a, .{
        .kind = .io_ok,
        .module = try a.dupe(u8, "auth"),
        .func = try a.dupe(u8, "jwtVerify"),
        .value = null,
    });

    var in = BehaviorPath{
        .route_method = try a.dupe(u8, "POST"),
        .route_pattern = try a.dupe(u8, "/api/session"),
        .conditions = conditions,
        .io_sequence = seq,
        .response_status = 201,
        .io_depth = 2,
        .is_failure_path = false,
    };
    defer in.deinit(a);

    var out = try canonicalize(a, &in, .default, null, null);
    defer out.deinit(a);

    try testing.expectEqualStrings("POST", out.route_method);
    try testing.expectEqualStrings("/api/session", out.route_pattern);
    try testing.expectEqual(@as(u16, 201), out.response_status);
    try testing.expectEqual(@as(usize, 1), out.conditions.items.len);
    try testing.expectEqual(@as(usize, 1), out.io_sequence.items.len);
    try testing.expectEqual(@as(u32, 1), out.io_depth);
}

test "LawRegistry default lookup finds seeded laws" {
    try testing.expect(LawRegistry.default.hasLaw("crypto", "sha256", .pure));
    try testing.expect(LawRegistry.default.hasLaw("env", "env", .pure));
    try testing.expect(LawRegistry.default.hasLaw("cache", "cacheSet", .idempotent_call));
    try testing.expect(!LawRegistry.default.hasLaw("cache", "cacheIncr", .pure));
    try testing.expect(!LawRegistry.default.hasLaw("cache", "cacheIncr", .idempotent_call));
}

test "LawRegistry lookup rejects wrong module name" {
    // sha256 exists in crypto, but not in 'env'.
    try testing.expect(LawRegistry.default.lookup("env", "sha256") == null);
    const hit = LawRegistry.default.lookup("crypto", "sha256");
    try testing.expect(hit != null);
    try testing.expectEqualStrings("crypto", hit.?.module_name);
    try testing.expectEqualStrings("sha256", hit.?.binding.name);
}

test "canonicalize chains multiple dedups across passes" {
    const a = testing.allocator;

    // Three adjacent identical pure calls collapse to one over two passes.
    const calls = [_]PathIoCall{
        try makeCall(a, "env", "env", "lit:KEY"),
        try makeCall(a, "env", "env", "lit:KEY"),
        try makeCall(a, "env", "env", "lit:KEY"),
    };
    var in = try makePath(a, &calls, 200);
    defer in.deinit(a);

    var stats: CanonicalStats = .{};
    var out = try canonicalize(a, &in, .default, &stats, null);
    defer out.deinit(a);

    try testing.expectEqual(@as(usize, 1), out.io_sequence.items.len);
    try testing.expectEqual(@as(u32, 2), stats.pure_dedup);
}

test "canonicalize reports only laws that actually fired" {
    const a = testing.allocator;

    // Path contains one dedupable pair (env.env) and one unrelated lawed
    // call (crypto.sha256). Without precise attribution, a naive collector
    // would report sha256.pure as "used". With per-rewrite tracking, we
    // expect exactly one fired law: env.env.pure.
    const calls = [_]PathIoCall{
        try makeCall(a, "env", "env", "lit:KEY"),
        try makeCall(a, "env", "env", "lit:KEY"),
        try makeCall(a, "crypto", "sha256", "lit:payload"),
    };
    var in = try makePath(a, &calls, 200);
    defer in.deinit(a);

    var fired: std.ArrayList(FiredLawId) = .empty;
    defer fired.deinit(a);

    var out = try canonicalize(a, &in, .default, null, &fired);
    defer out.deinit(a);

    try testing.expectEqual(@as(usize, 1), fired.items.len);
    try testing.expectEqualStrings("env", fired.items[0].module);
    try testing.expectEqualStrings("env", fired.items[0].func);
    try testing.expectEqual(LawKind.pure, fired.items[0].kind);
}
