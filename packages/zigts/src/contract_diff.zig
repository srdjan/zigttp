//! Contract Diff
//!
//! Compares two HandlerContract values and produces a structural diff
//! describing what changed between handler versions. Used by the proven
//! evolution pipeline to classify upgrades as equivalent, additive, or breaking.
//!
//! All string slices in ContractDiff point into the source contracts.
//! The caller must keep both contracts alive while using the diff.

const std = @import("std");
const handler_contract = @import("handler_contract.zig");
const behavior_canonical = @import("behavior_canonical.zig");
const builtin_modules = @import("builtin_modules.zig");
const HandlerContract = handler_contract.HandlerContract;
const RouteInfo = handler_contract.RouteInfo;
const ApiRouteInfo = handler_contract.ApiRouteInfo;

// -------------------------------------------------------------------------
// Proof level (same semantics as deploy_manifest.ProofLevel)
// -------------------------------------------------------------------------

pub const ProofLevel = enum {
    /// All sections fully proven (no dynamic flags, verification passed)
    complete,
    /// Some sections proven, some need manual review
    partial,
    /// No verification ran
    none,

    pub fn toString(self: ProofLevel) []const u8 {
        return switch (self) {
            .complete => "complete",
            .partial => "partial",
            .none => "none",
        };
    }
};

pub fn deriveProofLevel(contract: *const HandlerContract) ProofLevel {
    const has_verification = contract.verification != null;
    if (!has_verification) return .none;

    const v = contract.verification.?;
    const all_checks = v.exhaustive_returns and v.results_safe and v.bytecode_verified;
    const no_dynamic = !contract.env.dynamic and !contract.egress.dynamic and !contract.cache.dynamic and !contract.sql.dynamic;

    if (all_checks and no_dynamic) return .complete;
    return .partial;
}

// -------------------------------------------------------------------------
// Diff data types
// -------------------------------------------------------------------------

pub const Classification = enum {
    /// Contract unchanged AND all traces identical. Pure refactor.
    equivalent,
    /// Contract surface unchanged and every BehaviorPath, after
    /// canonicalization under the algebraic laws declared on virtual
    /// modules, is equal to its counterpart in the other version.
    /// Strictly stronger than `.equivalent`: the laws_used list
    /// enumerates which equations the proof relied on.
    equivalent_modulo_laws,
    /// New routes/capabilities added, existing preserved. No regressions.
    additive,
    /// Routes/capabilities removed OR traces diverged. Needs review.
    breaking,

    pub fn toString(self: Classification) []const u8 {
        return switch (self) {
            .equivalent => "equivalent",
            .equivalent_modulo_laws => "equivalent_modulo_laws",
            .additive => "additive",
            .breaking => "breaking",
        };
    }

    /// True when the deploy is a proven no-op: surfaces unchanged and
    /// behavior either byte-equal or canonically-equivalent under laws.
    /// Callers that want to skip work for no-op deploys must use this
    /// helper rather than comparing against `.equivalent` directly.
    pub fn isSafeNoOp(self: Classification) bool {
        return self == .equivalent or self == .equivalent_modulo_laws;
    }
};

pub const ChangeStatus = enum {
    added,
    removed,
    unchanged,
};

pub const ItemChange = struct {
    value: []const u8,
    status: ChangeStatus,
};

pub const RouteChange = struct {
    pattern: []const u8,
    route_type: []const u8,
    status: ChangeStatus,
};

pub const ApiResponseChange = enum {
    unchanged,
    additive,
    breaking,
    dynamic,
};

pub const ApiRouteChange = struct {
    method: []const u8,
    path: []const u8,
    status: ChangeStatus,
    response: ApiResponseChange = .unchanged,
};

pub const DynamicChange = struct {
    section: []const u8,
    old_dynamic: bool,
    new_dynamic: bool,
};

pub const PropertiesChange = struct {
    field: []const u8,
    old_value: bool,
    new_value: bool,
};

pub const BehaviorPathChange = struct {
    method: []const u8,
    pattern: []const u8,
    status: u16,
    change: BehaviorChangeKind,
};

pub const BehaviorChangeKind = enum {
    preserved,
    response_changed,
    removed,
    added,
};

pub const BehaviorDiff = struct {
    preserved: u32 = 0,
    response_changed: u32 = 0,
    removed: u32 = 0,
    added: u32 = 0,
    changes: std.ArrayList(BehaviorPathChange) = .empty,

    pub fn deinit(self: *BehaviorDiff, allocator: std.mem.Allocator) void {
        self.changes.deinit(allocator);
    }

    pub fn total(self: *const BehaviorDiff) u32 {
        return self.preserved + self.response_changed + self.removed + self.added;
    }

    pub fn hasBreaking(self: *const BehaviorDiff) bool {
        return self.removed > 0 or self.response_changed > 0;
    }
};

pub const ReplaySummary = struct {
    total: u32 = 0,
    identical: u32 = 0,
    status_changed: u32 = 0,
    body_changed: u32 = 0,
    diverged: u32 = 0,

    pub fn coverage(self: ReplaySummary) f64 {
        if (self.total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.identical)) / @as(f64, @floatFromInt(self.total));
    }
};

pub const ContractDiff = struct {
    routes: std.ArrayList(RouteChange),
    api_route_changes: std.ArrayList(ApiRouteChange),
    env_changes: std.ArrayList(ItemChange),
    egress_changes: std.ArrayList(ItemChange),
    cache_changes: std.ArrayList(ItemChange),
    sql_changes: std.ArrayList(ItemChange),
    dynamic_changes: std.ArrayList(DynamicChange),
    /// Informational only - not used for classification.
    effect_changes: std.ArrayList(PropertiesChange) = .empty,
    /// Behavioral path diff - present when both contracts have behaviors.
    behavior_diff: ?BehaviorDiff = null,
    /// Identifiers of laws that fired during canonical reclassification,
    /// in the form "<module>.<func>.<law_kind>". Owned. A non-empty list
    /// causes `classify()` to return `.equivalent_modulo_laws` when the
    /// structural verdict is `.equivalent`.
    laws_used: std.ArrayList([]const u8) = .empty,
    /// First canonical-path mismatch when `tryCanonicalEquivalence` runs
    /// but cannot reclassify. Owned. null when canonicalization succeeds
    /// or when it was never attempted.
    canonical_counterexample: ?[]const u8 = null,

    pub fn deinit(self: *ContractDiff, allocator: std.mem.Allocator) void {
        self.routes.deinit(allocator);
        self.api_route_changes.deinit(allocator);
        self.env_changes.deinit(allocator);
        self.egress_changes.deinit(allocator);
        self.cache_changes.deinit(allocator);
        self.sql_changes.deinit(allocator);
        self.dynamic_changes.deinit(allocator);
        self.effect_changes.deinit(allocator);
        if (self.behavior_diff) |*bd| bd.deinit(allocator);
        for (self.laws_used.items) |s| allocator.free(s);
        self.laws_used.deinit(allocator);
        if (self.canonical_counterexample) |c| allocator.free(c);
    }

    pub fn capabilitiesWidened(self: *const ContractDiff) bool {
        for (self.dynamic_changes.items) |dc| {
            if (dc.new_dynamic and !dc.old_dynamic) return true;
        }
        return false;
    }

    pub fn capabilitiesNarrowed(self: *const ContractDiff) bool {
        for (self.dynamic_changes.items) |dc| {
            if (!dc.new_dynamic and dc.old_dynamic) return true;
        }
        return false;
    }

    /// Classify the diff structurally (without replay data).
    /// When `tryCanonicalEquivalence` has recorded fired laws, returns
    /// `.equivalent_modulo_laws` in place of `.equivalent`.
    pub fn classify(self: *const ContractDiff) Classification {
        const structural = self.classifyStructural();
        if (structural == .equivalent and self.laws_used.items.len > 0) {
            return .equivalent_modulo_laws;
        }
        return structural;
    }

    /// Classify the diff purely structurally, ignoring any canonicalization
    /// upgrade. Exposed for tests and for callers that want the raw answer.
    pub fn classifyStructural(self: *const ContractDiff) Classification {
        var has_added = false;

        for (self.routes.items) |r| {
            if (r.status == .removed) return .breaking;
            if (r.status == .added) has_added = true;
        }
        for (self.api_route_changes.items) |route| {
            if (route.status == .removed) return .breaking;
            if (route.status == .added) has_added = true;
            switch (route.response) {
                .breaking => return .breaking,
                .additive => has_added = true,
                .unchanged, .dynamic => {},
            }
        }
        inline for (.{ self.env_changes.items, self.egress_changes.items, self.cache_changes.items, self.sql_changes.items }) |items| {
            for (items) |c| {
                if (c.status == .removed) return .breaking;
                if (c.status == .added) has_added = true;
            }
        }

        if (self.capabilitiesNarrowed()) return .breaking;
        if (has_added or self.capabilitiesWidened()) return .additive;

        return .equivalent;
    }

    /// Classify considering both structural diff and replay results.
    pub fn classifyWithReplay(self: *const ContractDiff, replay: *const ReplaySummary) Classification {
        // Any replay divergence is breaking regardless of structural diff
        if (replay.total > 0 and replay.identical < replay.total) return .breaking;

        return self.classify();
    }

    /// Attempt to upgrade `.equivalent` to `.equivalent_modulo_laws` by
    /// canonicalizing every BehaviorPath on both sides and checking that
    /// the canonical forms match. On success, populates `laws_used`. On
    /// mismatch, populates `canonical_counterexample`. Best-effort.
    ///
    /// No-op when the structural classification is not already
    /// `.equivalent` - canonicalization cannot rescue a surface change.
    pub fn tryCanonicalEquivalence(
        self: *ContractDiff,
        allocator: std.mem.Allocator,
        old: *const HandlerContract,
        new: *const HandlerContract,
    ) !void {
        if (self.laws_used.items.len > 0) return;
        if (self.classifyStructural() != .equivalent) return;
        if (old.behaviors.items.len == 0 and new.behaviors.items.len == 0) return;
        if (old.behaviors.items.len != new.behaviors.items.len) {
            try self.setCounterexample(allocator, "behavior path count differs");
            return;
        }

        var fired: std.ArrayList(behavior_canonical.FiredLawId) = .empty;
        defer fired.deinit(allocator);

        // Canonicalize every new path up front so the match loop is
        // O(|old| + |new|) instead of O(|old| * |new|).
        var new_canons = try std.ArrayList(handler_contract.BehaviorPath).initCapacity(
            allocator,
            new.behaviors.items.len,
        );
        defer {
            for (new_canons.items) |*c| c.deinit(allocator);
            new_canons.deinit(allocator);
        }
        for (new.behaviors.items) |*np| {
            const canon = try behavior_canonical.canonicalize(allocator, np, .default, null, &fired);
            new_canons.appendAssumeCapacity(canon);
        }

        var matched = try allocator.alloc(bool, new_canons.items.len);
        defer allocator.free(matched);
        @memset(matched, false);

        for (old.behaviors.items) |*old_path| {
            var old_canon = try behavior_canonical.canonicalize(allocator, old_path, .default, null, &fired);
            defer old_canon.deinit(allocator);

            var found = false;
            for (new_canons.items, 0..) |*new_canon, j| {
                if (matched[j]) continue;
                if (!canonicalPathsEqual(&old_canon, new_canon)) continue;
                matched[j] = true;
                found = true;
                break;
            }

            if (!found) {
                const msg = try std.fmt.allocPrint(
                    allocator,
                    "{s} {s} -> no canonically-equivalent new path (old response {d})",
                    .{ old_path.route_method, old_path.route_pattern, old_path.response_status },
                );
                defer allocator.free(msg);
                try self.setCounterexample(allocator, msg);
                return;
            }
        }

        // Every old path matched. Dedup the fired laws in-place, then
        // render each unique triple into an owned identifier string.
        var seen_end: usize = 0;
        for (fired.items) |law| {
            if (containsLawId(fired.items[0..seen_end], law)) continue;
            fired.items[seen_end] = law;
            seen_end += 1;
        }
        for (fired.items[0..seen_end]) |law| {
            const id = try std.fmt.allocPrint(allocator, "{s}.{s}.{s}", .{
                law.module,
                law.func,
                @tagName(law.kind),
            });
            errdefer allocator.free(id);
            try self.laws_used.append(allocator, id);
        }
    }

    fn setCounterexample(self: *ContractDiff, allocator: std.mem.Allocator, msg: []const u8) !void {
        const owned = try allocator.dupe(u8, msg);
        if (self.canonical_counterexample) |prev| allocator.free(prev);
        self.canonical_counterexample = owned;
    }
};

/// Canonicalized paths are equal when their keys (route, conditions)
/// match AND their response/failure status match AND their io_sequences
/// are byte-for-byte identical.
fn canonicalPathsEqual(a: *const handler_contract.BehaviorPath, b: *const handler_contract.BehaviorPath) bool {
    if (!behaviorPathKeysMatch(a, b)) return false;
    if (a.response_status != b.response_status) return false;
    if (a.is_failure_path != b.is_failure_path) return false;
    if (a.io_sequence.items.len != b.io_sequence.items.len) return false;
    for (a.io_sequence.items, b.io_sequence.items) |ia, ib| {
        if (!std.mem.eql(u8, ia.module, ib.module)) return false;
        if (!std.mem.eql(u8, ia.func, ib.func)) return false;
        if (!eqlOptStr(ia.arg_signature, ib.arg_signature)) return false;
    }
    return true;
}

/// Linear membership check on an already-seen list of FiredLawIds.
/// Typical cardinality is under 10 so a scan beats any hashed alternative.
fn containsLawId(seen: []const behavior_canonical.FiredLawId, law: behavior_canonical.FiredLawId) bool {
    for (seen) |s| {
        if (s.eql(law)) return true;
    }
    return false;
}

pub const ProofCertificate = struct {
    classification: Classification,
    old_handler: []const u8,
    new_handler: []const u8,
    diff: ContractDiff,
    replay: ?ReplaySummary,
    proof_level: ProofLevel,
    recommendation: []const u8,

    pub fn deinit(self: *ProofCertificate, allocator: std.mem.Allocator) void {
        self.diff.deinit(allocator);
        allocator.free(self.recommendation);
    }
};

// -------------------------------------------------------------------------
// Diff computation
// -------------------------------------------------------------------------

/// Compare two contracts and produce a structural diff.
/// All slices in the returned diff point into the old/new contracts.
pub fn diffContracts(
    allocator: std.mem.Allocator,
    old: *const HandlerContract,
    new: *const HandlerContract,
) !ContractDiff {
    var routes: std.ArrayList(RouteChange) = .empty;
    errdefer routes.deinit(allocator);

    var api_route_changes: std.ArrayList(ApiRouteChange) = .empty;
    errdefer api_route_changes.deinit(allocator);

    var env_changes: std.ArrayList(ItemChange) = .empty;
    errdefer env_changes.deinit(allocator);

    var egress_changes: std.ArrayList(ItemChange) = .empty;
    errdefer egress_changes.deinit(allocator);

    var cache_changes: std.ArrayList(ItemChange) = .empty;
    errdefer cache_changes.deinit(allocator);

    var sql_changes: std.ArrayList(ItemChange) = .empty;
    errdefer sql_changes.deinit(allocator);

    var dynamic_changes: std.ArrayList(DynamicChange) = .empty;
    errdefer dynamic_changes.deinit(allocator);

    // Route comparison (by pattern + type)
    for (old.routes.items) |old_route| {
        if (findRoute(new.routes.items, old_route.pattern, old_route.route_type)) |_| {
            try routes.append(allocator, .{
                .pattern = old_route.pattern,
                .route_type = old_route.route_type,
                .status = .unchanged,
            });
        } else {
            try routes.append(allocator, .{
                .pattern = old_route.pattern,
                .route_type = old_route.route_type,
                .status = .removed,
            });
        }
    }
    for (new.routes.items) |new_route| {
        if (findRoute(old.routes.items, new_route.pattern, new_route.route_type) == null) {
            try routes.append(allocator, .{
                .pattern = new_route.pattern,
                .route_type = new_route.route_type,
                .status = .added,
            });
        }
    }

    // API route comparison (method + path)
    for (old.api.routes.items) |old_route| {
        if (findApiRoute(new.api.routes.items, old_route.method, old_route.path)) |new_route| {
            try api_route_changes.append(allocator, .{
                .method = old_route.method,
                .path = old_route.path,
                .status = .unchanged,
                .response = try compareApiRouteResponse(allocator, &old_route, new_route),
            });
        } else {
            try api_route_changes.append(allocator, .{
                .method = old_route.method,
                .path = old_route.path,
                .status = .removed,
            });
        }
    }
    for (new.api.routes.items) |new_route| {
        if (findApiRoute(old.api.routes.items, new_route.method, new_route.path) == null) {
            try api_route_changes.append(allocator, .{
                .method = new_route.method,
                .path = new_route.path,
                .status = .added,
            });
        }
    }

    // Env comparison
    try diffStringList(allocator, old.env.literal.items, new.env.literal.items, &env_changes);

    // Egress comparison
    try diffStringList(allocator, old.egress.hosts.items, new.egress.hosts.items, &egress_changes);

    // Cache comparison
    try diffStringList(allocator, old.cache.namespaces.items, new.cache.namespaces.items, &cache_changes);

    // SQL comparison
    try diffSqlList(allocator, old.sql.queries.items, new.sql.queries.items, &sql_changes);

    // Dynamic flag changes
    inline for (.{
        .{ "env", old.env.dynamic, new.env.dynamic },
        .{ "egress", old.egress.dynamic, new.egress.dynamic },
        .{ "cache", old.cache.dynamic, new.cache.dynamic },
        .{ "sql", old.sql.dynamic, new.sql.dynamic },
    }) |entry| {
        if (entry[1] != entry[2]) {
            try dynamic_changes.append(allocator, .{
                .section = entry[0],
                .old_dynamic = entry[1],
                .new_dynamic = entry[2],
            });
        }
    }

    // Handler properties comparison (informational only)
    var effect_changes: std.ArrayList(PropertiesChange) = .empty;
    errdefer effect_changes.deinit(allocator);

    if (old.properties != null and new.properties != null) {
        const op = old.properties.?;
        const np = new.properties.?;
        inline for (.{
            .{ "pure", op.pure, np.pure },
            .{ "read_only", op.read_only, np.read_only },
            .{ "stateless", op.stateless, np.stateless },
            .{ "retry_safe", op.retry_safe, np.retry_safe },
            .{ "deterministic", op.deterministic, np.deterministic },
            .{ "injection_safe", op.injection_safe, np.injection_safe },
            .{ "idempotent", op.idempotent, np.idempotent },
            .{ "state_isolated", op.state_isolated, np.state_isolated },
            .{ "fault_covered", op.fault_covered, np.fault_covered },
        }) |entry| {
            if (entry[1] != entry[2]) {
                try effect_changes.append(allocator, .{
                    .field = entry[0],
                    .old_value = entry[1],
                    .new_value = entry[2],
                });
            }
        }

        // max_io_depth: ?u32 needs separate comparison (not a bool)
        const old_depth = op.max_io_depth orelse 0;
        const new_depth = np.max_io_depth orelse 0;
        if (old_depth != new_depth) {
            try effect_changes.append(allocator, .{
                .field = "max_io_depth",
                .old_value = old_depth > 0,
                .new_value = new_depth > 0,
            });
        }
    }

    // Behavioral path comparison (when both contracts have behaviors)
    var behavior_diff: ?BehaviorDiff = null;
    if (old.behaviors.items.len > 0 or new.behaviors.items.len > 0) {
        behavior_diff = try diffBehaviors(allocator, old.behaviors.items, new.behaviors.items);
    }

    var diff = ContractDiff{
        .routes = routes,
        .api_route_changes = api_route_changes,
        .env_changes = env_changes,
        .egress_changes = egress_changes,
        .cache_changes = cache_changes,
        .sql_changes = sql_changes,
        .dynamic_changes = dynamic_changes,
        .effect_changes = effect_changes,
        .behavior_diff = behavior_diff,
    };
    errdefer diff.deinit(allocator);

    try diff.tryCanonicalEquivalence(allocator, old, new);

    return diff;
}

// -------------------------------------------------------------------------
// Behavioral diff
// -------------------------------------------------------------------------

const BehaviorPath = handler_contract.BehaviorPath;
const PathCondition = handler_contract.PathCondition;

/// Compare two sets of behavioral paths. A path is identified by its
/// (method, pattern, conditions) tuple. If the conditions match but the
/// response status differs, the path is classified as response_changed.
fn diffBehaviors(
    allocator: std.mem.Allocator,
    old_paths: []const BehaviorPath,
    new_paths: []const BehaviorPath,
) !BehaviorDiff {
    var diff = BehaviorDiff{};
    errdefer diff.deinit(allocator);

    // For each old path, check if a matching new path exists
    for (old_paths) |old_path| {
        if (findMatchingPath(new_paths, &old_path)) |new_path| {
            if (old_path.response_status == new_path.response_status) {
                diff.preserved += 1;
                try diff.changes.append(allocator, .{
                    .method = old_path.route_method,
                    .pattern = old_path.route_pattern,
                    .status = old_path.response_status,
                    .change = .preserved,
                });
            } else {
                diff.response_changed += 1;
                try diff.changes.append(allocator, .{
                    .method = old_path.route_method,
                    .pattern = old_path.route_pattern,
                    .status = new_path.response_status,
                    .change = .response_changed,
                });
            }
        } else {
            diff.removed += 1;
            try diff.changes.append(allocator, .{
                .method = old_path.route_method,
                .pattern = old_path.route_pattern,
                .status = old_path.response_status,
                .change = .removed,
            });
        }
    }

    // Find new paths not present in old
    for (new_paths) |new_path| {
        if (findMatchingPath(old_paths, &new_path) == null) {
            diff.added += 1;
            try diff.changes.append(allocator, .{
                .method = new_path.route_method,
                .pattern = new_path.route_pattern,
                .status = new_path.response_status,
                .change = .added,
            });
        }
    }

    return diff;
}

/// Find a path in the list that matches the target by (method, pattern, conditions).
fn findMatchingPath(paths: []const BehaviorPath, target: *const BehaviorPath) ?*const BehaviorPath {
    for (paths) |*path| {
        if (behaviorPathKeysMatch(path, target)) return path;
    }
    return null;
}

/// Two behavior paths match if they have the same route and the same conditions.
fn behaviorPathKeysMatch(a: *const BehaviorPath, b: *const BehaviorPath) bool {
    if (!std.mem.eql(u8, a.route_method, b.route_method)) return false;
    if (!std.mem.eql(u8, a.route_pattern, b.route_pattern)) return false;
    if (a.conditions.items.len != b.conditions.items.len) return false;

    for (a.conditions.items, b.conditions.items) |ca, cb| {
        if (!conditionsEqual(&ca, &cb)) return false;
    }
    return true;
}

fn conditionsEqual(a: *const PathCondition, b: *const PathCondition) bool {
    if (a.kind != b.kind) return false;
    if (!eqlOptStr(a.module, b.module)) return false;
    if (!eqlOptStr(a.func, b.func)) return false;
    if (!eqlOptStr(a.value, b.value)) return false;
    return true;
}

fn eqlOptStr(a: ?[]const u8, b: ?[]const u8) bool {
    if (a == null and b == null) return true;
    if (a == null or b == null) return false;
    return std.mem.eql(u8, a.?, b.?);
}

// -------------------------------------------------------------------------
// JSON emission
// -------------------------------------------------------------------------

pub fn writeProofJson(cert: *const ProofCertificate, writer: anytype) !void {
    try writer.writeAll("{\n");

    try writer.writeAll("  \"classification\": \"");
    try writer.writeAll(cert.classification.toString());
    try writer.writeAll("\",\n");

    try writer.writeAll("  \"old_handler\": ");
    try writeJsonString(writer, cert.old_handler);
    try writer.writeAll(",\n");

    try writer.writeAll("  \"new_handler\": ");
    try writeJsonString(writer, cert.new_handler);
    try writer.writeAll(",\n");

    // contract_diff
    try writer.writeAll("  \"contract_diff\": {\n");
    try writeChangesJson(writer, "routes_added", cert.diff.routes.items, .added);
    try writer.writeAll(",\n");
    try writeChangesJson(writer, "routes_removed", cert.diff.routes.items, .removed);
    try writer.writeAll(",\n");
    try writeChangesJson(writer, "routes_unchanged", cert.diff.routes.items, .unchanged);
    try writer.writeAll(",\n");
    try writeApiRouteChangesJson(writer, "api_routes_added", cert.diff.api_route_changes.items, .added, null);
    try writer.writeAll(",\n");
    try writeApiRouteChangesJson(writer, "api_routes_removed", cert.diff.api_route_changes.items, .removed, null);
    try writer.writeAll(",\n");
    try writeApiRouteChangesJson(writer, "api_response_additive", cert.diff.api_route_changes.items, .unchanged, .additive);
    try writer.writeAll(",\n");
    try writeApiRouteChangesJson(writer, "api_response_breaking", cert.diff.api_route_changes.items, .unchanged, .breaking);
    try writer.writeAll(",\n");
    try writeItemChangesJson(writer, "env_added", cert.diff.env_changes.items, .added);
    try writer.writeAll(",\n");
    try writeItemChangesJson(writer, "env_removed", cert.diff.env_changes.items, .removed);
    try writer.writeAll(",\n");
    try writeItemChangesJson(writer, "egress_added", cert.diff.egress_changes.items, .added);
    try writer.writeAll(",\n");
    try writeItemChangesJson(writer, "egress_removed", cert.diff.egress_changes.items, .removed);
    try writer.writeAll(",\n");
    try writeItemChangesJson(writer, "sql_added", cert.diff.sql_changes.items, .added);
    try writer.writeAll(",\n");
    try writeItemChangesJson(writer, "sql_removed", cert.diff.sql_changes.items, .removed);
    try writer.writeAll(",\n");
    try writer.print("    \"capabilities_widened\": {s},\n", .{if (cert.diff.capabilitiesWidened()) "true" else "false"});
    try writer.print("    \"capabilities_narrowed\": {s}\n", .{if (cert.diff.capabilitiesNarrowed()) "true" else "false"});
    try writer.writeAll("  },\n");

    // replay_result
    if (cert.replay) |replay| {
        try writer.writeAll("  \"replay_result\": {\n");
        try writer.print("    \"total_traces\": {d},\n", .{replay.total});
        try writer.print("    \"identical\": {d},\n", .{replay.identical});
        try writer.print("    \"status_changed\": {d},\n", .{replay.status_changed});
        try writer.print("    \"body_changed\": {d},\n", .{replay.body_changed});

        const cov = replay.coverage();
        // Write coverage as a decimal with one digit
        const cov_pct = @as(u32, @intFromFloat(cov * 1000.0));
        const whole = cov_pct / 10;
        const frac = cov_pct % 10;
        try writer.print("    \"coverage\": {d}.{d}\n", .{ whole, frac });

        try writer.writeAll("  },\n");
    } else {
        try writer.writeAll("  \"replay_result\": null,\n");
    }

    try writer.writeAll("  \"proof_level\": \"");
    try writer.writeAll(cert.proof_level.toString());
    try writer.writeAll("\",\n");

    try writer.writeAll("  \"laws_used\": [");
    for (cert.diff.laws_used.items, 0..) |law_id, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeJsonString(writer, law_id);
    }
    try writer.writeAll("],\n");

    // Behavioral diff
    if (cert.diff.behavior_diff) |bd| {
        try writer.writeAll("  \"behavioralDiff\": {\n");
        try writer.print("    \"preserved\": {d},\n", .{bd.preserved});
        try writer.print("    \"responseChanged\": {d},\n", .{bd.response_changed});
        try writer.print("    \"removed\": {d},\n", .{bd.removed});
        try writer.print("    \"added\": {d}\n", .{bd.added});
        try writer.writeAll("  },\n");
    }

    try writer.writeAll("  \"recommendation\": ");
    try writeJsonString(writer, cert.recommendation);
    try writer.writeAll("\n");

    try writer.writeAll("}\n");
}

pub fn writeProofReport(writer: anytype, cert: *const ProofCertificate) !void {
    try writer.writeAll("PROVEN EVOLUTION REPORT\n");
    try writer.writeAll("=======================\n\n");

    try writer.writeAll("Classification: ");
    switch (cert.classification) {
        .equivalent => try writer.writeAll("EQUIVALENT (safe to deploy)\n"),
        .equivalent_modulo_laws => try writer.writeAll("EQUIVALENT MODULO LAWS (safe to deploy, proof via canonicalization)\n"),
        .additive => try writer.writeAll("ADDITIVE (safe to deploy)\n"),
        .breaking => try writer.writeAll("BREAKING (requires review)\n"),
    }

    if (cert.diff.laws_used.items.len > 0) {
        try writer.writeAll("\nLaws used in canonical equivalence proof:\n");
        for (cert.diff.laws_used.items) |law_id| {
            try writer.writeAll("  - ");
            try writer.writeAll(law_id);
            try writer.writeAll("\n");
        }
    } else if (cert.diff.canonical_counterexample) |cex| {
        try writer.writeAll("\nCanonical equivalence not established:\n  ");
        try writer.writeAll(cex);
        try writer.writeAll("\n");
    }

    try writer.writeAll("\nContract Diff:\n");

    // Routes
    for (cert.diff.routes.items) |route| {
        switch (route.status) {
            .unchanged => {
                try writer.writeAll("  UNCHANGED  ");
                try writer.writeAll(route.pattern);
                try writer.writeAll(" (");
                try writer.writeAll(route.route_type);
                try writer.writeAll(")\n");
            },
            .added => {
                try writer.writeAll("  + ADDED    ");
                try writer.writeAll(route.pattern);
                try writer.writeAll(" (");
                try writer.writeAll(route.route_type);
                try writer.writeAll(")\n");
            },
            .removed => {
                try writer.writeAll("  - REMOVED  ");
                try writer.writeAll(route.pattern);
                try writer.writeAll(" (");
                try writer.writeAll(route.route_type);
                try writer.writeAll(")\n");
            },
        }
    }

    for (cert.diff.api_route_changes.items) |route| {
        switch (route.status) {
            .added => {
                try writer.writeAll("  + ADDED    api: ");
                try writer.writeAll(route.method);
                try writer.writeByte(' ');
                try writer.writeAll(route.path);
                try writer.writeAll("\n");
            },
            .removed => {
                try writer.writeAll("  - REMOVED  api: ");
                try writer.writeAll(route.method);
                try writer.writeByte(' ');
                try writer.writeAll(route.path);
                try writer.writeAll("\n");
            },
            .unchanged => switch (route.response) {
                .additive => {
                    try writer.writeAll("  + ADDED    api response: ");
                    try writer.writeAll(route.method);
                    try writer.writeByte(' ');
                    try writer.writeAll(route.path);
                    try writer.writeAll("\n");
                },
                .breaking => {
                    try writer.writeAll("  - CHANGED  api response: ");
                    try writer.writeAll(route.method);
                    try writer.writeByte(' ');
                    try writer.writeAll(route.path);
                    try writer.writeAll("\n");
                },
                .dynamic => {
                    try writer.writeAll("  ~ REVIEW   api response: ");
                    try writer.writeAll(route.method);
                    try writer.writeByte(' ');
                    try writer.writeAll(route.path);
                    try writer.writeAll("\n");
                },
                .unchanged => {},
            },
        }
    }

    // Env
    for (cert.diff.env_changes.items) |c| {
        switch (c.status) {
            .added => {
                try writer.writeAll("  + ADDED    env: ");
                try writer.writeAll(c.value);
                try writer.writeAll("\n");
            },
            .removed => {
                try writer.writeAll("  - REMOVED  env: ");
                try writer.writeAll(c.value);
                try writer.writeAll("\n");
            },
            .unchanged => {},
        }
    }

    // Egress
    for (cert.diff.egress_changes.items) |c| {
        switch (c.status) {
            .added => {
                try writer.writeAll("  + ADDED    egress: ");
                try writer.writeAll(c.value);
                try writer.writeAll("\n");
            },
            .removed => {
                try writer.writeAll("  - REMOVED  egress: ");
                try writer.writeAll(c.value);
                try writer.writeAll("\n");
            },
            .unchanged => {},
        }
    }

    // Cache
    for (cert.diff.cache_changes.items) |c| {
        switch (c.status) {
            .added => {
                try writer.writeAll("  + ADDED    cache: ");
                try writer.writeAll(c.value);
                try writer.writeAll("\n");
            },
            .removed => {
                try writer.writeAll("  - REMOVED  cache: ");
                try writer.writeAll(c.value);
                try writer.writeAll("\n");
            },
            .unchanged => {},
        }
    }

    // SQL
    for (cert.diff.sql_changes.items) |c| {
        switch (c.status) {
            .added => {
                try writer.writeAll("  + ADDED    sql: ");
                try writer.writeAll(c.value);
                try writer.writeAll("\n");
            },
            .removed => {
                try writer.writeAll("  - REMOVED  sql: ");
                try writer.writeAll(c.value);
                try writer.writeAll("\n");
            },
            .unchanged => {},
        }
    }

    // Dynamic changes
    for (cert.diff.dynamic_changes.items) |dc| {
        try writer.writeAll("  ~ CHANGED  ");
        try writer.writeAll(dc.section);
        if (dc.new_dynamic) {
            try writer.writeAll(": proven -> dynamic\n");
        } else {
            try writer.writeAll(": dynamic -> proven\n");
        }
    }

    // Behavioral diff
    if (cert.diff.behavior_diff) |bd| {
        try writer.writeAll("\nBehavioral Paths:\n");
        try writer.print("  {d} preserved, {d} changed, {d} removed, {d} added\n", .{
            bd.preserved,
            bd.response_changed,
            bd.removed,
            bd.added,
        });
        for (bd.changes.items) |change| {
            switch (change.change) {
                .preserved => {},
                .response_changed => {
                    try writer.writeAll("  ~ CHANGED  ");
                    try writer.writeAll(change.method);
                    try writer.writeByte(' ');
                    try writer.writeAll(change.pattern);
                    try writer.print(" -> {d}\n", .{change.status});
                },
                .removed => {
                    try writer.writeAll("  - REMOVED  ");
                    try writer.writeAll(change.method);
                    try writer.writeByte(' ');
                    try writer.writeAll(change.pattern);
                    try writer.print(" ({d})\n", .{change.status});
                },
                .added => {
                    try writer.writeAll("  + ADDED    ");
                    try writer.writeAll(change.method);
                    try writer.writeByte(' ');
                    try writer.writeAll(change.pattern);
                    try writer.print(" ({d})\n", .{change.status});
                },
            }
        }
    }

    // Replay
    if (cert.replay) |replay| {
        try writer.writeAll("\nReplay Verification:\n");
        try writer.print("  {d} traces replayed\n", .{replay.total});
        try writer.print("  {d} identical responses", .{replay.identical});

        const cov = replay.coverage();
        const cov_pct = @as(u32, @intFromFloat(cov * 1000.0));
        const whole = cov_pct / 10;
        const frac = cov_pct % 10;
        try writer.print(" ({d}.{d}%)\n", .{ whole, frac });

        if (replay.status_changed > 0) {
            try writer.print("  {d} status changes\n", .{replay.status_changed});
        }
        if (replay.body_changed > 0) {
            try writer.print("  {d} body changes\n", .{replay.body_changed});
        }
        if (replay.diverged > 0) {
            try writer.print("  {d} diverged\n", .{replay.diverged});
        }
    }

    // Proof level
    try writer.writeAll("\nProof Level: ");
    try writer.writeAll(cert.proof_level.toString());
    try writer.writeAll("\n");

    // Recommendation
    try writer.writeAll("\nRecommendation: ");
    try writer.writeAll(cert.recommendation);
    try writer.writeAll("\n");
}

// -------------------------------------------------------------------------
// Recommendation generation
// -------------------------------------------------------------------------

pub fn generateRecommendation(
    allocator: std.mem.Allocator,
    classification: Classification,
    diff: *const ContractDiff,
    replay: ?*const ReplaySummary,
) ![]const u8 {
    var msg: std.ArrayList(u8) = .empty;
    errdefer msg.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &msg);
    const w = &aw.writer;

    switch (classification) {
        .equivalent => {
            try w.writeAll("Deploy immediately - ");
            if (replay) |r| {
                if (r.total > 0) {
                    try w.writeAll("all behavior preserved with 100% trace coverage");
                } else {
                    try w.writeAll("contract unchanged, no traces provided");
                }
            } else {
                try w.writeAll("contract unchanged");
            }
        },
        .equivalent_modulo_laws => {
            try w.writeAll("Deploy immediately - canonical equivalence proved via ");
            try w.print("{d} law{s}", .{
                diff.laws_used.items.len,
                if (diff.laws_used.items.len == 1) "" else "s",
            });
        },
        .additive => {
            try w.writeAll("Safe to deploy - ");
            var has_additions = false;

            // Count additions
            var route_adds: u32 = 0;
            for (diff.routes.items) |r| {
                if (r.status == .added) route_adds += 1;
            }
            if (route_adds > 0) {
                try w.print("{d} new route(s)", .{route_adds});
                has_additions = true;
            }

            var api_route_adds: u32 = 0;
            var api_response_adds: u32 = 0;
            for (diff.api_route_changes.items) |route| {
                if (route.status == .added) api_route_adds += 1;
                if (route.response == .additive) api_response_adds += 1;
            }
            if (api_route_adds > 0) {
                if (has_additions) try w.writeAll(", ");
                try w.print("{d} new API route(s)", .{api_route_adds});
                has_additions = true;
            }
            if (api_response_adds > 0) {
                if (has_additions) try w.writeAll(", ");
                try w.print("{d} expanded API response(s)", .{api_response_adds});
                has_additions = true;
            }

            var env_adds: u32 = 0;
            for (diff.env_changes.items) |c| {
                if (c.status == .added) env_adds += 1;
            }
            if (env_adds > 0) {
                if (has_additions) try w.writeAll(", ");
                try w.print("{d} new env var(s)", .{env_adds});
                has_additions = true;
            }

            var egress_adds: u32 = 0;
            for (diff.egress_changes.items) |c| {
                if (c.status == .added) egress_adds += 1;
            }
            if (egress_adds > 0) {
                if (has_additions) try w.writeAll(", ");
                try w.print("{d} new egress host(s)", .{egress_adds});
                has_additions = true;
            }

            var sql_adds: u32 = 0;
            for (diff.sql_changes.items) |c| {
                if (c.status == .added) sql_adds += 1;
            }
            if (sql_adds > 0) {
                if (has_additions) try w.writeAll(", ");
                try w.print("{d} new SQL query(s)", .{sql_adds});
                has_additions = true;
            }

            if (replay) |r| {
                if (r.total > 0) {
                    try w.writeAll(", all existing behavior preserved");
                }
            }
        },
        .breaking => {
            try w.writeAll("Requires review - ");

            if (replay) |r| {
                if (r.total > 0 and r.identical < r.total) {
                    const diverged = r.total - r.identical;
                    try w.print("{d}/{d} traces diverged", .{ diverged, r.total });
                    return (blk: {
                        msg = aw.toArrayList();
                        break :blk try msg.toOwnedSlice(allocator);
                    });
                }
            }

            var has_removals = false;
            for (diff.routes.items) |r| {
                if (r.status == .removed) {
                    if (has_removals) try w.writeAll(", ");
                    try w.writeAll("route removed: ");
                    try w.writeAll(r.pattern);
                    has_removals = true;
                }
            }
            for (diff.env_changes.items) |c| {
                if (c.status == .removed) {
                    if (has_removals) try w.writeAll(", ");
                    try w.writeAll("env removed: ");
                    try w.writeAll(c.value);
                    has_removals = true;
                }
            }
            for (diff.sql_changes.items) |c| {
                if (c.status == .removed) {
                    if (has_removals) try w.writeAll(", ");
                    try w.writeAll("sql removed: ");
                    try w.writeAll(c.value);
                    has_removals = true;
                }
            }
            if (diff.capabilitiesNarrowed()) {
                if (has_removals) try w.writeAll(", ");
                try w.writeAll("capabilities narrowed");
            }
        },
    }

    msg = aw.toArrayList();
    return try msg.toOwnedSlice(allocator);
}

// -------------------------------------------------------------------------
// Helpers
// -------------------------------------------------------------------------

fn findRoute(routes: []const RouteInfo, pattern: []const u8, route_type: []const u8) ?*const RouteInfo {
    for (routes) |*route| {
        if (std.mem.eql(u8, route.pattern, pattern) and std.mem.eql(u8, route.route_type, route_type)) {
            return route;
        }
    }
    return null;
}

fn diffStringList(
    allocator: std.mem.Allocator,
    old_items: []const []const u8,
    new_items: []const []const u8,
    changes: *std.ArrayList(ItemChange),
) !void {
    // Items in old
    for (old_items) |old_val| {
        if (containsString(new_items, old_val)) {
            try changes.append(allocator, .{ .value = old_val, .status = .unchanged });
        } else {
            try changes.append(allocator, .{ .value = old_val, .status = .removed });
        }
    }
    // Items only in new
    for (new_items) |new_val| {
        if (!containsString(old_items, new_val)) {
            try changes.append(allocator, .{ .value = new_val, .status = .added });
        }
    }
}

fn diffSqlList(
    allocator: std.mem.Allocator,
    old_items: []const handler_contract.SqlQueryInfo,
    new_items: []const handler_contract.SqlQueryInfo,
    changes: *std.ArrayList(ItemChange),
) !void {
    for (old_items) |old_query| {
        if (findSqlQuery(new_items, old_query.name, old_query.statement) != null) {
            try changes.append(allocator, .{ .value = old_query.name, .status = .unchanged });
        } else {
            try changes.append(allocator, .{ .value = old_query.name, .status = .removed });
        }
    }
    for (new_items) |new_query| {
        if (findSqlQuery(old_items, new_query.name, new_query.statement) == null) {
            try changes.append(allocator, .{ .value = new_query.name, .status = .added });
        }
    }
}

fn findApiRoute(
    haystack: []const ApiRouteInfo,
    method: []const u8,
    path: []const u8,
) ?*const ApiRouteInfo {
    for (haystack) |*item| {
        if (std.mem.eql(u8, item.method, method) and std.mem.eql(u8, item.path, path)) return item;
    }
    return null;
}

fn compareApiRouteResponse(
    allocator: std.mem.Allocator,
    old: *const ApiRouteInfo,
    new: *const ApiRouteInfo,
) !ApiResponseChange {
    var change = try compareApiParams(allocator, old.query_params.items, new.query_params.items);
    change = mergeApiChange(change, try compareApiParams(allocator, old.header_params.items, new.header_params.items));
    change = mergeApiChange(change, try compareRequestBodies(allocator, old, new));
    change = mergeApiChange(change, try compareResponses(allocator, old, new));
    return change;
}

fn mergeApiChange(current: ApiResponseChange, next: ApiResponseChange) ApiResponseChange {
    if (current == .breaking or next == .breaking) return .breaking;
    if (current == .dynamic or next == .dynamic) return .dynamic;
    if (current == .additive or next == .additive) return .additive;
    return .unchanged;
}

fn compareApiParams(
    allocator: std.mem.Allocator,
    old_items: []const handler_contract.ApiParamInfo,
    new_items: []const handler_contract.ApiParamInfo,
) !ApiResponseChange {
    var has_addition = false;
    for (old_items) |old_item| {
        const new_item = findApiParam(new_items, old_item.name) orelse return .breaking;
        if (!std.mem.eql(u8, old_item.location, new_item.location)) return .breaking;
        if (old_item.required and !new_item.required) {
            has_addition = true;
        } else if (!old_item.required and new_item.required) {
            return .breaking;
        }
        const schema_change = try compareSchemaJson(allocator, old_item.schema_json, new_item.schema_json);
        if (schema_change == .breaking or schema_change == .dynamic) return .breaking;
        if (schema_change == .additive) has_addition = true;
    }
    for (new_items) |new_item| {
        if (findApiParam(old_items, new_item.name) == null) has_addition = true;
    }
    return if (has_addition) .additive else .unchanged;
}

fn findApiParam(items: []const handler_contract.ApiParamInfo, name: []const u8) ?*const handler_contract.ApiParamInfo {
    for (items) |*item| {
        if (std.mem.eql(u8, item.name, name)) return item;
    }
    return null;
}

fn compareRequestBodies(
    allocator: std.mem.Allocator,
    old: *const ApiRouteInfo,
    new: *const ApiRouteInfo,
) !ApiResponseChange {
    if (old.request_bodies_dynamic or new.request_bodies_dynamic or old.request_schema_dynamic or new.request_schema_dynamic) {
        return .dynamic;
    }

    if (old.request_bodies.items.len == 0 and new.request_bodies.items.len == 0) {
        if (old.request_schema_refs.items.len != new.request_schema_refs.items.len) return .breaking;
        for (old.request_schema_refs.items) |schema_ref| {
            if (!containsString(new.request_schema_refs.items, schema_ref)) return .breaking;
        }
        return .unchanged;
    }

    if (old.request_bodies.items.len != new.request_bodies.items.len) return .breaking;
    if (old.request_bodies.items.len == 0) return .unchanged;

    for (old.request_bodies.items) |old_body| {
        const new_body = findRequestBody(new.request_bodies.items, old_body.content_type) orelse return .breaking;
        if (!eqlOptionalString(old_body.content_type, new_body.content_type)) return .breaking;
        const schema_change = try compareSchemaSpecs(allocator, old_body.schema, new_body.schema);
        if (schema_change == .dynamic) return .dynamic;
        if (schema_change != .unchanged) return .breaking;
    }

    return .unchanged;
}

fn findRequestBody(items: []const handler_contract.ApiBodyInfo, content_type: ?[]const u8) ?*const handler_contract.ApiBodyInfo {
    for (items) |*item| {
        if (eqlOptionalString(item.content_type, content_type)) return item;
    }
    return null;
}

fn compareResponses(
    allocator: std.mem.Allocator,
    old: *const ApiRouteInfo,
    new: *const ApiRouteInfo,
) !ApiResponseChange {
    if (old.responses.items.len == 0 and new.responses.items.len == 0) {
        if (old.response_status != new.response_status) return .breaking;
        if (!eqlOptionalString(old.response_content_type, new.response_content_type)) return .breaking;

        return try compareSchemaSpecs(
            allocator,
            schemaSpecView(old.response_schema_ref, old.response_schema_json, old.response_schema_dynamic),
            schemaSpecView(new.response_schema_ref, new.response_schema_json, new.response_schema_dynamic),
        );
    }

    if (old.responses_dynamic or new.responses_dynamic or old.response_schema_dynamic or new.response_schema_dynamic) return .dynamic;
    if (old.responses.items.len != new.responses.items.len) return .breaking;

    var has_addition = false;
    for (old.responses.items) |old_response| {
        const new_response = findResponse(new.responses.items, old_response.status, old_response.content_type) orelse return .breaking;
        const schema_change = try compareSchemaSpecs(allocator, old_response.schema, new_response.schema);
        if (schema_change == .breaking) return .breaking;
        if (schema_change == .dynamic) return .dynamic;
        if (schema_change == .additive) has_addition = true;
    }

    return if (has_addition) .additive else .unchanged;
}

fn findResponse(
    items: []const handler_contract.ApiResponseInfo,
    status: ?u16,
    content_type: ?[]const u8,
) ?*const handler_contract.ApiResponseInfo {
    for (items) |*item| {
        if (item.status == status and eqlOptionalString(item.content_type, content_type)) return item;
    }
    return null;
}

/// Borrowed-slice view of legacy scalar schema fields shaped as a SchemaSpec
/// for `compareSchemaSpecs`. The returned union must NOT be deinit'd: the
/// payload slices alias the caller's storage. Read-only use only.
fn schemaSpecView(
    schema_ref: ?[]const u8,
    schema_json: ?[]const u8,
    is_dynamic: bool,
) handler_contract.SchemaSpec {
    if (is_dynamic) return .dynamic;
    if (schema_json) |s| return .{ .inline_json = s };
    if (schema_ref) |s| return .{ .ref = s };
    return .none;
}

/// Compare two body schemas via exhaustive variant matching. Mixed-shape
/// pairs (one ref, one inline_json) and asymmetric none/something pairs
/// degrade to `.dynamic` rather than `.breaking`, matching the prior
/// behavior where one absent field signaled "cannot prove a structural diff".
fn compareSchemaSpecs(
    allocator: std.mem.Allocator,
    old_schema: handler_contract.SchemaSpec,
    new_schema: handler_contract.SchemaSpec,
) !ApiResponseChange {
    return switch (old_schema) {
        .dynamic => .dynamic,
        .none => switch (new_schema) {
            .none => .unchanged,
            .ref, .inline_json => .dynamic,
            .dynamic => .dynamic,
        },
        .ref => |old_ref| switch (new_schema) {
            .ref => |new_ref| if (std.mem.eql(u8, old_ref, new_ref)) .unchanged else .breaking,
            .none, .inline_json => .dynamic,
            .dynamic => .dynamic,
        },
        .inline_json => |old_json| switch (new_schema) {
            .inline_json => |new_json| try compareSchemaJson(allocator, old_json, new_json),
            .none, .ref => .dynamic,
            .dynamic => .dynamic,
        },
    };
}

fn compareSchemaJson(
    allocator: std.mem.Allocator,
    old_json: []const u8,
    new_json: []const u8,
) !ApiResponseChange {
    if (std.mem.eql(u8, old_json, new_json)) return .unchanged;

    var old_parsed = std.json.parseFromSlice(std.json.Value, allocator, old_json, .{}) catch return .breaking;
    defer old_parsed.deinit();
    var new_parsed = std.json.parseFromSlice(std.json.Value, allocator, new_json, .{}) catch return .breaking;
    defer new_parsed.deinit();

    return compareSchemaValue(old_parsed.value, new_parsed.value);
}

fn compareSchemaValue(old_value: std.json.Value, new_value: std.json.Value) ApiResponseChange {
    return switch (old_value) {
        .object => |old_obj| switch (new_value) {
            .object => |new_obj| blk: {
                const old_type = getSchemaString(old_obj, "type");
                const new_type = getSchemaString(new_obj, "type");
                if (!eqlOptionalString(old_type, new_type)) break :blk .breaking;
                if (old_type != null and std.mem.eql(u8, old_type.?, "object")) {
                    break :blk compareObjectSchema(old_obj, new_obj);
                }
                break :blk if (jsonValueEqual(old_value, new_value)) .unchanged else .breaking;
            },
            else => .breaking,
        },
        else => if (jsonValueEqual(old_value, new_value)) .unchanged else .breaking,
    };
}

fn compareObjectSchema(old_obj: std.json.ObjectMap, new_obj: std.json.ObjectMap) ApiResponseChange {
    const old_props = getSchemaObject(old_obj, "properties");
    const new_props = getSchemaObject(new_obj, "properties");
    const old_required = getRequiredSet(old_obj);
    const new_required = getRequiredSet(new_obj);

    var has_addition = false;
    if (old_props) |old_map| {
        var it = old_map.iterator();
        while (it.next()) |entry| {
            const new_value = if (new_props) |new_map| new_map.get(entry.key_ptr.*) else null;
            if (new_value == null) return .breaking;
            if (requiredContains(old_required, entry.key_ptr.*) != requiredContains(new_required, entry.key_ptr.*)) {
                return .breaking;
            }
            if (!jsonValueEqual(entry.value_ptr.*, new_value.?)) return .breaking;
        }
    }

    if (new_props) |new_map| {
        var it = new_map.iterator();
        while (it.next()) |entry| {
            if (old_props == null or old_props.?.get(entry.key_ptr.*) == null) {
                has_addition = true;
            }
        }
    }

    return if (has_addition) .additive else .unchanged;
}

fn getSchemaObject(obj: std.json.ObjectMap, key: []const u8) ?std.json.ObjectMap {
    if (obj.get(key)) |value| {
        return switch (value) {
            .object => |map| map,
            else => null,
        };
    }
    return null;
}

fn getSchemaString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    if (obj.get(key)) |value| {
        return switch (value) {
            .string => |str| str,
            else => null,
        };
    }
    return null;
}

fn getRequiredSet(obj: std.json.ObjectMap) ?[]const std.json.Value {
    if (obj.get("required")) |value| {
        return switch (value) {
            .array => |arr| arr.items,
            else => null,
        };
    }
    return null;
}

fn requiredContains(required: ?[]const std.json.Value, needle: []const u8) bool {
    const items = required orelse return false;
    for (items) |item| {
        switch (item) {
            .string => |value| if (std.mem.eql(u8, value, needle)) return true,
            else => {},
        }
    }
    return false;
}

fn jsonValueEqual(a: std.json.Value, b: std.json.Value) bool {
    return switch (a) {
        .null => switch (b) {
            .null => true,
            else => false,
        },
        .bool => |av| switch (b) {
            .bool => |bv| av == bv,
            else => false,
        },
        .integer => |av| switch (b) {
            .integer => |bv| av == bv,
            else => false,
        },
        .float => |av| switch (b) {
            .float => |bv| av == bv,
            else => false,
        },
        .string => |av| switch (b) {
            .string => |bv| std.mem.eql(u8, av, bv),
            else => false,
        },
        .array => |av| switch (b) {
            .array => |bv| blk: {
                if (av.items.len != bv.items.len) break :blk false;
                for (av.items, bv.items) |lhs, rhs| {
                    if (!jsonValueEqual(lhs, rhs)) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
        .object => |av| switch (b) {
            .object => |bv| blk: {
                if (av.count() != bv.count()) break :blk false;
                var it = av.iterator();
                while (it.next()) |entry| {
                    const other = bv.get(entry.key_ptr.*) orelse break :blk false;
                    if (!jsonValueEqual(entry.value_ptr.*, other)) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
        else => false,
    };
}

fn eqlOptionalString(a: ?[]const u8, b: ?[]const u8) bool {
    if (a == null and b == null) return true;
    if (a == null or b == null) return false;
    return std.mem.eql(u8, a.?, b.?);
}

const containsString = handler_contract.containsString;

fn findSqlQuery(
    haystack: []const handler_contract.SqlQueryInfo,
    name: []const u8,
    statement: []const u8,
) ?*const handler_contract.SqlQueryInfo {
    for (haystack) |*item| {
        if (std.mem.eql(u8, item.name, name) and std.mem.eql(u8, item.statement, statement)) return item;
    }
    return null;
}

const writeJsonString = handler_contract.writeJsonString;

fn writeChangesJson(writer: anytype, field: []const u8, routes: []const RouteChange, filter: ChangeStatus) !void {
    try writer.writeAll("    \"");
    try writer.writeAll(field);
    try writer.writeAll("\": [");
    var first = true;
    for (routes) |route| {
        if (route.status != filter) continue;
        if (!first) try writer.writeAll(", ");
        try writeJsonString(writer, route.pattern);
        first = false;
    }
    try writer.writeAll("]");
}

fn writeItemChangesJson(writer: anytype, field: []const u8, changes: []const ItemChange, filter: ChangeStatus) !void {
    try writer.writeAll("    \"");
    try writer.writeAll(field);
    try writer.writeAll("\": [");
    var first = true;
    for (changes) |change| {
        if (change.status != filter) continue;
        if (!first) try writer.writeAll(", ");
        try writeJsonString(writer, change.value);
        first = false;
    }
    try writer.writeAll("]");
}

fn writeApiRouteChangesJson(
    writer: anytype,
    field: []const u8,
    changes: []const ApiRouteChange,
    status_filter: ChangeStatus,
    response_filter: ?ApiResponseChange,
) !void {
    try writer.writeAll("    \"");
    try writer.writeAll(field);
    try writer.writeAll("\": [");
    var first = true;
    for (changes) |change| {
        if (change.status != status_filter) continue;
        if (response_filter) |filter| {
            if (change.response != filter) continue;
        }
        if (!first) try writer.writeAll(", ");
        try writer.writeByte('"');
        try writer.writeAll(change.method);
        try writer.writeByte(' ');
        try writer.writeAll(change.path);
        try writer.writeByte('"');
        first = false;
    }
    try writer.writeAll("]");
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

fn makeTestContract(allocator: std.mem.Allocator) !HandlerContract {
    const path = try allocator.dupe(u8, "handler.ts");
    return .{
        .handler = .{ .path = path, .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = handler_contract.emptySqlInfo(),
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
        },
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
        },
        .api = handler_contract.emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
}

test "diffContracts identical contracts are equivalent" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.equivalent, diff.classify());
    try std.testing.expectEqual(@as(usize, 0), diff.routes.items.len);
    try std.testing.expect(!diff.capabilitiesWidened());
    try std.testing.expect(!diff.capabilitiesNarrowed());
}

test "diffContracts added route is additive" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);

    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);
    const pattern = try allocator.dupe(u8, "/api/users");
    try new.routes.append(allocator, .{
        .pattern = pattern,
        .route_type = "exact",
        .field = "path",
        .status = 200,
        .content_type = "application/json",
        .aot = true,
    });

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.additive, diff.classify());
    try std.testing.expectEqual(@as(usize, 1), diff.routes.items.len);
    try std.testing.expectEqualStrings("/api/users", diff.routes.items[0].pattern);
    try std.testing.expectEqual(diff.routes.items[0].status, .added);
}

test "diffContracts removed route is breaking" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    const pattern = try allocator.dupe(u8, "/health");
    try old.routes.append(allocator, .{
        .pattern = pattern,
        .route_type = "exact",
        .field = "path",
        .status = 200,
        .content_type = "application/json",
        .aot = true,
    });

    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.breaking, diff.classify());
}

test "diffContracts added env var is additive" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);

    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);
    const env = try allocator.dupe(u8, "API_KEY");
    try new.env.literal.append(allocator, env);

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.additive, diff.classify());
    try std.testing.expectEqual(@as(usize, 1), diff.env_changes.items.len);
    try std.testing.expectEqualStrings("API_KEY", diff.env_changes.items[0].value);
}

test "diffContracts added sql query is additive" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);

    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);
    try new.sql.queries.append(allocator, .{
        .name = try allocator.dupe(u8, "getUser"),
        .statement = try allocator.dupe(u8, "SELECT id FROM users WHERE id = :id"),
        .operation = "select",
        .tables = .empty,
    });

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.additive, diff.classify());
    try std.testing.expectEqual(@as(usize, 1), diff.sql_changes.items.len);
    try std.testing.expectEqualStrings("getUser", diff.sql_changes.items[0].value);
    try std.testing.expectEqual(ChangeStatus.added, diff.sql_changes.items[0].status);
}

test "diffContracts removed env var is breaking" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    const env = try allocator.dupe(u8, "SECRET");
    try old.env.literal.append(allocator, env);

    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.breaking, diff.classify());
}

test "diffContracts dynamic flag change tracked" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);

    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);
    new.env.dynamic = true;

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), diff.dynamic_changes.items.len);
    try std.testing.expectEqualStrings("env", diff.dynamic_changes.items[0].section);
    try std.testing.expect(diff.capabilitiesWidened());
    try std.testing.expect(!diff.capabilitiesNarrowed());
    try std.testing.expectEqual(Classification.additive, diff.classify());
}

test "diffContracts api response field addition is additive" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    try old.api.routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/users/:id"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
        .response_status = 200,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_json = try allocator.dupe(u8, "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}"),
    });

    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);
    try new.api.routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/users/:id"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
        .response_status = 200,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_json = try allocator.dupe(u8, "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"}},\"required\":[\"id\"]}"),
    });

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), diff.api_route_changes.items.len);
    try std.testing.expectEqual(ApiResponseChange.additive, diff.api_route_changes.items[0].response);
    try std.testing.expectEqual(Classification.additive, diff.classify());
}

test "diffContracts api query param addition is additive" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    try old.api.routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/users"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
        .response_status = 200,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_json = try allocator.dupe(u8, "{\"type\":\"object\"}"),
    });

    var query_params: std.ArrayList(handler_contract.ApiParamInfo) = .empty;
    try query_params.append(allocator, .{
        .name = try allocator.dupe(u8, "verbose"),
        .location = "query",
        .required = false,
        .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
    });

    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);
    try new.api.routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/users"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
        .query_params = query_params,
        .response_status = 200,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_json = try allocator.dupe(u8, "{\"type\":\"object\"}"),
    });

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(ApiResponseChange.additive, diff.api_route_changes.items[0].response);
    try std.testing.expectEqual(Classification.additive, diff.classify());
}

test "diffContracts api response removal is breaking" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    try old.api.routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/users/:id"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
        .response_status = 200,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_json = try allocator.dupe(u8, "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"}},\"required\":[\"id\"]}"),
    });

    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);
    try new.api.routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/users/:id"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
        .response_status = 200,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_json = try allocator.dupe(u8, "{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}"),
    });

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(ApiResponseChange.breaking, diff.api_route_changes.items[0].response);
    try std.testing.expectEqual(Classification.breaking, diff.classify());
}

test "classifyWithReplay diverged traces are breaking" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    // Structurally equivalent, but traces diverge
    const replay = ReplaySummary{
        .total = 100,
        .identical = 95,
        .status_changed = 3,
        .body_changed = 2,
    };

    try std.testing.expectEqual(Classification.breaking, diff.classifyWithReplay(&replay));
}

test "classifyWithReplay all identical preserves structural classification" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    const replay = ReplaySummary{
        .total = 100,
        .identical = 100,
    };

    try std.testing.expectEqual(Classification.equivalent, diff.classifyWithReplay(&replay));
}

test "generateRecommendation equivalent" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    const rec = try generateRecommendation(allocator, .equivalent, &diff, null);
    defer allocator.free(rec);

    try std.testing.expect(std.mem.indexOf(u8, rec, "Deploy immediately") != null);
}

test "generateRecommendation breaking with diverged traces" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    const replay = ReplaySummary{
        .total = 100,
        .identical = 90,
        .status_changed = 5,
        .body_changed = 5,
    };

    const rec = try generateRecommendation(allocator, .breaking, &diff, &replay);
    defer allocator.free(rec);

    try std.testing.expect(std.mem.indexOf(u8, rec, "Requires review") != null);
    try std.testing.expect(std.mem.indexOf(u8, rec, "traces diverged") != null);
}

test "writeProofJson roundtrip" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    const diff = try diffContracts(allocator, &old, &new);

    const rec = try allocator.dupe(u8, "Deploy immediately - contract unchanged");

    var cert = ProofCertificate{
        .classification = .equivalent,
        .old_handler = "handler-v1.ts",
        .new_handler = "handler-v2.ts",
        .diff = diff,
        .replay = .{ .total = 50, .identical = 50 },
        .proof_level = .complete,
        .recommendation = rec,
    };
    defer cert.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeProofJson(&cert, &aw.writer);
    output = aw.toArrayList();

    const json = output.items;
    try std.testing.expect(std.mem.indexOf(u8, json, "\"classification\": \"equivalent\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"handler-v1.ts\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"handler-v2.ts\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"total_traces\": 50") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"proof_level\": \"complete\"") != null);
}

test "writeProofReport output" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);
    const pattern = try allocator.dupe(u8, "/api/v2");
    try new.routes.append(allocator, .{
        .pattern = pattern,
        .route_type = "exact",
        .field = "path",
        .status = 200,
        .content_type = "application/json",
        .aot = true,
    });

    const diff = try diffContracts(allocator, &old, &new);

    const rec = try allocator.dupe(u8, "Safe to deploy - 1 new route(s)");

    var cert = ProofCertificate{
        .classification = .additive,
        .old_handler = "handler-v1.ts",
        .new_handler = "handler-v2.ts",
        .diff = diff,
        .replay = .{ .total = 100, .identical = 100 },
        .proof_level = .complete,
        .recommendation = rec,
    };
    defer cert.deinit(allocator);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeProofReport(&aw.writer, &cert);
    output = aw.toArrayList();

    const report = output.items;
    try std.testing.expect(std.mem.indexOf(u8, report, "PROVEN EVOLUTION REPORT") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "ADDITIVE") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "+ ADDED    /api/v2") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "100 traces replayed") != null);
    try std.testing.expect(std.mem.indexOf(u8, report, "100 identical") != null);
}

test "diffBehaviors identical paths" {
    const allocator = std.testing.allocator;

    var cond_a: std.ArrayList(handler_contract.PathCondition) = .empty;
    defer cond_a.deinit(allocator);
    try cond_a.append(allocator, .{ .kind = .io_ok, .module = "auth", .func = "jwtVerify" });

    var io_a: std.ArrayList(handler_contract.PathIoCall) = .empty;
    defer io_a.deinit(allocator);
    try io_a.append(allocator, .{ .module = "auth", .func = "jwtVerify" });

    const path_a = handler_contract.BehaviorPath{
        .route_method = "GET",
        .route_pattern = "/users/:id",
        .conditions = cond_a,
        .io_sequence = io_a,
        .response_status = 200,
        .io_depth = 1,
        .is_failure_path = false,
    };

    const old_paths = [_]handler_contract.BehaviorPath{path_a};
    const new_paths = [_]handler_contract.BehaviorPath{path_a};

    var bd = try diffBehaviors(allocator, &old_paths, &new_paths);
    defer bd.deinit(allocator);

    try std.testing.expectEqual(@as(u32, 1), bd.preserved);
    try std.testing.expectEqual(@as(u32, 0), bd.removed);
    try std.testing.expectEqual(@as(u32, 0), bd.added);
    try std.testing.expectEqual(@as(u32, 0), bd.response_changed);
    try std.testing.expect(!bd.hasBreaking());
}

test "diffBehaviors removed path is breaking" {
    const allocator = std.testing.allocator;

    const path_a = handler_contract.BehaviorPath{
        .route_method = "GET",
        .route_pattern = "/health",
        .conditions = .empty,
        .io_sequence = .empty,
        .response_status = 200,
        .io_depth = 0,
        .is_failure_path = false,
    };

    const old_paths = [_]handler_contract.BehaviorPath{path_a};
    const new_paths = [_]handler_contract.BehaviorPath{};

    var bd = try diffBehaviors(allocator, &old_paths, &new_paths);
    defer bd.deinit(allocator);

    try std.testing.expectEqual(@as(u32, 0), bd.preserved);
    try std.testing.expectEqual(@as(u32, 1), bd.removed);
    try std.testing.expect(bd.hasBreaking());
}

test "diffBehaviors added path" {
    const allocator = std.testing.allocator;

    const path_a = handler_contract.BehaviorPath{
        .route_method = "POST",
        .route_pattern = "/users",
        .conditions = .empty,
        .io_sequence = .empty,
        .response_status = 201,
        .io_depth = 0,
        .is_failure_path = false,
    };

    const old_paths = [_]handler_contract.BehaviorPath{};
    const new_paths = [_]handler_contract.BehaviorPath{path_a};

    var bd = try diffBehaviors(allocator, &old_paths, &new_paths);
    defer bd.deinit(allocator);

    try std.testing.expectEqual(@as(u32, 0), bd.preserved);
    try std.testing.expectEqual(@as(u32, 1), bd.added);
    try std.testing.expect(!bd.hasBreaking());
}

// ---------------------------------------------------------------------------
// Canonical equivalence tests
// ---------------------------------------------------------------------------

/// Build a BehaviorPath owned by the caller. Mirrors the shape expected by
/// `diffContracts` and `tryCanonicalEquivalence`: everything deeply owned,
/// no aliasing with test-fixture literals.
fn makeOwnedPath(
    allocator: std.mem.Allocator,
    method: []const u8,
    pattern: []const u8,
    status: u16,
    calls: []const struct {
        module: []const u8,
        func: []const u8,
        args: ?[]const u8,
    },
) !handler_contract.BehaviorPath {
    var seq: std.ArrayList(handler_contract.PathIoCall) = .empty;
    errdefer {
        for (seq.items) |*c| @constCast(c).deinit(allocator);
        seq.deinit(allocator);
    }
    for (calls) |c| {
        try seq.append(allocator, .{
            .module = try allocator.dupe(u8, c.module),
            .func = try allocator.dupe(u8, c.func),
            .arg_signature = if (c.args) |a| try allocator.dupe(u8, a) else null,
        });
    }

    return .{
        .route_method = try allocator.dupe(u8, method),
        .route_pattern = try allocator.dupe(u8, pattern),
        .conditions = .empty,
        .io_sequence = seq,
        .response_status = status,
        .io_depth = @intCast(calls.len),
        .is_failure_path = false,
    };
}

test "diffContracts upgrades equivalent to equivalent_modulo_laws when dedup fires" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    // Old: handler reads env("KEY") once.
    try old.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/data", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY" },
    }));
    // New: handler reads env("KEY") twice - canonicalization collapses to one.
    try new.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/data", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY" },
        .{ .module = "env", .func = "env", .args = "lit:KEY" },
    }));

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.equivalent_modulo_laws, diff.classify());
    try std.testing.expect(diff.laws_used.items.len >= 1);

    // Verify law identifier format: "<module>.<func>.<law_kind>"
    var found_env_pure = false;
    for (diff.laws_used.items) |id| {
        if (std.mem.eql(u8, id, "env.env.pure")) found_env_pure = true;
    }
    try std.testing.expect(found_env_pure);
}

test "diffContracts stays equivalent when canonical forms already match" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    // Identical single-call paths - no canonicalization work, no laws fire.
    try old.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/data", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY" },
    }));
    try new.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/data", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY" },
    }));

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    // Canonicalization ran but no rewrites fired, so we stay at plain equivalent.
    try std.testing.expectEqual(Classification.equivalent, diff.classify());
    try std.testing.expectEqual(@as(usize, 0), diff.laws_used.items.len);
}

test "diffContracts does not reclassify when canonical forms differ" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    // Different calls - canonicalization cannot make them match.
    try old.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/data", 200, &.{
        .{ .module = "crypto", .func = "sha256", .args = "lit:A" },
    }));
    try new.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/data", 200, &.{
        .{ .module = "crypto", .func = "sha256", .args = "lit:B" },
    }));

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.equivalent, diff.classify());
    try std.testing.expectEqual(@as(usize, 0), diff.laws_used.items.len);
    // Structurally-matched paths that fail canonical equality record
    // a counterexample pointing at the divergence.
    try std.testing.expect(diff.canonical_counterexample != null);
}

test "diffContracts reclassifies cacheSet idempotent collapse" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    try old.behaviors.append(allocator, try makeOwnedPath(allocator, "POST", "/session", 201, &.{
        .{ .module = "cache", .func = "cacheSet", .args = "lit:sessions|lit:k|lit:v|?" },
    }));
    try new.behaviors.append(allocator, try makeOwnedPath(allocator, "POST", "/session", 201, &.{
        .{ .module = "cache", .func = "cacheSet", .args = "lit:sessions|lit:k|lit:v|?" },
        .{ .module = "cache", .func = "cacheSet", .args = "lit:sessions|lit:k|lit:v|?" },
    }));

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.equivalent_modulo_laws, diff.classify());
    var found_cache = false;
    for (diff.laws_used.items) |id| {
        if (std.mem.eql(u8, id, "cache.cacheSet.idempotent_call")) found_cache = true;
    }
    try std.testing.expect(found_cache);
}

test "diffContracts reclassifies when multiple paths match in any order" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    // Old side: /a followed by /b, each with one env read.
    try old.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/a", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY_A" },
    }));
    try old.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/b", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY_B" },
    }));

    // New side: same routes in reversed order, and /a has a redundant
    // env read that must collapse under canonicalization.
    try new.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/b", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY_B" },
    }));
    try new.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/a", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY_A" },
        .{ .module = "env", .func = "env", .args = "lit:KEY_A" },
    }));

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    try std.testing.expectEqual(Classification.equivalent_modulo_laws, diff.classify());
    try std.testing.expect(diff.laws_used.items.len >= 1);
}

test "diffContracts does not upgrade when surface differs" {
    const allocator = std.testing.allocator;

    var old = try makeTestContract(allocator);
    defer old.deinit(allocator);
    var new = try makeTestContract(allocator);
    defer new.deinit(allocator);

    // Add an env var only to the new side - surface change -> additive.
    try new.env.literal.append(allocator, try allocator.dupe(u8, "NEW_KEY"));

    try old.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/data", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY" },
    }));
    try new.behaviors.append(allocator, try makeOwnedPath(allocator, "GET", "/data", 200, &.{
        .{ .module = "env", .func = "env", .args = "lit:KEY" },
        .{ .module = "env", .func = "env", .args = "lit:KEY" },
    }));

    var diff = try diffContracts(allocator, &old, &new);
    defer diff.deinit(allocator);

    // Surface is additive, so canonicalization does not try to upgrade.
    try std.testing.expectEqual(Classification.additive, diff.classify());
    try std.testing.expectEqual(@as(usize, 0), diff.laws_used.items.len);
}
