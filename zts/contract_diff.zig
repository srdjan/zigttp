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
const HandlerContract = handler_contract.HandlerContract;
const RouteInfo = handler_contract.RouteInfo;

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
    /// New routes/capabilities added, existing preserved. No regressions.
    additive,
    /// Routes/capabilities removed OR traces diverged. Needs review.
    breaking,

    pub fn toString(self: Classification) []const u8 {
        return switch (self) {
            .equivalent => "equivalent",
            .additive => "additive",
            .breaking => "breaking",
        };
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

pub const DynamicChange = struct {
    section: []const u8,
    old_dynamic: bool,
    new_dynamic: bool,
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
    env_changes: std.ArrayList(ItemChange),
    egress_changes: std.ArrayList(ItemChange),
    cache_changes: std.ArrayList(ItemChange),
    sql_changes: std.ArrayList(ItemChange),
    dynamic_changes: std.ArrayList(DynamicChange),

    pub fn deinit(self: *ContractDiff, allocator: std.mem.Allocator) void {
        self.routes.deinit(allocator);
        self.env_changes.deinit(allocator);
        self.egress_changes.deinit(allocator);
        self.cache_changes.deinit(allocator);
        self.sql_changes.deinit(allocator);
        self.dynamic_changes.deinit(allocator);
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
    /// Single pass over each change list, checking for removed and added items.
    pub fn classify(self: *const ContractDiff) Classification {
        var has_added = false;

        for (self.routes.items) |r| {
            if (r.status == .removed) return .breaking;
            if (r.status == .added) has_added = true;
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
};

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

    return .{
        .routes = routes,
        .env_changes = env_changes,
        .egress_changes = egress_changes,
        .cache_changes = cache_changes,
        .sql_changes = sql_changes,
        .dynamic_changes = dynamic_changes,
    };
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
        .additive => try writer.writeAll("ADDITIVE (safe to deploy)\n"),
        .breaking => try writer.writeAll("BREAKING (requires review)\n"),
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

fn containsString(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |item| {
        if (std.mem.eql(u8, item, needle)) return true;
    }
    return false;
}

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
