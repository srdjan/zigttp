//! Manifest Alignment Tool
//!
//! Cross-references a governance manifest (JSON) against a compiler-proven
//! handler contract. Detects mismatches between what the manifest declares
//! (routes, SQL tables, env vars) and what the contract actually proves the
//! handler uses.
//!
//! Architecture: parseManifest (JSON) + checkAlignment (contract) -> AlignmentResult[]
//!
//! Called by precompile.zig when --manifest is passed alongside --contract.

const std = @import("std");
const zigts = @import("zigts");
const routePatternsMatch = zigts.route_match.pathsMatch;
const handler_contract = zigts.handler_contract;
const HandlerContract = handler_contract.HandlerContract;

// -------------------------------------------------------------------------
// Alignment types
// -------------------------------------------------------------------------

pub const AlignmentStatus = enum {
    pass,
    warn,
    fail,

    pub fn toString(self: AlignmentStatus) []const u8 {
        return switch (self) {
            .pass => "pass",
            .warn => "warn",
            .fail => "fail",
        };
    }
};

pub const AlignmentResult = struct {
    section: []const u8, // static literal, not owned
    status: AlignmentStatus,
    declared_count: u32,
    matched_count: u32,
    unmatched: []const []const u8, // declared but not in contract (owned slice, entries borrowed)
    undeclared: []const []const u8, // in contract but not declared (owned slice, entries borrowed)

    pub fn deinit(self: *AlignmentResult, allocator: std.mem.Allocator) void {
        allocator.free(self.unmatched);
        allocator.free(self.undeclared);
    }
};

pub const ManifestAlignment = struct {
    results: []AlignmentResult,
    overall: AlignmentStatus,

    pub fn deinit(self: *ManifestAlignment, allocator: std.mem.Allocator) void {
        for (self.results) |*result| result.deinit(allocator);
        allocator.free(self.results);
    }
};

// -------------------------------------------------------------------------
// Parsed manifest
// -------------------------------------------------------------------------

pub const ManifestRoute = struct {
    path: []const u8,
    method: []const u8,
};

pub const ParsedManifest = struct {
    version: u32,
    routes: []ManifestRoute,
    sql_tables: [][]const u8,
    env_vars: [][]const u8,

    // All data is owned by the parsed JSON value; keep it alive.
    parsed: std.json.Parsed(std.json.Value),

    pub fn deinit(self: *ParsedManifest, allocator: std.mem.Allocator) void {
        allocator.free(self.routes);
        allocator.free(self.sql_tables);
        allocator.free(self.env_vars);
        self.parsed.deinit();
    }
};

// -------------------------------------------------------------------------
// Manifest parsing
// -------------------------------------------------------------------------

/// Parse a governance manifest JSON file into a ParsedManifest.
/// Caller must keep the returned value alive while using its string data.
/// Unknown fields are silently ignored.
pub fn parseManifest(allocator: std.mem.Allocator, json_bytes: []const u8) !ParsedManifest {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_bytes, .{});
    errdefer parsed.deinit();

    if (parsed.value != .object) return error.InvalidManifest;
    const root = parsed.value.object;

    // Version (optional, default 2)
    const version: u32 = blk: {
        const v = root.get("manifestVersion") orelse break :blk 2;
        if (v != .integer) break :blk 2;
        break :blk @intCast(@max(0, v.integer));
    };

    // Routes
    const routes: []ManifestRoute = blk: {
        const arr = root.get("routes") orelse break :blk try allocator.alloc(ManifestRoute, 0);
        if (arr != .array) break :blk try allocator.alloc(ManifestRoute, 0);
        var list: std.ArrayList(ManifestRoute) = .empty;
        errdefer list.deinit(allocator);
        for (arr.array.items) |item| {
            if (item != .object) continue;
            const obj = item.object;
            const path = strOrNull(obj.get("path")) orelse continue;
            const method = strOrNull(obj.get("method")) orelse "ANY";
            try list.append(allocator, .{ .path = path, .method = method });
        }
        break :blk try list.toOwnedSlice(allocator);
    };
    errdefer allocator.free(routes);

    // SQL tables
    const sql_tables: [][]const u8 = blk: {
        const arr = root.get("sqlTables") orelse break :blk try allocator.alloc([]const u8, 0);
        if (arr != .array) break :blk try allocator.alloc([]const u8, 0);
        var list: std.ArrayList([]const u8) = .empty;
        errdefer list.deinit(allocator);
        for (arr.array.items) |item| {
            const s = strOrNull(item) orelse continue;
            try list.append(allocator, s);
        }
        break :blk try list.toOwnedSlice(allocator);
    };
    errdefer allocator.free(sql_tables);

    // Env vars
    const env_vars: [][]const u8 = blk: {
        const arr = root.get("envVars") orelse break :blk try allocator.alloc([]const u8, 0);
        if (arr != .array) break :blk try allocator.alloc([]const u8, 0);
        var list: std.ArrayList([]const u8) = .empty;
        errdefer list.deinit(allocator);
        for (arr.array.items) |item| {
            const s = strOrNull(item) orelse continue;
            try list.append(allocator, s);
        }
        break :blk try list.toOwnedSlice(allocator);
    };
    errdefer allocator.free(env_vars);

    return .{
        .version = version,
        .routes = routes,
        .sql_tables = sql_tables,
        .env_vars = env_vars,
        .parsed = parsed,
    };
}

fn strOrNull(val: ?std.json.Value) ?[]const u8 {
    const v = val orelse return null;
    if (v != .string) return null;
    return v.string;
}

// -------------------------------------------------------------------------
// Alignment check
// -------------------------------------------------------------------------

/// Cross-reference a parsed manifest against a handler contract.
/// Returns alignment results per section (routes, sql, env) and an overall status.
///
/// Strings in AlignmentResult.unmatched and .undeclared point into the manifest
/// or contract data. The caller must keep both alive while using the result.
pub fn checkAlignment(
    allocator: std.mem.Allocator,
    manifest: *const ParsedManifest,
    contract: *const HandlerContract,
) !ManifestAlignment {
    var results: std.ArrayList(AlignmentResult) = .empty;
    errdefer {
        for (results.items) |*r| r.deinit(allocator);
        results.deinit(allocator);
    }

    try results.append(allocator, try checkRoutes(allocator, manifest, contract));
    try results.append(allocator, try checkSqlTables(allocator, manifest, contract));
    try results.append(allocator, try checkEnvVars(allocator, manifest, contract));

    // Overall: fail if any section fails, warn if any warns, pass otherwise.
    var overall: AlignmentStatus = .pass;
    for (results.items) |result| {
        switch (result.status) {
            .fail => {
                overall = .fail;
                break;
            },
            .warn => overall = .warn,
            .pass => {},
        }
    }

    return .{
        .results = try results.toOwnedSlice(allocator),
        .overall = overall,
    };
}

fn checkRoutes(
    allocator: std.mem.Allocator,
    manifest: *const ParsedManifest,
    contract: *const HandlerContract,
) !AlignmentResult {
    var matched: u32 = 0;
    var unmatched_list: std.ArrayList([]const u8) = .empty;
    errdefer unmatched_list.deinit(allocator);
    var undeclared_list: std.ArrayList([]const u8) = .empty;
    errdefer undeclared_list.deinit(allocator);

    // For each manifest route, check if the contract has a matching pattern.
    for (manifest.routes) |manifest_route| {
        const found = routeExistsInContract(manifest_route.path, contract);
        if (found) {
            matched += 1;
        } else {
            try unmatched_list.append(allocator, manifest_route.path);
        }
    }

    // For each contract route, check if the manifest declares it.
    for (contract.routes.items) |contract_route| {
        const found = routeExistsInManifest(contract_route.pattern, manifest);
        if (!found) {
            try undeclared_list.append(allocator, contract_route.pattern);
        }
    }

    const unmatched = try unmatched_list.toOwnedSlice(allocator);
    errdefer allocator.free(unmatched);
    const undeclared = try undeclared_list.toOwnedSlice(allocator);

    const status: AlignmentStatus =
        if (unmatched.len > 0) .fail else if (undeclared.len > 0) .warn else .pass;

    return .{
        .section = "routes",
        .status = status,
        .declared_count = @intCast(manifest.routes.len),
        .matched_count = matched,
        .unmatched = unmatched,
        .undeclared = undeclared,
    };
}

fn checkSqlTables(
    allocator: std.mem.Allocator,
    manifest: *const ParsedManifest,
    contract: *const HandlerContract,
) !AlignmentResult {
    var matched: u32 = 0;
    var unmatched_list: std.ArrayList([]const u8) = .empty;
    errdefer unmatched_list.deinit(allocator);
    var undeclared_list: std.ArrayList([]const u8) = .empty;
    errdefer undeclared_list.deinit(allocator);

    // For each manifest table, check if any contract SQL query mentions it.
    for (manifest.sql_tables) |table| {
        const found = tableExistsInContract(table, contract);
        if (found) {
            matched += 1;
        } else {
            try unmatched_list.append(allocator, table);
        }
    }

    // Collect all unique tables from contract queries, check against manifest.
    for (contract.sql.queries.items) |query| {
        for (query.tables.items) |contract_table| {
            if (!tableExistsInManifest(contract_table, manifest)) {
                // Avoid duplicate entries in undeclared list.
                if (!sliceContains(undeclared_list.items, contract_table)) {
                    try undeclared_list.append(allocator, contract_table);
                }
            }
        }
    }

    const unmatched = try unmatched_list.toOwnedSlice(allocator);
    errdefer allocator.free(unmatched);
    const undeclared = try undeclared_list.toOwnedSlice(allocator);

    const status: AlignmentStatus =
        if (unmatched.len > 0) .fail else if (undeclared.len > 0) .warn else .pass;

    return .{
        .section = "sql",
        .status = status,
        .declared_count = @intCast(manifest.sql_tables.len),
        .matched_count = matched,
        .unmatched = unmatched,
        .undeclared = undeclared,
    };
}

fn checkEnvVars(
    allocator: std.mem.Allocator,
    manifest: *const ParsedManifest,
    contract: *const HandlerContract,
) !AlignmentResult {
    var matched: u32 = 0;
    var unmatched_list: std.ArrayList([]const u8) = .empty;
    errdefer unmatched_list.deinit(allocator);
    var undeclared_list: std.ArrayList([]const u8) = .empty;
    errdefer undeclared_list.deinit(allocator);

    // For each manifest env var, check if contract.env.literal contains it.
    for (manifest.env_vars) |env_var| {
        const found = envExistsInContract(env_var, contract);
        if (found) {
            matched += 1;
        } else {
            try unmatched_list.append(allocator, env_var);
        }
    }

    // For each contract env var, check if manifest declares it.
    for (contract.env.literal.items) |contract_env| {
        if (!envExistsInManifest(contract_env, manifest)) {
            try undeclared_list.append(allocator, contract_env);
        }
    }

    const unmatched = try unmatched_list.toOwnedSlice(allocator);
    errdefer allocator.free(unmatched);
    const undeclared = try undeclared_list.toOwnedSlice(allocator);

    const status: AlignmentStatus =
        if (unmatched.len > 0) .fail else if (undeclared.len > 0) .warn else .pass;

    return .{
        .section = "env",
        .status = status,
        .declared_count = @intCast(manifest.env_vars.len),
        .matched_count = matched,
        .unmatched = unmatched,
        .undeclared = undeclared,
    };
}

// -------------------------------------------------------------------------
// Route pattern normalization and matching
// -------------------------------------------------------------------------

/// Normalize a route pattern segment for comparison.

fn routeExistsInContract(manifest_path: []const u8, contract: *const HandlerContract) bool {
    for (contract.routes.items) |route| {
        if (routePatternsMatch(manifest_path, route.pattern)) return true;
    }
    return false;
}

fn routeExistsInManifest(contract_pattern: []const u8, manifest: *const ParsedManifest) bool {
    for (manifest.routes) |route| {
        if (routePatternsMatch(contract_pattern, route.path)) return true;
    }
    return false;
}

fn tableExistsInContract(table: []const u8, contract: *const HandlerContract) bool {
    for (contract.sql.queries.items) |query| {
        for (query.tables.items) |contract_table| {
            if (std.mem.eql(u8, table, contract_table)) return true;
        }
    }
    return false;
}

fn tableExistsInManifest(table: []const u8, manifest: *const ParsedManifest) bool {
    for (manifest.sql_tables) |manifest_table| {
        if (std.mem.eql(u8, table, manifest_table)) return true;
    }
    return false;
}

fn envExistsInContract(env_var: []const u8, contract: *const HandlerContract) bool {
    for (contract.env.literal.items) |contract_env| {
        if (std.mem.eql(u8, env_var, contract_env)) return true;
    }
    return false;
}

fn envExistsInManifest(env_var: []const u8, manifest: *const ParsedManifest) bool {
    for (manifest.env_vars) |manifest_env| {
        if (std.mem.eql(u8, env_var, manifest_env)) return true;
    }
    return false;
}

fn sliceContains(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |item| {
        if (std.mem.eql(u8, item, needle)) return true;
    }
    return false;
}

// -------------------------------------------------------------------------
// JSON output
// -------------------------------------------------------------------------

const writeJsonString = handler_contract.writeJsonString;
const writeJsonStringContent = handler_contract.writeJsonStringContent;

/// Serialize alignment results to JSON and write to a file path.
pub fn writeAlignmentJson(
    allocator: std.mem.Allocator,
    alignment: *const ManifestAlignment,
    output_path: []const u8,
) !void {
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
    const w = &aw.writer;

    try w.writeAll("{\n");
    try w.writeAll("  \"overall\": ");
    try writeJsonString(w, alignment.overall.toString());
    try w.writeAll(",\n");

    try w.writeAll("  \"sections\": [");
    for (alignment.results, 0..) |result, i| {
        if (i > 0) try w.writeAll(",");
        try w.writeAll("\n    {\n");

        try w.writeAll("      \"section\": ");
        try writeJsonString(w, result.section);
        try w.writeAll(",\n");

        try w.writeAll("      \"status\": ");
        try writeJsonString(w, result.status.toString());
        try w.writeAll(",\n");

        try w.print("      \"declaredCount\": {d},\n", .{result.declared_count});
        try w.print("      \"matchedCount\": {d},\n", .{result.matched_count});

        try w.writeAll("      \"unmatched\": [");
        for (result.unmatched, 0..) |item, j| {
            if (j > 0) try w.writeAll(", ");
            try writeJsonString(w, item);
        }
        try w.writeAll("],\n");

        try w.writeAll("      \"undeclared\": [");
        for (result.undeclared, 0..) |item, j| {
            if (j > 0) try w.writeAll(", ");
            try writeJsonString(w, item);
        }
        try w.writeAll("]\n");

        try w.writeAll("    }");
    }
    if (alignment.results.len > 0) {
        try w.writeAll("\n  ");
    }
    try w.writeAll("]\n");
    try w.writeAll("}\n");

    output = aw.toArrayList();
    try zigts.file_io.writeFile(allocator, output_path, output.items);
}

// -------------------------------------------------------------------------
// Human-readable summary (stderr)
// -------------------------------------------------------------------------

/// Print a human-readable alignment summary to stderr.
pub fn printAlignmentSummary(alignment: *const ManifestAlignment) void {
    std.debug.print("Manifest Alignment: {s}\n", .{alignment.overall.toString()});

    for (alignment.results) |result| {
        const icon: []const u8 = switch (result.status) {
            .pass => "OK",
            .warn => "WARN",
            .fail => "FAIL",
        };
        std.debug.print("  [{s}] {s}: {d}/{d} matched\n", .{
            icon,
            result.section,
            result.matched_count,
            result.declared_count,
        });

        for (result.unmatched) |item| {
            std.debug.print("    - declared but missing from contract: {s}\n", .{item});
        }
        for (result.undeclared) |item| {
            std.debug.print("    - in contract but not declared: {s}\n", .{item});
        }
    }
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "parseManifest: valid manifest" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\  "manifestVersion": 2,
        \\  "routes": [
        \\    {"path": "/api/orders", "method": "GET"},
        \\    {"path": "/api/orders/:id", "method": "POST"}
        \\  ],
        \\  "sqlTables": ["orders", "_governance_states"],
        \\  "envVars": ["PORT", "DB_PATH"],
        \\  "unknownField": true
        \\}
    ;

    var manifest = try parseManifest(allocator, json);
    defer manifest.deinit(allocator);

    try std.testing.expectEqual(@as(u32, 2), manifest.version);
    try std.testing.expectEqual(@as(usize, 2), manifest.routes.len);
    try std.testing.expectEqualStrings("/api/orders", manifest.routes[0].path);
    try std.testing.expectEqualStrings("GET", manifest.routes[0].method);
    try std.testing.expectEqualStrings("/api/orders/:id", manifest.routes[1].path);
    try std.testing.expectEqual(@as(usize, 2), manifest.sql_tables.len);
    try std.testing.expectEqualStrings("orders", manifest.sql_tables[0]);
    try std.testing.expectEqual(@as(usize, 2), manifest.env_vars.len);
    try std.testing.expectEqualStrings("PORT", manifest.env_vars[0]);
}

test "parseManifest: empty manifest" {
    const allocator = std.testing.allocator;
    const json = "{}";

    var manifest = try parseManifest(allocator, json);
    defer manifest.deinit(allocator);

    try std.testing.expectEqual(@as(u32, 2), manifest.version);
    try std.testing.expectEqual(@as(usize, 0), manifest.routes.len);
    try std.testing.expectEqual(@as(usize, 0), manifest.sql_tables.len);
    try std.testing.expectEqual(@as(usize, 0), manifest.env_vars.len);
}

test "parseManifest: not an object" {
    const allocator = std.testing.allocator;
    const result = parseManifest(allocator, "[]");
    try std.testing.expectError(error.InvalidManifest, result);
}

test "routePatternsMatch: identical static" {
    try std.testing.expect(routePatternsMatch("/api/orders", "/api/orders"));
}

test "routePatternsMatch: colon vs brace params" {
    try std.testing.expect(routePatternsMatch("/api/orders/:id", "/api/orders/{id}"));
    try std.testing.expect(routePatternsMatch("/api/orders/{id}", "/api/orders/:id"));
}

test "routePatternsMatch: different param names match" {
    try std.testing.expect(routePatternsMatch("/api/orders/:orderId", "/api/orders/:id"));
}

test "routePatternsMatch: different paths do not match" {
    try std.testing.expect(!routePatternsMatch("/api/orders", "/api/users"));
}

test "routePatternsMatch: different segment counts" {
    try std.testing.expect(!routePatternsMatch("/api/orders", "/api/orders/:id"));
}
