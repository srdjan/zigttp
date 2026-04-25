//! Capability policy parsing, validation, and runtime views.
//!
//! Policies are opt-in and restrict precompiled handlers to an explicit set of
//! env vars, outbound hosts, cache namespaces, and SQL query names.

const std = @import("std");
const contract_mod = @import("handler_contract.zig");

const HandlerContract = contract_mod.HandlerContract;
const ascii = std.ascii;

pub const AllowList = struct {
    values: std.ArrayListUnmanaged([]const u8) = .empty,

    pub fn deinit(self: *AllowList, allocator: std.mem.Allocator) void {
        for (self.values.items) |item| {
            allocator.free(item);
        }
        self.values.deinit(allocator);
    }

    fn appendUnique(self: *AllowList, allocator: std.mem.Allocator, item: []const u8) !void {
        if (self.contains(item, false)) return;
        try self.values.append(allocator, try allocator.dupe(u8, item));
    }

    fn contains(self: *const AllowList, candidate: []const u8, case_insensitive: bool) bool {
        for (self.values.items) |item| {
            const matches = if (case_insensitive)
                ascii.eqlIgnoreCase(item, candidate)
            else
                std.mem.eql(u8, item, candidate);
            if (matches) return true;
        }
        return false;
    }
};

pub const HandlerPolicy = struct {
    env: ?AllowList = null,
    egress: ?AllowList = null,
    cache: ?AllowList = null,
    sql: ?AllowList = null,

    pub fn deinit(self: *HandlerPolicy, allocator: std.mem.Allocator) void {
        if (self.env) |*section| section.deinit(allocator);
        if (self.egress) |*section| section.deinit(allocator);
        if (self.cache) |*section| section.deinit(allocator);
        if (self.sql) |*section| section.deinit(allocator);
    }
};

pub const RuntimeAllowList = struct {
    enabled: bool = false,
    values: []const []const u8 = &.{},

    pub fn allows(self: RuntimeAllowList, candidate: []const u8) bool {
        if (!self.enabled) return true;
        for (self.values) |item| {
            if (std.mem.eql(u8, item, candidate)) return true;
        }
        return false;
    }

    pub fn allowsCaseInsensitive(self: RuntimeAllowList, candidate: []const u8) bool {
        if (!self.enabled) return true;
        for (self.values) |item| {
            if (ascii.eqlIgnoreCase(item, candidate)) return true;
        }
        return false;
    }
};

pub const RuntimeSqlAllowList = struct {
    enabled: bool = false,
    values: []const []const u8 = &.{},
    queries: []const contract_mod.SqlQueryInfo = &.{},

    pub fn allows(self: RuntimeSqlAllowList, candidate: []const u8) bool {
        if (!self.enabled) return true;
        for (self.values) |value| {
            if (std.mem.eql(u8, value, candidate)) return true;
        }
        for (self.queries) |query| {
            if (std.mem.eql(u8, query.name, candidate)) return true;
        }
        return false;
    }
};

pub const RuntimePolicy = struct {
    env: RuntimeAllowList = .{},
    egress: RuntimeAllowList = .{},
    cache: RuntimeAllowList = .{},
    sql: RuntimeSqlAllowList = .{},

    pub fn allowsEnv(self: RuntimePolicy, key: []const u8) bool {
        return self.env.allows(key);
    }

    pub fn allowsEgressHost(self: RuntimePolicy, host: []const u8) bool {
        return self.egress.allowsCaseInsensitive(host);
    }

    pub fn allowsCacheNamespace(self: RuntimePolicy, ns: []const u8) bool {
        return self.cache.allows(ns);
    }

    pub fn allowsSqlQuery(self: RuntimePolicy, name: []const u8) bool {
        return self.sql.allows(name);
    }

    /// Same backing list as allowsSqlQuery in Phase 1 - the split lets the
    /// db.read vs db.write policy actions diverge later without a contract
    /// migration. See docs/zigttp_zigts_policy_wasm_spec.md section 7.3.
    pub fn allowsSqlWrite(self: RuntimePolicy, name: []const u8) bool {
        return self.sql.allows(name);
    }
};

/// Convert a HandlerContract's proven sections into a RuntimePolicy.
/// When `dynamic: false`, the compiler proved ALL access uses literal strings -
/// restrict to exactly those. When `dynamic: true`, some access is computed -
/// leave that section permissive.
///
/// The returned policy borrows string data from the contract. The contract
/// must outlive the policy. For precompilation this is fine because the data
/// gets embedded as compile-time constants in the generated .zig file.
pub fn contractToRuntimePolicy(contract: *const HandlerContract) RuntimePolicy {
    return .{
        .env = if (!contract.env.dynamic and contract.env.literal.items.len > 0)
            .{ .enabled = true, .values = contract.env.literal.items }
        else if (!contract.env.dynamic)
            .{ .enabled = true, .values = &.{} }
        else
            .{},
        .egress = if (!contract.egress.dynamic and contract.egress.hosts.items.len > 0)
            .{ .enabled = true, .values = contract.egress.hosts.items }
        else if (!contract.egress.dynamic)
            .{ .enabled = true, .values = &.{} }
        else
            .{},
        .cache = if (!contract.cache.dynamic and contract.cache.namespaces.items.len > 0)
            .{ .enabled = true, .values = contract.cache.namespaces.items }
        else if (!contract.cache.dynamic)
            .{ .enabled = true, .values = &.{} }
        else
            .{},
        .sql = if (!contract.sql.dynamic and contract.sql.queries.items.len > 0)
            .{ .enabled = true, .queries = contract.sql.queries.items }
        else if (!contract.sql.dynamic)
            .{ .enabled = true, .queries = &.{} }
        else
            .{},
    };
}

pub const PolicyCategory = enum {
    env,
    egress,
    cache,
    sql,
};

pub const ViolationKind = enum {
    literal_not_allowed,
    dynamic_not_allowed,
};

pub const PolicyViolation = struct {
    category: PolicyCategory,
    kind: ViolationKind,
    value: ?[]const u8 = null,
    introduced_by_patch: bool = false,
};

pub const ValidationReport = struct {
    violations: std.ArrayListUnmanaged(PolicyViolation) = .empty,

    pub fn deinit(self: *ValidationReport, allocator: std.mem.Allocator) void {
        self.violations.deinit(allocator);
    }

    pub fn hasViolations(self: *const ValidationReport) bool {
        return self.violations.items.len > 0;
    }
};

pub fn parsePolicyJson(allocator: std.mem.Allocator, source: []const u8) !HandlerPolicy {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, source, .{});
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidPolicy;

    const root = parsed.value.object;
    var iter = root.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*;
        if (!std.mem.eql(u8, key, "env") and
            !std.mem.eql(u8, key, "egress") and
            !std.mem.eql(u8, key, "cache") and
            !std.mem.eql(u8, key, "sql"))
        {
            return error.InvalidPolicy;
        }
    }

    return .{
        .env = try parseSection(allocator, root, "env", "allow"),
        .egress = try parseSection(allocator, root, "egress", "allow_hosts"),
        .cache = try parseSection(allocator, root, "cache", "allow_namespaces"),
        .sql = try parseSection(allocator, root, "sql", "allow_queries"),
    };
}

pub fn validateContract(
    allocator: std.mem.Allocator,
    contract: *const HandlerContract,
    policy: *const HandlerPolicy,
) !ValidationReport {
    var report = ValidationReport{};
    errdefer report.deinit(allocator);

    if (policy.env) |section| {
        for (contract.env.literal.items) |item| {
            if (!section.contains(item, false)) {
                try report.violations.append(allocator, .{
                    .category = .env,
                    .kind = .literal_not_allowed,
                    .value = item,
                });
            }
        }
        if (contract.env.dynamic) {
            try report.violations.append(allocator, .{
                .category = .env,
                .kind = .dynamic_not_allowed,
            });
        }
    }

    if (policy.egress) |section| {
        for (contract.egress.hosts.items) |item| {
            if (!section.contains(item, true)) {
                try report.violations.append(allocator, .{
                    .category = .egress,
                    .kind = .literal_not_allowed,
                    .value = item,
                });
            }
        }
        if (contract.egress.dynamic) {
            try report.violations.append(allocator, .{
                .category = .egress,
                .kind = .dynamic_not_allowed,
            });
        }
    }

    if (policy.cache) |section| {
        for (contract.cache.namespaces.items) |item| {
            if (!section.contains(item, false)) {
                try report.violations.append(allocator, .{
                    .category = .cache,
                    .kind = .literal_not_allowed,
                    .value = item,
                });
            }
        }
        if (contract.cache.dynamic) {
            try report.violations.append(allocator, .{
                .category = .cache,
                .kind = .dynamic_not_allowed,
            });
        }
    }

    if (policy.sql) |section| {
        for (contract.sql.queries.items) |query| {
            if (!section.contains(query.name, false)) {
                try report.violations.append(allocator, .{
                    .category = .sql,
                    .kind = .literal_not_allowed,
                    .value = query.name,
                });
            }
        }
        if (contract.sql.dynamic) {
            try report.violations.append(allocator, .{
                .category = .sql,
                .kind = .dynamic_not_allowed,
            });
        }
    }

    return report;
}

pub fn formatViolations(report: *const ValidationReport, writer: anytype) !void {
    for (report.violations.items) |violation| {
        switch (violation.kind) {
            .literal_not_allowed => {
                try writer.print(
                    "policy violation: {s} '{s}' is not allowed\n",
                    .{ categoryLiteralLabel(violation.category), violation.value.? },
                );
            },
            .dynamic_not_allowed => {
                try writer.print(
                    "policy violation: dynamic {s} access is not allowed\n",
                    .{categoryDynamicLabel(violation.category)},
                );
            },
        }
    }
}

fn parseSection(
    allocator: std.mem.Allocator,
    root: std.json.ObjectMap,
    section_name: []const u8,
    field_name: []const u8,
) !?AllowList {
    const raw_section = root.get(section_name) orelse return null;
    if (raw_section != .object) return error.InvalidPolicy;

    const section_obj = raw_section.object;
    var iter = section_obj.iterator();
    while (iter.next()) |entry| {
        if (!std.mem.eql(u8, entry.key_ptr.*, field_name)) return error.InvalidPolicy;
    }

    const raw_allow = section_obj.get(field_name) orelse return error.InvalidPolicy;
    if (raw_allow != .array) return error.InvalidPolicy;

    var section = AllowList{};
    errdefer section.deinit(allocator);

    for (raw_allow.array.items) |item| {
        if (item != .string or item.string.len == 0) return error.InvalidPolicy;
        try section.appendUnique(allocator, item.string);
    }

    return section;
}

fn categoryLiteralLabel(category: PolicyCategory) []const u8 {
    return switch (category) {
        .env => "env var",
        .egress => "outbound host",
        .cache => "cache namespace",
        .sql => "sql query",
    };
}

fn categoryDynamicLabel(category: PolicyCategory) []const u8 {
    return switch (category) {
        .env => "env",
        .egress => "outbound host",
        .cache => "cache namespace",
        .sql => "sql query",
    };
}

test "parse policy json with all sections" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "env": { "allow": ["JWT_SECRET", "API_KEY"] },
        \\  "egress": { "allow_hosts": ["api.example.com"] },
        \\  "cache": { "allow_namespaces": ["sessions"] }
        \\}
    ;

    var policy = try parsePolicyJson(allocator, source);
    defer policy.deinit(allocator);

    try std.testing.expect(policy.env != null);
    try std.testing.expect(policy.egress != null);
    try std.testing.expect(policy.cache != null);
    try std.testing.expectEqual(@as(usize, 2), policy.env.?.values.items.len);
    try std.testing.expectEqualStrings("api.example.com", policy.egress.?.values.items[0]);
    try std.testing.expectEqualStrings("sessions", policy.cache.?.values.items[0]);
}

test "parse policy json rejects unknown sections" {
    const allocator = std.testing.allocator;
    const source =
        \\{
        \\  "modules": { "allow": ["zigttp:env"] }
        \\}
    ;

    try std.testing.expectError(error.InvalidPolicy, parsePolicyJson(allocator, source));
}

test "validate contract rejects disallowed literals and dynamic access" {
    const allocator = std.testing.allocator;

    const path = try allocator.dupe(u8, "handler.ts");
    var env_literals: std.ArrayList([]const u8) = .empty;
    try env_literals.append(allocator, try allocator.dupe(u8, "JWT_SECRET"));
    var hosts: std.ArrayList([]const u8) = .empty;
    try hosts.append(allocator, try allocator.dupe(u8, "api.example.com"));
    var namespaces: std.ArrayList([]const u8) = .empty;
    try namespaces.append(allocator, try allocator.dupe(u8, "sessions"));

    var contract = HandlerContract{
        .handler = .{ .path = path, .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = env_literals, .dynamic = true },
        .egress = .{ .hosts = hosts, .dynamic = false },
        .cache = .{ .namespaces = namespaces, .dynamic = false },
        .sql = contract_mod.emptySqlInfo(),
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
        .api = contract_mod.emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
    defer contract.deinit(allocator);

    var policy = HandlerPolicy{
        .env = .{},
        .egress = .{},
        .cache = .{},
    };
    defer policy.deinit(allocator);
    try policy.env.?.appendUnique(allocator, "PUBLIC_KEY");
    try policy.egress.?.appendUnique(allocator, "api.example.com");
    try policy.cache.?.appendUnique(allocator, "metrics");

    var report = try validateContract(allocator, &contract, &policy);
    defer report.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 3), report.violations.items.len);
    try std.testing.expectEqual(PolicyCategory.env, report.violations.items[0].category);
    try std.testing.expectEqual(ViolationKind.literal_not_allowed, report.violations.items[0].kind);
    try std.testing.expectEqual(ViolationKind.dynamic_not_allowed, report.violations.items[1].kind);
    try std.testing.expectEqual(PolicyCategory.cache, report.violations.items[2].category);
}

test "runtime policy host matching is case insensitive" {
    const policy = RuntimePolicy{
        .egress = .{
            .enabled = true,
            .values = &[_][]const u8{"API.EXAMPLE.COM"},
        },
    };

    try std.testing.expect(policy.allowsEgressHost("api.example.com"));
    try std.testing.expect(!policy.allowsEgressHost("other.example.com"));
}

test "contractToRuntimePolicy restricts static sections" {
    const allocator = std.testing.allocator;

    const path = try allocator.dupe(u8, "handler.ts");
    var env_literals: std.ArrayList([]const u8) = .empty;
    try env_literals.append(allocator, try allocator.dupe(u8, "API_KEY"));
    try env_literals.append(allocator, try allocator.dupe(u8, "DB_URL"));
    var hosts: std.ArrayList([]const u8) = .empty;
    try hosts.append(allocator, try allocator.dupe(u8, "api.stripe.com"));
    var namespaces: std.ArrayList([]const u8) = .empty;
    try namespaces.append(allocator, try allocator.dupe(u8, "sessions"));

    var contract = HandlerContract{
        .handler = .{ .path = path, .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = env_literals, .dynamic = false },
        .egress = .{ .hosts = hosts, .dynamic = false },
        .cache = .{ .namespaces = namespaces, .dynamic = false },
        .sql = contract_mod.emptySqlInfo(),
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
        .api = contract_mod.emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
    defer contract.deinit(allocator);

    const policy = contractToRuntimePolicy(&contract);

    // All sections should be restricted
    try std.testing.expect(policy.env.enabled);
    try std.testing.expect(policy.egress.enabled);
    try std.testing.expect(policy.cache.enabled);

    // Should allow proven values
    try std.testing.expect(policy.allowsEnv("API_KEY"));
    try std.testing.expect(policy.allowsEnv("DB_URL"));
    try std.testing.expect(!policy.allowsEnv("SECRET"));

    try std.testing.expect(policy.allowsEgressHost("api.stripe.com"));
    try std.testing.expect(!policy.allowsEgressHost("evil.com"));

    try std.testing.expect(policy.allowsCacheNamespace("sessions"));
    try std.testing.expect(!policy.allowsCacheNamespace("other"));
}

test "contractToRuntimePolicy leaves dynamic sections permissive" {
    const allocator = std.testing.allocator;

    const path = try allocator.dupe(u8, "handler.ts");
    var env_literals: std.ArrayList([]const u8) = .empty;
    try env_literals.append(allocator, try allocator.dupe(u8, "API_KEY"));

    var contract = HandlerContract{
        .handler = .{ .path = path, .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = env_literals, .dynamic = true }, // dynamic
        .egress = .{ .hosts = .empty, .dynamic = false }, // static, empty
        .cache = .{ .namespaces = .empty, .dynamic = true }, // dynamic
        .sql = contract_mod.emptySqlInfo(),
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
        .api = contract_mod.emptyApiInfo(),
        .verification = null,
        .aot = null,
    };
    defer contract.deinit(allocator);

    const policy = contractToRuntimePolicy(&contract);

    // Dynamic sections are permissive (not enabled)
    try std.testing.expect(!policy.env.enabled);
    try std.testing.expect(policy.allowsEnv("ANYTHING"));

    // Static but empty: restricted to nothing (enabled with empty list)
    try std.testing.expect(policy.egress.enabled);
    try std.testing.expect(!policy.allowsEgressHost("any.host"));

    // Dynamic cache is permissive
    try std.testing.expect(!policy.cache.enabled);
    try std.testing.expect(policy.allowsCacheNamespace("anything"));
}
