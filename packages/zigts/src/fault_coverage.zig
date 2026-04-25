//! Compile-Time Fault Coverage Analysis
//!
//! Consumes the PathGenerator's enumerated execution paths and analyzes how
//! the handler responds to each I/O failure mode. Produces a FaultCoverageReport
//! containing per-call-site entries, an overall coverage grade, and diagnostics
//! for suspicious failure handling (e.g., 2xx on auth failure).
//!
//! This is possible because zigttp's virtual modules have known failure modes
//! declared via FailureSeverity in their FunctionBinding, and the PathGenerator
//! already enumerates every success/failure fork with constraints.
//!
//! The checker does NOT prescribe correct status codes. It warns about structural
//! patterns that are overwhelmingly correlated with bugs (returning 200 when
//! JWT verification fails) and produces a coverage matrix as a build artifact.

const std = @import("std");
const mb = @import("module_binding.zig");
const builtin_modules = @import("builtin_modules.zig");
const path_gen = @import("path_generator.zig");

const route_match = @import("route_match.zig");
const Constraint = path_gen.Constraint;
const StubInfo = path_gen.StubInfo;
const GeneratedTest = path_gen.GeneratedTest;

// ---------------------------------------------------------------------------
// External severity overrides
// ---------------------------------------------------------------------------

pub const ExternalSeverity = struct {
    path: []const u8,
    method: []const u8,
    severity: mb.FailureSeverity,
    reason: []const u8,
};

/// Parse external severity overrides from JSON bytes.
/// Expected format:
/// ```json
/// {
///   "callSites": [
///     { "path": "/api/orders/:id/approve", "method": "POST",
///       "severity": "critical", "reason": "Governed transition" }
///   ]
/// }
/// ```
pub fn parseExternalSeverities(
    allocator: std.mem.Allocator,
    json_bytes: []const u8,
) ![]const ExternalSeverity {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_bytes, .{}) catch
        return error.InvalidJson;
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidJson;
    const root = parsed.value.object;

    const call_sites_val = root.get("callSites") orelse return error.InvalidJson;
    if (call_sites_val != .array) return error.InvalidJson;
    const call_sites = call_sites_val.array;

    var result: std.ArrayList(ExternalSeverity) = .empty;
    errdefer {
        for (result.items) |item| {
            allocator.free(item.path);
            allocator.free(item.method);
            allocator.free(item.reason);
        }
        result.deinit(allocator);
    }

    for (call_sites.items) |entry| {
        if (entry != .object) continue;
        const obj = entry.object;

        const path_val = obj.get("path") orelse continue;
        const method_val = obj.get("method") orelse continue;
        const severity_val = obj.get("severity") orelse continue;

        if (path_val != .string or method_val != .string or severity_val != .string) continue;

        const severity = mapSeverityString(severity_val.string) orelse continue;

        const reason_val = obj.get("reason");
        const reason_str = if (reason_val) |rv|
            (if (rv == .string) rv.string else "")
        else
            "";

        const path_dupe = try allocator.dupe(u8, path_val.string);
        errdefer allocator.free(path_dupe);
        const method_dupe = try allocator.dupe(u8, method_val.string);
        errdefer allocator.free(method_dupe);
        const reason_dupe = try allocator.dupe(u8, reason_str);

        try result.append(allocator, .{
            .path = path_dupe,
            .method = method_dupe,
            .severity = severity,
            .reason = reason_dupe,
        });
    }

    return try result.toOwnedSlice(allocator);
}

/// Map a severity string to the FailureSeverity enum.
/// "high" is treated as an alias for "critical".
pub fn mapSeverityString(s: []const u8) ?mb.FailureSeverity {
    if (std.mem.eql(u8, s, "critical")) return .critical;
    if (std.mem.eql(u8, s, "high")) return .critical;
    if (std.mem.eql(u8, s, "normal")) return .expected;
    if (std.mem.eql(u8, s, "upstream")) return .upstream;
    if (std.mem.eql(u8, s, "none")) return .none;
    return null;
}

/// Return the more severe of two severity levels.
/// Ordering: critical > upstream > expected > none.
pub fn maxSeverity(a: mb.FailureSeverity, b: mb.FailureSeverity) mb.FailureSeverity {
    return if (severityRank(a) >= severityRank(b)) a else b;
}

fn severityRank(s: mb.FailureSeverity) u8 {
    return switch (s) {
        .critical => 3,
        .upstream => 2,
        .expected => 1,
        .none => 0,
    };
}

const pathsMatch = route_match.pathsMatch;

// ---------------------------------------------------------------------------
// Diagnostic types
// ---------------------------------------------------------------------------

pub const Severity = enum {
    warning,
    info,

    pub fn label(self: Severity) []const u8 {
        return switch (self) {
            .warning => "warning",
            .info => "info",
        };
    }
};

pub const DiagnosticKind = enum {
    success_on_critical_failure,
    success_on_upstream_failure,
};

pub const Diagnostic = struct {
    severity: Severity,
    kind: DiagnosticKind,
    func_name: []const u8,
    module_name: []const u8,
    path_name: []const u8,
    status: u16,
    message: []const u8,
};

// ---------------------------------------------------------------------------
// Per-call-site entry in the coverage report
// ---------------------------------------------------------------------------

pub const CallSiteEntry = struct {
    func: []const u8,
    module: []const u8,
    severity: mb.FailureSeverity,
    has_failure_path: bool,
    failure_status: u16,
    flagged: bool,
};

// ---------------------------------------------------------------------------
// Coverage report
// ---------------------------------------------------------------------------

pub const FaultCoverageReport = struct {
    entries: []const CallSiteEntry,
    diagnostics: []const Diagnostic,
    total_failable: u32,
    covered: u32,
    warning_count: u32,

    pub fn isCovered(self: FaultCoverageReport) bool {
        return self.covered == self.total_failable and self.total_failable > 0;
    }

    pub fn isClean(self: FaultCoverageReport) bool {
        return self.isCovered() and self.warning_count == 0;
    }
};

// ---------------------------------------------------------------------------
// FaultCoverageChecker
// ---------------------------------------------------------------------------

pub const FaultCoverageChecker = struct {
    allocator: std.mem.Allocator,
    tests: []const GeneratedTest,
    entries: std.ArrayList(CallSiteEntry),
    diagnostics: std.ArrayList(Diagnostic),
    external_severities: ?[]const ExternalSeverity = null,

    pub fn init(allocator: std.mem.Allocator, tests: []const GeneratedTest) FaultCoverageChecker {
        return .{
            .allocator = allocator,
            .tests = tests,
            .entries = .empty,
            .diagnostics = .empty,
        };
    }

    pub fn setExternalSeverities(self: *FaultCoverageChecker, severities: []const ExternalSeverity) void {
        self.external_severities = severities;
    }

    pub fn deinit(self: *FaultCoverageChecker) void {
        self.entries.deinit(self.allocator);
        self.diagnostics.deinit(self.allocator);
    }

    pub fn analyze(self: *FaultCoverageChecker) !void {
        var seen_funcs = std.StringHashMapUnmanaged(CallSiteEntry).empty;
        defer {
            var key_iter = seen_funcs.iterator();
            while (key_iter.next()) |entry| {
                self.allocator.free(entry.key_ptr.*);
            }
            seen_funcs.deinit(self.allocator);
        }

        for (self.tests) |test_case| {
            // Determine external severity override for this test case's route
            const ext_override = self.lookupExternalOverride(test_case);

            for (test_case.constraints) |c| {
                const info: StubInfo = switch (c) {
                    .stub_truthy, .stub_falsy => |s| s,
                    .result_ok, .result_not_ok => |s| s,
                    else => continue,
                };

                const key = try std.fmt.allocPrint(self.allocator, "{s}\x00{s}", .{ info.module, info.func });
                const gop = seen_funcs.getOrPut(self.allocator, key) catch |err| {
                    self.allocator.free(key);
                    return err;
                };
                if (!gop.found_existing) {
                    var severity = lookupSeverity(info.module, info.func);
                    // Elevate via external override (use the higher of the two)
                    if (ext_override) |ext| {
                        severity = maxSeverity(severity, ext.severity);
                    }
                    if (severity == .none) {
                        _ = seen_funcs.remove(key);
                        self.allocator.free(key);
                        continue;
                    }
                    gop.value_ptr.* = .{
                        .func = info.func,
                        .module = info.module,
                        .severity = severity,
                        .has_failure_path = false,
                        .failure_status = 0,
                        .flagged = false,
                    };
                } else {
                    self.allocator.free(key);
                }

                if (gop.value_ptr.severity == .none) {
                    continue;
                } else if (ext_override) |ext| {
                    // Elevate existing entry if external override is more severe
                    gop.value_ptr.severity = maxSeverity(gop.value_ptr.severity, ext.severity);
                }

                if (c.isFailure()) {
                    gop.value_ptr.has_failure_path = true;
                    gop.value_ptr.failure_status = test_case.expected_status;
                    try self.checkFailurePath(gop.value_ptr, test_case, ext_override);
                }
            }
        }

        // Collect entries sorted by function name for deterministic output
        var func_names: std.ArrayList([]const u8) = .empty;
        defer func_names.deinit(self.allocator);
        var it = seen_funcs.iterator();
        while (it.next()) |kv| {
            try func_names.append(self.allocator, kv.key_ptr.*);
        }
        std.mem.sort([]const u8, func_names.items, {}, struct {
            fn cmp(_: void, a: []const u8, b: []const u8) bool {
                return std.mem.order(u8, a, b) == .lt;
            }
        }.cmp);

        for (func_names.items) |name| {
            try self.entries.append(self.allocator, seen_funcs.get(name).?);
        }
    }

    fn checkFailurePath(
        self: *FaultCoverageChecker,
        entry: *CallSiteEntry,
        test_case: GeneratedTest,
        ext_override: ?ExternalSeverity,
    ) !void {
        const status = test_case.expected_status;
        if (status < 200 or status >= 300) return;

        switch (entry.severity) {
            .critical => {
                entry.flagged = true;
                const message = if (ext_override) |ext|
                    ext.reason
                else
                    "handler returns 2xx when auth/validation fails";
                try self.diagnostics.append(self.allocator, .{
                    .severity = .warning,
                    .kind = .success_on_critical_failure,
                    .func_name = entry.func,
                    .module_name = entry.module,
                    .path_name = test_case.name,
                    .status = status,
                    .message = message,
                });
            },
            .upstream => {
                const message = if (ext_override) |ext|
                    ext.reason
                else
                    "handler returns 2xx on upstream failure (graceful degradation?)";
                try self.diagnostics.append(self.allocator, .{
                    .severity = .info,
                    .kind = .success_on_upstream_failure,
                    .func_name = entry.func,
                    .module_name = entry.module,
                    .path_name = test_case.name,
                    .status = status,
                    .message = message,
                });
            },
            .expected, .none => {},
        }
    }

    fn lookupExternalOverride(self: *const FaultCoverageChecker, test_case: GeneratedTest) ?ExternalSeverity {
        const externals = self.external_severities orelse return null;
        for (externals) |ext| {
            if (pathsMatch(ext.path, test_case.url) and
                std.ascii.eqlIgnoreCase(ext.method, test_case.method))
            {
                return ext;
            }
        }
        return null;
    }

    pub fn getReport(self: *const FaultCoverageChecker) FaultCoverageReport {
        var total_failable: u32 = 0;
        var covered: u32 = 0;
        var warning_count: u32 = 0;

        for (self.entries.items) |entry| {
            total_failable += 1;
            if (entry.has_failure_path) covered += 1;
        }
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .warning) warning_count += 1;
        }

        return .{
            .entries = self.entries.items,
            .diagnostics = self.diagnostics.items,
            .total_failable = total_failable,
            .covered = covered,
            .warning_count = warning_count,
        };
    }

    pub fn formatMatrix(self: *const FaultCoverageChecker, report: FaultCoverageReport, writer: anytype) !void {
        if (report.total_failable == 0) return;

        try writer.writeAll("\nFAULT COVERAGE:\n");

        for (self.entries.items) |entry| {
            try writer.print("  {s:<20}{s:<10}", .{ entry.func, entry.module });

            if (entry.has_failure_path) {
                try writer.print("failure -> {d}", .{entry.failure_status});
                if (!entry.flagged) {
                    if (entry.severity == .expected) {
                        try writer.writeAll("  OK (graceful)");
                    } else {
                        try writer.writeAll("  OK");
                    }
                } else {
                    try writer.writeAll("  WARNING");
                }
            } else {
                try writer.writeAll("no failure path");
            }

            try writer.writeByte('\n');
        }

        try writer.print("\n  Coverage: {d}/{d} failure modes handled\n", .{
            report.covered, report.total_failable,
        });
        try writer.print("  Warnings: {d}\n", .{report.warning_count});
    }

    pub fn formatDiagnostics(self: *const FaultCoverageChecker, writer: anytype) !void {
        for (self.diagnostics.items) |diag| {
            if (diag.severity != .warning) continue;
            try writer.print("fault-coverage {s}: {s}\n", .{ diag.severity.label(), diag.message });
            try writer.print("  function: {s} (zigttp:{s})\n", .{ diag.func_name, diag.module_name });
            try writer.print("  path: {s}\n", .{diag.path_name});
            try writer.print("  status: {d}\n\n", .{diag.status});
        }
    }
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn lookupSeverity(module_name: []const u8, func_name: []const u8) mb.FailureSeverity {
    for (&builtin_modules.all) |*binding| {
        if (!std.mem.eql(u8, binding.name, module_name) and !std.mem.eql(u8, binding.specifier, module_name)) continue;
        for (binding.exports) |*candidate| {
            if (std.mem.eql(u8, candidate.name, func_name)) {
                return candidate.failure_severity;
            }
        }
    }
    return .none;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "empty test list produces empty report" {
    const tests: []const GeneratedTest = &.{};
    var checker = FaultCoverageChecker.init(std.testing.allocator, tests);
    defer checker.deinit();
    try checker.analyze();
    const report = checker.getReport();
    try std.testing.expectEqual(@as(u32, 0), report.total_failable);
    try std.testing.expect(!report.isCovered());
}

test "critical failure with 2xx produces warning" {
    const constraints = [_]Constraint{
        .{ .result_not_ok = .{ .module = "auth", .func = "jwtVerify", .returns = .result } },
    };
    const tests = [_]GeneratedTest{
        .{
            .name = "path 1 !result.ok",
            .method = "GET",
            .url = "/",
            .has_auth_header = false,
            .expected_status = 200,
            .io_stubs = .empty,
            .constraints = &constraints,
        },
    };
    var checker = FaultCoverageChecker.init(std.testing.allocator, &tests);
    defer checker.deinit();
    try checker.analyze();
    const report = checker.getReport();
    try std.testing.expectEqual(@as(u32, 1), report.total_failable);
    try std.testing.expectEqual(@as(u32, 1), report.covered);
    try std.testing.expectEqual(@as(u32, 1), report.warning_count);
    try std.testing.expect(report.isCovered());
    try std.testing.expect(!report.isClean());
}

test "critical failure with 403 produces no warning" {
    const constraints = [_]Constraint{
        .{ .result_not_ok = .{ .module = "auth", .func = "jwtVerify", .returns = .result } },
    };
    const tests = [_]GeneratedTest{
        .{
            .name = "path 1 !result.ok",
            .method = "GET",
            .url = "/",
            .has_auth_header = false,
            .expected_status = 403,
            .io_stubs = .empty,
            .constraints = &constraints,
        },
    };
    var checker = FaultCoverageChecker.init(std.testing.allocator, &tests);
    defer checker.deinit();
    try checker.analyze();
    const report = checker.getReport();
    try std.testing.expectEqual(@as(u32, 1), report.total_failable);
    try std.testing.expectEqual(@as(u32, 1), report.covered);
    try std.testing.expectEqual(@as(u32, 0), report.warning_count);
    try std.testing.expect(report.isClean());
}

test "expected failure (cache miss) with 200 produces no warning" {
    const constraints = [_]Constraint{
        .{ .stub_falsy = .{ .module = "cache", .func = "cacheGet", .returns = .optional_string } },
    };
    const tests = [_]GeneratedTest{
        .{
            .name = "path 1 cache miss",
            .method = "GET",
            .url = "/",
            .has_auth_header = false,
            .expected_status = 200,
            .io_stubs = .empty,
            .constraints = &constraints,
        },
    };
    var checker = FaultCoverageChecker.init(std.testing.allocator, &tests);
    defer checker.deinit();
    try checker.analyze();
    const report = checker.getReport();
    try std.testing.expectEqual(@as(u32, 1), report.total_failable);
    try std.testing.expectEqual(@as(u32, 0), report.warning_count);
    try std.testing.expect(report.isClean());
}

test "multiple call sites tracked independently" {
    const constraints_ok = [_]Constraint{
        .{ .result_not_ok = .{ .module = "auth", .func = "jwtVerify", .returns = .result } },
    };
    const constraints_bad = [_]Constraint{
        .{ .result_not_ok = .{ .module = "validate", .func = "validateJson", .returns = .result } },
    };
    const tests = [_]GeneratedTest{
        .{
            .name = "jwt failure",
            .method = "GET",
            .url = "/",
            .has_auth_header = false,
            .expected_status = 403,
            .io_stubs = .empty,
            .constraints = &constraints_ok,
        },
        .{
            .name = "validation failure",
            .method = "POST",
            .url = "/",
            .has_auth_header = false,
            .expected_status = 200,
            .io_stubs = .empty,
            .constraints = &constraints_bad,
        },
    };
    var checker = FaultCoverageChecker.init(std.testing.allocator, &tests);
    defer checker.deinit();
    try checker.analyze();
    const report = checker.getReport();
    try std.testing.expectEqual(@as(u32, 2), report.total_failable);
    try std.testing.expectEqual(@as(u32, 2), report.covered);
    try std.testing.expectEqual(@as(u32, 1), report.warning_count);
}

test "format matrix output" {
    const constraints = [_]Constraint{
        .{ .result_not_ok = .{ .module = "auth", .func = "jwtVerify", .returns = .result } },
    };
    const tests = [_]GeneratedTest{
        .{
            .name = "jwt failure",
            .method = "GET",
            .url = "/",
            .has_auth_header = false,
            .expected_status = 403,
            .io_stubs = .empty,
            .constraints = &constraints,
        },
    };
    var checker = FaultCoverageChecker.init(std.testing.allocator, &tests);
    defer checker.deinit();
    try checker.analyze();
    const report = checker.getReport();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);
    try checker.formatMatrix(report, &aw.writer);
    buf = aw.toArrayList();
    const output = buf.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "FAULT COVERAGE") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "jwtVerify") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "1/1") != null);
}

// ---------------------------------------------------------------------------
// External severity override tests
// ---------------------------------------------------------------------------

test "parseExternalSeverities with valid JSON" {
    const json =
        \\{
        \\  "callSites": [
        \\    { "path": "/api/orders/:id/approve", "method": "POST", "severity": "critical", "reason": "Governed transition" },
        \\    { "path": "/api/users", "method": "GET", "severity": "none", "reason": "Public endpoint" }
        \\  ]
        \\}
    ;
    const result = try parseExternalSeverities(std.testing.allocator, json);
    defer {
        for (result) |item| {
            std.testing.allocator.free(item.path);
            std.testing.allocator.free(item.method);
            std.testing.allocator.free(item.reason);
        }
        std.testing.allocator.free(result);
    }

    try std.testing.expectEqual(@as(usize, 2), result.len);

    try std.testing.expectEqualStrings("/api/orders/:id/approve", result[0].path);
    try std.testing.expectEqualStrings("POST", result[0].method);
    try std.testing.expectEqual(mb.FailureSeverity.critical, result[0].severity);
    try std.testing.expectEqualStrings("Governed transition", result[0].reason);

    try std.testing.expectEqualStrings("/api/users", result[1].path);
    try std.testing.expectEqual(mb.FailureSeverity.none, result[1].severity);
}

test "severity string mapping" {
    try std.testing.expectEqual(mb.FailureSeverity.critical, mapSeverityString("critical").?);
    try std.testing.expectEqual(mb.FailureSeverity.critical, mapSeverityString("high").?);
    try std.testing.expectEqual(mb.FailureSeverity.expected, mapSeverityString("normal").?);
    try std.testing.expectEqual(mb.FailureSeverity.upstream, mapSeverityString("upstream").?);
    try std.testing.expectEqual(mb.FailureSeverity.none, mapSeverityString("none").?);
    try std.testing.expect(mapSeverityString("invalid") == null);
    try std.testing.expect(mapSeverityString("") == null);
}

test "maxSeverity returns the more severe value" {
    try std.testing.expectEqual(mb.FailureSeverity.critical, maxSeverity(.critical, .expected));
    try std.testing.expectEqual(mb.FailureSeverity.critical, maxSeverity(.expected, .critical));
    try std.testing.expectEqual(mb.FailureSeverity.upstream, maxSeverity(.upstream, .expected));
    try std.testing.expectEqual(mb.FailureSeverity.upstream, maxSeverity(.none, .upstream));
    try std.testing.expectEqual(mb.FailureSeverity.critical, maxSeverity(.critical, .critical));
    try std.testing.expectEqual(mb.FailureSeverity.none, maxSeverity(.none, .none));
}

test "pathsMatch normalizes :id and {id} as equivalent" {
    try std.testing.expect(pathsMatch("/api/orders/:id/approve", "/api/orders/{id}/approve"));
    try std.testing.expect(pathsMatch("/api/orders/{id}/approve", "/api/orders/:id/approve"));
    try std.testing.expect(pathsMatch("/api/orders/:id", "/api/orders/123"));
    try std.testing.expect(pathsMatch("/api/orders/{id}", "/api/orders/123"));
    try std.testing.expect(!pathsMatch("/api/orders", "/api/users"));
    try std.testing.expect(!pathsMatch("/api/orders/:id", "/api/orders/:id/extra"));
    try std.testing.expect(pathsMatch("/", "/"));
}

test "external severity elevates expected to critical" {
    // cacheGet has .expected severity in builtin_modules. An external override
    // on the route marks it as critical. The checker should produce a warning
    // when the handler returns 2xx on the failure path.
    const constraints = [_]Constraint{
        .{ .stub_falsy = .{ .module = "cache", .func = "cacheGet", .returns = .optional_string } },
    };
    const tests = [_]GeneratedTest{
        .{
            .name = "cache miss elevated",
            .method = "POST",
            .url = "/api/orders/:id/approve",
            .has_auth_header = false,
            .expected_status = 200,
            .io_stubs = .empty,
            .constraints = &constraints,
        },
    };
    const ext = [_]ExternalSeverity{
        .{
            .path = "/api/orders/:id/approve",
            .method = "POST",
            .severity = .critical,
            .reason = "Governed transition",
        },
    };
    var checker = FaultCoverageChecker.init(std.testing.allocator, &tests);
    defer checker.deinit();
    checker.setExternalSeverities(&ext);
    try checker.analyze();
    const report = checker.getReport();

    // The entry should exist and be flagged as critical
    try std.testing.expectEqual(@as(u32, 1), report.total_failable);
    try std.testing.expectEqual(@as(u32, 1), report.covered);
    try std.testing.expectEqual(@as(u32, 1), report.warning_count);
    try std.testing.expect(!report.isClean());

    // Diagnostic message should contain the external reason
    try std.testing.expect(report.diagnostics.len > 0);
    try std.testing.expectEqualStrings("Governed transition", report.diagnostics[0].message);
}

test "external severity does not downgrade critical to expected" {
    // jwtVerify is .critical in builtin_modules. An external override of
    // .expected should not lower it - the function-level severity wins.
    const constraints = [_]Constraint{
        .{ .result_not_ok = .{ .module = "auth", .func = "jwtVerify", .returns = .result } },
    };
    const tests = [_]GeneratedTest{
        .{
            .name = "jwt failure not downgraded",
            .method = "GET",
            .url = "/api/public",
            .has_auth_header = false,
            .expected_status = 200,
            .io_stubs = .empty,
            .constraints = &constraints,
        },
    };
    const ext = [_]ExternalSeverity{
        .{
            .path = "/api/public",
            .method = "GET",
            .severity = .expected,
            .reason = "Public endpoint",
        },
    };
    var checker = FaultCoverageChecker.init(std.testing.allocator, &tests);
    defer checker.deinit();
    checker.setExternalSeverities(&ext);
    try checker.analyze();
    const report = checker.getReport();

    // Should still produce a warning because jwtVerify is .critical
    try std.testing.expectEqual(@as(u32, 1), report.warning_count);
    try std.testing.expect(report.diagnostics.len > 0);
    try std.testing.expectEqual(DiagnosticKind.success_on_critical_failure, report.diagnostics[0].kind);
}

test "external severity with {id} matches :id route" {
    const constraints = [_]Constraint{
        .{ .stub_falsy = .{ .module = "cache", .func = "cacheGet", .returns = .optional_string } },
    };
    const tests = [_]GeneratedTest{
        .{
            .name = "cache miss brace id",
            .method = "PUT",
            .url = "/api/items/{id}",
            .has_auth_header = false,
            .expected_status = 200,
            .io_stubs = .empty,
            .constraints = &constraints,
        },
    };
    const ext = [_]ExternalSeverity{
        .{
            .path = "/api/items/:id",
            .method = "PUT",
            .severity = .critical,
            .reason = "Sensitive update",
        },
    };
    var checker = FaultCoverageChecker.init(std.testing.allocator, &tests);
    defer checker.deinit();
    checker.setExternalSeverities(&ext);
    try checker.analyze();
    const report = checker.getReport();

    // External override should match despite :id vs {id} difference
    try std.testing.expectEqual(@as(u32, 1), report.warning_count);
    try std.testing.expectEqualStrings("Sensitive update", report.diagnostics[0].message);
}
