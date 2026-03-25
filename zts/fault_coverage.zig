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

const Constraint = path_gen.Constraint;
const StubInfo = path_gen.StubInfo;
const GeneratedTest = path_gen.GeneratedTest;

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

    pub fn init(allocator: std.mem.Allocator, tests: []const GeneratedTest) FaultCoverageChecker {
        return .{
            .allocator = allocator,
            .tests = tests,
            .entries = .empty,
            .diagnostics = .empty,
        };
    }

    pub fn deinit(self: *FaultCoverageChecker) void {
        self.entries.deinit(self.allocator);
        self.diagnostics.deinit(self.allocator);
    }

    pub fn analyze(self: *FaultCoverageChecker) !void {
        var seen_funcs = std.StringHashMapUnmanaged(CallSiteEntry).empty;
        defer seen_funcs.deinit(self.allocator);

        for (self.tests) |test_case| {
            for (test_case.constraints) |c| {
                const info: StubInfo = switch (c) {
                    .stub_truthy, .stub_falsy => |s| s,
                    .result_ok, .result_not_ok => |s| s,
                    else => continue,
                };

                const gop = try seen_funcs.getOrPut(self.allocator, info.func);
                if (!gop.found_existing) {
                    const severity = lookupSeverity(info.func);
                    if (severity == .none) {
                        _ = seen_funcs.remove(info.func);
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
                } else if (gop.value_ptr.severity == .none) {
                    continue;
                }

                if (c.isFailure()) {
                    gop.value_ptr.has_failure_path = true;
                    gop.value_ptr.failure_status = test_case.expected_status;
                    try self.checkFailurePath(gop.value_ptr, test_case);
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

    fn checkFailurePath(self: *FaultCoverageChecker, entry: *CallSiteEntry, test_case: GeneratedTest) !void {
        const status = test_case.expected_status;
        if (status < 200 or status >= 300) return;

        switch (entry.severity) {
            .critical => {
                entry.flagged = true;
                try self.diagnostics.append(self.allocator, .{
                    .severity = .warning,
                    .kind = .success_on_critical_failure,
                    .func_name = entry.func,
                    .module_name = entry.module,
                    .path_name = test_case.name,
                    .status = status,
                    .message = "handler returns 2xx when auth/validation fails",
                });
            },
            .upstream => {
                try self.diagnostics.append(self.allocator, .{
                    .severity = .info,
                    .kind = .success_on_upstream_failure,
                    .func_name = entry.func,
                    .module_name = entry.module,
                    .path_name = test_case.name,
                    .status = status,
                    .message = "handler returns 2xx on upstream failure (graceful degradation?)",
                });
            },
            .expected, .none => {},
        }
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

fn lookupSeverity(func_name: []const u8) mb.FailureSeverity {
    if (builtin_modules.findFunction(func_name)) |entry| {
        return entry.func.failure_severity;
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
