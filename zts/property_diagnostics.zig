//! Property Violation Diagnostics
//!
//! Aggregates actionable property violations from compile-time analyzers
//! (FaultCoverageChecker, FlowChecker, HandlerVerifier) into a unified list
//! with optional counterexample test case references.
//!
//! Output:
//!   - JSONL file of counterexample tests (runnable via --test)
//!   - Human-readable violations summary for the build output
//!
//! Counterexample tests demonstrate the violation: they configure I/O stubs
//! to place the handler in the violating state and assert the resulting
//! (incorrect) response. The tests PASS on the buggy handler and FAIL
//! once the handler is fixed.

const std = @import("std");
const ir = @import("parser/ir.zig");
const path_gen = @import("path_generator.zig");
const fault_cov = @import("fault_coverage.zig");
const flow_checker_mod = @import("flow_checker.zig");
const handler_verifier_mod = @import("handler_verifier.zig");
const handler_contract = @import("handler_contract.zig");

const IrView = ir.IrView;
const GeneratedTest = path_gen.GeneratedTest;

// ---------------------------------------------------------------------------
// Violation types
// ---------------------------------------------------------------------------

pub const ViolationKind = enum {
    /// A critical I/O call (auth/validation) has a failure path that returns 2xx.
    fault_uncovered,
    /// Unvalidated user input reaches a sensitive sink (fetchSync body, Response.html).
    injection_unsafe,
    /// Secret env var data flows to a response body, log, or egress URL.
    secret_leakage,
    /// Credential data (auth token, JWT) flows to a response body or log.
    credential_leakage,
    /// result.value accessed without checking result.ok first.
    result_unsafe,
    /// Optional value (string | undefined) used without narrowing.
    optional_unchecked,
};

pub const PropertyViolation = struct {
    kind: ViolationKind,
    /// Human-readable description. Does not own the memory - points into the
    /// source system's diagnostic string (FlowChecker, FaultCoverage, HandlerVerifier).
    /// Exception: when owns_strings is true, message was duped and must be freed.
    message: []const u8,
    /// Suggested fix. May be null. Does not own the memory.
    /// Exception: when owns_strings is true, help was duped and must be freed.
    help: ?[]const u8,
    /// Source file line number (0 = unknown).
    loc_line: u32,
    /// Source file column (0 = unknown).
    loc_col: u16,
    /// When non-null, identifies a GeneratedTest that demonstrates this violation.
    counterexample_name: ?[]const u8, // does not own
    counterexample_index: ?usize,
    /// When true, message and help were duped by the collector and must be freed.
    owns_strings: bool = false,
};

/// Free owned strings in violations that were allocated by collectors.
pub fn deinitViolations(allocator: std.mem.Allocator, violations: []const PropertyViolation) void {
    for (violations) |v| {
        if (v.owns_strings) {
            allocator.free(v.message);
            if (v.help) |h| allocator.free(h);
        }
    }
}

// ---------------------------------------------------------------------------
// Collection helpers
// ---------------------------------------------------------------------------

/// Append violations derived from FaultCoverageChecker diagnostics.
/// Only `success_on_critical_failure` warnings are included.
/// Counterexample tests are looked up by name in `tests`.
/// Caller owns the list items' lifetime (messages are borrowed from `fc_diags`).
pub fn collectFaultViolations(
    allocator: std.mem.Allocator,
    violations: *std.ArrayList(PropertyViolation),
    fc_diags: []const fault_cov.Diagnostic,
    tests: []const GeneratedTest,
) void {
    for (fc_diags) |diag| {
        if (diag.severity != .warning) continue;
        if (diag.kind != .success_on_critical_failure) continue;

        var cx_name: ?[]const u8 = null;
        var cx_idx: ?usize = null;
        for (tests, 0..) |t, i| {
            if (std.mem.eql(u8, t.name, diag.path_name)) {
                cx_name = t.name;
                cx_idx = i;
                break;
            }
        }

        violations.append(allocator, .{
            .kind = .fault_uncovered,
            .message = diag.message,
            .help = "ensure this function's failure path returns a non-2xx status",
            .loc_line = 0,
            .loc_col = 0,
            .counterexample_name = cx_name,
            .counterexample_index = cx_idx,
        }) catch {};
    }
}

/// Append violations derived from FlowChecker diagnostics.
/// Line/column are resolved from `ir_view` via the diagnostic's node index.
/// Flow violations do not have PathGenerator counterexamples because fetchSync
/// and Response sinks are not virtual module calls tracked by PathGenerator.
pub fn collectFlowViolations(
    allocator: std.mem.Allocator,
    violations: *std.ArrayList(PropertyViolation),
    flow_diags: []const flow_checker_mod.Diagnostic,
    ir_view: IrView,
) void {
    for (flow_diags) |diag| {
        const kind: ViolationKind = switch (diag.kind) {
            .unvalidated_input_in_egress => .injection_unsafe,
            .secret_in_response,
            .secret_in_log,
            .secret_in_egress_url,
            .secret_in_egress_body,
            => .secret_leakage,
            .credential_in_response,
            .credential_in_log,
            .credential_in_egress_url,
            => .credential_leakage,
        };

        var loc_line: u32 = 0;
        var loc_col: u16 = 0;
        if (ir_view.getLoc(diag.node)) |loc| {
            loc_line = loc.line;
            loc_col = loc.column;
        }

        // Dupe strings: flow messages may be allocated by messageWithReason and
        // freed when the FlowChecker deinits, which happens before JSONL/summary
        // generation. owns_strings = true signals the caller to free them.
        const message = allocator.dupe(u8, diag.message) catch continue;
        const help: ?[]const u8 = if (diag.help) |h|
            allocator.dupe(u8, h) catch {
                allocator.free(message);
                continue;
            }
        else
            null;

        violations.append(allocator, .{
            .kind = kind,
            .message = message,
            .help = help,
            .loc_line = loc_line,
            .loc_col = loc_col,
            .counterexample_name = null,
            .counterexample_index = null,
            .owns_strings = true,
        }) catch {
            allocator.free(message);
            if (help) |h| allocator.free(h);
        };
    }
}

/// Append violations derived from HandlerVerifier diagnostics.
/// Only result_unsafe and optional_unchecked errors are included.
pub fn collectVerifierViolations(
    allocator: std.mem.Allocator,
    violations: *std.ArrayList(PropertyViolation),
    verifier_diags: []const handler_verifier_mod.Diagnostic,
    ir_view: IrView,
) void {
    for (verifier_diags) |diag| {
        const kind: ViolationKind = switch (diag.kind) {
            .unchecked_result_value => .result_unsafe,
            .unchecked_optional_use, .unchecked_optional_access => .optional_unchecked,
            else => continue,
        };

        var loc_line: u32 = 0;
        var loc_col: u16 = 0;
        if (ir_view.getLoc(diag.node)) |loc| {
            loc_line = loc.line;
            loc_col = loc.column;
        }

        violations.append(allocator, .{
            .kind = kind,
            .message = diag.message,
            .help = diag.help,
            .loc_line = loc_line,
            .loc_col = loc_col,
            .counterexample_name = null,
            .counterexample_index = null,
        }) catch {};
    }
}

// ---------------------------------------------------------------------------
// JSONL export
// ---------------------------------------------------------------------------

/// Write counterexample test cases for violations that reference a GeneratedTest.
/// Format matches the declarative test runner (test_runner.zig) and is runnable
/// via `zig build run -- handler.ts --test handler.violations.jsonl`.
///
/// Test names are prefixed with violation kind and index for self-documentation.
/// Expected status reflects the INCORRECT status the buggy handler currently
/// returns: tests pass on the buggy handler, fail after the fix.
pub fn writeViolationsJsonl(
    writer: anytype,
    allocator: std.mem.Allocator,
    violations: []const PropertyViolation,
    tests: []const GeneratedTest,
) !void {
    var emitted: std.AutoHashMapUnmanaged(usize, void) = .empty;
    defer emitted.deinit(allocator);

    for (violations, 0..) |v, vi| {
        const cx_idx = v.counterexample_index orelse continue;
        if (cx_idx >= tests.len) continue;

        const gop = try emitted.getOrPut(allocator, cx_idx);
        if (gop.found_existing) continue;

        const test_case = tests[cx_idx];

        // Test header with descriptive name
        try writer.writeAll("{\"type\":\"test\",\"name\":\"");
        try writer.writeAll(violationKindTag(v.kind));
        try writer.writeByte('-');
        try writer.print("{d}", .{vi});
        try writer.writeAll(": ");
        try writeJsonStringContent(writer, v.message);
        try writer.writeAll("\"}\n");

        // Request
        try writer.writeAll("{\"type\":\"request\",\"method\":\"");
        try writer.writeAll(test_case.method);
        try writer.writeAll("\",\"url\":");
        try handler_contract.writeJsonString(writer, test_case.url);
        if (test_case.has_auth_header) {
            try writer.writeAll(",\"headers\":{\"authorization\":\"Bearer test-token\"},\"body\":null}\n");
        } else {
            try writer.writeAll(",\"headers\":{},\"body\":null}\n");
        }

        // IO stubs
        for (test_case.io_stubs.items) |stub| {
            try writer.print(
                "{{\"type\":\"io\",\"seq\":{d},\"module\":\"{s}\",\"fn\":\"{s}\",\"result\":{s}}}\n",
                .{ stub.seq, stub.module, stub.func, stub.result_json },
            );
        }

        // Expected response (the incorrect status the buggy handler returns)
        try writer.print("{{\"type\":\"expect\",\"status\":{d}}}\n", .{test_case.expected_status});
    }
}

fn violationKindTag(kind: ViolationKind) []const u8 {
    return switch (kind) {
        .fault_uncovered => "fault-uncovered",
        .injection_unsafe => "injection-unsafe",
        .secret_leakage => "secret-leakage",
        .credential_leakage => "credential-leakage",
        .result_unsafe => "result-unsafe",
        .optional_unchecked => "optional-unchecked",
    };
}

fn writeJsonStringContent(writer: anytype, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => try writer.writeByte(c),
        }
    }
}

// ---------------------------------------------------------------------------
// Build output formatting
// ---------------------------------------------------------------------------

/// Print a violations summary appended to the build output after the property
/// table. Includes fix suggestions and counterexample references.
pub fn formatViolationsSummary(
    writer: anytype,
    violations: []const PropertyViolation,
    handler_filename: []const u8,
    violations_jsonl_path: ?[]const u8,
) !void {
    if (violations.len == 0) return;

    try writer.print("\nVIOLATIONS ({d})\n", .{violations.len});
    try writer.writeAll("=" ** 60 ++ "\n");

    for (violations) |v| {
        try writer.print("  {s}", .{violationLabel(v.kind)});
        if (v.loc_line > 0) {
            try writer.print("  {s}:{d}", .{ handler_filename, v.loc_line });
        }
        try writer.writeByte('\n');
        try writer.print("    {s}\n", .{v.message});
        if (v.help) |help| {
            try writer.print("    Fix: {s}\n", .{help});
        }
        if (v.counterexample_name) |cx_name| {
            try writer.print("    Counterexample: \"{s}\"", .{cx_name});
            if (violations_jsonl_path) |path| {
                try writer.print(" (see {s})", .{path});
            }
            try writer.writeByte('\n');
        }
        try writer.writeByte('\n');
    }

    if (violations_jsonl_path) |path| {
        const with_cx = countWithCounterexamples(violations);
        if (with_cx > 0) {
            try writer.print(
                "  {d} counterexample test(s) written to {s}\n",
                .{ with_cx, path },
            );
            try writer.writeAll("  These tests PASS on the buggy handler and FAIL once fixed.\n");
        }
    }
}

fn violationLabel(kind: ViolationKind) []const u8 {
    return switch (kind) {
        .fault_uncovered => "FAULT COVERAGE GAP",
        .injection_unsafe => "INJECTION RISK",
        .secret_leakage => "SECRET LEAKAGE",
        .credential_leakage => "CREDENTIAL LEAKAGE",
        .result_unsafe => "RESULT UNSAFE",
        .optional_unchecked => "OPTIONAL UNCHECKED",
    };
}

fn countWithCounterexamples(violations: []const PropertyViolation) usize {
    var count: usize = 0;
    for (violations) |v| {
        if (v.counterexample_index != null) count += 1;
    }
    return count;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "empty violations - no output" {
    const allocator = std.testing.allocator;
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const violations: []const PropertyViolation = &.{};
    try formatViolationsSummary(&aw.writer, violations, "handler.ts", null);
    buf = aw.toArrayList();
    try std.testing.expectEqual(@as(usize, 0), buf.items.len);
}

test "formatViolationsSummary includes path when provided" {
    const allocator = std.testing.allocator;

    const violation = PropertyViolation{
        .kind = .fault_uncovered,
        .message = "handler returns 2xx when auth fails",
        .help = "return 401 on failure",
        .loc_line = 0,
        .loc_col = 0,
        .counterexample_name = "path 1 !result.ok",
        .counterexample_index = 0,
    };
    const violations = [_]PropertyViolation{violation};

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try formatViolationsSummary(&aw.writer, &violations, "handler.ts", "handler.violations.jsonl");
    buf = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, buf.items, "FAULT COVERAGE GAP") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "(see handler.violations.jsonl)") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "1 counterexample test(s) written to handler.violations.jsonl") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "PASS on the buggy handler") != null);
}

test "fault violation with counterexample emits JSONL" {
    const allocator = std.testing.allocator;

    var stubs: std.ArrayList(path_gen.IoStub) = .empty;
    defer stubs.deinit(allocator);
    try stubs.append(allocator, .{
        .seq = 0,
        .module = "zigttp:auth",
        .func = "jwtVerify",
        .result_json = "{\"ok\":false,\"error\":\"test-error\"}",
    });
    const test_case = GeneratedTest{
        .name = try allocator.dupe(u8, "path 1 POST /api jwtVerify !result.ok"),
        .method = "POST",
        .url = "/api",
        .has_auth_header = true,
        .expected_status = 200,
        .io_stubs = stubs,
    };
    defer allocator.free(test_case.name);

    const tests = [_]GeneratedTest{test_case};

    const fc_diag = fault_cov.Diagnostic{
        .severity = .warning,
        .kind = .success_on_critical_failure,
        .func_name = "jwtVerify",
        .module_name = "zigttp:auth",
        .path_name = "path 1 POST /api jwtVerify !result.ok",
        .status = 200,
        .message = "handler returns 2xx when auth/validation fails",
    };
    const fc_diags = [_]fault_cov.Diagnostic{fc_diag};

    var violations: std.ArrayList(PropertyViolation) = .empty;
    defer violations.deinit(allocator);
    collectFaultViolations(allocator, &violations, &fc_diags, &tests);

    try std.testing.expectEqual(@as(usize, 1), violations.items.len);
    try std.testing.expectEqual(ViolationKind.fault_uncovered, violations.items[0].kind);
    try std.testing.expectEqualStrings(
        "path 1 POST /api jwtVerify !result.ok",
        violations.items[0].counterexample_name.?,
    );
    try std.testing.expectEqual(@as(usize, 0), violations.items[0].counterexample_index.?);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeViolationsJsonl(&aw.writer, allocator, violations.items, &tests);
    buf = aw.toArrayList();

    try std.testing.expect(buf.items.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "fault-uncovered-0") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"POST\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "jwtVerify") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"status\":200") != null);
}
