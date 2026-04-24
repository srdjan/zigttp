//! Counterexample witness solver and wire format.
//!
//! Turns a symbolic path (a property tag + a chain of branch constraints +
//! the I/O call sequence gathered along the way) into an executable
//! counterexample: a concrete Request plus the virtual-module stub responses
//! needed to drive the handler down the witnessing path.
//!
//! The emitted JSONL mirrors `trace.zig`'s recorder output, so a witness
//! produced by the compiler can be fed to `zigttp mock --replay` against
//! the very same handler and the runtime will re-execute the exact path
//! that verification flagged.
//!
//! Why this exists: until now the verifier returned a verdict and a source
//! span. The symbolic execution engine underneath it (path_generator.zig)
//! already carries enough information to hand the agent an executable
//! witness. This module is the bridge.
//!
//! Scope (MVP): the solver is deliberately trivial. Literal comparisons
//! pick the literal; `result_ok` picks an ok stub with a minimal value;
//! truthiness picks a truthy stub; everything else receives a safe default.
//! No SMT; no multi-constraint unification on the same variable beyond
//! the last-write-wins policy that falls out of a linear walk. Enough to
//! drive concrete synthesis on the properties that currently have
//! first-class support (no_secret_leakage is the first consumer).

const std = @import("std");
const mb = @import("module_binding.zig");
const json_utils = @import("json_utils.zig");

// TODO: these types intentionally stay close to `path_generator.Constraint`
// / `StubInfo`; a future extraction into a shared `witness_types.zig` lets
// both files depend on the same definition without either pulling in the other.

pub const StubInfo = struct {
    module: []const u8,
    func: []const u8,
    returns: mb.ReturnKind,
    call_index: ?u32 = null,
};

pub const WitnessConstraint = union(enum) {
    req_method: []const u8,
    req_method_not: []const u8,
    req_url: []const u8,
    req_url_not: []const u8,
    stub_truthy: StubInfo,
    stub_falsy: StubInfo,
    result_ok: StubInfo,
    result_not_ok: StubInfo,
};

/// Invert a witness constraint's truthiness. Request method/url negation is
/// represented explicitly so the solver can pick one concrete different value.
pub fn negate(c: WitnessConstraint) ?WitnessConstraint {
    return switch (c) {
        .req_method => |value| .{ .req_method_not = value },
        .req_method_not => |value| .{ .req_method = value },
        .req_url => |value| .{ .req_url_not = value },
        .req_url_not => |value| .{ .req_url = value },
        .stub_truthy => |info| .{ .stub_falsy = info },
        .stub_falsy => |info| .{ .stub_truthy = info },
        .result_ok => |info| .{ .result_not_ok = info },
        .result_not_ok => |info| .{ .result_ok = info },
    };
}

/// An individual virtual-module call visited along the witness path, in
/// execution order. `result_json` is the stub value the runtime should
/// return when replaying this call under `zigttp mock --replay`.
pub const IoStubEntry = struct {
    seq: u32,
    module: []const u8,
    func: []const u8,
    result_json: []const u8,
};

/// A concrete request synthesised from the constraint chain.
pub const ConcreteRequest = struct {
    method: []const u8,
    url: []const u8,
    has_auth_header: bool,
    body: ?[]const u8,
};

/// Subset of FlowChecker diagnostic kinds that the MVP solver handles.
/// Kept as a separate enum (rather than importing `flow_checker.DiagnosticKind`)
/// because the solver runs before flow_checker is wired in, and because
/// downstream consumers (contract upgrade, system_linker) may want to
/// synthesize witnesses for properties that don't originate in flow_checker.
pub const PropertyTag = enum {
    no_secret_leakage,
    no_credential_leakage,
    input_validated,
    pii_contained,
    injection_safe,

    pub fn asString(self: PropertyTag) []const u8 {
        return @tagName(self);
    }
};

/// Source span for a witness endpoint. Columns are 1-based to match the
/// rest of the compiler diagnostics surface.
pub const SourceSpan = struct {
    line: u32,
    column: u32,
};

pub const CounterexampleWitness = struct {
    property: PropertyTag,
    origin: SourceSpan,
    sink: SourceSpan,
    /// Stable AST node identifier for the origin endpoint, or null if the
    /// producer has not yet wired node-level identity. Node IDs survive
    /// line/column shifts caused by edits above the site, so witness
    /// identity tracked by (property, origin_node_id, sink_node_id) does
    /// not flip false-positive when a patch inserts unrelated lines above
    /// the witnessing path.
    origin_node_id: ?u32 = null,
    sink_node_id: ?u32 = null,
    summary: []const u8,
    request: ConcreteRequest,
    io_stubs: []const IoStubEntry,

    /// Free the ArrayList-owned stub slice. `summary` and string fields
    /// inside the request / stubs are assumed to be caller-owned (either
    /// static literals or arena-allocated); nothing is duped by solve().
    pub fn deinit(self: *CounterexampleWitness, allocator: std.mem.Allocator) void {
        allocator.free(self.io_stubs);
        self.* = undefined;
    }

    /// Write a stable structural key for this witness. Prefers AST node
    /// IDs when present on either endpoint; falls back to line/column so
    /// callers that have not yet adopted node-level tracking still get a
    /// deterministic key. The output is a sha256 hex digest over the
    /// chosen fields, which lets consumers compare witnesses with
    /// constant-time byte equality.
    pub fn stableKey(self: CounterexampleWitness, writer: anytype) !void {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hasher.update(self.property.asString());
        hasher.update("\x00");
        if (self.origin_node_id) |id| {
            hasher.update("n");
            var buf: [4]u8 = undefined;
            std.mem.writeInt(u32, &buf, id, .big);
            hasher.update(&buf);
        } else {
            hasher.update("l");
            var buf: [8]u8 = undefined;
            std.mem.writeInt(u32, buf[0..4], self.origin.line, .big);
            std.mem.writeInt(u32, buf[4..8], self.origin.column, .big);
            hasher.update(&buf);
        }
        hasher.update("\x00");
        if (self.sink_node_id) |id| {
            hasher.update("n");
            var buf: [4]u8 = undefined;
            std.mem.writeInt(u32, &buf, id, .big);
            hasher.update(&buf);
        } else {
            hasher.update("l");
            var buf: [8]u8 = undefined;
            std.mem.writeInt(u32, buf[0..4], self.sink.line, .big);
            std.mem.writeInt(u32, buf[4..8], self.sink.column, .big);
            hasher.update(&buf);
        }
        var digest: [32]u8 = undefined;
        hasher.final(&digest);
        const hex = std.fmt.bytesToHex(digest, .lower);
        try writer.writeAll(&hex);
    }
};

// ---------------------------------------------------------------------------
// Solver
// ---------------------------------------------------------------------------

pub const WitnessInput = struct {
    property: PropertyTag,
    origin: SourceSpan,
    sink: SourceSpan,
    origin_node_id: ?u32 = null,
    sink_node_id: ?u32 = null,
    summary: []const u8,
    constraints: []const WitnessConstraint,
    io_calls: []const TrackedIoCall,
};

/// An I/O call visited on the path, independently of any constraint that
/// may pin its truthiness. Mirrors `path_generator.IoCall` but carries only
/// the fields the solver needs.
pub const TrackedIoCall = struct {
    module: []const u8,
    func: []const u8,
    returns: mb.ReturnKind,
};

/// Solve a witness from its input. The returned witness borrows every
/// `[]const u8` it points at from the input; only the `io_stubs` slice is
/// owned and must be freed with `CounterexampleWitness.deinit`.
pub fn solve(
    allocator: std.mem.Allocator,
    input: WitnessInput,
) error{OutOfMemory}!CounterexampleWitness {
    const request = solveRequest(input.constraints);
    const io_stubs = try solveStubs(allocator, input.constraints, input.io_calls);
    return .{
        .property = input.property,
        .origin = input.origin,
        .sink = input.sink,
        .origin_node_id = input.origin_node_id,
        .sink_node_id = input.sink_node_id,
        .summary = input.summary,
        .request = request,
        .io_stubs = io_stubs,
    };
}

fn solveRequest(constraints: []const WitnessConstraint) ConcreteRequest {
    var method: []const u8 = "GET";
    var url: []const u8 = "/";
    var has_auth = false;

    for (constraints) |c| {
        switch (c) {
            .req_method => |m| method = m,
            .req_method_not => |m| method = alternateMethod(m),
            .req_url => |u| url = u,
            .req_url_not => |u| url = alternateUrl(u),
            .stub_truthy => |info| {
                // The parseBearer import is the canonical signal that the
                // handler expects an Authorization header. Any witness that
                // needs parseBearer to return truthy must drive the request
                // with a Bearer token.
                if (std.mem.eql(u8, info.func, "parseBearer")) has_auth = true;
            },
            else => {},
        }
    }

    return .{
        .method = method,
        .url = url,
        .has_auth_header = has_auth,
        .body = null,
    };
}

fn solveStubs(
    allocator: std.mem.Allocator,
    constraints: []const WitnessConstraint,
    io_calls: []const TrackedIoCall,
) error{OutOfMemory}![]const IoStubEntry {
    var call_overrides: std.AutoHashMapUnmanaged(u32, []const u8) = .empty;
    defer call_overrides.deinit(allocator);

    // Legacy fallback for hand-authored tests or callers that have not yet
    // attached call indexes to stub constraints.
    var func_overrides: std.StringHashMapUnmanaged([]const u8) = .empty;
    defer func_overrides.deinit(allocator);

    for (constraints) |c| {
        switch (c) {
            .stub_truthy => |info| try putStubOverride(
                allocator,
                &call_overrides,
                &func_overrides,
                info,
                stubValue(info.returns, true),
            ),
            .stub_falsy => |info| try putStubOverride(
                allocator,
                &call_overrides,
                &func_overrides,
                info,
                stubValue(info.returns, false),
            ),
            .result_ok => |info| try putStubOverride(
                allocator,
                &call_overrides,
                &func_overrides,
                info,
                "{\"ok\":true,\"value\":{}}",
            ),
            .result_not_ok => |info| try putStubOverride(
                allocator,
                &call_overrides,
                &func_overrides,
                info,
                "{\"ok\":false,\"error\":\"counterexample\"}",
            ),
            else => {},
        }
    }

    const stubs = try allocator.alloc(IoStubEntry, io_calls.len);
    for (io_calls, 0..) |call, i| {
        const result_json = call_overrides.get(@intCast(i)) orelse
            func_overrides.get(call.func) orelse
            stubValue(call.returns, true);
        stubs[i] = .{
            .seq = @intCast(i),
            .module = call.module,
            .func = call.func,
            .result_json = result_json,
        };
    }
    return stubs;
}

fn putStubOverride(
    allocator: std.mem.Allocator,
    call_overrides: *std.AutoHashMapUnmanaged(u32, []const u8),
    func_overrides: *std.StringHashMapUnmanaged([]const u8),
    info: StubInfo,
    result_json: []const u8,
) error{OutOfMemory}!void {
    if (info.call_index) |idx| {
        try call_overrides.put(allocator, idx, result_json);
    } else {
        try func_overrides.put(allocator, info.func, result_json);
    }
}

fn alternateMethod(method: []const u8) []const u8 {
    if (std.mem.eql(u8, method, "GET")) return "POST";
    return "GET";
}

fn alternateUrl(url: []const u8) []const u8 {
    if (std.mem.eql(u8, url, "/")) return "/__zigttp_counterexample__";
    return "/";
}

fn stubValue(returns: mb.ReturnKind, truthy: bool) []const u8 {
    if (!truthy) {
        return switch (returns) {
            .optional_string, .optional_object => "null",
            .result => "{\"ok\":false,\"error\":\"counterexample\"}",
            .boolean => "false",
            .number => "0",
            else => "null",
        };
    }
    return switch (returns) {
        .optional_string, .string => "\"secret-sentinel\"",
        .optional_object, .object => "{\"id\":\"1\"}",
        .result => "{\"ok\":true,\"value\":\"secret-sentinel\"}",
        .boolean => "true",
        .number => "42",
        .undefined => "null",
        .unknown => "\"secret-sentinel\"",
    };
}

// ---------------------------------------------------------------------------
// JSON wire format
//
// Identical in shape to `trace.zig` so a witness can be replayed via the
// existing mock server without schema translation. Emitted as JSONL with
// three record kinds: `witness` (metadata header), `request`, then one
// `io` line per stub. No closing `response` record: the whole point is
// that the *response* is the thing under dispute.
// ---------------------------------------------------------------------------

pub fn writeJsonl(writer: anytype, witness: CounterexampleWitness) !void {
    try writer.print(
        "{{\"type\":\"witness\",\"property\":\"{s}\",\"origin\":{{\"line\":{d},\"column\":{d}",
        .{
            witness.property.asString(),
            witness.origin.line,
            witness.origin.column,
        },
    );
    if (witness.origin_node_id) |id| {
        try writer.print(",\"node_id\":{d}", .{id});
    }
    try writer.print(
        "}},\"sink\":{{\"line\":{d},\"column\":{d}",
        .{
            witness.sink.line,
            witness.sink.column,
        },
    );
    if (witness.sink_node_id) |id| {
        try writer.print(",\"node_id\":{d}", .{id});
    }
    try writer.writeAll("},\"summary\":");
    try json_utils.writeJsonString(writer, witness.summary);
    try writer.writeAll("}\n");

    try writer.print("{{\"type\":\"request\",\"method\":\"{s}\",\"url\":", .{witness.request.method});
    try json_utils.writeJsonString(writer, witness.request.url);
    if (witness.request.has_auth_header) {
        try writer.writeAll(",\"headers\":{\"authorization\":\"Bearer counterexample-token\"}");
    } else {
        try writer.writeAll(",\"headers\":{}");
    }
    if (witness.request.body) |body| {
        try writer.writeAll(",\"body\":");
        try json_utils.writeJsonString(writer, body);
    } else {
        try writer.writeAll(",\"body\":null");
    }
    try writer.writeAll("}\n");

    for (witness.io_stubs) |stub| {
        try writer.print(
            "{{\"type\":\"io\",\"seq\":{d},\"module\":\"{s}\",\"fn\":\"{s}\",\"result\":{s}}}\n",
            .{ stub.seq, stub.module, stub.func, stub.result_json },
        );
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "solve produces default GET / request with no constraints" {
    const allocator = std.testing.allocator;
    var witness = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 2, .column = 1 },
        .summary = "trivial",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer witness.deinit(allocator);

    try std.testing.expectEqualStrings("GET", witness.request.method);
    try std.testing.expectEqualStrings("/", witness.request.url);
    try std.testing.expect(!witness.request.has_auth_header);
    try std.testing.expectEqual(@as(usize, 0), witness.io_stubs.len);
}

test "solve picks literals from req_method and req_url constraints" {
    const allocator = std.testing.allocator;
    const constraints = [_]WitnessConstraint{
        .{ .req_method = "POST" },
        .{ .req_url = "/api/secret" },
    };
    var witness = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 1, .column = 1 },
        .summary = "t",
        .constraints = &constraints,
        .io_calls = &.{},
    });
    defer witness.deinit(allocator);

    try std.testing.expectEqualStrings("POST", witness.request.method);
    try std.testing.expectEqualStrings("/api/secret", witness.request.url);
}

test "solve picks concrete alternatives for negated request constraints" {
    const allocator = std.testing.allocator;
    const constraints = [_]WitnessConstraint{
        .{ .req_method_not = "GET" },
        .{ .req_url_not = "/" },
    };
    var witness = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 1, .column = 1 },
        .summary = "t",
        .constraints = &constraints,
        .io_calls = &.{},
    });
    defer witness.deinit(allocator);

    try std.testing.expectEqualStrings("POST", witness.request.method);
    try std.testing.expectEqualStrings("/__zigttp_counterexample__", witness.request.url);
}

test "solve emits auth header when parseBearer is constrained truthy" {
    const allocator = std.testing.allocator;
    const constraints = [_]WitnessConstraint{
        .{ .stub_truthy = .{ .module = "auth", .func = "parseBearer", .returns = .optional_string } },
    };
    var witness = try solve(allocator, .{
        .property = .no_credential_leakage,
        .origin = .{ .line = 3, .column = 5 },
        .sink = .{ .line = 7, .column = 12 },
        .summary = "bearer token reaches response",
        .constraints = &constraints,
        .io_calls = &.{
            .{ .module = "auth", .func = "parseBearer", .returns = .optional_string },
        },
    });
    defer witness.deinit(allocator);

    try std.testing.expect(witness.request.has_auth_header);
    try std.testing.expectEqual(@as(usize, 1), witness.io_stubs.len);
    try std.testing.expectEqualStrings("parseBearer", witness.io_stubs[0].func);
    try std.testing.expectEqualStrings("\"secret-sentinel\"", witness.io_stubs[0].result_json);
}

test "solve applies result_ok override for Result-returning calls" {
    const allocator = std.testing.allocator;
    const constraints = [_]WitnessConstraint{
        .{ .result_ok = .{ .module = "validate", .func = "validateJson", .returns = .result } },
    };
    var witness = try solve(allocator, .{
        .property = .input_validated,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 1, .column = 1 },
        .summary = "t",
        .constraints = &constraints,
        .io_calls = &.{
            .{ .module = "validate", .func = "validateJson", .returns = .result },
        },
    });
    defer witness.deinit(allocator);

    try std.testing.expectEqualStrings(
        "{\"ok\":true,\"value\":{}}",
        witness.io_stubs[0].result_json,
    );
}

test "solve applies result_not_ok override for failure paths" {
    const allocator = std.testing.allocator;
    const constraints = [_]WitnessConstraint{
        .{ .result_not_ok = .{ .module = "validate", .func = "validateJson", .returns = .result } },
    };
    var witness = try solve(allocator, .{
        .property = .input_validated,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 1, .column = 1 },
        .summary = "t",
        .constraints = &constraints,
        .io_calls = &.{
            .{ .module = "validate", .func = "validateJson", .returns = .result },
        },
    });
    defer witness.deinit(allocator);

    try std.testing.expectEqualStrings(
        "{\"ok\":false,\"error\":\"counterexample\"}",
        witness.io_stubs[0].result_json,
    );
}

test "solve preserves io_calls ordering and sequence numbers" {
    const allocator = std.testing.allocator;
    const io_calls = [_]TrackedIoCall{
        .{ .module = "env", .func = "env", .returns = .optional_string },
        .{ .module = "crypto", .func = "sha256", .returns = .string },
        .{ .module = "cache", .func = "cacheGet", .returns = .optional_string },
    };
    var witness = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 1, .column = 1 },
        .summary = "t",
        .constraints = &.{},
        .io_calls = &io_calls,
    });
    defer witness.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 3), witness.io_stubs.len);
    try std.testing.expectEqual(@as(u32, 0), witness.io_stubs[0].seq);
    try std.testing.expectEqual(@as(u32, 1), witness.io_stubs[1].seq);
    try std.testing.expectEqual(@as(u32, 2), witness.io_stubs[2].seq);
    try std.testing.expectEqualStrings("env", witness.io_stubs[0].func);
    try std.testing.expectEqualStrings("sha256", witness.io_stubs[1].func);
    try std.testing.expectEqualStrings("cacheGet", witness.io_stubs[2].func);
}

test "solve applies stub overrides to the constrained call occurrence" {
    const allocator = std.testing.allocator;
    const constraints = [_]WitnessConstraint{
        .{ .stub_truthy = .{ .module = "env", .func = "env", .returns = .optional_string, .call_index = 0 } },
        .{ .stub_falsy = .{ .module = "env", .func = "env", .returns = .optional_string, .call_index = 1 } },
    };
    const io_calls = [_]TrackedIoCall{
        .{ .module = "env", .func = "env", .returns = .optional_string },
        .{ .module = "env", .func = "env", .returns = .optional_string },
    };
    var witness = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 1, .column = 1 },
        .summary = "t",
        .constraints = &constraints,
        .io_calls = &io_calls,
    });
    defer witness.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 2), witness.io_stubs.len);
    try std.testing.expectEqualStrings("\"secret-sentinel\"", witness.io_stubs[0].result_json);
    try std.testing.expectEqualStrings("null", witness.io_stubs[1].result_json);
}

test "writeJsonl emits witness + request + io in trace-compatible order" {
    const allocator = std.testing.allocator;
    const constraints = [_]WitnessConstraint{
        .{ .req_method = "POST" },
        .{ .req_url = "/leak" },
        .{ .stub_truthy = .{ .module = "env", .func = "env", .returns = .optional_string } },
    };
    const io_calls = [_]TrackedIoCall{
        .{ .module = "env", .func = "env", .returns = .optional_string },
    };
    var witness = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 3, .column = 9 },
        .sink = .{ .line = 5, .column = 12 },
        .summary = "SECRET_KEY flows into Response body",
        .constraints = &constraints,
        .io_calls = &io_calls,
    });
    defer witness.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeJsonl(&aw.writer, witness);
    buf = aw.toArrayList();

    const out = buf.items;
    // Witness header comes first.
    try std.testing.expect(std.mem.startsWith(u8, out, "{\"type\":\"witness\","));
    try std.testing.expect(std.mem.indexOf(u8, out, "\"property\":\"no_secret_leakage\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "\"line\":3") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "\"line\":5") != null);
    // Request line carries the POST method and /leak url.
    try std.testing.expect(std.mem.indexOf(u8, out, "\"type\":\"request\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "\"method\":\"POST\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "\"url\":\"/leak\"") != null);
    // Single io record with seq 0 and the env stub.
    try std.testing.expect(std.mem.indexOf(u8, out, "\"type\":\"io\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "\"seq\":0") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "\"fn\":\"env\"") != null);
    // Three JSONL lines, newline-terminated.
    var line_count: usize = 0;
    for (out) |c| if (c == '\n') {
        line_count += 1;
    };
    try std.testing.expectEqual(@as(usize, 3), line_count);
}

test "solve preserves origin_node_id and sink_node_id when supplied" {
    const allocator = std.testing.allocator;
    var witness = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 10, .column = 3 },
        .sink = .{ .line = 22, .column = 7 },
        .origin_node_id = 41,
        .sink_node_id = 97,
        .summary = "node-id carried",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer witness.deinit(allocator);

    try std.testing.expectEqual(@as(?u32, 41), witness.origin_node_id);
    try std.testing.expectEqual(@as(?u32, 97), witness.sink_node_id);
}

test "stableKey is stable across line shifts when node ids are present" {
    const allocator = std.testing.allocator;

    var a = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 5, .column = 3 },
        .sink = .{ .line = 12, .column = 7 },
        .origin_node_id = 41,
        .sink_node_id = 97,
        .summary = "before-insert",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer a.deinit(allocator);

    var b = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 18, .column = 3 },
        .sink = .{ .line = 25, .column = 7 },
        .origin_node_id = 41,
        .sink_node_id = 97,
        .summary = "after-insert-above",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer b.deinit(allocator);

    var buf_a: std.ArrayList(u8) = .empty;
    defer buf_a.deinit(allocator);
    var aw_a: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf_a);
    try a.stableKey(&aw_a.writer);
    buf_a = aw_a.toArrayList();

    var buf_b: std.ArrayList(u8) = .empty;
    defer buf_b.deinit(allocator);
    var aw_b: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf_b);
    try b.stableKey(&aw_b.writer);
    buf_b = aw_b.toArrayList();

    try std.testing.expectEqualStrings(buf_a.items, buf_b.items);
    try std.testing.expectEqual(@as(usize, 64), buf_a.items.len);
}

test "stableKey differs when node ids differ" {
    const allocator = std.testing.allocator;

    var a = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 5, .column = 3 },
        .sink = .{ .line = 12, .column = 7 },
        .origin_node_id = 41,
        .sink_node_id = 97,
        .summary = "a",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer a.deinit(allocator);

    var b = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 5, .column = 3 },
        .sink = .{ .line = 12, .column = 7 },
        .origin_node_id = 42,
        .sink_node_id = 97,
        .summary = "b",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer b.deinit(allocator);

    var buf_a: std.ArrayList(u8) = .empty;
    defer buf_a.deinit(allocator);
    var aw_a: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf_a);
    try a.stableKey(&aw_a.writer);
    buf_a = aw_a.toArrayList();

    var buf_b: std.ArrayList(u8) = .empty;
    defer buf_b.deinit(allocator);
    var aw_b: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf_b);
    try b.stableKey(&aw_b.writer);
    buf_b = aw_b.toArrayList();

    try std.testing.expect(!std.mem.eql(u8, buf_a.items, buf_b.items));
}

test "stableKey falls back to line/column when node ids are absent" {
    const allocator = std.testing.allocator;

    var w = try solve(allocator, .{
        .property = .injection_safe,
        .origin = .{ .line = 7, .column = 2 },
        .sink = .{ .line = 9, .column = 4 },
        .summary = "fallback",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer w.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try w.stableKey(&aw.writer);
    buf = aw.toArrayList();

    try std.testing.expectEqual(@as(usize, 64), buf.items.len);
}

test "writeJsonl emits node_id fields when present" {
    const allocator = std.testing.allocator;
    var witness = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 3, .column = 9 },
        .sink = .{ .line = 5, .column = 12 },
        .origin_node_id = 11,
        .sink_node_id = 22,
        .summary = "with-node-ids",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer witness.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeJsonl(&aw.writer, witness);
    buf = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"node_id\":11") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"node_id\":22") != null);
}

test "writeJsonl escapes control characters and quotes in summary" {
    const allocator = std.testing.allocator;
    var witness = try solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 1, .column = 1 },
        .summary = "quote:\" backslash:\\ newline:\n ctrl:\x01",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer witness.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeJsonl(&aw.writer, witness);
    buf = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, buf.items, "quote:\\\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "backslash:\\\\") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "newline:\\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "ctrl:\\u0001") != null);
}
